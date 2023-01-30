#lang racket

(provide remove-complex-opera-tests)

(require rackunit)
(require "test-util.rkt")
(require "uniquify.rkt")
(require "remove-complex-oper.rkt")
(require/expose "remove-complex-oper.rkt" (find-tmp-last rco-exp rco-arg))
(require "rvar.rkt")

(test-eq
   (find-tmp-last (Program '()
                          (Let 'tmp.0 (Let 'x (Int 20)
                                           (Prim '+ (list (Var 'x)
                                                          (Let 'tmp.1 (Int 22)
                                                               (Var 'tmp.1))))) 
                               (Var 'tmp.0))))
   1)

(test-eq
   (find-tmp-last (Program '()
                          (Let 'tmp.x (Let 'x (Int 20)
                                           (Prim '+ (list (Var 'x)
                                                          (Let 'tmp.y (Int 22)
                                                               (Var 'tmp.y))))) 
                               (Var 'tmp.x))))
   -1)

(define programs
  (list
   (Program '()
            (Prim '- (list (Int 20))))
   (Program '()
            (Prim '- (list (Prim '- (list (Int 20))))))
   (Program '()
            (Prim '- (list (Prim '- (list (Prim '- (list (Int 20))))))))
   (Program '() (Prim '+ (list (Int 3) (Prim '- (list (Int 20))))))
   (Program '() 
            (Prim '+ (list (Prim '+ (list (Int 3) (Int 2)))
                           (Prim '+ (list (Int 4) (Int 5))))))
   (Program '() (Let 'x (Int 1) (Var 'x)))
   (Program '() (Let 'x (Prim '+ (list (Prim '- (list (Int 2))) 
                                       (Int 3)))
                        (Prim '+ (list (Var 'x) 
                                       (Prim '+ (list (Int 2) (Int 3)))))))
   (Program '() (Prim '+ 
                      (list (Let 'x 
                                 (Prim '+ 
                                       (list (Prim '- (list (Int 1)))
                                             (Int 2)))
                                 (Prim '+ (list (Var 'x) (Int 2))))
                            (Prim '+ (list (Int 4) (Int 5))))))
   (Program '()
     (Let 'a  (Int 42)
          (Let 'b (Var 'a)
               (Var 'b))))
   (Program '() (Let 'tmp (Prim '- (list (Int 1))) (Var 'tmp)))
   (Program '() (Prim '- 
                      (list (Let 'x (Int 1) (Var 'x)))))
   (Program '() (Let 'x (Let 'x (Int 1) (Var 'x)) (Prim '+ (list (Int 2) (Var 'x)))))
   (Program '()
            (Let 'y (Let 'x (Int 20)
                          (Prim '+ (list (Var 'x)
                                         (Let 'x (Int 22) (Var 'x)))))
                 (Var 'y)))
   (Program '() ; - pos 13
            (Prim '+ (list (Prim 'read '())
                           (Prim 'read '()))))

   (Program '()
            (Prim '+ (list (Let 'x (Int 1)
                               (Let 'x (Int 2)
                                    (Int 2)))
                           (Let 'x (Int 2)
                                (Var 'x)))))))

(begin
 (define-values (a b c) (rco-arg (Prim '- (list (Int 20))) -1))
 (test-eq a (Var 'tmp.0)))

(define (pass p)
    (remove-complex-opera* (uniquify p)))

(define (pass-2 p)
    (remove-complex-opera*-2 (uniquify p)))

(test-eq
 (pass-2 (Program '() (Int 20)))
 (Program '() (Let 'tmp.0 (Int 20) (Var 'tmp.0))))

(test-eq
 (pass-2 (Program '() (Let 'x (Int 20) (Var 'x))))
 (Program '() (Let 'x.1 (Int 20) (Var 'x.1))))

(test-eq
 (pass-2 (Program '() (Let 'x (Int 20) (Int 40))))
 (Program '() (Let 'x.1 (Int 20) (Let 'tmp.0 (Int 40) (Var 'tmp.0)))))

(test-eq
 (pass-2 (Program '() (Let 'x (Int 20) (Let 'y (Int 40) (Prim '+ (list (Var 'y) (Int 1)))))))
 (Program '() (Let 'x.1 (Int 20) 
                   (Let 'y.1 (Int 40) 
                        (Let 'tmp.0 (Int 1)
                             (Let 'tmp.1 (Prim '+ (list (Var 'y.1) (Var 'tmp.0)))
                                  (Var 'tmp.1)))))))
(test-eq
 (pass-2 (Program '() (Let 'x (Let 'y (Int 40) (Prim '+ (list (Var 'y) (Int 1)))) (Var 'x))))
 (Program '() (Let 'x.1 (Let 'y.1 (Int 40) 
                             (Let 'tmp.0 (Int 1)
                                  (Prim '+ (list (Var 'y.1) (Var 'tmp.0)))))
                   (Var 'x.1))))
(test-eq
 (pass-2 (list-ref programs 0))
 (Program '() (Let 'tmp.0 (Int 20) (Let 'tmp.1 (Prim '- (list (Var 'tmp.0))) (Var 'tmp.1)))))

(define inputs
   (let ([empty-inputs (build-list (length programs) (lambda (_) '()))])
     (list-set empty-inputs 13 '(2 3))))

(for ([program programs]
      [input-list inputs])
 (begin
  (test-eq (with-input-from-num-list input-list
               (lambda () (interp-RVar program)))
           (with-input-from-num-list input-list
               (lambda () (interp-RVar (pass program)))))
  (test-eq (with-input-from-num-list input-list
               (lambda () (interp-RVar program)))
           (with-input-from-num-list input-list
               (lambda () (interp-RVar (pass-2 program)))))))
