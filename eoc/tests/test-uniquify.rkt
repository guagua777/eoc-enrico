#lang racket

(provide uniquify-tests)

(require rackunit)

(require "../rvar.rkt")
(require "../uniquify.rkt")
(require/expose "../uniquify.rkt" (uniquify-exp))

; returns both the resulting symtable and the uniquified program
(define (list-uniquify-exp symtable)
  (lambda (exp)
    (call-with-values (lambda () ((uniquify-exp symtable symtable) exp)) list)))


(define p1
        (Program '()
          (Let 'x (Int 32)
            (Prim '+ (list (Prim '+ (list (Let 'x (Int 10) (Var 'x))
                                          (Let 'x (Int 3) (Var 'x))))
                           (Var 'x))))))
(define p2
        (Program '()
           (Let 'x (Let 'y (Int 9)
                        (Var 'y))
             (Prim '+ (list (Var 'x) (Int 5))))))

(define uniquify-tests
    (test-suite
     "Uniquify pass testsuite"
     (test-case
      "uniquify-exp correctness"
      (let ([tbl (hash-set (make-immutable-hash) 'x 1)])
        (check-equal? ((list-uniquify-exp tbl) (Var 'x))
                 (list #hash((x . 1)) (Var 'x.1))))

      (check-equal? ((list-uniquify-exp (make-immutable-hash)) (Prim 'read (list)))
              (list #hash() (Prim 'read '())))
       
      (check-equal? ((list-uniquify-exp (make-immutable-hash)) (Let 'x (Int 2) (Int 3)))
               (list #hash((x . 1)) (Let 'x.1 (Int 2) (Int 3))))
       
      (check-equal? ((list-uniquify-exp (make-immutable-hash)) (Let 'x (Int 2) (Prim '+ (list (Var 'x) (Int 3)))))
               (list #hash((x . 1)) (Let 'x.1 (Int 2) (Prim '+ (list (Var 'x.1) (Int 3)))))))
     (test-case
      "uniqufication pass"
       (check-equal?
         (uniquify
           (Program '()
             (Let 'x (Int 32)
               (Prim '+ (list (Let 'x (Int 10) (Var 'x)) (Var 'x))))))
         (Program '()
           (Let 'x.1 (Int 32)
             (Prim '+ (list (Let 'x.2 (Int 10) (Var 'x.2)) (Var 'x.1))))))
       
       (check-equal?
        (uniquify
          (Program '()
            (Let 'x (Int 32)
              (Prim '- (list (Var 'x))))))
        (Program '()
          (Let 'x.1 (Int 32)
            (Prim '- (list (Var 'x.1))))))
       
       
       (check-equal?
        (uniquify
          (Program '()
            (Let 'x (Int 32)
              (Prim '+ (list (Let 'x (Int 10) (Var 'x))
                             (Let 'x (Int 3) (Var 'x))
                             (Var 'x))))))
        (Program '()
          (Let 'x.1 (Int 32)
            (Prim '+ (list (Let 'x.2 (Int 10) (Var 'x.2))
                           (Let 'x.3 (Int 3) (Var 'x.3))
                           (Var 'x.1))))))
       (check-equal?
        (uniquify
          (Program '()
            (Let 'x (Let 'x (Int 4)
                      (Prim '+ (list (Var 'x) (Int 1))))
              (Prim '+ (list (Var 'x) (Int 2))))))
        (Program '()
          (Let 'x.1 (Let 'x.2 (Int 4)
                      (Prim '+ (list (Var 'x.2) (Int 1))))
            (Prim '+ (list (Var 'x.1) (Int 2)))))))

     (test-case
      "Uniquify interpretation"
      (for ([program (list p1 p2)])
       (check-equal? (interp-RVar program)
        (interp-RVar (uniquify program)))))))
