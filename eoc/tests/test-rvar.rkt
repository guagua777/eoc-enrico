#lang racket

(provide rvar-tests)

(require rackunit)
(require "../rvar.rkt")

(define (interp-exp env e)
  ((send (new interp-RVar-class) interp-exp env) e))

(define rvar-tests
    (test-suite 
     "RVar interpretation testsuite"

     (check-equal?
       (let ([env `((a . 1) (b . 2))])
         (interp-exp env (Var 'a)))
       1)

     (check-equal?
       (let ([env `((a . 1) (b . 2))])
         (interp-exp env (Prim '+ (list (Var 'a) (Int 3)))))
       4)

     (check-equal?
       (let ([env `((a . 1) (b . 2))])
         (interp-exp env (Prim '- `(,(Int 3)))))
       -3)

     (check-equal?
       (let ([env `((a . 1))])
         (interp-exp env (Prim '- (list (Int 3) (Var 'a)))))
       2)

     (check-equal?
       (let ([env `((a . 1) (b . 2))])
         (interp-exp env (Prim '+ (list (Var 'a) (Prim `- (list (Var 'b)))))))
       -1)

     (check-equal?
       (interp-exp `() (Let 'a
                            (Prim '+ (list (Int 1) (Int 2)))
                            (Prim '+ (list (Var 'a) (Int 3)))))
       6)

     (check-equal?
       (interp-RVar (Program `() (Let 'a
                                      (Prim '+ (list (Int 1) (Int 2)))
                                      (Prim '+ (list (Var 'a) (Int 3))))))
       6)))
