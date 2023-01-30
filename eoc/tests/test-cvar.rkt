#lang racket

(provide cvar-tests)

(require rackunit)
(require "test-util.rkt")
(require "../cvar.rkt")

(define seq-1
    (Return (Int 0)))

(define seq-2
    (Seq (Assign (Var 'x) (Int 3))
     (Return (Prim '- (list (Int 1) (Var 'x))))))

(define seq-3
    (Seq (Assign (Var 'x) (Int 42))
     (Return (Int 42))))

(define seq-4
    (Seq (Assign (Var 'x) (Int 42))
     (Return (Prim '+ (list (Int 1) (Var 'x))))))

(define seq-5
    (Return (Prim 'read '())))

(define (make-start-seq seq)
    (CProgram '() `((start . ,seq))))

(define cvar-tests
 (test-suite
  "CVar tests"
  (test-case
   "CVar interpretation tests"
   (check-equal?
      (interp-CVar (make-start-seq seq-1))
      0)
   (check-equal?
      (interp-CVar (make-start-seq seq-2))
      -2)
   (check-equal?
      (interp-CVar (make-start-seq seq-3))
      42)
   (check-equal?
      (interp-CVar (make-start-seq seq-4))
      43)
   (with-input-from-num-list '(21)
    (lambda ()
     (check-equal?
        (interp-CVar (make-start-seq seq-5))
        21))))))
