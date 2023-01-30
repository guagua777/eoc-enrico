#lang racket

(provide cvar-to-bril-tests)

(require rackunit)
(require "../cvar-to-bril.rkt")
(require "../uniquify.rkt")
(require "../remove-complex-oper.rkt")
(require "../explicate-control.rkt")
(require "../rvar.rkt")
(require bril/interpreter)

(define listings 
 (list
  (Program '() (Prim '+ (list (Int 1) (Int 2))))
  (Program '() (Prim '+ (list (Prim '- (list (Int 1))) (Int 2))))
  (Program '() (Prim '- (list (Int 1) (Int 2))))))

(define (evaluate-bril-main bril-program)
  (cadr (interp-bril bril-program "main")))

(define (pass program)
  (cvar-to-bril (explicate-control (remove-complex-opera*-2 (uniquify program)))))

(define cvar-to-bril-tests
 (test-suite
  "CVar to bril testsuite"
  (test-case "semantics preservation under interpretation"
   (for ([program listings])
    (check-equal? (interp-RVar program)
                  (evaluate-bril-main (pass program)))))))
