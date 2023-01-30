#lang racket

(provide read-bril-file read-rvar-file complete-tests)

(require json rackunit (only-in bril program-to-jsexpr))

(require "uniquify.rkt")
(require "remove-complex-oper.rkt")
(require "explicate-control.rkt")
(require "cvar-to-bril.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (read-bril-file path)
  (with-input-from-file path
   (lambda () (read-json))))

(define (read-rvar-file path)
  (with-input-from-file path
    (lambda () 
      (parameterize ([current-namespace ns])
        (namespace-attach-module-declaration ns '"rvar.rkt")
        (namespace-require '"rvar.rkt")
        (eval (read))))))

(define (rvar-to-cvar program)
  (explicate-control (remove-complex-opera*-2 (uniquify program))))

(define (rvar-to-bril-json program)
  (program-to-jsexpr (cvar-to-bril (rvar-to-cvar program))))

(define (complete-rvar-test test-desc rvar-path bril-path)
  (test-case
    test-desc
    (let ([rvar (read-rvar-file rvar-path)]
          [bril (read-bril-file bril-path)])
      (test-case
        "RVar->bril"
          (parameterize ([current-namespace ns])
            (namespace-attach-module-declaration ns '"rvar.rkt")
            (namespace-require '"rvar.rkt")
            (check-equal? bril (rvar-to-bril-json rvar)))))))

(define complete-tests
  (test-suite
   "Complete Tests"
   (complete-rvar-test
     "Simple addition"
     "complete-tests/test0.rvar"
     "complete-tests/test0.bril")
   (complete-rvar-test
     "Simple addition with subexpression"
     "complete-tests/test1.rvar"
     "complete-tests/test1.bril")))
    
