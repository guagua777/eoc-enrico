#lang racket

(provide all-tests)

(require rackunit)
(require "test-rint.rkt")
(require "test-rvar.rkt")
(require "test-cvar.rkt")
(require "test-uniquify.rkt")
;(require "test-remove-complex-opera.rkt")
;(require "test-explicate-control.rkt")
(require "test-cvar-to-bril.rkt")
(require "test-select-instr.rkt")
(require "test-allocate-regs.rkt")

(define all-tests
  (test-suite
   "All tests"
   rint-tests
   rvar-tests
   cvar-tests
   uniquify-tests
   ;remove-complex-opera-tests
   ;explicate-control-tests
   cvar-to-bril-tests
   select-instr-tests
   allocate-regs-tests))
   
