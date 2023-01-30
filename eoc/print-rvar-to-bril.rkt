#lang racket

(provide print-rvar-bril)

(require (prefix-in bril: bril/lang))
(require "cvar-to-bril.rkt")
(require "uniquify.rkt")
(require "remove-complex-oper.rkt")
(require "explicate-control.rkt")
(require "rvar.rkt")

(define (print-rvar-bril program)
 (bril:write-bril
  (cvar-to-bril
   (explicate-control
    (remove-complex-opera*-2
     (uniquify program))))))
