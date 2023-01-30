#lang racket

(provide explicate-control)

(require racket/fixnum)
(require "rvar.rkt")
(require "cvar.rkt")

(define (explicate-control program)
  (match program
    [(Program info body)
     (CProgram info (list `(start . ,(explicate-control-tail body))))]))

; after a remove-complex-opera*, all expressions
; are in monadic normal form
(define (explicate-control-tail exp)
  (match exp
    [(Int _) (Return exp)]
    [(Var _) (Return exp)]
    [(Prim _ _) (Return exp)]
    [(Let varname rexp body)
     (explicate-control-assign varname rexp (explicate-control-tail body))]))

; stmt :=  (assign var exp)
; tail :=  (return exp) / (seq stmt tail)
(define (explicate-control-assign varname exp cvar-body)
  (match exp
     [(Let v x b) (explicate-control-assign v x (explicate-control-assign varname b cvar-body))]
     [_ (Seq (Assign (Var varname) exp) cvar-body)]))
