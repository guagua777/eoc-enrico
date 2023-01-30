#lang racket

(provide uniquify)

(require "rvar.rkt")

(define (uniquify p)
  (let-values ([(_ res) (uniquify-ret-symtable p)])
    res))

(define (uniquify-ret-symtable p)
  (match p
    [(Program info body)
     (let-values ([(symtable res) ((uniquify-exp (make-immutable-hash)
                                                 (make-immutable-hash)) body)])
       (values symtable
               (Program info
                        res)))]))

(define (add-unique-assoc symtable varname)
  (hash-update symtable varname
               (lambda (ref) (+ ref 1))
               0))

(define (get-unique-assoc symtable varname)
   (if (hash-has-key? symtable varname)
       (string->symbol (format "~a.~a" varname (hash-ref symtable varname)))
       varname))

; symtable is a global table that keeps track of all the variables and what unique
; counter they have reached
; evaltable is the table used for the evaluation context
(define (uniquify-exp symtable evaltable)
  (lambda (sexp)
    (match sexp
      [(Int n) (values symtable (Int n))]
      [(Var name) (values symtable (Var (get-unique-assoc evaltable name)))]
      [(Let var rexp body)
       (begin
         (define outer-symtable (add-unique-assoc symtable var))
         ; create a new evaltable synced with the outer-symtable
         (define outer-evaltable (hash-set evaltable var (hash-ref outer-symtable var)))
         (define-values (inner-symtable uniquified-assignment-exp) ((uniquify-exp outer-symtable evaltable) rexp))
         (define-values (res-symtable uniquified-body-exp) ((uniquify-exp inner-symtable outer-evaltable) body))
         (values res-symtable (Let (get-unique-assoc outer-symtable var)
                                  uniquified-assignment-exp
                                  uniquified-body-exp)))]

      [(Prim op args) 
       (let-values ([(res-symtable res-args)
                     (for/fold ([cur-symtable symtable]
                                [cur-args '()])
                               ([arg args])
                       (let-values ([(new-symtable uniq-exp)
                                     ((uniquify-exp cur-symtable evaltable) arg)])
                          (values new-symtable (append cur-args (list uniq-exp)))))])
           (values res-symtable (Prim op res-args)))])))

