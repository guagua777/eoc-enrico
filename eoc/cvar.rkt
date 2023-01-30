#lang racket

(provide Int Prim Var Var-name Assign Seq Return CProgram interp-CVar% interp-CVar)

(require "rvar.rkt")
(require racket/dict)
(require racket/struct)

(struct CProgram (info labeled-seq) #:transparent)

(struct Assign (var exp) #:transparent)
(struct Seq (stmt tail) #:transparent)
(struct Return (exp) #:transparent)

(define interp-CVar%
    (class interp-RVar-class
       (super-new)
           
       (define/public ((interp-stmt env) stmt)
         (match stmt
           [(Assign (Var varname) exp)
            (let ([val ((send this interp-exp env) exp)])
                 (dict-set! env varname val))]))

       (define/public ((interp-seq env) seq)
         (match seq
           [(Seq stmt tail)
            (begin
             ((interp-stmt env) stmt)
             ((interp-seq env) tail))]
           [(Return exp) ((send this interp-exp env) exp)]))

       (define/public (interp-cprogram program)
          (match program
            [(CProgram _ labeled-seq) ((interp-seq (make-hash)) (cdar labeled-seq))]))))

(define (interp-CVar program)
  (send (new interp-CVar%) interp-cprogram program))
