#lang racket

(provide cvar-to-bril)

(require (prefix-in bril: bril/lang))
(require "cvar.rkt")

(define (cvar-to-bril program)
 (match program
   [(CProgram _ body)
    (bril:Program (list
                   (bril:Function "main" '() '()
                    (apply append (map cvar-labeled-seq-to-bril body)))))]))

(define (cvar-labeled-seq-to-bril labeled-seq)
 (match labeled-seq
  [(cons label seq)
   (cons
    (bril:Label (symbol->string label))
    (cvar-seq-to-bril seq))]))

(define (cvar-seq-to-bril seq)
 (match seq
  [(Seq stmt tail)
   (append
    (cvar-stmt-to-bril stmt)
    (cvar-seq-to-bril tail))]
  [(Return var)
   (list (bril:EffectInstr 'return (list (symbol->string (Var-name var)))
                          '() '()))]))

(define (cvar-stmt-to-bril stmt)
 (match stmt
  [(Assign (Var varname) rhs) (generate-bril-instrs (symbol->string varname) rhs)]))

(define (generate-bril-instrs dest expr)
 (match expr
  [(Int n) 
   (list (bril:ConstantInstr dest (bril:Type 'int) n))]
  [(Prim '+ (list (Var v1) (Var v2))) 
   (list (bril:ValueInstr 'add dest (bril:Type 'int)
                         (list (symbol->string v1)
                               (symbol->string v2))
                         '() '()))]
  [(Prim '- (list (Var v1))) 
   (list (bril:ConstantInstr dest (bril:Type 'int) 0)
         (bril:ValueInstr 'sub dest (bril:Type 'int)
                          (list dest
                                (symbol->string v1))
                          '() '()))]
  [(Prim '- (list (Var v1) (Var v2)))
   (list (bril:ValueInstr 'sub dest (bril:Type 'int)
                          (list (symbol->string v1)
                                (symbol->string v2))
                          '() '()))]))
