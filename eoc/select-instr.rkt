#lang racket

(require (prefix-in bril: bril/lang))
(require "aarch64var.rkt")

(define (bril-function-to-aarch64var function)
  (match function
    [(bril:Function name args ret-type instrs)
     (foldr append '()
            (map bril-instr-to-aarch64var instrs))]))

; only deals with int64 type!
(define (bril-instr-to-aarch64var bril-instr)
  (match bril-instr
   [(bril:Label label) (list (Label label))]
   [(bril:ConstantInstr dest-name _ (bril:Int const-val))
    (list (LMov (Imm const-val) (Var dest-name)))]
   [(bril:ValueInstr op dest type args _ _)
    (bril-value-instr-to-aarch64 op dest args)]
   [(bril:EffectInstr op args _ _)
    (match op
     ['return (bril-return-to-aarch64var args)])]))

(define (bril-value-instr-to-aarch64 op dest args)
  (case op
    ['add (list (Add (Var (list-ref args 0))
                     (Var (list-ref args 1))
                     (Var dest)))]
    ['sub (list (Sub (Var (list-ref args 0))
                     (Var (list-ref args 1))
                     (Var dest)))]))

(define (bril-return-to-aarch64var args)
  (match args
    [(list var)
     (list (LMov (Reg 'x0) (Var (list-ref args 0)))
           (RetDefault))]
    [_ (list (RetDefault))]))
       
