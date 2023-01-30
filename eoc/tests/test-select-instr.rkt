#lang racket

(provide select-instr-tests)

(require rackunit)

(require (prefix-in bril: bril/lang))
(require "../aarch64var.rkt")
(require/expose "../select-instr.rkt" 
                (bril-instr-to-aarch64var bril-function-to-aarch64var))

(define bril-label (bril:Label "main"))
(define bril-add
  (bril:ValueInstr 'add "a" (bril:Type 'int) '("c" "d") '() '()))
(define bril-const
  (bril:ConstantInstr "a" (bril:Type 'int) (bril:Int 21)))
(define bril-return
  (bril:EffectInstr 'return '("x") '() '()))

(define bril-func
  (bril:Function "main" '() (bril:Type 'int)
                 (list bril-label bril-add bril-return)))

(define select-instr-tests
  (test-suite
   "Instruction Selection Tests"
   (test-suite
     "Single instruction selection"
     (test-case
       "Label"
       (check-equal? (bril-instr-to-aarch64var bril-label)
                     (list (Label "main"))))
     (test-case
       "Addition"
       (check-equal? (bril-instr-to-aarch64var bril-add)
                     (list (Add (Var "c") (Var "d") (Var "a")))))
     (test-case
       "Constant Instr"
       (check-equal? (bril-instr-to-aarch64var bril-const)
                     (list (LMov (Imm 21) (Var "a")))))
     (test-case
       "Return"
       (check-equal? (bril-instr-to-aarch64var bril-return)
                     (list (LMov (Reg 'x0) (Var "x"))
                           (RetDefault)))))
   (test-suite
     "Function translation"
     (test-case
        "Simple function"
        (check-equal? (bril-function-to-aarch64var bril-func)
                      (list (Label "main")
                            (Add (Var "c") (Var "d") (Var "a"))
                            (LMov (Reg 'x0) (Var "x"))
                            (RetDefault)))))))


