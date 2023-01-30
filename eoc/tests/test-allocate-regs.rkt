#lang racket

(provide allocate-regs-tests)

(require rackunit)

(require "../aarch64var.rkt")
(require/expose "../allocate-regs.rkt"
                (allocate-regs-std-instr!))

(define listing-1
 (list (Label "main")
       (LMov (Imm 0) (Var "c"))
       (LMov (Imm 2) (Var "d"))
       (Add (Var "c") (Var "d") (Var "b"))
       (LMov (Reg 'x0) (Var "b"))
       (RetDefault)))

(define allocate-regs-tests
  (test-suite
   "Register Allocation"
   
   (test-suite
    "Single instruction allocation"
    (test-case
      "Add"
      (check-equal?
        (allocate-regs-std-instr! (Add (Var "a") (Var "b") (Var "c"))
                                  (make-hash (list (cons "a" -0) (cons "b" -8)))
                                  -16)
        (list (Ldr (AMOffset 'sp 0) (Reg 'x0))
              (Ldr (AMOffset 'sp -8) (Reg 'x1))
              (Add (Reg 'x0) (Reg 'x1) (Reg 'x2))
              (Str (Reg 'x2) (AMOffset 'sp -16)))))
    (test-case
      "RetDefault"
      (check-equal?
        (allocate-regs-std-instr! (RetDefault)
                                  (make-hash)
                                  0)
        (list (RetDefault)))))))
                 
