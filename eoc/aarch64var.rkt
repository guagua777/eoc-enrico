#lang racket

(provide 
 (struct-out Label)
 (struct-out Var)
 (struct-out Reg)
 (struct-out Imm)
 (struct-out LMov)
 (struct-out AMOffset)
 (struct-out Ldr)
 (struct-out Str)
 (struct-out Add)
 (struct-out Sub)
 (struct-out RetDefault)
 (struct-out Svc)
 (struct-out Bl))

(require racket/struct)

(struct AArch64VarProgram (info labeled-seq) #:transparent)
(struct Label (label) #:transparent)
(struct Var (var-name) #:transparent)
(struct Reg (reg-name) #:transparent)
(struct Imm (immediate) #:transparent)
(struct LMov (src dest) #:transparent)
(struct AMOffset (base-reg offset) #:transparent)
(struct Ldr (src dest) #:transparent)
(struct Str (src dest) #:transparent)
(struct Add (op1 op2 dest) #:transparent)
(struct Sub (op1 op2 dest) #:transparent)
(struct RetDefault () #:transparent)
(struct Svc (i) #:transparent)
(struct Bl (label) #:transparent)

(define valid-reg-names
  '(x0 x1 x2 x3 x4 x5 x6
    x7 x8 x9 x10 x11 x12
    x13 x14 x15 x16 x17 x18
    x19 x20 x21 x22 x23 x24
    x25 x26 x27 x28 x29 x30
    sp pc xzr))

