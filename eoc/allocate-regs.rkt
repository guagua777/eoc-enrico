#lang racket

(require "aarch64var.rkt")

; the allocation strategy is quite trivial:
; always use stack to store results of operations and retrieve operands
; use temporary registers starting from x0 to perform the operation
; note that this separation is necessary because ARM is a load/store ISA
; x0 is used to store the returned value as per Aarch64 calling convention,
; this is already handled by instruction selection


; allocate-regs-std-instr
; this procedure should only be applied to: Add, Sub, RetDefault, Svc, Bl etc
; Use allocate-regs-lmov-instr for LMov

; stack-map is a map with keys being the variables
; and values the position relative to stack base (frame pointer?)

; stack-top is the offset where an eventual next variable would be
; allocated into the stack
; note that this is a virtual offset; sp must by aligned to 16 bytes

; returns the generated instructions
; the stack-map is modified if needed
;
; TODO: does it make sense to use create-stack-bindings on instr-args?
(define (allocate-regs-std-instr! instr stack-map stack-top)
   (begin
     (define instr-dest (AArch64-instr-dest instr))
     (define instr-args (AArch64-instr-args instr))
     (create-stack-bindings! (append instr-args instr-dest) stack-map stack-top)
     (define reg-args (generate-regs 0 (length instr-args)))
     (define reg-dest (generate-regs (length instr-args) (length instr-dest)))
     (append (generate-ldr instr-args stack-map)
             (list (AArch64-instr-from-dest-args instr reg-dest reg-args))
             (if (empty? instr-dest)
                 '()
                 (generate-str (generate-reg-n (length instr-args))
                             (car instr-dest)
                             stack-map)))))

(define (generate-regs start len)
  (map generate-reg-n
       (range start (+ start len))))

(define (generate-reg-n n)
  (Reg (string->symbol (string-append "x" (number->string n)))))

(define (generate-ldr var-list stack-map)
  (for/list ([var var-list]
             [reg (generate-regs 0 (length var-list))])
     (Ldr (AMOffset 'sp (hash-ref stack-map (Var-var-name var))) reg)))

(define (generate-str reg dest-var stack-map)
  (list
    (Str reg (AMOffset 'sp (hash-ref stack-map (Var-var-name dest-var))))))


; generate stack read/writes and allocate registers for
; a LMov instruction
;(define (allocate-regs-lmov-instr! instr stack-map))

; create bindings for the operands that are Var
(define (create-stack-bindings! operand-list stack-map stack-top)
 (begin
  (for-each (lambda (op)
                (match op
                   [(Var var-name)
                    (begin
                     (create-stack-binding! var-name stack-map stack-top))]
                   [_ '()]))
       operand-list)))

; modify the stack map if the variable is not there yet
; only works for 64 bit integers
(define (create-stack-binding! var-name stack-map stack-top)
   (begin
     (hash-update! stack-map
                   var-name
                   identity
                   stack-top)
     (set! stack-top (- stack-top 8))))
                 
(define (AArch64-instr-dest instr)
  (match instr
    [(? LMov?) (list (LMov-dest instr))]
    [(? Add?) (list (Add-dest instr))]
    [(? Sub?) (list (Sub-dest instr))]
    [(? RetDefault?) (list)]))

(define (AArch64-instr-args instr)
  (match instr
    [(? LMov?) (list (LMov-src instr))]
    [(Add op1 op2 _) (list op1 op2)]
    [(Sub op1 op2 _) (list op1 op2)]
    [(? RetDefault?) (list)]))

(define (AArch64-instr-from-dest-args instr dest args)
  (let ([constructor-args
         (match instr
          [(? LMov?) (append args dest)]
          [(? Add?) (append args dest)]
          [(? Sub?) (append args dest)]
          [(? RetDefault?) (list)])])
     (construct-new-instr instr constructor-args)))

(define (construct-new-instr instr args)
   (let*-values ([(struct-type _) (struct-info instr)]
                 [(constructor) (struct-type-make-constructor struct-type)])
     (apply constructor args)))

