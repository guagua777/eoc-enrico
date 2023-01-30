#lang racket

(require racket/fixnum)

(provide Int Prim Program interp-RInt)

(struct Int (value))
(struct Prim (op args))

(struct Program (info body))

(define (read-fixnum)
  (let ([r (read)])
    (cond
       [(fixnum? r) r]
       [else (error "invalid fixnum in input: " r)])))


(define (interp-exp sexp)
  (match sexp
    [(Int n) n]
    [(Prim 'read '()) (read-fixnum)]
    [(Prim '- (list e)) (fx- 0 (interp-exp e))]
    [(Prim '- (list e1 e2)) (fx- (interp-exp e1) (interp-exp e2))]
    [(Prim '+ (list e1 e2)) (fx+ (interp-exp e1) (interp-exp e2))]))

(define (interp-RInt p)
  (match p
    [(Program '() body) (interp-exp body)]))
