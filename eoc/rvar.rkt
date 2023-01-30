#lang racket

(provide Int Prim Var Var-name Let Program interp-RVar-class interp-RVar)

(require racket/fixnum)
(require racket/dict)
(require racket/struct)

(struct Int (value) #:transparent)
(struct Var (name) #:transparent)
(struct Prim (op args) #:transparent)
(struct Let (var expr body) #:transparent)

(struct Program (info body) #:transparent)

(define interp-RVar-class
    (class object%
       (super-new)

       (define/public ((interp-exp env) exp)
         (match exp
           [(Int n) n]
           [(Var name) (eval-symbol env name)]
           [(Prim 'read '()) (read-fixnum)]
           [(Prim '- (list e)) (fx- 0 ((interp-exp env) e))]
           [(Prim '- `(,e1 ,e2)) (fx- ((interp-exp env) e1) ((interp-exp env) e2))]
           [(Prim '+ `(,e1 ,e2)) (fx+ ((interp-exp env) e1) ((interp-exp env) e2))]
           [(Let var rexp body)
            (let ([value ((interp-exp env) rexp)])
              ((interp-exp (dict-set env var value)) body))]))

       (define/public (interp-program program)
          (match program
            [(Program _ body) ((interp-exp (make-immutable-hash)) body)]))))

(define (read-fixnum)
  (let ([r (read)])
    (cond
       [(fixnum? r) r]
       [else (error "invalid fixnum in input: " r)])))

(define (eval-symbol env s)
  (if (dict-has-key? env s)
      (dict-ref env s)
      (error "Symbol " s " not found")))

(define (interp-RVar program)
  (send (new interp-RVar-class) interp-program program))
