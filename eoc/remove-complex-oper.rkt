#lang racket

; converts the program in monadic normal form
(provide remove-complex-opera* remove-complex-opera*-2)

(require "rvar.rkt")

; find number of the last variable named tmp.n
(define (find-tmp-last p)
  (match p
    [(Program _ body) (find-tmp-last-exp body)]))

(define (find-tmp-last-exp p)
  (match p
    [(Let sym rexp body)
     (let ([tmp-max-rexp (find-tmp-last-exp rexp)]
           [tmp-max-body (find-tmp-last-exp body)])
      (if (is-tmp-var sym)
          (max (get-tmp-num sym) (max tmp-max-rexp tmp-max-body))
          (max tmp-max-rexp tmp-max-body)))]
    [(Prim _ args) (apply max (cons -1 (map find-tmp-last-exp args)))]
    [_ -1]))

(define (is-tmp-var sym)
    (let ([ssym (symbol->string sym)])
         (and (string-prefix? ssym "tmp.")
              (not (eq? #f (string->number (substring ssym 4)))))))

(define (get-tmp-num sym)
  (string->number (substring (symbol->string sym) 4)))

(define (get-unique-symbol tmpcount)
  (string->symbol (format "tmp.~a" tmpcount)))

; assoc-list: '((var-symbol exp) ...)
; returns exp wrapped in a cascade of Let expressions that
; uses the assoc-list as bindings
(define (wrap-associations assoc-list exp)
  (if (empty? assoc-list)
    exp
    (let ([binding (car assoc-list)])
      (Let (car binding)
           (cadr binding)
           (wrap-associations (cdr assoc-list) exp)))))

; remove complex sub-expression 
; Transform the program into monadic normal form
; the resulting code is either
; "atoms", with no side effects
; - a integer literal
; - a variable
; complex expressions, which may have side effects:
; - (read)
; - (- x) where x is an atom
; - (+ x y) where x and y are atoms
; - (let ([var y]) z) where y and z are expressions
; this is achieved by introducing temporary variables when needed
; if (let ([var y]) z) only allowed z to be an atom, this would be called
; ANF (administrative normal form)
(define (remove-complex-opera* p)
  (match p 
    [(Program info body-exp)
     (begin
       (define initial-tmpcount (find-tmp-last-exp body-exp))
       (define-values (new-exp dis) (rco-exp body-exp initial-tmpcount))
       (Program info new-exp))]))

; rco-exp
; returns-values:
; - exp in MNF
; - the temporary var count reached
(define (rco-exp exp tmpcount)
  (match exp
    [(Int _) (values exp tmpcount)]
    [(Var _) (values exp tmpcount)]
    [(Prim op args)
     (begin
      (define-values (new-args assoc-list new-tmpcount)
                     (for/fold ([cur-args '()]
                                [cur-assoc-list '()]
                                [cur-tmpcount tmpcount])
                               ([arg args])
                      (begin
                       (define-values (atom assoc-list tmpcount) (rco-arg arg cur-tmpcount))
                       (values (append cur-args (list atom))
                               (append cur-assoc-list assoc-list)
                               tmpcount))))
      (values (wrap-associations assoc-list (Prim op new-args)) new-tmpcount))]
                               
    [(Let var e body)
     (begin
      (define-values (exp-tmp exp-tmpcount) (rco-exp e tmpcount))
      (define-values (new-body new-tmpcount) (rco-exp body exp-tmpcount))
      (values (Let var exp-tmp new-body)
              new-tmpcount))]))

; rco-arg
; returns-values: 
; new-exp (atom MNF expression)
; association list used to evaluate atom new-exp
; tmpcount reached after having created the new association list and the new temporaries
(define (rco-arg exp tmpcount)
  (match exp
   [(Int _) (values exp '() tmpcount)]
   [(Var _) (values exp '() tmpcount)]
   [(Prim op args)
    (begin
     (define-values (new-args assoc-list new-tmpcount)
                    (for/fold ([cur-args '()]
                               [cur-assoc-list '()]
                               [cur-tmpcount tmpcount])
                              ([arg args])
                     (begin
                      (define-values (atom assoc-list tmpcount) (rco-arg arg cur-tmpcount))
                      (values (append cur-args (list atom))
                              (append cur-assoc-list assoc-list)
                              tmpcount))))
     (define inc-tmpcount (+ new-tmpcount 1))
     (define tmpname (get-unique-symbol inc-tmpcount))
     (set! assoc-list (append assoc-list (list `(,tmpname ,(Prim op new-args)))))
     (values (Var tmpname)
             assoc-list
             inc-tmpcount))]
   ; this must return a simple term
   ; i.e.: either a symbol or number literal
   [(Let var rexp body)
    (begin
     (define-values (new-exp exp-tmpcount) (rco-exp rexp tmpcount))
     (define-values (new-body assoc-list new-tmpcount) (rco-arg body exp-tmpcount))
     (values new-body
             (cons `(,var ,new-exp) assoc-list)
             new-tmpcount))]))

; remove complex sub-expression 
; Transform the program into administrative normal form (ANF)
; This version of remove-complex-opera* is more aggressive
; Every main expression must be saved in a temporary,
; and every argument (e.g. of Prim) must be a variable
; Integers cannot appear naked if not in a binding
(define (remove-complex-opera*-2 p)
  (match p 
    [(Program info body-exp)
     (begin
       (define initial-tmpcount (find-tmp-last-exp body-exp))
       (define-values (new-exp assoc-list tmpcount) (rco-arg-2 body-exp initial-tmpcount))
       (Program info (wrap-associations assoc-list new-exp)))]))

; rco-arg
; returns-values: 
; new-exp (atom ANF expression)
; association list used to evaluate atom new-exp
; tmpcount reached after having created the new association list and the new temporaries
(define (rco-arg-2 exp tmpcount)
  (match exp
   [(Var _) (values exp '() tmpcount)]
   [(Int n) 
    (begin
     (define inc-tmpcount (+ tmpcount 1))
     (define tmpname (get-unique-symbol inc-tmpcount))
     (values (Var tmpname)
             `((,tmpname ,exp))
             inc-tmpcount))]
   [(Prim op args)
    (begin
     (define-values (new-args assoc-list new-tmpcount)
                    (for/fold ([cur-args '()]
                               [cur-assoc-list '()]
                               [cur-tmpcount tmpcount])
                              ([arg args])
                     (begin
                      (define-values (atom assoc-list tmpcount) (rco-arg-2 arg cur-tmpcount))
                      (values (append cur-args (list atom))
                              (append cur-assoc-list assoc-list)
                              tmpcount))))
     (define inc-tmpcount (+ new-tmpcount 1))
     (define tmpname (get-unique-symbol inc-tmpcount))
     (set! assoc-list (append assoc-list (list `(,tmpname ,(Prim op new-args)))))
     (values (Var tmpname)
             assoc-list
             inc-tmpcount))]
   ; this must return a simple term
   ; i.e.: either a symbol or number literal
   [(Let var rexp body)
    (begin
     (define-values (new-exp exp-tmpcount) (rco-exp-2 rexp tmpcount))
     (define-values (new-body assoc-list new-tmpcount) (rco-arg-2 body exp-tmpcount))
     (values new-body
             (cons `(,var ,new-exp) assoc-list)
             new-tmpcount))]))

(define (rco-exp-2 exp tmpcount)
 (match exp
   [(Int _) (values exp tmpcount)]
   [(Var _) (values exp tmpcount)]
   [(Prim op args)
    (begin
     (define-values (new-args assoc-list new-tmpcount)
                    (for/fold ([cur-args '()]
                               [cur-assoc-list '()]
                               [cur-tmpcount tmpcount])
                              ([arg args])
                     (begin
                      (define-values (atom assoc-list tmpcount) (rco-arg-2 arg cur-tmpcount))
                      (values (append cur-args (list atom))
                              (append cur-assoc-list assoc-list)
                              tmpcount))))
     (values (wrap-associations assoc-list (Prim op new-args)) new-tmpcount))]
                               
   [(Let var e body)
    (begin
     (define-values (exp-tmp exp-tmpcount) (rco-exp-2 e tmpcount))
     (define-values (new-body new-tmpcount) (rco-exp-2 body exp-tmpcount))
     (values (Let var exp-tmp new-body)
             new-tmpcount))]))
