#lang racket

(require racket/fixnum)

(define (pe-arith p)
  (match p
    [`(program ,e) (pe-exp e)]))


(define (pe-exp exp)
    (pe-remove-node-labels (pe-fold-right-constants (pe-move-reads-left (pe-do-negate exp)))))


; removes R/C/S labels from a tree
(define (pe-remove-node-labels tree)
  (match tree
    [`((+ ,e1 ,e2) ,_) `(+ ,(pe-remove-node-labels e1) ,(pe-remove-node-labels e2))]
    [`(,e C) e]
    [`(,e R) e]))


; partially evaluates an S/R/C-tree
(define (pe-fold-right-constants exp)
  (match exp
    [`(,_ S)  (make-S-node (node-S-left exp) `(,(pe-fold-constants (node-S-right exp)) C))]
    [`(,_ C) `(,(pe-fold-constants exp) C)]
    [`(,_ R) exp]))


; evaluates a C-tree and returns the integer result
(define (pe-fold-constants c-tree)
   (match c-tree
     [`((+ ,e1 ,e2) C) (fx+ (pe-fold-constants e1) (pe-fold-constants e2))]
     [`(,e C) e]))


; pushes down negation to literals, inside additions and elides multiple consecutive negations
(define (pe-do-negate exp)
  (match exp
    [`(- (- ,e))      (pe-do-negate e)]
    [`(- (+ ,e1 ,e2)) (pe-do-negate `(+ (- ,e1) (- ,e2)))]
    [`(+ ,e1 ,e2)     `(+ ,(pe-do-negate e1) ,(pe-do-negate e2))]
    [`(- ,e)          (if (fixnum? e) (fx- 0 e) `(- ,(pe-do-negate e)))]
    [_                exp]))


; takes an exepression and transforms it in an equivalent form such that it is either
; - a tree of C nodes
; - a tree of R nodes
; - a tree of type S, that is its left child is a tree of R nodes and its right child is a tree of C nodes
; a C node is either a node in the form '(<int> C) or ((+ n1 n2) C) where n1 and n2 are C-nodes
; the same applies for R nodes, but the base node is either of type '((read) R) or '((- (read)) R)
; this separation allows easy constant folding of the right subtree: the left subtree only has
; leaves that do (read), while the right subtree only has leaves of integer literal type
(define (pe-move-reads-left exp)
  (match exp
     [(? fixnum?) `(,exp C)]
     [`(read)     `(,exp R)]
     [`(- (read)) `(,exp R)]
     [`(+ ,e1 ,e2)
      (let ([e1-rl (pe-move-reads-left e1)]
            [e2-rl (pe-move-reads-left e2)])
        (pe-join-subtrees e1-rl e2-rl))]))



; join two subtrees and forms either a new C-tree, a new R-tree or a new S-tree
(define (pe-join-subtrees e1-rl e2-rl)
     (cond
       [(and (node-is-C e1-rl) (node-is-C e2-rl))  (make-C-node e1-rl e2-rl)]
       [(and (node-is-R e1-rl) (node-is-R e2-rl))  (make-R-node e1-rl e2-rl)]
       [(and (node-is-R e1-rl) (node-is-C e2-rl))  (make-S-node e1-rl e2-rl)]
       [(and (node-is-C e1-rl) (node-is-R e2-rl))  (make-S-node e2-rl e1-rl)]
       [(node-is-S e1-rl)
        (cond
           [(node-is-C e2-rl) (make-S-node
                                 (node-S-left e1-rl)
                                 (make-C-node (node-S-right e1-rl) e2-rl))]
           [(node-is-R e2-rl) (make-S-node
                                 (make-R-node (node-S-left e1-rl) e2-rl)
                                 (node-S-right e1-rl))]
           [(node-is-S e2-rl) (make-S-node
                                 (make-R-node (node-S-left e1-rl) (node-S-left e2-rl))
                                 (make-C-node (node-S-right e1-rl) (node-S-right e2-rl)))])]

       [(node-is-S e2-rl) (pe-join-subtrees e2-rl e1-rl)]))



(define (node-is-C n)
  (match n
    [`(,_ C) #t]
    [_       #f]))


(define (node-is-R n)
  (match n
    [`(,_ R) #t]
    [_       #f]))


(define (node-is-S n)
  (match n
    [`(,_ S) #t]
    [_       #f]))


(define (node-S-left n)
  (match n
    [`((+ ,e1 ,_) S) e1]
    [_ (error "cannot be here! (S left)")]))


(define (node-S-right n)
  (match n
    [`((+ ,_ ,e2) S) e2]
    [_ (error "cannot be here! (S right)")]))


(define (make-S-node r c)
  `((+ ,r ,c) S))

(define (make-C-node c1 c2)
  `((+ ,c1 ,c2) C))

(define (make-R-node r1 r2)
  `((+ ,r1 ,r2) R))

(define (test-eq a b)
   (if (equal? a b) #t (error "assert failed: " a " != " b)))

(define (test-pe-eq a b) (test-eq (pe-exp a) b))
(define (ppn exp) (begin (print (pe-exp exp)) (newline)))

(test-eq (pe-do-negate `(- 3)) -3)
(test-eq (pe-do-negate `(- (+ 3 4)) ) `(+ -3 -4))
(test-eq (pe-do-negate `(- (+ 2 (- (read)))) ) `(+ -2 (read)))
(test-eq (pe-do-negate `(+ (- 1) (- (- 2))) ) `(+ -1 2))
(test-eq (pe-do-negate `(- (- (- (+ (read) 1)))) ) `(+ (- (read)) -1))

(test-eq (node-is-C `(1 C)) #t)

(test-eq (pe-move-reads-left `1) `(1 C))
(test-eq (pe-move-reads-left `(+ 1 1) ) `((+ (1 C) (1 C)) C))
(test-eq (pe-move-reads-left `(+ 1 (read)) ) `((+ ((read) R) (1 C)) S))
(test-eq (pe-move-reads-left `(+ (read) (read)) ) `((+ ((read) R) ((read) R)) R))
(test-eq (pe-move-reads-left `(+ (- (read)) -2) ) `((+ ((- (read)) R) (-2 C)) S))

(test-eq (pe-fold-constants `((+ (1 C) (-3 C)) C) ) -2)
(test-eq (pe-fold-right-constants `((+ (1 C) (-3 C)) C) ) `(-2 C))
(test-eq (pe-fold-right-constants `((+ ((read) R) ((+ (1 C) (-3 C)) C)) S))
         `((+ ((read) R) (-2 C)) S))

(test-eq (pe-remove-node-labels `((read) R) ) `(read))
(test-eq (pe-remove-node-labels `((+ ((read) R) ((read) R)) R) ) `(+ (read) (read)))
(test-eq (pe-remove-node-labels `((+ ((- (read)) R) (-2 C)) S) ) `(+ (- (read)) -2))

(test-eq (pe-exp `(+ (- (+ (read) 9)) (+ (+ (read) 1) 5)) ) `(+ (+ (- (read)) (read)) -3))
