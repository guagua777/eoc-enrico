#lang racket

(provide rint-tests)

(require rackunit)
(require "test-util.rkt")
(require "../rint.rkt")

(define eight (Int 8))
(define rd (Prim 'read '()))
(define neg-eight (Prim '- (list eight)))
(define ast1.1 (Prim '+ (list rd neg-eight)))
(define program (Program '() ast1.1))

(define rint-tests
 (test-suite
  "RInt interpretation tests"
  (test-case
   "program with input"
   (check-equal?
    -5
    (with-input-from-num-list '(3)
       (lambda () (interp-RInt program)))))
  (test-case 
   "simple difference"
   (check-equal?
    -3
    (interp-RInt (Program '() (Prim '- (list (Int 5) (Int 8)))))))))


