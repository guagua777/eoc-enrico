#lang racket

(require racket/system)
(require racket/port)

(provide test-eq with-input-from-num-list compile-arm-asm run-arm-executable)

(define (with-input-from-num-list ls proc)
    (with-input-from-string (string-join (map number->string ls) "\n")
       proc))

(define (test-eq a b)
   (if (equal? a b) (displayln "PASS") (error (format "FAIL: ~a != ~a" a  b))))

(define (compile-arm-asm filename outfilename)
  (system (format "aarch64-linux-gnu-gcc -static ~a runtime.s -o ~a" filename outfilename)))

(define (run-arm-executable filename)
  (system/exit-code (format "qemu-aarch64 ./~a" filename)))
