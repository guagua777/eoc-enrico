#lang racket

(require "test-util.rkt")

(test-eq
 (compile-arm-asm "aarch64-hello.s" "a.out")
 #t)

(test-eq
 (run-arm-executable "a.out")
 42)
