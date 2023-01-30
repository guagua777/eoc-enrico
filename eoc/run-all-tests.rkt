#lang racket

(require rackunit rackunit/text-ui)
(require "tests/all-tests.rkt")
(require "complete-tests.rkt")

(run-tests all-tests)
(run-tests complete-tests)

