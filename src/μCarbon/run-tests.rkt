#lang typed/racket

(require typed/rackunit/text-ui)
(require typed/rackunit)

(require "tests/core.rkt")
(require "tests/mixin.rkt")

(run-tests
 (test-suite
  "Test programs for μCarbon"
  core-tests
  mixin-tests))
