#lang typed/racket

(require typed/rackunit/text-ui)
(require typed/rackunit)

(require "tests/core.rkt")

(run-tests
 (test-suite
  "Test programs for μCarbon"
  core-tests))
