#lang typed/racket

(require typed/rackunit)
(require "syntax.rkt")
(require "typecheck.rkt")
(require "evaluate.rkt")

(: check-program-output (-> Program value Any))
(define (check-program-output p expected)
  (define parsed (parse p))
  (⊢-program parsed)
  (define actual (eval-prog parsed))
  (check-equal? actual expected "Actual and expected outputs don't match"))

(: check-program-type-error (-> Program Any))
(define (check-program-type-error p)
  (define parsed (parse p))
  (check-exn exn:fail? (λ () (⊢-program parsed))))

(: check-program-run-error (-> Program Any))
(define (check-program-run-error p)
  (define parsed (parse p))
  (⊢-program parsed)
  (check-exn exn:fail? (λ () (eval-prog parsed))))

(provide (all-defined-out))
