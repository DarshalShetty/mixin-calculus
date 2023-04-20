#lang racket

;; Thanks to Hazel for these helper functions:
;; https://github.com/IUTypeTheoryCourse/IUTypeTheoryCourse.github.io/blob/main/assignments/starter/stlc.rkt

(require rackunit)
(define-syntax-rule (check-error exp)
  (check-exn exn:fail? (thunk exp)))

;; throw-type-error! : String ... Anything -> Void
;; Throws an error with the given format string and arguments.
(define (throw-type-error! fmt . stuff)
  (error (apply format (string-append "ERROR: " fmt) stuff)))

(define param-type=? (make-parameter equal?))
(define param-print-type (make-parameter identity))

;; assert-type-equal! : Type Type -> Void
;; Asserts that two types are equal, and error if not.
(define (assert-type-equal! t1 t2)
  (unless ((param-type=?) t1 t2)
    ;; NOTE: "~a" is kind of like "%s" in C's printf.
    (throw-type-error! "non-equal types: ~a is not ~a" ((param-print-type) t1) ((param-print-type) t2))))

(define (assert-types-equal! t1* t2*)
  (for ([t1 t1*]
        [t2 t2*])
    (assert-type-equal! t1 t2)))

(provide (all-defined-out))
