#lang typed/racket

(require "syntax.rkt")
(require "utilities.rkt")

(: eval-term (-> ClassTable Env term value))
(define (eval-term ct env t)
  (match t
    [(term/var x) (lookup-env env x)]
    [(term/field-acc o f)
     (match-define (value/object c args) (eval-term ct env o))
     (lookup-field (fields ct c) args f)]
    [(term/method-inv o m args)
     (match-define (and eval-o (value/object c _))
       (eval-term ct env o))
     (define eval-args (for/list : (Listof value)
                                 ([arg : term args])
                         (eval-term ct env arg)))
     (match-define (cons xs body) (mbody ct m c))
     (define env^ (for/fold : Env
                      ([env : Env env])
                      ([x : Symbol (cons 'self xs)]
                       [arg : value (cons eval-o eval-args)])
                    (extend-env x arg env)))
     (eval-term ct env^ body)]
    [(term/new c args)
     (define eval-args (for/list : (Listof value)
                                 ([arg : term args])
                         (eval-term ct env arg)))
     (value/object c eval-args)]))

(: eval-prog (-> program value))
(define (eval-prog p)
  (match-define (program mixins classes main) p)
  (eval-term classes empty-env main))

(provide (all-defined-out))
