#lang typed/racket

(require "syntax.rkt")

(define-type ClassTable (Listof decl/class))
(define-type SymbAssoc (Listof (Pairof Symbol Symbol)))

(: check-dup-name (All (A) (-> (Listof A) (-> A Symbol) String
                             Void)))
(define (check-dup-name ls f message)
  (define duplicate (check-duplicates (map f ls)))
  (when duplicate
    (error 'typerror-duplicate "~s ~a used multiple times" message duplicate)))

(: lookup-CT (-> ClassTable Symbol decl/class))
(define (lookup-CT ct c)
  (match ct
    [`() (error 'lookup-CT "Class not found: ~a" c)]
    [`(,(and cdecl (decl/class n _ _)) . ,rest-ct)
     #:when (eqv? n c)
     cdecl]
    [`(,(and cdecl (decl/class n _ _)) . ,rest-ct)
     (lookup-CT rest-ct c)]))


(: fields (-> ClassTable Symbol SymbAssoc))
(define (fields ct c)
  (for/list : SymbAssoc
      ([field : decl/var (decl/class-fields (lookup-CT ct c))])
    (cons (decl/var-name field) (decl/var-type field))))

(: lookup-method (-> (Listof decl/method) Symbol decl/method))
(define (lookup-method m-decls m)
  (match m-decls
    ['() (error 'lookup-method "Method not found: ~a" m)]
    [`(,(and m-decl (decl/method name _ _ _)) . ,rest-decls)
     #:when (eqv? name m) m-decl]
    [`(,(and m-decl (decl/method name _ _ _)) . ,rest-decls)
     (lookup-method rest-decls m)]))

(: mtype (-> ClassTable Symbol Symbol type/method))
(define (mtype ct m c)
  (match-define (decl/method _ params out-type _)
    (lookup-method (decl/class-methods (lookup-CT ct c)) m))
  (define doms (for/list : (Listof Symbol)
                         ([param : decl/var params])
                 (decl/var-type param)))
  (type/method doms out-type))

(: mbody (-> ClassTable Symbol Symbol (Pairof (Listof Symbol) term)))
(define (mbody ct m c)
  (match-define (decl/method _ params _ body)
    (lookup-method (decl/class-methods (lookup-CT ct c)) m))
  (define xs (for/list : (Listof Symbol)
                         ([param : decl/var params])
                 (decl/var-name param)))
  (cons xs body))

(: lookup-field (-> SymbAssoc (Listof value) Symbol
                  value))
(define (lookup-field f-decls vals f)
  (match* (f-decls vals)
    [('() '()) (error 'lookup-field "Field not found: ~a" f)]
    [(`(,(and f-decl (cons name _)) . ,rest-decls) `(,a-val . ,d-vals))
     #:when (eqv? name f) a-val]
    [(`(,(and f-decl (cons name _)) . ,rest-decls) `(,a-val . ,d-vals))
     (lookup-field rest-decls d-vals f)]))

(define-type Env (Listof (Pairof Var value)))

(: empty-env Env)
(define empty-env '())

(: extend-env (-> Var value Env Env))
(define (extend-env n v env)
  (cons (cons n v) env))

(: lookup-env (-> Env Var value))
(define (lookup-env env var)
  (match env
    [`() (error 'lookup-env "Variable not found: ~a" var)]
    [`((,var^ . ,val) . ,rest-env)
     #:when (eqv? var^ var)
     val]
    [`((,var^ . ,val) . ,rest-env)
     (lookup-env rest-env var)]))

(: lookup (All (K V)
            (-> K (Listof (Pairof K V)) String
              V)))
(define (lookup key ls message)
  (let ([res (assv key ls)])
    (if res
        (cdr res)
        (error message))))

(provide (all-defined-out))
