#lang typed/racket

(require "syntax.rkt")
(require "utilities.rkt")

(define-type Ctxt SymbAssoc)

(: ⊢-term (-> ClassTable Ctxt term Symbol))
(define (⊢-term ct Γ tm)
  (match tm
    [(term/var y) (lookup y Γ (format "Variable ~a not found" y))]
    [(term/field-acc o f)
     (define C (⊢-term ct Γ o))
     (define fs (fields ct C))
     (lookup f fs (format "Field ~a not found in class ~a" f C))]
    [(term/method-inv o m args)
     (define Cₒ (⊢-term ct Γ o))
     (match-define (type/method doms rng) (mtype ct m Cₒ))
     (define arg-tps (for/list : (Listof Symbol)
                       ((arg : term args))
                       (⊢-term ct Γ arg)))
     (unless (equal? arg-tps doms)
       (error 'method-arg-type-error
              (format
               (string-append
                "The types of the arguments [~a] don't match with "
                "the domains [~a] of the method ~a.~a")
               arg-tps doms o m)))
     rng]
    [(term/new c args)
     (define f-tps (for/list : (Listof Symbol)
                     ((f : (Pairof Symbol Symbol) (fields ct c)))
                     (cdr f)))
     (define arg-tps (for/list : (Listof Symbol)
                       ((arg : term args))
                       (⊢-term ct Γ arg)))
     (unless (equal? arg-tps f-tps)
       (error 'method-arg-type-error
              (format
               (string-append
                "The types of the constructor arguments [~a] don't match with "
                "the field types [~a] of the class ~a")
               arg-tps f-tps c)))
     c]))

(: ⊢-method (-> ClassTable decl/method Symbol Void))
(define (⊢-method ct m-decl c)
  (match-define (decl/method name params out-tp body) m-decl)
  (define param-Γ (for/list : SymbAssoc
                            ((param : decl/var params))
                    (match-define (decl/var name type) param)
                    (cons name type)))
  (define Γ (cons (cons 'self c) param-Γ))
  (check-dup-name Γ (λ ((pr : (Pairof Symbol Symbol))) (car pr))
                  (format "Multiple variable declarations in method ~a.~a." c name))
  (define body-tp (⊢-term ct Γ body))
  (unless (equal? out-tp body-tp)
       (error 'method-arg-type-error
              (format
               (string-append
                "The type of the method body [~a] don't match with "
                "the declared output type [~a] for the method ~a.~a")
               body-tp out-tp c name))))

(: ⊢-class (-> ClassTable decl/class Void))
(define (⊢-class ct c-decl)
  (match-define (decl/class name fields methods mixes) c-decl)
  (check-dup-name fields decl/var-name
                  (format "Duplicate field name in class ~a." name))
  (check-dup-name methods decl/method-name
                  (format "Duplicate method name in class ~a." name))
  (for ([m-decl : decl/method methods])
    (⊢-method ct m-decl name)))

(: ⊢-program (-> program Symbol))
(define (⊢-program p)
  (match-define (program mt ct main) p)
  (check-dup-name ct decl/class-name "Duplicate class name.")
  (for ([c-decl : decl/class ct])
    ;; classes can recursively mention classes in their body
    (⊢-class ct c-decl))
  (⊢-term ct '() main))

(provide (all-defined-out))
