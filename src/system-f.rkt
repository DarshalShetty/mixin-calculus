#lang typed/racket/no-check
(require rackunit)
(require "utils.rkt")

;; **TERMS**
(struct term () #:transparent)
;; pure terms
(struct term/var term ([x : Symbol]) #:transparent)
(struct term/abs term ([xs : (Listof Symbol)]
                       [tps : (Listof type)]
                       [body : term])
  #:transparent)
(struct term/app term ([rator : term] [rands : (Listof term)]) #:transparent)
(struct term/tabs term ([ys : (Listof Symbol)] [body : term]) #:transparent)
(struct term/tapp term ([tm : term] [tps : (Listof term)]) #:transparent)
;; naturals
(struct term/nat term ([n : Natural]) #:transparent)
(struct term/succ term ([n : term]) #:transparent)
(struct term/case-nat term ([scrut : term]
                            [case-zero : term]
                            [pred-bind : Symbol]
                            [case-succ : term])
  #:transparent)
;; lists
(struct term/null term () #:transparent)
(struct term/cons term () #:transparent)
(struct term/case-list term ([scrut : term]
                             [case-null : term]
                             [car-bind : Symbol]
                             [cdr-bind : Symbol]
                             [case-cons : term])
  #:transparent)
;; fix
(struct term/fix ([f : term]) #:transparent)
;; let
(struct term/let ([x : Symbol] [rhs : term] [body : term]) #:transparent)

;; **TYPES**
(struct type () #:transparent)
(struct type/var type ([y : Symbol]) #:transparent)
(struct type/fun type ([doms : (Listof type)] [rng : type]) #:transparent)
(struct type/univ type ([ys : (Listof Symbol)] [body : type]) #:transparent)
(struct type/nat type () #:transparent)
(struct type/list type ([tp : type]) #:transparent)

(define-type Term (U (Listof Any) Symbol Natural))
(define-type Type (U (Listof Any) Symbol))

(: pprint (-> term Term))
(define (pprint tm)
   (match tm
     [(term/var x) x]
     [(term/abs xs tps body) `(λ ,(map (λ ([x : Symbol] [tp : type])
                                         `(,x : ,(pprint-type tp))) xs tps)
                                ,(pprint body))]
     [(term/app rator rands) `(,(pprint rator) . ,(map pprint rands))]
     [(term/tabs ys body) `(Λ ,ys ,(pprint body))]
     [(term/tapp tm tps) `(@ ,(pprint tm) . ,(map pprint-type tps))]
     [(term/nat n) n]
     [(term/succ nexp) `(succ ,(pprint nexp))]
     [(term/case-nat scrut zero pred succ)
      `(case-ℕ ,(pprint scrut)
              (0 => ,(pprint zero))
              ((succ ,pred) => ,(pprint succ)))]
     [(term/null) 'null]
     [(term/cons) 'cons]
     [(term/case-list scrut null car cdr cons)
      `(case-List ,(pprint scrut)
                  (null => ,(pprint null))
                  ((cons ,car ,cdr) => ,(pprint cons)))]
     [(term/fix f) `(fix ,(pprint f))]
     [(term/let x rhs body) `(let ((,x ,(pprint rhs))) ,(pprint body))]))

(: pprint-type (-> type Type))
(define (pprint-type tp)
  (match tp
    [(type/var y) y]
    [(type/fun doms rng) `(-> ,(map pprint-type doms) ,(pprint-type rng))]
    [(type/univ ys body) `(∀ ,ys ,(pprint-type body))]
    [(type/nat) 'ℕ]
    [(type/list tp) `(List ,(pprint-type tp))]))

(param-print-type pprint-type)

(: parse (-> Term term))
(define (parse sexp)
   (match sexp
     ['null (term/null)]
     ['cons (term/cons)]
     [`,x #:when (symbol? x) (term/var x)]
     [`,n #:when (natural? n) (term/nat n)]
     [`(λ ,var-type-pairs ,body)
      #:when (list? var-type-pairs)
      (define-values (xs tps)
        (for/foldr ([xs : (Listof Symbol) '()]
                   [tps : (Listof Any) '()])
                  ([var-type : (List Symbol ': Type) var-type-pairs])
          (match-define `(,x : ,tp) var-type)
          (values (cons x xs) (cons tp tps))))
      (term/abs xs (map parse-type tps) (parse body)) ]
     [`(Λ ,ys ,body) (term/tabs ys (parse body))]
     [`(@ ,tm . ,tps)(term/tapp (parse tm) (map parse-type tps)) ]
     [`(succ ,nexp) (term/succ (parse nexp))]
     [`(case-ℕ ,scrut
              (0 => ,zero)
              ((succ ,pred) => ,succ))
      (term/case-nat (parse scrut) (parse zero) pred (parse succ))]
     [`(case-List ,scrut
                  (null => ,null)
                  ((cons ,car ,cdr) => ,cons))
      (term/case-list (parse scrut) (parse null) car cdr (parse cons))]
     [`(fix ,f) (term/fix (parse f))]
     [`(let ((,x ,rhs)) ,body) #:when (symbol? x)
                             (term/let x (parse rhs) (parse body))]
     [`(,rator . ,rands) (term/app (parse rator) (map parse rands)) ]))

(: parse-type (-> Type type))
(define (parse-type sexp)
  (match sexp
    ['ℕ (type/nat)]
    [`,y #:when (symbol? y) (type/var y)]
    [`(-> ,doms ,rng) (type/fun (map parse-type doms) (parse-type rng))]
    [`(∀ ,ys ,body) (type/univ ys (parse-type body)) ]
    [`(List ,tp) (type/list (parse-type tp))]))

(module+ test
  (define example-sum
    `(let ([plus (fix (λ ([plus : (-> (ℕ ℕ) ℕ)])
                        (λ ([m : ℕ] [n : ℕ])
                          (case-ℕ
                           m
                           (0 => n)
                           ((succ m^) => (succ (plus m^ n)))))))])
       (let ([sum (Λ (t)
                     (fix (λ ([sum : (-> ((List t) (-> (t t) t) t) t)])
                            (λ ([ls : (List t)] [add : (-> (t t) t)] [zero : t])
                              (case-List
                               ls
                               (null => zero)
                               ((cons a d) => (add a (sum d add zero))))))))])
         (let ([ls ((@ cons ℕ) 1 ((@ cons ℕ) 2 (@ null ℕ)))])
           ((@ sum ℕ) ls plus 0)))))

  (check-equal? (pprint (parse example-sum)) example-sum))

;; TODO: implement a type equality function that also checks for α equivalence

;; **TYPES with lexical addresses (de Bruijn index) and argument indexes**
(struct lextype () #:transparent)
(struct lextype/var lextype ([idx : Natural]) #:transparent)
(struct lextype/fun lextype ([doms : (Listof lextype)] [rng : lextype]) #:transparent)
(struct lextype/univ lextype ([body : lextype]) #:transparent)
(struct lextype/nat lextype () #:transparent)
(struct lextype/list lextype ([tp : lextype]) #:transparent)

(define (pprint-lextype tp)
  (match tp
    [(lextype/var y) y]
    [(lextype/fun doms rng) `(-> ,(map pprint-lextype doms) ,(pprint-lextype rng))]
    [(lextype/univ body) `(∀ ,(pprint-lextype body))]
    [(lextype/nat) 'ℕ]
    [(lextype/list tp) `(List ,(pprint-lextype tp))]))

(param-print-type pprint-lextype)

(: ctx-idx-of (-> Γ Symbol Natural))
(define (ctx-idx-of ctx X)
  (match ctx
    ['() (error 'ctx-idx-of "Type variable ~a not found in context~n" X)]
    [`(,(binding/type X^) . ,_) #:when (eq? X X^) 0]
    [`(,(binding/type _) . ,ctx-rest) (add1 (ctx-idx-of ctx-rest X))]
    [`(,(binding/term _ _) . ,ctx-rest) (ctx-idx-of ctx-rest X)]))

(module+ test
  (check-equal? (ctx-idx-of (ext-ctx-tp
                             '(X)
                             (ext-ctx-tm
                              '(x y z)
                              (list (lextype/nat)
                                    (lextype/nat)
                                    (lextype/nat))
                              (ext-ctx-tp '(Y) '())))
                            'Y) 1)
  (check-error (ctx-idx-of (ext-ctx-tp
                             '(X)
                             (ext-ctx-tm
                              '(x y z)
                              (list (lextype/nat)
                                    (lextype/nat)
                                    (lextype/nat))
                              (ext-ctx-tp '(Y) '())))
                            'Z)))

(: tlex (-> type Γ lextype))
(define (tlex tp (ctx '()))
  (match tp
    [(type/var y) (lextype/var (ctx-idx-of ctx y))]
    [(type/fun doms rng)
     (define lex-doms (for/list ([dom doms])
                        (tlex dom ctx)))
     (lextype/fun lex-doms (tlex rng ctx))]
    [(type/univ ys body)
     (for/fold ([lex-body (tlex body (ext-ctx-tp ys ctx))])
               ([y ys])
       (lextype/univ lex-body))]
    [(type/nat) (lextype/nat)]
    [(type/list tp) (lextype/list (tlex tp ctx))]))

(module+ test
  (check-equal? (tlex (parse-type '(∀ (X Y) (List Y))))
                (lextype/univ (lextype/univ (lextype/list (lextype/var 0)))))
  (check-equal? (tlex (parse-type '(∀ (X Y) (List X))))
                (lextype/univ (lextype/univ (lextype/list (lextype/var 1)))))
  (check-equal? (tlex (parse-type '(∀ (X Y) (-> ((∀ (Y) Y)) Y))))
                (lextype/univ (lextype/univ (lextype/fun (list (lextype/univ (lextype/var 0))) (lextype/var 0)))))
  (check-equal? (tlex (parse-type '(∀ (X Y) (-> ((∀ (Y) X)) X))))
                (lextype/univ (lextype/univ (lextype/fun (list (lextype/univ (lextype/var 2))) (lextype/var 1)))))
  (check-equal? (tlex (parse-type '(List (∀ (X Y) (∀ (X Y) (-> (X Y) ℕ))))))
                (tlex (parse-type '(List (∀ (X Y) (∀ (V W) (-> (V W) ℕ)))))))
  (check-not-equal? (tlex (parse-type '(List (∀ (X Y) (∀ (X Y) (-> (X Y) ℕ))))))
                    (tlex (parse-type '(List (∀ (X Y) (∀ (V W) (-> (W V) ℕ)))))))
  (check-error (tlex (parse-type '(∀ (X Y) (-> ((∀ (Y) Z)) X)))))
  (check-error (tlex (parse-type '(List (∀ (X Y) (∀ (X Y) (-> (X Z) ℕ))))))))

;; ***Context***
(struct binding ())
(struct binding/term ([var : Symbol] [tp : lextype]) #:transparent)
(struct binding/type ([var : Symbol]) #:transparent)
(define-type Γ (Listof (U binding/term binding/type)))

(: ∈ (-> Symbol Γ lextype))
(define (∈ x ctx)
  (match ctx
    ['() (error '∈ "Term variable not bound: ~a~n" x)]
    [`(,(binding/term var tp) . ,_) #:when (eqv? var x)
                                    tp]
    [`(,(binding/type var) . ,rest-ctx) (↑ 1 0 (∈ x rest-ctx))]
    [`(,_ . ,rest-ctx) (∈ x rest-ctx)]))

(module+ test
  (check-equal? (∈ 'f (ext-ctx-tm
                       '(x)
                       `(,(lextype/var 0))
                       (ext-ctx-tp
                        '(X)
                        (ext-ctx-tm
                         '(f) `(,(lextype/fun
                                  (list (lextype/univ (lextype/var 0))) (lextype/var 0))) '()))))
                (lextype/fun
                          (list (lextype/univ (lextype/var 0))) (lextype/var 1))))

#;
(: ∈-tp (-> Symbol Γ (U #f type/var)))
#;
(define (∈-tp X ctx)
  (match ctx
    ['() (error '∈-tp "Type variable not bound: ~a~n" X)]
    [`(,(binding/type var) . ,_) #:when (eqv? var X)
                                 (type/var var)]
    [`(,_ . ,rest-ctx) (∈-tp X rest-ctx)]))

(: ext-ctx-tm (-> (Listof Symbol) (Listof lextype) Γ Γ))
(define (ext-ctx-tm vars tps ctx)
  (for/fold ([ctx^ ctx])
            ([var vars]
             [tp tps])
    (cons (binding/term var tp) ctx^)))

(: ext-ctx-tp (-> (Listof Symbol) Γ Γ))
(define (ext-ctx-tp vars ctx)
  (for/fold ([ctx^ ctx])
            ([var vars])
    (cons (binding/type var) ctx^)))

;; Ensures that free type variables in tp are in ctx.
#;
(: ⊢ (-> Γ type lextype))
#;
(define (⊢ ctx tp)
  (match tp
    [(type/var y) (cond
                    [(∈-tp y ctx) => (λ (v) v)]
                    [else (error 'Γ⊢T "free variable ~a not found in type context ~a" y ctx)])]
    [(type/fun doms rng) (type/fun (map (λ (dom) (⊢ ctx dom)) doms)
                                   (⊢ ctx rng))]
    [(type/univ ys body) (type/univ ys (⊢ (ext-ctx-tp ys ctx)
                                          body))]
    [(type/nat) tp]
    [(type/list tp) (type/list (⊢ ctx tp))]))

(: ↑ (-> Integer Integer lextype lextype))
(define (↑ d c tp)
  (match tp
    [(lextype/var k)
     (cond
       [(< k c) (lextype/var k)]
       [(>= k c) (lextype/var (+ k d))])]
    [(lextype/fun doms rng)
     (define shifted-doms (for/list ([dom doms])
                            (↑ d c dom)))
     (lextype/fun shifted-doms (↑ d c rng))]
    [(lextype/univ body) (lextype/univ (↑ d (add1 c) body))]
    [(lextype/nat) (lextype/nat)]
    [(lextype/list tp) (lextype/list (↑ d c tp))]))

(: subst (-> Natural lextype lextype lextype))
(define (subst j s tp)
  (match tp
    [(lextype/var k)
     (cond
       [(eq? k j) s]
       [else (lextype/var k)])]
    [(lextype/fun doms rng)
     (define substed-doms (for/list ([dom doms])
                            (subst j s dom)))
     (lextype/fun substed-doms (subst j s rng))]
    [(lextype/univ body) (lextype/univ (subst (add1 j) (↑ 1 0 s) body))]
    [(lextype/nat) (lextype/nat)]
    [(lextype/list tp) (lextype/list (subst j s tp))]))

(: type-apply (-> lextype (Listof lextype) lextype))
(define type-apply
  (λ (rator-tp rand-tps)
    (match rand-tps
      ['() rator-tp]
      [`(,rand-tp . ,rest-tps)
       (match-define (lextype/univ body-tp) rator-tp)
       (type-apply (↑ -1 0 (subst 0 (↑ 1 0 rand-tp) body-tp)) rest-tps)])))

(module+ test
  (check-equal? (type-apply (tlex (parse-type '(∀ (X Y) (List (-> (X) (∀ (Y) Y))))) '())
                            (list (tlex (parse-type '(∀ (Y) Y)) '())
                                  (lextype/nat)))
                (tlex (parse-type '(List (-> ((∀ (Y) Y)) (∀ (Y) Y)))))))

(: typecheck (-> term Γ lextype))
(define typecheck
  (λ (tm ctx)
    (match tm
     [(term/var x) (∈ x ctx)]
     [(term/abs xs tps body)
      (define lex-tps (for/list ([tp tps])
                        (tlex tp ctx)))
      (define body-tp (typecheck body (ext-ctx-tm xs lex-tps ctx)))
      (lextype/fun lex-tps body-tp)]
     [(term/app rator rands)
      (match-define (lextype/fun doms rng) (typecheck rator ctx))
      (define rand-tps (for/list ([rand rands])
                         (typecheck rand ctx)))
      (assert-types-equal! doms rand-tps)
      rng]
     [(term/tabs ys body)
      (for/fold ([body-tp (typecheck body (ext-ctx-tp ys ctx))])
                ([y ys])
        (lextype/univ body-tp))]
     [(term/tapp tm tps)
      (define lex-tps (for/list ([tp tps])
                        (tlex tp ctx)))
      (type-apply (typecheck tm ctx) lex-tps)]
     [(term/nat n)
      #:when (natural? n)
      (lextype/nat)]
     [(term/succ nexp)
      (assert-type-equal! (typecheck nexp ctx) (lextype/nat))
      (lextype/nat)]
     [(term/case-nat scrut zero pred succ)
      (assert-type-equal! (typecheck scrut ctx) (lextype/nat))
      (define zero-tp (typecheck zero ctx))
      (define succ-tp (typecheck succ (ext-ctx-tm `(,pred) `(,(lextype/nat)) ctx)))
      (assert-type-equal! zero-tp succ-tp)
      zero-tp]
     [(term/null)
      (lextype/univ (lextype/list (lextype/var 0)))]
     [(term/cons)
      (lextype/univ (lextype/fun `(,(lextype/var 0) ,(lextype/list (lextype/var 0)))
                              (lextype/list (lextype/var 0))))]
     [(term/case-list scrut null car cdr cons)
      (match-define (lextype/list member-tp) (typecheck scrut ctx))
      (define null-tp (typecheck null ctx))
      (define cons-tp (typecheck cons
                                 (ext-ctx-tm `(,car ,cdr)
                                             `(,member-tp ,(lextype/list member-tp))
                                             ctx)
                                ))
      (assert-type-equal! null-tp cons-tp)
      null-tp]
     [(term/fix f)
      ;; fix :: (((a* -> b) -> a* -> b) -> a* -> b)
      (match-define (lextype/fun
                     (list
                      (lextype/fun doms1 rng1))
                     (lextype/fun doms2 rng2))
        (typecheck f ctx))
      (assert-types-equal! doms1 doms2)
      (assert-type-equal! rng1 rng2)
      (lextype/fun doms1 rng1)]
     [(term/let x rhs body)
      (define rhs-tp (typecheck rhs ctx))
      (typecheck body (ext-ctx-tm `(,x) `(,rhs-tp) ctx))])))

(module+ test
  (check-equal? (typecheck (parse example-sum) '())
                (lextype/nat))
  (check-error (typecheck (parse '(Λ (X) (λ ((x : (-> (Y) Y))) x)) '())))
  (check-error (typecheck
                (parse
                 '(Λ (X) (λ ((f : (-> (X) X)))
                           (Λ (X)
                              (λ ((x : X))
                                (f x))))))
                '())))

;; **VALUES**
(struct value () #:transparent)
(struct value/closure value ([xs : (Listof Symbol)]
                             [tps : (Listof type)] ;; left empty during type erased
                                                   ;; evaluation
                             [body : term]
                             [tmenv : TermEnv]
                             [tpenv : TypeEnv])
  #:transparent)
(struct value/tclosure value ([ys : (Listof Symbol)]
                              [body : term]
                              [tmenv : TermEnv]
                              [tpenv : TypeEnv]) #:transparent)
(struct value/nat value ([n : Natural]) #:transparent)

(struct value/list value () #:transparent)

;;Values corresponding to null and cons syntactic constructs. These
;;values can be also be returned by a term in a type application expression,
;;similar to a value/tclosure.
(struct value/null value () #:transparent)
(struct value/cons value () #:transparent)

;; Values returned after applying a type to value/null and value/cons. Here,
;; type field is #f during type erased evaluation.
(struct value/tnull value/list ([type : (U type #f)]) #:transparent)
(struct value/tcons value ([type : (U type #f)]) #:transparent)
;; The value above can also be returned after evaluating the rator in a lambda
;; application expression, similar to value/closure.

;; Value returned after applying a car and cdr to value/tcons.
(struct value/tcons-ht value/list ([type : (U type #f)]
                                   [head : value]
                                   [tail : value/list]) #:transparent)

;; **ENVIRONMENTS**
;;
;; variables wont be able to share the same symbol.
(define-type TermEnv (Listof (U (Pairof Symbol value)
                                (List 'fixed Symbol value/closure))))
(define-type TypeEnv (Listof (Pairof Symbol type)))

(: empty-env (U TermEnv TypeEnv))
(define empty-env '())

(: ext-env (U (-> (Listof Symbol) (Listof value) TermEnv TermEnv)
              (-> (Listof Symbol) (Listof type) TypeEnv TypeEnv)))
(define (ext-env xs vs env)
  (append (map cons xs vs) env))

(: fix-ext-env (-> Symbol value/closure TermEnv))
(define (fix-ext-env x clos tmenv)
  (cons `(fixed ,x ,clos) tmenv))

(: apply-env (U (-> TermEnv Symbol value)
                (-> TypeEnv Symbol type)))
(define (apply-env env x)
  (match env
    ['() (error 'apply-env "unbound variable ~a" x)]
    [`((fixed ,f ,(and v (value/closure xs tps body tmenv tpenv))) . ,_)
     #:when (eqv? f x)
     (value/closure xs tps body (fix-ext-env f v tmenv) tpenv)]
    [`((,x^ . ,v) . ,_) #:when (eqv? x^ x) v]
    [`(,_ . ,rest-env) (apply-env rest-env x)]))

(: evaluate (-> term TermEnv TypeEnv value))
(define (evaluate tm tmenv tpenv)
  (match tm
     [(term/var x) (apply-env tmenv x)]
     [(term/abs xs tps body) (value/closure xs tps body tmenv tpenv)]
     [(term/app rator rands)
      (match (evaluate rator tmenv tpenv)
        [(value/closure xs _ body tmenv^ tpenv^)
         (define args (map (λ (rand) (evaluate rand tmenv tpenv)) rands))
         (evaluate body (ext-env xs args tmenv^) tpenv^)]
        [(value/tcons tp)
         (match-define `(,head-expr ,tail-expr) rands)
         (define head (evaluate head-expr tmenv tpenv))
         (define tail (evaluate tail-expr tmenv tpenv))
         (value/tcons-ht tp head tail)])]
     [(term/tabs ys body) (value/tclosure ys body tmenv tpenv)]
     [(term/tapp tm tps)
      (match (evaluate tm tmenv tpenv)
        [(value/tclosure ys body tmenv^ tpenv^)
         (define closed-tps (map (λ (tp) (reify tp tpenv)) tps))
         (evaluate body tmenv^ (ext-env ys closed-tps tpenv^))]
        [(value/null)
         (match-define `(,tp) tps)
         (define closed-tp (reify tp tpenv))
         (value/tnull closed-tp)]
        [(value/cons)
         (match-define `(,tp) tps)
         (define closed-tp (reify tp tpenv))
         (value/tcons closed-tp)])]
     [(term/nat n) (value/nat n)]
     [(term/succ nexp)
      (match-define (value/nat n) (evaluate nexp tmenv tpenv))
      (value/nat (add1 n))]
     [(term/case-nat scrut zero pred succ)
      (match-define (value/nat n) (evaluate scrut tmenv tpenv))
      (if (zero? n)
          (evaluate zero tmenv tpenv)
          (evaluate succ (ext-env `(,pred) `(,(value/nat (sub1 n))) tmenv) tpenv))]
     [(term/null) (value/null)]
     [(term/cons) (value/cons)]
     [(term/case-list scrut null-case car-var cdr-var cons-case)
      (define list-val (evaluate scrut tmenv tpenv))
      (match list-val
        [(value/tnull tp) (evaluate null-case tmenv tpenv)]
        [(value/tcons-ht _ hd tl) (evaluate cons-case
                                            (ext-env `(,car-var ,cdr-var)
                                                     `(,hd ,tl)
                                                     tmenv)
                                            tpenv)])]
     [(term/fix f)
      (match-define (value/closure `(,fix) _ fun tmenv^ tpenv^)
        (evaluate f tmenv tpenv))
      (define fix-closure (evaluate fun tmenv^ tpenv^))
      (match-define (value/closure xs tps body tmenv^^ tpenv^^)
        fix-closure)
      (value/closure xs tps body
                     (fix-ext-env fix fix-closure tmenv^^)
                     tpenv^^)]
     [(term/let x rhs body)
      (evaluate body (ext-env `(,x) `(,(evaluate rhs tmenv tpenv)) tmenv) tpenv)]))

(: reify (-> type TypeEnv type))
(define (reify tp env)
  (match tp
    [(type/var y) (apply-env env y)]
    [(type/fun doms rng) (type/fun (map (λ (dom) (reify dom env)) doms) rng)]
    [(type/univ ys body) (type/univ ys (reify body
                                              (ext-env ys
                                                       (map (λ (y)
                                                              (type/var y))
                                                            ys)
                                                       env)))]
    [(type/nat) 'ℕ]
    [(type/list tp) (type/list (reify tp env))]))

(module+ test
  (check-equal? (evaluate
                 (parse '(@ (Λ (X) (λ ([l : (List X)]) l))
                               ℕ))
                 '() '())
                (value/closure '(l)
                               `(,(type/list (type/var 'X)))
                               (term/var 'l)
                               '()
                               '((X . ℕ))))
  (check-equal?
   (evaluate
    (parse
     '(let ([plus (fix (λ ([p : (-> (ℕ ℕ) ℕ)])
                         (λ ([m : ℕ] [n : ℕ])
                           (case-ℕ
                            m
                            (0 => n)
                            ((succ m^) => (succ (p m^ n)))))))])
        (plus 2 3)))
    empty-env empty-env)
   (value/nat 5))
  (check-equal?
   (evaluate
    (parse
     '(let ([almost-plus ((λ ([x : (-> ((-> (ℕ ℕ) ℕ)) (-> (ℕ ℕ) ℕ))]) x)
                          (λ ([p : (-> (ℕ ℕ) ℕ)])
                           (λ ([m : ℕ] [n : ℕ])
                             (case-ℕ
                              m
                              (0 => n)
                              ((succ m^) => (succ (p m^ n)))))))])
        (let ([plus (fix almost-plus)])
          (let ([silly-sum (fix (let ([x 1])
                                  (λ ([ss : (-> (List ℕ) ℕ)])
                                    (let ([y 2])
                                      (λ ([l : (List ℕ)])
                                        (case-List
                                         l
                                         (null => 0)
                                         ((cons a d)
                                          =>
                                          (plus a (plus x (plus y (ss d)))))))))))])
            (let ([ls ((@ cons ℕ) 1 ((@ cons ℕ) 2 (@ null ℕ)))])
              (silly-sum ls))))))
    empty-env empty-env)
   (value/nat 9))
  (check-equal? (evaluate (parse example-sum) empty-env empty-env)
                (value/nat 3)))

(module+ test
  ;; computing factorial of 5 using pure system F and Church encoding

  (define ℕ
    '(∀ (α)
        (-> (α (-> (α) α))
            α)))

  ;; A few encoded numbers

  (define zero
    '(Λ (α)
        (λ ([x : α] [f : (-> (α) α)])
          x)))

  (check-equal? (typecheck (parse zero) '()) (tlex (parse-type ℕ)))

  (define one
    '(Λ (α)
        (λ ([x : α] [f : (-> (α) α)])
          (f x))))

  (check-equal? (typecheck (parse one) '()) (tlex (parse-type ℕ)))

  ;; Elimination form for encoded naturals
  (define iterℕ
    `(Λ (α)
        (λ ([base : α] [step : (-> (α) α)] [n : ,ℕ])
          ((@ n α) base step))))

  (check-equal? (typecheck (parse iterℕ) '())
                (tlex (parse-type
                       `(∀ (α)
                           (-> (α (-> (α) α) ,ℕ)
                               α)))))

  ;; This function uses this System F implementation's in-built natural type to
  ;; decode the Church Encoding of natural numbers.
  (: decodeℕ (-> term Natural))
  (define (decodeℕ tm)
    (define tp (typecheck tm '()))
    (unless (equal? tp (tlex (parse-type ℕ)))
      (error 'decodeℕ
             "Input term should have the type of encoded naturals ~a.
            Got type: ~a"
             ℕ tp))
    (match-define (value/nat n)
      (evaluate (term/app (term/tapp tm (list (type/nat)))
                          (list (parse 0)
                                (parse '(λ ([x : ℕ]) (succ x)))))
                empty-env empty-env))
    n)

  (check-equal? (decodeℕ (parse zero)) 0)
  (check-equal? (decodeℕ (parse one)) 1)

  ;; Church encoded successor function
  (define succℕ
    `(λ ([n : ,ℕ])
       (Λ (α)
          (λ ([x : α] [f : (-> (α) α)])
            (f ((@ n α) x f))))))

  (check-equal? (decodeℕ (parse `(,succℕ ,zero))) 1)
  (check-equal? (decodeℕ (parse `(,succℕ (,succℕ (,succℕ ,one))))) 4)

  ;; Church encoded addition
  (define +ℕ
    `(λ ([m : ,ℕ] [n : ,ℕ])
       ((@ ,iterℕ ,ℕ) n
                      (λ ([r : ,ℕ])
                        (,succℕ r))
                      m)))

  (check-equal? (decodeℕ (parse `(,+ℕ ,one ,one))) 2)
  (check-equal? (decodeℕ (parse `(,+ℕ (,succℕ ,one) (,succℕ (,succℕ ,one))))) 5)

  ;; Church encoded multiplication
  (define *ℕ
    `(λ ([m : ,ℕ] [n : ,ℕ])
       ((@ ,iterℕ ,ℕ) ,zero
                      (λ ([r : ,ℕ])
                        (,+ℕ n r))
                      m)))

  (check-equal? (decodeℕ (parse `(,*ℕ ,one ,one))) 1)
  (check-equal? (decodeℕ (parse `(,*ℕ (,succℕ ,one) (,succℕ (,succℕ ,one))))) 6)

  ;; Church encoded pair of naturals
  (define ℕ×ℕ
    `(-> ((-> (,ℕ ,ℕ) ,ℕ))
         ,ℕ))

  ;; encoded pair introduction form
  (define consℕ
    `(λ ([e₁ : ,ℕ] [e₂ : ,ℕ])
       (λ ([f : (-> (,ℕ ,ℕ) ,ℕ)])
         (f e₁ e₂))))

  ;; encoded pairs elimination forms
  (define fstℕ
    `(λ ([p : ,ℕ×ℕ])
       (p (λ ([x : ,ℕ] [y : ,ℕ])
            x))))
  (define sndℕ
    `(λ ([p : ,ℕ×ℕ])
       (p (λ ([x : ,ℕ] [y : ,ℕ])
            y))))

  (check-equal? (decodeℕ (parse `(,fstℕ
                                  (,consℕ (,succℕ (,succℕ ,one))
                                          (,succℕ ,one)))))
                3)
  (check-equal? (decodeℕ (parse `(,sndℕ
                                  (,consℕ (,succℕ (,succℕ ,one))
                                          (,succℕ ,one)))))
                2)

  ;; Church encoded factorial
  (define !ℕ
    `(λ ([n : ,ℕ])
       (,sndℕ
        ((@ ,iterℕ ,ℕ×ℕ) (,consℕ ,zero ,one)
                         (λ ([r : ,ℕ×ℕ])
                           (,consℕ (,succℕ (,fstℕ r))
                                   (,*ℕ (,succℕ (,fstℕ r))
                                        (,sndℕ r))))
                         n))))

  ;; 5!
  (define 5!
    `(,!ℕ (,succℕ (,succℕ (,succℕ (,succℕ (,succℕ ,zero)))))))

  (check-equal? (decodeℕ (parse 5!)) 120))
