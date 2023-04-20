#lang typed/racket

(require typed/rackunit)
(require "../test-helpers.rkt")
(require "../syntax.rkt")

(define mixin-tests
  (test-suite
   "Test μCarbon programs that use mixins"
   (test-case
       "Simple mix"
     (check-program-output
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd))))
                      (mix M)))
         (mixin M ((fn constB () -> B (new B ())))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           constB ()))
      (value/object 'B '())))
   (test-case
    "Duplicate mixin name"
    (check-program-type-error
     '(program
       ((class A ())
        (class A ())
        (mixin M ((fn constB () -> B (new B ()))))
        (mixin M ((fn constA () -> A (new A ()))))
        (class Pair ((var fst : A)
                     (var fst : B)
                     (fn setfst ((newfst : A)) -> B
                         (new Pair (newfst (∘ self snd)))))))
       (∘ (new Pair ((new A ())
                     (new B ())))
          setfst
          ((new A ()))))))
   (test-case
       "Non-existent field type"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : C)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd))))))
         (mixin M ((fn snd () -> B (new B ())))))
        (∘ (new Pair ((new A ())
                      (new C ())))
           setfst
           ((new A ()) (new B ()))))))))

(provide mixin-tests)
