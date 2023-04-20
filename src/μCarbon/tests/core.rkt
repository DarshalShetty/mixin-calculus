#lang typed/racket

(require typed/rackunit)
(require "../test-helpers.rkt")
(require "../syntax.rkt")

(define core-tests
  (test-suite
   "Test programs using core μCarbon features"
   (test-case
       "Pairs"
     (check-program-output
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))
      (value/object 'Pair (list (value/object 'A '())
                                (value/object 'B '())))))
   (test-case
       "Unbound Variable"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (x (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Non-existent field access"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self scd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Non-existent method invocation"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setsnd
           ((new A ()))))))
   (test-case
       "Method args type mismatch"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new B ()))))))
   (test-case
       "Method args length mismatch"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()) (new B ()))))))
   (test-case
       "Constructor args type mismatch"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new B ())
                      (new A ())))
           setfst
           ((new A ()))))))
   (test-case
       "Same variable name use"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((self : A)) -> Pair
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Method return type mismatch"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var snd : B)
                      (fn setfst ((newfst : A)) -> B
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Duplicate field name"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var fst : B)
                      (fn setfst ((newfst : A)) -> B
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Duplicate method name"
     (check-program-type-error
      '(program
        ((class A ())
         (class B ())
         (class Pair ((var fst : A)
                      (var fst : B)
                      (fn setfst ((newfst : A)) -> B
                          (new Pair (newfst (∘ self snd))))
                      (fn setfst ((newsnd : A)) -> B
                          (new Pair ((∘ self fst) newsnd))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))
   (test-case
       "Duplicate class name"
     (check-program-type-error
      '(program
        ((class A ())
         (class A ())
         (class Pair ((var fst : A)
                      (var fst : B)
                      (fn setfst ((newfst : A)) -> B
                          (new Pair (newfst (∘ self snd)))))))
        (∘ (new Pair ((new A ())
                      (new B ())))
           setfst
           ((new A ()))))))))

(provide core-tests)
