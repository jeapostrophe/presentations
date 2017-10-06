#lang racket/base
(require "title.rkt"
         racket/fixnum
         racket/file
         racket/class
         racket/gui/base
         racket/runtime-path
         images/icons/style
         images/icons/control
         slideshow
         slideshow/code
         (prefix-in pl: plot))

(define-runtime-path r "r")
(define megas
  (for/vector ([i (in-range 1 4)])
    (bitmap (build-path r (format "mega~a.png" i)))))

;; xxx add line numbers to "code"

(define (introduce topic-l name affiliation)
  (slide
   (cc-superimpose
    plt-title-background
    (vr-append
     (vc-append
      (scale/improve-new-text (t (first topic-l)) 3)
      (scale/improve-new-text (t (second topic-l)) 2)
      (scale/improve-new-text (t (third topic-l)) 1.5)
      (scale/improve-new-text (t (fourth topic-l)) 1.5))
     (vr-append
      (scale/improve-new-text (t name) 1)
      (scale/improve-new-text (t (string-append affiliation " & PLT")) 0.5))))))

(define-syntax-rule (cslide e)
  (slide (lt-superimpose e (blank client-w client-h))))

(define (bitslide i)
  (slide (scale-to-fit (bitmap (build-path r i)) client-w client-h)))

(define (tslide title)
  ;; XXX
  (slide (t title)))

(module+ main
  (current-main-font "Verdana")
  (set-page-numbers-visible! #t)
  (current-keyword-list (append (list "provide/contract"
                                      "match-lambda"
                                      "match-define"
                                      "struct"
                                      "<-" "<~"
                                      "for/fold"
                                      "in-range"
                                      "define-values"
                                      "bytes-set!"
                                      "floor"
                                      "-" "+" "-" "<" "*" "/"
                                      "make-bytes"
                                      "vector-ref" "vector"
                                      "->")
                                (current-keyword-list)))


  (define progress -1)
  ;; xxx
  (define total-slide-count 75)
  (define default-slide-assembler (current-slide-assembler))  
  (current-slide-assembler
   (λ (title title-sep content-pict)
     (set! progress (add1 progress))

     (define which-m (modulo progress (vector-length megas)))
     (define the-m (vector-ref megas which-m))
     (define the-m-w (pict-width the-m))
     (define bar-w (- (* client-w (/ progress total-slide-count))
                      the-m-w))
     
     (lb-superimpose
      (cc-superimpose
       (blank client-w client-h)
       (default-slide-assembler
         title title-sep content-pict))
      (if (positive? bar-w)
        (hc-append
         (filled-rectangle #:color "Thistle"
                           bar-w
                           5)
         the-m)
        (blank)))))

  (introduce
   (list "Lessons"
         "in Software Reliability"
         "from the"
         "Graybeards")
   "Jay McCarthy"
   "UMass Lowell")

  (tslide "Introduction")
  ;; We are going to talk about a bunch of different ways to know that
  ;; your software/process/etc is correct. Why do we care about
  ;; correctness? In some domains, incorrectness is dangerous
  ;; (explosions), in others it is expensive (finance), but in most it
  ;; is a waste of your employee's and customer's time, which is
  ;; expensive in a different way.
  (slide
   'next
   (item "Testing")
   (item "Type Safety")
   (item "Software Contracts")
   (item "Property-based Testing")
   (item "Formal Verification")
   (item "Model Checking"))

  (tslide "Testing")
  ;; We know what testing is
  (slide
   (code (define (f x) ....)
         (code:comment "f(7) should be 14")
         (code:comment "f(10) should be 20")))
  ;; Automated vs manual tests
  (slide
   (code (define (f x) ....)
         (check-equal? (f 7) 14)
         (check-equal? (f 10) 20)))  
  ;; Mock objects and internal APIs
  (tslide "Why it is hard")
  ;; The problem of testing (finite tests vs infinite domain)
  (tslide "The fundamental problem of testing")
  (slide
   (code
    (= (* (      2 users)
          (      5 request per second)
          (     10 second average session length))
       (       100 test scenarios))))
  (slide
   (code
    (= (* (1000000 users)
          (      5 request per second)
          (     10 second average session length))
       (  50000000 test scenarios))))
  
  (tslide "Type Safety")
  ;; What a type is
  (slide
   (code
    (: f (Number -> Number))
    (define (f x) ....)))
  (slide
   (item "Prediction of the behavior of a program")
   (item "Expressed programmatically")
   (item "Automatically checked"))
  ;; Soundness & Completeness
  (tslide "Soundness")
  (tslide "Completeness")
  ;; Static analysis
  (tslide "The fundamental problem of types")
  (slide
   (code
    (: f (Number -> Number-that-is-twice-as-big))
    (define (f x) ....)))
  (tslide "Problem 1: Representing Your Program's Behavior in Types")
  ;; Designing a type that avoids errors (None vs NULL, optional fields, error value)
  (slide
   (code
    (: db-lookup (Database Key -> Record))))
  (slide
   (code
    (: db-lookup (Database Key -> Record-or-NULL))))
  (slide
   (code
    (: open-file (Path -> Integer))))
  (slide
   (code
    (data (Maybe a)
          (None)
          (Some a))
    (: try-to-open-file (Path -> (Maybe Integer)))))
  (tslide "Solution 1: Make errors unrepresentable")
  (slide
   (code
    (code:comment "If person structure has a name, then")
    (code:comment "it must include a first and last name")
    (struct Person ([Id Integer]
                    [FName String-or-NULL]
                    [LName String-or-NULL]
                    [Awesomeness Float]))))
  (slide
   (code
    (struct NamePair ([FName String]
                      [LName String]))
    (struct Person ([Id Integer]
                    [Name (Maybe NamePair)]
                    [Awesomeness Float]))))
  (tslide "Problem 2: Only some languages provide types")
  (tslide "Solution: Embed the type in the language")
  
  (tslide "Software Contracts")
  ;; In/Out contracts
  (slide
   (code
    (define (f x) ....)
    (provide/contract
     [f (-> number? number?)])))
  ;; Dependent contracts
  (slide
   (code
    (define (f x) ....)
    (provide/contract
     [f
      (->i ([x number?])
           [ans (x) (and/c number?
                           (< x ans))])])))
  (slide
   (code
    (define (f x) ....)
    (provide/contract
     [f
      (->i ([x number?])
           [ans (x) (and/c number?
                           (= (* 2 x) ans))])])))
  (tslide "Only reports violations at runtime")
  (slide
   (code
    (code:comment "expected a number?, given true")
    (code:comment "")
    (code:comment "expected a number?, returned \"eight\"")))
  ;; Blame
  (tslide "Identifies at-fault party")
  (slide
   (code
    (code:comment "expected a number?, given true")
    (code:comment "    at caller on line 8")
    (code:comment "")
    (code:comment "expected a number?, returned \"eight\"")
    (code:comment "    at function definition")))
  ;; Higher-order contracts
  (slide
   (code
    (provide/contract
     [sort (->i ([the-list (listof number?)]
                 [compare (-> number? number?
                              boolean?)])
                [ans (listof number?)])])
    (code:comment "sort expected list of number?, given list of bool")
    (code:comment "    at caller on line 17")
    (code:comment "")
    (code:comment "compare expected number?, given string")
    (code:comment "    at function definition")
    (code:comment "")
    (code:comment "compare returned number, expected bool")
    (code:comment "    at caller on line 17")))
  (tslide "The fundamental problem of contracts")
  (tslide "Identifies, does not find, faults")
  (tslide "Solution: Testing")

  (tslide "Property-based Testing")
  ;; Types are examples of properties
  (tslide "Types and Contracts are properties")
  ;; There are other simple properties
  (slide
   (code
    (-> number? number?)
    (code:comment "is the same as")
    (forall (x)
            (implies (number? x)
                     (number? (f x))))
    (code:comment "is the same as")
    (∀ (x)
       (⇒ (number? x)
          (number? (f x))))))
  ;; - The output is larger than the input
  (slide
   (code
    (∀ (x) (< x (f x)))))
  ;; - The output list is increasing
  (slide
   (code
    (∀ (l x y)
       (⇒ (<= x y)
          (<= (list-ref (sort l) x)
              (list-ref (sort l) y))))))
  ;; - The output list is a permutation
  (slide
   (code
    (∀ (l) (equal? (sort (sort l)) (sort l)))))
  ;; - The output has increased in size by one
  (slide
   (code
    (∀ (x l) (< (length l) (length (cons x l))))))
  (slide
   (code
    (∀ (x l) (= (length (cons x l)) (+ 1 (length l))))))
  ;; You can express the property and then generate infinite tests
  (tslide "The fundamental problem of properties")
  (tslide "How do you check them?")
  (tslide "Solution 1: Implement ∀")
  (slide
   (code
    (define (∀ property)
      (for ([i (in-range 1000)])
        (property (random))))))
  (slide
   (code
    (define (∀ property)
      (for ([i (in-range 1000)])
        (property (* (expt -1 (random 2))
                     (random)))))))
  (tslide "See QuickCheck")
  (slide
   (code
    (define (∀ property)
      (begin (property #true)
             (property #false)
             (for ([i (in-range 25)])
               (property i))
             ....))))
  (tslide "See Enumerative Testing")
  ;; Problems if the property is an imples (A => B) because A may not hold
  (tslide "Big problem: What if antecdent never occurs?")
  (slide
   (code
    (∀ (a-binary-tree)
       (⇒ (valid-bst? a-binary-tree)
          (f a-binary-tree)))))
  (tslide "See Making Random Judgments, Fetscher, ESOP 2015")
  (tslide "Solution 2: Write a proof")

  (tslide "Formal Verification")
  ;; Attempt to prove fuzz properties using the algebra of programming
  (tslide "Like a proof in algebra class")
  (tslide "But with the \"algebra\" of programming")  
  ;; Hoare Logic is this algebra
  (tslide "Lambda Calculus, Hoare Logic, etc.")
  ;; Or, it can be automated with theorem provers
  (tslide "On paper or automated (Coq, Agda, HOL, etc)")
  (tslide "EXPENSIVE")

  (tslide "Model Checking")
  ;; Express the behavior of the system as an automata
  (tslide "Automata 1 = System")
  ;; Express the desired behavior of the system an automata
  (tslide "Automata 2 = Property")
  ;; Compute the intersection/subset/etc
  (tslide "The behavior of the system is allowed by the property")
  (tslide "L(System) ⊆ L(Property)")
  (tslide "No behavior of the system is disallowed by the property")
  (tslide "L(System) ∩ ¬L(Property) = ∅")
  (tslide "See Alloy, LTL, Maude, etc")
  
  (tslide "fin")
  ;; We've gone through a bunch of different techniques, but this is
  ;; just the tip of the iceberg. You can learn a lot more about any
  ;; of these ideas.

  (slide
   (item "Testing")
   (item "Type Safety")
   (item "Software Contracts")
   (item "Property-based Testing")
   (item "Formal Verification")
   (item "Model Checking"))
  

  )
