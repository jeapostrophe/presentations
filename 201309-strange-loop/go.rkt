#lang slideshow
(require slideshow/code
         slideshow/step
         racket/runtime-path
         racket/format
         racket/gui
         (for-syntax syntax/parse)
         "lib.rkt")

                                        ; Setup
(icon-assembler #:logo-scale 0.65)
(code-scripts-enabled #t)
(define pnf (send the-font-list find-or-create-font 36 'default 'normal 'normal))
(current-page-number-font pnf)
(set-page-numbers-visible! #t)

(define (sub-para . l)
  (para #:width (* 3/4 (current-para-width)) l))

(define square-key
  (rectangle 20 20))
(define diamond-key
  (circle 20))
(define safe-key
  diamond-key)
(define unsafe-key
  (cloud 20 20))
(define ...-cloud
  (cloud 200 30))

(current-keyword-list
 (append (list "exit" "with-prompt" "let/cp" "let/cc"
               "int" "return" "while" "abort"
               "match-define" "match"
               "call-with-composable-continuation"
               "abort-current-continuation"
               "define-syscall-throw"
               "make-continuation-prompt-tag"
               "call-with-continuation-prompt")
         (current-keyword-list)))

(define current-program-name (make-parameter #f))
(define-syntax (---- stx) (raise-syntax-error '---- "Used outside program" stx))
(define-syntax (program stx)
  (syntax-parse
      stx
    #:literals (----)
    [(_ name:str prog-code:expr ... ---- hidden:expr ...)
     (syntax/loc stx
       (parameterize ([current-logo-clickback
                       (λ ()
                         (parameterize ([current-program-name name])
                           (let ()
                             prog-code ...
                             hidden ...)))])
         (slide #:title name
                (code prog-code ...))))]))

(define-syntax-rule (graph graph-eps-pth-str)
  (begin (define-runtime-path graph-eps-pth graph-eps-pth-str)
         (define graph-pth (path-replace-suffix graph-eps-pth #".png"))
         (printf "Loading ~a\n" graph-pth)
         (slide (bitmap graph-pth))))

(define-syntax-rule (tabular (col ...) ...)
  (table (length (first '((col ...) ...)))
         (list (t col) ... ...)
         vc-append
         hbl-append
         10 5))

(define-syntax-rule (compare title (version num) ...)
  (slide #:title title
         (table 2 (append (list (t version) (t num)) ...)
                (cons vc-append vr-append) hbl-append
                10 5)))

(define cframe frame) ;; xxx provide an inset
(define (cblackout p)
  (define fp (frame p))
  (filled-rectangle (pict-width fp) (pict-height fp)))
(define (result v)
  (tt (format "Result: ~a" v)))
(define (cslide . a)
  (keyword-apply slide '(#:layout) '(left) a))

;; XXX kind of ugly and would be nice to have BYU or Mormon logo
(title '("Continuations")
       '("On the Web and in your OS")
       '(("Jay McCarthy" "PLT @ Brigham Young University")))

;; Hello everyone, it's great to be here at Strange Loop. It's a bit
;; strange for me to not be presenting a research paper and not be
;; testing you afterwards, but nonetheless I hope that you will learn
;; a lot and enjoy it.

;; I'll be talking about continuations. I'll assume that you don't
;; know what they are, so I'll introduce them and then walk you
;; through some of classic ways they are cool through some more exotic
;; uses.

(define outline
  (make-outline
   'basics
   "What are Continuations?"
   #f

   'on-the-web
   "How are they used on the Web?"
   #f

   '(advanced networking os)
   "How many fit on the end of pin?"
   (match-lambda
    [   'advanced (sub-para "Web Components")]
    [ 'networking (sub-para "Event-based Network Servers")]
    [         'os (sub-para "Operating Systems")])))

(outline 'basics)

(slide
 #:layout 'center
 #:title "What is a continuation?"
 ;; The first basic question is, what is a continuation?
 'next
 ;; The classic explanation is that they are an abstraction of the
 ;; "rest" of the computation, meaning what will happen after the
 ;; "current" program is "over"
 (t "The 'rest' of the computation"))

;; To be more concrete, let's look at a specific program.

;; Here we have a basic arithmetic computation involving addition,
;; multiplication, and subtraction.

(cslide
 (code
  (+ 4
     (* 5
        (- 6 7)
        8)
     9))
 (result
  -27))

;; If we focus on the subtraction, we can ask the question, "What is
;; the continuation of this piece of the program?"

(cslide
 (code
  (+ 4
     (* 5
        #,(cframe
           (code
            (- 6 7)))
        8)
     9))
 (result
  -27))

;; The answer is everything "outside" of the box.

(cslide
 (cframe
  (code
   (+ 4
      (* 5
         #,(cblackout
            (code
             (- 6 7)))
         8)
      9)))
 (result
  -27))

;; Although I am using a beautiful language peppered with bananas, the
;; concept of continuation existing in all languages. For instance, in
;; this C-ish program...

(cslide
 (code
  #,(cframe (code int z = 6 - 7 ))
  int y = 5 * z * 8
  return 4 + y + 9)
 (result
  -27))

;; The 'rest' of the program is bit more obvious in this case because
;; the program is written linearly.

(cslide
 (cframe
  (code
   #,(cblackout (code int z = 6 - 7 ))
   int y = 5 * z * 8
   return 4 + y + 9))
 (result
  -27))

;; But my eyes are hurting a bit, so I'm going to go back to bananas.

(cslide
 (cframe
  (code
   (+ 4
      (* 5
         #,(cblackout
            (code
             (- 6 7)))
         8)
      9)))
 (result
  -27))

;; Once we understand what continuations are, we can think about
;; representing them inside of the language, rather than merely
;; holding them in our brains. For instance, the following code

(cslide
 (code
  (λ (z)
    (+ 4
       (* 5
          z
          8)
       9)))
 (result
  "#<procedure>"))

;; is a function that is, in some sense, equivalent to the above
;; continuation.

;; In a language like Racket, this representation of the continuation
;; is always available through a form called let/cc:

(cslide
 (code
  (+ 4
     (* 5
        (let/cc k
          (- 6 7))
        8)
     9))
 (result
  -27))

;; This program just 'names' the continuation, which is like the
;; function shown before. By tradition (from the CESK machine for
;; instance) we name continuations, k.

;; However, we could use the continuation is we want. If we call it,

(cslide
 (code
  (+ 4
     (* 5
        (let/cc k
          (k (- 6 7)))
        8)
     9))
 'alts
 (list
  ;; But what does this program return?
  (list
   (result "?"))
  ;; Obviously it should be this, right?
  (list
   (result
    -1067))
  ;; Actually, it is just -27, but why?
  (list
   (result
    -27))))

;; It turns out I lied earlier about the functional representation of
;; the continuation, I said it was

(cslide
 (code
  (λ (z)
    (+ 4
       (* 5
          z
          8)
       9)))
 (result
  "#<procedure>"))

;; But actually it is more like

(cslide
 (code
  (λ (z)
    (exit
     (+ 4
        (* 5
           z
           8)
        9))))
 (result
  "#<procedure>"))

;; So when we call the continuation

(cslide
 (code
  (+ 4
     (* 5
        (let/cc k
          (k (- 6 7)))
        8)
     9))
 (result
  -27))

;; It is not the same as

(cslide
 (code
  (+ 4
     (* 5
        (+ 4
           (* 5
              (- 6 7)
              8)
           9)
        8)
     9))
 (result
  -1067))

;; But is actually the same as

(cslide
 (code
  (+ 4
     (* 5
        (exit
         (+ 4
            (* 5
               (- 6 7)
               8)
            9))
        8)
     9))
 (result
  -27))

;; This is because the continuations I've discussed are undelimited,
;; meaning they contain /everything/ in the future of the computation

(slide
 #:layout 'center
 (bt "UNdelimited"))

;; In Racket, we also have DELIMITED continuations that you can
;; specify where the continuation "goes to" with something called a
;; PROMPT. For instance,

(define-syntax-rule (with-prompt . body)
  (call-with-continuation-prompt
   (λ () . body)))
(define-syntax-rule (let/cp k . body)
  (call-with-composable-continuation
   (λ (k) . body)))

(cslide
 (code
  (exit
   (with-prompt
    (+ 4
       (* 5
          (let/cp k
                  (k (- 6 7)))
          8)
       9))))
 (result
  -1067))

;; This allows some strange behavior, such as

(cslide
 (code
  (exit
   (with-prompt
    (+ 4
       (* 5
          (let/cp k
                  (* 2 (k (- 6 7))))
          8)
       9))))
 (result
  -2147))

(cslide
 (code
  (exit
   (with-prompt
    (+ 4
       (* 5
          (let/cp k
                  (k (* 2 (k (- 6 7)))))
          8)
       9))))
 (result
  -85867))

(cslide
 (code
  (exit
   (with-prompt
    (+ 4
       (* 5
          (let/cp k
                  (if (positive? (k (- 6 7)))
                    -1
                    0))
          8)
       9))))
 (result
  13))

(cslide
 (code
  (exit
   (* -1
      (with-prompt
       (+ 4
          (* 5
             (let/cp k
                     (if (positive? (k (- 6 7)))
                       -1
                       0))
             8)
          9)))))
 (result
  -13))

;; Programs like this are why people talk about continuations as akin
;; to "time travel" where a program goes to its own future, observes
;; the result and then does something different.

;; This concludes the basics of continuations

(slide
 #:layout 'center
 #:title "Continuations"
 (item "The future of the computation")
 (item "Can be represented as a function")
 'next
 (item "Undelimited (the entire future)")
 (item "Delimited (the future up to a point)")
 'next
 (item "Languages like Racket give you all of this"))

(outline 'on-the-web)

;; Continuations have been found to be useful on the Web. Take for
;; instance a simple console program:

(cslide
 (code
  (printf
   "Sum is: ~a"
   (+ (prompt "Enter first number:")
      (prompt "Enter second number:"))))
 ;; We when run this program, it asks us for the first number, which
 ;; we type in
 'alts
 (list
  (list (t "Enter first number:"))
  (list (t "Enter first number: 2"))
  (list (t "Enter first number: 20")))
 ;; Then it asks us for the next
 'alts
 (list
  (list (t "Enter second number:"))
  (list (t "Enter second number: 2"))
  (list (t "Enter second number: 22")))
 ;; And finally prints the sum
 (t "Sum is: 42"))

;; If we convert this program to the Web, it doesn't really work,
;; because we can't ask a question and wait for the answer.

;; xxx add a demo button
(cslide
 (code
  (web-printf
   "Sum is: ~a"
   (+ (web-prompt "Enter first number:")
      (web-prompt "Enter second number:")))))

;; This is why on the Web, you break interactions into multiple steps
;; and associate each with a different URL.

;; xxx add a demo button
(cslide
 (code
  (web-dispatch
   ["/"
    (web-prompt&go-to
     "Enter first number:"
     "/sum/")]
   ["/sum/$first"
    (web-prompt&go-to
     "Enter second number:"
     "/sum/$first/")]
   ["/sum/$first/$second"
    (web-printf
     "Sum is: ~a"
     (+ first second))])))

;; This process is tedious and brittle. We might make mistakes while
;; serializing and deserialization the information that gets passed
;; along (first and second). Furthermore, we hard-code the
;; control-flow of the program into the URL structure, where we can
;; make mistakes and are locked in a bit.

;; Continuations allow us to do this automatically, if we recognize
;; that each URL represents a different server continuations.

(cslide
 (code
  (define (continuation1 answer1)
    (define (continuation2 answer2)
      (web-printf
       "Sum is: ~a"
       (+ answer1
          answer2)))
    (web-prompt&go-to
     "Enter second number:"
     continuation2))
  (web-prompt&go-to
   "Enter first number:"
   continuation1)))

;; Of course, in a language like Racket, these continuations can be
;; made automatically

(cslide
 (code
  (define (web-prompt display-text)
    (let/cc server-continuation
      (define new-url
        (store-in-dispatch-table!
         server-continuation))
      (send-to-client
       (web-prompt&go-to
        display-text
        new-url))))))

;; With this definition of 'web-prompt', whenever a request of the
;; user is made, the continuation is captured and stored in the
;; dispatch table at a fresh URL, which you use in the normal Web
;; response producer

;; xxx add a demo button

(slide
 #:title "Plus a lot of details..."
 (para "Refer to OOPSLA 2010, ICFP 2009, and HOSC 2007 papers for most of them."))

;; But let's look at just two details...

;; This is basically the implementation of the Racket Web server:

(cslide
 (code
  (define (web-server some-servlet)
    (while true
      (define conn (wait-for-connection))
      (define req (get-a-request conn))
      (define resp (some-servlet req))
      (display-on-wire conn resp)))))

;; We just need another loop for HTTP/1.1

(cslide
 (code
  (define (web-server some-servlet)
    (while true
      (define conn (wait-for-connection))
      (while (connected? conn)
        (define req (get-a-request conn))
        (define resp (some-servlet req))
        (display-on-wire conn resp))))))

;; However, there's one big problem... if you capture the continuation
;; inside of the servlet, then it will get the connection from when
;; the first request was made and not when the new request was
;; made. So, it will try to return to the wrong place!

(cslide
 (code
  (define (web-prompt display-text)
    (let/cc #,(cframe (code server-continuation))
      (define new-url
        (store-in-dispatch-table!
         server-continuation))
      (send-to-client
       (web-prompt&go-to
        display-text
        new-url))))))

(cslide
 (code
  (define (web-server some-servlet)
    ....
    (define conn1 (wait-for-connection))
    (define req1 (get-a-request conn1))
    (define resp1 (some-servlet req1))
    (display-on-wire conn1 resp1)
    ....
    (define conn2 (wait-for-connection))
    (define req2 (get-a-request conn2))
    (define resp2 (some-servlet req2))
    (display-on-wire #,(cframe (code conn1)) resp2)
    ....)))

;; We can solve this by a small change to the server...

(cslide
 (code
  (define (web-server some-servlet)
    (while true
      (define conn (wait-for-connection))
      (while (connected? conn)
        (define req (get-a-request conn))
        (define resp
          (#,(cframe (code with-prompt))
           (some-servlet req)))
        (display-on-wire conn resp))))))

;; By wrapping all of the server's interaction with the servlet in a
;; prompt, the servlet will only capture continuations back to where
;; the server starts.

;; This brings us to the last feature of delimited continuations worth
;; talking about... aborts

;; If we look at web-prompt,

(cslide
 (code
  (define (web-prompt display-text)
    (let/cc server-continuation
      (define new-url
        (store-in-dispatch-table!
         server-continuation))
      (send-to-client
       (web-prompt&go-to
        display-text
        new-url))))))

;; It has a call to send-to-client

(cslide
 (code
  (define (web-prompt display-text)
    (let/cc server-continuation
      (define new-url
        (store-in-dispatch-table!
         server-continuation))
      (#,(cframe (code send-to-client))
       (web-prompt&go-to
        display-text
        new-url))))))

;; Which is really an abort

(cslide
 (code
  (define (web-prompt display-text)
    (let/cc server-continuation
      (define new-url
        (store-in-dispatch-table!
         server-continuation))
      (#,(cframe (code abort))
       (web-prompt&go-to
        display-text
        new-url))))))

;; An abort is like an exception throw (actually, an exception throw
;; is like an abort) but it goes back to the nearest continuation
;; prompt rather than to the nearest exception handler.

;; This is basically the real implementation of the RWS... except that
;; in Racket prompts are first-class so that uses of prompts for
;; different purposes (e.g. the Web and exceptions and logic
;; programming) don't interfere with each other.

(slide
 #:title "Plus some tiny details..."
 (para "Prompts are first-class"))

;; So, that's the "classic" story of continuations on the Web.

;; In the last part of the talk, we'll look at some more advanced
;; uses.

(outline 'advanced)

;; Continuations are not only useful in the infrastructure of the Web
;; application, they can be valuable inside of the application
;; itself. For instance, suppose we want to have a piece of
;; interaction inside of another interaction. A simple exampe would be
;; a counter that is embedded in a larger page.

(cslide
 (code
  (define (page header)
    (send/suspend/dispatch
     (λ (embed/url)
       `(p
         (h1 ,header)
         (a ([href
              ,(embed/url
                (λ (req) (page "First")))])
            "First")
         ,(include-counter embed/url)
         (a ([href
              ,(embed/url
                (λ (req) (page "Second")))])
            "Second")))))))

;; Here we have a page with two interactions, First and Second, which
;; change the header and they embed a counter.

(cslide
 ;; The counter includes a numeric counter with buttons to increase
 ;; and decrease it

 ;; The beautiful thing about this is that the counter captures the
 ;; continuation of the page's rendering, so that when you click on
 ;; links, it returns a different rendering of the counter component
 ;; back to the page's renderer so it shows the new inside

 ;; It's like a server-sided iframe.
 (code
  (define (include-counter embed/url)
    (let/cc k
      (let loop ([cnt 0])
        (k
         `(p
           (a ([href
                ,(embed/url
                  (λ (req) (loop (sub1 cnt))))])
              "-")
           ,(number->string cnt)
           (a ([href
                ,(embed/url
                  (λ (req) (loop (add1 cnt))))])
              "+"))))))))

;; xxx add a demo button

;; Next, let's look at network servers...

(outline 'networking)

;; In a traditional, blocking network server, you might write code
;; that reads a few network requests, does a computation, and then
;; returns the answer.

(cslide
 (code
  (define req1 (read-request fd))
  (define req2 (read-request fd))
  (define res1 (compute-answer req1 req2))
  (send-answer fd res1)
  (free-resources req1 req2 res1)))

;; This structure is "bad" because `read-request' will presumably
;; block while the network packets arrive, which might block the rest
;; of your server. One solution is to use threads, which might have
;; their own problems, but the more traditional approach is to write
;; in a non-blocking way.

;; This assumes that we have an event loop that has many event
;; handlers that are waiting for their events to finish, at which
;; point we may generate new events. Each stage of the request has to
;; create a new event that will handle the next stage.

(cslide
 (code
  (define (evloop evts)
    (sync
     (for/list ([e (in-list evts)])
       (e
        (λ (new-evts)
          (append
           new-evts
           (remove e evts)))))))))

(cslide
 (code
  (evloop
   (list
    (λ (next)
      (handle-evt
       (read-request-evt fd)
       (λ (req1)
         ....)))))))

(cslide
 (code
  (next
   (list
    (λ (next)
      (handle-evt
       (read-request-evt fd)
       (λ (req2)
         ....)))))))

(cslide
 (code
  (define res1 (compute-answer req1 req2))
  (next
   (list
    (λ (next)
      (handle-evt
       (send-answer-evt fd res1)
       (λ ()
         (free-resources req1 req2 res1)
         (next empty))))))))

;; This gross code is really just representing the continuation
;; explicitly as a function that handles the next part of the
;; work. This is almost exactly the same as the traditional Web use
;; and is, in fact, just like threads. We could just as easily provide
;; a new implementation of read-request:

(cslide
 (code
  (define (read-request fd)
    (let/cc k
      (switch-to-evloop-until
       (read-request-evt fd)
       k)))))

;; In fact, this is what cooperative threads really are in the first
;; place. In languages like Racket, they turn all potentially blocking
;; calls into continuation captures that associate the control state
;; with a pending event and put it back on the queue when it's time.

;; Taking this analogy more directly, let's look at threading systems
;; using continuations.

(outline 'os)

;; A thread system consists of a queue of threads, a way to create new
;; ones, to end old ones, and to swap the running context.

(cslide
 (code
  (define threads empty)
  (define (spawn body)
    (set! threads (snoc threads body)))
  (define (switch)
    (unless (empty? threads)
      (define next (first threads))
      (set! threads (rest threads))
      (next)))
  (define (yield)
    (let/cc k
      (spawn k)
      (switch)))))

;; This trivial threading system really works, for instance the
;; following program fairly interleaves two loops:

(cslide
 (code
  (define (looper)
    (for ([i (in-range 5)])
      (displayln i)
      (yield))
    (switch))

  (spawn looper)
  (looper)))

;; However, a thread system is more than this though. It has the
;; "system calls" that control volatile resources. Let's look at a
;; little example...

;; We'll be looking at about 85 lines of code... I hope it is not too
;; overwhelming.

;; The kernel stores the threads and a "safe", which is a value that
;; starts as 0.

(cslide
 (code
  (struct kernel (threads safe))))

;; Programs can be started from 'main' that use this. For example,
;; this program has two threads that go through some loops swapping
;; into the safe and observing what used to be in the safe.

(cslide
 (code
  (define (main)
    (define N 5)
    (thread
     (λ ()
       (for ([i (in-range (+ N 2))])
         (printf
          "iter: ~a -> ~a\n"
          i
          (swap (* 2 (add1 i)))))))
    (thread
     (λ ()
       (for/fold ([sum 0])
           ([i (in-range N)])
         (printf
          "adder: ~a -> ~a\n"
          i
          (swap (+ i sum)))
         (+ i sum)))))))

;; The core of the OS is very simple... it initializes the system to
;; the main program and 0 and continually runs a thread until there
;; are no more.

(cslide
 (code
  (define (boot main)
    (define initial (kernel (list main) 0))
    (let loop ([ks initial])
      (unless (empty? (kernel-threads ks))
        (loop (step-one-thread ks)))))))

;; To run a single thread, you pick the one at the top, like before,
;; run it until it gets to a system call, and then execute that system
;; call's effect on the kernel state

(cslide
 (code
  (define (step-one-thread ks)
    (match-define
     (kernel (cons top-thread other-threads)
             safe)
     ks)
    (define syscall
      (run-thread-until-syscall top-thread))
    (execute-syscall
     syscall
     (kernel other-threads safe)))))

;; The various system calls are creating new threads, exiting, and the
;; swap.

(cslide
 (code
  (struct syscall (user-context)))
 (code
  (struct syscall:thread syscall (child-thunk))
  (struct syscall:end syscall ())
  (struct syscall:swap syscall (new-safe))))

;; When you execute these, you return a new kernel state. Thread and
;; exit just affect the thread list, while swap installs a new safe
;; value and returns a version of the thread's context that gives the
;; old safe to the user code

(cslide
 (code
  (define (execute-syscall call kernel-state)
    (match-define
     (kernel threads safe)
     kernel-state)
    (match call
      [(syscall:thread user-ctxt child-t)
       (kernel (list* user-ctxt child-t threads)
               safe)]
      [(syscall:end user-ctxt)
       (kernel threads
               safe)]
      [(syscall:swap user-ctxt new-safe)
       (kernel (snoc threads
                     (λ () (user-ctxt safe)))
               new-safe)]))))

;; Each of these system call datastructures are like the way a real
;; system call works: the type tag of the structure is like the system
;; call number, the user context is like the
;; continuation/stack/registers/etc, and the additional information
;; are the arguments.

;; These are prepared by code like the following: after capturing the
;; continuation, they create the data structure and abort it back to
;; the kernel's prompt

(cslide
 (code
  (define (thread child-t)
    (call-with-composable-continuation
     (λ (user-ctxt)
       (abort-current-continuation
        kernel-prompt-tag
        (syscall:thread user-ctxt child-t)))
     kernel-prompt-tag))))

;; But of course, we need a lot of these, so we make a macro to define
;; them all:

(cslide
 (code
  (define-syntax-rule
    (define-syscall-throw user-id syscall-id)
    (define (user-id . syscall-args)
      (call-with-composable-continuation
       (λ (user-ctxt)
         (abort-current-continuation
          kernel-prompt-tag
          (apply syscall-id user-ctxt syscall-args)))
       kernel-prompt-tag)))

  (define-syscall-throw thread syscall:thread)
  (define-syscall-throw end syscall:end)
  (define-syscall-throw swap syscall:swap)))

;; Finally, after creating these "throwers", we can create the
;; "catchers"

(cslide
 (code
  (define kernel-prompt-tag
    (make-continuation-prompt-tag 'kernel))
  (define (run-thread-until-syscall thread-ctxt)
    (call-with-continuation-prompt
     (λ ()
       (thread-ctxt)
       (end))
     kernel-prompt-tag
     values))))

;; Thus, we have in about 50 lines a custom operating system with a
;; unique system call. This pattern can be turned into a library where
;; you just customize what scheduling policy you want and what system
;; calls there are.

(cslide
 (parameterize
     ([current-font-size 12])
   (code
    (struct kernel (threads safe))

    (define (boot main)
      (define initial (kernel (list main) 0))
      (let loop ([ks initial])
        (unless (empty? (kernel-threads ks))
          (loop (step-one-thread ks)))))

    (define (step-one-thread ks)
      (match-define (kernel (cons top-thread other-threads) safe) ks)
      (define syscall (run-thread-until-syscall top-thread))
      (execute-syscall syscall (kernel other-threads safe)))

    (struct syscall (user-context))

    (struct syscall:thread syscall (child-thunk))
    (struct syscall:end syscall ())
    (struct syscall:swap syscall (new-safe))

    (define (execute-syscall call kernel-state)
      (match-define (kernel threads safe) kernel-state)
      (match call
        [(syscall:thread user-ctxt child-t)
         (kernel (list* user-ctxt child-t threads) safe)]
        [(syscall:end user-ctxt)
         (kernel threads safe)]
        [(syscall:swap user-ctxt new-safe)
         (kernel (snoc threads (λ () (user-ctxt safe))) new-safe)]))

    (define-syntax-rule
      (define-syscall-throw user-id syscall-id)
      (define (user-id . syscall-args)
        (call-with-composable-continuation
         (λ (user-ctxt)
           (abort-current-continuation
            kernel-prompt-tag
            (apply syscall-id user-ctxt syscall-args)))
         kernel-prompt-tag)))

    (define-syscall-throw thread syscall:thread)
    (define-syscall-throw end syscall:end)
    (define-syscall-throw swap syscall:swap)

    (define kernel-prompt-tag
      (make-continuation-prompt-tag 'kernel))
    (define (run-thread-until-syscall thread-ctxt)
      (call-with-continuation-prompt
       (λ ()
         (thread-ctxt)
         (end))
       kernel-prompt-tag
       values)))))

;; I have found it to be a very interesting way to structure
;; concurrent systems with unique shared data

;; This concludes the presentation on continuations and exotic uses
;; thereof. Thank you!

(slide
 (para "Jay McCarthy")
 (hc-append
  (vc-append (bitmap "PLTnolarval-small.jpg")
             (t "PLT"))
  (t " @ ")
  (vc-append (bitmap "mormon-small.png")
             (t "Brigham Young University")))
 (para "Twitter: " (tt "@jeapostrophe"))
 (para "Github: " (tt "jeapostrophe"))
 (para "Blog: " (tt "http://jeapostrophe.github.io")))
