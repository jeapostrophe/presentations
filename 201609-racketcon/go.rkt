#lang at-exp racket/base
(require racket/file
         racket/runtime-path)
(define-runtime-path sample "sample")



(define CHARS
  (list->vector
   (string->list
    (string-append "abcdefghijklmnopqrstuvwxyz"
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   "1234567891"
                   "!@#$%^&*()"
                   "~`_-+={}[]|\\:;\"'<,>.?/"
                   "αβΓγΔδεζηΘθιλΛμΞξΠπρΣσςτΦφχΨψΩω"))))
(define (random-vector-ref v)
  (vector-ref v (random (vector-length v))))
(define (ploop! x y)
  (cursor x y)
  (define c (random-vector-ref CHARS))
  (charterm-display c))
(define (scaling-slide)
  (λ (slide-i)
    (charterm-clear-screen)
    (for* ([x (in-range 1 (add1 slide-w))]
           [y (in-range 1 (add1 slide-h))])
      (ploop! x y))
    
    #;(let loop ()
      (define x (add1 (random slide-w)))
      (define y (add1 (random slide-h)))
      (ploop! x y)
      (unless (char-ready?)
        (sleep 1/50)
        (loop)))))







(define pre-slides
  (list
   (scaling-slide)
   
   @slide{@(section! "Introduction")
@mx[4] @my[2]
@dp{
██████╗       ██████╗       ██████╗ ███████╗███╗   ███╗██╗██╗  ██╗██╗
██╔══██╗      ██╔══██╗      ██╔══██╗██╔════╝████╗ ████║██║╚██╗██╔╝██║
██████╔╝█████╗██████╔╝█████╗██████╔╝█████╗  ██╔████╔██║██║ ╚███╔╝ ██║
██╔══██╗╚════╝██╔══██╗╚════╝██╔══██╗██╔══╝  ██║╚██╔╝██║██║ ██╔██╗ ╚═╝
██║  ██║      ██║  ██║      ██║  ██║███████╗██║ ╚═╝ ██║██║██╔╝ ██╗██╗
╚═╝  ╚═╝      ╚═╝  ╚═╝      ╚═╝  ╚═╝╚══════╝╚═╝     ╚═╝╚═╝╚═╝  ╚═╝╚═╝
}
@mx[-4]
@my[2] @mx[36]
@dp{
Jay McCarthy
UMass Lowell}
@mx[-6]
@dp{
 ██▓███   ██▓  ▄▄▄█████▓
▓██░  ██▒▓██▒  ▓  ██▒ ▓▒
▓██░ ██▓▒▒██░  ▒ ▓██░ ▒░
▒██▄█▓▒ ▒▒██░  ░ ▓██▓ ░ 
▒██▒ ░  ░░██████▒▒██▒ ░ 
▒▓▒░ ░  ░░ ▒░▓  ░▒ ░░   
░▒ ░     ░ ░ ▒  ░  ░    
░░         ░ ░   ░      
             ░  ░       
}

@my[-2] @mx[-30]
@dp{
(sixth RacketCon)
       2016/09/17}
}

@slide{@mx[30]@my[10]@bold{@d{#lang remix}} @d{ is a dream}}

@slide!{@mx[5]@my[2]@d{It was ten years ago...}}

@slide!{@mx[45]@my[14]@d{Will it be fulfilled soon?}}

@slide{@my[3]@bold{@d{#lang racket}} @d{ is evolutionary}}
@slide!{@my[5]@mx[5]@d{* units -> modules}}
@slide!{@my[7]@mx[5]@d{* class system}}
@slide!{@my[9]@mx[5]@d{* immutability}}
@slide!{@my[11]@mx[5]@d{* scribble}}

@slide!{@my[14]@d{But what is } @bold{@d{Racket}} @d{?}}

@slide{@my[2]@bold{@d{#lang remix}} @d{ is the } @inverse{@d{most}}
@d{ Racket}}
@slide!{@my[5]@bold{@d{#lang racket}} @d{ is } @bold{@d{#lang r5rs plus good ideas}}}

@slide{@my[5]@mx[35]@bold{@d{*}} @d{ Notation}}
@slide!{@my[7]@mx[35]@bold{@d{*}} @d{ Core Syntax}}
@slide!{@my[9]@mx[35]@bold{@d{*}} @d{ Library Code}}

@slide{@(section! "Notation")
@my[3]@bold{@d{Scheme}} @d{ has very little notation: } @bold{@d{() "" ' ` ,}}

@mx[7]@my[3]@dp{
(define (weird (l '(one two three)))
  (cond
    ((empty? l) "empty")
    ((eq? (first l) 'one) "one")
    (else `(+ 1 ,(first l)))))}
}
@slide{
@my[3]@bold{@d{Racket}} @d{ adds a little: } @bold{@d{[] #:keyword}}

@mx[7]@my[3]@dp{
(define (weird [l '(one two three)] #:mode [mode 'one])
  (cond
    [(empty? l) "empty"]
    [(eq? (first l) mode) "one"]
    [else `(+ 1 ,(first l))]))}
}
@slide!{
@my[3]@mx[35]@bold{@d{{}}}
}

@slide{@mx[7]@my[4]@dp{
 #lang datalog
 edge(a, b). edge(b, c). edge(c, d). edge(d, a).
 path(X, Y) :- edge(X, Y).
 path(X, Y) :- edge(X, Z), path(Z, Y).
 path(X, Y)?
}}
@slide!{
@mx[7]@my[10]@d{----}
@my[2]@dp{
#lang scribble
@"@"title{How to make a slideshow...}

First, you write a slideshow library.
}}

@slide{@mx[10]@my[3]@bold{@d{#lang remix}} @d{ does the following:}}
@slide!{@mx[12]@my[5]@d{- Turn on the @"@"-reader always}}
@slide!{@mx[12]@my[7]@d{- Make [] and {} different from ()}}
@slide!{@mx[12]@my[9]@d{- Introduce a dot notation}}

@slide{@(subsection! "@-reader")
@mx[10]@my[3]@d{@"@"-reader is amazing, as you all know.}}
@slide!{@mx[10]@my[5]@dp[(file->string (build-path sample "datalog.rkt"))]}

@slide{@(subsection! "() and []")
 @mx[10]@my[10]@bold{@d{[}} @d{a b c} @bold{@d{]}} @d{ => } @d{(} @bold{@d{#%brackets}} @d{ a b c)}
 @my[2]@bold{@d{@"{"}} @d{a b c} @bold{@d{@"}"}} @d{ => } @d{(} @bold{@d{#%braces}} @d{   a b c)}
}

@slide{
 @mx[10]@my[5]@dp{
(cond
 [(empty? l) (f)]
 [(<= (first l) x) (g (first l))]
 [else (h (first l))]) 
}}
@slide!{
 @mx[15]@my[10]@bold{@d{=>}}
 @mx[-5]@my[2]@dp{
(cond
 [(empty? l) (f)]
 [else
  (define fl (first l))
  (cond
   [(<= fl x) (g fl)]
   [else (h fl)])])
}}
@slide!{
 @mx[35]@my[15]@bold{@d{=>}}
 @mx[3]@my[-3]@dp{
(cond
 [(empty? l) (f)]
 (define fl (first l))
 [(<= fl x) (g fl)]
 [else (h fl)])
}}
@slide!{
 @mx[40]@my[10]@bold{@d{Works well with ->*, defproc, λ}}
}

@slide{
 @mx[10]@my[5]@bold{@d{[]}} @d{ defaults to } @bold{@d{block}}
 @my[2]@bold{@d{{}}} @d{ defaults to } @bold{@d{infix}}
}

@slide{@(subsection! "Infix")
 @mxy[10 5]@d{Why has infix been done in the past?}
 @mx[2]@my[2]@d{- Math}
 @my[2]@d{- Technical Challenge}
}
@slide!{
 @mxy[10 5]@my[6]@d{Why hasn't worked?}
 @mx[2]@my[2]@d{- Have to } @bold{@d{require}}
 @my[2]@d{- Pain to switch into mode}
}

@slide{
 @mxy[10 5]@dp{
{3 + 4}
{2 * 3 - 48 / 4 - 4 * 5}
{z * 2 + 1}

(def & bitwise-and)
{5 & 1}

(def (→ x y) (+ (* x x) y))
{v7 → v7}

(def (f x y) (+ x y))
{v7 ,f v7}

(def % 2)
{v7 + ,%}
}}

@slide{@(subsection! "Dot")
 @mxy[10 5]@dp{
a.b       => (#%dot a b)
a.b.c     => (#%dot (#%dot a b) c)
a.(b c)   => (#%dot a (b c))
a.(b c).d => (#%dot (#%dot a (b c)) d)
}}
@slide!{
 @mxy[10 5]@my[6]@dp{
(posn-x (rectangle-upper-left (player-bounding-box p)))

     v.s.

p.bb.ul.x
}}

@slide{
@mxy[10 5]@d{What about the old infix notation?}
@my[2]@d{What about rest args?}
@my[2]@d{What about ...?}
@my[2]@d{What about numbers?}
}
@slide!{
@mxy[15 6]@d{=> dropped}
}
@slide!{
@mxy[15 8]@d{=> #%rest}
}
@slide!{
@mxy[15 10]@d{=> … and dotdotdot and ***}
}
@slide!{
@mxy[15 12]@d{=> require #i}
}

@slide{@(section! "Core Syntax")}
@slide!{
@mxy[20 10]@d{Every positions should be expandable.}
@my[2]@d{All macros should provide a way to cooperate.}
}

@slide{@(subsection! "def") @mxy[10 5]@dp{
(def x 5)

(def (f x) (+ x 5))
}}
@slide!{@mxy[10 9]@dp{
(def x
 (def y 1)
 (def z 2)
 (+ y z))
}}
@slide!{@mxy[10 14]@dp{
(def [<def-transformer> . head] . body)
}}

@slide{@(subsection! "def*")
@mxy[10 5]@dp{
[(def x 1)
 (def* y 2)
 (def z x)
 (+ x y z)]
}}

@slide{@(subsection! "require*")
@mxy[10 5]@dp{
(require* m)
body
....

       =>

(require m (rename-in m [#%require*-begin m:#%require*-begin]))
(m:#%require*-begin body ....)
}}

@slide{@(subsection! "#%dot")
@mxy[10 5]@dp{
(#%dot (#%dot a b) c) => (#%dot a b c)

p.x => (#%dot p x) => (posn-x p)
 if (def [posn p] ....)
}}

@slide{@(subsection! "λ")
@mxy[10 5]@dp{
(def (f x) x)
   =>
(def f (lambda (x) x))
}}
@slide!{
@mxy[10 9]@dp{
(remix-lambda (arg ...) body)
   =>
(racket-lambda (temp-arg ...)
 (def arg temp-arg) ...
 body)
}}
@slide!{
@mxy[10 15]@dp{
(λ ([posn p]) p.x)
   =>
(λ (tmp-p) (def [posn p] tmp-p) p.x)
   =>
(λ (tmp-p) (posn-x p))
}}

@slide{@(subsection! "static-interface")
@mxy[10 5]@dp{
(def [static-interface posn]
 (def [member x] posn-x)
 (def [member y] posn-y))
}}

@slide{@(subsection! "def/dots-transformers")}

@slide{@(subsection! "def+")
@mxy[10 5]@dp{
length empty = 0
length (cons x xs) = 1 + length xs
}}
@slide{@mxy[10 5]@dp{
(def+ (length '())
  0)
(def+ (length (cons x xs))
  {1 + (length xs)})
}}

@slide{@mxy[10 5]@dp{
(def+ (length '())
  0)
(def+ (length (cons x xs))
  {1 + (length xs)})
(def+ [contract length] (-> list? nat?))
(def+ [doc (length l)]
  @"@{"A most excellent function
    for discovering the length of lists.@"}")
(def+ [examples length]
 {(length '()) ≡ 0}
 {(length '(a b c)) ≡ 3})
(def+ [provide length])
}}

@slide{@mxy[10 5]@dp{
(def+ length
 [contract (-> list? nat?)]
 [case '() 
       0]
 [case (cons x xs)
       {1 + (length xs)}]
 [doc (l)
      @"@{"A most excellent function...@"}"]
 [examples
  {(length '()) ≡ 0}
  {(length '(a b c)) ≡ 3}]
 [provided])
}}
@slide!{@mxy[10 5]@mx[30]@bold{@d{Dependency tracking}}
@my[1]@dp{
def+-transformer : {
 deps   : (-> key x (listof keys))
 expand : (stx key->vals -> stx x val)
}
}}

@slide{
@mxy[20 10]@d{Every positions should be expandable.}
@my[2]@d{All macros should provide a way to cooperate.}
}
@slide!{@mxy[20 15]@bold{@d{cond, testing, structures, etc}}}

@slide{@(section! "Core Library")
@mxy[10 5]@d{Present wishlist:}}
@slide!{@mxy[12 7]@d{- generic interfaces for most things}}
@slide!{@mxy[12 9]@d{- specific interfaces (with def transformers)}}
@slide!{@mxy[12 11]@d{- normalization of naming scheme and argument patterns}}
@slide!{@mxy[12 13]@d{- dropping historical names and keyword-less many-argument functions}}

@slide{@(section! "(require remix)")
@mxy[15 5]@d{Why not...?}
@mxy[5 5]@dp{
#lang remix-stx racket/base
(require remix)
}}
@slide!{@mxy[15 15]@d{A new standard}}

@slide{@(section! "#lang remix")
@mxy[20 5]@dp{
Please help! I'm stuck in Limbo!
}}

@slide{@(section! "#lang racket3")
@mxy[20 5]@d{What about Racket 3?}}
@slide!{@mxy[20  7]@d{- Data representation control}}
@slide!{@mxy[20  9]@d{- No top-level expressions}}
@slide!{@mxy[20 11]@d{- Have VM make fewer promises}}
@slide!{@mxy[20 13]@d{- Unify match/syntax-parse and syntax templates}}
@slide!{@mxy[20 15]@d{- Merge modules and units}}

))

;;;;;;;;;;;;;;;;;;;

(require "charterm.rkt"
         racket/port
         racket/list
         racket/pretty
         racket/string)

(define slides
  (list->vector
   (filter procedure?
           (flatten pre-slides))))
(provide slides)

(define slide-h 24)
(define slide-w 80)
(define section "")
(define subsection "")
(define (section! x)
  (set! section x)
  (set! subsection ""))
(define (subsection! x)
  (set! subsection x))

(define-syntax-rule (slide* e ...)
  (λ (slide-i #:redux? [redux? #f])
    (cursor 1 1)
    e ...

    @(unless redux?
       @bottom{@inverse{@d[" "]@d{@section}@d[" "]@unless[(equal? subsection "")]{@d["> "]@d{@subsection}@d[" "]}@right{@(add1 slide-i) / @(vector-length slides)}}})
    @cursor[(- slide-w 3) slide-h]))

(define-syntax-rule (slide! e ...)
  (λ (slide-i #:redux? [redux? #f])
    ((vector-ref slides (sub1 slide-i))
     (sub1 slide-i)
     #:redux? #t)
    ((slide* e ...) slide-i #:redux? redux?)))
(define-syntax-rule (slide e ...)
  (slide* (clear!) e ...))

(define cur-x 1)
(define cur-y 1)
(define (clear!)
  (set! cur-x 1) (set! cur-y 1)
  (charterm-clear-screen))
(define (cursor x y)
  #;(define y (+ iy 5))
  (set! cur-x x) (set! cur-y y)
  (charterm-cursor x y))
(define (mx dx)
  (cursor (+ dx cur-x) cur-y))
(define (my dy)
  (cursor cur-x (+ dy cur-y)))
(define (mxy dx dy) (mx dx) (my dy))

(define-syntax-rule (t e ...)
  (with-output-to-string (λ () (display e) ...)))
(define-syntax-rule (d e ...)
  (charterm-display (t e ...)))
(define-syntax-rule (dp e ...)
  (for ([x (in-list (string-split (t e ...) #rx"\n"))])
    (charterm-display x)
    (my 1)))

(define-syntax-rule (define-style inverse charterm-inverse)
  (define-syntax-rule (inverse . e)
    (begin (charterm-inverse)
           (begin . e)
           (charterm-normal))))

(define-style inverse charterm-inverse)
(define-style bold charterm-bold)

(define-syntax-rule (bottom e ...)
  (begin (cursor 1 slide-h) e ...))

(define-syntax-rule (right arg ...)
  (let ([o (t arg ...)])
    (cursor (- slide-w (string-length o) -1) cur-y)
    (charterm-display o)))

(define-syntax-rule (pp x)
  (dp (pretty-format 'x #:mode 'display)))

;;;;

(require (prefix-in ss: slideshow)
         (prefix-in pict: pict))

(define SCREEN #f)
(define SCREEN-X 1)
(define SCREEN-Y 1)
(define *default 'modern)
(define SCREEN-STYLE *default)
(define (->string s)
  (with-output-to-string (λ () (display s))))
(define (charterm-display s)
  (for ([c (in-string (->string s))])
    (set! SCREEN-X (add1 SCREEN-X))
    (vector-set! (vector-ref SCREEN SCREEN-Y) SCREEN-X
                 (pict:text (string c) SCREEN-STYLE))))
(define (charterm-clear-screen)
  (set! SCREEN-X 1)
  (set! SCREEN-Y 1)
  (set! SCREEN
        (build-vector (add1 slide-h)
                      (λ (i)
                        (make-vector (+ 2 slide-w) (pict:text " " 'modern))))))
(define (charterm-cursor x y)
  (set! SCREEN-X x)
  (set! SCREEN-Y y))
(define (charterm-inverse)
  ;; XXX
  (set! SCREEN-STYLE *default))
(define (charterm-normal)
  (set! SCREEN-STYLE *default))
(define (charterm-bold)
  ;; XXX
  (set! SCREEN-STYLE *default))

(define (screen->pict scr)
  (for/fold ([p (pict:blank)])
            ([ROW (in-vector SCREEN)])
    (pict:vc-append
     p
     (for/fold ([p (pict:blank)])
               ([col (in-vector ROW)])
       (pict:hc-append p col)))))

(for ([s (in-vector slides)]
      [i (in-naturals)])
  (s i)
  (ss:slide
   (pict:scale-to-fit (screen->pict SCREEN)
                      ss:client-w
                      ss:client-h)))
