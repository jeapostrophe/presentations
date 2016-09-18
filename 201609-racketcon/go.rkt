#lang at-exp racket/base

(define slides
  (list
   @slide{
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

   ))

(provide slides)

;;;;;;;;;;;;;;;;;;;

(require "charterm.rkt"
         racket/port
         racket/string)

(define slide-h 24)
(define slide-w 80)

(define-syntax-rule (slide! e ...)
  (λ (#:slide slide-i
      #:slides slides)
    (cursor 1 1)
    e ...

    @bottom{@inverse{@right{@(add1 slide-i) / @slides}}}
    @cursor[slide-w slide-h]))

(define-syntax-rule (slide e ...)
  (slide! (clear!) e ...))

(define cur-x 1)
(define cur-y 1)
(define (clear!)
  (set! cur-x 1) (set! cur-y 1)
  (charterm-clear-screen))
(define (cursor x y)
  (set! cur-x x) (set! cur-y y)
  (charterm-cursor x y))
(define (mx dx)
  (cursor (+ dx cur-x) cur-y))
(define (my dy)
  (cursor cur-x (+ dy cur-y)))

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
