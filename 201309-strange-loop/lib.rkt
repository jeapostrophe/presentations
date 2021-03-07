#lang slideshow
(require racket/gui
         racket/runtime-path)
(provide title
         icon-assembler
         current-logo-clickback)

(define-runtime-path plt-background-path "plt-back.1024x768.png")

(define purple-color (make-object color% 150 0 150))
(define title-text-size 60)
(define large-text-size 48)
(define med-text-size 40)
(define normal-text-size 36)
(define small-text-size 24)
(define section-point-size 48)
(define big-bullet-size normal-text-size)
(define big-bullet-color "darkblue") 
(define small-bullet-size 30)
(define small-bullet-color purple-color)
(define other-small-bullet-color "blue")
(define bullet-inset 50)

(define section-title-color "purple")
(define institution-color "blue")
(define author-color "darkred")
(define title-color section-title-color)

(define (title title-strs subtitle-strs authors/institutions)
  (parameterize ([current-slide-assembler
                  (lambda (title sep content)
                    (inset 
                     content
                     (- margin)
                     (- margin)
                     0
                     0))])
    (slide
     (cc-superimpose
      (bitmap plt-background-path)
      (vr-append
       (vl-append
        (apply vl-append (map (lambda (x) (text x `roman title-text-size))
                              title-strs))
        (apply vl-append (map (lambda (x) (text x `(italic . roman) large-text-size))
                              subtitle-strs)))
       (blank 0 50)
       
       (apply vr-append 
              (map (lambda (x)
                     (vr-append (colorize (text (car x) 'decorative med-text-size) author-color)
                                (colorize (text (cadr x) 'decorative normal-text-size) institution-color)
                                (blank 0 10)))
                   authors/institutions)))))))

(define-runtime-path logo-path1 "PLTnolarval-small.jpg")
(define-runtime-path logo-path2 "mormon-small.png")
(define top-right-logos (list (bitmap logo-path1) (bitmap logo-path2)))
(define current-logo-clickback (make-parameter void))

(define (list-ref-random l)
  (list-ref l (random (length l))))

(define (icon-assembler
         #:logo-scale [logo-scale 1])
  (let ([orig (current-slide-assembler)])
    (current-slide-assembler
     (lambda (title sep content)
       (define logo-page
         (rt-superimpose 
          (size-in-pixels (clickback (scale (list-ref-random top-right-logos)
                                            logo-scale)
                                     (current-logo-clickback)))
          full-page))
       (define whole-page
         (orig title sep content))
       (if ((pict-width logo-page) . < . (pict-width whole-page))
           (lt-superimpose whole-page 
                           logo-page)
           (cc-superimpose whole-page 
                           logo-page))))))
