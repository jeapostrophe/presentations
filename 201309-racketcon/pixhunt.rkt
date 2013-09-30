#lang racket/gui

(define (hunt p)
  (define f (new frame% [label ""]))
  (define b (make-object bitmap% p))
  (define w (send b get-width))
  (define h (send b get-height))
  (define CX 0)
  (define CY 0)

  (define mcanvas%
    (class* canvas% ()
      (define/override (on-char k)
        (match (send k get-key-code)
          [ 'left (set! CX (sub1 CX))]
          ['right (set! CX (add1 CX))]
          [   'up (set! CY (sub1 CY))]
          [ 'down (set! CY (add1 CY))]
          [else
           (void)])
        (send f set-label (format "~a,~a" CX CY))
        (send c refresh-now))

      (super-new)))

  (define c
    (new mcanvas%
         [parent f]
         [paint-callback
          (λ (c dc)
            (send dc set-background "pink")
            (send dc clear)

            (define cw (send c get-width))
            (define ch (send c get-height))

            (define it (send dc get-transformation))
            (send dc set-smoothing 'unsmoothed)
            (send dc set-scale
                  (add1 (/ cw w))
                  (add1 (/ ch h)))

            (send dc draw-bitmap b 0 0)

            (send dc set-brush
                  (new brush% [color "white"]))
            (send dc set-pen
                  (new pen% [width 0] [color "white"]))
            (send dc draw-rectangle CX CY 1 1)

            (send dc set-transformation it))]))
  (send f show #t)
  (printf "~a\n" p))

(module+ main
  (command-line
   #:args (p) (hunt p)))
