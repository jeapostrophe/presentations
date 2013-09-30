#lang racket/base
(require racket/runtime-path
         racket/list
         pict
         racket/class
         racket/gui/base
         slideshow/base)

(provide (all-defined-out))

(define (enable-page-numbers!)
  (define pnf (send the-font-list find-or-create-font 36 'default 'normal 'normal))
  (current-page-number-font pnf)
  (set-page-numbers-visible! #t))

(define (title/movie #:title t #:subtitle st #:movie-imgs md)
  (define bms
    (for/vector ([f (in-list (directory-list md))])
      (make-object bitmap% (build-path md f) 'png/alpha)))
  (define rep-bm (vector-ref bms 0))
  (define w (send rep-bm get-width))
  (define h (send rep-bm get-height))
  (define cw client-w)
  (define (pict-backgroundize p c)
    (cc-superimpose (colorize (filled-rectangle cw (pict-height p)) c) p))
  (define pre-movie
    (pict-backgroundize
     (vc-append (colorize (text t '(bold caps) 64) "pink")
                (colorize (text st '(italic) 32) "lightblue"))
     "black"))
  (define ch (- client-h (pict-height pre-movie)))
  (slide
   (inset
    (vc-append
     pre-movie
     (interactive
      (filled-rectangle cw ch)
      (λ (f)
        (define i 0)
        (define c
          (new canvas%
               [parent f]
               [paint-callback
                (λ (c dc)
                  (send dc set-background "pink")
                  (send dc clear)
                  (draw-bm-on-c-scaled dc (vector-ref bms i) w h cw ch 1))]))
        (define t
          (thread
           (λ ()
             (let loop ()
               (set! i
                     (modulo (add1 i)
                             (vector-length bms)))
               (send c refresh-now)
               (sleep (/ 1 8.5))
               (loop)))))
        (λ ()
          (kill-thread t)))))
    (- margin))))

(define (draw-bm-on-c-scaled dc bm w h cw ch [hack? 0])
  (define it (send dc get-transformation))  
  (send dc set-smoothing 'unsmoothed)
  (send dc set-scale
        (+ hack? (/ cw w))
        (+ hack? (/ ch h)))

  (send dc draw-bitmap-section
        bm
        0 0
        0 0 w h)

  (send dc set-transformation it))
