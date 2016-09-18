#lang at-exp racket/base
(require racket/list
         racket/match
         racket/port
         racket/string
         unstable/error
         "charterm.rkt")

(define (clamp lo x hi)
  (max lo (min x hi)))

(define (default-slide
          #:slide [slide-i #f]
          #:slides [slides #f])
  (charterm-clear-screen))

(define (go! file)
  (define (read-slides)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (list
                        (λ (#:slide x #:slides y)
                          (charterm-clear-screen)
                          (define es
                            (with-output-to-string
                              (λ () (displayln (exn-message e)))))
                          (for ([i (in-naturals)]
                                [e (in-list (string-split es #rx"\n"))])
                            (charterm-cursor 0 i)
                            (charterm-display es)
                            (charterm-newline)))))])
      (define ns (make-base-namespace))
      #;(namespace-attach-module (current-namespace) 'charterm ns)
      (parameterize ([current-namespace ns])
        (namespace-require `(file ,file))
        (namespace-variable-value 'slides))))

  (define slides (vector default-slide))
  (define (reload!)
    (set! last-i 0)
    (set! slides
          (list->vector
           (filter procedure?
                   (flatten (read-slides))))))

  (define last-i 0)
  (define slide-i 0)
  (define (up! f)
    (set! slide-i (clamp 0 (f slide-i) (sub1 (vector-length slides)))))
  (define (next!) (up! add1))
  (define (prev!) (up! sub1))
  (define (display!)
    (define how-many (vector-length slides))
    (for ([disp-i (in-range last-i (min (add1 slide-i) how-many))])
      (define this-slide (vector-ref slides disp-i))
      (this-slide #:slide disp-i #:slides how-many)))

  (define (exit!)
    (exit))

  (reload!)
  (let loop ()
    (display!)
    (match (charterm-read-key)
      [(or 'right 'space 'return #\d #\space)
       (next!)]
      [(or 'left #\a)
       (prev!)]
      [(or #\r)
       (reload!)]
      [(or #\q 'escape)
       (exit!)]
      [_
       (void)])
    (loop)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "slideshow"
   #:args (file)
   (with-charterm
     (go! file))))
