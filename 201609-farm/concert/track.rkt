#lang racket/base
(require racket/match
         data/enumerate
         data/enumerate/lib
         srpnt/dev
         srpnt/nestration
         srpnt/nestration/instruments
         srpnt/music-theory
         srpnt/player
         srpnt/band
         srpnt/mixer
         srpnt/speaker/h
         srpnt/speaker/portaudio)

(define (compile-song* t)
  (match-define (vector l (cons c n)) t)
  (compile-song c n))

(define (compile! dest-p ts)
  (define los (map compile-song* ts))

  (define-values
    (wait? m done!)
    (cond
      [dest-p
       (define o (open-output-file dest-p #:exists 'replace))
       (define m
         (mixer:standard
          (speaker
           (λ (out-bs) (display out-bs o))
           void)))
       (define (done!)
         (close-output-port o))
       (values #f m done!)]
      [else
       (printf "Press a key...\n")
       (read-char)
       (define bp (make-bytes-player))       
       (values #t
               (mixer:standard
                (speaker
                 (λ (out-bs)
                   (bytes-play! bp out-bs))
                 void))
               (λ ()
                 (close-bytes-player! bp)))]))

  (for ([t (in-list ts)]
        [s (in-list los)])
    (match-define (vector l _) t)
    (define lms (* 1000 l))
    (define player-t
      (thread
       (λ ()
         (let loop ()
           (play-one!
            #:mixer (λ () m)
            s)
           (when wait?
             (loop))))))
    (cond
      [wait?
       (printf "Waiting for track for ~a\n" lms)
       (sync
        (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds) lms))
         (λ _
           (kill-thread player-t))))
       (printf "Track done.\n")]
      [else
       (thread-wait player-t)]))

  (done!))

(define (t x y z)
  (+ (* 60 x) y (/ z 100)))

(define track-list
  (list
   ;; Dr Mario - XXX
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 120)])
     #f #f))
   ;; FFI - XXX
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 120)])
     #f #f))
   ;; Castlevania - XXX
   (vector
    (t 1 50 11)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 120)])
     #f #f))
   ;; Tetris - XXX
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 120)])
     #f #f))
   ;; Mega Man 2 - XXX
   (vector
    (t 1 53 45)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 120)])
     #f #f))))

(module+ main
  (require racket/cmdline
           racket/runtime-path)
  (define-runtime-path here ".")

  (define live? #f)

  (command-line #:program "track"
                #:once-each
                ["--live" "Run live" (set! live? #t)])

  (compile!
   (if live? #f (build-path here "track.bin"))
   track-list))
