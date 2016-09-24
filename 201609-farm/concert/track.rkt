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
   ;; Dr Mario - soft introduction
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-diminished)]
                  [tempo/e (fin/e 140)]
                  [drums/e (fin/e i:drums:off)])
     #f #f))
   
   ;; FFI - peppy menu
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-major-pentatonic)]
                  [drum-measure/e
                   (λ (ts ap)
                     (fin/e beat:funk-beat))]
                  [tempo/e (fin/e 180)])
     #f #f))

   ;; Castlevania - action sequence
   (vector
    (t 1 12 00)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-diatonic-major)]
                  [drums/e drums/e]
                  [drum-measure/e
                   (λ (ts ap)
                     (fin/e beat:straight-rock beat:duple-triplets
                            beat:heavy-metal))]
                  [tempo/e (fin/e 240)])
     #f #f))

   ;; Castlevania - dark sequence
   (vector
    (t 0 36 00)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-harmonic-minor)]
                  [tempo/e (fin/e 110)]
                  [drums/e (fin/e i:drums:off)])
     #f #f))
   
   ;; Tetris - happy sequence
   (vector
    (t 0 30 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-whole-tone)]
                  [drum-measure/e
                   (λ (ts ap)
                     (fin/e beat:alternating-on beat:funk-beat))]
                  [tempo/e (fin/e 260)])
     #f #f))
   
   ;; Mega Man 2 - happy
   (vector
    (t 1 25 0)
    (use-bithoven
     #:style
     (struct-copy style style:classic
                  [scales/e (fin/e scale-diatonic-major)]
                  [drums/e drums/e]
                  [drum-measure/e
                   (λ (ts ap)
                     (fin/e beat:straight-rock beat:alternating-on))]
                  [tempo/e (fin/e 200)])
     #f #f))

   ;; Mega Man 2 - all drums
   (vector
    (t 0 28 45)
    (use-bithoven
     #:style
     (struct-copy style style:classic                  
                  [scales/e (fin/e scale-blues)]
                  [tempo/e (fin/e 180)]
                  [drum-measure/e
                   (λ (ts ap)
                     (fin/e beat:heavy-metal))])
     #f #f))
   
   ))

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
