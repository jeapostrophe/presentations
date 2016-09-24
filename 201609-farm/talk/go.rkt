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

(require srpnt/speaker
         srpnt/apu
         srpnt/player
         srpnt/dev
         srpnt/synth
         srpnt/band
         srpnt/music-theory
         srpnt/tones
         srpnt/bithoven
         data/enumerate
         data/enumerate/lib
         srpnt/nestration/instrument
         srpnt/nestration/instruments
         srpnt/nestration)
(define (old-synth:frame p1 p2 t1 n1 ld rd)
  (synth:frame p1 p2 t1 #f n1 #f #f ld rd))

(define-runtime-path r "r")

;; xxx add line numbers to "code"

(define (introduce topic-l name affiliation)
  (slide
   (cc-superimpose
    plt-title-background
    (vr-append
     (vc-append
      (scale/improve-new-text (t (first topic-l)) 3)
      (scale/improve-new-text (t (second topic-l)) 1.5)
      (scale/improve-new-text (t (third topic-l)) 1.5)
      (scale/improve-new-text (t (fourth topic-l)) 1.5))
     (vr-append
      (scale/improve-new-text (t name) 1)
      (scale/improve-new-text (t (string-append affiliation " & PLT")) 0.5))))))

(define-syntax-rule (cslide e)
  (slide (lt-superimpose e (blank client-w client-h))))

(define (bitslide i)
  (slide (scale-to-fit (bitmap (build-path r i)) client-w client-h)))

(define (bytes->number b)
  (for/fold ([x 0]) ([b (in-bytes b)])
    (+ (* x 256) b)))

(define SRPNT-t (thread void))
(define (stop-srpnt!)
  (kill-thread SRPNT-t))
(define (sample ->a
                #:allow-input? [allow-input? #f])
  (define (play! bn cn)
    (stop-srpnt!)
    (set! SRPNT-t
          (thread (λ () (play-one! (->a bn cn))))))
  (hc-append
   (clickback
    (bitmap (play-icon #:color run-icon-color #:height 32))
    (λ () (play! #f #f)))
   (if allow-input?
     (hc-append
      (blank 5 5)
      (clickback
       (bitmap (record-icon #:color "blue" #:height 32))
       (λ ()
         (define n
           (bytes->number
            (string->bytes/utf-8
             (get-text-from-user "Slideshow" "What song?"))))
         (play! n n)))
      (blank 5 5))
     (blank 5 5))
   (clickback
    (bitmap (stop-icon #:color halt-icon-color #:height 32))
    stop-srpnt!)))

(define (sample-song ->cn
                     #:allow-input? [allow-input? #f])
  (sample
   #:allow-input? allow-input?
   (λ (bn cn)
     (match-define (cons c n) (->cn bn cn))
     (compile-song c n))))

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

  (define progress 0)
  (define (progress! start end %)
    (set! progress (+ start (* end %))))
  (define default-slide-assembler (current-slide-assembler))
  (current-slide-assembler
   (λ (title title-sep content-pict)
     (lb-superimpose
      (cc-superimpose
       (blank client-w client-h)
       (default-slide-assembler
         title title-sep content-pict))
      (filled-rectangle #:color "Thistle"
                        (* client-w progress)
                        5))))

  (introduce
   (list "Bithoven"
         "Gödel Encoding of Chamber Music"
         "and"
         "Functional 8-Bit Audio Synthesis")
   "Jay McCarthy"
   "UMass Lowell")

  (progress! 0 1/4 1/5)
  (bitslide "NES.jpg")

  (progress! 0 1/4 2/5)
  (bitslide "Famicom.jpg")

  (progress! 0 1/4 3/5)
  (bitslide "RP2A07.jpg")

  (progress! 0 1/4 4/5)
  (bitslide "Gates.png")

  (define (apu-demo . _)
    (define f
      (old-synth:frame (wave:pulse 1 253 7) #f #f #f #f #f))
    (make-list 60 f))
  (define (nco-demo . _)
    (define how-many (frames-in-note (cons 0.25 60) 0.25))
    (define (f t)
      (old-synth:frame (wave:pulse 2 (pulse-tone->period t) 7)
                       #f #f #f #f #f))
    (append
     (make-list how-many (f 'C3))
     (make-list how-many (f 'D3))
     (make-list how-many (f 'E3))
     (make-list how-many (f 'F3))))
  (define (chords-demo scale)
    (define how-many (frames-in-note (cons 0.25 60) 0.25))
    (define (->t c base)
      (match-define (cons tone octave) c)
      (string->symbol
       (format "~a~a" tone (+ octave base))))
    (define (f i)
      (match-define
        (list p1 p2 t)
        (chord-triad (mode scale i)))
      (old-synth:frame (wave:pulse 2 (pulse-tone->period (->t p1 4))
                                   (if (odd? i) 8 7))
                       (wave:pulse 0 (pulse-tone->period (->t p2 3))
                                   (if (odd? i) 8 7))
                       (wave:triangle #t (triangle-tone->period (->t t 2)))
                       #f #f #f))
    (append
     (make-list how-many (f 0))
     (make-list how-many (f 1))
     (make-list how-many (f 2))
     (make-list how-many (f 3))))
  (define (scales-and-chords-demo . _)
    (chords-demo (scale-diatonic-major 'C)))

  (progress! 0 1/4 5/5)

  (define last-bithoven-p (blank 25 35))
  (define last-bithoven-canvas% #f)
  (define last-bithoven
    (interactive
     (blank client-w 25)
     (λ (frame%)
       (set! last-bithoven-canvas%
             (new canvas%
                  [parent frame%]
                  [paint-callback
                   (λ (c% dc%)
                     (draw-pict last-bithoven-p dc% 0 0))]))
       void)))
  (define (update-last-bithoven! nums)
    (match-define (cons bn cn) nums)
    (set! last-bithoven-p
          (vl-append (text (number->string bn))
                     (text (number->string cn))))
    (when last-bithoven-canvas%
      (send last-bithoven-canvas% refresh-now)))

  (slide #:title "Outline"
         (item (sample apu-demo)
               "Waves")
         'next
         (item (sample nco-demo)
               "Notes")
         'next
         (item (sample scales-and-chords-demo)
               "Scales and Chords")
         'next
         (item (sample-song
                (λ _
                  (use-bithoven
                   #:inform update-last-bithoven!
                   #:style
                   (struct-copy style style:classic
                                [scales/e (fin/e scale-diatonic-major)]
                                [tempo/e (fin/e 240)])
                   #f #f)))
               "Music")
         last-bithoven)

  (tslide "Synthesis")

  (define (pulse-demo duty)
    (λ _
      (define f
        (old-synth:frame (wave:pulse duty 253 7) #f #f #f #f #f))
      (make-list 60 f)))
  (define (pulse-points duty)
    (let ([% 0.0])
      (for/list ([i (in-range samples-per-buffer)])
        (define-values (p n%) (pulse-wave duty 253 7 %))
        (set! % n%)
        (list i p))))
  (define (pulse-pict duty)
    (pl:plot-pict
     (pl:points (pulse-points duty))
     #:x-min 0
     #:x-max samples-per-buffer
     #:y-min 0
     #:y-max 15))

  (define (triangle-demo . _)
    (define f
      (old-synth:frame #f #f (wave:triangle #t 126) #f #f #f))
    (make-list 60 f))
  (define triangle-points
    (let ([% 0.0])
      (for/list ([i (in-range samples-per-buffer)])
        (define-values (t n%) (triangle-wave #t 126 %))
        (set! % n%)
        (list i t))))
  (define triangle-pict
    (pl:plot-pict
     (pl:points triangle-points)
     #:x-min 0
     #:x-max samples-per-buffer
     #:y-min 0
     #:y-max 15))

  (define (noise-demo . _)
    (define f
      (old-synth:frame #f #f #f (wave:noise #f #xC 7) #f #f))
    (make-list 60 f))
  (define (noise-points short?)
    (let ([r 1] [% 0.0])
      (for/list ([i (in-range samples-per-buffer)])
        (define-values (n nr n%) (noise short? #xC 7 r %))
        (set! % n%)
        (set! r nr)
        (list i n))))
  (define noise-pict
    (pl:plot-pict
     (pl:points (noise-points #f))
     #:x-min 0
     #:x-max samples-per-buffer
     #:y-min 0
     #:y-max 15))

  (define (scale-plot p)
    (scale-to-fit p
                  (* 1/2 client-w)
                  (* 1/4 client-h)))

  (progress! 1/4 1/4 0/5)
  (slide
   (table 3
          (list (t "Pulse 1") (sample (pulse-demo 0)) (scale-plot (pulse-pict 0))
                (t "Pulse 2") (sample (pulse-demo 2)) (scale-plot (pulse-pict 2))
                (t "Triangle") (sample triangle-demo) (scale-plot triangle-pict)
                (t "Noise") (sample noise-demo) (scale-plot noise-pict))
          cc-superimpose cc-superimpose 10 10))

  (progress! 1/4 1/4 1/5)
  (cslide
   (code
    (define DUTY (vector 0.125 0.25 0.50 0.75))

    (define (pulse-wave n period volume %)
      (define freq
        (pulse-period->freq period))
      (define duty-cycle
        (vector-ref DUTY n))
      (define next-% (cycle%-step % freq))
      (define out
        (if (< next-% duty-cycle)
          volume
          0))
      (values out next-%))))

  (progress! 1/4 1/4 2/5)
  (cslide
   (code
    (define CPU-FREQ-MHz 1.789773)

    (define CPU-FREQ-Hz
      (* CPU-FREQ-MHz 1000.0 1000.0))

    (define (pulse-period->freq period)
      (/ CPU-FREQ-Hz (* 16.0 (+ 1.0 period))))

    (define (cycle%-step % freq)
      (define %step (/ freq 44100.0))
      (define next% (+ % %step))
      (- next% (floor next%)))))

  (progress! 1/4 1/4 3/5)
  (cslide
   (code
    (struct wave:pulse (duty period volume))
    (struct wave:triangle (on? period))
    (struct wave:noise (short? period volume))
    (struct wave:dmc (bs offset))
    (struct synth-frame (p1 p2 t n d))))

  (progress! 1/4 1/4 4/5)
  (cslide
   (code
    (struct synth-frame (p1))
    (define (synth sf)
      (match-define (synth-frame p1) sf)
      (match-define (wave:pulse d p v) p1)
      (define sample-count 735)
      (define samples (make-bytes sample-count))
      (for/fold ([p1-% 0.0])
                ([i (in-range sample-count)])
        (define-values (p1 new-p1-%)
          (pulse-wave d p v p1-%))
        (bytes-set! samples i p1-d)
        new-p1-%)
      samples)))

  (define (all-demo . _)
    (define f
      (old-synth:frame (wave:pulse 2 253 7) (wave:pulse 0 253 7)
                       (wave:triangle #t 126)
                       (wave:noise #f #xC 7) #f #f))
    (make-list 60 f))
  (progress! 1/4 1/4 5/5)
  (slide
   (pl:plot-pict
    (pl:points
     (for/list
         ([p1 (in-list (map second (pulse-points 2)))]
          [p2 (in-list (map second (pulse-points 0)))]
          [t (in-list (map second triangle-points))]
          [n (in-list (map second (noise-points #f)))]
          [i (in-naturals)])
       (define p (p-mix p1 p2))
       (define tnd (tnd-mix t n 0))
       (list i (fx+ 128 (fx+ p tnd)))))
    #:x-min 0
    #:x-max samples-per-buffer
    #:y-min 0
    #:y-max 255)
   (sample all-demo))

  (tslide "Instruments")

  (progress! 2/4 1/4 1/8)
  (cslide
   (code
    (define (pulse-freq->period freq)
      (define pre (/ CPU-FREQ-Hz (* 16.0 freq)))
      (round (- pre 1.0)))

    (define pulse-tone->period
      (compose1 pulse-freq->period tone->freq))))

  (progress! 2/4 1/4 2/8)
  (cslide
   (code
    (define (i:pulse #:duty ds
                     #:period ps
                     #:volume vs)
      (λ (frames tone)
        (define d* (stage-spec ds frames))
        (define p* (stage-spec ps frames))
        (define v* (stage-spec vs frames))
        (define base-per
          (pulse-tone->period tone))
        (for/list ([f (in-range frames)])
          (define duty (eval-spec d* f))
          (define per
            (fx+ base-per (eval-spec p* f)))
          (define volume (eval-spec v* f))
          (wave:pulse duty per volume))))))

  (define (i:pulse-demo i)
    (λ _
      (define how-many (frames-in-note (cons 0.25 60) 0.25))
      (map
       (λ (p1)
         (old-synth:frame p1 #f #f #f #f #f))
       (append
        (i how-many (cons 'C3 #f))
        (i how-many (cons 'D3 #t))
        (i how-many (cons 'E3 #f))
        (i how-many (cons 'F3 #t))))))

  (progress! 2/4 1/4 3/8)
  (cslide
   (vl-append
    (code
     (define (i:pulse:basic duty)
       (i:pulse
        #:duty (spec:constant duty)
        #:period (spec:constant 0)
        #:volume (spec:constant 7))))
    (sample (i:pulse-demo (i:pulse:basic 2)))))

  (progress! 2/4 1/4 4/8)
  (cslide
   (vl-append
    (code
     (define (i:pulse:tremolo freq duty)
       (i:pulse/spec
        #:duty (spec:constant duty)
        #:period (spec:constant 0)
        #:volume (spec:% (spec:modulate freq 7 4)))))
    (sample (i:pulse-demo (i:pulse:tremolo 60.0 2)))))

  (progress! 2/4 1/4 5/8)
  (cslide
   (vl-append
    (code
     (define (i:pulse:linear duty)
       (i:pulse/spec
        #:duty (spec:constant duty)
        #:period (spec:constant 0)
        #:volume (spec:% (spec:linear 7 0)))))
    (let ()
      (define (i:pulse:linear duty)
        (i:pulse/spec
         #:duty (spec:constant duty)
         #:period (spec:constant 0)
         #:volume (spec:% (spec:linear 7 0))))
      (sample (i:pulse-demo (i:pulse:linear 2))))))

  (progress! 2/4 1/4 6/8)
  (cslide
   (vl-append
    (code
     (define (i:pulse:plucky duty)
       (i:pulse/spec
        #:duty (spec:constant duty)
        #:period (spec:constant 0)
        #:volume
        (spec:adsr 'release
                   4 (spec:constant 14)
                   4 (spec:linear 14 7)
                   4 (spec:constant 7)
                   4 (spec:linear 7 0)))))
    (sample (i:pulse-demo (i:pulse:plucky 2)))))

  (progress! 2/4 1/4 7/8)
  (cslide
   (vl-append
    (code
     (define (i:pulse:natural duty)
       (i:pulse/spec
        #:duty (spec:constant duty)
        #:period (spec:constant 0)
        #:volume
        (spec:adsr 'sustain
                   4 (spec:constant 14)
                   4 (spec:linear 14 7)
                   4 (spec:constant 7)
                   4 (spec:linear 7 0)))))
    (sample (i:pulse-demo (i:pulse:natural 2)))))

  (progress! 2/4 1/4 8/8)
  (slide
   (parameterize ([get-current-code-font-size (λ () 20)])
     (cc-superimpose
      (sample-song
       (λ _
         (test-drums 
          (vector i:drum:hihat
                  i:drum:bass
                  i:drum:snare))))
      (rt-superimpose
       (code
        (define i:drum:snare
          (i:noise
           #:mode
           (spec:constant #f)
           #:period
           (spec:constant 7)
           #:volume
           (spec:adsr
            'release
            1 (spec:constant 11)
            4 (spec:linear 11 6)
            8 (spec:linear 6 2)
            4 (spec:constant 0)))))
       (ct-superimpose
        (code
         (define i:drum:bass
           (i:noise
            #:mode
            (spec:constant #f)
            #:period
            (spec:constant 9)
            #:volume
            (spec:adsr
             'release
             1 (spec:constant 10)
             2 (spec:constant 7)
             4 (spec:linear 4 2)
             4 (spec:constant 0)))))
        (lt-superimpose
         (code
          (define i:drum:hihat
            (i:noise
             #:mode
             (spec:constant #f)
             #:period
             (spec:constant #xC)
             #:volume
             (spec:adsr
              'release
              1 (spec:constant 4)
              2 (spec:constant 3)
              4 (spec:constant 2)
              4 (spec:constant 0)))))
         (blank client-w client-h)))))))
  
  (progress! 3/4 1/4 1/6)
  (slide #:title "Music Theory"
         'next (item "Notes")
         'next (item "Tempo")
         'next (item "Time Signature")
         'next (item "Scale")
         'next (subitem "Detached Scale")
         'next (subitem "Fixed Scale")
         'next (subitem "Abstract Scale")
         'next (subitem "Abstract Tone")
         'next (item "Tracker")
         'next (item "Chord"
                     (sample (λ _
                               (define e (cons/e tone-names/e scales/e))
                               (match-define
                                 (cons t sc)
                                 (from-nat e (random-index e)))
                               (chords-demo (sc t))))))

  (progress! 3/4 1/4 2/6)
  (slide #:title (code (require data/enumerate))
         'next
         (code (to-nat string/e "Bithoven"))
         (t (format "=> ~a"
                    (to-nat string/e "Bithoven")))
         'next
         (code (from-nat string/e 42))
         (t (format "=> ~v"
                    (from-nat string/e 42))))

  (progress! 3/4 1/4 3/6)
  (cslide
   (parameterize ([get-current-code-font-size (λ () 25)])
     (code
      (define (part/e ts ap cp measures len)
        (define cp-s (progression-seq cp))
        (define pulses
          (* len measures (accent-pattern-pulses-per-measure ap)))
        (define cp/e
          (chord-pulses/e pulses (length cp-s)))
        (do/e cps <- cp/e
              cp  <~ cps
              (rhythm/e ts (* cp (accent-pattern-notes-per-pulse ap)))))
      (define bithoven/e
        (vector/e
         (do/e          ts <- time-sig/e
                        ap <- (accent-pattern/e ts)
               (cons f cp) <- (cons/e form/e chord-progression/e)
                         p <~ (form-part-lens f)
               (part/e ts ap cp (length (progression-seq cp)) (cdr p)))
         bass-notes/e)))))

  (progress! 3/4 1/4 4/6)
  (cslide
   (parameterize ([get-current-code-font-size (λ () 25)])
     (code
      (define (make-nestration/e c)
        (match-define (vector ts ap pattern parts) c)
        (vector/e
         tone-names/e scales/e tempo/e
         pulse1/e pulse2/e triangle/e drums/e
         mhtb/e
         (fin/e 2 3) (fin/e 1 2) (fin/e 0 2)
         (do/e _ <~ parts
               (drum-measure/e ts ap))
         (do/e ms <~ parts
               rest-n <- rest-n/e
               (if rest-n
                 (listof-n/e
                  (below/e rest-n)
                  (add1 (ceiling (/ (length (append* ms)) rest-n))))
                 (single/e '()))))))))

  (progress! 3/4 1/4 5/6)
  (define-runtime-path srpnt '(lib "main.rkt" "srpnt"))
  (define WC 0)  
  (define (word-count! f a)
    (define c (length (file->lines (build-path (path-only srpnt) f))))
    (set! WC (+ WC c))
    (+ a c))
  (define (wc . l)
    (t (number->string (foldr word-count! 0 l))))
  (define (wc! . l)
    (apply wc l)
    (t (number->string WC)))
  (slide
   (table 2
          (list (t "Synthesizer") (wc "apu.rkt" "mixer.rkt" "synth.rkt")
                (t "Instrument DSL") (wc "nestration/instrument.rkt")
                (t "Music Theory") (wc "music-theory.rkt" "tones.rkt")
                (t "Tracker") (wc "tracker.rkt")
                (t "Bithoven") (wc "bithoven.rkt")
                (t "NEStration") (wc "band.rkt" "nestration/instruments.rkt"
                                     "nestration.rkt")
                (t "TOTAL") (wc! "player.rkt" "speaker/h.rkt" "speaker/portaudio.rkt"
                                 "speaker.rkt"))
          (list lc-superimpose rc-superimpose) cc-superimpose 10 10))

  (define (ubs label s)
    (list (t label)
          (sample-song
           #:allow-input? #t
           (λ (bn cn)
             (use-bithoven
              #:inform update-last-bithoven!
              #:style s
              bn cn)))))
  (progress! 3/4 1/4 6/6)
  (slide
   (table 2
          (flatten
           (list (ubs "Classic" style:classic)
                 (ubs "No Drums"
                      (struct-copy style style:classic
                                   [scales/e scales/e]
                                   [drums/e (fin/e i:drums:off)]))
                 (ubs "Happy" style:happy)
                 (ubs "Sad"
                      (struct-copy style style:sad
                                   [drums/e (fin/e i:drums:off)]))
                 (ubs "All" style:all)
            ))
          (list lc-superimpose cc-superimpose) cc-superimpose 10 10)
   last-bithoven)

  )
