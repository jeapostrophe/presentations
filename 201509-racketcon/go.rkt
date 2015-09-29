#lang at-exp racket/base
(require racket/list
         racket/string
         racket/format
         racket/runtime-path
         racket/gui/base
         racket/match
         racket/flonum
         racket/fixnum
         racket/promise
         racket/pretty
         pict
         pict/code
         pict/flash
         puresuri
         puresuri/plpict
         unstable/gui/pict
         puresuri/lib/title
         puresuri/lib/grid
         puresuri/lib/slide-numbers
         puresuri/lib/cmds
         srpnt/apu
         srpnt/speaker
         (prefix-in pl: plot))
(module+ test)

(define-runtime-path assets "assets")

;; Bithoven is a prolific composer of approximately
;; 107936338584579028906476999435802819420152571696145967835629469168
;; 256054600102650823583510099033608338153987460306459613902701999676
;; 787394921157801398029216877779737562970220998606832732608453952094
;; 357479307728375868180711548797277461020672 different compositions
;; based on three-part harmony and basic chord progressions. The NES
;; Chamber Orchestra is an engine for playing these (and other)
;; compositions on the Ricoh RP2A03 (a.k.a. the NES sound chip) in one
;; of approximately 422234004059019268090786172918417043456000
;; different arrangements or “NEStrations”.

;; In randomized trials, we’ve found that audiences familiar with
;; retro-style video game music can’t tell Bithoven’s compositions
;; apart from “real” NES-era music. Yet, Bithoven contains almost no
;; tuning for producing plausible retro-music and has fewer lines of
;; code than the implementation of /bin/ls. In this talk, we’ll
;; discuss the beautiful Racket programming, elegant mathematics, and
;; basic music theory that makes Bithoven work so well.

(define (background! color)
  (go! (relative-placer 1/2 1/2 'cc))
  (add! (colorize (filled-rectangle slide-w slide-h) color)))

(background! "black")
(let ([X 15] [Y 20])
  (for* ([x (in-range X)]
         [y (in-range Y)])
    (go! (relative-placer (/ (+ x .5) X) (/ (+ y 0.5 0.01) Y) 'cc))
    (add! (colorize (rectangle (/ slide-w X) (/ slide-h Y)) "white")))

  (go! (relative-placer 1/2 0.10 'ct))
  (add! (cellophane (colorize (filled-rectangle slide-w
                                                (* 8 (/ slide-h Y))) "black")
                    0.5))
  (go! (relative-placer 1/2 0.10 'ct))
  (add! (colorize (text "GET BONUS ENTERTAINMENT SYSTEM" 'modern 30) "white"))
  (add! (colorize (text "2015.09.27" 'modern 30) "white"))
  (add! (colorize (text "" 'modern 30) "white"))

  (define start #f)
  (commit! #:effect (λ () (set! start (current-inexact-milliseconds))))
  
  (add! (colorize (text "CHECKING MEMORY....." 'modern 30) "white"))
  (add! (colorize (text "" 'modern 30) "white"))

  
  (add!
   (λ ()
     (define now (current-inexact-milliseconds))
     (define len 10000000)
     (define HALF (/ len 2))
     (define p (inexact->exact (ceiling (* 1000 (- now start)))))
     (define (fmt n)
       (~a (~r #:base 16 #:min-width 12 #:pad-string "0"
               n) "0000"))
     (define OK!
       (~a #:min-width 16 #:align 'right "OK!"))
     (define-values (ca fa)
       (cond
         [(> p len)
          (values OK! OK!)]
         [(< p HALF)
          (values (fmt p) #f)]
         [else
          (define np (- p HALF))
          (values OK! (fmt np))]))
     
     (define code
       (colorize (text (format "CODE AREA...~a" ca)
                                'modern 30) "white"))
     (define free
       (colorize (text (format "FREE AREA...~a" fa)
                       'modern 30) "white"))
     (vc-append code
                (colorize (text "" 'modern 30) "white")
                (if fa free (cellophane free 0))))))

(define (sound-effect p)
  (λ () (play-sound p #t)))

(slide! #:effect (sound-effect (build-path assets "get-bonus.wav")))

(background! "yellow")
(go! (relative-placer 0.06 0.05 'cc))
(add! (text "The" null 60))
(go! (relative-placer 0.26 0.15 'cc))
(add! (text "Get Bonus" 'decorative 100))
(go! (relative-placer 0.31 0.22 'lc))
(add! (text "infinite entertainment system" null 30))

(require srpnt/player
         srpnt/dev
         srpnt/synth
         srpnt/band
         srpnt/music-theory
         srpnt/tones
         srpnt/bithoven
         data/enumerate
         data/enumerate/lib
         srpnt/nestration/instruments
         srpnt/nestration)

(define SRPNT-T #f)
(define-syntax-rule (SRPNT-raw! a)
  (λ ()
    (when SRPNT-T
      (kill-thread SRPNT-T))
    (set! SRPNT-T
          (thread
           (λ ()
             (play-one! a))))))
(define-syntax-rule (SRPNT! a)
  (SRPNT-raw! (let ()
                (match-define (cons c n) a)
                (compile-song c n))))
(define kill-SRPNT!
  (λ ()
    (when SRPNT-T (kill-thread SRPNT-T))
    (set! SRPNT-T #f)))

(define summary-start (save!))
(commit!)

(restore! summary-start)
(go! (relative-placer 0.5 0.35 'rc))
(add! (text "Level 1" 'modern 80))
(define (aka l r)
  (add! (hb-append (text l null 60) (text r null 30))))
(go! (relative-placer 0.25 0.45 'lc))
(aka "Ricoh RP2A03" "")
(commit! #:effect (SRPNT! (apu-demo)))
(commit! #:effect kill-SRPNT!)

(go! (relative-placer 0.5 0.55 'rc))
(add! (text "Level 2" 'modern 80))
(go! (relative-placer 0.25 0.65 'lc))
(aka "NES Chamber Orchestra" "")
(commit! #:effect (SRPNT! (nco-demo)))
(commit! #:effect kill-SRPNT!)

(go! (relative-placer 0.5 0.75 'rc))
(add! (text "Level 3" 'modern 80))
(go! (relative-placer 0.25 0.85 'lc))
(aka "Bithoven" "")
(define summary-end (save!))
(define bithoven-example0:c 17034937447589238875292491048880986423348676677269874515734788835411208296694341070947965714463046928135370824185326249995122150239327473124672517141053289956448792938868933533218648351214565581695124587274339009253612993279292174177459719)
(define bithoven-example0:n 276704817745602746248553755653757320)
(commit! #:effect
         (SRPNT! (use-bithoven
                  #:style
                  (struct-copy style style:classic
                               [tempo/e (fin/e 160)])
                  bithoven-example0:c
                  bithoven-example0:n)))
(slide! #:effect kill-SRPNT!)

(background! "yellow")
(go! (relative-placer 0.5 0.05 'ct))
(define (para-add! lines s)
  (define cols (ceiling (/ (string-length s) lines)))
  (for ([i (in-range lines)])
    (add! (text (~a #:min-width cols
                    (substring s (* i cols)
                               (min (string-length s) (* (add1 i) cols))))
                'modern 60))))
(para-add! 10 (format "(~a . ~a)" bithoven-example0:c bithoven-example0:n))

(slide!)

(background! "pink")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "Ricoh RP2A03" null 50))
(define ricoh (save!))

(commit!) (restore! ricoh)

(go! (relative-placer 1/2 1/2 'cc))
(add! (text "1.789773 MHz" null 100))
(commit!) (restore! ricoh)

(go! (relative-placer 1/2 1/4 'cc))
(add! (text "2 Pulse Waves" null 100))
(add! (text "1 Triangle Wave" null 100))
(add! (text "1 Noise Channel" null 100))
(add! (text "7-bit Samples" null 100))
(commit!) (restore! ricoh)

(go! (relative-placer 0.02 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define (pulse-period->freq period)
      (fl/ CPU-FREQ-Hz (fl* 16.0 (fl+ 1.0 (fx->fl period)))))
    (define (cycle%-step % freq)
      (define %step (fl/ freq sample-rate.0))
      (define next% (fl+ % %step))
      (fl- next% (flfloor next%)))
    (define DUTY-CYCLES (flvector 0.125 0.25 0.50 0.75))
    (define (duty-n->cycle n)
      (flvector-ref DUTY-CYCLES n))
    (define (pulse-wave duty-n period volume %)
      (define freq (pulse-period->freq period))
      (define duty-cycle (duty-n->cycle duty-n))
      (define next-% (cycle%-step % freq))
      (define out
        (if (fl< next-% duty-cycle)
            volume
            0))
      (values out next-%)))))
(go! (relative-placer 0.9 0.9 'rb))
(commit!)

(define pulse-code (save!))
(define pulse-points/duty
  (for/list ([duty (in-list '(0 1 2 3))])
    (define pulse-points
      (let ([% 0.0])
        (for/list ([i (in-range samples-per-buffer)])
          (define-values (p n%) (pulse-wave duty 253 7 %))
          (set! % n%)
          (list i p))))
    (add!
     (pl:plot-pict
      (pl:points pulse-points)
      #:x-min 0
      #:x-max samples-per-buffer
      #:y-min 0
      #:y-max 15))
    (commit! #:effect (SRPNT-raw!
                       (let ()
                         (define f
                           (synth:frame (wave:pulse duty 253 7) #f #f #f #f #f))
                         (make-list 60 f))))
    (commit! #:effect kill-SRPNT!) (restore! pulse-code)
    pulse-points))

(restore! ricoh)

(go! (relative-placer 0.02 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define (triangle-period->freq period)
      (fl/ (pulse-period->freq period) 2.0))
    (define TRIANGLE-PATTERN
      (bytes
       15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
        0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15))
    (define (triangle-wave on? period %)
      (define freq (triangle-period->freq period))
      (define next-% (cycle%-step % freq))
      (define %-as-step
        (fl->fx (flround (fl* next-% 31.0))))
      (define out
        (if on?
            (bytes-ref TRIANGLE-PATTERN %-as-step)
            0))
      (values out next-%)))))
(go! (relative-placer 0.9 0.9 'rb))
(commit!)

(define triangle-points
  (let ([% 0.0])
    (for/list ([i (in-range samples-per-buffer)])
      (define-values (t n%) (triangle-wave #t 126 %))
      (set! % n%)
      (list i t))))
(add!
 (pl:plot-pict
  (pl:points triangle-points)
  #:x-min 0
  #:x-max samples-per-buffer
  #:y-min 0
  #:y-max 15))

(commit! #:effect (SRPNT-raw!
                       (let ()
                         (define f
                           (synth:frame #f #f (wave:triangle #t 126) #f #f #f))
                         (make-list 60 f))))
(commit! #:effect kill-SRPNT!)
(restore! ricoh)

(go! (relative-placer 0.02 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 20)])
   (code
    (define NOISE-PERIODS
      (vector 4 8 16 32 64 96 128 160 202 254 380 508 762 1016 2034 4068))    
    (define (noise-period->freq period)
      (fl* (pulse-period->freq period) 8.0))
    (define (noise short? period volume register %)
      (define freq (noise-period->freq period))
      (define next-% (cycle%-step % freq))
      (define next-register
        (cond
          [(fl< next-% %)
           (define (bit i) (bitwise-bit-field register i (fx+ i 1)))
           (define other-bit (if short? 6 1))
           (define feedback (bitwise-xor (bit 0) (bit other-bit)))
           (define shifted-ref (arithmetic-shift register -1))
           (define feedback-at-bit14 (arithmetic-shift feedback 14))
           (bitwise-ior shifted-ref feedback-at-bit14)]
          [else
           register]))
      (values
       (fx* volume (fxmodulo next-register 2))
       next-register
       next-%)))))
(go! (relative-placer 0.9 0.9 'rb))
(commit!)

(define noise-code (save!))
(define noise-points/short?
  (for/list ([short? (in-list '(#f #t))])
    (define noise-points
      (let ([r 1] [% 0.0])
        (for/list ([i (in-range samples-per-buffer)])
          (define-values (n nr n%) (noise short? #xC 7 r %))
          (set! % n%)
          (set! r nr)
          (list i n))))
    (add!
     (pl:plot-pict
      (pl:points noise-points)
      #:x-min 0
      #:x-max samples-per-buffer
      #:y-min 0
      #:y-max 15))
    (commit! #:effect (SRPNT-raw!
                       (let ()
                         (define f
                           (synth:frame #f #f #f (wave:noise short? #xC 7) #f #f))
                         (make-list 60 f))))
    (commit! #:effect kill-SRPNT!) (restore! noise-code)
    noise-points))

(restore! ricoh)

(go! (relative-placer 0.02 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define (raw-p-mix p1 p2)
      (fl/ 95.88
           (fl+ (fl/ 8128.0
                     (fx->fl (fx+ p1 p2)))
                100.0)))
    (define (raw-tnd-mix t n d)
      (fl/ 159.79
           (fl+ (fl/ 1.0
                     (fl+ (fl/ (fx->fl t) 8227.0)
                          (fl+ (fl/ (fx->fl n) 12241.0)
                               (fl/ (fx->fl d) 22638.0))))
                100.0)))
    (define (mix p tnd)
      (fx+ 128 (fx+ p tnd))))))
(go! (relative-placer 0.9 0.9 'rb))
(commit!)

(add!
 (pl:plot-pict
  (pl:points
   (for/list
       ([p1 (in-list (map second (list-ref pulse-points/duty 2)))]
        [p2 (in-list (map second (list-ref pulse-points/duty 0)))]
        [t (in-list (map second triangle-points))]
        [n (in-list (map second (list-ref noise-points/short? 0)))]
        [i (in-naturals)])
     (define p (p-mix p1 p2))
     (define tnd (tnd-mix t n 0))
     (list i (fx+ 128 (fx+ p tnd)))))
  #:x-min 0
  #:x-max samples-per-buffer
  #:y-min 0
  #:y-max 255))

(commit! #:effect (SRPNT-raw!
                   (let ()
                     (define f
                       (synth:frame (wave:pulse 2 253 7) (wave:pulse 0 253 7)
                                    (wave:triangle #t 126)
                                    (wave:noise #f #xC 7) #f #f))
                     (make-list 60 f))))
(slide! #:effect kill-SRPNT!)

(background! "tomato")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "NES Chamber Orchestra" null 50))
(define nco (save!))

(commit!) (restore! nco)

(go! (relative-placer 0.01 0.2 'lc))
(add! (text "tones = frequencies" null 60))
(add! (text "notes = 2^{-n}, 0 <= n <= 4" null 60))
(add! (text "metronome = note x bpm" null 60))
(go! (relative-placer 0.01 0.9 'lb))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define (frames-in-note.0 me note)
      (match-define (cons beat-unit beats-per-minute) me)
      (define beats-per-second
        (fl/ (fx->fl beats-per-minute) 60.0))
      (define beats-per-frame (fl/ beats-per-second 60.0))
      (define frames-per-beat (fl/ 1.0 beats-per-frame))
      (define beats-in-note (fl/ note beat-unit))
      (define frames-in-note
        (fl* beats-in-note frames-per-beat))
      frames-in-note))))

(commit! #:effect (SRPNT-raw!
                   (let ()
                     (define how-many (frames-in-note (cons 0.25 60) 0.25))
                     (define (f t)
                       (synth:frame (wave:pulse 2 (pulse-tone->period t) 7)
                                    #f #f #f #f #f))
                     (append
                      (make-list how-many (f 'C3))
                      (make-list how-many (f 'D3))
                      (make-list how-many (f 'E3))
                      (make-list how-many (f 'F3))))))
(commit! #:effect kill-SRPNT!) (restore! nco)

;; Then do the same for time-signatures and bars

(go! (relative-placer 0.01 0.2 'lc))
(add! (text "scale = list tone, octave" null 60))
(add! (text "scale kind = tone -> scale" null 60))
(add! (text "mode = scale rotation" null 60))
(add! (text "chord (triad) = tones 2 2 0" null 60))

(go! (relative-placer 0.01 0.9 'lb))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define-scale scale-diatonic-major '(2 2 1 2 2 2 1))
    (define-scale scale-natural-minor '(2 1 2 2 1 2 2))
    (define-scale scale-melodic-minor '(2 1 2 2 2 2 1))
    (define-mode mode-ionian 0)
    (define-mode mode-dorian 1)
    (define-mode mode-phrygian 2))))

(commit! #:effect (SRPNT-raw!
                   (let ()
                     (define how-many (frames-in-note (cons 0.25 60) 0.25))
                     (define scale (scale-diatonic-major 'C))
                     (define (->t c base)
                       (match-define (cons tone octave) c)
                       (string->symbol
                        (format "~a~a" tone (+ octave base))))
                     (define (f i)
                       (match-define
                         (list p1 p2 t)
                         (chord-triad (mode scale i)))
                       (synth:frame (wave:pulse 2 (pulse-tone->period (->t p1 4))
                                                (if (odd? i) 8 7))
                                    (wave:pulse 0 (pulse-tone->period (->t p2 3))
                                                (if (odd? i) 8 7))
                                    (wave:triangle #t (triangle-tone->period (->t t 2)))
                                    #f #f #f))
                     (append
                      (make-list how-many (f 0))
                      (make-list how-many (f 1))
                      (make-list how-many (f 2))
                      (make-list how-many (f 3))))))
(commit! #:effect kill-SRPNT!) (restore! nco)

(commit!)  (restore! nco)

(go! (relative-placer 0.01 0.2 'lc))
(add! (text "song = list parts" null 60))
(add! (text "part = list measures" null 60))
(add! (text "measure = list pulses" null 60))
(add! (text "pulse = note, atone x3, drum, em?" null 60))
(add! (text "atone = scale-idx, d-octave" null 60))
(go! (relative-placer 0.01 0.9 'lb))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (track
     (scale-diatonic-major 'C)
     '((((0.25 [0 4] [2 3] [4 2] #f)
         (0.25 [1 4] [3 3] [5 2] #t)
         (0.25 [2 4] [4 3] [6 2] #f)
         (0.25 [3 4] [5 3] [0 3] #t))))))))

(commit!)  (restore! nco)

(go! (relative-placer 0.01 0.2 'lc))
(add! (text "instrument = frames atone em?" null 60))
(add! (text "                 -> wave" null 60))
(commit!)  (restore! nco)

(go! (relative-placer 0.01 0.2 'lc))
(add! (text "frp-like instrument dsl" null 60))
(commit!)
(add! (text "" null 60))
(add! (text "ADSR" null 60))
(add! (text "attack" null 60))
(add! (text "decay" null 60))
(add! (text "sustain" null 60))
(add! (text "release" null 60))

(go! (relative-placer 0.3 0.35 'lt))
(add!
 (pl:plot-pict
  (pl:function
   (λ (x)
     (cond
       [(< x 0.25)
        (* 4 x)]
       [(< x 0.50)
        (- 1 (* 2 (- x .25)))]
       [(< x 0.75)
        .50]
       [else
        (- 0.5 (* 2 (- x .75)))])))
  #:width (inexact->exact (floor (* slide-w 0.6)))
  #:height (inexact->exact (floor (* slide-h 0.5)))
  #:x-min 0
  #:x-max 1
  #:y-min 0
  #:y-max 1))

(commit!)  (restore! nco)

(define-syntax-rule (demo-pulse-instrument CODE FUN)
  (begin
  (go! (relative-placer 0.01 0.2 'lt))
  (add!
   (parameterize ([get-current-code-font-size (λ () 30)])
     CODE))
  (commit! #:effect (SRPNT-raw!
                     (let ()
                       (define how-many (frames-in-note (cons 0.25 60) 0.25))
                       (map
                        (λ (p1)
                          (synth:frame p1 #f #f #f #f #f))
                        (append
                         ((FUN 2) how-many (cons 'C3 #f))
                         ((FUN 2) how-many (cons 'D3 #t))
                         ((FUN 2) how-many (cons 'E3 #f))
                         ((FUN 2) how-many (cons 'F3 #t)))))))
  (commit! #:effect kill-SRPNT!) (restore! nco)))

(demo-pulse-instrument
 (code
  (define (i:pulse:basic duty)
    (i:pulse/spec
     #:duty (spec:constant duty)
     #:period (spec:constant 0)
     #:volume (spec:constant 7))))
 i:pulse:basic)
(demo-pulse-instrument
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
 i:pulse:plucky)
(demo-pulse-instrument
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
 i:pulse:natural)

(begin
  (go! (relative-placer 0.01 0.2 'lt))
  (add!
   (parameterize ([get-current-code-font-size (λ () 20)])
     (code
      (define hihat-adsr
        (spec:adsr 'release
                   1 (spec:constant 4)
                   2 (spec:constant 3)
                   4 (spec:constant 2)
                   4 (spec:constant 0)))
      (define i:drum:hihat
        (i:drum/spec #:mode (spec:constant #f)
                     #:period (spec:constant #xC)
                     #:volume hihat-adsr))
      (define i:drum:bass
        (i:drum/spec #:mode (spec:constant #f)
                     #:period (spec:constant 9)
                     #:volume
                     (spec:adsr 'release
                                1 (spec:constant 10)
                                2 (spec:constant 7)
                                4 (spec:linear 4 2)
                                4 (spec:constant 0))))
      (define i:drums:basic
        (i:drums (vector i:drum:hihat i:drum:bass i:drum:snare))))))
  (go! (relative-placer 0.50 0.2 'lt))  
  (add!
   (parameterize ([get-current-code-font-size (λ () 20)])
     (code
      (define snare-adsr
        (spec:adsr 'release
                   1 (spec:constant 11)
                   4 (spec:linear 11 6)
                   8 (spec:linear 6 2)
                   4 (spec:constant 0)))
      (define i:drum:snare
        (i:drum/spec #:mode (spec:constant #f)
                     #:period (spec:constant 7)
                     #:volume snare-adsr))
      (define beat:straight-rock
        (list (cons 0.125 1) (cons 0.125 0)
              (cons 0.125 2) (cons 0.125 0)
              (cons 0.125 1) (cons 0.125 0)
              (cons 0.125 2) (cons 0.125 0))))))
  (commit! #:effect (SRPNT!
                     (test-drums 
                      (i:drums (vector i:drum:hihat
                                       i:drum:bass
                                       i:drum:snare)))))
  (commit! #:effect kill-SRPNT!) (restore! nco))

(background! "GreenYellow")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "Bithoven" null 50))
(define bithoven (save!))

(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (require data/enumerate))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define time-sig/e
      (fin/e time-sig/ts:4:4 time-sig/ts:3:4)))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define time-sig->accents
      (hash
       time-sig/ts:4:4
       (list (accent-pattern "standard"  1 '(#t #f #f #f))
             (accent-pattern "on-beats"  2 '(#t #f #t #f))
             (accent-pattern "off-beats" 2 '(#f #t #f #t)))
       time-sig/ts:3:4
       (list (accent-pattern "waltz"  1 '(#t #f #f)))))

    (define (accent-pattern/e ts)
      (apply fin/e (hash-ref time-sig->accents ts))))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define form/e
      (fin/e
       (form "strophic"
             '((A . 1))
             '(A))
       (form "medley"
             '((A . 1) (B . 1) (C . 1) (D . 1))
             '(A B C D))
       (form "double medley"
             '((A . 1) (B . 1) (C . 1) (D . 1))
             '(A A B B C C D D))
       (form "binary"
             '((A . 1) (B . 1))
             '(A B))
       (form "double binary"
             '((A . 1) (B . 1))
             '(A A B B)) ....)))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define chord-progression/e
      (fin/e
       (progression '(0 3 4 4))
       (progression '(0 0 3 4))
       (progression '(0 3 0 4))
       (progression '(0 3 4 3))
       (progression '(0 2 4 4))
       (progression '(0 0 2 4))
       (progression '(0 2 0 4))
       (progression '(0 2 4 3))
       (progression '(0 3 4 2))
       (progression '(0 1 4))
       (progression '(1 4 0))
       (progression '(0 3 4))
       (progression '(0 5 3 4))
       (progression '(5 1 4 0))
       (progression '(0 4 0)) ....)))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 30)])
   (code
    (define (chord-pulses/e pulse-count chord-count)
      (list-of-length-n-summing-to-k-with-no-zeros/e
       chord-count
       pulse-count)))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 25)])
   (code
    (define (part/e ts ap cp measures len)
      (define cp-s (progression-seq cp))
      (define pulses
        (* len measures
           (accent-pattern-pulses-per-measure ap)))
      (define cp/e
        (chord-pulses/e
         pulses
         (length cp-s)))
      (dep/e
       #:one-way? #f
       #:flat? #t
       #:f-range-finite? #t
       cp/e
       (λ (cps)
         (traverse/e
          (λ (cp)
            (rhythm/e
             ts
             (* cp (accent-pattern-notes-per-pulse ap))))
          cps)))))))
(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 12)])
   (code
    (define bithoven/e
      (vector/e
       (dep/e
        #:one-way? #f
        #:flat? #t
        #:f-range-finite? #t
        time-sig/e
        (λ (ts)
          (dep/e
           #:one-way? #f
           #:flat? #t
           #:f-range-finite? #t
           (accent-pattern/e ts)
           (λ (ap)
             (dep/e
              #:one-way? #f
              #:flat? #t
              #:f-range-finite? #t
              (cons/e form/e chord-progression/e)
              (λ (f*cp)
                (match-define (cons f cp) f*cp)
                (define cp-s (progression-seq cp))
                (define measures-per-part
                  (*
                   (let ()
                     (ceiling
                      (/ (length cp-s)
                         (accent-pattern-pulses-per-measure ap))))
                   (let ()
                     (define pat-length (length (form-pattern f)))
                     (cond
                       [(< pat-length 3) 4]
                       [(< pat-length 5) 2]
                       [else 1]))))
                (define (this-kind-of-part/e len)
                  (part/e ts ap cp measures-per-part len))
                (traverse/e
                 (λ (p) (this-kind-of-part/e (cdr p)))
                 (form-part-lens f))))))))
       bass-notes/e)))))

(commit! #:effect
         (SRPNT! (use-bithoven
                  #:style
                  (struct-copy style style:classic
                               [tempo/e (fin/e 160)])
                  bithoven-example0:c
                  bithoven-example0:n)))
(restore! bithoven)

(para-add! 10 (format "~a" bithoven-example0:c))
(commit!) (restore! bithoven)

(define comp-strs
  (string-split
   (pretty-format
    (bithoven->composition
     (from-nat (force p:bithoven/e) 
               bithoven-example0:c)))
   #rx"\n"
   #:trim? #f))

(go! (relative-placer 0.01 0.1 'lt))
(for ([i (in-range 0 57)])
  (add! (text (list-ref comp-strs i) 'modern 10)))
(go! (relative-placer 0.3 0.1 'lt))
(for ([i (in-range 57 114)])
  (add! (text (list-ref comp-strs i) 'modern 10)))
(go! (relative-placer 0.6 0.1 'lt))
(for ([i (in-range 114 (length comp-strs))])
  (add! (text (list-ref comp-strs i) 'modern 10)))

(commit!) (restore! bithoven)

(go! (relative-placer 0.01 0.1 'lt))
(add!
 (parameterize ([get-current-code-font-size (λ () 20)])
   (code
    (vector/e
     tone-names/e scales/e tempo/e
     pulse1/e pulse2/e triangle/e drums/e
     mhb/e
     (fin/e 2 3) (fin/e 1 2) (fin/e 1 2)
     (hash-traverse/e
      #:get-contract
      (λ (x)
        (listof
         (cons/c real? exact-nonnegative-integer?)))
      (λ (_) (drum-measure/e ts ap))
      parts)
     (hash-traverse/e
      #:get-contract
      (λ (x)
        (listof exact-nonnegative-integer?))
      (λ (ms)
        (dep/e
         rest-n/e
         #:f-range-finite? #t
         (λ (rest-n)
           (if rest-n
               (listof-n/e
                (below/e rest-n)
                (add1 (ceiling (/ (length (append* ms)) rest-n))))
               (single/e '())))))
      parts)))))
(commit!) (restore! bithoven)

(para-add! 10 (format "~a" bithoven-example0:n))

(define nes-strs
  (string-split
   (pretty-format
    (let ()
      (define the-style
        (struct-copy style style:classic
                     [tempo/e (fin/e 160)]))
      (define comp
        (bithoven->composition
         (from-nat (force p:bithoven/e) 
                   bithoven-example0:c)))
      (define n/e (make-nestration/e #:style the-style comp))
      (from-nat n/e bithoven-example0:n)))
   #rx"\n"
   #:trim? #f))

(go! (relative-placer 0.2 0.1 'lt))
(for ([i (in-range 0 30)])
  (add! (text (list-ref nes-strs i) 'modern 15)))
(go! (relative-placer 0.6 0.1 'lt))
(for ([i (in-range 30 (length nes-strs))])
  (add! (text (list-ref nes-strs i) 'modern 15)))

(commit! #:effect kill-SRPNT!) (restore! bithoven)

(define (bit-example #:style style)
  (define bit-num-0 #f)
  (define bit-out-0
    (use-bithoven
     #:style style
     #:inform (λ (x) (set! bit-num-0 x))
     #f #f))
  (commit! #:effect (SRPNT! bit-out-0))
  (para-add! 10 (format "~a" bit-num-0))
  (commit! #:effect kill-SRPNT!) (restore! bithoven))

(bit-example #:style style:classic)
(bit-example #:style style:classic)
(bit-example #:style style:happy)
(bit-example #:style style:sad)

(define (bytes->number b)
  (for/fold ([x 0]) ([b (in-bytes b)])
    (+ (* x 256) b)))
(define the-user-song #f)
(commit! #:effect (λ ()
                    (set! the-user-song
                          (bytes->number
                           (string->bytes/utf-8
                            (get-text-from-user "GBES" "What song?"))))
                    (printf "the song is ~a\n" the-user-song)
                    ((sound-effect (build-path assets "Astley-long.wav")))))
(commit! #:effect
         (SRPNT! (use-bithoven
                  #:style style:classic
                  the-user-song
                  the-user-song)))

(restore! summary-end)

(slide!)
(go! (relative-placer 1/2 1/2 'cc))
(add! plt-title-background)
(go! (relative-placer 0.05 0.05 'lt))
(add! (text "  Now" null 60))
(add! (text "You're" null 60))
(add! (text "Playing" null 60))
(add! (text "  With" null 60))
(go! (relative-placer 0.5 0.65 'cc))
(add! (text "RACKET" null 250))
