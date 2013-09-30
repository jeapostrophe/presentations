#lang racket/base
(require racket/runtime-path
         slideshow/base
         "lib.rkt")
(require racket/class
         racket/draw)
(require pict
         pict/face
         racket/list
         racket/match
         racket/string)

;; xxx turn on spotlight
(define o-csa (current-slide-assembler))
(current-slide-assembler
 (λ (title sep content)
   (inset
    (cc-superimpose
     (colorize (filled-rectangle (+ client-w (* 2 margin))
                                 (+ client-h (* 2 margin)))
               "black")
     content)
    (- margin))))
(current-page-number-adjust
 (λ (n s)
   (if (= n 1)
     ""
     s)))
(enable-page-numbers!)

(define-runtime-path imgs "Movie/imgs")
(title/movie
 #:title "The Racket Package System"
 #:subtitle "Planet 5.0 and Beyond!"
 #:movie-imgs imgs)

(current-page-number-color (make-object color% "white"))

(struct char (name pict))

(define text-box-h (+ 3 (* 4 (pict-height (t "blah")))))
(define text-box-thickness 10)
(define text-box-w (- client-w (* 3 text-box-thickness)))
(define text-box-margin 20)

(define (text-box #:char c #:text s)
  (lc-superimpose
   (dc (λ (dc dx dy)
         (define old-brush (send dc get-brush))
         (define old-pen (send dc get-pen))
         (send dc set-brush
               (new brush% [color (make-object color% #x00 #x80 #x80)]))
         (send dc set-pen
               (new pen% [width text-box-thickness] [color "white"]))
         (define path (new dc-path%))
         (send path move-to 0 0)
         (send path line-to text-box-w 0)
         (send path line-to text-box-w text-box-h)
         (send path line-to 0 text-box-h)
         (send path close)
         (send dc draw-path path dx dy)
         (send dc set-brush old-brush)
         (send dc set-pen old-pen))
       text-box-w text-box-h)
   (ht-append
    text-box-margin
    (blank (/ text-box-thickness 2) 0)
    (cc-superimpose (blank 0 (- text-box-h (* 2 text-box-thickness)))
                    (char-pict c))
    (colorize s "white"))))

(define (command-line c)
  (match-define (*command cs os) c)
  (colorize
   (apply
    vl-append (tt (format "% ~a" cs))
    (map (λ (o) (tt (format "    ~a" o))) os))
   "green"))

(struct 6state (last-speech commands fs))
(define init-s (6state #f empty (hash)))

(struct *speech (char text))
(struct *command (text outputs))
(define (command text #:output [os empty])
  (*command text os))
(struct command-clear ())
(struct fs-set (p v))

(define (6exec s a)
  (match-define (6state ls cs fs) s)
  (match a
    [(? *speech?)
     (struct-copy 6state s
                  [last-speech a])]
    [(? *command?)
     (struct-copy 6state s
                  [commands (cons a cs)])]
    [(command-clear)
     (struct-copy 6state s
                  [last-speech (*speech (*speech-char ls) (blank))]
                  [commands empty])]
    [(fs-set p v)
     (struct-copy 6state s
                  [fs (hash-set* fs p v)])]
    [(? list? as)
     (for/fold ([s s]) ([a (in-list as)])
       (6exec s a))]))

(define (hash-set* ht l v)
  (match l
    [(list k)
     (hash-set ht k v)]
    [(list-rest k post)
     (hash-update ht k (λ (k-ht) (hash-set* k-ht post v)) (hash))]))

(define (hash*->list pre fs)
  (for/fold ([l empty])
      ([(k v) (in-hash fs)])
    (match v
      [#f l]
      [sub-fs
       (define new-pre (string-append pre k))
       (match sub-fs
         [(or #t (? string?)) (cons new-pre l)]
         [_ (append l (hash*->list (string-append new-pre "/") sub-fs))])])))

(define (6render s)
  (match-define (6state c cs fs) s)
  (slide
   (ct-superimpose
    (blank client-w client-h)
    (vc-append
     text-box-margin
     (match c
       [#f
        (blank text-box-w text-box-h)]
       [(*speech char text)
        (text-box #:char char #:text text)])
     (rt-superimpose
      (lc-superimpose
       (blank client-w 0)
       (apply vl-append
              (blank)
              (for/list ([c (in-list cs)]
                         ;; xxx 14 is wrong because commands may have
                         ;; multiple lines
                         [i (in-range 14)])
                (cellophane (command-line c)
                            (+ 1/2
                               (* (/ (- (length cs) i)
                                     (length cs))
                                  1/2))))))
      (if #t
        (blank)
        (rc-superimpose
         (blank client-w 0)
         (colorize (apply vr-append
                          (blank)
                          (map tt (sort (hash*->list "/" fs) string-ci<=?)))
                   "pink"))))))))

(define (6slide . actions)
  (for/fold ([s init-s])
      ([a (in-list actions)])
    (define ns (6exec s a))
    (6render ns)
    ns)
  (void))

(define-runtime-path sprite-root "Sprites")
(define (char-sprites-path c)
  (build-path sprite-root (format "SNES - Final Fantasy 6 - ~a.png" c)))
(define (bitmap-crop bm x y w h)
  (define cbm (make-object bitmap% w h #f #t))
  (define cbm-dc (send cbm make-dc))
  (send cbm-dc draw-bitmap-section bm 0 0 x y w h)
  cbm)

;; ul = 62, 170
;; ll = 62, 209
;; ur = 100, 170
;; lr = 100, 209

(define char-portrait-aw 38)
(define char-portrait-ah 39)
(define char-portrait-scale 3.5)
(define char-portrait-dw
  (inexact->exact (floor (* char-portrait-scale char-portrait-aw))))
(define char-portrait-dh
  (inexact->exact (floor (* char-portrait-scale char-portrait-ah))))

(define immoral-hack empty)
(define (char-portrait n ul-x ul-y
                       ;; lr-x lr-y
                       )
  (define obm
    (make-object
     bitmap%
     (char-sprites-path n)))
  (define cbm
    (bitmap-crop
     obm
     ul-x ul-y
     char-portrait-aw char-portrait-ah))

  (define sbm (make-object bitmap% char-portrait-dw char-portrait-dh #f #t))
  (define sbm-dc (send sbm make-dc))
  (draw-bm-on-c-scaled
   sbm-dc
   cbm
   char-portrait-aw char-portrait-ah
   char-portrait-dw char-portrait-dh)

  (define cp (bitmap sbm))
  (set! immoral-hack (cons cp immoral-hack))
  cp)

(define dev1
  (char "Terra" (char-portrait "Terra Branford" 62 170)))
(define user1
  (char "Banon" (char-portrait "Banon" 53 158)))
(define user2
  (char "Celes" (char-portrait "Celes Chere" 79 151)))
(define rdev1
  (char "Racket Developer" (char-portrait "Kappa" 48 166)))
(define user3
  (char "Cyan" (char-portrait "Cyan Garamonde" 62 170)))
(define user4
  (char "Edgar" (char-portrait "Edgar Roni Figaro" 63 170)))
(define user5
  (char "Gau" (char-portrait "Gau" 51 156)))
(define user6
  (char "Locke" (char-portrait "Locke Cole" 50 156)))
(define user7
  (char "Leo" (char-portrait "General Leo Christophe" 98 158)))
(define user8
  (char "Ghost" (char-portrait "Ghost" 49 157)))
(define user9
  (char "Gogo" (char-portrait "Gogo" 144 155)))
(define user10
  (char "Mog" (char-portrait "Mog" 51 159)))
(define user11
  (char "Relm" (char-portrait "Relm Arrowny" 118 158)))
(define user12
  (char "Mash" (char-portrait "Sabin Rene Figaro" 62 170)))
(define user13
  (char "Setzer" (char-portrait "Setzer Gabbiani" 50 155)))
(define user14
  (char "Shadow" (char-portrait "Shadow" 62 170)))
(define user15
  (char "Strago" (char-portrait "Strago Magus" 49 158)))
(define user16
  (char "Umaro" (char-portrait "Umaro" 62 170)))

(define (2s f)
  (string-join f "/"))

(define (:emacs f #:contains [i #t])
  (list (command (format "emacs ~a" (2s f))
                 #:output (cond
                            [(boolean? i) empty]
                            [(string? i) (list i)]
                            [else i]))
        (fs-set f #t)))
(define (:mkdir d)
  (list (command (format "mkdir ~a" (2s d)))
        (fs-set d (hash))))
(define (:mv from to)
  (list (command (format "mv ~a ~a" (2s from) (2s to)))
        (fs-set from #f)
        (fs-set from #t)))
(define :clear command-clear)

(define (speech c t)
  (*speech c (para (bt (format "~a:" (char-name c))) t)))
(define (thought c t)
  (*speech c (parameterize ([current-main-font (cons 'italic (current-main-font))])
               (para (format ".oO ~a" t)))))
(define (email c t)
  (*speech c (para  (bt (format "~a wrote:" (char-name c))) t)))

(6slide
 (thought dev1 "I want to write a program to simulate my experience in Narshe.")
 (:emacs '("magitek.rkt"))
 (:clear)

 (thought dev1 "Now I need to share my package...")
 (command "scp magitek.rkt server:public-html/")
 (email dev1 "Please try my program at: http://terra.com/magitek.rkt")
 (:clear)

 (thought dev1 "I should expand my simulation to include the treasure house.")
 (:mkdir '("narshe"))
 (:mv '("magitek.rkt") '("narshe" "magitek.rkt"))
 (:emacs '("narshe" "lone-wolf.rkt"))
 (command "scp -r narshe server:public-html/")
 (email dev1 "Please try my program at: http://terra.com/narshe")
 (:clear)

 (thought dev1 "Ah, I messed up the first time... let me fix that")
 (:emacs '("narshe" "lone-wolf"))
 (command "scp -r narshe server:public-html/")
 (email dev1 "Please re-download my program at: http://terra.com/narshe")
 (:clear)

 (speech user1 "Did you know you can install as a package to get updates without having to remember what to download?")
 (command "raco pkg install http://terra.com/narshe")
 (command "....")
 (command "raco pkg update")
 (:clear)

 (thought dev1 "I'd like to analyze the sound from Narshe.")
 (list (command "dd if=/dev/dsp of=narshe/soundtrack.wav")
       (fs-set '("narshe" "soundtrack.wav") #t))
 (:emacs '("narshe" "music.rkt"))
 (command "zip -r narshe.zip narshe")
 (command "scp narshe.zip server:public-html/")
 (:clear)

 (list (email dev1 "I just updated my package! The new source is...")
       (command "raco pkg update http://terra.com/narshe.zip"))
 (:clear)

 (thought dev1 "I found a way to improve the Lone Wolf scenario.")
 (:emacs '("narshe" "lone-wolf.rkt"))
 (command "zip -r narshe.zip narshe")
 (command "scp narshe.zip server:public-html/")
 (:clear)

 (email dev1 "I updated the package, you may want to update!")
 (speech user2 "Um, I try to check for updates regularly and it's annoying that it always downloads the whole ZIP file and it is never different.")
 (speech rdev1 "You should be using a checksum file for that.")
 (list (command "md5sum narshe.zip > narshe.zip.CHECKSUM")
       (fs-set '("narshe.zip.CHECKSUM") #t))
 (command "scp narshe.zip.CHECKSUM server:public-html/")
 (:clear)

 (thought dev1 "I found ANOTHER way to improve everything.")
 (:emacs '("narshe" "lone-wolf.rkt"))
 (list (command "zip -r narshe.zip narshe")
       (command "md5sum narshe.zip > narshe.zip.CHECKSUM"))
 (command "scp narshe.zip narshe.zip.CHECKSUM"
          #:output '("server:public-html/"))
 (email dev1 "Please run 'raco pkg update' if you want the new version.")
 (:clear)

 (speech dev1 "Why is it so inconvenient to update my package?")
 (speech rdev1 "It's because you're not using Github.")
 (list (command "cd narshe")
       (command "git init")
       (command "....")
       (command "git push"))
 (list (email dev1 "Thanks, now you should use a new source:")
       (command "raco pkg update git://github.com/terra/narshe"))
 (:clear)

 (thought dev1 "Is it much easier to update now?")
 (list (:emacs '("narshe" "music.rkt"))
       (command "git push"))
 (thought dev1 "That was awesome!")
 (email dev1 "I just did an update everyone!")
 (:clear)

 (speech rdev1 "Please stop spamming the mailing list when your package changes, just put it on the catalog and people can read the RSS feed.")
 (thought dev1 "That's great!")
 ;; FUTURE
 (command "raco pkg catalog-upload narshe"
          #:output
          '("git://github.com/terra/narshe"))
 (list (email dev1 "Now you can use:")
       (command "raco pkg update narshe"))
 (:clear)

 (thought dev1 "Let's make a change...")
 (list (:emacs '("narshe" "music.rkt"))
       (command "git push"))
 (:clear)

 (thought dev1 "I'll go to the next stage of the simulation")
 (list (:mkdir '("returners"))
       (:emacs '("returners" "banon.rkt") #:contains "(require narshe/magitek)"))
 (command "racket returners/banon.rkt"
          #:output '("ERROR"))
 (command "raco pkg install --link narshe")
 (:clear)

 (thought dev1 "Now I need to distribute Returners")
 (list (command "cd returners")
       (command "git init")
       (command "....")
       (command "git push")
       (command "raco pkg catalog-upload ...."))
 (:clear)

 (speech user3 "I tried to install Returners and it died because narshe/magitek wasn't found.")
 (:emacs '("returners" "info.rkt")
         #:contains '("#lang info"
                      "(define deps '(\"narshe\"))"))
 (:clear)

 (thought dev1 "I should really be using tm-halts from Racket v6")
 (:emacs '("narshe" "lone-wolf.rkt")
         #:contains '(".... tm-halts? ...."))
 (:clear)

 (speech user4 "Um, I can't use this in Racket v5.9")
 (list (command "git checkout master^")
       (command "git branch narshe-for-v5.9")
       (command "git push"))
 ;; FUTURE
 (command "raco pkg catalog-version narshe 5.9"
          #:output '("git://github.com/terra/narshe#narshe-for-v5.9"))
 (:clear)

 (speech user5 "Um, when I check this out on my machine into 'narshe-for-v5.9', then it is messed up because I can't require 'narshe/magitek'.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("#lang info"
                      "(define collection \"narshe\")"))
 (speech rdev1 "Internal linking is bad!")
 (:clear)

 (thought dev1 "I have a new feature to include about Locke")
 (:emacs '("narshe" "locke.rkt"))
 (:emacs '("returners" "jidoor.rkt")
         #:contains '("(require narshe/locke)"))

 (speech user6 "I just installed returners on my machine that already has narshe and it was broken because narshe/lock isn't there!")
 (speech rdev1 "You should use version 1.0 if this is the stable interface and version 2.0 if you meant to use version 1.0 before.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define version \"2.0\")"))
 (:emacs '("returners" "info.rkt")
         #:contains '("(define deps '((\"narshe\" #:version \"2.0\")))"))
 (:clear)

 (speech user7 "How do I get version 0.0 of narshe after it has gone? I loved that version even though it didn't last long.")
 (speech rdev1 "This question does not make sense.")

 (thought dev1 "I should play the music rather than analyze it.")
 (:emacs '("narshe" "music-player.rkt")
         #:contains '("(match (system-type) .... dynamic-require ...)"))
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define deps "
                      " '((\"openal\" #:platform macosx)"
                      "   (\"directaudio\" #:platform windows)"
                      "   (\"libsndfile\" #:platform unix)))"))
 (:clear)

 (thought dev1 "Gee, maybe I should write some documentation.")
 (:emacs '("narshe" "narshe.scrbl"))
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define scribblings '((\"narshe.scrbl\")))"))

 (speech user8 "Um, raco setup dies when I install your package because scribble isn't on my EC2 instance.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define deps .... \"scribble-lib\" ....)"))

 (speech user8 "Um, I don't /want/ to install scribble to run your package.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define build-deps '(\"scribble-lib\"))"))
 (:clear)

 (speech dev1 "How do you install it at all without building it?")
 ;; FUTURE
 (command "raco pkg export-pkg --binary narshe")
 (:clear)

 (speech user9 "I do the same thing for my students, but I want them to have the code too, so I use a built package and just download it from the server.")
 ;; FUTURE
 (command "wget http://..../built/v6.0/narshe.zip")
 (:clear)

 (speech dev1 "I'd really like to run 'raco play soundtrack.wav'")
 (:emacs '("narshe" "raco.rkt"))
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define raco-commands ....)"))

 (speech user10 "I installed your package and 'raco play' doesn't work like you say it does.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define setup-collects ....)"))
 (:clear)

 (speech user10 "This music player stuff is cool, but why do I need this Narshe stuff?")
 (:mkdir '("music-player"))
 (:mkdir '("music-player" "music"))
 (command "mv narshe/music-player.rkt"
          #:output '("music-player/music/music-player.rkt"))
 (:mkdir '("music-player" "narshe"))
 (:clear)
 (:emacs '("music-player" "narshe" "music-player.rkt"))
 (:emacs '("music-player" "info.rkt")
         #:contains '("(define collection 'multi)"
                      "(define raco-commands ....)"))
 (command "raco play ...."
          #:output '("ERROR"))
 (:emacs '("music-player" "narshe ""info.rkt")
         #:contains '("(define raco-commands ....)"))
 (:emacs '("narshe ""info.rkt")
         #:contains '("(define deps ....)"))
 (:clear)

 (speech user11 "Um, when I compile my program that built on narshe/music-player, I get a warning from raco setup.")
 (:emacs '("narshe ""info.rkt")
         #:contains '("(define implies ....)"))
 (:clear)

 (speech user12 "Here's a patch that makes it faster by using the GPU!")
 (command "curl ... | git am")
 (command "cat music-player/data/gpu-vector.rkt")

 (speech user13 "Um, I can't install music-player and general-gpu")
 (speech rdev1 "There's a conflict. I think we should talk about it.")
 (command "rm music-player/data/gpu-vector.rkt")
 (speech dev1 "Isn't this backwards incompatible?")
 (:clear)

 (thought dev1 "I have a new implementation idea...")
 (command "emacs narshe/*")
 (speech user14 "Ah, everything is broken!!")
 (speech rdev1 "This is a different package, you should name it different.")
 (list (command "git checkout master^")
       (command "git branch narshe-v1"))

 ;; FUTURE
 (command "raco pkg catalog-source narshe"
          #:output '("git://github.com/terra/narshe#narshe-v1"))
 ;; FUTURE
 (command "raco pkg catalog-upload narshe2"
          #:output
          '("git://github.com/terra/narshe"))
 (:clear)

 (speech user15 "I'd like to run all the versions at the same time for my Narshian simulation simulation environment")
 (speech rdev1 "Yes, it is possible to deprecate completely, but we generally want to allow all old versions.")
 (:emacs '("narshe" "info.rkt")
         #:contains '("(define collection \"narshe2\")"))
 (:clear)

 (speech user16 "I need to make sure I get everything install exactly right on my deployment.")
 ;; FUTURE
 (command "raco pkg export-installed > narshe.com.pkgs")
 (command "scp narshe.com.pkgs server:")
 ;; FUTURE
 (command "raco pkg import-installed < narshe.com.pkgs")
 (:clear)

 (speech user16 "It takes too long to run import-installed, because it installs everything and I'm worried that this Zip-backed package won't have old versions.")
 (speech rdev1 "Zip-backed packages aren't very good.")
 ;; FUTURE
 (command "raco pkg export-pkgs --binary narshe.com.zip"
          #:output '("pkg ..."))
 (command "scp narshe.com.zip server:")
 ;; FUTURE
 (command "raco pkg import-pkgs narshe.com.zip"))

(current-title-color "pink")
(define d-csa (current-slide-assembler))
(current-slide-assembler
 (λ (title sep content)
   (inset
    (ct-superimpose
     (colorize (filled-rectangle (+ client-w (* 2 margin))
                                 (+ client-h (* 2 margin)))
               "black")
     (vc-append
      (blank client-w margin)
      (titlet title)
      (blank client-w margin)
      (colorize content "lightblue")))
    (- margin))))

(slide
 #:title "Thank You!"
 (let ()
   (define N 3)
   (define faces (shuffle immoral-hack))
   (apply vc-append margin
          (for/list ([i (in-range N)])
            (apply ht-append margin
                   (for/list ([e (in-list faces)]
                              [j (in-naturals)]
                              #:when (= i (modulo j N)))
                     e))))))

(slide #:title "Model of Package System"
       'next
       (item "The core is the same as the leaves.")
       'next
       (item "Social processes are as valuable, if not more valuable, than technical frameworks.")
       'next
       (item "Compatibility is very valuable and rarely broken (2htdp/image, #lang mzscheme, etc)")
       'next
       (item "Incompatibility is removing features and disobeying documentation."))

(slide #:title "Model of a Package"
       'next
       (item "A package is a set of modules. From any collection.")
       'next
       (item "Version numbers go up to indicate new features.")
       'next
       (item "Dependencies are vanilla, versioned, and platformed.")
       (item "Dependency violations are warnings, not errors."))

(slide #:title "A Good Package..."
       (item "uses a neutral name.")
       (item "is on Github*.")
       (item "has an explicit name or is 'multi.")
       (item "is listed officially and does not conflict with anything."))

(slide #:title "A Better Package..."
       (item "starts at version 0.0 and switches to 1.0 on stability.")
       (subitem "If interface is same, no version change.")
       (subitem "If interface is grown, version increases.")
       (subitem "If interface shrinks, a fork occurs.")
       'next
       (item "updates the catalog when depencies on Racket versions are added.")
       'next
       (item "has informative tags and description in catalog."))

(slide #:title "A Best Package..."
       (item "includes documentation and tests.")
       (item "is a 'multi package.")
       (item "specifies a license and uses a problem tracker.")
       (item "has a responsive author."))

(slide #:title "A not so good package..."
       (item "exposes the internal development of the package.")
       (item "is not for public consumption.")
       'next
       (item "You can communicate with different groups by sharing sources and making your own catalog."))

(slide #:title "Model of a System"
       (item "The same computer may contain many users of Racket and many installations of Racket.")
       (item "Packages can be arbitrarily shared or not shared between any combination of these through installation names and scopes."))

(slide #:title "Open Problems"
       (item "There's work to do, but we know how to do it, it just takes time.")
       'next
       (item "Documentation will be a challenge.")
       'next
       (subitem "What is a structure property?")
       'next
       (subitem "What functions work on lists?")
       'next
       (subitem "How can the Tutorial, Guide, Exegesis, Reference, data/file/net, etc documentation be extensible?")
       'next
       (item "How can we truly support multiple simultaneous versions for all aspects of the system, core and otherwise?"))
