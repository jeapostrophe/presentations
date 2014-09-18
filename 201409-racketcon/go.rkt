#lang at-exp racket/base
(require racket/list
         racket/string
         racket/format
         racket/runtime-path
         racket/gui/base
         pict
         pict/code
         pict/flash
         puresuri
         puresuri/plpict
         unstable/gui/pict
         puresuri/lib/title
         puresuri/lib/grid
         puresuri/lib/slide-numbers
         puresuri/lib/cmds)
(module+ test)

(define-runtime-path assets "assets")

(define fbi-yellow (list 255 209 76))
(define fbi-background (list 115 140 156))

;; xxx pop/restore placer?
(define (background! color)
  (go! (relative-placer 1/2 1/2 'cc))
  (add! (colorize (filled-rectangle slide-w slide-h) color)))

(background! fbi-background)
(go! (relative-placer 1/2 3/8 'cc))
(add! (scale (bitmap (build-path assets "US-FBI-Seal.svg.png")) 0.5))
(go! (relative-placer 1/2 6/8 'cc))

(add! (colorize (text "“Winners Don’t Use Drugs”" null 60) fbi-yellow))
(add! (colorize (text "William S. Sessions, Director, FBI" null 30) fbi-yellow))
(go! (relative-placer 1/2 0.95 'cc))

(define (blinking t)
  (λ ()
    (if (zero? (modulo (current-seconds) 2))
      t
      (cellophane t 0))))

(add! (blinking (colorize (text "INSERT COIN TO PLAY" 'modern 30) "white")))

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

;; Abstract: Hard real-time embedded systems with tight operating
;; environments, a.k.a. console video games, are an exciting and
;; challenging place to program functionally. The Get Bonus project is
;; an effort to experiment in this space with Racket. This
;; progress-report presentation will discuss some of our goals and
;; some of the interesting implementations we've made in
;; Racket.

(define summary-start (save!))
(commit!)

(restore! summary-start)
(go! (relative-placer 0.5 0.35 'rc))
(add! (text "Level 1" 'modern 80))
(define (aka l r)
  (add! (hb-append (text l null 60) (text r null 30))))
(go! (relative-placer 0.25 0.45 'lc))
(aka "GBPPU" " a.k.a. Graphics")
(go! (relative-placer 0.5 0.55 'rc))
(add! (text "Level 2" 'modern 80))
(go! (relative-placer 0.25 0.65 'lc))
(aka "Multiverse" " a.k.a Architecture")
(go! (relative-placer 0.5 0.75 'rc))
(add! (text "Level 3" 'modern 80))
(go! (relative-placer 0.25 0.85 'lc))
(aka "Enumerator" " a.k.a. Design")
(slide!)

(background! "pink")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "The Get Bonus Picture-Processing Unit" null 50))
(define gbppu (save!))

(commit!) (restore! gbppu)

(go! (relative-placer 1/2 1/2 'cc))
(add! (scale (bitmap (build-path assets "SNES.png")) 0.9))
(commit!) (restore! gbppu)

(go! (relative-placer 1/2 1/2 'cc))
(add! (text "60 FPS" null 200))
(commit!) (restore! gbppu)

(go! (relative-placer 1/2 1/2 'cc))
(add! (text "16.(6) ms" null 200))
(commit!) (restore! gbppu)

(define (reso-box w h l)
  (rb-superimpose (scale (rectangle w h) 2)
                  (vr-append  (text l null 30)
                              (text (format "~ax~a" w h) null 30))))

(go! (relative-placer 1/2 1/2 'cc))
(add! (reso-box 256 239 "SNES"))
(commit!)
(go! (relative-placer 1/2 1/2 'cc))
(add! (reso-box (* 16 27) (* 9 27) "16:9 SNES"))
(commit!)
(go! (relative-placer 1/2 1/2 'cc))
(add! (scale (rectangle (* 16 28) (* 9 28)) 2))
(go! (relative-placer 1/2 0.9 'cc))
(add! (text "16:9 at 28 scale" null 60))
(commit!) (restore! gbppu)

(define mm-bm (scale-to-fit (bitmap (build-path assets "mm.png")) 32 32))
(define mm-hm-bm (scale-to-fit (bitmap (build-path assets "mm-hm.png")) 32 32))
(define zelda-bm (scale-to-fit (bitmap (build-path assets "zelda.png")) 32 32))

(define (tablify p w h)
  (for/fold ([s (blank)])
      ([x (in-range w)])
    (hc-append s
               (for/fold ([s (blank)])
                   ([y (in-range h)])
                 (vc-append s p)))))

(go! (relative-placer 1/2 1/2 'cc))
(add! #:tag 'grid
      (scale (tablify (rectangle 16 9) 28 28)
             2))

;; Placing sprites
(go! (relative-placer 0.5 0.5 'cc))
(add! #:tag 'placing mm-bm)
(commit!)
;; Shifting (dx)
(remove! 'placing)
(go! (relative-placer 0.75 0.5 'cc))
(add! #:tag 'shifting-dx mm-bm)
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx" null 60))
(commit!)
;; Shifting (dy)
(remove! 'shifting-dx)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-dy mm-bm)
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy" null 60))
(commit!)
;; Scaling (mx)
(remove! 'shifting-dy)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-mx (scale mm-bm 8 1))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx" null 60))
(commit!)
;; Scaling (my)
(remove! 'shifting-mx)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-my (scale mm-bm 8 8))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx my" null 60))
(commit!)
;; Rotate (theta)
(remove! 'shifting-my)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-theta (rotate (scale mm-bm 8 8) (* 1/4 3.14)))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx my θ" null 60))
(commit!)
;; palette
(remove! 'shifting-theta)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-pal (rotate (scale mm-hm-bm 8 8) (* 1/4 3.14)))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx my θ pal" null 60))
(commit!)
;; tint
(remove! 'shifting-pal)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-rgba (cellophane (rotate (scale mm-hm-bm 8 8) (* 1/4 3.14)) 0.5))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx my θ pal rgba" null 60))
(commit!)
;; sprite
(remove! 'shifting-rgba)
(go! (relative-placer 0.75 0.3 'cc))
(add! #:tag 'shifting-spr (cellophane (rotate (scale zelda-bm 8 8) (* 1/4 3.14)) 0.5))
(go! (relative-placer 0 0.9 'lc))
(add! (text "dx dy mx my θ pal rgba spr" null 60))
(commit!)

;; GPU code
(remove! 'shifting-spr)
(remove! 'grid)

(go! (relative-placer 0.5 0.5 'cc))
(define vert-p (scale zelda-bm 8 8))
(add! #:tag 'verts vert-p)
(commit!)

(define (snoc l x)
  (append l (list x)))

(define (pin*-arrow-line atop . corners)
  (for/fold ([p atop])
      ([start (in-list corners)]
       [end (in-list (snoc (rest corners) (first corners)))])
    (pin-arrow-line 15 p
                    #:line-width 5
                    atop start
                    atop end)))

(define (tri-atop vert-p 1st 2nd 3rd)
  (pin*-arrow-line vert-p 1st 2nd 3rd))

(replace! 'verts (pin*-arrow-line vert-p lt-find rt-find rb-find lb-find))
(go! (relative-placer 0 0.9 'lc))
(add! #:tag 'x4 (text "dx dy mx my θ pal rgba spr v h x4" null 60))
(commit!)
(replace! 'verts (tri-atop vert-p lt-find rt-find lb-find))
(go! (relative-placer 0 0.9 'lc))
(remove! 'x4)
(add! #:tag 'x3 (text "dx dy mx my θ pal rgba spr v h x3" null 60))
(commit!)
(replace! 'verts (tri-atop vert-p lb-find rb-find rt-find))
(go! (relative-placer 0 0.9 'lc))
(remove! 'x3)
(add! (text "dx dy mx my θ pal rgba spr v h x6" null 60))
(commit!) (restore! gbppu)

(define (verbatim . ss)
  (define s (string-append* ss))
  (define tt (current-code-tt))
  (for/fold ([p (blank)])
      ([s (in-list (string-split s "\n"))])
    (vl-append p (tt s))))

(define (add-arrow! start start-side end end-side
                    #:arrow-width [arrow-width 10])
  (bind!
   (λ (p)
     (pin-arrow-line arrow-width p
                     (find-tag p start) start-side
                     (find-tag p end) end-side))))

(parameterize ([get-current-code-font-size (λ () 20)])
(define vert-shader
    @verbatim{
vec4 sprd = 
 texelFetch(Sprites, ivec2(0, spr), 0);
Color = rgba / 255.0;
gl_Position = 
 vec4(h * hw * mx,
      v * hh * my,
      0.0, 1.0)
 * glRotate(θ, 0.0, 0.0, 1.0) 
 * glTranslate(dx, dy, 0.0)
 * glOrtho(0.0, 16*28, 0.0, 9*28, 1.0, -1.0);
TexCoord = 
 vec2(sprd.x + ((h + 1.0)/+2.0) * sprd.z,
      sprd.y + ((v - 1.0)/-2.0) * sprd.w);
Palette = pal; 
})
(define frag-shader
    @verbatim{
ivec2 TexCoord_uv =
 ivec2(clampit(TexCoord.x), clampit(TexCoord.y));
vec4 SpriteColor = 
 texelFetch(SpriteAtlasTex, TexCoord_uv, 0);
float PaletteOffset = SpriteColor.r * 256;
ivec2 PalCoord_uv = ivec2( PaletteOffset, Palette );
vec4 PaletteColor =
 texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );
out_Color = Color + PaletteColor;
if ( out_Color.a == 0.0 )
  discard;
})

(go! (relative-placer 0 0.1 'lt))
(add! #:tag 'cstruct
 (code
  (define-cstruct _sprite-info
    ([ dx  _float] [ dy  _float]
     [ hw  _float] [ hh  _float]
     [  r  _uint8] [  g  _uint8]
     [  b  _uint8] [  a  _uint8]
     [ mx  _float] [ my  _float]
     [  θ  _float]
     [pal _uint16] [spr _uint16]
     [  h  _sint8] [  v  _sint8]))))
(commit!)
(go! (relative-placer 0.45 0.1 'lt))
(add! #:tag 'make-buffer
 (code
  (make-cvector*
   (glMapBufferRange
    GL_ARRAY_BUFFER
    0
    (* HowManySprites
       VertsPerSprite
       (ctype-sizeof _sprite-info))
    (bitwise-ior
     GL_MAP_INVALIDATE_RANGE_BIT
     GL_MAP_INVALIDATE_BUFFER_BIT
     GL_MAP_WRITE_BIT))
   _sprite-info
   (* HowManySprites
      VertsPerSprite))))
(add-arrow! 'cstruct rc-find 'make-buffer lc-find)
(commit!)
(go! (relative-placer 0 0.45 'lt))
(add! #:tag 'gl-draw
 (code
  (glDrawArrays
   GL_TRIANGLES 0
   (* HowManySprites
      VertsPerSprite))))
(add-arrow! 'make-buffer lb-find 'gl-draw rc-find)
(commit!)
(go! (relative-placer 0 0.6 'lt))
(add! #:tag 'vert-shader
 (scale-to-fit vert-shader (* slide-w .55) (* slide-h .4)))
(add-arrow! 'gl-draw cb-find 'vert-shader ct-find)
(commit!)
(go! (relative-placer 0.55 0.59 'lt))
(add! #:tag 'frag-shader
 (scale-to-fit frag-shader (* slide-w .44) (* slide-h .5))))
(add-arrow! 'vert-shader rc-find 'frag-shader lc-find)
(commit!) (restore! gbppu)

(parameterize ([get-current-code-font-size (λ () 25)])
  (go! (relative-placer 0.5 0.15 'ct))
  (add! 
   (code (code:comment "Sprites      : (~1 kb)")
         (code:comment "   Index")
         (code:comment " ->")
         (code:comment "   Atlas Location")
         (code:comment " x Width")
         (code:comment " x Height")
         (code:comment "")
         (code:comment "SpriteAtlas  : (~10kb)")
         (code:comment "   X")
         (code:comment " x Y")
         (code:comment " ->")
         (code:comment "   Color ( 4-bit)")
         (code:comment "")
         (code:comment "PaletteAtlas : (~200b)")
         (code:comment "   Index")
         (code:comment " x Color ( 4-bit)")
         (code:comment " ->")
         (code:comment "   Color (32-bit)"))))
(commit!) (restore! gbppu)

(go! (relative-placer 0.05 0.15 'lt))
(add! (text "One vertex is 40b" null 60))
(add! (text "One sprite is 6 vertices" null 60))
(add! (text "One sprite is 240b" null 60))
(add! (text "One sprite @ 60 FPS is ~14kb/s" null 60))
(add! (text "Intel HD Graphics 4000 supports" null 60))
(add! (text " 1,908,874 sprites at 60 FPS" null 60))
(add! (text "OR" null 60))
(add! (text " 16 sprites per pixel" null 60))

(slide!)

;; First time: 11m

(background! "lime")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "Multiverse" null 50))
(define multiverse (save!))

(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 40)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (struct world (....))
    (big-bang (world ....)
              (on-tick world->world)
              (on-draw draw-world)
              (on-key world*key->world)))))
(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 40)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (struct world (player enemy1 enemy2))
    (define (world->world w)
      (match-define (world p e1 e2) w)
      (struct-copy 
       world w
       [player (player->player p)]
       [enemy1 (enemy->enemy e1)]
       [enemy2 (enemy->enemy e2)])))))
(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 40)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (struct world:paused (....))
    (struct world:playing (....))    
    (define (world*key->world w k)
      (match w
        [(world:paused ....)
         (if (start-button? k)
           ....
           ....)]
        [(world:playing ....)
         (if (start-button? k)
           ....
           ....)])))))
(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 40)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (provide/contract
     [big-bang/os
      (-> (-> any) any)]
     [os/thread
      (-> (-> any) any)]     
     [os/write
      (code:comment "blocks until next frame")
      (->* (hash/c symbol? any/c) any)]
     [os/read
      (-> symbol? set?)]
     [os/exit
      (-> any)]))))
(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 40)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (os/thread player)
    (os/thread enemy)
    (os/thread enemy)
    (let loop ()      
      ....
      (os/read 'controller)
      (os/read 'bullet)
      ....
      (os/write   'gfx MEGA-MAN
                'sound BEEP-BOOP)
      (loop)))))
(commit!) (restore! multiverse)

(parameterize ([get-current-code-font-size (λ () 30)])
  (go! (relative-placer 1/2 1/2 'cc))
  (add!
   (code
    (let loop ([lhs-y (/ height 2.0)])
      (define lhs-dy
        (controller-ldpad-y (os/read* 'controller)))
      (define lhs-y-n
        (clamp min-paddle-y
               (+ lhs-y (* lhs-dy speed))
               max-paddle-y))
      (os/write
       (list
        (cons 'lhs-y lhs-y-n)
        (cons 'graphics 
              (transform #:d
                         (+ lhs-x paddle-hw)
                         (- lhs-y-n paddle-hh)
                         (lhs-paddle)))))
      (loop lhs-y-n)))))
(commit!) (restore! multiverse)

(go! (relative-placer 1/2 1/2 'cc))
(add! (vc-append
       (text "os/write blocking" null 60)
       (text "os/read returns set" null 60)))
(commit!) (restore! multiverse)

(go! (relative-placer 1/2 1/2 'cc))
(add! (vc-append
       (text "Interface" null 60)
       (text "" null 60)
       (text "conforms to" null 60)
       (text "" null 60)
       (text "Map-Reduce" null 60)
       (text "and" null 60)
       (text "Entity-Component-System" null 60)))
(commit!) (restore! multiverse)

(add! (text "(Sequential) Implementation" null 50))
(define m:seq-impl (save!))
(parameterize ([get-current-code-font-size (λ () 30)])
  (go! (relative-placer 0 0.2 'lt))
  (add! (code
         (define 0x80 (make-continuation-prompt-tag 'kernel))
         (define (run-process-until-syscall p)
           (call-with-continuation-barrier
            (λ ()
              (call-with-continuation-prompt
               (λ () (os/exit (p)))
               0x80
               (λ (x) x)))))
         (define (trap-syscall k->syscall)
           (call-with-current-continuation
            (λ (k)
              (abort-current-continuation 0x80 (k->syscall k)))
            0x80))))
  (commit!) (restore! m:seq-impl))
(parameterize ([get-current-code-font-size (λ () 30)])
  (go! (relative-placer 0 0.2 'lt))
  (add! (code
         (struct process (pid k) #:transparent)         
         (struct os (cur-heap next-heap cur-procs next-procs))
         (define boot
           (match-lambda
            [(os cur-h next-h
                 (list) next-ps)
             (os next-h (make-hasheq) next-ps (list))]
            [(os cur-h next-h 
                 (list* (process pid now) cur-ps) next-ps)
             (define syscall (run-process-until-syscall now))
             (boot (syscall pid
                            (os cur-h next-h cur-ps next-ps)))]))))
  (commit!) (restore! m:seq-impl))
(parameterize ([get-current-code-font-size (λ () 19)])
  (go! (relative-placer 0 0.2 'lt))
  (add! (code
         (define-syscalls (pid current)
           [(os/exit k v)
            current]
           [(os/read k id)
            (match-define (os cur-h next-h cur-ps next-ps) current)
            (os cur-h next-h
                (snoc* cur-ps
                       (process pid (λ () (k (hash-ref cur-h id empty)))))
                next-ps)]
           [(os/write k id*vals)
            (match-define (os cur-h next-h cur-ps next-ps) current)
            (for ([id*val (in-list id*vals)])
              (match-define (cons id val) id*val)
              (hash-update! next-h id (curry cons val) (λ () empty)))
            (os cur-h next-h cur-ps
                (snoc* next-ps
                       (process pid (λ () (k (void))))))]
           [(os/thread k t)
            (match-define (os cur-h next-h cur-ps next-ps) current)
            (define t-pid (gensym 'pid))
            (os cur-h next-h
                (snoc* cur-ps
                       (process pid (λ () (k t-pid)))
                       (process t-pid t))
                next-ps)]))))

(slide!)

(background! "lightgray")
(go! (relative-placer 0.01 0.05 'lc))
(add! (text "Enumerator" null 60))
(define enumerator (save!))

(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.5 'cb))
(add! #:tag 'game (text "Game : Skill -> Score" null 60))
(add! #:tag 'score (blank))
(commit!)
(go! (at-placer 'game cc-find 'cc))
(add! #:tag 'determ
      (cc-superimpose 
       (scale (colorize (filled-flash 100 50) "orange") 3)
       (text "deterministic!" null 30)))
(commit!)
(remove! 'determ)
(replace! 'score (text "Score = Bool" null 60))
(commit!)
(replace! 'score (text "Score = [0,1]" null 60))
(commit!)
(replace! 'score (text "Score = Real" null 60))
(commit!)
(replace! 'score (text "Score = Score x Score" null 60))
(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.5 'cc))
(add! (bitmap (build-path assets "pacman.png")))
(commit!)
(go! (relative-placer 0.5 0.5 'cc))
(add! (bitmap (build-path assets "canabalt.jpg")))
(commit!)
(go! (relative-placer 0.5 0.5 'cc))
(add! (bitmap (build-path assets "asteroids.jpg")))
(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.5 'cb))
(add! (text "Cart : Level -> Game" null 60))
(commit!)
(add! #:tag 'biject (text "b = bijection(Level, Nat)" null 60))
(commit!)
(go! (at-placer 'biject cc-find 'cc))
(add! (cc-superimpose 
       (scale (colorize (filled-flash 120 50) "orange") 3)
       (text "not \"procedural\"" null 30)))
(commit!) (restore! enumerator)

(define (ml-bm p)
  (scale-to-fit (bitmap (build-path assets p))
                (* slide-w 0.75)
                (* slide-h 0.3)))

(define (ml ml-i p)
  (define l (text (~a #:align 'right #:min-width 3 ml-i) 'modern 60))
  (define m (ml-bm p))
  (pin-arrow-line 10
                  (hc-append l
                             (blank (* slide-w 0.1) (pict-height l))
                             m)
                  l rc-find
                  m lc-find))

(go! (relative-placer 0.02 0.15 'lt))
(add! (ml 0 "mario-1-1.gif"))
(add! (ml 7 "mario-1-2.gif"))
(add! (ml 24 "mario-1-3.gif"))
(add! (ml 32 "mario-1-4.gif"))
(add! (ml 67 "mario-2-1.gif"))
(add! (ml 81 "mario-2-2.gif"))
(add! (ml 90 "mario-2-3.gif"))
(add! (ml 101 "mario-7-2.gif"))
(add! (ml 102 "mario-4-3.gif"))
(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.5 'cb))
(add! (text "Cart.Gen : Params -> Level" null 60))
(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.25 'ct))
(add! #:tag 'cg.call (text "Cart.Gen(Underwater, Easy)" null 60))
(commit!)
(add! (blank 0 (* slide-h 0.1)))
(add! #:tag 'cg.output (text "81" null 60))
(add-arrow! 'cg.call cb-find 'cg.output ct-find)
(commit!)
(add! (blank 0 (* slide-h 0.1)))
(add! #:tag 'cg.level (inset (ml-bm "mario-2-2.gif") 0 5 0 0))
(add-arrow! 'cg.output cb-find 'cg.level  ct-find)
(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.15 'ct))
(add! (text "Engine : Variant -> Cart" null 60))

(define (ffv p)
  (scale-to-fit (bitmap (build-path assets p))
                (* slide-w 0.7)
                (* slide-h 0.7)))

(go! (relative-placer 0.5 0.6 'cc))
(commit!) (add! #:tag 'ff-variant (ffv "FF1.png"))
(commit!) (replace! 'ff-variant (ffv "FF1-psp.jpg"))
(commit!) (replace! 'ff-variant (ffv "FF7.jpg"))
(commit!) (replace! 'ff-variant (ffv "FF13.jpg"))

(commit!) (restore! enumerator)

(go! (relative-placer 0.5 0.15 'ct))
(add! (text "Engine : Variant -> Cart" null 60))
(commit!)
(add! (text "Cart.Gen : Params -> Level" null 60))
(commit!)
(add! (text "Cart : Level -> Game" null 60))
(commit!)
(add! (text "Game : Skill -> Score" null 60))

(slide!)

(restore! summary-start)
(go! (relative-placer 0.0 0.35 'lc))
(add! (text "Level 1" 'modern 80))
(go! (relative-placer 0.025 0.45 'lc))
(aka "GBPPU" " a.k.a. unsafe C interop, OpenGL, and GPU shaders")
(go! (relative-placer 0.0 0.55 'lc))
(add! (text "Level 2" 'modern 80))
(go! (relative-placer 0.025 0.65 'lc))
(aka "Multiverse" " a.k.a delim cont-based OS and map-reduce")
(go! (relative-placer 0.0 0.75 'lc))
(add! (text "Level 3" 'modern 80))
(go! (relative-placer 0.025 0.85 'lc))
(aka "Enumerator" " a.k.a. variants, configs, and Godel bijections")

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
