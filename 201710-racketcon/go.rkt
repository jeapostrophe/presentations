#lang at-exp racket/base
(require "title.rkt"
         racket/fixnum
         racket/file
         racket/class
         racket/gui/base
         racket/runtime-path
         images/icons/style
         images/icons/control
         pict/color
         (except-in slideshow slide)
         (prefix-in s: slideshow)
         slideshow/code
         slideshow/text)

(define XXX start-at-recent-slide)

(define C #f)
(define (jcomment oargs)
  (define args
    (filter-not (λ (s) (regexp-match #rx"^ +$" s)) oargs))
  (define-values (p _ l)
    (for/fold ([p (blank)]
               [was-nl? #f]
               [l empty])
              ([a (in-list (append args '("\n" "\n")))])
      (match* (was-nl? (equal? a "\n"))
        [( _ #f) (values p #f (cons a l))]
        [(#f #t) (values p #t l)]
        [(#t #t) (values (vl-append p (apply para (reverse l)) (para " ")) #f empty)])))
  (unless (empty? l) (error 'jcomment "Whoops: ~v" args))
  p)
(define (c . args) (set! C (comment (jcomment args))))
(define (slide . args)
  (apply s:slide (or C 'nothing) args))

(define-runtime-path r "r")
(define megas
  (for/vector ([i (in-range 1 4)])
    (bitmap (build-path r (format "mega~a.png" i)))))
(define client-eh (- client-h (* 2 (pict-height (vector-ref megas 0)))))

;; xxx add line numbers to "code"

(define (introduce topic-l name affiliation)
  (slide
   (cc-superimpose
    plt-title-background
    (vr-append
     (vc-append
      (scale/improve-new-text (t (first topic-l)) 2)
      (scale/improve-new-text (t (second topic-l)) 3)
      (scale/improve-new-text (t (third topic-l)) 1.5))
     (vr-append
      (scale/improve-new-text (t name) 1)
      (scale/improve-new-text (t (string-append affiliation " & PLT")) 0.5))))))

(define-syntax-rule (cslide e)
  (slide
   (lt-superimpose (scale-to-fit e client-w client-eh)
                   (blank client-w client-eh))))

(define bitmaps (make-hash))
(define (bitslide i
                  #:focus [focus #f]
                  #:width [w client-w]
                  #:height [h client-eh])
  (define bm (hash-ref! bitmaps i (λ () (bitmap (build-path r i)))))
  (define orig
    (match focus
      [#f bm]
      [(list x y w h)
       (pin-over bm x y (red (rectangle w h #:border-width 4)))]))
  (slide (scale-to-fit orig w h)))


(define (file->lines-pict f)
  (apply vl-append (map tt (file->lines (build-path r f)))))

(define (fileslide f #:scale [sc #f])
  (define lp (file->lines-pict f))
  (slide
   (if sc
     (scale sc lp)
     (scale-to-fit lp client-w client-eh))))

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
                                      "<-" "<~" "=>"
                                      "for/fold"
                                      "in-range"
                                      "define-values"
                                      "bytes-set!"
                                      "floor"
                                      "grammar"
                                      "-" "+" "-" "<" "*" "/"
                                      "make-bytes"
                                      "vector-ref" "vector"
                                      "->")
                                (current-keyword-list)))


  (define progress -1)
  ;; xxx
  (define total-slide-count 22)
  (define default-slide-assembler (current-slide-assembler))
  (current-slide-assembler
   (λ (title title-sep content-pict)
     (set! progress (add1 progress))

     (define which-m (modulo progress (vector-length megas)))
     (define the-m (vector-ref megas which-m))
     (define the-m-w (pict-width the-m))
     (define bar-w (- (* client-w (/ progress total-slide-count))
                      the-m-w))

     (lb-superimpose
      (cc-superimpose
       (blank client-w client-eh)
       (default-slide-assembler
         title title-sep content-pict))
      (if (positive? bar-w)
        (hc-append
         (filled-rectangle #:color "Thistle"
                           bar-w
                           5)
         the-m)
        (blank)))))

  (introduce
   (list "Teaching and Designing"
         "Microarchitecture"
         "in Racket")
   "Jay McCarthy"
   "UMass Lowell")

  @c{I teach a class that focuses on the low-level design of CPUs,
     which we try to understand completely. Given the complexities of
     modern CPUs, we start off the semester with a really simple CPU
     defined in our text book: the MIC-1

     It is defined in 11 pages in Section 4.2 of Structured Computer
     Organization by Tanenbaum

     When I first taught the course, I inherited a suite of simulator
tools in C that was about 4000 or 5000 lines of code. It was very
confusing and hard to change. One of the things I want students to do
is make modifications to the machine.}
  (bitslide "diagram.png")

  @c{It has 16 16-bit registers}
  (bitslide "diagram.png" #:focus (list 90 145 50 70))
  @c{and is attached with to a 12-bit memory}
  (bitslide "diagram.png" #:focus (list 5 255 70 60))
  @c{Typically, we program it in assembler.}
  (bitslide "mac1.png")
  @c{[WALKTHROUGH instruction]}
  (bitslide "mac1.png" #:focus (list 5 55 370 25))

  @c{Here's an example program that computes all of the Fibonacci
  numbers that fit inside 16-bits:}
  (slide
   (scale-to-fit
    (vc-append
     (file->lines-pict "fib.s")
     ;; This should compile to the following assembly:
     (code =>)
     (file->lines-pict "fib.o"))
    client-w client-eh))

  @c{You might think that an assembler is hard to write, but my Racket
  version (including the test suite) is only 256 lines (vs 1000). Here is 90% of it:

  I hope that it looks familiar, because it is basically the same
  as the specification.
  
  The only thing not shown is the lexing, alignment, and computing
  of offsets for references, but they are really not that
  complicated.
}
  (cslide
   (code
    (grammar
     (Program [() empty]
              [(Inst Program) (cons $1 $2)])
     (Arg [(NUM) $1] [(LABEL) (as-lref $1)])
     (Inst [(LABEL   ) (as-ldef $1)]
           [(     NUM) (as-lnum $1)]
           [(     STR) (as-lstr $1)]
           [( LOC NUM) (as-rloc $2)]
           [(LODD Arg) (as-iarg "0000" $2)]
           [(STOD Arg) (as-iarg "0001" $2)]
           [(ADDD Arg) (as-iarg "0010" $2)]
           [(SUBD Arg) (as-iarg "0011" $2)]
           [(JPOS Arg) (as-iarg "0100" $2)]
           [(JZER Arg) (as-iarg "0101" $2)]
           [(JUMP Arg) (as-iarg "0110" $2)]
           [(LOCO Arg) (as-iarg "0111" $2)]
           [(LODL Arg) (as-iarg "1000" $2)]
           [(STOL Arg) (as-iarg "1001" $2)]
           [(ADDL Arg) (as-iarg "1010" $2)]
           [(SUBL Arg) (as-iarg "1011" $2)]
           [(JNEG Arg) (as-iarg "1100" $2)]
           [(JNZE Arg) (as-iarg "1101" $2)]
           [(CALL Arg) (as-iarg "1110" $2)]
           [(PSHI    ) (as-inst "1111000000000000")]
           [(POPI    ) (as-inst "1111001000000000")]
           [(PUSH    ) (as-inst "1111010000000000")]
           [( POP    ) (as-inst "1111011000000000")]
           [(RETN    ) (as-inst "1111100000000000")]
           [(SWAP    ) (as-inst "1111101000000000")]
           [(INSP Arg) (as-iarg "11111100" $2)]
           [(DESP Arg) (as-iarg "11111110" $2)]
           [(HALT    ) (as-inst "1111111100000000")]))))

  @c{However, this is not just a class about assembly, instead we want
  to understand how the machine really works. Well, those
  instructions that we just saw are not actually what the machine
  interprets. Instead it is microcode, like a modern machine. This
  means that there is a very small control store that contains one
  program---an interpreter for the assembly. It's here in the block
  diagram.}
  (bitslide "diagram.png" #:focus (list 275 245 160 25))
  @c{It is exactly 256 instructions long, but each instruction, rather
  than being 16-bits (like the register size), instead is a 32-bit
  control string. We can see that here and each bit goes to a
  different part of the CPU, driving its execution.

  Let's zoom in a bit on that
  }
  (bitslide "diagram.png" #:focus (list 275 275 162 50))

  @c{There's one block for each functional unit and we can control
each one inpendently. This is like a VLIW machine, except rather than
have, for example, multiple adders, we just have one. However, we do
have multiple options per cycle: we can add, store, access memory, and
jump all in a single step.}
  (bitslide "microcode.png")

  @c{Here's an example program that computes Fibonaccis as before,
although this does it directly in the microcode. It is a lot
faster. Obviously, it is nearly impossible to understand.}
  (fileslide "fib.prom")

  @c{So, we make a programming language. Here's the same program in
         the microcode language.

         My compiler (and tests!) are 308 lines, compared to 2000 in
C. While writing my test suite, I discovered errors in the C version
as well. And I added features, like named labels rather than just
offsets.}
  (fileslide "fib.mc")

  @c{The key idea of the compiler is to realize that each piece of
instruction (called an op) controls one of the many bits in the
MIR. There's a small set of combinations and the main error-checking
just makes sure that the same bit is not given different values.}
  (cslide
   (code
    (define (μcompile-reg L reg which)
      (hash-set1 L reg
                 (match which
                   ["pc" 'PC] ["ac" 'AC] ["sp" 'SP] ["ir" 'IR] ["tir" 'TIR]
                   ["0" 'Z] ["1" 'P1] ["(-1)" 'N1] ["amask" 'AMASK] ["smask" 'SMASK]
                   ["a" 'A] ["b" 'B] ["c" 'C] ["d" 'D] ["e" 'E] ["f" 'F])))
    (define (μcompile-b-bus L b-bus)
      (μcompile-reg L 'B b-bus))
    (define (μcompile-c-bus L c-bus)
      (μcompile-reg L 'C c-bus))
    (define (μcompile-a-bus L a-bus)
      (match a-bus
        ['mbr
         (hash-set1 L 'AMUX 'MBR)]
        [_
         (μcompile-reg L 'A a-bus)]))
    (define (μcompile-alu L alu)
      (match alu
        [(alu-plus a-bus b-bus)
         (hash-set1 (μcompile-b-bus (μcompile-a-bus L a-bus) b-bus) 'ALU '+)]
        [(alu-id a-bus)
         (hash-set1 (μcompile-a-bus L a-bus) 'ALU 'A)]
        [(alu-band a-bus b-bus)
         (hash-set1 (μcompile-b-bus (μcompile-a-bus L a-bus) b-bus) 'ALU '&)]
        [(alu-inv a-bus)
         (hash-set1 (μcompile-a-bus L a-bus) 'ALU '!)]))
    (define (μcompile-sh L sh)
      (match-define (sh-op alu) sh)
      (hash-set1
       (μcompile-alu L alu) 'SH
       (match sh
         [(sh-id _) 'NS]
         [(sh-lshift _) 'LS]
         [(sh-rshift _) 'RS])))
    (define (μcompile-comp label->idx L comp)
      (match comp
        [(mc-setc c-bus sh)
         (hash-set1 (μcompile-c-bus (μcompile-sh L sh) c-bus) 'ENC 'ENC)]
        [(mc-alu alu)
         (μcompile-alu L alu)]
        [(mc-mar b-bus)
         (hash-set1 (μcompile-b-bus L b-bus) 'MAR 'MAR)]
        [(mc-mbr sh)
         (hash-set1 (μcompile-sh L sh) 'MBR 'MBR)]
        [(mc-if cond label)
         (hash-set1
          (hash-set1 L 'COND cond)
          'ADDR
          (hash-ref label->idx label
                    (λ ()
                      (error 'μcompile "L~a: Unknown label: ~v"
                             (μcompile-line) label))))]
        [(mc-rd) (hash-set1 L 'RD 'RD)]
        [(mc-wr) (hash-set1 L 'WR 'WR)]))))

  @c{But, we also want to understand how the actual machine is
implemented in the underlying hardware. So, I've implemented a HDL for
arbitrary circuits. Let's take a look at the ALU.}
  (bitslide "diagram.png" #:focus (list 125 345 95 40))

  @c{Here's the idea: Each block is a network of wires and other
blocks. Generally, block list their inputs first and their outputs at
the end. If a block needs internal wiring, then it declares how many
wires and what names they have.}
  (cslide
   (code
    (code:contract ALU : N N 2 -> N 1 1)
    (define (ALU A B Function-Select #,(tt "#;=>") Out Negative? Zero?)
      (define N (length A))
      (Net ([TheSum N] [TheAnd N] [NotA N] [Function-Selects 4])
           (Adder/N A B FALSE #,(tt "#;=>") GROUND TheSum)
           (And/N A B #,(tt "#;=>") TheAnd)
           (Not/N A #,(tt "#;=>") NotA)
           (Decoder/N Function-Select #,(tt "#;=>") Function-Selects)
           (RegisterRead (list TheSum TheAnd A NotA) Function-Selects
                         #,(tt "#;=>") Out)
           (IsZero? Out #,(tt "#;=>") Zero?)
           (IsNegative? Out #,(tt "#;=>") Negative?)))))

  @c{I've built an exhaustive testing system as well. This unit is
checked for all 5-bit inputs.}
  (cslide
   (code
    (define-chk-num chk-alu
      #:N N
      #:in ([A N] [B N] [Function-Select 2])
      #:out ([Out N] Negative? Zero?)
      #:circuit ALU #:exhaust 5
      #:check
      (chk (vector Out Negative? Zero?)
           (let* ([Ans-premod
                   (match Function-Select
                     [0 (+ A B)]
                     [1 (bitwise-and A B)]
                     [2 A]
                     [3 (bitwise-not A)])]
                  [Ans
                   (modulo Ans-premod (expt 2 N))])
             (vector Ans
                     (negative? (unsigned->signed N Ans))
                     (zero? Ans)))))))

  @c{The entire MIC-1 is just 60 lines of blocks, making 180 for the
whole file:}
  (cslide
   (code
    (define (MIC1 μCodeLength Microcode
                  Registers MPC-out
                  Read? Write?
                  MAR MBR)
      (define μAddrSpace (ROM-AddrSpace Microcode))
      (define WordBits (length MBR))
      (define RegisterCount (length Registers))
      (define RegisterBits (integer-length (sub1 RegisterCount)))

      (define MIR:RD Read?)
      (define MIR:WR Write?)
      (define-wires
        Clock:1 Clock:2 Clock:3 Clock:4
        N Z MicroSeqLogic-out
        [pre-MIR μCodeLength] [MIR μCodeLength]
        MIR:AMUX [MIR:COND 2] [MIR:ALU 2] [MIR:SH 2]
        MIR:MBR MBR? MIR:MAR MAR?
        MIR:ENC
        [MIR:C RegisterBits] [MIR:B RegisterBits] [MIR:A RegisterBits]
        [MIR:ADDR μAddrSpace]
        [Mmux-out μAddrSpace]
        MPC-Inc-carry [MPC-Inc-out μAddrSpace]
        [Asel RegisterCount] [Bsel RegisterCount] [Csel RegisterCount]

        [A-Bus WordBits] [B-Bus WordBits] [C-Bus WordBits]
        [A-latch-out WordBits] [B-latch-out WordBits]
        [Amux-out WordBits] [ALU-out WordBits]
        Shifter-Left? Shifter-Right? Write-C?)
      (Net ()
           (Clock (list Clock:1 Clock:2 Clock:3 Clock:4))
           (ROM Microcode MPC-out pre-MIR)
           (Latch/N Clock:1 pre-MIR MIR)
           (Cut/N MIR
                  (reverse
                   (list MIR:AMUX MIR:COND MIR:ALU MIR:SH
                         MIR:MBR MIR:MAR MIR:RD MIR:WR
                         MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR)))

           (Decoder/N MIR:A Asel)
           (Decoder/N MIR:B Bsel)
           (Decoder/N MIR:C Csel)
           (RegisterRead Registers Asel A-Bus)
           (RegisterRead Registers Bsel B-Bus)
           (Latch/N Clock:2 A-Bus A-latch-out)
           (Latch/N Clock:2 B-Bus B-latch-out)
           (And MIR:MAR Clock:3 MAR?)
           (Latch/N MAR? B-latch-out MAR)
           (Mux/N A-latch-out MBR MIR:AMUX Amux-out)
           (ALU Amux-out B-Bus MIR:ALU ALU-out N Z)
           (MicroSeqLogic N Z MIR:COND MicroSeqLogic-out)
           (Mux/N MPC-Inc-out MIR:ADDR MicroSeqLogic-out Mmux-out)
           (Decoder/N MIR:SH (list GROUND Shifter-Right? Shifter-Left? GROUND))
           (Shifter/N Shifter-Left? Shifter-Right? ALU-out C-Bus)
           (And MIR:MBR Clock:4 MBR?)
           (Latch/N MBR? C-Bus MBR)
           (And Clock:4 MIR:ENC Write-C?)
           (RegisterSet Write-C? C-Bus Csel Registers)
           (Latch/N Clock:4 Mmux-out MPC-out)
           (Increment/N MPC-out MPC-Inc-carry MPC-Inc-out)))))

  @c{But, how do I represent these blocks? Everything is ultimately a
NAND gate and every wire is just a box. All other constructs are
derived using the typical definitions. The language, library, and
tests is 1000 lines.}
  (cslide
   (code
    (define (simulate! net)
      (for ([n (in-list net)])
        (match-define (nand a b o) n)
        (box-set! o
                  (not
                   (and (unbox a)
                        (unbox b))))))))

  @c{However, in addition to this, I have a version that compiles to C
and performs optimizations. It is so ridiculous, I just have to show
it.

  First, for performance I have an array of words with one bit for
each wire value. The machine uses about 7000 wires, so that's 111
words.}
  (fileslide "C-wires.c")

  @c{Each nand will be a call to a simple function that performs the nand and updates the wire}
  (fileslide "C-nand.c")

  @c{Then the program is about 7000 calls to this function, one for every nand:}
  (slide
   (scale-to-fit
    (vc-append
     (file->lines-pict "C-main-top.c")
     (tt "... 7000 more lines ")
     (file->lines-pict "C-main-bot.c"))
    client-w client-eh))

  @c{The compiler is a measly 250 lines and includes an optimizer that
removes double negation and copies of the same block (which decreases
the MIC-1 from about 7000 to about 4000 gates.)

  In summary, we have a perfect simulation of this old machine,
include all the supporting compilers, as well as exhaustive or
extensive test suites and it only cost 2000 lines of code, which is
about a third. If I only consider the bare necessities and not the
test suite, it is a little less than half that. Racket is the tool for
discerning microarchitects!}
  (bitslide "diagram.png")

  @c{Clock (7 lines)}
  (bitslide "clock.png")

  @c{Memory Interface - 1 cycle delay and 20 lines}
  (bitslide "memory.png")

  @c{IO - Busy waiting UART and 25 lines}
  (bitslide "io.png"))
