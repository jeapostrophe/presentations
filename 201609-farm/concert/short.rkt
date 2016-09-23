#lang racket/base
(require racket/list
         racket/string
         racket/runtime-path
         racket/system)

(define (:system* . args)
  (printf "~a\n" `(system* ,@args))
  (apply system* args))

(define ffmpeg (find-executable-path "ffmpeg"))
(define-runtime-path here ".")
(define-runtime-path src "src")
(define-runtime-path short "short")

(define all empty)
(define (snoc l x) (append l (list x)))

(define (delete-file* full-to)
  (with-handlers ([exn:fail? void])
    (delete-file full-to)))

(define (one! from to start offset)
  (define full-to (build-path short to))
  (set! all (snoc all full-to))
  (cond
    [#f
     (void)]
    [else
     (delete-file* full-to)
     (:system* ffmpeg "-ss" start
              "-i" (build-path src from)
              "-frames" offset
              "-vf" "scale=320:240"
              "-aspect" "4:3"
              full-to)]))

(define (combine!)
  (define dest (build-path here "short.mp4"))
  (delete-file* dest)
  (apply
   :system* ffmpeg
   (flatten
    (list (for/list ([i (in-list all)])
            (list "-i" i))
          "-filter_complex"
          (string-join
           (flatten
            (list
             (for/list ([i (in-naturals)]
                        [_ (in-list all)])
               (format "[~a:0]" i))
             (format "concat=n=~a:v=1 [v]"
                     (length all)))))
          "-map" "[v]"
          dest))))

(module+ main
  (one! "DrMario.mkv" "DrMario.mp4" "0:13" "900")
  (one! "FF1.mkv" "FF1.mp4" "1:12" "900")
  (one! "Castlevania.mp4" "Castlevania.mp4" "0:00" "3300")
  (one! "Tetris.mp4" "Tetris.mp4" "0:00" "900")
  (one! "MegaMan2.webm" "MegaMan2.mp4" "0:10" "3400")

  (combine!))
