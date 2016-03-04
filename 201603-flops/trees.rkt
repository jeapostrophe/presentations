#lang racket/base
(require racket/list
         racket/class
         racket/match
         racket/contract/base
         pict
         pict/tree-layout
         data/enumerate
         data/enumerate/lib)

(define bt/e
  (delay/e
   (or/e (single/e #f)
         (list/e bt/e bt/e))))

(define (bt->tl b)
  (match b
    [#f #f]
    [(list x y)
     (tree-layout (bt->tl x) (bt->tl y))]))

;; xxx show Xs in slides
(define (trees->pict tls)
  (define ps (map binary-tidier (rest tls)))
  (define h (apply max (map pict-height ps)))
  (define w (apply max (map pict-width ps)))
  (apply hc-append (add-between ps (vline 5 h))))

(define (save-pict name p)
  (send (pict->bitmap p) save-file name 'png 100)
  p)

(save-pict
 "all.png"
 (trees->pict
  (for/list ([i (in-range 13)])
            (bt->tl (from-nat bt/e i)))))

(define (braun-insert l)
  (match l
    [#f (list #f #f)]
    [(list s t)
     (list (braun-insert t) s)]))

(save-pict
 "braun.png"
 (trees->pict
  (map bt->tl
       (reverse
        (for/fold ([l (list #f)]) ([i (in-range 8)])
          (cons (braun-insert (car l)) l))))))



