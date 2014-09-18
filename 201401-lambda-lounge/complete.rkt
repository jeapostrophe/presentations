#lang racket/base
(require racket/list
         racket/match)
(module+ test
  (require rackunit))

;; A singly-linked list is either
;; - An empty list (NULL)
;; - A node with a data pointer and a next pointer

(struct sll ())
(struct sll:end sll ())
(struct sll:node sll (data next))

(module+ test
  (define sll1
    (sll:node 1 (sll:node 2 (sll:node 3 (sll:node 4 (sll:end))))))
  (check-equal? (sll:node-data sll1)
                1)
  (check-equal? (sll:node-data (sll:node-next sll1))
                2)
  (check-equal? (sll:node-data (sll:node-next (sll:node-next sll1)))
                3)
  (check-equal? (sll:node-data (sll:node-next (sll:node-next (sll:node-next sll1))))
                4)
  (check-pred sll:end?
              (sll:node-next (sll:node-next (sll:node-next (sll:node-next sll1))))))

;; A list is either
;; - empty
;; - A cons with a first and a rest

(module+ test
  (define l1
    (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
  (check-equal? (first l1)
                1)
  (check-equal? (first (rest l1))
                2)
  (check-equal? (first (rest (rest l1)))
                3)
  (check-equal? (first (rest (rest (rest l1))))
                4)
  (check-pred empty?
              (rest (rest (rest (rest l1))))))

;; length : listof A -> num
(define (length l)
  (if (empty? l)
    0
    (add1 (length (rest l)))))

(module+ test
  (check-equal? (length empty) 0)
  (check-equal? (length l1) 4)
  (check-equal? (length (list 1 2 3 4 5 6 7)) 7))

;; all-even? : listof nat -> boolean
(define (all-even? l)
  (cond
    [(empty? l)
     #t]
    [(even? (first l))
     (all-even? (rest l))]
    [else
     #f]))

(module+ test
  (check-true (all-even? empty))
  (check-true (all-even? (list 2 4 6 8)))
  (check-false (all-even? (list 2 4 5 6 8))))

;; xxx show num->str

;; xxx show map (closures)

;; XXX show foldr and relate to length/all-even?

;; COMMENT on TCO

;; insert : nat nlist -> nlist
(define (insert x l)
  (cond
    [(empty? l)
     (list x)]
    [(< x (first l))
     (cons x l)]
    [else
     (cons (first l) (insert x (rest l)))]))

(module+ test
  (check-equal? (insert 10 empty)
                (list 10))
  (check-equal? (insert 8 (list 10))
                (list 8 10))
  (check-equal? (insert 9 (list 8 10))
                (list 8 9 10))

  (check-equal? (insert 5 (list 1 2 3 4 6 7 8 9 10))
                (list 1 2 3 4 5 6 7 8 9 10)))

;; sort : nlist -> nlist
(define (sort l)
  (cond
    [(empty? l)
     empty]
    [else
     (insert (first l) (sort (rest l)))]))

(module+ test
  (check-equal? (sort (list 3 2 4 6 1 9 10))
                (list 1 2 3 4 6 9 10))
  (check-equal? (sort (shuffle (list 1 2 3 4 5)))
                (list 1 2 3 4 5)))

(struct branch (left value right))

(module+ test
  (define bt1
    (branch
     (branch
      (branch #f 1 #f)
      2
      (branch #f 3 #f))
     4
     (branch
      (branch #f 5 #f)
      6
      (branch #f 7 #f)))))

(define (preorder bt)
  (match bt
    [#f 
     empty]
    [(branch left v right)
     (append (preorder left)
             (cons v (preorder right)))]))

(module+ test
  (check-equal? (preorder bt1) (list 1 2 3 4 5 6 7)))

(define (bt-insert bt v)
  (match bt
    [#f
     (branch #f v #f)]
    [(branch left bv right)
     (if (< v bv)
       (branch (bt-insert left v) bv right)
       (branch left bv (bt-insert right v)))]))

(module+ test
  (check-equal? (preorder (bt-insert #f 5))
                (list 5))
  (check-equal? (preorder 
                 (bt-insert (bt-insert #f 5) 7))
                (list 5 7))
  (check-equal? (preorder 
                 (bt-insert (bt-insert (bt-insert #f 5) 7) 3))
                (list 3 5 7)))

(struct zipper (ctxt focus))
(struct ctxt-left (bv right))
(struct ctxt-right (left bv))

(define (btree->zipper bt)
  (zipper empty bt))
(define (move-left z)
  (match z
    [(zipper ctxt (branch left bv right))
     (zipper (cons (ctxt-left bv right) ctxt) left)]))
(define (move-right z)
  (match z
    [(zipper ctxt (branch left bv right))
     (zipper (cons (ctxt-right left bv) ctxt) right)]))
(define (move-up z)
  (match z
    [(zipper (cons (ctxt-left bv right) ctxt) left)
     (zipper ctxt (branch left bv right))]
    [(zipper (cons (ctxt-right left bv) ctxt) right)
     (zipper ctxt (branch left bv right))]))
(define (zreplace z bt)
  (match-define (zipper ctxt _) z)
  (zipper ctxt bt))
(define (zread z)
  (match-define (zipper _ (branch _ v _)) z)
  v)

(module+ test
  (define z1 (btree->zipper bt1))
  (check-equal? (zread z1) 4)
  (check-equal? (zread (move-left z1)) 2)
  (check-equal? (zread (move-right (move-left z1))) 3)
  (check-equal? (zread (move-up (move-right (move-left z1)))) 2)
  (check-equal? (zread (move-up (move-up (move-right (move-left z1))))) 4)
  (check-equal? (zread (move-right (move-up (move-up (move-right (move-left z1)))))) 6)
  (check-equal? (zread (move-right z1)) 6))

;; XXX
