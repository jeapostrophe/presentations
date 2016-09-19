#lang racket/base
(require (for-syntax racket/base)
         srpnt/dev)

(define-syntax-rule (require/provide m ...)
  (begin (require m ...)
         (provide (all-from-out m ...))))

(require/provide
 srpnt/nestration
 srpnt/nestration/instruments
 data/enumerate
 data/enumerate/lib
 srpnt/music-theory)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ s)
     (syntax/loc stx
       (#%module-begin
        (define audio
          (use-bithoven
           #:style s
           #f #f))
        (provide audio)))]))

(provide
 (except-out (all-from-out racket/base)
             #%module-begin)
 (rename-out
  [module-begin #%module-begin]))
