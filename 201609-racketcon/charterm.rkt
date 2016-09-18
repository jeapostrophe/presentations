#lang racket/base

(define (tdisplay x)
  (display x)
  (flush-output))

(define ESC-ch (integer->char 27))
(define (ESC . more)
  (for-each tdisplay (cons ESC-ch more)))
(define (charterm-clear-screen)
  (ESC #\c))

(define (CSI . more)
  (apply ESC #\[ more))
(define (charterm-cursor x y)
  (CSI y #\; x #\f))
(define charterm-display tdisplay)
(define charterm-newline newline)
(define charterm-read-key read-char)
(define-syntax-rule (with-charterm . body)
  (let () . body))

(define (SGR code)
  (CSI code #\m))
(define (charterm-normal)
  (SGR 0))
(define (charterm-bold)
  (SGR 1))
(define (charterm-inverse)
  (SGR 7))

(provide (all-defined-out))
