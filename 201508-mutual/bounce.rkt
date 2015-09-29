;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define W 800)
(define H 600)

(define (tick-tock t)
  (+ t 10))

(define (show-me-show-me t)
  (place-image (circle 50 "solid" "blue")
               t 50
               (empty-scene W H)))

(define (interact ke t)
  0)

(define (done? t)
  (> t W))

(big-bang 0
          (on-tick tick-tock)
          (on-key interact)
          (stop-when done?)
          (on-draw show-me-show-me))