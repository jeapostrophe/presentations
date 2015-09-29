;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define lame-fred
  (overlay (circle 25 "solid" "red")
           (circle 50 "solid" "black")
           (circle 75 "solid" "red")))

(define (movie frame-number)
  (place-image lame-fred
               400
               (- 600 (* 10 frame-number))
               (place-image (rectangle 50 50 "solid" "blue")
                            400
                            300
                            (empty-scene 800 600))))

(define (tick-tock time)
  (+ time 1))

(define (press-button the-button time)
  0)

; (animate movie)
; =>
(big-bang 0 ;; starts at 0
          (on-tick tick-tock)
          (on-key press-button)
          (on-draw movie))