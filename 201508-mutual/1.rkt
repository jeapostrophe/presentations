;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define TRAIN-H 25)
(define TRAIN-W (* TRAIN-H 3))
(define train-body
  (rectangle TRAIN-W TRAIN-H "solid" "black"))

(define wheel
  (circle (/ TRAIN-H 2) "solid" "blue"))

(define train-bottom
  (overlay/xy
   wheel (+ (* -1 TRAIN-W) (* 2 (/ TRAIN-H 5))) (* -1 (/ TRAIN-H 5))
   (overlay/xy
    wheel (/ TRAIN-H 5) (* -1 (/ TRAIN-H 5))
    train-body)))

(define train
  (overlay/xy 
   (triangle (* 1.5 TRAIN-H) "solid" "red")
   (* -0.3 (image-width train-bottom))
   (image-height train-bottom)
   train-bottom))

(define bulls-eye
  (overlay/align
   "middle" "center"
   (circle TRAIN-H "solid" "red")
   (overlay/align
    "middle" "center"
    (circle (* 2 TRAIN-H) "solid" "black")
    (circle (* 3 TRAIN-H) "solid" "red"))))

(define W
  (* 6 (image-width train)))
(define H
  (+ (image-height train)
     (image-height bulls-eye)))
(define (crash t)
  (place-image train
               (+ t (+ 3 (/ (image-width train) 2)))
               (/ H 2)
               (place-image bulls-eye
                            (- W (/ (image-width bulls-eye) 2) 3)
                            (/ H 2)
                            (empty-scene W H))))

(crash 0)
(crash 100)

(animate crash)