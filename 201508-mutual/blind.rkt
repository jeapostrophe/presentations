;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blind) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define COLORS
  (list "red" "orange" "yellow"
        "green" "blue" "indigo" "violet"))

(define (blind t)
  (place-image
   (rectangle 800 600 "solid"
              (list-ref COLORS (modulo t (length COLORS))))
   400 300
   (empty-scene 800 600)))

(animate blind)