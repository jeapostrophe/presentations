;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3m) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; JAY'S TODO make more bullets!

(define-struct world (x y bx by))

;; world -> image
(define (display-field w)
  (place-image (circle 25 "solid" "red")
               (world-x w) (world-y w)
               (place-image (circle 5 "solid" "blue")
                            (world-bx w) (world-by w)
                            (empty-scene 800 600))))

(define (displace-player w dx dy)
  (make-world (+ (world-x w) dx)
              (+ (world-y w) dy)
              (world-bx w)
              (world-by w)))

;; world key -> world
(define (move-player w k)
  (cond
    [(key=? k "left")
     (displace-player w -50 0)]
    [(key=? k "right")
     (displace-player w +50 0)]
    [(key=? k "down")
     (displace-player w 0 +50)]
    [(key=? k "up")
     (displace-player w 0 -50)]
    [(key=? k " ")
     (make-world (world-x w) (world-y w) ;; <-- player's position
                 ;; v---- bullet's position
                 (world-x w) (world-y w))]
    [else
     w]))

;; world -> world
(define (move-bullet w)
  (make-world (world-x w) (world-y w)
              (+ 1 (world-bx w)) (world-by w)))

(big-bang (make-world 100 50 -10 -10)
          (on-key move-player)
          (on-tick move-bullet)
          (on-draw display-field))
