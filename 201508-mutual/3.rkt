;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;  YOUR TODO make the displace-player function
;;   hint... (define (display-player w dx dy) (make-world ......))
;;  YOUR TODO add one bullet
;;   hint... add bx by, add a on-tick function to big-bang that takes a world and returns a world
;;  YOUR TODO make sure the player doesn't leave the screen (stop or go to the other side)
;;   hint... put it in displace-player
;; JAY'S TODO add many bullets

(define-struct world (x y))
;; make-world, world?, world-x, world-y

;; world -> image
(define (display-field w)
  (place-image (circle 25 "solid" "red")
               (world-x w) (world-y w)
               (empty-scene 800 600)))

;; world key -> world
(define (move-player w k)
  (cond
    [(key=? k "left")  
     (make-world (- (world-x w) 50)
                 (world-y w))]
    [(key=? k "right")  
     (make-world (+ (world-x w) 50)
                 (world-y w))]
    [(key=? k "down")  
     (make-world (world-x w)
                 (+ (world-y w) 50))]
    [(key=? k "up")  
     (make-world (world-x w)
                 (- (world-y w) 50))]
    [else
     w]))

(big-bang (make-world 100 50)
          (on-key move-player)
          (on-draw display-field))