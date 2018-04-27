;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname growing-flowers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; growing-flowers.rkt

(require 2htdp/image)
(require 2htdp/universe)

;; growing flowers

;; =======================
;; Constants

(define WIDTH 600)
(define HEIGHT 400)

(define CTR-X (/ WIDTH  2))
(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT "LightGreen"))

(define INCR-SIZE 0.1)
(define INCR-ROT  5)
  


;; =======================
;; Data definitions

(define-struct flower (x y size rot))
;; Flower is (make-flower Number Number Number Number[0, 360))
;; interp. a flower at (x, y) of some size and angle of rotation rot
(define F1 (make-flower 10 20 4 4))
#;
(define (fn-for-flower f)
  (... (flower-x    f)          ; Number
       (flower-y    f)          ; Number
       (flower-size f)          ; Number
       (flower-rot  f)))        ; Number[0, 360)
;; Template rules used:
;;  - Compound: 4 fields



;; =======================
;; Functions


;; Flower -> Flower
;; start world with (main (make-flower CTR-X CTR-Y 0 0))
(define (main f)
  (big-bang f                             ; Flower
            (on-tick next-flower)         ; Flower -> Flower
            (to-draw render-flower)       ; Flower -> Image
            (on-mouse  handle-mouse)))    ; Flower Number Number MouseEvent -> Flower


;; Flower -> Flower
;; produce the next Flower
(check-expect (next-flower (make-flower 0 0 0 0))
              (make-flower 0 0 (+ 0 INCR-SIZE) (remainder (+ 0 INCR-ROT) 360)))
(check-expect (next-flower (make-flower 200 300 39 359))
              (make-flower 200 300 (+ 39 INCR-SIZE) (remainder (+ 359 INCR-ROT) 360)))

;(define (next-flower f) f) ; stub 

; <template from Flower
(define (next-flower f)
  (make-flower (flower-x f)
               (flower-y f)
               (+ (flower-size f) INCR-SIZE)
               (remainder (+ (flower-rot f) INCR-ROT) 360)))


;; Flower -> Image
;; render the flower with given parameters
(check-expect (render-flower F1)
              (place-image
               (rotate 4
                       (overlay (circle (flower-size F1) "solid" "yellow")
                                (pulled-regular-polygon (* (flower-size F1) 2) 5 1.1 140 "solid" "purple")))
               (flower-x F1)
               (flower-y F1)
               MTS))
                                                    
;(define (render-flower f) MTS) ; stub 

; <template from Flower>
(define (render-flower f)
  (place-image
   (rotate (flower-rot f)
           (overlay (circle (flower-size f) "solid" "yellow")
                    (pulled-regular-polygon (* (flower-size f) 2) 5 1.1 140 "solid" "purple")))
   (flower-x f)
   (flower-y f)
   MTS))       


;; Flower Number Number MouseEvent -> Flower
;; clicking mouse on the screen grows flowers at that position
(check-expect (handle-mouse F1 200 200  "button-down") (make-flower 200 200 0 0))
(check-expect (handle-mouse F1 200 200  "move")        F1)

;(define (handle-mouse f x y me) f) ; stub

;<template from MouseEvent>
(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 0 0)]
        [else                       f]))

