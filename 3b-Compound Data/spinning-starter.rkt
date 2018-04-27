;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; spinning-starter.rkt



;; a rotating and growing red box

;; =======================
;; Constants

;; Scene
(define WIDTH 400)
(define HEIGHT WIDTH)
(define MTS (empty-scene WIDTH HEIGHT))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

;; Box
(define FILL "solid")
(define COL "red")
(define DL 1)
(define DR 15)
  


;; =======================
;; Data definitions

(define-struct box (l r))
;; Box is (make-box Number Number[0, 360))
;; interp. a box with length l and angle of rotation r
(define START (make-box 0 0))
(define B1 (make-box 10 0))
(define B2 (make-box 140 130))
#;
(define (fn-for-box b)
  (... (box-l b)          ; Number
       (box-r b)))        ; Number[0, 360)
;; Template rules used:
;;  - Compound: 2 fields



;; =======================
;; Functions


;; Box -> Box
;; start world with (main START)
(define (main b)
  (big-bang b                        ; Box
            (on-tick next-box)       ; Box -> Box
            (to-draw render-box)     ; Box -> Image
            (on-key  handle-key)))   ; Box KeyEvent -> Box


;; Box -> Box
;; produce the next Box
(check-expect (next-box (make-box 10 0))
              (make-box (+ 10 DL) (remainder (+ 0 DR) 360)))
(check-expect (next-box (make-box 39 359))
              (make-box (+ 39 DL) (remainder (+ 359 DR) 360)))

;(define (next-box b) b) ; stub 

; <template from Box>
(define (next-box b)
  (make-box (+ (box-l b) DL) (remainder (+ (box-r b) DR) 360)))


;; Box -> Image
;; render the box with given parameters
(check-expect (render-box B1) (place-image (rotate 0 (square 10 FILL COL))
                                           CTR-X
                                           CTR-Y
                                           MTS))
(check-expect (render-box B2) (place-image (rotate 130 (square 140 FILL COL))
                                           CTR-X
                                           CTR-Y
                                           MTS))
                                                    
;(define (render-box b) MTS) ; stub 

; <template from Box>
(define (render-box b)
  (place-image (rotate (box-r b) (square (box-l b) FILL COL))
               CTR-X
               CTR-Y
               MTS))       


;; Box KeyEvent -> Box
;; reset the box to the start state if Space key pressed
(check-expect (handle-key B2 " ") START)
(check-expect (handle-key B2 "a") B2)

;(define (handle-key b ke) b) ; stub

;<template from KeyEvent>
(define (handle-key b ke)
  (cond [(key=? ke " ") START]
        [else b]))
