;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants
;; ==================================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 50)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions
;; ==================================

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



(define-struct tank (x dx))
;; Tank is (make-tank Number)
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the direction the tank is going is dx right if positive and left if negative

(define T0 (make-tank (/ WIDTH 2) 1))  ;center going right
(define T1 (make-tank 50 1))           ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t)
       (tank-dx t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfInvader is one of:
;; - empty
;; - (cons INVADER ListOfInvader)
;; interp. a list of invaders

(define LOINVADER0 empty)
(define LOINVADER1 (list I1))
(define LOINVADER2 (list I1 I2 I3))

#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else (... (first loinvader)
                   (fn-for-loinvader (rest loinvader)))]))

;; ListOfMissile is one of:
;; - empty
;; - (cons MISSILE ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-lom (rest lom)))]))
              


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions
;; ==================================

;; Game -> Game
;; start world with (main G0)
;; no tests for main

(define (main g)
  (big-bang g                               ; Game
            (on-tick   next-game-state)     ; Game -> Game
            (to-draw   render-game)         ; Game -> Image
            (on-key    handle-key)          ; Game KeyEvent -> Game 
            (stop-when last-game-state?)    ; Game -> Boolean
            (state true)))
;))
;; Game -> Game
;; calculate the game's next state

; examples are unpredictable

;(define (next-game-state g) g) ; stub

; template from Game
(define (next-game-state g)
  (make-game (next-loinvader (alive-invaders     (new-invaders (game-invaders g)) (game-missiles g)))
             (next-lom       (remaining-missiles (new-invaders (game-invaders g)) (game-missiles g)))
             (next-tank      (game-tank g))))

;; ListOfInvader -> ListOfInvader
;; append random invaders to listofinvaders according to INVADE-RATE

;; the outputs are random so examples are skipped

;(define (new-invaders loinvader) loinvader) ; stub

; template from ListOfInvader
(define (new-invaders loinvader)
  (cond [(empty? loinvader) (list I1)]
        [else (cond [(< (random 1000) INVADE-RATE)
                     (cons (make-invader (random WIDTH) 0 1) loinvader)]
                    [else
                     loinvader])]))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; produce list of invaders that have not collided with missiles
(check-expect (alive-invaders (list (make-invader 200 200 1)
                                    (make-invader 100 400 -1))
                              empty)
              (list (make-invader 200 200 1)
                    (make-invader 100 400 -1)))
(check-expect (alive-invaders (list (make-invader 200 200 1)
                                    (make-invader 100 400 -1))
                              (list (make-missile 200 (+ 200 9))))
              (list (make-invader 100 400 -1)))

;(define (alive-invaders loinvader lom) loinvader) ; stub

; template from 2 Oneof
(define (alive-invaders loinvader lom)
  (cond [(empty? lom) loinvader]
        [else
         (cond
           [(empty? loinvader) loinvader]
           [(is-hit? (first loinvader) lom)
            (alive-invaders (rest loinvader) lom)]
           [else (cons (first loinvader) (alive-invaders (rest loinvader) lom))])]))

;; Invader ListOfMissile -> Boolean
;; produce true if any missile in list of missile hits invader
(check-expect (is-hit? (make-invader 200 12 1) (list (make-missile 200 300)))
              false)
(check-expect (is-hit? (make-invader 200 12 1) (list (make-missile 200 13)))
              true)

;(define (is-hit? i lom) false) ; stub

; template from ListOfMissile

(define (is-hit? i lom)
  (cond [(empty? lom) false]
        [(and (<= (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE))
              (<= (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))) 
         true]
        [else
         (is-hit? i (rest lom))]))


;; ListOfInvader ListOfMissile -> ListOfMissile
;; produce list of missiles that have not hit invaders
(check-expect (remaining-missiles (list (make-invader 200 200 1)
                                        (make-invader 100 400 -1))
                                  (list (make-missile 200 (+ 200 9))
                                        (make-missile 3 3)))
              (list (make-missile 3 3)))

;(define (remaining-missiles loinvader lom) lom) ; stub

; template from 2 one of
(define (remaining-missiles loinvader lom)
  (cond [(empty? loinvader) lom]
        [else
         (cond [(empty? lom) lom]
               [(hits? (first lom) loinvader)
                (remaining-missiles loinvader (rest lom))]
               [else (cons (first lom) (remaining-missiles loinvader (rest lom) ))])]))


;; Missile ListOfInvader -> Boolean
;; produce true if any invader in list of invader is hit by the missile
(check-expect (hits? (make-missile 200 300) (list (make-invader 200 12 1)))
              false)
(check-expect (hits? (make-missile 200 13) (list (make-invader 200 12 1)))
              true)

;(define (hits? m loinvader) false) ; stub

; template from ListOfInvader

(define (hits? m loinvader)
  (cond [(empty? loinvader) false]
        [(and (<= (- (invader-y (first loinvader)) HIT-RANGE) (missile-y m) (+ (invader-y (first loinvader)) HIT-RANGE))
              (<= (- (invader-x (first loinvader)) HIT-RANGE) (missile-x m) (+ (invader-x (first loinvader)) HIT-RANGE))) 
         true]
        [else
         (hits? m (rest loinvader))]))


;; ListOfInvader -> ListOfInvader
;; produce next set of invaders

;(define (next-loinvader loinvader) loinvader) ; stub

; template from ListOfInvader
(define (next-loinvader loinvader)
  (cond [(empty? loinvader) empty]
        [else
         (if (invader-in-scene? (first loinvader))
             (cond [(>= (+ (invader-x (first loinvader)) INVADER-X-SPEED) WIDTH)
                    (cons (make-invader (- (invader-x (first loinvader)) INVADER-X-SPEED)
                                        (+ (invader-y (first loinvader)) INVADER-Y-SPEED)
                                        (- (invader-dx (first loinvader))))
                          (next-loinvader (rest loinvader)))]
                   [(<= (- (invader-x (first loinvader)) INVADER-X-SPEED) 0)
                    (cons (make-invader (+ (invader-x (first loinvader)) INVADER-X-SPEED)
                                        (+ (invader-y (first loinvader)) INVADER-Y-SPEED)
                                        (- (invader-dx (first loinvader))))
                          (next-loinvader (rest loinvader)))]
                   [else
                    (if (> (invader-dx (first loinvader)) 0)
                        (cons (make-invader (+ (invader-x (first loinvader)) INVADER-X-SPEED)
                                            (+ (invader-y (first loinvader)) INVADER-Y-SPEED)
                                            (invader-dx (first loinvader)))
                              (next-loinvader (rest loinvader)))
                        (cons (make-invader (- (invader-x (first loinvader)) INVADER-X-SPEED)
                                            (+ (invader-y (first loinvader)) INVADER-Y-SPEED)
                                            (invader-dx (first loinvader)))
                              (next-loinvader (rest loinvader))))])
             (next-loinvader (rest loinvader)))]))


;; ListOfMissile -> ListOfMissile
;; produce next set of missiles
(check-expect (next-lom empty) empty)
(check-expect (next-lom (list (make-missile 200 200) (make-missile 300 200)))
              (list (make-missile 200 (- 200 MISSILE-SPEED)) (make-missile 300 (- 200 MISSILE-SPEED))))

;(define (next-lom lom) lom) ; stub

;template from ListOfMissile
(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (m-in-scene? (first lom))
             (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                   (next-lom (rest lom)))
             (next-lom (rest lom)))]))

;; Tank -> Tank
;; produce the next tank
(check-expect (next-tank (make-tank 50 -1)) (make-tank 49 -1))
(check-expect (next-tank (make-tank 50  1)) (make-tank 51  1))

;(define (next-tank t) t) ; stub

; template from Tank
(define (next-tank t)            
  (cond [(and (> (tank-x t) 0)     (< (tank-dx t) 0))
         (make-tank (- (tank-x t) 1) (tank-dx t))]
        [(and (< (tank-x t) WIDTH) (> (tank-dx t) 0))
         (make-tank (+ (tank-x t) 1) (tank-dx t))]
        [else
         (make-tank (tank-x t) (tank-dx t))]))

;; Game -> Image
;; render the game state
(check-expect (render-game (make-game empty empty T0))
              (place-image TANK
                           (/ WIDTH 2)
                           (- HEIGHT TANK-HEIGHT/2)  
                           BACKGROUND))
(check-expect (render-game (make-game empty empty T1))
              (place-image TANK
                           50
                           (- HEIGHT TANK-HEIGHT/2)  
                           BACKGROUND))
(check-expect (render-game (make-game (list I1) (list M1) T1))
              (place-image INVADER
                           150 100
                           (place-image
                            MISSILE
                            150 300
                            (place-image TANK
                                         50
                                         (- HEIGHT TANK-HEIGHT/2)  
                                         BACKGROUND))))

;(define (render-game g) BACKGROUND) ; stub

;template from Game as function composition
(define (render-game g)
  (place-loinvader (game-invaders g)
                   (place-lom (game-missiles g)
                              (place-tank (game-tank g)))))

;; ListOfInvader Image -> Image
;; place the given list of invader(s) on the supplied image
(check-expect (place-loinvader LOINVADER0 BACKGROUND)
              (place-image empty-image
                           0 0
                           BACKGROUND))
(check-expect (place-loinvader LOINVADER2 BACKGROUND)
              (place-image INVADER
                           (invader-x I1) (invader-y I1)
                           (place-image INVADER
                                        (invader-x I2) (invader-y I2)
                                        (place-image INVADER
                                                     (invader-x I3) (invader-y I3)
                                                     BACKGROUND))))

;(define (place-loinvader invader i) BACKGROUND) ; stub

;template from ListOfInvader
(define (place-loinvader loinvader i)
  (cond [(empty? loinvader) i]
        [else (place-image INVADER
                           (invader-x (first loinvader)) (invader-y (first loinvader))
                           (place-loinvader (rest loinvader) i))]))
  

;; ListOfMissile Image -> Image
;; place the given list of missile(s) on the supplied image
(check-expect (place-lom LOM0 BACKGROUND)
              (place-image empty-image
                           0 0
                           BACKGROUND))
(check-expect (place-lom LOM2 BACKGROUND)
              (place-image MISSILE
                           (missile-x M1) (missile-y M1)
                           (place-image MISSILE
                                        (missile-x M2) (missile-y M2)
                                        (place-image MISSILE
                                                     (missile-x M3) (missile-y M3)
                                                     BACKGROUND))))

;(define (place-lom m i) BACKGROUND) ; stub

;template from ListOfMissile
(define (place-lom lom i)
  (cond [(empty? lom) i]
        [else (place-image MISSILE
                           (missile-x (first lom)) (missile-y (first lom))
                           (place-lom (rest lom) i))])) 


;; Tank Image -> Image
;; place the given tank on BACKGROUND
(check-expect (place-tank T0)
              (place-image TANK
                           (tank-x T0) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (place-tank T1)
              (place-image TANK
                           (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (place-tank T2)
              (place-image TANK
                           (tank-x T2) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
                           
;(define (place-tank tank) BACKGROUND) ; stub

; template from Tank
(define (place-tank t)
  (place-image TANK
               (tank-x t) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))

  
;; Game KeyEvent -> Game
;; respond to the arrow keys and spacebar
(check-expect (handle-key (make-game empty empty (make-tank 30 1)) " ")
              (make-game empty (list (make-missile 30 (- HEIGHT TANK-HEIGHT/2))) (make-tank 30 1)))
(check-expect (handle-key (make-game empty empty (make-tank 30 1)) "left")
              (make-game empty empty (make-tank 30 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 30 -1)) "right")
              (make-game empty empty (make-tank 30 1)))


;(define (handle-key g ke) g) ; stub

; template from KeyEvent
(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2))
                          (game-missiles g))
                    (game-tank g))]
        [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))] 
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g))  1))]
        [else
         g]))

;; Missile -> Boolean
;; consume missile and return true if it is still inside the scene
(check-expect (m-in-scene? (make-missile 10 20)) true)
(check-expect (m-in-scene? (make-missile 10 -20)) false)

;(define (m-in-scene? m) true) ; stub

; template from Missile
(define (m-in-scene? m)
  (<= 0 (missile-y m) HEIGHT))

;; Invader -> Boolean
;; consume invader and return true if it is still inside the scene
(check-expect (invader-in-scene? (make-invader 10 20 -10)) true)
(check-expect (invader-in-scene? (make-invader (+ WIDTH 200) 200 -10)) false)

;(define (invader-in-scene? invader) true) ; stub

; template from Missile
(define (invader-in-scene? invader)
  (and (<= 0 (invader-x invader) WIDTH)
       (<= 0 (invader-y invader) HEIGHT)))

;; Game(ListOfInvader) -> Boolean
;; is the game over?
(check-expect (last-game-state? (make-game (list (make-invader 300 (+ 2 HEIGHT) 12)) empty T1))
              true)

;(define (last-game-state? g) false) ; stub

; template from Game
(define (last-game-state? g)
  (cond [ (empty? (game-invaders g)) false]
        [(>= (invader-y (first (game-invaders g))) (- HEIGHT TANK-HEIGHT/2))
         true]
        [else
         (last-game-state? (make-game (rest (game-invaders g)) (game-missiles g) (game-tank g)))]))

