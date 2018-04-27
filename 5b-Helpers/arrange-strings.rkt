;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-strings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; ========================

(define BLANK (square 0 "solid" "white"))
(define TEXT-SIZE 20)
(define TEXT-COLOR "blue")

;; for tests

(define S1 "Apple")
(define S2 "Grapple")
(define S3 "Sally")
(define S4 "Tally")
(define S5 "Telly")

;; Data definitions
;; ====================

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
(define LOS0 empty)
(define LOS1 (cons "a" empty))
(define LOS2 (cons "a" (cons "b" empty)))
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
;; ====================

;; ListOfString -> Image
;; layout strings vertically in alphabetical order
(check-expect (arrange-strings (cons S3 (cons S1 empty)))
              (above/align "left"
                           (text S1 TEXT-SIZE TEXT-COLOR)
                           (text S3 TEXT-SIZE TEXT-COLOR)
                           BLANK))

;(define (arrange-strings los) BLANK) ; stub

(define (arrange-strings los)
  (layout-strings (sort-strings los)))

;; ListOfImage -> Image
;; place strings vertically
(check-expect (layout-strings empty) BLANK)
(check-expect (layout-strings (cons S1 (cons S3 empty)))
              (above/align "left"
                           (text S1 TEXT-SIZE TEXT-COLOR)
                           (text S3 TEXT-SIZE TEXT-COLOR)
                           BLANK))                              

;(define (layout-strings los) BLANK) ; stub

; template from ListOfString
(define (layout-strings los)
  (cond [(empty? los) BLANK]
        [else
         (above/align "left"
                      (text (first los) TEXT-SIZE TEXT-COLOR)
                      (layout-strings (rest los)))]))

;; ListOfString -> ListOfString
;; sort strings in alphabetical order
(check-expect (sort-strings empty) empty)
(check-expect (sort-strings (cons S1 (cons S3 empty)))
              (cons S1 (cons S3 empty)))
(check-expect (sort-strings (cons S3 (cons S1 empty)))
              (cons S1 (cons S3 empty)))
(check-expect (sort-strings (cons S2 (cons S4 (cons S1 empty))))
              (cons S1 (cons S2 (cons S4 empty))))

;(define (sort-strings los) los) ; stub

; template from ListOfString
(define (sort-strings los)
  (cond [(empty? los) empty]
        [else
         (insert-string (first los)
                        (sort-strings (rest los)))]))

;; String ListOfString -> ListOfString
;; insert s into proper place(alphabetical order) in lst
;; ASSUME lst is already sorted
(check-expect (insert-string S1 empty) (cons S1 empty))
(check-expect (insert-string S1 (cons S2 (cons S3 empty)))
              (cons S1 (cons S2 (cons S3 empty))))
(check-expect (insert-string S2 (cons S1 (cons S3 empty)))
              (cons S1 (cons S2 (cons S3 empty))))
(check-expect (insert-string S3 (cons S1 (cons S2 empty)))
              (cons S1 (cons S2 (cons S3 empty))))
(check-expect (insert-string S1 (cons S1 (cons S3 empty)))
              (cons S1 (cons S1 (cons S3 empty))))
(check-expect (insert-string S4 (cons S2 (cons S5 empty)))
              (cons S2 (cons S4 (cons S5 empty))))


;(define (insert-string s los) los) ; stub

; template from ListOfString
(define (insert-string s los)
  (cond [(empty? los) (cons s empty)]
        [else
         (if (string>=? s (first los))
             (cons (first los) (insert-string s (rest los)))
             (cons s los))]))
