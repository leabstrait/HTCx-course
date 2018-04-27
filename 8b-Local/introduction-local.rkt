;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname introduction-local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; introduction-local.rkt

(local [(define a 1)                   
        (define b 2)]
  (+ a b))

; (+ a b)                ; not defined

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (fetch "portkey"))


;; you can define anything inside local

(define c 1)
(define d 2)

(+ c
   (local [(define d 3)]
     (+ c d))
   d)

(define (foo x)
  (local [(define (bar y) (+ 2 y))]
    (+ x (bar (* 2 x)))))

(list (foo 2) (foo 3))