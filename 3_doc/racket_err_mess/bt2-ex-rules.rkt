#lang racket

(define (h x y) (+ x y))

(define-syntax plus
  (syntax-rules()
    [(plus x y) (h x y)]
    ))

(define (f x y) (g x y))
(define (g x y) (plus x y))

(f 3 2)
(f 3 'a)
