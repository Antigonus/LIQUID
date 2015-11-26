#lang racket

(define (h x y) (+ x y))

(define-syntax (plus stx)
  (syntax-case stx ()
    [(plus x y) #'(h x y)]
    ))


(define (f x y) (g x y))
(define (g x y) (plus x y))

(f 3 2)
(f 3 'a)
