#lang racket

(define (h x y) (+ x y))

(define-syntax (plus stx)
  (let(
        [datum (syntax->datum stx)]
        )
    (let(
          [x (cadr datum)]
          [y (caddr datum)]
          )
      (define program `(h ,x ,y))
      (datum->syntax stx program)
      )))

(define (f x y) (g x y))
(define (g x y) (plus x y))

(f 3 2)
(f 3 'a)
