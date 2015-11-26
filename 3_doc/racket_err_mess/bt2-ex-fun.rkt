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

      ;; bury syntax information from stx into the function name.., say 1-1 for now
      (define program 
        `(begin 
           (define (plus-fun-1-1 xx yy) (h xx yy)) 
           (plus-fun-1-1 ,x ,y)))

      (datum->syntax stx program)
      )))

(define (f x y) (g x y))
(define (g x y) (plus x y))

(f 3 2)
(f 3 'a)

#|  ah heck it didn't work, neither plus fun, nor g show up in the trace


  §growler:/home/deep/3_doc/racket_err_mess> racket -v
  Welcome to Racket v6.2.

  §growler:/home/deep/3_doc/racket_err_mess> racket -l errortrace -t bt2-ex-fun.rkt 
  5
  +: contract violation
    expected: number?
    given: 'a
    argument position: 2nd
    other arguments...:
     3
    errortrace...:
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun.rkt:3:16: (+ x y)
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun.rkt:26:0: (f 3 (quote a))
    context...:
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun.rkt:3:0: h
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun.rkt: [running body]
|#
