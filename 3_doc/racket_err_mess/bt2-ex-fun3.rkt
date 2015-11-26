
;; bt2-ex-fun3.rkt:
;;
  #lang racket

  (define (h x y) (+ x y))

  (define (hh x y) (h x y))

  (define (f x y)
    (gg x y)
    (h 0 1)
    )

  (define (gg x y) 
    (define z 7)
    (set! z (h 5 0))
    (g (- x z) y)
    (h (+ x z) 7)
    (hh 2 0)
    )

  (define zz 5)

  (define (g x y)
    (hh (+ x (h zz 0)) y)
    )

  (f 3 5)
  (f 3 'a)

#|

§growler:/home/deep/3_doc/racket_err_mess> racket -l errortrace -t bt2-ex-fun3.rkt
1
+: contract violation
  expected: number?
  given: 'a
  argument position: 2nd
  other arguments...:
   3
  errortrace...:
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:6:18: (+ x y)
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:15:2: (define (gg x y) (define z 7) (set! z (h 5 0)) (g (- x z) y) (h (+ x ....) 7) (hh 2 0))
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:30:2: (f 3 (quote a))
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:30:2: (f 3 (quote a))
  context...:
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:6:2: h
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:15:2: gg
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt:10:2: f
   /home/deep/3_doc/racket_err_mess/bt2-ex-fun3.rkt: [running body]

|#
