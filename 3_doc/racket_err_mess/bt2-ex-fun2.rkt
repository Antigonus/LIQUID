
I have a related question.  I turned on error trace using the command line Robby
suggested, but it doesn't give me a full trace.  Here is an example below, the
trace should be f -> gg -> g -> hh -> h then bang, the error.   But instead
Racket shows me f -> + bang,  "in context of h".  But which 'h'?

How do you turn on tracing?  If I go interactive is there a command which will
give me the trace?


;; bt2-ex-fun2.rkt:
;;
  #lang racket

  (define (h x y) (+ x y))

  (define (hh x y) (h x y))

  (define (f x y)
    (gg x y)
    )

  (define (gg x y) 
    (define z 7)
    (set! z (h 5 0))
    (g (- x z) y)
    )

  (define zz 5)

  (define (g x y)
    (hh (+ x (h zz 0)) y)
    )

  (f 3 5)
  (f 3 'a)

  #| result:

  §> racket -l errortrace -t bt2-ex-fun2.rkt 
  8
  +: contract violation
    expected: number?
    given: 'a
    argument position: 2nd
    other arguments...:
     3
    errortrace...:
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun2.rkt:3:16: (+ x y)
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun2.rkt:24:0: (f 3 (quote a))
    context...:
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun2.rkt:3:0: h
     /home/deep/3_doc/racket_err_mess/bt2-ex-fun2.rkt: [running body]

  §> 

  |#
