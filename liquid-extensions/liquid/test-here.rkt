  #lang racket

    (require errortrace)

    (require racket/match)
    (require (for-syntax racket/match))

    (require syntax/location)
    (require (for-syntax syntax/location))

    (require syntax/srcloc)
    (require (for-syntax syntax/srcloc))

    (require syntax/parse)
    (require (for-syntax syntax/parse))


  (define-syntax (here stx) #`(quote-srcloc #,stx))

  (define-syntax (here2 stx) 
      (let(
            [a-location (build-source-location stx)]
            )
        (datum->syntax stx a-location)
        ))

  (define-syntax (here3 stx) 
      (let(
             [program `(quote-srcloc ,stx)]
            )

        (datum->syntax stx program)
        ))


  (here)
  (here2)
  (here3)

#|

  (define-syntax (here4 stx) 
      (let(
             [program `(+ 1 2)]
            )

        (syntax/loc stx program)
        ))

  (here4)

  racket@test-here.rkt> (enter! "test-here.rkt")
    [re-loading /home/deep/liquid-extensions/liquid/test-here.rkt]
  test-here.rkt:47:0: program: identifier used out of context
    in: program
|#


#|
    (define-syntax (here5 stx) 
        (let(
               [program-syntax #'(+ 1 'a)]
              )

          program-syntax
          ))

  (here5)

  racket@test-here.rkt> (enter! "test-here.rkt")
    [re-loading /home/deep/liquid-extensions/liquid/test-here.rkt]
  (srcloc "<pkgs>/liquid-extensions/liquid/test-here.rkt" 36 2 689 6)
  (srcloc #<path:/home/deep/liquid-extensions/liquid/test-here.rkt> 37 2 698 7)
  (srcloc "<pkgs>/liquid-extensions/liquid/test-here.rkt" 38 2 708 7)
  +: contract violation
    expected: number?
    given: 'a
    argument position: 2nd
    other arguments...:
     1
    errortrace...:
     /home/deep/liquid-extensions/liquid/test-here.rkt:61:33: (+ 1 (quote a))
     /home/deep/liquid-extensions/liquid/test-here.rkt:61:33: (+ 1 (quote a))
|#


#|
    (define-syntax (here6 stx) 
        (let(
               [program-syntax (syntax/loc stx (+ 1 'a))]
              )

          program-syntax
          ))

  (here6)

  racket@test-here.rkt> (enter! "test-here.rkt")
    [re-loading /home/deep/liquid-extensions/liquid/test-here.rkt]
  (srcloc "<pkgs>/liquid-extensions/liquid/test-here.rkt" 36 2 689 6)
  (srcloc #<path:/home/deep/liquid-extensions/liquid/test-here.rkt> 37 2 698 7)
  (srcloc "<pkgs>/liquid-extensions/liquid/test-here.rkt" 38 2 708 7)
  +: contract violation
    expected: number?
    given: 'a
    argument position: 2nd
    other arguments...:
     1
    errortrace...:
     /home/deep/liquid-extensions/liquid/test-here.rkt:67:2: (here6)
     /home/deep/liquid-extensions/liquid/test-here.rkt:67:2: (here6)
    context...:
     /home/deep/liquid-extensions/liquid/test-here.rkt: [running body]
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-here.rkt> 
|#

 

    (define-syntax (here7 stx) 
        (let(
               [program-syntax (syntax/loc stx (+ 1 'a))]
              )

          program-syntax
          ))

  (here7)
