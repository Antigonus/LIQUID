  #lang racket

    (require errortrace)

    (require syntax/srcloc)
    (require (for-syntax syntax/srcloc))

#|

  (define-syntax (messup stx) 
      (let(
            [a-location (build-source-location stx)]
            )
        (atum->syntax stx a-location)
        ))

  (messup)

  racket@test-here.rkt> (enter! "test-macro-dev-error.rkt")
  atum->syntax: undefined;
   cannot reference an identifier before its definition
    in module: 'test-macro-dev-error
    phase: 1
    errortrace...:
    context...:
     /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-here.rkt> 

|#


#|

  (define-syntax (messup2 stx) 
      (let(
            [program '(define (a b) (^ a b))]
            )
        (datum->syntax stx program)
        ))

  (messup2)

  racket@test-here.rkt> (enter! "test-macro-dev-error.rkt")
  ^: unbound identifier in module
    in: ^
    errortrace...:
    context...:
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:434:2: errortrace-annotate
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:482:4
     standard-module-name-resolver
     /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-here.rkt> 

|#

#|
  (define-syntax (messup3 stx) 
    (syntax/loc stx (define (a b) (^ a b)))
    )

  (messup3)


  this is beautiful, syntax/loc really works, points right at the carret.

  racket@test-here.rkt> (enter! "test-macro-dev-error.rkt")
  test-macro-dev-error.rkt:62:35: ^: unbound identifier in module
    in: ^
    errortrace...:
    context...:
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:434:2: errortrace-annotate
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:482:4
     standard-module-name-resolver
     /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-here.rkt> 
|#



#|

  (define-syntax (messup4 stx) 
    (syntax/loc stx
      (let (
             [a 7]
             [b 9]
             )
        (^ a b)
       )))
        
  (define (f) (messup4))
  (f)



racket@test-macro-dev-error.rkt> (enter! "test-macro-dev-error.rkt")
  [re-loading /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt]
test-macro-dev-error.rkt:91:9: ^: unbound identifier in module
  in: ^
  errortrace...:
  context...:
   /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:434:2: errortrace-annotate
   /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:482:4
   /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
   /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
   /usr/share/racket/collects/racket/private/misc.rkt:87:7

|#

#|
  (define-syntax (messup5 stx) 
    (datum->syntax stx
      (let (
             [a 7]
             [b 9]
             )
        (^ a b)
       )))
        
  (messup5)

  racket@test-macro-dev-error.rkt> (enter! "test-macro-dev-error.rkt")
    [re-loading /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt]
  ^: undefined;
   cannot reference an identifier before its definition
    in module: 'test-macro-dev-error
    phase: 1
    errortrace...:
    context...:
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-macro-dev-error.rkt> 
|#


  (define-syntax (fine1 stx)
    (let (
           [a 7]
           [b 9]
           )
      (quasisyntax/loc stx
        (+ #,a #,b)
        )))


#|
 racket@test-macro-dev-error.rkt> (enter! "test-macro-dev-error.rkt")
   [re-loading /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt]
 racket@test-macro-dev-error.rkt> (fine1)
 16
 racket@test-macro-dev-error.rkt> 
|#

#|
  (define-syntax (messup6 stx)
    (let* (
           [a 7]
           [b 'a]
           )
      (quasisyntax/loc stx
        (+ #,a #,b)
        )))

  (define (f) (g) (h))
  (define (g) (messup6) (h))

  (define (h) (void)) ;; stop tail call optimization so we can see the trace


    racket gives no clue to the location of the error

    racket@test-macro-dev-error.rkt> (enter! "test-macro-dev-error.rkt")
      [re-loading /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt]

    a: identifier used out of context
      in: a
      errortrace...:
      context...:
       /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:434:2: errortrace-annotate
       /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:482:4
       /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
       /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
       /usr/share/racket/collects/racket/private/misc.rkt:87:7
  racket@test-macro-dev-error.rkt>
|#

#|
  (define-syntax (messup7 stx)
    (with-handlers (
                     [(λ(v) #t) (λ(v) (displayln "error!"))] ;; catches all errors
                     ) 
      (let* (
              [a 7]
              [b 'a]
              )
        (quasisyntax/loc stx
          (+ #,a #,b)
          ))))


  (define (call7)
   (with-handlers (
                    [(λ(v) #t) (λ(v) (displayln "call error!"))] ;; catches all errors
                    ) 
     (messup7)
     ))

  racket@test-macro-dev-error.rkt> (enter! "test-macro-dev-error.rkt")
    [re-loading /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt]
  a: identifier used out of context
    in: a
    errortrace...:
    context...:
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:434:2: errortrace-annotate
     /usr/share/racket/pkgs/errortrace-lib/errortrace/errortrace-lib.rkt:482:4
     /usr/share/racket/collects/racket/rerequire.rkt:18:0: rerequire
     /usr/share/racket/collects/racket/enter.rkt:54:0: dynamic-enter!6
     /usr/share/racket/collects/racket/private/misc.rkt:87:7
|#



#|
    (define-syntax (messup8 stx)
      (let* (
             [a 7]
             [b 8]
             )
        (quasisyntax/loc stx
          (+ #,a #,b (^ 3 4))
          )))


    racket@test-macro-dev-error.rkt> (messup8)
    ^: undefined;
     cannot reference an identifier before its definition
      in module: "/home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt"
      errortrace...:
       /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt:224:20: ^
       /home/deep/liquid-extensions/liquid/test-macro-dev-error.rkt:224:19: (^ 3 4)
       stdin::72893: (+ 7 8 (^ 3 4))
      context...:
       /usr/share/racket/collects/racket/private/misc.rkt:87:7
|#
