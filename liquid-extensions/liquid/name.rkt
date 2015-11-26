#|
   


|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require errortrace)

  (require racket/match)
  (require (for-syntax racket/match))

  (require syntax/location)
  (require (for-syntax syntax/location))

  (require syntax/srcloc)
  (require (for-syntax syntax/srcloc))

  (require "test-lib.rkt")

  (require "sequence.rkt")
  (require (for-syntax "sequence.rkt"))

  (require (for-syntax "location.rkt"))



;;--------------------------------------------------------------------------------
;;
  (define name-debug (make-parameter #f))
  (define-for-syntax name-debug (make-parameter #f))

;;--------------------------------------------------------------------------------
;; bind a name index on to a sequence
;;
;;  if the last name follows a period it is assigned to the rest of the 
;;  of the container
;;
  (define-syntax (name stx)
    (let(
          [datum (syntax->datum stx)]
          [call-loc (build-source-location stx)]
          )
      (match datum
        [(list _ a-list  (list n ...) body ...)
          (let(
                [program `(apply (λ ,n ,@body) ,a-list)]
                )
            (when name-debug (write (Λ "name-> " program))(newline))
            #'program
            )]
        [_
          (raise-syntax-error 
            #f
            "expected: (name a-list (Λ n ..) body ..)"
            stx
            )]
          )))
        


  (define (test-name-0)
    (define al '(1 2 3))
    (=
      (name al (one two three) (+ one two three))
      6
      ))
  (test-hook test-name-0)

  (define (test-name-1)
    (define a1 '(1 2 3))
    (equal?
      '(1 2 3)
      (name a1 (one two ...) (Λ one ,two))
      ))
  (test-hook test-name-1)


;;--------------------------------------------------------------------------------
;;
  (provide
    name
    )
