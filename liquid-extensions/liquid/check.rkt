#|
  created 2015-09-30 twl

  this was made a separate module from extensions-lib so as to make it simple
  to require it both directly and for syntax

  these are intended to be used by syntax transformers for doing both static and dynamic
  operand checks.  The dynamic (run time) checks are sometimes deemed worth the
  performance penalty because the native racket errors that result (such as match-let not
  having an option) are cryptic and sometimes without source location infomration.  That
  combined with the newness/bugginess of the library was leading to long debug times.


|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require "sequence.rkt")
  (require "strings-etc.rkt")


;;--------------------------------------------------------------------------------
;;

  ;; if pred is false, print a message
  ;;
    (define (check pred message source-location item)
      (define p (pred))
      (unless p
        (displayln (Λ "## " "at " source-location))
        (displayln (Λ "-- " message))
        (displayln (Λ "-- " "for: " item))
        )
      p
      )


  ;; pred checks operand is a list of the epxected length
  ;;
    (define (check-length= source-location a-list length-expected)
      (check
        (λ () (and (pair? a-list)  (length= a-list length-expected)))
        (string-append "expected length of " (->string length-expected) " ")
        source-location
        a-list
        ))

  ;; pred checks operand is a list of at least the specified length
  ;;
    (define (check-length≥ source-location a-list length-at-least [preamble ""])
      (check
        (λ () (and (pair? a-list)  (length≥ a-list length-at-least)))
        (string-append preamble "expected length to be at least " (->string length-at-least) " ")
        source-location
        a-list
        ))

  ;; generic arity test
  ;;
  ;;  (define (check-arity source-location mc-fun-name length-args length-conts)
  ;; this is already build into mc:define ..


;;--------------------------------------------------------------------------------
;;
;; generates place holder program for bad syntax
;;
    (define (broken-syntax source-location transformer-name)
      (Λ 'begin
        (Λ 'displayln (Λ transformer-name " at " source-location " had syntax errors"))
        (Λ 'raise (Λ 'quote 'exception:broken-syntax))
      ))

;;--------------------------------------------------------------------------------
;; mc:define
;;
  ;; the arg check for the syntax transformation
  ;;.. spits out redundent error message, should be passing a mess down to check instead ...
  ;;
    (define (check-mc:define mess source-location datum)
      (define check-result
        (and
          (check-length≥  source-location datum 4);
          (and-form ; no short circuit because we want all the error messages
            (check-length≥  source-location (caddr datum) 1) ; must contain a name for the arg list, even if it is empty
            (check-length≥  source-location (cadddr datum) 1) ; must contain a name for the cont list
            )
          ))

      (unless check-result
        (displayln (Λ "at syntax expansion " source-location mess " expected (mc:define fun-name (arg-list-name arg ...) (cont-list-name cont...) body ...)"))
        (displayln (Λ "but found: " datum))
        )

      check-result
      )

  ;; the run time arg check
  ;;
    (define (check-mc:define-args-on-call mess source-location given-args given-conts expected-args-length  expected-conts-length)
      (let(
            [args-check (check-length=  source-location given-args expected-args-length)]
            [conts-check (check-length=  source-location given-conts expected-conts-length)]
            )
        (define check-result (and args-check conts-check))
        (unless check-result (displayln (Λ "## " "at " source-location mess " arity problem")))
        (unless args-check (displayln (Λ "-- " "expected length " expected-args-length " for args, but found: " given-args)))
        (unless conts-check (displayln (Λ "-- " "expected length " expected-args-length " for conts, but found: " given-conts)))

        check-result
        ))

;;--------------------------------------------------------------------------------
;;

(provide-with-trace "check"
  check
  check-length=
  check-length≥
  broken-syntax
  check-mc:define
  check-mc:define-args-on-call
  )

