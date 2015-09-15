#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require "test-lib.rkt")
  (require "arith-lib.rkt")
  (require (for-syntax "arith-lib.rkt"))
  (require "sequence-lib.rkt")
  (require (for-syntax "sequence-lib.rkt"))
  (require (for-syntax racket/match))

;;--------------------------------------------------------------------------------
;; some utility functions
;;
  (define (do-nothing . args) void) ; can be used to force trace to show a value
  (define identity values)
  (define (boolify b) (not (not b))) ; outputs either #t or #f
  (define no-error not) ; when returning #f means there are no errors, alleviates confusion

  ;; e.g. (be true) returns a function that returns true, etc:
  ;; the returned function igonres arguments
  ;; this is sometimes used to generate a continuation
  ;;
    (define (be . it) 
      (define (beit . args) (apply values it))
      beit)


;;--------------------------------------------------------------------------------
;; define function with multiple continuations
;;
;;  (mc:define fun-name (arg-list-name arg ...) (cont-list-name cont...) body ...)
;;
;;  terminology can get confusing when writing a syntax transform function that transforms an mc function
;;  into a racket function ;-)   So we introduce some prefixs:
;;    mc-  these are the provided syntax multiple continuation function definition parts
;;    r-   these are the generated syntax racket reader acceptable function definition parts
;;
;;   I combine prefixes for things that apply to more than one domain
;;
  (define-syntax (mc:define stx) 
    (let(
          [datum  (syntax->datum stx)]
          )
      (match-let*(
                   [(list _  mcr-fun-name mc-args mc-conts mcr-body ...) datum]
                   [(list  r-args-list-name r-args ...) mc-args]
                   [(list  r-conts-list-name r-conts ...) mc-conts]
                   )
        (let(
              [program
                (Λ 'define (Λ mcr-fun-name r-args-list-name r-conts-list-name)
                    (Λ 'match-let (Λ
                                    [Λ (cons 'list r-args) r-args-list-name]
                                    [Λ (cons 'list r-conts) r-conts-list-name]
                                    )
                      ,mcr-body
                    ))
                ]
              )
          ;;(displayln program)
          (datum->syntax stx program)
          ))))


;;--------------------------------------------------------------------------------
;;  wrappers and adaptors
;;

  ;; for functions that follow a convention of returning: an error codde,
  ;; no value, a single value, or multiple values
  ;; 
  ;; input: list  lambda-not-pair lamda-0 lamda-1 lambda-many
  ;;
    (define (arity-case l λ-not-pair λ0 λ1 λ-many)
      (cond
        [(not (pair? l)) (λ-not-pair l)]
        [(null? l)     (λ0)]
        [(length= l 1) (λ1 (car l))]
        [else          (λ-many  l)]
        ))


  ;; adds continuations to hash ref
  ;;    result is anamed output
  ;;
    (mc:define x-hash-ref (args table key) (conts continue-ok continue-key-not-found)
      (define ok #t)
      (define result (hash-ref table key (λ() (set! ok #f))))
      (cond
        [ok (continue-ok result)]
        [else (continue-key-not-found args conts)]
        ))

    (define (test-x-hash-ref-0)
      (equal? '(1 7)
        (let*(
               [h0 (make-hash)]
               )
          (hash-set! h0 'a 1)
          (Λ
            (x-hash-ref (Λ h0 'a) (Λ (λ(e) e) (λ(a c) 7)))
            (x-hash-ref (Λ h0 'b) (Λ (λ(e) e) (λ(a c) 7)))
            ))))

    (test-hook test-x-hash-ref-0)

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  ;; functions
  ;;
    (provide-with-trace "misc-lib" ;; this context continues to the bottom of the page

      ;; fundamental functions
      ;;
        do-nothing
        identity
        boolify  ;; (not #f) -> #t,  #f -> #f
        no-error ;; same as (not)

        be

        x-hash-ref
        arity-case

      )
