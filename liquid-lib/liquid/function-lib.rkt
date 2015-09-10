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
  (require "list-lib.rkt")

;;--------------------------------------------------------------------------------
;; some utility functions
;;
  (define (do-nothing . args) void) ; can be used to force trace to show a value
  (define identity values)
  (define (boolify b) (not (not b))) ; outputs either #t or #f
  (define no-error not) ; useful when testing returning exception values on fail

  ;; e.g. (be true) returns a function that returns true, etc:
  ;; the returned function igonres arguments
  ;; this is sometimes used to generate a continuation
  ;;
    (define (be . it) 
      (define (beit . args) (apply values it))
      beit)


;;--------------------------------------------------------------------------------
;;  multiple continuations function support
;;
;;

    (define-syntax (dfc stx)
    (let(
           [datum  (syntax->datum stx)]
          )
      (let(
            [name    (  cadr datum)]
            [args    ( caddr datum)]
            [conts   (cadddr datum)]  
            [body    (cddddr datum)]    
            )
        #|
        (displayln name)
        (displayln args)
        (displayln conts)
        (displayln body)
        |#
        (let(
              [code-block `(define)]
              [lambda-block '(λ)]
              [replacement-table (for/list ([c conts]) (pair c args))]
              )
          #|
          (displayln code-block)
          (displayln lambda-block)
          |#
          (let*(
                 [program 
                   (append
                     code-block
                     (Λ name)
                     (append lamda-block
                       (Λ args)
                       )
                     )
                   ]
                 )
            ;;(displayln program)
            (datum->syntax stx program)
            ))))))





;;--------------------------------------------------------------------------------
;;  wrappers and adaptors
;;

   ;; This is used with functions that return a list.  It chooses different
   ;; continuations based on the number of items in the list.
   ;; 
   ;; input: list  lamda-0 lamda-1 lambda-many
   ;;
   ;; calls λ-no-list if there is no list
   ;; calls λ0 with no arguments if the list is empty,
   ;; calls λ1 with first element in the list, if the list is length 1
   ;; otherwise calls λ-many with the list
   ;;
     (define (by-arity l λ0 λ1 λ-many [λ-no-list identity])
       (cond
         [(not (pair? l)) (λ-no-list l)]
         [(null? l)     (λ0)]
         [(length= l 1) (λ1 (car l))]
         [else          (λ-many  l)]
         ))

  ;; adds continuations to hash ref
  ;;
    (define (x-hash-ref table field continue-ok continue-no-field)
      (define ok #t)
      (define value (hash-ref table field (λ() (set! ok #f))))
      (cond
        [ok (continue-ok value)]
        [else (continue-no-field table field continue-ok)]
          ))




;;--------------------------------------------------------------------------------
;; provides the following
;;    

  ;; functions
  ;;
    (provide-with-trace "misc-lib" ;; this context continues to the bottom of the page

      ;; arithmetic
      ;;
        ≠
        ≥
        ≤
        ++
        --

      ;; fundamental functions
      ;;
        do-nothing
        identity
        boolify  ;; (not #f) -> #t,  #f -> #f
        no-error ;; same as (not)

        be

        and-form
        and-form*
        or-form
        or-form*

        x-hash-ref
        by-arity

      )
