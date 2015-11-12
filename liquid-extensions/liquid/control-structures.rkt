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

  (require "sequence.rkt")
  (require (for-syntax "sequence.rkt"))

  (require (for-syntax racket/match))

  (require "strings-etc.rkt")
  (require (for-syntax "strings-etc.rkt"))

  (require "check.rkt")
  (require (for-syntax "check.rkt"))
  (provide (all-from-out "check.rkt"))

  (require "mc-lambda.rkt")
  (require (for-syntax "mc-lambda.rkt"))
  (provide (all-from-out "mc-lambda.rkt"))

;;--------------------------------------------------------------------------------
;; turns on printing of syntax programs ,etc.
;;    
  (define control-structures-debug (make-parameter #f))
  (provide control-structures-debug) 
  (define-for-syntax control-structures-debug (make-parameter #t))

;;--------------------------------------------------------------------------------
;; some utility functions
;;
  ;; void is already the do-nothing function as it will take args, e.g. (void 1 2 3)
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
;;  parallel cond, executes all that are true, if none are true, executes else clauses
;;  returns a list
;;
;;
  (define-syntax (cond* stx)
    (let(
          [datum (syntax->datum stx)]
          )
      (let(
            [seq-op (cadr datum)]
            [clauses (cddr datum)]
            )
        (let*(
               [cond-clauses (filter (λ(e)(not (eqv? 'else (car e)))) clauses)]
               [cond-clause-program
                 (for/list([clause cond-clauses])
                   (Λ 'when (car clause) ,(cdr clause))
                   )]
               [cond-clause-with-else-flag-program
                 (for/list([clause cond-clauses])
                   (Λ 'when (car clause) (Λ 'set! 'else-flag '#f) ,(cdr clause))
                   )]
               [else-clauses (filter (λ(e)(eqv? 'else (car e))) clauses)]
               [else-clause-program
                 (unwrap
                   (for/list([clause else-clauses])
                     (cdr clause)
                     ))]
               [program

                 (if (null? else-clause-program)

                   (Λ 'apply seq-op (Λ 'remove-void (cons 'Λ cond-clause-program)))

                   (Λ 'let
                     (Λ 
                       (Λ 'else-flag '#t)
                       )
                     (Λ 'apply seq-op (Λ 'remove-void (cons 'Λ cond-clause-with-else-flag-program)))
                     (Λ 'when 'else-flag
                       (Λ 'apply seq-op (Λ 'remove-void (cons 'Λ else-clause-program)))
                       ))
                   )

                 ]
               )        
          (when (control-structures-debug) (displayln (Λ  "cond* -> " program)))
          (datum->syntax stx program)
          ))))

    (define (cond*-test-0)
      (equal?
        (cond* list
          [#t 1]
          [#f (+ 1 1)]
          [#t (+ 2 1)]
          )
        '(1 3)
        ))

    (define (cond*-test-1)
      (equal?
        (cond* list
          [#f 1]
          [else 5]
          [#f (+ 1 1)]
          [else 7]
          [(= 1 3) (+ 2 1)]
          )
        '(5 7)
        ))

    (define (cond*-test-2)
      (equal?
        (cond* list
          [(odd? 3) 5]
          [(begin (+ 7 9) #f) (+ 11 13)]
          [#t (+ 15 17)]
        )
      '(5 32)
        ))


;;--------------------------------------------------------------------------------
;;  a with-semaphore block
;;
;;  (with-semaphore semaphore body ...) 
;;
  (define-syntax (with-semaphore stx)
    (syntax-case stx ()
      [(with-semaphore semaphore body ...)
        #`(begin-always
            (begin
              (semaphore-wait semaphore)
              body ...
              )
            (semaphore-post semaphore)
            )]))

;;--------------------------------------------------------------------------------
;; ruby style always block
;;
;;  form1 runs, then form2 runs.  The return value is from form1.
;;
;;  If form1 throws an exception, form2 still runs.  Then the exception is thrown.
;;
;;  If form1 throws  an exception, then form2 throws an exception, we thow the exception from
;;  form2.  I'm not sure this is the desired behavior, perhaps we should always throw the
;;  form1 exception.  What to do when two exceptions are in flight?  hmmm
;;
   (define-syntax (begin-always stx)
     (syntax-case stx ()
       [(begin-always form1 form2)
         #`(begin0
             (with-handlers
               (
                 [(λ(v) #t) ; this handler catches anything
                   (λ(v)
                     form2
                     (raise v)
                     )
                   ]
                 )
               form1
               )
             form2
             )
         ]
       ))

    (define (begin-always-test-0)
      (define x 3)
      (and
        (= 5 (begin-always 5 (set! x 7)))
        (= 7 x)
        ))

    (define (begin-always-test-1)
      (define x 2)
      (with-handlers (
                       [exn:fail:contract:divide-by-zero? (λ(v)(= x 4))]
                       )
        (begin-always
          (/ 1 0)
          (set! x 4)
          )
        #f
        ))


;;--------------------------------------------------------------------------------
;; define function with multiple continuations
;;
;;  (mc:define fun-name (arg-list-name arg ...) (cont-list-name cont...) body ...)
;;
;;  terminology can get confusing when writing a syntax transform function that transforms an mc function
;;  into a racket function ;-)   So we introduce some prefixs:
;;    mc-  these are the provided syntax input and syntax program local variables
;;    r-   these are parts of the output (which is a program for the reader)
;;
;;   I combine prefixes for things that apply to more than one domain
;;
;;  I've gone back to do the programmer the favor of reporting both static and dynamic
;;  argument form checks.  I've pulled the position information from the syntax so error
;;  messages can be printed in context.  This proved to be necessary for code stability.
;;
  (define-syntax (mc:define stx) 
    (let*(
           [datum  (syntax->datum stx)]
           [source-location
             (if (length≥ datum 2)
               (Λ (cadr datum) (syntax-source stx) (syntax-line stx) (syntax-column stx))
               (Λ (syntax-source stx) (syntax-line stx) (syntax-column stx))
               )]
           )
      (let(
            [program 
              (if (check-mc:define "upon expansion" source-location datum)
                (match-let(
                            [(list _  mcr-fun-name mc-args mc-conts mcr-body ...) datum]
                            )
                  (Λ 'define mcr-fun-name (Λ 'mc:λ mc-args mc-conts ,mcr-body))
                  )
                (broken-syntax source-location (car datum))
                )]
            )
        (when (control-structures-debug) (displayln (Λ "mc:define -> " program)))
        (datum->syntax stx program)
        )))

  (define (mc:define-test-0)
    (mc:define f (args x) (conts cont-odd cont-even)
                (if (odd? x) (cont-odd) (cont-even))
                )
    (mc:define g (args x) (conts cont-odd cont-even)
                (f args conts)
                )
    (and
      (f (Λ 3) (Λ (λ()#t) (λ()#f)))
      (not (g (Λ 4) (Λ (λ()#t) (λ()#f))))
      ))

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
      (cond
        [(not (hash? table))
          (displayln (Λ "x-hash-ref" mc:source-location "expected hash? for table, but found: " table))
          (raise 'exception:x-hash-ref-table)
          ]
        )
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


;;--------------------------------------------------------------------------------
;; initialize module
;;    
  (define (ext:init)
    (test-hook cond*-test-0)
    (test-hook cond*-test-1)
    (test-hook cond*-test-2)
    (test-hook begin-always-test-0)
    (test-hook begin-always-test-1)
    (test-hook mc:define-test-0)
    (test-hook test-x-hash-ref-0)
    )
  (ext:init)

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  (provide
    begin-always
    cond*
    mc:define
    with-semaphore
    )

  ;; functions
  ;;
    (provide-with-trace "control-structures" ;; this context continues to the bottom of the page

      ;; fundamental functions
      ;;
        identity
        boolify  ;; (not #f) -> #t,  #f -> #f
        no-error ;; same as (not)

        be

        x-hash-ref
        arity-case

      )
