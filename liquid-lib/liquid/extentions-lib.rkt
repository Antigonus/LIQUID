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

  (require "strings-etc.rkt")
  (require (for-syntax "strings-etc.rkt"))

  (require "check.rkt")
  (require (for-syntax "check.rkt"))

;;--------------------------------------------------------------------------------
;; turns on printing of syntax programs ,etc.
;;    
  ;;(define debug #t)
  ;; (define-for-syntax debug #f)
  (define debug #f)
  (define-for-syntax debug #f)

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
;; bind a name index on to a sequence
;; 
  (define-syntax (name stx) 
    (let(
          [datum  (syntax->datum stx)]
          )
      (match-let(
                  [(list _  container names body ...) datum]
                  )
        (let(
              [program
                (Λ 'match-let (Λ
                               [Λ (cons 'list names) container]
                                )
                  ,body
                  )
                ]
              )
          (when debug (displayln (Λ "name -> " program)))
          (datum->syntax stx program)
          ))))

  (define (test-name-0)
    (define al '(1 2 3))
    (=
      (name al (one two three) (+ one two three))
      6
      ))


;;--------------------------------------------------------------------------------
;;  parallel cond, executes all that are true
;;
;;  else true clauses are concatenated and run, so they run in order of appearence in
;;  the cond.  would like to have cond** that uses threads instead
;;

  (define-syntax (cond* stx)
    (let*(
           [datum (syntax->datum stx)]
           [clauses (cdr datum)]
           [condition-clauses (filter (λ(e)(not (eqv? 'else e))) clauses)]
           [else-clauses      (filter (λ(e)(eqv? 'else e)) clauses)]
           [program
             `(let*(
                    [condition (λ(e)(eval (car e)))]
                    [transform (λ(e)(eval (cons 'begin (cdr e))))]
                    [results   (filter-fold condition transform ',condition-clauses)]
                    )
                (if (null? results)
                  (filter-fold (λ(e)#t) transform ',else-clauses)
                  results
                  ))]
           )
      (when debug (displayln (Λ  "cond* -> " program)))
      (datum->syntax stx program)
      ))


;; notice no test of else, and it don't work

    (define (cond*-test-0)
      (equal?
        (cond*
          [#t 1]
          [#f (+ 1 1)]
          [#t (+ 2 1)]
          )
        '(1 3)
        ))

    (define (cond*-test-1)
      (equal?
        (cond*
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
                (mc:define-1 source-location datum)
                (broken-syntax source-location (car datum))
                )]
            )
        (when debug (displayln (Λ "mc:define -> " program)))
        (datum->syntax stx program)
        )))

      
  (define-for-syntax (mc:define-1 source-location datum)
    ;; this match-let should not have errors because of the gaurd (check-mc:define) before the call
    (match-let*(
                 [(list _  mcr-fun-name mc-args mc-conts mcr-body ...) datum]
                 [(list  r-args-list-name r-args ...) mc-args]
                 [(list  r-conts-list-name r-conts ...) mc-conts]
                 )
      (let(
            [mc-args-length (-- (length mc-args))]  ; don't count the list name (first items)
            [mc-conts-length (-- (length mc-conts))] ; don't count the list name (first items)
            )
        (let(
              [program
                (Λ 'define (Λ mcr-fun-name r-args-list-name r-conts-list-name) 
                  (Λ 'define 'mc:source-location (Λ 'quote source-location)) ; programmer may use mc:source-location
                  (Λ 'if

                    (Λ 'check-mc:define-args-on-call "upon call" 'mc:source-location r-args-list-name r-conts-list-name mc-args-length mc-conts-length)
                    
                    (Λ 'name r-args-list-name r-args
                      (Λ 'name r-conts-list-name r-conts
                        ,mcr-body
                        ))

                    (Λ 'raise (Λ 'quote 'exception:mc:define-arity))
                    ))]
              )
          program
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

    (test-hook test-x-hash-ref-0)


;;--------------------------------------------------------------------------------
;; initialize module
;;    
  (define (ext:construct)
    (test-hook test-name-0)
    (test-hook cond*-test-0)
    (test-hook cond*-test-1)
    (test-hook begin-always-test-0)
    (test-hook begin-always-test-1)
    )
  (ext:construct)

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  (provide
    mc:define
    name
    cond*
    with-semaphore
    begin-always
    )

  ;; functions
  ;;
    (provide-with-trace "extentions-lib" ;; this context continues to the bottom of the page

      ;; fundamental functions
      ;;
        identity
        boolify  ;; (not #f) -> #t,  #f -> #f
        no-error ;; same as (not)

        be

        x-hash-ref
        arity-case

      )
