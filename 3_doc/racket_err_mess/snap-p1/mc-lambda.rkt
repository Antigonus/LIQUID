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
  (require (for-syntax racket/match))

  (require "test-lib.rkt")

  (require "arith-lib.rkt")
  (require (for-syntax "arith-lib.rkt"))

  (require "sequence.rkt")
  (require (for-syntax "sequence.rkt"))

  (require (for-syntax racket/match))

  (require "check.rkt")
  (require (for-syntax "check.rkt"))
  (provide (all-from-out "check.rkt"))

;;--------------------------------------------------------------------------------
;;
  (define mc:λ-debug (make-parameter #f))
  (define-for-syntax mc:λ-debug (make-parameter #f))

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
          (when (mc:λ-debug) (displayln (Λ "name -> " program)))
          (datum->syntax stx program)
          ))))

  (define (test-name-0)
    (define al '(1 2 3))
    (=
      (name al (one two three) (+ one two three))
      6
      ))


;;--------------------------------------------------------------------------------
;;

  (define-syntax (mc:λ stx) 
    (let*(
           [datum  (syntax->datum stx)]
           [source-location
             (Λ 'mc:λ (syntax-source stx) (syntax-line stx) (syntax-column stx))
             ]
           )
      (let(
            [program 
              (if (check-mc:λ "upon expansion" source-location datum)
                (match-let(
                            [(list _  mc-args mc-conts mcr-body ...) datum]
                            )
                  (mc:λ-1 source-location mc-args mc-conts mcr-body)
                  )
                (broken-syntax source-location (car datum))
                )]
            )
        (when (mc:λ-debug) (write (Λ "mc:λ -> " program)) (newline))
        (datum->syntax stx program)
        )))

      
  (define-for-syntax (mc:λ-1 source-location mc-args mc-conts mcr-body)
    ;; this match-let should not have errors because of the gaurd (check-mc:λ) before the call
    (match-let(
                [(list  r-args-list-name r-args ...) mc-args]
                [(list  r-conts-list-name r-conts ...) mc-conts]
                )
      (let(
            [mc-args-length (-- (length mc-args))]  ; don't count the list name (first items)
            [mc-conts-length (-- (length mc-conts))] ; don't count the list name (first items)
            )
        (let(
              [program
                (Λ 'λ (Λ r-args-list-name r-conts-list-name) 
                  (Λ 'define 'mc:source-location (Λ 'quote source-location)) ; programmer may use mc:source-location
                  (Λ 'if

                    (Λ 'check-args-on-call "mc:λ at runtime: " 'mc:source-location r-args-list-name r-conts-list-name mc-args-length mc-conts-length)
                    
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
;;
  (define (mc:λ-test-0)
    (define f (mc:λ (args x) (conts cont-odd cont-even)
                (if (odd? x) (cont-odd) (cont-even))
                ))
    (define g (mc:λ (args x) (conts cont-odd cont-even)
                (f args conts)
                ))
    (and
      (f (Λ 3) (Λ (λ()#t) (λ()#f)))
      (not (g (Λ 4) (Λ (λ()#t) (λ()#f))))
      ))

;;--------------------------------------------------------------------------------
;; initialize module
;;    
  (define (mc:λ:init)
    (test-hook test-name-0)
    (test-hook mc:λ-test-0)
    )
  (mc:λ:init)



;;--------------------------------------------------------------------------------
;;
  (provide
    name
    mc:λ
    )
