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

  (require "sequence.rkt")
  (require (for-syntax "sequence.rkt"))

  (require (for-syntax racket/match))


;;--------------------------------------------------------------------------------
;;
  (define-syntax (mc:λ stx) 
    (let*(
           [datum  (syntax->datum stx)]
           )
      (let(
            [program 
              (match-let(
                          [(list _  mc-args mc-conts mcr-body ...) datum]
                          )
                (mc:λ-1 5 mc-args mc-conts mcr-body)
                )]

            )
        (datum->syntax stx program)
        )))

      
  (define-for-syntax (mc:λ-1 source-location mc-args mc-conts mcr-body)
    ;; this match-let should not have errors because of the gaurd (check-mc:λ) before the call
    (match-let(
                [(list  r-args-list-moniker r-args ...) mc-args]
                [(list  r-conts-list-moniker r-conts ...) mc-conts]
                )
      (let(
            [mc-args-length (length mc-args)]  ; don't count the list moniker (first items)
            [mc-conts-length (length mc-conts)] ; don't count the list moniker (first items)
            )
        (let(
              [program
                (Λ 'λ (Λ r-args-list-moniker r-conts-list-moniker) 
                  (Λ 'define 'mc:source-location (Λ 'quote source-location)) ; programmer may use mc:source-location
                  (Λ 'if

                    #t
                    
                    (Λ 'moniker r-args-list-moniker r-args
                      (Λ 'moniker r-conts-list-moniker r-conts
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
    (define f (mc:λ (args x) (conts cont-odd cont-even) 12))
    #t
    )    

