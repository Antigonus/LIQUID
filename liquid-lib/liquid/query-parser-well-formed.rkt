#|
 grammar form checks
   there is some maintenence overhead here, as changing the gammar requires changing
   these checks.  However, the cdr of the code that uses the grammar has built in
   assumptions on form also, so changing the grammar requires changing all
   uses of it.  Hence, it makes sense to have a test run through here.

 created: 2015-01-05T04:40:45Z twl

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "lynch-lib.rkt")
  (require "tokens.rkt")
  (require "filter.rkt")
  (require "query-parser-tokens.rkt")
  (require "query-parser.rkt")

;;--------------------------------------------------------------------------------
;;
   (define (well-formed-operand op) ; should add more checks ..
     (and
       (eqv? (type op) (tok:operand))
       (length= (tok-children op) 1)
       (let*(
              [child (car (tok-children op))]
              [child-type (type child)]
              )
         (or
           (eqv? child-type (tok:symbol))
           (and 
             (eqv? child-type (tok:number))
             (length= (tok-value child) 1)
             )
           (eqv? child-type (tok:string))
           (eqv? child-type (tok:pattern))
           )
         )))

    (define (operand->string op)
      (cond
        [(not (well-formed-operand op)) #f]
        [else
          (let*(
                 [child (car (tok-children op))]
                 [child-type (type child)]
                 )
            (cond
              [(eqv? child-type (tok:symbol))  (car (tok-lexeme child))]
              [(eqv? child-type (tok:number))  (number->string (car (tok-value child)))]
              [(eqv? child-type (tok:string))  (car (tok-lexeme child))]
              [(eqv? child-type (tok:pattern))  "_"]
              ))
          ]
        ))

    (define test-ops-0
      '((tok:operand
          ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
          (tok:string
            ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
              (attribute:lexeme "\"Dali\""))))
         (tok:operand
           ((attribute:source rule-operand "test-session" (19 1 12) (14 1 19)))
           (tok:pattern
             ((attribute:source rule-pattern "test-session" (19 1 12) (14 1 19)))))
         (tok:operand
           ((attribute:source rule-operand "test-session" (5 1 4) (6 1 5)))
           (tok:symbol
             ((attribute:source source-generator-lex "test-session" (5 1 4) (6 1 5))
               (attribute:lexeme "a"))))
         (tok:operand
           ((attribute:source rule-operand "test-session" (21 1 20) (22 1 21)))
           (tok:number
             ((attribute:source
                source-generator-lex
                "test-session"
                (21 1 20)
                (22 1 21))
               (attribute:lexeme "3")
               (attribute:value 3))))
         (tok:operand
           ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
           (tok:string
             ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
               (attribute:lexeme "\"Salvadore\"")))))
      )

    (define (operand->string-test-0)
      (equal?
        (map operand->string test-ops-0)
        '("\"Dali\"" "_" "a" "3" "\"Salvadore\"")
        ))

    (define (well-formed-pred a-pred)
      (and
        (eqv? (type a-pred) (tok:pred))
        (andmap well-formed-operand (tok-children a-pred))
        ))

    (define (well-formed-pred-test-0)
      (let*(
             [parse/errors (query-parser* "qed(a,_,c)")]
             [parse (car parse/errors)] 
             [errors (cadr parse/errors)]
             [the-pred (car (tok-children parse))]; the parse result is a conjunction
             )
        ;(pretty-print parse)
        (well-formed-pred the-pred)
        ))
    (test-hook well-formed-pred-test-0)

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "query-parser-well-formed"
    well-formed-operand
    operand->string
    well-formed-pred
    )

