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
  (require "misc-lib.rkt")
  (require "node.rkt")
  (require "filter.rkt")
  (require "parser-nodes.rkt")
  (require "parser.rkt")

;;--------------------------------------------------------------------------------
;;
   (define (well-formed-operand op) ; should add more checks ..
     (and
       (eqv? (type op) (tk:operand))
       (length= (ndchildren op) 1)
       (let*(
              [child (car (ndchildren op))]
              [child-type (type child)]
              )
         (or
           (eqv? child-type (tk:symbol))
           (and 
             (eqv? child-type (tk:number))
             (length= (ndvalue child) 1)
             )
           (eqv? child-type (tk:string))
           (eqv? child-type (tk:pattern))
           )
         )))

    (define (operand->string op)
      (cond
        [(not (well-formed-operand op)) #f]
        [else
          (let*(
                 [child (car (ndchildren op))]
                 [child-type (type child)]
                 )
            (cond
              [(eqv? child-type (tk:symbol))  (car (ndlexeme child))]
              [(eqv? child-type (tk:number))  (number->string (car (ndvalue child)))]
              [(eqv? child-type (tk:string))  (car (ndlexeme child))]
              [(eqv? child-type (tk:pattern))  "_"]
              ))
          ]
        ))

    (define test-ops-0
      '((tk:operand
          ((at:source rule-operand "test-session" (6 1 5) (12 1 11)))
          (tk:string
            ((at:source source-generator-lex "test-session" (6 1 5) (12 1 11))
              (at:lexeme "\"Dali\""))))
         (tk:operand
           ((at:source rule-operand "test-session" (19 1 12) (14 1 19)))
           (tk:pattern
             ((at:source rule-pattern "test-session" (19 1 12) (14 1 19)))))
         (tk:operand
           ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
           (tk:symbol
             ((at:source source-generator-lex "test-session" (5 1 4) (6 1 5))
               (at:lexeme "a"))))
         (tk:operand
           ((at:source rule-operand "test-session" (21 1 20) (22 1 21)))
           (tk:number
             ((at:source
                source-generator-lex
                "test-session"
                (21 1 20)
                (22 1 21))
               (at:lexeme "3")
               (at:value 3))))
         (tk:operand
           ((at:source rule-operand "test-session" (6 1 5) (12 1 11)))
           (tk:string
             ((at:source source-generator-lex "test-session" (6 1 5) (12 1 11))
               (at:lexeme "\"Salvadore\"")))))
      )

    (define (operand->string-test-0)
      (equal?
        (map operand->string test-ops-0)
        '("\"Dali\"" "_" "a" "3" "\"Salvadore\"")
        ))

    (define (well-formed-pred a-pred)
      (and
        (eqv? (type a-pred) (tk:pred))
        (andmap well-formed-operand (ndchildren a-pred))
        ))

    (define (well-formed-pred-test-0)
      (let*(
             [parse/errors (parser* "qed(a,_,c)")]
             [parse (car parse/errors)] 
             [errors (cadr parse/errors)]
             [the-pred (car (ndchildren parse))]; the parse result is a conjunction
             )
        ;(pretty-print parse)
        (well-formed-pred the-pred)
        ))
    (test-hook well-formed-pred-test-0)

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "parser-well-formed"
    well-formed-operand
    operand->string
    well-formed-pred
    )

