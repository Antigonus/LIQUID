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
       (eqv? (type op) (ndql0:operand))
       (length= (nd-children op) 1)
       (let*(
              [child (car (nd-children op))]
              [child-type (type child)]
              )
         (or
           (eqv? child-type (nd:symbol))
           (and 
             (eqv? child-type (nd:number))
             (length= (nd-value child) 1)
             )
           (eqv? child-type (nd:string))
           (eqv? child-type (ndql0:pattern))
           )
         )))

    (define (operand->string op)
      (cond
        [(not (well-formed-operand op)) #f]
        [else
          (let*(
                 [child (car (nd-children op))]
                 [child-type (type child)]
                 )
            (cond
              [(eqv? child-type (nd:symbol))  (car (nd-lexeme child))]
              [(eqv? child-type (nd:number))  (number->string (car (nd-value child)))]
              [(eqv? child-type (nd:string))  (car (nd-lexeme child))]
              [(eqv? child-type (ndql0:pattern))  "_"]
              ))
          ]
        ))

    (define test-ops-0
      '((ndql0:operand
          ((at:source rule-operand "test-session" (6 1 5) (12 1 11)))
          (nd:string
            ((at:source (lexer-ql0-name) "test-session" (6 1 5) (12 1 11))
              (at:lexeme "\"Dali\""))))
         (ndql0:operand
           ((at:source rule-operand "test-session" (19 1 12) (14 1 19)))
           (ndql0:pattern
             ((at:source rule-pattern "test-session" (19 1 12) (14 1 19)))))
         (ndql0:operand
           ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
           (nd:symbol
             ((at:source (lexer-ql0-name) "test-session" (5 1 4) (6 1 5))
               (at:lexeme "a"))))
         (ndql0:operand
           ((at:source rule-operand "test-session" (21 1 20) (22 1 21)))
           (nd:number
             ((at:source
                (lexer-ql0-name)
                "test-session"
                (21 1 20)
                (22 1 21))
               (at:lexeme "3")
               (at:value 3))))
         (ndql0:operand
           ((at:source rule-operand "test-session" (6 1 5) (12 1 11)))
           (nd:string
             ((at:source (lexer-ql0-name) "test-session" (6 1 5) (12 1 11))
               (at:lexeme "\"Salvadore\"")))))
      )

    (define (operand->string-test-0)
      (equal?
        (map operand->string test-ops-0)
        '("\"Dali\"" "_" "a" "3" "\"Salvadore\"")
        ))

    (define (well-formed-pred a-pred)
      (and
        (eqv? (type a-pred) (ndql0:pred))
        (andmap well-formed-operand (nd-children a-pred))
        ))

    (define (well-formed-pred-test-0)
      (let*(
             [parse/errors (parser* "qed(a,_,c)")]
             [parse (car parse/errors)] 
             [errors (cadr parse/errors)]
             [the-pred (car (nd-children parse))]; the parse result is a conjunction
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

