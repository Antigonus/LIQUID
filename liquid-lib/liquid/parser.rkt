;;--------------------------------------------------------------------------------
;; parse query
;;   the query is a string entered by the user
;;   created: 2014-11-21T14:38:53Z twl

#lang racket


;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "misc-lib.rkt")
  (require "node.rkt")
  (require "filter.rkt")
  (require "parser-nodes.rkt")
  (require "parser-lex.rkt")
  (require "parser-framing.rkt")

;;--------------------------------------------------------------------------------
;; test/debug
;;


  ;;----------------------------------------
  ;; grammar rules 
  ;;   these may be applied in one or more frame contexts
  ;;   actually, right now the grammar is very simple Q(x, ..) & Q(y, ...)  ...
  ;;   but we are ready for the bigger language!
  ;;
  ;;  for grammar rules
  ;;  input:  a node list the whole of which is matched
  ;;          an imparative flag saying whether it is an error not to match
  ;;  output: #f if the rule does not match,  or a node that is created by the rule
  ;;          the returned node may be marked with an error attribute if matching was forced


    ;;; Currently, when the operand to a  predicate is a single underscore '_' we consider this to be 
    ;;; a variable to be filled in by a query
    ;;;
    ;;; The underscore may also appear in identifiers, but not as the car character.
    ;;;
    ;;;
      (define (rule-pattern ts [imperative (imperative:test)])
        (cond 
          [(not (length= ts 1)) #f]
          [else
            (let*(
                   [t0 (car ts)]
                   [t0-type (type t0)]
                  )
              (cond
                [(and 
                   (eqv? t0-type (nd:punc))
                   (equal? (nd-lexeme-1 t0) (list "_"))
                   )
                  (nd-make-parse ts (ndql0:pattern) 'rule-pattern)
                  ]
                [else
                  (rule-errsyn-expected ts imperative (ndql0:pattern) 'rule-pattern)
                  ]
                ))
            ]
          ))

      (define (rule-pattern-test-0)
        (let*(
               [in (open-input-string "_")]
               [ts (qp-lex in (current-file-name))]
               [the-parse (rule-pattern ts)]
               )
          (nds-equal?
            the-parse
            '(ndql0:pattern ((at:source rule-pattern "test-session" (1 1 0) (2 1 1))))
            )
          ))

  
      (test-hook  rule-pattern-test-0)

    ;;; an operand
    ;;;   this rule appears to have a sole purpose of providing context for the recursive descent
    ;;;
      (define (rule-operand ts [imperative (imperative:test)])
        (cond
          [(null? ts) 
            (rule-errsyn-expected ts imperative (ndql0:operand) 'rule-operand)
            ]
          [else
            (let*(
                   [t0 (car ts)]
                   [t0-type (type t0)]
                   )
              (cond 
                [(or
                   (eqv? t0-type (nd:symbol))
                   (eqv? t0-type (nd:number))
                   (eqv? t0-type (nd:string))
                   )
                  (let*(
                         [tnew (nd-make-parse ts (ndql0:operand) 'rule-operand)]
                         [tret (on-children tnew bcons t0)]
                         )
                    tret
                    )]

                [else
                  (let(
                        [pat (rule-pattern ts)]
                        )
                    (cond 
                      [pat
                        (let*(
                               [tnew (nd-make-parse ts (ndql0:operand) 'rule-operand)]
                               [tret (on-children tnew bcons pat)]
                               )
                          tret
                          )]
                      [else
                        (rule-errsyn-expected ts imperative (ndql0:operand) 'rule-operand)
                        ]))]))]))

    (define (rule-operand-test-0)
      (let*(
             [in (open-input-string "\"abc\"")]
             [ts (qp-lex in (current-file-name))]
             [the-parse (rule-operand ts)]
             )
        (nds-equal?
          the-parse
          '(ndql0:operand
             ((at:source rule-operand "test-session" (1 1 0) (6 1 5)))
             (nd:string
               ((at:source (lexer-qpl0) "test-session" (1 1 0) (6 1 5))
                 (at:lexeme "\"abc\""))))
          )
        ))
    (test-hook rule-operand-test-0)

    (define (rule-operand-test-1)
      (let* ([in (open-input-string "_")]
              [ts (qp-lex in (current-file-name))]
              [the-parse (rule-operand ts)]
              )
        (nds-equal?
          the-parse
          '(ndql0:operand
             ((at:source rule-operand "test-session" (1 1 0) (2 1 1)))
             (ndql0:pattern
               ((at:source rule-pattern "test-session" (1 1 0) (2 1 1)))))
          )
        ))
    (test-hook rule-operand-test-1)


  ;;  pred(a,b,c)
  ;;
  ;;  the operand list parse is currently forced 
  ;;
    (define (rule-pred ts [imperative (imperative:test)])
      (cond
        [(and
           (length= ts 2)
           (($nd-type-is (nd:symbol)) (car ts))
           (($nd-type-is (ndql0:paren-node)) (cadr ts)))

          (let*(
                 [separator (λ(t) (punc-is t ","))]
                 [ts-of-operand-list (nd-children (cadr ts))]
                 [item-list (framed-items-sep ts-of-operand-list separator)]
                 [operand-list (map (λ(ts) (rule-operand ts (imperative:force))) item-list)]
                 [new-t0  (nd-make-parse ts (ndql0:pred) 'rule-pred)]
                 [new-t1  (on-children new-t0 append operand-list)]
                 [pname   (nd-lexeme (car ts))]
                 [at0     (attribute-make* (at:value) pname)]
                 [new-t2  (on-attributes new-t1 bcons at0)]
                 )
            new-t2
            )
          ]
        [else  
          (rule-errsyn-expected ts imperative (ndql0:pred) 'rule-pred)
          ]
        ))

    (define (rule-pred-test-0)
      (let*(
             [in (open-input-string "qed(a,_,c)")]
             [ts (qp-lex in (current-file-name))]
             [framed-ts (framed-by-parens ts)]
             [the-parse (rule-pred framed-ts)]
             )
        (nds-equal?
          the-parse
          '(ndql0:pred
             ((at:source rule-pred "test-session" (1 1 0) (11 1 10))
               (at:value "qed"))
             (ndql0:operand
               ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
               (nd:symbol
                 ((at:source (lexer-qpl0) "test-session" (5 1 4) (6 1 5))
                   (at:lexeme "a"))))
             (ndql0:operand
               ((at:source rule-operand "test-session" (7 1 6) (8 1 7)))
               (ndql0:pattern
                 ((at:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
             (ndql0:operand
               ((at:source rule-operand "test-session" (9 1 8) (10 1 9)))
               (nd:symbol
                 ((at:source (lexer-qpl0) "test-session" (9 1 8) (10 1 9))
                   (at:lexeme "c")))))
          )
        ))
    (test-hook rule-pred-test-0)

  ;; conjunctions of predicates
  ;;
  ;;
    (define (rule-conjunction ts  [imperative (imperative:test)]) 
      (cond
        [(null? ts)  (rule-errsyn-expected ts imperative (ndql0:conjunction) 'rule-conjunction)]
        [else
          (let*(
                 [separator (λ(t) (punc-is t "&"))]
                 [item-list (framed-items-sep ts separator)]
                 [pred-list (map (λ(ts) (rule-pred ts (imperative:force))) item-list)]
                 )
            (cond
              [(null? item-list) 
                (rule-errsyn-expected ts imperative (ndql0:conjunction) 'rule-conjunction)
                ]
              [else
                (let*(
                       [new-t0  (nd-make-parse ts (ndql0:conjunction) 'rule-conjunction)]
                       [new-t1  (on-children new-t0 append pred-list)]
                       )
                  new-t1
                  )
                ]))]))
    (define (rule-conjunction-test-0)
      (let*(
             [in (open-input-string "qed(a,_,c)&dlp(\"a\", 3, seven) & wikipedia(number_theory)")]
             [ts (qp-lex in (current-file-name))]
             [framed-ts (framed-by-parens ts)]
             [the-parse (rule-conjunction framed-ts)]
             )
        (nds-equal? the-parse
          '(ndql0:conjunction
             ((at:source rule-conjunction "test-session" (1 1 0) (57 1 56)))

             (ndql0:pred
               ((at:source rule-pred "test-session" (1 1 0) (57 1 56))
                 (at:value "qed"))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
                 (nd:symbol
                   ((at:source (lexer-qpl0) "test-session" (5 1 4) (6 1 5))
                     (at:lexeme "a"))))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (7 1 6) (8 1 7)))
                 (ndql0:pattern
                   ((at:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (9 1 8) (10 1 9)))
                 (nd:symbol
                   ((at:source (lexer-qpl0) "test-session" (9 1 8) (10 1 9))
                     (at:lexeme "c")))))

             (ndql0:pred
               ((at:source rule-pred "test-session" (12 1 11) (57 1 56))
                 (at:value "dlp"))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (16 1 15) (19 1 18)))
                 (nd:string
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (16 1 15)
                      (19 1 18))
                     (at:lexeme "\"a\""))))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (21 1 20) (22 1 21)))
                 (nd:number
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (21 1 20)
                      (22 1 21))
                     (at:lexeme "3")
                     (at:value 3))))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (24 1 23) (29 1 28)))
                 (nd:symbol
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (24 1 23)
                      (29 1 28))
                     (at:lexeme "seven")))))

             (ndql0:pred
               ((at:source rule-pred "test-session" (33 1 32) (57 1 56))
                 (at:value "wikipedia"))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (43 1 42) (56 1 55)))
                 (nd:symbol
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (43 1 42)
                      (56 1 55))
                     (at:lexeme "number_theory"))))))
          )))
    (test-hook rule-conjunction-test-0)

  ;;----------------------------------------
  ;; grammar rules 
  ;;
    (define (parser in)
      (let*(
             [ts (qp-lex in (current-file-name))]
             [framed-ts (framed-by-parens ts)]
             [the-parse (rule-conjunction framed-ts)] ; currently the top level must be a conjunction
             )
        the-parse
        ))

    (define (parser-test-0)
      (let*(
             [in (open-input-string "who(\"jim\", _) & writes(x, y)")]
             [the-parse (parser in)]
             )
        (nds-equal?
          the-parse
          '(ndql0:conjunction
             ((at:source rule-conjunction "test-session" (1 1 0) (29 1 28)))
             (ndql0:pred
                ((at:source rule-pred "test-session" (1 1 0) (29 1 28))
                  (at:value "who"))
                (ndql0:operand
                  ((at:source rule-operand "test-session" (5 1 4) (10 1 9)))
                  (nd:string
                    ((at:source (lexer-qpl0) "test-session" (5 1 4) (10 1 9))
                      (at:lexeme "\"jim\""))))
                (ndql0:operand
                  ((at:source rule-operand "test-session" (12 1 11) (13 1 12)))
                  (ndql0:pattern
                    ((at:source rule-pattern "test-session" (12 1 11) (13 1 12))))))
             (ndql0:pred
               ((at:source rule-pred "test-session" (17 1 16) (29 1 28))
                 (at:value "writes"))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (24 1 23) (25 1 24)))
                 (nd:symbol
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (24 1 23)
                      (25 1 24))
                     (at:lexeme "x"))))
               (ndql0:operand
                 ((at:source rule-operand "test-session" (27 1 26) (28 1 27)))
                 (nd:symbol
                   ((at:source
                      (lexer-qpl0)
                      "test-session"
                      (27 1 26)
                      (28 1 27))
                     (at:lexeme "y"))))))
          )))
      (test-hook parser-test-0)


;;--------------------------------------------------------------------------------
;;   returns the query parse with a separate error report
;;
  (define (parser* the-query)
    (let*
      (
        [in (open-input-string the-query)]
        [parse (parser in)]
        [parse-errors (trim-nd-err parse)]
        )
      (list parse parse-errors)
      ))

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "parser"
    parser
    parser*
    )


