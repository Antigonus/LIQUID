;;--------------------------------------------------------------------------------
;; parse query
;;   the query is a string entered by the user
;;   created: 2014-11-21T14:38:53Z twl

#lang racket


;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "lynch-lib.rkt")
  (require "tokens.rkt")
  (require "filter.rkt")
  (require "query-parser-tokens.rkt")
  (require "query-parser-lex.rkt")
  (require "query-parser-framing.rkt")

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
  ;;  input:  a token list the whole of which is matched
  ;;          an imparative flag saying whether it is an error not to match
  ;;  output: #f if the rule does not match,  or a token that is created by the rule
  ;;          the returned token may be marked with an error attribute if matching was forced


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
                   (eqv? t0-type (tok:punc))
                   (tok-attribute-is (attribute:lexeme) t0 "_")
                   )
                  (tok-make-parse ts (tok:pattern) 'rule-pattern)
                  ]
                [else
                  (rule-errsyn-expected ts imperative (tok:pattern) 'rule-pattern)
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
          (.eq.
            the-parse
            '(tok:pattern ((attribute:source rule-pattern "test-session" (1 1 0) (2 1 1))))
            )
          ))

  
      (test-hook  rule-pattern-test-0)

    ;;; an operand
    ;;;   this rule appears to have a sole purpose of providing context for the recursive descent
    ;;;
      (define (rule-operand ts [imperative (imperative:test)])
        (cond
          [(null? ts) 
            (rule-errsyn-expected ts imperative (tok:operand) 'rule-operand)
            ]
          [else
            (let*(
                   [t0 (car ts)]
                   [t0-type (type t0)]
                   )
              (cond 
                [(or
                   (eqv? t0-type (tok:symbol))
                   (eqv? t0-type (tok:number))
                   (eqv? t0-type (tok:string))
                   )
                  (let*(
                         [tnew (tok-make-parse ts (tok:operand) 'rule-operand)]
                         [tret (tok-append-children tnew t0)]
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
                               [tnew (tok-make-parse ts (tok:operand) 'rule-operand)]
                               [tret (tok-append-children tnew pat)]
                               )
                          tret
                          )]
                      [else
                        (rule-errsyn-expected ts imperative (tok:operand) 'rule-operand)
                        ]))]))]))

    (define (rule-operand-test-0)
      (let*(
             [in (open-input-string "\"abc\"")]
             [ts (qp-lex in (current-file-name))]
             [the-parse (rule-operand ts)]
             )
        (.eq.
          the-parse
          '(tok:operand
             ((attribute:source rule-operand "test-session" (1 1 0) (6 1 5)))
             (tok:string
               ((attribute:source source-generator-lex "test-session" (1 1 0) (6 1 5))
                 (attribute:lexeme "\"abc\""))))
          )
        ))
    (test-hook rule-operand-test-0)

    (define (rule-operand-test-1)
      (let* ([in (open-input-string "_")]
              [ts (qp-lex in (current-file-name))]
              [the-parse (rule-operand ts)]
              )
        (.eq.
          the-parse
          '(tok:operand
             ((attribute:source rule-operand "test-session" (1 1 0) (2 1 1)))
             (tok:pattern
               ((attribute:source rule-pattern "test-session" (1 1 0) (2 1 1)))))
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
           (($tok (tok:symbol)) (car ts))
           (($tok (tok:paren-node)) (cadr ts)))

          (let*(
                 [separator (λ(t) (punc-val-is t ","))]
                 [ts-of-operand-list (tok-children (cadr ts))]
                 [item-list (framed-items-sep ts-of-operand-list separator)]
                 [operand-list (map (λ(ts) (rule-operand ts (imperative:force))) item-list)]
                 [new-t0  (tok-make-parse ts (tok:pred) 'rule-pred)]
                 [new-t1  (tok-append-children* new-t0 operand-list)]
                 [pname   (tok-lexeme (car ts))]
                 [at0     (attribute-make* (attribute:value) pname)]
                 [new-t2  (tok-append-attributes new-t1 at0)]
                 )
            new-t2
            )
          ]
        [else  
          (rule-errsyn-expected ts imperative (tok:pred) 'rule-pred)
          ]
        ))

    (define (rule-pred-test-0)
      (let*(
             [in (open-input-string "qed(a,_,c)")]
             [ts (qp-lex in (current-file-name))]
             [framed-ts (framed-by-parens ts)]
             [the-parse (rule-pred framed-ts)]
             )
        (.eq.
          the-parse
          '(tok:pred
             ((attribute:source rule-pred "test-session" (1 1 0) (11 1 10))
               (attribute:value "qed"))
             (tok:operand
               ((attribute:source rule-operand "test-session" (5 1 4) (6 1 5)))
               (tok:symbol
                 ((attribute:source source-generator-lex "test-session" (5 1 4) (6 1 5))
                   (attribute:lexeme "a"))))
             (tok:operand
               ((attribute:source rule-operand "test-session" (7 1 6) (8 1 7)))
               (tok:pattern
                 ((attribute:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
             (tok:operand
               ((attribute:source rule-operand "test-session" (9 1 8) (10 1 9)))
               (tok:symbol
                 ((attribute:source source-generator-lex "test-session" (9 1 8) (10 1 9))
                   (attribute:lexeme "c")))))
          )
        ))
    (test-hook rule-pred-test-0)

  ;; conjunctions of predicates
  ;;
  ;;
    (define (rule-conjunction ts  [imperative (imperative:test)]) 
      (cond
        [(null? ts)  (rule-errsyn-expected ts imperative (tok:conjunction) 'rule-conjunction)]
        [else
          (let*(
                 [separator (λ(t) (punc-val-is t "&"))]
                 [item-list (framed-items-sep ts separator)]
                 [pred-list (map (λ(ts) (rule-pred ts (imperative:force))) item-list)]
                 )
            (cond
              [(null? item-list) 
                (rule-errsyn-expected ts imperative (tok:conjunction) 'rule-conjunction)
                ]
              [else
                (let*(
                       [new-t0  (tok-make-parse ts (tok:conjunction) 'rule-conjunction)]
                       [new-t1  (tok-append-children* new-t0 pred-list)]
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
        (.eq. the-parse
          '(tok:conjunction
             ((attribute:source rule-conjunction "test-session" (1 1 0) (57 1 56)))

             (tok:pred
               ((attribute:source rule-pred "test-session" (1 1 0) (57 1 56))
                 (attribute:value "qed"))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (5 1 4) (6 1 5)))
                 (tok:symbol
                   ((attribute:source source-generator-lex "test-session" (5 1 4) (6 1 5))
                     (attribute:lexeme "a"))))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (7 1 6) (8 1 7)))
                 (tok:pattern
                   ((attribute:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (9 1 8) (10 1 9)))
                 (tok:symbol
                   ((attribute:source source-generator-lex "test-session" (9 1 8) (10 1 9))
                     (attribute:lexeme "c")))))

             (tok:pred
               ((attribute:source rule-pred "test-session" (12 1 11) (57 1 56))
                 (attribute:value "dlp"))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (16 1 15) (19 1 18)))
                 (tok:string
                   ((attribute:source
                      source-generator-lex
                      "test-session"
                      (16 1 15)
                      (19 1 18))
                     (attribute:lexeme "\"a\""))))
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
                 ((attribute:source rule-operand "test-session" (24 1 23) (29 1 28)))
                 (tok:symbol
                   ((attribute:source
                      source-generator-lex
                      "test-session"
                      (24 1 23)
                      (29 1 28))
                     (attribute:lexeme "seven")))))

             (tok:pred
               ((attribute:source rule-pred "test-session" (33 1 32) (57 1 56))
                 (attribute:value "wikipedia"))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (43 1 42) (56 1 55)))
                 (tok:symbol
                   ((attribute:source
                      source-generator-lex
                      "test-session"
                      (43 1 42)
                      (56 1 55))
                     (attribute:lexeme "number_theory"))))))
          )))
    (test-hook rule-conjunction-test-0)

  ;;----------------------------------------
  ;; grammar rules 
  ;;
    (define (query-parser in)
      (let*(
             [ts (qp-lex in (current-file-name))]
             [framed-ts (framed-by-parens ts)]
             [the-parse (rule-conjunction framed-ts)] ; currently the top level must be a conjunction
             )
        the-parse
        ))

    (define (query-parser-test-0)
      (let*(
             [in (open-input-string "who(\"jim\", _) & writes(x, y)")]
             [the-parse (query-parser in)]
             )
        (.eq.
          the-parse
          '(tok:conjunction
             ((attribute:source rule-conjunction "test-session" (1 1 0) (29 1 28)))
             (tok:pred
                ((attribute:source rule-pred "test-session" (1 1 0) (29 1 28))
                  (attribute:value "who"))
                (tok:operand
                  ((attribute:source rule-operand "test-session" (5 1 4) (10 1 9)))
                  (tok:string
                    ((attribute:source source-generator-lex "test-session" (5 1 4) (10 1 9))
                      (attribute:lexeme "\"jim\""))))
                (tok:operand
                  ((attribute:source rule-operand "test-session" (12 1 11) (13 1 12)))
                  (tok:pattern
                    ((attribute:source rule-pattern "test-session" (12 1 11) (13 1 12))))))
             (tok:pred
               ((attribute:source rule-pred "test-session" (17 1 16) (29 1 28))
                 (attribute:value "writes"))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (24 1 23) (25 1 24)))
                 (tok:symbol
                   ((attribute:source
                      source-generator-lex
                      "test-session"
                      (24 1 23)
                      (25 1 24))
                     (attribute:lexeme "x"))))
               (tok:operand
                 ((attribute:source rule-operand "test-session" (27 1 26) (28 1 27)))
                 (tok:symbol
                   ((attribute:source
                      source-generator-lex
                      "test-session"
                      (27 1 26)
                      (28 1 27))
                     (attribute:lexeme "y"))))))
          )))
      (test-hook query-parser-test-0)


;;--------------------------------------------------------------------------------
;;   returns the query parse with a separate error report
;;
  (define (query-parser* the-query)
    (let*
      (
        [in (open-input-string the-query)]
        [parse (query-parser in)]
        [parse-errors (trim-tok-err parse)]
        )
      (list parse parse-errors)
      ))

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "query-parser"
    query-parser
    query-parser*
    )


