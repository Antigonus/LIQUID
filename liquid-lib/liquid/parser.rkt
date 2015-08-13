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
                   (eqv? t0-type (tk:punc))
                   (nd-attribute-is (at:lexeme) t0 "_")
                   )
                  (nd-make-parse ts (tk:pattern) 'rule-pattern)
                  ]
                [else
                  (rule-errsyn-expected ts imperative (tk:pattern) 'rule-pattern)
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
            '(tk:pattern ((at:source rule-pattern "test-session" (1 1 0) (2 1 1))))
            )
          ))

  
      (test-hook  rule-pattern-test-0)

    ;;; an operand
    ;;;   this rule appears to have a sole purpose of providing context for the recursive descent
    ;;;
      (define (rule-operand ts [imperative (imperative:test)])
        (cond
          [(null? ts) 
            (rule-errsyn-expected ts imperative (tk:operand) 'rule-operand)
            ]
          [else
            (let*(
                   [t0 (car ts)]
                   [t0-type (type t0)]
                   )
              (cond 
                [(or
                   (eqv? t0-type (tk:symbol))
                   (eqv? t0-type (tk:number))
                   (eqv? t0-type (tk:string))
                   )
                  (let*(
                         [tnew (nd-make-parse ts (tk:operand) 'rule-operand)]
                         [tret (nd-append-children tnew t0)]
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
                               [tnew (nd-make-parse ts (tk:operand) 'rule-operand)]
                               [tret (nd-append-children tnew pat)]
                               )
                          tret
                          )]
                      [else
                        (rule-errsyn-expected ts imperative (tk:operand) 'rule-operand)
                        ]))]))]))

    (define (rule-operand-test-0)
      (let*(
             [in (open-input-string "\"abc\"")]
             [ts (qp-lex in (current-file-name))]
             [the-parse (rule-operand ts)]
             )
        (.eq.
          the-parse
          '(tk:operand
             ((at:source rule-operand "test-session" (1 1 0) (6 1 5)))
             (tk:string
               ((at:source source-generator-lex "test-session" (1 1 0) (6 1 5))
                 (at:lexeme "\"abc\""))))
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
          '(tk:operand
             ((at:source rule-operand "test-session" (1 1 0) (2 1 1)))
             (tk:pattern
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
           (($tok (tk:symbol)) (car ts))
           (($tok (tk:paren-node)) (cadr ts)))

          (let*(
                 [separator (λ(t) (punc-is t ","))]
                 [ts-of-operand-list (nd-children (cadr ts))]
                 [item-list (framed-items-sep ts-of-operand-list separator)]
                 [operand-list (map (λ(ts) (rule-operand ts (imperative:force))) item-list)]
                 [new-t0  (nd-make-parse ts (tk:pred) 'rule-pred)]
                 [new-t1  (nd-append-children* new-t0 operand-list)]
                 [pname   (nd-lexeme (car ts))]
                 [at0     (attribute-make* (at:value) pname)]
                 [new-t2  (nd-ascribe-attribute new-t1 at0)]
                 )
            new-t2
            )
          ]
        [else  
          (rule-errsyn-expected ts imperative (tk:pred) 'rule-pred)
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
          '(tk:pred
             ((at:source rule-pred "test-session" (1 1 0) (11 1 10))
               (at:value "qed"))
             (tk:operand
               ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
               (tk:symbol
                 ((at:source source-generator-lex "test-session" (5 1 4) (6 1 5))
                   (at:lexeme "a"))))
             (tk:operand
               ((at:source rule-operand "test-session" (7 1 6) (8 1 7)))
               (tk:pattern
                 ((at:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
             (tk:operand
               ((at:source rule-operand "test-session" (9 1 8) (10 1 9)))
               (tk:symbol
                 ((at:source source-generator-lex "test-session" (9 1 8) (10 1 9))
                   (at:lexeme "c")))))
          )
        ))
    (test-hook rule-pred-test-0)

  ;; conjunctions of predicates
  ;;
  ;;
    (define (rule-conjunction ts  [imperative (imperative:test)]) 
      (cond
        [(null? ts)  (rule-errsyn-expected ts imperative (tk:conjunction) 'rule-conjunction)]
        [else
          (let*(
                 [separator (λ(t) (punc-is t "&"))]
                 [item-list (framed-items-sep ts separator)]
                 [pred-list (map (λ(ts) (rule-pred ts (imperative:force))) item-list)]
                 )
            (cond
              [(null? item-list) 
                (rule-errsyn-expected ts imperative (tk:conjunction) 'rule-conjunction)
                ]
              [else
                (let*(
                       [new-t0  (nd-make-parse ts (tk:conjunction) 'rule-conjunction)]
                       [new-t1  (nd-append-children* new-t0 pred-list)]
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
          '(tk:conjunction
             ((at:source rule-conjunction "test-session" (1 1 0) (57 1 56)))

             (tk:pred
               ((at:source rule-pred "test-session" (1 1 0) (57 1 56))
                 (at:value "qed"))
               (tk:operand
                 ((at:source rule-operand "test-session" (5 1 4) (6 1 5)))
                 (tk:symbol
                   ((at:source source-generator-lex "test-session" (5 1 4) (6 1 5))
                     (at:lexeme "a"))))
               (tk:operand
                 ((at:source rule-operand "test-session" (7 1 6) (8 1 7)))
                 (tk:pattern
                   ((at:source rule-pattern "test-session" (7 1 6) (8 1 7)))))
               (tk:operand
                 ((at:source rule-operand "test-session" (9 1 8) (10 1 9)))
                 (tk:symbol
                   ((at:source source-generator-lex "test-session" (9 1 8) (10 1 9))
                     (at:lexeme "c")))))

             (tk:pred
               ((at:source rule-pred "test-session" (12 1 11) (57 1 56))
                 (at:value "dlp"))
               (tk:operand
                 ((at:source rule-operand "test-session" (16 1 15) (19 1 18)))
                 (tk:string
                   ((at:source
                      source-generator-lex
                      "test-session"
                      (16 1 15)
                      (19 1 18))
                     (at:lexeme "\"a\""))))
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
                 ((at:source rule-operand "test-session" (24 1 23) (29 1 28)))
                 (tk:symbol
                   ((at:source
                      source-generator-lex
                      "test-session"
                      (24 1 23)
                      (29 1 28))
                     (at:lexeme "seven")))))

             (tk:pred
               ((at:source rule-pred "test-session" (33 1 32) (57 1 56))
                 (at:value "wikipedia"))
               (tk:operand
                 ((at:source rule-operand "test-session" (43 1 42) (56 1 55)))
                 (tk:symbol
                   ((at:source
                      source-generator-lex
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
        (.eq.
          the-parse
          '(tk:conjunction
             ((at:source rule-conjunction "test-session" (1 1 0) (29 1 28)))
             (tk:pred
                ((at:source rule-pred "test-session" (1 1 0) (29 1 28))
                  (at:value "who"))
                (tk:operand
                  ((at:source rule-operand "test-session" (5 1 4) (10 1 9)))
                  (tk:string
                    ((at:source source-generator-lex "test-session" (5 1 4) (10 1 9))
                      (at:lexeme "\"jim\""))))
                (tk:operand
                  ((at:source rule-operand "test-session" (12 1 11) (13 1 12)))
                  (tk:pattern
                    ((at:source rule-pattern "test-session" (12 1 11) (13 1 12))))))
             (tk:pred
               ((at:source rule-pred "test-session" (17 1 16) (29 1 28))
                 (at:value "writes"))
               (tk:operand
                 ((at:source rule-operand "test-session" (24 1 23) (25 1 24)))
                 (tk:symbol
                   ((at:source
                      source-generator-lex
                      "test-session"
                      (24 1 23)
                      (25 1 24))
                     (at:lexeme "x"))))
               (tk:operand
                 ((at:source rule-operand "test-session" (27 1 26) (28 1 27)))
                 (tk:symbol
                   ((at:source
                      source-generator-lex
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


