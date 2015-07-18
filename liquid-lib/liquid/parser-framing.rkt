#|
 parse query
   the query is a string entered by the user
   created: 2014-11-21T14:38:53Z twl
|#

#lang racket

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide framed-items-sep)
  (provide framed-by-parens)

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex)

  (require "lynch-lib.rkt")
  (require "tokens.rkt")
  (require "parser-tokens.rkt")
  (require "parser-lex.rkt")

;;--------------------------------------------------------------------------------
;; test/debug
;;


;;--------------------------------------------------------------------------------
;; framing
;; the framing abstraction brings some stability by constraining grammar parsing to cells
;;

  ;;|----------------------------------------
  ;;| 'items' framed by a separator (such as a comma or semicolon)
  ;;|
  ;;|  input token stream appears like this:
  ;;|      BOL item separator item separator .. EOL
  ;;|
  ;;| the separator must be a single token
  ;;| the item may be multiple tokens
  ;;|
  ;;| input: 
  ;;|     a token list
  ;;|     a single argument function that recognizes a separator token
  ;;|
  ;;|     state to support recursion
  ;;|        item-list, the list of separated items
  ;;|        item, the tokens gatherered so far to make the item
  ;;|        
  ;;| output: a list of items, where each item is a list of tokens
  ;;|
  ;;|
    (define (framed-items-sep ts separator)
      (define (framed-items-sep-0 ts item-list item)
        (cond 
          [(null? ts) 
            (cond
              [(null? item) item-list]
              [else (append item-list (list item))]
              )
            ]
          [else
            (let(
                  [t (car ts)]
                  [cdr-ts (cdr ts)]
                  )
              (cond
                [(and (null? item) (separator t))
                  (let* (
                          [a-tok (tok-make-errsyn ts (tok:errsyn) 'framed-items-sep-0 "expected item, found separator: ")]
                          [error-item (list a-tok)]
                          )
                    (framed-items-sep-0 cdr-ts (append item-list (list error-item)) '())
                    )]
                [(and (separator t) (null? cdr-ts))
                  (let* (
                          [a-tok (tok-make-errsyn ts (tok:errsyn) 'framed-items-sep-0 "list ended with a separator: ")]
                          [error-item (list a-tok)]
                          )
                    (framed-items-sep-0 cdr-ts (append item-list (list error-item)) '())
                    )]
                [(and (pair? item) (separator t))
                  (framed-items-sep-0 cdr-ts (append item-list (list item)) '())
                  ]
                [else ; in this case (separator t) must be false
                  (framed-items-sep-0 cdr-ts item-list (append item (list t)))
                  ]
                ))
            ]
          ))
        (framed-items-sep-0 ts '() '())
      )
      (define (framed-items-sep-test-0)
        (let*( 
               [in (open-input-string "1,2,3")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-val-is t ","))]
               [its (framed-items-sep ts sep)]
               [expected-its
                 `(
                  ((tok:number ((attribute:source source-generator-lex "test-session" (1 1 0) (2 1 1))(attribute:lexeme "1")(attribute:value 1))))
                  ((tok:number ((attribute:source source-generator-lex "test-session" (3 1 2) (4 1 3))(attribute:lexeme "2")(attribute:value 2))))
                  ((tok:number ((attribute:source source-generator-lex "test-session" (5 1 4) (6 1 5))(attribute:lexeme "3")(attribute:value 3))))
                  )
                 ]
               )
          (.eq. its expected-its)
          ))
      (test-hook framed-items-sep-test-0)

      (define (framed-items-sep-test-1)
        (let*( 
               [in (open-input-string "1 2, 3 4 5,,7")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-val-is t ","))]
               [its (framed-items-sep ts sep)]
               [expected-its 
                 '(((tok:number
                      ((attribute:source source-generator-lex "test-session" (1 1 0) (2 1 1))
                        (attribute:lexeme "1")
                        (attribute:value 1)))
                     (tok:number
                       ((attribute:source source-generator-lex "test-session" (3 1 2) (4 1 3))
                         (attribute:lexeme "2")
                         (attribute:value 2))))
                    ((tok:number
                       ((attribute:source source-generator-lex "test-session" (6 1 5) (7 1 6))
                         (attribute:lexeme "3")
                         (attribute:value 3)))
                      (tok:number
                        ((attribute:source source-generator-lex "test-session" (8 1 7) (9 1 8))
                          (attribute:lexeme "4")
                          (attribute:value 4)))
                      (tok:number
                        ((attribute:source source-generator-lex "test-session" (10 1 9) (11 1 10))
                          (attribute:lexeme "5")
                          (attribute:value 5))))
                    ((tok:errsyn
                       ((attribute:source framed-items-sep-0 "test-session" (12 1 11) (14 1 13))
                         (attribute:errsyn-mess "expected item, found separator: ")
                         (attribute:errsyn-ts
                           ((tok:punc
                              ((attribute:source
                                 source-generator-lex
                                 "test-session"
                                 (12 1 11)
                                 (13 1 12))
                                (attribute:lexeme ",")))
                             (tok:number
                               ((attribute:source
                                  source-generator-lex
                                  "test-session"
                                  (13 1 12)
                                  (14 1 13))
                                 (attribute:lexeme "7")
                                 (attribute:value 7))))))))
                    ((tok:number
                       ((attribute:source source-generator-lex "test-session" (13 1 12) (14 1 13))
                         (attribute:lexeme "7")
                         (attribute:value 7)))))
                 ]
               )
          (.eq. its expected-its)
          ))
      (test-hook framed-items-sep-test-1)

      (define (framed-items-sep-test-2)
        (let*( 
               [in (open-input-string "2 3, 3 4 5,,7")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-val-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap tok-has-err item)) its)] ; each item is a list
               )
          has-err
          ))
      (test-hook framed-items-sep-test-2)
      (define (framed-items-sep-test-3)
        (let*( 
               [in (open-input-string "3,")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-val-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap tok-has-err item)) its)] ; each item is a list
               )
          has-err
          ))
      (test-hook framed-items-sep-test-3)

      (define (framed-items-sep-test-4)
        (let*( 
               [in (open-input-string "3,4")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-val-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap tok-has-err item)) its)] ; each item is a list
               )
          (not has-err)
          ))
      (test-hook framed-items-sep-test-4)


  ;;----------------------------------------
  ;; token lists framed by parens
  ;;   
  ;;    this could be smarter in helping the user locate mismatched parents, but it is a start
  ;;
  ;; input: a token list
  ;; output: a tree of tokens, where nodes in the tree are (tok:paren-node)
  ;;
  ;; -currently only look at '('  and ')'
  ;;
    (define (open? t) (($tok-attribute (tok:punc) (attribute:lexeme) "(") t))
    (define (close? t) (($tok-attribute (tok:punc) (attribute:lexeme) ")") t))

    ;;;
    ;;;  input: a list of ts with balanced parens, outside parens (car and last of ts) are optional
    ;;;  output: a tree of ts with new paren nodes
    ;;;
    ;;;   if the input ts had outside parens, then we return a list containing just a new paren node
    ;;;   as the root of the tree
    ;;;
    ;;;   if the input ts were open on the ends, then we return a list of ts open on the ends, but
    ;;;   containing paren-node nodes where appropriate
    ;;;
    ;;;   if we reach a point that is not balanced we insert an errsyn token, further analaysis
    ;;;   could help find where the paren was missing - might help to tokenize the indention at
    ;;    line starts.
    ;;;
      (define (framed-by-parens ts)
        (cond 
          [(null? ts) '()]
          [else
            (let(
                  [t (car ts)]
                  [cdr-ts (cdr ts)]
                  )
              (cond
                [(open? t)
                  (let*(
                         [empty-paren-node       (tok-make-parse cdr-ts (tok:paren-node) 'framed-by-parens)]
                         [paren-node-and-tail-ts (framed-by-parens-open cdr-ts empty-paren-node)]
                         [paren-node             (car paren-node-and-tail-ts)]
                         [tail-ts                (cadr paren-node-and-tail-ts)]
                         )
                    (cons paren-node (framed-by-parens tail-ts)))
                  ]
                [(close? t) ;; this is one of two places we might learn of unmatched parens (other in framed-by-parens-open null case)
                  (let*(
                         [err-tok      (tok-make-parse (list t) (tok:errsyn) 'framed-by-parens)]
                         [mess         (attribute-make (attribute:errsyn-mess) "unexpected ')'")]
                         [err-tok-mess (tok-append-attribute err-tok mess)]
                         )
                    (cons err-tok (framed-by-parens cdr-ts)))
                  ]
                [else
                  (cons t (framed-by-parens cdr-ts))
                  ]
                ))
            ]
          ))

    ;;; helper function for framed-by-parens
    ;;;
    ;;;  input:  a list of ts that follows an open paren, we are now looking for the closing
    ;;;  output:  the completed paren-node, unprocessed tail ts
    ;;;
      (define (framed-by-parens-open ts paren-node)
        (cond 
          [(null? ts) ; oops we didn't get to see the closing paren
            (list 
              (append-errsyn '() paren-node "paren still open at end of stream") 
              '()
              )
            ]
          [else
            (let(
                  [t (car ts)]
                  [cdr-ts (cdr ts)]
                  )
              (cond
                [(open? t)
                  (let*(
                         [new-paren-node         (tok-make-parse cdr-ts (tok:paren-node) 'framed-by-parens-open)]
                         [descend-paren-and-tail (framed-by-parens-open cdr-ts new-paren-node)]
                         [descend-paren          (car descend-paren-and-tail)]
                         [descend-tail           (cadr descend-paren-and-tail)]
                         [updated-paren          (tok-append-child paren-node descend-paren)]
                         )
                    (framed-by-parens-open descend-tail updated-paren))
                  ]
                [(close? t)
                  (list paren-node cdr-ts)
                  ]
                [else
                  (let*(
                         [updated-paren  (tok-append-child paren-node t)]
                         )
                    (framed-by-parens-open cdr-ts updated-paren))
                  ]
                ))
            ]
          ))


      (define (framed-by-parens-test-0)
        (let*( 
               [in (open-input-string "(10  (21 22 23) 30)")]
               [ts (qp-lex in (current-file-name))]
               [fts (framed-by-parens ts)]
               )
          (.eq. fts
            '((tok:paren-node
                ((attribute:source framed-by-parens "test-session" (2 1 1) (20 1 19)))
                (tok:number
                  ((attribute:source source-generator-lex "test-session" (2 1 1) (4 1 3))
                    (attribute:lexeme "10")
                    (attribute:value 10)))
                (tok:paren-node
                  ((attribute:source framed-by-parens-open "test-session" (7 1 6) (20 1 19)))
                  (tok:number
                    ((attribute:source source-generator-lex "test-session" (7 1 6) (9 1 8))
                      (attribute:lexeme "21")
                      (attribute:value 21)))
                  (tok:number
                    ((attribute:source source-generator-lex "test-session" (10 1 9) (12 1 11))
                      (attribute:lexeme "22")
                      (attribute:value 22)))
                  (tok:number
                    ((attribute:source
                       source-generator-lex
                       "test-session"
                       (13 1 12)
                       (15 1 14))
                      (attribute:lexeme "23")
                      (attribute:value 23))))
                (tok:number
                  ((attribute:source source-generator-lex "test-session" (17 1 16) (19 1 18))
                    (attribute:lexeme "30")
                    (attribute:value 30))))))
          ))
        (test-hook framed-by-parens-test-0)

