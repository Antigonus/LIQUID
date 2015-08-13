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

  (require "misc-lib.rkt")
  (require "node.rkt")
  (require "parser-nodes.rkt")
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
  ;;|  input node stream appears like this:
  ;;|      BOL item separator item separator .. EOL
  ;;|
  ;;| the separator must be a single node
  ;;| the item may be multiple nodes
  ;;|
  ;;| input: 
  ;;|     a node list
  ;;|     a single argument function that recognizes a separator node
  ;;|
  ;;|     state to support recursion
  ;;|        item-list, the list of separated items
  ;;|        item, the nodes gatherered so far to make the item
  ;;|        
  ;;| output: a list of items, where each item is a list of nodes
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
                          [a-tok (nd-make-errsyn ts (tk:errsyn) 'framed-items-sep-0 "expected item, found separator: ")]
                          [error-item (list a-tok)]
                          )
                    (framed-items-sep-0 cdr-ts (append item-list (list error-item)) '())
                    )]
                [(and (separator t) (null? cdr-ts))
                  (let* (
                          [a-tok (nd-make-errsyn ts (tk:errsyn) 'framed-items-sep-0 "list ended with a separator: ")]
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
               [sep (λ(t) (punc-is t ","))]
               [its (framed-items-sep ts sep)]
               [expected-its
                 `(
                  ((tk:number ((at:source source-generator-lex "test-session" (1 1 0) (2 1 1))(at:lexeme "1")(at:value 1))))
                  ((tk:number ((at:source source-generator-lex "test-session" (3 1 2) (4 1 3))(at:lexeme "2")(at:value 2))))
                  ((tk:number ((at:source source-generator-lex "test-session" (5 1 4) (6 1 5))(at:lexeme "3")(at:value 3))))
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
               [sep (λ(t) (punc-is t ","))]
               [its (framed-items-sep ts sep)]
               [expected-its 
                 '(((tk:number
                      ((at:source source-generator-lex "test-session" (1 1 0) (2 1 1))
                        (at:lexeme "1")
                        (at:value 1)))
                     (tk:number
                       ((at:source source-generator-lex "test-session" (3 1 2) (4 1 3))
                         (at:lexeme "2")
                         (at:value 2))))
                    ((tk:number
                       ((at:source source-generator-lex "test-session" (6 1 5) (7 1 6))
                         (at:lexeme "3")
                         (at:value 3)))
                      (tk:number
                        ((at:source source-generator-lex "test-session" (8 1 7) (9 1 8))
                          (at:lexeme "4")
                          (at:value 4)))
                      (tk:number
                        ((at:source source-generator-lex "test-session" (10 1 9) (11 1 10))
                          (at:lexeme "5")
                          (at:value 5))))
                    ((tk:errsyn
                       ((at:source framed-items-sep-0 "test-session" (12 1 11) (14 1 13))
                         (attribute:errsyn-mess "expected item, found separator: ")
                         (attribute:errsyn-nds
                           ((tk:punc
                              ((at:source
                                 source-generator-lex
                                 "test-session"
                                 (12 1 11)
                                 (13 1 12))
                                (at:lexeme ",")))
                             (tk:number
                               ((at:source
                                  source-generator-lex
                                  "test-session"
                                  (13 1 12)
                                  (14 1 13))
                                 (at:lexeme "7")
                                 (at:value 7))))))))
                    ((tk:number
                       ((at:source source-generator-lex "test-session" (13 1 12) (14 1 13))
                         (at:lexeme "7")
                         (at:value 7)))))
                 ]
               )
          (.eq. its expected-its)
          ))
      (test-hook framed-items-sep-test-1)

      (define (framed-items-sep-test-2)
        (let*( 
               [in (open-input-string "2 3, 3 4 5,,7")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap nd-has-err item)) its)] ; each item is a list
               )
          has-err
          ))
      (test-hook framed-items-sep-test-2)
      (define (framed-items-sep-test-3)
        (let*( 
               [in (open-input-string "3,")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap nd-has-err item)) its)] ; each item is a list
               )
          has-err
          ))
      (test-hook framed-items-sep-test-3)

      (define (framed-items-sep-test-4)
        (let*( 
               [in (open-input-string "3,4")]
               [ts (qp-lex in (current-file-name))]
               [sep (λ(t) (punc-is t ","))]
               [its (framed-items-sep ts sep)]
               [has-err (ormap (λ(item) (ormap nd-has-err item)) its)] ; each item is a list
               )
          (not has-err)
          ))
      (test-hook framed-items-sep-test-4)


  ;;----------------------------------------
  ;; node lists framed by parens
  ;;   
  ;;    this could be smarter in helping the user locate mismatched parents, but it is a start
  ;;
  ;; input: a node list
  ;; output: a tree of nodes, where nodes in the tree are (tk:paren-node)
  ;;
  ;; -currently only look at '('  and ')'
  ;;
    (define (open? t) (($ndattribute (tk:punc) (at:lexeme) "(") t))
    (define (close? t) (($ndattribute (tk:punc) (at:lexeme) ")") t))

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
    ;;;   if we reach a point that is not balanced we insert an errsyn node, further analaysis
    ;;;   could help find where the paren was missing - might help to nodeize the indention at
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
                         [empty-paren-node       (nd-make-parse cdr-ts (tk:paren-node) 'framed-by-parens)]
                         [paren-node-and-tail-ts (framed-by-parens-open cdr-ts empty-paren-node)]
                         [paren-node             (car paren-node-and-tail-ts)]
                         [tail-ts                (cadr paren-node-and-tail-ts)]
                         )
                    (cons paren-node (framed-by-parens tail-ts)))
                  ]
                [(close? t) ;; this is one of two places we might learn of unmatched parens (other in framed-by-parens-open null case)
                  (let*(
                         [err-tok      (nd-make-parse (list t) (tk:errsyn) 'framed-by-parens)]
                         [mess         (attribute-make (attribute:errsyn-mess) "unexpected ')'")]
                         [err-nd-mess (nd-ascribe-attribute err-tok mess)]
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
                         [new-paren-node         (nd-make-parse cdr-ts (tk:paren-node) 'framed-by-parens-open)]
                         [descend-paren-and-tail (framed-by-parens-open cdr-ts new-paren-node)]
                         [descend-paren          (car descend-paren-and-tail)]
                         [descend-tail           (cadr descend-paren-and-tail)]
                         [updated-paren          (nd-append-child paren-node descend-paren)]
                         )
                    (framed-by-parens-open descend-tail updated-paren))
                  ]
                [(close? t)
                  (list paren-node cdr-ts)
                  ]
                [else
                  (let*(
                         [updated-paren  (ndappend-child paren-node t)]
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
            '((tk:paren-node
                ((at:source framed-by-parens "test-session" (2 1 1) (20 1 19)))
                (tk:number
                  ((at:source source-generator-lex "test-session" (2 1 1) (4 1 3))
                    (at:lexeme "10")
                    (at:value 10)))
                (tk:paren-node
                  ((at:source framed-by-parens-open "test-session" (7 1 6) (20 1 19)))
                  (tk:number
                    ((at:source source-generator-lex "test-session" (7 1 6) (9 1 8))
                      (at:lexeme "21")
                      (at:value 21)))
                  (tk:number
                    ((at:source source-generator-lex "test-session" (10 1 9) (12 1 11))
                      (at:lexeme "22")
                      (at:value 22)))
                  (tk:number
                    ((at:source
                       source-generator-lex
                       "test-session"
                       (13 1 12)
                       (15 1 14))
                      (at:lexeme "23")
                      (at:value 23))))
                (tk:number
                  ((at:source source-generator-lex "test-session" (17 1 16) (19 1 18))
                    (at:lexeme "30")
                    (at:value 30))))))
          ))
        (test-hook framed-by-parens-test-0)

