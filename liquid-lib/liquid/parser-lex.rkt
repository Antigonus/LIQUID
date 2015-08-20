#|
 lexer for the query parser
   created: 2014-11-21T14:38:53Z twl
   nodeizes the input string
|#

#lang racket


;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex)
  (require (prefix-in lex: parser-tools/lex-sre))

  (require "misc-lib.rkt")
  (require "node.rkt")
  (require "parser-nodes.rkt")

 
;;--------------------------------------------------------------------------------
;; make a lexer
;;
;;    input: an input port
;;   output: a list of nodes
;;
;; parts that don't lex are put in the node 'lex-other'
;;
 (define (lexer-ql0-name) 'lexer-ql0)

 (define (lexer-ql0 in filename)
   (port-count-lines! in)

   (define the-lexer
     (lexer
       [(lex:seq 
         (lex:or (char-range #\a #\z) (char-range #\A #\Z)) 
         (lex:* (lex:or "_" (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9))))
        ;
        (let (
              [a-tok (nd-make-lex (lexer-ql0-name) (nd:symbol) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       ; this fails to skip \" ... seems we need a quote lexer
       [(lex:seq "\"" (complement (lex:seq any-string "\"" any-string)) "\"")
        ;
        (let (
              [a-tok (nd-make-lex (lexer-ql0-name) (nd:string) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq "/*" (complement (lex:seq any-string "*/" any-string)) "*/")
        ;
        (let (
              [a-tok (nd-make-lex (lexer-ql0-name) (nd:comment) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:or "(" ")" "," "_" "&")
        ;
        (let (
              [a-tok (nd-make-lex (lexer-ql0-name) (nd:punc) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq (lex:? #\-) (lex:+ (char-range #\0 #\9)))
        ; 
        (let*(
              [t0 (nd-make-lex (lexer-ql0-name) (nd:number) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              [a0 (attribute-make (at:value) (string->number lexeme))]
              [t1 (on-attributes t0 bcons a0)]
              )
          (cons t1 (the-lexer input-port))
          )
        ]

       [whitespace 
        ; 
        (the-lexer input-port)
        ]

       [any-char ; 
         ;
         (let*(
               [other-string-continue (non-white input-port)] ; non-white is in utils.rkt
               [other-string (string-append lexeme other-string-continue)]
               [a-tok (nd-make-lex (lexer-ql0-name) (nd:lex-other) (position-deconstruct start-pos) (position-deconstruct end-pos) other-string)]
               )
           (cons a-tok (the-lexer input-port))
           )
        ]

       [(eof) '()]
      ))
    (the-lexer in)
  )

  (define (lexer-ql0-test-0)
    (let*(
           [in (open-input-string "a1(a ,b,c) & Q2(1,3,5) & q3(\"apple\")")]
           [ts (lexer-ql0 in "lex-test-0")]
           [expected-ts 
             `(
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (1 1 0)   (3 1 2))  (at:lexeme "a1")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (3 1 2)   (4 1 3))  (at:lexeme "(")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (4 1 3)   (5 1 4))  (at:lexeme "a")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (6 1 5)   (7 1 6))  (at:lexeme ",")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (7 1 6)   (8 1 7))  (at:lexeme "b")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (8 1 7)   (9 1 8))  (at:lexeme ",")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (9 1 8)   (10 1 9)) (at:lexeme "c")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (10 1 9)  (11 1 10))(at:lexeme ")")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (12 1 11) (13 1 12))(at:lexeme "&")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (14 1 13) (16 1 15))(at:lexeme "Q2")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (16 1 15) (17 1 16))(at:lexeme "(")))
                (nd:number ((at:source ,(lexer-ql0-name) "test-session" (17 1 16) (18 1 17))(at:lexeme "1")(at:value 1)))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (18 1 17) (19 1 18))(at:lexeme ",")))
                (nd:number ((at:source ,(lexer-ql0-name) "test-session" (19 1 18) (20 1 19))(at:lexeme "3")(at:value 3)))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (20 1 19) (21 1 20))(at:lexeme ",")))
                (nd:number ((at:source ,(lexer-ql0-name) "test-session" (21 1 20) (22 1 21))(at:lexeme "5")(at:value 5)))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (22 1 21) (23 1 22))(at:lexeme ")")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (24 1 23) (25 1 24))(at:lexeme "&")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (26 1 25) (28 1 27))(at:lexeme "q3")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (28 1 27) (29 1 28))(at:lexeme "(")))
                (nd:string ((at:source ,(lexer-ql0-name) "test-session" (29 1 28) (36 1 35))(at:lexeme "\"apple\"")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (36 1 35) (37 1 36))(at:lexeme ")")))
                )
             ]
           )
      (nds-equal? ts expected-ts)
      ))

  (test-hook lexer-ql0-test-0)

  (define (lexer-ql0-test-1)
    (let*(
           [in (open-input-string "qed(a,_,c)")]
           [ts (lexer-ql0 in "lex-test-1")]
           [expected-ts 
             `(
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (1 1 0)  (4 1 3))  (at:lexeme "qed")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (4 1 3)  (5 1 4))  (at:lexeme "(")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (5 1 4)  (6 1 5))  (at:lexeme "a")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (6 1 5)  (7 1 6))  (at:lexeme ",")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (7 1 6)  (8 1 7))  (at:lexeme "_")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (8 1 7)  (9 1 8))  (at:lexeme ",")))
                (nd:symbol ((at:source ,(lexer-ql0-name) "test-session" (9 1 8)  (10 1 9)) (at:lexeme "c")))
                (nd:punc   ((at:source ,(lexer-ql0-name) "test-session" (10 1 9) (11 1 10))(at:lexeme ")")))
                )
             ]
           )
      (nds-equal? ts expected-ts)
      ))
  (test-hook lexer-ql0-test-1)


;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "lexer-ql0" 
    lexer-ql0
    lexer-ql0-name
    )
