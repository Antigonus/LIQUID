#|
 lexer for the query parser
   created: 2014-11-21T14:38:53Z twl
   nodeizes the input string
|#

#lang racket

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide qp-lex)

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
 (define (qp-lex in filename)
   (port-count-lines! in)

   (define the-lexer
     (lexer
       [(lex:seq 
         (lex:or (char-range #\a #\z) (char-range #\A #\Z)) 
         (lex:* (lex:or "_" (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9))))
        ;
        (let (
              [a-tok (nd-make-lex (tk:symbol) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       ; this fails to skip \" ... seems we need a quote lexer
       [(lex:seq "\"" (complement (lex:seq any-string "\"" any-string)) "\"")
        ;
        (let (
              [a-tok (nd-make-lex (tk:string) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq "/*" (complement (lex:seq any-string "*/" any-string)) "*/")
        ;
        (let (
              [a-tok (nd-make-lex (tk:comment) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:or "(" ")" "," "_" "&")
        ;
        (let (
              [a-tok (nd-make-lex (tk:punc) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq (lex:? #\-) (lex:+ (char-range #\0 #\9)))
        ; 
        (let*(
              [t0 (nd-make-lex (tk:number) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              [a0 (attribute-make (at:value) (string->number lexeme))]
              [t1 (nd-ascribe-attribute t0 a0)]
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
               [a-tok (nd-make-lex (tk:lex-other) (position-deconstruct start-pos) (position-deconstruct end-pos) other-string)]
               )
           (cons a-tok (the-lexer input-port))
           )
        ]

       [(eof) '()]
      ))
    (the-lexer in)
  )

  (define (qp-lex-test-0)
    (let*(
           [in (open-input-string "a1(a ,b,c) & Q2(1,3,5) & q3(\"apple\")")]
           [ts (qp-lex in "lex-test-0")]
           [expected-ts 
             `(
                (tk:symbol ((at:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (at:lexeme "a1")))
                (tk:punc   ((at:source source-generator-lex "test-session" (3 1 2)   (4 1 3))  (at:lexeme "(")))
                (tk:symbol ((at:source source-generator-lex "test-session" (4 1 3)   (5 1 4))  (at:lexeme "a")))
                (tk:punc   ((at:source source-generator-lex "test-session" (6 1 5)   (7 1 6))  (at:lexeme ",")))
                (tk:symbol ((at:source source-generator-lex "test-session" (7 1 6)   (8 1 7))  (at:lexeme "b")))
                (tk:punc   ((at:source source-generator-lex "test-session" (8 1 7)   (9 1 8))  (at:lexeme ",")))
                (tk:symbol ((at:source source-generator-lex "test-session" (9 1 8)   (10 1 9)) (at:lexeme "c")))
                (tk:punc   ((at:source source-generator-lex "test-session" (10 1 9)  (11 1 10))(at:lexeme ")")))
                (tk:punc   ((at:source source-generator-lex "test-session" (12 1 11) (13 1 12))(at:lexeme "&")))
                (tk:symbol ((at:source source-generator-lex "test-session" (14 1 13) (16 1 15))(at:lexeme "Q2")))
                (tk:punc   ((at:source source-generator-lex "test-session" (16 1 15) (17 1 16))(at:lexeme "(")))
                (tk:number ((at:source source-generator-lex "test-session" (17 1 16) (18 1 17))(at:lexeme "1")(at:value 1)))
                (tk:punc   ((at:source source-generator-lex "test-session" (18 1 17) (19 1 18))(at:lexeme ",")))
                (tk:number ((at:source source-generator-lex "test-session" (19 1 18) (20 1 19))(at:lexeme "3")(at:value 3)))
                (tk:punc   ((at:source source-generator-lex "test-session" (20 1 19) (21 1 20))(at:lexeme ",")))
                (tk:number ((at:source source-generator-lex "test-session" (21 1 20) (22 1 21))(at:lexeme "5")(at:value 5)))
                (tk:punc   ((at:source source-generator-lex "test-session" (22 1 21) (23 1 22))(at:lexeme ")")))
                (tk:punc   ((at:source source-generator-lex "test-session" (24 1 23) (25 1 24))(at:lexeme "&")))
                (tk:symbol ((at:source source-generator-lex "test-session" (26 1 25) (28 1 27))(at:lexeme "q3")))
                (tk:punc   ((at:source source-generator-lex "test-session" (28 1 27) (29 1 28))(at:lexeme "(")))
                (tk:string ((at:source source-generator-lex "test-session" (29 1 28) (36 1 35))(at:lexeme "\"apple\"")))
                (tk:punc   ((at:source source-generator-lex "test-session" (36 1 35) (37 1 36))(at:lexeme ")")))
                )
             ]
           )
      (.eq. ts expected-ts)
      ))

  (test-hook qp-lex-test-0)

  (define (qp-lex-test-1)
    (let*(
           [in (open-input-string "qed(a,_,c)")]
           [ts (qp-lex in "lex-test-1")]
           [expected-ts 
             `(
                (tk:symbol ((at:source source-generator-lex "test-session" (1 1 0)  (4 1 3))  (at:lexeme "qed")))
                (tk:punc   ((at:source source-generator-lex "test-session" (4 1 3)  (5 1 4))  (at:lexeme "(")))
                (tk:symbol ((at:source source-generator-lex "test-session" (5 1 4)  (6 1 5))  (at:lexeme "a")))
                (tk:punc   ((at:source source-generator-lex "test-session" (6 1 5)  (7 1 6))  (at:lexeme ",")))
                (tk:punc   ((at:source source-generator-lex "test-session" (7 1 6)  (8 1 7))  (at:lexeme "_")))
                (tk:punc   ((at:source source-generator-lex "test-session" (8 1 7)  (9 1 8))  (at:lexeme ",")))
                (tk:symbol ((at:source source-generator-lex "test-session" (9 1 8)  (10 1 9)) (at:lexeme "c")))
                (tk:punc   ((at:source source-generator-lex "test-session" (10 1 9) (11 1 10))(at:lexeme ")")))
                )
             ]
           )
      (.eq. ts expected-ts)
      ))
  (test-hook qp-lex-test-1)


