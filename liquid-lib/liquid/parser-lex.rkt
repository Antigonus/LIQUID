#|
 lexer for the query parser
   created: 2014-11-21T14:38:53Z twl
   tokenizes the input string
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

  (require "lynch-lib.rkt")
  (require "tokens.rkt")
  (require "query-parser-tokens.rkt")

 
;;--------------------------------------------------------------------------------
;; make a lexer
;;
;;    input: an input port
;;   output: a list of tokens
;;
;; parts that don't lex are put in the token 'lex-other'
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
              [a-tok (tok-make-lex (tok:symbol) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       ; this fails to skip \" ... seems we need a quote lexer
       [(lex:seq "\"" (complement (lex:seq any-string "\"" any-string)) "\"")
        ;
        (let (
              [a-tok (tok-make-lex (tok:string) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq "/*" (complement (lex:seq any-string "*/" any-string)) "*/")
        ;
        (let (
              [a-tok (tok-make-lex (tok:comment) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:or "(" ")" "," "_" "&")
        ;
        (let (
              [a-tok (tok-make-lex (tok:punc) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(lex:seq (lex:? #\-) (lex:+ (char-range #\0 #\9)))
        ; 
        (let*(
              [t0 (tok-make-lex (tok:number) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              [a0 (attribute-make (attribute:value) (string->number lexeme))]
              [t1 (tok-append-attributes t0 a0)]
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
               [a-tok (tok-make-lex (tok:lex-other) (position-deconstruct start-pos) (position-deconstruct end-pos) other-string)]
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
                (tok:symbol ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (3 1 2)   (4 1 3))  (attribute:lexeme "(")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (4 1 3)   (5 1 4))  (attribute:lexeme "a")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (6 1 5)   (7 1 6))  (attribute:lexeme ",")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (7 1 6)   (8 1 7))  (attribute:lexeme "b")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (8 1 7)   (9 1 8))  (attribute:lexeme ",")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (9 1 8)   (10 1 9)) (attribute:lexeme "c")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (10 1 9)  (11 1 10))(attribute:lexeme ")")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (12 1 11) (13 1 12))(attribute:lexeme "&")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (14 1 13) (16 1 15))(attribute:lexeme "Q2")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (16 1 15) (17 1 16))(attribute:lexeme "(")))
                (tok:number ((attribute:source source-generator-lex "test-session" (17 1 16) (18 1 17))(attribute:lexeme "1")(attribute:value 1)))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (18 1 17) (19 1 18))(attribute:lexeme ",")))
                (tok:number ((attribute:source source-generator-lex "test-session" (19 1 18) (20 1 19))(attribute:lexeme "3")(attribute:value 3)))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (20 1 19) (21 1 20))(attribute:lexeme ",")))
                (tok:number ((attribute:source source-generator-lex "test-session" (21 1 20) (22 1 21))(attribute:lexeme "5")(attribute:value 5)))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (22 1 21) (23 1 22))(attribute:lexeme ")")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (24 1 23) (25 1 24))(attribute:lexeme "&")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (26 1 25) (28 1 27))(attribute:lexeme "q3")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (28 1 27) (29 1 28))(attribute:lexeme "(")))
                (tok:string ((attribute:source source-generator-lex "test-session" (29 1 28) (36 1 35))(attribute:lexeme "\"apple\"")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (36 1 35) (37 1 36))(attribute:lexeme ")")))
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
                (tok:symbol ((attribute:source source-generator-lex "test-session" (1 1 0)  (4 1 3))  (attribute:lexeme "qed")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (4 1 3)  (5 1 4))  (attribute:lexeme "(")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (5 1 4)  (6 1 5))  (attribute:lexeme "a")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (6 1 5)  (7 1 6))  (attribute:lexeme ",")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (7 1 6)  (8 1 7))  (attribute:lexeme "_")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (8 1 7)  (9 1 8))  (attribute:lexeme ",")))
                (tok:symbol ((attribute:source source-generator-lex "test-session" (9 1 8)  (10 1 9)) (attribute:lexeme "c")))
                (tok:punc   ((attribute:source source-generator-lex "test-session" (10 1 9) (11 1 10))(attribute:lexeme ")")))
                )
             ]
           )
      (.eq. ts expected-ts)
      ))
  (test-hook qp-lex-test-1)


