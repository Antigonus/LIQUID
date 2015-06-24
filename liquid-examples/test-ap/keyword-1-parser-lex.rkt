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

  (require liquid/lynch-lib)
  (require liquid/tokens)

  (require "keyword-1-tokens.rkt")

 
;;--------------------------------------------------------------------------------
;; make a lexer
;;
;;    input: an input port
;;   output: a list of tokens
;;
;; parts that don't lex are put in the token 'lex-other'
;;
 (define (keyword-1-lex in filename)
   (port-count-lines! in)

   (define the-lexer
     (lexer
       [ (lex:or (char-range #\a #\z) (char-range #\A #\Z))
        ;
        (let (
              [a-tok (tok-make-lex (tok:name-part) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       ["-"
        ;
        (let (
              [a-tok (tok-make-lex (tok:punc) (position-deconstruct start-pos) (position-deconstruct end-pos) lexeme)]
              )
          (cons a-tok (the-lexer input-port))
          )
        ]

       [(char-range #\0 #\9)
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
               [other-string-continue (non-white input-port)] ; non-white is in lynch.rkt
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

  (define (keyword-1-lex-test-0)
    (let*(
           [in (open-input-string "John Allen 480-90-7221")]
           [ts (qp-lex in "lex-test-0")]
           [expected-ts 
             `(
                (tok:name-part ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:name-part ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:number ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:punc ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:number ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:punc ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                (tok:number ((attribute:source source-generator-lex "test-session" (1 1 0)   (3 1 2))  (attribute:lexeme "a1")))
                )
             ]
           )
      (.eq. ts expected-ts)
      ))

  (test-hook keyword-1-lex-test-0)

