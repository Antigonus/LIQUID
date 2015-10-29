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
  (require "object.rkt")
  (require "node.rkt")

;;--------------------------------------------------------------------------------
;; node constructors
;;

  (define (make-source file start end) (Λ 'lexer-0 file start end))
  (define (make-source* file Λstart Λend) (make-source 'lexer-0 file (apply make-position Λstart) (apply make-position Λend)))

  (define (nd-make-lex tag lexeme filename start end)
    (define a-node (ndwat:make (Λ 'at:source (make-source filename start end) 'at:lexeme lexeme) (Λ)))
    (obj:set! nd:type a-node (Λ nd:field:tag tag))
    )

  (define (nd-make-lex* tag lexeme filename Λstart Λend)
     (nd-make-lex tag lexeme filename (apply make-position Λstart) (apply make-position Λend)))

 
;;--------------------------------------------------------------------------------
;; a simple lexer
;;
;;    input: an input port
;;   output: a list of nodes
;;
;; parts that don't lex are put in the node 'lex-other'
;; doesn't handle escaped quotes
;;
  ;; node tags
  (define lex0:symbol 'lex0:symbol) ; lexigraphical symbol
  (define lex0:string 'lex0:string)
  (define lex0:number 'lex0:number)
  (define lex0:punc   'lex0:punc)
  (define lex0:comment 'lex0:comment)
  (define lex0:other  'lex0:other)

  (define (lex0 in filename)
    (port-count-lines! in)

    (define (nd-make-lex* tag lexeme start end)
      (cons
        (nd-make-lex tag lexeme filename start end)
        (the-lexer in)))

    (define the-lexer
      (lexer
        [(lex0:seq 
           (lex0:or (char-range #\a #\z) (char-range #\A #\Z)) 
           (lex0:* (lex0:or "_" (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9)))
           )
          (nd-make-lex* lex0:symbol lexeme start-pos end-pos)
          ]

                                            ; this fails to skip \" ... seems we need a quote lexer
        [(lex0:seq "\"" (complement (lex0:seq any-string "\"" any-string)) "\"")
          (nd-make-lex* lex0:string lexeme start-pos end-pos)
          ]

        [(lex0:seq "/*" (complement (lex0:seq any-string "*/" any-string)) "*/")
          (nd-make-lex* lex0:comment lexeme start-pos end-pos)
          ]

        [(lex0:or "(" ")" "," "_" "&")
          (nd-make-lex* lex0:punc lexeme start-pos end-pos)
          ]

        [(lex0:seq (lex0:? #\-) (lex0:+ (char-range #\0 #\9)))
          (nd-make-lex* lex0:number lexeme start-pos end-pos)
          ]

        [whitespace 
          (the-lexer input-port)
          ]

        [any-char 
          (let*(
                 [other-string-continue (non-white input-port)] ; non-white is in misc.rkt
                 [other-string (string-append lexeme other-string-continue)]
                 )
            (nd-make-lex* lex0:other other-string start-pos end-pos)
            )
          ]

        [(eof) '()]
        ))
    (the-lexer in)
    )


  (define (lex0-test-0)
    (let*(
           [in (open-input-string "qed(a,_,3)\"s\"")]
           [ts (lex0 in "lex-test-0")]
           [expected-ts 
             (Λ
               (nd-make-lex* lex0:symbol "qed" "lex-test-0" '(1 1 0)  '(4 1 3))
               (nd-make-lex* lex0:punc   "("   "lex-test-0" '(4 1 3)  '(5 1 4))
               (nd-make-lex* lex0:symbol "a"   "lex-test-0" '(5 1 4)  '(6 1 5))
               (nd-make-lex* lex0:punc   ","   "lex-test-0" '(6 1 5)  '(7 1 6))
               (nd-make-lex* lex0:punc   "_"   "lex-test-0" '(7 1 6)  '(8 1 7))
               (nd-make-lex* lex0:punc   ","   "lex-test-0" '(8 1 7)  '(9 1 8))
               (nd-make-lex* lex0:number "3"   "lex-test-0" '(9 1 8)  '(10 1 9))
               (nd-make-lex* lex0:punc   ")"   "lex-test-0" '(10 1 9) '(11 1 10))
               (nd-make-lex* lex0:string "\"s\"" "lex-test-0" '(11 1 10) '(14 1 13))
               )
             ]
           )
      (for/and (
                 [t ts]
                 [et expected-ts]
                  )
        ;;(displayln (Λ t et))
        (obj:apply* type-type ndwat:type '= (Λ t et))
        )))

  (test-hook lex0-test-0)



;;--------------------------------------------------------------------------------
;; a basic lexer
;;
;;    input: an input port
;;   output: a list of nodes
;;
;; parts that don't lex are put in the node 'lex-other'
;; the 
;;
  (define (lex0 in filename)
    (port-count-lines! in)

    (define (nd-make-lex* tag lexeme start end)
      (cons
        (nd-make-lex tag lexeme filename start end)
        (the-lexer in)))

    (define the-lexer



;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "lex0" 
    lex0
    )
