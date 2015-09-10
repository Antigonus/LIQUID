#|
 lexer for html
   note the spec at:  http://www.w3.org/TR/html-markup/syntax.html
  
   I use an approach of doing a very primitive lex, as I hope to then do
   some basic analysis to handle such things as missing quotes and unabalanced
   framing markers.

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
;;  module globals
;;

  (define current-block-size (make-parameter 16)) ; initially set small for testing

  ;; node tags
  (define nd:tag:char-block 'char-block)

  ;; node fields
  (define nd:field:tag          'nd:field:tag)
  (define nd:field:data         'nd:field:data)        
  (define nd:field:data-length  'nd:field:data-length)
  (define nd:field:eof          'nd:field:eof)


;;--------------------------------------------------------------------------------
;; pulls chars from input stream in blocks
;;
;;    input: an input port
;;   output: a node holding a block of chars as a list
;;
;;
  (define (next-char-block in)
    (let*(
           [block-length 0]
           [data
               (for/list(
                          [i (current-block-size)]
                          )
                 (let(
                       [c (read-char in)]
                       )
                   (set! block-length i)
                   #:break(eof-object? c)
                   c
                   ))]

           [char-block 
             (nd:make
               (Λ
                 nd:field:tag nd:tag:char-block 
                 nd:field:data data 
                 nd:field:data-length block-length
                 nd:field:eof (< block-length (current-block-size))
                 ))]
           )
      char-block
      ))
          

    
    
    
      



  (define (lex-html-0-test-0)
    (let*(
           [in (open-input-string "qed(a,_,3)\"s\"")]
           [ts (lex-html-0 in "lex-test-0")]
           [expected-ts 
             (Λ
               (nd-make-lex* lex-html-0:symbol "qed" "lex-test-0" '(1 1 0)  '(4 1 3))
               (nd-make-lex* lex-html-0:punc   "("   "lex-test-0" '(4 1 3)  '(5 1 4))
               (nd-make-lex* lex-html-0:symbol "a"   "lex-test-0" '(5 1 4)  '(6 1 5))
               (nd-make-lex* lex-html-0:punc   ","   "lex-test-0" '(6 1 5)  '(7 1 6))
               (nd-make-lex* lex-html-0:punc   "_"   "lex-test-0" '(7 1 6)  '(8 1 7))
               (nd-make-lex* lex-html-0:punc   ","   "lex-test-0" '(8 1 7)  '(9 1 8))
               (nd-make-lex* lex-html-0:number "3"   "lex-test-0" '(9 1 8)  '(10 1 9))
               (nd-make-lex* lex-html-0:punc   ")"   "lex-test-0" '(10 1 9) '(11 1 10))
               (nd-make-lex* lex-html-0:string "\"s\"" "lex-test-0" '(11 1 10) '(14 1 13))
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

  (test-hook lex-html-0-test-0)



;;--------------------------------------------------------------------------------
;; a basic lexer
;;
;;    input: an input port
;;   output: a list of nodes
;;
;; parts that don't lex are put in the node 'lex-other'
;; the 
;;
  (define (lex-html-0 in filename)
    (port-count-lines! in)

    (define (nd-make-lex* tag lexeme start end)
      (cons
        (nd-make-lex tag lexeme filename start end)
        (the-lexer in)))

    (define the-lexer



;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "lex-html-0" 
    lex-html-0
    )
