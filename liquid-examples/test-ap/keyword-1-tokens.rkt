#|
 tokens for the query lexer and parser

 created: 2015-05-25T05:05:04Z twl

 Tokens for the first keyword search example.  Very simple parsing.  Now fancy
 name vairations supported.  Just First Last.


|#
#lang racket

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide (all-defined-out))

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex) ; for the position structure

  (require liquid/lynch-lib)
  (require liquid/tokens)

;;--------------------------------------------------------------------------------
;; tokens
;;
;;  See tokens.rkt for a definition of this class
;;
  ;; token children
  ;;
  (define (tok:name) 'tok:name-part) ; first name or last name
  (define (tok:name) 'tok:name) ; first name followed by last name
  (define (tok:ss-number) 'tok:ss-number)
  (define (tok:money) 'tok:money) 
  (define (tok:keyword-1-loose) 'tok:kw1-loose) 
  (define (tok:keyword-1) 'tok:kw1) 


  (tok-hook
    (tok:name-part)  
    (tok:name)
    (tok:ss-number)
    (tok:money)
    (tok:keyword-1-loose)
    (tok:keyword-1)
    )


