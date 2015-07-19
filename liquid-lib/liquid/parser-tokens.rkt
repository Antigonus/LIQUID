#|
 tokens for the query lexer and parser

 created: 2014-12-10T15:11:10Z twl

 this extends and specifies tokens defined in tokens.rkt for the query lexer and parser  
|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex) ; for the position structure
  (require "misc-lib.rkt")
  (require "tokens.rkt")

;;--------------------------------------------------------------------------------
;; query parser tokens
;;
;;  See tokens.rkt for a definition of this class
;;
  ;; token children
  ;;
  (define (tok:conjunction) 'tok:conjunction) ; query conjunction
  (define (tok:operand) 'tok:operand); predicate operand - value must be a token list
  (define (tok:operand-list) 'tok:operand-list) ; a list of predicate operands - value must be a list of operands
  (define (tok:pattern) 'tok:pattern) ; we use this to make the operands that need to be filled in
  (define (tok:pred) 'tok:pred)  ; search predicate
  (define (tok:paren-node) 'tok:paren-node)

  (tok-hook
    (tok:conjunction)
    (tok:operand)
    (tok:operand-list)
    (tok:pattern)
    (tok:pred)
    (tok:paren-node)
    )

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide (all-defined-out))

 

