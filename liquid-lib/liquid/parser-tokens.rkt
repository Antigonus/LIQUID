#|
 nodes for the query lexer and parser

 created: 2014-12-10T15:11:10Z twl

 this extends and specifies nodes defined in nodes.rkt for the query lexer and parser  
|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex) ; for the position structure
  (require "misc-lib.rkt")
  (require "node.rkt")

;;--------------------------------------------------------------------------------
;; query parser nodes
;;
;;  See nodes.rkt for a definition of this class
;;
  ;; node children
  ;;
  (define (tk:conjunction) 'tk:conjunction) ; query conjunction
  (define (tk:operand) 'tk:operand); predicate operand - value must be a node list
  (define (tk:operand-list) 'tk:operand-list) ; a list of predicate operands - value must be a list of operands
  (define (tk:pattern) 'tk:pattern) ; we use this to make the operands that need to be filled in
  (define (tk:pred) 'tk:pred)  ; search predicate
  (define (tk:paren-node) 'tk:paren-node)

  (ndhook
    (tk:conjunction)
    (tk:operand)
    (tk:operand-list)
    (tk:pattern)
    (tk:pred)
    (tk:paren-node)
    )

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide (all-defined-out))

 

