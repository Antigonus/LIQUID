#|
 nodes for the query language 0 lexer and parser

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
  (define (ndql0:conjunction) 'ndql0:conjunction) ; query conjunction
  (define (ndql0:operand) 'ndql0:operand); predicate operand - value must be a node list
  (define (ndql0:operand-list) 'ndql0:operand-list) ; a list of predicate operands - value must be a list of operands
  (define (ndql0:pattern) 'ndql0:pattern) ; we use this to make the operands that need to be filled in
  (define (ndql0:pred) 'ndql0:pred)  ; search predicate
  (define (ndql0:paren-node) 'ndql0:paren-node)

  (nd-hook
    (ndql0:conjunction)
    (ndql0:operand)
    (ndql0:operand-list)
    (ndql0:pattern)
    (ndql0:pred)
    (ndql0:paren-node)
    )

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide (all-defined-out))

 

