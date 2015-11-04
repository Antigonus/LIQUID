#|
 nodes http header parsing

 created: 2014-12-10T15:11:10Z twl

 this extends and specifies nodes defined in nodes.rkt for the query lexer and parser  
|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require parser-tools/lex) ; for the position structure
  (require liquid/extensions)
  (require liquid/node) ; declares the base node object


;;--------------------------------------------------------------------------------
;; query parser nodes
;;
;;  See nodes.rkt for a definition of this class
;;
  ;; node children
  ;;
    (define (ndhttp:raw-line) 'ndhttp:raw-line) ; anythign ending in a cr-lf
    (define (ndhttp:raw-header) 'ndhttp:raw-header) ; lines ending with a blank line
    (define (ndhttp:request-line) 'ndhttp:request-line)
    (define (ndhttp:

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

 

