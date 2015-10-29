#|
  A central place to put parameter definitions.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require racket/date)
  (require "test-lib.rkt")
  (require "arith-lib.rkt")
  (require "sequence-lib.rkt")

;;--------------------------------------------------------------------------------
;; global parameters
;;    
  (define current-db-log (make-parameter #f))
  (provide current-db-log)

