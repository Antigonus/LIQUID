#|
  all extensions related routines

|#

#lang racket

(require racket/date)

(require "allocator-lib.rkt")
(provide (all-from-out "allocator-lib.rkt"))
  
(require "arith-lib.rkt")
(provide (all-from-out "arith-lib.rkt"))

(require "control-structures.rkt")
(provide (all-from-out "control-structures.rkt"))

(require "sequence.rkt")
(provide (all-from-out "sequence.rkt"))

(require "strings-etc.rkt")
(provide (all-from-out "strings-etc.rkt"))

(require "test-lib.rkt")
(provide (all-from-out "test-lib.rkt"))

(define (extensions-trace)
  (allocator-lib-trace)
  (arith-lib-trace)
  (control-structures-trace)
  (sequence-trace)
  (strings-etc-trace)
  (test-lib-trace)
  )

(define (extensions-untrace)
  (allocator-lib-untrace)
  (arith-lib-untrace)
  (control-structures-untrace)
  (sequence-untrace)
  (strings-etc-untrace)
  (test-lib-untrace)
  )
