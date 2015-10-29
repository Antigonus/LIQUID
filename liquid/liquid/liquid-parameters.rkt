#|
  each directory has its own parameters file

|#

#lang racket

(define library-name (make-parameter "liquid"))
(provide library-name)

(define library-version (make-parameter "0.9"))
(provide library-version)
