#lang racket

(require syntax/srcloc)
(require "test-lib.rkt")

;; returns the location information from a syntax object
;;
  (define (source-location syntax-object)
    (make-srcloc
      (source-location-source syntax-object)
      (source-location-line  syntax-object)
      (source-location-column  syntax-object)
      (source-location-position syntax-object)
      (source-location-span syntax-object)
      ))

(provide-with-trace "location"
  source-location
  )
