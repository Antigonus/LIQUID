#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    

;;--------------------------------------------------------------------------------
;; test/debug
;;
;;  a test takes no arguments and returns #f if and only if it fails
;;
;;  all tests must hook into the test list
;;
;;  (qp-test-all)  to run the tests
;;
;;
  (define test-routines '())
  (define (test-name a-test) (symbol->string (object-name a-test)))

  (define (test-hook a-test)
    (display "hooking test: ") (displayln (test-name a-test))
    (set! test-routines (cons a-test (remove a-test test-routines (λ(e f) (string=? (test-name e) (test-name f)))))))

  (define (test-remove a-test) 
    (display "removing test ") (displayln (test-name a-test))
    (set! test-routines (remove a-test test-routines)))
  (define (test a-test)
    (let(
          [fun-name (test-name a-test)]
           )
      (display "running: ")(displayln fun-name)
      (let*(
             [fun-result    (with-handlers ([(λ(v) #t) (λ(v) 'exception)]) (a-test))]
             [fun-flag      (eqv? fun-result #t)]
            )
        (display "  ")
        (cond
          [(eqv? fun-result 'exception) (displayln "failed - test raised an exception")]
          [(eqv? fun-result #f)         (displayln "failed")]
          [(eqv? fun-result #t)         (displayln "+")]
          [else
            (displayln "failed - test must return either #f or #t other values are not taken as #t")
            ]
          )
        (list fun-name fun-flag)
        )))
      
  ;; + means the test ran and passed
  ;; 'failed' means the test ran and failed
  ;; returns true if all tests pass, otherwise false
  (define (test-all)
    (let*(
          [results (map test (reverse test-routines))]
          [no-tests (length results)]
          [result-flags (map cadr results)]
          [error-count (count (λ(e) (not e)) result-flags)]
          [all-passed (andmap (λ(e) e) result-flags)]
          )
      ;;(displayln results)
      (cond
        [all-passed (display "all ")(display no-tests) (displayln " passed")]
        [else 
          (display "failed: ") 
          (display error-count)
          (display " of ") 
          (displayln no-tests)
          ]
        )
      all-passed
      ))
 
  (define (example-pass-test) (= (+ 1 1) 2))
  (define (example-fail-test) (= (+ 1 1) 3))
  (define (example-exception-test) (car 17)) ; fails due to exception being raised

  (test-hook example-pass-test) ; adds the example test to the test suit
  (test-hook example-fail-test) ; adds the example test to the test suit
  (test-hook example-exception-test) ; adds the example test to the test suit


  (displayln "running example tests.. first passes, second fails, third has an exception ..")
  (test-all)
  (test-remove example-pass-test)
  (test-remove example-fail-test)
  (test-remove example-exception-test)

  (displayln "(test-all) to run the tests")


;;--------------------------------------------------------------------------------
;; defines a trace-able module interface
;;
;;  (provide-with-trace "lib-name" provided-function ...)
;;
;;   lib-name-trace to turn on tracing
;;   lib-name-untrace to turn off tracing
;;
  (define-syntax (provide-with-trace stx)
    (let(
           [datum  (syntax->datum stx)]
          )
      (let(
            [prefix              (cadr datum)]
            [interface-functions (cddr datum)]    
           )
        (let(
              [name-trace-fun   (string->symbol (string-append prefix "-trace"))]
              [name-untrace-fun (string->symbol (string-append prefix "-untrace"))]
              )
          #|
          (displayln name-trace-fun)
          (displayln name-untrace-fun)
          |#
          (let(
                [code-block `(begin)]
                [trace-require '(require racket/trace)]
                [trace-fun
                  (append
                    '(define)
                    (list (list name-trace-fun))
                    (map (λ(e)`(trace ,e)) interface-functions)
                    )
                  ]
                [untrace-fun
                  (append
                    '(define)
                    (list (list name-untrace-fun))
                    (map (λ(e)`(untrace ,e)) interface-functions)
                    )
                  ]
                [provide-calls (map (λ(e)`(provide ,e)) interface-functions)]
                [provide-trace `(provide ,name-trace-fun)]
                [provide-untrace `(provide ,name-untrace-fun)]
                )
            #|
            (displayln code-block)
            (displayln trace-require)
            (displayln trace-fun)
            (displayln untrace-fun)
            (displayln provide-calls)
            |#
            (let*(
                   [program 
                     (append
                       code-block
                       (list trace-require)
                       (list trace-fun)
                       (list untrace-fun)
                       provide-calls
                       (list provide-trace)
                       (list provide-untrace)
                       )
                     ]
                   )
              ;;(displayln program)
              (datum->syntax stx program)
              ))))))

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  ;; language extensions
  ;;
    (provide
      provide-with-trace
      )

  ;; functions
  ;;
    (provide-with-trace "test-lib" ;; this context continues to the bottom of the page

        test-hook
        test-remove
        test-all

      )
