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
  (require racket/date)
  (require parser-tools/lex); for list-strings
  (require (prefix-in lex: parser-tools/lex-sre))

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
    (cond 
      [(not (memv a-test test-routines))
        (display "hooking test: ") (displayln (test-name a-test))
        (set! test-routines (cons a-test test-routines))
        (void)
        ]
      [else (void)]
      ))

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
 
  (define (example-test-0) (= (+ 1 1) 2))
  (define (example-test-1) (= (+ 1 1) 3))
  (define (example-test-2) (car 17)) ; fails due to exception being raised

  (test-hook example-test-0) ; adds the example test to the test suit
  (test-hook example-test-1) ; adds the example test to the test suit
  (test-hook example-test-2) ; adds the example test to the test suit


  (displayln "running example tests.. car passes, cadr fails, caddr has an exception ..")
  (test-all)
  (test-remove example-test-0)
  (test-remove example-test-1)
  (test-remove example-test-2)

  (displayln "(test-all) to run the tests")

;;--------------------------------------------------------------------------------
;; global parameters
;;    
  (date-display-format `iso-8601)
  (define current-log-port (make-parameter (open-output-file "log-http-server.txt" #:exists `can-update)))
  (define current-read-request-timeout (make-parameter 5)); 5 cadrs to finish request header read
  (define current-file-name (make-parameter "test-session")); filename for query parsing
  (define current-get-webpage-timeout (make-parameter 60)) ; used in "web.rkt"
  (define current-db-log (make-parameter #f))

;;--------------------------------------------------------------------------------
;; global types
;;    
  (struct session-context (in out server-IP server-port client-IP client-port) #:transparent)
  (define (session-context->string a-session-context) 
    (string-append
     "server "
      (session-context-server-IP a-session-context)
      ":"
      (number->string (session-context-server-port a-session-context))
      ";"
      "client "
      (session-context-client-IP a-session-context)
      ":"
      (number->string (session-context-client-port a-session-context))
      ))


;;--------------------------------------------------------------------------------
;; arithmetic -- these should be set at the syntax level
;;
  (define (≠ . args) (not (apply = args)))
  (define ≥ >=)
  (define ≤ <=)
  (define (++ n) (+ n 1))
  (define (-- n) (- n 1))

;;--------------------------------------------------------------------------------
;; fundamental functions
;;    
  (define (do-nothing . args) void) ; possibly examine a value with trace
  (define identity values)
  (define (boolify b) (not (not b))) ; outputs either #t or #f
  (define no-error not) ; useful when returning exception values on fail

;;--------------------------------------------------------------------------------
;; log utils
;;
  (define (time-stamp) ; puts the time in the log
    (define stamp (date->string (current-date) #t))
    (display stamp (current-log-port))
  )

  (define (display-log mess) (display mess (current-log-port)))
  (define (display-log-append mess) (display " " (current-log-port)) (display mess (current-log-port)))
  (define (log mess)
    (time-stamp) 
    (if (list? mess)
       (for-each display-log-append mess)
       (display-log-append mess)
       )
    (newline (current-log-port))
    (flush-output (current-log-port))
    )

;;--------------------------------------------------------------------------------
;; list manipulation
;;
  ;; efficient length compares
  ;;
    (define (length* l limit [n 0])
      (cond [(null? l) n]
            [(= n limit) limit]
            [else (length* (cdr l) (++ n))]))

    (define (length≥ l n)
      (cond [ (and (null? l) (= n 0))  #t]
            [ (null? l) #f ]
            [ (= n 0) #t ]
            [ else (length≥ (cdr l) (-- n))]
            ))

    (define (length≥-test-0) (length≥ '(a b c) 2))
    (define (length≥-test-1) (length≥ '(a b c) 3))
    (define (length≥-test-2) (not (length≥ '(a b c) 4)))
    (define (length≥-test-3) (length≥ '() 0))
    (define (length≥-test-4) (not (length≥ '() 1)))

    (define (length< l n) (not (length≥ l n)))
    (define (length> l n) (length≥ l (++ n)))


    (define (length= l n)
      (cond [ (and (null? l) (= n 0))  #t]
            [ (null? l) #f ]
            [ (= n 0) #f ]
            [ else (length= (cdr l) (-- n))]
            ))
    (define (length=-test-0) (not (length= '(a b c) 2)))
    (define (length=-test-1) (length= '(a b c) 3))
    (define (length=-test-2) (not (length= '(a b c) 4)))

    (define (length≠ l n) (not (length= l n)))

    (define (singleton l) 
      (and
        (pair? l)
        (null? (cdr l))
        ))

    ;; input: two lists, an element lt comparison, optionally an equal comparison
    ;; output: bool
    ;;    shorter of otherwise equal lists is considered less than
    ;;
    (define (list< a b [element-lt <] [element-eq equal?])
      (cond
        [(and (null? a) (null? b)) #f] ; then the two lists are equal
        [(null? a) #t]
        [(null? b) #f]
        [else
          (let(
                [ea (car a)]
                [eb (car b)]
                [ra (cdr a)]
                [rb (cdr b)]
                )
            (cond
              [(element-lt ea eb) #t]
              [(element-eq ea eb) (list< ra rb)]
              [else #f]
              ))
          ]
        ))

    (define (list<-test-0)
      (and
        (list< '(5 7 4) '(7 7 4))
        (list< '(5 7 4) '(5 8 4))
        (list< '(5 7 4) '(5 7 4 3))
        (list< '(5 7 4 3) '(5 7 5))
        (not 
          (or
            (list< '(7 7 4) '(5 7 4))
            (list< '(5 8 4) '(5 7 4))
            (list< '(5 7 4 3) '(5 7 4))
            (list< '(5 7 5) '(5 7 4 3))
            (list< '(5 7 4 3) '(5 7 4 3))
            ))))
    (test-hook list<-test-0)         


;;--------------------------------------------------------------------------------
;; session unique numbers and identifiers
;;
;;   programmers should not make identifiers of the form "unique-to-session-name-[0-9]+"
;;   one would think that wouldn't be a very strong programming constraint ...
;;
  (define unique-to-session-count 0)
  (define (unique-to-session-number)
    (begin0
      unique-to-session-count
      (set! unique-to-session-count (++ unique-to-session-count))
      ))
   (define (unique-to-session-number-dealloc n) (void))
   (define (unique-to-session-name)
     (string-append "unique-to-session-name-" (number->string (unique-to-session-number))))
   (define (unique-to-session-name-dealloc name) (void))


;;--------------------------------------------------------------------------------
;; if a variable is defined
;;   see: Metaxal http://stackoverflow.com/questions/20076868/how-to-know-whether-a-racket-variable-is-defined-or-not
;;
  (define-syntax (if-defined stx)
    (syntax-case stx ()
      [(_ id iftrue iffalse)
       (let ([where (identifier-binding #'id)])
         (if where #'iftrue #'iffalse))]))

  (define (if-defined-test-0)
      (if-defined if-defined-test-x (void) (define if-defined-test-x 9))
      (and
        (eqv? (if-defined if-defined-test-no-such-var 'yes 'no) 'no)
        (eqv? (if-defined if-defined-test-x 'yes 'no) 'yes)
        (= if-defined-test-x 9)
        ))
  (test-hook if-defined-test-0)

  ;; quiet define, if the variable is already defined does nothing (instead of complaining)
  (define-syntax (qdefine stx)
    (syntax-case stx ()
      [(qdefine id value)
         (let ([where (identifier-binding #'id)])
           (if where #'(void) #'(define id value))
           )
        ]
      ))

  (define (if-defined-test-1)
    (let(
          [y 11]
          )
      (qdefine if-defined-test-x 9)
      (qdefine y 12)
      (and
        (= if-defined-test-x 9)
        (= y 11)
        )))
  (test-hook if-defined-test-1)

  ;; quiet define, if the variable is already defined uses set!, else uses define
  (define-syntax (qdefine! stx)
    (syntax-case stx ()
      [(qdefine id value)
         (let ([where (identifier-binding #'id)])
           (if where #'(set! id value) #'(define id value))
           )
        ]
      ))

  (define (if-defined-test-2)
    (let(
          [y 11]
          )
      (qdefine! if-defined-test-x 9)
      (qdefine! y 12)
      (and
        (= if-defined-test-x 9)
        (= y 12)
        )))
  (test-hook if-defined-test-2)



;;--------------------------------------------------------------------------------
;; common parsing functions
;;

  ;; some string processing routines
  ;;
    (define (->string x)
      (let(
            [out (open-output-string)]
            )
        (display x out)
        (get-output-string out)
        ))

    (define (->pretty-string x)
      (let(
            [out (open-output-string)]
            )
        (pretty-print x out)
        (get-output-string out)
        ))

    (define (with-quotes str) (string-append "\"" str "\""))
    (define (with-escaped-quotes str)  (string-append (string #\\ #\") str (string #\\ #\")))
    (define (with-squotes str) (string-append "'" str "'"))
    

    (define (drop-end-chars s) (list->string (drop-right (drop (string->list s) 1) 1))) 
    (define (drop-end-chars-test-0) (string=?  (drop-end-chars "\"abc\"")  "abc"))
    (test-hook drop-end-chars-test-0)

    (define (string-car str) (string-ref str 0))
    (define (char->string ch) (make-string 1 ch))

    (define (skip-white [in (current-input-port)]) 
      (define ch (peek-char in))
      (cond
            [(eof-object? ch) (void)]
            [(char-whitespace? ch) 
              (read-char in) 
              (skip-white in)
              ]
            [else (void)]))

    (define (non-white-list [in (current-input-port)]) 
      (define ch (peek-char in))
      (cond
            [(eof-object? ch) `()]
            [(char-whitespace? ch) `()]
            [else 
             (read-char in)
             (cons ch (non-white-list in))]
            ))

    (define (non-white [in (current-input-port)]) (list->string (non-white-list in)))

    (define (test-non-white-0)
      (define ip (open-input-string "ab cd ef"))
      (and
        (equal? (begin (skip-white ip) (non-white-list ip)) '(#\a #\b))
        (equal? (begin (skip-white ip) (non-white-list ip)) '(#\c #\d))
        (equal? (begin (skip-white ip) (non-white-list ip)) '(#\e #\f))
      ))

    (define (test-non-white-1)
      (define ip (open-input-string "ab cd ef"))
      (and
        (equal? (begin (skip-white ip) (non-white ip)) "ab")
        (equal? (begin (skip-white ip) (non-white ip)) "cd")
        (equal? (begin (skip-white ip) (non-white ip)) "ef")
      ))

    ;; tokenizer
    ;;
    ;; input: an input port 
    ;; output: a list of whitespace separated strings we found up until EOF
    ;;
      (define list-strings
        (lexer 

         [(lex:+ (lex:& any-char (lex:~ whitespace)))
          (begin
            (cons lexeme (list-strings input-port))
            )]

         [(lex:+ whitespace) 
          (list-strings input-port)
          ]

         [(eof) 
          `() ; terminate parsing
          ]
         ))


;;--------------------------------------------------------------------------------
;; ruby style always block
;;
;;  form1 runs, then form2 runs.  The return value is from form1.
;;
;;  If form1 throws an exception, form2 still runs.  Then the exception is thrown.
;;
;;  If form1 throws  an exception, then form2 throws an exception, we thow the exception from
;;  form2.  I'm not sure this is the desired behavior, perhaps we should always throw the
;;  form1 exception.  What to do when two exceptions are in flight?  hmmm
;;
   (define-syntax (begin-always stx)
     (syntax-case stx ()
       [(begin-always form1 form2)
         #`(begin0
             (with-handlers
               (
                 [(λ(v) #t) ; this handler catches anything
                   (λ(v)
                     form2
                     (raise v)
                     )
                   ]
                 )
               form1
               )
             form2
             )
         ]
       ))

    (define (begin-always-test-0)
      (define x 3)
      (and
        (= 5 (begin-always 5 (set! x 7)))
        (= 7 x)
        ))
    (test-hook begin-always-test-0)

    (define (begin-always-test-1)
      (define x 2)
      (with-handlers (
                       [exn:fail:contract:divide-by-zero? (λ(v)(= x 4))]
                       )
        (begin-always
          (/ 1 0)
          (set! x 4)
          )
        #f
        ))
    (test-hook begin-always-test-1)



;;--------------------------------------------------------------------------------
;;  a with-semaphore block
;;
;;  (with-semaphore semaphore body ...) 
;;
  (define-syntax (with-semaphore stx)
    (syntax-case stx ()
      [(with-semaphore semaphore body ...)
        #`(begin-always
            (begin
              (semaphore-wait semaphore)
              body ...
              )
            (semaphore-post semaphore)
            )]))

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
                       )
                     ]
                   )
;              #|
              (displayln program)
;              |#
              (datum->syntax stx program)
              ))))))

;;--------------------------------------------------------------------------------
;; provides the following
;;    
  ;; global parameters
  ;;
    (provide current-read-request-timeout) ; how long we wait for the http client to provide the complete request
    (provide current-file-name) ; the source file we are reading from for the query parser
    (provide current-get-webpage-timeout)

  ;; global type definitions
  ;;
    (provide session-context)  ; helps organize ssl connections
    (provide session-context-in)
    (provide session-context-out)
    (provide session-context-server-IP)
    (provide session-context-server-port)
    (provide session-context-client-IP)
    (provide session-context-client-port)
    (provide session-context->string)


  ;; language extensions
  ;;
    (provide
      if-defined
      qdefine
      qdefine!
      with-semaphore
      provide-with-trace
      begin-always
      )

  ;; functions
  ;;
    (provide-with-trace "misc-lib" ;; this context continues to the bottom of the page

      ;; arithmetic
      ;;
        ≠
        ≥
        ≤
        ++
        --

      ;; fundamental functions
      ;;
        do-nothing
        identity
        boolify  ;; (not #f) -> #t,  #f -> #f
        no-error ;; same as (not)

      ;; putting messages into the log
      ;;
        log

      ;; testing/debug
      ;;
        test-hook
        test-remove
        test-all

      ;; list manipulation
      ;;
        ;; efficient length compares 
        ;;   something like (length a-list) > 3  would take the length of the entire list before comparing
        ;;
          length* ; length with limit
          length>
          length≥
          length=
          length<
          length≠
          singleton

          list<

      ;; for creating unique to the session numbers or identifiers, might not be unique between sessions
      ;;
        unique-to-session-number ; gives out a number it never gave out before
        unique-to-session-number-dealloc
        unique-to-session-name ; gives out a name of the form idnnnn   where nnnn is a number.
        unique-to-session-name-dealloc

      ;; character/string utilities
      ;;
        ->string ; uses (write) to convert an object to string
        ->pretty-string ; uses pretty print to convert an object to a string

        with-quotes ; puts quotes on a string
        with-escaped-quotes ; with escaped double quotes
        with-squotes ; puts single quotes on a string
        drop-end-chars ; typically for removing quotes
        string-car ; car character in a string
        char->string
        skip-white   ; skips white chars on port
        non-white-list ; reads non-white chars from port and puts them in a list
        non-white ; returns string rather than list as per non-white-list
        list-strings ; returns list of white separated tokens found on port

      )
