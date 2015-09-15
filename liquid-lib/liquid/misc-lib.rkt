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

  (require "test-lib.rkt")
  (provide (all-from-out "test-lib.rkt")

  (require "arith-lib.rkt")
  (provide (all-from-out "arith-lib.rkt")

  (require "sequence-lib.rkt")
  (provide (all-from-out "sequence-lib.rkt"))

  (require "function-lib.rkt")
  (provide (all-from-out "list-lib.rkt"))




;;--------------------------------------------------------------------------------
;;  parallel cond, executes all that are true
;;
;;  else true clauses are concatenated and run, so they run in order of appearence in
;;  the cond.  would like to have cond** that uses threads instead
;;

  (define-syntax (cond* stx)
    (let*(
           [datum (syntax->datum stx)]
           [clauses (cdr datum)]
           [condition-clauses (filter (λ(e)(not (eqv? 'else e))) clauses)]
           [else-clauses      (filter (λ(e)(eqv? 'else e)) clauses)]
           [program
             `(let*(
                    [condition (λ(e)(eval (car e)))]
                    [transform (λ(e)(eval (cons 'begin (cdr e))))]
                    [results   (filter-fold condition transform ',condition-clauses)]
                    )
                (if (null? results)
                  (filter-fold (λ(e)#t) transform ',else-clauses)
                  results
                  ))]
           )
      ;;(display "cond* program:") (displayln program)
      (datum->syntax stx program)
      ))

;; notice no test of else, and it don't work

    (define (cond*-test-0)
      (equal?
        (cond*
          [#t 1]
          [#f (+ 1 1)]
          [#t (+ 2 1)]
          )
        '(1 3)
        ))
    (test-hook cond*-test-0)

    (define (cond*-test-1)
      (equal?
        (cond*
          [(odd? 3) 5]
          [(begin (+ 7 9) #f) (+ 11 13)]
          [#t (+ 15 17)]
        )
      '(5 32)
        ))
    (test-hook cond*-test-1)
  
;;--------------------------------------------------------------------------------
;; common parsing functions
;;

  ;; some string processing routines
  ;;
    (define (->string x [conv-fun display]) ; converts x to a string, default uses display, also consider write and pretty-print
      (let(
            [out (open-output-string)]
            )
        (conv-fun x out)
        (begin0
          (get-output-string out)
          (close-output-port out)
          )))

    (define (with-quotes str) (string-append "\"" str "\""))
    (define (with-escaped-quotes str)  (string-append (string #\\ #\") str (string #\\ #\")))
    (define (with-squotes str) (string-append "'" str "'"))
    
    (define-for-syntax (with-quotes str) (string-append "\"" str "\""))
    (define-for-syntax (with-escaped-quotes str)  (string-append (string #\\ #\") str (string #\\ #\")))
    (define-for-syntax (with-squotes str) (string-append "'" str "'"))


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

    ;; nodeizer
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
      cond*
      )

  ;; functions
  ;;
    (provide-with-trace "misc-lib" ;; this context continues to the bottom of the page

      ;; putting messages into the log
      ;;
        log

      ;; testing/debug
      ;;
        test-hook
        test-remove
        test-all


      ;; for creating unique to the session numbers or identifiers, might not be unique between sessions
      ;;
        unique-to-session-number ; gives out a number it never gave out before
        unique-to-session-number-dealloc
        unique-to-session-name ; gives out a name of the form idnnnn   where nnnn is a number.
        unique-to-session-name-dealloc

      ;; character/string utilities
      ;;
        ->string ; converts object to string using [display] or provide write or pretty-print

        with-quotes ; puts quotes on a string
        with-escaped-quotes ; with escaped double quotes
        with-squotes ; puts single quotes on a string
        drop-end-chars ; typically for removing quotes
        string-car ; car character in a string
        char->string
        skip-white   ; skips white chars on port
        non-white-list ; reads non-white chars from port and puts them in a list
        non-white ; returns string rather than list as per non-white-list
        list-strings ; returns list of white separated nodes found on port

      )
