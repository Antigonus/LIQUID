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
  (provide (all-from-out "test-lib.rkt"))

  (require "arith-lib.rkt")
  (provide (all-from-out "arith-lib.rkt"))

  (require "sequence.rkt")
  (provide (all-from-out "sequence.rkt"))


  
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



  ;; functions
  ;;
    (provide ;; this context continues to the bottom of the page

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
