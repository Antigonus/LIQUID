#|
  tests for filter.rkt 
    2015-05-25T08:53:03Z created twl

  These tests are here because they make use of parser.rkt
|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "misc-lib.rkt")
  (require "filter.rkt")
  (require "parser.rkt")
  (require "node.rkt")

;;--------------------------------------------------------------------------------
;;
  (define (marked-nds-test-0)
    (let*(
           [          in (open-input-string "d(n_t,)")]
           [       parse (parser in)]
           [          ts (list parse)]
           [the-marked-nds (marked-nds ts nd-has-err)]
           [expected-marked-nds 
             '((ndql0:operand
                 ((at:source rule-operand "test-session" (6 1 5) (7 1 6))
                   (attribute:errsyn-mess "expected ndql0:operand but found:")
                   (attribute:errsyn-nds
                     ((ndql0:errsyn
                        ((at:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                          (attribute:errsyn-mess "list ended with a separator: ")
                          (attribute:errsyn-nds
                            ((ndql0:punc
                               ((at:source
                                  source-generator-lex
                                  "test-session"
                                  (6 1 5)
                                  (7 1 6))
                                 (at:lexeme ","))))))))))))
             ]
           )
      (nds-equal? the-marked-nds expected-marked-nds)
      ))
  (test-hook marked-nds-test-0)

  (define (marked-nds-test-1)
    (let*(
           [         in (open-input-string "qed(a,_,c)&dlp(\"a\", __ , seven) & wikipedia(number_theory,)")]
           [       parse (parser in)]
           [          ts (list parse)]
           [the-marked-nds (marked-nds ts nd-has-err)]
           [expected-marked-nds 
             '((ndql0:operand
                 ((at:source rule-operand "test-session" (21 1 20) (23 1 22))
                   (attribute:errsyn-mess "expected ndql0:operand but found:")
                   (attribute:errsyn-nds
                     ((ndql0:punc
                        ((at:source
                           source-generator-lex
                           "test-session"
                           (21 1 20)
                           (22 1 21))
                          (at:lexeme "_")))
                       (ndql0:punc
                         ((at:source
                            source-generator-lex
                            "test-session"
                            (22 1 21)
                            (23 1 22))
                           (at:lexeme "_")))))))
                (ndql0:operand
                  ((at:source rule-operand "test-session" (58 1 57) (59 1 58))
                    (attribute:errsyn-mess "expected ndql0:operand but found:")
                    (attribute:errsyn-nds
                      ((ndql0:errsyn
                         ((at:source
                            framed-items-sep-0
                            "test-session"
                            (58 1 57)
                            (59 1 58))
                           (attribute:errsyn-mess "list ended with a separator: ")
                           (attribute:errsyn-nds
                             ((ndql0:punc
                                ((at:source
                                   source-generator-lex
                                   "test-session"
                                   (58 1 57)
                                   (59 1 58))
                                  (at:lexeme ","))))))))))))
             ]
           )
      (nds-equal? the-marked-nds expected-marked-nds)
      ))
    (test-hook marked-nds-test-1)

;;--------------------------------------------------------------------------------
;;
  (define (trim-nd-err-test-0) 
    (let*(
           [in (open-input-string "d(n_t,)")]
           [parse (parser in)]
           [trimmed (trim-nd-err parse)]
           [expected-trimmed 
             '(ndql0:conjunction
                ((at:source rule-conjunction "test-session" (1 1 0) (8 1 7)))
                (ndql0:pred
                  ((at:source rule-pred "test-session" (1 1 0) (8 1 7))
                    (at:value "d"))
                  (ndql0:operand
                    ((at:source rule-operand "test-session" (6 1 5) (7 1 6))
                      (attribute:errsyn-mess "expected ndql0:operand but found:")
                      (attribute:errsyn-nds
                        ((ndql0:errsyn
                           ((at:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-nds
                               ((ndql0:punc
                                  ((at:source
                                     source-generator-lex
                                     "test-session"
                                     (6 1 5)
                                     (7 1 6))
                                    (at:lexeme ",")))))))))))))
             ]
           )
      (nds-equal? trimmed expected-trimmed)
      ))
  (test-hook trim-nd-err-test-0)

  (define (trim-nd-err-test-1) 
    (let*(
           [in (open-input-string "d(n_t,2)")]
           [parse (parser in)]
           [trimmed (trim-nd-err parse)]
           )
      (not trimmed)
      ))
  (test-hook trim-nd-err-test-1)

  (define (trim-nd-err-test-2) 
    (let*(
           [in (open-input-string "qed(a,_,c)&dlp(\"a\", 3, seven) & wikipedia(number_theory,)")]
           [parse (parser in)]
           [trimmed (trim-nd-err parse)]
           [expected-trimmed 
             '(ndql0:conjunction
                ((at:source rule-conjunction "test-session" (1 1 0) (58 1 57)))
                (ndql0:pred
                  ((at:source rule-pred "test-session" (33 1 32) (58 1 57))
                    (at:value "wikipedia"))
                  (ndql0:operand
                    ((at:source rule-operand "test-session" (56 1 55) (57 1 56))
                      (attribute:errsyn-mess "expected ndql0:operand but found:")
                      (attribute:errsyn-nds
                        ((ndql0:errsyn
                           ((at:source
                              framed-items-sep-0
                              "test-session"
                              (56 1 55)
                              (57 1 56))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-nds
                               ((ndql0:punc
                                  ((at:source
                                     source-generator-lex
                                     "test-session"
                                     (56 1 55)
                                     (57 1 56))
                                    (at:lexeme ",")))))))))))))
             ]
           )
      (nds-equal? trimmed expected-trimmed)
      ))
  (test-hook trim-nd-err-test-2)

