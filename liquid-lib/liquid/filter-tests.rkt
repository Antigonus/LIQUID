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
  (define (marked-toks-test-0)
    (let*(
           [          in (open-input-string "d(n_t,)")]
           [       parse (parser in)]
           [          ts (list parse)]
           [the-marked-toks (marked-toks ts ndhas-err)]
           [expected-marked-toks 
             '((tk:operand
                 ((at:source rule-operand "test-session" (6 1 5) (7 1 6))
                   (attribute:errsyn-mess "expected tk:operand but found:")
                   (attribute:errsyn-nds
                     ((tk:errsyn
                        ((at:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                          (attribute:errsyn-mess "list ended with a separator: ")
                          (attribute:errsyn-nds
                            ((tk:punc
                               ((at:source
                                  source-generator-lex
                                  "test-session"
                                  (6 1 5)
                                  (7 1 6))
                                 (at:lexeme ","))))))))))))
             ]
           )
      (.eq. the-marked-toks expected-marked-toks)
      ))
  (test-hook marked-toks-test-0)

  (define (marked-toks-test-1)
    (let*(
           [         in (open-input-string "qed(a,_,c)&dlp(\"a\", __ , seven) & wikipedia(number_theory,)")]
           [       parse (parser in)]
           [          ts (list parse)]
           [the-marked-toks (marked-toks ts ndhas-err)]
           [expected-marked-toks 
             '((tk:operand
                 ((at:source rule-operand "test-session" (21 1 20) (23 1 22))
                   (attribute:errsyn-mess "expected tk:operand but found:")
                   (attribute:errsyn-nds
                     ((tk:punc
                        ((at:source
                           source-generator-lex
                           "test-session"
                           (21 1 20)
                           (22 1 21))
                          (at:lexeme "_")))
                       (tk:punc
                         ((at:source
                            source-generator-lex
                            "test-session"
                            (22 1 21)
                            (23 1 22))
                           (at:lexeme "_")))))))
                (tk:operand
                  ((at:source rule-operand "test-session" (58 1 57) (59 1 58))
                    (attribute:errsyn-mess "expected tk:operand but found:")
                    (attribute:errsyn-nds
                      ((tk:errsyn
                         ((at:source
                            framed-items-sep-0
                            "test-session"
                            (58 1 57)
                            (59 1 58))
                           (attribute:errsyn-mess "list ended with a separator: ")
                           (attribute:errsyn-nds
                             ((tk:punc
                                ((at:source
                                   source-generator-lex
                                   "test-session"
                                   (58 1 57)
                                   (59 1 58))
                                  (at:lexeme ","))))))))))))
             ]
           )
      (.eq. the-marked-toks expected-marked-toks)
      ))
    (test-hook marked-toks-test-1)

;;--------------------------------------------------------------------------------
;;
  (define (trim-nderr-test-0) 
    (let*(
           [in (open-input-string "d(n_t,)")]
           [parse (parser in)]
           [trimmed (trim-nderr parse)]
           [expected-trimmed 
             '(tk:conjunction
                ((at:source rule-conjunction "test-session" (1 1 0) (8 1 7)))
                (tk:pred
                  ((at:source rule-pred "test-session" (1 1 0) (8 1 7))
                    (at:value "d"))
                  (tk:operand
                    ((at:source rule-operand "test-session" (6 1 5) (7 1 6))
                      (attribute:errsyn-mess "expected tk:operand but found:")
                      (attribute:errsyn-nds
                        ((tk:errsyn
                           ((at:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-nds
                               ((tk:punc
                                  ((at:source
                                     source-generator-lex
                                     "test-session"
                                     (6 1 5)
                                     (7 1 6))
                                    (at:lexeme ",")))))))))))))
             ]
           )
      (.eq. trimmed expected-trimmed)
      ))
  (test-hook trim-nderr-test-0)

  (define (trim-nderr-test-1) 
    (let*(
           [in (open-input-string "d(n_t,2)")]
           [parse (parser in)]
           [trimmed (trim-nderr parse)]
           )
      (not trimmed)
      ))
  (test-hook trim-nderr-test-1)

  (define (trim-nderr-test-2) 
    (let*(
           [in (open-input-string "qed(a,_,c)&dlp(\"a\", 3, seven) & wikipedia(number_theory,)")]
           [parse (parser in)]
           [trimmed (trim-nderr parse)]
           [expected-trimmed 
             '(tk:conjunction
                ((at:source rule-conjunction "test-session" (1 1 0) (58 1 57)))
                (tk:pred
                  ((at:source rule-pred "test-session" (33 1 32) (58 1 57))
                    (at:value "wikipedia"))
                  (tk:operand
                    ((at:source rule-operand "test-session" (56 1 55) (57 1 56))
                      (attribute:errsyn-mess "expected tk:operand but found:")
                      (attribute:errsyn-nds
                        ((tk:errsyn
                           ((at:source
                              framed-items-sep-0
                              "test-session"
                              (56 1 55)
                              (57 1 56))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-nds
                               ((tk:punc
                                  ((at:source
                                     source-generator-lex
                                     "test-session"
                                     (56 1 55)
                                     (57 1 56))
                                    (at:lexeme ",")))))))))))))
             ]
           )
      (.eq. trimmed expected-trimmed)
      ))
  (test-hook trim-nderr-test-2)

