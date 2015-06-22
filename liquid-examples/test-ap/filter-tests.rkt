;;--------------------------------------------------------------------------------
;; tests for filter.rkt 
;;   2015-05-25T08:53:03Z created twl
;;
;; These tests are here because they make use of query-parser.rkt
;;

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "liquid/lynch-lib.rkt")
  (require "liquid/filter.rkt")

;;--------------------------------------------------------------------------------
;;
  (define (marked-toks-test-0)
    (let*(
           [          in (open-input-string "d(n_t,)")]
           [       parse (query-parser in)]
           [          ts (list parse)]
           [the-marked-toks (marked-toks ts tok-has-err)]
           [expected-marked-toks 
             '((tok:operand
                 ((attribute:source rule-operand "test-session" (6 1 5) (7 1 6))
                   (attribute:errsyn-mess "expected tok:operand but found:")
                   (attribute:errsyn-ts
                     ((tok:errsyn
                        ((attribute:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                          (attribute:errsyn-mess "list ended with a separator: ")
                          (attribute:errsyn-ts
                            ((tok:punc
                               ((attribute:source
                                  source-generator-lex
                                  "test-session"
                                  (6 1 5)
                                  (7 1 6))
                                 (attribute:lexeme ","))))))))))))
             ]
           )
      (.eq. the-marked-toks expected-marked-toks)
      ))
  (test-hook marked-toks-test-0)

  (define (marked-toks-test-1)
    (let*(
           [         in (open-input-string "qed(a,_,c)&dlp(\"a\", __ , seven) & wikipedia(number_theory,)")]
           [       parse (query-parser in)]
           [          ts (list parse)]
           [the-marked-toks (marked-toks ts tok-has-err)]
           [expected-marked-toks 
             '((tok:operand
                 ((attribute:source rule-operand "test-session" (21 1 20) (23 1 22))
                   (attribute:errsyn-mess "expected tok:operand but found:")
                   (attribute:errsyn-ts
                     ((tok:punc
                        ((attribute:source
                           source-generator-lex
                           "test-session"
                           (21 1 20)
                           (22 1 21))
                          (attribute:lexeme "_")))
                       (tok:punc
                         ((attribute:source
                            source-generator-lex
                            "test-session"
                            (22 1 21)
                            (23 1 22))
                           (attribute:lexeme "_")))))))
                (tok:operand
                  ((attribute:source rule-operand "test-session" (58 1 57) (59 1 58))
                    (attribute:errsyn-mess "expected tok:operand but found:")
                    (attribute:errsyn-ts
                      ((tok:errsyn
                         ((attribute:source
                            framed-items-sep-0
                            "test-session"
                            (58 1 57)
                            (59 1 58))
                           (attribute:errsyn-mess "list ended with a separator: ")
                           (attribute:errsyn-ts
                             ((tok:punc
                                ((attribute:source
                                   source-generator-lex
                                   "test-session"
                                   (58 1 57)
                                   (59 1 58))
                                  (attribute:lexeme ","))))))))))))
             ]
           )
      (.eq. the-marked-toks expected-marked-toks)
      ))
    (test-hook marked-toks-test-1)

;;--------------------------------------------------------------------------------
;;
  (define (trim-tok-err-test-0) 
    (let*(
           [in (open-input-string "d(n_t,)")]
           [parse (query-parser in)]
           [trimmed (trim-tok-err parse)]
           [expected-trimmed 
             '(tok:conjunction
                ((attribute:source rule-conjunction "test-session" (1 1 0) (8 1 7)))
                (tok:pred
                  ((attribute:source rule-pred "test-session" (1 1 0) (8 1 7))
                    (attribute:value "d"))
                  (tok:operand
                    ((attribute:source rule-operand "test-session" (6 1 5) (7 1 6))
                      (attribute:errsyn-mess "expected tok:operand but found:")
                      (attribute:errsyn-ts
                        ((tok:errsyn
                           ((attribute:source framed-items-sep-0 "test-session" (6 1 5) (7 1 6))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-ts
                               ((tok:punc
                                  ((attribute:source
                                     source-generator-lex
                                     "test-session"
                                     (6 1 5)
                                     (7 1 6))
                                    (attribute:lexeme ",")))))))))))))
             ]
           )
      (.eq. trimmed expected-trimmed)
      ))
  (test-hook trim-tok-err-test-0)

  (define (trim-tok-err-test-1) 
    (let*(
           [in (open-input-string "d(n_t,2)")]
           [parse (query-parser in)]
           [trimmed (trim-tok-err parse)]
           )
      (not trimmed)
      ))
  (test-hook trim-tok-err-test-1)

  (define (trim-tok-err-test-2) 
    (let*(
           [in (open-input-string "qed(a,_,c)&dlp(\"a\", 3, seven) & wikipedia(number_theory,)")]
           [parse (query-parser in)]
           [trimmed (trim-tok-err parse)]
           [expected-trimmed 
             '(tok:conjunction
                ((attribute:source rule-conjunction "test-session" (1 1 0) (58 1 57)))
                (tok:pred
                  ((attribute:source rule-pred "test-session" (33 1 32) (58 1 57))
                    (attribute:value "wikipedia"))
                  (tok:operand
                    ((attribute:source rule-operand "test-session" (56 1 55) (57 1 56))
                      (attribute:errsyn-mess "expected tok:operand but found:")
                      (attribute:errsyn-ts
                        ((tok:errsyn
                           ((attribute:source
                              framed-items-sep-0
                              "test-session"
                              (56 1 55)
                              (57 1 56))
                             (attribute:errsyn-mess "list ended with a separator: ")
                             (attribute:errsyn-ts
                               ((tok:punc
                                  ((attribute:source
                                     source-generator-lex
                                     "test-session"
                                     (56 1 55)
                                     (57 1 56))
                                    (attribute:lexeme ",")))))))))))))
             ]
           )
      (.eq. trimmed expected-trimmed)
      ))
  (test-hook trim-tok-err-test-2)

