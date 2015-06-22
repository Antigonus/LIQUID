#lang racket
(provide
  filter-pred-test-data-0
  filter-pred-test-data-1
)


    ;; this is a top level parse of a query containing a single web predicate
    (define filter-pred-test-data-0
      '(tok:conjunction
         ((attribute:source rule-conjunction "test-session" (1 1 0) (22 1 21)))
         (tok:pred
           ((attribute:source rule-pred "test-session" (1 1 0) (22 1 21))
             (attribute:value "dblp"))
           (tok:operand
             ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
             (tok:string
               ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
                 (attribute:lexeme "\"Cali\""))))
           (tok:operand
             ((attribute:source rule-operand "test-session" (13 1 12) (14 1 13)))
             (tok:pattern
               ((attribute:source rule-pattern "test-session" (13 1 12) (14 1 13)))))
           (tok:operand
             ((attribute:source rule-operand "test-session" (15 1 14) (16 1 15)))
             (tok:pattern
               ((attribute:source rule-pattern "test-session" (15 1 14) (16 1 15)))))
           (tok:operand
             ((attribute:source rule-operand "test-session" (17 1 16) (21 1 20)))
             (tok:number
               ((attribute:source
                  source-generator-lex
                  "test-session"
                  (17 1 16)
                  (21 1 20))
                 (attribute:lexeme "2015")
                 (attribute:value 2015)))))))

    ;; this is a list of predicates from filter-pred-test-data-0 - there is only one member in the list
    (define filter-pred-test-data-1
      '((tok:pred
          ((attribute:source rule-pred "test-session" (1 1 0) (22 1 21))
            (attribute:value "dblp"))
          (tok:operand
            ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
            (tok:string
              ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
                (attribute:lexeme "\"Cali\""))))
          (tok:operand
            ((attribute:source rule-operand "test-session" (13 1 12) (14 1 13)))
            (tok:pattern
              ((attribute:source rule-pattern "test-session" (13 1 12) (14 1 13)))))
          (tok:operand
            ((attribute:source rule-operand "test-session" (15 1 14) (16 1 15)))
            (tok:pattern
              ((attribute:source rule-pattern "test-session" (15 1 14) (16 1 15)))))
          (tok:operand
            ((attribute:source rule-operand "test-session" (17 1 16) (21 1 20)))
            (tok:number
              ((attribute:source
                 source-generator-lex
                 "test-session"
                 (17 1 16)
                 (21 1 20))
                (attribute:lexeme "2015")
                (attribute:value 2015)))))))

