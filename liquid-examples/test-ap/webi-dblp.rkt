#|
  webi for  'dblp.uni-trier.de'
  created: 2014-12-30T06:58:14Z twl

  dblp() 

  see the usage function for more information on usage of the dblp webi predicate

  see webi-lib.rkt for a definition of the webi

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require db)
  (require html)
  (require net/url net/uri-codec)
  (require (planet neil/html-parsing:2:0))
  (require liquid/lynch-lib.rkt) 
  (require liquid/tokens.rkt)
  (require liquid/filter.rkt)
  (require liquid/realtime.rkt)

  (require liquid/webi-lib.rkt)
  (require "webi-dblp-scrape-test-data-0.rkt")
  (require "webi-dblp-scrape-test-data-1.rkt")

  (require query-parser-tokens.rkt)
  (require query-parser-well-formed.rkt)
  (require "dbi-dblp.rkt")

;;--------------------------------------------------------------------------------
;; webi-dblp-usage
;;
;; need to add the superscripts etc.
;;
  (define (webi-dblp-usage) "dblp(author, venue, type, year)")


;;--------------------------------------------------------------------------------
;; webi-dblp-url
;;
;;   input: a predicate
;;   output:  search url, or #f if all operands are in predicate are patterns
;;
;;   we don't look at the pred name, that should be sorted out before calling the webi
;;
  (define dblp-facets '("author" "venue" "type" "year")) ; dblp calls the keys on the query 'facets'
  (define (webi-dblp-url a-pred)
    (and
      a-pred
      (let(
            [ops (tok-children a-pred)]
            )
        (and
          (andmap well-formed-dblp-operand ops)
          (webi-dblp-url-1 ops)
          ))
      ))

  (define test-pred-0
    '(tok:pred
       ((attribute:source rule-pred "test-session" (1 1 0) (19 1 18))
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
         ((attribute:source rule-operand "test-session" (21 1 20) (22 1 21)))
         (tok:number
           ((attribute:source
              source-generator-lex
              "test-session"
              (21 1 20)
              (22 1 21))
             (attribute:lexeme "2014")
             (attribute:value 2014)))))
    )

  (define (webi-dblp-url-test-0)
    (let*(
          [a-pred test-pred-0]
          [url (webi-dblp-url a-pred)]
          )
      (eqv? 1 1
        )))

 ;; variation of well-formed-operand found in query-parser-well-formed.rkt
 (define (well-formed-dblp-operand an-operand) ; should add more checks ..
   (and
     (eqv? (type an-operand) (tok:operand))
     (andmap 
       (λ(op)
         (let*(
                [op-type (type op)]
                )
           (or
             (eqv? op-type (tok:number))
             (eqv? op-type (tok:string))
             (eqv? op-type (tok:pattern))
             )
           ))
       (tok-children an-operand)
       )
     ))

  (define (dblp-operand->string op)
    (cond
      [(not (well-formed-dblp-operand op)) #f]
      [else
        (let*(
               [child (car (tok-children op))]
               [child-type (type child)]
               )
          (cond
            [(eqv? child-type (tok:number))  (number->string (car (tok-value child)))]
            [(eqv? child-type (tok:string))  (drop-end-chars (car (tok-lexeme child)))]
            [(eqv? child-type (tok:pattern))  "_"]
            ))
        ]
      ))

  ;; input: operands from a corresponding dblp predicate
  ;; output: the-url
  (define (webi-dblp-url-1 ops)
    (let*(
           [args (map dblp-operand->string ops)]
           [the-url (webi-dblp-url-from-args* args)]
           )
      the-url
      ))

  (define test-ops-0
    '((tok:operand
        ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
        (tok:string
          ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
            (attribute:lexeme "\"Dali\""))))
       (tok:operand
         ((attribute:source rule-operand "test-session" (19 1 12) (14 1 19)))
         (tok:pattern
           ((attribute:source rule-pattern "test-session" (19 1 12) (14 1 19)))))
       (tok:operand
         ((attribute:source rule-operand "test-session" (15 1 14) (16 1 15)))
         (tok:pattern
           ((attribute:source rule-pattern "test-session" (15 1 14) (16 1 15)))))
       (tok:operand
         ((attribute:source rule-operand "test-session" (6 1 5) (12 1 11)))
         (tok:string
           ((attribute:source source-generator-lex "test-session" (6 1 5) (12 1 11))
             (attribute:lexeme "\"Salvadore\"")))))
    )
   (define (webi-dblp-url-1-test-0)
     (string=?
      (url->string (webi-dblp-url test-pred-0))
       "http://dblp.uni-trier.de/search/publ?q=author%3ACali+year%3A2014"
       ))
   (test-hook webi-dblp-url-1-test-0)

  ;; Builds a dblp search url given a list of arguments that would normally go to 
  ;; a dblp search predicate.  This is a useful test entry point.
  ;;
  ;;  input: search args in order, an arg may be a pattern
  ;; output: the url for a dblp.uni-trier.de search, or #f if all args are patterns
  ;;
  (define (webi-dblp-url-from-args . args) (webi-dblp-url-from-args* args))
  (define (webi-dblp-url-from-args*  args) 
    (let(
          [dblp-query (webi-dblp-url-query-0 dblp-facets args)]
          )
      (cond
        [dblp-query  (string->url (string-append "http://dblp.uni-trier.de/search/publ?q=" dblp-query))]
        [else #f]
        )))

  (define (is-pat arg) (string=? arg "_"))
  (define (prepend-facet facet arg)  (if (is-pat arg) "" (string-append facet ":" arg ":")))

  ;(trace webi-dblp-url-from-args webi-dblp-url-query-0 webi-dblp-url-query-1)
  ;(untrace webi-dblp-url-from-args webi-dblp-url-query-0 webi-dblp-url-query-1)
  (define (webi-dblp-url-query-0 facets args) ; for car non-pattern arg
    (cond
      [(or (null? facets) (null? args)) #f] ; no car non-pattern arg found
      [else
        (let(
              [facet (car facets)]
              [r-facets (cdr facets)]
              [arg (car args)]
              [r-args (cdr args)]
              )
          (cond
            [(is-pat arg) (webi-dblp-url-query-0 r-facets r-args)]
            [else
              (string-append facet ":" arg (webi-dblp-url-query-1 r-facets r-args))
              ]
            ))
        ]
      ))

  (define (webi-dblp-url-query-1 facets args) ; for remaining args
    (cond
      [(or (null? facets) (null? args)) ""]
      [else
        (let(
              [facet (car facets)]
              [r-facets (cdr facets)]
              [arg (car args)]
              [r-args (cdr args)]
              )
          (cond
            [(is-pat arg) (webi-dblp-url-query-1 r-facets r-args)]
            [else 
              (string-append " " facet ":" arg (webi-dblp-url-query-1 r-facets r-args))
              ]
            ))
        ]
      ))

   #|
     ; dblp puts literal space, but string->url will turn the space to a '+'

     (define u (webi-dblp-url-from-args "Andrea_J._Goldsmith" "_" "_" "2014"))
     (url->string u)
       "http://dblp.uni-trier.de/search/publ?q=author%3AAndrea_J._Goldsmith+year%3A2014"
                                                                           ^
                                                 dblp search output puts a %20 here, not '+'

      the dblp url parser appears to accept the '+'
   |#
          
;;--------------------------------------------------------------------------------
;; webi-dblp-get
;;
;;   who would have guessed that building the url would be 100s of lines of code, but 
;;   getting the page would be 1?
;;
  (define webi-dblp-get get-webpage)

;;--------------------------------------------------------------------------------
;; webi-dblp-insert
;;

  ;; input: name for the search result,  list of citations
  ;; output:  a list of exceptions.. side effect of writing the tables with successful scrapes
  ;;
    (define (webi-dblp-insert search-result-name citations)
      (create-citation-schema search-result-name)
      (for-each 
        (λ(citation) (apply insert-citation (cons search-result-name citation)))
        citations
        ))

  ;; input: page-body html parse from a dblp search result
  ;; output: list of citations 
  ;;
    (define (webi-dblp-scrape parsed-page-body)
      (let*(
             [citation-div (filter-prefix '(div (@ (class "data"))) parsed-page-body)]
             [citations-exceptions (map webi-dblp-scrape-citation-div citation-div)]
             [citations (filter (λ(e) (type-is 'scraping e)) citations-exceptions)] ; manifold-out
             [exceptions (filter (λ(e) (type-is 'exception e)) citations-exceptions)] ; manifold-out
             )
        (list citations exceptions)
      ))

    (define (webi-dblp-scrape-test-0)
      (equal?
        (webi-dblp-scrape webi-dblp-scrape-test-data-0)
        webi-dblp-scrape-test-data-1
        ))
    (test-hook webi-dblp-scrape-test-0)

  ;; input: a (class "data") div xexp from a dblp search result
  ;;        we take such a div to be a citation to an article
  ;; output: a list, (authors, venue, type, year)
  ;;
    (define (webi-dblp-scrape-citation-div citation-div)
      (with-handlers
        (
          [(λ(v) #t) ; this handler catches anything
            (λ(v) (list 'exception (->pretty-string v) citation-div))
            ]
          )
        (let(
              [a-sections    (filter-prefix '(a) citation-div)]
              [title-section (filter-prefix '(span (@ (class "title"))) citation-div)]
              [year-section  (last citation-div)]
              )
          (let(
                [venue-section (last a-sections)]
                [author-sections (drop-right a-sections 1)]
                )
            (let(
                  [authors (map webi-dblp-scrape-auth author-sections)]
                  [venu (webi-dblp-scrape-venue venue-section)]
                  [type (webi-dblp-scrape-type (car (filter-prefix '(href) venue-section)))]
                  [year (webi-dblp-scrape-year year-section)]
                  )
              (list 'scraping authors venu type year)
              )))))

    (define (webi-dblp-scrape-auth auth-section)
      (last auth-section)
      )
    (define (webi-dblp-scrape-venue venu-section)
      (last venu-section)
      )

    ;; pulls the type out of the link text
    (define (webi-dblp-scrape-type link)
      (let*(
            [link-text (cadr link)]
            [type (cadr (regexp-match #rx"/db/(.*?)/" link-text))]
            )
        type
      ))
    (define (webi-dblp-scrape-year year-section)
      (let*(
             [match-result (regexp-match #rx"\\((.*?)\\)" year-section)]
             [year-string (cadr match-result)]
             [year (string->number year-string)]
             )
        year
      ))

  (define webi-dblp-scrape-a-citation-test-data-0
    '(div
      (@ (class "data"))
      (a
       (@ (href "http://dblp.uni-trier.de/pers/hd/c/Caliskan:Sina_Yamac"))
       "Sina Yamac Caliskan")
      ", "
      (a
       (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tabuada:Paulo"))
       "Paulo Tabuada")
      ":"
      (br)
      " "
      (span
       (@ (class "title"))
       "Towards Kron reduction of generalized electrical networks.")
      " "
      (a
       (@
        (href
         "http://dblp.uni-trier.de/db/journals/automatica/automatica50.html#CaliskanT14"))
       "Automatica 50(10)")
      ": 2586-2590 (2014)")
    )

    (define (webi-dblp-scrape-citation-div-test-0)
      (equal?
        (webi-dblp-scrape-citation-div webi-dblp-scrape-a-citation-test-data-0)
        '(scraping ("Sina Yamac Caliskan" "Paulo Tabuada") "Automatica 50(10)" "journals" 2014)
        ))
    (test-hook webi-dblp-scrape-citation-div-test-0)

#|

  (define (webi-dblp-insert-test)
    (let*(
           [table (webi-dblp-insert-test-0-create-table)]
           [html  (dblp-input-test-data-0)] ; see webi-dblp-html-data.rkt
           [data  (webi-dblp-insert-test-0-scrape html)] 
           [insert-status (webi-dblp-insert-test-0-do-insert table data)]
          )
      (equal? 1 1)
      ))

  (define (webi-dblp-insert-test-0-create-table)
    (let(
          [table       "dblp-test-0"]
          [command     "create temporary table"]
          [declaration "(author text ,venue text ,type text ,year integer)"]
          [statement   (string-append command " " table " " declaration)]
          )
      (query-exec pgc statement)
      table
      ))
|#

;;--------------------------------------------------------------------------------
;; webi definition and hook
;;
  (define webi-dblp (list webi-dblp-usage webi-dblp-url webi-dblp-get webi-dblp-scrape webi-dblp-insert))
  (webi-hook "dblp" webi-dblp)

;;--------------------------------------------------------------------------------
;; provides
;;
;;  hooks itself into the webis hash, see webi-lib.rkt
;;
