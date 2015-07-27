#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  
  2014-10-01 twl created

|#

#lang racket

;;--------------------------------------------------------------------------------
;; library includes
;;
  (require html)
  (require net/url)
  (require net/url net/uri-codec)
  (require (planet neil/html-parsing:2:0))
  (require (planet neil/html-writing:2:0))
  (require racket/match)

  (require liquid/misc-lib)
  (require liquid/http-session)
  (require liquid/http-server-pages)
  (require liquid/htmle-lib)
  (require liquid/tokens)
  (require liquid/filter)
  (require liquid/webi-lib)

  (require keyworld-1-tokens)
  (require "webi-dblp.rkt")
  (require "ap-page-lib.rkt")
  (require "ap-page-webi-pred-test-data.rkt")

;;--------------------------------------------------------------------------------
;; query extension form
;;
;; input: url
;; output: htmle for a form that requests entry of a single web predicate
;;    
  (define (keyword-search-1-request-form url)
    (let*
      (
        [the-query (query-extension url)] ; used as default in form value
        [the-form
          `(
             (form (@ 
                     (name "input") 
                     (method "get") 
                     (accept-charset="UTF-8")) ; is this necessary as the document already sets it
               (fieldset
                 (legend "Deep Web Search - keyword input")
                 "Input a single web interface predicate: "
                 (input (@ 
                          (type "text") 
                          (name "query")
                          (value ,the-query)
                          (size "80")
                          ))
                 (input (@
                          (type "submit")
                          (value "Submit")
                          ))
                 ))
             )
          ]
        )
      (list the-form (if (string=? the-query "") #f the-query))
      ))


;;--------------------------------------------------------------------------------
;; page body
;;
;; input: the url we arrived on the page with including query extentions if any
;; ouptut: htmle used for rendering the page body
;;
;; The user is shown a form for entering a keyword search specifically tailored
;; to the ss payments keyword search (a very simple system).
;;
;;

  ;; a place to put all the info collapse boxes for the test page
  ;;
   (define info-htmle '())
   (define (info-reset) (set! info-htmle '()))
   (define (info-hook . info) (set! info-htmle (append info-htmle info)))

  ;; -wi means "with info"
  ;; input: the arrival url (i.e. the url used to get here),  an info object
  ;; output: htmle form
  ;;         query found on the arrival url, or #f if none found
  ;;
    (define (get-form-query-wi arrival-url)
      (let*(
             [form/query (keyword-search-1-request-form arrival-url)]
             [the-form-htmle (car form/query)]
             [arrival-query (cadr form/query)]
             )
        (cond
          [arrival-query
            (let*(
                  [arrival-query-xexp `(p , arrival-query)]
                  [arrival-query-collapse-box-htmle (car (collapse-box-htmle "the query" arrival-query-xexp))]
                  )
              (info-hook arrival-query-collapse-box-htmle)
              )
            ]
          )
        (list the-form-htmle arrival-query)
        ))

  ;; input: the arrival query 
  ;; output: parse of the arrival query
  ;;
    (define (parse-query-wi arrival-query)
      (let*(
            [parse/parse-errors (parser* arrival-query)]
            [arrival-query-parse  (car parse/parse-errors)]
            [parse-errors (cadr parse/parse-errors)]
            [arrival-query-parse-htmle `(pre ,(->pretty-string arrival-query-parse))]
            [parse-errors-htmle `(pre ,(->pretty-string parse-errors))]
            [parse-collapse-box-htmle (car (collapse-box-htmle "the query parse" arrival-query-parse-htmle))]
            [parse-errors-collapse-box-htmle (car (collapse-box-htmle "query parse errors" parse-errors-htmle))]
            )
        (info-hook  parse-collapse-box-htmle parse-errors-collapse-box-htmle)
        arrival-query-parse
        ))

  ;; input: the arrival query parse
  ;; output: the single predicate found or false
  ;;
    (define (get-pred-wi arrival-query-parse)
      (let*(
             [pred/semantic-errors (filter-webi-pred arrival-query-parse)] 
             [pred (car pred/semantic-errors)]
             [semantic-errors (cadr pred/semantic-errors)]
             [semantic-errors-htmle `(pre ,(->pretty-string semantic-errors))]
             [semantic-errors-collapse-box-htmle (car (collapse-box-htmle "semmantic errors" semantic-errors-htmle))]
             )
        (info-hook semantic-errors-collapse-box-htmle)
        pred
        ))

  ;; webi lookup, with info
  ;; input: the pred found in the arrival-query-parse -- this must be an webi-web pred
  ;; output: webi broken out into components
  ;;
    (define (find-webi-wi pred)
      (let*(
             [i (find-webi-simple pred)]
             [i-usage  (list-ref i webi:usage)]
             [i-url    (list-ref i webi:url)]
             [i-get    (list-ref i webi:get)]
             [i-scrape (list-ref i webi:scrape)]

             [pred-usage-mess (i-usage)]
             [pred-usage-collapse-box-htmle (car (collapse-box-htmle "pred usage" `(p ,pred-usage-mess)))]
             )
        (info-hook pred-usage-collapse-box-htmle)
        (list i i-usage i-url i-get i-scrape)
        ))

  ;; create url for fetching the web page, so called webi-url
  ;; input: i-url interface from webi, pred
  ;; output: webi-url
  ;;
    (define (webi-url-wi i-url pred)
      (let*(
             [webi-url (i-url pred)]
             [webi-url-htmle (if webi-url `(p ,(url->string webi-url)) `(p "#f"))]
             [webi-url-collapse-box-htmle (car (collapse-box-htmle "search url" webi-url-htmle))]
             )
        (info-hook webi-url-collapse-box-htmle)
        webi-url
        ))
        
  ;; fetches the page from the website
  ;;
  ;; input: pred, web
  ;; output: webi-url, webi-url for display, fetch result, fetch result for display, parsed fetch result for display
  ;;
  ;; the fetch returns htmle .. we present this literally in using 'pre to be able to see the parse tree of the html doc
  ;;
    (define (fetch-page-wi i-get webi-url)
      (let*(
             [fetched-htmle (i-get webi-url)] ; this could take a while, note current-page-fetch-timeout
             [fetched-htmle-collapse-box-htmle (car (collapse-box-htmle "fetched-html" (or fetched-htmle '(p "#f"))))]
             [fetched-htmle-parse-collapse-box-htmle (car (collapse-box-htmle "fetched-html-parse" `(pre ,(->pretty-string fetched-htmle))))]
             )
        (info-hook fetched-htmle-collapse-box-htmle fetched-htmle-parse-collapse-box-htmle)
        fetched-htmle
        ))

  ;; page scrape
  ;;
  ;; input: page fetch as htmle
  ;; output: scrape of the htmle for use, scrape of the htmle wrapped in 'pre ready for direct rendering
  ;;
    (define (scrape-wi i-scrape fetched-htmle)
      (let*(
             [scrape-data (i-scrape fetched-htmle)]
             [scrape-collapse-box-htmle (car (collapse-box-htmle "scrape result" `(pre ,(->pretty-string scrape-data))))]
             )
        (info-hook scrape-collapse-box-htmle)
        scrape-data
        ))

     (define (page-test-keyword-body arrival-url)
       (info-reset)  ; should instead have (with-info ...) as threads are going to get messed up!!!

       (match-let(
                   [(list the-form-htmle arrival-query)  (get-form-query-wi arrival-url)]
                   )

         ;; this cond block has the side effect of setting up our info list
         (cond
           [arrival-query
             (let*(
                    [arrival-query-parse (parse-query-wi arrival-query)]
                    [pred (get-pred-wi arrival-query-parse)]
                   )
               (cond
                 [pred 
                   (match-let*(
                                [(list i i-usage i-url i-get i-scrape)  (find-webi-wi pred)]
                                )
                     (cond
                       [(is-webi i)
                          (let(
                               [webi-url (webi-url-wi i-url pred)]
                               )
                           (cond
                             [webi-url
                               (let(
                                     [fetched-htmle (fetch-page-wi i-get webi-url)]
                                     )
                                 (cond
                                   [fetched-htmle
                                     (let(
                                           [scrape-data   (scrape-wi i-scrape fetched-htmle)]
                                           )
                                       (void) ; normally scrape data would go to the database
                                       )]))]))]))]))])

         ;; the page body is constructed from the form and info list
         (let*(
               [basic-body
                 `(body 
                    ,(webi-preds-usage-htmle)
                    (br)
                    ,the-form-htmle
                    )
                 ]
               [body-with-info 
                 (append basic-body info-htmle)
                 ]
                )
           body-with-info
           )))

               
;;--------------------------------------------------------------------------------
;; this test page accepts queriest that contain a single predicate, there that
;; predicate is a web predicate
;;
;; input: a parse tree
;; output: a single web predicate or #f, an error message or #f
;;
  (define (filter-webi-pred the-parse)
    (let(
          [preds  (filter-preds the-parse)]
          )
      (cond
        [(null? preds) (list '() "did not find any predicates")]
        [(length> preds 1) (list preds "found more than one pred")]
        [else
          (let*(
                 [pred (car preds)]
                 [name (car (tok-value pred))]
                 )
            (cond
              [(not (is-webi name))
                (let(
                      [mess
                        (string-append
                          "not an web predicate: \""
                          (->pretty-string pred)
                          "\""
                          )
                        ]
                      )
                  (list pred mess)
                  )
                ]
              [else
                (list pred #f)
                ]
              ))
          ]
        )))

    (define (filter-pred-test-0)
      (equal?
        (filter-preds filter-pred-test-data-0)
        filter-pred-test-data-1
        ))

    (define (filter-pred-test-1)
      (let(
            [filter-result (filter-webi-pred filter-pred-test-data-0)]
            )
        (and
          (equal?
            (car filter-result)
            (car filter-pred-test-data-1)
            )
            (not (cadr filter-result))
          )))

;;--------------------------------------------------------------------------------
;;  the web page is assembled here
;;
;; input: ssl context, arrival url
;; output: htmle for the webpage
;;
    (define (page-test-keyword the-session-context arrival-url)
      (let*
        (
          [header   (html-header collapse-box-script-str)]
          [body     (xexp->html (page-test-keyword-body arrival-url))]
          [document (html-str header body)]
          )
        (parameterize [(current-output-port (session-context-out the-session-context))]
          (display (http-response)) ; comment this out to see the document as text on browser page
          (display document)
          )
        ))
     (page-hook "/test/webi-pred" page-test-keyword)



;;--------------------------------------------------------------------------------
;; trace/untrace (note use of editor syntax transforms .. occurs even before parse time!
;;
  (define (ap-page-keyword-internal-trace)
    (trace
      keyword-search-1-request-form
      info-reset
      info-hook
      get-form-query-wi
      parse-query-wi
      get-pred-wi
      find-webi-wi
      webi-url-wi
      fetch-page-wi
      scrape-wi
      page-test-keyword-body
      filter-webi-pred
      filter-pred-test-0
      filter-pred-test-1
      ))

  (define (ap-page-keyword-internal-untrace)
    (untrace
      keyword-search-1-request-form
      info-reset
      info-hook
      get-form-query-wi
      parse-query-wi
      get-pred-wi
      find-webi-wi
      webi-url-wi
      fetch-page-wi
      scrape-wi
      page-test-keyword-body
      filter-webi-pred
      filter-pred-test-0
      filter-pred-test-1
      ))  

;;--------------------------------------------------------------------------------
;; provides
;;   this module hooks the pages and otherwise does not provide anything
;;
  (provide
    ap-page-keyword-internal-trace
    ap-page-keyword-internal-untrace
    )

  (provide-with-trace "ap-page-keyword"
    page-test-keyword
    )

;; for debug:
;;(provide (all-defined-out))
