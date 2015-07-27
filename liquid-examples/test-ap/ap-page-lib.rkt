#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

  this file contains page rendering routines that are specific to the application,
  in contrast to those found in htmle-lib.rkt which are of general use.

  variable names ending in '-htmle' are html parsed into xexp form.

  pages are hooked via (page-hook "name" rendering-function), see "http-session.rkt"

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

  (require liquid/misc-lib)
  (require liquid/htmle-lib)
  (require liquid/tokens)
  (require liquid/filter)
  (require liquid/webi-lib)

  (require liquid/parser-tokens)


;;--------------------------------------------------------------------------------
;; gets the query extension off of a url
;;
;; input: url
;; output: "query=" value out of the url if any, else ""
;;
  (define (query-extension url)
    (let*
      (
        [url-params (url-query url)] ; takes the parameters off of the url string
        [query-extension (assoc 'query url-params)] ; finds the paramter starting "query="
        [query-str (if query-extension (cdr query-extension) "")] ; gets the value (right side of the '=')
        )
      query-str
      ))


;;--------------------------------------------------------------------------------
;; pull predicates out of a query 
;;
;; input: a parse tree
;; output: list of predicates found in the parse tree
;;
  (define (filter-preds the-parse) 
    (marked-toks (list the-parse) (λ(t)(type-is 'tok:pred t))); see filter.rkt for marked-toks
    )


;;--------------------------------------------------------------------------------
;; htmle for a table that provides usage message for all available web interface predicates
;;
;;   Web interface predicates correspond to interfaces for pulling data from
;;   websites.  Other predicates may also be defined
;; 
  (define (webi-preds-usage-htmle)
    (let(
          [surrounding-box-htmle
            `(fieldset
               (legend "Web Interface Predicates")
               )
            ]
          [row (λ(e)
                 (let*(
                        [name (car e)]
                        [webi (cdr e)]
                        [usage (list-ref webi webi:usage)]
                        [table-row `(tr (td ,name) (td ,(usage)))]
                        )
                   table-row
                   ))
            ]
          )
      (let(
            [table-htmle
              (cons
                'table
                (map row (webi-list))
                )
              ]
            )
        (append 
          surrounding-box-htmle
          (list table-htmle)
          )
        )))


;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "ap-page-lib"
    query-extension
    filter-preds
    webi-preds-usage-htmle
    )


