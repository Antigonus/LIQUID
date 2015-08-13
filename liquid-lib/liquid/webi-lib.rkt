#|
  website interface lib
  created: 2014-12-30T06:58:14Z twl

  Specific web interface definitions occur in other files.  For example,
  webi-dblp.rkt.  

  This file contains useful code pieces used in defining web interfaces.

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require html)
  (require net/url net/uri-codec)
  (require (planet neil/html-parsing:2:0))
  (require "misc-lib.rkt") 
  (require "node.rkt")
  (require "parser-nodes.rkt")
  (require "parser-well-formed.rkt")
  (require "realtime.rkt")

;;--------------------------------------------------------------------------------
;; get a webpage
;;
;;  input: a well formed url to be used on the Internet
;;  output: xexp for the body of the webpage at that url,  or #f
;;
  (define (get-webpage url)
    (with-handlers 
      (
        [(λ(v) #t) (λ(v) #f)] ; handler for all exceptions, fetch fails upon exception
        ) 
      (and 
        url
        (let*(
               [get-function (λ(e#) (get-pure-port url))]
               [completed/result-stream (fun-timed (current-get-webpage-timeout) get-function)]
               [completed (car completed/result-stream)]
               [result-stream (cadr completed/result-stream)]
               [result (html->xexp result-stream)] ; invokes Neil's html parser
               )
          (and completed result)
          ))))

;;--------------------------------------------------------------------------------
;; web interface
;;  a web interface, or just 'webi' for short, is a list of the following functions:
;;
  (define webi:usage 0)
  (define webi:url 1)
  (define webi:get 2)
  (define webi:scrape 3)
  (define webi:insert 4)

  (define the-webis (make-hash))

  ;; here is an example interface that we use as the 'interface not found' place holder
  ;;
    (define not-found-webi
      (list
        (λ() "webi-not-found") ; usage
        (λ(pred) #f) ; url
        (λ(url)  #f) ;get web page
        (λ(parsed-body-htmle) #f) ; scrape
        (λ(table-name-prefix data) #f) ;insert into database
        ))

  (define (well-formed-webi webi) (length≥ webi 5)) ; could probably do a better check ..

  ;; puts a webi into the-webis hash
  ;; input: name for the webi, the webi
  ;; output: side effect only: puts webi in the-webis keyed by name
  (define (webi-hook name webi)
    (cond
      [(not (well-formed-webi webi))
        (let(
             [err-message
               (string-append
                 "in webi-hook, "
                 " \"" name  "\""
                 "webi not well-formed"
                 "."
                 )
               ]
              )
          (displayln err-message)
          (log err-message)
          #f
          )
        ]
      [else 
        (display "adding database query webi: ") 
        (display ((car webi)))
        (display " as \"")(display name)(display "\"")
        (newline)
        (hash-set! the-webis name webi)
        ]
      ))

  (define (webi-lookup name)
    (hash-ref the-webis name (λ() not-found-webi))
    )

  (define (is-webi-name name)
    (not (eqv? (webi-lookup name) not-found-webi))
    )

  (define (is-webi a-webi)
    (not (eqv? a-webi not-found-webi))
    )


  (define (webi-list) (hash->list the-webis))


  (define test-webi-0
    (list
      (λ() "test-webi-0") ; usage
      (λ(pred) #f) ; url
      (λ(url)  #f) ;get web page
      (λ(parsed-body-htmle) #f) ; scrape
      (λ(table-name-prefix data) #f) ;insert into database
      ))
  (webi-hook "test-webi-0" test-webi-0)

  (define (webi-test-0)
    (let*(
           [a-name (unique-to-session-name)]
           [test-0 (not (is-webi-name a-name))]
           [test-1 (is-webi-name "test-webi-0")]
           [test-2 (string=? ((car (webi-lookup "test-webi-0"))) "test-webi-0")]
          )
      (and 
        test-0
        test-1
        test-2
        )
      ))
  (test-hook webi-test-0)

  ;; works when the webi name and the pred name are the same
  (define (find-webi-simple pred)
    (cond
      [(not (well-formed-pred pred)) not-found-webi]
      [else
        (let*(
               [pred-name (car (ndvalue pred))]
               [the-webi (webi-lookup pred-name)]
               )
          the-webi
          )
        ]
      ))

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "webi-lib"
    webi-hook
    webi-lookup
    is-webi-name
    is-webi
    webi-list
    find-webi-simple
    get-webpage
    )

  (provide 
    webi:usage
    webi:url
    webi:get
    webi:scrape
    webi:insert
    )


  
