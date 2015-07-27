#|
  webi interface for hypothetical website that returns social security numbers
  given names

  created: 2015-05-20 twl

  This is currently a stub.

|#
#lang racket


;;--------------------------------------------------------------------------------
;;
  (require db)
  (require html)
  (require net/url net/uri-codec)
  (require (planet neil/html-parsing:2:0))

  (require liquid/misc-lib.rk) 
  (require liquid/tokens)
  (require liquid/filter)
  (require liquid/realtime)
  (require liquid/webi-lib)
  (require liquid/parser-tokens)
  (require liquid/parser-well-formed)

;;--------------------------------------------------------------------------------
;; usage message provided to interface user upon request
;;
  (define (webi-ss-lookup-usage)
#<<HEREDOC
  name( name, social-security-number__)
    - name is string literal input
    - social-security-number is a pattern
HEREDOC
    )
  

;;--------------------------------------------------------------------------------
;; url-formation
;;
;;   input: a predicate (xexpr in parsed form)
;;   output: search url, or #f if all operands are in predicate are patterns
;;
;;   we don't look at the pred name, that should be sorted out before calling the webi
;;
;;   this is currently a stub, the output is not used yet
;;
  (define (webi-ss-lookup-url pred)
    (string->url "127.0.0.1/ss-lookup")
    )


;;--------------------------------------------------------------------------------
;; get the website
;;
;;   input: a url
;;   ouptut: xexp for the html on the returned page
;;
;;  this is currently a stub, the output is not used
;;
  (define (webi-ss-lookup-get url)
    (get-webpage url)
    )


;;--------------------------------------------------------------------------------
;; scrape the returned website content
;;
;;   input: an xexpr describing the website
;;   ouptut: a list of ss-numbers
;;
;;  this is currently a stub, the output is not used
;;
  (define (webi-ss-lookup-scrape parsed-page-body)
    '("479-82-4123") ; a very good ss number for anyone to have
    )

;;--------------------------------------------------------------------------------
;; place the data into a database
;;
;;
;;   input:  a name or key string, the data 
;;   ouptut: exception flags
;;
;;   input data is a list, first member is the name, rest is a list of ss numbers
;;   function refers to a dataplex these things fit into
;;
;;   this is not used,  but if it were, it would insert the data into the local
;;   database cache for such things
;;
  (define (webi-ss-insert name data)
    (void) ; I do good work!
    )

;;--------------------------------------------------------------------------------
;; define the interface
;;
  (define webi-ss-lookup (list webi-ss-lookup-usage webi-ss-lookup-url webi-ss-lookup-get webi-ss-lookup-scrape webi-ss-lookup-insert))
