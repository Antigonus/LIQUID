#|
 deep web search application prototype main routine
 2014-12-30T06:58:14Z created twl

 may be used locally or as a webserver. when used locally open a broweser to
 127.0.0.1:8080/query to view the program (port number can be changed with an argument)



(require racket/runtime-path)
(define-runtime-path rtd "realtime-test-data.rkt")
(define (fun-timed-test-2)
  (define ip (open-input-file rtd))
  ... rest of function ...)
```

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require liquid/lynch-lib) 
  (require liquid/http-server)
  (require liquid/http-session)
  (require liquid/db-lib)
  (require liquid/filter-tests)

  (require "ap-page-webi-predicate.rkt")
  (require "ap-page-conjunction.rkt")

;;--------------------------------------------------------------------------------
;;
  (define (dbis-init)
    (db-lib-init)
    ;; call intialization code for db interfaces here
    )

;;--------------------------------------------------------------------------------
;; 
  (define (ap-pages-init)
    ;; call any initialization code for pages here
    ;;(ap-page-webi-predicate-internal-trace)
    #f ; no errors
    )

;;--------------------------------------------------------------------------------
;; 
  (define (webis-init)
    ;; call any initialization code for web interfaces here
    #f ; no errors
    )

;;--------------------------------------------------------------------------------
;; main
;;
  (define (run [portno 8080]) 

    ;; other tests pages may be spread thoughout the application code, grep page-hook
    (page-hook "/test/webi-predicate" page-test-webi-predicate)
    (page-hook "/test/conjunction" page-test-conjunction)

    (let(
          [dbis-init-errors (dbis-init)]
          [ap-pages-init-errors (ap-pages-init)]
          [webis-init-errors (webis-init)]
          )
      (cond
        [dbis-init-errors 
          (displayln 
            (string-append 
              "database init exception: " 
              (->pretty-string dbis-init-errors)))
          ]
        [ap-pages-init-errors 
          (displayln 
            (string-append
              "application init exception: " 
              (->pretty-string ap-pages-init-errors)))
          ]
        [webis-init-errors
          (displayln 
            (string-append 
              "web interfaces init exception: " 
              (->pretty-string webis-init-errors)))
          ]
        [else
          (http-server portno)
          ]
        )))

