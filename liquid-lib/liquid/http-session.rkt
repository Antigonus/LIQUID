#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; routines provided
;;
  (provide http-session)
  (provide page-hook)

;;--------------------------------------------------------------------------------
;; library includes
;;
  (require "lynch-lib.rkt")
  (require "realtime.rkt") ; sets real time boundary on function call
  (require racket/match); for treating return lists like multiple values (values is not a car class citizen)
  (require net/url)
  ; (require net/head) ; extract-field

;;--------------------------------------------------------------------------------
;; loc-read-line
;;
;; input: a port
;; output: either a string or eof
;;
;;
;; 5.2.1 standard library version has a bug where it can't be interrupted for thread rotaiton
;; to be fixed next version due to our report ..
;;
;; now found a cadr bug, it is returning the newline separator though doc says it doesn't
;; so implementing a predictable one
;;
;; http header standard specifies CRLF as end of line, that is the only http standard connection here
;; at this time
;;
;;
  (define (loc-read-line-1 in a-string)
    (let (
          [ch0 (read-char in)]
          [ch1 (peek-char in)]
        )
      (cond 
       [(eof-object? ch0) ch0] ; return the eof object
       [(and (char=? ch0 #\return) (char=? ch1 #\newline)) (read-char in) a-string] ;read-char clears the newline
       [else (loc-read-line-1 in (string-append a-string (string ch0)))]
       )))

  (define (loc-read-line [in (current-input-port)]) (loc-read-line-1 in ""))

  ;;; 
    (define (loc-read-line-test-0)
      (displayln "loc-read-line test 0 begin")
      (define ip (open-input-file "test-read-line.txt"))
      (define result (list
        (string=? (loc-read-line ip) "car line")
        (string=? (loc-read-line ip) "cadr line")
        (string=? (loc-read-line ip) "")
        (eof-object? (loc-read-line ip))
      ))
      (close-input-port ip)
      (displayln "loc-read-line test 0 end")
      result
      )

;;--------------------------------------------------------------------------------
;; read until blank line
;;
;; input: an input port
;; output: status, the header string
;;
;; status is true if the blank line was found
;; string does not include the ending blank line
;;  
  (define (read-until-blank-line in) 
      (define next-line (loc-read-line in))  
      (cond [ (eof-object? next-line) (list next-line "")]  ; returns the eof object as status
            [ (string=? next-line "") (list #t "")]
            [ else 
              (match-define (list status cdr-header-string) (read-until-blank-line in))
              (list status (string-append next-line "\r\n" cdr-header-string))]
            ))
  ;;;
  ;;;
    (define (read-until-blank-line-test-0)
      (define ip (open-input-file "test-read-line.txt"))
      (match-define (list complete a-header) (read-until-blank-line ip))
      (and complete (string=? a-header "car line\r\ncadr line\r\n"))
      )

;;--------------------------------------------------------------------------------
;; parse request
;;
;;  input: a request line as a string
;;  output: `(well-formed message cmd url protocol)
;;
;; currently ignores protocol
;; the list-strings is found in lynch-lib.rkt
;;
  (define (parse-request request-line)
    (let ( 
           [ message ""]
           [ cmd-string ""]
           [ url-string ""]
           [ protocol-string ""]
           [ cmd `null]
           [ url `()]
           [ protocol `null] ; ignored
          )

      (define ip (open-input-string request-line))
      (define tokens (list-strings ip))
      (define well-formed (= (length tokens) 3))

      (if well-formed
          (begin
            (set!-values (cmd-string url-string protocol) (apply values tokens))
            ;(set! cmd (parse-command cmd-string))
            (cond
                  [(string=? cmd-string "GET") (set! cmd `GET)]
                  [else 
                     (set! well-formed #f)
                     (set! message 
                           (string-append 
                            "unrecognized request command"
                            ": \""
                            cmd-string
                            "\" in request line"
                            ": \""
                            request-line
                            "\""
                            ))]
                  )
            (set! url (string->url url-string) )
            ;.. for protocol
            )
          ;else
          (set! message 
                (string-append 
                 "expected three tokens, found " 
                 (number->string (length tokens))
                 ": \""
                 request-line 
                 "\""
                 )))
            
      (list well-formed message cmd url protocol)))
            
  ;;; 
  (define (parse-request-test-0)
    (define ip (open-input-file "test-header.txt"))
    (define request-line (read-line ip))
    (define parse-result (parse-request  request-line))
    ;(equal? parse-result '(#t "" GET "/tutorials/other/top-20-mysql-best-practices/" "HTTP/1.1"))
    (equal? parse-result (list #t "" `GET (string->url "/tutorials/other/top-20-mysql-best-practices/") "HTTP/1.1"))
    )

   ;;;
  (define (parse-request-test-1)
    (equal?
     (parse-request "X X X")
     '(#f
       "unrecognized request command: \"X\" in request line: \"X X X\""
       null
       "X"
       "X")))


;;-------------------------------------------------------------------------------
;; server failed error page
;;
;;  it should be possible to have css etc with error pages ..
;;
  (define (page-http-error the-tcp-context [message0 ""] [message1 ""] [code 400])
    (parameterize ([current-output-port (tcp-context-out the-tcp-context)]) 
      (display "HTTP/1.0 ")
      (display (number->string code))
      (display " Error\r\n")
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n")
      (display "<html><body>")
      (display message0)
      (display "<br>")
      (display message1)
      (display "<br>")
      (display "</body></html>")
      (void)
      ))

  (define (page-not-found the-tcp-context url)
     (page-http-error the-tcp-context "page not found: " (url->string url) 404))


;;-------------------------------------------------------------------------------
;; application pages
;;
;; page-hook used by applications to hook their pages into the server
;; page-get  used by http-session, given a tcp-context and a url, gets a page
;;
;;
  (define the-pages (make-hash))
  (define (page-hook page-path page-response)
    (display "page: ") 
    (display (symbol->string (object-name page-response)))
    (display " \"")(display page-path)(display "\"")
    (newline)
    (hash-set! the-pages page-path page-response)
    )
  (define (page-lookup page-name)
    (hash-ref the-pages page-name (λ() page-not-found)))

  ; remove trailing null strings from url path list (corespond to. trailing slashes in url string)
  ;
    (define (remove-leading-null-string a-list)
      (if (eq? a-list '()) 
          a-list
          (let ([e (car a-list)]
                [r (cdr a-list)])
            (cond [(string=? e "") (remove-leading-null-string r)]
                  [else a-list]
                  ))))
    (define (remove-trailing-null-string a-list) (reverse (remove-leading-null-string (reverse a-list))))

    (define (test-remove-leading-null-string-0) (equal? (remove-leading-null-string (list "" "" "" "a" "b" "c")) (list "a" "b" "c")))
    (define (test-remove-leading-null-string-1) (equal? (remove-leading-null-string (list "" "a")) (list "a")))
    (define (test-remove-leading-null-string-2) (equal? (remove-leading-null-string (list "a")) (list "a")))
    (define (test-remove-leading-null-string-3) (equal? (remove-leading-null-string (list "")) (list "")))

  ; turn url path to string .. used by the page lookup hash table
  ;
    (define (url-path->string a-url-path)
      (define path-list (map path/param-path a-url-path))
      (define clean-path-list (remove-trailing-null-string path-list))
      (define slashed-path-list (map (λ(e) (string-append "/" e)) clean-path-list))
      (apply string-append slashed-path-list))

    (define (url-path->string-test0)
      (define url (cadddr (parse-request "GET /tutorials/other/top-20-mysql-best-practices/ HTTP/1.1")))
      (string=? (url-path->string (url-path url)) "/tutorials/other/top-20-mysql-best-practices")
      )

  (define (page-get the-tcp-context url)
    (define page-name-string (url-path->string (url-path url)))
    (apply 
     (page-lookup page-name-string) ; returns a page function
     (list the-tcp-context url)     ; arguments for a page function
     ))


;;--------------------------------------------------------------------------------
;; page selector
;;
;;  in:  request string, attributes string, output port for response
;; out:  response placed on output port, function returns void
;;
;;  
  (define (http-request-handler request-line attributes the-tcp-context)
    (log request-line)
    (match-define (list well-formed message cmd url protocol) (parse-request request-line))
    (if well-formed
        (cond [(eq? cmd `GET) (page-get the-tcp-context url)]
              [else
               (page-http-error the-tcp-context "unrecognized request")])
        (page-http-error the-tcp-context "request not well formed" message)
        ))
        


;;--------------------------------------------------------------------------------
;; reads an http request on the input stream and places an http response on the output stream
;;
;;    input: the streams, a pass by reference flag for extending the timeout timer
;;    return: (void)
;;
;; format of a request header: 
;;   1. A request line, for example GET /images/logo.png HTTP/1.1, which requests a resource
;;      called /images/logo.png from the server. 
;;   2. Request header fields, such as Accept-Language: en 
;;   3. An empty line.  
;;   4. An optional message body. -- this is currently ignored
;;
;; for keep-alive how in the heck do we distinguish the optional message body from teh
;; next request line??
;;
;; skip-white will hang if the client doesn't send a header, this is normal occurance when
;; a keep alive client doesn't get back to us quickly enough.  Note the race between the
;; client getting back to us and the header read
;;
;; need parms for header-read-timeout and log file port
;; this loop assumes keep-alive, that should probably be updated
;;
  (define (http-session the-tcp-context extend#)
    (define in (tcp-context-in the-tcp-context))
    (skip-white in) ; client has connected, hang here until client sends data, and the data is not white
    (set-box! extend# #t) ; extends our time so this transaction won't be interrupted

    ;; function we will run on a timer
    (define (read-request e#)
      (list 
       (loc-read-line in) ; returns request line
       (read-until-blank-line in)  ; returns (list status attributes)
       ;; what of this optional message?
       ))

    ;; runs function on the timer
    ;; returns:  complete, request-line, status, attributes
    ;;
    ;;   complete - whether read-request finished before timing out
    ;;   request-line - the car line from an http request GET path protocol - or some such
    ;;   status - true if blank line at bottom was read before the connection closed or timedout
    ;;           
    (match-define 
     (list complete (list request-line (list status attributes))) ; timed function returns two values
     (fun-timed (current-read-request-timeout) read-request))

    (cond 
      [(and complete (eq? status #t) (not (string=? request-line "")))
       (http-request-handler request-line attributes the-tcp-context) ; session writes response to tcp-context-out
       ]
      [else
        `request-read-failed
        ]
      )
    )


  ;;; prints out the current response to the test-header.txt request
    (define (http-session-try-0)
      (define ip (open-input-file "test-header.txt"))
      (define op (open-output-string))
      (define extend# (box #f))
      (http-session ip op extend#)
      (displayln (unbox extend#))
      (displayln (get-output-string op))
      )

  ;;; displays the parsed header
    (define (http-session-try-1)
      (define ip (open-input-file "test-header.txt"))
      (define request (loc-read-line ip))
      (displayln request)
      (match-define (list status attributes-string) (read-until-blank-line ip))
      (displayln status)
      (displayln attributes-string)
      )

#|
eof
|#
