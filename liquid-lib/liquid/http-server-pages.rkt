#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; provides to the world
;;
  (provide server-pages-init)
  (provide http-response)
  (provide html-str)
  (provide html-header)
  (provide html-header*)

;;--------------------------------------------------------------------------------
;; library includes
;;
  (require "misc-lib.rkt")
  (require "http-session.rkt")
  (require net/url)
   

;;--------------------------------------------------------------------------------
;; initialize pages
;;
  (define (server-pages-init)
    ;; put calls to init calls here:
    ;;
       ;; not pages need init calls at present

    ;; no hook the initialized pages into the server
    (page-hook "/server/hello" page-hello)
    (page-hook "/server/session" page-session)
    )

;;--------------------------------------------------------------------------------
;; page pieces
;;  -these are used by the server, very basic stuff
;;  -these are all strings, we move to parsed html and xexps on the applicaiton pages
;;

    (define (http-response)
      (string-append
        "HTTP/1.1 200 Okay\r\n"
        "Server: DeepWeb/0.1\r\n"
        "Content-Type: text/html; charset=utf-8\r\n"
        "\r\n"
        )
      )

    (define (html-str header body)
      (string-append
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\r\n"
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">\r\n"
        header
        body
        "</html>\r\n"
        ))

    (define (html-header . header-funs) (html-header* header-funs))
    (define (html-header* header-funs)
      (string-append
        "<head>\r\n"
        (apply string-append (map (λ(e) (e)) header-funs))
        "</head>\r\n"
      ))

;;--------------------------------------------------------------------------------
;; pages
;;
  (define (page-hello the-session-context url)
    (parameterize ([current-output-port (session-context-out the-session-context)])
      (display (http-response))
      (display (html-str (html-header) "<body>hello<br></body>\r\n"))
      ))

  (define (page-session the-session-context url)
    (parameterize [(current-output-port (session-context-out the-session-context))]
      (let*(
             [an-http-response (http-response)]
             [a-session-context    (session-context->string the-session-context)]
             [doc-body         (string-append "<body>" a-session-context "<br>" "</body>\r\n") ]
             )
        (display an-http-response)
        (display (html-str (html-header) doc-body))
      )))


    
