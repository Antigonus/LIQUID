#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  A central place to put parameter definitions.
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; global parameters
;;    
  (define current-read-request-timeout (make-parameter 5)); 5 cadrs to finish request header read
  (define current-get-webpage-timeout (make-parameter 60)) ; used in "web.rkt"

;;--------------------------------------------------------------------------------
;; global types
;;    
  (struct session-context (in out server-IP server-port client-IP client-port) #:transparent)
  (define (session-context->string a-session-context) 
    (string-append
     "server "
      (session-context-server-IP a-session-context)
      ":"
      (number->string (session-context-server-port a-session-context))
      ";"
      "client "
      (session-context-client-IP a-session-context)
      ":"
      (number->string (session-context-client-port a-session-context))
      ))

;;--------------------------------------------------------------------------------
;; provides the following
;;    -- this stuff belongs in an application specific file ..    
;;
  ;; global parameters  
  ;;
    (provide current-read-request-timeout) ; how long we wait for the http client to provide the complete request
    (provide current-get-webpage-timeout)

  ;; global type definitions
  ;;
    (provide session-context)  ; helps organize ssl connections
    (provide session-context-in)
    (provide session-context-out)
    (provide session-context-server-IP)
    (provide session-context-server-port)
    (provide session-context-client-IP)
    (provide session-context-client-port)
    (provide session-context->string)


