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
  (provide http-server)


;;--------------------------------------------------------------------------------
;; library includes
;;
   (require "lynch-lib.rkt")
   (require "ssl-server.rkt")
   (require "http-session.rkt")
   (require "http-server-pages.rkt") ; for pages-init

;;--------------------------------------------------------------------------------
;; main
;;
   (define (http-server [portno 8080] [session-timeout 300] ) 
     (log "http-server start")
     (server-pages-init)
     (define stop (ssl-server http-session portno session-timeout))
     (λ()
       (log "http-server stop")
       (stop)
       ))
