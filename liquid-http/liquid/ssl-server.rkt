#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; library includes
;;
  (require liquid/extensions)
  (require racket/match); for treating return lists like multiple values (values is not a car class citizen)
  (require openssl)

  (require "realtime.rkt") ; sets real time boundary on function call
  (require "http-parameters.rkt")

;;--------------------------------------------------------------------------------
;;
  (define (has-ssl) 
    (if ssl-available?
      #t
      (begin (displayln ssl-load-fail-reason) #f)
      ))
  (test-hook has-ssl)

;;--------------------------------------------------------------------------------
;;  ssl session launcher
;;
;;    input: a session transactor, a ssl-port-listener, a timeout
;;    return: no return value
;;
;;  We are a ssl server, we already have a ssl port allocation (ssl listener).  In this
;;  function we wait for a client to connect to our allocated port - the listener takes
;;  care of negociating with the client for a higher port number and unblocks when the
;;  new ports are ready.  We then launch a session transactor on these ports for talking
;;  to the client.  We launch the session in its own thread so that we can immediately
;;  return to listening for another client on the allocated port.
;;
;; tail recursion forever loop will not eventually blow the stack, racket guide says: "This
;; evaluation behavior is sometimes called tail-call optimization, but it’s not merely an
;; “optimization” in Racket; it’s a guarantee about the way the code will run."
;;
;; should give thread its own custodian ? any other resources besides the ports?
;;
;; should log session and its return status
;;
  (define (ssl-session-launcher session-transactor a-ssl-listener session-timeout)
      (let*-values (
            [(error-log-port) (current-output-port)] ;; should be a parameter
            [(in out) (ssl-accept a-ssl-listener)] ; blocks until a client connects on our port
            [(server-IP server-port client-IP client-port) (ssl-addresses out #t)]
            )
        (define the-session-context (session-context in out server-IP server-port client-IP client-port))
        (define session (λ(extend#) (session-transactor the-session-context extend#)))
        (thread (λ()
          (log (list "start ssl session" the-session-context))
          (match-define (list complete exit-status) (fun-timed session-timeout session))
          (log (list "end session" the-session-context complete exit-status))
          (close-input-port in)
          (close-output-port out)
          )))
      (ssl-session-launcher session-transactor a-ssl-listener session-timeout)) ; loop forever

;;--------------------------------------------------------------------------------
;; this starts a ssl server and runs launches the ssl-session-launcher
;;        
;;        input: a-session, a port number (typically 80 for http servers),  a timeout for our session to complete
;;       return: a no argument function for killing the server
;;  
;; typical invocation: (define stop (server http-session))
;;
;; see ssl-listen doc for important parameters that are punted here, for example max-allow-wait
;;
;; really, all this routine does is to run the session transactor when a client shows up on one of our ports.
;;  
;;
  (define (ssl-server a-session [port-number 443])
    (define ssl-server-custodian (make-custodian))
    (parameterize (
                    [current-custodian ssl-server-custodian]
                    [current-log-port (open-output-file "ssl-server-log.txt" #:exists `can-update)]
                    )
      (log "ssl-server start")
      (define an-ssl-listener (ssl-listen port-number))
      (ssl-load-certificate-chain! an-ssl-listener "/usr/share/racket/collects/openssl/test.pem")
      (ssl-load-private-key! an-ssl-listener "/usr/share/racket/collects/openssl/test.pem")
      (thread 
        (λ() (ssl-session-launcher a-session an-ssl-listener ssl-session-timeout))
        )
      (λ()
        (custodian-shutdown-all ssl-server-custodian)
        (log "ssl-server stop")
        )
      ))

;;--------------------------------------------------------------------------------
;; provides
;;
  (provide-with-trace "ssl-server" ssl-server)

