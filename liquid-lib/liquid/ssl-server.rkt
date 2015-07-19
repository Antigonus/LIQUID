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
  (require racket/match); for treating return lists like multiple values (values is not a car class citizen)
  (require "misc-lib.rkt")
  (require "realtime.rkt") ; sets real time boundary on function call
  (require openssl)


;;--------------------------------------------------------------------------------
;; provides
;;
  (provide ssl-server)

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
;;        input: a-session-transactor, a port number (typically 80 for http servers),  a timeout for our session to complete
;;       return: a no argument function for killing the server
;;  
;; typical invocation: (define stop (server http-session-transactor 8080 10))
;;
;; note that the time out is for *our* server side session to run, if keep alive is true on the connection
;; we could be killing a valid session
;;
;; see ssl-listen doc for important parameters that are punted here, for example max-allow-wait
;;
;; This routine runs the session launcher in its own thread, and then returns immediately.  The
;; session launcher listens to the port for new sessions to launch forever.
;;
;;
  (define (ssl-server a-session-transactor port-number ssl-session-timeout)
    (define main-cust (make-custodian))
    (parameterize ([current-custodian main-cust]) 
      (define a-ssl-listener (ssl-listen port-number))
      (ssl-load-certificate-chain! a-ssl-listener "/usr/share/racket/collects/openssl/test.pem")
      (ssl-load-private-key! a-ssl-listener "/usr/share/racket/collects/openssl/test.pem")

      (thread (λ() (ssl-session-launcher a-session-transactor a-ssl-listener ssl-session-timeout)) )
    )
    (λ() (custodian-shutdown-all main-cust))) ;; this is the return value


#|
eof
|#
