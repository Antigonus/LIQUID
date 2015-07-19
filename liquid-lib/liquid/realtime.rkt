#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/match)
  (require racket/runtime-path)
  (require "misc-lib.rkt") ; currently just for the test-hook
  

;;--------------------------------------------------------------------------------
;; a timed function call
;;
;;  simplified by taking out possibility for partial results
;;
;;  input:
;;    - a timeout value
;;    - a function that takes one parameter: a reference to a flag to extend the timeout 
;; output: list, car a bool to signify completion, cadr the function result
;; exceptions: need to look into this
;;
;;
;;
  (define (fun-timed timeout fun)
    (let ([the-custodian (make-custodian)]
          [extend# (box #f)]
          [completed #f]
          [result (void)]
          )
      (parameterize ([current-custodian the-custodian]) 
        (define worker-thread (thread (λ() (set! result (fun extend#)) (set! completed #t))))
        (define (timer-loop)
          (sleep timeout) 
          (cond [(unbox extend#) (set-box! extend# #f) (timer-loop)]
                [else  (custodian-shutdown-all the-custodian)]))
        (thread (λ() (timer-loop)))
        (thread-wait worker-thread)
        )
      (list completed result)
      ))


;;--------------------------------------------------------------------------------
;; fun-timed trys and tests
;;

  ;  test cases for fun-timed:
  (define (fun-timed-test-g a n) (if (= n 0) 1 (* a (fun-timed-test-g a (-- n)))))
  (define (fun-timed-test-f n) (if (= n 0) 1  (* n (fun-timed-test-f (-- n)))))

  ; this hangs drracket (on this machine if not on yours add a zero to the 100 ;-)
  (define (fun-timed-test-hang)
    (fun-timed-test-f (-- (fun-timed-test-g 2 100))) 
    )

  (define (fun-timed-test-0)
    (match-define (list completed result) (fun-timed 10 (λ(e#) (fun-timed-test-f 5))))
    (and completed (= result 120)))
  (test-hook fun-timed-test-0)

  (define (fun-timed-test-1) ;; should timeout in 1 cadr
    (match-define (list completed result) (fun-timed 1 (λ(e#) (fun-timed-test-hang))))
    (not completed)
    )
  (test-hook fun-timed-test-1)


  (define-runtime-path rtd "realtime-test-data.rkt")

  (define (fun-timed-test-2)
    (define ip (open-input-file rtd))
;    (define ip (open-input-file "realtime-test-data.rkt"))
    (match-define (list completed result) (fun-timed 2 (λ(e#) (read-line ip))))
    (close-input-port ip)
    (and completed (string=? result "#|"))
  )
  (test-hook fun-timed-test-2)

  ;;; this test hangs as of racket version 5.2.1 - to be fixed as of racket 6.1.1
  ;;; another bug turned up in read-line with it returning the endofline marker
  ;;; consequently we are using our own read-line
  (define (fun-timed-test-3)
   (define ip (open-input-file "/dev/zero"))
   (match-define (list completed result) (fun-timed 2 (λ(e#) (read-line ip))))
   (close-input-port ip)
   (not completed)
  )
  ;;  (test-hook fun-timed-test-3)

;;--------------------------------------------------------------------------------
;; module interface
;;
  (provide-with-trace "realtime" fun-timed)

