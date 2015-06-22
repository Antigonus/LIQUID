#lang racket

; 2014-10-27T07:51:53Z: 
; racket@kill-bug.rkt> (version)
; "5.2.1"
;
; (kill-bug-1 "kill-bug.rkt" 2)
; (kill-bug-1 "/dev/zero" 2) ; hangs
;
; (kill-bug-2 "kill-bug.rkt" 2)
; (kill-bug-2 "/dev/zero" 2) ; hangs

; rolling own read-line from read-char works:
; racket@kill-bug.rkt> (kill-bug-3 "kill-bug.rkt" 2)
; end
; racket@kill-bug.rkt> (kill-bug-3 "/dev/zero" 2)
; end


  (define (kill-bug-1 file timeout)
    (let (
          [kill-bug-custodian (make-custodian)]
          [in (open-input-file file)]
          )

      (define worker-thread
        (parameterize ([current-custodian kill-bug-custodian]) 
          (thread (lambda () (read-line in)))))

      (define timer 
        (thread (lambda () (sleep timeout) (custodian-shutdown-all kill-bug-custodian) (displayln "thread killed"))))

      (thread-wait worker-thread)
      (kill-thread timer)
      (displayln "all done!")
      ))


  (define (kill-bug-2 file timeout)
    (let (
          [in (open-input-file file)]
          )
      (define worker-thread (thread (lambda () (read-line in))))
      (sleep timeout)
      (kill-thread worker-thread)
      (displayln "end")
      ))

   (define (loc-read-all [in (current-input-port)])
     (let (
           [ch (read-char in)]
           )
       (if (eof-object? ch) ch (loc-read-all in))))
       
     
  (define (kill-bug-3 file timeout)
    (let (
          [in (open-input-file file)]
          )
      (define worker-thread (thread (lambda () (loc-read-all in))))
      (sleep timeout)
      (kill-thread worker-thread)
      (displayln "end")
      ))
