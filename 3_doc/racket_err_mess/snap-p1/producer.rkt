

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "extensions.rkt")
  (require "object.rkt")

;;--------------------------------------------------------------------------------
;; this is all that is required to declare an abstract type
;;
  (define-type producer-type)

  ;; producers return status with parcels.  Status is a list of tags.  These tags
  ;; are standardized as the consumer must make sense of them.  Status must contain
  ;; exactly one of 'available or 'stopped.  Other tag(s) then provide a reason.
  ;;
    (define producer-type:available 'producer-type:available)
    (define producer-type:stopped 'producer-type:stopped)
    (define producer-type:eof 'producer-type:eof)
    (define producer-type:closed 'producer-type:closed)
    (define producer-type:error 'producer-type:error)

;;--------------------------------------------------------------------------------
;; simple byte producer
;;
;;   here is a working example of the producer-type interface
;;
;;   this example does not read ahead nor implement timeouts. 
;;
;;   it stops producing after either reaching an 'eof object or the port being closed
;;
;;
  (define-type simple-byte-producer-type)
  (obj:declare-type simple-byte-producer-type type-type)

  (define (make-simple-byte-producer duration parcel-length port-in)
    (define-object a-simple-byte-producer producer-type)
    (obj:set! a-simple-byte-producer producer-type
      (Λ
        'status (mutable-seteqv producer-type:available)
        'duration duration ;  max time from a request to  delivery of a parcel, ignored here
        'parcel-length parcel-length
        'port-in port-in
        ))
    a-simple-byte-producer
    )

  (obj:add-method simple-byte-producer-type 'status
    (λ (a-simple-byte-producer) 
      (obj:ref (Λ a-simple-byte-producer producer-type 'status)
        (Λ
          identity
          (λ args (raise 'no-status-key))
          (λ args (raise 'not-a-simple-byte-producer))
          ))))

  (obj:add-method simple-byte-producer-type 'duration
    (λ (a-simple-byte-producer) 
      (obj:ref (Λ a-simple-byte-producer producer-type 'duration)
        (Λ
          identity
          (λ args (raise 'no-duration-key))
          (λ args (raise 'not-a-simple-byte-producer))
          ))))

  (obj:add-method simple-byte-producer-type 'parcel-length
    (λ (a-simple-byte-producer) 
      (obj:ref (Λ a-simple-byte-producer producer-type 'parcel-length)
        (Λ
          identity
          (λ args (raise 'no-parcel-length-key))
          (λ args (raise 'not-a-simple-byte-producer))
          ))))
    
  ;; (cont-normal parcel) (cont-with-status status parcel)  (cont-only-status status)
  ;; we assume that port-closed  is syncrhonized to occur only after reading the last char from the buffer
  (obj:add-method simple-byte-producer-type 'next
    (mc:λ (args a-simple-byte-producer) (conts cont-normal cont-with-status cont-only-status)
      (let(
            [e (obj:get-ele a-simple-byte-producer producer-type)]
            )
        (let(
              [status (obj:ele:ref% e 'status)]
              [parcel-length (obj:ele:ref% e 'parcel-length)]
              [port-in (obj:ele:ref% e 'port-in)]
              )
          (cond
            [(set-member? status producer-type:stopped)
              (cont-only-status (set-copy status))
              ]
            [else
              (let(
                    [parcel (make-vector parcel-length)] ; the goal is to fill this
                    [offset -1] ; the handler needing the offset forced this shared at higher scope
                    )

                (define (stop flag)
                  (set-remove! status producer-type:available)
                  (set-add! status producer-type:stopped)
                  (set-add! status flag)
                  (if (< 0 offset)
                    (cont-only-status (set-copy status))
                    (cont-with-status (set-copy status) offset parcel)
                    ))

                ;; we don't change the stream position until we know there were no exceptions
                (define (fill-parcel-loop)
                  (cond
                    [(= offset parcel-length) (cont-normal parcel)]
                    [(port-closed? in) (stop producer-type:closed)]
                    [else
                      (let(
                            [ch (peek-byte in)]
                            )
                        (cond
                          [(eof-object? ch)
                            (read-byte in) ; toss out the eof
                            (stop producer-type:eof)]
                          [else
                            (vector-set! parcel (++ offset) ch)
                            (read-byte in) ; toss that byte we peeked while hoping for no exceptions
                            (++! offset)
                            (fill-parcel-loop)
                            ]))
                      ]))

                (with-handlers(
                                [(λ(v) #t) ; catches all exceptions
                                  (λ(v) (stop producer-type:error))
                                  ]
                                )
                  (fill-parcel-loop)
                  ))]
            )))))

  ;; our simple-byte-producer doesn't ever time out when putting items in a parcel.  Rather it blocks
  ;; until it fills the parcel or finds a completing condition.  Hence we would not expect a consumer
  ;; to ever actually call the simple-byte-producer's 'finish method ...
  ;;
  ;; in producers that can time out and return partial parcels, this routine updates the consumer
  ;; with a new offset value for the prior parcel that it had sent.  Hopefully this new offset shows
  ;; the parcel is now filled, but that is not a gurantee. 
  ;; 

  ;; (cont-normal) the block is now full
  ;; (cont-with-status status offset)
  ;; (cont-only-status status)
  (obj:add-method simple-byte-producer-type 'finish-up
    (mc:λ (args a-simple-byte-producer) (conts cont-normal cont-with-status cont-only-status)
      (let(
            [e (obj:get-ele a-simple-byte-producer producer-type)]
            )
        (let(
              [status (obj:ele:ref% e 'status)]
              )
          (cond
            [(set-member? status producer-type:stopped)
              (cont-only-status (set-copy status))
              ]
            [else
              (raise 'request-to-fill-partial-parcel) ; shouldn't happen with this implementation
              ])))))



;;--------------------------------------------------------------------------------
;;  tests
;;
  (define (simple-byte-producer-test-0)
   (let(
         [ip (open-input-string "our simple-byte-producer doesn't ever time out when putting items in a parcel.  Rather it blocks")]
         )
     (let(
           [bp (make-simple-byte-producer 0 8 ip)]
           )
       (let(
             [a-parcel (obj:apply% simple-byte-producer-type 'next (Λ bp))]
             )
         (display a-parcel)
         ))))
                   
       

     
                           
