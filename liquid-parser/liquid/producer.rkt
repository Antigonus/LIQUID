#|
  Producer
   
  created: 2015-11-08T05:59:49Z twl

  This defines the type for an abstract producer and gives a simple example.

  Because the producer-type is abstract it has no methods of its own; however objects are
  still declared to have producer-type.  Such objects are then operated on by methods
  found in concerete producer-types, such as the byte-producer-type described in this
  module.

  Producer types must have the following methods: status, duration,
  parcel-length, next, and finish-up.

  A consumer may use the 'next' or 'finish-up' calls to request parcels of items from a
  producer object.

  A parcel is an allocation for 'parcel-length' number of items.  Currently, the
  parcel length is fixed upon construction of the producer and does not change.  An empty
  parcel contains no items.  A 'full' parcel contains 'parcel-length' number of
  items.  A 'partial' parcel contains at least one item, but fewer items than a full
  parcel.

  The producer normally delivers full parcels, but in exceptional circumstances it may
  produce a partial parcel. Two such exceptional circumstances are: a time out, and upon
  exhausting the supply for more items.  The producer never produces an empty parcel;
  instead error continuation paths are used.  If a producer is delivering very many partial
  parcels it is an indication of a design problem or a performance bottleneck.

  It is intended that the producer will work ahead to complete a parcel before it is
  requested by the consumer.  This assures that the overhead of a parcel request is merely
  that of returning a reference to the block. Consequently, most producers will employ
  threads.  A producer could even be runing in another process, or be located remotely.

  When the producer delivers a partial block, it continues to work to fill it.  Upon
  request from the consumer, the producer will then update the block length.

  ----

  Normally in our code the type of an object is literally given in the method call.
  Take the summable example in object.rkt as an example:  

      (declare-type obj1 summable)
      (delcare-type obj2 summable)

       ...
      (obj:apply (Λ summable '+ (Λ obj1 obj2)) ; sums the two objects

  An abstract type defines only an interface.  For sake of discussion suppose our summable
  type is abstract and is required only to have the method '+ that takes two arguments and
  returns a sum.  We would then need some concrete summable types.  Say we have summable-float
  which does a floating-point sum, and sumable-vector, which does a vector sum.  The code
  would appear as:

      (declare-type obj1 summable)
      (delcare-type obj2 summable)
      (define the-summable-type summable-float)
     
       ...
      (obj:apply (Λ the-summable-type '+ (Λ obj1 obj2)) ; sums the two objects

  and,
      (declare-type obj1 summable)
      (delcare-type obj2 summable)
      (define the-summable-type summable-vector)

     ...
      (obj:apply (Λ the-summable-type '+ (Λ obj1 obj2)) ; sums the two objects

  Note in both of these cases the last line, where obj:apply is used, is the exact same.
  Thus we have successfully abstracted the summable type simply by using a variable to 
  hold the type rather than coding the type as a constant.

  But what if we wanted infomation in the operands to determine which type should 
  be used?  This would have the advantage of the programmer not having to do the 
  book keeping of matching type and operands.  Though it would also be an additional
  calculation that in many cases would be superfluous. 

  Suppose that the abstract type, in our example the summable-type, looked at the operands
  then decided which of the concrete types should be used.  For this purpose the
  key obj:ele:type has been reserved.  Sumable elementary objects would then have this
  field set.


|#



#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require liquid/extensions)
  (require liquid/object)

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
                   
       

     
                           
