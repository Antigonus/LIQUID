#|
  Producer
   
  created: 2015-11-08T05:59:49Z twl

  This defines the type for an abstract producer and gives some concrete examples. 

  Because it is an abstraction 'producer' is just a contract with programmers to provide a
  particular set of attributs for derived producer types namely: status, duration,
  parcel-length, next, and finish-up.

  A consumer may use the 'next' or 'finish-up' calls to request parcels of items from the producer.

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

  An abstract type defines only an interface.  Say for our summable type is abstract and
  is required only to have the method '+ that takes two arguments and returns a sum.  Now we
  may have many concrete summable types.  Say we have summable-float  which does a floating-point
  sum,  and sumable-vector, which does a vector sum.  The code would appear as:

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

  If we want polymorphic behavior, which we do not need here, we would add an attribute to
  the object to hold the type.  Then, say, the summable base would read and use the value
  of that atribute and use it.  So in the example, we would add an attribute to summable
  objects called 'type'.  We would set the value of that to either 'summable-float'  or
  'summable-vector'.  Each method in the summable base would then read this value
  and then defer to the method in that other type.



|#

;;--------------------------------------------------------------------------------
;; this is all that is required to declare an abstract type
;;
  (define-type producer-type)


;;--------------------------------------------------------------------------------
;; byte producer
;;   this example is given to complete the definition of the abstract interface
;;   this producer needs improvement, for one thing errors on the stream need to
;;   be noted further up stream, perhaps via an error continuation
;;
  (define-type byte-producer-type)
  (obj:declare-type byte-producer-type type-type)

  (define (make-byte-producer duration parcel-length port-in)
    (define-object a-byte-producer producer-type)

    (obj:set! a-byte-producer producer-type
      (Λ
        'status 'empty  ;  could be: active, empty, end of stream,
        'duration duration ;  for intializing the time out timer
        'parcel-length parcel-length
        'port-in port-in
        ))

    a-byte-producer
    )
      
  (obj:add-method byte-producer-type 'status
    (λ (a-byte-producer) 
      (obj:ref (Λ a-byte-producer producer-type 'status)
        (Λ
          identity
          (λ args (raise 'no-status-key))
          (λ args (raise 'not-a-byte-producer))
          )))

  (obj:add-method byte-producer-type 'duration
    (λ (a-byte-producer) 
      (obj:ref (Λ a-byte-producer producer-type 'duration)
        (Λ
          identity
          (λ args (raise 'no-duration-key))
          (λ args (raise 'not-a-byte-producer))
          )))

  (obj:add-method byte-producer-type 'parcel-length
    (λ (a-byte-producer) 
      (obj:ref (Λ a-byte-producer producer-type 'parcel-length)
        (Λ
          identity
          (λ args (raise 'no-parcel-length-key))
          (λ args (raise 'not-a-byte-producer))
          )))

  (obj:add-method byte-producer-type 'next
    (mc:λ (args a-byte-producer) (conts cont-with-bytes cont-no-bytes)
      
      (let(
            [e (obj:get-ele a-byte-producer producer-type)]
            )
        (let(
              [status (obj:ele:ref (Λ e 'status) (Λ identity raise:no-such-key-in-elementary))]
              [duration (obj:ele:ref (Λ e 'duration) (Λ identity raise:no-such-key-in-elementary))]
              [parcel-length (obj:ele:ref (Λ e 'parcel-length) (Λ identity raise:no-such-key-in-elementary))]
              [port-in (obj:ele:ref (Λ e 'port-in) (Λ identity raise:no-such-key-in-elementary))]
              )
          (cond
            [(eqv? 'eos status) (cont-eos)]
            [else
              (let(
                    [parcel (make-vector parcel-length)]
                    [eos #f]
                    )
                (let(
                      [last-dex
                        (for/last(
                                   [offset (range 0 parcel-length)]
                                   )
                          (define ch (read-byte in))
                          (set! eos (eof-object? ch))
                          #:break eos
                          (vector-set! parcel offset ch)
                          offset
                          )]
                      )
                  (cond
                    [(not last-dex) (cont-eos-no-bytes)]
                    [(eos (cont-last-bytes (Λ 
                  
                  ))])
                  
                        

