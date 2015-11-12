#|
  Sliding-Window
   
  created: 2015-11-04T02:43:31Z twl 

  For sake of setting nomenclature we visuallize the sliding-window as being linear and
  horizonatal, thus having left and right sides. 

  This sliding-window is an array of items, where items may be added to the right or
  truncated off of the left.  When the window contains more than one item, there is a distinct
  lefrmost and a distinct rightmost item.  When it has one item, that one item is both the
  leftmost and righmost item.  When it is empty there is no leftmost or rightmost.

  Internally we maintain two iterators, one that points to the leftmost item in the
  sliding-window and one that points to the rightmost item in the sliding-window.  The user
  may request a copy of the leftmost iterator.  The rightmost iterator is used internally
  and thus is more encapsulated.  The user may request a comparison against the rightmost
  iterator.

  In order to fill the window with more data after it becomes empty, while preserving the
  address space, we must know where the the rightmost iterator was before the window became
  empty. For construction there is no prior value, so this value is passed in.  The window
  is empty after construction.

  I use the term 'parcel' here instead of 'block' because block has another meaning related
  to threads.

  The sliding-window consistively addressed of parcels of items.  Each parcel has a
  'parcel address'.  Each item has an offset within a parcel. Sliding-Window extensions
  arrive in parcels of items, this is not an implementation detail, rather it is important
  facet of the architecture.  Our calculations are locally fixed in size, and globally
  unbounded.

  The sliding-window may be exteneded by 'fetching' new parcels.  The fetch function is
  passed into the sliding-window upon construction.  Also, parcels may be pushed into
  sliding-window by calling (force-fetch).  We do not ask how the fetch routine comes up
  with the data.

  In some applications the fetch routine should not block for too long of a duration of
  time.  Should fetch time out before filling a parcel but after consecutively placing from
  the left some items, this partial parcel should be returned.  A similar thing will happen
  should the item producer run out of items.  Finally it is possible that a producer simply
  decides to end the production of items.

  Hence, the rightmost parcel of the sliding-window may be partial. When the 
  rightmost parcel is partial, we complete it before going on to the next parcel. This is 
  done with a short fetch for the remaining items missing from the parcel. We always
  work to maintain parcel alignments. This assures that items have a contiguous address space
  within the window in order to support address arithmetic.

  Incrementing an iterator beyond rightmost, or decrementing beyond lefmost, causes an off
  left or off right continuation to be followed rather than the usual continuation. Typically
  walking off the left will be an error, and walking off the right will force a fetch or
  terminate the calculation should all the data be processed.

  The sliding-window may be diminised by dropping parcels from the left.  This is done by
  updating the leftmost iterator.  When the leftmost iterator is updated, all parcels 
  to the left of it are dropped from the window.

  about partial parcels:

  The memory allocation for a parcel is done by the producer.  This is because the
  producer may be running on an independent thread and working in advance to fill the parcel
  even before fetch is called.  For exmaple, when the parcel is a TCP buffer.

  When the producer times out, and returns a partial parcel, the producer continues to attempt
  to complete the parcel in the same allocation that was used when the parcel was given to
  the sliding-window.  Our sliding-window doesn't see this work because it happens to the
  right of the rightmost iterator.

  When 'fetch' is called, and it is discovered that the rightmost address is not at the
  end of the parcel, the routine first asks the producer to update the rightmost address for
  the parcel.  This is done by 'fetch-remainder' method on the producer.  The
  fetch-remainder method only accepts the mutable right address as an argument, and then
  modifies its offset.

  prefetch and threads:

  It is intended that the producer will work ahead and try to complete a parcel before it
  is requested.  Consequently, when the sliding-window requests the parcel there is little
  overhead in transfering it.  Typically the 'fetch' routine will be run in the 'stepped 
  beyone rightmost'  continuation for an interator increment.

  However, the producer should work ahead. Hopefully it has allocated and filled the
  next parcel before it is requested.  An implementation may facilitate this by either
  calling the producer before calling the sliding-window user, or by using threads inside
  the producer.

  The internal rightmost iterater (actually it is just an address) is locked while 
  'fetch' runs.  This prevents multiple 'beyond rightmost'  continuations running and then
  requesting multiple simultaneous fetches.

  The sliding-window may have multiple iterators, even in different threads, working
  simultenously. If the user desires accesses on these to be syncrhonized, then he or 
  she should externally implement a hash of semphore locks.  

  Calling 


|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require liquid/extensions)
  (require liquid/object)


;;--------------------------------------------------------------------------------
;; 
  (define sliding-window-debug (make-parameter #t))

;;--------------------------------------------------------------------------------
;; types
;;
  ;; by keeping a separate parcel and item address we avoid having to do a divide to recover an index
  (struct window-address-type (parcel-address offset) #:mutable #:transparent)  ; address-0 = parcel address,  address-1 offset

  ;; status 'unitialized 'empty 'not-empty
  ;; when status is empty,  max holds the prior max address
  (struct range-type (status parcel-length address-leftmost address-rightmost) #:mutable #:transparent) 

  (define-type sliding-window:type)
  (define-type sliding-window-iterator:type)

;;--------------------------------------------------------------------------------
;; functions
;;
  (define (sliding-window-iterator:make a-sliding-window address)
    (define-object an-iterator sliding-window-iterator:type)
    (obj:set! an-iterator sliding-window-iterator:type 
      (Λ
        'sliding-window a-sliding-window
        'address address
        ))
    an-iterator
    )
      
  (mc:define sliding-window:end-cap (args alloc-parcel-length) (conts cont-ok cont-eos cont-timedout)
    (cont-eos)
    )

  (define (sliding-window:make 
            [next-parcel sliding-window:end-cap]
            [complete-parcel sliding-window:end-cap] ; hopefully this doesn't get called often
            [parcel-length (current-parcel-length)]
            [initial-address (window-address-type 0 0)]
            )
    (define-object the-new-sliding-window sliding-window:type)
    (let*(
           [leftmost initial-address]
           [rightmost initial-address]
           [range (range-type 'empty parcel-length leftmost rightmost)]
           [window (make-hash)]
           )
      (obj:set! the-new-sliding-window sliding-window:type
        (Λ
          'range range
          'window window
          'fetch fetch
          ))
      the-new-sliding-window
      ))


   ;; fetch
   ;;   used internally to cause the fetch to happen
   ;;   used externally to invoke prefetch
   ;;   need to modify this so that fetch runs in its own thread and doesn't hold up calculation
   ;;     will need a semphore on righmost iterator .. be careful not to run fetch twice and get
   ;;     out of order results
   ;;
     (mc:define sliding-window:fetch (args a-sliding-window) (conts cont-ok cont-eos cont-timed-out)
       (let(
             [e (obj:get-ele 
                  (Λ a-sliding-window sliding-window:type)
                  (Λ idenitity (λ args (raise 'not-a-sliding-window)))
                  )]
             )
         (let(
               [range (obj:ele:ref (Λ e 'range))]
               [window (obj:ele:ref (Λ e 'window))]
               [fetch (obj:ele:ref (Λ e 'fetch))]
               )
           (let(
                 [partial (fetch-type-partial fetch)]
                 [full    (fetch-type-full fetch)]
                 [status (range-type-status range)]
                 [alloc-parcel-length (range-type-parcel-length range)]
                 [rightmost (range-type-rightmost range)]
                 )
             (cond
               [(eqv? 'empty status)
                 (full (Λ 
                   
             (let*(
                    [parcel-address (window-address-type-parcel-address rightmost)]
                    [offset (window-address-type-offset rightmost)]
                    [next-dex (++ offset)]
                    )                       
               (cond
                 [(< next-dex alloc-parcel-length)  ; then we have a partial parcel, fill it first
                   

           (fetch (Λ parcel-length
           )))

      
   ;; an example fetch routine for testing
   ;; this needs a timer and to return partial parcels when it is taking too long to fill one
   ;; fetch returns a vector
   ;;
   ;; the alloc-parcel-length is usually the length of a parcel, but can also be the length
   ;; of the unfinished part of the prior fetch
   ;;
     (define example-data (range 0 100))

     (mc:define example-fetch-remainder
       (args rightmost)
       (conts cont-now-completed cont-still-partial cont-eos)
       ;;...
       (cont-now-completed) ;; updated the rightmost offset
       (cont-still-partial) ;; updated the rightmost offset
       (cont-eos) ;; updated rightmost offset, and the stream has come to an end
       )


     (mc:define example-fetch-full (args parcel-length) (conts cont-ok cont-eos cont-timedout)
                           
       (cond
         [(parcel-length≥ example-data parcel-length)
           (let(
                 [parcel (list->vector (take example-data parcel-length))]
                 )
             (set! example-data (drop example-data parcel-length))
             (cont-ok parcel-length parcel)
             )]
         [(pair? example-data)
           (let*(
                  [parcel (make-vector parcel-length)]
                  [actual-parcel-parcel-length (parcel-length example-data)]
                  [short-parcel (list->vector example-data)]
                  )
             (set! example-data '())
             (vector-copy! parcel 0 short-parcel)
             (cont-ok actual-parcel-parcel-length parcel)
             )]
         [else
           (cont-eos)
           ]
         ))


  ;; abstraction for getting rightmost or leftmost
  ;;
    (mc:define get-iterator (args a-sliding-window range-field) (conts cont-ok cont-empty)
      (let*(
             [range  (obj:ref 
                       (Λ a-sliding-window sliding-window:type 'range) 
                       (Λ identity (λ args (raise 'not-a-sliding-window)) (λ args (raise 'sliding-window:no-range)))
                       )]
             [status (range-type-status range)]
             [address  (range-field range)]
             )
        (cond
          [(eqv? 'empty status) (cont-empty)]
          [else
            (let(
                  [iterator (sliding-window-iterator:make a-sliding-window address)]
                  )
              (cont-ok iterator)
              )]
          )))

  ;; returns an iterator to the leftmost item
  ;;
    (obj:add-method sliding-window:type 'leftmost
      (mc:λ (args a-sliding-window) (conts cont-ok cont-empty)
        (get-iterator (Λ a-sliding-window range-type-leftmost) (cont-ok cont-empty))
        ))

  ;; returns an iterator to the rightmost item
  ;;
    (obj:add-method sliding-window:type 'rightmost
      (mc:λ (args a-sliding-window) (conts cont-ok cont-empty)
        (get-iterator (Λ a-sliding-window range-type-rightmost) (cont-ok cont-empty))
        ))


;;--------------------------------------------------------------------------------
;; public interfaces
;;
  (obj:add-method sliding-window:type 'make sliding-window:make)                

  (obj:add-method sliding-window-iterator:type 'make sliding-window-iterator:make)



;; forcce fetch -- instead of having push
;; drop -- drop parcel on left
;; update iterator -- moves leftmost iterator right
;;

;; in iterator
;; get
;; ++  --
;; next-parcel
;; back-parcel?
;;  continuations for walking off of ends
