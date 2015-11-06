#|
  Sliding Window
   
  created: 2015-11-04T02:43:31Z twl 

  For sake of setting nomenclature we visuallize the sliding window as being linear and
  horizonatal, thus having left and right sides. 

  This sliding-window is an array of items, where items may be added to the right or
  truncated from the left.  When the window contains more than one item, there is a distinct
  lefrmost and a distinct rightmost item.  When it has one item, that one item is both the
  leftmost and righmost item.  When it is empty there is no leftmost or rightmost.

  Internally we maintain two iterators, one that points to the leftmost item in the
  sliding-window and one that points to the rightmost item in the sliding-window.  The user
  may request copies of these.

  In order to fill the window with more data after it becomes empty, while preserving the
  address space, we must know where the the rightmost iterator was before the window became
  empty. For construction there is no prior value, so this value is passed in.  The window
  is empty after construction.

  The sliding-window consists of blocks.  Each block has a 'block address'.  Each item has an
  offset within a block. Sliding-Window extensions arrive in blocks of items, this is not 
  an implementation detail, rather it is important facet of the architecture.  Our calculations
  are locally fixed in size, and globally unbounded.

  The sliding-window may be exteneded by 'fetching' new blocks.  The fetch function
  is passed into the sliding-window upon construction.  Also, blocks may
  be placed into sliding-window by pushing them.  We do not ask how the fetch routine
  comes up with the data.

  In some applications the fetch routine should not block for too long of a duration of
  time.  Should fetch time out before filling a block but after consecutively placing some
  items have been placed in the block, the partial block should be returned.  A similar
  thing will happen should the item producer runs out of items.  Finally it is possible that
  a producer simply decides to end the stream.

  Hence, the rightmost blocks of the sliding-window may be partial. When the 
  rightmost block is partial, we complete it before going on to the next block. We always
  maintain block alignments. This assures that items have a contiguous address space
  within the blocks, and thus can be indexed.

  Incrementing an iterator beyond rightmost, or decrementing beyond lefmost, causes an off
  left or off right continuation to be followed rather than the usual continuation. So, for
  example, If the window is single ended, then one of these continuations will be an error
  while the other will fetch more data.

  The user may provide a new leftmost iterator, this may have the effect of causing
  blocks to be dropped from the left.

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
  ;; by keeping a separate block and item address we avoid having to do a divide to recover an index
  (struct address-type (block-address offset) #:mutable #:transparent)  ; address-0 = block address,  address-1 offset

  ;; status 'unitialized 'empty 'not-empty
  ;; when status is empty,  max holds the prior max address
  (struct range-type (status block-length address-leftmost address-rightmost) #:mutable #:transparent) 

  (define-type sliding-window:type)
  (define-type sliding-window-iterator:type)

;;--------------------------------------------------------------------------------
;; methods
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
  (obj:add-method sliding-window-iterator:type 'make sliding-window-iterator:make)
      
  (mc:define sliding-window:fetch-cap (args allocate-block-length) (conts cont-ok cont-eos cont-timedout)
    (cont-eos)
    )

  (define (sliding-window:make 
            [fetch sliding-window:fetch-cap]
            [block-length (current-block-length)]
            [initial-address (address-type 0 0)]
            )
    (define-object the-new-sliding-window sliding-window:type)
    (let*(
           [leftmost initial-address]
           [rightmost initial-address]
           [range (range-type 'empty block-length leftmost rightmost)]
           [window (make-hash)]
           )
      (obj:set! the-new-sliding-window sliding-window:type
        (Λ
          'range range
          'window window
          ))
      the-new-sliding-window
      ))
  (obj:add-method sliding-window:type 'make sliding-window:make)
      
   ;; an example fetch routine for testing
   ;; this needs a timer and to return partial blocks when it is taking too long to fill one
   ;; fetch returns a vector
   ;;
     (define example-data (range 0 100))
     (mc:define example-fetch-0 (Λ allocate-block-length) (Λ cont-ok cont-eos cont-timedout)
       (cond
         [(length≥ example-data allocate-block-length)
           (let(
                 [block (list->vector (take example-data allocate-block-length))]
                 )
             (set! example-data (drop example-data allocate-block-length))
             (cont-ok allocate-block-length block)
             )]
         [(pair? example-data)
           (let*(
                  [block (make-vector allocate-block-length)]
                  [actual-block-length (length example-data)]
                  [short-block (list->vector example-data)]
                  )
             (set! example-data '())
             (vector-copy! block 0 short-block)
             (cont-ok actual-block-length block)
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

