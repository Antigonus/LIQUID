#|
 streams
   
  created: 2015-11-04T02:43:31Z twl 

  increasingly structured streams

|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require liquid/extensions)
  (require liquid/object)


;;--------------------------------------------------------------------------------
;; 
  (define streams-debug (make-parameter #t))



;;--------------------------------------------------------------------------------
;; byte stream
;;
;;  stream with iteration,  'get' on the itertor returns a block of bytes
;;  the byte stream is read only
;;
;;  we buffer blocks with addresses equal to or greater than the smallest
;;  outstanding iterator
;;
;;  each block is keyed by the block address, the value contains a ref count (for
;;  iterators), a length, and a byte string
;;
  (define-type byte-stream:type)

  (define (byte-stream:make input-stream)
    (define-object a-byte-stream byte-stream:type)
    (obj:set! a-byte-stream byte-stream:type 
      (Λ
        'input-stream input-stream
        'buffer (make-hash)
        'smallest-iterator '(0 -1)   ; '(ref-count block-address)
        'largest-block-address  -1
        ))
    a-byte-stream
    )


;;--------------------------------------------------------------------------------
;; byte stream iterator
;; 
;;
  (define-type byte-stream-iterator:type)

  ;; for internal use in the byte-stream
  (define (byte-stream-iterator:make a-byte-stream a-block-address)
    (define-object an-iterator byte-stream-iterator:type)
    (obj:set! an-iterator byte-stream-iterator:type 
      (Λ
        'target-sequence a-byte-stream
        'address a-block-address
        ))
    an-iterator
    )


;;--------------------------------------------------------------------------------
;;  methods
;;

  (obj:add-method byte-stream:type 'first
    (λ(a-byte-stream)
      (let*(
             [block (make-bytes (current-byte-stream-block-length))]
             [actual-block-length (read-bytes! block 0 (-- (current-byte-stream-block-length)))]
             )
        (let*(
               [e  (obj:elementary (Λ a-byte-stream byte-stream:type) (Λ identity raise:unreachable))]
               [buffer (hash-ref e 'buffer)]
               )
          (hash-set! buffer 0 (Λ 1 actual-block-length block))
          (hash-set! e 'smallest-iterator '(1 0))
          (hash-set! e 'largest-block-addres 0)
          (byte-stream-iterator:make a-byte-stream 0)
          ))))

    (obj:add-method byte-stream-iterator:type 'next
;;      (λ( ('arg 
      )
