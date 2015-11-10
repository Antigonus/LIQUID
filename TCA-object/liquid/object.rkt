#|
 object
   
  created: 2014-12-04T07:55:00Z twl
  major revision: 2015-08-22T06:05:02Z twl  

  polymorphism and latent copy happen at the ele level

  if a function would just pass through an arguent as a return value we instead return void

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/set)
  (require unstable/syntax) ; for (phase-of-enclosing-module)
  (require liquid/extensions)
  (require (for-syntax liquid/extensions))
  
;;--------------------------------------------------------------------------------
;; debug stuff
;;
  (define obj:debug (make-parameter #f))
  (define-for-syntax  obj:debug (make-parameter #t))

  (define obj:names (make-hash))
  (define (obj:name-hook objid name)
    (when (obj:debug) (display "(")(display objid) (display ".")(display name)(displayln ")"))
    (hash-set! obj:names objid name))
  (define (obj:lookup objid)
    (hash-ref obj:names objid (string-append (->string objid) ":not-registered-with-a-name")))



;;--------------------------------------------------------------------------------
;; exceptions
;;
;;   sometimes we are faced with doing something for condition that can't happen due to
;;   the logic of the code, in these cases we should raise an exception just in case there
;;   is a bug
;; 
;;   We also use exceptions when the caller breaks a contract with the library, and we
;;   don't know what else to do.  I.e. when the code simply isn't designed to deal with
;;   the case.
;;
;;   Otherwise we use continuations
;;
  (define obj:exception:no-such-object 'obj:exception:no-such-object)
  (define obj:exception:no-such-type 'obj:exception:no-such-type)
  (define obj:exception:no-such-type-in-compound 'obj:exception:no-such-type-in-compound)
  (define obj:exception:no-such-key-in-elementary 'obj:exception:no-such-key-in-elementary)
  (define obj:exception:broken 'obj:exception:broken)

  (define (raise:no-such-object the-bad-obj)
    (display "not a valid objid: ")
    (display the-bad-obj)
    (newline)
    (raise obj:exception:no-such-object))

  (define (raise:no-such-type the-bad-type)
    (display "this type object is not found in the table of valid objects: ")
    (display the-bad-type)
    (newline)
    (raise obj:exception:no-such-type))

  (define (raise:object-no-such-type-in-compound objid the-bad-type)
    (display "for objid: ")
    (display objid)
    (display ", object has no such type branch in its type manifold: ")
    (display the-bad-type)
    (newline)
    (raise obj:exception:no-such-type-in-compound))

  (define (raise:no-such-key-in-elementary objid the-bad-key)
    (display "object ") (display objid) 
    (display ",has no such key: ")
    (display the-bad-key)
    (newline)
    (raise obj:exception:no-such-key-in-elementary))

  (define (raise:unreachable . args) ;; when there is just no way this should have been called
    (display "this code should not have been reached, the program has a bug.")
    (newline)
    (raise obj:exception:broken))


;;--------------------------------------------------------------------------------
;;  elementary object - aka ele
;;

  ;; special keys
  ;;
    (define obj:key:copied-from 'obj:key:copied-from) ; holds the ele from an object this ele was copied from
    (define obj:key:type 'obj:key:type) ; for polymorphic typing and type introspection

  ;; ele access
  ;;
    (mc:define obj:ele:ref (args ele key) (conts cont-ok cont-no-such-key)
      (x-hash-ref (Λ ele key)
        (Λ
          cont-ok 
          (λ args-not-used ; key not in this ele, perhaps it is lazy copied from another
            (x-hash-ref (Λ ele obj:key:copied-from)
              (Λ
                (λ(copied-from-this-ele) 
                  (obj:ele:ref (Λ copied-from-this-ele key) conts)
                  )
                cont-no-such-key
              ))))))

  ;; this will once again expose the copied from object
  (define (obj:ele:remove! ele key)
    (hash-remove! ele key)
    )

  (define (obj:ele:set! ele Λkey-val)
    (apply hash-set*! (Λ ele ,Λkey-val))
    )

  (define (obj:ele:keys ele)
    (let*(
           [keys-list (hash-keys ele)]
           [keys (apply mutable-seteqv keys-list)]
           )
      (x-hash-ref (Λ ele obj:key:copied-from)
        (Λ
          (λ(copied-from-this-ele) (set-union! keys (obj:ele:keys copied-from-this-ele)))
          (be keys)
          ))))

  (define (obj:ele:has-key ele key)
    (or
      (hash-has-key? ele key)
      (x-hash-ref (Λ ele obj:key:copied-from)
        (Λ
          (λ(copied-from-this-ele) (obj:ele:has-key copied-from-this-ele key))
          (be false)
          ))))

  ;; copy-from
  ;;    obj:ele:ref continues to come from the copied-from ele until the attribute is set
  ;;    obj:else:set! directly affects the specified ele
  ;;
    (define (obj:ele:copy-from copied-from-ele )
      (let(
            [this-ele (make-hasheqv)]
            )
        (obj:ele:set! this-ele (Λ obj:key:copied-from copied-from-ele))
        this-ele
        ))

;;--------------------------------------------------------------------------------
;;  compound object
;;
  (define obj:tman-table (make-hasheq)) ; table of type manifolds with each branch of the manifold holding an elementary obj

  (define (obj:make  [debug-name "made-with-no-name"])
    (let(
          [objid (unique-to-session-number)]
          [a-type-manifold (make-hasheqv)]
          )
      (hash-set! obj:tman-table objid a-type-manifold)
      (when (obj:debug) (obj:name-hook objid debug-name))
      objid
      ))

  ;; this and ->string are the only functions that will not throw an exception when given an illegal objid
  (mc:define obj:is (args objid) (conts cont-yes cont-no)
    (x-hash-ref (Λ obj:tman-table objid) 
      (Λ 
        (λ ignore-args (cont-yes))
        (λ ignore-args (cont-no))
        )))

  ;; used internally for looking up the objid, the return value is often called tman for 'type manifold'
  (define (get-obj objid)
    (x-hash-ref (Λ obj:tman-table objid)
      (Λ
        identity
        (λ args (raise:no-such-object objid))
        )))

  ;; is this enough to make the object transparent?
  (define (obj:->string objid [conv-fun display])
    (obj:is (Λ objid)
      (Λ
        (->string (get-obj objid) conv-fun)
        "#illegal-objid"
        )))

  ;; gets the elememtary object out of the compound object, see the elementary object section above
  (mc:define obj:get-ele (args objid type) (conts cont-ok cont-no-such-type)
    (let(
          [tman (get-obj objid)]
          )
      (x-hash-ref (Λ tman type)
        (Λ
          cont-ok
          cont-no-such-type
          ))))

;;-------------------------------------------------------------------------------- 
;;  type manipulation
;;
;;

  ;; adds a type attribute (new section) to the compound object
  ;; this can be called multiple times to give an object mutliple type sections
  ;; if the section exists nothing is done
  ;; returns the new ele or ele found in the type section
  ;;
    (define (obj:declare-type objid type)
      (let(
            [tman (get-obj objid)]
            )
        (x-hash-ref (Λ tman type)
          (Λ
            identity
            (λ ignore-args
              (define new-ele (make-hasheqv))
              (hash-set! tman type new-ele)
              new-ele
              )))))

  (define (list-types objid)
    (let(
          [tman (get-obj objid)]
          )
      (hash-keys tman)
      ))

  (define (obj:types objid)
    (mutable-seteqv (list-types objid))
    )

  (define (obj:has-type objid type)
    (let(
          [tman (get-obj objid)]
          )
      (hash-has-key? tman type)
      ))

  (define (obj:keys objid type)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(e) (obj:ele:keys e))
        (be (mutable-seteqv)) ; returns an empty set
        )))
    
  (define (obj:has-key objid type key)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(e)(obj:ele:has-key e key))
        (be false)
        )))

  ;;  type objects are given this type
  (define type-type (obj:make "type-type"))
  (void (obj:declare-type type-type type-type))

  ;; macros to simplify creating new types or new typed objects
  ;;
    ;; e.g. (define-type a-type)
    (define-syntax (define-type stx)
      (let(
            [datum  (syntax->datum stx)]
            )
        (let(
              [type-name (cadr datum)]
              )
          (let(
                [program
                  (Λ 'begin
                    (Λ 'define type-name (Λ 'obj:make (symbol->string type-name)))
                    (Λ 'obj:declare-type type-name 'type-type)
                    type-name
                    )]
                )
            (when (obj:debug) (displayln program))
            (datum->syntax stx program)
            ))))

    ;; e.g. (define-object apple fruit)  use declare-type to give additional type sections to the object
    (define-syntax (define-object stx)
      (let(
            [datum  (syntax->datum stx)]
            )
        (let(
              [object-name (cadr datum)]
              [type-name (caddr datum)]
              )
          (let*(
                 [object-specifier (string-append (symbol->string type-name) "::" (symbol->string object-name))]
                 [program
                   (Λ 'begin
                     (Λ 'define object-name (Λ 'obj:make (symbol->string object-specifier)))
                     (Λ 'obj:declare-type object-name type-name)
                     object-name
                     )]
                 )
            (when (obj:debug) (displayln program))
            (datum->syntax stx program)
            ))))


;;-------------------------------------------------------------------------------- 
;;  compound object manipulations
;;

  ;; removes a key from an object
  (define (obj:remove! type objid key)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(ele) (obj:ele:remove! ele key))
        void
      )))

  (mc:define obj:set! (args objid type Λkey-val) (conts continue-ok cont-no-such-type)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(ele) (obj:ele:set! ele Λkey-val))
        cont-no-such-type
        ))
    (continue-ok objid)
    )

  ;; returns a value from the object
  (mc:define obj:ref (args objid type key) (conts cont-ok cont-no-such-key cont-no-such-type)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(ele) (obj:ele:ref (Λ ele key)
                  (Λ
                    cont-ok
                    cont-no-such-key
                    )))
        cont-no-such-type
        )))

  (define (copy-from-ele this-tman type copied-from-tman)
    (let(
          [this-ele (x-hash-ref (Λ this-tman type)
                      (Λ
                        identity
                        (λ ignore-args (raise 'no-such-type-in-compound))
                        ))]
          [copied-from-ele (x-hash-ref (Λ copied-from-tman type)
                      (Λ
                        identity
                        (λ ignore-args (raise 'no-such-type-in-compound))
                        ))]
          )
      (obj:ele:set! this-ele (Λ obj:key:copied-from copied-from-ele))
      ))

  (define (obj:copy-from-ele this-objid type copied-from-objid)
    (let(
          [this-tman (get-obj this-objid)]
          [copied-from-tman (get-obj copied-from-objid)]
          )
      (copy-from-ele this-tman type copied-from-tman)
      ))

  (define (obj:copy-from this-objid copied-from-objid)
    (let(
          [this-tman (get-obj this-objid)]
          [copied-from-tman (get-obj copied-from-objid)]
          )
      (let(
            [copied-from-types (hash-keys copied-from-tman)]
            )
        (for(
              [type copied-from-types]
              )
          (copy-from-ele this-tman type copied-from-tman)
          ))))



;;--------------------------------------------------------------------------------
;; method support
;;   most function methods will be added to the shared type rather than the object itself
;;      

  ;; this just adds an attribute to the specified type object, but it makes code more readible
  ;; e.g.  (add-method copmlex:type '== (λ(a b)(equal? a b)))
  ;;
    (define (obj:add-method type method-name method-def)
      (obj:set! type type-type (Λ method-name method-def))
      )

    (define (obj:add-method* type methods)
      (obj:set! type type-type methods)
      )

  (mc:define obj:apply (args type method-name method-args) (conts continue-ok continue-no-key continue-no-type)
     (when (obj:debug)
       (displayln (Λ "obj:apply args:" (obj:lookup type) method-name method-args))
       (displayln (Λ "obj:apply conts:" continue-ok continue-no-key continue-no-type))
       )
     (obj:ref (Λ type type-type method-name)
       (Λ
         (λ(method-proc) (continue-ok (apply method-proc method-args)))
         continue-no-key
         continue-no-type
         )))

  ;; returns the value from applying the method, or throws an exception
  (define (obj:apply* type method method-args)
    (obj:apply (Λ type method method-args) 
      (Λ
        identity 
        (λ ignore-args (raise:no-such-key-in-elementary type method))
        (λ ignore-args (raise:object-no-such-type-in-compound type-type type))
        )))



;;--------------------------------------------------------------------------------
;;  tests
;;
  (define (obj-test-0)
    (let(
          [summable (obj:make "type:summable")] ; summable is a type
          [obj1 (obj:make "test-0-obj1")]     ; an empty typeless object
          )

      (obj:declare-type obj1 summable) ; endows the object with type 'summable'
      (obj:set! (Λ obj1 summable (Λ 'x 5 'y 7)) (Λ void raise:unreachable)) ; gives obj1 keys x and y initialized to 5 and 7
      (obj:ref (Λ obj1 summable 'x)
        (Λ
          (λ(x) (= x 5))
          (λ ignore-args (raise 'exception:obj-test-0-1))
          (λ ignore-args (raise 'exception:obj-test-0-2))
          ))))

  (define (obj-test-1)
    
    (let(
          [summable (obj:make "type:summable")]
          [obj1 (obj:make "test-1-obj1")]
          [obj2 (obj:make "test-1-obj2")]
          )

      (define (plus a b)
        (define (plus-1 ax) ;; adds ax to the value in the x key of b
          (obj:ref (Λ b summable 'x)
            (Λ
              (λ(bx) (+ ax bx))
              (λ ignore-args (raise 'exception:obj-test-0-1))
              (λ ignore-args (raise 'exception:obj-test-0-2))
              )))
        (when (obj:debug) (trace plus-1))
          
        (obj:ref (Λ a summable 'x)
          (Λ
            plus-1
            (λ ignore-args (raise 'exception:obj-test-0-3))
            (λ ignore-args (raise 'exception:obj-test-0-4))
            ))
        )
      (when (obj:debug) (trace plus))

      (obj:declare-type summable type-type)

      ;; gives the object summable one key named '+' and intializes it to a function that adds two sumable objects
      (obj:set! 
        (Λ summable type-type 
          (Λ
            '+ plus
            )
          )
        (Λ void raise:unreachable)
        )

      (obj:declare-type obj1 summable)
      (obj:declare-type obj2 summable)

      (obj:set! (Λ obj1 summable (Λ 'x 5)) (Λ void raise:unreachable))
      (obj:set! (Λ obj2 summable (Λ 'x 7)) (Λ void raise:unreachable))

      (obj:apply (Λ summable '+ (Λ obj1 obj2)) ; sums the two objects
        (Λ
          (λ(sum) (= sum 12))
          (λ ignore-args (raise 'exception:obj-test-0-5))
          (λ ignore-args (raise 'exception:obj-test-0-6))
          ))))

(define (obj-test-2)
  (let(
        [summable (obj:make "type:summable")]
        [obj1 (obj:make "test-2-obj1")]
        [obj2 (obj:make "test-2-obj2")]
        [test-result #f]
        )

    (obj:declare-type obj1 summable)
    (obj:declare-type obj2 summable)
    
    (obj:set! (Λ obj2 summable (Λ 'x 21)) (Λ void raise:unreachable))
    (obj:copy-from obj1 obj2)

    (obj:ref (Λ obj1 summable 'x)
      (Λ
        (λ(x) (set! test-result (and test-result (= x 21))))
        (λ ignore-args (raise 'exception:obj-test-0-1))
        (λ ignore-args (raise 'exception:obj-test-0-2))
        ))
      
    ;; note, if we change the original, the lazy copy will continue to read from the original,
    ;; and thus get the updated value
    (obj:set! (Λ obj2 summable (Λ 'x 11)) (Λ void raise:unreachable))
    (obj:ref (Λ obj1 summable 'x)
      (Λ
        (λ(x) (set! test-result (= x 11)))
        (λ ignore-args (raise 'exception:obj-test-0-1))
        (λ ignore-args (raise 'exception:obj-test-0-2))
        ))

    ;; writing the copy will stop the reading from the original
    (obj:set! (Λ obj1 summable (Λ 'x 8)) (Λ void raise:unreachable))
    (obj:ref (Λ obj1 summable 'x)
      (Λ
        (λ(x) (set! test-result (and test-result (= x 8))))
        (λ ignore-args (raise 'exception:obj-test-0-1))
        (λ ignore-args (raise 'exception:obj-test-0-2))
        ))
    test-result
    ))


;;--------------------------------------------------------------------------------
;; initialize module
;;
   (define (obj:construct)
     (obj:declare-type type-type type-type)
     (when
       (and
         (= (phase-of-enclosing-module) 0)
         (current-hook-tests)
         )
       (test-hook obj-test-0)      
       (test-hook obj-test-1)
       (test-hook obj-test-2)
       ))

    (obj:construct)

;;--------------------------------------------------------------------------------
;; provides the following
;;    
  #|
    (provide
      obj:name-hook
      obj:lookup
      obj:key:copied-from
      obj:key:type
      )
  |#

    (provide
      obj:debug
      obj:exception:no-such-key-in-elementary
      obj:exception:no-such-object
      obj:exception:no-such-type-in-compound
      type-type
      define-type
      define-object
      )

   (provide (for-syntax obj:debug))


  (provide-with-trace "object"

    raise:no-such-object
    raise:no-such-type
    raise:object-no-such-type-in-compound
    raise:no-such-key-in-elementary
    raise:unreachable

    obj:ele:ref
    obj:ele:remove!
    obj:ele:set!
    obj:ele:keys
    obj:ele:has-key
    obj:ele:copy-from
    
    obj:make
    obj:is
    obj:->string
    obj:get-ele

    obj:declare-type
    obj:types
    obj:has-type
    obj:keys
    obj:has-key
    
    obj:remove!
    obj:set!
    obj:ref
    obj:copy-from-ele
    obj:copy-from

    obj:add-method
    obj:add-method*
    obj:apply
    obj:apply*
    )

    (provide-with-trace "object-private"
      list-types
      copy-from-ele
      )

  (when (obj:debug)
    (object-trace)
    (object-private-trace)
    )
