#|
 object
   
  created: 2014-12-04T07:55:00Z twl
  major revision: 2015-08-22T06:05:02Z twl  


|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/set)
  (require unstable/syntax) ; for (phase-of-enclosing-module)
  (require liquid/extensions)
  
;;--------------------------------------------------------------------------------
;;
  ;;(define obj:debug #t)
  (define obj:debug (make-parameter #t))
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
;; elementary objects have these keys reserved:
;;
  (define obj:key:copied-from 'obj:key:copied-from) ; holds an object


;;--------------------------------------------------------------------------------
;;  at the bottom of the recursion we find the following:
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


  (define (obj:->string objid [conv-fun display])
    (x-hash-ref (Λ obj:tman-table objid)
      (Λ
        (λ(e)(->string e conv-fun))
        (be "")
      )))


  ;; only function that allows an illegal objid
  ;;
    (define (obj:is objid) 
      ;;      (x-hash-ref (Λ obj:tman-table objid) (Λ (be true) (be false)))
      #t
      )

  ;; true if the given type has been added
   (define (obj:has-type objid type) 
     (when (not (obj:is type)) (raise:no-such-type type))

     (x-hash-ref (Λ obj:tman-table objid)
       (Λ 
         (λ(tman) 
           (x-hash-ref (Λ tman type)
             (Λ 
               (be true)
               (be false)
               )))
         (λ ignore-args (raise:no-such-object objid))
         )))


  ;; adds a type attribute (new section) to the compound object specified by 'objid'
  ;; put another way,  this object now shares the object specified by 'type'
  ;;
    (define (obj:declare-type objid type)

      ;; check that the objects are valid
      (when (not (obj:is type)) (raise:no-such-type type))
      (when (not (obj:is objid)) (raise:no-such-object objid))

      ;; if type isn't already there, add it
;;;--> integrate the declare type code into the has-type form  this has a redundant lookup
      (when (not (obj:has-type objid type))
        (x-hash-ref (Λ obj:tman-table objid)
          (Λ
            (λ(tman) (hash-set! tman type (make-hasheqv))) ; ok
            raise:unreachable ; we already checked and objid is in the tman-table
            )))

      objid
      )

;;-------------------------------------------------------------------------------- 
;;
;; All user visiable objects are compound objects.  All compound objects have sections
;; and these sections are given the same name as the objids of the type objects the
;; compound shares.
;;
;; We facilitate the convention that all objects used as type, have a section that shares
;; the object type-type. Type-type is then the type of type.  As such it is an object,
;; what type does it have?  It's type is also type-type. There is nothing wrong with this,
;; as it just means that the operators in type-type accept other type-type objects as well
;; as other type objects.
;;
  (define type-type (obj:make "type-type"))
  (declare-type type-type type-type)

  ;; a macro to make an object and declare its type to be 'type-type' in one step
  ;; i.e. make a type object that follows the convention that all type objects have type of 'type-type'
  ;;
    (define-syntax (define-type stx)
      (let(
            [datum  (syntax->datum stx)]
            )
        (let(
              [type-name (cadr datum)]
              )
          (let(
                [program
                  (Λ 'define type-name (Λ 'obj:declare-type (Λ 'obj:make (symbol->string type-name)) 'type-type))
                  ]
                )
            (when (obj:debug) (displayln program))
            (datum->syntax stx program)
            ))))

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
                   (Λ 'define object-name (Λ 'obj:declare-type (Λ 'obj:make object-specifier) type-name))
                   ]
                 )
            (when (obj:debug) (displayln program))
            (datum->syntax stx program)
            ))))



  ;; this just adds an elementary key to the type object, but it makes code more readible
  ;; e.g.  (add-method copmlex:type '== (λ(a b)(equal? a b)))
  ;;
    (define (obj:add-method type-name method-name method-def)
      (void (obj:set! type-name type-type (Λ method-name method-def)))
      )

    (define (obj:add-method* type-name methods)
      (void (obj:set! type-name type-type methods))
      )


;;--------------------------------------------------------------------------------
;; basic functions
;;
  
  ;;(require racket/trace)
  ;;(trace obj:elementary)

  ;; for module use an 'elementary' object is one without a type manifold.  In this
  ;;   implemetnation elementary objects only occur inside of compound objects An
  ;;   elementary object is found at the end of each leg of a type manifold.
  ;;
    (mc:define obj:get-ele (args objid type) (conts c0 c1)
      (x-hash-ref (Λ obj:tman-table objid)
        (Λ 
          (λ(tman) (x-hash-ref (Λ tman type) conts)) ; found the manifold, now get the elementary object
          (λ ignore-args (raise:no-such-object objid)) ; object not found in tman-table
          )))

    (mc:define obj:ele:ref (args ele key) (conts c-ok c-no-such-key)
      (x-hash-ref (Λ ele key)(Λ c-ok c-no-such-key))
      )
        
    (define (obj:ele:set! ele key val)
      (hash-set! ele key val)
      )


  ;; imposes type on an object, which selects an elementary object, 
  ;; then applies the key-val intiialization list
  ;;
    (mc:define obj:set! (args objid type Λkey-val) (conts continue-ok continue-no-type)
      (obj:get-ele (Λ objid type)
        (Λ
          (λ(e) (apply hash-set*! (cons e Λkey-val))) ; when elementary object found
          (λ ignore-args (continue-no-type args conts)) ; when objid has no such type
          ))
      (continue-ok objid)
      )

    ;; removes a key from an object, returns the objid
    (define (obj:remove! type objid key)
      (obj:get-ele (Λ objid type)
        (λ(an-elementary-object) (hash-remove! an-elementary-object key))
        (be objid)
        ))


  ;; imposes type and a key on the object, returns the value
  ;;    we pass the calling arguments to the error continuations, independent of where they are are
  ;;    invoked in the call tree.
  ;;
  ;;   note that the key name and object type are invarient in the recursion
  ;;
    (mc:define obj:ref (args objid type key) (conts continue-ok continue-no-key continue-no-such-type)
      (define (lookup-key-elementary e-obj) ;; given an elementary object, looks up the given key 
        (define (lookup-key-indirect . ignored-args) 
          (x-hash-ref (Λ e-obj obj:key:copied-from)
            (Λ
              (λ(objid-of-copied-from) (lookup-key-compound objid-of-copied-from)) ; found the copied-from object, now try accessing it
              (λ ignore-args (continue-no-key args conts)) ; ut oh ...  no copied-from object
              )))
        (when (obj:debug) (trace lookup-key-indirect))
        (x-hash-ref (Λ e-obj key)
          (Λ
            continue-ok
            lookup-key-indirect 
            ))
        )
      (when (obj:debug) (trace lookup-key-elementary))

      (define (lookup-key-compound objid) 
        (obj:get-ele (Λ objid type) ; reduce compound to elementary
          (Λ
            lookup-key-elementary
            (λ ignore-args (continue-no-such-type args conts)) ; no such type in the type manifold for this compund
            )))
      (when (obj:debug) (trace lookup-key-compound))

      (lookup-key-compound objid)
      )
  
  ;; returns looked up value or throws an exception
  (define (obj:ref* objid type key)
    (obj:ref (Λ objid type key) 
      (Λ 
        identity 
        (λ ignore-args (raise:no-such-key-in-elementary objid key))
        (λ ignore-args (raise:object-no-such-type-in-compound objid type))
        )))

  ;; in conventional object oriented programming, this would be a 'method call' where the
  ;; key the type object provides the method code or possibly method data.  Here the method
  ;;  must be a function that operates on the provided arg-objids.
  ;;
  ;; this routine first references the type object to find the method, then it applies the methods
  ;; to the given objects.  
  ;;
  ;;
    (mc:define obj:apply (args type method-name method-args) (conts continue-ok continue-no-key continue-no-type)
       (when (obj:debug)
         (displayln (Λ "obj:apply args:" (obj:lookup type) method-name method-args))
         (displayln (Λ "obj:apply conts:" continue-ok continue-no-key continue-no-type))
         )

       (obj:ref (Λ type type-type method-name)
         (Λ
           (λ(method-proc) (continue-ok (apply method-proc method-args)))
           (λ ignore-args (continue-no-key args conts))
           (λ ignore-args (continue-no-type args conts))
           )))

    ;; returns the value from applying the method, or throws an exception
    (define (obj:apply* type method method-args)
      (obj:apply (Λ type method method-args) 
        (Λ
          identity 
          (λ ignore-args (raise:no-such-key-in-elementary type method))
          (λ ignore-args (raise:object-no-such-type-in-compound type-type type))
          )))


  (define (obj:copy-element type source-objid target-objid)
    (obj:add-type target-objid type)
    (obj:set! (Λ target-objid type (Λ obj:key:copied-from source-objid))
      (Λ
        (be source-objid)
        raise:unreachable
        )))


  (define (list-types objid)
    (x-hash-ref (Λ obj:tman-table objid)
      (Λ
        (λ(tman) (hash-keys tman))
        (λ ignore-args (raise:no-such-object objid))
        )))

  ;; returns a set of types
  (define (obj:types objid) (apply mutable-seteqv (list-types objid)))

  (define (obj:copy source-objid target-objid)
    (for (
           [type (list-types source-objid)]
           )
      (obj:copy-element type source-objid target-objid)
      )
    target-objid
    )
    
  (define (obj:keys type objid)
    (obj:get-ele (Λ objid type)
      (Λ
        (λ(e)
          (let*(
                 [keys-list (hash-keys e)]
                 [keys (apply mutable-seteqv keys-list)]
                 )
            (x-hash-ref (Λ e obj:key:copied-from)
              (Λ
                (λ(source) (set-union! keys (obj:keys type source)))
                (be keys)
                ))))
        (be (mutable-seteqv)) ; returns an empty set
        )))
    
   (define (obj:has-key type objid key)
     (obj:get-ele (Λ objid type)
       (Λ
         (λ(e)
           (or
             (hash-has-key? e key)
             (x-hash-ref (Λ e obj:key:copied-from)
               (Λ
                 (λ(source) (obj:has-key type source key))
                 (be false)
                 ))))
         (be false)
         )))


;;--------------------------------------------------------------------------------
;;  tests
;;

  (define (obj-test-0)
    (let(
          [summable (obj:make "type:summable")] ; summable is a type
          [obj1 (obj:make "test-0-obj1")]     ; an empty typeless object
          )

      (obj:add-type obj1 summable) ; endows the object with type 'summable'
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

      (obj:add-type summable type-type)

      ;; gives the object summable one key named '+' and intializes it to a function that adds two sumable objects
      (obj:set! 
        (Λ summable type-type 
          (Λ
            '+ plus
            )
          )
        (Λ void raise:unreachable)
        )

      (obj:add-type obj1 summable)
      (obj:add-type obj2 summable)

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

    (obj:add-type obj1 summable)
    (obj:add-type obj2 summable)
    
    (obj:set! (Λ obj2 summable (Λ 'x 21)) (Λ void raise:unreachable))
    (obj:copy obj2 obj1)

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
     (obj:add-type type-type type-type)
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
  (provide
    (obj:debug)
    obj:name-hook
    obj:lookup
    obj:key:copied-from
    obj:exception:no-such-key-in-elementary
    obj:exception:no-such-object
    obj:exception:no-such-type-in-compound
    type-type
    define-type
    define-object
    )

  (provide-with-trace "object"
    obj:->string
    obj:add-method
    obj:add-method*
    obj:apply
    obj:apply*
    obj:construct
    obj:copy
    obj:copy-element
    obj:declare-type
    obj:get-ele     
    obj:keys
    obj:has-key
    obj:has-type
    obj:is
    obj:make
    obj:ref
    obj:ref*
    obj:remove!
    obj:set!
    obj:types
    )

    (provide-with-trace "object-private"
      raise:no-such-object
      raise:no-such-type
      raise:object-no-such-type-in-compound
      raise:no-such-key-in-elementary
      raise:unreachable
      list-types
      )

  (when (obj:debug)
    (object-trace)
    (object-private-trace)
    )
