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
  (define obj:debug #f)
  (define obj:names (make-hash))
  (define (obj:name-hook objid name)
    (when obj:debug (display "(")(display objid) (display ".")(display name)(displayln ")"))
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
  (define obj:exception:no-such-field-in-elementary 'obj:exception:no-such-field-in-elementary)
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

  (define (raise:no-such-field-in-elementary objid the-bad-field)
    (display "object ") (display objid) 
    (display ",has no such field: ")
    (display the-bad-field)
    (newline)
    (raise obj:exception:no-such-field-in-elementary))

  (define (raise:unreachable . args) ;; when there is just no way this should have been called
    (display "this code should not have been reached, the program has a bug.")
    (newline)
    (raise obj:exception:broken))



;;--------------------------------------------------------------------------------
;; elementary objects have these fields reserved:
;;
  (define field:copied-from 'field:copied-from) ; holds an object


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
      (when obj:debug (obj:name-hook objid debug-name))
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


  ;; adds type to the object
  ;;
    (define (obj:add-type objid type)

      ;; check that the objects are valid
      (when (not (obj:is type)) (raise:no-such-type type))
      (when (not (obj:is objid)) (raise:no-such-object objid))

      ;; if type isn't already there, add it
;;;--> integrate the add type code into the has-type form  this has a redundant lookup
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
;; In this implementation of TCA objects, we have added a constraint to make the
;; interfaces uniform: we require that all objects be compound objects, i.e. this is a
;; compound object interface.  In TCA type is sipmly an object that gets shared among
;; other objects 'of the type', so this constraint really says that all objects must
;; specify a shared object.
;;
;; ah, so you might ask what is the type for a type object?  Really it could be any
;; other object shared among the type objects, and used for operating on them. We
;; we provide a type that supports the method apply, it is called type-type.
;; 
;; ah, so you might ask, what is the type of type-type?  The type of type-type is
;; type-type.  There is nothing wrong with this, as it just means that the operators in
;; type-type accept other type-type objects as well as other type objects.
;;
  (define type-type (obj:make "type-type"))


;;--------------------------------------------------------------------------------
;; basic functions
;;
  
  ;;(require racket/trace)
  ;;(trace get-elementary)

  ;; for module use
  ;;   an 'elementary' object is one without a type manifold.  In this implemetnation
  ;;   elementary objects only occur inside of compound objects, and are only seen
  ;;   by our library code.  An elementary object is found at the end of each leg
  ;;   of a type manifold.
  ;;
    (mc:define get-elementary (args objid type) (conts c0 c1)
      (x-hash-ref (Λ obj:tman-table objid)
        (Λ 
          (λ(tman) (x-hash-ref (Λ tman type) conts)) ; found the manifold, now get the elementary object
          (λ ignore-args (raise:no-such-object objid)) ; object not found in tman-table
          )))


  ;; imposes type on an object, which selects an elementary object, 
  ;; then applies the field-val intiialization list
  ;;
    (mc:define obj:set! (args type objid Λfield-val) (conts continue-ok continue-no-type)
      (get-elementary (Λ objid type)
        (Λ
          (λ(e) (apply hash-set*! (cons e Λfield-val))) ; when elementary object found
          (λ ignore-args (continue-no-type args conts)) ; when objid has no such type
          ))
      (continue-ok objid)
      )

    ;; removes a field from an object, returns the objid
    (define (obj:remove! type objid field)
      (get-elementary (Λ objid type)
        (λ(an-elementary-object) (hash-remove! an-elementary-object field))
        (be objid)
        ))


  ;; imposes type and a field on the object, returns the value
  ;;    we pass the calling arguments to the error continuations, independent of where they are are
  ;;    invoked in the call tree.
  ;;
  ;;   note that the field name and object type are invarient in the recursion
  ;;
    (mc:define obj:ref (args type objid field) (conts continue-ok continue-no-field continue-no-such-type)
      (define (lookup-field-elementary e-obj) ;; given an elementary object, looks up the given field 
        (define (lookup-field-indirect . ignored-args) 
          (x-hash-ref (Λ e-obj field:copied-from)
            (Λ
              (λ(objid-of-copied-from) (lookup-field-compound objid-of-copied-from)) ; found the copied-from object, now try accessing it
              (λ ignore-args (continue-no-field args conts)) ; ut oh ...  no copied-from object
              )))
        (when obj:debug (trace lookup-field-indirect))
        (x-hash-ref (Λ e-obj field)
          (Λ
            continue-ok
            lookup-field-indirect 
            ))
        )
      (when obj:debug (trace lookup-field-elementary))

      (define (lookup-field-compound objid) 
        (get-elementary (Λ objid type) ; reduce compound to elementary
          (Λ
            lookup-field-elementary
            (λ ignore-args (continue-no-such-type args conts)) ; no such type in the type manifold for this compund
            )))
      (when obj:debug (trace lookup-field-compound))

      (lookup-field-compound objid)
      )
  
  ;; returns looked up value or throws an exception
  (define (obj:ref* type objid field)
    (obj:ref (Λ type objid field) 
      (Λ 
        identity 
        (λ ignore-args (raise:no-such-field-in-elementary objid field))
        (λ ignore-args (raise:object-no-such-type-in-compound objid type))
        )))

  ;; in conventional object oriented programming, this would be a 'method call' where the
  ;; field the type object provides the method code or possibly method data.  Here the method
  ;;  must be a function that operates on the provided arg-objids.
  ;;
  ;; this routine first references the type object to find the method, then it applies the methods
  ;; to the given objects.  
  ;;
  ;;
    (mc:define obj:apply (args type method-name method-args) (conts continue-ok continue-no-field continue-no-type)
       (when obj:debug
         (displayln (Λ "obj:apply args:" (obj:lookup type) method-name method-args))
         (displayln (Λ "obj:apply conts:" continue-ok continue-no-field continue-no-type))
         )

       (obj:ref (Λ type-type type method-name)
         (Λ
           (λ(method-proc) (continue-ok (apply method-proc method-args)))
           (λ ignore-args (continue-no-field args conts))
           (λ ignore-args (continue-no-type args conts))
           )))

    ;; returns the value from applying the method, or throws an exception
    (define (obj:apply* type method method-args)
      (obj:apply (Λ type method method-args) 
        (Λ
          identity 
          (λ ignore-args (raise:no-such-field-in-elementary type method))
          (λ ignore-args (raise:object-no-such-type-in-compound type-type type))
          )))


  (define (obj:copy-element type source-objid target-objid)
    (obj:add-type target-objid type)
    (obj:set! (Λ type target-objid (Λ field:copied-from source-objid))
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
    
  (define (obj:fields type objid)
    (get-elementary (Λ objid type)
      (Λ
        (λ(e)
          (let*(
                 [fields-list (hash-keys e)]
                 [fields (apply mutable-seteqv fields-list)]
                 )
            (x-hash-ref (Λ e field:copied-from)
              (Λ
                (λ(source) (set-union! fields (obj:fields type source)))
                (be fields)
                ))))
        (be (mutable-seteqv)) ; returns an empty set
        )))
    
   (define (obj:has-field type objid field)
     (get-elementary (Λ objid type)
       (Λ
         (λ(e)
           (or
             (hash-has-key? e field)
             (x-hash-ref (Λ e field:copied-from)
               (Λ
                 (λ(source) (obj:has-field type source field))
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
      (obj:set! (Λ summable obj1 (Λ 'x 5 'y 7)) (Λ void raise:unreachable)) ; gives obj1 fields x and y initialized to 5 and 7
      (obj:ref (Λ summable obj1 'x)
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
        (define (plus-1 ax) ;; adds ax to the value in the x field of b
          (obj:ref (Λ summable b 'x)
            (Λ
              (λ(bx) (+ ax bx))
              (λ ignore-args (raise 'exception:obj-test-0-1))
              (λ ignore-args (raise 'exception:obj-test-0-2))
              )))
        (when obj:debug (trace plus-1))
          
        (obj:ref (Λ summable a 'x)
          (Λ
            plus-1
            (λ ignore-args (raise 'exception:obj-test-0-3))
            (λ ignore-args (raise 'exception:obj-test-0-4))
            ))
        )
      (when obj:debug (trace plus))

      (obj:add-type summable type-type)

      ;; gives the object summable one field named '+' and intializes it to a function that adds two sumable objects
      (obj:set! 
        (Λ type-type summable
          (Λ
            '+ plus
            )
          )
        (Λ void raise:unreachable)
        )

      (obj:add-type obj1 summable)
      (obj:add-type obj2 summable)

      (obj:set! (Λ summable obj1 (Λ 'x 5)) (Λ void raise:unreachable))
      (obj:set! (Λ summable obj2 (Λ 'x 7)) (Λ void raise:unreachable))

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
    
    (obj:set! (Λ summable obj2 (Λ 'x 21)) (Λ void raise:unreachable))
    (obj:copy obj2 obj1)

    (obj:ref (Λ summable obj1 'x)
      (Λ
        (λ(x) (set! test-result (and test-result (= x 21))))
        (λ ignore-args (raise 'exception:obj-test-0-1))
        (λ ignore-args (raise 'exception:obj-test-0-2))
        ))
      
    ;; note, if we change the original, the lazy copy will continue to read from the original,
    ;; and thus get the updated value
    (obj:set! (Λ summable obj2 (Λ 'x 11)) (Λ void raise:unreachable))
    (obj:ref (Λ summable obj1 'x)
      (Λ
        (λ(x) (set! test-result (= x 11)))
        (λ ignore-args (raise 'exception:obj-test-0-1))
        (λ ignore-args (raise 'exception:obj-test-0-2))
        ))

    ;; writing the copy will stop the reading from the original
    (obj:set! (Λ summable obj1 (Λ 'x 8)) (Λ void raise:unreachable))
    (obj:ref (Λ summable obj1 'x)
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
    obj:debug
    obj:name-hook
    obj:lookup
    field:copied-from
    obj:exception:no-such-field-in-elementary
    obj:exception:no-such-object
    obj:exception:no-such-type-in-compound
    type-type
    )

  (provide-with-trace "object"
    obj:has-type
    obj:add-type
    obj:apply
    obj:apply*
    obj:construct
    obj:copy
    obj:copy-element
    obj:types
    obj:has-field
    obj:is
    obj:fields
    obj:make
    obj:ref
    obj:ref*
    obj:remove!
    obj:set!
    obj:->string
    )

    (provide-with-trace "object-private"
      raise:no-such-object
      raise:no-such-type
      raise:object-no-such-type-in-compound
      raise:no-such-field-in-elementary
      raise:unreachable
      get-elementary     
      list-types
      )

  (when obj:debug
    (object-trace)
    (object-private-trace)
    )
