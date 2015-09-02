#|
 object
   
  created: 2014-12-04T07:55:00Z twl
  major revision: 2015-08-22T06:05:02Z twl  

  Herein I implement some semblance of the type model from my Turing Complete
  Architecture.  

  A 'elementary object' is a kind of indexed container, where the index is a symbol.
  Herein we implement elementary objects with hash tables.  Related concepts are those of
  the associative list, and the data structure.  

  Each elementary object is said to have 'type'. Type is simply an association with another
  object.  Type objects are typically shared, and typically have functions in their entries.
  When we use the functions of a type object to affect other objects, we say that 'type
  is imposed' on the other object.  Type is imposed externally, i.e. the association
  is done by the caller, i.e. the user of the object.
  
  As a matter of simplifying this implementation, we consider an elementary object to be
  a case of a composite object.  So all our objects are composite objects.

  All composite objects are held in a 'type manifold table'.  This table is indexed by
  an id, which we affectionately call the 'objid'.  Our objects are never used directly,
  but rather are accessed through the objid.

  Hence, each objid is owned by a type manifold table. ID ownship by an objid table defines an
  object space.  Here in this first implementation we just have one global table for all
  objids.

  An entry in the type manifold table is a type manifold, strangely enough. This table
  is indexed against imposed type.  So if the composite object is really an elementary
  object, then the type manifold table will have one entry, that of the type of the 
  elementary object.  In general composite objects may be composed of many different
  type of elementary objects.  With this structure we can get the same functionality as
  gotten with sipmle or multiple inheritance.
  
  --

  As another feature of our objects, we support lazy copying.  With lazy copying 
  reading a value from an object continues to return the value from the copied from
  object instead of the new object until a write occurs. We use a hack to implment this
  feature, we reserve the key 'copied-from'  in all elemetnary objects.  The value for 
  this key is then the objid for the object being copied from.  If the elementary object
  has no such key entry, then the object is not being copied from another.
  
  --

  We use continuations when control decisions must be made within a function.  We
  use exceptions when conditions occur within a function for which the function was
  not designed to handle.  For example, not finding an objid in the obj:tman-table.
  
  --

  fields might be made an allocted resource with a lookup table should they need to
  convert to tring.  That way they would all be unique and it would not be necessary to
  have two levels of lookup to get to the data.

  --

  what we expect from objects:
  
  1. secret payloads that legacy code does not see.  This can be used to all
     features for new code without breaking old code.

  2. personality - when legacy code sends an operator message to an object
     the new object may perform a different operation than legacy objects do.
     I.e. the new objects takes the same message, and the same operation name,
     and does a different operation.   (The message may vary a little, see #1)

  3. intropsective typing.  The object itself picks the real operator given the name of
     the operator. We do have to agree on a convention of which operator names are
     available.  .. support heterogenous containers and generic algorithms.
  
  #3 is at odds with #1.  How do we now when to give legacy behavior so as
     not to break old code, and when to give new behavior?  Seems there must
     always be an externally imposed type context - even if that type means
     to select type from a number of options.

  polymoprhism - redefine the type object, replace the type object in the tman
  table with an updated version that checks for the extention type component
  or uses a different type field if it is available


|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/set)
  (require "misc-lib.rkt")
  
;;--------------------------------------------------------------------------------
;;
  (define obj:debug #f)
  (define obj:names (make-hash))
  (define (obj:name-hook key name)
    (display "(")(display key) (display ".")(display name)(displayln ")")
    (hash-set! obj:names key name))
  (define (obj:name-lookup key)
    (hash-ref obj:names key "no-name"))


;;--------------------------------------------------------------------------------
;; exceptions
;;
;;   sometimes we are faced with doing something for condition that can't happen due to
;;   the logic of the code, in these cases we should raise an exception just in case their
;;   is a bug
;; 
;;   We also use exceptions in when the caller breaks a contract with the library, and we
;;   don't know what else to do.  I.e. when the code simply isn't designed to deal with
;;   the case.
;;
;;   Otherwise we use continuations
;;
  (define obj:exception:no-such-object 'obj:exception:no-such-object)
  (define obj:exception:no-such-type 'obj:exception:no-such-type)
  (define obj:exception:no-such-key 'obj:exception:no-such-key)

  (define (raise:no-such-object) (raise obj:exception:no-such-object))
  (define (raise:no-such-type) (raise obj:exception:no-such-type))
  (define (raise:no-such-key) (raise obj:exception:no-such-key))

;;--------------------------------------------------------------------------------
;; elementary objects have these keys reserved:
;;
  (define key:copied-from 'field:copied-from) ; holds an object


;;--------------------------------------------------------------------------------
;;  at the bottom of the recursion we find the following:
;;
  (define obj:tman-table (make-hasheq)) ; table of all objects

  (define (obj:make)
    (let(
          [objid (unique-to-session-number)]
          [a-type-manifold (make-hasheqv)]
          )
      (hash-set! obj:tman-table objid a-type-manifold)
      objid
      ))

  ;; only function that allows an illegal objid
  ;; a spurious objid could alias against a legal objid, and this test would incorrectly return #t
  ;; .. I added this to facilitate writing contracts and documentation
  ;;
    (define (obj:is objid) (x-hash-ref obj:tman-table objid is-true is-false))


  ;; if type already exists, replaces it
  (define (obj:add-type objid type)
    (let(
          [type-manifold (hash-ref obj:tman-table objid raise:no-such-object)]
          )
      (if (obj:is type)
        (hash-set! type-manifold type (make-hasheqv))
        (raise:no-such-type)
        ))
    objid
    )

  (define type-type (obj:make)) ; the type of type

  (void (when obj:debug (obj:name-hook type-type "type-type")))
  (void (hash-set! obj:tman-table type-type (make-hasheqv)))
  (void (obj:add-type type-type type-type)) ; the type-type object needs to have at least one component



;;--------------------------------------------------------------------------------
;; basic functions
;;
  
  ;; true if the given type has been added
  (define (obj:has objid type)
    (let(
          [type-manifold (hash-ref obj:tman-table objid raise:no-such-object)]
          )
      (x-hash-ref type-manifold type  (λ(e) #t) (λ() #f))
      ))

  ;;(require racket/trace)
  ;;(trace get-elementary)

  ;; for module use
  (define (get-elementary type objid continue-ok continue-no-type)
    (let*(
           [type-manifold (hash-ref obj:tman-table objid raise:no-such-object)]
           [elementary-object (hash-ref type-manifold type continue-no-type)]
           )
      (continue-ok elementary-object)
      ))

  ; imposes type on an object then returns a field value from the selected component
  (define (obj:set! type objid Λkey-val)
    (get-elementary type objid
      (λ(e) 
        (apply hash-set*! (cons e Λkey-val))
        objid
        )
      raise:no-such-type
      )
    objid
    )

  (define (obj:remove! type objid key)
    (get-elementary type objid
      (λ(elementary) (hash-remove! elementary key))
      void
      )
    objid
    )

  ;; reference object to return a value
  ;;
    (define (obj:ref type objid key continue-ok continue-no-key continue-no-type)

      (define (obj:ref-1 objid)
        (get-elementary type objid
          (λ(e-obj)
            (x-hash-ref e-obj key
              continue-ok
              (λ()
                (x-hash-ref e-obj key:copied-from
                  (λ(original) (obj:ref-1 original))
                  continue-no-key
                  ))))
          continue-no-type
          ))

      (obj:ref-1 objid)
      )
  
  (define (obj:ref* type objid key)
    (obj:ref type objid key identity raise:no-such-key raise:no-such-type)
    )

  ;; reference object to find a value, that value is a function, applies the 
  ;; function to the arguments.  args is a list.
  ;;
    (define (obj:apply type objid key Λargs continue-ok continue-no-key continue-no-type)
      (obj:ref type objid key
        (λ(f)(continue-ok (apply f Λargs)))
        continue-no-key
        continue-no-type
        ))

    (define (obj:apply* type objid key Λargs)
      (obj:apply type objid key Λargs identity raise:no-such-key raise:no-such-type))

  (define (obj:copy-element type source-objid target-objid)
    (obj:add-type target-objid type)
    (obj:set! type target-objid (Λ key:copied-from source-objid))
    )

  ;; returns a set of types
  (define (obj:element-types objid)
    (let(
          [type-manifold (hash-ref obj:tman-table objid raise:no-such-object)]
          )
      (apply mutable-seteqv (hash-keys type-manifold))
      ))

  (define (list-element-types objid)
    (let(
          [type-manifold (hash-ref obj:tman-table objid raise:no-such-object)]
          )
      (hash-keys type-manifold)
     ))

  (define (obj:copy source-objid target-objid)
    (for (
           [type (list-element-types source-objid)]
           )
      (obj:copy-element type source-objid target-objid)
      )
    target-objid
    )
    
  (define (obj:keys type objid)
    (get-elementary type objid
      (λ(e)
        (let*(
               [keys-list (hash-keys e)]
               [keys (apply mutable-seteqv keys-list)]
              )
          (x-hash-ref e key:copied-from
            (λ(source) (set-union! keys (obj:keys type source)))
            (λ() keys)
            )))
      (λ() (mutable-seteqv)) ; returns an empty set
      ))
  
   (define (obj:has-key type objid key)
     (get-elementary type objid
       (λ(e)
         (or
           (hash-has-key? e key)
           (x-hash-ref e key:copied-from
             (λ(source) (obj:has-key type source key))
             is-false
             )))
       is-false
       ))
    


  ;;--------------------------------------------------------------------------------
;;  tests
;;

  (define (obj-test-0)
    (let(
          [summable (obj:make)]
          [obj1 (obj:make)]
          )

      (obj:add-type obj1 summable)
      (obj:set! summable obj1 (Λ 'x 5))
      (obj:ref summable obj1 'x
        (λ(x) (= x 5))
        (λ() (raise 'exception:obj-test-0-1))
        (λ() (raise 'exception:obj-test-0-2))
        )))
  (test-hook obj-test-0)      


  (define (obj-test-1)
    (let(
          [summable (obj:make)]
          [obj1 (obj:make)]
          [obj2 (obj:make)]
          )
      (obj:add-type summable type-type)

      (obj:set! type-type summable
        (Λ
          '+ (λ(a b)
               (obj:ref summable a 'x
                 (λ(ax)
                   (obj:ref summable b 'x
                     (λ(bx) (+ ax bx))
                     (λ() (raise 'exception:obj-test-0-1))
                     (λ() (raise 'exception:obj-test-0-2))
                     ))
                 (λ() (raise 'exception:obj-test-0-3))
                 (λ() (raise 'exception:obj-test-0-4))
                 )))
        )

      (obj:add-type obj1 summable)
      (obj:add-type obj2 summable)

      (obj:set! summable obj1 (Λ 'x 5))
      (obj:set! summable obj2 (Λ 'x 7))

      (obj:apply type-type summable '+ (Λ obj1 obj2)
        (λ(sum) (= sum 12))
        (λ() (raise 'exception:obj-test-0-5))
        (λ() (raise 'exception:obj-test-0-6)))
      ))    
   (test-hook obj-test-1)

(define (obj-test-2)
  (let(
        [summable (obj:make)]
        [obj1 (obj:make)]
        [obj2 (obj:make)]
        [test-result #t]
        )

    (obj:add-type obj1 summable)
    (obj:add-type obj2 summable)
    
    (obj:set! summable obj2 (Λ 'x 21))
    (obj:copy obj2 obj1)

    (obj:ref summable obj1 'x
      (λ(x) (set! test-result (and test-result (= x 21))))
      (λ() (raise 'exception:obj-test-0-1))
      (λ() (raise 'exception:obj-test-0-2))
      )
    
    ;; we haven't implemented coherency so if we change the original, the lazy copy
    ;; will still be reading it.
    (obj:set! summable obj2 (Λ 'x 11))
    (obj:ref summable obj1 'x
      (λ(x) (set! test-result (and test-result (= x 11))))
      (λ() (raise 'exception:obj-test-0-1))
      (λ() (raise 'exception:obj-test-0-2))
      )

    ;; writing the copy will stop the reading from the original
    (obj:set! summable obj1 (Λ 'x 8))
    (obj:ref summable obj1 'x
      (λ(x) (set! test-result (and test-result (= x 8))))
      (λ() (raise 'exception:obj-test-0-1))
      (λ() (raise 'exception:obj-test-0-2))
      )

    test-result
    ))
  (test-hook obj-test-2)


;;--------------------------------------------------------------------------------
;; provides the following
;;    
  (provide
    obj:debug
    obj:name-hook
    obj:name-lookup
    key:copied-from
    obj:exception:no-such-key
    obj:exception:no-such-object
    obj:exception:no-such-type
    type-type
    )

  (provide-with-trace "object"
    obj:add-type
    obj:apply
    obj:apply*
    obj:copy
    obj:copy-element
    obj:element-types
    obj:has
    obj:has-key
    obj:is
    obj:keys
    obj:make
    obj:ref
    obj:ref*
    obj:remove!
    obj:set!
    raise:no-such-key
    raise:no-such-object
    raise:no-such-type
    )
