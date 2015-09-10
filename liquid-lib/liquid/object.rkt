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
  feature, we reserve the field 'copied-from'  in all elemetnary objects.  The value for 
  this field is then the objid for the object being copied from.  If the elementary object
  has no such field entry, then the object is not being copied from another.
  
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
  (define (obj:name-hook field name)
    (display "(")(display field) (display ".")(display name)(displayln ")")
    (hash-set! obj:names field name))
  (define (obj:name-lookup field)
    (hash-ref obj:names field "no-name"))


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
  (define obj:exception:no-such-field 'obj:exception:no-such-field)
  (define obj:exception:broken 'obj:exception:broken)

  (define (raise:no-such-object obj-space bad-obj . continue-ok)
    (display "not a valid objid: ")
    (display bad-obj)
    (newline)
    (raise obj:exception:no-such-object))

  (define (raise:type-does-not-exist obj-space bad-type . continue-ok)
    (display "type is not a valid objid: ")
    (display bad-type)
    (newline)
    (raise obj:exception:type-does-not-exist))

  (define (raise:object-no-such-type obj-space objid bad-type . continue-ok)
    (display "for objid: ")
    (display objid)
    (display ", object has no such type: ")
    (display bad-type)
    (newline)
    (raise obj:exception:no-such-type))

  (define (raise:no-such-field obj-space objid type bad-field . continue-ok)
    (display "object has no such field: ")
    (display bad-field)
    (newline)
    (raise obj:exception:no-such-field))

  (define (raise:broken) ;; for when there is just no way this should be called
    (display "this code should not have been reached, the object system has a bug.")
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

  (define (obj:make)
    (let(
          [objid (unique-to-session-number)]
          [a-type-manifold (make-hasheqv)]
          )
      (hash-set! obj:tman-table objid a-type-manifold)
      objid
      ))

  (define (obj:->string objid [conv-fun display])
    (x-hash-ref obj:tman-table objid
      (λ(e)(->string e conv-fun))
      (be null)
      ))


  ;; only function that allows an illegal objid
  ;; a spurious objid could alias against a legal objid, and this test would incorrectly return #t
  ;; .. I added this to facilitate writing contracts and documentation
  ;;
    (define (obj:is objid) (x-hash-ref obj:tman-table objid (be true) (be false)))

  ;; if the type space already exists, its contents are clobbered
  ;;
    (define (obj:add-type objid type)

      ;; type is a shared object, thus it is an object, thus obj:is must be true
      (when (not (obj:is type)) (raise:type-does-not-exist tman-table type-objid))

      (x-hash-ref obj:tman-table objid
        (λ(tman) (hash-set! tman type (make-hasheqv)))
        raise:no-such-object
        )

      objid
      )

  (define type-type (obj:make)) ; the type of type

;;--------------------------------------------------------------------------------
;; In this implementation all objects have type, and type is an object, so then what is it's type?
;; Any shared object can be the type of type, and that shared object will carry operator for working
;; the objects that share it.  We provide one such object to be shared by type, called 'type-type'.
;; type-type is its own type.
;;
  (void (when obj:debug (obj:name-hook type-type "type-type")))
  (void (hash-set! obj:tman-table type-type (make-hasheqv)))
  (void (obj:add-type type-type type-type)) ; the type-type object needs to have at least one component


;;--------------------------------------------------------------------------------
;; basic functions
;;
  
  ;; true if the given type has been added
  (define (obj:has objid type)

    (when (not (obj:is type)) (raise:no-such-type tman-table objid type))

    (x-hash-ref obj:tman-table objid
      (λ(tman)
        (x-hash-ref tman type
          (be true)
          (be false)
          ))
      raise:no-such-object
      ))

  ;;(require racket/trace)
  ;;(trace get-elementary)

  ;; for module use
  (define (get-elementary objid type continue-ok continue-no-type)
    (x-hash-ref obj:tman-table objid
      (λ(tman)
        (x-hash-ref tman type
          continue-ok
          continue-no-type
          ))
      raise:no-such-obj
      ))

  ; imposes type on an object then returns a field value from the selected component
  (define (obj:set! type objid Λfield-val)
    (get-elementary objid type
      (λ(e) 
        (apply hash-set*! (cons e Λfield-val))
        objid
        )
      raise:no-such-type
      )
    objid
    )

  (define (obj:remove! type objid field)
    (get-elementary objid type
      (λ(elementary) (hash-remove! elementary field))
      void
      )
    objid
    )

  ;; reference object to return a value
  ;;
    (define (obj:ref type objid field continue-ok continue-no-field continue-no-type)

      (define (obj:ref-1 objid)
        (get-elementary objid type
          (λ(e-obj)
            (x-hash-ref e-obj field
              continue-ok
              (λ args
                (x-hash-ref e-obj field:copied-from
                  (λ(original) (obj:ref-1 original))
                  continue-no-field
                  ))))
          continue-no-type
          ))

      (obj:ref-1 objid)
      )
  
  (define (obj:ref* type objid field)
    (obj:ref type objid field identity raise:no-such-field raise:no-such-type)
    )

  ;; in conventional object oriented programming, this would be a 'method call' where the
  ;; field is the name of the method.
  ;;
  ;; reference object to find a value, that value is a function, applies the 
  ;; function to the arguments.  args is a list.
  ;;
  ;; the result of the function is returned
  ;;
    (define (obj:apply type objid field Λargs continue-ok continue-no-field continue-no-type)
      (obj:ref type objid field
        (λ(f)(continue-ok (apply f Λargs)))
        continue-no-field
        continue-no-type
        ))

    (define (obj:apply* type objid field Λargs)
      (obj:apply type objid field Λargs identity raise:no-such-field raise:no-such-type))


  (define (obj:copy-element type source-objid target-objid)
    (obj:add-type target-objid type)
    (obj:set! type target-objid (Λ field:copied-from source-objid))
    )


  (define (list-types objid)
    (x-hash-ref obj:tman-table objid
      (λ(tman) (hash-keys tman))
      raise:no-such-object
      ))

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
    (get-elementary objid type
      (λ(e)
        (let*(
               [fields-list (hash-keys e)]
               [fields (apply mutable-seteqv fields-list)]
              )
          (x-hash-ref e field:copied-from
            (λ(source) (set-union! fields (obj:fields type source)))
            (λ args fields)
            )))
      (λ args (mutable-seteqv)) ; returns an empty set
      ))
  
   (define (obj:has-field type objid field)
     (get-elementary objid type
       (λ(e)
         (or
           (hash-has-key? e field)
           (x-hash-ref e field:copied-from
             (λ(source) (obj:has-field type source field))
             (be false)
             )))
       (be false)
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
    field:copied-from
    obj:exception:no-such-field
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
    obj:has-field
    obj:is
    obj:fields
    obj:make
    obj:ref
    obj:ref*
    obj:remove!
    obj:set!
    obj:->string
    raise:no-such-field
    raise:no-such-object
    raise:no-such-type
    )
