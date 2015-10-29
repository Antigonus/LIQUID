#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "object-reference"]{TCA Objects}

@defmodule[liquid/object]

  For these routines, we throw an exception if the objid does not appear as a key in the global
  tman table.  However, we probably should have made this a continuation as well. Say, for example,
  the object is not found because it needs to be fetched from a stream.  This also puts us back
  to thinking about object spaces..  looks like stuff for v2.


@defproc[(obj:make) obj:is]

  Makes an empty object.  Returns the object id.  An object id is called an
  @racket[objid].  All functions work on objids, so the objids are often talked about as
  though they are the objects themselves.


  In this implementation all TCA objects are 'compound' which means they have a type
  manifold.  The type manifold is a hash table, where the key is a type objid.  Each entry
  in this hash table is an elementary object of the specified type.  An elementary object
  is logically equivalent to a data structure.  It has fields with values in them.

@defproc[(obj:is [objid obj:is]) bool?]

  This function will be true when the given objid has the form of an objid, and it is indeed
  a legal key in the master object table.

@defproc[(obj:add-type [objid obj:is] [type obj:is]) obj:is]

  Adds a branch to the type manifold inside the object.

@defproc[(obj:has [objid obj:is] [type obj:is]) bool?]

  True if 'objid' has a branch in the type manifold of the specified 'type', otherwise false.

can't get racket to take this, even verbtim, so here it is as text:

->defproc[(obj:set! (args [type obj:is] [objid obj:is] [field-symbol (listof [field symbol?] [value any/c] ...)])  (conts [continue-ok (-> [objid obj:is] any)]  [continue-no-type (-> (listof [arg anyc/c] ...)  (listof [cont proceedure?] ...) any)]) ) obj:is]

  Takes an Λargs list where the items come in twos, hence the list will always have an
  even length.  The first of each set sequence of two items is a field name, the second is
  a value.  Then sets the given fields on the appropriate type branch to the value given.

  If a field does not exist, it is created.  If the type does not exist on this object, an exception
  is thrown.
  
  @racket[continue-ok] is given the modified object's objid.

  @racket[continue-not-type] is called if the objects does not have the specified type.

@defproc[(obj:remove! [type obj:is] [objid obj:is] [field symbol?]) obj:is]

   Removes specified field from the specified type space in the given objid.  Returns
   the objid.

   If there is no such type or field, this function does nothing.

   It returns the objid being worked on.

can't get racket to take this, even verbtim, so here it is as text:

->defproc[ (obj:ref   (listof [type obj:is] [objid obj:is] [field symbol?])  (listof [continue-ok (-> any/c any/c)]   [continue-no-field (-> (listof [arg anyc/c] ...)  (listof [cont proceedure?] ...) any)]  [continue-no-type (-> (listof [arg anyc/c] ...)  (listof [cont proceedure?] ...) any)]         )     ) any/c]

    Accepts a type, objid, and field. If all goes well continue-ok is called
    with the value found in the field.

    Otherwise, if there is no such type space in objid, then continue-no-type is called
    without arguments. If there is no such field, then continue-no-field is called with no
    arguments.

    In any of these cases the tail position is returned from one of the continuations.

    Otherwise, if the objid is not found then the function raises the exception
    obj:exception:no-such-object.

@defproc[(obj:ref* [type obj:is] [objid obj:is] [field symbol?]) any/c]

    Wraps @racket[obj:ref] so that it either returns a value or throws an exception.

can't get racket to take this, even verbtim, so here it is as text:
->defproc[(obj:apply \
     (listof [type obj:is] [method symbol?] [method-args (listof any/c)])\
     (listof \
         [continue-ok (-> any/c any/c)] \
         [continue-no-field (-> (listof [arg anyc/c] ...)  (listof [cont proceedure?] ...) any)]\
         [continue-no-type (-> (listof [arg anyc/c] ...)  (listof [cont proceedure?] ...) any)]\
     ))  any/c]

    In conventional object oriented programming, this would be a 'method call' where the
    field from the type object provides the method code or possibly method data.  Here the method
    must be a function written to operate on the provided arg-objids.

    @racket[type] is the object that holds the method function to be applied. For purposes of this
    function, the @racket[type] object must be of type @racket[type-type].

    @racket[method] is the name of a field in the @racket[type] object.  The value of this field
    is the method function that will be applied to @racket[method-args].

    If all goes well the result of applying the function to the arguments is passed to
    continue-ok.

    Otherwise, if there is no such type space in the objid, then continue-no-type is called
    without arguments. If there is no such field, then continue-no-field is called with no
    arguments.

    In any of these cases the tail position is returned from the continuation that is taken.


@defproc[(obj:apply* [type obj:is] [method symbol?]  [method-args (listof any/c)]) any/c]

   Applies the method to the method-args and returns the result, or throws an exception.

@defproc[(obj:copy-element [type obj:is] [source obj:is] [target obj:is]) obj:is]

   Lazy copies the given type space (all fields) in the source to the target.  Returns the target.

   If the target already has fields of the same name as in the source, the target keeps
   them.  Future reads of fields not in the target come from the source, if the source has
   them.  The target gets the most up to date version of such reads, even if the copy
   happened a long time ago.

   Lazy copies will cascade.

@defproc[(obj:copy [source obj:is] [target obj:is]) obj:is]

   Applies obj:copy-element on all types in the object.  I.e. copies the whole object, source
   target.  Returns the target object.


@defproc[(obj:element-types [objid obj:is]) seteqv?]

   objid is a composite object.  This returns a set of the type spaces.  Note, if we had
   used a field allocation approach, this function would require set inclusion functions to
   get the information.  However, in our multiple type space implementation it should be fast.
   Each type in the returned set is an @racket[obj:is].


@defproc[(obj:fields [type obj:is] [objid obj:is])  seteqv?]

   In composite object @racket[objid], returns the set of fields within the @racket[type]
   space.  Each field is a symbol.

@defproc[(obj:has-field [type obj:is] [objid obj:is] [field symbol?]) bool?]

   True if the given objid has a the given field within the given type space.


