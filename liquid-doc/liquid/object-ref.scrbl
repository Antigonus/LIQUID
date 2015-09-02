#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "object-reference"]{Objects}

@defmodule[liquid/object]


@defproc[(obj:make) obj:is]

  Makes an empty object.  Returns the object id.  An object id is called an @racket[objid].


@defproc[(obj:is [objid obj:is]) bool?]

  This is the predicate to test if something is an object.

  This is currently not a completely reliable test, because objids can alias with non-ids.  This
  is an area for future improvement in later versions.  However we need this predicate
  for the documentation.

  All functions work on objids, so the objids are often talked about as though they are the
  objects themselves.  


@defproc[(obj:add-type [objid obj:is] [type obj:is]) obj:is]

  Fields within composite objects occur within type spaces.  This adds a new type space to the
  the object.  It returns the object it worked on.

@defproc[(obj:has [objid obj:is] [type obj:is]) bool?]

  True if 'objid' has a type space 'type', otherwise false.  Note that object might or might not
  also have other type spaces.

@defproc[(obj:set! [type obj:is] [objid obj:is] [Λfield-symbol (listof [field symbol?] [value any/c])]) obj:is]

  Takes a list that has field names followed by values and sets those fields to those values
  inside of the given objid.  Returns the objid.

@defproc[(obj:remove! [type obj:is] [objid obj:is] [field symbol?]) obj:is]

   Removes specified field from the specified type space in the given objid.  Returns
   the objid.

@defproc[(obj:ref [type obj:is] [objid obj:is] [field symbol?]
     [continue-ok (-> any/c any/c)] 
     [continue-no-field (->any/c)] 
     [continue-no-type (-> any/c)]
     )  any/c]


    Accepts a type, objid, and field. If all goes well continue-ok is called
    with the value found in the field.

    Otherwise, if there is no such type space in objid, then continue-no-type is called
    without arguments. If there is no such field, then continue-no-field is called with no
    arguments.

    In any of these cases the tail position is returned from one of the continuations.

    Otherwise, if the objid is not found then the function raises the exception
    obj:exception:no-such-object.

@defproc[(obj:ref* [type obj:is] [objid obj:is] [field symbol?]) any/c]

    Same as @racket[(obj:ref type objid field identity raise:no-such-field raise:no-such-type)]

@defproc[(obj:apply  [type obj:is] [objid obj:is] [field symbol?] [Λargs (listof any/c)]
     [continue-ok (-> any/c any/c)] 
     [continue-no-field (-> 2any/c)] 
     [continue-no-type (-> any/c)]
     )  any/c]

    In conventional object oriented programming this would be a method call.

    @racket[type] will often be @racket[type-type].  (@racket[type-type] is the type
    given to most types).  @racket[objid] is expected to be a type object.  @racket[field]
    will be the function name being called.  Λargs is the list of arguments to be given
    to the function.  These will often be objects that share this type.

    If all goes well the result of applying the function to the arguments is passed to
    continue-ok.

    Otherwise, if there is no such type space in the objid, then continue-no-type is called
    without arguments. If there is no such field, then continue-no-field is called with no
    arguments.

    In any of these cases the tail position is returned from one of the continuation.

    Otherwise, if the objid is not found then the function raises the exception
    obj:exception:no-such-object.

@defproc[(obj:apply* [type obj:is] [objid obj:is] [field symbol?]  [Λargs (listof any/c)]) any/c]

    Same as @racket[(obj:apply type objid field Λargs identity raise:no-such-field raise:no-such-type)]

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


