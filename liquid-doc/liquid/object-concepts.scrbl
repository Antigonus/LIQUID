

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "object-concepts"]{TCA Objects}

The definitions given here come from the TCA object implementation, and vary somewhat from
conventional use of the terms.


@section{Objects}

'Data' is a vector of bits with hardware native definition.  A value is data used by a
program.

An object is either 'primitive' and is the same as a value, or it is structured and can
hold a number of named values.  For structured objects the named values are said to live
in 'fields' within the object.  Hence, the names can also be called 'field names'.

This is an abstract definition for which a number of implementations are possible.  In the
C language, objects are called 'data structures' and are implemented using segments of
memory.  In our implementation here, we use hash tables.

Hash tables do not have native support on processors, so we may say our objects
"higher level" than those found C.  In the C implementation all possible fields for an
object are always present.  If constraints allow it, in our higher level implementation
it is possible that a field is not present, and new fields can be created on the fly at
run time.

@section{Format}

The list of all possible fields for an object is known as the object's 'format'.  With C
objects format is rigid and defined at compile time.  In our higher order implementation
format may be dynamic, limited only be external constraints.


@section{Type}

'Type' is simply a shared object.  Typically the fields within type objects hold programs.

A 'type binding' is a contract with a programmer, compiler, or interpreter, that says that
programs that operate on a certain set of objects always come from a specified shared
object, aka, the 'type'.  Before object oriented programming, it was conventional that all
objects would specify an association with exactly one type.

When multiple objects may share a type, these objects are said to be 'of the type'.  It
is possible for an object to be of more than one type.


@section{Type and Format}

There is a type step process for using type.  Say two data objects share a type object,
and the type object holds a binary operator.  To employ this operator, we first have to
look it up within the type object.  This lookup will be by field name.  Hence the fields
in a type object often have names that look like the names of functions.  After looking up
the field we will have the operator, so, then as a second step, we then apply the operator
to the two data objects.

Hence type functions are written with a-priori knowledge of the format of the objects they
will operate on, i.e. shared over.  The sharing of type objects among data objects is not
haphazard.


@section{Composite Objects}

In order to have modern properties of type such as inheritance or polymorphism, we hae to
allow that an object or group of objects may share more than one type.

When an object is of more than one type, then the functions contained in those multiple
types will be looking for fields with specific names related to each type.  This could
be a problem if two field names intended to have different semantics for each type have
the same name.

Perhaps the best solution to this problem is to make 'field name' a managed resource.
Accordingly there would be a function called 'make-field'  that would return a field.
Human readible field names would be lookedup in a dictionary against the field.  Hence
two different fields could indeed have the same name, yet for the code they would remain
unique.

We did not do this.  Instead we gave objects internal field spaces based on type ids.  The
field space is looked up based on the type operating on the object, and then the function
within that object picks among the fields available for that type.  Thus field names
remain as symbols in the program.  We call such typed spaced objects 'composite objecs'.
All objects in the current implementation are composite.

@section{Object Spaces}

We place all objects in hash table and then pass around the object ids instead
of the objects.  Currrently our implementaiton has only one global object space.


@section{Achieving The Things What We Get From Inheritance and Polymophrism}

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


For #1, as we have higher level objects, extra fields can just be added as required.
There is no need for an inheritance feature for adding fields.  If a new type is needed
for making use of new fields, then the new type is created as a distinct type, and applied
when it is needed.  The new type may look for the old type's type space within the
object, or or can have its own type space where the new fields are found.  

#2 here, personality, is just polymorphism by another name.  There are two chief methods
for getting this effect.  The first is to replace the type entry in the global object
space table, with a more sophisticated type. Hence, same object id, but different type
object.  The new type object will treat regular objects the same as before, and new
objects, that have fields in the new type space, differently.  Legacy code does not have
to be changed.

The second method for achieving personality is to define the new type that recognizes
composite objects with the new type space, and treats them accordingly.  In places where
polymorophism is desired the new type is used, in places it is not, the old type can be
used.  Typically, as the type carries the code, there is no need to update the legacy code
in the new type, because it isn't there anyway.

#3, introspective typing is just another form of polymorphism, and we already covered
 that.


@section{Multi-world Views}

This implements a small part of the multi-world transactional model from TCA.

In effect here, objects lazy copy.  We only create a field locally when the local
object field is written.  Otherwise, reads go back to the original object.
 
If the original object is to be changed, then a copy should be made before the change.

