

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "object-concepts"]{TCA Objects}

The definitions given here come from the TCA object implementation, and vary somewhat from
conventional use of the terms.


@section{TCA Objects}

'Data' is a vector of bits with hardware native definition.  A 'value' is data used by a
program.  An 'argument' is a value passed to a funciton.  An 'operand' is a value
passed to an operator. 

A name is data that is used to lookup data in a table.  A 'symbol' is text that is
typically used as a name.  A 'variable' is a name-value pair, where the name part is a 
symbol, known as the 'variable name'.

A 'primitive object' is the same as a value.  A 'structured' object is a container that
has multiple 'fields' where each field may hold a value.  Each field has a name.  If no
further structure than this is found in an object, then the object is said to be
'elementary'.

A 'compound' object is one that has the additional structure of a 'type manifold'.  A type
manifold has a number of branches.  At the end of each branch one finds an
elementary object. An example implementation of a type manifold is a hash table, where the
keys are type names and the values are elementary objects.  We call it a 'manifold' rather
than a tree, because each branch holds a space with a number of variables in it.  The
variable names are the field names, and the variable values are the field values.

Compound objects can act according to design patterns known in other object models, such
as the patterns of inheritance, multiple inheritance, and polymorphism.


@section{Format}

The list of all possible fields for an elementary object is known as the object's
'format'.  With C objects format is rigid and defined at compile time.  In this TCA
implementation format may be dynamic in that we may add or remove fields at run time.


@section{Type}

'Type' is simply a shared object.  Typically the fields within type objects hold programs.

A 'type binding' is a contract with a programmer, compiler, or interpreter, that says that
programs that operate on a certain set of objects always come from a specified shared
object, AKA, the 'type'.  

When multiple objects may share a type, these objects are said to be 'of the type'.
Compound objects may easily be of more than one type.  The elementary object corresponding
to each type is simply added to the type manifold under the corresponding type name.


@section{Compound Objects}

In order to have modern properties of type such as inheritance or polymorphism, we
allow that an object or group of objects may share more than one type.

When an object is of more than one type, then the functions contained in those multiple
types will be looking for fields with specific names related to each type.  This could
be a problem if two field names intended to have different semantics for each type have
the same name.

Perhaps the best solution to this problem is to make 'field name' a managed resource.
Accordingly there would be a function called 'make-field'  that would return a field.
Human readible field names would be lookedup in a dictionary against the field, and
two different fields could indeed have the same name, yet they would remain
unique.

We did not do this in this version.  Instead we gave objects internal field spaces based
on type names (and type names in this context are objids).  The space of available fields
is then looked up against the name of the type that wants to operate on it.  Thus field
names remain as symbols in the program.  We call such typed spaced objects 'compound
objecs'.  All objects in the current implementation are compound.


@section{Type Application}

There is a process for applying type to an object.  In this implementation of the TCA
objects it goes like this, say two data objects share a type object, and the type object
holds a binary operator.  To apply this operator, we first have to look it up within the
type object.  We know which type object to use because of the binding. After looking up
the operator we will have a lambda function. Then we apply this lamba to the operand
objects.

As a consequence of this process the programmer may notice that the functions in a type
object must be written with a-priori knowledge of the format of the objects they will
operate on.  This is why there is a correspondence between the names of the branches in
the type manifold and the name of the type object.  We can interpret the name of a
manifold branch of type X, having name X, as "The object on this branch was written to be
manipulated by the programs found in the X type object."



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
compound objects with the new type space, and treats them accordingly.  In places where
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

