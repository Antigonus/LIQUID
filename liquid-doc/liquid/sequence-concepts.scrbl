#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:style '(toc)]{Sequences}

  In that a program is is a logical control structure, the basic data type for Scheme has become the sequence.
  The question of whether the sequence is implemented as a list, vector, structure, etc.  does not change
  the logic, order of time complexity, or order of space complexity, and thus the implementation choice for
  the sequence is not part of the problem of creating logical programs.

  The view is different for the performance program.  For performance within a complexity category, one must
  chose implementations.

  Thus I have introduced a sequence builder operator, @racket[Λ],  that builds a sequence from a list of members
  or other sequences, while accepting as an optional keyword parameter its implementation.

  Sequence forms such as structures and hash tables integrate indexing with containment.  Indeed even the
  sequence form itself is a type of indexing scheme over a container.  So for example,

  @verbatim|{
     (define a-list '(1 2 3))
     (match-let(
          [(list a b c) a-list]
          )
          ...
          )
   }|    

   This @racket[match-let] form gives names to the members of a three element list, and is
   logically indistinguishable from the definition of a structure that has named fields.
   So you can see that structure paradigm is just a logical combination of sequencing and
   indexing.  We could get the same behavior from a hash table.

   Hence, logically we have containers and indexes.  However, we allow that the container
   will always have a sequence binding, so we can also say that in our language we have
   have sequences and indexes.  These can be implemented in different ways.

   In the beginning LISP had only the list, hence it had built in these abstractions.  However
   as other data types have been added, the abstraction was lost.  With sequences and indexes
   we try to recover the sequence abstraction.

   Now if we combine this with multiple execution path functions we can create base functionality
   according to our sequence abstraction, and then add on top of that additional paths to handle
   additional cases.   One important case will be that of pulling more data in the case of stream
   processing.