#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{To-Do}

@section{test-lib}

A parameter so that tests can know they are being run in regression so they can turn off messages etc.

Keyword tags on tests, and pass an optional predicate to test-all to chose which tests to run.

@section{TCA objects}

Move the fields to a managed resource.  Change ids to be pairs, space, and id, so that it is
quick to determine if two are in the same space.  Change the object catalog to be a weak
hash, so when the keys are no longer being used, the object goes away.  (Hmmm, how does that
work with the keys being copied when passed in as arguments etc.?) .. don't want multiple
hash tables rather keep the space as part of the key.

@section{db-lib}

    Patterns are not fully implemented.  Currently we only support the '_' that matches anything.

    We really need to makes sure the to string conversion stuff makes for safe SQL.  I kinda suspect
    that right now it doesn't.

    Deallocation for key spaces is not implemented.  Also we need to add tests for the existence of
    stale values use (using keys that were deallocated). 

    Add a function for setting the database connection parameters so we can add some flexibility.  Right
    now we just assume postgres and unix login credentials.  Brian's db library connect function has many
    more options.

@section{extensions}

  Version of for that returns the untraversed portions of the lists.  Actually, rather have an
  iterator that can be used to access current element, prefix, or suffix of a list.  For would
  assign the iterator rather than a value, and return the for loop value and the iterators.
  We could have multiple exit paths depending on how the loop is terminated .. hmm.


@table-of-contents[]




