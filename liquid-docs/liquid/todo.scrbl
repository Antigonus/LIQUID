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

@section{module init code}

  Module init code is code that runs when a module is loaded.  I have found it is a bad
  idea to have anything except defines at this level.  Initialization code should instead
  be placed inside of a constructor function.  There are a couple of reasons for this,
  firstly a module is not truly static, it is more rather like a class.  It can be
  instantiated multiple times as racket bumps up and down the phases.  Secondly, when
  racket loads a module and it finds a bug in the init code, it stops in its tracks and
  wipes clean all the module stated loaded so far.  Hence all module definitions that might
  be used for debugging are gone.  You can't for example, set a trace on a module function
  and then debug the load.

  So the todo item here is to collect initialization code into init (constructor) functions.
  Then we need to add explicit calls to the constructors.

  When we do this, we can talke the phase test code out of the test module, and instead
  have @racket[test-hook] code etc, wrapped in phase checks in the relevent constructors.

@section{define-syntax function guard checks with locations}

   If a syntax generated function has an error, racket doesn't tell you the source location.
   Source location for a define-syntax function can be gotten from the stx, note we did this
   in @racket[mc:define].   We need to go back to syntax functions and add operand checks, both
   static and dynamic, and other guard code, to spit out error messages with locations for
   the cases that the library user, i.e. the programmer,  makes a mistake in using one of these
   he isn't left with a mystery to solve.

@section{test-lib provide trace intenral}

   Note dataplex.rkt.  The approach to providing a routine at the bottom that calls out
   functions to trace has not worked out, because some routines call helper routines
   inside of routines, and these are out of scope.  (This is often done in recursive
   routines to separate what state and what is invarient.)

   Not sure, but suspect we need a special type of 'define' that hooks routines into
   one or more lists.  Then we have (trace-on list).  To trace a group of functions.

@section{dataplex}

   The bridge tables really need to say which semantic relation an sm-id belongs to.
   Otherwise we need to go check all semantic relations for a match.  Similarly when
   we are given an sp-id, though it is possible to know where it came from, it would
   be better if it just embdded in it knoweldge of which shape table it belongs to.

   Wonder if all the ids can't just be put in a big table.  Also the column shapes for
   the seantic table ... are these necessary, rather just lookup the sm-id and see what
   is found.

@table-of-contents[]




