#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "lang-reference"]{Language Extensions Reference}

  @defmodule[liquid/sequence-lib]

@defform[(Λ [item any] ...) sequence?)

  Creates a sequence of the items.  The implementation to be used for the sequence is passed
  in via a keyword argument (though this is not coded yet,so you get a list ;-)

  If an item is preceded by a comma, then the item following the comma is considered to be
  a sequence, and the items in this sequence are included rather than including the sequence
  itself.

  Here are some examples:

  @verbatim|{
    racket@sequence-lib.rkt> (Λ 1 2 3)
      '(1 2 3)
    racket@sequence-lib.rkt> (Λ 1 ,'(2 3 4))
      '(1 2 3 4)
    racket@sequence-lib.rkt> (Λ ,'(7 8 9) ,'(2 3 4))
      '(7 8 9 2 3 4)
   }|

  In the first example @racket[Λ] acts just like @racket[list].  In the second example @racket[Λ] does
  the same operation as @racket[cons].  In the third example it performs @racket[append].  All of these can
  be mixed:

  @verbatim|{
    racket@sequence-lib.rkt> (define a '(2 3 4))
    racket@sequence-lib.rkt> (define b '(5 6 7))
    racket@sequence-lib.rkt> (define c '(8 9))
    racket@sequence-lib.rkt> (Λ 1 ,a ,b c 10)
      '(1 2 3 4 5 6 7 (8 9) 10)
  |}

@defproc[(be [arg any] ...) procedure?]

  Generates a constant function. The constant returned is the argument passed in to the generator.
  So for example, @racket[(be 77)]  returns a function, and every time that function is invoked, with
  any number of arguments, it returns 77.

@defform[(mc:define [fun-name symbol?] ([argument any] ...) ([continuation procedure?] ...) [body any] ...) any?]

  Defines a function that has multiple execution path exists.  Each execution path exit
  has a continuation function.  The return value is in the tail position of the
  continuation that lies upon the execution path the function chooses.

  The arg list and continuation list must be present, though may be empty.

  The number of continuations and the prototype for each continuation is specified as part
  of the function definition, just as are the types of other arguments.  Continuation
  prototypes may refer to the argument list or the continuation list as a whole. So, for
  example, @racket[(a-continuation [al is-argument-list] [cl is-continuation-list]) bool?]
  specifies a continuation that accepts the calling function's argument list and
  continuation list and returns a bool value.
  
