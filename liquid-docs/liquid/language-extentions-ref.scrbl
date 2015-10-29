#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "lang-reference"]{Language Extentions Reference}




@section{arith}
  @defmodule[liquid/arith]

  @defproc[(≠ [n number?] ...) bool?]

  Is just the @racket[not] of @racket[=] applied to the same args.

  @defform[(≥)]

  >=

  @defform[(≤)]

  <=

  @defproc[(++ [n number?]) number?]

    Returns @racket[(+ n 1)]

  @defproc[(-- [n number?]) number?]

    Returns @racket[(- n 1)]

  @defproc[(++! [n number?]) number?]

    Returns @racket[(begin (set! n (+ n 1)) n) number?]

  @defproc[(--! [n number?]) number?]

    Returns @racket[(begin (set! n (- n 1)) n) number?]

  @defproc[(and-form [s any/c] ...) bool?]

    A form for @racket[and] that may be used with apply.

  @defproc[(or-form [s any/c] ...) bool?]

    A form for @racket[or] that may be used with apply.


@section{extentions}
  @defmodule[liquid/extentions]

@defproc[(do-nothing [arg any/c] ...) void?]

  Ignores its arguments, does nothing, and returns nothing.  It can be used to terminte
  an execution path or as a means of viewing variables with trace.

@defform[(identity)]

   This is an alias for @racket[values].  One might notice that values just returns whatever it is given.
   Using this makes it clearly that an identity function is being invoked.

@defproc[(boolify [b any/c]) bool?]

   Returns either #t  or #f.

@defform[(no-error)]

  This is an alias for @racket[not].  This is useful for alleviating confusion when the
  return value @racket[#f] means there is no error.
  
@defproc[(be [arg any/c] ...) procedure?]

  Generates a constant function. The constant returned is the argument passed in to the generator.
  So for example, @racket[(be 77)]  returns a function, and every time that function is invoked, with
  any/c number of arguments, it returns 77.

@defform[(mc:define [fun-name symbol?] ([arg any/c] ...) ([cont procedure?] ...) [body any/c] ...)]

  Defines a function that has multiple execution path exists.  Each execution path exit
  has a continuation function.  The return value is in the tail position of the
  continuation that lies upon the execution path the function choses.

  The arg list and contination list must be present, though may be empty.

  The number of continuations and the prototype for each continuation is specified as part
  of the function definition, just as are the types of other argumetns.  Continuation
  prototypes may refer to the argument list or the continuation list as a whole. So, for
  example, @racket[(a-continuation [al is-argument-list] [cl is-continuation-list]) bool?]
  specifies a continuation that accepts the calling function's argument list and
  continuation list and returns a bool value.
  
