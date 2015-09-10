#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:style '(toc)]{Multiple Continuations Architecture}

@section[The Basic Approach]

As a matter of convention, we simplify function calls by not using variable arguments, but
instead where variable arguments are required we use explicit lists.  We make this more
convenient by introducing the Unicode capital lambda, Λ, as a synonym for @racket[list].
In Emacs I have added both λ and Λ to my key map as alt-l  and alt-shift-l.

Our basic approach is to pass continuation functions as arguments.  By convention the
first function is the 'normal flow'.  By convention we call the first function
continuation-ok.  For example, this is the definition for an extended hash ref that takes
continuation functions.  It is implemented as a wrapper.  Of course it would be better to
have a native version.

@codeblock|{
  (define (x-hash-ref table field continue-ok continue-key-not-found)
    (define ok #t)
    (define result (hash-ref table field (λ() (set! ok #f))))
    (cond
      [ok (continue-ok result)]
      [else (continue-key-not-found)]
      ))
}|

Using x-hash-ref might appear something like:

@codeblock|{
  ...
  (define a 7)
  (x-hash-ref table a
    (λ(v) (display "found: ")(displayln v))
    (λ()  (displayln "did not find 7 :-(")
    )
  ...
  }|

The code that follows the function becomes a continuation for the final function
continuation.  The tail position from the final function continuation will be the
ultimate value for the function.

@section[Reduction to Conventional Functions]

In the library we provide some utility functions that can be used to stub of continuations
and to get conventional behavior.  As example, the function @racket[identity] can be used
for @racket[continue-ok] to force the function to immediately return the result.  The
function @racket[raise]  can be used to cause alternatives to return exceptions.  Typically
if a library function can be reverse curried against  @racket[ identity  raise raise ...]
to reduce it to conventional form without continuations.


@section[Why Multiple Continuations are Needed]

The function @racket[hash-ref] makes for an interesting example, because it already accepts
a continuation function, but only for the case that the key is not found.  This is problematic
as this means the tail position of the not found lambda must flow into the continuation for
the code where the value is found.  This might be useful if there is a default value for
hash key searches, but is a problem if the continuation should do its independent work on a separate
branch of execution.  @racket[x-hash-ref] provides two independent branches of execution.

@section[Option for Passing Continuation Function Arguments]

The above code example was indeed our first implementation for @racket[x-hash-ref],
however we ran into a problem in that @racket[continue-key-not-found] is called without
arguments.  If this function is to do any real work it must operate on something.

When a application programmer defines a function that accepts continuations, then the
programmer can handle the question of which arguments should be passed into the
continuation calls.  After all, he also wrote the continuation functions.

When a library programmer defines a function that accepts continutations to handle results
and end cases, such as in the @racket[x-hash-ref] function above, he can not know in
advance the arguments for the continuation function.  Rather he must dictate these back to
the programmer as part of the definiiton of the library function.  However, such dictations
could easily create artificial constraints.

Continuations when invoked in a library routine can only expect to see the arguments to
the function call and any implementation invariant values that are instrinsic to the
library function's behavior.  For example @racket[x-hash-ref] these values include the
table, the key, the value found when one is found, and the other continuation arguments.

In addition there is an expectation that continuations are for handling results and end
cases, where such end cases would convnetionally be exceptions.  They are not intended to
be part of the functionality for the library function itself.  For partitioning
functionality the library function should call helper functions and other library
functions.  These functions may indeed require values specific to the library function
impelmentation.

I've contemplated some alternatives to passing additional information to continuation
functions. 

One approach was to define a new syntax object to define funcitons, one where
continuations are given expliticly, the outer function call argument would then be passed
to continuations in a keyword argument.  However, this approach made defining
continuations a bit messy and confusing as all continuations given to library functions
would have to define keywords for argumetns they typically do not use.

In an alternative approach, continuations are provided to the function defintion syntax
as pairs. The first of the pair being be the continuation function as before, and the
second of the pair being a quoted list of arguments.  Then at the point the continuation
is invoked it would be applied to the arguments.  The user of the library function could
then state exactly what he wants passed to the continuation within the scope of the
continuation call. 

The advantage with this alternative is that the creator of the continuation function can
specify what arguments are needed.  Disadvantages include that the continue may specify
implementation specific variables,  should the continuation be passed to a helper function,
the cnotinue can not refer to arguments to the outer call as the context will have changed,
and thirdly it is a bit tedious to have to yet again provide argument declaration list every time
a continuation is passed as an argument.   We could make agreeement with the programmer not
to look at implementation state, or face a maintenance issue.  We could constrain library code
to make sure that arguments are always visable.  This could be a viable approach.

As a third option the lbirary routine could pack up everything that could possibly
relevent to a continuation function in a list, or hash table, and then pass it as the
first argument.  For the most common case of a the simple continuation function
definition, the first argument would then be called something like 'state', and simply not
used.  Hence the overhead for supporting uncommon complex responses remains low for the
continuation.  However, it is fairly high for the library functions, which must pack up
and pass all the potentially relevant state.

@section[The Function-Args Pair Option]

We have opted for the pair approach.  We handle the contract of maintaining context to
arguments to the main funciton which might be refernced in a continuation, by curring all
continuation functions against these arguments.  We then pass the curried version to the
helper (if indeed such continue function passing occurs at all).  We also allow that if a
continuation is provided without an argument list in a pair, that the library may chose
the arguments. These default choice is specified in the docs, but may change between
versions.  Thus in typical cases we do not have to specify continuation arguments.  All
this requires some work so I have created a customer function definition form to handle
it.

Accordingly our @racket[x-hash-ref] call example becomes:

@codeblock|{
  ...
  (define a 7)
  (x-hash-ref (Λ table a)
    (Λ
      (λ(v) (display "found") (displayln v)) . '(result)
      (λ(k) (display "no such key: ") (displayln k)) . '(key)
    ))
  ...
  }|

The first list passed to x-hashref is the argument list.  The second one is the continuations
list.  This will be true in general for functions defined with continuations using the
syntax form @racket[dfc].

The first pair in the continuations list has a @racket[cdr] of @racket['(result)], but it
was not necessary to provide this because @racket[continue-ok] is called with the lookup
result by default.  I.e. I could have written:

  @codeblock|{
    ...
    (define a 7)
    (x-hash-ref (Λ table a)
      (Λ
        (λ(v) (display "found") (displayln v))
        (λ(k) (display "no such key: ") (displayln k)) . '(key)
      ))
    ...
    }|


I find it intersting that the use of specifiers for the continuation argument forces the
library programmer to provide names for its return values.  In this case the return value
given to the first continuation is called @racket[result] while the return value given to
the second continuation is @racket[key].

