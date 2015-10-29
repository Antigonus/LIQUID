#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "testlib-reference"]{test-lib}

@defmodule[liquid/test-lib]

@defform[(provide-with-trace prefix function ...)]

  Provides the listed functions, and also defines two additional functions:  @racket[prefix-trace]
  and @racket[prefix-untrace].  These turn on, or off, tracing for all of the provided functions.
  So for example, to trace turn on tracing for the interface to the test-lib library use the
  invoke the function (test-lib-trace).

  The @racket[function] arguments must be of the type that (trace function) will work.  One cannot,
  as examples, provide module variables or syntax transformers with this form.

@defproc[(test-hook [a-test procedure?]) void?]

  Hooks the procedure into the list of test procedures that will be run when
  @racket[test-all] is invoked.

  The procedure will be invoked with no arguments.  It is considered to be a passing test
  if it returns Boolean true.  If the test causes an exception or returns any other value,
  it is considered to have failed.

@defproc[(test-remove [a-test procedure?]) void?]

  Removes the procedure from the hooked test procedures list.

@defproc[(test-all [a-test procedure?]) bool?]

  @racket[test-all] starts by running three built in tests.  The first of these built in
  tests will pass, the second will fail due to not returning '#t', and the third will fail
  due to an exception.  This shows the user that the test mechanism is working, and what
  pass, fail, fail with exception, and the summary message looks like.

  @racket[test-all] then removes the built in tests and runs the tests on the hook list.
  At the end it provides a summary of how many passed and failed.  Any messages printed
  out by the tests will be dumped into the running summary of pass fails.

  A test must return Boolean true to have passed, i.e. @racket[(eqv? #t return-value)]
  must be true.  Exceptions are caught and noted as failures.  If you need to tests
  exceptions then have your test handle these and return true if they are correct.

  @racket[test-all] returns either Boolean true (@racket[(eqv? #t test-all-return)]), or false.
  (@racket[(eqv? #f test-all-return)]).  It never throw and exception.  (Otherwise it is broken.)

  As a convention, do not print messages from your test routines.  Instead, if the test
  needs to tell the user something it should fail.  The user can then go back and run the
  test manually.  Tests can accept an optional argument that can be provided to turn on
  noise during manual invocation. 

  