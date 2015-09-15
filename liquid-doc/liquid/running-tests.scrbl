#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib-tests" #:style '(toc)]{Running Tests}

  A test program is one that accepts no arguments and then throws exceptions or returns any single value.
  However, it is only considered to have passed if it does not throw any exceptions and returns
  only Boolean true, i.e. @racket[(eqv? #t return-value)].  A perfect test only returns when the thing
  it tests really works, though, unfortunately, many tests are not perfect.

  With our test library a programmer can 'hook' a test into a global list.  Later all tests can be
  run with @racket[(test-all)].  @racket[(test-all)] returns true if all the constituent tests return
  true.

  Tests for library routines are scattered throughout the code.  They follow the naming convention of
  'test-X-N'  where X is the name of the thing being tested, and N is the Nth test.  The library
  tests are hooked into the test pool when the module containing the test is loaded.  Hence, running
  @racket[(test-all)] after loading one or more modules will run all the tests for those modules, and
  then return Boolean true if and only if all the tests have passed.

  Many of the db-lib and dataplex tests will fail if they do not have access to a postgress database.



     
