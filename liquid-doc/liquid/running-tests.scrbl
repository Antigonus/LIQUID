#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib-tests" #:style '(toc)]{Running Tests}

  A test program is any program that returns #t when it passes, and never otherwise
  returns true. It is ok for a test to throw an exception as a failure.  Tests are
  included in the regression using the function @racket[(test-hook <test-name>)],
  currently found in misc-lib.rkt

  To run the tests in a module, first require the modules one wants to run
  the tests for.  Also require msic-lib.rkt.  Then run @racket[(test-all)]

  Note that when a module from the library is entered!, you will see a series of messages of the form,
  "hooking test: <testname>"  

  After the tests are hooked, the module will then test its tester.  Yes, this too must
  be checked out, as there is nothing worse than a tester that didn't really do tests.
  The tester runs three practice tests, example-test-0, example-test-1, and example-test-2.
  The example-test-0 passes,  example-test-1 fails,  example-test-2 fails due to raising an
  exception.   This is the expected output.

  Lastly after loading a module will suggest runing (test-all).  This function when
  invoked will run the real tests.  It may be that don't want to test the library every
  time you load it, so (test-all) is left to be run manually.

  
