#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{Examples}

If you have trouble getting the database stuff running, be sure there is a
postgresql database server with a database named, "db-lib-test", and any other databases
you use.  There is currently no way to create a new database.  Typically on uses 'psql'
for this.  Database login is set to be by Unix login credentials.  The username comes from
(getenv "USER").  If you modify the library code, perhaps to get the database login
working, run 'racket setup' afterward.

There are currently 60 some functioning examples spread through the code in the form of
tests.  To find these grep any module for the symbol 'test-hook', or grep for
'<function-name>-test'.  Tests names always have a prefix of the function being tested
with a '-test' follwed by a '-<number>'.  However, some tests will cover more than one
function.  It is intended to move some of these examples to the documentation pages after
the code stabalizes.

There is an elaborate example in the liquid-example directory.  This example makes use
of all parts of the liquid library, including the web server, the parser, and the database
libraries.  It is in fact multiple examples, as the examples are driven from the various
web pages.  You can see which web pages are available by using grep for 'page-hook'.


@table-of-contents[]




