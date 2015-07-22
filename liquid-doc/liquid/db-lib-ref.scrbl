#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib-reference"]{db-lib}

@defmodule[liquid/db-lib]

@defform[(as-transaction body ...)]

        Context for a transaction.  The transaction begins when the context
        is entered, and ends upon leaving the context.  @racket[body] can be any LISP
        form, including those which do not invoke db-lib.

        Entering a transaction will block the current connection from
        being used in other threads.  See also, @secref{with-db}.


@defproc[(db:alloc-name) string?]

  Within the database context, returns a string which this function has never returned before,
  or a string which was legally deallocated using @racket[db:dealloc-name]. 

@defproc[(db:dealloc-name [name string?]) void?]

  Currently this is ignored, but in the future deallocating a name might make it available
  for allocation once again.


@defproc[(db:alloc-number) number?]

  Within the database context, returns a number which this function has never returned before,
  or returns a number that has been deallocated.

   
@defproc[(db:dealloc-number [n number?]) void?]

  Makes @racket[number] available for allocation once again.

  In the future there will be a mode that when invoked will make deallocating a number which
  was never allocatd an error, and we will have a another function for checking if an allocation
  is valid.


@defproc[(db:create-keyspace [keyspace string?]) void?]

  Declares that the string @racket[keyspace] is a keyspace.  Once a keyspace is declared unique
  keys maybe allocated from it.

  I'm not sure what happens if one tries to declare the same keyspace twice.

@defproc[(db:create-table [name string?] [column-count number?]) void?]

        Accepts a name and a column count, then creates a table in the database given this name
        and having that number of columns, or throws an exception.   Returns nothing.

        An exception will occur if the name already exists in the contextual database. [This
        behavior should change in future versions.]

@defproc[(db:delete-keyspace [keyspace string?]) void?]

   Un-declares a keyspace.

@defproc[(db:delete-table [table-name string?] ...) void?]
@defproc[(db:delete-table* [table-names (listof string?)]) void?]

   Drops one or more tables from the database.


@defproc[(db:is-keyspace [name string?]) boolean?]

        Whether @racket[name] is a keyspace.

@defproc[(db:is-table [name string?]) boolean?]

        Whether @racket[name] is a table.


@defproc[(db:tables [rx regexp? #f]) (listof string?)]

      Lists tables in the database.  If the regular expression is omitted or set to #f then it lists
      all the tables.


@defproc[(keyspace:alloc-number [keyspace string?]) number?]

      Returns a number unique within the specified keyspace.  I.e. adds a new number to the keyspace.


@defproc[(keyspace:dealloc-number [keyspace string?] [key number?]) void?]

      Removes a number from the keyspace.


@defproc[(table:delete [table-name string?] [a-pattern pattern?]) void?]

    Deletes all rows from the table that match the pattern.

    If the table has more than one column, pattern must be a list.  Currently pattern
    elements are either an underscore or a literal.  An underscore matches anything.


@defproc[(table:insert [table-name string?] [row (listof any)] ...) void?]
@defproc[(table:insert* [table-name string?] [rows  (listof (listof any))]) void?]

    Non-string field values are converted to string using @racket[->string].

@defproc[(table:match [table-name string?] [a-pattern pattern?] [a-filter (row? . -> . row?) identity]) (listof row?)]

    Returns all rows in the table that match the pattern.  If the filter is present then each
    fetched row is sent through it before being returned.


@defform[(with-db db-name body ...)]

   Defines a database connection context.  Upon entering the context a connection is opened with the
   posgresql server and attached to the database named db-name.  Information about connecting to
   the database is found in parameter variables at the top of db-lib.rkt.

   The database connection is opened when entering the context, and closed when leaving
   it.  There can be multiple simultaneous database contexts active, even on the same
   database, either due to nesting or threading.

   Inside the context function calls to db-lib or those of the dataplex-lib, which is built on top of
   db-lib, will automatically use the specified database.

   Multiple threads of execution may occur within a database context; however, simultaneous transactions
   contexts are not allowed.   If within a database context a given transaction begins on one thread, then the
   other threads are blocked from beginning a transaction until the given transaction completes. In contrast
   transactions in different database contexts are independent. (If two simultaneous transactions on the same
   database but different connections occur, it is up to the database server to gurantee correct behavior.)
   


   
