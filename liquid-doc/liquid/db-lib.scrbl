#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib"]{Liquid db-lib}

   A constrained interface for the racket db library.

@defmodule[liquid/db-lib]{The @racketmodname[liquid/db-lib] library provides tools
for creating, deleting, and using tables in a database.}

  Although @racketmodname[liquid/db-lib] is available for general use, we do not expect
  users of liquid to call these functions.  Instead users
  will access the database through @racketmodname[liquid/databplex].

  Currently @racketmodname[liquid/db-lib] is initialized against a single database
  connection.  In the next version I plan to provide a connection context through a
  'with-connection' block.  In either case no connection parameter appears on the database
  manipulation calls.

  All table columns are type text.  Text is the only readily available
  arbitrary length SQL object. Even the SQL bignum is a fixed sized object.  Type
  conversion then occurs on the client side. Some of the provided functions accept as
  arguments filter functions, and these may also perform type converstion.

  The @racketmodname[liquid/db-lib] is intended to be thread safe but testing of this
  aspect has been limited, and there is a known bug in nesting transaction blocks due to
  how thread semaphores are used.

@section{Function Naming Convention}

  Function names are prefixed according to the domain in which the function is working.  For
  example, functions that work on the database as an object are prefixed 'db:',  while those
  that work on table as objects are prefixed 'table:'.  This approach alleviated a great deal of
  confusion and some name collisions that existed before the convention was adopted.

@section{Allocators and Namespaces}

  An allocator is an object for creating unique numbers or unique names.  A programmer may
  use the @racketmodname[liquid/db-lib] to create any number of such objects.  Each allocator has a string bound
  to it known as its name.  We also call allocators 'namespaces'.  Allocators are
  persistent (held in the database).

  The allocator service may be used for any purpose the programmer finds useful. As one
  example, the allocator has been used to create unique names and numbers when testing the
  @racketmodname[liquid/db-lib].

  The @racketmodname[liquid/db-lib] creates a namespace with the same name as each table that is created.  It then
  uses this namespace to assign a unique number to each and every row.  The first column
  of every table holds such a row id.

  Numbers in namespace may be deallocated.  Currently we do not recycle deallocated numbers,
  but this may change in later versions.

  See lynch-lib's (unique-to-session-number) and (unique-to-session-name) for unique values within a
  session. Note, unique numbers to the session may not be unique to the database.

  The @racketmodname[liquid/db-lib] reserves the namespace 'unique_to_db'.

@subsection{namespace:alloc-number}

      @defproc[
        (namespace:alloc-number [namespace string?]) number?
        ]{
        Accepts a namespace and returns a unique number within that namespace.  See also db:create-namespace.
        }

@subsection{namespace:dealloc-number}

        (namespace:dealloc-number namespace id)


@section{The sql: Functions}

These are wrappers for the underlying db library.  They only difference is that they act on the
default connection. The * versions take lists, the others take variable parameters lists.  These are intended
to be used only as work workarounds.  In a later version they may go away.

      sql:exec
      sql:exec*
      sql:value
      sql:value*
      sql:maybe-value
      sql:maybe-value*
      sql:row
      sql:row*
      sql:rows
      sql:rows*
      sql:list
      sql:list*

      transaction:begin
      transaction:commit
      transaction:rollback

@section{as-transaction}

        (as-transaction body ...)

        This provides a scoped context for a transaction block.

        (There is a current a bug affecting nesting of transactions blocks having to do with
        locking out other threads on the same connection ... priority issue, may hang when
        nested)


@section{db}

@subsection{db:tables}

      (db:tables [rx #f])

      Accepts a regular expression. Returns a list of matching tables names.

@subsection{db:is-table}

        (db:is-table table-name)

        Accepts a table name, returns a Boole.

@subsection{db:table db:table*}

        (db:delete-table . ts)  (db:delete-table* ts)

        Deletes the named tables.  The table names are literal.

@subsection{db:create-table}

        (db:create-table name column-count)

        Accepts a name and a column count, then creates a table in the database with that name
        and number of columns, or throws an exception.   Returns nothing.

        An exception will occur if the name already
        exists as a table in the database.  

@subsection{db:delete-table db:delete-table*}
@subsection{etc.}
    db:alloc-number
    db:alloc-name

    db:create-namespace
    db:delete-namespace
    db:is-namespace

@section{table}
@subsection{table:insert table:insert*}

    (table:insert name . rows) 
    (table:insert* name rows) 


@subsection{table:delete}

        (table:delete table-name a-pattern)

    Accepts a table name and a pattern, then deletes all rows from the table that
    match the pattern.

    If the table has more than one column, pattern must be a list.  Currently pattern
    is either an underscore or a literal.  An underscore matches anything.

@subsection{table:match}

    (table:match table-name pattern [a-filter identity])

    Accepts a table name, a pattern, and optionally a filter function.  Returns all
    rows in the table that match the pattern.  If the filter is present then the filter
    is mapped over the rows to create the return value.






