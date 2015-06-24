#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib"]{db-lib reference}

@section{as-transaction}

        (as-transaction body ...)

        This provides a scoped context for a transaction block.

        (There is a current a bug affecting nesting of transactions blocks having to do with
        locking out other threads on the same connection ... priority issue, may hang when
        nested)

@section{column-list}

@section{db:alloc-name}

  Like db:alloc-number, but returns a unique string instead of a number.


@section{db:alloc-number}

        (db:alloc-number)

  Either, a) returns a number not ever issued by this function before against the contextual
  database, even across sessions, or b) it returns a number that was issued before by this
  function, but was deallocated, and has not been issued since.

  [Case b) can be turned off by setting xxx option, in the current implementation it is off.]

  See also db:create-keyspace.  Note that @racketmodname[liquid/db-lib] reserves the
  keyspace 'unique_to_db' to support this function, and the related function
  db:alloc-name.

@section{db:create-keyspace}


@section{db:create-table}

        (db:create-table name column-count)

        Accepts a name and a column count, then creates a table in the database with that name
        and number of columns, or throws an exception.   Returns nothing.

        An exception will occur if the name already exists in the contextual database. [This
        behavior is set to change to no exception in future versions.]

@section{db:delete-keyspace}

@section{db:delete-table}

@section{db:delete-table*}

@section{db:is-keyspace}

@section{db:is-table}

        (db:is-table table-name)

        Accepts a table name, returns a Boole.



@section{db:tables}

      (db:tables [rx #f])

      Accepts an optional regular expression. Returns a list of matching table names, or if there
      is no regular expression, returns all the table names in the database.

@section{db:table db:table*}

        (db:delete-table . ts)  (db:delete-table* ts)

        Deletes the named tables.  The table names are literal.


@section{keyspace:alloc-number}

      @defproc[
        (keyspace:alloc-number [keyspace string?]) number?
        ]{
        Accepts a keyspace and returns a unique number within that keyspace.  See also db:create-keyspace.
        }


  See lynch-lib's (unique-to-session-number) and (unique-to-session-name) for unique values within a
  session. Note, unique numbers to the session may not be unique to the database.


@section{keyspace:dealloc-number}

        (keyspace:dealloc-number keyspace id)


@section{The sql: Functions}

These are wrappers for the underlying db library.  They only difference is that they act on the
contextual connection. The * versions take lists, the others take variable parameters lists.  These are intended
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


@section{table:delete}

        (table:delete table-name a-pattern)

    Accepts a table name and a pattern, then deletes all rows from the table that
    match the pattern.

    If the table has more than one column, pattern must be a list.  Currently pattern
    is either an underscore or a literal.  An underscore matches anything.

@section{table:insert table:insert*}

    (table:insert name . rows) 
    (table:insert* name rows) 


@section{table:match}

    (table:match table-name pattern [a-filter identity])

    Accepts a table name, a pattern, and optionally a filter function.  Returns all
    rows in the table that match the pattern.  If the filter is present then the filter
    is mapped over the rows to create the return value.






