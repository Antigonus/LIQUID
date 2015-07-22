#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib-concepts" #:style '(toc)]{db-lib}


  db-lib is built on top of Ryan Culpepper's DB lib.

  The @racketmodname[liquid/db-lib] library provides an interface for specific purpose of
  providing persistent storage of LISP data in tabular format in a database.

  db-lib was written to support the dataplex library, though it may be used by anyone.

@section{Contexts}

   New databases are created manually using psql.  Each database has a
   name.

   To use a database a user opens a @racket[(with-db db-name body..)] context.  These
   contexts may be nested.  Each new context opens a new connection. Any number of contexts
   can be opened on the same database, each will be on their own connection.

   To create a transaction block the user opens a transaction context, @racket[(as-transaction body ..)].
   The transaction begins upon entering the context, and ends upon leaving it.  Database commands
   and LISP commands may be mixed inside the transaction block.  Tansaction contexts may be nested.
   Transactions in the same database context block between threads.  This prevents the two transaction
   blocks from interleaving commands on the connection.  

   The with db-context will close without waiting for spawned threads to complete, so if
   the user needs the db connection then he or she will have to code in a thread wait
   before leaving the context.

   A user may use different transaction contexts on different threads so as to avoid
   transaction contexts from blocking each other, and to avoid issues with threads wait.

@section{Function Naming Conventions}

    Functions in db-lib start with a colon prefix describing the object they operate on.  The prefix
    is typically "db:", "table:",  or "keyspace:".

@section{Explicit Key Management}

   A nagging problem in databases has been the limited number of keys available and lack
   of explicit management of those keys within explicit keyspaces.  In order to circumvent
   this problem the db-lib gives LISP direct control over key allocation by providing both an
   allocator for keyspaces, and an allocator for keys within keyspaces.

   Accordingly, when a program desires to have a unique key within a given keyspace, the
   program presents the keyspace to the key allocator and it then returns a key
   that is unique within that keyspace. The program may later deallocate an allocated
   key.  For record IDs the keyspace name is typically the same as the table name, and
   each record ID is then a uniquely allocated key.  It is up to the program to explicitly
   deallocate such a key upon deleting a record.

   The db-lib then treats key fields just as though they are data fields.  One immediate
   result of this is that keys are arbitrary length.

@section{Simplified Column Type}

   It is conventional in the relational model that each column is given an explicitly
   declared type.  However, this causes a lot of problems because values must then
   undergo multiple conversions in order to be shoe-horned from the program into SQL, then
   into the database, and then later back again.  Due to this there is a combinatorial
   explosion in the number of end cases.

   One of the more serious divergences is that LISP programs tend to deal with arbitrary
   length types, whereas databases typically have fixed length types.  If any, the only
   arbitrary length database type will be that of @italic{text}.  Even the Postgres bignum
   is a fixed length integer, though a very long one.

   So instead of following convention and maintaining type in the database, the db-lib
   sets the type for all columns of tables to be @italic{text}.  As all LISP types convert
   to and from text, this is actually a liberating feature rather than a constraining one.
   Nor does does this limit functionality in the database as text items can be ordered and
   placed into indexes. I wonder if this is even a performance issue given that modern
   integers are now 8 characters long, which is 3 characters longer than the average
   English word, and 5 characters longer than the typical number found in a program when
   expressed as a character string.

@section{A Table is a Two Level List}

   In the db-lib model a table row is considered to be a list of elements. A table is then
   considered to be a list of rows.  Each row has the same number of elements, i.e. the
   number of columns is fixed.  In contrast there may be an indefinite number of rows.

   In this model columns are not specifically named nor typed.  The position in a list for
   an element determines which column the element is considered to be part of. See the
   next section for a discussion of column type.

   For some functions it will be an error for a row list to not have the correct length.  For
   other functions there may be a default value for unspecified elements.  Check the
   reference documentation for the behavior of a given function.

@section{Pattern Matching for Fetch}

   We don't pass SQL through this interface. Instead all operations have function names.

   In order to fetch data we perform pattern matching. The user passes a pattern into the fetch routine,
   and then the fetch routine unifies the pattern against the specified table.

   In the db-lib model, patterns apply to specific tables.  See the @italic{dataplex} model for
   multi-table actions.

  [Implementation notes: At this time patterns apply to single tables. Also we really
  need to get code on the server side to do the unification, the current implementation is
  on the client side.]

@section{Transaction Blocks and Threads}

   If a given transaction in a given thread begins, and then another transaction in that same
   thread begins, the other transaction is nested within the given transaction.  The given
   transaction will not end until after the other transaction has ended, because
   transaction contexts require matched paraens.  This situation is considered normal
   nesting of transactions.

   If a given transaction in a given thread begins, then a another transaction begins in another
   thread, we have a problem.  This is because the database operations within both transactions will
   interleave on the database connection.  If the other transaction completes first, it will appear
   to the database server that it is nested, and it will retire all of the interleaved transactions
   since it began, including the ones belonging to the given transaction.   If the given transaction
   completely first, the database will be confused because that is not proper nesting order.

   Hence, we support nesting of transactions on the same thread but make simultaneous
   transactions on differing threads blocking.   

   This raises an unusual design issue for us, as we must recognize within a transaction
   context that a new transaction in the same thread should not block, but a new
   transaction in a different thread should. We make this distinction by giving a
   smemphore a thread owner property.  So a transaciton will say, "if the semphore is
   taken, but my thread is the owner of it, I get to go anyway."

   It might be a better design option for some programs to avoid the complications of
   transactions between threads blocking, by simply using different database contexts on
   differing threads.  The database server then knows not to interleave the operations
   that arrive on different connections.  There would still be a race condition as to
   which transaction finishes first, but this same race condition occurs between all of
   the users of the database on all the various connections.


@section{To-do}

    Patterns are not fully implemented.  Currently we only support the '_' that matches anything.

    We really need to makes sure the to string conversion stuff makes for safe SQL.  I kinda suspect
    that right now it doesn't.

    Deallocation for keysapces is not implemented.  Also we need to add tests for the existance of
    stale values use (using keys that were deallocated). 

    Add a function for setting the dataase connection parameters so we can add some flexibility.  Right
    now we just assume postgres and unix login credentials.  Brian's db library connect function has many
    more options.