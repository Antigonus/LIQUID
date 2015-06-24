#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "db-lib"]{db-lib guide}


@defmodule[liquid/db-lib]{The @racketmodname[liquid/db-lib] library provides tools
for creating, deleting, and using tables in a database.}


@section{A LISP friendly Database Interface}

   The following sections describe features of a simplified database interface useful
   for LISP programs that desire to have persistant relaitonal storage of data.

   Due to these simplications this interface might not be useful when applied to a
   database that have been created outside of this interface and make use of column
   types and other unsupported features.


@section{Program Level Type Interpretation}

   In a conventional table we are given a fixed number of columns and an indefinite number
   of rows.  Each column has a name and a type.

   It is the type we take issue with here, as it is difficult for a contrived interface to
   have to deal with the difference in interpretation of type by the database server, the
   SQL language, and by LISP itself. One of the more serious divergences is that LISP
   programrans tend to deal with arbitrary length types, whereas databases typically have
   fixed length types.  The only common arbitrary length type in databases is that of
   text.

   In order to circumvent these isssues of conversions and concommitant exceptions, the
   db-lib sets the type for all columns to text.  As all LISP types convert to and from
   text, this is actually a liberating feature rather than a constraining one.  Any type
   conversions then occur on the LISP side.  As text can be ordered, the database can
   still build indexes and perform comparisons.  One might even wonder in this day and age
   of 64 bit words if using text is even much of a constraint, as most any integer looks
   like a string of 8 characters.

   This choice also relieves the SQL interface and interpreter from having to perform
   double type conversion, first to text, and the to the destination type, or vice versa.

@section{Explicit Key Management}

   A nagging problem in databases has been the limited number of keys available and
   lack of explicit management of keys within keyspaces.  Hence we provide LISP with
   direct access to a keyspace allocator and a key allocator.

   Accordingly, when a program desires to have a unique key within a given keyspace, the
   program presents the keyspace to the interface and then the interface returns a unique
   key.  This is an allocation operation.  The program may later deallocate the key.  For
   record IDs the keyspace is typically the same as the table name and the record ID is
   then a uniquely allocated key.  It is up to the program to explicitly deallocate the
   key upon deleting a record.

@section{Fetch by Match}

   We don't pass SQL through this interface. Instead all operations have function names.
   In order to fetch data we perform patter matching.  Of course we wish the server would
   do the pattern matching for us, but until someone implements a server side program for
   this, we do the matching on the client side, and even worse, we don't cache the
   fetches.  There is certainly a lot of room for improvement in our match implementation.

@section{'with' Contexts}

    Currently the interface connects to postgresql using the user's role and database via
    unix credentials.  However, in the fture we will have connetion contexts, then all
    library calls will have to occur within with-connection blocks.

    Transactions occur within as-transaction blocks.  The transaction is closed when
    leaving the block.

    Currently transactions blocks are also semaphore blocks, thus making them all
    independent.  This will have to be changed to have separate semaphores for each
    connection.  Semaphores are needed so that the instructions in transaction blocks
    on separate threads don't get shuffled together on one connection.

