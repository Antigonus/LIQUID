#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "mulitple-continuations-concepts" #:style '(toc)]{Multiple Continuations}


@section{Functional Continuations}

    Any arbitrarey block of code an be considered to be a function with the current context
    as the argument list.  Since the continuation after a function call is a block of code,
    a continuation is also a function.

    As I explained to Bill Kahan some years ago, all exceptions are really signals that we
    need to do more processing.  Hence, what we would like to have is the option of
    muitple choice among different continuations.  This observation stems from my work in
    data continuations and arbitrary precisions computing, where an exception might signal
    the need to fetch more data or try a different computational rather than the fact that
    the program should crash.

    With the functional approach to continuations multiple choices for continuations are
    given as arguments passed into a function.  Accordingly when a function gets to a
    return point instead of returning it calls the appropriate continuation.

    It only makes sense that data continuations over a tree would have a corresponding
    structure of functional continuations with multiple control flow options.

    The functions that handle buffered streams are small albeit lineary example.  Buffer
    empty, or nearing empty, does not raise an exception, but rather it calls the lower
    level i/o routine to fetch another buffer.

    An interesting partitiong arises.  On blocks of data we employ finite computational
    methods, when an end case occurs this triggers are highler level arbitrary data length
    mechanism.  This partitioning is efficient for hardware, as the localized intense
    computation can occur against the registered or cached finite data block while using
    direct references, and then the next block fetch can be a separate prefect task that
    runs its own dedicated code tailored for prefetch that makes use of arbitrary data
    iteration approaches.  I made use of this partitioning in the Turing Complete
    Architecture processor.

