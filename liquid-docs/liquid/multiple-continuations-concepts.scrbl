#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "mulitple-continuations-concepts" #:style '(toc)]{Multiple Execution Path Exits}

    Any arbitrarey block of code can be considered to be a function with the current context
    as the input argument list, and the values potentially changed by the code as the
    outputs of the function.

    It follows that the continuation after an explicit function call is just another
    function.

    The most parallel interpretation of a program may be found by making a data flow graph and
    then removing all false dependencies.  In single threaded execution we will traverse one
    path through the flow graph, though this path may contain loops.  Thus, though there is only
    one thread of execution, there are multiple possible execution paths, and a thread will be
    on one of them.

    Note, that the @racket[if] function diverts execution down one of two different exeuction paths
    based on the result of a Boolean expression.

    Though we have constructs for creating multiple execution paths the current function
    paradigm does not allow multiple execution paths when leaving a function.  This
    creates an artificial constraints, and causes programmers to invent ways and write
    code to pack execution path information into messages, and then after the function
    returns, to unpack those messages and then recover the execution path information.
    Example contrivances include returning a value and then enttering and if block or case
    statement, returning 'error codes', and throwing exceptions.

    In this library we have provided direct support for defining functions that have
    multiple execution path exits so that the program will not have to invent and implement
    such contrivances.  I have chosen a method of multiple continuations.  Accordingly,
    each continuation, on each execution path, is passed as a lambda function.  When the
    function desires to return, it picks its continuation and calls it. Note, the syntax
    transform @racket[em:define], for defining multiple exit path / multiple continuations
    functions.

    With this technique, if a function makes a control decision, it doesn't have to burry
    that decision in a message that is later extracted and acted upon, but instead it can
    simply take a different exit path.

