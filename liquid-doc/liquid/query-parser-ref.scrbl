

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "query-parser-ref"]{query-parser}

@defmodule[liquid/query-parser]
@defmodule[liquid/query-parser-tokens]

@section{rule form}

   @racket[(a-rule ts [imperative (imperative:try)])]

   Each rule accepts a list of tokens and an optional mode, then returns #f or a token.  Typically
   the rule will want to match all of the tokens in the token list.  The two modes are:

  @tabular[
    (list
      (list
       @racket[imperative:try]
       )
       (list
       @racket[imperative:force]
      ))
   ]

   In the imperative:try mode the rule must return #f when there is no match.  In the
   imperative:force mode the rule must return an error token when there is no match.
   Error reports from a rule only go into the return token, so at the election of the
   parent rule, an error token returned from a child rule can be interpreted to mean as
   little as that there was a mismatch, or mean something so serious that the parent rule
   also must fail.

   Error tokens have an attribute set that marks them as such.  An error token can be unique for the
   error condition, or it can simply be a regular token with annotation saying there was an error.

   The example query-parser is called with:

   @racket[(query-parser in)]

@section{query-parser-tokens}

   This module defines some fundamental tokens.

  @tabular[(list
    (list @racket[(tok:comment)])
    (list @racket[(tok:errsyn)])
    (list @racket[(tok:lex-other)])
    (list @racket[(tok:null)])
    (list @racket[(tok:number)])
    (list @racket[(tok:punc)])
    (list @racket[(tok:string)])
    (list @racket[(tok:symbol)])
    (list @racket[(tok:paren-node)])
    )]

  The query parser example adds these tokens:

  @tabular[(list
    (list @racket[(tok:conjunction)])
    (list @racket[(tok:operand)])
    (list @racket[(tok:operand-list)])
    (list @racket[(tok:pattern)])
    (list @racket[(tok:pred)])
    )]
   
   These utility functions are provided:

@subsection{append-errorsyn}

  @racket[(append-errorsyn ts t mess)]

   ts is the list of tokens were were attempting to parse when the error occured.  t is a token without an error message.
   mess is the error message.  Returns the token with error attributes added.

@subsection{append-errint}

  @racket[(append-errint ts t mess)]

   Similar to append-errosyn, but also causes the error to be logged.

@subsection{tok-make-errsyn}

  @racket[(tok-make-errsyn ts tok-type generator message)]

  Similar to append-errorsyn, but creates a new token rather than modifying one that is passed in.

@subsection{tok-make-errint}

   @racket[(tok-make-errint ts tok-type generator mess)]

   Similar to append-errint, but creates a new token rather than modifying one that is passed in.


   @racket[punc-val-is]    accepts a token and a value returns a bool
         

