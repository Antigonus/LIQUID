

#lang scribble/base

@title{Query-Parser}

@section{Introduction}

   This is a framework for writing context free (or a little bit of context) parsers,
   where the resulting parse will contain error tokens when things don't go as planned.
   The error tokens can then be scanned out to create informative error messages within
   context. The example shown is specifically for parsing conjunctive queries.

   The basic approach is that of reccursive descent.  There is a top level rule provided
   by the program which attempts to parse the entire input.  This is the great grandaddy
   of all the rules.  Each parent rule then calls child rules in attempt to parse out
   sub-parts as specified by the grammar.  If the grammar allows for alternatives, then
   after a child rule mismatch, the parent rule will attempt the alternative child rule.
   Should no alternative match, then the parent will replace this part of the syntax part
   with an error token.

   The rules we refer to here are functions, and these functions are written by the author
   of the parser.  Hence full knoweldge of the grammar and how to handle errors can be
   custom built into each rule by the programmer.

   As an example, an example rule I have provided is for parsing 'framed' lists of
   syntactical elements.  Here the framing is provided by a separator punctuation and some
   sort of last item separator.  The framing rule  does not do recursive descent, but
   instead correlates the framing pattern against the input.  This is more stable than
   doing recursive descent for such situations.  (The example framing rule can use
   some improvements.)
   
   A parse rule reduces input text to a 'token'.  A token is a list, where the head of the
   list is a type, and then this is followed by an attribute list, and then remaining
   members are childr tokens, if any.  The form is almost the same as that of x-expr for
   the html parser.  On the next version we will make them identical so the two tools can
   exchange data.


@section{query-parser}

   (rule ts [imperative (imperative:try)])

   Each rule accepts a list of tokens and an optional mode, then returns #f or a token.  Typically
   the rule will want to match all of the tokens in the token list.  The two modes are:

     imperative:try
     imperative:force

   In the imperative:try mode the rule must return #f when there is no match.  In the
   imperative:force mode the rule must return an error token when there is no match.
   Error reports from a rule only go into the return token, so at the election of the
   parent rule, an error token returned from a child rule can be interpreted to mean as
   little as that there was a mismatch, or mean something so serious that the parent rule
   also must fail.

   Error tokens have an attribute set that marks them as such.  An error token can be unique for the
   error condition, or it can simply be a regular token with annotation saying there was an error.

   The example query-parser is called with:

   (query-parser in)

@section{query-parser-tokens}

   This module defines some fundamental tokens.

    (tok:comment)
    (tok:errsyn)
    (tok:lex-other)
    (tok:null)
    (tok:number)
    (tok:punc)
    (tok:string)
    (tok:symbol)
    (tok:paren-node)

  The query parser example adds these tokens:

    (tok:conjunction)
    (tok:operand)
    (tok:operand-list)
    (tok:pattern)
    (tok:pred)
   
   These utility functions are provided:

@subsection{append-errorsyn}

  (append-errorsyn ts t mess)

   ts is the list of tokens were were attempting to parse when the error occured.  t is a token without an error message.
   mess is the error message.  Returns the token with error attributes added.

@subsection{append-errint}

  (append-errint ts t mess)

   Similar to append-errosyn, but also causes the error to be logged.

@subsection{tok-make-errsyn}

  (tok-make-errsyn ts tok-type generator message)

  Similar to append-errorsyn, but creates a new token rather than modifying one that is passed in.

@subsection{tok-make-errint}

   (tok-make-errint ts tok-type generator mess)

   Similar to append-errint, but creates a new token rather than modifying one that is passed in.
   


   punc-val-is    accepts a token and a value returns a bool
         

