

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "query-parser-concepts"]{query-parser}

   This is a framework for writing context free (or a little bit of context) parsers,
   where the resulting parse will contain error tokens when things don't go as planned.
   The error tokens can then be scanned out to create informative error messages within
   context. The example shown is specifically for parsing conjunctive queries.

   The basic approach is that of reccursive descent.  There is a top level rule provided
   by the program which attempts to parse the entire input.  This is the great grandaddy
   of all the rules.  Each parent rule then calls child rules in attempt to parse out
   sub-parts as specified by the grammar.  If the grammar allows for alternatives, then
   after a child rule mismatch, the parent rule will attempt the alternative child rule.
   Should no alternative match, then the parent will replace this part of the syntax
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

@section{to-do}

The parse tree type should be unified with the x-expr type.  Probably should rename this to
just 'parser'  as it can be used for anything, not just queries.


