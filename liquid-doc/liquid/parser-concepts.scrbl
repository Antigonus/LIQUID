

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "parser-concepts"]{Stream Parser}

   We examine an input stream for one or more interpretations of a correlating parse tree.

   We pull input in blocks, though the pull may block if we are waiting for data to arrive.
   Hence, pulls should be wrapped in timed calls.

   The lexer pulls characters in blocks from the stream library.

   The parser pulls lexer tokens in blocks.  Upon analyzing each block, there will be
   multiple parse tree intpretations and unfinished business.  The state for completing
   the unfinished business is carried over to the parse of the next block.

   Due to errors or the fact that we haven't yet seen the last block of a parse (and may
   never due so), or even by design,  there may be more than one active interpretation of
   the token stream.

   For example,  quote balancing is unstable. Hence, we may purposely add interpretations
   that a closing quote is missing.  If the primary interpretation starts having many errors
   in the parse, then we may look to the other interpreations to see if they have fewer or
   less serious errors.

   


 

