

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "token-concepts"]{token}

@section{purpose}

   'token' was designed specifically to be a node in a parse tree created by the parser
   tools packaged here in the liquid library.  Of course programmers may use tokens for
   any purpose they find convenient. Same goes for the parser tools.

@section{operand ordering}
   
   When a function operates on an object, we list the object as the first operand.  Hence
   a function like @racket[type-is]  has as a first operand the typed list, and as a second
   operand the type we are checking against. In a sesnse we wish functions could be method
   calls for the object, but not badly enough to create racket classes.

@section{types}

@subsection{list and stream}

   Our list type is a slightly constrainted versinon LISP list. 

   Lists are containers for items.  An item may be any LISP object, including another list.

   A list with no items is said to be null.  

   A non-null list has a designated 'first' item.  The 'rest' of the list can be found by
   removing the first item.  The rest of the list is also a list, thus it will also have a
   first item.  So for a given list, the second item is the first item in the rest of the
   list. etc.

   Removing the first item from a list is known as 'iteration', or an iteration step.
   An iteration step returns the first item, and the rest list.  If an iteration step
   returns null for the rest list, then the item returned is also known as the 'last'
   item.

   All non-null lists have both a first and a last item.  In a list with one item, the first
   and last item are in fact the same item.  In lists with more than one item, they are distinct.

   The length of a null list is zero.  The length of a non-null list is the natural number
   described by its structure.  (Non-null list structure fits the Peano definition for a
   natural number.)

   A @raceket[stream] shares the properties of a list, though even a non-null stream might
   not have a last item.  

   Typically when we parse we start by pulling out lexical tokens from a character stream,
   and we expect the stream to end, i.e. to have a last character.  As lexxing is
   typically a 'first phase', continued processing does not occur until the lexer finishes,
   i.e. until it sees the last character on the stream and processes it.  Hence, such
   parsers will require that the stream has a last character.


@subsection{typed list}

   The @racket[typed list] is derived from the @racket[list].  A typed list has as its
   first item a symbol that is known as the list's 'type'. The rest of the items are known
   as the 'value'.  Because the rest of a list, is also a lit, the value is a list.
   It follows that typed lists are never null.

   The type for a typed list invokes a type definition.  Such a definition may be as little
   as a set of usage constraints that programmers promise to follow.  These constraints
   may specify the cardinality and form allowed for the value among other things.

@subsection{attribute)

   An attribute is derived from a typed list.  The thing that distinguishes an attribute
   from a typed list is mainly how programmers talk about it and use it.  Hence an
   attribute has a type and a value.  


@subsection{typed list with ascribed attributes}

   The 'typed list with ascribed attributes' has as its first item the list type, and has
   as its second item a list of ascribed attributes.  The rest of the items form the value.

   The ascribed attribute list may be null, but it is always present. In general it is
   possible to ascribe more than one attribute of the same type.  There is no semantic
   order between the attributes. No program may rely upon attribute order.

   When we speak of 'the X attribute', we are describing a filter operation, one that
   selects from the ascribed attributes list the attribute which has the type X. 

@subsection{token}

   A token is derived from the 'typed list with ascribed attributes' by adding the
   constraint that any value items are also tokens.  Thus a token has three parts,
   the 'token type', the 'ascribed attributes' and the 'token children'. 

   There are additional constraints placed on well formed tokens which appear within
   liquid parse trees.  Namely, the token types must come from an enumeration of allowed
   token types.  Secondly the attribute types must come from a list of allowed attribute
   types.  Though these constraints can be weakened when parsing a file that defines token
   types.  Typical query languages do not do this.  Each token must have a 'source'
   attribute.  The source attribute provides an interval of character positions in the
   source stream where the token was parsed from.

   In addition there is built in support for 'lexeme' and 'value' attributes.  The value
   for the lexeme attribute is the raw text from the source file for the object.  It is
   used in error messages.  The value attribute is used to hold the result when a single
   local interpretation is possible.

   When it comes to tokens, the meaning of the term 'value' depends on semantic context.
   In the context of its type inheritance a token's value is the list after dropping the
   type and ascribed attribute items.  In the context of parsing, a token's value is found
   in the value attribute.  Take for example the token type @racket[tok:number], this
   token will have a value type attribute holding a number.  The tok:number token never
   has children.  Similarly, a 'token's source' is the value of the ascribed source
   attribute.  In general, apart from type or children, a token's X is the value of the X
   attribute.

   Parse trees are intended to be persistent objects that are shared, hence the
   token type enumeration and attribute type enumerations are global.  For a specific
   parser one chooses a prefix, and then names the token and attribute types accordingly.
   The parser itself uses type prefixes of 'tk:' for token types, and 'at:' for attribute
   types.  Users should keep these and add their own extenstion to the prefix.  Say as
   a hypothetical example, 'tk:calc:'  for a calculator language token.

   Had this been created in Ruby, I suppose a token would simply be a hash
   keyed by attribute type, and token type and children would also have been attributes.
   The current design was to facilitate writing structural code that ignores attributes,
   and then leaving attributes for the programmer to customize the token. 

 @section{support for handling errors}






 
    