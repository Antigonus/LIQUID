

#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               


@title[#:tag "node-concepts"]{node}

   'node' was designed specifically to be a node in a parse tree created by the parser
   tools packaged here in the liquid library.  Of course programmers may use nodes for
   any purpose they find convenient. Same goes for the parser tools.

   I use a stylized coding that mimics object oriented programming.  Hence the first
   operand of functions that operate on our objects is the object itself.  We have a list
   type that is nearly identical to the LISP list, hence we provide functions like
   @racket[bcons] which is like @racket[cons] but the list comes first.  (I put
   @racket[bcons] into msic.rkt for now.)
   
@section{object types}

@subsection{list and stream}

   Our list type is a slightly constrained version of the LISP list. 

   Lists are containers for items.  An item may be any LISP object, including another list.

   A list with no items is said to be null.  

   A non-null list has a designated 'first' item.  The 'rest' of the list can be found by
   removing the first item.  The rest of the list is also a list, thus, if it is not null,
   it will also have a first item.  So for a given list, the second item is the first item
   in the rest of the list. etc.

   Splitting a list into a @racket[first] item and @racket[reset] list is known as
   'iteration', or an iteration step.  If an iteration step returns null for the rest
   list, then the item returned is also known as the 'last' item.

   All non-null lists have both a first and a last item.  In a list with one item, the first
   and last are in fact the same item.  In lists with more than one item, first and last
   are distinct items.

   The length of a null list is zero.  The length of a non-null list is the natural number
   described by its structure.  (Note that non-null list structure fits the Peano definition for a
   natural number.)

   A @raceket[stream] shares the properties of a list save one, namely, even a non-null
   stream might not have a last item.

   Typically when we parse we start by pulling out lexical nodes from a character stream,
   and we expect the stream to end, i.e. to have a last character.  As lexxing is
   typically a 'first phase', continued processing does not occur until the lexer finishes,
   i.e. until it sees the last character on the stream and processes it.  Hence, such
   parsers will require that the stream has a last character.


@subsection{typed list}

   The @racket[typed list] is derived from the @racket[list].  A typed list has as its
   first item a symbol that is known as the list's 'type'. The rest of the items are known
   as the 'value'.  Because the rest of a list, is also a list, the value is a list.
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

@subsection{node}

   A node is derived from the 'typed list with ascribed attributes' by adding the
   constraint that any value items are also nodes.  Thus a node has three parts,
   the 'node type', the 'ascribed attributes' and the 'node children'. 

   There are additional constraints placed on well formed nodes which appear within
   liquid parse trees.  Namely, the node types must come from an enumeration of allowed
   node types.  Secondly the attribute types must come from a list of allowed attribute
   types.  Though these constraints can be weakened when parsing a file that defines node
   or attribute types.  Typical query languages do not do this.  Each node must have a 'source'
   attribute.  The source attribute provides an interval of character positions in the
   source stream where the node was parsed from.  Alternatively, in a hierarchical parse
   higher level nodes may instead keep a list of nodes it was created from (rules reduce
   sections of a parse tree (node tree) to nodes).

   In addition there is built in support for 'lexeme' and 'value' attributes.  The value
   for the lexeme attribute is the raw text from the source file for the object.  It is
   used in error messages.  The value attribute is used to hold the result when a single
   local interpretation is possible.

   When it comes to nodes, the meaning of the term 'value' depends on semantic context.
   In the context of its type inheritance a node's value is the list after dropping the
   type and ascribed attribute items.  In the context of parsing, a node's value is found
   in the value attribute.  Take for example the node type @racket[nd:number], this node
   will have a value type attribute holding a number.  Similarly, a 'node's source' is the
   value of the ascribed source attribute.  In general, apart from type or children, a
   node's X is the value of the X attribute.

   Parse trees are intended to be persistent objects that are shared, hence the
   node type enumeration and attribute type enumerations are global.  For a specific
   parser one chooses a prefix, and then names the node and attribute types accordingly.
   The parser itself uses type prefixes of 'tk:' for node types, and 'at:' for attribute
   types.  Users should keep these and add their own extenstion to the prefix.  Say as
   a hypothetical example, 'tk:calc:'  for a calculator language node.

   To operate on a node object as though though it were an object of parent type, i.e. as
   an ascribed attribute list, or a typed list, use the provided functions
   @racket[on-attributes] or @racket[on-children] (the typed list value is the list of
   children).  These parts can also be accessed using @racket[nd-attributes] and
   @racket[nd-children].

 @section{support for handling errors}






 
    