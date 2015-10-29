#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "node-reference"]{node}

@defmodule[liquid/node]



@section{typed}

@defproc[(typed-make [the-type symbol?] [value-item any/c] ...) is-typed-list]
@defproc[(typed-make* [the-type symbol?] [values (listof any/c)]) is-typed-list]

  Makes a typed list of the given type.

@defproc[(typed-is-well-formed [tl is-typed-list] [type-enumeration (listof type?) null]) bool?]

  A well formed typed is a list that starts with a symbol representing the type.
  Optionally the type symbol is checked to see if it belongs to a type-enumeration.

@defproc[(type [tl typed-list?]) symbol?]

  Returns the type of the typed list.

@defproc[(value [tl typed-list?]) (listof any)]

  Returns the value of the list by dropping they type from the given list.

@defproc[(is-of-type [tl typed-list?] [a-type symbol?]) bool?]

  Returns true if the typed list is of the given type, otherwise returns false.


@defproc[(typed-equal-hook [type-a symbol?] [type-b symbol?] [a-function (-> is-typed-list is-typed-list bool?)]) void?]

  Registers a function to be used to check equality between two lists of the given
  types.  

@defproc[(typed-equal? [tl-a typed-list?] [tl-b typed-list?]) bool?]

   Checks equality between the typed lists.  If an equal function is registered for the
   types of the given lists, then the registered function is used, otherwise
   @racket[equal?] is used.

@section{attributes}
  
@defproc[(attribute-make [attribute-type symbol?] [attribute-value any/c] ...) the-attribute]
@defproc[  (attribute-make*     [attribute-type symbol?]     [attribute-values (listof any/c)]    )    the-attribute    ]

    Creates a new attribute of the given type.  This does not automatically register the
    attribute type.  The value part must follow any constraints associated for the given
    attribute.

@defproc[(at:lexeme) symbol?]
@defproc[(at:source) symbol?]
@defproc[(at:source-nds) symbol?]
@defproc[(at:value) symbol?]

   These are predefined attribute types. 

@defproc[(is-attribute-type [a-type symbol?]) bool?]

    True if a-type is registered as an attribute type, otherwise false.

@defproc[(is-attribute (at is-attribute)) bool?]

    True if the type of @racket[at] is registered as an attribute type.

@defproc[(attribute-hook [attribute-type symbol?] ...) void?]
@defproc[(attribute-hook* [attribute-types (listof [attribute-type symbol?])]) void?]

   Registers one or more new attribute types.

@defproc[(attribute-is-well-formed [at is-attribute]) bool?]

   A well formed check for attributes

@section{ascribed attributes}

@defproc[(has-attribute [attributes (listof is-attribute)] [a-type symbol?]) bool?]

   Given a list of attributes returns true if at least one of the attributes has the given type.
   Otherwise false.

@defproc[(get-attribute [attributes (listof is-attribute)] [a-type symbol?]) (listof is-attribute)]

   Returns those members of the given attribute list that have the given type.

@defproc[(remove-attribute [attributes (listof is-attribute)] [a-type symbol?]) void?]

    Removes members which have the given type.

@defproc[(ascribe [existing-ats (listof is-attribute)] [new-at is-attribute] ...)  (listof is-attribute)]
@defproc[(ascribe* [existing-ats (listof is-attribute)] [new-ats (listof is-attribute)]) (listof is-attribute)]

    Puts attributes into the given list.  I.e. ascribes them to the object that holds
    the attribute list.

@defproc[(update-attribute [attributes (listof is-attribute)] [at is-attribute]) (listof is-attribute)]

    Removes all attributes of the type of the new attribute, then ascribes the new
    attribute.  (Seems this function should be var arg like get and ascribe)

@defproc[(attribute-iterate [attributes (listof is-attribute)]) (list (listof is-attribute)  (listof is-attribute))]

    Returns a list of two items.  The first items is a list of all attributes that have the same
    type as the first attribute in the given attribute list.  This list includes the first attribute,
    so if the given attribute list is not null, then this list will have at least one member.  The
    second list contains the remaining attributes.
    
@defproc[(gather-values [attributes (listof is-attribute)]) (listof any)]

    Puts all value items from the attribute list into one list.  Follow a call to get-attributes
    with this call, and get all value items for all attributes of the given type.

@section{source}

@defproc[(position-deconstruct (p is-position)) (listof number?)]

   I wanted to maintain a list paradigm, so I deconstruct the @racket[pos?] structs into lists.

@defproc[(position-null) is-position]

    Position @racket[(0 0 0)].  Sometimes used as a placeholder or a flag.

@defproc[(position-is-well-formed [p is-position]) bool?]

   Checks that a position list has three numbers.  Note that the first number is a character
   offset, the second number is a line count, and the third number is a column.  Column 0
   signifies the line break character.  Column 1 is the fist character on the row.


@defproc[(source-generator [s is-source]) string?]

    Given a source attribute returns the generator.

    The generator is the program that created the node.  Typically this will be a string
    with a pathname to the module extended by the name of the function.  Or in the case of
    a helper function, function/helper.

    The generator should be a string, but as an artifact of development there are symbols for generators
    in the test code.

@defproc[(source-pathname [s is-source]) string?]

    Given a source attribute returns the name of the source data object. Typically this a
    file path or a path to a variable.  If it is a variable name, then it should be a
    pathname to the module followed by the function name followed by the variable name.

@defproc[(source-start [s is-source]) is-position]

    Given a source attribute, returns the start position for the text that was parsed.
    This is a list of three numbers.

@defproc[(source-end [s is-source]) is-position]

    Given a source attribute, returns the position for the last character for the text that
    was parsed.

@defproc[(source-is-well-formed [s is-source]) bool?]

    Returns true if the source is well formed.

@defproc[(source-make [generator string?]  [file-path string?] (start is-position)  (end is-position)) is-source]

    Given the generator, the file path to the source being parsed, the start position
    which is a list of 3 numbers, the end position which is also 3 numbers, returns a
    source objet.

@section{node}

@defproc[(nd-make [of-type symbol?] [attributes (listof is-attribute)] [child is-node] ...) is-node]
@defproc[(nd-make* [of-type symbol?] [attributes (listof is-attribute)] [children (listof is-node)]) is-node]

   Makes a node of the given type with the given attributes and children.

@defproc[(nd:null) symbol?]

   A built in node type.

@defproc[(is-nd-type [a-type symbol?]) bool?]

   True if the given type is registered as a node type.

@defproc[(is-nd [nd is-nd]) bool?]

   True if the type of @racket[nd] is registered as a node type.


@defproc[(nd-hook [a-type symbol?] ...) void?]
@defproc[(nd-hook* [types (listof [a-type symbol?])]) void?]

   Registers a node type.

@defproc[(nd-children [nd is-node]) (listof is-node)]

    Returns the children of @racket[nd].

@defproc[(nd-attributes [nd is-node]) (listof is-attribute)]

    Returns the attributes ascribed to the given node.

@defproc[(nd-is-well-formed [nd is-node]) bool?]

     True if the given node is well formed.

@defproc[(nd-equal? [a is-node] [b is-node]) bool?]

    Compares nodes for equality.  This will recur into the attributes and the children.


@defproc[(nds-equal? [nd-list-a (listof is-node)] [nd-list-b (listof is-node)]) bool?]

     Maps nd-equal? over the lists. Short circuits on a false compare.

@defproc[(on-attributes [nd is-node] [function ((listof is-attribute) [additional-arg any/c] ... . -> . (listof is-attribute))][additional-arg any]  ...) is-node]

      Calls the given function on: the attributes of the given node followed by the additional arguments.

      The given function must return the modified attributes list.

      @racket[on-attributes] returns the given node with attributes list replaced by the modified attributes list.

@defproc[(on-children [nd is-node] [function ((listof is-node)  [additional-arg any/c] ... . -> . (listof is-node))][additional-arg any]  ...) is-node]

      Calls the given function on: a list of children from the given node followed by the additional arguments.

      The given function must return a modified modified children list.

      @racket[on-children] returns the given node though with the modified children.

@section{node utilities}


@defproc[(nd-make-source [a-type symbol?] [s is-source]) is-node]

     Makes a node of the given type that has the given source attribute.  Presumably more
     will be added to the node after this call.

@defproc[(nd-make-value [a-type symbol?] [s is-source] [v any?]) is-node]

   Makes a node of the given type that has the given source attribute and a 
   value attribute with value @racket[v].

@defproc[(nd-make-lex [generator string?] [a-type symbol?] [start is-position] [end is-position] [lexeme string?]) is-node]

   Typically used by a lexer to make nodes.  Generator will be the path to the lexer module followed
   by the lexer function.  In our example code we use a symbol for the generator.  Type is the
   node type.  Start and end are numeric triples,  character offset, line, and column.  Lexeme is
   the series of characters that the lexer is recognizing.

@defproc[(nd-make-parse [node-source-stream (listof is-node)] [generator string?]) is-node]

    Makes a node from within a parser working on stream of nodes created by a lexer.  The list of
    nodes are the lexer tokens the node is being made from.  The generator is the path to the parser
    function.  @racket[nd-make-parse] derives a source attribute from the lex nodes, and sets the
    start and end values.

@defproc[(nd-source [nd is-node]) (listof is-attribute)]
@defproc[(nd-lexeme [nd is-node]) (listof is-attribute)]
@defproc[(nd-value [nd is-node]) (listof is-attribute)]
@defproc[(nd-source-1 [nd is-node]) is-source]
@defproc[(nd-lexeme-1 [nd is-node]) string?]
@defproc[(nd-value-1  [nd is-node]) (listof any/c)]

  These are @racket[get-attribute] shortcuts.  The first three get source, lexeme, and value attributes
  respectively.  The last three are for use when external constraints dictate that the node may have
  only one attribute of the specified type. For the latter three the routines then return the value for
  the attribute.

@defproc[(nd-start-pos [nd is-node]) is-position]

   Returns the start position from the source attribute.  If there is no source attribute
   it returns the null position.  If there is more than one source attribute it throws an
   exception.

@defproc[(nd-end-pos [nd is-node]) is-position]

   Returns the start position from the source attribute.  If there is no source attribute
   it returns the null position.  If there is more than one source attribute it throws an
   exception.


@section{node match helpers}

  @defproc[($nd-type-is [a-type symbol?]) (is-node . -> . bool?)]

    Returns a function that takes one argument.  That argument is a node.  The returned
    function returns true if the node has the given type.

  @defproc[($nd-has-attribute [attribute-type symbol?]) (is-node . -> . bool?)]

    Returns a function that takes one argument.  That argument is a node.  The returned
    function returns true if the node has an attribute of the given type.

  @defproc[($nd-has-value [attribute-type symbol?] [attribute-value any/c] ...)  (is-node . -> . bool?)]

    Returns a function that takes one argument.  That argument is a node.  The returned
    function returns true if the node has an attribute of the given type and given
    attribute value.


@section{ node type management }

  @defproc[(nd:comment) symbol?]
  @defproc[(nd:errsyn) symbol?]
  @defproc[(nd:lex-other) symbol?]
  @defproc[(nd:number) symbol?]
  @defproc[(nd:punc) symbol?]
  @defproc[(nd:string) symbol?]
  @defproc[(nd:symbol) symbol?]

    Built in node types.

  @defproc[(punc-is [nd is-node] [punc string?]) bool?]

    True if the given node is punctuation type and the lexeme is @racket[punc].  Punc
    can be any number of characters.

  @defproc[(symbol-is [nd is-node] [symbol string?]) bool?]

    True if the given node is of symbol type, and has a lexeme attribute with a value equal
    to the given string.
    
@section{ errror facility }

  @defproc[(at:errsyn-mess) symbol?]
  @defproc[(at:errsyn-nds) symbol?]

    Built in attribute types.  

  @defproc[(at:errsyn-nds-equal? [at-a is-attribute] [at-b is-attribute]) bool?]

    Given two attributes of type @racket[at:errsyn-nds] type, returns true if they are equal.
    This recursively compares the children nodes.
    This equality function is registered with @racket[typed-equal?], so it will be
    called automatically when @racket[typed-equal?] is called.
    
  @defproc[(imperative:test) symbol?]
  @defproc[(imperative:force) symbol?]
  @defproc[(imperative:committed) symbol?]

    These flags are used to tell rules what to do when the match does not work out. 

    When the imperative flag for a rule is set to  @racket[imperative:test]
    returns false if there is not a match.   This is the typical 'yacc' type grammar rule
    matching behavior.  This is the default setting for the imperative flag in the rule
    arguments list.

    Setting the imperative flag to @racket[imperative:force] tells the rule that it must
    return a node.  The rule will may an @racket[nd:errsyn] type node if the rule can make
    no sense of the input.  If the rule does not match, but the match is close, the rule may
    return a node of the type expected to be parsed, but with an @racket[at:errsyn-mess] attribute
    added, and optionally an @racket[at:errsyn-nds] holding the unintelligible part.

    Rules may be forced when a higher up rule finds enough information to know that it is locked
    into place.

    @racket[(imperative:committed)] rules have the option of returning false or returning a node marked
    with errors. This is the typical mode of use for rules.  
    
 @defproc[(ascribe-errsyn [source-nodes (listof is-node)] [nd is-node] [message string?]) is-node]

    The given list of nodes is parsed by the rule.  The given node is the result of the
    parse, and the string is an error message.  The result is the given node which is
    modified to have the error message attribute, and to have an list of nodes the error
    came from.

    This is commonly called within a rule before it returns an error node.

@defproc[(ascribe-errint [source-nodes (listof is-node)] [nd is-node] [message string?]) is-node]

    Similar to @racket[ascribe-errsyn], but for internal errors.  Such errors are not syntax errors,
    but are instead due to something such as a bug in the program.  @racket[ascribe-errint] returns an
    error node, and also logs the errror in the @racket[(current-log-file)].

@defproc[(nd-make-errsyn [source-nodes (listof is-node)] [a-type symbol?] [generator string?] [message string?]) is-node]

    Creates a new node marked with the given error.

@defproc[(nd-make-errint [source-nodes (listof is-node)] [a-type symbol?] [generator string?] [message string?]) is-node]

    Like @racket[nd-make-errsyn], but also logs the error.

@defproc[(rule-errsyn [source-nds (listof is-node)] [imperative symbol?] [a-type symbol?] [generator string?] [message string?]) (or/c is-node #f)]

    Similar to @racket[nd-make-errsyn]  but pays attention to the imperative flag.  Hence,
    if imperative is @racket[imperative:test] the rule just returns false.  It is an error to give
    this rule @racket[imperative:committed].

@defproc[(rule-errsyn-expected [source-nodes (listof is-node)] [imperative symbol?] [a-type symbol?] [generator string?]) (or/c is-node #f)]

    Similar to @racket[rule-errsyn] but the error message is automatically set to "expected <type> but got:".

@defproc[(rule-errparser [source-nodes (listof is-node)] [imperative symbol?] [a-type symbol?] [generator string?] [message string? ""]) (or/c is-node #f)]

    Similar to @racket[rule-errsyn], but the message will be logged.  This is for internal errors.

@defproc[(nd-has-err [nd is-node] ...) bool?]

    True if the given node is an error node, or if it is a regular node with an error message attached.  Otherwise
    false.

    Note that this function does not look at the child list.  Use a parse filter function if you want to look for errors
    in a parse tree rather than an individual node, see the module @racket[liquid/filter].

@defproc[(nd-has-err* [a-node-lit (listof is-node)]) bool?]

    Runs ormap @racket[nd-has-error] over the given node list.