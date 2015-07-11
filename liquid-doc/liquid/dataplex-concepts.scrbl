#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "dataplex-concepts"]{dataplex-lib}


@section{Introduction}

   There are two common approaches to organizing data into groups in memory.  One way is
   to place data together that has the same type format (i.e. same shape).  An example
   is the array type.  Another way is to place things together based on some sort
   of semantic interpretation of the data.  An example is an object from object oriented
   programming.

   The choice a programmer makes for organizing data has profound effects not only on run time
   performance, but also on the effort required for developing and maintaining a program.

   In the paper ....  Andrea Cali describes two kinds of data in his database, one type he
   calls an abstract type.  A phone number, for example, is an abstract concept.  The second
   kind of data is then concrete, for example a specific persons account infomration.

   The recognition of abstract types, can lead to very different schema, and thus affect
   run time performance and the ease of database maintenance.


@section{Database Morphology}

  @subsection{Allocation}

    In context of computer languages, a container is an object used to hold other objects.
    Some example containers are arrays, hash tables, and lists.  Another example is a table
    used in a database.  In this paper we will introduce yet another container, the dataplex.

    All memory used by a program is managed in some manner so that at run time the data
    references will be coherent.  Memory managers will have components known as allocators,
    which pass to programs portions of memory which the memory manager will consider to be
    allocation units.  The protocol of request and allocation unit return between the user of
    memory and the memory manager is called allocation.

    A database is a program that uses memory, and as such it must use a memory manager.
    However, a database is also a program that provides memory management services of a
    special kind to other programs.

    The rules for using an allocation unit may be regulated by hardware and/or left to
    contracts with the user. Allocation is typically considered to be either static, meaning
    that allocation is done at compile time, or dynamic, meaning it is done at run time.
    Database allocation is done at run time.

  @subsection{Data Type - Shape}

    The step after allocation is that of type binding.  Type binding provides internal
    structure to an allocation unit.  This structure is meaningful to the code that uses
    the allocation.  In a sense type binding is a second tier memory allocation, as the
    type system gives to the program the same contracts for use of sub-regions of the
    allocation unit that the memory manager specified for the allocation unit as a whole.

    Donald Knuth defined data type as that structure which is deemed to be present by a
    set of functions that use the allocated memory.  We call this abstract data type.  In
    some language environments today this is also called duck typing (if it quacks like a
    duck it is duck – get it?).  Though this definition might have some nice theoretical
    properties, it is essentially circular – as we had to have a concept of type when we
    wrote the functions that use the data.  This is especially so when such functions are
    written beforehand then compiled.

    Thus in practice we tend to define type in terms of features of the allocation unit,
    and these are typically expressed as a keyed list of byte offsets.  The keys may
    correspond to variable names or data structure field names.  Such a type definition
    gives geometry (also said to be form, or shape) to memory; and thus the science of
    type is also known as data morphology.  The key referred to here is often a symbol so
    the list of keys and offsets is also called a symbol table.  The keys may also be
    called attributes, or properties.

    The values stored in an allocation unit can be represented, say in a print out, with a
    key-value list.  The keys show the user how the represented values in the list
    correspond to the type binding.  For example if we had in memory a complex number, we
    might express its value as, say, (real:10, imaginary:15).  Here 'real' and 'imaginary'
    are the keys (aka attributes or properties), while 10 and 15 represent the “ “values”.
    Such a list is also called an attribute-value list, or a property-value list.

    The two definitions of type have collided with the advent of object oriented
    programming, and sometimes both are present to some extent in a program.  The way the
    methods interact with the data in an object may said to provide the abstract typing,
    while the compiled in offsets, or keys used, provide structured type.

    Like allocation of memory, the creation and binding of type can also be static or
    dynamic.  In conventional database design, type definition is static.  This means that
    it is defined before data is entered into the database, and remains constant while the
    database is used.  Conventionally, changes to type after data is entered is considered
    a program modification, i.e. beyond the purview of the data clerk operator, and
    involving a programmer.

    The term itself, type, today is highly overloaded with meaning and shades of meaning
    both in the colloquial and in computer science.  We would like to use a different term
    to refer specifically to a list of keyed byte offsets.  The term form is ambiguous in
    database land, as form more typically refers to input forms used by data clerks for
    putting data into the database. Hence, in the dataplex library we use the term shape.

  @subsection{Table, Row, and Column}

    A table as used in databases is a container.  The objects contained in tables are
    allocated in fixed length units called rows.  The database itself gives all rows
    belonging to the same table the same shape.  Because all the rows have the same shape,
    the keys found in row type are also called column names.

    An analogous term used in IBM argo for a row is a record.  A record is then said to
    consist of fields rather than of columns.  A table then has field names instead of
    column names or attributes.

    A tuple, also called a vector, is an ordered list of data value representations, where
    the data is of the same shape as specified for a row of a table.  Thus the data
    represented by a tuple may be contained in the row of a table, have been read from the
    row of a table, or may be written to a row.

    Because a database performs both allocation and type binding it can do more work than
    a conventional memory manager.  For example, the database code itself can create
    indexes on columns and can assist in the search for data.

  @subsection{Schema}

    Taken altogether, the sets of attribute lists for tables in a database, along with the
    index definitions, and some other related items, are known as a database schema.
    Hence a schema is yet another type definition.

@section{Database Queries and Statements in Logic}

  The rows in a table are related to each other because they belong to the same table.  Database table rows do not have to be distinct, otherwise we could say that the rows in a table belonged to an equivalence class named by the table.  Instead database scientists have created a new term that allows for this possibility of non-uniqueness.  They say that a table defines a relation.

  Say for example a table has the purpose of holding information about account holders, with one account holder per row - then any given row is a statement of truth that the described person is-a account holder, i.e. each row states a fact.  More specifically we can say that Jane Doe who has the phone number 555-1212 and lives at Mockingbird Lane is an account holder, by placing the tuple,

  <”Jane Doe”, 555-1212, “2727 Mockingbird Lane”>

  into a table called “account-holder”.  This table has three columns, that of account name, phone number, and address.  When we write the above row into said table, then  by definition Jane Doe is in fact an account holder.  On paper we might represent this fact using a well formed statement in a first order logic, something like:

  is-a-account-holder( Jane Doe, 555-1212, 2727 Mockingbird Lane)

  Now lets consider a variation to this example.  Suppose now that we don't know if the said tuple has been written into a row of the table or not.  Rather we plan to query the database to find out if Jane with that phone number and living at that address is an account holder.  In which case the statement is not a fact, but rather it is a proposition.  Propositions differ from facts in that they may be true or false.  We may then run the query in order to evaluate the truth of the proposition.  This process of evaluation creates a correspondence between statements in our first order logic and actions on the database.

  In general if we have a program that we can run to determine the truth of a proposition we may say that we are evaluating the proposition when we are running said program.  Thus in general, as we run a query to find out if a the values given in a tuple has been been written into a row in a table, or not, we are evaluating a corresponding is-a proposition.

  Now lets take this a step further.  Instead let us match only the persons name while leave the other columns in the table blank so they might match anything:

  is-a-account-holder( Jane Doe, _ , _ )

  Because the representation we are using for propositions has order determined arguments, I have used a '_' as a placeholder to signify parts we have left blank.  The statement above is now properly called a predicate.  A predicate is like a proposition in that it may be true or false, but differs from a proposition in that it allows variables to appear in the argument fields.  

  We could have given the blanks variable names without changing the meaning of the predicate.

  is-a-account-holder(Jane Doe, _X, _Y)

  Here I have given the free variables the names,  X and Y respectively.  We keep the '_' in front of the X and Y so there won't be ambiguity.  This makes it clear that we are not expecting Jane's phone number to be literally X or for her address to be literally Y.    This predicate can be read as, “There exists an X and a Y such that is-account-holder(Jane Doe, _X, _Y) is true”.

  A database query may be used to evaluate whether the above predicate is true or false. However, the result of said evaluation is probably not going to satisfy a clerk who is trying to determine if Jane already has an account.  Say  the clerk did the evaluation and found out, yes there is-a account holder by the name of Jane Doe.  He or she is still left to wonder if this is the Jane Doe now sitting at the clerk's desk asking for an account, or a different one.  What the clerk really wants is a list of account holders by the name of Jane Doe so he can ask Jane if any of them might be her.

  The process of finding all the arguments for which a predicate would be satisfied is called unification. We can write the unification problem as:

  is-a-Jane-Doe-account(X, Y) :=  is-a-account-holder(Jane Doe, _X, _Y)

  Our resulting list of Jane Doe account holders is in fact another table.  This new table has one less column, as we already know it is for the name Jane Doe.  In this notation we use a turn style which can be read as 'implied by'.   We might construct the is-a-Jane-Doe-account table by evaluating the predicate to find all pairs where it is true, and putting them in the is-a-Jane-Doe-account table.  We might then ask questions of truth against the facts of the is-a-Jane-Doe-account table using the is-a-Jane-Doe-account predicate, but more likely we will just print the table out so the clerk can use it.

@section{The 'Semantic Grouping' is 'Shape Equivalence' Fallacy}

  Typically we want to say that the tuples were place in a relation  because they share some sort of common semantic equivalence.  Non limiting examples of such semantics would include has-an-account, is-a city or is-a zipcode.

  However, relations are implemented on tables, and tables have enforced shape constraints due to the same type being applied to all rows in a given table.  There exists no theorem in database theory or in logic which says that facts found semantically equivalent in some aspect have the same shape.  In fact such a conclusion is demonstrably false.  Hence, it is merely a happy coincidence that the same shape among related facts is often the case. 

  As an example,  consider that we define a relation based on 'where-to-send-notice'.   We might decide this has the shape of name, street number, and city name, state, country, zip code.   But then we will run into countries that do not break addresses up by states, or have more than one level of breaking up, cities where we must identify the floor of a builds,  or in some places where to send a letter to a person we provide a mini set of directions as the country doesn't name the streets. 

  Another example we ran into early on is that of multiple entries for a given field.  The record morphology does not allow this.  The conventional solution is to use keys into a bridge to another table, but this requires building more complex schemas, and in so doing we lose the ability to 'evaluate' truth by querying a table to see if a corresponding tuple is present.

@section{Justification for Dataplex}

  What we would like is a more general correspondence between predicate logic and database queries, one that does not suffer from the semantic grouping equals shape equivalence fallacy.  This is what the dataplex provides.

  A dataplex is a generic schema that facilitates having tables with semantic groupings without running into shape constraints.

@section{The Dataplex}

  This section describes how to use the dataplex.

  Firstly, we move from the database paradigm to that of the scheme language paradigm.  Tables and other database objects are viewed simply as containers with interface methods.  This is not an unusual thing to do in libraries that interface with databases, though we have abstracted a bit further than most others.    

  The only field type we support is that of text.  All type is created by binding at the program level.  Typically this is through a conversion function.  Fields are found through order, not by name.  We do not have named attributes.  (Names can be bound at the program level.)

  A dataplex provides a generic schema.  There is no explicit encoding of schemas.

  Because we are using scheme we have lists of values instead of tuples.  

  The dataplex interface gives  the user the ability to create two different kinds of containers,  shape containers, and semantic containers.

  @subsection{Containers of values with the same shape, shape relations}

    A new container that holds items of the same shape may be created using a call to the library

    (dataplex:create-shape <name> <length>)

    This creates a named container for values of one level lists of fixed length length.   New values may be placed into the created container by calling insert:

    (shape:insert <name> <value> ...)

    The user must create and name all shapes used, but does not have to insert new values.  Instead he or she may let the system insert new into the shape containers automatically when entering new statements.  (See the next section for statements.)

    The provided value must be a list of precisely length number of members.  Like a database table container a dataplex shape container accepts only fixed length lists one level deep. The members of the value list can be anything which the scheme write function can serialize into text.  

    By contract with the user, we give each shape container semantic meaning.  Placing a value in a shape container name D means that the value is-of-shape D.  So for example if we create a shape container called 'zipcode'  and place the value '78701' in it, then we mean to say that,  '78701 is-of-shape zipcode'.  This statement, '78701 is shape zipcode',  is a fact by virtue of it being found in the container.

    Take as another example, suppose I have a shape of names container.  Suppose that a value is a list with two members, a first name and a second name, <John, Smith>.  Now placing this into the shape table means 'John Smith is-of-shape name'.  Note we are not referring to any particular individual.  And in fact there may not be any statements of fact about a person with this name the database at all.  It may be that no data entry clerk ever met a John Smith while performing his or her duty as a clerk.

    There may not be any foreign keys or other direct ties to other containers inserted into shape containers.  The shape containers are structurally independent.

    The current database implementation of a shape container uses two tables and multiple indexes.  One table is used to hold the values, while the other table is used to bridge references to these values.  The indexes used to facilitate performance in the modes that these tables are used within the dataplex.  However the internal implementation is not important to the theory of their use.

    Shape containers could be organized to hold facts from abstract domains.

  @subsection{Containers of values related by semantics, semantic-relations}

    As this is a first implementation we simplifying the coding problem by implementing a lesser functioning version of the statement container.  We call this the simple statement container, and abbreviate the name for this type of object as 's_statement'.

    A statement container is much like what a user is accustomed to finding in a table, however, it is more flexible in the structure of the data inserted into it.  As a first step we alleviate the constraint of single valued fields.  So for example, a citation record may hold multiple authors.

    A new statement container may be created using a call to the library:

    (dataplex:create-s_statements <name> <shapes>)

    This call accepts a name for a new statements container and a list of shape names.  A new values may be placed into the created container by calling insert:

    (s_statements:insert <name> <value> ...)

    Each value is a list.  Each member in that list must either belong to corresponding shape container that was provided in the shapes argument when the statement container was created, or it must be a list of items of such shape.  So for example, if the first shape is city, and the second is zip code,  then the first item in a value must be a city, or a list of cities, and the second items must be a zip code, or a list of zip codes.

