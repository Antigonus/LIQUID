#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:style '(toc)]{Dataplex}


Fundamentally a table has rows and columns.  A given row and column uniquely identifies a field.

A relation is an abstraction of the table concept that acknowledges that the values placed in
a row have logocial relationship to each other.  This logical relationship is invarient across the
rows, though the data from row to row changes.

A shape table has one or more columns of fixed type, i.e. this is a conventional table.
In a conventional implemenation such a type would be a type supported by the database.  In
our racket implementation it is any racket type.  The rows in a shape table pack well
because they all have the same data type and thus the same format.  

In the dataplex paradigm the values in a shape table come from a low level abstract
domain.

The library automatically prepends to each newly inserted shape relation row a unique
'sp-id', which is a numeric value.  Hence a shape table differs from a shape relation only
in that there is an additional first column that holds a row 'sp-id'.

Each shape table has a companion bridge table. This companion bridge table has two
columns, the first is for sm-ids (explained below), the second for sp-ids.  Hence, each
row in the bridge table consists of two fields.  The table is indexed on both
columns, so lookup is fast going in either direction.

A semantic relation consists of two single column tables.  The first table is a list of
shape relations.  These are the column types.  We say that a value in a relation, or a
column has a 'given shape relation'.  There is no actual data table with multiple columns.
Instead the second table is a list of 'sm-ids', where each sm-id may be used to
reconstruct the conceputal row by using the bridge tables noted above.  We looking up the
sm-id in each of the given shape relation's bridge table.  Hence if the table has n
columns, then there will be n lookups one for each given shape relation corresponding to
each column.

To perform a pattern match lookup on a semantic relation, we first lookup each value from
the pattern in the given shape relation for the column.  The sp-ids are then run through
the bridege table to get sm-ids.  This gives a list of sm-ids per column.  We then set
intersect the lists of sm-ids, and this gives us a list of sm-ids for the row matches.  We
can then reconstruct the rows corresponding to these sm-ids if a value based result is
desired.

In the future it may make more sense to have one master bridge table so that columns in the
semantic table need not be typed.  Such a master bridge table would need an additional column
to identify the shape table.

Our dataplex is implemented over our db-lib.  As such we have already eliminated type from
the tables.  So the utility here is in the connected structure.  For example, with this
structure it is possible to ask questions such as 'where are all the citations for this
phone number?'








