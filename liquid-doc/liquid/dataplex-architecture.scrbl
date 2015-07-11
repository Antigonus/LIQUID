#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:style '(toc)]{Dataplex}


A table has rows and columns.  A given row and a given column uniquely identify a field.

A relation is an abstrction of the table concept noting that structure of the table
implies that values in a row are related.

The semantic relations and shape relations of the dataplex, as described in the concept
section, are implemented on database tables.  Semantic relations in the dataplex differ
from conventional relations in that each field may hold zero or more values, instead of
just zero or one value.

In our dataplex implementation we have something called a semantic table which has the same number
of columns as the semantic relation.  Each row in the conceptual semantic relation has a
corresponding row in the semantic table.  However, in contrast, a field in a semantic table
holds a single number, a so called sm-id.  We use the sm-id from the semantic table to
construct the zero or more values for the semantic relation.

Shape tables are empty when created. The library automatically prepends to each newly
inserted row a unique 'sp-id', which is a numeric value.  Hence a shape table differs from
a shape relation only in that there is an additional first column that holds a row 'sp-id'.

Each shape table has a companion bridge table. This companion bridge table has two columns,
the first is for sm-ids, the second for sp-ids.  Hence, each row consists of two fields.  The
table is doubly indexed on each column, so lookup is fast going in either direction.

In the current implementation each column in the semantic table is placed into
correspondence with one shape table.  Thus, also with one bridge table.

We begin a lookup for the values belonging to a semantic relation field, by getting the
sm-id from the semantic table.  We then lookup this sm-id in the corresponding bridge table,
yielding a list of sp-ids.  If no sp-ids are found then the field is empty.  Otherwise we
lookup the sp-ids in the companion shape table to create a list of one or more values for
the field.

In the future it may make more sense to have one master bridge table so that columns in the
semantic table need not be typed.  Such a master bridge table would need an additional column
to identify the shape table.

Our dataplex is implemented over our db-lib.  As such we have already eliminated type from
the tables.  So the utility here is in the connected structure.  For example, with this
structure is possible to ask questions such as 'where are all the citations for this phone number?'








