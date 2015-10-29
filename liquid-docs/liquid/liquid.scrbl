#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{Racket Library Support for LIQUID}

@author["Thomas Walker Lynch"]

Novel contributions in this package include support language extensions for sequences, multiple
execuion path exit functions, TCA objects, stream parsing, and the databplex persistent
storage over a database.

The genesis of the code is described in the about section.

The concepts sections discusses theory for the various libraries.  

The architecture secton discuss the block partitioning and approach to the implementation.

The reference section defines the interfaces.

The example section is about the programs found in the example directory, though this section
has not yet been written -- see the source code ;-)


@table-of-contents[]
@include-section{about.scrbl}
@include-section{concepts.scrbl}
@include-section{architecture.scrbl}
@include-section{reference.scrbl}
@include-section{examples.scrbl}
@include-section{todo.scrbl}

@index-section[]

