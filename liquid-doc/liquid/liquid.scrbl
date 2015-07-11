#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{Racket Library Support for LIQUID}

@author["Thomas Walker Lynch"]

Useful racket functions for developing experimental algorithms for deep web search.  The
dataplex, for example, is an implementation of Dr. Andrea Cali's definition of abstract
data type in databases.  This work was funded in part by Birckbeck College Univeristy of
London as part of the LIQUID project.

@table-of-contents[]
@include-section{LIQUID.scrbl}
@include-section{concepts.scrbl}
@include-section{architecture.scrbl}
@include-section{reference.scrbl}
@include-section{examples.scrbl}

@index-section[]

