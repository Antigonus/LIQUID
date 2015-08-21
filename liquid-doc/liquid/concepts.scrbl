#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title[#:style '(toc)]{Concepts}

The guiding concepts behind each library.  The sections are given in order of complexity/dependencies rather
than alphabetic order.  This facilitates top to bottom reading.

@local-table-of-contents[]
@include-section{db-lib-concepts.scrbl}
@include-section{dataplex-concepts.scrbl}
@include-section{node-concepts.scrbl}
@include-section{parser-concepts.scrbl}
