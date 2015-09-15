#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title[#:style '(toc)]{Architecture}

How code has been organized to capture the concepts behind the libraries.


@local-table-of-contents[]
@include-section{dataplex-architecture.scrbl}
@include-section{multiple-continuations-architecture.scrbl}
