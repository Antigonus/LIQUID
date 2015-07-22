#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{Racket Library Support for LIQUID}

@author["Thomas Walker Lynch"]

This package is a framework, i.e. a bunch of functions, for integrating into programs or
custom web servers the deep web search algorithms that were created in the LIQUID research
project conducted by researchers at Birkbeck College University of London. See the
section "About Project LIQUID" for more information.


@table-of-contents[]
@include-section{LIQUID.scrbl}
@include-section{concepts.scrbl}
@include-section{architecture.scrbl}
@include-section{reference.scrbl}
@include-section{examples.scrbl}
@include-section[running-tests.scrbl}

@index-section[]

