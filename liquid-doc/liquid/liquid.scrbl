#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{Racket Library Support for LIQUID}

@author["Thomas Walker Lynch"]

This package started as a framework for implementing experimental deep websearch
algorithms created at Birkeck College, under the LIQUID project (see
http://gtr.rcuk.ac.uk/project/4D15285C-4599-4875-849B-19C4A7E21AE9).  That work resulted
in the liquid db-lib, the dataplex, a parser tailored to query languages, and the
webi approach to scraping.

I have also used this package for additional experimental code. 

I added a simplified webserver which was originally intended to support the browswer as a
window for the interactive application of a startup project company  project (see
http://www.slideshare.net/thomaslynch3979/presentation-31884619).  I thought that perhaps
some of the websearch algorithms might evolve into applications.   This code continues to
be developed.  I'm still not sure if it should be here ;-)

I have recently added the object model intended to be used on the Turing Completely
  Architecture.  It has been wonderful to see that approach in action and proven viable.

I've also employed techniques of using functions with multiple control exits.  This has
worked out even better than I had imagined.  I hope you find it the technique useful also.

I have developed the query parser into a more general parser so as to try out some concepts
on stable parsing in the presence of errors.

This documentation is broken into section of:  concepts, architecture, reference, and examples.
The concepts sections discuss theory.  The architecture secton discuss the apporach used for getting
the theory into code.  The reference section gives a list of functions that are available, and
the operand types.  Currently there is one big example for the deep websearch, but alas it remains
documented through its code.  There are many smaller examples and tests spread throughout the
source code.


@table-of-contents[]
@include-section{concepts.scrbl}
@include-section{architecture.scrbl}
@include-section{reference.scrbl}
@include-section{examples.scrbl}
@include-section{running-tests.scrbl}

@index-section[]

