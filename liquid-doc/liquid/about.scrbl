#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     ))

@title{About LIQUID}


This package started as a framework for implementing experimental deep websearch
algorithms created at Birkeck College, under the LIQUID project (see
http://gtr.rcuk.ac.uk/project/4D15285C-4599-4875-849B-19C4A7E21AE9).  That work resulted
in the liquid db-lib, the dataplex, a parser tailored to query languages, and the
webi approach to scraping.  This portion of the library is inspired by and draws from
the work of Andrea Cali and his collequges at Birkbeck.

I have also used this package for additional experimental code.

I added my simplified webserver which was originally intended to support the browswer as a
window for the interactive application of a startup project company project (see
http://www.slideshare.net/thomaslynch3979/presentation-31884619).  I thought that perhaps
some of the websearch algorithms might evolve into deep search applications.  This code
continues to be developed. 

I have added my object model intended to be used on the Turing Completely
Architecture, TCA, from another startup.  The TCA was intended to be hardware, but it has
been wonderful to see this approach in action and proven viable here as a library.  This
includes the concepts of function continuations and data continuations for arbitrary long
data, and type defined as a shared object.  (At this time the TCA continued number type and
precision analysis for reliable numerics are not yet integrated.)

I introduced here a stream parser with features for stability in presence of errors. This
is both an example of TCA techniques and useful for creating deep websearch applications.
I've included example of a query language parser and an html parser.



