#lang scribble/manual
@(require scribble/manual scribble/struct scribble/xref
          (for-label scheme/base
                     scheme/contract
                     liquid/db-lib
                     ))

@title{LIQUID -- Framework for Developing Deep Web Search Programs and Servers}

@author["Thomas Walker Lynch"]

@table-of-contents[]

@include-section{db-lib.scrbl}
@include-section{http-server.scrbl}
@include-section{query-parser.scrbl}
@include-section{realtime.scrbl}
@include-section{tokens.scrbl}

