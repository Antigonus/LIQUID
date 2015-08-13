#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "token-reference"]{token}

@defmodule[liquid/token]


@section{typed-list}

@defproc[(typed-list-make [type symbol?] any ...) list?]
@defproc[(typed-list-make* [type symbol?] (list of any)) list?]

  Makes a typed list of the given type.

@defproc[(typed-list-is-well-formed list? [type-enumeration list? '()]) bool?]

  A well formed typed-list is a list that starts with a symbol representing the type.
  Optionally the type symbol is checked to see if it belongs to a type-enumeration.


@defproc[(type typed-list?) symbol?]

  Returns the type of the typed list.

@defproc[(value typed-list?) symbol?]

  Drops the type symbol, returns the rest.

@defproc[(is-of-type typed-list? symbol?) bool?]

  Returns true if the typed list is of the given type, otherwise returns false.


@section{typed-list}

