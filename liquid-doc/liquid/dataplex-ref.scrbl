#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     ))               

@title[#:tag "dataplex-reference"]{dataplex-lib}

@defmodule[liquid/dataplex-lib]


@defproc[(dataplex-lib-init) void?]

  Initializes the dataplex interface.


    (define (dataplex:create-semantic-relation dataplex semantic-relation-name column-shape-relations)

@defproc[
(dataplex:create-semantic-relation [a-dataplex is-dataplex] [name string?] [column-shape-relations (listof is-shape-relation)])
is-semantic-relation
]
  Creates a new semantic relation object in the dataplex with the given name.

@defproc[
  (dataplex:delete-semantic-relation [a-dataplex is-dataplex] [a-semantic-relation is-semantic-relation] ...)
  void?
 ]
@defproc[
  (dataplex:delete-semantic-relation* [a-dataplex is-dataplex] [semantic-relations (listof is-semantic-relation)])
  void?
 ]
  Deletes semantic relations from the dataplex.   


@defproc[
 (dataplex:create-shape-relation [a-dataplex is-dataplex] [name string?] [column-count number?])
 (or/c is-shape-relation 'create-failed)
]
  Creates a new shape-relation object in the dataplex with the given name.

@defproc[(dataplex:delete-shape-relation [a-dataplex is-dataplex] [a-shape-relation is-shape-relation] ...) void?]
@defproc[(dataplex:delete-shape-relation* [a-dataplex is-dataplex] [shape-relations (listof is-shape-relation)]) void?]

   Deletes the shape relations from the dataplex.

@defproc[(db:dataplexes [rx regexp? #f]) (listof is-dataplex)]

  Returns a list of dataplexes that have names matching @racket[rx].  If @racket[rx] is not
  provided, or it is @racket[#f], returns a list of all dataplexes.

@defproc[(db:is-dataplex-name [name string?] ...) (or/c boolean? (listof boolean?))]
@defproc[(db:is-dataplex-name* [names (listof string?)]) (or/c boolean? (listof boolean?))]

  @racket[#t] if the provided name is a dataplex, otherwise @racket[#f].  If multiple names are provided the
  function maps over them.

@defproc[(db:find-dataplex [name string?] ...) (or/c #f is-dataplex (listof (or/c #f is-dataplex)))]
@defproc[(db:find-dataplex [names (listof string?)]) (or/c #f is-dataplex (listof (or/c #f is-dataplex)))]

  If a single name is provided and there is a corresponding dataplex with that name, the dataplex
  object is returned, otherwise #f is returned.  If multiple names are provided the function
  maps over them.

@defproc[(db:is-dataplex [object any] ...) (or/c boolean? (listof boolean?))]
@defproc[(db:is-dataplex* [objects (listof any)]) (or/c boolean? (listof boolean?))]

   If a single object is provided, returns #t if that object is a dataplex, otherwise #f.
   If multiple objects are provided the function maps over them.

@defproc[(db:create-dataplex [name string?] ...) (or/c 'exists is-dataplex (listof (or/c 'exists is-dataplex)))]
@defproc[(db:create-dataplex* [names (listof string?)]) (or/c 'exists is-dataplex (listof (or/c 'exists is-dataplex)))]

   If a single name is provided and that name is the name of a dataplex, the literal @racket['exists] is returned,
   otherwise a new dataplex object is returned.  If mulitple names are provided the function maps over them.

@defproc[(db:delete-dataplex [a-dataplex is-dataplex] ...) void?]
@defproc[(db:delete-dataplex* [dataplexes (listof is-dataplex)]) void?]

   Deletes the specified dataplexes. I wonder what happens if specified object(s) are not really dataplexes
   (sorry this is version 0.1 after all ;-)


@defproc[(shape-relation:insert [shape-relation is-shape-relation] [value row?]) [id number?]]

    If @racket[value] is not already in @racket[shape-relation] then @racket[shape-relation:insert] places
    it in @racket[shape-relation].  In either case, this function returns the value id.


@defproc[
  (shape-relation:delete [a-shape-relation is-shape-relation] [a-pattern is-pattern])
  (or/c list? 'no-error)
  ]

  If no semmantic relation is making use of a row in the shape relation, then that row is said to be
  an orphan.

  This routine deletes orphaned rows in @racket[a-shape-relation] that match  @racket[a-pattern].

  The list returned is for citings of non-orphaned rows, however we do not officially expose the
  citings tables on the interface, so we say that a returned list means there was an error.  If
  there is no error then the literal 'no-error is returned.  We might rework the return value
  for this function in a later release.

@defproc[
  (shape-relation:match [a-shape-relation is-shape-relation] [a-pattern is-pattern])
  (listof row?)
]
  Returns a list of roows from @racket[a-shape-relation] that match @racket[a-pattern].


@defproc[
  (semantic-relation:insert 
    [a-dataplex is-dataplex]
    [semantic-relation is-semantic-relation]
    [value row?])
  (or/c 'no-value 'value-length 'no-error)
  ]

  Inserts @racket[value] into the specified @racket[semantic-relation] belonging to the @racket[a-dataplex].

  It is legal to have multiple values in a given column.

  @racket['no-value] is returned when @racket[a-value] is null.  This isn't really needed.

  @racket['value-length] is returned if a-value doesn't have the same number of columns as the
  semantic relation was declared with.  Empty fields is not an error, so this should probably
  be changed to only check for too many column values.

@defproc[
  (semantic-relation:match [a-dataplex is-dataplex] [a-semantic-relation is-semantic-relation] [a-pattern is-pattern])
  [rows (listof row?)]
]

   

      ;semantic-relation:delete
