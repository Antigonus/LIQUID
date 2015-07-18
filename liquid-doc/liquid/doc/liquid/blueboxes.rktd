2553
((3) 0 () 2 ((q lib "liquid/db-lib.rkt") (q lib "liquid/dataplex-lib.rkt")) () (h ! (equal) ((c def c (c (? . 0) q db:create-keyspace)) q (243 . 3)) ((c form c (c (? . 0) q with-connection)) q (1606 . 2)) ((c def c (c (? . 1) q dataplex:create-semantic-relation)) q (1707 . 8)) ((c def c (c (? . 1) q dataplex:delete-shape-relation*)) q (3011 . 5)) ((c def c (c (? . 0) q db:alloc-number)) q (140 . 2)) ((c def c (c (? . 0) q db:tables)) q (813 . 3)) ((c def c (c (? . 0) q table:delete)) q (1079 . 4)) ((c def c (c (? . 1) q db:find-dataplex)) q (3660 . 4) (3534 . 4)) ((c def c (c (? . 1) q dataplex-lib-init)) q (1666 . 2)) ((c def c (c (? . 0) q db:delete-keyspace)) q (429 . 3)) ((c def c (c (? . 0) q db:is-keyspace)) q (680 . 3)) ((c def c (c (? . 1) q db:create-dataplex*)) q (4141 . 4)) ((c def c (c (? . 0) q keyspace:alloc-number)) q (890 . 3)) ((c def c (c (? . 1) q dataplex:delete-shape-relation)) q (2795 . 5)) ((c def c (c (? . 0) q db:is-table)) q (748 . 3)) ((c def c (c (? . 1) q shape-relation:delete)) q (4650 . 6)) ((c def c (c (? . 1) q db:dataplexes)) q (3227 . 3)) ((c form c (c (? . 0) q as-transaction)) q (0 . 2)) ((c def c (c (? . 1) q dataplex:create-shape-relation)) q (2507 . 8)) ((c def c (c (? . 0) q db:delete-table)) q (506 . 3)) ((c def c (c (? . 1) q db:is-dataplex-name*)) q (3419 . 4)) ((c def c (c (? . 0) q db:dealloc-number)) q (181 . 3)) ((c def c (c (? . 0) q table:insert*)) q (1303 . 4)) ((c def c (c (? . 0) q table:match)) q (1422 . 5)) ((c def c (c (? . 1) q db:delete-dataplex*)) q (4377 . 3)) ((c def c (c (? . 1) q dataplex:delete-semantic-relation)) q (2043 . 6)) ((c def c (c (? . 0) q db:delete-table*)) q (589 . 3)) ((c def c (c (? . 1) q semantic-relation:insert)) q (5042 . 8)) ((c def c (c (? . 1) q db:is-dataplex)) q (3793 . 3)) ((c def c (c (? . 1) q dataplex:delete-semantic-relation*)) q (2273 . 5)) ((c def c (c (? . 1) q db:create-dataplex)) q (4003 . 4)) ((c def c (c (? . 0) q table:insert)) q (1192 . 4)) ((c def c (c (? . 0) q keyspace:dealloc-number)) q (972 . 4)) ((c def c (c (? . 1) q shape-relation:insert)) q (4473 . 5)) ((c def c (c (? . 1) q semantic-relation:match)) q (5342 . 8)) ((c def c (c (? . 1) q shape-relation:match)) q (4850 . 5)) ((c def c (c (? . 1) q db:delete-dataplex)) q (4287 . 3)) ((c def c (c (? . 0) q db:dealloc-name)) q (74 . 3)) ((c def c (c (? . 0) q db:create-table)) q (320 . 4)) ((c def c (c (? . 1) q db:is-dataplex*)) q (3894 . 3)) ((c def c (c (? . 0) q db:alloc-name)) q (35 . 2)) ((c def c (c (? . 1) q db:is-dataplex-name)) q (3312 . 4))))
syntax
(as-transaction body ...)
procedure
(db:alloc-name) -> string?
procedure
(db:dealloc-name name) -> void?
  name : string?
procedure
(db:alloc-number) -> number?
procedure
(db:dealloc-number n) -> void?
  n : number?
procedure
(db:create-keyspace keyspace) -> void?
  keyspace : string?
procedure
(db:create-table name column-count) -> void?
  name : string?
  column-count : number?
procedure
(db:delete-keyspace keyspace) -> void?
  keyspace : string?
procedure
(db:delete-table table-name ...) -> void?
  table-name : string?
procedure
(db:delete-table* table-names) -> void?
  table-names : (listof string?)
procedure
(db:is-keyspace name) -> boolean?
  name : string?
procedure
(db:is-table name) -> boolean?
  name : string?
procedure
(db:tables [rx]) -> (listof string?)
  rx : regexp? = #f
procedure
(keyspace:alloc-number keyspace) -> number?
  keyspace : string?
procedure
(keyspace:dealloc-number keyspace key) -> void?
  keyspace : string?
  key : number?
procedure
(table:delete table-name a-pattern) -> void?
  table-name : string?
  a-pattern : pattern?
procedure
(table:insert table-name row ...) -> void?
  table-name : string?
  row : (listof any)
procedure
(table:insert* table-name rows) -> void?
  table-name : string?
  rows : (listof (listof any))
procedure
(table:match table-name a-pattern [a-filter]) -> (listof row?)
  table-name : string?
  a-pattern : pattern?
  a-filter : (row? . -> . row?) = identity
syntax
(with-connection (connection-info ...) body ...)
procedure
(dataplex-lib-init) -> void?
procedure
(dataplex:create-semantic-relation a-dataplex              
                                   name                    
                                   column-shape-relations) 
 -> is-semantic-relation
  a-dataplex : is-dataplex
  name : string?
  column-shape-relations : (listof is-shape-relation)
procedure
(dataplex:delete-semantic-relation a-dataplex               
                                   a-semantic-relation ...) 
 -> void?
  a-dataplex : is-dataplex
  a-semantic-relation : is-semantic-relation
procedure
(dataplex:delete-semantic-relation* a-dataplex              
                                    semantic-relations) -> void?
  a-dataplex : is-dataplex
  semantic-relations : (listof is-semantic-relation)
procedure
(dataplex:create-shape-relation a-dataplex    
                                name          
                                column-count) 
 -> (or/c is-shape-relation 'create-failed)
  a-dataplex : is-dataplex
  name : string?
  column-count : number?
procedure
(dataplex:delete-shape-relation a-dataplex                
                                a-shape-relation ...) -> void?
  a-dataplex : is-dataplex
  a-shape-relation : is-shape-relation
procedure
(dataplex:delete-shape-relation* a-dataplex           
                                 shape-relations) -> void?
  a-dataplex : is-dataplex
  shape-relations : (listof is-shape-relation)
procedure
(db:dataplexes [rx]) -> (listof is-dataplex)
  rx : regexp? = #f
procedure
(db:is-dataplex-name name ...)
 -> (or/c boolean? (listof boolean?))
  name : string?
procedure
(db:is-dataplex-name* names)
 -> (or/c boolean? (listof boolean?))
  names : (listof string?)
procedure
(db:find-dataplex name ...)
 -> (or/c #f is-dataplex (listof (or/c #f is-dataplex)))
  name : string?
procedure
(db:find-dataplex names)
 -> (or/c #f is-dataplex (listof (or/c #f is-dataplex)))
  names : (listof string?)
procedure
(db:is-dataplex object ...) -> (or/c boolean? (listof boolean?))
  object : any
procedure
(db:is-dataplex* objects) -> (or/c boolean? (listof boolean?))
  objects : (listof any)
procedure
(db:create-dataplex name ...)
 -> (or/c 'exists is-dataplex (listof (or/c 'exists is-dataplex)))
  name : string?
procedure
(db:create-dataplex* names)
 -> (or/c 'exists is-dataplex (listof (or/c 'exists is-dataplex)))
  names : (listof string?)
procedure
(db:delete-dataplex a-dataplex ...) -> void?
  a-dataplex : is-dataplex
procedure
(db:delete-dataplex* dataplexes) -> void?
  dataplexes : (listof is-dataplex)
procedure
(shape-relation:insert shape-relation     
                       value)         -> [id number?]
  shape-relation : is-shape-relation
  value : row?
procedure
(shape-relation:delete a-shape-relation 
                       a-pattern)       
 -> (or/c list? 'no-error)
  a-shape-relation : is-shape-relation
  a-pattern : is-pattern
procedure
(shape-relation:match a-shape-relation     
                      a-pattern)       -> (listof row?)
  a-shape-relation : is-shape-relation
  a-pattern : is-pattern
procedure
(semantic-relation:insert a-dataplex        
                          semantic-relation 
                          value)            
 -> (or/c 'no-value 'value-length 'no-error)
  a-dataplex : is-dataplex
  semantic-relation : is-semantic-relation
  value : row?
procedure
(semantic-relation:match a-dataplex          
                         a-semantic-relation 
                         a-pattern)          
 -> [rows (listof row?)]
  a-dataplex : is-dataplex
  a-semantic-relation : is-semantic-relation
  a-pattern : is-pattern
