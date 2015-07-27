2596
((3) 0 () 2 ((q lib "liquid/dataplex-lib.rkt") (q lib "liquid/db-lib.rkt")) () (h ! (equal) ((c def c (c (? . 0) q dataplex:delete-shape-relation)) q (2807 . 5)) ((c def c (c (? . 0) q shape-relation:delete)) q (4662 . 6)) ((c def c (c (? . 1) q db:alloc-name)) q (70 . 2)) ((c def c (c (? . 1) q table:match)) q (1457 . 5)) ((c def c (c (? . 0) q db:is-dataplex*)) q (3906 . 3)) ((c def c (c (? . 1) q table:insert*)) q (1338 . 4)) ((c def c (c (? . 1) q db:tables)) q (848 . 3)) ((c def c (c (? . 1) q db:dealloc-number)) q (216 . 3)) ((c def c (c (? . 1) q db:delete-keyspace)) q (464 . 3)) ((c def c (c (? . 0) q dataplex:delete-semantic-relation)) q (2055 . 6)) ((c def c (c (? . 0) q db:dataplexes)) q (3239 . 3)) ((c def c (c (? . 1) q keyspace:alloc-number)) q (925 . 3)) ((c def c (c (? . 1) q db:alloc-number)) q (175 . 2)) ((c def c (c (? . 0) q db:is-dataplex-name*)) q (3431 . 4)) ((c def c (c (? . 0) q semantic-relation:insert)) q (5054 . 8)) ((c def c (c (? . 0) q dataplex:delete-shape-relation*)) q (3023 . 5)) ((c def c (c (? . 0) q db:find-dataplex)) q (3672 . 4) (3546 . 4)) ((c def c (c (? . 0) q db:delete-dataplex*)) q (4389 . 3)) ((c def c (c (? . 0) q dataplex:delete-semantic-relation*)) q (2285 . 5)) ((c def c (c (? . 1) q db:create-table)) q (355 . 4)) ((c def c (c (? . 1) q db:create-keyspace)) q (278 . 3)) ((c def c (c (? . 1) q table:delete)) q (1114 . 4)) ((c form c (c (? . 1) q as-transaction)) q (0 . 2)) ((c def c (c (? . 1) q db:dealloc-name)) q (109 . 3)) ((c def c (c (? . 0) q dataplex-lib-init)) q (1678 . 2)) ((c def c (c (? . 1) q db:is-keyspace)) q (715 . 3)) ((c def c (c (? . 0) q db:create-dataplex*)) q (4153 . 4)) ((c def c (c (? . 0) q dataplex:create-shape-relation)) q (2519 . 8)) ((c def c (c (? . 1) q keyspace:dealloc-number)) q (1007 . 4)) ((c def c (c (? . 0) q db:delete-dataplex)) q (4299 . 3)) ((c def c (c (? . 1) q table:insert)) q (1227 . 4)) ((c def c (c (? . 1) q db-lib-init)) q (35 . 2)) ((c def c (c (? . 1) q db:delete-table)) q (541 . 3)) ((c def c (c (? . 1) q db:delete-table*)) q (624 . 3)) ((c def c (c (? . 0) q semantic-relation:match)) q (5354 . 8)) ((c def c (c (? . 0) q shape-relation:match)) q (4862 . 5)) ((c def c (c (? . 0) q db:create-dataplex)) q (4015 . 4)) ((c def c (c (? . 1) q db:is-table)) q (783 . 3)) ((c def c (c (? . 0) q dataplex:create-semantic-relation)) q (1719 . 8)) ((c form c (c (? . 1) q with-db)) q (1641 . 2)) ((c def c (c (? . 0) q db:is-dataplex)) q (3805 . 3)) ((c def c (c (? . 0) q db:is-dataplex-name)) q (3324 . 4)) ((c def c (c (? . 0) q shape-relation:insert)) q (4485 . 5))))
syntax
(as-transaction body ...)
procedure
(db-lib-init) -> void?
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
(with-db db-name body ...)
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
