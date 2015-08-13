#|
  db-lib

    generic tables interface with db and transaction contexts


  -- 
    in sql queries be sure to double quote table names, and to single quote values

    should create some more complex sql queries, for example, to delete a table only
    if it exists as we are checking for existence first in the dataplex so as to avoid
    the exception on deleting a non-existent table.  perhaps maybe catch the exception here
    instead - but in most programming environments exceptions should not be made part
    of the program logic.

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require db)
  (require "misc-lib.rkt")

;;--------------------------------------------------------------------------------
;; db-lib as an object shared state
;;
   ;; provided to others

  (define current-test-db (make-parameter "db-lib-test"))
  (define current-example-db (make-parameter "db-lib-example"))
  (define current-working-db (make-parameter "mordecai"))

  ;; private
  (define current-db-log (make-parameter #f)) ; set to true for logging
  (define current-db-connection (make-parameter #f))
  (define current-db-context-id (make-parameter #f))
  (define trans-locks (make-hash)) ; used for transactions to block between threads
  (define current-db-allocator-semaphore (make-parameter (make-semaphore 1))) ; used by key allocator

;;--------------------------------------------------------------------------------
;; used by with-db to establish a connection
;;
;;   this will surely become more sophisiticated in later versions ...
;;
  (define (db-connect db-name)
    (let*(
           [user-name (getenv "USER")]
           )
      (postgresql-connect #:database db-name  #:user user-name #:socket 'guess)
    ))


;;--------------------------------------------------------------------------------
;; database context
;;
;; input:  a database name, a body of code
;; output: result of last body expression or throws an exception    
;;
;; database must already exist
;;    
;; nested transaction blocks within a db-context is fine, simultaneous transaction
;; blocks can not be allowed (as the transactions would alias), To solve this problem
;; I put a semaphore on a parameter, each transaction block then has its own semaphore
;; context.
;;
;;  caused a lot of headaches -- the (my-parm new-value)  write does not cross a thread boundary
;;  so had to move the semaphore and its owner to a global hash table
;;
  ;; the owned-semaphore data type
  (define (owned-semaphore-owner t) (car t))
  (define (owned-semaphore-semaphore t) (cadr t))
  (define (owned-semaphore o s) (list o s))

  (define-syntax (with-db stx)
    (syntax-case stx ()
      [(with-db db-name body ...)
        #`(parameterize(
                         [current-db-context-id (unique-to-session-number)]
                         [current-db-connection (db-connect db-name)]
                         )
            (hash-set! trans-locks (current-db-context-id) (owned-semaphore #f (make-semaphore 1)))
            (begin-always
              (begin body ...)
              (begin
                ;; if the user doesn't want the connection pulled out from under a thread, needs to put
                ;; thread wait in body  (a thread might not even use the connection, we can't know)
                (disconnect (current-db-connection))
                (hash-remove! trans-locks (current-db-context-id))
                (unique-to-session-number-dealloc (current-db-context-id))
                )))
        ]
      ))

;;--------------------------------------------------------------------------------
;;  initialize the database
;;
;;
  (define (db-lib-init-1)
    (cond
      [(not (db:is-table "table_allocator_overhead")) 
        (allocator-create)
        (db:create-keyspace "unique_to_db") ; puts row in allocator_overhead for a db global unique count
        ]
    ))

  (define (db-lib-init)
    (cond [(current-db-log) (log "db-init")])
    (with-handlers
      (
        [(λ(v) #t) ; this handler catches anything
          (λ(v)
            (log (string-append "exception in db-lib-init: " (->string v)))
            v
            )
          ]
        )
      (with-db (current-test-db)
        (db-lib-init-1)
        )
      (with-db (current-working-db)
        (db-lib-init-1)
        )
      #f  ;; false - no errors exist
      ))

  (define (db-lib-init-test-0) (db-lib-init))
  (test-hook db-lib-init-test-0)
  


;;--------------------------------------------------------------------------------
;; helper function, gasket for the racket/db lib 
;;
;;    adds logging and the db connection to library db commands
;;
;;  input: λ command, list of arguments
;;  output: result of applying command
;;
  (define (db-gasket* command args)
    (let(
          [the-query (cons (current-db-connection) args)]
          )
      (cond
        [(current-db-log) (log the-query)]
        )
      (apply command the-query)
      ))
   (define (db-gasket command . args) (db-gasket* command args))

;;--------------------------------------------------------------------------------
;;  run a list of one or more queries
;;
;;   input: query strings for query-exec
;;   output: void as query-exec returns nothing
;;
;;   the query-exec library routine enforces having one query at a time,
;;   ';'  is not allowed in the query.  So we instead call query-exec on each
;;   member of a list. One may want to wrap this call in (as-transaction ...)
;;   
  ;; no results will be returned from query
  (define (sql:exec . query) (sql:exec* query))
  (define (sql:exec* query)
    (cond
      [(pair? query)  
        (for-each
          (λ(e)
            (cond
              [(null? e) (void)]
              [(pair? e) (db-gasket* query-exec e)]
              [else (db-gasket query-exec e)]
              )
            ) 
          query)
        ]
      [else #f]
      ))

;;--------------------------------------------------------------------------------
;; access to underlying lib routines
;;
  ;; result is exactly a single value
  (define (sql:value . arg) (sql:value* arg))
  (define (sql:value* arg) (db-gasket* query-value arg))

  ;; result is exactly a single value, no value and #f, or exception on multiple values
  (define (sql:maybe-value . arg) (sql:maybe-value* arg))
  (define (sql:maybe-value* arg) (db-gasket* query-maybe-value arg))

  ;; result is exactly one row, or there is an exception
  (define (sql:row . arg) (sql:row* arg))
  (define (sql:row* arg) (db-gasket* query-row arg))

  ;; result is zero or more rows
  (define (sql:rows . arg) (sql:rows* arg))
  (define (sql:rows* arg) (db-gasket* query-rows arg))

  ;; zero or more rows where the result has exactly one column
  (define (sql:list . arg) (sql:list* arg))
  (define (sql:list* arg) (db-gasket* query-list arg))


;;--------------------------------------------------------------------------------
;; as-transaction helpers
;;
  (define (transaction:begin)    (start-transaction (current-db-connection)))
  (define (transaction:commit)   (commit-transaction  (current-db-connection)))
  (define (transaction:rollback) (rollback-transaction  (current-db-connection)))

;;--------------------------------------------------------------------------------
;;  a transaction environment
;;
;;  (as-transaction body ...)
;;
;;  Transactions on the smae connection, but different threads, will block each other   
;; 
  (define-syntax (as-transaction stx)
    (syntax-case stx ()
      [(as-transaction body ...)
        #`(let*(
                 [an-owned-semaphore (hash-ref trans-locks (current-db-context-id))]
                 [owner (owned-semaphore-owner an-owned-semaphore)]
                 [semaphore (owned-semaphore-semaphore an-owned-semaphore)]
                )
            (cond
            [(equal? owner (current-thread)) ; no race possible as nothing else can change to our thread
              (begin-always
                (begin0 (begin (transaction:begin) body ...) (transaction:commit))
                (transaction:rollback))
              ]

            [else
              (with-semaphore semaphore
                (hash-set! trans-locks (current-db-context-id) (owned-semaphore (current-thread) semaphore))
                (begin-always
                  (begin0 (begin (transaction:begin) body ...) (transaction:commit))
                  (begin 
                    (transaction:rollback)
                    (hash-set! trans-locks (current-db-context-id) (owned-semaphore #f semaphore))
                    )))
              ]))]))



  (define (sql-test-0)
    (with-db (current-test-db)
      (as-transaction 
        (sql:exec  "create temporary table some_named_numbers (n integer, d varchar(20))")
        (sql:exec  "insert into some_named_numbers values (0, 'zero')")
        (sql:exec  "insert into some_named_numbers values (2, 'two')")
        (sql:exec  "insert into some_named_numbers values (1, 'one')")
        (sql:exec '("insert into some_named_numbers values ($1, $2)" 3 "three"))
        (let(
              [results
                (list
                  (equal?
                    (sql:rows "select n, d from some_named_numbers where n % 2 = 0")
                    '(#(0 "zero") #(2 "two"))
                    )
                  (equal?
                    (sql:row "select * from some_named_numbers where n = 0")
                    '#(0 "zero")
                    )
                  (equal?
                    (sql:list "select d from some_named_numbers order by n")
                    '("zero" "one" "two" "three")
                    )
                  (equal?
                    (sql:value "select count(*) from some_named_numbers")
                    4
                    )
                  (eqv?
                    (sql:maybe-value "select d from some_named_numbers where n = 5")
                    #f
                    )
                  )
                ]
              )
          (sql:exec "drop table some_named_numbers")
          ;(pretty-print results)(newline)
          (andmap (λ(e)e) results)
          ))))
    (test-hook sql-test-0)


  ;; basic scoping working?
  ;;
    (define (as-transaction-test-0)
      (with-db (current-test-db)
        (define x 22)
        (as-transaction
          (set! x 10)
          (void)
          )
        (let(
              [y (as-transaction 17)]
              )
          (and (= x 10) (= y 17))
        )))
    (test-hook as-transaction-test-0)

  ;;  exception occurs in (sql:row ...) because two rows are returned -- causes transaction
  ;;  to be canceled
  ;;
    (define (as-transaction-test-1)
      (with-db (current-test-db)
        (with-handlers
          (
            [(λ(v) #t) ; this handler catches anything
              (λ(v)
                (let(
                      [tables (sql:list "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")]
                      )
                  ;; table created before the transaction block exists, data put in after does not
                  (begin0
                    (and 
                      (member "some_named_numbers" tables)
                      (not  (sql:maybe-value "select n from some_named_numbers where d = 'zero'") )
                      )
                    (sql:exec "drop table some_named_numbers")
                    )))
              ]
            )
          (sql:exec  "create table some_named_numbers (n integer, d varchar(20))")
          (as-transaction 
            (sql:exec  "insert into some_named_numbers values (0, 'zero')")
            (sql:exec  "insert into some_named_numbers values (2, 'two')")
            (sql:exec  "insert into some_named_numbers values (1, 'one')")
            (sql:exec '("insert into some_named_numbers values ($1, $2)" 3 "three"))

            (sql:row "select n, d from some_named_numbers where n % 2 = 0") ; exception! result is two rows
            )
          (displayln "expected an exception")
          (sql:exec "drop table some_named_numbers")
          #f
          )))
    (test-hook as-transaction-test-1)

    ;; to make sure this transaction stuff can be done twice
    (define (as-transaction-test-2) (as-transaction-test-1))
    (test-hook as-transaction-test-2)

    ;; need to add a between threads blocking test!
    ;;



;;--------------------------------------------------------------------------------
;; db persistent unique numbers within a keyspace
;;
;;  here id stands for id number
;;
;;  this is a simple algorithm, and there is nearly an SQL column keyword for it
;;  even in this form using text (arbitrary nums) it should probably be a 
;;  a psql program run on the server when used with tables
;;
  (define (allocator-create); called from db init if table doesn't already exist, not on interface
    (sql:exec  "create table table_allocator_overhead (name text primary key ,counter text)")
    )
  (define (allocator-drop); called by no one .. for debug
    (sql:exec  "drop table table_allocator_overhead")
    )

  (define (get-counter keyspace); helper function
    (let(
          [counter-text
            (sql:maybe-value 
              (string-append
                "select counter from table_allocator_overhead"
                " where name = "
                (with-squotes keyspace)
                ))
            ]
          )
      (cond
        [(not counter-text) #f]
        [else (string->number counter-text)]
        )))
          
  ;; it is an error for keyspace not to already exist in the allocator_overhead table
  (define (update-counter keyspace new-value); helper function
    (sql:exec
      (string-append
        "update table_allocator_overhead set counter = "
        (with-squotes (number->string new-value))
        " where name = "
        (with-squotes keyspace)
        )))

  ;; upon car call to keyspace:alloc-number on a new keyspace it returns the number 1
  (define (keyspace:alloc-number keyspace)
    (with-semaphore (current-db-allocator-semaphore)
      (let(
            [count (get-counter keyspace)]
            )
        (cond
          [(not count) #f]
          [else
            (let(
                  [next-count (++ count)]
                  )
              (update-counter keyspace next-count)
              next-count
              )
            ]
          ))))

  ;; right now we just count to infinity and never recycle
  ;; probably should keep a used id list to assist with integrity checks
  (define (keyspace:dealloc-number keyspace id) (void)) 

  (define (db:create-keyspace keyspace)
    (sql:exec
      (string-append
        "insert into table_allocator_overhead values"
        "("
        (with-squotes keyspace)
        ", '0' )"
        )))

  (define (db:delete-keyspace keyspace)
    (sql:exec
      (string-append 
        "delete from table_allocator_overhead where name ="
        " "
        (with-squotes keyspace)
        )))

  (define (db:is-keyspace keyspace)
    (boolify
      (sql:maybe-value
        "select name from table_allocator_overhead where name = 'dataplex_test_1_0_keyspace'"
        )))

  (define (keyspace-alloc-test-0)
    (with-db (current-test-db)
      (let(
            [name (db:alloc-name)]
            )
        (db:create-keyspace name)
        (let(
              [n1 (keyspace:alloc-number name)]
              [n2 (keyspace:alloc-number name)]
              [n3 (keyspace:alloc-number name)]
              )
          (db:delete-keyspace name)
          (and
            (> n1 0)
            (> n2 0)
            (> n3 0)
            (≠ n1 n2)
            (≠ n1 n3)
            (≠ n2 n3)
            )))))
    (test-hook keyspace-alloc-test-0)

;;--------------------------------------------------------------------------------
;; global db persistent unique number/name service
;;
;;   used for creating temporaries in testing etc.
;;
;;   note service above for db persistent unique ids within a keyspace
;;
;;   programmers should not make identifiers of the form "unique_to_db_name-[0-9]+"
;;   one would think that wouldn't be a very strong programming constraint ...
;;
;;   analogous to session-unique service in misc-lib.rkt, but 
;;   for the life of the db instead of the session
;;
;;   deallocation, when implemented, should have two modes, one where the names
;;   are reused, for production, and ones where they can be tested to makes sure they
;;   haven't been reused - the testing stuff is not implemented.
;;
  (define (db:alloc-number) (keyspace:alloc-number "unique_to_db"))
  (define (db:alloc-name) (string-append "unique_to_db_name_" (number->string (db:alloc-number))))

  (define (db:dealloc-number number) (keyspace:dealloc-number "unique_to_db" number))
  (define (db:dealloc-name name) (void)); move it to the end of the infinite queue ..


  (define (db:alloc-name-test-0)
    (with-db (current-test-db)
      (let(
            [name-0 (db:alloc-name)]
            [name-1 (db:alloc-name)]
            )
        (not (string=? name-0 name-1))
        )))
  (test-hook db:alloc-name-test-0)

;;--------------------------------------------------------------------------------
;; table support
;;

  #|
    ; lists all user defined objects matching rx
    ; allow path names to support keyspaces
    (define (db:ls [rx #f])) 
    (define (db:with-keyspace [rx #f]) ...) ; a sort of 'cd'
    (define (db:with [rx #f]) ..)  ; limit view for scoped ls to just the selected objects

    in general want a db:object to be returned by the create calls,
    then one ls, one delete, etc, that deletes the object.  Some
    commands, like match, only work on some objects, and that is ok.

    need scopes:
     (with-connection dc ..)
     (with-db ..) 

  |#

  (define (db:tables [rx #f]) 
    (let(
          [tables-in-db (sql:list "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")]
          )
      (cond
        [(not rx) tables-in-db]
        [else
          (filter (λ(e)(regexp-match? rx e)) tables-in-db)
          ]
        )))

  ;; gets confusing to return the list back, so I boolified it
  (define (db:is-table table-name)
    (boolify (member table-name (db:tables)))
    )

  (define (db:is-table-test-0)
    (with-db (current-test-db)
      (and
        (db:is-table "table_allocator_overhead")
        (not (db:is-table (db:alloc-name)))
        )))
  (test-hook db:is-table-test-0)

  ;; e.g. deletes all tables beginning with "uni": (db:delete-table* (db:tables #rx"^uni"))
  ;;     
    (define (db:delete-table . ts) (db:delete-table* ts))
    (define (db:delete-table* ts) 
        (sql:exec*
          (map (λ(e)(string-append "drop table " (with-quotes e))) ts)
          ))

  ;; car column is automatically made the primary key
  ;;
    (define (db:create-table name column-count)
      (let(
            [db:create-table-query
              (string-append
                "create table"
                " "
                (with-quotes name)
                " "
                (make-columns-decl column-count)
                )
              ]
            )
        (sql:exec db:create-table-query)
        ))

    (define (column-name column-index)
      (string-append "column_" (number->string column-index))
      )
    (define (column-name-test-0)
      (string=? (column-name 3) "column_3")
      )
    (test-hook column-name-test-0)


    (define (make-columns-decl column-count)
      (cond  
        [(= column-count 0) "()"]
        [else
          (string-append "(" (column-name 0) " text primary key" (make-columns-decl-1 (-- column-count) 1))
          ]
        ))
    (define (make-columns-decl-1 column-count column-index)
      (cond  
        [(= column-count 0) ")"]
        [else
            (string-append " ," (column-name column-index) " text" (make-columns-decl-1 (-- column-count) (++ column-index)))
          ]
        ))
    (define (columns-decl-test-0)
      (string=?
        (make-columns-decl 3)
        "(column_0 text primary key ,column_1 text ,column_2 text)"
        )
      )
    (test-hook columns-decl-test-0)

    (define (db:create-delete-table-test-0)
      (with-db (current-test-db)
        (let*(
               [tables (map (λ(e) (db:alloc-name)) '(1 2 3))]
               [nada (map (λ(e)(db:create-table e 3)) tables)]
               [exists (andmap (λ(e)(db:is-table e)) tables)]
               )
          (db:delete-table* tables)
          (let(
                [still-exists (ormap  (λ(e)(db:is-table e)) tables)]
                )
            (and exists (not still-exists))
            ))))
    (test-hook db:create-delete-table-test-0)


  ;; a column list
  ;;   creates an ASCII list of common separated column names
  ;;   Each column name has the form 'column_i', where i is the column index.
  ;; used when creating SQL statements 
  ;;
    (define (column-list column-last [column-car 0])
      (cond
        [(< column-last column-car) "()"]
        [else
          (string-append
            "("
            (column-name column-car)
            (column-list-1 column-last (++ column-car))
            )
          ]
        ))
     (define (column-list-1 column-last column-car)
      (cond
        [(< column-last column-car) ")"]
        [else
          (string-append
            " ,"
            (column-name column-car)
            (column-list-1 column-last (++ column-car))
            )
          ]
        ))
      (define (column-list-test-0)
        (string=?
          (column-list 2)
          "(column_0 ,column_1 ,column_2)"
          ))
      (test-hook column-list-test-0)

  ;; add a value to a table, a row is a list even if the table only has one column
  ;; if the table has more than one column, the value will be a list
  ;; if the table has one column, a field value can be given directly, or as singleton list
  ;;
    (define (table:insert name . rows) (table:insert* name rows))
    (define (table:insert* name rows) ; should modify this to do one sql query
      (for-each (λ(e)(table:insert-1 name e)) rows)
      )
    (define (table:insert-1 name a-value)
      (let(
            [row  (if (pair? a-value) a-value (list a-value))]
            )
        (let(
              [row-query-arg (map (λ(e)(with-quotes (->string e))) row)]
              )
          (let(
                [insert-query
                  (string-append
                    "insert into"
                    " "
                    (with-quotes name)
                    " "
                    "values"
                    (make-values-list row)
                    )
                  ]
                )
            (sql:exec insert-query)
            ))))

    (define (make-values-list data)
      (cond  
        [(null? data) "()"]
        [else
          (string-append 
            "("
            (with-squotes (->string (car data)))
            (make-values-list-1 (cdr data))
            )
          ]
        ))

    (define (make-values-list-1 data)
      (cond  
        [(null? data) ")"]
        [else 
          (string-append 
            ","
            (with-squotes (->string (car data)))
            (make-values-list-1 (cdr data))
            )
          ]
        ))


  ;; input: a table name, a pattern to unify with records, an output filter, and an index
  ;; output: a list or records
  ;;
  ;;  in the pattern, literals much match literally, while '_' matches anything
  ;;  ->string is used on literals before building the sql statement
  ;;
  ;;  output records are strings unless translated to something else
  ;;
    (define (table:match table-name pattern [a-filter identity] [index #f])
       (let(
             [query-string
               (string-append
                 "select * from"
                 " "
                 (with-quotes table-name)
                 " "
                 (build-constraints pattern)
                 ;; " "
                 ;; (with-clause index)   ; index hint not supported in pgsql
                 )
               ]
             )
         (map 
           (λ(e)
             (a-filter (vector->list e))
             )
           (sql:rows query-string)
           )))

     ;; using 'with' to force index usage doesn't appear to be supported in pgsql
     (define (with-clause index)
       (cond
         [(not index) ""]
         [else
           (string-append
             "with ( index( "
             (with-quotes index)
             " ))"
             )
           ]
         ))

     (define (build-constraints pattern [column 0])
       (cond
         [(null? pattern) ""]
         [else
           (let(
                 [p (car pattern)]
                 [r (cdr pattern)]
                 )
             (cond
               [(eqv? p '_) (build-constraints r (++ column))]
               [else
                 (string-append
                   " where "
                   (column-name column)
                   " = "
                   (with-squotes (->string p))
                   " "
                   (build-constraints-1 r (++ column))
                   )
                 ]
               ))
           ]
         ))

     (define (build-constraints-1 pattern [column 0])
       (cond
         [(null? pattern) ""]
         [else
           (let(
                 [p (car pattern)]
                 [r (cdr pattern)]
                 )
             (cond
               [(eqv? p '_) (build-constraints-1 r (++ column))]
               [else
                 (string-append
                   " and "
                   (column-name column)
                   " = "
                   (with-squotes (->string p))
                   " "
                   (build-constraints-1 r (++ column))
                   )
                 ]
               ))
           ]
         ))

      (define (db:table-test-0)
        (with-db (current-test-db)
          (let(
                [table (db:alloc-name)]
                )
            (db:create-table table 5)
            (table:insert table '(1 2 2 7 8))
            (table:insert table '(2 2 3 7 10))
            (table:insert table '(3 3 4 7 11))
            (table:insert table '(4 2 4 6 17))
            (let(
                  [two-seven (table:match table '(_ 2 _ 7 _))]
                  )
              (begin0
                (equal?
                  two-seven
                  '(("1" "2" "2" "7" "8") ("2" "2" "3" "7" "10"))
                  )
                (db:delete-table table)
                )))))
      (test-hook db:table-test-0)

  ;; input: a table name, a pattern to identify records
  ;; output: sideffect of deleting from the table records that match the pattern
  ;;
  ;; pattern is the same as for match
  ;; if the table has more than one column, pattern must be a list
  ;;
    (define (table:delete table-name a-pattern [index #f])
      (let (
             [pattern (if (pair? a-pattern) a-pattern (list a-pattern))]
             )
        (let(
              [query-string
                (string-append
                  "delete from"
                  " "
                  (with-quotes table-name)
                  " "
                  (build-constraints pattern)
                  ;; " "        
                  ;; (with-clause index) ; pgsql does not accept a trailing with clause
                  )
                ]
              )
          (sql:exec query-string)
          )))

      (define (table:insert-delete-test-0)
        (with-db (current-test-db)
          (let(
                [table (db:alloc-name)]
                )
            (db:create-table table 2) ; will throw an exception (test fail) if the table already exists
            (let(
                  [table-found-1 (db:is-table table)]
                  [initial-rows (table:match table '(_ _))]
                  [proposed-rows '((1 2) (2 2) (3 4))]
                  [outf (λ(e)(map string->number e))]
                  )
              (table:insert* table proposed-rows)
              (let(
                    [found-rows-1 (table:match table '(_ _) outf)]
                    )
                (table:delete table '(_ 2))
                (let(
                      [found-rows-2 (table:match table '(_ _) outf)]
                      )
                  (table:delete table '(3 4))
                  (let(
                        [found-rows-3 (table:match table '(_ _) outf)]
                        )
                    (db:delete-table table)
                    (let(
                          [table-found-2 (db:is-table table)]
                          )
                      (and
                        table-found-1
                        (null? initial-rows)
                        (equal? found-rows-1 proposed-rows)
                        (equal? found-rows-2 '((3 4)))
                        (null? found-rows-3)
                        (not table-found-2)
                        ))
                    )
                  )
                )
              )
            )))
      (test-hook table:insert-delete-test-0)
;;--------------------------------------------------------------------------------
;; some tests
;;

;;--------------------------------------------------------------------------------
;; module interface
;;
  (provide
    current-test-db   
    current-working-db
    current-example-db
    with-db      
    as-transaction
    db-lib-trace
    db-lib-untrace
    )

  (provide-with-trace "db-lib"
    db-lib-init

    ;; rather not expose these if possible, sql:exec is currently used in dataplex-lib.rkt
    ;;
      sql:exec
      sql:exec*
      sql:value
      sql:value*
      sql:maybe-value
      sql:maybe-value*
      sql:row
      sql:row*
      sql:rows
      sql:rows*
      sql:list
      sql:list*

    ;; db-lib interface
    ;;
      column-list
      db:alloc-name
      db:alloc-number
      db:create-keyspace
      db:create-table
      db:delete-keyspace
      db:delete-table
      db:delete-table*
      db:is-keyspace
      db:is-table ; is a particular table in the db
      db:tables  ; lists all tables in the db
      keyspace:alloc-number
      keyspace:dealloc-number
      table:delete
      table:insert
      table:insert*
      table:match

    )

