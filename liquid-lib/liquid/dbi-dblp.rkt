#|
  database functions for citations
   created: 2015-01-14T08:34:34Z twl

  this is part of the dbpl interface

  A citation is held in a group of tables, here is a set of
  routines for manipulating such a table group as an entity

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require db)
  (require "misc-lib.rkt")
  (require "db-lib.rkt")

;;--------------------------------------------------------------------------------
;;  provides a context with the citation tables named against a schema-name
;;    input: (with-tables schema-name body ...)  
;;
  (define-syntax (with-citation-tables stx)
    (let*(
           [datum  (syntax->datum stx)]
           [schema-name   (cadr datum)]
           [body   (cddr datum)]
           )
      (datum->syntax stx
        (append
          `(let(
                 [table-publication (string-append ,schema-name "_publication")]
                 [table-author (string-append ,schema-name "_author")]
                 [table-bridge (string-append ,schema-name "_bridge_publication_author")]
                 ))
          body
          ))))

;;--------------------------------------------------------------------------------
;;  create tables in the database
;;
;;   input: a schema-name from which to make table names
;;   output: side effect, creates the tables
;;
;;  bridge should probably have a couple of indexes on it
;;
  (define (create-citation-schema schema-name)
    (with-citation-tables schema-name
      (let(
            [create-table-publication-query
              (string-append 
                "create table"
                " "
                table-publication 
                " "
                "(id text primary key ,venue text ,type text ,year text)"
                )
              ]
            [create-table-author-query
              (string-append 
                "create table"
                " "
                table-author
                " "
                "(id text primary key ,name text)"
                )
              ]
            [create-table-bridge-query
              (string-append
                "create table"
                " "
                table-bridge
                " "
                "(bridge_id text primary key ,publication_id text ,author_id text)"
                )
              ]
            )
        (let(
              [create-citation-schema-work-query (list
                             create-table-publication-query
                             create-table-author-query
                             create-table-bridge-query
                             )
                ]
              )
          ;;(pretty-print transaction)
          (as-transaction 
            (sql:exec* create-citation-schema-work-query)
            (db:create-keyspace table-publication)
            (db:create-keyspace table-author)
            (db:create-keyspace table-bridge)
            )
          ))))


;;--------------------------------------------------------------------------------
;;  drops a citation table group
;;
;;   input: a schema-name from which to make table names
;;   output: side effect, creates the tables
;;  
  (define (drop-citation-schema schema-name)
    (with-citation-tables schema-name
      (let(
            [tables (list table-publication table-author table-bridge)]
            )
        (as-transaction
          (for-each db:delete-table tables)
          (for-each db:delete-keyspace tables)
          ))))


;;--------------------------------------------------------------------------------
;;  insert an publication
;;
;;   input: table schema-name,  an publication
;;  output: 'publication-exissts, or the new publication id
;;
;; (id text primary key ,venue text ,type text ,year text)
;;
  (define (insert-publication-0 schema-name venue type year)
    (let(
          [table-publication (string-append schema-name "_publication")]
          )
      (let(
            [publication-id (get-publication-id-1 table-publication venue type year)]
            [new-publication-id  (keyspace:alloc-number table-publication)]
            )
        (cond
          [publication-id 'publication-exists]
          [else
            (insert-publication-1 table-publication new-publication-id venue type year)
            new-publication-id
            ]
          ))))

  ;; input: publication table to use, the name of an publication
  ;; output: the unique record id for this publication, or false if not found, throws error if more than one match
  ;;   requires that the counter table has been properly initializedxs
  ;;
    (define (get-publication-id-1 table-publication venue type year)
      (let(
            [publication-id-text
              (sql:maybe-value 
                (string-append
                  "select id from " table-publication 
                  " where "
                  "venue = " (with-squotes venue)
                  " AND "
                  "type = " (with-squotes type)
                  " AND "
                  "year = " (with-squotes year)
                  ))
                ]
          )
        (and
          publication-id-text
          (string->number publication-id-text)
          )))

  (define (insert-publication-1 table-publication publication-id venu type year)
    (sql:exec
      (string-append
        "insert into "
        table-publication 
        " values (" 
        (with-squotes (number->string publication-id))
        " ,"
        (with-squotes venu)
        " ,"
        (with-squotes type)
        " ,"
        (with-squotes year)
        ")"
        )))

;;--------------------------------------------------------------------------------
;;  insert an author
;;
;;   input: table schema-name,  an author
;;  output: existing or new author id
;;
;;
  (define (insert-author-0 schema-name new-author)
    (with-citation-tables schema-name
      (let(
            [author-id (get-author-id-1 table-author new-author)] ; returns false if author not found
            [new-author-id  (keyspace:alloc-number table-author)]
            )
        (cond
          [author-id author-id]
          [else
            (insert-author-1 table-author new-author-id new-author)
            new-author-id
            ]
          ))))

  ;; input: author table to use, the name of an author
  ;; output: the record id for an author in text form, or false
  ;;   requires that the counter table has been properly initializedxs
  ;;
    (define (get-author-id-1 table-author author-name)
      (let(
            [author-id-text
              (sql:maybe-value 
                (string-append
                  "select id from " 
                  table-author 
                  " where name = " 
                  (with-squotes author-name)
                  ))
              ]
            )
        (and
          author-id-text
          (string->number author-id-text)
          )))

  (define (insert-author-1 table-author author-id author-name)
    (sql:exec
      (string-append
        "insert into "
        table-author 
        " values (" 
        (with-squotes (number->string author-id))
        " ,"
        (with-squotes author-name)
        ")"
        )))

;;--------------------------------------------------------------------------------
;;  insert bridge
;;
;;   input: a publication id, an author id
;;  output: 'bridge-exists, or the record id for the bridge entry
;;
;;
  (define (insert-bridge-0 schema-name publication-id author-id)
    (with-citation-tables schema-name
      (let(
            [author-ids (get-authors-for-publication-1 table-bridge publication-id)]
            [new-bridge-id  (keyspace:alloc-number table-bridge)]
            )
        (cond
          [(member author-id author-ids) 'bridge-exists]
          [else
            (insert-bridge-1 table-bridge new-bridge-id publication-id author-id)
            ]
          ))))

  ;; input: bridge table to use, a publication id
  ;; output: a list of author ids
  ;;
    (define (get-authors-for-publication-1 table-bridge publication-id)
      (let(
            [author-ids
              (sql:list
                (string-append
                  "select author_id from"
                  " "
                  table-bridge
                  " "
                  "where publication_id ="
                  " "
                  (with-squotes (number->string publication-id))
                  ))
              ]
            )
        author-ids
        ))

  (define (insert-bridge-1 table-bridge new-bridge-id publication-id author-id)
    (sql:exec
      (string-append
        "insert into "
        table-bridge
        " values (" 
        (with-squotes (number->string new-bridge-id))
        " ,"
        (with-squotes (number->string publication-id))
        " ,"
        (with-squotes (number->string author-id))
        ")"
        )))


;;--------------------------------------------------------------------------------
;;  insert an citation
;;
;;   input: schema-name,  a citation for a new publication
;;  output: 'publication-exists, or 'complete
;;
  (define (insert-citation schema-name venue type year . authors)
    (insert-citation* schema-name venue type year authors))

  (define (insert-citation* schema-name venue type year authors)
    (as-transaction
      (let(
            [publication-id (insert-publication-0 schema-name venue type year)]
            )
        (cond
          [(eqv? 'publication-exists publication-id) 'publication-exists]
          [else
            (for-each 
              (λ(author)
                (let(
                      [author-id (insert-author-0 schema-name author)]
                      )
                  (insert-bridge-0 schema-name publication-id author-id)
                  ))
              authors
              )
            'complete
            ]
          ))))

;;--------------------------------------------------------------------------------
;;  get citations
;;
;;   input: schema-name
;;  output: #f if no such schema,  '() if no citations, or a list of citations
;;
;;  a citaiton has the form '( venu type year (author ...))
;;
    (define (get-citations schema-name)
     (as-transaction 
       (let*(
              [pubs (get-publications schema-name)]
             )
         (map 
           (λ(pub)
             (let(
                   [pub-id-as-text (car pub)]
                   [pub-no-id (cdr pub)]
                   )
             (append
               pub-no-id
               (get-authors schema-name pub-id-as-text)
               )))
           pubs
           ))))

    (define (get-publications schema-name)
      (with-citation-tables schema-name
        (let*(
               [pubs-as-vecs
                 (sql:rows
                   (string-append
                     "select * from"
                     " "
                     table-publication
                     ))
                 ]
               [pubs-as-lists (map (λ(rec)(vector->list rec)) pubs-as-vecs)]
               )
          pubs-as-lists
          )))

    (define (get-authors schema-name publication-id-as-text)
      (with-citation-tables schema-name
        (let(
              [author-ids-as-text
                (sql:list
                  (string-append
                    "select author_id from"
                    " "
                    table-bridge 
                    " "
                    "where publication_id ="
                    " "
                    (with-squotes publication-id-as-text)
                    ))
                ]
              )
          (let(
                [authors 
                  (map 
                    (λ(author-id) 
                      (sql:maybe-value
                        (string-append
                          "select name from"
                          " "
                          table-author
                          " "
                          "where id ="
                          " "
                          (with-squotes author-id)
                          )))
                    author-ids-as-text
                    )
                  ]
                )
            authors
            ))))

  (define (test-db-citation-0)
    (with-db (current-test-db)
      (let*(
             [schema (db:alloc-name)]
             [author-table (string-append schema "_author")]
             [pub-0 '("How to Ride a Horse" "journal" "1873" "Alex" "Tomlinson" "Smith" "Johnson")]
             [pub-1 '("Making Glue" "conference" "1921" "Johnson" "B. Johnson")]
             )
        (create-citation-schema schema)
        (apply insert-citation schema pub-0)
        (apply insert-citation schema pub-1)
        (and
          (db:is-table author-table)
          (equal?
            (get-citations schema)
            (list pub-0 pub-1))
          (begin
            (drop-citation-schema schema)
            (not (db:is-table author-table))
            )
          ))))
  (test-hook test-db-citation-0)

;;--------------------------------------------------------------------------------
;; provides
;;
;;
  (provide-with-trace "db-citation"
    create-citation-schema
    drop-citation-schema
    insert-citation
    get-citations
    )

