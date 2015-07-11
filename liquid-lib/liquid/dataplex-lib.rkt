#|
  database dataplex
   created: 2015-02-10T15:45:46Z twl

   +two types of tables:

   1. tables that relate values by shape, shape-relations
   2. tables that relate values semmantic relationship, semmantic-relations


|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/trace)
  (require racket/list)
  (require db)
  (require "lynch-lib.rkt")
  (require "db-lib.rkt")

;;--------------------------------------------------------------------------------
;;  initialize the dataplex
;;    
;;     intializes the db-lib
;;     if there is already not one, creates a table to hold dataplex names
;;
  (define (db:dataplex-directory-name) "dataplexes")

  (define (dataplex-lib-init)
    (db-lib-init)
    (as-transaction
      (cond
        [(not (db:is-table (db:dataplex-directory-name))) (db:create-table (db:dataplex-directory-name) 1)]
        ))
    'no-errors
    )

  (define (dataplex-lib-init-test-0) (eqv? 'no-errors (dataplex-lib-init)))
  (test-hook dataplex-lib-init-test-0)

;;--------------------------------------------------------------------------------
;; dataplex context
;;

  (define (dataplex:keyspace dataplex-name) (string-append dataplex-name "_keyspace"))

  (define (dataplex:object-scope dataplex-name)
    (list
      'dataplex
      dataplex-name
      (dataplex:keyspace dataplex-name)
      (string-append dataplex-name "_shape_relations")
      (string-append dataplex-name "_semantic_relations")
      ))

  (define-syntax (with-dataplex stx)
    (let*(
           [datum (syntax->datum stx)]
           [dataplex-object (cadr datum)]
           [body (cddr datum)]
           )
      (datum->syntax stx
        (append
          `(let(
                 [dataplex:name (list-ref ,dataplex-object 1)]
                 [dataplex:keyspace (list-ref ,dataplex-object 2)]
                 [dataplex:shape-relations (list-ref ,dataplex-object 3)]
                 [dataplex:semantic-relations (list-ref ,dataplex-object 4)]
                 ))
          body
          ))))

  (define (syntax-with-dataplex-test-0)
    (let*(
           [a-dataplex-name (db:alloc-name)]
           [shouldbe-the-keyspace (string-append a-dataplex-name "_keyspace")]
           [dataplex-object (dataplex:object-scope a-dataplex-name)] ; lexical scope of symbols, object not in db
           )
      ;;(pretty-print shouldbe-the-keyspace)
      (with-dataplex dataplex-object
        ;;(pretty-print dataplex:keyspace)
        (string=? dataplex:keyspace shouldbe-the-keyspace)
        )
      ))
  (test-hook syntax-with-dataplex-test-0)

  (define (shape-relation:object-scope dataplex relation-name)
    (with-dataplex dataplex
      (let(
            [scoped-name (string-append dataplex:name "_" relation-name)]
            )
        (list
          'shape-relation
          dataplex:name
          relation-name
          (string-append scoped-name "_values")
          (string-append scoped-name "_citings")
          (string-append scoped-name "_values_index_by_value")
          (string-append scoped-name "_citings_index_by_value_id")
          ))))

  (define-syntax (with-shape-relation stx)
    (let*(
           [datum (syntax->datum stx)]
           [relation-object (cadr datum)]
           [body (cddr datum)]
           )
      (datum->syntax stx
        (append
          `(let*(
                  [shape-relation:owner   (list-ref ,relation-object 1)]
                  [shape-relation:name    (list-ref ,relation-object 2)]
                  [shape-relation:values  (list-ref ,relation-object 3)]
                  [shape-relation:citings (list-ref ,relation-object 4)]

                  ;; primary key on shape-relation-values does lookup by record id
                  ;; lookup here is by the user view of the value (value without the prepended id field)
                  ;; this index is typically used to determine existence or to find the id
                  [shape-relation:values:index-by-value (list-ref ,relation-object 5)]

                  ;; the primary key on meme-citings does lookup by s_statement id
                  ;; lookup here goes the other way - lookup by the meme id
                  [shape-relation:citings:index-by-shape-value-id  (list-ref ,relation-object 6)]
                 ))
          body
          ))))

  (define (semantic-relation:object-scope dataplex relation-name)
    (with-dataplex dataplex
      (let(
            [scoped-name (string-append dataplex:name "_" relation-name)]
            )
        (list
          'semantic-relation
          dataplex:name
          relation-name
          (string-append scoped-name "_shape_relations")
          (string-append scoped-name "_values")
          ))))

  (define-syntax (with-semantic-relation stx)
    (let*(
           [datum (syntax->datum stx)]
           [relation-object (cadr datum)]
           [body (cddr datum)]
           )
      (datum->syntax stx
        (append
          `(let*(
                  [semantic-relation:owner           (list-ref ,relation-object 1)]
                  [semantic-relation:name            (list-ref ,relation-object 2)]
                  [semantic-relation:shape-relations (list-ref ,relation-object 3)]
                  [semantic-relation:value-ids       (list-ref ,relation-object 4)]
                 ))
          body
          ))))


;;--------------------------------------------------------------------------------
;; top level of dataplex actions
;;
  ;; input: a regular expression, or false
  ;; oputput: a list of dataplex names
  ;;
    (define (db:dataplexes [rx #f]) 
      (let(
            [dataplex-names (map car (table:match (db:dataplex-directory-name) '(_)))]
            )
        (let(
            [selected-names (if rx (filter (λ(e)(regexp-match? rx e)) dataplex-names) dataplex-names)]
            )
        (map dataplex:object-scope selected-names)
        )))

  ;; input: a text string name for a dataplex
  ;; output: #f or #t
  ;;
    (define (db:is-dataplex-name . names)
      (cond
        [(singleton names) (db:is-dataplex-name-1 (car names))]
        [else (db:is-dataplex-name* names)]
        ))
    (define (db:is-dataplex-name* names) (map db:is-dataplex-name-1 names))

    (define (db:is-dataplex-name-1 name)
      (pair? (table:match (db:dataplex-directory-name) (list name)))
      )

    (define (db:is-dataplex . objs)
      (cond
        [(singleton objs) (db:is-dataplex-1 (car objs))]
        [else (db:is-dataplex* objs)]
        ))
    (define (db:is-dataplex* objs) (map db:is-dataplex-1 objs))
    (define (db:is-dataplex-1 obj)
      (and
        (eqv? (car obj) 'dataplex)
        (with-dataplex obj (db:is-dataplex-name dataplex:name))
        ))
                               
  ;; input: a text string name for a new dataplex
  ;; oputput: a dataplex object or 'exists
  ;;
    (define (db:create-dataplex . names) (db:create-dataplex* names))
    (define (db:create-dataplex* names)
      (cond
        [(null? names) (db:create-dataplex-1 (db:alloc-name))]
        [(singleton names) (db:create-dataplex-1 (car names))]
        [else  (map db:create-dataplex-1 names)]
        ))

    (define (db:create-dataplex-1 name)
      (cond
        [(db:is-dataplex-name name) 'exists]
        [else
          (let(
                [dataplex-scope (dataplex:object-scope name)]
                )
            (with-dataplex dataplex-scope
              (as-transaction
                (table:insert (db:dataplex-directory-name) dataplex:name)
                (db:create-keyspace dataplex:keyspace) ;a source of unique numbers for the dataplex
                (db:create-table dataplex:shape-relations 2) ;relation name, arity
                (db:create-table dataplex:semantic-relations 1) ;relation name
                )
              dataplex-scope
              ))
          ]
        ))

  (define (db:find-dataplex . names) 
      (cond
        [(singleton names) (db:find-dataplex-1 (car names))]
        [else (db:find-dataplex* names)]
        ))
  (define (db:find-dataplex* names) (map db:find-dataplex-1 names))
  (define (db:find-dataplex-1 name)
    (cond
      [(not (db:is-dataplex-name name)) #f]
      [else
        (dataplex:object-scope name)
        ]
      ))


  ;; input: a dataplex object
  ;; output: error symbol or 'no-error
  ;;
  ;;  deletes the parts of the dataplex independently so as to be useful in deleting damaged
  ;;
  ;;
    (define (db:delete-dataplex . dataplexes)(db:delete-dataplex* dataplexes))
    (define (db:delete-dataplex* dataplexes) (map db:delete-dataplex-1 dataplexes))
    (define (db:delete-dataplex-1 dataplex)
      (with-dataplex dataplex ; this will define the scoped symbols even if the dataplex is not registered
        (define (delete-shape-relation)
          (let(
                [shape-relation-names (table:match dataplex:shape-relations '( _ _ ) car)]
                )
            (let(
                  [shape-relations (map (λ(e)(shape-relation:object-scope dataplex e)) shape-relation-names)]
                  )
              (dataplex:delete-shape-relation* dataplex shape-relations)
              (db:delete-table dataplex:shape-relations)
              )))
        (define (delete-semantic-relation)
          (let(
                [semantic-relation-names (table:match dataplex:semantic-relations '(_) car)]
                )
            (let(
                  [semantic-relations (map (λ(e)(semantic-relation:object-scope dataplex e)) semantic-relation-names)]
                  )
              (dataplex:delete-semantic-relation* dataplex semantic-relations)
              (db:delete-table dataplex:semantic-relations)
              )))
        (when (db:is-table dataplex:shape-relations) (delete-shape-relation))
        (when (db:is-table dataplex:semantic-relations) (delete-semantic-relation))
        (db:delete-keyspace dataplex:keyspace)
        (table:delete (db:dataplex-directory-name) dataplex:name)
        ))


  ;; this test only looks at the tables that exist after creation, inserting 
  ;; data might cause more tables to come into existance, and delete should
  ;; get those also, but that is not tested here.
  ;;
    (define (db:dataplex-test-0)
      (db-lib-init)
      (let(
            [dataplex-name (db:alloc-name)]
            [exists-flag #f]
            [still-exists-flag #t]
            )
        (let(
              [new-dataplex (db:create-dataplex dataplex-name)]
              )
          (with-dataplex new-dataplex
            (set! exists-flag 
              (andmap 
                db:is-table 
                (list 
                  dataplex:shape-relations 
                  dataplex:semantic-relations
                  ))))

          (db:delete-dataplex new-dataplex)
          (with-dataplex new-dataplex  ;; ok to use scoped symbols from deleted dataplex object ..
            (set! still-exists-flag 
              (ormap 
                db:is-table 
                (list 
                  dataplex:shape-relations 
                  dataplex:semantic-relations
                  ))))
          ;;(pretty-print (list 'exists-flag exists-flag 'stil-exists-flag still-exists-flag))
          (and exists-flag (not still-exists-flag))
          )))
      (test-hook db:dataplex-test-0)

  (define (dataplex:is-shape-relation proposed-shape-relation)
    (eqv? 'shape-relation (car proposed-shape-relation))
    )

  (define (dataplex:is-semantic-relation proposed-semantic-relation)
    (eqv? 'semantic-relation (car proposed-semantic-relation))
    )  

;;--------------------------------------------------------------------------------
;; shape-relations
;;

  ;; input: dataplex object, shape-relation name, a column-count
  ;; output: shape relation object or 'create-failed
  ;;
  ;; creates a new shape-relation in the given dataplex.  A prepended column becomes
  ;; the primary key record id
  ;;
    (define (dataplex:create-shape-relation a-dataplex shape-relation-name column-count)
      (let(
            [shape-relation (shape-relation:object-scope a-dataplex shape-relation-name)]
            )
        (with-shape-relation shape-relation
          (let(
                [create-index-by-value-query
                  (string-append
                    "create unique index"
                    " "
                    (with-quotes shape-relation:values:index-by-value)
                    " on "
                    (with-quotes shape-relation:values)
                    " "
                    (column-list column-count 1) ; creates a column list starting at 1 going to column-count
                    )
                  ]
                [create-index-by-shape-value-id-query
                  (string-append
                    "create index"
                    " "
                    (with-quotes shape-relation:citings:index-by-shape-value-id)
                    " on "
                    (with-quotes shape-relation:citings)
                    " "
                    "(column_2)" ; careful! if the table shapeat is changed this must be updated
                    )
                  ]
                )

            (as-transaction
              (db:create-table shape-relation:values (++ column-count)); id, value fields
              (sql:exec create-index-by-value-query)

              (db:create-table shape-relation:citings 3); semantic value id, order, shape value id
              (sql:exec create-index-by-shape-value-id-query)

              ;; put the new shape relation name into the dataplex shape-relations list
              (with-dataplex a-dataplex
                (table:insert dataplex:shape-relations (list shape-relation:name column-count))
                )
              shape-relation
              )))))

  ;;  input: dataplex, a shape relation object
  ;;  output: 'no-error (or an exception)
  ;;  sideeffect: deletes related tables
  ;;       
  ;;  we also try to delete pieces from damaged dataplexes
  ;;
    (define (dataplex:delete-shape-relation dataplex . shape-relations) 
      (dataplex:delete-shape-relation* dataplex shape-relations)
      )
    (define (dataplex:delete-shape-relation* dataplex shape-relations)
      (for-each (λ(e)(dataplex:delete-shape-relation-1 dataplex e)) shape-relations)
      'no-error
      )
    (define (dataplex:delete-shape-relation-1 dataplex a-shape-relation) 
      (with-shape-relation a-shape-relation    
        (when (db:is-dataplex dataplex)
          (with-dataplex dataplex
            (table:delete dataplex:shape-relations `(,shape-relation:name _))
            )
          )
        (when (db:is-table shape-relation:citings) (db:delete-table shape-relation:citings))
        (when (db:is-table shape-relation:values) (db:delete-table shape-relation:values))
        ))

   ;; input: a shape value
   ;; output: the value id, or 0
   ;;
     (define (shape-relation:lookup-id shape-relation a-value)
       (with-shape-relation shape-relation
         (as-transaction
           (let(
                 [id-list
                   (table:match
                     shape-relation:values 
                     (cons '_ a-value)
                     (λ(e)(string->number (car e)))
                     shape-relation:values:index-by-value
                     )
                   ]
                 )
             (cond
               [(null? id-list) 0]
               [else (car id-list)]
               )))))

   ;; input: shape-relation, and a new value for the shape relation
   ;; output: a shape value id
   ;;     lookups up value, if founds returns its id, if not found inserts and returns its id
   ;;
     (define (shape-relation:insert shape-relation a-value)
           (let(
                 [id (shape-relation:lookup-id shape-relation a-value)]
                 )
             (cond
               [(not (= id 0)) id]
               [else
                 (with-shape-relation shape-relation
                   (let(
                         [new-id (keyspace:alloc-number (dataplex:keyspace shape-relation:owner))]
                         )
                     (table:insert shape-relation:values (cons new-id a-value))
                     new-id
                     ))
                 ]
               )))

   ;; input: citing info
   ;; output: side effect only, inserts the citing
   ;;    this is a helper, does not appear on the interface
   ;;    inserts the bridge
   ;;
     (define (shape-relation:citing:insert shape-relation semantic-value-id semantic-column shape-relation-value-id)
       (with-shape-relation shape-relation
         (table:insert shape-relation:citings (list semantic-value-id semantic-column shape-relation-value-id))
         ))


   ;; input: shape-relation, shape-relation value
   ;; output: list of semantic value ids
   ;;
   ;;      begs the question of which semantic relation owns the value id
   ;;      might want to create and id owner table, would help with integrity
   ;;      checks also. right the user can't do anything with the output.
   ;;      Should implement a higher performance 'has-semantic-citings.
   ;;
     (define (shape-relation:list-semantic-citings shape-relation a-value)
       (let(
             [id (shape-relation:lookup-id shape-relation a-value)]
             )
         (cond
           [(= id 0) 'no-such-value]
           [else
             (with-shape-relation shape-relation
               (table:match 
                 shape-relation:citings
                 '(_ _ ,id) 
                 (λ(e)(string->number (car e)))
                 shape-relation:citings:index-by-shape-value-id
                 ))
             ]
           )))


   ;; input: shape-relation, pattern (constants and '_) for rows to be deleted
   ;; output: list of citings and no delete occured,  or 'no-error
   ;;
     (define (shape-relation:delete shape-relation pattern)
       (let(
            [citings (shape-relation:list-semantic-citings shape-relation pattern)]
             )
         (cond 
           [(pair? citings) citings]
           [else 
             (with-shape-relation shape-relation
               (table:delete shape-relation:values (cons '_ pattern) shape-relation:values:index-by-value)
               )
             'no-error
             ]
           )))

   ;; input: shape-relation, pattern to match values against
   ;; output: list of matches
   ;;
     (define (shape-relation:match shape-relation pattern [filter identity])
       (with-shape-relation shape-relation
         (table:match 
           shape-relation:values 
           (cons '_ pattern)
           (λ(e)(filter (cdr e))) ; cdr drops the id field
           )))

   ;; input: shape-relation, pattern 
   ;; output: list of semantic value ids that cite the matched value
   ;;
     (define (shape-relation:match-semantic-citings shape-relation pattern)
       (with-shape-relation shape-relation
         (let(
               [shape-value-ids
                 (table:match 
                   shape-relation:values 
                   (cons '_ pattern)
                   car
                   )
                 ]
               )
           #|
           (pretty-print 
             (list 
               'pattern pattern
               'shape-relation:values shape-relation:values 
               'shape-value-ids shape-value-ids
               ))
           |#
           (define (find-citing id)
             (table:match 
               shape-relation:citings
               `(_ _ ,id) 
               (λ(e)(string->number (car e)))
               shape-relation:citings:index-by-shape-value-id
               ))
           (append-map find-citing shape-value-ids)
           )))

    (define (shape-relation-test-0)
      (let(
            [d0  (dataplex:object-scope "shape-relation-test-0-dp")]
            )
        (db:delete-dataplex d0)
        (let*(
               [d1 (db:create-dataplex "shape-relation-test-0-dp")]
               [p0 (dataplex:create-shape-relation d1 "p0-name" 3)]
               [id-0 (shape-relation:insert p0 '(27 28 29))]
               [id-1 (shape-relation:insert p0 '(57 28 29))]
               [id-2 (shape-relation:insert p0 '(27 28 29))]
               [m-0  (shape-relation:match p0 '(27 28 _) (λ(e)(map string->number e)))]
               [m-1  (shape-relation:match p0 '(_ 28 29) (λ(e)(map string->number e)))]
               [nada-0 (shape-relation:delete p0 '(27 28 _))]
               [m-2  (shape-relation:match p0 '(27 _ _)  (λ(e)(map string->number e)))]
               )
          #|
          (pretty-print
            (list
              'id-0 id-0
              'id-1 id-1
              'id-2 id-2
              'm-0  m-0
              'm-1  m-1
              'm-2  m-2
              ))
          |#
          (db:delete-dataplex d0)
          (cond
            [(= id-0 id-1) #f]
            [(not (= id-0 id-2)) #f]
            [(not (equal? m-0 '((27 28 29)))) #f]
            [(not 
               (or
                 (equal? m-1 '( (27 28 29) (57 28 29) ))
                 (equal? m-1 '( (57 28 29) (27 28 29) ))
                 ))
               #f]
            [(pair? m-2) #f]
            [else #t]
            )
          )))
    (test-hook shape-relation-test-0)

;;--------------------------------------------------------------------------------
;; semantic-relations
;;

  ;;  input: a name for the semantic-relation, a sequence of shape-relations for the columns
  ;;  output: a semantic-relation object
  ;;       
    (define (dataplex:create-semantic-relation dataplex semantic-relation-name column-shape-relations)
      (cond
        [(not (andmap dataplex:is-shape-relation column-shape-relations)) #f] ; column-shape-relations must be shape-relations
        [else
          (let(
                [column-shape-names (map (λ(e)(with-shape-relation e shape-relation:name)) column-shape-relations)]
                [semantic-relation (semantic-relation:object-scope dataplex semantic-relation-name)]
                )
          (with-dataplex dataplex
            (table:insert dataplex:semantic-relations semantic-relation-name)
            )
          (with-semantic-relation semantic-relation
            (as-transaction
              (db:create-table semantic-relation:shape-relations 1)
              (map
                (λ(e) (table:insert semantic-relation:shape-relations e) )
                column-shape-names
                )
              (db:create-table semantic-relation:value-ids 1)
              semantic-relation
              )
            ))
          ]
        ))

  ;;  input: a semantic relation object
  ;;  output: 'no-error (or an exception)
  ;;  sideeffect: deletes related tables
  ;;
  ;;  we also try to delete pieces from damaged dataplexes
  ;;       
    (define (dataplex:delete-semantic-relation dataplex . semantic-relations) 
      (dataplex:delete-semantic-relation* dataplex semantic-relations)
      )
    (define (dataplex:delete-semantic-relation* dataplex semantic-relations)
      (for-each (λ(e)(dataplex:delete-semantic-relation-1 dataplex e)) semantic-relations)
      'no-error
      )
    (define (dataplex:delete-semantic-relation-1 dataplex a-semantic-relation) 
      (with-semantic-relation a-semantic-relation    
        (when (db:is-dataplex dataplex)
          (with-dataplex dataplex
            (table:delete dataplex:semantic-relations `(,semantic-relation:name _))
            )
          )
        (when (db:is-table semantic-relation:shape-relations) (db:delete-table semantic-relation:shape-relations))
        (when (db:is-table semantic-relation:value-ids) (db:delete-table semantic-relation:value-ids))
        ))

  ;; input: a semantic relation and a value to put in it
  ;; output:  'no-value, 'value-length, or 'no-error
  ;;
  ;;    should store the column count in the object, silly to go back out to the db
  ;;    to get it (table:match ..) ?  or do we need the list anyway?
  ;;
  ;; Dataplex is needed to access the keyspace, and to create object-scope for
  ;; shape relations.  Both could be designed around .. we could also get the dataplex
  ;; from the semantic-relation owner field, and that would even be more stable.
  ;;
    (define (semantic-relation:insert dataplex semantic-relation a-value)
      (with-semantic-relation semantic-relation
        (let(
              [column-shape-names (table:match semantic-relation:shape-relations '(_) car)]
              )
          (cond
            [(null? a-value) 'no-value]
            [(≠ (length a-value) (length column-shape-names)) 'value-length]
            [else
              (with-dataplex dataplex
                (let(
                      [semantic-value-id (keyspace:alloc-number dataplex:keyspace)]
                      )
                  (define (semantic-relation:insert-1 a-value shape-relations column) ; a recursive form, i.e. iterator
                    (cond
                      [(null? shape-relations) 'no-error]
                      [else
                        (let(
                              [shape-relation (car shape-relations)]
                              [cdr-shape-relations  (cdr shape-relations)]
                              [item (car a-value)]
                              [cdr-a-value (cdr a-value)]
                              [next-column (++ column)] 
                              )
                          (let(
                                [shape-relation-item-id (shape-relation:insert shape-relation item)]
                                )
                            (shape-relation:citing:insert shape-relation semantic-value-id column shape-relation-item-id)
                            (semantic-relation:insert-1 cdr-a-value cdr-shape-relations next-column)
                            ))
                        ]
                      ))
                  ;(trace semantic-relation:insert-1)
                  (let(
                        [the-shape-relations
                          (map 
                            (λ(e) (shape-relation:object-scope dataplex e)) 
                            column-shape-names
                            )
                          ]
                        )
                    (semantic-relation:insert-1 a-value the-shape-relations 0)
                    (table:insert semantic-relation:value-ids semantic-value-id)
                    )))
              ]
            ))))

    ;; input: a semantic relation, a list of semantic ids
    ;; output: 'rows' from the semantic relation
    ;;
;;      (define (semantic-relation:lookup-ids semantic-relation pattern) (void))


    ;; input: a semantic relation, a pattern for data matching 
    ;; output: a possibly null list of semantic value ids
    ;;
    ;;  a semantic-relation has a number of typed (shaped) columns
    ;;
    ;;  the data for a semantic relation column is retrieved by using the value id to
    ;;  cross the bridge table into the corresponding column shape relation.  A semantic
    ;;  row's field, i.e. an column entry in a row, may hold many values.  In
    ;;  this implementation such values will have the same type (shape).
    ;;
    ;;  We are to match each column data value against a pattern element.  There is
    ;;  one pattern element per column.
    ;;
    ;;  A pattern element may be '_' which means all entries in the column are considered
    ;;  matches, or it may be a literal value.  A ltieral value may only be used when the
    ;;  corresponding column table (shape relation) is one column wide.  Otherwise the
    ;;  pattern element will be a list, where this list becomes a pattern for matching the
    ;;  on the corresponding shape relation table.
    ;;
    ;;  Rather than keeping the match values on the correspding shape relation table,
    ;;  we instead keep a list of semmantic value ids that had a match.  Hence each
    ;;  column matching subproblem will return a list of semantic value ids.
    ;;
    ;;  After finding the semantic value ids for each column, we will end up with a list
    ;;  of the column matching ids.  Our job is then to find all the ids that are represented
    ;;  in all the column matches.  I.e. we take a big conjunction.  The resulting semantic
    ;;  value ids are the semantic row matches.  We can then use them to fetch the matching
    ;;  rows if we want the data. (perhaps we just wanted to know which rows matched).
    ;;
    ;;  Note, if all the patttern elements are '_ then all the rows in our semantic value
    ;;  table are matches, and we don't need to examine the column contents. We just return
    ;;  with the semantic value ids list
    ;;
    ;;  Note also, that if a pattern element is a list, and all the elements in the list
    ;;  are '_ this is the same thing as the pattern element just being set to '_
    ;;
    ;;   note... racket version 6 will allow set-intersect to be applied to lists
    ;;   so we don't need the conversions
    ;;
      (define (semantic-relation:lookup-ids dataplex semantic-relation pattern)
        (with-semantic-relation semantic-relation
          (cond
            [(open-pattern pattern) ; then all ids are matches
              (table:match semantic-relation:value-ids '(_) (λ(e)(string->number (car e))))
              ] 
            [else
              (let*(
                     [column-shape-names (table:match semantic-relation:shape-relations '(_) car)]
                     [ids-per-column (match-column-ids dataplex column-shape-names pattern)]
                     [common-ids (set->list (apply set-intersect (map list->set ids-per-column)))]
                     )
                common-ids
                )
              ]
            )))

      (define (open-pattern pattern) (andmap (λ(e)(eqv? '_ e)) pattern))

    ;; input: ordered list of the shape-relation names for the semantic table, a match pattern
    ;; output: list per column of semantic ids that are matches for the column
    ;;
      (define (match-column-ids dataplex column-shape-names pattern)
        (cond
          [(and (null? column-shape-names) (null? pattern)) '()]
          [(null? column-shape-names) 'extraneous-pattern-columns]
          [(null? pattern) 'pattern-missing-columns]
          [else
            (let(
                  [column-name (car column-shape-names)]
                  [column-name-cdr (cdr column-shape-names)]
                  [column-pattern (car pattern)]
                  [pattern-cdr (cdr pattern)]
                  )
              (cond
                [(eqv? '_ column-pattern) (match-column-ids dataplex column-name-cdr pattern-cdr)]
                [else
                  (cons 
                    (match-shape-relation-ids dataplex column-name column-pattern)
                    (match-column-ids dataplex column-name-cdr pattern-cdr)
                    )
                  ]
              ))
            ]
          ))

    ;; input: a semantic table column type (i.e. a shape relation for the column), an element from the match pattern
    ;; output:  a list of semantic ids that appear in the shape relation with the given value
    ;;
    ;;  currently when this routine is called the pattern-element will be a literal value
    ;;
      (define (match-shape-relation-ids dataplex shape-relation-name pattern-element)
        (let(
              [shape-relation (shape-relation:object-scope dataplex shape-relation-name)]
              )
          (shape-relation:match-semantic-citings shape-relation pattern-element)
          ))
         

    (define (semantic-relation:lookup-ids-test-0)
      (db:delete-dataplex (dataplex:object-scope "semantic-relation:lookup-ids-test-0"))
      (let*(
             [dp (db:create-dataplex "semantic-relation:lookup-ids-test-0")]
             [sp-name   (dataplex:create-shape-relation dp "name" 3)]
             [sp-number (dataplex:create-shape-relation dp "number" 2)]
             [sm-account  (dataplex:create-semantic-relation dp "account" (list sp-name sp-number))]
             )
        (semantic-relation:insert dp sm-account '(("James" "M" "Doe") (345 1053)))
        (begin0
          (equal?
            (semantic-relation:lookup-ids dp sm-account '(_ _))
            '(1)
            )
          (db:delete-dataplex (dataplex:object-scope "semantic-relation:lookup-ids-test-0"))
          )
      ))
    (test-hook semantic-relation:lookup-ids-test-0) 

    (define (semantic-relation:lookup-ids-test-1)
      (db:delete-dataplex (dataplex:object-scope "semantic-relation:lookup-ids-test-1"))
      (let*(
             [dp (db:create-dataplex "semantic-relation:lookup-ids-test-1")]
             [sp-name (dataplex:create-shape-relation dp "name" 3)]
             [sp-number (dataplex:create-shape-relation dp "number" 2)]
             [sm-account (dataplex:create-semantic-relation dp "account" (list sp-name sp-number))]
             )
        (semantic-relation:insert dp sm-account '(("James"  "M" "Doe")    (345 1053)))
        (semantic-relation:insert dp sm-account '(("John"   "H" "Smith")  (555 1212)))
        (semantic-relation:insert dp sm-account '(("Ronald" "M" "Donald") (312 5860)))
        (semantic-relation:insert dp sm-account '(("John"   "M" "Ward")   (463 5860)))
        (let(
              [t0
                (equal?
                  (semantic-relation:lookup-ids dp sm-account '(_ _))
                  '(1 4 7 10)
                  )
                ]
              [t1
                (equal?
                  (semantic-relation:lookup-ids dp sm-account '( (_  "M" _) _))
                  '(1 7 10)
                  )
                ]
              [t2
                (equal?
                  (semantic-relation:lookup-ids dp sm-account '( (_  "M" _) (312 _)))
                  '(7)
                  )
                ]
              )
          ;;(pretty-print (list 't0 t0 't1 t1 't2 t2))
          (and t0 t1 t2)
      )))
      (test-hook semantic-relation:lookup-ids-test-1)


  ;; input: a dataplex, a semantic-relation, a pattern
  ;; output: filtered rows from the semantic relation
  ;;
      (define (semantic-relation:match dataplex semantic-relation [pattern '(_)] [filter identity])
        (with-semantic-relation semantic-relation
          (define (fetch id)
            (let*(
                   [column-shape-names (table:match semantic-relation:shape-relations '(_) car)]
                   [shapes (map (λ(e)(shape-relation:object-scope dataplex e)) column-shape-names)]
                   )
              (map (λ(e)(shape-relation:match-by-semantic-id e id)) shapes)
              ))

          (let*(
                 [ids (semantic-relation:lookup-ids dataplex semantic-relation pattern)]
                 [rows (map fetch ids)]
                 )
            (map filter rows)
            )))

      (define (shape-relation:match-by-semantic-id shape-relation id)
        (with-shape-relation shape-relation
          (let(
                [shape-row-ids
                  (table:match 
                    shape-relation:citings
                    `(,id _ _) 
                    caddr
                    )
                  ]
                )
            (let(
                  [matched-rows
                    (append-map 
                      (λ(e)(table:match shape-relation:values `(,e) cdr)) 
                      shape-row-ids
                      )
                    ]
                  )
              ;;(pretty-print (list 'matched-rows matched-rows))
              (if (singleton matched-rows) (car matched-rows) matched-rows)
              ))))
          
    (define (semantic-relation:match-test-1)
      (db:delete-dataplex (dataplex:object-scope "semantic-relation:match-test-1"))
      (let*(
             [dp (db:create-dataplex "semantic-relation:match-test-1")]
             [sp-name (dataplex:create-shape-relation dp "name" 3)]
             [sp-number (dataplex:create-shape-relation dp "number" 2)]
             [sm-account (dataplex:create-semantic-relation dp "account" (list sp-name sp-number))]
             )
        (semantic-relation:insert dp sm-account '(("James"  "M" "Doe")    (345 1053)))
        (semantic-relation:insert dp sm-account '(("John"   "H" "Smith")  (312 1212)))
        (semantic-relation:insert dp sm-account '(("Ronald" "M" "Donald") (312 5860)))
        (semantic-relation:insert dp sm-account '(("John"   "M" "Ward")   (463 5860)))
        (let(
              [m0 (semantic-relation:match dp sm-account '(_ _))]
              [m1 (semantic-relation:match dp sm-account '( (_  "M" _) _))]
              [m2 (semantic-relation:match dp sm-account '( (_  "M" _) (312 _)))]
              )
          ;;(pretty-print (list 'm0 m0 'm1 m1 'm2 m2))
          (and
            (equal?
              m0
              '(
                 (("James" "M" "Doe") ("345" "1053"))
                 (("John" "H" "Smith") ("312" "1212"))
                 (("Ronald" "M" "Donald") ("312" "5860"))
                 (("John" "M" "Ward") ("463" "5860"))
                 ))
            (equal?
              m1
              '(
                 (("James" "M" "Doe") ("345" "1053"))
                 (("Ronald" "M" "Donald") ("312" "5860"))
                 (("John" "M" "Ward") ("463" "5860"))
                 ))
            (equal?
              m2
              '(
                 (("Ronald" "M" "Donald") ("312" "5860"))
                 ))))))
      (test-hook semantic-relation:match-test-1)


;;--------------------------------------------------------------------------------
;; some more tests
;;

  (define (dataplex-test-1)

    ;; clear out prior test tables, if any
    (table:delete (db:dataplex-directory-name) '("dataplex_test_1_0"))
    (db:delete-keyspace "dataplex_test_1_0_keyspace")
    (db:delete-table* (db:tables "^dataplex_test_1_0"))

    ;; car build a dataplex
    (let*(
           [name-0 "dataplex_test_1_0"]
           [pattern-0 (string-append "^" name-0 "_")]
           [table-list-0 (tables pattern-0)] 
           [dataplex-0 (db:create-dataplex name-0)]
           [table-list-1 (tables pattern-0)]
           )
      (cond 
        [(eqv? dataplex-0 'exists) #f]
        [else
          (let*(
                 [name-1 "p1"]
                 [pattern-1 (string-append pattern-0 name-1 "_")]
                 [table-list-2 (tables pattern-1)]
                 [shape-relation-0 (dataplex:create-shape-relation dataplex-0 name-1 3)]
                 [table-list-3 (tables pattern-1)]

                 [name-2 "p2"]
                 [pattern-2 (string-append pattern-0 name-2 "_")]
                 [table-list-4 (tables pattern-2)]
                 [shape-relation-1 (dataplex:create-shape-relation dataplex-0 name-2 1)]
                 [table-list-5 (tables pattern-2)]

                 [name-3 "s1"]
                 [pattern-3 (string-append pattern-0 name-3 "_")]
                 [table-list-6 (tables pattern-3)]
                 [semantic-relation-0 (dataplex:create-semantic-relation dataplex-0 name-3 (list shape-relation-0 shape-relation-1))]
                 [table-list-7 (tables pattern-3)]
                 )

            #|;; useful info should the test fail
            (pretty-print 
              (list
                'pattern-0 pattern-0 
                'pattern-1 pattern-1
                'pattern-2 pattern-2
                'pattern-3 pattern-3
                ))

            (pretty-print 
              (list
                'table-list-2 table-list-2
                'table-list-3 table-list-3
                'table-list-6 table-list-6
                'table-list-7 table-list-7
                ))
           |#
            
            ;; check that we understand what the build does
            ;; too bad we don't check the two indexes also ..
            (let(
                  [dataplex-shell
                    (and
                      (null? table-list-0)
                      (db:is-keyspace name-0)
                      (equal? table-list-1 
                        `(
                           "dataplex_test_1_0_semantic_relations"
                           "dataplex_test_1_0_shape_relations"
                           )
                        ))
                    ]

                  [shape-relation-shell-0
                    (and
                      (null? table-list-2)
                      (equal? table-list-3 
                        `(
                           "dataplex_test_1_0_p1_citings"
                           "dataplex_test_1_0_p1_values"
                           )
                        ))
                    ]

                  [shape-relation-shell-1
                    (and
                      (null? table-list-4)
                      (equal? table-list-5 
                        `(
                           "dataplex_test_1_0_p2_citings"
                           "dataplex_test_1_0_p2_values"
                           )
                        ))
                    ] 

                  [semmantic-relation-shell-0
                    (and
                      (null? table-list-6)
                      (equal? 
                        table-list-7
                        '(
                           "dataplex_test_1_0_s1_shape_relations" 
                           "dataplex_test_1_0_s1_values"
                           ))
                      (equal? 
                        (sort (table:match "dataplex_test_1_0_shape_relations" '(_ _) car) string<?)
                        `("p1" "p2")
                        )
                      )  
                    ]
                  )
              
               #|;; if there is a failure this shows which component failed
                (pretty-print 
                  (list 
                    'dataplex-shell dataplex-shell
                    'shape-relation-shell-0 shape-relation-shell-0
                    'shape-relation-shell-1 shape-relation-shell-1
                    'semmantic-relation-shell-0 semmantic-relation-shell-0
                    ))
              |#
              
              (define build-result (and dataplex-shell shape-relation-shell-0 shape-relation-shell-1 semmantic-relation-shell-0))
              (db:delete-dataplex dataplex-0)
              ;;(pretty-print(list 'pattern-0 pattern-0))
              (define table-list-8 (tables pattern-0))
              (and
                build-result
                (null? table-list-8)
                )
            ))
          ]
          )))
          (test-hook dataplex-test-1)

   (define (tables pattern) (sort (db:tables pattern) string<?))

;;--------------------------------------------------------------------------------
;; module interface
;;
  (provide-with-trace "dataplex-lib"
    dataplex-lib-init

    ;; affect the database object
    ;;
      db:dataplexes
      db:is-dataplex-name
      db:is-dataplex-name*

      db:find-dataplex ; given name returns object
      db:find-dataplex*

      db:is-dataplex
      db:is-dataplex*
 
      db:create-dataplex
      db:create-dataplex*

      db:delete-dataplex
      db:delete-dataplex*

    ;; affect the dataplex
    ;;
      dataplex:create-shape-relation
      dataplex:delete-shape-relation
      dataplex:delete-shape-relation*

      dataplex:create-semantic-relation
      dataplex:delete-semantic-relation
      dataplex:delete-semantic-relation*

    ;; affect a shape relation
    ;;
      shape-relation:insert
      shape-relation:delete
      shape-relation:match

    ;; affect a semantic relation
    ;;
      semantic-relation:insert
      semantic-relation:match

      ;semantic-relation:delete

    )

#| 
    (define (dataplex-lib-trace-internal)
      (trace shape-relation:list-semantic-citings)
      (trace semantic-relation:lookup-ids)
      (trace match-column-ids)
      (trace match-shape-relation-ids)
      (trace shape-relation:match-semantic-citings)
      (trace shape-relation:match-by-semantic-id)
      )
|#
