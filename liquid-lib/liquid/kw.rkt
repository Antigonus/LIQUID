#| 
  Keyword search example.

1. We take the next 'q' value from head of the input queue.
2. We examine q and recognize which abstract domain it belongs to.  
3. We check the relation schemas to see which also make use of said abstract domain, and do so as an input.
4. We query all of these relations, and get back returned tupples. 
4.1 we put each return tupple in a cache table that corresponds to the relation it came from.   We will have three cache tables per your example.
4.2 we place each value from all returned tupples onto the input queue for search,  and then repeat from 1.

|#
#lang racket

(require "misc-lib.rkt")
(require "db-lib.rkt")
(require "dataplex-lib.rkt")

(provide (all-defined-out))

;;--------------------------------------------------------------------------------
;;  setup database to be a cache for web fetches
;;
  (define dp-ex1 null)

  (define A1 null)
  (define A2 null)
  (define A3 null)

  (define R1 null)
  (define R2 null)
  (define R3 null)

  (define (cache:init) ; < ---- this must be the first call!
    (with-db (current-example-db) 
      (dataplex-lib-init)
      ))

  (define (cache:delete)
    (with-db (current-example-db) 
      (define existing-dp-ex1 (db:find-dataplex "dp-ex1"))
      (when existing-dp-ex1 (db:delete-dataplex existing-dp-ex1)); start each run of the example fresh
      ))
    
  (define (cache:build)
    (with-db (current-example-db) 

      (set! dp-ex1 (db:create-dataplex "dp-ex1"))

      (set! A1 (dataplex:create-shape-relation dp-ex1 "A1" 1))
      (set! A2 (dataplex:create-shape-relation dp-ex1 "A2" 1))
      (set! A3 (dataplex:create-shape-relation dp-ex1 "A3" 1))

      ;; the caching opertion will build these if they are not given
      (shape-relation:insert A1 '(c2))
      (shape-relation:insert A1 '(c5))
      (shape-relation:insert A1 '(c6))
      (shape-relation:insert A1 '(k1))

      (shape-relation:insert A2 '(c1))
      (shape-relation:insert A2 '(c3))
      (shape-relation:insert A2 '(c4))
      (shape-relation:insert A2 '(c7))

      (shape-relation:insert A3 '(k2))

      (set! R1 (dataplex:create-semantic-relation dp-ex1 "R1" (Λ A1 A2)))
      (set! R2 (dataplex:create-semantic-relation dp-ex1 "R2" (Λ A2 A1)))
      (set! R3 (dataplex:create-semantic-relation dp-ex1 "R3" (Λ A1 A2 A3)))
      ))

   ;; for sanity's sake, we do this on load:
     (cache:init)
     (cache:delete)
     (cache:build)

  ;; pretty display of a semantic relation (specific to this example with two character singleton abstract domain values)
  (define (display-sm an-sm)
    (with-db (current-example-db)
      (let*(
             [t0 (semantic-relation:match dp-ex1 an-sm '_)] ; returns a list of rows, each row item is a singleton sp value
             [t1  (map (λ(row)(map (λ(item)(car item)) row))  t0)] ; strips parens off of the singleton row items
             )
        (map (λ(row)(displayln row)) t1)
        (void)
        )))
  
  ;; gives some visibility into the cache .. hmm should probably print the shape tables also
  (define (cache:display)
      (displayln "R1:")
      (display-sm R1)
      (displayln "R2:")
      (display-sm R2)
      (displayln "R3:")
      (display-sm R3)
      )


;;--------------------------------------------------------------------------------
;;  web interfaces - gets data from website
;;
;;  normally a webi goes out to a website and scrapes the requested data,
;;  but here our scrapper mockups just pretend to do that
;;
;;  each webi accepts an input items and returns a list of rows
;;  the input is simplified for sake of example
;;
   (define (webi-1 x1) ; first website scraper
     (define r1 '(
                   (k1 c1)
                   (c2 c3)
                   ))
     (filter (λ(e)(eqv? x1 (first e))) r1)
     )

   (define (webi-2 x1) ; second website scraper
     (define r2 '(
                   (c1 c2) 
                   (c4 c2) 
                   (c1 c6)
                   ))
     (filter (λ(e)(eqv? x1 (first e))) r2)
     )

   (define (webi-3 x1) ; third website scraper
     (define r3 '(
      (c2 c1 k2)
      (c5 c4 k2)
      (c6 c7 k2)
      ))
     (filter (λ(e)(eqv? x1 (first e))) r3)
     )

;;--------------------------------------------------------------------------------
;;  fetch-local - gets data from cache
;;
  (define (fetch-local-1 x)
    (with-db (current-example-db) 
      (semantic-relation:match dp-ex1 R1 (Λ (Λ x) '_))
      ))

  (define (fetch-local-2 x)
    (with-db (current-example-db) 
      (semantic-relation:match dp-ex1 R2 (Λ (Λ x) '_))
      ))

  (define (fetch-local-3 x)
    (with-db (current-example-db) 
      (semantic-relation:match dp-ex1 R3 (Λ (Λ x) '_ '_))
      ))

;;--------------------------------------------------------------------------------
;;  fetch-fresh - data from webi that is not in the cache (also caches the new data)
;;
  (define (fetch-fresh-1 x)
    (with-db (current-example-db) 
      (if (pair? (fetch-local-1 x))
        '()
        (let(
              [data (webi-1 x)]
              )
          (map ; place data in cache
            (λ(e) (semantic-relation:insert dp-ex1 R1 (map list e)))
            data
            )
          data
          ))))

  (define (fetch-fresh-2 x)
    (with-db (current-example-db) 
      (if (pair? (fetch-local-2 x))
        '()
        (let(
              [data (webi-2 x)]
              )
          (map ; place data in cache
            (λ(e) (semantic-relation:insert dp-ex1 R2 (map list e)))
            data
            )
          data
          ))))

  (define (fetch-fresh-3 x)
    (with-db (current-example-db) 
      (if (pair? (fetch-local-3 x))
        '()
        (let(
              [data (webi-3 x)]
              )
          (map ; place data in cache
            (λ(e) (semantic-relation:insert dp-ex1 R3 (map list e)))
            data
            )
          data
          ))))

  ;; a utility function
  (define (semantic-relation-match relation pattern)
    (with-db (current-example-db)
      (semantic-relation:match dp-ex1 relation pattern)
      ))



;;--------------------------------------------------------------------------
;;  query planning
;;  

    ;; given a keyword classifies it's shape
    ;;   alternatively shapes can be inferred from the schema
    ;;
      ;; input a shape and an item, get back a bool
      (define (is-shape-relation-match shape x)
        (with-db (current-example-db)
          (pair? (shape-relation:match shape (Λ x)))
          ))

      ;;   input an item, x, receive back a set of matching shapes
      (define (identify-shapes x)
        (with-db (current-example-db)
          (for/list(
                     [shape (Λ A1 A2 A3)]
                     [shape-tag '(A1 A2 A3)]
                     #:when (is-shape-relation-match shape x)
                     )
            shape-tag
            )))

      ;;   input an item, x, receive back a set of matching shape-relations
      (define (identify-shape-relations x)
        (with-db (current-example-db)
          (for/list(
                     [shape (Λ A1 A2 A3)]
                     [shape-tag '(A1 A2 A3)]
                     #:when (is-shape-relation-match shape x)
                     )
            shape
            )))

     ;; schema and access contraints bindings
     ;; given a set of shapes, returns a list of applicable webis
     ;; 
       (define (select-fetch-fresh-routines shape-tags)
         (flatten
           (for/list(
                      [a-shape-tag '(A1 A2 A3)]
                      [fetch-routine (Λ (Λ fetch-fresh-1 fetch-fresh-3) fetch-fresh-2 '())] ; nothing fetches A3 types
                      #:when (set-member? shape-tags a-shape-tag)
                      )
             fetch-routine
             )))

         
;;--------------------------------------------------------------------------
;;  getting data from a single keyword
;;  

     ;; fetch
     ;; given an item fetches other items
     ;;
       (define (fetch-fresh x)
         (let*(
                [shape-tags (identify-shapes x)]
                [fetch-routines (select-fetch-fresh-routines shape-tags)]
                )
           (let*(
                 [fetched-items 
                   (for/list(
                              [f fetch-routines]
                              )
                     (f x)
                     )
                   ]
                  [flat-fetched-items (flatten fetched-items)]
                  [no-x-fetched-items (filter (λ(e)(not (eqv? e x))) flat-fetched-items)]
                  )
             no-x-fetched-items
             )))
                
     ;; reach
     ;;   recursively fetches new data
     ;;   example starts from an empty cache
     ;;   intersting case not reached here would be new paths that start from cached data
     ;;   
       (define (reach x)
         (reach-1 (Λ x))
         )

       (define (reach-1 xs)
         (cond
           [(null? xs) (void)]
           [else
             (let(
                   [x (car xs)]
                   [xr (cdr xs)]
                   )
               (let*(
                      [new-items (fetch-fresh x)]
                      [items-queue (append new-items xr)]
                      )
                 (reach-1 items-queue)
               ))
             ]
           ))

;;--------------------------------------------------------------------------
;;  keyword search
;;  

    ;; the join graph is now cached, access constraints are gone
    ;; finding the answers is just a breadth first search problem returning minimum depth results
    ;;
      (define (find-answers e0 e1)
        (cond
          [(equal? e0 e1) (Λ e0)]
          [else
            (reach e0)
            (reach e1)
            (let(
                  [i0 (abstract e0)]
                  [i1 (abstract e1)]
                  )
              (let(
                    [abstract-answers (breadth-search (Λ(Λ i0)) i1)]
                    )
                abstract-answers
                ))]))
            
           
    ;; the current dataplex drops the table ownership info from the sp-id (woops)
    ;; This needs to be fixed, but until then this routine suffices for this exmple to recover that info
    ;;
      (define (owner sp-id)
        (with-db (current-example-db)
          (cond
            [(shape-relation:has-id A1 sp-id) A1]
            [(shape-relation:has-id A2 sp-id) A2]
            [(shape-relation:has-id A3 sp-id) A3]
            [else '()]
            )))

    ;; given a value returns an sp_id
    ;;
      (define (abstract e)
        (with-db (current-example-db) 
          (shape-relation:lookup-id (car (identify-shape-relations e)) (Λ e))
        ))

    ;; given an sp-id finds sp-ids that are connected through relations
    ;;
      (define (infer sp-id)
        (with-db (current-example-db)
          (let(
                [sm-ids (shape-relation:sm-id-from-sp-id (owner sp-id) sp-id)]
                )
            (flatten 
              (append
                (map (λ(id)(shape-relation:sp-id-from-sm-id A1 id)) sm-ids)
                (map (λ(id)(shape-relation:sp-id-from-sm-id A2 id)) sm-ids)
                (map (λ(id)(shape-relation:sp-id-from-sm-id A3 id)) sm-ids)
                )))))

    ;; breadth first search step
    ;;   extends search paths by one inference upon call
    ;;   paths grow to the left (newest value on the left)
    ;;   does not visit the same value twice
    ;;
      (define (breadth-search-1 paths visited [infer-fun infer])
        (let*(
               [newly-visited (mutable-set)]
               [extended-paths
                 (flatten-1
                   (for/list(
                              [path paths]
                              )
                     (let*(
                            [inference (infer-fun (car path))]
                            [clean-inference (set-subtract (list->set inference) visited)]
                            )
                       (set-union! newly-visited clean-inference)
                       (for/list(
                                  [e clean-inference]
                                  )
                         (cons e path)
                         ))))
                 ]
               )
          (values extended-paths (set-union visited newly-visited))
          ))

      (define (breadth-search-1-test-1)
        (define paths '((4 1) (5 1) (8 1)))
        (define visited (set 7))
        (define infer-fun (λ(e)(Λ (+ e 2) (+ e 3))))
        (let-values([(rp1 v) (breadth-search-1 paths visited infer-fun)])
          (and
            (equal?
              rp1
              '((6 4 1) (8 5 1) (10 8 1) (11 8 1)) 
              )
            (equal?
              v
              (set 6 7 8 10 11)
              ))))

      (test-hook breadth-search-1-test-1)

    ;; breadth first search
    ;;
      (define (breadth-search paths goal [visited (set)])
        (cond
          [(null? paths) '()]
          [else
            (let(
                  [answers (filter (λ(path)(equal? (car path) goal)) paths)]
                  )
              (cond
                [(pair? answers) answers]
                [else
                  (let-values(
                        [(new-paths new-visited) (breadth-search-1 paths visited)]
                        )
                    (breadth-search new-paths goal new-visited)
                    )
                  ]
                ))
            ]
          ))

;;--------------------------------------------------------------------------------
;; module interface
;;
  (provide-with-trace "kw"

    cache:init
    cache:delete
    cache:build
    display-sm
    cache:display

    webi-1
    webi-2
    webi-3

    fetch-local-1
    fetch-local-2
    fetch-local-3

    fetch-fresh-1
    fetch-fresh-2
    fetch-fresh-3

    semantic-relation-match

    is-shape-relation-match
    identify-shapes
    select-fetch-fresh-routines
    fetch-fresh
    reach
    reach-1

    find-answers
    owner
    infer
    breadth-search-1
    breadth-search

    )
