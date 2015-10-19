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

      (shape-relation:insert A1 '(c2))
      (shape-relation:insert A1 '(c5))
      (shape-relation:insert A1 '(c6))
      (shape-relation:insert A1 '(k1))

      (shape-relation:insert A2 '(c1))
      (shape-relation:insert A2 '(c4))
      (shape-relation:insert A2 '(c7))

      (shape-relation:insert A3 '(k2))

      (set! R1 (dataplex:create-semantic-relation dp-ex1 "R1" (Λ A1 A2)))
      (set! R2 (dataplex:create-semantic-relation dp-ex1 "R2" (Λ A2 A1)))
      (set! R3 (dataplex:create-semantic-relation dp-ex1 "R3" (Λ A1 A2 A3)))
      ))

   ;; for sanity's sake:
     (cache:init)
     (cache:delete)
     (cache:build)

  ;; a utility function
  (define (cache:display)
    (with-db (current-example-db) 
      (pretty-print (semantic-relation:match dp-ex1 R1 '(_ _)))
      (pretty-print (semantic-relation:match dp-ex1 R2 '(_ _)))
      (pretty-print (semantic-relation:match dp-ex1 R3 '(_ _ _)))
      ))


;;--------------------------------------------------------------------------------
;;  web interfaces mockups
;;
;;  normally a webi goes out to a website and scrapes the requested data,
;;  here our scrapper mockups just pretend to do that
;;
;;  each webi accepts an input items and returns a list of rows
;;  the input is simplified for sake of example
;;
   (define (webi-1 x1) ; first website scraper
     (define r1 '(
                   (k1 c1)
                   (c2 c4)
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
;;  fetch - if data is not already local, goes out to the webi to get it
;;
;; each fetch routine accepts an input item to match, returns newly fetched data
;;
  (define (fetch-1 x)
    (with-db (current-example-db) 
      (if (pair? (semantic-relation:match dp-ex1 R1 (Λ (Λ x) '_)))
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

  (define (fetch-2 x)
    (with-db (current-example-db) 
      (if (pair? (semantic-relation:match dp-ex1 R2 (Λ (Λ x) '_)))
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

  (define (fetch-3 x)
    (with-db (current-example-db) 
      (if (pair? (semantic-relation:match dp-ex1 R3 (Λ (Λ x) '_ '_)))
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
        (for/list(
                   [shape (Λ A1 A2 A3)]
                   [shape-tag '(A1 A2 A3)]
                   #:when (is-shape-relation-match shape x)
                   )
          shape-tag
          ))


     ;; schema and access contraints bindings
     ;; given a set of shapes, returns a list of applicable webis
     ;; 
       (define (select-fetch-routines shape-tags)
         (for/list(
                    [a-shape-tag '(A1 A2 A3)]
                    [fetch-routine (Λ fetch-1 fetch-2 fetch-3)]
                    #:when (set-member? shape-tags a-shape-tag)
                    )
           fetch-routine
           ))

     ;; fetch
     ;; given an item fetches other items
     ;;
       (define (fetch x)
         (let*(
                [shape-tags (identify-shapes x)]
                [fetch-routines (select-fetch-routines shape-tags)]
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
     ;;   recursively fetches, as side effect fetches are cached, retursn nothing
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
                      [new-items (fetch x)]
                      [items-queue (append new-items xr)]
                      )
                 (reach-1 items-queue)
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
    cache:display

    webi-1
    webi-2
    webi-3

    fetch-1
    fetch-2
    fetch-3

    semantic-relation-match

    is-shape-relation-match
    identify-shapes
    select-fetch-routines
    fetch
    reach
    reach-1
    )
