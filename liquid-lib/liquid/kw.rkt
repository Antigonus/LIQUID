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

;;--------------------------------------------------------------------------------
;;  define abstract domains
;;
  (with-db (current-example-db) ;; need to make this exaple specific rather than shared by examples

    
    (dataplex-lib-init)

#|
    (define dp-ex1 (db:create-dataplex "ex1"))

    (define A1 (dataplex:create-shape-relation dp-ex1 "A1" 1))
    (define A2 (dataplex:create-shape-relation dp-ex1 "A2" 1))
    (define A3 (dataplex:create-shape-relation dp-ex1 "A3" 1))

    (shape-relation:insert A1 '(c2))
    (shape-relation:insert A1 '(c5))
    (shape-relation:insert A1 '(c6))
    (shape-relation:insert A1 '(k1))

    (shape-relation:insert A2 '(c1))
    (shape-relation:insert A2 '(c4))
    (shape-relation:insert A2 '(c7))

    (shape-relation:insert A3 '(k2))

|#
    )

#|

(define (domains x)
  (cond*
    [(pair? (shape-relation:match A1 (Λ x))) 'A1]
    [(pair? (shape-relation:match A2 (Λ x))) 'A2]
    [(pair? (shape-relation:match A3 (Λ x))) 'A3]
    ))


;;--------------------------------------------------------------------------------
;;  local knowldge
;;

  ;; relation schemas
  ;;
    (define R1 '( A1 A2 ))
    (define R2 '( A2 A1 ))
    (define R3 '( A1 A2 A3 ))

  ;; access constraints
  ;;   
    (define P1 '(i o o))
    (define P2 '(i o o))
    (define P3 '(i o o))



;;--------------------------------------------------------------------------------
;;  web interfaces  -- this are mock ups to match the paper example
;;
;;    each webi accepts input values and returns a list of tupples that match
;;    for this simple example we did not need to use patterns
;;    webis make use of local knowledge
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

(define (aplicable-webi x)
  (let*(
        [d  (domains x)]
        [

|#
  ;;--------------------------------------------------------------------------
  ;;  query planning
  ;;      the strategy is to qualify webis based on
  ;;



