#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require "test-lib.rkt")

;;--------------------------------------------------------------------------------
;;  make a list
;;
  (define Λ list)
  (define-for-syntax Λ list)

;;--------------------------------------------------------------------------------
;; arith operators
;;
  (define (≠ . args) (not (apply = args)))
  (define ≥ >=)
  (define ≤ <=)
  (define (++ n) (+ n 1))
  (define (-- n) (- n 1))

  ;; and can not be used with apply,  and-form can be
  ;; and-form does not short circuit
  ;;
    (define (and-form . args) (and-form* args))
    (define (and-form* arg-list) (andmap identity arg-list))

    (define (or-form . args) (or-form* args))
    (define (or-form* arg-list) (ormap identity arg-list))

    (define (and-form-test-0)
      (and
        (and-form #t #t #t)
        (not (and-form #t #f #t))))
    (test-hook and-form-test-0)


;;--------------------------------------------------------------------------------
;; list manipulation
;;
  ;; efficient length compares
  ;;
    (define (length* l limit [n 0])
      (cond [(null? l) n]
            [(= n limit) limit]
            [else (length* (cdr l) (++ n))]))

    (define (length≥ l n)
      (cond [ (and (null? l) (= n 0))  #t]
            [ (null? l) #f ]
            [ (= n 0) #t ]
            [ else (length≥ (cdr l) (-- n))]
            ))

    (define (length≥-test-0) (length≥ '(a b c) 2))
    (define (length≥-test-1) (length≥ '(a b c) 3))
    (define (length≥-test-2) (not (length≥ '(a b c) 4)))
    (define (length≥-test-3) (length≥ '() 0))
    (define (length≥-test-4) (not (length≥ '() 1)))

    (define (length< l n) (not (length≥ l n)))
    (define (length> l n) (length≥ l (++ n)))


    (define (length= l n)
      (cond [ (and (null? l) (= n 0))  #t]
            [ (null? l) #f ]
            [ (= n 0) #f ]
            [ else (length= (cdr l) (-- n))]
            ))
    (define (length=-test-0) (not (length= '(a b c) 2)))
    (define (length=-test-1) (length= '(a b c) 3))
    (define (length=-test-2) (not (length= '(a b c) 4)))

    (define (length≠ l n) (not (length= l n)))

    (define (singleton l) 
      (and
        (pair? l)
        (null? (cdr l))
        ))

    ;; input: two lists, an element lt comparison, optionally an equal comparison
    ;; output: bool
    ;;    shorter of otherwise equal lists is considered less than
    ;;
    (define (list< a b [element-lt <] [element-eq equal?])
      (cond
        [(and (null? a) (null? b)) #f] ; then the two lists are equal
        [(null? a) #t]
        [(null? b) #f]
        [else
          (let(
                [ea (car a)]
                [eb (car b)]
                [ra (cdr a)]
                [rb (cdr b)]
                )
            (cond
              [(element-lt ea eb) #t]
              [(element-eq ea eb) (list< ra rb)]
              [else #f]
              ))
          ]
        ))

    (define (list<-test-0)
      (and
        (list< '(5 7 4) '(7 7 4))
        (list< '(5 7 4) '(5 8 4))
        (list< '(5 7 4) '(5 7 4 3))
        (list< '(5 7 4 3) '(5 7 5))
        (not 
          (or
            (list< '(7 7 4) '(5 7 4))
            (list< '(5 8 4) '(5 7 4))
            (list< '(5 7 4 3) '(5 7 4))
            (list< '(5 7 5) '(5 7 4 3))
            (list< '(5 7 4 3) '(5 7 4 3))
            ))))
    (test-hook list<-test-0)         



  ;;  all rows and columns in the outer product should have a true element
  ;;  I didn't use list->set because set didn't provide n equal lambda (or did I miss that?)
  ;;  surely this is in a library somewhere ..
  ;;
    (define (unordered-equal? a b [the-eq-fun equal?])
      (define (unordered-equal-1 a v)
        (cond
          [(null? a) (and-form* v)] ; if all v are true, then every b was equal to something
          [else
            (let(
                  [a0 (car a)]
                  [ar (cdr a)]
                  )
              (let*(
                     [afit/v 
                       (foldr 
                         (λ(bi vi r)
                           (let(
                                 [a0=bi (the-eq-fun a0 bi)]
                                 [afit-i0 (first r)]
                                 [v0 (second r)]
                                 )
                             (list
                               (or afit-i0 a0=bi) ; afit-i1
                               (cons (or vi a0=bi) v0)    ; v1
                               )))
                         '(#f ())
                         b v
                         )
                       ]
                     [afit (first afit/v)]
                     [v (second afit/v)]
                     )
                (cond
                  [(not afit) #f] ; if a0 wansn't equal to any b, then we short circuit out
                  [else
                    (unordered-equal-1 ar v)
                    ]
                  )))]))

      (let(
            [a0 (car a)]
            [ar (cdr a)]
            )
        (let(
              [v (map (λ(bi)(the-eq-fun a0 bi)) b)]
              )
          (cond
            [(not (or-form* v)) #f]
            [else
              (unordered-equal-1 ar v)
              ]
            ))))

  (define (test-unordered-equal?-0)
    (let(
          [a1  '(a1 1 2 3)]
          [a10 '(a10 10 20 30)]
          [a11 '(a10 11 21 31)]
          [a2  '(a2 7 89)]
          )
      (let*(
             [i0 (list a1 a10 a11 a2)]
             [i1 (list a11 a1 a10 a2)]
             )
        (unordered-equal? i0 i1))))
  (test-hook test-unordered-equal?-0)

  (define (test-unordered-equal?-1)
    (let(
          [a1  '(a1 1 2 3)]
          [a1b  '(a1 1 2 3 4)]
          [a10 '(a10 10 20 30)]
          [a11 '(a10 11 21 31)]
          [a2  '(a2 7 89)]
          )
      (let*(
             [i0 (list a1 a10 a11 a2)]
             [i1 (list a11 a1b a10 a2)]
             )
        (not (unordered-equal? i0 i1)))))
  (test-hook test-unordered-equal?-1)


  (define (bcons a-list item) (cons item a-list)) ; we like to specifiy the object being worked on first

  (define (cat a-list . items) (append a-list items))
  (define (cat-test-0)
    (and
      (equal?
        (cat '(a b) 'c)
        '(a b c))
      (equal?
        (cat '(1 3) 5 7)
        '(1 3 5 7)
        )
      ))
  (test-hook cat-test-0)

;;--------------------------------------------------------------------------------
;;  flatten n levels
;;
;;
  (define (flatten-1 l)
    (foldr
      (λ(e0 r0)
        (if (pair? e0)
          (foldr (λ(e1 r1) (cons e1 r1)) r0 e0)
          (cons e0 r0)
          ))
      '()
      l
      ))

  (define-for-syntax (flatten-1 l)
    (foldr
      (λ(e0 r0)
        (if (pair? e0)
          (foldr (λ(e1 r1) (cons e1 r1)) r0 e0)
          (cons e0 r0)
          ))
      '()
      l
      ))


  (define (test-flatten-1)
    (and
      (equal? (flatten-1 '()) '())
      (equal? (flatten-1 '(1)) '(1))
      (equal? (flatten-1 '(1 2 3)) '(1 2 3))
      (equal? (flatten-1 '((1))) '(1))
      (equal? (flatten-1 '(1 (2 3) 4)) '(1 2 3 4))
      (equal? (flatten-1 '((1 2) (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
      ))
  (test-hook test-flatten-1)    


;;--------------------------------------------------------------------------------
;;
  (define (filter-fold pred tran l [init-r '()])
    (foldr
      (λ(e r) (if (pred e) (cons (tran e) r) r) )
      init-r
      l
      ))
  (define (filter-fold-test-0)
    (equal?
      (filter-fold (λ(e)#t) (λ(e)e) '(1 2 3))
      '(1 2 3)
      ))
  (define (filter-fold-test-1)
    (equal?
      (filter-fold (λ(e)(odd? e)) (λ(e)(- e 1)) '(1 2 3))
      '(0 2)
      ))


;;--------------------------------------------------------------------------------
;;  replace
;;
;; 'replacements' is a list of key-value pairs
;; if item is a list, replace descends
;; each non-list item is checked to see  if it is a key, and if so it is replaced with the value
;;
  (define (replace replacements the-list)
    (let(
          [table (make-hash replacements)]
          )

      (define (replace-1 a-list)
        (for/list(
              [e a-list]
              )
          (cond
            [(list? e)  (replace-1 e)]
            [else (hash-ref table e e)]
            )))
      
      (replace-1 the-list)
      ))
       
  (define (test-replace-0)
    (define l '(a b (c d)))
    (equal?
      (replace '((a . 1) (b . 2) (c . 3) (d . 4)) l)
      '(1 2 (3 4))))
  (test-hook test-replace-0)

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  ;; functions
  ;;
    (provide-with-trace "list-lib" ;; this context continues to the bottom of the page

      ;; defines a list (Λ item-0 item-1 ...)
      ;;
        Λ

      ;; efficient length compares 
      ;;   something like (length a-list) > 3  would take the length of the entire list before comparing
      ;;
        length* ; length with limit
        length>
        length≥
        length=
        length<
        length≠
        singleton

        list<

        unordered-equal?

        bcons
        cat
          
      )
