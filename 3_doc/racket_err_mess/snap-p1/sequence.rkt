#|
  extensions dealing with sequences


  for wrap and unwrap ..

  (void) is an interesting case.  If a (void) stands in as a sort of a mistake,
  saying 'oops nothing should be here',  then it should be skipped rather than
  wrapped.  However, if we unwrap a null list, then we get nothing, so the inverse
  function, wrapping nothing should produce a null list.  The difference is whether
  wrap was called or not.  The (void) place holder causes wrap to be called, trully
  nothing present does not.  What the placement of (void) in a list does is to
  preserve arity and the ability to index:  "Nothing is here, but here is a place."

  our unwrap leaves non-lists unperturbed.  It is a sort of saturanting condition.
  (void) is a non-list, so we do the same with that. 
  
  In the case that the library user does not want (void) to be a palce holder, he
  or she can call (remove-void l).  Call (remove-void) before wrapping to prevent
  null list place holders from being created.  Call (remove-void) after unwrap
  to prevent (voids) being created as place hodlers.


|#

#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require "test-lib.rkt")
  (require "arith-lib.rkt")

;;--------------------------------------------------------------------------------
;; debug switch
;;    
  (define sequence-debug (make-parameter #f))
  (provide sequence-debug) ;; so others can flip it on
  (define-for-syntax sequence-debug (make-parameter #f))


;;--------------------------------------------------------------------------------
;;  remove-void
;;     given a list returns a new list
;;     removes itesm that are (void)
;;      
  (define (remove-void l) (filter (λ(e)(not (void? e))) l))

;;--------------------------------------------------------------------------------
;;  unwrap
;;     given a list returns a new list
;;     removes one level of parens from items that happen to be lists
;;       non-list items are copied over directly
;;       top level null list items are not included in the new list
;;       (void) values do not occur as top level items in the new list
;;
  (define (unwrap l)
    (cond
      [(null? l) '()]
      [else
        (let(
              [e (car l)]
              [r (cdr l)]
              )
          (cond
            [(null? e) (unwrap r)]
            [(not (pair? e)) (cons e (unwrap r))]
            [else (append e (unwrap r))]
            ))]
     ))

  (define (test-unwrap)
    (and
      (equal? (unwrap '()) '())
      (equal? (unwrap '(1)) '(1))
      (equal? (unwrap '(1 2 3)) '(1 2 3))
      (equal? (unwrap '((1))) '(1))
      (equal? (unwrap '(1 (2 3) 4)) '(1 2 3 4))
      (equal? (unwrap '((1 2) (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
      (equal? (unwrap '((1 2) () (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
      ))
  (test-hook test-unwrap)    


;;--------------------------------------------------------------------------------
;;  wrap
;;    -given a list returns a new list
;;    -puts each item found in a given list into a list (adds one level of parens)
;;
;;
    (define (wrap items)
      (cond
        [(null? items) '()]
        [else
          (let(
                [i (car items)]
                [r (cdr items)]
                )
            (cond
              [(void? i)         (cons '() (wrap r))]
              [else              (cons (list i) (wrap r))]
              ))]))

;;--------------------------------------------------------------------------------
;;  wrap for syntax
;;    -given a list returns a result list
;;    -wraps each item found in a given list with parens (puts it in its own list)
;;    -when 'unquote is found in the function channel, the parameter of the unquote is
;;     placed in the result list without being wrapped
;;
  (begin-for-syntax

    (define (is-unsequence i) (and (pair? i) (not (null? i)) (eqv? 'unquote (car i))))

    (define (wrap items) (cons 'list (wrap-1 items)))

    (define (wrap-1 items)
      (cond
        [(null? items) '()]
        [else
          (let(
                [i (car items)]
                [r (cdr items)]
                )
            (cond
              [(void? i) (cons '() (wrap-1 r))] 
              [(is-unsequence i)
                (let(
                      [insert-items (cadr i)]
                      )
                  (cond
                    [(null? insert-items) (wrap-1 r)] ; direct-inserting a null list produces void, so there is nothing to wrap
                    [else (cons insert-items (wrap-1 r))] ; include variable holding unquoted list without wrap-1ping it
                    ))]
              [else (cons (list 'list i) (wrap-1 r))]
              ))]))

    (define (test-wrap)
      (and
        (equal? (wrap '(1 2 3)) '(list (list 1) (list 2) (list 3)))
        (equal? (wrap (list (void) 2 '(unquote (a b)))) '(list (list 2) (list a) (list b)))
        (equal? (wrap (list '(unquote ()) 2 '(unquote x))) '(list (list 2) x))
        ))

    (when (sequence-debug) (displayln (list "wrap test: " (test-wrap))))
    )

;;--------------------------------------------------------------------------------
;;  Λ - make a sequence
;;
;;  similar to the function 'list
;;
;;  it makes sense that when building a sequence  that the programmer might like to have
;;  some of the items come from another sequence, after all, sequences are how we move
;;  groups of items around.  So I would like something like 'V' that tells the
;;  packer to take items from the sequence rather than including the sequence as an item:
;;
;;    (define a-list '(3 4))
;;
;;    (Λ  1 2 (V a-list))--> '(1 2 3 4)
;;
;;    like Mathematica' Sequence operator ... sort of backwards from quasilist
;;
;;    however V evaluation would have to be delayed until run time
;;    which makes it an inband message, thus indistinguishable from a data V.
;;
;;    We don't really have access to the list packer anyway.  Wish we did, so instead I
;;    will pull unquote out of the function channel during syntax expansion and build the
;;    desired functionality with unwrap.  This is what I have been doing manually in
;;    the code already.  So
;;
;;    at syntax expansion
;;    (Λ  1 2 ,a-list 5 6)  -->  (unwrap  (list 1 2) a-list (list 3 4)) --> (1 2 3 4 5 6)
;;
;;
  (define-syntax (Λ stx)
    (let(
          [datum  (syntax->datum stx)]
          )
      (let*(
             [items (cdr datum)]
             [wrapped-items (wrap items)]
            )
        (let(
              [program (list 'unwrap wrapped-items)]
              )
          ;;(displayln program)
          (datum->syntax stx program)
          ))))


  ;; does the job of list
  (define (test-Λ-0)
    (equal? '(1 2 3) (Λ 1 2 3))
    )
  (test-hook test-Λ-0)

  ;; cons functionality
  (define (test-Λ-1)
    (equal? (cons 1 '(2 3 4)) (Λ 1 ,'(2 3 4)))
    )
  (test-hook test-Λ-1)

  ;; append functionality
  (define (test-Λ-2)
    (equal? (append '(7 8 9) '(2 3 4)) (Λ ,'(7 8 9) ,'(2 3 4)))
    )
  (test-hook test-Λ-2)

  ;; insert items from a list produced by a function
  (define (test-Λ-3)
    (equal? (Λ 1 ,(range 2 7) 7) (range 1 8))
    )
  (test-hook test-Λ-3)
  

;;--------------------------------------------------------------------------------
;; computational operations
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

    (define (is-singleton l) 
      (and
        (pair? l)
        (null? (cdr l))
        ))

    ;; input: two sequences, an element lt comparison, optionally an equal comparison
    ;; output: bool
    ;;    shorter of otherwise equal lists is considered less than
    ;;
    (define (list< a b [element-lt <] [element-eq equal?])
      (cond
        [(and (null? a) (null? b)) #f] ; then the two sequences are equal
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



;;--------------------------------------------------------------------------------
;;   given a list and an optional initial result value,  returns a list
;;   for each given list item, if pred(item)  then places in the result list (tran item)
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
  (define (replace the-list replacements)
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
      (replace l '((a . 1) (b . 2) (c . 3) (d . 4)))
      '(1 2 (3 4))))
  (test-hook test-replace-0)

;;--------------------------------------------------------------------------------
;; provides the following
;;    

  (provide 
    Λ
    )

  ;; functions
  ;;
    (provide-with-trace "sequence" ;; this context continues to the bottom of the page

      ;; efficient length compares 
      ;;   something like (length a-list) > 3  would take the length of the entire list before comparing
      ;;
        length* ; length with limit
        length>
        length≥
        length=
        length<
        length≠
        is-singleton

        list<

        unordered-equal?

        remove-void
        wrap
        unwrap
        filter-fold
        replace
          
      )

