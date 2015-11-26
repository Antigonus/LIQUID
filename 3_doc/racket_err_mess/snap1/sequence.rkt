
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;    
  (require "arith-lib.rkt")


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
              ))])))


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


  




;;--------------------------------------------------------------------------------
;; provides the following
;;    

  (provide 
    Λ
    )

  ;; functions
  ;;
    (provide


        remove-void
        wrap
        unwrap

          
      )

