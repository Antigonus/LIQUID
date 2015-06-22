#|
 filter parse trees

   created: 2014-12-21T10:24:28Z twl
|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  ;(require racket/trace)
  (require "lynch-lib.rkt")
  (require "tokens.rkt")

;;---------------------------------------------------------------------------------
;; trims the parse tree based on a-predicate
;;
;;  input: a list of tokens that may be roots of parse trees, a predicate that accepts and
;;         may mark a token
;;
;; output: branches leading to marked tokens
;;         
;; After trimming all leaves will be marked. Some intermediate marked takens may appear
;; within branches that lead to marked leaves.
;;
;; Often used to pull out branches leading to error messages.  The branch gives the 
;; context for the error.  See trim-tok-errs below.
;;
;; Often a parse tree has a single root, see trim-tok below.
;;
;;
  (define (trim-toks ts a-predicate)
    (define (traverse-tokens ts)
      (cond
        [(null? ts) '()]
        [(not ts) '()]
        [else 
          (let*(
                 [t (car ts)]
                 [r (cdr ts)]
                 [t-marked (a-predicate t)]
                 [depth-search-results (traverse-tokens (tok-children t))]
                 [branch-marked (pair? depth-search-results)]
                 )
            (cond
              [(or t-marked branch-marked)
                (let(
                      [y (type t)]
                      [a (tok-attributes t)]
                      [c depth-search-results]
                      )
                  (cons (tok-make* y a c) (traverse-tokens r))
                  )
                ]
              [else
                (traverse-tokens r)
                ]
              ))
          ]
        ))
    ;(trace traverse-tokens)
    (traverse-tokens ts)
    )

  ;; useful for testing
  (define (pred-true t) #t)
  (define (pred-false t) #f)
  

;;--------------------------------------------------------------------------------
;; for triming a single token
;;
;; Convenience function for running trim-toks on a single token
;;
;; input: a token and a predicate
;; output: the trimmed token, or #f if no marked tokens were found
;;
  (define (trim-tok t a-predicate) 
    (cond
      [(not t) #f]
      [else
        (let(
              [trim-result (trim-toks (list t) a-predicate)]
              )
          (cond
            [(null? trim-result) #f]
            [else
              (car trim-result)
              ]
            )
          )
        ]
      ))



;;;--------------------------------------------------------------------------------
;;; the trim predicate is set to (tok-has-err)
;;;
  (define (trim-tok-err t) (trim-tok t tok-has-err))


;;---------------------------------------------------------------------------------
;; marked
;;
;;  input: a list of tokens that may be roots of parse trees, a predicate that accepts and
;;         may mark a token, just as for trim-toks.
;;
;; output: a list of all marked tokens independent of where they were found in the tree.
;;
;;
  (define (marked-toks ts a-predicate)
    (define (traverse-tokens ts)
      (cond
        [(null? ts) '()]
        [(not ts) '()]
        [else 
          (let*(
                 [                   t (car ts)]
                 [                   r (cdr ts)]
                 [            t-marked (a-predicate t)]
                 [depth-search-results (traverse-tokens (tok-children t))]
                 [     breadth-results (traverse-tokens r)]
                 [             results (append depth-search-results breadth-results)]
                 )
            (cond
              [t-marked (cons t results)]
              [else results]
              ))
          ]
        ))
    (traverse-tokens ts)
    )

;;---------------------------------------------------------------------------------
;; is-prefix
;;
;;  input: a tree,  a proposed prefix ; both are lists
;;  output: true or false
;;   
;;  true if and only if the proposed prefix is indeed a prefix for the tree.  
;;
;;  proposed-prefix may have tree structure.  Starting at the root of the proposed-prefix,
;;  and the root of the tree, see if the proposed-prefix lays over the tree.  If it does
;;  return #t, otherwise #f.
;;
  (define (is-prefix proposed-prefix tree)
    (cond
      [(null? proposed-prefix) #t] ; null is a prefix for all trees
      [(null? tree) #f]
      [else
        (let(
              [e-proposed-prefix (car proposed-prefix)]
              [r-proposed-prefix (cdr proposed-prefix)]
              [e-tree     (car tree)]
              [r-tree     (cdr tree)]
              )
          (cond
            [(list? e-proposed-prefix) 
              (and 
                (list? e-tree) 
                (is-prefix e-proposed-prefix e-tree)
                (is-prefix r-proposed-prefix r-tree))
              ]
            [else 
              (and
                (equal? e-proposed-prefix e-tree) 
                (is-prefix r-proposed-prefix r-tree))
              ]
          ))
        ]
      ))

  (define (is-prefix-test-0)
    (is-prefix '(3) '(3))
    )
  (test-hook is-prefix-test-0)

  (define (is-prefix-test-1) 
    (and
      (is-prefix '(a b c) '(a b c d e f g))
      (is-prefix '(1 "c" x) '(1 "c" x y 7 "q"))
      (not (is-prefix '(1 "c" x) '(1 "d" x)))
      ))
  (test-hook is-prefix-test-1)

  (define (is-prefix-test-2) 
    (and
      (is-prefix '(a (b c) e) '(a (b c d) e f))
      (is-prefix '((1 2) (3 (4 7)) 1) '((1 2 a v) (3 (4 7 d)) 1 e))
      (not (is-prefix '((1 2) (3 (4 7)) 1) '((1 2 a v) 2 (3 (4 7 d)) 1 e)))
      (not (is-prefix '((1 2) (3 (4 7)) 1) '((1 3 2 a v) (3 (4 7 d)) 1 e)))
      (not (is-prefix '((1 2) (3 (4 7)) 1) '((1 3 a v) (3 (4 2 7 d)) 1 e)))
      (not (is-prefix '((1 2) (3 (4 7)) 1) '((1 2 a v) (3 (4 7 d)) 2 1 e)))
    ))
  (test-hook is-prefix-test-2)
      
;;---------------------------------------------------------------------------------
;; filter-prefix
;;
;;  returns nodes from tree for which is-prefix is true
;;
;;  input: a prefix tree, a tree ; both are lists
;;
;;  this is similar to marked-toks above, where the predicate is the prefix match, but
;;  marked-toks operates specifically on token trees and ignores attributes.
;;
;;  use repeated applications of fitler-prefix to get order indpendence of such things
;;  as attributes
;;
  (define (filter-prefix prefix tree)

    (define (filter-cdr e r)
      (or
        (and 
          (list? e)
          (append (filter e) (filter r))
          )
        (filter r)
        ))

    (define (filter tree)
      (cond
        [(null? tree) '()]
        [else
          (let*(
                 [e (car tree)]
                 [r (cdr tree)]
                 [cdr-result (filter-cdr e r)]
                )
            (or
              (and
                (is-prefix prefix tree)
                (cons tree cdr-result)
                )
              cdr-result
            ))
          ]
        ))

    (filter tree)
    )

  (define (filter-prefix-test-0)
    (equal?
      (filter-prefix '(3) '(3))
      '((3))
      ))
  (test-hook filter-prefix-test-0)

  (define filter-prefix-test-data-0
    '(*TOP*
      (*DECL* DOCTYPE html)
      "\n"
      (html
       (@ (lang "en"))
       "\n"
       (head
        (meta (@ (charset "UTF-8")))
        (title "dblp: CompleteSearch")
        (link (@
               (id "favicon")
               (rel "shortcut icon")
               (type "image/x-icon")
               (href "http://dblp.uni-trier.de/img/favicon.ico")))
         )))
    )

  (define (filter-prefix-test-1)
    (equal?
      (filter-prefix '(head (meta (@ (charset "UTF-8")))) filter-prefix-test-data-0)
      '((head
          (meta (@ (charset "UTF-8")))
          (title "dblp: CompleteSearch")
          (link (@
                  (id "favicon")
                  (rel "shortcut icon")
                  (type "image/x-icon")
                  (href "http://dblp.uni-trier.de/img/favicon.ico")))))
      ))
  (test-hook filter-prefix-test-1)

  (define (filter-prefix-test-2)
    (equal?
      (filter-prefix '(@) filter-prefix-test-data-0)
      '((@ (lang "en"))
         (@ (charset "UTF-8"))
         (@
           (id "favicon")
           (rel "shortcut icon")
           (type "image/x-icon")
           (href "http://dblp.uni-trier.de/img/favicon.ico")))
      ))
  (test-hook filter-prefix-test-2)

;;--------------------------------------------------------------------------------
;; provides
;;
   (provide-with-trace "filter"
     trim-toks
     trim-tok-err
     marked-toks
     filter-prefix
     )

