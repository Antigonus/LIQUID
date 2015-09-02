#|
 node
   
  created: 2014-12-04T07:55:00Z twl
  major revision: 2015-08-22T06:05:02Z twl  

  The problem with the first version of node came when we said an attribute could be
  anything.  That fomented a need to dynamically pick an equal operation for each attribute
  when comparing nodes.  Node compares are common in the test code, but we also see them
  in the parse filters.  Soon we hope to havea  more general pattern compare.


|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "misc-lib.rkt")
  (require "object.rkt")


;;--------------------------------------------------------------------------------
;; node
;;
  ;; place shared data and functions for all nodes here
  ;; if there is an enumeration of allowed keys for the node type, it would go in here
  ;;
    (define nd:key:tag 'nd:key:tag)
    (define nd:key:children 'nd:key:children) ; set of children

    (define nd:type 
      (let(
             [objid (obj:add-type (obj:make) type-type)]
             )
        (when obj:debug (obj:name-hook objid "nd:type"))
        objid
        ))

    (define (nd:is n) (obj:has n nd:type))
    (define (nd:make) (obj:add-type (obj:make) nd:type))

;;--------------------------------------------------------------------------------
;; attribute set
;;   an attribute is a key value pair.  We collect them in a hash table.  Each
;;   attribute can potentially compare differently and the racket has table doesn't
;;   give us the means to do that, so here we wrap a hash table in an  object
;;
  (define atset:key:lexeme (obj:add-type (obj:make) type-type)) ; value is a string
  (define atset:key:source (obj:add-type (obj:make) type-type)) ; value is a source object
  (define atset:key:source-nds (obj:add-type (obj:make) type-type)) ; value is a set of nodes
  (define atset:key:value (obj:add-type (obj:make) type-type))
                
  (define atset:type
     (let(
            [objid (obj:add-type (obj:make) type-type)]
            )
       (when obj:debug (obj:name-hook objid "atset:type"))
       objid
       ))

    (define (atset:is as) (obj:has as atset:type))
    (define (atset:make) (obj:add-type (obj:make) atset:type))


;;--------------------------------------------------------------------------------
;; node with attributes
;;
  (define ndwat:type 
    (let(
          [objid (obj:add-type (obj:make) type-type)]
          )
      (when obj:debug (obj:name-hook objid "ndwat:type"))
      objid
      ))

  (define (ndwat:is n)
    (and
      (obj:has n nd:type)
      (obj:has n atset:type)
      ))

  ;; Λattributes alternating key val list, Λchildren list of children
  (define (ndwat:make Λattributes Λchildren)
    (let(
          [objid (obj:make)]
          )
      (when obj:debug (obj:name-hook objid "ndwat:object"))
      (obj:add-type objid nd:type)
      (obj:add-type objid atset:type)
      (cond
        [(not (null? Λattributes)) (obj:set! atset:type objid Λattributes)]
        [(not (null? Λchildren)) (obj:set! nd:type objid (Λ nd:key:children (list->set Λchildren)))]
      )))


;;--------------------------------------------------------------------------------
;; add functions to the types, (code located here in order avoid forward reference problems
;; on type defiinitions.)
;;
  (void (obj:set! type-type atset:type
    (Λ 
      '= (λ(a b) ; a and b are two data objects that share atset:type
           ;;(displayln (Λ "running equal on two atsets: " a " " b))
           (let(
                 [a-keys (obj:keys atset:type a)]
                 [b-keys (obj:keys atset:type b)]
                 )
             (and
               (equal? a-keys b-keys)
               (for/and(
                         [key a-keys]
                         )
                 (let(
                       [a-value (obj:ref* atset:type a key)]
                       [b-value (obj:ref* atset:type b key)]
                       )
                   (cond
                     [(eqv? key atset:key:source-nds) ; the source-nds attribute holds a set of nodes
                       (and
                         (= (set-count a-value) (set-count b-value))
                         (for/and(
                                   [ndwat-a a-value]
                                   [ndwat-b b-value]
                                   )
                           (obj:apply* type-type ndwat:type '= (Λ ndwat-a ndwat-b))
                           ))
                       ]
                     [else
                       (equal? a-value b-value)
                       ]
                     ))))))
      )))

  (void (obj:set! type-type nd:type
    (Λ 
      '=  (λ(a b) ; a and b are two data objects that share nd:type
            (let(
                  [tag-a null]
                  [tag-b null]
                  [children-a null]
                  [children-b null]
                  )
              (obj:ref nd:type a nd:key:tag (λ(tag)(set! tag-a tag)) void void)
              (obj:ref nd:type b nd:key:tag (λ(tag)(set! tag-b tag)) void void)
              (obj:ref nd:type a nd:key:children (λ(children)(set! children-a children)) void void)
              (obj:ref nd:type b nd:key:children (λ(children)(set! children-b children)) void void)

              (and
                (equal? tag-a tag-b)
                (length= children-a children-b)
                (for/and(
                          [child-a children-a]
                          [child-b children-b]
                          )
                  (obj:apply* type-type nd:type '= child-a child-b)
                  )))))))

  (void (obj:set! type-type ndwat:type
    (Λ '=  (λ( a b)
             ;;(display "running ndwat::type equal on a/b:")(display a)(display " ")(display b)(newline)
             (and
               (obj:apply type-type atset:type '= (Λ a b)
                 identity
                 raise:no-such-key
                 (λ() (not (obj:has b atset:type)))  ; no-such-key is ok if b doesn't have attributes either
                 )
               (ndwat-nd-equal a b) ; nodes may have ndwat children, so ndwat has its our own nd equal
               )))))

   (define (ndwat-nd-equal a b) ; a and b are two data objects that share nd:type
      (let(
            [tag-a null]
            [tag-b null]
            [children-a null]
            [children-b null]
            )
        (obj:ref nd:type a nd:key:tag (λ(tag)(set! tag-a tag)) void void)
        (obj:ref nd:type b nd:key:tag (λ(tag)(set! tag-b tag)) void void)
        (obj:ref nd:type a nd:key:children (λ(children)(set! children-a children)) void void)
        (obj:ref nd:type b nd:key:children (λ(children)(set! children-b children)) void void)

        (and
          (equal? tag-a tag-b)
          (= (set-count children-a) (set-count children-b))
          (for/and(
                    [child-a children-a]
                    [child-b children-b]
                    )
            (obj:apply* type-type ndwat:type '= child-a child-b)
            ))))

    (define (nd-equal?-test-0)
      (let(
            [node-a (ndwat:make
                      (Λ 
                        'at:source '(lexer-ql0-name "test-session" (1 1 0) (2 1 1))
                        'at:lexeme "1"
                        'at:value 1
                        )
                      (Λ)
                      )]
            [node-b (ndwat:make
                      (Λ 
                        'at:value 1
                        'at:lexeme "1"
                        'at:source '(lexer-ql0-name "test-session" (1 1 0) (2 1 1))
                        )
                      (Λ)
                      )]
            )

        (obj:apply* type-type ndwat:type '= (Λ node-a node-b))
        ))
      (test-hook nd-equal?-test-0)


#|

  ;;;  input: a node, a-lambda and arg for the lambda
  ;;;  output: a modified node
  ;;;
  ;;; a-lambda is given the node attributes as a first argument, and other args as remaining args
  ;;; it must return the modified attributes list.
  ;;;
    (define (on-attributes t the-lambda . args)
      (nd-make* 
        (type t)
        (apply the-lambda (cons (nd-attributes t) args))
        (nd-children t)
        ))

    (define (on-children t the-lambda . args)
      (nd-make*
        (type t)
        (nd-attributes t)
        (apply the-lambda (cons (nd-children t) args))
        ))


;;--------------------------------------------------------------------------------
;; common nodes
;;
;;
  ;; node children
  ;;
  (define (nd:comment) 'nd:comment) ; the lexer creates a node for comments
  (define (nd:errsyn) 'nd:errsyn) ; a place holder a parser can add if it can't make sense of the syntax
  (define (nd:lex-other) 'nd:lex-other) ; these characters did not lex to any node
  (define (nd:number) 'nd:number)
  (define (nd:punc) 'nd:punc) ; see the lexeme for the punc char or char sequence
  (define (nd:string) 'nd:string)
  (define (nd:symbol) 'nd:symbol) ; symbol name is in the lexeme member

  (nd-hook
    (nd:null)
    (nd:comment)
    (nd:errsyn)
    (nd:lex-other)
    (nd:number)
    (nd:punc)
    (nd:string)
    (nd:symbol)
    )

   (define (punc-is n p)
     (and
       (type-is n (nd:punc))
       (equal? (nd-lexeme-1 n) (Λ p))
       ))

   (define (punc-is-test-0)
     (punc-is nd-example-2 ","))
   (test-hook punc-is-test-0)
 
   (define (symbol-is n s) 
     (and
       (type-is n (nd:symbol))
       (string=? (nd-lexeme-1 n) (Λ s))
       ))


|#
      
;;--------------------------------------------------------------------------------
;; provides
;;
;;   typed list, attributes, nodes and supporting routines
;;
  (provide 

    nd:key:tag
    nd:key:children
    nd:type

    atset:key:lexeme
    atset:key:source
    atset:key:source-nds
    atset:key:value
    atset:type

    ndwat:type
    )

  (provide-with-trace "node"

    nd:is
    nd:make

    atset:is
    atset:make

    ndwat:is
    ndwat:make

  )


