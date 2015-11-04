#|
 node
   
  created: 2014-12-04T07:55:00Z twl
  major revision: 2015-08-22T06:05:02Z twl  

  a parse tree node type

  provides 'ndwat' - 'node with attributes' 

  

|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "misc-lib.rkt")
  (require "object.rkt")


;; object:set syntax needs to be updated

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
             [objid (obj:declare-type (obj:make) type-type)]
             )
        (when (obj-debug) (obj:name-hook objid "nd:type"))
        objid
        ))

    (define (nd:is n) (obj:has-type n nd:type))

    (define (nd:make Λkey-vals) 
      (let(
            [objid (obj:make)]
            )
        (when (obj-debug) (obj:name-hook objid "nd"))
        (obj:declare-type objid nd:type)
        (cond
          [(not (null? Λkey-vals)) (obj:set! nd:type objid Λkey-vals)]
          )))

;;--------------------------------------------------------------------------------
;; attribute set
;;   an attribute is a key value pair.  We collect them in a hash table.  Each
;;   attribute can potentially compare differently and the racket hash table doesn't
;;   give us the means to do that, so here we wrap a hash table in another type
;;
  (define at:key:lexeme (obj:declare-type (obj:make) type-type)) ; value is a string
  (define at:key:source (obj:declare-type (obj:make) type-type)) ; value is a source object
  (define at:key:source-nds (obj:declare-type (obj:make) type-type)) ; value is a set of nodes
  (define at:key:value (obj:declare-type (obj:make) type-type))
                
  (define at:type
     (let(
            [objid (obj:declare-type (obj:make) type-type)]
            )
       (when (obj-debug) (obj:name-hook objid "at:type"))
       objid
       ))

    (define (at:is as) (obj:has-type as at:type))
    (define (at:make) (obj:declare-type (obj:make) at:type))


;;--------------------------------------------------------------------------------
;; node with attributes
;;
  (define ndwat:type 
    (let(
          [objid (obj:declare-type (obj:make) type-type)]
          )
      (when (obj-debug) (obj:name-hook objid "ndwat:type"))
      objid
      ))

  (define (ndwat:is n)
    (and
      (obj:has-type n nd:type)
      (obj:has-type n at:type)
      ))

  ;; Λattributes alternating key val list, Λchildren list of children
  (define (ndwat:make Λattributes Λchildren)
    (let(
          [objid (obj:make)]
          )
      (when (obj-debug) (obj:name-hook objid "ndwat"))
      (obj:declare-type objid nd:type)
      (obj:declare-type objid at:type)
      (cond
        [(not (null? Λattributes)) (obj:set! at:type objid Λattributes)]
        [(not (null? Λchildren)) (obj:set! nd:type objid (Λ nd:key:children (list->set Λchildren)))]
      )))


;;--------------------------------------------------------------------------------
;; atset methods
;;
  (void (obj:set! type-type at:type
    (Λ 

      '= (λ(a b) ; a and b are two data objects that share at:type
           ;;(displayln (Λ "running equal on two atsets: " a " " b))
           (let(
                 [a-keys (obj:keys at:type a)]
                 [b-keys (obj:keys at:type b)]
                 )
             (and
               (equal? a-keys b-keys)
               (for/and(
                         [key a-keys]
                         )
                 (let(
                       [a-value (obj:ref* at:type a key)]
                       [b-value (obj:ref* at:type b key)]
                       )
                   (cond
                     [(eqv? key at:key:source-nds) ; the source-nds attribute holds a set of nodes
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

;;--------------------------------------------------------------------------------
;; nd methods
;;
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
                  ))))
    
      

      )))





;;--------------------------------------------------------------------------------
;; ndwat methods
;;
  (void (obj:set! type-type ndwat:type
    (Λ 

      '=  (λ( a b)
            ;;(display "running ndwat::type equal on a/b:")(display a)(display " ")(display b)(newline)
            (and
              (obj:apply type-type at:type '= (Λ a b)
                identity
                raise:no-such-key-in-elementary
                (λ() (not (obj:has-type b at:type)))  ; no-such-key is ok if b doesn't have attributes either
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


;;--------------------------------------------------------------------------------
;; tests
;;

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


      
;;--------------------------------------------------------------------------------
;; provides
;;
;;   typed list, attributes, nodes and supporting routines
;;
  (provide 

    nd:key:tag
    nd:key:children
    nd:type

    at:key:lexeme
    at:key:source
    at:key:source-nds
    at:key:value
    at:type

    ndwat:type
    )

  (provide-with-trace "node"

    nd:is
    nd:make

    at:is
    at:make

    ndwat:is
    ndwat:make

  )


