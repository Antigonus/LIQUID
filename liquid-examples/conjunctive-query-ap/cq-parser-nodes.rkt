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


;; object:set syntax needs to be updated

;;--------------------------------------------------------------------------------
;; node
;;
  ;; place shared data and functions for all nodes here
  ;; if there is an enumeration of allowed fields for the node type, it would go in here
  ;;
    (define nd:field:tag 'nd:field:tag)
    (define nd:field:children 'nd:field:children) ; set of children

    (define nd:type 
      (let(
             [objid (obj:add-type (obj:make) type-type)]
             )
        (when obj:debug (obj:name-hook objid "nd:type"))
        objid
        ))

    (define (nd:is n) (obj:has-type n nd:type))

    (define (nd:make Λfield-vals) 
      (let(
            [objid (obj:make)]
            )
        (when obj:debug (obj:name-hook objid "nd"))
        (obj:add-type objid nd:type)
        (cond
          [(not (null? Λfield-vals)) (obj:set! nd:type objid Λfield-vals)]
          )))

;;--------------------------------------------------------------------------------
;; attribute set
;;   an attribute is a field value pair.  We collect them in a hash table.  Each
;;   attribute can potentially compare differently and the racket has table doesn't
;;   give us the means to do that, so here we wrap a hash table in an  object
;;
  (define atset:field:lexeme (obj:add-type (obj:make) type-type)) ; value is a string
  (define atset:field:source (obj:add-type (obj:make) type-type)) ; value is a source object
  (define atset:field:source-nds (obj:add-type (obj:make) type-type)) ; value is a set of nodes
  (define atset:field:value (obj:add-type (obj:make) type-type))
                
  (define atset:type
     (let(
            [objid (obj:add-type (obj:make) type-type)]
            )
       (when obj:debug (obj:name-hook objid "atset:type"))
       objid
       ))

    (define (atset:is as) (obj:has-type as atset:type))
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
      (obj:has-type n nd:type)
      (obj:has-type n atset:type)
      ))

  ;; Λattributes alternating field val list, Λchildren list of children
  (define (ndwat:make Λattributes Λchildren)
    (let(
          [objid (obj:make)]
          )
      (when obj:debug (obj:name-hook objid "ndwat"))
      (obj:add-type objid nd:type)
      (obj:add-type objid atset:type)
      (cond
        [(not (null? Λattributes)) (obj:set! atset:type objid Λattributes)]
        [(not (null? Λchildren)) (obj:set! nd:type objid (Λ nd:field:children (list->set Λchildren)))]
      )))


;;--------------------------------------------------------------------------------
;; atset methods
;;
  (void (obj:set! type-type atset:type
    (Λ 

      '= (λ(a b) ; a and b are two data objects that share atset:type
           ;;(displayln (Λ "running equal on two atsets: " a " " b))
           (let(
                 [a-fields (obj:fields atset:type a)]
                 [b-fields (obj:fields atset:type b)]
                 )
             (and
               (equal? a-fields b-fields)
               (for/and(
                         [field a-fields]
                         )
                 (let(
                       [a-value (obj:ref* atset:type a field)]
                       [b-value (obj:ref* atset:type b field)]
                       )
                   (cond
                     [(eqv? field atset:field:source-nds) ; the source-nds attribute holds a set of nodes
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
              (obj:ref nd:type a nd:field:tag (λ(tag)(set! tag-a tag)) void void)
              (obj:ref nd:type b nd:field:tag (λ(tag)(set! tag-b tag)) void void)
              (obj:ref nd:type a nd:field:children (λ(children)(set! children-a children)) void void)
              (obj:ref nd:type b nd:field:children (λ(children)(set! children-b children)) void void)

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
              (obj:apply type-type atset:type '= (Λ a b)
                identity
                raise:no-such-field-in-elementary
                (λ() (not (obj:has-type b atset:type)))  ; no-such-field is ok if b doesn't have attributes either
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
        (obj:ref nd:type a nd:field:tag (λ(tag)(set! tag-a tag)) void void)
        (obj:ref nd:type b nd:field:tag (λ(tag)(set! tag-b tag)) void void)
        (obj:ref nd:type a nd:field:children (λ(children)(set! children-a children)) void void)
        (obj:ref nd:type b nd:field:children (λ(children)(set! children-b children)) void void)

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

    nd:field:tag
    nd:field:children
    nd:type

    atset:field:lexeme
    atset:field:source
    atset:field:source-nds
    atset:field:value
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


