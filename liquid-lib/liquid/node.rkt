#|
 node
   
  created: 2014-12-04T07:55:00Z twl

  This file defines typed list, attribute, and node.  


|#


#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "misc-lib.rkt")
  (require parser-tools/lex) ; for the position structure

;;--------------------------------------------------------------------------------
;; typed-list
;;
;; a 'list' is either null, has one 'item', or has an ordered list of items.  For
;; linquistic convience, items may also be called 'members', or 'elements'.
;;
;; The type of the null list, or a list that does not start with a symbol, is #f,
;; otherwise the type of the list is the car item.  The type is not considered
;; to be a member of the typed-list (though it is a member of the base list).
;;
;; typed-list is the parent for attributes and nodes.
;;
;;
  (define (typed-list-make the-type . the-items) (typed-list-make* the-type the-items))
  (define (typed-list-make* the-type the-items) (cons the-type the-items))
  
  (define (typed-list-is-well-formed tlst [a-type-enumeration '()])
    (and
      (pair? tlst)
      (let(
            [should-be-type (car tlst)]
            )
        (and
          (symbol? should-be-type)
          (or
            (null? a-type-enumeration)
            (memv should-be-type a-type-enumeration)
            )))))

  (define (type a-typed-list) (car a-typed-list))
  (define (value a-typed-list) (cdr a-typed-list))
  (define (type-is t a-type) (eqv? (type t) a-type))

  (define (type-is-test-0) (type-is (list 'a 1 2) 'a))
  (test-hook type-is-test-0)

  (define (type-is-test-1) (not (type-is '(b 2 3) 'a)))
  (test-hook type-is-test-1)

  (define (typed-list-test-0)

      (typed-list-make 'a 1 2)
      '(a 1 2)))  ; doesn't actually do much ;-), but user is required to provide a type or function args won't match
  (test-hook typed-list-test-0)

  ;; these are used in tests further down
  (define typed-list-example-0 (typed-list-make 'a 1 2))
  (define typed-list-example-1 (typed-list-make 'b 3 4))


;;--------------------------------------------------------------------------------
;; attribute
;;
;;  Basically an attribute is just a typed list that is used in the context of anotating
;;  nodes. Parsing places some constraints on attributes that do not exist for typed
;;  lists in general, for example, requiring the type to be part of an attibute type
;;  enumeration that is separate from the toekn type enumeration.  We are required to
;;  keep a source attribute, and it displays differently than typed lists.  Also this
;;  makes code a little clearer
;;

  (define (attribute-make the-type . items ) (typed-list-make* the-type items))
  (define (attribute-make* the-type items ) (typed-list-make* the-type items))

  (define (at:lexeme) 'at:lexeme)
  (define (at:source) 'at:source)
  (define (at:value) 'at:value)
  (define attribute-type-enumeration
    (list
      (at:lexeme)
      (at:source)
      (at:value)
      ))
  (define (is-attribute-type test-type) (memv test-type attribute-type-enumeration))

  (define (attribute-hook . introduced-types) (attribute-hook* introduced-types))
  (define (attribute-hook* introduced-types) (set! attribute-type-enumeration (append attribute-type-enumeration introduced-types)))

  (define (attribute-is-well-formed at [the-attribute-type-enumeration attribute-type-enumeration])
    (typed-list-is-well-formed at the-attribute-type-enumeration))


;;--------------------------------------------------------------------------------
;; ascribed attributes - an attribute container
;;
;;   ascribed attributes is just a list of attributes
;;
;;    the behavior of adding an attribute in the presence of another attribute
;;    with the same type may depend on the attribute type or even external factors
;;

  ;; true if some attribute in the aa list has the given type, otherwise false
  (define (has-attribute aa type) (pair? (assv type aa)))

  ;; returns list of attributes in aa with given type
  (define (get-attribute aa type) (filter (λ(e)(eqv? (car e) type)) aa))

  ;; removes all attributes of a given type, if any
  (define (remove-attribute aa a-type) (filter (λ(e)(not (type-is e a-type))) aa))

  ;; ascribes an attribute or attributes
  (define (ascribe-attribute aa . new-at) (attribute-acribe* aa new-at))
  (define (ascribe-attribute* aa new-ats) (append new-ats aa))

  ;; removes and acribes
  (define (update-attribute aa new-at) (insert (remove-attribute aa (type new-at)) new-at))

  ;; returns (first rest) for an attribute
  (define (attribute-iterate aa)
    (cond
      [(null? aa) '()]
      [else
        (let([the-type (type (first aa))])
          (list
            (get-attribute aa the-type)
            (remove-attribute aa the-type)
            ))
        ]))

  (define (test-attribute-iterate-0)
    (let(
          [a1  '(a1 1 2 3)]
          [a10 '(a10 10 20 30)]
          [a11 '(a10 11 21 31)]
          [a2  '(a2 7 89)]
          )
      (let*(
             [i0 (list a1 a10 a11 a2)]
             [i1 (attribute-iterate i0)]
             [i2 (attribute-iterate (second i1))]
             [i3 (attribute-iterate (second i2))]
             [i4 (attribute-iterate (second i3))]
             )
        (and
          (equal? i1 (list (list a1) (list a10 a11 a2)))
          (equal? i2 (list (list a10 a11) (list a2)))
          (equal? i3 (list (list a2) '()))
          (equal? i4 '())
          ))))
  (test-hook test-attribute-iterate-0)

  ;; returns appended values from list of typed lists
  (define (gather-values aa)
    (foldr (λ(e r) (append (cdr e) r)) '() aa))

  (define (test-gather-values-0)
    (equal?
      (append-values '((a 1 2 3) (b 10 20) (c 101 102)))
      '(1 2 3 10 20 101 102)
      ))
  (test-hook test-gather-values-0)
             

  ;; there is no order between attributes
  (define (ascribed-attribues-equal? a b) (set= (list->set a) (list->set b)))

  (define (test-ascribed-attributes-equal?-0)
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
        (ascribed-attributes-equal? i0 i1))))
  (test-hook test-ascribed-attributes-equal?-0)

  (define (test-ascribed-attributes-equal?-1)
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
        (not (ascribed-attributes-equal? i0 i1)))))
  (test-hook test-ascribed-attributes-equal?-1)


;;-----------------------------------------------------------------------------------------
;; source attribute
;;

  ;; maybe we should just keep the pos structure instead of turning this into a list ..
  ;;
  ;; the position struct is defined in the module parser-tools/lex.
  ;; a position within a file is given by offset, line, and character.
  ;;
    (define (position-deconstruct p)  (map (λ(f) (f p)) (list position-offset position-line position-col)))
    (define (position-null) '(0 0 0))

    ;; these are used in tests
    (define position-example-0 `(12 3 10))
    (define position-example-1 `(80 7 3))
    (define position-example-3 `(427 6 11))
    (define position-example-4 `(728 10 2))

    (define (position-is-well-formed pos)
      (and
        (length= pos 3)
        (andmap number? pos)
        ))

    ;; generator is either a hash key for more info, a program name, or an interesting symbol
    (define (source-generator src) (list-ref src 1)) ;; typically the name of the parser
    (define (source-pathname src)  (list-ref src 2)) ;; end of path might be a file name, or variable name in a file
    (define (source-start src) (list-ref src 3))
    (define (source-end src)   (list-ref src 4))

    (define (source-is-well-formed src) ; seems this could be improved
      (define (is-symbol-or-string s) (or (symbol? s) (string? s)))
      (and
        (attribute-is-well-formed src)
        (length= src 5)
        (let-values(
                     [(type generator file start end) (apply values src)]
                     )
          (and
            (type-is src (at:source))
            (is-symbol-or-string generator)
            (is-symbol-or-string file)
            (position-is-well-formed start)
            (position-is-well-formed end)
            ))))

    (define (source-make generator file start end)
      (attribute-make (at:source) generator file start  end)
      )

    (define source-example-0 (source-make 'source-example-0 "node.rkt" position-example-0 position-example-1))
    (define source-example-1 (source-make 'source-example-1 "node.rkt" position-example-3 position-example-4))


;;--------------------------------------------------------------------------------
;; nodes
;;
;;  A node is a typed list with ascribed attributes, where value items, if any, are also nodes.
;;
;;  Each node should have a at least one attribute called the 'at:source' that tells where
;;  in the source file the node lexeme came from.  Many nodes carry an 'at:lexeme' so as
;;  to be able to quote the source in errors.  Many nodes have an 'at:value' which holds a
;;  a numeric interpreation of the node or some such. There is native support for 'at:source'
;;  'at:lexeme'  and 'at:value'.
;;
  (define (nd-make the-type attributes . children)  (nd-make* the-type attributes children))
  (define (nd-make* the-type attributes [children '()]) (typed-list-make* the-type (cons attributes children)))

  (define (nd:null) 'nd:null) ; this node has no semmantic
  (define (nd:example-0) 'nd:example-0)
  (define (nd:example-1) 'nd:example-1)

  (define nd-type-enumeration
    (list
      (nd:null)
      (nd:example-0)
      (nd:example-1)
      ))
  (define (is-nd-type test-type) (memv test-type nd-list))

  (define (nd-hook . nd-types) (nd-hook* nd-types))
  (define (nd-hook* nd-type-list) (set! nd-list (append nd-list nd-type-list)))

  (define (nd-children t) (cddr t))
  (define (nd-attributes t) (cadr t))

  (define (nd-is-well-formed n)
    (and 
      (typed-list-is-well-formed nl nd-type-enumertion))
      (length≥ n 2)
      (pair? (nd-attributes n))
      (andmap attribute-is-well-formed (nd-attributes n))
      (pair? (nd-children n))
      ;;; (andmap nd-is-well-formed (nd-children n))  ; noo intensive, do nhis instead:
      (andmap (λ(e)(typed-list-is-well-formed e nd-type-enumeration)) (nd-children n))
      ))

  ;; used in testing
  (define (nd-equal? x y)
    (and
      (eqv? (type x) (type y))
      (equal? (nd-children x) (nd-children y))
      (ascribed-attributes-equal? (nd-attributes x) (nd-attributes y))
      ))

  ;;;  input: a node, a-lambda and arg for the lambda
  ;;;  output: a modified node
  ;;;
  ;;; a-lambda is given the node attributes as a first argument, and other args as remaining args
  ;;; it must return the modified attributes list.
  ;;;
    (define (on-attributes t the-lambda . args)
      (nd-make 
        (type t)
        (apply a-lambda (cons (nd-attributes t) args))
        (nd-children t)
        ))

    (define (on-children t the-lambda . args)
      (nd-make 
        (type t)
        (nd-attributes t)
        (apply a-lambda (cons (nd-children t) args))
        ))

;;--------------------------------------------------------------------------------
;;
;; utilities for making nodes
;;
  ;; makes a node with a source attribute
  ;;
    (define (nd-make-source the-type the-source) 
      (let*(
             [t0 (nd-make the-type '())]
             [t1 (nd-ascribe-attribute t0 the-source)]
             )
        t1))

  ;; makes a node with source and value attributes, the value of the value attribute is passed in
  ;;
    (define (nd-make-value the-type the-source v)
      (let*(
             [new-tok (nd-make-source the-type the-source)]
             [value-attribute (attribute-make (at:value) v)]
             )
        (nd-ascribe-attribute new-tok value-attribute)
        ))

  ;; used by the lexer to make nodes
  ;;
    (define (nd-make-lex the-type start end lexeme)
      (let*(
             [source-at (source-make (source-generator-lex) (current-file-name) start end)]
             [lexeme-at (attribute-make (at:lexeme) lexeme)]
             [nd0     (nd-make-source the-type source-at)]
             [nd1     (nd-ascribe-attribute nd0 lexeme-at)]
             )
        nd1
        ))

  ;; used by the parser to make nodes
  ;;
    (define (nd-make-parse ts the-type generator)
      (let*(
             [source-at
               (source-make
                 generator
                 (current-file-name)
                 (nd-start-pos* (car ts))
                 (nd-end-pos* (last ts))
                 )]
             [nd0 (nd-make-source the-type source-at)]
             )
        nd0
        ))

  ;; the tok examples are used in testing
  (define nd-example-0
    (nd-make-source
      (nd:example-0) 
      (source-make 'example-0 "node.rkt" position-example-0 position-example-1)
      ))
   (define (nd-make-test-0)
     (nd-equal?
       nd-example-0
       `(nd:example-0 
          ((at:source example-0 "node.rkt" (12 3 10) (80 7 3)))
          )))
   (test-hook nd-make-test-0)


  (define nd-example-2
    (nd-make
      'nd:punc
      (list
        (source-make 'example-2 "node.rkt" position-example-0 position-example-4)
        (attribute-make 'at:lexeme ",")
        )
      ))

  (define nd-example-1
    (nd-make
      (nd:example-1) 
      (list
        (source-make 'example-1 "node.rkt" position-example-3 position-example-4)
        )
      nd-example-0
      nd-example-2
      ))

;;--------------------------------------------------------------------------------
;;
;;  utilities for manipulating nodes
;;
                                                    
   (define (nd-source t) (get-attribute (nd-attributes t) (at:value)))
   (define (nd-lexeme t) (get-attribute (nd-attributes t) (at:lexeme)))
   (define (nd-value t) (get-attribute (nd-attributes t) (at:value)))

   ;; commonly a parser guarantees there will be exactly one source, lexeme or at:value
   ;; these routines have that assumption built in
   ;;
     (define (nd-source-1 t) (value (first (get-attribute (nd-attributes t) (at:value)))))
     (define (nd-lexeme-1 t) (value (first (get-attribute (nd-attributes t) (at:lexeme)))))
     (define (nd-value-1 t) (value (first ((get-attribute (nd-attributes t) (at:value))))))

   (define (nd-start-pos* t)
     (let(
           [source-attribute (on-attribute t get-attribute (at:source))]
           )
       (by-arity source-attribute
         (λ() (position-null))
         (λ(a) (source-start a))
         (λ(a-list) (map source-start a-list))
         )))

   (define (nd-end-pos* t)
     (let(
           [source-attribute (on-attribute t get-attribute (at:source))]
           )
       (by-arity source-attribute
         (λ() (position-null))
         (λ(a) (source-end a))
         (λ(a-list) (map source-end a-list))
         )))

;;--------------------------------------------------------------------------------
;; routines for matching nodes
;;  
;;   these are to be used as predicates for the '?' operator in match
;;
;;   match predicates take one operand so these routines return a function that takes one
;;   operand
;;
  (define ($nd-type-is type)
    (λ(t) (and (pair? t) (type-is t type))))

  (define ($nd-has-attribute attribute-type)
    (λ(t) (and (pair? t) (length≥ t 2) (has-attribute (nd-attributes t) attribute-type))))

  (define ($nd-has-value nd-type attribute-type . attribute-val)
     (λ(t)
       (and
         (nd-is-well-formed t)
         (type-is t nd-type)
         (cond 
           [(null? attribute-val) (has-attribute (nd-attributes t) attribute-type)]
           [else
             (let(
                   [qualified-ats (get-attribute (nd-attributes t) attribute-type)]
                   )
               (ormap (λ(at)(equal? (value at) attribute-val)) qualified-ats)
               )]))))

   (define ($nd-has-value-test-0)
     (let*(
            [t    nd-example-2]
            [pred ($nd-has-value 'nd:punc 'at:lexeme ",")]
            )
       (pred t)
       ))
   (test-hook $nd-attribute-test-0)


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

   (define (punc-is t p)
     (and
       (type-is t (nd:punc))
       (string=? (nd-lexeme-1 t) p)
       ))

   (define (punc-is-test-0)
     (punc-is nd-example-2 ","))
   (test-hook punc-is-test-0)
 
   (define (symbol-is t s) 
     (and
       (type-is t (nd:symbol))
       (string=? (nd-lexeme-1 t) s)
       ))


      
;;--------------------------------------------------------------------------------
;; error facilities
;;

  ;;attributes
  ;;
    ;;. a rule can create try nodes, maybe keeping them, so we keep the error message in the node
    ;;. then later we scan all the error messages out
    ;;.
    (define (attribute:errsyn-mess) 'attribute:errsyn-mess) ; syntax error message

    ;;. a higher level parse, for example the parser that looks at the lexer output, may not
    ;;. be able to make sense out of a series of tokens, and can put those tokens in this attribute
    (define (attribute:errsyn-nds) 'attribute:errsyn-nds)

    (attribute-hook
      (attribute:errsyn-mess) 
      (attribute:errsyn-nds)
      )

  ;;. in order to facilitate errsyn reporting we give rules  an 'imperative' setting
  ;;. withtout imparatives grammar rules will simply not match when there is an error
  ;;.
    (define (imperative:test) 'imperative:test) ; on nomatch, returns false there can be no errsyn
    (define (imperative:force) 'imperative:force) ; on nomatch, returns place holder with errsyn
    (define (imperative:committed) 'imperative:committed) ; on nomatch, rule decides to return #f or an errsyn

  ;;. adds an error message to a node
  ;;.
  ;;.    input: ns the node is made from, the node we are to append to, the message
  ;;.    returns: the node with members added
  ;;.
  ;;.  The ts are only for annotation of the error, and are not read.
  ;;.  If you do not have the ts use a null list
  ;;.
    (define (append-errsyn ns n mess)
      (let*(
            [a0 (attribute-make (attribute:errsyn-mess) mess)]
            [n0 (on-attributes n bcons a0)]
            )
        (cond
          [(null? ns) n0]
          [else
            (let*(
                  [a1 (attribute-make* (attribute:errsyn-nds) ns)]
                  [n1 (on-attributes n0 bcons a1)]
                  )
              n1
              )]
          )))

  (define (append-errint ns n mess)
    (let*(
           [umess (string-append "internal errsyn when parsing" (if (string=? mess "") "" " ") mess)]
           [err-nd (append-errsyn ns n umess)]
            )
      (log (nd>string err-nd))
      err-nd
      ))


  ;;. makes a new node of nd-type and appends errsyn attribute with message
  ;;.
  ;;.  input:  ts for the source position information,  etc.
  ;;.  output:  a new node
  ;;.
    (define (nd-make-errsyn ts nd-type generator message)
      (let (
            [new-nd (nd-make-parse ts nd-type generator)]
            )
        (append-errsyn ts new-nd message)
        )
      )

    (define (nd-make-errint ns nd-type generator mess)
      (let* (
              [umess (string-append "internal errsyn when parsing" (if (string=? mess "") "" " ") mess)]
              [err-nd (nd-make-errsyn ns nd-type generator umess)]
              )
        (log (nd>string err-nd))
        err-nd
        ))


  ;;. a grammar rule that either returns #f or a syntax error node depending on the
  ;;. value of 'imparative' 
  ;;.
    (define (rule-errsyn ts imperative nd-type generator message)
      (cond
         [(eqv? imperative (imperative:test)) #f]
         [(eqv? imperative (imperative:force))
           (nd-make-errsyn ts nd-type generator message)
           ]
         [else
           (rule-errparser ts imperative nd-type 'rule-errsyn "imperative selected else")
         ]))

  ;;. same as above but the message is set to 'expected <type> but got: '
  ;;.   input: the nodes being parsed, etc.
  ;;.   output: #f, or a new node of the given nd-type with an errsyn member
  ;;.
    (define (rule-errsyn-expected ts imperative nd-type generator)
      (rule-errsyn ts imperative nd-type generator 
        (string-append "expected " (symbol->string nd-type) " but found:")
        ))

  ;;. when the problem is our parser not the syntax of the thing we are parsing
  ;;.   oops our parser has a bug, we will log an error and say there is no match
  ;;.   if the imparative is forced, then we return an error node
  ;;.
    (define (rule-errparser ts imperative nd-type generator [mess ""])
      (let* (
            [err-tok (nd-make-errint ts nd-type generator mess)]
            )
      (cond
         [(eqv? imperative (imperative:test)) #f]
         [else err-tok]
         )
      ))

  ;; flat check for error conditions on a node or list of nodes
  ;;    see filter.rkt (filter-nd-err) for a tree search
  ;;
    (define (nd-has-err t) 
      (or
        (not (nd-is-well-formed t))
        (type-is t (nd:errsyn))
        (pair? (nd-find-attribute (attribute:errsyn-mess) t))
        ))

    (define (nds-has-err ts)
      (and
        (not (null? ts))
        (ormap nd-has-err ts)
        ))
    ;note test framed-item-sep-test-2 & 3 in "parser-framing.rkt"

      
;;--------------------------------------------------------------------------------
;; provides
;;
;;   typed list, attributes, nodes and supporting routines
;;
(provide (all-defined-out))

#|
  (provide 
    )

(provide-with-trace "node"

  ;; typed list
  ;;
    typed-list-make
    typed-list-make*
    typed-list-is-well-formed

    type 
    value
    type-is

  ;; attribute
  ;;
    attribute-make
    attribute-make*

    at:lexeme
    at:source
    at:value

    attribute-hook
    attribute-hook*
    attribute-is-well-formed

  ;; ascribed attribute list
  ;;
    get-attribute
    attribute-iterate
    gather-values


  ;; source attribute
  ;;
    position-deconstruct
    position-null

    position-is-well-formed

    source-generator
    source-pathname
    source-start
    source-end

    source-is-well-formed
    source-make

  )
  
|#
