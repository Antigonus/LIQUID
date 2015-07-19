#|
 tokens
   
  created: 2014-12-04T07:55:00Z twl

  This file defines typed list, attribute, and token.  

  We implement our class types with lists.

  The car element of a typed list is a type name.

  Attributes are typed lists where the type is found in a list of legal attributes. The 
  remaining format for an attribute depends on the attribute, often they are duck typed.
  Attributes are typically used to annotate tokens with information such as error 
  messages. They are not intended to be structural.

  A token has attributes and children tokens.  The children token list facilitates
  building trees. 

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
;; linquistic convience, items may also be called 'members'.
;;
;; The type of the null list, or a list that does not start with a symbol, is #f,
;; otherwise the type of the list is the car item.  The type is not considered
;; to be a member of the typed-list (though it is a member of the base list).
;;
;; typed-list is the parent for attributes and tokens.
;;
;;
  (define typed-list list)
  (define (typed-list-well-formed? l) (and (pair? l) (symbol? (car l))))
  (define (typed-list-make the-type . the-items) (typed-list-make* the-type the-items))
  (define (typed-list-make* the-type the-items) (cons the-type the-items))
  
  (define (type t) (and (typed-list-well-formed? t) (car t)))
  (define (items a-typed-list) (and (typed-list-well-formed? a-typed-list) (cdr a-typed-list)))

  (define (type-is a-type t) (eqv? (type t) a-type))

  (define (type-is-test-0) (type-is 'a (list 'a 1 2)))
  (test-hook type-is-test-0)

  (define (type-is-test-1) (type-is #f 1))
  (test-hook type-is-test-1)

  (define typed-list-example-0 (typed-list-make 'a 1 2))
  (define typed-list-example-1 (typed-list-make* 'b '(3 4)))

  ;;----------------------------------------
  ;; find
  ;;
  
  ;;  input: a typed list where the members are potentially typed lists
  ;; output: a list of members from that typed list that match the given type
  ;;
  ;;
    (define (member-find type-to-match member-list) 
      (let*(
             [pred (λ(member) (type-is type-to-match member))]
             [marked-members (filter pred member-list)]
             )
         marked-members
        ))

  ;;  input: same as for member-find
  ;; output: the list of all items from the matching members
  ;;
    (define (member-find-items type-to-match member-list) 
      (let*(
             [marked-members  (member-find type-to-match member-list)]
             [items-only      (map items marked-members)]
             [item-collection (apply append items-only)]
             )
        item-collection
        ))

    (define (member-find-items-test-0)
      (equal? 
        (member-find-items 'a (tok (tok:null) 0 typed-list-example-0 typed-list-example-1))
        (list 1 2)))
    (test-hook member-find-items-test-0)

    (define (member-find-items-test-1)
      (null?
        (member-find-items 'c (tok (tok:null) 0 typed-list-example-0 typed-list-example-1))))
    (test-hook member-find-items-test-1)

  ;; can be used as a 'cdr' for member-find
  ;;
  ;;  input: as for member-find
  ;; output: the members other than the ones member-find returns
  ;;
    (define (member-find-complement type-not-to-match member-list) 
      (let*(
             [pred (λ(member) (not (type-is type-not-to-match member)))]
             [marked-members (filter pred  member-list)]
             )
        marked-members
        ))

    (define (member-find-complement-test-0)
      (equal? 
        (member-find-complement 'a (tok (tok:null) 0 typed-list-example-0 typed-list-example-1))
        '(tok:null 0 (b 3 4))
        ))
   (test-hook member-find-complement-test-0)


;;--------------------------------------------------------------------------------
;; attributes
;;
;;  The intended purpose of attributes is as annotation on tokens.  This annotation is
;;  intended to be easily in minor revisions and dynamically.
;;
;;  An attribute is a typed list.  The type of the typed list is called the attribute's
;;  'property'.  All attribute types may be found in the global 'property-list' given below.
;;  The items in such a typed list are said to be the attribute's 'value'.
;; 
;;  Format of an attribute value is property dependent.  
;;
  (define attribute typed-list)
  (define (attribute-well-formed? at) 
    (and
      (typed-list-well-formed? at)
      (memv (type at) attribute-list)
      ))

  (define (attribute-property a) (type a))
  (define (attribute-property-is a p) (eqv? (type a) p))
  (define (attribute-value a) (items a))

  (define (attribute-make the-type . items ) (typed-list-make* the-type items))
  (define (attribute-make* the-type items ) (typed-list-make* the-type items))
  (define (attributes m)(items m))

  (define (attribute:lexeme) 'attribute:lexeme)
  (define (attribute:source) 'attribute:source)
  (define (attribute:value) 'attribute:value)

  (define attribute-list 
    (list
      (attribute:lexeme)
      (attribute:source)
      (attribute:value)
      ))
  (define (attribute-hook . properties) (attribute-hook* properties))
  (define (attribute-hook* property-list) (set! attribute-list (append attribute-list property-list)))
  
  (define (property? proposed-property) (memv proposed-property attribute-list))
  (define (attribute? proposed-at) (attribute-well-formed? proposed-at))

  (define (attribute->string a)
    (cond 
      [(attribute-property-is a (attribute:source)) (source->string a)]
      [else
        (let(
              [os (open-output-string)]
              )
          (write a os)
          (get-output-string os))
        ]
      ))

  ;;----------------------------------------
  ;; we use this in the source attribute shown below
  ;;
  ;; the position struct is defined in the module parser-tools/lex.
  ;; a position within a file is given by offset, line, and character.
  ;;
    (define (position-deconstruct p)  (map (λ(f) (f p)) (list position-offset position-line position-col)))
    (define (position->string p) (display p))
    (define (position-null) (0 0 0))

    (define position-example-0 `(12 3 10))
    (define position-example-1 `(80 7 3))
    (define position-example-3 `(427 6 11))
    (define position-example-4 `(728 10 2))

  ;;-------------------------------------------------
  ;; source attribute
  ;;
    (define (source-well-formed src)
      (and
        (attribute-well-formed? src)
        (length≥ src 5)))

    (define (source-generator src)      (and (source-well-formed src) (list-ref src 1)))
    (define (source-file src)           (and (source-well-formed src) (list-ref src 2)))
    (define (source-start-position src) (and (source-well-formed src) (list-ref src 3)))
    (define (source-end-position src)   (and (source-well-formed src) (list-ref src 4)))

    (define (source-make generator file start end)
      (attribute-make (attribute:source) generator file start  end)
      )


    ;; some predefined values for the generator
    ;;
    (define (source-generator-test) 'source-generator-test)
    (define (source-generator-lex) 'source-generator-lex)
    (define (source-generator-parser) 'source-generator-parser)

    (define source-example-0 (source-make 'source-example-0 "tokens.rkt" position-example-0 position-example-1))
    (define source-example-1 (source-make 'source-example-1 "tokens.rkt" position-example-3 position-example-4))

    (define (source->string s)
      (define os (open-output-string))
      (write s os)
      (begin0
        (get-output-string os)
        (close-output-port os)
        ))

    (define (source->string-test-0)
      (let*(
            [x0 source-example-1]
            [s  (string-append "`" (source->string x0))] 
            [x1 (eval (read (open-input-string s)))]
            )
        ;(writeln x0 s x1)
        (.eq. x0 x1)
        ))
    (test-hook source->string-test-0)


;;--------------------------------------------------------------------------------
;; tokens
;;
;;  A token is typed-list with a special cadr item. (The car items is the type.)
;;  The cadr item is an attribute list.  Third and remaining tokens are token children.
;;
;;  Token children are traversed when traversing a parse tree. At this time there are no
;;  attributes that are examined for tokens during traversal.
;;
;;  Each token should have a at least one attribute called the 'attribute:source' that
;;  tells where in the source file the token appears.  Many tokens have an 'attribute:value'  which
;;  holds a 'token value'.  Whether at token has a value, and the format of that value depends
;;  upon the token type.
;;
;;
  (define (.eq. x y) (equal? x y)) ; need to expand this to do position independent compare on attributes


  (define tok typed-list) 
  (define (tok-well-formed? t) 
    (and 
      (typed-list-well-formed? t)
      (length≥ t 2)
      (assv (attribute:source) (tok-attributes t))
      (andmap tok-well-formed? (tok-children t))
      ))

  (define (tok:null) 'tok:null) ; this token has no semmantic
  (define (tok:example-0) 'tok:example-0)
  (define (tok:example-1) 'tok:example-1)

  (define tok-list 
    (list
      (tok:null)
      (tok:example-0)
      (tok:example-1)
      ))
  (define (tok-type? y) (memv y tok-list))

  ;; for performance reasons may want to change this just to look in the tok-list for the type
  (define (tok? t) (tok-well-formed? t))

  (define (tok-hook . tok-types) (tok-hook* tok-types))
  (define (tok-hook* tok-type-list) (set! tok-list (append tok-list tok-type-list)))

  (define (tok-children t) (drop t 2))
  (define (tok-attributes t) (cadr t))

  (define (tok->string t)
    (cond
      [(not t) #f]
      [(not (tok-well-formed? t)) "`tok-not-well-formed"]
      [else
        (let(
              [y (type t)]
              [a (tok-attributes t)]
              [c (tok-children t)]
              )
          (let(
                [ys (symbol->string y)]
                [as (map attribute->string a)]
                [cs (map tok->string c)]
                )
            (string-append "(" ys " (" (apply string-append as)  ")" (apply string-append cs) ")")
            ))
        ]
      ))
  (define (tok->string-test-0)
      (let*(
            [x0 tok-example-0]
            [s  (tok->string x0)] 
            [x1 (eval (read (open-input-string (string-append "`" s))))]
            )
        ;(writeln x0 s x1)
        (.eq. x0 x1)
        ))
  (test-hook tok->string-test-0)


  (define (tok->string* t) (string-append (tok->string t) "\n"))

  (define (toks->string ts) (apply string-append (map tok->string ts)))
  (define (toks->string* ts) (apply string-append (map tok->string* ts)))

  (define (tok-append-child t child) (tok-append-children t child))
  (define (tok-append-children t . children) (tok-append-children* t children))
  (define (tok-append-children* t children) (append t children))

  (define (tok-prepend-child t child)
    (tok-append-children*
      (tok-make (type t) (tok-attributes t))
      (cons child (tok-children t))
      ))

  (define (tok-prepend-child-test-0)
    (.eq.
      (tok-prepend-child tok-example-0 tok-example-2)
      `(tok:example-0 
        ((attribute:source example-0 "tokens.rkt" ,position-example-0 ,position-example-1))
        ,tok-example-2
         )
      ))
  (test-hook tok-prepend-child-test-0)

  (define (tok-append-attribute t at) (tok-append-attributes t at))
  (define (tok-append-attributes t . ats) (tok-append-attributes* t ats))
  (define (tok-append-attributes* t ats)
    (let*(
          [t0 (tok (type t) (append (tok-attributes t) ats))]
          [t1 (append t0 (tok-children t))]
          )
      t1
      ))

  (define (tok-make* the-type attributes [children '()]) 
    (typed-list-make* the-type (cons attributes children)))

  (define (tok-make the-type attributes . children) 
    (tok-make* the-type attributes children))

  (define (tok-make-source the-type the-source) 
    (let*(
           [t0 (tok-make the-type '())]
           [t1 (tok-append-attributes t0 the-source)]
           )
      t1))

  (define tok-example-0
    (tok-make-source
      (tok:example-0) 
      (source-make 'example-0 "tokens.rkt" position-example-0 position-example-1)
      ))
   (define (tok-make-test-0)
     (.eq.
       tok-example-0
       `(tok:example-0 
          ((attribute:source example-0 "tokens.rkt" (12 3 10) (80 7 3)))
          )))
   (test-hook tok-make-test-0)


  (define tok-example-2
    (tok-make
      'tok:punc
      (list
        (source-make 'example-2 "tokens.rkt" position-example-0 position-example-4)
        (attribute-make 'attribute:lexeme ",")
        )
      ))

  (define tok-example-1
    (tok-make
      (tok:example-1) 
      (list
        (source-make 'example-1 "tokens.rkt" position-example-3 position-example-4)
        )
      tok-example-0
      tok-example-2
      ))


  ;; makes a token with a value attribute
    (define (tok-make-value the-type the-source v)
      (let*(
             [new-tok (tok-make-source the-type the-source)]
             [value-attribute (attribute-make (attribute:value) v)]
             )
        (tok-append-attributes new-tok value-attribute)
        ))

  ;; used by the lexer to make tokens
    (define (tok-make-lex the-type start end lexeme)
      (let*(
             [source-at (source-make (source-generator-lex) (current-file-name) start end)]
             [lexeme-at (attribute-make (attribute:lexeme) lexeme)]
             [tok-0     (tok-make-source the-type source-at)]
             [tok-1     (tok-append-attributes tok-0 lexeme-at)]
             )
        tok-1
        ))

  ;; used by the parser to make tokens
    (define (tok-make-parse ts the-type generator)
      (let*(
             [source-at
               (source-make
                 generator
                 (current-file-name)
                 (tok-start-pos* (car ts))
                 (tok-end-pos* (last ts))
                 )]
             [tok-0 (tok-make-source the-type source-at)]
             )
        tok-0
        ))

   (define (tok-find-attribute-items the-type t) (member-find-items the-type (tok-attributes t)))
   (define (tok-find-attribute the-type t) (member-find the-type (tok-attributes t)))
   (define (tok-find-children the-type t)(member-find the-type (tok-children t)))

   (define (tok-attribute-is the-attribute t . items) (tok-attribute-is* the-attribute t items))
   (define (tok-attribute-is* the-attribute t items)
     (let(
           [at (tok-find-attribute-items the-attribute t)]
           )
       (.eq. at items)))

   (define (tok-attribute-is*-test-0)
     (let(
           [t tok-example-2]
           )
       (tok-attribute-is* 'attribute:lexeme t (list ","))))
   (test-hook tok-attribute-is*-test-0)
         
                                                    
   (define (tok-value t)  (member-find-items (attribute:value) (tok-attributes t)))
   (define (tok-lexeme t) (member-find-items (attribute:lexeme) (tok-attributes t)))

   (define (tok-start-pos* t)
     (let(
           [source-attribute (tok-find-attribute (attribute:source) t)]
           )
       (cond
         [(null? source-attribute) (position-null)]
         [else
           (if (length> source-attribute 1) 
             (log 
               (string-append
                 "tok-start-pos* t has more than one source attribute: "
                 (tok->string t)
                 ))
             (void))
           (source-start-position (car source-attribute))
           ]
         )))
   (define (tok-end-pos* t)
     (let(
           [source-attribute (tok-find-attribute (attribute:source) t)]
           )
       (cond
         [(null? source-attribute) (position-null)]
         [else
           (if (length> source-attribute 1) 
             (log 
               (string-append
                 "tok-end-pos* t has more than one source attribute: "
                 (tok->string t)
                 ))
             (void))
           (source-end-position (car source-attribute))
           ]
         )))


;;--------------------------------------------------------------------------------
;; routines for matching tokens
;;  
;;   these are to be used as predicates for the '?' operator in match
;;
;;   match predicates take one operand so these routines return a function that takes one
;;   operand
;;
  (define ($tok type) (λ(t) (type-is type t)))
  (define ($tok-attribute tok-type attribute-type . attribute-val)
    (λ(t)
      (cond 
        [(null? attribute-val) (and (type-is tok-type t) (tok-find-attribute attribute-type t))]
        [else
          (and (type-is tok-type t) (tok-attribute-is* attribute-type t attribute-val))
          ]
        )))
        
   (define ($tok-attribute-test-0)
     (let*(
            [t    tok-example-2]
            [pred ($tok-attribute 'tok:punc 'attribute:lexeme ",")]
            )
       (pred t)
       ))
   (test-hook $tok-attribute-test-0)


;;--------------------------------------------------------------------------------
;; common tokens
;;
;;  See tokens.rkt for a definition of this class
;;
  ;; token children
  ;;
  (define (tok:comment) 'tok:comment) ; the lexer creates a token for comments
  (define (tok:errsyn) 'tok:errsyn) ; a place holder the parser can add if it can't make sense of the syntax
  (define (tok:lex-other) 'tok:lex-other) ; these characters did not lex to any token
  (define (tok:number) 'tok:number)
  (define (tok:punc) 'tok:punc) ; punctuation gets to be its own value
  (define (tok:string) 'tok:string)
  (define (tok:symbol) 'tok:symbol) ; symbol name is in the lexeme member

  (tok-hook
    (tok:null)
    (tok:comment)
    (tok:errsyn)
    (tok:lex-other)
    (tok:number)
    (tok:punc)
    (tok:string)
    (tok:symbol)
    )

   (define (punc-val-is t p)
     (and
       (type-is (tok:punc) t)
       (apply string=? (cons p (tok-lexeme t)))
       ))

   (define (punc-val-is-test-0)
     (punc-val-is tok-example-2 ","))
   (test-hook punc-val-is-test-0)
 
      
;;--------------------------------------------------------------------------------
;; error facilities
;;

  ;;attributes
  ;;
    (define (attribute:errsyn-mess) 'attribute:errsyn-mess) ; syntax error message
    (define (attribute:errsyn-ts) 'attribute:errsyn-ts)

    (attribute-hook
      (attribute:errsyn-mess) 
      (attribute:errsyn-ts)
      )

    (define (attribute:errsyn-mess-make mess) (attribute-make (attribute:errsyn-mess) (list mess)))
    (define (attribute:errysn-ts-make ts) (attribute-make (attribute:errsyn-ts) ts))


  ;;. in order to facilitate errsyn reporting we give rules  an 'imperative' setting
  ;;. withtout imparatives grammar rules will simply not match when there is an error
  ;;.
    (define (imperative:test) 'imperative:test) ; on nomatch, returns false there can be no errsyn
    (define (imperative:force) 'imperative:force) ; on nomatch, returns place holder with errsyn
    (define (imperative:committed) 'imperative:committed) ; on nomatch, rule decides to return #f or an errsyn

  ;;. adds an error message to a token
  ;;.
  ;;.    input: ts the token is made from, the token, the message
  ;;.    returns: the token with members added
  ;;.
  ;;.  The ts are only for annotation of the error, and are not read.
  ;;.  If you do not have the ts use a null list
  ;;.
    (define (append-errsyn ts t mess)
      (let*(
            [a0 (attribute-make (attribute:errsyn-mess) mess)]
            [t0 (tok-append-attributes t a0)]
            )
        (cond
          [(null? ts) t0]
          [else
            (let*(
                  [a1 (attribute-make (attribute:errsyn-ts) ts)]
                  [t1 (tok-append-attributes t0 a1)]
                  )
              t1
              )]
          )))

  (define (append-errint ts t mess)
    (let*(
           [umess (string-append "internal errsyn when parsing" (if (string=? mess "") "" " ") mess)]
           [err-tok (append-errsyn ts t umess)]
            )
      (log (tok->string err-tok))
      err-tok
      ))


  ;;. makes a new token of tok-type and appends errsyn attribute with message
  ;;.
  ;;.  input:  ts for the source position information,  etc.
  ;;.  output:  a new token
  ;;.
    (define (tok-make-errsyn ts tok-type generator message)
      (let (
            [newt (tok-make-parse ts tok-type generator)]
            )
        (append-errsyn ts newt message)
        )
      )

    (define (tok-make-errint ts tok-type generator mess)
      (let* (
              [umess (string-append "internal errsyn when parsing" (if (string=? mess "") "" " ") mess)]
              [err-tok (tok-make-errsyn ts tok-type generator umess)]
              )
        (log (tok->string err-tok))
        err-tok
        ))


  ;;. a grammar rule that either returns #f or a syntax error token depending on the
  ;;. value of 'imparative' 
  ;;.
    (define (rule-errsyn ts imperative tok-type generator message)
      (cond
         [(eqv? imperative (imperative:test)) #f]
         [(eqv? imperative (imperative:force))
           (tok-make-errsyn ts tok-type generator message)
           ]
         [else
           (rule-errparser ts imperative tok-type 'rule-errsyn "imperative selected else")
         ]))

  ;;. same as above but the message is set to 'expected <type> but got: '
  ;;.   input: the tokens being parsed, etc.
  ;;.   output: #f, or a new token of the given tok-type with an errsyn member
  ;;.
    (define (rule-errsyn-expected ts imperative tok-type generator)
      (rule-errsyn ts imperative tok-type generator 
        (string-append "expected " (symbol->string tok-type) " but found:")
        ))

  ;;. when the problem is our parser not the syntax of the thing we are parsing
  ;;.   oops our parser has a bug, we will log an error and say there is no match
  ;;.   if the imparative is forced, then we return an error token
  ;;.
    (define (rule-errparser ts imperative tok-type generator [mess ""])
      (let* (
            [err-tok (tok-make-errint ts tok-type generator mess)]
            )
      (cond
         [(eqv? imperative (imperative:test)) #f]
         [else err-tok]
         )
      ))

  ;; flat check for error conditions on a token or list of tokens
  ;;    see filter.rkt (filter-tok-err) for a tree search
  ;;
    (define (tok-has-err t) 
      (or
        (not (tok-well-formed? t))
        (type-is (tok:errsyn) t)
        (pair? (tok-find-attribute (attribute:errsyn-mess) t))
        ))

    (define (toks-has-err ts)
      (and
        (not (null? ts))
        (ormap tok-has-err ts)
        ))
    ;note test framed-item-sep-test-2 & 3 in "parser-framing.rkt"


;;--------------------------------------------------------------------------------
;; provides
;;
;;   typed list, attributes, tokens and supporting routines
;;
  (provide (all-defined-out))

