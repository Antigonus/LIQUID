#|
   code snippets trying out rackets db library

    2015-01-01T07:43:11Z created  twl

|#
#lang racket

(require db)

(define pgc (postgresql-connect #:database "mordecai" #:user "mordecai" #:socket 'guess))


;; with query-exec return values are ignored
;;
(query-exec pgc  "create temporary table some_named_numbers (n integer, d varchar(20))")

(query-exec pgc  "insert into some_named_numbers values (0, 'zero')")
(query-exec pgc  "insert into some_named_numbers values (2, 'two')")
(query-exec pgc  "insert into some_named_numbers values (1, 'one')")

;; using parameters in the SQL string
;; "Every standard query function accepts query parameters."
(query-exec pgc
    "insert into some_named_numbers values ($1, $2)"
    (+ 1 2)
    "three")

;; result is zero or more rows
(equal?
  (query-rows pgc "select n, d from some_named_numbers where n % 2 = 0")
  '(#(0 "zero") #(2 "two"))
)

;; result is exactly one row, or there is an exception
(equal?
 (query-row pgc "select * from some_named_numbers where n = 0")
  '#(0 "zero")
)

;; zero or more rows where the result has exactly one column
(equal?
  (query-list pgc "select d from some_named_numbers order by n")
  '("zero" "one" "two")
)

;; result is exactly a single value
(equal?
  (query-value pgc "select count(*) from some_named_numbers")
  3
)

;; result is exactly a single value, or nothing is found, else exception
(eqv?
  (query-maybe-value pgc "select d from some_named_numbers where n = 5")
  #f
)

(query-exec pgc "drop table some_named_numbers")
