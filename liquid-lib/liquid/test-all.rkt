#|

racket test-all runs all of the tests


|#

#lang racket

(require "dataplex-lib.rkt")
(require "dbi-dblp.rkt")
(require "db-lib.rkt")
(require "filter.rkt")
(require "filter-tests.rkt")
(require "htmle-lib.rkt")
(require "http-server-pages.rkt")
(require "http-server.rkt")
(require "http-session.rkt")
(require "misc-lib.rkt")
(require "parser-framing.rkt")
(require "parser-lex.rkt")
(require "parser.rkt")
(require "parser-nodes.rkt")
(require "parser-well-formed.rkt")
(require "realtime.rkt")
(require "realtime-test-data.rkt")
(require "ssl-server.rkt")
(require "node.rkt")
(require "try-postgres.rkt")
(require "webi-lib.rkt")

(test-all)
