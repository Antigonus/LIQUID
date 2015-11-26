#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  A central place to put parameter definitions.
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

|#

#lang racket

;;--------------------------------------------------------------------------------
;; parser parameters
;;    
  (define current-file-name (make-parameter "placeholder-or-test-session")); filename for test parsing of data
  (provide current-file-name) ; the source file we are reading from for the query parser

  ;; this is set low so that we will see a lot of block boundaries for debug
  (define current-block-length (make-parameter 16)); 
  (provide current-block-length)

