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
;; global parameters
;;    
  (define current-byte-stream-block-length (make-parameter 1024)); should synch this with the port buffering
  (provide current-byte-stream-block-length)

