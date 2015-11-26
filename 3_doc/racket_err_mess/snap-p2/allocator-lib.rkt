#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
  2014-10-01 This file is being released under the GNU Lesser General Public License. 
  Please see the file ../doc/lpgl-3.0.txt for a copy of the license.

  Handles unique number allocation on a per session basis.  A session starts when
  the program's process begins, and it ends when the program's process ends.

|#

#lang racket


;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require "test-lib.rkt")
  (require "arith-lib.rkt")


;;--------------------------------------------------------------------------------
;; session unique numbers and identifiers
;;
;;   programmers should not make identifiers of the form "unique-to-session-name-[0-9]+"
;;   one would think that wouldn't be a very strong programming constraint ...
;;
  (define unique-to-session-count 0)
  (define (unique-to-session-number)
    (begin0
      unique-to-session-count
      (set! unique-to-session-count (++ unique-to-session-count))
      ))
   (define (unique-to-session-number-dealloc n) (void))
   (define (unique-to-session-name)
     (string-append "unique-to-session-name-" (number->string (unique-to-session-number))))
   (define (unique-to-session-name-dealloc name) (void))

;;--------------------------------------------------------------------------------
;;
  (provide ;; this context continues to the bottom of the page

    ;; for creating unique to the session numbers or identifiers, might not be unique between sessions
    ;;
    unique-to-session-number ; gives out a number it never gave out before
    unique-to-session-number-dealloc
    unique-to-session-name ; gives out a name of the form idnnnn   where nnnn is a number.
    unique-to-session-name-dealloc
    )
