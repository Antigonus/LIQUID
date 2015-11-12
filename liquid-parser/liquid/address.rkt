#|




|#



#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;

  ;; by keeping a separate parcel and item address we avoid having to do a divide to recover an index
  (struct window-address-type (parcel-address offset) #:mutable #:transparent)  ; address-0 = parcel address,  address-1 offset

