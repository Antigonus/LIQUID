#|
 htmle-lib
 
   useful pieces of javascript and htmle
   created: 2014-12-23T04:13:10Z twl

   htmle is html that has been parsed into xexp form.  Note Planet Neil's html
   parser.  Thanks to Neil htmle can be written out as html, so this form is
   also useful for assembling web pages.

|#
#lang racket

;;--------------------------------------------------------------------------------
;; uses these libraries
;;
  (require racket/string)

  (require (planet neil/html-parsing:2:0))
  (require (planet neil/html-writing:2:0))

  (require "lynch-lib.rkt")
  (require "http-session.rkt") ; test routines use (page-hook)
  (require "http-server.rkt") ; test routines run a web server 
  (require "http-server-pages.rkt") ; test routines grap header out of here


;;--------------------------------------------------------------------------------
;; javascript for a collapse box
;;
   (define (collapse-box-script-str)
#<<HSTRING0
  <script type="text/javascript">
    function collapseBox(a){
      var e=document.getElementById(a);
      if(!e)return true;
      if(e.style.display=="none"){
        e.style.display="block"
      }else{
        e.style.display="none"
      }
      return true;
    }
  </script>
HSTRING0
     )

;;--------------------------------------------------------------------------------
;; htmle for a collapse box
;;  input: a button label and a body that is to be expanded/collapsed
;;  output: an xexp for the collapse box
;;
;; default is for text to initially not show so that there can be many collapse
;; boxes and the user can view contents one at a time
;;
  (define (collapse-box-htmle given-label body)
    (let*(
           [an-id       (unique-to-session-name)]
           [the-label   (string-append "+ " given-label)]
           [script-call (string-append "return collapseBox(" (with-quotes an-id) ")")]
           )
      (list
        `(
           (input (@ (type "button") (onclick ,script-call) (value ,the-label)))
           (div (@  (style "margin-left:20px; display:none") (id ,an-id))
             ,body
             ))
        (list an-id)
        )
      ))
   
      ;;helper for collapse-box-test-0
      (define (collapse-box-test-0-1  the-tcp-context url) 
      ;;(define (collapse-box-test-0-1) ; for testing
          (let*(
                 [response-str (http-response)]
                 [header-str   (html-header collapse-box-script-str)]
                 [cbox-xexp    (car (collapse-box-htmle "hello" '(p "hello Shilu")))]
                 [cbox-str     (xexp->html cbox-xexp)]
                 [document-str (html-str header-str cbox-str)]
                 )

            #| ; useful for debug:
              (newline)
              (displayln "r---")
                          (displayln response-str)
              (newline)
              (displayln "h---")
                          (displayln header-str)
              (newline)
              (displayln "cbox---")
                          (displayln cbox-str)
              (newline)
              (displayln "doc---")
                          (displayln document-str)
              (newline)
              (displayln "---")
            |#

            (parameterize [(current-output-port (tcp-context-out the-tcp-context))]
              (display response-str)
              (display document-str)
              )))
       (page-hook "/test/collapse-box-0" collapse-box-test-0-1)

      ;; run test like this:
      ;;  (define stop (collapse-box-test-0))
      ;;  (define stop (collapse-box-test-0 8081))
      (define (collapse-box-test-0 [portno 8080])
        (display 
#<<HSTRING1
   1. open browser to 127.0.0.1:8080 (or whatever the correct port is)
   2. navigate to /test/collapse-box-0 and see display of a button labeled "+ hello" 
   3. Click the button and the text "hello!" will appear
   2. Click a cadr time and the text disappears
   3. click four more times see the above effect repeat.

HSTRING1
        )
        (http-server portno) ; returns stop program
        )


;;--------------------------------------------------------------------------------
;; provides
;;
;;
  ;; for debug
  (provide (all-defined-out))

#|
  (provide
    collapse-box-script-str
    )

  (provide-with-trace "htmle-lib"
    collapse-box-htmle
  )
|#
