(match 
    (list "<apple>" "apple-stuff" "</apple>" 
          "<orange>" "orange-stuff" "</orange>" 
          "<apple>" "more-apple-stuff" "</apple>" 
    )
  [(list  "<apple>" a-stuff ... "</apple>" etc ...)
   a-stuff
  ]
)



racket@query-parser.rkt> (match 
    (list "<apple>" "apple-stuff" "</apple>" 
          "<orange>" "orange-stuff" "</orange>" 
          "<apple>" "more-apple-stuff" "</apple>" 
    )
  [(list  "<apple>" a-stuff ... "</apple>" etc ...)
   a-stuff
  ]
)

'("apple-stuff"
  "</apple>"
  "<orange>"
  "orange-stuff"
  "</orange>"
  "<apple>"
  "more-apple-stuff")


; expected just "apple-stuff"