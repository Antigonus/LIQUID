

#lang scribble/base

@title{Real-Time Support} 

@section{fun-timed}

  (fun-timed timeout fun)

  Accepts a timeout value and a one operand function.  Returns a two element list.  The first element
  is a flag indicting if function returned naturally (was not interrupted), while the second is the function result.

  The one operand given the function is a bass by value 'extend' flag.  If this flag is set to true when the
  timeout occurs, the function is given another timeout amount of time to complete.  This features is used, for example,
  from within an http server session to indicate that indeed the client did respond, so we haven't timed out yet.

   

  
         

