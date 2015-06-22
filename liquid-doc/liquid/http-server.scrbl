

#lang scribble/base

@title{http-server}

@section{Introduction}

  This is http server facilitates the browsing of application pages.  Programmers use the
  function 'page-hook' to associate link strings to programmer defined page functions.  When
  a browser visits the server and requests a given link string, the corresponding function
  is called.  The output from the page function is then sent back to the browser.

  Browsing may be local for implementing a local application, or placed on a server for remote
  browsing as for a conventional website.

  This server is implemented independent of the server provided with the standard racket
  libraries.  The thinking was that a server specific to a LIQUID application is simpler than a general
  purpose server, and that it may need to take a separate evolution path.

  This server is multithreaded and employs the LIQUID module 'realtime' for time out
  interrupts.  Currently we use single process threads so as to simplify development and
  debug. It is intended that this will change in a later version.

@section{page function}

    (define (page-hello the-tcp-context url)
      (parameterize ([current-output-port (tcp-context-out the-tcp-context)])
        (display (http-response))
        (display (html-str (html-header) "<body>hello<br></body>\r\n"))
        ))


  A page function accepts a tcp context object and a url.  

  The tcp context object is defined in the lynch-lib module, It holds the TCP input and
  output stream objects, and identifying information for both the server and client IP
  address and port.  Typically the first line of the page function will bind the tcp
  output port wth the standard output port, and then the page function will display html
  to the standard output stream.

  The url parameter is a url object as defined by the racket net/url package. This is the
  url the browser requested of the server.  Most page functions ignore this parameter.

  The contents of the page function is anything defined by the programmer.

@section{hooking the page function}

      (page-hook "/server/hello" page-hello)

  The http-server module provides the 'page-hook' function for hooking server functions.  These can
  be hooked at any time, so a given page can provide new pages for the server.  Static websites will
  hook all their pages before running the server.

@section{running the server}

  As for the http-server lib in the main racket catelog, our server returns an function for stopping the
  server.  This leads to a paradoxical startup syntax of:

     (define stop (http-server))

  To run run the sever.  And then

     (stop)

  To stop the server.

  http-server optionally accepts a parameter for a port number.  When spinning through run edit cycles
  while debugging an application it is common to find a server already using the default port, and thus
  to provide a new port number.







