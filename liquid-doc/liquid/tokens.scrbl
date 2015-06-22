

#lang scribble/base

@title{tokens} 

@section{Introduction}

  This module defines typed list, attribute, and token.

  The car element of a typed list is a type name.

  Attributes are typed lists where the type is found in a list of legal attributes. The 
  remaining format for an attribute depends on the attribute, often they are duck typed.
  Attributes are typically used to annotate tokens with information such as error 
  messages. They are not intended to be structural.

  A token has attributes and children tokens.  The children token list facilitates
  building trees. 

@section{typed-list}

  


  
   

  
         

