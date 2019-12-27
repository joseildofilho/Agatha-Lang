#lang brag

agatha-program : agatha-list?
agatha-list    : (agatha-exp | agatha-command)+
agatha-exp 	   : VARIABLE OP agatha-b-exp ENDDEF
agatha-b-exp   : (OP? OP? (( "(" agatha-b-exp ")" )* | VARIABLE))+
agatha-command : OP VARIABLE ENDDEF
