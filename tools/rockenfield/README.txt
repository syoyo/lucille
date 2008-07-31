 
 rockenfield

   rockenfield is a TCP/IP-based display driver for lucille.

 Note:

 rockenfield uses fltk library.

 http://www.fltk.org/
 
 fltk library must be compiled with --enable-threads configure option.

 If your fltk library doesn't compiled with --enable-threads,
 undef -DENABLE_THREADING in the Makefile. 
 (Usually this decrease the performance of rockenfield)
