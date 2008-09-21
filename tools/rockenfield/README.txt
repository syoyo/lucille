===========
rockenfield
===========

rockenfield is a TCP/IP-based display driver for lucille.

Requirements
============

rockenfield uses fltk library 1.1.9.

http://www.fltk.org/
 
fltk library must be compiled with --enable-threads configure option.

If your fltk library doesn't compiled with --enable-threads,
undef -DENABLE_THREADING in the Makefile. 
(Usually this decrease the performance of rockenfield)

Compile
=======

Just type make to build rockenfield.::

  $ make


Install
=======

Just copy rockenfield to directory where PATH is set. e.g. ::

  $ sudo cp rockenfield /usr/local/bin
