====================================
lucille global illumination renderer
====================================

Introduction
============

lucille is a fast, next-gen, cutting-edge global illumination renderer with programmable shading engine.

lucille supports RenderMan(R) interface so you can use lucille as an replacement
of exising RenderMan(R) compiliant renderer.
But lucille's ultimate goal is to develop next-gen de-facto standard global
illumination render, not to develop RenderMan(R) compliant renderer.


Features
========

 - Efficient ray tracing.
 - Support for RIB( The RenderMan(R) Interface Bytestream ) file format.
 - Support for RenderMan Shading Language.
 - Multicore ready, SIMD optimized.


Supported platforms
===================

lucille is written in very portable C, thus lucille will run various platforms.
At this time, fully tested platforms are,

 - Mac OS X ( x86, 32bit and 64bit )
 - Linux ( x86, 32bit and 64bit )
 - Windows ( MinGW or CYGWIN )


Installation
============

Edit 'custom.py' if required, then type ::

  $ make

to build the lucille.

See INSTALL for more help.


Documentation
=============

See 'doc' directory.


Project information
===================

 - home page
   http://lucille.sourceforge.net/

 - project management site
   http://redmine.s21g.com/projects/show/lucille


License
=======

lucille is distributed under BSD3 license. See 'LICENSE' for more licensing information.


Copyright
=========

2003-2203 Syoyo FUJITA( syoyo@lucillerender.org )

The RenderMan(R) Interface Procedures and Protocol are: Copyright 1988, 1989, 2000 Pixar All Rights Reserved
