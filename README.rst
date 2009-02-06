====================================
lucille global illumination renderer
====================================


Introduction
============

http://lucille.sourceforge.net/img/toprendered.jpg

lucille is a fast, next-gen, cutting-edge global illumination renderer with programmable shading engine.

lucille supports RenderMan(R) interface so you can use lucille as a replacement
of exising RenderMan(R) compiliant renderer.

lucille's ultimate goal is to develop next-gen de-facto standard global
illumination render, not to develop RenderMan(R) compliant renderer though.


Features
========

- Efficient ray tracing and global illumination computation.
- Fully analytic rendering(no noise, experimental feature).
- Support for RIB( The RenderMan(R) Interface Bytestream ) file format.
- Support for RenderMan Shading Language with JIT execution.
- Multicore ready, SIMD optimized. Designed for fully unleash the processor power.
- Highly portable architecture.


Supported platforms
===================

lucille is written in very portable C, thus lucille will run various platform and processors.
At this time, fully tested platforms are,

- Mac OS X ( x86, 32bit and 64bit )
- Linux ( x86, 32bit and 64bit )
- Windows ( MinGW or CYGWIN )


Installation
============

Edit ''custom.py'' if required, then type ::

  $ make

to build the lucille.

See ''INSTALL'' for more help.


Documentation
=============

See ''doc'' directory.


Project information
===================

- home page
  http://lucille.sourceforge.net/

- project management site
  http://redmine.s21g.com/projects/show/lucille


License
=======

lucille is distributed under BSD3 license. See ''LICENSE'' for more information on licensing.


Copyright
=========

2003-2203 Syoyo FUJITA( syoyo@lucillerender.org )

The RenderMan(R) Interface Procedures and Protocol are: Copyright 1988, 1989, 2000 Pixar All Rights Reserved
