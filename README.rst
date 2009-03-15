====================================
lucille global illumination renderer
====================================


Introduction
============

.. image:: http://lucille.sourceforge.net/img/toprendered.jpg

**lucille** is a fast, next-gen, cutting-edge global illumination renderer with programmable shading engine.

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

**lucille** is written in very portable C, thus lucille will run various platform and processors.
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


Quick tutorial
==============

''bin/lsh'', lucille renderer command, will be build after compilation finishes successfully.

Type following to render example scene.

::

  $ cd bin
  $ ./lsh ../examples/ambient_occlusion/ambient_occlusion.rib

You'll see ''ambient_occlusion.hdr'' in the current directory. The file is a HDR file. You can see this image with HDR ready image viewer.
If you build lucille with ''with_x11'' option, you'll see the rendering image during rendering is running. 

**lucille** has its own HDR image viewer called **Rockenfield**, which is located at ``$(lucille)/tools/rockenfield``. To use Rockenfield, you need manually build it by, ::

  $ cd tools/rockenfield
  $ make

Note that FLTK 1.1.9 is required to build Rockenfield.

To see HDR image with Rockenfield, type, ::

  $ rockenfield <image.hdr>


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

2003-2203 Syoyo FUJITA( syoyofujita@gmail.com )

The RenderMan(R) Interface Procedures and Protocol are: Copyright 1988, 1989, 2000 Pixar All Rights Reserved
