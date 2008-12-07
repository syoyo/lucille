======================================
htol, Houdini to lucille(RIB) exporter
======================================

.. Author: Syoyo Fujita
.. Mail: syoyo@lucillerender.org

What is htol?
=============

htol is a Python script for houdini. It converts houdini scene into RIB structure.
At this time, generated RIB file is specific to lucille so you can't render RIB file with other RenderMan compiliant renderer.

Usage
=====

Put htol.py into $HOME/houdini9.5/scripts/python.
(If the directory is not exist on your system, mkdir it)

In houdini, open Python Shell, enter ::
  
  >>> execfile("$HOME/houdini9.5/scripts/python/htol.py")

$HOME should be expanded to your home directoly, for example::

  >>> execfile("/home/syoyo/houdini9.5/scripts/python/htol.py")
  
RIB file will be generated on your $HOME directry with name ``untitled.rib``
   

Limitation
==========

 * Only polygon geometry are exported.
 * Camera's fov is fiexed.
 * Image resolution is fixed.
 * No materials
 * No shaders
 * No dynamics
 * No particles
  

Question from the author
========================

 * When running python script from shelf, python runtime says

   Error: global name 'hou' is not defined

   But the script imports hou module in advance, why?


Contribution and Bug report
===========================

If you found bugs, or encounter problems, want to add function to the exporter,
let me know at,

https://redmine.s21g.com/projects/show/lucille

 * Bug & Request & Contribution -> Go [New issue] or [Wiki]
 * Discuss                      -> Go [Forum]

