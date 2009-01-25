Fur render
==========

Goal
----

Develop a

 - Fast
 - Alias-free
 - Raytrcing-based
 
rendering system for fur(or equivarently thin-geomery).


Ideas
-----

 - Fur is represented as control points
 - Use beam tracing for accurate, alias-free rendering
 - Use specific acceleration data structure for thin geometry
   (based on OBB, ellipsoid, tube, etc)


References
----------

 - RAY TRACING FOR CURVES PRIMITIVE
   Koji Nakamaru and Yoshio Ono
   WSCG 2002.
