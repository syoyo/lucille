Natively supported framebuffer drivers
--------------------------------------

framebufferdrv.c
~~~~~~~~~~~~~~~~

Output rendering image to framebuffer(e.g. Win32 window, X window, etc)


hdrdrv.c
~~~~~~~~

Output rendering image to file. File format is Greg Wald's RGBE format.

openexrdrv.c
~~~~~~~~~~~~

Output rendering image to file with OpenEXR image format.


sockdrv.c
~~~~~~~~~

Output rendering image to TCP/IP network. This is useful when render server locates over the network.


Optional framebuffer drivers
--------------------------------------

jpegdrv.c
~~~~~~~~~

TODO
