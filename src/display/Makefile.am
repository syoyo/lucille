lib_LIBRARIES = libridisplay.a

libridisplay_a_SOURCES =  rgbe.c \
			  rgbe.h \
			  sockdrv.c \
			  sockdrv.h \
			  hdrdrv.c \
			  hdrdrv.h

if HAVE_JPEGLIB
libridisplay_a_SOURCES += jpeg.c jpeg.h
endif

if HAVE_OPENEXRLIB
libridisplay_a_SOURCES += openexrdrv.c openexrdrv.h
endif


INCLUDES = -I../../include -I../basic

EXTRA_DIST = rgbe.txt
