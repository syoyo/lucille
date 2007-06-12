lib_LTLIBRARIES = libridisplay.la

libridisplay_la_LDFLAGS = 
libridisplay_la_SOURCES = rgbe.c \
			  rgbe.h \
			  sockdrv.c \
			  sockdrv.h \
			  hdrdrv.c \
			  hdrdrv.h

if HAVE_JPEGLIB
libridisplay_la_SOURCES += jpeg.c jpeg.h
endif

if HAVE_OPENEXRLIB
libridisplay_la_SOURCES += openexrdrv.c openexrdrv.h
endif


INCLUDES = -I../../include -I../basic

EXTRA_DIST = rgbe.txt
