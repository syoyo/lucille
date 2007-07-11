lib_LIBRARIES = liblsh.a
liblsh_a_SOURCES = lsh.h \
		   lsh.c

INCLUDES = -I../basic -I../ri -I../render -I../../include

#YFLAGS = -d -v -t
#bison version 1.35
AM_YFLAGS = -d -v -t
bin_PROGRAMS = lsh 
lsh_SOURCES = main.c \
	      my_getopt.c \
	      parserib.y \
	      lexrib.l 
lsh_LDFLAGS = 
lsh_LDADD   = liblsh.a \
              ../ri/libri.a \
              ../render/librirender.a \
              ../transport/libritransport.a \
              ../display/libridisplay.a \
              ../basic/libribasic.a \
              @X_LIBS@ \
              @X11_LIB@ \
              @JPEG_LIBS@ \
              @OPENEXR_LIBS@ \
              -lm \
              @DL_LIBS@ \
              @PTHREAD_LIB@ \
              @SYSTEM_LIBS@

# hack
#if OSXCARBON
#noinst_SCRIPTS = lsh_postbuild
#
#lsh_postbuild:
#	/Developer/Tools/Rez -t APPL -o lsh carbon.r
#endif

EXTRA_DIST  = parserib.y parserib.c lexrib.c parserib.h 

