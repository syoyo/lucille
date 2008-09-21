FLCFLAGS=`fltk-config --cflags`
FLCXXFLAGS=`fltk-config --cxxflags`
FLLDFLAGS=`fltk-config --ldflags`
POSTEXEC=fltk-config --post
INSTALLDIR=/usr/local/bin
CC=gcc
CXX=g++
RM=rm -f
#OPTFLAGS=-O2 -ffast-math
OPTFLAGS=-g 
TARGET=rockenfield

EXTRA_DEF=-DENABLE_THREADING
# EXTRA_DEF=

# If you have libpng, use those settings.
DEFINES=$(EXTRA_DEF) -DWITH_PNG -I.
PNGLIBS=-L/opt/local/lib -lpng -lz
# if you haven't libpng, use those settins.
#DEFINES=-I. $(EXTRA_DEF)
#PNGLIBS=

SRCS=rockenfield.cpp ../rgbe.c
OBJS=rockenfield.o ../rgbe.o

all: $(TARGET)

$(TARGET): $(SRCS) $(OBJS)
	$(CXX) $(DEFINES) $(FLCXXFLAGS) $(CXXFLAGS) $(OPTFLAGS) -o $(TARGET) $(OBJS) $(FLLDFLAGS)	$(PNGLIBS)
	$(POSTEXEC) $(TARGET)

install:
	cp $(TARGET) $(INSTALLDIR)/
	$(POSTEXEC) $(INSTALLDIR)/$(TARGET)

clean:
	$(RM) $(OBJS)
	$(RM) $(TARGET)

.c.o:
	$(CC) $(DEFINES) $(FLCFLAGS) $(OPTFLAGS) -c -o $*.o $<

.cpp.o:
	$(CXX) $(DEFINES) $(FLCXXFLAGS) $(OPTFLAGS) -c -o $*.o $<

