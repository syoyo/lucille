FLTK_CONFIG   = fltk-config
FLTK_CXXFLAGS = `$(FLTK_CONFIG) --use-gl --cflags`
FLTK_LDFLAGS  = `$(FLTK_CONFIG) --use-gl --ldflags`

CXX           = g++
CC            = gcc
CXXFLAGS      = -g
CFLAGS        = -g

SRCS = main.cpp mie.c
OBJS = main.o mie.o

TARGET = mie

all: $(TARGET)


$(TARGET) : $(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(FLTK_LDFLAGS)

.cpp.o:
	$(CXX) -c $(FLTK_CXXFLAGS) $(CXXFLAGS) $<

.c.o:
	$(CC) -c $(CFLAGS) $<

clean:
	rm -rf $(OBJS) $(TARGET) depend.mk

depend:
	$(CXX) $(FLTK_CXXFLAGS) $(CXXFLAGS) -MM $(SRCS) > depend.mk

-include depend.mk

depend.mk:
	touch depend.mk
	$(MAKE) depend

