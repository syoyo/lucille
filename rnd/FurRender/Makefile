FLTK_CONFIG   = fltk-config
FLTK_CXXFLAGS = `$(FLTK_CONFIG) --cxxflags --use-gl`
FLTK_LIBS     = `$(FLTK_CONFIG) --ldflags --use-gl`

all:
	gcc -g $(FLTK_CXXFLAGS) curve.c main.cpp $(FLTK_LIBS)
