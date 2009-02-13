#
# -- Begin config
#
LLVM_CONFIG   = llvm-config
LLVM_INCDIR   = `$(LLVM_CONFIG) --includedir`
LLVM_CXXFLAGS = `$(LLVM_CONFIG) --cxxflags`
LLVM_LIBDIR   = `$(LLVM_CONFIG) --libdir`
LLVM_LIBS     = `$(LLVM_CONFIG) --libs core bitreader engine jit interpreter`

FLTK_CONFIG   = fltk-config
FLTK_CXXFLAGS = `$(FLTK_CONFIG) --use-gl --cflags`
FLTK_LDFLAGS  = `$(FLTK_CONFIG) --use-gl --ldflags`

CXX         = g++
TARGET      = slengine

#
# -- End condig
#


SRCS = jit.cpp gui.cpp trackball.cpp callbacks.cpp MyGLWindow.cpp main.cpp render.cpp timer.cpp cachelib.cpp hbuffer.cpp occlusion.cpp texture.cpp
OBJS = ${SRCS:.cpp=.o}

MY_CXXFLAGS  = -g -ffast-math -I$(LLVM_INCDIR)
MY_LDFLAGS   = -L$(LLVM_LIBDIR) $(FLTK_LDFLAGS)
MY_LIBS      = $(LLVM_LIBS) -lm

.PHONY: gen

all: $(TARGET)

$(TARGET): $(OBJS)
	gcc -o $(TARGET) $(OBJS) $(MY_LDFLAGS) $(LDFLAGS) $(MY_LIBS) -pthread -ldl -lstdc++

gen:
	python gen.py
	clang -emit-llvm-bc shaderlib.c noise.c

clean:
	rm -rf $(TARGET) 
	rm -rf $(OBJS)
	rm -rf depend.mk

gui.cpp gui.h: gui.fl
	fluid -o .cpp -c $<

.cpp.o:
	$(CXX) -c $(MY_CXXFLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS) $<

depend:
	$(CXX) $(FLTK_CXXFLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS) $(OPT_FLAGS) -MM $(SRCS) > depend.mk

-include depend.mk

depend.mk:
	touch depend.mk
	$(MAKE) depend
