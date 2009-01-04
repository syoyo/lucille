#
# -- Begin config
#
LLVM_CONFIG   	= llvm-config
LLVM_INCDIR   	= `$(LLVM_CONFIG) --includedir`
LLVM_CXXFLAGS 	= `$(LLVM_CONFIG) --cxxflags`
LLVM_LIBDIR   	= `$(LLVM_CONFIG) --libdir`
LLVM_LIBS     	= `$(LLVM_CONFIG) --libs bitreader engine jit interpreter`

SDL_CONFIG    	= sdl-config
SDL_CXXFLAGS  	= `$(SDL_CONFIG) --cflags`
SDL_LIBS      	= `$(SDL_CONFIG) --libs`

CXX         	= g++
TARGET      	= jittest

ARCH            = $(shell uname -s)
#
# -- End condig
#

GL_LIBS     = -lGL
ifeq ($(ARCH), Darwin)
    GL_LIBS     = -framework OpenGL
endif

SRCS = jit.cpp timer.cpp texture.cpp render.cpp
OBJS = ${SRCS:.cpp=.o}

MY_CXXFLAGS  = -g -O2 -I$(LLVM_INCDIR)
MY_LDFLAGS   = -L$(LLVM_LIBDIR) 
MY_LIBS      = $(LLVM_LIBS) $(SDL_LIBS) -pthread -ldl -lm $(GL_LIBS) -lstdc++

.PHONY: gen

all: $(TARGET)

$(TARGET): $(OBJS)
	gcc -o $(TARGET) $(OBJS) $(MY_LDFLAGS) $(LDFLAGS) $(MY_LIBS)

gen:
	clang -emit-llvm-bc shaderlib.c noise.c
	cp ../shader_env.ll .
	cp ../shader_env.h .

clean:
	rm -rf $(TARGET) 
	rm -rf $(OBJS)
	rm -rf depend.mk

gui.cpp gui.h: gui.fl
	fluid -o .cpp -c $<

.cpp.o:
	$(CXX) -c $(MY_CXXFLAGS) $(SDL_CXXFLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS) $<

depend:
	$(CXX) $(SDL_CXXFLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS) $(OPT_FLAGS) -MM $(CXX_SRCS) > depend.mk

-include depend.mk

depend.mk:
	touch depend.mk
	$(MAKE) depend
