#
# -- Begin config
#
LLVM_CONFIG   = llvm-config
LLVM_INCDIR   = `$(LLVM_CONFIG) --includedir`
LLVM_CXXFLAGS = `$(LLVM_CONFIG) --cxxflags`
LLVM_LIBDIR   = `$(LLVM_CONFIG) --libdir`
LLVM_LIBS     = `$(LLVM_CONFIG) --libs`

CXX         = g++
TARGET      = jittest

#
# -- End condig
#


SRCS = jit.cpp
OBJS = ${SRCS:.cpp=.o}

MY_CXXFLAGS  = -g -I$(LLVM_INCDIR)
MY_LDFLAGS   = -L$(LLVM_LIBDIR) 
MY_LIBS      = $(LLVM_LIBS)

.PHONY: gen

all: $(TARGET)

$(TARGET): $(OBJS)
	gcc -o $(TARGET) $(OBJS) $(MY_LDFLAGS) $(LDFLAGS) $(MY_LIBS) -pthread -ldl -lstdc++

gen:
	python gen.py

clean:
	rm -rf $(TARGET) 
	rm -rf $(OBJS)
	rm -rf depend.mk

gui.cpp gui.h: gui.fl
	fluid -o .cpp -c $<

.cpp.o:
	$(CXX) -c $(MY_CXXFLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS) $<

depend:
	$(CXX) $(LLVM_CXXFLAGS) $(CXXFLAGS) $(OPT_FLAGS) -MM $(CXX_SRCS) > depend.mk

-include depend.mk

depend.mk:
	touch depend.mk
	$(MAKE) depend
