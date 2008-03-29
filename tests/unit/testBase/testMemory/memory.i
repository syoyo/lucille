%module base_memory
%{
#include "memory.h"
%}

%include "../../../../src/base/memory.h"

// private functions
void *aligned_malloc(size_t size, unsigned int align);
int   aligned_free(void *addr);
