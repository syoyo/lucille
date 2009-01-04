#ifndef LUCILLE_JIT_H
#define LUCILLE_JIT_H

#include "llvm/Module.h"
#include "llvm/ExecutionEngine/JIT.h"

// Create JIT engine. JIT engine requires the module.
llvm::ExecutionEngine *createJITEngine(llvm::Module *M);

// JIT compile LLVM bitcode then return pointer to the function.
void *JITCompileFunction(llvm::ExecutionEngine *EE, llvm::Function *F);


#endif  // LUCILLE_JIT_H
