#ifndef JIT_H
#define JIT_H

#include "llvm/Module.h"
// #include "llvm/Constants.h"
// #include "llvm/DerivedTypes.h"
// #include "llvm/Instructions.h"
// #include "llvm/ModuleProvider.h"
#include "llvm/ExecutionEngine/JIT.h"
// #include "llvm/ExecutionEngine/Interpreter.h"
// #include "llvm/ExecutionEngine/GenericValue.h"

// Create JIT engine. JIT engine requires the module.
llvm::ExecutionEngine *createJITEngine(llvm::Module *M);

// JIT compile LLVM bitcode then return pointer to the function.
void *JITCompileFunction(llvm::ExecutionEngine *EE, llvm::Function *F);


#endif  // JIT_H
