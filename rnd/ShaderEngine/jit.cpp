#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"

#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Transforms/Scalar.h"

#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"


#include "jit.h"

#include <iostream>

using namespace llvm;

static FunctionPassManager *TheFPM;

//
// -- API
//
ExecutionEngine *
createJITEngine(Module *M)
{

    ExistingModuleProvider* MP = new ExistingModuleProvider(M);
    ExecutionEngine* EE = ExecutionEngine::create(MP, false);

    FunctionPassManager *FPM = new FunctionPassManager(MP);

    // Add optimizer
    FPM->add(new TargetData(*EE->getTargetData()));
    FPM->add(createInstructionCombiningPass());
    FPM->add(createCFGSimplificationPass());

    TheFPM = FPM;

    return EE;
}

void *
JITCompileFunction(ExecutionEngine *EE, Function *F)
{
    void *ptr;

    // Debug
    F->dump();


    // Apply optimization
    TheFPM->run(*F);

    ptr = EE->getPointerToFunction(F);
    assert(ptr != NULL);

    

}

#if 0

int main() {
  // Create some module to put our function into it.
  Module *M = new Module("test");

  // Create the add1 function entry and insert this entry into module M.  The
  // function will have a return type of "int" and take an argument of "int".
  // The '0' terminates the list of argument types.
  Function *Add1F =
    cast<Function>(M->getOrInsertFunction("add1", Type::Int32Ty, Type::Int32Ty,
                                          (Type *)0));

  // Add a basic block to the function. As before, it automatically inserts
  // because of the last argument.
  BasicBlock *BB = BasicBlock::Create("EntryBlock", Add1F);

  // Get pointers to the constant `1'.
  Value *One = ConstantInt::get(Type::Int32Ty, 1);

  // Get pointers to the integer argument of the add1 function...
  assert(Add1F->arg_begin() != Add1F->arg_end()); // Make sure there's an arg
  Argument *ArgX = Add1F->arg_begin();  // Get the arg
  ArgX->setName("AnArg");            // Give it a nice symbolic name for fun.

  // Create the add instruction, inserting it into the end of BB.
  Instruction *Add = BinaryOperator::CreateAdd(One, ArgX, "addresult", BB);

  // Create the return instruction and add it to the basic block
  ReturnInst::Create(Add, BB);

  // Now, function add1 is ready.


  // Now we going to create function `foo', which returns an int and takes no
  // arguments.
  Function *FooF =
    cast<Function>(M->getOrInsertFunction("foo", Type::Int32Ty, (Type *)0));

  // Add a basic block to the FooF function.
  BB = BasicBlock::Create("EntryBlock", FooF);

  // Get pointers to the constant `10'.
  Value *Ten = ConstantInt::get(Type::Int32Ty, 10);

  // Pass Ten to the call call:
  CallInst *Add1CallRes = CallInst::Create(Add1F, Ten, "add1", BB);
  Add1CallRes->setTailCall(true);

  // Create the return instruction and add it to the basic block.
  ReturnInst::Create(Add1CallRes, BB);

    ExecutionEngine *EE = createJITEngine(M);

    void *p;

    p = JITCompileFunction(EE, FooF); 

    printf("p = 0x%08x\n", p);

#if 0
  // Now we create the JIT.
  ExistingModuleProvider* MP = new ExistingModuleProvider(M);
  ExecutionEngine* EE = ExecutionEngine::create(MP, false);

  std::cout << "We just constructed this LLVM module:\n\n" << *M;
  std::cout << "\n\nRunning foo: " << std::flush;

  // Call the `foo' function with no arguments:
  std::vector<GenericValue> noargs;
  GenericValue gv = EE->runFunction(FooF, noargs);

  // Import result of execution:
  std::cout << "Result: " << gv.IntVal.toStringUnsigned(10) << "\n";
#endif
  return 0;
}
#endif
