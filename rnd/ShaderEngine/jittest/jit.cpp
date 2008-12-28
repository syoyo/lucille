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

#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/MemoryBuffer.h"

#include "jit.h"
#include "shader_env.h"

#include <iostream>

using namespace llvm;

static FunctionPassManager *TheFPM;

ri_shader_env_t genv;

void
init_shader_env()
{
    memset(&genv, 0, sizeof(ri_shader_env_t)); 
    genv.N[0] = 0.0f;
    genv.N[1] = 1.0f;
    genv.N[2] = 2.0f;
    genv.N[3] = 3.0f;

}

void
dump_shader_env()
{
    printf("N  = %f, %f, %f\n", genv.N[0] , genv.N[1] , genv.N[2] );
    printf("Ci = %f, %f, %f\n", genv.Ci[0], genv.Ci[1], genv.Ci[2]);
}

//
// Create the JIT engine from the module `M`
// (i.e., the JIT engine is bounded with the module)
//
ExecutionEngine *
createJITEngine(Module *M)
{

    ExistingModuleProvider* MP = new ExistingModuleProvider(M);
    ExecutionEngine* EE = ExecutionEngine::create(MP, false);

    FunctionPassManager *FPM = new FunctionPassManager(MP);

    // Add some code optimizer
    FPM->add(new TargetData(*EE->getTargetData()));
    FPM->add(createInstructionCombiningPass());
    FPM->add(createCFGSimplificationPass());

    TheFPM = FPM;

    return EE;
}

//
// Returns the pointer to the function F
//
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

typedef void (*ShaderFunP)(void);
typedef void (*ShaderEnvSetFunP)(ri_shader_env_t *env);
typedef void (*ShaderEnvGetFunP)(ri_shader_env_t *env);

void
runShader(void *FunP)
{
    ShaderFunP f = (ShaderFunP)FunP;
 
    f();
}

#if 1

int
main(int argc, char **argv)
{
    const char *defShaderModuleFilename = "matte.bc";
    const char *shaderModuleFilename;

    const char *bcext = ".bc";
    char *p;
    int  len;
    char buf[1024];
    char *basename;

    std::string ErrorMessage;

    if (argc > 1) {
        shaderModuleFilename = argv[1]; 
    } else {
        shaderModuleFilename = defShaderModuleFilename; 
    }

    p = strrchr(shaderModuleFilename, '.');
    assert(p != NULL);

    len = p - shaderModuleFilename + 1;
    strncpy(buf, shaderModuleFilename, len);

    //strncpy(buf + len, bcext, strlen(bcext));
    //buf[len + strlen(bcext)] = '\0';
    buf[len-1] = '\0';
    basename = buf;

        
    // Load the input module...
    std::auto_ptr<Module> M;
    if (MemoryBuffer *Buffer = MemoryBuffer::getFileOrSTDIN(shaderModuleFilename, &ErrorMessage)) {
        M.reset(ParseBitcodeFile(Buffer, &ErrorMessage));
        delete Buffer;
    }

    if (M.get() == 0) {
        cerr << argv[0] << ": ";
        if (ErrorMessage.size())
            cerr << ErrorMessage << "\n";
        else
            cerr << "bitcode didn't read correctly.\n";
        return 1;
    }

    cout << "bitcode read OK.\n";

    ExecutionEngine *EE = createJITEngine(M.get());

    Function *F;
    void *FunP;

    F = (M.get())->getFunction(basename);
    if (F == NULL) {
        cerr << "can't find the function [ " << basename << " ] from the module\n";
        return 1;
    }

    {
        Function *ShaderEnvSetF;
        const char *shader_env_init_func_name = "set_shader_env";
        void *ptr;

        ShaderEnvSetF = (M.get())->getFunction(shader_env_init_func_name);
        if (ShaderEnvSetF == NULL) {
            cerr << "can't find the function [ " << shader_env_init_func_name << " ] from the module\n";
            return 1;
        }

        ptr = EE->getPointerToFunction(ShaderEnvSetF);
        assert(ptr != NULL);

        init_shader_env();
        
        ((ShaderEnvSetFunP)ptr)(&genv);

    }

    //FunP = JITCompileFunction(EE, F); 
    //printf("FunP = 0x%08x\n", FunP);
    //runShader(FunP);

    std::vector<GenericValue> noargs;
    GenericValue gv = EE->runFunction(F, noargs);

    {
        Function *ShaderEnvGetF;
        const char *shader_env_init_func_name = "get_shader_env";
        void *ptr;

        ShaderEnvGetF = (M.get())->getFunction(shader_env_init_func_name);
        if (ShaderEnvGetF == NULL) {
            cerr << "can't find the function [ " << shader_env_init_func_name << " ] from the module\n";
            return 1;
        }

        ptr = EE->getPointerToFunction(ShaderEnvGetF);
        assert(ptr != NULL);

        ((ShaderEnvGetFunP)ptr)(&genv);

    }

    dump_shader_env();

#if 0
  // Call the `foo' function with no arguments:
  std::vector<GenericValue> noargs;
  GenericValue gv = EE->runFunction(FooF, noargs);

  // Import result of execution:
  std::cout << "Result: " << gv.IntVal.toStringUnsigned(10) << "\n";
#endif
  return 0;
}
#endif
