#include <stdio.h>
#include <math.h>
#include <SDL.h>
#include <GL/gl.h>

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
#include "timer.h"

#define WINDOW_WIDTH  512
#define WINDOW_HEIGHT 512
SDL_Surface     *surface;
unsigned char   *img;

using namespace llvm;

static FunctionPassManager *TheFPM;

ri_shader_env_t genv;

#ifdef __cplusplus
extern "C" {
#endif

extern void bora();

#ifdef __cplusplus
}
#endif

typedef void (*ShaderFunP)(void);
typedef void (*ShaderEnvSetFunP)(ri_shader_env_t *env);
typedef void (*ShaderEnvGetFunP)(ri_shader_env_t *env);

typedef struct _ri_shader_jit_t
{

    ShaderEnvSetFunP shader_env_set;
    ShaderEnvGetFunP shader_env_get;
    ShaderFunP       shader_fun;

} ri_shader_jit_t;

ri_shader_jit_t g_shader_jit;

static unsigned char
clamp(float f)
{
    static int count = 0;

    float gamma = 2.2f;

    int i = powf(f, 1.0/gamma) * 255.5f;   // gamma correction

    if (i < 0) i = 0;
    if (i > 255) i = 255;

    return (unsigned char)i;

}

void
bora()
{
    printf("bora called\n");
}

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
// Custom symbol resolver
//
void *customSymbolResolver(const std::string &name)
{
    cout << "Resolving " << name << "\n";
    if (name == "bora") return (void *)bora;    // ptr to the function

    return NULL;    // fail
}

void
dummy_render(int width, int height)
{
    int         x, y;
    mytimer_t   start_time, end_time;
    double      elap;

    ri_shader_env_t ret_env;    // FIXME: check align.

    get_time(&start_time);

    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {

            genv.Cs[0] = (x / (float)width);
            genv.Cs[1] = (y / (float)height);

            g_shader_jit.shader_env_set(&genv);
            //g_shader_jit.shader_fun();
            g_shader_jit.shader_env_get(&genv);

            //img[3 * (y * width + x) + 0] = clamp(genv.Ci[0]);
            //img[3 * (y * width + x) + 1] = clamp(genv.Ci[1]);
            //img[3 * (y * width + x) + 2] = 255;

        }
    }

    get_time(&end_time);

    elap = elapsed_time(&start_time, &end_time); 

    printf("\r[Exec] elapsed = %f (sec)", elap);
    fflush(stdout);
    
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
    cout << "========= Before optimization  ===================\n";
    F->dump();

    // Apply optimization
    TheFPM->run(*F);

    // Debug
    cout << "========= After optimization  ===================\n";
    F->dump();

    ptr = EE->getPointerToFunction(F);
    assert(ptr != NULL);

    return ptr;
}

void
runShader(void *FunP)
{
    ShaderFunP f = (ShaderFunP)FunP;
 
    f();
}

#if 1

static void
display()
{
    dummy_render(WINDOW_WIDTH, WINDOW_HEIGHT);

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glDrawPixels(WINDOW_WIDTH, WINDOW_HEIGHT, GL_RGB, GL_UNSIGNED_BYTE, img);

    SDL_GL_SwapBuffers();

}

static void
gui_main()
{
    SDL_Event event;
    SDL_Init(SDL_INIT_VIDEO);

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    surface = SDL_SetVideoMode(WINDOW_WIDTH, WINDOW_HEIGHT, 32, SDL_OPENGL);
    
    img = (unsigned char *)malloc(WINDOW_WIDTH * WINDOW_HEIGHT * 3);

    while (1) {
        while (SDL_PollEvent(&event)) {
        
            if ((event.type == SDL_QUIT) ||
                 event.type == SDL_KEYUP &&
                 event.key.keysym.sym == SDLK_ESCAPE) {
                goto quit_app;
            }
        }

        display();
        SDL_Delay(0);
    }

quit_app:

    SDL_Quit();

}

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
    EE->InstallLazyFunctionCreator(customSymbolResolver);

    Function *F;
    void *FunP;

    F = (M.get())->getFunction(basename);
    if (F == NULL) {
        cerr << "can't find the function [ " << basename << " ] from the module\n";
        return 1;
    }

    {
        Function *GF;
        GlobalVariable *GV;

        const char *bora_name = "bora";

        GF = (M.get())->getFunction("bora");
        if (GV == NULL) {
            cerr << "can't find bora()\n";
            return 1;
        }
        cout << "bora OK\n";
        
    }

    init_shader_env();

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

        
        g_shader_jit.shader_env_set = (ShaderEnvSetFunP)ptr;

    }

    FunP = JITCompileFunction(EE, F); 
    printf("FunP = 0x%08x\n", FunP);
    //runShader(FunP);
    g_shader_jit.shader_fun = (ShaderFunP)FunP;

    //std::vector<GenericValue> noargs;
    //GenericValue gv = EE->runFunction(F, noargs);

    {
        Function *ShaderEnvGetF;
        const char *shader_env_get_func_name = "get_shader_env";
        void *ptr;

        ShaderEnvGetF = (M.get())->getFunction(shader_env_get_func_name);
        if (ShaderEnvGetF == NULL) {
            cerr << "can't find the function [ " << shader_env_get_func_name << " ] from the module\n";
            return 1;
        }

        ptr = EE->getPointerToFunction(ShaderEnvGetF);
        assert(ptr != NULL);

        g_shader_jit.shader_env_get = (ShaderEnvGetFunP)ptr;

    }

    dump_shader_env();

    gui_main();

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
