#include <stdio.h>
#include <math.h>
#include <SDL.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

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

// #include "llvm/System/Disassembler.h"

#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/MemoryBuffer.h"

#include "jit.h"
#include "shader_env.h"
#include "texture.h"
#include "render.h"
#include "cachelib.h"

#include <iostream>
#include "timer.h"

#define WINDOW_WIDTH  256
#define WINDOW_HEIGHT 256
SDL_Surface     *surface;
unsigned char   *img;
int              g_mouse_button;
int              g_mouse_pressed = 0;
int              g_mouse_x;
int              g_mouse_y;
float            g_offt_x;
float            g_offt_y;

using namespace llvm;

static FunctionPassManager *TheFPM;

#ifdef __cplusplus
extern "C" {
#endif

ri_shader_env_t genv;

texture_t *gtex;

texture_t *
get_texture()
{
    return gtex;
}

extern void bora();

#ifdef __cplusplus
}
#endif

typedef void (*ShaderFunP)(void);
typedef void (*ShaderCacheGenFunP)(void);
typedef void (*ShaderEnvSetFunP)(ri_shader_env_t *env);
typedef void (*ShaderEnvGetFunP)(ri_shader_env_t *env);

typedef struct _ri_shader_jit_t
{

    ShaderEnvSetFunP    shader_env_set;
    ShaderEnvGetFunP    shader_env_get;
    ShaderFunP          shader_fun;
    ShaderCacheGenFunP  shader_cache_gen_fun;

} ri_shader_jit_t;

ri_shader_jit_t g_shader_jit;


static unsigned char
clamp(float f)
{
    static int count = 0;

    float gamma = 2.2f;

    int i = powf(f, 1.0f/gamma) * 255.0f;   // gamma correction

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

}

void
dump_shader_env()
{
    printf("Ci = %f, %f, %f\n", genv.Ci[0], genv.Ci[1], genv.Ci[2]);
}


//
// Custom symbol resolver
//
void *customSymbolResolver(const std::string &name)
{
    cout << "Resolving " << name << "\n";
    if (name == "bora") return (void *)bora;
    if (name == "get_texture") return (void *)get_texture;
    if (name == "texture_map") return (void *)texture_map;
    if (name == "lse_save_cache_iiic") return (void *)lse_save_cache_iiic;

    return NULL;    // fail
}

void *malloc32(size_t sz)
{
    void *p;
    void *p32;
    
    size_t sz32 = (sz + 31) & (~31);
    if (sz32 == 0) sz32 = 32;
    
    p = malloc(sz32 + 32);

    p32 = (void *)(((uintptr_t)p + 31U) & (~31U));

    return p32;
}

void
dummy_render(int width, int height)
{
    int         x, y;
    mytimer_t   start_time, end_time;
    double      elap;

    float       rayorg[3];
    float       raydir[3];
    float       texcol[4];
    sphere_t    sphere;
    isect_t     isect;

    ri_shader_env_t *ret_env;    // FIXME: check align.

    ret_env = (ri_shader_env_t *)malloc32(sizeof(ri_shader_env_t));

    sphere.center[0] = 0.0;
    sphere.center[1] = 0.0;
    sphere.center[2] = -2.0;

    sphere.radius    = 0.75;

    get_time(&start_time);

    raydir[0] = 0.0;
    raydir[1] = 0.0;
    raydir[2] = -1.0;

    float theta, phi;
    float tu, tv;

    float u, v;
    int   hit;

    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {

            isect.t = 1.0e+30f;
            
            u = (x - 0.5f * width) / (float)(0.5f * width);
            v = (y - 0.5f * height) / (float)(0.5f * height);

            rayorg[0] = u;
            rayorg[1] = v;
            rayorg[2] = 0.0;

            hit = sphere_isect(&isect, &sphere, rayorg, raydir);

            if (hit) {

                theta = acosf(isect.n[1]);
                tv = theta / M_PI;

                phi = atan2f(-isect.n[2], isect.n[0]);
                tu = (phi + M_PI) / (2.0 * M_PI);

                //texture_map(texcol, gtex, tu, tv);

                genv.Cs[0] = (x / (float)width);
                genv.Cs[1] = (y / (float)height);
                genv.Cl[0] = 0.8f;
                genv.Cl[1] = 1.0f;
                genv.Cl[2] = 0.5f;
                genv.N[0] = isect.n[0];
                genv.N[1] = isect.n[1];
                genv.N[2] = isect.n[2];
                genv.L[0] = 1.0f + g_offt_x;
                genv.L[1] = 1.0f + g_offt_y;
                genv.L[2] = 1.0f;
                genv.P[0] = isect.p[0];
                genv.P[1] = isect.p[1];
                genv.P[2] = isect.p[2];
                genv.P[3] = 1.0f;
                genv.I[0] = isect.p[0] - rayorg[0];
                genv.I[1] = isect.p[1] - rayorg[1];
                genv.I[2] = isect.p[2] - rayorg[2];
                genv.I[3] = 1.0f;
                genv.s = tu;
                genv.t = tv;

                g_shader_jit.shader_env_set(&genv);
                g_shader_jit.shader_fun();
                g_shader_jit.shader_env_get(&genv);

                img[3 * (y * width + x) + 0] = clamp(genv.Ci[0]);
                img[3 * (y * width + x) + 1] = clamp(genv.Ci[1]);
                img[3 * (y * width + x) + 2] = clamp(genv.Ci[2]);
                //img[3 * (y * width + x) + 0] = clamp(texcol[0]);
                //img[3 * (y * width + x) + 1] = clamp(texcol[1]);
                //img[3 * (y * width + x) + 2] = clamp(texcol[2]);

            } else {

                img[3 * (y * width + x) + 0] = 0;
                img[3 * (y * width + x) + 1] = 0;
                img[3 * (y * width + x) + 2] = 0; 

            }

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

    // Debug
    //std::string s = sys::disassembleBuffer((uint8_t *)ptr, 128);
    //cout << "========= Disassemble  ===================\n";
    //cout << s << "\n";

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
mouse_down(SDL_Event event)
{
    if (event.button.button == SDL_BUTTON_LEFT) {
        g_mouse_button  = SDL_BUTTON_LEFT;
        g_mouse_pressed = 1; 
        g_mouse_x = event.button.x;
        g_mouse_y = event.button.y;
    }
}

static void
mouse_up(SDL_Event event)
{
    if (event.button.button == SDL_BUTTON_LEFT) {
        //g_mouse_button  = SDL_BUTTON_LEFT;
        g_mouse_pressed = 0; 
    }
}

static void
mouse_motion(SDL_Event event)
{

    if (g_mouse_pressed) {

        if (g_mouse_button == SDL_BUTTON_LEFT) {

            g_offt_x += (event.button.x - g_mouse_x) / 50.0f;
            g_offt_y += (event.button.y - g_mouse_y) / 50.0f;

        }
    }

    g_mouse_x = event.button.x;
    g_mouse_y = event.button.y;
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

            switch (event.type) {
            case SDL_MOUSEBUTTONDOWN:
                mouse_down(event);
                break;
            case SDL_MOUSEBUTTONUP:
                mouse_up(event);
                break;
            case SDL_MOUSEMOTION:
                mouse_motion(event);
                break;
            }
        }

        display();
        SDL_Delay(0);
    }

quit_app:

    SDL_Quit();

}

void
init_render()
{
    gtex = texture_load("muda512.ppm");
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
    char cacheGenPassFName[1024];

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

    sprintf(cacheGenPassFName, "%s_cache_gen_pass", basename);
        
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

    Function *F, *CacheGenF;
    void *FunP, *CacheGenFunP;

    F = (M.get())->getFunction(basename);
    if (F == NULL) {
        cerr << "can't find the function [ " << basename << " ] from the module\n";
        return 1;
    }

    CacheGenF = (M.get())->getFunction(cacheGenPassFName);
    if (CacheGenF == NULL) {
        cerr << "can't find the function [ " << cacheGenPassFName << " ] from the module\n";
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
    g_shader_jit.shader_fun = (ShaderFunP)FunP;

    CacheGenFunP = JITCompileFunction(EE, CacheGenF); 
    printf("CacheGenFunP = 0x%08x\n", FunP);
    g_shader_jit.shader_cache_gen_fun = (ShaderCacheGenFunP)CacheGenFunP;

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

    init_render();
    lse_init(WINDOW_WIDTH, WINDOW_HEIGHT);

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
