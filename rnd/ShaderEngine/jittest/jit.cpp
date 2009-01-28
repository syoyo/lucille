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

#define MODE_SPECIALIZE_COARSE_UPDATE 0
#define MODE_SPECIALIZE_FINE_UPDATE   2
#define MODE_FULL_UPDATE              1

SDL_Surface     *surface;
unsigned char   *img;
int              g_mouse_button;
int              g_mouse_pressed = 0;
int              g_mouse_x;
int              g_mouse_y;
float            g_offt_x;
float            g_offt_y;
int              g_enable_specialization = 1;
int              g_cached = 0;
int              g_skip   = 1;
int              g_mode   = MODE_SPECIALIZE_COARSE_UPDATE;

bool             g_run_interpreter = false;

using namespace llvm;

static FunctionPassManager *TheFPM;
static ExecutionEngine     *TheEE;

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
typedef void (*ShaderDynamicFunP)(void);
typedef void (*ShaderEnvSetFunP)(ri_shader_env_t *env);
typedef void (*ShaderEnvGetFunP)(ri_shader_env_t *env);

typedef struct _ri_shader_jit_t
{

    ShaderEnvSetFunP    shader_env_set;
    ShaderEnvGetFunP    shader_env_get;
    ShaderFunP          shader_fun;
    ShaderCacheGenFunP  shader_cache_gen_fun;
    ShaderDynamicFunP   shader_dynamic_fun;

    // Used for execution by interpreter.
    Function           *shader_env_set_f;
    Function           *shader_env_get_f;
    Function           *shader_fun_f;
    Function           *shader_cache_gen_fun_f;
    Function           *shader_dynamic_fun_f;

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

static double vdot(vec v0, vec v1)
{
    return v0.x * v1.x + v0.y * v1.y + v0.z * v1.z;
}

static void vcross(vec *c, vec v0, vec v1)
{
    
    c->x = v0.y * v1.z - v0.z * v1.y;
    c->y = v0.z * v1.x - v0.x * v1.z;
    c->z = v0.x * v1.y - v0.y * v1.x;
}

static void vnormalize(vec *c)
{
    double length = sqrt(vdot((*c), (*c)));

    if (fabs(length) > 1.0e-17) {
        c->x /= length;
        c->y /= length;
        c->z /= length;
    }
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
    if (name == "lse_load_cache_iiic") return (void *)lse_load_cache_iiic;

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

/*
 * Re-render the image using cache.
 */
void
dummy_rerender(int width, int height, int skip)
{

    int         x, y;
    mytimer_t   start_time, end_time;
    double      elap;

    ray_t       ray;
    float       texcol[4];
    isect_t     isect;

    ri_shader_env_t *ret_env;    // FIXME: check align.

    std::vector<GenericValue> Args(1);
    GenericValue ResultGV;

    ret_env = (ri_shader_env_t *)malloc32(sizeof(ri_shader_env_t));

    get_time(&start_time);

    float theta, phi;
    float tu, tv;
    float cacheval[4];
    float Pcache[4];
    float UVTcache[4];
    float Ncache[4];
    unsigned char shadecol[4];

    float u, v;
    float dist;
    int   hit;
    int   xx, yy;

    Args[0].PointerVal = (void *)&genv;

    ShaderFunP shader_fun;

    if (g_enable_specialization) {
        if (g_cached) {
            shader_fun = g_shader_jit.shader_dynamic_fun;
        } else {
            shader_fun = g_shader_jit.shader_cache_gen_fun;
        }
    } else {
        shader_fun = g_shader_jit.shader_fun;
    }
        

    for (y = 0; y < height; y += skip) {
        for (x = 0; x < width; x += skip) {

            u = (x - 0.5f * width) / (float)(0.5f * width);
            v = (y - 0.5f * height) / (float)(0.5f * height);

            ray.org.x = 0.0f;
            ray.org.y = 0.0f;
            ray.org.z = 0.0f;

            ray.dir.x = u;
            ray.dir.y = v;
            ray.dir.z = -1.0f;

            vnormalize(&ray.dir);

            // load cached uv and t.
            lse_load_cache_iiic(UV_LAYER, x, y, UVTcache);

            dist = UVTcache[2];
            if (dist > 0.0f) {

                // load cached P
                lse_load_cache_iiic(P_LAYER, x, y, Pcache);

                // load cached N
                lse_load_cache_iiic(N_LAYER, x, y, Ncache);

                genv.Cs[0] = 1.0f;
                genv.Cs[1] = 1.0f;
                genv.Cs[2] = 1.0f;
                genv.Os[0] = 1.0f;
                genv.Os[1] = 1.0f;
                genv.Os[2] = 1.0f;
                genv.Cl[0] = 0.8f;
                genv.Cl[1] = 1.0f;
                genv.Cl[2] = 0.5f;
                genv.N[0] = Ncache[0];
                genv.N[1] = Ncache[1];
                genv.N[2] = Ncache[2];
                genv.L[0] = 1.0f + g_offt_x;
                genv.L[1] = 1.0f + g_offt_y;
                genv.L[2] = 1.0f;
                genv.P[0] = Pcache[0];
                genv.P[1] = Pcache[1];
                genv.P[2] = Pcache[2];
                genv.P[3] = 1.0f;
                genv.I[0] = Pcache[0] - ray.org.x;
                genv.I[1] = Pcache[1] - ray.org.y;
                genv.I[2] = Pcache[2] - ray.org.z;
                genv.I[3] = 1.0f;
                genv.s = UVTcache[0];
                genv.t = UVTcache[1];
                genv.x = x;
                genv.y = y;

                g_shader_jit.shader_env_set(&genv);
                shader_fun();
                g_shader_jit.shader_env_get(&genv);

                shadecol[0] = clamp(genv.Ci[0]);
                shadecol[1] = clamp(genv.Ci[1]);
                shadecol[2] = clamp(genv.Ci[2]);

            } else {
                
                shadecol[0] = 0;
                shadecol[1] = 0;
                shadecol[2] = 0;

            }

            if (skip != 1) {

                for (yy = 0; yy < skip; yy++) {
                    for (xx = 0; xx < skip; xx++) {

                        img[3 * ((y+yy) * width + (x+xx)) + 0] = shadecol[0];
                        img[3 * ((y+yy) * width + (x+xx)) + 1] = shadecol[1];
                        img[3 * ((y+yy) * width + (x+xx)) + 2] = shadecol[2];
                    }
                }
                

            } else {

                img[3 * (y * width + x) + 0] = shadecol[0];
                img[3 * (y * width + x) + 1] = shadecol[1];
                img[3 * (y * width + x) + 2] = shadecol[2];

            }

        }
    }

    get_time(&end_time);

    elap = elapsed_time(&start_time, &end_time); 

    printf("\r[Re-render] elapsed = %f (sec)", elap);
    fflush(stdout);


}

/*
 * Render the image with full update.
 */
void
dummy_render(int width, int height)
{
    int         x, y;
    mytimer_t   start_time, end_time;
    double      elap;

    float       texcol[4];
    //sphere_t    sphere;
    isect_t     isect;
    ray_t       ray;

    ri_shader_env_t *ret_env;    // FIXME: check align.

    std::vector<GenericValue> Args(1);
    GenericValue ResultGV;

    ret_env = (ri_shader_env_t *)malloc32(sizeof(ri_shader_env_t));

    //sphere.center.x = 0.0;
    //sphere.center.y = 0.0;
    //sphere.center.z = -2.0;
    //sphere.radius    = 0.75;

    get_time(&start_time);

    float theta, phi;
    float tu, tv;
    float cacheval[4];
    unsigned char shadecol[4];

    float u, v;
    int   hit;
    int   xx, yy;

    Args[0].PointerVal = (void *)&genv;

    ShaderFunP shader_fun;

    if (g_enable_specialization) {
        if (g_cached) {
            shader_fun = g_shader_jit.shader_dynamic_fun;
        } else {
            shader_fun = g_shader_jit.shader_cache_gen_fun;
        }
    } else {
        shader_fun = g_shader_jit.shader_fun;
    }
        

    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {

            isect.t = 1.0e+30f;
            
            u = (x - 0.5f * width) / (float)(0.5f * width);
            v = (y - 0.5f * height) / (float)(0.5f * height);

            ray.org.x = 0.0f;
            ray.org.y = 0.0f;
            ray.org.z = 0.0f;

            ray.dir.x = u;
            ray.dir.y = v;
            ray.dir.z = -1.0f;

            vnormalize(&ray.dir);

            ray_sphere_intersect(&isect, &ray, &scene_spheres[0]);
            //ray_sphere_intersect(&isect, &ray, &scene_spheres[1]);

            hit = isect.t < 1.0e+30f;

            if (hit) {

                theta = acosf(isect.n.y);
                tv = theta / M_PI;

                phi = atan2f(-isect.n.z, isect.n.x);
                tu = (phi + M_PI) / (2.0 * M_PI);

                //texture_map(texcol, gtex, tu, tv);

                genv.Cs[0] = 1.0f;
                genv.Cs[1] = 1.0f;
                genv.Cs[2] = 1.0f;
                genv.Os[0] = 1.0f;
                genv.Os[1] = 1.0f;
                genv.Os[2] = 1.0f;
                genv.Cl[0] = 0.8f;
                genv.Cl[1] = 1.0f;
                genv.Cl[2] = 0.5f;
                genv.N[0] = isect.n.x;
                genv.N[1] = isect.n.y;
                genv.N[2] = isect.n.z;
                genv.L[0] = 1.0f + g_offt_x;
                genv.L[1] = 1.0f + g_offt_y;
                genv.L[2] = 1.0f;
                genv.P[0] = isect.p.x;
                genv.P[1] = isect.p.y;
                genv.P[2] = isect.p.z;
                genv.P[3] = 1.0f;
                genv.I[0] = isect.p.x - ray.org.x;
                genv.I[1] = isect.p.y - ray.org.y;
                genv.I[2] = isect.p.z - ray.org.z;
                genv.I[3] = 1.0f;
                genv.s = tu;
                genv.t = tv;
                genv.x = x;
                genv.y = y;

                // cache P
                cacheval[0] = isect.p.x;
                cacheval[1] = isect.p.y;
                cacheval[2] = isect.p.z;
                lse_save_cache_iiic(P_LAYER, x, y, cacheval);

                // cache N
                cacheval[0] = isect.n.x;
                cacheval[1] = isect.n.y;
                cacheval[2] = isect.n.z;
                lse_save_cache_iiic(N_LAYER, x, y, cacheval);

                // cache UV and T
                cacheval[0] = tu;
                cacheval[1] = tv;
                cacheval[2] = isect.t;
                lse_save_cache_iiic(UV_LAYER, x, y, cacheval);


#if 0
                if (g_run_interpreter) {
                    ResultGV = TheEE->runFunction(g_shader_jit.shader_env_set_f, Args);
                    //g_shader_jit.shader_cache_gen_fun();
                    //g_shader_jit.shader_fun();
                    //g_shader_jit.shader_env_get(&genv);
                } else {
                    g_shader_jit.shader_env_set(&genv);
                    g_shader_jit.shader_cache_gen_fun();
                    //g_shader_jit.shader_fun();
                    g_shader_jit.shader_env_get(&genv);
                }
#endif

                g_shader_jit.shader_env_set(&genv);
                shader_fun();
                g_shader_jit.shader_env_get(&genv);

                shadecol[0] = clamp(genv.Ci[0]);
                shadecol[1] = clamp(genv.Ci[1]);
                shadecol[2] = clamp(genv.Ci[2]);

            } else {

                // cache UV and T
                cacheval[0] = 0.0f;
                cacheval[1] = 0.0f;
                cacheval[2] = -1.0f;    // Neg = not hit.
                lse_save_cache_iiic(UV_LAYER, x, y, cacheval);
                
                shadecol[0] = 0;
                shadecol[1] = 0;
                shadecol[2] = 0;

            }


            img[3 * (y * width + x) + 0] = shadecol[0];
            img[3 * (y * width + x) + 1] = shadecol[1];
            img[3 * (y * width + x) + 2] = shadecol[2];

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
createJITEngine(Module *M, bool ForceInterpreter)
{
    
    ExistingModuleProvider* MP = new ExistingModuleProvider(M);
    ExecutionEngine* EE = ExecutionEngine::create(MP, ForceInterpreter);

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

    if (g_cached) {

        dummy_rerender(WINDOW_WIDTH, WINDOW_HEIGHT, g_skip);

    } else {

        dummy_render(WINDOW_WIDTH, WINDOW_HEIGHT);
        g_cached = 1;

    }

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
        if (g_enable_specialization) {
            if (g_mode == MODE_SPECIALIZE_COARSE_UPDATE) {
                g_skip = 4;
            }
        }
    }
}

static void
mouse_up(SDL_Event event)
{
    if (event.button.button == SDL_BUTTON_LEFT) {
        //g_mouse_button  = SDL_BUTTON_LEFT;
        g_mouse_pressed = 0; 
        g_skip = 1;
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
keyboard(SDL_Event event)
{
    
    if (event.key.keysym.sym == 'j') {
        g_mode++;

        if (g_mode > 2) {
            g_mode = MODE_SPECIALIZE_COARSE_UPDATE;
        }

        switch (g_mode) {
        case MODE_SPECIALIZE_COARSE_UPDATE:
            g_enable_specialization = 1;
            g_cached = 0;   // clear cache
            break;
        case MODE_SPECIALIZE_FINE_UPDATE:
            g_enable_specialization = 1;
            g_cached = 0;   // clear cache
            break;
        case MODE_FULL_UPDATE:
            g_enable_specialization = 0;
            break;
        }

        printf("Mode = %d\n", g_mode);

    }

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
            case SDL_KEYUP:
                keyboard(event);
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
    init_render_scene();
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
    char dynamicPassFName[1024];

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
    sprintf(dynamicPassFName, "%s_dynamic_pass", basename);
        
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

    TheEE = createJITEngine(M.get(), g_run_interpreter);
    TheEE->InstallLazyFunctionCreator(customSymbolResolver);

    Function *F, *CacheGenF, *DynamicF;
    void *FunP, *CacheGenFunP, *DynamicFunP;

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

    DynamicF = (M.get())->getFunction(dynamicPassFName);
    if (DynamicF == NULL) {
        cerr << "can't find the function [ " << dynamicPassFName << " ] from the module\n";
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

        ptr = TheEE->getPointerToFunction(ShaderEnvSetF);
        assert(ptr != NULL);

        
        g_shader_jit.shader_env_set = (ShaderEnvSetFunP)ptr;
        g_shader_jit.shader_env_set_f = ShaderEnvSetF;
    }

    FunP = JITCompileFunction(TheEE, F); 
    printf("FunP = 0x%08x\n", FunP);
    g_shader_jit.shader_fun = (ShaderFunP)FunP;
    g_shader_jit.shader_fun_f = F;

    CacheGenFunP = JITCompileFunction(TheEE, CacheGenF); 
    printf("CacheGenFunP = 0x%08x\n", FunP);
    g_shader_jit.shader_cache_gen_fun = (ShaderCacheGenFunP)CacheGenFunP;
    g_shader_jit.shader_cache_gen_fun_f = CacheGenF;

    DynamicFunP = JITCompileFunction(TheEE, DynamicF); 
    printf("DynamicFunP = 0x%08x\n", FunP);
    g_shader_jit.shader_dynamic_fun   = (ShaderDynamicFunP)DynamicFunP;
    g_shader_jit.shader_dynamic_fun_f = DynamicF;

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

        ptr = TheEE->getPointerToFunction(ShaderEnvGetF);
        assert(ptr != NULL);

        g_shader_jit.shader_env_get = (ShaderEnvGetFunP)ptr;
        g_shader_jit.shader_env_get_f = ShaderEnvGetF;

    }

    dump_shader_env();

    init_render();

    srand48(13);

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
