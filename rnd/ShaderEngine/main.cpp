#include "gui.h"        // FLTK resources
#include "callbacks.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <string>

#define WINDOW_SIZE 256

const char *defaultShaderFile = "matte.sl";
const char *outputLLFile      = "output.ll";

void
slengine_init()
{
    rslTextBuf  = new Fl_Text_Buffer(30000);
    llvmTextBuf = new Fl_Text_Buffer(30000);

    const char *llext = "ll";
    char *p;
    char buf[1024];

    int ret;
    int len;

    setShaderFile(defaultShaderFile);
    ret = openRSLAndAddToDisplay(defaultShaderFile);

    if (ret == 0) {
        ret = compileShader(defaultShaderFile);
    }

    if (ret == 0) {
    
#if 0
        p = strrchr(defaultShaderFile, '.');
        assert(p != NULL);

        len = p - defaultShaderFile + 1;
        strncpy(buf, defaultShaderFile, len);
        
        strncpy(buf + len, llext, strlen(llext));
        buf[len + strlen(llext)] = '\0';
#endif
        

        openLLAndAddToDisplay(outputLLFile);
        
    }
}

const char *getBitcodeName(const char *shaderName)
{
    const char *bcext = ".bc";
    char *p;
    int  len;
    char buf[1024];
    char *basename;
    char bitcodeName[1024];

    p = strrchr(shaderName, '.');
    assert(p != NULL);

    len = p - shaderName + 1;
    strncpy(buf, shaderName, len);

    buf[len-1] = '\0';
    basename = buf;

    sprintf(bitcodeName, "%s.bc", basename);

    return strdup(bitcodeName);
}

int
main(int argc, char **argv)
{
    const char *bitcodeName;

    slengine_init(); // Should call here.

    int ret;
    bitcodeName = getBitcodeName(defaultShaderFile);
    ret = jitInit(bitcodeName, WINDOW_SIZE, WINDOW_SIZE);
    assert(ret == 0);

    //
    // Create a window.
    //
    Fl_Double_Window *renderWindow = makeRenderWindow();
    GLWindow->resize(GLWindow->x(), GLWindow->y(), WINDOW_SIZE, WINDOW_SIZE);
    Fl_Double_Window *shaderWindow = makeShaderWindow();

    llvmTextDisplay->buffer(llvmTextBuf);
    rslTextDisplay->buffer(rslTextBuf);
    
    renderWindow->show();

    shaderWindow->position( renderWindow->x() + renderWindow->w() + 15,
                            renderWindow->y() );

    shaderWindow->show();

    renderWindow->show();

    Fl::focus(renderWindow);

    return Fl::run();
}
