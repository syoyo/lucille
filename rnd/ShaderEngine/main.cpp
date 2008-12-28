#include "gui.h"        // FLTK resources
#include "callbacks.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <string>

const char *defaultShaderFile = "matte.sl";
const char *outputLLFile      = "output.ll";

void
init()
{
    rslTextBuf  = new Fl_Text_Buffer(30000);
    llvmTextBuf = new Fl_Text_Buffer(30000);
    jitTextBuf  = new Fl_Text_Buffer(30000);

    const char *llext = "ll";
    char *p;
    char buf[1024];

    int ret;
    int len;
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

int
main(int argc, char **argv)
{
    init(); // Should call here.

    //
    // Create a window.
    //
    Fl_Double_Window *renderWindow = makeRenderWindow();
    Fl_Double_Window *shaderWindow = makeShaderWindow();

    rslTextDisplay->buffer(rslTextBuf);
    llvmTextDisplay->buffer(llvmTextBuf);
    jitTextDisplay->buffer(jitTextBuf);
    
    renderWindow->show();

    shaderWindow->position( renderWindow->x() + renderWindow->w() + 15,
                            renderWindow->y() );

    shaderWindow->show();

    renderWindow->show();

    Fl::focus(renderWindow);

    return Fl::run();
}
