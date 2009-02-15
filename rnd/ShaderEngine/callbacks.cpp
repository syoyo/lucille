#include <FL/Fl_Window.h>
#include <FL/Fl_Text_Display.h>
#include <FL/Fl_Text_Buffer.h>
#include <FL/Fl_File_Chooser.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <string>

#include "gui.h"
#include "callbacks.h"
#include "jit.h"
#include "constants.h"

using namespace std;

static const char *outputLLFile = "output.ll";
static const char *shaderFileName = NULL;
static int compileSuccess = 0;

static const char *getBitcodeName(const char *shaderName)
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

static const char *getBaseShaderFileName(const char *shaderName)
{
    const char *bcext = ".bc";
    char *p;
    int  len;
    char buf[1024];
    char *basename;
    char baseName[1024];

    p = strrchr(shaderName, '/');
    assert(p != NULL);

    len = p - shaderName + 1;
    strncpy(buf, shaderName + len, strlen(shaderName) - len);

    buf[strlen(shaderName) - len] = '\0';

    printf("val = [%s]\n", buf);
    return strdup(buf);
}

int
compileShader(const char *fname)
{
    int ret;
    string cmd;

    cmd = "./compile.py ";
    cmd += fname;

    printf(cmd.c_str());

    ret = system(cmd.c_str());
    printf("retcode = %d\n", ret);

    return 0;
}

int
openRSLAndAddToDisplay(const char *fname)
{
    char buf[1024];
    FILE *fp;

    fp = fopen(fname, "r");
    if (!fp) {
        printf("failed to open file [ %s ]\n", fname);
        return -1;
    }

    rslTextBuf->remove(0, rslTextBuf->length());
    while (fgets(buf, 1024, fp) != NULL) {
        rslTextBuf->append(buf);
    }

    fclose(fp);

    printf("Loaded SL file: %s\n", fname);
    printf("[dbg] text = \n%s\n", rslTextBuf->text()); 

    return 0;
}

int
openLLAndAddToDisplay(const char *fname)
{
    char buf[1024];
    FILE *fp;

    fp = fopen(fname, "r");
    if (!fp) {
        printf("failed to open file [ %s ]\n", fname);
        return -1;
    }

    llvmTextBuf->remove(0, llvmTextBuf->length());
    while (fgets(buf, 1024, fp) != NULL) {
        llvmTextBuf->append(buf);
    }

    fclose(fp);

    printf("Loaded SL file: %s\n", fname);
    printf("[dbg] text = \n%s\n", llvmTextBuf->text()); 

    return 0;
}

void
handleOpenRSL()
{
    Fl_File_Chooser *fc;
    char buf[1024];
    const char *shaderName;

    fc = new Fl_File_Chooser(".", "*.sl", Fl_File_Chooser::SINGLE, "Open RenderMan Shading Language source");

    fc->show();

    while (fc->visible()) {
        Fl::wait();
    }

    if (fc->count() > 0) {
        shaderName = getBaseShaderFileName(fc->value());
        setShaderFile(shaderName);
        openRSLAndAddToDisplay(fc->value());
    }
}

void
setShaderFile(const char *fname)
{
    shaderFileName = fname;
}

void
showRenderTime(float sec)
{
    char text[1024];

    sprintf(text, "%f sec", sec);

    secDisplay->value(text);
}

//
// GUI callback event handler
//

void
compile_cb()
{
    int ret;
    printf("compile button pressed\n");
    assert(shaderFileName && "shader is not set\n");
    ret = compileShader(shaderFileName);

    if (ret == 0) {
        // outputCompileResult->value("OK");
        compileSuccess = 1;
        openLLAndAddToDisplay(outputLLFile);
    } else {
        // outputCompileResult->value("FAILED");
        compileSuccess = 0;
    }

    const char *bcName = getBitcodeName(shaderFileName);
    jitInit(bcName, WINDOW_SIZE, WINDOW_SIZE);
    
}

void specialization_cb()
{
    int onoff = buttonSpecialization->value();
    printf("specialization %d\n", onoff);

    setSpecialized(onoff);
}

void coarse_update_cb()
{
    int onoff = button4x4Update->value();
    printf("coarse = %d\n", onoff);

    setCoarseUpdate(onoff);
}

void help_cb()
{
    Fl_Window *win = new Fl_Window(600, 400);
    win->label("help");
    win->begin();

    Fl_Text_Buffer *text = new Fl_Text_Buffer();
    text->append(" Technical demonstration of shader JIT and specialization.\n\n");
    text->append(" (Specialize) button -> on/off shader specialization\n");
    text->append(" (4x4 update) button -> on/off coarse update(valid when specialization is on)\n");
    text->append("\n\n Mouse drag          -> move light pos\n");
    text->append("\n\n You'll see faster update when specialization is on.\n");

    Fl_Text_Display *textDisplay = new Fl_Text_Display(10, 40, 580, 350, "");
    textDisplay->buffer(text);

    win->end();
    win->show();

    // FIXME: memory leak.
}

