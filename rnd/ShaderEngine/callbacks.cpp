#include <FL/Fl_File_Chooser.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <string>

#include "gui.h"
#include "callbacks.h"

using namespace std;

static const char *outputLLFile = "output.ll";
static const char *shaderFileName = NULL;
static int compileSuccess = 0;

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

    fc = new Fl_File_Chooser(".", "*", Fl_File_Chooser::SINGLE, "Open RenderMan Shading Language source");

    fc->show();

    while (fc->visible()) {
        Fl::wait();
    }

    if (fc->count() > 0) {
        setShaderFile(fc->value());
        openRSLAndAddToDisplay(fc->value());
    }
}

void
setShaderFile(const char *fname)
{
    shaderFileName = fname;
}

void
compile_cb()
{
    int ret;
    printf("compile button pressed\n");
    assert(shaderFileName && "shader is not set\n");
    ret = compileShader(shaderFileName);

    if (ret == 0) {
        outputCompileResult->value("OK");
        compileSuccess = 1;
        openLLAndAddToDisplay(outputLLFile);
    } else {
        outputCompileResult->value("FAILED");
        compileSuccess = 0;
    }
    
}
