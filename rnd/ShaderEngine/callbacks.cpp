#include <FL/Fl_File_Chooser.h>

#include <stdio.h>
#include <stdlib.h>

#include <string>

#include "gui.h"
#include "callbacks.h"

using namespace std;

int
compileShader(const char *fname)
{
    int ret;
    string cmd;

    cmd = "./compile.py ";
    cmd += fname;

    printf(cmd.c_str());

    ret = system(cmd.c_str());


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
        openRSLAndAddToDisplay(fc->value());
    }
}

