#ifndef CALLBACKS_H
#define CALLBACKS_H

extern void setShaderFile(const char *fname);
extern int  openRSLAndAddToDisplay(const char *fname);
extern int  openLLAndAddToDisplay(const char *fname);
extern int  compileShader(const char *fname);

extern void handleOpenRSL();

extern int  jitInit(const char *shaderModuleFilename, int w, int h);

extern void dummy_rerender(int width, int height, int skip);
extern void dummy_render  (int width, int height);
extern unsigned char *get_render_image();

//
// CB for FLTK GUI
//
void compile_cb();

#endif
