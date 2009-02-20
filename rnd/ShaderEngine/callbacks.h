#ifndef CALLBACKS_H
#define CALLBACKS_H

extern void setShaderFile(const char *fname);
extern int  openRSLAndAddToDisplay(const char *fname);
extern int  openLLAndAddToDisplay(const char *fname);
extern int  compileShader(const char *fname);


extern int  jitInit(const char *shaderModuleFilename, int w, int h);

// extern void dummy_rerender(int width, int height, int skip);
// extern void dummy_render  (int width, int height);
extern unsigned char *get_render_image();

extern void jitTest();

extern void showRenderTime(float sec);

//
// CB for FLTK GUI
//
extern void open_rsl_cb();
extern void compile_cb();
extern void coarse_update_cb();
extern void specialization_cb();
extern void help_cb();

#endif
