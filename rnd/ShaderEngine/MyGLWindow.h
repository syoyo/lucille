#ifndef MYGLWINDOW_H
#define MYGLWINDOW_H

#include <Fl/Fl.H>
#include <Fl/Fl_Gl_Window.H>
#include <Fl/gl.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif

#include <stdio.h>

// #include "glm.h"
#include "trackball.h"

class MyGLWindow : public Fl_Gl_Window {

    void draw();
    void resizeImageBuffer(int x, int y, int w, int h);
    int  handle(int e);
    bool handleKey(int k);
	
public:
    void resize(int x, int y, int w, int h);

    MyGLWindow(int x, int y, int w, int h, const char *l = 0) : Fl_Gl_Window(x, y, w, h, l) {

        // obj = 0;

        viewOrg[0] = 0.0f;
        viewOrg[1] = 0.0f;
        viewOrg[2] = -20.0f;

        viewTarget[0] = 0.0f;
        viewTarget[1] = 0.0f;
        viewTarget[2] = -10.0f;

        trackball(currQuat, 0.0, 0.0, 0.0, 0.0);

        displayImage  = false;
        image         = 0;
        fimage        = 0;

        lx = 0.0f;
        ly = 0.0f;

        this->resizeImageBuffer(x, y, w, h);

    };

    // void      setObj(GLMmodel *obj) { this->obj = obj;  };
    // GLMmodel *getObj() const        { return this->obj; };

    void      setImage(unsigned char *image, int width, int height) {
        
        this->image       = image;
        this->imageWidth  = width;
        this->imageHeight = height;

    }

    void      renderImage();

    void      getView( float eye[4], float lookat[4], float up[4] );
    void      restore();

    void      showImage() { this->displayImage = true; };
    void      hideImage() { this->displayImage = false; };

    float     viewOrg[3];
    float     viewTarget[3];

private:

    void glInit();


    // GLMmodel       *obj;

    // mouse
    float          currQuat[4];
    float          prevQuat[4];
    int            mouseX, mouseY;
    int            pressed;
    int            pressedButton;

    unsigned char *image;
    float         *fimage;
    int            imageWidth;
    int            imageHeight;

    bool           displayImage;

    // light pos(intaracted by mouse movement)
    float          lx, ly;

};
    
#endif  // MYGLWINDOW_H
