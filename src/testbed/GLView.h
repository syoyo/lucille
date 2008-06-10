#include <FL/Fl_Gl_Window.h>
#include <Fl/gl.h>

#include "trackball.h"
#include "glm.h"
#include "BVHVisualizer.h"

class GLView : public Fl_Gl_Window {

    void draw();
    void resize(int x, int y, int w, int h);
    int  handle(int e);
    void restore();

    void glInit();
    void getView( float eye[4], float lookat[4], float up[4] );

    bool handleKey(int k);

  public:

    GLView(int x, int y, int w, int h) : Fl_Gl_Window(x, y, w, h) {

        viewOrg[0]    =   0.0f;
        viewOrg[1]    =   0.0f;
        viewOrg[2]    = -10.0f;

        viewTarget[0] =   0.0f;
        viewTarget[1] =   0.0f;
        viewTarget[2] =   0.0f;

        trackball( currQuat, 0.0, 0.0, 0.0, 0.0 );

        bvhVisualizer = new BVHVisualizer();
    }

    void           renderImage();


    float          viewOrg[3];
    float          viewTarget[3];

    // mouse
    float          currQuat[4];
    float          prevQuat[4];
    int            mouseX, mouseY;
    int            pressed;
    int            pressedButton;
  
    unsigned char *image;
    int            imageWidth;
    int            imageHeight;
    
    bool           displayImage;

    GLMmodel      *obj;
    void           setObj(GLMmodel *obj) { this->obj = obj; }

    BVHVisualizer *bvhVisualizer;

};
