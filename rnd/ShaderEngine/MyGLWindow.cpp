#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "MyGLWindow.h"

#include "render.h"
#include "timer.h"
#include "trackball.h"
#include "callbacks.h"

//
// -- static functions
//

static inline float
fclamp( float f, float fmin, float fmax )
{
    if (f < fmin) f = fmin;
    if (f > fmax) f = fmax;

    return f;
}

static inline unsigned char
clamp( float f )
{
    int i;

    i = f * 255.5f;

    if (i < 0  ) i = 0;
    if (i > 255) i = 255;

    return (unsigned char)i;

}

static void
tonemap( unsigned char *image, float *fimage, int width, int height, float minval, float maxval )
{
    int i;
    float fval;
    float invscale;

    invscale = 1.0f / (maxval - minval);
    for (i = 0; i < width * height * 3; i++) {
        fval = (fimage[i] - minval) * invscale;
        image[i] = clamp(fclamp(fval, 0.0f, 1.0f));
    }

}

static void
find_minmaxval( float *minval, float *maxval, float *fimage, int width, int height )
{
    int i;
    float fmax = fimage[0];
    float fmin = fimage[0];

    for (i = 1; i < width * height * 3; i++) {
        if (fmax < fimage[i]) fmax = fimage[i];
        if (fmin > fimage[i]) fmin = fimage[i];
    }

    (*minval) = fmin;
    (*maxval) = fmax;
}


void
MyGLWindow::glInit()
{
    GLfloat light_pos[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat light_col[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat light_amb[4] = {0.4, 0.4, 0.4, 0.2};

    glLightfv(GL_LIGHT0, GL_POSITION, light_pos);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_col);
    glLightfv(GL_LIGHT0, GL_AMBIENT, light_amb);

    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);

    // glCullFace(GL_FRONT);
    glEnable(GL_NORMALIZE);

    glEnable(GL_DEPTH_TEST);


}

bool
MyGLWindow::handleKey(int k)
{
    bool needRedraw = false;

    switch (k) {
    case 'g':
        // this->renderImage();
        needRedraw = true;
        break;
    
    default:
        break;
    }

    return needRedraw;
    
}

int
MyGLWindow::handle(int e)
{
    int x, y;
    int mx, my;
    float t = -1.0f;
    float shiftScale = 10.0f;
    float zoomScale  =  8.0f;

    bool needRedraw = false;

    mx = this->mouseX;
    my = this->mouseY;

    int k;
    int state;

    float eye[4], lookat[4], up[4];

    state = Fl::event_state();

    switch (e) {

        // Keyborad
        case FL_KEYDOWN:
        case FL_SHORTCUT:
            k = Fl::event_key();
            needRedraw = this->handleKey(k);
            break;
    
        case FL_PUSH:

            this->pressed = 1;
            this->pressedButton = Fl::event_button();
            this->mouseX = Fl::event_x();
            this->mouseY = Fl::event_y();

            if ((state & FL_SHIFT) && (this->pressedButton == FL_LEFT_MOUSE)) {
                this->pressedButton = FL_MIDDLE_MOUSE;
            }

            if ((state & FL_CTRL) && (this->pressedButton == FL_LEFT_MOUSE)) {
                this->pressedButton = FL_RIGHT_MOUSE;
            }

            // if (this->displayImage == true) {
            //     this->displayImage = false;
            //     this->restore();
            // }

            trackball(this->prevQuat, 0, 0, 0, 0);

            return 1;   // To recieve FL_DRAG & FL_RELEASE, FL_PUSH must return 
            break;

        case FL_DRAG:

            x = Fl::event_x();
            y = Fl::event_y();


            if (this->pressed) {

                if (this->pressedButton == FL_LEFT_MOUSE) {

                    trackball(this->prevQuat,
                              t * (2.0 * mx - w()) / (float)w(),
                              t * (h() - 2.0 * my) / (float)h(),
                              t * (2.0 * x - w())  / (float)w(),
                              t * (h() - 2.0 * y)  / (float)h());

                    add_quats(this->prevQuat, this->currQuat, this->currQuat);


                } else if (this->pressedButton == FL_MIDDLE_MOUSE) {

                    this->viewOrg[0]    -= shiftScale * (mx - (float)x) / (float)w();
                    this->viewOrg[1]    += shiftScale * ((float)y - my) / (float)h();
                    this->viewTarget[0] -= shiftScale * (mx - (float)x) / (float)w();
                    this->viewTarget[1] += shiftScale * ((float)y - my) / (float)h();

                } else if (this->pressedButton == FL_RIGHT_MOUSE) {

                    this->viewOrg[2] += zoomScale*((float)y - this->mouseY) / (float)h();
                    this->viewTarget[2] += zoomScale*((float)y - this->mouseY) / (float)h();

                }

                needRedraw = true;
            }

            this->mouseX = x;
            this->mouseY = y;

            break;


        case FL_HIDE:
        case FL_RELEASE:
        case FL_LEAVE:
            this->pressed = 0;
            break;

        
    }

    if (needRedraw) {
    
        this->renderImage();
        Fl_Gl_Window::redraw();
        return 1;
    }

    return Fl_Gl_Window::handle(e);

}

void
MyGLWindow::restore()
{
    float fov = 45.0f;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0,0,w(),h());
    gluPerspective(fov, (float)w() / (float)h(), 0.01f, 100.0f);
    glMatrixMode(GL_MODELVIEW); 
    glLoadIdentity();

}

void
MyGLWindow::draw()
{
    GLfloat mat[4][4];

    // First time? init viewport, etc.
    if (!valid()) {
        valid(1);

        this->glInit();
        this->restore();

    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    if (this->displayImage && this->image) {

        glViewport(0,0,this->imageWidth,this->imageHeight);
        glMatrixMode(GL_PROJECTION);
        glPushMatrix();
        glLoadIdentity();
        gluOrtho2D(0.0, (float)(this->imageWidth), 0.0, (float)(this->imageHeight)); 
    
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();

        glDisable(GL_LIGHTING);
        glDisable(GL_DEPTH_TEST);
        glRasterPos2i(0, 0);
        glPixelZoom(1.0, 1.0);
        glDrawPixels(this->imageWidth, this->imageHeight, GL_RGB, GL_UNSIGNED_BYTE, this->image);

        glMatrixMode(GL_PROJECTION);
        glPopMatrix();
        glMatrixMode(GL_MODELVIEW);

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_LIGHTING);

    } else {
 
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        gluLookAt(this->viewOrg[0], this->viewOrg[1], this->viewOrg[2],
                  this->viewTarget[0], this->viewTarget[1], this->viewTarget[2],
                  0.0, 1.0, 0.0);

        build_rotmatrix(mat, this->currQuat);
        glMultMatrixf(&mat[0][0]);

        glPushMatrix();

        // GL draw here
        
        glPopMatrix();

    }

}

void
MyGLWindow::resizeImageBuffer(int X,int Y,int W,int H)
{
    delete this->image;
    delete this->fimage;

    this->imageWidth  = W;
    this->imageHeight = H;
    this->image  = new unsigned char [W * H * 3];
    this->fimage = new float [W * H * 3];

}

void
MyGLWindow::resize(int X,int Y,int W,int H)
{

    this->resizeImageBuffer(X, Y, W, H);

    Fl_Gl_Window::resize(X,Y,W,H);
    glLoadIdentity();
    glViewport(0,0,W,H);
    glOrtho(-W,W,-H,H,-1,1);
    redraw();
}

// r = M v
void vmatmul(float ret[4], float m[4][4], float v[4])
{
    int i;

    ret[0] = ret[1] = ret[2] = ret[3] = 0.0f;
    for (i = 0; i < 4; i ++) {
        ret[0] += m[0][i] * v[i];
        ret[1] += m[1][i] * v[i];
        ret[2] += m[2][i] * v[i];
        // no w coord
    }

}

void
MyGLWindow::getView( float eye[4], float lookat[4], float up[4] )
{
    float m[4][4];
    float localUp[4] = { 0.0f, 1.0f, 0.0f, 1.0f };

    build_rotmatrix(m, this->currQuat);

    float localEye[4];
    float localTarget[4];

    localEye[0] = this->viewOrg[0];
    localEye[1] = this->viewOrg[1];
    localEye[2] = this->viewOrg[2];
    localEye[3] = 1.0f;

    localTarget[0] = this->viewTarget[0];
    localTarget[1] = this->viewTarget[1];
    localTarget[2] = this->viewTarget[2];
    localTarget[3] = 1.0f;

    vmatmul(eye   , m, localEye);
    vmatmul(lookat, m, localTarget);
    vmatmul(up    , m, localUp);

    int i, j;
    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            printf("%f ", m[i][j]);
        }
        printf("\n");
    }

    printf("global eye    = %f, %f, %f\n", eye[0], eye[1], eye[2]);
    printf("global lookat = %f, %f, %f\n", lookat[0], lookat[1], lookat[2]);
    printf("global up     = %f, %f, %f\n", up[0], up[1], up[2]);
    
}

//
// -- Private
//
void
MyGLWindow::renderImage()
{

    float eye[4], lookat[4], up[4];

    printf("[render] renderImage()\n");

    this->getView( eye, lookat, up );

    mytimer_t s, e;
    
    assert(this->fimage);

    //
    // Render!
    // 
    printf("[render] render...\n");
    get_time(&s);
    dummy_render(this->imageWidth, this->imageHeight);
    //render( this->fimage, this->imageWidth, this->imageHeight, eye, lookat, up );
    memcpy(this->image, get_render_image(), this->imageWidth * this->imageHeight * 3);
    get_time(&e);

    double elap = elapsed_time(&s, &e);
    printf("[render] renderImage() finished: %f sec\n", elap);

    //
    // Apply tonemap
    //
    //tonemap(this->image, this->fimage, this->imageWidth, this->imageHeight, 0.0f, 1.0f);

    get_time(&s);
    this->showImage();
    get_time(&e);

    elap = elapsed_time(&s, &e);
    printf("[tonemap] tone mapping: %f sec\n", elap);

}
