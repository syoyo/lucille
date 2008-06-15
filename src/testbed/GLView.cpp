#include <stdio.h>
#include <stdlib.h>

#include <Fl/Fl.H>
#ifdef __APPLE__
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif

#include "GLView.h"
#include "timer.h"
#include "controller.h"


void GLView::glInit()
{

    GLfloat light_pos[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat light_col[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat light_amb[4] = {0.4, 0.4, 0.4, 0.2};

    glLightfv(GL_LIGHT0, GL_POSITION, light_pos);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_col);
    glLightfv(GL_LIGHT0, GL_AMBIENT, light_amb);

    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);

    glEnable(GL_NORMALIZE);
    glEnable(GL_DEPTH_TEST);

}

static const char *viewfile = "view.dat";

static void saveView(GLView *w)
{
    FILE *fp;

    fp = fopen(viewfile, "w");
    if (!fp) {
        return;
    }

    fprintf(fp, "%f %f %f\n",
        w->viewOrg[0], w->viewOrg[1], w->viewOrg[2]);
    fprintf(fp, "%f %f %f\n",
        w->viewTarget[0], w->viewTarget[1], w->viewTarget[2]);
    fprintf(fp, "%f %f %f %f\n",
        w->currQuat[0], w->currQuat[1], w->currQuat[2], w->currQuat[3]);

    fclose(fp);

    printf("saved view.\n");
}

static void loadView(GLView *w)
{
    FILE *fp;

    fp = fopen(viewfile, "r");
    if (!fp) {
        return;
    }

    fscanf(fp, "%f %f %f\n",
        &w->viewOrg[0], &w->viewOrg[1], &w->viewOrg[2]);
    fscanf(fp, "%f %f %f\n",
        &w->viewTarget[0], &w->viewTarget[1], &w->viewTarget[2]);
    fscanf(fp, "%f %f %f %f\n",
        &w->currQuat[0], &w->currQuat[1], &w->currQuat[2], &w->currQuat[3]);

    fclose(fp);

    printf("loaded view.\n");
}

bool
GLView::handleKey(int k)
{

    bool needRedraw = false;

    switch (k) {
    case 's':
        loadView(this);
        needRedraw = true;
        break;

    case 'e':
        saveView(this);
        break;

    case 'w':
        this->wireMode = !this->wireMode;
        needRedraw = true;
        break;

    case 'g':
        this->renderImage();
        needRedraw = true;
        break;

    case 'u':
        this->bvhVisualizer->followParent();
        needRedraw = true;
        break;

    case 'l':
        this->bvhVisualizer->followLeft();
        needRedraw = true;
        break;

    case 'r':
        this->bvhVisualizer->followRight();
        needRedraw = true;
        break;

    }


    return needRedraw;

}

int
GLView::handle(int e)
{
    int x, y;
    int mx, my;
    int state;
    float t = -1.0f;

    bool needRedraw = false;

    mx = this->mouseX;
    my = this->mouseY;

    int k;

    switch (e) {

        // Keyborad
        case FL_KEYDOWN:
        case FL_SHORTCUT:
            k = Fl::event_key();

            needRedraw = this->handleKey(k);

            break;
    
        case FL_PUSH:

            state = Fl::event_state();

            this->pressed = 1;

            this->pressedButton = Fl::event_button();
            
            // Shift + L mouse -> M mouse
            if ( (state & FL_SHIFT) && (this->pressedButton == FL_LEFT_MOUSE) ) {
                this->pressedButton = FL_MIDDLE_MOUSE;
            }

            this->mouseX = Fl::event_x();
            this->mouseY = Fl::event_y();

            if (this->displayImage == true) {
                this->displayImage = false;
                this->restore();
            }

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

                    this->viewOrg[0]    -= 8.0 * (mx - (float)x) / (float)w();
                    this->viewOrg[1]    += 8.0 * ((float)y - my) / (float)h();
                    this->viewTarget[0] -= 8.0 * (mx - (float)x) / (float)w();
                    this->viewTarget[1] += 8.0 * ((float)y - my) / (float)h();

                } else if (this->pressedButton == FL_RIGHT_MOUSE) {

                    this->viewOrg[2] += 10.0*((float)y - this->mouseY) / (float)h();
                    this->viewOrg[2] += 10.0*((float)y - this->mouseY) / (float)h();

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
        Fl_Gl_Window::redraw();
        return 1;
    }

    return Fl_Gl_Window::handle(e);

}

void
GLView::restore()
{
    float fov = 45.0f;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0,0,w(),h());
    gluPerspective(fov, (float)w() / (float)h(), 0.01f, 5000.0f);
    glMatrixMode(GL_MODELVIEW); 
    glLoadIdentity();

}

void
GLView::draw()
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

#if 1
        //
        // Draw obj
        //
        if (this->obj) {
            if (this->wireMode) {
                glPolygonMode( GL_FRONT, GL_LINE );
            }
            glmDraw(this->obj, GLM_FLAT);
            if (this->wireMode) {
                glPolygonMode( GL_FRONT, GL_FILL );
            }
        }
#endif

        //
        // Draw bbox
        // 
        if (this->bvhVisualizer) {
            this->bvhVisualizer->drawBVH();
        }



        glPopMatrix();

    }


}

void
GLView::resize(int X,int Y,int W,int H)
{
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
GLView::getView( float eye[4], float lookat[4], float up[4] )
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
value_to_heatmap( float col[3], float val, float minval, float maxval )
{
    float red[3]   = { 1.0, 0.0, 0.0 };
    float green[3] = { 0.0, 1.0, 0.0 };
    float blue[3]  = { 0.0, 0.0, 1.0 };

    float s = (val - minval) / (maxval - minval);
    float t;

    if (s < 0.0) s = 0.0;
    if (s > 1.0) s = 1.0;

    if (s < 0.5) {

        t = s * 2.0;
        col[0] = (1.0 - t) * blue[0] + t * green[0];
        col[1] = (1.0 - t) * blue[1] + t * green[1];
        col[2] = (1.0 - t) * blue[2] + t * green[2];

    } else {

        t = (s - 0.5) * 2.0;
        col[0] = (1.0 - t) * green[0] + t * red[0];
        col[1] = (1.0 - t) * green[1] + t * red[1];
        col[2] = (1.0 - t) * green[2] + t * red[2];

    }

}

static void
heatmap( unsigned char *image, float *fimage, int width, int height, float minval, float maxval )
{
    int i;
    float fval;
    float invscale;
    float col[3];
    
    invscale = 1.0f / (maxval - minval);

    for (i = 0; i < width * height; i++) {
        fval = fimage[3*i];
        value_to_heatmap( col, fval, minval, maxval );
        image[3*i+0] = clamp(col[0]);
        image[3*i+1] = clamp(col[1]);
        image[3*i+2] = clamp(col[2]);
    }

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

    printf("maxval = %f\n", fmax);
    printf("minval = %f\n", fmin);

    (*minval) = fmin;
    (*maxval) = fmax;
}


void
GLView::renderImage()
{
    float eye[4], lookat[4], up[4];

    vec   veye, vlookat, vup;

    printf("[render] renderImage()\n");

    this->getView( eye, lookat, up );

    veye[0]    = eye[0];
    veye[1]    = eye[1];
    veye[2]    = eye[2];
    vlookat[0] = lookat[0];
    vlookat[1] = lookat[1];
    vlookat[2] = lookat[2];
    vup[0]     = up[0];
    vup[1]     = up[1];
    vup[2]     = up[2];

    mytimer_t s, e;
    
    get_time(&s);
    
    // TODO: refactor
    // render_image( eye, lookat, up );
    //
    
    render( this->floatImage, this->imageWidth, this->imageHeight,
            veye, vlookat, vup );
            
    get_time(&e);

    // this->setImage( grender->image, grender->width, grender->height );
    // this->showImage();
     
    float minval, maxval;
    find_minmaxval( &minval, &maxval, this->floatImage, this->imageWidth, this->imageHeight);

    switch (gvisualizeMode) {
    case VISUALIZE_NUM_TRAVERSALS:
        heatmap( this->image, this->floatImage, this->imageWidth, this->imageHeight , 0.0, 150.0);
        break;

    case VISUALIZE_NUM_ISECTS:
        heatmap( this->image, this->floatImage, this->imageWidth, this->imageHeight , 0.0, 50.0);
        break;

    default:
        tonemap( this->image, this->floatImage, this->imageWidth, this->imageHeight , minval, maxval);
        break;

    }

    this->displayImage = true;

    double elap = elapsed_time(&s, &e);
    printf("[render] renderImage() finished: %f sec\n", elap);
}
