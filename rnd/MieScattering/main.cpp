#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/gl.h>
#include <FL/Fl_Gl_Window.H>
#include <FL/Fl_Value_Slider.H>

#include "mie.h"

#define RESOLUTION 1024

double g_wavelength    = 0.600; // [um]
double g_particle_size = 1.0000;
double g_fat_content   = 4;

ri_mie_phase_function_t phase;
    
static double
find_max_intensity()
{
    int i;
    double max_intensity = 0.0;

    for (i = 0; i < RESOLUTION; i++) {
        if (max_intensity < phase.phase[i]) {
            max_intensity = phase.phase[i];
        }
    }

    return max_intensity;
}

static void draw_circle_coord()
{

    int i;
    float c, s;

    glLineWidth(1.0);
    glColor3f(0.0, 1.0, 0.0);
    glBegin(GL_LINE_LOOP);
    for (i = 0; i < RESOLUTION; i++) {
        c = cosf((i / (float)RESOLUTION) * 2.0 * M_PI);
        s = sinf((i / (float)RESOLUTION) * 2.0 * M_PI);

        glVertex2f(c, s);
    }

    glEnd();

}

static void draw_phase()
{
    int i;
    float c, s;

    double max_val = find_max_intensity();
    double r = 1.0 / max_val;

    printf("max_val = %f\n", max_val);

    r = 1.0;

    glLineWidth(2.0);
    glColor3f(1.0, 1.0, 1.0);
    glBegin(GL_LINE_LOOP);
    for (i = 0; i < RESOLUTION; i++) {
        c = r * phase.phase[i] * cosf((i / (float)RESOLUTION) * 2.0 * M_PI);
        s = r * phase.phase[i] * sinf((i / (float)RESOLUTION) * 2.0 * M_PI);

        glVertex2f(c, s);
    }

    glEnd();

}

class MyGLWindow : public Fl_Gl_Window {
    void draw();
    int  handle(int e);
    int  handleKey(int k);
    
  public:
    MyGLWindow(int x, int y, int w, int h, const char *l = 0) : Fl_Gl_Window(x, y, w, h, l) {
        
    }

};

int MyGLWindow::handleKey(int k)
{
    int state = Fl::event_state();

    switch (k) {
        case 27:
            exit(0);
            break;

        case 'r':
            if (state == FL_SHIFT) g_particle_size -= 10.0;
            else                   g_particle_size += 10.0;
            break;

        case 'l':
            if (state == FL_SHIFT) g_wavelength -= 10.0;
            else                   g_wavelength += 10.0;
            break;

        case 'f':
            g_fat_content += 1.0;
            break;
    }

    return 0;
}

int MyGLWindow::handle(int e)
{
    int k;

    int redraw = 0;

    switch(e) {
    case FL_KEYDOWN:
    case FL_SHORTCUT:

        k = Fl::event_key();
        this->handleKey(k);
        redraw = 1;

    }

    if (redraw) {

        ri_mie_compute_phase_function_milk(&phase, 
            g_wavelength,
            g_particle_size,
            g_fat_content);

        Fl::redraw();
        return 1;
    }

    return Fl_Gl_Window::handle(e);

}

void MyGLWindow::draw()
{
    if (!valid()) {
        valid(1);
        glLoadIdentity();
        glViewport(0, 0, w(), h());
    }

    glOrtho(-1, 1, -1, 1, 0, 2);
    glClear(GL_COLOR_BUFFER_BIT);

    draw_circle_coord();
    draw_phase();

}

void
test()
{
    double wavelength, particle_size;
    double fat_content;

    ri_mie_compute_phase_function_milk(&phase, 
        g_wavelength,
        g_particle_size,
        g_fat_content);
    
}

int
main(int argc, char **argv)
{
    test();

    Fl_Window window(500, 500);

    MyGLWindow glview(10, 10, window.w() - 20, window.h() - 20);
    window.resizable(&glview);

    window.end();
    window.show(argc, argv);

    glview.show();

    return Fl::run();

}

