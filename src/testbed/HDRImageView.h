#include <FL/Fl_Window.H>
#include <Fl/gl.h>

class HDRImageView : public Fl_Window {

    void draw();

  public:

    HDRImageView(int x, int y, int w, int h) : Fl_Window(x, y, w, h) {

        image       = new unsigned char[3 * w * h];
        floatImage  = new float[3 * w * h];
        imageWidth  = w;
        imageHeight = h;

    }

    unsigned char *image;
    float         *floatImage;
    int            imageWidth;
    int            imageHeight;
    
};
