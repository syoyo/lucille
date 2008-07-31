/*
 * rockenfield, the framebuffer display driver for lucille.
 *
 * $Id: rockenfield.cpp 276 2007-04-07 12:01:44Z lucille $
 */
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include <cstdarg>
#include <string>
#include <iostream>

#ifdef WIN32
#include <winsock2.h>			// use WinSock version 2
#else
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#endif

#include <fstream>		/* open(), close() */

#ifdef WIN32
#include <windows.h>
#include <process.h>		/* _beginthread */
#else
#include <pthread.h>
#endif

using namespace std;

#include <FL/Fl.H>
#include <FL/Fl_File_Chooser.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Image.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Value_Slider.H>


#ifdef __cplusplus
extern "C" {
#endif
#include "../rgbe.h"

#ifdef WITH_PNG
#include "png.h"
#endif

#ifdef __cplusplus
}	/* extern C */
#endif

#include "../../src/display/sockdrv_defs.h"

#define DEFAULT_WIDTH  512
#define DEFAULT_HEIGHT 512

#define MAX_WINDOW_WIDTH   1024
#define MAX_WINDOW_HEIGHT  768

#ifdef WIN32
#define FLAGS 0
#else
#define FLAGS MSG_WAITALL
#endif

#ifdef WIN32
#define MYSOCKET  SOCKET
#define MYSOCKLEN int
#else
#define MYSOCKET  int
#define MYSOCKLEN socklen_t
#endif

#define PACKET_ARRIVED         0
#define PACKET_NOPACKET        1
#define PACKET_NEWCONNECTION   2

class MyWindow : public Fl_Double_Window {
	int handle(int event);
    public:
	MyWindow(int w, int h, const char *t=0L)
	  : Fl_Double_Window(w, h, t) {}; 
};

typedef struct _pixpacket
{
	int x;
	int y;
	float col[4];
} pixpacket;

typedef struct _imageinfo
{
	int width;
	int height;
} imageinfo;

typedef struct _state_t
{
	int connected;
	int comm_new;
	int comm_pixel;
	int comm_finish;
} state_t;

static const char *hdrfilename = NULL;
static unsigned char *imagebuf = NULL;
static float      *hdrimagebuf = NULL;
static float      *tm_hdrimagebuf = NULL;
static float       scaling = 1.0;
static int         zoom = 1;
static int         image_width = DEFAULT_WIDTH, image_height = DEFAULT_HEIGHT;
static int         window_width = DEFAULT_WIDTH, window_height = DEFAULT_HEIGHT;
static int         margin = 15;
static int         mousex = 0, mousey = 0;
static int         movex = 0, movey = 0;
static int         offsetx = 0, offsety = 0;
static int         mouse_left_pressed = 0;
static int         menu_heightmargin = 30;
static unsigned short g_port = DEFAULT_PORT;
static int         g_count = 0, g_update_count = 32 * 32;
static int         need_update = 0;
static int         need_raise = 0;
static int         g_debugmode = 0;
static state_t     g_state;
static int         g_eop = 0;			/* end of pixel data */
static char        gstrbuf[1024];
static MYSOCKET    g_fd;
static MYSOCKET    g_sockfd;
static pixpacket   g_packets[MAXPACKETS];
static struct sockaddr_in g_cl;
static unsigned char g_bg_col[3] = {102, 128, 180};
static unsigned char *rgbimagebuf;

// Image statistics
static double      Cav[3];		/* channel average	*/
static double	   Lav;			/* average luminance	*/
static double	   Llav;		/* log average lum	*/
static double	   Lmin, Lmax;		/* min and max lum	*/
static double      defContrast;		/* default contrast val */

// Static functions ----------------------------------------------------

static int check_packet(MYSOCKET fd);
static void tonemap_reinhard04_init();
static void tonemap_reinhard04(float *img,
			       double f, double m, double a, double c);

// ---------------------------------------------------------------------


// Static FLTK widgets -------------------------------------------------
static Fl_File_Chooser     *gfc;
#ifdef WITH_PNG
static Fl_File_Chooser     *gpng_save_fc;
#endif

// Main window
static MyWindow            *gMainWindow;
static Fl_Box		   *gPanel;	// image display panel.
static Fl_Menu_Bar         *gMenuBar;

// Log window
static Fl_Window           *gLogWindow;
static Fl_Text_Display     *gLogText;
static Fl_Text_Buffer      *gTextBuf;

// Tone mapping parameter window
static Fl_Window           *gToneMapParamWindow;
static Fl_Value_Slider     *gToneMapParamConstant;
static Fl_Value_Slider     *gToneMapParamIntensity;
static Fl_Value_Slider     *gToneMapParamChromaAdapt;
static Fl_Value_Slider     *gToneMapParamLightAdapt;

// Help window
static Fl_Window           *gHelpWindow;
static Fl_Text_Display     *gHelpText;
static Fl_Text_Buffer      *gHelpTextBuf;


// ---------------------------------------------------------------------

static void rescale(float scale);
static void refresh();
static void display_image_update(int off_x, int off_y, int scaling);

#ifdef WITH_PNG
static void save_png_file_cb(Fl_Widget *widget, void *data);
static void save_png_cb(Fl_Widget *widget, void *data);
#endif

static void save_hdr(const char *filename);
static void save_hdr_cb(Fl_Widget *widget, void *data);
static void load_hdr(const char *filename);
static void load_hdr_cb(Fl_Widget *widget, void *data);

static void quit_cb(Fl_Widget *widget, void *data) { exit(0); }
static void fc_cb(Fl_File_Chooser *fc, void *data);

static void logwindow_cb(Fl_Widget *widget, void *data);
static void clearlog_cb(Fl_Widget *widget, void *data);

static void helpwindow_cb(Fl_Widget *widget, void *data);
static void closehelp_cb(Fl_Widget *widget, void *data);

static void refresh()
{
	display_image_update(offsetx, offsety, zoom);
	gPanel->image()->uncache();
	Fl::redraw();
}


// Returns the radiance value after tone mapping.
static int pickup_value(int *posx, int *posy, float *vals)
{
	int x, y;

	x = (movex - offsetx) >> (zoom - 1);
	y = (movey - offsety) >> (zoom - 1);

	if ((x >= 0) &&	
	    (y >= 0) &&	
	    (x <  image_width) &&	
	    (y <  image_height)) {

		/*
		vals[0] = tm_hdrimagebuf[3 * (y * image_width + x) + 0];
		vals[1] = tm_hdrimagebuf[3 * (y * image_width + x) + 1];
		vals[2] = tm_hdrimagebuf[3 * (y * image_width + x) + 2];
		*/
		vals[0] = hdrimagebuf[3 * (y * image_width + x) + 0];
		vals[1] = hdrimagebuf[3 * (y * image_width + x) + 1];
		vals[2] = hdrimagebuf[3 * (y * image_width + x) + 2];

		(*posx) = x;
		(*posy) = y;

		return 1;

	} else {

		vals[0] = 0.0;
		vals[1] = 0.0;
		vals[2] = 0.0;

		return 0;
	}
}

static void display_image_update(int off_x, int off_y, int scaling)
{
	int i, j;
	int x, y;
	int scale_shift;
	unsigned int out_idx;
	unsigned int in_idx;

	int pixsize;

	if (scaling < 1) scaling = 1;

	for (j = 0; j < window_height; j++) {
		for (i = 0; i < window_width; i++) {

			out_idx = j * window_width + i;

			x = (i - off_x) >> (scaling - 1);
			y = (j - off_y) >> (scaling - 1);

			if ((x < 0) ||
			    (x > image_width - 1) ||
			    (y < 0) ||
			    (y > image_height - 1)) {
				// border.

			
				rgbimagebuf[3 * out_idx + 0] = g_bg_col[0];
				rgbimagebuf[3 * out_idx + 1] = g_bg_col[1];
				rgbimagebuf[3 * out_idx + 2] = g_bg_col[2];

				continue;
			} 

			in_idx = y * image_width + x;

			rgbimagebuf[3 * out_idx + 0] = imagebuf[3 * in_idx + 0];
			rgbimagebuf[3 * out_idx + 1] = imagebuf[3 * in_idx + 1];
			rgbimagebuf[3 * out_idx + 2] = imagebuf[3 * in_idx + 2];
		}
	}
}

void logging(const char *msg, ...)
{
	char buf[1024];

	va_list list;

	va_start(list, msg);

	vsprintf(buf, msg, list);

	gTextBuf->append(buf);

	//gLogText->value(gLogStr.c_str());

}

void clearlog_cb(Fl_Widget *widget, void *data)
{
	gTextBuf->text("");	
}

void closelog_cb(Fl_Widget *widget, void *data)
{
	gLogWindow->hide();
}

void closehelp_cb(Fl_Widget *widget, void *data)
{
	gHelpWindow->hide();
}


void increase_cb(Fl_Widget *widget, void *data) {
	char buf[256];

	if (scaling < 1.0) {
		scaling += 0.1;
	} else {
		scaling += 1.0;
	}

	logging("scaling = %f\n", scaling);

	rescale(scaling);
	refresh();
}

void decrease_cb(Fl_Widget *widget, void *data) {
	if (scaling <= 1.0) {
		scaling -= 0.1;
		if (scaling < 0.1) scaling = 0.1;
	} else {
		scaling -= 1.0;
	}

	logging("scaling = %f\n", scaling);

	rescale(scaling);
	refresh();
}

void zoomin_cb(Fl_Widget *widget, void *data) {

	zoom++;
	if (zoom > 8) {
		zoom = 8;
	} else {
		offsetx -= ((1 << (zoom - 1)) * image_width / 2) / 2;
		offsety -= ((1 << (zoom - 1)) * image_height / 2) / 2;

		printf("off = %d, %d\n", offsetx, offsety);
	}

	refresh();
}

void zoomout_cb(Fl_Widget *widget, void *data) {

	zoom--;
	if (zoom < 1) {
		zoom = 1;
	} else {

		offsetx += ((1 << (zoom - 1)) * image_width / 2);
		offsety += ((1 << (zoom - 1)) * image_height / 2);

	}

	refresh();
}

void logwindow_cb(Fl_Widget *widget, void *data)
{
	gLogWindow->show();
}

void helpwindow_cb(Fl_Widget *widget, void *data)
{
	gHelpWindow->show();
}


void tonemapwindow_cb(Fl_Widget *widget, void *data)
{
	gToneMapParamWindow->show();
}

Fl_Menu_Item item[] = {
	{ "&File", 0, 0, 0, FL_SUBMENU },
		{ "&Open", FL_ALT + 'o', load_hdr_cb },
#ifdef WITH_PNG
		{ "&Save as PNG", FL_ALT + 's', save_png_cb },
#endif
		{ "&Save as .hdr", FL_ALT + 's', save_hdr_cb },
		{ "&Quit", FL_ALT + 'q', quit_cb },
		{ "&Muda", FL_ALT + 'q', quit_cb },
		{ 0 },
	{ "&Image", 0, 0, 0, FL_SUBMENU },
		{ "&Zoom in", 'z', (Fl_Callback *)zoomin_cb },
		{ "&Zoom out", 'x', (Fl_Callback *)zoomout_cb },
		{ "&Increase intensity", '+', (Fl_Callback *)increase_cb },
		{ "&Decrease intensity", '-', (Fl_Callback *)decrease_cb },
		{ 0 },
	{ "&Window", 0, 0, 0, FL_SUBMENU },
		{ "&Log", 'l', (Fl_Callback *)logwindow_cb },
		{ "&Tone mapping", 't', (Fl_Callback *)tonemapwindow_cb },
		{ 0 },
	{ "&Help", 0, 0, 0, FL_SUBMENU },
		{ "&Show help", 'h', (Fl_Callback *)helpwindow_cb },
		{ 0 },
	{ 0 }
};


#ifdef WITH_PNG
void save_png_file(const char *pngfilename)
{
	int i, j;

	FILE *fp;
	png_structp png_ptr;
	png_infop info_ptr;
	const char *filename;
	png_bytep *rowp;

	logging("Saving the image to PNG file: %s\n", pngfilename);

	fflush(stdout);

	fp = fopen(pngfilename, "wb");
	if (!fp) {
		logging("Cannot save the file.\n");
		return;
	}

	rowp = (png_bytep *)malloc(sizeof(png_bytep *) * image_height);
	assert(rowp);

	for (i = 0; i < image_height; i++) {
		rowp[i] = (png_bytep)&imagebuf[3 * ((image_height - i - 1) * image_width)];
		
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
					  NULL, NULL, NULL);
	assert(png_ptr);
	info_ptr = png_create_info_struct(png_ptr);
	assert(info_ptr);

	png_init_io(png_ptr, fp);
	png_set_IHDR(png_ptr, info_ptr, image_width, image_height, 8, PNG_COLOR_TYPE_RGB,
		     PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
		     PNG_FILTER_TYPE_DEFAULT);
	png_write_info(png_ptr, info_ptr);
	png_write_image(png_ptr, rowp);
	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);

	free(rowp);

	fclose(fp);

	logging("Save OK.\n");
}

void
select_png_file_cb(Fl_File_Chooser *fc, void *data)
{
	//sprintf(pngfilename, "%s", fc->value());
}

void save_png_cb(Fl_Widget *widget, void *data)
{
	int count;

	gpng_save_fc->show();

	while (gpng_save_fc->visible()) {
		Fl::wait();
	}

	if (gpng_save_fc->count() > 0) {
		save_png_file(gpng_save_fc->value());
	}
}
#endif

void save_hdr(const char *filename)
{
        int              i, j;
        int              ret;
        FILE             *fp;

        fp = fopen(filename, "wb");
        if (!fp) {
		logging("Cannot open the file to write: %s\n", filename);
                return;
        }

        ret = RGBE_WriteHeader(fp, image_width, image_height, NULL);
        if (ret != RGBE_RETURN_SUCCESS) {
		logging("Cannot write RGBE header info\n");
		fclose(fp);
                return;
        }

        ret = RGBE_WritePixels_RLE(fp, hdrimagebuf, image_width, image_height);
        if (ret != RGBE_RETURN_SUCCESS) {
		logging("Can't write RGBE image content\n");
		fclose(fp);
                return;
        }

	fclose(fp);
}

void save_hdr_cb(Fl_Widget *widget, void *data)
{
	int count;

	// Make the .hdr save dialog
	Fl_File_Chooser *fc = new Fl_File_Chooser(
		".", "*.hdr", Fl_File_Chooser::CREATE,
				  "Save as .hdr file");

	fc->show();

	while (fc->visible()) {
		Fl::wait();
	}

	if (fc->count() > 0) {
		save_hdr(fc->value());
		logging("Saved the .hdr image : %s\n", fc->value());
	}
}

void
fc_cb(Fl_File_Chooser *fc, void *data)
{
	const char *filename;

	filename = fc->value();
}

void
load_hdr_cb(Fl_Widget *widget, void *data)
{
	int count;

	// Make the .hdr open dialog
	Fl_File_Chooser *fc = new Fl_File_Chooser(
		".", "*.hdr", Fl_File_Chooser::SINGLE,
				  "Open as .hdr file");

	fc->show();

	while (fc->visible()) {
		Fl::wait();
	}

	if (fc->count() > 0) {
		load_hdr(fc->value());
		logging("Loaded .hdr image : %s\n", fc->value());
	}
}


void
update_tm_param_cb(Fl_Widget *widget, void *data)
{
	float m, f, c, a;

	m = gToneMapParamConstant->value();
	f = gToneMapParamIntensity->value();
	c = gToneMapParamChromaAdapt->value();
	a = gToneMapParamLightAdapt->value();

	memcpy(tm_hdrimagebuf, hdrimagebuf, sizeof(float) * image_width * image_height * 3);
	tonemap_reinhard04(tm_hdrimagebuf, f, m, a, c);

	rescale(scaling);

	refresh();
}
           
unsigned char
clamp(float f)
{
	int val;

	val = (int)(f * 255.0f);

	if (val < 0)   val = 0;
	if (val > 255) val = 255;

	return (unsigned char)val;
}

void
rgb2xyz(float rgb[3], float xyz[3])
{
	// obverver = 2 deg, illuminant = D65
	xyz[0] = 0.4124 * rgb[0] + 0.3756 * rgb[1] + 0.1805 * rgb[2];
	xyz[1] = 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2];
	xyz[2] = 0.0193 * rgb[0] + 0.1192 * rgb[1] + 0.9505 * rgb[2];
}

float luminance(float rgb[3])
{
	return 0.2125 * rgb[0] + 0.7154 * rgb[1] + 0.0721 * rgb[2];
}

/*
 * Tone map operator. See,
 *
 * E. Reinhard and K. Devlin
 * Dynamic Range Reduction Inspired by Photoreceptor Physiology
 * In IEEE Transactions on Visualization and Computer Graphics, 2004 
 * http://www.mpi-sb.mpg.de/resources/tmo/reinhard04/
 *
 */
void
tonemap_reinhard04_init()
{
	int i;
	float y;
	float ave;

	y = luminance(&hdrimagebuf[0]);
	Lmax = y;
	Lmin = y;
	ave  = y;

	for (i = 1; i < image_width * image_height; i++) {
		y = luminance(&hdrimagebuf[3 * i]);
		Lmax = (y < Lmax) ? Lmax : y; 
		Lmin = (y > Lmin) ? Lmin : y; 
		ave  += log(2.3e-5+y);
	}

	Lav = exp(ave / (double)(image_width * image_height));
	Llav = log(Lav);

	if (fabs(Lmax - Lmin) < 1.0e-6f) {
		defContrast = 0.3;
	} else {
		defContrast = 0.3 + 0.7 * pow((log(Lmax) - Llav) / (log(Lmax) - log(Lmin)), 1.4);
	}

	printf("lmin = %f, lmax = %f\n", Lmin, Lmax);
	printf("Lav = %f\n", Lav);
	printf("def cont = %f\n", defContrast);
}

void
tonemap_reinhard04(float *img,		/* image is overwritten */
		   double f,		/* paramters		*/
		   double m,		 
		   double a,		 
		   double c)		  
{
	int x, y, i;
	int idx;
	double L;			/* pixel luminance	*/
	double I_a;			/* pixel adaptation	*/
	double I_g, I_l;		/* global and local	*/

	float col;
	float maxcol = 0.0, mincol = 1.0;

	f = exp(-f);
	m = (m > 0.0) ? m : 0.3 + 0.7 * pow((log(Lmax) - Llav) / (log(Lmax) - log(Lmin)), 1.4);

	for (y = 0; y < image_height; y++) {
		for (x = 0; x < image_width; x++) {
			idx = 3 * (y * image_width + x);
			
			L = luminance(&img[idx]);

			for (i = 0; i < 3; i++) {
				col = img[idx + i];

				if (col != 0.0f) {
					I_l = c * col    + (1-c) * L;
					I_g = c * Cav[i] + (1-c) * Lav;
					I_a = a * I_l    + (1-a) * I_g;

					
					col /= col + powf(f * I_a, m);

				}

				if (col > maxcol) maxcol = col;
				if (col < mincol) mincol = col;

				img[idx + i] = col;
			}
		}
	}


	// normalize
	if (fabs(maxcol - mincol) > 1.0e-6f) {
		for (y = 0; y < image_height; y++) {
			for (x = 0; x < image_width; x++) {
				idx = 3 * (y * image_width + x);
				img[idx+0] = (img[idx+0] - mincol) / (maxcol - mincol);
				img[idx+1] = (img[idx+1] - mincol) / (maxcol - mincol);
				img[idx+2] = (img[idx+2] - mincol) / (maxcol - mincol);
			}
		}
	}
}

/* simple linear scaling tone mapping */
void
rescale(float scale)
{
        int i, j;

        for (j = image_height - 1; j >= 0; j--) {
                for (i = 0; i < image_width; i++) {
                        imagebuf[(i + j * image_width) * 3 + 0] =
                                clamp(tm_hdrimagebuf[(j * image_width + i) * 3 + 0] * scale);
                        imagebuf[(i + j * image_width) * 3 + 1] =
                                clamp(tm_hdrimagebuf[(j * image_width + i) * 3 + 1] * scale);
                        imagebuf[(i + j * image_width) * 3 + 2] =
                                clamp(tm_hdrimagebuf[(j * image_width + i) * 3 + 2] * scale);
                }
        }
}

void
init_buffer(int width, int height)
{
	if (hdrimagebuf) free(hdrimagebuf);
	if (tm_hdrimagebuf) free(tm_hdrimagebuf);
	if (imagebuf) free(imagebuf);

        hdrimagebuf = (float *)malloc(sizeof(float) * width * height * 3);
	if (!hdrimagebuf) {
		fprintf(stderr, "can't alloc memory.\n");
		exit(-1);
	}
        tm_hdrimagebuf = (float *)malloc(sizeof(float) * width * height * 3);
	if (!tm_hdrimagebuf) {
		fprintf(stderr, "can't alloc memory.\n");
		exit(-1);
	}
        imagebuf = (unsigned char *)malloc(sizeof(unsigned char) * width * height * 3);
	if (!imagebuf) {
		fprintf(stderr, "can't alloc memory.\n");
		exit(-1);
	}

	memset(hdrimagebuf, 0, sizeof(float) * width * height * 3);
	memset(tm_hdrimagebuf, 0, sizeof(float) * width * height * 3);
	memset(imagebuf, 0, sizeof(unsigned char) * width * height * 3);
}

void
init_display_buffer(int width, int height)
{
	int i;
	if (rgbimagebuf) free(rgbimagebuf);

	rgbimagebuf = (unsigned char *)malloc(sizeof(unsigned char) * width * height * 3);

	for (i = 0; i < width * height; i++) {
		rgbimagebuf[3 * i + 0] = g_bg_col[0];
		rgbimagebuf[3 * i + 1] = g_bg_col[1];
		rgbimagebuf[3 * i + 2] = g_bg_col[2];
	}
}

void
load_hdr(const char *file)
{
        int              i, j;
        int              ret;
        rgbe_header_info info;
        FILE             *fp;

        fp = fopen(file, "rb");
        if (!fp) {
                logging("Can't open file [ %s ].\n", file);
                exit(-1);
        }

        ret = RGBE_ReadHeader(fp, &image_width, &image_height, &info);
        if (ret != RGBE_RETURN_SUCCESS) {
                exit(-1);
        }

	init_buffer(image_width, image_height);

        ret = RGBE_ReadPixels_RLE(fp, hdrimagebuf, image_width, image_height);
        if (ret != RGBE_RETURN_SUCCESS) {
                exit(-1);
        }

	fclose(fp);

	tonemap_reinhard04_init();

	memcpy(tm_hdrimagebuf, hdrimagebuf, sizeof(float) * image_width * image_height * 3);
	tonemap_reinhard04(tm_hdrimagebuf, 0.0, defContrast, 1.0, 0.0);

        for (j = image_height - 1; j >= 0; j--) {
                for (i = 0; i < image_width; i++) {
                        imagebuf[(i + j * image_width) * 3 + 0] =
                                clamp(tm_hdrimagebuf[((image_height - j - 1) * image_width + i) * 
3 + 0]);
                        imagebuf[(i + j * image_width) * 3 + 1] = 
                                clamp(tm_hdrimagebuf[((image_height - j - 1) * image_width + i) * 
3 + 1]);
                        imagebuf[(i + j * image_width) * 3 + 2] = 
                                clamp(tm_hdrimagebuf[((image_height - j - 1) * image_width + i) * 
3 + 2]);
                }
        }

	rescale(scaling);
}

int
keyboard(int key)
{
	int need_update = 0;

        switch (key) {
        case FL_Escape:        /* ESC key */
        case 'q': 
        case 'Q': 
                exit(0);
                break;
        case ' ':        /* space key */
		//offsetx = (int)(0.5 * (window_width - (int)(zoom * window_width)));
		//offsety = (int)(0.5 * (window_height - (int)(zoom * window_height)));

		offsetx = 0; offsety = 0; zoom = 1;
		need_update = 1;
		break;
        case '=':
        case '+':
		if (scaling < 1.0) {
			scaling += 0.1;
		} else {
			scaling += 1.0;
		}

                rescale(scaling);
		need_update = 1;
                break;
        case '-':
                scaling -= 1.0;
                if (scaling < 1.0) scaling = 1.0;
                rescale(scaling);
		need_update = 1;
                break;
        case '0':
                scaling += 0.1;
                rescale(scaling);
		need_update = 1;
                break;
        case '9':
                scaling -= 0.1;
                if (scaling < 0.1) scaling = 0.1;
                rescale(scaling);
		need_update = 1;
                break;
	case 'z':
		zoom++; if (zoom > 8) zoom = 8;
		need_update = 1;
		break;
	case 'x':
		zoom--; if (zoom < 1) zoom = 1;
		need_update = 1;
		break;
	case 'r':
		/* reload image */
		load_hdr(hdrfilename);
		need_update = 1;
		break;

	default:
		break;
        }

	return need_update;
}

int
MyWindow::handle(int event)
{
	int x, y;
	float vals[3];
	char buf[256];

	if (Fl_Window::handle(event)) return 1;

	switch (event) {
	case FL_PUSH:	/* mouse down event */
		if (Fl::event_button() == 1) {	/* left mouse button */
			mouse_left_pressed = 1;
			mousex = Fl::event_x();
			mousey = Fl::event_y();
		}

		redraw();
		return 1;
	break;

	case FL_MOVE:
		movex = Fl::event_x() - margin;
		movey = Fl::event_y() - (margin + menu_heightmargin);
		if (pickup_value(&x, &y, vals)) {
			sprintf(buf, "rockenfield [%d, %d] = (%f, %f, %f)",
				x, y, vals[0], vals[1], vals[2]);
		} else {
			sprintf(buf, "rokenfield (background)");
		}
		gMainWindow->label(buf);

		redraw();

	break;


	case FL_DRAG:
		if (mouse_left_pressed) {
			offsetx += Fl::event_x() - mousex;
			offsety += Fl::event_y() - mousey;

			mousex = Fl::event_x();
			mousey = Fl::event_y();

			//reshape(w(), h());
			display_image_update(offsetx, offsety, zoom);
			gPanel->image()->uncache();
			redraw();
		}

		return 1;
	break;

	case FL_RELEASE:
		mouse_left_pressed = 0;
		return 1;
	break;

	case FL_FOCUS:
		redraw();
		return 1;
	break;

	case FL_UNFOCUS:
		
		return 1;
	break;

	case FL_SHOW:
		Fl_Window::handle(event);
	break;
		

	case FL_KEYBOARD:
		// first check menu's shortcut
		if (gMenuBar->test_shortcut()) {
			return 1;
		}

		if (keyboard(Fl::event_key())) {
			display_image_update(offsetx, offsety, zoom);
			gPanel->image()->uncache();
			redraw();
		}
		return 1;
	break;

	default:
	break;
	}

	return 0;
}

#if 0
int
glwindow::handle(int event)
{

	switch (event) {
	case FL_PUSH:	/* mouse down event */
		if (Fl::event_button() == 1) {	/* left mouse button */
			mouse_left_pressed = 1;
			mousex = Fl::event_x();
			mousey = Fl::event_y();
		}

		//redraw();
		return 1;

	case FL_DRAG:
		if (mouse_left_pressed) {
			offsetx += Fl::event_x() - mousex;
			offsety += mousey - Fl::event_y();

			mousex = Fl::event_x();
			mousey = Fl::event_y();
			reshape(w(), h());
			redraw();
		}

		return 1;

	case FL_RELEASE:
		mouse_left_pressed = 0;
		return 1;
	case FL_FOCUS:
	case FL_UNFOCUS:
		redraw();
		return 1;

	case FL_KEYBOARD:
		// first check menu's shortcut
		if (gMenuBar->test_shortcut()) {
			return 1;
		}
		keyboard(Fl::event_key());
		
		//reshape((int)(width * zoom), (int)(height * zoom));
		redraw();
		return 1;
	default:
		//redraw();
		return Fl_Gl_Window::handle(event);
	}
}
#endif

void
write_pixel(int x, int y, float col[4])
{
	if (x < 0 || x >= image_width) return;
	if (y < 0 || y >= image_height) return;

	hdrimagebuf[3 * (y  * image_width + x) + 0] = col[0];
	hdrimagebuf[3 * (y  * image_width + x) + 1] = col[1];
	hdrimagebuf[3 * (y  * image_width + x) + 2] = col[2];

	imagebuf[(x + y * image_width) * 3 + 0] = clamp(hdrimagebuf[(y * image_width + x) * 3 + 0] * scaling);
	imagebuf[(x + y * image_width) * 3 + 1] = clamp(hdrimagebuf[(y * image_width + x) * 3 + 1] * scaling);
	imagebuf[(x + y * image_width) * 3 + 2] = clamp(hdrimagebuf[(y * image_width + x) * 3 + 2] * scaling);
}

void
parse_command(int comm, MYSOCKET fd)
{
	int        i;
	MYSOCKLEN  len;
	MYSOCKLEN  readlen;
	MYSOCKLEN  reqlen;
	MYSOCKLEN  totallen;
	char      *pixpacketptr;
	imageinfo  info;

	switch (comm) {
		case COMMAND_NEW:
			g_state.comm_new = 1;

			recv(fd, (char *)&len, sizeof(int), FLAGS);
			recv(fd, (char *)&info, len, FLAGS);

			image_width = info.width; image_height = info.height;
			init_buffer(image_width, image_height);

			logging("====================================\n");
			logging("New connection: w = %d, h = %d\n", image_width, image_height);
			g_eop = 0;

			// Raise the main window
			need_raise = 1;

		break;

		case COMMAND_PIXEL:

			if (!g_state.comm_new) {
				logging("Lack of NEW command before PIXEL command.\n");
				exit(-1);
			}

			g_state.comm_pixel = 1;

			recv(fd, (char *)&len, sizeof(int), FLAGS);
			if ((len / sizeof(pixpacket)) > MAXPACKETS) {
				logging("len > MAXPACKETS, len = %d\n", len);
				exit(-1);
			}

			pixpacketptr = (char *)g_packets;

			totallen = 0;
			while (totallen < len) {
				reqlen = len - totallen;
				readlen = recv(fd,
					       pixpacketptr + totallen,
					       reqlen, FLAGS);

				totallen += readlen;
			}


			for (i = 0; i < len / sizeof(pixpacket); i++) {
				write_pixel(g_packets[i].x,
					    g_packets[i].y,
					    g_packets[i].col);
			}

			need_update = 1;
		break;

		case COMMAND_FINISH:
			if (!g_state.comm_pixel) {
				logging("FINISH command without any pixeldata.\n");
			}
			g_state.comm_new    = 0;
			g_state.comm_pixel  = 0;
			g_state.comm_finish = 1;
			g_state.connected   = 0;


			logging("Received finish command.\n");

#ifdef WIN32
			closesocket(g_fd);
#else
			close(g_fd);
#endif

		break;

		default:
		break;
	}
}

static int
check_packet()
{
	int n = 0;
	MYSOCKLEN len;
	struct timeval tm;

	// Set timeout to zero sec. so that select() returns immediately.
	tm.tv_sec = 0;
	tm.tv_usec = 0;

	fd_set fds;
	
	if (!g_state.connected) {
		// there is no connection yet.

		FD_ZERO(&fds);
		FD_SET(g_sockfd, &fds);

		n = select(g_sockfd + 1, &fds, NULL, NULL, &tm);
		if (n == -1) {
			perror("select");
			exit(-1);
		}

		if (FD_ISSET(g_sockfd, &fds)) {
			len = sizeof(g_cl);
			g_fd = accept(g_sockfd, (struct sockaddr *)&g_cl, &len);
			if (g_fd < 0) {
				perror("accept");
				exit(-1);
			}

			g_state.connected = 1;

			return PACKET_NEWCONNECTION;
		}


	} else {

		FD_ZERO(&fds);
		FD_SET(g_fd, &fds);

		n = select(g_fd + 1, &fds, NULL, NULL, &tm);
		if (n == -1) {
			perror("select");
			exit(-1);
		}

		if (FD_ISSET(g_fd, &fds)) {

			return PACKET_ARRIVED;
		}
	}

	return PACKET_NOPACKET;
}

static int
init_connection()
{
	int    i;
	int    n;
	int    comm;
	int    done = 0;
	int    on = 1;
	int    ret;
        struct sockaddr_in addr;
        char   buf[1024];

#ifdef WIN32
	WSADATA ws;
	WORD    wversion = MAKEWORD(2, 0);	// use version 2
	int     err;

	err = WSAStartup(wversion, &ws);
	if (err) {
		printf("WSAStartup() error\n");
		exit(-1);
	}

	if (wversion != ws.wVersion) {
		printf("WinSock version mismatchi error\n");
		exit(-1);
	}
#endif

        g_sockfd = socket(AF_INET, SOCK_STREAM, 0);
 
        memset((void *)&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_port        = htons(g_port);
#ifdef WIN32
        addr.sin_addr.S_un.S_addr = htonl(INADDR_ANY);
#else
        addr.sin_addr.s_addr = htonl(INADDR_ANY);
#endif
 
	/* To know why below setting should be specified,
	 * see Socket F.A.Q on the internet.
	 */
#if defined(WIN32)
	if (setsockopt(g_sockfd, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on)) != 0) {
#else
	if (setsockopt(g_sockfd, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on)) < 0) {
#endif
		printf("muda\n");
		perror("setsockopt");
		exit(2);
	}

#ifdef WIN32
        if (bind(g_sockfd, (struct sockaddr *)&addr, sizeof(addr)) != 0) {
#else
        if (bind(g_sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
#endif
                perror("bind");
                exit(2);
        };

	ret = listen(g_sockfd, 1);
#ifdef WIN32
	if (ret == SOCKET_ERROR) {
		printf("[error] listen() returns SOCKET_ERROR\n");
		printf("[error] err code = %d\n", WSAGetLastError());
		exit(-1);
	}
#else
        if (ret < 0) {
                perror("listen");
                exit(-1);
        }
#endif

	logging("Listening port = %d\n", g_port);
	logging("Init OK\n");

}


#ifdef WIN32
void
#else
void *
#endif
server_drive(void *data)
{
	MYSOCKLEN len;
	int comm;

	while (1) {
		len = sizeof(g_cl);
		if ((g_fd = accept(g_sockfd, (struct sockaddr *)&g_cl, &len)) < 0) {
			perror("accept");
			exit(-1);
		}

		while (1) {
			recv(g_fd, (char *)&comm, sizeof(int), FLAGS); /* get command */
			parse_command(comm, g_fd);

			if (comm == COMMAND_FINISH) {
				break;
			}
		}
	}

	Fl::redraw();
}

void
idle(void *data)
{

#if !defined(ENABLE_THREADING)
	int comm;
	int ret;

	ret = check_packet();

	if (ret == PACKET_ARRIVED ||
	    ret == PACKET_NEWCONNECTION) {
		recv(g_fd, (char *)&comm, sizeof(int), FLAGS);
		parse_command(comm, g_fd);
		
	}

	Fl::add_timeout(0.01, idle);
#else
	Fl::add_timeout(0.5, idle);
#endif

	if (need_raise) {
		gMainWindow->show();

		need_raise = 0;
	}

	if (need_update) {
		display_image_update(offsetx, offsety, zoom);
		gPanel->image()->uncache();
		Fl::redraw();
		Fl::wait();
		need_update = 0;
	}

}

void
helptext()
{
	gHelpTextBuf->append("'x'  Zoom in\n");
	gHelpTextBuf->append("'z'  Zoom out");
}

int
main(int argc, char **argv)
{
	int i;

	init_buffer(image_width, image_height);
	init_display_buffer(window_width, window_height);

	if (argc > 1) {
		for (i = 1; i < argc; i++) {
			if (strcmp("-d", argv[i]) == 0) {
				/* turn on debug print mode */
				g_debugmode = 1;

				i++;
			} else if (strcmp("-port", argv[i]) == 0) {
				if (i + 1 >= argc) {
					fprintf(stderr, "rockenfield: [warning] please specify port number.\n");
					continue;	
				}

				g_port = (unsigned short)atoi(argv[i+1]);	
				if (g_debugmode) {
					printf("rockenfield: port num = %d\n",
						g_port);
				}

				i++;
			} else {
				hdrfilename = argv[i];

				load_hdr(hdrfilename);

				i++;
			}
		}
	}

	// Make the file chooser
	gfc = new Fl_File_Chooser(".", "*", Fl_File_Chooser::CREATE,
				  "File Chooser");
	gfc->callback(fc_cb);

#ifdef WITH_PNG
	// Make the png save dialog
	gpng_save_fc = new Fl_File_Chooser(".", "*.png", Fl_File_Chooser::CREATE,
				  "File Chooser");
	gpng_save_fc->callback(select_png_file_cb);
#endif

	if (image_width  > window_width)  window_width = image_width;
	if (image_height > window_height) window_height = image_height;

	if (window_width > MAX_WINDOW_WIDTH) window_width = MAX_WINDOW_WIDTH;
	if (window_height > MAX_WINDOW_HEIGHT) window_height = MAX_WINDOW_HEIGHT;

	init_display_buffer(window_width, window_height);

	gMainWindow = new MyWindow(
				window_width  + margin * 2,
				window_height + margin * 2 + menu_heightmargin, "rockenfield");
	//gmainwindow->box(FL_NO_BOX);
	gMainWindow->begin();

	gMenuBar = new Fl_Menu_Bar(0, 0, gMainWindow->w(), menu_heightmargin);
	gMenuBar->menu(item);

	gPanel = new Fl_Box(margin, margin + menu_heightmargin, window_width, window_height);
	gPanel->image(new Fl_RGB_Image(rgbimagebuf, window_width, window_height, 3));

	gMainWindow->end();

	gMainWindow->show();


	// Create log window.
	gLogWindow = new Fl_Window(500, 400);
	gLogWindow->label("Log window");
	gLogWindow->begin();

	Fl_Button *buttonClear = new Fl_Button(10, 10, 100, 20, "Clear log.");
	buttonClear->callback(clearlog_cb, 0);

	Fl_Button *buttonClose = new Fl_Button(130, 10, 80, 20, "Close.");
	buttonClose->callback(closelog_cb, 0);

	gTextBuf = new Fl_Text_Buffer();

	gLogText = new Fl_Text_Display(10, 40, 480, 350, "");
	gLogText->tooltip("Output log window.");
	gLogText->buffer(gTextBuf);
	gLogWindow->resizable(gLogText);
	gLogWindow->end();

	// Create help window.
	gHelpWindow = new Fl_Window(500, 400);
	gHelpWindow->label("Help window");
	gHelpWindow->begin();

	Fl_Button *buttonClose2 = new Fl_Button(130, 10, 80, 20, "Close.");
	buttonClose2->callback(closehelp_cb, 0);

	gHelpTextBuf = new Fl_Text_Buffer();
	helptext();

	gHelpText = new Fl_Text_Display(10, 40, 480, 350, "");
	gHelpText->tooltip("Help window.");
	gHelpText->buffer(gHelpTextBuf);
	gHelpWindow->resizable(gHelpText);
	gHelpWindow->end();
	gHelpWindow->hide();

	// Create tone mapping parameter window
	gToneMapParamWindow = new Fl_Window(300, 220);
	gToneMapParamWindow->label("Setting tone map parameters.");
	gToneMapParamWindow->begin();

	gToneMapParamConstant = new Fl_Value_Slider(50, 20, 200, 20, "m : constant");
	gToneMapParamConstant->bounds(0.3, 1.0);
	gToneMapParamConstant->step(0.01);
	gToneMapParamConstant->type(1);
	gToneMapParamConstant->value(0.3);
	gToneMapParamConstant->callback(update_tm_param_cb);

	gToneMapParamIntensity = new Fl_Value_Slider(50, 70, 200, 20, "f' : intensity");
	gToneMapParamIntensity->bounds(-8.0, 8.0);
	gToneMapParamIntensity->step(0.01);
	gToneMapParamIntensity->type(1);
	gToneMapParamIntensity->value(0.0);
	gToneMapParamIntensity->callback(update_tm_param_cb);

	gToneMapParamChromaAdapt = new Fl_Value_Slider(50, 120, 200, 20, "c : chromatic adaptation");
	gToneMapParamChromaAdapt->bounds(0.0, 1.0);
	gToneMapParamChromaAdapt->step(0.01);
	gToneMapParamChromaAdapt->type(1);
	gToneMapParamChromaAdapt->value(0.0);
	gToneMapParamChromaAdapt->callback(update_tm_param_cb);

	gToneMapParamLightAdapt = new Fl_Value_Slider(50, 170, 200, 20, "a : light adaptation");
	gToneMapParamLightAdapt->bounds(0.0, 1.0);
	gToneMapParamLightAdapt->step(0.01);
	gToneMapParamLightAdapt->type(1);
	gToneMapParamLightAdapt->value(1.0);
	gToneMapParamLightAdapt->callback(update_tm_param_cb);
	

	gToneMapParamWindow->end();

#if defined(ENABLE_THREADING)
	logging("Rokenfield starting up(threading version)...\n");
#else
	logging("Rokenfield starting up(select() version)...\n");
#endif

	refresh();

	init_connection();

	logging("Rokenfield started.\n");

	//gMainWindow->size(128, 128);

#ifdef ENABLE_THREADING

#ifdef WIN32
	HANDLE handle;
	DWORD  tid;
	handle = (HANDLE)_beginthread(server_drive, 0, NULL);
#else
	pthread_t tid;
	pthread_create(&tid, NULL, server_drive, NULL);
#endif

	Fl::add_timeout(0.5, idle);

#else	// !ENABLE_THREADING
	
	Fl::add_timeout(0.01, idle);

#endif

	return Fl::run();

#if defined(ENABLE_THREADING)
#ifdef WIN32
	WaitForSingleObject(handle, INFINITE);
	CloseHandle(handle);
#endif
#endif
}
