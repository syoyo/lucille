/*
 * $Id: framebufferdrv.c,v 1.5 2004/06/15 12:58:59 syoyo Exp $
 * OpenGL framebuffer display driver.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#ifdef WIN32
#include <windows.h>
#include <process.h>	/* _begeinthread */
#else 
#include <unistd.h>
#include <pthread.h>
#endif

#ifdef WITH_AQUA
#include <sys/types.h>
#include <sys/wait.h>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#endif

#ifdef WITH_X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#endif

#include <string.h>

#include "memory.h"
#include "framebufferdrv.h"

static char *gbuf;
static char *gmask;
static int   gwidth, gheight;

#ifdef WIN32
static int       gactive = 1;
static HWND      ghwnd  = NULL;
static BITMAPINFO bitmap;
#endif

#ifdef WITH_X11
static int              gactive = 1;
static pthread_t        thread;
static Window           window;
static int              screen;
static int              depth;
static Display         *disp;
static GC               gc;
static XImage          *xim;
static Atom             WM_DELETE_WINDOW;
static Atom             WM_PROTOCOLS;
static void *display_thread(void *data);
static void  window_loop();
static void  redraw_window();
#endif

#ifdef WITH_AQUA
typedef struct _pixel_packet_t
{
	int           x, y;
	unsigned char col[4];
} pixel_packet_t;

#define PIXBUF 16				/* 4x4 pixel tile */
pixel_packet_t       pixpackets[PIXBUF];	/* send PIXBUF pixels at once */
static int           bufcount = 0;

static pid_t         gpid;
static int           gfd[2];
static unsigned long pixcount = 0;

static void  child();
static void  setup_glut();
static void  display();
static void  reshape(int x, int y);
static void  keyboard(unsigned char key, int x, int y);
static void  idle();
static void  update_pixel();
#endif

#ifdef WIN32
LRESULT CALLBACK win32_window_proc(HWND hwnd, UINT message,
				   WPARAM wparam, LPARAM lparam);
static void window_redraw();
static void window_loop();
//static DWORD WINAPI display_thread(void *data);
static void display_thread(void *data);

#endif

#ifdef WIN32
void window_loop()
{
	WNDCLASSEX	wc;
	RECT		clientrect, winrect;
	int		nw, nh;
	MSG		msg;
	HINSTANCE	hinst;

	hinst		= (HINSTANCE)GetModuleHandle(NULL);
	wc.cbSize	= sizeof(WNDCLASSEX);
	wc.style        = CS_HREDRAW | CS_VREDRAW;
	wc.lpfnWndProc  = win32_window_proc;
	wc.cbClsExtra	= 0;
	wc.cbWndExtra	= 0;
	wc.hInstance	= hinst;
	wc.hIcon	= LoadIcon(NULL, IDI_APPLICATION);
	wc.hIconSm	= LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor	= LoadCursor(NULL, IDC_ARROW);
	//wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wc.lpszMenuName = NULL;
	wc.lpszClassName = "framebuffer";

	if (!RegisterClassEx(&wc)) {
		printf("cannot register class\n");
		exit(-1);
	}

	ghwnd = CreateWindow(
		"framebuffer", "framebuffer",
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT,
		0, gwidth, gheight,
		NULL, NULL, hinst, NULL);

	if (!ghwnd) {
		printf("cant create window\n");
		exit(-1);
	}

	ShowWindow(ghwnd, SW_SHOW);
	UpdateWindow(ghwnd);

	GetWindowRect(ghwnd, &winrect);
	GetClientRect(ghwnd, &clientrect);

	nw = gwidth + (winrect.right - winrect.left) -
		      (clientrect.right - clientrect.left);
	nh = gheight + (winrect.bottom - winrect.top) -
		       (clientrect.bottom - clientrect.top);

	SetWindowPos(ghwnd, NULL, 0, 0, nw, nh, SWP_SHOWWINDOW);

	bitmap.bmiHeader.biSize		= sizeof(BITMAPINFOHEADER);
	bitmap.bmiHeader.biWidth	= gwidth;
	bitmap.bmiHeader.biHeight	= gheight;
	bitmap.bmiHeader.biPlanes	= 1;
	bitmap.bmiHeader.biBitCount	= 32;
	bitmap.bmiHeader.biCompression	= BI_RGB;
	bitmap.bmiHeader.biSizeImage	= gwidth * gheight * 4;
	bitmap.bmiHeader.biXPelsPerMeter = 0;
	bitmap.bmiHeader.biYPelsPerMeter = 0;
	bitmap.bmiHeader.biClrUsed	= 0;
	bitmap.bmiHeader.biClrImportant	= 0;

	window_redraw();

	/* message loop */
	while (GetMessage(&msg, NULL, 0, 0)) {
		if (msg.message == WM_PAINT) {
			window_redraw();
		}

		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	gactive = 0;
}

//DWORD WINAPI
void
display_thread(void *data)
{
	window_loop();
}
#endif

#ifdef WITH_X11
void
window_loop()
{
	XSetWindowAttributes	win_attrib;
	unsigned long		win_mask;
	//XSizeHints		win_hints;
	XEvent			event;
	int			running = 1;

	win_attrib.border_pixel      = BlackPixel(disp, screen);
	win_attrib.background_pixel  = BlackPixel(disp, screen);
	win_attrib.override_redirect = 0;
	win_mask		     = CWBackPixel | CWBorderPixel;

	window			     = XCreateWindow(disp,
						     DefaultRootWindow(disp),
						     0, 0, gwidth, gheight,
						     0, depth,
						     InputOutput,
						     CopyFromParent,
						     win_mask,
						     &win_attrib); 

	xim = XCreateImage(disp, CopyFromParent, depth, ZPixmap, 0,
			   (char *)gbuf, gwidth, gheight, 32, gwidth * 4);
	
	gc  = XCreateGC(disp, window, 0, 0);

	XMapWindow(disp, window);
	XChangeProperty(disp, window, WM_PROTOCOLS, XA_ATOM, 32, 0,
			(unsigned char *)&WM_DELETE_WINDOW, 1);
	XSelectInput(disp, window, ExposureMask | StructureNotifyMask);
 
	gactive = 1;

	while (running) {
		XNextEvent(disp, &event);
		switch (event.type) {
			case Expose:
				XPutImage(disp, window, gc, xim,
					  0, 0, 0, 0,
					  gwidth, gheight);
				XFlush(disp);

				break;

			case KeyPress:
				if (XLookupKeysym(&event.xkey, 0)
							== XK_Escape) {
					running = 0;
				}

				break;

			case DestroyNotify:
				running = 0;
				break;

			case ClientMessage:
				if ((Atom)(event.xclient.data.l[0])
							== WM_DELETE_WINDOW) {
					running = 0;
				}

				break;
		}
	}

	XUnmapWindow(disp, window);
	XFreeGC(disp, gc);

    /* Note that XDestroyImage() frees image data set by XCreateImage(). */
	XDestroyImage(xim);
	XDestroyWindow(disp, window);
	XCloseDisplay(disp);
}

void *
display_thread(void *data)
{
	window_loop();

	return NULL;
}
#endif

#ifdef WIN32
static void
window_redraw()
{
	HDC  hdc;

	hdc = GetDC(ghwnd);
	
	SetStretchBltMode(hdc, COLORONCOLOR);
	StretchDIBits(hdc,
		      0, 0, gwidth, gheight,
		      0, 0, gwidth, gheight,
		      gbuf, &bitmap, DIB_RGB_COLORS, SRCCOPY);

}

#endif


int
fb_dd_open(const char *name, int width, int height,
	   int bits, RtToken component, const char *format)
{
	int i;

	(void)name;

	if (strcmp(format, "byte") != 0) return 0;
	
	if (bits != 8) return 0;

	if (strcmp(component, RI_RGB) != 0) return 0;

	gwidth     = width;	
	gheight    = height;

#ifdef WIN32
	gbuf  = (char *)ri_mem_alloc(width * height * 4);
	gmask = (char *)ri_mem_alloc(width * height);
	for (i = 0; i < width * height; i++) {
		gbuf[4 * i + 0] = 0; 
		gbuf[4 * i + 1] = 152;
		gbuf[4 * i + 2] = 127;
		gbuf[4 * i + 3] = 127;
		gmask[i] = 1;	/* first time of writing to this pixel. */
	}

	gactive = 1;
	_beginthread(display_thread, 0, NULL);
#endif

#ifdef WITH_X11
	gbuf = (char *)ri_mem_alloc(width * height * 4);
	gmask = (char *)ri_mem_alloc(width * height);
	for (i = 0; i < width * height; i++) {
		gbuf[4 * i + 0] = 127;
		gbuf[4 * i + 1] = 127;
		gbuf[4 * i + 2] = 127;
		gbuf[4 * i + 3] = 127;
		gmask[i] = 1;	/* first time of writing to this pixel. */
	}

	XInitThreads();
	disp			= XOpenDisplay(NULL);
	if (!disp) {
		printf("Can't open display\n");
		exit(-1);
	}

	WM_DELETE_WINDOW	= XInternAtom(disp, "WM_DELETE_WINDOW", 0);
	WM_PROTOCOLS		= XInternAtom(disp, "WM_PROTOCOLS", 0);
	screen			= DefaultScreen(disp);
	depth			= DefaultDepth(disp, screen);

	if (depth != 24 && depth != 32) {
		printf("needs true color display: current depth = %d\n", depth);
		exit(-1);
	}

	pthread_create(&thread, NULL, display_thread, NULL);
#endif

#ifdef WITH_AQUA
	//pthread_create(&thread, NULL, display_thread, NULL);

	if (pipe(gfd) == -1) {
		perror("pipe");
		exit(-1);
	}

	if ((gpid = fork()) == 0) {

		/* child process */
		child();

	} else if (gpid > 0) {

		/* parent process */
		close(gfd[0]);

		gbuf = (char *)ri_mem_alloc(width * height * 4);
		gmask = (char *)ri_mem_alloc(width * height);
		for (i = 0; i < width * height; i++) {
			gbuf[4 * i + 3] = 0;
			gbuf[4 * i + 2] = 152;
			gbuf[4 * i + 1] = 127;
			gbuf[4 * i + 0] = 127;
			gmask[i] = 1;
		}


	} else {

		perror("fork");
		exit(-1);

	}
#endif

#if !defined(WITH_AQUA) && !defined(WITH_X11) && !defined(WIN32)
	/* compiled with --disable-framebuffer */
	return 0;
#else
	return 1;
#endif
}

int
fb_dd_write(int x, int y, const void *pixel)
{
#ifdef WIN32
	static int ndrawed = 0;
	       int progress_tick = gwidth;     

	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	//memcpy(&(gbuf[3 * (x + (gheight - y - 1) * gwidth)]), pixel, 3);
	/* color order in DIB is BGR */
	if (gmask[x + (gheight - y - 1) * gwidth]) {
		/* first time. */
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 0] =
			((char *)pixel)[2];
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 1] =
			((char *)pixel)[1];
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 2] =
			((char *)pixel)[0];
		gmask[x + (gheight - y - 1) * gwidth] = 0;
	} else {
		/* additive */
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 0] +=
			((char *)pixel)[2];
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 1] +=
			((char *)pixel)[1];
		gbuf[4 * (x + (gheight - y - 1) * gwidth) + 2] +=
			((char *)pixel)[0];
	}

	ndrawed++;
	if (gactive) {
		if (ndrawed >= progress_tick) {
			ndrawed = 0;
			PostMessage(ghwnd, WM_PAINT, 0, 0);
		}
	}
#endif

#ifdef WITH_X11
	static int ndrawed = 0;
	       int progress_tick = gwidth;     

    int yy = (gheight - y - 1);

	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	if (gmask[x + yy * gwidth]) {
		/* first time */
		gbuf[4 * (x + yy * gwidth) + 3] = 255; /* A */
		gbuf[4 * (x + yy * gwidth) + 2] = ((unsigned char *)pixel)[0];    /* R */
		gbuf[4 * (x + yy * gwidth) + 1] = ((unsigned char *)pixel)[1]; /* G */
		gbuf[4 * (x + yy * gwidth) + 0] = ((unsigned char *)pixel)[2]; /* B */
		gmask[x + yy * gwidth] = 0;
	} else {
		/* additive */
		gbuf[4 * (x + yy * gwidth) + 2] += ((unsigned char *)pixel)[0];    /* R */
		gbuf[4 * (x + yy * gwidth) + 1] += ((unsigned char *)pixel)[1]; /* G */
		gbuf[4 * (x + yy * gwidth) + 0] += ((unsigned char *)pixel)[2]; /* B */
	}

	ndrawed++;
	if (ndrawed >= progress_tick) {
		ndrawed = 0;
		if (gactive) {
			XPutImage(disp, window, gc, xim, 0, 0, 0, 0,
				  gwidth, gheight);
			XFlush(disp);
		}
	}
#endif

#ifdef WITH_AQUA
	//pixel_packet_t pixpacket;
	int packetsize;

	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	pixpackets[bufcount].x = x;
	pixpackets[bufcount].y = y;
	pixpackets[bufcount].col[0] = ((unsigned char *)pixel)[0];
	pixpackets[bufcount].col[1] = ((unsigned char *)pixel)[1];
	pixpackets[bufcount].col[2] = ((unsigned char *)pixel)[2];
	pixpackets[bufcount].col[3] = 255;
	bufcount++;

	/* buffer pixel data, and if buffer is full, then send buffered pixel
	 * data for IPC performance.
	 */
	if (bufcount >= PIXBUF) {
		packetsize = PIXBUF;

		/* send packet data size */
		write(gfd[1], (void *)&packetsize, sizeof(int));

		/* send pixel contents */
		write(gfd[1],
		      (void *)&pixpackets,
		      sizeof(pixel_packet_t) * PIXBUF);

		bufcount = 0;
	}
 

#endif

	return 1;
}

int
fb_dd_close(void)
{
#ifdef WIN32
	window_redraw();
	
	//WaitForSingleObject(thread, INFINITE);
#endif	/* WITH_OPENGL	*/



#ifdef WITH_X11
	XPutImage(disp, window, gc, xim, 0, 0, 0, 0, gwidth, gheight);
	XFlush(disp);

	pthread_join(thread, NULL);

    /* gbuf is already freed by XDestroyImage().
     * No ri_mem_free(gbuf) is required.
     */

#endif

#ifdef WITH_AQUA
	int packetsize;

	//display(aglctx);

	//pthread_join(thread, NULL);

	if (bufcount != 0) {
		/* send reminder */

		packetsize = bufcount;

		/* send packet data size */
		write(gfd[1], (void *)&packetsize, sizeof(int));

		/* send pixel contents */
		write(gfd[1],
		      (void *)&pixpackets,
		      sizeof(pixel_packet_t) * packetsize);

	}

	wait((int *)0);

	ri_mem_free(gbuf);

#endif

	ri_mem_free(gmask);

	return 1;
}

int
fb_dd_progress(void)
{
	return 1;
}

/* --- private functions --- */

#ifdef WIN32
LRESULT CALLBACK
win32_window_proc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam)
{

	switch (message) {
		case WM_DESTROY:
			//PostQuitMessage(0);
			exit(0);
			break;

		case WM_KEYDOWN:
			switch (wparam) {
				case VK_ESCAPE:
					//PostQuitMessage(0);
					exit(0);
					break;
			}		
			break;

		default:
			return DefWindowProc(hwnd, message, wparam, lparam);
	}	

	return 0;
}

#endif	/* WIN32	*/


#ifdef WITH_AQUA
static void
child()
{
	int i;

	dup2(gfd[0], 0);
	close(gfd[1]);

	gbuf = (char *)ri_mem_alloc(gwidth * gheight * 4);
	gmask = (char *)ri_mem_alloc(gwidth * gheight);
	for (i = 0; i < gwidth * gheight; i++) {
		gbuf[4 * i + 3] = 0;
		gbuf[4 * i + 2] = 152;
		gbuf[4 * i + 1] = 127;
		gbuf[4 * i + 0] = 127;
		gmask[i] = 1;
	}

	setup_glut();
}

static void
display()
{
	glClearColor(0.3, 0.5, 0.85, 1.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glRasterPos2i(0, 0);
	glDrawPixels(gwidth, gheight, GL_RGBA, GL_UNSIGNED_BYTE, gbuf);

	glutSwapBuffers();
}

void
reshape(int w, int h)
{
	glViewport(0, 0, w, h);	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, w, 0, h);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
}

void
keyboard(unsigned char k, int x, int y)
{
	(void)x;
	(void)y;

	switch (k) {
	case 27:
		exit(-1);
		break;
	case '1':
		printf("1\n");
		break;
	}
}

static void
update_pixel()
{
	static int     ndrawed = 0;
	int            npixels;
	int            progress_tick = gwidth * 2; 
	int            i;
	int            x, y;
	pixel_packet_t pixpacket[PIXBUF];


	/* first read how many pixels to be recieved */
	read(gfd[0], &npixels, sizeof(int));
	assert(npixels <= PIXBUF);

	/* then read pixel content */
	read(gfd[0], &pixpacket, sizeof(pixel_packet_t) * npixels);

	
	for (i = 0; i < npixels; i++) {
		x = pixpacket[i].x;
		y = pixpacket[i].y;

		/* RGBA pixel order, bottom to top scan order */
		//if (gmask[x + (gheight - y - 1) * gwidth]) {
		if (gmask[x + y * gwidth]) {
	#if 0
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 0] = pixpacket.col[0];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 1] = pixpacket.col[1];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 2] = pixpacket.col[2];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 3] = pixpacket.col[3];
			gmask[x + (gheight - y - 1)*gwidth] = 0;
	#endif
			gbuf[4*(x + y*gwidth) + 0] = pixpacket[i].col[0];
			gbuf[4*(x + y*gwidth) + 1] = pixpacket[i].col[1];
			gbuf[4*(x + y*gwidth) + 2] = pixpacket[i].col[2];
			gbuf[4*(x + y*gwidth) + 3] = pixpacket[i].col[3];
			gmask[x + y*gwidth] = 0;
			pixcount++;
		} else {
			/* additive */
	#if 0
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 0] += pixpacket.col[0];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 1] += pixpacket.col[1];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 2] += pixpacket.col[2];
			gbuf[4*(x + (gheight - y - 1)*gwidth) + 3] += pixpacket.col[3];
	#endif
			gbuf[4*(x + y*gwidth) + 0] += pixpacket[i].col[0];
			gbuf[4*(x + y*gwidth) + 1] += pixpacket[i].col[1];
			gbuf[4*(x + y*gwidth) + 2] += pixpacket[i].col[2];
			gbuf[4*(x + y*gwidth) + 3] += pixpacket[i].col[3];
		}
	}

	ndrawed += npixels;
	if (ndrawed >= progress_tick) {
		ndrawed = 0;
		glutPostRedisplay();
	}
}

static void
idle()
{
	if (pixcount < (unsigned long)(gwidth * gheight)) update_pixel();
}

static void
setup_glut()
{
	int argc = 1;
	char *argv[1] = {"framebuffer"};

	printf("width, height = %d, %d\n", gwidth, gheight);
	glutInit(&argc, argv);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(gwidth, gheight);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH);
	glutCreateWindow(argv[0]);	
	glutSetWindowTitle(argv[0]);	
	glutSetIconTitle(argv[0]);	

	glutReshapeFunc(reshape);
	glutDisplayFunc(display);
	glutKeyboardFunc(keyboard);
	glutIdleFunc(idle);
	
	glDisable(GL_DEPTH_TEST);

	glutMainLoop();
}
#endif
