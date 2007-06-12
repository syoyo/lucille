#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#if defined(__APPLE__) && defined(__MACH__)
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <stdio.h>
#include <stdlib.h>

#include "rgbe.h"

static int	 width, height;
static GLubyte	*image;
static float    *hdrimage;
static float     scaling = 1.0;

GLubyte
clamp(float f)
{
	int val;

	val = (int)(f * 255.0f + 0.5f);

	if (val < 0)   val = 0;
	if (val > 255) val = 255;

	return (GLubyte)val;
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
		fprintf(stderr, "Can't open file [ %s ].\n", file);
		exit(-1);
	}

	ret = RGBE_ReadHeader(fp, &width, &height, &info);
	if (ret != RGBE_RETURN_SUCCESS) {
		exit(-1);
	}
	printf("width x height = %d x %d\n", width, height);

	hdrimage = (float *)malloc(sizeof(float) * width * height * 3);
	image = (GLubyte *)malloc(sizeof(GLubyte) * width * height * 3);

	ret = RGBE_ReadPixels_RLE(fp, hdrimage, width, height);
	if (ret != RGBE_RETURN_SUCCESS) {
		exit(-1);
	}

	for (j = height - 1; j >= 0; j--) {
		for (i = 0; i < width; i++) {
			image[(i + j * width) * 3 + 0] =
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 0]);
			image[(i + j * width) * 3 + 1] = 
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 1]);
			image[(i + j * width) * 3 + 2] = 
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 2]);
		}
	}
}

void
rescale(float scale)
{
	int i, j;

	for (j = height - 1; j >= 0; j--) {
		for (i = 0; i < width; i++) {
			image[(i + j * width) * 3 + 0] =
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 0] * scale);
			image[(i + j * width) * 3 + 1] = 
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 1] * scale);
			image[(i + j * width) * 3 + 2] = 
				clamp(hdrimage[((height - j - 1) * width + i) * 3 + 2] * scale);
		}
	}
}

void
display()
{
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glDisable(GL_DEPTH_TEST);

	glRasterPos2i(0.0, 0.0);
	glDrawPixels(width, height, GL_RGB, GL_UNSIGNED_BYTE, image);

	glutSwapBuffers();
}

void
key(unsigned char key, int x, int y)
{
	switch (key) {
	case 27:	/* ESC key */
		exit(0);
	case '+':
		scaling += 1.0;
		rescale(scaling);
		break;
	case '-':
		scaling -= 1.0;
		if (scaling < 0.0) scaling = 0.0;
		rescale(scaling);
		break;
	case '0':
		scaling += 0.1;
		rescale(scaling);
		break;
	case '9':
		scaling -= 0.1;
		if (scaling < 0.0) scaling = 0.0;
		rescale(scaling);
		break;
	}

	glutPostRedisplay();
}

void
reshape(int w, int h)
{
	glViewport(0, 0, (GLint)w, (GLint)h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0.0, 1.0, 0.0, 1.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
}

int
main(int argc, char **argv)
{
	if (argc < 2) {
		printf("Usage: %s filename.hdr\n", argv[0]);
		exit(-1);
	}

	load_hdr(argv[1]);

	glutInit(&argc, argv);
	glutInitWindowPosition(0, 0);
	glutInitWindowSize(width, height);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
	glutCreateWindow(argv[0]);
	glutKeyboardFunc(key);
	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutMainLoop();
}
