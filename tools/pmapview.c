/*
 * Simple photomap viewer for renderer debugging.
 *
 * Compile(Mac OS X 10.3)
 *
 * gcc pmapview.c trackball.c -framework OpenGL -framework GLUT -lobjc
 *
 *
 * $Id: pmapview.c,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__APPLE__) && defined(__MACH__)
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include <assert.h>

#include "trackball.h"

#define WINDOW_WIDTH	800
#define WINDOW_HEIGHT	600

/* 
 * global variable 
 */
static int	mouse_x, mouse_y;	/* x, y location		*/
static float	curr_quat[4];		/* current quaternion state	*/
static float	prev_quat[4];		/* previous quaternion state	*/
static int	mouse_moving;		/* Is mouse moving?		*/
static int	spinning;		/* Is scene spinning?		*/
static int	width, height;		/* current window width&height	*/
static float    zoom;			/* zoom factor			*/
static int	mouse_r_pressed;	/* Is right mouse pressed?	*/
static int	mouse_m_pressed;	/* Is middle mouse pressed?	*/
static GLfloat  view_org[3];		/* view origin			*/
static GLfloat  view_tgt[3];		/* view target			*/
static float    bmin[3], bmax[3];
static float    center[3];
static float    maxval;
static float    scenesize;

static void reshape(int w, int h);
static void animate();

static GLfloat *ppos;
static GLfloat *pcol;
static unsigned int nphotons;

void
load_pmap(const char *file)
{
	FILE *fp;
	unsigned int i;
	
	fp = fopen(file, "r");
	if (!fp) {
		printf("can't find file [ %s ]\n");
		exit(-1);
	}


	fscanf(fp, "%u", &nphotons);
	fscanf(fp, "%f %f %f", &bmin[0], &bmin[1], &bmin[2]);
	fscanf(fp, "%f %f %f", &bmax[0], &bmax[1], &bmax[2]);

	center[0] = (bmax[0] - bmin[0]) / 2.0;
	center[1] = (bmax[1] - bmin[1]) / 2.0;
	center[2] = (bmax[2] - bmin[2]) / 2.0;

	scenesize = 0;
	scenesize = center[0];
	if (scenesize < center[1]) scenesize = center[1];
	if (scenesize < center[2]) scenesize = center[2];

	printf("scenesize = %f\n", scenesize);

	ppos = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nphotons);
	if (!ppos) exit(-1);
	pcol = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nphotons);
	if (!pcol) exit(-1);

	maxval = 0.0;

	for (i = 0; i < nphotons; i++) {
		fscanf(fp, "%f %f %f %f %f %f",
					&(ppos[3 * i + 0]),
				        &(ppos[3 * i + 1]),	
				        &(ppos[3 * i + 2]),
				        &(pcol[3 * i + 0]),
				        &(pcol[3 * i + 1]),
				        &(pcol[3 * i + 2]));	

		if (pcol[3 * i + 0] > maxval) maxval = pcol[3 * i + 0];
		if (pcol[3 * i + 1] > maxval) maxval = pcol[3 * i + 1];
		if (pcol[3 * i + 2] > maxval) maxval = pcol[3 * i + 2];

		ppos[3 * i + 0] -= center[0];
		ppos[3 * i + 1] -= center[1];
		ppos[3 * i + 2] -= center[2];
	}
	
	fclose(fp);

	for (i = 0; i < nphotons; i++) {
		pcol[3 * i + 0] /= maxval;
		pcol[3 * i + 1] /= maxval;
		pcol[3 * i + 2] /= maxval;
	}
}

void
draw_pmap()
{
	unsigned int i;

	glPointSize(2.0);
	glBegin(GL_POINTS);
	for (i = 0; i < nphotons; i++) {
		glColor3f(pcol[3 * i + 0], pcol[3 * i + 1], pcol[3 * i + 2]);
		/* -Z because OpenGL is right-handed. */
		glVertex3f(ppos[3 * i + 0], ppos[3 * i + 1], -ppos[3 * i + 2]);
	}
	glEnd();
}


void
build_rot_matrix(GLfloat m[4][4])
{
	/* get rotation matrix */
	build_rotmatrix(m, curr_quat);
}

void
display()
{
	GLfloat  m[4][4];

	glClearColor(0.0, 0.0, 0.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	/* get rotation matrix */
	build_rot_matrix(m);

	//set_orthoview_pass(width, height);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	gluLookAt(view_org[0], view_org[1], view_org[2],
		  view_tgt[0], view_tgt[1], view_tgt[2],
		  0, 1, 0);	/* Y up */

	glMultMatrixf(&(m[0][0]));

	// draw scene bounding box
	glPushMatrix();
	glColor3f(1.0, 1.0, 1.0);
	glScalef(center[0], center[1], center[2]);
	glutWireCube(2.0 / scenesize);
	glPopMatrix();
	
	glScalef(1.0 / scenesize, 1.0 / scenesize, 1.0 / scenesize);

	draw_pmap();

	glutSwapBuffers();
}

void
reshape(int w, int h)
{
	glViewport(0, 0, w, h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.0f, (float)w / (float)h, 0.1f, 50.0f);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	width  = w; 
	height = h;

}

void
animate()
{
	if (spinning) {
		add_quats(prev_quat, curr_quat, curr_quat);
	}

	glutPostRedisplay();
}

void
mouse(int button, int state, int x, int y)
{
	if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
		trackball(prev_quat, 0, 0, 0, 0);	/* initialize */
		glutIdleFunc(NULL);
		mouse_moving = 1;
		mouse_x = x;
		mouse_y = y;
		spinning = 0;
	}

	if (button == GLUT_LEFT_BUTTON && state == GLUT_UP) {
		mouse_moving = 0;
	}

	if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
		mouse_r_pressed = 1;
		mouse_m_pressed = 0;
		mouse_moving = 1;
		mouse_x = x;
		mouse_y = y;
		spinning = 0;
	}

	if (button == GLUT_RIGHT_BUTTON && state == GLUT_UP) {
		mouse_r_pressed = 0;
		mouse_m_pressed = 0;
		mouse_moving = 0;
	}

	if (button == GLUT_MIDDLE_BUTTON && state == GLUT_DOWN) {
		mouse_m_pressed = 1;
		mouse_r_pressed = 0;
		mouse_moving = 1;
		mouse_x = x;
		mouse_y = y;
		spinning = 0;
	}

	if (button == GLUT_MIDDLE_BUTTON && state == GLUT_UP) {
		mouse_m_pressed = 0;
		mouse_r_pressed = 0;
		mouse_moving = 0;
	}

	glutPostRedisplay();
}

void
motion(int x, int y)
{
	float w = 1;
	float tmp[4];

	if (mouse_moving) {
		if (mouse_r_pressed) {
			view_org[2] += 4.0*((float)y - mouse_y) / (float)height;
		} else if (mouse_m_pressed) {
			view_org[0] += 6.0 * zoom * (mouse_x - (float)x) / (float)width;
			view_org[1] += 6.0 * zoom * ((float)y - mouse_y) / (float)height;
			view_tgt[0] += 6.0 * zoom * (mouse_x - (float)x) / (float)width;
			view_tgt[1] += 6.0 * zoom * ((float)y - mouse_y) / (float)height;
		} else {
			trackball(prev_quat, 
				  w * (2.0 * mouse_x - width)  / width,
				  w * (height - 2.0 * mouse_y) / height,
				  w * (2.0 * x - width)  / width,
				  w * (height - 2.0 * y) / height);
		}

		mouse_x = x;
		mouse_y = y;
		spinning = 1;
	
		glutIdleFunc(animate);
	}

	glutPostRedisplay();
}

void
keyboard(unsigned char k, int x, int y)
{
	static int presetnum = 0;

	switch(k) {
	case 27:	/* ESC */
	case 'q':
		exit(0);
		break;
	case ' ':	/* space */
		/* reset view */
		trackball(curr_quat, 0.0, 0.0, 0.0, 0.0);
		mouse_moving = 0;
		spinning     = 0;
		view_org[0] = view_org[1] = 0.0; view_org[2] = 5.0;
		view_tgt[0] = view_tgt[1] = view_tgt[2] = 0.0;
		break;
	default:
		break;
	}
}

void
init()
{
	/*
	 * global variable initialization
	 */
	width         = WINDOW_WIDTH;
	height        = WINDOW_HEIGHT;
	mouse_moving  = 0;
	spinning      = 0;
	zoom          = 1;

	view_org[0] = view_org[1] = 0.0; view_org[2] = 3.0;
	view_tgt[0] = view_tgt[1] = view_tgt[2] = 0.0;

	trackball(curr_quat, 0.0, 0.0, 0.0, 0.0);
	
	glDisable(GL_DEPTH_TEST);
}

int
main(int argc, char **argv)
{
	if (argc < 2) {
		printf("usage: pmapview file\n");

		exit(-1);
	}

	glutInit(&argc, argv);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutCreateWindow(argv[0]);

	init();

	load_pmap(argv[1]);

	glutReshapeFunc(reshape);
	glutDisplayFunc(display);
	glutKeyboardFunc(keyboard);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	glutIdleFunc(animate);

	glutMainLoop();

	return 0;
}
