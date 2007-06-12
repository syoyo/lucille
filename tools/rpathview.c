/*
 * Simple ray path viewer for renderer debugging.
 * Dumped data are currently generated from photon tracing stage.
 * (see photomap.c)
 *
 * Compile(Mac OS X 10.3)
 *
 * gcc pmapview.c trackball.c -framework OpenGL -framework GLUT -lobjc
 *
 * 
 * $Id: rpathview.c,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
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

static void reshape(int w, int h);
static void animate();

#define MAX_RAYS 100000
#define MAX_PATHS 10

static GLfloat rpos[MAX_RAYS][MAX_PATHS * 3];
static unsigned int nrays;
static unsigned int npaths[MAX_RAYS];
static char rtype[MAX_RAYS];
static float scale;
static int gmode;
static int drawdepth;
static int drawall;
static int drawray;

void
load_rpath(const char *file)
{
	FILE *fp;
	int i;
	float minval, maxval;
	
	
	fp = fopen(file, "r");
	if (!fp) {
		printf("can't find file [ %s ]\n");
		exit(-1);
	}

	minval = 1.0e+6;
	maxval = 1.0e-6;

	nrays = 0;

	fscanf(fp, "%d", &npaths[nrays]);
	while (!feof(fp)) {

		fscanf(fp, " %c ", &rtype[nrays]);

		for (i = 0; i < npaths[nrays]; i++) {
			fscanf(fp, "%f %f %f ", &rpos[nrays][3 * i + 0],
					        &rpos[nrays][3 * i + 1],
					        &rpos[nrays][3 * i + 2]);

			if (rpos[nrays][3 * i + 0] > maxval) 
				maxval = rpos[nrays][3 * i + 0];
			if (rpos[nrays][3 * i + 1] > maxval) 
				maxval = rpos[nrays][3 * i + 1];
			if (rpos[nrays][3 * i + 2] > maxval) 
				maxval = rpos[nrays][3 * i + 2];
			if (rpos[nrays][3 * i + 0] < minval)
				minval = rpos[nrays][3 * i + 0];
			if (rpos[nrays][3 * i + 1] < minval)
				minval = rpos[nrays][3 * i + 1];
			if (rpos[nrays][3 * i + 2] < minval)
				minval = rpos[nrays][3 * i + 2];
		}	

		nrays++;
		if (nrays >= MAX_RAYS) break;
	
		fscanf(fp, "%d", &npaths[nrays]);
	}
	
	fclose(fp);

	scale = maxval - minval;
	printf("scale = %f\n", scale);
}

void
draw_raypath()
{
	unsigned int i, j;
	GLfloat col[4];

	glPointSize(4.0);
	//glColor3f(1.0f, 1.0f, 1.0f);
	//glLineWidth(1.4);

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	for (i = 0; i < nrays; i++) {
		if (drawray > 0 && drawray != i) continue;

		if (gmode && rtype[i] != 'S') continue;

		col[3] = 0.25;
		if (rtype[i] == 'S') {
			col[0] = col[1] = 0.3; col[2] = 1.0;
		} else if (rtype[i] == 'D') {
			col[0] = 0.2; col[1] = 1.0; col[2] = 0.1;
		} else if (rtype[i] == 'E') {
			col[0] = 1.0; col[1] = 0.2; col[2] = 1.0;
		}
			
		glColor4fv(col);

		if (npaths[i] < 2) {
			glBegin(GL_POINTS);
			glVertex3f(rpos[i][0], rpos[i][1], rpos[i][2]);
			glEnd();
		} else {
			if (drawdepth == 0) {
				glColor4f(1.0f, 1.0f, 1.0f, 0.25f);
			} else {
				glColor4fv(col);
			}	
			glBegin(GL_LINES);
			glVertex3f(rpos[i][0], rpos[i][1], rpos[i][2]);
			glVertex3f(rpos[i][3], rpos[i][4], rpos[i][5]);
			glEnd();

			if (drawdepth == 0) {
				glColor4f(1.0, 1.0, 0.0, 0.25);
				glBegin(GL_POINTS);
				glVertex3f(rpos[i][3], rpos[i][4], rpos[i][5]);
				glEnd();
			}

			for (j = 1; j < npaths[i]-1; j++) {
				if (drawdepth == j) {
					glColor4f(1.0f, 1.0f, 1.0f, 0.25f);
				} else {
					glColor4fv(col);
				}

				glBegin(GL_LINES);
				glVertex3f(rpos[i][3*j+0], rpos[i][3*j+1], rpos[i][3*j+2]);
				glVertex3f(rpos[i][3*(j+1)+0], rpos[i][3*(j+1)+1], rpos[i][3*(j+1)+2]);
				glEnd();

				if (drawdepth == j) {
					glColor4f(1.0, 1.0, 0.0, 0.25);
					glBegin(GL_POINTS);
					glVertex3f(rpos[i][3*(j+1)+0], rpos[i][3*(j+1)+1], rpos[i][3*(j+1)+2]);
					glEnd();
				}
			}
		}
	}
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
		  0, 1, 0);	/* Z up */

	glMultMatrixf(&(m[0][0]));

	glScalef(1.0 / scale, 1.0 / scale, 1.0 / scale);

	draw_raypath();

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
		//add_quats(prev_quat, curr_quat, curr_quat);
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
			add_quats(prev_quat, curr_quat, curr_quat);
		}

		mouse_x = x;
		mouse_y = y;
		spinning = 1;
	
		//glutIdleFunc(animate);
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
		view_org[0] = view_org[1] = 0.0; view_org[2] = 3.0;
		view_tgt[0] = view_tgt[1] = view_tgt[2] = 0.0;
		break;
	case 'g':
		gmode = !gmode;
		break;
	case '+':
		drawdepth++; if (drawdepth >= 10) drawdepth = 0;
		drawall = 0;
		printf("depth = %d\n", drawdepth);
		break;
	case '-':
		drawdepth--; if (drawdepth < 0) drawdepth = 0;
		drawall = 0;
		printf("depth = %d\n", drawdepth);
		break;
	case 'a':
		drawall = !drawall;
		break;
	case 'n':
		if (drawray < 0) drawray = 0;
		drawray++;
		if (drawray >= nrays) drawray = nrays;
		break;
	case 'f':
		drawray--;
		if (drawray < 0) drawray = 0;
		break;
	case 's':
		drawray = -1;
		break;
	default:
		break;
	}

	glutPostRedisplay();
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
	gmode         = 0;
	drawall       = 1;
	drawdepth     = 0;
	drawray       = -1;

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

	load_rpath(argv[1]);

	glutReshapeFunc(reshape);
	glutDisplayFunc(display);
	glutKeyboardFunc(keyboard);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	//glutIdleFunc(animate);

	glutMainLoop();

	return 0;
}
