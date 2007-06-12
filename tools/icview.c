/*
 * Irradiance cache viewer
 *
 * How to comple(Mac OS X, Panther).
 * 
 * $ gcc -framework OpenGL -framework GLUT icview.c trackball.c -lobjc
 * 
 * $Id: icview.c,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
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
static float    scenesize;
static float    maxval;
static float    maxradius = 0.0;

static int      show_trans;
static int      show_rot;
static int      show_normal;

static void reshape(int w, int h);
static void animate();

static GLfloat *icpos;
static GLfloat *icnormal;
static GLfloat *icintensity;
static GLfloat *ictrans;
static GLfloat *icrot;
static GLfloat *icradius;
static unsigned int nsamples;

void
load_pmap(const char *file)
{
	FILE *fp;
	unsigned int i, j;
	double min[3], max[3];
	float  tmp[3];
	
	fp = fopen(file, "r");
	if (!fp) {
		printf("can't find file [ %s ]\n");
		exit(-1);
	}


	fscanf(fp, "%u", &nsamples);

	icpos = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nsamples);
	if (!icpos) exit(-1);
	icnormal = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nsamples);
	if (!icnormal) exit(-1);
	icintensity = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nsamples);
	if (!icintensity) exit(-1);
	ictrans = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nsamples);
	if (!ictrans) exit(-1);
	icrot = (GLfloat *)malloc(sizeof(GLfloat) * 3 * nsamples);
	if (!icrot) exit(-1);
	icradius = (GLfloat *)malloc(sizeof(GLfloat) * nsamples);
	if (!icradius) exit(-1);

	maxval = 0.0;

	printf("number of irradiance samples = [ %d ]\n", nsamples);

	for (i = 0; i < nsamples; i++) {
		fscanf(fp, "%f %f %f",  &(icpos[3 * i + 0]),
				        &(icpos[3 * i + 1]),	
				        &(icpos[3 * i + 2]));
		fscanf(fp, "%f %f %f",  &(icnormal[3 * i + 0]),
				        &(icnormal[3 * i + 1]),	
				        &(icnormal[3 * i + 2]));
		fscanf(fp, "%f %f %f",  &(icintensity[3 * i + 0]),
				        &(icintensity[3 * i + 1]),	
				        &(icintensity[3 * i + 2]));
		fscanf(fp, "%f %f %f",  &(ictrans[3 * i + 0]),
				        &(ictrans[3 * i + 1]),	
				        &(ictrans[3 * i + 2]));
		fscanf(fp, "%f %f %f",  &(icrot[3 * i + 0]),
				        &(icrot[3 * i + 1]),	
				        &(icrot[3 * i + 2]));
		fscanf(fp, "%f",  &(icradius[i]));

		icpos[3 * i + 2] = -icpos[3 * i + 2];
		icnormal[3 * i + 2] = -icnormal[3 * i + 2];
		ictrans[3 * i + 2] = -ictrans[3 * i + 2];
		icrot[3 * i + 2] = -icrot[3 * i + 2];

		if (i == 0) {
			min[0] = max[0] = icpos[3 * i + 0];
			min[1] = max[1] = icpos[3 * i + 1];
			min[2] = max[2] = icpos[3 * i + 2];
		} else {
			for (j = 0; j < 3; j++) {
				if (min[j] > icpos[3 * i + j])
					min[j] = icpos[3 * i + j];

				if (max[j] < icpos[3 * i + j])
					max[j] = icpos[3 * i + j];
			}
		}

		if (maxradius < icradius[i]) maxradius = icradius[i];
	}
	
	fclose(fp);

	scenesize = max[0] - min[0];
	if (max[1] - min[1] > scenesize) scenesize = max[1] - min[1];
	if (max[2] - min[2] > scenesize) scenesize = max[2] - min[2];
	printf("scenesize = %f\n", scenesize);

	if (maxradius < 1.0e-4) maxradius = 1.0;
	printf("maxradius = %f\n", maxradius);

}

void
draw_ic()
{
	unsigned int i;
	float scale = scenesize * 0.02;

	glColor3f(1.0f, 1.0f, 1.0f);
	for (i = 0; i < nsamples; i++) {
		glPointSize((icradius[i] / maxradius) * 5.0);
		//glPointSize(5.0);
		glBegin(GL_POINTS);
		//glColor3f(pcol[3 * i + 0], pcol[3 * i + 1], pcol[3 * i + 2]);
		glVertex3f(icpos[3 * i + 0], icpos[3 * i + 1], icpos[3 * i + 2]);
		glEnd();

		if (show_normal) {
			glBegin(GL_LINES);
				glColor3f(1.0, 1.0, 1.0);
				glVertex3f(icpos[3 * i + 0],
					   icpos[3 * i + 1],
					   icpos[3 * i + 2]);
				glColor3f(0.0, 1.0, 0.0);
				glVertex3f(icpos[3 * i + 0] +
					   icnormal[3 * i + 0] * scale,
					   icpos[3 * i + 1] +
					   icnormal[3 * i + 1] * scale,
					   icpos[3 * i + 2] +
					   icnormal[3 * i + 2] * scale);
			glEnd();
		}

		if (show_trans) {
			glBegin(GL_LINES);
				glColor3f(1.0, 1.0, 1.0);
				glVertex3f(icpos[3 * i + 0],
					   icpos[3 * i + 1],
					   icpos[3 * i + 2]);
				glColor3f(1.0, 0.0, 0.0);
				glVertex3f(icpos[3 * i + 0] +
					   ictrans[3 * i + 0] * scale,
					   icpos[3 * i + 1] +
					   ictrans[3 * i + 1] * scale,
					   icpos[3 * i + 2] +
					   ictrans[3 * i + 2] * scale);
			glEnd();
		}

		if (show_rot) {
			glBegin(GL_LINES);
				glColor3f(1.0, 1.0, 1.0);
				glVertex3f(icpos[3 * i + 0],
					   icpos[3 * i + 1],
					   icpos[3 * i + 2]);
				glColor3f(0.0, 0.0, 1.0);
				glVertex3f(icpos[3 * i + 0] +
					   icrot[3 * i + 0] * scale,
					   icpos[3 * i + 1] +
					   icrot[3 * i + 1] * scale,
					   icpos[3 * i + 2] +
					   icrot[3 * i + 2] * scale);
			glEnd();
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
		  0, 1, 0);	/* Y up */

	glMultMatrixf(&(m[0][0]));

	glScalef(1.0 / scenesize, 1.0 / scenesize, 1.0 / scenesize);
	glTranslatef(-0.5 * scenesize, -0.5 * scenesize, 0.5 * scenesize);

	draw_ic();

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
		view_org[0] = view_org[1] = 0.0; view_org[2] = 3.0;
		view_tgt[0] = view_tgt[1] = view_tgt[2] = 0.0;
		glutPostRedisplay();
		break;
	case 't':
		show_trans = !show_trans;
		glutPostRedisplay();
		break;
	case 'r':
		show_rot = !show_rot;
		glutPostRedisplay();
		break;
	case 'n':
		show_normal = !show_normal;
		glutPostRedisplay();
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
