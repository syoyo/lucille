/*
 * Simple bsp viewer for renderer debugging.
 *
 * Compile(Mac OS X 10.3)
 *
 * gcc bspview.c trackball.c -framework OpenGL -framework GLUT -lobjc
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
static float    scenesize = 0.0;

static void reshape(int w, int h);
static void animate();


typedef struct _bsp_t
{
	int    plane;
	int    ntris;
	float *vlist;
	float *nlist;
	float  min[3];
	float  max[3];
	struct _bsp_t *child[2];
	struct _bsp_t *parent;
} bsp_t;

static bsp_t   *bsp;
static bsp_t   *curr;

static void load_bsp_traverse(bsp_t **root, FILE *fp);

void
calc_normal(float *v, float *n)
{
	float v01[3];
	float v02[3];
	float norm;

	v01[0] = v[3 * 1 + 0] - v[3 * 0 + 0];
	v01[1] = v[3 * 1 + 1] - v[3 * 0 + 1];
	v01[2] = v[3 * 1 + 2] - v[3 * 0 + 2];

	v02[0] = v[3 * 2 + 0] - v[3 * 0 + 0];
	v02[1] = v[3 * 2 + 1] - v[3 * 0 + 1];
	v02[2] = v[3 * 2 + 2] - v[3 * 0 + 2];

	n[0] = v01[1] * v02[2] - v01[2] * v02[1];
	n[1] = v01[2] * v02[0] - v01[0] * v02[2];
	n[2] = v01[0] * v02[1] - v01[1] * v02[0];

	norm = sqrt(n[0] * n[0] + n[1] * n[1] + n[2] * n[2]);
	if (norm > 1.0e-6) {
		n[0] /= norm;
		n[1] /= norm;
		n[2] /= norm;
	}
}

bsp_t *
load_bsp(const char *file)
{
	bsp_t *bsp;
	FILE *fp;
	unsigned int i;
	
	fp = fopen(file, "r");
	if (!fp) {
		printf("can't find file [ %s ]\n");
		exit(-1);
	}

	load_bsp_traverse(&bsp, fp);

	fclose(fp);

	return bsp;
}

static void
load_bsp_traverse(bsp_t **root, FILE *fp)
{
	bsp_t *bsp;
	unsigned int i;
	int childnode;
	
	bsp = (bsp_t *)malloc(sizeof(bsp_t));

	bsp->parent = NULL;

	fscanf(fp, "%d", &bsp->plane);
	fscanf(fp, "%d", &bsp->ntris);

	bsp->vlist = (float *)malloc(sizeof(float) * 9 * bsp->ntris);
	bsp->nlist = (float *)malloc(sizeof(float) * 3 * bsp->ntris);

	bmin[0] = bmin[1] = bmin[2] = 1.0e+6;
	bmax[0] = bmax[1] = bmax[2] = -1.0e6;

	for (i = 0; i < bsp->ntris * 3; i++) {
		fscanf(fp, "%f %f %f",
			&bsp->vlist[3 * i + 0],
			&bsp->vlist[3 * i + 1],
			&bsp->vlist[3 * i + 2]);
		if (bsp->vlist[3 * i + 0] < bmin[0]) {
			bmin[0] = bsp->vlist[3 * i + 0];
		}
		if (bsp->vlist[3 * i + 1] < bmin[1]) {
			bmin[1] = bsp->vlist[3 * i + 1];
		}
		if (bsp->vlist[3 * i + 2] < bmin[2]) {
			bmin[2] = bsp->vlist[3 * i + 2];
		}
		if (bsp->vlist[3 * i + 0] > bmax[0]) {
			bmax[0] = bsp->vlist[3 * i + 0];
		}
		if (bsp->vlist[3 * i + 1] > bmax[1]) {
			bmax[1] = bsp->vlist[3 * i + 1];
		}
		if (bsp->vlist[3 * i + 2] > bmax[2]) {
			bmax[2] = bsp->vlist[3 * i + 2];
		}

	}

	bsp->min[0] = bmin[0];
	bsp->min[1] = bmin[1];
	bsp->min[2] = bmin[2];
	bsp->max[0] = bmax[0];
	bsp->max[1] = bmax[1];
	bsp->max[2] = bmax[2];

	for (i = 0; i < bsp->ntris; i++) {
		calc_normal(&bsp->vlist[9 * i], &bsp->nlist[3 * i]);
	}

	center[0] = (bmax[0] - bmin[0]) / 2.0;
	center[1] = (bmax[1] - bmin[1]) / 2.0;
	center[2] = (bmax[2] - bmin[2]) / 2.0;

	if (scenesize < center[0]) scenesize = center[0];
	if (scenesize < center[1]) scenesize = center[1];
	if (scenesize < center[2]) scenesize = center[2];

	//printf("scenesize = %f\n", scenesize);

	(*root) = bsp;

	fscanf(fp, "%d\n", &childnode);

	(*root)->child[0] = NULL;
	(*root)->child[1] = NULL;

	if (childnode == -1) {
		return;
	}	
	
	if (childnode == 1) {
		load_bsp_traverse(&((*root)->child[0]), fp);
		(*root)->child[0]->parent = (*root);
	} else if (childnode == 2) {
		load_bsp_traverse(&((*root)->child[1]), fp);
		(*root)->child[1]->parent = (*root);
	}

	fscanf(fp, "%d\n", &childnode);

	if (childnode == 1) {
		load_bsp_traverse(&((*root)->child[0]), fp);
		(*root)->child[0]->parent = (*root);
	} else if (childnode == 2) {
		load_bsp_traverse(&((*root)->child[1]), fp);
		(*root)->child[1]->parent = (*root);
	} else {

	}

}


void
draw_plane(bsp_t *node)
{
	float p0[3], p1[3];

	if (node->plane == 1) {
		glColor4f(1.0, 0.0, 0.0, 0.5);
		p0[0] = node->min[0] + 0.5 * (node->max[0] - node->min[0]);
		p0[1] = node->min[1];
		p0[2] = node->min[2];
		p1[0] = node->min[0] + 0.5 * (node->max[0] - node->min[0]);
		p1[1] = node->max[1];
		p1[2] = node->max[2];

		glBegin(GL_QUADS);
			glVertex3f(p0[0], p0[1], p0[2]);
			glVertex3f(p0[0], p0[1], p1[2]);
			glVertex3f(p0[0], p1[1], p1[2]);
			glVertex3f(p0[0], p1[1], p0[2]);
		glEnd();
	} else if (node->plane == 2) {
		glColor4f(0.0, 1.0, 0.0, 0.5);
		p0[0] = node->min[0];
		p0[1] = node->min[1] + 0.5 * (node->max[1] - node->min[1]);
		p0[2] = node->min[2];
		p1[0] = node->max[0];
		p1[1] = node->min[1] + 0.5 * (node->max[1] - node->min[1]);
		p1[2] = node->max[2];

		glBegin(GL_QUADS);
			glVertex3f(p0[0], p0[1], p0[2]);
			glVertex3f(p0[0], p0[1], p1[2]);
			glVertex3f(p1[0], p0[1], p1[2]);
			glVertex3f(p1[0], p0[1], p0[2]);
		glEnd();
	} else {
		glColor4f(0.0, 0.0, 1.0, 0.5);
		p0[0] = node->min[0];
		p0[1] = node->min[1];
		p0[2] = node->min[2] + 0.5 * (node->max[2] - node->min[2]);
		p1[0] = node->max[0];
		p1[1] = node->max[1];
		p1[2] = node->min[2] + 0.5 * (node->max[2] - node->min[2]);

		glBegin(GL_QUADS);
			glVertex3f(p0[0], p0[1], p0[2]);
			glVertex3f(p1[0], p0[1], p0[2]);
			glVertex3f(p1[0], p1[1], p0[2]);
			glVertex3f(p0[0], p1[1], p0[2]);
		glEnd();
	}
}

void
draw_node(bsp_t *node)
{
	unsigned int i;

	glColor3f(1.0, 1.0, 1.0);
	for (i = 0; i < node->ntris; i++) {
		glNormal3f(node->nlist[3 * i + 0],
			   node->nlist[3 * i + 1],
			   node->nlist[3 * i + 2]);
		glBegin(GL_TRIANGLES);
		glVertex3f(node->vlist[9 * i + 0],
			   node->vlist[9 * i + 1],
			   node->vlist[9 * i + 2]);
		glVertex3f(node->vlist[9 * i + 3],
			   node->vlist[9 * i + 4],
			   node->vlist[9 * i + 5]);
		glVertex3f(node->vlist[9 * i + 6],
			   node->vlist[9 * i + 7],
			   node->vlist[9 * i + 8]);
		glEnd();
	}

	glDisable(GL_LIGHTING);

	draw_plane(node);

	if (node->parent) {
		glDisable(GL_CULL_FACE);
		glDisable(GL_DEPTH_TEST);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		draw_plane(node->parent);

		glDisable(GL_BLEND);
		glEnable(GL_DEPTH_TEST);

	}


	glEnable(GL_LIGHTING);
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

	draw_node(curr);

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
		curr = bsp;
		break;
	case 'j':
		if (curr->child[0]) {
			curr = curr->child[0];
		} else {
			printf("no left child\n");
		}
		glutPostRedisplay();
		break;
	case 'k':
		if (curr->child[1]) {
			curr = curr->child[1];
		} else {
			printf("no right child\n");
		}
		glutPostRedisplay();
		break;
	case 'l':
		if (curr->parent) {
			curr = curr->parent;
		} else {
			printf("root node\n");
		}
		glutPostRedisplay();
	default:
		break;
	}
}

void
init()
{
	GLfloat lightpos[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat lightcol[4] = {1.0, 1.0, 1.0, 1.0};

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
	
	glEnable(GL_DEPTH_TEST);

	glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lightcol);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	
	glEnable(GL_NORMALIZE);
}

int
main(int argc, char **argv)
{
	if (argc < 2) {
		printf("usage: bspview file\n");

		exit(-1);
	}

	glutInit(&argc, argv);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutCreateWindow(argv[0]);

	init();

	bsp = load_bsp(argv[1]);
	curr = bsp;

	glutReshapeFunc(reshape);
	glutDisplayFunc(display);
	glutKeyboardFunc(keyboard);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	glutIdleFunc(animate);

	glutMainLoop();

	return 0;
}
