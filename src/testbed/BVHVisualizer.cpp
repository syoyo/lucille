#include <Fl/gl.h>

#include "BVHVisualizer.h"

// codes from freeglut
static void
drawWireCube(double dSize)
{
    double size = dSize * 0.5;

#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );

    /* PWO: I dared to convert the code to use macros... */
    glBegin( GL_LINE_LOOP ); N( 1.0, 0.0, 0.0); V(+,-,+); V(+,-,-); V(+,+,-); V(+,+,+); glEnd();
    glBegin( GL_LINE_LOOP ); N( 0.0, 1.0, 0.0); V(+,+,+); V(+,+,-); V(-,+,-); V(-,+,+); glEnd();
    glBegin( GL_LINE_LOOP ); N( 0.0, 0.0, 1.0); V(+,+,+); V(-,+,+); V(-,-,+); V(+,-,+); glEnd();
    glBegin( GL_LINE_LOOP ); N(-1.0, 0.0, 0.0); V(-,-,+); V(-,+,+); V(-,+,-); V(-,-,-); glEnd();
    glBegin( GL_LINE_LOOP ); N( 0.0,-1.0, 0.0); V(-,-,+); V(-,-,-); V(+,-,-); V(+,-,+); glEnd();
    glBegin( GL_LINE_LOOP ); N( 0.0, 0.0,-1.0); V(-,-,-); V(-,+,-); V(+,+,-); V(+,-,-); glEnd();

#   undef V
#   undef N
}

static void
drawCube(double dSize)
{
    double size = dSize * 0.5;

#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );

    /* PWO: Again, I dared to convert the code to use macros... */
    glBegin( GL_QUADS );
        N( 1.0, 0.0, 0.0); V(+,-,+); V(+,-,-); V(+,+,-); V(+,+,+);
        N( 0.0, 1.0, 0.0); V(+,+,+); V(+,+,-); V(-,+,-); V(-,+,+);
        N( 0.0, 0.0, 1.0); V(+,+,+); V(-,+,+); V(-,-,+); V(+,-,+);
        N(-1.0, 0.0, 0.0); V(-,-,+); V(-,+,+); V(-,+,-); V(-,-,-);
        N( 0.0,-1.0, 0.0); V(-,-,+); V(-,-,-); V(+,-,-); V(+,-,+);
        N( 0.0, 0.0,-1.0); V(-,-,-); V(-,+,-); V(+,+,-); V(+,-,-);
    glEnd();

#   undef V
#   undef N
}

static void
drawBoundingBox(double bmin[3], double bmax[3])
{
	double center[3];
	double width[3];

	width[0] = (bmax[0] - bmin[0]) * 0.5;
	width[1] = (bmax[1] - bmin[1]) * 0.5;
	width[2] = (bmax[2] - bmin[2]) * 0.5;

	center[0] = bmin[0] + width[0];
	center[1] = bmin[1] + width[1];
	center[2] = bmin[2] + width[2];

	glPushMatrix();
	glTranslatef(center[0], center[1], center[2]);
	glScalef(width[0], width[1], width[2]);

	glEnable(GL_BLEND);
	glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glColor4f(0.3f, 0.6f, 0.3f, 0.2f);

	drawCube(2.0);

	glDisable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);

	glColor3f(1.0f, 1.0f, 0.0f);
	drawWireCube(2.0);

	glPopMatrix();

	glEnable(GL_LIGHTING);

}

static void
drawBoundingBoxWithColor(float bmin[4], float bmax[4], float col[4])
{
	double center[3];
	double width[3];

	width[0] = (bmax[0] - bmin[0]) * 0.5;
	width[1] = (bmax[1] - bmin[1]) * 0.5;
	width[2] = (bmax[2] - bmin[2]) * 0.5;

	center[0] = bmin[0] + width[0];
	center[1] = bmin[1] + width[1];
	center[2] = bmin[2] + width[2];

	glPushMatrix();
	glTranslatef(center[0], center[1], center[2]);
	glScalef(width[0], width[1], width[2]);

	glEnable(GL_BLEND);
	glDisable(GL_LIGHTING);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glColor4f(col[0], col[1], col[2], col[3]);

	drawCube(2.0);

	glDisable(GL_BLEND);

	glColor3f(1.0f, 1.0f, 0.0f);
	drawWireCube(2.0);

	glPopMatrix();

	glEnable(GL_LIGHTING);

}

typedef struct _triangle_t {

    ri_float_t v0x, v0y, v0z;
    ri_float_t v1x, v1y, v1z;
    ri_float_t v2x, v2y, v2z;

    void       *geom;
    uint32_t    index;

} triangle_t;

static void
drawTriangles( ri_qbvh_node_t *node )
{
    float v[3][3];

    int i;
    triangle_t *triangles;
    int ntriangles;

    if (node == NULL) return;
    if (!node->is_leaf) return;

    triangles = (triangle_t *)node->child[0];
    ntriangles = (int)node->child[1];

    glBegin(GL_TRIANGLES);

    for (i = 0; i < ntriangles; i++) {

        v[0][0] = triangles[i].v0x;
        v[0][1] = triangles[i].v0y;
        v[0][2] = triangles[i].v0z;
        v[1][0] = triangles[i].v1x;
        v[1][1] = triangles[i].v1y;
        v[1][2] = triangles[i].v1z;
        v[2][0] = triangles[i].v2x;
        v[2][1] = triangles[i].v2y;
        v[2][2] = triangles[i].v2z;

        glVertex3f( v[0][0], v[0][1], v[0][2] );
        glVertex3f( v[1][0], v[1][1], v[1][2] );
        glVertex3f( v[2][0], v[2][1], v[2][2] );

    }

    glEnd();
}

void
BVHVisualizer::drawBVH()
{
    float colors[2][4] = {
        {0.7f, 0.2f, 0.2f, 0.5f},
        {0.2f, 0.7f, 0.2f, 0.5f} };

    ri_qbvh_node_t *node;

    node = this->getCurrentNode();

    if (node->is_leaf) {
        return;
    }

    float bmin[3], bmax[3];

    // printf("left\n");
    // drawTriangles( node->child[0] );
    // printf("right\n");
    // drawTriangles( node->child[1] );

    // left
    bmin[0] = node->bbox[BMIN_X0] - 0.01;
    bmin[1] = node->bbox[BMIN_Y0] - 0.01;
    bmin[2] = node->bbox[BMIN_Z0] - 0.01;
    bmax[0] = node->bbox[BMAX_X0] + 0.01;
    bmax[1] = node->bbox[BMAX_Y0] + 0.01;
    bmax[2] = node->bbox[BMAX_Z0] + 0.01;
    drawBoundingBoxWithColor(bmin, bmax, colors[0]);

    // right
    bmin[0] = node->bbox[BMIN_X1] - 0.01;
    bmin[1] = node->bbox[BMIN_Y1] - 0.01;
    bmin[2] = node->bbox[BMIN_Z1] - 0.01;
    bmax[0] = node->bbox[BMAX_X1] + 0.01;
    bmax[1] = node->bbox[BMAX_Y1] + 0.01;
    bmax[2] = node->bbox[BMAX_Z1] + 0.01;
    drawBoundingBoxWithColor(bmin, bmax, colors[1]);

}

int
BVHVisualizer::followLeft()
{
    ri_qbvh_node_t *nextNode;

    if (this->getCurrentNode()->is_leaf) {

        return 1;

    } else {

        nextNode = this->getCurrentNode()->child[0];

        this->nodeStack.push_back(nextNode);
    }

    return 0;
}

int
BVHVisualizer::followRight()
{
    ri_qbvh_node_t *nextNode;

    if (this->getCurrentNode()->is_leaf) {

        return 1;

    } else {

        nextNode = this->getCurrentNode()->child[1];

        this->nodeStack.push_back(nextNode);

    }

    return 0;
}

int
BVHVisualizer::followParent()
{
    if (this->nodeStack.size() < 2) {

        return 1;

    } else {

        this->nodeStack.pop_back();

    }

    return 0;
}
