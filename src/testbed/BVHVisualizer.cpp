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
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glColor4f(0.3f, 0.6f, 0.3f, 0.2f);

	drawCube(2.0);

	glDisable(GL_BLEND);

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

    // left
    bmin[0] = node->bbox[0] - 0.01;
    bmin[1] = node->bbox[1] - 0.01;
    bmin[2] = node->bbox[2] - 0.01;
    bmax[0] = node->bbox[3] + 0.01;
    bmax[1] = node->bbox[4] + 0.01;
    bmax[2] = node->bbox[5] + 0.01;
    printf("left: (%f, %f, %f)-(%f, %f, %f)\n",
        bmin[0], bmin[1], bmin[2],
        bmax[0], bmax[1], bmax[2]);
    drawBoundingBoxWithColor(bmin, bmax, colors[0]);

    // right
    bmin[0] = node->bbox[6+0] - 0.01;
    bmin[1] = node->bbox[6+1] - 0.01;
    bmin[2] = node->bbox[6+2] - 0.01;
    bmax[0] = node->bbox[6+3] + 0.01;
    bmax[1] = node->bbox[6+4] + 0.01;
    bmax[2] = node->bbox[6+5] + 0.01;
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
