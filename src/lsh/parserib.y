%{
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
//#include <alloca.h>

#include "ri.h"
#include "array.h"
#include "memory.h"
#include "log.h"

#define YYDEBUG 1

#define STRING_ARRAY 1
#define NUM_ARRAY    2 


extern int yylex(void);

extern int lexrib_mode_param;
extern int lexrib_mode_skip;
extern int line_num;

ri_ptr_array_t *curr_array       = NULL;
unsigned int    curr_array_count = 0;

int             array_type;

long      rib_param_num         = 0;
long      rib_param_num_alloced = 0;
RtToken   *rib_param_tokens      = NULL;
RtPointer *rib_param_args        = NULL;
RtInt     *rib_param_arg_size    = NULL;

static const int	max_unknown_commands = 1000;
static int		nunknown_commands = 0;

void yyerror( char *str ) {
	printf( "parse error near line[%d]: %s\n", line_num, str );
}

static void numptrfree(void *data)
{
	RtFloat *p;
	p = (RtFloat *)data;

	ri_mem_free(p);
}

static void strptrfree(void *data)
{
	char *p;
	p = (char *)data;

	ri_mem_free(p);
}

static void enter_mode_param()
{
	lexrib_mode_param = 1;
}

static void enter_mode_skip()
{
	lexrib_mode_skip = 1;
}


static void add_array(void *val)
{
	assert(curr_array);

	ri_ptr_array_insert(curr_array, curr_array_count,
			val);
	curr_array_count++;
}

static void init_array()
{
	if (curr_array) {
		if (array_type == STRING_ARRAY) {
			ri_ptr_array_traverse(curr_array, strptrfree);
		} else {
			ri_ptr_array_traverse(curr_array, numptrfree);
		}
		ri_ptr_array_free(curr_array);
	}

	curr_array = ri_ptr_array_new();
	curr_array_count = 0;
}

static ri_ptr_array_t *array_dup(ri_ptr_array_t *array)
{
	ri_ptr_array_t *dup = NULL;

	if (array == NULL) return NULL;

	dup = ri_ptr_array_new();
	dup->alloc   = array->alloc;
	dup->nelems  = array->nelems;
	dup->data    = (void **)ri_mem_alloc(sizeof(void *) * array->alloc);

	ri_mem_copy(dup->data, array->data, sizeof(void *) * array->alloc);

	return dup;	
}

%}

%union {
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
}

%token <string> STRING ID
%token <num> NUM
%token LBRACKET RBRACKET
%token AREALIGHTSOURCE
%token ATTRIBUTE
%token ATTRIBUTEBEGIN ATTRIBUTEEND
%token CLIPPING
%token CONCATTRANSFORM
%token COLOR
%token COORDINATESYSTEM
%token DECLARE
%token DEPTHOFFIELD
%token DISPLAY
%token EXPOSURE
%token FORMAT
%token FRAMEBEGIN FRAMEEND
%token FRAMEASPECTRATIO
%token HIDER
%token IDENTITY
%token ILLUMINATE
%token IMAGER
%token LIGHTSOURCE
%token MOTIONBEGIN MOTIONEND
%token OPACITY
%token OPTION
%token ORIENTATION
%token PERSPECTIVE
%token PIXELFILTER
%token PIXELSAMPLES
%token POINTSPOLYGONS
%token POINTSGENERALPOLYGONS
%token PROJECTION
%token POLYGON
%token QUANTIZE
%token ROTATE
%token RIBVERSION
%token SCALE
%token SHADINGRATE
%token SHADINGINTERPOLATION
%token SHUTTER
%token SIDES
%token SPHERE
%token SUBDIVISIONMESH
%token SURFACE
%token TRANSFORM
%token TRANSFORMBEGIN TRANSFORMEND
%token TRANSLATE
%token WORLDBEGIN WORLDEND

%token HIGH_PRECEDENCE

%token UNKNOWN

%type <paramarray> param_array param_num_array param_str_array

%%


start: ri_command_list
;

array_init: %prec HIGH_PRECEDENCE
{
	init_array();
}
;

num_list_init: %prec HIGH_PRECEDENCE
{
	array_type = NUM_ARRAY;
}
;

str_list_init: %prec HIGH_PRECEDENCE
{
	array_type = STRING_ARRAY;
}
;

param_array: param_num_array
{
	$$ = $1;
}
| param_str_array
{
	$$ = $1;
}
| NUM
{
	RtFloat *num;

	init_array();
	array_type = NUM_ARRAY;

	num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
	*num = (RtFloat)($1);
	add_array(num);

	$$ = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
;


param_num_array: array_init LBRACKET num_list RBRACKET
{
	$$ = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
;

param_str_array: array_init LBRACKET str_list RBRACKET
{
	$$ = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
| array_init STRING
{
	RtToken str = (RtToken)strdup((char *)($2));
	add_array(str);

	$$ = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
;

num_list: num_list num_list_entry
	| num_list_entry
;

str_list: str_list str_list_entry
	| str_list_entry
;

str_list_entry: str_list_init STRING
{
	RtToken str = (RtToken)strdup((char *)($2));
	add_array(str);
}
;

num_list_entry: num_list_init NUM
{
	RtFloat *num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
	*num = (RtFloat)($2);
	add_array(num);
}
	      | /* no elements */
{
	//printf("no elem\n");
}
;

param_list: param_list_init param_lists
;

param_list_init: %prec HIGH_PRECEDENCE
{
	rib_param_num = 0;
	curr_array_count = 0;
}
;

param_lists: param_list_entry param_lists
	   | /* or empty */
;

param_list_entry: STRING param_array
{
	unsigned long i;
	unsigned long n;
	RtPointer arg;
	RtToken   tag;

	ri_ptr_array_t *t;

	tag = $1;
	t = $2;

	n = $2->nelems;
	if (array_type == NUM_ARRAY) {
		arg = ri_mem_alloc(n * sizeof(RtFloat));
	} else { /* STRING_ARRAY */
		arg = ri_mem_alloc(n * sizeof(RtToken));
	}

	for (i = 0; i < n; i++) {
		if (array_type == NUM_ARRAY) {
			((RtFloat *)arg)[i] = *((RtFloat *)ri_ptr_array_at(t, i));
		} else { /* STRING_ARRAY */
			((RtToken *)arg)[i] = strdup((RtToken)ri_ptr_array_at(t, i));
		}
	}

	if (rib_param_num >= rib_param_num_alloced) {
		rib_param_num_alloced = 2 * rib_param_num_alloced + 1;

		rib_param_tokens = (RtToken *)realloc(rib_param_tokens, rib_param_num_alloced * sizeof(RtToken));
		rib_param_args = (RtPointer *)realloc(rib_param_args, rib_param_num_alloced * sizeof(RtPointer));
		rib_param_arg_size = (RtInt *)realloc(rib_param_arg_size, rib_param_num_alloced * sizeof(RtInt));
	}

	rib_param_tokens[rib_param_num]   = tag;
	rib_param_arg_size[rib_param_num] = n;
	rib_param_args[rib_param_num]     = arg;

	rib_param_num++;

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse($2, numptrfree);
	} else {
		ri_ptr_array_traverse($2, strptrfree);
	}
	ri_ptr_array_free($2);
}
;


ri_command_list: ri_command_list ri_command
	       | ri_command
;

ri_command: protocol
{
	enter_mode_skip();
}
;

protocol: ribversion NUM
{
	//printf("RIB Version: %f\n", $2 );
}
| attribute STRING param_list
{
	RiAttributeV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| arealightsource STRING NUM param_list
{
	RiAreaLightSourceV((RtToken)$2,
			   rib_param_num, rib_param_tokens, rib_param_args);
}
| attributebegin
{
	RiAttributeBegin();
}
| attributeend
{
	RiAttributeEnd();
}
| clipping NUM NUM
{
	RiClipping($2, $3);
}
| color param_num_array
{
	int     i;
	RtColor col;
	ri_ptr_array_t *p;

	p = $2;

	/* currently, only consider 3 component color */
	if (p->nelems < 3) {
		ri_log("warning", "RiColor() with compnent < 3");
	} else {
		for (i = 0; i < 3; i++) {
			col[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiColor(col);
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse($2, numptrfree);
	} else {
		ri_ptr_array_traverse($2, strptrfree);
	}
	ri_ptr_array_free(p);
}
| concattransform LBRACKET NUM NUM NUM NUM
			   NUM NUM NUM NUM
			   NUM NUM NUM NUM
			   NUM NUM NUM NUM RBRACKET
{
	RtMatrix mat;

	mat[0][0] = $3 ; mat[0][1] = $4 ; mat[0][2] = $5 ; mat[0][3] = $6 ;
	mat[1][0] = $7 ; mat[1][1] = $8 ; mat[1][2] = $9 ; mat[1][3] = $10;
	mat[2][0] = $11; mat[2][1] = $12; mat[2][2] = $13; mat[2][3] = $14;
	mat[3][0] = $15; mat[3][1] = $16; mat[3][2] = $17; mat[3][3] = $18;

	RiConcatTransform(mat);

}
| coordinatesystem STRING
{
	RiCoordinateSystem((RtToken)$2);
}
| declare STRING STRING
{
	RiDeclare($2, $3);
}
| depthoffield NUM NUM NUM
{
	RiDepthOfField((RtFloat)$2, (RtFloat)$3, (RtFloat)$4);
}
| display STRING STRING STRING param_list
{
	RiDisplayV($2, $3, $4,
		   rib_param_num, rib_param_tokens, rib_param_args);
}
| exposure NUM NUM
{
	RiExposure($2, $3);
}
| format NUM NUM NUM
{
	RiFormat($2, $3, $4);
}
| framebegin NUM
{
	RiFrameBegin($2);
}
| frameend
{
	RiFrameEnd();

}
| frameaspectratio NUM
{
	RiFrameAspectRatio($2);

}
| hider STRING param_list
{
	RiHiderV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| identity
{
	RiIdentity();
}
| illuminate NUM NUM
{
	RtInt lightid;

	lightid = (RtInt)$2;

	RiIlluminate((RtLightHandle)&(lightid), $3);
}
| imager STRING param_list
{
	RiImagerV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| lightsource STRING NUM param_list
{
	RiLightSourceV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| motionbegin param_num_array
{ 
	unsigned int    i;
	RtFloat        *times;
	ri_ptr_array_t *p;

	if (array_type != NUM_ARRAY) {
		ri_log("warning", "MotionBegin: not a float array argument.\n");
	} else {
		p = $2;

		times = (RtFloat *)ri_mem_alloc(sizeof(RtFloat) * p->nelems);

		for (i = 0; i < p->nelems; i++) {
			times[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiMotionBeginV(p->nelems, times);

		ri_ptr_array_traverse(p, numptrfree);
		ri_ptr_array_free(p);
	}
}
| motionend
{
	RiMotionEnd();
}
| opacity param_num_array
{
	int     i;
	RtColor opa;
	ri_ptr_array_t *p;

	p = $2;

	/* currently, only consider 3 component color */
	if (p->nelems < 3) {
		ri_log("warning", "RiOpacity() with compnent < 3");
	} else {
		for (i = 0; i < 3; i++) {
			opa[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiOpacity(opa);
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse($2, numptrfree);
	} else {
		ri_ptr_array_traverse($2, strptrfree);
	}
	ri_ptr_array_free(p);
}
| option STRING param_list
{
	RiOptionV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| orientation STRING
{
	RiOrientation((RtToken)$2);
}
| perspective NUM
{
	RiPerspective($2);
}
| pixelfilter STRING NUM NUM
{
	RiPixelFilter(RiBoxFilter, $3, $4);
}
| pixelsamples NUM NUM
{
	RiPixelSamples($2, $3);
}
| pointspolygons param_num_array param_num_array param_list
{
	int             i;
	int             have_p = 0;
	RtInt           npolys, *nverts, *verts;
	ri_ptr_array_t *p;

	p = $2;
	npolys = p->nelems;
	
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = $3;
	verts  = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		verts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	for (i = 0; i < rib_param_num; i++) {
		if (strcmp(rib_param_tokens[i], RI_P) == 0) {
			have_p = 1;
		}
	} 

	if (!have_p) {
		printf("PointsPolygons without RI_P parameter.\n");
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse($2, numptrfree);
	} else {
		ri_ptr_array_traverse($2, strptrfree);
	}
	ri_ptr_array_free(p);

	RiPointsPolygonsV(npolys, nverts, verts,
			  rib_param_num, rib_param_tokens, rib_param_args);

}
| pointsgeneralpolygons param_num_array param_num_array param_num_array param_list
{
	int             i;
	int             docall = 1;
	int             have_p = 0;
	int             loop_warn = 0;
	RtInt           loopsize;
	RtInt           npolys, *nloops, *nverts, *verts;
	ri_ptr_array_t *p;

	p = $2;
	npolys = p->nelems;
	
	nloops = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nloops[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}
	loopsize = p->nelems;

	p = $3;
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = $4;
	verts  = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		verts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	for (i = 0; i < rib_param_num; i++) {
		if (strcmp(rib_param_tokens[i], RI_P) == 0) {
			have_p = 1;
		}
	} 

	if (!have_p) {
		printf("PointsGeneralPolygons without RI_P parameter.\n");
		docall = 0;
	}

	for (i = 0; i < loopsize; i++) {
		if (nloops[i] != 1 && !loop_warn) {
			printf("Currently, nloops[%d] must be 1.\n", i);
			docall = 0;
			loop_warn = 1;	/* prevent multiple warning. */
		}
	}

	if (docall) {
		RiPointsGeneralPolygonsV(npolys, nloops, nverts, verts,
					 rib_param_num,
					 rib_param_tokens, rib_param_args);
	}
}
| polygon param_list
{
	int   i;
	RtInt nvertices = 0;

        for (i = 0; i < rib_param_num; i++) {
                if (strcmp(rib_param_tokens[i], RI_P) == 0) {
                        nvertices = rib_param_arg_size[i];

                        if (nvertices % 3 != 0) {
                                printf("RI_P array must be 3*n in length.");
                                nvertices = 0;
                        } else {
                                nvertices /= 3;
                        }
                } else if (strcmp(rib_param_tokens[i], RI_N) == 0) {
                        nvertices = rib_param_arg_size[i];

                        if (nvertices % 3 != 0) {
                                printf("RI_N array must be 3*n in length.");
                                nvertices = 0;
                        } else {
                                nvertices /= 3;
                        }
                }
        } 

	if (nvertices) {
		RiPolygonV(nvertices,
			   rib_param_num, rib_param_tokens, rib_param_args);
	} else {
		ri_log("warning", "Polygon call with invalid parameter");
	}
}
| projection STRING param_list
{
	RiProjectionV((RtToken)$2,
		      rib_param_num, rib_param_tokens, rib_param_args);
} 
| quantize STRING NUM NUM NUM NUM
{
	RiQuantize((RtToken)$2, $3, $4, $5, $6);
}
| rotate NUM NUM NUM NUM
{
	RiRotate((RtFloat)$2, (RtFloat)$3, (RtFloat)$4, (RtFloat)$5);
}
| scale NUM NUM NUM
{
	RiScale((RtFloat)$2, (RtFloat)$3, (RtFloat)$4);
}
| sphere NUM NUM NUM NUM param_list
{
	RiSphereV((RtFloat)$2, (RtFloat)$3, (RtFloat)$4, (RtFloat)$5,
		  rib_param_num, rib_param_tokens, rib_param_args);
}
| sides NUM
{
	RiSides((RtFloat)$2);
}
| shadinginterpolation STRING
{
	RiShadingInterpolation((RtToken)$2);
}
| shadingrate NUM
{
	RiShadingRate($2);
}
| shutter NUM NUM
{
	RiShutter($2, $3);
}
| subdivisionmesh STRING param_num_array param_num_array param_str_array param_num_array param_num_array param_num_array param_list
{
	int             i;
	int             docall = 1;
	int             have_p = 0;
	int             face_warn = 0;
	RtInt           nfaces, *nverts, *verts;
	ri_ptr_array_t *p;

	p = $3;
	nfaces = p->nelems;
	
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * nfaces);
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = $4;
	verts  = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		verts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	for (i = 0; i < rib_param_num; i++) {
		if (strcmp(rib_param_tokens[i], RI_P) == 0) {
			have_p = 1;
		}
	} 

	if (!have_p) {
		printf("PointsPolygons without RI_P parameter.\n");
		docall = 0;
	}

	for (i = 0; i < nfaces; i++) {
		if (nverts[i] != 4 && !face_warn) {
			printf("Currently, only quad face are supported\n");
			docall = 0;
			face_warn = 1;	/* prevent multiple warning. */
		}
	}

	if (docall) {
		RiSubdivisionMeshV($2, nfaces, nverts, verts,
				   0,			/* ntags	*/
				   NULL,		/* tags		*/
				   NULL,		/* nargs	*/
				   NULL,		/* intargs	*/
				   NULL,		/* floatargs	*/
				   rib_param_num,
				   rib_param_tokens, rib_param_args);
	}
	
}
| surface STRING param_list
{
	RiSurfaceV($2, rib_param_num, rib_param_tokens, rib_param_args);
}
| transform param_num_array
{
	int     i, j;
	RtMatrix matrix;
	ri_ptr_array_t *p;
	
	p = $2;
	if (p->nelems == 16) {
		for (i = 0; i < 4; i++) {
			for (j = 0; j < 4; j++) {
				matrix[i][j] = (*((RtFloat *)ri_ptr_array_at(p, i * 4 + j)));
			}
		}

		RiTransform(matrix);
	} else {	
		ri_log("warning", "nelems != 16");
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse($2, numptrfree);
	} else {
		ri_ptr_array_traverse($2, strptrfree);
	}
	ri_ptr_array_free(p);
}
| transformbegin
{
	RiTransformBegin();
}
| transformend
{
	RiTransformEnd();
}
| translate NUM NUM NUM
{
	RiTranslate((RtFloat)$2, (RtFloat)$3, (RtFloat)$4);
}
| worldbegin
{
	RiWorldBegin();
}
| worldend
{
	RiWorldEnd();
}
| UNKNOWN
{
	enter_mode_skip();
	printf("[RIB parse] Unknown command: %s at line %d\n", yylval.string, line_num);
	nunknown_commands++;
	if (nunknown_commands > max_unknown_commands) {
		printf("[RIB parse] Too many unknown commands. Give up parsing.\n");
		exit(-1);
	}
}
;

ribversion		: RIBVERSION 		{ enter_mode_param();	}
;
arealightsource		: AREALIGHTSOURCE	{ enter_mode_param();	}
;
attribute		: ATTRIBUTE 		{ enter_mode_param();	}
;
attributebegin		: ATTRIBUTEBEGIN 	{ enter_mode_param();	}
;
attributeend		: ATTRIBUTEEND		{ enter_mode_param();	}
;
clipping		: CLIPPING		{ enter_mode_param();	}
;
color			: COLOR			{ enter_mode_param();	}
;
concattransform		: CONCATTRANSFORM 	{ enter_mode_param();	}
; 
coordinatesystem	: COORDINATESYSTEM	{ enter_mode_param();	}
;
declare			: DECLARE		{ enter_mode_param();	}
;
depthoffield		: DEPTHOFFIELD		{ enter_mode_param(); 	}
;
display			: DISPLAY		{ enter_mode_param();	}
;
exposure		: EXPOSURE		{ enter_mode_param();	}
;
framebegin		: FRAMEBEGIN		{ enter_mode_param();	}
;
frameend		: FRAMEEND		{ enter_mode_param();	}
;
frameaspectratio	: FRAMEASPECTRATIO	{ enter_mode_param();	}
;
format			: FORMAT		{ enter_mode_param();	}
;
hider			: HIDER			{ enter_mode_param();	}
;
identity		: IDENTITY		{ enter_mode_param();	}
;
illuminate		: ILLUMINATE		{ enter_mode_param();	}
;
imager			: IMAGER		{ enter_mode_param();	}
;
lightsource		: LIGHTSOURCE		{ enter_mode_param();	}
;
motionbegin		: MOTIONBEGIN		{ enter_mode_param();	}
;
motionend		: MOTIONEND		{ enter_mode_param();	}
;
opacity			: OPACITY		{ enter_mode_param();	}
;
option			: OPTION		{ enter_mode_param();	}
;
orientation		: ORIENTATION		{ enter_mode_param();	}
;
perspective		: PERSPECTIVE		{ enter_mode_param();	}
;
pixelfilter		: PIXELFILTER		{ enter_mode_param();	}
;
pixelsamples		: PIXELSAMPLES		{ enter_mode_param();	}
;
pointspolygons		: POINTSPOLYGONS	{ enter_mode_param();	}
;
pointsgeneralpolygons	: POINTSGENERALPOLYGONS { enter_mode_param();	}
;
polygon			: POLYGON		{ enter_mode_param();	}
;
projection		: PROJECTION		{ enter_mode_param();	}
;
quantize		: QUANTIZE		{ enter_mode_param();	}
;
rotate			: ROTATE		{ enter_mode_param();	}
;
scale			: SCALE			{ enter_mode_param();	}
;
sphere			: SPHERE		{ enter_mode_param();	}
;
sides			: SIDES			{ enter_mode_param();	}
;
shadinginterpolation	: SHADINGINTERPOLATION	{ enter_mode_param(); }
;
shadingrate		: SHADINGRATE		{ enter_mode_param();	}
;
shutter			: SHUTTER		{ enter_mode_param();	}
;
subdivisionmesh		: SUBDIVISIONMESH	{ enter_mode_param();	}
;
surface			: SURFACE		{ enter_mode_param();	}
;
transform		: TRANSFORM		{ enter_mode_param();	}
;
transformbegin		: TRANSFORMBEGIN	{ enter_mode_param();	}
;
transformend		: TRANSFORMEND		{ enter_mode_param();	}
;
translate		: TRANSLATE		{ enter_mode_param();	}
;
worldbegin		: WORLDBEGIN		{ enter_mode_param();	}
;
worldend		: WORLDEND		{ enter_mode_param();	}
;

%%
