
/*  A Bison parser, made from parserib.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	STRING	257
#define	ID	258
#define	NUM	259
#define	LBRACKET	260
#define	RBRACKET	261
#define	AREALIGHTSOURCE	262
#define	ATTRIBUTE	263
#define	ATTRIBUTEBEGIN	264
#define	ATTRIBUTEEND	265
#define	CLIPPING	266
#define	CONCATTRANSFORM	267
#define	COLOR	268
#define	COORDINATESYSTEM	269
#define	DECLARE	270
#define	DEPTHOFFIELD	271
#define	DISPLAY	272
#define	EXPOSURE	273
#define	FORMAT	274
#define	FRAMEBEGIN	275
#define	FRAMEEND	276
#define	FRAMEASPECTRATIO	277
#define	HIDER	278
#define	IDENTITY	279
#define	ILLUMINATE	280
#define	IMAGER	281
#define	LIGHTSOURCE	282
#define	MOTIONBEGIN	283
#define	MOTIONEND	284
#define	OPACITY	285
#define	OPTION	286
#define	ORIENTATION	287
#define	PERSPECTIVE	288
#define	PIXELFILTER	289
#define	PIXELSAMPLES	290
#define	POINTSPOLYGONS	291
#define	POINTSGENERALPOLYGONS	292
#define	PROJECTION	293
#define	POLYGON	294
#define	QUANTIZE	295
#define	ROTATE	296
#define	RIBVERSION	297
#define	SCALE	298
#define	SCREENWINDOW	299
#define	SHADINGRATE	300
#define	SHADINGINTERPOLATION	301
#define	SHUTTER	302
#define	SIDES	303
#define	SPHERE	304
#define	SUBDIVISIONMESH	305
#define	SURFACE	306
#define	TRANSFORM	307
#define	TRANSFORMBEGIN	308
#define	TRANSFORMEND	309
#define	TRANSLATE	310
#define	WORLDBEGIN	311
#define	WORLDEND	312
#define	HIGH_PRECEDENCE	313
#define	UNKNOWN	314

#line 1 "parserib.y"

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


#line 117 "parserib.y"
typedef union {
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
} YYSTYPE;
#ifndef YYDEBUG
#define YYDEBUG 1
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		252
#define	YYFLAG		-32768
#define	YYNTBASE	61

#define YYTRANSLATE(x) ((unsigned)(x) <= 314 ? yytranslate[x] : 130)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     3,     4,     5,     7,     9,    11,    16,    21,
    24,    27,    29,    32,    34,    37,    40,    41,    44,    45,
    48,    49,    52,    55,    57,    59,    62,    66,    71,    73,
    75,    79,    82,   102,   105,   109,   114,   120,   124,   129,
   132,   134,   137,   141,   143,   147,   151,   156,   159,   161,
   164,   168,   171,   174,   179,   183,   188,   194,   197,   201,
   208,   214,   219,   225,   232,   235,   238,   241,   245,   255,
   259,   262,   264,   266,   271,   273,   275,   277,   279,   281,
   283,   285,   287,   289,   291,   293,   295,   297,   299,   301,
   303,   305,   307,   309,   311,   313,   315,   317,   319,   321,
   323,   325,   327,   329,   331,   333,   335,   337,   339,   341,
   343,   345,   347,   349,   351,   353,   355,   357,   359,   361,
   363,   365,   367,   369,   371,   373,   375,   377
};

static const short yyrhs[] = {    76,
     0,     0,     0,     0,    66,     0,    67,     0,     5,     0,
    62,     6,    68,     7,     0,    62,     6,    69,     7,     0,
    62,     3,     0,    68,    71,     0,    71,     0,    69,    70,
     0,    70,     0,    64,     3,     0,    63,     5,     0,     0,
    73,    74,     0,     0,    75,    74,     0,     0,     3,    65,
     0,    76,    77,     0,    77,     0,    78,     0,    79,     5,
     0,    81,     3,    72,     0,    80,     3,     5,    72,     0,
    82,     0,    83,     0,    84,     5,     5,     0,    85,    66,
     0,    86,     6,     5,     5,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     7,
     0,    87,     3,     0,    88,     3,     3,     0,    89,     5,
     5,     5,     0,    90,     3,     3,     3,    72,     0,    91,
     5,     5,     0,    95,     5,     5,     5,     0,    92,     5,
     0,    93,     0,    94,     5,     0,    96,     3,    72,     0,
    97,     0,    98,     5,     5,     0,    99,     3,    72,     0,
   100,     3,     5,    72,     0,   101,    66,     0,   102,     0,
   103,    66,     0,   104,     3,    72,     0,   105,     3,     0,
   106,     5,     0,   107,     3,     5,     5,     0,   108,     5,
     5,     0,   109,    66,    66,    72,     0,   110,    66,    66,
    66,    72,     0,   111,    72,     0,   112,     3,    72,     0,
   113,     3,     5,     5,     5,     5,     0,   114,     5,     5,
     5,     5,     0,   115,     5,     5,     5,     0,   116,     5,
     5,     5,     5,     0,   117,     5,     5,     5,     5,    72,
     0,   118,     5,     0,   119,     3,     0,   120,     5,     0,
   121,     5,     5,     0,   122,     3,    66,    66,    67,    66,
    66,    66,    72,     0,   123,     3,    72,     0,   124,    66,
     0,   125,     0,   126,     0,   127,     5,     5,     5,     0,
   128,     0,   129,     0,    60,     0,    43,     0,     8,     0,
     9,     0,    10,     0,    11,     0,    12,     0,    14,     0,
    13,     0,    15,     0,    16,     0,    17,     0,    18,     0,
    19,     0,    21,     0,    22,     0,    23,     0,    20,     0,
    24,     0,    25,     0,    26,     0,    27,     0,    28,     0,
    29,     0,    30,     0,    31,     0,    32,     0,    33,     0,
    34,     0,    35,     0,    36,     0,    37,     0,    38,     0,
    40,     0,    39,     0,    41,     0,    42,     0,    44,     0,
    45,     0,    50,     0,    49,     0,    47,     0,    46,     0,
    48,     0,    51,     0,    52,     0,    53,     0,    54,     0,
    55,     0,    56,     0,    57,     0,    58,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   182,   185,   191,   197,   203,   207,   211,   229,   237,   243,
   254,   255,   258,   259,   262,   269,   275,   281,   284,   291,
   292,   295,   346,   347,   350,   356,   360,   364,   369,   373,
   377,   381,   407,   422,   426,   430,   434,   439,   443,   447,
   451,   456,   461,   465,   469,   477,   481,   485,   508,   512,
   538,   542,   546,   550,   554,   558,   600,   656,   690,   695,
   699,   703,   707,   711,   716,   720,   724,   728,   732,   786,
   790,   816,   820,   824,   828,   832,   836,   848,   850,   852,
   854,   856,   858,   860,   862,   864,   866,   868,   870,   872,
   874,   876,   878,   880,   882,   884,   886,   888,   890,   892,
   894,   896,   898,   900,   902,   904,   906,   908,   910,   912,
   914,   916,   918,   920,   922,   924,   926,   928,   930,   932,
   934,   936,   938,   940,   942,   944,   946,   948
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","STRING",
"ID","NUM","LBRACKET","RBRACKET","AREALIGHTSOURCE","ATTRIBUTE","ATTRIBUTEBEGIN",
"ATTRIBUTEEND","CLIPPING","CONCATTRANSFORM","COLOR","COORDINATESYSTEM","DECLARE",
"DEPTHOFFIELD","DISPLAY","EXPOSURE","FORMAT","FRAMEBEGIN","FRAMEEND","FRAMEASPECTRATIO",
"HIDER","IDENTITY","ILLUMINATE","IMAGER","LIGHTSOURCE","MOTIONBEGIN","MOTIONEND",
"OPACITY","OPTION","ORIENTATION","PERSPECTIVE","PIXELFILTER","PIXELSAMPLES",
"POINTSPOLYGONS","POINTSGENERALPOLYGONS","PROJECTION","POLYGON","QUANTIZE","ROTATE",
"RIBVERSION","SCALE","SCREENWINDOW","SHADINGRATE","SHADINGINTERPOLATION","SHUTTER",
"SIDES","SPHERE","SUBDIVISIONMESH","SURFACE","TRANSFORM","TRANSFORMBEGIN","TRANSFORMEND",
"TRANSLATE","WORLDBEGIN","WORLDEND","HIGH_PRECEDENCE","UNKNOWN","start","array_init",
"num_list_init","str_list_init","param_array","param_num_array","param_str_array",
"num_list","str_list","str_list_entry","num_list_entry","param_list","param_list_init",
"param_lists","param_list_entry","ri_command_list","ri_command","protocol","ribversion",
"arealightsource","attribute","attributebegin","attributeend","clipping","color",
"concattransform","coordinatesystem","declare","depthoffield","display","exposure",
"framebegin","frameend","frameaspectratio","format","hider","identity","illuminate",
"imager","lightsource","motionbegin","motionend","opacity","option","orientation",
"perspective","pixelfilter","pixelsamples","pointspolygons","pointsgeneralpolygons",
"polygon","projection","quantize","rotate","scale","screenwindow","sphere","sides",
"shadinginterpolation","shadingrate","shutter","subdivisionmesh","surface","transform",
"transformbegin","transformend","translate","worldbegin","worldend", NULL
};
#endif

static const short yyr1[] = {     0,
    61,    62,    63,    64,    65,    65,    65,    66,    67,    67,
    68,    68,    69,    69,    70,    71,    71,    72,    73,    74,
    74,    75,    76,    76,    77,    78,    78,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    78,    78,    78,    78,    78,    79,    80,    81,
    82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
    92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
   102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
   112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
   122,   123,   124,   125,   126,   127,   128,   129
};

static const short yyr2[] = {     0,
     1,     0,     0,     0,     1,     1,     1,     4,     4,     2,
     2,     1,     2,     1,     2,     2,     0,     2,     0,     2,
     0,     2,     2,     1,     1,     2,     3,     4,     1,     1,
     3,     2,    19,     2,     3,     4,     5,     3,     4,     2,
     1,     2,     3,     1,     3,     3,     4,     2,     1,     2,
     3,     2,     2,     4,     3,     4,     5,     2,     3,     6,
     5,     4,     5,     6,     2,     2,     2,     3,     9,     3,
     2,     1,     1,     4,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1
};

static const short yydefact[] = {     0,
    79,    80,    81,    82,    83,    85,    84,    86,    87,    88,
    89,    90,    94,    91,    92,    93,    95,    96,    97,    98,
    99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
   109,   111,   110,   112,   113,    78,   114,   115,   119,   118,
   120,   117,   116,   121,   122,   123,   124,   125,   126,   127,
   128,    77,     1,    24,    25,     0,     0,     0,    29,    30,
     0,     2,     0,     0,     0,     0,     0,     0,     0,    41,
     0,     0,     0,    44,     0,     0,     0,     2,    49,     2,
     0,     0,     0,     0,     0,     2,     2,    19,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     2,    72,    73,     0,    75,    76,    23,    26,     0,    19,
     0,     0,    32,     0,    34,     0,     0,     0,     0,    40,
    42,     0,    19,     0,    19,     0,    48,    50,    19,    52,
    53,     0,     0,     2,     2,    58,    21,    19,     0,     0,
     0,     0,     0,    65,    66,    67,     0,     2,    19,    71,
     0,    19,    27,    31,     3,     0,    35,     0,     0,    38,
     0,    43,    45,    46,    19,    51,     0,    55,    19,     2,
     2,    18,    21,    59,     0,     0,     0,     0,     0,    68,
     2,    70,     0,    28,     0,     3,    12,     0,    36,    19,
    39,    47,    54,    56,    19,     7,     0,    22,     5,     6,
    20,     0,     0,    62,     0,     0,     2,    74,    16,     8,
    11,     0,    37,    57,    10,     3,     0,    61,    63,    19,
     0,     2,     0,     0,     4,    14,    60,    64,     4,     2,
     0,    15,     9,    13,     2,     0,    19,     0,    69,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    33,     0,
     0,     0
};

static const short yydefgoto[] = {   250,
   112,   185,   224,   198,   113,   200,   186,   225,   226,   187,
   136,   137,   172,   173,    53,    54,    55,    56,    57,    58,
    59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
    69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
    79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
    89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
    99,   100,   101,   102,   103,   104,   105,   106
};

static const short yypact[] = {   150,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   150,-32768,-32768,     0,     9,    10,-32768,-32768,
    12,-32768,    13,    17,    18,    19,    22,    21,    23,-32768,
    24,    25,    29,-32768,    28,    31,    32,-32768,-32768,-32768,
    33,    35,    34,    37,    36,-32768,-32768,-32768,    40,    41,
    42,    43,    44,    45,    46,    49,    48,    50,    51,    56,
-32768,-32768,-32768,    55,-32768,-32768,-32768,-32768,    58,-32768,
    59,    60,-32768,    62,-32768,    65,    64,    68,    67,-32768,
-32768,    69,-32768,    70,-32768,    71,-32768,-32768,-32768,-32768,
-32768,    72,    73,-32768,-32768,-32768,    76,-32768,    75,    77,
    79,    80,    81,-32768,-32768,-32768,    82,-32768,-32768,-32768,
    84,-32768,-32768,-32768,    20,    85,-32768,    86,    78,-32768,
    89,-32768,-32768,-32768,-32768,-32768,    90,-32768,-32768,-32768,
    91,-32768,    76,-32768,    92,    93,    94,    95,    96,-32768,
-32768,-32768,    97,-32768,    99,    39,-32768,   100,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,     1,-32768,-32768,-32768,
-32768,   101,   102,-32768,   103,   104,-32768,-32768,-32768,-32768,
-32768,   105,-32768,-32768,-32768,     3,   106,-32768,-32768,-32768,
     8,-32768,   107,   111,    54,-32768,-32768,-32768,-32768,-32768,
   110,-32768,-32768,-32768,-32768,   112,-32768,   113,-32768,   114,
   115,   116,   117,   118,   119,   120,   121,    66,-32768,    15,
   127,-32768
};

static const short yypgoto[] = {-32768,
  -170,-32768,-32768,-32768,   -78,  -142,-32768,-32768,  -109,   -58,
  -107,-32768,   -44,-32768,-32768,    83,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768
};


#define	YYLAST		210


static const short yytable[] = {   127,
   197,   128,   153,   215,   108,    -4,   216,   134,   135,   -17,
   215,   109,   110,   229,   251,   162,   111,   164,   114,   115,
   116,   166,   150,   117,   118,   119,   -17,   120,   121,   122,
   174,   123,   124,   125,   126,   129,   221,   130,   131,   132,
   133,   182,   138,   139,   184,   210,   140,   141,   142,   143,
   144,   145,   146,   148,   147,   169,   170,   192,   149,   151,
   233,   194,   152,   154,   222,   155,   156,   157,   158,   181,
   159,   160,   249,   161,   163,   165,   167,   168,   171,   175,
   190,   176,   213,   177,   178,   179,   180,   214,   183,   188,
   189,   195,   199,   191,   193,   196,   202,   203,   204,   205,
   206,   208,   207,   209,   212,   217,   218,   219,   220,   223,
   227,   231,   228,   232,   236,   234,   238,   240,   241,   242,
   243,   244,   245,   246,   247,   248,   252,   211,   201,   239,
     0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
     0,     0,     0,   230,     0,     0,     0,     0,     0,     0,
     0,   235,     0,     0,     0,     0,   237,     1,     2,     3,
     4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,     0,    52
};

static const short yycheck[] = {    78,
   171,    80,   110,     3,     5,     3,     6,    86,    87,     7,
     3,     3,     3,     6,     0,   123,     5,   125,     6,     3,
     3,   129,   101,     5,     3,     5,     7,     5,     5,     5,
   138,     3,     5,     3,     3,     3,   207,     3,     5,     3,
     5,   149,     3,     3,   152,     7,     5,     5,     5,     5,
     5,     3,     5,     3,     5,   134,   135,   165,     3,     5,
     7,   169,     5,     5,   207,     6,     5,     3,     5,   148,
     3,     5,     7,     5,     5,     5,     5,     5,     3,     5,
     3,     5,   190,     5,     5,     5,     5,   195,     5,     5,
     5,   170,   171,     5,     5,     5,     5,     5,     5,     5,
     5,     5,   181,     5,     5,     5,     5,     5,     5,     5,
     5,     5,   220,     3,     5,   225,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     0,   186,   173,   237,
    -1,    -1,    -1,    -1,    -1,    53,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,   222,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,   230,    -1,    -1,    -1,    -1,   235,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
    51,    52,    53,    54,    55,    56,    57,    58,    -1,    60
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 186 "parserib.y"
{
	init_array();
;
    break;}
case 3:
#line 192 "parserib.y"
{
	array_type = NUM_ARRAY;
;
    break;}
case 4:
#line 198 "parserib.y"
{
	array_type = STRING_ARRAY;
;
    break;}
case 5:
#line 204 "parserib.y"
{
	yyval.paramarray = yyvsp[0].paramarray;
;
    break;}
case 6:
#line 208 "parserib.y"
{
	yyval.paramarray = yyvsp[0].paramarray;
;
    break;}
case 7:
#line 212 "parserib.y"
{
	RtFloat *num;

	init_array();
	array_type = NUM_ARRAY;

	num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
	*num = (RtFloat)(yyvsp[0].num);
	add_array(num);

	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
;
    break;}
case 8:
#line 230 "parserib.y"
{
	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
;
    break;}
case 9:
#line 238 "parserib.y"
{
	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
;
    break;}
case 10:
#line 244 "parserib.y"
{
	RtToken str = (RtToken)strdup((char *)(yyvsp[0].string));
	add_array(str);

	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
;
    break;}
case 15:
#line 263 "parserib.y"
{
	RtToken str = (RtToken)strdup((char *)(yyvsp[0].string));
	add_array(str);
;
    break;}
case 16:
#line 270 "parserib.y"
{
	RtFloat *num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
	*num = (RtFloat)(yyvsp[0].num);
	add_array(num);
;
    break;}
case 17:
#line 276 "parserib.y"
{
	//printf("no elem\n");
;
    break;}
case 19:
#line 285 "parserib.y"
{
	rib_param_num = 0;
	curr_array_count = 0;
;
    break;}
case 22:
#line 296 "parserib.y"
{
	unsigned long i;
	unsigned long n;
	RtPointer arg;
	RtToken   tag;

	ri_ptr_array_t *t;

	tag = yyvsp[-1].string;
	t = yyvsp[0].paramarray;

	n = yyvsp[0].paramarray->nelems;
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
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(yyvsp[0].paramarray);
;
    break;}
case 25:
#line 351 "parserib.y"
{
	enter_mode_skip();
;
    break;}
case 26:
#line 357 "parserib.y"
{
	//printf("RIB Version: %f\n", $2 );
;
    break;}
case 27:
#line 361 "parserib.y"
{
	RiAttributeV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 28:
#line 365 "parserib.y"
{
	RiAreaLightSourceV((RtToken)yyvsp[-2].string,
			   rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 29:
#line 370 "parserib.y"
{
	RiAttributeBegin();
;
    break;}
case 30:
#line 374 "parserib.y"
{
	RiAttributeEnd();
;
    break;}
case 31:
#line 378 "parserib.y"
{
	RiClipping(yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 32:
#line 382 "parserib.y"
{
	int     i;
	RtColor col;
	ri_ptr_array_t *p;

	p = yyvsp[0].paramarray;

	/* currently, only consider 3 component color */
	if (p->nelems < 3) {
		ri_log(LOG_WARN, "RiColor() with compnent < 3");
	} else {
		for (i = 0; i < 3; i++) {
			col[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiColor(col);
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
;
    break;}
case 33:
#line 411 "parserib.y"
{
	RtMatrix mat;

	mat[0][0] = yyvsp[-16].num ; mat[0][1] = yyvsp[-15].num ; mat[0][2] = yyvsp[-14].num ; mat[0][3] = yyvsp[-13].num ;
	mat[1][0] = yyvsp[-12].num ; mat[1][1] = yyvsp[-11].num ; mat[1][2] = yyvsp[-10].num ; mat[1][3] = yyvsp[-9].num;
	mat[2][0] = yyvsp[-8].num; mat[2][1] = yyvsp[-7].num; mat[2][2] = yyvsp[-6].num; mat[2][3] = yyvsp[-5].num;
	mat[3][0] = yyvsp[-4].num; mat[3][1] = yyvsp[-3].num; mat[3][2] = yyvsp[-2].num; mat[3][3] = yyvsp[-1].num;

	RiConcatTransform(mat);

;
    break;}
case 34:
#line 423 "parserib.y"
{
	RiCoordinateSystem((RtToken)yyvsp[0].string);
;
    break;}
case 35:
#line 427 "parserib.y"
{
	RiDeclare(yyvsp[-1].string, yyvsp[0].string);
;
    break;}
case 36:
#line 431 "parserib.y"
{
	RiDepthOfField((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
;
    break;}
case 37:
#line 435 "parserib.y"
{
	RiDisplayV(yyvsp[-3].string, yyvsp[-2].string, yyvsp[-1].string,
		   rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 38:
#line 440 "parserib.y"
{
	RiExposure(yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 39:
#line 444 "parserib.y"
{
	RiFormat(yyvsp[-2].num, yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 40:
#line 448 "parserib.y"
{
	RiFrameBegin(yyvsp[0].num);
;
    break;}
case 41:
#line 452 "parserib.y"
{
	RiFrameEnd();

;
    break;}
case 42:
#line 457 "parserib.y"
{
	RiFrameAspectRatio(yyvsp[0].num);

;
    break;}
case 43:
#line 462 "parserib.y"
{
	RiHiderV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 44:
#line 466 "parserib.y"
{
	RiIdentity();
;
    break;}
case 45:
#line 470 "parserib.y"
{
	RtInt lightid;

	lightid = (RtInt)yyvsp[-1].num;

	RiIlluminate((RtLightHandle)&(lightid), yyvsp[0].num);
;
    break;}
case 46:
#line 478 "parserib.y"
{
	RiImagerV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 47:
#line 482 "parserib.y"
{
	RiLightSourceV(yyvsp[-2].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 48:
#line 486 "parserib.y"
{ 
	unsigned int    i;
	RtFloat        *times;
	ri_ptr_array_t *p;

	if (array_type != NUM_ARRAY) {
		ri_log(LOG_WARN, "MotionBegin: not a float array argument.\n");
	} else {
		p = yyvsp[0].paramarray;

		times = (RtFloat *)ri_mem_alloc(sizeof(RtFloat) * p->nelems);

		for (i = 0; i < p->nelems; i++) {
			times[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiMotionBeginV(p->nelems, times);

		ri_ptr_array_traverse(p, numptrfree);
		ri_ptr_array_free(p);
	}
;
    break;}
case 49:
#line 509 "parserib.y"
{
	RiMotionEnd();
;
    break;}
case 50:
#line 513 "parserib.y"
{
	int     i;
	RtColor opa;
	ri_ptr_array_t *p;

	p = yyvsp[0].paramarray;

	/* currently, only consider 3 component color */
	if (p->nelems < 3) {
		ri_log(LOG_WARN, "RiOpacity() with compnent < 3");
	} else {
		for (i = 0; i < 3; i++) {
			opa[i] = *((RtFloat *)ri_ptr_array_at(p, i));
		}

		RiOpacity(opa);
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
;
    break;}
case 51:
#line 539 "parserib.y"
{
	RiOptionV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 52:
#line 543 "parserib.y"
{
	RiOrientation((RtToken)yyvsp[0].string);
;
    break;}
case 53:
#line 547 "parserib.y"
{
	RiPerspective(yyvsp[0].num);
;
    break;}
case 54:
#line 551 "parserib.y"
{
	RiPixelFilter(RiBoxFilter, yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 55:
#line 555 "parserib.y"
{
	RiPixelSamples(yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 56:
#line 559 "parserib.y"
{
	int             i;
	int             have_p = 0;
	RtInt           npolys, *nverts, *verts;
	ri_ptr_array_t *p;

	p = yyvsp[-2].paramarray;
	npolys = p->nelems;
	
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = yyvsp[-1].paramarray;
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
		ri_ptr_array_traverse(yyvsp[-2].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[-2].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);

	RiPointsPolygonsV(npolys, nverts, verts,
			  rib_param_num, rib_param_tokens, rib_param_args);

;
    break;}
case 57:
#line 601 "parserib.y"
{
	int             i;
	int             docall = 1;
	int             have_p = 0;
	int             loop_warn = 0;
	RtInt           loopsize;
	RtInt           npolys, *nloops, *nverts, *verts;
	ri_ptr_array_t *p;

	p = yyvsp[-3].paramarray;
	npolys = p->nelems;
	
	nloops = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nloops[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}
	loopsize = p->nelems;

	p = yyvsp[-2].paramarray;
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = yyvsp[-1].paramarray;
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
;
    break;}
case 58:
#line 657 "parserib.y"
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
		ri_log(LOG_WARN, "Polygon call with invalid parameter");
	}
;
    break;}
case 59:
#line 691 "parserib.y"
{
	RiProjectionV((RtToken)yyvsp[-1].string,
		      rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 60:
#line 696 "parserib.y"
{
	RiQuantize((RtToken)yyvsp[-4].string, yyvsp[-3].num, yyvsp[-2].num, yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 61:
#line 700 "parserib.y"
{
	RiRotate((RtFloat)yyvsp[-3].num, (RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
;
    break;}
case 62:
#line 704 "parserib.y"
{
	RiScale((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
;
    break;}
case 63:
#line 708 "parserib.y"
{
	RiScreenWindow((RtFloat)yyvsp[-3].num, (RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
;
    break;}
case 64:
#line 712 "parserib.y"
{
	RiSphereV((RtFloat)yyvsp[-4].num, (RtFloat)yyvsp[-3].num, (RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num,
		  rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 65:
#line 717 "parserib.y"
{
	RiSides((RtFloat)yyvsp[0].num);
;
    break;}
case 66:
#line 721 "parserib.y"
{
	RiShadingInterpolation((RtToken)yyvsp[0].string);
;
    break;}
case 67:
#line 725 "parserib.y"
{
	RiShadingRate(yyvsp[0].num);
;
    break;}
case 68:
#line 729 "parserib.y"
{
	RiShutter(yyvsp[-1].num, yyvsp[0].num);
;
    break;}
case 69:
#line 733 "parserib.y"
{
	int             i;
	int             docall = 1;
	int             have_p = 0;
	int             face_warn = 0;
	RtInt           nfaces, *nverts, *verts;
	ri_ptr_array_t *p;

	p = yyvsp[-6].paramarray;
	nfaces = p->nelems;
	
	nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * nfaces);
	for (i = 0; i < (int)p->nelems; i++) {
		nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
	}

	p = yyvsp[-5].paramarray;
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
		RiSubdivisionMeshV(yyvsp[-7].string, nfaces, nverts, verts,
				   0,			/* ntags	*/
				   NULL,		/* tags		*/
				   NULL,		/* nargs	*/
				   NULL,		/* intargs	*/
				   NULL,		/* floatargs	*/
				   rib_param_num,
				   rib_param_tokens, rib_param_args);
	}
	
;
    break;}
case 70:
#line 787 "parserib.y"
{
	RiSurfaceV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
;
    break;}
case 71:
#line 791 "parserib.y"
{
	int     i, j;
	RtMatrix matrix;
	ri_ptr_array_t *p;
	
	p = yyvsp[0].paramarray;
	if (p->nelems == 16) {
		for (i = 0; i < 4; i++) {
			for (j = 0; j < 4; j++) {
				matrix[i][j] = (*((RtFloat *)ri_ptr_array_at(p, i * 4 + j)));
			}
		}

		RiTransform(matrix);
	} else {	
		ri_log(LOG_WARN, "nelems != 16");
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
;
    break;}
case 72:
#line 817 "parserib.y"
{
	RiTransformBegin();
;
    break;}
case 73:
#line 821 "parserib.y"
{
	RiTransformEnd();
;
    break;}
case 74:
#line 825 "parserib.y"
{
	RiTranslate((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
;
    break;}
case 75:
#line 829 "parserib.y"
{
	RiWorldBegin();
;
    break;}
case 76:
#line 833 "parserib.y"
{
	RiWorldEnd();
;
    break;}
case 77:
#line 837 "parserib.y"
{
	enter_mode_skip();
	printf("[RIB parse] Unknown command: %s at line %d\n", yylval.string, line_num);
	nunknown_commands++;
	if (nunknown_commands > max_unknown_commands) {
		printf("[RIB parse] Too many unknown commands. Give up parsing.\n");
		exit(-1);
	}
;
    break;}
case 78:
#line 848 "parserib.y"
{ enter_mode_param();	;
    break;}
case 79:
#line 850 "parserib.y"
{ enter_mode_param();	;
    break;}
case 80:
#line 852 "parserib.y"
{ enter_mode_param();	;
    break;}
case 81:
#line 854 "parserib.y"
{ enter_mode_param();	;
    break;}
case 82:
#line 856 "parserib.y"
{ enter_mode_param();	;
    break;}
case 83:
#line 858 "parserib.y"
{ enter_mode_param();	;
    break;}
case 84:
#line 860 "parserib.y"
{ enter_mode_param();	;
    break;}
case 85:
#line 862 "parserib.y"
{ enter_mode_param();	;
    break;}
case 86:
#line 864 "parserib.y"
{ enter_mode_param();	;
    break;}
case 87:
#line 866 "parserib.y"
{ enter_mode_param();	;
    break;}
case 88:
#line 868 "parserib.y"
{ enter_mode_param(); 	;
    break;}
case 89:
#line 870 "parserib.y"
{ enter_mode_param();	;
    break;}
case 90:
#line 872 "parserib.y"
{ enter_mode_param();	;
    break;}
case 91:
#line 874 "parserib.y"
{ enter_mode_param();	;
    break;}
case 92:
#line 876 "parserib.y"
{ enter_mode_param();	;
    break;}
case 93:
#line 878 "parserib.y"
{ enter_mode_param();	;
    break;}
case 94:
#line 880 "parserib.y"
{ enter_mode_param();	;
    break;}
case 95:
#line 882 "parserib.y"
{ enter_mode_param();	;
    break;}
case 96:
#line 884 "parserib.y"
{ enter_mode_param();	;
    break;}
case 97:
#line 886 "parserib.y"
{ enter_mode_param();	;
    break;}
case 98:
#line 888 "parserib.y"
{ enter_mode_param();	;
    break;}
case 99:
#line 890 "parserib.y"
{ enter_mode_param();	;
    break;}
case 100:
#line 892 "parserib.y"
{ enter_mode_param();	;
    break;}
case 101:
#line 894 "parserib.y"
{ enter_mode_param();	;
    break;}
case 102:
#line 896 "parserib.y"
{ enter_mode_param();	;
    break;}
case 103:
#line 898 "parserib.y"
{ enter_mode_param();	;
    break;}
case 104:
#line 900 "parserib.y"
{ enter_mode_param();	;
    break;}
case 105:
#line 902 "parserib.y"
{ enter_mode_param();	;
    break;}
case 106:
#line 904 "parserib.y"
{ enter_mode_param();	;
    break;}
case 107:
#line 906 "parserib.y"
{ enter_mode_param();	;
    break;}
case 108:
#line 908 "parserib.y"
{ enter_mode_param();	;
    break;}
case 109:
#line 910 "parserib.y"
{ enter_mode_param();	;
    break;}
case 110:
#line 912 "parserib.y"
{ enter_mode_param();	;
    break;}
case 111:
#line 914 "parserib.y"
{ enter_mode_param();	;
    break;}
case 112:
#line 916 "parserib.y"
{ enter_mode_param();	;
    break;}
case 113:
#line 918 "parserib.y"
{ enter_mode_param();	;
    break;}
case 114:
#line 920 "parserib.y"
{ enter_mode_param();	;
    break;}
case 115:
#line 922 "parserib.y"
{ enter_mode_param();	;
    break;}
case 116:
#line 924 "parserib.y"
{ enter_mode_param();	;
    break;}
case 117:
#line 926 "parserib.y"
{ enter_mode_param();	;
    break;}
case 118:
#line 928 "parserib.y"
{ enter_mode_param(); ;
    break;}
case 119:
#line 930 "parserib.y"
{ enter_mode_param();	;
    break;}
case 120:
#line 932 "parserib.y"
{ enter_mode_param();	;
    break;}
case 121:
#line 934 "parserib.y"
{ enter_mode_param();	;
    break;}
case 122:
#line 936 "parserib.y"
{ enter_mode_param();	;
    break;}
case 123:
#line 938 "parserib.y"
{ enter_mode_param();	;
    break;}
case 124:
#line 940 "parserib.y"
{ enter_mode_param();	;
    break;}
case 125:
#line 942 "parserib.y"
{ enter_mode_param();	;
    break;}
case 126:
#line 944 "parserib.y"
{ enter_mode_param();	;
    break;}
case 127:
#line 946 "parserib.y"
{ enter_mode_param();	;
    break;}
case 128:
#line 948 "parserib.y"
{ enter_mode_param();	;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 951 "parserib.y"

