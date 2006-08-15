/* A Bison parser, made by GNU Bison 1.875d.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     STRING = 258,
     ID = 259,
     NUM = 260,
     LBRACKET = 261,
     RBRACKET = 262,
     AREALIGHTSOURCE = 263,
     ATTRIBUTE = 264,
     ATTRIBUTEBEGIN = 265,
     ATTRIBUTEEND = 266,
     CLIPPING = 267,
     CONCATTRANSFORM = 268,
     COLOR = 269,
     COORDINATESYSTEM = 270,
     DECLARE = 271,
     DEPTHOFFIELD = 272,
     DISPLAY = 273,
     EXPOSURE = 274,
     FORMAT = 275,
     FRAMEBEGIN = 276,
     FRAMEEND = 277,
     FRAMEASPECTRATIO = 278,
     HIDER = 279,
     IDENTITY = 280,
     ILLUMINATE = 281,
     IMAGER = 282,
     LIGHTSOURCE = 283,
     MOTIONBEGIN = 284,
     MOTIONEND = 285,
     OPACITY = 286,
     OPTION = 287,
     ORIENTATION = 288,
     PERSPECTIVE = 289,
     PIXELFILTER = 290,
     PIXELSAMPLES = 291,
     POINTSPOLYGONS = 292,
     POINTSGENERALPOLYGONS = 293,
     PROJECTION = 294,
     POLYGON = 295,
     QUANTIZE = 296,
     ROTATE = 297,
     RIBVERSION = 298,
     SCALE = 299,
     SHADINGRATE = 300,
     SHADINGINTERPOLATION = 301,
     SHUTTER = 302,
     SIDES = 303,
     SPHERE = 304,
     SUBDIVISIONMESH = 305,
     SURFACE = 306,
     TRANSFORM = 307,
     TRANSFORMBEGIN = 308,
     TRANSFORMEND = 309,
     TRANSLATE = 310,
     WORLDBEGIN = 311,
     WORLDEND = 312,
     HIGH_PRECEDENCE = 313,
     UNKNOWN = 314
   };
#endif
#define STRING 258
#define ID 259
#define NUM 260
#define LBRACKET 261
#define RBRACKET 262
#define AREALIGHTSOURCE 263
#define ATTRIBUTE 264
#define ATTRIBUTEBEGIN 265
#define ATTRIBUTEEND 266
#define CLIPPING 267
#define CONCATTRANSFORM 268
#define COLOR 269
#define COORDINATESYSTEM 270
#define DECLARE 271
#define DEPTHOFFIELD 272
#define DISPLAY 273
#define EXPOSURE 274
#define FORMAT 275
#define FRAMEBEGIN 276
#define FRAMEEND 277
#define FRAMEASPECTRATIO 278
#define HIDER 279
#define IDENTITY 280
#define ILLUMINATE 281
#define IMAGER 282
#define LIGHTSOURCE 283
#define MOTIONBEGIN 284
#define MOTIONEND 285
#define OPACITY 286
#define OPTION 287
#define ORIENTATION 288
#define PERSPECTIVE 289
#define PIXELFILTER 290
#define PIXELSAMPLES 291
#define POINTSPOLYGONS 292
#define POINTSGENERALPOLYGONS 293
#define PROJECTION 294
#define POLYGON 295
#define QUANTIZE 296
#define ROTATE 297
#define RIBVERSION 298
#define SCALE 299
#define SHADINGRATE 300
#define SHADINGINTERPOLATION 301
#define SHUTTER 302
#define SIDES 303
#define SPHERE 304
#define SUBDIVISIONMESH 305
#define SURFACE 306
#define TRANSFORM 307
#define TRANSFORMBEGIN 308
#define TRANSFORMEND 309
#define TRANSLATE 310
#define WORLDBEGIN 311
#define WORLDEND 312
#define HIGH_PRECEDENCE 313
#define UNKNOWN 314




/* Copy the first part of user declarations.  */
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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 117 "parserib.y"
typedef union YYSTYPE {
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 316 "parserib.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 328 "parserib.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short int yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short int) + sizeof (YYSTYPE))			\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short int yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  106
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   205

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  60
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  69
/* YYNRULES -- Number of rules. */
#define YYNRULES  127
/* YYNRULES -- Number of states. */
#define YYNSTATES  246

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short int yyprhs[] =
{
       0,     0,     3,     5,     6,     7,     8,    10,    12,    14,
      19,    24,    27,    30,    32,    35,    37,    40,    43,    44,
      47,    48,    51,    52,    55,    58,    60,    62,    65,    69,
      74,    76,    78,    82,    85,   105,   108,   112,   117,   123,
     127,   132,   135,   137,   140,   144,   146,   150,   154,   159,
     162,   164,   167,   171,   174,   177,   182,   186,   191,   197,
     200,   204,   211,   217,   222,   229,   232,   235,   238,   242,
     252,   256,   259,   261,   263,   268,   270,   272,   274,   276,
     278,   280,   282,   284,   286,   288,   290,   292,   294,   296,
     298,   300,   302,   304,   306,   308,   310,   312,   314,   316,
     318,   320,   322,   324,   326,   328,   330,   332,   334,   336,
     338,   340,   342,   344,   346,   348,   350,   352,   354,   356,
     358,   360,   362,   364,   366,   368,   370,   372
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short int yyrhs[] =
{
      61,     0,    -1,    76,    -1,    -1,    -1,    -1,    66,    -1,
      67,    -1,     5,    -1,    62,     6,    68,     7,    -1,    62,
       6,    69,     7,    -1,    62,     3,    -1,    68,    71,    -1,
      71,    -1,    69,    70,    -1,    70,    -1,    64,     3,    -1,
      63,     5,    -1,    -1,    73,    74,    -1,    -1,    75,    74,
      -1,    -1,     3,    65,    -1,    76,    77,    -1,    77,    -1,
      78,    -1,    79,     5,    -1,    81,     3,    72,    -1,    80,
       3,     5,    72,    -1,    82,    -1,    83,    -1,    84,     5,
       5,    -1,    85,    66,    -1,    86,     6,     5,     5,     5,
       5,     5,     5,     5,     5,     5,     5,     5,     5,     5,
       5,     5,     5,     7,    -1,    87,     3,    -1,    88,     3,
       3,    -1,    89,     5,     5,     5,    -1,    90,     3,     3,
       3,    72,    -1,    91,     5,     5,    -1,    95,     5,     5,
       5,    -1,    92,     5,    -1,    93,    -1,    94,     5,    -1,
      96,     3,    72,    -1,    97,    -1,    98,     5,     5,    -1,
      99,     3,    72,    -1,   100,     3,     5,    72,    -1,   101,
      66,    -1,   102,    -1,   103,    66,    -1,   104,     3,    72,
      -1,   105,     3,    -1,   106,     5,    -1,   107,     3,     5,
       5,    -1,   108,     5,     5,    -1,   109,    66,    66,    72,
      -1,   110,    66,    66,    66,    72,    -1,   111,    72,    -1,
     112,     3,    72,    -1,   113,     3,     5,     5,     5,     5,
      -1,   114,     5,     5,     5,     5,    -1,   115,     5,     5,
       5,    -1,   116,     5,     5,     5,     5,    72,    -1,   117,
       5,    -1,   118,     3,    -1,   119,     5,    -1,   120,     5,
       5,    -1,   121,     3,    66,    66,    67,    66,    66,    66,
      72,    -1,   122,     3,    72,    -1,   123,    66,    -1,   124,
      -1,   125,    -1,   126,     5,     5,     5,    -1,   127,    -1,
     128,    -1,    59,    -1,    43,    -1,     8,    -1,     9,    -1,
      10,    -1,    11,    -1,    12,    -1,    14,    -1,    13,    -1,
      15,    -1,    16,    -1,    17,    -1,    18,    -1,    19,    -1,
      21,    -1,    22,    -1,    23,    -1,    20,    -1,    24,    -1,
      25,    -1,    26,    -1,    27,    -1,    28,    -1,    29,    -1,
      30,    -1,    31,    -1,    32,    -1,    33,    -1,    34,    -1,
      35,    -1,    36,    -1,    37,    -1,    38,    -1,    40,    -1,
      39,    -1,    41,    -1,    42,    -1,    44,    -1,    49,    -1,
      48,    -1,    46,    -1,    45,    -1,    47,    -1,    50,    -1,
      51,    -1,    52,    -1,    53,    -1,    54,    -1,    55,    -1,
      56,    -1,    57,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   181,   181,   184,   190,   196,   202,   206,   210,   228,
     236,   242,   253,   254,   257,   258,   261,   268,   275,   280,
     283,   290,   291,   294,   345,   346,   349,   355,   359,   363,
     368,   372,   376,   380,   406,   421,   425,   429,   433,   438,
     442,   446,   450,   455,   460,   464,   468,   476,   480,   484,
     507,   511,   537,   541,   545,   549,   553,   557,   599,   655,
     689,   694,   698,   702,   706,   711,   715,   719,   723,   727,
     781,   785,   811,   815,   819,   823,   827,   831,   843,   845,
     847,   849,   851,   853,   855,   857,   859,   861,   863,   865,
     867,   869,   871,   873,   875,   877,   879,   881,   883,   885,
     887,   889,   891,   893,   895,   897,   899,   901,   903,   905,
     907,   909,   911,   913,   915,   917,   919,   921,   923,   925,
     927,   929,   931,   933,   935,   937,   939,   941
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "STRING", "ID", "NUM", "LBRACKET",
  "RBRACKET", "AREALIGHTSOURCE", "ATTRIBUTE", "ATTRIBUTEBEGIN",
  "ATTRIBUTEEND", "CLIPPING", "CONCATTRANSFORM", "COLOR",
  "COORDINATESYSTEM", "DECLARE", "DEPTHOFFIELD", "DISPLAY", "EXPOSURE",
  "FORMAT", "FRAMEBEGIN", "FRAMEEND", "FRAMEASPECTRATIO", "HIDER",
  "IDENTITY", "ILLUMINATE", "IMAGER", "LIGHTSOURCE", "MOTIONBEGIN",
  "MOTIONEND", "OPACITY", "OPTION", "ORIENTATION", "PERSPECTIVE",
  "PIXELFILTER", "PIXELSAMPLES", "POINTSPOLYGONS", "POINTSGENERALPOLYGONS",
  "PROJECTION", "POLYGON", "QUANTIZE", "ROTATE", "RIBVERSION", "SCALE",
  "SHADINGRATE", "SHADINGINTERPOLATION", "SHUTTER", "SIDES", "SPHERE",
  "SUBDIVISIONMESH", "SURFACE", "TRANSFORM", "TRANSFORMBEGIN",
  "TRANSFORMEND", "TRANSLATE", "WORLDBEGIN", "WORLDEND", "HIGH_PRECEDENCE",
  "UNKNOWN", "$accept", "start", "array_init", "num_list_init",
  "str_list_init", "param_array", "param_num_array", "param_str_array",
  "num_list", "str_list", "str_list_entry", "num_list_entry", "param_list",
  "param_list_init", "param_lists", "param_list_entry", "ri_command_list",
  "ri_command", "protocol", "ribversion", "arealightsource", "attribute",
  "attributebegin", "attributeend", "clipping", "color", "concattransform",
  "coordinatesystem", "declare", "depthoffield", "display", "exposure",
  "framebegin", "frameend", "frameaspectratio", "format", "hider",
  "identity", "illuminate", "imager", "lightsource", "motionbegin",
  "motionend", "opacity", "option", "orientation", "perspective",
  "pixelfilter", "pixelsamples", "pointspolygons", "pointsgeneralpolygons",
  "polygon", "projection", "quantize", "rotate", "scale", "sphere",
  "sides", "shadinginterpolation", "shadingrate", "shutter",
  "subdivisionmesh", "surface", "transform", "transformbegin",
  "transformend", "translate", "worldbegin", "worldend", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    60,    61,    62,    63,    64,    65,    65,    65,    66,
      67,    67,    68,    68,    69,    69,    70,    71,    71,    72,
      73,    74,    74,    75,    76,    76,    77,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     0,     0,     0,     1,     1,     1,     4,
       4,     2,     2,     1,     2,     1,     2,     2,     0,     2,
       0,     2,     0,     2,     2,     1,     1,     2,     3,     4,
       1,     1,     3,     2,    19,     2,     3,     4,     5,     3,
       4,     2,     1,     2,     3,     1,     3,     3,     4,     2,
       1,     2,     3,     2,     2,     4,     3,     4,     5,     2,
       3,     6,     5,     4,     6,     2,     2,     2,     3,     9,
       3,     2,     1,     1,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,    79,    80,    81,    82,    83,    85,    84,    86,    87,
      88,    89,    90,    94,    91,    92,    93,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   111,   110,   112,   113,    78,   114,   118,   117,
     119,   116,   115,   120,   121,   122,   123,   124,   125,   126,
     127,    77,     0,     2,    25,    26,     0,     0,     0,    30,
      31,     0,     3,     0,     0,     0,     0,     0,     0,     0,
      42,     0,     0,     0,    45,     0,     0,     0,     3,    50,
       3,     0,     0,     0,     0,     0,     3,     3,    20,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       3,    72,    73,     0,    75,    76,     1,    24,    27,     0,
      20,     0,     0,    33,     0,    35,     0,     0,     0,     0,
      41,    43,     0,    20,     0,    20,     0,    49,    51,    20,
      53,    54,     0,     0,     3,     3,    59,    22,    20,     0,
       0,     0,     0,    65,    66,    67,     0,     3,    20,    71,
       0,    20,    28,    32,     4,     0,    36,     0,     0,    39,
       0,    44,    46,    47,    20,    52,     0,    56,    20,     3,
       3,    19,    22,    60,     0,     0,     0,     0,    68,     3,
      70,     0,    29,     0,     4,    13,     0,    37,    20,    40,
      48,    55,    57,    20,     8,     0,    23,     6,     7,    21,
       0,     0,    63,     0,     3,    74,    17,     9,    12,     0,
      38,    58,    11,     4,     0,    62,    20,     0,     3,     0,
       0,     5,    15,    61,    64,     5,     3,     0,    16,    10,
      14,     3,     0,    20,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short int yydefgoto[] =
{
      -1,    52,   112,   183,   220,   196,   113,   198,   184,   221,
     222,   185,   136,   137,   171,   172,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -175
static const short int yypact[] =
{
     146,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,     5,   146,  -175,  -175,     6,    10,    14,  -175,
    -175,    13,  -175,    15,    16,    20,    19,    22,    21,    23,
    -175,    25,    26,    24,  -175,    27,    30,    31,  -175,  -175,
    -175,    32,    33,    35,    38,    39,  -175,  -175,  -175,    40,
      42,    41,    43,    44,    45,    48,    47,    49,    50,    55,
    -175,  -175,  -175,    56,  -175,  -175,  -175,  -175,  -175,    57,
    -175,    58,    54,  -175,    59,  -175,    62,    61,    64,    63,
    -175,  -175,    65,  -175,    66,  -175,    67,  -175,  -175,  -175,
    -175,  -175,    68,    69,  -175,  -175,  -175,    72,  -175,    71,
      73,    75,    76,  -175,  -175,  -175,    77,  -175,  -175,  -175,
      78,  -175,  -175,  -175,    70,    80,  -175,    81,    84,  -175,
      83,  -175,  -175,  -175,  -175,  -175,    85,  -175,  -175,  -175,
      88,  -175,    72,  -175,    89,    90,    91,    92,  -175,  -175,
    -175,    93,  -175,    94,    82,  -175,    95,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,     1,  -175,  -175,  -175,  -175,
      97,    98,  -175,    99,  -175,  -175,  -175,  -175,  -175,   100,
    -175,  -175,  -175,     3,   101,  -175,  -175,     9,  -175,   103,
     106,   104,  -175,  -175,  -175,  -175,  -175,   105,  -175,  -175,
    -175,  -175,   107,  -175,   108,  -175,   109,   110,   111,   112,
     113,   114,   115,   116,   118,  -175
};

/* YYPGOTO[NTERM-NUM].  */
static const short int yypgoto[] =
{
    -175,  -175,  -167,  -175,  -175,  -175,   -78,  -166,  -175,  -175,
    -174,   -62,  -109,  -175,   -49,  -175,  -175,    74,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,
    -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175,  -175
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -19
static const short int yytable[] =
{
     127,   152,   128,   195,   212,   106,    -5,   213,   134,   135,
     -18,   108,   212,   109,   161,   225,   163,   110,   111,   115,
     165,   114,   149,   116,   117,   118,   119,   123,   120,   173,
     121,   122,   124,   125,   126,   129,   130,   217,   218,   180,
     131,   132,   182,   138,   133,   139,   140,   230,   141,   142,
     143,   144,   145,   147,   146,   190,   168,   169,   148,   192,
     154,   150,   151,   153,   155,   156,   157,   158,   159,   179,
     160,   162,   164,   166,   167,   170,   174,   -18,   175,   210,
     176,   177,   178,   181,   211,   186,   187,   188,   189,   207,
     191,   193,   197,   194,   200,   201,   202,   203,   205,   206,
     209,   204,   214,   215,   216,   219,   223,   224,   227,   228,
     232,   229,   234,   236,   237,   238,   239,   240,   241,   242,
     243,   244,   208,   199,   235,   245,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,     0,     0,     0,     0,     0,   231,     0,
       0,     0,     0,   233,     1,     2,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,     0,    51
};

static const short int yycheck[] =
{
      78,   110,    80,   170,     3,     0,     3,     6,    86,    87,
       7,     5,     3,     3,   123,     6,   125,     3,     5,     3,
     129,     6,   100,     3,     5,     3,     5,     3,     5,   138,
       5,     5,     5,     3,     3,     3,     3,   204,   204,   148,
       5,     3,   151,     3,     5,     3,     5,   221,     5,     5,
       5,     3,     5,     3,     5,   164,   134,   135,     3,   168,
       6,     5,     5,     5,     5,     3,     5,     3,     5,   147,
       5,     5,     5,     5,     5,     3,     5,     7,     5,   188,
       5,     5,     5,     5,   193,     5,     5,     3,     5,     7,
       5,   169,   170,     5,     5,     5,     5,     5,     5,     5,
       5,   179,     5,     5,     5,     5,     5,   216,     5,     3,
       5,     7,     5,     5,     5,     5,     5,     5,     5,     5,
       5,     5,   184,   172,   233,     7,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   226,    -1,
      -1,    -1,    -1,   231,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    61,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,     0,    77,     5,     3,
       3,     5,    62,    66,     6,     3,     3,     5,     3,     5,
       5,     5,     5,     3,     5,     3,     3,    66,    66,     3,
       3,     5,     3,     5,    66,    66,    72,    73,     3,     3,
       5,     5,     5,     5,     3,     5,     5,     3,     3,    66,
       5,     5,    72,     5,     6,     5,     3,     5,     3,     5,
       5,    72,     5,    72,     5,    72,     5,     5,    66,    66,
       3,    74,    75,    72,     5,     5,     5,     5,     5,    66,
      72,     5,    72,    63,    68,    71,     5,     5,     3,     5,
      72,     5,    72,    66,     5,    62,    65,    66,    67,    74,
       5,     5,     5,     5,    66,     5,     5,     7,    71,     5,
      72,    72,     3,     6,     5,     5,     5,    62,    67,     5,
      64,    69,    70,     5,    72,     6,    66,     5,     3,     7,
      70,    66,     5,    66,     5,    72,     5,     5,     5,     5,
       5,     5,     5,     5,     5,     7
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short int *bottom, short int *top)
#else
static void
yy_stack_print (bottom, top)
    short int *bottom;
    short int *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short int yyssa[YYINITDEPTH];
  short int *yyss = yyssa;
  register short int *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;


  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short int *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short int *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 185 "parserib.y"
    {
	init_array();
}
    break;

  case 4:
#line 191 "parserib.y"
    {
	array_type = NUM_ARRAY;
}
    break;

  case 5:
#line 197 "parserib.y"
    {
	array_type = STRING_ARRAY;
}
    break;

  case 6:
#line 203 "parserib.y"
    {
	yyval.paramarray = yyvsp[0].paramarray;
}
    break;

  case 7:
#line 207 "parserib.y"
    {
	yyval.paramarray = yyvsp[0].paramarray;
}
    break;

  case 8:
#line 211 "parserib.y"
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
}
    break;

  case 9:
#line 229 "parserib.y"
    {
	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
    break;

  case 10:
#line 237 "parserib.y"
    {
	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
    break;

  case 11:
#line 243 "parserib.y"
    {
	RtToken str = (RtToken)strdup((char *)(yyvsp[0].string));
	add_array(str);

	yyval.paramarray = array_dup(curr_array);
	ri_ptr_array_free(curr_array);
	curr_array = NULL;
}
    break;

  case 16:
#line 262 "parserib.y"
    {
	RtToken str = (RtToken)strdup((char *)(yyvsp[0].string));
	add_array(str);
}
    break;

  case 17:
#line 269 "parserib.y"
    {
	RtFloat *num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
	*num = (RtFloat)(yyvsp[0].num);
	add_array(num);
}
    break;

  case 18:
#line 275 "parserib.y"
    {
	//printf("no elem\n");
}
    break;

  case 20:
#line 284 "parserib.y"
    {
	rib_param_num = 0;
	curr_array_count = 0;
}
    break;

  case 23:
#line 295 "parserib.y"
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
}
    break;

  case 26:
#line 350 "parserib.y"
    {
	enter_mode_skip();
}
    break;

  case 27:
#line 356 "parserib.y"
    {
	//printf("RIB Version: %f\n", $2 );
}
    break;

  case 28:
#line 360 "parserib.y"
    {
	RiAttributeV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 29:
#line 364 "parserib.y"
    {
	RiAreaLightSourceV((RtToken)yyvsp[-2].string,
			   rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 30:
#line 369 "parserib.y"
    {
	RiAttributeBegin();
}
    break;

  case 31:
#line 373 "parserib.y"
    {
	RiAttributeEnd();
}
    break;

  case 32:
#line 377 "parserib.y"
    {
	RiClipping(yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 33:
#line 381 "parserib.y"
    {
	int     i;
	RtColor col;
	ri_ptr_array_t *p;

	p = yyvsp[0].paramarray;

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
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
}
    break;

  case 34:
#line 410 "parserib.y"
    {
	RtMatrix mat;

	mat[0][0] = yyvsp[-16].num ; mat[0][1] = yyvsp[-15].num ; mat[0][2] = yyvsp[-14].num ; mat[0][3] = yyvsp[-13].num ;
	mat[1][0] = yyvsp[-12].num ; mat[1][1] = yyvsp[-11].num ; mat[1][2] = yyvsp[-10].num ; mat[1][3] = yyvsp[-9].num;
	mat[2][0] = yyvsp[-8].num; mat[2][1] = yyvsp[-7].num; mat[2][2] = yyvsp[-6].num; mat[2][3] = yyvsp[-5].num;
	mat[3][0] = yyvsp[-4].num; mat[3][1] = yyvsp[-3].num; mat[3][2] = yyvsp[-2].num; mat[3][3] = yyvsp[-1].num;

	RiConcatTransform(mat);

}
    break;

  case 35:
#line 422 "parserib.y"
    {
	RiCoordinateSystem((RtToken)yyvsp[0].string);
}
    break;

  case 36:
#line 426 "parserib.y"
    {
	RiDeclare(yyvsp[-1].string, yyvsp[0].string);
}
    break;

  case 37:
#line 430 "parserib.y"
    {
	RiDepthOfField((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
}
    break;

  case 38:
#line 434 "parserib.y"
    {
	RiDisplayV(yyvsp[-3].string, yyvsp[-2].string, yyvsp[-1].string,
		   rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 39:
#line 439 "parserib.y"
    {
	RiExposure(yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 40:
#line 443 "parserib.y"
    {
	RiFormat(yyvsp[-2].num, yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 41:
#line 447 "parserib.y"
    {
	RiFrameBegin(yyvsp[0].num);
}
    break;

  case 42:
#line 451 "parserib.y"
    {
	RiFrameEnd();

}
    break;

  case 43:
#line 456 "parserib.y"
    {
	RiFrameAspectRatio(yyvsp[0].num);

}
    break;

  case 44:
#line 461 "parserib.y"
    {
	RiHiderV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 45:
#line 465 "parserib.y"
    {
	RiIdentity();
}
    break;

  case 46:
#line 469 "parserib.y"
    {
	RtInt lightid;

	lightid = (RtInt)yyvsp[-1].num;

	RiIlluminate((RtLightHandle)&(lightid), yyvsp[0].num);
}
    break;

  case 47:
#line 477 "parserib.y"
    {
	RiImagerV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 48:
#line 481 "parserib.y"
    {
	RiLightSourceV(yyvsp[-2].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 49:
#line 485 "parserib.y"
    { 
	unsigned int    i;
	RtFloat        *times;
	ri_ptr_array_t *p;

	if (array_type != NUM_ARRAY) {
		ri_log("warning", "MotionBegin: not a float array argument.\n");
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
}
    break;

  case 50:
#line 508 "parserib.y"
    {
	RiMotionEnd();
}
    break;

  case 51:
#line 512 "parserib.y"
    {
	int     i;
	RtColor opa;
	ri_ptr_array_t *p;

	p = yyvsp[0].paramarray;

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
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
}
    break;

  case 52:
#line 538 "parserib.y"
    {
	RiOptionV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 53:
#line 542 "parserib.y"
    {
	RiOrientation((RtToken)yyvsp[0].string);
}
    break;

  case 54:
#line 546 "parserib.y"
    {
	RiPerspective(yyvsp[0].num);
}
    break;

  case 55:
#line 550 "parserib.y"
    {
	RiPixelFilter(RiBoxFilter, yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 56:
#line 554 "parserib.y"
    {
	RiPixelSamples(yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 57:
#line 558 "parserib.y"
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

}
    break;

  case 58:
#line 600 "parserib.y"
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
}
    break;

  case 59:
#line 656 "parserib.y"
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
    break;

  case 60:
#line 690 "parserib.y"
    {
	RiProjectionV((RtToken)yyvsp[-1].string,
		      rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 61:
#line 695 "parserib.y"
    {
	RiQuantize((RtToken)yyvsp[-4].string, yyvsp[-3].num, yyvsp[-2].num, yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 62:
#line 699 "parserib.y"
    {
	RiRotate((RtFloat)yyvsp[-3].num, (RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
}
    break;

  case 63:
#line 703 "parserib.y"
    {
	RiScale((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
}
    break;

  case 64:
#line 707 "parserib.y"
    {
	RiSphereV((RtFloat)yyvsp[-4].num, (RtFloat)yyvsp[-3].num, (RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num,
		  rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 65:
#line 712 "parserib.y"
    {
	RiSides((RtFloat)yyvsp[0].num);
}
    break;

  case 66:
#line 716 "parserib.y"
    {
	RiShadingInterpolation((RtToken)yyvsp[0].string);
}
    break;

  case 67:
#line 720 "parserib.y"
    {
	RiShadingRate(yyvsp[0].num);
}
    break;

  case 68:
#line 724 "parserib.y"
    {
	RiShutter(yyvsp[-1].num, yyvsp[0].num);
}
    break;

  case 69:
#line 728 "parserib.y"
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
	
}
    break;

  case 70:
#line 782 "parserib.y"
    {
	RiSurfaceV(yyvsp[-1].string, rib_param_num, rib_param_tokens, rib_param_args);
}
    break;

  case 71:
#line 786 "parserib.y"
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
		ri_log("warning", "nelems != 16");
	}

	if (array_type == NUM_ARRAY) {
		ri_ptr_array_traverse(yyvsp[0].paramarray, numptrfree);
	} else {
		ri_ptr_array_traverse(yyvsp[0].paramarray, strptrfree);
	}
	ri_ptr_array_free(p);
}
    break;

  case 72:
#line 812 "parserib.y"
    {
	RiTransformBegin();
}
    break;

  case 73:
#line 816 "parserib.y"
    {
	RiTransformEnd();
}
    break;

  case 74:
#line 820 "parserib.y"
    {
	RiTranslate((RtFloat)yyvsp[-2].num, (RtFloat)yyvsp[-1].num, (RtFloat)yyvsp[0].num);
}
    break;

  case 75:
#line 824 "parserib.y"
    {
	RiWorldBegin();
}
    break;

  case 76:
#line 828 "parserib.y"
    {
	RiWorldEnd();
}
    break;

  case 77:
#line 832 "parserib.y"
    {
	enter_mode_skip();
	printf("[RIB parse] Unknown command: %s at line %d\n", yylval.string, line_num);
	nunknown_commands++;
	if (nunknown_commands > max_unknown_commands) {
		printf("[RIB parse] Too many unknown commands. Give up parsing.\n");
		exit(-1);
	}
}
    break;

  case 78:
#line 843 "parserib.y"
    { enter_mode_param();	}
    break;

  case 79:
#line 845 "parserib.y"
    { enter_mode_param();	}
    break;

  case 80:
#line 847 "parserib.y"
    { enter_mode_param();	}
    break;

  case 81:
#line 849 "parserib.y"
    { enter_mode_param();	}
    break;

  case 82:
#line 851 "parserib.y"
    { enter_mode_param();	}
    break;

  case 83:
#line 853 "parserib.y"
    { enter_mode_param();	}
    break;

  case 84:
#line 855 "parserib.y"
    { enter_mode_param();	}
    break;

  case 85:
#line 857 "parserib.y"
    { enter_mode_param();	}
    break;

  case 86:
#line 859 "parserib.y"
    { enter_mode_param();	}
    break;

  case 87:
#line 861 "parserib.y"
    { enter_mode_param();	}
    break;

  case 88:
#line 863 "parserib.y"
    { enter_mode_param(); 	}
    break;

  case 89:
#line 865 "parserib.y"
    { enter_mode_param();	}
    break;

  case 90:
#line 867 "parserib.y"
    { enter_mode_param();	}
    break;

  case 91:
#line 869 "parserib.y"
    { enter_mode_param();	}
    break;

  case 92:
#line 871 "parserib.y"
    { enter_mode_param();	}
    break;

  case 93:
#line 873 "parserib.y"
    { enter_mode_param();	}
    break;

  case 94:
#line 875 "parserib.y"
    { enter_mode_param();	}
    break;

  case 95:
#line 877 "parserib.y"
    { enter_mode_param();	}
    break;

  case 96:
#line 879 "parserib.y"
    { enter_mode_param();	}
    break;

  case 97:
#line 881 "parserib.y"
    { enter_mode_param();	}
    break;

  case 98:
#line 883 "parserib.y"
    { enter_mode_param();	}
    break;

  case 99:
#line 885 "parserib.y"
    { enter_mode_param();	}
    break;

  case 100:
#line 887 "parserib.y"
    { enter_mode_param();	}
    break;

  case 101:
#line 889 "parserib.y"
    { enter_mode_param();	}
    break;

  case 102:
#line 891 "parserib.y"
    { enter_mode_param();	}
    break;

  case 103:
#line 893 "parserib.y"
    { enter_mode_param();	}
    break;

  case 104:
#line 895 "parserib.y"
    { enter_mode_param();	}
    break;

  case 105:
#line 897 "parserib.y"
    { enter_mode_param();	}
    break;

  case 106:
#line 899 "parserib.y"
    { enter_mode_param();	}
    break;

  case 107:
#line 901 "parserib.y"
    { enter_mode_param();	}
    break;

  case 108:
#line 903 "parserib.y"
    { enter_mode_param();	}
    break;

  case 109:
#line 905 "parserib.y"
    { enter_mode_param();	}
    break;

  case 110:
#line 907 "parserib.y"
    { enter_mode_param();	}
    break;

  case 111:
#line 909 "parserib.y"
    { enter_mode_param();	}
    break;

  case 112:
#line 911 "parserib.y"
    { enter_mode_param();	}
    break;

  case 113:
#line 913 "parserib.y"
    { enter_mode_param();	}
    break;

  case 114:
#line 915 "parserib.y"
    { enter_mode_param();	}
    break;

  case 115:
#line 917 "parserib.y"
    { enter_mode_param();	}
    break;

  case 116:
#line 919 "parserib.y"
    { enter_mode_param();	}
    break;

  case 117:
#line 921 "parserib.y"
    { enter_mode_param(); }
    break;

  case 118:
#line 923 "parserib.y"
    { enter_mode_param();	}
    break;

  case 119:
#line 925 "parserib.y"
    { enter_mode_param();	}
    break;

  case 120:
#line 927 "parserib.y"
    { enter_mode_param();	}
    break;

  case 121:
#line 929 "parserib.y"
    { enter_mode_param();	}
    break;

  case 122:
#line 931 "parserib.y"
    { enter_mode_param();	}
    break;

  case 123:
#line 933 "parserib.y"
    { enter_mode_param();	}
    break;

  case 124:
#line 935 "parserib.y"
    { enter_mode_param();	}
    break;

  case 125:
#line 937 "parserib.y"
    { enter_mode_param();	}
    break;

  case 126:
#line 939 "parserib.y"
    { enter_mode_param();	}
    break;

  case 127:
#line 941 "parserib.y"
    { enter_mode_param();	}
    break;


    }

/* Line 1010 of yacc.c.  */
#line 2501 "parserib.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {
		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
	  yychar = YYEMPTY;

	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

  yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 944 "parserib.y"


