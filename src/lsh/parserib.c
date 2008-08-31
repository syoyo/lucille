/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

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
     ATMOSPHERE = 267,
     CLIPPING = 268,
     CONCATTRANSFORM = 269,
     COLOR = 270,
     COORDINATESYSTEM = 271,
     DECLARE = 272,
     DEPTHOFFIELD = 273,
     DISPLACEMENT = 274,
     DISPLAY = 275,
     EXPOSURE = 276,
     FORMAT = 277,
     FRAMEBEGIN = 278,
     FRAMEEND = 279,
     FRAMEASPECTRATIO = 280,
     HIDER = 281,
     IDENTITY = 282,
     ILLUMINATE = 283,
     IMAGER = 284,
     LIGHTSOURCE = 285,
     MOTIONBEGIN = 286,
     MOTIONEND = 287,
     OPACITY = 288,
     OPTION = 289,
     ORIENTATION = 290,
     PERSPECTIVE = 291,
     PIXELFILTER = 292,
     PIXELSAMPLES = 293,
     POINTSPOLYGONS = 294,
     POINTSGENERALPOLYGONS = 295,
     PROJECTION = 296,
     POLYGON = 297,
     QUANTIZE = 298,
     ROTATE = 299,
     RIBVERSION = 300,
     SCALE = 301,
     SCREENWINDOW = 302,
     SHADINGRATE = 303,
     SHADINGINTERPOLATION = 304,
     SHUTTER = 305,
     SIDES = 306,
     SPHERE = 307,
     SUBDIVISIONMESH = 308,
     SURFACE = 309,
     TRANSFORM = 310,
     TRANSFORMBEGIN = 311,
     TRANSFORMEND = 312,
     TRANSLATE = 313,
     WORLDBEGIN = 314,
     WORLDEND = 315,
     HIGH_PRECEDENCE = 316,
     UNKNOWN = 317
   };
#endif
/* Tokens.  */
#define STRING 258
#define ID 259
#define NUM 260
#define LBRACKET 261
#define RBRACKET 262
#define AREALIGHTSOURCE 263
#define ATTRIBUTE 264
#define ATTRIBUTEBEGIN 265
#define ATTRIBUTEEND 266
#define ATMOSPHERE 267
#define CLIPPING 268
#define CONCATTRANSFORM 269
#define COLOR 270
#define COORDINATESYSTEM 271
#define DECLARE 272
#define DEPTHOFFIELD 273
#define DISPLACEMENT 274
#define DISPLAY 275
#define EXPOSURE 276
#define FORMAT 277
#define FRAMEBEGIN 278
#define FRAMEEND 279
#define FRAMEASPECTRATIO 280
#define HIDER 281
#define IDENTITY 282
#define ILLUMINATE 283
#define IMAGER 284
#define LIGHTSOURCE 285
#define MOTIONBEGIN 286
#define MOTIONEND 287
#define OPACITY 288
#define OPTION 289
#define ORIENTATION 290
#define PERSPECTIVE 291
#define PIXELFILTER 292
#define PIXELSAMPLES 293
#define POINTSPOLYGONS 294
#define POINTSGENERALPOLYGONS 295
#define PROJECTION 296
#define POLYGON 297
#define QUANTIZE 298
#define ROTATE 299
#define RIBVERSION 300
#define SCALE 301
#define SCREENWINDOW 302
#define SHADINGRATE 303
#define SHADINGINTERPOLATION 304
#define SHUTTER 305
#define SIDES 306
#define SPHERE 307
#define SUBDIVISIONMESH 308
#define SURFACE 309
#define TRANSFORM 310
#define TRANSFORMBEGIN 311
#define TRANSFORMEND 312
#define TRANSLATE 313
#define WORLDBEGIN 314
#define WORLDEND 315
#define HIGH_PRECEDENCE 316
#define UNKNOWN 317




/* Copy the first part of user declarations.  */
#line 1 "src/lsh/parserib.y"

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

static const int    max_unknown_commands = 1000;
static int        nunknown_commands = 0;

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

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 117 "src/lsh/parserib.y"
{
char            string[1024];
float           num;
ri_ptr_array_t *paramarray;
}
/* Line 193 of yacc.c.  */
#line 342 "src/lsh/parserib.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 355 "src/lsh/parserib.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
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
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  112
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   217

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  63
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  72
/* YYNRULES -- Number of rules.  */
#define YYNRULES  133
/* YYNRULES -- Number of states.  */
#define YYNSTATES  260

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   317

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
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
      55,    56,    57,    58,    59,    60,    61,    62
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     6,     7,     8,    10,    12,    14,
      19,    24,    27,    30,    32,    35,    37,    40,    43,    44,
      47,    48,    51,    52,    55,    58,    60,    62,    65,    69,
      74,    76,    78,    82,    86,    89,   109,   112,   116,   121,
     125,   131,   135,   140,   143,   145,   148,   152,   154,   158,
     162,   167,   170,   172,   175,   179,   182,   185,   190,   194,
     199,   205,   208,   212,   219,   225,   230,   236,   243,   246,
     249,   252,   256,   266,   270,   273,   275,   277,   282,   284,
     286,   288,   290,   292,   294,   296,   298,   300,   302,   304,
     306,   308,   310,   312,   314,   316,   318,   320,   322,   324,
     326,   328,   330,   332,   334,   336,   338,   340,   342,   344,
     346,   348,   350,   352,   354,   356,   358,   360,   362,   364,
     366,   368,   370,   372,   374,   376,   378,   380,   382,   384,
     386,   388,   390,   392
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      64,     0,    -1,    79,    -1,    -1,    -1,    -1,    69,    -1,
      70,    -1,     5,    -1,    65,     6,    71,     7,    -1,    65,
       6,    72,     7,    -1,    65,     3,    -1,    71,    74,    -1,
      74,    -1,    72,    73,    -1,    73,    -1,    67,     3,    -1,
      66,     5,    -1,    -1,    76,    77,    -1,    -1,    78,    77,
      -1,    -1,     3,    68,    -1,    79,    80,    -1,    80,    -1,
      81,    -1,    82,     5,    -1,    84,     3,    75,    -1,    83,
       3,     5,    75,    -1,    85,    -1,    86,    -1,    87,     3,
      75,    -1,    88,     5,     5,    -1,    89,    69,    -1,    90,
       6,     5,     5,     5,     5,     5,     5,     5,     5,     5,
       5,     5,     5,     5,     5,     5,     5,     7,    -1,    91,
       3,    -1,    92,     3,     3,    -1,    93,     5,     5,     5,
      -1,    94,     3,    75,    -1,    95,     3,     3,     3,    75,
      -1,    96,     5,     5,    -1,   100,     5,     5,     5,    -1,
      97,     5,    -1,    98,    -1,    99,     5,    -1,   101,     3,
      75,    -1,   102,    -1,   103,     5,     5,    -1,   104,     3,
      75,    -1,   105,     3,     5,    75,    -1,   106,    69,    -1,
     107,    -1,   108,    69,    -1,   109,     3,    75,    -1,   110,
       3,    -1,   111,     5,    -1,   112,     3,     5,     5,    -1,
     113,     5,     5,    -1,   114,    69,    69,    75,    -1,   115,
      69,    69,    69,    75,    -1,   116,    75,    -1,   117,     3,
      75,    -1,   118,     3,     5,     5,     5,     5,    -1,   119,
       5,     5,     5,     5,    -1,   120,     5,     5,     5,    -1,
     121,     5,     5,     5,     5,    -1,   122,     5,     5,     5,
       5,    75,    -1,   123,     5,    -1,   124,     3,    -1,   125,
       5,    -1,   126,     5,     5,    -1,   127,     3,    69,    69,
      70,    69,    69,    69,    75,    -1,   128,     3,    75,    -1,
     129,    69,    -1,   130,    -1,   131,    -1,   132,     5,     5,
       5,    -1,   133,    -1,   134,    -1,    62,    -1,    45,    -1,
       8,    -1,     9,    -1,    10,    -1,    11,    -1,    12,    -1,
      13,    -1,    15,    -1,    14,    -1,    16,    -1,    17,    -1,
      18,    -1,    19,    -1,    20,    -1,    21,    -1,    23,    -1,
      24,    -1,    25,    -1,    22,    -1,    26,    -1,    27,    -1,
      28,    -1,    29,    -1,    30,    -1,    31,    -1,    32,    -1,
      33,    -1,    34,    -1,    35,    -1,    36,    -1,    37,    -1,
      38,    -1,    39,    -1,    40,    -1,    42,    -1,    41,    -1,
      43,    -1,    44,    -1,    46,    -1,    47,    -1,    52,    -1,
      51,    -1,    49,    -1,    48,    -1,    50,    -1,    53,    -1,
      54,    -1,    55,    -1,    56,    -1,    57,    -1,    58,    -1,
      59,    -1,    60,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   184,   184,   187,   193,   199,   205,   209,   213,   231,
     239,   245,   256,   257,   260,   261,   264,   271,   278,   283,
     286,   293,   294,   297,   348,   349,   352,   358,   362,   366,
     371,   375,   379,   383,   387,   413,   428,   432,   436,   440,
     444,   449,   453,   457,   461,   466,   471,   475,   479,   487,
     491,   495,   518,   522,   548,   552,   556,   560,   564,   568,
     610,   666,   700,   705,   709,   713,   717,   721,   726,   730,
     734,   738,   742,   796,   800,   826,   830,   834,   838,   842,
     846,   858,   860,   862,   864,   866,   868,   870,   872,   874,
     876,   878,   880,   882,   884,   886,   888,   890,   892,   894,
     896,   898,   900,   902,   904,   906,   908,   910,   912,   914,
     916,   918,   920,   922,   924,   926,   928,   930,   932,   934,
     936,   938,   940,   942,   944,   946,   948,   950,   952,   954,
     956,   958,   960,   962
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "STRING", "ID", "NUM", "LBRACKET",
  "RBRACKET", "AREALIGHTSOURCE", "ATTRIBUTE", "ATTRIBUTEBEGIN",
  "ATTRIBUTEEND", "ATMOSPHERE", "CLIPPING", "CONCATTRANSFORM", "COLOR",
  "COORDINATESYSTEM", "DECLARE", "DEPTHOFFIELD", "DISPLACEMENT", "DISPLAY",
  "EXPOSURE", "FORMAT", "FRAMEBEGIN", "FRAMEEND", "FRAMEASPECTRATIO",
  "HIDER", "IDENTITY", "ILLUMINATE", "IMAGER", "LIGHTSOURCE",
  "MOTIONBEGIN", "MOTIONEND", "OPACITY", "OPTION", "ORIENTATION",
  "PERSPECTIVE", "PIXELFILTER", "PIXELSAMPLES", "POINTSPOLYGONS",
  "POINTSGENERALPOLYGONS", "PROJECTION", "POLYGON", "QUANTIZE", "ROTATE",
  "RIBVERSION", "SCALE", "SCREENWINDOW", "SHADINGRATE",
  "SHADINGINTERPOLATION", "SHUTTER", "SIDES", "SPHERE", "SUBDIVISIONMESH",
  "SURFACE", "TRANSFORM", "TRANSFORMBEGIN", "TRANSFORMEND", "TRANSLATE",
  "WORLDBEGIN", "WORLDEND", "HIGH_PRECEDENCE", "UNKNOWN", "$accept",
  "start", "array_init", "num_list_init", "str_list_init", "param_array",
  "param_num_array", "param_str_array", "num_list", "str_list",
  "str_list_entry", "num_list_entry", "param_list", "param_list_init",
  "param_lists", "param_list_entry", "ri_command_list", "ri_command",
  "protocol", "ribversion", "arealightsource", "attribute",
  "attributebegin", "attributeend", "atmosphere", "clipping", "color",
  "concattransform", "coordinatesystem", "declare", "depthoffield",
  "displacement", "display", "exposure", "framebegin", "frameend",
  "frameaspectratio", "format", "hider", "identity", "illuminate",
  "imager", "lightsource", "motionbegin", "motionend", "opacity", "option",
  "orientation", "perspective", "pixelfilter", "pixelsamples",
  "pointspolygons", "pointsgeneralpolygons", "polygon", "projection",
  "quantize", "rotate", "scale", "screenwindow", "sphere", "sides",
  "shadinginterpolation", "shadingrate", "shutter", "subdivisionmesh",
  "surface", "transform", "transformbegin", "transformend", "translate",
  "worldbegin", "worldend", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    63,    64,    65,    66,    67,    68,    68,    68,    69,
      70,    70,    71,    71,    72,    72,    73,    74,    74,    75,
      76,    77,    77,    78,    79,    79,    80,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     0,     0,     1,     1,     1,     4,
       4,     2,     2,     1,     2,     1,     2,     2,     0,     2,
       0,     2,     0,     2,     2,     1,     1,     2,     3,     4,
       1,     1,     3,     3,     2,    19,     2,     3,     4,     3,
       5,     3,     4,     2,     1,     2,     3,     1,     3,     3,
       4,     2,     1,     2,     3,     2,     2,     4,     3,     4,
       5,     2,     3,     6,     5,     4,     5,     6,     2,     2,
       2,     3,     9,     3,     2,     1,     1,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    82,    83,    84,    85,    86,    87,    89,    88,    90,
      91,    92,    93,    94,    95,    99,    96,    97,    98,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   116,   115,   117,   118,    81,   119,
     120,   124,   123,   125,   122,   121,   126,   127,   128,   129,
     130,   131,   132,   133,    80,     0,     2,    25,    26,     0,
       0,     0,    30,    31,     0,     0,     3,     0,     0,     0,
       0,     0,     0,     0,     0,    44,     0,     0,     0,    47,
       0,     0,     0,     3,    52,     3,     0,     0,     0,     0,
       0,     3,     3,    20,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     3,    75,    76,     0,
      78,    79,     1,    24,    27,     0,    20,    20,     0,     0,
      34,     0,    36,     0,     0,    20,     0,     0,    43,    45,
       0,    20,     0,    20,     0,    51,    53,    20,    55,    56,
       0,     0,     3,     3,    61,    22,    20,     0,     0,     0,
       0,     0,    68,    69,    70,     0,     3,    20,    74,     0,
      20,    28,    32,    33,     4,     0,    37,     0,    39,     0,
      41,     0,    46,    48,    49,    20,    54,     0,    58,    20,
       3,     3,    19,    22,    62,     0,     0,     0,     0,     0,
      71,     3,    73,     0,    29,     0,     4,    13,     0,    38,
      20,    42,    50,    57,    59,    20,     8,     0,    23,     6,
       7,    21,     0,     0,    65,     0,     0,     3,    77,    17,
       9,    12,     0,    40,    60,    11,     4,     0,    64,    66,
      20,     0,     3,     0,     0,     5,    15,    63,    67,     5,
       3,     0,    16,    10,    14,     3,     0,    20,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    35
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    55,   119,   195,   234,   208,   120,   210,   196,   235,
     236,   197,   144,   145,   182,   183,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -186
static const yytype_int16 yypact[] =
{
     155,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,     5,   155,  -186,  -186,     2,
      12,    14,  -186,  -186,    16,    17,  -186,    15,    22,    23,
      24,    25,    27,    26,    29,  -186,    30,    31,    35,  -186,
      34,    37,    38,  -186,  -186,  -186,    39,    40,    41,    42,
      43,  -186,  -186,  -186,    46,    47,    48,    49,    50,    51,
      52,    55,    56,    58,    61,    62,  -186,  -186,  -186,    63,
    -186,  -186,  -186,  -186,  -186,    64,  -186,  -186,    65,    21,
    -186,    66,  -186,    69,    70,  -186,    71,    72,  -186,  -186,
      73,  -186,    74,  -186,    75,  -186,  -186,  -186,  -186,  -186,
      76,    77,  -186,  -186,  -186,    80,  -186,    79,    81,    83,
      84,    85,  -186,  -186,  -186,    86,  -186,  -186,  -186,    88,
    -186,  -186,  -186,  -186,    44,    89,  -186,    90,  -186,    82,
    -186,    91,  -186,  -186,  -186,  -186,  -186,    94,  -186,  -186,
    -186,    95,  -186,    80,  -186,    96,    97,    98,    99,   100,
    -186,  -186,  -186,   101,  -186,   102,    45,  -186,   104,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,     8,  -186,  -186,
    -186,  -186,   105,   106,  -186,   107,   108,  -186,  -186,  -186,
    -186,  -186,   109,  -186,  -186,  -186,     3,   110,  -186,  -186,
    -186,    10,  -186,   111,   115,    60,  -186,  -186,  -186,  -186,
    -186,   114,  -186,  -186,  -186,  -186,   116,  -186,   117,  -186,
     118,   119,   120,   121,   122,   123,   124,   125,   113,  -186
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -186,  -186,  -180,  -186,  -186,  -186,   -83,  -185,  -186,  -186,
    -159,   -65,  -113,  -186,   -51,  -186,  -186,    87,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -19
static const yytype_int16 yytable[] =
{
     135,   207,   136,   161,   162,   112,    -5,   114,   142,   143,
     -18,   225,   168,   225,   226,   115,   239,   116,   172,   117,
     174,   121,   118,   158,   176,   122,   123,   164,   125,   124,
     126,   127,   232,   184,   128,   129,   130,   231,   131,   132,
     133,   134,   137,   138,   192,   140,   139,   194,   141,   146,
     147,   -18,   220,   148,   149,   150,   151,   152,   153,   179,
     180,   154,   202,   155,   156,   157,   204,   243,   159,   160,
     163,   165,   166,   191,   169,   167,   244,   170,   171,   173,
     175,   177,   178,   181,   185,   200,   186,   223,   187,   188,
     189,   190,   224,   193,   198,   199,   201,   205,   209,   203,
     206,   212,   213,   214,   215,   216,   218,   219,   217,   222,
     227,   228,   229,   230,   233,   237,   241,   238,   242,   246,
     259,   248,   250,   251,   252,   253,   254,   255,   256,   257,
     258,   221,   211,     0,   249,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,     0,     0,   240,
       0,     0,     0,     0,     0,     0,     0,   245,     0,     0,
       0,     0,   247,     1,     2,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,    54
};

static const yytype_int16 yycheck[] =
{
      83,   181,    85,   116,   117,     0,     3,     5,    91,    92,
       7,     3,   125,     3,     6,     3,     6,     3,   131,     3,
     133,     6,     5,   106,   137,     3,     3,     6,     3,     5,
       3,     5,   217,   146,     5,     5,     5,   217,     3,     5,
       3,     3,     3,     3,   157,     3,     5,   160,     5,     3,
       3,     7,     7,     5,     5,     5,     5,     5,     3,   142,
     143,     5,   175,     5,     3,     3,   179,     7,     5,     5,
       5,     5,     3,   156,     3,     5,   235,     5,     5,     5,
       5,     5,     5,     3,     5,     3,     5,   200,     5,     5,
       5,     5,   205,     5,     5,     5,     5,   180,   181,     5,
       5,     5,     5,     5,     5,     5,     5,     5,   191,     5,
       5,     5,     5,     5,     5,     5,     5,   230,     3,     5,
       7,     5,     5,     5,     5,     5,     5,     5,     5,     5,
       5,   196,   183,    -1,   247,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,   232,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,
      -1,    -1,   245,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    62
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    62,    64,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,     0,    80,     5,     3,     3,     3,     5,    65,
      69,     6,     3,     3,     5,     3,     3,     5,     5,     5,
       5,     3,     5,     3,     3,    69,    69,     3,     3,     5,
       3,     5,    69,    69,    75,    76,     3,     3,     5,     5,
       5,     5,     5,     3,     5,     5,     3,     3,    69,     5,
       5,    75,    75,     5,     6,     5,     3,     5,    75,     3,
       5,     5,    75,     5,    75,     5,    75,     5,     5,    69,
      69,     3,    77,    78,    75,     5,     5,     5,     5,     5,
       5,    69,    75,     5,    75,    66,    71,    74,     5,     5,
       3,     5,    75,     5,    75,    69,     5,    65,    68,    69,
      70,    77,     5,     5,     5,     5,     5,    69,     5,     5,
       7,    74,     5,    75,    75,     3,     6,     5,     5,     5,
       5,    65,    70,     5,    67,    72,    73,     5,    75,     6,
      69,     5,     3,     7,    73,    69,     5,    69,     5,    75,
       5,     5,     5,     5,     5,     5,     5,     5,     5,     7
};

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
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
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
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

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
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
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

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
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
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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
#line 188 "src/lsh/parserib.y"
    {
    init_array();
;}
    break;

  case 4:
#line 194 "src/lsh/parserib.y"
    {
    array_type = NUM_ARRAY;
;}
    break;

  case 5:
#line 200 "src/lsh/parserib.y"
    {
    array_type = STRING_ARRAY;
;}
    break;

  case 6:
#line 206 "src/lsh/parserib.y"
    {
    (yyval.paramarray) = (yyvsp[(1) - (1)].paramarray);
;}
    break;

  case 7:
#line 210 "src/lsh/parserib.y"
    {
    (yyval.paramarray) = (yyvsp[(1) - (1)].paramarray);
;}
    break;

  case 8:
#line 214 "src/lsh/parserib.y"
    {
    RtFloat *num;

    init_array();
    array_type = NUM_ARRAY;

    num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
    *num = (RtFloat)((yyvsp[(1) - (1)].num));
    add_array(num);

    (yyval.paramarray) = array_dup(curr_array);
    ri_ptr_array_free(curr_array);
    curr_array = NULL;
;}
    break;

  case 9:
#line 232 "src/lsh/parserib.y"
    {
    (yyval.paramarray) = array_dup(curr_array);
    ri_ptr_array_free(curr_array);
    curr_array = NULL;
;}
    break;

  case 10:
#line 240 "src/lsh/parserib.y"
    {
    (yyval.paramarray) = array_dup(curr_array);
    ri_ptr_array_free(curr_array);
    curr_array = NULL;
;}
    break;

  case 11:
#line 246 "src/lsh/parserib.y"
    {
    RtToken str = (RtToken)strdup((char *)((yyvsp[(2) - (2)].string)));
    add_array(str);

    (yyval.paramarray) = array_dup(curr_array);
    ri_ptr_array_free(curr_array);
    curr_array = NULL;
;}
    break;

  case 16:
#line 265 "src/lsh/parserib.y"
    {
    RtToken str = (RtToken)strdup((char *)((yyvsp[(2) - (2)].string)));
    add_array(str);
;}
    break;

  case 17:
#line 272 "src/lsh/parserib.y"
    {
    RtFloat *num = (RtFloat *)ri_mem_alloc(sizeof(RtFloat));
    *num = (RtFloat)((yyvsp[(2) - (2)].num));
    add_array(num);
;}
    break;

  case 18:
#line 278 "src/lsh/parserib.y"
    {
    //printf("no elem\n");
;}
    break;

  case 20:
#line 287 "src/lsh/parserib.y"
    {
    rib_param_num = 0;
    curr_array_count = 0;
;}
    break;

  case 23:
#line 298 "src/lsh/parserib.y"
    {
    unsigned long i;
    unsigned long n;
    RtPointer arg;
    RtToken   tag;

    ri_ptr_array_t *t;

    tag = (yyvsp[(1) - (2)].string);
    t = (yyvsp[(2) - (2)].paramarray);

    n = (yyvsp[(2) - (2)].paramarray)->nelems;
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
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), numptrfree);
    } else {
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), strptrfree);
    }
    ri_ptr_array_free((yyvsp[(2) - (2)].paramarray));
;}
    break;

  case 26:
#line 353 "src/lsh/parserib.y"
    {
    enter_mode_skip();
;}
    break;

  case 27:
#line 359 "src/lsh/parserib.y"
    {
    //printf("RIB Version: %f\n", $2 );
;}
    break;

  case 28:
#line 363 "src/lsh/parserib.y"
    {
    RiAttributeV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 29:
#line 367 "src/lsh/parserib.y"
    {
    RiAreaLightSourceV((RtToken)(yyvsp[(2) - (4)].string),
               rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 30:
#line 372 "src/lsh/parserib.y"
    {
    RiAttributeBegin();
;}
    break;

  case 31:
#line 376 "src/lsh/parserib.y"
    {
    RiAttributeEnd();
;}
    break;

  case 32:
#line 380 "src/lsh/parserib.y"
    {
    RiAtmosphereV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 33:
#line 384 "src/lsh/parserib.y"
    {
    RiClipping((yyvsp[(2) - (3)].num), (yyvsp[(3) - (3)].num));
;}
    break;

  case 34:
#line 388 "src/lsh/parserib.y"
    {
    int     i;
    RtColor col;
    ri_ptr_array_t *p;

    p = (yyvsp[(2) - (2)].paramarray);

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
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), numptrfree);
    } else {
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), strptrfree);
    }
    ri_ptr_array_free(p);
;}
    break;

  case 35:
#line 417 "src/lsh/parserib.y"
    {
    RtMatrix mat;

    mat[0][0] = (yyvsp[(3) - (19)].num) ; mat[0][1] = (yyvsp[(4) - (19)].num) ; mat[0][2] = (yyvsp[(5) - (19)].num) ; mat[0][3] = (yyvsp[(6) - (19)].num) ;
    mat[1][0] = (yyvsp[(7) - (19)].num) ; mat[1][1] = (yyvsp[(8) - (19)].num) ; mat[1][2] = (yyvsp[(9) - (19)].num) ; mat[1][3] = (yyvsp[(10) - (19)].num);
    mat[2][0] = (yyvsp[(11) - (19)].num); mat[2][1] = (yyvsp[(12) - (19)].num); mat[2][2] = (yyvsp[(13) - (19)].num); mat[2][3] = (yyvsp[(14) - (19)].num);
    mat[3][0] = (yyvsp[(15) - (19)].num); mat[3][1] = (yyvsp[(16) - (19)].num); mat[3][2] = (yyvsp[(17) - (19)].num); mat[3][3] = (yyvsp[(18) - (19)].num);

    RiConcatTransform(mat);

;}
    break;

  case 36:
#line 429 "src/lsh/parserib.y"
    {
    RiCoordinateSystem((RtToken)(yyvsp[(2) - (2)].string));
;}
    break;

  case 37:
#line 433 "src/lsh/parserib.y"
    {
    RiDeclare((yyvsp[(2) - (3)].string), (yyvsp[(3) - (3)].string));
;}
    break;

  case 38:
#line 437 "src/lsh/parserib.y"
    {
    RiDepthOfField((RtFloat)(yyvsp[(2) - (4)].num), (RtFloat)(yyvsp[(3) - (4)].num), (RtFloat)(yyvsp[(4) - (4)].num));
;}
    break;

  case 39:
#line 441 "src/lsh/parserib.y"
    {
    RiDisplacementV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 40:
#line 445 "src/lsh/parserib.y"
    {
    RiDisplayV((yyvsp[(2) - (5)].string), (yyvsp[(3) - (5)].string), (yyvsp[(4) - (5)].string),
           rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 41:
#line 450 "src/lsh/parserib.y"
    {
    RiExposure((yyvsp[(2) - (3)].num), (yyvsp[(3) - (3)].num));
;}
    break;

  case 42:
#line 454 "src/lsh/parserib.y"
    {
    RiFormat((yyvsp[(2) - (4)].num), (yyvsp[(3) - (4)].num), (yyvsp[(4) - (4)].num));
;}
    break;

  case 43:
#line 458 "src/lsh/parserib.y"
    {
    RiFrameBegin((yyvsp[(2) - (2)].num));
;}
    break;

  case 44:
#line 462 "src/lsh/parserib.y"
    {
    RiFrameEnd();

;}
    break;

  case 45:
#line 467 "src/lsh/parserib.y"
    {
    RiFrameAspectRatio((yyvsp[(2) - (2)].num));

;}
    break;

  case 46:
#line 472 "src/lsh/parserib.y"
    {
    RiHiderV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 47:
#line 476 "src/lsh/parserib.y"
    {
    RiIdentity();
;}
    break;

  case 48:
#line 480 "src/lsh/parserib.y"
    {
    RtInt lightid;

    lightid = (RtInt)(yyvsp[(2) - (3)].num);

    RiIlluminate((RtLightHandle)&(lightid), (yyvsp[(3) - (3)].num));
;}
    break;

  case 49:
#line 488 "src/lsh/parserib.y"
    {
    RiImagerV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 50:
#line 492 "src/lsh/parserib.y"
    {
    RiLightSourceV((yyvsp[(2) - (4)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 51:
#line 496 "src/lsh/parserib.y"
    { 
    unsigned int    i;
    RtFloat        *times;
    ri_ptr_array_t *p;

    if (array_type != NUM_ARRAY) {
        ri_log(LOG_WARN, "MotionBegin: not a float array argument.\n");
    } else {
        p = (yyvsp[(2) - (2)].paramarray);

        times = (RtFloat *)ri_mem_alloc(sizeof(RtFloat) * p->nelems);

        for (i = 0; i < p->nelems; i++) {
            times[i] = *((RtFloat *)ri_ptr_array_at(p, i));
        }

        RiMotionBeginV(p->nelems, times);

        ri_ptr_array_traverse(p, numptrfree);
        ri_ptr_array_free(p);
    }
;}
    break;

  case 52:
#line 519 "src/lsh/parserib.y"
    {
    RiMotionEnd();
;}
    break;

  case 53:
#line 523 "src/lsh/parserib.y"
    {
    int     i;
    RtColor opa;
    ri_ptr_array_t *p;

    p = (yyvsp[(2) - (2)].paramarray);

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
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), numptrfree);
    } else {
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), strptrfree);
    }
    ri_ptr_array_free(p);
;}
    break;

  case 54:
#line 549 "src/lsh/parserib.y"
    {
    RiOptionV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 55:
#line 553 "src/lsh/parserib.y"
    {
    RiOrientation((RtToken)(yyvsp[(2) - (2)].string));
;}
    break;

  case 56:
#line 557 "src/lsh/parserib.y"
    {
    RiPerspective((yyvsp[(2) - (2)].num));
;}
    break;

  case 57:
#line 561 "src/lsh/parserib.y"
    {
    RiPixelFilter(RiBoxFilter, (yyvsp[(3) - (4)].num), (yyvsp[(4) - (4)].num));
;}
    break;

  case 58:
#line 565 "src/lsh/parserib.y"
    {
    RiPixelSamples((yyvsp[(2) - (3)].num), (yyvsp[(3) - (3)].num));
;}
    break;

  case 59:
#line 569 "src/lsh/parserib.y"
    {
    int             i;
    int             have_p = 0;
    RtInt           npolys, *nverts, *verts;
    ri_ptr_array_t *p;

    p = (yyvsp[(2) - (4)].paramarray);
    npolys = p->nelems;
    
    nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
    for (i = 0; i < (int)p->nelems; i++) {
        nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
    }

    p = (yyvsp[(3) - (4)].paramarray);
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
        ri_ptr_array_traverse((yyvsp[(2) - (4)].paramarray), numptrfree);
    } else {
        ri_ptr_array_traverse((yyvsp[(2) - (4)].paramarray), strptrfree);
    }
    ri_ptr_array_free(p);

    RiPointsPolygonsV(npolys, nverts, verts,
              rib_param_num, rib_param_tokens, rib_param_args);

;}
    break;

  case 60:
#line 611 "src/lsh/parserib.y"
    {
    int             i;
    int             docall = 1;
    int             have_p = 0;
    int             loop_warn = 0;
    RtInt           loopsize;
    RtInt           npolys, *nloops, *nverts, *verts;
    ri_ptr_array_t *p;

    p = (yyvsp[(2) - (5)].paramarray);
    npolys = p->nelems;
    
    nloops = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
    for (i = 0; i < (int)p->nelems; i++) {
        nloops[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
    }
    loopsize = p->nelems;

    p = (yyvsp[(3) - (5)].paramarray);
    nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * (p->nelems));
    for (i = 0; i < (int)p->nelems; i++) {
        nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
    }

    p = (yyvsp[(4) - (5)].paramarray);
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
            loop_warn = 1;    /* prevent multiple warning. */
        }
    }

    if (docall) {
        RiPointsGeneralPolygonsV(npolys, nloops, nverts, verts,
                     rib_param_num,
                     rib_param_tokens, rib_param_args);
    }
;}
    break;

  case 61:
#line 667 "src/lsh/parserib.y"
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
;}
    break;

  case 62:
#line 701 "src/lsh/parserib.y"
    {
    RiProjectionV((RtToken)(yyvsp[(2) - (3)].string),
              rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 63:
#line 706 "src/lsh/parserib.y"
    {
    RiQuantize((RtToken)(yyvsp[(2) - (6)].string), (yyvsp[(3) - (6)].num), (yyvsp[(4) - (6)].num), (yyvsp[(5) - (6)].num), (yyvsp[(6) - (6)].num));
;}
    break;

  case 64:
#line 710 "src/lsh/parserib.y"
    {
    RiRotate((RtFloat)(yyvsp[(2) - (5)].num), (RtFloat)(yyvsp[(3) - (5)].num), (RtFloat)(yyvsp[(4) - (5)].num), (RtFloat)(yyvsp[(5) - (5)].num));
;}
    break;

  case 65:
#line 714 "src/lsh/parserib.y"
    {
    RiScale((RtFloat)(yyvsp[(2) - (4)].num), (RtFloat)(yyvsp[(3) - (4)].num), (RtFloat)(yyvsp[(4) - (4)].num));
;}
    break;

  case 66:
#line 718 "src/lsh/parserib.y"
    {
    RiScreenWindow((RtFloat)(yyvsp[(2) - (5)].num), (RtFloat)(yyvsp[(3) - (5)].num), (RtFloat)(yyvsp[(4) - (5)].num), (RtFloat)(yyvsp[(5) - (5)].num));
;}
    break;

  case 67:
#line 722 "src/lsh/parserib.y"
    {
    RiSphereV((RtFloat)(yyvsp[(2) - (6)].num), (RtFloat)(yyvsp[(3) - (6)].num), (RtFloat)(yyvsp[(4) - (6)].num), (RtFloat)(yyvsp[(5) - (6)].num),
          rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 68:
#line 727 "src/lsh/parserib.y"
    {
    RiSides((RtFloat)(yyvsp[(2) - (2)].num));
;}
    break;

  case 69:
#line 731 "src/lsh/parserib.y"
    {
    RiShadingInterpolation((RtToken)(yyvsp[(2) - (2)].string));
;}
    break;

  case 70:
#line 735 "src/lsh/parserib.y"
    {
    RiShadingRate((yyvsp[(2) - (2)].num));
;}
    break;

  case 71:
#line 739 "src/lsh/parserib.y"
    {
    RiShutter((yyvsp[(2) - (3)].num), (yyvsp[(3) - (3)].num));
;}
    break;

  case 72:
#line 743 "src/lsh/parserib.y"
    {
    int             i;
    int             docall = 1;
    int             have_p = 0;
    int             face_warn = 0;
    RtInt           nfaces, *nverts, *verts;
    ri_ptr_array_t *p;

    p = (yyvsp[(3) - (9)].paramarray);
    nfaces = p->nelems;
    
    nverts = (RtInt *)ri_mem_alloc(sizeof(RtInt) * nfaces);
    for (i = 0; i < (int)p->nelems; i++) {
        nverts[i] = (RtInt)(*((RtFloat *)ri_ptr_array_at(p, i)));
    }

    p = (yyvsp[(4) - (9)].paramarray);
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
            face_warn = 1;    /* prevent multiple warning. */
        }
    }

    if (docall) {
        RiSubdivisionMeshV((yyvsp[(2) - (9)].string), nfaces, nverts, verts,
                   0,            /* ntags    */
                   NULL,        /* tags        */
                   NULL,        /* nargs    */
                   NULL,        /* intargs    */
                   NULL,        /* floatargs    */
                   rib_param_num,
                   rib_param_tokens, rib_param_args);
    }
    
;}
    break;

  case 73:
#line 797 "src/lsh/parserib.y"
    {
    RiSurfaceV((yyvsp[(2) - (3)].string), rib_param_num, rib_param_tokens, rib_param_args);
;}
    break;

  case 74:
#line 801 "src/lsh/parserib.y"
    {
    int     i, j;
    RtMatrix matrix;
    ri_ptr_array_t *p;
    
    p = (yyvsp[(2) - (2)].paramarray);
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
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), numptrfree);
    } else {
        ri_ptr_array_traverse((yyvsp[(2) - (2)].paramarray), strptrfree);
    }
    ri_ptr_array_free(p);
;}
    break;

  case 75:
#line 827 "src/lsh/parserib.y"
    {
    RiTransformBegin();
;}
    break;

  case 76:
#line 831 "src/lsh/parserib.y"
    {
    RiTransformEnd();
;}
    break;

  case 77:
#line 835 "src/lsh/parserib.y"
    {
    RiTranslate((RtFloat)(yyvsp[(2) - (4)].num), (RtFloat)(yyvsp[(3) - (4)].num), (RtFloat)(yyvsp[(4) - (4)].num));
;}
    break;

  case 78:
#line 839 "src/lsh/parserib.y"
    {
    RiWorldBegin();
;}
    break;

  case 79:
#line 843 "src/lsh/parserib.y"
    {
    RiWorldEnd();
;}
    break;

  case 80:
#line 847 "src/lsh/parserib.y"
    {
    enter_mode_skip();
    printf("[RIB parse] Unknown command: %s at line %d\n", yylval.string, line_num);
    nunknown_commands++;
    if (nunknown_commands > max_unknown_commands) {
        printf("[RIB parse] Too many unknown commands. Give up parsing.\n");
        exit(-1);
    }
;}
    break;

  case 81:
#line 858 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 82:
#line 860 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 83:
#line 862 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 84:
#line 864 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 85:
#line 866 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 86:
#line 868 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 87:
#line 870 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 88:
#line 872 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 89:
#line 874 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 90:
#line 876 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 91:
#line 878 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 92:
#line 880 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 93:
#line 882 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 94:
#line 884 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 95:
#line 886 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 96:
#line 888 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 97:
#line 890 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 98:
#line 892 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 99:
#line 894 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 100:
#line 896 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 101:
#line 898 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 102:
#line 900 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 103:
#line 902 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 104:
#line 904 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 105:
#line 906 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 106:
#line 908 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 107:
#line 910 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 108:
#line 912 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 109:
#line 914 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 110:
#line 916 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 111:
#line 918 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 112:
#line 920 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 113:
#line 922 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 114:
#line 924 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 115:
#line 926 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 116:
#line 928 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 117:
#line 930 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 118:
#line 932 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 119:
#line 934 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 120:
#line 936 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 121:
#line 938 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 122:
#line 940 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 123:
#line 942 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 124:
#line 944 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 125:
#line 946 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 126:
#line 948 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 127:
#line 950 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 128:
#line 952 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 129:
#line 954 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 130:
#line 956 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 131:
#line 958 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 132:
#line 960 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;

  case 133:
#line 962 "src/lsh/parserib.y"
    { enter_mode_param();    ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2890 "src/lsh/parserib.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
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
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
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


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 965 "src/lsh/parserib.y"


