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
     SURFACE = 258,
     IDENTIFIER = 259,
     NUMBER = 260,
     STRINGCONSTANT = 261,
     VOID = 262,
     FLOAT = 263,
     NORMAL = 264,
     VECTOR = 265,
     COLOR = 266,
     POINT = 267,
     STRING = 268,
     LIGHTSOURCE = 269,
     VARYING = 270,
     UNIFORM = 271,
     ENVIRONMENT = 272,
     TEXTURE = 273,
     RADIANS = 274,
     DEGREES = 275,
     ABS = 276,
     FLOOR = 277,
     CEIL = 278,
     ROUND = 279,
     MIX = 280,
     MOD = 281,
     NOISE = 282,
     STEP = 283,
     SMOOTHSTEP = 284,
     SQRT = 285,
     INVERSESQRT = 286,
     LENGTH = 287,
     SIN = 288,
     ASIN = 289,
     COS = 290,
     ACOS = 291,
     TAN = 292,
     ATAN = 293,
     POW = 294,
     EXP = 295,
     LOG = 296,
     SIGN = 297,
     RANDOM = 298,
     MATH_PI = 299,
     REFRACT = 300,
     OCCLUSION = 301,
     TRACE = 302,
     AMBIENT = 303,
     DIFFUSE = 304,
     SPECULAR = 305,
     PLUSEQ = 306,
     MINUSEQ = 307,
     MULEQ = 308,
     XCOMP = 309,
     YCOMP = 310,
     ZCOMP = 311,
     SETXCOMP = 312,
     SETYCOMP = 313,
     SETZCOMP = 314,
     AREA = 315,
     FOR = 316,
     WHILE = 317,
     IF = 318,
     ELSE = 319,
     ILLUMINANCE = 320,
     OPASSIGN = 321,
     OPVARDEF = 322,
     OPMUL = 323,
     OPDIV = 324,
     OPDOT = 325,
     OPADD = 326,
     OPSUB = 327,
     OPNEG = 328,
     OPFUNC = 329,
     OPFUNCARG = 330,
     OPASSIGNADD = 331,
     OPASSIGNSUB = 332,
     OPASSIGNMUL = 333,
     OPCOND = 334,
     OPPARENT = 335,
     OPLE = 336,
     OPNEQ = 337,
     OPFTOV = 338,
     OPFOR = 339,
     OPFORCOND = 340,
     OPWHILE = 341,
     OPIF = 342,
     OPILLUMINANCE = 343,
     TYPECAST = 344,
     UMINUS = 345
   };
#endif
#define SURFACE 258
#define IDENTIFIER 259
#define NUMBER 260
#define STRINGCONSTANT 261
#define VOID 262
#define FLOAT 263
#define NORMAL 264
#define VECTOR 265
#define COLOR 266
#define POINT 267
#define STRING 268
#define LIGHTSOURCE 269
#define VARYING 270
#define UNIFORM 271
#define ENVIRONMENT 272
#define TEXTURE 273
#define RADIANS 274
#define DEGREES 275
#define ABS 276
#define FLOOR 277
#define CEIL 278
#define ROUND 279
#define MIX 280
#define MOD 281
#define NOISE 282
#define STEP 283
#define SMOOTHSTEP 284
#define SQRT 285
#define INVERSESQRT 286
#define LENGTH 287
#define SIN 288
#define ASIN 289
#define COS 290
#define ACOS 291
#define TAN 292
#define ATAN 293
#define POW 294
#define EXP 295
#define LOG 296
#define SIGN 297
#define RANDOM 298
#define MATH_PI 299
#define REFRACT 300
#define OCCLUSION 301
#define TRACE 302
#define AMBIENT 303
#define DIFFUSE 304
#define SPECULAR 305
#define PLUSEQ 306
#define MINUSEQ 307
#define MULEQ 308
#define XCOMP 309
#define YCOMP 310
#define ZCOMP 311
#define SETXCOMP 312
#define SETYCOMP 313
#define SETZCOMP 314
#define AREA 315
#define FOR 316
#define WHILE 317
#define IF 318
#define ELSE 319
#define ILLUMINANCE 320
#define OPASSIGN 321
#define OPVARDEF 322
#define OPMUL 323
#define OPDIV 324
#define OPDOT 325
#define OPADD 326
#define OPSUB 327
#define OPNEG 328
#define OPFUNC 329
#define OPFUNCARG 330
#define OPASSIGNADD 331
#define OPASSIGNSUB 332
#define OPASSIGNMUL 333
#define OPCOND 334
#define OPPARENT 335
#define OPLE 336
#define OPNEQ 337
#define OPFTOV 338
#define OPFOR 339
#define OPFORCOND 340
#define OPWHILE 341
#define OPIF 342
#define OPILLUMINANCE 343
#define TYPECAST 344
#define UMINUS 345




/* Copy the first part of user declarations.  */
#line 1 "parsesl.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tree.h"
#include "sl2c.h"

#define EXPRLIST_SIZE 1024
#define MAX_BLOCK 128

typedef struct _exprblock_t
{
	node_t *expr;
	int     start;		/* block start */
	int     end;		/* block end   */ 
	int     elsestart;	/* else block start */
	int     elseend;	/* else block end */
	int     indent;
} exprblock_t;

typedef struct _block_t
{
	int              block;
	struct _block_t *prev;
} block_t;

typedef struct _func_body_t
{
	block_t *blocks;
	int currblockdepth;
	exprblock_t exprlist[EXPRLIST_SIZE];
	node_t *formalexprlist[EXPRLIST_SIZE];

	int nexpr;
	int nformalexpr;
	int nscalarexpr;

	char *funcname;
} func_body_t;

block_t *push_block(block_t *block)
{
	block_t *p;

	p = (block_t *)malloc(sizeof(block_t));
	p->prev = block;

	return p;
}

block_t *pop_block(block_t *block)
{
	block_t *p;

	if (block->prev == NULL) {
		return NULL;
	}

	p = block->prev;

	free(block);

	return p;
}

func_body_t f;			/* gloal singleton */

static int vartype;		/* type of variable */

extern int nlines;		/* defined in lexsl.l */
int        get_parsed_line();

static void write_header();
static void write_body();
static void write_block(int *curr);
static void write_funcarg();
static void write_paraminitializer();

static void init_func_body(char *funcname);


static void
write_header()
{
	fprintf(g_csfp, "/*				\n");
	fprintf(g_csfp, " * Generated by sl2c.		\n");
	fprintf(g_csfp, " */				\n");
	fprintf(g_csfp, "\n");
	fprintf(g_csfp, "#include <stdio.h>\n");
	fprintf(g_csfp, "#include <stdlib.h>\n");
	fprintf(g_csfp, "#include <math.h>\n");
	fprintf(g_csfp, "\n");
	fprintf(g_csfp, "#include \"shader.h\"\n");
	fprintf(g_csfp, "\n");
}

static void
write_body()
{
	int i;
	sym_t *sp;

	fprintf(g_csfp, "DLLEXPORT void\n");
	fprintf(g_csfp, "%s", f.funcname);
	write_funcarg();
	fprintf(g_csfp, "{\n");
			
	for (i = 0; i < f.nformalexpr; i++) {
		write_defvar(f.formalexprlist[i]);
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nexpr; i++) {
		write_tmpvar(f.exprlist[i].expr);
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nformalexpr; i++) {
		if (f.formalexprlist[i]->opcode != OPVARDEF) continue;

		sp = (sym_t *)f.formalexprlist[i]->left->left;

		if (sp->type == FLOAT) {
			fprintf(g_csfp, "\tri_param_eval(&%s, param, \"%s\");\n",
				sp->name, sp->name);
		} else {
			fprintf(g_csfp, "\tri_param_eval(&%s, param, \"%s\");\n",
				sp->name, sp->name);
		}
	}

	fprintf(g_csfp, "\n");

	i = 0;
	while (i < f.nexpr) {
		if (f.exprlist[i].expr->opcode == OPFOR   ||
		    f.exprlist[i].expr->opcode == OPWHILE ||
		    f.exprlist[i].expr->opcode == OPIF    ||
		    f.exprlist[i].expr->opcode == OPILLUMINANCE) {
			write_block(&i);
		} else if (f.exprlist[i].expr->type == FLOAT) {
			if (f.exprlist[i].expr->opcode == OPVARDEF &&
			    !f.exprlist[i].expr->right) {
				/* do nothing */
			} else {
				write_scalar_node(f.exprlist[i].expr);
				fprintf(g_csfp, ";\n");
			}
		} else {
			write_node(f.exprlist[i].expr);
		}

		i++;
	}

	fprintf(g_csfp, "}\n");
}

static void
write_block(int *curr)
{
	int i, indent;
	int start, end;
	int elsestart, elseend;

	start     = f.exprlist[*curr].start;
	end       = f.exprlist[*curr].end;
	elsestart = f.exprlist[*curr].elsestart;
	elseend   = f.exprlist[*curr].elseend;

	indent = f.exprlist[*curr].indent;
	set_indent(indent);
	write_scalar_node(f.exprlist[*curr].expr);
	fprintf(g_csfp, "\n");

	i = start;
	while (i <= end) {
		if (f.exprlist[i].expr->opcode == OPFOR   ||
		    f.exprlist[i].expr->opcode == OPWHILE ||
		    f.exprlist[i].expr->opcode == OPIF    ||
		    f.exprlist[i].expr->opcode == OPILLUMINANCE) {
			write_block(&i);
		} else if (f.exprlist[i].expr->type == FLOAT) {
			set_indent(indent + 1);
			write_scalar_node(f.exprlist[i].expr);
			fprintf(g_csfp, ";\n");
		} else {
			set_indent(indent + 1);
			write_node(f.exprlist[i].expr);
		}

		i++;
	}

	set_indent(indent);
	write_indent();
	fprintf(g_csfp, "\t}");

	if (elseend != 0) {
		fprintf(g_csfp, " else {\n");
		i = elsestart;
		while (i <= elseend) {
			if (f.exprlist[i].expr->opcode == OPFOR   ||
			    f.exprlist[i].expr->opcode == OPWHILE ||
			    f.exprlist[i].expr->opcode == OPIF    ||
			    f.exprlist[i].expr->opcode == OPILLUMINANCE) {
				write_block(&i);
			} else if (f.exprlist[i].expr->type == FLOAT) {
				set_indent(indent + 1);
				write_scalar_node(f.exprlist[i].expr);
				fprintf(g_csfp, ";\n");
			} else {
				set_indent(indent + 1);
				write_node(f.exprlist[i].expr);
			}

			i++;
		}

		set_indent(indent);
		write_indent();
		fprintf(g_csfp, "\t}");
	}

	fprintf(g_csfp, "\n");
	set_indent(indent);

	if (elseend == 0) {
		*curr = end;
	} else {
		*curr = elseend;
	}

}

static void
write_funcarg()
{
	fprintf(g_csfp, "(ri_output_t *output, ");
	fprintf(g_csfp, "ri_status_t *status, ");
	fprintf(g_csfp, "ri_parameter_t *param)\n");
}

static void
write_paraminitializer()
{
	int i;
	sym_t *sp;
	char buf[256];

	fprintf(g_csfp, "DLLEXPORT void\n");
	fprintf(g_csfp, "%s_initparam(ri_parameter_t *param)\n", f.funcname);
	fprintf(g_csfp, "{\n");

	for (i = 0; i < f.nformalexpr; i++) {
		write_tmpvar(f.formalexprlist[i]);
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nformalexpr; i++) {
		if (f.formalexprlist[i]->type == FLOAT) {
			write_scalar_node(f.formalexprlist[i]);
			fprintf(g_csfp, ";\n");
		} else {
			write_node(f.formalexprlist[i]);
		}
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nformalexpr; i++) {
		if (f.formalexprlist[i]->opcode != OPVARDEF) continue;

		sp = (sym_t *)f.formalexprlist[i]->left->left;

		switch(sp->type) {
		case FLOAT:
			strcpy(buf, "TYPEFLOAT");
			
			break;

		case VECTOR:
		case COLOR:
		case POINT:
			strcpy(buf, "TYPEVECTOR");
			if (!f.formalexprlist[i]->right) {
				/* set default value. */
				fprintf(g_csfp, "\tri_vector_set(%s", sp->name);
				fprintf(g_csfp, ", 0.0, 0.0, 0.0, 1.0);\n");
			}

			break;

		case STRING:
			strcpy(buf, "TYPESTRING");
			if (!f.formalexprlist[i]->right) {
				/* set default value. */
				fprintf(g_csfp, "\t%s = "";\n", sp->name);
			}

			break;

		default:
			strcpy(buf, "TYPEUNKNOWN");
			break;
		}

		if (sp->type == FLOAT) {
			fprintf(g_csfp, "\tri_param_add(param, \"%s\", %s, &%s);\n",
				sp->name, buf, sp->name);
		} else {
			fprintf(g_csfp, "\tri_param_add(param, \"%s\", %s, &%s);\n",
				sp->name, buf, sp->name);
		}

	}
	fprintf(g_csfp, "}\n");
}

void
init_func_body(char *funcname)
{
	static int first = 1;

	if (first) {
		f.blocks = (block_t *)malloc(sizeof(block_t));
		f.blocks->prev = NULL;
		first = 0;

	} else {
		while ((f.blocks = pop_block(f.blocks)) != NULL);

		free(f.funcname);
	}

	f.nexpr          = 0;
	f.nformalexpr    = 0; 
	f.nscalarexpr    = 0;

	f.funcname	 = strdup(funcname);

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
#line 350 "parsesl.y"
typedef union YYSTYPE {
	char   *string;
	node_t *np;
	double  fval;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 611 "parsesl.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 623 "parsesl.c"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   399

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  106
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  36
/* YYNRULES -- Number of rules. */
#define YYNRULES  124
/* YYNRULES -- Number of states. */
#define YYNSTATES  308

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   345

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      98,    99,    93,    91,   101,    92,    95,    94,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   105,   100,
      90,    89,     2,   104,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   102,     2,   103,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    96,    97
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short int yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    11,    14,    15,    22,
      23,    25,    29,    32,    35,    38,    41,    43,    45,    47,
      49,    51,    53,    55,    56,    58,    60,    62,    66,    68,
      72,    75,    76,    79,    83,    84,    87,    89,    92,    95,
      98,   102,   105,   107,   111,   120,   125,   134,   137,   142,
     146,   150,   152,   156,   160,   164,   168,   172,   175,   181,
     185,   188,   190,   192,   194,   196,   198,   200,   202,   204,
     212,   213,   215,   218,   222,   226,   230,   234,   239,   244,
     249,   254,   259,   264,   269,   274,   279,   284,   289,   294,
     299,   303,   308,   313,   318,   323,   328,   333,   338,   343,
     348,   353,   358,   363,   368,   373,   378,   383,   388,   393,
     398,   403,   408,   413,   418,   423,   428,   433,   434,   436,
     440,   445,   447,   449,   451
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short int yyrhs[] =
{
     107,     0,    -1,    -1,   108,   109,    -1,    -1,   109,   110,
      -1,   111,   123,    -1,    -1,   117,     4,   112,    98,   113,
      99,    -1,    -1,   114,    -1,   113,   100,   114,    -1,   113,
     100,    -1,   116,   119,    -1,   116,   120,    -1,   118,   117,
      -1,     8,    -1,     9,    -1,    10,    -1,    11,    -1,    12,
      -1,    13,    -1,     3,    -1,    -1,    15,    -1,    16,    -1,
     121,    -1,   119,   101,   121,    -1,   121,    -1,   120,   101,
     121,    -1,     4,   122,    -1,    -1,    89,   131,    -1,   102,
     124,   103,    -1,    -1,   124,   126,    -1,   126,    -1,   125,
     126,    -1,   115,   100,    -1,   136,   100,    -1,   102,   125,
     103,    -1,   127,   126,    -1,   128,    -1,   128,    64,   126,
      -1,    61,    98,   131,   100,   130,   100,   131,    99,    -1,
      62,    98,   130,    99,    -1,    65,    98,   131,   101,   131,
     101,   131,    99,    -1,   129,   126,    -1,    63,    98,   130,
      99,    -1,   131,    90,   131,    -1,   131,    82,   131,    -1,
     132,    -1,   131,    91,   131,    -1,   131,    92,   131,    -1,
     131,    93,   131,    -1,   131,    94,   131,    -1,   131,    95,
     131,    -1,    92,   131,    -1,   130,   104,   131,   105,   131,
      -1,    98,   131,    99,    -1,   135,   131,    -1,     5,    -1,
      44,    -1,     6,    -1,     4,    -1,   139,    -1,   137,    -1,
     136,    -1,   133,    -1,    98,   131,   101,   131,   101,   131,
      99,    -1,    -1,     6,    -1,    11,   134,    -1,     4,    89,
     131,    -1,     4,    51,   131,    -1,     4,    52,   131,    -1,
       4,    53,   131,    -1,    19,    98,   138,    99,    -1,    20,
      98,   138,    99,    -1,    21,    98,   138,    99,    -1,    33,
      98,   138,    99,    -1,    34,    98,   138,    99,    -1,    35,
      98,   138,    99,    -1,    36,    98,   138,    99,    -1,    37,
      98,   138,    99,    -1,    38,    98,   138,    99,    -1,    39,
      98,   138,    99,    -1,    40,    98,   138,    99,    -1,    41,
      98,   138,    99,    -1,    42,    98,   138,    99,    -1,    43,
      98,    99,    -1,    22,    98,   138,    99,    -1,    23,    98,
     138,    99,    -1,    24,    98,   138,    99,    -1,    25,    98,
     138,    99,    -1,    45,    98,   138,    99,    -1,    26,    98,
     138,    99,    -1,    27,    98,   138,    99,    -1,    32,    98,
     138,    99,    -1,    48,    98,   138,    99,    -1,    49,    98,
     138,    99,    -1,    50,    98,   138,    99,    -1,    17,    98,
     138,    99,    -1,    46,    98,   138,    99,    -1,    47,    98,
     138,    99,    -1,    28,    98,   138,    99,    -1,    29,    98,
     138,    99,    -1,    30,    98,   138,    99,    -1,    31,    98,
     138,    99,    -1,    54,    98,   138,    99,    -1,    55,    98,
     138,    99,    -1,    56,    98,   138,    99,    -1,    57,    98,
     138,    99,    -1,    58,    98,   138,    99,    -1,    59,    98,
     138,    99,    -1,    60,    98,   138,    99,    -1,     4,    98,
     138,    99,    -1,    -1,   131,    -1,   131,   101,   138,    -1,
     140,    98,   141,    99,    -1,    17,    -1,    18,    -1,   131,
      -1,   131,   101,   141,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   404,   404,   404,   407,   408,   411,   421,   421,   424,
     425,   426,   427,   430,   433,   436,   439,   443,   447,   451,
     455,   459,   463,   468,   469,   473,   478,   484,   491,   496,
     503,   515,   518,   526,   529,   530,   533,   534,   536,   537,
     542,   543,   548,   557,   567,   576,   585,   596,   608,   619,
     623,   631,   635,   639,   643,   647,   651,   656,   660,   665,
     669,   675,   679,   683,   688,   693,   696,   699,   702,   708,
     713,   714,   719,   723,   730,   737,   744,   754,   758,   762,
     766,   770,   774,   778,   782,   786,   790,   794,   798,   802,
     806,   810,   814,   818,   822,   829,   833,   837,   842,   847,
     851,   855,   859,   868,   873,   877,   882,   887,   891,   895,
     900,   905,   910,   915,   920,   925,   930,   940,   943,   950,
     958,   969,   976,   985,   991
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SURFACE", "IDENTIFIER", "NUMBER",
  "STRINGCONSTANT", "VOID", "FLOAT", "NORMAL", "VECTOR", "COLOR", "POINT",
  "STRING", "LIGHTSOURCE", "VARYING", "UNIFORM", "ENVIRONMENT", "TEXTURE",
  "RADIANS", "DEGREES", "ABS", "FLOOR", "CEIL", "ROUND", "MIX", "MOD",
  "NOISE", "STEP", "SMOOTHSTEP", "SQRT", "INVERSESQRT", "LENGTH", "SIN",
  "ASIN", "COS", "ACOS", "TAN", "ATAN", "POW", "EXP", "LOG", "SIGN",
  "RANDOM", "MATH_PI", "REFRACT", "OCCLUSION", "TRACE", "AMBIENT",
  "DIFFUSE", "SPECULAR", "PLUSEQ", "MINUSEQ", "MULEQ", "XCOMP", "YCOMP",
  "ZCOMP", "SETXCOMP", "SETYCOMP", "SETZCOMP", "AREA", "FOR", "WHILE",
  "IF", "ELSE", "ILLUMINANCE", "OPASSIGN", "OPVARDEF", "OPMUL", "OPDIV",
  "OPDOT", "OPADD", "OPSUB", "OPNEG", "OPFUNC", "OPFUNCARG", "OPASSIGNADD",
  "OPASSIGNSUB", "OPASSIGNMUL", "OPCOND", "OPPARENT", "OPLE", "OPNEQ",
  "OPFTOV", "OPFOR", "OPFORCOND", "OPWHILE", "OPIF", "OPILLUMINANCE",
  "'='", "'<'", "'+'", "'-'", "'*'", "'/'", "'.'", "TYPECAST", "UMINUS",
  "'('", "')'", "';'", "','", "'{'", "'}'", "'?'", "':'", "$accept",
  "definitions", "@1", "funclist", "function", "func_head", "@2",
  "formals", "formal_variable_definitions", "variable_definitions",
  "typespec", "type", "detail", "formal_def_expressions",
  "variable_def_expressions", "def_expression", "def_init", "block",
  "statements", "statement_list", "statement", "loop_control", "if_state",
  "if_control", "relation", "expression", "primary", "triple", "spacetype",
  "typecast", "assignexpression", "procedurecall", "proc_arguments",
  "texture", "texture_type", "texture_arguments", 0
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
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,    61,
      60,    43,    45,    42,    47,    46,   344,   345,    40,    41,
      59,    44,   123,   125,    63,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,   106,   108,   107,   109,   109,   110,   112,   111,   113,
     113,   113,   113,   114,   115,   116,   117,   117,   117,   117,
     117,   117,   117,   118,   118,   118,   119,   119,   120,   120,
     121,   122,   122,   123,   124,   124,   125,   125,   126,   126,
     126,   126,   126,   126,   127,   127,   127,   128,   129,   130,
     130,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   132,   132,   132,   132,   132,   132,   132,   132,   133,
     134,   134,   135,   136,   136,   136,   136,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   138,   138,   138,
     139,   140,   140,   141,   141
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     0,     2,     2,     0,     6,     0,
       1,     3,     2,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     1,     3,     1,     3,
       2,     0,     2,     3,     0,     2,     1,     2,     2,     2,
       3,     2,     1,     3,     8,     4,     8,     2,     4,     3,
       3,     1,     3,     3,     3,     3,     3,     2,     5,     3,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     7,
       0,     1,     2,     3,     3,     3,     3,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       3,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     0,     1,     3,
       4,     1,     1,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     4,     1,     3,    22,    16,    17,    18,    19,
      20,    21,     5,     0,     0,    34,     6,     7,    23,     0,
       0,    24,    25,     0,     0,     0,     0,    23,    33,     0,
       0,     0,    35,    23,    42,    23,     0,    23,     0,     0,
       0,     0,     0,     0,     0,     0,    23,    36,    38,    31,
      14,    28,    15,    41,    23,    47,    39,     0,    10,     0,
      64,    61,    63,    70,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    51,
      68,     0,    67,    66,    65,     0,    75,    76,    73,     0,
       0,     0,     0,     0,    40,    37,     0,    30,     0,    43,
       8,    23,    13,    26,   117,    71,    72,   117,   117,   117,
     117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
     117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
     117,   117,     0,   117,   117,   117,   117,   117,   117,   117,
     117,   117,   117,   117,   117,   117,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,    45,
      48,     0,    32,    29,    11,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    90,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,     0,     0,
      50,    49,    52,    53,    54,    55,    56,   123,     0,     0,
       0,    27,   117,   116,   102,    77,    78,    79,    91,    92,
      93,    94,    96,    97,   105,   106,   107,   108,    98,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    95,
     103,   104,    99,   100,   101,   109,   110,   111,   112,   113,
     114,   115,     0,     0,     0,   120,     0,     0,   119,     0,
      58,   124,     0,     0,     0,    44,    46,    69
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short int yydefgoto[] =
{
      -1,     1,     2,     4,    12,    13,    19,    57,    58,    29,
      30,    14,    31,   132,    50,    51,   127,    16,    18,    46,
      32,    33,    34,    35,   107,   196,   109,   110,   136,   111,
     112,   113,   197,   114,   115,   248
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -125
static const short int yypact[] =
{
    -125,    11,  -125,  -125,   122,  -125,  -125,  -125,  -125,  -125,
    -125,  -125,  -125,   -64,    62,  -125,  -125,  -125,    49,   -22,
      34,  -125,  -125,   -18,   -16,    -9,     0,    59,  -125,     9,
      96,   122,  -125,    59,    44,    59,    13,    39,   184,   184,
     184,   184,   184,   184,   184,   184,    54,  -125,  -125,    38,
      28,  -125,  -125,  -125,    59,  -125,  -125,     4,  -125,    96,
       8,  -125,  -125,   130,    57,  -125,    67,    75,    76,    78,
      80,    81,    83,    85,    86,    88,    89,    99,   100,   101,
     137,   138,   153,   154,   159,   162,   170,   171,   174,   176,
     179,  -125,   180,   181,   182,   183,   192,   194,   195,   197,
     198,   206,   208,   209,   211,   184,   184,    33,   155,  -125,
    -125,   184,  -125,  -125,  -125,   212,   155,   155,   155,     1,
     -42,   155,   -27,   193,  -125,  -125,   184,  -125,    96,  -125,
    -125,    63,    74,  -125,   184,  -125,  -125,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,    61,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   112,   172,   184,   184,
     184,   184,   184,   184,   184,   184,   112,   184,   184,  -125,
    -125,   184,   155,  -125,  -125,    96,   207,    97,   219,   220,
     222,   224,   225,   233,   234,   236,   238,   239,   247,   248,
     250,   252,   253,   261,   262,   264,   265,   266,   267,   275,
     276,   278,   279,  -125,   280,   281,   288,   289,   290,   292,
     293,   294,   295,   296,   297,   298,   299,  -125,   184,    77,
     155,   155,    98,    98,   -11,   -11,   112,   221,   300,   -48,
     235,  -125,   184,  -125,  -125,  -125,  -125,  -125,  -125,  -125,
    -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,
    -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,
    -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,
    -125,  -125,   249,   184,   184,  -125,   184,   184,  -125,   184,
     155,  -125,   263,   277,   291,  -125,  -125,  -125
};

/* YYPGOTO[NTERM-NUM].  */
static const short int yypgoto[] =
{
    -125,  -125,  -125,  -125,  -125,  -125,  -125,  -125,   139,  -125,
     -29,   260,  -125,  -125,  -125,   -47,  -125,  -125,  -125,  -125,
     131,  -125,  -125,  -125,   -34,   -38,  -125,  -125,  -125,  -125,
      72,  -125,  -124,  -125,  -125,   -41
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -13
static const short int yytable[] =
{
     108,   116,   117,   118,   119,   121,   121,   123,    59,   120,
     122,     3,   133,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,    15,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   296,    20,    21,    22,   178,   189,    20,    38,
      39,    40,   178,    20,    21,    22,    17,   176,   177,    21,
      22,   179,   190,   186,    21,    22,    37,   178,    21,    22,
      42,   193,    43,   179,   185,    38,    39,    40,   192,    44,
      36,   180,   181,   182,   183,   184,   185,    41,    45,    36,
      49,   188,    59,   130,   131,    36,   134,    36,    54,    48,
      23,    24,    25,    56,    26,    23,    24,    25,    36,    26,
      23,    24,    25,    41,    26,     5,    36,   126,   298,   128,
       6,     7,     8,     9,    10,    11,   135,   178,    -9,    -9,
     239,   240,   241,   242,   243,   244,   245,   246,   251,   247,
     121,    27,    28,   250,   249,   137,    27,   124,    47,   179,
     223,    27,   -12,   -12,    53,   138,    55,   180,   181,   182,
     183,   184,   185,   139,   140,   195,   141,   125,   142,   143,
     179,   144,   293,   145,   146,   129,   147,   148,    60,    61,
      62,   183,   184,   185,   179,    63,   253,   149,   150,   151,
     292,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,   152,   153,   179,    98,    99,
     100,   101,   102,   103,   104,   180,   181,   182,   183,   184,
     185,   154,   155,   301,   179,   300,   247,   156,   302,   303,
     157,   304,   180,   181,   182,   183,   184,   185,   158,   159,
     194,   237,   160,   238,   161,   179,   105,   162,   163,   164,
     165,   166,   106,   180,   181,   182,   183,   184,   185,   179,
     167,    52,   168,   169,   191,   170,   171,   180,   181,   182,
     183,   184,   185,   179,   172,     0,   173,   174,   252,   175,
     187,   180,   181,   182,   183,   184,   185,   179,   254,   255,
       0,   256,   294,   257,   258,   180,   181,   182,   183,   184,
     185,   179,   259,   260,     0,   261,   297,   262,   263,   180,
     181,   182,   183,   184,   185,   179,   264,   265,     0,   266,
     299,   267,   268,   180,   181,   182,   183,   184,   185,   179,
     269,   270,   305,   271,   272,   273,   274,   180,   181,   182,
     183,   184,   185,   179,   275,   276,   306,   277,   278,   279,
     280,   180,   181,   182,   183,   184,   185,   281,   282,   283,
     307,   284,   285,   286,   287,   288,   289,   290,   291,   295
};

static const short int yycheck[] =
{
      38,    39,    40,    41,    42,    43,    44,    45,    37,    43,
      44,     0,    59,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   156,   157,   158,   159,   160,   161,   102,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   100,     4,    15,    16,   104,    99,     4,    51,
      52,    53,   104,     4,    15,    16,     4,   105,   106,    15,
      16,    82,    99,   111,    15,    16,    98,   104,    15,    16,
      98,   128,    98,    82,    95,    51,    52,    53,   126,    98,
      18,    90,    91,    92,    93,    94,    95,    89,    98,    27,
       4,   100,   131,    99,   100,    33,    98,    35,    64,   100,
      61,    62,    63,   100,    65,    61,    62,    63,    46,    65,
      61,    62,    63,    89,    65,     3,    54,    89,   252,   101,
       8,     9,    10,    11,    12,    13,     6,   104,    99,   100,
     178,   179,   180,   181,   182,   183,   184,   185,   195,   187,
     188,   102,   103,   191,   188,    98,   102,   103,    27,    82,
      99,   102,    99,   100,    33,    98,    35,    90,    91,    92,
      93,    94,    95,    98,    98,   101,    98,    46,    98,    98,
      82,    98,   105,    98,    98,    54,    98,    98,     4,     5,
       6,    93,    94,    95,    82,    11,    99,    98,    98,    98,
     238,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    98,    98,    82,    54,    55,
      56,    57,    58,    59,    60,    90,    91,    92,    93,    94,
      95,    98,    98,   294,    82,   293,   294,    98,   296,   297,
      98,   299,    90,    91,    92,    93,    94,    95,    98,    98,
     131,    99,    98,   101,    98,    82,    92,    98,    98,    98,
      98,    98,    98,    90,    91,    92,    93,    94,    95,    82,
      98,    31,    98,    98,   101,    98,    98,    90,    91,    92,
      93,    94,    95,    82,    98,    -1,    98,    98,   101,    98,
      98,    90,    91,    92,    93,    94,    95,    82,    99,    99,
      -1,    99,   101,    99,    99,    90,    91,    92,    93,    94,
      95,    82,    99,    99,    -1,    99,   101,    99,    99,    90,
      91,    92,    93,    94,    95,    82,    99,    99,    -1,    99,
     101,    99,    99,    90,    91,    92,    93,    94,    95,    82,
      99,    99,    99,    99,    99,    99,    99,    90,    91,    92,
      93,    94,    95,    82,    99,    99,    99,    99,    99,    99,
      99,    90,    91,    92,    93,    94,    95,    99,    99,    99,
      99,    99,    99,    99,    99,    99,    99,    99,    99,    99
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,   107,   108,     0,   109,     3,     8,     9,    10,    11,
      12,    13,   110,   111,   117,   102,   123,     4,   124,   112,
       4,    15,    16,    61,    62,    63,    65,   102,   103,   115,
     116,   118,   126,   127,   128,   129,   136,    98,    51,    52,
      53,    89,    98,    98,    98,    98,   125,   126,   100,     4,
     120,   121,   117,   126,    64,   126,   100,   113,   114,   116,
       4,     5,     6,    11,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    54,    55,
      56,    57,    58,    59,    60,    92,    98,   130,   131,   132,
     133,   135,   136,   137,   139,   140,   131,   131,   131,   131,
     130,   131,   130,   131,   103,   126,    89,   122,   101,   126,
      99,   100,   119,   121,    98,     6,   134,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,   131,   131,   104,    82,
      90,    91,    92,    93,    94,    95,   131,    98,   100,    99,
      99,   101,   131,   121,   114,   101,   131,   138,   138,   138,
     138,   138,   138,   138,   138,   138,   138,   138,   138,   138,
     138,   138,   138,   138,   138,   138,   138,   138,   138,   138,
     138,   138,   138,    99,   138,   138,   138,   138,   138,   138,
     138,   138,   138,   138,   138,   138,   138,    99,   101,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   141,   130,
     131,   121,   101,    99,    99,    99,    99,    99,    99,    99,
      99,    99,    99,    99,    99,    99,    99,    99,    99,    99,
      99,    99,    99,    99,    99,    99,    99,    99,    99,    99,
      99,    99,    99,    99,    99,    99,    99,    99,    99,    99,
      99,    99,   131,   105,   101,    99,   100,   101,   138,   101,
     131,   141,   131,   131,   131,    99,    99,    99
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
        case 2:
#line 404 "parsesl.y"
    { write_header(); }
    break;

  case 6:
#line 412 "parsesl.y"
    {
				write_paraminitializer();

				fprintf(g_csfp, "\n");
			
				write_body();
			}
    break;

  case 7:
#line 421 "parsesl.y"
    { init_func_body(yyvsp[0].string); }
    break;

  case 16:
#line 440 "parsesl.y"
    {
				vartype = FLOAT;
			}
    break;

  case 17:
#line 444 "parsesl.y"
    {
				vartype = NORMAL;
			}
    break;

  case 18:
#line 448 "parsesl.y"
    {
				vartype = VECTOR;
			}
    break;

  case 19:
#line 452 "parsesl.y"
    {
				vartype = COLOR;
			}
    break;

  case 20:
#line 456 "parsesl.y"
    {
				vartype = POINT;
			}
    break;

  case 21:
#line 460 "parsesl.y"
    {
				vartype = STRING;
			}
    break;

  case 22:
#line 464 "parsesl.y"
    {
			}
    break;

  case 24:
#line 470 "parsesl.y"
    {
				/* do nothing */
			}
    break;

  case 25:
#line 474 "parsesl.y"
    {
				/* do nothing */
			}
    break;

  case 26:
#line 479 "parsesl.y"
    {
				f.formalexprlist[f.nformalexpr] = yyvsp[0].np;
				f.nformalexpr++;
	
			}
    break;

  case 27:
#line 485 "parsesl.y"
    {
				f.formalexprlist[f.nformalexpr] = yyvsp[0].np;
				f.nformalexpr++;
			}
    break;

  case 28:
#line 492 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = yyvsp[0].np;
				f.nexpr++;
			}
    break;

  case 29:
#line 497 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = yyvsp[0].np;
				f.nexpr++;
			}
    break;

  case 30:
#line 504 "parsesl.y"
    {
				var_reg(yyvsp[-1].string, vartype);
				
				yyval.np = make_node(OPVARDEF,
				     make_leaf(yyvsp[-1].string),
				     yyvsp[0].np);
			}
    break;

  case 31:
#line 515 "parsesl.y"
    {
				yyval.np = NULL;
			}
    break;

  case 32:
#line 519 "parsesl.y"
    {
				yyval.np = yyvsp[0].np;
			}
    break;

  case 39:
#line 538 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = yyvsp[-1].np;	
				f.nexpr++;
			}
    break;

  case 41:
#line 544 "parsesl.y"
    {
				f.blocks = pop_block(f.blocks);
				f.exprlist[f.blocks->block].end = f.nexpr - 1;
			}
    break;

  case 42:
#line 549 "parsesl.y"
    {
				int pos;

				f.blocks = pop_block(f.blocks);
				pos = f.blocks->block;
				f.exprlist[pos].elsestart = 0; 
				f.exprlist[pos].elseend = 0; 
			}
    break;

  case 43:
#line 558 "parsesl.y"
    {
				int pos;

				f.blocks = pop_block(f.blocks);
				pos = f.blocks->block;
				f.exprlist[pos].elseend = f.nexpr - 1;
			}
    break;

  case 44:
#line 568 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = make_for(yyvsp[-5].np, yyvsp[-3].np, yyvsp[-1].np);
				f.exprlist[f.nexpr].start = f.nexpr + 1;
				f.exprlist[f.nexpr].indent = f.currblockdepth;
				f.blocks->block = f.nexpr;
				f.blocks = push_block(f.blocks);
				f.nexpr++;
			}
    break;

  case 45:
#line 577 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = make_while(yyvsp[-1].np);
				f.exprlist[f.nexpr].start = f.nexpr + 1;
				f.exprlist[f.nexpr].indent = f.currblockdepth;
				f.blocks->block = f.nexpr;
				f.blocks = push_block(f.blocks);
				f.nexpr++;
			}
    break;

  case 46:
#line 586 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = make_illuminance(yyvsp[-5].np, yyvsp[-3].np, yyvsp[-1].np);
				f.exprlist[f.nexpr].start = f.nexpr + 1;
				f.exprlist[f.nexpr].indent = f.currblockdepth;
				f.blocks->block = f.nexpr;
				f.blocks = push_block(f.blocks);
				f.nexpr++;
			}
    break;

  case 47:
#line 597 "parsesl.y"
    {
				int pos;

				pos = f.blocks->prev->block;
				f.exprlist[pos].end       = f.nexpr - 1;
				f.exprlist[pos].elsestart = f.nexpr;
				f.exprlist[pos].elseend   = 0;
				
			}
    break;

  case 48:
#line 609 "parsesl.y"
    {
				f.exprlist[f.nexpr].expr = make_if(yyvsp[-1].np);
				f.exprlist[f.nexpr].start = f.nexpr + 1;
				f.exprlist[f.nexpr].indent = f.currblockdepth;
				f.blocks->block = f.nexpr;
				f.blocks = push_block(f.blocks);
				f.nexpr++;
			}
    break;

  case 49:
#line 620 "parsesl.y"
    {
				yyval.np = make_node(OPLE, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 50:
#line 624 "parsesl.y"
    {
				yyval.np = make_node(OPNEQ, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 51:
#line 632 "parsesl.y"
    {
				yyval.np = yyvsp[0].np;
			}
    break;

  case 52:
#line 636 "parsesl.y"
    {
				yyval.np = make_node(OPADD, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 53:
#line 640 "parsesl.y"
    {
				yyval.np = make_node(OPSUB, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 54:
#line 644 "parsesl.y"
    {
				yyval.np = make_node(OPMUL, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 55:
#line 648 "parsesl.y"
    {
				yyval.np = make_node(OPDIV, yyvsp[-2].np, yyvsp[0].np);
			}
    break;

  case 56:
#line 652 "parsesl.y"
    {
				yyval.np = make_node(OPDOT, yyvsp[-2].np, yyvsp[0].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 57:
#line 657 "parsesl.y"
    {
				yyval.np = make_node(OPNEG, yyvsp[0].np, NULL);
			}
    break;

  case 58:
#line 661 "parsesl.y"
    {
				yyval.np = make_cond(yyvsp[-4].np, yyvsp[-2].np, yyvsp[0].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 59:
#line 666 "parsesl.y"
    {
				yyval.np = make_node(OPPARENT, yyvsp[-1].np, NULL);
			}
    break;

  case 60:
#line 670 "parsesl.y"
    {
				yyval.np = yyvsp[0].np;
			}
    break;

  case 61:
#line 676 "parsesl.y"
    {
				yyval.np = make_constnum(yyvsp[0].fval);
			}
    break;

  case 62:
#line 680 "parsesl.y"
    {
				yyval.np = make_constnum(3.141593);
			}
    break;

  case 63:
#line 684 "parsesl.y"
    {
				var_reg(yyvsp[0].string, vartype);
				yyval.np = make_conststr(yyvsp[0].string);
			}
    break;

  case 64:
#line 689 "parsesl.y"
    {
				var_reg(yyvsp[0].string, vartype);
				yyval.np = make_leaf(yyvsp[0].string);
			}
    break;

  case 65:
#line 694 "parsesl.y"
    {
			}
    break;

  case 66:
#line 697 "parsesl.y"
    {
			}
    break;

  case 67:
#line 700 "parsesl.y"
    {
			}
    break;

  case 68:
#line 703 "parsesl.y"
    {
				yyval.np = yyvsp[0].np;
			}
    break;

  case 69:
#line 709 "parsesl.y"
    {
				yyval.np = make_triple(yyvsp[-5].np, yyvsp[-3].np, yyvsp[-1].np);
			}
    break;

  case 71:
#line 715 "parsesl.y"
    {
			}
    break;

  case 73:
#line 724 "parsesl.y"
    {
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OPASSIGN, 
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			}
    break;

  case 74:
#line 731 "parsesl.y"
    {
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OPASSIGNADD, 
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			}
    break;

  case 75:
#line 738 "parsesl.y"
    {
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OPASSIGNSUB, 
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			}
    break;

  case 76:
#line 745 "parsesl.y"
    {
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OPASSIGNMUL, 
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			}
    break;

  case 77:
#line 755 "parsesl.y"
    {
				yyval.np = make_node(RADIANS, NULL, yyvsp[-1].np);
			}
    break;

  case 78:
#line 759 "parsesl.y"
    {
				yyval.np = make_node(DEGREES, NULL, yyvsp[-1].np);
			}
    break;

  case 79:
#line 763 "parsesl.y"
    {
				yyval.np = make_node(ABS, NULL, yyvsp[-1].np);
			}
    break;

  case 80:
#line 767 "parsesl.y"
    {
				yyval.np = make_node(SIN, NULL, yyvsp[-1].np);
			}
    break;

  case 81:
#line 771 "parsesl.y"
    {
				yyval.np = make_node(ASIN, NULL, yyvsp[-1].np);
			}
    break;

  case 82:
#line 775 "parsesl.y"
    {
				yyval.np = make_node(COS, NULL, yyvsp[-1].np);
			}
    break;

  case 83:
#line 779 "parsesl.y"
    {
				yyval.np = make_node(ACOS, NULL, yyvsp[-1].np);
			}
    break;

  case 84:
#line 783 "parsesl.y"
    {
				yyval.np = make_node(TAN, NULL, yyvsp[-1].np);
			}
    break;

  case 85:
#line 787 "parsesl.y"
    {
				yyval.np = make_node(ATAN, NULL, yyvsp[-1].np);
			}
    break;

  case 86:
#line 791 "parsesl.y"
    {
				yyval.np = make_node(POW, NULL, yyvsp[-1].np);
			}
    break;

  case 87:
#line 795 "parsesl.y"
    {
				yyval.np = make_node(EXP, NULL, yyvsp[-1].np);
			}
    break;

  case 88:
#line 799 "parsesl.y"
    {
				yyval.np = make_node(LOG, NULL, yyvsp[-1].np);
			}
    break;

  case 89:
#line 803 "parsesl.y"
    {
				yyval.np = make_node(SIGN, NULL, yyvsp[-1].np);
			}
    break;

  case 90:
#line 807 "parsesl.y"
    {
				yyval.np = make_node(RANDOM, NULL, NULL);
			}
    break;

  case 91:
#line 811 "parsesl.y"
    {
				yyval.np = make_node(FLOOR, NULL, yyvsp[-1].np);
			}
    break;

  case 92:
#line 815 "parsesl.y"
    {
				yyval.np = make_node(CEIL, NULL, yyvsp[-1].np);
			}
    break;

  case 93:
#line 819 "parsesl.y"
    {
				yyval.np = make_node(ROUND, NULL, yyvsp[-1].np);
			}
    break;

  case 94:
#line 823 "parsesl.y"
    {
				yyval.np = make_node(MIX, NULL, yyvsp[-1].np);	
				if (yyvsp[-1].np) {
					yyval.np->type = yyvsp[-1].np->type;
				}
			}
    break;

  case 95:
#line 830 "parsesl.y"
    {
				yyval.np = make_node(REFRACT, NULL, yyvsp[-1].np);	
			}
    break;

  case 96:
#line 834 "parsesl.y"
    {
				yyval.np = make_node(MOD, NULL, yyvsp[-1].np);	
			}
    break;

  case 97:
#line 838 "parsesl.y"
    {
				yyval.np = make_node(NOISE, NULL, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			}
    break;

  case 98:
#line 843 "parsesl.y"
    {
				yyval.np = make_node(LENGTH, NULL, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			}
    break;

  case 99:
#line 848 "parsesl.y"
    {
				yyval.np = make_node(AMBIENT, NULL, yyvsp[-1].np);	
			}
    break;

  case 100:
#line 852 "parsesl.y"
    {
				yyval.np = make_node(DIFFUSE, NULL, yyvsp[-1].np);	
			}
    break;

  case 101:
#line 856 "parsesl.y"
    {
				yyval.np = make_node(SPECULAR, NULL, yyvsp[-1].np);	
			}
    break;

  case 102:
#line 860 "parsesl.y"
    {
				yyval.np = make_node(ENVIRONMENT, NULL, yyvsp[-1].np);	
			}
    break;

  case 103:
#line 869 "parsesl.y"
    {
				yyval.np = make_node(OCCLUSION, NULL, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			}
    break;

  case 104:
#line 874 "parsesl.y"
    {
				yyval.np = make_node(TRACE, NULL, yyvsp[-1].np);	
			}
    break;

  case 105:
#line 878 "parsesl.y"
    {
				yyval.np = make_node(STEP, NULL, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			}
    break;

  case 106:
#line 883 "parsesl.y"
    {
				yyval.np = make_node(SMOOTHSTEP, NULL, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			}
    break;

  case 107:
#line 888 "parsesl.y"
    {
				yyval.np = make_node(SQRT, NULL, yyvsp[-1].np);
			}
    break;

  case 108:
#line 892 "parsesl.y"
    {
				yyval.np = make_node(INVERSESQRT, NULL, yyvsp[-1].np);
			}
    break;

  case 109:
#line 896 "parsesl.y"
    {
				yyval.np = make_node(XCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 110:
#line 901 "parsesl.y"
    {
				yyval.np = make_node(YCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 111:
#line 906 "parsesl.y"
    {
				yyval.np = make_node(ZCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 112:
#line 911 "parsesl.y"
    {
				yyval.np = make_node(SETXCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = VOID;
			}
    break;

  case 113:
#line 916 "parsesl.y"
    {
				yyval.np = make_node(SETYCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = VOID;
			}
    break;

  case 114:
#line 921 "parsesl.y"
    {
				yyval.np = make_node(SETZCOMP, NULL, yyvsp[-1].np);
				yyval.np->type = VOID;
			}
    break;

  case 115:
#line 926 "parsesl.y"
    {
				yyval.np = make_node(AREA, NULL, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			}
    break;

  case 116:
#line 931 "parsesl.y"
    {
				var_reg(yyvsp[-3].string, VECTOR);
				yyval.np = make_node(OPFUNC,
					       make_leaf(yyvsp[-3].string),
					       yyvsp[-1].np);
			}
    break;

  case 117:
#line 940 "parsesl.y"
    {
				yyval.np = NULL;
			}
    break;

  case 118:
#line 944 "parsesl.y"
    {
				yyval.np = make_node(OPFUNCARG,
					       yyvsp[0].np,
					       NULL);
				//$$ = $1;
			}
    break;

  case 119:
#line 951 "parsesl.y"
    {
				yyval.np = make_node(OPFUNCARG,
					       yyvsp[-2].np,
					       yyvsp[0].np);
			}
    break;

  case 120:
#line 960 "parsesl.y"
    {
				yyval.np = make_node(TEXTURE,
					       yyvsp[-3].np,
					       yyvsp[-1].np);
				//$$->type = COLOR;

			}
    break;

  case 121:
#line 970 "parsesl.y"
    {
				char *tex = "environment";
				var_reg(tex, COLOR);

				yyval.np = make_leaf(tex);
			}
    break;

  case 122:
#line 977 "parsesl.y"
    {
				char *tex = "texture";
				var_reg(tex, COLOR);

				yyval.np = make_leaf(tex);
			}
    break;

  case 123:
#line 986 "parsesl.y"
    {
				yyval.np = make_node(OPFUNCARG,
					       yyvsp[0].np,
					       NULL);
			}
    break;

  case 124:
#line 992 "parsesl.y"
    {
				yyval.np = make_node(OPFUNCARG,
					       yyvsp[-2].np,
					       yyvsp[0].np);
			}
    break;


    }

/* Line 1010 of yacc.c.  */
#line 2607 "parsesl.c"

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


#line 999 "parsesl.y"


#if 0
int
main(int argc, char **argv)
{
	extern FILE *yyin;
	extern int yydebug;

	if (argc < 2) {
		printf("usage: %s file.sl\n", argv[0]);
		exit(-1);
	}

	yyin = fopen(argv[1], "r");

	//yydebug = 1;

	yyparse();

	return 0;
}
#endif

int
get_parsed_line()
{
	return nlines;
}

void
yyerror(char *s)
{
	fprintf(stderr, "Parse error: %s at line %d\n", s, nlines);
}


