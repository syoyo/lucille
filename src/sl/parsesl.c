
/*  A Bison parser, made from parsesl.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	SURFACE	257
#define	IDENTIFIER	258
#define	NUMBER	259
#define	STRINGCONSTANT	260
#define	VOID	261
#define	FLOAT	262
#define	NORMAL	263
#define	VECTOR	264
#define	COLOR	265
#define	POINT	266
#define	STRING	267
#define	LIGHTSOURCE	268
#define	VARYING	269
#define	UNIFORM	270
#define	ENVIRONMENT	271
#define	TEXTURE	272
#define	RADIANS	273
#define	DEGREES	274
#define	ABS	275
#define	FLOOR	276
#define	CEIL	277
#define	ROUND	278
#define	MIX	279
#define	MOD	280
#define	NOISE	281
#define	STEP	282
#define	SMOOTHSTEP	283
#define	SQRT	284
#define	INVERSESQRT	285
#define	LENGTH	286
#define	SIN	287
#define	ASIN	288
#define	COS	289
#define	ACOS	290
#define	TAN	291
#define	ATAN	292
#define	POW	293
#define	EXP	294
#define	LOG	295
#define	SIGN	296
#define	RANDOM	297
#define	MATH_PI	298
#define	REFRACT	299
#define	OCCLUSION	300
#define	TRACE	301
#define	AMBIENT	302
#define	DIFFUSE	303
#define	SPECULAR	304
#define	PLUSEQ	305
#define	MINUSEQ	306
#define	MULEQ	307
#define	XCOMP	308
#define	YCOMP	309
#define	ZCOMP	310
#define	SETXCOMP	311
#define	SETYCOMP	312
#define	SETZCOMP	313
#define	TRIPLE	314
#define	AREA	315
#define	FOR	316
#define	WHILE	317
#define	IF	318
#define	ELSE	319
#define	ILLUMINANCE	320
#define	OP_NULL	321
#define	OP_ASSIGN	322
#define	OP_VARDEF	323
#define	OP_DEFEXPR	324
#define	OP_FORMAL_DEFEXPR	325
#define	OP_MUL	326
#define	OP_DIV	327
#define	OP_DOT	328
#define	OP_ADD	329
#define	OP_SUB	330
#define	OP_NEG	331
#define	OP_FUNC	332
#define	OP_FUNCARG	333
#define	OP_FUNC_HEADER	334
#define	OP_CALLFUNC	335
#define	OP_ASSIGNADD	336
#define	OP_ASSIGNSUB	337
#define	OP_ASSIGNMUL	338
#define	OP_COND	339
#define	OP_COND_TRIPLE	340
#define	OP_PARENT	341
#define	OP_LE	342
#define	OP_GE	343
#define	OP_EQ	344
#define	OP_NEQ	345
#define	OP_FTOV	346
#define	OP_FOR	347
#define	OP_FORCOND	348
#define	OP_WHILE	349
#define	OP_IF	350
#define	OP_ILLUMINANCE	351
#define	OP_IF_ELSE	352
#define	OP_STMT	353
#define	UMINUS	354
#define	TYPECAST	355

#line 1 "parsesl.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "tree.h"
#include "sl2c.h"

/* ---------------------------------------------------------------------------
 *
 * Notes.
 *
 *   Usually, shader code is small compared to other languages(C/C++, etc).
 *   Thus I use basic data structure(doubly-linked list, linear search, etc)
 *   and don't use efficient data structure(hash, binary search, etc).
 *
 * ------------------------------------------------------------------------ */

/*
 * List of nodes
 */
typedef struct _node_list_t
{
	node_t               *expr;
	
	/*
	 * Doubly-linked list
	 */
	struct _node_list_t  *next;
	struct _node_list_t  *prev;

} node_list_t;

typedef struct _stmt_block_t
{

	node_list_t          *node_list;

	int                   depth;
	int                   type;


} stmt_block_t;

typedef struct _stmt_block_list_t
{
	stmt_block_t               *block;

	/*
	 * Doubly-linked list.
	 */
	struct _stmt_block_list_t  *next;
	struct _stmt_block_list_t  *prev;

} stmt_block_list_t;

typedef struct _function_t
{
	stmt_block_list_t    *stmt_block_list;

	//int             currblockdepth;

	node_list_t          *expr_list;
	node_list_t          *formal_expr_list;

#if 0	/* to be removed */
	int             nexpr;
	int             nformalexpr;
	int             nscalarexpr;
#endif

	char                 *func_name;

} function_t;

typedef struct _function_list_t
{
	function_t *func;

	/*
	 * Doubly-linked list.
	 */
	struct _function_list_t *next;
	struct _function_list_t *prev;

} function_list_t;

/*
 * Abstract syntax tree
 */
typedef struct _ast_t
{
	function_list_t *func_list;

} ast_t;


/* ---------------------------------------------------------------------------
 *
 * Defines
 *
 * ------------------------------------------------------------------------ */

#define BLOCK_BEGIN      1
#define BLOCK_END        2

/*
 * gcc specific.
 */
#define SL2C_DEBUG(fmt, ...) { \
	if (g_debug) { \
		printf("%s:%d " fmt, __FUNCTION__, __LINE__, ## __VA_ARGS__); \
	} \
}
 
/* ---------------------------------------------------------------------------
 *
 * Globals
 *
 * ------------------------------------------------------------------------ */

/* defined in sl2c.c */
extern char sl2c_verstr[];
extern char sl2c_datestr[]; 
extern char sl2c_progname[]; 


int g_debug = 1;

ast_t       ast;		/* global singletion	*/


// TODO: to be removed
//func_body_t f;			/* gloal singleton */
function_t  *curr_func;		/* ptr to current function */
stmt_block_t      *curr_stmt_block;      /* ptr to current statement block */
stmt_block_list_t *curr_stmt_block_list; /* ptr to current statement block list */
node_list_t *curr_node_list;	/* ptr to current node list */

static int vartype;		/* type of variable */

extern int nlines;		/* defined in lexsl.l */
int        get_parsed_line();

void	   yyerror(char *s);

static void emit_header();
static void emit_function(function_t *func);
static void emit_block(stmt_block_t *block);
static void emit_func_arg();
static void emit_param_initializer(const function_t *func);

static void init_function(char *func_name);

static node_list_t *
append_node(node_list_t *node_list, node_t *node)
{
	node_list_t *new_list = (node_list_t *)malloc(sizeof(node_list_t));
	assert(new_list);

	new_list->expr = node;

	if (node_list != NULL) {

		new_list->prev = node_list;
		new_list->next = NULL;

		node_list->next = new_list;

	} else {

		new_list->prev = NULL;
		new_list->next = NULL;
	}

	return new_list;
}

static function_list_t *
append_function(function_list_t *func_list, function_t *func)
{
	function_list_t *new_list = (function_list_t *)malloc(
					sizeof(function_list_t));
	assert(new_list);

	new_list->func = func;

	if (func_list != NULL) {

		new_list->prev = func_list;
		new_list->next = NULL;

		func_list->next = new_list;

	} else {

		new_list->prev = NULL;
		new_list->next = NULL;
	}

	return new_list;
}

static stmt_block_t *
new_stmt_block()
{
	stmt_block_t *new_stmt = (stmt_block_t *)malloc(sizeof(stmt_block_t));
	assert(new_stmt);

	new_stmt->node_list = NULL;

	return new_stmt;
}

static stmt_block_list_t *
append_stmt_block(stmt_block_list_t *stmt_block_list, stmt_block_t *block)
{
	stmt_block_list_t *new_list = (stmt_block_list_t *)malloc(
					sizeof(stmt_block_list_t));
	assert(new_list);

	new_list->block = block;

	if (stmt_block_list != NULL) {

		new_list->prev = stmt_block_list;
		new_list->next = NULL;

		stmt_block_list->next = new_list;

	} else {

		new_list->prev = NULL;
		new_list->next = NULL;
	}

	return new_list;
}


static void
emit_header()
{
	fprintf(g_csfp, "/*				\n");
	fprintf(g_csfp, " * Generated by %s. (version %s)	\n",
		sl2c_progname, sl2c_verstr);
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
emit_function(function_t *func)
{
	sym_t *sp;

	// TODO:
	return;

	fprintf(g_csfp, "DLLEXPORT void\n");
	fprintf(g_csfp, "%s", func->func_name);
	emit_func_arg();
	fprintf(g_csfp, "{\n");

	
			
	/*
	 * Write list of formal variable definiton.
	 */
	{
		node_list_t *formal_expr_list;

		formal_expr_list = func->formal_expr_list;
		while (formal_expr_list != NULL) {

			emit_defvar(formal_expr_list->expr);

			formal_expr_list = formal_expr_list->next;
		}

		fprintf(g_csfp, "\n");
	}


	/*
	 * Write list of variable definition.
	 */
	{

		node_list_t *expr_list;

		expr_list = func->expr_list;
		while (expr_list != NULL) {

			emit_defvar(expr_list->expr);

			expr_list = expr_list->next;
		}

		fprintf(g_csfp, "\n");
	}

	/*
	 * Write param initializer code.
 	 */
	{
		node_list_t *expr_list;

		expr_list = func->formal_expr_list;

		while (expr_list != NULL) {

			if (expr_list->expr->opcode != OP_VARDEF) continue;


			sp = (sym_t *)expr_list->expr->left->left;

			fprintf(g_csfp,
				"\tri_param_eval(&%s, param, \"%s\");\n",
				sp->name, sp->name);

			expr_list = expr_list->next;
		}

		fprintf(g_csfp, "\n");
	}

	/*
	 * Write expressions
	 */
	{
		node_list_t  *expr_list;
		node_t       *expr;

		while (expr_list != NULL) {

			expr = expr_list->expr;

			if (expr->opcode == OP_FOR   ||
			    expr->opcode == OP_WHILE ||
			    expr->opcode == OP_IF    ||
			    expr->opcode == OP_ILLUMINANCE) {

				// TODO:
				//emit_block(&i);

			} else if (expr->type == FLOAT) {

				if (expr->opcode == OP_VARDEF &&
				    !expr->right) {

					/*
					 * do nothing
					 */

				} else {

					emit_scalar_node(expr);
					fprintf(g_csfp, ";\n");

				}

			} else {

				emit_node(expr);

			}

			expr_list = expr_list->next;
		}
	}

	fprintf(g_csfp, "}\n");
}

static void
emit_stmt(stmt_block_t *stmt)
{
	(void)stmt;
#if 0 // TODO: implement
	int i, indent;
	int start, end;
	int elsestart, elseend;

#if 0
	start     = expr_list->start;
	end       = expr_list->end;
	elsestart = expr_list->elsestart;
	elseend   = expr_list->elseend;
#endif

	indent = stmt->depth;

	set_indent(indent);

	emit_scalar_node(expr_list->expr);
	fprintf(g_csfp, "\n");

	i = start;
	while (i <= end) {
		if (expr_list->expr->opcode == OP_FOR   ||
		    expr_list->expr->opcode == OP_WHILE ||
		    expr_list->expr->opcode == OP_IF    ||
		    expr_list->expr->opcode == OP_ILLUMINANCE) {
			emit_block(&i);
		} else if (expr_list->expr->type == FLOAT) {
			set_indent(indent + 1);
			emit_scalar_node(exprlist->expr);
			fprintf(g_csfp, ";\n");
		} else {
			set_indent(indent + 1);
			emit_node(f.exprlist[i].expr);
		}

		i++;
	}

	set_indent(indent);
	emit_indent();
	fprintf(g_csfp, "\t}");

	if (elseend != 0) {
		fprintf(g_csfp, " else {\n");
		i = elsestart;
		while (i <= elseend) {
			if (f.exprlist[i].expr->opcode == OP_FOR   ||
			    f.exprlist[i].expr->opcode == OP_WHILE ||
			    f.exprlist[i].expr->opcode == OP_IF    ||
			    f.exprlist[i].expr->opcode == OP_ILLUMINANCE) {
				emit_block(&i);
			} else if (f.exprlist[i].expr->type == FLOAT) {
				set_indent(indent + 1);
				emit_scalar_node(f.exprlist[i].expr);
				fprintf(g_csfp, ";\n");
			} else {
				set_indent(indent + 1);
				emit_node(f.exprlist[i].expr);
			}

			i++;
		}

		set_indent(indent);
		emit_indent();
		fprintf(g_csfp, "\t}");
	}

	fprintf(g_csfp, "\n");
	set_indent(indent);

	if (elseend == 0) {
		*curr = end;
	} else {
		*curr = elseend;
	}
#endif

}

static void
emit_func_arg()
{
	fprintf(g_csfp, "(ri_output_t *output, ");
	fprintf(g_csfp, "ri_status_t *status, ");
	fprintf(g_csfp, "ri_parameter_t *param)\n");
}

static void
emit_param_initializer(const function_t *func)
{
	//int i;
	//sym_t *sp;
	//char buf[256];

	fprintf(g_csfp, "DLLEXPORT void\n");
	fprintf(g_csfp, "%s_initparam(ri_parameter_t *param)\n",
			func->func_name);
	fprintf(g_csfp, "{\n");

#if 0	// TODO: implement
	for (i = 0; i < f.nformalexpr; i++) {
		emit_tmpvar(f.formalexprlist[i]);
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nformalexpr; i++) {
		if (f.formalexprlist[i]->type == FLOAT) {
			emit_scalar_node(f.formalexprlist[i]);
			fprintf(g_csfp, ";\n");
		} else {
			emit_node(f.formalexprlist[i]);
		}
	}

	fprintf(g_csfp, "\n");

	for (i = 0; i < f.nformalexpr; i++) {
		if (f.formalexprlist[i]->opcode != OP_VARDEF) continue;

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
#endif
	fprintf(g_csfp, "}\n");
}

/*
 * Initialize Abstract Syntax Tree.
 */
void
init_ast()
{
	SL2C_DEBUG("init_ast()\n");

	ast.func_list = NULL;
}

void
init_function(char *func_name)
{
	//static int first = 1;

	SL2C_DEBUG("Enter function def: %s\n", func_name);

	curr_func = (function_t *)malloc(sizeof(function_t));
	assert(curr_func);
	curr_func->func_name = strdup(func_name);

#if 0	// TODO: implement
	if (first) {
		f.blocks = (block_t *)malloc(sizeof(block_t));
		f.blocks->prev = NULL;
		first = 0;

	} else {
		while ((f.blocks = pop_block(f.blocks)) != NULL);

		free(f.func_name);
	}

	f.nexpr          = 0;
	f.nformalexpr    = 0; 
	f.nscalarexpr    = 0;

	f.func_name	 = strdup(func_name);
#endif

}


#line 599 "parsesl.y"
typedef union {
	char   *string;
	node_t *np;
	double  fval;
	int     ival;
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



#define	YYFINAL		308
#define	YYFLAG		-32768
#define	YYNTBASE	118

#define YYTRANSLATE(x) ((unsigned)(x) <= 355 ? yytranslate[x] : 149)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,   109,
   110,   104,   102,   112,   103,   106,   105,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,   117,   111,   101,
   100,   115,   116,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,   113,     2,   114,     2,     2,     2,     2,     2,
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
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
    97,    98,    99,   107,   108
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     1,     4,     5,     8,    11,    17,    18,    20,    24,
    27,    30,    33,    36,    38,    40,    42,    44,    46,    48,
    50,    51,    53,    55,    57,    61,    63,    67,    70,    71,
    74,    78,    79,    82,    84,    87,    90,    94,    97,   103,
   111,   120,   125,   134,   138,   142,   146,   150,   152,   156,
   160,   164,   168,   172,   175,   181,   185,   188,   190,   192,
   194,   196,   198,   200,   202,   204,   212,   213,   215,   218,
   222,   226,   230,   234,   239,   244,   249,   254,   259,   264,
   269,   274,   279,   284,   289,   294,   299,   303,   308,   313,
   318,   323,   328,   333,   338,   343,   348,   353,   358,   363,
   368,   373,   378,   383,   388,   393,   398,   403,   408,   413,
   418,   423,   428,   433,   434,   436,   440,   445,   447,   449,
   451
};

static const short yyrhs[] = {    -1,
   119,   120,     0,     0,   120,   121,     0,   122,   133,     0,
   127,     4,   109,   123,   110,     0,     0,   124,     0,   123,
   111,   124,     0,   123,   111,     0,   126,   129,     0,   126,
   130,     0,   128,   127,     0,     8,     0,     9,     0,    10,
     0,    11,     0,    12,     0,    13,     0,     3,     0,     0,
    15,     0,    16,     0,   131,     0,   129,   112,   131,     0,
   131,     0,   130,   112,   131,     0,     4,   132,     0,     0,
   100,   138,     0,   113,   134,   114,     0,     0,   134,   135,
     0,   111,     0,   125,   111,     0,   143,   111,     0,   113,
   134,   114,     0,   136,   135,     0,    64,   109,   137,   110,
   135,     0,    64,   109,   137,   110,   135,    65,   135,     0,
    62,   109,   138,   111,   137,   111,   138,   110,     0,    63,
   109,   137,   110,     0,    66,   109,   138,   112,   138,   112,
   138,   110,     0,   138,   101,   138,     0,   138,   115,   138,
     0,   138,    90,   138,     0,   138,    91,   138,     0,   139,
     0,   138,   102,   138,     0,   138,   103,   138,     0,   138,
   104,   138,     0,   138,   105,   138,     0,   138,   106,   138,
     0,   103,   138,     0,   137,   116,   138,   117,   138,     0,
   109,   138,   110,     0,   142,   138,     0,     5,     0,    44,
     0,     6,     0,     4,     0,   146,     0,   144,     0,   143,
     0,   140,     0,   109,   138,   112,   138,   112,   138,   110,
     0,     0,     6,     0,    11,   141,     0,     4,   100,   138,
     0,     4,    51,   138,     0,     4,    52,   138,     0,     4,
    53,   138,     0,    19,   109,   145,   110,     0,    20,   109,
   145,   110,     0,    21,   109,   145,   110,     0,    33,   109,
   145,   110,     0,    34,   109,   145,   110,     0,    35,   109,
   145,   110,     0,    36,   109,   145,   110,     0,    37,   109,
   145,   110,     0,    38,   109,   145,   110,     0,    39,   109,
   145,   110,     0,    40,   109,   145,   110,     0,    41,   109,
   145,   110,     0,    42,   109,   145,   110,     0,    43,   109,
   110,     0,    22,   109,   145,   110,     0,    23,   109,   145,
   110,     0,    24,   109,   145,   110,     0,    25,   109,   145,
   110,     0,    45,   109,   145,   110,     0,    26,   109,   145,
   110,     0,    27,   109,   145,   110,     0,    32,   109,   145,
   110,     0,    48,   109,   145,   110,     0,    49,   109,   145,
   110,     0,    50,   109,   145,   110,     0,    17,   109,   145,
   110,     0,    46,   109,   145,   110,     0,    47,   109,   145,
   110,     0,    28,   109,   145,   110,     0,    29,   109,   145,
   110,     0,    30,   109,   145,   110,     0,    31,   109,   145,
   110,     0,    54,   109,   145,   110,     0,    55,   109,   145,
   110,     0,    56,   109,   145,   110,     0,    57,   109,   145,
   110,     0,    58,   109,   145,   110,     0,    59,   109,   145,
   110,     0,    61,   109,   145,   110,     0,     4,   109,   145,
   110,     0,     0,   138,     0,   138,   112,   145,     0,   147,
   109,   148,   110,     0,    17,     0,    18,     0,   138,     0,
   138,   112,   148,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   667,   672,   674,   675,   678,   700,   712,   717,   722,   727,
   735,   741,   747,   752,   757,   762,   767,   772,   777,   782,
   789,   790,   794,   799,   810,   819,   829,   838,   850,   854,
   866,   872,   876,   882,   887,   892,   899,   903,   908,   914,
   923,   927,   931,   937,   941,   945,   949,   961,   965,   969,
   973,   978,   982,   987,   991,   996,  1000,  1006,  1010,  1014,
  1019,  1024,  1027,  1030,  1033,  1039,  1044,  1045,  1050,  1054,
  1062,  1070,  1078,  1089,  1093,  1097,  1101,  1105,  1109,  1113,
  1117,  1121,  1125,  1129,  1133,  1137,  1141,  1145,  1149,  1153,
  1157,  1164,  1168,  1172,  1177,  1182,  1186,  1190,  1194,  1206,
  1211,  1215,  1220,  1225,  1229,  1233,  1238,  1243,  1248,  1253,
  1258,  1263,  1268,  1278,  1282,  1289,  1298,  1310,  1317,  1326,
  1332
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","SURFACE",
"IDENTIFIER","NUMBER","STRINGCONSTANT","VOID","FLOAT","NORMAL","VECTOR","COLOR",
"POINT","STRING","LIGHTSOURCE","VARYING","UNIFORM","ENVIRONMENT","TEXTURE","RADIANS",
"DEGREES","ABS","FLOOR","CEIL","ROUND","MIX","MOD","NOISE","STEP","SMOOTHSTEP",
"SQRT","INVERSESQRT","LENGTH","SIN","ASIN","COS","ACOS","TAN","ATAN","POW","EXP",
"LOG","SIGN","RANDOM","MATH_PI","REFRACT","OCCLUSION","TRACE","AMBIENT","DIFFUSE",
"SPECULAR","PLUSEQ","MINUSEQ","MULEQ","XCOMP","YCOMP","ZCOMP","SETXCOMP","SETYCOMP",
"SETZCOMP","TRIPLE","AREA","FOR","WHILE","IF","ELSE","ILLUMINANCE","OP_NULL",
"OP_ASSIGN","OP_VARDEF","OP_DEFEXPR","OP_FORMAL_DEFEXPR","OP_MUL","OP_DIV","OP_DOT",
"OP_ADD","OP_SUB","OP_NEG","OP_FUNC","OP_FUNCARG","OP_FUNC_HEADER","OP_CALLFUNC",
"OP_ASSIGNADD","OP_ASSIGNSUB","OP_ASSIGNMUL","OP_COND","OP_COND_TRIPLE","OP_PARENT",
"OP_LE","OP_GE","OP_EQ","OP_NEQ","OP_FTOV","OP_FOR","OP_FORCOND","OP_WHILE",
"OP_IF","OP_ILLUMINANCE","OP_IF_ELSE","OP_STMT","'='","'<'","'+'","'-'","'*'",
"'/'","'.'","UMINUS","TYPECAST","'('","')'","';'","','","'{'","'}'","'>'","'?'",
"':'","definitions","@1","funclist","function","func_head","formals","formal_variable_definitions",
"variable_definitions","typespec","type","detail","formal_def_expressions","variable_def_expressions",
"def_expression","def_init","block","statements","statement","loop_control",
"relation","expression","primary","triple","spacetype","typecast","assignexpression",
"procedurecall","proc_arguments","texture","texture_type","texture_arguments", NULL
};
#endif

static const short yyr1[] = {     0,
   119,   118,   120,   120,   121,   122,   123,   123,   123,   123,
   124,   125,   126,   127,   127,   127,   127,   127,   127,   127,
   128,   128,   128,   129,   129,   130,   130,   131,   132,   132,
   133,   134,   134,   135,   135,   135,   135,   135,   135,   135,
   136,   136,   136,   137,   137,   137,   137,   138,   138,   138,
   138,   138,   138,   138,   138,   138,   138,   139,   139,   139,
   139,   139,   139,   139,   139,   140,   141,   141,   142,   143,
   143,   143,   143,   144,   144,   144,   144,   144,   144,   144,
   144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
   144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
   144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
   144,   144,   144,   145,   145,   145,   146,   147,   147,   148,
   148
};

static const short yyr2[] = {     0,
     0,     2,     0,     2,     2,     5,     0,     1,     3,     2,
     2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
     0,     1,     1,     1,     3,     1,     3,     2,     0,     2,
     3,     0,     2,     1,     2,     2,     3,     2,     5,     7,
     8,     4,     8,     3,     3,     3,     3,     1,     3,     3,
     3,     3,     3,     2,     5,     3,     2,     1,     1,     1,
     1,     1,     1,     1,     1,     7,     0,     1,     2,     3,
     3,     3,     3,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     3,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     0,     1,     3,     4,     1,     1,     1,
     3
};

static const short yydefact[] = {     1,
     3,     2,    20,    14,    15,    16,    17,    18,    19,     4,
     0,     0,    32,     5,     0,    21,    21,     0,    22,    23,
     0,     0,     0,     0,    34,    32,    31,     0,     0,     0,
    33,    21,     0,     0,     8,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    21,    35,    29,    12,    26,    13,
    38,    36,     6,    21,    11,    24,    61,    58,    60,    67,
     0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    59,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    71,    48,    65,     0,    64,    63,
    62,     0,    72,    73,    70,     0,     0,     0,     0,     0,
    37,     0,    28,     0,     9,     0,   114,    68,    69,   114,
   114,   114,   114,   114,   114,   114,   114,   114,   114,   114,
   114,   114,   114,   114,   114,   114,   114,   114,   114,   114,
   114,   114,   114,   114,     0,   114,   114,   114,   114,   114,
   114,   114,   114,   114,   114,   114,   114,   114,    54,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    57,     0,     0,    42,    21,     0,    30,    27,    25,   115,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    87,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    56,     0,     0,    46,    47,    44,    49,    50,    51,    52,
    53,    45,   120,     0,     0,    39,     0,   114,   113,    99,
    74,    75,    76,    88,    89,    90,    91,    93,    94,   102,
   103,   104,   105,    95,    77,    78,    79,    80,    81,    82,
    83,    84,    85,    86,    92,   100,   101,    96,    97,    98,
   106,   107,   108,   109,   110,   111,   112,     0,     0,     0,
   117,     0,    21,     0,   116,     0,    55,   121,     0,    40,
     0,     0,    41,    43,    66,     0,     0,     0
};

static const short yydefgoto[] = {   306,
     1,     2,    10,    11,    34,    35,    28,    29,    12,    30,
    55,    48,    49,   123,    14,    16,    31,    32,   104,   190,
   106,   107,   129,   108,   109,   110,   191,   111,   112,   244
};

static const short yypact[] = {-32768,
-32768,   230,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -102,    10,-32768,-32768,   -76,    16,    21,   -29,-32768,-32768,
   -74,   -61,   -55,   -54,-32768,-32768,-32768,   -48,    60,   230,
-32768,   145,   -28,   -67,-32768,    60,   263,   263,   263,   263,
   263,   263,   263,   263,   152,-32768,   -24,   -43,-32768,-32768,
-32768,-32768,-32768,    23,   -27,-32768,   -34,-32768,-32768,    75,
   -25,-32768,   -23,     4,    19,    37,    46,    48,    49,    50,
    55,    56,    57,    61,    62,    63,    74,    76,    77,    79,
    80,    87,    90,    92,    93,    94,    96,-32768,   101,   108,
   110,   111,   112,   113,   120,   121,   123,   126,   127,   135,
   136,   263,   263,    65,   345,-32768,-32768,   263,-32768,-32768,
-32768,   137,   345,   345,   345,    89,   -95,   345,   -82,   122,
-32768,   263,-32768,    60,-32768,    60,   263,-32768,-32768,   263,
   263,   263,   263,   263,   263,   263,   263,   263,   263,   263,
   263,   263,   263,   263,   263,   263,   263,   263,   263,   263,
   263,   263,   263,   263,    25,   263,   263,   263,   263,   263,
   263,   263,   263,   263,   263,   263,   263,   263,   -38,    72,
   263,   263,   263,   263,   263,   263,   263,   263,   263,   263,
   -38,   263,   263,-32768,   145,   263,   345,-32768,-32768,   224,
    88,   138,   139,   140,   141,   142,   143,   150,   154,   160,
   161,   162,   163,   165,   166,   168,   169,   206,   213,   223,
   225,   227,   228,   231,   240,   242,-32768,   244,   245,   247,
   248,   255,   259,   261,   264,   265,   266,   267,   276,   278,
-32768,   263,   -45,   345,   345,   345,   -64,   -64,   -41,   -41,
   -38,   345,   241,   280,   -86,   182,   258,   263,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   277,   263,   263,
-32768,   263,   145,   263,-32768,   263,   345,-32768,   294,-32768,
   311,   328,-32768,-32768,-32768,   334,   340,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,   177,-32768,    -7,   321,-32768,
-32768,-32768,    26,-32768,-32768,   361,   -31,-32768,   -30,   -35,
-32768,-32768,-32768,-32768,   -16,-32768,   -42,-32768,-32768,   103
};


#define	YYLAST		460


static const short yytable[] = {    33,
    51,   105,   113,   114,   115,   116,   118,   118,   120,    36,
    13,   117,   119,    15,   184,    33,    37,    38,    39,    18,
   171,    37,    38,    39,   292,   172,   173,   185,    33,   171,
    19,    20,    17,   171,    41,    19,    20,    19,    20,   177,
   178,   179,    53,    54,   172,   173,    36,    42,   172,   173,
   180,   172,   173,    43,    44,   174,   175,   176,   177,   178,
   179,    56,    46,    47,   179,    40,   169,   170,   124,   180,
    40,   289,   181,   180,   127,   122,   180,    21,    22,    23,
   128,    24,    52,   130,   126,   131,   187,   192,   193,   194,
   195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
   205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
   215,   216,   132,   218,   219,   220,   221,   222,   223,   224,
   225,   226,   227,   228,   229,   230,    25,   133,    26,    27,
    -7,    -7,   -10,   -10,   217,   233,   234,   235,   236,   237,
   238,   239,   240,   241,   242,   134,   243,   118,    18,   188,
   247,   189,   245,   246,   135,    18,   136,   137,   138,    19,
    20,   172,   173,   139,   140,   141,    19,    20,    33,   142,
   143,   144,   174,   175,   176,   177,   178,   179,   172,   173,
   171,   231,   145,   232,   146,   147,   180,   148,   149,   174,
   175,   176,   177,   178,   179,   150,   288,   249,   151,   183,
   152,   153,   154,   180,   155,   295,    21,    22,    23,   156,
    24,   172,   173,    21,    22,    23,   157,    24,   158,   159,
   160,   161,   174,   175,   176,   177,   178,   179,   162,   163,
   125,   164,     3,   186,   165,   166,   180,     4,     5,     6,
     7,     8,     9,   167,   168,   182,   293,   250,   251,   252,
   253,   254,   255,   297,   243,    25,   299,    26,   301,   256,
   302,   300,    25,   257,    26,   121,    57,    58,    59,   258,
   259,   260,   261,    60,   262,   263,    33,   264,   265,    61,
    62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
    72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
    82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
    92,    93,    94,   172,   173,   266,    95,    96,    97,    98,
    99,   100,   267,   101,   174,   175,   176,   177,   178,   179,
   172,   173,   268,   307,   269,   248,   270,   271,   180,   308,
   272,   174,   175,   176,   177,   178,   179,   172,   173,   273,
    50,   274,   290,   275,   276,   180,   277,   278,   174,   175,
   176,   177,   178,   179,   279,   102,   172,   173,   280,   294,
   281,   103,   180,   282,   283,   284,   285,   174,   175,   176,
   177,   178,   179,   172,   173,   286,    45,   287,   296,   291,
     0,   180,   298,     0,   174,   175,   176,   177,   178,   179,
   172,   173,     0,   303,     0,     0,     0,     0,   180,     0,
     0,   174,   175,   176,   177,   178,   179,   172,   173,     0,
   304,     0,     0,     0,     0,   180,     0,     0,   174,   175,
   176,   177,   178,   179,   172,   173,     0,   305,     0,     0,
     0,     0,   180,     0,     0,   174,   175,   176,   177,   178,
   179,     0,     0,     0,     0,     0,     0,     0,     0,   180
};

static const short yycheck[] = {    16,
    32,    37,    38,    39,    40,    41,    42,    43,    44,    17,
   113,    42,    43,     4,   110,    32,    51,    52,    53,     4,
   116,    51,    52,    53,   111,    90,    91,   110,    45,   116,
    15,    16,   109,   116,   109,    15,    16,    15,    16,   104,
   105,   106,   110,   111,    90,    91,    54,   109,    90,    91,
   115,    90,    91,   109,   109,   101,   102,   103,   104,   105,
   106,    36,   111,     4,   106,   100,   102,   103,   112,   115,
   100,   117,   108,   115,   109,   100,   115,    62,    63,    64,
     6,    66,   111,   109,   112,   109,   122,   130,   131,   132,
   133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
   143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
   153,   154,   109,   156,   157,   158,   159,   160,   161,   162,
   163,   164,   165,   166,   167,   168,   111,   109,   113,   114,
   110,   111,   110,   111,   110,   171,   172,   173,   174,   175,
   176,   177,   178,   179,   180,   109,   182,   183,     4,   124,
   186,   126,   183,   185,   109,     4,   109,   109,   109,    15,
    16,    90,    91,   109,   109,   109,    15,    16,   185,   109,
   109,   109,   101,   102,   103,   104,   105,   106,    90,    91,
   116,   110,   109,   112,   109,   109,   115,   109,   109,   101,
   102,   103,   104,   105,   106,   109,   232,   110,   109,   111,
   109,   109,   109,   115,   109,   248,    62,    63,    64,   109,
    66,    90,    91,    62,    63,    64,   109,    66,   109,   109,
   109,   109,   101,   102,   103,   104,   105,   106,   109,   109,
    54,   109,     3,   112,   109,   109,   115,     8,     9,    10,
    11,    12,    13,   109,   109,   109,    65,   110,   110,   110,
   110,   110,   110,   289,   290,   111,   292,   113,   294,   110,
   296,   293,   111,   110,   113,   114,     4,     5,     6,   110,
   110,   110,   110,    11,   110,   110,   293,   110,   110,    17,
    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
    28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
    38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
    48,    49,    50,    90,    91,   110,    54,    55,    56,    57,
    58,    59,   110,    61,   101,   102,   103,   104,   105,   106,
    90,    91,   110,     0,   110,   112,   110,   110,   115,     0,
   110,   101,   102,   103,   104,   105,   106,    90,    91,   110,
    30,   110,   112,   110,   110,   115,   110,   110,   101,   102,
   103,   104,   105,   106,   110,   103,    90,    91,   110,   112,
   110,   109,   115,   110,   110,   110,   110,   101,   102,   103,
   104,   105,   106,    90,    91,   110,    26,   110,   112,   110,
    -1,   115,   290,    -1,   101,   102,   103,   104,   105,   106,
    90,    91,    -1,   110,    -1,    -1,    -1,    -1,   115,    -1,
    -1,   101,   102,   103,   104,   105,   106,    90,    91,    -1,
   110,    -1,    -1,    -1,    -1,   115,    -1,    -1,   101,   102,
   103,   104,   105,   106,    90,    91,    -1,   110,    -1,    -1,
    -1,    -1,   115,    -1,    -1,   101,   102,   103,   104,   105,
   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115
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

case 1:
#line 668 "parsesl.y"
{
				init_ast();
				emit_header();
			;
    break;}
case 5:
#line 679 "parsesl.y"
{
				
				SL2C_DEBUG("Leave func def\n");

				yyval.np = make_node(OP_FUNC, 2, yyvsp[-1].np, yyvsp[0].np);
	
				dump_node(yyval.np);

#if 0	// TODO
				emit_param_initializer(curr_func);

				fprintf(g_csfp, "\n");
			
				emit_function(curr_func);

				ast.func_list = append_function(
					ast.func_list, curr_func);
#endif
			;
    break;}
case 6:
#line 701 "parsesl.y"
{
				var_reg(yyvsp[-3].string, yyvsp[-4].ival);

				yyval.np = make_node(OP_FUNC_HEADER,
					       2,
					       make_leaf(yyvsp[-3].string),
					       yyvsp[-1].np);

			;
    break;}
case 7:
#line 713 "parsesl.y"
{
				// TODO
				yyval.np = make_node(OP_NULL, 0);
			;
    break;}
case 8:
#line 718 "parsesl.y"
{
				// TODO
				yyval.np = yyvsp[0].np;
			;
    break;}
case 9:
#line 723 "parsesl.y"
{
				// TODO
				yyval.np = make_node(OP_VARDEF, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 10:
#line 728 "parsesl.y"
{
				// TODO
				yyval.np = yyvsp[-1].np;
			;
    break;}
case 11:
#line 736 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 12:
#line 742 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 13:
#line 748 "parsesl.y"
{
			;
    break;}
case 14:
#line 753 "parsesl.y"
{
				vartype = FLOAT;
				yyval.ival = FLOAT;
			;
    break;}
case 15:
#line 758 "parsesl.y"
{
				vartype = NORMAL;
				yyval.ival = NORMAL;
			;
    break;}
case 16:
#line 763 "parsesl.y"
{
				vartype = VECTOR;
				yyval.ival = VECTOR;
			;
    break;}
case 17:
#line 768 "parsesl.y"
{
				vartype = COLOR;
				yyval.ival = COLOR;
			;
    break;}
case 18:
#line 773 "parsesl.y"
{
				vartype = POINT;
				yyval.ival = POINT;
			;
    break;}
case 19:
#line 778 "parsesl.y"
{
				vartype = STRING;
				yyval.ival = STRING;
			;
    break;}
case 20:
#line 783 "parsesl.y"
{
				vartype = SURFACE;
				yyval.ival = SURFACE;
			;
    break;}
case 22:
#line 791 "parsesl.y"
{
				/* do nothing */
			;
    break;}
case 23:
#line 795 "parsesl.y"
{
				/* do nothing */
			;
    break;}
case 24:
#line 800 "parsesl.y"
{
				SL2C_DEBUG("formal_def\n");
				
				yyval.np = make_node(OP_DEFEXPR,
					       2,
					       yyvsp[0].np,
					       make_node(OP_NULL, 0));

	
			;
    break;}
case 25:
#line 811 "parsesl.y"
{
				SL2C_DEBUG("formal_def, \n");

				yyval.np = make_node(OP_DEFEXPR, 2, yyvsp[-2].np, yyvsp[0].np);

			;
    break;}
case 26:
#line 820 "parsesl.y"
{
				SL2C_DEBUG("def_expr\n");

				yyval.np = make_node(OP_DEFEXPR,
					       2,
					       yyvsp[0].np,
					       make_node(OP_NULL, 0));

			;
    break;}
case 27:
#line 830 "parsesl.y"
{
				SL2C_DEBUG("def_expr,\n");

				yyval.np = make_node(OP_DEFEXPR, 2, yyvsp[-2].np, yyvsp[0].np);

			;
    break;}
case 28:
#line 839 "parsesl.y"
{
				var_reg(yyvsp[-1].string, vartype);
				
				yyval.np = make_node(OP_VARDEF,
				     2,
				     make_leaf(yyvsp[-1].string),
				     yyvsp[0].np);
			;
    break;}
case 29:
#line 851 "parsesl.y"
{
				yyval.np = make_node(OP_NULL, 0);
			;
    break;}
case 30:
#line 855 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 31:
#line 867 "parsesl.y"
{
				yyval.np = yyvsp[-1].np;
			;
    break;}
case 32:
#line 873 "parsesl.y"
{
				yyval.np = make_node(OP_NULL, 0);
			;
    break;}
case 33:
#line 877 "parsesl.y"
{
				yyval.np = make_node(OP_STMT, 2, yyvsp[-1].np, yyvsp[0].np);
			;
    break;}
case 34:
#line 883 "parsesl.y"
{
				printf("make ';'\n");
				yyval.np = make_node(OP_NULL, 0);
			;
    break;}
case 35:
#line 888 "parsesl.y"
{
				SL2C_DEBUG("variable_def ';'\n");
				yyval.np = yyvsp[-1].np;
			;
    break;}
case 36:
#line 893 "parsesl.y"
{
				SL2C_DEBUG("statement\n");

				yyval.np = yyvsp[-1].np;

			;
    break;}
case 37:
#line 900 "parsesl.y"
{
				yyval.np = yyvsp[-1].np;
			;
    break;}
case 38:
#line 904 "parsesl.y"
{
				// TODO

			;
    break;}
case 39:
#line 909 "parsesl.y"
{
				// TODO
				SL2C_DEBUG("if relation stmt\n");

			;
    break;}
case 40:
#line 915 "parsesl.y"
{
				SL2C_DEBUG("if ELSE statement\n");

				yyval.np = make_node(OP_IF_ELSE, 3, yyvsp[-4].np, yyvsp[-2].np, yyvsp[0].np);

			;
    break;}
case 41:
#line 924 "parsesl.y"
{
				// TODO;
			;
    break;}
case 42:
#line 928 "parsesl.y"
{
				// TODO;
			;
    break;}
case 43:
#line 932 "parsesl.y"
{
				// TODO;
			;
    break;}
case 44:
#line 938 "parsesl.y"
{
				yyval.np = make_node(OP_LE, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 45:
#line 942 "parsesl.y"
{
				yyval.np = make_node(OP_GE, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 46:
#line 946 "parsesl.y"
{
				yyval.np = make_node(OP_EQ, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 47:
#line 950 "parsesl.y"
{
				yyval.np = make_node(OP_NEQ, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 48:
#line 962 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 49:
#line 966 "parsesl.y"
{
				yyval.np = make_node(OP_ADD, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 50:
#line 970 "parsesl.y"
{
				yyval.np = make_node(OP_SUB, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 51:
#line 974 "parsesl.y"
{
				yyval.np = make_node(OP_MUL, 2, yyvsp[-2].np, yyvsp[0].np);
				emit_node(yyval.np);
			;
    break;}
case 52:
#line 979 "parsesl.y"
{
				yyval.np = make_node(OP_DIV, 2, yyvsp[-2].np, yyvsp[0].np);
			;
    break;}
case 53:
#line 983 "parsesl.y"
{
				yyval.np = make_node(OP_DOT, 2, yyvsp[-2].np, yyvsp[0].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 54:
#line 988 "parsesl.y"
{
				yyval.np = make_node(OP_NEG, 1, yyvsp[0].np);
			;
    break;}
case 55:
#line 992 "parsesl.y"
{
				yyval.np = make_node(OP_COND_TRIPLE, 3, yyvsp[-4].np, yyvsp[-2].np, yyvsp[0].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 56:
#line 997 "parsesl.y"
{
				yyval.np = make_node(OP_PARENT, 1, yyvsp[-1].np);
			;
    break;}
case 57:
#line 1001 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 58:
#line 1007 "parsesl.y"
{
				yyval.np = make_constnum(yyvsp[0].fval);
			;
    break;}
case 59:
#line 1011 "parsesl.y"
{
				yyval.np = make_constnum(3.141593);
			;
    break;}
case 60:
#line 1015 "parsesl.y"
{
				var_reg(yyvsp[0].string, vartype);
				yyval.np = make_conststr(yyvsp[0].string);
			;
    break;}
case 61:
#line 1020 "parsesl.y"
{
				var_reg(yyvsp[0].string, vartype);
				yyval.np = make_leaf(yyvsp[0].string);
			;
    break;}
case 62:
#line 1025 "parsesl.y"
{
			;
    break;}
case 63:
#line 1028 "parsesl.y"
{
			;
    break;}
case 64:
#line 1031 "parsesl.y"
{
			;
    break;}
case 65:
#line 1034 "parsesl.y"
{
				yyval.np = yyvsp[0].np;
			;
    break;}
case 66:
#line 1040 "parsesl.y"
{
				yyval.np = make_node(TRIPLE, 3, yyvsp[-5].np, yyvsp[-3].np, yyvsp[-1].np);
			;
    break;}
case 68:
#line 1046 "parsesl.y"
{
			;
    break;}
case 70:
#line 1055 "parsesl.y"
{
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OP_ASSIGN, 
					       2,
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			;
    break;}
case 71:
#line 1063 "parsesl.y"
{
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OP_ASSIGNADD, 
					       2,
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			;
    break;}
case 72:
#line 1071 "parsesl.y"
{
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OP_ASSIGNSUB, 
					       2,
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			;
    break;}
case 73:
#line 1079 "parsesl.y"
{
				var_reg(yyvsp[-2].string, vartype);
				yyval.np = make_node(OP_ASSIGNMUL, 
					       2,
					       make_leaf(yyvsp[-2].string),
					       yyvsp[0].np);
			;
    break;}
case 74:
#line 1090 "parsesl.y"
{
				yyval.np = make_node(RADIANS, 1, yyvsp[-1].np);
			;
    break;}
case 75:
#line 1094 "parsesl.y"
{
				yyval.np = make_node(DEGREES, 1, yyvsp[-1].np);
			;
    break;}
case 76:
#line 1098 "parsesl.y"
{
				yyval.np = make_node(ABS, 1, yyvsp[-1].np);
			;
    break;}
case 77:
#line 1102 "parsesl.y"
{
				yyval.np = make_node(SIN, 1, yyvsp[-1].np);
			;
    break;}
case 78:
#line 1106 "parsesl.y"
{
				yyval.np = make_node(ASIN, 1, yyvsp[-1].np);
			;
    break;}
case 79:
#line 1110 "parsesl.y"
{
				yyval.np = make_node(COS, 1, yyvsp[-1].np);
			;
    break;}
case 80:
#line 1114 "parsesl.y"
{
				yyval.np = make_node(ACOS, 1, yyvsp[-1].np);
			;
    break;}
case 81:
#line 1118 "parsesl.y"
{
				yyval.np = make_node(TAN, 1, yyvsp[-1].np);
			;
    break;}
case 82:
#line 1122 "parsesl.y"
{
				yyval.np = make_node(ATAN, 1, yyvsp[-1].np);
			;
    break;}
case 83:
#line 1126 "parsesl.y"
{
				yyval.np = make_node(POW, 1, yyvsp[-1].np);
			;
    break;}
case 84:
#line 1130 "parsesl.y"
{
				yyval.np = make_node(EXP, 1, yyvsp[-1].np);
			;
    break;}
case 85:
#line 1134 "parsesl.y"
{
				yyval.np = make_node(LOG, 1, yyvsp[-1].np);
			;
    break;}
case 86:
#line 1138 "parsesl.y"
{
				yyval.np = make_node(SIGN, 1, yyvsp[-1].np);
			;
    break;}
case 87:
#line 1142 "parsesl.y"
{
				yyval.np = make_node(RANDOM, 0);
			;
    break;}
case 88:
#line 1146 "parsesl.y"
{
				yyval.np = make_node(FLOOR, 1, yyvsp[-1].np);
			;
    break;}
case 89:
#line 1150 "parsesl.y"
{
				yyval.np = make_node(CEIL, 1, yyvsp[-1].np);
			;
    break;}
case 90:
#line 1154 "parsesl.y"
{
				yyval.np = make_node(ROUND, 1, yyvsp[-1].np);
			;
    break;}
case 91:
#line 1158 "parsesl.y"
{
				yyval.np = make_node(MIX, 1, yyvsp[-1].np);	
				if (yyvsp[-1].np) {
					yyval.np->type = yyvsp[-1].np->type;
				}
			;
    break;}
case 92:
#line 1165 "parsesl.y"
{
				yyval.np = make_node(REFRACT, 1, yyvsp[-1].np);	
			;
    break;}
case 93:
#line 1169 "parsesl.y"
{
				yyval.np = make_node(MOD, 1, yyvsp[-1].np);	
			;
    break;}
case 94:
#line 1173 "parsesl.y"
{
				yyval.np = make_node(NOISE, 1, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			;
    break;}
case 95:
#line 1178 "parsesl.y"
{
				yyval.np = make_node(LENGTH, 1, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			;
    break;}
case 96:
#line 1183 "parsesl.y"
{
				yyval.np = make_node(AMBIENT, 1, yyvsp[-1].np);	
			;
    break;}
case 97:
#line 1187 "parsesl.y"
{
				yyval.np = make_node(DIFFUSE, 1, yyvsp[-1].np);	
			;
    break;}
case 98:
#line 1191 "parsesl.y"
{
				yyval.np = make_node(SPECULAR, 1, yyvsp[-1].np);	
			;
    break;}
case 99:
#line 1195 "parsesl.y"
{
				yyval.np = make_node(ENVIRONMENT, 1, yyvsp[-1].np);	
			;
    break;}
case 100:
#line 1207 "parsesl.y"
{
				yyval.np = make_node(OCCLUSION, 1, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			;
    break;}
case 101:
#line 1212 "parsesl.y"
{
				yyval.np = make_node(TRACE, 1, yyvsp[-1].np);	
			;
    break;}
case 102:
#line 1216 "parsesl.y"
{
				yyval.np = make_node(STEP, 1, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			;
    break;}
case 103:
#line 1221 "parsesl.y"
{
				yyval.np = make_node(SMOOTHSTEP, 1, yyvsp[-1].np);	
				yyval.np->type = FLOAT;
			;
    break;}
case 104:
#line 1226 "parsesl.y"
{
				yyval.np = make_node(SQRT, 1, yyvsp[-1].np);
			;
    break;}
case 105:
#line 1230 "parsesl.y"
{
				yyval.np = make_node(INVERSESQRT, 1, yyvsp[-1].np);
			;
    break;}
case 106:
#line 1234 "parsesl.y"
{
				yyval.np = make_node(XCOMP, 1, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 107:
#line 1239 "parsesl.y"
{
				yyval.np = make_node(YCOMP, 1, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 108:
#line 1244 "parsesl.y"
{
				yyval.np = make_node(ZCOMP, 1, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 109:
#line 1249 "parsesl.y"
{
				yyval.np = make_node(SETXCOMP, 1, yyvsp[-1].np);
				yyval.np->type = VOID;
			;
    break;}
case 110:
#line 1254 "parsesl.y"
{
				yyval.np = make_node(SETYCOMP, 1, yyvsp[-1].np);
				yyval.np->type = VOID;
			;
    break;}
case 111:
#line 1259 "parsesl.y"
{
				yyval.np = make_node(SETZCOMP, 1, yyvsp[-1].np);
				yyval.np->type = VOID;
			;
    break;}
case 112:
#line 1264 "parsesl.y"
{
				yyval.np = make_node(AREA, 1, yyvsp[-1].np);
				yyval.np->type = FLOAT;
			;
    break;}
case 113:
#line 1269 "parsesl.y"
{
				var_reg(yyvsp[-3].string, VECTOR);
				yyval.np = make_node(OP_CALLFUNC,
					       2,
					       make_leaf(yyvsp[-3].string),
					       yyvsp[-1].np);
			;
    break;}
case 114:
#line 1279 "parsesl.y"
{
				yyval.np = NULL;
			;
    break;}
case 115:
#line 1283 "parsesl.y"
{
				yyval.np = make_node(OP_FUNCARG,
					       1,
					       yyvsp[0].np);
				//$$ = $1;
			;
    break;}
case 116:
#line 1290 "parsesl.y"
{
				yyval.np = make_node(OP_FUNCARG,
					       2,
					       yyvsp[-2].np,
					       yyvsp[0].np);
			;
    break;}
case 117:
#line 1300 "parsesl.y"
{
				yyval.np = make_node(TEXTURE,
					       2,
					       yyvsp[-3].np,
					       yyvsp[-1].np);
				//$$->type = COLOR;

			;
    break;}
case 118:
#line 1311 "parsesl.y"
{
				char *tex = "environment";
				var_reg(tex, COLOR);

				yyval.np = make_leaf(tex);
			;
    break;}
case 119:
#line 1318 "parsesl.y"
{
				char *tex = "texture";
				var_reg(tex, COLOR);

				yyval.np = make_leaf(tex);
			;
    break;}
case 120:
#line 1327 "parsesl.y"
{
				yyval.np = make_node(OP_FUNCARG,
					       1,
					       yyvsp[0].np);
			;
    break;}
case 121:
#line 1333 "parsesl.y"
{
				yyval.np = make_node(OP_FUNCARG,
					       2,
					       yyvsp[-2].np,
					       yyvsp[0].np);
			;
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
#line 1341 "parsesl.y"


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

