#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>		/* va_list */
#include <assert.h>

#include "sym.h"
#include "tree.h"
#include "parsesl.h"
#include "sl2c.h"

#include "opcodes.def"		/* must include after parsesl.h */

// TODO: to be removed
#define IDS      400
#define CONSTNUM 401
#define CONSTSTR 402
//#define TRIPLE   403

typedef struct _triplenode_t
{
	node_t *nodes[3];
} triplenode_t;

extern int     get_parsed_line();	/* defined in parsesl.y */

static int     ntmpvar = 0;
static int     indent = 0;

static node_t *new_node(int opcode, node_t *left, node_t *right);
static char   *get_typestr(int type);
static sym_t  *new_tmpvar(int type);
static void    emit_node_trav(node_t *node);
static void    emit_tmpvar_trav(node_t *node);
static void    emit_defvar_trav(node_t *node);
static const char *var_prefix(const char *str);
static void    emit_scalar_node_trav(node_t *node);

/* utility functions */
static int     nargs_check(int nargs, node_t *args);

static void    err_printf(int line, const char *msg, ...);

static opcode_t *lookup_opcode(int opcode);

node_t *
make_leaf(char *name)
{
	sym_t  *sym;
	node_t *node;

	sym = lookup_sym(name);

	if (sym == NULL) {		
		/* ??? */
		return NULL;
	}

	// TODO: to be removed.
	node = new_node(IDENTIFIER, (node_t *)sym, NULL);

	node->n_ops = 1;

	node->ops = (node_t **)malloc(sizeof(node_t *));
	node->ops[0] = (node_t *)sym;

	node->type = sym->type;

	return node;
}

node_t *
make_constnum(double num)
{
	double *p;
	node_t *np;

	p = (double *)malloc(sizeof(double));
	*p = num;

	np = new_node(NUMBER, (node_t *)p, NULL);

	np->type   = FLOAT;
	//np->tmpvar = new_tmpvar(np->type);

	return np;
}

node_t *
make_conststr(char *string)
{
	node_t *np;

	np = new_node(CONSTSTR, (node_t *)string, NULL);

	np->type   = STRING;
	np->tmpvar = new_tmpvar(np->type);

	return np;
}

node_t *
make_ftov(node_t *node)
{
	node_t *new_node;

	new_node         = (node_t *)malloc(sizeof(node_t));
	assert(new_node);

	new_node->ops    = (node_t **)malloc(sizeof(node_t *));
	assert(new_node->ops);

	new_node->opcode = OP_FTOV;
	new_node->n_ops  = 1;

	new_node->ops[0]    = node;
	
	return new_node;
}

node_t *
make_node(int opcode, int n_ops, ...)
{
	va_list valist;
	int i;
	opcode_t *op;

	node_t *new_node;
	node_t *opnode;
	
	/*
	 * allocate node
	 */
	new_node = (node_t *)malloc(sizeof(node_t));
	assert(new_node);

	new_node->ops = (node_t **)malloc(n_ops * sizeof(node_t *));
	assert(new_node->ops);

	op = lookup_opcode(opcode);

	va_start(valist, n_ops);

	for (i = 0; i < n_ops; i++) {

		opnode = va_arg(valist, struct _node_t* );
		
		if (op->class == CLASS_ARITH &&
		    opnode->type == FLOAT) {

			/*
			 * Upcast to vector type
			 */
			new_node->ops[i] = make_ftov(opnode);

		} else {

			new_node->ops[i] = opnode;

		}
	}

	va_end(valist);

	new_node->opcode = opcode;
	new_node->n_ops  = n_ops;

	if (op->class == CLASS_ARITH) {
		new_node->type = VECTOR;
	}	

	return new_node;

	
#if 0
	/*
	 * upcasting
	 */
	
	node_t *np = NULL;
	node_t *newl = NULL, *newr = NULL;

	if (left && right && opcode != OP_FUNCARG) {
		if (left->type  == FLOAT &&
		    (right->type != FLOAT && right->type != STRING)) {

			/* convert left symbol from float to vector */
			newl = new_node(OP_FTOV, left, NULL);
			newl->type = right->type;
			newl->tmpvar = new_tmpvar(right->type);
			newr = right;

		} else  if (right->type  == FLOAT &&
			    (left->type != FLOAT && left->type != STRING)) {

			/* convert right symbol from float to vector */
			newr = new_node(OP_FTOV, right, NULL);
			newr->type = right->type;
			newr->tmpvar = new_tmpvar(left->type);
			newl = left;
		} else if (left->type == VOID || right->type == VOID) {
			err_printf(get_parsed_line(), "void type argument\n");
		} else {
			newl = left;
			newr = right;
		}
	} else {
		newl = left;
		newr = right;
	}

	np = new_node(opcode, newl, newr);

	if (opcode == OP_FTOV) {
		np->type = VECTOR;
	} else {
		if (!newl && !newr) {
			np->type = VECTOR;
		} else if (newl) {
			np->type = newl->type;
		} else if (newr) {
			np->type = newr->type;
		}
	}

	if (opcode != IDS &&
	    opcode != OP_ASSIGN &&
	    opcode != OP_VARDEF &&
	    opcode != OP_FUNCARG) {
		/* allocate temporaly variable. */
		np->tmpvar = new_tmpvar(np->type);
	}

	return np;
#endif
}

#if 0
void
emit_node(node_t *node)
{
	emit_node_trav(node);
}
#endif

void
emit_scalar_node(node_t *node)
{
	emit_node_trav(node);
	emit_indent();
	fprintf(g_csfp, "\t");
	emit_scalar_node_trav(node);
}

void
emit_tmpvar(node_t *node)
{
	emit_tmpvar_trav(node);
}

void
emit_defvar(node_t *node)
{
	emit_defvar_trav(node);
}

void
set_indent(int level)
{
	indent = level;
}

void
emit_indent()
{
	int i;
	for (i = 0; i < indent; i++) {
		fprintf(g_csfp, "\t");
	}
}

/* --- private functions --- */

static node_t *
new_node(int opcode, node_t *left, node_t *right)
{
	node_t *np;

	np = (node_t *)malloc(sizeof(node_t));

	np->opcode = opcode;
	np->left   = left;
	np->right  = right;
	np->tmpvar = NULL;

	np->line   = get_parsed_line();

	return np;
}

static char *
typestr(int type)
{
	char *str = NULL;

	switch (type) {
	case COLOR:
		str = "ri_color_t";
		break;
	case FLOAT:
		str = "ri_color_t";
		break;
	case VECTOR:
	case NORMAL:
	case POINT:
		str = "ri_vector_t";
		break;
	case STRING:
		str = "char *";
		break;
	case LIGHTSOURCE:
		str = "ri_lightsource_t *";
		break;
	case SURFACE:
		str = "surface";
		break;
		
	default:
		str = "unknown";
		break;
	}

	return str;
}

/*
 * create a new temporary variable name.
 */
static sym_t *
new_tmpvar(int type)
{
	char buf[256];
	char *newvar;
	sym_t *sp;

	sprintf(buf, "tmp%d", ntmpvar); ntmpvar++;
	sp = lookup_sym(buf);
	while (sp) {
		/* find unique name for temporaly variable. */
		sprintf(buf, "tmp%d\n", ntmpvar); ntmpvar++;
		sp = lookup_sym(buf);
	}

	newvar = strdup((const char *)&buf[0]);

	sp = var_reg(newvar, type);

	return sp;
}

static void
emit_node_trav(node_t *node)
{
	sym_t  *sp = NULL;
	char   *leftvar = NULL, *rightvar = NULL;
	double *fval;
	char   *cval;
	int     type = 0;
	node_t *np;
	triplenode_t *trip;

	if ((node->left && node->opcode != OP_ASSIGN) &&
	    ((node->left && node->right) ||
	     (node->left && node->left->opcode == OP_FUNC))) {
		emit_node_trav(node->left);
	}

	if (node->right) {
		emit_node_trav(node->right);
	}

	if (node->opcode == OP_FUNCARG) {
		if (node->left->opcode != IDS &&
		    node->left->opcode != OP_FUNC &&
		    node->left->opcode != CONSTSTR) {
			emit_node_trav(node->left);
		}
	}

	if (node->opcode != OP_FTOV && node->type == FLOAT) return;

	switch (node->opcode) {
	case IDS:
		return;

	case CONSTNUM:
		if (!node->left) {
			err_printf(node->line, "Invalid constant number\n");
		}

		fval = (double *)node->left;

		emit_indent();
		fprintf(g_csfp, "\tri_vector_set4(&%s%s, %f, %f, %f, 1.0);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name,
			*fval, *fval, *fval);
		break;

	case CONSTSTR:
		if (!node->left) {
			err_printf(node->line, "Invalid constant string\n");
		}

		cval = (char *)node->left;

		emit_indent();
		fprintf(g_csfp, "\t%s = %s;\n", node->tmpvar->name, cval);

		break;

	case TRIPLE:
		if (!node->left) {
			err_printf(node->line, "Invalid triple element\n");
		}
		if (!node->tmpvar) {
			err_printf(node->line, "!node->tmpvar\n");
		}

		trip = (triplenode_t *)node->left;

		emit_indent();
		fprintf(g_csfp, "\tri_vector_set4(&%s%s", 
			var_prefix(node->tmpvar->name), node->tmpvar->name);
		if (trip->nodes[0]->opcode != CONSTNUM) {
			err_printf(node->line, "currently triple element must be a number\n");
		}
		fval = (double *)trip->nodes[0]->left;
		fprintf(g_csfp, ", %f", *fval);
		if (trip->nodes[1]->opcode != CONSTNUM) {
			err_printf(node->line, "currently triple element must be a number\n");
		}
		fval = (double *)trip->nodes[1]->left;
		fprintf(g_csfp, ", %f", *fval);
		if (trip->nodes[2]->opcode != CONSTNUM) {
			err_printf(node->line, "currently triple element must be a number\n");
		}
		fval = (double *)trip->nodes[2]->left;
		fprintf(g_csfp, ", %f", *fval);
		fprintf(g_csfp, ", 1.0);\n");

		break;

	case OP_MUL:
		if (!node->tmpvar) {
			err_printf(node->line, "Invalid multiply operator\n");
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar){
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		if (node->right->opcode == IDS) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		if (node->type == FLOAT) {
			fprintf(g_csfp, "\t%s%s = %s%s * %s%s;\n",
				var_prefix(node->tmpvar->name),
				node->tmpvar->name,
				var_prefix(leftvar), leftvar,
				var_prefix(rightvar), rightvar);
		} else {
			fprintf(g_csfp, "\tri_vector_mul(&%s%s, &%s%s, &%s%s);\n",
				var_prefix(node->tmpvar->name),
				node->tmpvar->name,
				var_prefix(leftvar), leftvar,
				var_prefix(rightvar), rightvar);
		}
		break;

	case OP_ADD:
		if (!node->tmpvar) {
			err_printf(node->line, "Unrecognized add operator");
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar){
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized symbol");
		}

		if (node->right->opcode == IDS) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized symbol");
		}

		emit_indent();
		fprintf(g_csfp, "\tri_vector_add(&%s%s, &%s%s, &%s%s);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name,
			var_prefix(leftvar), leftvar,
			var_prefix(rightvar), rightvar);
		break;

	case OP_SUB:
		if (!node->tmpvar) {
			err_printf(node->line, "Unrecognized subtract operator");
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar){
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized symbol");
		}

		if (node->right->opcode == IDS) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized symbol");
		}

		emit_indent();
		fprintf(g_csfp, "\tri_vector_sub(&%s%s, &%s%s, &%s%s);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name,
			var_prefix(leftvar), leftvar,
			var_prefix(rightvar), rightvar);
		break;

	case OP_NEG:
		if (!node->tmpvar) {
			err_printf(node->line, "Unrecognized negative operator");
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar){
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		fprintf(g_csfp, "\tri_vector_copy(&%s%s, &%s%s);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name,
			var_prefix(leftvar), leftvar);
		emit_indent();
		fprintf(g_csfp, "\tri_vector_neg(&%s%s);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name);
		break;

	case OP_ASSIGN:
		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar) {
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized assign operator");
		}

		if (node->right->opcode == IDS ||
		    node->right->opcode == CONSTSTR) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		switch (node->type) {
		case FLOAT:
		case COLOR:
		case NORMAL:
		case VECTOR:
		case POINT:
			fprintf(g_csfp, "\tri_vector_copy(&%s%s, &%s%s);\n",
				var_prefix(leftvar), leftvar,
				var_prefix(rightvar), rightvar);
			break;

		case STRING:
			fprintf(g_csfp, "\t%s = %s;\n", leftvar, rightvar);
			break;
		default:
			fprintf(g_csfp, "type = %d\n", node->type);
			break;
		}

		break;
		
	case OP_ASSIGNADD:
		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar) {
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		if (node->right->opcode == IDS ||
		    node->right->opcode == CONSTSTR) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		switch (node->type) {
		case FLOAT:
		case COLOR:
		case NORMAL:
		case VECTOR:
		case POINT:
			fprintf(g_csfp, "\tri_vector_add(&%s%s, &%s%s, &%s%s);\n",
				var_prefix(leftvar), leftvar,
				var_prefix(leftvar), leftvar,
				var_prefix(rightvar), rightvar);
			break;

		default:
			printf("type = %d\n", node->type);
			break;
		}

		break;

	case OP_VARDEF:
		if (node->right && node->right->opcode == IDS) {
			sp = (sym_t *)node->right->left;
			rightvar = sp->name;
		} else if (node->right && node->right->tmpvar){
			rightvar = node->right->tmpvar->name;
		} else {
			/* no initializer */
			break;
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		switch (node->type) {
		case FLOAT:
		case COLOR:
		case NORMAL:
		case VECTOR:
		case POINT:
			fprintf(g_csfp, "\tri_vector_copy(&%s%s, &%s%s);\n",
				var_prefix(leftvar), leftvar,
				var_prefix(rightvar), rightvar);
			break;

		case STRING:
			fprintf(g_csfp, "\t%s = %s;\n", leftvar, rightvar);
			break;
		default:
			break;
		}

		break;

	case OP_FUNC:
		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized function syntax");
		}

		if (!node->tmpvar) {
			/* ??? */
			err_printf(node->line, "Unrecognized function syntax");
		}

		emit_indent();
		fprintf(g_csfp, "\t%s(&%s", leftvar, node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->left->tmpvar) {
				leftvar = np->left->tmpvar->name;
				type = np->left->tmpvar->type;
			} else if (np->left->opcode == IDS) {
				sp = (sym_t *)np->left->left;
				leftvar = sp->name;
				type = sp->type;
			} else {
				/* ??? */
				err_printf(node->line, "Unrecognized syntax");
			}

			if (type == FLOAT || type == STRING) {
				fprintf(g_csfp, ", %s%s", var_prefix(leftvar), leftvar);
			} else {
				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}
				
		fprintf(g_csfp, ");\n");
		break;

	case OP_PARENT:
		if (node->left) {
			emit_node_trav(node->left);
		}

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->tmpvar) {
			leftvar = node->left->tmpvar->name;
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		fprintf(g_csfp, "\tri_vector_copy(&%s%s, &%s%s);\n",
			var_prefix(node->tmpvar->name), node->tmpvar->name,
			var_prefix(leftvar), leftvar);

		break;

	case OP_FTOV:
		if (!node->tmpvar) {
			err_printf(node->line, "Unrecognized syntax");
		}

		fval = NULL;

		if (node->left->opcode == IDS) {
			sp = (sym_t *)node->left->left;
			leftvar = sp->name;
		} else if (node->left->opcode == CONSTNUM) {
			fval = (double *)node->left->left;
		} else if (node->left->tmpvar){
			if (node->left->type == FLOAT) {
				emit_indent();
				fprintf(g_csfp, "\t%s%s.e[0] = ",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);
				emit_scalar_node_trav(node->left);
				fprintf(g_csfp, ";\n");

				emit_indent();
				fprintf(g_csfp, "\tri_vector_set4(");
				fprintf(g_csfp, "&%s%s, %s%s.e[0], %s%s.e[0], %s%s.e[0], 1.0)",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name,
					var_prefix(node->tmpvar->name),
					node->tmpvar->name,
					var_prefix(node->tmpvar->name),
					node->tmpvar->name,
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);
				fprintf(g_csfp, ";\n");

				break;
			} else {
				emit_node_trav(node->left);
				leftvar = node->left->tmpvar->name;
			}
		} else if (node->left->opcode == OP_FUNCARG){
			if (node->left->left->opcode == IDS) {
				sp = (sym_t *)node->left->left->left;
				leftvar = sp->name;
			} else if (node->left->left->tmpvar){
				emit_node_trav(node->left->left);
				leftvar = node->left->left->tmpvar->name;
			} else {
				/* ??? */
				err_printf(node->line, "Unrecognized syntax");
			}
		} else {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		fprintf(g_csfp, "\tri_vector_set4(&%s%s",
			var_prefix(node->tmpvar->name),
			node->tmpvar->name);
			
		if (fval) {
			fprintf(g_csfp, ", %f, %f, %f, 1.0);\n",
				*fval, *fval, *fval);
		} else {
			fprintf(g_csfp, ", %s%s, %s%s, %s%s, 1.0);\n",
				var_prefix(leftvar), leftvar,
				var_prefix(leftvar), leftvar,
				var_prefix(leftvar), leftvar);
		}
		break;

	case MIX:
		if (!node->tmpvar) {
			/* ??? */
			err_printf(node->line, "Unrecognized syntax");
		}

		emit_indent();
		printf("\tmixv(&%s", node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					err_printf(node->line, "Unrecognized syntax");
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}
				
		fprintf(g_csfp, ");\n");
		break;

	case REFRACT:
		if (!nargs_check(3, node)) {
			err_printf(node->line,
				   "refract() requires 3 arguments\n");
		}

		emit_indent();
		fprintf(g_csfp, "\trefract(&%s%s", var_prefix(node->tmpvar->name),
				       node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					err_printf(node->line, "Unrecognized syntax");
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}

		fprintf(g_csfp, ");\n");

		break;

	case AMBIENT:
		emit_indent();
		fprintf(g_csfp, "\tambient(status, &%s%s", var_prefix(node->tmpvar->name),
				       node->tmpvar->name);

		fprintf(g_csfp, ");\n");

		break;

	case DIFFUSE:
		if (!nargs_check(1, node)) {
			err_printf(node->line, "diffuse() requires 1 argument\n");
		}

		emit_indent();
		fprintf(g_csfp, "\tdiffuse(status, &%s%s",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					printf("???: left var\n");
					exit(-1);
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}

		fprintf(g_csfp, ");\n");

		break;

	case SPECULAR:
		if (!node->right &&
		    !node->right->right &&
		    !node->right->right->right) {
			printf("trace() requires three arguments\n");
			exit(-1);
		}

		emit_indent();
		fprintf(g_csfp, "\tspecular(status, &%s%s",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					printf("???: left var\n");
					exit(-1);
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}

		fprintf(g_csfp, ");\n");

		break;

	case TEXTURE:
		if (!nargs_check(1, node)) {
			err_printf(node->line, "Currently, lucille supports texture() with one argument.\n");
		}

		emit_indent();
		fprintf(g_csfp, "\ttexture(status, &%s%s, ",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);

		np = node->right;

		if (np->type != STRING) {
			err_printf(node->line, "argument 1 should be a string variable.\n");
		}

		if (np->left->tmpvar) {
			leftvar = np->left->tmpvar->name;
		} else if (np->left->opcode == IDS) {
			sp = (sym_t *)np->left->left;
			leftvar = sp->name;
		} else {
			/* ??? */
			printf("???: left var\n");
			exit(-1);
		}

		fprintf(g_csfp, "%s%s", var_prefix(leftvar), leftvar);

		fprintf(g_csfp, ");\n");

		break;

	case ENVIRONMENT:
		if (!node->right &&
		    !node->right->right) {
			printf("trace() requires two arguments\n");
			exit(-1);
		}

		emit_indent();
		fprintf(g_csfp, "\tenvironment(status, &%s%s",
					var_prefix(node->tmpvar->name),
					node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					printf("???: left var\n");
					exit(-1);
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}

		fprintf(g_csfp, ");\n");

		break;

	case TRACE:
		if (!node->right &&
		    !node->right->right) {
			printf("trace() requires two arguments\n");
			exit(-1);
		}

		emit_indent();
		fprintf(g_csfp, "\ttrace(status, &%s%s", var_prefix(node->tmpvar->name),
				       node->tmpvar->name);

		np = node->right;
		while(np) {

			if (np->type == FLOAT) {
				fprintf(g_csfp, ", ");
				emit_scalar_node_trav(np->left);
			} else {
				if (np->left->tmpvar) {
					leftvar = np->left->tmpvar->name;
				} else if (np->left->opcode == IDS) {
					sp = (sym_t *)np->left->left;
					leftvar = sp->name;
				} else {
					/* ??? */
					printf("???: left var\n");
					exit(-1);
				}

				fprintf(g_csfp, ", &%s%s", var_prefix(leftvar), leftvar);
			}

			np = np->right;
		}

		fprintf(g_csfp, ");\n");

		break;

	default:
		break;
	}
}

static void
emit_scalar_node_trav(node_t *node)
{
	sym_t  *sp;
	char   *leftvar; //, *rightvar;
	double *fval;
	node_t *np;
	int     type;

	if (node == NULL) return;

	switch (node->opcode) {
	case IDS:
		sp = (sym_t *)node->left;
		leftvar = sp->name;

		if (sp->type == FLOAT || sp->type == STRING) {
			fprintf(g_csfp, "%s%s ", var_prefix(leftvar), leftvar);
		} else {
			fprintf(g_csfp, "&%s%s ", var_prefix(leftvar), leftvar);
		}
		return;

	case CONSTNUM:

		fval = (double *)node->left;

		fprintf(g_csfp, "%f ", *fval);
		break;

	case OP_MUL:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "* ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_DIV:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "/ ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_ADD:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "+ ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_SUB:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "- ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_NEG:
		fprintf(g_csfp, "- ");

		emit_scalar_node_trav(node->left);
	
		break;

	case OP_LE:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "< ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_NEQ:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "!= ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_ASSIGN:
	case OP_VARDEF:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "= ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_ASSIGNADD:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "+= ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_ASSIGNSUB:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "-= ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_ASSIGNMUL:
		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, "*= ");

		emit_scalar_node_trav(node->right);

		break;

	case OP_PARENT:
		fprintf(g_csfp, "( ");

		emit_scalar_node_trav(node->left);

		fprintf(g_csfp, ") ");

		break;

	case OP_DOT:
		if (!node->right ||
		    !node->left) {
			printf("Invalid dot expression\n");
			exit(-1);
		}

		fprintf(g_csfp, "ri_vector_dot3(");
		emit_scalar_node_trav(node->left);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ABS:
		if (!node->right) {
			printf("\nabs() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "fabs( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case SIN:
		if (!node->right) {
			printf("\nsin() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "sin( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ASIN:
		if (!node->right) {
			printf("\nasin() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "asin( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case COS:
		if (!node->right) {
			printf("\ncos() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "cos( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ACOS:
		if (!node->right) {
			printf("\nacos() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "acos( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case TAN:
		if (!node->right) {
			printf("\ntan() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "tan( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ATAN:
		if (!node->right) {
			printf("\ntan() without argument\n");
			exit(-1);
		}

		if (!node->right->right) {	/* one arugument tan() */
			fprintf(g_csfp, "atan( ");
			emit_scalar_node_trav(node->right);
			fprintf(g_csfp, ") ");
		} else {
			fprintf(g_csfp, "atan2( ");
			emit_scalar_node_trav(node->right);
			fprintf(g_csfp, ") ");
		}

		break;

	case POW:
		if (!node->right && !node->right->right) {
			printf("\npow() requires two arguments\n");
			exit(-1);
		}

		fprintf(g_csfp, "pow( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case EXP:
		if (!node->right) {
			printf("\nexp() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "exp( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case LOG:
		if (!node->right) {
			printf("\nexp() without argument\n");
			exit(-1);
		}

		if (!node->right->right) {	/* one argument log() */
			fprintf(g_csfp, "log( ");
			emit_scalar_node_trav(node->right);
			fprintf(g_csfp, ") ");
		} else {
			fprintf(g_csfp, "log_base( ");
			emit_scalar_node_trav(node->right);
			fprintf(g_csfp, ") ");
		}

		break;

	case SIGN:
		if (!node->right) {
			printf("\nsign() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "sign( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case FLOOR:
		if (!node->right) {
			printf("floor() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "floor( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case CEIL:
		if (!node->right) {
			printf("ceil() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "ceil( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ROUND:
		if (!node->right) {
			printf("round() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "round( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case MOD:
		if (!nargs_check(2, node)) {
			err_printf(node->line, "mod() requires 2 arguments\n");
		}

		fprintf(g_csfp, "fmod( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right->right);
		fprintf(g_csfp, ") ");

		break;

	case LENGTH:
		if (!node->tmpvar) {
			/* ??? */
			printf("???: !node->tmpvar\n");
			exit(-1);
		}

		if (!nargs_check(1, node)) {
			err_printf(node->line, "length() requires 1 argument\n");
		}

		np = node->right;
		if (np->tmpvar) {
			leftvar = np->tmpvar->name;
			type = np->tmpvar->type;
		} else if (np->left->tmpvar) {
			leftvar = np->left->tmpvar->name;
			type = np->left->tmpvar->type;
		} else if (np->left->opcode == IDS) {
			sp = (sym_t *)np->left->left;
			leftvar = sp->name;
			type = np->left->type;
		} else {
			/* ??? */
			printf("???: left var\n");
			exit(-1);
		}

		if (type == FLOAT) {
			err_printf(node->line, "length() requires vector argument\n");
		} else {
			fprintf(g_csfp, "length(&%s%s) ", var_prefix(leftvar), leftvar);
		}
		break;

	case AREA:
		if (!node->tmpvar) {
			/* ??? */
			printf("???: !node->tmpvar\n");
			exit(-1);
		}

		if (!node->right) {
			printf("area() requires one argument\n");
			exit(-1);
		}

		np = node->right;
		if (np->tmpvar) {
			leftvar = np->tmpvar->name;
			type = np->tmpvar->type;
		} else if (np->left->tmpvar) {
			leftvar = np->left->tmpvar->name;
			type = np->left->tmpvar->type;
		} else if (np->left->opcode == IDS) {
			sp = (sym_t *)np->left->left;
			leftvar = sp->name;
			type = np->left->type;
		} else {
			/* ??? */
			printf("???: left var\n");
			exit(-1);
		}

		if (type == FLOAT) {
			printf("area() requires vector argument\n");
		} else {
			fprintf(g_csfp, "area(&%s%s) ", var_prefix(leftvar), leftvar);
		}
		break;

	case NOISE:
		if (!node->tmpvar) {
			/* ??? */
			printf("???: !node->tmpvar\n");
			exit(-1);
		}

		if (!node->right) {
			printf("noise() requires at least one argument\n");
			exit(-1);
		}

		np = node->right;
		if (np->tmpvar) {
			leftvar = np->tmpvar->name;
			type = np->tmpvar->type;
		} else if (np->left->tmpvar) {
			leftvar = np->left->tmpvar->name;
			type = np->left->tmpvar->type;
		} else if (np->left->opcode == IDS) {
			sp = (sym_t *)np->left->left;
			leftvar = sp->name;
			type = np->left->type;
		} else {
			/* ??? */
			printf("???: left var\n");
			exit(-1);
		}

		if (type == FLOAT) {
			fprintf(g_csfp, "noise1d( ");
			emit_scalar_node_trav(node->right);
			fprintf(g_csfp, ") ");
		} else {
			fprintf(g_csfp, "noise3d(&%s%s) ", var_prefix(leftvar), leftvar);
		}
		break;

	case OCCLUSION:
		if (!node->right &&
		    !node->right->right &&
		    !node->right->right->right) {
			printf("occlusion() requires at least three arguments\n");
			exit(-1);
		}

		fprintf(g_csfp, "occlusion(status, ");
		emit_scalar_node_trav(node->right);
		//fprintf(g_csfp, ", ");
		//emit_scalar_node_trav(node->right->right);
		//fprintf(g_csfp, ", ");
		//emit_scalar_node_trav(node->right->right->right);
		fprintf(g_csfp, ") ");

		break;

	case STEP:
		if (!node->right && !node->right->right) {
			printf("step() requires two arguments\n");
			exit(-1);
		}

		fprintf(g_csfp, "step( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case SMOOTHSTEP:
		if (!node->right &&
		    !node->right->right &&
		    !node->right->right->right) {
			printf("smoothstep() requires three arguments\n");
			exit(-1);
		}

		fprintf(g_csfp, "smoothstep( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case SQRT:
		if (!node->right) {
			printf("\nsqrt() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "sqrt( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case INVERSESQRT:
		if (!node->right) {
			printf("inversesqrt() without argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "inveresqrt( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case XCOMP:
		if (!node->right) {
			printf("xcomp() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "xcomp( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case YCOMP:
		if (!node->right) {
			printf("ycomp() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "ycomp( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case ZCOMP:
		if (!node->right) {
			printf("zcomp() requires one argument\n");
			exit(-1);
		}

		fprintf(g_csfp, "zcomp( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") ");

		break;

	case OP_FOR:
		if (!node->right) {
			printf("???: !node->right\n");
			exit(-1);
		}

		fprintf(g_csfp, "for ( ");
		emit_scalar_node_trav(node->left);
		fprintf(g_csfp, "; ");
		emit_scalar_node_trav(node->right->left);
		fprintf(g_csfp, "; ");
		emit_scalar_node_trav(node->right->right);
		fprintf(g_csfp, ") {");

		break;

	case OP_COND:
		if (!node->right) {
			printf("???: !node->right\n");
			exit(-1);
		}

		emit_scalar_node_trav(node->left);
		fprintf(g_csfp, " ? ");
		emit_scalar_node_trav(node->right->left);
		fprintf(g_csfp, " : ");
		emit_scalar_node_trav(node->right->right);
		fprintf(g_csfp, " ");

		break;

	case OP_WHILE:
		if (!node->right) {
			printf("???: !node->right\n");
			exit(-1);
		}

		fprintf(g_csfp, "while ( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") {");

		break;

	case OP_IF:
		if (!node->right) {
			printf("???: !node->right\n");
			exit(-1);
		}

		fprintf(g_csfp, "if ( ");
		emit_scalar_node_trav(node->right);
		fprintf(g_csfp, ") {");

		break;

	case OP_ILLUMINANCE:
		if (!node->right) {
			printf("???: !node->right\n");
			exit(-1);
		}

		fprintf(g_csfp, "for (light = next_lightsource(status, ");
		emit_scalar_node_trav(node->left);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right->left);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right->right);
		fprintf(g_csfp, ");\n");

		emit_indent();
		fprintf(g_csfp, "             light != NULL;\n");

		emit_indent();
		fprintf(g_csfp, "             light = next_lightsource(status, ");
		emit_scalar_node_trav(node->left);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right->left);
		fprintf(g_csfp, ", ");
		emit_scalar_node_trav(node->right->right);
		fprintf(g_csfp, ")) {");

		break;

	case OP_FUNCARG:
		emit_scalar_node_trav(node->left);

		if (node->right) {
			fprintf(g_csfp, ", ");
			emit_scalar_node_trav(node->right);
		}

		break;
	}
}

static void
emit_tmpvar_trav(node_t *node)
{
	sym_t *sp;

	if ((node->left && node->right) ||
	    node->opcode == OP_NEG ||
	    node->opcode == OP_FUNCARG ||
	    node->opcode == OP_PARENT ||
	    node->opcode == OP_FTOV) {
		emit_tmpvar_trav(node->left);
	}

	if (node->right) {
		emit_tmpvar_trav(node->right);
	}

	if (node->opcode == OP_VARDEF) {
		if (!node->left  || node->left->opcode !=IDS) {
			printf("???\n");
			exit(-1);
		}

		sp = (sym_t *)node->left->left;
		fprintf(g_csfp, "\t%s %s;\n", typestr(node->left->type), sp->name);

	} else if (node->tmpvar) {

		fprintf(g_csfp, "\t%s %s;\n", typestr(node->tmpvar->type), node->tmpvar->name);
	}
}

static void
emit_defvar_trav(node_t *node)
{
	sym_t *sp;

	if (node->left && node->right) {
		emit_defvar_trav(node->left);
	}

	if (node->right) {
		emit_defvar_trav(node->right);
	}

	if (node->opcode == OP_VARDEF) {
		if (!node->left  || node->left->opcode !=IDS) {
			printf("???\n");
			exit(-1);
		}

		sp = (sym_t *)node->left->left;
		fprintf(g_csfp, "\t%s %s;\n", typestr(node->left->type), sp->name);
	}
}

static const char *
var_prefix(const char *str)
{
	static const char *outputprefix = "output->";
	static const char *inputprefix = "status->input.";
	static const char *illumprefix = "light->";
	static const char *noprefix = "";

	if      (strcmp(str, "Oi") == 0) return outputprefix;
	else if (strcmp(str, "Ci") == 0) return outputprefix;
	else if (strcmp(str, "Os") == 0) return inputprefix;
	else if (strcmp(str, "Cs") == 0) return inputprefix;
	else if (strcmp(str, "s")  == 0)  return inputprefix;
	else if (strcmp(str, "t")  == 0)  return inputprefix;
	else if (strcmp(str, "N")  == 0)  return inputprefix;
	else if (strcmp(str, "I")  == 0)  return inputprefix;
	else if (strcmp(str, "P")  == 0)  return inputprefix;
	else if (strcmp(str, "L")  == 0)  return illumprefix;
	else if (strcmp(str, "Cl") == 0)  return illumprefix;

	return noprefix;
}

static int
nargs_check(int nargs, node_t *args)
{
	int     i;
	node_t *np = args;

	for (i = 0; i < nargs; i++) {
		if (!np->right) return 0;

		np = np->right;
	}

	/* more args exists */
	if (np->right) return 0;

	return 1;
}

static void
err_printf(int line, const char *msg, ...)
{
	va_list args;

	if (line > 0) {
		fprintf(stderr, "(sl2c) Translation error at line %d : ", line + 1);
	} else {
		fprintf(stderr, "(sl2c) Translation error : ");
	}

	va_start(args, msg);

	vfprintf(stderr, msg, args);
	fprintf(stderr, "\n");

	va_end(args);

	fflush(stdout);
	fflush(stderr);

	exit(-1);

	return;
}

static opcode_t *
lookup_opcode(int opcode)
{
	int i;

	i = 0;
	while (g_opcodes[i].op != 0) {
		if (g_opcodes[i].op == opcode) {
			return &g_opcodes[i];
		}

		i++;
	}

	return NULL;
}

static void
emit_whitespace(int n)
{
	int i;

	for (i = 0; i < n * 2; i++) {
		printf(" ");
	}
}

static char *
get_typestr(int type)
{
	switch (type) {
	case COLOR:
		return "color";
		break;
	case FLOAT:
		return "float";
		break;
	case VECTOR:
		return "vector";
		break;
	case NORMAL:
		return "normal";
		break;
	case POINT:
		return "point";
		break;
	case STRING:
		return "string";
		break;
	case LIGHTSOURCE:
		return "lightsource";
		break;
	case SURFACE:
		return "surface";
		break;
	default:
		return "unknown";
		break;
	}
}

static void
dump_ident(node_t *node, opcode_t *opcode)
{
	printf("%s(\"%s\", type=%s)\n",
		opcode->name,
		((sym_t *)node->left)->name,
		get_typestr(((sym_t *)node->left)->type) );
}

static void
dump_number(node_t *node, opcode_t *opcode)
{
	(void)opcode;
	printf("num(%f)\n", *((double *)node->left) );
}

static void
dump_string(node_t *node, opcode_t *opcode)
{
	(void)opcode;
	printf("str(%s)\n", ((char *)node->left) );
}

static void
dump_node_trav(node_t *node, int num, int depth)
{
	int i;
	opcode_t *opcode;

	if (!node) {
		printf("err: num = %d, depth = %d\n", num, depth);
		exit(-1);
	}

	opcode = lookup_opcode(node->opcode);

	if (!opcode) {
		printf("Unknown opcode: %d\n", node->opcode);
		return;
	}

	emit_whitespace(depth);
	printf("[%d] ", num);

	if (node->opcode == IDENTIFIER) {
		dump_ident(node, opcode);
	} else if (node->opcode == STRINGCONSTANT) {
		dump_string(node, opcode);
	} else if (node->opcode == NUMBER) {
		dump_number(node, opcode);
	} else {
		printf("%s(n_ops = %d, type = %s)\n",
			opcode->name, node->n_ops, get_typestr(node->type));
	}

	for (i = 0; i < node->n_ops; i++) {
		dump_node_trav(node->ops[i], i+1, depth+1);
	}
}

void
dump_node(node_t *node)
{
	dump_node_trav(node, 0, 0);
}


void
dispatch_node(node_t *node)
{
	int i;

	void (*func)(node_t *node);

	i = 0;
	while (g_opcodes[i].op != 0) {
		if (g_opcodes[i].op == node->opcode) {
			func = g_opcodes[i].func;

			if (func) {
				(*func)(node);
			} else {
				/* do nothing */
			}
		}

		i++;
	}
}

void
emit_stmt(node_t *node)
{
	int i;

	for (i = 0; i < node->n_ops; i++) {
		dispatch_node(node->ops[i]);
	}
}

void
emit_mul(node_t *node)
{
	sym_t *sp = NULL;
	char  *left_var = NULL, *right_var = NULL;

	/*
	if (!node->tmpvar) {
		err_printf(node->line, "Invalid multiply operator\n");
	}
	*/

	if (node->ops[0]->opcode == IDENTIFIER) {
		sp = (sym_t *)node->ops[0]->ops[0];
		left_var = sp->name;
	} else if (node->ops[0]->tmpvar){
		left_var = node->ops[0]->tmpvar->name;
	} else {
		/* ??? */
		err_printf(node->line, "Unrecognized syntax\n"
		                       "  opcode = %d\n", node->ops[1]->opcode);
	}

	if (node->ops[1]->opcode == IDENTIFIER) {
		sp = (sym_t *)node->ops[1]->ops[0];
		right_var = sp->name;
	} else if (node->ops[1]->tmpvar){
		right_var = node->ops[1]->tmpvar->name;
	} else {
		/* ??? */
		err_printf(node->line, "Unrecognized syntax\n"
				       "  opcode = %d\n", node->ops[1]->opcode);
	}

	emit_indent();

	dump_node(node);

	if (node->type == FLOAT) {
		fprintf(g_csfp, "\t%s%s = %s%s * %s%s;\n",
			var_prefix(node->tmpvar->name),
			node->tmpvar->name,
			var_prefix(left_var), left_var,
			var_prefix(right_var), right_var);
	} else {
		fprintf(g_csfp, "\tri_vector_mul(&%s%s, &%s%s, &%s%s);\n",
			var_prefix(node->tmpvar->name),
			node->tmpvar->name,
			var_prefix(left_var), left_var,
			var_prefix(right_var), right_var);
	}
}

void
emit_node(node_t *node)
{
	dispatch_node(node);
}
