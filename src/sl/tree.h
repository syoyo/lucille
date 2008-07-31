/*
 * AST(Abstract Syntax Tree) routines.
 */
 
#ifndef TREE_H
#define TREE_H

#include "sym.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _node_t
{
	int              opcode;	/* opcode		*/
	int              type;		/* type of opcode	*/
	sym_t           *tmpvar;	/* temporary variable	*/             
	struct _node_t  *left;		/* left of children	*/
	struct _node_t  *right;		/* right of children	*/

	int              line;		/* corresponding line number in RSL */

	struct _node_t **ops;		/* operands		*/
	int              n_ops;		/* number of operands 	*/
} node_t;

/* make leaf node */
extern node_t *make_leaf(char *name);

/* make constant number node */
extern node_t *make_constnum(double num);

/* make constant string node */
extern node_t *make_conststr(char *string);

#if 0
/* make triple node */
extern node_t *make_triple(node_t *n1, node_t *n2, node_t *n3);

/* make `for' syntax */
extern node_t *make_for(node_t *init, node_t *cond, node_t *step);

/* make `if' syntax */
extern node_t *make_if(node_t *node);

/* make `while' syntax */
extern node_t *make_while(node_t *node);

/* make 'a ? b : c' style conditinal operator syntax */
extern node_t *make_cond(node_t *cond, node_t *expr1, node_t *expr2);

/* make `illuminance' syntax */
extern node_t *make_illuminance(node_t *n1, node_t *n2, node_t *n3);
#endif

/* make tree node */
//extern node_t *make_node(int opcode, node_t *left, node_t *right);
extern node_t *make_node(int opcode, int n_ops, ...);

extern void    dump_node(node_t *node);
extern void    emit_node(node_t *node);
extern void    emit_tmpvar(node_t *node);
extern void    emit_defvar(node_t *node);

extern void    emit_scalar_node(node_t *node);

extern void    set_indent(int level);
extern void    emit_indent();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
