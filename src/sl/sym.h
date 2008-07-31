#ifndef SYM_H
#define SYM_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _sym_t
{
	char		 *name;		/* name of IDENTIFIER	*/
	int		  type;		/* type of IDENTIFIER	*/
	int               class;	/* class of IDENTIFIER  */
	struct _sym_t *next;		/* next symbol		*/
} sym_t;

/* register variable symbol in the symbol table. */
extern sym_t *var_reg(char *name, int type);

/* lookup symbol by name. */ 
extern sym_t *lookup_sym(char *name);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

