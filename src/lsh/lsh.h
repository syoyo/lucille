/*
 *
 * lucille Shell.
 *
 * $Id: lsh.h,v 1.1.1.1 2004/01/06 13:57:15 syoyo Exp $
 */

#ifndef LSH_LSH_H
#define LSH_LSH_H

#include <stddef.h>

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _lsh_t
{
	char *result;		/* result string			*/
	size_t len;		/* bytes used by result field		*/
	size_t lim;		/* byted allocated to result field	*/
	ri_list_t *syn_init;	/* stack of syntax initializers		*/
	ri_list_t *syn_finish;	/* stack of syntax finalizers		*/
	// lsh_state_t *state	/* state data from syntax extensions	*/
} lsh_t;

extern lsh_t *lsh_new();
extern void   lsh_free(lsh_t *lsh);
extern void   lsh_exec(lsh_t *lsh);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
