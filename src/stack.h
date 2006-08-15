/*
 * stack routine.
 *
 * $Id: stack.h,v 1.2 2004/01/30 04:43:57 syoyo Exp $
 */

#ifndef STACK_H
#define STACK_H

//#include "list.h"
#include "array.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_stack_t
{
	ri_ptr_array_t  *stacklist;
} ri_stack_t;

extern ri_stack_t *ri_stack_new  ();
extern void	   ri_stack_free (ri_stack_t *stack);
extern void	   ri_stack_push (ri_stack_t *stack, void *data);
extern void	   ri_stack_pop  (ri_stack_t *stack);
extern void       *ri_stack_get  (ri_stack_t *stack);
extern int         ri_stack_depth(ri_stack_t *stack);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
