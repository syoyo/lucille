/*
 * $Id: stack.c,v 1.2 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "memory.h"
#include "log.h"
#include "stack.h"

ri_stack_t *
ri_stack_new()
{
	ri_stack_t *p = NULL;

	p = (ri_stack_t *)ri_mem_alloc(sizeof(ri_stack_t));

	p->stacklist = ri_ptr_array_new();

	return p;
}

void
ri_stack_free(ri_stack_t *stack)
{
	ri_ptr_array_free(stack->stacklist);
	ri_mem_free(stack);
}

void
ri_stack_push(ri_stack_t *stack, void *data)
{
	ri_ptr_array_insert(stack->stacklist,
			    stack->stacklist->nelems,
			    data);
}

void
ri_stack_pop(ri_stack_t *stack)
{

	ri_log_and_return_if(stack->stacklist == NULL);
	ri_log_and_return_if(stack->stacklist->nelems < 1);

	ri_ptr_array_remove_at(stack->stacklist,
			       stack->stacklist->nelems - 1);
}

void *
ri_stack_get(ri_stack_t *stack)
{
	void *data;

	if (stack->stacklist == NULL) {
#ifdef DEBUG
		ri_log(LOG_WARN, "stack is empty!");
#endif
		return NULL;
	}

	if (stack->stacklist->nelems < 1) {
		return NULL;
	}

	data = ri_ptr_array_at(stack->stacklist,
			       stack->stacklist->nelems - 1);
	if (data  == NULL) {
#ifdef DEBUG
		ri_log(LOG_WARN, "stack is empty!");
#endif
		return NULL;
	}

	return data;
}

int
ri_stack_depth(ri_stack_t *stack)
{
	return stack->stacklist->nelems;
}

