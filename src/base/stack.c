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

    p->stack_array = ri_ptr_array_new();

    return p;
}

void
ri_stack_free(ri_stack_t *stack)
{
    ri_ptr_array_free(stack->stack_array);
    ri_mem_free(stack);
}

int
ri_stack_push(ri_stack_t *stack, void *data)
{
    ri_ptr_array_insert(stack->stack_array,
                        stack->stack_array->nelems,
                        data);

    return 0;
}

int
ri_stack_pop(ri_stack_t *stack)
{

    if (stack->stack_array == NULL) {
        ri_log(LOG_WARN, "Stack is NULL.");
        return -1;
    }

    if (stack->stack_array->nelems < 1) {
        ri_log(LOG_WARN, "Stack pop operation for empty stack.");
        return -1;
    }

    ri_ptr_array_remove_at(stack->stack_array,
                           stack->stack_array->nelems - 1);

    return 0;
}

void *
ri_stack_get(ri_stack_t *stack)
{
    void *data;

    if (stack->stack_array == NULL) {
        ri_log(LOG_WARN, "Stack is empty!");
        return NULL;
    }

    if (stack->stack_array->nelems < 1) {
        /* ri_log(LOG_WARN, "Trying to get data from empty stack!"); */
        return NULL;
    }

    data = ri_ptr_array_at(stack->stack_array, stack->stack_array->nelems - 1);

    if (data  == NULL) {
        ri_log(LOG_WARN, "stack is empty!");
        return NULL;
    }

    return data;
}

int
ri_stack_depth(ri_stack_t *stack)
{
    return stack->stack_array->nelems;
}

