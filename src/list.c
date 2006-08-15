/*
 * $Id: list.c,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <assert.h>

#include "list.h"
#include "memory.h"

ri_list_t *
ri_list_new()
{
	ri_list_t *p;

	p = (ri_list_t *)ri_mem_alloc(sizeof(ri_list_t));

	/* initialize */
	p->data = NULL;
	p->next = NULL;
	p->prev = NULL;

	return p;
}

void
ri_list_append(ri_list_t *list, void *data)
{
	ri_list_t *p;
	ri_list_t *last;

	assert(list);

	if (list) {
		last = ri_list_last(list);
		p = ri_list_new();
		p->data = data;

		last->next = p;
		p->prev    = last;
	} 
}

void
ri_list_remove_last(ri_list_t *list)
{
	ri_list_t *p;

	assert(list);

	if (list) {
		p = ri_list_last(list);
		if (p->prev) {
			list = p->prev;
			list->next = NULL;
			ri_mem_free(p);
		}
		
	}
}

ri_list_t *
ri_list_last(ri_list_t *list)
{
	assert(list);

	while (list->next) list = list->next;

	return list;
}

ri_list_t *
ri_list_first(ri_list_t *list)
{
	ri_list_t *p;

	assert(list);

	p = list;

	while (p->prev) p = p->prev;

	return p->next; /* first element is not used. */
}

void
ri_list_free(ri_list_t *list)
{
	ri_list_t *p;
	ri_list_t *last;

	assert(list);

	p = ri_list_first(list);

	if (!p) {
		/* list is empty */
		ri_mem_free(list);
	} else {
		ri_mem_free(p->prev); /* first element */

		while (p) {
			last = p;
			p    = p->next;
			ri_mem_free(last);
		}
	}

	return;
}

ri_list_t *
ri_list_next(ri_list_t *list)
{
	if (list) return list->next;

	return NULL;
}

ri_list_t *
ri_list_prev(ri_list_t *list)
{
	if (list) return list->prev;

	return NULL;
}
