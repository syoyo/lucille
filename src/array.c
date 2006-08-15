#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "array.h"
#include "log.h"

/* default allocation size */
#define DEFAULT_ALLOC_SIZE 8

static void ri_array_resize_intl    (ri_array_t *array, unsigned int size);
static void ri_ptr_array_resize_intl(ri_ptr_array_t *array, unsigned int size);

/*
 * Function: ri_array_new
 *
 *     Allocates new dynamic array data structure.
 *
 * Parameters:
 *
 *     element_size - Size of element stored.
 *
 * Returns:
 *
 *     Allocated dyamic array data structure.
 */
ri_array_t *
ri_array_new(unsigned int element_size)
{
	unsigned int  i;
	ri_array_t   *p;

	p = (ri_array_t *)ri_mem_alloc(sizeof(ri_array_t));

	p->alloc         = DEFAULT_ALLOC_SIZE * element_size;
	p->element_size  = element_size;
	p->nelems        = 0;
	p->data          = (char *)ri_mem_alloc(p->alloc);

	/* fill with zero */	
	for (i = 0; i < p->alloc; i++) {
		p->data[i] = 0;
	}

	return p;
}

/*
 * Function: ri_array_free
 *
 *     Frees dynamic array data structure.
 *     The function also frees its contents memory.
 *
 * Parameters:
 *
 *     *array - dynamic array data structure freed.
 *
 * Returns:
 *
 *     None.
 */
void
ri_array_free(ri_array_t *array)
{
	ri_mem_free(array->data);
	ri_mem_free(array);
}


/*
 * Function: ri_array_insert
 *
 *     Inserts data into arbitrary index position.
 *     Array is dynamically expanded if index exceeds current array size.
 *
 * Parameters:
 *
 *     *array - Dynamic array data the data is inserted.
 *     index  - Insert position of array index.
 *     data   - contents.
 *
 * Returns:
 *
 *     None.
 */
void
ri_array_insert(ri_array_t *array, unsigned int index, const void *data)
{
	if (index * array->element_size >= array->alloc) {
		/* resize array with twice larger */
		ri_array_resize_intl(array, index * array->element_size * 2);
	}

	memcpy((array->data + (index * array->element_size)),
	       data, array->element_size);

	if (array->nelems <= index + 1) {
		array->nelems = index + 1;
	}
}

/*
 * Function: ri_array_at
 *
 *     Returns the array content at index.
 *
 * Parameters:
 *
 *     *array - Dynamic array data.
 *     index  - Array index.
 *
 * Returns:
 *
 *     Pointer to the content.
 */
const char *
ri_array_at(const ri_array_t *array, unsigned int index)
{
#ifdef DEBUG
	if (index * array->element_size > array->alloc) {
		ri_log(LOG_DEBUG, "index * array->element_size > array->alloc");
		return NULL;
	}
#endif

	return (array->data + (index * array->element_size));
}

/*
 * Function: ri_array_remove_at
 *
 *     Removes array element.
 *
 * Parameters:
 *
 *     *array - Dynamic array data.
 *     index  - Array index to be removed.
 *
 * Returns:
 *
 *     None.
 */
void
ri_array_remove_at(ri_array_t *array, unsigned int index)
{
	ri_log_and_return_if(array == NULL);
	ri_log_and_return_if(index > array->nelems - 1);

	if (index != array->nelems - 1) {
		memmove(array->data + index * array->element_size,
			array->data + (index + 1) * array->element_size,
			(array->nelems - index - 1) * array->element_size);
	}

	array->nelems--;
	memset(array->data + array->nelems * array->element_size,
	       0,
	       array->element_size);
}

ri_ptr_array_t *
ri_ptr_array_new()
{
	unsigned int  i;
	ri_ptr_array_t   *p;

	p = (ri_ptr_array_t *)ri_mem_alloc(sizeof(ri_ptr_array_t));

	p->alloc         = DEFAULT_ALLOC_SIZE;
	p->nelems        = 0;
	p->data          = (void **)ri_mem_alloc(sizeof(void *) * p->alloc);

	/* fill with NULL */	
	for (i = 0; i < p->alloc; i++) {
		p->data[i] = NULL;
	}

	return p;
}

void
ri_ptr_array_free(ri_ptr_array_t *array)
{
	if (array == NULL) return;

	ri_mem_free(array->data);
	array->data = NULL;
	ri_mem_free(array);
}


void
ri_ptr_array_insert(ri_ptr_array_t *array, unsigned int index, void *data)
{
	if (index >= array->alloc) {
		/* resize array with twice larger */
		ri_ptr_array_resize_intl(array, index * 2);
	}

	array->data[index] = data;

	if (array->nelems <= index + 1) {
		array->nelems = index + 1;
	}
}

void *
ri_ptr_array_at(ri_ptr_array_t *array, unsigned int index)
{
#ifdef DEBUG
	if (index > array->alloc) {
		ri_log(LOG_DEBUG, "index >  array->alloc");
		return NULL;
	}
#endif

	return array->data[index];
}

void
ri_ptr_array_traverse(ri_ptr_array_t *array,
		      void (*travfunc)(void *data))
{
	unsigned int i;

	for (i = 0; i < array->nelems; i++) {
		travfunc(array->data[i]);
	}
}

void
ri_ptr_array_remove(ri_ptr_array_t *array, void *data)
{
	unsigned int i;

	ri_log_and_return_if(array == NULL);

	for (i = 0; i < array->nelems; i++) {
		if (array->data[i] == data) {
			ri_ptr_array_remove_at(array, i);
			return;
		}
	}

	return;
}

void
ri_ptr_array_remove_at(ri_ptr_array_t *array, unsigned int index)
{
	ri_log_and_return_if(array == NULL);
	ri_log_and_return_if(index > array->nelems - 1);

	if (index != array->nelems - 1) {
		memmove(array->data + index, array->data + index + 1,
			sizeof(void *) * (array->nelems - index - 1));
	}

	array->nelems--;
	array->data[array->nelems] = NULL;
	
}

static void
ri_array_resize_intl(ri_array_t *array, unsigned int size)
{
	char         *p;
	unsigned int  i;

	ri_log_and_return_if(size < array->alloc);

	p   = (char *)ri_mem_alloc(size);

	memcpy(p, array->data, array->alloc);

	/* fill expanded area with 0 */	
	for (i = array->alloc; i < size; i++) {
		p[i] = 0;
	}

	ri_mem_free(array->data);
	array->data  = p;
	array->alloc = size;
}

static void
ri_ptr_array_resize_intl(ri_ptr_array_t *array, unsigned int size)
{
	void         **p;
	unsigned int   i;

	ri_log_and_return_if(size < array->alloc);

	p   = (void **)ri_mem_alloc(sizeof(void *) * size);

	memcpy(p, array->data, array->alloc * sizeof(void *));

	/* fill expanded area with NULL */	
	for (i = array->alloc; i < size; i++) {
		p[i] = NULL;
	}

	ri_mem_free(array->data);
	array->data  = p;
	array->alloc = size;
}
