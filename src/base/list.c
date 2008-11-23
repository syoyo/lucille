/*
 * $Id$
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <assert.h>

#include "list.h"
#include "memory.h"

/*
 * Function: ri_list_new
 *
 *   Create a list data structure.
 * 
 * Parameters:
 *
 *   None
 *
 * Returns:
 *
 *   Pointer to the newly created list data. 
 *
 */
ri_list_t *
ri_list_new()
{
    ri_list_t *p;

    p = (ri_list_t *)ri_mem_alloc(sizeof(ri_list_t));

    p->data = NULL;
    p->next = NULL;
    p->prev = NULL;

    return p;
}

/*
 * Function: ri_list_append
 *
 *   Adds data to the list.
 * 
 * Parameters:
 *
 *   - list : The list 
 *   - data : Data to be appended to the list.
 *            Just a pointer is stored to the list. 
 *
 * Returns:
 *
 *   0 if success, -1 fail. 
 *
 */
int
ri_list_append(ri_list_t *list, void *data)
{
    ri_list_t *p;
    ri_list_t *last;

    if (list == NULL) return -1;
    if (data == NULL) return -1;

    last       = ri_list_last(list);
    p          = ri_list_new();
    p->data    = data;

    last->next = p;
    p->prev    = last;


    return 0;
}

/*
 * Function: ri_list_remove_last
 *
 *   Removes last element from the list.
 * 
 * Parameters:
 *
 *   - list : The list 
 *
 * Returns:
 *
 *   0 if success, -1 if list is null. 
 *
 */
int
ri_list_remove_last(ri_list_t *list)
{
    ri_list_t *p;

    if (list == NULL) return -1;


    p = ri_list_last(list);

    if (p->prev) {

        list       = p->prev;
        list->next = NULL;

        ri_mem_free(p);

    }
        
    return 0;

}

/*
 * Function: ri_list_remove_first
 *
 *   Removes first element from the list.
 * 
 * Parameters:
 *
 *   - list : The list 
 *
 * Returns:
 *
 *   Head of the list. 
 *
 */
ri_list_t *
ri_list_remove_first(ri_list_t *list)
{
    ri_list_t *hdr;
    ri_list_t *first;
    ri_list_t *second;

    /*
     * Before remove : [hdr] -> first -> second
     * After remove  : [hdr] -> second
     */

    if (list == NULL) return NULL;

    first = ri_list_first(list);
    assert(first != NULL);

    second = first->next;
    if (second) {

        hdr = first->prev;  // p->prev points to the header item in the list.
        assert(hdr);

        hdr->next   = second;
        second->prev = hdr;
        
        ri_mem_free(first);

        return second;

    } else {
        return NULL;
    }
        
}

/*
 * Function: ri_list_last
 *
 *   Gets last element in the list.
 * 
 * Parameters:
 *
 *   - list : The list 
 *
 * Returns:
 *
 *   Pointer to the last element in the list. 
 *
 */
ri_list_t *
ri_list_last(ri_list_t *list)
{
    assert(list);

    if (list == NULL) return NULL;

    while (list->next) list = list->next;

    return list;
}

/*
 * Function: ri_list_fist
 *
 *   Gets first element in the list.
 * 
 * Parameters:
 *
 *   - list : The list 
 *
 * Returns:
 *
 *   Pointer to the first element in the list. 
 *
 */
ri_list_t *
ri_list_first(ri_list_t *list)
{
    ri_list_t *p;

    if (list == NULL) return NULL;

    p = list;

    while (p->prev) p = p->prev;

    return p->next; /* first element is not used. */
}

/*
 * Function: ri_list_free
 *
 *   Frees the list data structure.
 *   Not that ri_list_free()does not free its content(i.e, list->data)
 *   Users should free each list->data in the list manually before calling
 *   ri_list_free() to prevent memory leaks.
 * 
 * Parameters:
 *
 *   - list : The list to be freed
 *
 * Returns:
 *
 *   None. 
 *
 */
void
ri_list_free(ri_list_t *list)
{
    ri_list_t *p;
    ri_list_t *last;

    assert(list);

    if (list == NULL) return;

    p = ri_list_first(list);

    if (!p) {

        ri_mem_free(list);

    } else {

        ri_mem_free(p->prev); /* Delete first element */

        /*
         * Traverse all element in the list, deleting each element.
         */
        while (p) {

            last = p;
            p    = p->next;
            ri_mem_free(last);

        }

    }

    return;
}

/*
 * Function: ri_list_next
 *
 *   Gets next element in the list.
 * 
 * Parameters:
 *
 *   - list : The list
 *
 * Returns:
 *
 *   Pointer to the next element of the list. 
 *   If input is NULL, return NULL.
 *
 */
ri_list_t *
ri_list_next(ri_list_t *list)
{
    if (list) return list->next;

    return NULL;
}

/*
 * Function: ri_list_prev
 *
 *   Gets previous element in the list.
 * 
 * Parameters:
 *
 *   - list : The list
 *
 * Returns:
 *
 *   Pointer to the previous element of the list. 
 *   If input is NULL, return NULL.
 *
 */
ri_list_t *
ri_list_prev(ri_list_t *list)
{
    if (list) return list->prev;

    return NULL;
}
