/*
 * Film interface.
 *
 * $Id: film.c,v 1.1 2004/10/10 15:17:20 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ri.h"
#include "vector.h"
#include "option.h"
#include "render.h"
#include "memory.h"
#include "film.h"

/*
 * Function: ri_film_new
 *
 *     Creates new film plane object.
 *
 * Parameters:
 *
 *     None.
 *
 * Return:
 *
 *     Newly created film plane object.
 */
ri_film_t *
ri_film_new()
{
	return NULL;
}

/*
 * Function: ri_film_free
 *
 *     Frees film plane object's memory.
 *
 * Parameters:
 *
 *     *film - film to be freed.
 *
 * Return:
 *
 *     None.
 */
void
ri_film_free(ri_film_t *film)
{
	ri_mem_free(film);
}
