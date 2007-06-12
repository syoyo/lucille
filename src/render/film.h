/*
 * Film plane interface.
 *
 * $Id: film.h,v 1.1 2004/10/10 15:17:21 syoyo Exp $
 */

#ifndef FILM_H
#define FILM_H

#include "ri.h"
#include "display.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_film_t
{
	int dummy;
} ri_film_t;

extern ri_film_t *ri_film_new();
extern void       ri_film_free(ri_film_t *film);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
