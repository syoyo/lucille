/*
 *
 * Timer routine.
 *
 * $Id: timer.h,v 1.1.1.1 2004/01/06 13:57:13 syoyo Exp $
 */

#ifndef TIMER_H
#define TIMER_H

#include "hash.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_timer_t
{
    ri_hash_t *entrylist;
} ri_timer_t;

/*
 * Usage:
 *
 *   ri_timer_t *tm;
 *   tm = ri_timer_new();
 *
 *   ri_timer_start(tm, "muda");
 *   ...
 *   ri_timer_end(tm, "muda");
 *   printf("elapsed = %f sec\n", ri_timer_elapsed(tm, "muda"));
 *
 */

extern ri_timer_t *ri_timer_new    ();
extern void        ri_timer_free   (ri_timer_t *timer);
extern void        ri_timer_start  (ri_timer_t *timer, const char *name);
extern void        ri_timer_end    (ri_timer_t *timer, const char *name);
extern double      ri_timer_elapsed(ri_timer_t *timer, const char *name);
extern double      ri_timer_elapsed_current(
                                    ri_timer_t *timer, const char *name);

/* for debug */
extern void        ri_timer_dump   (ri_timer_t *timer);

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif
