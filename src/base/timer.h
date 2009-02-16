/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * Copyright 2003-2203 Syoyo Fujita.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors nor the names of their contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef LUCILLE_TIMER_H
#define LUCILLE_TIMER_H

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

#endif  /* LUCILLE_TIMER_H */
