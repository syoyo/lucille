/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

/*
 * global rendering manager object.
 *
 * $Id: render.h,v 1.4 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef LUCILLE_RENDERER_H
#define LUCILLE_RENDERER_H

#include "list.h"
#include "light.h"
#include "hash.h"
#include "queue.h"

#include "context.h"
#include "geom.h"
#include "display.h"
#include "scene.h"
#include "debugger.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef MAX_RIBPATH
#define MAX_RIBPATH 1024
#endif

#define BUCKET_ORDER_SPIRAL          0
#define BUCKET_ORDER_SCANLINE        1
#define BUCKET_ORDER_HILBERT         2

typedef struct _ri_statistic_t
{
    /* raytracing statistics */
    unsigned long long ngridtravs;
    unsigned long long ntesttris;
    unsigned long long nrays;
    unsigned long long nmailboxhits;
} ri_statistic_t;

typedef struct _ri_render_t
{
    /* TODO: Support for multiple context */

    ri_context_t   *context;                /* rendering context        */

    ri_hash_t      *geom_drvs;              /* geometry driver list     */
    ri_hash_t      *display_drvs;           /* display driver list      */

    ri_display_drv_t *curr_display_drv;     /* currently selected
                                             * display driver           */

    ri_statistic_t  stat;                   /* statistics for rendering */


    void           (*progress_handler)(void);

#if 0    // to be removed.
    ri_vector_t     bmin;            /* bounding box of scene */
    ri_vector_t     bmax;            /* bounding box of scene */

    ri_float_t      bmaxwidth;        /* maximum width of bounding
                         * box.                  */
#endif

    int             dd_use_callback;    /* use callback display
                           driver?         */

    char            ribpath[MAX_RIBPATH];    /* RIB file path     */

    ri_debugger_t  *debugger;

    int             is_debug_mode;    

    /* QMC related stuff */
    int             **perm_table;        /* permutation sequences */


    /* background map(environment map) */
    ri_texture_t   *background_map;

    ri_scene_t     *scene;

    int             bucket_size;
    ri_mt_queue_t  *bucket_queue;
    int             bucket_order;    

    int             nthreads;


} ri_render_t;

extern void         ri_render_init();    /* should be called in RiBegin() */
extern ri_render_t *ri_render_get ();    /* get global renderer */
extern void         ri_render_free();    /* should be called in RiEnd() */

extern void         ri_render_setup(ri_render_t       *render);
extern void         ri_render_frame();

/* register geometry driver */
extern void         ri_render_register_geom_drv(
                                    ri_render_t       *render,
                                    const char        *type,
                                    ri_geom_drv_t     *drv);

/* register display driver */
extern void         ri_render_register_display_drv(
                                    ri_render_t       *render,
                                    const char        *type,
                                    ri_display_drv_t  *drv);

extern void         ri_render_set_progress_handler(
                                    ri_render_t       *render,
                                    void             (*func)(void));

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif    /* LUCILLE_RENDERER_H */
