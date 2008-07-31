/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

/*
 * Debugging interface for the renderer.
 */
#ifndef LUCILLE_DEBUGGER_H
#define LUCILLE_DEBUGGER_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_debugger_t {

	int             sample_xpos;
	int		sample_ypos;

} ri_debugger_t;

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif /* LUCILLE_DEBUGGER_H */
