/*
 * Light mapping implementation.
 * 
 * Syoyo Fujita
 * 
 * $Id: pathtrace.h,v 1.1 2004/05/04 02:26:49 syoyo Exp $
 */
#ifndef LIGHTMAPPING_H
#define LIGHTMAPPING_H

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_transport_lightmapping(const ri_display_drv_t *ddrv);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
