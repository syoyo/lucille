/*
 * Simple path tracing implementation.
 * 
 * Syoyo Fujita
 * 
 * $Id: pathtrace.h,v 1.1 2004/05/04 02:26:49 syoyo Exp $
 */
#ifndef PATHTRACE_H
#define PATHTRACE_H

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_transport_pathtrace(const ri_display_drv_t *ddrv);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
