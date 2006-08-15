/*
 * Metropolis Light Transport implementation.
 * 
 * Syoyo Fujita
 * 
 * $Id: metropolis.h,v 1.1 2004/02/26 16:22:42 syoyo Exp $
 */
#ifndef METROPOLIS_H
#define METROPOLIS_H

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_transport_mlt(const ri_display_drv_t *ddrv);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
