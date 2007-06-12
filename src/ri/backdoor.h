/*
 * Backdoor interfaces useful for Debugging.
 *
 * $Id: backdoor.h,v 1.1.1.1 2004/01/06 13:57:07 syoyo Exp $
 */

#ifndef BACKDOOR_H
#define BACKDOOR_H

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_backdoor_world_begin_cb(void (*callback)(void));
extern void ri_backdoor_world_end_cb(void (*callback)(void));
extern void ri_backdoor_render_end_cb(void (*callback)(void));
extern void heavens_door();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

