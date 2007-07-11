/*
 *
 * Log system.
 *
 * $Id: log.h,v 1.3 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef LOG_H
#define LOG_H

#ifdef __cplusplus
extern "C" {
#endif

#define LOG_INFO   0
#define LOG_WARN   1
#define LOG_ERROR  2
#define LOG_DEBUG  3

extern void ri_log_set_debug(int onoff);
extern int  ri_log_get_debug();

extern void ri_log(int level, const char *message, ...);

#define ri_log_and_return_if(eval) {					\
		if ((eval)) {						\
			ri_log(LOG_INFO, #eval);			\
			return;						\
		}							\
	}								

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

