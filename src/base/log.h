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

#define LOG_LEVEL_DEBUG (0)
#define LOG_LEVEL_INFO  (1)
#define LOG_LEVEL_WARN  (2)
#define LOG_LEVEL_ERROR (3)
#define LOG_LEVEL_FATAL (4)

#define LOG_DEBUG  LOG_LEVEL_DEBUG, __FILE__, __LINE__
#define LOG_INFO   LOG_LEVEL_INFO,  __FILE__, __LINE__
#define LOG_WARN   LOG_LEVEL_WARN,  __FILE__, __LINE__
#define LOG_ERROR  LOG_LEVEL_ERROR, __FILE__, __LINE__
#define LOG_FATAL  LOG_LEVEL_FATAL, __FILE__, __LINE__

extern void ri_log_set_debug(int onoff);
extern int  ri_log_get_debug();

extern void ri_log(int level, const char *filename, int linenum, const char *message, ...);

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

