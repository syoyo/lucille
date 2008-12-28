#ifndef MY_TIMER_H
#define MY_TIMER_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WIN32
#include <sys/time.h>
#endif

typedef struct _mytimer_t
{
#ifdef WIN32
	LARGE_INTEGER  time;	
#else
	struct timeval time;
#endif
} mytimer_t;

//
// -- API
//
extern void   get_time(mytimer_t *t);
extern double elapsed_time(mytimer_t *start, mytimer_t *end);

#ifdef __cplusplus
}
#endif

#endif  /* MY_TIMER_H */
