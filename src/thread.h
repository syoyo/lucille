/*
 *
 * Thread handling.
 *
 * $Id: thread.h,v 1.2 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef THREAD_H
#define THREAD_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WITH_PTHREAD
#include <pthread.h>
#endif

#ifdef WIN32
#include <windows.h>
#include <process.h>
#endif

#include "array.h"

/* thread-specific data */
typedef struct _ri_tsd_t
{
#ifdef WITH_PTHREAD
	pthread_key_t  key;
#elif !defined(NOTHREAD) && defined(WIN32)
	DWORD          key;
#else
	void          *key;
#endif
} ri_tsd_t;

typedef struct _ri_mutex_t
{
#if defined(WITH_PTHREAD)
	pthread_mutex_t  mtx;
#elif !defined(NOTHREAD) && defined(WIN32)
	//HANDLE           mtx;
	CRITICAL_SECTION mtx;
#else
	int              mtx;	/* dummy */
#endif
} ri_mutex_t;

typedef struct _ri_thread_t
{
#ifdef WITH_PTHREAD
	pthread_t id;	
#elif !defined(NOTHREAD) && defined(WIN32)
	HANDLE    id;
#else
	int       id;
#endif	
} ri_thread_t;

typedef struct _ri_thread_once_t
{
#ifdef WITH_PTHREAD
	pthread_once_t ctl;	
#elif !defined(NOTHREAD) && defined(WIN32)
	int            done;
	long           started;		/* TODO: this must be 32bit aligned? */
#else
	int            ctl;
#endif	
} ri_thread_once_t;

typedef struct _ri_thread_cond_t
{
#ifdef WITH_PTHREAD
	pthread_cond_t    cnd;
#elif !defined(NOTHREAD) && defined(WIN32)
	ri_ptr_array_t   *array;
	CRITICAL_SECTION  lock;
#else
	int             cnd;
#endif	
} ri_thread_cond_t;

#ifdef WITH_PTHREAD
#define RI_THREAD_ONCE_INIT { PTHREAD_ONCE_INIT }
#elif !defined(NOTHREAD) && defined(WIN32)
#define RI_THREAD_ONCE_INIT { 0, -1 }
#else
#define RI_THREAD_ONCE_INIT { 0 }
#endif

/* Maximum threads in one process. */
#define RI_MAX_THREADS 16

extern int         ri_thread_supported();
extern void        ri_thread_initialize();
extern void        ri_thread_shutdown();

extern ri_mutex_t *ri_mutex_new    ();
extern void        ri_mutex_init   (ri_mutex_t *mutex);
extern void	   ri_mutex_lock   (ri_mutex_t *mutex);
extern void	   ri_mutex_trylock(ri_mutex_t *mutex);
extern void	   ri_mutex_unlock (ri_mutex_t *mutex);

extern void        ri_thread_once(ri_thread_once_t *once, void (*func)(void));

extern ri_tsd_t   *ri_thread_specific_new();
extern void       *ri_thread_specific_get(ri_tsd_t *tsd);
extern void        ri_thread_specific_set(ri_tsd_t *tsd, void *data);

extern ri_thread_cond_t *ri_thread_cond_new();
extern void        ri_thread_cond_free(ri_thread_cond_t *cond);
extern void        ri_thread_cond_init(ri_thread_cond_t *cond);
extern void        ri_thread_cond_signal(ri_thread_cond_t *cond);
extern void        ri_thread_cond_wait(ri_thread_cond_t *cond,
				       ri_mutex_t *mutex);

extern int         ri_thread_create(ri_thread_t *thread,
				    void *(*func)(void *),
				    void *arg);
extern int         ri_thread_join(ri_thread_t *thread);
extern void        ri_thread_exit(void *valptr);
extern void        ri_thread_free(ri_thread_t *thread);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
