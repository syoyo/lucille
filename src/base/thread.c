/*
 * Thread implementaion.
 *
 * $Id: thread.c,v 1.2 2004/06/13 06:44:51 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#include <process.h>
#endif

#ifdef WITH_PTHREAD
#include <pthread.h>
#endif

#include "thread.h"
#include "memory.h"

#ifdef WIN32
static DWORD cond_tls;
#endif

/*
 * Most Win32 thread code is from Pthread for Win32 and glib.
 */

int
ri_thread_supported()
{
#if !defined(NOTHREAD) && defined(WIN32)
    return 1;
#elif defined(WITH_PTHREAD)
    return 1;
#else
    return 0;
#endif
}

void
ri_thread_initialize()
{
#if !defined(NOTHREAD) && defined(WIN32)
    cond_tls = TlsAlloc();    
#endif
}

void
ri_thread_shutdown()
{
#if !defined(NOTHREAD) && defined(WIN32)
    TlsFree(cond_tls);
#endif    
}



ri_mutex_t *
ri_mutex_new()
{
    ri_mutex_t *p = NULL;

    p = (ri_mutex_t *)ri_mem_alloc(sizeof(ri_mutex_t));
    if (p == NULL) {
        printf("malloc error\n");
        exit(1);
    }

    return p;
}

int
ri_mutex_free(ri_mutex_t *mutex)
{
    ri_mem_free(mutex);

    return 0;
}

void
ri_mutex_init(ri_mutex_t *mutex)
{
#ifdef WITH_PTHREAD
    pthread_mutex_init(&(mutex->mtx), NULL);
#elif !defined(NOTHREAD) && defined(WIN32)
    InitializeCriticalSection(&(mutex->mtx));
    //mutex->mtx = CreateMutex(NULL, FALSE, NULL);
#else
    mutex->mtx = 1;
#endif
}

void
ri_mutex_lock(ri_mutex_t *mutex)
{
#ifdef WITH_PTHREAD
    pthread_mutex_lock(&(mutex->mtx));
#elif !defined(NOTHREAD) && defined(WIN32)
    EnterCriticalSection(&(mutex->mtx));
    //WaitForSingleObject(mutex->mtx, INFINITE);
#else
    (void)mutex; /* for preventing gcc warning */
#endif
}

void
ri_mutex_trylock(ri_mutex_t *mutex)
{
    (void)mutex; /* for preventing gcc warning */
}

void
ri_mutex_unlock(ri_mutex_t *mutex)
{
#ifdef WITH_PTHREAD
    pthread_mutex_unlock(&(mutex->mtx));
#elif !defined(NOTHREAD) && defined(WIN32)
    LeaveCriticalSection(&(mutex->mtx));
    //ReleaseMutex(mutex->mtx);
#else
    (void)mutex; /* for preventing gcc warning */
#endif
}

void
ri_thread_once(ri_thread_once_t *once, void (*func)(void))
{
#ifdef WITH_PTHREAD
    pthread_once(&(once->ctl), func);    
#elif !defined(NOTHREAD) && defined(WIN32)
    if (!once->done) {
        /* TODO: once->started must be 16byte aligned
         * on multi-processor machine?
         */
        if (InterlockedIncrement(&(once->started)) == 0) {
            /* Called first */
            (*func)();
            once->done = 1;
        } else {
            /* Block until other thread finishes ri_thread_once() */
            while (!once->done) {
                /* light wait loop */
                Sleep(0);
            }
        }
    }
#else
    /* no thread support */
    if (!(once->ctl)) {
        (*func)();
        once->ctl = 1;
    }
#endif
}

ri_tsd_t *
ri_thread_specific_new()
{
    ri_tsd_t *p;
    
    p = ri_mem_alloc(sizeof(ri_tsd_t));

#ifdef WITH_PTHREAD
    if (pthread_key_create(&(p->key), free) != 0) {
        fprintf(stderr, "cannot create key\n");
        exit(-1);
    }
#elif !defined(NOTHREAD) && defined(WIN32)
    p->key = TlsAlloc();
    if (p->key == TLS_OUT_OF_INDEXES) {
        fprintf(stderr, "cannot create Tls\n");
        exit(-1);
    }
#else
    /* no thread support */
    p->key = NULL;
#endif

    return p;
}

void *
ri_thread_specific_get(ri_tsd_t *tsd)
{
    void *p = NULL;

#ifdef WITH_PTHREAD

    p = pthread_getspecific(tsd->key);

#elif !defined(NOTHREAD) && defined(WIN32)

    p = TlsGetValue(tsd->key);

#else
    /* no thread support */
    p = tsd->key;
#endif

    return p;
}

void
ri_thread_specific_set(ri_tsd_t *tsd, void *data)
{
#ifdef WITH_PTHREAD

    pthread_setspecific(tsd->key, data);

#elif !defined(NOTHREAD) && defined(WIN32)

    TlsSetValue(tsd->key, data);

#else
    /* no thread support */
    tsd->key = data;
#endif
}

int
ri_thread_create(
    ri_thread_t *thread,
    void        *(*func)(void *),
    void        *arg)
{
    int ret;
#ifdef WITH_PTHREAD

    ret = pthread_create(&(thread->id), NULL, func, arg);

#elif !defined(NOTHREAD) && defined(WIN32)

    unsigned int addr;    /* dummy */

    /* If we use _beginthreadex(), must call CloseHandle(). 
     * If we use _beginthread(), must not call CloseHandle().
     */
    thread->id = (HANDLE)_beginthreadex(
            NULL, 0,
             (unsigned int (__stdcall *)(void *))func, arg,
            0, &addr);
    if (thread->id == NULL) {
        fprintf(stderr, "cant' create thread.\n");
        exit(-1);
    }

    ret = 0;
#else
    /* no thread support */
    (void)thread;
    (*func)(arg);
    ret = 0;
#endif
    return ret;
}

int
ri_thread_join(ri_thread_t *thread)
{
    int ret;

#ifdef WITH_PTHREAD

    ret = pthread_join(thread->id, NULL);    

#elif !defined(NOTHREAD) && defined(WIN32)
    
    ret = (int)WaitForSingleObject(thread->id, INFINITE);

#else
    /* no thread support */
    (void)thread;
    ret = 1;
#endif

    return ret;
}

void
ri_thread_exit(void *val_ptr)
{
#ifdef WITH_PTHREAD
    pthread_exit(val_ptr);
#else
    (void)val_ptr;

#endif
}

void
ri_thread_free(ri_thread_t *thread)
{
#if !defined(NOTHREAD) && defined(WIN32)
    CloseHandle(thread->id);
#endif
    ri_mem_free(thread);
}

ri_thread_cond_t *
ri_thread_cond_new()
{
    ri_thread_cond_t *p;

    p = (ri_thread_cond_t *)ri_mem_alloc(sizeof(ri_thread_cond_t));

    
    return p;
}

void
ri_thread_cond_free(ri_thread_cond_t *cond)
{
#if !defined(NOTHREAD) && defined(WIN32)
    DeleteCriticalSection(&(cond->lock));
    ri_ptr_array_free(cond->array);
#endif
    ri_mem_free(cond);
}

void
ri_thread_cond_init(ri_thread_cond_t *cond)
{
#ifdef WITH_PTHREAD
    pthread_cond_init(&(cond->cnd), NULL);
#elif !defined(NOTHREAD) && defined(WIN32)
    cond->array = ri_ptr_array_new();
    InitializeCriticalSection(&(cond->lock)); 
#else
    /* no thread support */
    (void)cond;
#endif
}

void
ri_thread_cond_signal(ri_thread_cond_t *cond)
{
#ifdef WITH_PTHREAD
    pthread_cond_signal(&(cond->cnd));
#elif !defined(NOTHREAD) && defined(WIN32)
    EnterCriticalSection(&(cond->lock));

    if (cond->array->nelems > 0) {
        SetEvent(ri_ptr_array_at(cond->array, 0));
        ri_ptr_array_remove_at(cond->array, 0);
    }

    LeaveCriticalSection(&(cond->lock));
#else
    /* no thread support */
    (void)cond;
#endif

}

void
ri_thread_cond_wait(ri_thread_cond_t *cond, ri_mutex_t *mutex)
{
#ifdef WITH_PTHREAD
    pthread_cond_wait(&(cond->cnd), &(mutex->mtx));
#elif !defined(NOTHREAD) && defined(WIN32)
    DWORD  ret;
    HANDLE event = TlsGetValue(cond_tls);

    if (!event) {
        event = CreateEvent(0, FALSE, FALSE, 0);
        TlsSetValue(cond_tls, event);
    }

    EnterCriticalSection(&(cond->lock));

    WaitForSingleObject(event, 0);

    ri_ptr_array_insert(cond->array, cond->array->nelems, event);

    LeaveCriticalSection(&(cond->lock));

    ri_mutex_unlock(mutex);

    ret = WaitForSingleObject(event, INFINITE);

    ri_mutex_lock(mutex);

    if (ret == WAIT_TIMEOUT) {
        EnterCriticalSection(&(cond->lock));

        WaitForSingleObject(event, 0);

        LeaveCriticalSection(&(cond->lock));
    }
#else
    /* no thread support */
    (void)cond;
    (void)mutex;
#endif

}
