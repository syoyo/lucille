/*
 * MPI routine.
 *
 * $Id: parallel.h,v 1.1.1.1 2004/01/06 13:57:10 syoyo Exp $
 */

#ifndef PARALLEL_H
#define PARALLEL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WITH_MPI
#include <mpi.h>
#endif

typedef struct _ri_parallel_status_t
{
#ifdef WITH_MPI
    MPI_Status status;
#else
    int status;        /* dummy    */
#endif
} ri_parallel_status_t;

/* this is needed for non-blocking communication. */
typedef struct _ri_parallel_request_t
{
#ifdef WITH_MPI
    MPI_Request request;
#else
    int request;        /* dummy */
#endif
} ri_parallel_request_t;

extern void ri_parallel_init    (int                    *argc,
                                 char                 ***argv);
extern void ri_parallel_finalize();
extern int  ri_parallel_ntasks  ();
extern int  ri_parallel_taskid  ();
extern void ri_parallel_barrier ();
extern void ri_parallel_bcast   (void                  *src,
                                 size_t                 size);  
extern void ri_parallel_gather  (void                  *src,
                                 void                  *dst,
                                 size_t                 size);  
extern void ri_parallel_send    (void                  *src,
                                 size_t                 size,
                                 int                    dest,
                                 int                    tag);  
extern void ri_parallel_recv    (void                  *src,
                                 size_t                 size,
                                 int                    source,
                                 int                    tag,
                                 ri_parallel_status_t  *status);  
extern void ri_parallel_irecv   (void                  *src,
                                 size_t                 size,
                                 int                    source,
                                 int                    tag,
                                 ri_parallel_request_t *request);  
extern int  ri_parallel_test    (ri_parallel_request_t *request,
                                 ri_parallel_status_t  *status);
extern int  ri_parallel_wait    (ri_parallel_request_t *request,
                                 ri_parallel_status_t  *status);

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif
