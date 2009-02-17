/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * Copyright 2003-2203 Syoyo Fujita.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors nor the names of their contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* ----------------------------------------------------------------------------
 *
 * File: parallel.h
 *
 *   Header file for wrapper API of message passing interface
 *
 * ------------------------------------------------------------------------- */

#ifndef LUCILLE_PARALLEL_H
#define LUCILLE_PARALLEL_H

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

#endif  /* LUCILLE_PARALLEL_H */
