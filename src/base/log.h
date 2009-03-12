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

/* ---------------------------------------------------------------------------
 *
 * Simple logging system.
 *
 * Example:
 *
 *    ri_log(LOG_DEBUG, "Debug message");
 *    ri_log(LOG_Info , "Value n = %d", n);
 *
 * ------------------------------------------------------------------------ */

#ifndef LUCILLE_LOG_H
#define LUCILLE_LOG_H

#ifdef __cplusplus
extern "C" {
#endif

#define RI_LOG_LEVEL_DEBUG (0)
#define RI_LOG_LEVEL_INFO  (1)
#define RI_LOG_LEVEL_WARN  (2)
#define RI_LOG_LEVEL_ERROR (3)
#define RI_LOG_LEVEL_FATAL (4)

#define LOG_DEBUG  RI_LOG_LEVEL_DEBUG, __FILE__, __LINE__
#define LOG_INFO   RI_LOG_LEVEL_INFO,  __FILE__, __LINE__
#define LOG_WARN   RI_LOG_LEVEL_WARN,  __FILE__, __LINE__
#define LOG_ERROR  RI_LOG_LEVEL_ERROR, __FILE__, __LINE__
#define LOG_FATAL  RI_LOG_LEVEL_FATAL, __FILE__, __LINE__

extern void ri_log_set_debug(      int   onoff);
extern int  ri_log_get_debug();
extern void ri_log_set_level(      int   level);

extern void ri_log          (      int   level,
                             const char *filename,
                                   int   linenum,
                             const char *message,
                             ...);


#define ri_log_and_return_if(eval) {                        \
        if ((eval)) {                                       \
            ri_log(LOG_INFO, #eval);                        \
            return;                                         \
        }                                                   \
    }                                

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif  /* LUCILLE_LOG_H    */

