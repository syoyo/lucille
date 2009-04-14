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
 * ------------------------------------------------------------------------ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <time.h>

#include "log.h"
#include "parallel.h"
#include "hash.h"

static int gdebug    = 0;
static int gloglevel = RI_LOG_LEVEL_INFO;

static char *level_msg[] = {
    "debug",
    "info ",
    "warn ",
    "ERROR",
    "FATAL"
};

/*
 * Message hash to prevent duplicated log message(used in ri_log_once).
 */
static ri_hash_t *g_msg_hash = NULL;

void
ri_log_set_debug(int onoff)
{
    gdebug = onoff;
}

int
ri_log_get_debug()
{
    return gdebug;
}

void
ri_log_set_level(int level)
{
    gloglevel = level;
}

void
ri_log(int level, const char *filename, int linenum, const char *message, ...)
{
    va_list list;

    /* time_t tm; */

    va_start(list, message);

    assert((unsigned int)level < sizeof(level_msg));

    /* time(&tm); */

    if (gdebug) {
        fprintf(stdout, "[lucille] %s:%d %s : ",
            filename, linenum, level_msg[level]);
        vfprintf(stdout, message, list);
        fprintf(stdout, "\n");
        fflush(stdout);
    } else {
        if (level >= gloglevel) {
            fprintf(stdout, "[lucille] %s : ", level_msg[level]);
            vfprintf(stdout, message, list);
            fprintf(stdout, "\n");
            fflush(stdout);
        }
    }
#if 0
        /* print only if master node */
        if (ri_parallel_taskid() == 0) {
            fprintf(stdout, "[%s] %s: %s, line %d | %s",
                    level, message, file, line, ctime(&tm));
        }
#endif
}

void
ri_log_once(
    int         level,
    const char *filename,
    int         linenum,
    const char *message,
    ...) 
{
    va_list ap;
    
    /* TODO: Make this MT-safe. */
    if (g_msg_hash == NULL) {
        g_msg_hash = ri_hash_new();
    }

    char msg[2048];
    char *msg_buf;

    va_start(ap, message);

    vsnprintf(msg, 2047, message, ap);

    if (ri_hash_lookup(g_msg_hash, msg) != NULL) {

        /* Same log message was already printed.
         * Discard this message.
         */
        return;

    } else {

        ri_log(level, filename, linenum, msg);

        /* Cache log message string. */
        msg_buf = strdup(msg);
        ri_hash_insert(g_msg_hash, msg, msg_buf);

    }

}
