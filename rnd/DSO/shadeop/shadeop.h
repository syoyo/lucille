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

/*
 * File: shadeop.h defines shader dso interfaces.
 *
 */

#ifdef __cplusplus
extern "C" {
#endif

#if defined(WIN32)
#  define LSL_EXPORT __declspec(dllexport)
#else   // Posix
#  define LSL_EXPORT
#endif

/**
 * Defines field for shadeop function.
 */
typedef struct li_shade_op_t
{
    char *signature;
    char *init_func;
    char *cleanup_func;
} li_shade_op_t;


#define SHADEOP_TABLE(name) li_shade_op_t LSL_EXPORT name ## _optables []

#define SHADEOP_INIT(method) LSL_EXPORT void * method ()
#define SHADEOP(method) LSL_EXPORT int method (void *initdata, int argc, void **argv)
#define SHADEOP_CLEANUP(func) LSL_EXPORT void * method (void *initdata)

#ifdef __cplusplus
}
#endif
