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
 * H-Buffer
 * --------
 *
 * Hierarchical & flexible buffer. More general buffer strucuture than
 * G-Buffer [1].
 *
 * [1] Takafumi Saito und Tokiichiro Takahashi.
 *     Comprehensible Rendering of 3-D Shapes.
 *     In Computer Graphics (Proceedings of ACM SIGGRAPH 90),
 *     vol 24, p.197–206, 1990.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "hbuffer.h"

ri_hbuffer_t *
ri_hbuffer_new()
{
    ri_hbuffer_t *hbuf;

    hbuf = (ri_hbuffer_t *)malloc(sizeof(ri_hbuffer_t));

    return hbuf;

}
