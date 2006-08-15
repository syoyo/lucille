/*
 * File: entropy_as.h
 *
 * This code is an implementation of
 *
 * - Jaume Rigau , Miquel Feixas and Mateu Sbert,
 *   "Entropy-based Adaptive Sampling",
 *    Graphics Interface 2003.
 *
 * $Id: ibl.h,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef ENTROPY_AS_H
#define ENTROPY_AS_H

#include "vector.h"
#include "light.h"
#include "raytrace.h"
#include "irradcache.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int ri_entropy_as();

#ifdef __cplusplus
}
#endif

#endif
