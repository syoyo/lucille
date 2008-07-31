/*
 * Subdivision surface(Catmull-Clark) implementation
 *
 * Original C++ by Yusuke Yasui, Converted to C by Syoyo Fujita.
 */
#ifndef LUCILLE_SUBD_H
#define LUCILLE_SUBD_H

#include "vector.h"
#include "ri.h"
#include "log.h"
#include "apitable.h"
#include "attribute.h"
#include "geom.h"
#include "render.h"
#include "context.h"

#define MAXSUBDIVLEVEL 4

#ifdef __clupslus
extern "C" {
#endif

typedef struct _ri_subd_vertex_t{
	double p[3]; 
	double st[2];
} ri_subd_vertex_t;

typedef struct _ri_subd_face_t {
	int v_id[4];
} ri_subd_face_t;

typedef struct _ri_subd_edge_t {
	int           pair;
	unsigned char boundary;
} ri_subd_edge_t;

typedef struct _ri_subd_t
{
	ri_array_t *vertex;
	ri_array_t *face;
	ri_ptr_array_t *edges;

} ri_subd_t;

extern ri_subd_t *ri_subd_new();
extern void    ri_subd_delete(ri_subd_t *subd);

extern void ri_subd_subdivide(ri_subd_t *new_subd, const ri_subd_t *old_subd);

#ifdef __clupslus
extern "C" {
#endif

#endif	/* LUCILLE_SUBD_H */
