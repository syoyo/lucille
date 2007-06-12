/*
 * geometry routine
 *
 * $Id: geom.h,v 1.3 2004/06/13 06:44:51 syoyo Exp $
 *
 */

#ifndef GEOM_H
#define GEOM_H

#include "geom.h"
#include "material.h"
#include "vector.h"
#include "shader.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * internal polygonal geometry structure
 */
typedef struct _ri_geom_t {
	ri_vector_t   *positions;	/* vertex position(P)      */
	unsigned int   npositions;
	ri_vector_t   *normals;		/* vertex normal(N)        */
	unsigned int   nnormals;
	ri_vector_t   *tangents;	/* tangent vector	   */
	unsigned int   ntangents;
	ri_vector_t   *binormals;	/* binormal vector	   */
	unsigned int   nbinormals;
	ri_vector_t   *colors;		/* vertex color(Cs)        */
	unsigned int   ncolors;
	ri_vector_t   *opacities;	/* vertex opacity(Os)      */
	unsigned int   nopacities;
	RtFloat       *texcoords;	/* texture coordinates
					 * (st, 2 floats)          */
	unsigned int   ntexcoords;
	unsigned int  *indices;         /* vertex index	           */
	unsigned int   nindices;

	RtFloat        kd;		/* diffuse coefficient	   */
	RtFloat        ks;		/* specular coefficient	   */
	char          *shadername;	/* surface shader name	   */
	ri_shader_t   *shader;		/* surface shader	   */
	ri_material_t *material;

	int            two_side;        /* two-sided or not 	   */
} ri_geom_t;

#if 0
/* surface information used for raytracing */
typedef struct _ri_surface_info_t {
	ri_vector_t    normal;
	ri_vector_t    tangent;
	ri_vector_t    binormal;
	ri_vector_t    pos;
	ri_vector_t    color;
	RtFloat        opacity;		/* TODO: RGB opacity */
	RtFloat        kd, ks;		/* TODO: remove this parameter. */
	RtFloat        u, v;
	//char        *shader;
	ri_geom_t     *geom;
	unsigned int   index;           /* triange index of geom */
	unsigned char  inside;		/* surface represents inside of the
					 * object */
} ri_surface_info_t;
#endif

/* geometry driver */
typedef struct _ri_geom_drv_t {

	/*
	 * parse() converts a geometry described in RenderMan Interface to
	 * internal geometry format.
	 */
	ri_geom_t *( *parse )( RtInt nverts, RtInt n,
			      RtToken tokens[], RtPointer params[] );

	/* TODO: */
	//int        (* dice)(void *data);	/* dice func	 */
	//int	   (* split)(void *data);	/* split func	 */
} ri_geom_drv_t;

extern ri_geom_t * ri_geom_new();

extern void ri_geom_delete( ri_geom_t           *geom );

extern void ri_geom_add_positions( ri_geom_t           *geom,
	unsigned int         npositions,
	const ri_vector_t   *positions );

extern void		ri_geom_add_normals(ri_geom_t           *geom,
	unsigned int         nnormals,
	const ri_vector_t   *normals );

extern void		ri_geom_add_tangents(ri_geom_t           *geom,
	unsigned int         ntangents,
	const ri_vector_t   *tangnets );

extern void		ri_geom_add_binormals(ri_geom_t           *geom,
	unsigned int         nbinormals,
	const ri_vector_t   *binormals );

extern void		ri_geom_add_colors(ri_geom_t           *geom,
	unsigned int         ncolors,
	const ri_vector_t   *normals );

extern void		ri_geom_add_opacities(ri_geom_t           *geom,
	unsigned int         nopacities,
	const ri_vector_t   *opacities );

extern void ri_geom_add_texcoords( ri_geom_t           *geom,
	unsigned int         ntexcoords,
	const RtFloat       *texcoords );

extern void ri_geom_add_indices( ri_geom_t           *geom,
	unsigned int         nindices,
	const unsigned int  *indices );

extern ri_float_t		ri_geom_area(ri_geom_t           *geom );

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif

