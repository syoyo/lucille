/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

/*
 *   File: path.h
 *
 *       Header file for ray path structure & methods.       
 */
#ifndef PATH_H__
#define PATH_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "brdf.h"

#define RI_MAX_PATH_VERTICES 100

/*
 * Struct: path_vertex_t
 *
 *     Structure of light path vertex.
 */
typedef struct path_vertex
{

	ri_vector_t  P;			/* position			*/
	ri_vector_t  N;			/* normal			*/

	ri_vector_t  in_dir;		/* incoming direction		*/
	ri_vector_t  out_dir;		/* outgoing direction		*/

	ri_brdf_t   *brdf;

	int          is_specular;	/* 1 if this vertex is specular */
	int          is_light;		/* 1 if this vertex is on light */

} path_vertex_t;

/*
 * Struct: path_t
 *
 *     Structure of light path nodes.
 */
typedef struct path
{
	int           nvertices;
	path_vertex_t vertices[RI_MAX_PATH_VERTICES];

} path_t;


#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif	/* PATH_H__ */
