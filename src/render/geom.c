/*
 * $Id: geom.c,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "geom.h"
#include "memory.h"
#include "log.h"


ri_geom_t *
ri_geom_new()
{
	ri_geom_t *p;

	p = ( ri_geom_t * )ri_mem_alloc( sizeof( ri_geom_t ) );

	p->positions  = NULL;
	p->normals    = NULL;
	p->tangents   = NULL;
	p->binormals  = NULL;
	p->colors     = NULL;
	p->opacities  = NULL;
	p->texcoords  = NULL;
	p->indices    = NULL;

	p->nnormals   = 0;
	p->ntangents  = 0;
	p->nbinormals = 0;
	p->npositions = 0;
	p->nindices   = 0;
	p->ntexcoords = 0;
	p->ncolors    = 0;
	p->nopacities = 0;

	p->kd = 0.0;
	p->ks = 0.0;

	p->two_side = 0;

	p->shadername = NULL;
	p->shader     = NULL;
	p->material   = NULL;


	return p;
}

void
ri_geom_delete(
	ri_geom_t *geom )
{
	ri_mem_free( geom->positions );
	ri_mem_free( geom->normals );
	ri_mem_free( geom->tangents );
	ri_mem_free( geom->binormals );
	ri_mem_free( geom->indices );
	ri_mem_free( geom->texcoords );
	ri_mem_free( geom->colors );
	ri_mem_free( geom->opacities );
	ri_mem_free( geom->shader );
	ri_mem_free( geom );
}

void
ri_geom_add_positions(
	ri_geom_t           *geom,
	unsigned int         npositions,
	const ri_vector_t   *positions )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( npositions == 0 );
	ri_log_and_return_if( positions == NULL );


	size = sizeof( ri_vector_t ) * npositions;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, positions, size );

	geom->positions = p;
	geom->npositions = npositions;
}

void
ri_geom_add_normals(
	ri_geom_t           *geom,
	unsigned int         nnormals,
	const ri_vector_t   *normals )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( nnormals == 0 );
	ri_log_and_return_if( normals  == NULL );

	size = sizeof( ri_vector_t ) * nnormals;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, normals, size );

	geom->normals  = p;
	geom->nnormals = nnormals;
}

void
ri_geom_add_tangents(
	ri_geom_t           *geom,
	unsigned int         ntangents,
	const ri_vector_t   *tangents )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( ntangents == 0 );
	ri_log_and_return_if( tangents  == NULL );

	size = sizeof( ri_vector_t ) * ntangents;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, tangents, size );

	geom->tangents  = p;
	geom->ntangents = ntangents;
}

void
ri_geom_add_binormals(
	ri_geom_t           *geom,
	unsigned int         nbinormals,
	const ri_vector_t   *binormals )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( nbinormals == 0 );
	ri_log_and_return_if( binormals  == NULL );

	size = sizeof( ri_vector_t ) * nbinormals;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, binormals, size );

	geom->binormals  = p;
	geom->nbinormals = nbinormals;
}

void
ri_geom_add_colors(
	ri_geom_t           *geom,
	unsigned int         ncolors,
	const ri_vector_t   *colors )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( ncolors == 0 );
	ri_log_and_return_if( colors  == NULL );

	size = sizeof( ri_vector_t ) * ncolors;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, colors, size );

	geom->colors  = p;
	geom->ncolors = ncolors;
}

void
ri_geom_add_opacities(
	ri_geom_t           *geom,
	unsigned int         nopacities,
	const ri_vector_t   *opacities )
{
	ri_vector_t *p;
	size_t       size;

	ri_log_and_return_if( nopacities == 0 );
	ri_log_and_return_if( opacities  == NULL );

	size = sizeof( ri_vector_t ) * nopacities;

	p = ( ri_vector_t * )ri_mem_alloc( ( long )size );

	memcpy( p, opacities, size );

	geom->opacities  = p;
	geom->nopacities = nopacities;
}

/*
 * Function: ri_geom_add_texcoords
 *
 *    Add texture coord vector to the geometry.
 *
 * Parameters:
 *
 *    geom       - the geometry to which texture coord vector is added.
 *    ntexcoords - the number of coords in the vector *texcoords*.
 *    texcoords  - the texture coord vector to be added.
 *
 * Returns:
 *
 *    None.
 *
 */
void
ri_geom_add_texcoords(
	ri_geom_t      *geom,
	unsigned int    ntexcoords,
	const RtFloat  *texcoords )
{
	RtFloat     *p;
	size_t       size;

	ri_log_and_return_if( ntexcoords == 0 );
	ri_log_and_return_if( texcoords  == NULL );

	size = sizeof( RtFloat ) * ntexcoords * 2; /* st */

	p = ( RtFloat * )ri_mem_alloc( ( long )size );

	memcpy( p, texcoords, size );

	geom->texcoords  = p;
	geom->ntexcoords = ntexcoords;
}

/*
 * Function: ri_geom_add_indices
 *
 *    Add triangle index vector to the geometry.
 *
 * Parameters:
 *
 *    geom     - the geometry to which triangle index vector is added.
 *    nindices - the number of indices in the vector *indices*.
 *    indices  - the index vector to be added.
 *
 * Returns:
 *
 *    None.
 *
 */
void
ri_geom_add_indices(
	ri_geom_t           *geom,
	unsigned int         nindices,
	const unsigned int  *indices )
{
	unsigned int *p;
	size_t        size;

	ri_log_and_return_if( nindices == 0 );
	ri_log_and_return_if( indices  == NULL );

	size = sizeof( unsigned int ) * nindices;

	p = ( unsigned int * )ri_mem_alloc( ( long )size );

	memcpy( p, indices, size );

	geom->indices  = p;
	geom->nindices = nindices;
}

/*
 * Function: ri_geom_area
 *
 *    Calculates surface area of the geometry(set of triangles).
 *
 * Parameters:
 *
 *    geom - a geometry data
 *
 * Returns:
 *
 *    The area of a geometry *geom*.
 *
 */
ri_float_t
ri_geom_area(
	ri_geom_t *geom )
{
	unsigned int i;
	unsigned int i0, i1, i2;
	ri_float_t       area;
	ri_vector_t  *v0, *v1, *v2;
	ri_vector_t  v01, v02;
	ri_vector_t  cross;

	area = 0.0;
	for (i = 0; i < geom->nindices / 3; i++) {
		i0 = geom->indices[3 * i + 0];
		i1 = geom->indices[3 * i + 1];
		i2 = geom->indices[3 * i + 2];

		v0 = &geom->positions[i0];
		v1 = &geom->positions[i1];
		v2 = &geom->positions[i2];

		ri_vector_sub( &v01, v1, v0 );
		ri_vector_sub( &v02, v2, v0 );
		ri_vector_cross3( &cross, &v01, &v02 );

		area += ri_vector_length3( &cross ) * 0.5;
	}

	return area;
}
