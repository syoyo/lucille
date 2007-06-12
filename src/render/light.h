/*
 * light source object.
 *
 * $Id: light.h,v 1.4 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef LIGHT_H
#define LIGHT_H

#include "vector.h"
#include "geom.h"
#include "texture.h"
#include "sunsky.h"

#ifdef __cplusplus
extern "C" {
#endif

#define IBL_SAMPLING_COSWEIGHT  0
#define IBL_SAMPLING_IMPORTANCE 1
#define IBL_SAMPLING_STRATIFIED 2
#define IBL_SAMPLING_STRUCTURED 3
#define IBL_SAMPLING_BRUTEFORCE 4 
#define IBL_SMAPLING_EIHDRI     5

#define LIGHTTYPE_NONE          0
#define LIGHTTYPE_SUNLIGHT      1
#define LIGHTTYPE_SUNSKY        2
#define LIGHTTYPE_DIRECTIONAL   3
#define LIGHTTYPE_POINTLIGHT    4 
#define LIGHTTYPE_IBL           5
#define LIGHTTYPE_DOME          6

typedef struct _ri_light_t
{
	int          type;		/* light type		*/
	ri_vector_t  pos;		/* light position	*/
	double       intensity;		/* light intensity	*/
	ri_vector_t  col;		/* light color		*/
	ri_geom_t   *geom;		/* for area light	*/
	//int          directional;	/* directional light?	*/
	ri_vector_t  direction;		/* light direction	*/

	ri_texture_t *texture;		/* for IBL		*/
	//int           ibl;		/* Is light IBL?	*/

	char         *sisfile;		/* for structured importance
					 * sampling. 		*/
	char         *eihdrifile;	/* for EIHDRI smpling.	*/
	int           iblsampler;	/* IBL sampling method	*/
	//int           domelight;	/* domelight?		*/
	//int           sunskylight;	/* sunsky light?	*/

	ri_sunsky_t  *sunsky;		/* for sunsky		*/
} ri_light_t;

extern ri_light_t *ri_light_new();
extern void        ri_light_free(ri_light_t *light);
extern void        ri_light_attach_geom(ri_light_t *light, ri_geom_t *geom);

/* randomly sample position onto light geometry */
extern void        ri_light_sample_pos_and_normal(ri_light_t *light,
				                  ri_vector_t *pos,
				                  ri_vector_t *normal);

/* quasi-randomly sample position onto light geometry */
extern void        ri_light_sample_pos_and_normal_qmc(ri_light_t *light,
				                      ri_vector_t *pos,
				                      ri_vector_t *normal,
						      int d, int i, int **perm);

/* randomly sample position and direction onto light geometry */
extern void        ri_light_sample_pos_and_dir(ri_light_t *light,
				               ri_vector_t *pos,
				               ri_vector_t *dir);

/* quasi-randomly sample position and direction onto light geometry */
extern void        ri_light_sample_pos_and_dir_qmc(ri_light_t *light,
				                   ri_vector_t *pos,
				                   ri_vector_t *dir,
						   int d, int i, int **perm);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

