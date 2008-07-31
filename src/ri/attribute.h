/*
 * The RenderMan Graphics State Attributes.
 *
 * $Id: attribute.h,v 1.2 2004/01/25 16:20:44 syoyo Exp $
 */

#ifndef ATTRIBUTE_H
#define ATTRIBUTE_H


#include "ri.h"
#include "material.h"
#include "shader.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_attribute_t
{
    /* The reflective color of the object. */
    ri_vector_t    color;

    /* The opacity of the objject. */
    ri_vector_t    opacity;

    /*
     * The texture coordinates(s, t) at the 4 corners of parametric
     * primitive.
     */
    RtFloat    texture_coordinates[4][2];

    /*
     * A list of light source shaders that illuminate subsequent
     * primitive.
     */
    /* Not implemented yet. */
    /* GList    *light_sources; */

    /* A shader contrilling the surface shading model. */
    /* Not implemented yet. */
    RtToken surface;

    /* Surface shader. */
    ri_shader_t *shader;

    /*
     * A volume shader that specifies how the color of light is changed
     * as it travels from a visible surface to the eye.
     */
    /* Not implemented yet. */
    /* RtToken    atomosphere; */

    /*
     * A volume shader that specifies how the color of light is changed
     * as it travels a volume in space.
     */
    /* Not implemented yet. */
    /* RtToken    interior_voume, exterior_volume; */

    /* Minimum rate of surface shading. */
    RtFloat    effective_shading_rate;

    /* How the results of shading samples are interpolated. */
    RtToken    shading_interpolation;

    /* 
     * A flag indicating the surfaces of the subsequent primitives are
     * opaque to the rendering program, but transparent on output.    
     */
    RtBoolean    matte_surface_flag;

    /* material parameter */
    ri_material_t *material;

    /*
     * two-sided or one-sided.
     */
    int             sides;

} ri_attribute_t;

extern ri_attribute_t *ri_attribute_new ();
extern void            ri_attribute_copy(ri_attribute_t *dst,
                     const ri_attribute_t *src);
extern void           ri_attribute_free(ri_attribute_t *attribute);

#ifdef __cplusplus
}
#endif

#endif
