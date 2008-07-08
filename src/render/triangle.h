#ifndef LUCILLE_TRIANGLE_H
#define LUCILLE_TRIANGLE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "geom.h"

typedef struct _ri_triangle_t {

    ri_vector_t v[3];

    ri_geom_t  *geom;
    uint32_t    index;

} ri_triangle_t;

extern int ri_triangle_isect(       uint32_t      *tid_inout,
                                    ri_float_t    *t_inout,
                                    ri_float_t    *u_inout,
                                    ri_float_t    *v_inout,
                              const ri_triangle_t *triangle,
                                    ri_vector_t    rayorg,
                                    ri_vector_t    raydir,
                                    uint32_t       tid);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_TRIANGLE_H */
