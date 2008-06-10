#include "geometric.h"


ri_float_t
ri_area(
    const ri_vector_t v0,
    const ri_vector_t v1,
    const ri_vector_t v2)
{
    ri_vector_t v01, v02;
    ri_vector_t cross;

    ri_vector_sub(v01, v1, v0);
    ri_vector_sub(v02, v2, v0);
    ri_vector_cross(cross, v01, v02);

    return (ri_vector_length(cross) * 0.5);
}

void
ri_normal_of_triangle(
    ri_vector_t       n,
    const ri_vector_t v0,
    const ri_vector_t v1,
    const ri_vector_t v2)
{
    ri_vector_t v01, v02;

    ri_vector_sub(v01, v1, v0);
    ri_vector_sub(v02, v2, v0);
    ri_vector_cross(n, v01, v02);
    ri_vector_normalize(n);
}


/*
 * n = (1 - u - v) n0 + u n1 + v n2
 *
 */
void
ri_lerp_vector(
    ri_vector_t       n_out,
    const ri_vector_t n0,
    const ri_vector_t n1,
    const ri_vector_t n2,
    ri_float_t        u,
    ri_float_t        v )
{

    ri_vector_t ns0, ns1, ns2;


    ri_vector_copy( ns0, n0 );
    ri_vector_scale( ns0, ns0, ( 1.0 - u - v ) );
    ri_vector_copy( ns1, n1 );
    ri_vector_scale( ns1, ns1, u );
    ri_vector_copy( ns2, n2 );
    ri_vector_scale( ns2, ns2, v );

    ri_vector_add( n_out, ns0, ns1 );
    ri_vector_add( n_out, n_out, ns2 );
}
