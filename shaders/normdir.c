#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "shader.h"

DLLEXPORT void
normdir_initparam(ri_parameter_t *param)
{


}

DLLEXPORT void
normdir(ri_output_t *output, ri_status_t *status, ri_parameter_t *param)
{

	ri_vector_t tmp0;
	float tmp1;
	float tmp2;
	ri_vector_t tmp3;
	ri_vector_t tmp4;


	if ( ri_vector_dot3(&status->input.I , &status->input.N ) < 0.000000 ) {
		ri_vector_set4(&tmp3, 0.000000, 1.000000, 0.000000, 1.0);
		ri_vector_copy(&output->Ci, &tmp3);
	} else {
		ri_vector_set4(&tmp4, 1.000000, 0.000000, 0.000000, 1.0);
		ri_vector_copy(&output->Ci, &tmp4);
	}
	ri_vector_copy(&output->Oi, &status->input.Os);
}
