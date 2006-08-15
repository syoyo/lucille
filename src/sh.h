#ifndef SH_H
#define SH_H

#ifdef __cplusplus
extern "C" {
#endif

/* real spherical harmonics in Cartesian coordiante up to l + 4. */
extern double shY(float sh[25], float x, float y, float z);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
