#ifndef HILBERT2D_H
#define HILBERT2D_H

#ifdef __cplusplus
extern "C" {
#endif


extern void hil_xy_from_s(unsigned int s, int n,
			  unsigned int *xp, unsigned int *yp);
extern void hil_setup(unsigned int wpix, unsigned int hpix,
		      unsigned int bucket_size);
extern int  hil_get_nextlocation(unsigned int *xp, unsigned int *yp);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
