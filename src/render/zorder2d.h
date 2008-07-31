#ifndef ZORDER2D_H
#define ZORDER2D_H

#ifdef __cplusplus
extern "C" {
#endif


extern void zorder_xy_from_s(unsigned int s, int n,
			     unsigned int *xp, unsigned int *yp);
extern void zorder_setup(unsigned int wpix, unsigned int hpix,
		         unsigned int bucket_size);
extern int  zorder_get_nextlocation(unsigned int *xp, unsigned int *yp);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
