#ifndef LUCILLE_SPIRAL_H
#define LUCILLE_SPIRAL_H

#ifdef __cplusplus
extern "C" {
#endif


extern void spiral_setup(unsigned int wpix, unsigned int hpix,
		         unsigned int bucket_size);
extern int  spiral_get_nextlocation(unsigned int *xp, unsigned int *yp);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif	/* LUCILLE_SPIRAL_H */
