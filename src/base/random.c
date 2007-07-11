/* A C-program for MT19937: Real number version([0,1)-interval) (1998/4/6) */
/*   genrand() generates one pseudorandom real number (double) */
/* which is uniformly distributed on [0,1)-interval, for each  */
/* call. sgenrand(seed) set initial values to the working area */
/* of 624 words. Before genrand(), sgenrand(seed) must be      */
/* called once. (seed is any 32-bit integer except for 0).     */
/* Integer generator is obtained by modifying two lines.       */
/*   Coded by Takuji Nishimura, considering the suggestions by */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.           */

/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public     */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later         */
/* version.                                                        */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General      */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */ 
/* 02111-1307  USA                                                 */

/* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.       */
/* When you use this, send an email to: matumoto@math.keio.ac.jp   */
/* with an appropriate reference to your work.                     */

/* REFERENCE                                                       */
/* M. Matsumoto and T. Nishimura,                                  */
/* "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform  */
/* Pseudo-Random Number Generator",                                */
/* ACM Transactions on Modeling and Computer Simulation,           */
/* Vol. 8, No. 1, January 1998, pp 3--30.                          */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include<stdio.h>

#include "random.h"
#include "thread.h"

/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */   
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

static unsigned long mt2[RI_MAX_THREADS][N]; /* the array for the state vector  */
static int mti2[RI_MAX_THREADS]; 
static int initialized[RI_MAX_THREADS]; 

static ri_thread_once_t once = RI_THREAD_ONCE_INIT;
static ri_mutex_t mutex;

static void rand_init(void);

/* initializing the array with a NONZERO seed */
void
#if 0
sgenrand(seed)
    unsigned long seed;	
#endif
seedMT(seed)
    unsigned long seed;	
{
    /* setting initial seeds to mt[N] using         */
    /* the generator Line 25 of Table 1 in          */
    /* [KNUTH 1981, The Art of Computer Programming */
    /*    Vol. 2 (2nd Ed.), pp102]                  */
    mt[0]= seed & 0xffffffff;
    for (mti=1; mti<N; mti++)
        mt[mti] = (69069 * mt[mti-1]) & 0xffffffff;
}

/* initializing the array with a NONZERO seed */
void
#if 0
sgenrand(seed)
    unsigned long seed;	
#endif
seedMT2(seed, tid)
    unsigned long seed;
    int           tid;	
{
    /* setting initial seeds to mt[N] using         */
    /* the generator Line 25 of Table 1 in          */
    /* [KNUTH 1981, The Art of Computer Programming */
    /*    Vol. 2 (2nd Ed.), pp102]                  */
    mt2[tid][0]= seed & 0xffffffff;
    for (mti2[tid]=1; mti2[tid]<N; mti2[tid]++)
        mt2[tid][mti2[tid]] = (69069 * mt2[tid][mti2[tid]-1]) & 0xffffffff;

    initialized[tid] = 1;
}

#if 0
double /* generating reals */
/* unsigned long */ /* for integer generation */
randomMT()
{
    unsigned long y;
    static unsigned long mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if sgenrand() has not been called, */
            //sgenrand(4357); /* a default initial seed is used   */
            seedMT(4357); /* a default initial seed is used   */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        mti = 0;
    }
  
    y = mt[mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);

    return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
    /* return y; */ /* for integer generation */
}
#else
/* thread-safe version */

double /* generating reals */
/* unsigned long */ /* for integer generation */
randomMT()
{
    unsigned long y;
    static unsigned long mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    ri_thread_once(&once, rand_init);

    ri_mutex_lock(&mutex);

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if sgenrand() has not been called, */
            //sgenrand(4357); /* a default initial seed is used   */
            seedMT(4357); /* a default initial seed is used   */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        mti = 0;
    }
  
    y = mt[mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);

    ri_mutex_unlock(&mutex);

    return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
    /* return y; */ /* for integer generation */
}
#endif


/* faster thread-safe version without locking */

double /* generating reals */
/* unsigned long */ /* for integer generation */
randomMT2(int thread_id)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    int tid = thread_id;

    if (!initialized[tid])   /* if sgenrand() has not been called, */
        //sgenrand(4357); /* a default initial seed is used   */
        seedMT2(4357, tid); /* a default initial seed is used   */

    if (mti2[tid] >= N) { /* generate N words at one time */
        int kk;
        for (kk=0;kk<N-M;kk++) {
            y = (mt2[tid][kk]&UPPER_MASK)|(mt2[tid][kk+1]&LOWER_MASK);
            mt2[tid][kk] = mt2[tid][kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt2[tid][kk]&UPPER_MASK)|(mt2[tid][kk+1]&LOWER_MASK);
            mt2[tid][kk] = mt2[tid][kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt2[tid][N-1]&UPPER_MASK)|(mt2[tid][0]&LOWER_MASK);
        mt2[tid][N-1] = mt2[tid][M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        mti2[tid] = 0;
    }
  
    y = mt2[tid][mti2[tid]++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);

    return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
    /* return y; */ /* for integer generation */
}


void
random_uniform_vector(ri_vector_t *dst)
{
	//ri_vector_t vec;
	RtVector v;
	RtFloat  len;

	do {
		v[0] = (RtFloat)(2.0 * randomMT() - 1.0); /* [-1.0, 1.0) */
		v[1] = (RtFloat)(2.0 * randomMT() - 1.0); 
		v[2] = (RtFloat)(2.0 * randomMT() - 1.0);
		len = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
	} while (len > 1.0 || len == 0.0); 

	ri_vector_set_rman(dst, v);
}

static void
rand_init(void)
{
	ri_mutex_init(&mutex);	
}

#if 0
/* this main() outputs first 1000 generated numbers  */
main()
{ 
    int j;

    //sgenrand(4357); /* any nonzero integer can be used as a seed */
    seedMT(4357); /* any nonzero integer can be used as a seed */
    for (j=0; j<1000; j++) {
        printf("%10.8f ", genrand());
        if (j%8==7) printf("\n");
    }
    printf("\n");
}
#endif
