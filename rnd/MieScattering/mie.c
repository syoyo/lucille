/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * An implementation of
 * "Computing the scattering properties of participating media
 *  using Lorentz-Mie theory"
 * Jeppe Revall Frisvad, Niels Jorgen Christensen and Henrik Wann Jensen,
 * SIGGRAPH 2007.
 *
 * References:
 *
 *   [1] Mie scattering
 *       http://omlc.ogi.edu/software/mie/index.html
 *
 *   [2] Mie scattering in Maple script(Japanese)
 *       http://www.cybernet.co.jp/maple/example/engineering/050_Mie.html
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "mie.h"

#define MAX_M            1024
#define MIN_WAVELENGTH   375
#define MAX_WAVELENGTH   775
#define WAVELENGTH_STEP  25
#define THETA_RESOLUTION 1024

/* Im(eta) */

double water_im_refractive_indices[] = {
    3.30e-10,
    2.11e-10,
    1.62e-10,
    3.30e-10,
    4.31e-10,
    8.12e-10,
    1.74e-9,
    2.47e-9,
    3.53e-9,
    1.06e-8,
    1.41e-8,
    1.76e-8,
    2.41e-8,
    3.48e-8,
    8.59e-8,
    1.47e-7,
    1.49e-7 };
    
double fat_im_refractive_indices[] = {
    4.0e-6,
    6.4e-6,
    8.6e-6,
    1.1e-5,
    1.1e-5,
    1.0e-5,
    4.7e-6,
    4.6e-6,
    4.7e-6,
    4.9e-6,
    5.0e-6,
    5.0e-6,
    5.1e-6,
    5.2e-6,
    5.2e-6,
    5.2e-6,
    5.2e-6 };

/*
 * Support library
 */

typedef struct _complex_t
{
    double re;
    double im;
} complex_t;

static complex_t cadd(complex_t a, complex_t b)
{
    complex_t ret;

    ret.re = a.re + b.re;
    ret.im = a.im + b.im;

    return ret;
}

static complex_t csub(complex_t a, complex_t b)
{
    complex_t ret;

    ret.re = a.re - b.re;
    ret.im = a.im - b.im;

    return ret;
}

static complex_t cmul(complex_t a, complex_t b)
{
    complex_t ret;

    ret.re = (a.re * b.re - a.im * b.im);
    ret.im = (a.im * b.re + a.re * b.im);

    return ret;
}

static complex_t cdiv(complex_t a, complex_t b)
{
    complex_t ret;
    double c2_d2 = b.re * b.re + b.im * b.im;

    if (fabs(c2_d2) > 1.0e-17) {
        ret.re = (a.re * b.re + a.im * b.im) / c2_d2;
        ret.im = (a.im * b.re - a.re * b.im) / c2_d2;
    } else {
        ret.re = (a.re * b.re + a.im * b.im);
        ret.im = (a.im * b.re - a.re * b.im);
    }

    return ret;
}

static double csqure(complex_t a)
{
    return a.re * a.re + a.im * a.im;
}


/*
 * First derivative of the Legendre polynominal: P_n'(x)
 * Use recurrence formula to calculate P_n'(x).
 * See [2] for more information.
 */
double
Pnd(int n, double x)
{

    double p, pminus, pplus;
    int    i;

    if (n <= 0) return 0.0;
    if (n == 1) return 1.0;

    if (x >  1.0) x =  1.0;
    if (x < -1.0) x = -1.0;
    
    pminus = 0.0;
    p      = 1.0;

    for (i = 1; i < n; i++) {
        pplus  = ((2 * i + 1) * x * p - (i + 1) * pminus) / (double)i;
        pminus = p;
        p      = pplus;
    }

    return p;
}

/*
 * Second derivative of the Legendre polynominal: P_n''(x)
 * Use recurrence formula to calculate P_n''(x).
 * See [2] for more information.
 */
double
Pndd(int n, double x)
{

    double p, pminus, pplus;
    int    m;

    if (n <= 1) return 0.0;
    if (n == 2) return 3.0;

    if (x >  1.0) x =  1.0;
    if (x < -1.0) x = -1.0;
    
    pminus = 0.0;
    p      = 3.0;

    for (m = 2; m < n; m++) {
        pplus  = ((2 * m + 1) * x * p - (m + 2) * pminus) / (double)(m - 1);
        pminus = p;
        p      = pplus;
    }

    return p;
}

/* 
 * (19) 
 *
 * M = ceil(|x| + p |x|^(1/3) + 1)
 *
 */
int
calculateM(
    double x)
{
    const double p = 4.3;

    double ax = fabs(x);       
    double M = ceil(ax + p * pow(ax, 1.0/3.0) + 1.0);

    return (int)M;
}

/*
 * (15)
 *
 * An(z) = (n+1)/z - ( (n+1)/z + An+1(z) )^-1
 *
 * (*) A is real number.
 */
void
calculateA(
    double *A,      /* [out] */
    double  z,
    int     M)
{
    int n;

    A[M] = 0.0;
    double k;

    for (n = M-1; n >= 0; n--) {  /* downward reccurence */
        k = ((n + 1.0) / z) + A[n+1];
        if (fabs(k) > 1.0e-6) {
            A[n] = (n + 1.0) / z - 1.0 / k;
        } else {
            A[n] = (n + 1.0) / z - 1.0;
        }

        printf("A[%d] = %f\n", n, A[n]);
    }

}

/*
 * (16), (17)
 *
 * Bn(z) = An(z) + i / (psi_n(z) zeta_n(z))
 *
 * (psi_n(z) zeta_n(z)) = psi_n-1(z) zeta_n-1(z) (n/z - An-1(z)) (n/z - Bn-1(z))
 * 
 * B and zeta are complex number.
 */
void
calculateB(
    complex_t *B,      /* [out] */
    double    *A,      /* [in] */
    double     z,
    int        M)
{
    int n;
    complex_t psi_zeta_n_1;
    complex_t psi_zeta_n  ;
    complex_t nz_sub_A;
    complex_t nz_sub_B;
    complex_t Ione;
    complex_t k0;
    complex_t k1;

    Ione.re = 0.0;
    Ione.im = 1.0;

    B[0].re = 0.0;                                  /* B0(z) = i    */
    B[0].im = 1.0;

    psi_zeta_n_1.re = 0.5 - 0.5 * cos(2.0 * z);     /* (1 - e^i2z) / 2 */
    psi_zeta_n_1.im =       0.5 * sin(2.0 * z);
    
    for (n = 1; n < M+1; n++) {  /* forward reccurence */

        nz_sub_A.re = (n/z - A[n-1]);
        nz_sub_A.im = 0.0;

        nz_sub_B.re = (n/z - B[n-1].re);
        nz_sub_B.im = -B[n-1].im;

        /* (n/z - A[n-1]) * (n/z - B[n-1])  */
        k0 = cmul(nz_sub_A, nz_sub_B);  

        /* psi_zeta_n = psi_zeta_n_1 * (n/z - A[n-1]) * (n/z - B[n-1]) */
        psi_zeta_n = cmul(psi_zeta_n_1, k0);

        /* B[n] = A[n] + i / (psi_zeta_n) */
        k1 = cdiv(Ione, psi_zeta_n);

        B[n].re = A[n] + k1.re;
        B[n].im =        k1.im;

        psi_zeta_n_1 = psi_zeta_n;
    }

}

/*
 * (18)
 *
 * (psi_n(z) / zeta_n(z)) = ( (psi_n-1(z) / zeta_n-1(z)) )
 *                        * ( (Bn(z) + n/z) / (An(z) + n/z) )
 * 
 * B and zeta are complex number.
 */
void
calculate_psi_over_zeta(
    complex_t *psi_over_zeta,   /* [out]    */
    double    *A,               /* [in] */
    complex_t *B,               /* [in] */
    double     z,
    int        M)
{
    int n;
    double    A_nz;
    complex_t psi_over_zeta_n_1;
    complex_t psi_over_zeta_n  ;
    complex_t B_nz_div_A_nz;
    complex_t k0;

    psi_over_zeta_n_1.re = 0.5 - 0.5 * cos(-2.0 * z);     /* (1 - e^-i2z) / 2 */
    psi_over_zeta_n_1.im =       0.5 * sin(-2.0 * z);
    
    for (n = 1; n <= M; n++) {  /* forward reccurence */

    
        /* (Bn(z) + n/z) */
        k0.re = B[n].re + n / z;
        k0.im = B[n].im;

            
        /* (Bn(z) + n/z) / (An(z) + n/z) )  */
        A_nz = A[n] + n / z;
        B_nz_div_A_nz.re = k0.re / A_nz;
        B_nz_div_A_nz.im = k0.im / A_nz;

        /* (psi_n(z) / zeta_n(z)) = ( (psi_n-1(z) / zeta_n-1(z)) )
         *                        * ( (Bn(z) + n/z) / (An(z) + n/z) )
         */
        psi_over_zeta_n = cmul(psi_over_zeta_n_1, B_nz_div_A_nz);

        psi_over_zeta[n]  = psi_over_zeta_n;
        psi_over_zeta_n_1 = psi_over_zeta_n;

        //printf("psi_over_zeta[%d] = %f\n", n, psi_over_zeta[n]);
    }

}

/*
 * (12)
 *
 * a_n = (psi_n(x) / zeta_n(x))
 *     * ( (eta_med * An(y) - eta * An(x)) / (eta_med * An(y) - eta * Bn(x)))
 * 
 * a_n is complex number.
 */
void
calculate_a(
    complex_t *a,               /* [out]    */
    complex_t *psi_over_zeta_x, /* [in]     */
    double    *Ax,              /* [in]     */
    double    *Ay,              /* [in]     */
    complex_t *Bx,              /* [in]     */
    double     eta_med,
    double     eta,
    int        M)
{
    int        n;
    complex_t  k0;
    complex_t  k1;
    complex_t  k2;

    for (n = 1; n <= M; n++) { 
    
        /* (eta_med * An(y) - eta * An(x)) */
        k0.re = eta_med * Ay[n] - eta * Ax[n];
        k0.im = 0.0;

        /* (eta_med * An(y) - eta * Bn(x)) */
        k1.re = eta_med * Ay[n] - eta * Bx[n].re;
        k1.im =                 - eta * Bx[n].im; 
 
        /* ((eta_med * An(y) - eta * An(x)) / (eta_med * An(y) - eta * Bn(x))) */
        k2 = cdiv(k0, k1);
        
        
        /* a_n = (psi_n(x) / zeta_n(x))
         *     * ( (eta_med * An(y) - eta * An(x)) / 
         *         (eta_med * An(y) - eta * Bn(x)) )
         */
        a[n] = cmul(psi_over_zeta_x[n], k2);

    }

}

/*
 * (13)
 *
 * b_n = (psi_n(x) / zeta_n(x))
 *     * ( (eta * An(y) - eta_med * An(x)) / (eta * An(y) - eta_med * Bn(x)))
 * 
 * b_n is complex number.
 */
void
calculate_b(
    complex_t *b,               /* [out]    */
    complex_t *psi_over_zeta_x, /* [in]     */
    double    *Ax,              /* [in]     */
    double    *Ay,              /* [in]     */
    complex_t *Bx,              /* [in]     */
    double     eta_med,
    double     eta,
    int        M)
{
    int        n;
    complex_t  k0;
    complex_t  k1;
    complex_t  k2;

    for (n = 1; n <= M; n++) { 
    
        /* (eta * An(y) - eta_med * An(x)) */
        k0.re = eta * Ay[n] - eta_med * Ax[n];
        k0.im = 0.0;

        /* (eta * An(y) - eta_med * Bn(x)) */
        k1.re = eta * Ay[n] - eta_med * Bx[n].re;
        k1.im =             - eta_med * Bx[n].im; 
 
        /* ((eta * An(y) - eta_med * An(x)) / (eta * An(y) - eta_med * Bn(x))) */
        k2 = cdiv(k0, k1);
        
        
        /* a_n = (psi_n(x) / zeta_n(x))
         *     * ( (eta * An(y) - eta_med * An(x)) / 
         *         (eta * An(y) - eta_med * Bn(x)) )
         */
        b[n] = cmul(psi_over_zeta_x[n], k2);

    }

}
    

/*
 * (2), (3)
 */
static void
calculateS(
    complex_t   *S1_out,
    complex_t   *S2_out,
    double       theta,
    complex_t   *a,
    complex_t   *b,
    int          M)
{
    int n;

    /*
     *              M   2n+1
     * S1(theta) = sum ------ ( an phi_n(cos(theta)) + bn tau_n(cos(theta)) )
     *             n=1 n(n+1)
     *
     *              M   2n+1
     * S2(theta) = sum ------ ( bn phi_n(cos(theta)) + an tau_n(cos(theta)) )
     *             n=1 n(n+1)
     *
     */

    double phi_n_cos_theta;
    double tau_n_cos_theta;

    double cos_theta;

    cos_theta = cos(theta);

    complex_t S1, S2;
    complex_t a_phi, b_tau, a_tau, b_phi;
    double    k;

    S1.re = 0.0; S1.im = 0.0;
    S2.re = 0.0; S2.im = 0.0;

    for (n = 1; n < M; n++) {

        phi_n_cos_theta = Pnd(n, cos_theta);
        tau_n_cos_theta = cos_theta * Pnd(n, cos_theta)
                        - (1 - cos_theta * cos_theta) * Pndd(n, cos_theta);

        k = (2 * n + 1) / (double)(n * (n + 1));

        a_phi.re = a[n].re * phi_n_cos_theta;
        a_phi.im = a[n].im * phi_n_cos_theta;

        a_tau.re = a[n].re * tau_n_cos_theta;
        a_tau.im = a[n].im * tau_n_cos_theta;

        b_phi.re = b[n].re * phi_n_cos_theta;
        b_phi.im = b[n].im * phi_n_cos_theta;

        b_tau.re = b[n].re * tau_n_cos_theta;
        b_tau.im = b[n].im * tau_n_cos_theta;

        S1.re += k * (a_phi.re + b_tau.re);
        S1.im += k * (a_phi.im + b_tau.im);

        S2.re += k * (b_phi.re + a_tau.re);
        S2.im += k * (b_phi.im + a_tau.im);

    }

    (*S1_out) = S1;
    (*S2_out) = S2;
}

/* (22) */
static double
calculateCt(
    int        M,
    complex_t *a,
    complex_t *b,
    double     lambda0,
    double     eta_med)
{
    int n;

    double Ct = 0.0;
    double k0, k1;

    /*
     * Ct = (lambda0^2 / 2 pi) sum_n^M (2n + 1) Re((an + bn) / eta_med^2)
     */
    for (n = 1; n <= M; n++) {
        Ct += (2.0 * n + 1.0) * (a[n].re + b[n].re) / (eta_med * eta_med);
    }

    Ct *= lambda0 * lambda0 / (2.0 * M_PI);

    return Ct;  // [nm^2]
}

/* (23) */
static double
calculateCs(
    int        M,
    complex_t *a,
    complex_t *b,
    double     lambda0,
    double     r,
    double     eta_med,
    double    *eta_med_im)
{
    int n;
    int index;

    double Cs = 0.0;
    double k0, k1;
    double gamma;
    double alpha;

    index = (int)((lambda0 - MIN_WAVELENGTH) / WAVELENGTH_STEP);

    /* (24) */
    alpha = 4.0 * M_PI *  r * eta_med_im[index] / lambda0;

    if (alpha > 1.0e-6) {
        gamma = (2.0 * (1.0 + (alpha - 1) * exp(alpha))) / (alpha * alpha);
    } else {
        gamma = 1.0;
    }

    for (n = 1; n <= M; n++) {
        k0 = csqure(a[n]) + csqure(b[n]);
        Cs += (2.0 * n + 1.0) * k0;
    }

    Cs *= (lambda0 * lambda0 * exp(-alpha)) / (2.0 * M_PI * gamma * eta_med * eta_med);

    return Cs;  // [nm^2]
}

/*
 * (26)
 */
static double
calculateg(
    int        M,
    complex_t *a,
    complex_t *b)
{
    int n;

    complex_t k0, k1;
    complex_t a_n1_cg;
    complex_t b_n1_cg;
    complex_t b_n_cg;
    double numerator = 0.0;
    double denom = 0.0;
    

    for (n = 1; n < M; n++) {
        a_n1_cg.re =  a[n+1].re;
        a_n1_cg.im = -a[n+1].im;
        b_n_cg.re  =  b[n].re;
        b_n_cg.im  = -b[n].im;
        b_n1_cg.re =  b[n+1].re;
        b_n1_cg.im = -b[n+1].im;

        k0 = cadd(cmul(a[n], a_n1_cg), cmul(b[n], b_n1_cg));
        k1 = cmul(a[n], b_n_cg);

        numerator += ((n * (n + 2.0)) / (n + 1.0)) * k0.re + ((2.0 * n + 1.0) / (n * (n + 1.0))) * k1.re;
        
        denom += (2.0 * n + 1.0) * (csqure(a[n]) + csqure(b[n]));
    }
    denom *= 0.5;

    return numerator / denom;
} 

/* Appendix A */
static double
particle_volume_frequency(double r, double mean, double sigma)
{
    double alpha;
    double beta;

    alpha = log(mean) - 0.5 * log(sigma * sigma / (mean * mean) + 1.0);
    beta  = sqrt(log(sigma * sigma / (mean * mean) + 1.0));

    /* r^3 N(r) = 1/(r beta sqrt(2 pi)) e^(-0.5 * ((ln(r) - alpha)/beta)^2) */
    
    double k0 = (log(r) - alpha) / beta;
    double k1 = 1.0 / (r * beta * sqrt(2.0 * M_PI));

    return k1 * exp(-0.5 * k0 * k0);
}

void
milk()
{
    /* wt.-% of the fat. */
    double wf        = 0.0;                 // [wt.-%] [0.0, 10.0?]

    double eta_fat   = 1.46;                // Re(eta_fat)
    double eta_cas   = 1.503;               // Re(eta_cas)

    double r_min_fat = 0.005 * 1000.0;      // [nm]
    double r_max_fat = 10.0 * 1000.0;       // [nm] 

    double r_min_cas = 0.0;                 // [nm]
    double r_max_cas = 150.0;               // [nm]

    double c_v_fat   = 0.6;                 // usually [0.4, 12.0]


    /* 
     * r_vs_fat: the mean of the volume-to-surface area equivalent sphere radii
     *           of the fat.
     */
    double r_vs_fat;                        
    double r_43_fat;                        // [um]

    /* (37) */
    if (wf < 2.0) {
        r_43_fat = -0.2528 * wf * wf + 1.419 * wf;
    } else {
        r_43_fat = 1.456 * pow(wf, 0.36);
    }

    /* (38) */
    r_vs_fat = r_43_fat / (c_v_fat * c_v_fat + 1.0);
}

static double
calculatePhase(
    complex_t  S1,
    complex_t  S2,
    int        M,
    complex_t *a,
    complex_t *b)
{
    /*
     * Compute phase function using eq (25)
     *
     *                    |S1(theta)|^2 + |S2(theta)|^2
     * p(theta) = ------------------------------------------
     *            4 pi sum_n^M ( (2n + 1) ( |an|^2 + |bn|^2 )
     *
     */

    int n;
    double k0, k1;

    k0 = csqure(S1) + csqure(S2);
    k1 = 0.0;

    for (n = 1; n <= M; n++) {
        k1 += (2 * n + 1) * (csqure(a[n]) + csqure(b[n]));
    }

    return k0 / (4.0 * M_PI * k1);
}

void
milk_phase(
    double *phase,                          /* [out]                        */
    int     theta_resolution,
    double  lambda0,                        /* [375, 775]                   */
    double  r,
    double  eta,
    double  eta_med)
{
    double x, y;

    double    Ax[MAX_M];                    /* An(x)                        */
    double    Ay[MAX_M];                    /* An(y)                        */
    complex_t Bx[MAX_M];                    /* Bn(x)                        */
    complex_t By[MAX_M];                    /* Bn(y)                        */

    complex_t a[MAX_M];                     /* a_n                          */
    complex_t b[MAX_M];                     /* b_n                          */

    complex_t psi_over_zeta_x[MAX_M];       /* psi_n(x) / zeta_n(x)         */
    complex_t psi_over_zeta_y[MAX_M];       /* psi_n(x) / zeta_n(x)         */

    int M;

    /* 
     * For a given wavelength and particle size,
     * compute Lorenz-Mie coefficient an and bn by evaluating
     * eq 19, 15, 20, 16, 17, 18, 12 and 13.
     */
    
    /* (11) size paremeter x and y. */
    x = 2.0 * M_PI * r * eta_med / lambda0;
    y = 2.0 * M_PI * r * eta / lambda0;

    printf("Input: wavelength    = %f [nm]\n", lambda0);
    printf("       particle size = %f [nm]\n", r);
    printf("       eta           = %f \n", eta);
    printf("       eta_med       = %f \n", eta_med);

    printf("\n\n");

    printf("  x = %f \n", x);
    printf("  y = %f \n", y);

    /* (19) */
    M = calculateM(x);
    printf("M = %d\n", M);
    assert(M < (MAX_M - 1));

    /* (15) */
    calculateA(Ax, x, M);  
    calculateA(Ay, y, M); 
    
    /* (16), (17) */
    calculateB(Bx, Ax, x, M);  
    calculateB(By, Ay, y, M);  

    /* (18) */
    calculate_psi_over_zeta(psi_over_zeta_x, Ax, Bx, x, M);  
    calculate_psi_over_zeta(psi_over_zeta_y, Ay, By, y, M);  // not needed?

    /* (12) */
    calculate_a(a, psi_over_zeta_x, Ax, Ay, Bx, eta_med, eta, M);
    calculate_b(b, psi_over_zeta_x, Ax, Ay, Bx, eta_med, eta, M);


    int i;
    double theta;
    double k0, k1;
    double Ct, Cs;

    complex_t S1;
    complex_t S2;

#if 0
    for (i = 1; i < M; i++) {
        printf("Ax[%d] = %f\n", i, Ax[i]);
        printf("Bx[%d] = %f\n", i, Bx[i]);
        printf("Ay[%d] = %f\n", i, Ay[i]);
        printf("By[%d] = %f\n", i, By[i]);
        printf("psi_over_zeta[%d] = %f\n", i, psi_over_zeta_x[i]);
        printf("a[%d] = %f\n", i, a[i]);
        printf("b[%d] = %f\n", i, b[i]);
    }
#endif

    for (i = 0; i < theta_resolution; i++) {
        theta = (i / (double)(theta_resolution)) * 2.0 * M_PI;

        /* S1(theta), S2(theta) */
        calculateS(&S1, &S2, theta, a, b, M);

        phase[i] = calculatePhase(S1, S2, M, a, b);

    }

    Ct = calculateCt(M, a, b, lambda0, eta_med);
    printf("  Ct = %f\n", Ct);
    Cs = calculateCs(M, a, b, lambda0, r, eta_med, water_im_refractive_indices);
    printf("  Cs = %f\n", Cs);
}

/* ---------------------------------------------------------------------------
 * 
 * Public functions
 *
 * ------------------------------------------------------------------------ */

void
ri_mie_compute_phase_function_milk(
    ri_mie_phase_function_t *phase,
    double                   wavelength,
    double                   particle_size,
    double                   fat_content)
{

    double r = particle_size;

    double eta_fat   = 1.46;
    double eta_water = 1.00;

    milk_phase(phase->phase, 1024, wavelength, particle_size, eta_fat, eta_water);
}



