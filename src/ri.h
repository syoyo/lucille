/*
 * $Id: ri.h,v 1.2 2004/01/30 04:43:57 syoyo Exp $
 */

/*
 *		RenderMan Interface Standard Include File
 *			(for ANSI Standard C)
 */

#ifndef RI_H
#define RI_H

/* Definition of Abstract Types used in RI */
typedef short	RtBoolean;
typedef int	RtInt;
typedef float	RtFloat;

typedef char	*RtToken;

typedef RtFloat	RtColor[3];
typedef RtFloat	RtPoint[3];
typedef RtFloat	RtVector[3];
typedef RtFloat	RtNormal[3];
typedef RtFloat	RtHpoint[4];
typedef RtFloat	RtMatrix[4][4];
typedef RtFloat	RtBasis[4][4];
typedef RtFloat	RtBound[6];
typedef char	*RtString;

typedef void	*RtPointer;
#define RtVoid	 void

typedef RtFloat (*RtFilterFunc)(RtFloat, RtFloat, RtFloat, RtFloat);
typedef RtVoid  (*RtErrorHandler)(RtInt, RtInt, char *);

typedef RtVoid (*RtProcSubdivFunc)(RtPointer, RtFloat);
typedef RtVoid (*RtProcFreeFunc)(RtPointer);
typedef RtVoid (*RtArchiveCallback)(RtToken, char *, ...);

typedef RtPointer RtObjectHandle;
typedef RtPointer RtLightHandle;
typedef RtPointer RtContextHandle;

/* Extern Declarations for Predefined RI Data Structures */
#define RI_FALSE    0
#define RI_TRUE     (! RI_FALSE)
#define RI_INFINITY 1.0e38
#define RI_EPSILON  1.0e-10
#define RI_NULL     ((RtToken)0)

extern RtToken  RI_FRAMEBUFFER, RI_FILE;
extern RtToken  RI_RGB, RI_RGBA, RI_RGBZ, RI_A, RI_Z, RI_AZ;
extern RtToken  RI_P, RI_N, RI_ST, RI_CS;
extern RtToken  RI_PERSPECTIVE, RI_ORTHOGRAPHIC;
extern RtToken  RI_LH, RI_RH;
/* ... */

extern RtInt    RiLastError;

/* Declarations of All the RenderMan Interface Subroutines */

extern RtFloat RiGaussianFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiBoxFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiTriangleFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiCatmullRomFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiSincFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth);

/* ... */

extern RtContextHandle RiGeiContext(void);
extern RtVoid RiContext(RtContextHandle);

extern RtToken RiDeclare(char *name, char *declaration);

extern RtVoid RiBegin(RtToken name);
extern RtVoid RiEnd(void);
extern RtVoid RiFrameBegin(RtInt frame);
extern RtVoid RiFrameEnd(void);
extern RtVoid RiWorldBegin(void);
extern RtVoid RiWorldEnd(void);

extern RtVoid RiFormat(RtInt xres, RtInt yres, RtFloat aspect);
extern RtVoid RiFrameAspectRatio(RtFloat aspect);
extern RtVoid RiScreenWindow(RtFloat left, RtFloat right,
			     RtFloat bot, RtFloat top);
extern RtVoid RiCropWindow(RtFloat xmin, RtFloat xmax, RtFloat ymin, RtFloat ymax); 
extern RtVoid RiProjection(RtToken name, ...);
extern RtVoid RiProjectionV(RtToken name,
			    RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiClipping(RtFloat hither, RtFloat yon);
extern RtVoid RiClippingPlane(RtFloat x, RtFloat y, RtFloat z,
                              RtFloat nx, RtFloat ny, RtFloat nz);
extern RtVoid RiDepthOfField(RtFloat fstop,
			     RtFloat focallength,
			     RtFloat focaldistance);
extern RtVoid RiShutter(RtFloat min, RtFloat max);

extern RtVoid RiQuantize(RtToken type, RtInt one, RtInt min, RtInt max, RtFloat ampl);
extern RtVoid RiShadingInterpolation(RtToken type);
extern RtVoid RiPixelFilter(RtFilterFunc filterfunc, RtFloat xwidth, RtFloat ywidth);
extern RtVoid RiExposure(RtFloat gain, RtFloat gamma);
extern RtVoid RiDisplay(char *name, RtToken type, RtToken mode, ...);
extern RtVoid RiDisplayV(char *name, RtToken type, RtToken mode,
			 RtInt n, RtToken tokens[], RtPointer params[]);

extern RtVoid RiOption(RtToken name, ...);
extern RtVoid RiOptionV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[]);

extern RtVoid RiShadingRate(RtFloat size);

/* transformation */
extern RtVoid RiIdentity(void);
extern RtVoid RiTransform(RtMatrix transform);
extern RtVoid RiConcatTransform(RtMatrix transform);
extern RtVoid RiPerspective(RtFloat fov);
extern RtVoid RiTranslate(RtFloat dx, RtFloat dy, RtFloat dz);
extern RtVoid RiRotate(RtFloat angle, RtFloat dx, RtFloat dy, RtFloat dz);
extern RtVoid RiScale(RtFloat sx, RtFloat sy, RtFloat sz);
extern RtVoid RiCoordinateSystem(RtToken space);
/* ... */

extern RtVoid RiPolygon (RtInt nverts, ...);
extern RtVoid RiPolygonV(RtInt nverts, RtInt n,
			 RtToken tokens[], RtPointer params[]);
extern RtVoid RiPointsPolygons (RtInt npolys,
				RtInt nverts[], RtInt verts[], ...);
extern RtVoid RiPointsPolygonsV(RtInt npolys, 
				RtInt nverts[], RtInt verts[], 
				RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiPointsGeneralPolygons (RtInt npolys, RtInt nloops[],
				       RtInt nverts[], RtInt verts[], ...);
extern RtVoid RiPointsGeneralPolygonsV(RtInt npolys, RtInt nloops[],
				       RtInt nverts[], RtInt verts[], 
				       RtInt n, RtToken tokens[], RtPointer params[]);
/* ... */

extern RtVoid RiSphere(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax, ...);
extern RtVoid RiSphereV(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax,
			RtInt n, RtToken tokens[], RtPointer params[]);


/* attributes */
extern RtVoid RiAttribute(RtToken name, ...);
extern RtVoid RiAttributeV(RtToken name,
			   RtInt n, RtToken tokens[], RtPointer params[]);

extern RtVoid RiAttributeBegin();
extern RtVoid RiAttributeEnd();
extern RtVoid RiColor(RtColor color);
extern RtVoid RiOpacity(RtColor color);
extern RtVoid RiTextureCoordinates(RtFloat s1, RtFloat t1,
                                   RtFloat s2, RtFloat t2,
                                   RtFloat s3, RtFloat t3,
                                   RtFloat s4, RtFloat t4);

/* lights */
extern RtLightHandle RiAreaLightSource(RtToken name, ...);
extern RtLightHandle RiAreaLightSourceV(RtToken name, RtInt n,
				        RtToken tokens[], RtPointer params[]);
extern RtLightHandle RiLightSource(RtToken name, ...);
extern RtLightHandle RiLightSourceV(RtToken name, RtInt n,
				    RtToken tokens[], RtPointer params[]);

/* transformation block */
extern RtVoid RiTransformBegin(void);
extern RtVoid RiTransformEnd(void);

extern RtVoid RiOrientation(RtToken orientation);
extern RtVoid RiSides(RtInt sides);

extern RtVoid RiPixelSamples(RtFloat xsamples, RtFloat ysamples);

/* shaders */
extern RtVoid RiSurface(RtToken name, ...);
extern RtVoid RiSurfaceV(RtToken name,
			 RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiImager(RtToken name, ...);
extern RtVoid RiImagerV(RtToken name,
			RtInt n, RtToken tokens[], RtPointer params[]);

/* temporary. for liquid building */
extern RtToken RI_FOV, RI_RGBA;
extern RtBasis RiBSplineBasis;
extern RtVoid RiIlluminate(RtLightHandle light, RtBoolean onoff);
extern RtVoid RiDisplatement(const char *name, ...);
extern RtVoid RiDisplacementV(const char *name,
			      RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiAtmosphere(const char *name, ...);
extern RtVoid RiAtmosphereV(const char *name,
			    RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiBasis(RtBasis ubasis, RtInt ustep,
		      RtBasis vbasis, RtInt vstep);
extern RtVoid RiMatte(RtBoolean onoff);
extern RtVoid RiMotionBegin(RtInt n, ...);
extern RtVoid RiMotionBeginV(RtInt n, RtFloat times[]);
extern RtVoid RiMotionEnd();
extern RtFloat RiTriangleFilter(RtFloat x, RtFloat y,
				RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiCatmullRomFilter(RtFloat x, RtFloat y,
				  RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiGaussianFilter(RtFloat x, RtFloat y,
				RtFloat xwidth, RtFloat ywidth);
extern RtFloat RiSincFilter(RtFloat x, RtFloat y,
			    RtFloat xwidth, RtFloat ywidth);
extern RtVoid RiTrimCurve(RtInt nloops, RtInt ncurves[], RtInt order[],
			  RtFloat knot[], RtFloat min[], RtFloat max[],
			  RtInt n[], RtFloat u[], RtFloat v[], RtFloat w[]);
extern RtVoid RiNuPatchV(RtInt nu, RtInt uorder, RtFloat uknot[],
			 RtFloat umin, RtFloat umax, RtInt nv, RtInt vorder,
			 RtFloat vknot[], RtFloat vmin, RtFloat vmax,
		         RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiSubdivisionMesh(RtToken scheme, RtInt nfaces,
				RtInt nvertices[], RtInt vertices[],
				RtInt ntags, RtToken tags[], RtInt nargs[],
				RtInt intargs[], RtFloat floatargs[], ...);
extern RtVoid RiSubdivisionMeshV(RtToken scheme, RtInt nfaces,
				 RtInt nvertices[], RtInt vertices[],
				 RtInt ntags, RtToken tags[], RtInt nargs[],
				 RtInt intargs[], RtFloat floatargs[],
				 RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiCurvesV(RtToken type, RtInt ncurves, RtInt nvertices[],
			RtToken wrap,
			RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiArchiveRecord(RtToken type, char *format, ...);
extern RtVoid RiAtmosphere(const char *name, ...);
extern RtVoid RiAtmosphereV(const char *name,
			    RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiPoints(RtInt npoints, ...);
extern RtVoid RiPointsV(RtInt npoints,
			RtInt n, RtToken tokens[], RtPointer params[]);
extern RtVoid RiErrorHandler(RtErrorHandler handler);

extern RtVoid RiHider(RtToken type, ...);
extern RtVoid RiHiderV(RtToken type,
		       RtInt n, RtToken tokens[], RtPointer params[]);

/* ... */

#endif
