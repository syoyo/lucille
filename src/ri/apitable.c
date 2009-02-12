/*
 *                                  lucille
 *                       A global illumination renderer
 *
 *                  Copyright (c) 2003 - 2203, Syoyo Fujita.
 *
 */

/*
 * $Id: apitable.c,v 1.4 2004/06/13 06:44:50 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>

#include "apitable.h"
#include "geom.h"
#include "util.h"
#include "render.h"
#include "log.h"

RtVoid
RiBegin(RtToken name)
{
    ri_api_begin(name);

    return;
}

RtVoid
RiEnd(void)
{
    ri_api_end();

    return;
}

RtVoid
RiFrameBegin(RtInt frame)
{
    ri_log(LOG_WARN, "(RI    ) RiFrameBegin() is not yet implemented, ignored.");
    //ri_api_frame_begin(frame);
    (void)frame;
}

RtVoid
RiFrameEnd(void)
{
    ri_log(LOG_WARN, "(RI    ) RiFrameEnd() is not yet implemented, ignored.");
    //ri_api_frame_end();
}

RtToken
RiDeclare(char *name, char *declaration)
{
    return ri_api_declare(name, declaration);
}

RtVoid
RiDepthOfField(RtFloat fstop, RtFloat focallength, RtFloat focaldistance)
{
    ri_api_depth_of_field(fstop, focallength, focaldistance);
}

RtVoid
RiWorldBegin(void)
{
    ri_api_world_begin();
}

RtVoid
RiWorldEnd(void)
{
    ri_api_world_end();
}


RtVoid
RiFormat(RtInt xres, RtInt yres, RtFloat aspect)
{
    ri_api_format(xres, yres, aspect);
}

RtVoid
RiFrameAspectRatio(RtFloat aspect)
{
    ri_api_frame_aspect_ratio(aspect);
}

RtVoid
RiScreenWindow(RtFloat left, RtFloat right, RtFloat bot, RtFloat top)
{
    ri_api_screen_window(left, right, bot, top);
}

RtVoid
RiCropWindow(RtFloat xmin, RtFloat xmax, RtFloat ymin, RtFloat ymax)
{
    ri_api_crop_window(xmin, xmax, ymin, ymax);
}

RtVoid
RiProjection(RtToken name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiProjectionV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;

}

RtVoid
RiProjectionV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_projection(name, n, tokens, params);
}

RtVoid
RiClipping(RtFloat hither, RtFloat yon)
{
    ri_api_clipping(hither, yon);
}

RtVoid
RiClippingPlane(RtFloat x, RtFloat y, RtFloat z,
                RtFloat nx, RtFloat ny, RtFloat nz)
{
    ri_api_clipping_plane(x, y, z, nx, ny, nz);
}


RtVoid
RiDisplay(char *name, RtToken type, RtToken mode, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, mode);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiDisplayV(name, type, mode, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;
}

RtVoid
RiDisplayV(char *name, RtToken type, RtToken mode,
       RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_display(name, type, mode, n, tokens, params);
}

RtVoid
RiPerspective(RtFloat fov)
{
    ri_api_perspective(fov);
}

RtVoid
RiOption(RtToken token, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, token);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    ri_log_and_return_if(n == 0);

    RiOptionV(token, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;

}

RtVoid
RiOptionV(RtToken token, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_option(token, n, tokens, params);
}

RtVoid
RiSphere(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, tmax);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiSphereV(radius, zmin, zmax, tmax, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;
}

RtVoid
RiSphereV(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax, 
      RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_sphere(radius, zmin, zmax, tmax, n, tokens, params);
}

RtVoid
RiPolygon(RtInt nverts, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, nverts);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiPolygonV(nverts, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;
}

RtVoid
RiPolygonV(RtInt nverts, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_polygon(nverts, n, tokens, params);

//    ri_render_parse_geom(ri_render_get(),
//                 "polygon", nverts, n, tokens, params);    
}

RtVoid
RiPointsPolygons(RtInt npolys, RtInt nverts[], RtInt verts[], ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, verts);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiPointsPolygonsV(npolys, nverts, verts, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;
}

RtVoid
RiPointsPolygonsV(RtInt npolys, RtInt nverts[], RtInt verts[],
          RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_pointspolygons(npolys, nverts, verts, n, tokens, params);

//    ri_render_parse_geom(ri_render_get(),
//                 "pointspolygon", nverts, n, tokens, params);    
}

RtVoid
RiPointsGeneralPolygons(RtInt npolys, RtInt nloops[], RtInt nverts[], RtInt verts[], ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, verts);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiPointsGeneralPolygonsV(npolys, nloops, nverts, verts,
                 n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;
}

RtVoid
RiPointsGeneralPolygonsV(RtInt npolys, RtInt nloops[],
             RtInt nverts[], RtInt verts[],
             RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_pointsgeneralpolygons(npolys, nloops, nverts, verts,
                     n, tokens, params);

}

RtVoid
RiSubdivisionMesh(RtToken scheme, RtInt nfaces,
          RtInt nvertices[], RtInt vertices[],
          RtInt ntags, RtToken tags[], RtInt nargs[],
          RtInt intargs[], RtFloat floatargs[], ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, floatargs);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiSubdivisionMeshV(scheme, nfaces, nvertices, vertices,
               ntags, tags, nargs, intargs, floatargs,
               n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;

}

RtVoid
RiSubdivisionMeshV(RtToken scheme, RtInt nfaces,
           RtInt nvertices[], RtInt vertices[],
           RtInt ntags, RtToken tags[], RtInt nargs[],
           RtInt intargs[], RtFloat floatargs[],
           RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_subdivision_mesh(scheme, nfaces, nvertices, vertices,
                ntags, tags, nargs, intargs, floatargs,
                n, tokens, params);
}

RtVoid
RiAttribute(RtToken token, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, token);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    if (n == 0) return;

    RiAttributeV(token, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return;

}

RtVoid
RiAttributeV(RtToken token, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_attribute(token, n, tokens, params);
}


RtVoid
RiAttributeBegin()
{
    ri_api_attribute_begin();
}

RtVoid
RiAttributeEnd()
{
    ri_api_attribute_end();
}

RtVoid
RiColor(RtColor color)
{
    ri_api_color(color);
}

RtVoid
RiOpacity(RtColor color)
{
    ri_api_opacity(color);
}

RtVoid
RiTextureCoordinates(RtFloat s1, RtFloat t1, RtFloat s2, RtFloat t2,
                     RtFloat s3, RtFloat t3, RtFloat s4, RtFloat t4)
{
    ri_api_texture_coordinates(s1, t1, s2, t2, s3, t3, s4, t4);
}

RtLightHandle
RiAreaLightSource(RtToken name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;
    RtLightHandle handle;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    handle = RiAreaLightSourceV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return handle;
}

RtLightHandle
RiAreaLightSourceV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    return ri_api_area_light_source(name, n, tokens, params);
}

RtLightHandle
RiLightSource(RtToken name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;
    RtLightHandle handle;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    handle = RiLightSourceV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

    return handle;
}

RtLightHandle
RiLightSourceV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    return ri_api_light_source(name, n, tokens, params);
}

RtVoid
RiTransformBegin(void)
{
    ri_api_transform_begin();
}

RtVoid
RiTransformEnd(void)
{
    ri_api_transform_end();
}

RtVoid
RiIdentity(void)
{
    ri_api_identity();
}

RtVoid
RiTransform(RtMatrix transform)
{
    ri_api_transform(transform);
}

RtVoid
RiConcatTransform(RtMatrix transform)
{
    ri_api_concat_transform(transform);
}

RtVoid
RiTranslate(RtFloat dx, RtFloat dy, RtFloat dz)
{
    ri_api_translate(dx, dy, dz);
}

RtVoid
RiRotate(RtFloat angle, RtFloat dx, RtFloat dy, RtFloat dz)
{
    ri_api_rotate(angle, dx, dy, dz);
}

RtVoid
RiScale(RtFloat sx, RtFloat sy, RtFloat sz)
{
    ri_api_scale(sx, sy, sz);
}

RtVoid
RiOrientation(RtToken orientation)
{
    ri_api_orientation(orientation);
}

RtVoid
RiSides(RtInt sides)
{
    ri_api_sides(sides);
}

RtVoid
RiPixelSamples(RtFloat xsamples, RtFloat ysamples)
{
    ri_api_pixel_samples(xsamples, ysamples);
}

RtVoid
RiShadingRate(RtFloat size)
{
    ri_log(LOG_WARN, "(RI    ) RiShadingRate() is not yet implemented");
    (void)size;
}

RtVoid
RiShadingInterpolation(RtToken type)
{
    ri_log(LOG_WARN, "(RI    ) RiShadingInterpolation() is not yet implemented, ignored.");
    (void)type;
}

RtVoid
RiShutter(RtFloat min, RtFloat max)
{
    ri_log(LOG_WARN, "(RI    ) RiShutter() is not yet implemented, ignored.");
    (void)min;
    (void)max;
}

RtVoid
RiPixelFilter(RtFilterFunc filterfunc, RtFloat xwidth, RtFloat ywidth)
{
    ri_log(LOG_WARN, "(RI    ) RiPixelFilter() is not yet implemented, ignored.");
    (void)filterfunc;
    (void)xwidth;
    (void)ywidth;
}

RtVoid
RiQuantize(RtToken type, RtInt one, RtInt min, RtInt max, RtFloat ampl)
{
    ri_log(LOG_WARN, "(RI    ) RiQuantize() is not yet implemented, ignored.");
    (void)type;
    (void)one;
    (void)min;
    (void)max;
    (void)ampl;
}

RtVoid
RiCoordinateSystem(RtToken space)
{
    ri_log(LOG_WARN, "(RI    ) RiCoordinateSystem() is not yet implemented, ignored.");
    (void)space;
}


RtVoid
RiExposure(RtFloat gain, RtFloat gamma)
{
    ri_api_exposure(gain, gamma);
}

RtVoid
RiSurface(RtToken name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiSurfaceV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiSurfaceV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_surface(name, n, tokens, params);
}

RtVoid
RiImager(RtToken name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiImagerV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiImagerV(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_imager(name, n, tokens, params);
}

RtVoid
RiIlluminate(RtLightHandle light, RtBoolean onoff)
{
    ri_log(LOG_WARN, "(RI    ) RiIlluminate() is not yet implemented, ignored.");
    (void)light;
    (void)onoff;
}

RtVoid
RiDisplatement(const char *name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiDisplacementV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiDisplacementV(const char *name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiDisplacementV() is not yet implemented, ignored.");
    (void)name;
    (void)n;
    (void)tokens;
    (void)params;


}

RtVoid
RiAtmosphere(const char *name, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, name);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiAtmosphereV(name, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiAtmosphereV(const char *name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiAtomosphereV() is not yet implemented, ignored.");
    (void)name;
    (void)n;
    (void)tokens;
    (void)params;


}

RtVoid
RiPoints(RtInt npoints, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, npoints);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiPointsV(npoints, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiPointsV(RtInt npoints, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiPointsV() is not yet implemented, ignored.");
    (void)npoints;
    (void)n;
    (void)tokens;
    (void)params;
}

RtVoid
RiBasis(RtBasis ubasis, RtInt ustep, RtBasis vbasis, RtInt vstep)
{
    ri_log(LOG_WARN, "(RI    ) RiBasis() is not yet implemented, ignored.");
    (void)ubasis;
    (void)ustep;
    (void)vbasis;
    (void)vstep;

}

RtVoid
RiMatte(RtBoolean onoff)
{
    ri_log(LOG_WARN, "(RI    ) RiMatte() is not yet implemented, ignored.");
    (void)onoff;
}

RtVoid
RiMotionBegin(RtInt n, ...)
{
    ri_log(LOG_WARN, "(RI    ) RiMotionBegin() is not yet implemented, ignored.");
    (void)n;

}

RtVoid
RiMotionBeginV(RtInt n, RtFloat times[])
{
    ri_log(LOG_WARN, "(RI    ) RiMotionBeginV() is not yet implemented, ignored.");
    (void)n;
    (void)times;
}

RtVoid
RiMotionEnd()
{
    ri_log(LOG_WARN, "(RI    ) RiMotionEnd() is not yet implemented, ignored.");
}

RtVoid
RiTrimCurve(RtInt nloops, RtInt ncurves[], RtInt order[], RtFloat knot[],
        RtFloat min[], RtFloat max[], RtInt n[], RtFloat u[], RtFloat v[],
        RtFloat w[])
{
    ri_log(LOG_WARN, "(RI    ) RiTrimCurve() will not be implemented, ignored.");
    (void)nloops;
    (void)ncurves;
    (void)order;
    (void)knot;
    (void)min;
    (void)max;
    (void)n;
    (void)u;
    (void)v;
    (void)w;
}

RtVoid
RiPatch(
    RtToken type,
    ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, type);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiPatchV(type, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

}

RtVoid
RiPatchV(
    RtToken   type,
    RtInt     n,
    RtToken   tokens[],
    RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiPatch() is not yet implemened, ignored.");

    (void)type;
    (void)n;
    (void)tokens;
    (void)params;
}

RtVoid
RiPatchMesh(
    RtToken   type,
    RtInt     nu,
    RtToken   uwrap,
    RtInt     nv,
    RtToken   vwrap,
    ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, vwrap);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiPatchMeshV(type, nu, uwrap, nv, vwrap, n, tokens, params);

    ri_util_paramlist_free(tokens, params);

}

RtVoid
RiPatchMeshV(
    RtToken   type,
    RtInt     nu,
    RtToken   uwrap,
    RtInt     nv,
    RtToken   vwrap,
    RtInt     n,
    RtToken   tokens[],
    RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiPatchMesh() is not yet implemened, ignored.");
    (void)type;
    (void)nu;
    (void)uwrap;
    (void)nv;
    (void)vwrap;
    (void)n;
    (void)tokens;
    (void)params;

}


RtVoid
RiNuPatchV(RtInt nu, RtInt uorder, RtFloat uknot[], RtFloat umin, RtFloat umax,
       RtInt nv, RtInt vorder, RtFloat vknot[], RtFloat vmin, RtFloat vmax,
       RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiNuPatchV() will not be implemented, ignored.");
    (void)nu;
    (void)uorder;
    (void)uknot;
    (void)umin;
    (void)umax;
    (void)nv;
    (void)vorder;
    (void)vknot;
    (void)vmin;
    (void)vmax;
    (void)n;
    (void)tokens;
    (void)params;
}

RtVoid
RiCurvesV(RtToken type, RtInt ncurves, RtInt nvertices[], RtToken wrap,
      RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "(RI    ) RiCurvesV() will not be implemented, ignored.");
    (void)type;
    (void)ncurves;
    (void)nvertices;
    (void)wrap;
    (void)n;
    (void)tokens;
    (void)params;
}

RtVoid
RiArchiveRecord(RtToken type, char *format, ...)
{
    ri_log(LOG_WARN, "(RI    ) RiArchiveRecord() is not yet implemented, ignored.");
    (void)type;
    (void)format;

}

RtVoid
RiErrorHandler(RtErrorHandler handler)
{
    ri_log(LOG_WARN, "(RI    ) RiErrorHandler() is not yet implemented, ignored.");
    (void)handler;
}

RtVoid
RiHider(RtToken type, ...)
{
    RtInt      n;
    va_list    args;
    RtToken   *tokens;
    RtPointer *params;

    va_start(args, type);

    n = ri_util_paramlist_build(args, &tokens, &params);

    va_end(args);

    RiHiderV(type, n, tokens, params);

    ri_util_paramlist_free(tokens, params);
}

RtVoid
RiHiderV(RtToken type, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_api_hider(type, n, tokens, params);
}
