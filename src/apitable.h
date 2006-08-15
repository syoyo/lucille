#ifndef APITABLE_H
#define APITABLE_H

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_api_begin(RtToken name);
extern void ri_api_end(void);

extern void ri_api_world_begin();
extern void ri_api_world_end();

extern RtToken ri_api_declare(char *name, char *declaration);

#if 0
extern void ri_api_frame_begin(RtInt frame);
extern void ri_api_frame_end();
#endif

/* options */
extern void ri_api_format            (RtInt xresplution, RtInt yresolution, RtFloat aspect);
extern void ri_api_frame_aspect_ratio(RtFloat aspect);
extern void ri_api_screen_window     (RtFloat left, RtFloat right,
				      RtFloat bot,  RtFloat top);
extern void ri_api_crop_window       (RtFloat xmin, RtFloat xmax,
                                      RtFloat ymin, RtFloat ymax);
extern void ri_api_projection        (RtToken name,
			              RtInt n, RtToken tokens[], RtPointer params[]);
extern void ri_api_clipping          (RtFloat hither, RtFloat yon);
extern void ri_api_clipping_plane    (RtFloat x, RtFloat y, RtFloat z,
                                      RtFloat nx, RtFloat ny, RtFloat nz);
extern void ri_api_depth_of_field    (RtFloat fstop, RtFloat focallength,
				      RtFloat focaldistance);
extern void ri_api_shutter           (RtFloat min, RtFloat max);
extern void ri_api_exposure	     (RtFloat gain, RtFloat gamma);
extern void ri_api_display           (char *name, RtToken type, RtToken mode,
			              RtInt n, RtToken tokens[], RtPointer params[]);
extern void ri_api_pixel_samples     (RtFloat xsamples, RtFloat ysamples);

extern void ri_api_option(RtToken name, RtInt n, RtToken tokens[], RtPointer params[]);

/* transformations */
extern void ri_api_identity        (void);
extern void ri_api_transform       (RtMatrix transform);
extern void ri_api_concat_transform(RtMatrix transform);
extern void ri_api_perspective     (RtFloat fov);
extern void ri_api_translate       (RtFloat dx, RtFloat dy, RtFloat dz);
extern void ri_api_rotate          (RtFloat angle, RtFloat dx, RtFloat dy, RtFloat dz);
extern void ri_api_scale           (RtFloat sx, RtFloat sy, RtFloat sz);

/* lights */
extern RtLightHandle ri_api_area_light_source(RtToken name, RtInt n,
					 RtToken tokens[], RtPointer params[]);
extern RtLightHandle ri_api_light_source(RtToken name, RtInt n,
					 RtToken tokens[], RtPointer params[]);

extern void ri_api_sphere(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax,
			  RtInt n, RtToken tokens[], RtPointer params[]);

/* attributes */
extern void ri_api_attribute(RtToken token,
			     RtInt n, RtToken tokens[], RtPointer params[]);
extern void ri_api_attribute_end  ();
extern void ri_api_attribute_begin();
extern void ri_api_attribute_end  ();
extern void ri_api_color          (RtColor color);
extern void ri_api_opacity        (RtColor color);
extern void ri_api_texture_coordinates(RtFloat s1, RtFloat t1,
                                       RtFloat s2, RtFloat t2,
                                       RtFloat s3, RtFloat t3,
                                       RtFloat s4, RtFloat t4);

/* transformation block */
extern void ri_api_transform_begin();
extern void ri_api_transform_end  ();

extern void ri_api_orientation(RtToken orientation);

extern void ri_api_polygon       (RtInt nverts,
				  RtInt n,
				  RtToken tokens[], RtPointer params[]);
extern void ri_api_pointspolygons(RtInt npolys, RtInt nverts[], RtInt verts[],
				  RtInt n,
				  RtToken tokens[], RtPointer params[]);
extern void ri_api_pointsgeneralpolygons(RtInt npolys, RtInt nloops[],
					 RtInt nverts[], RtInt verts[],
				         RtInt n,
				         RtToken tokens[], RtPointer params[]);
extern void ri_api_subdivision_mesh(RtToken scheme, RtInt nfaces,
				    RtInt nvertices[], RtInt vertices[],
				    RtInt ntags, RtToken tags[], RtInt nargs[],
				    RtInt intargs[], RtFloat floatargs[],
				    RtInt n,
				    RtToken tokens[], RtPointer params[]);
extern void ri_api_surface(RtToken name,
			   RtInt n, RtToken tokens[], RtPointer params[]);
extern void ri_api_imager(RtToken name,
			  RtInt n, RtToken tokens[], RtPointer params[]);

extern void ri_api_sides(RtInt sides);

extern void ri_api_hider(RtToken type,
			 RtInt n, RtToken tokens[], RtPointer params[]);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
