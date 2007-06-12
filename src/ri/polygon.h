/*
 * Polygon geometry handling routine.
 *
 * $Id: polygon.h,v 1.1.1.1 2004/01/06 13:57:10 syoyo Exp $
 */

#ifndef POLYGON_H
#define POLYGON_H

#include "ri.h"
#include "geom.h"
#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

ri_geom_t *ri_polygon_parse(RtInt nverts, RtInt n,
			    RtToken tokens[], RtPointer values[]);

ri_geom_t *ri_pointspolygons_parse(RtInt npolys, RtInt nverts[], RtInt verts[],
				   RtInt n, RtToken tokens[], RtPointer params[]);
ri_geom_t *ri_pointsgeneralpolygons_parse(RtInt npolys, RtInt nloops[],
					  RtInt nverts[], RtInt verts[],
				          RtInt n,
					  RtToken tokens[], RtPointer params[]);


#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
 
