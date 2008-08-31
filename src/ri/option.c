/*
 * RenderMan option
 *
 * $Id: option.c,v 1.7 2004/06/13 06:44:51 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(__MACH__) && defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#if defined(WIN32)
#include <windows.h>	/* GetSystemInfo */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "accel.h"
#include "option.h"
#include "memory.h"
#include "apitable.h"
#include "log.h"
#include "hash.h"
#include "render.h"
#include "parallel.h"
#include "transport.h"

typedef struct _opt_t
{
	RtToken   *tokens;
	RtPointer *params; 
} opt_t;

static ri_hash_t *impl_options;

static void       free_func(void *data);

/* detect the number of cpus in the system. */
#if defined(WITH_PTHREAD) || (!defined(NOTHREAD) && defined(WIN32))
static int get_numcpus();
#endif

static int toInt(RtPointer p)
{
	RtFloat *f;
	f = (RtFloat *)p;
	return (int)(*f);
}

ri_option_t *
ri_option_new()
{
	ri_option_t *p = NULL;

	p = (ri_option_t *)ri_mem_alloc(sizeof(ri_option_t));

	p->camera                    = ri_camera_new();
	p->display                   = ri_display_new();
	p->hider                     = "hidden";
	p->searchpath                = ri_ptr_array_new();
	p->relative_detail           = 1.0;
	p->orientation               = RI_LH;
	p->nfinalgather_rays         = 5;
	p->narealight_rays           = 16;
	p->max_ray_depth             = 5;

	p->irradcache_find_tolerance = 5.0;
	p->irradcache_max_radius     = 1.0;

	p->bssrdf_nsamples           = 100;
	p->bssrdf_scatter            = 2.19;
	p->bssrdf_absorb             = 0.0021;
	p->bssrdf_scale              = 1.0;
	p->bssrdf_tree_level         = 3;
	p->bssrdf_cache_file         = NULL;

	p->enable_direct_lighting    = 1;
	p->enable_indirect_lighting  = 0;
	p->enable_caustics_lighting  = 0;
	p->irradcache_file           = NULL;

	p->accel_method              = RI_ACCEL_BVH;

	p->compute_prt               = 0;
	p->prt_is_glossy             = 0;
	p->prt_nsamples              = 64;
	p->prt_samplinglevel         = 3;
	p->prt_do_distscale          = 0;
	p->prt_scale                 = 1.0;

	ri_vector_setzero( p->bgcolor );
	ri_vector_setzero( p->ambcolor );

#if defined(WITH_PTHREAD) || (!defined(NOTHREAD) && defined(WIN32))
	/*
	 * default: automatically determine the number of threads to use, 
	 * from the number of cpus installed in the system.
	 */

	p->nthreads = get_numcpus();
#else	/* compiled with no thread support */
	p->nthreads = 0;
#endif

	p->use_qmc = 0;
	p->render_method = TRANSPORT_MCRAYTRACE;

	p->pt_nsamples = 4;

	p->mlt_nsamples = 10000;
	p->bsp_tree_depth = 6;
	p->kd_tree_depth = 16;

	//p->pixel_filter = RiBoxFilter;
	p->pixel_filter_widthx = 1.0;
	p->pixel_filter_widthy = 1.0;

	p->do_adaptive_supersampling = 1;

	return p;
}

void
ri_option_free(ri_option_t *option)
{
	ri_camera_free(option->camera);
	ri_display_free(option->display);
	ri_ptr_array_traverse(option->searchpath, free_func);
	ri_ptr_array_free(option->searchpath);

	ri_mem_free(option);
}

void
ri_option_add_searchpath(ri_option_t *option, const char *path)
{
	char *p = NULL;

	p = strdup(path);

	ri_ptr_array_insert(option->searchpath,
			    option->searchpath->nelems,
			    (void *)p);
}

/* Function: ri_option_find_file
 *
 *     Finds a file with search paths specified by Option command.
 *
 * Parameters:
 *
 *     fullpath - Pointer to buffer for storing full path to the found file.
 *                fullpath must point to the memory buffer to store enough
 *                path string.
 *     option   - The option which contains parsed search paths.
 *     file     - Name of file to find.
 *
 * Returns:
 *
 *     If the file found, return 1.
 *     If not, return 0.
 *
 */
int
ri_option_find_file(char *fullpath, const ri_option_t *option, const char *file)
{
	unsigned int  i;
	FILE         *fp = NULL;
	char         *path;

	fp = fopen(file, "r");
	if (fp) {
		/* found the file! */
		sprintf(fullpath, "%s", file);
		fclose(fp);
		return 1;
	} 

	for (i = 0; i < option->searchpath->nelems; i++) {
		path = (char *)ri_ptr_array_at(option->searchpath, i);

		if (path) {
			sprintf(fullpath, "%s%s", path, file);

			fp = fopen(fullpath, "r");

			if (fp) {
				fclose(fp);
				return 1;		/* found the file! */
			}
		}
	}

	return 0;
}

void
ri_impl_option_insert(const char *name, void *val)
{
	static int initialized = 0;

	if (!initialized) {
		impl_options = ri_hash_new();

		initialized = 1;
	}

	ri_hash_insert(impl_options, name, val);

}

void *
ri_impl_option_get(const char *name)
{
	return ri_hash_lookup(impl_options, name);
}

void
ri_api_option(RtToken token, RtInt n, RtToken tokens[], RtPointer params[])
{
	int i;
	ri_option_t           *ctxopt;
	ri_camera_t           *camera;
	RtFloat *valp;
	RtToken *tokp;
	//ri_geom_t *prtgeom;

	//opt_t *opt;

	//opt = (opt_t *)ri_mem_alloc(sizeof(opt_t));

	ctxopt  = ri_render_get()->context->option;
	camera  = ctxopt->camera;

	if (strcmp(token, "raytrace") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "finalgather_rays") == 0) {
				valp = (RtFloat *)params[i];
				ctxopt->nfinalgather_rays =
					(unsigned int)(*valp);
			} else if (strcmp(tokens[i], "arealight_rays") == 0) {
				valp = (RtFloat *)params[i];

				ctxopt->narealight_rays =
					(unsigned int)(*valp++);
			} else if (strcmp(tokens[i], "max_ray_depth") == 0) {
				valp = (RtFloat *)params[i];

				ctxopt->max_ray_depth =
					(unsigned int)(*valp);
			} else if (strcmp(tokens[i], "accel_method") == 0) {
				tokp = (RtToken *)params[i];

				if (strcmp(*tokp, "grid") == 0) {
					ctxopt->accel_method = RI_ACCEL_UGRID;	
					ri_log(LOG_INFO, "Use ugrid");
				} else if (strcmp(*tokp, "bvh") == 0) {
					ctxopt->accel_method = RI_ACCEL_BVH;	
					ri_log(LOG_INFO, "Use BVH");
				}
			}
		}
	} else if (strcmp(token, "lighting") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "direct_lighting") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "yes") == 0) {
					ctxopt->enable_direct_lighting = 1;
				} else {
					ctxopt->enable_direct_lighting = 0;
				}
			} else if (strcmp(tokens[i], "indirect_lighting") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "yes") == 0) {
					ctxopt->enable_indirect_lighting = 1;
				} else {
					ctxopt->enable_indirect_lighting = 0;
				}
			} else if (strcmp(tokens[i], "caustics_lighting") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "yes") == 0) {
					ctxopt->enable_caustics_lighting = 1;
				} else {
					ctxopt->enable_caustics_lighting = 0;
				}
			}
		}
	} else if (strcmp(token, "global") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "background") == 0) {
				valp = (RtFloat *)params[i];

				ctxopt->bgcolor[0] = (ri_float_t)(*valp++);
				ctxopt->bgcolor[1] = (ri_float_t)(*valp++);
				ctxopt->bgcolor[2] = (ri_float_t)(*valp);
			}
		}
	} else if (strcmp(token, "renderer") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "multithread") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "no") == 0) {
					ctxopt->nthreads = 0;
				}
			} else if (strcmp(tokens[i], "nthreads") == 0) {
#if defined(WITH_PTHREAD) || (!defined(NOTHREAD) && defined(WIN32))
				valp = (RtFloat *)params[i];
				if ((int)(*valp) < 0) {
					// -1 means auto-detect # of CPUs
					// by get_numcpus().
				} else {
					ctxopt->nthreads = (int)(*valp);
				}
#endif
			} else if (strcmp(tokens[i], "qmc") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "no") == 0) {
					ctxopt->use_qmc = 0;
				}
			} else if (strcmp(tokens[i], "method") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "mlt") == 0) {
					ctxopt->render_method = TRANSPORT_MLT;
				} else if (strcmp(*tokp, "pathtrace") == 0) {
					ctxopt->render_method =
							TRANSPORT_PATHTRACE;
				}
			} else if (strcmp(tokens[i], "adaptive_supersampling") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "no") == 0) {
					ctxopt->do_adaptive_supersampling = 0;
				}
			}
		}
	} else if (strcmp(token, "mlt") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "nsamples") == 0) {
				valp = (RtFloat *)params[i];
				ctxopt->mlt_nsamples = (int)(*valp);
			}
		}
	} else if (strcmp(token, "pathtrace") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "nsamples") == 0) {
				ctxopt->pt_nsamples = toInt(params[i]);
			}
		}
	} else if (strcmp(token, "camera") == 0) {
		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "glcamera") == 0) {
				tokp = (RtToken *)params[i];
				if (strcmp(*tokp, "yes") == 0) {
					camera->use_glcamera = 1;
				} else {
					camera->use_glcamera = 0;
				}
			} else if (strcmp(tokens[i], "origin") == 0) {
				valp = (RtFloat *)params[i];

				camera->cam_pos[0] = (ri_float_t)(*valp++);
				camera->cam_pos[1] = (ri_float_t)(*valp++);
				camera->cam_pos[2] = (ri_float_t)(*valp);
			} else if (strcmp(tokens[i], "target") == 0) {
				valp = (RtFloat *)params[i];

				camera->cam_at[0] = (ri_float_t)(*valp++);
				camera->cam_at[1] = (ri_float_t)(*valp++);
				camera->cam_at[2] = (ri_float_t)(*valp);
			} else if (strcmp(tokens[i], "up") == 0) {
				valp = (RtFloat *)params[i];

				camera->cam_up[0] = (ri_float_t)(*valp++);
				camera->cam_up[1] = (ri_float_t)(*valp++);
				camera->cam_up[2] = (ri_float_t)(*valp);
			}
		}
	}
}

#if defined(WITH_PTHREAD) || (!defined(NOTHREAD) && defined(WIN32))
static int
get_numcpus()
{
	int cpus = 0;	/* zero means disable multithreaded rendering. */

#ifdef WIN32
	SYSTEM_INFO info;
	
	GetSystemInfo(&info);

	if (info.dwNumberOfProcessors > 1) {
		cpus = info.dwNumberOfProcessors;	
	}
#elif defined(__APPLE__) && defined(__MACH__)	/* OS X */
	int mib[2], rc;
	size_t len;

	mib[0] = CTL_HW;
	mib[1] = HW_NCPU;
	len = sizeof(cpus);
	rc = sysctl(mib, 2, &cpus, &len, NULL, 0);

#elif defined(LINUX)	/* linux */
	FILE *fp;
	char buf[1024];

	fp = fopen("/proc/cpuinfo", "r");
	if (!fp) return 0;

	while (!feof(fp)) {
		fgets(buf, 1023, fp);
		if (strncasecmp("ht", buf, strlen("ht")) == 0) {
			/* Hyper Thread */
			cpus++;
		} else if (strncasecmp("processor", buf, strlen("processor")) == 0) {
			/* Pysical CPU processor */
			cpus++;
		}
	}
#endif
	if (cpus < 2) cpus = 0;

	return cpus;
}
#endif

static void
free_func(void *data)
{
	ri_mem_free(data);
}
