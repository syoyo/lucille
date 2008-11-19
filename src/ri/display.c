/*
 * RenderMan display option
 *
 * $Id: display.c,v 1.2 2004/04/16 13:46:45 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>	/* tolower() */

#include "apitable.h"
#include "display.h"
#include "memory.h"
#include "log.h"
#include "render.h"

static void auto_detect_format();
static int  casecmp(const char *s1, const char *s2);

ri_display_t *
ri_display_new()
{
	ri_display_t *p = NULL;

	p = (ri_display_t *)ri_mem_alloc(sizeof(ri_display_t));

	/* todo initialize code here */
	p->pixel_variance = 0.0;
	p->sampling_rates[0] = 2.0;
	p->sampling_rates[1] = 2.0;

	/* p->filter = RiGaussianFilter; */

	p->filter_width[0] = 2.0;
	p->filter_width[1] = 2.0;

	p->gain  = 1.0;
	p->gamma = 1.0;

	p->color_quantizer.one              = 255;
	p->color_quantizer.maximum          =   0;
	p->color_quantizer.minimum          = 255;
	p->color_quantizer.dither_amplitude = 0.5;

	p->depth_quantizer.one              =   0;
	p->depth_quantizer.maximum          =   0;
	p->depth_quantizer.minimum          =   0;
	p->depth_quantizer.dither_amplitude =   0;

	p->display_type   = RI_FILE;	/* = .hdr		*/
	p->display_name   = "untitled.hdr";
	p->display_mode   = RI_RGB;
	p->display_format = "byte";	/* .hdr DD ignore this	*/

	return p;
}

void
ri_display_free(ri_display_t *display)
{
	ri_mem_free(display);
}

void
ri_api_display(char *name, RtToken type, RtToken mode,
	       RtInt n, RtToken tokens[], RtPointer params[])
{
	/* todo: full implementation */
	int i;
	RtToken *tokp;
	ri_display_t *disp;

	if (strcmp(mode, RI_RGB) != 0) {
		ri_log(LOG_WARN, "Display: currently, only \"rgb\" mode is supported");
		mode = RI_RGB;
	}


    /*
     * Check if multiple display
     */
    if (name[0] == '+') {
        
        /* Create new display. 
         * FIXME: copy display state from previous display.
         */
        disp = ri_display_new();
        assert(strlen(name) > 1);
	    disp->display_name = strdup(name + 1);

    } else {

        disp = ri_option_get_curr_display(ri_render_get()->context->option);
	    disp->display_name = strdup(name);

    }

	disp->display_type = strdup(type);

	for (i = 0; i < n; i++) {
		printf("disp token[%d] = %s\n", i, tokens[i]); 
		if (strcmp(tokens[i], "format") == 0) {
			tokp = (RtToken *)params[i];
			if (strcmp(*tokp, "float") == 0) {
				disp->display_format = strdup(*tokp);
			}
		}
	}

	auto_detect_format();
}

void
ri_dd_callback(int (*opencb)(const char *name,
			     int width, int height,
			     int bits, RtToken component,
			     const char *format),
	       int (*closecb)(void),
	       int (*writecb)(int x, int y, const void *pixel))
{
	ri_display_drv_t *drv = NULL;

	drv = (ri_display_drv_t *)ri_mem_alloc(sizeof(ri_display_drv_t));
	drv->open  = opencb;
	drv->close = closecb;
	drv->write = writecb;

	ri_render_register_display_drv(ri_render_get(), "callback", drv);

	ri_render_get()->dd_use_callback = 1;
}

static void
auto_detect_format()
{
	ri_display_t *disp = NULL;
	char         *ext  = NULL;
	char          buf[1024];
	int           len;

	disp = ri_option_get_curr_display(ri_render_get()->context->option);


	if (casecmp(disp->display_type, "file") == 0)  {
		/* Get file extension. */
		ext = strrchr(disp->display_name, '.');

		if (ext == NULL) {	/* file extension does not found. */
			/* use lucille native .hdr image format. */

			/* add file extension. */
			sprintf(buf, "%s.hdr", disp->display_name);
			free(disp->display_name);
			disp->display_name = strdup(buf);
		} else {
			if (casecmp(ext, ".hdr") == 0) {
				/* native .hdr image format. do nothing. */
					
#ifdef HAVE_LIBTIFF
			} else if (casecmp(ext, ".tif") == 0 ||
				   casecmp(ext, ".tiff") == 0) {
				/* TIFF */
				free(disp->display_type);
				disp->display_type = strdup("tiff");
#endif
			} else {
				/* use lucille native .hdr image format. */

				/* replace file extension. */
				len = ext - disp->display_name;

				strncpy(buf, disp->display_name, len);
				strncpy(&buf[0] + len, ".hdr\0", strlen(".hdr") + 1);

				free(disp->display_name);
				disp->display_name = strdup(buf);
				free(disp->display_type);
				disp->display_type = strdup("hdr");
			}
		}

#ifdef HAVE_LIBTIFF
	} else if (casecmp(disp->display_type, "tiff") == 0 ||
		   casecmp(disp->display_type, "tif") == 0) {

		/* Get file extension. */
		ext = strrchr(disp->display_name, '.');

		if (ext == NULL) {	/* no extension */

			/* add file extension. */
			sprintf(buf, "%s.tif", disp->display_name);
			free(disp->display_name);
			disp->display_name = strdup(buf);
			
		} else {
			if (casecmp(ext, ".tif") != 0 &&
			    casecmp(ext, ".tiff") != 0) {

				/* replace file extension. */
				len = ext - disp->display_name;

				strncpy(buf, disp->display_name, len);
				strncpy(&buf[0] + len, ".tif\0", strlen(".tif") + 1);
				free(disp->display_name);
				disp->display_name = strdup(buf);
			}
		}
#endif
	} else {

	}
}

/*
 * Compare two strings ignoring differences in case.
 * Same as strcasecmp(), but strcasecmp() seems not posix or ansi-C function.
 */
static int
casecmp(const char *s1, const char *s2)
{
        const char *k1 = s1, *k2 = s2;

        while (k1 != NULL && *k1 != '\0' &&
               k2 != NULL && *k2 != '\0') {
                if (tolower((int)*k1) != tolower((int)*k2)) {
                        return -1;
                }

                k1++; k2++;
        }
        
        return 0;
}
