/*
 * main routine for lsh
 *
 * $Id: main.c 109 2005-09-08 14:54:49Z lucille $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _MSC_VER		// visual c++
#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "user32.lib")
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__CYGWIN__) || defined(LINUX) || defined(__MACH__)
#include <unistd.h>
#endif

#if defined(__CYGWIN__) || defined(WIN32)
#include "my_getopt.h"	// BSD licensed my_getopt
#define optind my_optind
#define optarg my_optarg
#define getopt_long my_getopt_long
#else
#include <getopt.h>	// assume getopt.h is installed in the system.
#endif


#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include "lsh.h"
#include "ri.h"
#include "option.h"
#include "hash.h"
#include "display.h"
#include "parallel.h"
#include "timer.h"
#include "render.h"
#include "backdoor.h"
#include "thread.h"
#include "log.h"

/* this RIB path is used for texture file finding. */
#ifndef MAX_RIBPATH
#define MAX_RIBPATH 1024
#endif

static char version[] = "0.2";
static char compile[] = __DATE__;

char   *ribfile = NULL;	/* filename of RIB */

int     flag_pixelsamples = 0;
int     pixelsamples      = 0;
int     flag_maxraydepth  = 0;
int     maxraydepth       = 0;
int     debug_mode        = 0;

static void info();
static void option_list();
static void usage();
static void world_begin_cb(void);		/* WorldBegin callback */
static void parse_arg(int argc, char **argv);	
static void show_ddlist();
static void show_ddlist_func(void *data, void *userdata);

void
set_ribpath(const char *path)
{
	char buf[MAX_RIBPATH];
	char *filenamepos; 
	int  len;

#if defined(WIN32) && !defined(__CYGWIN__)
	filenamepos = strrchr(path, '\\');
#else
	filenamepos = strrchr(path, '/');
#endif
	if (filenamepos) {

		len = filenamepos - path + 1; /* +1 for delimiter */
		strncpy(buf, path, len);
		buf[len] = '\0';

		/* TODO: move this kind to option->searchpath */
		strcpy(ri_render_get()->ribpath, buf);

		ri_option_add_searchpath(ri_render_get()->context->option,
				         buf);
	}
	
}

int
main(int argc, char **argv)
{
	extern FILE *yyin;
	extern void  yyparse();
	extern int   yydebug;
	char         buf[1024];
#ifdef WITH_GUNZIP
	char        *ext;
#endif

#ifdef WITH_READLINE
	lsh_t *lsh = NULL;
#endif

	ri_parallel_init(&argc, &argv);

	if (argc < 2) {

		info();

#ifdef WITH_READLINE
		lsh = lsh_new();
		
		//printf("Opening lucille shell...\n");

		lsh_exec(lsh);

		lsh_free(lsh);
#else
		usage();
		exit(-1);
#endif
	} else {

		parse_arg(argc, argv);

		if (!ribfile) {
			info();
			usage();
			exit(-1);
		}

		yydebug = 0;
		if (debug_mode) {
			/* turn on parser debug mode */
			//yydebug = 1;
		} 

		RiBegin(RI_NULL);

		/* set debuggin mode */
		ri_render_get()->debug_mode = debug_mode;


		/* register WorldBegin callback to override
		 * rendering options by command line argument.
		 */
		ri_backdoor_world_begin_cb(world_begin_cb);

		/* ri_timer_end() is called in ri_api_world_end() */
		ri_timer_start(ri_render_get()->context->timer, "RIB parsing");

#ifdef WITH_GUNZIP
		/* Is file is gzip'ed? */
		ext = strrchr(ribfile, '.');
		printf("ext = %s\n", ext);
		if (ext != NULL && (strcasecmp(ext, ".gz") == 0)) {
			/* file has .gz suffix. */
			sprintf(buf, "gunzip -c %s", ribfile);
			yyin = popen(buf, "r");
			printf("gzipped rib\n");
		} else {
			yyin = fopen(ribfile, "r");
			
		}
#else
		set_ribpath(ribfile);
		
		yyin = fopen(ribfile, "r");
#endif

		if (!yyin) {
			printf("can't open file [ %s ]\n", ribfile);
			exit(-1);
		}

#if !defined(WIN32)
		/* Add current working directory to search path. */
		getcwd(buf, 1024 - 1);
		ri_option_add_searchpath(ri_render_get()->context->option,
					 buf);
#endif
		

		yyparse(yyin);

		RiEnd();
	}

#ifdef WITH_DMALLOC
	dmalloc_shutdown();
#endif

	ri_parallel_finalize();

	return 0;
}

static void
world_begin_cb(void)
{
	/* Override values with the value specified by command line argument. */

	ri_option_t *opt = ri_render_get()->context->option;

	if (flag_pixelsamples) {
		opt->display->sampling_rates[0] = pixelsamples;
		opt->display->sampling_rates[1] = pixelsamples;
	}

	if (flag_maxraydepth) {
		opt->max_ray_depth = maxraydepth;
	}
}

static void
parse_arg(int argc, char **argv)
{
	int ival;

	struct option longopts[] = {
		{"help", 0, 0, 'h'},
		{"info", 0, 0, 'i'},
		{"version", 0, 0, 'v'},
		/* PRMan specific options */
		{"recover", 0, 0, 'r'},
		{"progress", 0, 0, 'p'},
		{"Progress", 0, 0, 'P'},
		/* lucille specific options */
		{"pixelsamples", 1, 0, 's'},	
		{"maxraydepth", 1, 0, 'm'},	
		{"debug", 0, 0, 'd'},
		{"verbose", 0, 0, 'b'},
		{0, 0, 0, 0}
	};

	int opt;
	int longind = 0;

	while ((opt = getopt_long(argc, argv, "", longopts, &longind)) != -1) {
		switch (opt) {
		case 'h':
		case 'v':
			/* TODO: Print help information. */
			RiBegin(RI_NULL);	/* Initialize renderer. */
			info();
			option_list();	/* Print program options. */
			RiEnd();
			exit(-1);

			break;

		case 'i':
			/* TODO: Print help information. */
			RiBegin(RI_NULL);	/* Initialize renderer. */
			info();
			show_ddlist();	/* Print display drivers available. */
			RiEnd();
			exit(-1);

			break;
		case 'r':
			/* "-recover". This is a PRMan's option. Ignore it. */
			break;
		case 'p':
		case 'P':
			/* "-progress". This is a PRMan's option. Ignore it. */
			break;
		case 's':
			ival = atoi(optarg);
			if (ival > 16) ival = 16;
			printf("[lucille] pixelsamples = %d\n", ival);

			pixelsamples = ival;
			flag_pixelsamples = 1;

			break;
			
		case 'm':

			ival = atoi(optarg);
			if (ival > 16) ival = 16;

			maxraydepth = ival;
			flag_maxraydepth = 1;
			
			break;

		case 'd':

			/* Turn on debug mode */
			debug_mode = 1;
			printf("[lucille] Debug mode on.\n");

			break;

		case 'b':
			/* Verbose mode. print more message. */
			ri_log_set_debug(1);

			break;

		default:
			break;
		}
	}	

	if (optind < argc) {
		/* Might be a RIB file... */
		ribfile = argv[optind];
	}
}

static void
info()
{
	printf("lucille renderer shell.\n");
#ifdef __VERSION__
	printf("  Version        : %s (compiled on %s with %s)\n", version, compile, __VERSION__);
#else
	printf("  Version        : %s (compiled on %s)\n", version, compile);
#endif
	printf("  Multi-threaded : ");

	if (ri_thread_supported()) {
		printf("On\n");
	} else {
		printf("Off\n");
	}

	printf("  Build options  : ");

#if !defined(NDEBUG) || defined(DEBUG)
	printf("Debug build");
#else
	printf("Release build");
#endif

#if defined(__APPLE__) && defined(__MACH__)
	printf(", AltiVec ");
#ifdef WITH_ALTIVEC
	printf("[on] ");
#else
	printf("[off] ");
#endif
#endif

#if defined(WIN32) || defined(LINUX)
	printf(", SSE ");
#ifdef WITH_SSE
	printf("[on] ");
#else
	printf("[off] ");
#endif
#endif

	printf("\n\n");
	printf("  Usage: lsh [OPTIONS] RIBFILE\n");
	printf("\n\n");
}

static void
usage()
{
	printf("  Example: lsh mudah.rib\n");
	printf("\n");
	printf("  To show available display drivers in this build, type 'lsh --info'\n");
	printf("\n");
}

static void
option_list()
{
	printf("  [OPTIONS]\n\n");
	printf("    --info            Print information.\n");
	printf("    --help            Print this help.\n");
	printf("    --version         Show version and help.\n");
	printf("    --verbose         Verbose mode.\n");
	printf("    --debug           Run in debug mode.\n");
	printf("    --pixelsamples N  Samples per pixel(max = 16).\n");
	printf("    --maxraydepth N   Maximum ray depth(max = 16).\n");
	printf("\n");
}

static void
show_ddlist()
{
	printf("  The list of Display Drivers available.\n");
	printf("\n");
	ri_hash_traverse(ri_render_get()->display_drv, show_ddlist_func, NULL);
	printf("\n");
}

static void
show_ddlist_func(void *data, void *userdata)
{
	ri_display_drv_t *drv;	

	if (data == NULL) return;

	drv = (ri_display_drv_t *)data;

	printf("    %-12s  -  %s\n", drv->name, drv->info);
}


