#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sl2c.h"

char sl2c_verstr[]   = "0.2";
char sl2c_datestr[]  = __DATE__; 
char sl2c_progname[] = "sl2c";

int
main(int argc, char **argv)
{
	extern FILE *yyin;
	extern int   yydebug;
	extern int   yyparse(void);
	char        *ext;
	char        *infile;
	char         cfilename[1024];
	int          len;
	int          output_stdout = 0;

	int          i;

	if (argc < 2) {
		printf("\n");
		printf("RenderMan Shader Language to lucille C Language shader converter.\n");
		printf("  version %s (compiled on %s)\n", sl2c_verstr, sl2c_datestr);
		printf("                                                           \n");
		printf("  Usage: %s [option] FILE.sl\n", sl2c_progname);
		printf("  (By default, translated code is written to FILE.c)       \n");
		printf("                                                           \n");
		printf("    [option]                                               \n");
		printf("                                                           \n");
		printf("      -c     : output to stdout                            \n");
		printf("      -g     : debug                            \n");
		printf("                                                           \n");
		printf("                                                           \n");
		printf("  (Example)                                                \n");
		printf("                                                           \n");
		printf("    %s clouds.sl\n", sl2c_progname);
		printf("\n");
		printf("\n");
		exit(-1);
	}

	if (argc > 2) {
		for (i = 1; i < argc - 1; i++) {

			if (strcmp(argv[i], "-c") == 0) {
				output_stdout = 1;
			} else if (strcmp(argv[i], "-g") == 0) {
				printf("debug");
				yydebug = 1;
			}
		}
	}

	infile = argv[argc - 1];

	ext = strrchr(infile, '.');	/* get file extension string */
	if (ext == NULL) {
		sprintf(cfilename, "%s.c", infile);
	} else {
		len = ext - argv[1];

		strncpy(cfilename, infile, len);
		strncpy(&cfilename[0] + len, ".c", strlen(".c"));
		strncpy(&cfilename[0] + len, ".c", strlen(".c"));
		cfilename[len+strlen(".c")] = '\0';
	}

	if (!output_stdout) {
		g_csfp = fopen(cfilename, "w");
		if (!g_csfp) {
			fprintf(stderr, "Can't open [ %s ] to write.\n", cfilename);
			exit(-1);
		}
	} else {
		g_csfp = stdout;
	}

	yyin = fopen(infile, "r");

	//yydebug = 1;

	yyparse();

	if (!output_stdout) {
		fclose(g_csfp);
	}

	return 0;
}
