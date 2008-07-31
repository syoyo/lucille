#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#ifdef WITH_READLINE
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/errno.h>
#ifdef __cplusplus
extern "C" {
#endif
#include <readline/readline.h>
#include <readline/history.h>
#ifdef __cplusplus
}
#endif
#endif

#include "ri.h"

#include "memory.h"
#include "lsh.h"
#include "matrix.h"
#include "stack.h"
#include "render.h"
#include "option.h"


typedef struct {
	char *name;		/* user printable name of the function	*/
#ifdef WITH_READLINE
	rl_icpfunc_t *func;	/* function to call to do the job	*/
#else
	int (*func)(char *arg);
#endif
	char *doc;		/* help message				*/
} command_t;

static FILE *ribfile;

int command_file(char *arg);
int command_render(char *arg);
int command_quit(char *arg);
int command_nsamples(char *arg);
int command_nphotons(char *arg);
int command_set(char *arg);
int command_stat(char *arg);
int command_matrix(char *arg);
int command_maxdepth(char *arg);

command_t commands[] = {
	{ "file", command_file, "RIB file to render" },
	{ "render", command_render, "render frame" },
	{ "quit", command_quit, "quit shell" },
	{ "nsamples", command_nsamples, "number of samples per pixel" },
	{ "nphotons", command_nphotons, "number of photons to shoot" },
	{ "set", command_set, "set variable" },
	{ "stat", command_stat, "show current status" },
	{ "matrix", command_matrix, "show current transformation matrix"},
	{ "maxdepth", command_maxdepth, "show option"},
	{ (char *)NULL, NULL, (char *)NULL }
};

#ifdef WITH_READLINE
static char       *stripwhite(char *str);
static int         exec_line(char *line);
static char      **completion(const char *text, int start, int end);
static char       *command_generator(const char *text, int state);
static command_t  *find_command(char *name);
static int         is_whitespace(char s);
#endif

lsh_t *
lsh_new()
{
	lsh_t *p = NULL;

	p = (lsh_t *)ri_mem_alloc(sizeof(lsh_t));
	
	/* todo: write initialize code */
	
	ribfile = NULL;

#ifdef WITH_READLINE
	rl_readline_name = "lsh";
	rl_attempted_completion_function = completion;
#endif
	return p;
}

void
lsh_free(lsh_t *lsh)
{
	/* todo: write freeing code */

	free(lsh);
}

void
lsh_exec(lsh_t *lsh)
{
#ifdef WITH_READLINE
	int ret;
	char *line, *s;
#endif

	RiBegin(RI_NULL);

#ifdef WITH_READLINE
	while (1) {
		printf("Example usage:\n");
		printf("lsh: file RIBFILE  -> specify RIB file to render\n");
		printf("lsh: render        -> start rendering\n");
		printf("\n");

		line = readline("lsh: ");

		if (!line) break;

		s = stripwhite(line);

		if (*s) {
			add_history(s);
			ret = exec_line(s);
			if (ret < 0) break;
		}

		free(line);
	}
#endif

	RiEnd();

	(void)lsh;		/* for gcc */
}

/* --- private functions --- */

#ifdef WITH_READLINE
/* strip whitespace from the start and end of string. */
char *
stripwhite(char *str)
{
	char *s, *t;

	for (s = str; is_whitespace(*s); s++) ;

	if (*s == 0) return s;

	t = s + strlen(s) - 1;
	
	while (t > s && is_whitespace(*t)) t--;

	*(++t) = '\0';
	
	return s;
}

int 
exec_line(char *line)
{
	int        i;
	char      *word;
	command_t *command;

	i = 0;
	while (line[i] && is_whitespace(line[i])) i++;

	word = line + i;
	
	while (line[i] && !is_whitespace(line[i])) i++;

	if (line[i]) line[i++] = '\0';

	command = find_command(word);

	if (strcmp(word, "q") == 0) return -1;
	if (strcmp(word, "quit") == 0) return -1;
	if (strcmp(word, "exit") == 0) return -1;

	if (!command) {
		printf("no such command\n");
		return 0;
	} else {
		while (is_whitespace(line[i])) i++;

		word = line + i;

		return (*(command->func))(word);
	}
}

char **
completion(const char *text, int start, int end)
{
	char **matches;

	matches = (char **)NULL;

	/*
	 * If this word is at the start of the line, then it is a command
	 * to complete. 
	 */
	if (start == 0) {
		matches = rl_completion_matches(text, command_generator);
	}

	(void)end;
	return matches;	
}

char *
command_generator(const char *text, int state)
{
	static int list_index, len;

	/*
	 * If this is a new word to complete, initialize now.
	 * This includes saving the length of TEXT for efficiency, and
	 * initializing the index variable to 0.
	 */
	if (!state) {
		list_index = 0;
		len = strlen(text);
	}

	/*
	 * Return the next name which partially matches from the command 
	 * list.
	 */
	while (commands[list_index].name) {
		list_index++;

		if (strncmp(commands[list_index].name, text, len) == 0) {
			return strdup(commands[list_index].name);
		}
	}

	/* No name matched. */
	return (char *)NULL;
}

command_t *
find_command(char *name)
{
	int i;

	for (i = 0; commands[i].name; i++) {
		if (strcmp(name, commands[i].name) == 0) {
			return &commands[i];
		}
	}

	return (command_t *)NULL;
}

int
is_whitespace(char s)
{
	if (s == ' ' || s == '\t') return 1;
	return 0;
}

#endif	/* WITH_READLINE */

int
command_file(char *arg)
{
	ribfile = fopen(arg, "r");
	if (!ribfile) {
		printf("Err: [ %s ] No such file.\n", arg);
		return 0;
	}

		

	return 0;
}


int
command_render(char *arg)
{
	extern FILE *yyin;
	extern void yyparse();
	extern int  yydebug;

	if (!ribfile) {
		printf("no RIB file specified!\n");
		return 0;
	}

	yyin = ribfile;
	
	if (strcmp(arg, "-d") == 0) {
		yydebug = 1;
	} else {
		yydebug = 0;
	}	
	yydebug = 1; // hack

	fseek(yyin, 0, SEEK_SET);
	yyparse();

	yyin = NULL;

	printf("rendered\n");

	return 0;
}

int
command_quit(char *arg)
{
	if (ribfile) {
		fclose(ribfile);
	}

	(void)arg;
	printf("quit\n");
	return -1;
}

int
command_nsamples(char *arg)
{
	printf("nsamples with %s\n", arg);
	(void)arg;
	return 0;
}

int
command_nphotons(char *arg)
{
	printf("nphotons with %s\n", arg);
	(void)arg;
	return 0;
}

int
command_set(char *arg)
{
	printf("set with %s\n", arg);
	(void)arg;
	return 0;
}

int
command_stat(char *arg)
{
	printf("stati with %s\n", arg);

	return 0;
}

int
command_matrix(char *arg)
{
	ri_matrix_t *m;
	
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_print(m);

	(void)arg;

	return 0;
}

int
command_maxdepth(char *arg)
{
	int *d;

	if (ri_impl_option_get("raytrace_max_depth")) {
		d = (int *)ri_impl_option_get("raytrace_max_depth");
		printf("ray_max_depth = [ %d ]\n", *d);	
	}


	(void)arg;

	return 0;
}

