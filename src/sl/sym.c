#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sym.h"
#include "tree.h"
#include "parsesl.h"

typedef struct _shadervar_info_t
{
	char *name;
	int   type;
} shadervar_info_t;

static shadervar_info_t shadervars[] = {
	{ "Cs", COLOR  },
	{ "Os", COLOR  },
	{ "P",  POINT  },
	{ "N",  NORMAL },
	{ "s",  FLOAT  },
	{ "t",  FLOAT  },
	{ "I",  VECTOR },
	{ "Ci", COLOR  },
	{ "Oi", COLOR  },
	{ "L",  VECTOR },
	{ "Cl", COLOR  },
	{ NULL, 0      },
};

#define CLASS_VAR 0

#define HASHTABLE_SIZE 131
static sym_t *hashtable[HASHTABLE_SIZE];

static unsigned int  hash(const char *str);
static sym_t        *sym_reg(char *name, int type, int class);
static int           isshadervar(char *name, int *type);

sym_t *
var_reg(char *name, int type)
{
	sym_t *p;
	int    vartype;

	p = lookup_sym(name);

	if (p == NULL) {		
		if (isshadervar(name, &vartype)) {
			p = sym_reg(name, vartype, CLASS_VAR);
		} else {
			p = sym_reg(name, type, CLASS_VAR);
		}
	} else {
		/* duplicated declaration */
		return NULL;
	}

	return p;
}

sym_t *
lookup_sym(char *name)
{
	unsigned int  h;
	sym_t        *p;

	h = hash(name);
	p = hashtable[h];

	for (; p != NULL && strcmp(p->name, name) != 0; p = p->next);

	return p;
}

/* --- private functions --- */

static unsigned int
hash(const char *str)
{
        const char *p = str;
        unsigned long h = *p;

        if (h) {
                for (p += 1; *p != '\0'; p++) {
                        h = (h << 5) - h + *p;
                }
        }

        return h % HASHTABLE_SIZE;
}

static sym_t *
sym_reg(char *name, int type, int class)
{
	unsigned int  h;
	sym_t        *p;

	h = hash((const char *)name);

	p = (sym_t *)malloc(sizeof(sym_t));
	p->name  = name;
	p->type  = type;
	p->class = class;

	p->next      = hashtable[h];
	hashtable[h] = p;

	return p;
}

int
isshadervar(char *name, int *type)
{
	int i;

	i = 0;
	while (shadervars[i].name != NULL) {
		if (strcmp(shadervars[i].name, name) == 0) {
			*type = shadervars[i].type;
			return 1;
		}

		i++;
	}

	return 0;
}
