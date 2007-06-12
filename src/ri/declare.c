#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "declare.h"

RtToken ri_api_declare(char *name, char *declaration)
{
	ri_hash_t *dec;

	dec = ri_render_get()->context->declares;

	ri_hash_insert(dec, name, (void *)declaration);

	return NULL;
}

