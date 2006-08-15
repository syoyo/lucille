/*
 * $Id: hash.c,v 1.6 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include "util.h"
#include "memory.h"
#include "hash.h"

/*
 * string <-> integer hash only
 */

static unsigned int     ri_hash_func_priv       (const char *key);
static ri_hash_node_t **ri_hash_node_lookup_priv(ri_hash_t *hash,
						 const char *key);
static void	        ri_hash_resize_priv     (ri_hash_t *hash);

ri_hash_t *
ri_hash_new()
{
	unsigned int  i;
	ri_hash_t    *p;

	p = (ri_hash_t *)ri_mem_alloc(sizeof(ri_hash_t));

	p->size   = ri_util_min_prime(); /* initial hash table size */
	p->nnodes = 0;
	p->nodes  = (ri_hash_node_t **)ri_mem_alloc(sizeof(ri_hash_node_t *)
						    * p->size);

	for (i = 0; i < p->size; i++) {
		p->nodes[i] = NULL;
	}	

	return p;
}

void
ri_hash_free(ri_hash_t *hash)
{
	unsigned int i;
	ri_hash_node_t *p, *q;

	for (i = 0; i < hash->size; i++) {
		if (hash->nodes[i]) {
			p = hash->nodes[i];

			while (p) {
				q = p;
				p = q->next;

				ri_mem_free(q->key);
				ri_mem_free(q);
			}
		}
	}

	ri_mem_free(hash->nodes);
	ri_mem_free(hash);
}

static unsigned int
ri_hash_func_priv(const char *str)
{
	const char *p = str;
	unsigned long h = *p;

	if (h) {
		for (p += 1; *p != '\0'; p++) {
			h = (h << 5) - h + *p;
		}
	}

	return h;
}

static ri_hash_node_t **
ri_hash_node_lookup_priv(ri_hash_t *hash, const char *key)
{
	unsigned int     index;
	int              len;
	ri_hash_node_t **p;

	index = ri_hash_func_priv(key) % hash->size;

	p = &(hash->nodes[index]);

	len = (int)strlen(key);

	/* find node */
	for(; *p != NULL; p = &((*p)->next)) {
		if ((*p)->len == len && (strcmp((*p)->key, key) == 0)) {
			/* found. */
			break;
		}
	}

	return p;
}

void *
ri_hash_lookup(ri_hash_t *hash, const char *key)
{
	ri_hash_node_t *p;

	if (hash->nnodes == 0) return NULL;

	p = *(ri_hash_node_lookup_priv(hash, key));

	return p ? p->val : NULL;
}

void
ri_hash_insert(ri_hash_t *hash, const char *key, void *val)
{
	ri_hash_node_t **p;

	p = ri_hash_node_lookup_priv(hash, key);

	if (*p) {
		(*p)->val = val;
	} else {
		(*p) = (ri_hash_node_t *)ri_mem_alloc(sizeof(ri_hash_node_t));
		(*p)->key  = strdup(key);
		(*p)->len  = (int)strlen((*p)->key);
		(*p)->val  = val;
		(*p)->next = NULL;
		hash->nnodes++;

		if ((hash->size >= 3 * hash->nnodes &&
		     hash->size >  ri_util_min_prime()) ||
		    (hash->size <= hash->nnodes &&
		     hash->size <  ri_util_max_prime())) {
			ri_hash_resize_priv(hash);
		}
	}
}

void
ri_hash_traverse(ri_hash_t *hash, void (*travfunc)(void *data, void *userdata),
		 void *userdata)
{
	unsigned int    i;
	ri_hash_node_t *p;

	for (i = 0; i < hash->size; i++) {
		for (p = hash->nodes[i]; p != NULL; p = p->next) {
			travfunc(p->val, userdata);
		}
	}
}

static void
ri_hash_resize_priv(ri_hash_t *hash)
{
	ri_hash_node_t **newnodes;
	ri_hash_node_t *next;
	ri_hash_node_t *p;

	unsigned int    hashval;
	unsigned int    newsize;
	unsigned int    i;

	newsize = ri_util_closest_prime(hash->nnodes);

	if (newsize > ri_util_max_prime()) {
		newsize = ri_util_max_prime();
	}

	newnodes = (ri_hash_node_t **)ri_mem_alloc(sizeof(ri_hash_node_t *) *
						   newsize);
	for (i = 0; i < newsize; i++) {
		newnodes[i] = NULL;
	}

	for (i = 0; i < hash->size; i++) {
		for (p = hash->nodes[i]; p != NULL; p = next) {
			next = p->next;

			hashval = ri_hash_func_priv(p->key) % newsize;

			p->next = newnodes[hashval];
			newnodes[hashval] = p;
		}
	}

	ri_mem_free(hash->nodes);
	hash->nodes = newnodes;
	hash->size  = newsize;
}
