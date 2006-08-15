/*
 * string hash.
 *
 * $Id: hash.h,v 1.3 2004/01/28 08:50:55 syoyo Exp $
 */

#ifndef HASH_H
#define HASH_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_hash_node_t
{
	char                   *key;
	int                     len;	/* strlen(key) */
	void		       *val;
	struct _ri_hash_node_t *next;
} ri_hash_node_t;

typedef struct _ri_hash_t
{
	unsigned int	 size;
	unsigned int	 nnodes;
	ri_hash_node_t **nodes;
} ri_hash_t;

extern ri_hash_t *ri_hash_new     ();
extern void	  ri_hash_free    (ri_hash_t *hash);
extern void	  ri_hash_insert  (ri_hash_t *hash, const char *key,
				   void *data);
extern void	 *ri_hash_lookup  (ri_hash_t *hash, const char *key);
extern void       ri_hash_traverse(ri_hash_t *hash,
				   void (*travfunc)(void *data, void *userdata),
				   void *userdata);
/*
extern void	  ri_hash_remove(ri_hash_t *hash, const char *key);
*/

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

