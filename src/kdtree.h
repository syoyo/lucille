#ifndef KDTREE_H
#define KDTREE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _bbox_t
{
	float bmin[3];
	float bmax[3];
} bbox_t;

typedef struct _tri_info_t
{
	unsigned int  index;	/* vertex index		*/
	ri_geom_t    *geom;	/* reference for geom	*/
	ri_vector_t   v[3];	/* triangle vertives	*/
} tri_info_t;

// Stack for traversal.
typedef struct _trav_stack_t
{
	float ray_min;
	float ray_max;
	unsigned int tnode;
} trav_stack_t;

typedef struct _ri_kdtree_t
{
	unsigned int   ncachelines;	/* # of cache line sized mem segs  */
	unsigned int   ncachelinenodes;	/* # of nodes per $ line	   */
	unsigned int   ncachelinebytes;	/* # of bytes per $ line           */
	unsigned int   depth;		/* tree depth			   */

	bbox_t         bbox;		/* bounding box 		   */

	float         *enode_table;	/* memory allocated for nodes      */
	float         *node_table;	/* Node array. aligned on $ lines  */

	tri_info_t   **objects;		/* object list			   */
	unsigned int   nobjs;		/* # of objects			   */

	// stack to implement the recursive traversal algorithm
	trav_stack_t  *trav_stack;
	int            stack_ptr;

	/* FIFO */
	unsigned int  *freenodes;
	unsigned int   nfreenodes;
	unsigned int   max_freenodes;
	unsigned int   last_freenode_index;
	unsigned int   first_freenode_index;

	int            maximum_depth;	/* maximum depth of tree */
} ri_kdtree_t;

typedef struct _ray_t
{
	ri_vector_t org;
	ri_vector_t dir;
} ray_t;

typedef struct hit_t
{
	ri_vector_t   pos;	
	float         t, u, v;
	unsigned int  obj;
	int           is_intersect;
	ri_geom_t    *geom;
	unsigned int  index;
} hit_t;

extern ri_kdtree_t *ri_accel_build_kdtree();
extern int ri_kdtree_traverse(ri_kdtree_t *kd,
			      const ray_t *ray, hit_t *rec,
			      float mint, float maxt);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
