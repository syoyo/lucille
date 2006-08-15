/*
 * dynamically expanded array.
 *
 * $Id: array.h,v 1.2 2004/01/31 08:30:04 syoyo Exp $
 */

#ifndef ARRAY_H
#define ARRAY_H


#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_array_t
{
	char 	     *data;		/* array of data        */
	unsigned int  element_size;	/* element size	        */	
	unsigned int  nelems;		/* number of elements	*/
	unsigned int  alloc;		/* allocated size       */
} ri_array_t;

typedef struct _ri_ptr_array_t
{
	void         **data;		/* pointer array of data */
	unsigned int   nelems;		/* number of elements	*/
	unsigned int   alloc;		/* allocated size	 */
} ri_ptr_array_t;

extern ri_array_t *ri_array_new   (unsigned int element_size);
extern void	   ri_array_free  (ri_array_t *array);
extern void	   ri_array_insert(ri_array_t *array, unsigned int index,
				   const void *data);
extern const char *ri_array_at    (const ri_array_t *array, unsigned int index);
extern void         ri_array_remove_at(ri_array_t *array,
				       unsigned int index);


extern ri_ptr_array_t *ri_ptr_array_new   ();
extern void	       ri_ptr_array_free  (ri_ptr_array_t *array);
extern void	       ri_ptr_array_insert(ri_ptr_array_t *array,
				           unsigned int    index,
				           void           *data);
extern void           *ri_ptr_array_at    (ri_ptr_array_t *array,
				           unsigned int    index);
extern void            ri_ptr_array_traverse(ri_ptr_array_t *array,
					     void (*travfunc)(void *data));
extern void            ri_ptr_array_remove(ri_ptr_array_t *array,
					   void *data);
extern void            ri_ptr_array_remove_at(ri_ptr_array_t *array,
					      unsigned int index);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
 
