/*
 *
 * ri_mt_queue_t: mutli-thread safe queue.
 *
 * ri_queue_t   : 64-bit clean lock-free queue algorithm.
 * 
 * x86_64 and x86_32 archtectures only.
 *
 * reference:
 *
 * Simon Doherty, Maurice Herlihy, Victor Luchangco and Mark Moir.
 * Bringing practical lock-free synchronization to 64-bit applications.
 * PODC 2004. pp. 31--39.
 *
 * $Id: queue.h 268 2007-03-19 15:40:46Z lucille $
 */

#ifndef QUEUE_H
#define QUEUE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "thread.h"
#include "list.h"

typedef struct _ri_entry_tag_t
{
    int ver;
    int count;

} ri_entry_tag_t;

typedef struct _ri_exit_tag_t
{
    int count;
    int transfers_left;            /* 64 bits        */

    int nlp;
    int to_be_freed;            /* another 64 bits    */

} ri_exit_tag_t;


/*
 * queue node
 */
typedef struct _ri_queue_node_t
{
    void         *data;            /* content        */
    unsigned int  data_size;        /* content data size    */

    struct _ri_queue_node_t *next;
    struct _ri_queue_node_t *pred;

    ri_exit_tag_t exit;

} ri_queue_node_t;

typedef struct _ri_llsc_var_t
{
    ri_queue_node_t *ptr0, *ptr1;
    ri_entry_tag_t entry;

} ri_llsc_var_t;

typedef struct _ri_queue_t
{
    ri_llsc_var_t head;
    ri_llsc_var_t tail;

    int           max_queue_size;

    /* # of elements in the queue. */
    int           nelems;

} ri_queue_t;

typedef struct _ri_mt_queue_t
{
    ri_mutex_t *mutex;
    ri_list_t  *nodes;
    
} ri_mt_queue_t;


extern ri_queue_t * ri_queue_new();

extern int ri_queue_push(
        ri_queue_t       *queue,            /* [in]    */
        const void       *data,             /* [in]    */
        unsigned int      data_size,        /* [in]    */
        int              *out_ver,          /* [inout] */
        ri_queue_node_t **out_node);        /* [inout] */

extern int ri_queue_pop(
        ri_queue_t       *queue,            /* [in]    */
        void             *out_data,         /* [out]   */
        unsigned int     *out_data_size,    /* [out]   */
        int              *pver,             /* [inout] */
        ri_queue_node_t **pnode);           /* [inout] */

extern void ri_queue_free(
        ri_queue_t       *queue);           /* [in]  */


//
// MT queue
//

extern ri_mt_queue_t *ri_mt_queue_new();

extern int            ri_mt_queue_push(
                            ri_mt_queue_t  *queue,           /* [in]  */
                            const void     *data,            /* [in]  */
                            uint32_t        data_size);      /* [in]  */

extern int            ri_mt_queue_pop (
                            ri_mt_queue_t  *queue,           /* [in]  */ 
                            void          **data_out,        /* [out] */
                            uint32_t       *data_size_out);  /* [out] */

extern int            ri_mt_queue_free(
                            ri_mt_queue_t  *queue);          /* [in]  */

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif

