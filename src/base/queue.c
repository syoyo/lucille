/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * 64-bit Lock-free queue implementation from,
 *
 * Simon Doherty, Maurice Herlihy, Victor Luchangco and Mark Moir.
 * Bringing practical lock-free synchronization to 64-bit applications.
 * PODC 2004. pp. 31--39.
 */

/*
 * The code can work on x86-64 and x86-32 for now.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>

#include "memory.h"
#include "queue.h"
#include "atomic.h"

//#define LOCAL_DEBUG
//#define LOCAL_VERBOSE

#if defined(__x86__)

/*
 * Macros
 */
#define CURRENT(loc, ver) ( ((ver) % 2) == 0 ? (loc)->ptr0 : (loc)->ptr1)
#define CLEAN(ext) ( ((ext).count == 0) && ((ext).transfers_left == 0) )
#define FREEABLE(ext) ( CLEAN(ext) && ext.nlp && ext.to_be_freed )
#define CASTTOUINT64(val) (*((uint64_t *)&(val)))

/* return the address of the pointer */
void *NONCURADDRPTR(ri_llsc_var_t *loc, int ver)
{
	if ((ver % 2) == 0) {
		return (void *)&(loc->ptr1);
	} else {
		return (void *)&(loc->ptr0);
	}
}

/*
 * Static vars
 */
ri_exit_tag_t INIT_EXIT = {0, 2, 0, 0};


/*
 * note: myver and mynode are theread local variables.
 */

static ri_queue_node_t *ll_op(
	ri_llsc_var_t *loc,
	int *myver,
	ri_queue_node_t **mynode);

static int sc_op(
	ri_llsc_var_t *loc,
	ri_queue_node_t *nd,
	int ver,
	ri_queue_node_t *pnode);

static void transfer_op(
	ri_queue_node_t *nd,
	int count);

static void release_op(
	ri_queue_node_t *nd);

static void set_nlpred(
	ri_queue_node_t *pred_nd);

/* Load-Linked opration */
ri_queue_node_t *
ll_op(	ri_llsc_var_t     *loc,
	int               *myver,		/* [out] */
	ri_queue_node_t  **mynode)		/* [out] */
{
	ri_entry_tag_t e, n;

	do {
		e = loc->entry;

		(*myver) = e.ver;
		(*mynode) = CURRENT(loc, e.ver);

		n.ver = e.ver;
		n.count = e.count + 1;

	} while (!RI_ATOMIC_CAS64(&loc->entry, CASTTOUINT64(e), CASTTOUINT64(n)));

	return (*mynode);
}


/* Store-Conditional opration */
int sc_op(
	ri_llsc_var_t   *loc,
	ri_queue_node_t *nd, 
	int              ver,	/* tls var */
	ri_queue_node_t *pnode)	/* tls var */
{
	
	int              success;
	ri_entry_tag_t   e, n;

	ri_queue_node_t *pred_nd;
	void            *addr;

	pred_nd = pnode->pred;

	addr = NONCURADDRPTR(loc, ver);

#ifdef __X86_64__
	success = RI_ATOMIC_CAS64(addr, (uintptr_t)pred_nd, (uintptr_t)nd);
#else
	success = RI_ATOMIC_CAS32(addr, (uintptr_t)pred_nd, (uintptr_t)nd);
#endif

	while ((e = loc->entry).ver == ver) {
		n.ver = e.ver + 1;
		n.count = 0;

		if (RI_ATOMIC_CAS64(&loc->entry,
				     CASTTOUINT64(e), CASTTOUINT64(n))) {
			transfer_op(pnode, e.count);
		}
	}

	release_op(pnode);

	return success;
}

void
transfer_op(ri_queue_node_t *nd, int count)
{
	ri_exit_tag_t pre, post;

	do {
		pre = nd->exit;

		post.count = pre.count + count;
		post.transfers_left = pre.transfers_left - 1;
		post.nlp = pre.nlp;
		post.to_be_freed = pre.to_be_freed;

	} while (!RI_ATOMIC_CAS64(&nd->exit,
				   CASTTOUINT64(pre), CASTTOUINT64(post)));
}

void
release_op(ri_queue_node_t *nd)
{
	ri_queue_node_t *pred_nd;
	ri_exit_tag_t pre, post;

	pred_nd = nd->pred;

	do {
		pre = nd->exit;

		post.count = pre.count - 1;
		post.transfers_left = pre.transfers_left;
		post.nlp = pre.nlp;
		post.to_be_freed = pre.to_be_freed;

	} while (!RI_ATOMIC_CAS64(&nd->exit,
				   CASTTOUINT64(pre), CASTTOUINT64(post)));

	if (CLEAN(post)) set_nlpred(pred_nd);
	if (FREEABLE(post)) {
		ri_mem_free_aligned(nd->data);
		ri_mem_free_aligned(nd);
	}
}


void
set_nlpred(ri_queue_node_t *nd)
{
	ri_exit_tag_t pre, post;

	do {
		pre = nd->exit;

		post.count = pre.count;
		post.transfers_left = pre.transfers_left;
		post.nlp = 1;	// true
		post.to_be_freed = pre.to_be_freed;

	// update for <.nlp, .to_be_freed> only
	} while (!RI_ATOMIC_CAS64(&(nd->exit.nlp),
				   CASTTOUINT64(pre.nlp),
				   CASTTOUINT64(post.nlp)));

	if (FREEABLE(post)) {
		ri_mem_free_aligned(nd->data);
		ri_mem_free_aligned(nd);
	}
}

void
set_to_be_freed(ri_queue_node_t *nd)
{
	ri_exit_tag_t pre, post;

	do {
		pre = nd->exit;

		post.count = pre.count;
		post.transfers_left = pre.transfers_left;
		post.nlp = pre.nlp;
		post.to_be_freed = 1;	// true

	// update for <.nlp, .to_be_freed> only
	} while (!RI_ATOMIC_CAS64(&(nd->exit.nlp),
				   CASTTOUINT64(pre.nlp),
				   CASTTOUINT64(post.nlp)));

	if (FREEABLE(post)) {
		ri_mem_free_aligned(nd->data);
		ri_mem_free_aligned(nd);
	}
}

/*
 * Function: ri_queue_push
 *
 *     Enqueues data into the lock-free queue.
 *
 * Parameters:
 *
 *     q         - lock-free queue object. 
 *     data      - data to be enqueued. 
 *     data_size - size of data. 
 *     out_ver   - specify thread-local storage. 
 *     out_node  - specify thread-local storage. 
 *
 * Returns:
 *
 *     0 if enqueueing successes, -1 if not.
 *                     
 *
 */
int
ri_queue_push(
	ri_queue_t       *q,
	const void       *data,
	unsigned int      data_size,
        int              *out_ver,	/* [out] stored in thread-local storage */
	ri_queue_node_t **out_node)	/* [out] stored in thread-local storage */
{

	ri_queue_node_t *nd;
	ri_queue_node_t *tail;

	int nelems;
	
	nelems = ri_atomic_read(&q->nelems);

	/*
	 * Note that there is a possibility that the queue has
	 * `max_queue_size' + O(m) elements in maximum, where m is the number 
	 * of threads currently operating the queue.
	 * Thus, we can not acculately limit the number of elements in the
	 * queue to `max_queue_size'.
	 *
	 */
	if (nelems >= q->max_queue_size) {
		return -1;	/* fail */
	}

	nd = ri_mem_alloc_aligned(sizeof(ri_queue_node_t), 16);
	assert(nd != 0);	

	nd->data = ri_mem_alloc_aligned(data_size, 16);
	assert(nd->data != 0);	

	memcpy(nd->data, data, data_size);
	nd->data_size = data_size;

	nd->next = NULL;
	nd->exit = INIT_EXIT;

	while (1) {

		tail = ll_op(&q->tail, out_ver, out_node);

		nd->pred = tail;

#ifdef __X86_64__
		if (RI_ATOMIC_CAS64(&tail->next, (uintptr_t)NULL, (uintptr_t)nd)) {
#else
		if (RI_ATOMIC_CAS32(&tail->next, (uintptr_t)NULL, (uintptr_t)nd)) {
#endif
			sc_op(&q->tail, nd, (*out_ver), (*out_node));

			ri_atomic_inc(&q->nelems);

			return 0;	// OK
		} else {

			sc_op(&q->tail, tail->next, (*out_ver), (*out_node));
		}
	}
}


/*
 * Function: ri_queue_pop
 *
 *     Dequeues data from the lock-free queue.
 *
 * Parameters:
 *
 *     queue         - lock-free queue object. 
 *     out_data      - pointer where the dequeued data is wrote. 
 *     out_data_size - pointer whwre the size of dequeued data is wrote. 
 *     pver          - specify thread-local storage. 
 *     pnode         - specify thread-local storage. 
 *
 * Returns:
 *
 *     0 if dequeueing successes, -1 if not.
 *                     
 *
 */

int
ri_queue_pop(
	ri_queue_t       *queue,
	void             *out_data,
	unsigned int     *out_data_size,
    int              *pver,
	ri_queue_node_t **pnode)
{

	ri_queue_node_t *head;
	ri_queue_node_t *next;

	while (1) {

		head = ll_op(&queue->head, pver, pnode);
		next = head->next;

		if (next == NULL) {
			release_op((*pnode));
			return -1;	/* fail */
		}

		if (sc_op(&queue->head, next, (*pver), (*pnode))) {

			memcpy(out_data, next->data, next->data_size);
			(*out_data_size) = next->data_size;

			set_to_be_freed(next);

			ri_atomic_dec(&queue->nelems);

			return 0;	/* OK */
		}
	}
}

/*
 * Function: ri_queue_new
 *
 *     Creates lock-free queue object.
 *
 * Parameters:
 *
 *     max_queue_size - approximately specify maximum elements in the queue. 
 *
 * Returns:
 *
 *     created lock-free queue object.
 *                     
 *
 */
ri_queue_t *
ri_queue_new(int max_queue_size)
{
	ri_queue_t *q;

	q = ri_mem_alloc_aligned(sizeof(ri_queue_t), 16);

	q->tail.entry.ver = 0;
	q->tail.entry.count = 0;

	q->tail.ptr0 = (ri_queue_node_t *)ri_mem_alloc_aligned(
                        sizeof(ri_queue_node_t), 16);
	q->tail.ptr1 = (ri_queue_node_t *)ri_mem_alloc_aligned(
                        sizeof(ri_queue_node_t), 16);

	q->tail.ptr0->pred = q->tail.ptr1;

	q->tail.ptr0->exit.count          = 0;
	q->tail.ptr0->exit.transfers_left = 2;
	q->tail.ptr0->exit.nlp            = 0;
	q->tail.ptr0->exit.to_be_freed    = 0;
	q->tail.ptr0->next                = NULL;

	q->tail.ptr1->exit.count          = 0;
	q->tail.ptr1->exit.transfers_left = 0;
	q->tail.ptr1->exit.nlp            = 0;
	q->tail.ptr1->exit.to_be_freed    = 0;
	
	q->head = q->tail;

	q->max_queue_size = max_queue_size;
	
	q->nelems = 0;


	return q;
}

void
ri_queue_free(
	ri_queue_t *queue)
{
	ri_mem_free_aligned(queue->tail.ptr0);
	ri_mem_free_aligned(queue->tail.ptr1);

	ri_mem_free_aligned(queue);

}

#endif  /* __x86__ */

/* -------------------------------------------------------------------------
 *
 * MT queue. 
 * Use pthread for concurrency. Works well on any machine where  pthread is 
 * supported.
 *
 * ------------------------------------------------------------------------- */

typedef struct _mt_queue_item_t
{

    void     *data;
    uint32_t  len;

} mt_queue_item_t;


ri_mt_queue_t *
ri_mt_queue_new()
{

    // ri_mt_queue_new() is not MT-safe.

    ri_mt_queue_t *q;

    q           = ri_mem_alloc(sizeof(ri_mt_queue_t));
    q->mutex    = ri_mutex_new(); 
    q->nodes    = ri_list_new(); 
    q->nnodes   = 0; 

    ri_mutex_init(q->mutex);

    return q;
}

int
ri_mt_queue_push(
    ri_mt_queue_t *queue,
    const void    *data,
    uint32_t       size)
{
    assert(queue != NULL);
    assert(data  != NULL);
    assert(size > 4);

    ri_mutex_lock(queue->mutex);

    {
        // Alloate memory and copy content
        mt_queue_item_t *item;

        item       = ri_mem_alloc(sizeof(mt_queue_item_t));
        item->data = ri_mem_alloc(size);
        item->len  = size;

        memcpy(item->data, data, size);

        ri_list_append(queue->nodes, item);

        queue->nnodes++; 
    }

    ri_mutex_unlock(queue->mutex);

    return 0;   // OK

}


int
ri_mt_queue_pop(
    ri_mt_queue_t  *queue,
    void          **data_out,
    uint32_t       *data_size_out)
{
    assert(queue         != NULL);
    assert(data_out      != NULL);
    assert(data_size_out != NULL);

    ri_mutex_lock(queue->mutex);

    {
        ri_list_t       *l;
        mt_queue_item_t *item;

        l = ri_list_first(queue->nodes);
        if (l == NULL) {
            ri_mutex_unlock(queue->mutex);
            return -1;           /* empty */
        } 

        item = (mt_queue_item_t *)(l->data);

        (*data_out)      = item->data;
        (*data_size_out) = item->len; 

        /*
         * Notice: ri_list_remove_first() does not free memory pointed by
         * l->data.
         */
        queue->nodes = ri_list_remove_first(queue->nodes);

        queue->nnodes--; 

    }

    ri_mutex_unlock(queue->mutex);

    return 0;
}

/*
 * Returns number of items remain in this queue.
 */
int
ri_mt_queue_len(
    const ri_mt_queue_t  *queue)
{
    int nnodes;

    ri_mutex_lock(queue->mutex);

    {
        nnodes = queue->nnodes;
    }

    ri_mutex_unlock(queue->mutex);

    return nnodes;
}

int
ri_mt_queue_free(
    ri_mt_queue_t  *queue)
{

    // ri_mt_queue_free() is not MT-safe.

    ri_mutex_free(queue->mutex);
    ri_list_free(queue->nodes);
    ri_mem_free(queue);

    return 0;
} 



#ifdef TEST_QUEUE

/* ----------------------------------------------------------------------------
 *
 * TEST code for lock-free queue.
 *
 * ------------------------------------------------------------------------- */

typedef struct _thread_local_var_t
{
	ri_queue_node_t *mynode __attribute__((aligned(16)));
	int myver;
	int pad;

} thread_local_var_t __attribute__((aligned(16)));


#define MAXELEMS 1000000
#define MAXTHREADS 300

ri_queue_t *q;
pthread_t tid[MAXTHREADS];
thread_local_var_t tlv[MAXTHREADS];
int64_t sum[MAXTHREADS];
int64_t checkForDequeue[MAXELEMS+1];
int thread_ids[MAXTHREADS];
int nthreads;
int nelems;
int nstep;

void *
thread_func(void *arg)
{
	int i;
	int ret;
	uint32_t tid;
	int64_t putval;
	int64_t getval;
	unsigned int size;

	tid = *(uint32_t *)arg;

	printf("tid = %d\n", tid);

	sum[tid] = 0;
	for (i = 0; i < nstep; i++) {

		// generate the unique value among all threads.	
		putval = nstep * tid + i + 1;

#ifdef LOCAL_VERBOSE
		printf("thread[%ld] enqueue: %ld\n", tid, putval);
#endif
		ri_queue_push(q, (const void *)&putval, sizeof(uint64_t), &(tlv[tid].myver), &(tlv[tid].mynode));

		//usleep(tid * 10);

		ret = ri_queue_pop(q, &getval, &size, &(tlv[tid].myver), &(tlv[tid].mynode));
		if (ret < 0) {
			printf("???\n");
			exit(-1);
		}
		
#ifdef LOCAL_VERBOSE
		printf("dequeue: val = %ld\n", getval);
		printf("dequeue: size = %d\n", size);
#endif
		sum[tid] += getval;
		
		// mark for cheking
		checkForDequeue[getval]++;	
	}
}

void
test()
{
	int ret;
	int err;
	uint32_t i;
	int64_t total;
	int64_t expected;

	struct timeval startTime, endTime;
	double elapsed;

	nstep = nelems / nthreads;

	q = ri_queue_new(0);

	for (i = 1; i < nelems+1; i++) {
		checkForDequeue[i] = 0;
	}

	gettimeofday(&startTime, NULL);

	i = 0;
	for (i = 0; i < nthreads; i++) {
		thread_ids[i] = i;
		ret = pthread_create(&tid[i], NULL, thread_func, (void *)&thread_ids[i]);
		if (ret != 0) {
			fprintf(stderr, "thread create err.\n");
			exit(-1);
		}
	}

	for (i = 0; i < nthreads; i++) {
		pthread_join(tid[i], NULL);
	}

	gettimeofday(&endTime, NULL);

	elapsed = (double)(endTime.tv_sec - startTime.tv_sec) +
                  (double)(endTime.tv_usec - startTime.tv_usec) / (double)1.0e6;

	printf("nthreads = %d: elapsed = %f sec\n", nthreads, elapsed);
	printf("done\n");

	// check for mark
	err = 0;
	for (i = 1; i < nelems+1; i++) {
		if (checkForDequeue[i] != 1) {
			printf("err: checkForDequeue[%d] must be 1, but got %ld\n",
				i, checkForDequeue[i]);
			err = 1;
		}
	}

	if (err) exit(-1);

	// calc total sum
	// it must equal to (nelems + 1) * (nelems / 2)
	total = 0;
	for (i = 0; i < nthreads; i++) {
		total += sum[i];
	}

	expected = (int64_t)((nelems + 1) * (nelems * 0.5));

	printf("sum = %ld\n", total);
	printf("expected = %ld\n", expected);

	if (expected == total) {
		printf("test OK!\n");
	} else {
		printf("test FAILED!\n");
	}
	
}


int
main(int argc, char **argv)
{
	printf("usage: test <nthreads> <nelems>\n");

	nthreads = 100;
	nelems = MAXELEMS;


	if (argc > 1) {
		nthreads = atoi(argv[1]);	
		if (nthreads > MAXTHREADS) {
			nthreads = MAXTHREADS;
		}
	}

	if (argc > 2) {
		nelems = atoi(argv[2]);	
		if (nelems > MAXELEMS) {
			nelems > MAXELEMS;
		}
	}

	if ((nelems % nthreads) != 0) {
		printf("nelems should be a multiple of nthreads\n");
		printf("  nelems = %d, nthreads = %d\n",
			nelems, nthreads);
		exit(-1);
	}

	printf("queue test: use %d threads and %d elems\n", nthreads, nelems);
#ifdef __X86_64__
	printf("  64-bit mode\n");
#else
	printf("  32-bit mode\n");
#endif

	test();
}
#endif	// TEST_QUEUE
