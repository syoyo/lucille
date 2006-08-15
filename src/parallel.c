#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#include "memory.h"
#include "parallel.h"

static int ntasks;
static int taskid;

void
ri_parallel_init(int *argc, char ***argv)
{
#ifdef WITH_MPI
	MPI_Init(argc, argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &taskid);
	MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

	printf("ntasks: %d\n", ntasks);
#else
	ntasks = 1;
	taskid = 0;
	(void)argc;
	(void)argv;
#endif
}

void
ri_parallel_finalize()
{
#ifdef WITH_MPI
	MPI_Finalize();
#endif
}

int
ri_parallel_ntasks()
{
	return ntasks;
}

int
ri_parallel_taskid()
{
	return taskid;
}

void
ri_parallel_gather(void *src, void *dst, size_t size)
{
#ifdef WITH_MPI
	int ret;
	ret = MPI_Gather(src, size, MPI_BYTE,
		         dst, size, MPI_BYTE,
		         0, MPI_COMM_WORLD);

	if (ret != MPI_SUCCESS) {
		printf("err: [%d]\n", ret);
	}
#else
	ri_mem_copy(dst, src, size);
#endif
}

void
ri_parallel_barrier()
{
#ifdef WITH_MPI
	MPI_Barrier(MPI_COMM_WORLD);
#endif
}

void
ri_parallel_send(void *src, size_t size, int dest, int tag)
{
#ifdef WITH_MPI
	MPI_Send(src, size, MPI_BYTE, dest, tag, MPI_COMM_WORLD);
#else
	(void)src;
	(void)size;
	(void)dest;
	(void)tag;
#endif
}

void
ri_parallel_recv(void *src, size_t size, int source, int tag,
		 ri_parallel_status_t *status)
{
#ifdef WITH_MPI
	MPI_Recv(src, size, MPI_BYTE, source, tag, MPI_COMM_WORLD,
		 &(status->status));
#else
	(void)src;
	(void)size;
	(void)source;
	(void)tag;
	(void)status;
#endif
}

void
ri_parallel_irecv(void *src, size_t size, int source, int tag,
		  ri_parallel_request_t *request)
{
#ifdef WITH_MPI
	MPI_Irecv(src, size, MPI_BYTE, source, tag, MPI_COMM_WORLD,
		 &(request->request));
#else
	(void)src;
	(void)size;
	(void)source;
	(void)tag;
	(void)request;
#endif
}

void
ri_parallel_bcast(void *src, size_t size)
{
#ifdef WITH_MPI
	MPI_Bcast(src, size, MPI_BYTE, 0, MPI_COMM_WORLD);
#else
	(void)src;
	(void)size;
#endif
}

int
ri_parallel_test(ri_parallel_request_t *request,
		 ri_parallel_status_t *status)
{
#ifdef WITH_MPI
	int flag;

	MPI_Test(&(request->request), &flag, &(status->status));

	return flag;
#else
	(void)request;
	(void)status;

	return 1;
#endif
}

int
ri_parallel_wait(ri_parallel_request_t *request,
		 ri_parallel_status_t *status)
{
#ifdef WITH_MPI
	int ret;

	ret = MPI_Wait(&(request->request), &(status->status));

	return ret;
#else
	(void)request;
	(void)status;

	return 1;
#endif
}
