#ifdef WIN32
#include <windows.h>
#else
#include <sys/time.h>
#endif

#include <stdlib.h>

#include "timer.h"

void
get_time(mytimer_t *t)
{
#ifdef WIN32
	QueryPerformanceCounter(&t->time);
#else
	gettimeofday(&t->time, NULL);
#endif
}

double
elapsed_time(mytimer_t *start, mytimer_t *end)
{
	double elap;

#ifdef WIN32
	LARGE_INTEGER freq;

	QueryPerformanceFrequency(&freq);
	elap = ((double)end->time.QuadPart - (double)start->time.QuadPart) / (double)freq.QuadPart;
#else

	elap = (double)(end->time.tv_sec - start->time.tv_sec) + 
		(double)(end->time.tv_usec - start->time.tv_usec) / (double)1.0e6;
#endif

	return elap;
	
}
