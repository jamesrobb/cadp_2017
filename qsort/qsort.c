/* Copyright (c) 2014, 2017, Marcel Kyas
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
   STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
   OF THE POSSIBILITY OF SUCH DAMAGE.

   The views and conclusions contained in the software and
   documentation are those of the authors and should not be
   interpreted as representing official policies, either expressed or
   implied, of the Reykjavik University. */


#define _XOPEN_SOURCE 700
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include <float.h>

#define MIN_SEGMENT 1000
#define NUM_TESTS   10
#define MAX_THREADS 16

struct args_struct {
    int* array;
    int start;
    int end;
};

int current_max_threads = 1;

/* how many threads are currently running */
atomic_int thread_count = 1;

/* Seed to use for generating random numbers. */
static unsigned short seed[3] = { 42, 42, 42 };

/* Allocate an array of test data. */
static int *testdata(const size_t length)
{
	register int i;
	int  *result = malloc(sizeof(int) * length);
	seed48(seed);
	for (i = 0; i < length; ++i) {
		result[i] = (int) mrand48();
	}
	return result;
}


/* Release the test data after checking it. */
static void releasedata(int *data, const size_t length)
{
        for (int i = 1; i < length; i++) {
                if (data[i-1] > data[i]) {
                        printf("Array not sorted\n");
                        abort();
                }
        }
	free(data);
}



static inline void __attribute__((__always_inline__,__gnu_inline__))
exchange(int *array, const int i, const int j)
{
	const int t = array[i];
	array[i] = array[j];
	array[j] = t;
}



static inline int __attribute__((__always_inline__,__gnu_inline__))
partition(int *array, const int start, const int end)
{
	const int median = array[start];
	int i = start - 1, j = end + 1;
	for (;;) {
		do --j; while (array[j] > median);
		do ++i; while (array[i] < median);
		if (i < j) exchange(array, i, j); else return j;
        }
}

static void quicksort(int *array, const int start, const int end)
{

	if (start < end) {
		const int middle = partition(array, start, end);
		quicksort(array, start, middle);
		quicksort(array, middle + 1, end);
	}

}

static void* pquicksort(void *arg)
{
    struct args_struct* sort_args = (struct args_struct*)arg;
    bool thread_created = false;

    if(sort_args->end - sort_args->start <= MIN_SEGMENT) {
        quicksort(sort_args->array, sort_args->start, sort_args->end);
        return NULL;
    }

    if(sort_args->start < sort_args->end) {
        const int middle = partition(sort_args->array, sort_args->start, sort_args->end);

        pthread_t left_partition;
        struct args_struct left_args;
        struct args_struct right_args;
        left_args.array = sort_args->array;
        left_args.start = sort_args->start;
        left_args.end = middle;
        right_args.array = sort_args->array;
        right_args.start = middle + 1;
        right_args.end = sort_args->end;

        if(atomic_fetch_add(&thread_count, 1) < current_max_threads) {
            thread_created = true;
            pthread_create(&left_partition, NULL, pquicksort, &left_args);
        } else {
            atomic_fetch_sub(&thread_count, 1);
        }

        pquicksort((void *)(&right_args));
        if(!thread_created) {
            pquicksort((void *)(&left_args));
        } else {
            pthread_join(left_partition, NULL);
            atomic_fetch_sub(&thread_count, 1);
        }
    }

    return NULL;
}

void remove_min_max(double* array, int length)
{
    double max = 0;
    double min = DBL_MAX;
    int max_x = 0;
    int min_x = 0;

    for(int i = 0; i < length; i++) {

        if(array[i] > max) {
            max = array[i];
            max_x = i;
        }

        if(array[i] < min) {
            min = array[i];
            min_x = i;
        }

    }

    array[max_x] = 0;
    array[min_x] = 0;

    return;
}

int main(int argc, char **argv)
{
	struct timespec start, end;
    double elapsed;
	int *data;
    //size_t lengths[] =
    //        { 1000, 10000, 100000, 1000000, 10000000, 100000000 };

    size_t lengths[] =
            { 10000000 };
    int lengths_used = (int) (sizeof(lengths) / sizeof(lengths[0]));
    double sort_times_sequential[lengths_used][MAX_THREADS][NUM_TESTS];
    double sort_times_parallel[lengths_used][MAX_THREADS][NUM_TESTS];

    for(current_max_threads = 1; current_max_threads <= MAX_THREADS; current_max_threads++){
        for(int k = 0; k < NUM_TESTS; k++) { 
            for(int i = 0; i < lengths_used; i++) {
                    const size_t length = lengths[i];

                    // printf("Length: %zd\nSequential:\n", length);
                    // data = testdata(length);
                    // clock_gettime(CLOCK_MONOTONIC, &start);
                    // quicksort(data, 0, length - 1);
                    // clock_gettime(CLOCK_MONOTONIC, &end);
                    // elapsed = (double) (end.tv_sec - start.tv_sec) + ((double) (end.tv_nsec - start.tv_nsec) * 1e-9);
                    // sort_times_sequential[i][current_max_threads-1][k] = elapsed;
                    // printf("Elapsed: %g s\n", elapsed);
                    // releasedata(data, length);

                    printf("Multi-threaded:\n");
                    data = testdata(length);
                    clock_gettime(CLOCK_MONOTONIC, &start);
                    // FIXME: Your code goes here.
                    struct args_struct sort_args;
                    sort_args.array = data;
                    sort_args.start = 0;
                    sort_args.end = length - 1;
                    pquicksort((void *)(&sort_args));
                    clock_gettime(CLOCK_MONOTONIC, &end);
                    elapsed = (double) (end.tv_sec - start.tv_sec) + ((double) (end.tv_nsec - start.tv_nsec) * 1e-9);
                    sort_times_parallel[i][current_max_threads-1][k] = elapsed;
                    printf("Elapsed: %g s\n\n", elapsed);
                    releasedata(data, length);
            }
        }
    }

    printf("\nAverage Stats:\n\n");

    for(int i = 0; i < lengths_used; i++) {
        for(int k = 0; k < MAX_THREADS; k++){
            //remove_min_max(sort_times_sequential[i][k], NUM_TESTS);
            remove_min_max(sort_times_parallel[i][k], NUM_TESTS);

            //double sequential_total = 0;
            double parallel_total = 0;

            for(int j = 0; j < NUM_TESTS; j++) {
                //sequential_total += sort_times_sequential[i][k][j];
                parallel_total += sort_times_parallel[i][k][j];
            }

            //printf("length: %d\nN-Threads: %d\nsequential: %f\nparallel: %f\n\n", (int)lengths[i], k+1, sequential_total/(NUM_TESTS-2), parallel_total/(NUM_TESTS-2));
            printf("length: %d\nN-Threads: %d\nparallel: %f\n\n", (int)lengths[i], k+1, parallel_total/(NUM_TESTS-2));   
        }
    }

	return 0;
}
