#define _XOPEN_SOURCE 700
#include <pthread.h>
#include <semaphore.h>
#include <time.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#define K	5
#define	M 	15

int seats_occupied = 0, waiters = 0;
sem_t so_sem, waiter_sem;

static void* customer(void* arg __attribute__((unused))) {

	/* wait some random time before entering the store */
	struct timespec rand_wait;
	rand_wait.tv_sec = rand() % 5;
	rand_wait.tv_nsec = (rand() % 5) * 1e+8;
	nanosleep(&rand_wait, NULL);

	bool is_waiter = false;

	sem_wait(&so_sem);
	if(seats_occupied == K) {
		printf("entered sushi bar and waited\n");
		is_waiter = true;
		waiters += 1;
		printf("waiters = %d\n", waiters);
	} else {
		printf("entered sushi bar and sat immediately\n");
		seats_occupied += 1;
	}
	sem_post(&so_sem);

	if(is_waiter) {
		sem_wait(&waiter_sem);
	}

	/* eat for some random time */
	rand_wait.tv_sec = rand() % 2;
	rand_wait.tv_nsec = (rand() % 5) * 1e+8;
	nanosleep(&rand_wait, NULL);

	sem_wait(&so_sem);
	printf("seats_occupied = %d\n", seats_occupied);
	seats_occupied -= 1;

	if(seats_occupied == 0) {
		printf("empty sushi bar\n");
		int waiters_to_free = K < waiters ? K : waiters;
		
		for(int i = 0; i < waiters_to_free; i++) {
			waiters -= 1;
			seats_occupied += 1;
			sem_post(&waiter_sem);
		}
	}
	sem_post(&so_sem);

	return NULL;
}

int main() {
	srand(time(NULL));
	pthread_t customers[M];
	sem_init(&so_sem, 0, 1);
	sem_init(&waiter_sem, 0, 0);

	for(int i = 0; i < M; i++) {
		pthread_create(&customers[i], NULL, customer, NULL);
	}

	for(int i = 0; i < M; i++) {
		pthread_join(customers[i], NULL);
	}

	sem_destroy(&so_sem);
	sem_destroy(&waiter_sem);

	return 0;
}