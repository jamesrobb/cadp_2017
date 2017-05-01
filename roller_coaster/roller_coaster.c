#define _XOPEN_SOURCE 700
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdatomic.h>

#define M   10
#define N   40
#define C   2

int num_people = 0;
int on_car = 0;
int next_car = 0;
bool can_board = true;

pthread_mutex_t conductor_mutex;
pthread_cond_t begin_loading, begin_unloading, enter_car, leave_car, car_empty;

/* monitor functions */
void conductor_take_ride() {
    
    pthread_mutex_lock(&conductor_mutex);
    
    num_people += 1;
    printf("Passenger %u has arrived for a ride.\n", (unsigned int)pthread_self());
    if(num_people >= C && can_board) {
        can_board = false;
        pthread_cond_broadcast(&begin_loading);
    }

    pthread_cond_wait(&enter_car, &conductor_mutex);
    on_car += 1;
    if(on_car == C) {
        pthread_cond_signal(&begin_unloading);
    }

    pthread_cond_wait(&leave_car, &conductor_mutex);
    on_car -= 1;
    num_people -= 1;
    if(on_car == 0) {
        pthread_cond_signal(&car_empty);
    }

    pthread_mutex_unlock(&conductor_mutex);
}

void conductor_load(int car_number) {

    pthread_mutex_lock(&conductor_mutex);
    
    while(num_people < C || next_car != car_number) {
        pthread_cond_wait(&begin_loading, &conductor_mutex);
    }

    printf("num people %d\n", num_people);
    for(int i = 0; i < C; i++) {
        pthread_cond_signal(&enter_car);
    }

    pthread_mutex_unlock(&conductor_mutex);
}

void conductor_unload(int car_number) {

    pthread_mutex_lock(&conductor_mutex);

    if(on_car < C) {
        pthread_cond_wait(&begin_unloading, &conductor_mutex);
    }

    printf("Car %d ran with %d passengers. Now unloading.\n", car_number, on_car);
    for(int i = 0; i < C; i++) {
        pthread_cond_signal(&leave_car);
    }

    if(on_car > 0) {
        pthread_cond_wait(&car_empty, &conductor_mutex);
    }
    can_board = true;
    next_car = (next_car + 1) % M;
    printf("Car %d has unloaded.\n", car_number);
    pthread_cond_broadcast(&begin_loading);

    pthread_mutex_unlock(&conductor_mutex);
}

/* thread functions */
void* car(void* arg) {

    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

    int car_number = *((int *)arg);

    while(true) {
        conductor_load(car_number);
        conductor_unload(car_number);

    }

    return NULL;
}

void* passenger() {
    
    /* the passenger may or may not take a ride */
    if(true) {
    //if(rand() % 2 == 0) {
        conductor_take_ride();
    }

    return NULL;
}


/* main function */
int main() {
	srand(time(NULL));
    pthread_t passengers[N];
    pthread_t cars[M];
    int car_numbers[M];

    if(pthread_mutex_init(&conductor_mutex, NULL) != 0) {
        printf("pthread_mutex_init error\n");
        exit(1);
    }

    if(pthread_cond_init(&begin_loading, NULL) != 0) {
        printf("pthread_cond_init error\n");
        exit(1);
    }
    
    if(pthread_cond_init(&begin_unloading, NULL) != 0) {
        printf("pthread_cond_init error\n");
        exit(1);
    }
    
    if(pthread_cond_init(&enter_car, NULL) != 0) {
        printf("pthread_cond_init error\n");
        exit(1);
    }

    if(pthread_cond_init(&leave_car, NULL) != 0) {
        printf("pthread_cond_init error\n");
        exit(1);
    }

    if(pthread_cond_init(&car_empty, NULL) != 0) {
        printf("pthread_cond_init error\n");
        exit(1);
    }


    for(int i = 0; i < N; i++) {
        pthread_create(&passengers[i], NULL, passenger, NULL);
    }

    for(int i = 0; i < M; i++) {
        car_numbers[i] = i;
        pthread_create(&cars[i], NULL, car, (void *)(&car_numbers[i]));
    }

    for(int i = 0; i < N; i++) {
        pthread_join(passengers[i], NULL);
    }
    printf("All passenger threads joined.\n");

    for(int i = 0; i < M; i++) {
        pthread_cancel(cars[i]);
    }
    printf("All car threads terminated.\n");

    pthread_mutex_destroy(&conductor_mutex);
    pthread_cond_destroy(&begin_loading);
    pthread_cond_destroy(&begin_unloading);
    pthread_cond_destroy(&enter_car);
    pthread_cond_destroy(&leave_car);
    pthread_cond_destroy(&car_empty);

    return 0;
}


