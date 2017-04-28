/* Copyright (c) 2014, 2017, Sigurður Helgason
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

#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include <semaphore.h>
#include <time.h>
#include <unistd.h>

#define K 5
#define M 100
#define TIME_TO_EAT 400
sem_t door_mut;
sem_t party_sem;
atomic_int availableSeats = K;
atomic_int currentlyWaiting = 0;

void Eat(int ms){
  long sleep = rand() % 10;
  usleep((ms * 1000) + (sleep * 1000));
}

void *customer(void *arg) {
  int *ID = (int *) arg;
  /* ENTER BEGIN */
  printf("%d: Entered waiting room\n", *ID);
  sem_wait(&door_mut);
  if(availableSeats == 0){
    printf("%d: Found party and is waiting\n", *ID);
    atomic_fetch_add(&currentlyWaiting, 1);
    sem_post(&door_mut);
    sem_wait(&party_sem);
  } else {
    if(currentlyWaiting != 0)
      printf("%d: Managed to cut in line\n", *ID);
    atomic_fetch_sub(&availableSeats, 1);
    sem_post(&door_mut);
  }
  printf("%d: Entered bar\n", *ID);
  /* ENTER END*/

  printf("%d: Starting eating\n", *ID);
  Eat(TIME_TO_EAT);

  /* EXIT BEGIN */
  sem_wait(&door_mut);
  atomic_fetch_add(&availableSeats, 1);
  if(availableSeats == K && currentlyWaiting != 0){
    int toLetIn = K;
    if(currentlyWaiting < toLetIn)
      toLetIn = currentlyWaiting;
    atomic_fetch_sub(&availableSeats, toLetIn);
    atomic_fetch_sub(&currentlyWaiting, toLetIn);
    for(; toLetIn > 0; toLetIn--){ 
      sem_post(&party_sem);
    }
  }
  sem_post(&door_mut);
  /* EXIT END */
  printf("%d Exited the SUSHIBAR\n",*ID);
  free(ID);
  return NULL;
}

int main() {
  time_t t;
  srand((unsigned) time(&t));
  int error;
  if((error = sem_init(&door_mut, 0, 1)) != 0){
    exit(0);
  }
  if((error = sem_init(&party_sem, 0, 0)) != 0){
    exit(0);
  }
  pthread_t customers[M];
  for(int i = 0; i < M; i++) { 
    int *id = malloc(sizeof(int));
    *id = i;
    pthread_create(&customers[i], NULL, customer, id);
    usleep(500);
  }
  for(int i = 0; i < M; i++) { 
    pthread_join(customers[i], NULL);
  }
  sem_destroy(&door_mut);
  sem_destroy(&party_sem);
  return 0;
}
