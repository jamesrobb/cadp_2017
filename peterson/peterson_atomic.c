#include <pthread.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdatomic.h>

atomic_bool flag[2] = {false, false};
atomic_int turn = 0;

void *process(void *arg)
{
     const int id = *((int *) arg);
     int n = 0;
start:
     /* Remainder of code */
     flag[id] = true;
     turn = 1 - id;
     while (flag[1 - id] == true &&
	    turn == 1 - id) {
	  /* busy waiting */
     }
     /* Critical section */
     fprintf(stdout, "%d: Thread %d in critical section\n", n++, id);
     fflush(stdout);
     flag[id] = false;
     goto start;
     return NULL;
}

int main()
{
     pthread_t p1, p2;
     int a1 = 0, a2 = 1;

     pthread_create(&p1, NULL, process, &a1);
     pthread_create(&p2, NULL, process, &a2);
     pthread_join(p2, NULL);
     pthread_join(p1, NULL);
     return 0;
}
