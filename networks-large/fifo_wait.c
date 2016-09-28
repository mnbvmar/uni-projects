// Marek Sokolowski - Computer Network large assignment problem
// Implementation of:
//   * "semaphore" implemented using FIFO queue
//   * thread-safe queue working as (linear-time) id => elem map;
//      it can wait for any element showing up in the structure.
#include "fifo_wait.h"
#include "headers.h"

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


void fifo_init(struct fifo_wait *wait) {
  memset(wait, 0, sizeof(struct fifo_wait));
  pthread_mutex_init(&wait->mutex, NULL);
  pthread_cond_init(&wait->cond, NULL);
  wait->head = wait->tail = 0;
}

void fifo_destroy(struct fifo_wait *wait) {
  pthread_mutex_destroy(&wait->mutex);
  pthread_cond_destroy(&wait->cond);
}

void fifo_enter(struct fifo_wait *wait) {
  pthread_mutex_lock(&wait->mutex);

  uint8_t my_token = (wait->head)++;
  while (my_token != wait->tail) {
    pthread_cond_wait(&wait->cond, &wait->mutex);
  }

  pthread_mutex_unlock(&wait->mutex);
}

void fifo_leave(struct fifo_wait *wait) {
  pthread_mutex_lock(&wait->mutex);
  (wait->tail)++;
  pthread_cond_broadcast(&wait->cond);
  pthread_mutex_unlock(&wait->mutex);
}



static struct safe_fifo_elem *new_fifo_elem() {
  struct safe_fifo_elem *elem = (struct safe_fifo_elem*)malloc(sizeof(struct safe_fifo_elem));
  elem->elem = NULL;
  elem->id = 0;
  elem->prev = elem->next = NULL;
  return elem;
}

static void free_fifo_elem(struct safe_fifo_elem *elem) {
  free(elem);
}


void safe_fifo_init(struct safe_fifo *fifo) {
  pthread_mutex_init(&fifo->mutex, NULL);
  pthread_cond_init(&fifo->cond, NULL);
  fifo->head = fifo->tail = NULL;
}


// Wszystkie wazne operacje sa opakowane w wziecie/zwolnienie muteksa.
int safe_fifo_push_back(struct safe_fifo *fifo, void *elem, int id) {
  struct safe_fifo_elem *nelem = new_fifo_elem();
  nelem->elem = elem;

  pthread_mutex_lock(&fifo->mutex);

  if (id == -1) {
#define ID_BASE 31400000
    // Trzeba znalezc wolne id.
    id = ID_BASE;
    while (true) {
      bool found = false;
      for (struct safe_fifo_elem *ptr = fifo->tail; ptr; ptr = ptr->next) {
        if (ptr->id == id) { found = true; break; }
      }
      if (!found) { break; }
      id++;
    }
  }

  debug("insert id %d\n", id);

  nelem->id = id;
  nelem->prev = fifo->head;
  if (nelem->prev) { nelem->prev->next = nelem; }
  fifo->head = nelem;
  if (!fifo->tail) { fifo->tail = nelem; }

  // Puszczamy wszystkich, ktorzy oczekiwali na jakikolwiek element.
  pthread_cond_broadcast(&fifo->cond);
  pthread_mutex_unlock(&fifo->mutex);

  return id;
}

struct safe_fifo_elem *safe_fifo_front(struct safe_fifo *fifo) {
  return fifo->tail;
}

struct safe_fifo_elem *safe_fifo_front_by_id(struct safe_fifo *fifo, int id) {
  debug("id = %d\n", id);
  pthread_mutex_lock(&fifo->mutex);
  struct safe_fifo_elem *result = NULL, *cur = fifo->tail;
  while (cur) {
    debug("cid = %d\n", cur->id);
    if (cur->id == id) {
      result = cur;
      break;
    }
    cur = cur->next;
  }
  
  pthread_mutex_unlock(&fifo->mutex);
  return result;
}

void safe_fifo_erase(struct safe_fifo *fifo, struct safe_fifo_elem *elem) {
  pthread_mutex_lock(&fifo->mutex);
  if (fifo->head == elem) { fifo->head = elem->prev; }
  if (fifo->tail == elem) { fifo->tail = elem->next; }
  if (elem->prev) { elem->prev->next = elem->next; }
  if (elem->next) { elem->next->prev = elem->prev; }
  pthread_mutex_unlock(&fifo->mutex);

  free_fifo_elem(elem);
}

void safe_fifo_apply(struct safe_fifo *fifo, void (*func)(void*, void*), void *param) {
  pthread_mutex_lock(&fifo->mutex);
  for (struct safe_fifo_elem *ptr = fifo->tail; ptr; ptr = ptr->next) {
    func(ptr->elem, param);
  }
  pthread_mutex_unlock(&fifo->mutex);
}

void safe_fifo_wait_for_elem(struct safe_fifo *fifo) {
  pthread_mutex_lock(&fifo->mutex);
  // Oczekiwanie na jakikolwiek element.
  while (!fifo->head) {
    pthread_cond_wait(&fifo->cond, &fifo->mutex);
  }
  pthread_mutex_unlock(&fifo->mutex);
}

