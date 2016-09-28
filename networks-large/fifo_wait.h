// Marek Sokolowski - Computer Network large assignment problem
// Implementation of:
//   * "semaphore" implemented using FIFO queue
//   * thread-safe queue working as (linear-time) id => elem map;
//      it can wait for any element showing up in the structure.
#ifndef __FIFO_WAIT
#define __FIFO_WAIT
#include <pthread.h>
#include <stdint.h>


// Kolejka wejscia do sekcji krytycznej. Kazdy moze ustawic sie w kolejce
// i miec pewnosc (silna gwarancja), ze zostanie obsluzony szybko.
struct fifo_wait {
  pthread_cond_t cond;
  pthread_mutex_t mutex;
  uint16_t head, tail;
};

void fifo_init(struct fifo_wait *wait);
void fifo_destroy(struct fifo_wait *wait);
void fifo_enter(struct fifo_wait *wait);    // Stan w kolejce. Moze deadlockowac!
void fifo_leave(struct fifo_wait *wait);    // Opusc kolejke.



// Bezpieczna kolejka jednokierunkowa sluzaca za (multi)mape z indeksow na
// wskazniki na elementy (void*). Kolejka nie utrzymuje ownershipu na te wskazniki,
// wiec trzeba uwazac, by przypadkiem ich nie zwolnic lub zeby ich nie zgubic.
struct safe_fifo_elem {
  int id;      // Indeks.
  void *elem;  // Element.
  struct safe_fifo_elem *prev, *next;
};

struct safe_fifo {
  pthread_cond_t cond;
  pthread_mutex_t mutex;
  struct safe_fifo_elem *head, *tail;
};


// Stworz / usun kolejke.
void safe_fifo_init(struct safe_fifo *fifo);
void safe_fifo_destroy(struct safe_fifo *fifo);

// Wrzuc na kolejke element o podanym id; jesli -1, wybierz jeszcze nieuzywany.
// Zwraca wrzucony id.
int safe_fifo_push_back(struct safe_fifo *fifo, void *elem, int id);

// Podaj pierwszy element kolejki (o podanym id).
struct safe_fifo_elem *safe_fifo_front(struct safe_fifo *fifo);
struct safe_fifo_elem *safe_fifo_front_by_id(struct safe_fifo *fifo, int id);

// Usun element z kolejki.
void safe_fifo_erase(struct safe_fifo *fifo, struct safe_fifo_elem *elem);

// Dla kazdego wskaznika X w kolejce wywolaj func(X, param).
void safe_fifo_apply(struct safe_fifo *fifo, void (*func)(void*, void*), void *param);

// Czekaj na pojawienie sie w kolejce jakiegokolwiek elementu.
void safe_fifo_wait_for_elem(struct safe_fifo *fifo);

#endif
