#include <stdio.h>
#include <string.h>
#include <raaz/primitives/config.h>

#ifdef RAAZ_HAVE_mlock
#include <sys/mman.h>
#endif

#ifdef RAAZ_HAVE_memalign
#include <stdlib.h>
#include <unistd.h>
#endif

void wipememory (void*, size_t);
void * createpool(size_t);
void freepool(void*, size_t);
int memorylock(void*, size_t);
void memoryunlock(void*, size_t);


/* Wipes the entire memory with 0 */
void wipememory (void* mem, size_t size){
  memset(mem, 0, size);
}

/* Creates the page aligned memory */
void * createpool(size_t size){
#ifdef RAAZ_HAVE_memalign
  void *o;
  posix_memalign(&o,getpagesize(),size);
  return o;
#endif
}

void freepool (void* ptr, size_t size){
#ifdef RAAZ_HAVE_memalign
  free(ptr);
#endif
}

/* Locks and Unlocks Memory */
int memorylock(void* ptr, size_t size){
#ifdef RAAZ_HAVE_mlock
  return mlock(ptr,size);
#endif
}

void memoryunlock(void* ptr, size_t size){
#ifdef RAAZ_HAVE_mlock
  munlock(ptr,size);
#endif
}
