#include <stdint.h>

void heap_init(void);
void *halloc(uint32_t size);
void hfree(void *ptr);
void heap_finalize(void);
