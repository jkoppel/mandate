#include <stdint.h>

typedef struct Node {
  uint32_t key;
  uint32_t value;
  struct Node *next;
} Node_t;

typedef struct HashMap {
  uint32_t  size;
  uint32_t  num_entries;
  Node_t  **table;
} HashMap_t;

void hm_insert(HashMap_t *map, void *key, void *val);
void hm_remove(HashMap_t *map, void *key);

int   hm_iselement(HashMap_t *map, void *key);
void *hm_lookup(HashMap_t *map, void *key);

HashMap_t *hm_newmap(void);
void hm_deletemap(HashMap_t *map);
