#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "hashmap.h"

void _initialize_table(Node_t *table[], uint32_t size);

void _initialize_table(Node_t *table[], uint32_t size)
{
  for (uint32_t i = 0; i < size; i++) {
    table[i] = NULL;
  }
}

static inline uint32_t hash(uint32_t input, uint32_t size)
{
  uint32_t x = input;
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = ((x >> 16) ^ x);
  return x % size;
}

void _rehash_chain(HashMap_t *map, Node_t *chain)
{
  while (chain) {
    hm_insert(map, (void *)chain->key, (void *)chain->value);
    Node_t *next = chain->next;
    chain = next;
  }
}

void _rehash_table(HashMap_t *map, Node_t *table[], uint32_t size)
{
  for (uint32_t i = 0; i < size; i++) {
    _rehash_chain(map, table[i]);
  }
}

void _expand_if_necessary(HashMap_t *map)
{
  float load = (float)map->num_entries / map->size;

  if (load > 2.0) {
    uint32_t oldsize  = map->size;
    uint32_t newsize  = oldsize * 2;
    Node_t **newtable = malloc(sizeof(Node_t *)*newsize);
    Node_t **oldtable = map->table;

    map->table       = newtable;
    map->size        = newsize;
    map->num_entries = 0;

    _initialize_table(newtable, newsize);

    _rehash_table(map, oldtable, oldsize);

    free(oldtable);
  }
}

void hm_insert(HashMap_t *map, void *key, void *val)
{
  uint32_t idx = hash((uint32_t)key, map->size);

  assert(idx < map->size);

  Node_t *node = malloc(sizeof(Node_t));
  node->next   = map->table[idx];
  node->key    = (uint32_t)key;
  node->value  = (uint32_t)val;

  map->table[idx] = node;
  map->num_entries++;
}

void hm_remove(HashMap_t *map, void *key)
{
  if (hm_iselement(map, key)) {

    uint32_t idx = hash((uint32_t)key, map->size);
    Node_t *chain = map->table[idx];
    Node_t *prev;

    while (chain && chain->key != (uint32_t)key) {
      prev = chain;
      chain = chain->next;
    }

    assert(chain != NULL);

    if (chain && chain != map->table[idx]) {
      prev->next = chain->next;
      free(chain);
    } else if (chain == map->table[idx]) {
      map->table[idx] = chain->next;
      free(chain);
    }

  }
}

int _walk_chain(Node_t *chain, void *key)
{
  while (chain) {
    if (chain->key == (uint32_t)key) return chain->value;
    chain = chain->next;
  }
  return 0;
}

int hm_iselement(HashMap_t *map, void *key)
{
  uint32_t idx = hash((uint32_t)key, map->size);

  return (_walk_chain(map->table[idx], key) != 0);
}

void *hm_lookup(HashMap_t *map, void *key)
{
  uint32_t idx = hash((uint32_t)key, map->size);

  return (void *)_walk_chain(map->table[idx], key);
}

HashMap_t *hm_newmap()
{
  HashMap_t *map = malloc(sizeof(HashMap_t));

  map->size = 1024;
  map->num_entries = 0;
  map->table = malloc(sizeof(Node_t *) * map->size);

  _initialize_table(map->table, map->size);

  return map;
}

void _free_chain(Node_t *chain)
{
  while (chain) {
    Node_t *next = chain->next;
    free(chain);
    chain = next;
  }
}

void hm_deletemap(HashMap_t *map)
{
  for (uint32_t i = 0; i < map->size; i++)
  {
    _free_chain(map->table[i]);
  }
  free(map->table);
  free(map);
}
