#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#define HEAP_MAGIC 0xAABBCCDD

typedef struct BlockHeader {
  uint32_t magic;
  uint32_t size;
} BlockHeader_t;

typedef struct BlockFooter {
  uint32_t magic;
  BlockHeader_t *header;
} BlockFooter_t;

typedef struct FreeNode {
  BlockHeader_t *header;
  struct FreeNode *next;
} FreeNode_t;

uint32_t HEAP_SZ = 0x10000;

void *HEAP_START = NULL;
void *HEAP_END   = NULL;

FreeNode_t *freelist = NULL;

void _add_to_freelist(BlockHeader_t *header);
void _remove_from_freelist(BlockHeader_t *header);
int  _is_block_free(BlockHeader_t *header);

void heap_init(void)
{
  HEAP_START = malloc(HEAP_SZ);
  HEAP_END = (void *)((uint32_t)HEAP_START+HEAP_SZ);

  BlockHeader_t *blockheader = (BlockHeader_t *)HEAP_START;

  blockheader->magic = HEAP_MAGIC;
  blockheader->size = HEAP_SZ - sizeof(BlockHeader_t) - sizeof(BlockFooter_t);

  BlockFooter_t *blockfooter = (BlockFooter_t *)((uint32_t)HEAP_END-sizeof(BlockFooter_t));
  blockfooter->magic = HEAP_MAGIC;
  blockfooter->header = blockheader;

  _add_to_freelist(blockheader);
}

void _add_to_freelist(BlockHeader_t *header)
{
  FreeNode_t *freenode = malloc(sizeof(FreeNode_t));
  freenode->header = header;
  freenode->next = freelist;
  freelist = freenode;
}

/*
 * This function assumes that there is a freenode
 * whose header value is equal to header. Otherwise
 * undefined things might happen.
 */
void _remove_from_freelist(BlockHeader_t *header)
{
  FreeNode_t *freenode = freelist;
  FreeNode_t *prev;

  while (freenode && freenode->header != header) {
    prev = freenode;
    freenode = freenode->next;
  }

  if (freenode && freenode != freelist) {
    prev->next = freenode->next;
    free(freenode);
  } else if (freenode == freelist) {
    freelist = freelist->next;
    free(freenode);
  }
}

/* 
 * Splits a block pointed to by @p header, into 2 blocks
 * The first one would have size = size, the second one
 * would have size header->size - size - sizeof(BlockHeader_t) - sizeof(BlockFooter_t)
 * Note that the latter 2 components subtracted from the header->size value
 * are overhead.
 *
 * This function assumes that the second block would have a positive size.
 *
 * Returns a header to the second block.
 *
 */
BlockHeader_t *_split_block(BlockHeader_t *header, uint32_t size)
{

  uint32_t second_block_size = header->size - size - sizeof(BlockHeader_t) - sizeof(BlockFooter_t);

  assert(header->magic == HEAP_MAGIC);
  assert(second_block_size > 0);

  BlockFooter_t *footer = (BlockFooter_t *)((uint32_t)header + sizeof(BlockHeader_t) + header->size);
  assert(footer->magic == HEAP_MAGIC);
  assert(footer->header == header);

  BlockFooter_t *new_footer = (BlockFooter_t *)((uint32_t)header + sizeof(BlockHeader_t) + size);
  BlockHeader_t *new_header = (BlockHeader_t *)((uint32_t)new_footer+sizeof(BlockFooter_t));

  new_footer->magic = HEAP_MAGIC;
  new_footer->header = header;

  new_header->magic = HEAP_MAGIC;
  new_header->size = second_block_size;

  footer->header = new_header;

  header->size = size;

  return new_header;
}

void *halloc(uint32_t size)
{
  FreeNode_t *freenode = freelist;

  while (freenode && freenode->header->size < size) {
    freenode = freenode->next;
  }

  if (freenode) {
    BlockHeader_t *header = freenode->header;
    
    _remove_from_freelist(header);

    if (header->size - size > sizeof(BlockHeader_t) + sizeof(BlockFooter_t)) {
      BlockHeader_t *second_block = _split_block(header, size);
      _add_to_freelist(second_block);
    }

    return (void *)((uint32_t)header+sizeof(BlockHeader_t));
  } else {
    return NULL;
  }
}

int _is_block_free(BlockHeader_t *header)
{
  FreeNode_t *freenode = freelist;
  while (freenode && freenode->header != header) {
    freenode = freenode->next;
  }
  return (freenode == NULL) ? 0 : (freenode->header == header);
}

void hfree(void *ptr)
{
  BlockHeader_t *header = (BlockHeader_t *)((uint32_t)ptr - sizeof(BlockHeader_t));
  assert(header->magic == HEAP_MAGIC);
  
  BlockFooter_t *footer = (BlockFooter_t *)((uint32_t)header+sizeof(BlockHeader_t)+header->size);
  assert(footer->magic == HEAP_MAGIC);
  assert(footer->header == header);

  BlockFooter_t *prev_footer = (BlockFooter_t *)((uint32_t)header - sizeof(BlockFooter_t));

  if ((uint32_t)HEAP_START <= (uint32_t)prev_footer && (uint32_t)prev_footer < (uint32_t)HEAP_END) {
    assert(prev_footer->magic == HEAP_MAGIC);

    BlockHeader_t *prev_header = prev_footer->header;
    assert(prev_header->magic == HEAP_MAGIC);

    if (_is_block_free(prev_header)) {
      footer->header = prev_header;
      prev_header->size += header->size + sizeof(BlockHeader_t) + sizeof(BlockFooter_t);
    } else {
      _add_to_freelist(header);
    }
  }
}

void heap_finalize(void)
{
  while (freelist) {
    FreeNode_t *next = freelist->next;
    free(freelist);
    freelist = next;
  }

  free(HEAP_START);
}
