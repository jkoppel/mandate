#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include "hashmap.h"

#define DEBUG 1

#ifdef DEBUG
#define debug(fmt, ...) \
  do {fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, \
          __LINE__, __func__, __VA_ARGS__);} while (0)
#else
#define debug(fmt, ...)
#endif

typedef struct PtrMapEntry {
  struct PtrMapEntry *prev;
  uint32_t key;
  uint32_t reg_usage;
  int32_t *stack_usage; // 0 terminated
} PtrMapEntry_t;

typedef struct RegSet {
  uint32_t edi;
  uint32_t esi;
  uint32_t ebp;
  uint32_t esp;
  uint32_t ebx;
  uint32_t edx;
  uint32_t ecx;
  uint32_t eax;
} RegSet_t;

extern PtrMapEntry_t *GCINITHEAD;

HashMap_t *PMTable = NULL;

static int32_t *TOPSTACK = NULL;
static int32_t *BOTSTACK = (int32_t *)0xffffffff;

uint32_t walk_stack_usage(int32_t *stack_usage);
void copy_stack_usage(int32_t *stack_usage, uint32_t ebp, uint32_t *ptrs);

#define REG_IN_USE(r, idx) \
  static inline uint32_t is_##r##_inuse(uint32_t bitmap) \
  { \
    return (bitmap & (0x1 << idx)); \
  }

REG_IN_USE(eax, 0)
REG_IN_USE(ebx, 1)
REG_IN_USE(ecx, 2)
REG_IN_USE(edx, 3)
REG_IN_USE(esi, 5)
REG_IN_USE(edi, 6)
REG_IN_USE(ebp, 7)
REG_IN_USE(esp, 8)

#undef REG_IN_USE

void update_top_stack(int32_t *newtop)
{
  TOPSTACK = (newtop > TOPSTACK) ? newtop : TOPSTACK;
}

void update_bot_stack(int32_t *newbot)
{
  BOTSTACK = (newbot < BOTSTACK) ? newbot : BOTSTACK;
}

void gc_init(void)
{
  PMTable = hm_newmap();
  PtrMapEntry_t *head = GCINITHEAD;

  uint32_t count = 1;

  while (head) {
    hm_insert(PMTable, (void *)head->key, (void *)head);
    head = head->prev;
    count++;
  }
}

void gc_finalize(void)
{
  hm_deletemap(PMTable);
}

uint32_t walk_stack_usage(int32_t *stack_usage)
{
  uint32_t idx = 0;
  while (stack_usage[idx]) {
    //debug("Stack[%d] = %d\n", idx, stack_usage[idx]);
    idx++;
  }
  return idx;
}

void copy_stack_usage(int32_t *stack_usage, uint32_t ebp, uint32_t *ptrs)
{
  uint32_t idx = 0;
  while (stack_usage[idx]) {
    ptrs[idx] = *((uint32_t *)(ebp + stack_usage[idx]));
    idx++;
  }
}

void dump_stack_limits(void)
{
  debug("TOPSTACK = 0x%x\n", (int32_t)TOPSTACK);
  debug("BOTSTACK = 0x%x\n", (int32_t)BOTSTACK);
}

void gcentry(uint32_t retaddr, RegSet_t *registers)
{
  PtrMapEntry_t *frameinfo = 
    (PtrMapEntry_t *)hm_lookup(PMTable, (void *)retaddr);



  if (frameinfo) {

    assert(frameinfo->key == retaddr);

    uint32_t ptrcount = 0;
    uint32_t ebp = registers->ebp;

#define checkreg(r) \
  if (is_##r##_inuse(frameinfo->reg_usage)) ptrcount++
  checkreg(eax);
  checkreg(ebx);
  checkreg(ecx);
  checkreg(edx);
  checkreg(esi);
  checkreg(edi);
#undef checkreg

  ptrcount += walk_stack_usage(frameinfo->stack_usage);

  uint32_t pointers[ptrcount];
  uint32_t ptridx = 0;

#define loadreg(r) \
  if (is_##r##_inuse(frameinfo->reg_usage)) pointers[ptridx++] = registers->r;
  loadreg(eax);
  loadreg(ebx);
  loadreg(ecx);
  loadreg(edx);
  loadreg(esi);
  loadreg(edi);
#undef loadreg

  copy_stack_usage(frameinfo->stack_usage, ebp, pointers+ptridx);

  } else {
    debug("RETADDR = %x DOES NOT MAP TO PTRMAP.\n", retaddr);
  }
}
