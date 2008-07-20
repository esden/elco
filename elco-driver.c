/* a simple driver for elco_entry */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

#include "elco-character-table.h"

/* define all lisp constants */
#define FXMASK 0x03
#define FXSHIFT 2
#define FXTAG 0x00
#define NIL 0x3F
#define CHARMASK 0xFF
#define CHARSHIFT 8
#define CHARTAG 0x0F
#define F 0x2F
#define T 0x6F

unsigned int elco_entry(char *stack_base);

/* all lisp values are of type ptr */
typedef unsigned int ptr_t;

/* stack handling functions */
static char *allocate_protected_space(int size){
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page + 1) / page) * page;
    char *p = mmap(0, aligned_size + 2 * page,
                   PROT_READ | PROT_WRITE,
                   MAP_ANON | MAP_PRIVATE,
                   0, 0);
    if(p == MAP_FAILED){
        printf("Could not map memory!\n");
        exit(1);
    }
    status = mprotect(p, page, PROT_NONE);
    if(status != 0){
        printf("First protection page failed to initialize!\n");
        exit(1);
    }
    status = mprotect(p + page + aligned_size, page, PROT_NONE);
    if(status != 0){
        printf("Second protection page failed to initialize!\n");
        exit(1);
    }
    return(p + page);
}

static void deallocate_protected_space(char *p, int size){
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page + 1) / page) * page;
    status = munmap(p - page, aligned_size + 2 * page);
    if(status != 0){
        printf("Could not free allocated protected space!\n");
        exit(1);
    }
}

/* print immediate value */
static void print_ptr(ptr_t x){
    //printf("%08X", x);
    if((x & FXMASK) == FXTAG){
        printf("%d", (int)((int)(x) >> FXSHIFT));
    }else if(x == NIL){
        printf("NIL");
    }else if(x == T){
        printf("T");
    }else if(x == F){
        printf("F");
    }else if((x & CHARMASK) == CHARTAG){
        if((x >> CHARSHIFT) < 128)
            printf("%s", character_table[x >> CHARSHIFT]);
        else
            printf("#\\%d", x >> CHARSHIFT);
    }else{
        printf("#<unknown 0x%08X>", x);
    }

    printf("\n");
}

int main(int argc, char **argv){
    int stack_size = (16 * 4096); /* holds 16K cells */
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;
    print_ptr(elco_entry(stack_base));
    deallocate_protected_space(stack_top, stack_size);
    return 0;
}
