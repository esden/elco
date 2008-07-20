/* a simple driver for elco_entry */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stdbool.h>

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
#define CONSMASK 0x07
#define CONSTAG 0x01
#define STRINGMASK 0x07
#define STRINGTAG 0x02

/* 
 * context struct for the elco code 
 * elco_entry stores registers there
 */
typedef struct {
    void *eax; /* 0   scratch */
    void *ebx; /* 4   preserve */
    void *ecx; /* 8   scratch */
    void *edx; /* 12  scratch */
    void *esi; /* 16  preserve */
    void *edi; /* 20  preserve */
    void *ebp; /* 24  preserve */
    void *esp; /* 28  preserve */
} context;

unsigned int elco_entry(context *ctxt, char *stack_base, char *heap_base);

/* all lisp values are of type ptr */
typedef unsigned int ptr_t;

/* stack handling functions */
static char *allocate_protected_space(int size){
    int i;
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
    for(i=0; i < size/4; i++){
        *(p+page+i*4+0) = 0xDE;
        *(p+page+i*4+1) = 0xAD;
        *(p+page+i*4+2) = 0xBE;
        *(p+page+i*4+3) = 0xEF;
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
static void print_ptr(ptr_t x, ptr_t parent, bool car){
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
            printf("#\\%02X", x >> CHARSHIFT);
    }else if((x & CONSMASK) == CONSTAG){
        if((parent == NIL) || ((parent != NIL) && car)){
            printf("(");
        }

        if(*((ptr_t *)(x + 3)) == NIL){
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(")");
        }else if((*((ptr_t *)(x + 3)) & CONSMASK) == CONSTAG){
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(" ");
            print_ptr(*((ptr_t *)(x + 3)), x, false);
        }else{
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(" . ");
            print_ptr(*((ptr_t *)(x + 3)), x, true);
            printf(")");
        }
    }else if((x & STRINGMASK) == STRINGTAG){
        //printf("%08x\n", x);
        //printf("%08x\n", (int)*((ptr_t *)(x - 2)) >> FXSHIFT);
        
        int i;
        int size = (int)*((ptr_t *)(x - 2)) >> FXSHIFT;
        unsigned char ch;

        printf("\"");
        for(i=0; i < size; i++){
            ch = (unsigned char)*((ptr_t *)(x + 2 + i));

            if(ch == '\0') continue;

            if(ch == '\"'){
                printf("\\\"");
            }else if(ch == '\\'){
                printf("\\\\");
            }else if(ch > 32 && ch < 127){
                printf("%c", ch);
            }else if(ch < 128){
                printf("%s", character_table[(int)ch]);
            }else{
                printf("#\\%02X", ch);
            }
        }
        printf("\"");
    }else{
        printf("#<unknown 0x%08X>", x);
    }

    fflush(stdout);
}

int main(int argc, char **argv){
    int stack_size = (16 * 4096); /* holds 16K cells */
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;

    int heap_size = (16 * 4096); /* holds 16K cells */
    char *heap_top = allocate_protected_space(heap_size);
    //char *heap_base = heap_top + heap_size;

    context ctxt;

    print_ptr(elco_entry(&ctxt, stack_base, heap_top), NIL, true);
    printf("\n");

    deallocate_protected_space(heap_top, heap_size);
    deallocate_protected_space(stack_top, stack_size);
    return 0;
}
