/* a simple driver for elco_entry */
#include <stdio.h>

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

unsigned char elco_entry(void);

/* all lisp values are of type ptr */
typedef unsigned int ptr_t;

static void print_ptr(ptr_t x){
    if((x & FXMASK) == FXTAG){
        printf("%d", (int)(x >> FXSHIFT));
    }else if(x == NIL){
        printf("NIL");
    }else if(x == T){
        printf("T");
    }else if(x == F){
        printf("F");
    }else if((x & CHARMASK) == CHARTAG){
        printf("%c", (char)(x >> CHARSHIFT));
    }else{
        printf("#<unknown 0x%08X>", x);
    }

    printf("\n");
}

int main(int argc, char **argv){
	print_ptr(elco_entry());
	return 0;
}
