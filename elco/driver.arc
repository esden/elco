(def create-driver-object ()
  (with (tmp (mktmp-file-name "c")
         driver-object (mktmp-file-name "o"))
    (writefileraw (map (fn (x) (coerce x 'int)) (coerce "
/*
 *  Copyright (c) 2008, Piotr Esden-Tempski <piotr at esden.net>
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without 
 *  modification, are permitted provided that the following conditions are 
 *  met:
 *  
 *  * Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in the 
 *    documentation and/or other materials provided with the distribution.
 *  * The names of its contributors may not be used to endorse or promote 
 *    products derived from this software without specific prior written 
 *    permission.
 * 
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 *  \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
 *  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef ELCO_CHARACTER_TABLE_C
#define ELCO_CHARACTER_TABLE_C

char *character_table[128] = {
    \"#\\\\nul\",
    \"#\\\\soh\",
    \"#\\\\stx\",
    \"#\\\\etx\",
    \"#\\\\eot\",
    \"#\\\\enq\",
    \"#\\\\ack\",
    \"#\\\\bel\",
    \"#\\\\bs\",
    \"#\\\\tab\",
    \"#\\\\newline\",
    \"#\\\\vt\",
    \"#\\\\ff\",
    \"#\\\\return\",
    \"#\\\\so\",
    \"#\\\\si\",
    \"#\\\\dle\",
    \"#\\\\dc1\",
    \"#\\\\dc2\",
    \"#\\\\dc3\",
    \"#\\\\dc4\",
    \"#\\\\nak\",
    \"#\\\\syn\",
    \"#\\\\etb\",
    \"#\\\\can\",
    \"#\\\\em\",
    \"#\\\\sub\",
    \"#\\\\esc\",
    \"#\\\\fs\",
    \"#\\\\gs\",
    \"#\\\\rs\",
    \"#\\\\us\",
    \"#\\\\space\",
    \"#\\\\!\",
    \"#\\\\\\\"\",
    \"#\\\\#\",
    \"#\\\\$\",
    \"#\\\\%\",
    \"#\\\\&\",
    \"#\\\\'\",
    \"#\\\\(\",
    \"#\\\\)\",
    \"#\\\\*\",
    \"#\\\\+\",
    \"#\\\\,\",
    \"#\\\\-\",
    \"#\\\\.\",
    \"#\\\\/\",
    \"#\\\\0\",
    \"#\\\\1\",
    \"#\\\\2\",
    \"#\\\\3\",
    \"#\\\\4\",
    \"#\\\\5\",
    \"#\\\\6\",
    \"#\\\\7\",
    \"#\\\\8\",
    \"#\\\\9\",
    \"#\\\\:\",
    \"#\\\\;\",
    \"#\\\\<\",
    \"#\\\\=\",
    \"#\\\\>\",
    \"#\\\\?\",
    \"#\\\\@\",
    \"#\\\\A\",
    \"#\\\\B\",
    \"#\\\\C\",
    \"#\\\\D\",
    \"#\\\\E\",
    \"#\\\\F\",
    \"#\\\\G\",
    \"#\\\\H\",
    \"#\\\\I\",
    \"#\\\\J\",
    \"#\\\\K\",
    \"#\\\\L\",
    \"#\\\\M\",
    \"#\\\\N\",
    \"#\\\\O\",
    \"#\\\\P\",
    \"#\\\\Q\",
    \"#\\\\R\",
    \"#\\\\S\",
    \"#\\\\T\",
    \"#\\\\U\",
    \"#\\\\V\",
    \"#\\\\W\",
    \"#\\\\X\",
    \"#\\\\Y\",
    \"#\\\\Z\",
    \"#\\\\[\",
    \"#\\\\\\\\\",
    \"#\\\\]\",
    \"#\\\\^\",
    \"#\\\\_\",
    \"#\\\\`\",
    \"#\\\\a\",
    \"#\\\\b\",
    \"#\\\\c\",
    \"#\\\\d\",
    \"#\\\\e\",
    \"#\\\\f\",
    \"#\\\\g\",
    \"#\\\\h\",
    \"#\\\\i\",
    \"#\\\\j\",
    \"#\\\\k\",
    \"#\\\\l\",
    \"#\\\\m\",
    \"#\\\\n\",
    \"#\\\\o\",
    \"#\\\\p\",
    \"#\\\\q\",
    \"#\\\\r\",
    \"#\\\\s\",
    \"#\\\\t\",
    \"#\\\\u\",
    \"#\\\\v\",
    \"#\\\\w\",
    \"#\\\\x\",
    \"#\\\\y\",
    \"#\\\\z\",
    \"#\\\\{\",
    \"#\\\\|\",
    \"#\\\\}\",
    \"#\\\\~\",
    \"#\\\\del\"
};

#endif /* ELCO_CHARACTER_TABLE_C */
/*
 *  Copyright (c) 2008, Piotr Esden-Tempski <piotr at esden.net>
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without 
 *  modification, are permitted provided that the following conditions are 
 *  met:
 *  
 *  * Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in the 
 *    documentation and/or other materials provided with the distribution.
 *  * The names of its contributors may not be used to endorse or promote 
 *    products derived from this software without specific prior written 
 *    permission.
 * 
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 *  \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
 *  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* a simple driver for elco_entry */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <string.h>

//#include \"elco-character-table.h\"

/* define all lisp constants */
#define FXMASK 0x03
#define FXSHIFT 2
#define FXTAG 0x00
#define NIL 0x2F
#define CHARMASK 0xFF
#define CHARSHIFT 8
#define CHARTAG 0x0F
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

/* all lisp values are of type ptr */
typedef unsigned int ptr_t;

extern ptr_t elco_entry (context *ctxt, char *stack_base, char *heap_base) asm(\"elco_entry\");

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
        printf(\"Could not map memory!\\n\");
        exit(1);
    }
    status = mprotect(p, page, PROT_NONE);
    if(status != 0){
        printf(\"First protection page failed to initialize!\\n\");
        exit(1);
    }
    status = mprotect(p + page + aligned_size, page, PROT_NONE);
    if(status != 0){
        printf(\"Second protection page failed to initialize!\\n\");
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
        printf(\"Could not free allocated protected space!\\n\");
        exit(1);
    }
}

/* foreign function helpers */
int unshift(ptr_t val){
    return val >> FXSHIFT;
}

ptr_t shift(int val){
    return val << FXSHIFT;
}

char* string_data(ptr_t str){
    return (char *)(str + 2);
}

/* foreign function wrappers */
extern ptr_t e_write(ptr_t fd, ptr_t str, ptr_t len) asm(\"e_write\");
ptr_t e_write(ptr_t fd, ptr_t str, ptr_t len){
    //printf(\"Writing to %i the string %s with length %i!\\n\", unshift(fd), string_data(str), unshift(len));
    return shift(write(unshift(fd), string_data(str), unshift(len)));
}

extern void e_exit (ptr_t val) asm(\"e_exit\");
void e_exit(ptr_t val){
    printf(\"An exit issued from elco, with return code %i!\\n\", unshift(val));
    exit(unshift(val));
}

extern ptr_t e_sleep (ptr_t seconds) asm(\"e_sleep\");
ptr_t e_sleep(ptr_t seconds){
    return shift(sleep(unshift(seconds)));
}

/* print immediate value */
static void print_ptr(ptr_t x, ptr_t parent, bool car){
    //printf(\"%08X\", x);

    if((x & FXMASK) == FXTAG){
        printf(\"%d\", (int)((int)(x) >> FXSHIFT));
    }else if(x == NIL){
        printf(\"nil\");
    }else if(x == T){
        printf(\"t\");
    }else if((x & CHARMASK) == CHARTAG){
        if((x >> CHARSHIFT) < 128)
            printf(\"%s\", character_table[x >> CHARSHIFT]);
        else
            printf(\"#\\\\%02X\", x >> CHARSHIFT);
    }else if((x & CONSMASK) == CONSTAG){
        if((parent == NIL) || ((parent != NIL) && car)){
            printf(\"(\");
        }

        if(*((ptr_t *)(x + 3)) == NIL){
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(\")\");
        }else if((*((ptr_t *)(x + 3)) & CONSMASK) == CONSTAG){
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(\" \");
            print_ptr(*((ptr_t *)(x + 3)), x, false);
        }else{
            print_ptr(*((ptr_t *)(x - 1)), x, true);
            printf(\" . \");
            print_ptr(*((ptr_t *)(x + 3)), x, true);
            printf(\")\");
        }
    }else if((x & STRINGMASK) == STRINGTAG){
        //printf(\"%08x\\n\", x);
        //printf(\"%08x\\n\", (int)*((ptr_t *)(x - 2)) >> FXSHIFT);
        
        int i;
        int size = (int)*((ptr_t *)(x - 2)) >> FXSHIFT;
        unsigned char ch;

        printf(\"\\\"\");
        for(i=0; i < size; i++){
            ch = (unsigned char)*((ptr_t *)(x + 2 + i));

            if(ch == '\\0') continue;

            if(ch == '\\\"'){
                printf(\"\\\\\\\"\");
            }else if(ch == '\\\\'){
                printf(\"\\\\\\\\\");
            }else if(ch > 32 && ch < 127){
                printf(\"%c\", ch);
            }else if(ch < 128){
                printf(\"%s\", character_table[(int)ch]);
            }else{
                printf(\"#\\\\%02X\", ch);
            }
        }
        printf(\"\\\"\");
    }else{
        printf(\"#<unknown 0x%08X>\", x);
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
    printf(\"\\n\");

    deallocate_protected_space(heap_top, heap_size);
    deallocate_protected_space(stack_top, stack_size);
    return 0;
}
" 'cons)) tmp)
    (system:string "/usr/bin/gcc -O3 -Wall -c " 
                   tmp
                   " -o "
                   driver-object)
    (system:string "/bin/rm -f " tmp)
    driver-object))
