    .text
.globl _elco_entry
_elco_entry:
    movl $60, %eax
    xor $64, %al
    ret
