    .text
.globl _elco_entry
_elco_entry:
    movl $949852, %eax
    shrl $2, %eax
    notl %eax
    shll $2, %eax
    shrl $2, %eax
    notl %eax
    shll $2, %eax
    ret
