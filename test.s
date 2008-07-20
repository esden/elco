	.text
	.align 4,0x90
.globl _sheme_entry
_sheme_entry:
	movl	$42, %eax
	ret
	.subsections_via_symbols
