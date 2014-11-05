;	.section .mdebug.abi32
;	.previous
;	.gnu_attribute 4, 1
	.text
	.align	2
	.globl	_start
	.globl _main
	.set	nomips16
	.ent	_start
	.type	_start, @function
_start:
	li $sp, 0x3000000
	li $gp, 0x2000000
	li $fp, 0x3000000

	li $t0, 0x88000000
	lw $t0, 0x04($t0)
	sll $t0, $t0, 20
	add $sp, $sp, $t0
	add $fp, $fp, $t0
	
	jal     init_lib
        jal	main
    
        li $t0, 0x80000000
        sw $t0, 0x04($t0)

forever:
	j	forever

	.end
