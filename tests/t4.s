#Test case 4: Adding many registers
	.text
main:	add	$t0, $s0, $0
	add	$t1, $s1, $0
	add	$t2, $t1, $t0
	add	$t3, $t2, $t1
	add	$t4, $t3, $t2
	add	$t5, $t4, $t3
	add	$t6, $t5, $t4
	li	$v0, 5
	syscall