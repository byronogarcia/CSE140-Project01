#Test case 3: loading and storing
	.text
	addiu	$t4, $0, 0x00401004
	addiu	$t4, $t4, -2
	addiu	$s0, $0, 20
	sw	$s0, 4($t7)
	lw	$v0, 4($t7)
	
	sub	$a0, $a2, $a1