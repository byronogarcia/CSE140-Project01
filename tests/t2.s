#Test case 2: Attempting loop
	.text 
	addiu 	$a2, $0, 7
	addiu	$a3, $a2, 2
	
Loop:	
	bne	$a2, $a3, Done
	addiu	$a2, $a2, 1
	j Loop
Done:
	add $a2, $a3, $0