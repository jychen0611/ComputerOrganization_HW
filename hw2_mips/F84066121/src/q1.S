main:
    addi $a0, $zero, 19654 # a0= 19654
    addi $a1, $zero, 100654  # a1=100654
    addi $a2, $zero, 10065  # a2= 10065
    jal average     # call average
    j exit

average:
    addi $sp, $sp, -4
    sw $ra, 0x0($sp)    # Save $ra register into stack

    #--------------------------------------#
    #  \^o^/   Write your code here~  \^o^/#
    
    add $t0,$a0,$a1
    add $t0,$a2,$t0
    addi $t1, $zero, 0# t1= 0
loop:
    addi $t0,$t0,-3    
    addi $t1,$t1,1
    addi $t2,$t0,-3
    slt $t3,$t2,$zero
    bne $t3,$zero,done
    j loop
done :
    add $v0,$zero,$t1
      #--------------------------------------#

    lw $ra, 0x0($sp)    # Load $ra register from stack
    addi $sp, $sp, 4
    jr $ra

exit:
