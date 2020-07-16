
#===================================
.text
.align 16
#===================================

deregister_tm_clones:

            movl $.L_60103f,%eax # orig ea=0x400460
            pushq %rbp # orig ea=0x400465
            subq $__TMC_END__,%rax # orig ea=0x400466
            cmpq $14,%rax # orig ea=0x40046c
            movq %rsp,%rbp # orig ea=0x400470
            jbe .L_400490 # orig ea=0x400473
.L_400475:

            movl $0,%eax # orig ea=0x400475
            testq %rax,%rax # orig ea=0x40047a
            je .L_400490 # orig ea=0x40047d
.L_40047f:

            popq %rbp # orig ea=0x40047f
            movl $__TMC_END__,%edi # orig ea=0x400480
            jmpq *%rax # orig ea=0x400485
.L_400490:

            popq %rbp # orig ea=0x400490
            retq  # orig ea=0x400491
#===================================
# end section .text
#===================================
