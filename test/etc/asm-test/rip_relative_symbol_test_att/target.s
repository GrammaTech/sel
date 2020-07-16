
#===================================
.text
.align 16
#===================================

debloat____do_global_dtors_aux:

            cmpb $0,__TMC_END__(%rip) # orig ea=0x4004e0
            jne .L_debloat__4004fa # orig ea=0x4004e7
.L_debloat__4004e9:

            pushq %rbp # orig ea=0x4004e9
            movq %rsp,%rbp # orig ea=0x4004ea
            callq deregister_tm_clones # orig ea=0x4004ed
.L_debloat__4004f2:

            popq %rbp # orig ea=0x4004f2
            movb $1,__TMC_END__(%rip) # orig ea=0x4004f3
.L_debloat__4004fa:

            retq  # orig ea=0x4004fa
#===================================
# end section .text
#===================================
