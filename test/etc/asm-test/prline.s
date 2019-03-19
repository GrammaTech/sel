prline:

            pushq %rbp
            movq %rsp,%rbp
            subq $32,%rsp
            movq %rdi,-8(%rbp)
            movq %rsi,-16(%rbp)
            movl %edx,%eax
            movb %al,-20(%rbp)
            movl .L_618760(%rip),%eax
            testl %eax,%eax
            je .L_4019bd
.L_4019a0:
            movsbl -20(%rbp),%edx
            movq .L_618660(%rip),%rax
            movq %rax,%rsi
            movl $.L_416862,%edi
            movl $0,%eax
            callq printf@PLT
.L_4019bd:

            movl .L_618764(%rip),%eax
            testl %eax,%eax
            je .L_401a12
.L_4019c7:
            movq -8(%rbp),%rax
            movq %rax,%rdi
            movl $0,%eax
            callq nlscan
.L_4019d8:
            movsbl -20(%rbp),%edx
            movq .L_618790(%rip),%rax
            addq $1,%rax
            movq %rax,.L_618790(%rip)
            movq .L_618790(%rip),%rax
            movq %rax,%rsi
            movl $.L_416867,%edi
            movl $0,%eax
            callq printf@PLT
.L_401a07:
            movq -16(%rbp),%rax
            movq %rax,.L_618780(%rip)
.L_401a12:

            movl .L_618768(%rip),%eax
            testl %eax,%eax
            je .L_401a4f
.L_401a1c:
            movsbl -20(%rbp),%eax
            movq -8(%rbp),%rdx
            movq .L_618690(%rip),%rcx
            subq %rcx,%rdx
            movq %rdx,%rcx
            movq .L_618778(%rip),%rdx
            addq %rdx,%rcx
            movl %eax,%edx
            movq %rcx,%rsi
            movl $.L_41686c,%edi
            movl $0,%eax
            callq printf@PLT
.L_401a4f:

            movq stdout(%rip),%rdx
            movq -16(%rbp),%rcx
            movq -8(%rbp),%rax
            subq %rax,%rcx
            movq %rcx,%rax
            movq %rax,%rsi
            movq -8(%rbp),%rax
            movq %rdx,%rcx
            movq %rsi,%rdx
            movl $1,%esi
            movq %rax,%rdi
            callq fwrite@PLT
.L_401a7e:
            movq stdout(%rip),%rax
            movq %rax,%rdi
            callq ferror@PLT
.L_401a8d:
            testl %eax,%eax
            je .L_401aa9
.L_401a91:
            callq __errno_location@PLT
.L_401a96:
            movl (%rax),%eax
            movl %eax,%esi
            movl $.L_416872,%edi
            movl $0,%eax
            callq error
.L_401aa9:

            movq -16(%rbp),%rax
            movq %rax,.L_618788(%rip)
            nop
            leave 
            retq 
