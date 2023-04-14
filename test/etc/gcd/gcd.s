	.file	"gcd.c"
	.intel_syntax noprefix
	.text
	.section	.rodata
.LC1:
	.string	"%g\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB6:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	DWORD PTR -20[rbp], edi
	mov	QWORD PTR -32[rbp], rsi
	mov	rax, QWORD PTR -32[rbp]
	add	rax, 8
	mov	rax, QWORD PTR [rax]
	mov	rdi, rax
	call	atoi@PLT
	cvtsi2sd	xmm0, eax
	movsd	QWORD PTR -16[rbp], xmm0
	mov	rax, QWORD PTR -32[rbp]
	add	rax, 16
	mov	rax, QWORD PTR [rax]
	mov	rdi, rax
	call	atoi@PLT
	cvtsi2sd	xmm0, eax
	movsd	QWORD PTR -8[rbp], xmm0
	pxor	xmm0, xmm0
	ucomisd	xmm0, QWORD PTR -16[rbp]
	jp	.L4
	pxor	xmm0, xmm0
	ucomisd	xmm0, QWORD PTR -16[rbp]
	jne	.L2
	mov	rax, QWORD PTR -8[rbp]
	movq	xmm0, rax
	lea	rdi, .LC1[rip]
	mov	eax, 1
	call	printf@PLT
.L2:
	jmp	.L4
.L7:
	movsd	xmm0, QWORD PTR -16[rbp]
	comisd	xmm0, QWORD PTR -8[rbp]
	jbe	.L11
	movsd	xmm0, QWORD PTR -16[rbp]
	subsd	xmm0, QWORD PTR -8[rbp]
	movsd	QWORD PTR -16[rbp], xmm0
	jmp	.L4
.L11:
	movsd	xmm0, QWORD PTR -8[rbp]
	subsd	xmm0, QWORD PTR -16[rbp]
	movsd	QWORD PTR -8[rbp], xmm0
.L4:
	pxor	xmm0, xmm0
	ucomisd	xmm0, QWORD PTR -8[rbp]
	jp	.L7
	pxor	xmm0, xmm0
	ucomisd	xmm0, QWORD PTR -8[rbp]
	jne	.L7
	mov	rax, QWORD PTR -16[rbp]
	movq	xmm0, rax
	lea	rdi, .LC1[rip]
	mov	eax, 1
	call	printf@PLT
	mov	eax, 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x3
3:
	.align 8
4:
