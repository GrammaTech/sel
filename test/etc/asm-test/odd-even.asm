; -------------- ELF Copy Externs and Aliases (pin=#f) ---------------
extern $stderr
%define $stderr@@GLIBC_2.2.5 $stderr

; -------------- Renames ---------------

; -------------- Overlap aliases ---------------

; -------------- Externs ---------------
extern $puts
extern $atoi
extern $__init_array_start
extern $__libc_start_main
extern $fwrite
extern $__init_array_end
extern $__gmon_start__

; -------------- Globals (defs) ---------------
global $loc_400690:function 
global $_IO_stdin_used:data 
global $loc_40069A:function 
global $loc_400619:function 
global $is_even:function 
global $deregister_tm_clones:function 
global $_init:function 
global $loc_4005DB:function 
global $loc_400560:function 
global $completed.7594:data 
global $loc_4006F0:function 
global $loc_400660:function 
global $main:function 
global $loc_400624:function 
global $_start:function 
global $__data_start:data 
global $unk_400750:data 
global $_fini:function 
global $loc_400706:function 
global $loc_40069F:function 
global $frame_dummy:function 
global $__frame_dummy_init_array_entry:data 
global $loc_400608:function 
global $unk_400734:data 
global $register_tm_clones:function 
global $__do_global_dtors_aux:function 
global $loc_40048D:function 
global $stderr@@GLIBC_2.2.5:data 
global $loc_4005CA:function 
global $loc_4005E0:function 
global $__libc_csu_init:function 
global $loc_400615:function 
global $__do_global_dtors_aux_fini_array_entry:data 
global $unk_40074B:data 
global $__JCR_LIST__:data 
global $__libc_csu_fini:function 
global $loc_4005A8:function 
global $__dso_handle:data 

; -------------- Globals (exported) ---------------
global $_start:function 

; -------------- Globals (hidden) ---------------

; -------------- Stack Vars ---------------
$main.var_14 equ -20
$main.var_20 equ -32
$main.var_4 equ -4
$is_even.var_4 equ -4

; -------------- Stack --------------
section .note.GNU-stack noalloc noexec nowrite progbits

; ----------- Code & Data ------------
section .init exec nowrite  align=4
	align 4
$_init: sub rsp,8 ; orig ea=0x400478
	mov rax,qword [rel $__gmon_start__ wrt ..got] ; orig ea=0x40047C
	test rax,rax ; orig ea=0x400483
	jz $loc_40048D ; orig ea=0x400486
	call $__gmon_start__ wrt ..plt ; orig ea=0x400488
$bb_fallthrough_n5_12_40048d:
$loc_40048D: add rsp,8 ; orig ea=0x40048D
	ret ; orig ea=0x400491
section .plt.got exec nowrite  align=8
	dw 0x9066 ; orig ea=0x4004F6
section .text exec nowrite  align=16
	db 0x66, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x400557
	db 0xF, 0x1F, 0x40, 0, 0x66, 0x2E, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x400562
	db 0xF, 0x1F, 0 ; orig ea=0x4005A5
	db 0x66, 0xF, 0x1F, 0x44, 0, 0 ; orig ea=0x4005AA
	dd 0x401F0F ; orig ea=0x4005CC
	db 0xF, 0x1F, 0 ; orig ea=0x4005DD
	db 0x66, 0x2E, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0, 0xF, 0x1F, 0x44, 0, 0 ; orig ea=0x4006A1
	db 0x90, 0x66, 0x2E, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x400715
	align 4
$_start: xor ebp,ebp ; orig ea=0x400500
	mov r9,rdx ; orig ea=0x400502
	pop rsi ; orig ea=0x400505
	mov rdx,rsp ; orig ea=0x400506
	and rsp,-16 ; orig ea=0x400509
	push rax ; orig ea=0x40050D
	push rsp ; orig ea=0x40050E
	mov r8,$__libc_csu_fini ; orig ea=0x40050F
	mov rcx,$__libc_csu_init ; orig ea=0x400516
	mov rdi,$main ; orig ea=0x40051D
	call $__libc_start_main wrt ..plt ; orig ea=0x400524
$bb_fallthrough_n21_18_400529:
	hlt ; orig ea=0x400529
	nop ; orig ea=0x40052A
jmp $deregister_tm_clones
	align 4
$deregister_tm_clones: mov eax,0x60104F ; orig ea=0x400530
	push rbp ; orig ea=0x400535
	sub rax,0x601048 ; orig ea=0x400536
	cmp rax,0xE ; orig ea=0x40053C
	mov rbp,rsp ; orig ea=0x400540
	jna $loc_400560 ; orig ea=0x400543
	mov eax,0 ; orig ea=0x400545
	test rax,rax ; orig ea=0x40054A
	jz $loc_400560 ; orig ea=0x40054D
	pop rbp ; orig ea=0x40054F
	mov edi,0x601048 ; orig ea=0x400550
	jmp rax ; orig ea=0x400555
$loc_400560: pop rbp ; orig ea=0x400560
	ret ; orig ea=0x400561
	align 4
$register_tm_clones: mov esi,0x601048 ; orig ea=0x400570
	push rbp ; orig ea=0x400575
	sub rsi,0x601048 ; orig ea=0x400576
	sar rsi,3 ; orig ea=0x40057D
	mov rbp,rsp ; orig ea=0x400581
	mov rax,rsi ; orig ea=0x400584
	shr rax,0x3F ; orig ea=0x400587
	add rsi,rax ; orig ea=0x40058B
	sar rsi,1 ; orig ea=0x40058E
	jz $loc_4005A8 ; orig ea=0x400591
	mov eax,0 ; orig ea=0x400593
	test rax,rax ; orig ea=0x400598
	jz $loc_4005A8 ; orig ea=0x40059B
	pop rbp ; orig ea=0x40059D
	mov edi,0x601048 ; orig ea=0x40059E
	jmp rax ; orig ea=0x4005A3
$loc_4005A8: pop rbp ; orig ea=0x4005A8
	ret ; orig ea=0x4005A9
	align 4
$__do_global_dtors_aux: cmp byte [rel $completed.7594],0 ; orig ea=0x4005B0
	jnz $loc_4005CA ; orig ea=0x4005B7
	push rbp ; orig ea=0x4005B9
	mov rbp,rsp ; orig ea=0x4005BA
	call $deregister_tm_clones ; orig ea=0x4005BD
$bb_fallthrough_n24_12_4005c2:
	pop rbp ; orig ea=0x4005C2
	mov byte [rel $completed.7594],1 ; orig ea=0x4005C3
$loc_4005CA: ret ; orig ea=0x4005CA
jmp $bb_fallthrough_n24_12_4005c2
	align 4
$frame_dummy: mov edi,$__JCR_LIST__ ; orig ea=0x4005D0
	cmp qword [rdi],0 ; orig ea=0x4005D5
	jnz $loc_4005E0 ; orig ea=0x4005D9
$loc_4005DB: jmp $register_tm_clones ; orig ea=0x4005DB
$loc_4005E0: mov eax,0 ; orig ea=0x4005E0
	test rax,rax ; orig ea=0x4005E5
	jz $loc_4005DB ; orig ea=0x4005E8
	push rbp ; orig ea=0x4005EA
	mov rbp,rsp ; orig ea=0x4005EB
	call rax ; orig ea=0x4005EE
$bb_fallthrough_n25_19_4005f0:
	pop rbp ; orig ea=0x4005F0
	jmp $register_tm_clones ; orig ea=0x4005F1
	align 4
$is_even: push rbp ; orig ea=0x4005F6
	mov rbp,rsp ; orig ea=0x4005F7
	mov dword [rbp+$is_even.var_4],edi ; orig ea=0x4005FA
	cmp dword [rbp+$is_even.var_4],0 ; orig ea=0x4005FD
	jns $loc_400619 ; orig ea=0x400601
	neg dword [rbp+$is_even.var_4] ; orig ea=0x400603
	jmp $loc_400619 ; orig ea=0x400606
$loc_400608: cmp dword [rbp+$is_even.var_4],1 ; orig ea=0x400608
	jnz $loc_400615 ; orig ea=0x40060C
	mov eax,0 ; orig ea=0x40060E
	jmp $loc_400624 ; orig ea=0x400613
$loc_400615: sub dword [rbp+$is_even.var_4],2 ; orig ea=0x400615
$loc_400619: cmp dword [rbp+$is_even.var_4],0 ; orig ea=0x400619
	jg $loc_400608 ; orig ea=0x40061D
	mov eax,1 ; orig ea=0x40061F
$loc_400624: pop rbp ; orig ea=0x400624
	ret ; orig ea=0x400625
	align 4
$main: push rbp ; orig ea=0x400626
	mov rbp,rsp ; orig ea=0x400627
	sub rsp,0x20 ; orig ea=0x40062A
	mov dword [rbp+$main.var_14],edi ; orig ea=0x40062E
	mov qword [rbp+$main.var_20],rsi ; orig ea=0x400631
	cmp dword [rbp+$main.var_14],2 ; orig ea=0x400635
	jz $loc_400660 ; orig ea=0x400639
	mov rax,qword [rel $stderr@@GLIBC_2.2.5] ; orig ea=0x40063B
	mov rcx,rax ; orig ea=0x400642
	mov edx,0x16 ; orig ea=0x400645
	mov esi,1 ; orig ea=0x40064A
	mov edi,$unk_400734 ; orig ea=0x40064F
	call $fwrite wrt ..plt ; orig ea=0x400654
$bb_fallthrough_n27_20_400659:
	mov eax,1 ; orig ea=0x400659
	jmp $loc_40069F ; orig ea=0x40065E
$loc_400660: mov rax,qword [rbp+$main.var_20] ; orig ea=0x400660
	add rax,8 ; orig ea=0x400664
	mov rax,qword [rax] ; orig ea=0x400668
	mov rdi,rax ; orig ea=0x40066B
	call $atoi wrt ..plt ; orig ea=0x40066E
$bb_fallthrough_n27_29_400673:
	mov dword [rbp+$main.var_4],eax ; orig ea=0x400673
	mov eax,dword [rbp+$main.var_4] ; orig ea=0x400676
	mov edi,eax ; orig ea=0x400679
	call $is_even ; orig ea=0x40067B
$bb_fallthrough_n27_35_400680:
	test eax,eax ; orig ea=0x400680
	jz $loc_400690 ; orig ea=0x400682
	mov edi,$unk_40074B ; orig ea=0x400684
	call $puts wrt ..plt ; orig ea=0x400689
$bb_fallthrough_n27_41_40068e:
	jmp $loc_40069A ; orig ea=0x40068E
$loc_400690: mov edi,$unk_400750 ; orig ea=0x400690
	call $puts wrt ..plt ; orig ea=0x400695
$bb_fallthrough_n27_46_40069a:
$loc_40069A: mov eax,0 ; orig ea=0x40069A
$loc_40069F: leave ; orig ea=0x40069F
	ret ; orig ea=0x4006A0
jmp $bb_fallthrough_n27_20_400659
jmp $bb_fallthrough_n27_29_400673
jmp $bb_fallthrough_n27_35_400680
jmp $bb_fallthrough_n27_41_40068e
jmp $bb_fallthrough_n27_46_40069a
	align 4
$__libc_csu_init: push r15 ; orig ea=0x4006B0
	push r14 ; orig ea=0x4006B2
	mov r15d,edi ; orig ea=0x4006B4
	push r13 ; orig ea=0x4006B7
	push r12 ; orig ea=0x4006B9
	lea r12,[rel $__init_array_start] ; orig ea=0x4006BB
	push rbp ; orig ea=0x4006C2
	lea rbp,[rel $__init_array_end] ; orig ea=0x4006C3
	push rbx ; orig ea=0x4006CA
	mov r14,rsi ; orig ea=0x4006CB
	mov r13,rdx ; orig ea=0x4006CE
	sub rbp,r12 ; orig ea=0x4006D1
	sub rsp,8 ; orig ea=0x4006D4
	sar rbp,3 ; orig ea=0x4006D8
	call $_init ; orig ea=0x4006DC
$bb_fallthrough_n28_22_4006e1:
	test rbp,rbp ; orig ea=0x4006E1
	jz $loc_400706 ; orig ea=0x4006E4
	xor ebx,ebx ; orig ea=0x4006E6
	nop ; orig ea=0x4006E8
$loc_4006F0: mov rdx,r13 ; orig ea=0x4006F0
	mov rsi,r14 ; orig ea=0x4006F3
	mov edi,r15d ; orig ea=0x4006F6
	call qword [r12+rbx*8] ; orig ea=0x4006F9
$bb_fallthrough_n28_32_4006fd:
	add rbx,1 ; orig ea=0x4006FD
	cmp rbx,rbp ; orig ea=0x400701
	jnz $loc_4006F0 ; orig ea=0x400704
$loc_400706: add rsp,8 ; orig ea=0x400706
	pop rbx ; orig ea=0x40070A
	pop rbp ; orig ea=0x40070B
	pop r12 ; orig ea=0x40070C
	pop r13 ; orig ea=0x40070E
	pop r14 ; orig ea=0x400710
	pop r15 ; orig ea=0x400712
	ret ; orig ea=0x400714
jmp $bb_fallthrough_n28_22_4006e1
	align 4
$__libc_csu_fini: ret ; orig ea=0x400720
section .fini exec nowrite  align=4
	align 4
$_fini: sub rsp,8 ; orig ea=0x400724
	add rsp,8 ; orig ea=0x400728
	ret ; orig ea=0x40072C
section .rodata noexec nowrite  align=4
$_IO_stdin_used: dd 0x20001 ; orig ea=0x400730
$unk_400734: db 'Usage: asm-test [int]', 0xA, 0 ; orig ea=0x400734
$unk_40074B: db 'even', 0 ; orig ea=0x40074B
$unk_400750: db 'odd', 0 ; orig ea=0x400750
section .init_array noexec write  align=8
$__frame_dummy_init_array_entry: dq $frame_dummy ; orig ea=0x600E10
section .fini_array noexec write  align=8
$__do_global_dtors_aux_fini_array_entry: dq $__do_global_dtors_aux ; orig ea=0x600E18
section .jcr noexec write  align=8
$__JCR_LIST__: dq 0 ; orig ea=0x600E20
section .data noexec write  align=8
$__data_start: dq 0 ; orig ea=0x601038
$__dso_handle: dq 0 ; orig ea=0x601040
section .bss noexec write  align=32
$completed.7594: resb 8

