; ELF COPY dummy references
section .text exec nowrite  
; orig ea=0x601060
extern stderr
	dq stderr
; -------------- ELF Copy Externs and Aliases (pin=(bss)) ---------------
%define $stderr@@GLIBC_2.2.5 $label_stderr@@GLIBC_2.2.5

; -------------- Renames ---------------

; -------------- Overlap aliases ---------------

; -------------- Externs ---------------
extern $fprintf
extern $__libc_start_main
extern $fwrite
extern $atof
extern $__gmon_start__
extern $printf

; -------------- Globals (defs) ---------------
global $loc_40062A:function 
global $loc_400640:function 
global $loc_400874:function 
global $unk_400928:data 
global $_start:function 
global $__do_global_dtors_aux:function 
global $loc_400811:function 
global $loc_40063B:function 
global $loc_4008F6:function 
global $unk_40092F:data 
global $loc_4006D2:function 
global $frame_dummy:function 
global $loc_400898:function 
global $loc_400747:function 
global $completed.7585:data 
global $print:function 
global $__JCR_LIST__:data 
global $unk_40094A:data 
global $_IO_stdin_used:data 
global $deregister_tm_clones:function 
global $proc_400550:function 
global $loc_4007BB:function 
global $unk_400965:data 
global $__data_start:data 
global $loc_4006DC:function 
global $loc_40089D:function 
global $__frame_dummy_init_array_entry:data 
global $loc_400608:function 
global $register_tm_clones:function 
global $stderr@@GLIBC_2.2.5:data 
global $plus:function 
global $minus:function 
global $__libc_csu_init:function 
global $__dso_handle:data 
global $__libc_csu_fini:function 
global $loc_400852:function 
global $times:function 
global $unk_400980:data 
global $loc_4007E6:function 
global $_init:function 
global $divide:function 
global $main:function 
global $_fini:function 
global $loc_4005C0:function 
global $__do_global_dtors_aux_fini_array_entry:data 
global $loc_4008E0:function 
global $loc_4004E5:function 

; -------------- Globals (exported) ---------------
global $_start:function 

; -------------- Globals (hidden) ---------------

; -------------- Stack Vars ---------------
$main.var_10 equ -16
$divide.var_10 equ -16
$main.var_11 equ -17
$main.var_30 equ -48
$times.var_8 equ -8
$times.var_10 equ -16
$main.var_8 equ -8
$divide.var_8 equ -8
$print.var_10 equ -16
$main.var_38 equ -56
$main.var_24 equ -36
$plus.var_8 equ -8
$minus.var_8 equ -8
$plus.var_10 equ -16
$minus.var_10 equ -16
$print.var_8 equ -8

; -------------- Stack --------------
section .note.GNU-stack noalloc noexec nowrite progbits

; ----------- Code & Data ------------
section .init exec nowrite  align=4
	align 4
$_init: sub rsp,8 ; orig ea=0x4004D0
	mov rax,qword [rel $__gmon_start__ wrt ..got] ; orig ea=0x4004D4
	test rax,rax ; orig ea=0x4004DB
	jz $loc_4004E5 ; orig ea=0x4004DE
	call $__gmon_start__ wrt ..plt ; orig ea=0x4004E0
$bb_fallthrough_n5_12_4004e5:
$loc_4004E5: add rsp,8 ; orig ea=0x4004E5
	ret ; orig ea=0x4004E9
jmp $bb_fallthrough_n5_12_4004e5
$gt_endsec_0: ; null-instruction ; created
section .fini_array noexec write  align=1
$__do_global_dtors_aux_fini_array_entry: dq $__do_global_dtors_aux ; orig ea=0x600E18
section .bss noexec write  align=1
$label_stderr@@GLIBC_2.2.5: resb 8
$completed.7585: resb 8
section .eh_frame noexec nowrite  align=8
$eh_cie_1_1: dd $eh_end_cie_1_1-$eh_cie_1_1 + -4 ; created
	dd 0 ; created
	db 1 ; created
	db 0x7A, 0x52 ; created
	db 0 ; orig ea=BADEA
	db 1 ; created
	db 0x78 ; created
	db 0x10 ; created
	db 0xB ; created
TIMES 2 db 0 ; orig ea=BADEA
	align 8, db 0
$eh_end_cie_1_1: ; null-instruction ; created
$eh_fde___libc_csu_fini: dd $end_eh_fde___libc_csu_fini-$eh_fde___libc_csu_fini + -4 ; created
	dd $eh_fde___libc_csu_fini-$eh_cie_1_1 + 4 ; created
	dq $__libc_csu_fini ; created
	dq $__libc_csu_init-$__libc_csu_fini ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde___libc_csu_fini: ; null-instruction ; created
$eh_fde___libc_csu_init: dd $end_eh_fde___libc_csu_init-$eh_fde___libc_csu_init + -4 ; created
	dd $eh_fde___libc_csu_init-$eh_cie_1_1 + 4 ; created
	dq $__libc_csu_init ; created
	dq $main-$__libc_csu_init ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_4008A2 ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x8F ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_4008A4 ; created
	db 0xE ; created
	db 0x18 ; created
	db 0x8E ; created
	db 3 ; created
	db 1 ; created
	dq $eh_label_at_4008A9 ; created
	db 0xE ; created
	db 0x20 ; created
	db 0x8D ; created
	db 4 ; created
	db 1 ; created
	dq $eh_label_at_4008AB ; created
	db 0xE ; created
	db 0x28 ; created
	db 0x8C ; created
	db 5 ; created
	db 1 ; created
	dq $eh_label_at_4008B3 ; created
	db 0xE ; created
	db 0x30 ; created
	db 0x86 ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_4008BB ; created
	db 0xE ; created
	db 0x38 ; created
	db 0x83 ; created
	db 7 ; created
	db 1 ; created
	dq $eh_label_at_4008C8 ; created
	db 0xE ; created
	db 0x40 ; created
	db 1 ; created
	dq $eh_label_at_4008FA ; created
	db 0xE ; created
	db 0x38 ; created
	db 1 ; created
	dq $eh_label_at_4008FB ; created
	db 0xE ; created
	db 0x30 ; created
	db 1 ; created
	dq $eh_label_at_4008FC ; created
	db 0xE ; created
	db 0x28 ; created
	db 1 ; created
	dq $eh_label_at_4008FE ; created
	db 0xE ; created
	db 0x20 ; created
	db 1 ; created
	dq $eh_label_at_400900 ; created
	db 0xE ; created
	db 0x18 ; created
	db 1 ; created
	dq $eh_label_at_400902 ; created
	db 0xE ; created
	db 0x10 ; created
	db 1 ; created
	dq $eh_label_at_400904 ; created
	db 0xE ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde___libc_csu_init: ; null-instruction ; created
$eh_fde_main: dd $end_eh_fde_main-$eh_fde_main + -4 ; created
	dd $eh_fde_main-$eh_cie_1_1 + 4 ; created
	dq $main ; created
	dq $print-$main ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_40070B ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_40070E ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_40089E ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_main: ; null-instruction ; created
$eh_fde_print: dd $end_eh_fde_print-$eh_fde_print + -4 ; created
	dd $eh_fde_print-$eh_cie_1_1 + 4 ; created
	dq $print ; created
	dq $divide-$print ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_4006DF ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_4006E2 ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_400709 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_print: ; null-instruction ; created
$eh_fde_divide: dd $end_eh_fde_divide-$eh_fde_divide + -4 ; created
	dd $eh_fde_divide-$eh_cie_1_1 + 4 ; created
	dq $divide ; created
	dq $times-$divide ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_4006A5 ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_4006A8 ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_4006DD ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_divide: ; null-instruction ; created
$eh_fde_times: dd $end_eh_fde_times-$eh_fde_times + -4 ; created
	dd $eh_fde_times-$eh_cie_1_1 + 4 ; created
	dq $times ; created
	dq $minus-$times ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_40068B ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_40068E ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_4006A3 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_times: ; null-instruction ; created
$eh_fde_minus: dd $end_eh_fde_minus-$eh_fde_minus + -4 ; created
	dd $eh_fde_minus-$eh_cie_1_1 + 4 ; created
	dq $minus ; created
	dq $plus-$minus ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_400671 ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_400674 ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_400689 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_minus: ; null-instruction ; created
$eh_fde_plus: dd $end_eh_fde_plus-$eh_fde_plus + -4 ; created
	dd $eh_fde_plus-$eh_cie_1_1 + 4 ; created
	dq $plus ; created
	dq $frame_dummy-$plus ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 1 ; created
	dq $eh_label_at_400657 ; created
	db 0xE ; created
	db 0x10 ; created
	db 0x86 ; created
	db 2 ; created
	db 1 ; created
	dq $eh_label_at_40065A ; created
	db 0xD ; created
	db 6 ; created
	db 1 ; created
	dq $eh_label_at_40066F ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde_plus: ; null-instruction ; created
$eh_fde__start: dd $end_eh_fde__start-$eh_fde__start + -4 ; created
	dd $eh_fde__start-$eh_cie_1_1 + 4 ; created
	dq $_start ; created
	dq $gt_endsec_3-$_start ; created
	db 8 ; created
	dq 0 ; created
	db 0xC ; created
	db 7 ; created
	db 8 ; created
	db 0x90 ; created
	db 1 ; created
	db 7 ; created
	db 0x10 ; created
	db 0 ; orig ea=BADEA
	align 8, db 0
$end_eh_fde__start: ; null-instruction ; created
section .rodata noexec nowrite  align=1
$_IO_stdin_used: dq 0x20001 ; orig ea=0x400920
$unk_400928: db '%.4lf', 0xA, 0 ; orig ea=0x400928
$unk_40092F: db 'Usage: [op] [num1] [num2]', 0xA, 0 ; orig ea=0x40092F
$unk_40094A: db 'Error: cannot divide by 0', 0xA, 0 ; orig ea=0x40094A
$unk_400965: db 'Unrecognized operator %c', 0xA, 0 ; orig ea=0x400965
	db 0 ; orig ea=0x40097F
$unk_400980: dq 0x7FF8000000000000 ; orig ea=0x400980
section .jcr noexec write  align=1
$__JCR_LIST__: dq 0 ; orig ea=0x600E20
section .fini exec nowrite  align=4
	align 4
$_fini: sub rsp,8 ; orig ea=0x400914
	add rsp,8 ; orig ea=0x400918
	ret ; orig ea=0x40091C
$gt_endsec_1: ; null-instruction ; created
section .init_array noexec write  align=1
$__frame_dummy_init_array_entry: dq $frame_dummy ; orig ea=0x600E10
section .data noexec write  align=1
$__data_start: dq 0 ; orig ea=0x601040
$__dso_handle: dq 0 ; orig ea=0x601048
section .plt.got exec nowrite  align=8
	dw 0x9066 ; orig ea=0x400556
	align 4
$proc_400550: jmp qword [rel $__gmon_start__ wrt ..got] ; orig ea=0x400550
$gt_endsec_2: ; null-instruction ; created
section .text exec nowrite  align=16
	db 0x66, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x4005B7
	db 0xF, 0x1F, 0x40, 0, 0x66, 0x2E, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x4005C2
	db 0xF, 0x1F, 0 ; orig ea=0x400605
	db 0x66, 0xF, 0x1F, 0x44, 0, 0 ; orig ea=0x40060A
	dd 0x401F0F ; orig ea=0x40062C
	db 0xF, 0x1F, 0 ; orig ea=0x40063D
	db 0x90 ; orig ea=0x40089F
	db 0x90, 0x66, 0x2E, 0xF, 0x1F, 0x84, 0, 0, 0, 0, 0 ; orig ea=0x400905
	align 4
$__libc_csu_fini: ret ; orig ea=0x400910
	align 4
$__libc_csu_init: push r15 ; orig ea=0x4008A0
$eh_label_at_4008A2: push r14 ; orig ea=0x4008A2
$eh_label_at_4008A4: mov r15d,edi ; orig ea=0x4008A4
	push r13 ; orig ea=0x4008A7
$eh_label_at_4008A9: push r12 ; orig ea=0x4008A9
$eh_label_at_4008AB: lea r12,[rel $__frame_dummy_init_array_entry] ; orig ea=0x4008AB
	push rbp ; orig ea=0x4008B2
$eh_label_at_4008B3: lea rbp,[rel $__do_global_dtors_aux_fini_array_entry] ; orig ea=0x4008B3
	push rbx ; orig ea=0x4008BA
$eh_label_at_4008BB: mov r14,rsi ; orig ea=0x4008BB
	mov r13,rdx ; orig ea=0x4008BE
	sub rbp,r12 ; orig ea=0x4008C1
	sub rsp,8 ; orig ea=0x4008C4
$eh_label_at_4008C8: sar rbp,3 ; orig ea=0x4008C8
	call $_init ; orig ea=0x4008CC
$bb_fallthrough_n35_22_4008d1:
	test rbp,rbp ; orig ea=0x4008D1
	jz $loc_4008F6 ; orig ea=0x4008D4
	xor ebx,ebx ; orig ea=0x4008D6
	nop ; orig ea=0x4008D8
$loc_4008E0: mov rdx,r13 ; orig ea=0x4008E0
	mov rsi,r14 ; orig ea=0x4008E3
	mov edi,r15d ; orig ea=0x4008E6
	call qword [r12+rbx*8] ; orig ea=0x4008E9
$bb_fallthrough_n35_32_4008ed:
	add rbx,1 ; orig ea=0x4008ED
	cmp rbx,rbp ; orig ea=0x4008F1
	jnz $loc_4008E0 ; orig ea=0x4008F4
$loc_4008F6: add rsp,8 ; orig ea=0x4008F6
$eh_label_at_4008FA: pop rbx ; orig ea=0x4008FA
$eh_label_at_4008FB: pop rbp ; orig ea=0x4008FB
$eh_label_at_4008FC: pop r12 ; orig ea=0x4008FC
$eh_label_at_4008FE: pop r13 ; orig ea=0x4008FE
$eh_label_at_400900: pop r14 ; orig ea=0x400900
$eh_label_at_400902: pop r15 ; orig ea=0x400902
$eh_label_at_400904: ret ; orig ea=0x400904
jmp $bb_fallthrough_n35_22_4008d1
	align 4
$main: push rbp ; orig ea=0x40070A
$eh_label_at_40070B: mov rbp,rsp ; orig ea=0x40070B
$eh_label_at_40070E: sub rsp,0x40 ; orig ea=0x40070E
	mov dword [rbp+$main.var_24],edi ; orig ea=0x400712
	mov qword [rbp+$main.var_30],rsi ; orig ea=0x400715
	cmp dword [rbp+$main.var_24],4 ; orig ea=0x400719
	jz $loc_400747 ; orig ea=0x40071D
	mov rax,qword [rel $stderr@@GLIBC_2.2.5] ; orig ea=0x40071F
	mov rcx,rax ; orig ea=0x400726
	mov edx,0x1A ; orig ea=0x400729
	mov esi,1 ; orig ea=0x40072E
	mov edi,$unk_40092F ; orig ea=0x400733
	call $fwrite wrt ..plt ; orig ea=0x400738
$bb_fallthrough_n34_20_40073d:
	mov eax,1 ; orig ea=0x40073D
	jmp $loc_40089D ; orig ea=0x400742
$loc_400747: mov rax,qword [rbp+$main.var_30] ; orig ea=0x400747
	add rax,8 ; orig ea=0x40074B
	mov rax,qword [rax] ; orig ea=0x40074F
	movzx eax,byte [rax] ; orig ea=0x400752
	mov byte [rbp+$main.var_11],al ; orig ea=0x400755
	mov rax,qword [rbp+$main.var_30] ; orig ea=0x400758
	add rax,0x10 ; orig ea=0x40075C
	mov rax,qword [rax] ; orig ea=0x400760
	mov rdi,rax ; orig ea=0x400763
	call $atof wrt ..plt ; orig ea=0x400766
$bb_fallthrough_n34_34_40076b:
	movq rax,xmm0 ; orig ea=0x40076B
	mov qword [rbp+$main.var_10],rax ; orig ea=0x400770
	mov rax,qword [rbp+$main.var_30] ; orig ea=0x400774
	add rax,0x18 ; orig ea=0x400778
	mov rax,qword [rax] ; orig ea=0x40077C
	mov rdi,rax ; orig ea=0x40077F
	call $atof wrt ..plt ; orig ea=0x400782
$bb_fallthrough_n34_43_400787:
	movq rax,xmm0 ; orig ea=0x400787
	mov qword [rbp+$main.var_8],rax ; orig ea=0x40078C
	cmp byte [rbp+$main.var_11],0x2B ; orig ea=0x400790
	jnz $loc_4007BB ; orig ea=0x400794
	movsd xmm0,[rbp+$main.var_8] ; orig ea=0x400796
	mov rax,qword [rbp+$main.var_10] ; orig ea=0x40079B
	movupd xmm1,xmm0 ; orig ea=0x40079F
	mov qword [rbp+$main.var_38],rax ; orig ea=0x4007A3
	movsd xmm0,[rbp+$main.var_38] ; orig ea=0x4007A7
	call $plus ; orig ea=0x4007AC
$bb_fallthrough_n34_55_4007b1:
	call $print ; orig ea=0x4007B1
$bb_fallthrough_n34_58_4007b6:
	jmp $loc_400898 ; orig ea=0x4007B6
$loc_4007BB: cmp byte [rbp+$main.var_11],0x2D ; orig ea=0x4007BB
	jnz $loc_4007E6 ; orig ea=0x4007BF
	movsd xmm0,[rbp+$main.var_8] ; orig ea=0x4007C1
	mov rax,qword [rbp+$main.var_10] ; orig ea=0x4007C6
	movupd xmm1,xmm0 ; orig ea=0x4007CA
	mov qword [rbp+$main.var_38],rax ; orig ea=0x4007CE
	movsd xmm0,[rbp+$main.var_38] ; orig ea=0x4007D2
	call $minus ; orig ea=0x4007D7
$bb_fallthrough_n34_69_4007dc:
	call $print ; orig ea=0x4007DC
$bb_fallthrough_n34_72_4007e1:
	jmp $loc_400898 ; orig ea=0x4007E1
$loc_4007E6: cmp byte [rbp+$main.var_11],0x2A ; orig ea=0x4007E6
	jnz $loc_400811 ; orig ea=0x4007EA
	movsd xmm0,[rbp+$main.var_8] ; orig ea=0x4007EC
	mov rax,qword [rbp+$main.var_10] ; orig ea=0x4007F1
	movupd xmm1,xmm0 ; orig ea=0x4007F5
	mov qword [rbp+$main.var_38],rax ; orig ea=0x4007F9
	movsd xmm0,[rbp+$main.var_38] ; orig ea=0x4007FD
	call $times ; orig ea=0x400802
$bb_fallthrough_n34_83_400807:
	call $print ; orig ea=0x400807
$bb_fallthrough_n34_86_40080c:
	jmp $loc_400898 ; orig ea=0x40080C
$loc_400811: cmp byte [rbp+$main.var_11],0x2F ; orig ea=0x400811
	jnz $loc_400874 ; orig ea=0x400815
	pxor xmm0,xmm0 ; orig ea=0x400817
	ucomisd xmm0,[rbp+$main.var_8] ; orig ea=0x40081B
	jp $loc_400852 ; orig ea=0x400820
	pxor xmm0,xmm0 ; orig ea=0x400822
	ucomisd xmm0,[rbp+$main.var_8] ; orig ea=0x400826
	jnz $loc_400852 ; orig ea=0x40082B
	mov rax,qword [rel $stderr@@GLIBC_2.2.5] ; orig ea=0x40082D
	mov rcx,rax ; orig ea=0x400834
	mov edx,0x1A ; orig ea=0x400837
	mov esi,1 ; orig ea=0x40083C
	mov edi,$unk_40094A ; orig ea=0x400841
	call $fwrite wrt ..plt ; orig ea=0x400846
$bb_fallthrough_n34_103_40084b:
	mov eax,1 ; orig ea=0x40084B
	jmp $loc_40089D ; orig ea=0x400850
$loc_400852: movsd xmm0,[rbp+$main.var_8] ; orig ea=0x400852
	mov rax,qword [rbp+$main.var_10] ; orig ea=0x400857
	movupd xmm1,xmm0 ; orig ea=0x40085B
	mov qword [rbp+$main.var_38],rax ; orig ea=0x40085F
	movsd xmm0,[rbp+$main.var_38] ; orig ea=0x400863
	call $divide ; orig ea=0x400868
$bb_fallthrough_n34_113_40086d:
	call $print ; orig ea=0x40086D
$bb_fallthrough_n34_116_400872:
	jmp $loc_400898 ; orig ea=0x400872
$loc_400874: movsx edx,byte [rbp+$main.var_11] ; orig ea=0x400874
	mov rax,qword [rel $stderr@@GLIBC_2.2.5] ; orig ea=0x400878
	mov esi,$unk_400965 ; orig ea=0x40087F
	mov rdi,rax ; orig ea=0x400884
	mov eax,0 ; orig ea=0x400887
	call $fprintf wrt ..plt ; orig ea=0x40088C
$bb_fallthrough_n34_125_400891:
	mov eax,1 ; orig ea=0x400891
	jmp $loc_40089D ; orig ea=0x400896
$loc_400898: mov eax,0 ; orig ea=0x400898
$loc_40089D: leave ; orig ea=0x40089D
$eh_label_at_40089E: ret ; orig ea=0x40089E
jmp $bb_fallthrough_n34_20_40073d
jmp $bb_fallthrough_n34_34_40076b
jmp $bb_fallthrough_n34_43_400787
jmp $bb_fallthrough_n34_55_4007b1
jmp $bb_fallthrough_n34_58_4007b6
jmp $bb_fallthrough_n34_69_4007dc
jmp $bb_fallthrough_n34_72_4007e1
jmp $bb_fallthrough_n34_83_400807
jmp $bb_fallthrough_n34_86_40080c
jmp $bb_fallthrough_n34_103_40084b
jmp $bb_fallthrough_n34_113_40086d
jmp $bb_fallthrough_n34_116_400872
jmp $bb_fallthrough_n34_125_400891
	align 4
$print: push rbp ; orig ea=0x4006DE
$eh_label_at_4006DF: mov rbp,rsp ; orig ea=0x4006DF
$eh_label_at_4006E2: sub rsp,0x10 ; orig ea=0x4006E2
	movsd [rbp+$print.var_8],xmm0 ; orig ea=0x4006E6
	mov rax,qword [rbp+$print.var_8] ; orig ea=0x4006EB
	mov qword [rbp+$print.var_10],rax ; orig ea=0x4006EF
	movsd xmm0,[rbp+$print.var_10] ; orig ea=0x4006F3
	mov edi,$unk_400928 ; orig ea=0x4006F8
	mov eax,1 ; orig ea=0x4006FD
	call $printf wrt ..plt ; orig ea=0x400702
$bb_fallthrough_n33_17_400707:
	nop ; orig ea=0x400707
	leave ; orig ea=0x400708
$eh_label_at_400709: ret ; orig ea=0x400709
jmp $bb_fallthrough_n33_17_400707
	align 4
$divide: push rbp ; orig ea=0x4006A4
$eh_label_at_4006A5: mov rbp,rsp ; orig ea=0x4006A5
$eh_label_at_4006A8: movsd [rbp+$divide.var_8],xmm0 ; orig ea=0x4006A8
	movsd [rbp+$divide.var_10],xmm1 ; orig ea=0x4006AD
	pxor xmm0,xmm0 ; orig ea=0x4006B2
	ucomisd xmm0,[rbp+$divide.var_10] ; orig ea=0x4006B6
	jp $loc_4006D2 ; orig ea=0x4006BB
	pxor xmm0,xmm0 ; orig ea=0x4006BD
	ucomisd xmm0,[rbp+$divide.var_10] ; orig ea=0x4006C1
	jnz $loc_4006D2 ; orig ea=0x4006C6
	movsd xmm0,[rel $unk_400980] ; orig ea=0x4006C8
	jmp $loc_4006DC ; orig ea=0x4006D0
$loc_4006D2: movsd xmm0,[rbp+$divide.var_8] ; orig ea=0x4006D2
	divsd xmm0,[rbp+$divide.var_10] ; orig ea=0x4006D7
$loc_4006DC: pop rbp ; orig ea=0x4006DC
$eh_label_at_4006DD: ret ; orig ea=0x4006DD
	align 4
$times: push rbp ; orig ea=0x40068A
$eh_label_at_40068B: mov rbp,rsp ; orig ea=0x40068B
$eh_label_at_40068E: movsd [rbp+$times.var_8],xmm0 ; orig ea=0x40068E
	movsd [rbp+$times.var_10],xmm1 ; orig ea=0x400693
	movsd xmm0,[rbp+$times.var_8] ; orig ea=0x400698
	mulsd xmm0,[rbp+$times.var_10] ; orig ea=0x40069D
	pop rbp ; orig ea=0x4006A2
$eh_label_at_4006A3: ret ; orig ea=0x4006A3
	align 4
$minus: push rbp ; orig ea=0x400670
$eh_label_at_400671: mov rbp,rsp ; orig ea=0x400671
$eh_label_at_400674: movsd [rbp+$minus.var_8],xmm0 ; orig ea=0x400674
	movsd [rbp+$minus.var_10],xmm1 ; orig ea=0x400679
	movsd xmm0,[rbp+$minus.var_8] ; orig ea=0x40067E
	subsd xmm0,[rbp+$minus.var_10] ; orig ea=0x400683
	pop rbp ; orig ea=0x400688
$eh_label_at_400689: ret ; orig ea=0x400689
	align 4
$plus: push rbp ; orig ea=0x400656
$eh_label_at_400657: mov rbp,rsp ; orig ea=0x400657
$eh_label_at_40065A: movsd [rbp+$plus.var_8],xmm0 ; orig ea=0x40065A
	movsd [rbp+$plus.var_10],xmm1 ; orig ea=0x40065F
	movsd xmm0,[rbp+$plus.var_8] ; orig ea=0x400664
	addsd xmm0,[rbp+$plus.var_10] ; orig ea=0x400669
	pop rbp ; orig ea=0x40066E
$eh_label_at_40066F: ret ; orig ea=0x40066F
	align 4
$frame_dummy: mov edi,$__JCR_LIST__ ; orig ea=0x400630
	cmp qword [rdi],0 ; orig ea=0x400635
	jnz $loc_400640 ; orig ea=0x400639
$loc_40063B: jmp $register_tm_clones ; orig ea=0x40063B
$loc_400640: mov eax,0 ; orig ea=0x400640
	test rax,rax ; orig ea=0x400645
	jz $loc_40063B ; orig ea=0x400648
	push rbp ; orig ea=0x40064A
	mov rbp,rsp ; orig ea=0x40064B
	call rax ; orig ea=0x40064E
$bb_fallthrough_n28_19_400650:
	pop rbp ; orig ea=0x400650
	jmp $register_tm_clones ; orig ea=0x400651
	align 4
$__do_global_dtors_aux: cmp byte [rel $completed.7585],0 ; orig ea=0x400610
	jnz $loc_40062A ; orig ea=0x400617
	push rbp ; orig ea=0x400619
	mov rbp,rsp ; orig ea=0x40061A
	call $deregister_tm_clones ; orig ea=0x40061D
$bb_fallthrough_n27_12_400622:
	pop rbp ; orig ea=0x400622
	mov byte [rel $completed.7585],1 ; orig ea=0x400623
$loc_40062A: ret ; orig ea=0x40062A
jmp $bb_fallthrough_n27_12_400622
	align 4
$register_tm_clones: mov esi,0x601050 ; orig ea=0x4005D0
	push rbp ; orig ea=0x4005D5
	sub rsi,0x601050 ; orig ea=0x4005D6
	sar rsi,3 ; orig ea=0x4005DD
	mov rbp,rsp ; orig ea=0x4005E1
	mov rax,rsi ; orig ea=0x4005E4
	shr rax,0x3F ; orig ea=0x4005E7
	add rsi,rax ; orig ea=0x4005EB
	sar rsi,1 ; orig ea=0x4005EE
	jz $loc_400608 ; orig ea=0x4005F1
	mov eax,0 ; orig ea=0x4005F3
	test rax,rax ; orig ea=0x4005F8
	jz $loc_400608 ; orig ea=0x4005FB
	pop rbp ; orig ea=0x4005FD
	mov edi,0x601050 ; orig ea=0x4005FE
	jmp rax ; orig ea=0x400603
$loc_400608: pop rbp ; orig ea=0x400608
	ret ; orig ea=0x400609
	align 4
$deregister_tm_clones: mov eax,0x601057 ; orig ea=0x400590
	push rbp ; orig ea=0x400595
	sub rax,0x601050 ; orig ea=0x400596
	cmp rax,0xE ; orig ea=0x40059C
	mov rbp,rsp ; orig ea=0x4005A0
	jna $loc_4005C0 ; orig ea=0x4005A3
	mov eax,0 ; orig ea=0x4005A5
	test rax,rax ; orig ea=0x4005AA
	jz $loc_4005C0 ; orig ea=0x4005AD
	pop rbp ; orig ea=0x4005AF
	mov edi,0x601050 ; orig ea=0x4005B0
	jmp rax ; orig ea=0x4005B5
$loc_4005C0: pop rbp ; orig ea=0x4005C0
	ret ; orig ea=0x4005C1
	align 4
$_start: xor ebp,ebp ; orig ea=0x400560
	mov r9,rdx ; orig ea=0x400562
	pop rsi ; orig ea=0x400565
	mov rdx,rsp ; orig ea=0x400566
	and rsp,-16 ; orig ea=0x400569
	push rax ; orig ea=0x40056D
	push rsp ; orig ea=0x40056E
	mov r8,$__libc_csu_fini ; orig ea=0x40056F
	mov rcx,$__libc_csu_init ; orig ea=0x400576
	mov rdi,$main ; orig ea=0x40057D
	call $__libc_start_main wrt ..plt ; orig ea=0x400584
$bb_fallthrough_n24_18_400589:
	hlt ; orig ea=0x400589
	nop ; orig ea=0x40058A
jmp $deregister_tm_clones
$gt_endsec_3: ; null-instruction ; created

