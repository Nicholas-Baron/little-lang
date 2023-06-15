.intel_syntax noprefix
.data

.p2align 3
.global argc
argc: .space 8

.global argv
argv: .space 8

.global envp
envp: .space 8

.text
.global _start
.global main
.global exit
.global arg_at

_start:
#argc is at rsp
    mov rax, [rsp]
    mov [argc], rax
    mov rdi, rax

#argv is one off rsp
    lea rax, [rsp + 8]
    mov [argv], rax

# envp starts after the null of argv
    shl rdi, 3
    add rax, rdi
    add rax, 8
    mov [envp], rax

    call main

exit:
    mov ax, 60
    xor rdi, rdi
    syscall

arg_at:
	cmp [argc], rdi
	jae find_arg
	xor rax, rax
	ret

find_arg:
	mov rax, [argv]
	mov rax, [rax + 8*rdi]
	ret
