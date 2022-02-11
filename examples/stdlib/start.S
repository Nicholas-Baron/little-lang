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

_start:
#argc is at rsp
    mov rax, [rsp]
    mov [argc], rax

#argv is one off rsp
    lea rax, [rsp + 8]
    mov [argv], rax

    mov rdi, rax

# envp starts after the null of argv
    mov rax, [argv]
find_null:
    mov rdi, [rax]
    cmp rdi, 0
    je store_envp

    add rax, 8
    jmp find_null

store_envp:
    add rax, 8
    mov [envp], rax

    call main

exit:
    mov ax, 60
    xor rdi, rdi
    syscall
