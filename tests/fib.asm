@org 0x0000
@define COUNT 3

@include <macros>

fib:
    mv A, 0
    mv B, 1
    mv C, COUNT
.loop:
    mv D, A
    add C, B
    mv A, B
    mv B, D
    dec C
    lda [.loop]
    jnz C
    halt
