; calculates the 8th fibonacci number and leaves the result in d

@org 0x0000
@define COUNT 7

@include <macros>

_start:
    mv A, COUNT
    call [fib]
    halt

fib:
    mv C, A
    mv A, 0
    mv B, 1
    mv C, COUNT
.loop:
    mv D, A
    add D, B
    mv A, B
    mv B, D
    dec C
    lda [.loop]
    jnz C
    ret
