/// calculates the 8th fibonacci number and leaves the result in register D

@org 0x0000
@define COUNT 7

_start:
    mv A, COUNT
    call [fib]
    halt

fib:
    mv C, A
    mv A, 0
    mv B, 1
.loop:
    mv D, A
    add D, B
    mv A, B
    mv B, D
    dec C
    lda [.loop]
    jnz C
    ret
