@org 0x0000
@define COUNT 7

@if COUNT == 8
something
@endif

@macro add16 {
    (%h0:reg, %l0:reg, %h1:reg|imm, %l1:reg|imm) {
        add %l0, %l1
        adc %h0, %h1
    }
}

fib:
    mv r0, 0
    mv r1, 1
    mv r2, COUNT
.loop:
    mv r3, r0
    add r3, r1
    mv r0, r1
    mv r1, r3
    dec r2
    lda [.loop]
    jnz r2
.halt:
    jmp [.halt]
