@ifndef MUL_ASM
@define MUL_ASM

/// Multiplies the most recent numbers on the stack, pushing the result back to the stack
mul:
    mv A, 0
    pop C
    pop B
.loop:
    add A, C
    dec B
    jnz B, [.loop]
    push A
    ret

/// Multiples the top 16-bit integers on the stack, pushing the result back to the stack,
/// with high at the top of the stack
mul16:
    pop H, L ; save return address

    mv A, 0
    mv B, 0
    pop E ; H0
    pop F ; L0
    pop C ; H1
    pop D ; L1

    push L, H ; store return address
.loop:
    add16 A, B, E, F
    dec C, D
    jnz C, [.loop]
    jnz D, [.loop]

    pop H, L ; save return address 
    push B, A ; LO, HO
    push L, H ; store return address

    ret

@endif