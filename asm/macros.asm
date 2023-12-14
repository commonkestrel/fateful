/// Adds two 16-bit integers
@macro add16 (%h0:reg, %l0:reg, %h1:reg|imm, %l1:reg|imm) {
    add %l0, %l1
    adc %h0, %h1
}

/// Subtracts two 16-bit integers
@macro sub16 (%h0:reg, %l0:reg, %h1:reg|imm, %l1:reg|imm) {
    sub %l0, %l1
    sbc %h0, %h1
}

@macro jmp {
    () {
        jnz 0
    }
    (%addr:addr) {
        lda %addr
        jmp
    }
}
