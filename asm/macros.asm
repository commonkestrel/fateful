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

@macro dec (%reg:reg) {
    sub %reg, 1
}

@macro jmp {
    () {
        jnz 1
    }
    (%addr:label) {
        lda %addr
        jmp
    }
}

@macro call {
    () {
        push (($ + 6) & 0xFF) ; 2 bytes
        push (($ + 4) >> 8)   ; 2 bytes
        jmp                   ; 2 bytes
    }
    (%location:label) {
        push (($ + 9) & 0xFF) ; 2 bytes
        push (($ + 7) >> 8)   ; 2 bytes
        jmp %location         ; 5 bytes
    }
}

@macro ret () {
    pop H
    pop L
    jmp
}
