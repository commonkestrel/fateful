@macro jmp {
    () {
        jnz 1
    }
    (%location:label) {
        lda %location
        jmp
    }
}

@macro jnz (%condition:reg|imm, %location:label) {
    lda %location
    jnz %condition
}

/// jump if less than
@macro jlt {
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 2)
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 2)
        nand F, F
        jnz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, (1 << 2)
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, (1 << 2)
        nand F, F
        jnz F
    }
}

/// jump if less than or equal to
@macro jle {
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, ((1 << 2) | (1 << 3))
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, ((1 << 2) | (1 << 3))
        nand F, F
        jnz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, ((1 << 2) | (1 << 3))
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, ((1 << 2) | (1 << 3))
        nand F, F
        jnz F
    }
}

/// jump if greater than
@macro jgt {
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 4)
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 4)
        nand F, F
        jnz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, (1 << 4)
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, (1 << 4)
        nand F, F
        jnz F
    }
}

/// jump if greater than or equal to
@macro jge {
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, ((1 << 4) | (1 << 3))
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, ((1 << 4) | (1 << 3))
        nand F, F
        jnz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, ((1 << 4) | (1 << 3))
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, ((1 << 4) | (1 << 3))
        nand F, F
        jnz F
    }
}

/// jump if equal
@macro jeq {
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F
    }
}

/// jump if equal
@macro jeq {
    (%x:imm, %y:imm, %location:label) {
        lda %location
        jnz (%x == %y)
    }
    (%x:reg, %y:reg|imm) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F
    }
    (%x:reg, %y:reg|imm, %location:label) {
        cmp %x, %y
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        nz F, %location
    }
    (%location:label) {
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F, %location
    }
    () {
        ld F, [0xFFFF]
        nand F, (1 << 3)
        nand F, F
        jnz F
    }
}

@macro jz (%condition:reg|imm, %location:label) {
    jeq %condition, 0, %location
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
