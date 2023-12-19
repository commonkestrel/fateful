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
