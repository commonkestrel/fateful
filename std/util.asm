@ifndef UTIL_MACROS
@define UTIL_MACROS

@macro push {
    ; push two registers
    (%r0:reg, %r1:reg) {
        push %r0
        push %r1
    }
    ; push three registers
    (%r0:reg, %r1:reg, %r2:reg) {
        push %r0
        push %r1, %r2
    }
    ; push four registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg) {
        push %r0
        push %r1, %r2, %r3
    }
    ; push five registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg, %r4:reg) {
        push %r0
        push %r1, %r2, %r3, %r4
    }
    ; push six registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg, %r4:reg, %r5:reg) {
        push %r0
        push %r1, %r2, %r3, %r4, %r5
    }
}

@macro pop {
    ; pop two registers
    (%r0:reg, %r1:reg) {
        pop %r0
        pop %r1
    }
    ; pop three registers
    (%r0:reg, %r1:reg, %r2:reg) {
        pop %r0
        pop %r1, %r2
    }
    ; pop four registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg) {
        pop %r0
        pop %r1, %r2, %r3
    }
    ; pop five registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg, %r4:reg) {
        pop %r0
        pop %r1, %r2, %r3, %r4
    }
    ; pop six registers
    (%r0:reg, %r1:reg, %r2:reg, %r3:reg, %r4:reg, %r5:reg) {
        pop %r0
        pop %r1, %r2, %r3, %r4, %r5
    }
}

@macro pusha () {
    push A, B, C, D, E, F
}

@macro popa () {
    pop F, E, D, C, B, A
}

@endif
