@ifndef ARITHMETIC_MACROS
@define ARITHMETIC_MACROS

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

/// Increments the given byte
@macro inc (%reg:reg) {
    add %reg, 1
}

/// Decrements the given byte
@macro dec (%reg:reg) {
    sub %reg, 1
}

/// Bitwise inverts the given byte
@macro not (%reg:reg) {
    nand %reg, %reg
}

/// Bitwise and
@macro and (%x:reg, %y:reg|imm) {
    nand %x, %y
    nand %x, %x
}

/// Bitwise xor
@macro xor (%x:reg, %y:reg|imm) {
    mw F, %y
    or F, %x
    nand %x, %y
    and %x, F
}

@endif
