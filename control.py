from enum import IntFlag

# TODO: Figure out how to work `IN` and `OUT` into 24 bits
class ControlWord(IntFlag):
    """
    Representation of the CPU control word.
    
    Find more in-depth explanations of flags in `Arch.md`.
    """
    
    EMPTY = 0
    """Empty set"""
    AOL = 1 << 0
    """ALU opcode low"""
    AOM = 1 << 1
    """ALU opcode middle"""
    AOH = 1 << 2
    """ALU opcode high"""
    AA = 1 << 3
    """ALU active"""
    ALP = 1 << 4
    """ALU load primary"""
    ALS = 1 << 5
    """ALU load secondary"""
    RI = 1 << 6
    """Register Bank in"""
    RO = 1 << 7
    """Register Bank out"""
    RBA = 1 << 8
    """Instruction builtin register address"""
    RPA = 1 << 9
    """Instruction primary register address"""
    SPI = 1 << 10
    """Stack Pointer increment"""
    SPD = 1 << 11
    """Stack Pointer decrement"""
    CR = 1 << 12
    """Clock reset"""
    PCI = 1 << 13
    """Program Counter increment"""
    JNZ = 1 << 14
    """Set Program Counter"""
    LI = 1 << 15
    """Load instruction"""
    PO = 1 << 16
    """Program out"""
    ST = 1 << 17
    """Swap Temp Register"""
    THL = 1 << 18
    """Transfer HIGH/LOW"""
    LA = 1 << 19
    """Load ADDRESS"""
    SA = 1 << 20
    """Store ADDRESS"""
    AL = 1 << 21
    """ADDRESS low in"""
    AH = 1 << 22
    """ADDRESS high in"""
    LSP = 1 << 23
    """Load Stack Pointer"""

BLANK: list[ControlWord] = [ControlWord.EMPTY] * (1 << 6)

def main():
    content = generate_content()
        
    wb("src/ctrl_low.rom", bytes(map(lambda x: int(x) & 0xFF, content)))
    wb("src/ctrl_mid.rom", bytes(map(lambda x: (int(x) >> 8) & 0xFF, content)))
    wb("src/ctrl_high.rom", bytes(map(lambda x: (int(x) >> 16) & 0xFF, content)))

def wb(file: str, content: bytes):
    with open(file, 'wb') as f:
        f.write(bytes(content))

def generate_content() -> list[ControlWord]:
    content = []
    
    # Yeah I know this is terrible sorry
    content += generate_add()
    content += generate_sub()
    content += generate_adc()
    content += generate_sbc()
    content += generate_nand()
    content += generate_or()
    content += generate_cmp()
    content += generate_mv()
    content += generate_ld()
    content += generate_st()
    content += generate_lda()
    content += generate_push()
    content += generate_pop()
    content += generate_jnz()
    content += generate_in()
    content += generate_out()
    
    print(len(content))

    return content

def generate_add() -> list[ControlWord]:
    add = BLANK
    
    add[0b00000] = ControlWord.LI
    add[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    add[0b00010] = ControlWord.PCI
    add[0b00011] = ControlWord.RPA | ControlWord.ALS
    add[0b00100] = ControlWord.AA | ControlWord.RBA | ControlWord.RI
    add[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    add[0b10000] = ControlWord.LI
    add[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    add[0b10010] = ControlWord.PCI
    add[0b10011] = ControlWord.PO | ControlWord.ALS
    add[0b10100] = ControlWord.AA | ControlWord.RBA | ControlWord.RI
    add[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return add

def generate_sub() -> list[ControlWord]:
    sub = BLANK
    
    sub[0b00000] = ControlWord.LI
    sub[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    sub[0b00010] = ControlWord.PCI
    sub[0b00011] = ControlWord.RPA | ControlWord.ALS
    sub[0b00100] = ControlWord.AA | ControlWord.AOL | ControlWord.RBA | ControlWord.RI
    sub[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    sub[0b10000] = ControlWord.LI
    sub[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    sub[0b10010] = ControlWord.PCI
    sub[0b10011] = ControlWord.PO | ControlWord.ALS
    sub[0b10100] = ControlWord.AA | ControlWord.AOL | ControlWord.RBA | ControlWord.RI
    sub[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return sub

def generate_adc() -> list[ControlWord]:
    adc = BLANK
    
    adc[0b00000] = ControlWord.LI
    adc[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    adc[0b00010] = ControlWord.PCI
    adc[0b00011] = ControlWord.RPA | ControlWord.ALS
    adc[0b00100] = ControlWord.AA | ControlWord.AOM | ControlWord.RBA | ControlWord.RI
    adc[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    adc[0b10000] = ControlWord.LI
    adc[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    adc[0b10010] = ControlWord.PCI
    adc[0b10011] = ControlWord.PO | ControlWord.ALS
    adc[0b10100] = ControlWord.AA | ControlWord.AOM | ControlWord.RBA | ControlWord.RI
    adc[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return adc

def generate_sbc() -> list[ControlWord]:
    sbc = BLANK
    
    sbc[0b00000] = ControlWord.LI
    sbc[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    sbc[0b00010] = ControlWord.PCI
    sbc[0b00011] = ControlWord.RPA | ControlWord.ALS
    sbc[0b00100] = ControlWord.AA | ControlWord.AOL | ControlWord.AOM | ControlWord.RBA | ControlWord.RI
    sbc[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    sbc[0b10000] = ControlWord.LI
    sbc[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    sbc[0b10010] = ControlWord.PCI
    sbc[0b10011] = ControlWord.PO | ControlWord.ALS
    sbc[0b10100] = ControlWord.AA | ControlWord.AOL | ControlWord.AOM | ControlWord.RBA | ControlWord.RI
    sbc[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return sbc

def generate_nand() -> list[ControlWord]:
    nand = BLANK
    
    nand[0b00000] = ControlWord.LI
    nand[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    nand[0b00010] = ControlWord.PCI
    nand[0b00011] = ControlWord.RPA | ControlWord.ALS
    nand[0b00100] = ControlWord.AA | ControlWord.AOH | ControlWord.RBA | ControlWord.RI
    nand[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    nand[0b10000] = ControlWord.LI
    nand[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    nand[0b10010] = ControlWord.PCI
    nand[0b10011] = ControlWord.PO | ControlWord.ALS
    nand[0b10100] = ControlWord.AA | ControlWord.AOH | ControlWord.RBA | ControlWord.RI
    nand[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return nand

def generate_or() -> list[ControlWord]:
    ctrl_or = BLANK
    
    ctrl_or[0b00000] = ControlWord.LI
    ctrl_or[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    ctrl_or[0b00010] = ControlWord.PCI
    ctrl_or[0b00011] = ControlWord.RPA | ControlWord.ALS
    ctrl_or[0b00100] = ControlWord.AA | ControlWord.AOH | ControlWord.AOL | ControlWord.RBA | ControlWord.RI
    ctrl_or[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    ctrl_or[0b10000] = ControlWord.LI
    ctrl_or[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    ctrl_or[0b10010] = ControlWord.PCI
    ctrl_or[0b10011] = ControlWord.PO | ControlWord.ALS
    ctrl_or[0b10100] = ControlWord.AA | ControlWord.AOH | ControlWord.AOL | ControlWord.RBA | ControlWord.RI
    ctrl_or[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return ctrl_or

def generate_cmp() -> list[ControlWord]:
    cmp = BLANK
    
    cmp[0b00000] = ControlWord.LI
    cmp[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    cmp[0b00010] = ControlWord.PCI
    cmp[0b00011] = ControlWord.RPA | ControlWord.ALS
    cmp[0b00100] = ControlWord.AA | ControlWord.AOH | ControlWord.AOM
    cmp[0b00101] = ControlWord.CR | ControlWord.PCI 
    
    cmp[0b10000] = ControlWord.LI
    cmp[0b10001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    cmp[0b10010] = ControlWord.PCI
    cmp[0b10011] = ControlWord.PO | ControlWord.ALS
    cmp[0b10100] = ControlWord.AA | ControlWord.AOH | ControlWord.AOM
    cmp[0b10101] = ControlWord.CR | ControlWord.PCI
    
    return cmp

def generate_mv() -> list[ControlWord]:
    mv = BLANK
    
    mv[0b00000] = ControlWord.LI
    mv[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ST
    mv[0b00010] = ControlWord.PCI
    mv[0b00011] = ControlWord.RPA | ControlWord.RI | ControlWord.ST
    mv[0b00100] = ControlWord.CR | ControlWord.PCI
    
    mv[0b10000] = ControlWord.LI
    mv[0b10001] = ControlWord.PCI
    mv[0b10010] = ControlWord.RBA | ControlWord.RI | ControlWord.PO
    mv[0b10011] = ControlWord.CR | ControlWord.PCI
    
    return mv

def generate_ld() -> list[ControlWord]:
    ld = BLANK
    
    ld[0b00000] = ControlWord.LI
    ld[0b00001] = ControlWord.THL | ControlWord.RO
    ld[0b00010] = ControlWord.LA | ControlWord.RBA | ControlWord.RI
    ld[0b00011] = ControlWord.CR | ControlWord.PCI
    
    ld[0b10000] = ControlWord.LI
    ld[0b10001] = ControlWord.PCI
    ld[0b10010] = ControlWord.AH | ControlWord.PO
    ld[0b10011] = ControlWord.PCI
    ld[0b10100] = ControlWord.AL | ControlWord.PO
    ld[0b10101] = ControlWord.LA | ControlWord.RBA | ControlWord.RI
    ld[0b10110] = ControlWord.CR | ControlWord.PCI
    
    return ld

def generate_st() -> list[ControlWord]:
    st = BLANK
    
    st[0b00000] = ControlWord.LI
    st[0b00001] = ControlWord.THL | ControlWord.RO
    st[0b00010] = ControlWord.SA | ControlWord.RBA | ControlWord.RO
    st[0b00011] = ControlWord.CR | ControlWord.PCI
    
    st[0b10000] = ControlWord.LI
    st[0b10001] = ControlWord.PCI
    st[0b10010] = ControlWord.AH | ControlWord.PO
    st[0b10011] = ControlWord.PCI
    st[0b10100] = ControlWord.AL | ControlWord.PO
    st[0b10101] = ControlWord.SA | ControlWord.RBA | ControlWord.RO
    st[0b10110] = ControlWord.CR | ControlWord.PCI
    
    return st

def generate_lda() -> list[ControlWord]:
    lda = BLANK
    
    # LDA doesn't have a register version, so we just skip to immediate
    lda[0b10000] = ControlWord.LI
    lda[0b10001] = ControlWord.PCI
    lda[0b10010] = ControlWord.PO | ControlWord.AH
    lda[0b10011] = ControlWord.PCI
    lda[0b10100] = ControlWord.PO | ControlWord.AL
    lda[0b10101] = ControlWord.THL | ControlWord.RI
    lda[0b10110] = ControlWord.CR | ControlWord.PCI
    
    return lda

def generate_push() -> list[ControlWord]:
    push = BLANK
    
    push[0b00000] = ControlWord.LI
    push[0b00001] = ControlWord.LSP
    push[0b00010] = ControlWord.RBA | ControlWord.RO | ControlWord.SA
    push[0b00011] = ControlWord.SPI
    push[0b00100] = ControlWord.CR | ControlWord.PCI
    
    push[0b10000] = ControlWord.LI
    push[0b10001] = ControlWord.LSP | ControlWord.PCI
    push[0b10010] = ControlWord.PO | ControlWord.SA
    push[0b00011] = ControlWord.SPI
    push[0b00100] = ControlWord.CR | ControlWord.PCI
    
    return push

def generate_pop() -> list[ControlWord]:
    pop = BLANK
    
    pop[0b00000] = ControlWord.LI
    pop[0b00001] = ControlWord.SPD
    pop[0b00010] = ControlWord.LSP
    pop[0b00011] = ControlWord.RBA | ControlWord.RO | ControlWord.SA
    pop[0b00100] = ControlWord.CR | ControlWord.PCI
    
    # POP doesn't have an immediate argument, so we can just skip this
    
    return pop

def generate_jnz() -> list[ControlWord]:
    jnz = BLANK
    
    jnz[0b00000] = ControlWord.LI
    jnz[0b00001] = ControlWord.RBA | ControlWord.RO | ControlWord.ALP
    jnz[0b00010] = ControlWord.AOL | ControlWord.AOM | ControlWord.AOH
    jnz[0b00011] = ControlWord.JNZ
    jnz[0b00100] = ControlWord.CR
    
    jnz[0b10000] = ControlWord.LI
    jnz[0b10001] = ControlWord.PCI
    jnz[0b10010] = ControlWord.PO | ControlWord.ALP
    jnz[0b10011] = ControlWord.AOL | ControlWord.AOM | ControlWord.AOH
    jnz[0b10100] = ControlWord.JNZ
    jnz[0b10101] = ControlWord.CR
    
    return jnz

def generate_in() -> list[ControlWord]:
    ctrl_in = BLANK
    
    # TODO: Finish `IN`
    
    return ctrl_in

def generate_out() -> list[ControlWord]:
    out = BLANK
    
    # TODO: Finish `OUT`
    
    return out

if __name__ == "__main__":
    main()
