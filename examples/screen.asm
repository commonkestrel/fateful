@org 0x0000
@define BUF_START 0xF800

_start:
    mv A, 0
.loop:
    lda [BUF_START]
    add16 H, L, 0x00, A
    st A
    inc A
    jnz A, [.loop]
    halt
