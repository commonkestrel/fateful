@define TEXT_BUFFER 0xF800
@define SCREEN_WIDTH 80
@define SCREEN_HEIGHT 25
@define BOX_WIDTH (SCREEN_WIDTH - 2)
@define BOX_HEIGHT (SCREEN_HEIGHT - 2)

@define TR_CORNER 0xBB 
@define BR_CORNER 0xBC
@define BL_CORNER 0xC8
@define TL_CORNER 0xC9
@define WALL 0xBA
@define DASH 0xCD

@org 0x0000
jmp [_start]

/// math = ./math
@include <math/mul.asm>

_start:
    call [draw_top]
    call [draw_bottom]
    call [draw_left]
    call [draw_right]
    halt

draw_top:
    mv A, 1 ; A contains the X coordinate
    push A, 1, TL_CORNER ; x, y, character
    call [draw_character]
    mv B, (BOX_WIDTH - 2)
.loop:
    inc A
    push A, B

    push A, 1, DASH
    call [draw_character]

    pop B, A

    dec B
    jnz B, [.loop]

    inc A
    push A, 1, TR_CORNER
    call [draw_character]

    ret

draw_bottom:
    ret

draw_left:
    ret

draw_right:
    ret

draw_character:
    pop H, L ; save return address
    pop C, B, A ; character, Y, X
    push L, H ; store return address
    push A, C ; save X coordinate and character

    push B, 0, SCREEN_WIDTH, 0
    call [mul16] ; get Y offset
    pop H, L ; get value

    pop C, A ; get character X coordinate

    add16 H, L, (TEXT_BUFFER >> 8), (TEXT_BUFFER & 0xFF) ; Shift address to text-buffer space
    add16 H, L, 0, A ; add X to address

    st C

    ret
