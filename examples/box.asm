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
    mv A, 1


    ret

draw_bottom:
    ret

draw_left:
    ret

draw_right:
    ret
