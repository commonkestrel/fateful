call [_start]

/// math = ./math
@include <math/mul.asm>

_start:
    push 5, 0, 120, 0
    call [mul16]
    pop H, L
    halt