# Architecture

## Control Word

The control word toggles operations inside the CPU,
and enables CPU control based on instructions.

This control word changes every clock cycle,
and is determined in 3 ROM chips inside the control circuit
based on the current instruction, the instruction immediate bit,
and the clock cycle.

### ALU opcode

The first three bits of the control word (`AOL`, `AOM`, and `AOH`) represent the ALU opcode.
ALU operations based on these opcodes are shown below:

| `AOL` | `AOM` | `AOH` | Operation |
|-------|-------|-------|-----------|
|  `0`  |  `0`  |  `0`  |   `ADD`   |
|  `0`  |  `0`  |  `1`  |   `SUB`   |
|  `0`  |  `1`  |  `0`  |   `ADC`   |
|  `0`  |  `1`  |  `0`  |   `SBC`   |
|  `1`  |  `0`  |  `0`  |  `NAND`   |
|  `1`  |  `0`  |  `1`  |    `OR`   |
|  `1`  |  `1`  |  `0`  |   `CMP`   |
|  `1`  |  `1`  |  `1`  |    `CZ`   |

While most of these are pretty self explanitory,
a few need a bit more explanation.

`CMP` compares the numbers in the primary and secondary registers,
and sets the `L`, `E`, and `G` flags in the Flags Register accordingly.

`CZ` stands for Check Zero.
This operation checks if the number in the primary register is zero,
and sets the `Z` flag in the Flags Register accordingly.
