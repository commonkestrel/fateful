# Architecture

## Control Word

The control word toggles operations inside the CPU,
and enables CPU control based on instructions.

This control word changes every clock cycle,
and is determined in 3 ROM chips inside the control circuit
based on the current instruction, the instruction immediate bit,
and the clock cycle.

The control word is split into 3 bytes,
just because I couldn't find any EEPROMs with a 24-bit word.
It is layed out like so:

|           | `B7` | `B6` | `B5` | `B4` | `B3` | `B2` | `B1` | `B0` |
|-----------|------|------|------|------|------|------|------|------|
| Low Byte  | `RPA` | `RBA` | `RO` | `RI` | `AO` | `AOH` | `AOM` | `AOL` |
| Mid Byte  | `` | `` | `` | `` | `` | `` | `` | `` |
| High Byte | `` | `` | `` | `` | `` | `` | `` | `` |

### ALU Opcode

The first four bits of the control word (`AOL`, `AOM`, and `AOH`) represent the ALU opcode.
ALU operations based on these opcodes are shown below:

|  `AO` | `AO2` | `AO1` | `AO0` | Operation |
|-------|-------|-------|-------|-----------|
|  `0`  |  `0`  |  `0`  |  `0`  |   `NOP`   |
|  `0`  |  `0`  |  `0`  |  `1`  |   `CMP`   |
|  `0`  |  `0`  |  `1`  |  `0`  |    `CZ`   |
|  `0`  |  `0`  |  `1`  |  `1`  |   `ALP`   |
|  `0`  |  `1`  |  `0`  |  `0`  |   `ALS`   |
|  `1`  |  `0`  |  `0`  |  `0`  |   `ADD`   |
|  `1`  |  `0`  |  `0`  |  `1`  |   `SUB`   |
|  `1`  |  `0`  |  `1`  |  `0`  |   `ADC`   |
|  `1`  |  `0`  |  `1`  |  `0`  |   `SBC`   |
|  `1`  |  `1`  |  `0`  |  `0`  |  `NAND`   |
|  `1`  |  `1`  |  `0`  |  `1`  |    `OR`   |

The `AO` bit, or Arithmetic Operation bit, designates an arithmetic operation.
If `AO` is set, the output of the operation will be output to the bus.
When not set, the operation executed is a special operation,
or an operation that is not part of general integer arithmatic.
These need a bit more explanation.

- `NOP` (No Op) does nothing.
- `CMP` (Compare) compares the integers in the primary and secondary register,\
  setting the `L`, `E`, and `G` bits in the Flags register respectively.
- `CZ` (Check Zero) checks if the number in the primary register is `0`,\
  setting the `Z` bit in the Flags register respectively.
- `ALP` (ALU Load Primary) loads the ALU primary register from the bus.
- `ALS` (ALU Load Secondary) loads the ALU secondary register from the bus.

## Memory

There are 64kb of accessable RAM on the board,
with the top 64 addresses (`0xFFC0` - `0xFFFF`) being reserved for memory mapped peripherals.
The stack starts at `0xFFBF` and grows downward.

There are 64 addresses for memory mapped I/O to allow for expansion,
but there are a few peripherals that are required:
- `0xFFFF` Status Register: This register is written to and read by the CPU without addressing,\
  but programs can access it through memory operations. The contents of the Status Register are explained more in [Status Register](#status-register).

## Status Register


