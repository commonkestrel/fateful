# Fateful
![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/commonkestrel/fateful/rust.yml)

Fateful is a CLI tool foring with my custom CPU, F8ful.
It contains an emulator and an assembler.
Fateful can be installed through [cargo](https://github.com/rust-lang/cargo) via `cargo install --git https://github.com/commonkestrel/fateful`.

## Assembler

The assembler can be used with the `fateful asm` or `fateful assemble` command to assembler f8ful assembly into f8ful machine code.
The input and output are both optional, and default to `stdin` and `stdout` respectively.
Input is positional, being the first argument, and the output can be specified with the `-o` or `--output` flag.

### Instruction Set

0. [ADD](#add)
0. [SUB](#sub)
0. [ADC](#adc)
0. [SBB](#sbb)
0. [NAND](#nand)
0. [OR](#or)
0. [CMP](#cmp)
0. [MV](#mv)
0. [LD](#ld)
0. [ST](#st)
0. [LDA](#lda)
0. [LPM](#lpm)
0. [PUSH](#push)
0. [POP](#pop)
0. [JNZ](#jnz)
0. [HALT](#halt)

#### ADD

Machine Code: `0x00`

Operation: Adds the first and second operand, storing the result in the first operand.

```asm
add <register>, <register/imm8>
```

#### SUB

Machine Code: `0x01`

Operation: Subtracts the second operand from the first, storing the result in the first operand.

```asm
sub <register>, <register/imm8>
```

#### ADC

Machine Code: `0x02`

Operation: Adds the first and second operands, plus an additional 1 if the carry bit is set, storing the result in the first operand.

```asm
adc <register>, <register/imm8>
```

#### SBB

Machine Code: `0x03`

Operation: Subtracts the second operand from the first, subtracting an additional 1 if the carry bit is set, storing the result in the first operand.

```asm
sbb <register>, <register/imm8>
```

#### NAND

Machine Code: `0x04`

Operation: Performs a bitwise NAND on the first and second operands, storing the result in the first operand.

```asm
nand <register>, <register/imm8>
```

#### OR

Machine Code: `0x05`

Operation: Performs a bitwise OR on the first and second operands,
storing the result in the first operand.

```asm
or <register>, <register/imm8>
```

## Emulator

## Tests

## Peripherals
Peripherals are a way to extend the emulator,
simulating a memory-mapped peripheral.
This is through the use of dynamic library loading,
so you can create a peripheral in any language that supports the C ABI.

There is a Rust crate ([`fateful_peripheral`](https://github.com/commonkestrel/fateful_peripheral))
to make creating peripherals easy.
