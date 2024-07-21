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

Machine Code: `0x0`

Operation: Adds the first and second operand, storing the result in the first operand.

```asm
add <register>, <register/imm8>
```

#### SUB

Machine Code: `0x1`

Operation: Subtracts the second operand from the first, storing the result in the first operand.

```asm
sub <register>, <register/imm8>
```

#### ADC

Machine Code: `0x2`

Operation: Adds the first and second operands, plus an additional 1 if the carry bit is set, storing the result in the first operand.

```asm
adc <register>, <register/imm8>
```

#### SBB

Machine Code: `0x3`

Operation: Subtracts the second operand from the first, subtracting an additional 1 if the carry bit is set, storing the result in the first operand.

```asm
sbb <register>, <register/imm8>
```

#### NAND

Machine Code: `0x4`

Operation: Performs a bitwise NAND on the first and second operands, storing the result in the first operand.

```asm
nand <register>, <register/imm8>
```

#### OR

Machine Code: `0x5`

Operation: Performs a bitwise OR on the first and second operands,
storing the result in the first operand.

```asm
or <register>, <register/imm8>
```

#### CMP

Machine Code: `0x6`

Operation: Compares the first and second operands, storing the comparison results in the status register. 

```asm
cmp <register>, <register/imm8>
```

#### MV

Machine Code: `0x7`

Operation: Copies the second operand into the first operand.

```asm
mv <register>, <register/imm8>
```

#### LD

Machine Code: `0x8`

Operation: Loads the byte at either the RAM address provided, or the RAM address stored in the HL registers if none is provided, into the first operand.

```asm
ld <register>, [address]
```

#### ST

Machine Code: `0x9`

Operation: Stores the first operand into RAM at either the address provided, or the address stored in the HL registers if none is provided.

```asm
st [address,] <register>
```

#### LDA

Machine Code: `0xA`

Operation: Loads the provided 16-bit address into the HL registers.

```asm
lda <address>
```

#### LPM

Machine Code: `0xB`

Operation: Loads the byte at either the ROM address provided, or the ROM address stored in the HL registers if none is provided, into the first operand.

```asm
lpm <register>, [address]
```

#### PUSH

Machine Code: `0xC`

Operation: Stores the first operand to the RAM location currently pointed to by the stack pointer, then decrements the stack pointer. 

```asm
push <register/imm8>
```

#### POP

Machine Code: `0xD`

Operation: Increments the stack pointer, then loads the RAM location currently pointed to by the stack pointer into the first operand.

```asm
pop <register>
```

#### JNZ

Machine Code: `0xE`

Operation: Jumps to the address pointed to by the HL registers only if the first operand is not zero.

```asm
jnz <register/imm8>
```

#### HALT

Machine Code: `0xF`

Operation: Sets the H bit in the status register, halting the CPU

```asm
halt
```

### Labels

Labels help make blocks of your program easily accessible.
They consist of an identifier followed by a colon (`:`).
Labels with an identifier beginning with `.` are considered local to the most recent global label.
Local labels can also be accessed globally via `<parent>.<local>`.

Example:
```asm
parent:
    ; assembly code
.local1:
    jmp [.local1]
.local2:
    jmp [parent.local2]
```

### Preprocessor Directives

There are a variety of C-style preprocessor directives included in the assembler, indicated with a preceding `@`.
These directives can apply conditional transformations to the source before compilation.
Macros are processed in top-down order, meaning if a `@define` is placed below an `@ifdef` in the file, the define will not be in scope during the check.

#### DEFINE

The define macro links an identifier to a group of tokens.
Before compiling, each instance of this identifier is replaced with the specified tokens.

Unlike C, this does not support function-style definitions, meaning no arguments are allowed.

Syntax:
```rs
@define <identifier> <value>
```

#### UNDEF

The `@undef` macro removes (undefines) the current definition of the given identifier.
Consequently, subsequent occurrences of the identifier are ignored by the preprocessor.

Syntax:
```rs
@undef <identifier>
```

#### ERROR

The `@error` directive emits a user-specified error message before terminating the assembly.

Syntax:
```rs
@error "error message"
```

#### IF

The `@if` directive controls compilation of portions of a source file.
If the expression you write after the `@if` is greater than 0, the block following the `@if` is retained for assembly.

Syntax:
```
@if <expr>
    ...
@endif
```

#### ELIF

The `@elif` directive is only allowed as part of an `@if` block,
and is only evaluated if the previously evaluated blocks' check evaluates to 0.
Similar to the `@if` directive, if the expression you write after the `@elif` is greater than 0, the block following the `@elif` is retained for assembly.

Syntax:

```
@if <expr>
    ...
@elif <expr>
    ...
@endif
```

#### ELSE

The `@else` directive is only allowed at the end of an `@if` block.
If the expression of the previously evaluated block's check evaluates to 0,
then the block following the `@else` is retained for assembly.

Syntax:

```
@if <expr>
    ...
@else
    ...
@endif
```

#### IFDEF

The `@ifdef` directive is functionally the same as `@if 1` if the identifier has been defined,
and `@if 0` when the identifier hasn't been defined, or has been undefined by the `@undef` directive.

Syntax:
```
@ifdef <identifier>
    ...
@endif
```

#### IFNDEF

The `@ifndef` directive is functionally the same as `@if 0` if the identifier has been defined,
and `@if 1` when the identifier hasn't been defined, or has been undefined by the `@undef` directive.

Syntax:
```
@ifndef <identifier>
    ...
@endif
```

#### Include

The include macro pastes a stream of tokens from another file.
The file must be located in a package, and can be indexed by filepath relative to the root of the package.

A package is linked to an identifier through a rich comment, and can be either a local directory or a remote git repository.


Syntax:
```rs
/// <package> = <path/git repository>
@include <<package>/<file path>>
```

Example:

```rs
/// error = https://github.com/commonkestrel/f8ful_os
@include <error/error.asm>
```

### Segments

The assembly is divided into segments, specified with the `@cseg` and `@dseg` directives,
and organized by the `@org` directive.
Segments can be used to organize blocks of data and code throughout the address space.

#### Code Segments

Code segments, signified by the `@cseg` directive,
are where all of your assembly instructions are located.
Each assembly program starts in an initial code segment.

#### Data Segments

Data segments, signified by the `@dseg`,
are a block in RAM rather than the ROM.
These segments allow you to reserve blocks for global variables.
The variables defined in a data segment will reserve a RAM address while automatically avoiding collision.

Variables can be defined with a few directives.
These directives allow reserving blocks of variable size, specified here:
| Directive                  | Size   |
|----------------------------|--------|
| `@byte <identifier>`       | 1      |
| `@double <identifier>`     | 2      |
| `@quad <identifier>`       | 4      |
| `@var <size> <identifier>` | *size* |

These variables will resolve to an address at assembly, and can be accessed via *$identifier*.

#### Organization

Segments are automatically arranged to avoid collision,
but they can be manually organized with the `@org` directive.
This directive will place the origin of a segment at the address specified.
This is especially useful to make sure that the origni of your code is placed at `0x0000`,
since the program starts running from this point.

Unfortunately, manual organization can result in collisions,
so you must be careful to avoid these if manually organizing two or more segments of the same type.

### Data

You can place raw data within the program as well.
This data can be acessed with `lpm`.
Similar to variables in the data segment, these are placed with the following directives:

* `@byte <imm8>`
* `@double <imm16>`
* `@quad <imm32>`
* `@str <string>`

This data is often used in conjunction with a label in order to make it easily locatable.

Example
```c
hello:
    @str "hello world"
```

### Macros

Macros are an incredibly powerful part of this assembler, 
and are defined with the `@macro` directive.
They are similar to C's function-style `#define` macros,
but with optionally typed parameters and multiple definitions for different parameters.

Each parameter can have an accepted type, or multiple accepted types with the `|` operator.
Parameter identifiers must begin with a `%`.
These types are listed here:

* `reg`: Matches a register input (A, B, C, D, E, F, H, L)
* `imm`: Matches an immediate integer
* `addr`: Matches a RAM address
* `label`: Matches a ROM address
* `ident`: Matches any identifier
* `str`: Matches a string literal
* `any`: Matches any of the previous

Syntax:
```asm
; This syntax defines a singular signiture
@macro <identifier> (<parameters>) {
    ; assembly code
}

; This syntax allows for the definition of multiple signitures
@macro <identifier> {
    (<parameters>) {

    }

    (<parameters>) {

    }
}
```

This may be a little confusing, so we can use an example from the built-in macros (we'll get to these later):

```asm
@macro jmp {
    () {
        jnz 1
    }
    (%location:label) {
        lda %location
        jmp
    }
}
```

What are we even looking at here?
Well, this macro contains two signitures - 
one with an empty parameter list and one with a ROM address bound to the `%location` parameter.
As you can see, the second signiture contains another `jmp` instruction,
showcasing the fact that these macros are evaluated recursively.

Macros are used just like normal instructions.
For example, the `jmp` macro can be used like `jmp [foo]`,
which expands to this:

```asm
lda [foo]
jnz 1
```

### Built-in Macros

* [PUSH](#push-macro)
* [POP](#pop-macro)
* [PUSHA](#pusha-macro)
* [POPA](#popa-macro)
* [JMP](#jmp-macro)
* [JNZ](#jnz-macro)
* [JLT](#jlt-macro)
* [JLE](#jle-macro)
* [JGT](#jgt-macro)
* [JGE](#jge-macro)

#### PUSH Macro

`push r0: reg|imm, r1: reg|imm`

Pushes two values to the stack in ascending parameter order.

`push r0: reg|imm, r1: reg|imm, r2: reg|imm`

Pushes three values to the stack in ascending parameter order.

`push r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm`

Pushes four values to the stack in ascending parameter order.

`push r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm, r4: reg|imm`

Pushes five values to the stack in ascending parameter order.

`push r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm, r4: reg|imm, r5: reg|imm`

Pushes six values to the stack in ascending parameter order.

#### POP Macro

`pop r0: reg|imm, r1: reg|imm`

Pops two values from the stack in ascending parameter order.

`pop r0: reg|imm, r1: reg|imm, r2: reg|imm`

Pops three values from the stack in ascending parameter order.

`pop r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm`

Pops four values from the stack in ascending parameter order.

`pop r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm, r4: reg|imm`

Pops five values from the stack in ascending parameter order.

`pop r0: reg|imm, r1: reg|imm, r2: reg|imm, r3: reg|imm, r4: reg|imm, r5: reg|imm`

Pops six values from the stack in ascending parameter order.

#### PUSHA Macro

`pusha`

Pushes all six general-purpose registers to the stack in ascending order.
Designed to be paired with the `popa` macro.

#### POPA Macro

`popa` 

Pops the top 6 values on the stack into the six general-purpose registers in decending order.
Designed to be paired with the `pusha` macro.

#### JMP Macro

`jmp`

Jumps to the location pointed to by the HL register unconditionally.

`jmp location: label`

Jumps to *location* unconditionally.

#### JNZ Macro

`jnz condition: reg|imm, location:label`

Jumps to *location* if *condition* is not zero.

#### JLT Macro

`jlt`

Jumps to the address pointed to by the HL registers if the `L` flag in the status register is set.

`jlt location: label`

Jumps to *location* if the `L` flag in the status register is set.

`jlt x: reg, y: reg|imm`

Jumps to the location pointed to by the HL registers if *x* < *y*.

`jlt x: reg, y: reg|imm, location: label`

Jumps to *location* if *x* < *y*.

#### JLE Macro

`jle`

Jumps to the address pointed to by the HL registers if the `L` or `E` flags in the status register are set.

`jle location: label`

Jumps to *location* if the `L` or `E` flags in the status register are set.

`jle x: reg, y: reg|imm`

Jumps to the location pointed to by the HL registers if *x* <= *y*.

`jle x: reg, y: reg|imm, location: label`

Jumps to *location* if *x* <= *y*.

#### JGT Macro

`jgt`

Jumps to the address pointed to by the HL registers if the `G` flag in the status register is set.

`jgt location: label`

Jumps to *location* if the `G` flag in the status register is set.

`jgt x: reg, y: reg|imm`

Jumps to the location pointed to by the HL registers if *x* > *y*.

`jgt x: reg, y: reg|imm, location: label`

Jumps to *location* if *x* > *y*.

#### JGE Macro

`jge`

Jumps to the address pointed to by the HL registers if the `G` or `E` flags in the status register are set.

`jge location: label`

Jumps to *location* if the `G` or `E` flags in the status register are set.

`jge x: reg, y: reg|imm`

Jumps to the location pointed to by the HL registers if *x* >= *y*.

`jge x: reg, y: reg|imm, location: label`

Jumps to *location* if *x* >= *y*.

## Emulator

## Tests

## Peripherals
Peripherals are a way to extend the emulator,
simulating a memory-mapped peripheral.
This is through the use of dynamic library loading,
so you can create a peripheral in any language that supports the C ABI.

There is a Rust crate ([`fateful_peripheral`](https://github.com/commonkestrel/fateful_peripheral))
to make creating peripherals easy.
