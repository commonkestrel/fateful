# Fateful
![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/commonkestrel/fateful/rust.yml)

Fateful is a CLI tool foring with my custom CPU, F8ful.
It contains an emulator and an assembler.
Fateful can be installed through [cargo](https://github.com/rust-lang/cargo) via `cargo install --git https://github.com/commonkestrel/fateful`.

## Assembler

The assembler can be used with the `fateful asm` or `fateful assemble` command to assembler f8ful assembly into f8ful machine code.
The input and output are both optional, and default to `stdin` and `stdout` respectively.

### Instructions

There are sixteen instructions in f8ful assembly.

## Emulator

## Tests

## Peripherals
Peripherals are a way to extend the emulator,
simulating a memory-mapped peripheral.
This is through the use of dynamic library loading,
so you can create a peripheral in any language that supports the C ABI.

There is a Rust crate ([`fateful_peripheral`](https://github.com/commonkestrel/fateful_peripheral))
to make creating peripherals easy.
