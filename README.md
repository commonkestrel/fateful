# Fateful
![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/commonkestrel/fateful/rust.yml)

Fateful is a CLI tool for working with my custom CPU, F8ful.
Very much a work in progress.

# Peripherals

Peripherals are a way to extend the emulator,
simulating a memory-mapped peripheral.
This is through the use of dynamic library loading,
so you can create a peripheral in any language that supports the C ABI.

There is a Rust crate ([`fateful_peripheral`](https://github.com/commonkestrel/fateful_peripheral))
to make creating peripherals easy.
