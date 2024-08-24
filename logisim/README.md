# Logisim Simulation

There is a Logisim circuit available for simulating the F8ful CPU.
It requires version 3.9.0 or later of Logisim Evolution.
If lines turn to error values you may be running an earlier version.

The circuit is broken up into a few major subcircuits:
 * [main](#main-circuit)
 * [register_bank](#register-bank)
 * [memory_bank](#memory-bank)
 * [alu](#alu)
 * [control](#control)
 * [sreg](#status-register)
 * [ports](#ports)

You can run a program by first loading it into the 64Kb ROM located in the `control` subcircuit.
After your program is loaded, simply reset the CPU by toggling the `RESET` button, then run the clock.

## Main Circuit

The main circuit connects all of the subcircuits through the bus (B),
as well as supplying the reset (R) and clock (C) signals.

![Image of the main circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-main.jpg)

## Register Bank

The register bank contains the 8 general-purpose registers.
It handles reading and writing to specific registers.

![Image of the register bank circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-register-bank.jpg)

## Memory Bank

The memory bank handles reading and writing to adresses in memory.
In addition, it controls writing to ports for MMI/O.

![Image of the memory bank circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-memory-bank.jpg)

## Arithmetic Logic Unit

The Arithmetic Logic Unit (ALU) handles arithmetic and logic operations (woah),
such as addition, bitwise operations, etc..
It contains two internal registers, which are used for the operations.

![Image of the ALU circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-alu.jpg)

## Control

The control circuit controls the rest of the CPU.
It contains a program counter that adresses the program file stored in ROM.
It uses a series of 3 ROMs to map instructions to bits in the Control Word,
which instructs the rest of the CPU what to do each clock cycle.

![Image of the control circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-control.jpg)

## Ports

The ports circuit simply acts as a sample MMI/O chip,
behaving similarly to the GPIO pins on an ATMega or ATtiny microcontroller.

![Image of the ports circuit](https://github.com/commonkestrel/fateful/raw/master/misc/logisim-ports.jpg)

