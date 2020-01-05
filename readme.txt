This is a set of functional tests for the 6502/65C02 type processors.

The 6502_functional_test.a65 is an assembler sourcecode to test all valid
opcodes and addressing modes of the original NMOS 6502 cpu.

The 65C02_extended_opcodes_test.a65c tests all additional opcodes of the
65C02 processor including undefined opcodes.

The 6502_interrupt_test.a65 is a simple test to check the interrupt system
of both processors. A feedback register is required to inject IRQ and NMI
requests.

The 6502_decimal_test.a65 is Bruce Clark's code to accurately test decimal mode
of the various 6502 cores (6502, 65c02 & 65816 in 8-bit mode) with added
configuration options (invalid bcd or not, which flags to ignore).

Detailed information about how to configure, assemble and run the tests is
included in each source file.

The assembler used is no longer available on the author's website. as65_142.zip
is now included in this repository.

And no, I will not switch to another assembler. However, GitHub user amb5l has
a CA65 compatible version in his repository.

Good luck debugging your emulator, simulator, fpga core, discrete
logic implementation or whatever you have!


