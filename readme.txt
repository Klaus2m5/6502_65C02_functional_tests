This is a set of functional tests for the 6502/65C02 type processors.

The 6502_functional_test.a65 is an assembler sourcecode to test all valid
opcodes and addressing modes of the original NMOS 6502 cpu.

The 65C02_extended_opcodes_test.a65c tests all additional opcodes of the
65C02 processor including undefined opcodes.

The 6502_interrupt_test.a65 is a simple test to check the interrupt system
of both processors. A feedback register is required to inject IRQ and NMI
requests.

Detailed information about how to configure, assemble and run the tests is
included in each source file.

The tests have primarily been written to test my own ATMega16 6502 emulator
project. You can find it here: http://2m5.de/6502_Emu/index.htm

A discussion about the tests can be found here:
http://forum.6502.org/viewtopic.php?f=2&t=2241

Good luck debugging your emulator, simulator, fpga core, discrete
logic implementation or whatever you have!


