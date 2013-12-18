AS65 Assembler for R6502 [1.42].  Copyright 1994-2007, Frank A. Kingswood                                                Page    1
------------------------------------------------ 65C02_extended_opcodes_test.a65c ------------------------------------------------

2748 lines read, no errors in pass 1.
                        ;
                        ; 6 5 C 0 2   E X T E N D E D   O P C O D E S   T E S T
                        ;
                        ; Copyright (C) 2013  Klaus Dormann
                        ;
                        ; This program is free software: you can redistribute it and/or modify
                        ; it under the terms of the GNU General Public License as published by
                        ; the Free Software Foundation, either version 3 of the License, or
                        ; (at your option) any later version.
                        ;
                        ; This program is distributed in the hope that it will be useful,
                        ; but WITHOUT ANY WARRANTY; without even the implied warranty of
                        ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                        ; GNU General Public License for more details.
                        ;
                        ; You should have received a copy of the GNU General Public License
                        ; along with this program.  If not, see <http://www.gnu.org/licenses/>.
                        
                        
                        ; This program is designed to test all additional 65C02 opcodes, addressing
                        ; modes and functionality not available in the NMOS version of the 6502.
                        ; The 6502_functional_test is a prerequisite to this test.
                        ; NMI, IRQ, BRK, STP & WAI are covered in the 6502_interrupt_test.
                        ; 
                        ; version 16-aug-2013
                        ; edited to provide a pre-configured bin file loadable at $0000 for full 64k
                        ; contact info at http://2m5.de or email K@2m5.de
                        ;
                        ; assembled with AS65 from http://www.kingswood-consulting.co.uk/assemblers/
                        ; command line switches: -l -m -s2 -w -x -h0
                        ;                         |  |  |   |  |  no page headers in listing
                        ;                         |  |  |   |  65C02 extensions
                        ;                         |  |  |   wide listing (133 char/col)
                        ;                         |  |  write intel hex file instead of binary
                        ;                         |  expand macros in listing
                        ;                         generate pass2 listing
                        ;
                        ; No IO - should be run from a monitor with access to registers.
                        ; To run load intel hex image with a load command, than alter PC to 400 hex
                        ; (code_segment) and enter a go command.
                        ; Loop on program counter determines error or successful completion of test.
                        ; Check listing for relevant traps (jump/branch *).
                        ; Please note that in early tests some instructions will have to be used before
                        ; they are actually tested!
                        ;
                        ; RESET, NMI or IRQ should not occur and will be trapped if vectors are enabled.
                        ; Tests documented behavior of the original 65C02 only!
                        ; Decimal ops will only be tested with valid BCD operands and the V flag will
                        ; be ignored as it is absolutely useless in decimal mode.
                        ;
                        ; Debugging hints:
                        ;     Most of the code is written sequentially. if you hit a trap, check the
                        ;   immediately preceeding code for the instruction to be tested. Results are
                        ;   tested first, flags are checked second by pushing them onto the stack and
                        ;   pulling them to the accumulator after the result was checked. The "real"
                        ;   flags are no longer valid for the tested instruction at this time!
                        ;     If the tested instruction was indexed, the relevant index (X or Y) must
                        ;   also be checked. Opposed to the flags, X and Y registers are still valid.
                        ;
                        ; versions:
                        ;   19-jul-2013  1st version distributed for testing
                        ;   23-jul-2013  fixed BRA out of range due to larger trap macros
                        ;                added RAM integrity check
                        ;   16-aug-2013  added error report to standard output option
                        
                        
                        ; C O N F I G U R A T I O N
                        
                        ;ROM_vectors writable (0=no, 1=yes)
                        ;if ROM vectors can not be used interrupts will not be trapped
                        ;as a consequence BRK can not be tested but will be emulated to test RTI
0001 =                  ROM_vectors = 1
                        
                        ;load_data_direct (0=move from code segment, 1=load directly)
                        ;loading directly is preferred but may not be supported by your platform
                        ;0 produces only consecutive object code, 1 is not suitable for a binary image
0001 =                  load_data_direct = 1
                        
                        ;I_flag behavior (0=force enabled, 1=force disabled, 2=prohibit change, 3=allow
                        ;change) 2 requires extra code and is not recommended.
0003 =                  I_flag = 3
                        
                        ;configure memory - try to stay away from memory used by the system
                        ;zero_page memory start address, $4e (78) consecutive Bytes required
                        ;                                add 2 if I_flag = 2
000a =                  zero_page = $a  
                        
                        ;data_segment memory start address, $5D (93) consecutive Bytes required
                        ; + 12 Bytes at data_segment + $f9 (JMP indirect page cross test)
0200 =                  data_segment = $200
                            if (data_segment & $ff) != 0
                                ERROR ERROR ERROR low byte of data_segment MUST be $00 !!
                            endif  
                        
                        ;code_segment memory start address, 10kB of consecutive space required
                        ;                                   add 1 kB if I_flag = 2
                        ;parts of the code are self modifying and must reside in RAM
0400 =                  code_segment = $400  
                        
                        ;added WDC only opcodes WAI & STP (0=test as NOPs, >0=no test)
0001 =                  wdc_op = 1
                        
                        ;added Rockwell & WDC opcodes BBR, BBS, RMB & SMB
                        ;(0=test as NOPs, 1=full test, >1=no test) 
0001 =                  rkwl_wdc_op = 1
                        
                        ;report errors through I/O channel (0=use standard self trap loops, 1=include
                        ;report.i65 as I/O channel, add 3 kB)
0000 =                  report = 0
                        
                        ;RAM integrity test option. Checks for undesired RAM writes.
                        ;set lowest non RAM or RAM mirror address page (-1=disable, 0=64k, $40=16k)
                        ;leave disabled if a monitor, OS or background interrupt is allowed to alter RAM
ffff =                  ram_top = -1
                        
                                noopt       ;do not take shortcuts
                                
                        ;macros for error & success traps to allow user modification
                        ;example:
                        ;trap    macro
                        ;        jsr my_error_handler
                        ;        endm
                        ;trap_eq macro
                        ;        bne skip\?
                        ;        trap           ;failed equal (zero)
                        ;skip\?
                        ;        endm
                        ;
                        ; my_error_handler should pop the calling address from the stack and report it.
                        ; putting larger portions of code (more than 3 bytes) inside the trap macro
                        ; may lead to branch range problems for some tests.
                            if report = 0
                        trap    macro
                                jmp *           ;failed anyway
                                endm
                        trap_eq macro
                                beq *           ;failed equal (zero)
                                endm
                        trap_ne macro
                                bne *           ;failed not equal (non zero)
                                endm
                        trap_cs macro
                                bcs *           ;failed carry set
                                endm
                        trap_cc macro
                                bcc *           ;failed carry clear
                                endm
                        trap_mi macro
                                bmi *           ;failed minus (bit 7 set)
                                endm
                        trap_pl macro
                                bpl *           ;failed plus (bit 7 clear)
                                endm
                        trap_vs macro
                                bvs *           ;failed overflow set
                                endm
                        trap_vc macro
                                bvc *           ;failed overflow clear
                                endm
                        ; please observe that during the test the stack gets invalidated
                        ; therefore a RTS inside the success macro is not possible
                        success macro
                                jmp *           ;test passed, no errors
                                endm
                            endif
                            if report = 1
                        trap    macro
                                jsr report_error
                                endm
                        trap_eq macro
                                bne skip\?
                                trap           ;failed equal (zero)
                        skip\?
                                endm
                        trap_ne macro
                                beq skip\?
                                trap            ;failed not equal (non zero)
                        skip\?
                                endm
                        trap_cs macro
                                bcc skip\?
                                trap            ;failed carry set
                        skip\?
                                endm
                        trap_cc macro
                                bcs skip\?
                                trap            ;failed carry clear
                        skip\?
                                endm
                        trap_mi macro
                                bpl skip\?
                                trap            ;failed minus (bit 7 set)
                        skip\?
                                endm
                        trap_pl macro
                                bmi skip\?
                                trap            ;failed plus (bit 7 clear)
                        skip\?
                                endm
                        trap_vs macro
                                bvc skip\?
                                trap            ;failed overflow set
                        skip\?
                                endm
                        trap_vc macro
                                bvs skip\?
                                trap            ;failed overflow clear
                        skip\?
                                endm
                        ; please observe that during the test the stack gets invalidated
                        ; therefore a RTS inside the success macro is not possible
                        success macro
                                jsr report_success
                                endm
                            endif
                        
                        
0001 =                  carry   equ %00000001   ;flag bits in status
0002 =                  zero    equ %00000010
0004 =                  intdis  equ %00000100
0008 =                  decmode equ %00001000
0010 =                  break   equ %00010000
0020 =                  reserv  equ %00100000
0040 =                  overfl  equ %01000000
0080 =                  minus   equ %10000000
                        
0001 =                  fc      equ carry
0002 =                  fz      equ zero
0003 =                  fzc     equ carry+zero
0040 =                  fv      equ overfl
0042 =                  fvz     equ overfl+zero
0080 =                  fn      equ minus
0081 =                  fnc     equ minus+carry
0082 =                  fnz     equ minus+zero
0083 =                  fnzc    equ minus+zero+carry
00c0 =                  fnv     equ minus+overfl
                        
0030 =                  fao     equ break+reserv    ;bits always on after PHP, BRK
0034 =                  fai     equ fao+intdis      ;+ forced interrupt disable
00ff =                  m8      equ $ff             ;8 bit mask
00fb =                  m8i     equ $ff&~intdis     ;8 bit mask - interrupt disable
                        
                        ;macros to allow masking of status bits.
                        ;masking of interrupt enable/disable on load and compare
                        ;masking of always on bits after PHP or BRK (unused & break) on compare
                                if I_flag = 0
                        load_flag   macro
                                    lda #\1&m8i         ;force enable interrupts (mask I)
                                    endm
                        cmp_flag    macro
                                    cmp #(\1|fao)&m8i   ;I_flag is always enabled + always on bits
                                    endm
                        eor_flag    macro
                                    eor #(\1&m8i|fao)   ;mask I, invert expected flags + always on bits
                                    endm
                                endif
                                if I_flag = 1
                        load_flag   macro
                                    lda #\1|intdis      ;force disable interrupts
                                    endm
                        cmp_flag    macro
                                    cmp #(\1|fai)&m8    ;I_flag is always disabled + always on bits
                                    endm
                        eor_flag    macro
                                    eor #(\1|fai)       ;invert expected flags + always on bits + I
                                    endm
                                endif
                                if I_flag = 2
                        load_flag   macro
                                    lda #\1
                                    ora flag_I_on       ;restore I-flag
                                    and flag_I_off
                                    endm
                        cmp_flag    macro
                                    eor flag_I_on       ;I_flag is never changed
                                    cmp #(\1|fao)&m8i   ;expected flags + always on bits, mask I
                                    endm
                        eor_flag    macro
                                    eor flag_I_on       ;I_flag is never changed
                                    eor #(\1&m8i|fao)   ;mask I, invert expected flags + always on bits
                                    endm
                                endif
                                if I_flag = 3
                        load_flag   macro
                                    lda #\1             ;allow test to change I-flag (no mask)
                                    endm
                        cmp_flag    macro
                                    cmp #(\1|fao)&m8    ;expected flags + always on bits
                                    endm
                        eor_flag    macro
                                    eor #\1|fao         ;invert expected flags + always on bits
                                    endm
                                endif
                        
                        ;macros to set (register|memory|zeropage) & status
                        set_stat    macro       ;setting flags in the processor status register
                                    load_flag \1
                                    pha         ;use stack to load status
                                    plp
                                    endm
                        
                        set_a       macro       ;precharging accu & status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda #\1     ;precharge accu
                                    plp
                                    endm
                        
                        set_x       macro       ;precharging index & status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    ldx #\1     ;precharge index x
                                    plp
                                    endm
                        
                        set_y       macro       ;precharging index & status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    ldy #\1     ;precharge index y
                                    plp
                                    endm
                        
                        set_ax      macro       ;precharging indexed accu & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,x    ;precharge accu
                                    plp
                                    endm
                        
                        set_ay      macro       ;precharging indexed accu & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,y    ;precharge accu
                                    plp
                                    endm
                        
                        set_z       macro       ;precharging indexed zp & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,x    ;load to zeropage
                                    sta zpt
                                    plp
                                    endm
                        
                        set_zx      macro       ;precharging zp,x & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,x    ;load to indexed zeropage
                                    sta zpt,x
                                    plp
                                    endm
                        
                        set_abs     macro       ;precharging indexed memory & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,x    ;load to memory
                                    sta abst
                                    plp
                                    endm
                        
                        set_absx    macro       ;precharging abs,x & immediate status
                                    load_flag \2
                                    pha         ;use stack to load status
                                    lda \1,x    ;load to indexed memory
                                    sta abst,x
                                    plp
                                    endm
                        
                        ;macros to test (register|memory|zeropage) & status & (mask)
                        tst_stat    macro       ;testing flags in the processor status register
                                    php         ;save status
                                    pla         ;use stack to retrieve status
                                    pha
                                    cmp_flag \1
                                    trap_ne
                                    plp         ;restore status
                                    endm
                                    
                        tst_a       macro       ;testing result in accu & flags
                                    php         ;save flags
                                    cmp #\1     ;test result
                                    trap_ne
                                    pla         ;load status
                                    pha
                                    cmp_flag \2
                                    trap_ne
                                    plp         ;restore status
                                    endm
                        
                        tst_as      macro       ;testing result in accu & flags, save accu
                                    pha
                                    php         ;save flags
                                    cmp #\1     ;test result
                                    trap_ne
                                    pla         ;load status
                                    pha
                                    cmp_flag \2
                                    trap_ne
                                    plp         ;restore status
                                    pla
                                    endm
                        
                        tst_x       macro       ;testing result in x index & flags
                                    php         ;save flags
                                    cpx #\1     ;test result
                                    trap_ne
                                    pla         ;load status
                                    pha
                                    cmp_flag \2
                                    trap_ne
                                    plp         ;restore status
                                    endm
                        
                        tst_y       macro       ;testing result in y index & flags
                                    php         ;save flags
                                    cpy #\1     ;test result
                                    trap_ne
                                    pla         ;load status
                                    pha
                                    cmp_flag \2
                                    trap_ne
                                    plp         ;restore status
                                    endm
                        
                        tst_ax      macro       ;indexed testing result in accu & flags
                                    php         ;save flags
                                    cmp \1,x    ;test result
                                    trap_ne
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,x    ;test flags
                                    trap_ne     ;
                                    endm
                        
                        tst_ay      macro       ;indexed testing result in accu & flags
                                    php         ;save flags
                                    cmp \1,y    ;test result
                                    trap_ne     ;
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,y    ;test flags
                                    trap_ne
                                    endm
                                
                        tst_z       macro       ;indexed testing result in zp & flags
                                    php         ;save flags
                                    lda zpt
                                    cmp \1,x    ;test result
                                    trap_ne
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,x    ;test flags
                                    trap_ne
                                    endm
                        
                        tst_zx      macro       ;testing result in zp,x & flags
                                    php         ;save flags
                                    lda zpt,x
                                    cmp \1,x    ;test result
                                    trap_ne
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,x    ;test flags
                                    trap_ne
                                    endm
                        
                        tst_abs     macro       ;indexed testing result in memory & flags
                                    php         ;save flags
                                    lda abst
                                    cmp \1,x    ;test result
                                    trap_ne
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,x    ;test flags
                                    trap_ne
                                    endm
                        
                        tst_absx    macro       ;testing result in abs,x & flags
                                    php         ;save flags
                                    lda abst,x
                                    cmp \1,x    ;test result
                                    trap_ne
                                    pla         ;load status
                                    eor_flag \3
                                    cmp \2,x    ;test flags
                                    trap_ne
                                    endm
                                    
                        ; RAM integrity test
                        ;   verifies that none of the previous tests has altered RAM outside of the
                        ;   designated write areas.
                        ;   uses zpt word as indirect pointer, zpt+2 word as checksum
                                if ram_top > -1
                        check_ram   macro 
                                    cld
                                    lda #0
                                    sta zpt         ;set low byte of indirect pointer
                                    sta zpt+3       ;checksum high byte
                                    ldx #11         ;reset modifiable RAM
                        ccs1\?      sta jxi_tab,x   ;JMP indirect page cross area
                                    dex
                                    bpl ccs1\?
                                    sta chkdadi     ;self modifying code
                                    sta chkdsbi
                                    clc
                                    ldx #zp_bss-zero_page ;zeropage - write test area
                        ccs3\?      adc zero_page,x
                                    bcc ccs2\?
                                    inc zpt+3       ;carry to high byte
                                    clc
                        ccs2\?      inx
                                    bne ccs3\?
                                    ldx #hi(data_segment) ;set high byte of indirect pointer
                                    stx zpt+1
                                    ldy #lo(data_bss) ;data after write test area
                        ccs5\?      adc (zpt),y
                                    bcc ccs4\?
                                    inc zpt+3       ;carry to high byte
                                    clc
                        ccs4\?      iny
                                    bne ccs5\?
                                    inx             ;advance RAM high address
                                    stx zpt+1
                                    cpx #ram_top
                                    bne ccs5\?
                                    sta zpt+2       ;checksum low is
                                    cmp ram_chksm   ;checksum low expected
                                    trap_ne         ;checksum mismatch
                                    lda zpt+3       ;checksum high is
                                    cmp ram_chksm+1 ;checksum high expected
                                    trap_ne         ;checksum mismatch
                                    endm            
                                else
                        check_ram   macro
                                    ;RAM check disabled - RAM size not set
                                    endm
                                endif
                                    
                        next_test   macro           ;make sure, tests don't jump the fence
                                    lda test_case   ;previous test
                                    cmp #test_num
                                    trap_ne         ;test is out of sequence
                        test_num = test_num + 1
                                    lda #test_num   ;*** next tests' number
                                    sta test_case
                                    ;check_ram       ;uncomment to find altered RAM after each test
                                    endm
                        
                            if load_data_direct = 1
                                data
                            else
                                bss                 ;uninitialized segment, copy of data at end of code!
                            endif
                        ;        org zero_page
0000 =                          org 0               ;edited to provide binaries loading from 0
0000 : 00000000000000..         ds  zero_page
                            if I_flag = 2
                        ;masking for I bit in status
                        flag_I_on   ds  1           ;or mask to load flags   
                        flag_I_off  ds  1           ;and mask to load flags
                            endif
000a :                  zpt                         ;5 bytes store/modify test area
                        ;add/subtract operand generation and result/flag prediction
000a : 00               adfc    ds  1               ;carry flag before op
000b : 00               ad1     ds  1               ;operand 1 - accumulator
000c : 00               ad2     ds  1               ;operand 2 - memory / immediate
000d : 00               adrl    ds  1               ;expected result bits 0-7
000e : 00               adrh    ds  1               ;expected result bit 8 (carry)
000f : 00               adrf    ds  1               ;expected flags NV0000ZC (-V in decimal mode)
0010 : 00               sb2     ds  1               ;operand 2 complemented for subtract
0011 :                  zp_bss
0011 : c3824100         zp1     db  $c3,$82,$41,0   ;test patterns for LDx BIT ROL ROR ASL LSR
0015 : 7f               zp7f    db  $7f             ;test pattern for compare  
                        ;logical zeropage operands
0016 : 001f7180         zpOR    db  0,$1f,$71,$80   ;test pattern for OR
001a : 0fff7f80         zpAN    db  $0f,$ff,$7f,$80 ;test pattern for AND
001e : ff0f8f8f         zpEO    db  $ff,$0f,$8f,$8f ;test pattern for EOR
                        ;indirect addressing pointers
0022 : 0a02             ind1    dw  abs1            ;indirect pointer to pattern in absolute memory
0024 : 0b02                     dw  abs1+1
0026 : 0c02                     dw  abs1+2
0028 : 0d02                     dw  abs1+3
002a : 0e02                     dw  abs7f
002c : 1201             inw1    dw  abs1-$f8        ;indirect pointer for wrap-test pattern
002e : 0502             indt    dw  abst            ;indirect pointer to store area in absolute memory
0030 : 0602                     dw  abst+1
0032 : 0702                     dw  abst+2
0034 : 0802                     dw  abst+3
0036 : 0d01             inwt    dw  abst-$f8        ;indirect pointer for wrap-test store
0038 : 4102             indAN   dw  absAN           ;indirect pointer to AND pattern in absolute memory
003a : 4202                     dw  absAN+1
003c : 4302                     dw  absAN+2
003e : 4402                     dw  absAN+3
0040 : 4502             indEO   dw  absEO           ;indirect pointer to EOR pattern in absolute memory
0042 : 4602                     dw  absEO+1
0044 : 4702                     dw  absEO+2
0046 : 4802                     dw  absEO+3
0048 : 3d02             indOR   dw  absOR           ;indirect pointer to OR pattern in absolute memory
004a : 3e02                     dw  absOR+1
004c : 3f02                     dw  absOR+2
004e : 4002                     dw  absOR+3
                        ;add/subtract indirect pointers
0050 : 0502             adi2    dw  ada2            ;indirect pointer to operand 2 in absolute memory
0052 : 0602             sbi2    dw  sba2            ;indirect pointer to complemented operand 2 (SBC)
0054 : 0601             adiy2   dw  ada2-$ff        ;with offset for indirect indexed
0056 : 0701             sbiy2   dw  sba2-$ff
0058 :                  zp_bss_end
                            
0200 =                          org data_segment
0200 : 0000             pg_x    ds  2               ;high JMP indirect address for page cross bug
0202 : 00               test_case   ds  1           ;current test number
0203 : 0000             ram_chksm   ds  2           ;checksum for RAM integrity test
                        ;add/subtract operand copy - abs tests write area
0205 :                  abst                        ;5 bytes store/modify test area
0205 : 00               ada2    ds  1               ;operand 2
0206 : 00               sba2    ds  1               ;operand 2 complemented for subtract
0207 : 000000                   ds  3               ;fill remaining bytes
020a :                  data_bss
020a : c3824100         abs1    db  $c3,$82,$41,0   ;test patterns for LDx BIT ROL ROR ASL LSR
020e : 7f               abs7f   db  $7f             ;test pattern for compare
                        ;loads
020f : 80800002         fLDx    db  fn,fn,0,fz      ;expected flags for load
                        ;shifts
0213 :                  rASL                        ;expected result ASL & ROL -carry  
0213 : 86048200         rROL    db  $86,$04,$82,0   ; "
0217 : 87058301         rROLc   db  $87,$05,$83,1   ;expected result ROL +carry
021b :                  rLSR                        ;expected result LSR & ROR -carry
021b : 61412000         rROR    db  $61,$41,$20,0   ; "
021f : e1c1a080         rRORc   db  $e1,$c1,$a0,$80 ;expected result ROR +carry
0223 :                  fASL                        ;expected flags for shifts
0223 : 81018002         fROL    db  fnc,fc,fn,fz    ;no carry in
0227 : 81018000         fROLc   db  fnc,fc,fn,0     ;carry in
022b :                  fLSR
022b : 01000102         fROR    db  fc,0,fc,fz      ;no carry in
022f : 81808180         fRORc   db  fnc,fn,fnc,fn   ;carry in
                        ;increments (decrements)
0233 : 7f80ff0001       rINC    db  $7f,$80,$ff,0,1 ;expected result for INC/DEC
0238 : 0080800200       fINC    db  0,fn,fn,fz,0    ;expected flags for INC/DEC
                        ;logical memory operand
023d : 001f7180         absOR   db  0,$1f,$71,$80   ;test pattern for OR
0241 : 0fff7f80         absAN   db  $0f,$ff,$7f,$80 ;test pattern for AND
0245 : ff0f8f8f         absEO   db  $ff,$0f,$8f,$8f ;test pattern for EOR
                        ;logical accu operand
0249 : 00f11f00         absORa  db  0,$f1,$1f,0     ;test pattern for OR
024d : f0ffffff         absANa  db  $f0,$ff,$ff,$ff ;test pattern for AND
0251 : fff0f00f         absEOa  db  $ff,$f0,$f0,$0f ;test pattern for EOR
                        ;logical results
0255 : 00ff7f80         absrlo  db  0,$ff,$7f,$80
0259 : 02800080         absflo  db  fz,fn,0,fn
025d :                  data_bss_end
                        ;define area for page crossing JMP (abs) & JMP (abs,x) test
02f9 =                  jxi_tab equ data_segment + $100 - 7     ;JMP (jxi_tab,x) x=6
02fd =                  ji_tab  equ data_segment + $100 - 3     ;JMP (ji_tab+2)
0300 =                  jxp_tab equ data_segment + $100         ;JMP (jxp_tab-255) x=255
                        
                        
                                code
0400 =                          org code_segment
0400 : d8               start   cld
0401 : a2ff                     ldx #$ff
0403 : 9a                       txs
0404 : a900                     lda #0          ;*** test 0 = initialize
0406 : 8d0202                   sta test_case
0000 =                  test_num = 0
                        
                        ;stop interrupts before initializing BSS
                            if I_flag = 1
                                sei
                            endif
                            
                        ;initialize I/O for report channel
                            if report = 1
                                jsr report_init
                            endif
                            
                        ;initialize BSS segment
                            if load_data_direct != 1
                                ldx #zp_end-zp_init-1
                        ld_zp   lda zp_init,x
                                sta zp_bss,x
                                dex
                                bpl ld_zp
                                ldx #data_end-data_init-1
                        ld_data lda data_init,x
                                sta data_bss,x
                                dex
                                bpl ld_data
                              if ROM_vectors = 1
                                ldx #5
                        ld_vect lda vec_init,x
                                sta vec_bss,x
                                dex
                                bpl ld_vect
                              endif
                            endif
                        
                        ;retain status of interrupt flag
                            if I_flag = 2
                                php
                                pla
                                and #4          ;isolate flag
                                sta flag_I_on   ;or mask
                                eor #lo(~4)     ;reverse
                                sta flag_I_off  ;and mask
                            endif
                                
                        ;generate checksum for RAM integrity test
                            if ram_top > -1
                                lda #0 
                                sta zpt         ;set low byte of indirect pointer
                                sta ram_chksm+1 ;checksum high byte
                                ldx #11         ;reset modifiable RAM
                        gcs1    sta jxi_tab,x   ;JMP indirect page cross area
                                dex
                                bpl gcs1
                                sta chkdadi     ;self modifying code
                                sta chkdsbi
                                clc
                                ldx #zp_bss-zero_page ;zeropage - write test area
                        gcs3    adc zero_page,x
                                bcc gcs2
                                inc ram_chksm+1 ;carry to high byte
                                clc
                        gcs2    inx
                                bne gcs3
                                ldx #hi(data_segment) ;set high byte of indirect pointer
                                stx zpt+1
                                ldy #lo(data_bss) ;data after write test area
                        gcs5    adc (zpt),y
                                bcc gcs4
                                inc ram_chksm+1 ;carry to high byte
                                clc
                        gcs4    iny
                                bne gcs5
                                inx             ;advance RAM high address
                                stx zpt+1
                                cpx #ram_top
                                bne gcs5
                                sta ram_chksm   ;checksum complete
                            endif
                                next_test            
0409 : ad0202          >            lda test_case   ;previous test
040c : c900            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
040e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0001 =                 >test_num = test_num + 1
0410 : a901            >            lda #test_num   ;*** next tests' number
0412 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ;testing stack operations PHX PHY PLX PLY
0415 : a999                     lda #$99        ;protect a
0417 : a2ff                     ldx #$ff        ;initialize stack
0419 : 9a                       txs
041a : a255                     ldx #$55
041c : da                       phx
041d : a2aa                     ldx #$aa
041f : da                       phx
0420 : ecfe01                   cpx $1fe        ;on stack ?
                                trap_ne
0423 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0425 : ba                       tsx
0426 : e0fd                     cpx #$fd        ;sp decremented?
                                trap_ne
0428 : d0fe            >        bne *           ;failed not equal (non zero)
                        
042a : 7a                       ply
042b : c0aa                     cpy #$aa        ;successful retreived from stack?
                                trap_ne
042d : d0fe            >        bne *           ;failed not equal (non zero)
                        
042f : 7a                       ply
0430 : c055                     cpy #$55
                                trap_ne
0432 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0434 : ccff01                   cpy $1ff        ;remains on stack?
                                trap_ne
0437 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0439 : ba                       tsx
043a : e0ff                     cpx #$ff        ;sp incremented?
                                trap_ne
043c : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                
043e : a0a5                     ldy #$a5
0440 : 5a                       phy
0441 : a05a                     ldy #$5a
0443 : 5a                       phy
0444 : ccfe01                   cpy $1fe        ;on stack ?
                                trap_ne
0447 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0449 : ba                       tsx
044a : e0fd                     cpx #$fd        ;sp decremented?
                                trap_ne
044c : d0fe            >        bne *           ;failed not equal (non zero)
                        
044e : fa                       plx
044f : e05a                     cpx #$5a        ;successful retreived from stack?
                                trap_ne
0451 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0453 : fa                       plx
0454 : e0a5                     cpx #$a5
                                trap_ne
0456 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0458 : ecff01                   cpx $1ff        ;remains on stack?
                                trap_ne
045b : d0fe            >        bne *           ;failed not equal (non zero)
                        
045d : ba                       tsx
045e : e0ff                     cpx #$ff        ;sp incremented?
                                trap_ne
0460 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0462 : c999                     cmp #$99        ;unchanged?
                                trap_ne
0464 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test            
0466 : ad0202          >            lda test_case   ;previous test
0469 : c901            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
046b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0002 =                 >test_num = test_num + 1
046d : a902            >            lda #test_num   ;*** next tests' number
046f : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                                
                        ; test PHX does not alter flags or X but PLX does
0472 : a0aa                     ldy #$aa        ;protect y
                                set_x 1,$ff     ;push
                       >            load_flag $ff     
0474 : a9ff            >            lda #$ff                  ;allow test to change I-flag (no mask)
                       >
0476 : 48              >            pha         ;use stack to load status
0477 : a201            >            ldx #1     ;precharge index x
0479 : 28              >            plp
                        
047a : da                       phx
                                tst_x 1,$ff
047b : 08              >            php         ;save flags
047c : e001            >            cpx #1     ;test result
                       >            trap_ne
047e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0480 : 68              >            pla         ;load status
0481 : 48              >            pha
                       >            cmp_flag $ff
0482 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0484 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0486 : 28              >            plp         ;restore status
                        
                                set_x 0,0
                       >            load_flag 0
0487 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0489 : 48              >            pha         ;use stack to load status
048a : a200            >            ldx #0     ;precharge index x
048c : 28              >            plp
                        
048d : da                       phx
                                tst_x 0,0
048e : 08              >            php         ;save flags
048f : e000            >            cpx #0     ;test result
                       >            trap_ne
0491 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0493 : 68              >            pla         ;load status
0494 : 48              >            pha
                       >            cmp_flag 0
0495 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0497 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0499 : 28              >            plp         ;restore status
                        
                                set_x $ff,$ff
                       >            load_flag $ff
049a : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
049c : 48              >            pha         ;use stack to load status
049d : a2ff            >            ldx #$ff     ;precharge index x
049f : 28              >            plp
                        
04a0 : da                       phx
                                tst_x $ff,$ff
04a1 : 08              >            php         ;save flags
04a2 : e0ff            >            cpx #$ff     ;test result
                       >            trap_ne
04a4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04a6 : 68              >            pla         ;load status
04a7 : 48              >            pha
                       >            cmp_flag $ff
04a8 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
04aa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04ac : 28              >            plp         ;restore status
                        
                                set_x 1,0
                       >            load_flag 0
04ad : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
04af : 48              >            pha         ;use stack to load status
04b0 : a201            >            ldx #1     ;precharge index x
04b2 : 28              >            plp
                        
04b3 : da                       phx
                                tst_x 1,0
04b4 : 08              >            php         ;save flags
04b5 : e001            >            cpx #1     ;test result
                       >            trap_ne
04b7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04b9 : 68              >            pla         ;load status
04ba : 48              >            pha
                       >            cmp_flag 0
04bb : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
04bd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04bf : 28              >            plp         ;restore status
                        
                                set_x 0,$ff
                       >            load_flag $ff
04c0 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
04c2 : 48              >            pha         ;use stack to load status
04c3 : a200            >            ldx #0     ;precharge index x
04c5 : 28              >            plp
                        
04c6 : da                       phx
                                tst_x 0,$ff
04c7 : 08              >            php         ;save flags
04c8 : e000            >            cpx #0     ;test result
                       >            trap_ne
04ca : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04cc : 68              >            pla         ;load status
04cd : 48              >            pha
                       >            cmp_flag $ff
04ce : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
04d0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04d2 : 28              >            plp         ;restore status
                        
                                set_x $ff,0
                       >            load_flag 0
04d3 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
04d5 : 48              >            pha         ;use stack to load status
04d6 : a2ff            >            ldx #$ff     ;precharge index x
04d8 : 28              >            plp
                        
04d9 : da                       phx
                                tst_x $ff,0
04da : 08              >            php         ;save flags
04db : e0ff            >            cpx #$ff     ;test result
                       >            trap_ne
04dd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04df : 68              >            pla         ;load status
04e0 : 48              >            pha
                       >            cmp_flag 0
04e1 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
04e3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04e5 : 28              >            plp         ;restore status
                        
                                set_x 0,$ff     ;pull
                       >            load_flag $ff     
04e6 : a9ff            >            lda #$ff                  ;allow test to change I-flag (no mask)
                       >
04e8 : 48              >            pha         ;use stack to load status
04e9 : a200            >            ldx #0     ;precharge index x
04eb : 28              >            plp
                        
04ec : fa                       plx
                                tst_x $ff,$ff-zero
04ed : 08              >            php         ;save flags
04ee : e0ff            >            cpx #$ff     ;test result
                       >            trap_ne
04f0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04f2 : 68              >            pla         ;load status
04f3 : 48              >            pha
                       >            cmp_flag $ff-zero
04f4 : c9fd            >            cmp #($ff-zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
04f6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
04f8 : 28              >            plp         ;restore status
                        
                                set_x $ff,0
                       >            load_flag 0
04f9 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
04fb : 48              >            pha         ;use stack to load status
04fc : a2ff            >            ldx #$ff     ;precharge index x
04fe : 28              >            plp
                        
04ff : fa                       plx
                                tst_x 0,zero
0500 : 08              >            php         ;save flags
0501 : e000            >            cpx #0     ;test result
                       >            trap_ne
0503 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0505 : 68              >            pla         ;load status
0506 : 48              >            pha
                       >            cmp_flag zero
0507 : c932            >            cmp #(zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0509 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
050b : 28              >            plp         ;restore status
                        
                                set_x $fe,$ff
                       >            load_flag $ff
050c : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
050e : 48              >            pha         ;use stack to load status
050f : a2fe            >            ldx #$fe     ;precharge index x
0511 : 28              >            plp
                        
0512 : fa                       plx
                                tst_x 1,$ff-zero-minus
0513 : 08              >            php         ;save flags
0514 : e001            >            cpx #1     ;test result
                       >            trap_ne
0516 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0518 : 68              >            pla         ;load status
0519 : 48              >            pha
                       >            cmp_flag $ff-zero-minus
051a : c97d            >            cmp #($ff-zero-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
051c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
051e : 28              >            plp         ;restore status
                        
                                set_x 0,0
                       >            load_flag 0
051f : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0521 : 48              >            pha         ;use stack to load status
0522 : a200            >            ldx #0     ;precharge index x
0524 : 28              >            plp
                        
0525 : fa                       plx
                                tst_x $ff,minus
0526 : 08              >            php         ;save flags
0527 : e0ff            >            cpx #$ff     ;test result
                       >            trap_ne
0529 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
052b : 68              >            pla         ;load status
052c : 48              >            pha
                       >            cmp_flag minus
052d : c9b0            >            cmp #(minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
052f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0531 : 28              >            plp         ;restore status
                        
                                set_x $ff,$ff
                       >            load_flag $ff
0532 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0534 : 48              >            pha         ;use stack to load status
0535 : a2ff            >            ldx #$ff     ;precharge index x
0537 : 28              >            plp
                        
0538 : fa                       plx
                                tst_x 0,$ff-minus
0539 : 08              >            php         ;save flags
053a : e000            >            cpx #0     ;test result
                       >            trap_ne
053c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
053e : 68              >            pla         ;load status
053f : 48              >            pha
                       >            cmp_flag $ff-minus
0540 : c97f            >            cmp #($ff-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0542 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0544 : 28              >            plp         ;restore status
                        
                                set_x $fe,0
                       >            load_flag 0
0545 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0547 : 48              >            pha         ;use stack to load status
0548 : a2fe            >            ldx #$fe     ;precharge index x
054a : 28              >            plp
                        
054b : fa                       plx
                                tst_x 1,0
054c : 08              >            php         ;save flags
054d : e001            >            cpx #1     ;test result
                       >            trap_ne
054f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0551 : 68              >            pla         ;load status
0552 : 48              >            pha
                       >            cmp_flag 0
0553 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0555 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0557 : 28              >            plp         ;restore status
                        
0558 : c0aa                     cpy #$aa        ;Y unchanged
                                trap_ne
055a : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test            
055c : ad0202          >            lda test_case   ;previous test
055f : c902            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
0561 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0003 =                 >test_num = test_num + 1
0563 : a903            >            lda #test_num   ;*** next tests' number
0565 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                         
                        ; test PHY does not alter flags or Y but PLY does
0568 : a255                     ldx #$55        ;x & a protected
                                set_y 1,$ff     ;push
                       >            load_flag $ff     
056a : a9ff            >            lda #$ff                  ;allow test to change I-flag (no mask)
                       >
056c : 48              >            pha         ;use stack to load status
056d : a001            >            ldy #1     ;precharge index y
056f : 28              >            plp
                        
0570 : 5a                       phy
                                tst_y 1,$ff
0571 : 08              >            php         ;save flags
0572 : c001            >            cpy #1     ;test result
                       >            trap_ne
0574 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0576 : 68              >            pla         ;load status
0577 : 48              >            pha
                       >            cmp_flag $ff
0578 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
057a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
057c : 28              >            plp         ;restore status
                        
                                set_y 0,0
                       >            load_flag 0
057d : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
057f : 48              >            pha         ;use stack to load status
0580 : a000            >            ldy #0     ;precharge index y
0582 : 28              >            plp
                        
0583 : 5a                       phy
                                tst_y 0,0
0584 : 08              >            php         ;save flags
0585 : c000            >            cpy #0     ;test result
                       >            trap_ne
0587 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0589 : 68              >            pla         ;load status
058a : 48              >            pha
                       >            cmp_flag 0
058b : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
058d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
058f : 28              >            plp         ;restore status
                        
                                set_y $ff,$ff
                       >            load_flag $ff
0590 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0592 : 48              >            pha         ;use stack to load status
0593 : a0ff            >            ldy #$ff     ;precharge index y
0595 : 28              >            plp
                        
0596 : 5a                       phy
                                tst_y $ff,$ff
0597 : 08              >            php         ;save flags
0598 : c0ff            >            cpy #$ff     ;test result
                       >            trap_ne
059a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
059c : 68              >            pla         ;load status
059d : 48              >            pha
                       >            cmp_flag $ff
059e : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05a0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05a2 : 28              >            plp         ;restore status
                        
                                set_y 1,0
                       >            load_flag 0
05a3 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
05a5 : 48              >            pha         ;use stack to load status
05a6 : a001            >            ldy #1     ;precharge index y
05a8 : 28              >            plp
                        
05a9 : 5a                       phy
                                tst_y 1,0
05aa : 08              >            php         ;save flags
05ab : c001            >            cpy #1     ;test result
                       >            trap_ne
05ad : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05af : 68              >            pla         ;load status
05b0 : 48              >            pha
                       >            cmp_flag 0
05b1 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05b3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05b5 : 28              >            plp         ;restore status
                        
                                set_y 0,$ff
                       >            load_flag $ff
05b6 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
05b8 : 48              >            pha         ;use stack to load status
05b9 : a000            >            ldy #0     ;precharge index y
05bb : 28              >            plp
                        
05bc : 5a                       phy
                                tst_y 0,$ff
05bd : 08              >            php         ;save flags
05be : c000            >            cpy #0     ;test result
                       >            trap_ne
05c0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05c2 : 68              >            pla         ;load status
05c3 : 48              >            pha
                       >            cmp_flag $ff
05c4 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05c6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05c8 : 28              >            plp         ;restore status
                        
                                set_y $ff,0
                       >            load_flag 0
05c9 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
05cb : 48              >            pha         ;use stack to load status
05cc : a0ff            >            ldy #$ff     ;precharge index y
05ce : 28              >            plp
                        
05cf : 5a                       phy
                                tst_y $ff,0
05d0 : 08              >            php         ;save flags
05d1 : c0ff            >            cpy #$ff     ;test result
                       >            trap_ne
05d3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05d5 : 68              >            pla         ;load status
05d6 : 48              >            pha
                       >            cmp_flag 0
05d7 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05d9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05db : 28              >            plp         ;restore status
                        
                                set_y 0,$ff     ;pull
                       >            load_flag $ff     
05dc : a9ff            >            lda #$ff                  ;allow test to change I-flag (no mask)
                       >
05de : 48              >            pha         ;use stack to load status
05df : a000            >            ldy #0     ;precharge index y
05e1 : 28              >            plp
                        
05e2 : 7a                       ply
                                tst_y $ff,$ff-zero
05e3 : 08              >            php         ;save flags
05e4 : c0ff            >            cpy #$ff     ;test result
                       >            trap_ne
05e6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05e8 : 68              >            pla         ;load status
05e9 : 48              >            pha
                       >            cmp_flag $ff-zero
05ea : c9fd            >            cmp #($ff-zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05ec : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05ee : 28              >            plp         ;restore status
                        
                                set_y $ff,0
                       >            load_flag 0
05ef : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
05f1 : 48              >            pha         ;use stack to load status
05f2 : a0ff            >            ldy #$ff     ;precharge index y
05f4 : 28              >            plp
                        
05f5 : 7a                       ply
                                tst_y 0,zero
05f6 : 08              >            php         ;save flags
05f7 : c000            >            cpy #0     ;test result
                       >            trap_ne
05f9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
05fb : 68              >            pla         ;load status
05fc : 48              >            pha
                       >            cmp_flag zero
05fd : c932            >            cmp #(zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
05ff : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0601 : 28              >            plp         ;restore status
                        
                                set_y $fe,$ff
                       >            load_flag $ff
0602 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0604 : 48              >            pha         ;use stack to load status
0605 : a0fe            >            ldy #$fe     ;precharge index y
0607 : 28              >            plp
                        
0608 : 7a                       ply
                                tst_y 1,$ff-zero-minus
0609 : 08              >            php         ;save flags
060a : c001            >            cpy #1     ;test result
                       >            trap_ne
060c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
060e : 68              >            pla         ;load status
060f : 48              >            pha
                       >            cmp_flag $ff-zero-minus
0610 : c97d            >            cmp #($ff-zero-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0612 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0614 : 28              >            plp         ;restore status
                        
                                set_y 0,0
                       >            load_flag 0
0615 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0617 : 48              >            pha         ;use stack to load status
0618 : a000            >            ldy #0     ;precharge index y
061a : 28              >            plp
                        
061b : 7a                       ply
                                tst_y $ff,minus
061c : 08              >            php         ;save flags
061d : c0ff            >            cpy #$ff     ;test result
                       >            trap_ne
061f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0621 : 68              >            pla         ;load status
0622 : 48              >            pha
                       >            cmp_flag minus
0623 : c9b0            >            cmp #(minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0625 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0627 : 28              >            plp         ;restore status
                        
                                set_y $ff,$ff
                       >            load_flag $ff
0628 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
062a : 48              >            pha         ;use stack to load status
062b : a0ff            >            ldy #$ff     ;precharge index y
062d : 28              >            plp
                        
062e : 7a                       ply
                                tst_y 0,$ff-minus
062f : 08              >            php         ;save flags
0630 : c000            >            cpy #0     ;test result
                       >            trap_ne
0632 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0634 : 68              >            pla         ;load status
0635 : 48              >            pha
                       >            cmp_flag $ff-minus
0636 : c97f            >            cmp #($ff-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0638 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
063a : 28              >            plp         ;restore status
                        
                                set_y $fe,0
                       >            load_flag 0
063b : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
063d : 48              >            pha         ;use stack to load status
063e : a0fe            >            ldy #$fe     ;precharge index y
0640 : 28              >            plp
                        
0641 : 7a                       ply
                                tst_y 1,0
0642 : 08              >            php         ;save flags
0643 : c001            >            cpy #1     ;test result
                       >            trap_ne
0645 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0647 : 68              >            pla         ;load status
0648 : 48              >            pha
                       >            cmp_flag 0
0649 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
064b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
064d : 28              >            plp         ;restore status
                        
064e : e055                     cpx #$55        ;x unchanged?
                                trap_ne
0650 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test            
0652 : ad0202          >            lda test_case   ;previous test
0655 : c903            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
0657 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0004 =                 >test_num = test_num + 1
0659 : a904            >            lda #test_num   ;*** next tests' number
065b : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                         
                        ; PC modifying instructions (BRA, BBR, BBS, 1, 2, 3 byte NOPs, JMP(abs,x))
                        ; testing unconditional branch BRA
                        
065e : a281                     ldx #$81        ;protect unused registers
0660 : a07e                     ldy #$7e
                                set_a 0,$ff
                       >            load_flag $ff
0662 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0664 : 48              >            pha         ;use stack to load status
0665 : a900            >            lda #0     ;precharge accu
0667 : 28              >            plp
                        
0668 : 8003                     bra br1         ;branch should always be taken
                                trap 
066a : 4c6a06          >        jmp *           ;failed anyway
                        
066d :                  br1
                                tst_a 0,$ff
066d : 08              >            php         ;save flags
066e : c900            >            cmp #0     ;test result
                       >            trap_ne
0670 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0672 : 68              >            pla         ;load status
0673 : 48              >            pha
                       >            cmp_flag $ff
0674 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0676 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0678 : 28              >            plp         ;restore status
                        
                                set_a $ff,0
                       >            load_flag 0
0679 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
067b : 48              >            pha         ;use stack to load status
067c : a9ff            >            lda #$ff     ;precharge accu
067e : 28              >            plp
                        
067f : 8003                     bra br2         ;branch should always be taken
                                trap 
0681 : 4c8106          >        jmp *           ;failed anyway
                        
0684 :                  br2
                                tst_a $ff,0
0684 : 08              >            php         ;save flags
0685 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
0687 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0689 : 68              >            pla         ;load status
068a : 48              >            pha
                       >            cmp_flag 0
068b : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
068d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
068f : 28              >            plp         ;restore status
                        
0690 : e081                     cpx #$81
                                trap_ne
0692 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0694 : c07e                     cpy #$7e
                                trap_ne
0696 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test            
0698 : ad0202          >            lda test_case   ;previous test
069b : c904            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
069d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0005 =                 >test_num = test_num + 1
069f : a905            >            lda #test_num   ;*** next tests' number
06a1 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                                
06a4 : a000                     ldy #0          ;branch range test  
06a6 : 8061                     bra bra0
                                
06a8 : c001             bra1    cpy #1
                                trap_ne         ;long range backward
06aa : d0fe            >        bne *           ;failed not equal (non zero)
                        
06ac : c8                       iny        
06ad : 8053                     bra bra2
                                        
06af : c003             bra3    cpy #3
                                trap_ne         ;long range backward
06b1 : d0fe            >        bne *           ;failed not equal (non zero)
                        
06b3 : c8                       iny        
06b4 : 8045                     bra bra4
                                        
06b6 : c005             bra5    cpy #5
                                trap_ne         ;long range backward
06b8 : d0fe            >        bne *           ;failed not equal (non zero)
                        
06ba : c8                       iny        
06bb : a000                     ldy #0
06bd : 8004                     bra brf0
                                
06bf : c8                       iny
06c0 : c8                       iny
06c1 : c8                       iny
06c2 : c8                       iny        
06c3 : 8003             brf0    bra brf1
                        
06c5 : c8                       iny
06c6 : c8                       iny
06c7 : c8                       iny
06c8 : c8               brf1    iny        
06c9 : 8002                     bra brf2
                                
06cb : c8                       iny
06cc : c8                       iny
06cd : c8               brf2    iny
06ce : c8                       iny        
06cf : 8001                     bra brf3
                                
06d1 : c8                       iny
06d2 : c8               brf3    iny
06d3 : c8                       iny
06d4 : c8                       iny        
06d5 : 8000                     bra brf4
                                
06d7 : c8               brf4    iny
06d8 : c8                       iny
06d9 : c8                       iny
06da : c8                       iny
06db : c00a                     cpy #10
                                trap_ne     ;short range forward
06dd : d0fe            >        bne *           ;failed not equal (non zero)
                        
06df : 8012                     bra brb0
                        
06e1 : 88               brb4    dey
06e2 : 88                       dey
06e3 : 88                       dey
06e4 : 88                       dey
06e5 : 800e                     bra brb5        
                        
06e7 : 88               brb3    dey
06e8 : 88                       dey
06e9 : 88                       dey
06ea : 80f5                     bra brb4        
                        
06ec : 88               brb2    dey
06ed : 88                       dey
06ee : 80f7                     bra brb3        
                        
06f0 : 88               brb1    dey
06f1 : 80f9                     bra brb2        
                        
06f3 : 80fb             brb0    bra brb1        
                        
06f5 : c000             brb5    cpy #0
                                trap_ne     ;short range backward
06f7 : d0fe            >        bne *           ;failed not equal (non zero)
                        
06f9 : 8015                     bra bra6
                        
06fb : c004             bra4    cpy #4
                                trap_ne     ;long range forward
06fd : d0fe            >        bne *           ;failed not equal (non zero)
                        
06ff : c8                       iny        
0700 : 80b4                     bra bra5
                                        
0702 : c002             bra2    cpy #2
                                trap_ne     ;long range forward
0704 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0706 : c8                       iny        
0707 : 80a6                     bra bra3
                                        
0709 : c000             bra0    cpy #0
                                trap_ne     ;long range forward
070b : d0fe            >        bne *           ;failed not equal (non zero)
                        
070d : c8                       iny        
070e : 8098                     bra bra1
                                        
0710 :                  bra6
                                next_test
0710 : ad0202          >            lda test_case   ;previous test
0713 : c905            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
0715 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0006 =                 >test_num = test_num + 1
0717 : a906            >            lda #test_num   ;*** next tests' number
0719 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                                
                            if rkwl_wdc_op = 1
                        ; testing BBR & BBS
                        
                        bbt     macro           ;\1 = bitnum
                                lda #(1<<\1)    ;testing 1 bit on
                                sta zpt
                                set_a $33,0     ;with flags off
                                bbr \1,zpt,fail1\?
                                bbs \1,zpt,ok1\?
                                trap            ;bbs branch not taken
                        fail1\?
                                trap            ;bbr branch taken
                        ok1\?   
                                tst_a $33,0
                                set_a $cc,$ff   ;with flags on
                                bbr \1,zpt,fail2\?
                                bbs \1,zpt,ok2\?
                                trap            ;bbs branch not taken
                        fail2\? 
                                trap            ;bbr branch taken
                        ok2\?   
                                tst_a $cc,$ff
                                lda zpt
                                cmp #(1<<\1)
                                trap_ne         ;zp altered
                                lda #$ff-(1<<\1) ;testing 1 bit off
                                sta zpt
                                set_a $33,0     ;with flags off
                                bbs \1,zpt,fail3\?
                                bbr \1,zpt,ok3\?
                                trap            ;bbr branch not taken
                        fail3\? 
                                trap            ;bbs branch taken
                        ok3\?   
                                tst_a $33,0
                                set_a $cc,$ff   ;with flags on
                                bbs \1,zpt,fail4\?
                                bbr \1,zpt,ok4\?
                                trap            ;bbr branch not taken
                        fail4\? 
                                trap            ;bbs branch taken
                        ok4\?   
                                tst_a $cc,$ff
                                lda zpt
                                cmp #$ff-(1<<\1)
                                trap_ne         ;zp altered
                                endm
                        
071c : a211                     ldx #$11        ;test bbr/bbs integrity
071e : a022                     ldy #$22
                                bbt 0
0720 : a901            >        lda #(1<<0)    ;testing 1 bit on
0722 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0724 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0726 : 48              >            pha         ;use stack to load status
0727 : a933            >            lda #$33     ;precharge accu
0729 : 28              >            plp
                       >
072a : 0f0a06          >        bbr 0,zpt,fail10196
072d : 8f0a06          >        bbs 0,zpt,ok10196
                       >        trap            ;bbs branch not taken
0730 : 4c3007          >        jmp *           ;failed anyway
                       >
0733 :                 >fail10196
                       >        trap            ;bbr branch taken
0733 : 4c3307          >        jmp *           ;failed anyway
                       >
0736 :                 >ok10196   
                       >        tst_a $33,0
0736 : 08              >            php         ;save flags
0737 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0739 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
073b : 68              >            pla         ;load status
073c : 48              >            pha
                       >            cmp_flag 0
073d : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
073f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0741 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0742 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0744 : 48              >            pha         ;use stack to load status
0745 : a9cc            >            lda #$cc     ;precharge accu
0747 : 28              >            plp
                       >
0748 : 0f0a06          >        bbr 0,zpt,fail20196
074b : 8f0a06          >        bbs 0,zpt,ok20196
                       >        trap            ;bbs branch not taken
074e : 4c4e07          >        jmp *           ;failed anyway
                       >
0751 :                 >fail20196 
                       >        trap            ;bbr branch taken
0751 : 4c5107          >        jmp *           ;failed anyway
                       >
0754 :                 >ok20196   
                       >        tst_a $cc,$ff
0754 : 08              >            php         ;save flags
0755 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0757 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0759 : 68              >            pla         ;load status
075a : 48              >            pha
                       >            cmp_flag $ff
075b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
075d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
075f : 28              >            plp         ;restore status
                       >
0760 : a50a            >        lda zpt
0762 : c901            >        cmp #(1<<0)
                       >        trap_ne         ;zp altered
0764 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0766 : a9fe            >        lda #$ff-(1<<0) ;testing 1 bit off
0768 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
076a : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
076c : 48              >            pha         ;use stack to load status
076d : a933            >            lda #$33     ;precharge accu
076f : 28              >            plp
                       >
0770 : 8f0a06          >        bbs 0,zpt,fail30196
0773 : 0f0a06          >        bbr 0,zpt,ok30196
                       >        trap            ;bbr branch not taken
0776 : 4c7607          >        jmp *           ;failed anyway
                       >
0779 :                 >fail30196 
                       >        trap            ;bbs branch taken
0779 : 4c7907          >        jmp *           ;failed anyway
                       >
077c :                 >ok30196   
                       >        tst_a $33,0
077c : 08              >            php         ;save flags
077d : c933            >            cmp #$33     ;test result
                       >            trap_ne
077f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0781 : 68              >            pla         ;load status
0782 : 48              >            pha
                       >            cmp_flag 0
0783 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0785 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0787 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0788 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
078a : 48              >            pha         ;use stack to load status
078b : a9cc            >            lda #$cc     ;precharge accu
078d : 28              >            plp
                       >
078e : 8f0a06          >        bbs 0,zpt,fail40196
0791 : 0f0a06          >        bbr 0,zpt,ok40196
                       >        trap            ;bbr branch not taken
0794 : 4c9407          >        jmp *           ;failed anyway
                       >
0797 :                 >fail40196 
                       >        trap            ;bbs branch taken
0797 : 4c9707          >        jmp *           ;failed anyway
                       >
079a :                 >ok40196   
                       >        tst_a $cc,$ff
079a : 08              >            php         ;save flags
079b : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
079d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
079f : 68              >            pla         ;load status
07a0 : 48              >            pha
                       >            cmp_flag $ff
07a1 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
07a3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07a5 : 28              >            plp         ;restore status
                       >
07a6 : a50a            >        lda zpt
07a8 : c9fe            >        cmp #$ff-(1<<0)
                       >        trap_ne         ;zp altered
07aa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 1
07ac : a902            >        lda #(1<<1)    ;testing 1 bit on
07ae : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
07b0 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
07b2 : 48              >            pha         ;use stack to load status
07b3 : a933            >            lda #$33     ;precharge accu
07b5 : 28              >            plp
                       >
07b6 : 1f0a06          >        bbr 1,zpt,fail10231
07b9 : 9f0a06          >        bbs 1,zpt,ok10231
                       >        trap            ;bbs branch not taken
07bc : 4cbc07          >        jmp *           ;failed anyway
                       >
07bf :                 >fail10231
                       >        trap            ;bbr branch taken
07bf : 4cbf07          >        jmp *           ;failed anyway
                       >
07c2 :                 >ok10231   
                       >        tst_a $33,0
07c2 : 08              >            php         ;save flags
07c3 : c933            >            cmp #$33     ;test result
                       >            trap_ne
07c5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07c7 : 68              >            pla         ;load status
07c8 : 48              >            pha
                       >            cmp_flag 0
07c9 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
07cb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07cd : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
07ce : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
07d0 : 48              >            pha         ;use stack to load status
07d1 : a9cc            >            lda #$cc     ;precharge accu
07d3 : 28              >            plp
                       >
07d4 : 1f0a06          >        bbr 1,zpt,fail20231
07d7 : 9f0a06          >        bbs 1,zpt,ok20231
                       >        trap            ;bbs branch not taken
07da : 4cda07          >        jmp *           ;failed anyway
                       >
07dd :                 >fail20231 
                       >        trap            ;bbr branch taken
07dd : 4cdd07          >        jmp *           ;failed anyway
                       >
07e0 :                 >ok20231   
                       >        tst_a $cc,$ff
07e0 : 08              >            php         ;save flags
07e1 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
07e3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07e5 : 68              >            pla         ;load status
07e6 : 48              >            pha
                       >            cmp_flag $ff
07e7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
07e9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07eb : 28              >            plp         ;restore status
                       >
07ec : a50a            >        lda zpt
07ee : c902            >        cmp #(1<<1)
                       >        trap_ne         ;zp altered
07f0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
07f2 : a9fd            >        lda #$ff-(1<<1) ;testing 1 bit off
07f4 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
07f6 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
07f8 : 48              >            pha         ;use stack to load status
07f9 : a933            >            lda #$33     ;precharge accu
07fb : 28              >            plp
                       >
07fc : 9f0a06          >        bbs 1,zpt,fail30231
07ff : 1f0a06          >        bbr 1,zpt,ok30231
                       >        trap            ;bbr branch not taken
0802 : 4c0208          >        jmp *           ;failed anyway
                       >
0805 :                 >fail30231 
                       >        trap            ;bbs branch taken
0805 : 4c0508          >        jmp *           ;failed anyway
                       >
0808 :                 >ok30231   
                       >        tst_a $33,0
0808 : 08              >            php         ;save flags
0809 : c933            >            cmp #$33     ;test result
                       >            trap_ne
080b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
080d : 68              >            pla         ;load status
080e : 48              >            pha
                       >            cmp_flag 0
080f : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0811 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0813 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0814 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0816 : 48              >            pha         ;use stack to load status
0817 : a9cc            >            lda #$cc     ;precharge accu
0819 : 28              >            plp
                       >
081a : 9f0a06          >        bbs 1,zpt,fail40231
081d : 1f0a06          >        bbr 1,zpt,ok40231
                       >        trap            ;bbr branch not taken
0820 : 4c2008          >        jmp *           ;failed anyway
                       >
0823 :                 >fail40231 
                       >        trap            ;bbs branch taken
0823 : 4c2308          >        jmp *           ;failed anyway
                       >
0826 :                 >ok40231   
                       >        tst_a $cc,$ff
0826 : 08              >            php         ;save flags
0827 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0829 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
082b : 68              >            pla         ;load status
082c : 48              >            pha
                       >            cmp_flag $ff
082d : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
082f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0831 : 28              >            plp         ;restore status
                       >
0832 : a50a            >        lda zpt
0834 : c9fd            >        cmp #$ff-(1<<1)
                       >        trap_ne         ;zp altered
0836 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 2
0838 : a904            >        lda #(1<<2)    ;testing 1 bit on
083a : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
083c : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
083e : 48              >            pha         ;use stack to load status
083f : a933            >            lda #$33     ;precharge accu
0841 : 28              >            plp
                       >
0842 : 2f0a06          >        bbr 2,zpt,fail10266
0845 : af0a06          >        bbs 2,zpt,ok10266
                       >        trap            ;bbs branch not taken
0848 : 4c4808          >        jmp *           ;failed anyway
                       >
084b :                 >fail10266
                       >        trap            ;bbr branch taken
084b : 4c4b08          >        jmp *           ;failed anyway
                       >
084e :                 >ok10266   
                       >        tst_a $33,0
084e : 08              >            php         ;save flags
084f : c933            >            cmp #$33     ;test result
                       >            trap_ne
0851 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0853 : 68              >            pla         ;load status
0854 : 48              >            pha
                       >            cmp_flag 0
0855 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0857 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0859 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
085a : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
085c : 48              >            pha         ;use stack to load status
085d : a9cc            >            lda #$cc     ;precharge accu
085f : 28              >            plp
                       >
0860 : 2f0a06          >        bbr 2,zpt,fail20266
0863 : af0a06          >        bbs 2,zpt,ok20266
                       >        trap            ;bbs branch not taken
0866 : 4c6608          >        jmp *           ;failed anyway
                       >
0869 :                 >fail20266 
                       >        trap            ;bbr branch taken
0869 : 4c6908          >        jmp *           ;failed anyway
                       >
086c :                 >ok20266   
                       >        tst_a $cc,$ff
086c : 08              >            php         ;save flags
086d : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
086f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0871 : 68              >            pla         ;load status
0872 : 48              >            pha
                       >            cmp_flag $ff
0873 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0875 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0877 : 28              >            plp         ;restore status
                       >
0878 : a50a            >        lda zpt
087a : c904            >        cmp #(1<<2)
                       >        trap_ne         ;zp altered
087c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
087e : a9fb            >        lda #$ff-(1<<2) ;testing 1 bit off
0880 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0882 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0884 : 48              >            pha         ;use stack to load status
0885 : a933            >            lda #$33     ;precharge accu
0887 : 28              >            plp
                       >
0888 : af0a06          >        bbs 2,zpt,fail30266
088b : 2f0a06          >        bbr 2,zpt,ok30266
                       >        trap            ;bbr branch not taken
088e : 4c8e08          >        jmp *           ;failed anyway
                       >
0891 :                 >fail30266 
                       >        trap            ;bbs branch taken
0891 : 4c9108          >        jmp *           ;failed anyway
                       >
0894 :                 >ok30266   
                       >        tst_a $33,0
0894 : 08              >            php         ;save flags
0895 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0897 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0899 : 68              >            pla         ;load status
089a : 48              >            pha
                       >            cmp_flag 0
089b : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
089d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
089f : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
08a0 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
08a2 : 48              >            pha         ;use stack to load status
08a3 : a9cc            >            lda #$cc     ;precharge accu
08a5 : 28              >            plp
                       >
08a6 : af0a06          >        bbs 2,zpt,fail40266
08a9 : 2f0a06          >        bbr 2,zpt,ok40266
                       >        trap            ;bbr branch not taken
08ac : 4cac08          >        jmp *           ;failed anyway
                       >
08af :                 >fail40266 
                       >        trap            ;bbs branch taken
08af : 4caf08          >        jmp *           ;failed anyway
                       >
08b2 :                 >ok40266   
                       >        tst_a $cc,$ff
08b2 : 08              >            php         ;save flags
08b3 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
08b5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
08b7 : 68              >            pla         ;load status
08b8 : 48              >            pha
                       >            cmp_flag $ff
08b9 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
08bb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
08bd : 28              >            plp         ;restore status
                       >
08be : a50a            >        lda zpt
08c0 : c9fb            >        cmp #$ff-(1<<2)
                       >        trap_ne         ;zp altered
08c2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 3
08c4 : a908            >        lda #(1<<3)    ;testing 1 bit on
08c6 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
08c8 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
08ca : 48              >            pha         ;use stack to load status
08cb : a933            >            lda #$33     ;precharge accu
08cd : 28              >            plp
                       >
08ce : 3f0a06          >        bbr 3,zpt,fail10301
08d1 : bf0a06          >        bbs 3,zpt,ok10301
                       >        trap            ;bbs branch not taken
08d4 : 4cd408          >        jmp *           ;failed anyway
                       >
08d7 :                 >fail10301
                       >        trap            ;bbr branch taken
08d7 : 4cd708          >        jmp *           ;failed anyway
                       >
08da :                 >ok10301   
                       >        tst_a $33,0
08da : 08              >            php         ;save flags
08db : c933            >            cmp #$33     ;test result
                       >            trap_ne
08dd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
08df : 68              >            pla         ;load status
08e0 : 48              >            pha
                       >            cmp_flag 0
08e1 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
08e3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
08e5 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
08e6 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
08e8 : 48              >            pha         ;use stack to load status
08e9 : a9cc            >            lda #$cc     ;precharge accu
08eb : 28              >            plp
                       >
08ec : 3f0a06          >        bbr 3,zpt,fail20301
08ef : bf0a06          >        bbs 3,zpt,ok20301
                       >        trap            ;bbs branch not taken
08f2 : 4cf208          >        jmp *           ;failed anyway
                       >
08f5 :                 >fail20301 
                       >        trap            ;bbr branch taken
08f5 : 4cf508          >        jmp *           ;failed anyway
                       >
08f8 :                 >ok20301   
                       >        tst_a $cc,$ff
08f8 : 08              >            php         ;save flags
08f9 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
08fb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
08fd : 68              >            pla         ;load status
08fe : 48              >            pha
                       >            cmp_flag $ff
08ff : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0901 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0903 : 28              >            plp         ;restore status
                       >
0904 : a50a            >        lda zpt
0906 : c908            >        cmp #(1<<3)
                       >        trap_ne         ;zp altered
0908 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
090a : a9f7            >        lda #$ff-(1<<3) ;testing 1 bit off
090c : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
090e : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0910 : 48              >            pha         ;use stack to load status
0911 : a933            >            lda #$33     ;precharge accu
0913 : 28              >            plp
                       >
0914 : bf0a06          >        bbs 3,zpt,fail30301
0917 : 3f0a06          >        bbr 3,zpt,ok30301
                       >        trap            ;bbr branch not taken
091a : 4c1a09          >        jmp *           ;failed anyway
                       >
091d :                 >fail30301 
                       >        trap            ;bbs branch taken
091d : 4c1d09          >        jmp *           ;failed anyway
                       >
0920 :                 >ok30301   
                       >        tst_a $33,0
0920 : 08              >            php         ;save flags
0921 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0923 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0925 : 68              >            pla         ;load status
0926 : 48              >            pha
                       >            cmp_flag 0
0927 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0929 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
092b : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
092c : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
092e : 48              >            pha         ;use stack to load status
092f : a9cc            >            lda #$cc     ;precharge accu
0931 : 28              >            plp
                       >
0932 : bf0a06          >        bbs 3,zpt,fail40301
0935 : 3f0a06          >        bbr 3,zpt,ok40301
                       >        trap            ;bbr branch not taken
0938 : 4c3809          >        jmp *           ;failed anyway
                       >
093b :                 >fail40301 
                       >        trap            ;bbs branch taken
093b : 4c3b09          >        jmp *           ;failed anyway
                       >
093e :                 >ok40301   
                       >        tst_a $cc,$ff
093e : 08              >            php         ;save flags
093f : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0941 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0943 : 68              >            pla         ;load status
0944 : 48              >            pha
                       >            cmp_flag $ff
0945 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0947 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0949 : 28              >            plp         ;restore status
                       >
094a : a50a            >        lda zpt
094c : c9f7            >        cmp #$ff-(1<<3)
                       >        trap_ne         ;zp altered
094e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 4
0950 : a910            >        lda #(1<<4)    ;testing 1 bit on
0952 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0954 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0956 : 48              >            pha         ;use stack to load status
0957 : a933            >            lda #$33     ;precharge accu
0959 : 28              >            plp
                       >
095a : 4f0a06          >        bbr 4,zpt,fail10336
095d : cf0a06          >        bbs 4,zpt,ok10336
                       >        trap            ;bbs branch not taken
0960 : 4c6009          >        jmp *           ;failed anyway
                       >
0963 :                 >fail10336
                       >        trap            ;bbr branch taken
0963 : 4c6309          >        jmp *           ;failed anyway
                       >
0966 :                 >ok10336   
                       >        tst_a $33,0
0966 : 08              >            php         ;save flags
0967 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0969 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
096b : 68              >            pla         ;load status
096c : 48              >            pha
                       >            cmp_flag 0
096d : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
096f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0971 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0972 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0974 : 48              >            pha         ;use stack to load status
0975 : a9cc            >            lda #$cc     ;precharge accu
0977 : 28              >            plp
                       >
0978 : 4f0a06          >        bbr 4,zpt,fail20336
097b : cf0a06          >        bbs 4,zpt,ok20336
                       >        trap            ;bbs branch not taken
097e : 4c7e09          >        jmp *           ;failed anyway
                       >
0981 :                 >fail20336 
                       >        trap            ;bbr branch taken
0981 : 4c8109          >        jmp *           ;failed anyway
                       >
0984 :                 >ok20336   
                       >        tst_a $cc,$ff
0984 : 08              >            php         ;save flags
0985 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0987 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0989 : 68              >            pla         ;load status
098a : 48              >            pha
                       >            cmp_flag $ff
098b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
098d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
098f : 28              >            plp         ;restore status
                       >
0990 : a50a            >        lda zpt
0992 : c910            >        cmp #(1<<4)
                       >        trap_ne         ;zp altered
0994 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0996 : a9ef            >        lda #$ff-(1<<4) ;testing 1 bit off
0998 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
099a : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
099c : 48              >            pha         ;use stack to load status
099d : a933            >            lda #$33     ;precharge accu
099f : 28              >            plp
                       >
09a0 : cf0a06          >        bbs 4,zpt,fail30336
09a3 : 4f0a06          >        bbr 4,zpt,ok30336
                       >        trap            ;bbr branch not taken
09a6 : 4ca609          >        jmp *           ;failed anyway
                       >
09a9 :                 >fail30336 
                       >        trap            ;bbs branch taken
09a9 : 4ca909          >        jmp *           ;failed anyway
                       >
09ac :                 >ok30336   
                       >        tst_a $33,0
09ac : 08              >            php         ;save flags
09ad : c933            >            cmp #$33     ;test result
                       >            trap_ne
09af : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09b1 : 68              >            pla         ;load status
09b2 : 48              >            pha
                       >            cmp_flag 0
09b3 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
09b5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09b7 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
09b8 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
09ba : 48              >            pha         ;use stack to load status
09bb : a9cc            >            lda #$cc     ;precharge accu
09bd : 28              >            plp
                       >
09be : cf0a06          >        bbs 4,zpt,fail40336
09c1 : 4f0a06          >        bbr 4,zpt,ok40336
                       >        trap            ;bbr branch not taken
09c4 : 4cc409          >        jmp *           ;failed anyway
                       >
09c7 :                 >fail40336 
                       >        trap            ;bbs branch taken
09c7 : 4cc709          >        jmp *           ;failed anyway
                       >
09ca :                 >ok40336   
                       >        tst_a $cc,$ff
09ca : 08              >            php         ;save flags
09cb : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
09cd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09cf : 68              >            pla         ;load status
09d0 : 48              >            pha
                       >            cmp_flag $ff
09d1 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
09d3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09d5 : 28              >            plp         ;restore status
                       >
09d6 : a50a            >        lda zpt
09d8 : c9ef            >        cmp #$ff-(1<<4)
                       >        trap_ne         ;zp altered
09da : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 5
09dc : a920            >        lda #(1<<5)    ;testing 1 bit on
09de : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
09e0 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
09e2 : 48              >            pha         ;use stack to load status
09e3 : a933            >            lda #$33     ;precharge accu
09e5 : 28              >            plp
                       >
09e6 : 5f0a06          >        bbr 5,zpt,fail10371
09e9 : df0a06          >        bbs 5,zpt,ok10371
                       >        trap            ;bbs branch not taken
09ec : 4cec09          >        jmp *           ;failed anyway
                       >
09ef :                 >fail10371
                       >        trap            ;bbr branch taken
09ef : 4cef09          >        jmp *           ;failed anyway
                       >
09f2 :                 >ok10371   
                       >        tst_a $33,0
09f2 : 08              >            php         ;save flags
09f3 : c933            >            cmp #$33     ;test result
                       >            trap_ne
09f5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09f7 : 68              >            pla         ;load status
09f8 : 48              >            pha
                       >            cmp_flag 0
09f9 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
09fb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
09fd : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
09fe : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0a00 : 48              >            pha         ;use stack to load status
0a01 : a9cc            >            lda #$cc     ;precharge accu
0a03 : 28              >            plp
                       >
0a04 : 5f0a06          >        bbr 5,zpt,fail20371
0a07 : df0a06          >        bbs 5,zpt,ok20371
                       >        trap            ;bbs branch not taken
0a0a : 4c0a0a          >        jmp *           ;failed anyway
                       >
0a0d :                 >fail20371 
                       >        trap            ;bbr branch taken
0a0d : 4c0d0a          >        jmp *           ;failed anyway
                       >
0a10 :                 >ok20371   
                       >        tst_a $cc,$ff
0a10 : 08              >            php         ;save flags
0a11 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0a13 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a15 : 68              >            pla         ;load status
0a16 : 48              >            pha
                       >            cmp_flag $ff
0a17 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0a19 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a1b : 28              >            plp         ;restore status
                       >
0a1c : a50a            >        lda zpt
0a1e : c920            >        cmp #(1<<5)
                       >        trap_ne         ;zp altered
0a20 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a22 : a9df            >        lda #$ff-(1<<5) ;testing 1 bit off
0a24 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0a26 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0a28 : 48              >            pha         ;use stack to load status
0a29 : a933            >            lda #$33     ;precharge accu
0a2b : 28              >            plp
                       >
0a2c : df0a06          >        bbs 5,zpt,fail30371
0a2f : 5f0a06          >        bbr 5,zpt,ok30371
                       >        trap            ;bbr branch not taken
0a32 : 4c320a          >        jmp *           ;failed anyway
                       >
0a35 :                 >fail30371 
                       >        trap            ;bbs branch taken
0a35 : 4c350a          >        jmp *           ;failed anyway
                       >
0a38 :                 >ok30371   
                       >        tst_a $33,0
0a38 : 08              >            php         ;save flags
0a39 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0a3b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a3d : 68              >            pla         ;load status
0a3e : 48              >            pha
                       >            cmp_flag 0
0a3f : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0a41 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a43 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0a44 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0a46 : 48              >            pha         ;use stack to load status
0a47 : a9cc            >            lda #$cc     ;precharge accu
0a49 : 28              >            plp
                       >
0a4a : df0a06          >        bbs 5,zpt,fail40371
0a4d : 5f0a06          >        bbr 5,zpt,ok40371
                       >        trap            ;bbr branch not taken
0a50 : 4c500a          >        jmp *           ;failed anyway
                       >
0a53 :                 >fail40371 
                       >        trap            ;bbs branch taken
0a53 : 4c530a          >        jmp *           ;failed anyway
                       >
0a56 :                 >ok40371   
                       >        tst_a $cc,$ff
0a56 : 08              >            php         ;save flags
0a57 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0a59 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a5b : 68              >            pla         ;load status
0a5c : 48              >            pha
                       >            cmp_flag $ff
0a5d : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0a5f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a61 : 28              >            plp         ;restore status
                       >
0a62 : a50a            >        lda zpt
0a64 : c9df            >        cmp #$ff-(1<<5)
                       >        trap_ne         ;zp altered
0a66 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 6
0a68 : a940            >        lda #(1<<6)    ;testing 1 bit on
0a6a : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0a6c : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0a6e : 48              >            pha         ;use stack to load status
0a6f : a933            >            lda #$33     ;precharge accu
0a71 : 28              >            plp
                       >
0a72 : 6f0a06          >        bbr 6,zpt,fail10406
0a75 : ef0a06          >        bbs 6,zpt,ok10406
                       >        trap            ;bbs branch not taken
0a78 : 4c780a          >        jmp *           ;failed anyway
                       >
0a7b :                 >fail10406
                       >        trap            ;bbr branch taken
0a7b : 4c7b0a          >        jmp *           ;failed anyway
                       >
0a7e :                 >ok10406   
                       >        tst_a $33,0
0a7e : 08              >            php         ;save flags
0a7f : c933            >            cmp #$33     ;test result
                       >            trap_ne
0a81 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a83 : 68              >            pla         ;load status
0a84 : 48              >            pha
                       >            cmp_flag 0
0a85 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0a87 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0a89 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0a8a : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0a8c : 48              >            pha         ;use stack to load status
0a8d : a9cc            >            lda #$cc     ;precharge accu
0a8f : 28              >            plp
                       >
0a90 : 6f0a06          >        bbr 6,zpt,fail20406
0a93 : ef0a06          >        bbs 6,zpt,ok20406
                       >        trap            ;bbs branch not taken
0a96 : 4c960a          >        jmp *           ;failed anyway
                       >
0a99 :                 >fail20406 
                       >        trap            ;bbr branch taken
0a99 : 4c990a          >        jmp *           ;failed anyway
                       >
0a9c :                 >ok20406   
                       >        tst_a $cc,$ff
0a9c : 08              >            php         ;save flags
0a9d : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0a9f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0aa1 : 68              >            pla         ;load status
0aa2 : 48              >            pha
                       >            cmp_flag $ff
0aa3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0aa5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0aa7 : 28              >            plp         ;restore status
                       >
0aa8 : a50a            >        lda zpt
0aaa : c940            >        cmp #(1<<6)
                       >        trap_ne         ;zp altered
0aac : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0aae : a9bf            >        lda #$ff-(1<<6) ;testing 1 bit off
0ab0 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0ab2 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0ab4 : 48              >            pha         ;use stack to load status
0ab5 : a933            >            lda #$33     ;precharge accu
0ab7 : 28              >            plp
                       >
0ab8 : ef0a06          >        bbs 6,zpt,fail30406
0abb : 6f0a06          >        bbr 6,zpt,ok30406
                       >        trap            ;bbr branch not taken
0abe : 4cbe0a          >        jmp *           ;failed anyway
                       >
0ac1 :                 >fail30406 
                       >        trap            ;bbs branch taken
0ac1 : 4cc10a          >        jmp *           ;failed anyway
                       >
0ac4 :                 >ok30406   
                       >        tst_a $33,0
0ac4 : 08              >            php         ;save flags
0ac5 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0ac7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ac9 : 68              >            pla         ;load status
0aca : 48              >            pha
                       >            cmp_flag 0
0acb : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0acd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0acf : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0ad0 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0ad2 : 48              >            pha         ;use stack to load status
0ad3 : a9cc            >            lda #$cc     ;precharge accu
0ad5 : 28              >            plp
                       >
0ad6 : ef0a06          >        bbs 6,zpt,fail40406
0ad9 : 6f0a06          >        bbr 6,zpt,ok40406
                       >        trap            ;bbr branch not taken
0adc : 4cdc0a          >        jmp *           ;failed anyway
                       >
0adf :                 >fail40406 
                       >        trap            ;bbs branch taken
0adf : 4cdf0a          >        jmp *           ;failed anyway
                       >
0ae2 :                 >ok40406   
                       >        tst_a $cc,$ff
0ae2 : 08              >            php         ;save flags
0ae3 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0ae5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ae7 : 68              >            pla         ;load status
0ae8 : 48              >            pha
                       >            cmp_flag $ff
0ae9 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0aeb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0aed : 28              >            plp         ;restore status
                       >
0aee : a50a            >        lda zpt
0af0 : c9bf            >        cmp #$ff-(1<<6)
                       >        trap_ne         ;zp altered
0af2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                bbt 7
0af4 : a980            >        lda #(1<<7)    ;testing 1 bit on
0af6 : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0af8 : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0afa : 48              >            pha         ;use stack to load status
0afb : a933            >            lda #$33     ;precharge accu
0afd : 28              >            plp
                       >
0afe : 7f0a06          >        bbr 7,zpt,fail10441
0b01 : ff0a06          >        bbs 7,zpt,ok10441
                       >        trap            ;bbs branch not taken
0b04 : 4c040b          >        jmp *           ;failed anyway
                       >
0b07 :                 >fail10441
                       >        trap            ;bbr branch taken
0b07 : 4c070b          >        jmp *           ;failed anyway
                       >
0b0a :                 >ok10441   
                       >        tst_a $33,0
0b0a : 08              >            php         ;save flags
0b0b : c933            >            cmp #$33     ;test result
                       >            trap_ne
0b0d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b0f : 68              >            pla         ;load status
0b10 : 48              >            pha
                       >            cmp_flag 0
0b11 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0b13 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b15 : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0b16 : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0b18 : 48              >            pha         ;use stack to load status
0b19 : a9cc            >            lda #$cc     ;precharge accu
0b1b : 28              >            plp
                       >
0b1c : 7f0a06          >        bbr 7,zpt,fail20441
0b1f : ff0a06          >        bbs 7,zpt,ok20441
                       >        trap            ;bbs branch not taken
0b22 : 4c220b          >        jmp *           ;failed anyway
                       >
0b25 :                 >fail20441 
                       >        trap            ;bbr branch taken
0b25 : 4c250b          >        jmp *           ;failed anyway
                       >
0b28 :                 >ok20441   
                       >        tst_a $cc,$ff
0b28 : 08              >            php         ;save flags
0b29 : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0b2b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b2d : 68              >            pla         ;load status
0b2e : 48              >            pha
                       >            cmp_flag $ff
0b2f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0b31 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b33 : 28              >            plp         ;restore status
                       >
0b34 : a50a            >        lda zpt
0b36 : c980            >        cmp #(1<<7)
                       >        trap_ne         ;zp altered
0b38 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b3a : a97f            >        lda #$ff-(1<<7) ;testing 1 bit off
0b3c : 850a            >        sta zpt
                       >        set_a $33,0     ;with flags off
                       >            load_flag 0     
0b3e : a900            >            lda #0                  ;allow test to change I-flag (no mask)
                       >
0b40 : 48              >            pha         ;use stack to load status
0b41 : a933            >            lda #$33     ;precharge accu
0b43 : 28              >            plp
                       >
0b44 : ff0a06          >        bbs 7,zpt,fail30441
0b47 : 7f0a06          >        bbr 7,zpt,ok30441
                       >        trap            ;bbr branch not taken
0b4a : 4c4a0b          >        jmp *           ;failed anyway
                       >
0b4d :                 >fail30441 
                       >        trap            ;bbs branch taken
0b4d : 4c4d0b          >        jmp *           ;failed anyway
                       >
0b50 :                 >ok30441   
                       >        tst_a $33,0
0b50 : 08              >            php         ;save flags
0b51 : c933            >            cmp #$33     ;test result
                       >            trap_ne
0b53 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b55 : 68              >            pla         ;load status
0b56 : 48              >            pha
                       >            cmp_flag 0
0b57 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0b59 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b5b : 28              >            plp         ;restore status
                       >
                       >        set_a $cc,$ff   ;with flags on
                       >            load_flag $ff   
0b5c : a9ff            >            lda #$ff                ;allow test to change I-flag (no mask)
                       >
0b5e : 48              >            pha         ;use stack to load status
0b5f : a9cc            >            lda #$cc     ;precharge accu
0b61 : 28              >            plp
                       >
0b62 : ff0a06          >        bbs 7,zpt,fail40441
0b65 : 7f0a06          >        bbr 7,zpt,ok40441
                       >        trap            ;bbr branch not taken
0b68 : 4c680b          >        jmp *           ;failed anyway
                       >
0b6b :                 >fail40441 
                       >        trap            ;bbs branch taken
0b6b : 4c6b0b          >        jmp *           ;failed anyway
                       >
0b6e :                 >ok40441   
                       >        tst_a $cc,$ff
0b6e : 08              >            php         ;save flags
0b6f : c9cc            >            cmp #$cc     ;test result
                       >            trap_ne
0b71 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b73 : 68              >            pla         ;load status
0b74 : 48              >            pha
                       >            cmp_flag $ff
0b75 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0b77 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0b79 : 28              >            plp         ;restore status
                       >
0b7a : a50a            >        lda zpt
0b7c : c97f            >        cmp #$ff-(1<<7)
                       >        trap_ne         ;zp altered
0b7e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
0b80 : e011                     cpx #$11
                                trap_ne         ;x overwritten
0b82 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0b84 : c022                     cpy #$22
                                trap_ne         ;y overwritten
0b86 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test 
0b88 : ad0202          >            lda test_case   ;previous test
0b8b : c906            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
0b8d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0007 =                 >test_num = test_num + 1
0b8f : a907            >            lda #test_num   ;*** next tests' number
0b91 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        bbrc    macro           ;\1 = bitnum
                                bbr \1,zpt,skip\?
                                eor #(1<<\1)       
                        skip\?
                                endm
                        bbsc    macro           ;\1 = bitnum
                                bbs \1,zpt,skip\?
                                eor #(1<<\1)       
                        skip\?
                                endm
                        
0b94 : a900                     lda #0          ;combined bit test
0b96 : 850a                     sta zpt
0b98 : a900             bbcl    lda #0
                                bbrc 0
0b9a : 0f0a02          >        bbr 0,zpt,skip0480
0b9d : 4901            >        eor #(1<<0)       
0b9f :                 >skip0480
                        
                                bbrc 1
0b9f : 1f0a02          >        bbr 1,zpt,skip0481
0ba2 : 4902            >        eor #(1<<1)       
0ba4 :                 >skip0481
                        
                                bbrc 2
0ba4 : 2f0a02          >        bbr 2,zpt,skip0482
0ba7 : 4904            >        eor #(1<<2)       
0ba9 :                 >skip0482
                        
                                bbrc 3
0ba9 : 3f0a02          >        bbr 3,zpt,skip0483
0bac : 4908            >        eor #(1<<3)       
0bae :                 >skip0483
                        
                                bbrc 4
0bae : 4f0a02          >        bbr 4,zpt,skip0484
0bb1 : 4910            >        eor #(1<<4)       
0bb3 :                 >skip0484
                        
                                bbrc 5
0bb3 : 5f0a02          >        bbr 5,zpt,skip0485
0bb6 : 4920            >        eor #(1<<5)       
0bb8 :                 >skip0485
                        
                                bbrc 6
0bb8 : 6f0a02          >        bbr 6,zpt,skip0486
0bbb : 4940            >        eor #(1<<6)       
0bbd :                 >skip0486
                        
                                bbrc 7
0bbd : 7f0a02          >        bbr 7,zpt,skip0487
0bc0 : 4980            >        eor #(1<<7)       
0bc2 :                 >skip0487
                        
0bc2 : 450a                     eor zpt
                                trap_ne         ;failed bbr bitnum in accu       
0bc4 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0bc6 : a9ff                     lda #$ff
                                bbsc 0
0bc8 : 8f0a02          >        bbs 0,zpt,skip0489
0bcb : 4901            >        eor #(1<<0)       
0bcd :                 >skip0489
                        
                                bbsc 1
0bcd : 9f0a02          >        bbs 1,zpt,skip0490
0bd0 : 4902            >        eor #(1<<1)       
0bd2 :                 >skip0490
                        
                                bbsc 2
0bd2 : af0a02          >        bbs 2,zpt,skip0491
0bd5 : 4904            >        eor #(1<<2)       
0bd7 :                 >skip0491
                        
                                bbsc 3
0bd7 : bf0a02          >        bbs 3,zpt,skip0492
0bda : 4908            >        eor #(1<<3)       
0bdc :                 >skip0492
                        
                                bbsc 4
0bdc : cf0a02          >        bbs 4,zpt,skip0493
0bdf : 4910            >        eor #(1<<4)       
0be1 :                 >skip0493
                        
                                bbsc 5
0be1 : df0a02          >        bbs 5,zpt,skip0494
0be4 : 4920            >        eor #(1<<5)       
0be6 :                 >skip0494
                        
                                bbsc 6
0be6 : ef0a02          >        bbs 6,zpt,skip0495
0be9 : 4940            >        eor #(1<<6)       
0beb :                 >skip0495
                        
                                bbsc 7
0beb : ff0a02          >        bbs 7,zpt,skip0496
0bee : 4980            >        eor #(1<<7)       
0bf0 :                 >skip0496
                        
0bf0 : 450a                     eor zpt
                                trap_ne         ;failed bbs bitnum in accu       
0bf2 : d0fe            >        bne *           ;failed not equal (non zero)
                        
0bf4 : e60a                     inc zpt
0bf6 : d0a0                     bne bbcl
                                next_test            
0bf8 : ad0202          >            lda test_case   ;previous test
0bfb : c907            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
0bfd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0008 =                 >test_num = test_num + 1
0bff : a908            >            lda #test_num   ;*** next tests' number
0c01 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                            endif
                            
                        ; testing NOP
                        
                        nop_test    macro       ;\1 = opcode, \2 = # of bytes
                                    ldy #$42
                                    ldx #4-\2
                                    db  \1          ;test nop length
                                if \2 = 1
                                    dex
                                    dex
                                endif
                                if \2 = 2
                                    iny
                                    dex
                                endif
                                if \2 = 3
                                    iny
                                    iny
                                endif
                                    dex
                                    trap_ne         ;wrong number of bytes
                                    set_a $ff-\1,0
                                    db  \1          ;test nop integrity - flags off
                                    nop
                                    nop
                                    tst_a $ff-\1,0
                                    set_a $aa-\1,$ff
                                    db  \1          ;test nop integrity - flags on
                                    nop
                                    nop
                                    tst_a $aa-\1,$ff
                                    cpy #$42
                                    trap_ne         ;y changed
                                    cpx #0
                                    trap_ne         ;x changed
                                    endm
                                    
                                nop_test $02,2
0c04 : a042            >            ldy #$42
0c06 : a202            >            ldx #4-2
0c08 : 02              >            db  $02          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0c09 : c8              >            iny
0c0a : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0c0b : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0c0c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$02,0
                       >            load_flag 0
0c0e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0c10 : 48              >            pha         ;use stack to load status
0c11 : a9fd            >            lda #$ff-$02     ;precharge accu
0c13 : 28              >            plp
                       >
0c14 : 02              >            db  $02          ;test nop integrity - flags off
0c15 : ea              >            nop
0c16 : ea              >            nop
                       >            tst_a $ff-$02,0
0c17 : 08              >            php         ;save flags
0c18 : c9fd            >            cmp #$ff-$02     ;test result
                       >            trap_ne
0c1a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c1c : 68              >            pla         ;load status
0c1d : 48              >            pha
                       >            cmp_flag 0
0c1e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0c20 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c22 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$02,$ff
                       >            load_flag $ff
0c23 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0c25 : 48              >            pha         ;use stack to load status
0c26 : a9a8            >            lda #$aa-$02     ;precharge accu
0c28 : 28              >            plp
                       >
0c29 : 02              >            db  $02          ;test nop integrity - flags on
0c2a : ea              >            nop
0c2b : ea              >            nop
                       >            tst_a $aa-$02,$ff
0c2c : 08              >            php         ;save flags
0c2d : c9a8            >            cmp #$aa-$02     ;test result
                       >            trap_ne
0c2f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c31 : 68              >            pla         ;load status
0c32 : 48              >            pha
                       >            cmp_flag $ff
0c33 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0c35 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c37 : 28              >            plp         ;restore status
                       >
0c38 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0c3a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c3c : e000            >            cpx #0
                       >            trap_ne         ;x changed
0c3e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $22,2
0c40 : a042            >            ldy #$42
0c42 : a202            >            ldx #4-2
0c44 : 22              >            db  $22          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0c45 : c8              >            iny
0c46 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0c47 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0c48 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$22,0
                       >            load_flag 0
0c4a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0c4c : 48              >            pha         ;use stack to load status
0c4d : a9dd            >            lda #$ff-$22     ;precharge accu
0c4f : 28              >            plp
                       >
0c50 : 22              >            db  $22          ;test nop integrity - flags off
0c51 : ea              >            nop
0c52 : ea              >            nop
                       >            tst_a $ff-$22,0
0c53 : 08              >            php         ;save flags
0c54 : c9dd            >            cmp #$ff-$22     ;test result
                       >            trap_ne
0c56 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c58 : 68              >            pla         ;load status
0c59 : 48              >            pha
                       >            cmp_flag 0
0c5a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0c5c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c5e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$22,$ff
                       >            load_flag $ff
0c5f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0c61 : 48              >            pha         ;use stack to load status
0c62 : a988            >            lda #$aa-$22     ;precharge accu
0c64 : 28              >            plp
                       >
0c65 : 22              >            db  $22          ;test nop integrity - flags on
0c66 : ea              >            nop
0c67 : ea              >            nop
                       >            tst_a $aa-$22,$ff
0c68 : 08              >            php         ;save flags
0c69 : c988            >            cmp #$aa-$22     ;test result
                       >            trap_ne
0c6b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c6d : 68              >            pla         ;load status
0c6e : 48              >            pha
                       >            cmp_flag $ff
0c6f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0c71 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c73 : 28              >            plp         ;restore status
                       >
0c74 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0c76 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c78 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0c7a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $42,2
0c7c : a042            >            ldy #$42
0c7e : a202            >            ldx #4-2
0c80 : 42              >            db  $42          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0c81 : c8              >            iny
0c82 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0c83 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0c84 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$42,0
                       >            load_flag 0
0c86 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0c88 : 48              >            pha         ;use stack to load status
0c89 : a9bd            >            lda #$ff-$42     ;precharge accu
0c8b : 28              >            plp
                       >
0c8c : 42              >            db  $42          ;test nop integrity - flags off
0c8d : ea              >            nop
0c8e : ea              >            nop
                       >            tst_a $ff-$42,0
0c8f : 08              >            php         ;save flags
0c90 : c9bd            >            cmp #$ff-$42     ;test result
                       >            trap_ne
0c92 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c94 : 68              >            pla         ;load status
0c95 : 48              >            pha
                       >            cmp_flag 0
0c96 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0c98 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0c9a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$42,$ff
                       >            load_flag $ff
0c9b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0c9d : 48              >            pha         ;use stack to load status
0c9e : a968            >            lda #$aa-$42     ;precharge accu
0ca0 : 28              >            plp
                       >
0ca1 : 42              >            db  $42          ;test nop integrity - flags on
0ca2 : ea              >            nop
0ca3 : ea              >            nop
                       >            tst_a $aa-$42,$ff
0ca4 : 08              >            php         ;save flags
0ca5 : c968            >            cmp #$aa-$42     ;test result
                       >            trap_ne
0ca7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ca9 : 68              >            pla         ;load status
0caa : 48              >            pha
                       >            cmp_flag $ff
0cab : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0cad : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0caf : 28              >            plp         ;restore status
                       >
0cb0 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0cb2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0cb4 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0cb6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $62,2
0cb8 : a042            >            ldy #$42
0cba : a202            >            ldx #4-2
0cbc : 62              >            db  $62          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0cbd : c8              >            iny
0cbe : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0cbf : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0cc0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$62,0
                       >            load_flag 0
0cc2 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0cc4 : 48              >            pha         ;use stack to load status
0cc5 : a99d            >            lda #$ff-$62     ;precharge accu
0cc7 : 28              >            plp
                       >
0cc8 : 62              >            db  $62          ;test nop integrity - flags off
0cc9 : ea              >            nop
0cca : ea              >            nop
                       >            tst_a $ff-$62,0
0ccb : 08              >            php         ;save flags
0ccc : c99d            >            cmp #$ff-$62     ;test result
                       >            trap_ne
0cce : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0cd0 : 68              >            pla         ;load status
0cd1 : 48              >            pha
                       >            cmp_flag 0
0cd2 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0cd4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0cd6 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$62,$ff
                       >            load_flag $ff
0cd7 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0cd9 : 48              >            pha         ;use stack to load status
0cda : a948            >            lda #$aa-$62     ;precharge accu
0cdc : 28              >            plp
                       >
0cdd : 62              >            db  $62          ;test nop integrity - flags on
0cde : ea              >            nop
0cdf : ea              >            nop
                       >            tst_a $aa-$62,$ff
0ce0 : 08              >            php         ;save flags
0ce1 : c948            >            cmp #$aa-$62     ;test result
                       >            trap_ne
0ce3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ce5 : 68              >            pla         ;load status
0ce6 : 48              >            pha
                       >            cmp_flag $ff
0ce7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0ce9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ceb : 28              >            plp         ;restore status
                       >
0cec : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0cee : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0cf0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0cf2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $82,2
0cf4 : a042            >            ldy #$42
0cf6 : a202            >            ldx #4-2
0cf8 : 82              >            db  $82          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0cf9 : c8              >            iny
0cfa : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0cfb : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0cfc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$82,0
                       >            load_flag 0
0cfe : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0d00 : 48              >            pha         ;use stack to load status
0d01 : a97d            >            lda #$ff-$82     ;precharge accu
0d03 : 28              >            plp
                       >
0d04 : 82              >            db  $82          ;test nop integrity - flags off
0d05 : ea              >            nop
0d06 : ea              >            nop
                       >            tst_a $ff-$82,0
0d07 : 08              >            php         ;save flags
0d08 : c97d            >            cmp #$ff-$82     ;test result
                       >            trap_ne
0d0a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d0c : 68              >            pla         ;load status
0d0d : 48              >            pha
                       >            cmp_flag 0
0d0e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d10 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d12 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$82,$ff
                       >            load_flag $ff
0d13 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0d15 : 48              >            pha         ;use stack to load status
0d16 : a928            >            lda #$aa-$82     ;precharge accu
0d18 : 28              >            plp
                       >
0d19 : 82              >            db  $82          ;test nop integrity - flags on
0d1a : ea              >            nop
0d1b : ea              >            nop
                       >            tst_a $aa-$82,$ff
0d1c : 08              >            php         ;save flags
0d1d : c928            >            cmp #$aa-$82     ;test result
                       >            trap_ne
0d1f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d21 : 68              >            pla         ;load status
0d22 : 48              >            pha
                       >            cmp_flag $ff
0d23 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d25 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d27 : 28              >            plp         ;restore status
                       >
0d28 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0d2a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d2c : e000            >            cpx #0
                       >            trap_ne         ;x changed
0d2e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $c2,2
0d30 : a042            >            ldy #$42
0d32 : a202            >            ldx #4-2
0d34 : c2              >            db  $c2          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0d35 : c8              >            iny
0d36 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0d37 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0d38 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$c2,0
                       >            load_flag 0
0d3a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0d3c : 48              >            pha         ;use stack to load status
0d3d : a93d            >            lda #$ff-$c2     ;precharge accu
0d3f : 28              >            plp
                       >
0d40 : c2              >            db  $c2          ;test nop integrity - flags off
0d41 : ea              >            nop
0d42 : ea              >            nop
                       >            tst_a $ff-$c2,0
0d43 : 08              >            php         ;save flags
0d44 : c93d            >            cmp #$ff-$c2     ;test result
                       >            trap_ne
0d46 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d48 : 68              >            pla         ;load status
0d49 : 48              >            pha
                       >            cmp_flag 0
0d4a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d4c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d4e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$c2,$ff
                       >            load_flag $ff
0d4f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0d51 : 48              >            pha         ;use stack to load status
0d52 : a9e8            >            lda #$aa-$c2     ;precharge accu
0d54 : 28              >            plp
                       >
0d55 : c2              >            db  $c2          ;test nop integrity - flags on
0d56 : ea              >            nop
0d57 : ea              >            nop
                       >            tst_a $aa-$c2,$ff
0d58 : 08              >            php         ;save flags
0d59 : c9e8            >            cmp #$aa-$c2     ;test result
                       >            trap_ne
0d5b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d5d : 68              >            pla         ;load status
0d5e : 48              >            pha
                       >            cmp_flag $ff
0d5f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d61 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d63 : 28              >            plp         ;restore status
                       >
0d64 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0d66 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d68 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0d6a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $e2,2
0d6c : a042            >            ldy #$42
0d6e : a202            >            ldx #4-2
0d70 : e2              >            db  $e2          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0d71 : c8              >            iny
0d72 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0d73 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0d74 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$e2,0
                       >            load_flag 0
0d76 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0d78 : 48              >            pha         ;use stack to load status
0d79 : a91d            >            lda #$ff-$e2     ;precharge accu
0d7b : 28              >            plp
                       >
0d7c : e2              >            db  $e2          ;test nop integrity - flags off
0d7d : ea              >            nop
0d7e : ea              >            nop
                       >            tst_a $ff-$e2,0
0d7f : 08              >            php         ;save flags
0d80 : c91d            >            cmp #$ff-$e2     ;test result
                       >            trap_ne
0d82 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d84 : 68              >            pla         ;load status
0d85 : 48              >            pha
                       >            cmp_flag 0
0d86 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d88 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d8a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$e2,$ff
                       >            load_flag $ff
0d8b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0d8d : 48              >            pha         ;use stack to load status
0d8e : a9c8            >            lda #$aa-$e2     ;precharge accu
0d90 : 28              >            plp
                       >
0d91 : e2              >            db  $e2          ;test nop integrity - flags on
0d92 : ea              >            nop
0d93 : ea              >            nop
                       >            tst_a $aa-$e2,$ff
0d94 : 08              >            php         ;save flags
0d95 : c9c8            >            cmp #$aa-$e2     ;test result
                       >            trap_ne
0d97 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d99 : 68              >            pla         ;load status
0d9a : 48              >            pha
                       >            cmp_flag $ff
0d9b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0d9d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0d9f : 28              >            plp         ;restore status
                       >
0da0 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0da2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0da4 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0da6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $44,2
0da8 : a042            >            ldy #$42
0daa : a202            >            ldx #4-2
0dac : 44              >            db  $44          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0dad : c8              >            iny
0dae : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0daf : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0db0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$44,0
                       >            load_flag 0
0db2 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0db4 : 48              >            pha         ;use stack to load status
0db5 : a9bb            >            lda #$ff-$44     ;precharge accu
0db7 : 28              >            plp
                       >
0db8 : 44              >            db  $44          ;test nop integrity - flags off
0db9 : ea              >            nop
0dba : ea              >            nop
                       >            tst_a $ff-$44,0
0dbb : 08              >            php         ;save flags
0dbc : c9bb            >            cmp #$ff-$44     ;test result
                       >            trap_ne
0dbe : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0dc0 : 68              >            pla         ;load status
0dc1 : 48              >            pha
                       >            cmp_flag 0
0dc2 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0dc4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0dc6 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$44,$ff
                       >            load_flag $ff
0dc7 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0dc9 : 48              >            pha         ;use stack to load status
0dca : a966            >            lda #$aa-$44     ;precharge accu
0dcc : 28              >            plp
                       >
0dcd : 44              >            db  $44          ;test nop integrity - flags on
0dce : ea              >            nop
0dcf : ea              >            nop
                       >            tst_a $aa-$44,$ff
0dd0 : 08              >            php         ;save flags
0dd1 : c966            >            cmp #$aa-$44     ;test result
                       >            trap_ne
0dd3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0dd5 : 68              >            pla         ;load status
0dd6 : 48              >            pha
                       >            cmp_flag $ff
0dd7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0dd9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ddb : 28              >            plp         ;restore status
                       >
0ddc : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0dde : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0de0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0de2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $54,2
0de4 : a042            >            ldy #$42
0de6 : a202            >            ldx #4-2
0de8 : 54              >            db  $54          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0de9 : c8              >            iny
0dea : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0deb : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0dec : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$54,0
                       >            load_flag 0
0dee : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0df0 : 48              >            pha         ;use stack to load status
0df1 : a9ab            >            lda #$ff-$54     ;precharge accu
0df3 : 28              >            plp
                       >
0df4 : 54              >            db  $54          ;test nop integrity - flags off
0df5 : ea              >            nop
0df6 : ea              >            nop
                       >            tst_a $ff-$54,0
0df7 : 08              >            php         ;save flags
0df8 : c9ab            >            cmp #$ff-$54     ;test result
                       >            trap_ne
0dfa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0dfc : 68              >            pla         ;load status
0dfd : 48              >            pha
                       >            cmp_flag 0
0dfe : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e00 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e02 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$54,$ff
                       >            load_flag $ff
0e03 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0e05 : 48              >            pha         ;use stack to load status
0e06 : a956            >            lda #$aa-$54     ;precharge accu
0e08 : 28              >            plp
                       >
0e09 : 54              >            db  $54          ;test nop integrity - flags on
0e0a : ea              >            nop
0e0b : ea              >            nop
                       >            tst_a $aa-$54,$ff
0e0c : 08              >            php         ;save flags
0e0d : c956            >            cmp #$aa-$54     ;test result
                       >            trap_ne
0e0f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e11 : 68              >            pla         ;load status
0e12 : 48              >            pha
                       >            cmp_flag $ff
0e13 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e15 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e17 : 28              >            plp         ;restore status
                       >
0e18 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0e1a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e1c : e000            >            cpx #0
                       >            trap_ne         ;x changed
0e1e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $d4,2
0e20 : a042            >            ldy #$42
0e22 : a202            >            ldx #4-2
0e24 : d4              >            db  $d4          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0e25 : c8              >            iny
0e26 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0e27 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0e28 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$d4,0
                       >            load_flag 0
0e2a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0e2c : 48              >            pha         ;use stack to load status
0e2d : a92b            >            lda #$ff-$d4     ;precharge accu
0e2f : 28              >            plp
                       >
0e30 : d4              >            db  $d4          ;test nop integrity - flags off
0e31 : ea              >            nop
0e32 : ea              >            nop
                       >            tst_a $ff-$d4,0
0e33 : 08              >            php         ;save flags
0e34 : c92b            >            cmp #$ff-$d4     ;test result
                       >            trap_ne
0e36 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e38 : 68              >            pla         ;load status
0e39 : 48              >            pha
                       >            cmp_flag 0
0e3a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e3c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e3e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$d4,$ff
                       >            load_flag $ff
0e3f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0e41 : 48              >            pha         ;use stack to load status
0e42 : a9d6            >            lda #$aa-$d4     ;precharge accu
0e44 : 28              >            plp
                       >
0e45 : d4              >            db  $d4          ;test nop integrity - flags on
0e46 : ea              >            nop
0e47 : ea              >            nop
                       >            tst_a $aa-$d4,$ff
0e48 : 08              >            php         ;save flags
0e49 : c9d6            >            cmp #$aa-$d4     ;test result
                       >            trap_ne
0e4b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e4d : 68              >            pla         ;load status
0e4e : 48              >            pha
                       >            cmp_flag $ff
0e4f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e51 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e53 : 28              >            plp         ;restore status
                       >
0e54 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0e56 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e58 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0e5a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $f4,2
0e5c : a042            >            ldy #$42
0e5e : a202            >            ldx #4-2
0e60 : f4              >            db  $f4          ;test nop length
                       >        if 2 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 2 = 2
0e61 : c8              >            iny
0e62 : ca              >            dex
                       >        endif
                       >        if 2 = 3
                       >            iny
                       >            iny
                       >        endif
0e63 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0e64 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$f4,0
                       >            load_flag 0
0e66 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0e68 : 48              >            pha         ;use stack to load status
0e69 : a90b            >            lda #$ff-$f4     ;precharge accu
0e6b : 28              >            plp
                       >
0e6c : f4              >            db  $f4          ;test nop integrity - flags off
0e6d : ea              >            nop
0e6e : ea              >            nop
                       >            tst_a $ff-$f4,0
0e6f : 08              >            php         ;save flags
0e70 : c90b            >            cmp #$ff-$f4     ;test result
                       >            trap_ne
0e72 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e74 : 68              >            pla         ;load status
0e75 : 48              >            pha
                       >            cmp_flag 0
0e76 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e78 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e7a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$f4,$ff
                       >            load_flag $ff
0e7b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0e7d : 48              >            pha         ;use stack to load status
0e7e : a9b6            >            lda #$aa-$f4     ;precharge accu
0e80 : 28              >            plp
                       >
0e81 : f4              >            db  $f4          ;test nop integrity - flags on
0e82 : ea              >            nop
0e83 : ea              >            nop
                       >            tst_a $aa-$f4,$ff
0e84 : 08              >            php         ;save flags
0e85 : c9b6            >            cmp #$aa-$f4     ;test result
                       >            trap_ne
0e87 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e89 : 68              >            pla         ;load status
0e8a : 48              >            pha
                       >            cmp_flag $ff
0e8b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0e8d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e8f : 28              >            plp         ;restore status
                       >
0e90 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0e92 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0e94 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0e96 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $5c,3
0e98 : a042            >            ldy #$42
0e9a : a201            >            ldx #4-3
0e9c : 5c              >            db  $5c          ;test nop length
                       >        if 3 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 3 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 3 = 3
0e9d : c8              >            iny
0e9e : c8              >            iny
                       >        endif
0e9f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0ea0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$5c,0
                       >            load_flag 0
0ea2 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0ea4 : 48              >            pha         ;use stack to load status
0ea5 : a9a3            >            lda #$ff-$5c     ;precharge accu
0ea7 : 28              >            plp
                       >
0ea8 : 5c              >            db  $5c          ;test nop integrity - flags off
0ea9 : ea              >            nop
0eaa : ea              >            nop
                       >            tst_a $ff-$5c,0
0eab : 08              >            php         ;save flags
0eac : c9a3            >            cmp #$ff-$5c     ;test result
                       >            trap_ne
0eae : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0eb0 : 68              >            pla         ;load status
0eb1 : 48              >            pha
                       >            cmp_flag 0
0eb2 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0eb4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0eb6 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$5c,$ff
                       >            load_flag $ff
0eb7 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0eb9 : 48              >            pha         ;use stack to load status
0eba : a94e            >            lda #$aa-$5c     ;precharge accu
0ebc : 28              >            plp
                       >
0ebd : 5c              >            db  $5c          ;test nop integrity - flags on
0ebe : ea              >            nop
0ebf : ea              >            nop
                       >            tst_a $aa-$5c,$ff
0ec0 : 08              >            php         ;save flags
0ec1 : c94e            >            cmp #$aa-$5c     ;test result
                       >            trap_ne
0ec3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ec5 : 68              >            pla         ;load status
0ec6 : 48              >            pha
                       >            cmp_flag $ff
0ec7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0ec9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ecb : 28              >            plp         ;restore status
                       >
0ecc : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0ece : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ed0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0ed2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $dc,3
0ed4 : a042            >            ldy #$42
0ed6 : a201            >            ldx #4-3
0ed8 : dc              >            db  $dc          ;test nop length
                       >        if 3 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 3 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 3 = 3
0ed9 : c8              >            iny
0eda : c8              >            iny
                       >        endif
0edb : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0edc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$dc,0
                       >            load_flag 0
0ede : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0ee0 : 48              >            pha         ;use stack to load status
0ee1 : a923            >            lda #$ff-$dc     ;precharge accu
0ee3 : 28              >            plp
                       >
0ee4 : dc              >            db  $dc          ;test nop integrity - flags off
0ee5 : ea              >            nop
0ee6 : ea              >            nop
                       >            tst_a $ff-$dc,0
0ee7 : 08              >            php         ;save flags
0ee8 : c923            >            cmp #$ff-$dc     ;test result
                       >            trap_ne
0eea : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0eec : 68              >            pla         ;load status
0eed : 48              >            pha
                       >            cmp_flag 0
0eee : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0ef0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ef2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$dc,$ff
                       >            load_flag $ff
0ef3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0ef5 : 48              >            pha         ;use stack to load status
0ef6 : a9ce            >            lda #$aa-$dc     ;precharge accu
0ef8 : 28              >            plp
                       >
0ef9 : dc              >            db  $dc          ;test nop integrity - flags on
0efa : ea              >            nop
0efb : ea              >            nop
                       >            tst_a $aa-$dc,$ff
0efc : 08              >            php         ;save flags
0efd : c9ce            >            cmp #$aa-$dc     ;test result
                       >            trap_ne
0eff : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f01 : 68              >            pla         ;load status
0f02 : 48              >            pha
                       >            cmp_flag $ff
0f03 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0f05 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f07 : 28              >            plp         ;restore status
                       >
0f08 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0f0a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f0c : e000            >            cpx #0
                       >            trap_ne         ;x changed
0f0e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $fc,3
0f10 : a042            >            ldy #$42
0f12 : a201            >            ldx #4-3
0f14 : fc              >            db  $fc          ;test nop length
                       >        if 3 = 1
                       >            dex
                       >            dex
                       >        endif
                       >        if 3 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 3 = 3
0f15 : c8              >            iny
0f16 : c8              >            iny
                       >        endif
0f17 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0f18 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$fc,0
                       >            load_flag 0
0f1a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0f1c : 48              >            pha         ;use stack to load status
0f1d : a903            >            lda #$ff-$fc     ;precharge accu
0f1f : 28              >            plp
                       >
0f20 : fc              >            db  $fc          ;test nop integrity - flags off
0f21 : ea              >            nop
0f22 : ea              >            nop
                       >            tst_a $ff-$fc,0
0f23 : 08              >            php         ;save flags
0f24 : c903            >            cmp #$ff-$fc     ;test result
                       >            trap_ne
0f26 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f28 : 68              >            pla         ;load status
0f29 : 48              >            pha
                       >            cmp_flag 0
0f2a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0f2c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f2e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$fc,$ff
                       >            load_flag $ff
0f2f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0f31 : 48              >            pha         ;use stack to load status
0f32 : a9ae            >            lda #$aa-$fc     ;precharge accu
0f34 : 28              >            plp
                       >
0f35 : fc              >            db  $fc          ;test nop integrity - flags on
0f36 : ea              >            nop
0f37 : ea              >            nop
                       >            tst_a $aa-$fc,$ff
0f38 : 08              >            php         ;save flags
0f39 : c9ae            >            cmp #$aa-$fc     ;test result
                       >            trap_ne
0f3b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f3d : 68              >            pla         ;load status
0f3e : 48              >            pha
                       >            cmp_flag $ff
0f3f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0f41 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f43 : 28              >            plp         ;restore status
                       >
0f44 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0f46 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f48 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0f4a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $03,1
0f4c : a042            >            ldy #$42
0f4e : a203            >            ldx #4-1
0f50 : 03              >            db  $03          ;test nop length
                       >        if 1 = 1
0f51 : ca              >            dex
0f52 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
0f53 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0f54 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$03,0
                       >            load_flag 0
0f56 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0f58 : 48              >            pha         ;use stack to load status
0f59 : a9fc            >            lda #$ff-$03     ;precharge accu
0f5b : 28              >            plp
                       >
0f5c : 03              >            db  $03          ;test nop integrity - flags off
0f5d : ea              >            nop
0f5e : ea              >            nop
                       >            tst_a $ff-$03,0
0f5f : 08              >            php         ;save flags
0f60 : c9fc            >            cmp #$ff-$03     ;test result
                       >            trap_ne
0f62 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f64 : 68              >            pla         ;load status
0f65 : 48              >            pha
                       >            cmp_flag 0
0f66 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0f68 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f6a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$03,$ff
                       >            load_flag $ff
0f6b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0f6d : 48              >            pha         ;use stack to load status
0f6e : a9a7            >            lda #$aa-$03     ;precharge accu
0f70 : 28              >            plp
                       >
0f71 : 03              >            db  $03          ;test nop integrity - flags on
0f72 : ea              >            nop
0f73 : ea              >            nop
                       >            tst_a $aa-$03,$ff
0f74 : 08              >            php         ;save flags
0f75 : c9a7            >            cmp #$aa-$03     ;test result
                       >            trap_ne
0f77 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f79 : 68              >            pla         ;load status
0f7a : 48              >            pha
                       >            cmp_flag $ff
0f7b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0f7d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f7f : 28              >            plp         ;restore status
                       >
0f80 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0f82 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0f84 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0f86 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $13,1
0f88 : a042            >            ldy #$42
0f8a : a203            >            ldx #4-1
0f8c : 13              >            db  $13          ;test nop length
                       >        if 1 = 1
0f8d : ca              >            dex
0f8e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
0f8f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0f90 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$13,0
                       >            load_flag 0
0f92 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0f94 : 48              >            pha         ;use stack to load status
0f95 : a9ec            >            lda #$ff-$13     ;precharge accu
0f97 : 28              >            plp
                       >
0f98 : 13              >            db  $13          ;test nop integrity - flags off
0f99 : ea              >            nop
0f9a : ea              >            nop
                       >            tst_a $ff-$13,0
0f9b : 08              >            php         ;save flags
0f9c : c9ec            >            cmp #$ff-$13     ;test result
                       >            trap_ne
0f9e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fa0 : 68              >            pla         ;load status
0fa1 : 48              >            pha
                       >            cmp_flag 0
0fa2 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0fa4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fa6 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$13,$ff
                       >            load_flag $ff
0fa7 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0fa9 : 48              >            pha         ;use stack to load status
0faa : a997            >            lda #$aa-$13     ;precharge accu
0fac : 28              >            plp
                       >
0fad : 13              >            db  $13          ;test nop integrity - flags on
0fae : ea              >            nop
0faf : ea              >            nop
                       >            tst_a $aa-$13,$ff
0fb0 : 08              >            php         ;save flags
0fb1 : c997            >            cmp #$aa-$13     ;test result
                       >            trap_ne
0fb3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fb5 : 68              >            pla         ;load status
0fb6 : 48              >            pha
                       >            cmp_flag $ff
0fb7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0fb9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fbb : 28              >            plp         ;restore status
                       >
0fbc : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0fbe : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fc0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
0fc2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $23,1
0fc4 : a042            >            ldy #$42
0fc6 : a203            >            ldx #4-1
0fc8 : 23              >            db  $23          ;test nop length
                       >        if 1 = 1
0fc9 : ca              >            dex
0fca : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
0fcb : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
0fcc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$23,0
                       >            load_flag 0
0fce : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
0fd0 : 48              >            pha         ;use stack to load status
0fd1 : a9dc            >            lda #$ff-$23     ;precharge accu
0fd3 : 28              >            plp
                       >
0fd4 : 23              >            db  $23          ;test nop integrity - flags off
0fd5 : ea              >            nop
0fd6 : ea              >            nop
                       >            tst_a $ff-$23,0
0fd7 : 08              >            php         ;save flags
0fd8 : c9dc            >            cmp #$ff-$23     ;test result
                       >            trap_ne
0fda : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fdc : 68              >            pla         ;load status
0fdd : 48              >            pha
                       >            cmp_flag 0
0fde : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0fe0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0fe2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$23,$ff
                       >            load_flag $ff
0fe3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
0fe5 : 48              >            pha         ;use stack to load status
0fe6 : a987            >            lda #$aa-$23     ;precharge accu
0fe8 : 28              >            plp
                       >
0fe9 : 23              >            db  $23          ;test nop integrity - flags on
0fea : ea              >            nop
0feb : ea              >            nop
                       >            tst_a $aa-$23,$ff
0fec : 08              >            php         ;save flags
0fed : c987            >            cmp #$aa-$23     ;test result
                       >            trap_ne
0fef : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ff1 : 68              >            pla         ;load status
0ff2 : 48              >            pha
                       >            cmp_flag $ff
0ff3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
0ff5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ff7 : 28              >            plp         ;restore status
                       >
0ff8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
0ffa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0ffc : e000            >            cpx #0
                       >            trap_ne         ;x changed
0ffe : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $33,1
1000 : a042            >            ldy #$42
1002 : a203            >            ldx #4-1
1004 : 33              >            db  $33          ;test nop length
                       >        if 1 = 1
1005 : ca              >            dex
1006 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1007 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1008 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$33,0
                       >            load_flag 0
100a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
100c : 48              >            pha         ;use stack to load status
100d : a9cc            >            lda #$ff-$33     ;precharge accu
100f : 28              >            plp
                       >
1010 : 33              >            db  $33          ;test nop integrity - flags off
1011 : ea              >            nop
1012 : ea              >            nop
                       >            tst_a $ff-$33,0
1013 : 08              >            php         ;save flags
1014 : c9cc            >            cmp #$ff-$33     ;test result
                       >            trap_ne
1016 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1018 : 68              >            pla         ;load status
1019 : 48              >            pha
                       >            cmp_flag 0
101a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
101c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
101e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$33,$ff
                       >            load_flag $ff
101f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1021 : 48              >            pha         ;use stack to load status
1022 : a977            >            lda #$aa-$33     ;precharge accu
1024 : 28              >            plp
                       >
1025 : 33              >            db  $33          ;test nop integrity - flags on
1026 : ea              >            nop
1027 : ea              >            nop
                       >            tst_a $aa-$33,$ff
1028 : 08              >            php         ;save flags
1029 : c977            >            cmp #$aa-$33     ;test result
                       >            trap_ne
102b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
102d : 68              >            pla         ;load status
102e : 48              >            pha
                       >            cmp_flag $ff
102f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1031 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1033 : 28              >            plp         ;restore status
                       >
1034 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1036 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1038 : e000            >            cpx #0
                       >            trap_ne         ;x changed
103a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $43,1
103c : a042            >            ldy #$42
103e : a203            >            ldx #4-1
1040 : 43              >            db  $43          ;test nop length
                       >        if 1 = 1
1041 : ca              >            dex
1042 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1043 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1044 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$43,0
                       >            load_flag 0
1046 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1048 : 48              >            pha         ;use stack to load status
1049 : a9bc            >            lda #$ff-$43     ;precharge accu
104b : 28              >            plp
                       >
104c : 43              >            db  $43          ;test nop integrity - flags off
104d : ea              >            nop
104e : ea              >            nop
                       >            tst_a $ff-$43,0
104f : 08              >            php         ;save flags
1050 : c9bc            >            cmp #$ff-$43     ;test result
                       >            trap_ne
1052 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1054 : 68              >            pla         ;load status
1055 : 48              >            pha
                       >            cmp_flag 0
1056 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1058 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
105a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$43,$ff
                       >            load_flag $ff
105b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
105d : 48              >            pha         ;use stack to load status
105e : a967            >            lda #$aa-$43     ;precharge accu
1060 : 28              >            plp
                       >
1061 : 43              >            db  $43          ;test nop integrity - flags on
1062 : ea              >            nop
1063 : ea              >            nop
                       >            tst_a $aa-$43,$ff
1064 : 08              >            php         ;save flags
1065 : c967            >            cmp #$aa-$43     ;test result
                       >            trap_ne
1067 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1069 : 68              >            pla         ;load status
106a : 48              >            pha
                       >            cmp_flag $ff
106b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
106d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
106f : 28              >            plp         ;restore status
                       >
1070 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1072 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1074 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1076 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $53,1
1078 : a042            >            ldy #$42
107a : a203            >            ldx #4-1
107c : 53              >            db  $53          ;test nop length
                       >        if 1 = 1
107d : ca              >            dex
107e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
107f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1080 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$53,0
                       >            load_flag 0
1082 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1084 : 48              >            pha         ;use stack to load status
1085 : a9ac            >            lda #$ff-$53     ;precharge accu
1087 : 28              >            plp
                       >
1088 : 53              >            db  $53          ;test nop integrity - flags off
1089 : ea              >            nop
108a : ea              >            nop
                       >            tst_a $ff-$53,0
108b : 08              >            php         ;save flags
108c : c9ac            >            cmp #$ff-$53     ;test result
                       >            trap_ne
108e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1090 : 68              >            pla         ;load status
1091 : 48              >            pha
                       >            cmp_flag 0
1092 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1094 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1096 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$53,$ff
                       >            load_flag $ff
1097 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1099 : 48              >            pha         ;use stack to load status
109a : a957            >            lda #$aa-$53     ;precharge accu
109c : 28              >            plp
                       >
109d : 53              >            db  $53          ;test nop integrity - flags on
109e : ea              >            nop
109f : ea              >            nop
                       >            tst_a $aa-$53,$ff
10a0 : 08              >            php         ;save flags
10a1 : c957            >            cmp #$aa-$53     ;test result
                       >            trap_ne
10a3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10a5 : 68              >            pla         ;load status
10a6 : 48              >            pha
                       >            cmp_flag $ff
10a7 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
10a9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10ab : 28              >            plp         ;restore status
                       >
10ac : c042            >            cpy #$42
                       >            trap_ne         ;y changed
10ae : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10b0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
10b2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $63,1
10b4 : a042            >            ldy #$42
10b6 : a203            >            ldx #4-1
10b8 : 63              >            db  $63          ;test nop length
                       >        if 1 = 1
10b9 : ca              >            dex
10ba : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
10bb : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
10bc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$63,0
                       >            load_flag 0
10be : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
10c0 : 48              >            pha         ;use stack to load status
10c1 : a99c            >            lda #$ff-$63     ;precharge accu
10c3 : 28              >            plp
                       >
10c4 : 63              >            db  $63          ;test nop integrity - flags off
10c5 : ea              >            nop
10c6 : ea              >            nop
                       >            tst_a $ff-$63,0
10c7 : 08              >            php         ;save flags
10c8 : c99c            >            cmp #$ff-$63     ;test result
                       >            trap_ne
10ca : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10cc : 68              >            pla         ;load status
10cd : 48              >            pha
                       >            cmp_flag 0
10ce : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
10d0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10d2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$63,$ff
                       >            load_flag $ff
10d3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
10d5 : 48              >            pha         ;use stack to load status
10d6 : a947            >            lda #$aa-$63     ;precharge accu
10d8 : 28              >            plp
                       >
10d9 : 63              >            db  $63          ;test nop integrity - flags on
10da : ea              >            nop
10db : ea              >            nop
                       >            tst_a $aa-$63,$ff
10dc : 08              >            php         ;save flags
10dd : c947            >            cmp #$aa-$63     ;test result
                       >            trap_ne
10df : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10e1 : 68              >            pla         ;load status
10e2 : 48              >            pha
                       >            cmp_flag $ff
10e3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
10e5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10e7 : 28              >            plp         ;restore status
                       >
10e8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
10ea : d0fe            >        bne *           ;failed not equal (non zero)
                       >
10ec : e000            >            cpx #0
                       >            trap_ne         ;x changed
10ee : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $73,1
10f0 : a042            >            ldy #$42
10f2 : a203            >            ldx #4-1
10f4 : 73              >            db  $73          ;test nop length
                       >        if 1 = 1
10f5 : ca              >            dex
10f6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
10f7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
10f8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$73,0
                       >            load_flag 0
10fa : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
10fc : 48              >            pha         ;use stack to load status
10fd : a98c            >            lda #$ff-$73     ;precharge accu
10ff : 28              >            plp
                       >
1100 : 73              >            db  $73          ;test nop integrity - flags off
1101 : ea              >            nop
1102 : ea              >            nop
                       >            tst_a $ff-$73,0
1103 : 08              >            php         ;save flags
1104 : c98c            >            cmp #$ff-$73     ;test result
                       >            trap_ne
1106 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1108 : 68              >            pla         ;load status
1109 : 48              >            pha
                       >            cmp_flag 0
110a : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
110c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
110e : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$73,$ff
                       >            load_flag $ff
110f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1111 : 48              >            pha         ;use stack to load status
1112 : a937            >            lda #$aa-$73     ;precharge accu
1114 : 28              >            plp
                       >
1115 : 73              >            db  $73          ;test nop integrity - flags on
1116 : ea              >            nop
1117 : ea              >            nop
                       >            tst_a $aa-$73,$ff
1118 : 08              >            php         ;save flags
1119 : c937            >            cmp #$aa-$73     ;test result
                       >            trap_ne
111b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
111d : 68              >            pla         ;load status
111e : 48              >            pha
                       >            cmp_flag $ff
111f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1121 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1123 : 28              >            plp         ;restore status
                       >
1124 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1126 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1128 : e000            >            cpx #0
                       >            trap_ne         ;x changed
112a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $83,1
112c : a042            >            ldy #$42
112e : a203            >            ldx #4-1
1130 : 83              >            db  $83          ;test nop length
                       >        if 1 = 1
1131 : ca              >            dex
1132 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1133 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1134 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$83,0
                       >            load_flag 0
1136 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1138 : 48              >            pha         ;use stack to load status
1139 : a97c            >            lda #$ff-$83     ;precharge accu
113b : 28              >            plp
                       >
113c : 83              >            db  $83          ;test nop integrity - flags off
113d : ea              >            nop
113e : ea              >            nop
                       >            tst_a $ff-$83,0
113f : 08              >            php         ;save flags
1140 : c97c            >            cmp #$ff-$83     ;test result
                       >            trap_ne
1142 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1144 : 68              >            pla         ;load status
1145 : 48              >            pha
                       >            cmp_flag 0
1146 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1148 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
114a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$83,$ff
                       >            load_flag $ff
114b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
114d : 48              >            pha         ;use stack to load status
114e : a927            >            lda #$aa-$83     ;precharge accu
1150 : 28              >            plp
                       >
1151 : 83              >            db  $83          ;test nop integrity - flags on
1152 : ea              >            nop
1153 : ea              >            nop
                       >            tst_a $aa-$83,$ff
1154 : 08              >            php         ;save flags
1155 : c927            >            cmp #$aa-$83     ;test result
                       >            trap_ne
1157 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1159 : 68              >            pla         ;load status
115a : 48              >            pha
                       >            cmp_flag $ff
115b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
115d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
115f : 28              >            plp         ;restore status
                       >
1160 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1162 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1164 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1166 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $93,1
1168 : a042            >            ldy #$42
116a : a203            >            ldx #4-1
116c : 93              >            db  $93          ;test nop length
                       >        if 1 = 1
116d : ca              >            dex
116e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
116f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1170 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$93,0
                       >            load_flag 0
1172 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1174 : 48              >            pha         ;use stack to load status
1175 : a96c            >            lda #$ff-$93     ;precharge accu
1177 : 28              >            plp
                       >
1178 : 93              >            db  $93          ;test nop integrity - flags off
1179 : ea              >            nop
117a : ea              >            nop
                       >            tst_a $ff-$93,0
117b : 08              >            php         ;save flags
117c : c96c            >            cmp #$ff-$93     ;test result
                       >            trap_ne
117e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1180 : 68              >            pla         ;load status
1181 : 48              >            pha
                       >            cmp_flag 0
1182 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1184 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1186 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$93,$ff
                       >            load_flag $ff
1187 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1189 : 48              >            pha         ;use stack to load status
118a : a917            >            lda #$aa-$93     ;precharge accu
118c : 28              >            plp
                       >
118d : 93              >            db  $93          ;test nop integrity - flags on
118e : ea              >            nop
118f : ea              >            nop
                       >            tst_a $aa-$93,$ff
1190 : 08              >            php         ;save flags
1191 : c917            >            cmp #$aa-$93     ;test result
                       >            trap_ne
1193 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1195 : 68              >            pla         ;load status
1196 : 48              >            pha
                       >            cmp_flag $ff
1197 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1199 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
119b : 28              >            plp         ;restore status
                       >
119c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
119e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11a0 : e000            >            cpx #0
                       >            trap_ne         ;x changed
11a2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $a3,1
11a4 : a042            >            ldy #$42
11a6 : a203            >            ldx #4-1
11a8 : a3              >            db  $a3          ;test nop length
                       >        if 1 = 1
11a9 : ca              >            dex
11aa : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
11ab : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
11ac : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$a3,0
                       >            load_flag 0
11ae : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
11b0 : 48              >            pha         ;use stack to load status
11b1 : a95c            >            lda #$ff-$a3     ;precharge accu
11b3 : 28              >            plp
                       >
11b4 : a3              >            db  $a3          ;test nop integrity - flags off
11b5 : ea              >            nop
11b6 : ea              >            nop
                       >            tst_a $ff-$a3,0
11b7 : 08              >            php         ;save flags
11b8 : c95c            >            cmp #$ff-$a3     ;test result
                       >            trap_ne
11ba : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11bc : 68              >            pla         ;load status
11bd : 48              >            pha
                       >            cmp_flag 0
11be : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
11c0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11c2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$a3,$ff
                       >            load_flag $ff
11c3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
11c5 : 48              >            pha         ;use stack to load status
11c6 : a907            >            lda #$aa-$a3     ;precharge accu
11c8 : 28              >            plp
                       >
11c9 : a3              >            db  $a3          ;test nop integrity - flags on
11ca : ea              >            nop
11cb : ea              >            nop
                       >            tst_a $aa-$a3,$ff
11cc : 08              >            php         ;save flags
11cd : c907            >            cmp #$aa-$a3     ;test result
                       >            trap_ne
11cf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11d1 : 68              >            pla         ;load status
11d2 : 48              >            pha
                       >            cmp_flag $ff
11d3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
11d5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11d7 : 28              >            plp         ;restore status
                       >
11d8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
11da : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11dc : e000            >            cpx #0
                       >            trap_ne         ;x changed
11de : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $b3,1
11e0 : a042            >            ldy #$42
11e2 : a203            >            ldx #4-1
11e4 : b3              >            db  $b3          ;test nop length
                       >        if 1 = 1
11e5 : ca              >            dex
11e6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
11e7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
11e8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$b3,0
                       >            load_flag 0
11ea : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
11ec : 48              >            pha         ;use stack to load status
11ed : a94c            >            lda #$ff-$b3     ;precharge accu
11ef : 28              >            plp
                       >
11f0 : b3              >            db  $b3          ;test nop integrity - flags off
11f1 : ea              >            nop
11f2 : ea              >            nop
                       >            tst_a $ff-$b3,0
11f3 : 08              >            php         ;save flags
11f4 : c94c            >            cmp #$ff-$b3     ;test result
                       >            trap_ne
11f6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11f8 : 68              >            pla         ;load status
11f9 : 48              >            pha
                       >            cmp_flag 0
11fa : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
11fc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
11fe : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$b3,$ff
                       >            load_flag $ff
11ff : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1201 : 48              >            pha         ;use stack to load status
1202 : a9f7            >            lda #$aa-$b3     ;precharge accu
1204 : 28              >            plp
                       >
1205 : b3              >            db  $b3          ;test nop integrity - flags on
1206 : ea              >            nop
1207 : ea              >            nop
                       >            tst_a $aa-$b3,$ff
1208 : 08              >            php         ;save flags
1209 : c9f7            >            cmp #$aa-$b3     ;test result
                       >            trap_ne
120b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
120d : 68              >            pla         ;load status
120e : 48              >            pha
                       >            cmp_flag $ff
120f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1211 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1213 : 28              >            plp         ;restore status
                       >
1214 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1216 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1218 : e000            >            cpx #0
                       >            trap_ne         ;x changed
121a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $c3,1
121c : a042            >            ldy #$42
121e : a203            >            ldx #4-1
1220 : c3              >            db  $c3          ;test nop length
                       >        if 1 = 1
1221 : ca              >            dex
1222 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1223 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1224 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$c3,0
                       >            load_flag 0
1226 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1228 : 48              >            pha         ;use stack to load status
1229 : a93c            >            lda #$ff-$c3     ;precharge accu
122b : 28              >            plp
                       >
122c : c3              >            db  $c3          ;test nop integrity - flags off
122d : ea              >            nop
122e : ea              >            nop
                       >            tst_a $ff-$c3,0
122f : 08              >            php         ;save flags
1230 : c93c            >            cmp #$ff-$c3     ;test result
                       >            trap_ne
1232 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1234 : 68              >            pla         ;load status
1235 : 48              >            pha
                       >            cmp_flag 0
1236 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1238 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
123a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$c3,$ff
                       >            load_flag $ff
123b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
123d : 48              >            pha         ;use stack to load status
123e : a9e7            >            lda #$aa-$c3     ;precharge accu
1240 : 28              >            plp
                       >
1241 : c3              >            db  $c3          ;test nop integrity - flags on
1242 : ea              >            nop
1243 : ea              >            nop
                       >            tst_a $aa-$c3,$ff
1244 : 08              >            php         ;save flags
1245 : c9e7            >            cmp #$aa-$c3     ;test result
                       >            trap_ne
1247 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1249 : 68              >            pla         ;load status
124a : 48              >            pha
                       >            cmp_flag $ff
124b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
124d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
124f : 28              >            plp         ;restore status
                       >
1250 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1252 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1254 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1256 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $d3,1
1258 : a042            >            ldy #$42
125a : a203            >            ldx #4-1
125c : d3              >            db  $d3          ;test nop length
                       >        if 1 = 1
125d : ca              >            dex
125e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
125f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1260 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$d3,0
                       >            load_flag 0
1262 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1264 : 48              >            pha         ;use stack to load status
1265 : a92c            >            lda #$ff-$d3     ;precharge accu
1267 : 28              >            plp
                       >
1268 : d3              >            db  $d3          ;test nop integrity - flags off
1269 : ea              >            nop
126a : ea              >            nop
                       >            tst_a $ff-$d3,0
126b : 08              >            php         ;save flags
126c : c92c            >            cmp #$ff-$d3     ;test result
                       >            trap_ne
126e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1270 : 68              >            pla         ;load status
1271 : 48              >            pha
                       >            cmp_flag 0
1272 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1274 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1276 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$d3,$ff
                       >            load_flag $ff
1277 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1279 : 48              >            pha         ;use stack to load status
127a : a9d7            >            lda #$aa-$d3     ;precharge accu
127c : 28              >            plp
                       >
127d : d3              >            db  $d3          ;test nop integrity - flags on
127e : ea              >            nop
127f : ea              >            nop
                       >            tst_a $aa-$d3,$ff
1280 : 08              >            php         ;save flags
1281 : c9d7            >            cmp #$aa-$d3     ;test result
                       >            trap_ne
1283 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1285 : 68              >            pla         ;load status
1286 : 48              >            pha
                       >            cmp_flag $ff
1287 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1289 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
128b : 28              >            plp         ;restore status
                       >
128c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
128e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1290 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1292 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $e3,1
1294 : a042            >            ldy #$42
1296 : a203            >            ldx #4-1
1298 : e3              >            db  $e3          ;test nop length
                       >        if 1 = 1
1299 : ca              >            dex
129a : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
129b : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
129c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$e3,0
                       >            load_flag 0
129e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
12a0 : 48              >            pha         ;use stack to load status
12a1 : a91c            >            lda #$ff-$e3     ;precharge accu
12a3 : 28              >            plp
                       >
12a4 : e3              >            db  $e3          ;test nop integrity - flags off
12a5 : ea              >            nop
12a6 : ea              >            nop
                       >            tst_a $ff-$e3,0
12a7 : 08              >            php         ;save flags
12a8 : c91c            >            cmp #$ff-$e3     ;test result
                       >            trap_ne
12aa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12ac : 68              >            pla         ;load status
12ad : 48              >            pha
                       >            cmp_flag 0
12ae : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
12b0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12b2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$e3,$ff
                       >            load_flag $ff
12b3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
12b5 : 48              >            pha         ;use stack to load status
12b6 : a9c7            >            lda #$aa-$e3     ;precharge accu
12b8 : 28              >            plp
                       >
12b9 : e3              >            db  $e3          ;test nop integrity - flags on
12ba : ea              >            nop
12bb : ea              >            nop
                       >            tst_a $aa-$e3,$ff
12bc : 08              >            php         ;save flags
12bd : c9c7            >            cmp #$aa-$e3     ;test result
                       >            trap_ne
12bf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12c1 : 68              >            pla         ;load status
12c2 : 48              >            pha
                       >            cmp_flag $ff
12c3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
12c5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12c7 : 28              >            plp         ;restore status
                       >
12c8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
12ca : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12cc : e000            >            cpx #0
                       >            trap_ne         ;x changed
12ce : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $f3,1
12d0 : a042            >            ldy #$42
12d2 : a203            >            ldx #4-1
12d4 : f3              >            db  $f3          ;test nop length
                       >        if 1 = 1
12d5 : ca              >            dex
12d6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
12d7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
12d8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$f3,0
                       >            load_flag 0
12da : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
12dc : 48              >            pha         ;use stack to load status
12dd : a90c            >            lda #$ff-$f3     ;precharge accu
12df : 28              >            plp
                       >
12e0 : f3              >            db  $f3          ;test nop integrity - flags off
12e1 : ea              >            nop
12e2 : ea              >            nop
                       >            tst_a $ff-$f3,0
12e3 : 08              >            php         ;save flags
12e4 : c90c            >            cmp #$ff-$f3     ;test result
                       >            trap_ne
12e6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12e8 : 68              >            pla         ;load status
12e9 : 48              >            pha
                       >            cmp_flag 0
12ea : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
12ec : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12ee : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$f3,$ff
                       >            load_flag $ff
12ef : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
12f1 : 48              >            pha         ;use stack to load status
12f2 : a9b7            >            lda #$aa-$f3     ;precharge accu
12f4 : 28              >            plp
                       >
12f5 : f3              >            db  $f3          ;test nop integrity - flags on
12f6 : ea              >            nop
12f7 : ea              >            nop
                       >            tst_a $aa-$f3,$ff
12f8 : 08              >            php         ;save flags
12f9 : c9b7            >            cmp #$aa-$f3     ;test result
                       >            trap_ne
12fb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
12fd : 68              >            pla         ;load status
12fe : 48              >            pha
                       >            cmp_flag $ff
12ff : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1301 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1303 : 28              >            plp         ;restore status
                       >
1304 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1306 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1308 : e000            >            cpx #0
                       >            trap_ne         ;x changed
130a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $0b,1
130c : a042            >            ldy #$42
130e : a203            >            ldx #4-1
1310 : 0b              >            db  $0b          ;test nop length
                       >        if 1 = 1
1311 : ca              >            dex
1312 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1313 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1314 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$0b,0
                       >            load_flag 0
1316 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1318 : 48              >            pha         ;use stack to load status
1319 : a9f4            >            lda #$ff-$0b     ;precharge accu
131b : 28              >            plp
                       >
131c : 0b              >            db  $0b          ;test nop integrity - flags off
131d : ea              >            nop
131e : ea              >            nop
                       >            tst_a $ff-$0b,0
131f : 08              >            php         ;save flags
1320 : c9f4            >            cmp #$ff-$0b     ;test result
                       >            trap_ne
1322 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1324 : 68              >            pla         ;load status
1325 : 48              >            pha
                       >            cmp_flag 0
1326 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1328 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
132a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$0b,$ff
                       >            load_flag $ff
132b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
132d : 48              >            pha         ;use stack to load status
132e : a99f            >            lda #$aa-$0b     ;precharge accu
1330 : 28              >            plp
                       >
1331 : 0b              >            db  $0b          ;test nop integrity - flags on
1332 : ea              >            nop
1333 : ea              >            nop
                       >            tst_a $aa-$0b,$ff
1334 : 08              >            php         ;save flags
1335 : c99f            >            cmp #$aa-$0b     ;test result
                       >            trap_ne
1337 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1339 : 68              >            pla         ;load status
133a : 48              >            pha
                       >            cmp_flag $ff
133b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
133d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
133f : 28              >            plp         ;restore status
                       >
1340 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1342 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1344 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1346 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $1b,1
1348 : a042            >            ldy #$42
134a : a203            >            ldx #4-1
134c : 1b              >            db  $1b          ;test nop length
                       >        if 1 = 1
134d : ca              >            dex
134e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
134f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1350 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$1b,0
                       >            load_flag 0
1352 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1354 : 48              >            pha         ;use stack to load status
1355 : a9e4            >            lda #$ff-$1b     ;precharge accu
1357 : 28              >            plp
                       >
1358 : 1b              >            db  $1b          ;test nop integrity - flags off
1359 : ea              >            nop
135a : ea              >            nop
                       >            tst_a $ff-$1b,0
135b : 08              >            php         ;save flags
135c : c9e4            >            cmp #$ff-$1b     ;test result
                       >            trap_ne
135e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1360 : 68              >            pla         ;load status
1361 : 48              >            pha
                       >            cmp_flag 0
1362 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1364 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1366 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$1b,$ff
                       >            load_flag $ff
1367 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1369 : 48              >            pha         ;use stack to load status
136a : a98f            >            lda #$aa-$1b     ;precharge accu
136c : 28              >            plp
                       >
136d : 1b              >            db  $1b          ;test nop integrity - flags on
136e : ea              >            nop
136f : ea              >            nop
                       >            tst_a $aa-$1b,$ff
1370 : 08              >            php         ;save flags
1371 : c98f            >            cmp #$aa-$1b     ;test result
                       >            trap_ne
1373 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1375 : 68              >            pla         ;load status
1376 : 48              >            pha
                       >            cmp_flag $ff
1377 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1379 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
137b : 28              >            plp         ;restore status
                       >
137c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
137e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1380 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1382 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $2b,1
1384 : a042            >            ldy #$42
1386 : a203            >            ldx #4-1
1388 : 2b              >            db  $2b          ;test nop length
                       >        if 1 = 1
1389 : ca              >            dex
138a : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
138b : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
138c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$2b,0
                       >            load_flag 0
138e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1390 : 48              >            pha         ;use stack to load status
1391 : a9d4            >            lda #$ff-$2b     ;precharge accu
1393 : 28              >            plp
                       >
1394 : 2b              >            db  $2b          ;test nop integrity - flags off
1395 : ea              >            nop
1396 : ea              >            nop
                       >            tst_a $ff-$2b,0
1397 : 08              >            php         ;save flags
1398 : c9d4            >            cmp #$ff-$2b     ;test result
                       >            trap_ne
139a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
139c : 68              >            pla         ;load status
139d : 48              >            pha
                       >            cmp_flag 0
139e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
13a0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13a2 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$2b,$ff
                       >            load_flag $ff
13a3 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
13a5 : 48              >            pha         ;use stack to load status
13a6 : a97f            >            lda #$aa-$2b     ;precharge accu
13a8 : 28              >            plp
                       >
13a9 : 2b              >            db  $2b          ;test nop integrity - flags on
13aa : ea              >            nop
13ab : ea              >            nop
                       >            tst_a $aa-$2b,$ff
13ac : 08              >            php         ;save flags
13ad : c97f            >            cmp #$aa-$2b     ;test result
                       >            trap_ne
13af : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13b1 : 68              >            pla         ;load status
13b2 : 48              >            pha
                       >            cmp_flag $ff
13b3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
13b5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13b7 : 28              >            plp         ;restore status
                       >
13b8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
13ba : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13bc : e000            >            cpx #0
                       >            trap_ne         ;x changed
13be : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $3b,1
13c0 : a042            >            ldy #$42
13c2 : a203            >            ldx #4-1
13c4 : 3b              >            db  $3b          ;test nop length
                       >        if 1 = 1
13c5 : ca              >            dex
13c6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
13c7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
13c8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$3b,0
                       >            load_flag 0
13ca : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
13cc : 48              >            pha         ;use stack to load status
13cd : a9c4            >            lda #$ff-$3b     ;precharge accu
13cf : 28              >            plp
                       >
13d0 : 3b              >            db  $3b          ;test nop integrity - flags off
13d1 : ea              >            nop
13d2 : ea              >            nop
                       >            tst_a $ff-$3b,0
13d3 : 08              >            php         ;save flags
13d4 : c9c4            >            cmp #$ff-$3b     ;test result
                       >            trap_ne
13d6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13d8 : 68              >            pla         ;load status
13d9 : 48              >            pha
                       >            cmp_flag 0
13da : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
13dc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13de : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$3b,$ff
                       >            load_flag $ff
13df : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
13e1 : 48              >            pha         ;use stack to load status
13e2 : a96f            >            lda #$aa-$3b     ;precharge accu
13e4 : 28              >            plp
                       >
13e5 : 3b              >            db  $3b          ;test nop integrity - flags on
13e6 : ea              >            nop
13e7 : ea              >            nop
                       >            tst_a $aa-$3b,$ff
13e8 : 08              >            php         ;save flags
13e9 : c96f            >            cmp #$aa-$3b     ;test result
                       >            trap_ne
13eb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13ed : 68              >            pla         ;load status
13ee : 48              >            pha
                       >            cmp_flag $ff
13ef : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
13f1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13f3 : 28              >            plp         ;restore status
                       >
13f4 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
13f6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
13f8 : e000            >            cpx #0
                       >            trap_ne         ;x changed
13fa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $4b,1
13fc : a042            >            ldy #$42
13fe : a203            >            ldx #4-1
1400 : 4b              >            db  $4b          ;test nop length
                       >        if 1 = 1
1401 : ca              >            dex
1402 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
1403 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1404 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$4b,0
                       >            load_flag 0
1406 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1408 : 48              >            pha         ;use stack to load status
1409 : a9b4            >            lda #$ff-$4b     ;precharge accu
140b : 28              >            plp
                       >
140c : 4b              >            db  $4b          ;test nop integrity - flags off
140d : ea              >            nop
140e : ea              >            nop
                       >            tst_a $ff-$4b,0
140f : 08              >            php         ;save flags
1410 : c9b4            >            cmp #$ff-$4b     ;test result
                       >            trap_ne
1412 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1414 : 68              >            pla         ;load status
1415 : 48              >            pha
                       >            cmp_flag 0
1416 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1418 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
141a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$4b,$ff
                       >            load_flag $ff
141b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
141d : 48              >            pha         ;use stack to load status
141e : a95f            >            lda #$aa-$4b     ;precharge accu
1420 : 28              >            plp
                       >
1421 : 4b              >            db  $4b          ;test nop integrity - flags on
1422 : ea              >            nop
1423 : ea              >            nop
                       >            tst_a $aa-$4b,$ff
1424 : 08              >            php         ;save flags
1425 : c95f            >            cmp #$aa-$4b     ;test result
                       >            trap_ne
1427 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1429 : 68              >            pla         ;load status
142a : 48              >            pha
                       >            cmp_flag $ff
142b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
142d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
142f : 28              >            plp         ;restore status
                       >
1430 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1432 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1434 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1436 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $5b,1
1438 : a042            >            ldy #$42
143a : a203            >            ldx #4-1
143c : 5b              >            db  $5b          ;test nop length
                       >        if 1 = 1
143d : ca              >            dex
143e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
143f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1440 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$5b,0
                       >            load_flag 0
1442 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1444 : 48              >            pha         ;use stack to load status
1445 : a9a4            >            lda #$ff-$5b     ;precharge accu
1447 : 28              >            plp
                       >
1448 : 5b              >            db  $5b          ;test nop integrity - flags off
1449 : ea              >            nop
144a : ea              >            nop
                       >            tst_a $ff-$5b,0
144b : 08              >            php         ;save flags
144c : c9a4            >            cmp #$ff-$5b     ;test result
                       >            trap_ne
144e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1450 : 68              >            pla         ;load status
1451 : 48              >            pha
                       >            cmp_flag 0
1452 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1454 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1456 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$5b,$ff
                       >            load_flag $ff
1457 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1459 : 48              >            pha         ;use stack to load status
145a : a94f            >            lda #$aa-$5b     ;precharge accu
145c : 28              >            plp
                       >
145d : 5b              >            db  $5b          ;test nop integrity - flags on
145e : ea              >            nop
145f : ea              >            nop
                       >            tst_a $aa-$5b,$ff
1460 : 08              >            php         ;save flags
1461 : c94f            >            cmp #$aa-$5b     ;test result
                       >            trap_ne
1463 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1465 : 68              >            pla         ;load status
1466 : 48              >            pha
                       >            cmp_flag $ff
1467 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1469 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
146b : 28              >            plp         ;restore status
                       >
146c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
146e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1470 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1472 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $6b,1
1474 : a042            >            ldy #$42
1476 : a203            >            ldx #4-1
1478 : 6b              >            db  $6b          ;test nop length
                       >        if 1 = 1
1479 : ca              >            dex
147a : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
147b : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
147c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$6b,0
                       >            load_flag 0
147e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1480 : 48              >            pha         ;use stack to load status
1481 : a994            >            lda #$ff-$6b     ;precharge accu
1483 : 28              >            plp
                       >
1484 : 6b              >            db  $6b          ;test nop integrity - flags off
1485 : ea              >            nop
1486 : ea              >            nop
                       >            tst_a $ff-$6b,0
1487 : 08              >            php         ;save flags
1488 : c994            >            cmp #$ff-$6b     ;test result
                       >            trap_ne
148a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
148c : 68              >            pla         ;load status
148d : 48              >            pha
                       >            cmp_flag 0
148e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1490 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1492 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$6b,$ff
                       >            load_flag $ff
1493 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1495 : 48              >            pha         ;use stack to load status
1496 : a93f            >            lda #$aa-$6b     ;precharge accu
1498 : 28              >            plp
                       >
1499 : 6b              >            db  $6b          ;test nop integrity - flags on
149a : ea              >            nop
149b : ea              >            nop
                       >            tst_a $aa-$6b,$ff
149c : 08              >            php         ;save flags
149d : c93f            >            cmp #$aa-$6b     ;test result
                       >            trap_ne
149f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14a1 : 68              >            pla         ;load status
14a2 : 48              >            pha
                       >            cmp_flag $ff
14a3 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
14a5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14a7 : 28              >            plp         ;restore status
                       >
14a8 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
14aa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14ac : e000            >            cpx #0
                       >            trap_ne         ;x changed
14ae : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $7b,1
14b0 : a042            >            ldy #$42
14b2 : a203            >            ldx #4-1
14b4 : 7b              >            db  $7b          ;test nop length
                       >        if 1 = 1
14b5 : ca              >            dex
14b6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
14b7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
14b8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$7b,0
                       >            load_flag 0
14ba : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
14bc : 48              >            pha         ;use stack to load status
14bd : a984            >            lda #$ff-$7b     ;precharge accu
14bf : 28              >            plp
                       >
14c0 : 7b              >            db  $7b          ;test nop integrity - flags off
14c1 : ea              >            nop
14c2 : ea              >            nop
                       >            tst_a $ff-$7b,0
14c3 : 08              >            php         ;save flags
14c4 : c984            >            cmp #$ff-$7b     ;test result
                       >            trap_ne
14c6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14c8 : 68              >            pla         ;load status
14c9 : 48              >            pha
                       >            cmp_flag 0
14ca : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
14cc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14ce : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$7b,$ff
                       >            load_flag $ff
14cf : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
14d1 : 48              >            pha         ;use stack to load status
14d2 : a92f            >            lda #$aa-$7b     ;precharge accu
14d4 : 28              >            plp
                       >
14d5 : 7b              >            db  $7b          ;test nop integrity - flags on
14d6 : ea              >            nop
14d7 : ea              >            nop
                       >            tst_a $aa-$7b,$ff
14d8 : 08              >            php         ;save flags
14d9 : c92f            >            cmp #$aa-$7b     ;test result
                       >            trap_ne
14db : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14dd : 68              >            pla         ;load status
14de : 48              >            pha
                       >            cmp_flag $ff
14df : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
14e1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14e3 : 28              >            plp         ;restore status
                       >
14e4 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
14e6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
14e8 : e000            >            cpx #0
                       >            trap_ne         ;x changed
14ea : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $8b,1
14ec : a042            >            ldy #$42
14ee : a203            >            ldx #4-1
14f0 : 8b              >            db  $8b          ;test nop length
                       >        if 1 = 1
14f1 : ca              >            dex
14f2 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
14f3 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
14f4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$8b,0
                       >            load_flag 0
14f6 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
14f8 : 48              >            pha         ;use stack to load status
14f9 : a974            >            lda #$ff-$8b     ;precharge accu
14fb : 28              >            plp
                       >
14fc : 8b              >            db  $8b          ;test nop integrity - flags off
14fd : ea              >            nop
14fe : ea              >            nop
                       >            tst_a $ff-$8b,0
14ff : 08              >            php         ;save flags
1500 : c974            >            cmp #$ff-$8b     ;test result
                       >            trap_ne
1502 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1504 : 68              >            pla         ;load status
1505 : 48              >            pha
                       >            cmp_flag 0
1506 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1508 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
150a : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$8b,$ff
                       >            load_flag $ff
150b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
150d : 48              >            pha         ;use stack to load status
150e : a91f            >            lda #$aa-$8b     ;precharge accu
1510 : 28              >            plp
                       >
1511 : 8b              >            db  $8b          ;test nop integrity - flags on
1512 : ea              >            nop
1513 : ea              >            nop
                       >            tst_a $aa-$8b,$ff
1514 : 08              >            php         ;save flags
1515 : c91f            >            cmp #$aa-$8b     ;test result
                       >            trap_ne
1517 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1519 : 68              >            pla         ;load status
151a : 48              >            pha
                       >            cmp_flag $ff
151b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
151d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
151f : 28              >            plp         ;restore status
                       >
1520 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1522 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1524 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1526 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $9b,1
1528 : a042            >            ldy #$42
152a : a203            >            ldx #4-1
152c : 9b              >            db  $9b          ;test nop length
                       >        if 1 = 1
152d : ca              >            dex
152e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
152f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1530 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$9b,0
                       >            load_flag 0
1532 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1534 : 48              >            pha         ;use stack to load status
1535 : a964            >            lda #$ff-$9b     ;precharge accu
1537 : 28              >            plp
                       >
1538 : 9b              >            db  $9b          ;test nop integrity - flags off
1539 : ea              >            nop
153a : ea              >            nop
                       >            tst_a $ff-$9b,0
153b : 08              >            php         ;save flags
153c : c964            >            cmp #$ff-$9b     ;test result
                       >            trap_ne
153e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1540 : 68              >            pla         ;load status
1541 : 48              >            pha
                       >            cmp_flag 0
1542 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1544 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1546 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$9b,$ff
                       >            load_flag $ff
1547 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1549 : 48              >            pha         ;use stack to load status
154a : a90f            >            lda #$aa-$9b     ;precharge accu
154c : 28              >            plp
                       >
154d : 9b              >            db  $9b          ;test nop integrity - flags on
154e : ea              >            nop
154f : ea              >            nop
                       >            tst_a $aa-$9b,$ff
1550 : 08              >            php         ;save flags
1551 : c90f            >            cmp #$aa-$9b     ;test result
                       >            trap_ne
1553 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1555 : 68              >            pla         ;load status
1556 : 48              >            pha
                       >            cmp_flag $ff
1557 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1559 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
155b : 28              >            plp         ;restore status
                       >
155c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
155e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1560 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1562 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $ab,1
1564 : a042            >            ldy #$42
1566 : a203            >            ldx #4-1
1568 : ab              >            db  $ab          ;test nop length
                       >        if 1 = 1
1569 : ca              >            dex
156a : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
156b : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
156c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$ab,0
                       >            load_flag 0
156e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1570 : 48              >            pha         ;use stack to load status
1571 : a954            >            lda #$ff-$ab     ;precharge accu
1573 : 28              >            plp
                       >
1574 : ab              >            db  $ab          ;test nop integrity - flags off
1575 : ea              >            nop
1576 : ea              >            nop
                       >            tst_a $ff-$ab,0
1577 : 08              >            php         ;save flags
1578 : c954            >            cmp #$ff-$ab     ;test result
                       >            trap_ne
157a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
157c : 68              >            pla         ;load status
157d : 48              >            pha
                       >            cmp_flag 0
157e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1580 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1582 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$ab,$ff
                       >            load_flag $ff
1583 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1585 : 48              >            pha         ;use stack to load status
1586 : a9ff            >            lda #$aa-$ab     ;precharge accu
1588 : 28              >            plp
                       >
1589 : ab              >            db  $ab          ;test nop integrity - flags on
158a : ea              >            nop
158b : ea              >            nop
                       >            tst_a $aa-$ab,$ff
158c : 08              >            php         ;save flags
158d : c9ff            >            cmp #$aa-$ab     ;test result
                       >            trap_ne
158f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1591 : 68              >            pla         ;load status
1592 : 48              >            pha
                       >            cmp_flag $ff
1593 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1595 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1597 : 28              >            plp         ;restore status
                       >
1598 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
159a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
159c : e000            >            cpx #0
                       >            trap_ne         ;x changed
159e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $bb,1
15a0 : a042            >            ldy #$42
15a2 : a203            >            ldx #4-1
15a4 : bb              >            db  $bb          ;test nop length
                       >        if 1 = 1
15a5 : ca              >            dex
15a6 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
15a7 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
15a8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$bb,0
                       >            load_flag 0
15aa : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
15ac : 48              >            pha         ;use stack to load status
15ad : a944            >            lda #$ff-$bb     ;precharge accu
15af : 28              >            plp
                       >
15b0 : bb              >            db  $bb          ;test nop integrity - flags off
15b1 : ea              >            nop
15b2 : ea              >            nop
                       >            tst_a $ff-$bb,0
15b3 : 08              >            php         ;save flags
15b4 : c944            >            cmp #$ff-$bb     ;test result
                       >            trap_ne
15b6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15b8 : 68              >            pla         ;load status
15b9 : 48              >            pha
                       >            cmp_flag 0
15ba : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
15bc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15be : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$bb,$ff
                       >            load_flag $ff
15bf : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
15c1 : 48              >            pha         ;use stack to load status
15c2 : a9ef            >            lda #$aa-$bb     ;precharge accu
15c4 : 28              >            plp
                       >
15c5 : bb              >            db  $bb          ;test nop integrity - flags on
15c6 : ea              >            nop
15c7 : ea              >            nop
                       >            tst_a $aa-$bb,$ff
15c8 : 08              >            php         ;save flags
15c9 : c9ef            >            cmp #$aa-$bb     ;test result
                       >            trap_ne
15cb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15cd : 68              >            pla         ;load status
15ce : 48              >            pha
                       >            cmp_flag $ff
15cf : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
15d1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15d3 : 28              >            plp         ;restore status
                       >
15d4 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
15d6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15d8 : e000            >            cpx #0
                       >            trap_ne         ;x changed
15da : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $eb,1
15dc : a042            >            ldy #$42
15de : a203            >            ldx #4-1
15e0 : eb              >            db  $eb          ;test nop length
                       >        if 1 = 1
15e1 : ca              >            dex
15e2 : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
15e3 : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
15e4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$eb,0
                       >            load_flag 0
15e6 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
15e8 : 48              >            pha         ;use stack to load status
15e9 : a914            >            lda #$ff-$eb     ;precharge accu
15eb : 28              >            plp
                       >
15ec : eb              >            db  $eb          ;test nop integrity - flags off
15ed : ea              >            nop
15ee : ea              >            nop
                       >            tst_a $ff-$eb,0
15ef : 08              >            php         ;save flags
15f0 : c914            >            cmp #$ff-$eb     ;test result
                       >            trap_ne
15f2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15f4 : 68              >            pla         ;load status
15f5 : 48              >            pha
                       >            cmp_flag 0
15f6 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
15f8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
15fa : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$eb,$ff
                       >            load_flag $ff
15fb : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
15fd : 48              >            pha         ;use stack to load status
15fe : a9bf            >            lda #$aa-$eb     ;precharge accu
1600 : 28              >            plp
                       >
1601 : eb              >            db  $eb          ;test nop integrity - flags on
1602 : ea              >            nop
1603 : ea              >            nop
                       >            tst_a $aa-$eb,$ff
1604 : 08              >            php         ;save flags
1605 : c9bf            >            cmp #$aa-$eb     ;test result
                       >            trap_ne
1607 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1609 : 68              >            pla         ;load status
160a : 48              >            pha
                       >            cmp_flag $ff
160b : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
160d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
160f : 28              >            plp         ;restore status
                       >
1610 : c042            >            cpy #$42
                       >            trap_ne         ;y changed
1612 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1614 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1616 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                nop_test $fb,1
1618 : a042            >            ldy #$42
161a : a203            >            ldx #4-1
161c : fb              >            db  $fb          ;test nop length
                       >        if 1 = 1
161d : ca              >            dex
161e : ca              >            dex
                       >        endif
                       >        if 1 = 2
                       >            iny
                       >            dex
                       >        endif
                       >        if 1 = 3
                       >            iny
                       >            iny
                       >        endif
161f : ca              >            dex
                       >            trap_ne         ;wrong number of bytes
1620 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                       >            set_a $ff-$fb,0
                       >            load_flag 0
1622 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1624 : 48              >            pha         ;use stack to load status
1625 : a904            >            lda #$ff-$fb     ;precharge accu
1627 : 28              >            plp
                       >
1628 : fb              >            db  $fb          ;test nop integrity - flags off
1629 : ea              >            nop
162a : ea              >            nop
                       >            tst_a $ff-$fb,0
162b : 08              >            php         ;save flags
162c : c904            >            cmp #$ff-$fb     ;test result
                       >            trap_ne
162e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1630 : 68              >            pla         ;load status
1631 : 48              >            pha
                       >            cmp_flag 0
1632 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1634 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1636 : 28              >            plp         ;restore status
                       >
                       >            set_a $aa-$fb,$ff
                       >            load_flag $ff
1637 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1639 : 48              >            pha         ;use stack to load status
163a : a9af            >            lda #$aa-$fb     ;precharge accu
163c : 28              >            plp
                       >
163d : fb              >            db  $fb          ;test nop integrity - flags on
163e : ea              >            nop
163f : ea              >            nop
                       >            tst_a $aa-$fb,$ff
1640 : 08              >            php         ;save flags
1641 : c9af            >            cmp #$aa-$fb     ;test result
                       >            trap_ne
1643 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1645 : 68              >            pla         ;load status
1646 : 48              >            pha
                       >            cmp_flag $ff
1647 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1649 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
164b : 28              >            plp         ;restore status
                       >
164c : c042            >            cpy #$42
                       >            trap_ne         ;y changed
164e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1650 : e000            >            cpx #0
                       >            trap_ne         ;x changed
1652 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                            if rkwl_wdc_op = 0      ;NOPs not available on Rockwell & WDC 65C02
                                nop_test $07,1
                                nop_test $17,1
                                nop_test $27,1
                                nop_test $37,1
                                nop_test $47,1
                                nop_test $57,1
                                nop_test $67,1
                                nop_test $77,1
                                nop_test $87,1
                                nop_test $97,1
                                nop_test $a7,1
                                nop_test $b7,1
                                nop_test $c7,1
                                nop_test $d7,1
                                nop_test $e7,1
                                nop_test $f7,1
                                nop_test $0f,1
                                nop_test $1f,1
                                nop_test $2f,1
                                nop_test $3f,1
                                nop_test $4f,1
                                nop_test $5f,1
                                nop_test $6f,1
                                nop_test $7f,1
                                nop_test $8f,1
                                nop_test $9f,1
                                nop_test $af,1
                                nop_test $bf,1
                                nop_test $cf,1
                                nop_test $df,1
                                nop_test $ef,1
                                nop_test $ff,1
                            endif
                            if  wdc_op = 0          ;NOPs not available on WDC 65C02 (WAI, STP)
                                nop_test $cb,1
                                nop_test $db,1
                            endif
                                next_test
1654 : ad0202          >            lda test_case   ;previous test
1657 : c908            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
1659 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0009 =                 >test_num = test_num + 1
165b : a909            >            lda #test_num   ;*** next tests' number
165d : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                                    
                        ; jump indirect (test page cross bug is fixed)
1660 : a203                     ldx #3          ;prepare table
1662 : bd4026           ji1     lda ji_adr,x
1665 : 9dfd02                   sta ji_tab,x
1668 : ca                       dex
1669 : 10f7                     bpl ji1
166b : a927                     lda #hi(ji_px) ;high address if page cross bug
166d : 8d0002                   sta pg_x
                                set_stat 0
                       >            load_flag 0
1670 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1672 : 48              >            pha         ;use stack to load status
1673 : 28              >            plp
                        
1674 : a949                     lda #'I'
1676 : a24e                     ldx #'N'
1678 : a044                     ldy #'D'        ;N=0, V=0, Z=0, C=0
167a : 6cfd02                   jmp (ji_tab)
167d : ea                       nop
                                trap_ne         ;runover protection
167e : d0fe            >        bne *           ;failed not equal (non zero)
                        
                        
1680 : 88                       dey
1681 : 88                       dey
1682 : 08               ji_ret  php             ;either SP or Y count will fail, if we do not hit
1683 : 88                       dey
1684 : 88                       dey
1685 : 88                       dey
1686 : 28                       plp
                                trap_eq         ;returned flags OK?
1687 : f0fe            >        beq *           ;failed equal (zero)
                        
                                trap_pl
1689 : 10fe            >        bpl *           ;failed plus (bit 7 clear)
                        
                                trap_cc
168b : 90fe            >        bcc *           ;failed carry clear
                        
                                trap_vc
168d : 50fe            >        bvc *           ;failed overflow clear
                        
168f : c9e3                     cmp #('I'^$aa)  ;returned registers OK?
                                trap_ne
1691 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1693 : e04f                     cpx #('N'+1)
                                trap_ne
1695 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1697 : c03e                     cpy #('D'-6)
                                trap_ne
1699 : d0fe            >        bne *           ;failed not equal (non zero)
                        
169b : ba                       tsx             ;SP check
169c : e0ff                     cpx #$ff
                                trap_ne
169e : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
16a0 : ad0202          >            lda test_case   ;previous test
16a3 : c909            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
16a5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000a =                 >test_num = test_num + 1
16a7 : a90a            >            lda #test_num   ;*** next tests' number
16a9 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; jump indexed indirect
16ac : a20b                     ldx #11         ;prepare table
16ae : bd7926           jxi1    lda jxi_adr,x
16b1 : 9df902                   sta jxi_tab,x
16b4 : ca                       dex
16b5 : 10f7                     bpl jxi1
16b7 : a927                     lda #hi(jxi_px) ;high address if page cross bug
16b9 : 8d0002                   sta pg_x
                                set_stat 0
                       >            load_flag 0
16bc : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
16be : 48              >            pha         ;use stack to load status
16bf : 28              >            plp
                        
16c0 : a958                     lda #'X'
16c2 : a204                     ldx #4
16c4 : a049                     ldy #'I'        ;N=0, V=0, Z=0, C=0
16c6 : 7cf902                   jmp (jxi_tab,x)
16c9 : ea                       nop
                                trap_ne         ;runover protection
16ca : d0fe            >        bne *           ;failed not equal (non zero)
                        
                        
16cc : 88                       dey
16cd : 88                       dey
16ce : 08               jxi_ret php             ;either SP or Y count will fail, if we do not hit
16cf : 88                       dey
16d0 : 88                       dey
16d1 : 88                       dey
16d2 : 28                       plp
                                trap_eq         ;returned flags OK?
16d3 : f0fe            >        beq *           ;failed equal (zero)
                        
                                trap_pl
16d5 : 10fe            >        bpl *           ;failed plus (bit 7 clear)
                        
                                trap_cc
16d7 : 90fe            >        bcc *           ;failed carry clear
                        
                                trap_vc
16d9 : 50fe            >        bvc *           ;failed overflow clear
                        
16db : c9f2                     cmp #('X'^$aa)  ;returned registers OK?
                                trap_ne
16dd : d0fe            >        bne *           ;failed not equal (non zero)
                        
16df : e006                     cpx #6
                                trap_ne
16e1 : d0fe            >        bne *           ;failed not equal (non zero)
                        
16e3 : c043                     cpy #('I'-6)
                                trap_ne
16e5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
16e7 : ba                       tsx             ;SP check
16e8 : e0ff                     cpx #$ff
                                trap_ne
16ea : d0fe            >        bne *           ;failed not equal (non zero)
                        
                        
16ec : a908                     lda #lo(jxp_ok) ;test with index causing a page cross
16ee : 8d0003                   sta jxp_tab
16f1 : a917                     lda #hi(jxp_ok)
16f3 : 8d0103                   sta jxp_tab+1
16f6 : a905                     lda #lo(jxp_px)
16f8 : 8d0002                   sta pg_x
16fb : a917                     lda #hi(jxp_px)
16fd : 8d0102                   sta pg_x+1
1700 : a2ff                     ldx #$ff
1702 : 7c0102                   jmp (jxp_tab-$ff,x)
                                
1705 :                  jxp_px  
                                trap            ;page cross by index to wrong page
1705 : 4c0517          >        jmp *           ;failed anyway
                        
                        
1708 :                  jxp_ok
                                next_test
1708 : ad0202          >            lda test_case   ;previous test
170b : c90a            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
170d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000b =                 >test_num = test_num + 1
170f : a90b            >            lda #test_num   ;*** next tests' number
1711 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                            if ROM_vectors = 1
                        ; test BRK clears decimal mode
1714 : f8                       sed
1715 : 00                       brk
1716 : ea                       nop
1717 :                  brk_ret
                                next_test
1717 : ad0202          >            lda test_case   ;previous test
171a : c90b            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
171c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000c =                 >test_num = test_num + 1
171e : a90c            >            lda #test_num   ;*** next tests' number
1720 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                            endif
                         
                        ; testing accumulator increment/decrement INC A & DEC A
1723 : a2ac                     ldx #$ac    ;protect x & y
1725 : a0dc                     ldy #$dc
                                set_a $fe,$ff
                       >            load_flag $ff
1727 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1729 : 48              >            pha         ;use stack to load status
172a : a9fe            >            lda #$fe     ;precharge accu
172c : 28              >            plp
                        
172d : 1a                       inc a           ;ff
                                tst_as $ff,$ff-zero
172e : 48              >            pha
172f : 08              >            php         ;save flags
1730 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1732 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1734 : 68              >            pla         ;load status
1735 : 48              >            pha
                       >            cmp_flag $ff-zero
1736 : c9fd            >            cmp #($ff-zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1738 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
173a : 28              >            plp         ;restore status
173b : 68              >            pla
                        
173c : 1a                       inc a           ;00
                                tst_as 0,$ff-minus
173d : 48              >            pha
173e : 08              >            php         ;save flags
173f : c900            >            cmp #0     ;test result
                       >            trap_ne
1741 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1743 : 68              >            pla         ;load status
1744 : 48              >            pha
                       >            cmp_flag $ff-minus
1745 : c97f            >            cmp #($ff-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1747 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1749 : 28              >            plp         ;restore status
174a : 68              >            pla
                        
174b : 1a                       inc a           ;01
                                tst_as 1,$ff-minus-zero
174c : 48              >            pha
174d : 08              >            php         ;save flags
174e : c901            >            cmp #1     ;test result
                       >            trap_ne
1750 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1752 : 68              >            pla         ;load status
1753 : 48              >            pha
                       >            cmp_flag $ff-minus-zero
1754 : c97d            >            cmp #($ff-minus-zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1756 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1758 : 28              >            plp         ;restore status
1759 : 68              >            pla
                        
175a : 3a                       dec a           ;00
                                tst_as 0,$ff-minus
175b : 48              >            pha
175c : 08              >            php         ;save flags
175d : c900            >            cmp #0     ;test result
                       >            trap_ne
175f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1761 : 68              >            pla         ;load status
1762 : 48              >            pha
                       >            cmp_flag $ff-minus
1763 : c97f            >            cmp #($ff-minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1765 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1767 : 28              >            plp         ;restore status
1768 : 68              >            pla
                        
1769 : 3a                       dec a           ;ff
                                tst_as $ff,$ff-zero
176a : 48              >            pha
176b : 08              >            php         ;save flags
176c : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
176e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1770 : 68              >            pla         ;load status
1771 : 48              >            pha
                       >            cmp_flag $ff-zero
1772 : c9fd            >            cmp #($ff-zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1774 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1776 : 28              >            plp         ;restore status
1777 : 68              >            pla
                        
1778 : 3a                       dec a           ;fe
                                set_a $fe,0
                       >            load_flag 0
1779 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
177b : 48              >            pha         ;use stack to load status
177c : a9fe            >            lda #$fe     ;precharge accu
177e : 28              >            plp
                        
177f : 1a                       inc a           ;ff
                                tst_as $ff,minus
1780 : 48              >            pha
1781 : 08              >            php         ;save flags
1782 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1784 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1786 : 68              >            pla         ;load status
1787 : 48              >            pha
                       >            cmp_flag minus
1788 : c9b0            >            cmp #(minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
178a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
178c : 28              >            plp         ;restore status
178d : 68              >            pla
                        
178e : 1a                       inc a           ;00
                                tst_as 0,zero
178f : 48              >            pha
1790 : 08              >            php         ;save flags
1791 : c900            >            cmp #0     ;test result
                       >            trap_ne
1793 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1795 : 68              >            pla         ;load status
1796 : 48              >            pha
                       >            cmp_flag zero
1797 : c932            >            cmp #(zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1799 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
179b : 28              >            plp         ;restore status
179c : 68              >            pla
                        
179d : 1a                       inc a           ;01
                                tst_as 1,0
179e : 48              >            pha
179f : 08              >            php         ;save flags
17a0 : c901            >            cmp #1     ;test result
                       >            trap_ne
17a2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17a4 : 68              >            pla         ;load status
17a5 : 48              >            pha
                       >            cmp_flag 0
17a6 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
17a8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17aa : 28              >            plp         ;restore status
17ab : 68              >            pla
                        
17ac : 3a                       dec a           ;00
                                tst_as 0,zero
17ad : 48              >            pha
17ae : 08              >            php         ;save flags
17af : c900            >            cmp #0     ;test result
                       >            trap_ne
17b1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17b3 : 68              >            pla         ;load status
17b4 : 48              >            pha
                       >            cmp_flag zero
17b5 : c932            >            cmp #(zero|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
17b7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17b9 : 28              >            plp         ;restore status
17ba : 68              >            pla
                        
17bb : 3a                       dec a           ;ff
                                tst_as $ff,minus
17bc : 48              >            pha
17bd : 08              >            php         ;save flags
17be : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
17c0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17c2 : 68              >            pla         ;load status
17c3 : 48              >            pha
                       >            cmp_flag minus
17c4 : c9b0            >            cmp #(minus|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
17c6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
17c8 : 28              >            plp         ;restore status
17c9 : 68              >            pla
                        
17ca : e0ac                     cpx #$ac
                                trap_ne     ;x altered during test
17cc : d0fe            >        bne *           ;failed not equal (non zero)
                        
17ce : c0dc                     cpy #$dc
                                trap_ne     ;y altered during test
17d0 : d0fe            >        bne *           ;failed not equal (non zero)
                        
17d2 : ba                       tsx
17d3 : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
17d5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
17d7 : ad0202          >            lda test_case   ;previous test
17da : c90c            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
17dc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000d =                 >test_num = test_num + 1
17de : a90d            >            lda #test_num   ;*** next tests' number
17e0 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; testing load / store accumulator LDA / STA (zp)
17e3 : a299                     ldx #$99    ;protect x & y
17e5 : a066                     ldy #$66
                                set_stat 0  
                       >            load_flag 0  
17e7 : a900            >            lda #0               ;allow test to change I-flag (no mask)
                       >
17e9 : 48              >            pha         ;use stack to load status
17ea : 28              >            plp
                        
17eb : b222                     lda (ind1)
17ed : 08                       php         ;test stores do not alter flags
17ee : 49c3                     eor #$c3
17f0 : 28                       plp
17f1 : 922e                     sta (indt)
17f3 : 08                       php         ;flags after load/store sequence
17f4 : 49c3                     eor #$c3
17f6 : c9c3                     cmp #$c3    ;test result
                                trap_ne
17f8 : d0fe            >        bne *           ;failed not equal (non zero)
                        
17fa : 68                       pla         ;load status
                                eor_flag 0
17fb : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                        
17fd : cd0f02                   cmp fLDx    ;test flags
                                trap_ne
1800 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat 0
                       >            load_flag 0
1802 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1804 : 48              >            pha         ;use stack to load status
1805 : 28              >            plp
                        
1806 : b224                     lda (ind1+2)
1808 : 08                       php         ;test stores do not alter flags
1809 : 49c3                     eor #$c3
180b : 28                       plp
180c : 9230                     sta (indt+2)
180e : 08                       php         ;flags after load/store sequence
180f : 49c3                     eor #$c3
1811 : c982                     cmp #$82    ;test result
                                trap_ne
1813 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1815 : 68                       pla         ;load status
                                eor_flag 0
1816 : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                        
1818 : cd1002                   cmp fLDx+1  ;test flags
                                trap_ne
181b : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat 0
                       >            load_flag 0
181d : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
181f : 48              >            pha         ;use stack to load status
1820 : 28              >            plp
                        
1821 : b226                     lda (ind1+4)
1823 : 08                       php         ;test stores do not alter flags
1824 : 49c3                     eor #$c3
1826 : 28                       plp
1827 : 9232                     sta (indt+4)
1829 : 08                       php         ;flags after load/store sequence
182a : 49c3                     eor #$c3
182c : c941                     cmp #$41    ;test result
                                trap_ne
182e : d0fe            >        bne *           ;failed not equal (non zero)
                        
1830 : 68                       pla         ;load status
                                eor_flag 0
1831 : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                        
1833 : cd1102                   cmp fLDx+2  ;test flags
                                trap_ne
1836 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat 0
                       >            load_flag 0
1838 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
183a : 48              >            pha         ;use stack to load status
183b : 28              >            plp
                        
183c : b228                     lda (ind1+6)
183e : 08                       php         ;test stores do not alter flags
183f : 49c3                     eor #$c3
1841 : 28                       plp
1842 : 9234                     sta (indt+6)
1844 : 08                       php         ;flags after load/store sequence
1845 : 49c3                     eor #$c3
1847 : c900                     cmp #0      ;test result
                                trap_ne
1849 : d0fe            >        bne *           ;failed not equal (non zero)
                        
184b : 68                       pla         ;load status
                                eor_flag 0
184c : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                        
184e : cd1202                   cmp fLDx+3  ;test flags
                                trap_ne
1851 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1853 : e099                     cpx #$99
                                trap_ne     ;x altered during test
1855 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1857 : c066                     cpy #$66
                                trap_ne     ;y altered during test
1859 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                        
185b : a003                     ldy #3      ;testing store result
185d : a200                     ldx #0
185f : b90502           tstai1  lda abst,y
1862 : 49c3                     eor #$c3
1864 : d90a02                   cmp abs1,y
                                trap_ne     ;store to indirect data
1867 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1869 : 8a                       txa
186a : 990502                   sta abst,y  ;clear                
186d : 88                       dey
186e : 10ef                     bpl tstai1
                        
1870 : a299                     ldx #$99    ;protect x & y
1872 : a066                     ldy #$66
                                set_stat $ff  
                       >            load_flag $ff  
1874 : a9ff            >            lda #$ff               ;allow test to change I-flag (no mask)
                       >
1876 : 48              >            pha         ;use stack to load status
1877 : 28              >            plp
                        
1878 : b222                     lda (ind1)
187a : 08                       php         ;test stores do not alter flags
187b : 49c3                     eor #$c3
187d : 28                       plp
187e : 922e                     sta (indt)
1880 : 08                       php         ;flags after load/store sequence
1881 : 49c3                     eor #$c3
1883 : c9c3                     cmp #$c3    ;test result
                                trap_ne
1885 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1887 : 68                       pla         ;load status
                                eor_flag lo~fnz ;mask bits not altered
1888 : 497d            >            eor #lo~fnz |fao         ;invert expected flags + always on bits
                        
188a : cd0f02                   cmp fLDx    ;test flags
                                trap_ne
188d : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat $ff
                       >            load_flag $ff
188f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1891 : 48              >            pha         ;use stack to load status
1892 : 28              >            plp
                        
1893 : b224                     lda (ind1+2)
1895 : 08                       php         ;test stores do not alter flags
1896 : 49c3                     eor #$c3
1898 : 28                       plp
1899 : 9230                     sta (indt+2)
189b : 08                       php         ;flags after load/store sequence
189c : 49c3                     eor #$c3
189e : c982                     cmp #$82    ;test result
                                trap_ne
18a0 : d0fe            >        bne *           ;failed not equal (non zero)
                        
18a2 : 68                       pla         ;load status
                                eor_flag lo~fnz ;mask bits not altered
18a3 : 497d            >            eor #lo~fnz |fao         ;invert expected flags + always on bits
                        
18a5 : cd1002                   cmp fLDx+1  ;test flags
                                trap_ne
18a8 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat $ff
                       >            load_flag $ff
18aa : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
18ac : 48              >            pha         ;use stack to load status
18ad : 28              >            plp
                        
18ae : b226                     lda (ind1+4)
18b0 : 08                       php         ;test stores do not alter flags
18b1 : 49c3                     eor #$c3
18b3 : 28                       plp
18b4 : 9232                     sta (indt+4)
18b6 : 08                       php         ;flags after load/store sequence
18b7 : 49c3                     eor #$c3
18b9 : c941                     cmp #$41    ;test result
                                trap_ne
18bb : d0fe            >        bne *           ;failed not equal (non zero)
                        
18bd : 68                       pla         ;load status
                                eor_flag lo~fnz ;mask bits not altered
18be : 497d            >            eor #lo~fnz |fao         ;invert expected flags + always on bits
                        
18c0 : cd1102                   cmp fLDx+2  ;test flags
                                trap_ne
18c3 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                set_stat $ff
                       >            load_flag $ff
18c5 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
18c7 : 48              >            pha         ;use stack to load status
18c8 : 28              >            plp
                        
18c9 : b228                     lda (ind1+6)
18cb : 08                       php         ;test stores do not alter flags
18cc : 49c3                     eor #$c3
18ce : 28                       plp
18cf : 9234                     sta (indt+6)
18d1 : 08                       php         ;flags after load/store sequence
18d2 : 49c3                     eor #$c3
18d4 : c900                     cmp #0      ;test result
                                trap_ne
18d6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
18d8 : 68                       pla         ;load status
                                eor_flag lo~fnz ;mask bits not altered
18d9 : 497d            >            eor #lo~fnz |fao         ;invert expected flags + always on bits
                        
18db : cd1202                   cmp fLDx+3  ;test flags
                                trap_ne
18de : d0fe            >        bne *           ;failed not equal (non zero)
                        
18e0 : e099                     cpx #$99
                                trap_ne     ;x altered during test
18e2 : d0fe            >        bne *           ;failed not equal (non zero)
                        
18e4 : c066                     cpy #$66
                                trap_ne     ;y altered during test
18e6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                        
18e8 : a003                     ldy #3      ;testing store result
18ea : a200                     ldx #0
18ec : b90502           tstai2  lda abst,y
18ef : 49c3                     eor #$c3
18f1 : d90a02                   cmp abs1,y
                                trap_ne     ;store to indirect data
18f4 : d0fe            >        bne *           ;failed not equal (non zero)
                        
18f6 : 8a                       txa
18f7 : 990502                   sta abst,y  ;clear                
18fa : 88                       dey
18fb : 10ef                     bpl tstai2
18fd : ba                       tsx
18fe : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
1900 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
1902 : ad0202          >            lda test_case   ;previous test
1905 : c90d            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
1907 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000e =                 >test_num = test_num + 1
1909 : a90e            >            lda #test_num   ;*** next tests' number
190b : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; testing STZ - zp / abs / zp,x / abs,x
190e : a07b                     ldy #123    ;protect y
1910 : a204                     ldx #4      ;precharge test area
1912 : a907                     lda #7
1914 : 950a             tstz1   sta zpt,x
1916 : 0a                       asl a
1917 : ca                       dex
1918 : 10fa                     bpl tstz1
191a : a204                     ldx #4
                                set_a $55,$ff
                       >            load_flag $ff
191c : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
191e : 48              >            pha         ;use stack to load status
191f : a955            >            lda #$55     ;precharge accu
1921 : 28              >            plp
                        
1922 : 640a                     stz zpt     
1924 : 640b                     stz zpt+1
1926 : 640c                     stz zpt+2
1928 : 640d                     stz zpt+3
192a : 640e                     stz zpt+4
                                tst_a $55,$ff
192c : 08              >            php         ;save flags
192d : c955            >            cmp #$55     ;test result
                       >            trap_ne
192f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1931 : 68              >            pla         ;load status
1932 : 48              >            pha
                       >            cmp_flag $ff
1933 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1935 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1937 : 28              >            plp         ;restore status
                        
1938 : b50a             tstz2   lda zpt,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
193a : d0fe            >        bne *           ;failed not equal (non zero)
                        
193c : ca                       dex
193d : 10f9                     bpl tstz2
193f : a204                     ldx #4      ;precharge test area
1941 : a907                     lda #7
1943 : 950a             tstz3   sta zpt,x
1945 : 0a                       asl a
1946 : ca                       dex
1947 : 10fa                     bpl tstz3
1949 : a204                     ldx #4
                                set_a $aa,0
                       >            load_flag 0
194b : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
194d : 48              >            pha         ;use stack to load status
194e : a9aa            >            lda #$aa     ;precharge accu
1950 : 28              >            plp
                        
1951 : 640a                     stz zpt     
1953 : 640b                     stz zpt+1
1955 : 640c                     stz zpt+2
1957 : 640d                     stz zpt+3
1959 : 640e                     stz zpt+4
                                tst_a $aa,0
195b : 08              >            php         ;save flags
195c : c9aa            >            cmp #$aa     ;test result
                       >            trap_ne
195e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1960 : 68              >            pla         ;load status
1961 : 48              >            pha
                       >            cmp_flag 0
1962 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1964 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1966 : 28              >            plp         ;restore status
                        
1967 : b50a             tstz4   lda zpt,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
1969 : d0fe            >        bne *           ;failed not equal (non zero)
                        
196b : ca                       dex
196c : 10f9                     bpl tstz4
                                
196e : a204                     ldx #4      ;precharge test area
1970 : a907                     lda #7
1972 : 9d0502           tstz5   sta abst,x
1975 : 0a                       asl a
1976 : ca                       dex
1977 : 10f9                     bpl tstz5
1979 : a204                     ldx #4
                                set_a $55,$ff
                       >            load_flag $ff
197b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
197d : 48              >            pha         ;use stack to load status
197e : a955            >            lda #$55     ;precharge accu
1980 : 28              >            plp
                        
1981 : 9c0502                   stz abst     
1984 : 9c0602                   stz abst+1
1987 : 9c0702                   stz abst+2
198a : 9c0802                   stz abst+3
198d : 9c0902                   stz abst+4
                                tst_a $55,$ff
1990 : 08              >            php         ;save flags
1991 : c955            >            cmp #$55     ;test result
                       >            trap_ne
1993 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1995 : 68              >            pla         ;load status
1996 : 48              >            pha
                       >            cmp_flag $ff
1997 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1999 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
199b : 28              >            plp         ;restore status
                        
199c : bd0502           tstz6   lda abst,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ abs
199f : d0fe            >        bne *           ;failed not equal (non zero)
                        
19a1 : ca                       dex
19a2 : 10f8                     bpl tstz6
19a4 : a204                     ldx #4      ;precharge test area
19a6 : a907                     lda #7
19a8 : 9d0502           tstz7   sta abst,x
19ab : 0a                       asl a
19ac : ca                       dex
19ad : 10f9                     bpl tstz7
19af : a204                     ldx #4
                                set_a $aa,0
                       >            load_flag 0
19b1 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
19b3 : 48              >            pha         ;use stack to load status
19b4 : a9aa            >            lda #$aa     ;precharge accu
19b6 : 28              >            plp
                        
19b7 : 9c0502                   stz abst     
19ba : 9c0602                   stz abst+1
19bd : 9c0702                   stz abst+2
19c0 : 9c0802                   stz abst+3
19c3 : 9c0902                   stz abst+4
                                tst_a $aa,0
19c6 : 08              >            php         ;save flags
19c7 : c9aa            >            cmp #$aa     ;test result
                       >            trap_ne
19c9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
19cb : 68              >            pla         ;load status
19cc : 48              >            pha
                       >            cmp_flag 0
19cd : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
19cf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
19d1 : 28              >            plp         ;restore status
                        
19d2 : bd0502           tstz8   lda abst,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ abs
19d5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
19d7 : ca                       dex
19d8 : 10f8                     bpl tstz8
                                
19da : a204                     ldx #4      ;precharge test area
19dc : a907                     lda #7
19de : 950a             tstz11  sta zpt,x
19e0 : 0a                       asl a
19e1 : ca                       dex
19e2 : 10fa                     bpl tstz11
19e4 : a204                     ldx #4
19e6 :                  tstz15
                                set_a $55,$ff
                       >            load_flag $ff
19e6 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
19e8 : 48              >            pha         ;use stack to load status
19e9 : a955            >            lda #$55     ;precharge accu
19eb : 28              >            plp
                        
19ec : 740a                     stz zpt,x     
                                tst_a $55,$ff
19ee : 08              >            php         ;save flags
19ef : c955            >            cmp #$55     ;test result
                       >            trap_ne
19f1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
19f3 : 68              >            pla         ;load status
19f4 : 48              >            pha
                       >            cmp_flag $ff
19f5 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
19f7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
19f9 : 28              >            plp         ;restore status
                        
19fa : ca                       dex
19fb : 10e9                     bpl tstz15
19fd : a204                     ldx #4
19ff : b50a             tstz12  lda zpt,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
1a01 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1a03 : ca                       dex
1a04 : 10f9                     bpl tstz12
1a06 : a204                     ldx #4      ;precharge test area
1a08 : a907                     lda #7
1a0a : 950a             tstz13  sta zpt,x
1a0c : 0a                       asl a
1a0d : ca                       dex
1a0e : 10fa                     bpl tstz13
1a10 : a204                     ldx #4
1a12 :                  tstz16
                                set_a $aa,0
                       >            load_flag 0
1a12 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1a14 : 48              >            pha         ;use stack to load status
1a15 : a9aa            >            lda #$aa     ;precharge accu
1a17 : 28              >            plp
                        
1a18 : 740a                     stz zpt,x
                                tst_a $aa,0
1a1a : 08              >            php         ;save flags
1a1b : c9aa            >            cmp #$aa     ;test result
                       >            trap_ne
1a1d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a1f : 68              >            pla         ;load status
1a20 : 48              >            pha
                       >            cmp_flag 0
1a21 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1a23 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a25 : 28              >            plp         ;restore status
                        
1a26 : ca                       dex
1a27 : 10e9                     bpl tstz16
1a29 : a204                     ldx #4
1a2b : b50a             tstz14  lda zpt,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
1a2d : d0fe            >        bne *           ;failed not equal (non zero)
                        
1a2f : ca                       dex
1a30 : 10f9                     bpl tstz14
                                
1a32 : a204                     ldx #4      ;precharge test area
1a34 : a907                     lda #7
1a36 : 9d0502           tstz21  sta abst,x
1a39 : 0a                       asl a
1a3a : ca                       dex
1a3b : 10f9                     bpl tstz21
1a3d : a204                     ldx #4
1a3f :                  tstz25
                                set_a $55,$ff
                       >            load_flag $ff
1a3f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1a41 : 48              >            pha         ;use stack to load status
1a42 : a955            >            lda #$55     ;precharge accu
1a44 : 28              >            plp
                        
1a45 : 9e0502                   stz abst,x     
                                tst_a $55,$ff
1a48 : 08              >            php         ;save flags
1a49 : c955            >            cmp #$55     ;test result
                       >            trap_ne
1a4b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a4d : 68              >            pla         ;load status
1a4e : 48              >            pha
                       >            cmp_flag $ff
1a4f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1a51 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a53 : 28              >            plp         ;restore status
                        
1a54 : ca                       dex
1a55 : 10e8                     bpl tstz25
1a57 : a204                     ldx #4
1a59 : bd0502           tstz22  lda abst,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
1a5c : d0fe            >        bne *           ;failed not equal (non zero)
                        
1a5e : ca                       dex
1a5f : 10f8                     bpl tstz22
1a61 : a204                     ldx #4      ;precharge test area
1a63 : a907                     lda #7
1a65 : 9d0502           tstz23  sta abst,x
1a68 : 0a                       asl a
1a69 : ca                       dex
1a6a : 10f9                     bpl tstz23
1a6c : a204                     ldx #4
1a6e :                  tstz26
                                set_a $aa,0
                       >            load_flag 0
1a6e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1a70 : 48              >            pha         ;use stack to load status
1a71 : a9aa            >            lda #$aa     ;precharge accu
1a73 : 28              >            plp
                        
1a74 : 9e0502                   stz abst,x
                                tst_a $aa,0
1a77 : 08              >            php         ;save flags
1a78 : c9aa            >            cmp #$aa     ;test result
                       >            trap_ne
1a7a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a7c : 68              >            pla         ;load status
1a7d : 48              >            pha
                       >            cmp_flag 0
1a7e : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1a80 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1a82 : 28              >            plp         ;restore status
                        
1a83 : ca                       dex
1a84 : 10e8                     bpl tstz26
1a86 : a204                     ldx #4
1a88 : bd0502           tstz24  lda abst,x   ;verify zeros stored
                                trap_ne     ;non zero after STZ zp
1a8b : d0fe            >        bne *           ;failed not equal (non zero)
                        
1a8d : ca                       dex
1a8e : 10f8                     bpl tstz24
                                
1a90 : c07b                     cpy #123
                                trap_ne     ;y altered during test 
1a92 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1a94 : ba                       tsx
1a95 : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
1a97 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
1a99 : ad0202          >            lda test_case   ;previous test
1a9c : c90e            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
1a9e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
000f =                 >test_num = test_num + 1
1aa0 : a90f            >            lda #test_num   ;*** next tests' number
1aa2 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; testing BIT - zp,x / abs,x / #
1aa5 : a042                     ldy #$42
1aa7 : a203                     ldx #3
                                set_a $ff,0
                       >            load_flag 0
1aa9 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1aab : 48              >            pha         ;use stack to load status
1aac : a9ff            >            lda #$ff     ;precharge accu
1aae : 28              >            plp
                        
1aaf : 3411                     bit zp1,x   ;00 - should set Z / clear  NV
                                tst_a $ff,fz 
1ab1 : 08              >            php         ;save flags
1ab2 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1ab4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ab6 : 68              >            pla         ;load status
1ab7 : 48              >            pha
                       >            cmp_flag fz 
1ab8 : c932            >            cmp #(fz |fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1aba : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1abc : 28              >            plp         ;restore status
                        
1abd : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1abe : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1ac0 : 48              >            pha         ;use stack to load status
1ac1 : a901            >            lda #1     ;precharge accu
1ac3 : 28              >            plp
                        
1ac4 : 3411                     bit zp1,x   ;41 - should set V (M6) / clear NZ
                                tst_a 1,fv
1ac6 : 08              >            php         ;save flags
1ac7 : c901            >            cmp #1     ;test result
                       >            trap_ne
1ac9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1acb : 68              >            pla         ;load status
1acc : 48              >            pha
                       >            cmp_flag fv
1acd : c970            >            cmp #(fv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1acf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ad1 : 28              >            plp         ;restore status
                        
1ad2 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1ad3 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1ad5 : 48              >            pha         ;use stack to load status
1ad6 : a901            >            lda #1     ;precharge accu
1ad8 : 28              >            plp
                        
1ad9 : 3411                     bit zp1,x   ;82 - should set N (M7) & Z / clear V
                                tst_a 1,fnz
1adb : 08              >            php         ;save flags
1adc : c901            >            cmp #1     ;test result
                       >            trap_ne
1ade : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ae0 : 68              >            pla         ;load status
1ae1 : 48              >            pha
                       >            cmp_flag fnz
1ae2 : c9b2            >            cmp #(fnz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ae4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ae6 : 28              >            plp         ;restore status
                        
1ae7 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1ae8 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1aea : 48              >            pha         ;use stack to load status
1aeb : a901            >            lda #1     ;precharge accu
1aed : 28              >            plp
                        
1aee : 3411                     bit zp1,x   ;c3 - should set N (M7) & V (M6) / clear Z
                                tst_a 1,fnv
1af0 : 08              >            php         ;save flags
1af1 : c901            >            cmp #1     ;test result
                       >            trap_ne
1af3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1af5 : 68              >            pla         ;load status
1af6 : 48              >            pha
                       >            cmp_flag fnv
1af7 : c9f0            >            cmp #(fnv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1af9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1afb : 28              >            plp         ;restore status
                        
                                
                                set_a 1,$ff
                       >            load_flag $ff
1afc : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1afe : 48              >            pha         ;use stack to load status
1aff : a901            >            lda #1     ;precharge accu
1b01 : 28              >            plp
                        
1b02 : 3411                     bit zp1,x   ;c3 - should set N (M7) & V (M6) / clear Z
                                tst_a 1,~fz
1b04 : 08              >            php         ;save flags
1b05 : c901            >            cmp #1     ;test result
                       >            trap_ne
1b07 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b09 : 68              >            pla         ;load status
1b0a : 48              >            pha
                       >            cmp_flag ~fz
1b0b : c9fd            >            cmp #(~fz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b0d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b0f : 28              >            plp         ;restore status
                        
1b10 : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1b11 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1b13 : 48              >            pha         ;use stack to load status
1b14 : a901            >            lda #1     ;precharge accu
1b16 : 28              >            plp
                        
1b17 : 3411                     bit zp1,x   ;82 - should set N (M7) & Z / clear V
                                tst_a 1,~fv
1b19 : 08              >            php         ;save flags
1b1a : c901            >            cmp #1     ;test result
                       >            trap_ne
1b1c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b1e : 68              >            pla         ;load status
1b1f : 48              >            pha
                       >            cmp_flag ~fv
1b20 : c9bf            >            cmp #(~fv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b22 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b24 : 28              >            plp         ;restore status
                        
1b25 : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1b26 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1b28 : 48              >            pha         ;use stack to load status
1b29 : a901            >            lda #1     ;precharge accu
1b2b : 28              >            plp
                        
1b2c : 3411                     bit zp1,x   ;41 - should set V (M6) / clear NZ
                                tst_a 1,~fnz
1b2e : 08              >            php         ;save flags
1b2f : c901            >            cmp #1     ;test result
                       >            trap_ne
1b31 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b33 : 68              >            pla         ;load status
1b34 : 48              >            pha
                       >            cmp_flag ~fnz
1b35 : c97d            >            cmp #(~fnz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b37 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b39 : 28              >            plp         ;restore status
                        
1b3a : e8                       inx
                                set_a $ff,$ff
                       >            load_flag $ff
1b3b : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1b3d : 48              >            pha         ;use stack to load status
1b3e : a9ff            >            lda #$ff     ;precharge accu
1b40 : 28              >            plp
                        
1b41 : 3411                     bit zp1,x   ;00 - should set Z / clear  NV
                                tst_a $ff,~fnv 
1b43 : 08              >            php         ;save flags
1b44 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1b46 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b48 : 68              >            pla         ;load status
1b49 : 48              >            pha
                       >            cmp_flag ~fnv 
1b4a : c93f            >            cmp #(~fnv |fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b4c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b4e : 28              >            plp         ;restore status
                        
                                
                                set_a $ff,0
                       >            load_flag 0
1b4f : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1b51 : 48              >            pha         ;use stack to load status
1b52 : a9ff            >            lda #$ff     ;precharge accu
1b54 : 28              >            plp
                        
1b55 : 3c0a02                   bit abs1,x  ;00 - should set Z / clear  NV
                                tst_a $ff,fz 
1b58 : 08              >            php         ;save flags
1b59 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1b5b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b5d : 68              >            pla         ;load status
1b5e : 48              >            pha
                       >            cmp_flag fz 
1b5f : c932            >            cmp #(fz |fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b61 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b63 : 28              >            plp         ;restore status
                        
1b64 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1b65 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1b67 : 48              >            pha         ;use stack to load status
1b68 : a901            >            lda #1     ;precharge accu
1b6a : 28              >            plp
                        
1b6b : 3c0a02                   bit abs1,x  ;41 - should set V (M6) / clear NZ
                                tst_a 1,fv
1b6e : 08              >            php         ;save flags
1b6f : c901            >            cmp #1     ;test result
                       >            trap_ne
1b71 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b73 : 68              >            pla         ;load status
1b74 : 48              >            pha
                       >            cmp_flag fv
1b75 : c970            >            cmp #(fv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b77 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b79 : 28              >            plp         ;restore status
                        
1b7a : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1b7b : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1b7d : 48              >            pha         ;use stack to load status
1b7e : a901            >            lda #1     ;precharge accu
1b80 : 28              >            plp
                        
1b81 : 3c0a02                   bit abs1,x  ;82 - should set N (M7) & Z / clear V
                                tst_a 1,fnz
1b84 : 08              >            php         ;save flags
1b85 : c901            >            cmp #1     ;test result
                       >            trap_ne
1b87 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b89 : 68              >            pla         ;load status
1b8a : 48              >            pha
                       >            cmp_flag fnz
1b8b : c9b2            >            cmp #(fnz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1b8d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b8f : 28              >            plp         ;restore status
                        
1b90 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1b91 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1b93 : 48              >            pha         ;use stack to load status
1b94 : a901            >            lda #1     ;precharge accu
1b96 : 28              >            plp
                        
1b97 : 3c0a02                   bit abs1,x  ;c3 - should set N (M7) & V (M6) / clear Z
                                tst_a 1,fnv
1b9a : 08              >            php         ;save flags
1b9b : c901            >            cmp #1     ;test result
                       >            trap_ne
1b9d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1b9f : 68              >            pla         ;load status
1ba0 : 48              >            pha
                       >            cmp_flag fnv
1ba1 : c9f0            >            cmp #(fnv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ba3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ba5 : 28              >            plp         ;restore status
                        
                                
                                set_a 1,$ff
                       >            load_flag $ff
1ba6 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1ba8 : 48              >            pha         ;use stack to load status
1ba9 : a901            >            lda #1     ;precharge accu
1bab : 28              >            plp
                        
1bac : 3c0a02                   bit abs1,x  ;c3 - should set N (M7) & V (M6) / clear Z
                                tst_a 1,~fz
1baf : 08              >            php         ;save flags
1bb0 : c901            >            cmp #1     ;test result
                       >            trap_ne
1bb2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bb4 : 68              >            pla         ;load status
1bb5 : 48              >            pha
                       >            cmp_flag ~fz
1bb6 : c9fd            >            cmp #(~fz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1bb8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bba : 28              >            plp         ;restore status
                        
1bbb : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1bbc : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1bbe : 48              >            pha         ;use stack to load status
1bbf : a901            >            lda #1     ;precharge accu
1bc1 : 28              >            plp
                        
1bc2 : 3c0a02                   bit abs1,x  ;82 - should set N (M7) & Z / clear V
                                tst_a 1,~fv
1bc5 : 08              >            php         ;save flags
1bc6 : c901            >            cmp #1     ;test result
                       >            trap_ne
1bc8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bca : 68              >            pla         ;load status
1bcb : 48              >            pha
                       >            cmp_flag ~fv
1bcc : c9bf            >            cmp #(~fv|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1bce : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bd0 : 28              >            plp         ;restore status
                        
1bd1 : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1bd2 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1bd4 : 48              >            pha         ;use stack to load status
1bd5 : a901            >            lda #1     ;precharge accu
1bd7 : 28              >            plp
                        
1bd8 : 3c0a02                   bit abs1,x  ;41 - should set V (M6) / clear NZ
                                tst_a 1,~fnz
1bdb : 08              >            php         ;save flags
1bdc : c901            >            cmp #1     ;test result
                       >            trap_ne
1bde : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1be0 : 68              >            pla         ;load status
1be1 : 48              >            pha
                       >            cmp_flag ~fnz
1be2 : c97d            >            cmp #(~fnz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1be4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1be6 : 28              >            plp         ;restore status
                        
1be7 : e8                       inx
                                set_a $ff,$ff
                       >            load_flag $ff
1be8 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1bea : 48              >            pha         ;use stack to load status
1beb : a9ff            >            lda #$ff     ;precharge accu
1bed : 28              >            plp
                        
1bee : 3c0a02                   bit abs1,x  ;00 - should set Z / clear  NV
                                tst_a $ff,~fnv 
1bf1 : 08              >            php         ;save flags
1bf2 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1bf4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bf6 : 68              >            pla         ;load status
1bf7 : 48              >            pha
                       >            cmp_flag ~fnv 
1bf8 : c93f            >            cmp #(~fnv |fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1bfa : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1bfc : 28              >            plp         ;restore status
                        
                                
                                set_a $ff,0
                       >            load_flag 0
1bfd : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1bff : 48              >            pha         ;use stack to load status
1c00 : a9ff            >            lda #$ff     ;precharge accu
1c02 : 28              >            plp
                        
1c03 : 8900                     bit #$00    ;00 - should set Z
                                tst_a $ff,fz 
1c05 : 08              >            php         ;save flags
1c06 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1c08 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c0a : 68              >            pla         ;load status
1c0b : 48              >            pha
                       >            cmp_flag fz 
1c0c : c932            >            cmp #(fz |fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c0e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c10 : 28              >            plp         ;restore status
                        
1c11 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1c12 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1c14 : 48              >            pha         ;use stack to load status
1c15 : a901            >            lda #1     ;precharge accu
1c17 : 28              >            plp
                        
1c18 : 8941                     bit #$41    ;41 - should clear Z
                                tst_a 1,0
1c1a : 08              >            php         ;save flags
1c1b : c901            >            cmp #1     ;test result
                       >            trap_ne
1c1d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c1f : 68              >            pla         ;load status
1c20 : 48              >            pha
                       >            cmp_flag 0
1c21 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c23 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c25 : 28              >            plp         ;restore status
                        
                        ; *** DEBUG INFO ***
                        ; if it fails the previous test and your BIT # has set the V flag
                        ; see http://forum.6502.org/viewtopic.php?f=2&t=2241&p=27243#p27239
                        ; why it shouldn't alter N or V flags on a BIT #
1c26 : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1c27 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1c29 : 48              >            pha         ;use stack to load status
1c2a : a901            >            lda #1     ;precharge accu
1c2c : 28              >            plp
                        
1c2d : 8982                     bit #$82    ;82 - should set Z
                                tst_a 1,fz
1c2f : 08              >            php         ;save flags
1c30 : c901            >            cmp #1     ;test result
                       >            trap_ne
1c32 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c34 : 68              >            pla         ;load status
1c35 : 48              >            pha
                       >            cmp_flag fz
1c36 : c932            >            cmp #(fz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c38 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c3a : 28              >            plp         ;restore status
                        
1c3b : ca                       dex
                                set_a 1,0
                       >            load_flag 0
1c3c : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1c3e : 48              >            pha         ;use stack to load status
1c3f : a901            >            lda #1     ;precharge accu
1c41 : 28              >            plp
                        
1c42 : 89c3                     bit #$c3    ;c3 - should clear Z
                                tst_a 1,0
1c44 : 08              >            php         ;save flags
1c45 : c901            >            cmp #1     ;test result
                       >            trap_ne
1c47 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c49 : 68              >            pla         ;load status
1c4a : 48              >            pha
                       >            cmp_flag 0
1c4b : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c4d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c4f : 28              >            plp         ;restore status
                        
                                
                                set_a 1,$ff
                       >            load_flag $ff
1c50 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1c52 : 48              >            pha         ;use stack to load status
1c53 : a901            >            lda #1     ;precharge accu
1c55 : 28              >            plp
                        
1c56 : 89c3                     bit #$c3    ;c3 - clear Z
                                tst_a 1,~fz
1c58 : 08              >            php         ;save flags
1c59 : c901            >            cmp #1     ;test result
                       >            trap_ne
1c5b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c5d : 68              >            pla         ;load status
1c5e : 48              >            pha
                       >            cmp_flag ~fz
1c5f : c9fd            >            cmp #(~fz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c61 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c63 : 28              >            plp         ;restore status
                        
1c64 : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1c65 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1c67 : 48              >            pha         ;use stack to load status
1c68 : a901            >            lda #1     ;precharge accu
1c6a : 28              >            plp
                        
1c6b : 8982                     bit #$82    ;82 - should set Z
                                tst_a 1,$ff
1c6d : 08              >            php         ;save flags
1c6e : c901            >            cmp #1     ;test result
                       >            trap_ne
1c70 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c72 : 68              >            pla         ;load status
1c73 : 48              >            pha
                       >            cmp_flag $ff
1c74 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c76 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c78 : 28              >            plp         ;restore status
                        
1c79 : e8                       inx
                                set_a 1,$ff
                       >            load_flag $ff
1c7a : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1c7c : 48              >            pha         ;use stack to load status
1c7d : a901            >            lda #1     ;precharge accu
1c7f : 28              >            plp
                        
1c80 : 8941                     bit #$41    ;41 - should clear Z
                                tst_a 1,~fz
1c82 : 08              >            php         ;save flags
1c83 : c901            >            cmp #1     ;test result
                       >            trap_ne
1c85 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c87 : 68              >            pla         ;load status
1c88 : 48              >            pha
                       >            cmp_flag ~fz
1c89 : c9fd            >            cmp #(~fz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1c8b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c8d : 28              >            plp         ;restore status
                        
1c8e : e8                       inx
                                set_a $ff,$ff
                       >            load_flag $ff
1c8f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1c91 : 48              >            pha         ;use stack to load status
1c92 : a9ff            >            lda #$ff     ;precharge accu
1c94 : 28              >            plp
                        
1c95 : 8900                     bit #$00   ;00 - should set Z
                                tst_a $ff,$ff
1c97 : 08              >            php         ;save flags
1c98 : c9ff            >            cmp #$ff     ;test result
                       >            trap_ne
1c9a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1c9c : 68              >            pla         ;load status
1c9d : 48              >            pha
                       >            cmp_flag $ff
1c9e : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ca0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ca2 : 28              >            plp         ;restore status
                        
                                
1ca3 : e003                     cpx #3
                                trap_ne     ;x altered during test
1ca5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1ca7 : c042                     cpy #$42
                                trap_ne     ;y altered during test 
1ca9 : d0fe            >        bne *           ;failed not equal (non zero)
                        
1cab : ba                       tsx
1cac : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
1cae : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
1cb0 : ad0202          >            lda test_case   ;previous test
1cb3 : c90f            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
1cb5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0010 =                 >test_num = test_num + 1
1cb7 : a910            >            lda #test_num   ;*** next tests' number
1cb9 : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; testing TRB, TSB - zp / abs
                        
                        trbt    macro       ;\1 = memory, \2 = flags   
                                sty \1
                                load_flag \2
                                pha
                                lda zpt+1
                                plp
                                trb \1
                                php
                                cmp zpt+1
                                trap_ne     ;accu was changed
                                pla
                                pha
                                ora #fz     ;mask Z
                                cmp_flag \2|fz
                                trap_ne     ;flags changed except Z
                                pla
                                and #fz
                                cmp zpt+2
                                trap_ne     ;Z flag invalid
                                lda zpt+3
                                cmp zpt
                                trap_ne     ;altered bits in memory wrong       
                                endm
                        
                        tsbt    macro       ;\1 = memory, \2 = flags   
                                sty \1
                                load_flag \2
                                pha
                                lda zpt+1
                                plp
                                tsb \1
                                php
                                cmp zpt+1
                                trap_ne     ;accu was changed
                                pla
                                pha
                                ora #fz     ;mask Z
                                cmp_flag \2|fz
                                trap_ne     ;flags changed except Z
                                pla
                                and #fz
                                cmp zpt+2
                                trap_ne     ;Z flag invalid
                                lda zpt+4
                                cmp zpt
                                trap_ne     ;altered bits in memory wrong        
                                endm
                        
1cbc : a2c0                     ldx #$c0
1cbe : a000                     ldy #0      ;op1 - memory save
                                ;   zpt     ;op1 - memory modifiable
1cc0 : 640b                     stz zpt+1   ;op2 - accu
                                ;   zpt+2   ;and flags
                                ;   zpt+3   ;memory after reset
                                ;   zpt+4   ;memory after set
                                
1cc2 : 98               tbt1    tya
1cc3 : 250b                     and zpt+1   ;set Z by anding the 2 operands
1cc5 : 08                       php
1cc6 : 68                       pla
1cc7 : 2902                     and #fz     ;mask Z
1cc9 : 850c                     sta zpt+2
1ccb : 98                       tya         ;reset op1 bits by op2
1ccc : 49ff                     eor #$ff
1cce : 050b                     ora zpt+1
1cd0 : 49ff                     eor #$ff
1cd2 : 850d                     sta zpt+3
1cd4 : 98                       tya         ;set op1 bits by op2
1cd5 : 050b                     ora zpt+1
1cd7 : 850e                     sta zpt+4
                        
                                trbt zpt,$ff
1cd9 : 840a            >        sty zpt
                       >        load_flag $ff
1cdb : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1cdd : 48              >        pha
1cde : a50b            >        lda zpt+1
1ce0 : 28              >        plp
1ce1 : 140a            >        trb zpt
1ce3 : 08              >        php
1ce4 : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1ce6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ce8 : 68              >        pla
1ce9 : 48              >        pha
1cea : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag $ff|fz
1cec : c9ff            >            cmp #($ff|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1cee : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1cf0 : 68              >        pla
1cf1 : 2902            >        and #fz
1cf3 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1cf5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1cf7 : a50d            >        lda zpt+3
1cf9 : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong       
1cfb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                trbt abst,$ff
1cfd : 8c0502          >        sty abst
                       >        load_flag $ff
1d00 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1d02 : 48              >        pha
1d03 : a50b            >        lda zpt+1
1d05 : 28              >        plp
1d06 : 1c0502          >        trb abst
1d09 : 08              >        php
1d0a : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1d0c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d0e : 68              >        pla
1d0f : 48              >        pha
1d10 : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag $ff|fz
1d12 : c9ff            >            cmp #($ff|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1d14 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d16 : 68              >        pla
1d17 : 2902            >        and #fz
1d19 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1d1b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d1d : a50d            >        lda zpt+3
1d1f : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong       
1d21 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                trbt zpt,0
1d23 : 840a            >        sty zpt
                       >        load_flag 0
1d25 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1d27 : 48              >        pha
1d28 : a50b            >        lda zpt+1
1d2a : 28              >        plp
1d2b : 140a            >        trb zpt
1d2d : 08              >        php
1d2e : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1d30 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d32 : 68              >        pla
1d33 : 48              >        pha
1d34 : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag 0|fz
1d36 : c932            >            cmp #(0|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1d38 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d3a : 68              >        pla
1d3b : 2902            >        and #fz
1d3d : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1d3f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d41 : a50d            >        lda zpt+3
1d43 : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong       
1d45 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                trbt abst,0
1d47 : 8c0502          >        sty abst
                       >        load_flag 0
1d4a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1d4c : 48              >        pha
1d4d : a50b            >        lda zpt+1
1d4f : 28              >        plp
1d50 : 1c0502          >        trb abst
1d53 : 08              >        php
1d54 : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1d56 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d58 : 68              >        pla
1d59 : 48              >        pha
1d5a : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag 0|fz
1d5c : c932            >            cmp #(0|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1d5e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d60 : 68              >        pla
1d61 : 2902            >        and #fz
1d63 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1d65 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d67 : a50d            >        lda zpt+3
1d69 : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong       
1d6b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                tsbt zpt,$ff
1d6d : 840a            >        sty zpt
                       >        load_flag $ff
1d6f : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1d71 : 48              >        pha
1d72 : a50b            >        lda zpt+1
1d74 : 28              >        plp
1d75 : 040a            >        tsb zpt
1d77 : 08              >        php
1d78 : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1d7a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d7c : 68              >        pla
1d7d : 48              >        pha
1d7e : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag $ff|fz
1d80 : c9ff            >            cmp #($ff|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1d82 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d84 : 68              >        pla
1d85 : 2902            >        and #fz
1d87 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1d89 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1d8b : a50e            >        lda zpt+4
1d8d : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong        
1d8f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                tsbt abst,$ff
1d91 : 8c0502          >        sty abst
                       >        load_flag $ff
1d94 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1d96 : 48              >        pha
1d97 : a50b            >        lda zpt+1
1d99 : 28              >        plp
1d9a : 0c0502          >        tsb abst
1d9d : 08              >        php
1d9e : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1da0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1da2 : 68              >        pla
1da3 : 48              >        pha
1da4 : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag $ff|fz
1da6 : c9ff            >            cmp #($ff|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1da8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1daa : 68              >        pla
1dab : 2902            >        and #fz
1dad : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1daf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1db1 : a50e            >        lda zpt+4
1db3 : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong        
1db5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                tsbt zpt,0
1db7 : 840a            >        sty zpt
                       >        load_flag 0
1db9 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1dbb : 48              >        pha
1dbc : a50b            >        lda zpt+1
1dbe : 28              >        plp
1dbf : 040a            >        tsb zpt
1dc1 : 08              >        php
1dc2 : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1dc4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1dc6 : 68              >        pla
1dc7 : 48              >        pha
1dc8 : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag 0|fz
1dca : c932            >            cmp #(0|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1dcc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1dce : 68              >        pla
1dcf : 2902            >        and #fz
1dd1 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1dd3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1dd5 : a50e            >        lda zpt+4
1dd7 : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong        
1dd9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                tsbt abst,0
1ddb : 8c0502          >        sty abst
                       >        load_flag 0
1dde : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1de0 : 48              >        pha
1de1 : a50b            >        lda zpt+1
1de3 : 28              >        plp
1de4 : 0c0502          >        tsb abst
1de7 : 08              >        php
1de8 : c50b            >        cmp zpt+1
                       >        trap_ne     ;accu was changed
1dea : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1dec : 68              >        pla
1ded : 48              >        pha
1dee : 0902            >        ora #fz     ;mask Z
                       >        cmp_flag 0|fz
1df0 : c932            >            cmp #(0|fz|fao)&m8    ;expected flags + always on bits
                       >
                       >        trap_ne     ;flags changed except Z
1df2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1df4 : 68              >        pla
1df5 : 2902            >        and #fz
1df7 : c50c            >        cmp zpt+2
                       >        trap_ne     ;Z flag invalid
1df9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1dfb : a50e            >        lda zpt+4
1dfd : c50a            >        cmp zpt
                       >        trap_ne     ;altered bits in memory wrong        
1dff : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                
1e01 : c8                       iny         ;iterate op1
1e02 : d004                     bne tbt3
1e04 : e60b                     inc zpt+1   ;iterate op2
1e06 : f003                     beq tbt2
1e08 : 4cc21c           tbt3    jmp tbt1        
1e0b :                  tbt2
1e0b : e0c0                     cpx #$c0
                                trap_ne     ;x altered during test
1e0d : d0fe            >        bne *           ;failed not equal (non zero)
                        
1e0f : ba                       tsx
1e10 : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
1e12 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test    
1e14 : ad0202          >            lda test_case   ;previous test
1e17 : c910            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
1e19 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0011 =                 >test_num = test_num + 1
1e1b : a911            >            lda #test_num   ;*** next tests' number
1e1d : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                            if rkwl_wdc_op
                        ; testing RMB, SMB - zp
                        rmbt    macro       ;\1 = bitnum
                                lda #$ff
                                sta zpt
                                set_a $a5,0
                                rmb \1,zpt
                                tst_a $a5,0
                                lda zpt
                                cmp #$ff-(1<<\1)
                                trap_ne     ;wrong bits set or cleared
                                lda #1<<\1
                                sta zpt
                                set_a $5a,$ff
                                rmb \1,zpt
                                tst_a $5a,$ff
                                lda zpt
                                trap_ne     ;wrong bits set or cleared
                                endm
                        smbt    macro       ;\1 = bitnum
                                lda #$ff-(1<<\1)
                                sta zpt
                                set_a $a5,0
                                smb \1,zpt
                                tst_a $a5,0
                                lda zpt
                                cmp #$ff
                                trap_ne     ;wrong bits set or cleared
                                lda #0
                                sta zpt
                                set_a $5a,$ff
                                smb \1,zpt
                                tst_a $5a,$ff
                                lda zpt
                                cmp #1<<\1
                                trap_ne     ;wrong bits set or cleared
                                endm
                        
1e20 : a2ba                     ldx #$ba    ;protect x & y
1e22 : a0d0                     ldy #$d0
                                rmbt 0
1e24 : a9ff            >        lda #$ff
1e26 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1e28 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1e2a : 48              >            pha         ;use stack to load status
1e2b : a9a5            >            lda #$a5     ;precharge accu
1e2d : 28              >            plp
                       >
1e2e : 070a            >        rmb 0,zpt
                       >        tst_a $a5,0
1e30 : 08              >            php         ;save flags
1e31 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1e33 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e35 : 68              >            pla         ;load status
1e36 : 48              >            pha
                       >            cmp_flag 0
1e37 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1e39 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e3b : 28              >            plp         ;restore status
                       >
1e3c : a50a            >        lda zpt
1e3e : c9fe            >        cmp #$ff-(1<<0)
                       >        trap_ne     ;wrong bits set or cleared
1e40 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e42 : a901            >        lda #1<<0
1e44 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1e46 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1e48 : 48              >            pha         ;use stack to load status
1e49 : a95a            >            lda #$5a     ;precharge accu
1e4b : 28              >            plp
                       >
1e4c : 070a            >        rmb 0,zpt
                       >        tst_a $5a,$ff
1e4e : 08              >            php         ;save flags
1e4f : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1e51 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e53 : 68              >            pla         ;load status
1e54 : 48              >            pha
                       >            cmp_flag $ff
1e55 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1e57 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e59 : 28              >            plp         ;restore status
                       >
1e5a : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1e5c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 1
1e5e : a9ff            >        lda #$ff
1e60 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1e62 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1e64 : 48              >            pha         ;use stack to load status
1e65 : a9a5            >            lda #$a5     ;precharge accu
1e67 : 28              >            plp
                       >
1e68 : 170a            >        rmb 1,zpt
                       >        tst_a $a5,0
1e6a : 08              >            php         ;save flags
1e6b : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1e6d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e6f : 68              >            pla         ;load status
1e70 : 48              >            pha
                       >            cmp_flag 0
1e71 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1e73 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e75 : 28              >            plp         ;restore status
                       >
1e76 : a50a            >        lda zpt
1e78 : c9fd            >        cmp #$ff-(1<<1)
                       >        trap_ne     ;wrong bits set or cleared
1e7a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e7c : a902            >        lda #1<<1
1e7e : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1e80 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1e82 : 48              >            pha         ;use stack to load status
1e83 : a95a            >            lda #$5a     ;precharge accu
1e85 : 28              >            plp
                       >
1e86 : 170a            >        rmb 1,zpt
                       >        tst_a $5a,$ff
1e88 : 08              >            php         ;save flags
1e89 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1e8b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e8d : 68              >            pla         ;load status
1e8e : 48              >            pha
                       >            cmp_flag $ff
1e8f : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1e91 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1e93 : 28              >            plp         ;restore status
                       >
1e94 : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1e96 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 2
1e98 : a9ff            >        lda #$ff
1e9a : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1e9c : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1e9e : 48              >            pha         ;use stack to load status
1e9f : a9a5            >            lda #$a5     ;precharge accu
1ea1 : 28              >            plp
                       >
1ea2 : 270a            >        rmb 2,zpt
                       >        tst_a $a5,0
1ea4 : 08              >            php         ;save flags
1ea5 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1ea7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ea9 : 68              >            pla         ;load status
1eaa : 48              >            pha
                       >            cmp_flag 0
1eab : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ead : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1eaf : 28              >            plp         ;restore status
                       >
1eb0 : a50a            >        lda zpt
1eb2 : c9fb            >        cmp #$ff-(1<<2)
                       >        trap_ne     ;wrong bits set or cleared
1eb4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1eb6 : a904            >        lda #1<<2
1eb8 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1eba : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1ebc : 48              >            pha         ;use stack to load status
1ebd : a95a            >            lda #$5a     ;precharge accu
1ebf : 28              >            plp
                       >
1ec0 : 270a            >        rmb 2,zpt
                       >        tst_a $5a,$ff
1ec2 : 08              >            php         ;save flags
1ec3 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1ec5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ec7 : 68              >            pla         ;load status
1ec8 : 48              >            pha
                       >            cmp_flag $ff
1ec9 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ecb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ecd : 28              >            plp         ;restore status
                       >
1ece : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1ed0 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 3
1ed2 : a9ff            >        lda #$ff
1ed4 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1ed6 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1ed8 : 48              >            pha         ;use stack to load status
1ed9 : a9a5            >            lda #$a5     ;precharge accu
1edb : 28              >            plp
                       >
1edc : 370a            >        rmb 3,zpt
                       >        tst_a $a5,0
1ede : 08              >            php         ;save flags
1edf : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1ee1 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ee3 : 68              >            pla         ;load status
1ee4 : 48              >            pha
                       >            cmp_flag 0
1ee5 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1ee7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ee9 : 28              >            plp         ;restore status
                       >
1eea : a50a            >        lda zpt
1eec : c9f7            >        cmp #$ff-(1<<3)
                       >        trap_ne     ;wrong bits set or cleared
1eee : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1ef0 : a908            >        lda #1<<3
1ef2 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1ef4 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1ef6 : 48              >            pha         ;use stack to load status
1ef7 : a95a            >            lda #$5a     ;precharge accu
1ef9 : 28              >            plp
                       >
1efa : 370a            >        rmb 3,zpt
                       >        tst_a $5a,$ff
1efc : 08              >            php         ;save flags
1efd : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1eff : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f01 : 68              >            pla         ;load status
1f02 : 48              >            pha
                       >            cmp_flag $ff
1f03 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f05 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f07 : 28              >            plp         ;restore status
                       >
1f08 : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1f0a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 4
1f0c : a9ff            >        lda #$ff
1f0e : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1f10 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1f12 : 48              >            pha         ;use stack to load status
1f13 : a9a5            >            lda #$a5     ;precharge accu
1f15 : 28              >            plp
                       >
1f16 : 470a            >        rmb 4,zpt
                       >        tst_a $a5,0
1f18 : 08              >            php         ;save flags
1f19 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1f1b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f1d : 68              >            pla         ;load status
1f1e : 48              >            pha
                       >            cmp_flag 0
1f1f : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f21 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f23 : 28              >            plp         ;restore status
                       >
1f24 : a50a            >        lda zpt
1f26 : c9ef            >        cmp #$ff-(1<<4)
                       >        trap_ne     ;wrong bits set or cleared
1f28 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f2a : a910            >        lda #1<<4
1f2c : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1f2e : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1f30 : 48              >            pha         ;use stack to load status
1f31 : a95a            >            lda #$5a     ;precharge accu
1f33 : 28              >            plp
                       >
1f34 : 470a            >        rmb 4,zpt
                       >        tst_a $5a,$ff
1f36 : 08              >            php         ;save flags
1f37 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1f39 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f3b : 68              >            pla         ;load status
1f3c : 48              >            pha
                       >            cmp_flag $ff
1f3d : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f3f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f41 : 28              >            plp         ;restore status
                       >
1f42 : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1f44 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 5
1f46 : a9ff            >        lda #$ff
1f48 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1f4a : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1f4c : 48              >            pha         ;use stack to load status
1f4d : a9a5            >            lda #$a5     ;precharge accu
1f4f : 28              >            plp
                       >
1f50 : 570a            >        rmb 5,zpt
                       >        tst_a $a5,0
1f52 : 08              >            php         ;save flags
1f53 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1f55 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f57 : 68              >            pla         ;load status
1f58 : 48              >            pha
                       >            cmp_flag 0
1f59 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f5b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f5d : 28              >            plp         ;restore status
                       >
1f5e : a50a            >        lda zpt
1f60 : c9df            >        cmp #$ff-(1<<5)
                       >        trap_ne     ;wrong bits set or cleared
1f62 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f64 : a920            >        lda #1<<5
1f66 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1f68 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1f6a : 48              >            pha         ;use stack to load status
1f6b : a95a            >            lda #$5a     ;precharge accu
1f6d : 28              >            plp
                       >
1f6e : 570a            >        rmb 5,zpt
                       >        tst_a $5a,$ff
1f70 : 08              >            php         ;save flags
1f71 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1f73 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f75 : 68              >            pla         ;load status
1f76 : 48              >            pha
                       >            cmp_flag $ff
1f77 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f79 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f7b : 28              >            plp         ;restore status
                       >
1f7c : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1f7e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 6
1f80 : a9ff            >        lda #$ff
1f82 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1f84 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1f86 : 48              >            pha         ;use stack to load status
1f87 : a9a5            >            lda #$a5     ;precharge accu
1f89 : 28              >            plp
                       >
1f8a : 670a            >        rmb 6,zpt
                       >        tst_a $a5,0
1f8c : 08              >            php         ;save flags
1f8d : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1f8f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f91 : 68              >            pla         ;load status
1f92 : 48              >            pha
                       >            cmp_flag 0
1f93 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1f95 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f97 : 28              >            plp         ;restore status
                       >
1f98 : a50a            >        lda zpt
1f9a : c9bf            >        cmp #$ff-(1<<6)
                       >        trap_ne     ;wrong bits set or cleared
1f9c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1f9e : a940            >        lda #1<<6
1fa0 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1fa2 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1fa4 : 48              >            pha         ;use stack to load status
1fa5 : a95a            >            lda #$5a     ;precharge accu
1fa7 : 28              >            plp
                       >
1fa8 : 670a            >        rmb 6,zpt
                       >        tst_a $5a,$ff
1faa : 08              >            php         ;save flags
1fab : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1fad : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1faf : 68              >            pla         ;load status
1fb0 : 48              >            pha
                       >            cmp_flag $ff
1fb1 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1fb3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fb5 : 28              >            plp         ;restore status
                       >
1fb6 : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1fb8 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                rmbt 7
1fba : a9ff            >        lda #$ff
1fbc : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1fbe : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1fc0 : 48              >            pha         ;use stack to load status
1fc1 : a9a5            >            lda #$a5     ;precharge accu
1fc3 : 28              >            plp
                       >
1fc4 : 770a            >        rmb 7,zpt
                       >        tst_a $a5,0
1fc6 : 08              >            php         ;save flags
1fc7 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
1fc9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fcb : 68              >            pla         ;load status
1fcc : 48              >            pha
                       >            cmp_flag 0
1fcd : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1fcf : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fd1 : 28              >            plp         ;restore status
                       >
1fd2 : a50a            >        lda zpt
1fd4 : c97f            >        cmp #$ff-(1<<7)
                       >        trap_ne     ;wrong bits set or cleared
1fd6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fd8 : a980            >        lda #1<<7
1fda : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
1fdc : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
1fde : 48              >            pha         ;use stack to load status
1fdf : a95a            >            lda #$5a     ;precharge accu
1fe1 : 28              >            plp
                       >
1fe2 : 770a            >        rmb 7,zpt
                       >        tst_a $5a,$ff
1fe4 : 08              >            php         ;save flags
1fe5 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
1fe7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fe9 : 68              >            pla         ;load status
1fea : 48              >            pha
                       >            cmp_flag $ff
1feb : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
1fed : d0fe            >        bne *           ;failed not equal (non zero)
                       >
1fef : 28              >            plp         ;restore status
                       >
1ff0 : a50a            >        lda zpt
                       >        trap_ne     ;wrong bits set or cleared
1ff2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 0
1ff4 : a9fe            >        lda #$ff-(1<<0)
1ff6 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
1ff8 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
1ffa : 48              >            pha         ;use stack to load status
1ffb : a9a5            >            lda #$a5     ;precharge accu
1ffd : 28              >            plp
                       >
1ffe : 870a            >        smb 0,zpt
                       >        tst_a $a5,0
2000 : 08              >            php         ;save flags
2001 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
2003 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2005 : 68              >            pla         ;load status
2006 : 48              >            pha
                       >            cmp_flag 0
2007 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2009 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
200b : 28              >            plp         ;restore status
                       >
200c : a50a            >        lda zpt
200e : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
2010 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2012 : a900            >        lda #0
2014 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
2016 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2018 : 48              >            pha         ;use stack to load status
2019 : a95a            >            lda #$5a     ;precharge accu
201b : 28              >            plp
                       >
201c : 870a            >        smb 0,zpt
                       >        tst_a $5a,$ff
201e : 08              >            php         ;save flags
201f : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
2021 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2023 : 68              >            pla         ;load status
2024 : 48              >            pha
                       >            cmp_flag $ff
2025 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2027 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2029 : 28              >            plp         ;restore status
                       >
202a : a50a            >        lda zpt
202c : c901            >        cmp #1<<0
                       >        trap_ne     ;wrong bits set or cleared
202e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 1
2030 : a9fd            >        lda #$ff-(1<<1)
2032 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
2034 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2036 : 48              >            pha         ;use stack to load status
2037 : a9a5            >            lda #$a5     ;precharge accu
2039 : 28              >            plp
                       >
203a : 970a            >        smb 1,zpt
                       >        tst_a $a5,0
203c : 08              >            php         ;save flags
203d : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
203f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2041 : 68              >            pla         ;load status
2042 : 48              >            pha
                       >            cmp_flag 0
2043 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2045 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2047 : 28              >            plp         ;restore status
                       >
2048 : a50a            >        lda zpt
204a : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
204c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
204e : a900            >        lda #0
2050 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
2052 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2054 : 48              >            pha         ;use stack to load status
2055 : a95a            >            lda #$5a     ;precharge accu
2057 : 28              >            plp
                       >
2058 : 970a            >        smb 1,zpt
                       >        tst_a $5a,$ff
205a : 08              >            php         ;save flags
205b : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
205d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
205f : 68              >            pla         ;load status
2060 : 48              >            pha
                       >            cmp_flag $ff
2061 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2063 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2065 : 28              >            plp         ;restore status
                       >
2066 : a50a            >        lda zpt
2068 : c902            >        cmp #1<<1
                       >        trap_ne     ;wrong bits set or cleared
206a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 2
206c : a9fb            >        lda #$ff-(1<<2)
206e : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
2070 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2072 : 48              >            pha         ;use stack to load status
2073 : a9a5            >            lda #$a5     ;precharge accu
2075 : 28              >            plp
                       >
2076 : a70a            >        smb 2,zpt
                       >        tst_a $a5,0
2078 : 08              >            php         ;save flags
2079 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
207b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
207d : 68              >            pla         ;load status
207e : 48              >            pha
                       >            cmp_flag 0
207f : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2081 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2083 : 28              >            plp         ;restore status
                       >
2084 : a50a            >        lda zpt
2086 : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
2088 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
208a : a900            >        lda #0
208c : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
208e : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2090 : 48              >            pha         ;use stack to load status
2091 : a95a            >            lda #$5a     ;precharge accu
2093 : 28              >            plp
                       >
2094 : a70a            >        smb 2,zpt
                       >        tst_a $5a,$ff
2096 : 08              >            php         ;save flags
2097 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
2099 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
209b : 68              >            pla         ;load status
209c : 48              >            pha
                       >            cmp_flag $ff
209d : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
209f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20a1 : 28              >            plp         ;restore status
                       >
20a2 : a50a            >        lda zpt
20a4 : c904            >        cmp #1<<2
                       >        trap_ne     ;wrong bits set or cleared
20a6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 3
20a8 : a9f7            >        lda #$ff-(1<<3)
20aa : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
20ac : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
20ae : 48              >            pha         ;use stack to load status
20af : a9a5            >            lda #$a5     ;precharge accu
20b1 : 28              >            plp
                       >
20b2 : b70a            >        smb 3,zpt
                       >        tst_a $a5,0
20b4 : 08              >            php         ;save flags
20b5 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
20b7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20b9 : 68              >            pla         ;load status
20ba : 48              >            pha
                       >            cmp_flag 0
20bb : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
20bd : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20bf : 28              >            plp         ;restore status
                       >
20c0 : a50a            >        lda zpt
20c2 : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
20c4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20c6 : a900            >        lda #0
20c8 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
20ca : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
20cc : 48              >            pha         ;use stack to load status
20cd : a95a            >            lda #$5a     ;precharge accu
20cf : 28              >            plp
                       >
20d0 : b70a            >        smb 3,zpt
                       >        tst_a $5a,$ff
20d2 : 08              >            php         ;save flags
20d3 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
20d5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20d7 : 68              >            pla         ;load status
20d8 : 48              >            pha
                       >            cmp_flag $ff
20d9 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
20db : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20dd : 28              >            plp         ;restore status
                       >
20de : a50a            >        lda zpt
20e0 : c908            >        cmp #1<<3
                       >        trap_ne     ;wrong bits set or cleared
20e2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 4
20e4 : a9ef            >        lda #$ff-(1<<4)
20e6 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
20e8 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
20ea : 48              >            pha         ;use stack to load status
20eb : a9a5            >            lda #$a5     ;precharge accu
20ed : 28              >            plp
                       >
20ee : c70a            >        smb 4,zpt
                       >        tst_a $a5,0
20f0 : 08              >            php         ;save flags
20f1 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
20f3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20f5 : 68              >            pla         ;load status
20f6 : 48              >            pha
                       >            cmp_flag 0
20f7 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
20f9 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
20fb : 28              >            plp         ;restore status
                       >
20fc : a50a            >        lda zpt
20fe : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
2100 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2102 : a900            >        lda #0
2104 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
2106 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2108 : 48              >            pha         ;use stack to load status
2109 : a95a            >            lda #$5a     ;precharge accu
210b : 28              >            plp
                       >
210c : c70a            >        smb 4,zpt
                       >        tst_a $5a,$ff
210e : 08              >            php         ;save flags
210f : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
2111 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2113 : 68              >            pla         ;load status
2114 : 48              >            pha
                       >            cmp_flag $ff
2115 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2117 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2119 : 28              >            plp         ;restore status
                       >
211a : a50a            >        lda zpt
211c : c910            >        cmp #1<<4
                       >        trap_ne     ;wrong bits set or cleared
211e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 5
2120 : a9df            >        lda #$ff-(1<<5)
2122 : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
2124 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2126 : 48              >            pha         ;use stack to load status
2127 : a9a5            >            lda #$a5     ;precharge accu
2129 : 28              >            plp
                       >
212a : d70a            >        smb 5,zpt
                       >        tst_a $a5,0
212c : 08              >            php         ;save flags
212d : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
212f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2131 : 68              >            pla         ;load status
2132 : 48              >            pha
                       >            cmp_flag 0
2133 : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2135 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2137 : 28              >            plp         ;restore status
                       >
2138 : a50a            >        lda zpt
213a : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
213c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
213e : a900            >        lda #0
2140 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
2142 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2144 : 48              >            pha         ;use stack to load status
2145 : a95a            >            lda #$5a     ;precharge accu
2147 : 28              >            plp
                       >
2148 : d70a            >        smb 5,zpt
                       >        tst_a $5a,$ff
214a : 08              >            php         ;save flags
214b : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
214d : d0fe            >        bne *           ;failed not equal (non zero)
                       >
214f : 68              >            pla         ;load status
2150 : 48              >            pha
                       >            cmp_flag $ff
2151 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2153 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2155 : 28              >            plp         ;restore status
                       >
2156 : a50a            >        lda zpt
2158 : c920            >        cmp #1<<5
                       >        trap_ne     ;wrong bits set or cleared
215a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 6
215c : a9bf            >        lda #$ff-(1<<6)
215e : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
2160 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2162 : 48              >            pha         ;use stack to load status
2163 : a9a5            >            lda #$a5     ;precharge accu
2165 : 28              >            plp
                       >
2166 : e70a            >        smb 6,zpt
                       >        tst_a $a5,0
2168 : 08              >            php         ;save flags
2169 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
216b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
216d : 68              >            pla         ;load status
216e : 48              >            pha
                       >            cmp_flag 0
216f : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2171 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2173 : 28              >            plp         ;restore status
                       >
2174 : a50a            >        lda zpt
2176 : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
2178 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
217a : a900            >        lda #0
217c : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
217e : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2180 : 48              >            pha         ;use stack to load status
2181 : a95a            >            lda #$5a     ;precharge accu
2183 : 28              >            plp
                       >
2184 : e70a            >        smb 6,zpt
                       >        tst_a $5a,$ff
2186 : 08              >            php         ;save flags
2187 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
2189 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
218b : 68              >            pla         ;load status
218c : 48              >            pha
                       >            cmp_flag $ff
218d : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
218f : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2191 : 28              >            plp         ;restore status
                       >
2192 : a50a            >        lda zpt
2194 : c940            >        cmp #1<<6
                       >        trap_ne     ;wrong bits set or cleared
2196 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
                                smbt 7
2198 : a97f            >        lda #$ff-(1<<7)
219a : 850a            >        sta zpt
                       >        set_a $a5,0
                       >            load_flag 0
219c : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
219e : 48              >            pha         ;use stack to load status
219f : a9a5            >            lda #$a5     ;precharge accu
21a1 : 28              >            plp
                       >
21a2 : f70a            >        smb 7,zpt
                       >        tst_a $a5,0
21a4 : 08              >            php         ;save flags
21a5 : c9a5            >            cmp #$a5     ;test result
                       >            trap_ne
21a7 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21a9 : 68              >            pla         ;load status
21aa : 48              >            pha
                       >            cmp_flag 0
21ab : c930            >            cmp #(0|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
21ad : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21af : 28              >            plp         ;restore status
                       >
21b0 : a50a            >        lda zpt
21b2 : c9ff            >        cmp #$ff
                       >        trap_ne     ;wrong bits set or cleared
21b4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21b6 : a900            >        lda #0
21b8 : 850a            >        sta zpt
                       >        set_a $5a,$ff
                       >            load_flag $ff
21ba : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
21bc : 48              >            pha         ;use stack to load status
21bd : a95a            >            lda #$5a     ;precharge accu
21bf : 28              >            plp
                       >
21c0 : f70a            >        smb 7,zpt
                       >        tst_a $5a,$ff
21c2 : 08              >            php         ;save flags
21c3 : c95a            >            cmp #$5a     ;test result
                       >            trap_ne
21c5 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21c7 : 68              >            pla         ;load status
21c8 : 48              >            pha
                       >            cmp_flag $ff
21c9 : c9ff            >            cmp #($ff|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
21cb : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21cd : 28              >            plp         ;restore status
                       >
21ce : a50a            >        lda zpt
21d0 : c980            >        cmp #1<<7
                       >        trap_ne     ;wrong bits set or cleared
21d2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
21d4 : e0ba                     cpx #$ba
                                trap_ne     ;x altered during test
21d6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
21d8 : c0d0                     cpy #$d0
                                trap_ne     ;y altered during test
21da : d0fe            >        bne *           ;failed not equal (non zero)
                        
21dc : ba                       tsx
21dd : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
21df : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
21e1 : ad0202          >            lda test_case   ;previous test
21e4 : c911            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
21e6 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0012 =                 >test_num = test_num + 1
21e8 : a912            >            lda #test_num   ;*** next tests' number
21ea : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                            endif        
                                 
                        ; testing CMP - (zp)         
21ed : a2de                     ldx #$de    ;protect x & y
21ef : a0ad                     ldy #$ad
                                set_a $80,0
                       >            load_flag 0
21f1 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
21f3 : 48              >            pha         ;use stack to load status
21f4 : a980            >            lda #$80     ;precharge accu
21f6 : 28              >            plp
                        
21f7 : d22a                     cmp (ind1+8)
                                tst_a $80,fc
21f9 : 08              >            php         ;save flags
21fa : c980            >            cmp #$80     ;test result
                       >            trap_ne
21fc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
21fe : 68              >            pla         ;load status
21ff : 48              >            pha
                       >            cmp_flag fc
2200 : c931            >            cmp #(fc|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2202 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2204 : 28              >            plp         ;restore status
                        
                                set_a $7f,0
                       >            load_flag 0
2205 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2207 : 48              >            pha         ;use stack to load status
2208 : a97f            >            lda #$7f     ;precharge accu
220a : 28              >            plp
                        
220b : d22a                     cmp (ind1+8)
                                tst_a $7f,fzc
220d : 08              >            php         ;save flags
220e : c97f            >            cmp #$7f     ;test result
                       >            trap_ne
2210 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2212 : 68              >            pla         ;load status
2213 : 48              >            pha
                       >            cmp_flag fzc
2214 : c933            >            cmp #(fzc|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2216 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2218 : 28              >            plp         ;restore status
                        
                                set_a $7e,0
                       >            load_flag 0
2219 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
221b : 48              >            pha         ;use stack to load status
221c : a97e            >            lda #$7e     ;precharge accu
221e : 28              >            plp
                        
221f : d22a                     cmp (ind1+8)
                                tst_a $7e,fn
2221 : 08              >            php         ;save flags
2222 : c97e            >            cmp #$7e     ;test result
                       >            trap_ne
2224 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2226 : 68              >            pla         ;load status
2227 : 48              >            pha
                       >            cmp_flag fn
2228 : c9b0            >            cmp #(fn|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
222a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
222c : 28              >            plp         ;restore status
                        
                                set_a $80,$ff
                       >            load_flag $ff
222d : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
222f : 48              >            pha         ;use stack to load status
2230 : a980            >            lda #$80     ;precharge accu
2232 : 28              >            plp
                        
2233 : d22a                     cmp (ind1+8)
                                tst_a $80,~fnz
2235 : 08              >            php         ;save flags
2236 : c980            >            cmp #$80     ;test result
                       >            trap_ne
2238 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
223a : 68              >            pla         ;load status
223b : 48              >            pha
                       >            cmp_flag ~fnz
223c : c97d            >            cmp #(~fnz|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
223e : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2240 : 28              >            plp         ;restore status
                        
                                set_a $7f,$ff
                       >            load_flag $ff
2241 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2243 : 48              >            pha         ;use stack to load status
2244 : a97f            >            lda #$7f     ;precharge accu
2246 : 28              >            plp
                        
2247 : d22a                     cmp (ind1+8)
                                tst_a $7f,~fn
2249 : 08              >            php         ;save flags
224a : c97f            >            cmp #$7f     ;test result
                       >            trap_ne
224c : d0fe            >        bne *           ;failed not equal (non zero)
                       >
224e : 68              >            pla         ;load status
224f : 48              >            pha
                       >            cmp_flag ~fn
2250 : c97f            >            cmp #(~fn|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2252 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2254 : 28              >            plp         ;restore status
                        
                                set_a $7e,$ff
                       >            load_flag $ff
2255 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
2257 : 48              >            pha         ;use stack to load status
2258 : a97e            >            lda #$7e     ;precharge accu
225a : 28              >            plp
                        
225b : d22a                     cmp (ind1+8)
                                tst_a $7e,~fzc
225d : 08              >            php         ;save flags
225e : c97e            >            cmp #$7e     ;test result
                       >            trap_ne
2260 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2262 : 68              >            pla         ;load status
2263 : 48              >            pha
                       >            cmp_flag ~fzc
2264 : c9fc            >            cmp #(~fzc|fao)&m8    ;expected flags + always on bits
                       >
                       >            trap_ne
2266 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2268 : 28              >            plp         ;restore status
                        
2269 : e0de                     cpx #$de
                                trap_ne     ;x altered during test
226b : d0fe            >        bne *           ;failed not equal (non zero)
                        
226d : c0ad                     cpy #$ad
                                trap_ne     ;y altered during test 
226f : d0fe            >        bne *           ;failed not equal (non zero)
                        
2271 : ba                       tsx
2272 : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
2274 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
2276 : ad0202          >            lda test_case   ;previous test
2279 : c912            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
227b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0013 =                 >test_num = test_num + 1
227d : a913            >            lda #test_num   ;*** next tests' number
227f : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; testing logical instructions - AND EOR ORA (zp)
2282 : a242                     ldx #$42    ;protect x & y
                        
2284 : a000                     ldy #0      ;AND
2286 : a538                     lda indAN   ;set indirect address
2288 : 850a                     sta zpt
228a : a539                     lda indAN+1
228c : 850b                     sta zpt+1
228e :                  tand1
                                set_ay  absANa,0
                       >            load_flag 0
228e : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
2290 : 48              >            pha         ;use stack to load status
2291 : b94d02          >            lda absANa,y    ;precharge accu
2294 : 28              >            plp
                        
2295 : 320a                     and (zpt)
                                tst_ay  absrlo,absflo,0
2297 : 08              >            php         ;save flags
2298 : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
229b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
229d : 68              >            pla         ;load status
                       >            eor_flag 0
229e : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                       >
22a0 : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
22a3 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
22a5 : e60a                     inc zpt
22a7 : c8                       iny
22a8 : c004                     cpy #4
22aa : d0e2                     bne tand1
22ac : 88                       dey
22ad : c60a                     dec zpt
22af :                  tand2
                                set_ay  absANa,$ff
                       >            load_flag $ff
22af : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
22b1 : 48              >            pha         ;use stack to load status
22b2 : b94d02          >            lda absANa,y    ;precharge accu
22b5 : 28              >            plp
                        
22b6 : 320a                     and (zpt)
                                tst_ay  absrlo,absflo,$ff-fnz
22b8 : 08              >            php         ;save flags
22b9 : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
22bc : d0fe            >        bne *           ;failed not equal (non zero)
                       >
22be : 68              >            pla         ;load status
                       >            eor_flag $ff-fnz
22bf : 497d            >            eor #$ff-fnz|fao         ;invert expected flags + always on bits
                       >
22c1 : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
22c4 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
22c6 : c60a                     dec zpt
22c8 : 88                       dey
22c9 : 10e4                     bpl tand2
                        
22cb : a000                     ldy #0      ;EOR
22cd : a540                     lda indEO   ;set indirect address
22cf : 850a                     sta zpt
22d1 : a541                     lda indEO+1
22d3 : 850b                     sta zpt+1
22d5 :                  teor1
                                set_ay  absEOa,0
                       >            load_flag 0
22d5 : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
22d7 : 48              >            pha         ;use stack to load status
22d8 : b95102          >            lda absEOa,y    ;precharge accu
22db : 28              >            plp
                        
22dc : 520a                     eor (zpt)
                                tst_ay  absrlo,absflo,0
22de : 08              >            php         ;save flags
22df : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
22e2 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
22e4 : 68              >            pla         ;load status
                       >            eor_flag 0
22e5 : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                       >
22e7 : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
22ea : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
22ec : e60a                     inc zpt
22ee : c8                       iny
22ef : c004                     cpy #4
22f1 : d0e2                     bne teor1
22f3 : 88                       dey
22f4 : c60a                     dec zpt
22f6 :                  teor2
                                set_ay  absEOa,$ff
                       >            load_flag $ff
22f6 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
22f8 : 48              >            pha         ;use stack to load status
22f9 : b95102          >            lda absEOa,y    ;precharge accu
22fc : 28              >            plp
                        
22fd : 520a                     eor (zpt)
                                tst_ay  absrlo,absflo,$ff-fnz
22ff : 08              >            php         ;save flags
2300 : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
2303 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
2305 : 68              >            pla         ;load status
                       >            eor_flag $ff-fnz
2306 : 497d            >            eor #$ff-fnz|fao         ;invert expected flags + always on bits
                       >
2308 : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
230b : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
230d : c60a                     dec zpt
230f : 88                       dey
2310 : 10e4                     bpl teor2
                        
2312 : a000                     ldy #0      ;ORA
2314 : a548                     lda indOR   ;set indirect address
2316 : 850a                     sta zpt
2318 : a549                     lda indOR+1
231a : 850b                     sta zpt+1
231c :                  tora1
                                set_ay  absORa,0
                       >            load_flag 0
231c : a900            >            lda #0             ;allow test to change I-flag (no mask)
                       >
231e : 48              >            pha         ;use stack to load status
231f : b94902          >            lda absORa,y    ;precharge accu
2322 : 28              >            plp
                        
2323 : 120a                     ora (zpt)
                                tst_ay  absrlo,absflo,0
2325 : 08              >            php         ;save flags
2326 : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
2329 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
232b : 68              >            pla         ;load status
                       >            eor_flag 0
232c : 4930            >            eor #0|fao         ;invert expected flags + always on bits
                       >
232e : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
2331 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
2333 : e60a                     inc zpt
2335 : c8                       iny
2336 : c004                     cpy #4
2338 : d0e2                     bne tora1
233a : 88                       dey
233b : c60a                     dec zpt
233d :                  tora2
                                set_ay  absORa,$ff
                       >            load_flag $ff
233d : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
233f : 48              >            pha         ;use stack to load status
2340 : b94902          >            lda absORa,y    ;precharge accu
2343 : 28              >            plp
                        
2344 : 120a                     ora (zpt)
                                tst_ay  absrlo,absflo,$ff-fnz
2346 : 08              >            php         ;save flags
2347 : d95502          >            cmp absrlo,y    ;test result
                       >            trap_ne     ;
234a : d0fe            >        bne *           ;failed not equal (non zero)
                       >
234c : 68              >            pla         ;load status
                       >            eor_flag $ff-fnz
234d : 497d            >            eor #$ff-fnz|fao         ;invert expected flags + always on bits
                       >
234f : d95902          >            cmp absflo,y    ;test flags
                       >            trap_ne
2352 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
                        
2354 : c60a                     dec zpt
2356 : 88                       dey
2357 : 10e4                     bpl tora2
                        
2359 : e042                     cpx #$42
                                trap_ne     ;x altered during test
235b : d0fe            >        bne *           ;failed not equal (non zero)
                        
235d : ba                       tsx
235e : e0ff                     cpx #$ff
                                trap_ne     ;sp push/pop mismatch
2360 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
2362 : ad0202          >            lda test_case   ;previous test
2365 : c913            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
2367 : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0014 =                 >test_num = test_num + 1
2369 : a914            >            lda #test_num   ;*** next tests' number
236b : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                                
                            if I_flag = 3
236e : 58                       cli
                            endif                
                        
                        ; full binary add/subtract test - (zp) only
                        ; iterates through all combinations of operands and carry input
                        ; uses increments/decrements to predict result & result flags
236f : d8                       cld
2370 : a20c                     ldx #ad2        ;for indexed test
2372 : a0ff                     ldy #$ff        ;max range
2374 : a900                     lda #0          ;start with adding zeroes & no carry
2376 : 850a                     sta adfc        ;carry in - for diag
2378 : 850b                     sta ad1         ;operand 1 - accumulator
237a : 850c                     sta ad2         ;operand 2 - memory or immediate
237c : 8d0502                   sta ada2        ;non zp
237f : 850d                     sta adrl        ;expected result bits 0-7
2381 : 850e                     sta adrh        ;expected result bit 8 (carry out)
2383 : a9ff                     lda #$ff        ;complemented operand 2 for subtract
2385 : 8510                     sta sb2
2387 : 8d0602                   sta sba2        ;non zp
238a : a902                     lda #2          ;expected Z-flag
238c : 850f                     sta adrf
238e : 18               tadd    clc             ;test with carry clear
238f : 200326                   jsr chkadd
2392 : e60a                     inc adfc        ;now with carry
2394 : e60d                     inc adrl        ;result +1
2396 : 08                       php             ;save N & Z from low result
2397 : 08                       php
2398 : 68                       pla             ;accu holds expected flags
2399 : 2982                     and #$82        ;mask N & Z
239b : 28                       plp
239c : d002                     bne tadd1
239e : e60e                     inc adrh        ;result bit 8 - carry
23a0 : 050e             tadd1   ora adrh        ;merge C to expected flags
23a2 : 850f                     sta adrf        ;save expected flags except overflow
23a4 : 38                       sec             ;test with carry set
23a5 : 200326                   jsr chkadd
23a8 : c60a                     dec adfc        ;same for operand +1 but no carry
23aa : e60b                     inc ad1
23ac : d0e0                     bne tadd        ;iterate op1
23ae : a900                     lda #0          ;preset result to op2 when op1 = 0
23b0 : 850e                     sta adrh
23b2 : ee0502                   inc ada2
23b5 : e60c                     inc ad2
23b7 : 08                       php             ;save NZ as operand 2 becomes the new result
23b8 : 68                       pla
23b9 : 2982                     and #$82        ;mask N00000Z0
23bb : 850f                     sta adrf        ;no need to check carry as we are adding to 0
23bd : c610                     dec sb2         ;complement subtract operand 2
23bf : ce0602                   dec sba2
23c2 : a50c                     lda ad2         
23c4 : 850d                     sta adrl
23c6 : d0c6                     bne tadd        ;iterate op2
                        
23c8 : e00c                     cpx #ad2
                                trap_ne         ;x altered during test
23ca : d0fe            >        bne *           ;failed not equal (non zero)
                        
23cc : c0ff                     cpy #$ff
                                trap_ne         ;y altered during test 
23ce : d0fe            >        bne *           ;failed not equal (non zero)
                        
23d0 : ba                       tsx
23d1 : e0ff                     cpx #$ff
                                trap_ne         ;sp push/pop mismatch
23d3 : d0fe            >        bne *           ;failed not equal (non zero)
                        
                                next_test
23d5 : ad0202          >            lda test_case   ;previous test
23d8 : c914            >            cmp #test_num
                       >            trap_ne         ;test is out of sequence
23da : d0fe            >        bne *           ;failed not equal (non zero)
                       >
0015 =                 >test_num = test_num + 1
23dc : a915            >            lda #test_num   ;*** next tests' number
23de : 8d0202          >            sta test_case
                       >            ;check_ram       ;uncomment to find altered RAM after each test
                        
                        
                        ; decimal add/subtract test
                        ; *** WARNING - tests documented behavior only! ***
                        ;   only valid BCD operands are tested, the V flag is ignored
                        ;   although V is declared as beeing valid on the 65C02 it has absolutely
                        ;   no use in BCD math. No sign = no overflow!
                        ; iterates through all valid combinations of operands and carry input
                        ; uses increments/decrements to predict result & carry flag
23e1 : f8                       sed 
23e2 : a20c                     ldx #ad2        ;for indexed test
23e4 : a0ff                     ldy #$ff        ;max range
23e6 : a999                     lda #$99        ;start with adding 99 to 99 with carry
23e8 : 850b                     sta ad1         ;operand 1 - accumulator
23ea : 850c                     sta ad2         ;operand 2 - memory or immediate
23ec : 8d0502                   sta ada2        ;non zp
23ef : 850d                     sta adrl        ;expected result bits 0-7
23f1 : a901                     lda #1          ;set carry in & out
23f3 : 850a                     sta adfc        ;carry in - for diag
23f5 : 850e                     sta adrh        ;expected result bit 8 (carry out)
23f7 : a981                     lda #$81        ;set N & C (99 + 99 + C = 99 + C)
23f9 : 850f                     sta adrf
23fb : a900                     lda #0          ;complemented operand 2 for subtract
23fd : 8510                     sta sb2
23ff : 8d0602                   sta sba2        ;non zp
2402 : 38               tdad    sec             ;test with carry set
2403 : 20ae24                   jsr chkdad
2406 : c60a                     dec adfc        ;now with carry clear
2408 : a50d                     lda adrl        ;decimal adjust result
240a : d008                     bne tdad1       ;skip clear carry & preset result 99 (9A-1)
240c : c60e                     dec adrh
240e : a999                     lda #$99
2410 : 850d                     sta adrl
2412 : d012                     bne tdad3
2414 : 290f             tdad1   and #$f         ;lower nibble mask
2416 : d00c                     bne tdad2       ;no decimal adjust needed
2418 : c60d                     dec adrl        ;decimal adjust (?0-6)
241a : c60d                     dec adrl
241c : c60d                     dec adrl
241e : c60d                     dec adrl
2420 : c60d                     dec adrl
2422 : c60d                     dec adrl
2424 : c60d             tdad2   dec adrl        ;result -1
2426 : 08               tdad3   php             ;save valid flags
2427 : 68                       pla
2428 : 2982                     and #$82        ;N-----Z-
242a : 050e                     ora adrh        ;N-----ZC
242c : 850f                     sta adrf
242e : 18                       clc             ;test with carry clear
242f : 20ae24                   jsr chkdad
2432 : e60a                     inc adfc        ;same for operand -1 but with carry
2434 : a50b                     lda ad1         ;decimal adjust operand 1
2436 : f015                     beq tdad5       ;iterate operand 2
2438 : 290f                     and #$f         ;lower nibble mask
243a : d00c                     bne tdad4       ;skip decimal adjust
243c : c60b                     dec ad1         ;decimal adjust (?0-6)
243e : c60b                     dec ad1
2440 : c60b                     dec ad1
2442 : c60b                     dec ad1
2444 : c60b                     dec ad1
2446 : c60b                     dec ad1
2448 : c60b             tdad4   dec ad1         ;operand 1 -1
244a : 4c0224                   jmp tdad        ;iterate op1
                        
244d : a999             tdad5   lda #$99        ;precharge op1 max
244f : 850b                     sta ad1
2451 : a50c                     lda ad2         ;decimal adjust operand 2
2453 : f039                     beq tdad7       ;end of iteration
2455 : 290f                     and #$f         ;lower nibble mask
2457 : d018                     bne tdad6       ;skip decimal adjust
2459 : c60c                     dec ad2         ;decimal adjust (?0-6)
245b : c60c                     dec ad2
245d : c60c                     dec ad2
245f : c60c                     dec ad2
2461 : c60c                     dec ad2
2463 : c60c                     dec ad2
2465 : e610                     inc sb2         ;complemented decimal adjust for subtract (?9+6)
2467 : e610                     inc sb2
2469 : e610                     inc sb2
246b : e610                     inc sb2
246d : e610                     inc sb2
246f : e610                     inc sb2
2471 : c60c             tdad6   dec ad2         ;operand 2 -1
2473 : e610                     inc sb2         ;complemented operand for subtract
2475 : a510                     lda sb2
2477 : 8d0602                   sta sba2        ;copy as non zp operand
247a : a50c                     lda ad2
247c : 8d0502                   sta ada2        ;copy as non zp operand
247f : 850d                     sta adrl        ;new result since op1+carry=00+carry +op2=op2
2481 : 08                       php             ;save flags
2482 : 68                       pla
2483 : 2982                     and #$82        ;N-----Z-
2485 : 0901                     ora #1          ;N-----ZC
2487 : 850f                     sta adrf
2489 : e60e                     inc adrh        ;result carry
248b : 4c0224                   jmp tdad        ;iterate op2
                        
248e : e00c             tdad7   cpx #ad2
                                trap_ne         ;x altered during test
2490 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2492 : c0ff                     cpy #$ff
                                trap_ne         ;y altered during test 
2494 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2496 : ba                       tsx
2497 : e0ff                     cpx #$ff
                                trap_ne         ;sp push/pop mismatch
2499 : d0fe            >        bne *           ;failed not equal (non zero)
                        
249b : d8                       cld
                        
249c : ad0202                   lda test_case
249f : c915                     cmp #test_num
                                trap_ne         ;previous test is out of sequence
24a1 : d0fe            >        bne *           ;failed not equal (non zero)
                        
24a3 : a9f0                     lda #$f0        ;mark opcode testing complete
24a5 : 8d0202                   sta test_case
                        
                        ; final RAM integrity test
                        ;   verifies that none of the previous tests has altered RAM outside of the
                        ;   designated write areas.
                                check_ram
                       >            ;RAM check disabled - RAM size not set
                        
                        ; *** DEBUG INFO ***
                        ; to debug checksum errors uncomment check_ram in the next_test macro to 
                        ; narrow down the responsible opcode.
                        ; may give false errors when monitor, OS or other background activity is
                        ; allowed during previous tests.
                        
                        
                        ; S U C C E S S ************************************************       
                        ; -------------       
                                success         ;if you get here everything went well
24a8 : 4ca824          >        jmp *           ;test passed, no errors
                        
                        ; -------------       
                        ; S U C C E S S ************************************************       
24ab : 4c0004                   jmp start       ;run again      
                        
                        ; core subroutine of the decimal add/subtract test
                        ; *** WARNING - tests documented behavior only! ***
                        ;   only valid BCD operands are tested, V flag is ignored
                        ; iterates through all valid combinations of operands and carry input
                        ; uses increments/decrements to predict result & carry flag
24ae :                  chkdad
                        ; decimal ADC / SBC zp
24ae : 08                       php             ;save carry for subtract
24af : a50b                     lda ad1
24b1 : 650c                     adc ad2         ;perform add
24b3 : 08                       php          
24b4 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
24b6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
24b8 : 68                       pla             ;check flags
24b9 : 2983                     and #$83        ;mask N-----ZC
24bb : c50f                     cmp adrf
                                trap_ne         ;bad flags
24bd : d0fe            >        bne *           ;failed not equal (non zero)
                        
24bf : 28                       plp
24c0 : 08                       php             ;save carry for next add
24c1 : a50b                     lda ad1
24c3 : e510                     sbc sb2         ;perform subtract
24c5 : 08                       php          
24c6 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
24c8 : d0fe            >        bne *           ;failed not equal (non zero)
                        
24ca : 68                       pla             ;check flags
24cb : 2983                     and #$83        ;mask N-----ZC
24cd : c50f                     cmp adrf
                                trap_ne         ;bad flags
24cf : d0fe            >        bne *           ;failed not equal (non zero)
                        
24d1 : 28                       plp
                        ; decimal ADC / SBC abs
24d2 : 08                       php             ;save carry for subtract
24d3 : a50b                     lda ad1
24d5 : 6d0502                   adc ada2        ;perform add
24d8 : 08                       php          
24d9 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
24db : d0fe            >        bne *           ;failed not equal (non zero)
                        
24dd : 68                       pla             ;check flags
24de : 2983                     and #$83        ;mask N-----ZC
24e0 : c50f                     cmp adrf
                                trap_ne         ;bad flags
24e2 : d0fe            >        bne *           ;failed not equal (non zero)
                        
24e4 : 28                       plp
24e5 : 08                       php             ;save carry for next add
24e6 : a50b                     lda ad1
24e8 : ed0602                   sbc sba2        ;perform subtract
24eb : 08                       php          
24ec : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
24ee : d0fe            >        bne *           ;failed not equal (non zero)
                        
24f0 : 68                       pla             ;check flags
24f1 : 2983                     and #$83        ;mask N-----ZC
24f3 : c50f                     cmp adrf
                                trap_ne         ;bad flags
24f5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
24f7 : 28                       plp
                        ; decimal ADC / SBC #
24f8 : 08                       php             ;save carry for subtract
24f9 : a50c                     lda ad2
24fb : 8d0125                   sta chkdadi     ;self modify immediate
24fe : a50b                     lda ad1
2501 =                  chkdadi = * + 1         ;operand of the immediate ADC
2500 : 6900                     adc #0          ;perform add
2502 : 08                       php          
2503 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2505 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2507 : 68                       pla             ;check flags
2508 : 2983                     and #$83        ;mask N-----ZC
250a : c50f                     cmp adrf
                                trap_ne         ;bad flags
250c : d0fe            >        bne *           ;failed not equal (non zero)
                        
250e : 28                       plp
250f : 08                       php             ;save carry for next add
2510 : a510                     lda sb2
2512 : 8d1825                   sta chkdsbi     ;self modify immediate
2515 : a50b                     lda ad1
2518 =                  chkdsbi = * + 1         ;operand of the immediate SBC
2517 : e900                     sbc #0          ;perform subtract
2519 : 08                       php          
251a : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
251c : d0fe            >        bne *           ;failed not equal (non zero)
                        
251e : 68                       pla             ;check flags
251f : 2983                     and #$83        ;mask N-----ZC
2521 : c50f                     cmp adrf
                                trap_ne         ;bad flags
2523 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2525 : 28                       plp
                        ; decimal ADC / SBC zp,x
2526 : 08                       php             ;save carry for subtract
2527 : a50b                     lda ad1
2529 : 7500                     adc 0,x         ;perform add
252b : 08                       php          
252c : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
252e : d0fe            >        bne *           ;failed not equal (non zero)
                        
2530 : 68                       pla             ;check flags
2531 : 2983                     and #$83        ;mask N-----ZC
2533 : c50f                     cmp adrf
                                trap_ne         ;bad flags
2535 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2537 : 28                       plp
2538 : 08                       php             ;save carry for next add
2539 : a50b                     lda ad1
253b : f504                     sbc sb2-ad2,x   ;perform subtract
253d : 08                       php          
253e : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2540 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2542 : 68                       pla             ;check flags
2543 : 2983                     and #$83        ;mask N-----ZC
2545 : c50f                     cmp adrf
                                trap_ne         ;bad flags
2547 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2549 : 28                       plp
                        ; decimal ADC / SBC abs,x
254a : 08                       php             ;save carry for subtract
254b : a50b                     lda ad1
254d : 7df901                   adc ada2-ad2,x  ;perform add
2550 : 08                       php          
2551 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2553 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2555 : 68                       pla             ;check flags
2556 : 2983                     and #$83        ;mask N-----ZC
2558 : c50f                     cmp adrf
                                trap_ne         ;bad flags
255a : d0fe            >        bne *           ;failed not equal (non zero)
                        
255c : 28                       plp
255d : 08                       php             ;save carry for next add
255e : a50b                     lda ad1
2560 : fdfa01                   sbc sba2-ad2,x  ;perform subtract
2563 : 08                       php          
2564 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2566 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2568 : 68                       pla             ;check flags
2569 : 2983                     and #$83        ;mask N-----ZC
256b : c50f                     cmp adrf
                                trap_ne         ;bad flags
256d : d0fe            >        bne *           ;failed not equal (non zero)
                        
256f : 28                       plp
                        ; decimal ADC / SBC abs,y
2570 : 08                       php             ;save carry for subtract
2571 : a50b                     lda ad1
2573 : 790601                   adc ada2-$ff,y  ;perform add
2576 : 08                       php          
2577 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2579 : d0fe            >        bne *           ;failed not equal (non zero)
                        
257b : 68                       pla             ;check flags
257c : 2983                     and #$83        ;mask N-----ZC
257e : c50f                     cmp adrf
                                trap_ne         ;bad flags
2580 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2582 : 28                       plp
2583 : 08                       php             ;save carry for next add
2584 : a50b                     lda ad1
2586 : f90701                   sbc sba2-$ff,y  ;perform subtract
2589 : 08                       php          
258a : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
258c : d0fe            >        bne *           ;failed not equal (non zero)
                        
258e : 68                       pla             ;check flags
258f : 2983                     and #$83        ;mask N-----ZC
2591 : c50f                     cmp adrf
                                trap_ne         ;bad flags
2593 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2595 : 28                       plp
                        ; decimal ADC / SBC (zp,x)
2596 : 08                       php             ;save carry for subtract
2597 : a50b                     lda ad1
2599 : 6144                     adc (lo adi2-ad2,x) ;perform add
259b : 08                       php          
259c : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
259e : d0fe            >        bne *           ;failed not equal (non zero)
                        
25a0 : 68                       pla             ;check flags
25a1 : 2983                     and #$83        ;mask N-----ZC
25a3 : c50f                     cmp adrf
                                trap_ne         ;bad flags
25a5 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25a7 : 28                       plp
25a8 : 08                       php             ;save carry for next add
25a9 : a50b                     lda ad1
25ab : e146                     sbc (lo sbi2-ad2,x) ;perform subtract
25ad : 08                       php          
25ae : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
25b0 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25b2 : 68                       pla             ;check flags
25b3 : 2983                     and #$83        ;mask N-----ZC
25b5 : c50f                     cmp adrf
                                trap_ne         ;bad flags
25b7 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25b9 : 28                       plp
                        ; decimal ADC / SBC (abs),y
25ba : 08                       php             ;save carry for subtract
25bb : a50b                     lda ad1
25bd : 7154                     adc (adiy2),y   ;perform add
25bf : 08                       php          
25c0 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
25c2 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25c4 : 68                       pla             ;check flags
25c5 : 2983                     and #$83        ;mask N-----ZC
25c7 : c50f                     cmp adrf
                                trap_ne         ;bad flags
25c9 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25cb : 28                       plp
25cc : 08                       php             ;save carry for next add
25cd : a50b                     lda ad1
25cf : f156                     sbc (sbiy2),y   ;perform subtract
25d1 : 08                       php          
25d2 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
25d4 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25d6 : 68                       pla             ;check flags
25d7 : 2983                     and #$83        ;mask N-----ZC
25d9 : c50f                     cmp adrf
                                trap_ne         ;bad flags
25db : d0fe            >        bne *           ;failed not equal (non zero)
                        
25dd : 28                       plp
                        ; decimal ADC / SBC (zp)
25de : 08                       php             ;save carry for subtract
25df : a50b                     lda ad1
25e1 : 7250                     adc (adi2)      ;perform add
25e3 : 08                       php          
25e4 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
25e6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25e8 : 68                       pla             ;check flags
25e9 : 2983                     and #$83        ;mask N-----ZC
25eb : c50f                     cmp adrf
                                trap_ne         ;bad flags
25ed : d0fe            >        bne *           ;failed not equal (non zero)
                        
25ef : 28                       plp
25f0 : 08                       php             ;save carry for next add
25f1 : a50b                     lda ad1
25f3 : f252                     sbc (sbi2)      ;perform subtract
25f5 : 08                       php          
25f6 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
25f8 : d0fe            >        bne *           ;failed not equal (non zero)
                        
25fa : 68                       pla             ;check flags
25fb : 2983                     and #$83        ;mask N-----ZC
25fd : c50f                     cmp adrf
                                trap_ne         ;bad flags
25ff : d0fe            >        bne *           ;failed not equal (non zero)
                        
2601 : 28                       plp
2602 : 60                       rts
                        
                        ; core subroutine of the full binary add/subtract test
                        ; iterates through all combinations of operands and carry input
                        ; uses increments/decrements to predict result & result flags
2603 : a50f             chkadd  lda adrf        ;add V-flag if overflow
2605 : 2983                     and #$83        ;keep N-----ZC / clear V
2607 : 48                       pha
2608 : a50b                     lda ad1         ;test sign unequal between operands
260a : 450c                     eor ad2
260c : 300a                     bmi ckad1       ;no overflow possible - operands have different sign
260e : a50b                     lda ad1         ;test sign equal between operands and result
2610 : 450d                     eor adrl
2612 : 1004                     bpl ckad1       ;no overflow occured - operand and result have same sign
2614 : 68                       pla
2615 : 0940                     ora #$40        ;set V
2617 : 48                       pha
2618 : 68               ckad1   pla
2619 : 850f                     sta adrf        ;save expected flags
                        ; binary ADC / SBC (zp)
261b : 08                       php             ;save carry for subtract
261c : a50b                     lda ad1
261e : 7250                     adc (adi2)      ;perform add
2620 : 08                       php          
2621 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2623 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2625 : 68                       pla             ;check flags
2626 : 29c3                     and #$c3        ;mask NV----ZC
2628 : c50f                     cmp adrf
                                trap_ne         ;bad flags
262a : d0fe            >        bne *           ;failed not equal (non zero)
                        
262c : 28                       plp
262d : 08                       php             ;save carry for next add
262e : a50b                     lda ad1
2630 : f252                     sbc (sbi2)      ;perform subtract
2632 : 08                       php          
2633 : c50d                     cmp adrl        ;check result
                                trap_ne         ;bad result
2635 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2637 : 68                       pla             ;check flags
2638 : 29c3                     and #$c3        ;mask NV----ZC
263a : c50f                     cmp adrf
                                trap_ne         ;bad flags
263c : d0fe            >        bne *           ;failed not equal (non zero)
                        
263e : 28                       plp
263f : 60                       rts
                                
                        ; target for the jump indirect test
2640 : 4626             ji_adr  dw test_ji
2642 : 8216                     dw ji_ret
                        
2644 : 88                       dey
2645 : 88                       dey
2646 :                  test_ji
2646 : 08                       php             ;either SP or Y count will fail, if we do not hit
2647 : 88                       dey
2648 : 88                       dey
2649 : 88                       dey
264a : 28                       plp
                                trap_cs         ;flags loaded?
264b : b0fe            >        bcs *           ;failed carry set
                        
                                trap_vs
264d : 70fe            >        bvs *           ;failed overflow set
                        
                                trap_mi
264f : 30fe            >        bmi *           ;failed minus (bit 7 set)
                        
                                trap_eq 
2651 : f0fe            >        beq *           ;failed equal (zero)
                        
2653 : c949                     cmp #'I'        ;registers loaded?
                                trap_ne
2655 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2657 : e04e                     cpx #'N'
                                trap_ne        
2659 : d0fe            >        bne *           ;failed not equal (non zero)
                        
265b : c041                     cpy #('D'-3)
                                trap_ne
265d : d0fe            >        bne *           ;failed not equal (non zero)
                        
265f : 48                       pha             ;save a,x
2660 : 8a                       txa
2661 : 48                       pha
2662 : ba                       tsx
2663 : e0fd                     cpx #$fd        ;check SP
                                trap_ne
2665 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2667 : 68                       pla             ;restore x
2668 : aa                       tax
                                set_stat $ff
                       >            load_flag $ff
2669 : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
266b : 48              >            pha         ;use stack to load status
266c : 28              >            plp
                        
266d : 68                       pla             ;restore a
266e : e8                       inx             ;return registers with modifications
266f : 49aa                     eor #$aa        ;N=1, V=1, Z=0, C=1
2671 : 6cff02                   jmp (ji_tab+2)
2674 : ea                       nop
2675 : ea                       nop
                                trap            ;runover protection
2676 : 4c7626          >        jmp *           ;failed anyway
                        
                        
                        ; target for the jump indirect test
2679 : bd26             jxi_adr dw  trap_ind
267b : bd26                     dw  trap_ind
267d : 8726                     dw  test_jxi    ;+4
267f : ce16                     dw  jxi_ret     ;+6
2681 : bd26                     dw  trap_ind
2683 : bd26                     dw  trap_ind
                        
2685 : 88                       dey
2686 : 88                       dey
2687 :                  test_jxi
2687 : 08                       php             ;either SP or Y count will fail, if we do not hit
2688 : 88                       dey
2689 : 88                       dey
268a : 88                       dey
268b : 28                       plp
                                trap_cs         ;flags loaded?
268c : b0fe            >        bcs *           ;failed carry set
                        
                                trap_vs
268e : 70fe            >        bvs *           ;failed overflow set
                        
                                trap_mi
2690 : 30fe            >        bmi *           ;failed minus (bit 7 set)
                        
                                trap_eq 
2692 : f0fe            >        beq *           ;failed equal (zero)
                        
2694 : c958                     cmp #'X'        ;registers loaded?
                                trap_ne
2696 : d0fe            >        bne *           ;failed not equal (non zero)
                        
2698 : e004                     cpx #4
                                trap_ne        
269a : d0fe            >        bne *           ;failed not equal (non zero)
                        
269c : c046                     cpy #('I'-3)
                                trap_ne
269e : d0fe            >        bne *           ;failed not equal (non zero)
                        
26a0 : 48                       pha             ;save a,x
26a1 : 8a                       txa
26a2 : 48                       pha
26a3 : ba                       tsx
26a4 : e0fd                     cpx #$fd        ;check SP
                                trap_ne
26a6 : d0fe            >        bne *           ;failed not equal (non zero)
                        
26a8 : 68                       pla             ;restore x
26a9 : aa                       tax
                                set_stat $ff
                       >            load_flag $ff
26aa : a9ff            >            lda #$ff             ;allow test to change I-flag (no mask)
                       >
26ac : 48              >            pha         ;use stack to load status
26ad : 28              >            plp
                        
26ae : 68                       pla             ;restore a
26af : e8                       inx             ;return registers with modifications
26b0 : e8                       inx
26b1 : 49aa                     eor #$aa        ;N=1, V=1, Z=0, C=1
26b3 : 7cf902                   jmp (jxi_tab,x)
26b6 : ea                       nop
26b7 : ea                       nop
                                trap            ;runover protection
26b8 : 4cb826          >        jmp *           ;failed anyway
                        
                        
                        ; JMP (abs,x) with bad x
26bb : ea                       nop
26bc : ea                       nop
26bd :                  trap_ind
26bd : ea                       nop
26be : ea                       nop
                                trap            ;near miss indexed indirect jump
26bf : 4cbf26          >        jmp *           ;failed anyway
                        
                        
                        ;trap in case of unexpected IRQ, NMI, BRK, RESET
26c2 :                  nmi_trap
                                trap            ;check stack for conditions at NMI
26c2 : 4cc226          >        jmp *           ;failed anyway
                        
26c5 :                  res_trap
                                trap            ;unexpected RESET
26c5 : 4cc526          >        jmp *           ;failed anyway
                        
26c8 :                  irq_trap
26c8 : 08                       php             ;save decimal flag
26c9 : ba                       tsx             ;test break on stack
26ca : bd0201                   lda $102,x
26cd : 2910                     and #break
                                trap_eq         ;check stack for conditions at IRQ
26cf : f0fe            >        beq *           ;failed equal (zero)
                        
                            if ROM_vectors = 1
26d1 : 68                       pla             ;test decimal mode cleared
26d2 : 2908                     and #decmode
                                trap_ne         ;decimal mode not cleared after BRK
26d4 : d0fe            >        bne *           ;failed not equal (non zero)
                        
26d6 : 28                       plp             ;pop saved flags
26d7 : 68                       pla             ;return address low
26d8 : c917                     cmp #lo(brk_ret)
                                trap_ne         ;unexpected BRK
26da : d0fe            >        bne *           ;failed not equal (non zero)
                        
26dc : 68                       pla             ;return address high
26dd : c917                     cmp #hi(brk_ret)
                                trap_ne         ;unexpected BRK
26df : d0fe            >        bne *           ;failed not equal (non zero)
                        
26e1 : 4c1717                   jmp brk_ret
                            else
                                trap_ne         ;check stack for conditions at BRK
                            endif
                                
                            if report = 1
                                include "report.i65"
                            endif
                                    
                        ;copy of data to initialize BSS segment
                            if load_data_direct != 1
                        zp_init
                        zp1_    db  $c3,$82,$41,0   ;test patterns for LDx BIT ROL ROR ASL LSR
                        zp7f_   db  $7f             ;test pattern for compare  
                        ;logical zeropage operands
                        zpOR_   db  0,$1f,$71,$80   ;test pattern for OR
                        zpAN_   db  $0f,$ff,$7f,$80 ;test pattern for AND
                        zpEO_   db  $ff,$0f,$8f,$8f ;test pattern for EOR
                        ;indirect addressing pointers
                        ind1_   dw  abs1            ;indirect pointer to pattern in absolute memory
                                dw  abs1+1
                                dw  abs1+2
                                dw  abs1+3
                                dw  abs7f
                        inw1_   dw  abs1-$f8        ;indirect pointer for wrap-test pattern
                        indt_   dw  abst            ;indirect pointer to store area in absolute memory
                                dw  abst+1
                                dw  abst+2
                                dw  abst+3
                        inwt_   dw  abst-$f8        ;indirect pointer for wrap-test store
                        indAN_  dw  absAN           ;indirect pointer to AND pattern in absolute memory
                                dw  absAN+1
                                dw  absAN+2
                                dw  absAN+3
                        indEO_  dw  absEO           ;indirect pointer to EOR pattern in absolute memory
                                dw  absEO+1
                                dw  absEO+2
                                dw  absEO+3
                        indOR_  dw  absOR           ;indirect pointer to OR pattern in absolute memory
                                dw  absOR+1
                                dw  absOR+2
                                dw  absOR+3
                        ;add/subtract indirect pointers
                        adi2_   dw  ada2            ;indirect pointer to operand 2 in absolute memory
                        sbi2_   dw  sba2            ;indirect pointer to complemented operand 2 (SBC)
                        adiy2_  dw  ada2-$ff        ;with offset for indirect indexed
                        sbiy2_  dw  sba2-$ff
                        zp_end
                            if (zp_end - zp_init) != (zp_bss_end - zp_bss)   
                                ;force assembler error if size is different   
                                ERROR ERROR ERROR   ;mismatch between bss and zeropage data
                            endif 
                        data_init
                        abs1_   db  $c3,$82,$41,0   ;test patterns for LDx BIT ROL ROR ASL LSR
                        abs7f_  db  $7f             ;test pattern for compare
                        ;loads
                        fLDx_   db  fn,fn,0,fz      ;expected flags for load
                        ;shifts
                        rASL_                       ;expected result ASL & ROL -carry  
                        rROL_   db  $86,$04,$82,0   ; "
                        rROLc_  db  $87,$05,$83,1   ;expected result ROL +carry
                        rLSR_                       ;expected result LSR & ROR -carry
                        rROR_   db  $61,$41,$20,0   ; "
                        rRORc_  db  $e1,$c1,$a0,$80 ;expected result ROR +carry
                        fASL_                       ;expected flags for shifts
                        fROL_   db  fnc,fc,fn,fz    ;no carry in
                        fROLc_  db  fnc,fc,fn,0     ;carry in
                        fLSR_
                        fROR_   db  fc,0,fc,fz      ;no carry in
                        fRORc_  db  fnc,fn,fnc,fn   ;carry in
                        ;increments (decrements)
                        rINC_   db  $7f,$80,$ff,0,1 ;expected result for INC/DEC
                        fINC_   db  0,fn,fn,fz,0    ;expected flags for INC/DEC
                        ;logical memory operand
                        absOR_  db  0,$1f,$71,$80   ;test pattern for OR
                        absAN_  db  $0f,$ff,$7f,$80 ;test pattern for AND
                        absEO_  db  $ff,$0f,$8f,$8f ;test pattern for EOR
                        ;logical accu operand
                        absORa_ db  0,$f1,$1f,0     ;test pattern for OR
                        absANa_ db  $f0,$ff,$ff,$ff ;test pattern for AND
                        absEOa_ db  $ff,$f0,$f0,$0f ;test pattern for EOR
                        ;logical results
                        absrlo_ db  0,$ff,$7f,$80
                        absflo_ db  fz,fn,0,fn
                        data_end
                            if (data_end - data_init) != (data_bss_end - data_bss)
                                ;force assembler error if size is different   
                                ERROR ERROR ERROR   ;mismatch between bss and data
                            endif 
                        
                        vec_init
                                dw  nmi_trap
                                dw  res_trap
                                dw  irq_trap
                        vec_bss equ $fffa
                            endif                   ;end of RAM init data
                            
                        ; code at end of image due to the need to add blank space as required
                            if ($ff & (ji_ret - * - 2)) < ($ff & (jxi_ret - * - 2))
                        ; JMP (abs) when $xxff and $xx00 are from same page
26e4 : 00000000000000..         ds  lo(ji_ret - * - 2)
2780 : ea                       nop
2781 : ea                       nop
2782 : ea               ji_px   nop             ;low address byte matched with ji_ret 
2783 : ea                       nop
                                trap            ;jmp indirect page cross bug
2784 : 4c8427          >        jmp *           ;failed anyway
                        
                        
                        ; JMP (abs,x) when $xxff and $xx00 are from same page
2787 : 00000000000000..         ds  lo(jxi_ret - * - 2)
27cc : ea                       nop
27cd : ea                       nop
27ce : ea               jxi_px  nop             ;low address byte matched with jxi_ret 
27cf : ea                       nop
                                trap            ;jmp indexed indirect page cross bug
27d0 : 4cd027          >        jmp *           ;failed anyway
                        
                            else
                        ; JMP (abs,x) when $xxff and $xx00 are from same page
                                ds  lo(jxi_ret - * - 2)
                                nop
                                nop
                        jxi_px  nop             ;low address byte matched with jxi_ret 
                                nop
                                trap            ;jmp indexed indirect page cross bug
                        
                        ; JMP (abs) when $xxff and $xx00 are from same page
                                ds  lo(ji_ret - * - 2)
                                nop
                                nop
                        ji_px   nop             ;low address byte matched with ji_ret 
                                nop
                                trap            ;jmp indirect page cross bug
                            endif
                            
                            if (load_data_direct = 1) & (ROM_vectors = 1)  
fffa =                          org $fffa       ;vectors
fffa : c226                     dw  nmi_trap
fffc : c526                     dw  res_trap
fffe : c826                     dw  irq_trap
                            endif
                        
fffa =                          end start
                                    
No errors in pass 2.
Wrote binary from address $0000 through $ffff.
Total size 65536 bytes.
Program start address is at $0400 (1024).
