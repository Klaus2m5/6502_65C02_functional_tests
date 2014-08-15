;
; 6 5 0 2   I N T E R R U P T   T E S T
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


; This program is designed to test IRQ and NMI of a 6502 emulator. It requires
; an internal or external feedback register to the IRQ & NMI inputs
; 
; version 15-aug-2014
; contact info at http://2m5.de or email K@2m5.de
;
; assembled with AS65 from http://www.kingswood-consulting.co.uk/assemblers/
; command line switches: -l -m -s2 -w -h0
;                         |  |  |   |  no page headers in listing
;                         |  |  |   wide listing (133 char/col)
;                         |  |  write intel hex file instead of binary
;                         |  expand macros in listing
;                         generate pass2 listing
;
; No IO - should be run from a monitor with access to registers.
; To run load intel hex image with a load command, than alter PC to 400 hex and
; enter a go command.
; Loop on program counter determines error or successful completion of test.
; Check listing for relevant traps (jump/branch *).
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
;   16-aug-2013  added error report to standard output option
;   15-aug-2014  added filter to feedback (bit 7 will cause diag stop in emu)


; C O N F I G U R A T I O N
;
;ROM_vectors MUST be writable & the I_flag MUST be alterable

;load_data_direct (0=move from code segment, 1=load directly)
;loading directly is preferred but may not be supported by your platform
;0 produces only consecutive object code, 1 is not suitable for a binary image
load_data_direct = 1

;NMI & IRQ are tested with a feedback register
;emulators diag register - set i_drive = 0 for a latch (74HC573)
I_port      = $bffc     ;feedback port address
I_ddr       = 0         ;feedback DDR address, 0 = no DDR
I_drive     = 1         ;0 = totem pole, 1 = open collector
IRQ_bit     = 0         ;bit number of feedback to IRQ
NMI_bit     = 1         ;bit number of feedback to NMI, -1 if not available
I_filter    = $7f       ;filtering bit 7 = diag stop

;typical IO chip port B - set i_drive = 0 to avoid pullup resistors
;I_port      = $bfb2     ;feedback port address
;I_ddr       = $bfb3     ;feedback DDR address, 0 = no DDR
;I_drive     = 1         ;0 = totem pole, 1 = open collector
;IRQ_bit     = 0         ;bit number of feedback to IRQ
;NMI_bit     = 1         ;bit number of feedback to NMI, -1 if not available
;I_filter    = $ff       ;no bits filtered

;decimal mode flag during IRQ, NMI & BRK
D_clear     = 0         ;0 = not cleared (NMOS), 1 = cleared (CMOS)

;configure memory - try to stay away from memory used by the system
;zero_page memory start address, 6 consecutive Bytes required
zero_page = $a  

;data_segment memory start address, 4 consecutive Bytes required
data_segment = $200  

;code_segment memory start address
code_segment = $400

;report errors through I/O channel (0=use standard self trap loops, 1=include
;report.i65 as I/O channel)
report = 0

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
; please observe that during the test the stack gets invalidated
; therefore a RTS inside the success macro is not possible
success macro
        jsr report_success
        endm
    endif


carry   equ %00000001   ;flag bits in status
zero    equ %00000010
intdis  equ %00000100
decmode equ %00001000
break   equ %00010000
reserv  equ %00100000
overfl  equ %01000000
minus   equ %10000000

fc      equ carry
fz      equ zero
fzc     equ carry+zero
fv      equ overfl
fvz     equ overfl+zero
fn      equ minus
fnc     equ minus+carry
fnz     equ minus+zero
fnzc    equ minus+zero+carry
fnv     equ minus+overfl

fao     equ break+reserv    ;bits always on after PHP, BRK
fai     equ fao+intdis      ;+ forced interrupt disable
m8      equ $ff             ;8 bit mask
m8i     equ $ff&~intdis     ;8 bit mask - interrupt disable

;macros to set status
push_stat   macro       ;setting flags in the processor status register
            lda #\1
            pha         ;use stack to load status
            endm

set_stat    macro       ;setting flags in the processor status register
            lda #\1
            pha         ;use stack to load status
            plp
            endm

    if load_data_direct = 1
        data
    else
        bss                 ;uninitialized segment, copy of data at end of code!
    endif
        org zero_page
;BRK, IRQ, NMI test interrupt save
zpt
irq_a   ds  1               ;a register
irq_x   ds  1               ;x register
irq_f   ds  1               ;flags
nmi_a   ds  1               ;a register
nmi_x   ds  1               ;x register
nmi_f   ds  1               ;flags
zp_bss

;fixed stack locations
lst_f   equ $1fe            ;last flags before interrupt
lst_a   equ $1ff            ;last accumulator before interrupt
    
        org data_segment
;concurrent NMI, IRQ & BRK test result
nmi_count   ds  1           ;lowest number handled first, $ff = never
irq_count   ds  1           ;separation-1 = instructions between interrupts
brk_count   ds  1
;expected interrupt mask
I_src       ds  1           ;bit: 0=BRK, 1=IRQ, 2=NMI
data_bss

        code
        org code_segment
start   cld
        lda #0           ;clear expected interrupts for 2nd run
        sta I_src
        ldx #$ff
        txs
    
;initialize I/O for report channel
    if report = 1
        jsr report_init
    endif

; load system vectors
    if load_data_direct != 1
        ldx #5
ld_vect lda vec_init,x
        sta vec_bss,x
        dex
        bpl ld_vect
    endif

; IRQ & NMI test - requires a feedback register
    if I_drive > 1
        ERROR           ;invalid interrupt drive!
    endif
  if NMI_bit < 0
    if I_drive = 0      ;totem pole (push/pull, 0 -> I_port to force interrupt)
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn on interrupt by bit
        and #I_filter-(1<<\1)
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_port      ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn off interrupt by bit
        and #I_filter
        ora #(1<<ibit)
        sta I_port
        endm
        I_clr   IRQ_bit ;turn off IRQ
      if I_ddr != 0     ;with DDR
        lda I_ddr       ;set DDR for IRQ to enabled
        and #I_filter
        ora #(1<<IRQ_bit)
        sta I_ddr
      endif    
    else                ;open collector, 0 -> I_DDR or I_port to force interrupt
      if I_ddr != 0     ;with DDR
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_ddr       ;turn on interrupt by bit
        and #I_filter
        ora #(1<<\1)
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_ddr       ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_ddr       ;turn off interrupt by bit
        and #I_filter-(1<<ibit)
        sta I_ddr 
        endm
        I_clr   IRQ_bit ;turn off IRQ
        lda I_port      ;precharge IRQ
        and #I_filter-(1<<IRQ_bit)
        sta I_port
      else              ;no DDR
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn on interrupt by bit
        and #I_filter
        ora #(1<<\1)
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_port      ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn off interrupt by bit
        and #I_filter-(1<<ibit)
        sta I_port
        endm
        I_clr   IRQ_bit ;turn off IRQ
      endif
    endif
  else
    if I_drive = 0      ;totem pole (push/pull, 0 -> I_port to force interrupt)
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn on interrupt by bit
        if ibit > 7     ;set both NMI & IRQ
          and #I_filter-(1<<IRQ_bit|1<<NMI_bit)
        else
          and #I_filter-(1<<\1)
        endif
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_port      ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn off interrupt by bit
        and #I_filter
        ora #(1<<ibit)
        sta I_port
        endm
        I_clr   IRQ_bit ;turn off IRQ & NMI
        I_clr   NMI_bit
      if I_ddr != 0     ;with DDR
        lda I_ddr       ;set DDR for IRQ & NMI to enabled
        and #I_filter
        ora #(1<<IRQ_bit|1<<NMI_bit)
        sta I_ddr
      endif    
    else                ;open collector, 0 -> I_DDR or I_port to force interrupt
      if I_ddr != 0     ;with DDR
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_ddr       ;turn on interrupt by bit
        and #I_filter
        if ibit > 7     ;set both NMI & IRQ
          ora #(1<<IRQ_bit|1<<NMI_bit)
        else
          ora #(1<<\1)
        endif
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_ddr       ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_ddr       ;turn off interrupt by bit
        and #I_filter-(1<<ibit)
        sta I_ddr 
        endm
        I_clr   IRQ_bit ;turn off IRQ & NMI
        I_clr   NMI_bit
        lda I_port      ;precharge IRQ & NMI
        and #I_filter-(1<<IRQ_bit|1<<NMI_bit)
        sta I_port
      else              ;no DDR
I_set   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn on interrupt by bit
        and #I_filter
        if ibit > 7     ;set both NMI & IRQ
          ora #(1<<IRQ_bit|1<<NMI_bit)
        else
          ora #(1<<\1)
        endif
        plp             ;set flags
        pha             ;save to verify
        php
        sta I_port      ;interrupt next instruction plus outbound delay
        endm
I_clr   macro  ibit     ;ibit = interrupt bit
        lda I_port      ;turn off interrupt by bit
        and #I_filter-(1<<ibit)
        sta I_port
        endm
        I_clr   IRQ_bit ;turn off IRQ & NMI
        I_clr   NMI_bit
      endif
    endif
  endif
  
; IRQ integrity test
; test for clear flags seen in IRQ vector
        lda #2          ;set expected interrupt source IRQ
        sta I_src
        push_stat 0
        I_set IRQ_bit
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;IRQ timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda irq_f       ;flags seen in IRQ vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda irq_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs
; test all other registers
        ldx #'I'
        ldy #'R'
        lda #2          ;set expected interrupt source IRQ
        sta I_src
        push_stat 0
        I_set IRQ_bit
        dey             ;Y count will fail, if instructions are skipped
        dey
        dey
        dey
        php             ;check processor status later
        cpx #('I'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #('R'-7)
        trap_ne         ;returned Y
        cmp #'Q'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda irq_a       ;accu seen in IRQ vector
        cmp lst_a
        trap_ne         ;IRQ A received
        ldx #$ff        ;reset stack pointer
        txs
; repeat with reversed registers
        ldx #$ff-'I'
        ldy #$ff-'R'
        lda #2          ;set expected interrupt source IRQ
        sta I_src
        push_stat $ff-intdis
        I_set IRQ_bit
        dey             ;Y count will fail, if instructions are skipped
        dey
        dey
        dey
        php             ;check processor status later
        cpx #($ff-'I'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #($ff-'R'-7)
        trap_ne         ;returned Y
        cmp #'Q'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda irq_a       ;accu seen in IRQ vector
        cmp lst_a
        trap_ne         ;IRQ A received
        ldx #$ff        ;reset stack pointer
        txs
; retest for set flags seen in IRQ vector
        lda #2          ;set expected interrupt source IRQ
        sta I_src
        push_stat $ff-intdis
        I_set IRQ_bit
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;IRQ timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda irq_f       ;flags seen in IRQ vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda irq_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs

; BRK integrity test
; test for clear flags seen in IRQ vector
        lda #1          ;set expected interrupt source BRK
        sta I_src
        set_stat 0
        pha             ;save entry registers
        php
        brk
        nop             ;should not be executed
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;IRQ timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda irq_f       ;flags seen in IRQ vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda irq_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs
; test all other registers
        ldx #'B'
        ldy #'R'
        lda #1          ;set expected interrupt source BRK
        sta I_src
        set_stat 0
        pha             ;save entry
        php
        brk
        dey             ;should not be executed
        dey             ;Y count will fail, if return address is wrong
        dey
        dey
        dey
        php             ;check processor status later
        cpx #('B'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #('R'-7)
        trap_ne         ;returned Y
        cmp #'K'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda irq_a       ;accu seen in IRQ vector
        cmp lst_a
        trap_ne         ;IRQ A received
        ldx #$ff        ;reset stack pointer
        txs
; repeat with reversed registers
        ldx #$ff-'B'
        ldy #$ff-'R'
        lda #1          ;set expected interrupt source BRK
        sta I_src
        set_stat $ff
        pha             ;save entry registers
        php
        brk
        dey             ;should not be executed
        dey             ;Y count will fail, if return address is wrong
        dey
        dey
        dey
        php             ;check processor status later
        cpx #($ff-'B'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #($ff-'R'-7)
        trap_ne         ;returned Y
        cmp #'K'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda irq_a       ;accu seen in IRQ vector
        cmp lst_a
        trap_ne         ;IRQ A received
        ldx #$ff        ;reset stack pointer
        txs
; retest for set flags seen in IRQ vector
        lda #1          ;set expected interrupt source BRK
        sta I_src
        set_stat $ff
        pha             ;save entry registers
        php
        brk
        nop             ;should not be executed
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;IRQ timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda irq_f       ;flags seen in IRQ vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda irq_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs

    if NMI_bit < 0
; test IRQ with interrupts disabled
        ldx #0
        lda #0
        sta I_src
        push_stat intdis        
        I_set IRQ_bit   ;IRQ pending
        inx
        inx
        inx
        ldx #0
        lda #2          ;now re-enable IRQ
        sta I_src
        cli
        inx
        inx
        inx
        lda I_src       ;test IRQ done?
        trap_ne
        ldx #$ff        ;purge stack
        txs

        ldx #0          ;now overlap IRQ & BRK
        lda #3
        sta I_src
        lda #$ff        ;measure timing
        sta nmi_count
        sta irq_count
        sta brk_count
        push_stat 0        
        I_set IRQ_bit   ;trigger IRQ
    else
; NMI integrity test
; test for clear flags seen in NMI vector
        lda #4          ;set expected interrupt source NMI
        sta I_src
        push_stat 0
        I_set NMI_bit
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;NMI timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda nmi_f       ;flags seen in NMI vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda nmi_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs
; test all other registers
        ldx #'N'
        ldy #'M'
        lda #4          ;set expected interrupt source NMI
        sta I_src
        push_stat 0
        I_set NMI_bit
        dey             ;Y count will fail, if instructions are skipped
        dey
        dey
        dey
        php             ;check processor status later
        cpx #('N'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #('M'-7)
        trap_ne         ;returned Y
        cmp #'I'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda nmi_a       ;accu seen in NMI vector
        cmp lst_a
        trap_ne         ;NMI A received
        ldx #$ff        ;reset stack pointer
        txs
; repeat with reversed registers
        ldx #$ff-'N'
        ldy #$ff-'M'
        lda #4          ;set expected interrupt source NMI
        sta I_src
        push_stat $ff-intdis
        I_set NMI_bit
        dey             ;Y count will fail, if instructions are skipped
        dey
        dey
        dey
        php             ;check processor status later
        cpx #($ff-'N'+1)    ;returned registers OK?
        trap_ne         ;returned X
        cpy #($ff-'M'-7)
        trap_ne         ;returned Y
        cmp #'I'
        trap_ne         ;returned A
        tsx
        cpx #$ff-3
        trap_ne         ;returned SP
        pla             ;flags
        eor lst_f
        and #$ff-fnz    ;ignore flags changed by dey
        trap_ne         ;returned flags
        lda nmi_a       ;accu seen in NMI vector
        cmp lst_a
        trap_ne         ;NMI A received
        ldx #$ff        ;reset stack pointer
        txs
; retest for set flags seen in NMI vector
        lda #4          ;set expected interrupt source NMI
        sta I_src
        push_stat $ff-intdis
        I_set NMI_bit
        nop             ;allow 6 cycles for interrupt to trip
        nop
        nop
        lda I_src
        trap_ne         ;NMI timeout
        tsx
        cpx #$ff-2      ;original accu & flags remain on stack
        trap_ne         ;returned SP
        lda nmi_f       ;flags seen in NMI vector
      if D_clear = 1
        and #decmode
        trap_ne         ;D-flag not cleared
        lda nmi_f
        eor lst_f       ;turn off unchanged bits
        and #m8-fai-decmode ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C) changed
      else
        eor lst_f       ;turn off unchanged bits
        and #m8-fai     ;mask untested other flags
        trap_ne         ;other flags (N,V,Z,C,D) changed
      endif
        ldx #$ff        ;reset stack pointer
        txs

; test IRQ & NMI with interrupts disabled
        ldx #0
        lda #4          ;set expected interrupt NMI only
        sta I_src
        push_stat intdis        
        I_set 8         ;both interrupts pending
        inx
        inx
        inx
        lda I_src       ;test NMI done?
        trap_ne
        ldx #0
        lda #2          ;now re-enable IRQ
        sta I_src
        cli
        inx
        inx
        inx
        lda I_src       ;test IRQ done?
        trap_ne
        ldx #$ff        ;purge stack
        txs

;test overlapping NMI, IRQ & BRK
        ldx #0
        lda #7
        sta I_src
        lda #$ff        ;measure timing
        sta nmi_count
        sta irq_count
        sta brk_count
        push_stat 0
        I_set 8         ;trigger NMI + IRQ
    endif
        brk
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        lda I_src       ;test all done?
;may fail due to a bug on a real NMOS 6502 - NMI could mask BRK
        trap_ne         ;lost an interrupt

; S U C C E S S ************************************************       
; -------------       
        success         ;if you get here everything went well
; -------------       
; S U C C E S S ************************************************       
; check data_segment +0 to +2 for sequence of concurrent interrupts
; e.g. 0x200 = NMI, 0x201 = IRQ, 0x202 = BRK, lower values = earlier
        jmp start       ;run again      

; manual tests for the WAI opcode of the 65c02

wai     macro   
        db  $cb         ;WAI opcode
        endm
        
; requires single step operation, report = 0
;   set PC to the 1st instruction of the test
;   step to the WAI opcode, then manually tie the IRQ input low
;   continue to step until you see the PC advance, then remove IRQ
;   allow the routine to complete.

; WAI with interrupts disabled
        ldx #$ff
        txs
        ldy #3
        lda #0          ;IRQ not expected
        sta I_src
        set_stat intdis
        wai
        dey
        dey
        dey
        trap_ne         ;skipped opcodes!

        success
        
; WAI with interrupts enabled
        ldx #$ff
        txs
        ldy #7
        lda #2          ;IRQ expected
        sta I_src
        set_stat 0
        wai
        dey
        dey
        dey
        lda I_src
        trap_ne         ;IRQ vector not called
        dey
        trap_ne         ;skipped opcodes!

        success
        
; manual test for the STP opcode of the 65c02

stp     macro   
        db  $db         ;STP opcode
        endm
        
; set PC to the 1st instruction of the test, then run
        nop
        nop
        stp             ;expected end of operation
        nop
        nop
        trap            ;overran STP

;end of manual tests

;---------------------------------------------------------------------------
;trap in case of unexpected IRQ, NMI, BRK, RESET - IRQ, NMI, BRK test target
        dey
        dey
nmi_trap
    if NMI_bit < 0
        dey
        dey
        dey
        trap            ;unexpected NMI
    else
        php             ;either SP or Y count will fail, if we do not hit
        dey
        dey
        dey
        sta nmi_a       ;save regsters during NMI
        stx nmi_x
        pla
        pha
        sta nmi_f
        lda I_src       ;NMI expected?
        and #4   
        trap_eq         ;unexpexted NMI - check stack for conditions
        pla             ;test I-flag was set
        pha
        and #intdis
        trap_eq         ;I-flag not set
        pla             ;return with other flags reversed
        eor #m8-fai-decmode
        pha
        tsx        
        lda $102,x     ;test break on stack
        and #break
        trap_ne         ;unexpected B-flag! - this may fail on a real 6502
                        ;due to a hardware bug on concurrent BRK & NMI
        lda I_src       ;mark expected NMI has occured
        and #$ff-4
        sta I_src
        I_clr   NMI_bit   
        ldx nmi_x
        inx
        stx nmi_count
        lda #'I'        ;mark (NM)I
        plp             ;should be reversed by rti
        rti
    endif

res_trap
        trap            ;unexpected RESET
        
        dey
        dey
irq_trap                ;BRK & IRQ test
        php             ;either SP or Y count will fail, if we do not hit
        dey
        dey
        dey
        sta irq_a       ;save registers during IRQ/BRK
        stx irq_x
        pla
        pha
        sta irq_f
        lda I_src       ;IRQ expected?
        and #3   
        trap_eq         ;unexpexted IRQ/BRK - check stack for conditions
        pla             ;test I-flag was set
        pha
        and #intdis
        trap_eq         ;I-flag not set
        pla             ;return with other flags reversed
        eor #m8-fai-decmode
        pha        
        tsx
        lda $102,x      ;test break on stack
        and #break
        bne brk_trap
        
        lda I_src       ;IRQ expected?
        and #2   
        trap_eq         ;unexpexted IRQ - check stack for conditions
        lda I_src       ;mark expected IRQ has occured
        and #$ff-2
        sta I_src
        I_clr   IRQ_bit   
        ldx irq_x
        inx
        stx irq_count
        lda #'Q'        ;mark (IR)Q
        plp             ;should be reversed by rti
        rti
        
brk_trap
        lda I_src       ;break expected?
        and #1
        trap_eq         ;unexpected BRK - check stack for conditions
        lda I_src       ;mark expected BRK has occured
        and #$ff-1
        sta I_src
        ldx irq_x
        inx
        stx brk_count   
        lda irq_a
        lda #'K'        ;mark (BR)K
        plp             ;should be reversed by rti
        rti
        
    if report = 1
rep_int = 1
        include "report.i65"
    endif

        
;system vectors
    if (load_data_direct = 1)
        org $fffa
        dw  nmi_trap
        dw  res_trap
        dw  irq_trap
    else
vec_init
vec_bss equ $fffa
        dw  nmi_trap
        dw  res_trap
        dw  irq_trap
    endif
    
        end start
        
    