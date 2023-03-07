; #------------------------------------------------------------------------# <:t17,25,41,45:>
; |                                                                        |
; |   TESTCASE.I                                                           |
; |                                                                        |
; |   Include test file.                                                   |
; |                                                                        |
; |   Copyright 1996-2005, Frank A. Kingswood                              |
; |   http://www.kingswood-consulting.co.uk/assemblers/                    |
; |                                                                        |
; #------------------------------------------------------------------------#
;
; File created 22-feb-96

; ----- more number bases --------------------------------------------------

SomeLabel       dd      0x1
                dd      0x10
                dd      0x100
                dd      0x1000
                dd      0xabcdef

AnotherLabel:   dd      0b1
                dd      0b10
                dd      0b100
                dd      0b1000
                dd      0b10101001010101010101001

                dd      2#1
                dd      2#10
                dd      2#100
                dd      2#1000

                dd      3#1
                dd      3#10
                dd      3#100
                dd      3#1000
                dd      3#12

                dd      4#1
                dd      4#10
                dd      4#100
                dd      4#1000
                dd      4#123

                dd      5#1
                dd      5#10
                dd      5#100
                dd      5#1000
                dd      5#1234

                dd      6#1
                dd      6#10
                dd      6#100
                dd      6#1000
                dd      6#2345

                dd      7#1
                dd      7#10
                dd      7#100
                dd      7#1000
                dd      7#3456

                dd      8#1
                dd      8#10
                dd      8#100
                dd      8#1000
                dd      8#4567

                dd      9#1
                dd      9#10
                dd      9#100
                dd      9#1000
                dd      9#5678

                dd      10#1
                dd      10#10
                dd      10#100
                dd      10#1000
                dd      10#6789

                dd      11#1
                dd      11#10
                dd      11#100
                dd      11#1000
                dd      11#789a

                dd      12#1
                dd      12#10
                dd      12#100
                dd      12#1000
                dd      12#89ab

                dd      13#1
                dd      13#10
                dd      13#100
                dd      13#1000
                dd      13#9abc

                dd      14#1
                dd      14#10
                dd      14#100
                dd      14#1000
                dd      14#abcd

                dd      15#1
                dd      15#10
                dd      15#100
                dd      15#1000
                dd      15#bcde

                dd      16#1
                dd      16#10
                dd      16#100
                dd      16#1000
                dd      16#cdef

                dd      17#1
                dd      17#10
                dd      17#100
                dd      17#1000
                dd      17#defg

                dd      18#1
                dd      18#10
                dd      18#100
                dd      18#1000
                dd      18#efgh

                dd      19#1
                dd      19#10
                dd      19#100
                dd      19#1000
                dd      19#fghi

                dd      20#1
                dd      20#10
                dd      20#100
                dd      20#1000
                dd      20#ghij

                dd      21#1
                dd      21#10
                dd      21#100
                dd      21#1000
                dd      21#hijk

                dd      22#1
                dd      22#10
                dd      22#100
                dd      22#1000
                dd      22#ijkl

                dd      23#1
                dd      23#10
                dd      23#100
                dd      23#1000
                dd      23#jklm

                dd      24#1
                dd      24#10
                dd      24#100
                dd      24#1000
                dd      24#klmn

                dd      25#1
                dd      25#10
                dd      25#100
                dd      25#1000
                dd      25#lmno

                dd      26#1
                dd      26#10
                dd      26#100
                dd      26#1000
                dd      26#mnop

                dd      27#1
                dd      27#10
                dd      27#100
                dd      27#1000
                dd      27#nopq

                dd      28#1
                dd      28#10
                dd      28#100
                dd      28#1000
                dd      28#opqr

                dd      29#1
                dd      29#10
                dd      29#100
                dd      29#1000
                dd      29#pqrs

                dd      30#1
                dd      30#10
                dd      30#100
                dd      30#1000
                dd      30#qrst

                dd      31#1
                dd      31#10
                dd      31#100
                dd      31#1000
                dd      31#rstu

                dd      32#1
                dd      32#10
                dd      32#100
                dd      32#1000
                dd      32#stuv

                dd      33#1
                dd      33#10
                dd      33#100
                dd      33#1000
                dd      33#tuvw

                dd      34#1
                dd      34#10
                dd      34#100
                dd      34#1000
                dd      34#uvwx

                dd      35#1
                dd      35#10
                dd      35#100
                dd      35#1000
                dd      35#vwxy

                dd      36#1
                dd      36#10
                dd      36#100
                dd      36#1000
                dd      36#wxyz

                if ERRORS
                dd      37#1
                dd      37#10
                dd      37#100
                dd      37#1000

                dd      1#1
                dd      1#10
                dd      1#100
                dd      1#1000

                dd      0#1
                dd      0#10
                dd      0#100
                dd      0#1000
                endif


; ----- if ... else ... endif ----------------------------------------------

                if      5=6
                db      0
                if      0
                db      1
                else
                db      2
                endif
                db      3
                else
                db      4
                if      1
                db      5
                else
                db      6
                endif
                db      7
                endif


; ----- list, nolist -------------------------------------------------------

                nolist
                ; comment not listed
                db      10

                list
                ; comment is listed
                db      10


; ----- opt, noopt ---------------------------------------------------------

                noopt
                opt


; ----- nop ----------------------------------------------------------------

                nop
                nop     3
                

; ----- struct -------------------------------------------------------------

                struct  ListNode
                dw      LN_Next
                dw      LN_Previous
                db      LN_Type
                end struct


; ----- rubbish in inactive if-clause --------------------------------------
                
                if 0
 !"#$%&'()*+,-./@"
0123456789:;<=>?
@ABCDEFGHIJKLMNO
PQRSTUVWXYZ[\]^_
`abcdefghijklmno
pqrstuvwxyz{|}~
ÄÅÇÉÑÖÜáàâäãåçéè
êëíìîïñóòôöõúùûü
†°¢£§•¶ß®©™´¨≠ÆØ
∞±≤≥¥µ∂∑∏π∫ªºΩæø
¿¡¬√-≈∆«»… ÀÃ-Œœ
–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ
‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔ
ÒÚÛÙıˆ˜¯˘.˚¸˝˛ˇ
                endif


; +------------------------------------------------------------------------+
; |                                                                        |
; |   Macros.                                                              |
; |                                                                        |
; +------------------------------------------------------------------------+

; ----- macro definition ---------------------------------------------------

macro_one       macro   name,address,city,phone
                local   p_name,p_address,p_city,p_phone
                dw      p_name,p_address,p_city
                if      \0<4
                dw      -1
                else
                dw      p_phone
                endif
p_name          db      name,"\n"
p_address       db      address,"\n"
p_city          db      city,"\n"
p_phone         db      'phone\n'       ; value substituted
                db      "phone\n"       ; no substitution here
                endm

MACRO2          macro
                db      \0              ; number of parameters
test_\1_label   db      "\1",0          ; watch the label
uniq_\?         db      \2
                if      \0<3
                exitm
                endif
                db      \3
                endm

MacroLoop       macro   count
                if      count<1
                exitm
                endif
                db      count
                MacroLoop count-1
                endm

; ----- macro expansion ----------------------------------------------------

                macro_one "john doe","one street","city"
                macro_one "mary doe","one street","city","1-800-555-1212"

                MACRO2  11,22,"text\r\n"
                MACRO2  111,222

; ----- huge nested macro expansion ----------------------------------------

                MacroLoop 12

; ----- cmap character map -------------------------------------------------

                db      "Hello\0"
                
                cmap    ; restore normal map
                cmap    "a","ABCDEFGHIJKLMNOPQRSTUVWXYZ"     ; map lower case to upper
                
                db      "Hello\0"       ; only DB will be affected by CMAP instruction
                
                cmap    -1              ; set all map entries to -1
                cmap    " ",0           ; fill in entries
                cmap    "0",1,2,3,4,5,6,7,8,9,10
                cmap    "A",11,12,13,14,15,16,17,18,19,20,21,22,23,24,1,25,26,27,28,29,30,31,32,33,34,35
                cmap    "a",11,12,13,14,15,16,17,18,19,20,21,22,23,24,1,25,26,27,28,29,30,31,32,33,34,35
                
                db      "Hello\0"
                
                cmap    ; back to normal

; ----- non-text include files ---------------------------------------------

                include "testincl.bin"
                ; testincl.bin is generated by one-line source file:
                ;
                ; db    "Binary include file",13,10

                include "testincl.s19"
                ; testincl.s19 is generated by one-line source file:
                ;
                ; db    "S-Records include file",13,10
                
                include "testincl.hex"
                ; testincl.hex is generated by one-line source file:
                ;
                ; db    "intel-hex include file",13,10

; ----- EOF -----
