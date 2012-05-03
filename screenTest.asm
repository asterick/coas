; ==== DISPLAY TEST

.org 0x000

.macro  jmp address
        SUB PC, _ref - address
_ref:
.endmacro

.data "Hello World"

.proc
reset:  JSR setup_screen
        SET B, 0
        SET A, 0xC900
        SET A, [A]
_loop:  SET [B+screen], A
        ADD B, 1
        ADD A, 1
        SET C, A
        AND C, 0x80
        ADD A, C
        IFN B, 0x180
            JMP _loop
.endproc

crash:  jmp crash

.proc
setup_screen:
        HWN I
_loop:  SUB I, 1
        IFU I, 0
            SET PC, POP
        HWQ I

        ; Locate a LEM-1802
        IFN A, $f615
        IFN B, $7349
        IFN C, $1802
            JMP _loop
        
        ; Set screen mapping
        SET A, 0
        SET B, screen
        HWI I

        ; Set character mapping
        SET A, 1
        SET B, char
        HWI I
        SET A, 4
        SET B, char
        HWI I
        
        JMP _loop
.endproc

screen: .bss 0x180
char:   .bss 0x100
