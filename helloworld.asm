; ==== DISPLAY TEST

.org 0x000

.macro  jmp address
        SUB PC, _ref - address
_ref:
.endmacro

.proc
reset:  JSR setup_screen
        SET B, screen
        SET A, data
_loop:  SET [B], [A]
        BOR [B], $C000
        ADD B, 1
        ADD A, 1
        IFN [A], 0
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
        JMP _loop
.endproc

; === DATA SECTION
data: .data "Hello World", 0

; === UNINTIALIZED SECTION 
screen: .bss 0x180
