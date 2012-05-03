COAS - The 0x10c assembler (python based)
=========================================

COAS is a basic assembler for the 0x10c DCPU.  Currently, it supports a the v1.7 DCPU standard
and a good number of directives.

Current list of features
------------------------

* Binary output in big and little endian raw files, IntelHEX and notch assembly `dat` output
* Macros
* Expressions
* Local scoping of labels
* `.org`, `.align` and `.bss` operations for fine tuned control over your memory map
* Includes of raw source as well as binaries
* Relocation table generation
* Linkage table generation
* String and Character literals (`.data`, `.big` and `.little` blocks)

The syntax is similar to the dos assembler "TASM".  I chose this mostly because it is what I
am most familiar with.  To keep the impedance level down for new developers, or porting existing
assembling to a minimum.

Example Source
--------------

            .org 0x0000
            .include "system.inc"

    .macro RET
            SET PC, POP
    .endmacro

    reset:  JSR setup
            ADD PC, -1

    
    ; === Setup screen and devices

            .proc
    :setup  ; We support pre-and-post colon labels.
            RET
            .endproc

Directives
----------

`.org` address`
Redefine the origin address for the following instructions and labels (defaults to 0)

`.align block-width`
Pad out the current data to N bytes to maintain even alignment.

`.bss block-size`
Defines a space without create actual binary data (useful for creating uninitialized data sections)

`.include "filename"`
Insert the contents of a file inside of the current assembly

`.incbytes "filename"`
Insert the contents of a file inside of the current assembly, converting each byte to a word of data

`.incbig "filename"`
Insert the contents of a file in big-endian format as a data block of words

`.inclittle "filename"`
Insert the contents of a file in little-endian format as a data block of words

`.data expression, expression, expression`
Insert a series of words into the assembly (each value truncated to 16bits)

`.big expression, expression, expression`
For each two expression, treat them as a series of bytes and pack them into words (big-endian order).

`.little expression, expression, expression`
For each two expression, treat them as a series of bytes and pack them into words (little-endian order)

`.equ word expression`
Assign the value of a word to the value of an expression, doing a replacement across the entire project

`.proc ... .endproc`
Creates local scoping, making any label beginning with '_' only be visible inside of the block

`.macro word arg, arg, arg  ... .endmacro`


Installation Instructions
-------------------------

If you just want to run it:

````
pip install git+git://github.com/asterick/coas.git
````

For development:

````
git clone git://github.com/asterick/coas.git
# install it as editable
pip install -e coas
````
