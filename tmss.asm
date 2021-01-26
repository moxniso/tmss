; Trademark Security System bootrom for Sega Genesis
; (C) 1990 Sega of America

; Disassembled, cleaned up, and commented by Moxniso using Ghidra v9.2.1 (https://ghidra-sre.org/) 
; Special thanks to wiki.megadrive.org for label naming help
; Last updated: January 25 2021

; Macros to make code easier to read
RAM_Start equ 0x00FF0000
RAM_End equ 0x00FFFFFF
TMSS_Address equ 0x00A14101
VDP_Control equ 0x00C00004
VDP_Data equ 0x00C00000

ROM_Start:

	; CPU vectors

	dc.l   0x00FFFF00       ; Initial stack pointer value
        dc.l   _start           ; Start of program (ROM offset 0x202)
        dc.l   null      ; Bus error
        dc.l   null      ; Address error
        dc.l   null      ; Illegal instruction
        dc.l   null      ; Division by zero
        dc.l   null      ; CHK CPU_Exception
        dc.l   null      ; TRAPV CPU_Exception
        dc.l   null      ; Privilege violation

	dc.l   null  ; TRACE exception
        dc.l   null  ; Line-A emulator
        dc.l   null  ; Line-F emulator
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Unused (reserved)
        dc.l   null  ; Spurious exception
        dc.l   null  ; IRQ level 1
        dc.l   null  ; IRQ level 2
        dc.l   null  ; IRQ level 3
        dc.l   null ; IRQ level 4 (horizontal retrace interrupt)
        dc.l   null  ; IRQ level 5
        dc.l   null ; IRQ level 6 (vertical retrace interrupt)
        dc.l   null  ; IRQ level 7
        dc.l   null  ; TRAP #00 exception
        dc.l   null  ; TRAP #01 exception
        dc.l   null  ; TRAP #02 exception
        dc.l   null  ; TRAP #03 exception
        dc.l   null  ; TRAP #04 exception
        dc.l   null  ; TRAP #05 exception
        dc.l   null  ; TRAP #06 exception
        dc.l   null  ; TRAP #07 exception
        dc.l   null  ; TRAP #08 exception
        dc.l   null  ; TRAP #09 exception
        dc.l   null  ; TRAP #10 exception
        dc.l   null  ; TRAP #11 exception
        dc.l   null  ; TRAP #12 exception
        dc.l   null  ; TRAP #13 exception
        dc.l   null  ; TRAP #14 exception
        dc.l   null  ; TRAP #15 exception
        dc.l   null  ; The rest are unused/reserved...
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null
        dc.l   null

	; ROM header
	dc.b "SEGA GENESIS    " ; Console name (16)
        dc.b "(C)SEGA 1990.MAY" ; Author and copyright year (16)
        dc.b "GENESIS OS                                       " ; Domestic name (48)                       
        dc.b "GENESIS OS                                       " ; International name (48)
        dc.b "GM 00000000-00" ; Serial number (2, 12)
        dc.w 0x5b74 ; Checksum (2)
        dc.b "                " ; I/O support (16)
        dc.l ROM_Start ; ROM Start address (4)
        dc.l ROM_End-1  ; ROM End address (4)
        dc.l RAM_Start ; Backup RAM start address (4)
        dc.l RAM_End ; Backup RAM end address (4)

        dc.l 0x00000000 ; Start address of SRAM - normally 0x20001
        dc.l 0x00000000 ; End address of SRAM - (rom_start + (sram_size*2))
        dc.w 0x0000 ; SRAM enable flag "RA" (2)
        dc.w 0x0000 ; 0xFA20 for SRAM on odd bytes (2)
        dc.b "                        " ; Modem support (24)
        dc.b "                                        " ; Memo (40)
        dc.b "U               "                                 ; Country codes (16)   

null: ; Any CPU exceptions are thrown in this infinite loop
	bra null

_start:
	lea	vdp_regs, a5 
        movem.l (a5)+, d5/d6/d7/a0/a1/a2/a3/a4
        move.b  -$10FF(a1), d0 ; Read console version address at $A10001
        andi.b  #$0F, d0
        beq.b   @model_1
        move.l  #'SEGA', $2F00(a1) ; Enable the VDP 

	@model_1:
		move.w  (a4), d0
        	moveq   #0, d0
        	movea.l d0, a6
        	move.l  a6, usp ; Set stack to 0x0
        	
	moveq   #$00000017, d1 ; Set loop counter to 23
	@setup_vdp_regs:
		move.b  (a5)+, d5
        	move.w  d5, (a4)
        	add.w   d7, d5
        	dbf     d1, @setup_vdp_regs
        
	move.l  #$40000080, (a4)  ; Clear VRAM with VDP fill command
        move.w  d0, (a3)

	@wait_for_dma:
		move.w  (a4), d4
        	btst.l  #1, d4
        	bne.b   @wait_for_dma
        	
	move.l  #$81048F02, (a4) ; Set VDP autoincrement to 2 (one word) and disable DMA
        move.l  #$C0000000, (a4) ; Set VDP write to CRAM address
        moveq   #$0000001F, d3 ; Set loop counter to 31
	@clear_cram:
		move.l  d0, (a3)
        	dbf     d3, @clear_cram
        	
	move.l  #$40000010, (a4) ; Set VRAM write to 0x10
        moveq   #$00000013, d4 ; Set loop counter to 14
	@clear_vsram:
		move.l  d0, (a3)
        	dbf     d4, @clear_vsram
        	
	moveq  #3, d5 ; Set loop counter to 4
	@something_psg:
		move.b  (a5)+, $11(a3) ; (Most likely) sets the 4 PSG channels to max attentuation, which is zero volume
        	dbf d5, @something_psg
        
	bra.b   jmp_over_regs

vdp_regs:
	dc.l    0x8000      ;D5
        dc.l    0x3FFF      ;D6
        dc.l    0x0100      ;D7
        dc.l    0xA00000    ;A0

        dc.l    0xA11100    ;A1
        dc.l    0xA11200    ;A2 (Predicted offset)
        dc.l    VDP_Data    ;A3 (Predicted offset)
        dc.l    VDP_Control ;A4

	dc.b 0x04 ; 0x00:  H interrupt on, palettes on
        dc.b 0x14 ; 0x01:  V interrupt on, display on, DMA on, Genesis mode on
        dc.b 0x30 ; 0x02:  Pattern table for Scroll Plane A at VRAM 0xC000 (bits 3-5 = bits 13-15)
        dc.b 0x3C ; 0x03:  Pattern table for Window Plane at VRAM 0x0000 (disabled) (bits 1-5 = bits 11-15)
        dc.b 0x07 ; 0x04:  Pattern table for Scroll Plane B at VRAM 0xE000 (bits 0-2 = bits 11-15)
        dc.b 0x6C ; 0x05:  Sprite table at VRAM 0xF000 (bits 0-6 = bits 9-15)
        dc.b 0x00 ; 0x06:  Unused
        dc.b 0x05 ; 0x07:  Background colour: bits 0-3 = colour, bits 4-5 = palette
        dc.b 0x00 ; 0x08:  Unused
        dc.b 0x00 ; 0x09:  Unused
        dc.b 0xFF ; 0x0A: Frequency of Horiz. interrupt in Rasters (number of lines travelled by the beam)
        dc.b 0x00 ; 0x0B: External interrupts off, V scroll fullscreen, H scroll fullscreen
        dc.b 0x81 ; 0x0C: Shadows and highlights off, interlace off, H40 mode (320 x 224 screen res)
        dc.b 0x37 ; 0x0D: Horiz. scroll table at VRAM 0xFC00 (bits 0-5)
        dc.b 0x00 ; 0x0E: Unused
        dc.b 0x01 ; 0x0F: Autoincrement 2 bytes
        dc.b 0x01 ; 0x10: Scroll plane size: 64x32 tiles
        dc.b 0x00 ; 0x11: Window Plane X pos 0 left (pos in bits 0-4, left/right in bit 7)
        dc.b 0x00 ; 0x12: Window Plane Y pos 0 up (pos in bits 0-4, up/down in bit 7)
        dc.b 0xFF ; 0x13: DMA length lo byte
        dc.b 0xFF ; 0x14: DMA length hi byte
        dc.b 0x00 ; 0x15: DMA source address lo byte
        dc.b 0x00 ; 0x16: DMA source address mid byte
        dc.b 0x80 ; 0x17: DMA source address hi byte, memory-to-VRAM mode (bits 6-7)
	dc.b 0x9F ; ???
	dc.b 0xBF ; ???
	dc.b 0xDF ; ???
	dc.b 0xFF ; ???

jmp_over_regs:
	lea     $FFFFC000.w, a0 ; Get RAM address to copy code to
        lea     loaded_code_and_regs(pc), a1
        movem.l (a1)+, d4/d5/d6/d7/a2/a3/a4/a5/a6 ; Set new registers
        
	move.w  #$003F, d0 ; Set loop counter to 63
	@copy_to_ram: 
		move.w	(a1)+, (a0)+ ; Copy hardcoded machine instructions at loaded_code_and_regs to work RAM  
		dbf	d0, @copy_to_ram	
	
	jsr 0xFFFFC000

trap: ; If the jump failed, PC falls in this infinite loop 
	bra.s trap

loaded_code_and_regs:
	dc.l    0x20534547    ;D4
        dc.l    0x45940003    ;D5 
        dc.l    0x000000F7    ;D6 | Might have something to do with d0

        dc.l    'SEGA'        ;D7
        dc.l    0x00A14000    ;A2
        dc.l    TMSS_Address  ;A3
        dc.l    VDP_Control   ;A4 (Predicted offset)
        dc.l    VDP_Data      ;A5 (Predicted offset)
        dc.l    0x00A10001    ;A6

test_cart:
	bset.b #0, (a3) ; Sets a3 to $A14101
	cmp.l (0x100).w, d7 ; Compare ROM offset 0x100 with 'SEGA'
	beq.b cart_ok

	cmp.l (0x100).w, d4 ; If 'SEGA' not found, try ' SEG'
	bne.b @skip ; Branches to @skip if this (and thus both) comparisons failed

	cmpi.b #'A', (0x104).w ; If ' SEG' comparison worked, check for the last 'A'
	beq.b cart_ok

	@skip: 
		bclr.b #0x0, (a3) ; Re-enable TMSS
		move.b (a6), d0 
		andi.b #0xF, d0
		beq.b @done
		move.l #0x0, (a2)
	@done:
		rts

cart_ok:
	bclr.b #0x0, (a3) ; Re-enable TMSS
	jsr cram_write ; Upload the white two-color palette to CRAM
	move.l #0x4c200000, (a4) ; Set VRAM write to 0xC20

	@write_map:
		move.l (a1)+, (a5)
		dbf d6, @write_map
	
	jsr write_license
	move.w #0x8144, (a4) ; VDP command to enable display
	move.w #0x3c, d0
	bsr.s delay
	move.w #0x8104, (a4) ; VDP command to disable display
	move.b (a6), d0
	andi.b #0xF, d0

	beq.b @begin_cart
	move.l #0x0, (a2) ; Write 0x0 to $A14000
	@begin_cart:
		bset.b #0x0, (a3) ; Disable TMSS for the game
		moveq #0x0, d0 ; Clear d0
		movea.l d0, a0
		movea.l (a0)+, sp ; Reset stack pointer to 0x0 (The game's vector table should set the new stack pointer)
		movea.l (a0)+, a0
		jmp (a0) ; Jump directly to game's start vector


; Kill time by having a long decrementing branch loop
delay:
	move.w #0x95CE, d1 ; Set loop counter to 38,350
	@delay_loop:
		dbf d1, @delay_loop
		dbf d0, delay
		rts

pal_and_font:
	dc.w 1 ; Palette size
	dc.w 0xEEE ; White
	dc.w 0xEE8 ; Kinda white

	; Text font
	dc.l 0x1111100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11111100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111100
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000000
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11111100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111100
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0x11111100
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0x11111100
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000000
	dc.l 0x11001110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0
	dc.l 0x110
	dc.l 0x110
	dc.l 0x110
	dc.l 0x110
	dc.l 0x110
	dc.l 0x1100110
	dc.l 0x1111110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11001100
	dc.l 0x11111000
	dc.l 0x11111000
	dc.l 0x11001100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0x1111110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11101110
	dc.l 0x11111110
	dc.l 0x11010110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11100110
	dc.l 0x11110110
	dc.l 0x11011110
	dc.l 0x11001110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0x11000000
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11001110
	dc.l 0x11001110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111100
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x11000110
	dc.l 0x11000000
	dc.l 0x11111110
	dc.l 0x110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11111110
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11111110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x1101100
	dc.l 0x111000
	dc.l 0x10000
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11010110
	dc.l 0x11111110
	dc.l 0x11101110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11101110
	dc.l 0x1111100
	dc.l 0x11101110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x11000110
	dc.l 0x1101100
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0x111000
	dc.l 0
	dc.l 0x11111110
	dc.l 0x1110
	dc.l 0x11100
	dc.l 0x111000
	dc.l 0x1110000
	dc.l 0x11100000
	dc.l 0x11111110
	dc.l 0
	dc.l 0
	dc.l 0
	dc.l 0
	dc.l 0
	dc.l 0
	dc.l 0x1100000
	dc.l 0x1100000
	dc.l 0
	dc.l 0x2222200
	dc.l 0x22000220
	dc.l 0x22000000
	dc.l 0x2222200
	dc.l 0x220
	dc.l 0x22000220
	dc.l 0x2222200
	dc.l 0
	dc.l 0x2222220
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x22222200
	dc.l 0x22000000
	dc.l 0x22000000
	dc.l 0x2222220
	dc.l 0
	dc.l 0x2222200
	dc.l 0x22000220
	dc.l 0x22000000
	dc.l 0x22002220
	dc.l 0x22000220
	dc.l 0x22000220
	dc.l 0x2222220
	dc.l 0
	dc.l 0x22000
	dc.l 0x222200
	dc.l 0x222200
	dc.l 0x2200220
	dc.l 0x2200220
	dc.l 0x22000022
	dc.l 0x22022222
	dc.l 0	

	dc.b "   produced by or" 
	dc.b 0xFF
	
	dc.b " under license from"
	dc.b 0xFF

	dc.b "sega,enterprises ltd{" ; The opening curly brace represents a period
	dc.b 0x00 ; Terminate string

cram_write: 
	move.w (a1)+, d0 ; Set loop counter to the palette size (0x1, aka 2 colors)
	move.l #0xC0020000, (a4) ; Set CRAM write to 0x2

	@cram_loop:
		move.w (a1)+, (a5)
		dbf d0, @cram_loop
		rts

write_license:
	move.l d5, (a4) ; Set VRAM write to 0xC594

	@main:
		moveq #0x0, d1
		move.b (a1)+, d1
		bmi.b @loop
		bne.b @set
		rts

	@set:
		move.w d1, (a5)
		bra.b @main

	@loop:
		addi.l #0x1000000, d5
		bra.b write_license

; Padding for checksum
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000
	dc.l 0x00000000

ROM_End:

