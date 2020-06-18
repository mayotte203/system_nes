;
; example.s
; Brad Smith (rainwarrior), 4/06/2014
; http://rainwarrior.ca
;
; This is intended as an introductory example to NES programming with ca65.
; It covers the basic use of background, sprites, and the controller.
; This does not demonstrate how to use sound.
;
; This is not intended as a ready-made game. It is only a very minimal
; playground to assist getting started in NES programming. The idea here is
; to get you past the most difficult parts of a minimal NES program setup
; so that you can experiment from an almost blank slate.
;
; To use your own graphics, replace the two 4k tile banks provided.
; They are named "background.chr" and "sprite.chr".
;
; The reset and nmi routines are provided as a simple working example of
; these things. Writing these from scratch is a more advanced topic, so they
; will not be fully explained here.
;
; Under "drawing utilities" are some very primitive routines for working
; with the NES graphics. See the "main" section for examples of how to use them.
;
; Finally at the bottom you will find the "main" section that provides
; a small example program. A cursor is shown. Pressing the d-pad will move
;   - pressing the d-pad will move the cursor around the screen
;   - pressing B will draw a tile to the screen
;   - pressing A will draw several tiles to the screen
;   - pressing SELECT will reset the background
;   - holding START will demonstrate scrolling
;
; Please note that this example code is intended to be simple, not necessarily
; efficient. I have tried to avoid optimization in favour of easier to understand code.
;
; You may notice some odd behaviour when using the A button around the edges of the screen.
; I will leave it as an exercise for the curious to understand what is going on.
;

;
; iNES header
;

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;
; CHR ROM
;

.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"

;
; vectors placed at top 6 bytes of memory area
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; reset routine
;

.segment "CODE"
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	;jmp @nmi_end ; disable nmi for now
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	; palettes
	lda #%10001000
	sta $2000 ; set horizontal nametable increment
	lda $2002
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta $2000
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	; enable rendering
	lda #%00011110
	sta $2001
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

;
; irq
;

.segment "CODE"
irq:
	rti

;
; drawing utilities
;

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta $2006 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

;/\ - bbbradsmith's beautiful work
; main
;\/ - my disgrace

.segment "RODATA"
example_palette:
.byte $0F,$30,$30,$10 ; bg0 my custom ; i dont have the resolve to mess with this
.byte $0F,$30,$30,$10 ; bg1 my custom
.byte $0F,$30,$30,$10 ; bg2 my custom
.byte $0F,$30,$30,$10 ; bg3 my custom
.byte $0F,$18,$11,$38 ; sp0 my custom
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

.segment "ZEROPAGE"
cursor_x:		.res 1
cursor_y:		.res 1
temp_x:			.res 1
temp_y:			.res 1
signature_x:	.res 1
signature_y:	.res 1
color_counter:	.res 1

.segment "CODE"
main:
	; setup 
	ldx #0
	:
		lda example_palette, X
		sta palette, X
		inx
		cpx #32
		bcc :-
	; show the screen
	jsr setup_background
	lda #180
	sta signature_x
	lda #200
	sta signature_y
	jsr draw_signature
	jsr ppu_update
	; main loop
	APUSTATUS	= $4015
	ldx #$00
    stx APUSTATUS	
	lda #$01		; enable pulse 1
	sta $4015
    lda #$ff		; period
    sta $4002
    lda #$20
    sta $4003
    lda #$bf		; volume
    sta $4000
	lda #<pattern	; u can get low and high bytes of label with <>
	sta $50
	lda #>pattern
	sta $51
	ldx #$00
@loop:
	txa
	pha
	tay
	lda ($50), y ;addressing is hard
	tay
	pla
	tax
	cpy #$00
	beq @no_note
	lda pulse_notes_low, y		; note period
    sta $4002
    lda pulse_notes_high, y
    sta $4003		; causes phase reset and click ; idk how to fix it and idc now
@no_note:	
	pha
	txa
	pha
	tya
	pha
	ldx #$00
	ldy #$00
	lda #$3A  ; ~138bpm i think
 	jsr delay_axy_clocks ;delay code from nesdev
	pla
	tay
	pla
	tax
	pla
	inx
	cpx $00
	bne @same_pattern
	inc $51
	lda $51
	clc
	sbc #>pattern
	cmp #$02
	beq @end_dmc
@same_pattern:
	inc signature_x
	inc signature_x
	inc signature_x
	txa
	pha
	tya
	pha
	inc color_counter
	lda color_counter
	cmp #3
	bne @no_color_change
	inc palette+$12
	lda palette+$12
	cmp #$1d
	bne @no_color_overflow
	lda #$11
@no_color_overflow:
	sta palette+$12
	lda #$0
	sta color_counter
@no_color_change:
	jsr draw_signature
	jsr ppu_update
	pla
	tay
	pla
	tax
	jmp @loop
@end_dmc:
	jsr ppu_update
	lda #$00		; disable pulse 1
	sta $4015
	lda #$0F         ; pitch
	sta $4010
	lda #$1F         ; mark output
    sta $4011
	lda #$00        ; address $C000
    sta $4012
    lda #$FF         ; samples count
    sta $4013
	lda #$10        ; start DMC
	sta $4015
@end:
	jmp @end
	
;pulse channel timer values
;         C Major/A Minor ; why there is A#
;	      G3  A3  B3  C4  D4  E4  F4  G4  A4  B4  C5  D5  E5  F5 ;A#4  A5  G5  A#3 A#5 ;
pulse_notes_low:
.byte $00,$3B,$FD,$C6,$AC,$7E,$54,$41,$1E,$FF,$E3,$D7,$BF,$AB,$A1,$F0,$7f,$8E,$E0,$78
pulse_notes_high:
.byte $00,$02,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00

pattern: ;i should have been just .incbin this like a normal human being
;0
.byte $05,$00,$00,$00 ;00
.byte $00,$00,$02,$05 ;04
.byte $07,$00,$00,$00 ;08
.byte $00,$00,$05,$08 ;0C
.byte $09,$00,$08,$00 ;10
.byte $07,$00,$06,$00 ;14
.byte $07,$00,$00,$00 ;18
.byte $05,$00,$00,$00 ;1C
.byte $08,$00,$00,$00 ;20
.byte $00,$00,$00,$00 ;24
.byte $07,$05,$01,$05 ;28
.byte $08,$05,$01,$08 ;2C
.byte $07,$00,$00,$00 ;30
.byte $00,$00,$00,$00 ;34
.byte $08,$04,$02,$04 ;38
.byte $07,$04,$02,$07 ;3C
;1
.byte $05,$00,$00,$00
.byte $00,$00,$02,$05
.byte $07,$00,$00,$00
.byte $00,$00,$05,$08
.byte $09,$00,$08,$00
.byte $07,$00,$06,$00
.byte $07,$00,$00,$00
.byte $05,$00,$00,$00
.byte $08,$00,$00,$00
.byte $00,$00,$00,$00
.byte $07,$05,$01,$05
.byte $08,$05,$01,$05
.byte $07,$00,$00,$00
.byte $00,$00,$00,$00
.byte $08,$04,$02,$04
.byte $07,$04,$07,$09
;2
.byte $0F,$00,$00,$00
.byte $00,$00,$09,$08
.byte $07,$00,$00,$00
.byte $00,$00,$00,$00
.byte $09,$00,$00,$00
.byte $00,$00,$08,$07
.byte $05,$00,$00,$00
.byte $00,$00,$00,$00
.byte $08,$05,$08,$09
.byte $08,$05,$08,$09
.byte $08,$05,$08,$0F
.byte $08,$05,$08,$0F
.byte $08,$05,$08,$0B
.byte $08,$05,$08,$0B
.byte $0F,$08,$0F,$0C
.byte $0F,$08,$0F,$0C
;3
.byte $0E,$00,$00,$00
.byte $00,$00,$0D,$0C
.byte $0F,$00,$00,$00
.byte $00,$00,$0B,$0C
.byte $0E,$00,$00,$00
.byte $0D,$00,$00,$00
.byte $0C,$00,$0B,$00
.byte $0F,$09,$08,$0F
.byte $0C,$0F,$08,$0F
.byte $0C,$0F,$08,$0F
.byte $0D,$0F,$08,$0F
.byte $0D,$0F,$08,$0D
.byte $0E,$0B,$09,$0B
.byte $0E,$0B,$09,$0E
.byte $0D,$0B,$06,$0B
.byte $0D,$0B,$06,$0D
;4
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$0B
.byte $0E,$00,$0B,$09
.byte $07,$04,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$06,$04
.byte $0D,$0B,$0F,$09
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$0F
.byte $07,$04,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$04,$02
.byte $0D,$0B,$0F,$09
;5
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$09
.byte $07,$04,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$06,$04
.byte $0D,$0B,$0F,$09
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$09
.byte $07,$04,$10,$00
.byte $11,$0E,$0D,$0C
.byte $0B,$0F,$09,$08
.byte $0D,$0B,$0F,$09
;6
.byte $0E,$00,$0B,$0F
.byte $07,$05,$0D,$00
.byte $0B,$0F,$07,$05
.byte $0E,$00,$0B,$0F
.byte $07,$05,$11,$00
.byte $0C,$0B,$0F,$07
.byte $0E,$00,$0C,$0B
.byte $0D,$00,$0F,$07
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$09
.byte $07,$04,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$06,$04
.byte $0D,$0B,$0F,$09
;7
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$0F
.byte $09,$07,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$06,$04
.byte $0D,$0B,$0F,$09
.byte $0E,$00,$0B,$09
.byte $07,$04,$0D,$00
.byte $0B,$09,$07,$04
.byte $0E,$00,$0B,$0F
.byte $09,$07,$10,$00
.byte $11,$0E,$0D,$0B
.byte $09,$07,$06,$04
.byte $0B,$0F,$09,$07
;8
.byte $0C,$00,$0F,$09
.byte $07,$05,$0D,$00
.byte $0F,$09,$07,$05
.byte $0E,$00,$00,$00
.byte $0C,$00,$00,$00
.byte $0F,$09,$07,$05
.byte $10,$0E,$0C,$0F
.byte $09,$07,$05,$02
.byte $0C,$00,$0F,$09
.byte $07,$05,$0D,$00
.byte $0F,$09,$07,$05
.byte $0E,$00,$00,$00
.byte $0D,$00,$00,$00
.byte $0C,$09,$07,$05
.byte $0E,$0D,$0C,$0F
.byte $09,$07,$05,$07
;9
.byte $0C,$00,$0F,$09
.byte $07,$05,$0D,$00
.byte $0F,$09,$07,$05
.byte $0C,$00,$00,$00
.byte $0E,$00,$00,$00
.byte $0F,$09,$07,$05
.byte $09,$0E,$0C,$0F
.byte $09,$07,$05,$02
.byte $0C,$00,$0F,$09
.byte $07,$05,$0D,$00
.byte $0F,$09,$07,$05
.byte $0E,$00,$00,$00
.byte $0B,$00,$00,$00
.byte $0F,$09,$07,$04
.byte $0B,$0F,$09,$08
.byte $07,$06,$05,$04
;A
.byte $0C,$00,$0F,$07
.byte $05,$12,$0C,$00
.byte $0B,$0F,$07,$05
.byte $0E,$00,$0C,$0F
.byte $09,$07,$11,$00
.byte $0C,$0F,$07,$05
.byte $10,$0E,$0D,$0B
.byte $0F,$08,$05,$04
.byte $0C,$00,$09,$07
.byte $05,$12,$0D,$00
.byte $0B,$0F,$07,$05
.byte $11,$00,$0C,$0F
.byte $08,$05,$13,$00
.byte $0E,$0C,$0F,$08
.byte $10,$00,$0E,$0D
.byte $0B,$0F,$09,$08
;B
.byte $0C,$00,$0F,$07
.byte $05,$12,$0D,$00
.byte $0B,$0F,$07,$05
.byte $11,$00,$0C,$0F
.byte $09,$07,$11,$00
.byte $0C,$0F,$07,$05
.byte $10,$0E,$0D,$0B
.byte $0F,$08,$05,$04
.byte $0C,$00,$0F,$07
.byte $05,$12,$0D,$00
.byte $0B,$0F,$07,$05
.byte $11,$00,$0C,$0F
.byte $09,$07,$13,$00
.byte $0C,$0F,$07,$05
.byte $10,$0E,$0D,$0B
.byte $0F,$08,$05,$04

delay_axy_clocks: ; 13×(65536×Y + 256×A + X) + 18 cycles delay
	iny
@l1:  
	nop
    nop
@l2: 
    cpx #1
    dex
    sbc #0
    bcs @l1
    dey
    bne @l2
	rts

draw_code:
	lda #160
	ldy #5 
	:
		pha 
		ldx #4 
		jsr ppu_address_tile
		pla 
		ldx #4
		:
			sta $2007
			clc
			adc #$01
			inx
			cpx #(15)
			bcc :-
		clc
		adc #$05
		iny
		cpy #(8)
		bcc :--
		
	ldy #5 
	:
		pha 
		ldx #15 
		jsr ppu_address_tile
		pla
		ldx #15
		:
			sta $2007
			clc
			adc #$01
			inx
			cpx #(29)
			bcc :-
		clc
		adc #$02
		iny
		cpy #(8)
		bcc :--
	rts

draw_signature:
	
	lda signature_x
	ldx #0
	ldy #0
@draw_signature_high:
	pha
	lda signature_y
	sta oam, x
	inx
	tya
	clc
	adc #128
	sta oam, x
	inx
	lda #%00000000
	sta oam, x
	inx
	pla
	sta oam, x
	inx
	clc
	adc #8
	iny
	cpy #8
	bne @draw_signature_high
	
	lda signature_x
	ldx #32
	ldy #0
@draw_signature_low:
	pha
	lda signature_y
	clc
	adc #8
	sta oam, x
	inx
	tya
	clc
	adc #144
	sta oam, x
	inx
	lda #%00000000
	sta oam, x
	inx
	pla
	sta oam, x
	inx
	clc
	adc #8
	iny
	cpy #8
	bne @draw_signature_low	
	rts

setup_background:
	; first nametable, start by clearing to empty
	lda $2002 ; reset latch
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	; empty nametable
	lda #09
	ldy #30 ; 30 rows
	:
		ldx #32 ; 32 columns
		:
			sta $2007
			dex
			bne :-
		dey
		bne :--
	; set all attributes to 0
	ldx #64 ; 64 bytes
	:
		sta $2007
		dex
		bne :-
	; draw logo
	lda #0
	ldy #11 ; start at row 11
	:
		pha ; temporarily store A, it will be clobbered by ppu_address_tile routine
		ldx #12 ; start at column 12
		jsr ppu_address_tile
		pla ; recover A
		; write a line of logo
		ldx #12
		:
			sta $2007
			clc
			adc #$01
			inx
			cpx #(20)
			bcc :-
		clc
		adc #$08
		iny
		cpy #(19)
		bcc :--
		
	; idk
	lda #$24
	sta $2006
	lda #$00
	sta $2006
	lda #$00
	ldy #30
	:
		ldx #32
		:
			sta $2007
			dex
			bne :-
		dey
		bne :--
	; 
	lda #0
	ldy #4
	:
		ldx #16
		:
			lda #0
			sta $2007
			dex
			bne :-
		dey
		bne :--
	jsr draw_code
	rts
	
.segment "DMC"
dmc:
.incbin "spread.dmc" ;rjdmc generated
;
; end of file
;
