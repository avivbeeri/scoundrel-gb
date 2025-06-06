INCLUDE "hardware.inc"
INCLUDE "charmap.inc"

; Define a new section and hard-code it to be at $0040.
SECTION "VBlank Interrupt", ROM0[INT_HANDLER_VBLANK]
VBlankInterrupt:
	; This instruction is equivalent to `ret` and `ei`
  push af
	push bc
	push de
	push hl
	jp VBlankHandler
 
SECTION "VBlank Handler", ROM0
VBlankHandler:
	; Now we just have to `pop` those registers and return!
; Make sure to use `ldh` for HRAM and registers, and not a regular `ld`

	ldh a, [hFrameCounter]
	inc a
	ldh [hFrameCounter], a

	pop hl
	pop de
	pop bc
	pop af
	reti

SECTION "STAT Interrupt", ROM0[INT_HANDLER_STAT]
HBlankInterrupt:
  ; This needs to be super fast
	; This instruction is equivalent to `ret` and `ei`
	reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header

EntryPoint:
  di

  ; Initialize data to 0
  ld a, 0
	ldh [hGameState], a
	ldh [hFrameCounter], a
  ld [wCurKeys], a
  ld [wNewKeys], a

	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

  ; Enable the VBLANK interrupt
  ld a, IEF_VBLANK | IEF_STAT
	ldh [rIE], a

  ld a, STATF_LYC
  ldh [rSTAT], a

  ; Clear rIF for safety
  xor a, a ; This is equivalent to `ld a, 0`!
	ldh [rIF], a

  ei

	; Do not turn the LCD off outside of VBlank
  call WaitForVBlank

	; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

	; Copy the tile data
	ld de, Tiles
	ld hl, $8000
	ld bc, TilesEnd - Tiles
  call MemCopy

	; Copy the tilemap
	ld de, TitleTilemap
	ld hl, $9800
	ld bc, TitleTilemapEnd - TitleTilemap
  call MemCopy

	; Turn the LCD on
  ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
  ; Note that some of these constants (LCDCF_OBJOFF, LCDCF_WINOFF) are zero, but are included for clarity
  ld a, 127 - WX_OFS
  ld [rWY], a
  ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON | LCDCF_OBJOFF | LCDCF_WINOFF
  ldh [rLCDC], a      ; Enable and configure the LCD to show the background

	; During the first (blank) frame, initialize background palette
	ld a, %11100100
	ld [rBGP], a

GameLoop:
	halt
  ; Check which game state we're in
  ldh a, [hGameState]
  cp a, 1 ; 1 = Game
  jp nz, .skipGame
  call DrawGameState
  jp .endGameLoop
  ; Do game loop processing
.skipGame
  call DrawTitleState
  ; Default to the title screen
.endGameLoop
 
	jp GameLoop
; ----------------------------

DrawGameState:

  ret

; ----------------------------

DrawTitleState:
  ldh a, [hFrameCounter]

  bit 5, a
  jr z, .blank

  ; write or blank out text
	ld de, StartText
  ld hl, $9985
  ld bc, START_TEXT_LEN
  call MemCopy

  jp .end ; jump over this, "else"

.blank:
  ; re-copy text
  ld d, 0
  ld bc, START_TEXT_LEN
  ld hl, $9985
  call Memset

.end:

  call UpdateKeys

  ld a, [wCurKeys]
  and a, PADF_A | PADF_B | PADF_START
  jr z, .complete
.beginGame
  ; Change game state to GAME (1)
  ld a, 1
	ldh [hGameState], a

	; Turn the LCD off
  call WaitForVBlank
	ld a, 0
	ld [rLCDC], a
  ; Setup the game board
	ld de, GameTilemap
	ld bc, GameTilemapEnd - GameTilemap
  ld hl, $9800
  call MemCopy
  ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON | LCDCF_OBJOFF | LCDCF_WINON | LCDCF_WIN9C00
  ldh [rLCDC], a      ; Enable and configure the LCD to show the background

.complete:

  ret


; ----------------------------


WaitForVBlank::
	ld a, [rLY]
	cp 144
	jp c, WaitForVBlank
  ret

; ----------------------------

MemCopy::
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jr nz, MemCopy
  ret

; ----------------------------

Memset::
; d: value to set
; hl: destination start address
; bc: byte count to write 
  ld a, d
	ld [hli], a
	dec bc
	ld a, b
	or a, c
	jr nz, Memset
  ret

; ----------------------------


UpdateKeys::
  ; Poll half the controller
  ld a, P1F_GET_BTN
  call .onenibble
  ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a, P1F_GET_DPAD
  call .onenibble
  swap a ; A7-4 = unpressed directions; A3-0 = 1
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; And release the controller
  ld a, P1F_GET_NONE
  ldh [rP1], a

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret

.onenibble
  ldh [rP1], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
  ldh a, [rP1]
  ldh a, [rP1] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret

; ----------------------------


SECTION "Game Data", WRAM0, ALIGN[8]
; These are used for tracking input
wCurKeys: db
wNewKeys: db


SECTION "Constants", ROM0

def START_TEXT_LEN equ 10
StartText: DB "Push Start"

SECTION "Tile data", ROM0

Tiles: INCBIN "tiles.bin"
TilesEnd:

SECTION "Game Tilemap", ROM0
GameTilemap:
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $53, $54, $54, $55, $53, $54, $54, $55, $53, $54, $54, $55, $53, $54, $54, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $63, $4c, $00, $65, $63, $4c, $00, $65, $63, $4c, $00, $65, $63, $4c, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $63, $00, $00, $65, $63, $00, $00, $65, $63, $00, $00, $65, $63, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $63, $01, $01, $65, $63, $01, $01, $65, $63, $01, $01, $65, $63, $01, $01, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $73, $74, $74, $75, $73, $74, $74, $75, $73, $74, $74, $75, $73, $74, $74, $75, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $50, $51, $51, $52, $00, $00, $53, $54, $54, $55, $53, $54, $54, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $56, $57, $62, $00, $00, $63, $4c, $00, $65, $63, $4c, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $66, $67, $62, $00, $00, $63, $00, $00, $65, $63, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $76, $77, $62, $00, $00, $63, $01, $01, $65, $63, $01, $01, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $70, $71, $71, $72, $00, $00, $73, $74, $74, $75, $73, $74, $74, $75, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
GameTilemapEnd:

SECTION "Title Tilemap", ROM0
TitleTilemap:
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $85, $86, $87, $88, $87, $89, $8a, $8b, $8c, $8d, $00, $8e, $8f, $8d, $90, $89, $91, $92, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f, $a0, $a1, $a2, $a3, $a4, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $a5, $a6, $a7, $a8, $a9, $aa, $ab, $aa, $ac, $ad, $ab, $aa, $ab, $ae, $af, $b0, $b1, $b2, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $53, $54, $54, $54, $54, $54, $54, $54, $54, $54, $54, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $63, $1f, $44, $42, $37, $00, $22, $43, $30, $41, $43, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $73, $74, $74, $74, $74, $74, $74, $74, $74, $74, $74, $75, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
TitleTilemapEnd:



SECTION "Frame Counter", HRAM
hFrameCounter:
	db
hGameState:
	db
