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
  ld a, 1
	ldh [hVBlankFlag], a

	pop hl
	pop de
	pop bc
	pop af
	reti

SECTION "STAT Interrupt", ROM0[INT_HANDLER_STAT]
HBlankInterrupt:
  ; This needs to be super fast
	; This instruction is equivalent to `ret` and `ei`
  push af
	; During the first (blank) frame, initialize background palette
	ld a, %00011011
	ld [rBGP], a
  pop af
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
  ld a, IEF_VBLANK
	ldh [rIE], a

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
  ld a, 136 - WX_OFS
  ld [rWY], a
  ld [rLYC], a
  ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON | LCDCF_OBJOFF | LCDCF_WINOFF
  ldh [rLCDC], a      ; Enable and configure the LCD to show the background


	; During the first (blank) frame, initialize background palette
	ld a, %11100100
	ld [rBGP], a

GameLoop:
  call PauseForVBlank

	ld a, %11100100
	ld [rBGP], a

  call UpdateKeys

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
InitGameState:

	; Turn the LCD off
  call WaitForVBlank
  di

;; init health
  ld a, $14
  ld [wHealth], a

  ld a, 0
  ld [wFlag], a

  ; set room
  ld d, 0
  ld bc, 6
  ld hl, wCards
  call Memset

  ; initialize the deck with cards
  ; suit by suit

  ; 
  ;  cards are encoded $xz
  ;  x = suit 0:none 4:spade, 3:club, 1: heart, 2:diamonds, 5:special (joker?)
  ;  z = value, 2-14
  ld hl, wDeck
  ld c, $09 ; 
  ld b, $10 ; suit
  call InitSuit
  ld c, $09 ; 
  ld b, $20 ; suit
  call InitSuit
  ld c, $0D ; 
  ld b, $40 ; suit
  call InitSuit
  ld c, $0D ; 
  ld b, $30 ; suit
  call InitSuit

  ld a, 46
  ld [wDeckSize], a

  ld hl, wDeckTop
  ld a, LOW(wDeck)
  ld [hli], a
  ld a, HIGH(wDeck)
  ld [hl], a

 ; pointer for the bottom of the deck, should point at the next open slot in circular buffer
  ld hl, wDeckBottom
  ld a, LOW(wDeck)
  ld [hli], a
  ld a, HIGH(wDeck)
  ld [hl], a
  ld a, STATF_LYC
  ldh [rSTAT], a

  ; Enable the VBLANK interrupt
  ld a, IEF_VBLANK | IEF_STAT
	ldh [rIE], a

	ld a, 0
	ld [rLCDC], a

  ; Setup the game board
	ld de, GameTilemap
	ld bc, GameTilemapEnd - GameTilemap
  ld hl, $9800
  call MemCopy

	ld de, WindowTilemap
	ld hl, $9C00
	ld bc, WindowTilemapEnd - WindowTilemap
  call MemCopy

  ; shuffle the deck
  ; Set up rendering
  ld hl, CARD_ROOM_0 ; room card 1
  call ClearCardBorder
  ld hl, CARD_ROOM_1 ; room card 2
  call ClearCardBorder
  ld hl, CARD_ROOM_2 ; room card 3
  call ClearCardBorder
  ld hl, CARD_ROOM_3 ; room card 4
  call ClearCardBorder
  ld hl, CARD_WEAPON ; weapon card corner
  call ClearCardBorder
  ld hl, CARD_MONSTER ; weapon monster card
  call ClearCardBorder

  ; we're ready now
  ; enable the background
  ; enable the window
  ; turn on the display
  ld a, LCDCF_ON | LCDCF_BLK01 | LCDCF_BGON | LCDCF_OBJOFF | LCDCF_WINON | LCDCF_WIN9C00
  ldh [rLCDC], a      

  ei
  ret

; ----------------------------

DrawGameState:

  ld a, [wNewKeys]
  and a, PADF_LEFT
  jr z, :+
  ld b, 3
  call DecreaseHealth
:

  ld a, [wNewKeys]
  and a, PADF_RIGHT
  jr z, :+
  ld b, 4
  call IncreaseHealth
:

; render weapon value and suit

  ld a, 4 ;id = 4
  ld d, a ; save a copy for future lookups
  ; add a, a for an address word table, we multiple by two, 
  add a, LOW(wCards)
  ld l, a
  adc HIGH(wCards)
  sub l
  ld h, a
  ; dereference
  ld a, [hl]

  ld c, a ; c for card value, we save it before decoding
  swap a
  and a, $0F ; check the suit to see if a card is present
  jr z, .skipWeapon
.drawWeapon
  ld hl, CARD_WEAPON + SUIT_OFFSET
  add a, $4B
  ld [hl], a

  ld a, c
  and a, $0F ; check the suit to see if a card is present
  call BCDSplit
  ld hl, CARD_WEAPON + ONES_OFFSET
  inc a
  ld [hld], a
  ld a, b
  inc a
  ld [hl], a

  ld hl, CARD_WEAPON ; weapon card corner
  call DrawCardBorder
  jr .drawWeaponComplete
.skipWeapon
.drawWeaponComplete
  call DrawHealthBar
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


  ld a, [wCurKeys]
  and a, PADF_A | PADF_B | PADF_START
  jr z, .complete
.beginGame
  ; Change game state to GAME (1)
  ld a, 1
	ldh [hGameState], a

  call InitGameState
.complete:

  ret

; ---------------------------
DrawHealthBar:
; render current health from BCD
  ld a, [wHealth]
  call BCDSplit
  ld hl, HEALTH_ONES
  inc a
  ld [hld], a
  ld a, b
  inc a
  ld [hl], a
; --
; -- assumes we haven't got the health already
  ld c, 10
  ld a, [wHealth]
  ld b, a
  ld hl, HEALTH_FIRST_HEART
.printHeartLoopStart:
  xor a
  ld a, b
  cp a, $2
  jr c, :+
  dec b
  dec b
  ld a, $4C
  ld [hld], a
  jr .printHeartLoopEnd
:
  xor a
  ld a, b
  cp a, $1
  jr c, :+
  dec b
  ld a, $4B
  ld [hld], a
  jr .printHeartLoopEnd
:
  ld a, $4A
  ld [hld], a
  jr .printHeartLoopEnd
.printHeartLoopEnd
  dec c
  jr nz, .printHeartLoopStart
.printHeartLoopFinish

  ret
; ---------------------------
ClearCardBorder:
  ld b, 5
:
  xor a, a
  ld [hli], a
  ld [hli], a
  ld [hli], a
  ld [hli], a
  ld a, l
  add a, $1C
  ld l, a
  dec b
  jr nz, :-
.clearCardEnd
  ret
; ---------------------------
DrawCardBorder:
; draw card border
  ; row one corner
  ld a, $53
  ld [hli], a
  ld a, $54
  ld [hli], a
  ld a, $54
  ld [hli], a
  ld a, $55
  ld [hl], a
  ; row 2
  ld a, l
  add a, $1D
  ld l, a

  ld b, 3
:
  ld a, $63
  ld [hl], a
  ld a, l
  add a, $03
  ld l, a
  ld a, $65
  ld [hl], a
  ld a, l
  add a, $1D
  ld l, a
  dec b
  jr nz, :-
  ; final row
  ld a, $73
  ld [hli], a
  ld a, $74
  ld [hli], a
  ld a, $74
  ld [hli], a
  ld a, $75
  ld [hl], a
  ret

; ----------------------------
; b - suit
; c - count of cards in suit
InitSuit:
  ld a, b ; suit
  or a, c
  inc a
  ld [hli], a
  dec c
  jr nz, InitSuit
  ret
; ----------------------------
IncreaseHealth:
  ld a, [wHealth]
  add a, b
  cp a, $15 ; greater than 20?
  jr c, :+
  ld a, $14
:
  ld [wHealth], a
  ret
; ----------------------------
DecreaseHealth:
  ld a, [wHealth]
  sub a, b
  jr nc, :+
  ld a, 0
:
  ld [wHealth], a
  ret

; ----------------------------

; power efficient
PauseForVBlank::
	halt
  ldh a, [hVBlankFlag]
  cp a, 1
  jr nz, PauseForVBlank
  xor a, a
  ldh [hVBlankFlag], a
  ret

; ----------------------------

; only works if 
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
  ; Take a value and split it into 
  ; in: a: value <100
  ; out: a: units; b: tens
BCDSplit::
  ld b, -1
.loop
  inc b
  sub 10
  jr nc, .loop
  add a, 10
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


SECTION "Game Data", WRAMX, ALIGN[8]
; These are used for tracking input
wCurKeys: DB
wNewKeys: DB

wHealth: DB
wFlag: DB ; zero if we can run from the room, one if we can't
wCards: DS 6 ; 4 cards in a room

wDeckSize: DB
wDeck: DS 46

wDeckTop: DW
wDeckBottom: DW

SECTION "Constants", ROM0

def START_TEXT_LEN equ 10
StartText: DB "Push Start"

def HEALTH_FIRST_HEART equ $9C0C
def HEALTH_TENS equ $9C0E
def HEALTH_ONES equ $9C0F

def WEAPON_SUIT equ $9927
def WEAPON_TENS equ $9967
def WEAPON_ONES equ $9968

def CARD_WEAPON equ $9906
def CARD_MONSTER equ $990A
def CARD_ROOM_0 equ $9842
def CARD_ROOM_1 equ $9846
def CARD_ROOM_2 equ $984A
def CARD_ROOM_3 equ $984E

def SUIT_OFFSET equ $21
def TENS_OFFSET equ $61
def ONES_OFFSET equ $62

CARD_LUT:
  dw CARD_ROOM_0
  dw CARD_ROOM_1
  dw CARD_ROOM_2
  dw CARD_ROOM_3
  dw CARD_WEAPON
  dw CARD_MONSTER


SECTION "Tile data", ROM0

Tiles: INCBIN "tiles.bin"
TilesEnd:

SECTION "Window Tilemap", ROM0
WindowTilemap:
	db  $00, $00, $00, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4C, $00, $00, $00, $0F, $03, $01
WindowTilemapEnd:
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
hVBlankFlag:
	db
hFrameCounter:
	db
hGameState:
	db
