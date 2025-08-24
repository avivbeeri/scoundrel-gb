INCLUDE "hardware.inc"
INCLUDE "charmap.inc"
MACRO ENUM
  RSRESET
ENDM
MACRO ENUM_END
  RSRESET
ENDM

MACRO ENUM_MEMBER
  def \1 rb
ENDM

ENUM VRAM_QUEUE
ENUM_MEMBER VRAM_QUEUE_COMPLETE
ENUM_MEMBER VRAM_QUEUE_PENDING
ENUM_MEMBER VRAM_QUEUE_PAUSED
ENUM_END
; Setting up a game state enum
ENUM STATE
ENUM_MEMBER STATE_INIT 
ENUM_MEMBER STATE_ROOM
ENUM_MEMBER STATE_SELECT
ENUM_MEMBER STATE_SELECT_ATTACK
ENUM_MEMBER STATE_END_TURN
ENUM_MEMBER STATE_END_WIN
ENUM_MEMBER STATE_END_LOSE
ENUM_MEMBER STATE_COUNT
ENUM_END

SECTION "rst28", ROM0[$0028]
JumpTable::
	push de
	ld e, a
	ld d, 0
	add hl, de
	add hl, de
	ld a, [hli]
	ld h, [hl]
; SECTION "rst30", ROM0[$0030]
	ld l, a
	pop de
	jp hl

SECTION "Joypad Interrupt", ROM0[INT_HANDLER_JOYPAD]
JoypadInterrupt:
  push af
  ldh A, [rLY]
  ldh [hLY], A
  pop af
  reti
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

	ld a, %11100100
	ldh [rBGP], a

  ld A, STAT_LYC
  ldh [rSTAT], A

	ldh a, [hFrameCounter]
	inc a
	ldh [hFrameCounter], a
  ld a, 1
	ldh [hVBlankFlag], a

  ; update OAM
  ld a, HIGH(wShadowOAM)
  call hOAMDMA
  call ReadVRAMUpdate

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
	jp StatHandler

SECTION "STAT Handler", ROM0
StatHandler:
  ldh A, [rSTAT]
  and A, STAT_LYCF
  jr z, :+
  call WaitForHBlank
	ld a, %00011011
	ld [rBGP], a
  jr .handlerComplete
:
  ldh A, [hUpdateVRAMFlag]
  cp A, $2
  jr nz, .handlerComplete
  push BC
  push DE
  push HL
  call ReadVRAMUpdate
  pop HL
  pop DE
  pop BC
.handlerComplete
	pop af
	reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header

EntryPoint:
  di
  ld SP, $E000 ; reposition the stack pointer to the end of WRAM

  call CopyDMARoutine

	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

  ; Enable the VBLANK interrupt
  ld a, IE_VBLANK | IE_JOYPAD
	ldh [rIE], a

  ; Clear rIF for safety
  xor a, a ; This is equivalent to `ld a, 0`!
	ldh [rIF], a


	; Do not turn the LCD off outside of VBlank
  call WaitForVBlank

	; Turn the LCD off
	ld a, 0
	ldh [rLCDC], a

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

  ld d, 0
  ld bc, 160 ; 160 bytes, 4 * 40
  ld hl, wShadowOAM
  call Memset

  ; Set up the VRAM update queue
  ld d, 0
  ld bc, (256 * 4)
  ld hl, wQueue
  call Memset

  xor a
  ldh [hUpdateVRAMFlag], a
  ld a, LOW(wQueue)
  ld [wQueuePtr], a
  ld a, HIGH(wQueue)
  ld [wQueuePtr+1], a

	; Turn the LCD on
  ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
  ; Note that some of these constants (LCDCF_OBJOFF, LCDCF_WINOFF) are zero, but are included for clarity
  ld A, 134 - WX_OFS
  ldh [rLYC], A ; this is a line prior so we can pull some HBlank trickery
  inc A
  ld A, 144
  ldh [rWY], A

  ld A, $07
  ldh [rWX], A

  ld a, LCDC_ON | LCDC_BLOCK01 | LCDC_BG_ON | LCDC_OBJ_OFF | LCDC_WIN_OFF
  ldh [rLCDC], a      ; Enable and configure the LCD to show the background


	; During the first (blank) frame, initialize background palette
	ld a, %11100100
	ld [rBGP], a

  ; Initialize data to 0
  ld a, 0
	ldh [hScene], a
	ldh [hFrameCounter], a
  ld [wCurKeys], a
  ld [wNewKeys], a
  ld a, 1
  ldh [hRNGState], a

  ei

GameLoop:
  call PauseForVBlank
  ; We didn't quite finish the VRAM update, skip a frame
  ldh A, [hUpdateVRAMFlag]
  and A
  jr z, :+
  jr GameLoop
  :
  call ResetVRAMQueue

  call UpdateKeys

  ; Check which scene we're in
  ldh a, [hScene]
  cp a, 1 ; 1 = Game
  jp nz, .skipGame
  call UpdateGameScene
  jp .endGameLoop
  ; Do game loop processing
.skipGame
  call UpdateTitleScene
  ; Default to the title screen

.endGameLoop
  call ConcludeVRAMQueue
	jp GameLoop

; ----------------------------
InitGameScene:

	; Turn the LCD off
  call WaitForVBlank
  di


  ld a, 0
  ld [wRunFlag], a
  ld [wCursor], a
  ld [wMenuCursor], a
  ld [wCardFlags], a
  ld [wActions], a
  ld [wHealFlag], a
  ldh [hGameState], a

   ; Display set up
  ld A, STAT_LYC
  ldh [rSTAT], A

  ; Enable the VBLANK interrupt
  ld a, IE_VBLANK | IE_STAT
	ldh [rIE], a

	ld a, 0
	ldh [rLCDC], a ; LCD Off

  ; Setup the game board
	ld de, GameTilemap
	ld bc, GameTilemapEnd - GameTilemap
  ld hl, $9800
  call MemCopy

	ld de, HealthTilemap
	ld hl, $9A00
	ld bc, HealthTilemapEnd - HealthTilemap
  call MemCopy

	ld de, AttackMenuTilemap
	ld hl, $9C00
	ld bc, AttackMenuTilemapEnd - AttackMenuTilemap
  call MemCopy

  ; we're ready now
  ; enable the background
  ; enable the window
  ; turn on the display
  ld a, LCDC_ON | LCDC_BLOCK01 | LCDC_BG_ON | LCDC_OBJ_ON | LCDC_WIN_ON | LCDC_WIN_9C00
  ldh [rLCDC], a      

	ld a, %11100100
	ldh [rOBP0], a
	ldh [rOBP1], a
  ei
ret

; ----------------------------

; ----------------------------
UpdateGameScene:
  ldh A, [hGameState]
	ld HL, StateJumpTable
	rst JumpTable

  call DrawCursorSprites
  call UpdateCardGraphics

  ret

GameInit:
   ;; init health
  ld a, $14
  ld [wHealth], a

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

  ld a, 44
  ld [wDeckSize], a

  ld hl, wDeckTop
  ld a, 0
  ld [hl], a

 ; pointer for the bottom of the deck, should point at the next open slot in circular buffer
  ld hl, wDeckBottom
  ld a, [wDeckSize]
  ld [hl], a

  call InitRNGState
  call ShuffleDeck

  ld A, STATE_ROOM
  ldh [hGameState], A

  ld A, [wCardFlags]
  or A, $C0 ; Only update the deck count and health
  ld [wCardFlags], A
  ret

GameDrawRoom:
  ; Draw cards to fill the room
  xor A 
  ld C, 4
  ; loop through four slots
  ; if a slot is empty, check if there's a card in the deck to draw
  ; if so, draw and insert.

  ld DE, wCards
.loopStart
  ld A, [DE] ; card = cards[i]

  swap a
  and a, $0F ; check the suit to see if a card is present
  jr nz, .loopDec
   
  call DrawCard
  ; put card into A
  ld [DE], A ; card[i] = drawCard(deck); i++
.loopDec
  inc DE
  dec C
  jr nz, .loopStart
.loopExit
  ld A, [wCardFlags]
  or A, $8F ; Only update the deck count and health
  ld [wCardFlags], A

  ld A, STATE_SELECT
  ldh [hGameState], A
  ret

; ---------------------------------
; A = card
BuryCard:
  ld D, A
  ld A, [wDeckBottom] 
  ld E, A
  ld A, D
  ld D, HIGH(wDeck)
  ld [DE], A  ; deck[bottom] = A

  ; check deck bounds
  ld A, E
  inc A
  cp A, 48 ; 48 is max allocated size for deck buffer I guess
  jr c, .finishMove
  ld A, 0
  .finishMove
  ld [wDeckBottom], A

  ld HL, wDeckSize
  inc [HL]

  ret

; ---------------------------------
DrawCard:
  push DE
  ld D, 0
  ; Read the 
  ld A, [wDeckBottom] 
  ld E, A ; E = bottom
  ld A, [wDeckTop] 
  cp A, E ; top == bottom?
  jr z, .emptyDeck
  ld H, HIGH(wDeck)
  ld L, A
  ld B, [HL] ; A = deck[top]
  ld [HL], D
  inc A
  cp A, 48 ; 48 is max allocated size for deck buffer I guess
  jr c, .finishMove
  ld A, 0
  .finishMove
  ld [wDeckTop], A
  ld A, B
  ld HL, wDeckSize
  dec [HL]
  jr .finishDraw
  .emptyDeck
  xor A ; No value here if we're empty deck
  .finishDraw
  pop DE

  ret
; ---------------------------------
GameEnd:
;  ld A, 0
;  ldh [hScene], A
  ret

GameSelectAttack:
  ld a, [wNewKeys]
  and a, PAD_UP | PAD_DOWN
  jr z, :+
  call MoveMenuCursor

:
  ld a, [wNewKeys]
  and a, PAD_A
  jr z, :+
  call CompleteAttackAction
:
  ld a, [wNewKeys]
  and a, PAD_B
  jr z, :+
  ld A, STATE_SELECT
  ldh [hGameState], A
  ld A, 144
  ldh [rWY], A
:
  ret

CompleteAttackAction:
  ld A, [hCardValue]
  ld B, A
  call DecreaseHealth
  call DiscardSelection

  ld A, [wCardFlags]
  or a, $40
  ld [wCardFlags], a

  ld A, [wActions]
  inc A
  ld [wActions], A

  ld A, STATE_SELECT
  ldh [hGameState], A

  ld A, 144
  ldh [rWY], A
  ret

MoveMenuCursor:
  ret

GameEndTurn:
  xor A
  ld [wActions], A
  ld [wHealFlag], A
  ld [wRunFlag], A
  
  ld A, STATE_ROOM
  ldh [hGameState], A
  ret

GameSelectMove:
  ld a, [wNewKeys]
  and a, PAD_UP | PAD_DOWN
  jr z, :+
  call MoveCursorRow
:

  ld a, [wNewKeys]
  and a, PAD_LEFT
  jr z, :+
  call MoveCursorLeft
:

  ld a, [wNewKeys]
  and a, PAD_RIGHT
  jr z, :+
  call MoveCursorRight
:
  ld a, [wNewKeys]
  and a, PAD_A
  jr z, :+
  call PerformGameAction
:

  ld a, [wActions]
  cp a, 3
  jr nz, .skip
  ld A, STATE_END_TURN
  ldh [hGameState], A
.skip
  ; tail call
  jp CheckEndConditions

; ---------------------------------
CheckEndConditions:
  ; Check end condition
  ld A, [wHealth]
  and A
  ld A, STATE_END_LOSE
  jr z, .gameEnded

  ld A, [wDeckSize]
  and A
  jr nz, .complete

; iterate over the room?
  ld C, 4
  ld HL, wCards
.loopStart
  ld A, [HLI] ; card = cards[i]
  swap a
  and a, $0F ; check the suit to see if a card is present
  jr nz, .complete
  dec C
  jr nz, .loopStart

  ld A, STATE_END_WIN
  jr z, .gameEnded

  jr .complete
.gameEnded
  ldh [hGameState], A
.complete
  ret

; ---------------------------------

PerformGameAction:
  ld A, [wCursor]
  cp A, 4
  jr nz, .notDeck

  call PerformRedraw
  jr .complete
.notDeck
  ld DE, wCards
  add A, E
  ld E, A
  ld A, [DE] ; card = cards[i]
  ; now do something with card

  ld B, A
  swap a
  and a, $0F ; check the suit to see if a card is present
  jr z, .complete

  ldh [hCardSuit], A ; save the suit
  ld A, B 
  and A, $0F ; split value
  ldh [hCardValue], A ; save the value

  ldh A, [hCardSuit]
	ld HL, PerformActionTable
	rst JumpTable

  and A
  jr nz, .complete ; if not zero, we didn't complete an action
  call DiscardSelection

.actionComplete
  ld A, [wActions]
  inc A
  ld [wActions], A
.complete
  ret
; ---------------------------------

PerformNone:
  ret

PerformHeal:
  ld A, [wHealFlag]
  or A ; fast compare to 0
  jr nz, .complete ; skip if we healed on this turn already
  ld A, [hCardValue]
  ld B, A
  call IncreaseHealth

  ld A, [wCardFlags]
  or a, $40
  ld [wCardFlags], a

  ld A, 1
  ld [wHealFlag], A
.complete
  xor A
  ret

PerformWeaponPickup:
  ld A, [hCardValue]
  ld B, A
  ld A, [hCardSuit]
  swap A
  or B
  ld [wCards + 4], A

  ; clear weapon power stack
  xor A
  ld [wCards + 5], A

  ld A, [wCardFlags]
  or a, $30
  ld [wCardFlags], a

  xor A
  ret

PerformAttack:
  ; if a weapon is equipped
  ;   if no monster killed yet, open menu
  ;   check last killed with value
  ;   if so, open the menu, presenting punch and strike options
  ; else punch it.
  ld A, [hCardValue]
  ld B, A ; b = target

  ld A, [wCards + 4] ; get value of weapon?
  ld C, A ; C = weapon
  swap a
  and a, $0F ; check the suit to see if a weapon is present
  jr z, .punch ; no weapon equipped, jump to punch

  ld A, [wCards + 5] ; check last monster killed
  ld D, A ; D = last monster
  swap a
  and a, $0F ;
  jr z, .openMenu ; no monster killed yet, open menu to choose

  ; compare the current weapon strength with the target
  ld A, D ; A = last monster [total]
  and A, $0F ; get value of monster
  ld D, A ; D = weapon strength
  inc B
  cp A, B ; weapon - monster?
  jr nc, .openMenu
  jr .punch
.openMenu
  call DuplicateHealthBar

  ld A, 112 
  ldh [rWY], A

  ld A, STATE_SELECT_ATTACK
  ldh [hGameState], A
  jr .complete
.punch
  call DecreaseHealth

  ld A, [wCardFlags]
  or a, $40
  ld [wCardFlags], a

  xor A
.complete
  ret

PerformSpecial:
  ret

PerformRedraw:
  ld A, [wRunFlag]
  and A ; if not zero, we've run once this turn already
  jr nz, .skip

  ld A, [wActions]
  and A ; if not zero, we've already taken an action this turn
  ; no running
  jr nz, .skip

  
  ; cycle through room, putting cards on the deck
  ld C, 4
  ld HL, wCards
.loopStart
  ld A, [HL] ; card = cards[i]
  ld B, A
  and a, $F0 ; check the suit to see if a card is present
  jr z, .loopNext
  ; there's a card here, add it to the deck

  push HL
  ld A, B
  call BuryCard
  pop HL
  xor a
  ld [HL], A ; card = 0
.loopNext
  inc HL
  dec C
  jr nz, .loopStart

; Set to redraw the room
  ld A, STATE_ROOM
  ldh [hGameState], A

  ld a, [wCardFlags]
  or A, $0F
  ld [wCardFlags], A

  ld A, 1
  ld [wRunFlag], A
.skip
  ret
; ---------------------------------

DiscardSelection:
  ld DE, wCards
  ld A, [wCursor]
  add A, E
  ld E, A

  xor A
  ld [DE], A ; set the card to zero

  ld A, [wCursor]
  ld B, A
  inc B
  ld A, 1
:
  dec B
  jr z, :+
  sla A
  jr :-
:
; A = 1 << wCursor

  ld C, A
  ld A, [wCardFlags]
  or A, C
  ld [wCardFlags], A
  ret

; ---------------------------------

UpdateCardGraphics:
  ld a, [wCardFlags]
  ld b, a
  ld c, $FF
.flagLoop
  ld a, b
  cp a, 0
  jr z, .complete

  inc c
  srl b
  jr nc, .flagLoop

  push BC
  ld a, c
  cp a, 6 ; 0 - health 0 - deck 00 - bottom row 0000 - top row
  jr nc, :+
  call RenderCard
  jr .loopEnd
:
  cp a, 6
  jr z, :+
  call DrawDeckTotal
  jr .loopEnd
:
  call DrawHealthBar
.loopEnd
  pop BC
  jr .flagLoop
.complete
  xor a
  ld [wCardFlags], a
  ret

; ----------------------------

UpdateTitleScene:
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
  and a, PAD_A | PAD_B | PAD_START
  jr z, .complete
.beginGame
  ; Change game scene to GAME (1)
  ld a, 1
	ldh [hScene], a

  call InitGameScene
.complete:

  ret

DrawCursorSprites:
  ld a, [wCursor] 
  cp a, 4
  jr nz, .notDeck

  ld b, $51
  ld c, $09
  jr .setOAM

.notDeck
  ld a, [wCursor] 
  ld c, a

; compute y by row
  ld b, $21
  ld a, $19
; start figuring out x
  inc c
:
  dec c
  jr z, :+
  add $20
  jr :-
:
  ; add a, $18 ; or 18 or ;
  ld c, a
.setOAM
  ld hl, wShadowOAM
  ;ld b, $50 ; y
  ;ld c, $08 ; x
  ; top left
  ld a, b
  ld [hli], a
  ld a, c
  ld [hli], a
  ld a, $2F ; tile
  ld [hli], a
  ld a, $00 ; non rotated flags
  ld [hli], a

; top right
  ld a, b
  ld [hli], a
  ld a, c
  add $18
  ld [hli], a
  ld a, $2F ; tile
  ld [hli], a
  ld a, $20 ; non rotated flags
  ld [hli], a
; bottom left
  ld a, b
  add $20
  ld [hli], a
  ld a, c
  ld [hli], a
  ld a, $2F ; tile
  ld [hli], a
  ld a, $40 ; non rotated flags
  ld [hli], a
  ; bottom right

  ld a, b
  add $20
  ld [hli], a
  ld a, c
  add $18
  ld [hli], a
  ld a, $2F ; tile
  ld [hli], a
  ld a, $60 ; non rotated flags
  ld [hli], a
  ret

; ---------------------------
RenderCard:
  ; PRE: Put card index into A
  ldh [hCardIndex], A
  add A, LOW(wCards)
  ld L, A
  adc HIGH(wCards)
  sub L
  ld H, A
  ld A, [hl]
  ldh [hCardValue], A ; save the card data, [Suit-Value]
.getVRAMPosition
  ldh A, [hCardIndex]
  add A, A ; multiply by 2
  add A, LOW(CARD_LUT)
  ld L, A
  adc HIGH(CARD_LUT)
  sub L
  ld H, A
  ; HL now contains the array address for card vram

  ; Dereference [HL] -> DE
  ld A, [HLI]
  ld D, [HL]
  ld E, A

.getCardValues
  ldh A, [hCardValue]
  ld B, A
  swap a
  and a, $0F ; check the suit to see if a card is present
  jr z, .skipCard

  ldh [hCardSuit], A ; save the suit
  ld A, B 
  and A, $0F ; get value to split into Tens-Ones
  call BCDSplit ; A is ones, B is 10s
  inc A
  ldh [hCardOnes], A
  ld A, B
  inc A
  ldh [hCardTens], A
.renderCardBorder
  ; we need to use the address in DE twice more, so we preserve it twice on the stack
  push DE
  push DE
  call RenderCardBorder
  pop DE
.renderCardSuit
  ld A, SUIT_OFFSET
  call AddByteToDE
  ldh A, [hCardSuit]
  add A, $4B
  ld B, A
  call PushVRAMUpdate
.renderCardValue
  pop DE
  ld A, TENS_OFFSET
  call AddByteToDE
  ldh A, [hCardTens] ; TODO: check if card is actually present and clear it?
  ld B, A
  call PushVRAMUpdate
  inc DE
  ldh A, [hCardOnes]
  ld B, A
  call PushVRAMUpdate
  jr .complete
.skipCard
  call ClearCardBorder
.complete
  ret
; ----------------
; ---------------------------
DrawDeckTotal:
  ld C, 0
  ld DE, DECK_TENS

  ld A, [wDeckSize]
  call BCDSplit
  inc A ; increment to get tile index from number
  inc B
  ldh [hCardOnes], A ; preserve A for later
  call PushVRAMUpdate

  inc DE ; move to OnesPlace
  ldh A, [hCardOnes]
  ld B, A
  call PushVRAMUpdate

  ret

; ---------------------------
DuplicateHealthBar:
	ld de, $9A00
	ld hl, $9C40
	ld bc, HealthTilemapEnd - HealthTilemap
  jp MemCopy
; ---------------------------
DrawHealthBar:
; render current health from BCD
  ld a, [wHealth]
  call BCDSplit
  ld c, a

  ld de, HEALTH_TENS
  inc b
  call PushVRAMUpdate
  ld de, HEALTH_ONES
  ld b, c
  inc b
  call PushVRAMUpdate

  ; -- assumes we haven't got the health already
  ; -- 10 hearts - we'll try to do this implied
  ; -- health total

  ; de - vram location
  ld a, [wHealth]
  ld c, a; save a copy of health
  ld de, HEALTH_FIRST_HEART
.printHeartLoopStart:
  xor a ; is a zero here?
  ld a, c
  cp a, $2
  jr c, :+
  dec c
  dec c
  ld b, $4C
  jr .printHeartLoopEnd
:
  xor a ; is a zero here?
  ld a, c
  cp a, $1
  jr c, :+
  dec c
  ld b, $4B
  jr .printHeartLoopEnd
:
  ld b, $4A
.printHeartLoopEnd
  call PushVRAMUpdate
  dec e
  ld a, e
  cp a, (LOW(HEALTH_FIRST_HEART) - 10) ; We have a known fixed number to draw so we can just compare it here
  jr nz, .printHeartLoopStart
.printHeartLoopFinish
  ret

; ---------------------------
; DE - VRAM pointer to card
; clobbers HL, DE, BC
ClearCardBorder:
  push BC
  push DE
  push HL
  ld L, 5
  ld B, 0
  xor C
:
  push HL ; updates clobber L, which we need to maintain
  call GetVRAMQueueSlot
  call PushVRAMQueue
  inc DE
  call PushVRAMQueue
  inc DE
  call PushVRAMQueue
  inc DE
  call PushVRAMQueue
  inc DE
  call SaveVRAMQueueSlot
  ld A, E ; this might not be good enough
  add A, $1C
  ld E, A
  pop HL
  dec L
  jr nz, :-
.clearCardEnd
  pop HL
  pop DE
  pop BC
  ret

; ---------------------------
RenderCardBorder:
  push BC
  push HL

  xor C

; row 1
  ld B, $53
  call GetVRAMQueueSlot
  call PushVRAMQueue
  inc DE
  ld B, $54
  call PushVRAMQueue
  inc DE
  call PushVRAMQueue
  inc DE
  ld B, $55
  call PushVRAMQueue

  ld A, E
  add A, $1D
  ld E, A

  ld B, $63
REPT 3 ; 3 middle rows for border
  call PushVRAMQueue
  inc E
  inc E
  inc E
  call PushVRAMQueue
  ; carriage return
  ld A, E
  add A, $1D
  ld E, A
ENDR
  ;final row
  ld B, $73
  call PushVRAMQueue
  inc DE
  ld B, $74
  call PushVRAMQueue
  inc DE
  call PushVRAMQueue
  inc DE
  ld B, $75
  call PushVRAMQueue
  call SaveVRAMQueueSlot

  pop HL
  pop BC
  ret

; ----------------------------
ShuffleDeck:
  ; Assumes wDeck is aligned on boundary
  ; so we can index by the low byte into the array
  ld HL, wDeck + 43
  ld DE, wDeck
  ;
  ;for (var i = deck.length - 1; i > 0; i--) {
  ;  var j = Math.floor(Math.random() * (i + 1));
  ;  var temp = deck[i];
  ;  deck[i] = deck[j];
  ;  deck[j] = temp;
  ;}
  ;
.shuffleLoop 
.jloop
  call GetRandomByte ;
  and A, 63
  cp A, L
  jr nc, .jloop
  ld E, A
  ; E is now j
  ld A, [HL] 
  ld B, A ; temp = deck[i]

  ld A, [DE]
  ld [HL], A ; deck[i] = deck[j]

  ld A, B
  ld [DE], A ; deck[j] = temp

  dec L
  jr nz, .shuffleLoop
.exit
  ret

; ----------------------------
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
MoveCursorRow:
  ld a, [wCursor]
  cp a, 4
  jr nz, :+
  xor a
  jr .complete
:
  ld a, 4
.complete
  ld [wCursor], a
  ret

; ----------------------------
MoveCursorLeft:
  ld a, [wCursor]
  sub a, 1
  jr nc, .complete
  ld a, 4
.complete
  ld [wCursor], a
  ret
; ----------------------------
MoveCursorRight:
  ld a, [wCursor]
  inc a
  cp a, 5
  jr c, .complete
  xor a
.complete
  ld [wCursor], a
  ret
; ----------------------------
IncreaseHealth:
  ld A, [hCardValue]
  ld B, A
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
  xor A
  ldh [hVBlankFlag], A
  ret

; ----------------------------

WaitForHBlank::
.wait
  ldh A, [rSTAT]
  and A, STAT_BUSY
  jr nz, .wait
  ret

; only works if 
WaitForVBlank::
	ldh a, [rLY]
	cp 144
	jr c, WaitForVBlank
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
; Add A to HL, unsigned addition
; https://www.plutiedev.com/z80-add-8bit-to-16bit
AddByteToHL::
  add   a, l    ; A = A+L
  ld    l, a    ; L = A+L
  adc   a, h    ; A = A+L+H+carry
  sub   l       ; A = H+carry
  ld    h, a
  ret

; Add A to DE, unsigned addition
; https://www.plutiedev.com/z80-add-8bit-to-16bit
AddByteToDE::
  add   a, e    ; A = A+E
  ld    e, a    ; E <= (A+E)
  adc   a, d    ; A = (A+E)+D+carry
  sub   e       ; A = D + carry
  ld    d, a    ; D <= A
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

ReadVRAMUpdate::
  ldh A, [hUpdateVRAMFlag]
  or A
  jr z, .skip

  ld HL, SP+0      

  cp A, $2
  jr nz, .fresh
.resume
  ; Copy stack pointer to BC from HL, so we can load the stack pointer
  ld C, L
  ld B, H

  ; load QueuePtr -> SP
  ld A, [wQueuePtr]
  ld L, A
  ld A, [wQueuePtr+1]
  ld H, A
  ld SP, HL
  ; restore the stack pointer to HL for later restore
  ld L, C
  ld H, B

  jr .loop
.fresh
; Using stack operations to read the queue is faster, so we save the stack pointer in HL
  ld SP, wQueue
.loop
  ldh A, [$FF41]     ; STAT Register
  and A, STAT_BUSY
  jr nz, .pauseUpdate

  pop DE ; pop the VRAM address
  pop AF ; Pop the tile value and flags ($80 will eventually be end of list)
  ld [DE], A
  jr nz, .loop
  ; fallthrough
  jr .complete

.pauseUpdate
  ; vblank ended or hblank ending
  ; we should pause so we can resume
  LD [wQueuePtr], SP
  ld SP, HL ; restore the stack pointer for safety

  ; Set the hUpdateVRAMFlag flag to $2, for "in progress"
  ld A, VRAM_QUEUE_PAUSED ; in progress
  ldh [hUpdateVRAMFlag], A ; set flag
  ld A, STAT_LYC | STAT_MODE_0
  ldh [rSTAT], A
  jr .skip
.complete
  ld A, VRAM_QUEUE_COMPLETE ; complete
  ldh [hUpdateVRAMFlag], A ; set flag
  ld SP, HL ; restore the stack pointer for safety
.skip
  ret

;-------------------------------
; HL: queue slot
; DE: VRAM addr
; B: tile-value to set
; C : $00 for regular, $80 for final item
GetVRAMQueueSlot:
  ld A, [wQueuePtr]
  ld L, A
  ld A, [wQueuePtr+1]
  ld H, A
  ret

SaveVRAMQueueSlot:
  ld A, L
  ld [wQueuePtr], A
  ld A, H
  ld [wQueuePtr+1], A
  ; ld      A, VRAM_QUEUE_PENDING
  ; ldh     [hUpdateVRAMFlag], A
  ret

ResetVRAMQueue:
  ld A, LOW(wQueue)
  ld [wQueuePtr], A
  ld A, HIGH(wQueue)
  ld [wQueuePtr+1], A
  ret
  
ConcludeVRAMQueue:
  ld A, [wQueuePtr]
  or A
  jr z, :+
  ld L, A
  ld A, [wQueuePtr+1]
  ld H, A
  dec HL
  dec HL
  ld A, $80
  ld [HL], A

  ld      A, VRAM_QUEUE_PENDING
  ldh     [hUpdateVRAMFlag], A
  
:
  ret

; Returns HL- new queue slot
PushVRAMQueue:
	ld	A,	E		    ; VRAM lo-byte
	ld	[HL+],	A		; write to queue and advance
	ld	A,	D		    ; VRAM hi-byte
	ld	[HL+],	A		; write to queue and advance
	ld	A,	C		    ; CPU flags (last-tile flag)
	ld	[HL+],	A		; write to queue and advance
	ld	A,	B		    ; tile-value
	ld	[HL+],	A		; write and advance

	; since V-blank could interrupt whilst the queue is being added to,
	; DON'T set the queue-ready flag until AFTER the tile is added
	;
	;ld	A,	C		; get last-tile flag again
	;ldh	[hUpdateVRAMFlag], A	; inform V-blank of queue if last tile
	ret

;-------------------------------
;
; de - addr in VRAM
; b - value to write
; clobbers HL
PushVRAMUpdate::
  call GetVRAMQueueSlot
  xor C
  call PushVRAMQueue
  call SaveVRAMQueueSlot
  ret
; ----------------------------

; ----------------------------
UpdateKeys::


  ld a, JOYP_GET_CTRL_PAD
  call .onenibble
  swap a ; A7-4 = unpressed directions; A3-0 = 1
  ld b, a ; B7-4 = unpressed buttons; B3-0 = 1

  ld a, JOYP_GET_BUTTONS
  call .onenibble
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret

.onenibble
  ldh [rJOYP], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rJOYP] ; ignore value while waiting for the key matrix to settle
  ldh a, [rJOYP]
  ldh a, [rJOYP] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret

; ----------------------------

SECTION "OAM DMA routine", ROM0
CopyDMARoutine:
  ld  hl, DMARoutine
  ld  b, DMARoutineEnd - DMARoutine ; Number of bytes to copy
  ld  c, LOW(hOAMDMA) ; Low byte of the destination address
.copy
  ld  a, [hli]
  ldh [c], a
  inc c
  dec b
  jr  nz, .copy
  ret

SECTION "OAM DMA Code", ROM0
DMARoutine::
LOAD "OAM DMA", HRAM
hOAMDMA::
  ldh [rDMA], a
  ld  a, 40
.wait
  dec a
  jr  nz, .wait
  ret
ENDL
DMARoutineEnd:


SECTION "Game Data", WRAM0, ALIGN[8]
; These are used for tracking input
wCurKeys: DB
wNewKeys: DB

wCardFlags: DB
wCursor: DB
wMenuCursor: DB
wHealth: DB
wActions: DB ; zero if we can run from the room, one if we can't
wRunFlag: DB ; zero if we can run from the room, one if we can't
wHealFlag: DB ; zero if we can run from the room, one if we can't
wCards: DS 6 ; 4 cards in a room

wDeckTop: DB
wDeckBottom: DB
wDeckSize: DB

SECTION "Deck",WRAM0, ALIGN[8]
wDeck: DS 44


SECTION "VRAM Update Queue",WRAM0, ALIGN[8]
wQueue: DS (256 * 4)
wQueuePtr: DW

SECTION "Shadow OAM", WRAM0, ALIGN[8]
; Cursor sprites for Deck
; 50 08 2F 00
; 50 22 2F 20
; 70 08 2F 40
; 70 22 2F 60
wShadowOAM::
  ds 160

SECTION "Fast vars", HRAM
hFrameCounter: db
hVBlankFlag: db
hLY: db ; used for setting random seed
hScene: db
hGameState:	db
hUpdateVRAMFlag: db

; Temp vars
hCardIndex: db
hCardSuit: db
hCardValue: db
hCardOnes: db
hCardTens: db
hAttackValue: db

SECTION "Constants", ROM0

def START_TEXT_LEN equ 10
StartText: DB "Push Start"
FightText: DB "Weapon   Punch"

def HEALTH_FIRST_HEART equ $9A0C
def HEALTH_TENS equ $9A0E
def HEALTH_ONES equ $9A0F

def WEAPON_SUIT equ $9927
def WEAPON_TENS equ $9967
def WEAPON_ONES equ $9968

def DECK_TENS equ $99A1
def DECK_ONES equ $99A2

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

StateJumpTable:
  dw GameInit
  dw GameDrawRoom
  dw GameSelectMove
  dw GameSelectAttack
  dw GameEndTurn
  dw GameEnd
  dw GameEnd

PerformActionTable:
  dw PerformNone ; none
  dw PerformHeal ; heart
  dw PerformWeaponPickup ; diamond
  dw PerformAttack ; spade
  dw PerformAttack ; club
  dw PerformSpecial ; joker?


STATE_TABLE:

; TOP_ROW_CURSOR_OFFSET:
; db $20, $18


SECTION "Tile data", ROM0

Tiles: INCBIN "tiles.bin"
TilesEnd:


SECTION "AttackMenuTilemap", ROM0
AttackMenuTilemap:
db $00 , $00 , $00 , $00 , $1F , $24 , $1D , $12 , $17 , $00 , $00 , $00 , $26 , $14 , $10 , $1F
db $1E , $1D , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00
db $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00
db $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00 , $00
AttackMenuHealth: ; we need this to duplicate the healthbar
db $00 , $00 , $00 , $4C , $4C , $4C , $4C , $4C , $4C , $4C , $4C , $4C , $4C , $00 , $03 , $01 , $0F , $03 , $01 , $00
AttackMenuTilemapEnd:

SECTION "HealthBar Tilemap", ROM0
HealthTilemap:
	db  $00, $00, $00, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $4A, $00, $00, $00, $0F, $03, $01, $00
HealthTilemapEnd:
SECTION "Game Tilemap", ROM0
GameTilemap:
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $61, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $50, $51, $51, $52, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $56, $57, $62, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $66, $67, $62, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $60, $76, $77, $62, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db  $70, $71, $71, $72, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
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

INCLUDE "rng.inc"
