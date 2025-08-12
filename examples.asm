gameStatePointers::
  ; DW stores a WORD (16-bit value) with the given value, in this case, the address for the given routine/function
  dw updateTitlescreen
  dw updateGame
  dw updateMenu

; Call on each frame to call game state
runGameState::
  ld a, [wGameState] ; A value 0-255 to determine the current game state
  add a ; A + A = A * 2, since we are working with WORD values
  ; Add address of LUT
  add LOW(gameStatePointers)
  ld l, a
  adc HIGH(gameStatePointers)
  sub l
  ld h, a
  ; Now de-reference the pointed WORD into HL
  ld a, [hli]
  ld h, [hl]
  ld l, a
  ; HL = Function pointer for current state
  ; Jump execution to the state function
  jp hl
