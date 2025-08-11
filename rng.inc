
SECTION "RNG State", HRAM
; 8-bit  state
hRNGState: db

SECTION "RNG Routine", ROM0
InitRNGState::
  ldh A, [hRNGState]
  cp A, A
  jr nz, :+
  ld A, $AA ; fixed seed
:
  ldh [hRNGState], A
  ret

GetRandomByte::
  push BC
  ldh A, [hRNGState]
  ld B, 0
  ld C, 9 ; 8 times, we need one more
.loopStart
  dec C
  jr z, .exit
  sla B ; pre-shift B
  srl A ; shift the state, LSB to carry
  jr nc, :+ ; check carry
  xor A, $8E ; Tap points are 10001110
  set 0, B
  jr .loopStart
:  
  res 0, B
  jr .loopStart
.exit
  ldh [hRNGState], A
  ld A, B
  pop BC
  dec A
  ret

