 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif
; This is a 2-line kernel!
kernel
 sta WSYNC
 lda #255
 sta TIM64T

 lda #1
 sta VDELBL
 sta VDELP0
 ldx ballheight
 inx
 inx
 stx temp4
 lda player1y
 sta temp3

 ifconst shakescreen
   jsr doshakescreen
 else
   ldx missile0height
   inx
 endif

 inx
 stx stack1

 lda bally
 sta stack2

 lda player0y
 ldx #0
 sta WSYNC
 stx GRP0
 stx GRP1
 stx PF1
 stx PF2
 stx CXCLR
 ifconst readpaddle
   stx paddle
 else
   sleep 3
 endif

 sta temp2,x

 ;store these so they can be retrieved later
 ifnconst pfres
   ldx #128-44
 else
   ldx #132-pfres*4
 endif

 inc player1y

 lda missile0y
 sta temp5
 lda missile1y
 sta temp6

 lda playfieldpos
 sta temp1
 
 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 clc
 sbc playfieldpos
 sta playfieldpos
 jmp .startkernel

.skipDrawP0
 lda #0
 tay
 jmp .continueP0

.skipDrawP1
 lda #0
 tay
 jmp .continueP1

.kerloop ; enter at cycle 59??

continuekernel
 sleep 2
continuekernel2
 lda ballheight
 
 ifconst pfres
 ldy playfield+pfres*4-132,x
 sty PF1 ;3
 ldy playfield+pfres*4-131,x
 sty PF2 ;3
 ldy playfield+pfres*4-129,x
 sty PF1 ; 3 too early?
 ldy playfield+pfres*4-130,x
 sty PF2 ;3
 else
 ldy playfield+44-128,x ;4
 sty PF1 ;3
 ldy playfield+45-128,x ;4
 sty PF2 ;3
 ldy playfield+47-128,x ;4
 sty PF1 ; 3 too early?
 ldy playfield+46-128,x;4
 sty PF2 ;3
 endif

 dcp bally
 rol
 rol
; rol
; rol
goback
 sta ENABL 
.startkernel
 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawP1 ;2
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continueP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
   rol;2
   rol;2
   sta ENAM1 ;3
 else
   lda (player1color),y
   sta COLUP1
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif

 ifconst pfres
 lda playfield+pfres*4-132,x 
 sta PF1 ;3
 lda playfield+pfres*4-131,x 
 sta PF2 ;3
 lda playfield+pfres*4-129,x 
 sta PF1 ; 3 too early?
 lda playfield+pfres*4-130,x 
 sta PF2 ;3
 else
 lda playfield+44-128,x ;4
 sta PF1 ;3
 lda playfield+45-128,x ;4
 sta PF2 ;3
 lda playfield+47-128,x ;4
 sta PF1 ; 3 too early?
 lda playfield+46-128,x;4
 sta PF2 ;3
 endif 
; sleep 3

 lda player0height
 dcp player0y
 bcc .skipDrawP0
 ldy player0y
 lda (player0pointer),y
.continueP0
 sta GRP0

 ifnconst no_blank_lines
 ifnconst playercolors
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
 else
   lda (player0color),y
   sta player0colorstore
   sleep 6
 endif
   dec temp1
   bne continuekernel
 else
   dec temp1
   beq altkernel2
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle
   inc paddle
   jmp continuekernel2
noreadpaddle
   sleep 2
   jmp continuekernel
 else
 ifnconst playercolors 
 ifconst PFcolors
   txa
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
   jmp continuekernel
 else
   sleep 12
 endif
 else
   lda (player0color),y
   sta player0colorstore
   sleep 4
 endif
   jmp continuekernel
 endif
altkernel2
   txa
   sbx #252
   bmi lastkernelline
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
   jmp continuekernel
 endif

altkernel

 ifconst PFmaskvalue
   lda #PFmaskvalue
 else
   lda #0
 endif
 sta PF1
 sta PF2


 ;sleep 3

 ;28 cycles to fix things
 ;minus 11=17

; lax temp4
; clc
 txa
 sbx #252

 bmi lastkernelline

 ifconst PFcolorandheight
   ldy playfieldcolorandheight-87,x
 ifnconst backgroundchange
   sty COLUPF
 else
   sty COLUBK
 endif
   lda playfieldcolorandheight-88,x
   sta.w temp1
 endif
 ifconst PFheights
   lsr
   lsr
   tay
   lda (pfheighttable),y
   sta.w temp1
 endif
 ifconst PFcolors
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 ifnconst PFcolorandheight
 ifnconst PFcolors
 ifnconst PFheights
 ifnconst no_blank_lines
 ; read paddle 0
 ; lo-res paddle read
  ; bit INPT0
  ; bmi paddleskipread
  ; inc paddle0
;donepaddleskip
   sleep 10
 ifconst pfrowheight
   lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 endif
 endif
 endif
 

 lda ballheight
 dcp bally
 sbc temp4


 jmp goback


 ifnconst no_blank_lines
lastkernelline
 ifnconst PFcolors
   sleep 10
 else
   ldy #124
   lda (pfcolortable),y
   sta COLUPF
 endif

 ifconst PFheights
 ldx #1
 sleep 4
 else
 ldx playfieldpos
 sleep 3
 endif

 jmp enterlastkernel

 else
lastkernelline
 
 ifconst PFheights
 ldx #1
 sleep 5
 else
   ldx playfieldpos
 sleep 4
 endif

   cpx #1
   bne .enterfromNBL
   jmp no_blank_lines_bailout
 endif

 if ((<*)>$d5)
 align 256
 endif
 ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
 sleep 2
 lda #0
 jmp .continuelastP1

.endkerloop ; enter at cycle 59??
 
 nop

.enterfromNBL
 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

enterlastkernel
 lda ballheight

; tya
 dcp bally
; sleep 4

; sbc stack3
 rol
 rol
 sta ENABL 

 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawlastP1
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continuelastP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
 else
   lda (player1color),y
   sta COLUP1
 endif

 dex
 ;dec temp4 ; might try putting this above PF writes
 beq endkernel


 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

 ifnconst player1colors
   rol;2
   rol;2
   sta ENAM1 ;3
 else
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif
 
 lda.w player0height
 dcp player0y
 bcc .skipDrawlastP0
 ldy player0y
 lda (player0pointer),y
.continuelastP0
 sta GRP0



 ifnconst no_blank_lines
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
   jmp .endkerloop
 else
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle2
   inc paddle
   jmp .endkerloop
noreadpaddle2
   sleep 4
   jmp .endkerloop
 else ; no_blank_lines and no paddle reading
 sleep 14
 jmp .endkerloop
 endif
 endif


;  ifconst donepaddleskip
;paddleskipread
 ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
 ; plus we get a lo-res paddle read
; bmi donepaddleskip
;  endif

.skipDrawlastP0
 sleep 2
 lda #0
 jmp .continuelastP0

 ifconst no_blank_lines
no_blank_lines_bailout
 ldx #0
 endif

endkernel
 ; 6 digit score routine
 stx PF1
 stx PF2
 stx PF0
 clc

 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 sbc playfieldpos
 sta playfieldpos
 txa

 ifconst shakescreen
   bit shakescreen
   bmi noshakescreen2
   ldx #$3D
noshakescreen2
 endif

   sta WSYNC,x

;                STA WSYNC ;first one, need one more
 sta REFP0
 sta REFP1
                STA GRP0
                STA GRP1
 ;               STA PF1
   ;             STA PF2
 sta HMCLR
 sta ENAM0
 sta ENAM1
 sta ENABL

 lda temp2 ;restore variables that were obliterated by kernel
 sta player0y
 lda temp3
 sta player1y
 ifnconst player1colors
   lda temp6
   sta missile1y
 endif
 ifnconst playercolors
 ifnconst readpaddle
   lda temp5
   sta missile0y
 endif
 endif
 lda stack2
 sta bally

 ifconst no_blank_lines
 sta WSYNC
 endif

 lda INTIM
 clc
 ifnconst vblank_time
 adc #43+12+87
 else
 adc #vblank_time+12+87
 endif
; sta WSYNC
 sta TIM64T

 ifconst minikernel
 jsr minikernel
 endif

 ; now reassign temp vars for score pointers

; score pointers contain:
; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
; swap lo2->temp1
; swap lo4->temp3
; swap lo6->temp5
 ifnconst noscore
 lda scorepointers+1
; ldy temp1
 sta temp1
; sty scorepointers+1

 lda scorepointers+3
; ldy temp3
 sta temp3
; sty scorepointers+3


 sta HMCLR
 tsx
 stx stack1 
 ldx #$10
 stx HMP0

 sta WSYNC
 ldx #0
                STx GRP0
                STx GRP1 ; seems to be needed because of vdel

 lda scorepointers+5
; ldy temp5
 sta temp5,x
; sty scorepointers+5
 lda #>scoretable
 sta scorepointers+1
 sta scorepointers+3
 sta scorepointers+5,x
 sta temp2,x
 sta temp4,x
 sta temp6,x
                LDY #7
                STA RESP0
                STA RESP1


        LDA #$03
        STA NUSIZ0
        STA NUSIZ1,x
        STA VDELP0
        STA VDELP1
        LDA #$20
        STA HMP1
               LDA scorecolor 
;               STA HMCLR
;               STA WSYNC; second one
                STA HMOVE ; cycle 73 ?

                STA COLUP0
                STA COLUP1
 lda  (scorepointers),y
 sta  GRP0
 ifconst pfscore
 lda pfscorecolor
 sta COLUPF
 endif
 lda  (scorepointers+8),y
 sta WSYNC
 sleep 2
 jmp beginscore

 if ((<*)>$d4)
 align 256 ; kludge that potentially wastes space!  should be fixed!
 endif

loop2
 lda  (scorepointers),y     ;+5  68  204
 sta  GRP0            ;+3  71  213      D1     --      --     --
 ifconst pfscore
 lda.w pfscore1
 sta PF1
 else
 sleep 7
 endif
 ; cycle 0
 lda  (scorepointers+$8),y  ;+5   5   15
beginscore
 sta  GRP1            ;+3   8   24      D1     D1      D2     --
 lda  (scorepointers+$6),y  ;+5  13   39
 sta  GRP0            ;+3  16   48      D3     D1      D2     D2
 lax  (scorepointers+$2),y  ;+5  29   87
 txs
 lax  (scorepointers+$4),y  ;+5  36  108
 sleep 3

 ifconst pfscore
 lda pfscore2
 sta PF1
 else
 sleep 6
 endif

 lda  (scorepointers+$A),y  ;+5  21   63
 stx  GRP1            ;+3  44  132      D3     D3      D4     D2!
 tsx
 stx  GRP0            ;+3  47  141      D5     D3!     D4     D4
 sta  GRP1            ;+3  50  150      D5     D5      D6     D4!
 sty  GRP0            ;+3  53  159      D4*    D5!     D6     D6
 dey
 bpl  loop2           ;+2  60  180

 ldx stack1 
 txs
; lda scorepointers+1
 ldy temp1
; sta temp1
 sty scorepointers+1

                LDA #0   
 sta PF1
               STA GRP0
                STA GRP1
        STA VDELP0
        STA VDELP1;do we need these
        STA NUSIZ0
        STA NUSIZ1

; lda scorepointers+3
 ldy temp3
; sta temp3
 sty scorepointers+3

; lda scorepointers+5
 ldy temp5
; sta temp5
 sty scorepointers+5
 endif ;noscore
 LDA #%11000010
 sta WSYNC
 STA VBLANK
 RETURN

 ifconst shakescreen
doshakescreen
   bit shakescreen
   bmi noshakescreen
   sta WSYNC
noshakescreen
   ldx missile0height
   inx
   rts
 endif

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifconst pfrowheight
 lda pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*4-1
 else
 ldx #47
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 asl ; multiply y pos by 4
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 asl ; multiply by 4
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 iny
 iny
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
 ifconst debugscore
   ldx #14
   lda INTIM ; display # cycles left in the score

 ifconst mincycles
 lda mincycles 
 cmp INTIM
 lda mincycles
 bcc nochange
 lda INTIM
 sta mincycles
nochange
 endif

;   cmp #$2B
;   bcs no_cycles_left
   bmi cycles_left
   ldx #64
   eor #$ff ;make negative
cycles_left
   stx scorecolor
   and #$7f ; clear sign bit
   tax
   lda scorebcd,x
   sta score+2
   lda scorebcd1,x
   sta score+1
   jmp done_debugscore   
scorebcd
 .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
 .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
 .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
 .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
 .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
 .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
 .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
 .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
 endif

 ifconst debugcycles
   lda INTIM ; if we go over, it mucks up the background color
;   cmp #$2B
;   BCC overscan
   bmi overscan
   sta COLUBK
   bcs doneoverscan
 endif

 
overscan
 lda INTIM ;wait for sync
 bmi overscan
doneoverscan
;do VSYNC
 lda #2
 sta WSYNC
 sta VSYNC
 STA WSYNC
 STA WSYNC
 LDA #0
 STA WSYNC
 STA VSYNC
 sta VBLANK
 ifnconst overscan_time
 lda #37+128
 else
 lda #overscan_time+128
 endif
 sta TIM64T

 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop
 lda player0x,x
 sec
 sbc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop
 endif
 endif
 if (<*)>$F0
 align 256, $EA
 endif
  sta WSYNC
  ldx #4
  SLEEP 3
HorPosLoop       ;     5
  lda player0x,X  ;+4   9
  sec           ;+2  11
DivideLoop
  sbc #15
  bcs DivideLoop;+4  15
  sta temp1,X    ;+4  19
  sta RESP0,X   ;+4  23
  sta WSYNC
  dex
  bpl HorPosLoop;+5   5
                ;     4

  ldx #4
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 18

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 32

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 46

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 60

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 74

  sta WSYNC
 
  sta HMOVE     ;+3   3


 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop2
 lda player0x,x
 clc
 adc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop2
 endif
 endif




;set score pointers
 lax score+2
 jsr scorepointerset
 sty scorepointers+5
 stx scorepointers+2
 lax score+1
 jsr scorepointerset
 sty scorepointers+4
 stx scorepointers+1
 lax score
 jsr scorepointerset
 sty scorepointers+3
 stx scorepointers

vblk
; run possible vblank bB code
 ifconst vblank_bB_code
   jsr vblank_bB_code
 endif
vblk2
 LDA INTIM
 bmi vblk2
 jmp kernel
 

    .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
    .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
 and #$0F
 asl
 asl
 asl
 adc #<scoretable
 tay 
 txa
; and #$F0
; lsr
 asr #$F0
 adc #<scoretable
 tax
 rts
game
.L00 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L01 ;  COLUBK  =  $F4

	LDA #$F4
	STA COLUBK
.
 ; 

.L02 ;  dim nodown  =  a

.L03 ;  dim noup  =  b

.L04 ;  dim noleft  =  c

.L05 ;  dim noright  =  d

.L06 ;  dim room  =  e

.L07 ;  dim haslance  =  f

.L08 ;  dim hasshield  =  g

.L09 ;  dim hascoin  =  h

.L010 ;  dim randnumber  =  i

.L011 ;  dim coinvalue  =  j

.L012 ;  dim compass  =  k

.L013 ;  dim dead  =  l

.L014 ;  dim minotauro  =  m

.L015 ;  dim hitted  =  n

.
 ; 

.L016 ;  haslance  =  0

	LDA #0
	STA haslance
.L017 ;  hasshield  =  0

	LDA #0
	STA hasshield
.L018 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L019 ;  coinvalue  =  1

	LDA #1
	STA coinvalue
.L020 ;  dead  =  0

	LDA #0
	STA dead
.L021 ;  hitted  =  0

	LDA #0
	STA hitted
.
 ; 

.L022 ;  room  =  0

	LDA #0
	STA room
.L023 ;  nodown  =  0

	LDA #0
	STA nodown
.L024 ;  noup  =  0

	LDA #0
	STA noup
.L025 ;  noleft  =  0

	LDA #0
	STA noleft
.L026 ;  noright  =  0

	LDA #0
	STA noright
.
 ; 

.L027 ;  player0x  =  24

	LDA #24
	STA player0x
.L028 ;  player0y  =  76

	LDA #76
	STA player0y
.
 ; 

.L029 ;  missile0height  =  8

	LDA #8
	STA missile0height
.L030 ;  missile0x  =  83

	LDA #83
	STA missile0x
.L031 ;  missile0y  =  48

	LDA #48
	STA missile0y
.
 ; 

.L032 ;  ballheight  =  4

	LDA #4
	STA ballheight
.L033 ;  CTRLPF  =  $21

	LDA #$21
	STA CTRLPF
.L034 ;  ballx  =  0

	LDA #0
	STA ballx
.L035 ;  bally  =  0

	LDA #0
	STA bally
.
 ; 

.mainloop
 ; mainloop

.
 ; 

.L036 ;  if room  =  0 then gosub room1  :  gosub moverderecha  :  gosub minoheridados  :  COLUP0  =  $86

	LDA room
	CMP #0
     BNE .skipL036
.condpart0
 jsr .room1
 jsr .moverderecha
 jsr .minoheridados
	LDA #$86
	STA COLUP0
.skipL036
.
 ; 

.L037 ;  NUSIZ1  =  $10

	LDA #$10
	STA NUSIZ1
.
 ; 

.L038 ;  COLUP1  =  $4A

	LDA #$4A
	STA COLUP1
.L039 ;  if coinvalue  =  5 then COLUP1  =  $0A

	LDA coinvalue
	CMP #5
     BNE .skipL039
.condpart1
	LDA #$0A
	STA COLUP1
.skipL039
.L040 ;  if coinvalue  =  32 then COLUP1  =  $1E

	LDA coinvalue
	CMP #32
     BNE .skipL040
.condpart2
	LDA #$1E
	STA COLUP1
.skipL040
.
 ; 

.L041 ;  if hitted  >  0 then hitted  =  hitted  -  1  :  COLUP0  =  $40 else COLUP0  =  $86

	LDA #0
	CMP hitted
     BCS .skipL041
.condpart3
	DEC hitted
	LDA #$40
	STA COLUP0
 jmp .skipelse0
.skipL041
	LDA #$86
	STA COLUP0
.skipelse0
.
 ; 

.L042 ;  if collision(missile0,player0)  &&  haslance  =  0 then haslance  =  1  :  NUSIZ0  =  $00  :  missile0height  =  8

	BIT CXM0P
	BVC .skipL042
.condpart4
	LDA haslance
	CMP #0
     BNE .skip4then
.condpart5
	LDA #1
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip4then
.skipL042
.L043 ;  if collision(missile0,player0)  &&  haslance  =  3 then haslance  =  1  :  NUSIZ0  =  $00  :  missile0height  =  8

	BIT CXM0P
	BVC .skipL043
.condpart6
	LDA haslance
	CMP #3
     BNE .skip6then
.condpart7
	LDA #1
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip6then
.skipL043
.L044 ;  if collision(ball,player0)  &&  hasshield  =  0 then hasshield  =  1

	BIT CXP0FB
	BVC .skipL044
.condpart8
	LDA hasshield
	CMP #0
     BNE .skip8then
.condpart9
	LDA #1
	STA hasshield
.skip8then
.skipL044
.
 ; 

.L045 ;  if minotauro  >  0  &&  hitted  =  0  &&  collision(player1,player0)  &&  hasshield  =  1 then hasshield  =  0  :  ballx  =  0  :  bally  =  0  :  gosub hit

	LDA #0
	CMP minotauro
     BCS .skipL045
.condpart10
	LDA hitted
	CMP #0
     BNE .skip10then
.condpart11
	BIT CXPPMM
	BPL .skip11then
.condpart12
	LDA hasshield
	CMP #1
     BNE .skip12then
.condpart13
	LDA #0
	STA hasshield
	STA ballx
	STA bally
 jsr .hit

.skip12then
.skip11then
.skip10then
.skipL045
.L046 ;  if minotauro  >  0  &&  hitted  =  0  &&  collision(player1,player0)  &&  hasshield  =  0 then drawscreen  :  goto mainloop

	LDA #0
	CMP minotauro
     BCS .skipL046
.condpart14
	LDA hitted
	CMP #0
     BNE .skip14then
.condpart15
	BIT CXPPMM
	BPL .skip15then
.condpart16
	LDA hasshield
	CMP #0
     BNE .skip16then
.condpart17
 jsr drawscreen
 jmp .mainloop

.skip16then
.skip15then
.skip14then
.skipL046
.L047 ;  if minotauro  =  0  &&  collision(player1,player0)  &&  hascoin  =  0 then hascoin  =  1  :  score  =  score  +  coinvalue  :  gosub colocarmoneda

	LDA minotauro
	CMP #0
     BNE .skipL047
.condpart18
	BIT CXPPMM
	BPL .skip18then
.condpart19
	LDA hascoin
	CMP #0
     BNE .skip19then
.condpart20
	LDA #1
	STA hascoin
	SED
	CLC
	LDA score+2
	ADC coinvalue
	STA score+2
	LDA score+1
	ADC #0
	STA score+1
	LDA score
	ADC #0
	STA score
	CLD
 jsr .colocarmoneda

.skip19then
.skip18then
.skipL047
.
 ; 

.L048 ;  if joy0left  &&  !joy0right  &&  !joy0up  &&  !joy0down  &&  noleft  =  0 then gosub moverizquierda

 lda #$40
 bit SWCHA
	BNE .skipL048
.condpart21
 lda #$80
 bit SWCHA
	BEQ .skip21then
.condpart22
 lda #$10
 bit SWCHA
	BEQ .skip22then
.condpart23
 lda #$20
 bit SWCHA
	BEQ .skip23then
.condpart24
	LDA noleft
	CMP #0
     BNE .skip24then
.condpart25
 jsr .moverizquierda

.skip24then
.skip23then
.skip22then
.skip21then
.skipL048
.L049 ;  if !joy0left  &&  joy0right  &&  !joy0up  &&  !joy0down  &&  noright  =  0 then gosub moverderecha

 lda #$40
 bit SWCHA
	BEQ .skipL049
.condpart26
 lda #$80
 bit SWCHA
	BNE .skip26then
.condpart27
 lda #$10
 bit SWCHA
	BEQ .skip27then
.condpart28
 lda #$20
 bit SWCHA
	BEQ .skip28then
.condpart29
	LDA noright
	CMP #0
     BNE .skip29then
.condpart30
 jsr .moverderecha

.skip29then
.skip28then
.skip27then
.skip26then
.skipL049
.L050 ;  if !joy0left  &&  !joy0right  &&  joy0up  &&  !joy0down  &&  noup  =  0 then gosub moverarriba

 lda #$40
 bit SWCHA
	BEQ .skipL050
.condpart31
 lda #$80
 bit SWCHA
	BEQ .skip31then
.condpart32
 lda #$10
 bit SWCHA
	BNE .skip32then
.condpart33
 lda #$20
 bit SWCHA
	BEQ .skip33then
.condpart34
	LDA noup
	CMP #0
     BNE .skip34then
.condpart35
 jsr .moverarriba

.skip34then
.skip33then
.skip32then
.skip31then
.skipL050
.L051 ;  if !joy0left  &&  !joy0right  &&  !joy0up  &&  joy0down  &&  nodown  =  0 then gosub moverabajo

 lda #$40
 bit SWCHA
	BEQ .skipL051
.condpart36
 lda #$80
 bit SWCHA
	BEQ .skip36then
.condpart37
 lda #$10
 bit SWCHA
	BEQ .skip37then
.condpart38
 lda #$20
 bit SWCHA
	BNE .skip38then
.condpart39
	LDA nodown
	CMP #0
     BNE .skip39then
.condpart40
 jsr .moverabajo

.skip39then
.skip38then
.skip37then
.skip36then
.skipL051
.L052 ;  if joy0fire  &&  haslance  =  1 then haslance  =  2

 lda #$80
 bit INPT4
	BNE .skipL052
.condpart41
	LDA haslance
	CMP #1
     BNE .skip41then
.condpart42
	LDA #2
	STA haslance
.skip41then
.skipL052
.
 ; 

.L053 ;  if haslance  =  2  &&  !collision(playfield,missile0) then gosub moverlanza

	LDA haslance
	CMP #2
     BNE .skipL053
.condpart43
	BIT CXM0FB
	BMI .skip43then
.condpart44
 jsr .moverlanza

.skip43then
.skipL053
.L054 ;  if haslance  =  2  &&  collision(playfield,missile0)  &&  compass  =  2 then haslance  =  3  :  missile0x  =  missile0x  -  0

	LDA haslance
	CMP #2
     BNE .skipL054
.condpart45
	BIT CXM0FB
	BPL .skip45then
.condpart46
	LDA compass
	CMP #2
     BNE .skip46then
.condpart47
	LDA #3
	STA haslance
	LDA missile0x
	SEC
	SBC #0
	STA missile0x
.skip46then
.skip45then
.skipL054
.L055 ;  if haslance  =  2  &&  collision(playfield,missile0) then haslance  =  3

	LDA haslance
	CMP #2
     BNE .skipL055
.condpart48
	BIT CXM0FB
	BPL .skip48then
.condpart49
	LDA #3
	STA haslance
.skip48then
.skipL055
.L056 ;  if haslance  =  2  &&  collision(player1,missile0) then haslance  =  3  :  minotauro  =  minotauro  -  1

	LDA haslance
	CMP #2
     BNE .skipL056
.condpart50
	BIT CXM0P
	BPL .skip50then
.condpart51
	LDA #3
	STA haslance
	DEC minotauro
.skip50then
.skipL056
.
 ; 

.L057 ;  if minotauro  >  0 then gosub moverenemigo else gosub minomuerto

	LDA #0
	CMP minotauro
     BCS .skipL057
.condpart52
 jsr .moverenemigo
 jmp .skipelse1
.skipL057
 jsr .minomuerto

.skipelse1
.L058 ;  if minotauro  =  1 then gosub minoheridauno

	LDA minotauro
	CMP #1
     BNE .skipL058
.condpart53
 jsr .minoheridauno

.skipL058
.
 ; 

.L059 ;  if compass  =  1  &&  haslance  =  3 then NUSIZ0  =  $00  :  missile0height  =  8

	LDA compass
	CMP #1
     BNE .skipL059
.condpart54
	LDA haslance
	CMP #3
     BNE .skip54then
.condpart55
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip54then
.skipL059
.L060 ;  if compass  =  2  &&  haslance  =  3 then NUSIZ0  =  $30  :  missile0height  =  0

	LDA compass
	CMP #2
     BNE .skipL060
.condpart56
	LDA haslance
	CMP #3
     BNE .skip56then
.condpart57
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
.skip56then
.skipL060
.L061 ;  if compass  =  3  &&  haslance  =  3 then NUSIZ0  =  $00  :  missile0height  =  8

	LDA compass
	CMP #3
     BNE .skipL061
.condpart58
	LDA haslance
	CMP #3
     BNE .skip58then
.condpart59
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip58then
.skipL061
.L062 ;  if compass  =  4  &&  haslance  =  3 then NUSIZ0  =  $30  :  missile0height  =  0

	LDA compass
	CMP #4
     BNE .skipL062
.condpart60
	LDA haslance
	CMP #3
     BNE .skip60then
.condpart61
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
.skip60then
.skipL062
.
 ; 

.L063 ;  if room  =  1  &&  haslance  =  0 then missile0x  =  83  :  missile0y  =  48

	LDA room
	CMP #1
     BNE .skipL063
.condpart62
	LDA haslance
	CMP #0
     BNE .skip62then
.condpart63
	LDA #83
	STA missile0x
	LDA #48
	STA missile0y
.skip62then
.skipL063
.L064 ;  if room  <>  1  &&  haslance  =  0 then missile0x  =  0  :  missile0y  =  0

	LDA room
	CMP #1
     BEQ .skipL064
.condpart64
	LDA haslance
	CMP #0
     BNE .skip64then
.condpart65
	LDA #0
	STA missile0x
	STA missile0y
.skip64then
.skipL064
.
 ; 

.L065 ;  if room  =  2  &&  hasshield  =  0 then ballx  =  83  :  bally  =  45

	LDA room
	CMP #2
     BNE .skipL065
.condpart66
	LDA hasshield
	CMP #0
     BNE .skip66then
.condpart67
	LDA #83
	STA ballx
	LDA #45
	STA bally
.skip66then
.skipL065
.L066 ;  if room  <>  2  &&  hasshield  =  0 then ballx  =  0  :  bally  =  0

	LDA room
	CMP #2
     BEQ .skipL066
.condpart68
	LDA hasshield
	CMP #0
     BNE .skip68then
.condpart69
	LDA #0
	STA ballx
	STA bally
.skip68then
.skipL066
.
 ; 

.L067 ;  if room  =  1  &&  player0x  >  145 then gosub room2  :  player0x  =  22

	LDA room
	CMP #1
     BNE .skipL067
.condpart70
	LDA #145
	CMP player0x
     BCS .skip70then
.condpart71
 jsr .room2
	LDA #22
	STA player0x
.skip70then
.skipL067
.L068 ;  if room  =  2  &&  player0x  <  5 then gosub room1  :  player0x  =  140

	LDA room
	CMP #2
     BNE .skipL068
.condpart72
	LDA player0x
	CMP #5
     BCS .skip72then
.condpart73
 jsr .room1
	LDA #140
	STA player0x
.skip72then
.skipL068
.
 ; 

.L069 ;  if room  =  1  &&  player0y  <  10 then gosub room3  :  player0y  =  80

	LDA room
	CMP #1
     BNE .skipL069
.condpart74
	LDA player0y
	CMP #10
     BCS .skip74then
.condpart75
 jsr .room3
	LDA #80
	STA player0y
.skip74then
.skipL069
.L070 ;  if room  =  3  &&  player0y  >  85 then gosub room1  :  player0y  =  10

	LDA room
	CMP #3
     BNE .skipL070
.condpart76
	LDA #85
	CMP player0y
     BCS .skip76then
.condpart77
 jsr .room1
	LDA #10
	STA player0y
.skip76then
.skipL070
.
 ; 

.L071 ;  if room  =  3  &&  player0x  >  145 then gosub room4  :  player0x  =  22

	LDA room
	CMP #3
     BNE .skipL071
.condpart78
	LDA #145
	CMP player0x
     BCS .skip78then
.condpart79
 jsr .room4
	LDA #22
	STA player0x
.skip78then
.skipL071
.L072 ;  if room  =  4  &&  player0x  <  5 then gosub room3  :  player0x  =  140

	LDA room
	CMP #4
     BNE .skipL072
.condpart80
	LDA player0x
	CMP #5
     BCS .skip80then
.condpart81
 jsr .room3
	LDA #140
	STA player0x
.skip80then
.skipL072
.
 ; 

.L073 ;  if room  =  2  &&  player0y  <  5 then gosub room4  :  player0y  =  80

	LDA room
	CMP #2
     BNE .skipL073
.condpart82
	LDA player0y
	CMP #5
     BCS .skip82then
.condpart83
 jsr .room4
	LDA #80
	STA player0y
.skip82then
.skipL073
.L074 ;  if room  =  4  &&  player0y  >  85 then gosub room2  :  player0y  =  10

	LDA room
	CMP #4
     BNE .skipL074
.condpart84
	LDA #85
	CMP player0y
     BCS .skip84then
.condpart85
 jsr .room2
	LDA #10
	STA player0y
.skip84then
.skipL074
.
 ; 

.L075 ;  if room  =  3  &&  player0y  <  10 then gosub room5  :  player0y  =  80

	LDA room
	CMP #3
     BNE .skipL075
.condpart86
	LDA player0y
	CMP #10
     BCS .skip86then
.condpart87
 jsr .room5
	LDA #80
	STA player0y
.skip86then
.skipL075
.L076 ;  if room  =  5  &&  player0y  >  85 then gosub room3  :  player0y  =  10

	LDA room
	CMP #5
     BNE .skipL076
.condpart88
	LDA #85
	CMP player0y
     BCS .skip88then
.condpart89
 jsr .room3
	LDA #10
	STA player0y
.skip88then
.skipL076
.
 ; 

.L077 ;  if room  =  4  &&  player0x  >  145 then gosub room6  :  player0x  =  22

	LDA room
	CMP #4
     BNE .skipL077
.condpart90
	LDA #145
	CMP player0x
     BCS .skip90then
.condpart91
 jsr .room6
	LDA #22
	STA player0x
.skip90then
.skipL077
.L078 ;  if room  =  6  &&  player0x  <  5 then gosub room4  :  player0x  =  140

	LDA room
	CMP #6
     BNE .skipL078
.condpart92
	LDA player0x
	CMP #5
     BCS .skip92then
.condpart93
 jsr .room4
	LDA #140
	STA player0x
.skip92then
.skipL078
.
 ; 

.L079 ;  if room  =  6  &&  player0x  >  145 then gosub room7  :  player0x  =  22

	LDA room
	CMP #6
     BNE .skipL079
.condpart94
	LDA #145
	CMP player0x
     BCS .skip94then
.condpart95
 jsr .room7
	LDA #22
	STA player0x
.skip94then
.skipL079
.L080 ;  if room  =  7  &&  player0x  <  5 then gosub room6  :  player0x  =  140

	LDA room
	CMP #7
     BNE .skipL080
.condpart96
	LDA player0x
	CMP #5
     BCS .skip96then
.condpart97
 jsr .room6
	LDA #140
	STA player0x
.skip96then
.skipL080
.
 ; 

.L081 ;  if room  =  7  &&  player0x  >  145 then gosub room9  :  player0x  =  22

	LDA room
	CMP #7
     BNE .skipL081
.condpart98
	LDA #145
	CMP player0x
     BCS .skip98then
.condpart99
 jsr .room9
	LDA #22
	STA player0x
.skip98then
.skipL081
.L082 ;  if room  =  9  &&  player0x  <  5 then gosub room7  :  player0x  =  140

	LDA room
	CMP #9
     BNE .skipL082
.condpart100
	LDA player0x
	CMP #5
     BCS .skip100then
.condpart101
 jsr .room7
	LDA #140
	STA player0x
.skip100then
.skipL082
.
 ; 

.L083 ;  if room  =  8  &&  player0y  <  5 then gosub room7  :  player0y  =  80

	LDA room
	CMP #8
     BNE .skipL083
.condpart102
	LDA player0y
	CMP #5
     BCS .skip102then
.condpart103
 jsr .room7
	LDA #80
	STA player0y
.skip102then
.skipL083
.L084 ;  if room  =  7  &&  player0y  >  85 then gosub room8  :  player0y  =  10

	LDA room
	CMP #7
     BNE .skipL084
.condpart104
	LDA #85
	CMP player0y
     BCS .skip104then
.condpart105
 jsr .room8
	LDA #10
	STA player0y
.skip104then
.skipL084
.
 ; 

.L085 ;  drawscreen

 jsr drawscreen
.L086 ;  goto mainloop

 jmp .mainloop

.
 ; 

.moverizquierda
 ; moverizquierda

.L087 ;  player0:

	LDA #<playerL087_0

	STA player0pointerlo
	LDA #>playerL087_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L088 ;  if haslance  =  1 then compass  =  4

	LDA haslance
	CMP #1
     BNE .skipL088
.condpart106
	LDA #4
	STA compass
.skipL088
.L089 ;  if collision(playfield,player0) then player0x  =  player0x  +  1  :  noright  =  0  :  noleft  =  1  :  noup  =  0  :  nodown  =  0 else player0x  =  player0x  -  1  :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL089
.condpart107
	INC player0x
	LDA #0
	STA noright
	LDA #1
	STA noleft
	LDA #0
	STA noup
	STA nodown
 jmp .skipelse2
.skipL089
	DEC player0x
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse2
.L090 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL090
.condpart108
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL090
.L091 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL091
.condpart109
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL091
.L092 ;  return

	RTS
.
 ; 

.moverderecha
 ; moverderecha

.L093 ;  player0:

	LDA #<playerL093_0

	STA player0pointerlo
	LDA #>playerL093_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L094 ;  if haslance  =  1 then compass  =  2

	LDA haslance
	CMP #1
     BNE .skipL094
.condpart110
	LDA #2
	STA compass
.skipL094
.L095 ;  if collision(playfield,player0) then player0x  =  player0x  -  1  :  noright  =  1  :  noleft  =  0  :  noup  =  0  :  nodown  =  0 else player0x  =  player0x  +  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL095
.condpart111
	DEC player0x
	LDA #1
	STA noright
	LDA #0
	STA noleft
	STA noup
	STA nodown
 jmp .skipelse3
.skipL095
	INC player0x
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse3
.L096 ;  if haslance  =  1 then missile0x  =  player0x  +  9  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL096
.condpart112
	LDA player0x
	CLC
	ADC #9
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL096
.L097 ;  if hasshield  =  1 then ballx  =  player0x  -  1  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL097
.condpart113
	LDA player0x
	SEC
	SBC #1
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL097
.L098 ;  return

	RTS
.
 ; 

.moverarriba
 ; moverarriba

.L099 ;  player0:

	LDA #<playerL099_0

	STA player0pointerlo
	LDA #>playerL099_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L0100 ;  if haslance  =  1 then compass  =  1

	LDA haslance
	CMP #1
     BNE .skipL0100
.condpart114
	LDA #1
	STA compass
.skipL0100
.L0101 ;  if collision(playfield,player0) then player0y  =  player0y  +  1  :  noright  =  0  :  noleft  =  0  :  noup  =  1  :  nodown  =  0 else player0y  =  player0y  -  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL0101
.condpart115
	INC player0y
	LDA #0
	STA noright
	STA noleft
	LDA #1
	STA noup
	LDA #0
	STA nodown
 jmp .skipelse4
.skipL0101
	DEC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse4
.L0102 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL0102
.condpart116
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL0102
.L0103 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL0103
.condpart117
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL0103
.L0104 ;  return

	RTS
.
 ; 

.moverabajo
 ; moverabajo

.L0105 ;  player0:

	LDA #<playerL0105_0

	STA player0pointerlo
	LDA #>playerL0105_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L0106 ;  if haslance  =  1 then compass  =  3

	LDA haslance
	CMP #1
     BNE .skipL0106
.condpart118
	LDA #3
	STA compass
.skipL0106
.L0107 ;  if collision(playfield,player0) then player0y  =  player0y  -  1  :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  1 else player0y  =  player0y  +  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL0107
.condpart119
	DEC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	LDA #1
	STA nodown
 jmp .skipelse5
.skipL0107
	INC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse5
.L0108 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL0108
.condpart120
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL0108
.L0109 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL0109
.condpart121
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL0109
.L0110 ;  return

	RTS
.
 ; 

.hit
 ; hit

.L0111 ;  if hitted  =  0 then hitted  =  100

	LDA hitted
	CMP #0
     BNE .skipL0111
.condpart122
	LDA #100
	STA hitted
.skipL0111
.L0112 ;  return

	RTS
.
 ; 

.colocarmoneda
 ; colocarmoneda

.L0113 ;  gosub minoheridados

 jsr .minoheridados

.L0114 ;  if hascoin  =  1 then player1x  =  0  :  player1y  =  0

	LDA hascoin
	CMP #1
     BNE .skipL0114
.condpart123
	LDA #0
	STA player1x
	STA player1y
.skipL0114
.
 ; 

.L0115 ;  randnumber  =  rand

 jsr randomize
	STA randnumber
.L0116 ;  if hascoin  =  0  &&  randnumber  <=  153 then coinvalue  =  1

	LDA hascoin
	CMP #0
     BNE .skipL0116
.condpart124
	LDA #153
	CMP randnumber
     BCC .skip124then
.condpart125
	LDA #1
	STA coinvalue
.skip124then
.skipL0116
.L0117 ;  if hascoin  =  0  &&  randnumber  >  153  &&  randnumber  <=  204 then coinvalue  =  5

	LDA hascoin
	CMP #0
     BNE .skipL0117
.condpart126
	LDA #153
	CMP randnumber
     BCS .skip126then
.condpart127
	LDA #204
	CMP randnumber
     BCC .skip127then
.condpart128
	LDA #5
	STA coinvalue
.skip127then
.skip126then
.skipL0117
.L0118 ;  if hascoin  =  0  &&  randnumber  >  204  &&  randnumber  <=  255 then coinvalue  =  32

	LDA hascoin
	CMP #0
     BNE .skipL0118
.condpart129
	LDA #204
	CMP randnumber
     BCS .skip129then
.condpart130
	LDA #255
	CMP randnumber
     BCC .skip130then
.condpart131
	LDA #32
	STA coinvalue
.skip130then
.skip129then
.skipL0118
.
 ; 

.L0119 ;  randnumber  =  rand

 jsr randomize
	STA randnumber
.L0120 ;  if hascoin  =  0  &&  randnumber  <=  64 then player1x  =  28  :  player1y  =  22

	LDA hascoin
	CMP #0
     BNE .skipL0120
.condpart132
	LDA #64
	CMP randnumber
     BCC .skip132then
.condpart133
	LDA #28
	STA player1x
	LDA #22
	STA player1y
.skip132then
.skipL0120
.L0121 ;  if hascoin  =  0  &&  randnumber  >  65  &&  randnumber  <=  128 then player1x  =  118  :  player1y  =  22

	LDA hascoin
	CMP #0
     BNE .skipL0121
.condpart134
	LDA #65
	CMP randnumber
     BCS .skip134then
.condpart135
	LDA #128
	CMP randnumber
     BCC .skip135then
.condpart136
	LDA #118
	STA player1x
	LDA #22
	STA player1y
.skip135then
.skip134then
.skipL0121
.L0122 ;  if hascoin  =  0  &&  randnumber  >  129  &&  randnumber  <=  192 then player1x  =  28  :  player1y  =  77

	LDA hascoin
	CMP #0
     BNE .skipL0122
.condpart137
	LDA #129
	CMP randnumber
     BCS .skip137then
.condpart138
	LDA #192
	CMP randnumber
     BCC .skip138then
.condpart139
	LDA #28
	STA player1x
	LDA #77
	STA player1y
.skip138then
.skip137then
.skipL0122
.L0123 ;  if hascoin  =  0  &&  randnumber  >  193  &&  randnumber  <=  255 then player1x  =  118  :  player1y  =  77

	LDA hascoin
	CMP #0
     BNE .skipL0123
.condpart140
	LDA #193
	CMP randnumber
     BCS .skip140then
.condpart141
	LDA #255
	CMP randnumber
     BCC .skip141then
.condpart142
	LDA #118
	STA player1x
	LDA #77
	STA player1y
.skip141then
.skip140then
.skipL0123
.L0124 ;  return

	RTS
.
 ; 

.moverlanza
 ; moverlanza

.L0125 ;  if compass  =  1 then NUSIZ0  =  $00  :  missile0height  =  8  :  missile0y  =  missile0y  -  2

	LDA compass
	CMP #1
     BNE .skipL0125
.condpart143
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
	LDA missile0y
	SEC
	SBC #2
	STA missile0y
.skipL0125
.L0126 ;  if compass  =  2 then NUSIZ0  =  $30  :  missile0height  =  0  :  missile0x  =  missile0x  +  2

	LDA compass
	CMP #2
     BNE .skipL0126
.condpart144
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
	LDA missile0x
	CLC
	ADC #2
	STA missile0x
.skipL0126
.L0127 ;  if compass  =  3 then NUSIZ0  =  $00  :  missile0height  =  8  :  missile0y  =  missile0y  +  2

	LDA compass
	CMP #3
     BNE .skipL0127
.condpart145
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
	LDA missile0y
	CLC
	ADC #2
	STA missile0y
.skipL0127
.L0128 ;  if compass  =  4 then NUSIZ0  =  $30  :  missile0height  =  0  :  missile0x  =  missile0x  -  2

	LDA compass
	CMP #4
     BNE .skipL0128
.condpart146
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
	LDA missile0x
	SEC
	SBC #2
	STA missile0x
.skipL0128
.L0129 ;  return

	RTS
.
 ; 

.moverenemigo
 ; moverenemigo

.L0130 ;  if minotauro  >  0  &&  player1y  =  22  &&  player1x  <  118 then player1x  =  player1x  +  1

	LDA #0
	CMP minotauro
     BCS .skipL0130
.condpart147
	LDA player1y
	CMP #22
     BNE .skip147then
.condpart148
	LDA player1x
	CMP #118
     BCS .skip148then
.condpart149
	INC player1x
.skip148then
.skip147then
.skipL0130
.L0131 ;  if minotauro  >  0  &&  player1y  =  77  &&  player1x  >  28 then player1x  =  player1x  -  1

	LDA #0
	CMP minotauro
     BCS .skipL0131
.condpart150
	LDA player1y
	CMP #77
     BNE .skip150then
.condpart151
	LDA #28
	CMP player1x
     BCS .skip151then
.condpart152
	DEC player1x
.skip151then
.skip150then
.skipL0131
.L0132 ;  if minotauro  >  0  &&  player1x  =  118  &&  player1y  <  77 then player1y  =  player1y  +  1

	LDA #0
	CMP minotauro
     BCS .skipL0132
.condpart153
	LDA player1x
	CMP #118
     BNE .skip153then
.condpart154
	LDA player1y
	CMP #77
     BCS .skip154then
.condpart155
	INC player1y
.skip154then
.skip153then
.skipL0132
.L0133 ;  if minotauro  >  0  &&  player1x  =  28  &&  player1y  >  22 then player1y  =  player1y  -  1

	LDA #0
	CMP minotauro
     BCS .skipL0133
.condpart156
	LDA player1x
	CMP #28
     BNE .skip156then
.condpart157
	LDA #22
	CMP player1y
     BCS .skip157then
.condpart158
	DEC player1y
.skip157then
.skip156then
.skipL0133
.L0134 ;  return

	RTS
.
 ; 

.minoheridados
 ; minoheridados

.L0135 ;  player1:

	LDA #<playerL0135_1

	STA player1pointerlo
	LDA #>playerL0135_1

	STA player1pointerhi
	LDA #12
	STA player1height
.L0136 ;  minotauro  =  2

	LDA #2
	STA minotauro
.L0137 ;  return

	RTS
.
 ; 

.minoheridauno
 ; minoheridauno

.L0138 ;  player1:

	LDA #<playerL0138_1

	STA player1pointerlo
	LDA #>playerL0138_1

	STA player1pointerhi
	LDA #11
	STA player1height
.L0139 ;  return

	RTS
.
 ; 

.minomuerto
 ; minomuerto

.L0140 ;  player1:

	LDA #<playerL0140_1

	STA player1pointerlo
	LDA #>playerL0140_1

	STA player1pointerhi
	LDA #8
	STA player1height
.L0141 ;  return

	RTS
.
 ; 

.room1
 ; room1

.L0142 ;  room  =  1

	LDA #1
	STA room
.L0143 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0144 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.L0145 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel0
PF_data0
	.byte %11111111, %00011111, %00001111, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00000000, %00010000, %00000000
	.byte %10000000, %00000000, %00010000, %00000000
	.byte %10000000, %00000000, %00010000, %00000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L0146 ;  if haslance  =  1 then gosub colocarmoneda else hascoin  =  1  :  gosub colocarmoneda

	LDA haslance
	CMP #1
     BNE .skipL0146
.condpart159
 jsr .colocarmoneda
 jmp .skipelse6
.skipL0146
	LDA #1
	STA hascoin
 jsr .colocarmoneda

.skipelse6
.L0147 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0147
.condpart160
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0147
.
 ; 

.L0148 ;  return

	RTS
.
 ; 

.room2
 ; room2

.L0149 ;  room  =  2

	LDA #2
	STA room
.L0150 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0151 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel1
PF_data1
	.byte %11111111, %00011111, %00001111, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %00000000, %00100000, %00000000, %10000000
	.byte %00000000, %00100000, %00000000, %10000000
	.byte %00000000, %00100000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.L0152 ;  if hasshield  =  1 then gosub colocarmoneda else hascoin  =  1  :  gosub colocarmoneda

	LDA hasshield
	CMP #1
     BNE .skipL0152
.condpart161
 jsr .colocarmoneda
 jmp .skipelse7
.skipL0152
	LDA #1
	STA hascoin
 jsr .colocarmoneda

.skipelse7
.L0153 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0153
.condpart162
	LDA #0
	STA haslance
.skipL0153
.L0154 ;  return

	RTS
.
 ; 

.room3
 ; room3

.L0155 ;  room  =  3

	LDA #3
	STA room
.L0156 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0157 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel2
PF_data2
	.byte %10000001, %11111111, %00000001, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00000000, %00000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %00011111, %00001111, %11111111
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.L0158 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0159 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0159
.condpart163
	LDA #0
	STA haslance
.skipL0159
.L0160 ;  return

	RTS
.
 ; 

.room4
 ; room4

.L0161 ;  room  =  4

	LDA #4
	STA room
.L0162 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0163 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel3
PF_data3
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00010000, %00001000, %10000000
	.byte %00000000, %00010000, %00001000, %00000000
	.byte %00000000, %00010000, %00001000, %00000000
	.byte %00000000, %00010000, %00001000, %00000000
	.byte %10000000, %00010000, %00001000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %00011111, %00001111, %11111111
pflabel3
	lda PF_data3,x
	sta playfield,x
	dex
	bpl pflabel3
.L0164 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0165 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0165
.condpart164
	LDA #0
	STA haslance
.skipL0165
.L0166 ;  return

	RTS
.
 ; 

.room5
 ; room5

.L0167 ;  room  =  5

	LDA #5
	STA room
.L0168 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0169 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel4
PF_data4
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000001, %11111111, %11111111, %11111111
	.byte %10000001, %00000000, %00000000, %10000000
	.byte %10000001, %00000000, %00000000, %10000000
	.byte %10000001, %00000000, %00000000, %10000000
	.byte %10000001, %00000000, %00000000, %10000000
	.byte %10000001, %00000000, %00000000, %10000000
	.byte %10000001, %11111111, %00000001, %11111111
pflabel4
	lda PF_data4,x
	sta playfield,x
	dex
	bpl pflabel4
.L0170 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0171 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0171
.condpart165
	LDA #0
	STA haslance
.skipL0171
.L0172 ;  return

	RTS
.
 ; 

.room6
 ; room6

.L0173 ;  room  =  6

	LDA #6
	STA room
.L0174 ;  hascoin  =  1

	LDA #1
	STA hascoin
.L0175 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel5
PF_data5
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %00000000
	.byte %11111111, %11111111, %11111111, %00000000
	.byte %11111111, %11111111, %11111111, %00000000
	.byte %00000000, %11111110, %11111111, %10000000
	.byte %00000000, %11111110, %11111111, %10000000
	.byte %00000000, %11111110, %11111111, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel5
	lda PF_data5,x
	sta playfield,x
	dex
	bpl pflabel5
.L0176 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0177 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0177
.condpart166
	LDA #0
	STA haslance
.skipL0177
.L0178 ;  return

	RTS
.
 ; 

.room7
 ; room7

.L0179 ;  room  =  7

	LDA #7
	STA room
.L0180 ;  hascoin  =  1

	LDA #1
	STA hascoin
.L0181 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel6
PF_data6
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %00000000, %00000000, %00001111, %11111111
	.byte %00000000, %00000000, %00001111, %11111111
	.byte %00000000, %00000000, %00001111, %11111111
	.byte %11111111, %00011111, %00000000, %10000000
	.byte %11111111, %00011111, %00000000, %10000000
	.byte %11111111, %00011111, %00000000, %10000000
	.byte %11111111, %00011111, %00001111, %11111111
	.byte %11111111, %00011111, %00001000, %00000000
	.byte %11111111, %00011111, %00001000, %00000000
	.byte %11111111, %00011111, %00001000, %10000000
pflabel6
	lda PF_data6,x
	sta playfield,x
	dex
	bpl pflabel6
.L0182 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0183 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0183
.condpart167
	LDA #0
	STA haslance
.skipL0183
.L0184 ;  return

	RTS
.
 ; 

.room8
 ; room8

.L0185 ;  room  =  8

	LDA #8
	STA room
.L0186 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0187 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel7
PF_data7
	.byte %11111111, %00011111, %00001100, %10000000
	.byte %10000000, %00000000, %00001100, %10000000
	.byte %10000000, %00000000, %00001100, %10000000
	.byte %10000000, %00000000, %00001100, %10000000
	.byte %10000000, %00000000, %00001100, %10000000
	.byte %10000000, %00000000, %00001100, %10000000
	.byte %10000000, %11111111, %11111100, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel7
	lda PF_data7,x
	sta playfield,x
	dex
	bpl pflabel7
.L0188 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0189 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0189
.condpart168
	LDA #0
	STA haslance
.skipL0189
.L0190 ;  return

	RTS
.
 ; 

.room9
 ; room9

.L0191 ;  room  =  9

	LDA #9
	STA room
.L0192 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0193 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel8
PF_data8
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %10000000, %00100000, %00000000, %10000000
	.byte %10000000, %00100000, %00000000, %10000000
	.byte %10000000, %00100000, %00000000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %00000000, %00000000, %00000000, %10000000
	.byte %00000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel8
	lda PF_data8,x
	sta playfield,x
	dex
	bpl pflabel8
.L0194 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0195 ;  if haslance  =  3 then haslance  =  0

	LDA haslance
	CMP #3
     BNE .skipL0195
.condpart169
	LDA #0
	STA haslance
.skipL0195
.L0196 ;  return

	RTS
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL087_0

	.byte 0
	.byte  %01101100
	.byte  %00100100
	.byte  %00100100
	.byte  %00011000
	.byte  %11111111
	.byte  %10011001
	.byte  %00100100
	.byte  %00111100
	.byte  %00110110
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL093_0

	.byte 0
	.byte  %00110110
	.byte  %00100100
	.byte  %00100100
	.byte  %00011000
	.byte  %11111111
	.byte  %10011001
	.byte  %00100100
	.byte  %00111100
	.byte  %01101100
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL099_0

	.byte 0
	.byte  %01100110
	.byte  %00100100
	.byte  %00100100
	.byte  %00011000
	.byte  %11111111
	.byte  %10011001
	.byte  %00100100
	.byte  %00111100
	.byte  %00100100
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0105_0

	.byte 0
	.byte  %01100110
	.byte  %00100100
	.byte  %00100100
	.byte  %10011001
	.byte  %11111111
	.byte  %00011000
	.byte  %00100100
	.byte  %00111100
	.byte  %00100100
 if (<*) > (<(*+13))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0135_1

	.byte 0
	.byte  %01101100
	.byte  %01101100
	.byte  %00100100
	.byte  %00100100
	.byte  %00011000
	.byte  %00011000
	.byte  %11011011
	.byte  %11111111
	.byte  %00011000
	.byte  %00100100
	.byte  %01111110
	.byte  %01000010
 if (<*) > (<(*+12))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0138_1

	.byte 0
	.byte  %01101100
	.byte  %01101100
	.byte  %00100100
	.byte  %00100100
	.byte  %00011000
	.byte  %00011000
	.byte  %11011011
	.byte  %11111111
	.byte  %00011000
	.byte  %00100100
	.byte  %00111100
 if (<*) > (<(*+9))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0140_1

	.byte 0
	.byte  %00010000
	.byte  %00001000
	.byte  %11101011
	.byte  %00011101
	.byte  %00011101
	.byte  %11101011
	.byte  %00001000
	.byte  %00010000
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif


scoretable
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 


 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
