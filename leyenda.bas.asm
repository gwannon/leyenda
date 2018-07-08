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
.L00 ;  player1:

	LDA #<playerL00_1

	STA player1pointerlo
	LDA #>playerL00_1

	STA player1pointerhi
	LDA #4
	STA player1height
.L01 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L02 ;  COLUBK  =  $04

	LDA #$04
	STA COLUBK
.L03 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.
 ; 

.L04 ;  dim nodown  =  a

.L05 ;  dim noup  =  b

.L06 ;  dim noleft  =  c

.L07 ;  dim noright  =  d

.L08 ;  dim room  =  e

.L09 ;  dim haslance  =  f

.L010 ;  dim hasshield  =  g

.L011 ;  dim hascoin  =  h

.L012 ;  dim randnumber  =  i

.L013 ;  dim coinvalue  =  j

.L014 ;  dim compass  =  k

.
 ; 

.L015 ;  haslance  =  0

	LDA #0
	STA haslance
.L016 ;  hasshield  =  0

	LDA #0
	STA hasshield
.L017 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L018 ;  coinvalue  =  1

	LDA #1
	STA coinvalue
.
 ; 

.L019 ;  room  =  0

	LDA #0
	STA room
.L020 ;  nodown  =  0

	LDA #0
	STA nodown
.L021 ;  noup  =  0

	LDA #0
	STA noup
.L022 ;  noleft  =  0

	LDA #0
	STA noleft
.L023 ;  noright  =  0

	LDA #0
	STA noright
.
 ; 

.L024 ;  player0x  =  21

	LDA #21
	STA player0x
.L025 ;  player0y  =  80

	LDA #80
	STA player0y
.
 ; 

.
 ; 

.L026 ;  missile0height  =  8

	LDA #8
	STA missile0height
.L027 ;  missile0x  =  83

	LDA #83
	STA missile0x
.L028 ;  missile0y  =  48

	LDA #48
	STA missile0y
.
 ; 

.L029 ;  player1x  =  120

	LDA #120
	STA player1x
.L030 ;  player1y  =  20

	LDA #20
	STA player1y
.
 ; 

.L031 ;  ballheight  =  4

	LDA #4
	STA ballheight
.L032 ;  CTRLPF  =  $21

	LDA #$21
	STA CTRLPF
.L033 ;  ballx  =  0

	LDA #0
	STA ballx
.L034 ;  bally  =  0

	LDA #0
	STA bally
.
 ; 

.mainloop
 ; mainloop

.
 ; 

.L035 ;  if room  =  0 then gosub room1  :  gosub moverderecha

	LDA room
	CMP #0
     BNE .skipL035
.condpart0
 jsr .room1
 jsr .moverderecha

.skipL035
.L036 ;  const screenheight = 84

.
 ; 

.L037 ;  COLUP0  =  $86

	LDA #$86
	STA COLUP0
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

.L041 ;  AUDV0  =  0

	LDA #0
	STA AUDV0
.
 ; 

.L042 ;  if joy0left  &&  !joy0right  &&  !joy0up  &&  !joy0down  &&  noleft  =  0 then gosub moverizquierda

 lda #$40
 bit SWCHA
	BNE .skipL042
.condpart3
 lda #$80
 bit SWCHA
	BEQ .skip3then
.condpart4
 lda #$10
 bit SWCHA
	BEQ .skip4then
.condpart5
 lda #$20
 bit SWCHA
	BEQ .skip5then
.condpart6
	LDA noleft
	CMP #0
     BNE .skip6then
.condpart7
 jsr .moverizquierda

.skip6then
.skip5then
.skip4then
.skip3then
.skipL042
.L043 ;  if !joy0left  &&  joy0right  &&  !joy0up  &&  !joy0down  &&  noright  =  0 then gosub moverderecha

 lda #$40
 bit SWCHA
	BEQ .skipL043
.condpart8
 lda #$80
 bit SWCHA
	BNE .skip8then
.condpart9
 lda #$10
 bit SWCHA
	BEQ .skip9then
.condpart10
 lda #$20
 bit SWCHA
	BEQ .skip10then
.condpart11
	LDA noright
	CMP #0
     BNE .skip11then
.condpart12
 jsr .moverderecha

.skip11then
.skip10then
.skip9then
.skip8then
.skipL043
.L044 ;  if !joy0left  &&  !joy0right  &&  joy0up  &&  !joy0down  &&  noup  =  0 then gosub moverarriba

 lda #$40
 bit SWCHA
	BEQ .skipL044
.condpart13
 lda #$80
 bit SWCHA
	BEQ .skip13then
.condpart14
 lda #$10
 bit SWCHA
	BNE .skip14then
.condpart15
 lda #$20
 bit SWCHA
	BEQ .skip15then
.condpart16
	LDA noup
	CMP #0
     BNE .skip16then
.condpart17
 jsr .moverarriba

.skip16then
.skip15then
.skip14then
.skip13then
.skipL044
.L045 ;  if !joy0left  &&  !joy0right  &&  !joy0up  &&  joy0down  &&  nodown  =  0 then gosub moverabajo

 lda #$40
 bit SWCHA
	BEQ .skipL045
.condpart18
 lda #$80
 bit SWCHA
	BEQ .skip18then
.condpart19
 lda #$10
 bit SWCHA
	BEQ .skip19then
.condpart20
 lda #$20
 bit SWCHA
	BNE .skip20then
.condpart21
	LDA nodown
	CMP #0
     BNE .skip21then
.condpart22
 jsr .moverabajo

.skip21then
.skip20then
.skip19then
.skip18then
.skipL045
.L046 ;  if joy0fire  &&  haslance  =  1 then haslance  =  2

 lda #$80
 bit INPT4
	BNE .skipL046
.condpart23
	LDA haslance
	CMP #1
     BNE .skip23then
.condpart24
	LDA #2
	STA haslance
.skip23then
.skipL046
.
 ; 

.L047 ;  if haslance  =  2  &&  !collision(playfield,missile0) then gosub moverlanza

	LDA haslance
	CMP #2
     BNE .skipL047
.condpart25
	BIT CXM0FB
	BMI .skip25then
.condpart26
 jsr .moverlanza

.skip25then
.skipL047
.L048 ;  if haslance  =  2  &&  collision(playfield,missile0)  &&  compass  =  2 then haslance  =  3  :  missile0x  =  missile0x  -  0

	LDA haslance
	CMP #2
     BNE .skipL048
.condpart27
	BIT CXM0FB
	BPL .skip27then
.condpart28
	LDA compass
	CMP #2
     BNE .skip28then
.condpart29
	LDA #3
	STA haslance
	LDA missile0x
	SEC
	SBC #0
	STA missile0x
.skip28then
.skip27then
.skipL048
.L049 ;  if haslance  =  2  &&  collision(playfield,missile0) then haslance  =  3

	LDA haslance
	CMP #2
     BNE .skipL049
.condpart30
	BIT CXM0FB
	BPL .skip30then
.condpart31
	LDA #3
	STA haslance
.skip30then
.skipL049
.
 ; 

.L050 ;  if compass  =  1  &&  haslance  =  3 then NUSIZ0  =  $00  :  missile0height  =  8

	LDA compass
	CMP #1
     BNE .skipL050
.condpart32
	LDA haslance
	CMP #3
     BNE .skip32then
.condpart33
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip32then
.skipL050
.L051 ;  if compass  =  2  &&  haslance  =  3 then NUSIZ0  =  $30  :  missile0height  =  0

	LDA compass
	CMP #2
     BNE .skipL051
.condpart34
	LDA haslance
	CMP #3
     BNE .skip34then
.condpart35
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
.skip34then
.skipL051
.L052 ;  if compass  =  3  &&  haslance  =  3 then NUSIZ0  =  $00  :  missile0height  =  8

	LDA compass
	CMP #3
     BNE .skipL052
.condpart36
	LDA haslance
	CMP #3
     BNE .skip36then
.condpart37
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip36then
.skipL052
.L053 ;  if compass  =  4  &&  haslance  =  3 then NUSIZ0  =  $30  :  missile0height  =  0

	LDA compass
	CMP #4
     BNE .skipL053
.condpart38
	LDA haslance
	CMP #3
     BNE .skip38then
.condpart39
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
.skip38then
.skipL053
.
 ; 

.L054 ;  if collision(missile0,player0)  &&  haslance  =  0 then haslance  =  1  :  NUSIZ0  =  $00  :  missile0height  =  8

	BIT CXM0P
	BVC .skipL054
.condpart40
	LDA haslance
	CMP #0
     BNE .skip40then
.condpart41
	LDA #1
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip40then
.skipL054
.L055 ;  if collision(missile0,player0)  &&  haslance  =  3 then haslance  =  1  :  NUSIZ0  =  $00  :  missile0height  =  8

	BIT CXM0P
	BVC .skipL055
.condpart42
	LDA haslance
	CMP #3
     BNE .skip42then
.condpart43
	LDA #1
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skip42then
.skipL055
.L056 ;  if collision(ball,player0)  &&  hasshield  =  0 then hasshield  =  1

	BIT CXP0FB
	BVC .skipL056
.condpart44
	LDA hasshield
	CMP #0
     BNE .skip44then
.condpart45
	LDA #1
	STA hasshield
.skip44then
.skipL056
.L057 ;  if collision(player1,player0)  &&  hascoin  =  0 then hascoin  =  1  :  score  =  score  +  coinvalue  :  gosub colocarmoneda

	BIT CXPPMM
	BPL .skipL057
.condpart46
	LDA hascoin
	CMP #0
     BNE .skip46then
.condpart47
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

.skip46then
.skipL057
.
 ; 

.L058 ;  if room  =  1  &&  haslance  =  0 then missile0x  =  83  :  missile0y  =  48

	LDA room
	CMP #1
     BNE .skipL058
.condpart48
	LDA haslance
	CMP #0
     BNE .skip48then
.condpart49
	LDA #83
	STA missile0x
	LDA #48
	STA missile0y
.skip48then
.skipL058
.L059 ;  if room  <>  1  &&  haslance  =  0 then missile0x  =  0  :  missile0y  =  0

	LDA room
	CMP #1
     BEQ .skipL059
.condpart50
	LDA haslance
	CMP #0
     BNE .skip50then
.condpart51
	LDA #0
	STA missile0x
	STA missile0y
.skip50then
.skipL059
.
 ; 

.L060 ;  if room  =  2  &&  hasshield  =  0 then ballx  =  83  :  bally  =  45

	LDA room
	CMP #2
     BNE .skipL060
.condpart52
	LDA hasshield
	CMP #0
     BNE .skip52then
.condpart53
	LDA #83
	STA ballx
	LDA #45
	STA bally
.skip52then
.skipL060
.L061 ;  if room  <>  2  &&  hasshield  =  0 then ballx  =  0  :  bally  =  0

	LDA room
	CMP #2
     BEQ .skipL061
.condpart54
	LDA hasshield
	CMP #0
     BNE .skip54then
.condpart55
	LDA #0
	STA ballx
	STA bally
.skip54then
.skipL061
.
 ; 

.L062 ;  if room  =  1  &&  player0x  >  145 then gosub room2  :  player0x  =  22

	LDA room
	CMP #1
     BNE .skipL062
.condpart56
	LDA #145
	CMP player0x
     BCS .skip56then
.condpart57
 jsr .room2
	LDA #22
	STA player0x
.skip56then
.skipL062
.L063 ;  if room  =  2  &&  player0x  <  5 then gosub room1  :  player0x  =  140

	LDA room
	CMP #2
     BNE .skipL063
.condpart58
	LDA player0x
	CMP #5
     BCS .skip58then
.condpart59
 jsr .room1
	LDA #140
	STA player0x
.skip58then
.skipL063
.
 ; 

.L064 ;  if room  =  1  &&  player0y  <  10 then gosub room3  :  player0y  =  80

	LDA room
	CMP #1
     BNE .skipL064
.condpart60
	LDA player0y
	CMP #10
     BCS .skip60then
.condpart61
 jsr .room3
	LDA #80
	STA player0y
.skip60then
.skipL064
.L065 ;  if room  =  3  &&  player0y  >  85 then gosub room1  :  player0y  =  10

	LDA room
	CMP #3
     BNE .skipL065
.condpart62
	LDA #85
	CMP player0y
     BCS .skip62then
.condpart63
 jsr .room1
	LDA #10
	STA player0y
.skip62then
.skipL065
.
 ; 

.L066 ;  if room  =  3  &&  player0x  >  145 then gosub room4  :  player0x  =  22

	LDA room
	CMP #3
     BNE .skipL066
.condpart64
	LDA #145
	CMP player0x
     BCS .skip64then
.condpart65
 jsr .room4
	LDA #22
	STA player0x
.skip64then
.skipL066
.L067 ;  if room  =  4  &&  player0x  <  5 then gosub room3  :  player0x  =  140

	LDA room
	CMP #4
     BNE .skipL067
.condpart66
	LDA player0x
	CMP #5
     BCS .skip66then
.condpart67
 jsr .room3
	LDA #140
	STA player0x
.skip66then
.skipL067
.
 ; 

.L068 ;  if room  =  2  &&  player0y  <  5 then gosub room4  :  player0y  =  80

	LDA room
	CMP #2
     BNE .skipL068
.condpart68
	LDA player0y
	CMP #5
     BCS .skip68then
.condpart69
 jsr .room4
	LDA #80
	STA player0y
.skip68then
.skipL068
.L069 ;  if room  =  4  &&  player0y  >  85 then gosub room2  :  player0y  =  10

	LDA room
	CMP #4
     BNE .skipL069
.condpart70
	LDA #85
	CMP player0y
     BCS .skip70then
.condpart71
 jsr .room2
	LDA #10
	STA player0y
.skip70then
.skipL069
.
 ; 

.L070 ;  if room  =  3  &&  player0y  <  10 then gosub room5  :  player0y  =  80

	LDA room
	CMP #3
     BNE .skipL070
.condpart72
	LDA player0y
	CMP #10
     BCS .skip72then
.condpart73
 jsr .room5
	LDA #80
	STA player0y
.skip72then
.skipL070
.L071 ;  if room  =  5  &&  player0y  >  85 then gosub room3  :  player0y  =  10

	LDA room
	CMP #5
     BNE .skipL071
.condpart74
	LDA #85
	CMP player0y
     BCS .skip74then
.condpart75
 jsr .room3
	LDA #10
	STA player0y
.skip74then
.skipL071
.
 ; 

.L072 ;  if room  =  4  &&  player0x  >  145 then gosub room6  :  player0x  =  22

	LDA room
	CMP #4
     BNE .skipL072
.condpart76
	LDA #145
	CMP player0x
     BCS .skip76then
.condpart77
 jsr .room6
	LDA #22
	STA player0x
.skip76then
.skipL072
.L073 ;  if room  =  6  &&  player0x  <  5 then gosub room4  :  player0x  =  140

	LDA room
	CMP #6
     BNE .skipL073
.condpart78
	LDA player0x
	CMP #5
     BCS .skip78then
.condpart79
 jsr .room4
	LDA #140
	STA player0x
.skip78then
.skipL073
.
 ; 

.L074 ;  if room  =  6  &&  player0x  >  145 then gosub room7  :  player0x  =  22

	LDA room
	CMP #6
     BNE .skipL074
.condpart80
	LDA #145
	CMP player0x
     BCS .skip80then
.condpart81
 jsr .room7
	LDA #22
	STA player0x
.skip80then
.skipL074
.L075 ;  if room  =  7  &&  player0x  <  5 then gosub room6  :  player0x  =  140

	LDA room
	CMP #7
     BNE .skipL075
.condpart82
	LDA player0x
	CMP #5
     BCS .skip82then
.condpart83
 jsr .room6
	LDA #140
	STA player0x
.skip82then
.skipL075
.
 ; 

.L076 ;  if room  =  7  &&  player0x  >  145 then gosub room9  :  player0x  =  22

	LDA room
	CMP #7
     BNE .skipL076
.condpart84
	LDA #145
	CMP player0x
     BCS .skip84then
.condpart85
 jsr .room9
	LDA #22
	STA player0x
.skip84then
.skipL076
.L077 ;  if room  =  9  &&  player0x  <  5 then gosub room7  :  player0x  =  140

	LDA room
	CMP #9
     BNE .skipL077
.condpart86
	LDA player0x
	CMP #5
     BCS .skip86then
.condpart87
 jsr .room7
	LDA #140
	STA player0x
.skip86then
.skipL077
.
 ; 

.L078 ;  if room  =  8  &&  player0y  <  5 then gosub room7  :  player0y  =  80

	LDA room
	CMP #8
     BNE .skipL078
.condpart88
	LDA player0y
	CMP #5
     BCS .skip88then
.condpart89
 jsr .room7
	LDA #80
	STA player0y
.skip88then
.skipL078
.L079 ;  if room  =  7  &&  player0y  >  85 then gosub room8  :  player0y  =  10

	LDA room
	CMP #7
     BNE .skipL079
.condpart90
	LDA #85
	CMP player0y
     BCS .skip90then
.condpart91
 jsr .room8
	LDA #10
	STA player0y
.skip90then
.skipL079
.
 ; 

.L080 ;  drawscreen

 jsr drawscreen
.L081 ;  goto mainloop

 jmp .mainloop

.
 ; 

.moverizquierda
 ; moverizquierda

.L082 ;  player0:

	LDA #<playerL082_0

	STA player0pointerlo
	LDA #>playerL082_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L083 ;  if haslance  =  1 then compass  =  4

	LDA haslance
	CMP #1
     BNE .skipL083
.condpart92
	LDA #4
	STA compass
.skipL083
.L084 ;  if collision(playfield,player0) then player0x  =  player0x  +  1  :  noright  =  0  :  noleft  =  1  :  noup  =  0  :  nodown  =  0 else player0x  =  player0x  -  1  :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL084
.condpart93
	INC player0x
	LDA #0
	STA noright
	LDA #1
	STA noleft
	LDA #0
	STA noup
	STA nodown
 jmp .skipelse0
.skipL084
	DEC player0x
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse0
.L085 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL085
.condpart94
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL085
.L086 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL086
.condpart95
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL086
.L087 ;  return

	RTS
.
 ; 

.moverderecha
 ; moverderecha

.L088 ;  player0:

	LDA #<playerL088_0

	STA player0pointerlo
	LDA #>playerL088_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L089 ;  if haslance  =  1 then compass  =  2

	LDA haslance
	CMP #1
     BNE .skipL089
.condpart96
	LDA #2
	STA compass
.skipL089
.L090 ;  if collision(playfield,player0) then player0x  =  player0x  -  1  :  noright  =  1  :  noleft  =  0  :  noup  =  0  :  nodown  =  0 else player0x  =  player0x  +  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL090
.condpart97
	DEC player0x
	LDA #1
	STA noright
	LDA #0
	STA noleft
	STA noup
	STA nodown
 jmp .skipelse1
.skipL090
	INC player0x
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse1
.L091 ;  if haslance  =  1 then missile0x  =  player0x  +  9  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL091
.condpart98
	LDA player0x
	CLC
	ADC #9
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL091
.L092 ;  if hasshield  =  1 then ballx  =  player0x  -  1  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL092
.condpart99
	LDA player0x
	SEC
	SBC #1
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL092
.L093 ;  return

	RTS
.
 ; 

.moverarriba
 ; moverarriba

.L094 ;  player0:

	LDA #<playerL094_0

	STA player0pointerlo
	LDA #>playerL094_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L095 ;  if haslance  =  1 then compass  =  1

	LDA haslance
	CMP #1
     BNE .skipL095
.condpart100
	LDA #1
	STA compass
.skipL095
.L096 ;  if collision(playfield,player0) then player0y  =  player0y  +  1  :  noright  =  0  :  noleft  =  0  :  noup  =  1  :  nodown  =  0 else player0y  =  player0y  -  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL096
.condpart101
	INC player0y
	LDA #0
	STA noright
	STA noleft
	LDA #1
	STA noup
	LDA #0
	STA nodown
 jmp .skipelse2
.skipL096
	DEC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse2
.L097 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL097
.condpart102
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL097
.L098 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL098
.condpart103
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL098
.L099 ;  return

	RTS
.
 ; 

.moverabajo
 ; moverabajo

.L0100 ;  player0:

	LDA #<playerL0100_0

	STA player0pointerlo
	LDA #>playerL0100_0

	STA player0pointerhi
	LDA #9
	STA player0height
.
 ; 

.L0101 ;  if haslance  =  1 then compass  =  3

	LDA haslance
	CMP #1
     BNE .skipL0101
.condpart104
	LDA #3
	STA compass
.skipL0101
.L0102 ;  if collision(playfield,player0) then player0y  =  player0y  -  1  :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  1 else player0y  =  player0y  +  1 :  noright  =  0  :  noleft  =  0  :  noup  =  0  :  nodown  =  0

	BIT CXP0FB
	BPL .skipL0102
.condpart105
	DEC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	LDA #1
	STA nodown
 jmp .skipelse3
.skipL0102
	INC player0y
	LDA #0
	STA noright
	STA noleft
	STA noup
	STA nodown
.skipelse3
.L0103 ;  if haslance  =  1 then missile0x  =  player0x  :  missile0y  =  player0y  -  2

	LDA haslance
	CMP #1
     BNE .skipL0103
.condpart106
	LDA player0x
	STA missile0x
	LDA player0y
	SEC
	SBC #2
	STA missile0y
.skipL0103
.L0104 ;  if hasshield  =  1 then ballx  =  player0x  +  7  :  bally  =  player0y  -  3

	LDA hasshield
	CMP #1
     BNE .skipL0104
.condpart107
	LDA player0x
	CLC
	ADC #7
	STA ballx
	LDA player0y
	SEC
	SBC #3
	STA bally
.skipL0104
.L0105 ;  return

	RTS
.
 ; 

.colocarmoneda
 ; colocarmoneda

.L0106 ;  if hascoin  =  1 then player1x  =  0  :  player1y  =  0

	LDA hascoin
	CMP #1
     BNE .skipL0106
.condpart108
	LDA #0
	STA player1x
	STA player1y
.skipL0106
.
 ; 

.L0107 ;  randnumber  =  rand

 jsr randomize
	STA randnumber
.L0108 ;  if hascoin  =  0  &&  randnumber  <=  153 then coinvalue  =  1

	LDA hascoin
	CMP #0
     BNE .skipL0108
.condpart109
	LDA #153
	CMP randnumber
     BCC .skip109then
.condpart110
	LDA #1
	STA coinvalue
.skip109then
.skipL0108
.L0109 ;  if hascoin  =  0  &&  randnumber  >  153  &&  randnumber  <=  204 then coinvalue  =  5

	LDA hascoin
	CMP #0
     BNE .skipL0109
.condpart111
	LDA #153
	CMP randnumber
     BCS .skip111then
.condpart112
	LDA #204
	CMP randnumber
     BCC .skip112then
.condpart113
	LDA #5
	STA coinvalue
.skip112then
.skip111then
.skipL0109
.L0110 ;  if hascoin  =  0  &&  randnumber  >  204  &&  randnumber  <=  255 then coinvalue  =  32

	LDA hascoin
	CMP #0
     BNE .skipL0110
.condpart114
	LDA #204
	CMP randnumber
     BCS .skip114then
.condpart115
	LDA #255
	CMP randnumber
     BCC .skip115then
.condpart116
	LDA #32
	STA coinvalue
.skip115then
.skip114then
.skipL0110
.
 ; 

.L0111 ;  randnumber  =  rand

 jsr randomize
	STA randnumber
.L0112 ;  if hascoin  =  0  &&  randnumber  <=  51 then player1x  =  30  :  player1y  =  20

	LDA hascoin
	CMP #0
     BNE .skipL0112
.condpart117
	LDA #51
	CMP randnumber
     BCC .skip117then
.condpart118
	LDA #30
	STA player1x
	LDA #20
	STA player1y
.skip117then
.skipL0112
.L0113 ;  if hascoin  =  0  &&  randnumber  >  51  &&  randnumber  <=  102 then player1x  =  120  :  player1y  =  20

	LDA hascoin
	CMP #0
     BNE .skipL0113
.condpart119
	LDA #51
	CMP randnumber
     BCS .skip119then
.condpart120
	LDA #102
	CMP randnumber
     BCC .skip120then
.condpart121
	LDA #120
	STA player1x
	LDA #20
	STA player1y
.skip120then
.skip119then
.skipL0113
.L0114 ;  if hascoin  =  0  &&  randnumber  >  102  &&  randnumber  <=  153 then player1x  =  30  :  player1y  =  75

	LDA hascoin
	CMP #0
     BNE .skipL0114
.condpart122
	LDA #102
	CMP randnumber
     BCS .skip122then
.condpart123
	LDA #153
	CMP randnumber
     BCC .skip123then
.condpart124
	LDA #30
	STA player1x
	LDA #75
	STA player1y
.skip123then
.skip122then
.skipL0114
.L0115 ;  if hascoin  =  0  &&  randnumber  >  153  &&  randnumber  <=  204 then player1x  =  120  :  player1y  =  75

	LDA hascoin
	CMP #0
     BNE .skipL0115
.condpart125
	LDA #153
	CMP randnumber
     BCS .skip125then
.condpart126
	LDA #204
	CMP randnumber
     BCC .skip126then
.condpart127
	LDA #120
	STA player1x
	LDA #75
	STA player1y
.skip126then
.skip125then
.skipL0115
.L0116 ;  if hascoin  =  0  &&  randnumber  >  204  &&  randnumber  <=  255 then player1x  =  77  :  player1y  =  45

	LDA hascoin
	CMP #0
     BNE .skipL0116
.condpart128
	LDA #204
	CMP randnumber
     BCS .skip128then
.condpart129
	LDA #255
	CMP randnumber
     BCC .skip129then
.condpart130
	LDA #77
	STA player1x
	LDA #45
	STA player1y
.skip129then
.skip128then
.skipL0116
.L0117 ;  return

	RTS
.
 ; 

.moverlanza
 ; moverlanza

.L0118 ;  if compass  =  1 then NUSIZ0  =  $00  :  missile0height  =  8  :  missile0y  =  missile0y  -  2

	LDA compass
	CMP #1
     BNE .skipL0118
.condpart131
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
	LDA missile0y
	SEC
	SBC #2
	STA missile0y
.skipL0118
.L0119 ;  if compass  =  2 then NUSIZ0  =  $30  :  missile0height  =  0  :  missile0x  =  missile0x  +  2

	LDA compass
	CMP #2
     BNE .skipL0119
.condpart132
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
	LDA missile0x
	CLC
	ADC #2
	STA missile0x
.skipL0119
.L0120 ;  if compass  =  3 then NUSIZ0  =  $00  :  missile0height  =  8  :  missile0y  =  missile0y  +  2

	LDA compass
	CMP #3
     BNE .skipL0120
.condpart133
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
	LDA missile0y
	CLC
	ADC #2
	STA missile0y
.skipL0120
.L0121 ;  if compass  =  4 then NUSIZ0  =  $30  :  missile0height  =  0  :  missile0x  =  missile0x  -  2

	LDA compass
	CMP #4
     BNE .skipL0121
.condpart134
	LDA #$30
	STA NUSIZ0
	LDA #0
	STA missile0height
	LDA missile0x
	SEC
	SBC #2
	STA missile0x
.skipL0121
.L0122 ;  return

	RTS
.
 ; 

.room1
 ; room1

.L0123 ;  room  =  1

	LDA #1
	STA room
.L0124 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0125 ;  pfclear

	LDA #0
 jsr pfclear
.L0126 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.L0127 ;  playfield:

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
.L0128 ;  if haslance  =  1 then gosub colocarmoneda else hascoin  =  1  :  gosub colocarmoneda

	LDA haslance
	CMP #1
     BNE .skipL0128
.condpart135
 jsr .colocarmoneda
 jmp .skipelse4
.skipL0128
	LDA #1
	STA hascoin
 jsr .colocarmoneda

.skipelse4
.L0129 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0129
.condpart136
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0129
.L0130 ;  drawscreen

 jsr drawscreen
.L0131 ;  return

	RTS
.
 ; 

.room2
 ; room2

.L0132 ;  room  =  2

	LDA #2
	STA room
.L0133 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0134 ;  pfclear

	LDA #0
 jsr pfclear
.L0135 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.L0136 ;  playfield:

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
.L0137 ;  if hasshield  =  1 then gosub colocarmoneda else hascoin  =  1  :  gosub colocarmoneda

	LDA hasshield
	CMP #1
     BNE .skipL0137
.condpart137
 jsr .colocarmoneda
 jmp .skipelse5
.skipL0137
	LDA #1
	STA hascoin
 jsr .colocarmoneda

.skipelse5
.L0138 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0138
.condpart138
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0138
.L0139 ;  drawscreen

 jsr drawscreen
.L0140 ;  return

	RTS
.
 ; 

.room3
 ; room3

.L0141 ;  room  =  3

	LDA #3
	STA room
.L0142 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0143 ;  pfclear

	LDA #0
 jsr pfclear
.L0144 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.L0145 ;  playfield:

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
.L0146 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0147 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0147
.condpart139
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0147
.L0148 ;  drawscreen

 jsr drawscreen
.L0149 ;  return

	RTS
.
 ; 

.room4
 ; room4

.L0150 ;  room  =  4

	LDA #4
	STA room
.L0151 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0152 ;  pfclear

	LDA #0
 jsr pfclear
.L0153 ;  COLUPF  =  $C0

	LDA #$C0
	STA COLUPF
.L0154 ;  playfield:

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
.L0155 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0156 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0156
.condpart140
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0156
.L0157 ;  drawscreen

 jsr drawscreen
.L0158 ;  return

	RTS
.
 ; 

.room5
 ; room5

.L0159 ;  room  =  5

	LDA #5
	STA room
.L0160 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0161 ;  pfclear

	LDA #0
 jsr pfclear
.L0162 ;  COLUPF  =  $40

	LDA #$40
	STA COLUPF
.L0163 ;  playfield:

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
.L0164 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0165 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0165
.condpart141
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0165
.L0166 ;  drawscreen

 jsr drawscreen
.L0167 ;  return

	RTS
.
 ; 

.room6
 ; room6

.L0168 ;  room  =  6

	LDA #6
	STA room
.L0169 ;  hascoin  =  1

	LDA #1
	STA hascoin
.L0170 ;  pfclear

	LDA #0
 jsr pfclear
.L0171 ;  COLUPF  =  $A0

	LDA #$A0
	STA COLUPF
.L0172 ;  playfield:

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
.L0173 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0174 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0174
.condpart142
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0174
.L0175 ;  drawscreen

 jsr drawscreen
.L0176 ;  return

	RTS
.
 ; 

.room7
 ; room7

.L0177 ;  room  =  7

	LDA #7
	STA room
.L0178 ;  hascoin  =  1

	LDA #1
	STA hascoin
.L0179 ;  pfclear

	LDA #0
 jsr pfclear
.L0180 ;  COLUPF  =  $A0

	LDA #$A0
	STA COLUPF
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

.L0183 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0183
.condpart143
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0183
.L0184 ;  drawscreen

 jsr drawscreen
.L0185 ;  return

	RTS
.
 ; 

.room8
 ; room8

.L0186 ;  room  =  8

	LDA #8
	STA room
.L0187 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0188 ;  pfclear

	LDA #0
 jsr pfclear
.L0189 ;  COLUPF  =  $A0

	LDA #$A0
	STA COLUPF
.L0190 ;  playfield:

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
	.byte %10000000, %11111111, %11111100, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel7
	lda PF_data7,x
	sta playfield,x
	dex
	bpl pflabel7
.L0191 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0192 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0192
.condpart144
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0192
.L0193 ;  drawscreen

 jsr drawscreen
.L0194 ;  return

	RTS
.
 ; 

.room9
 ; room9

.L0195 ;  room  =  9

	LDA #9
	STA room
.L0196 ;  hascoin  =  0

	LDA #0
	STA hascoin
.L0197 ;  pfclear

	LDA #0
 jsr pfclear
.L0198 ;  COLUPF  =  $A0

	LDA #$A0
	STA COLUPF
.L0199 ;  playfield:

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
	.byte %10000000, %00100000, %00010000, %10000000
	.byte %10000000, %00100000, %00010000, %10000000
	.byte %10000000, %00100000, %00010000, %10000000
	.byte %10000000, %11100000, %11110000, %10000000
	.byte %00000000, %00000000, %00000000, %10000000
	.byte %00000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel8
	lda PF_data8,x
	sta playfield,x
	dex
	bpl pflabel8
.L0200 ;  gosub colocarmoneda

 jsr .colocarmoneda

.L0201 ;  if haslance  =  3 then haslance  =  0  :  NUSIZ0  =  $00  :  missile0height  =  8

	LDA haslance
	CMP #3
     BNE .skipL0201
.condpart145
	LDA #0
	STA haslance
	LDA #$00
	STA NUSIZ0
	LDA #8
	STA missile0height
.skipL0201
.L0202 ;  drawscreen

 jsr drawscreen
.L0203 ;  return

	RTS
 if (<*) > (<(*+5))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL00_1

	.byte 0
	.byte  %0110
	.byte  %1111
	.byte  %1111
	.byte  %0110
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL082_0

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
playerL088_0

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
playerL094_0

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
playerL0100_0

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
