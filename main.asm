*         = $6000

; Screen memory, unexpanded VIC
SCR       = $1E00

; Colour RAM
CRAM      = $9600

; Horizontal centering (bit 7 sets interlace)
HCREG     = $9000

; Number of columns (bit 7 is part of video matrix address)
SCOLREG   = $9002

; Number of rows (bit 0 sets 8x8 or 16x8 chars)
SROWREG   = $9003

; Start of character memory (bit4-7 is rest of video address, normally F)
CMEMREG   = $9005

; Character memory
CHMEMC    = $FD
CHMEM     = $1400


          ; We initialize the screen to 16x10 characters of 8x16 pixels each.
          ; This will give a screen resolution of 128x160 pixels.
          ; The characters are filled in row-first.
          ; In the character memory, this means we will adress each pixel
          ; (x,y) as
          ; bit = x & 0x07
          ; adr = CHMEM + x & 0xf8 + y
          ; Preserve registers
GINIT     pha
          txa
          pha
          tya
          pha
          ; Fill the screen memory
          ldx            #$00      
          txa
          ; A is column index. Save to $01
L7008     sta            $01
          ; Copy column index to Y as "first address of column"
          tay
          ; Inner loop. X is incremented for each new cell
L700b     txa
          ; Store X to the screen memory indexed by Y
          sta            SCR,y     
          ; Increment Y by 16, one column. So, next row
          tya
          clc
          adc            #$10      
          tay
          ; When we've passed #$a0, we've done the last row
          cmp            #$a0      
          bcs            L701b     
          ; Increase X and repeat the inner loop
          inx
          bne            L700b     
          ; Outer loop. Load stored A and increment it
L701b     lda            $01
          clc
          adc            #$01      
          ; This could be improved by replacing both inx with a single one
          inx
          ; Unless we've passed column 16, repeat
          cmp            #$10      
          bcc            L7008     
          ; Initialize color RAM at $9600, 16x10 = 0xa0, but offset by one
          ldy            #$a1      
          ; The color was stored at $00 before calling $7000
          lda            $00       
L7029     sta            CRAM-1,y
          dey
          bne            L7029     
          ; Adjust horizontal centering
          lda            #$12      
          sta            HCREG    
          ; Set 16 columns. Bit 7 part of video matrix address
          lda            #$90      
          sta            SCOLREG      
          ; 00010101 = 16x8 chars, 10 rows
          lda            #$15      
          sta            SROWREG      
          ; Screen (char buffer) at $1400, f for rest of video address
          lda            #CHMEMC      
          sta            CMEMREG
          ; Pop everything and return
          pla
          tay
          pla
          tax
          pla
          rts

          ; We have 16x10 characters of 16x8 pixels, which makes for a
          ; a character generator containing 16*10*16 bytes = 2560 bytes.
          ; The inner loop handles the low byte of the address and the outer
          ; loop handles the high byte, but we stop after $0a runs of the 
          ; outer loop, because 2560 / 256 = 10

          ; Note that the code as it stands takes advantage of the fact that
          ; the low byte of the address is zero, and doesn't reload A with
          ; the initialization value.
                                
GCLRSCR                            ; Preserve registers
          pha
          txa
          pha
          tya
          pha
          lda            #>CHMEM      
          sta            $01       
          lda            #<CHMEM      
          sta            $00       
          ldy            #$00      
          ldx            #$0a      
L705a     sta            ($00),y
          inc            $00       
          bne            L705a     
          inc            $01       
          dex
          bne            L705a     
          pla
          tay
          pla
          tax
          pla
          rts

                                   ; For each row, rotate the entire row left
GSCRLLEFT pha
          txa
          pha
          tya
          pha
          ldx            $01       
                                   ; Go from right to left and rotate each octet through carry
L7072     clc
          rol            $1d5f,x   
          rol            $1cbf,x   
          rol            $1c1f,x   
          rol            $1b7f,x   
          rol            $1adf,x   
          rol            $1a3f,x   
          rol            $199f,x   
          rol            $18ff,x   
          rol            $185f,x   
          rol            $17bf,x   
          rol            $171f,x   
          rol            $167f,x   
          rol            $15df,x   
          rol            $153f,x   
          rol            $149f,x   
          rol            $13ff,x   
          rol            $13ff     
                                   ; Finally, copy the remainder over to the right end of the row
          lda            $13ff     
          and            #$01      
          ora            $1d5f,x   
          sta            $1d5f,x   
          dex
          cpx            $00       
          bne            L7072     
          pla
          tay
          pla
          tax
          pla
          rts

                                   ; For each row, rotate the entire row right
GSCRLRGHT pha
          txa
          pha
          tya
          pha
          ldx            $01       
                                   ; Go from left to right and rotate each octet through carry
L70c3     clc
          ror            $13ff,x   
          ror            $149f,x   
          ror            $153f,x   
          ror            $15df,x   
          ror            $167f,x   
          ror            $171f,x   
          ror            $17bf,x   
          ror            $185f,x   
          ror            $18ff,x   
          ror            $199f,x   
          ror            $1a3f,x   
          ror            $1adf,x   
          ror            $1b7f,x   
          ror            $1c1f,x   
          ror            $1cbf,x   
          ror            $1d5f,x   
          ror            $13ff     
                                   ; Finally, copy the remainder over to the left end of the row
          lda            $13ff     
          and            #$80      
          ora            $13ff,x   
          sta            $13ff,x   
          dex
          cpx            $00       
          bne            L70c3     
          pla
          tay
          pla
          tax
          pla
          rts

GSCRLDWN  pha
          txa
          pha
          tya
          pha
          ldx            #$00      
          stx            $00       
          dex
          stx            $02       
          ldx            #$14      
          stx            $01       
          dex
          stx            $03       
L7120     ldy            #$00
          lda            ($00),y   
          sta            ($02),y   
          clc
          lda            $02       
          adc            #$01      
          sta            $02       
          lda            $03       
          adc            #$00      
          sta            $03       
          clc
          lda            $00       
          adc            #$01      
          sta            $00       
          lda            $01       
          adc            #$00      
          sta            $01       
          cmp            #>SCR     
          bne            L7120     
          ldx            #$ff      
          stx            $00       
          ldx            #$1d      
          stx            $01       
          stx            $03       
          ldx            #$5f      
          stx            $02       
L7152     ldy            #$00
          lda            ($02),y   
          sta            ($00),y   
          sec
          lda            $00       
          sbc            #$a0      
          sta            $00       
          lda            $01       
          sbc            #$00      
          sta            $01       
          sec
          lda            $02       
          sbc            #$a0      
          sta            $02       
          lda            $03       
          sbc            #$00      
          sta            $03       
          lda            $01       
          cmp            #$13      
          bne            L7152     
          pla
          tay
          pla
          tax
          pla
          rts

GSCRLUP   pha
          txa
          pha
          tya
          pha
          ldx            #$fe      
          stx            $00       
          inx
          stx            $02       
          ldx            #$1d      
          stx            $01       
          stx            $03       
          lda            $1dff     
          sta            $13ff     
L7196     ldy            #$00
          lda            ($00),y   
          sta            ($02),y   
          sec
          lda            $02       
          sbc            #$01      
          sta            $02       
          lda            $03       
          sbc            #$00      
          sta            $03       
          sec
          lda            $00       
          sbc            #$01      
          sta            $00       
          lda            $01       
          sbc            #$00      
          sta            $01       
          cmp            #$13      
          bne            L7196     
          ldx            #$00      
          stx            $00       
          ldx            #$14      
          stx            $01       
          stx            $03       
          ldx            #$a0      
          stx            $02       
L71c8     ldy            #$00
          lda            ($02),y   
          sta            ($00),y   
          clc
          lda            $00       
          adc            #$a0      
          sta            $00       
          lda            $01       
          adc            #$00      
          sta            $01       
          clc
          lda            $02       
          adc            #$a0      
          sta            $02       
          lda            $03       
          adc            #$00      
          sta            $03       
          cmp            #>SCR     
          bne            L71c8     
          lda            $13ff     
          sta            $1d60     
          pla
          tay
          pla
          tax
          pla
          rts

L71f8     pha
          txa
          pha
          tya
          pha
          lda            $00       
          ldy            $01       
          sty            $00       
                                   ; a=$00, moved $01 to $00
          tay
          and            #$07      
          tax
                                   ; y=old $00, x=old $00 & 0x07
          lda            #$00      
                                   ; Zero out $01
          sta            $01       
          tya
          lsr            a         
          lsr            a         
          lsr            a         
                                   ; Y=A=Y/8 (shifted right thrice)
          tay
          ror            a         
          ror            a         
          and            #$80      
                                   ; a now has only the old bit 5 in the bit 7 position
          clc
                                   ; We add this with carry to $00,$01
          adc            $00       
          sta            $00       
          lda            #$00      
          adc            $01       
          sta            $01       
          clc
          tya
          ror            a         
          ror            a         
          ror            a         
          ror            a         
          and            #$e0      
          adc            $00       
          sta            $00       
          lda            #$00      
          adc            $01       
          sta            $01       
          tya
          lsr            a         
          lsr            a         
          lsr            a         
          clc
          adc            $01       
          sta            $01       
          tya
          lsr            a         
          clc
          adc            $01       
          sta            $01       
          lda            #$14      
          adc            $01       
          sta            $01       
          stx            $02       
          sec
          lda            #$00      
          ldx            #$ff      
L724e     inx
          ror            a         
          cpx            $02       
          bne            L724e     
          sta            $02       
          tya
          cmp            #$08      
          bmi            L7268     
          sec
          lda            $00       
          sbc            #$01      
          sta            $00       
          lda            $01       
          sbc            #$00      
          sta            $01       
L7268     pla
          tay
          pla
          tax
          pla
          rts

GDRAW     pha
          tya
          pha
          txa
          pha
          jsr            L71f8     
          ldy            #$00      
          lda            ($00),y   
          ora            $02       
          sta            ($00),y   
          pla
          tax
          pla
          tay
          pla
          rts

GERASE    pha
          tya
          pha
          txa
          pha
          jsr            L71f8     
          ldy            $02       
          eor            #$ff      
          ldy            #$00      
          and            ($00),y   
          sta            ($00),y   
          pla
          tax
          pla
          tay
          pla
          rts

L729c     lda            #$02
          sta            $00       
          jsr            GINIT     
                                   ; Set DDRB on VIA2 to all out except pin7/joy3
          lda            #$7f      
          sta            $9122     
          jsr            GCLRSCR   
                                   ; Read VIA2 PORTA
DRAWLOOP  lda            $9111
          tay
                                   ; Joy 0
          and            #$04      
          cmp            #$00      
          bne            L72b8     
          jsr            GSCRLUP   
L72b8     tya
                                   ; Joy 1
          and            #$08      
          cmp            #$00      
          bne            L72c2     
          jsr            GSCRLDWN  
L72c2     tya
                                   ; Joy 2
          and            #$10      
          cmp            #$00      
          bne            L72d4     
          lda            #$00      
          sta            $00       
          lda            #$a0      
          sta            $01       
          jsr            GSCRLRGHT 
                                   ; Read VIA2 PORTB
L72d4     lda            $9120
                                   ; Joy 3
          and            #$80      
          cmp            #$00      
          bne            L72e8     
          lda            #$00      
          sta            $00       
          lda            #$a0      
          sta            $01       
          jsr            GSCRLLEFT 
L72e8     lda            #$3f
          sta            $00       
          lda            #$4f      
          sta            $01       
          tya
                                   ; Check fire button, from VIA2 PORTA
          and            #$20      
          cmp            #$00      
          beq            L72fd     
          jsr            GDRAW     
          jmp            DRAWLOOP  

L72fd     jsr            GERASE
          jmp            DRAWLOOP

          ; Address and bitmask from [X,Y] coordinates
          ; [x,y] = [X,Y]
          ; Result: [a,y] = address, x=bitmask
XLATE     clc
          txa
          pha
          tya
          pha
          ; Initialize [x,y] as a 16-bit version of X & 0xF8
          txa
          and #$f8
          tax
          lda #$00
          tay
          ; Now rotate [x,y] twice to yield (X & 0xF8) * 0x04
          txa
          rol
          tax
          tya
          rol
          tay
          txa
          rol
          tax
          tya
          rol
          tay
          ; Store the temporary sum at [$00, $01]
          sta $01
          txa
          sta $00
          ; Rotate [x,y] twice again to yield (X & 0xF8) * 0x10
          rol
          tax
          tya
          rol
          tay
          txa
          rol
          tax
          tya
          rol
          tay
          ; Add the temporary sum, to yield (X & 0xF8) * 0x14
          txa
          adc $00
          sta $00
          tya
          adc $01
          sta $01
          ; Now pop Y from the stack and add that to the address,
          ; then add the base CHMEM address to that.
          pla
          adc $00
          sta $00
          lda $01
          adc #$00
          sta $01
          lda #<CHMEM
          adc $00
          sta $00
          lda #>CHMEM
          adc $01
          sta $01
          ; Pop the original X and generate the bitmask from bits 0-3
          ; Remember, we need to count from MSB to LSB here
          pla
          and #$07
          tax
          lda #$00
          sec
@XLLOOP   ror
          dex
          bpl @XLLOOP
          tax
          lda $00
          tay
          lda $01
          rts

          ; plot a pixel, either set it or reset it
          ; [x,y] = coordinates, a = reset?
PLOT      ; push the reset? bool
          pha
          ; translate the coordinates into an address and bitmask
          jsr XLATE
          sta $01
          tya
          sta $00
          txa
          sta $02
          ; now pop reset? and decide whether to set or clear the pixel
          pla
          beq @PLOT2
          lda #$FF
          eor $02
          sta $02
          ldy #$00
          lda ($00),y
          and $02
          sta ($00),y
          rts
@PLOT2    ldy #$00
          lda ($00),y
          ora $02
          sta ($00),y
          rts

GTEST     lda #$02
          sta $00       
          jsr GINIT     
          jsr GCLRSCR
          ldy #$0a
@GTLP2    tya
          pha
          ldx #08
@GTLOOP   txa
          pha
          tya
          pha
          lda #$00
          jsr PLOT
          pla
          tay
          pla
          tax
          iny
          inx
          txa
          cmp #$60
          bne @GTLOOP
          pla
          tay
          iny
          iny
          tya
          cmp #$2a
          bne @GTLP2
@GTSTL    jmp @GTSTL
