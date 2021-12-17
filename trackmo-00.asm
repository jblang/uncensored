EndOfRaster = $8000
ScriptPtr = $04
CursorAddr = $D3
LoadAndDecrunch = $0200
Page2000Routine = $2004
Page8000Routine = $8004

        * = $0900

; -----------------------------------------------------------------------------
; setup for intro sequence

-       BIT $D011               ; wait for vblank
        BPL -
-       BIT $D011
        BMI -
        SEI 
        LDA #$7F                ; disable all cia interrupts
        STA $DC0D
        STA $DD0D
        LDA #$01                ; disable all vic interrupts except raster
        STA $D01A
        LDA #<DefaultRaster     ; set default raster routine
        STA RasterTblLo
        LDA #>DefaultRaster
        STA RasterTblHi
        LDA #$01
        STA RasterShift
        LDA #$00                ; raster interrupt on line 0
        STA $D012
        LDA $D011
        AND #$7F
        STA $D011
        LDA #<IntHandler        ; setup interrupt handler
        STA $FFFE
        LDA #>IntHandler
        STA $FFFF
        LDA $DC0D               ; clear interrupt flags
        LDA $DD0D
        INC $D019
        LDA #$00                ; clear counter
        STA Counter
        STA Counter+1
        LDX #<GlitchScript      ; introduce glitches on the screen
        LDY #>GlitchScript
        JSR SetScript
        CLI 
        JMP Loop

; -----------------------------------------------------------------------------

LoadNext 
        LDA NextChain           ; request the next chain from disk
        INC NextChain
        JMP LoadAndDecrunch
NextChain 
        .BYTE $01               ; index of next chain to load from disk

; -----------------------------------------------------------------------------

IncCounter 
        INC Counter             ; increment interrupt counter
        BNE +
        INC Counter+1
NextRoutine   =*+$01
+       JSR NoOp                ; execute next configured interrupt routine
SetNextRoutine 
        LDY #$00                ; get next routine from script and set jmp destination
        LDA (ScriptPtr),Y
        TAX 
        INY 
        LDA (ScriptPtr),Y
        STX NextRoutine
        STA NextRoutine+1
IncScriptPtr2 
        LDA #$02                ; increment script pointer by 2 (for addresses)
IncScriptPtrA                   ; increment script pointer by contents of A
        CLC 
IncScriptPtrAC                  ; Increment script pointer by contents of A+C
        ADC ScriptPtr
        STA ScriptPtr
        BCC NoOp
        INC ScriptPtr+1
NoOp    
        RTS 

Counter 
        .WORD $0000

; -----------------------------------------------------------------------------

LoopIndex   =*+$01
Loop    
        LDA #$00
LoopCount   =*+$01
        CMP #$00
        BEQ Loop                ; busy wait if loop routines are empty
        ASL 
        TAX 
        LDA LoopRoutines,X      ; load next loop routine and jump to it
        STA LoopRoutine
        LDA LoopRoutines+1,X
        STA LoopRoutine+1
LoopRoutine   =*+$01
        JSR NoOp
        LDA LoopIndex           ; increment loop routine index (max of 4 loop routines)
        CLC 
        ADC #$01
        AND #$03
        STA LoopIndex
        JMP Loop

AddLoopRoutine 
        LDA LoopCount           ; add another routine to execute during loop
        PHA 
        ASL 
        TAX 
        LDY #$00
        LDA (ScriptPtr),Y
        STA LoopRoutines,X
        INY 
        LDA (ScriptPtr),Y
        STA LoopRoutines+1,X
        PLA 
        CLC 
        ADC #$01                ; update loop routine count
        AND #$03
        STA LoopCount
        JMP IncScriptPtr2

LoopRoutines 
        .WORD $0988,$0988,$0988,$0988

; -----------------------------------------------------------------------------

SetScript 
        STX ScriptPtr           ; update script pointer to address in X and Y
        STY ScriptPtr+1
        JMP SetNextRoutine

        LDY #$00                ; dead code?
        LDA (ScriptPtr),Y
        INC ScriptPtr
        BNE +
        INC ScriptPtr+1
+       RTS 

SetAndDoNextRoutine             ; set next routine and jump to it
        JSR SetNextRoutine
        JMP (NextRoutine)

; -----------------------------------------------------------------------------

SetStackPops                    ; set number of stack frames to pop
        LDY #$00
        LDA (ScriptPtr),Y
        STA StackPops
        INY 
        LDA (ScriptPtr),Y
        STA StackPops+1
        JMP IncScriptPtr2

StackPops 
        .WORD $0000

PopStack 
        INC StackPops           ; pop routine from return stack, unless we have
        BNE +                   ; already reached the configured limit
        INC StackPops+1
        BEQ ++
+       PLA 
        PLA 
+       RTS 

; -----------------------------------------------------------------------------

SetNewScript                    ; get the address of the new script
        LDY #$00
        LDA (ScriptPtr),Y
        TAX 
        INY 
        LDA (ScriptPtr),Y
        STA ScriptPtr+1         ; and store it in the script pointer
        STX ScriptPtr
        RTS 

GotoScript 
        JSR SetNewScript        ; set a new script and jump there
        JMP SetAndDoNextRoutine

SetSubScript 
        LDY #$00
        LDA (ScriptPtr),Y
        STA NewScriptPtrLo
        INY 
        LDA (ScriptPtr),Y
        STA NewScriptPtrHi
        LDX ScriptStackPtr      ; save location in current script on script stack
        LDA ScriptPtr
        CLC 
        ADC #$02
        STA ScriptStack,X
        LDA ScriptPtr+1
        ADC #$00
        STA ScriptStack+1,X
        INX 
        INX 
        STX ScriptStackPtr
NewScriptPtrHi   =*+$01
        LDA #$00                ; update script pointer with location of new script
        STA ScriptPtr+1
NewScriptPtrLo   =*+$01
        LDA #$00
        STA ScriptPtr
        RTS 

GosubScript 
        JSR SetSubScript
        JMP SetAndDoNextRoutine

GosubScriptIndirect             ; run a subscript whose address is at the given 
        JSR SetSubScript
        JMP GotoScript

ScriptReturn                    ; continue executing the previous script
        LDX ScriptStackPtr
        DEX 
        DEX 
        STX ScriptStackPtr
        LDA ScriptStack,X
        STA ScriptPtr
        LDA ScriptStack+1,X
        STA ScriptPtr+1
        STX ScriptStackPtr
        JMP SetAndDoNextRoutine

ScriptStackPtr 
        .BYTE $00
ScriptStack 
        .WORD $0000,$0000,$0000,$0000

; -----------------------------------------------------------------------------

SetExitParam 
        LDY #$00                ; set up an exit check routine with next byte passed as parameter in A
        LDA (ScriptPtr),Y
        STA ExitParam
        INY 
        .BYTE $2C               ; skip next instruction

SetExitRoutine 
        LDY #$00
        LDA (ScriptPtr),Y       ; set up an exit check routine that sets carry to exit
        STA ExitRoutine
        INY 
        LDA (ScriptPtr),Y
        STA ExitRoutine+1
        INY 
        TYA 
        JMP IncScriptPtrA

ExitParam   =*+$01
        LDA #$00
ExitRoutine   =*+$01
CheckExit 
        JSR NoOp                ;run exit check routine and exit if it sets the carry flag
        BCC DoExit
        RTS 

CheckLoopIndex 
        LDA LoopCount           ; check if the loop index equals the count and return if so
        CMP LoopIndex
        BEQ +
DoExit  PLA 
        PLA 
+       RTS 

        LDY #$01
        LDA Counter+1
        CMP (ScriptPtr),Y
        BCC DoExit
        DEY 
        LDA Counter
        CMP (ScriptPtr),Y
        BCC DoExit
        JMP IncScriptPtr2

; -----------------------------------------------------------------------------

Poke1Byte 
        LDX #$00                ; poke 1 byte from script
        BIT $01A2               ; changed to JMP elsewhere?
        LDY #$00                ; copy X bytes from script
        LDA (ScriptPtr),Y       ; next word in script contains destination base
        STA PokeDest
        INY 
        LDA (ScriptPtr),Y
        STA PokeDest+1
-       INY                     ; copy number of bytes specified in X from script to destination
        LDA (ScriptPtr),Y
PokeDest   =*+$01
        STA $FFFF,Y
        DEX 
        BPL -
        TYA                     ; increment script pointer by number of bytes used
        SEC 
        JMP IncScriptPtrAC

; -----------------------------------------------------------------------------

IntHandler                      ;  raster interrupt handler
        PHA 
        TXA 
        PHA 
        TYA 
        PHA 
RasterRoutine   =*+$01
        JSR NoOp                ; execute previously stored raster routine
RasterFlag   =*+$01
        LDA #$80
        ASL                     ; rotate left bit into right bit
        ROL
        TAX                     ; save result in X
RasterBase1   =*+$01
-       LDA RasterFlag-1,X      ; get raster flag
        BPL SetRasterBit8       ; if the left bit is set, skip setting a new raster table
        STA RasterFlag          ; otherwise update the raster flag
        LDX #$01
        LSR RasterShift         ; repeat up to 8 times
        BCC -
RasterTblLo   =*+$01
        LDY #$00                ; update multiple base addresses to configured raster table
        STY RasterBase1
        STY RasterBase2
        STY RasterBase3
        STY RasterBase4
RasterTblHi   =*+$01
        LDY #$00
        STY RasterBase1+1
        STY RasterBase2+1
        STY RasterBase3+1
        STY RasterBase4+1
        BCS -
SetRasterBit8 
        LSR                     ; set raster line bit 8 from right bit in accumulator
        LDA $D011
        AND #$7F
        BCC +
        ORA #$80
+       STA $D011
        DEX 
RasterBase2   =*+$01
        LDA $FFFF,X             ; get raster line bits 0-7 from table and poke in register
        STA $D012
        INX 
        INX 
RasterBase3   =*+$01
        LDA $FFFF,X             ; get raster routine from table and poke in JSR operand
        STA RasterRoutine
        INX 
RasterBase4   =*+$01
        LDA $FFFF,X
        STA RasterRoutine+1
        INC RasterFlag
        INC $D019               ; clear VIC interrupt flags
        PLA 
        TAY 
        PLA 
        TAX 
        PLA 
        RTI 

RasterShift 
        .BYTE $00

; -----------------------------------------------------------------------------

SetRaster 
        LDY #$00                ; Update pointers to raster table
        LDA (ScriptPtr),Y
        STA RasterTblLo
        INY 
        LDA (ScriptPtr),Y
        STA RasterTblHi
        STY RasterShift
        JMP IncScriptPtr2

; -----------------------------------------------------------------------------

DefaultRaster                   ; default raster routine that executes standard function every 0th line
        .WORD $0000,Housekeeping
        .WORD EndOfRaster

Housekeeping 
        BIT $1000               ; changed to a jmp elsewhere?
        JMP IncCounter

; -----------------------------------------------------------------------------

SatelliteScript                                 ; satellite sprite
        .WORD AddLoopRoutine, SatelliteRoutine
        .WORD GotoScript, ReviewScript

SatelliteRoutine 
        LDA Page2000Routine
        STA SatelliteJmp
        LDA Page2000Routine+1
        STA SatelliteJmp+1
SatelliteJmp   =*+$01
        JMP $0000

; -----------------------------------------------------------------------------

ReviewScript                                    ; reviews for previous prods
        .WORD CheckLoopIndex
        .WORD GosubScriptIndirect, $2000
        .WORD AddLoopRoutine, LoadNext
        .WORD AddLoopRoutine, ReviewRoutine
        .WORD GotoScript, CloudScript

ReviewRoutine 
        LDA Page8000Routine
        STA ReviewJmp
        LDA Page8000Routine+1
        STA ReviewJmp+1
ReviewJmp   =*+$01
        JMP $0000

; -----------------------------------------------------------------------------

CloudScript                                     ; cloud effect
        .WORD GosubScriptIndirect, $2002
        .WORD GosubScriptIndirect, $8000
        .WORD AddLoopRoutine, LoadNext
        .WORD AddLoopRoutine, CloudRoutine
        .WORD GotoScript, LibertyScript

CloudRoutine 
        LDA Page2000Routine
        STA CloudJmp
        LDA Page2000Routine+1
        STA CloudJmp+1
CloudJmp   =*+$01
        JMP $0000

; -----------------------------------------------------------------------------

LibertyScript                                   ; "death of liberty" scroller
        .WORD GosubScriptIndirect, $8002
        .WORD GosubScriptIndirect, $2000
        .WORD AddLoopRoutine, LibertyRoutine
        .WORD CheckLoopIndex
        .WORD GosubScriptIndirect, $3000
        .WORD GosubScriptIndirect, $2500
        .WORD Poke1Byte, Counter
        .BYTE $60
        .WORD DoExit

LibertyRoutine 
        LDY #$18
        LDX #$00
SrcPage1   =*+$02
-       LDA $6800,X
DstPage1   =*+$02
        STA $3000,X
SrcPage2   =*+$02
        LDA $8000,X
DstPage2   =*+$02
        STA $4800,X
        INX 
        BNE -
        INC SrcPage1
        INC DstPage1
        INC SrcPage2
        INC DstPage2
        DEY 
        BNE -
        JMP ($3004)

; -----------------------------------------------------------------------------

GlitchScript                                    ; glitchy interference effect
        .WORD SetupGlitch
        .WORD PrepSprites
        .WORD SetRaster, GlitchRaster
        .WORD AddLoopRoutine, LoadNext
        .WORD SetExitRoutine, CountdownOrKeyExit
        .WORD CheckExit
        .WORD Poke1Byte, CursorAddrPtr          ; stop flashing cursor
        .BYTE $4C
        .WORD GosubScript, ColorBarScript
        .WORD SetRaster, DefaultRaster
        .WORD Poke1Byte, $D01E                  ; disable sprite collision detection
        .BYTE $00
        .WORD Poke1Byte, $D00F                  ; set sprite #7 y coordinate
        .BYTE $0B
        .WORD GotoScript, SatelliteScript

GlitchRaster 
        .WORD $0000, Glitch
        .WORD EndOfRaster

; -----------------------------------------------------------------------------

Countdown 
        .WORD $0320
CountdownOrKeyExit
        SEC
        DEC Countdown           ; exit after countdown or if Q key is pressed
        BNE +
        DEC Countdown+1
        BEQ RetCarry
+       LDA $DC01               ; check for Q key pressed
        CMP #$BF
        BEQ RetCarry
        CLC 
RetCarry 
        RTS 

; -----------------------------------------------------------------------------

RR2Count   =*+$01
Glitch  
        LDY #$01
        DEC RR2Count            ; count how many times we have executed
        BPL +                   ; only flip cursor cell every 15 cycles
        LDA #$0E                ; reset cycle counter
        STA RR2Count
        LDA (CursorAddr),Y      ; flip cursor cell
        EOR #$80
CursorAddrPtr   =*+$01
        STA (CursorAddr),Y
+       BIT LdaOp
Duration   =*+$01
        LDA #$7F                ; countdown for bursts of static, periods of normality
        DEC Duration
        BNE +
        JSR Random              ; select a random duration for the next burst
        AND #$7F
        ORA #$01
        STA Duration
        INC Alternator          ; alternate between static and normality
Alternator   =*+$01
+       LDA #$00
        AND #$01
        BEQ Scramble
LdaOp   
        LDA #$00
        STA $D418               ; silence sid, all filters off
        LDA #$F4                ; character memory at $9000, screen memory at $BC00
        STA $D018
        LDA #$1B                ; restore VIC video mode registers to default
        STA $D011
        LDA #$C8
        STA $D016
        LDA #$3E                ; make bank bit 0 an input (effectively bank 3)
        STA $DD02
SaveBorder   =*+$01
        LDA #$00                ; restore border and background color
        STA $D020
SaveBg1   =*+$01
        LDA #$00
        STA $D021
        JMP Housekeeping

Seed   =*+$01
Random  
        LDA #$1D                ; LSFR pseudorandom number generator
        BEQ +
        ASL 
        BEQ ++
        BCC ++
+       EOR #$1D
+       STA Seed
        RTS 

Scramble  
        JSR Random              ; get a pseudo-random number
        STA $D020               ; use it to set the border color
        AND #$1F 
        STA $D016               ; random horizontal scroll position/video mode
        LDA #$3C                ; both bank bits to inputs (effectively 11, bank 0)
        STA $DD02               ; bank 0 will be full of garbage characters
        JSR Random              ; get another random number
        STA $D021               ; use it for the backround
        AND #$3F                ; random vertical scroll position/video mode
        ORA #$10                ; make sure video doesn't blank
        STA $D011 
        JSR Random              ; another random number and shift bit into carry
        ASL  
RegOffset   =*+$01 
        LDY #$00 
        ADC RegOffset           ; add random number to the last register offset
        STA $D401,Y             ; stuff garbage in sid registers to make static
        LDA #$FF
        STA $D406,Y
        LDA #$0F
        STA $D418
        STA $D405,Y
        LDA #$85
        STA $D404,Y
        TYA 
        CLC 
        ADC #$07                ; increment register offset by 7
        CMP #$15                ; go back to zero after 3 increments
        BNE +
        LDA #$00
+       STA RegOffset
        JMP Housekeeping

SetupGlitch 
        LDA $D020               ; save border and background color 1
        STA SaveBorder
        LDA $D021
        STA SaveBg1
        LDX #$03                ; copy 3 pages from $0D26 to $F000
        LDY #$00
SrcPg   =*+$02
-       LDA ColorBarBlock,Y
DstPg   =*+$02
        STA ColorBarScript,Y
        INY 
        BNE -
        INC SrcPg
        INC DstPg
        DEX 
        BNE -
        LDY #$00                ; flip cursor cell
        LDA (CursorAddr),Y
        EOR #$80
        STA (CursorAddr),Y
        RTS 

; -----------------------------------------------------------------------------

ColorBarBlock
        .LOGICAL $f000
ColorBarScript 
        .WORD SetRaster, ColorBarRaster
        .WORD SetStackPops, $FF6A
        .WORD PopStack
        .WORD SetExitRoutine,FadeOutSid
        .WORD CheckExit
        .WORD ScriptReturn
PrepSprites 
        LDA #$FF                ; all sprites double width and double height
        STA $D017
        STA $D01D
        LDX #$10                ; set sprite position from table
-       LDA SpritePos,X
        STA $D000,X
        DEX 
        BPL -
        LDY #$08                ; set sprites 0-7 to colors 1-8, respectively
-       TYA 
        STA $D026,Y
        DEY 
        BNE -
        RTS 

SpriteCounter
        .BYTE $06
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00
SpritePos 
        .BYTE $EC,$08           ; X,Y for sprites 0-7
        .BYTE $24,$08
        .BYTE $54,$08
        .BYTE $84,$08
        .BYTE $B4,$08
        .BYTE $E4,$08
        .BYTE $14,$08
        .BYTE $44,$08
        .BYTE %11000001         ; high bits for sprites 0-7 X coordinate
ColorBarRaster 
        .WORD $0005, ColorBar1
SecondRaster
        .WORD $00F8, UpdateColorBarRaster
        .WORD EndOfRaster
UpdateColorBarRaster 
        LDA #<EndOfRaster       ; change raster line to end of raster indicator
        STA SecondRaster        ; so this raster routine only executes once
        LDA #>EndOfRaster
        STA SecondRaster+1
        LDA #$00                ; configure raster routine to blank the screen
        STA ScreenMode2
        LDA #$C8                ; reset X scroll and mode to defaults
        STA $D016
        LDA #$1B                ; reset Y scroll and mode register to defaults
        STA $D011
        JMP ColorBar3           ; execute last part of color bar raster routine

ColorBar1 
        LDA #<ColorBar2         ; Set interrupt handler for color bars
        STA $FFFE
        LDA #>ColorBar2
        STA $FFFF
        INC $D012               ; trigger next interrupt on next raster line
        INC $D019               ; clear interrupt flags
        TSX                     ; save return address to caller
        CLI
        NOP                     ; nop-nopity-nop until next interrupt fires
        NOP                     
        NOP                     ; nops take two cycles so interrupts
        NOP                     ; have maximum jitter of 1 cycle
        NOP
        NOP
        NOP 
        NOP 
        NOP 
        NOP 
        NOP 
        NOP 
        NOP 
        NOP 
        NOP 
                                ; cycle
ColorBar2                       ;  7/8          max 1 cycle of jitter when executing nop
        TXS                     ;  9/10         set stack pointer back to caller of ColorBar1    
        LDA #<IntHandler        ; 11/12         
        STA $FFFE               ; 15/16         switch back to normal interrupt handler                                                
        LDA #>IntHandler        ; 17/18                                                         
        STA $FFFF               ; 21/22
        LDX #$05                ; 23/24
-       DEX                     ; ..            waste cycles until we get to 53 or 54                                                      
        BNE -                   ; 47/48
        CMP ($00,X)             ; 53/54                                                         
        LDA $D012               ; 57/58         will be on same line whether on cycle 53 or 54
        CMP $D012               ; 61/62         current raster line will change on cycle 62
        BEQ +                   ; 1             take 2 cycles if raster changed, 3 if not
                                ;               so now we're guaranteed to be on cycle 1
ScreenMode1=*+1
+       LDA #$18                ; 3             first time: screen on, 25 rows
        STA $D011               ; 7             turn off top/bottom border
        AND #$08                ; 9             next time:  screen off to avoid badline?
        STA ScreenMode1         ; 13            
        NOP                     ; 15            waste cycles                                 
        LDY #$0A                ; 17                                                         
        LDX #$06                ; 19                                                         
-       DEX                     ; ..                                                        
        BNE -                   ; 48
        DEC $D016               ; 54            turn off side border
        INC $D016               ; 60                                                 
        LDX #$22                ; 62            waste cycles
-       DEY                     ; ..    ..      
        BNE -                   ; 48    19
        DEC $D016               ; 54     6      turn off side border
        INC $D016               ; 60     6
        NOP                     ; 62     2
        INC $D018               ; 5      6      switch sprite memory
        LDY #$04                ; 7      2      random data will create a noise effect
        DEX                     ; 9      2
        BNE -                   ; 12     3
                                ;       --      total cycles per iteration is less than 63
                                ;       46 <--- because vic is stealing 2 cycles per sprite
        CLC                     ;                
        ADC #$2A                
        STA $D001               ; move sprites 0-3 down the screen
        STA $D003
        STA $D005
        STA $D007
        DEC $D016               ; turn off the side border
        INC $D016
        STA $D009               ; move sprites 4-7 down the screen
        STA $D00B
        STA $D00D
        STA $D00F
        LDY #$01
        LDX #$28
        DEC SpriteCounter
        BNE -
ScreenMode2=*+1
ColorBar3
        LDX #$18
        STX $D011               ; turn off top border
        DEC $D016               ; turn off side border
        INC $D016
        LDX #$34
        BIT $EA                 
-       LDY #$04
-       DEY 
        BNE -
        NOP 
        INC $D018               ; switch sprite memory
        DEC $D016               ; turn off side border
        INC $D016
        DEX 
        BNE --
        LDA #$08
        STA $D001               ; move sprites down the screen
        STA $D003
        STA $D005
        STA $D007
        STA $D009
        STA $D00B
        STA $D00D
        STA $D00F
        LDA #$06
        STA SpriteCounter       ; get ready to start at the top again
        LDX #$FF
        STX $D015               ; enable all 8 sprites
        INX 
        STX $D021               ; black background color
        JMP Housekeeping

FadeOutSid 
        CLC 
VolumeDelay   =*+$01
        LDA #$00                ; volume decrease only every 4 cycles
        INC VolumeDelay
        AND #$03
        BNE +
        DEC Volume              ; decrease sid volume
Volume   =*+$01
        LDA #$0F
        STA $D418
        BNE +                   
        SEC                     ; exit when volume is completely off
+       RTS 
        .HERE