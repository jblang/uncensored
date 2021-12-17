a02 = $02
a03 = $03

CursorAddr = $D3
CursorRow = $D6
CopyLength = $F0
CurrentByte = $F1
ByteDst = $F2
ByteSrc = $F4
BytesLeft = $F6
CopySrc = $F8

s0840 = $0840
s0843 = $0843
s0846 = $0846
s0849 = $0849
Intro = $0900
s0F00 = $0F00
e1400 = $1400
e1403 = $1403
e2000 = $2000
e2003 = $2003
e2006 = $2006
e2280 = $2280
e2283 = $2283
e2400 = $2400
e2403 = $2403
e3E00 = $3E00
e3E03 = $3E03
e6600 = $6600
e6603 = $6603
e7D00 = $7D00
e7D03 = $7D03
e8000 = $8000
e8003 = $8003
e8006 = $8006
e9000 = $9000
e9003 = $9003
eA500 = $A500
eA503 = $A503
eB000 = $B000
eB003 = $B003
eC000 = $C000
eC003 = $C003
eE000 = $E000
eE003 = $E003
eE400 = $E400
eE403 = $E403
eEA31 = $EA31
eEB00 = $EB00
eEB03 = $EB03
eEB06 = $EB06
eFE00 = $FE00
eFE03 = $FE03



        * = $0801
    
; basic bootstrap
        .WORD EndBasic
        .WORD $0000
        .TEXT $9E, "2061", $00
EndBasic
        .WORD $0000


; -----------------------------------------------------------------------------
; delay initialization for 64 interrupts while basic looks normal

        SEI 
        LDA #<CountdownInt      ; setup countdown irq handler
        STA $0314
        LDA #>CountdownInt
        STA $0315
        CLI 
        RTS                     ; back to basic... "nothing to see here"


CountdownInt 
IntCount=*+1
        LDY #$40                ; count down from 64 each time interrupt fires
        DEC IntCount
        BEQ Init                ; launch setup routine after 64 interrupts
        JMP eEA31               ; let user interact with basic during this time


; -----------------------------------------------------------------------------
; initialize the demo

Init
        SEI 
        LDY #$04                ; copy vic screen area to $BC00 (4 pages total)
        LDX #$00
ScreenSrcPg=*+2                 ; high byte of LDA operand; incremented below
-       LDA $0400,X
ScreenDstPg=*+2                 ; high byte of STA operand; incremented below
        STA $BC00,X
        INX 
        BNE -                   ; copy this page until offset wraps to zero
        INC ScreenSrcPg         ; increment base address to copy the next page
        INC ScreenDstPg
        DEY 
        BNE -

                                ; calculate cursor address: $BC00 + Y * 40 + X
        STY CursorAddr+1        ; = 0
        ASL CursorRow           ; = row * 2
        ASL CursorRow           ; = row * 4
        ASL CursorRow           ; = row * 8
        LDA CursorRow           ; = row * 8
        ASL                     ; = row * 16 (low bits)
        ROL CursorAddr+1        ; = high bit of row x 16
        ASL                     ; = row * 32 (low bits)
        ROL CursorAddr+1        ; = high bits row * 32
        ADC CursorRow           ; = row * 32 + row * 8 = row * 40 (low bits)
        BCC +
        INC CursorAddr+1        ; = high bits of row * 40
        CLC 
+       ADC CursorAddr          ; = row * 40 (low bits) + column
        STA CursorAddr          ; = absolute address of cursor (low byte)
        LDA CursorAddr+1        ; = row * 40 (high bits)
        ADC #$BC                ; = row * 40 (high bits) + $BC
        STA CursorAddr+1        ; = absolute address of cursor (high byte)

-       BIT $D011               ; wait for vblank before reconfiguring vic
        BPL -
-       BIT $D011
        BMI -
        LDA $DD00               ; clear vic bank bit 1 (11 => 01)
        AND #$FD                ; switches to bank 3 ($8000-$BFFF)
        STA $DD00
        LDA #$F4                ; character memory at $9000, screen memory at $BC00
        STA $D018
        JSR SetupDecrunch
-       BIT $D011               ; wait for another vblank
        BPL -
-       BIT $D011
        BMI -
        LDA #$1B                ; default video mode
        STA $D011
        LDA $DD00
        AND #$FC                ; clear both vic bank bits (01 => 00)
        STA $DD00
        LDA #$3E                ; make bank bit 0 an input (gets pulled up to 1)
        STA $DD02               ; so bank bits are now effectively 01 (bank 3)
        JSR IntConfig1          ; setup interrupts

        LDA #$B1                ; tell drive to reinit and look for disk $31
        JSR LoadAndDecrunch

        LDX #$00                ; copy code from $0B6C-$0F6B to $0420-$081F
-       LDA MainBlock,X
        STA Main,X
        LDA MainBlock+$100,X
        STA Main+$100,X
        LDA MainBlock+$200,X
        STA Main+$200,X
        LDA MainBlock+$300,X
        STA Main+$300,X
        INX 
        BNE -

        LDA #>ResetInt          ; reconfigure copied interupt setup routine
        STA IntAddrHi           ; to use $07A1 for interrupt handler
        LDA #<ResetInt
        STA IntAddrLo

        JMP Main

; -----------------------------------------------------------------------------
; blink the cursor while decrunching the first scene

BlinkInt
        PHA
        TYA 
        PHA 
BlinkCount=*+1                  ; cycle counter in LDA operand; modified below
        LDY #$01
        DEC BlinkCount
        BPL +                   ; wait until cycle counts down
        LDA (CursorAddr),Y      ; flip upper bit of memory at cursor location
        EOR #$80                ; to change cell to inverse colors and back again
        STA (CursorAddr),Y
        LDA #$0E                ; don't flip again for 15 more interrupt cycles
        STA BlinkCount
+       INC $D019               ; clear vic interrupt flags
        PLA 
        TAY 
        PLA 
        RTI 

; -----------------------------------------------------------------------------
; get ready for decrunching

SetupDecrunch
        JSR InitDisk
        SEI 
        LDA #$35                ; swap out basic and kernal
        STA $01
        LDX #$00                ; copy data from $0953-$0C52 to $0200-$04ff
-       LDA LoadBlock,X
        STA LoadAndDecrunch,X
        LDA LoadBlock+$100,X
        STA LoadAndDecrunch+$100,X
        LDA LoadBlock+$200,X
        STA LoadAndDecrunch+$200,X
        INX 
        BNE -
        RTS

; -----------------------------------------------------------------------------
; send bootstrap code to drive

InitDisk
        LDA #$08                ; tell drive 8 to listen
        JSR $FFB1
        LDA #$6F                ; command channel 15
        JSR $FF93
        LDA #$49                ; tell drive to reinitialize
        JSR $FFA8     
        JSR $FFAE               ; flush command
        LDA #$08
        JSR $FFB1               ; listen again
        LDA #$6F
        JSR $FF93               ; another command
        LDX #$00
-       LDA DiskBoot,X          ; send bootstrap routine to drive
        JSR $FFA8
        INX 
        CPX #$27
        BCC -
        JMP $FFAE               ; execute

; -----------------------------------------------------------------------------
; disk bootstrap routine (runs on drive)

DiskBoot
        .TEXT "M-E"             ; execute memory
        .WORD $020B             ; code stored in the input buffer
        .BYTE $12,$12           ; tracks and sectors to load
        .BYTE $12,$0F
        .BYTE $12,$0C
        LDX #$05
-       LDA $0205,X             ; load sectors and tracks from above
        STA $06,X               ; into registers for buffers 0, 1 and 2
        DEX 
        BPL -
        LDX #$02                ; issue read sector command for buffers 0, 1, and 2
-       LDA #$80
        STA $00,X
        LDA #$01
-       CMP $00,X               ; wait for read to complete
        BNE -
        DEX 
        BPL --
        JMP $04DF               ; jump to code loaded into buffer

; -----------------------------------------------------------------------------
; load and decrunch the chain whose index is in A

LoadBlock
        .LOGICAL $0200
LoadAndDecrunch
        PHA                     ; save command          
        JSR SendByte            ; send command in A
        PLA                     ; restore command
        BPL +                   ; if it wasn't a reinit command, start loading data
        LDA #$08                ; ATN high
        STA $DD00
-       BIT $DD00               ; wait for DATA low
        BPL -
        LDA #$00                ; ATN low
        STA $DD00
        RTS                     ; we're done
+       JSR LoadFirstSector     ; read requested chain into memory
        SEC 
        SBC #$03                ; calculate number of bytes left to decrunch
        STA BytesLeft
        LDA #$00
        STA BytesLeft+1         ; upper byte is 0 right now
        STY ByteSrc             ; save start address low byte
        STX ByteSrc+1           ; save start address high byte
        LDY #$02                ; set up current byte and destination address
-       LDA (ByteSrc),Y
        STA CurrentByte,Y
        DEY 
        BPL -
        CLC 
        LDA #$03                ; skip the 3 bytes we just copied
        ADC ByteSrc
        STA ByteSrc
        BCC GetCopyBit
        INC ByteSrc+1
GetCopyBit 
        ASL CurrentByte         ; get copy bit
        BNE GotCopyBit
        JSR GetNextBit
GotCopyBit 
        PHP                     ; save copy bit for later
        LDA #$01                ; initialize copy length to 1
GetCopyLength 
        ASL CurrentByte         ; get end of length marker
        BNE +
        JSR GetNextBit
+       BCC GotCopyLength       ; 0 means end of length
        ASL CurrentByte         ; get next length bit
        BNE +
        JSR GetNextBit
+       ROL
        BPL GetCopyLength       ; max 7 length bits
GotCopyLength 
        PLP                     ; restore copy bit
        BCS CopyBytes           ; 1 indicates copy, 0 plain
PlainBytes
        STA CopyLength          ; save copy length for later
        SEC                     ; update number of bytes left
        LDA BytesLeft
        SBC CopyLength
        STA BytesLeft
        LDA BytesLeft+1
        SBC #$00
        STA BytesLeft+1
        BCS +
        JSR GetNewBits          ; ask drive for more bits if we ran out
+       LDY #$00                ; copy bytes verbatim
-       LDA (ByteSrc),Y
        STA (ByteDst),Y
        INY 
        CPY CopyLength          ; copied enough bytes yet?
        BNE -
        LDX #$02                ; update source and dest pointers
        JSR UpdatePointers
        INY 
        BEQ GetCopyBit          ; get new copy bit if Y wrapped back to 0
        SEC 
        BCS GotCopyBit          ; otherwise implied copy bit = 1
CopyBytes 
        ADC #$00                ; copy bit + 0xff length = 0 = end of data
        BNE +
        JMP EndDecrunch

+       STA CopyLength          ; save length for later
        CMP #$03                ; length >= 3?
        LDA #$00                ; clear copy source offset
        STA CopySrc
        STA CopySrc+1
        ROL                     ; if length >= 3, use last 4 entries in table
        ASL CurrentByte         ; get first bit of table index
        BNE +
        JSR GetNextBit
+       ROL 
        ASL CurrentByte         ; get second bit of table index
        BNE +
        JSR GetNextBit
+       ROL 
        TAX                     ; save table index for later
GetSourceLength 
        LDY Tab,X               ; look up number of bits in copy source from table
GetSourceBits     
        ASL CurrentByte         ; get next bit of copy source
        BNE +
        JSR GetNextBit
+       ROL CopySrc
        ROL CopySrc+1
        DEY 
        BNE GetSourceBits       ; read enough source bits?
        TXA                     ; restore previous table index
        DEX                     ; decrement index
        AND #$03                ; check if any bit in the previous index was 1
        BEQ GotSourceBits       ; if not, we're done
        INC CopySrc             ; if so, increment copy source
        BNE GetSourceLength
        INC CopySrc+1
        BNE GetSourceLength     ; ... and get more bits
GotSourceBits 
        SEC                     ; subtract copy source offset from destination address
        LDA ByteDst
        SBC CopySrc
        STA CopySrc             ; ... to get absolute source address
        LDA ByteDst+1
        SBC CopySrc+1
        STA CopySrc+1
-       LDA (CopySrc),Y         ; copy bytes from source address to destination
        STA (ByteDst),Y
        INY 
        CPY CopyLength          ; loop until we copied the specified number of bytes
        BNE -
        LDX #$00                ; increment dest pointer by number of bytes copied
        JSR UpdatePointers
        JMP GetCopyBit          ; always loop

GetNextBit
        PHA
        TYA 
        PHA 
        LDA BytesLeft           ; check if any bytes are left in buffer
        ORA BytesLeft+1
        BEQ +
        BIT $DD00               ; see if drive has more data to send (DATA=1)
        BPL ++
+       TXA
        PHA 
        JSR GetNewBits          ; if so, get more data and update bytes left
        PLA 
        TAX
+       LDA BytesLeft           ; decrement bytes left
        BNE +
        DEC BytesLeft+1
+       DEC BytesLeft
        LDY #$00                ; get next byte of crunched data
        LDA (ByteSrc),Y
        INC ByteSrc             ; update pointer to next byte
        BNE +
        INC ByteSrc+1
+       SEC                     ; sentinel bit
        ROL                     ; next bit is now in carry
        STA CurrentByte         ; save remains of current byte
        PLA
        TAY 
        PLA 
        RTS 

UpdatePointers
        CLC                     ; if X = 0 ByteDst += Y
        TYA                     ; if X = 2 ByteSrc += Y and ByteDst += Y
        ADC ByteDst,X
        STA ByteDst,X
        BCC +
        INC ByteDst+1,X
+       DEX 
        DEX 
        BPL UpdatePointers
EndDecrunch 
        RTS 

GetNewBits 
        JSR LoadNextSector      ; get more bytes
        CLC                     ; add bytes retreived to bytes left
        ADC BytesLeft
        STA BytesLeft
        BCC +
        INC BytesLeft+1
+       RTS 

Tab     .BYTE $04,$02,$02,$02
        .BYTE $05,$02,$02,$03

; Load first sector in a chain
LoadFirstSector 
        LDA #$FF                ; we're starting a new chain
        STA Chain
        JSR ReadNewChain        ; get next track #
        CMP #$00                ; 0 indicates end of chain
        PHP
        JSR ReadNextByte        ; get next sector # or size (if last block in chain)
        PHA
        JSR ReadNextByte        ; get load address low byte
        PHA
        JSR ReadNextByte        ; get load address high byte
        STA Store+1             ; poke into base address for byte storage
        PLA                     ; restore low byte
        STA Store               ; poke into base address for byte storage
        PLA                     ; restore block size
        PLP                     ; restore track # = 0 comparison
        PHP                     ; save it back
        BEQ +                   ; if this is the last block, size in accumulator
        LDA #$FF                ; otherwise block is 512 bytes
+       SEC 
        SBC #$04                ; subtract 4 bytes (sector, track, and load address)
        JMP LoadSector

LoadNextSector
        LDA #$00                ; bail at the end of the chain
Chain=*-1                       ; address of LDA operand
        BEQ EndSector
        JSR ReadFirstByte       ; get next track #
        CMP #$00                ; track #0 indicates last sector
        PHP
        JSR ReadNextByte        ; get next sector #
        PLP                     ; restore last sector flag
        PHP
        BEQ +                   ; accumulator already contains size for last sector
        LDA #$FF                ; otherwise sector contains 256 bytes
+       SEC 
        SBC #$02                ; subtract the two bytes we already read

LoadSector 
        STA ReadCount           ; read the entire sector
        JSR ReadNextByte
        LDY Store               ; return low/high base address in Y/X
        LDX Store+1
        SEC 
        LDA ReadCount           ; update base address for next sector
        ADC Store
        STA Store
        BCC +
        INC Store+1
+       PLP                     ; restore last sector flag
        BEQ +
        LDA #$08                ; ATN high tells drive we want more data
        STA $DD00
        BNE ++
+       LDA #$00                ; we're done with the chain
        STA Chain
+       LDA ReadCount           ; return number of bytes read in ACC
        CLC 
        ADC #$01
EndSector 
        RTS 

ReadNewChain 
        LDY #<Temp              ; store first byte at $0418
        LDX #>Temp              ; until we read the load address from the sector
        STY Store
        STX Store+1 

ReadFirstByte 
        LDX #$08
        STX $DD00               ; ATN high
-       BIT $DD00               ; wait for DATA high
        BPL -
        LDY #$00
        STY $DD00               ; ATN low
        STY ReadCount           ; only loop once
        NOP 

ReadNextByte 
        DEY                     ; loop Y+1 times
        LDX #$08
-       LDA $DD00               ; acc = DCxxxxxx
        STX $DD00               ; ATN high
        LSR                     ; acc = 0DCxxxxx
        LSR                     ; acc = 00DCxxxx
        INY
        NOP
        LDX #$00
        EOR $DD00               ; acc = DCDCxxxx
        STX $DD00               ; ATN low
        LSR                     ; acc = 0DCDCxxx
        LSR                     ; acc = 00DCDCxx
        NOP
        NOP 
        LDX #$08
        EOR $DD00               ; acc = DCDCDCxx
        STX $DD00               ; ATN high
        LSR                     ; acc = 0DCDCDCx
        LSR                     ; acc = 00DCDCDC
        CPY #$00
ReadCount=*-1                   ; addr of CPY operand
        STA LowSixBits          ; poke low 6 bits into ORA operand
        LDA #$C0                ; acc = 11000000
        AND $DD00               ; acc = DC000000
        STA $DD00               ; ATN low
        ORA #$00                ; acc = DCDCDCDC
LowSixBits=*-1                  ; addr of ORA operand
        STA Temp,Y
Store=*-2                       ; addr of STA operand
        BCC -
        RTS 

SendByte
        SEC                     ; add sentinel bit
        ROR                     ; current bit in carry
-       TAX                     ; save remaining bits for later
        LDA #$08                ; ATN high
        STA $DD00
-       BIT $DD00               ; wait DATA high
        BPL -
        ROR                     ; current bit in position 7
        LSR
        LSR
        LSR                     ; current bit in position 4 (clk output)
        STA $DD00               ; ATN low
        TXA                     ; restore remaining bits
        LSR
        BNE --                  ; byte will be zero after last bit sent
        RTS 

Temp    .BYTE $00
        .HERE

; -----------------------------------------------------------------------------

MainBlock
        .LOGICAL $0420
Main    
        LDA #$00                ; intro (through title screen)
        JSR LoadAndDecrunch
        LDX #$FF
        TXS 
        JSR Intro
        LDA #$20                ; activate jump in interrupt routine
        STA IntJmpOp
        JSR ResetVic
        LDY #$20                ; move 58 pages from $2000 to $C000
        LDA #$C0
        LDX #$3A
        JSR MoveMem
        LDA #$09                ; disco floor effect
        JSR LoadAndDecrunch
-       LDA a03
        CMP #$01
        BCC -
-       LDA a02
        CMP #$A0
        BCC -
        JSR e8000
        LDA #$0A
        JSR LoadAndDecrunch
        JSR e8006
        JSR e8003
        LDA #$0B
        JSR LoadAndDecrunch
        LDA #$0C
        JSR LoadAndDecrunch
        JSR e8006
        JSR ResetVic
        LDA #$0D
        JSR LoadAndDecrunch
        JSR e2000
        LDA #$0E
        JSR LoadAndDecrunch
        JSR e2003
        JSR ResetVic
        LDY #$A4
        LDA #$24
        LDX #$1B
        JSR MoveMem
        JSR e3E00
        LDA #$0F
        JSR LoadAndDecrunch
        LDA #$10
        JSR LoadAndDecrunch
        JSR e3E03
        JSR e9000
        JSR e9003
        JSR ResetVic
        LDA #$11
        JSR LoadAndDecrunch
        JSR eE000
        LDA #$12
        JSR LoadAndDecrunch
        LDA #$13
        JSR LoadAndDecrunch
        JSR eE003
        JSR e2000
        JSR e2003
        JSR eC000
        LDA #$90
        STA IntJmpPage
        LDA #$14
        JSR LoadAndDecrunch
        JSR eC003
        JSR ResetVic
        JSR s0840
        LDA #$15
        JSR LoadAndDecrunch
        LDA #$16
        JSR LoadAndDecrunch
        JSR s0849
        JSR ResetVic
        LDY #$80
        LDA #$40
        LDX #$0E
        JSR MoveMem
        LDY #$9D
        LDA #$60
        LDX #$09
        JSR MoveMem
        LDA #$17
        JSR LoadAndDecrunch
        JSR s0843
        LDA #$18
        JSR LoadAndDecrunch
        LDA #$19
        JSR LoadAndDecrunch
        JSR s0849
        JSR ResetVic
        JSR s0846
        JSR s0849
        JSR ResetVic
        LDY #$80
        LDA #$08
        LDX #$0F
        JSR MoveMem
        LDY #$9D
        LDA #$18
        LDX #$09
        JSR MoveMem
        LDA #$1A
        JSR LoadAndDecrunch
        LDA #$1B
        JSR LoadAndDecrunch
        JSR e7D00
        JSR e7D03
        JSR ResetVic
        LDA #$1C
        JSR LoadAndDecrunch
        JSR e1400
        LDA #$1D
        JSR LoadAndDecrunch
        LDA #$1E
        JSR LoadAndDecrunch
        JSR e1403
        JSR eA500
        LDA #$1F
        JSR LoadAndDecrunch
        JSR eA503
        JSR ResetVic
        LDY #$20
        LDA #$A0
        LDX #$56
        JSR MoveMem
        LDA #$20
        JSR LoadAndDecrunch
        JSR e6600
        JSR e6603
        JSR ResetVic
        LDA #$21
        JSR LoadAndDecrunch
        JSR eE400
        LDA #$B2                ; flip disk
        JSR LoadAndDecrunch
        JSR eE403
        LDA #$2C
        STA IntJmpOp            ; deactivate jump in interrupt routine
        LDA #$10
        STA IntJmpPage
        JSR ResetVic
        LDA #$00
        JSR LoadAndDecrunch
        LDA #$00
        STA a02
        STA a03
        JSR e2000
        LDA #$10
        STA IntJmpPage
        LDA #$20                ; activate jump in interrupt routine
        STA IntJmpOp
        JSR e2003
        LDA #$01
        JSR LoadAndDecrunch
        JSR e2006
        JSR ResetVic
        LDY #$52
        LDA #$C0
        LDX #$34
        JSR MoveMem
        JSR eEB00
        LDA #$02
        JSR LoadAndDecrunch
        LDA #$03
        JSR LoadAndDecrunch
        JSR eEB06
        JSR eEB03
        JSR eEB06
        JSR ResetVic
        LDY #$B0
        LDA #$D8
        LDX #$07
        JSR MoveMem
        JSR e2000
        LDA #$04
        JSR LoadAndDecrunch
        LDA #$05
        JSR LoadAndDecrunch
        LDY #$B0
        LDA #$D0
        LDX #$07
        JSR MoveMem
        LDA #$06
        JSR LoadAndDecrunch
        LDA #$07
        JSR LoadAndDecrunch
        JSR e2003
        JSR ResetVic
        LDA #$08
        JSR LoadAndDecrunch
        LDA #$09
        JSR LoadAndDecrunch
        JSR eEB00
        LDA #$0A
        JSR LoadAndDecrunch
        JSR eEB03
        LDA #$0B
        JSR LoadAndDecrunch
        JSR eEB06
        JSR ResetVic
        LDA #$0C
        JSR LoadAndDecrunch
        JSR eC000
        LDA #$0D
        JSR LoadAndDecrunch
        JSR eC003
-       LDA a03
        CMP #$02
        BCC -
-       LDA a02
        CMP #$62
        BCC -
        JSR e2000
        JSR ResetVic
        LDA #$11
        JSR LoadAndDecrunch
        LDX #$3F
        TXS 
        JSR SwapOutLoad
-       LDA a03
        CMP #$0C
        BCC -
-       LDA a02
        CMP #$38
        BCC -
        JSR e2280
        LDA #$0E
        JSR s0F00
        JSR e2283
        JSR SwapInLoad
        LDX #$FF
        TXS 
        JSR ResetVic
        LDA #$0F
        JSR LoadAndDecrunch
        JSR e2400
        LDA #$10
        JSR LoadAndDecrunch
        JSR e2403
        LDY #$60
        LDA #$B0
        LDX #$4E
        JSR MoveMem
        JSR eB000
        LDA #$12
        JSR LoadAndDecrunch
        JSR eB003
        LDA #$10
        STA IntJmpPage
        LDA #$20
        STA IntJmpOp            ; activate jump in interrupt routine
        JSR ResetVic
        LDX #$3F
        TXS 
        JSR SwapOutLoad
        JSR e2280
        LDA #$13
        JSR s0F00
        LDA #$16
        JSR s0F00
        JSR e2283
        JSR ResetVic
        JSR SwapInLoad
        LDX #$FF
        TXS 
        JSR ResetVic
        LDA #$15
        JSR LoadAndDecrunch
        JSR eFE00
        LDY #$C8
        LDA #$08
        LDX #$07
        JSR MoveMem
        LDA #$17
        JSR LoadAndDecrunch
        LDY #$20
        LDA #$A0
        LDX #$4F
        JSR MoveMem
        LDA #$18
        JSR LoadAndDecrunch
        LDA #$19
        JSR LoadAndDecrunch
        JSR eFE03
        JSR e2000
        LDA #$1A
        JSR LoadAndDecrunch
        JSR e2003
        JSR eFE00
        LDA #$1B
        JSR LoadAndDecrunch
        JSR eFE03
        JSR e2000
        LDA #$1C
        JSR LoadAndDecrunch
        JSR e2003
        LDA #$1D
        JSR LoadAndDecrunch
        LDA #$1E
        JSR LoadAndDecrunch
        JSR e2006
        JSR ResetVic
-       INC $D020
        JMP -

; -----------------------------------------------------------------------------

SwapOutLoad                     ; copy LoadAndDecrunch up to $FE00
        LDX #$00
-       LDA LoadAndDecrunch,X
        STA eFE00,X
        INX 
        BNE -
        RTS 

SwapInLoad                      ; copy LoadAndDecrunch back to $0200
        LDX #$00
-       LDA eFE00,X
        STA LoadAndDecrunch,X
        INX 
        BNE -
        RTS 

; move memory block: Y = src page, A = dest page, X = # of pages
MoveMem 
        DEC $01                 ; swap out I/O ports for ram
        STY SrcPage
        STA DstPage
        LDY #$00
SrcPage=*+2                     ; high byte of LDA operand
-       LDA $FF00,Y             ; copy 1 page of memory from src to dest
DstPage=*+2                     ; high byte of STA operand
        STA $FF00,Y
        INY 
        BNE -
        INC SrcPage             ; increment src/dest pages
        INC DstPage
        DEX                     ; decrement remaining pages to copy
        BPL -
        INC $01                 ; restore I/O ports
        RTS 

; -----------------------------------------------------------------------------

ResetVic 
        BIT $D011               ; wait for vblank
        BPL ResetVic
-       BIT $D011
        BMI -
        LDA #$00
        STA $D011               ; blank screen
        STA $D015               ; disable sprites
        STA $D020               ; black border
        STA $D021               ; black background

; -----------------------------------------------------------------------------

IntConfig1=*-Main+MainBlock     ; address before relocation
IntConfig 
        SEI 
        LDA #$7F                ; disable all interrupts on both CIAs
        STA $DC0D 
        STA $DD0D 
        BIT $DC0D               ; read interrupt flags on both CIAs to clear them
        BIT $DD0D 
        LDA #$01 
        STA $D019               ; clear $D012 compare interrupt flag
        STA $D01A               ; enable $D012 compare interrupt
IntAddrLo=*+1                   ; low byte of interrupt addres; modified elswhere
        LDA #<BlinkInt          ; setup interrupt handler
        STA $FFFE
IntAddrHi=*+1
        LDA #>BlinkInt          ; high byte of interrupt address; modified elsewhere
        STA $FFFE+1
        LDA #$F9                ; $D012 compare interrupt on last line (249)
        STA $D012
        CLI 
        RTS 

ResetInt 
        PHA                     ; save registers and memory layout
        LDA $01
        PHA 
        TXA 
        PHA 
        TYA 
        PHA 
        LDA #$35                ; set memory layout to I/O ports + ram only
        STA $01
        DEC $D019               ; clear VIC interrupt flags
        LDA #$73                ; bitmap extended color mode, scrolled 5 pixels down, 24 rows
        STA $D011
IntJmpPage=*+2
IntJmpOp
        BIT $1000               ; gets replaced by a JMP instruction elsewhere
-       BIT $D011               ; wait for second half of screen
        BPL -
        LDA #$00
        STA $D015               ; disable all sprites
        STA $D021               ; black background
        LDA #$7B                ; bitmap extended color mode, scrolled 5 pixels down, 25 rows
        STA $D011
        PLA                     ; restore registers and memory layout
        TAY 
        PLA 
        TAX 
        PLA 
        STA $01
        PLA 
        RTI 
        .HERE
