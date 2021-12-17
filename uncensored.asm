; Uncensored decruncher

cpl = $02
cur = $03
zp = $04
cps = $08

PrgDst = $FF00

        * = $0801

; -----------------------------------------------------------------------------
; basic bootloader

        .WORD EndBasic
        .WORD $0000
        .TEXT $9E, "2061", $00
EndBasic
        .WORD $0000

; -----------------------------------------------------------------------------
; stage 1 bootloader

Stage1
        SEI 
        LDA #$34                ; swap out basic, kernal, ports
        STA $01
        LDX #$3D                ; copy table ptr + stage2 bootstrap from $081F-$085C to $06-$43
-       LDA Stage2,X
        STA get,X
        DEX 
        BPL -
        JMP cps                 ; start stage2 from $08

; -----------------------------------------------------------------------------
; stage 2 bootloader

Stage2
        .LOGICAL $06
get     .WORD $FAD8
-       LDY #$FF                ; copy $0839-$0D39 to $FA00-$FFFF, one page at a time
SrcAddr=*+1
-       LDA PrgSrc,Y            ; source page (starts at $0D)
DstAddr=*+1
        STA PrgDst,Y            ; dest page (starts at $FF)
        DEY 
        BNE -
        LDA (SrcAddr),Y         ; copy last byte of page
        STA (DstAddr),Y
        DEC DstAddr+1           ; decrement source page
        DEC SrcAddr+1           ; decrement dest page
        LDA SrcAddr+1
        CMP #$08                ; stop after copying page at $0839
        BCS --
-       LDA Stage3,Y            ; copy decruncher from $085D-$0910 to $0334-$03E7
        STA D_loop,Y
        INY 
        CPY #$B4
        BCC -
        LDY #$02                ; copy first 3 bytes of crunched data to cur and put hi/lo
-       LDA (get),Y
        STA cur,Y
        DEY 
        BPL -
        CLC                     ; increment source pointer by 3 bytes
        LDA #$03
        ADC get
        STA get
        BCC +
        INC get+1
+       JMP D_loop              ; start decruncher from $0334
        .HERE

; -----------------------------------------------------------------------------
; decruncher from ByteBoozer 1.1: http://csdb.dk/release/?id=109317

Stage3
        .LOGICAL $0334
D_loop  JSR D_get               ; get copy bit
Dl_1    PHP                     ; save it for later
        LDA #$01                ; initialize copy length to 1
Dl_2    JSR D_get               ; check end of length marker
        BCC Dl_2e               ; 0 marks end of length, so bail
        JSR D_get               ; otherwise, get next bit of length
        ROL                     ; pull it into the length
        BPL Dl_2                ; pull in up to 7 more bits
Dl_2e   PLP                     ; restore copy bit
        BCS D_copy              ; 1 indicates copy, otherwise verbatim
D_plain STA cpl                 ; save copy length in cpl
        LDY #$00                ; copy bytes verbatim from src to dst
-       LDA (get),Y
        STA (zp),Y
        INY 
        CPY cpl                 ; copy number of bytes indicated by copy length
        BNE -
        LDX #$02
        JSR D_add               ; add number of bytes copied to put and get
        INY 
        BEQ D_loop              ; get new copy bit if y wrapped back to 0
        SEC 
        BCS Dl_1                ; otherwise, use 1 for next copy bit
D_copy  ADC #$00                ; copy bit + 0xff = 0, indicates end of data
        BEQ Done
        STA cpl                 ; save copy length
        CMP #$03                ; check if copy length >= 3
        LDA #$00                ; clear copy source offset
        STA cps
        STA cps+1
        ROL                     ; if copy length >= 3, add 4 to table index
        JSR D_get               ; get first bit of table index
        ROL 
        JSR D_get               ; get second bit of table index
        ROL 
        TAX 
Dc_1s   LDY Tab,X               ; look up number of bits in copy source from table
Dc_1    JSR D_get               ; get next bit of copy source offset
        ROL cps                 ; roll it into the pointer
        ROL cps+1
        DEY 
        BNE Dc_1                ; loop until read all bits of copy length
        TXA                     ; save previous index in A
        DEX                     ; decrement index
        AND #$03                ; check if either bit of previous index was 1
        BEQ Dc_1e               ; if not, we're done
        INC cps                 ; if so, increment copy source offset
        BNE Dc_1s               ; handle carry
        INC cps+1
        BNE Dc_1s               ; do another lookup
Dc_1e   SEC                     ; subtract copy source offset from dest address
        LDA zp                  ; to get absolute source address
        SBC cps
        STA cps
        LDA zp+1
        SBC cps+1
        STA cps+1
-       LDA (cps),Y             ; copy byte from source address to destination
        STA (zp),Y
        INY 
        CPY cpl                 ; copy specified number of bytes
        BNE -
        LDX #$00                ; increment dest pointer by number of bytes copied
        JSR D_add
        BMI D_loop              ; always loop
Done    LDA #$37
        STA $01
        CLI 
        JMP Stage1

D_get   ASL cur                 ; get left bit from $03 in carry
        BNE Dg_end              ; return if there are bits left in $03
        PHA                     ; save A & Y
        TYA 
        PHA 
        LDY #$00                ; get next byte from table
        LDA (get),Y
        INC get                 ; increment table pointer
        BNE +
        INC get+1
+       SEC                     ; rotate 1 into right bit of next byte
        ROL                     ; left bit is now in carry
        STA cur                 ; save it in $03
        PLA                     ; restore A & Y
        TAY 
        PLA 
Dg_end  RTS 

D_add   CLC                     ; x specifies which pointers hould be incremented:
        TYA                     ; 2 for put and get, or 1 for only put
        ADC zp,X                ; increment specified pointers
        STA zp,X
        BCC +
        INC zp+1,X              ; handle carry
+       DEX                     ; next address
        DEX 
        BPL D_add               ; loop again if we have another address to increment
D_end   RTS 

Tab     .BYTE $04,$02,$02,$02,$05,$02,$02,$03
        .HERE

; -----------------------------------------------------------------------------
; crunched program starts here

        .BYTE $5D,$01,$08,$01,$0B,$08,$00,$00
        .BYTE $9E,$32,$30,$36,$31,$00,$75,$CC
        .BYTE $78,$A9,$1A,$8D,$14,$03,$A9,$08
        .BYTE $8D,$15,$03,$58,$60,$A0,$40,$CE
        .BYTE $1B,$08,$F0,$03,$4C,$31,$EA,$78
        .BYTE $A0,$04,$A2,$00,$BD,$00,$04,$9D
        .BYTE $00,$BC,$E8,$D0,$F7,$EE,$2B,$08
        .BYTE $EE,$2E,$08,$88,$D0,$EE,$84,$D4
        .BYTE $06,$D6,$09,$68,$A5,$D6,$0A,$26
        .BYTE $D4,$0D,$7C,$65,$D6,$90,$03,$E6
        .BYTE $D4,$18,$65,$D3,$85,$D3,$A5,$D4
        .BYTE $69,$BC,$85,$D4,$2C,$11,$D0,$10
        .BYTE $FB,$81,$55,$02,$30,$FB,$AD,$00
        .BYTE $DD,$29,$FD,$8D,$AA,$56,$A9,$F4
        .BYTE $8D,$18,$D0,$20,$E0,$08,$34,$C1
        .BYTE $A9,$1B,$8D,$F8,$F9,$FC,$8F,$BA
        .BYTE $46,$3E,$8D,$02,$DD,$20,$C5,$0E
        .BYTE $A9,$B1,$20,$00,$02,$9A,$C1,$6C
        .BYTE $0B,$9D,$20,$04,$80,$0C,$C0,$05
        .BYTE $60,$0D,$30,$06,$18,$0E,$0C,$23
        .BYTE $07,$57,$42,$E5,$A9,$07,$8D,$96
        .BYTE $07,$A9,$A1,$8D,$91,$07,$4C,$92
        .BYTE $E8,$48,$98,$48,$A0,$01,$CE,$C8
        .BYTE $08,$10,$0B,$B1,$D3,$49,$80,$91
        .BYTE $D3,$A9,$0E,$8D,$C8,$95,$1C,$19
        .BYTE $D0,$68,$A8,$68,$40,$3C,$8A,$09
        .BYTE $19,$A2,$35,$85,$01,$B9,$90,$53
        .BYTE $09,$9D,$34,$30,$0A,$18,$03,$0D
        .BYTE $41,$55,$01,$3C,$22,$EB,$60,$9D
        .BYTE $78,$20,$B1,$FF,$A9,$6F,$20,$93
        .BYTE $2B,$D5,$49,$20,$A8,$FF,$20,$AE
        .BYTE $FF,$89,$62,$51,$21,$2C,$09,$57
        .BYTE $09,$E8,$E0,$27,$90,$F5,$4C,$4E
        .BYTE $01,$4D,$2D,$45,$0B,$02,$12,$51
        .BYTE $0F,$12,$0C,$A2,$20,$AB,$C3,$05
        .BYTE $02,$95,$06,$CA,$10,$F8,$A2,$02
        .BYTE $A9,$80,$95,$00,$A9,$01,$D5,$00
        .BYTE $D0,$FC,$D9,$F3,$4C,$DF,$1F,$B8
        .BYTE $20,$FF,$03,$68,$10,$10,$78,$C8
        .BYTE $64,$03,$2C,$14,$10,$A1,$00,$05
        .BYTE $2B,$0E,$60,$20,$39,$03,$38,$E9
        .BYTE $03,$85,$F6,$7C,$85,$F7,$84,$F4
        .BYTE $86,$F5,$A0,$02,$B1,$F4,$99,$F1
        .BYTE $00,$88,$39,$AB,$C8,$18,$A9,$03
        .BYTE $65,$F4,$85,$F4,$90,$02,$E6,$F5
        .BYTE $06,$F1,$D0,$03,$20,$E6,$02,$08
        .BYTE $03,$C1,$49,$90,$0A,$C1,$2E,$C7
        .BYTE $2A,$10,$ED,$28,$B0,$2A,$85,$F0
        .BYTE $38,$A5,$F6,$E5,$F0,$66,$A5,$F7
        .BYTE $E9,$8A,$80,$B0,$94,$A1,$24,$03
        .BYTE $A0,$00,$E5,$66,$91,$F2,$C8,$C4
        .BYTE $F0,$26,$51,$E8,$20,$C6,$B7,$92
        .BYTE $C8,$F0,$B9,$38,$B0,$BD,$69,$05
        .BYTE $8D,$88,$23,$03,$61,$48,$C9,$03
        .BYTE $D2,$53,$F8,$85,$F9,$2A,$8A,$9D
        .BYTE $41,$0A,$70,$AA,$BC,$31,$03,$62
        .BYTE $8C,$26,$F8,$26,$F9,$82,$56,$F2
        .BYTE $8A,$CA,$29,$03,$F0,$08,$E6,$F8
        .BYTE $60,$82,$88,$E6,$F9,$D0,$E1,$69
        .BYTE $8D,$F2,$E5,$F8,$4A,$1B,$A5,$F3
        .BYTE $E5,$F9,$93,$B1,$F8,$8C,$3A,$C5
        .BYTE $A1,$3A,$09,$49,$3C,$02,$35,$68
        .BYTE $E9,$4D,$05,$F7,$F0,$05,$10,$D9
        .BYTE $07,$8A,$48,$1C,$D5,$28,$AA,$42
        .BYTE $73,$D0,$02,$C6,$F7,$C6,$F6,$3C
        .BYTE $DA,$E6,$F4,$D0,$43,$11,$38,$31
        .BYTE $92,$F1,$74,$3C,$60,$18,$98,$75
        .BYTE $F2,$95,$F2,$A4,$D9,$F6,$F3,$CA
        .BYTE $6B,$0A,$F2,$F5,$19,$63,$03,$B2
        .BYTE $27,$F6,$7C,$8A,$8A,$01,$F7,$60
        .BYTE $04,$02,$01,$05,$D2,$C3,$23,$FF
        .BYTE $8D,$64,$E5,$18,$AA,$03,$C9,$00
        .BYTE $66,$DC,$C7,$03,$48,$08,$88,$8D
        .BYTE $FB,$25,$02,$8D,$FA,$0D,$28,$3D
        .BYTE $0A,$02,$55,$E6,$D2,$04,$4C,$79
        .BYTE $19,$29,$F0,$42,$20,$B4,$C4,$4F
        .BYTE $C6,$54,$02,$8D,$EB,$03,$83,$C2
        .BYTE $AC,$E2,$30,$AE,$FB,$6C,$AD,$1A
        .BYTE $0A,$6D,$C4,$CD,$A8,$A8,$EE,$FB
        .BYTE $54,$4A,$F0,$07,$4F,$04,$D0,$05
        .BYTE $35,$E3,$16,$4D,$A2,$F4,$69,$01
        .BYTE $6C,$9A,$85,$18,$A2,$04,$8C,$20
        .BYTE $8E,$A0,$DC,$A2,$08,$8E,$A1,$B4
        .BYTE $D6,$07,$8C,$0A,$8C,$08,$EA,$88
        .BYTE $44,$CB,$F1,$85,$D4,$4A,$4A,$C8
        .BYTE $EA,$54,$C7,$4D,$06,$21,$EA,$EA
        .BYTE $23,$E1,$81,$C0,$F0,$F8,$47,$88
        .BYTE $C0,$2D,$33,$17,$6B,$D5,$09,$00
        .BYTE $99,$18,$04,$90,$CC,$60,$38,$6A
        .BYTE $AA,$AA,$DA,$01,$6A,$4A,$C1,$8E
        .BYTE $22,$8A,$4A,$D0,$EA,$60,$00,$BE
        .BYTE $C0,$DD,$25,$FF,$9A,$F4,$5C,$A9
        .BYTE $20,$8D,$B5,$07,$20,$61,$07,$A0
        .BYTE $20,$37,$A8,$A2,$3A,$20,$42,$93
        .BYTE $88,$09,$70,$A5,$50,$4F,$06,$01
        .BYTE $90,$FA,$A5,$02,$C9,$A0,$8F,$68
        .BYTE $80,$A9,$0A,$52,$DA,$56,$80,$20
        .BYTE $03,$16,$0B,$0B,$A9,$52,$0C,$C4
        .BYTE $26,$89,$7C,$0D,$C2,$E8,$02,$D9
        .BYTE $BE,$11,$54,$FC,$AB,$5A,$A4,$A9
        .BYTE $24,$A2,$1B,$2A,$E4,$26,$3E,$A9
        .BYTE $0F,$C8,$F2,$10,$87,$00,$3E,$C8
        .BYTE $90,$20,$AD,$B9,$2A,$51,$11,$2A
        .BYTE $D8,$E0,$A9,$12,$F1,$13,$43,$C4
        .BYTE $E0,$52,$E2,$82,$19,$62,$C0,$A9
        .BYTE $90,$8D,$B7,$99,$14,$42,$C1,$C0
        .BYTE $7A,$77,$1A,$40,$6C,$64,$15,$49
        .BYTE $16,$89,$A6,$49,$08,$61,$D1,$C3
        .BYTE $21,$40,$A2,$0E,$38,$95,$B4,$9D
        .BYTE $A9,$60,$A2,$09,$17,$31,$17,$D0
        .BYTE $43,$B4,$64,$18,$51,$19,$64,$57
        .BYTE $27,$53,$46,$08,$C9,$3A,$BD,$4A
        .BYTE $A2,$0F,$24,$E5,$4D,$12,$77,$8A
        .BYTE $62,$1A,$EA,$7D,$1D,$62,$7D,$1B
        .BYTE $33,$7D,$E4,$1C,$A0,$E6,$14,$A9
        .BYTE $1D,$C6,$CA,$1E,$38,$F1,$D2,$D4
        .BYTE $A5,$A9,$1F,$16,$5A,$A5,$44,$B4
        .BYTE $A0,$A2,$56,$4F,$D7,$56,$88,$C0
        .BYTE $66,$8E,$64,$66,$D1,$21,$41,$CD
        .BYTE $E4,$A9,$B2,$44,$54,$41,$2C,$25
        .BYTE $C5,$54,$D3,$D7,$4B,$DD,$26,$86
        .BYTE $60,$BA,$D2,$02,$85,$03,$23,$68
        .BYTE $5B,$49,$B1,$91,$42,$E1,$0D,$25
        .BYTE $5E,$92,$60,$52,$93,$8C,$34,$A4
        .BYTE $98,$68,$EB,$A9,$02,$5E,$E3,$ED
        .BYTE $0E,$05,$EB,$72,$EB,$06,$E4,$12
        .BYTE $D1,$B0,$A9,$D8,$A2,$07,$05,$CA
        .BYTE $63,$04,$20,$88,$05,$17,$0B,$04
        .BYTE $D0,$6E,$79,$10,$06,$75,$E8,$3F
        .BYTE $4E,$9C,$B9,$38,$2F,$4A,$61,$8B
        .BYTE $2D,$29,$26,$2A,$B4,$A4,$3B,$2B
        .BYTE $F4,$A4,$F9,$34,$6C,$74,$DA,$66
        .BYTE $65,$56,$29,$02,$55,$80,$62,$D8
PrgSrc  .BYTE $C2,$D2,$84,$F5,$89,$19,$3F,$B2
        .BYTE $90,$2A,$07,$E9,$0C,$43,$A4,$38
        .BYTE $3A,$9A,$80,$22,$A8,$57,$F2,$D2
        .BYTE $83,$22,$20,$36,$07,$D4,$E2,$2E
        .BYTE $C9,$AD,$15,$A7,$24,$2A,$6A,$3E
        .BYTE $24,$A0,$09,$A9,$B0,$A2,$4E,$03
        .BYTE $06,$B0,$4C,$68,$50,$B0,$A4,$5A
        .BYTE $FA,$E9,$6C,$B7,$C5,$5D,$96,$38
        .BYTE $AB,$B2,$37,$65,$86,$19,$D4,$59
        .BYTE $D9,$4D,$A2,$CD,$93,$FE,$A0,$C8
        .BYTE $D5,$48,$FD,$51,$74,$9C,$24,$4F
        .BYTE $1D,$E5,$4B,$D1,$76,$48,$FE,$21
        .BYTE $64,$06,$D1,$6B,$12,$56,$43,$6A
        .BYTE $16,$B2,$0F,$62,$DE,$53,$3D,$49
        .BYTE $9C,$E4,$EE,$20,$D0,$4C,$24,$07
        .BYTE $CA,$EA,$41,$0C,$9D,$28,$E8,$D0
        .BYTE $EA,$EC,$19,$13,$99,$7D,$70,$C5
        .BYTE $47,$C6,$01,$8C,$4E,$07,$8D,$51
        .BYTE $07,$1A,$A8,$B9,$00,$FF,$99,$0C
        .BYTE $C8,$99,$E7,$86,$4E,$49,$06,$EE
        .BYTE $44,$EE,$E6,$73,$06,$A6,$72,$F9
        .BYTE $95,$9E,$67,$2E,$80,$8D,$15,$C0
        .BYTE $20,$6A,$80,$21,$D0,$78,$A9,$7F
        .BYTE $8D,$0D,$DC,$EE,$24,$86,$03,$2C
        .BYTE $59,$1A,$84,$8D,$19,$77,$A6,$1A
        .BYTE $D0,$A9,$C4,$8D,$FE,$FF,$1D,$80
        .BYTE $FF,$57,$1E,$F9,$8D,$12,$D0,$58
        .BYTE $60,$C4,$D3,$01,$48,$8A,$21,$9E
        .BYTE $CD,$C5,$0A,$CE,$19,$04,$73,$52
        .BYTE $DC,$AE,$45,$10,$97,$66,$0B,$3F
        .BYTE $E2,$B2,$45,$08,$7B,$B9,$9C,$D5
        .BYTE $EF,$81,$68,$51,$3F,$68,$40,$FF
