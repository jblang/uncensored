; 
; **** ZP ABSOLUTE ADRESSES **** 
; 
a00 = $00
a04 = $04
a06 = $06
a07 = $07
a08 = $08
a09 = $09
a0E = $0E
a0F = $0F
a12 = $12
a13 = $13
a16 = $16
a17 = $17
a3E = $3E
a86 = $86
; 
; **** FIELDS **** 
; 
f017F = $017F
f0180 = $0180
f0181 = $0181
f0200 = $0200
f067E = $067E
f067F = $067F
f0700 = $0700
f0701 = $0701
f0780 = $0780
f0781 = $0781
fF8A0 = $F8A0
fF8C0 = $F8C0
; 
; **** ABSOLUTE ADRESSES **** 
; 
a01FF = $01FF
a0201 = $0201
a0202 = $0202
a0203 = $0203
a06FF = $06FF
a077E = $077E
a077F = $077F
a07FF = $07FF
a1800 = $1800
a1C00 = $1C00
a1C01 = $1C01
a1C07 = $1C07
; 
; **** POINTERS **** 
; 
p12 = $0012
pA4 = $00A4
p6D37 = $6D37
; 
; **** EXTERNAL JUMPS **** 
; 
eF50A = $F50A
eF969 = $F969

        * = $0300

        TSX 
        STX a04D8
j0304   LDX #$FF
        TXS 
        JSR eF50A
b030A   BVC b030A
        CLV 
        LDA a1C01
        LDA #$00
        LDY #$80
b0314   BVC b0314
        STA a86
        .BYTE $AF,$01,$1C
        AND #$C0
        ASL 
        ORA a86
        ROL 
        ROL 
        STA f067E,Y
        TXA 
        .BYTE $4B,$3F
        STA f067F,Y
        .BYTE $AF,$01,$1C
        CLV 
        DEY 
        BMI b0367
        DEY 
        ROR 
b0334   BVC b0334
        PHA 
        LDA a1C01
        STA f0700,Y
        ASL 
        AND #$06
        STA a86
        TXA 
        ROL 
        AND #$1F
        STA f0701,Y
        .BYTE $AF,$01,$1C
        AND #$1F
        STA f0781,Y
        TXA 
        AND #$E0
        ASL 
        ROL 
        ORA a86
        ROL 
        ROL 
        STA f0780,Y
        LDA a1C01
        CLV 
        PHA 
        AND #$07
        JMP b0314

b0367   LDX #$FF
        TXS 
        LDY #$7F
b036C   DEY 
        LDA f0181,Y
        LSR 
        LSR 
        LSR 
        PHA 
        LDA f0180,Y
        LSR 
        LSR 
        LSR 
        PHA 
        LDA f0700,Y
        .BYTE $4B,$7C
        LSR 
        STA f0700,Y
        DEY 
        BPL b036C
        LDY a01FF
        LDX a06FF
        LDA fF8C0,Y
        ORA fF8A0,X
        STA a08
        LDY a077E
        LDX a077F
        LDA fF8C0,Y
        ORA fF8A0,X
        STA a09
        NOP 
        NOP 
        NOP 
        NOP 
        LDX #$00
        LDA a08
        BNE b03BD
        LDA a09
        AND #$03
        TAX 
        LDY f0200,X
        LDA a09
        LSR 
        EOR #$7F
        STA f03D7,Y
b03BD   STX a86
        LDA #$08
        ORA a1C00
        STA a1C00
        LDY #$80
        LDX f017F,Y
        LDA #$10
b03CE   BIT a1800
        BPL b03CE
        STA a1800
f03D7   =*+$01
        CPY #$00
        BEQ b0445
        LDA f0200,X
b03DD   BIT a1800
        BMI b03DD
        STA a1800
        ASL 
        ORA #$10
        LDX f067F,Y
b03EB   BIT a1800
        BPL b03EB
        STA a1800
        DEY 
        LDA f0200,X
b03F7   BIT a1800
        BMI b03F7
        STA a1800
        ASL 
        ORA #$10
        LDX a06FF,Y
b0405   BIT a1800
        BPL b0405
        STA a1800
        CPY #$FF
        BEQ b0445
        LDA f0200,X
b0414   BIT a1800
        BMI b0414
        STA a1800
        ASL 
        ORA #$10
        LDX f0700,Y
b0422   BIT a1800
        BPL b0422
        STA a1800
        LDA f0200,X
b042D   BIT a1800
        BMI b042D
        STA a1800
        ASL 
        ORA #$10
        LDX f0780,Y
b043B   BIT a1800
        BPL b043B
        STA a1800
        CPY #$FF
b0445   BEQ b04AB
        LDA f0200,X
b044A   BIT a1800
        BMI b044A
        STA a1800
        ASL 
        ORA #$10
        LDX a077F,Y
b0458   BIT a1800
        BPL b0458
        STA a1800
        DEY 
        LDA f0200,X
b0464   BIT a1800
        BMI b0464
        STA a1800
        ASL 
        ORA #$10
        LDX f067E,Y
b0472   BIT a1800
        BPL b0472
        STA a1800
        CPY #$FF
        BEQ b04AB
        LDA f0200,X
b0481   BIT a1800
        BMI b0481
        STA a1800
        ASL 
        ORA #$10
        LDX f0180,Y
b048F   BIT a1800
        BPL b048F
        STA a1800
        LDA f0200,X
        LDX f017F,Y
b049D   BIT a1800
        BMI b049D
        STA a1800
        ASL 
        ORA #$10
        JMP b03CE

b04AB   LDA #$00
b04AD   BIT a1800
        BMI b04AD
        STA a1800
        LDA #$F7
        AND a1C00
        STA a1C00
        LDX a86
        LDY f0200,X
        TYA 
        BEQ b04C7
        LDA #$FF
b04C7   STA f03D7,Y
        LDA a08
        CMP a06
        BNE b04D7
        LDA a09
        STA a07
        JMP j0304

a04D8   =*+$01
b04D7   LDX #$00
        TXS 
        LDA #$01
        JMP eF969

; entry point
        JSR s05A4
        SEI 
        LDA #$20
        STA a1C07
b04E8   LDA #$FF
        STA a3E
        LDA #$F3
        AND a1C00
        STA a1C00
        JSR s0582
        BMI b0524
        ASL 
        TAX 
        LDA s05A4,X
        STA a06
        LDA f05A5,X
        STA a07
        LDA #$04
        ORA a1C00
        STA a1C00
j050D   LDA #$E0
        STA a00
        CLI 
b0512   LDA a00
        BMI b0512
        SEI 
        LDA a08
        BEQ b04E8
        STA a06
        LDA a09
        STA a07
        JMP j050D

b0524   AND #$7F
        STA a86
        LDA #<p12
        LDX #>p12
        STA a0E
        STX a0F
        LDA #$0C
        ORA a1C00
        STA a1C00
b0538   JSR s0573
        LDA a07FF
        CMP a86
        BNE b0538
        LDA #$10
b0544   BIT a1800
        BPL b0544
        STA a1800
        LDA #$00
b054E   BIT a1800
        BMI b054E
        STA a1800
        LDX #$09
        STX a0F
        JSR s0573
        LDX #$00
b055F   LDA f0700,X
        STA s05A4,X
        INX 
        BNE b055F
        JMP b04E8

b056B   LDA a16
        STA a12
        LDA a17
        STA a13
s0573   LDA #$80
        STA a04
        CLI 
b0578   LDA a04
        BMI b0578
        SEI 
        CMP #$01
        BNE b056B
        RTS 

s0582   LDA #$80
b0584   TAY 
        LDX #$10
b0587   BIT a1800
        BPL b0587
        STX a1800
        LDX #$00
b0591   BIT a1800
        BMI b0591
        LDA a1800
        STX a1800
        LSR 
        LSR 
        LSR 
        TYA 
        ROR 
        BCC b0584
        RTS 

f05A5   =*+$01
s05A4   LDX #$1F
b05A6   LDA fF8C0,X
        BMI b05C7
        STA a86
        AND #$F0
        LSR a86
        BCS b05B5
        ORA #$08
b05B5   LSR a86
        BCS b05BB
        ORA #$02
b05BB   LSR a86
        BCS b05C1
        ORA #$04
b05C1   LSR a86
        BCS b05C7
        ORA #$01
b05C7   STA f0200,X
        DEX 
        BPL b05A6
        LDA #<p6D37
        STA f0200
        LDA #>p6D37
        STA a0201
        LDA #<pA4
        STA a0202
        LDA #>pA4
        STA a0203
        RTS 

        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00
