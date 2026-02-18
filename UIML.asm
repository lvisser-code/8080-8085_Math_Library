;**********************************************************************
;                8080/8085 UNSIGNED INTEGER MATH LIBRARY
; The Unsigned Integer Math Library (UIML) contains basic integer
; subroutines and functions. The operations provided are: addition,
; subtraction, multiplication, division, value comparison and 
; conversion between decimal and binary integer representations.
;
; UIML procedures operate on single precision unsigned integer binary
; numbers in the range: [0 to 65535].
;
; Procedures & execution times (2 MHz 8080)
;   UADD  HL = HL + DE          16 uS
;   USUB  HL = HL - DE          25 uS
;   UMUL  HL = HL * DE          112 to 372 uS
;   UDIV  HL = HL / DE          54 to 2352 uS
;   UCMP  S & Z FLAGS SET TO REFLECT HL - DE
;   UD2B  DECIMAL NUMBER IS CONVERTED TO BINARY
;   UB2D  BINARY NUMBER IS CONVERTED TO DECIMAL
;
; Author: Leonard Visser
;**********************************************************************

ERROF:  HLT             ;Add code here to handle overflow errors

        
;------------------------------- UADD ---------------------------------
; Integer addition.
; On call: Registers DE and HL contain the addends.
; On retn: HL contains the result.
;----------------------------------------------------------------------
UADD:   DAD D           ;HL=HL+DE: Only CY flag affected
        CC  ERROF       ;If overflow, then call error handler
        RET


;------------------------------- USUB ---------------------------------
; Integer subtraction.
; On call: Register HL contains minuend and DE contains subtrahend.
; On retn: HL contains the result.
;----------------------------------------------------------------------
USUB:   MOV A,L         ;Get low byte of minuend
        SUB E           ;Subtract low byte of subtrahend
        MOV L,A         ;L = result
        MOV A,H         ;Get high byte of minuend
        SBB D           ;Subtract high byte of subtrahend
        MOV H,A         ;H = result
        CC ERROF        ;If underflow, then call error handler
        RET


;------------------------------- UMUL ---------------------------------
; Integer multiplication.
; On call: Registers HL and DE contain the multiplicands.
; On retn: HL contains the result.
;----------------------------------------------------------------------
UMUL:   PUSH D
        XRA A           ;Test for HL less than 256
        ADD H
        JZ UMUL1        ;Branch if HL less than
        XRA A
        ADD D           ;Else, DE must be < 256...
        CNZ ERROF       ;...or overflow would result
        XCHG            ;HL now has an op < 256
UMUL1:  MOV A,L         ;Move 255 or less multiplier to A
        LXI H,0         ;Initialize partial product
UMUL2:  STC
        CMC
        RAR             ;Rotate multiplier right off end
        JNC UMUL3       ;If bit shifted out was 0, skip
        DAD D           ;Else, add multiplicand to partial product
        CC  ERROF       ;...while checking for overflow
UMUL3:  XCHG
        DAD H           ;Shift multiplicand left 1 bit...
        XCHG
        ORA A
        JNZ UMUL2       ;Branch to top of loop if mult is non-0
        POP D
        RET


;------------------------------- UDIV ---------------------------------
; Integer division.
; On call: HL = dividend, DE = divisor.
; On retn: HL = quotient, DE = remainder.
;----------------------------------------------------------------------
UDIV:   PUSH B
        MOV A,D         ;If divisor MSB = 0...
        ORA A
        JNZ UDIV0       ;...then skip
;Check for special cases
        MOV A,E         ;If divisor LSB = 0...
        ORA A
        CZ  ERROF       ;...then call error handler (divide by 0)
        CPI 1           ;If divisor = 1...
        JZ UDIVF1       ;...then branch to fast divide by 1
        CPI 2           ;If divisor = 2...
        JZ UDIVF2       ;...then branch to fast divide by 2

UDIV0:  MOV C,L         ;Move dividend (=rem) to BC
        MOV B,H
        LXI H,0         ;initialize quotient = 0...
        PUSH H          ;...on top of stack (TOS)
        INR L           ;Initialize HL (pos) = 1
;Now BC = rem, DE=div, HL = pos, TOS=quo
;Shift pos & div left until rem >= div
UDIV1:  MOV A,D         ;If msb of div = 1...
        RAL
        JC UDIV3        ;...jump
        DAD H           ;pos = pos*2 (shift left)
        XCHG 
        DAD H           ;div = div*2 (shift left)
        XCHG
        MOV A,C         ;If div < rem...
        SUB L
        MOV A,B
        SBB H
        JNC UDIV1       ;...loop

UDIV2:  CALL UDIVR      ;pos = pos/2 (shift right)
        JZ UDIV4        ;If pos = 0, we're done
        XCHG            ;Now HL=div
        CALL UDIVR      ;div = div/2 (shift right)
        XCHG            ;Now HL=pos
UDIV3:  MOV A,C         ;If div > rem...
        SUB E
        MOV A,B
        SBB D
        JC UDIV2        ;...loop
        MOV A,C         ;rem = rem - div...
        SUB E
        MOV C,A
        MOV A,B
        SBB D
        MOV B,A
        XCHG            ;Now HL=div
        XTHL            ;Now BC=rem, DE=pos, HL=quo, TOS=div
        DAD D           ;quo = quo + pos
        XTHL            ;Now BC=rem, DE=pos, HL=div, TOS=quo
        XCHG            ;Now HL=pos
        JMP UDIV2       ;Loop

UDIV4:  POP H           ;Get quotient to HL
        MOV E,C         ;Move final rem to DE
        MOV D,B
        POP B
        RET

;Fast divide by 1
UDIVF1: LXI D,0         ;Remainder = 0
        POP B
        RET

;Fast divide by 2
UDIVF2: XRA A           ;Clear CY
        MOV A,H         ;Shift H
        RAR
        MOV H,A
        MOV A,L         ;Shift L
        RAR
        MOV L,A
        LXI D,0         ;Set DE remainder = CY
        MOV A,E
        RAL
        MOV E,A
        POP B
        RET


;Right shift HL, remainder returned in CY, Z set if zero
UDIVR:  XRA A           ;Clear CY
        MOV A,H         ;Shift H
        RAR
        MOV H,A
        MOV A,L         ;Shift L
        RAR
        MOV L,A
        ORA H           ;Set zero flag if both H and L = 0
        RET


;------------------------------- UCMP ---------------------------------
; Integer compare.  Flags CY and Z set per result of HL - DE
; Sets CY flag if HL < DE (unsigned comparison).
;----------------------------------------------------------------------
UCMP:   MOV A,L         ;Subtract DE from HL
        SUB E
        MOV A,H
        SBB D           ;Flags now set
        RET             ;Return with HL & DE unchanged


;------------------------------- UD2B ---------------------------------
; Convert decimal number to binary
; On call: DE = ptr to decimal number in ASCII
; On retn: HL = binary result, DE is advanced past decimal number.
;----------------------------------------------------------------------
UD2B:   LDAX D          ;Skip any leading blanks
        INX D
        CPI ' '
        JZ UD2B
        DCX D
        LXI H,0         ;Initialize result = 0
UD2B1:  LDAX D          ;Fetch next ascii character
        SUI 48          ;Convert char to BCD digit if possible
        MOV C,A         ;Save (character-48) in C
        RM              ;Is it a digit 0 thru 9...
        CPI 10
        RP              ;...If not, then exit
        PUSH D          ;...If so, save buffer pointer
        LXI D,10        ;...Multiply partial result by 10...
        CALL UMUL       ;...(also) checking for overflow...
        MVI D,0         ;...and add in value of digit...
        MOV E,C
        CALL UADD       ;HL = HL*10 + digit
        POP D           ;Recall buffer pointer
        INX D           ;Bump buffer pointer
        JMP UD2B1       ;...and we're ready for next character


;------------------------------- UB2D ---------------------------------
; Convert binary number to ASCII decimal
; On call: HL = binary number, DE = ptr to string location
; On retn: string 0 terminated, A = number of characters.
;----------------------------------------------------------------------
UB2D:   PUSH B
        PUSH D
        PUSH H
        MVI C,0         ;C=digit counter
        PUSH D          ;Save a copy of ptr
UB2D1:  PUSH D          ;Save DE ptr prior to divide
        LXI D,10
        CALL UDIV       ;HL = HL / 10
        MOV A,E         ;E = remainder
        ADI 48          ;Convert to ASCII char
        POP D           ;Recall DE ptr
        STAX D          ;Move char to string
        INX D           ;Inc ptr
        INR C           ;Inc counter
        MOV A,H         ;If HL > 0...
        ORA L
        JNZ UB2D1       ;...loop
        SUB A
        STAX D          ;Move 0 to end of string
        INR C           ;Inc digit count
;Reverse string
        DCX D           ;Ptr to end of string
        POP H           ;Ptr to beginning of string
        PUSH B          ;Save a copy of digit count
        DCR C
UB2D2:  MOV B,M         ;Leading char to temp
        LDAX D          ;Get next trailing char
        MOV M,A         ;...move it
        MOV A,B         ;Get leading char
        STAX D          ;...move it
        INX H           ;Update pointers
        DCX D
        MOV A,C         ;Decrement count by 2
        SUI 2
        JZ UB2D3        ;Check whether done
        JC UB2D3
        MOV C,A
        JMP UB2D2       ;Loop
UB2D3:  POP B           ;Recall digit count
        MOV A,C         ;A = count
        POP H
        POP D
        POP B
        RET

