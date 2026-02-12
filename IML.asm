;**********************************************************************
;                  8080/8085 INTEGER MATH LIBRARY
; The Integer Math Library (IML) contains basic integer subroutines
; and functions.  The operations provided are: addition, subtraction,
; multiplication, division, value comparison, negation, absolute value,
; and conversion between decimal and binary integer representations.
;
; IML procedures operate on single precision integer binary numbers
; in the range: [-32767, +32767].  The value 8000H is NaN.
;
; Procedures & execution times (2 MHz 8080)
;   IADD  HL = HL + DE          20 to 29 uS
;   ISUB  HL = HL - DE          56 to 59 uS
;   IMUL  HL = HL * DE          150 to 553 uS
;   IDIV  HL = HL / DE          330 to 3082 uS
;   IABS  HL = |HL|
;   INEG  HL = -HL
;   ICMP  S & Z FLAGS SET TO REFLECT HL - DE
;   IFLG  S & Z FLAGS SET TO REFLECT HL
;   ID2B  DECIMAL NUMBER IS CONVERTED TO BINARY
;   IB2D  BINARY NUMBER IS CONVERTED TO DECIMAL
;
; Author: Leonard Visser
;**********************************************************************

ERROF:  HLT             ;Add code here to handle overflow errors
ERRCV:  HLT             ;Add code here to handle conversion errors


;------------------------------- IADD ---------------------------------
; Integer addition.
; On call: Registers DE and HL contain the addends.
; On retn: HL contains the result.
;----------------------------------------------------------------------
IADD:   MOV A,D         ;Clear Z flag if addend signs differ
        XRA H
        ANI 80H
        DAD D           ;Add without affecting Z flag
        RNZ             ;If signs differ, no need to check for overflow
        RAR             ;If signs same then XOR CY with sign of result
        XRA H           ;Now A is negative if overflow
        CM ERROF        ;If overflow, then call error handler
        RET


;------------------------------- ISUB ---------------------------------
; Integer subtraction.
; On call: Register HL contains minuend and DE contains subtrahend.
; On retn: HL contains the result.
;----------------------------------------------------------------------
ISUB:   PUSH  B
        MOV A,D         ;XOR signs of operands...
        XRA H
        MOV B,A         ;...and save in msb of B
        MOV A,L
        SUB E           ;Subtract LSBs without borrow
        MOV L,A
        MOV A,H
        SBB D           ;Subtract MSBs with borrow
        MOV H,A         ;Now HL = result
        RAR
        MOV C,A         ;Save CY in msb of C
        MOV A,B         ;Get XOR of operand signs
        ANI 80H
        JZ ISUB1        ;If signs same, no need to check for overflow
        MOV A,C         ;Exclusive or CY with sign of result
        XRA H           ;Now A msb = 0 if overflow
        JP ERROF        ;If overflow, then call error handler
ISUB1:  POP B
        RET


;------------------------------- IMUL ---------------------------------
; Integer multiplication.
; On call: Registers DE and HL contain the multiplicands.
; On retn: HL contains the result.
;----------------------------------------------------------------------
IMUL:   PUSH B
        PUSH D
        CALL ISWP       ;Compute B=sign of result, abs(swap(DE,HL))
        MOV A,H         ;If HL > 255...
        ORA A
        JZ IMUL1
        MOV A,D         ;and DE is > 255...
        ORA A
        CNZ ERROF       ;...then overflow - call error handler
        XCHG            ;HL is now < 256
IMUL1:  MOV A,L         ;Move small multiplier to A
        LXI H,0         ;Initialize partial product
IMUL2:  STC
        CMC
        RAR             ;Rotate multiplier lsb into CY
        JNC IMUL3       ;If bit = 0 then skip
        DAD D           ;else, add multiplicand to partial product
        CC  ERROF       ;If overflow then call error handler
IMUL3:  XCHG
        DAD H           ;Shift multiplicand left 1 bit
        CC ERROF        ;If overflow then call error handler
        XCHG
        ORA A
        JNZ IMUL2       ;If multiplier > 0 then loop
        POP D           ;...else drop thru to sign result
; Compute final sign of result.
IFNL:   MOV A,H         ;If result > 32767...
        RLC
        CC  ERROF       ;...then overflow - call error handler
        MOV A,B         ;Get sign bit
        RAL
        CC  INEG        ;Change sign if negative
        POP B
        RET


;----------------------------------------------------------------------
; Compute result sign for * and /.  Return with |DE| and |HL| swapped.
;----------------------------------------------------------------------
ISWP:   MOV B,H         ;Move sign of 1st operand to B msb...
        MOV A,H
        RAL             ;...and also into CY
        CC  INEG        ;If negative the form absolute value |HL|
        XCHG            ;Swap DE and HL
        MOV A,H         ;Move sign of 2nd operand to A
        XRA B           ;...and exclusive or them
        MOV B,A         ;Save to B msb
        MOV A,H         ;Move sign of 2nd operand to A
        RAL             ;Rotate into CY
        JC INEG         ;If negative the form absolute value & return
        RET


;------------------------------- IDIV ---------------------------------
; Integer division.
; On call: HL = dividend, DE = divisor.
; On retn: HL = quotient, DE = remainder.
;----------------------------------------------------------------------
IDIV:   PUSH B
        MOV A,D         ;Check for divide by zero
        ORA E
        CZ  ERROF       ;If zero, then call error handler
        CALL ISWP       ;Compute result sign in B and swap(|DE|&|HL|)
        PUSH B          ;Save result sign byte
        MOV C,E         ;Move dividend (REMainder) to BC
        MOV B,D
        LXI D,0         ;Initialize QUOtient on Top Of Stack (TOS)
        PUSH D
        XCHG            ;Now BC = REM, DE=DIV, TOS=QUO
        LXI H, 1        ;Initialize bit POSition = 1
IDIV1:  DAD H           ;Left shift POS
        XCHG            ;NOW BC=REM, DE=POS, HL=DIV, TOS=QUO
        DAD H           ;Left shift DIV
        CALL IDIVC      ;Compare DIV to REM
        XCHG            ;Now BC=REM, DE=DIV, HL=POS, TOS=QUO
        JNC IDIV1       ;Loop if DIV < REM
        XCHG
IDIV2:  XCHG            ;Now BC=REM, DE=DIV, HL=POS, TOS=QUO
        CALL IDIVR      ;Right shift POS
        JZ IDIV3        ;IF POS = 0, we are done
        XCHG            ;Now BC=REM, DE=POS, HL=DIV, TOS=QUO
        CALL IDIVR      ;Right shift DIV
        CALL IDIVC      ;Compare DIV to REM...
        JM IDIV2        ;Loop if DIV > REM
        MOV A,C         ;REM = REM - DIV
        SUB L
        MOV C,A
        MOV A,B
        SBB H
        MOV B,A
        XTHL            ;Now BC=REM, DE=POS, HL=QUO, TOS=DIV
        DAD D           ;QUO = QUO + POS
        XTHL            ;Now BC=REM, DE=POS, HL=DIV, TOS=QUO
        JMP IDIV2       ;Loop
IDIV3:  POP H           ;HL = QUOtient from TOS
        MOV E,C         ;DE = REMainder
        MOV D,B
        POP B           ;Recall sign byte for result
        JMP IFNL        ;Compute final sign of result and return
;Compare BC to HL.  Flags S and Z set per result of BC - HL
IDIVC:  MOV A,C
        SUB L
        MOV A,B
        SBB H           ;Sign, Zero now reflect BC - HL
        RET
;Right shift HL, remainder returned in CY
IDIVR:  XRA A
        MOV A,H
        RAR
        MOV H,A
        MOV A,L
        RAR
        MOV L,A
        ORA H           ;Set Z flag if both H and L = 0
        RET


;------------------------------- INEG ---------------------------------
; Integer negation.  Returns -HL.  Note -(-32768) causes overflow.
;----------------------------------------------------------------------
INEG:   MOV A,H         ;If HL = -32768...
        CPI 80H
        JNZ INEG2
        MOV A,L
        ORA A
        CZ ERROF        ;...then call error handler
INEG2:  MOV A,H         ;Form 2s complement of HL
        CMA             ;Complement H
        MOV H,A
        MOV A,L
        CMA             ;Complement L
        MOV L,A
        INX H           ;Add 1
        RET


;------------------------------- ICMP ---------------------------------
; Integer compare.  Flags S and Z set per result of HL - DE
;----------------------------------------------------------------------
ICMP:   MOV A,L
        SUB E
        MOV A,H
        SBB D
        RET


;------------------------------- IFLG ---------------------------------
; Flags S and Z set per contents of HL
;----------------------------------------------------------------------
IFLG:   SUB A           ;Clear Flags
        ADD H           ;Set flags per high byte
        RNZ             ;Done if non-zero
        ADD L           ;Else check if low byte is zero
        RZ              ;Done if zero
        SUB A           ;Else set S=0 and Z=0
        INR A
        RET


;------------------------------- ID2B ---------------------------------
; Convert decimal number to binary
; On call: DE = ptr to decimal number in ASCII
; On retn: HL = binary result, DE is advanced past decimal number.
;----------------------------------------------------------------------
ID2B:   PUSH B
        MVI B,0         ;Initialize flags
        LXI H,0         ;Initialize result
ID2B1:  LDAX D          ;Get next char of number
        SUI 48          ;Attempt to convert char to BCD digit (char-48)
        MOV C,A         ;C = (char-48)
        JM ID2B2        ;Skip if not a digit 0 thru 9
        CPI 10
        JP ID2B2
        PUSH D          ;Save ptr
        LXI D,10
        CALL IMUL       ;Multiply partial result by 10
        MVI D,0
        MOV E,C
        CALL IADD       ;Add digit to partial result
        POP D           ;Recall and increment ptr
        INX D
        MVI A,1         ;Set digit encountered flag in B
        ORA B
        MOV B,A
        JMP ID2B1       ;Loop back for next char
ID2B2:  MOV A,C         ;Recall (char-48)
        CPI 0F0H        ;Is it a blank?  If so, set Z flag
        MOV A,B         ;Recall flags
        RRC             ;Rotate digit encountered flag into CY
        JC IFNL         ;Exit if digits encountered prior
        JNZ ID2B3       ;...else, if not blank try + or -
        INX D           ;...else, ignore leading blank
        JMP ID2B1       ;Loop back for next char
ID2B3:  MOV A,B         ;If sign encountered flag is set...
        RLC
        RLC
        CC ERRCV        ;...then call error handler
        MOV A,C         ;Recall (char-48)
        CPI 0FDH        ;Is it "-"?
        JNZ ID2B4       ;...no, then test for "+" sign
        MVI A,0C0H      ;...yes - set sign encountered and minus flags
        ORA B
        MOV B,A
        INX D           ;Inc ptr
        JMP ID2B1       ;Loop back for next char
ID2B4:  CPI 0FBH        ;Is it "+"?
        CNZ ERRCV       ;...no, then call error handler
        MVI A,40H       ;...yes - set sign encountered flag
        ORA B
        MOV B,A
        INX D           ;Inc ptr
        JMP ID2B1       ;Loop back for next char


;------------------------------- IB2D ---------------------------------
; Convert binary number to ASCII decimal
; On call: HL = binary number, DE = ptr to string location
; On retn: string 0 terminated, A = number of characters.
;----------------------------------------------------------------------
IB2D:   PUSH B
        PUSH D
        PUSH H
        LXI B,0         ;B=minus flag, C=digit counter
        MOV A,H
        ORA A           ;Is number negative?
        JP IB2D0        ;...no, then skip
        MOV B,A         ;Set minus flag
        MVI A,'-'       ;Move '-' to string
        STAX D
        INX D           ;Inc ptr
        CALL INEG       ;Get absolute value of HL
IB2D0:  PUSH D          ;Save a copy of ptr
IB2D1:  PUSH D
        LXI D,10
        CALL IDIV       ;HL = HL / 10
        MOV A,E         ;E = remainder
        ADI 48          ;Convert to ASCII char
        POP D
        STAX D          ;Move char to string
        INX D           ;Inc ptr
        INR C           ;Inc counter
        MOV A,H         ;If HL > 0...
        ORA L
        JNZ IB2D1       ;Loop
        SUB A
        STAX D          ;Move 0 to end of string
        INR C           ;Inc digit count
;Reverse string
        DCX D           ;Ptr to end of string
        POP H           ;Ptr to beginning of string
        PUSH B          ;Save a copy of minus flag and digit count
IB2D2:  MOV B,M         ;Leading char to temp
        LDAX D          ;Get next trailing char
        MOV M,A         ;...move it
        MOV A,B         ;Get leading char
        STAX D          ;...move it
        INX H           ;Update pointers
        DCX D
        MOV A,C         ;Decrement count by 2
        SUI 2
        JZ IB2D3        ;Check whether done
        JC IB2D3
        MOV C,A
        JMP IB2D2       ;Loop
IB2D3:  POP B
        MOV A,B         ;If negative, inc count to include '-'
        ORA A
        JP IB2D4
        INR C
IB2D4:  MOV A,C         ;A = count
        POP H
        POP D
        POP B
        RET

