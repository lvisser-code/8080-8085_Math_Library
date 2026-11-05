;**********************************************************************
;               8080/8085 FLOATING-POINT MATH LIBRARY
;
; Author: Leonard Visser
; Revision: 2026-11-01
;
; This module contains basic floating point subroutines and functions.
; Intended for retrocomputing, this module is not strictly IEEE 754 
; compliant.
;
; Floating-Point Format
;   Floating-point 32-bit numbers contain 3 fields. 
;   Sign (1 bit), exponent (8 bits) and significand (23 bits.)  
;   Exponent field bias is 127. Significand has a hidden leading 1 bit. 
;   Floating-point number examples (stored internally little-endian):
;       Decimal         Hexadecimal
;       +0              00000000
;       +1              3F800000
;       -1              BF800000
;       3.40x10^38      7F7FFFFF
;       1.17x10^-38     00800000
;       +infinity       7F800000
;       -infinity       FF800000
;   IEEE values not implemented are -0 and NaN.  Denormalized numbers
;   are not implemented.  Any result whose exponent becomes zero is
;   flushed to zero and sets the underflow error flag.
;
; Integer Format
;   Integers are signed 32-bit binary numbers.  Examples:
;       Decimal         Hexadecimal
;       +2.147x10^9     7FFFFFFF
;       +0              00000000
;       -1              FFFFFFFF
;       -2.147X10^9     80000000
;
; Internally, floating point numbers are unpacked and stored in a
; 19 byte floating point record (FPR) for computations. The upper
; 6 bytes of the FPR are the floating point accumulator (FAC).
; FAC mantissa is a 24-bit unsigned integer. Bit 23 is always 1 for 
; non-zero normalized values.  For zero, exp and bit 23 are 0.
; Rounding rule is round-to-nearest, ties to even.
; Floating-Point Record (FPR) Format:
;      12   S   -   -   -   -   -   -   st    FAC-S=sign, st=sticky bit
;      11   e7  e6  e5  e4  e3  e2  e1  e0    FAC-exponent
;      10   f23 f22 f21 f20 f19 f18 f17 f16   FAC \
;      F    f15 f14 f13 f12 f11 f10 f9  f8    FAC -fraction
;      E    f7  f6  f5  f4  f3  f2  f1  f0    FAC /
;FAC   D    g7  g6  g5  g4  g3  g2  g1  g0    FAC guard byte
;FTMP6 C    -
;FTMP5 B    -
;FTMP4 A    -
;FTMP3 9    -
;FTMP2 8    -
;FTMP1 7    -
;      6    S   -   -   -   -   -   -   st    ARG-S=sign, st=sticky bit
;      5    e7  e6  e5  e4  e3  e2  e1  e0    ARG-exponent
;      4    f23 f22 f21 f20 f19 f18 f17 f16   ARG \                                 
;      3    f15 f14 f13 f12 f11 f10 f9  f8    ARG -fraction
;      2    f7  f6  f5  f4  f3  f2  f1  f0    ARG /
;FARG  1    g7  g6  g5  g4  g3  g2  g1  g0    ARG guard byte
;FPR   0    E   G   L   --  --  EC  EC  EC    status field
;
; Status field bits E, G, and L act as flags following a comparison.
;   E = 1   if the FAC = second operand
;   G = 1   if the FAC > second operand
;   L = 1   if the FAC < second operand
; Status field EC (error condition) bits indicate an error
;   000 = No error
;   001 = attempted division by zero
;   010 = domain error (e.g. sqrt(-1.))
;   011 = overflow
;   100 = underflow
;   101 = invalid number in FAC
;   110 = invalid number in memory
;   111 = undefined error
;   Each routine clears the entire status field on entry.
;   FCMPR and FZTST set exactly one of E/G/L and leave EC=000.
;   unless an error occurs.  All other arithmetic routines 
;   leave E/G/L cleared and may set EC on error.
;   The following are not accepted as a function input: Infinity
;   and all inputs with exponent FFh - they will result in EC=101
;   or EC=110.  Attempted division 0/0 results in divide-by-zero error.
;   All calculation errors are considered unrecoverable resulting
;   in jump to user supplied error handler FERROR.  All registers
;   may be corrupted when FERROR is entered.
;
; Procedures to initialize and access the FPR fields:
;   FSET    A subroutine to clear the entire FPR. Must be called once
;           before any floating-point library routine is used.
;   FLOAD   A subroutine to load a floating-point number from memory
;           into the Floating-Point Accumulator (FAC) field of the FPR
;           Call with DE = address of the number
;   FSTOR   A subroutine to store a floating-point number from the FAC
;           into memory. Call with DE = memory address of the number
;   FSTAT   A function that copies the Status field of the FPR into
;           the accumulator
;
; Procedures for performing floating-point arithmetic:
;   FADD    FAC = FAC + mem(DE)   Add floating-point numbers
;   FSUB    FAC = FAC - mem(DE)   Subtract floating-point numbers
;   FMUL    FAC = FAC * mem(DE)   Multiply floating-point numbers
;   FDIV    FAC = FAC / mem(DE)   Divide floating-point numbers
;   FSQRT   FAC = sqrt(FAC)       Compute the square root
;   FQFD2B  FAC = float(mem(DE))  Decimal to Binary conversion
;           DE points to a zero-terminated string
;   FQFB2D  mem(DE) = str(FAC)    Binary to decimal conversion
;           DE points to a 14-byte buffer. Output is null-terminated ASCII
;   FIXSD   mem(DE) = FAC         Convert a floating-point number to an
;           integer.  Overflow causes an unrecoverable error 011
;   FLTDS   FAC = float(mem(DE))  Convert an integer to a float
;   FCMPR   FAC and mem(DE)       Compare two floating-point numbers
;           Clears the status field then sets exactly one of E/G/L.
;   FZTST   Compare the FAC to zero.  
;           Clears the status field then sets exactly one of E/G/L.
;   FNEG    FAC = -(FAC)          Negate (change) the sign of the FAC
;   FCLR    FAC = 0               Clear the FAC only to zero
;   FABS    FAC = abs(FAC)        Set the FAC to its absolute value
;
;   FERROR is a user-supplied entry point whose address must be defined
;   by the host application before any floating-point routine is called.
;
; Helper routines
;   FALGN   FAC, ARG              Align mantissas so exponents match
;   FMADD   FAC, ARG              Add FAC = FAC + ARG (CY out)
;   FMSUB   FAC, ARG              Subtract FAC = FAC - ARG
;   FNORM   FAC, sticky bit, CY   Normalize the mantissa after addition
;   FROUND  FAC, sticky bit       Round-to-nearest, ties to even
;   FMMUL   FAC, ARG              Multiply FAC = FAC * ARG
;   FMNOR   FAC, FTMP             Normalize product from FMMUL
;   FMDIV   FAC, ARG              Divide FAC = FAC / ARG
;   FNORD   FAC, FTMP             Normalize quotient from FMDIV
;   FSWAP   mem(HL), mem(DE)      Swap unpacked floats
;
; Unless otherwise stated: A and flags are trashed.  BC,DE,HL preserved
;**********************************************************************

;Reserve 19 bytes for the Floating-Point Record - must be in RAM
        ORG 4000H
FPR:    DS 19
FARG:   EQU FPR+1   ;Ptr to 6 byte argument area of FPR
FTMP1:  EQU FPR+7   ;Ptr to temp1
FTMP2:  EQU FPR+8   ;Ptr to temp2
FTMP3:  EQU FPR+9   ;Ptr to temp3
FTMP4:  EQU FPR+10  ;Ptr to temp4
FTMP5:  EQU FPR+11  ;Ptr to temp5
FTMP6:  EQU FPR+12  ;Ptr to temp6
FAC     EQU FPR+13  ;Ptr to 6 byte accumulator area of FPR

        ORG 0000H
FERROR: HLT         ;Replace with your error handler

;Attempted division by zero error entry
FERRDZ: MVI A, 001B ;Set error condition bits
        STA FPR
        JMP FERROR

;Domain error entry
FERRDE: MVI A, 010B ;Set error condition bits
        STA FPR
        JMP FERROR

;Overflow error entry
FERROF: MVI A, 011B ;Set error condtion bits
        STA FPR
        JMP FERROR

;Underflow error entry
FERRUF: CALL FCLR   ;Clear the FAC to zero
        MVI A, 100B ;Set error condition bits
        STA FPR
        JMP FERROR

;Invalid number in FAC error entry
FERRIF: MVI A, 101B ;Set error condition bits
        STA FPR
        JMP FERROR

;Invalid number in memory error entry
FERRIM: MVI A, 110B ;Set error condition bits
        STA FPR
        JMP FERROR

;Undefined error
FERRUN: MVI A, 111B ;Set error condition bits
        STA FPR
        JMP FERROR

;------------------------------- FSET ---------------------------------
; Clear the Floating-Point Record (FPR)
; On call: no params
; On retn: FPR cleared
;----------------------------------------------------------------------
FSET:   PUSH H
        PUSH B
        LXI H, FPR  ;HL = ptr to FPR
        MVI C, 16   ;Counter
        SUB A       ;A = 0
FSET1:  MOV M, A    ;Write 0 to location in FPR
        INX H       ;Inc ptr to next byte
        DCR C       ;Dec count
        JNZ FSET1   ;Loop until count = 0
        POP B
        POP H
        RET

;-------------------------------- FLOAD -------------------------------
; Load a floating-point number from memory into the Floating-Point
; Record (FPR) accumulator FAC.
; On call: DE = address of the number
; On retn: unpacked number is in FAC
;----------------------------------------------------------------------
FLOAD:  PUSH B      ;Loads into the default FAC location
        PUSH D
        LXI B, FAC  ;BC = ptr to FAC
        CALL FLOADB ;Load the number pointed to by DE into FPR
        POP D
        POP B
        RET

;Entry point requires BC ptr to FPR accumulator FAC or ARG
;Destroys A, BC, DE and flags.
FLOADB: SUB A       ;A = 0
        STA FPR     ;Clear the status field
        STAX B      ;Set guard byte = 0
        INX B       ;Inc B destination ptr to fraction
        LDAX D      ;Copy f0-f7 to FPR
        STAX B
        INX B       ;Inc pointers B & D
        INX D
        LDAX D      ;Copy f8-f15 to FPR
        STAX B
        INX B       ;Inc pointers B & D
        INX D
        LDAX D      ;Copy f16-f22 (bits 0-6) plus implicit 1 to FPR
        ORI 80H     ;...set f23 (bit 7) to 1
        STAX B
        LDAX D      ;Reload byte containing e1 in msb position
        RLC         ;...rotate e1 into CY
        INX B       ;Inc pointers B & D
        INX D
        LDAX D      ;Load exponent bits e2-e8 plus sign bit
        RAL         ;Rotate e1 in from CY and sign into CY
        STAX B      ;Copy e1-e8 to FPR
        INX B
        RAR         ;Rotate sign from CY to msb of accumulator
        ANI 80H     ;...mask off 7 least significant bits, sticky to 0
        STAX B      ;Copy sign to FPR
        DCX B       ;Dec B back to exponent
        LDAX B      ;Check for zero exponent indicating number = zero
        ORA A
        RNZ         ;Exit if non-zero
        INX B       ;Clear ARG or FAC to zero
        MVI E, 6
        SUB A
FLOADD: STAX B      ;Write six 0 bytes
        DCX B
        DCR E
        JNZ FLOADD
        RET

;--------------------------------- FSTOR ------------------------------
; Store a floating-point number from the Floating-Point Accumulator
; (FAC) into memory
; On call: DE = destination address of the number
; On retn: number is in memory
;----------------------------------------------------------------------
FSTOR:  PUSH B
        PUSH D
        PUSH H
        LXI B,FAC+1 ;BC = ptr to FAC fraction bytes
        LDAX B      ;Copy f0-f7 of FAC to memory pointed to  by DE
        STAX D
        INX B       ;Inc pointers B & D
        INX D
        LDAX B      ;Copy f8-f15 of FAC to memory
        STAX D
        INX B       ;Inc pointers B & D
        INX D
        LDAX B      ;Load f16-f23
        ANI 7FH     ;...strip off msb (implicit bit)
        MOV L, A    ;...and save result in L
        INX B       ;Inc B pointer
        LDAX B      ;Load exponent
        ANI 1       ;...mask off all but e1
        RRC         ;...rotate e1 from lsb to msb
        ORA L       ;...combine f16-f22 with e1
        STAX D      ;Copy f16-f22 plus e1 to memory
        INX D       ;Inc D pointer
        LDAX B      ;Copy e2-e7 plus sign to memory
        RRC         ;...rotate exponent right
        ANI 7FH     ;...mask off what was e1
        MOV L, A    ;...and save result in L
        INX B
        LDAX B      ;Load sign and mask off low 7 bits
        ANI 80H
        ORA L       ;...combine sign with e2-e7
        STAX D
        POP H
        POP D
        POP B
        RET

;------------------------------- FSTAT --------------------------------
; Place the contents of the Status field into the accumulator
;----------------------------------------------------------------------
FSTAT:  LDA FPR     ;A = FPR Status Field
        RET


;-------------------------------- FADD --------------------------------
; Add two floating-point numbers
; On call: FAC = 1st addend, DE = ptr to second addend in memory.
; On retn: FAC = sum
;----------------------------------------------------------------------
FADD:   PUSH B      ;Save registers
        PUSH D
        PUSH H
        SUB A       ;A = 0
        STA FPR     ;Clear entire status field

;Load second addend from memory into ARG, check arguments for infinity
        LXI B,FARG  ;BC = ptr to ARG, DE = ptr to 2nd addend
        CALL FLOADB ;Load number
FADDE:  LDA FAC+4   ;Check FAC for infinity (exponent = FFh)
        INR A
        JZ FERRIF   ;Error handler for invalid number in FAC
        LDA FARG+4  ;Check ARG for infinity
        INR A
        JZ FERRIM   ;Error hander for invalid number in memory

;Handle trivial case of a zero addend
        LDA FARG+4  ;Check for ARG exponent = 0
        ORA A
        JZ FADDX    ;For ARG = 0 we are done
        LDA FAC+4   ;Check for FAC exponent = 0
        ORA A
        JNZ FADD1   ;Branch if FAC != 0
        LXI H, FAC  ;For FAC = 0 swap ARG into FAC
        LXI D, FARG
        CALL FSWAP  ;Swap ARG & FAC
        JMP FADDX   ;We are done

;Align exponents
FADD1:  CALL FALGN

;Check signs and either add or subtract
        LDA FAC+5   ;Get FAC sign
        ANI 80H
        MOV B,A
        LDA FARG+5  ;Get ARG sign
        ANI 80H
        CMP B
        JNZ FADD2   ;If signs differ subtract, else add
        CALL FMADD  ;Add the mantissas
        JMP FADD6
;Subtract requires larger argument in FAC
FADD2:  LXI D, FAC+3
        LXI H, FARG+3
        CALL FCMP   ;Compare FAC to ARG, CY=1 if FAC < ARG
        JZ FADD4    ;Branch if they are equal
        JNC FADD3
        LXI H,FAC
        LXI D,FARG
        CALL FSWAP  ;Swap arguments so FAC > ARG
FADD3:  CALL FMSUB  ;Subtract the mantissas
        JMP FADD5

FADD4:  CALL FCLR   ;Clear FAC to zero
        JMP FADDX   ;Done

;Normalize the FAC
FADD5:  ORA A       ;Clear CY on subtract
FADD6:  CALL FNORM  ;Uses CY from addition

;Round the FAC
        CALL FROUND

;Exit when done
FADDX:  POP H       ;Restore registers
        POP D
        POP B
        RET


;-------------------------------- FSUB --------------------------------
; Subtract two floating-point numbers
; On call: FAC = minuend, DE = ptr to subtrahend in memory.
; On retn: FAC = difference
;----------------------------------------------------------------------
FSUB:   PUSH B
        PUSH D
        PUSH H
        LXI B, FARG
        CALL FLOADB ;Load subtrahend in to ARG
        LDA FARG+5  ;Load ARG sign (b7)
        XRI 80H     ;Flip the sign bit only
        STA FARG+5
        JMP FADDE   ;Branch to add routine


;-------------------------------- FMUL --------------------------------
; Multiply two floating-point numbers
; On call: FAC = multiplier, DE = ptr to multiplicand in memory.
; On retn: FAC = product
;----------------------------------------------------------------------
FMUL:   PUSH B      ;Save registers
        PUSH D
        PUSH H
        SUB A       ;A = 0
        STA FPR     ;Clear entire status field

;Load multiplicand from memory into ARG, check arguments for infinity
        LXI B,FARG  ;BC = ptr to ARG, DE = ptr to multiplicand
        CALL FLOADB ;Load number
        LDA FAC+4   ;Check FAC for infinity (exponent = FFh)
        INR A
        JZ FERRIF   ;Error hander for invalid number in FAC
        LDA FARG+4  ;Check ARG for infinity
        INR A
        JZ FERRIM   ;Error hander for invalid number in memory

;Handle trivial zero operand cases
        LDA FAC+4   ;Check for FAC exponent = 0
        ORA A
        JZ FMULX    ;For FAC = 0 we are done
        LDA FARG+4  ;Check for FARG exponent = 0
        ORA A
        JNZ FMUL1   ;Branch if FARG != 0
        CALL FCLR   ;Clear FAC to zero
        JMP FMULX   ;We are done

;Sign calculation
FMUL1:  LDA FAC+5   ;Get FAC sign
        MOV B,A
        LDA FARG+5  ;Get ARG sign
        XRA B       ;Xor of sign bit (b7)
        ANI 80H
        STA FAC+5   ;Save result sign

;Multiply mantissas
        CALL FMMUL

;Normalize product and compute final exponent
        CALL FMNOR

;Round
        CALL FROUND

FMULX:  POP H       ;Restore registers
        POP D
        POP B
        RET


;-------------------------------- FDIV --------------------------------
; Divide two floating-point numbers
; On call: FAC = dividend, DE = ptr to divisor in memory.
; On retn: FAC = quotient
;----------------------------------------------------------------------
FDIV:   PUSH B      ;Save registers
        PUSH D
        PUSH H
        SUB A       ;A = 0
        STA FPR     ;Clear entire status field

;Load divisor from memory into ARG, check arguments for infinity
        LXI B,FARG  ;BC = ptr to ARG, DE = ptr to multiplicand
        CALL FLOADB ;Load number
        LDA FAC+4   ;Check FAC for infinity (exponent = FFh)
        INR A
        JZ FERRIF   ;Error hander for invalid number in FAC
        LDA FARG+4  ;Check ARG for infinity
        INR A
        JZ FERRIM   ;Error hander for invalid number in memory

;Test for zero dividend or divisor 
        LDA FARG+4  ;Check for FARG exponent = 0
        ORA A
        JZ FERRDZ   ;Divide by zero error
        LDA FAC+4   ;Check for FAC exponent = 0
        ORA A
        JZ FDIVX    ;For FAC = 0 we are done

;Compute result sign
        LDA FAC+5   ;Get FAC sign
        MOV B,A
        LDA FARG+5  ;Get ARG sign
        XRA B       ;Xor of sign bit (b7)
        ANI 80H
        STA FAC+5   ;Save result sign

;Divide mantissas
        CALL FMDIV

;Normalize quotient and compute final exponent
        CALL FNORD

;Round result
        CALL FROUND

FDIVX:  POP H       ;Restore registers
        POP D
        POP B
        RET


;-------------------------------- FSQRT -------------------------------
; Compute the square root of a floating-point number, X using Newton's
; method with 4 iterations.
; On call: FAC = radicand number
; On retn: FAC = square root of number
;----------------------------------------------------------------------
FSQRT:  PUSH B      ;Save registers
        PUSH D
        PUSH H
        SUB A       ;A = 0
        STA FPR     ;Clear entire status field
;Validate number in FAC
        LDA FAC+4   ;Get X exponent
        INR A       ;If exponent = FF then error
        JZ FERRIF
        LDA FAC+5   ;Get X sign
        ANI 80H     ;If sign is negative then error
        JNZ FERRDE
        LDA FAC+4   ;Get X exponent
        ORA A       ;If X = 0 then return +0.0
        JNZ FSQRT1
        CALL FCLR
        JMP FSQRTX  ;Exit
;Allocate a 4-byte scratch area on the stack for Y, DE = ptr to Y
FSQRT1: LXI H,-4
        DAD SP      ;HL = SP - 4
        SPHL        ;SP = SP - 4
        XCHG        ;DE = ptr to y on stack
;Copy X to registers HL/BC
        LHLD FAC+1  ;Set BC = mantissa f15-f0
        MOV C,L
        MOV B,H
        LHLD FAC+3  ;Set H=exponent, L=mantissa f23-f16
;Calculate initial guess for Y: exponent = floor((Xexp + 127) / 2)
;  mantissa = 1.5 if Xexp is even, 1.0 if Xexp is odd
        MOV A,H     ;X exponent
        ADI 127     ;E + 127, CY = bit 8
        RAR         ;divide 9-bit result by 2
        STA FAC+4   ;Save Y exponent in FAC
        SBB A       ;A = FFH IF CY else 00H
        ANI 40H
        ADI 80H     ;A = C0H (1.5) or 80H (1.0)
        STA FAC+3   ;Save Y mantissa in FAC
        SUB A
        STA FAC+2
        STA FAC+1
        STA FAC     ;Save guard byte (0)
        STA FAC+5   ;Save sign and sticky (0)
        CALL FSTOR  ;mem(DE) = Y (stack scratch)
;Newton iteration loop
        MVI A,4     ;Iteration counter
        PUSH PSW    ;Save counter on stack
FSQRT2: SUB A
        STA FAC     ;Clear guard and sign/sticky
        STA FAC+5
        MOV A,C     ;FAC = X (from HL/BC)
        STA FAC+1
        MOV A,B
        STA FAC+2
        SHLD FAC+3
        CALL FDIV   ;FAC = X/Y (Y=mem(DE))
        CALL FADD   ;FAC = FAC+Y (Y=mem(DE))
        LDA FAC+4   ;Decrement exponent
        DCR A
        STA FAC+4   ;FAC = FAC/2
        POP PSW
        DCR A    
        JZ FSQRT3   ;If count=0 then exit loop
        PUSH PSW
        CALL FSTOR  ;Y at mem(DE) = FAC
        JMP FSQRT2
FSQRT3: POP PSW     ;Free stack scratch area
        POP PSW
FSQRTX: POP H       ;Restore registers
        POP D
        POP B
        RET


;------------------------------- FCMPR --------------------------------
; Compare a floating point number in FAC to one in memory.
; On call: DE = ptr to low byte of number in memory.
; On retn: One of the following bit patterns in reg A and status field
;          10000000  FAC = number in memory
;          01000000  FAC > number in memory
;          00100000  FAC < number in memory
;----------------------------------------------------------------------
FCMPR:  PUSH H
        PUSH D
        PUSH B
        SUB A       ;A = 0
        STA FPR     ;Clear entire status field
        LXI B, FARG ;BC = ptr to FARG (operand area)
        CALL FLOADB ;Load memory number mem(DE) into FARG
FCMPR1: LDA FAC+5   ;A = FAC sign in msb (0=pos, 80H=neg)
        ANI 80H
        MOV B,A     ;B = FAC sign
        LDA FARG+5  ;A = FARG sign
        ANI 80H
        CMP B       ;Compare signs
        JNZ FCMPR3  ;Branch if signs differ
;Signs same: compare magnitudes using FCMP (exponent first)
        LXI D, FAC+4 ;DE = ptr to FAC exponent
        LXI H, FARG+4 ;HL = ptr to FARG exponent
        CALL FCMP   ;Unsigned compare; Z=equal, CY=|FAC| < |FARG|
        JZ FCMPRE   ;Branch if equal
        JNC FCMPR3  ;|FAC| > |FARG|: same result as signs differ
        MOV A,B     ;|FAC| < |FARG|: reverse it
        ORA A
        JZ FCMPRL   ;Positive: FAC < FARG
        JMP FCMPRG  ;Negative: FAC > FARG
;Signs differ: positive number is larger
FCMPR3: MOV A,B     ;A = FAC sign
        ORA A
        JZ FCMPRG   ;If FAC positive, FARG negative => FAC > 
        JMP FCMPRL  ;FAC negative, FARG positive => FAC <
;Set flags in status field and in reg A
FCMPRE: MVI A,80H   ;FAC = second operand
        JMP FCMPRS
FCMPRG: MVI A,40H   ;FAC > second operand
        JMP FCMPRS
FCMPRL: MVI A,20H   ;FAC < second operand
FCMPRS: STA FPR     ;Set status field
        POP B
        POP D
        POP H
        RET

;-------------------------------- FCMP --------------------------------
; Compare two 32 bit integers X and Y.
; On call: DE ptr to high byte of X, HL = ptr to high  byte of Y
; On retn: Z set if equal, CY set if X < Y.  Regs DE, HL trashed.
;----------------------------------------------------------------------
FCMP:   LDAX D
        CMP M       ;Compare high bytes
        RNZ
        DCX H
        DCX D
        LDAX D
        CMP M
        RNZ
        DCX H
        DCX D
        LDAX D
        CMP M
        RNZ
        DCX H
        DCX D
        LDAX D
        CMP M       ;Compare low bytes
        RET

;-------------------------------- FZTST -------------------------------
; Compare the FAC to zero
; On call: no params
; On retn: One of the following bit patterns in reg A and status field
;          10000000  FAC = 0
;          01000000  FAC > 0
;          00100000  FAC < 0
;----------------------------------------------------------------------
FZTST:  LXI B,FARG  ;B = ptr to FARG
        MVI L, 6
        SUB A       ;A = zero
FZTST1: STAX B      ;Fill FARG with zeros
        INX B
        DCR L
        JNZ FZTST1
        JMP FCMPR1  ;Now compare FAC to zero

;--------------------------------- FABS -------------------------------
; Set the FAC to its absolute value
; On call: no params
; On retn: FAC sign bit = 0
;----------------------------------------------------------------------
FABS:   SUB A       ;A = 0
        STA FAC     ;Clear entire status field
        LDA FAC+5   ;A = FAC Sign and sticky bits
        ANI 7FH     ;Clear sign bit
        STA FAC+5   ;Write result to FAC
        RET

;--------------------------------- FCLR -------------------------------
; Clear the 6 byte FAC to zero
; On call: no params
; On retn: FAC = zero
;----------------------------------------------------------------------
FCLR:   PUSH H
        PUSH B
        SUB A       ;A = 0
        STA FAC     ;Clear entire status field
        LXI H, FAC  ;HL = ptr to FAC
        MVI C, 6
FCLR0:  MVI M, 0    ;Write six 0 bytes in FAC
        INX H
        DCR C
        JNZ FCLR0
        POP B
        POP H
        RET

;--------------------------------- FNEG -------------------------------
; Negate (change) the sign of the FAC (unless the number is zero)
; On call: no params
; On retn: FAC = -FAC
;----------------------------------------------------------------------
FNEG:   SUB A       ;A = 0
        STA FAC     ;Clear entire status field
        LDA FAC+4   ;Test for FAC exponent = 0
        ORA A
        RZ          ;Return if 0 without changing sign
        LDA FAC+5   ;A = Sign and Sticky bits
        XRI 80H     ;Toggle the sign bit
        STA FAC+5   ;Save result
        RET

;--------------------------------- FALGN ------------------------------
; Shift the operand with the smaller exponent right til exponents match
; On call: mem(FAC), mem(ARG)
; On retn: Exponents equal, guard byte updated, sticky bit updated
;----------------------------------------------------------------------
FALGN:  PUSH B
        PUSH D
        PUSH H
        SUB A       ;A=0
        STA FAC     ;Clear FAC guard byte
        STA FARG    ;Clear ARG guard byte
        LDA FAC+5   ;Clear FAC sticky bit
        ANI 0FEH
        STA FAC+5
        LDA FARG+5  ;Clear ARG sticky bit
        ANI 0FEH
        STA FARG+5
;Compare exponents
        LDA FARG+4  ;C = Exponent of ARG
        MOV C, A
        LDA FAC+4   ;A = Exponent of FAC
        SUB C       ;Compare the exponents
        JZ FALGNX   ;If exponents equal then Done
        JC FALGN1   ;If exponent FAC < ARG then branch
;ARG exponent is smaller
        MOV B,A     ;B = shift count
        LDA FARG+4  ;Adjust ARG Exponent
        ADD B
        STA FARG+4
        LXI H,FARG  ;HL is ptr to ARG guard byte
        JMP FALGN2
;FAC exponent is smaller
FALGN1: CMA
        INR A
        MOV B,A     ;B = shift count
        LDA FAC+4   ;Adjust FAC Exponent
        ADD B
        STA FAC+4
        LXI H,FAC   ;HL is ptr to  FAC guard byte
FALGN2: CALL FSHFT  ;Shift right by B bits
FALGNX: POP H
        POP D
        POP B
        RET

;------------------------------- FSHFT --------------------------------
; Shift unpacked mantissa+guard right by B bits (B = 1..255)
; On call: HL = ptr to record (guard byte), B = shift count
; On retn: mantissa/guard shifted, lost bits ORed into guard bit 0
; Destroys A,B,C,D,E,H,L and flags
;----------------------------------------------------------------------
FSHFT:  MOV A,B
        CPI 32
        JNC FSH32       ;guard=1, mantissa=0
        PUSH H          ;save record ptr
        MOV E,M         ;E = guard
        INX H
        MOV D,M         ;D = LSB
        INX H
        MOV C,M         ;C = mid
        INX H
        MOV B,M         ;B = MSB
        MVI H,0         ;H = sticky flag / 5th rotate byte
        MOV L,A         ;L = count (A still holds it)
;Whole-byte shifts while count >= 8
FSBYT:  MOV A,L
        SUI 8
        JC FSBIT        ;fewer than 8 left
        MOV L,A
        MOV A,E         ;guard byte falls off the end:
        ORA A
        JZ FSB1
        MVI H,80H       ;...nonzero -> sticky flag, parked in bit 7
FSB1:   MOV E,D         ;guard <- LSB
        MOV D,C         ;LSB   <- mid
        MOV C,B         ;mid   <- MSB
        MVI B,0         ;MSB   <- 0
        JMP FSBYT
;Remaining 0-7 bit shifts, 40-bit chain B,C,D,E,H
FSBIT:  MOV A,L
        ORA A
        JZ FSDON
FSLP:   ORA A           ;CY = 0
        MOV A,B
        RAR
        MOV B,A
        MOV A,C
        RAR
        MOV C,A
        MOV A,D
        RAR
        MOV D,A
        MOV A,E
        RAR
        MOV E,A
        MOV A,H         ;lost bits enter H at bit 7
        RAR
        MOV H,A
        DCR L
        JNZ FSLP
;Store back and merge sticky
FSDON:  MOV A,H         ;nonzero if any bit was lost
        ORA A
        JZ FSD1
        MOV A,E
        ORI 1           ;jam sticky into guard bit 0
        MOV E,A
FSD1:   POP H
        MOV M,E         ;guard
        INX H
        MOV M,D         ;LSB
        INX H
        MOV M,C         ;mid
        INX H
        MOV M,B         ;MSB
        RET

FSH32:  MVI M,1         ;guard = 1 (jam), mantissa = 0
        INX H
        XRA A
        MOV M,A
        INX H
        MOV M,A
        INX H
        MOV M,A
        RET

;------------------------------- FROTL --------------------------------
; Rotate left mantissa+guard byte.
; On call: DE = ptr to guard byte
; On retn: mantissa+guard byte rotated left.
;----------------------------------------------------------------------
FROTL:  PUSH D
        ORA A       ;Clear CY
        LDAX D
        RAL         ;Rotate g7-g0
        STAX D
        INX D
        LDAX D
        RAL         ;Rotate f7-f0
        STAX D
        INX D
        LDAX D
        RAL         ;Rotate f15-f8
        STAX D
        INX D
        LDAX D
        RAL         ;Rotate f23-f16
        STAX D
        POP D
        RET

;------------------------------- FNORM --------------------------------
; Normalize FAC mantissa after addition
; On call: FAC sign, exponent, mantissa, guard, sticky, CY overflow
; On retn: FAC normalized
;----------------------------------------------------------------------
FNORM:  PUSH B
        PUSH D
        PUSH H
;Check for addition overflow: CY=1.  
;On overflow right shift once and increment exponent.
        JNC FNORM1      ;Branch if CY=0
        LXI H,FAC       ;HL -> guard byte
        MVI B,1         ;shift count = 1
        CALL FSHFT      ;lost bit is jammed into guard bit 0
        LDA FAC+3       ;Set f23
        ORI 80H
        STA FAC+3
        LDA FAC+4       ;Get exponent
        INR A
        STA FAC+4
        CPI 0FFH
        JZ FERROF       ;Exponent overflow
        JMP FNORMX

;Check for subtraction leading zero bytes
FNORM1: MVI C,0         ;Init exponent adjustment total to zero
        LDA FAC+3       ;Check mantissa MSB = 0
        ORA A
        JNZ FNORMB
        LHLD FAC+1      ;Move 3 bytes left overwriting mantissa MSB
        SHLD FAC+2
        LDA FAC
        STA FAC+1
        SUB A
        STA FAC         ;Write 0 in guard byte
        MVI C,8

        LDA FAC+3       ;Check mantissa MSB = 0
        ORA A
        JNZ FNORMB
        LHLD FAC+1      ;Move 2 bytes left overwriting mantissa MSB
        SHLD FAC+2
        SUB A
        STA FAC+1       ;Write 0 in f7-f0 byte
        MVI C,16

        LDA FAC+3       ;Check mantissa MSB = 0
        ORA A
        JNZ FNORMB
        LDA FAC+2       ;Move 1 byte left overwriting mantissa MSB
        STA FAC+3
        SUB A
        STA FAC+2       ;Write 0 in f15-f8
        MVI C,24

        LDA FAC+3       ;Check mantissa = 0
        ORA A
        JNZ FNORMB
        CALL FCLR       ;Clear FAC to zero
        JMP FNORMX      ;Done

;Check for subtraction leading zero bits.  If 0 then shift left.
FNORMB: LDA FAC+3       ;Get mantissa MSB
        ANI 80H         ;If msb is 1 then...
        JNZ FNORME      ;Exit loop
        LXI D, FAC      ;Ptr to FAC guard byte
        CALL FROTL      ;Shift left mantissa+guard byte
        INR C           ;Inc exponent adjustment total
        JMP FNORMB

;Reduce the exponent by adjustment total in reg C
FNORME: LDA FAC+4       ;Get exponent
        SUB C
        JC FNORMZ       ;Exponent < 0: underflow, flush to zero
        JZ FNORMZ       ;Exponent = 0: flush to zero
        STA FAC+4

FNORMX: POP H
        POP D
        POP B
        RET

FNORMZ: CALL FCLR       ;Silent flush to +0
        JMP FNORMX

;------------------------------- FROUND --------------------------------
; Round FAC using rule round-to-nearest, ties to even after arithmetic
; On call: FAC exponent, mantissa, guard byte, sticky bit
; On retn: FAC rounded.  Uses A, flags only
;----------------------------------------------------------------------
FROUND: LDA FAC     ;Get guard byte G
        CPI 80H
        RC          ;If G < 80H then truncate
        JNZ FROUP   ;If G > 80H then round up
        LDA FAC+5   ;G = 80H, tie: check sticky bit
        ANI 1
        JNZ FROUP   ;If sticky not 0 then round up
        LDA FAC+1   ;Else round to even
        ANI 1
        RZ          ;If mantissa lsb=0 then even, return
;Round up
FROUP:  LDA FAC+1   ;Inc mantissa
        ADI 1
        STA FAC+1
        LDA FAC+2
        ACI 0
        STA FAC+2
        LDA FAC+3
        ACI 0
        STA FAC+3
        RNC         ;No carry out of mantissa, then return
        MVI A,80H   ;Carry out: mantissa = 800000h, inc exponent
        STA FAC+3
        LDA FAC+4
        INR A
        STA FAC+4
        CPI 0FFH
        RNZ         ;Return if no exponent overflow
        JMP FERROF  ;Exponent = 0FFH: overflow error

;------------------------------- FMADD --------------------------------
; Add FAC and ARG 32-bit mantissa+guard fields
; On call: FAC mantissa/guard byte, ARG mantissa/guard byte
; On retn: FAC contains sum, CY=carry out
;----------------------------------------------------------------------
FMADD:  PUSH H
        PUSH D
        PUSH B
        LXI H,FAC   ;HL = ptr to FAC
        LXI D,FARG  ;DE = ptr to ARG
        MVI C,4     ;C = byte counter
        ORA A       ;Clear CY
FMADD1: LDAX D
        ADC M       ;Add byte
        MOV M,A     ;Save result
        INX H
        INX D
        DCR C       ;Dec C without affecting CY 
        JNZ FMADD1  ;Loop until C = 0
        POP B
        POP D
        POP H
        RET

;------------------------------- FMSUB --------------------------------
; Subtract smaller magnitude mantissa in ARG from larger in FAC
; On call: FAC mantissa/guard byte, ARG mantissa/guard byte
; On retn: FAC contains difference
;----------------------------------------------------------------------
FMSUB:  PUSH H
        PUSH D
        PUSH B
        LXI H,FARG  ;HL = ptr to ARG
        LXI D,FAC   ;DE = ptr to FAC
        MVI C,4     ;C = byte counter
        ORA A       ;Clear CY
FMSUB1: LDAX D
        SBB M       ;Subtract byte
        STAX D      ;Save result
        INX H
        INX D
        DCR C       ;Dec C without affecting CY 
        JNZ FMSUB1  ;Loop until C = 0
        POP B
        POP D
        POP H
        RET

;------------------------------- FMMUL --------------------------------
; Multiply mantissas in FAC and ARG to get 48-bit product
; On call: FAC mantissa (24-bit), ARG mantissa (24-bit)
; On retn: FTMP1..FTMP6 contains 48-bit product
;----------------------------------------------------------------------
FMMUL:  PUSH H      ;Save registers
        PUSH D
        PUSH B
;Multiply mantissas: A(@FARG) x B(@FAC) = R(@FTMP1)
        LXI H, 0    ;Init result to 0
        SHLD FTMP1
        SHLD FTMP1+2
        SHLD FTMP1+4
;--- STEP 1: Process Multiplicand Byte 0 (A0) ---
;    Bound analysis proves no carries can occur
        LDA  FARG+1
        MOV  E, A   ;E = A0
        LDA  FAC+1  ;A0 * B0 -> FTMP1,FTMP2
        MOV  H, A   ;H = B0
        CALL FMUL8  ;8 bit multiply (1of9)
        XCHG
        SHLD FTMP1  ;Save result to FTMP1,FTMP2 (no CY out)

        LDA  FARG+1
        MOV  E, A   ;E = A0
        LDA  FAC+2  ;A0 * B1 -> Add to FTMP2,FTMP3
        MOV  H, A   ;H = B1
        CALL FMUL8  ;8 bit multiply (2of9)
        LHLD FTMP2
        DAD  D      ;16 bit add
        SHLD FTMP2  ;no CY out

        LDA  FARG+1
        MOV  E, A   ;E = A0
        LDA  FAC+3  ;A0 * B2 -> Add to FTMP3, FTMP4
        MOV  H, A   ;H = B2
        CALL FMUL8  ;8 bit multiply (3of9)
        LHLD FTMP3
        DAD  D      ;16 bit add
        SHLD FTMP3  ;no CY out

; --- STEP 2: Process Multiplicand Byte 1 (A1) ---
;     At most one-byte carry propagation required
        LDA  FARG+2
        MOV  E, A   ;E = A1
        LDA  FAC+1  ;A1 * B0 -> Add to FTMP2, FTMP3
        MOV  H, A   ;H = B0
        CALL FMUL8  ;8 bit multiply (4of9)
        LHLD FTMP2
        DAD  D      ;16 bit add
        SHLD FTMP2
        JNC FMMUL1
        LXI  H,FTMP4
        INR M       ;Ripple carry into FTMP4

FMMUL1: LDA  FARG+2
        MOV  E, A   ;E = A1
        LDA  FAC+2  ;A1 * B1 -> Add to FTMP3, FTMP4
        MOV  H, A   ;H = B1
        CALL FMUL8  ;8 bit multiply (5of9)
        LHLD FTMP3
        DAD  D      ;16 bit add
        SHLD FTMP3
        JNC FMMUL2
        LXI  H,FTMP5
        INR M       ;Ripple carry into FTMP5

FMMUL2: LDA  FARG+2
        MOV  E, A   ;E = A1
        LDA  FAC+3  ;A1 * B2 -> Add to FTMP4, FTMP5
        MOV  H, A   ;H = B2
        CALL FMUL8  ;8 bit multiply (6of9)
        LHLD FTMP4
        DAD  D      ;16 bit add
        SHLD FTMP4  ;no CY out

; --- STEP 3: Process Multiplicand Byte 2 (A2) ---
        LDA  FARG+3
        MOV  E, A   ;E = A2
        LDA  FAC+1  ;A2 * B0 -> Add to FTMP3, FTMP4
        MOV  H, A   ;H = B0
        CALL FMUL8  ;8 bit multiply (7of9)
        LHLD FTMP3
        DAD  D      ;16 bit add
        SHLD FTMP3
        JNC FMMUL3
        MVI  A, 0   ;Ripple carry to FTMP5, FTMP6
        LXI  H, FTMP5
        ADC  M 
        MOV M, A
        INX H
        MOV  A, M
        ACI 0
        MOV M, A

FMMUL3: LDA  FARG+3
        MOV  E, A   ;E = A2
        LDA  FAC+2  ;A2 * B1 -> Add to FTMP4, FTMP5
        MOV  H, A   ;H = B1
        CALL FMUL8  ;8 bit multiply (8of9)
        LHLD FTMP4
        DAD  D      ;16 bit add
        SHLD FTMP4
        JNC FMMUL4
        LXI H,FTMP6
        INR M       ;Ripple carry into FTMP6

FMMUL4: LDA  FARG+3
        MOV  E, A   ;E = A2
        LDA  FAC+3  ;A2 * B2 -> Add to FTMP5, FTMP6
        MOV  H, A   ;H = B2
        CALL FMUL8  ;8 bit multiply (9of9)
        LHLD FTMP5
        DAD  D      ;16 bit add
        SHLD FTMP5  ;no CY out

        POP B       ;Restore registers
        POP D
        POP H
        RET

;------------------------------- FMUL8 --------------------------------
; Fast 8-bit x 8-bit -> 16-bit Multiply
; On call: H = multiplier and E = multiplicand
; On retn: DE = result. A, B, HL trashed
;----------------------------------------------------------------------
FMUL8:  MOV A, H    ; Multiplier -> A
        MVI D, 0    ; Ensure D is 0 so DE is exactly the Multiplicand
        LXI H, 0    ; Clear the result accumulator (HL)
        MVI B, 8    ; 8 bits to process
FMUL8L: DAD H       ; Shift result left (HL = HL * 2)
        RAL         ; Rotate Multiplier bit into Carry
        JNC FMUL8S  ; If Carry is 0, don't add the multiplicand
        DAD D       ; Add Multiplicand (DE) to Result (HL)
FMUL8S: DCR B       ; Decrement bit counter
        JNZ FMUL8L  ; Loop until all 8 bits are done
        XCHG        ; Move result from HL to DE for return
        RET

;------------------------------- FMDIV --------------------------------
; Divide mantissas in FAC and ARG to get quotient
; On call: FAC dividend mantissa, ARG divisor mantissa (both 24-bit)
; On retn: FTMP1-FTEMP4 contains 25 bit quotient, CY in FTMP5 lsb,
;   remainder in FAC.
; During loop: Copy dividend from FAC to registers DEB (also remainder)
;   Quotient will be in split, with low bytes on top of stack (TOS) and
;   high bytes in FTMP3 & 4.
;----------------------------------------------------------------------
FMDIV:  PUSH H      ;Save registers
        PUSH D
        PUSH B
        SUB A       ;Init FAC+0 to 0.  This location
        STA FAC     ;  will store CY out from FAC left shifts
        LXI D,FAC+3 ;Compare FAC and ARG.  If FAC is less then
        LXI H,FARG+3 ;quotient will require normalization (left shift)
        CALL FCMP   ;CY=1 if FAC<ARG
        RAL         ;lsb of A = CY
        ANI 1
        STA FTMP5   ;Save CY in FTMP5
        LXI H,0     ;Clear quotient in TOS & FTMP3-FTMP4
        PUSH H
        SHLD FTMP3
        LHLD FAC+2  ;Copy FAC to DEB
        XCHG
        LDA FAC+1
        MOV B,A
        MVI C,25    ;Init loop counter for 25 shifts
        LDA FTMP5   ;Quotient < 1.0 needs one more shift
        ORA A
        JZ FMDIV1
        INR C       ;26 shifts
;Shift 25 bit quotient (Q) left
FMDIV1: POP H       ;Rotate Q1,Q2 from TOS
        DAD H
        PUSH H
        LDA FTMP3   ;Get Q3
        RAL
        STA FTMP3
        LDA FTMP4   ;Get Q4
        RAL
        STA FTMP4
;If FAC >= ARG then...
        LDA FAC     ;Check FAC msb for CY out from shift left
        RAL
        JC FMDIV2   ;If 1 then FAC > ARG
        LXI H,FARG+3
        MOV A,D
        CMP M       ;Perform upper FAC - ARG
        JC FMDIV3   ;Branch if FAC < ARG
        JNZ FMDIV2  ;Branch if FAC > ARG
        DCX H
        MOV A,E
        CMP M
        JC FMDIV3
        JNZ FMDIV2
        DCX H
        MOV A,B
        CMP M
        JC FMDIV3
;...FAC = FAC - ARG
FMDIV2: MOV A,B      ;Restore remainder LSB
        LXI H,FARG+1 ;HL = ptr to ARG LSB
        SUB M 
        MOV B,A
        INX H
        MOV A,E
        SBB M
        MOV E,A
        INX H
        MOV A,D
        SBB M
        MOV D,A
;...Set q0 bit = 1
        POP H
        MVI L, 80H
        PUSH H
;Shift 24 bit FAC mantissa left
FMDIV3: ORA A       ;Clear CY
        MOV A,B
        RAL
        MOV B,A
        MOV A,E
        RAL
        MOV E,A
        MOV A,D
        RAL
        MOV D,A
        RAR         ;Save CY out in FAC msb
        STA FAC
;Decrement loop counter and loop if not zero
        DCR C
        JNZ FMDIV1
        POP H       ;Copy Quotient low bytes from TOS to FTMP
        SHLD FTMP1
        XCHG        ;Copy remainder from registers DEB to FAC
        SHLD FAC+2
        MOV A,B
        STA FAC+1
        POP B       ;Restore registers
        POP D
        POP H
        RET

;------------------------------- FSWAP --------------------------------
; Swap two unpacked (6 byte) floating-point numbers in memory
; On call: HL and DE point to the low byte of numbers to swap
; On retn: numbers swapped, All registers trashed
;----------------------------------------------------------------------
FSWAP:  MVI C, 6    ;Counter for 6 byte move
FSWAP1: MOV B, M    ;Read byte from (HL)
        LDAX D      ;Read byte from (DE)
        MOV M, A    ;Swap them
        MOV A, B
        STAX D
        INX D
        INX H
        DCR C
        JNZ FSWAP1
        RET

;------------------------------- FMNOR --------------------------------
; Normalize 48-bit product from FMMUL, extract 24-bit mantissa -> FAC
; Generate guard byte, sticky bit, calculate result exponent and check
; for overflow/underflow.  Return if no errors.
; On call: 48-bit product from FMMUL, FAC & ARG exponents, result sign
; On retn: FAC exponent updated, mantissa normalized, guard byte &
;          sticky bit loaded, CY cleared
;----------------------------------------------------------------------
FMNOR:  PUSH H      ;Save registers
        PUSH D
        PUSH B
;Clear FAC guard byte and sticky bit
        SUB A
        STA FAC     ;Guard byte = 0
        LDA FAC+5
        ANI 0FEH
        STA FAC+5   ;Sticky = 0
;Compute HL = expFAC + expARG - 127
        LHLD FAC+4  ;Get FAC in L (H is garbage)
        MOV E, L    ;Now FAC in E
        LHLD FARG+4 ;Get ARG in L (H is garbage)
        SUB A       ;Clear A
        MOV H,A     ;Clear H
        MOV D,A     ;Clear D
        DAD D       ;HL = 16 bit sum (cannot overflow)
        LXI D, -127 ;DE = -127 (0xFF81)
        DAD D       ;HL = FAC + ARG = 127

;Determine if normalization shift is needed: product < 2.0
        LDA FTMP6   ;Check b47 of product
        ANI 80H
        JNZ FMNOR2  ;Skip normalization if bit is one
;Shift 48-bit product left one bit
        LXI D,FTMP1
        MVI C,6     ;Counter: shift 6 bytes
        ORA A       ;Clear CY
FMNOR1: LDAX D
        RAL         ;Rotate next byte
        STAX D
        INX D
        DCR C
        JNZ FMNOR1
        JMP FMNOR3
FMNOR2: INX H       ;Increment provisional expFAC
;Check HL = provisional expFAC for over/under flow else save expFAC
FMNOR3: MOV A,H  
        ORA A
        JM FERRUF   ;If HL negative then underflow error
        JNZ FERROF  ;If HL > 255 then overflow error
        MOV A,L
        CPI 0FFH
        JZ FERROF   ;If L = 255 then overflow error
        MOV A,L
        ORA A
        JZ FERRUF   ;If L = 0 then underflow error
        STA FAC+4   ;Save final expFAC
;Copy the leading 24 bits of product into FAC
        LDA FTMP6
        ORI 80H     ;Set hidden leading 1
        STA FAC+3
        LHLD FTMP4
        SHLD FAC+1
;Load the FAC guard byte
        LDA FTMP3
        STA FAC
;Generate sticky bit by ORing of all remaining discarded bits
        MVI C,0     ;Initialize C to 0
        LDA FTMP2
        MOV B,A
        LDA FTMP1
        ORA B
        JZ FMNOR4
        INR C       ;If any bit in non-zero, then C = 1
FMNOR4: LDA FAC+5
        ORA C       ;Combine FAC sign/sticky with C
        STA FAC+5
        SUB A       ;Clear CY
        POP B       ;Restore registers
        POP D
        POP H
        RET

;------------------------------- FNORD --------------------------------
; Normalize 25-bit quotient from FMDIV, extract 24-bit mantissa -> FAC
; On call: FAC sign (done), FAC & ARG exponents, FTMP1-FTMP4 = 25 bit
; quotient from FMDIV, FAC is remainder, 
; FTMP5 normalization flag: 0 = quo >= 1.0, 1 = quo < 1.0.
; On retn: FAC exponent updated, mantissa normalized, guard byte &
;          sticky bit loaded
;----------------------------------------------------------------------
FNORD:  PUSH H      ;Save registers
        PUSH D
        PUSH B
;Clear FAC guard byte and sticky bit
        SUB A
        STA FAC     ;Guard byte = 0
        LDA FAC+5
        ANI 0FEH
        STA FAC+5   ;Sticky = 0
;Compute the exponent = FACexp - ARGexp + 127
        LDA FAC+4   ;Set HL = FACexp
        MOV L,A
        MVI H,0
        LDA FARG+4  ;Set DE = ARGexp
        MOV E,A
        MVI D,0
        MOV A,L     ;16 bit subtract
        SUB E
        MOV L,A
        MOV A,H
        SBB D
        MOV H,A
        LXI D,127   ;Add 127
        DAD D
        LDA FTMP5   ;Normalization correction required (FTMP5=1)?
        ORA A
        JZ FNORD1
        DCX H       ;Yes, then exp = exp - 1
;Check for exponent over/under flow then save result in FACexp
FNORD1: MOV A,H  
        ORA A
        JM FERRUF   ;If HL negative then underflow error
        JNZ FERROF  ;If HL > 255 then overflow error
        MOV A,L
        CPI 0FFH
        JZ FERROF   ;If L = 255 then overflow error
        MOV A,L
        ORA A
        JZ FERRUF   ;If L = 0 then underflow error
        STA FAC+4   ;Save final FACexp
;Generate the sticky bit from remainder in FAC
        LHLD FAC+1  ;Load and or 3 bytes in FAC mantissa
        LDA FAC+3
        ORA H
        ORA L
        JZ FNORD2
        LDA FAC+5   ;If remainder > 0 then set sticky bit
        ORI 1
        STA FAC+5
;Copy quotient into the FAC guard byte and mantissa
FNORD2: LDA FTMP1   ;Get q0
        STA FAC     ;Write to guard byte
        LHLD FTMP2  ;Get q1-q16
        SHLD FAC+1  ;Write to FAC
        LDA FTMP4   ;Get q17-24
        STA FAC+3   ;Write to FAC
        POP B       ;Restore registers
        POP D
        POP H
        RET


;----------------------------- FQFD2B ---------------------------------
; Decimal to Binary Conversion.  Generates a single precision float.
; On call: DE points to the decimal string (no leading blanks).
; On retn: FAC = binary float, DE advanced.
;   FAC = float(mem(DE))  Convert an ASCII decimal string to a
;   floating-point number.  DE points to a zero-terminated (or
;   otherwise-terminated) input string, parsed with 1-character
;   lookahead and no backtracking.  Terminates on the first char
;   that does not extend the grammar below; DE is left pointing
;   at that character (this routine does NOT preserve DE, unlike
;   the general library convention).
;
;   Grammar:
;     number   ::= sign? digits? ('.' digits?)? exponent?
;     sign     ::= '+' | '-'
;     digits   ::= digit+
;     exponent ::= ('E'|'e') expsign? digit+
;     expsign  ::= '+' | '-'
;   (at least one digit must appear before or after the decimal
;   point; exponent sign is optional -- '+' is implied if absent)
;
;   Valid examples:  123   1.23   1.23E+2   1.23e-1   .5   1.5E10
;   Maximum significand digits = 9 (including leading 0s).
;   Maximum exponent digits = 2 (including leading 0s).
;   Characters beyond these limits are not consumed; DE is left
;   pointing at the first one.
;   Invalid examples: 0.00000000123 returns 0.0 (DE at '1');
;   12345678901234.5 returns 123456789 (DE at the '0');
;   123456789.5 returns 123456789 (DE at the '.');
;----------------------------------------------------------------------
FQFD2B: PUSH B      ;Save registers, except for A, DE
        PUSH H
        SUB A
        STA FPR     ;Clear entire status field
        STA FTMP1   ;Clear temporary storage for sign

;Parse optional leading sign and save on stack
        CALL FQFDGN ;(DE) returns A = 80H if '-' else 00H
        PUSH PSW

;Acccumulate integer + fraction digits into one 32-bit accumulator
;(FTMP1-FTMP4) also track digit count (FTMP6) and dp flag + fracDigits
;(FTMP5).
        LXI H, 0
        PUSH H      ;Initialize expSign & expVal to 0, push on stack
        SHLD FTMP1  ;Initialize 32-bit accumulator to 0 
        SHLD FTMP3
        SUB A
        STA FTMP5   ;Initialize decimal point flag and fracDigits to 0
        STA FTMP6   ;Initialize digit count to 0

;Loop to parse valid characters
FQFD3:  LDAX D      ;Get next char
        CPI '.'     ;Check for decimal point '.'
        JZ FQFDDP
        CPI 'E'     ;Check for Scientific notation 'E' or 'e'
        JZ FQFDSN
        CPI 'e'
        JZ FQFDSN
        CALL FQFDD  ;Check for digit '0'..'9'
        JC FQFDT    ;Else terminator found

;Digit, so multiply accumulator by 10 and add digit
        CALL FQFD10 ;FTMP Accumulator *= 10
        LDAX D      ;FTMP Accumulator += numeric digit
        SUI 30H
        MOV B,A
        LXI H,FTMP1
        MVI C,4     ;Counter for 4 byte accum
        SUB A
FQFD4:  MOV A,M
        ADC B
        MVI B,0     ;After first add, just ripple CY to higher bytes
        MOV M,A
        INX H
        DCR C
        JNZ FQFD4   ;Loop if counter > 0
        LDA FTMP6   ;Inc digit count
        INR A
        STA FTMP6
        LDA FTMP5   ;If dp flag clear, then branch
        ORA A
        JP FQFD5
        INR A       ;If dp flag set then inc fracDigits
        STA FTMP5
FQFD5:  INX D
        LDA FTMP6
        CPI 9       ;Check for digit count = maxDigits (9)
        JZ FQFD6
        JMP FQFD3   ;Loop back for next char

FQFD6:  LDAX D      ;When digit count = maxDigits...
        CPI 'E'     ;Check for 'E' or 'e'
        JZ FQFDSN
        CPI 'e'
        JZ FQFDSN
        JMP FQFDT

;Decimal point, so set dp flag in FTMP5 msb
FQFDDP: LDA FTMP5
        ORA A
        JM FQFDT    ;If dp is already set, then terminate
        ORI 80H     ;Set bit 7 (msb)
        STA FTMP5
        INX D
        JMP FQFD3

;Parse Scientific Notation 
FQFDSN: INX D       ;Consume 'E' or 'e'
        POP B       ;B = expSign, C = expVal (both initially 0)
        CALL FQFDGN ;Get optional sign: A = 80H if '-' else 00H
        MOV B,A     ;B = expSign
        LDAX D      ;Get next char
        CALL FQFDD  ;Check for digit
        JC FERRUN   ;No digit is undefined error 111.
        INX D       ;Consume digit
        SUI 30H     ;Convert to val(A)
        MOV C,A     ;expVal = A
        LDAX D
        CALL FQFDD  ;Check for second digit
        JC FQFDS1
        MOV A,C     ;Multiply 1st digit by 10
        ADD A
        ADD A
        ADD A
        ADD C
        ADD C
        MOV C,A
        LDAX D      ;Add second digit
        INX D       ;Consume second digit
        SUI 30H
        ADD C
        MOV C,A        
FQFDS1: PUSH B      ;Push expSign & expVal back on stack

;Terminator reached (char not a number part)
FQFDT:  LDA FTMP6   ;Check for digit count = 0 error
        ORA A
        JZ FERRUN   ;Exit with error code 111
;Calc totalExp = expVal - fracDigits
        POP B       ;B = expSign, C = expVal

;Test 32 bit accumulator for zero
        LXI H,FTMP1
        MOV A,M
        INX H
        ORA M
        INX H
        ORA M
        INX H
        ORA M
        JNZ FQFD7
        CALL FCLR   ;Set FAC = 0
        POP PSW     ;Discard sign
        JMP FQFDX

;Calc totalExp = expVal - fracDigits  (expSign in B, expVal in C)
FQFD7:  MOV A,B
        ORA A       ;Set flags from expSign
        MOV A,C     ;MOV leaves the flags alone
        JP FQFD71   ;expSign clear: exp = +expVal
        CMA         ;expSign set: negate expVal
        INR A
FQFD71: MOV C,A     ;C = signed exp (-99..+99)
        LDA FTMP5
        ANI 7FH     ;Strip dp flag, A = fracDigits (0..9)
        MOV B,A
        MOV A,C
        SUB B       ;A = totalExp (-108..+99), signed byte

;Split into direction and magnitude, clamp magnitude to 63
        MVI B,0     ;B = direction: 00H = multiply
        ORA A       ;Test sign of totalExp
        JP FQFD72
        MVI B,0FFH  ;Negative: FFH = divide
        CMA
        INR A       ;A = |totalExp|
FQFD72: CPI 64
        JC FQFD73
        MVI A,63    ;Clamp to table range
FQFD73: MOV C,A     ;B = direction, C = n (0..63)

;Float the unsigned 32 bit result and place in FAC
        PUSH D
        LXI D,FTMP1
        CALL FLTDS  ;Float int to FAC (HL/DE/BC preserved)
        POP D

;Scale FAC by 10^totalExp via binary exponentiation
        MOV A,C
        ORA A
        JZ FQFD9    ;n = 0, nothing to scale (also skips the PUSH D)
        PUSH D      ;Save string pointer; DE becomes the table pointer
        LXI D,F10TAB
FQFD8:  MOV A,C
        ORA A       ;Clear CY so RAR shifts in a 0
        RAR         ;CY = next bit of n, A = n >> 1
        MOV C,A     ;MOV leaves CY alone
        JNC FQFD82  ;Bit clear, skip this power
        MOV A,B     ;Direction: 00H = multiply, FFH = divide
        ORA A
        JNZ FQFD81
        CALL FMUL   ;FAC = FAC * mem(DE)
        JMP FQFD82
FQFD81: CALL FDIV   ;FAC = FAC / mem(DE)
FQFD82: INX D       ;Advance table pointer by 4 bytes
        INX D
        INX D
        INX D
        MOV A,C
        ORA A
        JNZ FQFD8   ;Loop while bits remain in n
        POP D       ;Restore string pointer

;Apply sign
FQFD9:  POP PSW     ;A = 80H if negative else 00H
        ORA A
        JP FQFDX
        CALL FNEG
FQFDX:  POP H       ;Restore registers
        POP B
        RET         ;Exit FQFD2B

;Table of powers 10^1 10^2 10^4 10^8 10^16 10^32 packed little endian
;Plus 10^7.  Keep this order.
F10TAB: DB 00H, 00H, 20H, 41H   ;10^1
        DB 00H, 00H, 0C8H, 42H  ;10^2
        DB 00H, 40H, 1CH, 46H   ;10^4
        DB 20H, 0BCH, 0BEH, 4CH ;10^8
        DB 0CAH, 1BH, 0EH, 5AH  ;10^16
        DB 0AEH, 0C5H, 9DH, 74H ;10^32
        DB 80H, 96H, 18H, 4BH   ;10^7


;Get optional sign char, return 80h if '-' else 00h.  DE advanced.
FQFDGN: LDAX D      ;Get char
        CPI '-'
        JNZ FQFDG1
        MVI A,80H
        INX D
        RET
FQFDG1: CPI '+'
        JNZ FQFDG2
        SUB A
        INX D
        RET
FQFDG2: SUB A
        RET

;Check for A = ASCII digit 0..9: Retn 0=yes, 1=no
FQFDD:  CPI '0'     ;Check for numeric 0..9
        RC
        CPI '9'+1
        CMC
        RET

;Multiply 10 * 32-bit unsigned integer.  Use X*10 = X*8 + X*2
;On call: int in FTMP1 (little endian)
;On retn: FTMP1 *= 10. DE preserved, all other regs and FARG trashed
;  CY=1 indicates overflow
FQFD10: PUSH D
        LHLD FTMP3  ;Copy FTMP1 to FARG and regs HL/DE
        SHLD FARG+2
        XCHG
        LHLD FTMP1
        SHLD FARG
        MVI C,3     ;FTMP1 *= 8: left shift 3 bits
        CALL FSHLDE
        JC FQFD1X   ;Exit if overflow
        SHLD FTMP1
        XCHG
        SHLD FTMP3
        LHLD FARG+2 ;HL/DE = FARG * 2: left shift 1 bit
        XCHG
        LHLD FARG
        MVI C,1
        CALL FSHLDE
        LXI B,FTMP1 ;FTMP1 += HL/DE
        LDAX B
        ADD L
        STAX B
        INX B
        LDAX B
        ADC H
        STAX B
        INX B
        LDAX B
        ADC E
        STAX B
        INX B
        LDAX B
        ADC D
        STAX B
FQFD1X: POP D
        RET

;Shift 32-bit integer left by C bits.
;On call: C is bit count, integer in registers LHED (LSB to MSB)
;On retn: LHED shifted, CY=1 if overflow occurs
FSHLDE: DAD H
        MOV A,E
        RAL
        MOV E,A
        MOV A,D
        RAL
        MOV D,A
        RC
        DCR C
        JNZ FSHLDE
        RET


;------------------------------- FQFB2D -------------------------------
; Binary to decimal conversion
; On call: FAC = float, DE = address of a 14-byte buffer
; On retn: mem(DE) = null-terminated ASCII string.  FAC, BC, DE, HL
;   are preserved.  ARG and FTMP are trashed.
; Let x = |FAC| rounded to 7 digits, x = D.DDDDDD x 10^d.
;   FAC < 0 gives a leading '-'.   FAC = 0 gives "0".
;   0 <= d <= 6  decimal: '.' only if a nonzero fraction remains,
;                trailing zeros trimmed: 1234567  1.2  -1.234567
;   otherwise    scientific: D.DDDDDDE[-]n, trailing zeros trimmed
;                but one fraction digit kept: 1.0E7  5.0E-1  -1.234567E-12
;   Exponent has no '+' and no leading zeros.  Longest string is
;   -1.234567E-38.  Exponent FFh in FAC gives EC=101 and FERROR.
;   The last digit may be in error due to rounding.
;----------------------------------------------------------------------
;Entry and special cases
FQFB2D: PUSH B      ;Save registers
        PUSH D
        PUSH H
        SUB A
        STA FPR     ;Clear entire status field
        LDA FAC+4
        CPI 0FFH    ;Check for invalid exponent
        JZ FERRIF   ;  Error - code 101
        ORA A       ;Check for zero
        JNZ FQFB2
        MVI A,'0'   ;  Buffer = 30H 00H
        STAX D
        INX D
        SUB A
        STAX D
        JMP FQFBX

;Copy FAC onto stack, and form |FAC|
FQFB2:  LHLD FAC
        PUSH H
        LHLD FAC+2
        PUSH H
        LHLD FAC+4
        PUSH H
        SUB A
        STA FAC+5   ;FAC = |FAC|, sign and sticky cleared

;Estimate the decimal exponent
FQFB3:  LDA FAC+4   ;A =FACexp
        SUI 127     ;e2 = FACexp - 127
        MOV L,A
        RAL
        SBB A
        MOV H,A
        MOV D,H     ;Multiply HL *= 77
        MOV E,L     ;DE = x
        DAD H       ;2x
        DAD H       ;4x
        DAD H       ;8x
        DAD D       ;9x
        DAD H       ;18x
        DAD D       ;19x
        DAD H       ;38x
        DAD H       ;76x
        DAD D       ;77x, H = floor(77x/256)
        MOV B,H     ;B = d = floor(e2*0.30078)
        MVI A,7
        SUB H
        MOV C,A     ;n = 7 − d, the scaling exponent
        CALL FQSCAL ;FAC = FAC * 10^n

;Fix up to [1E7, 1E8)
FQFB5:  LXI D,F10TAB+12 ;If FAC >= 1E8:
        CALL FCMPR
        CPI 20H
        JZ FQFB51
        LXI D,F10TAB    ;  then FAC /= 10
        CALL FDIV
        INR B           ;  d++
        JMP FQFB5
FQFB51: LXI D,F10TAB+24 ;If FAC < 1E7:
        CALL FCMPR
        CPI 20H
        JNZ FQFB6
        LXI D,F10TAB    ;  then FAC *= 10
        CALL FMUL
        DCR B           ;  d--
        JMP FQFB5

;Convert FAC to integer and round to 7 digits
FQFB6:  LXI D,FTMP1 ;Convert FAC to 32 bit integer in FTMP1
        CALL FIXSD
        LDA FTMP1   ;Calc FTMP1 + 5
        ADI 5
        STA FTMP1
        LDA FTMP2
        ACI 0       ;Propagate carry
        STA FTMP2
        LDA FTMP3
        ACI 0
        STA FTMP3
        LDA FTMP4
        ACI 0
        STA FTMP4
        CALL FDIV10 ;Calc (FTMP1 + 5) / 10

;Generate 7 digits, least significant first
FQFB8:  LXI H,8     ;Frame at SP: +0 exp, +1 sign, +2..+5 FAC,
        DAD SP      ;  +6,+7 saved HL, +8,+9 saved DE
        MOV E,M     ;E = low byte of buffer address
        INX H
        MOV D,M     ;DE = buffer address
        LXI H,12
        DAD D       ;HL = buffer + 12
        MVI C,7     ;Digit counter
FQFB8A: CALL FDIV10 ;FTMP1..FTMP4/10, A=remainder
        ADI '0'     ;Convert to ASCII
        MOV M,A     ;Save digit in buffer
        DCX H
        DCR C       ;Dcr digit count, loop if > 0
        JNZ FQFB8A

;Trim trailing zeros. On entry DE = buffer base, B = d.
;On exit C = K (1..7), B and DE unchanged.
FQFB9:  LXI H,12
        DAD D       ;HL = ptr to S[7], the last digit
        MVI C,7     ;K = 7
FQFB9A: MOV A,M
        CPI '0'
        JNZ FQFB10  ;Stop at the last nonzero digit
        DCX H
        DCR C       ;K--
        JNZ FQFB9A  ;Never reaches 0: S[1] is always nonzero

;Format the string adding sign, decimal pt, and E as needed
;On entry reg B = d (signed) and reg C = K (1..7)
FQFB10: LXI H,1
        DAD SP
        MOV A,M     ;sign byte
        ANI 80H
        LXI H,6
        DAD D
        XCHG        ;DE = src (buffer+6), HL = dest (buffer)
        JZ FQFB1A   ;no minus sign
        MVI M,'-'
        INX H
FQFB1A: MOV A,B     ;If 0 <= d <= 6:
        ORA A
        JM FQFB1S
        CPI 7
        JNC FQFB1S
;Decimal number
        MOV A,C     ;Calc F = A = K - d - 1
        SUB B
        DCR A
        MOV C,B     ;C = d + 1
        INR C
        MOV B,A     ;d is no longer needed
        CALL FQCPY  ;Copy d + 1 digits
        MOV A,B
        ORA A       ;If F = A <= 0 then...
        JZ FQFB1E   ; nothing after the point: omit '.'
        JM FQFB1E
        MVI M,'.'   ;Else, write '.'
        INX H
        MOV C,B
        CALL FQCPY  ;Copy F fraction digits
        JMP FQFB1E  ;Go write terminating 0
;Scientific number
FQFB1S: LDAX D      ;Move first digit
        MOV M,A
        INX D
        INX H
        MVI M,'.'   ;Write '.'
        INX H
        DCR C       ;C = K -1
        JNZ FQFB1B  ;If C is zero then inc C
        INR C
FQFB1B: CALL FQCPY  ;Copy fraction digits
        MVI M,'E'   ;Write 'E'
        INX H
        CALL FQEXPO ;B = d on entry, appends the exponent

FQFB1E: MVI M,0     ;Write terminating zero
        JMP FQFB12  ;Exit

;Copy C (>=1) bytes from DE to HL
FQCPY:  LDAX D
        MOV M,A
        INX H
        INX D
        DCR C
        JNZ FQCPY
        RET

;Append the exponent string after 'E'.  Reg B = d (signed)
FQEXPO: MOV A,B
        ORA A
        JP FQEXP1   ;If d < 0:
        MVI M,'-'   ;  Write '-'
        INX H
        CMA         ;  d = -d
        INR A
        MOV B,A
FQEXP1: MVI C,0
FQEXP2: MOV A,B     ;While B >= 10:
        CPI 10
        JC FQEXP3
        SUI 10      ;  B -=10
        MOV B,A
        INR C       ;  C++
        JMP FQEXP2
FQEXP3: MOV A,C     ;If C != 0:
        ORA A
        JZ FQEXP4
        ADI '0'
        MOV M,A     ; Write tens digit
        INX H
FQEXP4: MOV A,B
        ADI '0'
        MOV M,A     ;Write ones digit
        INX H
        RET

;Restore original FAC from stack
FQFB12: POP H
        SHLD FAC+4
        POP H
        SHLD FAC+2
        POP H
        SHLD FAC
FQFBX:  POP H       ;Restore registers & exit
        POP D
        POP B
        RET

;FAC = FAC * 10^n. On call: C = n.
FQSCAL: PUSH B
        MOV A,C     ;If n = 0 then done
        ORA A
        JZ FQSCA5
        MVI B,0
        JP FQSCA1   ;If n > 0 Op = mult (B=0)
        INR B       ;If n < 0 Op = div (B=1)
        CMA         ;  and n = -n
        INR A
        MOV C,A
FQSCA1: LXI D,F10TAB ;Ptr to 1E1,1E2,1E4,1E8,1E16,1E32 (4 bytes ea)
FQSCA2: MOV A,C
        ORA A
        RAR         ;Shift n right, lsb into CY
        MOV C,A
        JNC FQSCA4  ;Branch if CY = 0
        MOV A,B     ;Select Op (B)
        ORA A
        JNZ FQSCA3
        CALL FMUL   ;If Op = mult, then FAC * mem(DE)
        JMP FQSCA4
FQSCA3: CALL FDIV   ;If Op = div, then FAC / mem(DE)
FQSCA4: INX D       ;Ptr += 4
        INX D
        INX D
        INX D
        MOV A,C     ;If n > 0 then loop
        ORA A
        JNZ FQSCA2
FQSCA5: POP B
        RET

;32-bit divide N by 10. N in FTMP1..FTMP4, little-endian.
; On return: N = quotient, A = remainder. Only A and flags trashed.
FDIV10: PUSH H      ;Save registers
        PUSH D
        PUSH B
        LXI D,2000H ;D=32 (bit counter), E=0 (remainder)
        SUB A       ;Clear CY (first quotient bit in = 0)
FDIV1A: CALL FDIV1R ;Shift N left; CY in = q, CY out = top bit
        MOV A,E
        RAL         ;E*2 + top bit (E <= 9, so CY out = 0)
        MOV E,A     ;New remainder, kept as if its < 10
        SUI 10      ;CY = 1 if remainder < 10
        JC FDIV1B   ;  then keep it, q = 0
        MOV E,A     ;  else remainder -= 10, q = 1
FDIV1B: CMC         ;CY = q
        DCR D
        JNZ FDIV1A
        CALL FDIV1R ;Shift in the last quotient bit
        MOV A,E     ;A = remainder
        POP B       ;Restore registers
        POP D
        POP H
        RET

;Shift the 32-bit N left one bit through CY
FDIV1R: LXI H,FTMP1
        MVI C,4
FDIV1S: MOV A,M
        RAL         ;CY in from previous byte
        MOV M,A
        INX H       ;INX, DCR and MOV leave CY alone
        DCR C
        JNZ FDIV1S
        RET         ;CY = top bit of N


;------------------------------- FIXSD --------------------------------
; Convert a float to a 32-bit integer.  Truncate fractional portion.
; On call: FAC contains float, DE points to the integer destination
; On retn: mem(DE) contains the integer, FAC unchanged.
;----------------------------------------------------------------------
FIXSD:  PUSH H
        PUSH B
        PUSH D
        SUB A
        STA FPR     ;Clear entire status field
        LXI H,0     ;Init result(HL/DE)= 0
        LXI D,0
        LDA FAC+4   ;Get FAC exponent
        CPI 0FFH    ;Check for invalid exponent
        JZ FERRIF   ;  Error - invalid number in FAC code 101
        CPI 7FH     ;Check for float < 1.0
        JC FIXST    ;  Store result = 0
        CPI 9FH     ;Check for float too large
        JNC FERROF  ;  Error - overflow code 011
        CPI 9EH     ;Check for exponent < 9EH (normal path)
        JNZ FIXSD1  ;  Else when exponent = 9EH...   
        LDA FAC+5   ;  If sign=-1 and mantissa=800000 then..
        ANI 80H     ;  ...result = 80000000
        JZ FERROF
        LDA FAC+1
        MOV B,A
        LDA FAC+2
        ORA B
        JNZ FERROF
        LDA FAC+3
        CPI 80H
        JNZ FERROF
        MOV H,A
        JMP FIXST
;Build 32-bit magnitude in HL/DE
FIXSD1: LHLD FAC+2  ;Copy FAC mantissa (24bits) to HL/D and 0 in E
        LDA FAC+1
        MOV D,A
        MVI E,0
        LDA FAC+4   ;Get FAC exponent
        MOV B,A     ;B = exp
        CPI 8FH     ;If exp < 8F: shift right 2 bytes
        JNC FIXSD2
        MOV E,L
        MOV D,H
        LXI H,0
        MOV A,B     ;exp += 16
        ADI 16
        MOV B,A
FIXSD2: MOV A,B
        CPI 97H     ;If exp < 97: shift right 1 byte
        JNC FIXSD3
        MOV E,D
        MOV D,L
        MOV L,H
        MVI H,0
        MOV A,B     ;exp += 8
        ADI 8
        MOV B,A
FIXSD3: MOV A,B     ;If exp < 9EH: shift right 1 bit
        CPI 9EH
        JNC FIXSD4
        ORA A       ;Clear CY
        MOV A,H     ;Shift right
        RAR
        MOV H,A
        MOV A,L
        RAR
        MOV L,A
        MOV A,D
        RAR
        MOV D,A
        MOV A,E
        RAR
        MOV E,A
        INR B       ;exp += 1
        JMP FIXSD3
;Handle negative values
FIXSD4: LDA FAC+5   ;Get sign bit
        ANI 80H     ;If sign negative, complement result
        JZ FIXST
        SUB A       ;Complement result using 0-int
        SUB E
        MOV E,A
        MVI A,0
        SBB D
        MOV D,A
        MVI A,0
        SBB L
        MOV L,A
        MVI A,0
        SBB H
        MOV H,A
;Store result in mem(DE)  
FIXST:  POP B       ;BC now contains mem ptr
        PUSH B
        MOV A,E
        STAX B
        INX B
        MOV A,D
        STAX B
        INX B
        MOV A,L
        STAX B
        INX B
        MOV A,H
        STAX B
        POP D
        POP B
        POP H
        RET


;------------------------------- FLTDS --------------------------------
; Convert a 32-bit integer to a float.
; On call: DE points to the integer in memory
; On retn: FAC contains the float equivalent, original int unchanged.
;----------------------------------------------------------------------
FLTDS:  PUSH H
        PUSH D
        PUSH B
;Move copy of integer to HL/BC
        LDAX D      ;C = least significant byte
        MOV C,A
        INX D
        LDAX D      ;B = next byte
        MOV B,A
        INX D
        LDAX D      ;L = next byte
        MOV L,A
        INX D
        LDAX D      ;H = most significant byte
        MOV H,A
;Check for special case int=0
        ORA L       ;Logical OR all 4 bytes
        ORA B
        ORA C
        JNZ FLTDS1  ;If integer = 0 then...
        CALL FCLR   ;  Clear FAC to zero
        JMP FLTDSX  ;  Exit
;Extract sign and form magnitude if negative
FLTDS1: MOV A,H
        ANI 80H
        STA FAC+5   ;Save sign bit in FAC
        JZ FLTDS2   ;Branch if positive
        SUB A       ;Else complement using 0-int
        SUB C
        MOV C,A
        MVI A,0
        SBB B
        MOV B,A
        MVI A,0
        SBB L
        MOV L,A
        MVI A,0
        SBB H
        MOV H,A
;Normalize: shift left until H bit 7 = 1; count shifts in E
FLTDS2: MVI E,9EH   ;Init exponent to 158 = 127 + 31
FLTDS3: MOV A,H     ;While H = 0: shift left whole byte
        ORA A
        JNZ FLTDS4
        MOV H,L
        MOV L,B
        MOV B,C
        MVI C,0
        MOV A,E
        SUI 8       ;E = E - 8
        MOV E,A
        JMP FLTDS3
FLTDS4: JM FLTDS6   ;If H bit 7 is set, skip shifts
FLTDS5: MOV A,C     ;(A=H) do: shift left one bit
        RAL
        MOV C,A
        MOV A,B
        RAL
        MOV B,A
        MOV A,L
        RAL
        MOV L,A
        MOV A,H
        RAL
        MOV H,A
        DCR E       ;E = E -1
        ORA A
        JP FLTDS5   ;while H bit 7 is zero
FLTDS6: MOV A,E     ;Save exponent in FAC
        STA FAC+4
;Copy mantissa, and guard byte to FAC (sticky =0)
        SHLD FAC+2
        MOV H,B
        MOV L,C
        SHLD FAC
;Round-to-nearest, ties to even
        CALL FROUND
FLTDSX: POP B
        POP D
        POP H
        RET
