# 8080-8085 Math Library
## Assembly language routines to extend the math capabilities of Intel 8080/8085 microprocessors.

**FML.asm - 8080/8085 FLOATING-POINT MATH LIBRARY** <br>
This module contains basic floating point subroutines and functions. Intended for retrocomputing, this module is not strictly IEEE 754 compliant.

FML procedures operate on sigle precision floating-point numbers.

Procedures:
*  FADD: Addition
*  FSUB: Subtraction
*  FMUL: Multiplication
*  FDIV: Division
*  FSQRT: Square Root
*  FQFD2B: Convert decimal to binary
*  FQFB2D: Convert binary to decimal
*  FIXSD: Convert float to integer
*  FLTDS: Convert integer to float
*  FCMPR: Compare two floats
*  FZTST: Compare float to zero
*  FNEG: Negate a float
*  FCLR: Clear to zero
*  FABS: Absolute value
***

**IML.asm - 8080/8085 INTEGER MATH LIBRARY** <br>
The Integer Math Library (IML) contains basic integer subroutines and functions.  The operations provided are: addition, subtraction, multiplication, division, value comparison, negation, absolute value, and conversion between decimal and binary integer representations.

IML procedures operate on single precision integer binary numbers in the range: [-32767, +32767].  The value 8000H is NaN. <br>

Procedures & execution times (2 MHz 8080)
*   IADD:  HL = HL + DE (20 to 29 uS)
*   ISUB:  HL = HL - DE (56 to 59 uS)
*   IMUL:  HL = HL * DE (150 to 553 uS)
*   IDIV:  HL = HL / DE (330 to 3082 uS)
*   IABS:  HL = |HL|
*   INEG:  HL = -HL
*   ICMP:  S & Z FLAGS SET TO REFLECT HL - DE
*   IFLG:  S & Z FLAGS SET TO REFLECT HL
*   ID2B:  DECIMAL NUMBER IS CONVERTED TO BINARY
*   IB2D:  BINARY NUMBER IS CONVERTED TO DECIMAL
***

**UIML.asm - 8080/8085 UNSIGNED INTEGER MATH LIBRARY** <br>
The Unsigned Integer Math Library (UIML) contains basic integer subroutines and functions.  The operations provided are: addition, subtraction, multiplication, division, value comparison, and conversion between decimal and binary integer representations.

UIML procedures operate on single precision unsigned integer binary numbers in the range: [0 to 65535]. <br>

Procedures & execution times (2 MHz 8080)
*   UADD:  HL = HL + DE (16 uS)
*   USUB:  HL = HL - DE (25 uS)
*   UMUL:  HL = HL * DE (112 to 373 uS)
*   UDIV:  HL = HL / DE (54 to 2352 uS)
*   UCMP:  CY & Z FLAGS SET TO REFLECT HL - DE
*   UD2B:  DECIMAL NUMBER IS CONVERTED TO BINARY
*   UB2D:  BINARY NUMBER IS CONVERTED TO DECIMAL
