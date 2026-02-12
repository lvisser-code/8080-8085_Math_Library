# 8080-8085 Math Library
## Assembly language routines to extend the math capabilities of Intel 8080/8085 microprocessors.
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