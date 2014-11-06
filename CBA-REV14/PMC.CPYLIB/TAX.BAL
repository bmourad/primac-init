***************************
*  REVISION - [08.0]      *
***************************
*
*   KEY = CONO : DIV : DEPT : CCTR : TAX JURS
*
TAX.BAL.REC.SIZE = 10
DIM TAX.BAL.REC(10)
EQU TBR.PERIOD  TO TAX.BAL.REC(1)
EQU TBR.TAXABLE TO TAX.BAL.REC(2)
EQU TBR.CHARGED TO TAX.BAL.REC(3)
