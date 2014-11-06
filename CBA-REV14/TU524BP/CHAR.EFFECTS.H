* CHAR.EFFECTS.H
*
** SBClient Host 3GL API
** Copyright (C) Ardent Software Inc. 1998
** Copyright (C) UniData, Inc. 1996, 1997
** Copyright (C) System Builder Corporation. 1995
**
**      This software is the proprietary property and contains
**      trade secrets of Ardent Software, Inc. Any unauthorized use,
**      disclosure or duplication is strictly prohibited.
**      All rights reserved.
*
************************************************
* Equates for character effects
* NOTE: If your version does not support EQU LIT , then you will need
* to change statements below to assignments
************************************************
*
CHAR.EFFECTS.ON = CHAR(27):'_@':CHAR(27):'e1.':CHAR(27):'\'
CHAR.EFFECTS.OFF = CHAR(27):'_@':CHAR(27):'e0.':CHAR(27):'\'
CHAR.TRANSP.ON = CHAR(27):'_@':CHAR(27):'V2.':CHAR(27):'\'
CHAR.TRANSP.OFF = CHAR(27):'_@':CHAR(27):'V0.':CHAR(27):'\'
CHAR.TEXT.IN.ON = CHAR(27):'_@':CHAR(27):'V11.':CHAR(27):'\'
CHAR.TEXT.IN.OFF = CHAR(27):'_@':CHAR(27):'V10.':CHAR(27):'\'
CHAR.TEXT.OUT.ON = CHAR(27):'_@':CHAR(27):'V12.':CHAR(27):'\'
CHAR.TEXT.OUT.OFF = CHAR(27):'_@':CHAR(27):'V10.':CHAR(27):'\'
CHAR.TEXT.DISABLE.ON = CHAR(27):'_@':CHAR(27):'V9.':CHAR(27):'\'
CHAR.TEXT.DISABLE.OFF = CHAR(27):'_@':CHAR(27):'V10.':CHAR(27):'\'
CHAR.FRAME.IN.ON = CHAR(27):'_@':CHAR(27):'V22.':CHAR(27):'\'
CHAR.FRAME.IN.OFF = CHAR(27):'_@':CHAR(27):'V20.':CHAR(27):'\'
CHAR.FRAME.OUT.ON = CHAR(27):'_@':CHAR(27):'V21.':CHAR(27):'\'
CHAR.FRAME.OUT.OFF = CHAR(27):'_@':CHAR(27):'V20.':CHAR(27):'\'
CHAR.FRAME.MS.ON = CHAR(27):'_@':CHAR(27):'V24.':CHAR(27):'\'
CHAR.FRAME.MS.OFF = CHAR(27):'_@':CHAR(27):'V20.':CHAR(27):'\'
