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
***************************************************************************
* Common Macro functions for word
*
IF INDEX(OPTIONS, 'R', 1) THEN
MACRO = MACRO:AM:'[AppRestore]'
END
IF INDEX(OPTIONS, 'P', 1) THEN
MACRO = MACRO:AM:'[FilePrint .Background = 1]'
END
IF INDEX(OPTIONS, 'S', 1) THEN
MACRO = MACRO:AM:'[FileSaveAll 1, 0]'
END
IF INDEX(OPTIONS, 'X', 1) THEN
MACRO = MACRO:AM:'[FileQuit]'
END
