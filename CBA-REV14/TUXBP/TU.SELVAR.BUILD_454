$BASICTYPE 'U'
SUBROUTINE TU.SELVAR.BUILD(F.FILEVAR, SELVAR, BUILD.TYPE)
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
**************************************************************************
* This routine has been written to get around the syntax for READLIST
* on the BASICTYPE 'P' version. It is passed a BUILD.TYPE and based on that
* modifies SELVAR accordingly.
*
* Valid values for BUILD.TYPE are :
*       1. IF SYSTEM(11) THEN SELECT TO SELVAR
*       2. IF SYSTEM(11) THEN SELECT TO SELVAR ELSE SELVAR = ''
*       3. SELECT TO SELVAR
*       4. SELECT F.FILEVAR TO SELVAR
*
**************************************************************************
BEGIN CASE
CASE BUILD.TYPE = 1; IF SYSTEM(11) THEN READLIST SELVAR ELSE NULL
CASE BUILD.TYPE = 2; READLIST SELVAR ELSE SELVAR = ''
CASE BUILD.TYPE = 3; READLIST SELVAR ELSE SELVAR = ''
CASE BUILD.TYPE = 4; SELECT F.FILEVAR; READLIST SELVAR ELSE SELVAR = ''
END CASE
*
RETURN
*
END
