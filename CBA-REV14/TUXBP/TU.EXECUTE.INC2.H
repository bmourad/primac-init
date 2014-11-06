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
*-* R83, Prime
*-* Convert VERB to SMA.VERB if required, PI/Open only
!@!SMA.VERB = VERB ; !*PI/O*!
!@!OPEN '','MD' TO F.MD THEN ; !*PI/O*!
!@!READV VTYPE FROM F.MD,'SMA.':VERB,1 THEN ; !*PI/O*!
!@!IF NOT(INDEX('F,K,D', VTYPE[1,1], 1)) THEN SMA.VERB = 'SMA.':VERB ; !*PI/O*!
!@!END ; !*PI/O*!
!@!END ; !*PI/O*!
*-* Adds Version 3
!@!DTA = '' ; !*ADDS*!
!@!IF NOT(DTA.FLG) AND SYSTEM(11) THEN ; !*ADDS*!
!@!OPEN '', 'MD' TO F.MD THEN ; !*ADDS*!
!@!DTA = VERB ; VERB = '' ; Z = '' ; !*ADDS*!
!@!GOSUB 5000; * select to SELVAR ; !*ADDS*!
!@!LOOP ; !*ADDS*!
!@!READNEXT ID FROM SELVAR ELSE ID = AM ; !*ADDS*!
!@!UNTIL ID = AM DO ; !*ADDS*!
!@!Z<-1> = ID ; !*ADDS*!
!@!REPEAT ; !*ADDS*!
!@!WRITE Z ON F.MD,'$LIST':PORTNO ; !*ADDS*!
!@!VERB = 'QSELECT MD $LIST':PORTNO ; !*ADDS*!
!@!DATA DTA ; !*ADDS*!
!@!END ; !*ADDS*!
!@!END ; !*ADDS*!
BEGIN CASE
CASE CAPT.FLG AND ERR.FLG
EXECUTE VERB CAPTURING CAPT RETURNING ERNO; !*-MRX*!
CASE CAPT.FLG
EXECUTE VERB CAPTURING CAPT; !*-MRX*!
CASE ERR.FLG
EXECUTE VERB RETURNING ERNO; !*-MRX*!
CASE 1
EXECUTE VERB; !*-MRX*!
END CASE
