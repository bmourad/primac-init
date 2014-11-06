*********************************************************************
*
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
*
* PROGRAM  - BLD.SCN.FRM
*
* AUTHOR   - NICK AMENDOLA, COMPUTER BUSINESS ASSOCIATES
*
* DATE     - 07/30/90
*
* DESCRIPTION
*
* This program creates the *FRM and *CLR items for the screen files
* selected.
*
*********************************************************************
*
*COPY>CPYLIB>ALT.BUILD.SCREEN
*COPY>CPYLIB>CHAR
*
*---- DEFINE VARIABLES
*
      DIM FORM(24)
      DIM CLRS(24,2)
*
*---- INITIALIZATION
*
      MIN.CRT.ROW = 0
      MAX.CRT.ROW = 22
      MIN.CRT.COL = 0
      MAX.CRT.COL = 79
      MAX.WIDTH = MAX.CRT.COL - MIN.CRT.COL + 1
      SPACE.WIDTH = SPACE(MAX.WIDTH)
      ADR.CNT = 4
*
*---- GET FILE PREFIXES FOR FILES TO BE CONVERTED
*
10 *
      PROMPT ""
      PRINT @(-1)
      OPEN "","VOC" TO MD ELSE
         ERRMSG = "CANNOT OPEN MASTER DICTIONARY"
         GOSUB 91000
         STOP
      END
      PRINT ' This program will create the "*FRM" and "*CLR" items'
      PRINT " for the screen files selected."
      PRINT
      PRINT " These item are required for terminal independence."
      PRINT
      PRINT " Please standby. Building file list..."
      SELECT MD
      MLEN = 0
      PCNT = 0
      SPFXS = ""
      DONE = 0
      LOOP
         READNEXT ID ELSE DONE = 1
      UNTIL DONE DO
         IF FIELD(ID,".",2) = "SCREENS" THEN
            OPEN "",ID TO FID ELSE GOTO 19
            SPFX = FIELD(ID,".",1)
            IF LEN(SPFX) > MLEN THEN MLEN = LEN(SPFX)
            PCNT = PCNT + 1
            LOCATE SPFX IN SPFXS,1 BY "AL" SETTING P ELSE NULL
            SPFXS = INSERT(SPFXS,P,0,0,SPFX)
         END
19    REPEAT
*
20    PRINT
      PRINT "Files prefixes available:"
      PRINT
      FMT = "L#":(MLEN+2)
      CCNT = INT(80/(MLEN+2))
      LCNT = INT(PCNT / CCNT + 0.99)
      FOR LPTR = 1 TO LCNT
         FOR CPTR = 1 TO CCNT
            P = (LPTR-1)*CCNT+CPTR
            PRINT SPFXS<P> FMT:
         NEXT CPTR
         PRINT
      NEXT LPTR
      PRINT
      ERRMSG = ""
      PRINT 'Enter "ALL" or file name prefixes separated by commas:'
      PRINT
      INPUT NPFXS
      BEGIN CASE
      CASE NPFXS = "END"
         GOTO 99999
      CASE NPFXS = CHAR(27)
         GOTO 99999
      CASE NPFXS = "^"
         GOTO 99999
      CASE NPFXS = "ALL"
         PFXS = SPFXS
      CASE 1
         FCNT = DCOUNT(NPFXS,",")
         FOR FPTR = 1 TO FCNT
            NPFX = TRIM(FIELD(NPFXS,",",FPTR))
            LOCATE NPFX IN SPFXS,1 SETTING P ELSE
               ERRMSG = "Invalid screen file - ":NPFX:".SCREENS"
               PRINT ERRMSG
            END
         NEXT FPTR
         IF ERRMSG # "" THEN GOTO 20
         PFXS = ""
         FOR FPTR = 1 TO FCNT
            PFXS<FPTR> = TRIM(FIELD(NPFXS,",",FPTR))
         NEXT FPTR
      END CASE
*
*---- OPEN ALL THE FILES AND PROCESS
*
30 *
      FOR PPTR = 1 TO PCNT
         FILENAME = PFXS<PPTR>:".SCREENS"
         OPEN "",FILENAME TO SYS.SCREENS ELSE GOTO 39
         PRINT @(-1)
         PRINT "Processing ":FILENAME
         GOSUB 100
39    NEXT PPTR
      GOTO 99999
*
*---- MAIN PROCESSING
100 *
      SELECT SYS.SCREENS
      DATA = 1
      LOOP
         READNEXT FRM.ID ELSE DATA = 0
      WHILE DATA DO
         IF FIELD(FRM.ID,"*",2) # "DEF" THEN GOTO 999
         READU BLD.DEF.SCRN FROM SYS.SCREENS, FRM.ID ELSE
            GOTO 999
         END
         SCREEN.NAME = FIELD(FRM.ID,"*",1)
         FIRST.ROW = 0
         LAST.ROW = 22
         FIRST.COL = 0
         LAST.COL = 79
         CLR.R1 = 0
         CLR.R2 = 23
         CLR.C1 = 0
         CLR.C2 = 79
         DATA.ROW = ""
         DATA.COL1 = FIRST.COL
         DATA.COL2 = LAST.COL
         COL.REF.FLAG = 0
         ROW.REF.FLAG = 0
         GOSUB 6100
         BLD.COMP.SCRN = ""
         GOSUB 5300
         PRINT BLD.COMP.SCRN:
         READU DUMMY FROM SYS.SCREENS, SCREEN.NAME:"*FRM" ELSE NULL
*        RELEASE SYS.SCREENS, SCREEN.NAME:"*FRM"
         WRITE BLD.COMP.SCRN ON SYS.SCREENS, SCREEN.NAME:"*FRM"
         MATREAD ABLD.SCRN FROM SYS.SCREENS, SCREEN.NAME:"*FLD" THEN
            GOSUB 5500
            READU DUMMY FROM SYS.SCREENS, SCREEN.NAME:"*CLR" ELSE NULL
*           RELEASE SYS.SCREENS, SCREEN.NAME:"*CLR"
            WRITE BLD.CLR.DATA ON SYS.SCREENS, SCREEN.NAME:"*CLR"
            READU DUMMY FROM SYS.SCREENS, SCREEN.NAME:"*DCL" ELSE NULL
            RELEASE SYS.SCREENS, SCREEN.NAME:"*DCL"
*           WRITE BLD.DEF.CLR ON SYS.SCREENS, SCREEN.NAME:"*DCL"
         END ELSE
            MAT ABLD.SCRN = ""
         END
         GOSUB 7000; PRINT
999 *
         RELEASE SYS.SCREENS, FRM.ID
      REPEAT
      RETURN
*
*---- Build BLD.COMP.SCRN record
5300 *
      BEGIN CASE
      CASE CLR.R1 <= MIN.CRT.ROW AND CLR.R2 >= MAX.CRT.ROW AND CLR.C1 <= MIN.CRT.COL AND CLR.C2 >= MAX.CRT.COL
         BLD.COMP.SCRN = @(-1)
      CASE CLR.C2 = MAX.CRT.COL
         BLD.COMP.SCRN = ""
         FOR ROW = CLR.R1 TO CLR.R2
            BLD.COMP.SCRN = BLD.COMP.SCRN:@(CLR.C1,ROW):@(-4)
         NEXT ROW
      CASE 1
         BLD.COMP.SCRN = ""
         FOR ROW = CLR.R1 TO CLR.R2
            BLD.COMP.SCRN = BLD.COMP.SCRN:@(CLR.C1,ROW):SPACE(CLR.C2-CLR.C1+1)
         NEXT ROW
      END CASE
      FOR DEF = 1 TO DPTR
         BLD.COMP.SCRN = BLD.COMP.SCRN:@(BLD.DEF.SCRN<DEF,2>,BLD.DEF.SCRN<DEF,1>):BLD.DEF.SCRN<DEF,3>
      NEXT DEF
      RETURN
*
*---- DERIVE STRING TO CLEAR DATA ONLY
*
5500 *
      MAT CLRS = ""
      FOR ROW = FIRST.ROW TO LAST.ROW
         DONE = 0
         FOR COL = LAST.COL TO FIRST.COL STEP -1 UNTIL DONE
            IF FORM(ROW+1)[COL+1,1] # " " THEN
               CLRS(ROW+1,2) = COL + 1
               DONE = 1
            END
         NEXT COL
      NEXT ROW
      FOR FLD = 1 TO BLD.FIELD.CNT
         ROW = ABLD.SCRN(FLD)<1,B.Y>
         COL = ABLD.SCRN(FLD)<1,B.X>
         ML = ABLD.SCRN(FLD)<1,B.MULTI.LN>
         IF ML = "" THEN ML = 1
         LS = ABLD.SCRN(FLD)<1,B.MULTI.SP>
         IF LS = "" THEN LS = 1
         FOR M = 1 TO ML STEP LS
            MROW = ROW + (M-1) * LS
            IF COL < CLRS(MROW+1,2) THEN
               CLR.FLD = MROW
               CLR.FLD<1,2> = COL
               CLR.FLD<1,3> = ABLD.SCRN(FLD)<1,B.MAXL>
               CLRS(MROW+1,1)<-1> = CLR.FLD
            END
         NEXT M
      NEXT FLD
      BLD.DEF.CLR = ""; DLOC = 0
      FOR ROW = FIRST.ROW TO LAST.ROW
         MROW = ROW + 1; FLOC = 1
         LOOP WHILE CLRS(MROW,1)<FLOC> # "" DO
            DLOC = DLOC + 1
            BLD.DEF.CLR<DLOC> = CLRS(MROW,1)<FLOC>
            BLD.DEF.CLR<DLOC> = CLRS(MROW,1)<FLOC>
            FLOC = FLOC + 1
         REPEAT
         COL = CLRS(MROW,2) + 0
         IF COL < 80 THEN
            DLOC = DLOC + 1
            BLD.DEF.CLR<DLOC> = ROW
            BLD.DEF.CLR<DLOC,2> = COL
            BLD.DEF.CLR<DLOC,3> = "L"
         END
      NEXT ROW
      BLD.CLR.DATA = ""
      FOR ROW = 1 TO DLOC
         Y = BLD.DEF.CLR<ROW,1>
         X = BLD.DEF.CLR<ROW,2>
         ML = BLD.DEF.CLR<ROW,3>
         IF NUM(ML) THEN
            BLD.CLR.DATA = BLD.CLR.DATA:@(X,Y):SPACE(ML)
         END ELSE
            BLD.CLR.DATA = BLD.CLR.DATA:@(X,Y):@(-4)
         END
      NEXT ROW
      RETURN
*
6100 *
      MAT FORM = SPACE.WIDTH
      DPTR = DCOUNT(BLD.DEF.SCRN,AM)
      FOR DEF = 1 TO DPTR
         Y = BLD.DEF.SCRN<DEF,1> + 1
         X = BLD.DEF.SCRN<DEF,2>
         STMT = BLD.DEF.SCRN<DEF,3>
         L = LEN(STMT)
         FORM(Y) = FORM(Y)[1,X]:STMT:FORM(Y)[X+L+1,MAX.WIDTH-X-1]
      NEXT DEF
      RETURN
*
*---- DISPLAY DATA FIELDS
7000 *
      FOR FLD = 1 TO BLD.FIELD.CNT
         FX = ABLD.SCRN(FLD)<1,B.X>
         IF FX = "" THEN GOTO 7090
         FY = ABLD.SCRN(FLD)<1,B.Y>
         IF FY > LAST.ROW THEN GOTO 7090
         ML = ABLD.SCRN(FLD)<1,B.MULTI.LN>
         IF ML = "" THEN ML = 1
         LS = ABLD.SCRN(FLD)<1,B.MULTI.SP>
         IF LS = "" THEN LS = 1
         IF DATA.ROW = "" OR (DATA.ROW >= FY AND DATA.ROW <= (FY+(ML-1)*LS)) THEN
            IF DATA.COL1 < FX AND DATA.COL2 < FX THEN GOTO 7090
            IF DATA.COL1 >= FX+ABLD.SCRN(FLD)<1,B.MAXL> THEN GOTO 7090
            IF ABLD.SCRN(FLD)<1,B.MASK> = "" THEN
               DFILL = "#"
            END ELSE
               DFILL = ABLD.SCRN(FLD)<1,B.MASK>
            END
            PTYP = ABLD.SCRN(FLD)<1,B.TYP>
            PMAXL = ABLD.SCRN(FLD)<1,B.MAXL>
            BEGIN CASE
            CASE PTYP = 6
               PMASK = STR(DFILL,2):"/":STR(DFILL,2):"/":STR(DFILL,2)
               PMASK = PMASK[1,PMAXL]
            CASE PTYP = 4
               PSCALER = ABLD.SCRN(FLD)<1,B.SCALER>
               PMASK = STR(DFILL,PMAXL-PSCALER-1):".":STR(DFILL,PSCALER)
            CASE PTYP = 10
               PMASK = STR(DFILL,2):":":STR(DFILL,4)
               PMASK = PMASK[1,PMAXL]
            CASE 1
               PMASK = STR(DFILL,PMAXL)
            END CASE
            FOR L = 1 TO ML
               PRINT @(FX,FY+(L-1)*LS):PMASK:
            NEXT L
         END
7090 *
      NEXT FLD
      DATA.ROW = ""
      DATA.COL1 = FIRST.COL
      DATA.COL2 = LAST.COL
      RETURN
*
*---- ERROR PROCESS
91000 *
      PRINT @(0,23):@(-4):ERRMSG:
      INPUT REPLY,1:
      PRINT @(0,23):@(-4):
      RETURN
*
*---- END OF PROGRAM
99999 *
      END
