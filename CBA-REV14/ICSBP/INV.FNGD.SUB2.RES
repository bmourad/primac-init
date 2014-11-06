      SUBROUTINE INV.FNGD.SUB2(CONO,PDNO,NEWREC,STATUS)
*COPY>CPYLIB>COM1
*COPY>ICS.CPYLIB>COM.INVENTORY
*********************************************************************
* REVISION    - [08.0]
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
* SYSTEM      - PRIMAC
* SOURCE      - ICSBP
* PROGRAM     - INV.FNGD.SUB2
* BY          - JULIANNE RIVERA, C.B.A
* DATE        - 08/09/91
* DESCRIPTION
* This program is use to maintain bill of materials for a
* given product.
*   SFC 3-6-96 MODIFIED FOR NEW VERSION OF ROLL LABEL ESTIMATING
*********************************************************************
*
*---- Data Structure Libraries
*
*COPY>ICS.CPYLIB>INVENTORY
*COPY>ICS.CPYLIB>INV.FNGD
*COPY>JES.CPYLIB>ESTIMATE.RES.DIE
*COPY>CPYLIB>EDIT.COM
*COPY>CPYLIB>EDIT.COM.DRIVER
*COPY>CPYLIB>FILE.VARS
*COPY>CPYLIB>GEN.XREF.SUB
*COPY>CPYLIB>SYSCOM
*COPY>CPYLIB>CHAR
*
*---- Initialize SYSCOM
*
      SYS.TYPE = 1
      CALL SYSCOM(MAT SYSCOM.REC)
*
*---- Setup GEN.XREF
*
*      MAT GEN.XREF.REC = ""
*      GXR.CO = CONO
*
      OPEN "","ESTIMATE.RES.DIE" TO ESTIMATE.RES.DIE ELSE RETURN
*---- Initialization
*
      LINE.SPACE = 2
      BEGIN.PAGE = 7
      PAGE.SIZE = 9
*
      ESN = ECD.SCRN.NO
*
      DIE.CNT = DCOUNT(IOF.DIE,VM)
*
*---- Display Data
100 *
      ECD.NUM = 1; SCV.REC(ECD.NUM)<ESN> = PDNO
      ECD.ACTION = 5; CALL SCRN.EDIT
      ECD.NUM = 2; SCV.REC(ECD.NUM)<ESN> = INV.FULL.DESC
      ECD.ACTION = 5; CALL SCRN.EDIT
*
*---- Die
*
      IF DIE.CNT = 0 AND NEWREC THEN
         OPTION = 1
         GOSUB 1000
      END ELSE
         LN = 1
         GOSUB 80000
      END
500 *
*
*---- Prompt Line
*
      MORE = 1
      LOOP
         IF NEWREC THEN ECD.NUM = 55 ELSE ECD.NUM = 51
         SCV.REC(ECD.NUM)<ESN> = ""
         ECD.ACTION = 4; CALL SCRN.EDIT
         OPTION = ECD.RET.VALUE
         BEGIN CASE
            CASE OPTION = "E" OR OPTION = "END"
               IF NEWREC THEN
                  ECD.NUM = 56; SCV.REC(ECD.NUM)<ESN> = ""
                  ECD.ACTION = 4; CALL SCRN.EDIT
                  IF ECD.RET.VALUE = "Y" THEN
                     STATUS = "END"
                     MORE = 0
                  END
               END ELSE
                  MORE = 0
               END
            CASE OPTION = "N" OR OPTION = "P" 
               IF IOF.DIE<1,1> = '' THEN
                  ERRMSG = 'PLEASE ENTER PRIMARY DIE'
                  GOSUB 91000
               END ELSE
                  STATUS = OPTION
                  MORE = 0
               END
            CASE OPTION = "D"
               ECD.NUM = 52; SCV.REC(ECD.NUM)<ESN> = ''
               ECD.ACTION = 4; CALL SCRN.EDIT
               DLINE = ECD.RET.VALUE
               IF IOF.DIE<1,DLINE> # '' THEN
                  DEL IOF.DIE<1,DLINE>
                  DEL SCV.REC(3)<ESN,DLINE>
                  DEL SCV.REC(4)<ESN,DLINE>
                  DEL SCV.REC(5)<ESN,DLINE>
                  DEL SCV.REC(6)<ESN,DLINE>
                  DEL SCV.REC(7)<ESN,DLINE>
                  DEL SCV.REC(8)<ESN,DLINE>
                  OLD.START = 0; LN = 1
                  GOSUB 81000
               END
            CASE NOT(NUM(OPTION))
               ERRMSG = "Invalid Entry, try again !!"
               GOSUB 91000
*            CASE OPTION = 1
*               MORE = 0
            CASE OPTION < 1 OR OPTION > 5
               ERRMSG = "Out Of Range !!"
               GOSUB 91000
            CASE 1
               GOSUB 1000
         END CASE
      WHILE MORE DO REPEAT
      GOTO 99999
*
*---- Die Entry
1000 *
      REF.NO = OPTION
      ECD.NUM = 3; ECD.SUB.NUM = REF.NO
      ECD.ACTION = 4; CALL SCRN.EDIT
      IF ECD.RET.VALUE = "" OR ECD.RET.VALUE = "END" THEN RETURN
      DIENO = ECD.RET.VALUE
      MATREAD RLDIE.REC FROM ESTIMATE.RES.DIE, CONO:DIENO ELSE
         ERRMSG = "Cannot locate Die # ":DIENO
         GOSUB 91000; GOTO 1000
      END
      IOF.DIE<1,REF.NO> = DIENO
      SCV.REC(3)<ESN,REF.NO> = DIENO
      SCV.REC(4)<ESN,REF.NO> = RLDIE.REPEAT
      SCV.REC(5)<ESN,REF.NO> = RLDIE.NO.ACROSS
      SCV.REC(6)<ESN,REF.NO> = RLDIE.NO.AROUND
      SCV.REC(7)<ESN,REF.NO> = RLDIE.SZ.ACROSS
      SCV.REC(8)<ESN,REF.NO> = RLDIE.SZ.AROUND
      OLD.START = 0; LN = 1
      GOSUB 81000
      RETURN
*****************
80000 *
      DIE.CNT = DCOUNT(IOF.DIE,VM)
      FOR I = 1 TO DIE.CNT
         SCV.REC(3)<ESN,I> = IOF.DIE<1,I>
         IF IOF.DIE<1,I> = '' THEN
            MAT RLDIE.REC = ''
         END ELSE
            MATREAD RLDIE.REC FROM ESTIMATE.RES.DIE,CONO:IOF.DIE<1,I> ELSE
               ERRMSG = "NOT IN DIE FILE"; GOSUB 91000
               MAT RLDIE.REC = ""
            END
         END
         SCV.REC(4)<ESN,I> = RLDIE.REPEAT
         SCV.REC(5)<ESN,I> = RLDIE.NO.ACROSS
         SCV.REC(6)<ESN,I> = RLDIE.NO.AROUND
         SCV.REC(7)<ESN,I> = RLDIE.SZ.ACROSS
         SCV.REC(8)<ESN,I> = RLDIE.SZ.AROUND
      NEXT I
*
*---- Display Die Scrolling Area
81000 *
      START.LINE = 1 + INT((LN-1)/PAGE.SIZE) * PAGE.SIZE
      LAST.LINE = START.LINE + PAGE.SIZE -1
      CNT = 1
      FOR N = START.LINE TO LAST.LINE
         SLN = BEGIN.PAGE + LINE.SPACE * MOD(N-1,PAGE.SIZE)
         P_X  = 13 ; P_Y = SLN ; P_VALUE = IOF.DIE<1,N>"L#9" ; P_OPT = "CL"
         P_X  := AM:24 ; P_Y := AM:SLN ; P_VALUE := AM:OCONV(SCV.REC(4)<ESN,N>,"MD4") "R#7"
         P_X  := AM:33 ; P_Y := AM:SLN ; P_VALUE := AM:SCV.REC(5)<ESN,N> "R#2"
         P_X  := AM:37 ; P_Y := AM:SLN ; P_VALUE := AM:SCV.REC(6)<ESN,N> "R#2"
         P_X  := AM:41 ; P_Y := AM:SLN ; P_VALUE := AM:OCONV(SCV.REC(7)<ESN,N>,"MD4") "R#7"
         P_X  := AM:50 ; P_Y := AM:SLN ; P_VALUE := AM:OCONV(SCV.REC(8)<ESN,N>,"MD4") "R#7"
         CALL VSI_PLINE(P_X,P_Y,P_VALUE,P_OPT)
         CNT = CNT + 1
      NEXT N
      FOR N = CNT TO PAGE.SIZE
         SLN = BEGIN.PAGE + LINE.SPACE * MOD(N-1,PAGE.SIZE)
         P_X  = 0 ; P_Y = SLN ; P_VALUE = "" ; P_OPT = "CL"
         CALL VSI_PLINE(P_X,P_Y,P_VALUE,P_OPT)
      NEXT N
      RETURN
*
*---- Error Message Routines
*
91000 ERR.TYPE = 1
      CALL SYSCOM(MAT SYSCOM.REC)
      RETURN
99999 *
      ECD.ACTION=99;CALL SCRN.EDIT
      RETURN
   END
