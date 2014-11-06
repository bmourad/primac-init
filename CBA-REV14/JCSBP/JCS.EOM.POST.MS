   SUBROUTINE JCS.EOM.POST.MS
*COPY>CPYLIB>COM1
*COPY>JCS.CPYLIB>COM.JCS.LINK
*COPY>ICS.CPYLIB>COM.INV.MAIN  
*COPY>ICS.CPYLIB>COM.INV.SERIAL
*COPY>JCS.CPYLIB>COM.INV.STATS 
*COPY>PMC.CPYLIB>COM.CUST
*COPY>OPS.CPYLIB>COM.ORDER
*COPY>JCS.CPYLIB>COM.JCS.EOM.POST
*********************************************************************
* REVISION    - [08.0]
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
* SYSTEM      - PRIMAC
* SOURCE      - JCSBP
* PROGRAM     - JCS.EOM.POST.MS
* BY          - ZIAD YAMOUT, C.B.A
* DATE        - 02/28/87
* DESCRIPTION -
*T26334 epitka 12/19/2001 * REV12
*ENDDOC
*********************************************************************
*COPY>PMC.CPYLIB>POST.REJECTS
*COPY>JCS.CPYLIB>EOM.ACCT
*COPY>JCS.CPYLIB>JOB
*COPY>JCS.CPYLIB>JOB.MISC
*COPY>PMC.CPYLIB>WIP.SALES.STATS
*COPY>PMC.CPYLIB>EOM.TRANS
*COPY>CPYLIB>FILE.VARS
*COPY>CPYLIB>CHAR
   MAT ETR.REC = ''
   MCNT = COUNT(JOB.MS.SEQ,VM) + (JOB.MS.SEQ # '')
   CHECK.WIP = 0; NEW.REC = 0
   FOR M = 1 TO MCNT
      DPNO = JOB.MS.DEPT<1,M>; CCNO = JOB.MS.CCTR<1,M>
      GLS.DEPT = DPNO[1,2]
      MS.ID = CONO:JOBNO:"!":DPNO:"!":CCNO:"!"
      FOR S = 1 TO JOB.MS.SEQ<1,M>
         MATREADU JMS.REC FROM JOB.MISC, MS.ID:S ELSE
            MAT PRR.REC = ''
            PRR.JOB = JOBNO
            PRR.FILE = 'JOB.MISC'
            PRR.ID = MS.ID[4,999]:S
            PRR.ERR = 'CANNOT LOCATE'
            PRR.SEQ = PRR.SEQ + 1
            MATWRITE PRR.REC ON POST.REJECTS, PRR.SEQ
            GOTO 800
         END
         WIP.COST = JMS.WIP
         BEGIN CASE
         CASE JMS.MON = ""
            GOTO 800
         CASE JMS.MON<1,1> # PERIOD OR (JMS.GLA.DATE<1,1> # "" AND JMS.GLA.DATE<1,1> # "P")
            WCNT = COUNT(WIP.COST,VM) + 1
            WPTR = COUNT(WIP.COST<1,1>,SVM) + 1
            FOR W = 1 TO WPTR
               CHECK.WIP = CHECK.WIP + WIP.COST<1,1,W>
            NEXT W
            GOTO 600
         CASE WIP.COST = ""
            WCNT = 0; WPTR = 0
            WIP.CNTR = 0
         CASE JMS.COST < 0
            WCNT = COUNT(WIP.COST,VM) + 1
            WPTR = COUNT(WIP.COST<1,1>,SVM) + 1
            WIP.CNTR = 1
            TOT.WIP = 0
            FOR W = 1 TO WPTR
               TOT.WIP = TOT.WIP + WIP.COST<1,1,W>
            NEXT W
            CHECK.WIP = CHECK.WIP + TOT.WIP
            FOR I = 2 TO WCNT
               IF JMS.MON<1,I> = PERIOD AND (JMS.GLA.DATE<1,I> = "" OR JMS.GLA.DATE<1,I> = "P") THEN
                  FOR W = 1 TO WPTR
                     TOT.WIP = TOT.WIP + WIP.COST<1,I,W>
                  NEXT W
                  JMS.GLA.DATE<1,I> = "P"
               END
            NEXT I
            IF TOT.WIP = 0 THEN
               WIP.CNTR = 2
               WCNT = 0; WPTR = 0
               WIP.COST = ""
            END ELSE
               WIP.CNTR = 1
            END
         CASE 1
            WCNT = COUNT(WIP.COST,VM) + 1
            WPTR = COUNT(WIP.COST<1,1>,SVM) + 1
            WIP.CNTR = 1
            FOR W = 1 TO WPTR
               CHECK.WIP = CHECK.WIP + WIP.COST<1,1,W>
            NEXT W
         END CASE
         COG.COST = JMS.DCOST; COG.COST<2> = JMS.COST - JMS.DCOST
         FOR W = 1 TO 2
            BEGIN CASE
            CASE COG.COST<W> + 0 = 0 AND WIP.COST<1,1,W> + 0 = 0
               GOTO 500
            CASE WIP.COST<1,1,W> + 0 = 0
               BEGIN CASE
               CASE MS.COG.LEVEL<W> < 1
                  DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MS.COG.ACCT<W>
               CASE MS.COG.LEVEL<W> < 2
                  DB.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : MS.COG.ACCT<W>
               CASE MS.COG.LEVEL<W> < 3
                  DB.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : MS.COG.ACCT<W>
               CASE 1
                  DB.ID = JMS.DIV : GLS.DEPT : CCNO : MS.COG.ACCT<W>
               END CASE
               WIP.AMT = COG.COST<W>
            CASE 1
               BEGIN CASE
               CASE MS.WIP.LEVEL<W> < 1
                  DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE MS.WIP.LEVEL<W> < 2
                  DB.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE MS.WIP.LEVEL<W> < 3
                  DB.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE 1
                  DB.ID = JMS.DIV : GLS.DEPT : CCNO : MS.WIP.ACCT<W>
               END CASE
               WIP.AMT = WIP.COST<1,1,W>
            END CASE
            BEGIN CASE
            CASE MS.APL.LEVEL<W> < 1
               CR.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MS.APL.ACCT<W>
            CASE MS.APL.LEVEL<W> < 2
               CR.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : MS.APL.ACCT<W>
            CASE MS.APL.LEVEL<W> < 3
               CR.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : MS.APL.ACCT<W>
            CASE 1
               CR.ID = JMS.DIV : GLS.DEPT : CCNO : MS.APL.ACCT<W>
            END CASE
            GOSUB 1000
            JMS.GLA.DATE<1,1> = "P"
500      NEXT W
         GOSUB 2000      ;* COLLECT WIP FOR SALES STATS
600      FOR I = 2 TO WCNT
            IF JMS.MON<1,I> # PERIOD OR (JMS.GLA.DATE<1,I> # "" AND JMS.GLA.DATE<1,I> # "P") THEN
               GOTO 700
            END
            WIP.CNTR = 1
            FOR W = 1 TO WPTR
* CSF 21858
*                 IF JMS.FNGD<1,I> # "" THEN
               IF JMS.FNGD<1,I> = "*" THEN
                  BEGIN CASE
                  CASE WIP.CLR.LEVEL < 1
                     DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE WIP.CLR.LEVEL < 2
                     DB.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE WIP.CLR.LEVEL < 3
                     DB.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE 1
                     DB.ID = JMS.DIV : GLS.DEPT : CCNO : WIP.CLR.ACCT
                  END CASE
               END ELSE
                  BEGIN CASE
                  CASE MS.COG.LEVEL<W> < 1
                     DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MS.COG.ACCT<W>
                  CASE MS.COG.LEVEL<W> < 2
                     DB.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : MS.COG.ACCT<W>
                  CASE MS.COG.LEVEL<W> < 3
                     DB.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : MS.COG.ACCT<W>
                  CASE 1
                     DB.ID = JMS.DIV : GLS.DEPT : CCNO : MS.COG.ACCT<W>
                  END CASE
               END
               BEGIN CASE
               CASE MS.WIP.LEVEL<W> < 1
                  CR.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE MS.WIP.LEVEL<W> < 2
                  CR.ID = JMS.DIV : GEN.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE MS.WIP.LEVEL<W> < 3
                  CR.ID = JMS.DIV : GLS.DEPT : GEN.CCTR : MS.WIP.ACCT<W>
               CASE 1
                  CR.ID = JMS.DIV : GLS.DEPT : CCNO : MS.WIP.ACCT<W>
               END CASE
               WIP.AMT = 0 - WIP.COST<1,I,W>
               GOSUB 1000
               JMS.GLA.DATE<1,I> = "P"
            NEXT W
700      NEXT I
*CSF 26275 v
         MATWRITE JMS.REC ON JOB.MISC,MS.ID:S
*CSF 26275 ^
800      RELEASE JOB.MISC, MS.ID:S
      NEXT S
900 NEXT M
   CHK.WIP = 0
   FOR W = 1 TO 2
      CHK.WIP = CHK.WIP + JOB.MS.WIP<1,2,W> + JOB.MS.WIP<1,3,W>
   NEXT W
   IF CHECK.WIP <> CHK.WIP THEN
      MAT PRR.REC = ''
      PRR.JOB = JOBNO
      PRR.ERR = 'TOTAL MS = ':CHK.WIP:' AND SEQ SUM = ':CHECK.WIP
      PRR.SEQ = PRR.SEQ + 1
      MATWRITE PRR.REC ON POST.REJECTS, PRR.SEQ
   END
   GOTO 9999
1000 BEGIN CASE
   CASE WIP.AMT > 0
      ETR.DB = CONO : DB.ID : JOBNO : "!MS-D*" : NEW.REC
      ETR.CR = CONO : CR.ID : JOBNO : "!MS-C*" : NEW.REC
   CASE WIP.AMT + 0 < 0
      ETR.DB = CONO : DB.ID : JOBNO : "!MS-C*" : NEW.REC
      ETR.CR = CONO : CR.ID : JOBNO : "!MS-D*" : NEW.REC
   CASE 1
      GOTO 1999
   END CASE
   MATREAD ETR.REC FROM EOM.TRANS, ETR.DB ELSE
      MAT ETR.REC = ''
      ETR.AMT = 0
      ETR.REF = JOB.CUST
   END
   ETR.CNTR = ETR.CNTR + WIP.CNTR
   WIP.CNTR = 0
   ETR.AMT = ETR.AMT + WIP.AMT
   IF NO.DETAIL OR WIP.AMT + 0 = 0 THEN
      MATWRITE ETR.REC ON EOM.TRANS, ETR.DB
      MATREAD ETR.REC FROM EOM.TRANS, ETR.CR ELSE
         MAT ETR.REC = ''
         ETR.AMT = 0
         ETR.REF = JOB.CUST
      END
      ETR.AMT = ETR.AMT - WIP.AMT
      MATWRITE ETR.REC ON EOM.TRANS, ETR.CR
      GOTO 1999
   END
   LOCATE JMS.SEQ IN ETR.TRAN<1>,1 BY "AR" SETTING PTR ELSE NULL
   INS JMS.SEQ BEFORE ETR.TRAN<1,PTR>
   INS JMS.DATE BEFORE ETR.DATE<1,PTR>
   INS WIP.AMT BEFORE ETR.TAMT<1,PTR>
   MATWRITE ETR.REC ON EOM.TRANS, ETR.DB
   MATREAD ETR.REC FROM EOM.TRANS, ETR.CR ELSE
      MAT ETR.REC = ''
      ETR.AMT = 0
      ETR.REF = JOB.CUST
   END
   ETR.AMT = ETR.AMT - WIP.AMT
   LOCATE JMS.SEQ IN ETR.TRAN<1>,1 BY "AR" SETTING PTR ELSE NULL
   INS JMS.SEQ BEFORE ETR.TRAN<1,PTR>
   INS JMS.DATE BEFORE ETR.DATE<1,PTR>
   INS (0 - WIP.AMT) BEFORE ETR.TAMT<1,PTR>
   MATWRITE ETR.REC ON EOM.TRANS, ETR.CR
   IF PTR > 99 THEN NEW.REC = NEW.REC + 1
1999 RETURN
*--------
*
2000  * GET AND UPDATE THE WIP.SALES.STATS RECORD
*
*--------
   WSS.KEY = CONO:"_J_":JOBNO:"_":PERIOD:"XX_0_0"
   MATREADU WSS.REC FROM WIP.SALES.STATS, WSS.KEY ELSE
      MAT WSS.REC=''
   END
   IF JMS.DCOST > JMS.WIP<1,1,1> THEN
      WSS.MISC.DCOST = WSS.MISC.DCOST + (JMS.DCOST - JMS.WIP<1,1,1>)
   END
   T.FIXED = JMS.COST - JMS.DCOST
   IF T.FIXED > JMS.WIP<1,1,2> THEN
      WSS.MISC.FFOH = WSS.MISC.FFOH + (T.FIXED - JMS.WIP<1,1,2>)
   END
   WSS.INVOICE.DATE = DATE()
   WSS.CUST.ID = JOB.CUST
   WSS.SLSM.ID = JOB.SLSMN
   WSS.SALES.CODE = JOB.SALES.CODE
   WSS.JOB.CAT = JOB.CATG
   WSS.MASTER = JOB.MASTER
   WSS.NO.COLORS = JOB.COLORS
   WSS.POSTING.MONTH = PERIOD
   MATWRITE WSS.REC ON WIP.SALES.STATS, WSS.KEY
9999 RETURN
   END
**************************************************************************
**************************************************************************
**************************** EOP *****************************************
**************************************************************************
**************************************************************************
