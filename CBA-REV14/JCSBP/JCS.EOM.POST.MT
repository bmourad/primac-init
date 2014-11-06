   SUBROUTINE JCS.EOM.POST.MT
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
* PROGRAM     - JCS.EOM.POST.MT
* BY          - ZIAD YAMOUT, C.B.A
* DATE        - 02/28/87
* 
* TASK
*     19256 10/12/95 LLH - EOD PROCESS
* DESCRIPTION -
*********************************************************************
*COPY>PMC.CPYLIB>GLTABLE
*COPY>JCS.CPYLIB>EOM.ACCT
*COPY>JCS.CPYLIB>JOB
*COPY>PMC.CPYLIB>POST.REJECTS
*COPY>JCS.CPYLIB>JOB.MATL
*COPY>PMC.CPYLIB>WIP.SALES.STATS
*COPY>PMC.CPYLIB>EOM.TRANS
*COPY>ICS.CPYLIB>INVENTORY
*COPY>ICS.CPYLIB>CATEGORY
*COPY>PMC.CPYLIB>COA
*COPY>CPYLIB>FILE.VARS
*COPY>CPYLIB>CHAR
   MAT ETR.REC = ''
   MCNT = COUNT(JOB.MT.SEQ,VM) + (JOB.MT.SEQ # '')
   CHECK.WIP = 0; NEW.REC = 0
   FOR M = 1 TO MCNT
      DPNO = JOB.MT.DEPT<1,M>; CCNO = JOB.MT.CCTR<1,M>
      PDNO = JOB.MT.PROD<1,M>; WHNO = JOB.MT.WHSE<1,M>
      GLS.DEPT = DPNO[1,2]
      MT.ID = CONO:JOBNO:"!":DPNO:"!":CCNO:"!":PDNO:"!":WHNO:"!"
      MATREAD INV.REC FROM INVENTORY, CONO : PDNO ELSE MAT INV.REC = ''
      IF INV.LINE = '' THEN
         MAT CATG.REC = ''
      END ELSE
         MATREAD CATG.REC FROM CATEGORY, CONO : INV.LINE ELSE MAT CATG.REC = ''
      END
      BEGIN CASE
      CASE CATG.WIP = MT.WIP.ACCT<1>
      CASE CATG.WIP # ""
         CATG.WIP = CATG.WIP : STR("0",IN.ACCT.LEN-LEN(CATG.WIP))
         CATG.WIP = CATG.WIP[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<1> = CATG.WIP
         MT.WIP.LEVEL<1> = COA.LEVEL
      CASE GLTB.MT.DIR.WIP = MT.WIP.ACCT<1>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.DIR.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<1> = GLTB.MT.DIR.WIP
         MT.WIP.LEVEL<1> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.INV = MT.INV.ACCT<1>
      CASE CATG.INV # ""
         CATG.INV = CATG.INV : STR("0",IN.ACCT.LEN-LEN(CATG.INV))
         CATG.INV = CATG.INV[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.INV ELSE COA.LEVEL = 0
         MT.INV.ACCT<1> = CATG.INV
         MT.INV.LEVEL<1> = COA.LEVEL
      CASE GLTB.INV = MT.INV.ACCT<1>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.INV ELSE COA.LEVEL = 0
         MT.INV.ACCT<1> = GLTB.INV
         MT.INV.LEVEL<1> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.COG = MT.COG.ACCT<1>
      CASE CATG.COG # ""
         CATG.COG = CATG.COG : STR("0",IN.ACCT.LEN-LEN(CATG.COG))
         CATG.COG = CATG.COG[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.COG ELSE COA.LEVEL = 0
         MT.COG.ACCT<1> = CATG.COG
         MT.COG.LEVEL<1> = COA.LEVEL
      CASE GLTB.MT.DIR.COG = MT.COG.ACCT<1>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.DIR.COG ELSE COA.LEVEL = 0
         MT.COG.ACCT<1> = GLTB.MT.DIR.COG
         MT.COG.LEVEL<1> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.FOH.WIP = MT.WIP.ACCT<2>
      CASE CATG.FOH.WIP # ""
         CATG.FOH.WIP = CATG.FOH.WIP : STR("0",IN.ACCT.LEN-LEN(CATG.FOH.WIP))
         CATG.FOH.WIP = CATG.FOH.WIP[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.FOH.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<2> = CATG.FOH.WIP
         MT.WIP.LEVEL<2> = COA.LEVEL
      CASE GLTB.MT.FOH.WIP = MT.WIP.ACCT<2>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.FOH.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<2> = GLTB.MT.FOH.WIP
         MT.WIP.LEVEL<2> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.FOH.APL = MT.INV.ACCT<2>
      CASE CATG.FOH.APL # ""
         CATG.FOH.APL = CATG.FOH.APL : STR("0",IN.ACCT.LEN-LEN(CATG.FOH.APL))
         CATG.FOH.APL = CATG.FOH.APL[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.FOH.APL ELSE COA.LEVEL = 0
         MT.INV.ACCT<2> = CATG.FOH.APL
         MT.INV.LEVEL<2> = COA.LEVEL
      CASE GLTB.MT.FOH.APL = MT.INV.ACCT<2>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.FOH.APL ELSE COA.LEVEL = 0
         MT.INV.ACCT<2> = GLTB.MT.FOH.APL
         MT.INV.LEVEL<2> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.FOH = MT.COG.ACCT<2>
      CASE CATG.FOH # ""
         CATG.FOH = CATG.FOH : STR("0",IN.ACCT.LEN-LEN(CATG.FOH))
         CATG.FOH = CATG.FOH[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.FOH ELSE COA.LEVEL = 0
         MT.COG.ACCT<2> = CATG.FOH
         MT.COG.LEVEL<2> = COA.LEVEL
      CASE GLTB.MT.FOH.COG = MT.COG.ACCT<2>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.FOH.COG ELSE COA.LEVEL = 0
         MT.COG.ACCT<2> = GLTB.MT.FOH.COG
         MT.COG.LEVEL<2> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.VOH.WIP = MT.WIP.ACCT<3>
      CASE CATG.VOH.WIP # ""
         CATG.VOH.WIP = CATG.VOH.WIP : STR("0",IN.ACCT.LEN-LEN(CATG.VOH.WIP))
         CATG.VOH.WIP = CATG.VOH.WIP[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.VOH.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<3> = CATG.VOH.WIP
         MT.WIP.LEVEL<3> = COA.LEVEL
      CASE GLTB.MT.VOH.WIP = MT.WIP.ACCT<3>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.VOH.WIP ELSE COA.LEVEL = 0
         MT.WIP.ACCT<3> = GLTB.MT.VOH.WIP
         MT.WIP.LEVEL<3> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.VOH.APL = MT.INV.ACCT<3>
      CASE CATG.VOH.APL # ""
         CATG.VOH.APL = CATG.VOH.APL : STR("0",IN.ACCT.LEN-LEN(CATG.VOH.APL))
         CATG.VOH.APL = CATG.VOH.APL[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.VOH.APL ELSE COA.LEVEL = 0
         MT.INV.ACCT<3> = CATG.VOH.APL
         MT.INV.LEVEL<3> = COA.LEVEL
      CASE GLTB.MT.VOH.APL = MT.INV.ACCT<3>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.VOH.APL ELSE COA.LEVEL = 0
         MT.INV.ACCT<3> = GLTB.MT.VOH.APL
         MT.INV.LEVEL<3> = COA.LEVEL
      END CASE
      BEGIN CASE
      CASE CATG.VOH = MT.COG.ACCT<3>
      CASE CATG.VOH # ""
         CATG.VOH = CATG.VOH : STR("0",IN.ACCT.LEN-LEN(CATG.VOH))
         CATG.VOH = CATG.VOH[1,IN.ACCT.LEN]
         MATREAD COA.REC FROM COA, CONO : CATG.VOH ELSE COA.LEVEL = 0
         MT.COG.ACCT<3> = CATG.VOH
         MT.COG.LEVEL<3> = COA.LEVEL
      CASE GLTB.MT.VOH.COG = MT.COG.ACCT<3>
      CASE 1
         MATREAD COA.REC FROM COA, CONO : GLTB.MT.VOH.COG ELSE COA.LEVEL = 0
         MT.COG.ACCT<3> = GLTB.MT.VOH.COG
         MT.COG.LEVEL<3> = COA.LEVEL
      END CASE
      FOR S = 1 TO JOB.MT.SEQ<1,M>
         MATREADU JMT.REC FROM JOB.MATL, MT.ID:S ELSE
            MAT PRR.REC = ''
            PRR.JOB = JOBNO
            PRR.FILE = 'JOB.MATL'
            PRR.ID = MT.ID[4,999]:S
            PRR.ERR = 'CANNOT LOCATE'
            PRR.SEQ = PRR.SEQ + 1
            MATWRITE PRR.REC ON POST.REJECTS, PRR.SEQ
            GOTO 800
         END
         WIP.COST = JMT.WIP
         BEGIN CASE
         CASE JMT.MON = ""
            GOTO 800
         CASE JMT.MON<1,1> # PERIOD OR (JMT.GLA.DATE<1,1> # "" AND JMT.GLA.DATE<1,1> # "P")
            WCNT = COUNT(WIP.COST,VM) + 1
            WPTR = COUNT(WIP.COST<1,1>,SVM) + 1
            FOR W = 1 TO WPTR
               CHECK.WIP = CHECK.WIP + WIP.COST<1,1,W>
            NEXT W
            GOTO 600
         CASE WIP.COST = ""
            WCNT = 0; WPTR = 0
            WIP.CNTR = 0
         CASE JMT.COST < 0
            WCNT = COUNT(WIP.COST,VM) + 1
            WPTR = COUNT(WIP.COST<1,1>,SVM) + 1
            TOT.WIP = 0
            FOR W = 1 TO WPTR
               TOT.WIP = TOT.WIP + WIP.COST<1,1,W>
            NEXT W
            CHECK.WIP = CHECK.WIP + TOT.WIP
            FOR I = 2 TO WCNT
               IF JMT.MON<1,I> = PERIOD AND (JMT.GLA.DATE<1,I> = "" OR JMT.GLA.DATE<1,I> = "P") THEN
                  FOR W = 1 TO WPTR
                     TOT.WIP = TOT.WIP + WIP.COST<1,I,W>
                  NEXT W
                  JMT.GLA.DATE<1,I> = "P"
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
         COG.COST = JMT.DCOST<1,1>; COG.COST<2> = JMT.DCOST<1,3>
         COG.COST<3> = JMT.DCOST<1,2>
         FOR W = 1 TO 3
            BEGIN CASE
            CASE COG.COST<W> + 0 = 0 AND WIP.COST<1,1,W> + 0 = 0
               GOTO 500
            CASE WIP.COST<1,1,W> + 0 = 0
               BEGIN CASE
               CASE MT.COG.LEVEL<W> < 1
                  DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MT.COG.ACCT<W>
               CASE MT.COG.LEVEL<W> < 2
                  DB.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : MT.COG.ACCT<W>
               CASE MT.COG.LEVEL<W> < 3
                  DB.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : MT.COG.ACCT<W>
               CASE 1
                  DB.ID = JMT.DIV : GLS.DEPT : CCNO : MT.COG.ACCT<W>
               END CASE
               WIP.AMT = COG.COST<W>
            CASE 1
               BEGIN CASE
               CASE MT.WIP.LEVEL<W> < 1
                  DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE MT.WIP.LEVEL<W> < 2
                  DB.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE MT.WIP.LEVEL<W> < 3
                  DB.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE 1
                  DB.ID = JMT.DIV : GLS.DEPT : CCNO : MT.WIP.ACCT<W>
               END CASE
               WIP.AMT = WIP.COST<1,1,W>
            END CASE
            BEGIN CASE
            CASE MT.INV.LEVEL<W> < 1
               CR.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MT.INV.ACCT<W>
            CASE MT.INV.LEVEL<W> < 2
               CR.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : MT.INV.ACCT<W>
            CASE MT.INV.LEVEL<W> < 3
               CR.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : MT.INV.ACCT<W>
            CASE 1
               CR.ID = JMT.DIV : GLS.DEPT : CCNO : MT.INV.ACCT<W>
            END CASE
            GOSUB 1000
            JMT.GLA.DATE<1,1> = "P"
500      NEXT W
         GOSUB 2000     ;* COLLECT WIP SALES STATS
600      FOR I = 2 TO WCNT
            IF JMT.MON<1,I> # PERIOD OR (JMT.GLA.DATE<1,I> # "" AND JMT.GLA.DATE<1,I> # "P") THEN
               GOTO 700
            END
            WIP.CNTR = 1
            FOR W = 1 TO WPTR
* CSF 21858
*                 IF JMT.FNGD<1,I> # "" THEN
               IF JMT.FNGD<1,I> = "*" THEN
                  BEGIN CASE
                  CASE WIP.CLR.LEVEL < 1
                     DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE WIP.CLR.LEVEL < 2
                     DB.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE WIP.CLR.LEVEL < 3
                     DB.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : WIP.CLR.ACCT
                  CASE 1
                     DB.ID = JMT.DIV : GLS.DEPT : CCNO : WIP.CLR.ACCT
                  END CASE
               END ELSE
                  BEGIN CASE
                  CASE MT.COG.LEVEL<W> < 1
                     DB.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MT.COG.ACCT<W>
                  CASE MT.COG.LEVEL<W> < 2
                     DB.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : MT.COG.ACCT<W>
                  CASE MT.COG.LEVEL<W> < 3
                     DB.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : MT.COG.ACCT<W>
                  CASE 1
                     DB.ID = JMT.DIV : GLS.DEPT : CCNO : MT.COG.ACCT<W>
                  END CASE
               END
               BEGIN CASE
               CASE MT.WIP.LEVEL<W> < 1
                  CR.ID = GEN.DIV : GEN.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE MT.WIP.LEVEL<W> < 2
                  CR.ID = JMT.DIV : GEN.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE MT.WIP.LEVEL<W> < 3
                  CR.ID = JMT.DIV : GLS.DEPT : GEN.CCTR : MT.WIP.ACCT<W>
               CASE 1
                  CR.ID = JMT.DIV : GLS.DEPT : CCNO : MT.WIP.ACCT<W>
               END CASE
               WIP.AMT = 0 - WIP.COST<1,I,W>
               GOSUB 1000
               JMT.GLA.DATE<1,I> = "P"
            NEXT W
700      NEXT I
         MATWRITE JMT.REC ON JOB.MATL, MT.ID:S
800      RELEASE JOB.MATL, MT.ID:S
      NEXT S
   NEXT M
   CHK.WIP = 0
   FOR W = 1 TO 3
      CHK.WIP = CHK.WIP + JOB.MT.WIP<1,2,W> + JOB.MT.WIP<1,3,W>
   NEXT W
   IF CHECK.WIP <> CHK.WIP THEN
      MAT PRR.REC = ''
      PRR.JOB = JOBNO
      PRR.ERR = 'TOTAL MT = ':CHK.WIP:' AND SEQ SUM = ':CHECK.WIP
      PRR.SEQ = PRR.SEQ + 1
      MATWRITE PRR.REC ON POST.REJECTS, PRR.SEQ
   END
   GOTO 9999
1000 BEGIN CASE
   CASE WIP.AMT > 0
      ETR.DB = CONO : DB.ID : JOBNO : "!MT-D*" : NEW.REC
      ETR.CR = CONO : CR.ID : JOBNO : "!MT-C*" : NEW.REC
   CASE WIP.AMT + 0 < 0
      ETR.DB = CONO : DB.ID : JOBNO : "!MT-C*" : NEW.REC
      ETR.CR = CONO : CR.ID : JOBNO : "!MT-D*" : NEW.REC
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
   LOCATE JMT.SEQ IN ETR.TRAN<1>,1 BY "AR" SETTING PTR ELSE NULL
   INS JMT.SEQ BEFORE ETR.TRAN<1,PTR>
   INS JMT.DATE BEFORE ETR.DATE<1,PTR>
   INS WIP.AMT BEFORE ETR.TAMT<1,PTR>
   MATWRITE ETR.REC ON EOM.TRANS, ETR.DB
   MATREAD ETR.REC FROM EOM.TRANS, ETR.CR ELSE
      MAT ETR.REC = ''
      ETR.AMT = 0
      ETR.REF = JOB.CUST
   END
   ETR.AMT = ETR.AMT - WIP.AMT
   LOCATE JMT.SEQ IN ETR.TRAN<1>,1 BY "AR" SETTING PTR ELSE NULL
   INS JMT.SEQ BEFORE ETR.TRAN<1,PTR>
   INS JMT.DATE BEFORE ETR.DATE<1,PTR>
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
   IF JMT.DCOST<1,1> > JMT.WIP<1,1,1> THEN
      WSS.MATL.DCOST = WSS.MATL.DCOST + (JMT.DCOST<1,1> - JMT.WIP<1,1,1>)
   END
   IF JMT.DCOST<1,2> > JMT.WIP<1,1,3> THEN
      WSS.MATL.FFOH = WSS.MATL.FFOH + (JMT.DCOST<1,2> - JMT.WIP<1,1,3>)
   END
   IF JMT.DCOST<1,3> > JMT.WIP<1,1,2> THEN
      WSS.MATL.VFOH = WSS.MATL.VFOH + (JMT.DCOST<1,3> - JMT.WIP<1,1,2>)
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
******************************************************************************
******************************************************************************
***************************** EOP ********************************************
******************************************************************************
******************************************************************************
