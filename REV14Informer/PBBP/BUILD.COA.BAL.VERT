*COPY>GLS.CPYLIB>COA.BAL
*COPY>PB.CPYLIB>COA_BAL_VT
*COPY>CPYLIB>CHAR
*
   OPEN '','CONTROL' TO CONTROL ELSE
      ERRMSG = "CANNOT OPEN CONTROL FILE"
      GOSUB 91000
      STOP
   END
   OPEN '','CO.COA.BAL' TO CO.COA.BAL ELSE
      ERRMSG = "CANNOT OPEN CO.COA.BAL FILE"
      GOSUB 91000
      STOP
   END
   OPEN '','CO_COA_BAL_VT' TO CO_COA_BAL_VT ELSE
      ERRMSG = "CANNOT OPEN CO_COA_BAL_VT"
      GOSUB 91000
      STOP
   END
   OPEN '','COMPANY' TO COMPANY ELSE
      ERRMSG = "CANNOT OPEN COMPANY"
      GOSUB 91000
      STOP
   END
*
   CNT = 0
*
10*
   DATA = 1
   CONOS = ''
   NO.PERIODS = ''
   CUR.YR = ''
   CUR.PERIOD.NO = ''
   SELECT COMPANY
   LOOP
      READNEXT CONO ELSE DATA = 0
   WHILE DATA DO
*
      READV CURFIS FROM CONTROL, CONO:"FISCAL",1 THEN
         READV NUMPERS FROM CONTROL, CONO:"ACCT.PERIODS",1 ELSE NUMPERS = 12
         CONOS<-1> = CONO
         NO.PERIODS<-1> = NUMPERS
         CUR.YR<-1> = CURFIS[1,4]
         CUR.PERIOD.NO<-1> = CURFIS[5,2]
      END
   REPEAT
*
*
   SELECT CO.COA.BAL
*
*
100 *
   READNEXT CBID ELSE GOTO 999
   MATREAD CB.REC FROM CO.COA.BAL, CBID ELSE
      ERRMSG = "CANNOT READ CO.COA.BAL RECORD ":CBID
      GOSUB 91000
      GOTO 100
   END
   CNT = CNT + 1
   CONO = CBID[1,3]
   ACCT = CBID[4,99]
   ACCT = TRIM(ACCT)
   LOCATE CONO IN CONOS,1 SETTING PTR ELSE GO 100
   FOR XX = 1 TO CUR.PERIOD.NO<PTR>
      MAT CBVT.REC = ''
      CBVT_ID = CONO:"!":ACCT
      PRD = STR("0",2-LEN(XX)):XX
      CBVT_ID = CBVT_ID: "!" : CUR.YR<PTR> : PRD
      IF XX = NO.PERIODS<PTR> THEN
         CBVT.REC(1) = CB.REC(55)
         CBVT.REC(2) = CB.REC(108 + XX)
      END ELSE
         CBVT.REC(1) = CB.REC(3 + XX)
         CBVT.REC(2) = CB.REC(108 + XX)
      END
*  IF XX = CUR.PERIOD.NO<PTR> THEN
*    CBVT.REC(1) = CBVT.REC(1) + CB.DEBITS
*    CBVT.REC(1) = CBVT.REC(1) + CB.CREDITS
*  END
      MATWRITE CBVT.REC ON CO_COA_BAL_VT, CBVT_ID
   NEXT XX
   FOR ZZ = 1 TO NO.PERIODS<PTR>
      MAT CBVT.REC = ''
      IF ZZ = NO.PERIODS<PTR> THEN
         CBVT.REC(1) = CB.REC(108)
      END ELSE
         CBVT.REC(1) = CB.REC(56 + ZZ)
      END
      CBVT_ID = CONO:"!":ACCT
      PRD = STR("0",2-LEN(ZZ)):ZZ
      CBVT_ID = CBVT_ID: "!" : CUR.YR<PTR> - 1 : PRD
      MATWRITE CBVT.REC ON CO_COA_BAL_VT, CBVT_ID
   NEXT ZZ
   ERRMSG = CNT:" RECORDS PROCESSED"
   GOSUB 92000
   GOTO 100
91000 *
   PRINT ERRMSG
   STOP
92000 *
   PRINT ERRMSG
   RETURN
999 *
   PRINT "PROCESSING COMPLETE"
END
