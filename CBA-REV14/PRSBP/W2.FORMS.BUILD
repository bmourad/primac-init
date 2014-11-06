*********************************************************************
* REVISION    - [08.1]
* Copyright 1982 Computer Business Associates (Vercom Software, Inc.)
* SYSTEM      - PRIMAC
* SOURCE      - PRSBP
* PROGRAM     - W2.FORMS.BUILD
* BY          - ZIAD YAMOUT, CBA
* DATE        - 09/25/88
* DESCRIPTION - Build W2 forms with preset variables.
* MOD         - Task14682 csf10946 RWW 11.22.89
*             - 401k amt to be employer's Matching amt (to W2 form)
*T22304 lanny 10/21/1997 * Include EIC on W2 and get State Tax ID from
*                          TAX TABLE.
*T26128 cm 09/12/2001 * Changes for 2001 W-2 forms.
*********************************************************************
*ENDDOC
*COPY>PMC.CPYLIB>EMPLOYEE
*COPY>PRS.CPYLIB>MISCDED
*COPY>PRS.CPYLIB>QTD-YTD
*COPY>PMC.CPYLIB>COMPANY
*COPY>PRS.CPYLIB>AUTO.FORMS
*COPY>PRS.CPYLIB>AUTO.FORMS.DATA
*COPY>PRS.CPYLIB>401K.MATCH 
*COPY>PMC.CPYLIB>FICA.TABLE
*COPY>PRS.CPYLIB>TAX2
*COPY>CPYLIB>CHAR
*
*--- Dimension arrays
*
   PROMPT ""
   DIM I.REC(100)
   FORM.NAME = "W2.FORMS"
   ERRORS = ""
   LABEL.SWITCH = "N"
*
*--- Get CONO from proc
*
   PROCREAD BUFFER ELSE
      ERRMSG = "Must be executed from a PROC"
      GOSUB 91000; GOTO 99999
   END
   CONO = BUFFER<1>
*
*--- OPEN FILES
*
   OPEN "S.COMPANY" TO COMPANY ELSE
      ERRMSG = "Cannot open S.COMPANY file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "S.EMPLOYEE" TO EMPLOYEE ELSE
      ERRMSG = "Cannot open S.EMPLOYEE file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "S.QTD-YTD" TO QTD.YTD ELSE
      ERRMSG = "Cannot open S.QTD-YTD file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "MISCDED" TO MISCDED ELSE
      ERRMSG = "Cannot open MISCDED file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "AUTO.FORMS" TO AUTO.FORMS ELSE
      ERRMSG = "Cannot open AUTO.FORMS file"
      GOSUB 91000; GOTO 99999
   END
   OPEN FORM.NAME TO FORM.FILE ELSE
      ERRMSG = "Cannot open ":FORM.NAME:" file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "CONTROL" TO CONTROL ELSE
      ERRMSG = "Cannot open CONTROL file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "S.FICA.TABLE" TO FICA.TABLE ELSE
      ERRMSG = "Cannot open FICA.TABLE file"
      GOSUB 91000; GOTO 99999
   END
   OPEN "TAX2" TO TAX2 ELSE
      ERRMSG = "Cannot open TAX2 file"
      GOSUB 91000; GOTO 99999
   END
*
*--- Get company info
*
   MATREAD COMP.REC FROM COMPANY,CONO ELSE
      ERRMSG = "CANNOT LOCATE COMPANY, ":CONO
      GOSUB 91000; GOTO 99999
   END
   READ DC.REC FROM CONTROL, CONO:"401K" ELSE DC.REC = ""
*CSF10946
   MATCH.CODES = ''
   MATCH.CODES<1>=DC.REC<1>
   MATCH.CODES<2>=DC.REC<3>
   MATREAD MTCH.REC FROM CONTROL, CONO:"401K.MATCH" ELSE MAT MTCH.REC =""
   MATREAD FIC.REC FROM FICA.TABLE, CONO ELSE
      ERRMSG = "CANNOT LOCATE FICA TABLE FOR CONO ":CONO
      GOSUB 91000; GOTO 99999
   END
*
   READNEXT EMP.ID ELSE
      ERRMSG = "No employees selected"
      GOSUB 91000; GOTO 99999
   END
*
*--- Get forms information
*
   MATREAD AFH.REC FROM AUTO.FORMS, FORM.NAME ELSE
      ERRMSG = "Cannot locate AUTO.FORMS, ":FORM.NAME
      GOSUB 91000; GOTO 99999
   END
   LOCATE 0 IN AFH.D.SEQ<1> SETTING LNO ELSE
      ERRMSG = "Cannot locate sequence 0 for form ":FORM.NAME
      GOSUB 91000; GOTO 99999
   END
   F.ID = FORM.NAME:"!D!":AFH.D.REF<1,LNO>
   MATREAD AFD.REC FROM AUTO.FORMS, F.ID ELSE
      ERRMSG = "Cannot locate reference # " : AFH.D.REF<1,LNO>
      GOSUB 91000; GOTO 99999
   END
   IF AFD.TYP # 3 THEN
      ERRMSG = "Detail sequence 0 should be numeric"
      GOSUB 91000; GOTO 99999
   END
   ATT.CHK = 0
   FOR I = 1 TO 7 WHILE ATT.CHK = 0
      LOCATE I IN AFH.H.ATT<1> SETTING FND ELSE ATT.CHK = I
      IF I = 6 THEN ATT.CHK = 0   ;*T22304
   NEXT I
   IF ATT.CHK THEN
      ERRMSG = "Header attribute (":ATT.CHK:") is not setup"
      GOSUB 91000; GOTO 99999
   END
   FOR I = 1 TO 20 WHILE ATT.CHK = 0
      IF I # 13 AND I # 14 THEN
         LOCATE I IN AFH.D.ATT<1> SETTING FND ELSE ATT.CHK = I
      END
   NEXT I
   IF ATT.CHK THEN
      ERRMSG = "Detail attribute (":ATT.CHK:") is not setup"
      GOSUB 91000; GOTO 99999
   END
   AFD.FILL.CHR = TRIM(AFD.FILL.CHR)
   SEQ.NO = 1
*
*--- Load detail records
*
   DATA = 1
   LOOP
      MATREAD EMP.REC FROM EMPLOYEE, EMP.ID ELSE
         ERRORS<-1> = "EMPLOYEE, " : EMP.ID
         GOTO 999
      END
*csf10946
      MAT.PCT = 0
      MFLAG=0
      MPCT = 0
      FOR CD.VAL = 1 TO DCOUNT(MATCH.CODES,AM)
         LOCATE MATCH.CODES<CD.VAL> IN EMP.MSC.CODE<1>,1 SETTING MLOC ELSE MLOC=0
         IF MLOC NE 0 THEN MPCT = MPCT + EMP.MSC.AMT<1,MLOC>
         MLOC = 0
         MFLAG=1
      NEXT CD.VAL
      IF MPCT GT 0 OR MFLAG THEN
         MTCH.CNT = DCOUNT(MTCH.EMP.PCT<1>,VM)
         FOUND = 0
         FOR MVAL = 1 TO MTCH.CNT UNTIL FOUND
            IF MPCT LE MTCH.EMP.PCT<1,MVAL> THEN
               MAT.PCT = MTCH.CO.PCT<1,MVAL>
               FOUND = 1
            END
         NEXT MVAL
         IF NOT(FOUND) THEN
            MAT.PCT = MTCH.CO.PCT<1,MTCH.CNT>
         END
      END
*
      ALT.GROSS=0; ALT.NTSICK=0
      ALT.FICA.GROSS=0
      ALT.STATE.GROSS=0; ALT.CITY.GROSS=0
      IF EMP.ALT.NO # "" THEN
         MATREAD QTD.REC FROM QTD.YTD, CONO : EMP.ALT.NO THEN
            ALT.GROSS = SUM(QTD.GROSS)
            ALT.NTSICK = SUM(QTD.SK.NO.FI)
            GOSUB 5000
            ADJ.ALT.GROSS = ALT.GROSS - NON.TAX.DED
            ALT.FICA.GROSS = ALT.GROSS - NON.TAX.FICA
            ALT.STATE.GROSS = ALT.GROSS - NON.TAX.STATE
            ALT.CITY.GROSS = ALT.GROSS - NON.TAX.CITY
         END
      END
      MATREAD QTD.REC FROM QTD.YTD, EMP.ID ELSE
         ERRORS<-1> = "QTD-YTD, " : EMP.ID
         GOTO 999
      END
      IF EMP.PENSION = "Y" THEN PEN = "X" ELSE PEN = ""
      IF EMP.H.R.STATUS ="D" THEN DECEASED = "X" ELSE DECEASED = ""
*
*---- SUM QUARTERLY AMTS FOR GROSS, FWHT, FICA, STATE, CITY
*
      FWHT = SUM(QTD.FWHT)
      GROSS = SUM(QTD.GROSS)
      FICA = SUM(QTD.FICA)
      STATE = SUM(QTD.SWHT)
      CITY = SUM(QTD.CWHT)
      SICK = SUM(QTD.SCK.PAY)
      NTSICK = SUM(QTD.SK.NO.FI)
      FICA.L1 = SUM(QTD.LVL1.FICA)
      FICA.L2 = SUM(QTD.LVL2.FICA)
      EIC = SUM(QTD.EIC)  ;* T22304
*
      GOSUB 5000
      ADJ.GROSS = GROSS - NON.TAX.DED
      FICA.GROSS = GROSS - NON.TAX.FICA
      STATE.GROSS = GROSS - NON.TAX.STATE
      CITY.GROSS = GROSS - NON.TAX.CITY
      THIS.FICA = FICA.GROSS - NTSICK
      THAT.FICA = ALT.FICA.GROSS - ALT.NTSICK
      BEGIN CASE
         CASE THIS.FICA > FIC.LVL1.MAX.FICA.TAX
            THIS.FICA = FIC.LVL1.MAX.FICA.TAX
         CASE THAT.FICA > FIC.LVL1.MAX.FICA.TAX
            THIS.FICA = 0
         CASE (THIS.FICA+THAT.FICA) > FIC.LVL1.MAX.FICA.TAX
            IF EMP.TERM.DATE = "" THEN
               THIS.FICA = FIC.LVL1.MAX.FICA.TAX - THAT.FICA
            END
            IF THIS.FICA < 0 THEN THIS.FICA = 0
      END CASE
      TOTFICA.L1 = THIS.FICA
      THIS.FICA = FICA.GROSS - NTSICK
      THAT.FICA = ALT.FICA.GROSS - ALT.NTSICK
      BEGIN CASE
         CASE THIS.FICA > FIC.LVL2.MAX.FICA.TAX
            THIS.FICA = FIC.LVL2.MAX.FICA.TAX
         CASE THAT.FICA > FIC.LVL2.MAX.FICA.TAX
            THIS.FICA = 0
         CASE (THIS.FICA+THAT.FICA) > FIC.LVL2.MAX.FICA.TAX
            IF EMP.TERM.DATE = "" THEN
               THIS.FICA = FIC.LVL2.MAX.FICA.TAX - THAT.FICA
            END
            IF THIS.FICA < 0 THEN THIS.FICA = 0
      END CASE
      TOTFICA.L2 = THIS.FICA
*
      IF K401.DED > 0 THEN
         DEF.COMP = "X"
         K401.LABEL = "D"
         LABEL.SWITCH = "Y"
      END ELSE
         DEF.COMP = ""
         K401.LABEL = ""
      END
*
      MAT I.REC = ""
      I.REC(1) = EMP.ID[4,99]
*T26128 v
*     I.REC(2) = EMP.FRST.NAME:" ":EMP.LAST.NAME
      I.REC(2) = EMP.FRST.NAME
      I.REC(40) = EMP.LAST.NAME
*T26128 ^
      I.REC(3) = EMP.ADDR1
      I.REC(4) = EMP.ADDR2
      I.REC(5) = EMP.CITY :", ": EMP.STATE
      I.REC(6) = EMP.ZIP
      I.REC(7) = EMP.SOC.SEC
      I.REC(8) = PEN
      I.REC(9) = EMP.ST.CODE[1,2]
      I.REC(10) = EMP.CITY.CODE
      I.REC(11) = FWHT
      I.REC(12) = ADJ.GROSS
      I.REC(13) = FICA.L1 + FICA.L2
*     I.REC(14) = TOTFICA
      I.REC(15) = CITY
      I.REC(16) = STATE
      I.REC(17) = DECEASED
*     I.REC(18)  See note below!!!
*     Reserved for FRINGE.BENEFITS which do not pull from
*     PRS but is included in the reserve for W2 form and magnetic
*     tape purposes.    * RRG * 8/29/89.
      I.REC(19) = STATE.GROSS
      I.REC(20) = CITY.GROSS
      I.REC(21) = DEF.COMP
      I.REC(33) = FICA.L1
      I.REC(34) = TOTFICA.L1
      I.REC(35) = FICA.L2
      I.REC(36) = TOTFICA.L2
*T22304 v
      I.REC(37) = EIC
*C38290  IF EMP.ST.CODE[1,2] # '' THEN
      IF EMP.ST.CODE # '' THEN
         MATREAD TAX.TAB.REC FROM TAX2, CONO:EMP.ST.CODE THEN
            IF TTR.STATE.TAX.ID # '' THEN
*T26128 v
*              I.REC(38) = TTR.STATE.TAX.ID
               I.REC(42) = TTR.STATE.TAX.ID
*T26128 ^
            END
         END
      END
*T26128 v
*     IF I.REC(38) = '' THEN I.REC(38) = CO.STATE.TAX.ID
      IF I.REC(42) = '' THEN I.REC(42) = CO.STATE.TAX.ID
*T26128 ^
*T22304 ^
      IF K401.DED = 0 THEN K401.DED = ""
      I.REC(28) = K401.DED
      I.REC(27) = K401.LABEL
      BEGIN CASE
         CASE AFD.FILL.CHR = ""
         CASE AFD.JUSTIFY = "R"
            SEQ.NO = STR(AFD.FILL.CHR,AFD.MAXL-LEN(SEQ.NO)):SEQ.NO
         CASE 1
            SEQ.NO = SEQ.NO:STR(AFD.FILL.CHR,AFD.MAXL-LEN(SEQ.NO))
      END CASE
      MATWRITE I.REC ON FORM.FILE, CONO:SEQ.NO
      SEQ.NO = SEQ.NO + 1
999   READNEXT EMP.ID ELSE DATA = 0
   WHILE DATA DO REPEAT
   GOSUB 7000
   GOTO 99999
*
*--- Non taxable deductions
5000 K401.DED = 0
   NON.TAX.DED = 0
   NON.TAX.FICA = 0
   NON.TAX.STATE = 0
   NON.TAX.CITY = 0
   NTD.CNT = DCOUNT(QTD.DED,VM)
   FOR REF = 1 TO NTD.CNT
      MATREAD MDED.REC FROM MISCDED, CONO:QTD.DED<1,REF> THEN
         IF MDED.TAXABLE = "B" THEN
            FOR I = 1 TO 4
               NON.TAX.DED = NON.TAX.DED + QTD.AMNT<1,REF,I>
            NEXT I
            IF MDED.NTXFICA = "Y" THEN
               FOR J = 1 TO 4
                  NON.TAX.FICA = NON.TAX.FICA + QTD.AMNT<1,REF,J>
               NEXT J
            END
            IF MDED.NTXSTATE = "Y" THEN
               FOR J = 1 TO 4
                  NON.TAX.STATE = NON.TAX.STATE + QTD.AMNT<1,REF,J>
               NEXT J
            END
            IF MDED.NTXCITY = "Y" THEN
               FOR J = 1 TO 4
                  NON.TAX.CITY = NON.TAX.CITY + (QTD.AMNT<1,REF,J>)
               NEXT J
            END
         END
      END
   NEXT REF
*
   FOR K = 1 TO 2
      LOCATE DC.REC<K> IN QTD.DED<1> SETTING POS ELSE POS = 0
      IF POS THEN
         MATREAD MDED.REC FROM MISCDED, CONO:QTD.DED<1,POS> ELSE MAT MDED.REC = ""
         IF MDED.TAXABLE = "B" AND MDED.NTXFED = "Y" THEN
            FOR X = 1 TO 4
               K401.DED = K401.DED + QTD.AMNT<1,POS,X>
            NEXT X
         END
      END
   NEXT K
*C10946
**      K401.DED.M = 0
**      FOR CD.VAL = 1 TO DCOUNT(MATCH.CODES,AM)
**         LOCATE MATCH.CODES<CD.VAL> IN QTD.DED<1>,1 SETTING MLOC ELSE MLOC=0
**         IF MLOC NE 0 THEN
**         FOR X = 1 TO 4
**           K401.DED.M = K401.DED.M + QTD.AMNT<1,MLOC,X>
**         NEXT X
**         END
**         MLOC = 0
**      NEXT CD.VAL
**      K401.DED.EMPR = 0
**      IF K401.DED.M GT 0 AND MAT.PCT GT 0 THEN
**         K401.DED.EMPR = INT((K401.DED.M * MAT.PCT / 10000) + .5)
**      END
**      K401.DED = K401.DED.EMPR
*
   RETURN
*
7000 *
*
*--- Load header record
*
   MAT I.REC = ""
   I.REC(1) = CO.NAME
   I.REC(2) = CO.ADDRESS
   I.REC(3) = CO.CITY : ", " : CO.STATE
   I.REC(4) = CO.ZIP
   I.REC(5) = CO.FED.TAX.ID
*T22304      I.REC(6) = CO.STATE.TAX.ID
*T26128   I.REC(7) = "X"
   IF LABEL.SWITCH = "Y" THEN
      I.REC(8) = "401(k)"
   END ELSE
      I.REC(8) = "      "
   END
   MATWRITE I.REC ON FORM.FILE, CONO:FORM.NAME
   RETURN
*
*--- Error message routine
91000 PRINT @(0,23) : ERRMSG : CL :
   INPUT REPLY :
   PRINT @(0,23) : CL :
   RETURN
99999 IF ERRORS # "" THEN
      ERRMSG = "Please make a note of the following unprocessed records."
      GOSUB 91000
      CNT = DCOUNT(ERRORS,AM)
      FOR I = 1 TO CNT
         ERRMSG = I "R#4" : " - " : ERRORS<I>
         GOSUB 91000
      NEXT I
      ERRMSG = "This was the last of the unprocessed records."
      GOSUB 91000
   END
END
