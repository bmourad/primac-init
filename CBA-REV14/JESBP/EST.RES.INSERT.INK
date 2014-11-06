      SUBROUTINE EST.RES.INSERT.INK (CONO, EQTY, DEPT, COMP, REF.NO)
***************************************************************************
*
* REVISION - [08.0]
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
*
* PROGRAM  - EST.RES.INSERT.INK
*
* AUTHOR   - NICK AMENDOLA, COMPUTER BUSINESS ASSOCIATES
*
* DATE     - 02/04/86
*
* DESCRIPTION
*
* This subroutine updates the ESTIMATE.DEPT data with data from the
* ink material file.
*
***************************************************************************
*
*---- COPY STATEMENTS
*
*COPY>JES.CPYLIB>SCOMMON.ESTIMATE
*COPY>JES.CPYLIB>ESTIMATE
*COPY>JES.CPYLIB>ESTIMATE.MATL
*COPY>JES.CPYLIB>ESTIMATE.DEPT
*COPY>JES.CPYLIB>ESTIMATE.TEMP
*COPY>JES.CPYLIB>JES.FILE.VARS
*COPY>CPYLIB>CHAR
*
*---- INITIALIZATION
*
      LOCATE EQTY IN EST.QTY<1>,1 SETTING QP ELSE RETURN
      MATREAD ESTM.REC FROM ESTIMATE.MATL, CONO:"I" ELSE RETURN
*
*---- MAIN PROCESSING
*
      GRP.ID = TEMP.GRP.ID
      GRP.QTY = TEMP.GRP.QTY
      GRP.FCTR = TEMP.GRP.FCTR
      GRP.TYPE = TEMP.TYPE
      GRP.CCTR = TEMP.CCTR
      GRP.QCALC = TEMP.GRP.QCALC
      GRP.QPARAM = TEMP.GRP.QPARAM
      GRP.QOR = TEMP.GRP.QOR
      GRP.FCALC = TEMP.GRP.FCALC
      GRP.FPARAM = TEMP.GRP.FPARAM
      GRP.FOR = TEMP.GRP.FOR
      WCNT = EST.PROD.WEB.CNT<1,COMP>+1
      IF WCNT = 0 THEN WCNT = 1
      FOR MPTR = 1 TO WCNT
         ICNT = COUNT(EST.PROD.INK.ID<1,COMP,MPTR>,"!") + (EST.PROD.INK.ID<1,COMP,MPTR> # "")
         FOR IPTR = 1 TO ICNT
            MAT ESTD.TEMP = ""
            TEMP.GRP.ID = GRP.ID
            TEMP.GRP.QTY = GRP.QTY
            TEMP.GRP.FCTR = GRP.FCTR
            TEMP.TYPE = GRP.TYPE
            TEMP.CCTR = GRP.CCTR
            TEMP.GRP.QCALC = GRP.QCALC
            TEMP.GRP.QPARAM = GRP.QPARAM
            TEMP.GRP.QOR = GRP.QOR
            TEMP.GRP.FCALC = GRP.FCALC
            TEMP.GRP.FPARAM = GRP.FPARAM
            TEMP.GRP.FOR = GRP.FOR
            TEMP.OPV = FIELD(EST.PROD.INK.ID<1,COMP,MPTR>,"!",IPTR)
            LOCATE TEMP.OPV IN ESTM.PROD<1>,1 SETTING MT.PTR ELSE GOTO 390
            IF TEMP.GRP.QCALC = "" THEN
               TEMP.GRP.QCALC = ESTM.QCALC<1,MT.PTR>
               TEMP.GRP.QPARAM = ESTM.QPARAM<1,MT.PTR>
               TEMP.GRP.QOR = ESTM.QOR<1,MT.PTR>
            END
            IF TEMP.GRP.FCALC = "" THEN
               TEMP.GRP.FCALC = ESTM.FCALC<1,MT.PTR>
               TEMP.GRP.FPARAM = ESTM.FPARAM<1,MT.PTR>
               TEMP.GRP.FOR = ESTM.FOR<1,MT.PTR>
            END
            TEMP.MPTR = MPTR
            TEMP.FVAR1 = IPTR
            TEMP.FVAR2 = MT.PTR
            BEGIN CASE
            CASE TEMP.GRP.QCALC = ""
               TEMP.QTY = ""
            CASE TEMP.GRP.QCALC = "C"
               TEMP.QTY = TEMP.GRP.QPARAM
            CASE TEMP.GRP.QCALC = "F"
               TEMP.FPTR = REF.NO + 1
               SUB.NAME = "EST.FORMULA.Q":TEMP.GRP.QPARAM
               CALL @SUB.NAME (CONO, "", EQTY, DEPT, COMP)
*              IF TEMP.QTY = 0 THEN GOTO 390
               IF TEMP.QTY = "" THEN
                  TEMP.GRP.QOR = ""
               END
            END CASE
            BEGIN CASE
            CASE TEMP.GRP.FCALC = ""
               TEMP.FCTR = 1000
            CASE TEMP.GRP.FCALC = "C"
               TEMP.FCTR = INT(TEMP.GRP.FPARAM*1000+0.5)
            CASE TEMP.GRP.FCALC = "M"
               TEMP.FCTR = INT(GRP.FCTR*TEMP.GRP.FPARAM+0.5)
            CASE TEMP.GRP.FCALC = "F"
               TEMP.FPTR = REF.NO + 1
               SUB.NAME = "EST.FORMULA.F":TEMP.GRP.FPARAM
               CALL @SUB.NAME (CONO, "", EQTY, DEPT, COMP)
*              IF TEMP.FCTR = 0 THEN GOTO 390
               IF TEMP.FCTR = "" THEN TEMP.GRP.FOR = ""
            CASE TEMP.GRP.FCALC = "T"
               TBL.ID = TEMP.GRP.FPARAM
               CALL EST.TABLE.FCTR (CONO, TBL.ID, TEMP.GRP.FCTR)
               IF TEMP.FCTR+0 = 0 THEN TEMP.FCTR = 1000
            END CASE
            TEMP.STD = ESTM.COST<1,MT.PTR> / 100
            TEMP.STD.TYPE = "$/":ESTM.UM<1,MT.PTR>
            TEMP.UM = ESTM.COST.UNIT<1,MT.PTR>
            TEMP.IND.1 = ESTM.FOH.PCT<1,MT.PTR>
            TEMP.MARKUP = ESTM.MARKUP<1,MT.PTR>
            TEMP.DESC = ESTM.FULL.DESC<1,MT.PTR>
            CALL EST.RES.CALC.COST (CONO,EQTY,DEPT)
            REF.NO = REF.NO + 1
            FOR A = ESTD.FIRST.MV TO ESTD.LAST.MV
               ESTD.REC(A) = INSERT(ESTD.REC(A),1,REF.NO,0,ESTD.TEMP(A))
            NEXT A
            ESTD.QTY<1,REF.NO> = TEMP.QTY * 100
            ESTD.STD<1,REF.NO> = TEMP.STD * 100
390      NEXT IPTR
      NEXT MPTR
      RETURN
   END
