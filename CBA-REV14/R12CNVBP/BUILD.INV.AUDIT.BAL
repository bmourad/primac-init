**********************************************
* REVISION    - [XX.X]
* Copyright 1995 by Computer Business Associates (Vercom Software, Inc.)
* SYSTEM      - PRIMAC
* SOURCE      - R12BP
* PROGRAM     - BUILD.INV.AUDIT.BAL
* BY          - EDVARD PITKA
* DATE        - 06/13/2001
* DESCRIPTION -
*
*ENDDOC
**********************************************
*   CPYLIBS
*COPY>CPYLIB>SYSCOM
*COPY>ICS.CPYLIB>INV_AUDIT_BAL
*COPY>ICS.CPYLIB>INV.WHSE
*COPY>ICS.CPYLIB>INV.WHSE.REV11
*COPY>ICS.CPYLIB>WAREHOUSE
*COPY>PMC.CPYLIB>COMPANY
*COPY>PMC.CPYLIB>FISCAL
*COPY>CPYLIB>CHAR
*
DEFFUN DIVISION.POSITION(CONO,CONTROL.FILE,DIV.CODE)
DEFFUN CURRENT.PERIOD(CONO,CONTROL.FILE,DIV.POS,FISCAL.FLAG)
GEN.DIV='00'
*
*** SETUP FOR SYSTEM ERRMSGS
*
SYS.TYPE = 1
CALL SYSCOM(MAT SYSCOM.REC)
*
*   FILE OPENS
*
OPEN "", "INV_AUDIT_BAL" TO INV_AUDIT_BAL ELSE ERRMSG = "CANNOT OPEN INV_AUDIT_BAL FILE"; GOTO 93000
OPEN "", "CONTROL" TO CONTROL ELSE ERRMSG = "CANNOT OPEN CONTROL FILE"; GOTO 93000
OPEN "", "WAREHOUSE" TO WAREHOUSE ELSE ERRMSG = "CANNOT OPEN WAREHOUSE FILE"; GOTO 93000
OPEN "", "COMPANY" TO COMPANY ELSE ERRMSG = "CANNOT OPEN COMPANY FILE"; GOTO 93000
OPEN '','CONVERSION.ERRORS' TO CONVERSION.ERRORS ELSE ERRMSG='CANNOT OPEN CONVERSION.ERRORS FILE'; GOTO 93000
OPEN '','INV.WHSE' TO INV.WHSE ELSE ERRMSG='CANNOT OPEN INV.WHSE FILE';GOTO 93000
OPEN '','INV.WHSE.OLD' TO INV.WHSE.OLD ELSE ERRMSG='CANNOT OPEN INV.WHSE.OLD FILE';GOTO 93000
*
*   INITIALIZATION SECTION
OLD.IWH.ID = "$%&*&&&"
ERR="" ; CNT=0
PRINT @(-1)
PRINT @(1,5):'SELECTING INV_AUDIT_BAL':@(-4)
CMD = "SSELECT INV_AUDIT_BAL BY CONO BY PROD BY WHSE BY PERIOD"
UDTEXECUTE CMD CAPTURING MSG
PRINT @(1,5):'BUILDING INV_AUDIT_BAL RECORD':@(-4)
EOF=0
FIRST.TIME = 1
CNT=0
LOOP
  READNEXT ITB.ID ELSE EOF=1
  IWH.ID = OCONV(ITB.ID,"G!2")
  WHSE=OCONV(ITB.ID,"G1!1")
  PREV.END.BAL.AMT=0
  PREV.END.BAL=0
UNTIL EOF DO
  CNT+=1                   
  IF REM(CNT,100)=0 THEN   
    PRINT @(45,5):CNT:@(-4)
  END                      
  IF OLD.IWH.ID # IWH.ID THEN
    FIRST.TIME=1
    OLD.IWH.ID = IWH.ID
    MATREAD ITB.REC FROM INV_AUDIT_BAL,ITB.ID THEN
      CONO = ITB.ID[1,3]
      MATREAD COMP.REC FROM COMPANY, CONO THEN
        MATREAD WHSE.REC FROM WAREHOUSE,CONO:WHSE THEN
          IF WHS.DIV='' THEN WHS.DIV=GEN.DIV
          DIV.POS=DIVISION.POSITION(CONO,CONTROL,WHS.DIV)
          IF DIV.POS<1,1>='' THEN
            DIV.POS=DIV.POS<1,2>
            PERIOD=CURRENT.PERIOD(CONO,CONTROL,DIV.POS,"IC")
            IF PERIOD<1,1>='' THEN
              PERIOD=PERIOD<1,2>
              ITB.PERIOD = OCONV(ITB.ID,"G2!1")
              EOP = 0
              IWH.ID=OCONV(ITB.ID,"G!2")
              MATREAD IWH.REC FROM INV.WHSE,IWH.ID THEN
                LOOP
                  BEGIN CASE
                    CASE ITB.PERIOD < PERIOD
                      IF (FIRST.TIME) THEN
                        FIRST.TIME=0
                        IF ITB.BEG = "" THEN
                          ITB.BEG=0
                          ITB.BEG.AMT=0
                          MATWRITE ITB.REC ON INV_AUDIT_BAL,ITB.ID
                          GOSUB UPD.IWH.BAL
                        END
                        GOSUB CALC.PREV.END.BAL
                        GOSUB CALC.NEXT.PERIOD
                        ITB.PERIOD = PYR:PNO
                        ITB.ID = OCONV(ITB.ID,"G!2"):"!":ITB.PERIOD
                      END ELSE
                        MATREAD ITB.REC FROM INV_AUDIT_BAL,ITB.ID ELSE MAT ITB.REC=""
                        IF ITB.BEG="" THEN
                          ITB.BEG = PREV.END.BAL
                          ITB.BEG.AMT = PREV.END.BAL.AMT
                        END ELSE
                          IF ITB.BEG # PREV.END.BAL THEN
                            ERR<-1>="ITB.BEG=":ITB.BEG:" PREV.END.BAL=":PREV.END.BAL:" INV_AUDIT_BAL,":ITB.ID
                          END
                          IF ITB.BEG.AMT # PREV.END.BAL.AMT THEN
                            ERR<-1>="ITB.BEG.AMT=":ITB.BEG:" PREV.END.BAL.AMT=":PREV.END.BAL:" INV_AUDIT_BAL,":ITB.ID
                          END
                        END
                        MATWRITE ITB.REC ON INV_AUDIT_BAL,ITB.ID
                        GOSUB UPD.IWH.BAL
                        GOSUB CALC.PREV.END.BAL
                        GOSUB CALC.NEXT.PERIOD
                        ITB.PERIOD = PYR:PNO
                        ITB.ID = OCONV(ITB.ID,"G!2"):"!":ITB.PERIOD
                      END
                    CASE ITB.PERIOD = PERIOD
                      EOP = 1
                      MATREAD ITB.REC FROM INV_AUDIT_BAL,ITB.ID ELSE MAT ITB.REC=""
                      IF ITB.BEG="" THEN
                        ;* current period should only have begining balances
                        MAT ITB.REC=''
                        ITB.BEG = PREV.END.BAL
                        ITB.BEG.AMT = PREV.END.BAL.AMT
                      END ELSE
                        IF ITB.BEG # PREV.END.BAL THEN
                          ERR<-1>="ITB.BEG=":ITB.BEG:" PREV.END.BAL=":PREV.END.BAL:" INV_AUDIT_BAL,":ITB.ID
                        END
                        IF ITB.BEG.AMT # PREV.END.BAL.AMT THEN
                          ERR<-1>="ITB.BEG.AMT=":ITB.BEG:" PREV.END.BAL.AMT=":PREV.END.BAL:" INV_AUDIT_BAL,":ITB.ID
                        END
                      END
                      ;* current period should only have begining balances
                      MAT ITB.REC=''
                      ITB.BEG=PREV.END.BAL
                      ITB.BEG.AMT=PREV.END.BAL.AMT
                      MATWRITE ITB.REC ON INV_AUDIT_BAL,ITB.ID
                      GOSUB UPD.IWH.BAL
                    CASE 1
                      ;* future period should not have anything
*                      MAT ITB.REC=''
*                      MATWRITE ITB.REC ON INV_AUDIT_BAL,ITB.ID
                      DELETE INV_AUDIT_BAL,ITB.ID
                      EOP=1
                  END CASE
                UNTIL (EOP) DO REPEAT
              END
            END
          END
        END
      END
    END
  END
REPEAT
;* build beginging balances for the IWH records that we did not
;* have record in INV.TRAN.HIST file.
SELECT INV.WHSE.OLD
DONE=0
LOOP
  READNEXT IWH.ID ELSE DONE=1
UNTIL DONE DO
  MATREAD R11.IWH.REC FROM INV.WHSE.OLD,IWH.ID THEN
    CONO=IWH.ID[1,3]
    WHSE=OCONV(IWH.ID,"G1!1")
    WHSE.ID=CONO:WHSE
    MATREAD WHSE.REC FROM WAREHOUSE,WHSE.ID THEN
      IF WHS.DIV='' THEN WHS.DIV=GEN.DIV                
      DIV.POS=DIVISION.POSITION(CONO,CONTROL,WHS.DIV)   
      IF DIV.POS<1,1>='' THEN                           
        DIV.POS=DIV.POS<1,2>                            
        PERIOD=CURRENT.PERIOD(CONO,CONTROL,DIV.POS,"IC")
        IF PERIOD<1,1>='' THEN                          
          PERIOD=PERIOD<1,2>                            
          ITB.PERIOD=PERIOD
          ITB.ID=IWH.ID:"!":ITB.PERIOD
          MATREAD ITB.REC FROM INV_AUDIT_BAL,ITB.ID THEN
            TMP1=ITB.BEG
            TMP2=ITB.BEG.AMT
            MAT ITB.REC=''
            ITB.BEG=TMP1
            ITB.BEG.AMT=TMP2
          END ELSE
            MAT ITB.REC=''
            ITB.BEG=R11.IWH.BEG.INV
            ITB.BEG.AMT=R11.IWH.BEG.VALUE
            MATWRITE ITB.REC ON INV_AUDIT_BAL,ITB.ID
          END
        END
      END
    END
  END
REPEAT
WRITE ERR ON CONVERSION.ERRORS,"BUILD.INV.AUDIT.BAL"
CALL CONV.RPT("BUILD.INV.AUDIT.BAL")
*
GOTO 99999
*
**************************************************************************
********* S U B R O U T I N E S ******************************************
**************************************************************************
*
*
*
*****************
CALC.NEXT.PERIOD: 
*****************
*
*   Calculate the next Period Year and Period Number
*
PYR = ITB.PERIOD[1,4]
PNO = ITB.PERIOD[5,2] + 1
IF PNO > CO.LAST.FISCAL THEN
  PYR = PYR + 1
  PNO = "01"
END
PNO = PNO "R%2"
RETURN
*
********************
CALC.PREV.END.BAL: 
********************
*
PREV.END.BAL=ITB.BEG+ITB.RECV+ITB.SALE
PREV.END.BAL+=ITB.SHRNK+ITB.TRAN.IN+ITB.TRAN.OUT
PREV.END.BAL.AMT=ITB.BEG.AMT+ITB.RECV.AMT
PREV.END.BAL.AMT+=ITB.SHRNK.AMT+ITB.SALE.AMT
PREV.END.BAL.AMT+=ITB.TRAN.IN.AMT+ITB.TRAN.OUT.AMT
RETURN
*
*******************
UPD.IWH.BAL: 
********************
*
IWH.BAL.PERIOD<1,-1>=ITB.PERIOD
IWH.BEG.QTY<1,-1>=ITB.BEG
IWH.BEG.AMT<1,-1>=ITB.BEG.AMT
MATWRITE IWH.REC ON INV.WHSE,IWH.ID
RETURN
*
*   STANDARD SUBROUTINES
*
91000 ERR.TYPE=1; CALL SYSCOM(MAT SYSCOM.REC)
RETURN
93000 ERR.TYPE=3; CALL SYSCOM(MAT SYSCOM.REC)
99999 
END
