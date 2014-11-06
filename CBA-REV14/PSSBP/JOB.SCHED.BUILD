OBSOLETE
*COPY>CPYLIB>SCOMMON1
*COPY>PSS.CPYLIB>COM.PSS.FILE.VARS
*COPY>PSS.CPYLIB>COM.CCTR.SCHED
*COPY>PSS.CPYLIB>COM.JOB.SCHED
*COPY>PMC.CPYLIB>COM.COMPANY
*COPY>PMC.CPYLIB>COM.DIVISION
*COPY>JES.CPYLIB>COM.ESTIMATE
*COPY>JCS.CPYLIB>COM.JOB
*********************************************************************
* REVISION - [08.0]
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
* SYSTEM   - PRIMAC
* SOURCE   - PSSBP
* PROGRAM  - JOB.SCHED.BUILD
* AUTHOR   - WALID YAMOUT, COMPUTER BUSINESS ASSOCIATES
* DATE     - 05/30/86
* DESCRIPTION
* This program is a driver to build the JOB.SCHED record. It validat
* the estimate number or the job number, which ever was selected.
* Then validat the ESTIMATE file to insure that the job selected
* for scheduling was created through booking an estimate.
*ENDDOC
*********************************************************************
*
*---- FILE COPY STATEMENTS
*
*COPY>PSS.CPYLIB>JOB.SCHED
*COPY>PSS.CPYLIB>PSS.FILE.VARS
*COPY>PSS.CPYLIB>PSS.HIERARCHY
*COPY>PMC.CPYLIB>COMPANY
*COPY>PMC.CPYLIB>CUSTOMER
*COPY>JCS.CPYLIB>JOB
*COPY>JES.CPYLIB>ESTIMATE
*COPY>CPYLIB>SCREEN.COM
*COPY>CPYLIB>FILE.VARS
*COPY>CPYLIB>GEN.XREF
*COPY>CPYLIB>CHAR
*COPY>CPYLIB>SYSCOM
MAT SYSCOM.REC =  ""; SYS.TYPE = 2
*
*--- PERFORM PROCREAD
*
      PROCREAD BUFFER ELSE BUFFER = "J"
      BEGIN CASE
      CASE BUFFER<1> = "E"
         PROG.FLG = "E"
      CASE 1
         PROG.FLG = "J"
      END CASE
      MV.JOB = 1
      MV.TYPE = 2
      MV.DATE = 3
*
*--- OPEN FILES
*
      OPEN "","CONTROL" TO CONTROL ELSE ERRMSG = "CONTROL FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","PREFIX" TO PREFIX ELSE ERRMSG = "PREFIX FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","COMPANY" TO COMPANY ELSE ERRMSG = "COMPANY FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","PSS.SCREENS" TO M.SCREENS ELSE ERRMSG = "PSS.SCREENS FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","JOB.SCHED" TO JOB.SCHED ELSE ERRMSG = "JOB.SCHED FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","PEND.JOB.SCHED" TO PEND.JOB.SCHED ELSE ERRMSG = "PEND.JOB.SCHED FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","CCTR.AVAIL" TO CCTR.AVAIL ELSE ERRMSG = "CCTR.AVAIL FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","ESTIMATE" TO ESTIMATE ELSE ERRMSG = "ESTIMATE FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","ESTIMATE.DEPT" TO ESTIMATE.DEPT ELSE ERRMSG = "ESTIMATE.DEPT FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","JOB" TO JOB ELSE ERRMSG = "JOB FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","CUSTOMER" TO CUSTOMER ELSE ERRMSG = "CUSTOMER FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","CUSTOMER.XREF" TO CUSTOMER.XREF ELSE ERRMSG = "CUSTOMER.XREF FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","CUST.EST.XREF" TO CUST.EST.XREF ELSE ERRMSG = "CUST.EST.XREF FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","PSS.CONTROL" TO PSS.CONTROL ELSE ERRMSG = "PSS.CONTROL FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","PSS.HIERARCHY" TO PSS.HIERARCHY ELSE ERRMSG = "PSS.HIERARCHY FILE IS MISSING"; GOSUB 90000; STOP
      OPEN "","COST.CNTR" TO COST.CNTR ELSE ERRMSG = "COST.CNTR FILE IS MISSING"; GOSUB 90000; STOP
*
*--- GET COMPANY
*
      CONO = ""
      CALL GET.CONO1 (CONO, MAT COMP.REC, COMPANY, CONTROL)
      IF CONO = "END" THEN GOTO 999999
      READ LOADING.FLG FROM PSS.CONTROL, CONO : "LOADING.FLG" ELSE
         READ LOADING.FLG FROM PSS.CONTROL, "LOADING.FLG" ELSE LOADING.FLG = "N"
      END
      MATREAD PSHR.REC FROM PSS.CONTROL, "HIERARCHY" ELSE
         ERRMSG = "SYSTEM HIERARCHY RECORD IS MISSING"
         GOSUB 90000
         GOTO 999999
      END
      MAT GEN.XREF.REC = ""
      GXR.CO = CONO
*
*--- INITIALIZE SCREEN
*
      SCREEN INIT;#
      S$SCR = 1
      SCREEN DEFINE;JOB.SCHED.BUILD
      SCREEN FORMAT
      SCREEN COUNT;;8
      LINE.COUNT = S$LCNT
      LINE.SPACE = S$LSPC
      GOTO 110
100*
      SCREEN CLEAR
      RELEASE
*
*--- MAIN PROCESS
*
110*
      MAT JOB.REC = ""
      MAT EST.REC = ""
      JOB.NUM = ""
      EST.KEY = ""
      CNT = 0
      CURR.REF.NO = ""
      BEGIN CASE
      CASE PROG.FLG = "J"
         S$DATA(1)<S$SCR> = ""
         SCREEN FIELD;;1
         SCREEN INPUT;;1
         IF S$VALUE = "END" THEN GOTO 999999
         BEGIN CASE
         CASE S$VALUE = ""
            GOSUB 10000
            IF S$VALUE = "END" OR GXR.ID = "END" THEN GOTO 100
            GXR.XREF = CUSTOMER
            GXR.SRCH.ID = GXR.ID
            GXR.FILE = JOB
            GXR.LOC = 34
            GXR.TOP.LINE = "JOB XREF SEARCH"
            GXR.TOP.LINE<1,2> = "FOR CUSTOMER : " : GXR.SRCH.ID
            GXR.HEADING = "JOB NUMBER"
            GXR.ATT = 0
            GXR.LEN = 10
            GXR.HEADING<1,2> = "ESTIMATE NUM"
            GXR.ATT<1,2> = 18
            GXR.LEN<1,2> = 12
            GXR.HEADING<1,3> = "DESCRIPTION"
            GXR.ATT<1,3> = 16
            GXR.LEN<1,3> = 48
            GXR.ID = ""
            GXR.OCOVN = ""
            CALL GEN.XREF1 (MAT GEN.XREF.REC,PREFIX)
            S$DATA(1)<S$SCR> = GXR.ID
            IF GXR.ACTION # "X" THEN
               GOSUB 1000
            END ELSE
               SCREEN DISPLAY;;1
            END
            IF GXR.ID = "" THEN GOTO 100
            MATREAD JOB.REC FROM JOB, CONO : GXR.ID ELSE
               ERRMSG = "Cannot locate job. Try again! "
               GOSUB 90000
               GOTO 100
            END
            JOB.NUM = GXR.ID
         CASE 1
            MATREAD JOB.REC FROM JOB, CONO:S$VALUE ELSE
               ERRMSG = "Cannot locate job. Try again! "
               GOSUB 90000; GOTO 110
            END
            JOB.NUM = S$VALUE
         END CASE
         IF JOB.EST = "" THEN
            ERRMSG = "No estimate for this job"
            GOSUB 90000; GOTO 110
         END
         BEGIN CASE
         CASE JOB.STATUS<1,1> = 9
            ERRMSG = "This job is cancelled"
            GOSUB 90000; GOTO 110
         CASE JOB.STATUS # ""
            ERRMSG = "Job status must be booked"
            GOSUB 90000; GOTO 110
         END CASE
         MATREAD CUST.REC FROM CUSTOMER, CONO:JOB.CUST ELSE
            ERRMSG = "Cannot locate customer - ":JOB.CUST
            GOSUB 90000; GOTO 110
         END
         MATREAD EST.REC FROM ESTIMATE, CONO:JOB.EST ELSE
            ERRMSG = "Cannot locate estimate - ":JOB.EST
            GOSUB 90000; GOTO 110
         END
         GOSUB 80000
         CNT = COUNT(JOB.DESC,VM) + (JOB.DESC # "")
         REF.NO = 1
         GOSUB 82000
         IF EST.STATUS<1,1> # "BOOKED" OR EST.BOOK.JOB = "" THEN
            ERRMSG = "Estimate status must be booked"
            GOSUB 90000; GOTO 100
         END
         LOCATE JOB.NUM IN EST.BOOK.JOB<1>,1 SETTING FND ELSE FND = 0
         IF FND = 0 THEN
            IF EST.JOB = JOB.NUM THEN
               ERRMSG = "This job is a master header"
            END ELSE
               ERRMSG = "This job is not for this estimate"
            END
            GOSUB 90000; GOTO 100
         END
         FND = 1
         READU REC FROM JOB.SCHED, CONO:JOB.NUM ELSE FND = 0
         REC = ""
         IF FND THEN
            RELEASE JOB.SCHED, CONO:JOB.NUM
            ERRMSG = "JOB.SCHED exists for this job"
            GOSUB 90000; GOTO 100
         END
      CASE PROG.FLG = "E"
         S$DATA(2)<S$SCR> = ""
         SCREEN FIELD;;2
         SCREEN INPUT;;2
         IF S$VALUE = "END" THEN GOTO 999999
         BEGIN CASE
         CASE S$VALUE = ""
            GOSUB 10000
            IF S$VALUE = "END" OR GXR.ID = "END" THEN GOTO 100
            GXR.OCOVN = ""
            GXR.XREF = CUST.EST.XREF
            GXR.SRCH.ID = GXR.ID
            GXR.FILE = ESTIMATE
            GXR.LOC = 1
            GXR.TOP.LINE = "ESTIMATE XREF SEARCH"
            GXR.TOP.LINE<1,2> = "FOR CUSTOMER : " : GXR.SRCH.ID
            GXR.HEADING = "ESTIMATE NUM"
            GXR.ATT = 0
            GXR.LEN = 12
            GXR.HEADING<1,2> = "DESCRIPTION"
            GXR.ATT<1,2> = 31
            GXR.LEN<1,2> = 43
            GXR.HEADING<1,3> = "ENT-DATE"
            GXR.ATT<1,3> = 18
            GXR.LEN<1,3> = 8
            GXR.OCONV<1,3> = "D2/"
            GXR.HEADING<1,4> = "STATUS"
            GXR.ATT<1,4> = 1
            GXR.LEN<1,4> = 8
            GXR.ID = ""
            CALL GEN.XREF1 (MAT GEN.XREF.REC,PREFIX)
            S$DATA(2)<S$SCR> = GXR.ID
            IF GXR.ACTION # "X" THEN
               GOSUB 1000
            END ELSE
               SCREEN DISPLAY;;2
            END
            IF GXR.ID = "" THEN GOTO 100
            MATREAD EST.REC FROM ESTIMATE, CONO : GXR.ID ELSE
               ERRMSG = "Cannot locate estimate. Try again! "
               GOSUB 90000
               GOTO 100
            END
            EST.KEY = GXR.ID
         CASE 1
            MATREAD EST.REC FROM ESTIMATE, CONO : S$VALUE ELSE
               ERRMSG = "Cannot locate estimate. Try again! "
               GOSUB 90000; GOTO 110
            END
            EST.KEY = S$VALUE
         END CASE
         IF EST.STATUS<1,1> # "BOOKED" OR EST.BOOK.JOB = "" THEN
            ERRMSG = "Estimate status must be booked"
            GOSUB 90000; GOTO 100
         END
         IF EST.JOB = "" OR EST.BOOK.JOB = "" THEN
            ERRMSG = "No job for this estimate"
            GOSUB 90000; GOTO 110
         END
         MATREAD JOB.REC FROM JOB, CONO : EST.JOB ELSE
            ERRMSG = "Cannot locate job - ":EST.JOB
            GOSUB 90000; GOTO 110
         END
         MATREAD CUST.REC FROM CUSTOMER, CONO:JOB.CUST ELSE
            ERRMSG = "Cannot locate customer - ":JOB.CUST
            GOSUB 90000; GOTO 110
         END
         GOSUB 80000
         CNT = COUNT(JOB.DESC,VM) + (JOB.DESC # "")
         REF.NO = 1
         GOSUB 82000
      CASE 1
         ERRMSG = "ERROR IN PROCREAD"
         GOSUB 90000; GOTO 999999
      END CASE
*
*--- GET OPERATOR REQUEST
*
500*
      SCREEN FIELD;;9
      SCREEN INPUT;;9
      OPTION = S$VALUE
      BEGIN CASE
      CASE OPTION = "E" OR OPTION = "END"
         GOTO 100
      CASE OPTION = "P" AND PROG.FLG = "J"
         LOCATE JOB.NUM IN EST.BOOK.JOB<1>,1 SETTING JFND ELSE JFND = 0
         IF JFND THEN
            IF EST.BOOK.COMP<1,JFND> = "ALL" THEN
               COMP.NUM = "ALL"
            END ELSE
               COMP.NUM = EST.BOOK.COMP<1,JFND>
            END
         END ELSE
            ERRMSG = "This job is not for this estimate"
            GOSUB 90000
            GOTO 100
         END
         EST.KEY = JOB.EST
         CALL JOB.SCHED.BUILD.UPD (CONO , "", EST.KEY, JOB.NUM, COMP.NUM, CUST.NAME,LOADING.FLG)
         GOTO 100
      CASE OPTION = "P" AND PROG.FLG = "E"
         BAD.JOB = ""
         MAST.CUST = JOB.CUST
         JCNT = COUNT(EST.BOOK.JOB,VM) + (EST.BOOK.JOB # "")
         FOR I = 1 TO JCNT
            IF EST.BOOK.JOB<1,I> = "" THEN GOTO 509
            JOB.NUM = EST.BOOK.JOB<1,I>
            LOCATE JOB.NUM IN BAD.JOB,1 SETTING FND ELSE FND = 0
            IF FND THEN GOTO 509
            S$DATA(1)<S$SCR> = JOB.NUM
            SCREEN DISPLAY;;1
            LOCATE JOB.NUM IN EST.BOOK.JOB<1>,(I+1) SETTING FND ELSE FND = 0
            IF FND THEN
               ERRMSG = "ERROR in estimate booked jobs"
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            END
            MATREAD JOB.REC FROM JOB, CONO : JOB.NUM ELSE
               ERRMSG = "cannot locate job - " : JOB.NUM
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            END
            IF JOB.CUST # MAST.CUST THEN
               MATREAD CUST.REC FROM CUSTOMER, CONO : JOB.CUST ELSE
                  ERRMSG = "Cannot locate customer - " : JOB.CUST
                  GOSUB 90000
                  BAD.JOB<-1> = JOB.NUM
                  GOTO 509
               END
               MAST.CUST = JOB.CUST
            END
            IF JOB.EST # EST.KEY THEN
               ERRMSG = "Job is for estimate - " : JOB.EST
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            END
            BEGIN CASE
            CASE JOB.STATUS<1,1> = 9
               ERRMSG = "This job is cancelled"
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            CASE JOB.STATUS # ""
               ERRMSG = "Job status must be booked"
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            END CASE
            FND = 1
            READU REC FROM JOB.SCHED, CONO : JOB.NUM ELSE FND = 0
            REC = ""
            IF FND THEN
               RELEASE JOB.SCHED, CONO : JOB.NUM
               ERRMSG = "JOB.SCHED exists for this job"
               GOSUB 90000
               BAD.JOB<-1> = JOB.NUM
               GOTO 509
            END
            COMP.NUM = EST.BOOK.COMP<1,I>
            CALL JOB.SCHED.BUILD.UPD (CONO , "", EST.KEY, JOB.NUM, COMP.NUM, CUST.NAME,LOADING.FLG)
509*
         NEXT I
         GOTO 100
      CASE OPTION = "P"
         ERRMSG = "ERROR IN PROCREAD. NO PROCESS IS DONE."
         GOSUB 90000
         GOTO 999999
      CASE OPTION = "S"
         REF.NO = CURR.REF.NO + LINE.COUNT
         IF REF.NO > CNT THEN REF.NO = 1
         GOSUB 82000
      END CASE
      GOTO 500
*-----------------------*
*---   SUBROUTINES   ---*
*-----------------------*
*
*--- REFORMAT SCREEN
*
1000*
      SCREEN FORMAT
      GOSUB 81000
      CUR.REF.NO = ""
      REF.NO = 1
      GOSUB 82000
      RETURN
*
*--- XREF
*
10000*
      GXR.ID = ""
      SCREEN FIELD;;5
      SCREEN INPUT;;5;RETURN
      BEGIN CASE
      CASE S$VALUE = "END"
         GOTO 10999
      CASE S$VALUE = ""
         SCREEN FIELD;;6
         SCREEN INPUT;;6
         BEGIN CASE
         CASE S$VALUE = "END" OR S$VALUE = ""
            GOTO 10000
         CASE 1
            GXR.XREF = CUSTOMER.XREF
            GXR.SRCH.ID = S$VALUE
            GXR.FILE = CUSTOMER
            GXR.LOC = 1
            GXR.TOP.LINE = "CUSTOMER XREF SEARCH"
            GXR.TOP.LINE<1,2> = "SEARCH BY NAME : " : S$VALUE
            GXR.HEADING = "CUSTOMER"
            GXR.ATT = 0
            GXR.LEN = 10
            GXR.HEADING<1,2> = "DESCRIPTION"
            GXR.ATT<1,2> = 1
            GXR.LEN<1,2> = 60
            GXR.ID = ""
            GXR.OCONV = ""
            CALL GEN.XREF1 (MAT GEN.XREF.REC,PREFIX)
            S$DATA(5)<S$SCR> = GXR.ID
            IF GXR.ACTION # "X" THEN
               GOSUB 1000
            END ELSE
               SCREEN DISPLAY;;5
            END
            BEGIN CASE
            CASE GXR.ID = "END" OR GXR.ID = ""
               GOTO 10000
            CASE 1
               MATREAD CUST.REC FROM CUSTOMER, CONO : GXR.ID ELSE
                  ERRMSG = "Invalid customer. Try again! "
                  GOSUB 90000
                  GOTO 10000
               END
            END CASE
         END CASE
      CASE 1
         MATREAD CUST.REC FROM CUSTOMER, CONO : S$VALUE ELSE
            ERRMSG = "Invalid customer. Try again! "
            GOSUB 90000
            GOTO 10000
         END
         GXR.ID = S$VALUE
      END CASE
      S$DATA(6)<S$SCR> = CUST.NAME
      SCREEN DISPLAY;;6
10999*
      RETURN
*
*--- LOAD AND PRINT SCREEN
*
80000*
      S$DATA(1)<S$SCR> = JOB.NUM
      S$DATA(2)<S$SCR> = JOB.EST
      IF JOB.MASTER = "" THEN
         S$DATA(3)<S$SCR> = JOB.NUM
      END ELSE
         S$DATA(3)<S$SCR> = JOB.MASTER
      END
      IF EST.MASTER = "" THEN
         S$DATA(4)<S$SCR> = JOB.EST
      END ELSE
         S$DATA(4)<S$SCR> = EST.MASTER
      END
      S$DATA(5)<S$SCR> = JOB.CUST
      S$DATA(6)<S$SCR> = CUST.NAME
      S$DATA(8)<S$SCR> = JOB.DESC
81000*
      SCREEN DISPLAY;;ALL
      RETURN
*
*--- DISPLAY MULTI LINE AREA
*
82000*
      START.REF.NO = 1 + INT((REF.NO - 1) / LINE.COUNT) * LINE.COUNT
      IF START.REF.NO = CURR.REF.NO THEN RETURN
      CUR.REF.NO = START.REF.NO
      S$VAL = START.REF.NO
      S$CNT = CNT
      SCREEN MULTI;;C;7;8
      RETURN
*
*--- ERROR ROUTINE
*
90000 ERR.TYPE=1;CALL SI_SYSCOM(MAT SYSCOM.REC);RETURN
* 90000*
*       PRINT @(0,23) : CL : ERRMSG :
*       INPUT REPLY,1 :
*       PRINT @(0,23) : CL :
*       RETURN
999999*
*       PRINT @(-1):
   END
