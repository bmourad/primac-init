*COPY>CPYLIB>COMMON1
**************************************************************************
* REVISION    - [08.0]
* SYSTEM        - PRIMAC
* PROGRAM       - DICT.BUILD
* BY            - JIHAD YAMOUT, C.B.A
* DATE          - 12/13/84
* DESCRIPTION
**************************************************************************
*
**** INSERT FILE EQUATES
*
*COPY>CPYLIB>DICT.FILE
*COPY>CPYLIB>EDIT.COM
*COPY>CPYLIB>FILE.VARS
*COPY>CPYLIB>EDIT.COM.DRIVER
*COPY>CPYLIB>SYSCOM
*COPY>CPYLIB>CHAR
*
**** SETUP FOR SYSTEM ERRMSGS
*
     SYS.TYPE = 1
     CALL SYSCOM(MAT SYSCOM.REC)
*
**** OPEN FILES
*
   OPEN '','CPYLIB' TO CPYLIB ELSE
       ERRMSG = 'CPYLIB FILE IS MISSING'
       GOTO 93000
   END
   OPEN '','UTL.SCREENS' TO M.SCREENS ELSE
       ERRMSG = 'UTL.SCREENS FILE IS MISSING'
       GOTO 93000
   END
   OPEN '','CONTROL' TO CONTROL ELSE
      ERRMSG = 'CANNOT LOCATE CONTROL FILE'
      GOTO 93000
   END
   NO.CUST.DICT = 0
   READ MENUS.CONTROL FROM CONTROL, "MENUS.CONTROL" ELSE NO.CUST.DICT = 1
   IF NO.CUST.DICT = 0 THEN
      CUST.DICT.NAME = MENUS.CONTROL<1> : '.DICT'
      OPEN '',CUST.DICT.NAME TO CUST.DICT ELSE NO.CUST.DICT = 1
   END
*
**** MAIN PROCESSING
*
   MAT EDIT.COM.DRIVER = ''
   MAT EDIT.COM = ''
   TYP = 0 ; CALL EDIT.SUB
   FILL = "#"
   ECD.SCRN.CNT = 18
   ECD.SCRN.NAME<1> = "DICT.BUILD"
   ECD.SCRN.NAME<2> = "DICT.BUILD.SUB0"
   ECD.SCRN.NAME<3> = "DICT.BUILD.SUBA"
   ECD.SCRN.NAME<4> = "DICT.BUILD.SUBC"
   ECD.SCRN.NAME<5> = "DICT.BUILD.SUBD"
   ECD.SCRN.NAME<6> = "DICT.BUILD.SUBD1"
   ECD.SCRN.NAME<7> = "DICT.BUILD.SUBD2"
   ECD.SCRN.NAME<8> = "DICT.BUILD.SUBF"
   ECD.SCRN.NAME<9> = "DICT.BUILD.SUBG"
   ECD.SCRN.NAME<10> = "DICT.BUILD.SUBMD"
   ECD.SCRN.NAME<11> = "DICT.BUILD.SUBMF"
   ECD.SCRN.NAME<12> = "DICT.BUILD.SUBMP"
   ECD.SCRN.NAME<13> = "DICT.BUILD.SUBMT"
   ECD.SCRN.NAME<14> = "DICT.BUILD.SUBMX"
   ECD.SCRN.NAME<15> = "DICT.BUILD.SUBT"
   ECD.SCRN.NAME<16> = "DICT.BUILD.SUBTF"
   ECD.SCRN.NAME<17> = "DICT.BUILD.SUBU"
   ECD.SCRN.NAME<18> = "DICT.BUILD.SUBV"
   ECD.ACTION=1;CALL SCREEN.EDIT
**** PRINT SCREEN
1*
   ECD.SCRN.NO = 1
   MAT DICT.REC = ""
   S.NAME = ""
   S.CONV = ""
   S.CORR = ""
   ECD.RET.VALUES<ECD.SCRN.NO> = ''
   ECD.ACTION=6;CALL SCREEN.EDIT
*
**** ENTER FILE NAME
*
5*
  ECD.NUM = 1
  IF FILE.NAME # "" THEN ECD.DEFAULT = FILE.NAME
  ECD.ACTION=4;CALL SCREEN.EDIT
  IF ECD.RET.VALUE = "END" THEN GOTO 99999
  FILE.NAME = ECD.RET.VALUE
  OPEN '',FILE.NAME TO F.NAME ELSE
    ERRMSG = FILE.NAME:" IS MISSING "
    GOSUB 91000 ; GOTO 5
  END
  OPEN 'DICT',FILE.NAME TO D.F.NAME ELSE
    ERRMSG = "DICT ":FILE.NAME:" IS MISSING"
    GOSUB 91000 ; GOTO 5
  END
*
**** ENTER ITEM NAME
*
7*
  FND = 1
  ECD.NUM = 2
  ECD.ACTION = 4;CALL SCREEN.EDIT
  IF ECD.RET.VALUE = 'END' THEN GOTO 1
  ITEM.NAME = ECD.RET.VALUE
  MATREAD DICT.REC FROM D.F.NAME , ITEM.NAME ELSE
    MAT DICT.REC = '' ; FND = 0
    FOR X = 1 TO 7
      ON X GOSUB 100,200,300,400,500,600,700
      IF ECD.RET.VALUE = "END" THEN GOTO 1
    NEXT X
  END
  IF FND = 1 THEN GOSUB 1000
  GOSUB 60000
  GOTO 1
*
**** ENTER D.CODE
*
100*
   ECD.NUM = 3
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE # 'END' THEN
      DICT.D.CODE = ECD.RET.VALUE
   END
   RETURN
*
**** ENTER A.AMC
*
200*
   ECD.NUM = 4
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE # 'END' THEN
      DICT.A.AMC = ECD.RET.VALUE
   END
   RETURN
*
**** ENTER FIRST HEADING LINE
*
300*
   ECD.NUM = 5
   ECD.DEFAULT = ITEM.NAME
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 399
   IF ECD.RET.VALUE = '' THEN ECD.RET.VALUE = ' '
   S.NAME<1,1> = ECD.RET.VALUE
*
**** ENTER SECOND HEADING LINE
*
   ECD.NUM = 6
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 399
   S.NAME<1,2> = ECD.RET.VALUE
*
**** ENTER THIRD HEADING LINE
*
   ECD.NUM = 7
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 399
   S.NAME<1,3> = ECD.RET.VALUE
399*
   DICT.S.NAME = S.NAME<1,1>:S.NAME<1,2>:S.NAME<1,3>
   ECD.NUM=5;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.S.NAME[1,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=6;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.S.NAME[60,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=7;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.S.NAME[120,59];ECD.ACTION=5;CALL SCREEN.EDIT
   RETURN
*
**** ENTER FIRST CONV LINE
*
400*
   ECD.NUM = 8
   IF S.CONV<1,1> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[1,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CONV<1,1>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 499
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 400
   END
   S.CONV<1,1> = ECD.RET.VALUE
*
**** SECOND CONV LINE
*
   ECD.NUM = 9
   IF S.CONV<1,2> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[60,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CONV<1,2>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 499
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 400
   END
   S.CONV<1,2> = ECD.RET.VALUE
*
**** ENTER THIRD CONV LINE
*
   ECD.NUM = 10
   IF S.CONV<1,3> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[120,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CONV<1,3>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 499
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 400
   END
   S.CONV<1,3> = ECD.RET.VALUE
499*
   DICT.CONV = S.CONV<1,1>:S.CONV<1,2>:S.CONV<1,3>
   ECD.NUM=8;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[1,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=9;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[60,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=10;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CONV[120,59];ECD.ACTION=5;CALL SCREEN.EDIT
   RETURN
*
**** ENTER FIRST CORR LINE
*
500*
   ECD.NUM = 11
   IF S.CORR<1,1> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[1,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CORR<1,1>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 599
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 500
   END
   S.CORR<1,1> = ECD.RET.VALUE
*
**** ENTER SECOND CORR LINE
*
   ECD.NUM = 12
   IF S.CORR<1,2> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[60,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CORR<1,2>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 599
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 500
   END
   S.CORR<1,2> = ECD.RET.VALUE
*
**** ENTER THIRD CORR LINE
*
   ECD.NUM = 13
   IF S.CORR<1,3> = "" THEN
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[120,59]
   END ELSE
      ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = S.CORR<1,3>
   END
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE = 'END' THEN GOTO 599
   IF ECD.RET.VALUE = "???" THEN
      GOSUB 5000;GOTO 500
   END
   S.CORR<1,3> = ECD.RET.VALUE
599*
   DICT.CORR = S.CORR<1,1>:S.CORR<1,2>:S.CORR<1,3>
   ECD.NUM=11;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[1,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=12;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[60,59];ECD.ACTION=5;CALL SCREEN.EDIT
   ECD.NUM=13;ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM,1> = DICT.CORR[120,59];ECD.ACTION=5;CALL SCREEN.EDIT
   RETURN
*
**** ENTER JUSTIFY
*
600*
   ECD.NUM = 14
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE # "END" THEN
      DICT.JUSTIFY = ECD.RET.VALUE
   END
   RETURN
*
**** ENTER MAXL LENGTH
*
700*
   ECD.NUM = 15
   SAVE.S.NAME = DICT.S.NAME<1,1>:DICT.S.NAME<1,2>:DICT.S.NAME<1,3>
   SAVE.S.NAME = FIELD(SAVE.S.NAME,"}",1)
   ECD.DEFAULT = LEN(SAVE.S.NAME)
   ECD.ACTION=4;CALL SCREEN.EDIT
   IF ECD.RET.VALUE # 'END' THEN
      DICT.MAX.LEN = ECD.RET.VALUE
   END
   RETURN
*
**** GET ALL VALUES FROM FILE
*
1000*
      L.VALUE = LEN(DICT.S.NAME)
      V.VALUE = DICT.S.NAME
      GOSUB 3000
      DICT.S.NAME = FV.VALUE
      L.VALUE = LEN(DICT.CONV)
      V.VALUE = DICT.CONV
      GOSUB 3000
      DICT.CONV = FV.VALUE
      L.VALUE = LEN(DICT.CORR)
      V.VALUE = DICT.CORR
      GOSUB 3000
      DICT.CORR = FV.VALUE
      ECD.RET.VALUES<ECD.SCRN.NO,3,1> = DICT.D.CODE
      ECD.RET.VALUES<ECD.SCRN.NO,4,1> = DICT.A.AMC
      ECD.RET.VALUES<ECD.SCRN.NO,5,1> = DICT.S.NAME[1,59]
      ECD.RET.VALUES<ECD.SCRN.NO,6,1> = DICT.S.NAME[60,59]
      ECD.RET.VALUES<ECD.SCRN.NO,7,1> = DICT.S.NAME[120,59]
      ECD.RET.VALUES<ECD.SCRN.NO,8,1> = DICT.CONV[1,59]
      ECD.RET.VALUES<ECD.SCRN.NO,9,1> = DICT.CONV[60,59]
      ECD.RET.VALUES<ECD.SCRN.NO,10,1> = DICT.CONV[120,59]
      ECD.RET.VALUES<ECD.SCRN.NO,11,1> = DICT.CORR[1,59]
      ECD.RET.VALUES<ECD.SCRN.NO,12,1> = DICT.CORR[60,59]
      ECD.RET.VALUES<ECD.SCRN.NO,13,1> = DICT.CORR[120,59]
      ECD.RET.VALUES<ECD.SCRN.NO,14,1> = DICT.JUSTIFY
      ECD.RET.VALUES<ECD.SCRN.NO,15,1> = DICT.MAX.LEN
      ECD.ACTION = 3;CALL SCREEN.EDIT
    RETURN
*
**** CHANGE ALL VALUE MARK TO ]
*
3000*
    FV.VALUE = ''
    FOR LL = 1 TO L.VALUE
      IF V.VALUE[LL,1] = VM THEN
         FV.VALUE = FV.VALUE:"}"
      END ELSE
         FV.VALUE = FV.VALUE:V.VALUE[LL,1]
      END
    NEXT LL
    RETURN
*
**** CHANGE ALL ] TO VALUE MARK
*
4000*
    FV.VALUE = ''
    FOR LL = 1 TO L.VALUE
      IF V.VALUE[LL,1] = "}" THEN
         FV.VALUE = FV.VALUE:VM
      END ELSE
         FV.VALUE = FV.VALUE:V.VALUE[LL,1]
      END
    NEXT LL
    RETURN
*
**** PRINT PRIMARY XREF SCREEN
*
5000*
    ECD.SCRN.NO = 2 ; ECD.ACTION = 2 ; CALL SCREEN.EDIT
    GOSUB 50000
    ECD.SCRN.NO = 1 ; ECD.ACTION = 2 ; CALL SCREEN.EDIT
    ECD.ACTION = 3 ; CALL SCREEN.EDIT
    RETURN
*
**** PRINT SECONDARY XREF SCREENS
*
6000*
    ECD.ACTION = 2 ; CALL SCREEN.EDIT
    ERRMSG = "PRESS <RETURN> TO CONTINUE :" ; GOSUB 91000
    ECD.SCRN.NO = 2 ; ECD.ACTION = 2 ; CALL SCREEN.EDIT
    RETURN
*
**** PRINT CODES DEFINITION
*
50000*
   HELP = 0
   LOOP
    ECD.NUM = 1
    ECD.RET.VALUES<ECD.SCRN.NO,ECD.NUM> = ''
    ECD.ACTION=4;CALL SCREEN.EDIT
    OPTION = ECD.RET.VALUE
    BEGIN CASE
      CASE OPTION = 'END' OR OPTION = ''
        HELP = 1
      CASE OPTION = 'A'
        ECD.SCRN.NO = 3; GOSUB 6000
      CASE OPTION = 'C'
        ECD.SCRN.NO = 4; GOSUB 6000
      CASE OPTION = 'D'
        ECD.SCRN.NO = 5; GOSUB 6000
      CASE OPTION = 'D1'
        ECD.SCRN.NO = 6; GOSUB 6000
      CASE OPTION = 'D2'
        ECD.SCRN.NO = 7; GOSUB 6000
      CASE OPTION = 'F'
        ECD.SCRN.NO = 8; GOSUB 6000
      CASE OPTION = 'G'
        ECD.SCRN.NO = 9; GOSUB 6000
      CASE OPTION = 'MD'
        ECD.SCRN.NO = 10; GOSUB 6000
      CASE OPTION = 'MF'
        ECD.SCRN.NO = 11; GOSUB 6000
      CASE OPTION = 'MP'
        ECD.SCRN.NO = 12; GOSUB 6000
      CASE OPTION = 'MT'
        ECD.SCRN.NO = 13; GOSUB 6000
      CASE OPTION = 'MX'
        ECD.SCRN.NO = 14; GOSUB 6000
      CASE OPTION = 'T'
        ECD.SCRN.NO = 15; GOSUB 6000
      CASE OPTION = 'TFILE'
        ECD.SCRN.NO = 16; GOSUB 6000
      CASE OPTION = 'U'
        ECD.SCRN.NO = 17; GOSUB 6000
      CASE OPTION = 'V'
        ECD.SCRN.NO = 18; GOSUB 6000
    END CASE
    WHILE HELP = 0 DO REPEAT
    RETURN
*
**** ENTER OPTIONS
*
60000*
  MORE = 0
  LOOP
    ECD.NUM = 16
    ECD.RET.VALUES<1,ECD.NUM> = ''
    ECD.ACTION=4;CALL SCREEN.EDIT
    OPTION = ECD.RET.VALUE
    BEGIN CASE
       CASE OPTION = 'END' OR OPTION = 'E'
           MORE = 1
       CASE NUM(OPTION)
           ON OPTION GOSUB 100,200,300,400,500,600,700
       CASE OPTION = 'D'
           X=0;Y=21;O.R='R';TYP=8
           PMSG='YOU ARE ABOUT TO DELETE THIS ITEM OK (Y/N)'
           CALL EDIT.SUB
           PRINT @(0,21):CL:
           IF VALUE = "Y" THEN
             DELETE D.F.NAME,ITEM.NAME
             IF NO.CUST.DICT = 0 THEN
                DELETE CUST.DICT, FILE.NAME : "!" : ITEM.NAME
             END
             MORE = 1
           END
       CASE OPTION = 'C'
           L.VALUE = LEN(DICT.S.NAME)
           V.VALUE = DICT.S.NAME
           GOSUB 4000
           DICT.S.NAME = FV.VALUE
           L.VALUE = LEN(DICT.CONV)
           V.VALUE = DICT.CONV
           GOSUB 4000
           DICT.CONV = V.VALUE
           L.VALUE = LEN(DICT.CORR)
           V.VALUE = DICT.CORR
           GOSUB 4000
           DICT.CORR = FV.VALUE
60001      X=0;Y=21;TYP=1;O.R='O';MAXL=24;DEFAULT=FILE.NAME;PMSG="ENTER FILE TO COPY TO"
           CALL EDIT.SUB
           PRINT @(0,21):CL:
           T.FILE.NAME = VALUE
           IF VALUE # "END" THEN
              OPEN "DICT",VALUE TO T.D.FILE ELSE
                   ERRMSG = VALUE:" FILE IS MISSING" ; GOSUB 91000
                   GOTO 60001
              END
60002         X=0;Y=21;TYP=1;O.R='O';MAXL=24;DEFAULT = ITEM.NAME ;PMSG="ENTER ITEM NAME TO COPY TO"
              CALL EDIT.SUB
              PRINT @(0,21):CL:
              IF VALUE = 'END' THEN GOTO 60001
              T.ITEM.NAME = VALUE
              READV CHECK FROM T.D.FILE,VALUE,1 ELSE CHECK = ''
              IF CHECK # '' THEN
                 X=0;Y=21;TYP=8;O.R='R';PMSG="ITEM EXIST ON FILE OK TO OVERWRITE (Y/N)"
                 CALL EDIT.SUB
                 PRINT @(0,21):CL:
                 IF VALUE = 'N' THEN GOTO 60002
              END
              X=0;Y=21;TYP=8;O.R='R';PMSG = "ITEM (":T.ITEM.NAME:") IN FILE (":T.FILE.NAME:") WILL BE WRITTEN OK (Y/N)"
              CALL EDIT.SUB
              PRINT @(0,21):CL:
              IF VALUE = "Y" THEN
                 MATWRITE DICT.REC ON T.D.FILE ,T.ITEM.NAME
              END ELSE
                 GOSUB 60001
              END
              MORE = 1
           END
       CASE OPTION = 'F' OR OPTION = 'S'
           L.VALUE = LEN(DICT.S.NAME)
           V.VALUE = DICT.S.NAME
           GOSUB 4000
           DICT.S.NAME = FV.VALUE
           L.VALUE = LEN(DICT.CONV)
           V.VALUE = DICT.CONV
           GOSUB 4000
           DICT.CONV = V.VALUE
           L.VALUE = LEN(DICT.CORR)
           V.VALUE = DICT.CORR
           GOSUB 4000
           DICT.CORR = FV.VALUE
           MATWRITE DICT.REC ON D.F.NAME , ITEM.NAME
           IF OPTION = 'S' AND NO.CUST.DICT = 0 THEN
              MATWRITE DICT.REC ON CUST.DICT, FILE.NAME : "!" : ITEM.NAME
           END
           MORE = 1
       END CASE
    WHILE MORE = 0 DO REPEAT
    RETURN
*
**** CALLS FOR SYSCOM
*
91000 ERR.TYPE = 1
      CALL SYSCOM(MAT SYSCOM.REC)
      RETURN
92000 ERR.TYPE = 2
      CALL SYSCOM(MAT SYSCOM.REC)
      RETURN
93000 ERR.TYPE = 3
      CALL SYSCOM(MAT SYSCOM.REC)
99999 PRINT @(-1):
      END