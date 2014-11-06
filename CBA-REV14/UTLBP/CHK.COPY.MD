      PROMPT ""
      PROCREAD BUFFER ELSE STOP
      OPEN "","DL/ID" TO MD ELSE STOP
      FROM.MD = BUFFER<1>
      F.MD.LEN = LEN(FROM.MD)
      TO.MD = BUFFER<2>
*      PORT.ACCT = OCONV(0,"U50BB")
*      PORT = FIELD(PORT.ACCT," ",1)
*      FROM.ACCT = FIELD(PORT.ACCT," ",2)
      PORT = SYSTEM(18)
      FROM.ACCT = SYSTEM(19)
      PORT.ACCT = PORT:" ":FROM.ACCT
      QF.ID = "QF*":PORT
      READU O.QFILE FROM MD, QF.ID ELSE O.QFILE = ""
      QFILE = "Q"
      QFILE<2> = FROM.MD
      QFILE<3> = "DL/ID"
      WRITEU QFILE ON MD, QF.ID
      OPEN "",QF.ID TO F.MD ELSE STOP
      QFILE<2> = TO.MD
      WRITEU QFILE ON MD, QF.ID
      OPEN "",QF.ID TO T.MD ELSE STOP
      IF O.QFILE = "" THEN
         DELETE MD, QF.ID
      END ELSE
         WRITE O.QFILE ON MD, QF.ID
      END
      BEGIN CASE
      CASE FROM.MD = "CBA"
         F.PTR = "CBA"
         F.PTR<2> = "PRIMAC"
      CASE FIELD(FROM.MD,"-",1) = "CBA"
         F.PTR = FROM.MD
         F.PTR<2> = FIELD(FROM.MD,"-",2)
      CASE FIELD(FROM.MD,"-",1) = "PRIMAC"
         F.PTR = "CBA"
         F.PTR<2> = "PRIMAC"
      CASE 1
         F.PTR = "CBA-" : FIELD(FROM.MD,"-",1)
         F.PTR<2> = FIELD(FROM.MD,"-",1)
      END CASE
      BEGIN CASE
      CASE TO.MD = "CBA"
         T.PTR = "CBA"
         T.PTR<2> = "PRIMAC"
      CASE FIELD(TO.MD,"-",1) = "CBA"
         T.PTR = TO.MD
         T.PTR<2> = FIELD(TO.MD,"-",2)
      CASE FIELD(TO.MD,"-",1) = "PRIMAC"
         T.PTR = "CBA"
         T.PTR<2> = "PRIMAC"
      CASE 1
         T.PTR = "CBA-" : FIELD(TO.MD,"-",1)
         T.PTR<2> = FIELD(TO.MD,"-",1)
      END CASE
      R.PTR = "CBA-8.1"
      R.PTR<2> = "8.1"
      F.PTR<3> = F.PTR<2> : "-ARS"; T.PTR<3> = T.PTR<2> : "-ARS"
      R.PTR<3> = "8.1-ARS"
      F.PTR<4> = F.PTR<2> : "-APS"; T.PTR<4> = T.PTR<2> : "-APS"
      R.PTR<4> = "8.1-APS"
      F.PTR<5> = F.PTR<2> : "-PRS"; T.PTR<5> = T.PTR<2> : "-PRS"
      R.PTR<5> = "8.1-PRS"
      F.PTR<6> = F.PTR<2> : "-FAS"; T.PTR<6> = T.PTR<2> : "-FAS"
      R.PTR<6> = "8.1-FAS"
      F.PTR<7> = F.PTR<2> : "-GLS"; T.PTR<7> = T.PTR<2> : "-GLS"
      R.PTR<7> = "8.1-GLS"
      F.PTR<8> = F.PTR<2> : "-SAS"; T.PTR<8> = T.PTR<2> : "-SAS"
      R.PTR<8> = "8.1-SAS"
      F.PTR<9> = F.PTR<2> : "-ICS"; T.PTR<9> = T.PTR<2> : "-ICS"
      R.PTR<9> = "8.1-ICS"
      F.PTR<10> = F.PTR<2> : "-POS"; T.PTR<10> = T.PTR<2> : "-POS"
      R.PTR<10> = "8.1-POS"
      F.PTR<11> = F.PTR<2> : "-JES"; T.PTR<11> = T.PTR<2> : "-JES"
      R.PTR<11> = "8.1-JES"
      F.PTR<12> = F.PTR<2> : "-JCS"; T.PTR<12> = T.PTR<2> : "-JCS"
      R.PTR<12> = "8.1-JCS"
      F.PTR<13> = F.PTR<2> : "-PSS"; T.PTR<13> = T.PTR<2> : "-PSS"
      R.PTR<13> = "8.1-PSS"
      F.PTR<14> = F.PTR<2> : "-MLS"; T.PTR<14> = T.PTR<2> : "-MLS"
      R.PTR<14> = "8.1-MLS"
      F.PTR<15> = F.PTR<2> : "-EES"; T.PTR<15> = T.PTR<2> : "-EES"
      R.PTR<15> = "8.1-EES"
      SELECT F.MD
      DATA = 1
      LOOP
         READNEXT ID ELSE DATA = 0
      WHILE DATA DO
         READ F.ITEM FROM F.MD, ID ELSE GOTO 99
         BEGIN CASE
         CASE ID = FROM.MD
            GOTO 99
         CASE ID = "MD" OR ID = "M/DICT" OR ID = "PROC" OR ID = "DL/ID"
            GOTO 99
         CASE ID = "QFILE"
            GOTO 99
         CASE ID[1,3] = "QF*"
            GOTO 99
         CASE ID[1,14] = "EASY.MENU.PROC"
            GOTO 99
         CASE ID[1,15] = "JCR.REPORT.PROC"
            GOTO 99
         CASE ID[LEN(ID)-5,6] = ".LOGTO"
            GOTO 99
         CASE F.ITEM<1> = "PQN"
         CASE F.ITEM<2> = "10B4"
            BEGIN CASE
            CASE F.ITEM<5>[1,7] = "SYSPROG"
               GOTO 99
            CASE F.ITEM<5>[1,F.MD.LEN] = FROM.MD
               GOTO 99
            CASE F.ITEM<5>[1,8] = "REALCALC"
               GOTO 99
            CASE F.ITEM<5>[1,8] = "REALLINK"
               GOTO 99
            CASE F.ITEM<5>[1,2] = "WP"
               GOTO 99
            END CASE
            R.ITEM = F.ITEM
            NAME = FIELD(F.ITEM<5>," ",2)
            IF NAME = "" THEN
               F.ITEM<5> = T.PTR<1>
               R.ITEM<5> = R.PTR<1>
            END ELSE
               F.ITEM<5> = T.PTR<1> : " " : NAME
               R.ITEM<5> = R.PTR<1> : " " : NAME
            END
         CASE F.ITEM<1> = "Q"
            X.ITEM = F.ITEM<1>
            X.ITEM<2> = F.ITEM<2>; X.ITEM<3> = F.ITEM<3>
            F.ITEM = X.ITEM; X.ITEM = ""
            BEGIN CASE
            CASE F.ITEM<2> = "SYSPROG"
               GOTO 99
            CASE F.ITEM<2> = "REALCALC"
               GOTO 99
            CASE F.ITEM<2> = "REALLINK"
               GOTO 99
            CASE F.ITEM<2> = "WP"
               GOTO 99
            END CASE
            R.ITEM = F.ITEM
            LOCATE F.ITEM<2> IN F.PTR,1 SETTING FND ELSE FND = 0
            IF FND THEN F.ITEM<2> = T.PTR<FND>
            LOCATE R.ITEM<2> IN R.PTR,1 SETTING FND ELSE FND = 0
            IF FND THEN R.ITEM<2> = R.PTR<FND>
         CASE 1
            GOTO 99
         END CASE
         READ T.ITEM FROM T.MD, ID ELSE T.ITEM = ""
         IF T.ITEM<1> = "Q" THEN
            X.ITEM = "Q"
            X.ITEM<2> = T.ITEM<2>; X.ITEM<3> = T.ITEM<3>
            T.ITEM = X.ITEM; X.ITEM = ""
         END
         IF F.ITEM = T.ITEM THEN GOTO 99
         IF R.ITEM = T.ITEM THEN GOTO 99
         PRINT CHAR(12)
         PRINT SPACE(10) : FROM.MD "L#40" : TO.MD
         PRINT
         PRINT "ITEM ID : " : ID
         PRINT
         DATA = DATA + 1
         F.CNT = COUNT(F.ITEM,CHAR(254)) + 1
         T.CNT = COUNT(T.ITEM,CHAR(254)) + 1
         IF F.CNT > T.CNT THEN CNT = F.CNT ELSE CNT = T.CNT
         FOR I = 1 TO CNT
            PRINT I "L#3" : F.ITEM<I> "L#35" : " <-> " : T.ITEM<I>
         NEXT I
         PRINT
         PRINT "ITEM ID : " : ID
         PRINT
         PRINT "(C)opy, (D)elete, <RETURN> : " :
         INPUT ANS
         BEGIN CASE
         CASE ANS = "C"
            WRITE F.ITEM ON T.MD, ID
         CASE ANS = "D"
            DELETE F.MD, ID
         END CASE
99    REPEAT
   END
