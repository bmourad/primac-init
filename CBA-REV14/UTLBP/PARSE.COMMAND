      SUBROUTINE PARSE.COMMAND(ACTION, CTYPE, CMD, FACCT, FDICT, FNAME, FITEM, OPTS, ERRMSG)
*********************************************************************
*
* PROGRAM - PARSE.COMMAND
*
* AUTHOR  - NICK AMENDOLA, VERCOM SOFTWARE, INC.
*
* DATE    - 06/02/92
*
* DESCRIPTION
*
*  This subroutine will parse the command line specified in CMD.
*
*   If ACTION = "V", file and item ID's will be verified.
*
*   Type 1 commands:
*
*      COMMAND (account DICT filename) itemname (options)
*      COMMAND (account DICT filename) itemname
*      COMMAND (account DICT filename) * (options)
*      COMMAND (account DICT filename) *
*      COMMAND (account DICT filename) (options)
*      COMMAND (account DICT filename)
*
*      COMMAND (account filename) itemname (options)
*      COMMAND (account filename) itemname
*      COMMAND (account filename) * (options)
*      COMMAND (account filename) *
*      COMMAND (account filename) (options)
*      COMMAND (account filename)
*
*      COMMAND DICT filename itemname (options)
*      COMMAND DICT filename itemname
*      COMMAND DICT filename * (options)
*      COMMAND DICT filename *
*      COMMAND DICT filename (options)
*      COMMAND DICT filename
*
*      COMMAND filename itemname (options)
*      COMMAND filename itemname
*      COMMAND filename * (options)
*      COMMAND filename *
*      COMMAND filename (options)
*      COMMAND filename
*
*   Type 2 commands:
*
*      (account DICT filename) itemname
*      (account DICT filename)
*
*      (account filename) itemname
*      (account filename)
*
*      (DICT filename) itemname
*      (DICT filename)
*
*      (filename) itemname
*      (filename)
*
*      itemname
*
*   CTYPE = "1" 1st line formats as defined above
*           "2" 2nd line formats as defined above
*
*   FACCT - specified account or NULL
*
*   FDICT - "DICT" or NULL
*
*   FNAME - specified filename or NULL
*
*   FITEM - specified itemname, "*" or NULL
*
*   OPTS  - multi-valued options or NULL
*
*********************************************************************
*
*---- INITIALIZATION
*
      PROMPT ""
      SYSTYPE = "U"
      BEGIN CASE
      CASE SYSTYPE = "U"
         PORT = @TTY
         IND.POS = INDEX(PORT,"/",1)
         IF IND.POS = 0 THEN
            PORT = PORT[4,LEN(PORT)-3]
         END ELSE
            PORT = PORT[4,IND.POS-4]
         END
         ACCT = @PATH
      CASE 1
         PORT = SYSTEM(18)
         ACCT = SYSTEM(19)
      END CASE
      FACCT  = ""
      FDICT  = ""
      FNAME  = ""
      FITEM  = ""
      OPTS   = ""
      ERRMSG = ""
*
      BEGIN CASE
      CASE CTYPE = "1"
         CMD = DELETE(CMD,1,0,0)
      CASE CTYPE = "2"
         TEMP = TRIM(CMD)
         PCNT = DCOUNT(TEMP," ")
         CMD = ""
         FOR P = 1 TO PCNT
            CMD<P> = FIELD(TEMP," ",P)
         NEXT P
      END CASE
*
      RFLAG = 0
      PCNT = DCOUNT(CMD,CHAR(254))
      FOR P = PCNT TO 1 STEP -1
         CMD<P> = TRIM(CMD<P>)
         BEGIN CASE
         CASE CMD<P> = ""
            CMD = DELETE(CMD,P,0,0)
         CASE CMD<P> = ")"
            CMD = DELETE(CMD,P,0,0)
            RFLAG = 1
         CASE CMD<P> = "("
            CMD = DELETE(CMD,P,0,0)
            CMD<P> = "(":CMD<P>
         CASE RFLAG
            CMD<P> = CMD<P>:")"
            RFLAG = 0
         END CASE
      NEXT P
*
*---- NORMALIZE COMMAND
*
      BEGIN CASE
      CASE CMD<1>[1,1] = "("
         CMD<1> = CMD<1>[2,99]
         L = LEN(CMD<1>)
         BEGIN CASE
         CASE CMD<1> = "DICT"              ;* (DICT filename)
            CMD = INSERT(CMD,1,0,0,"")
         CASE CMD<1>[L,1] = ")"            ;* (filename)
            CMD = INSERT(CMD,1,0,0,"")
            CMD = INSERT(CMD,2,0,0,"")
         CASE CMD<2> = ""                  ;* (filename)
            CMD = INSERT(CMD,1,0,0,"")
            CMD = INSERT(CMD,2,0,0,"")
         CASE CMD<2> = "DICT"              ;* (account DICT filename)
            NULL
         CASE 1                            ;* (account filename)
            CMD = INSERT(CMD,2,0,0,"")
         END CASE
         L = LEN(CMD<3>)
         IF CMD<3>[L,1] = ")" THEN CMD<3> = CMD<3>[1,L-1]
      CASE CTYPE = "1"
         BEGIN CASE
         CASE CMD<1> = "DICT"              ;* DICT filename item
            CMD = INSERT(CMD,1,0,0,"")
         CASE 1                            ;* filename item
            CMD = INSERT(CMD,1,0,0,"")
            CMD = INSERT(CMD,2,0,0,"")
         END CASE
      CASE CTYPE = "2"                     ;* itemname
         CMD = INSERT(CMD,1,0,0,"")
         CMD = INSERT(CMD,2,0,0,"")
         CMD = INSERT(CMD,3,0,0,"")
      END CASE
      IF CMD<4>[1,1] = "(" THEN CMD = INSERT(CMD,4,0,0,"")
*
*---- EDIT INPUT SPECIFICATION AND OPTIONS
*
      FACCT = CMD<1>
      FDICT = CMD<2>
      FNAME = CMD<3>
      FITEM = CMD<4>
      COPT  = CMD<5>
*
      IF ACTION = "V" THEN
         IF FNAME = "" THEN ERRMSG = "FILE NAME MISSING"; GOTO 99999
         IF FACCT = "" THEN
            OPEN FDICT,FNAME TO FVAR ELSE
               ERRMSG = "INVALID FILE NAME"; GOTO 99999
            END
         END ELSE
            BEGIN CASE
            CASE SYSTYPE = "U"
               OPEN "","VOC" TO MD ELSE
                  ERRMSG = "CANNOT OPEN VOC FILE"; GOTO 99999
               END
               IF FNAME[LEN(FNAME)-1,2] = "BP" OR FNAME[LEN(FNAME)-5,6] = "CPYLIB" THEN
                  QREC = "DIR"
               END ELSE
                  QREC = "F"
               END
               QREC<2> = FACCT:"/":FNAME
               QREC<3> = FACCT:"/D_":FNAME
            CASE 1
               OPEN "","MD" TO MD ELSE
                  ERRMSG = "CANNOT OPEN MASTER DICTIONARY"; GOTO 99999
               END
               QREC = "Q"
               QREC<2> = FACCT
               QREC<3> = FNAME
            END CASE
            QFILE = "QFILE*":PORT
            WRITE QREC ON MD, QFILE
            OPEN FDICT,QFILE TO FVAR THEN
               DELETE MD, QFILE
            END ELSE
               DELETE MD, QFILE
               ERRMSG = "INVALID FILE NAME"; GOTO 99999
            END
         END
*
         IF FITEM # "" AND FITEM # "*" THEN
            READV REC FROM FVAR, FITEM,1 ELSE
               ERRMSG = "INVALID ITEM NAME"; GOTO 99999
            END
         END
      END
*
      IF COPT # "" THEN
         IF COPT[1,1] # "(" THEN
            ERRMSG = "INVALID OPTION(S)"; GOTO 99999
         END
         COPT = COPT[2,99]
         IF COPT[LEN(COPT),1] = ")" THEN COPT = COPT[1,LEN(COPT)-1]
         OCNT = DCOUNT(COPT,",")
         FOR OPTR = 1 TO OCNT
            OPTS<1,-1> = FIELD(COPT,",",OPTR)
         NEXT OPTR
      END
*
*---- END OF PROGRAM
*
99999 *
      IF ERRMSG # "" THEN ERRMSG = "(PARSE.COMMAND) ":ERRMSG
      RETURN
   END
