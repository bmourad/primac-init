SUBROUTINE WIN.TCL
* Save screen and start TCL session
* Compile for: GENERIC AP MD ME PI.SUB PR.SUB SQ UD UL UP UV UC IN GA
* (C) Copyright IBM Corporation 2003. All rights reserved
* Copyright (c) 1991-97. Impact Business Systems
*
AMVM=1; AMVM<1,2>=2; AMVM<2>=3
AM=AMVM[4,1]
VM=AMVM[2,1]
*
PROMPT ""
STATE=''
CALL WIN.SSTATE(STATE,'')
CALL WIN.SPUSH    
*
GOSUB 100; * Initialise
EXECSUB = "WIN.EXEC":SUB.SUFFIX
*
PRINT @(-1):"wIntegrate TCL - Press ? for help, EX to quit"
FINISHED=0
*
LOOP      
   CALL WIN.STACK(1)
   PRINT ">":
   INPUT CMND      
   CALL WIN.STACK(0)
*
   WORD = FIELD(TRIM(CMND)," ",1)
   WORD = OCONV(WORD, "MCU")
*
   BEGIN CASE
   CASE WORD = "EX";FINISHED=1
   CASE WORD = "Q"; FINISHED=1
   CASE WORD = "?"; GOSUB 1000;* Display Help
   CASE WORD[1,1]="."; GOSUB 2000; * Dot commands
   CASE 1
      IF TRIM(CMND) # '' THEN CALL @EXECSUB(CMND,'','')
   END CASE
*
UNTIL FINISHED DO
REPEAT
*
CALL WIN.SPULL
CALL WIN.SSTATE("",STATE)
*
RETURN
*
* Initialisation
100 MACHINE.TYPE = ''
OPEN.OK = 1
OPEN '','WIN.PARAMS' TO F.BP ELSE
    OPEN '','WIN.PROGS' TO F.BP ELSE OPEN.OK = 0
END
IF OPEN.OK THEN
   READV MACHINE.TYPE FROM F.BP,"MACHINE.TYPE",1 ELSE MACHINE.TYPE=''
END
*
    SUB.SUFFIX =''
    IF MACHINE.TYPE # "GENERIC" AND MACHINE.TYPE # "" THEN SUB.SUFFIX = "." : MACHINE.TYPE
*
*
RETURN
*
* Help
1000 TEXT = "This program executes all the commands entered except the"
TEXT<-1> = "following which have special meanings"
TEXT<-1> = ' '
TEXT<-1> = "EX     Exit and restore the calling screen"
TEXT<-1> = "Q      as EX"
TEXT<-1> = "?      This message"                               
TEXT<-1> = ' '
TEXT<-1> = "Dot commands"
TEXT<-1> = ".E     Popup stacked commands editor"
TEXT<-1> = ".W     Start wIntegrate command line (WIN.COMLINE)"
TEXT<-1> = ".A     Query statement builder"
*
N=DCOUNT(TEXT, AM)
FOR J = 1 TO N
    PRINT "  ":TEXT<J>
NEXT J
*
RETURN
*
* Process dot commands
2000 DC = WORD[2,1]
     BEGIN CASE                                                     
     CASE DC = 'A'; SCRIPT='query\query'; GOSUB 4000; * Run script
     CASE DC = 'E'; SCRIPT='wintsys\script\TCLStack'; GOSUB 4000; * Run script
     CASE DC = "W"; PROG = "WIN.COMLINE"; GOSUB 4100; * Run program 
     CASE 1
         PRINT 'Invalid dot commands, ? for help'
     END CASE
*
     RETURN
*          
* Run a script file                                
4000 SCRIPT = 'Chain "':SCRIPT:'"'
     CALL WIN.COMSUB(SCRIPT)
     RETURN
*
* Run a program
4100 CALL @EXECSUB(PROG,'','')
     RETURN
*
END
