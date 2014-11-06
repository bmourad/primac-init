*****************************************
*      DISTRIBUITION MODULE             *
*                                       *
*      PRIMAC - REVISION [09.O]         *
*                                       *
*      KEY = CONO:JOB                   *
*                                       *
*      KEY DEFINITION  - CONO (3)       *
*                        JOB.NO (8)     *
*                                       *
* (*) INDICATES MAX LENGTH           *
*****************************************
*
*
DJS.REC.LEN = 40
*
DIM DJS.REC(40)
*
EQU DJS.CUST         TO DJS.REC(1)  ;*  customer number
EQU DJS.TEMPLATE     TO DJS.REC(2)  ;*  customer template number to use for shipping list
EQU DJS.PPL          TO DJS.REC(3)  ;*  finished pieces per pound
EQU DJS.BASIS        TO DJS.REC(4)  ;*  basis weight
EQU DJS.NOP          TO DJS.REC(5)  ;*  number of pages
EQU DJS.FORMAT       TO DJS.REC(6)  ;*  standard format code
EQU DJS.WIDTH        TO DJS.REC(7)  ;*  finished item width
EQU DJS.BOX          TO DJS.REC(8)  ;*  box code
EQU DJS.ZLINE        TO DJS.REC(9)  ;*  additional "Z" line info to be printed on BOL
EQU DJS.BOX.MIN      TO DJS.REC(10) ;* minimun qty per box
EQU DJS.BOX.MAX      TO DJS.REC(11) ;* maximum qty per box
EQU DJS.SKD.MIN      TO DJS.REC(12) ;* minimum qty per skid
EQU DJS.SKD.MAX      TO DJS.REC(13) ;* maximum qty per skid
EQU DJS.LAY.QTY      TO DJS.REC(14) ;* no. of copies per skid-layer
EQU DJS.WEEK         TO DJS.REC(15) ;* T20284 ADPLEX
EQU DJS.AD.COPY      TO DJS.REC(16) ;* T20284 ADPLEX
EQU DJS.PAPER        TO DJS.REC(17) ;* T20284 ADPLEX
EQU DJS.INK          TO DJS.REC(18) ;* T20284
EQU DJS.FOR          TO DJS.REC(19) ;* T20284 ADPLEX
EQU DJS.FINISH       TO DJS.REC(20) ;* T20284 ADPLEX
EQU DJS.IMAGE        TO DJS.REC(21) ;* T20285 ADPLEX
EQU DJS.CONFIG       TO DJS.REC(22) ;* T22585 ADPLEX (MV)
EQU DJS.VERSION      TO DJS.REC(23) ;* T22585 ADPLEX (MV)
EQU DJS.CTN.TARE.WT  TO DJS.REC(24) ;* T23852 GHP 
EQU DJS.SKD.TARE.WT  TO DJS.REC(25) ;* T23852 GHP 
* MV1 for FOLD > 1=SKID 1/4, 2=SKID 1/2, 3=CTN/SKID 1/4, 4=CTN/SKID 1/2
EQU DJS.FOLD.SKD.MIN TO DJS.REC(26) ;* T25620 GHP (MV1)
EQU DJS.FOLD.SKD.MAX TO DJS.REC(27) ;* T25620 GHP (MV1)
EQU DJS.FOLD.MAX.CTN TO DJS.REC(28) ;* T25620 GHP (MV1)
EQU DJS.FOLD.LAY.QTY TO DJS.REC(29) ;* T25620 GHP (MV1)
EQU DJS.FOLD.PATTERN TO DJS.REC(30) ;* T25620 GHP (MV1)
EQU DJS.FOLD.TURNS   TO DJS.REC(31) ;* T25620 GHP (MV1)
EQU DJS.FOLD.STACKS  TO DJS.REC(32) ;* T25620 GHP (MV1)
EQU DJS.SKD.BOX.MIN  TO DJS.REC(33) ;* ADPLEX T28623
EQU DJS.SKD.BOX.MAX  TO DJS.REC(34) ;* ADPLEX T28623
EQU DJS.COMMENT      TO DJS.REC(35) ;* ADPLEX T28623
