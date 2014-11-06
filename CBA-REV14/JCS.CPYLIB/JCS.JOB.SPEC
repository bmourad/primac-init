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
   DJS.REC.LEN = 20
*
   DIM DJS.REC(20)
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
