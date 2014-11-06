*************************************************************
* REVISION  - [10.0]                                        *
* DESC = This file contains estimate roll label information *
* KEY = CONO:EST#                                           *
*************************************************************
*
      DIM ESTRL.REC(100)
      ESTRL.REC.SIZE = 100
*
      EQU ESTRL.JOB.TYPE               TO ESTRL.REC(1)
      EQU ESTRL.PREV.EST               TO ESTRL.REC(2)
      EQU ESTRL.TYPE.CHG               TO ESTRL.REC(3)
      EQU ESTRL.CPY.DIR                TO ESTRL.REC(4)
      EQU ESTRL.DIM.AC                 TO ESTRL.REC(5)
      EQU ESTRL.DIM.AR                 TO ESTRL.REC(6)
      EQU ESTRL.PRESS.CCTR             TO ESTRL.REC(7)
      EQU ESTRL.DIE                    TO ESTRL.REC(8)
      EQU ESTRL.DIE.REPEAT             TO ESTRL.REC(9)
      EQU ESTRL.DIE.NO.ACR             TO ESTRL.REC(10)
      EQU ESTRL.DIE.NO.ARD             TO ESTRL.REC(11)
      EQU ESTRL.DIE.CYL.ID             TO ESTRL.REC(12)
      EQU ESTRL.DIE.QTY                TO ESTRL.REC(13)
      EQU ESTRL.BACKPRINT              TO ESTRL.REC(14)
      EQU ESTRL.VARNISH.TYPE           TO ESTRL.REC(15)
      EQU ESTRL.DIETYPE               TO ESTRL.REC(15) ;* Die Type R or F
      EQU ESTRL.FULL.SPOT              TO ESTRL.REC(16)
      EQU ESTRL.COLOR.MATCH            TO ESTRL.REC(17)
      EQU ESTRL.STD.MATCH              TO ESTRL.REC(18)
      EQU ESTRL.PREM.MATCH             TO ESTRL.REC(19)
      EQU ESTRL.ADHESIVE               TO ESTRL.REC(19);* 1 = y/n
                                                         ;* 2 = Cols
      EQU ESTRL.KINDS                  TO ESTRL.REC(20)
      EQU ESTRL.COPY.CHANGES           TO ESTRL.REC(21)
      EQU ESTRL.PLATE.CHANGES          TO ESTRL.REC(21);* No Plate Chnges
      EQU ESTRL.COLOR.CHANGES          TO ESTRL.REC(22)
      EQU ESTRL.VERSION.CHANGES        TO ESTRL.REC(23)
      EQU ESTRL.COL.PLATE              TO ESTRL.REC(23);* Col/Plate Chnges
      EQU ESTRL.LINEAL.SLIT            TO ESTRL.REC(24)
      EQU ESTRL.NO.SLITS               TO ESTRL.REC(25)
      EQU ESTRL.ROLLS                  TO ESTRL.REC(26)
      EQU ESTRL.NO.PER.ROLL            TO ESTRL.REC(27)
      EQU ESTRL.CORE                   TO ESTRL.REC(28)
      EQU ESTRL.REG.HDUTY              TO ESTRL.REC(29)
      EQU ESTRL.CORE.SIZE              TO ESTRL.REC(30)
      EQU ESTRL.MAX.OD                 TO ESTRL.REC(31)
      EQU ESTRL.DISP.PACK              TO ESTRL.REC(32)
      EQU ESTRL.APPLY                  TO ESTRL.REC(33)
      EQU ESTRL.CORELABELS             TO ESTRL.REC(34)
      EQU ESTRL.SHEETS                 TO ESTRL.REC(35)
      EQU ESTRL.NO.PER.PKG             TO ESTRL.REC(36)
      EQU ESTRL.FANFOLD                TO ESTRL.REC(37)
      EQU ESTRL.NO.PER.BATCH           TO ESTRL.REC(38)
      EQU ESTRL.NO.PER.FOLD            TO ESTRL.REC(39)
      EQU ESTRL.SHRK.WRP               TO ESTRL.REC(40)
      EQU ESTRL.PKG.LABELS             TO ESTRL.REC(41)
      EQU ESTRL.CYL.PRICE              TO ESTRL.REC(42)
      EQU ESTRL.ORD.STK                TO ESTRL.REC(43);* (Y/N)
      EQU ESTRL.ORD.REQ                TO ESTRL.REC(44)
      EQU ESTRL.PAP.MSI                TO ESTRL.REC(45)
      EQU ESTRL.PAP.PRICE              TO ESTRL.REC(46)
      EQU ESTRL.PAP.CAL                TO ESTRL.REC(47);* Material Caliper
      EQU ESTRL.PAP.GSM                TO ESTRL.REC(48);* Material GSM
      EQU ESTRL.PAP.BAS.WHT            TO ESTRL.REC(48);* Material Basis Weight
      EQU ESTRL.DIE.SHAPE              TO ESTRL.REC(49);* Die Shape
      EQU ESTRL.GAP.CALC               TO ESTRL.REC(50);* Calculated Gap
      EQU ESTRL.FOIL.SPEED             TO ESTRL.REC(51);* Use Foiling Speeds
      EQU ESTRL.LAB.ACR                TO ESTRL.REC(52);* ADDED BY AUZZIE
      EQU ESTRL.MR.DIE                 TO ESTRL.REC(55)
      EQU ESTRL.MR.PREMIUM             TO ESTRL.REC(56)
      EQU ESTRL.MR.FDIE                TO ESTRL.REC(56)
      EQU ESTRL.MR.STANDARD            TO ESTRL.REC(57)
      EQU ESTRL.MR.BACKPRINT           TO ESTRL.REC(58)
      EQU ESTRL.MR.PLATE               TO ESTRL.REC(58)
      EQU ESTRL.MR.FOIL                TO ESTRL.REC(58)
      EQU ESTRL.MR.WEBS                TO ESTRL.REC(59)
      EQU ESTRL.MR.COL.MIX             TO ESTRL.REC(59)
      EQU ESTRL.MR.LIN.SLITS           TO ESTRL.REC(60)
      EQU ESTRL.MR.COPIES              TO ESTRL.REC(61)
      EQU ESTRL.MR.COL.CHANGE          TO ESTRL.REC(61)
      EQU ESTRL.MR.COLORS              TO ESTRL.REC(62)
      EQU ESTRL.MR.VERSIONS            TO ESTRL.REC(63)
      EQU ESTRL.MR.COL.PLATE           TO ESTRL.REC(63)
      EQU ESTRL.TOTAL.PC               TO ESTRL.REC(64)
      EQU ESTRL.MR.CSETDOWN            TO ESTRL.REC(65)
      EQU ESTRL.MR.DSETDOWN            TO ESTRL.REC(66)
      EQU ESTRL.MR.FSETDOWN            TO ESTRL.REC(67)
      EQU ESTRL.FOIL                   TO ESTRL.REC(68)
      EQU ESTRL.COL.MIX                TO ESTRL.REC(69)
      EQU ESTRL.MR.SSCREEN             TO ESTRL.REC(70)
      EQU ESTRL.MR.GRAVURE             TO ESTRL.REC(71)
      EQU ESTRL.MR.EMBOSS              TO ESTRL.REC(72)
      EQU ESTRL.MR.SSETDOWN            TO ESTRL.REC(73)
      EQU ESTRL.MR.GSETDOWN            TO ESTRL.REC(74)
      EQU ESTRL.MR.ESETDOWN            TO ESTRL.REC(75)
      EQU ESTRL.SSCREEN                TO ESTRL.REC(76) ;* 1= Y/N 2=COLS
      EQU ESTRL.GRAVURE                TO ESTRL.REC(77) ;* 1= Y/N 2=COLS
      EQU ESTRL.EMBOSS                 TO ESTRL.REC(78) ;* Y/N
      EQU ESTRL.MR.OVERLAM             TO ESTRL.REC(79)
      EQU ESTRL.MR.OSETDOWN            TO ESTRL.REC(80) ;* Laminate
      EQU ESTRL.MR.SPROCKET            TO ESTRL.REC(81) ;* AUZZIE STUFF
      EQU ESTRL.OVERLAM                TO ESTRL.REC(82) ;* Over Laminate
      EQU ESTRL.DIE.PRESS.NO           TO ESTRL.REC(83) ;*MV Press 1 or 2
      EQU ESTRL.TOTAL.LAM.PC           TO ESTRL.REC(84)
      EQU ESTRL.SPROCKET               TO ESTRL.REC(85)
      EQU ESTRL.PRINTGEARS             TO ESTRL.REC(86);* Print Gear Code
      EQU ESTRL.PRINTAR                TO ESTRL.REC(87);* Print Around No
      EQU ESTRL.ADDCHARGE              TO ESTRL.REC(88);* PLATE,DIE etc
      EQU ESTRL.NO.ROLLS               TO ESTRL.REC(89);* Calculated No of Rolls
      EQU ESTRL.NO.CTNS                TO ESTRL.REC(90);* Calculated No of Cartons
      EQU ESTRL.NO.KGS                 TO ESTRL.REC(91);* Calculated Total Weight (Kgs)
      EQU ESTRL.CALC.MAX.OD            TO ESTRL.REC(92);* Calculated Max OD in mm
      EQU ESTRL.CALC.ROLL.QTY          TO ESTRL.REC(93);* Calculated Qty/Roll
      EQU ESTRL.TOT.COLOURS            TO ESTRL.REC(94);* Calculated No of Colours
      EQU ESTRL.ACT.WIDTH              TO ESTRL.REC(95);*
      EQU ESTRL.SETS                   TO ESTRL.REC(96);* Set/Label Ratio
      EQU ESTRL.SETS.SIZES             TO ESTRL.REC(97);* Set Sizes (2x20)
      EQU ESTRL.GAP                    TO ESTRL.REC(98);* Minimum Gap
      EQU ESTRL.BUTT                   TO ESTRL.REC(99);* Butt Cut Flag
      EQU ESTRL.PP.INCLUDE             TO ESTRL.REC(100);* Include PPress in $/1000 Y/N
