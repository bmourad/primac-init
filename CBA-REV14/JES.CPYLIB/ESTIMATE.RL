*************************************************************
* REVISION  - [08.1]                                        *
* DESC = This file contains estimate roll label information *
* KEY = CONO:EST#                                           *
* Increased Dim for New Rev10 RES (International)
*************************************************************
*
      DIM EST.RL.REC(100)
      EST.RL.REC.SIZE = 100
*
      EQU EST.RL.JOB.TYPE               TO EST.RL.REC(1)
      EQU EST.RL.PREV.EST               TO EST.RL.REC(2)
      EQU EST.RL.TYPE.CHG               TO EST.RL.REC(3)
      EQU EST.RL.CPY.DIR                TO EST.RL.REC(4)
      EQU EST.RL.DIM.AC                 TO EST.RL.REC(5)
      EQU EST.RL.DIM.AR                 TO EST.RL.REC(6)
      EQU EST.RL.PRESS.CCTR             TO EST.RL.REC(7)
      EQU EST.RL.DIE                    TO EST.RL.REC(8)
      EQU EST.RL.DIE.REPEAT             TO EST.RL.REC(9)
      EQU EST.RL.DIE.NO.ACR             TO EST.RL.REC(10)
      EQU EST.RL.DIE.NO.ARD             TO EST.RL.REC(11)
      EQU EST.RL.DIE.CYL.ID             TO EST.RL.REC(12)
      EQU EST.RL.DIE.QTY                TO EST.RL.REC(13)
      EQU EST.RL.BACKPRINT              TO EST.RL.REC(14)
      EQU EST.RL.VARNISH.TYPE           TO EST.RL.REC(15)
      EQU EST.RL.FULL.SPOT              TO EST.RL.REC(16)
      EQU EST.RL.COLOR.MATCH            TO EST.RL.REC(17)
      EQU EST.RL.STD.MATCH              TO EST.RL.REC(18)
      EQU EST.RL.PREM.MATCH             TO EST.RL.REC(19)
      EQU EST.RL.KINDS                  TO EST.RL.REC(20)
      EQU EST.RL.COPY.CHANGES           TO EST.RL.REC(21)
      EQU EST.RL.COLOR.CHANGES          TO EST.RL.REC(22)
      EQU EST.RL.VERSION.CHANGES        TO EST.RL.REC(23)
      EQU EST.RL.LINEAL.SLIT            TO EST.RL.REC(24)
      EQU EST.RL.NO.SLITS               TO EST.RL.REC(25)
      EQU EST.RL.ROLLS                  TO EST.RL.REC(26)
      EQU EST.RL.NO.PER.ROLL            TO EST.RL.REC(27)
      EQU EST.RL.CORE                   TO EST.RL.REC(28)
      EQU EST.RL.REG.HDUTY              TO EST.RL.REC(29)
      EQU EST.RL.CORE.SIZE              TO EST.RL.REC(30)
      EQU EST.RL.MAX.OD                 TO EST.RL.REC(31)
      EQU EST.RL.DISP.PACK              TO EST.RL.REC(32)
      EQU EST.RL.APPLY                  TO EST.RL.REC(33)
      EQU EST.RL.CORELABELS             TO EST.RL.REC(34)
      EQU EST.RL.SHEETS                 TO EST.RL.REC(35)
      EQU EST.RL.NO.PER.PKG             TO EST.RL.REC(36)
      EQU EST.RL.FANFOLD                TO EST.RL.REC(37)
      EQU EST.RL.NO.PER.BATCH           TO EST.RL.REC(38)
      EQU EST.RL.NO.PER.FOLD            TO EST.RL.REC(39)
      EQU EST.RL.SHRK.WRP               TO EST.RL.REC(40)
      EQU EST.RL.PKG.LABELS             TO EST.RL.REC(41)
      EQU EST.RL.CYL.PRICE              TO EST.RL.REC(42)
      EQU EST.RL.ORD.STK                TO EST.RL.REC(43);* (Y/N)
      EQU EST.RL.ORD.REQ                TO EST.RL.REC(44)
      EQU EST.RL.PAP.MSI                TO EST.RL.REC(45)
      EQU EST.RL.PAP.PRICE              TO EST.RL.REC(46)
      EQU EST.RL.LAM                    TO EST.RL.REC(47);* (Y/N)
      EQU EST.RL.LAM.USAGE              TO EST.RL.REC(48)
      EQU EST.RL.LAM.PROD               TO EST.RL.REC(49)
      EQU EST.RL.LAM.INV                TO EST.RL.REC(50)
      EQU EST.RL.LAM.WIDTH              TO EST.RL.REC(51)
      EQU EST.RL.LAM.MSI                TO EST.RL.REC(52)
      EQU EST.RL.LAM.PRICE              TO EST.RL.REC(53)
      EQU EST.RL.LAM.DESC               TO EST.RL.REC(54)
      EQU EST.RL.MR.DIE                 TO EST.RL.REC(55)
      EQU EST.RL.MR.PREMIUM             TO EST.RL.REC(56)
      EQU EST.RL.MR.STANDARD            TO EST.RL.REC(57)
      EQU EST.RL.MR.BACKPRINT           TO EST.RL.REC(58)
      EQU EST.RL.MR.WEBS                TO EST.RL.REC(59)
      EQU EST.RL.MR.LIN.SLITS           TO EST.RL.REC(60)
      EQU EST.RL.MR.COPIES              TO EST.RL.REC(61)
      EQU EST.RL.MR.COLORS              TO EST.RL.REC(62)
      EQU EST.RL.MR.VERSIONS            TO EST.RL.REC(63)
      EQU EST.RL.TOTAL.PC               TO EST.RL.REC(64)
      EQU EST.RL.EXCESS.PAP.MSI         TO EST.RL.REC(65);* STDREG
      EQU EST.RL.EXCESS.LAM.MSI         TO EST.RL.REC(66);* STDREG
      EQU EST.RL.LAM.PRWIDTH            TO EST.RL.REC(67);* Press width STDREG
      EQU EST.RL.LAM.RLWIDTH            TO EST.RL.REC(68);* Roll width STDREG
      EQU EST.RL.SP.AROUND              TO EST.RL.REC(69);* Space Around STDREG
      EQU EST.RL.SP.ACROSS              TO EST.RL.REC(70);* Space Across STDREG
      EQU EST.RL.SZ.AROUND              TO EST.RL.REC(71);* Size Around STDREG
      EQU EST.RL.SZ.ACROSS              TO EST.RL.REC(72);* Size Across STDREG
      EQU EST.RL.DIE.LOC                   TO EST.RL.REC(73);* Die Location STDREG
      EQU EST.RL.DIE.CMTS                  TO EST.RL.REC(74);* Die Comments STDREG
      EQU EST.RL.SLS.REGION                TO EST.RL.REC(75);* Sales Region STDREG
      EQU EST.RL.NO.PER.SHRK.WRP        TO EST.RL.REC(76) ;* STDREG
      EQU EST.RL.SHAPE                  TO EST.RL.REC(77) ;* CCL
*
* NEXT THREE ARE PRESS COUNT ADDED FOR TASK 20222 MEYERS 07/10/96
      EQU EST.RL.MR.COPIES.PC           TO EST.RL.REC(78) ;* MEYERS T20222
      EQU EST.RL.MR.COLORS.PC           TO EST.RL.REC(79) ;* MEYERS T20222
      EQU EST.RL.MR.VERSIONS.PC         TO EST.RL.REC(80) ;* MEYERS T20222
