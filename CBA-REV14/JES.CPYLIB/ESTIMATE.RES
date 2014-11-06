*************************************************************
* REVISION  - [10.0]                                        *
* DESC = This file contains estimate roll label information *
* KEY = CONO:EST#                                           *
*************************************************************
*
$IFNDEF ESTIMATERES
DIM EST.RL.REC(200)
$ENDIF
EST.RL.REC.SIZE = 200
*
EQU EST.RL.JOB.TYPE          TO EST.RL.REC(1);* Job type N,RR,RC
EQU EST.RL.PREV.EST          TO EST.RL.REC(2);* Previous estimate
EQU EST.RL.TYPE.CHG          TO EST.RL.REC(3);* Job change type
EQU EST.RL.CPY.DIR           TO EST.RL.REC(4);* (SV1) Copy direction
EQU EST.RL.DIM.AC            TO EST.RL.REC(5);* Inches across
EQU EST.RL.DIM.AR            TO EST.RL.REC(6);* Inches around
EQU EST.RL.PRESS.CCTR        TO EST.RL.REC(7);* (SV2) Press cost center
EQU EST.RL.DIE               TO EST.RL.REC(8);* (SV3) Die number
EQU EST.RL.DIE.REPEAT        TO EST.RL.REC(9);* (SV3) Die repeat inches
EQU EST.RL.DIE.NO.ACR        TO EST.RL.REC(10);*(SV3) Labels across
EQU EST.RL.DIE.NO.ARD        TO EST.RL.REC(11);*(SV3) Labels around
EQU EST.RL.DIE.CYL.ID        TO EST.RL.REC(12);*(SV3) Cylinder ID
EQU EST.RL.PKG.PER.BOX       TO EST.RL.REC(13);* Sheet Packages per Box
EQU EST.RL.BACKPRINT         TO EST.RL.REC(14);*(SV2) # Colors back
EQU EST.RL.DIETYPE           TO EST.RL.REC(15);*(SV3) Die Type R,F
EQU EST.RL.FULL.SPOT         TO EST.RL.REC(16);*(SV2) Varnish F,S
EQU EST.RL.CROSS.PERF        TO EST.RL.REC(17);* Cross Perf Repeat (DataDoc)
EQU EST.RL.CROSS.PERF.FOLD   TO EST.RL.REC(18);* Cross Perf Repeat Fold (DataDoc)
EQU EST.RL.ADHESIVE          TO EST.RL.REC(19);*(SV2) # Adhesives
EQU EST.RL.KINDS             TO EST.RL.REC(20);*(SV2) # Varieties
EQU EST.RL.PLATE.CHANGES     TO EST.RL.REC(21);*(SV2) # Plate changes
EQU EST.RL.COPY.CHANGES      TO EST.RL.REC(21)
EQU EST.RL.COLOR.CHANGES     TO EST.RL.REC(22);*(SV2) # Color changes
EQU EST.RL.VERSION.CHANGES   TO EST.RL.REC(23)
EQU EST.RL.COL.PLATE         TO EST.RL.REC(23)
EQU EST.RL.LINEAL.SLIT       TO EST.RL.REC(24);* Lineal slits Y/N
EQU EST.RL.NO.SLITS          TO EST.RL.REC(25);* # Slits
EQU EST.RL.ROLLS             TO EST.RL.REC(26);* Rolls Y/N
EQU EST.RL.NO.PER.ROLL       TO EST.RL.REC(27);* Labels per roll
EQU EST.RL.CORE              TO EST.RL.REC(28);* Core Y/N
EQU EST.RL.DIE.USE.PER       TO EST.RL.REC(29);* (SV3) DIE USE PERCENT
EQU EST.RL.CORE.SIZE         TO EST.RL.REC(30);* Core diameter
EQU EST.RL.MAX.OD            TO EST.RL.REC(31);* Roll maximum diameter
EQU EST.RL.DISP.PACK         TO EST.RL.REC(32);* Display pack Y/N
EQU EST.RL.APPLY             TO EST.RL.REC(33);* Apply M,H,P
EQU EST.RL.CORELABELS        TO EST.RL.REC(34)
EQU EST.RL.SHEETS            TO EST.RL.REC(35);* Sheets Y/N
EQU EST.RL.NO.PER.PKG        TO EST.RL.REC(36);* Sheets per pack
EQU EST.RL.FANFOLD           TO EST.RL.REC(37);* Fanfold Y/N
EQU EST.RL.NO.PER.BATCH      TO EST.RL.REC(38);* Fanfold per pack
EQU EST.RL.NO.PER.FOLD       TO EST.RL.REC(39);* Quantity per fold
EQU EST.RL.SHRK.WRP          TO EST.RL.REC(40);* Shrink wrap Y/N
EQU EST.RL.PKG.LABELS        TO EST.RL.REC(41);* Label package Y/N
EQU EST.RL.871.FLAG          TO EST.RL.REC(42);*(SV3) 871 CALC FLAG (DataDoc)
EQU EST.RL.PERF.CYL.ID       TO EST.RL.REC(43);*(SV3) Perf Cyl ID (Datadoc)
EQU EST.RL.PAP.LENGTH        TO EST.RL.REC(44); * T25577 FOR LTI.
EQU EST.RL.PAP.MSI           TO EST.RL.REC(45);*(SV4,!) Paper MSI
EQU EST.RL.PAP.PRICE         TO EST.RL.REC(46);*(SV4,!) Paper unit price
EQU EST.RL.PAP.CAL           TO EST.RL.REC(47);*(SV4) Paper caliper
EQU EST.RL.PAP.GSM           TO EST.RL.REC(48);*(SV4) Paper basis wt
EQU EST.RL.DIE.SHAPE         TO EST.RL.REC(49);* Die shape
EQU EST.RL.GAP.CALC          TO EST.RL.REC(50);* Calculated gap
EQU EST.RL.FOIL.SPEED        TO EST.RL.REC(51);*(SV2) Foiling Speeds Y/N
EQU EST.RL.LAB.ACR           TO EST.RL.REC(52);* Roll labels across
EQU EST.RL.DIE.DESC          TO EST.RL.REC(53);*(SV2) (LTI) T27371
* AVAILABLE                     EST.RL.REC(54)
EQU EST.RL.MR.DIE            TO EST.RL.REC(55);*(SV2) Die makeready
EQU EST.RL.MR.FDIE           TO EST.RL.REC(56);*(SV2) Flatbed makeready
EQU EST.RL.MR.PLATE          TO EST.RL.REC(57);*(SV2) Plate makeready
EQU EST.RL.MR.FOIL           TO EST.RL.REC(58);*(SV2) Foil makeready
EQU EST.RL.MR.COL.MIX        TO EST.RL.REC(59);*(SV2) Color mix makeready
EQU EST.RL.MR.LIN.SLITS      TO EST.RL.REC(60);*(SV2) Slit makeready
EQU EST.RL.MR.COL.CHANGE     TO EST.RL.REC(61);*(SV2) Color Chng makeready
EQU EST.RL.MR.COPIES         TO EST.RL.REC(61)
EQU EST.RL.MR.COLORS         TO EST.RL.REC(62);*(SV2) Color makeready
EQU EST.RL.MR.VERSIONS       TO EST.RL.REC(63)
EQU EST.RL.MR.COL.PLATE      TO EST.RL.REC(63)
EQU EST.RL.TOTAL.PC          TO EST.RL.REC(64);*(SV4,!) Paper quantity
EQU EST.RL.MR.CSETDOWN       TO EST.RL.REC(65);*(SV2) Color setdown
EQU EST.RL.MR.DSETDOWN       TO EST.RL.REC(66);*(SV2) Die setdown
EQU EST.RL.MR.FSETDOWN       TO EST.RL.REC(67);*(SV2) Foil setdown
EQU EST.RL.FOIL              TO EST.RL.REC(68);*(SV2) Foil Y/N
EQU EST.RL.COL.MIX           TO EST.RL.REC(69);*(SV2) # Color mixes
EQU EST.RL.MR.SSCREEN        TO EST.RL.REC(70);*(SV2) Silkscreen makeready
EQU EST.RL.MR.GRAVURE        TO EST.RL.REC(71);*(SV2) Gravure makeready
EQU EST.RL.MR.EMBOSS         TO EST.RL.REC(72);*(SV2) Emboss makeready
EQU EST.RL.MR.SSETDOWN       TO EST.RL.REC(73);*(SV2) Silkscreen setdown
EQU EST.RL.MR.GSETDOWN       TO EST.RL.REC(74);*(SV2) Gravure setdown
EQU EST.RL.MR.ESETDOWN       TO EST.RL.REC(75);*(SV2) Emboss setdown
EQU EST.RL.SSCREEN           TO EST.RL.REC(76);*(SV2) # Silkscreens
EQU EST.RL.GRAVURE           TO EST.RL.REC(77);*(SV2) # Gravures
EQU EST.RL.EMBOSS            TO EST.RL.REC(78);*(SV2) Emboss Y/N
EQU EST.RL.MR.OVERLAM        TO EST.RL.REC(79);*(SV2) Laminate makeready
EQU EST.RL.MR.OSETDOWN       TO EST.RL.REC(80);*(SV2) Laminate setdown
EQU EST.RL.MR.SPROCKET       TO EST.RL.REC(81);*(SV2) Sprocket makeready
EQU EST.RL.OVERLAM           TO EST.RL.REC(82);*(SV2) Laminate Y/N
EQU EST.RL.DIE.PRESS.NO      TO EST.RL.REC(83);*(SV3) Die cost center
EQU EST.RL.TURNBAR           TO EST.RL.REC(84);*(SV2) Turnbar Flag Y/N
EQU EST.RL.SPROCKET          TO EST.RL.REC(85);*(SV2) Sprocket Y/N
EQU EST.RL.PRINTGEARS        TO EST.RL.REC(86);*(SV3) Print gear code
EQU EST.RL.PRINTAR           TO EST.RL.REC(87);*(SV3) Print around
EQU EST.RL.WEBDIV            TO EST.RL.REC(88);*(SV2) Webdiv Y/N
EQU EST.RL.NO.ROLLS          TO EST.RL.REC(89);* Calculated # rolls
EQU EST.RL.NO.CTNS           TO EST.RL.REC(90);* Calculated # cartons
EQU EST.RL.NO.KGS            TO EST.RL.REC(91);* Calculated total wt
EQU EST.RL.CALC.MAX.OD       TO EST.RL.REC(92);* Calculated maximum diam
EQU EST.RL.CALC.ROLL.QTY     TO EST.RL.REC(93);* Calculated qty per roll
EQU EST.RL.TOT.COLOURS       TO EST.RL.REC(94);* Calculated # colors
EQU EST.RL.ACT.WIDTH         TO EST.RL.REC(95);*(SV4,!) Actual paper width
EQU EST.RL.SETS              TO EST.RL.REC(96);* Set to label ratio
EQU EST.RL.OTHER             TO EST.RL.REC(97);* OTHER Y/N
EQU EST.RL.GAP               TO EST.RL.REC(98);* Minimum gap
EQU EST.RL.BUTT              TO EST.RL.REC(99);*(SV3) Butt cut flag
EQU EST.RL.PP.INCLUDE        TO EST.RL.REC(100);* Include PPress in $/1000 Y/N
EQU EST.RL.DUAL.WEB          TO EST.RL.REC(101);*(SV2) Dual Web (DataDoc)
EQU EST.RL.CORONA            TO EST.RL.REC(102);*(SV2) Corona Treatment (DataDoc)
EQU EST.RL.FOUR.COLOR        TO EST.RL.REC(103);*(SV2) Four Color Process (DataDoc)
EQU EST.RL.UV.COAT           TO EST.RL.REC(104);*(SV2) UV Coating (DataDoc)
EQU EST.RL.SHEETER           TO EST.RL.REC(105);*(SV2) Sheeter (DataDoc)
EQU EST.RL.ROLL.TO.ROLL      TO EST.RL.REC(106);*(SV2) Roll to Roll (DataDoc)
EQU EST.RL.BUTT.SPLICE        TO EST.RL.REC(107);*(SV2) Butt Splicer (DataDoc)
EQU EST.RL.TOP.CUT           TO EST.RL.REC(108);*(SV2) Top Cut (DataDoc)
EQU EST.RL.UNDER.CUT         TO EST.RL.REC(109);*(SV2) Under Cutting (DataDoc)
EQU EST.RL.SINGLE.PUN        TO EST.RL.REC(110);*(SV2) Single Punch (DataDoc)
EQU EST.RL.DBL.PUN           TO EST.RL.REC(111);*(SV2) Double Punch (DataDoc)
EQU EST.RL.PERF.STA          TO EST.RL.REC(112);*(SV2) Perf Stations (DataDoc)
EQU EST.RL.PERF.TYPE         TO EST.RL.REC(113);*(SV2) Perf Types (DataDoc)
EQU EST.RL.FACT.TABLE        TO EST.RL.REC(114);*(SV2) Factor Table (DataDoc)
EQU EST.RL.MR.BASIC          TO EST.RL.REC(115);*(SV2) Basic (DataDoc)
EQU EST.RL.MR.TURNBAR        TO EST.RL.REC(116);*(SV2) Turnbar (DataDoc)
EQU EST.RL.MR.SU.PERF.CYL    TO EST.RL.REC(117);*(SV2) Setup Perf Cylinder (DataDoc)
EQU EST.RL.MR.SU.PERF.BLD    TO EST.RL.REC(118);*(SV2) Setup Perf Blades (DataDoc)
EQU EST.RL.MR.SU.BACKCUT     TO EST.RL.REC(119);*(SV2) Setup Backcut (DataDoc)
EQU EST.RL.MR.SHEETER        TO EST.RL.REC(120);*(SV2) Sheeter Makeready (DataDoc)
EQU EST.RL.MR.DBL.PUN        TO EST.RL.REC(121);*(SV2) Double Punch Makeready (DataDoc)
EQU EST.RL.MR.CORONA         TO EST.RL.REC(122);*(SV2) Corona Makeready (DataDoc)
EQU EST.RL.MR.WEB.DIV        TO EST.RL.REC(123);*(SV2) Web Diverter Makeready (DataDoc)
EQU EST.RL.MR.SU.OTHER       TO EST.RL.REC(124);*(SV2) Setup Other Makeready (DataDoc)
EQU EST.RL.MR.SD.PERF.CYL    TO EST.RL.REC(125);*(SV2) Setdown Perf Cyl Makeready (DataDoc)
EQU EST.RL.MR.SD.BACK.CUT    TO EST.RL.REC(126);*(SV2) Setdown Backcut Makeready (DataDoc)
EQU EST.RL.MR.SD.OTHER       TO EST.RL.REC(127);*(SV2) Setdown Other Makeready (DataDoc)
EQU EST.RL.ADD.FACTOR        TO EST.RL.REC(128);*(SV2) Additional factor table (DataDoc)
EQU EST.RL.MR.PC             TO EST.RL.REC(129);*(SV4,!) MAKE READY LINEAL FEET
EQU EST.RL.PLT.CHG           TO EST.RL.REC(130);* Plate charge (LTI)
EQU EST.RL.COL.CHG           TO EST.RL.REC(131);* Color charge (LTI)
EQU EST.RL.PPTHOU            TO EST.RL.REC(132);* Price per thousand (LIT)
EQU EST.RL.CORE.THICK        TO EST.RL.REC(133);* Thickness of core
EQU EST.MR.ROLL.CHG          TO EST.RL.REC(134);* (LTI) (SV2) Roll Chg Amt
EQU EST.RL.MIN.MSI           TO EST.RL.REC(134);* Minimum MSI (Creative)
EQU EST.RL.EXCESS.PAP.MSI    TO EST.RL.REC(150); * (SRC)
EQU EST.RL.EXCESS.LAM.MSI    TO EST.RL.REC(151); * (SRC)
EQU EST.RL.LAM.PRWIDTH       TO EST.RL.REC(152); * (SRC)
EQU EST.RL.LAM.RLWIDTH       TO EST.RL.REC(153); * (SRC)
EQU EST.RL.SP.AROUND         TO EST.RL.REC(154); * (SRC)
EQU EST.RL.SP.ACROSS         TO EST.RL.REC(155); * (SRC)
EQU EST.RL.SZ.AROUND         TO EST.RL.REC(156); * (SRC)
EQU EST.RL.SZ.ACROSS         TO EST.RL.REC(157); * (SRC)
EQU EST.RL.DIE.LOC           TO EST.RL.REC(158); * (SRC)
EQU EST.RL.DIE.CMTS          TO EST.RL.REC(159); * (SRC)
EQU EST.RL.SLS.REGION        TO EST.RL.REC(160); * Mail Code (SRC)
EQU EST.RL.NO.PER.SHRK.WRP   TO EST.RL.REC(161); * (SRC)
EQU EST.RL.FACE.SLITS        TO EST.RL.REC(162); * (SRC)
EQU EST.RL.CARRIER.SLITS     TO EST.RL.REC(163); * (SRC)
EQU EST.RL.NO.WIDE           TO EST.RL.REC(164); * (SRC)
EQU EST.RL.PERF.EVERY        TO EST.RL.REC(165); * (SRC)
EQU EST.RL.MATCH.WEB         TO EST.RL.REC(166); * (SRC)
EQU EST.RL.BARC.NO.IMP       TO EST.RL.REC(167); * (SRC)
EQU EST.RL.FANFOLD.EVERY     TO EST.RL.REC(168); * (SRC)
EQU EST.RL.SHEET.LENGTH      TO EST.RL.REC(169); * (SRC)
EQU EST.RL.POLY.BAG          TO EST.RL.REC(170); * (SRC)
EQU EST.RL.FACE.PERFS        TO EST.RL.REC(171); * (SRC)
EQU EST.RL.FILE.HOLES        TO EST.RL.REC(172); * (SRC)
EQU EST.RL.CARRIER.WIDTH     TO EST.RL.REC(173); * (SRC)
EQU EST.RL.CLEAN.RELEASE     TO EST.RL.REC(174); * (SRC)
EQU EST.RL.PATTERN.ADHSV     TO EST.RL.REC(175); * (SRC)
EQU EST.RL.PRS.SENS.TAPE     TO EST.RL.REC(176); * (SRC)
EQU EST.RL.PINFEED.HOLES     TO EST.RL.REC(177); * (SRC)
EQU EST.RL.SHEET.WIDTH       TO EST.RL.REC(178); * (SRC)
EQU EST.RL.FLOOD.TINT        TO EST.RL.REC(179); * (SRC)
EQU EST.RL.VARNISH.TYPE      TO EST.RL.REC(180); * (REV9 & SRC)
EQU EST.RL.COLOR.MATCH       TO EST.RL.REC(181); * (REV9 & SRC)
EQU EST.RL.STD.MATCH         TO EST.RL.REC(182); * (REV9 & SRC)
EQU EST.RL.PREM.MATCH        TO EST.RL.REC(183); * (REV9 & SRC)
EQU EST.RL.REG.HDUTY         TO EST.RL.REC(184); * (REV9 & SRC)
EQU EST.RL.DIE.QTY           TO EST.RL.REC(185); * (REV9 & SRC)
EQU EST.RL.CYL.PRICE         TO EST.RL.REC(186); * (REV9 & SRC)
EQU EST.RL.NO.PER.CARTON     TO EST.RL.REC(187); * (SRC)
EQU EST.RL.TIERED.PER        TO EST.RL.REC(188); * (SRC)
