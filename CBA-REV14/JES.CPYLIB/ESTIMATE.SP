******************************
* REVISION  - [08.0]         *
* DESC = This file contains estimate demographic and control information
*        for a single revision of an estimate.
*  ESTIMATE FILE DEFINITION  *
*     KEY = CONO:EST#        *
******************************
*
      DIM EST.REC(300)
*
      EST.REC.SIZE = 300
      EST.A9A="69-122"
      EST.A9B="90-122"
      EST.A9C="71-89,91-98,100-120"
      EST.MASTER.STATUS.ATT = 1
      EST.MASTER.JOB.ATT = 3
      EST.MASTER.ATT = 26
      EST.MASTER.SUBS.ATT = 27
*
      EQU EST.STATUS                    TO EST.REC(1)     ;* (MV1) Status
      EQU EST.STAT.DATE                 TO EST.REC(2)     ;* (MV1) Status date
      EQU EST.JOB                       TO EST.REC(3)     ;* Master Job number if applicable
*
      EQU EST.ESTIMATOR                 TO EST.REC(6)     ;* Estimator ID
      EQU EST.CUST                      TO EST.REC(7)     ;* Customer ID or P if Prospect
      EQU EST.CUST.NAME                 TO EST.REC(8)     ;* Customer name
      EQU EST.CUST.ADDR1                TO EST.REC(9)     ;* Customer address
      EQU EST.CUST.ADDR2                TO EST.REC(10)    ;* Customer address
      EQU EST.CUST.ADDR3                TO EST.REC(11)    ;* Customer address
      EQU EST.CUST.ADDR4                TO EST.REC(12)    ;* Customer address
      EQU EST.CUST.ATTN                 TO EST.REC(13)    ;* Attention of
      EQU EST.SALESMAN                  TO EST.REC(14)    ;* Salesman ID
      EQU EST.DIV                       TO EST.REC(15)    ;* Division ID
      EQU EST.TYPE                      TO EST.REC(16)    ;* Estimate type (R,C,S,N)
      EQU EST.CATG                      TO EST.REC(17)    ;* Estimate category
      EQU EST.SALES.CODE                TO EST.REC(18)    ;* Sales code
      EQU EST.DATE.ENTERED              TO EST.REC(19)    ;* Date entered
      EQU EST.DATE.REQUIRED             TO EST.REC(20)    ;* Date required
      EQU EST.MARKUP                    TO EST.REC(21)    ;* (MV) Profit mark-up
*                                                         ;*   1 = labor
*                                                         ;*   2 = stock
*                                                         ;*   3 = reg matl
*                                                         ;*   4 = O/P
*                                                         ;*   5 = shipping
*                                                         ;*   6 = misc
      EQU EST.PURGE.TYPE                TO EST.REC(22)    ;* Purge type (M)anual, (A)utomatic
*
      EQU EST.CUST.PO                   TO EST.REC(24)    ;* Customer P/O number
      EQU EST.TITLE                     TO EST.REC(25)    ;* Title ID
      EQU EST.MASTER                    TO EST.REC(26)    ;* Master estimate ID or NULL
      EQU EST.SUBS                      TO EST.REC(27)    ;* (MV) Sub-estimates or NULL
*
      EQU EST.COMMENT.TYPE              TO EST.REC(31)    ;* (MV2) I = Internal comments, "" = Description or Component number
      EQU EST.COMMENTS                  TO EST.REC(32)    ;* (MV2) Internal comments
*
      EQU EST.DESC.COMP                 TO EST.REC(35)    ;* (MV3) Description component number
      EQU EST.DESC.CD                   TO EST.REC(36)    ;* (MV3) Estimate description code
      EQU EST.DESC.HEADING              TO EST.REC(37)    ;* (MV3) Estimate description heading
      EQU EST.DESC                      TO EST.REC(38)    ;* (MV3) Estimate description
*
      EQU EST.QTY                       TO EST.REC(41)    ;* (MV4) Estimate quantities
      EQU EST.BASE.QTY                  TO EST.REC(42)    ;* Estimate base quantity
      EQU EST.PDEF.TYPE                 TO EST.REC(43)    ;* Product definition type
      EQU EST.RECALC.FLAG               TO EST.REC(44)    ;* Re-calculation flag
      EQU EST.PRICE.THOU                TO EST.REC(45)    ;* (MV4) Price per thousand
      EQU EST.BOOK.JOB                  TO EST.REC(46)    ;* (MV7) Job number for components
      EQU EST.BOOK.COMP                 TO EST.REC(47)    ;* (MV7,MSV) Component numbers
      EQU EST.BOOK.QTY                  TO EST.REC(48)    ;* Quantity booked to Job
      EQU EST.JOB.PURGE.DATE            TO EST.REC(49)    ;* Date associated job was purged
      EQU EST.COMPONENT.CNT             TO EST.REC(51)    ;* Component count
      EQU EST.DEPT                      TO EST.REC(52)    ;* (MV5) Department ID
      EQU EST.DEPT.TYPE                 TO EST.REC(53)    ;* (MV5) Department type
      EQU EST.DEPT.DESC                 TO EST.REC(54)    ;* (MV5) Department name
      EQU EST.COMP.FLAG                 TO EST.REC(55)    ;* (MV5,MSV) Component flag
      EQU EST.DEPT.COMP                 TO EST.REC(56)    ;* (MV6) Department, component, quantity ID
*
      EQU EST.DEPT.COMP.HRS             TO EST.REC(58)    ;* (MV6) Total hours
      EQU EST.DEPT.COMP.DCOST           TO EST.REC(59)    ;* (MV6) Total direct cost
      EQU EST.DEPT.COMP.COST            TO EST.REC(60)    ;* (MV6) Total cost
      EQU EST.DEPT.COMP.SALE            TO EST.REC(61)    ;* (MV6) Total sale
      EQU EST.DEPT.COMP.TSALE           TO EST.REC(62)    ;* (MV6) Total sale with overall mark-ups
      EQU EST.DEPT.COMP.VSALE           TO EST.REC(63)    ;* (MV6) Variable sale
      EQU EST.DEPT.OSP                  TO EST.REC(64)    ;* (MV8) Department, component
      EQU EST.DEPT.OSP.ID               TO EST.REC(65)    ;* (MV8,MSV) Vendor, O/P category
      EQU EST.DEPT.OSP.UM               TO EST.REC(66)    ;* (MV8,MSV) O/P unit of measure
      EQU EST.DEPT.OSP.QTY              TO EST.REC(67)    ;* (MV8,MSV,MSE) O/P quantity
      EQU EST.DEPT.OSP.PRICE            TO EST.REC(68)    ;* (MV8,MSV,MSE) O/P price
      EQU EST.PROD.DESC                 TO EST.REC(69)    ;* (MV9) Component description
      EQU EST.PROD.OS.TYPE              TO EST.REC(70)    ;* (MV9) Paper type
      EQU EST.PROD.OS.USAGE             TO EST.REC(71)    ;* (MV9,MSV) Paper usage code
      EQU EST.PROD.OS.PROD              TO EST.REC(72)    ;* (MV9,MSV) Order sheet product ID
      EQU EST.PROD.OS.DESC              TO EST.REC(73)    ;* (MV9,MSV) Paper description
      EQU EST.PROD.OS.WIDTH             TO EST.REC(74)    ;* (MV9,MSV) Order sheet width
      EQU EST.PROD.OS.LENGTH            TO EST.REC(75)    ;* (MV9,MSV) Order sheet length
      EQU EST.PROD.OS.GRAIN             TO EST.REC(76)    ;* (MV9,MSV) Order sheet grain
      EQU EST.PROD.PS.WIDTH             TO EST.REC(77)    ;* (MV9,MSV) Press sheet width
      EQU EST.PROD.PS.LENGTH            TO EST.REC(78)    ;* (MV9,MSV) Press sheet length
      EQU EST.PROD.PS.GRAIN             TO EST.REC(79)    ;* (MV9,MSV) Press sheet grain
      EQU EST.PROD.BASIS.WT             TO EST.REC(80)    ;* (MV9,MSV) Order sheet basis weight
      EQU EST.PROD.COATING              TO EST.REC(81)    ;* (MV9,MSV) Order sheet coating
      EQU EST.PROD.M.WT                 TO EST.REC(82)    ;* (MV9,MSV) Order sheet M-Weight
      EQU EST.PROD.CALIPER              TO EST.REC(83)    ;* (MV9,MSV) Order sheet caliper
      EQU EST.PROD.PLINE                TO EST.REC(84)    ;* (MV9,MSV) Product line
      EQU EST.PROD.COLOR                TO EST.REC(85)    ;* (MV9,MSV) Paper color
      EQU EST.PROD.FINISH               TO EST.REC(86)    ;* (MV9,MSV) Paper finish
      EQU EST.PROD.INV.ID               TO EST.REC(87)    ;* (MV9,MSV) Inventory product ID
      EQU EST.PROD.PSS.OSS              TO EST.REC(88)    ;* (MV9,MSV) PSS / OSS
      EQU EST.PROD.PPI                  TO EST.REC(89)    ;* (MV9,MSV) Order sheet PPI
      EQU EST.PROD.PRESS.ID             TO EST.REC(90)    ;* (MV9) Press ID
      EQU EST.PROD.MODE                 TO EST.REC(91)    ;* (MV9,MSV) Print mode
*                                                         ;* SS = Single Side
*                                                         ;* WT = Work & Turn
*                                                         ;* WR = Work & Roll
*                                                         ;* SW = Sheetwise
*                                                         ;* P  = Perfecting
      EQU EST.PROD.NO.UP                TO EST.REC(92)    ;* (MV9,MSV) Finished pieces per press sheet
      EQU EST.PROD.SIGS                 TO EST.REC(93)    ;* (MV9,MSV) Signatures
      EQU EST.PROD.FORMS                TO EST.REC(93)    ;* (MV9,MSV) Number of forms
      EQU EST.PROD.BIND.SPOIL           TO EST.REC(94)    ;* (MV9,MSV) Bindery spoilage table or %
      EQU EST.PROD.PQTY                 TO EST.REC(95)    ;* (MV9,MSV,MSE) Paper quantity
      EQU EST.PROD.PCST                 TO EST.REC(96)    ;* (MV9,MSV,MSE) Paper cost
      EQU EST.PROD.PG.CNT               TO EST.REC(97)    ;* (MV9,MSV) Page count
      EQU EST.PROD.PGS.PER.FORM         TO EST.REC(98)    ;* (MV9,MSV) Pages per form
      EQU EST.PROD.DC.CODE              TO EST.REC(99)    ;* (MV9) Press configuration code
      EQU EST.PROD.INK.ID               TO EST.REC(100)   ;* (MV9,MSV,MSE) Ink product ID
      EQU EST.PROD.INK.SIDES            TO EST.REC(101)   ;* (MV9,MSV,MSE) Number of sides
      EQU EST.PROD.INK.COVER            TO EST.REC(102)   ;* (MV9,MSV,MSE) Ink coverage %
      EQU EST.PROD.COLORS.1             TO EST.REC(103)   ;* (MV9,MSV) Colors on side 1
      EQU EST.PROD.COLORS.2             TO EST.REC(104)   ;* (MV9,MSV) Colors on side 2
      EQU EST.PROD.PRESS.SPOIL          TO EST.REC(105)   ;* (MV9,MSV) Press spoilage table or %
*
      EQU EST.PROD.INIT.MR.CNT          TO EST.REC(107)   ;* (MV9,MSV) Initial M/R count
      EQU EST.PROD.INIT.MR.IMP          TO EST.REC(108)   ;* (MV9,MSV) Initial M/R impressions
      EQU EST.PROD.SUBS.MR.CNT          TO EST.REC(109)   ;* (MV9,MSV) Subsequent M/R count
      EQU EST.PROD.SUBS.MR.IMP          TO EST.REC(110)   ;* (MV9,MSV) Subsequent M/R impressions
      EQU EST.PROD.PLATES               TO EST.REC(111)   ;* (MV9,MSV) Number of plates
      EQU EST.PROD.PASSES               TO EST.REC(112)   ;* (MV9,MSV) Number of passes per form
      EQU EST.PROD.IMP                  TO EST.REC(113)   ;* (MV9,MSV) Impressions per form
      EQU EST.PROD.MR.REQ               TO EST.REC(114)   ;* (MV9,MSV) M/R spoilage requirement
      EQU EST.PROD.PS.REQ               TO EST.REC(115)   ;* (MV9,MSV) Press spoilage per form
      EQU EST.PROD.BS.REQ               TO EST.REC(116)   ;* (MV9,MSV) Bindery spoilage requirement
      EQU EST.PROD.US.WIDTH             TO EST.REC(117)   ;* (MV9,MSV) Untrimmed signature width
      EQU EST.PROD.US.LENGTH            TO EST.REC(118)   ;* (MV9,MSV) Untrimmed signature length
      EQU EST.PROD.TS.WIDTH             TO EST.REC(119)   ;* (MV9,MSV) Trimmed signature width
      EQU EST.PROD.TS.LENGTH            TO EST.REC(120)   ;* (MV9,MSV) Trimmed signature length
      EQU EST.PROD.WEB.CNT              TO EST.REC(121)   ;* (MV9) Web count
      EQU EST.PROD.PGS.ALL.WEBS         TO EST.REC(122)   ;* (MV9) Pages all webs
*
      EQU EST.PROD.OS.UM                TO EST.REC(131)   ;* (MV9,MSV) Order stock U/M
      EQU EST.PROD.D1                   TO EST.REC(132)   ;* (MV9,MSV) Die # 1 ID
      EQU EST.PROD.D1.UP                TO EST.REC(133)   ;* (MV9,MSV) Die # 1 up
      EQU EST.PROD.D2                   TO EST.REC(134)   ;* (MV9,MSV) Die # 2 ID
      EQU EST.PROD.D2.UP                TO EST.REC(135)   ;* (MV9,MSV) Die # 2 up
      EQU EST.PROD.REGISTER             TO EST.REC(136)   ;* (MV9,MSV) Registration
      EQU EST.PROD.COMBO                TO EST.REC(137)   ;* (MV9,MSV) Combo parts
*
      EQU EST.FORMULA.VAR               TO EST.REC(220)   ;* (MV) 1 = Lbs per carton
*                                                         ;*      2 = Inches per lift
