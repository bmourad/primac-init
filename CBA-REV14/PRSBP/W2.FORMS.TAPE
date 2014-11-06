*********************************************************************
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
* REVISION    - [08.1]
* SYSTEM      - PRIMAC
* SOURCE      - PRSBP
* PROGRAM     - W2.FORMS.DISKETTE
* BT          - CHRIS MYKLEBUST, PRIMAC SYSTEMS
* DATE        - 11/12/2001
* DESCRIPTION - This program formats annual W2 data for reporting
*               to the Social Security Administration on magnetic
*               media.
* T26277 cm 11/12/2001 * W2 tapes requires a totally new format.
*ENDDOC
*********************************************************************
*COPY>PMC.CPYLIB>COMPANY
*COPY>PMC.CPYLIB>MENUS.CONTROL
*COPY>CPYLIB>CHAR
*COPY>PRS.CPYLIB>Z.REC
*COPY>PRS.CPYLIB>I.REC
*COPY>PRS.CPYLIB>ELECTRONIC.FILING.REPORT
*
* ROUTINE TO CHECK FOR TAPE AVAILABLE (UNIDATA)
*
   PROCREAD PROCDATA ELSE PROCDATA = ""
   BUFF.NO = 3
   IF NUM(PROCDATA<BUFF.NO>) THEN
      IF LEN(PROCDATA<BUFF.NO>) LT 2 THEN PROCDATA<BUFF.NO> = STR('0',2-LEN(PROCDATA<BUFF.NO>)):PROCDATA<BUFF.NO>
   END
   UDTEXECUTE "T.ATT ":PROCDATA<BUFF.NO>:" BLKSIZE 512" CAPTURING MSG
   M.CNT = DCOUNT(MSG,CHAR(254)) - 1
   W.CNT = COUNT(MSG<M.CNT>," ") + 1
   LAST.WORD = FIELD(MSG<M.CNT>," ",W.CNT)[1,4]
   IF LAST.WORD = "Fail" OR LAST.WORD = "fail" OR LAST.WORD = "erro" THEN
      PRINT "CANNOT ATTACH TAPE.UNIT ":PROCDATA<BUFF.NO>
      INPUT X;STOP
   END
   UDTEXECUTE "T.STATUS"
   CONO = PROCDATA<1>
*
*--- OPEN FILES
*
   PROMPT ""
   OPEN "S.COMPANY" TO COMPANY ELSE PRINT "CAN'T OPEN S.COMPANY FILE! HIT RETURN!":;INPUT WAIT;STOP
   OPEN "DICT","S.COMPANY" TO D.COMPANY ELSE PRINT "CAN'T OPEN DICT OF S.COMPANY! HIT RETURN!":;INPUT WAIT;STOP
   OPEN "DICT","S.EMPLOYEE" TO D.EMP ELSE PRINT "CAN'T OPEN D.EMP FILE! HIT RETURN":;INPUT WAIT;STOP
   OPEN "CONTROL" TO CONTROL ELSE PRINT "CAN'T OPEN CONTROL FILE! HIT RETURN!":;INPUT WAIT;STOP
   OPEN "W2.FORMS" TO W2.FORMS ELSE PRINT "CAN'T OPEN W2.FORMS! HIT RETURN!":;INPUT WAIT; STOP
*
*--- DEFINE CONSTANTS
*
   DIM ERRMSG(1000)
   MAT ERRMSG = ""
   CNT = 1
   ZERO = 0
   TOTALS = ''
   MATREAD COMP.REC FROM COMPANY,CONO ELSE
      PRINT "CAN'T GET INFO FOR COMPANY ":CONO:" ITEM!  HIT RETURN!"
      INPUT WAIT
      STOP
   END
   MATREAD EFR.REC FROM CONTROL, CONO:"ELECTRONIC.FILING.REPORT" ELSE
      PRINT "ELECTRONIC FILING COMPANY IMFORMATION HAS NOT BEEN SET UP!"
      INPUT WAIT
      STOP
   END
*  EOR=CHAR(13):CHAR(10)
   EOR = ''
*
*--- SET UP CODE RA - SUBMITTER RECORD
*
   CODEA='RA'
   CODEA=CODEA:EFR.EIN          'L#9' ;* Transmitter Employer ID#
   CODEA=CODEA:EFR.PIN          'L#17';* Submitter employee PIN #
   CODEA=CODEA:EFR.RESUBMIT.FLAG'L#1' ;* Resubmit flag
   CODEA=CODEA:EFR.RESUBMIT.TLCN'L#6' ;* Resubmit TLCN # from SSA
   CODEA=CODEA:'98'             'L#2' ;* In-House Program
   CODEA=CODEA:EFR.COMP.NAME    'L#57';* Company Name
   CODEA=CODEA:EFR.COMP.ADDR1   'L#22';* Location Address (suite,room,etc)
   CODEA=CODEA:EFR.COMP.ADDR2   'L#22';* Delivery Address (street, po box)
   CODEA=CODEA:EFR.COMP.CITY    'L#22';* City
   CODEA=CODEA:EFR.COMP.STATE   'L#2' ;* State
   CODEA=CODEA:EFR.COMP.ZIP     'L#9' ;* Zip Code or Zip + 4
   CODEA=CODEA:SPACE(5)         'L#5' ;* Reserved for SSA
   CODEA=CODEA:EFR.COMP.FGN.STATE   'L#23';* Foregin State/Province
   CODEA=CODEA:EFR.COMP.FGN.POSTAL  'L#15';* Foreign Postal Code
   CODEA=CODEA:EFR.COMP.COUNTRY 'L#2' ;* Country Code
   CODEA=CODEA:EFR.SUB.NAME     'L#57';* Submitter Name
   CODEA=CODEA:EFR.SUB.ADDR1    'L#22';* Location Address (suite,room,etc)
   CODEA=CODEA:EFR.SUB.ADDR2    'L#22';* Delivery Address (street, po box)
   CODEA=CODEA:EFR.SUB.CITY     'L#22';* City
   CODEA=CODEA:EFR.SUB.STATE    'L#2' ;* State
   CODEA=CODEA:EFR.SUB.ZIP      'L#9' ;* Zip Code or Zip + 4
   CODEA=CODEA:SPACE(5)         'L#5' ;*Reserved for SSA
   CODEA=CODEA:EFR.SUB.FGN.STATE'L#23';*Foreign State/Province
   CODEA=CODEA:EFR.SUB.FGN.POSTAL'L#15';*Foreign Postal Code
   CODEA=CODEA:EFR.SUB.COUNTRY  'L#2' ;*County Code or Blank
   CODEA=CODEA:EFR.CONTACT.NAME 'L#27';*Contact Name for SSA problems
   CODEA=CODEA:EFR.CONTACT.PHONE'L#15';*Contact phone #
   CODEA=CODEA:EFR.CONTACT.EXT  'L#5' ;*Contact phone extension #
   CODEA=CODEA:SPACE(3)         'L#3' ;*Reserved for SSA
   CODEA=CODEA:EFR.CONTACT.EMAIL'L#40';*Contact e-mail address
   CODEA=CODEA:SPACE(3)         'L#3' ;*Reserved for SSA
   CODEA=CODEA:EFR.CONTACT.FAX  'L#10';*Contact fax #
   CODEA=CODEA:EFR.NOTIFY.CODE  'L#1' ;*Preferred method of notification
   CODEA=CODEA:EFR.PREPARER.CODE'L#1' ;*Preparer code
   CODEA=CODEA:SPACE(12)        'L#12';* Reserved for SSA
   WRITET CODEA ELSE PRINT "CAN'T WRITE CODE RA RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
*
*--- SET UP CODE RE - EMPLOYER RECORD
*
   CODEB='RE'
   CODEB=CODEB:EFR.TAX.YEAR      'L#4' ;* Tax year
   CODEB=CODEB:EFR.AGENT.CODE    'L#1' ;* Agent indicator code
   CODEB=CODEB:EFR.AGENT.EIN     'L#9' ;* Employer/Agent EIN
   CODEB=CODEB:EFR.AGENT.FOR.EIN 'L#9' ;* Agent for EIN
   CODEB=CODEB:EFR.TERM.BUSINESS 'L#1' ;* Terminated business flag
   CODEB=CODEB:EFR.ESTABLISHMENT 'L#4' ;* Establishment #
   CODEB=CODEB:EFR.OTHER.EIN     'L#9' ;* Other EIN
   CODEB=CODEB:EFR.EMP.NAME      'L#57';* Employer name
   CODEB=CODEB:EFR.EMP.ADDR1     'L#22';* Location address
   CODEB=CODEB:EFR.EMP.ADDR2     'L#22';* Delivery address
   CODEB=CODEB:EFR.EMP.CITY      'L#22';* Employer city
   CODEB=CODEB:EFR.EMP.STATE     'L#2' ;* Employer state
   CODEB=CODEB:EFR.EMP.ZIP       'L#9' ;* Employer zip code
   CODEB=CODEB:SPACE(5)          'L#5' ;* Reserved for SSA
   CODEB=CODEB:EFR.EMP.FGN.STATE 'L#23';* Foreign state/province
   CODEB=CODEB:EFR.EMP.FGN.POSTAL'L#15';* Foreign postal code
   CODEB=CODEB:EFR.EMP.COUNTRY   'L#2' ;* Employer country code
   CODEB=CODEB:EFR.EMPLOY.CODE   'L#1' ;* Employment code
   CODEB=CODEB:EFR.TAX.JUR       'L#1' ;* Tax jurisdiction code
   CODEB=CODEB:EFR.THIRD.PARTY   'L#1' ;* 3rd party sick pay flag
   CODEB=CODEB:SPACE(291)       'L#291';* Reserved SSA
   WRITET CODEB ELSE PRINT "CAN'T WRITE CODE RE RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
*
*--- BUILD CODE RW - EMPLOYEE WAGE RECORDS
*
10 *
   READNEXT SEQ.NO ELSE 
      EOJ="Y"
*     GOSUB 200
      GOTO 800
   END
   MATREAD Z.REC FROM W2.FORMS,SEQ.NO ELSE ERRMSG(CNT)="COULD NOT READ W2.FORMS ":SEQ.NO;CNT=CNT+1;GOTO 10
   IF ZR.SSNO # '' THEN GOSUB 200
   GOTO 10
*
200 * DETAIL TAPE ROUTINE
*
   SSNO = ZR.SSNO 
   SOC.SEC = FIELD(SSNO,"-",1):FIELD(SSNO,"-",2):FIELD(SSNO,"-",3)
   EMP.CITY = TRIM(FIELD(Z.REC(5),',',1))
   EMP.STATE = TRIM(FIELD(Z.REC(5),',',2))
   EMP.ZIP = FIELD(Z.REC(6),'-',1)
   EMP.ZIP.EXT = FIELD(Z.REC(6),'-',2)
   *** RETIREMENT PLAN FLAG ***
   IF Z.REC(8) = 'X' THEN
      EMP.RET.PLAN = 1
   END ELSE
      EMP.RET.PLAN = 0
   END
   *** END OF RETIREMENT PLAN FLAG ROUTINE ***
   *** DEFERRED COMPENSATION ***
   DEF.401K = 0 ; DEF.457B = 0 ; DEF.LIFE.INS = 0
   *** BOX 12A DEDUCTIONS ***
   IF Z.REC(27) # '' THEN
      BEGIN CASE
         CASE Z.REC(27) = 'C'
            DEF.LIFE.INS = Z.REC(28)
         CASE Z.REC(27) = 'D'
            DEF.401K = Z.REC(28)
         CASE Z.REC(27) = 'G'
            DEF.457B = Z.REC(28)
      END CASE
   END
   *** END OF BOX 12A DEDUCTIONS ***
   *** BOX 12B DEDUCTIONS ***
   IF Z.REC(29) # '' THEN
      BEGIN CASE
         CASE Z.REC(29) = 'C'
            DEF.LIFE.INS = Z.REC(30)
         CASE Z.REC(29) = 'D'
            DEF.401K = Z.REC(30)
         CASE Z.REC(29) = 'G'
            DEF.457B = Z.REC(30)
      END CASE
   END
   *** END OF BOX 12B DEDUCTIONS ***
   *** BOX 12C DEDUCTIONS ***
   IF Z.REC(31) # '' THEN
      BEGIN CASE
         CASE Z.REC(31) = 'C'
            DEF.LIFE.INS = Z.REC(32)
         CASE Z.REC(31) = 'D'
            DEF.401K = Z.REC(32)
         CASE Z.REC(31) = 'G'
            DEF.457B = Z.REC(32)
      END CASE
   END
   *** END OF BOX 12C DEDUCTIONS ***
   *** BOX 12D DEDUCTIONS ***
   IF Z.REC(38) # '' THEN
      BEGIN CASE
         CASE Z.REC(38) = 'C'
            DEF.LIFE.INS = Z.REC(41)
         CASE Z.REC(38) = 'D'
            DEF.401K = Z.REC(41)
         CASE Z.REC(38) = 'G'
            DEF.457B = Z.REC(41)
      END CASE
   END
   *** END OF BOX 12D DEDUCTIONS ***
   *** END OF GET DEFFERED DEDUCTIONS ROUTINE ***
   *** TOTALS ROUTINE ***
   TOTALS<1> = TOTALS<1> + 1           ; * # of RW Records
   TOTALS<10> = TOTALS<10> + Z.REC(12) ; * Wages, Tips and Other
   TOTALS<11> = TOTALS<11> + Z.REC(11) ; * Federal Income tax withheld
   TOTALS<12> = TOTALS<12> + Z.REC(34) ; * Social Security Wages
   TOTALS<13> = TOTALS<13> + Z.REC(33) ; * Social Security Taxes
   TOTALS<14> = TOTALS<14> + Z.REC(36) ; * Medicare Wages
   TOTALS<15> = TOTALS<15> + Z.REC(22) ; * Dependant Care
   TOTALS<16> = TOTALS<16> + DEF.401K  ; * 401k Plan
   TOTALS<17> = TOTALS<17> + DEF.457B  ; * 457b Plan
   TOTALS<18> = TOTALS<18> + DEF.LIFE.INS ; * Employer Life Insurance
   *** END OF TOTALS ROUTINE ***
*
*--- BUILD RW - EMPLOYEE RECORD
*
   CODEW='RW'
   CODEW=CODEW:SOC.SEC     'L#9' ;* Social security #
   CODEW=CODEW:Z.REC(2)    'L#15';* Employee first name
   CODEW=CODEW:SPACE(15)   'L#15';* Employee middle name
   CODEW=CODEW:Z.REC(40)   'L#20';* Employee last name
   CODEW=CODEW:SPACE(4)    'L#4' ;* Employee suffix
   CODEW=CODEW:Z.REC(4)    'L#22';* Employee location address
   CODEW=CODEW:Z.REC(3)    'L#22';* Employee delivery address
   CODEW=CODEW:EMP.CITY    'L#22';* Employee city
   CODEW=CODEW:EMP.STATE   'L#2' ;* Employee state
   CODEW=CODEW:EMP.ZIP     'L#5' ;* Employee zip code
   CODEW=CODEW:EMP.ZIP.EXT 'L#4' ;* Employee zip code extension
   CODEW=CODEW:SPACE(5)    'L#5' ;* Reserved for SSA
   CODEW=CODEW:SPACE(23)   'L#23';* Foreign state/province
   CODEW=CODEW:SPACE(15)   'L#15';* Foreign postal code
   CODEW=CODEW:SPACE(2)    'L#2' ;* Foreign country code
   CODEW=CODEW:Z.REC(12)   'R%11';* Wages, tips and other compensation
   CODEW=CODEW:Z.REC(11)   'R%11';* Federal income tax withheld
   CODEW=CODEW:Z.REC(34)   'R%11';* Social Security Wages
   CODEW=CODEW:Z.REC(33)   'R%11';* Social Security tax withheld
   CODEW=CODEW:Z.REC(36)   'R%11';* Medicare wages & tips
   CODEW=CODEW:Z.REC(35)   'R%11';* Medicare tax withheld
   CODEW=CODEW:ZERO        'R%11';* Social security tips
   CODEW=CODEW:ZERO        'R%11';* Advanced earned income credit
   CODEW=CODEW:Z.REC(22)   'R%11';* Dependant care benefits
   CODEW=CODEW:DEF.401K    'R%11';* 401k contributions.
   CODEW=CODEW:ZERO        'R%11';* Deferred Comp - Section 403(b)
   CODEW=CODEW:ZERO        'R%11';* Deferred Comp - Section 408(k)(6)
   CODEW=CODEW:DEF.457B    'R%11';* Deferred Comp - Section 457(b)
   CODEW=CODEW:ZERO        'R%11';* Deferred Comp - Section 501(c)(18)(D)
   CODEW=CODEW:ZERO        'R%11';* Military Employees
   CODEW=CODEW:ZERO        'R%11';* Non-qualified plan - Section 457
   CODEW=CODEW:SPACE(11)   'L#11';* Reserved for SSA
   CODEW=CODEW:ZERO        'R%11';* Non-qualified plan - Not section 457
   CODEW=CODEW:SPACE(22)   'L#22';* Reserved for SSA
   CODEW=CODEW:DEF.LIFE.INS'R%11';* Employer cost life insurance
   CODEW=CODEW:ZERO        'R%11';* Nonstatutory stock options
   CODEW=CODEW:SPACE(56)   'L#56';* Reserved for SSA
   CODEW=CODEW:ZERO        'L#1' ;* Statutory employee flag
   CODEW=CODEW:SPACE(1)    'L#1' ;* Reserved for SSA
   CODEW=CODEW:EMP.RET.PLAN'L#1' ;* Retirement plan
   CODEW=CODEW:ZERO        'L#1' ;* Third-party sick pay flag
   CODEW=CODEW:SPACE(23)   'L#23';* Reserved for SSA
   WRITET CODEW ELSE PRINT "CAN'T WRITE CODE RW RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
   RETURN
*
*---- LAST RECORD FOR COMPANY; SUBTOTALS AT BOTTOM OF PAGE
800 *
*
*--- SET UP CODE RT - TOTALS RECORD
*
   CODET='RT'
   CODET=CODET:TOTALS<1>    'R%7' ;* # of RW employee records
   CODET=CODET:TOTALS<10>   'R%15';* Total wages, tips and other comp.
   CODET=CODET:TOTALS<11>   'R%15';* Total federal income tax withheld
   CODET=CODET:TOTALS<12>   'R%15';* Total social security wages
   CODET=CODET:TOTALS<13>   'R%15';* Total social security tax withheld
   CODET=CODET:TOTALS<14>   'R%15';* Total Medicare wages and tips
   CODET=CODET:ZERO         'R%15';* Total Medicare tax withheld
   CODET=CODET:ZERO         'R%15';* Total Social security tips
   CODET=CODET:ZERO         'R%15';* Total Advanced income credit
   CODET=CODET:TOTALS<15>   'R%15';* Total Dependant care benefits
   CODET=CODET:TOTALS<16>   'R%15';* Total 401k contributions
   CODET=CODET:ZERO         'R%15';* Total Def comp. Section 403(b)
   CODET=CODET:ZERO         'R%15';* Total Def comp. Section 408(k)(6)
   CODET=CODET:TOTALS<17>   'R%15';* Total Def comp. Section 457(b)
   CODET=CODET:ZERO         'R%15';* Total Def comp. Section 501(c)(18)(D)
   CODET=CODET:ZERO         'R%15';* Total Military Employee's
   CODET=CODET:ZERO         'R%15';* Total Non-Qualified Section 457
   CODET=CODET:SPACE(15)    'L#15';* Reserved for SSA
   CODET=CODET:ZERO         'R%15';* Total Non-Qualified not Section 457
   CODET=CODET:SPACE(30)    'L#30';* Reserved for SSA
   CODET=CODET:TOTALS<18>   'R%15';* Total Employer life insurance
   CODET=CODET:ZERO         'R%15';* Total Income tax withheld/3rd party
   CODET=CODET:ZERO         'R%15';* Total for stock options
   CODET=CODET:SPACE(158)  'L#158';* Reserved for SSA
   WRITET CODET ELSE PRINT "CAN'T WRITE CODE RF RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
*
*--- SET UP CODE RF - FINAL RECORD
*
   CODET='RF'
   CODET=CODET:SPACE(5)     'L#5';* Reserved for SSA
   CODET=CODET:TOTALS<1>    'R%9';* Total # of RW records
   CODET=CODET:SPACE(496) 'L#496';* Reserved for SSA
   WRITET CODEA ELSE PRINT "CAN'T WRITE CODE RA RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
*--------------------------------------------------------------------
   IF ERRMSG(1)='' THEN
      WEOF ELSE PRINT "CAN'T WRITE EOF RECORD TO TAPE! PRESS RETURN! ":;INPUT WAIT;STOP
   END
   IF ERRMSG(1)="" THEN STOP
   FOR X=1 TO CNT
      WRITEV ERRMSG(X) ON D.EMP,"TAPE.ERRORS",X
   NEXT X
END
