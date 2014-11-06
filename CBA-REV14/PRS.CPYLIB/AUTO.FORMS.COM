****************************************
*FILE = DEVBP                          *
*PROG = FORMS.SETUP.MAINT              *
*ID   = COMMON VARIABLES               *
****************************************
      DIM COM.REC(25)
      EQU FORM.ID TO COM.REC(1);* Form name
      EQU OPTION  TO COM.REC(2);* (H)eader or (D)etail
      EQU ERRFLG  TO COM.REC(3);* Error flag, cannot file
      EQU REF.NO  TO COM.REC(4);* Reference number
      EQU H.REF   TO COM.REC(5);* (MV1) AFH.H.REF or AFH.D.REF
      EQU H.ATT   TO COM.REC(6);* (MV1) AFH.H.ATT or AFH.D.ATT
      EQU P.D     TO COM.REC(7);* (MV1) AFD.DESC
      EQU P.F.T   TO COM.REC(8);* (MV1) AFD.OPT
      EQU P.F.Y   TO COM.REC(9);* (MV1) AFD.F.Y
      EQU P.F.X   TO COM.REC(10);* (MV1) AFD.F.X
      EQU H.SEQ   TO COM.REC(11);* (MV1) AFH.H.SEQ or AFH.D.SEQ
      EQU P.H.Y   TO COM.REC(12);* (MV1) AFD.S.Y
      EQU P.H.X   TO COM.REC(13);* (MV1) AFD.S.X
      EQU P.D.T   TO COM.REC(14);* (MV1) AFD.TYP
      EQU P.D.Y   TO COM.REC(15);* (MV1) AFD.Y
      EQU P.D.X   TO COM.REC(16);* (MV1) AFD.X
      EQU H.S.Y   TO COM.REC(17);* (MV2) AFH.HS.Y or AFH.DS.Y
      EQU H.S.X   TO COM.REC(18);* (SMV2) AFH.HS.X or AFH.DS.X
      EQU H.S.L   TO COM.REC(19);* (SMV2) AFH.HS.LEN or AFH.DS.LEN
      EQU H.S.R   TO COM.REC(20);* (SMV2) AFH.HS.REF or AFH.DS.REF
      EQU H.F.Y   TO COM.REC(21);* (MV3) AFH.HF.Y or AFH.DF.Y
      EQU H.F.X   TO COM.REC(22);* (SMV3) AFH.HF.X or AFH.DF.X
      EQU H.F.L   TO COM.REC(23);* (SMV3) AFH.HF.LEN or AFH.DF.LEN
      EQU H.F.R   TO COM.REC(24);* (SMV3) AFH.HF.REF or AFH.DF.REF
