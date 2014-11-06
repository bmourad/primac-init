SUBROUTINE PRINT.IOF.SPEC(IOF.SI.DEPT,IOF.SI.NO,IOF.SI.DESC)
*********************************************************************
* REVISION    - [08.0]
* Copyright 1982 by Computer Business Associates (Vercom Software, Inc.)
* SYSTEM      - PRIMAC
* SOURCE      - ICSBP
* PROGRAM     - PRINT.IOF.SPEC
* BY          - JULIANNE RIVERA, VERCOM SOFTWARE, INC
* DATE        - 03/18/92
*********************************************************************
*COPY>CPYLIB>CHAR
SPEC.CNT = DCOUNT(IOF.SI.DEPT,VM)
               FOR S = 1 TO SPEC.CNT
                  S.LINE = IOF.SI.DEPT<1,S>"L#5":" "
                  S.LINE = S.LINE:IOF.SI.NO<1,S>"L#10":" "
                  S.LINE = S.LINE:IOF.SI.DESC<1,S>"L#55"
                  PRINT S.LINE
               NEXT S
RETURN
END
