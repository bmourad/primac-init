      SUBROUTINE WIN.PARAM.UC(F.MD, R.PARAM)
* Subroutine to initialise. Unidata P mode
* Compile for: UC
* Copyright (c) 1991-2000. Impact Business Systems
* Copyright (c) 1993-2000. Ardent Software, Inc
*
      ACCNAME = @WHO
      PORTNO = @USERNO
*
      TERM.WIDTH = SYSTEM(2)
*
      OPEN '','VOC' TO F.MD ELSE STOP 'CANT OPEN FILE "VOC"'
*
      R.PARAM = ''
      R.PARAM<1> = 9999999         ;* Max record size
      R.PARAM<2> = 0               ;* Prime style
      R.PARAM<3> = 0               ;* SV.LIST = 0 flag
      R.PARAM<4> = 'select\sselect\';* Extra verbs allowed in items
      R.PARAM<5> = 1               ;* Allow D type dict items
      R.PARAM<6> = 'F1 = "D]" "I]" "V]" "d]" "i]" "v]" USING DICT VOC' ;* Dictionary selection
      R.PARAM<7> = 'ID.SUPP NOPAGE COL.HDR.SUPP NI.SUPP HEADING "IGNORED"'  ;* Capture statement flags
      R.PARAM<8> = ACCNAME
      R.PARAM<9> = PORTNO
      R.PARAM<10> = TERM.WIDTH
*
      FSEL = 'WITH F1 = "F]"'
      FSEL = FSEL : ' "f]"'                      
      FSEL = FSEL : ' "DIR]" "dir]"'
      FSEL = FSEL : ' "LF]" "lf]"'  
      FSEL = FSEL : ' "LD]" "ld]"' 
      R.PARAM<11>= FSEL ;* Files Selection
*
* Packet sizes
      R.PARAM<13,1> = 512;* Max size to receive from PC
      R.PARAM<13,2> = 2048;* Max size to send to PC
*
      RETURN
*
   END
