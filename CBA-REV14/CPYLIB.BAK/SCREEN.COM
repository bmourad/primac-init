******************************
*  SCREEN.COMMON DEFINITION  *
******************************
      DIM S$DATA(60)
*
      DIM S$GEN(5)
      EQU S$SCR             TO S$GEN(1)     ;* Screen number
      EQU S$NAME            TO S$GEN(2)     ;* Screen name
      EQU S$FLD             TO S$GEN(3)     ;* Field ref number
      EQU S$VAL             TO S$GEN(4)     ;* Field value #
      EQU S$CNT             TO S$GEN(5)     ;* Multi-line count
*
      DIM S$EDIT(45)
      EQU S$TYP             TO S$EDIT(1)    ;* Data type
      EQU S$X               TO S$EDIT(2)    ;* Column
      EQU S$Y               TO S$EDIT(3)    ;* Row
      EQU S$MINL            TO S$EDIT(4)    ;* Minimum length
      EQU S$MAXL            TO S$EDIT(5)    ;* Maximum length
      EQU S$MINV            TO S$EDIT(6)    ;* Minimum value
      EQU S$MAXV            TO S$EDIT(7)    ;* Maximum value
      EQU S$O.R             TO S$EDIT(8)    ;* Optional/Required
      EQU S$DEFAULT         TO S$EDIT(9)    ;* Default value
      EQU S$JUSTIFY         TO S$EDIT(10)   ;* Justification
      EQU S$SCALER          TO S$EDIT(11)   ;* Scaling factor
      EQU S$PATRN           TO S$EDIT(12)   ;* Pattern match
      EQU S$EMODE           TO S$EDIT(13)   ;* Error mode
      EQU S$EX              TO S$EDIT(14)   ;* Error msg column
      EQU S$EY              TO S$EDIT(15)   ;* Error msg row
      EQU S$MASK            TO S$EDIT(16)   ;* Input mask
      EQU S$DVALUE          TO S$EDIT(17)   ;* Input value (ext)
      EQU S$VALUE           TO S$EDIT(18)   ;* Input value(int)
      EQU S$PMSG            TO S$EDIT(19)   ;* Prompt message
      EQU S$BLANK           TO S$EDIT(20)   ;* Blank flag
      EQU S$REVERSE         TO S$EDIT(21)   ;* Reverse video
      EQU S$HMSG            TO S$EDIT(22)   ;* Help message
      EQU S$VALDAT          TO S$EDIT(23)   ;* Validation (AND)
      EQU S$PVALUE          TO S$EDIT(24)   ;* Prev value
      EQU S$AUTO.RTN        TO S$EDIT(25)   ;* Auto return flag
      EQU S$FILL            TO S$EDIT(26)   ;* Fill character
      EQU S$LCNT            TO S$EDIT(27)   ;* Screen line count
      EQU S$LSPC            TO S$EDIT(28)   ;* Screen line space
      EQU S$LSCR            TO S$EDIT(29)   ;* 
      EQU S$VALDAT.FILE     TO S$EDIT(30)   ;* Validation file
      EQU S$VALDAT.CODE     TO S$EDIT(31)   ;* Validation code
      EQU S$VALDAT.ITEM     TO S$EDIT(32)   ;* Validation item
      EQU S$VALDATION       TO S$EDIT(33)   ;* Validation type
      EQU S$PREFIX          TO S$EDIT(34)   ;* Validation prefix
      EQU S$STOP            TO S$EDIT(35)   ;* Stop flag
      EQU S$ETX.OFF         TO S$EDIT(36)   ;* Video-off code
      EQU S$ETX.VALS        TO S$EDIT(37)   ;* Video values
      EQU S$ORVALDAT        TO S$EDIT(38)   ;* Validation (OR)
      EQU S$INVALCHR        TO S$EDIT(39)   ;* Invalid characters
      EQU S$EDITLEN         TO S$EDIT(40)   ;* Edit Length
      EQU S$SCRN.PC         TO S$EDIT(41)   ;* PC screen name
      EQU S$GUIFORM         TO S$EDIT(42)   ;* GUI Form Number
      EQU S$FLDTYPE         TO S$EDIT(43)   ;* Field Type
      EQU S$EXVALDAT        TO S$EDIT(44)   ;* Form exit flag
      EQU S$PCNAME          TO S$EDIT(45)   ;* SAME AS BLD.PCNAME
*
      DIM S$FORMAT(10)
      DIM S$FILE(10)
*
      DIM S$CLEAR(10)
      DIM S$FVAR(10)
