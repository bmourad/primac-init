*
** SBClient Host 3GL API
** Copyright (C) Ardent Software Inc. 1998
** Copyright (C) UniData, Inc. 1996, 1997
** Copyright (C) System Builder Corporation. 1995
**
**      This software is the proprietary property and contains
**      trade secrets of Ardent Software, Inc. Any unauthorized use,
**      disclosure or duplication is strictly prohibited.
**      All rights reserved.
*
*
EQU TRUE TO 1, FALSE TO 0, NUL TO ''
ROC.FS = CHAR(251)
ROC.US = CHAR(250)
****************************************************
* Storage area for GUI sequences. Define standard  *
* names for these sequences.                       *
****************************************************
*
EQU GUI.END TO 1; * Terminator for ALL Gui sequences
EQU GUI.TEXT TO 2
EQU GUI.IGNORE TO 3
EQU OBJ.GETATTRIB TO 4
EQU OBJ.SETATTRIB TO 5
EQU OBJ.CREATE TO 6
EQU OBJ.DESTROY TO 7
EQU OBJ.EVENT TO 8
EQU OBJ.GETHANDLE TO 9
EQU GUI.TOKEN.SET TO 10
EQU GUI.LABEL TO 11
EQU GUI.INPUT TO 12
EQU GUI.WRITE TO 13
EQU GUI.LEADIN TO 14
EQU GUI.TOKEN.POP TO 15
*
*************************************************************
* Define any other GUI sequences.
*************************************************************
*
*-- Type codes (Used in SDG screen/painter)
*
*
*******************************************************
* Set or clear sequences based on COMMON variable GUI *
*******************************************************
*
GUISEQS = ''
*
GUISEQS<GUI.END> = ESC.CHAR:'_X'; *!24!R GUISEQS<GUI.END> = ''
GUISEQS<GUI.TEXT> = ESC.CHAR:'_U3'
GUISEQS<GUI.IGNORE> = ESC.CHAR:'_U6'
GUISEQS<OBJ.GETATTRIB> = ESC.CHAR:'_Ug'
GUISEQS<OBJ.SETATTRIB> = ESC.CHAR:'_Us'
GUISEQS<OBJ.CREATE> = ESC.CHAR:'_Uc'
GUISEQS<OBJ.DESTROY> = ESC.CHAR:'_Ud'
GUISEQS<OBJ.EVENT> = ESC.CHAR:'_Ue'
GUISEQS<OBJ.GETHANDLE> = ESC.CHAR:'_Uh'
GUISEQS<GUI.TOKEN.SET> = ESC.CHAR:'_Ut'
GUISEQS<GUI.LABEL> = ESC.CHAR:'_U1'
GUISEQS<GUI.INPUT> = ESC.CHAR:'_Ui'
GUISEQS<GUI.WRITE> = ESC.CHAR:'_Uw'
GUISEQS<GUI.LEADIN> = ESC.CHAR:'_U'
GUISEQS<GUI.TOKEN.POP> = ESC.CHAR:'_Up'
*
* Remote Object Call Header File
EQU ROC.CREATE TO 1
EQU ROC.DESTROY TO 2
EQU ROC.SET.ATTRIB TO 3
EQU ROC.GET.ATTRIB TO 4
EQU ROC.EVENT TO 5
EQU ROC.GET.HANDLE TO 6
EQU ROC.INIT TO 7
EQU ROC.INPUT TO 8
EQU ROC.SERVE TO 9
EQU ROC.POP TO 10
EQU ROC.GET.EVENT TO 11
*
* Token equates
EQU RTX.FS TO 1
EQU RTX.AM TO 2
EQU RTX.VM TO 3
EQU RTX.SVM TO 4
EQU RTX.US TO 5
EQU RRX.FS TO 6
EQU RRX.AM TO 7
EQU RRX.VM TO 8
EQU RRX.SVM TO 9
EQU RRX.US TO 10
EQU ROC.HND TO 11
EQU ROC.RSP TO 12
EQU ROC.LDN TO 2; * 13
EQU ASCII.TOKEN TO 1
EQU ROC.HANDLE TO 14
* sbhandlerclass Attribute names
EQU SBH.CLEANUP TO "gui_cleanup"
*
*   TOKENS = CHAR(22):VM:CHAR(30):VM:CHAR(29):VM:CHAR(28):VM:CHAR(25)
*   TOKENS = TOKENS:VM:TOKENS:VM:'$$':VM:CHAR(1)
*   TOKENS = TOKENS:VM:CHAR(2):CHAR(2)
*
TOKENS = CHAR(1):VM:CHAR(2):CHAR(2)
*
NAK = TOKENS<1,ROC.LDN>[1, 1]
*
OBJTOKENS = TOKENS<1, 1>:TOKENS<1, 2>:TOKENS<1, 3>:TOKENS<1, 4>:TOKENS<1, 5>
