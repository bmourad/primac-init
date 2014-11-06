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
* ROC.H
*
* General Equates
*
EQU NUL TO ''
EQU TRUE TO 1
EQU FALSE TO 0
ROC.FS = CHAR(251)
ROC.US = CHAR(250)
*
* Object.H Stuff
*
EQU GAIN.FOCUS TO "gain_focus"
EQU LOSE.FOCUS TO "lose_focus"
EQU LOSE.FIELD.FOCUS TO "lose_field_focus"
EQU ACTIVATE TO "activate"
EQU CHANGE.STATE TO "change_state"
EQU CLOSE TO "close"
*
* Equates For ROC.MAIN Calling
*
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
EQU ROC.SERVER.INPUT TO 12
*
* Equates For ROC Handler Attribute Names
*
EQU DISPLAY.DIALOG TO "display_dialog"
EQU INPUT.MENU TO "input_menu"
EQU INPUT.FIELD TO "input_field"
EQU LOAD.FORM TO "load_form"
EQU DOWNLOAD.FORM TO "download_form"
EQU GET.FORM TO "get_form"
EQU SET.FORM TO "set_form"
EQU FOCUS.FORM TO "focus_form"
EQU FORM.FILE TO "form_file"
EQU CURRENT.FORM TO "current_form"
EQU EDIT.FORM TO "edit_form"
EQU LOAD.MENU TO "load_menu"
EQU DOWNLOAD.MENU TO "download_menu"
EQU LOAD.TOOLBAR TO "load_toolbar"
EQU DOWNLOAD.TOOLBAR TO "download_toolbar"
EQU KILL.TOOLBAR TO "kill_toolbar"
EQU CLEAR.FORM TO "clear_form"
EQU UPDATE.FIELD TO "update_field"
EQU SET.FORMDATA TO "set_formdata"
EQU GET.FORMDATA TO "get_formdata"
EQU SET.FIELDATTR TO "set_fieldattr"
EQU SET.DEFAULTS TO "set_defaults"
EQU GET.FIELDATTR TO "get_fieldattr"
EQU GET.EVENT TO "get_event"
EQU EDIT.TEXT TO "edit_text"
EQU GET.EDIT.TEXT TO "get_edit_text"
EQU DOS.WRITE TO "dos_write"
EQU DOS.READ TO "dos_read"
EQU KILL.FORM TO "kill_form"
EQU KILL.MENU TO "kill_menu"
EQU MENU.EFFECT TO "menu_effect"
EQU TOOLBAR.EFFECT TO "toolbar_effect"
EQU ROC.DEFOBJ TO "roc_defobj"
EQU ROC.HANDLE TO "roc_handle"
EQU HOUR.CURSOR TO "hour_cursor"
EQU RUN.COMMAND TO "run_command"
EQU OS.RUN TO "os_run"
EQU CURR.COORDINATES TO "curr_coordinates"
EQU MOUSE.FLAG TO "mouse_flag"
EQU CALL.FUNCTION TO "call_function"
EQU DLG.ERROR TO 1
EQU DLG.QUESTION TO 2
EQU DLG.INFO TO 3
EQU DLG.WARNING TO 4
EQU SBO.READ TO "sbo_read"
EQU SBO.WRITE TO "sbo_write"
EQU FORM.CALLHELP TO "form_callhelp"
*
GUI.LABEL = CHAR(27):"_U3"
GUI.END = CHAR(27):"_X"
*
* END
*
EQU FORM.ADDOBJ TO "form_addobj"
EQU FORM.DELOBJ TO "form_delobj"
EQU ROC.FORMSTACK TO "formstack"
EQU ROC.ACTIVE.FORM TO "active_form"
EQU SMART.CURSOR TO "smart_cursor"
EQU SELECT.LIST TO "select_list"
*
*-* Stuff for SBClient COM
*
EQU SBSERVER TO "sbserver"
EQU SERVER.EVENT TO "ext_server_event"
EQU SEND.EVENT TO 0
EQU SERVER.STATE TO 1
