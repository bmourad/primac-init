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
** ************************ **
** Object Class Definitions **
** ************************ **
EQU CBCLASS TO "cbclass"
EQU CEOCLASS TO "ceoclass"
EQU CLIPBOARDCLASS TO "clipboardclass"
EQU COMDLGCLASS TO "comdlgclass"
EQU CONTAINERCLASS TO "containerclass"
EQU DEBUGCLASS TO "debugclass"
EQU DDECLASS TO "ddeclass"
EQU DEFAULTCLASS TO "defaultclass"
EQU DIALOGCLASS TO "dialogclass"
EQU EDITCLASS TO "editclass"
EQU FONTCLASS TO "fontclass"
EQU FORMCLASS TO "formclass"
EQU GENWINCLASS TO "genwinclass"
EQU GHOSTCLASS TO "ghostclass"
EQU GRIDCLASS TO "gridclass"
EQU GUICLASS TO "guiclass"
EQU HELPCLASS TO "helpclass"
EQU INTERACTIVECLASS TO "interactiveclass"
EQU LABELCLASS TO "labelclass"
EQU LISTCLASS TO "listclass"
EQU MAINWINCLASS TO "mainwinclass"
EQU MANAGERCLASS TO "managerclass"
EQU MASTERCLASS TO "masterclass"
EQU MCICLASS TO "mciclass"
EQU MDICLASS TO "mdiclass"
EQU OBJECTCLASS TO "objectclass"
EQU OPTIONLISTCLASS TO "optionlistclass"
EQU PBCLASS TO "pbclass"
EQU POPUPMENUCLASS TO "popupmenuclass"
EQU RADIOCLASS TO "radioclass"
EQU RECTCLASS TO "rectclass"
EQU SCROLLBARCLASS TO "scrollbarclass"
EQU SELECTLISTCLASS TO "selectlistclass"
EQU SEPARATORCLASS TO "separatorclass"
EQU SESSIONCLASS TO "sessionclass"
EQU SYSINFOCLASS TO "sysinfoclass"
EQU TEXTCLASS TO "textclass"
EQU TIMERCLASS TO "timerclass"
EQU TOGGLECLASS TO "toggleclass"
EQU TRANSPARENTCLASS TO "transparentclass"
*
** ********************* **
** Attribute Definitions **
** ********************* **
*
EQU ACTION TO "action"
EQU ADDCHILD.METHOD TO "addchild_method"
EQU ASCENT TO "ascent"
EQU ASPECT TO "aspect"
EQU ASSIGN.DEFAULTS TO "assign_defaults"
EQU ATTACHED.SET TO "attached_set"
EQU ATTRIBUTES TO "attributes"
EQU AUTO.RESIZE TO "auto_resize"
EQU BACK TO "back"
EQU BACKGROUND TO "background"
EQU BALLOON TO "balloon"
EQU BLOCK.MODE TO "block_mode"
EQU BORDER.COLOR TO "border_color"
EQU BORDER.STYLE TO "border_style"
EQU BORDER.WIDTH TO "border_width"
EQU BOTTOM TO "bottom"
EQU BREAK.SESSION TO "break_session"
*EQU CALL TO                      "call"
EQU CALL.DATA TO "call_data"
EQU CALLBACKS TO "callbacks"
EQU CAPTURE TO "capture"
EQU CELL.DIMENSIONS TO "cell_dimensions"
EQU CHAR.DIMENSIONS TO "char_dimensions"
EQU CHARSET TO "charset"
EQU CHECKED.SET TO "checked_set"
EQU CHILD.SET TO "child_set"
EQU CHILD.SUBMENU TO "child_submenu"
EQU CLASS.NAME TO "class_name"
EQU CLASS.SET TO "class_set"
EQU CODE TO "code"
EQU CONFIG TO "config"
EQU CONNECT TO "connect"
EQU CONNECT.STRING TO "connect_string"
EQU CONSOLE TO "console"
EQU COORDINATES TO "coordinates"
EQU COPY TO "copy"
EQU CREATE TO "create"
EQU CURSOR TO "cursor"
EQU CURSOR.POS TO "cursor_pos"
EQU CUT TO "cut"
EQU DRAG.CURSOR TO "drag_cursor"
EQU DRAG.DATA TO "drag_data"
EQU EDIT.CONFIG TO "edit_config"
*EQU EXECUTE TO                   "execute"
EQU DEBUFFER TO "debuffer"
EQU DEFAULTCLASSNAME TO "defaultclassname"
EQU DEFAULTATTRIBUTES TO "defaultattributes"
EQU DEFAULTVALUES TO "defaultvalues"
EQU DFLT.PRINTER TO "dflt_printer"
EQU DEF.BUTTON TO "def_button"
EQU DELCHILD.METHOD TO "delchild_method"
EQU DESCENT TO "descent"
EQU DIMENSIONS TO "dimensions"
EQU DIRECTION TO "direction"
EQU DISABLED TO "disabled"
EQU DISABLED.SET TO "disabled_set"
EQU DISCONNECT TO "disconnect"
EQU DOWN.GRAPHIC TO "down_graphic"
EQU DOWN.STRING TO "down_string"
EQU DRAWABLE TO "drawable"
EQU EDITABLE TO "editable"
EQU EMPHASIZED TO "emphasized"
EQU ERROR.INFO TO "error_info"
EQU ERROR.STRING TO "error_string"
EQU EVENT.SET TO "event_set"
EQU EVENT.STRING TO "event_string"
EQU EVENTS TO "events"
EQU EXIT TO "exit"
EQU EXTERNAL.LEADING TO "external_leading"
EQU FALSE.FILL TO "false_fill"
EQU FALSE.GRAPHIC TO "false_graphic"
EQU FAMILY TO "family"
EQU FILE.NAME TO "file_name"
EQU FIRST.CHAR TO "first_char"
EQU FOCUS TO "focus"
EQU FONT TO "font"
EQU FONTS TO "fonts"
EQU FOREGROUND TO "foreground"
EQU FORMAT.SET TO "format_set"
EQU FRONT TO "front"
EQU FUNCTION.NAME TO "function_name"
EQU GADGET TO "gadget"
EQU GHOST TO "ghost"
EQU GRAB.MOUSE TO "grab_mouse"
EQU GRAPHIC TO "graphic"
EQU GRID TO "grid"
EQU HIGHLIGHT TO "highlight"
EQU HIST.ACTION TO "hist_action"
EQU HIST.CONFIG TO "hist_config"
EQU HOLD.SELECT TO "hold_select"
EQU HOT.LINK TO "hot_link"
EQU ICON TO "icon"
EQU IMPRESSION TO "impression"
EQU INDENT TO "indent"
EQU INHERITANCE TO "inheritance"
EQU INITIALIZE TO "initialize"
EQU INSERT TO "insert"
EQU INTERNAL.LEADING TO "internal_leading"
EQU IS.VISIBLE TO "is_visible"
EQU ITALIC TO "italic"
EQU ITEM.SET TO "item_set"
EQU JUSTIFICATION TO "justification"
EQU KEY.BUFFER TO "key_buffer"
EQU KEYWORD TO "keyword"
EQU LAST.CHAR TO "last_char"
EQU MANAGE TO "manage"
EQU MANAGE.DATA TO "manage_data"
EQU MASTER TO "master"
EQU MAX.CHARS TO "max_chars"
EQU MAX.LINES TO "max_lines"
EQU MAXIMIZE TO "maximize"
EQU MAXIMIZEABLE TO "maximizeable"
EQU MDICLIENT TO "mdiclient"
EQU MDIFRAME TO "mdiframe"
EQU MENU.SPEC TO "menu_spec"
EQU METHOD TO "method"
EQU MINIMIZE TO "minimize"
EQU MINIMIZEABLE TO "minimizeable"
EQU MODAL TO "modal"
EQU MODIFIED TO "modified"
EQU MOUSE.POS TO "mouse_pos"
EQU MULTILINE TO "multiline"
*EQU NEXT TO                      "next"
EQU NEXT.ACTION TO "next_action"
EQU NODROP TO "nodrop"
EQU NOPREFIX TO "noprefix"
EQU NO.BUTTON TO "no_button"
EQU NUM.CHILDREN TO "num_children"
EQU NUM.ITEMS TO "num_items"
EQU OBJECT.NAME TO "object_name"
EQU ON.TOGGLE TO "on_toggle"
EQU OVER.HANG TO "over_hang"
EQU OVERWRITE TO "overwrite"
EQU PARENT TO "parent"
EQU PARENT.APP TO "parent_app"
EQU PASSWORD TO "password"
EQU PASTE TO "paste"
EQU PERIOD TO "period"
EQU PITCH TO "pitch"
EQU POINT.SIZE TO "point_size"
EQU POPUP TO "popup"
EQU POSITION TO "position"
EQU PRINTER.LIST TO "printer_list"
EQU PREV TO "prev"
EQU PROTOCOL TO "protocol"
EQU PROPAGATE TO "propagate"
*EQU READ TO                      "read"
EQU READDATA TO "readdata"
EQU RAW.STRING TO "raw_string"
EQU RECEIVE TO "receive"
EQU REPLACE TO "replace"
EQU RESIZEABLE TO "resizeable"
EQU RETURN.STRING TO "return_string"
EQU RUN TO "run"
EQU SCALE.BMP TO "scale_bmp"
EQU SCOPE TO "scope"
EQU SCREEN.PARAMS TO "screen_params"
EQU SCROLL TO "scroll"
EQU SCROLL.PAGE TO "scroll_page"
EQU SCROLL.RANGE TO "scroll_range"
EQU SCROLL.UNIT TO "scroll_unit"
EQU SECURITY TO "security"
EQU SELECT.BORDER TO "select_border"
EQU SELECT.COLOR TO "select_color"
EQU SELECT.METHOD TO "select_method"
EQU SELECT.SPEC TO "select_spec"
EQU SELECT.STYLE TO "select_style"
EQU SELECTED TO "selected"
EQU SELECTED.SET TO "selected_set"
EQU SEND TO "send"
EQU SEND.INFO TO "send_info"
EQU SEND.KEY TO "send_key"
EQU SERVICE.TOPIC TO "service_topic"
EQU SET.CHANGED.DATA TO "set_changed_data"
EQU SETDEFAULTS TO "setdefaults"
EQU SHOW TO "show"
EQU SHOW.FOCUS TO "show_focus"
EQU SMART TO "smart"
EQU SPLAT TO "splat"
EQU SPECIAL.KEY.SET TO "special_key_set"
EQU START TO "start"
EQU STATE TO "state"
EQU STRIKEOUT TO "strikeout"
*EQU STRING TO "string"
EQU STYLE TO "style"
EQU SYS.CHECKED.SET TO "sys_checked_set"
EQU SYS.COLORS TO "sys_colors"
EQU SYS.DISABLED.SET TO "sys_disabled_set"
EQU SYS.MENU.SPEC TO "sys_menu_spec"
EQU SYS.METRICS TO "sys_metrics"
EQU SYSMENU TO "sysmenu"
EQU SYSTEM.DATA TO "system_data"
EQU TILE TO "tile"
*EQU TIME TO "time"
EQU TIMEOUT TO "timeout"
EQU TITLE TO "title"
EQU TITLE.BAR TO "title_bar"
EQU TOGGLE.BORDER.WIDTH TO "toggle_border_width"
EQU TOGGLE.CHILD.SET TO "toggle_child_set"
EQU TOGGLE.SIZE TO "toggle_size"
EQU TOP TO "top"
EQU TOP.ITEM.POS TO "top_item_pos"
EQU TOPIC TO "topic"
EQU TOPMOST TO "topmost"
EQU TRUE.FILL TO "true_fill"
EQU TRUE.GRAPHIC TO "true_graphic"
EQU TYPE TO "type"
EQU UNDERLINE TO "underline"
EQU UPDATE TO "update"
EQU USER.DATA TO "user_data"
EQU WEIGHT TO "weight"
EQU WORD.WRAP TO "word_wrap"
*EQU WRITE TO                     "write"
EQU WRITEDATA TO "writedata"
EQU Z.ORDER TO "z_order"
*
*
** ************** **
** Justifications **
** ************** **
EQU TOP.LEFT TO "top_left"
EQU TOP.CENTER TO "top_center"
EQU TOP.CENTRE TO "top_center"
EQU TOP.RIGHT TO "top_right"
EQU LEFT TO "left"
EQU CENTER TO "center"
EQU CENTRE TO "center"
EQU RIGHT TO "right"
EQU BOTTOM.LEFT TO "bottom_left"
EQU BOTTOM.CENTER TO "bottom_center"
EQU BOTTOM.CENTRE TO "bottom_center"
EQU BOTTOM.RIGHT TO "bottom_right"
*
** ************* **
** Toggle styles **
** ************* **
EQU CHECK TO "check"
EQU CROSS TO "cross"
EQU DIAMOND TO "diamond"
EQU CIRCLE TO "circle"
EQU BUTTON TO "button"
EQU MSCROSS TO "mscross"
EQU MSCIRCLE TO "mscircle"
EQU BITMAP TO "bitmap"
*
** ***********************
** Common dialog styles **
** ***********************
EQU DLG.COLOR TO "dlg_color"
EQU DLG.FONT TO "dlg_font"
EQU DLG.OPEN TO "dlg_open"
EQU DLG.SAVE TO "dlg_save"
EQU DLG.PRINT TO "dlg_print"
EQU DLG.PRINT.SETUP TO "dlg_print_setup"
*
*
** ************** **
** Select methods **
** ************** **
EQU BROWSE TO "browse"
EQU MULTI TO "multi"
*
*
** ************************ **
** Character sets and pitch **
** ************************ **
EQU CHARSET.ANSI TO "charset_ansi"
EQU CHARSET.OEM TO "charset_oem"
EQU PITCH.FIXED TO "pitch_fixed"
EQU PITCH.VARIABLE TO "pitch_variable"
*
*
** *************** **
** FONT ATTRIBUTES **
** *************** **
EQU FONT.THIN TO "font_thin"
EQU FONT.EXTRALIGHT TO "font_extralight"
EQU FONT.LIGHT TO "font_light"
EQU FONT.NORMAL TO "font_normal"
EQU FONT.MEDIUM TO "font_medium"
EQU FONT.SEMIBOLD TO "font_semibold"
EQU FONT.BOLD TO "font_bold"
EQU FONT.EXTRABOLD TO "font_extrabold"
EQU FONT.HEAVY TO "font_heavy"
*
*
** *********** **
** Directions  **
** *********** **
EQU HORIZONTAL TO "horizontal"
EQU VERTICAL TO "vertical"
*
*
** ************* **
** Set Operators **
** ************* **
*EQU APPEND TO                    "append"
EQU DELETE TO "delete"
*
*
** ************ **
** Border Style **
** ************ **
EQU RAISED TO "raised"
EQU LOWERED TO "lowered"
EQU FLAT TO "flat"
EQU MSSTYLE TO "msstyle"
*
** Format types **
EQU TEXT.FORMAT TO "text_format"
EQU BITMAP.FORMAT TO "bitmap_format"
*
** *************** **
** TERM ATTRIBUTES **
** *************** **
EQU COMM TO "comm"
EQU COMMAND TO "command"
EQU SERIAL TO "serial"
EQU TE TO "te"
EQU AUTOWRAP TO "autowrap"
*
*
** ******************* **
** DEBUGGER ATTRIBUTES **
** ******************* **
EQU ADDBREAKPOINT TO "addbreakpoint"
EQU DELETEBREAKPOINT TO "deletebreakpoint"
EQU ACTIVATEBREAKPOINT TO "activatebreakpoint"
EQU DEACTIVATEBREAKPOINT TO "deactivatebreakpoint"
EQU DELETEALLBREAKPOINTS TO "deleteallbreakpoints"
EQU GETALLBREAKPOINTS TO "getallbreakpoints"
EQU EVALUATEBREAKPOINT TO "evaluatebreakpoint"
*
*
** ****** **
** Events **
** ****** **
EQU ACK TO "ack"
* EQU ACTIVATE TO                  "activate"
EQU ACTIVE TO "active"
EQU ADD.CHILD TO "add_child"
EQU ADVISE TO "advise"
*EQU BREAK TO                     "break"
* EQU CHANGE.STATE TO              "change_state"
* EQU CLOSE TO                     "close"
EQU COMDLG TO "comdlg"
EQU COM.RX TO "com_rx"
EQU DATA TO "data"
EQU DDE.ADVISE.CLIENT TO "dde_advise_client"
EQU DDE.ACK.CLIENT TO "dde_ack_client"
EQU DDE.ACK.SERVER TO "dde_ack_server"
EQU DDE.DATA.CLIENT TO "dde_data_client"
EQU DDE.DATA.SERVER TO "dde_data_server"
EQU DDE.ERROR.CLIENT TO "dde_error_client"
EQU DDE.ERROR.SERVER TO "dde_error_server"
EQU DROP TO "drop"
EQU DELETE.CHILD TO "delete_child"
EQU DESTROY TO "destroy"
EQU DOUBLE.SELECT TO "double_select"
EQU EXCEPTION TO "exception"
* EQU GAIN.FOCUS TO                "gain_focus"
EQU INACTIVE TO "inactive"
EQU KEY TO "key"
EQU KILLEVENT TO "killevent"
* EQU LOSE.FOCUS TO                "lose_focus"
* EQU LOSE.FIELD.FOCUS TO          "lose_field_focus"
EQU MENU TO "menu"
EQU MENU.INIT TO "menu_init"
EQU MOUSE.MOTION TO "mouse_motion"
EQU MOUSE.B1.DOUBLE TO "mouse_b1_double"
EQU MOUSE.B1.DOWN TO "mouse_b1_down"
EQU MOUSE.B1.UP TO "mouse_b1_up"
EQU MOUSE.B2.DOUBLE TO "mouse_b2_double"
EQU MOUSE.B2.DOWN TO "mouse_b2_down"
EQU MOUSE.B2.UP TO "mouse_b2_up"
EQU MOUSE.B3.DOUBLE TO "mouse_b3_double"
EQU MOUSE.B3.DOWN TO "mouse_b3_down"
EQU MOUSE.B3.UP TO "mouse_b3_up"
EQU MOVE TO "move"
EQU OBJECT.DESTROY TO "object_destroy"
EQU RESIZE TO "resize"
EQU SCROLL.END TO "scroll_end"
EQU SCROLL.LINE TO "scroll_line"
EQU SCROLL.MOVE TO "scroll_move"
EQU SESSION TO "session"
EQU SESSION.MESSAGE TO "session_message"
EQU SPECIAL TO "special"
EQU TEXT.LIMIT TO "text_limit"
EQU TIMER TO "timer"
* TO
*
*
** ******************************* **
** Possible mouse button positions **
** ******************************* **
EQU BUTTON.UP TO "button_up"
EQU BUTTON.DOWN TO "button_down"
*
*
** ***************************** **
** Just some fairly random stuff **
** ***************************** **
*#ifndef TOFALSE
* EQU FALSE TO                     0
*#endif
*#ifndef TOTRUE
* EQU TRUE TO                      1
*#endif
*
EQU GLOBAL.MASTER TO 1
*
* EQU DLG.ERROR TO                 1
* EQU DLG.QUESTION TO              2
* EQU DLG.INFO TO                  3
* EQU DLG.WARNING TO               4
*
EQU MENU.PUSH TO "menu_push"
EQU MENU.POP TO "menu_pop"
*
** some DDE stuff
EQU CLIENT TO "client"
EQU SERVER TO "server"
** not sure whether to use ADD or ADDSERVICE - delete later
EQU ADD TO "add"
EQU ADDSERVICE TO "addservice"
EQU DELETESERVICE TO "deleteservice"
EQU ADVSTART TO "advstart"
EQU ADVSTOP TO "advstop"
*
EQU DDE.SYNCHRONOUS TO 0
EQU DDE.ASYNCHRONOUS TO 1
EQU DDE.WARMLINK TO 0
EQU DDE.HOTLINK TO 1
*
*
** *********************** **
** Special Key Definitions **
** *********************** **
EQU FIRST.NONSPECIAL TO 32; ** space **
EQU LAST.NONSPECIAL TO 255; ** god knows **
EQU SBK.CTRLA TO 1; ** Macro Cut     ? **
EQU SBK.CTRLL TO 12; ** Lower Case    ? **
EQU SBK.CTRLT TO 20; ** Text Case     ? **
EQU SBK.CTRLU TO 21; ** Upper Case    ? **
EQU SBK.CTRLW TO 23; ** Invoke editor ? **
EQU SBK.CTRLX TO 24; ** Exit          ? **
EQU SBK.CTRLY TO 25; ** Clear to EOL  ? **
EQU SBK.CTRLZ TO 26; ** Macro Key*Paste **
*
EQU FIRST.SBK TO 518
EQU SBK.BKSPCE TO 8
EQU SBK.TAB TO 9
EQU SBK.RETURN TO 13
EQU SBK.ESCAPE TO 27
EQU SBK.DELETE TO 544
EQU SBK.PAGEUP TO 518
EQU SBK.PAGEDOWN TO 519
EQU SBK.END TO 520
EQU SBK.HOME TO 521
EQU SBK.LEFT TO 522
EQU SBK.UP TO 523
EQU SBK.RIGHT TO 524
EQU SBK.DOWN TO 525
EQU SBK.INSERT TO 526
EQU SBK.F1 TO 527
EQU SBK.F2 TO 528
EQU SBK.F3 TO 529
EQU SBK.F4 TO 530
EQU SBK.F5 TO 531
EQU SBK.F6 TO 532
EQU SBK.F7 TO 533
EQU SBK.F8 TO 534
EQU SBK.F9 TO 535
EQU SBK.F10 TO 536
EQU SBK.F11 TO 537
EQU SBK.F12 TO 538
EQU SBK.F13 TO 539
EQU SBK.F14 TO 540
EQU SBK.F15 TO 541
EQU SBK.F16 TO 542
EQU SBK.PAUSE TO 543
** NumPad keys (NumLock ON)
EQU SBK.NUMPAD0 TO 545
EQU SBK.NUMPAD1 TO 546
EQU SBK.NUMPAD2 TO 547
EQU SBK.NUMPAD3 TO 548
EQU SBK.NUMPAD4 TO 549
EQU SBK.NUMPAD5 TO 550
EQU SBK.NUMPAD6 TO 551
EQU SBK.NUMPAD7 TO 552
EQU SBK.NUMPAD8 TO 553
EQU SBK.NUMPAD9 TO 554
EQU SBK.MULTIPLY TO 555
EQU SBK.ADD TO 556
EQU SBK.SEPARATOR TO 557
EQU SBK.SUBTRACT TO 558
EQU SBK.DECIMAL TO 559
EQU SBK.DIVIDE TO 560
** toggle keys
EQU SBK.NUMLOCK TO 561
EQU SBK.SCROLL TO 562
EQU SBK.CAPITAL TO 563
** state keys
EQU SBK.SHIFT TO 564
EQU SBK.ALT TO 565
EQU SBK.CONTROL TO 566
** PrintScreen
EQU SBK.SNAPSHOT TO 567
** NumPad5 with NumLock OFF
EQU SBK.CLEAR TO 568
*
EQU LAST.SBK TO 568
*
EQU SBK.GROUP.CONTROL TO 1025
EQU SBK.GROUP.FUNCTION TO 1551
EQU SBK.GROUP.EXT.FUNCTION TO 1560
EQU SBK.GROUP.NAVIGATION TO 1542
EQU SBK.GROUP.NUMPAD TO 1569
*
*
** ****** **
** Colors **
** ****** **
BLACK = "0":VM:"0":VM:"0"
BLUE = "0":VM:"0":VM:"128"
GREEN = "0":VM:"128":VM:"0"
CYAN = "0":VM:"128":VM:"128"
RED = "128":VM:"0":VM:"0"
MAGENTA = "128":VM:"0":VM:"128"
BROWN = "128":VM:"128":VM:"0"
GRAY = "128":VM:"128":VM:"128"
GREY = "128":VM:"128":VM:"128"
LIGHT.GRAY = "192":VM:"192":VM:"192"
LIGHT.GREY = "192":VM:"192":VM:"192"
LIGHT.BLUE = "0":VM:"0":VM:"255"
LIGHT.GREEN = "0":VM:"255":VM:"0"
LIGHT.CYAN = "0":VM:"255":VM:"255"
LIGHT.RED = "255":VM:"0":VM:"0"
LIGHT.MAGENTA = "255":VM:"0":VM:"255"
YELLOW = "255":VM:"255":VM:"0"
WHITE = "255":VM:"255":VM:"255"
*
*
** ******************************* **
** Conversion of col*row to pixels **
** ******************************* **
EQU MAX.PIX.HOR TO 640
EQU MAX.PIX.VERT TO 480
EQU HOR.CHAR.CEO TO 8
EQU VERT.CHAR.CEO TO 12
*EQU HOR.CHAR.PIX TO             INT(MAX.PIX.HOR * 80) + 1
*EQU VERT.CHAR.PIX TO            INT(MAX.PIX.VERT * 25) + 4
*
*
** ********************** **
** Serial COMM parameters **
** ********************** **
EQU SERCOMM TO "sercomm"
EQU PORT TO "port"
EQU BAUDRATE TO "baudrate"
EQU PARITY TO "parity"
EQU STOPBITS TO "stopbits"
EQU DATALENGTH TO "datalength"
EQU HARDWAREFLOWCONTROL TO "hardwareflowcontrol"
EQU SOFTWAREFLOWCONTROL TO "softwareflowcontrol"
EQU XONCHAR TO "xonchar"
EQU XOFFCHAR TO "xoffchar"
EQU BREAKDURATION TO "breakduration"
EQU NONE TO "NONE"
EQU EVEN TO "EVEN"
EQU ODD TO "ODD"
EQU MARK TO "MARK"
EQU RTSCTS TO "RTS*CTS"
EQU DTRDSR TO "DTR*DSR"
EQU DTR TO "dtr"
EQU RTS TO "rts"
EQU XONXOFF TO "XON*XOFF"
*
*
** ****************** **
** SYS.METRICS values **
** ****************** **
EQU SM.TITLE.HEIGHT TO 1
EQU SM.MENU.HEIGHT TO 2
EQU SM.RESIZE.BORDER.WIDTH TO 3
EQU SM.RESIZE.BORDER.HEIGHT TO 4
EQU SM.NORESIZE.BORDER.WIDTH TO 5
EQU SM.NORESIZE.BORDER.HEIGHT TO 6
EQU SM.DIALOG.BORDER.WIDTH TO 7
EQU SM.DIALOG.BORDER.HEIGHT TO 8
EQU SM.SCREEN.WIDTH TO 9
EQU SM.SCREEN.HEIGHT TO 10
EQU SM.FULLSCREEN.CLIENT.WIDTH TO 11
EQU SM.SCROLL.BAR.WIDTH TO 13
EQU SM.SCROLL.BAR.HEIGHT TO 14
*
*
** ****************** **
** SYS.COLORS  values **
** ****************** **
*
EQU COLOR.ACTIVEBORDER TO 1; ** Active window border.
EQU COLOR.ACTIVECAPTION TO 2; ** Active window title.
EQU COLOR.APPWORKSPACE TO 3; ** Background color of MDI object.
EQU COLOR.BACKGROUND TO 4; ** Desktop color.
EQU COLOR.BTNFACE TO 5; ** Face shading on pushbutton.
EQU COLOR.BTNHIGHLIGHT TO 6; ** Edge highlighting on pushbutton.
EQU COLOR.BTNSHADOW TO 7; ** Edge shading on button.
EQU COLOR.BTNTEXT TO 8; ** Text on button.
EQU COLOR.CAPTIONTEXT TO 9; ** Text on titlebar.
EQU COLOR.GRAYTEXT TO 10; ** Grayed (dimmed) text.
EQU COLOR.HIGHLIGHT TO 11; ** Background of selected item.
EQU COLOR.HIGHLIGHTTEXT TO 12; ** Text of selected item.
EQU COLOR.INACTIVEBORDER TO 13; ** Inactive window border.
EQU COLOR.INACTIVECAPTION TO 14; ** Inactive window title.
EQU COLOR.INACTIVECAPTIONTEXT TO 15; ** Inactive window title text.
EQU COLOR.MENU TO 16; ** Menu background.
EQU COLOR.MENUTEXT TO 17; ** Text in menu.
EQU COLOR.SCROLLBAR TO 18; ** Scrollbar gray area.
EQU COLOR.WINDOW TO 19; ** Window background.
EQU COLOR.WINDOWFRAME TO 20; ** Window frame.
