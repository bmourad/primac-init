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
*** Unidata ***
IF CAPT.FLG THEN
IF SQLATOR.FILE THEN
UDTEXECUTE VERB CAPTURING CAPT
END ELSE
EXECUTE VERB CAPTURING CAPT
END
END ELSE
IF SQLATOR.FILE THEN
UDTEXECUTE VERB
END ELSE
EXECUTE VERB
END
END
IF ERR.FLG THEN ERNO = @SYSTEM.RETURN.CODE
