* Modified 12/04/2002 by Abdullah Jibaly, Vercom Software
**************************************************************************
* Redback RBO server api definitions.
*
* (c) Copyright 2000 Informix Software Corp. - All Rights Reserved
* This is unpublished proprietary source code of Informix Software Corp.
* The copyright notice above does not provide evidence of any actual or intended
* publication of such source code.
*
**************************************************************************
*
* RBO api function definitions. All functions are called
* using a syntax like:
*
*   status = RBO.apiName(val [, val]...)
*
* A status of RBO.NO.ERROR indicates no error, any other value
* represents an error condition.
*
**************************************************************************
*
*DEFFUN RBO.createObject(CLASS$NAME, OBJECT$HANDLE)
*DEFFUN RBO.callMethod(OBJECT$HANDLE, METHOD$NAME)
*DEFFUN RBO.getProperty(OBJECT$HANDLE, PROP$NAME, PROP$DATA)
*DEFFUN RBO.setProperty(OBJECT$HANDLE, PROP$NAME, PROP$DATA)
*DEFFUN RBO.setError(MSG$TEXT)
*DEFFUN RBO.setDBVals(NAME$LIST, VAL$LIST)
*DEFFUN RBO.getDBVals(NAME$LIST, VAL$LIST)
DEFFUN RBO.getProperty(HANDLE,PROPNAME,PROPVAL) CALLING 'DEBUG_GET_PROPERTY'
DEFFUN RBO.setProperty(HANDLE,PROPNAME,PROPVAL) CALLING 'DEBUG_SET_PROPERTY'
*
* Error status codes. Errors are all negative numbers.
*
EQU RBO.NO.ERROR        TO 0   ;* No error.
EQU RBO.BAD.ARRAYSIZE   TO -1  ;* Invalid parameter array size!
EQU RBO.BAD.OBJECT      TO -2  ;* Object not found
EQU RBO.BAD.CLASS       TO -3  ;* Class not found
EQU RBO.BAD.PROPNAME    TO -4  ;* Property not found
EQU RBO.BAD.PERMS       TO -5  ;* Permission denied (for property access)
*
EQU RBO.ERROR           TO -99 ;* Generic error code.
*
*end-of-item
