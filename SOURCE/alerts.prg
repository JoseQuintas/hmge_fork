/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2019-2025 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"
#include "i_winuser.ch"

*-----------------------------------------------------------------------------*
FUNCTION AlertYesNo ( Message, Title, RevertDefault, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21] }
   LOCAL nDefaultButton := 1

   IF ISARRAY( Title )
      aOptions := Title [2]
      Title := Title [1]
   ENDIF

   IF hb_defaultValue( RevertDefault, .F. )
      nDefaultButton := 2
   ENDIF

RETURN ( _Alert( Message, aOptions, Title, , nDefaultButton, Icon, nSize, aColors, lTopMost, bInit ) == IDOK )

*-----------------------------------------------------------------------------*
FUNCTION AlertYesNoCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21], '&' + _HMG_aABMLangButton [13] }

   IF ISARRAY( Title )
      aOptions := Title [2]
      Title := Title [1]
   ENDIF

   SWITCH _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. )

   CASE 1

      RETURN ( 1 )

   CASE 2

      RETURN ( 0 )

   END SWITCH

RETURN ( -1 )

*-----------------------------------------------------------------------------*
FUNCTION AlertRetryCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] } // P.D. July 3, 2021

   IF ISARRAY( Title )
      aOptions := Title [2]
      Title := Title [1]
   ENDIF

RETURN ( _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. ) == IDOK )

*-----------------------------------------------------------------------------*
FUNCTION AlertOkCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { _HMG_BRWLangButton [4], _HMG_BRWLangButton [3] }

   IF ISARRAY( Title )
      aOptions := Title [2]
      Title := Title [1]
   ENDIF

RETURN ( _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. ) == IDOK )

*-----------------------------------------------------------------------------*
FUNCTION AlertExclamation ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ELSEIF ISARRAY( Title )
      nWaitSec := Title [2]
      Title := Title [1]
   ENDIF

   IF Empty( lNoSound )
      PlayExclamation()
   ENDIF                 

RETURN _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [10] ), , , Icon, nSize, aColors, lTopMost, bInit )

*-----------------------------------------------------------------------------*
FUNCTION AlertInfo ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ELSEIF ISARRAY( Title )
      nWaitSec := Title [2]
      Title := Title [1]
   ENDIF

   IF Empty( lNoSound )
      PlayAsterisk()
   ENDIF

RETURN _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [11] ), ICON_INFORMATION, , Icon, nSize, aColors, lTopMost, bInit )

*-----------------------------------------------------------------------------*
FUNCTION AlertStop ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ELSEIF ISARRAY( Title )
      nWaitSec := Title [2]
      Title := Title [1]
   ENDIF

   IF Empty( lNoSound )
      PlayHand()
   ENDIF                 

RETURN _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [12] ), ICON_STOP, , Icon, nSize, aColors, lTopMost, bInit )

*-----------------------------------------------------------------------------*
STATIC FUNCTION _Alert ( cMsg, aOptions, cTitle, nType, nDefault, xIcon, nSize, aColors, lTopMost, bInit, lClosable )
*-----------------------------------------------------------------------------*
   __defaultNIL( @cMsg, "" )
   hb_default( @nDefault, 0 )

   IF ! Empty( nDefault )
      _HMG_ModalDialogReturn := nDefault
   ENDIF

   IF hb_defaultValue( lTopMost, .T. ) .AND. Empty( bInit )
      bInit := {|| This.TopMost := .T. }
   ENDIF

   IF AScan( _HMG_aFormType, 'A' ) == 0
      _HMG_MainWindowFirst := .F.
   ENDIF

RETURN HMG_Alert( cMsg, aOptions, cTitle, nType, xIcon, nSize, aColors, bInit, lClosable )
