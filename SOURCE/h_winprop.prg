/* 
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

   Parts of this project are based upon:

    "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
    www - https://harbour.github.io/

    "Harbour Project"
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "minigui.ch"

*-----------------------------------------------------------------------------*
PROCEDURE _SetWindowProp ( xParentForm, cPropName, xValue, lDirect )
*-----------------------------------------------------------------------------*
   LOCAL cParentFormName := ""

   xParentForm := _GetFormHandle ( xParentForm, @cParentFormName )

   IF ! SetProp( xParentForm, cPropName, xValue, hb_defaultValue( lDirect, .F. ) )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " is not defined." )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _GetWindowProp ( xParentForm, cPropName, lDirect )
*-----------------------------------------------------------------------------*
   LOCAL cParentFormName := ""
   LOCAL xValue

   xParentForm := _GetFormHandle ( xParentForm, @cParentFormName )

   xValue := GetProp( xParentForm, cPropName, hb_defaultValue( lDirect, .F. ) )

   IF HB_ISNIL( xValue )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " is not defined." )
   ENDIF

RETURN xValue

*-----------------------------------------------------------------------------*
FUNCTION _RemoveWindowProp ( xParentForm, cPropName, lNoFree )
*-----------------------------------------------------------------------------*

RETURN RemoveProp( _GetFormHandle ( xParentForm ), cPropName, hb_defaultValue( lNoFree, .F. ) )

*-----------------------------------------------------------------------------*
FUNCTION _EnumWindowProps( xParentForm )
*-----------------------------------------------------------------------------*

RETURN EnumProps( _GetFormHandle ( xParentForm ) )

*-----------------------------------------------------------------------------*
STATIC FUNCTION _GetFormHandle ( xParentForm, cParentFormName )
*-----------------------------------------------------------------------------*

   IF xParentForm == NIL

      IF _HMG_BeginWindowMdiActive

         xParentForm := GetActiveMdiHandle()

      ELSE

         IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
            xParentForm := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
         ENDIF

      ENDIF

   ENDIF

   IF HB_ISSTRING( xParentForm )

      IF ! _IsWindowDefined ( xParentForm )
         MsgMiniGuiError( "Window: " + xParentForm + " is not defined." )
      ENDIF

      cParentFormName := xParentForm
      xParentForm := GetFormHandle ( cParentFormName )

   ENDIF

RETURN xParentForm
