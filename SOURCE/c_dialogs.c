/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

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

   ---------------------------------------------------------------------------*/
#define _WIN32_IE 0x0501
#ifdef __POCC__
#define _WIN32_WINNT 0x0500
#else
#define _WIN32_WINNT 0x0400
#endif
#include <mgdefs.h>
#include <commdlg.h>
#include <shlobj.h>
#include <commctrl.h>
#include "hbapiitm.h"

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

/**
 * Function: CHOOSEFONT
 * Description: Displays a font selection dialog where users can choose font name,
 *              size, style (bold/italic), color, and effects (underline, strikethrough).
 */
HB_FUNC( CHOOSEFONT )
{
   CHOOSEFONT  cf;
   LOGFONT     lf;
   long        PointSize;
   HDC         hdc;
   HWND        hwnd;

#ifdef UNICODE
   LPSTR       pStr;
   LPWSTR      pWStr = AnsiToWide( hb_parc( 1 ) );
   lstrcpy( lf.lfFaceName, pWStr );
   hb_xfree( pWStr );
#else
   lstrcpy( lf.lfFaceName, hb_parc( 1 ) );
#endif
   hwnd = GetActiveWindow();
   hdc = GetDC( hwnd );

   // Calculate font height in pixels
   lf.lfHeight = -MulDiv( hb_parnl( 2 ), GetDeviceCaps( hdc, LOGPIXELSY ), 72 );

   // Set font style: Bold or normal weight
   lf.lfWeight = hb_parl( 3 ) ? FW_BOLD : FW_NORMAL;

   // Set font style: Italic
   lf.lfItalic = ( BYTE ) hb_parl( 4 );

   // Set font style: Underline
   lf.lfUnderline = ( BYTE ) hb_parl( 6 );

   // Set font style: StrikeOut
   lf.lfStrikeOut = ( BYTE ) hb_parl( 7 );

   // Set character set or default to system charset
   lf.lfCharSet = HB_ISNIL( 8 ) ? ( BYTE ) DEFAULT_CHARSET : hmg_par_BYTE( 8 );

   // Initialize the CHOOSEFONT structure
   ZeroMemory( &cf, sizeof( cf ) );

   // Set font dialog properties
   cf.lStructSize = sizeof( CHOOSEFONT );
   cf.hwndOwner = hwnd;
   cf.hDC = ( HDC ) NULL;
   cf.lpLogFont = &lf;                    // Assign the LOGFONT structure
   cf.Flags = HB_ISNUM( 9 ) ? hb_parni( 9 ) : CF_SCREENFONTS | CF_EFFECTS | CF_INITTOLOGFONTSTRUCT;
   cf.rgbColors = hmg_par_COLORREF( 5 );  // Set initial color
   cf.nFontType = SCREEN_FONTTYPE;

   if( !ChooseFont( &cf ) )
   {
      hb_reta( 8 );                 // Return an empty array if dialog fails or is canceled
      HB_STORC( "", -1, 1 );
      HB_STORVNL( ( LONG ) 0, -1, 2 );
      HB_STORL( 0, -1, 3 );
      HB_STORL( 0, -1, 4 );
      HB_STORVNL( 0, -1, 5 );
      HB_STORL( 0, -1, 6 );
      HB_STORL( 0, -1, 7 );
      HB_STORNI( 0, -1, 8 );
      ReleaseDC( hwnd, hdc );
      return;
   }

   // Convert font height back to point size
   PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( hdc, LOGPIXELSY ) );

   // Populate return array with selected font details
   hb_reta( 8 );
#ifndef UNICODE
   HB_STORC( lf.lfFaceName, -1, 1 );
#else
   pStr = WideToAnsi( lf.lfFaceName );
   HB_STORC( pStr, -1, 1 );
   hb_xfree( pStr );
#endif
   HB_STORVNL( ( LONG ) PointSize, -1, 2 );
   HB_STORL( lf.lfWeight == FW_BOLD ? TRUE : FALSE, -1, 3 );
   HB_STORL( lf.lfItalic, -1, 4 );
   HB_STORVNL( cf.rgbColors, -1, 5 );
   HB_STORL( lf.lfUnderline, -1, 6 );
   HB_STORL( lf.lfStrikeOut, -1, 7 );
   HB_STORNI( lf.lfCharSet, -1, 8 );

   ReleaseDC( hwnd, hdc );
}

static TCHAR   s_szWinName[MAX_PATH + 1];

/**
 * Function: BrowseCallbackProc
 * Description: callback function for C_BROWSEFORFOLDER().
 * Contributed By Andy Wos.
 */
int CALLBACK BrowseCallbackProc( HWND hWnd, UINT uMsg, LPARAM lParam, LPARAM lpData )
{
   TCHAR szPath[MAX_PATH];

   switch( uMsg )
   {
      case BFFM_INITIALIZED:
         if( lpData )
         {
            SendMessage( hWnd, BFFM_SETSELECTION, TRUE, lpData );
   #ifndef UNICODE
            SetWindowText( hWnd, ( LPCSTR ) s_szWinName );
   #else
            SetWindowText( hWnd, ( LPCWSTR ) s_szWinName );
   #endif
         }
         break;

      case BFFM_VALIDATEFAILED:
         MessageBeep( MB_ICONHAND );
         return 1;

      case BFFM_SELCHANGED:
         if( lpData )
         {
            SHGetPathFromIDList( ( LPITEMIDLIST ) lParam, szPath );
            SendMessage( hWnd, BFFM_SETSTATUSTEXT, 0, ( LPARAM ) szPath );
         }
   }

   return 0;
}

/**
 * Function: C_BROWSEFORFOLDER
 * Syntax: C_BROWSEFORFOLDER([<hWnd>],[<cTitle>],[<nFlags>],[<nFolderType>],[<cInitPath>])
 * Description: Displays a folder selection dialog and returns the selected path.
 *              Supports customization for initial path, dialog title, and flags.
 */
HB_FUNC( C_BROWSEFORFOLDER )
{
   HWND           hWnd = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );
   BROWSEINFO     BrowseInfo;
   TCHAR          lpBuffer[MAX_PATH];
   LPITEMIDLIST   pidlBrowse;

#ifdef UNICODE
   LPWSTR         pW, pW2;
   LPSTR          pStr;
#endif
   if( HB_ISCHAR( 5 ) )
   {
#ifndef UNICODE
      GetWindowText( hWnd, ( LPSTR ) s_szWinName, MAX_PATH );
#else
      GetWindowText( hWnd, ( LPWSTR ) s_szWinName, MAX_PATH );
#endif
   }

   // Get special folder location to start the browse dialog
   SHGetSpecialFolderLocation( hWnd, HB_ISNIL( 4 ) ? CSIDL_DRIVES : hb_parni( 4 ), &pidlBrowse );

   // Setup browse dialog info
   BrowseInfo.hwndOwner = hWnd;
   BrowseInfo.pidlRoot = pidlBrowse;
   BrowseInfo.pszDisplayName = lpBuffer;
#ifndef UNICODE
   BrowseInfo.lpszTitle = HB_ISNIL( 2 ) ? "Select a Folder" : hb_parc( 2 );
#else
   pW = AnsiToWide( hb_parc( 2 ) );
   BrowseInfo.lpszTitle = HB_ISNIL( 2 ) ? TEXT( "Select a Folder" ) : pW;
#endif
   BrowseInfo.ulFlags = hb_parni( 3 ) | ( HB_ISCHAR( 5 ) ? BIF_STATUSTEXT | BIF_RETURNONLYFSDIRS : 0 );
   BrowseInfo.lpfn = BrowseCallbackProc;
#ifndef UNICODE
   BrowseInfo.lParam = HB_ISCHAR( 5 ) ? ( LPARAM ) ( char * ) hb_parc( 5 ) : 0;
#else
   pW2 = AnsiToWide( hb_parc( 5 ) );
   BrowseInfo.lParam = HB_ISCHAR( 5 ) ? ( LPARAM ) pW2 : 0;
#endif
   BrowseInfo.iImage = 0;

   pidlBrowse = SHBrowseForFolder( &BrowseInfo );

   // If folder selected, retrieve path
   if( pidlBrowse )
   {
      SHGetPathFromIDList( pidlBrowse, lpBuffer );
#ifndef UNICODE
      hb_retc( lpBuffer );
#else
      pStr = hb_osStrU16Decode( lpBuffer );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
   else
   {
      hb_retc( "" );
   }

   CoTaskMemFree( pidlBrowse );     // Free memory allocated by SHBrowseForFolder
#ifdef UNICODE
   hb_xfree( pW );
   hb_xfree( pW2 );
#endif
}

/**
 * Function: CHOOSECOLOR
 * Description: Displays a color selection dialog with an optional custom color palette.
 */
HB_FUNC( CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[16];
   int         i;

   // Populate custom color array or default system color
   for( i = 0; i < 16; i++ )
   {
      crCustClr[i] = ( HB_ISARRAY( 3 ) ? hmg_parv_COLORREF( 3, i + 1 ) : GetSysColor( COLOR_BTNFACE ) );
   }

   memset( &cc, 0, sizeof( cc ) );  // Zero out CHOOSECOLOR structure

   cc.lStructSize = sizeof( CHOOSECOLOR );
   cc.hwndOwner = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );
   cc.rgbResult = hmg_par_COLORREF( 2 );  // Set initial color
   cc.lpCustColors = crCustClr;           // Set custom colors
   cc.Flags = HB_ISNIL( 4 ) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hmg_par_DWORD( 4 );

   // Return selected color or -1 on cancel
   if( ChooseColor( &cc ) )
   {
      hmg_ret_COLORREF( cc.rgbResult );
   }
   else
   {
      hb_retni( -1 );
   }

   // Update custom color array if passed by reference
   if( HB_ISBYREF( 3 ) )
   {
      PHB_ITEM pArray = hb_param( 3, HB_IT_ANY );
      PHB_ITEM pSubarray = hb_itemNew( NULL );

      hb_arrayNew( pArray, 16 );

      for( i = 0; i < 16; i++ )
      {
         hb_arrayNew( pSubarray, 3 );
         hb_arraySetNL( pSubarray, 1, GetRValue( crCustClr[i] ) );
         hb_arraySetNL( pSubarray, 2, GetGValue( crCustClr[i] ) );
         hb_arraySetNL( pSubarray, 3, GetBValue( crCustClr[i] ) );

         hb_arraySet( pArray, i + 1, pSubarray );
      }

      hb_itemRelease( pSubarray );
   }
}

/**
 * Function: UNITSTOPIXELSX
 * Description: Converts horizontal DLUs to pixels based on system metrics.
 */
HB_FUNC( UNITSTOPIXELSX )
{
   int   UnitsX = hb_parni( 1 );
   DWORD dwDLU = GetDialogBaseUnits();

   hb_retni( MulDiv( UnitsX, LOWORD( dwDLU ), 4 ) );
}

/**
 * Function: UNITSTOPIXELSY
 * Description: Converts vertical DLUs to pixels based on system metrics.
 */
HB_FUNC( UNITSTOPIXELSY )
{
   int   UnitsY = hb_parni( 1 );
   DWORD dwDLU = GetDialogBaseUnits();

   hb_retni( MulDiv( UnitsY, HIWORD( dwDLU ), 8 ) );
}
