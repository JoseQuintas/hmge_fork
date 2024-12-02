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
    Copyright 1999-2024, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

  ---------------------------------------------------------------------------*/

// Set compatibility for Windows/Internet Explorer features at version 5.01
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );                  // Convert ANSI string to Wide string
#endif

// Function to free resources associated with graphical objects or handles
void pascal DelResource( HANDLE hResource );

// Compatibility for older Harbour versions with function translation
#ifndef HMG_LEGACY_OFF
#if !defined( __MINGW32__ ) && !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x020000 ) && ( __HARBOUR__ - 0 < 0x030200 )
HB_FUNC_TRANSLATE( HB_SETCODEPAGE, HB_CDPSELECT )  // Translate code page selection function
#endif
#endif

// MAKELONG combines two integers into a LONG value
HB_FUNC( MAKELONG )
{
   hmg_ret_LONG( MAKELONG( hb_parni( 1 ), hb_parni( 2 ) ) );   // Return combined LONG value
}

// Enables or disables scroll bars for a window
HB_FUNC( _ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

// Deletes a graphics object and releases resources
HB_FUNC( DELETEOBJECT )
{
   HANDLE   hRes = hmg_par_raw_HANDLE( 1 );

   if( hRes )
   {
      DelResource( hRes ); // Release associated resources
      hb_retl( DeleteObject( ( HGDIOBJ ) hRes ) ); // Return deletion success
   }
   else
   {
      hb_retl( HB_FALSE );                // Return false if handle is invalid
   }
}

// Destroys an image list, freeing associated resources
HB_FUNC( IMAGELIST_DESTROY )
{
   HIMAGELIST  himl = hmg_par_raw_HIMAGELIST( 1 );

   DelResource( himl );                   // Free resources related to the image list
   hb_retl( ImageList_Destroy( himl ) );  // Return success status of destruction
}

// Sets keyboard focus on a window
HB_FUNC( SETFOCUS )
{
   hmg_ret_raw_HWND( SetFocus( hmg_par_raw_HWND( 1 ) ) );
}

// Simulates Shift + Tab key press (for backwards navigation)
HB_FUNC( INSERTSHIFTTAB )
{
   keybd_event( VK_SHIFT, 0, 0, 0 );      // Press Shift key
   keybd_event( VK_TAB, 0, 0, 0 );        // Press Tab key
   keybd_event( VK_SHIFT, 0, KEYEVENTF_KEYUP, 0 ); // Release Shift key
}

// Adjusts system-wide parameters with SystemParametersInfo
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   hb_retl( SystemParametersInfoA( hmg_par_UINT( 1 ), hmg_par_UINT( 2 ), ( VOID * ) hb_parc( 3 ), hmg_par_UINT( 4 ) ) );
}

// Calculates and returns the width of a string in pixels for accurate rendering
HB_FUNC( GETTEXTWIDTH )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );            // Get device context
   HWND     hWnd = ( HWND ) NULL;
   BOOL     bDestroyDC = FALSE;
   HFONT    hFont = hmg_par_raw_HFONT( 3 );
   HFONT    hOldFont = ( HFONT ) NULL;
   SIZE     sz;

#ifndef UNICODE
   LPCSTR   lpString = hb_parc( 2 );
#else
   LPCWSTR  lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   if( !hDC )
   {
      bDestroyDC = TRUE;
      hWnd = GetActiveWindow();
      hDC = GetDC( hWnd ); // Acquire device context if not provided
   }

   if( hFont )
   {
      hOldFont = ( HFONT ) SelectObject( hDC, hFont );   // Select specified font if provided
   }

   GetTextExtentPoint32( hDC, lpString, ( int ) lstrlen( lpString ), &sz );   // Calculate text width
   if( hFont )
   {
      SelectObject( hDC, hOldFont );   // Restore original font
   }

   if( bDestroyDC )
   {
      ReleaseDC( hWnd, hDC );          // Release device context if created
   }

   hmg_ret_LONG( sz.cx );              // Return calculated text width
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpString );
#endif
}

// Sends a simulated keyboard event (key press/release)
HB_FUNC( KEYBD_EVENT )
{
   keybd_event( hmg_par_BYTE( 1 ), ( BYTE ) MapVirtualKey( hmg_par_UINT( 1 ), 0 ), hb_parl( 2 ) ? KEYEVENTF_KEYUP : 0, 0 );
}

// Inserts a single virtual key press
HB_FUNC( INSERTVKEY )
{
   keybd_event( hmg_par_BYTE( 1 ), 0, 0, 0 );
}

// Sets vertical scroll bar position in a window
HB_FUNC( _HMG_SETVSCROLLVALUE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_VSCROLL, MAKEWPARAM( SB_THUMBPOSITION, hb_parni( 2 ) ), 0 );
}

// Sets horizontal scroll bar position in a window
HB_FUNC( _HMG_SETHSCROLLVALUE )
{
   SendMessage( hmg_par_raw_HWND( 1 ), WM_HSCROLL, MAKEWPARAM( SB_THUMBPOSITION, hb_parni( 2 ) ), 0 );
}

// Shows the caret in a window
HB_FUNC( SHOWCARET )
{
   hb_retl( ShowCaret( hmg_par_raw_HWND( 1 ) ) );
}

// Hides the caret in a window
HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( hmg_par_raw_HWND( 1 ) ) );
}

// Destroys the caret, freeing resources
HB_FUNC( DESTROYCARET )
{
   hb_retl( DestroyCaret() );
}

// Creates a caret with specified dimensions in a window
HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( hmg_par_raw_HWND( 1 ), hmg_par_raw_HBITMAP( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * CHANGESTYLE (hWnd,dwAdd,dwRemove,lExStyle)
 * Action: Modifies the basic styles of a window
 * Parameters: hWnd - handle to window
 *             dwAdd - window styles to add
 *             dwRemove - window styles to remove
 *             lExStyle - TRUE for Extended style otherwise FALSE
 * HMG 1.1 Experimental Build 12a
 */
HB_FUNC( CHANGESTYLE )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   LONG_PTR dwAdd = hmg_par_raw_LONG_PTR( 2 );
   LONG_PTR dwRemove = hmg_par_raw_LONG_PTR( 3 );
   int      iStyle = hb_parl( 4 ) ? GWL_EXSTYLE : GWL_STYLE;               // Determine style type
   LONG_PTR dwStyle, dwNewStyle;

   dwStyle = GetWindowLongPtr( hWnd, iStyle );
   dwNewStyle = ( dwStyle & ( ~dwRemove ) ) | dwAdd;                       // Calculate new style by adding and removing styles
   HB_RETNL( ( LONG_PTR ) SetWindowLongPtr( hWnd, iStyle, dwNewStyle ) );  // Apply new style
   SetWindowPos( hWnd, NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ); // Update window style
}

// Function: MoveBtnTextBox(hEdit, hBtn1, hBtn2, fBtn2, BtnWidth, width, height)
// Repositions buttons relative to a text box, adjusting dimensions
HB_FUNC( MOVEBTNTEXTBOX )
{
   HWND  hedit = hmg_par_raw_HWND( 1 );
   HWND  hBtn1 = hmg_par_raw_HWND( 2 );
   HWND  hBtn2 = hmg_par_raw_HWND( 3 );
   BOOL  fBtn2 = hb_parl( 4 );   // Flag indicating if a second button is used
   int   BtnWidth = ( int ) hb_parni( 5 );
   int   BtnWidth2;
   int   width = ( int ) hb_parni( 6 );
   int   height = ( int ) hb_parni( 7 );
   BOOL  fBtns = ( hb_parnl( 2 ) > 0 );

   BtnWidth = ( BtnWidth >= GetSystemMetrics( SM_CYSIZE ) ? BtnWidth : GetSystemMetrics( SM_CYSIZE ) - 1 );          // Minimum button width
   BtnWidth = fBtns ? BtnWidth : 0;
   BtnWidth2 = fBtn2 ? BtnWidth : 0;

   SetWindowPos( hedit, NULL, 0, 0, width, height, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );  // Resize text box
   if( fBtns )
   {
      SetWindowPos( hBtn1, NULL, width - BtnWidth - 4, -1, BtnWidth, height - 2, SWP_NOACTIVATE | SWP_NOZORDER );    // Position first button
      if( fBtn2 )
      {
         SetWindowPos( hBtn2, NULL, width - BtnWidth - BtnWidth2 - 4, -1, BtnWidth2, height - 2, SWP_NOACTIVATE | SWP_NOZORDER );   // Position second button if needed
      }
   }
}

// Compatibility functions for Harbour/xHarbour, handling date and string operations
#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

// Function to handle date
HB_FUNC( HB_DATE )
{
   hb_retd( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 < 0x030200 )
#define hb_cdppage   hb_vmCDP // Define code page pointer
#endif

// Case-insensitive string comparison for left-side equality
HB_FUNC( HB_LEFTEQI )
{
   PHB_ITEM pItem1 = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pItem2 = hb_param( 2, HB_IT_STRING );

   if( pItem1 && pItem2 )
   {
      hb_retl( hb_cdpicmp( hb_itemGetCPtr( pItem1 ), hb_itemGetCLen( pItem1 ), hb_itemGetCPtr( pItem2 ), hb_itemGetCLen( pItem2 ), hb_cdppage(), HB_FALSE ) == 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );   // Raise error if arguments are invalid
   }
}
#endif
