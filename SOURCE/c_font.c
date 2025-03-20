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
 */
#include <mgdefs.h>
#include <windowsx.h>

#include "hbapiitm.h"
#include "hbapierr.h"

// Include hbwinuni.h if not using xHarbour and Harbour version is greater than 3.0.0
#if !defined( __XHARBOUR__ ) && ( __HARBOUR__ - 0 > 0x030000 )
#include "hbwinuni.h"
#else
#define HB_STRNCPY   hb_strncpy
#endif

// Convert ANSI string to Wide string
#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif

// MiniGUI Resources management for loading resources
void     RegisterResource( HANDLE hResource, LPCSTR szType );

#ifdef __XCC__
#define HB_ISBLOCK   ISBLOCK
#endif

// Function to prepare a font using given specifications
HFONT PrepareFont( TCHAR *FontName, int FontSize, int Weight, DWORD Italic, DWORD Underline, DWORD StrikeOut, DWORD Angle, DWORD charset )
{
   // Get device context for the desktop
   HDC   hDC = GetDC( HWND_DESKTOP );

   // Convert font size to logical units based on DPI settings
   FontSize = -MulDiv( FontSize, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );

   // Release the device context
   ReleaseDC( HWND_DESKTOP, hDC );

   // Create and return the font with specified attributes
   return CreateFont
      (
         FontSize,                        // Height of font
         0,                               // Width of font
         Angle,                           // Text rotation angle
         0,                               // Baseline rotation angle
         Weight,                          // Font weight (bold or normal)
         Italic,                          // Italic setting
         Underline,                       // Underline setting
         StrikeOut,                       // Strikeout setting
         charset,                         // Character set
         OUT_TT_PRECIS,                   // Output precision
         CLIP_DEFAULT_PRECIS,             // Clipping precision
         DEFAULT_QUALITY,                 // Output quality
         FF_DONTCARE,                     // Family and pitch
         FontName                         // Font name
      );
}

// Harbour function to initialize a font with given attributes
HB_FUNC( INITFONT )
{
   HFONT hFont;
   int   bold = hb_parl( 3 ) ? FW_BOLD : FW_NORMAL;
   DWORD italic = ( DWORD ) hb_parl( 4 );
   DWORD underline = ( DWORD ) hb_parl( 5 );
   DWORD strikeout = ( DWORD ) hb_parl( 6 );
   DWORD angle = hb_parnl( 7 );
   DWORD charset = hb_parnldef( 8, DEFAULT_CHARSET );

#ifdef UNICODE
   // Convert ANSI font name to Wide string if in UNICODE mode
   LPWSTR   pStr = AnsiToWide( hb_parc( 1 ) );
   hFont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 2 ), bold, italic, underline, strikeout, angle, charset );
   hb_xfree( pStr );                      // Free converted Wide string
#else
   hFont = PrepareFont( ( TCHAR * ) hb_parc( 1 ), hb_parni( 2 ), bold, italic, underline, strikeout, angle, charset );
#endif
   RegisterResource( hFont, "FONT" );     // Register the font resource
   hmg_ret_raw_HANDLE( hFont );           // Return the font handle
}

// Harbour function to set a font to a specified window
HB_FUNC( _SETFONT )
{
#ifdef UNICODE
   LPWSTR   pStr;
#endif
   HWND     hwnd = hmg_par_raw_HWND( 1 );

   // Check if the window is valid
   if( IsWindow( hwnd ) )
   {
      HFONT hFont;
      int   bold = hb_parl( 4 ) ? FW_BOLD : FW_NORMAL;
      DWORD italic = ( DWORD ) hb_parl( 5 );
      DWORD underline = ( DWORD ) hb_parl( 6 );
      DWORD strikeout = ( DWORD ) hb_parl( 7 );
      DWORD angle = hb_parnl( 8 );
      DWORD charset = hb_parnldef( 9, DEFAULT_CHARSET );

#ifdef UNICODE
      // Convert ANSI font name to Wide string if in UNICODE mode
      pStr = AnsiToWide( hb_parc( 2 ) );
      hFont = PrepareFont( ( TCHAR * ) pStr, hb_parni( 3 ), bold, italic, underline, strikeout, angle, charset );
      hb_xfree( pStr );
#else
      hFont = PrepareFont( ( TCHAR * ) hb_parc( 2 ), hb_parni( 3 ), bold, italic, underline, strikeout, angle, charset );
#endif

      // Apply the font to the window
      SetWindowFont( hwnd, hFont, TRUE );

      RegisterResource( hFont, "FONT" );  // Register the font resource
      hmg_ret_raw_HANDLE( hFont );        // Return the font handle
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

// Function to directly set an existing font handle to a window
HB_FUNC( _SETFONTHANDLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      if( GetObjectType( hmg_par_raw_HGDIOBJ( 2 ) ) == OBJ_FONT )
      {
         SetWindowFont( hwnd, hmg_par_raw_HFONT( 2 ), TRUE );
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 5050 + OBJ_FONT, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

// Function to get the system font used in non-client area metrics (e.g., window borders)
HB_FUNC( GETSYSTEMFONT )
{
   LOGFONT           lfDlgFont;
   NONCLIENTMETRICS  ncm;

#ifdef UNICODE
   LPSTR             pStr;
#endif

   // Set the size of NONCLIENTMETRICS structure
   ncm.cbSize = sizeof( ncm );

   // Retrieve system metrics for non-client areas
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0 );

   lfDlgFont = ncm.lfMessageFont;

   hb_reta( 2 );  // Return an array
#ifndef UNICODE
   HB_STORC( lfDlgFont.lfFaceName, -1, 1 );     // Store font name
#else
   pStr = WideToAnsi( lfDlgFont.lfFaceName );
   HB_STORC( pStr, -1, 1 );
   hb_xfree( pStr );
#endif
   HB_STORNI( 21 + lfDlgFont.lfHeight, -1, 2 ); // Store font height
}

/*
   Function for enumerating fonts based on provided criteria
   This code is partially based on original work by Dr. Claudio Soto (2014)

   EnumFontsEx ([ hDC ], [ cFontFamilyName ], [ nCharSet ], [ nPitch ], [ nFontType ], [ SortCodeBlock ], [ @aFontName ])
   Returns an array of font properties { { cFontName, nCharSet, nPitchAndFamily, nFontType }, ... }
*/
int CALLBACK   EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam );

HB_FUNC( ENUMFONTSEX )
{
   HDC      hdc;
   LOGFONT  lf;
   PHB_ITEM pArray = hb_itemArrayNew( 0 );
   BOOL     bReleaseDC = FALSE;

   memset( &lf, 0, sizeof( LOGFONT ) );

   // Check if a device context is provided, else get the default DC
   if( GetObjectType( hmg_par_raw_HGDIOBJ( 1 ) ) == OBJ_DC )
   {
      hdc = hmg_par_raw_HDC( 1 );
   }
   else
   {
      hdc = GetDC( NULL );
      bReleaseDC = TRUE;
   }

   // Set font family name, if provided
   if( hb_parclen( 2 ) > 0 )
   {
      HB_STRNCPY( lf.lfFaceName, ( LPCTSTR ) hb_parc( 2 ), HB_MIN( LF_FACESIZE - 1, hb_parclen( 2 ) ) );
   }
   else
   {
      lf.lfFaceName[0] = TEXT( '\0' );
   }

   // Set charset and pitch/family based on parameters or defaults
   lf.lfCharSet = HB_ISNUM( 3 ) ? ( BYTE ) ( hb_parni( 3 ) == DEFAULT_CHARSET ? GetTextCharset( hdc ) : hb_parni( 3 ) ) : ( BYTE ) hb_parni( 3 );
   lf.lfPitchAndFamily = HB_ISNUM( 4 ) ? ( BYTE ) ( hb_parni( 4 ) == DEFAULT_PITCH ? 0 : ( hb_parni( 4 ) | FF_DONTCARE ) ) : ( BYTE ) 0;

   // Enumerate fonts using the specified parameters
   EnumFontFamiliesEx( hdc, &lf, ( FONTENUMPROC ) EnumFontFamExProc, ( LPARAM ) pArray, ( DWORD ) 0 );

   if( bReleaseDC )
   {
      ReleaseDC( NULL, hdc );
   }

   // Sort the array if a sorting block is provided
   if( HB_ISBLOCK( 6 ) )
   {
      hb_arraySort( pArray, NULL, NULL, hb_param( 6, HB_IT_BLOCK ) );
   }

   // If a by-reference array for font names is provided, store names in it
   if( HB_ISBYREF( 7 ) )
   {
      PHB_ITEM aFontName = hb_param( 7, HB_IT_ANY );
      int      nLen = ( int ) hb_arrayLen( pArray ), i;

      hb_arrayNew( aFontName, nLen );

      for( i = 1; i <= nLen; i++ )
      {
         hb_arraySetC( aFontName, i, hb_arrayGetC( hb_arrayGetItemPtr( pArray, i ), 1 ) );
      }
   }

   hb_itemReturnRelease( pArray );              // Return font enumeration array
}

// Callback function for EnumFontFamExProc to handle each font found
int CALLBACK EnumFontFamExProc( ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme, DWORD FontType, LPARAM lParam )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   HB_SYMBOL_UNUSED( lpntme );

   // Avoid fonts prefixed with '@'
   if( lpelfe->elfLogFont.lfFaceName[0] != '@' )
   {
      PHB_ITEM pSubArray = hb_itemArrayNew( 4 );

#ifdef UNICODE
      pStr = WideToAnsi( lpelfe->elfLogFont.lfFaceName );
      hb_arraySetC( pSubArray, 1, pStr );       // Font name
#else
      hb_arraySetC( pSubArray, 1, lpelfe->elfLogFont.lfFaceName );
#endif
      hb_arraySetNL( pSubArray, 2, lpelfe->elfLogFont.lfCharSet );   // Charset
      hb_arraySetNI( pSubArray, 3, lpelfe->elfLogFont.lfPitchAndFamily & FIXED_PITCH );   // Pitch and Family
      hb_arraySetNI( pSubArray, 4, FontType & TRUETYPE_FONTTYPE );   // Font type (TrueType)
      hb_arrayAddForward( ( PHB_ITEM ) lParam, pSubArray );
      hb_itemRelease( pSubArray );
#ifdef UNICODE
      hb_xfree( pStr );
#endif
   }

   return 1;   // Continue enumeration
}
