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
#define _WIN32_IE 0x0501            // Sets the minimum required version of Internet Explorer to 5.01 for compatibility.

#include <mgdefs.h>                 // Includes necessary definitions and macros.
#include <commctrl.h>               // Includes common controls header for status bar and tooltips.

// Function prototypes for character conversion functions if using Unicode.
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );   // Converts an ANSI string to a wide (Unicode) string.
LPSTR       WideToAnsi( LPWSTR );   // Converts a wide (Unicode) string to an ANSI string.
#endif

// Function prototypes for obtaining instances and resources.
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

// Initializes a status bar with one section/part.
HB_FUNC( INITMESSAGEBAR )
{
   HWND  hWndSB;                    // Handle for the status bar window.
   int   ptArray[40];               // Array to define sections in the status bar.
   int   nrOfParts = 1;             // Number of sections/parts in the status bar.

   // Creates a visible status bar window as a child of a specified parent window.
   hWndSB = CreateStatusWindow( WS_CHILD | WS_VISIBLE | SBT_TOOLTIPS, NULL, hmg_par_raw_HWND( 1 ), hb_parni( 2 ) );

   if( hWndSB )
   {
      // Sets the number of parts/sections in the status bar to `nrOfParts`.
      SendMessage( hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray );
   }

   // Returns the handle of the created status bar window.
   hmg_ret_raw_HWND( hWndSB );
}

// Initializes a customized item bar with icons and text.
HB_FUNC( INITITEMBAR )
{
   HWND  hWndSB;                    // Handle for the status bar window.
   int   cSpaceInBetween = 8;       // Spacing between status bar parts.
   int   ptArray[40];               // Array to define the sections of the bar.
   int   nrOfParts = 0;             // Number of parts/sections in the status bar.
   int   n;             // Loop variable for sections.
   RECT  rect;          // Rectangle to store client area dimensions.
   HDC   hDC;           // Device context handle.
   WORD  displayFlags;  // Display style for the item.
   HICON hIcon;         // Icon handle for the item.
   int   Style;         // Style of the status bar window.
   int   cx, cy;        // Dimensions for the icon.
#ifndef UNICODE
   // ANSI strings for item text, icon name, and tooltip text.
   LPCSTR   lpText = hb_parc( 2 );
   LPCSTR   lpIconName = hb_parc( 6 );
   LPCSTR   lpTipText = hb_parc( 7 );
#else
   // Convert ANSI strings to Unicode if in Unicode mode.
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPWSTR   lpIconName = AnsiToWide( ( char * ) hb_parc( 6 ) );
   LPWSTR   lpTipText = AnsiToWide( ( char * ) hb_parc( 7 ) );
#endif

   hWndSB = hmg_par_raw_HWND( 1 );

   // Get the parent window style.
   Style = GetWindowLong( ( HWND ) GetParent( hWndSB ), GWL_STYLE );

   // Sets display flags based on the parameter value.
   switch( hb_parni( 8 ) )
   {
      case 0:
         displayFlags = 0;
         break;

      case 1:
         displayFlags = SBT_POPOUT;
         break;

      case 2:
         displayFlags = SBT_NOBORDERS;
         break;

      default:
         displayFlags = 0;
   }

   // Checks if the function should initialize parts from existing parts.
   if( hb_parnl( 5 ) )
   {
      nrOfParts = ( int ) SendMessage( hWndSB, SB_GETPARTS, 40, 0 );                   // Gets current number of parts.
      SendMessage( hWndSB, SB_GETPARTS, ( WPARAM ) 40, ( LPARAM ) ( LPINT ) ptArray ); // Copies current parts to ptArray.
   }

   nrOfParts++;   // Increase the number of parts by one.

   // Get the client area rectangle dimensions for the status bar.
   hDC = GetDC( hWndSB );
   GetClientRect( hWndSB, &rect );

   if( hb_parnl( 5 ) == 0 )
   {
      ptArray[nrOfParts - 1] = rect.right;   // Set the last part to fill the width.
   }
   else
   {
      for( n = 0; n < nrOfParts - 1; n++ )
      {
         ptArray[n] -= hb_parni( 4 ) - cSpaceInBetween;
      }

      // Adjusts the last part if the parent has a size box style.
      if( Style & WS_SIZEBOX )
      {
         if( nrOfParts == 2 )
         {
            ptArray[0] -= 21;
         }

         ptArray[nrOfParts - 1] = rect.right - rect.bottom - rect.top + 2;
      }
      else
      {
         ptArray[nrOfParts - 1] = rect.right;
      }
   }

   ReleaseDC( hWndSB, hDC );                 // Release device context.

   // Apply the parts array to the status bar.
   SendMessage( hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray );

   // Set icon dimensions and load icon image.
   cy = rect.bottom - rect.top - 4;
   cx = cy;
   hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, cx, cy, 0 );

   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE );
   }

   // Set the icon in the last part if successfully loaded.
   if( hIcon != NULL )
   {
      SendMessage( hWndSB, SB_SETICON, ( WPARAM ) nrOfParts - 1, ( LPARAM ) hIcon );
   }

   // Set text and tooltip for the last part of the status bar.
   SendMessage( hWndSB, SB_SETTEXT, ( WPARAM ) ( ( nrOfParts - 1 ) | displayFlags ), ( LPARAM ) lpText );
   SendMessage( hWndSB, SB_SETTIPTEXT, ( WPARAM ) nrOfParts - 1, ( LPARAM ) lpTipText );

   hb_retni( nrOfParts );                    // Return the updated number of parts.
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );           // Free Unicode memory allocations.
   hb_xfree( ( TCHAR * ) lpIconName );
   hb_xfree( ( TCHAR * ) lpTipText );
#endif
}

// Updates the text in a specified part of the item bar.
HB_FUNC( SETITEMBAR )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );    // Handle for the status bar.
   int      iPos = hb_parni( 3 );            // Position of the item to update.
   WORD     nFlags;                 // Current display flags for the text.
#ifndef UNICODE
   LPCSTR   lpText = hb_parc( 2 );  // ANSI text to set.
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert to Unicode text if needed.
#endif

   // Retrieve the current text length and style flags for the part.
   nFlags = HIWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, ( WPARAM ) iPos, 0 ) );
   SendMessage( hWnd, SB_SETTEXT, ( WPARAM ) ( iPos | nFlags ), ( LPARAM ) lpText );   // Update the text.
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );     // Free allocated memory if using Unicode.
#endif
}

// Retrieves and returns the text of a specified part of the item bar.
HB_FUNC( GETITEMBAR )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   HWND  hWnd = hmg_par_raw_HWND( 1 ); // Handle for the status bar.
   int   iPos = hb_parni( 2 );         // Position of the item to retrieve.
   TCHAR *cString;                     // Pointer to hold the retrieved text.

   // Allocate memory based on text length and retrieve the text.
   cString = ( TCHAR * ) hb_xgrab( ( LOWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, ( WPARAM ) iPos - 1, 0 ) ) + 1 ) * sizeof( TCHAR ) );
   SendMessage( hWnd, SB_GETTEXT, ( WPARAM ) iPos - 1, ( LPARAM ) cString );

#ifndef UNICODE
   hb_retc( cString );                 // Return the ANSI string directly.
#else
   pStr = WideToAnsi( cString );       // Convert to ANSI and return.
   hb_retc( pStr );
   hb_xfree( pStr );                   // Free memory after conversion.
#endif
   hb_xfree( cString );                // Free the initial memory allocation.
}

// Refreshes the layout and dimensions of parts in the item bar.
HB_FUNC( REFRESHITEMBAR )
{
   HWND  hWndSB;        // Handle for the status bar.
   int   ptArray[40];   // Array for parts' boundaries.
   int   nDev;          // Device adjustment for resizing.
   int   n, s;          // Loop and status variables.
   int   nrOfParts;     // Number of parts in the status bar.
   RECT  rect;          // Rectangle for client area dimensions.
   HDC   hDC;           // Device context handle.
   int   size;          // New size parameter for the part.
   hWndSB = hmg_par_raw_HWND( 1 );
   size = hb_parni( 2 );
   nrOfParts = ( int ) SendMessage( hWndSB, SB_GETPARTS, 40, 0 );                   // Gets the current part count.
   SendMessage( hWndSB, SB_GETPARTS, ( WPARAM ) 40, ( LPARAM ) ( LPINT ) ptArray ); // Copies the part boundaries to ptArray.
   hDC = GetDC( hWndSB );           // Retrieves device context.
   GetClientRect( hWndSB, &rect );  // Gets the client rectangle of the status bar.

   // Calculates required adjustment based on window state and size.
   if( ( nrOfParts == 1 ) || ( IsZoomed( GetParent( hWndSB ) ) ) || ( !( GetWindowLong( ( HWND ) GetParent( hWndSB ), GWL_STYLE ) & WS_SIZEBOX ) ) )
   {
      nDev = rect.right - ptArray[nrOfParts - 1];
   }
   else
   {
      nDev = rect.right - ptArray[nrOfParts - 1] - rect.bottom - rect.top + 2;
   }

   s = TRUE;   // Indicates if adjustment is feasible.
   if( rect.right > 0 )
   {
      for( n = 0; n <= nrOfParts - 1; n++ )
      {
         if( n == 0 )
         {
            if( size >= ptArray[n] && nDev < 0 )
            {
               s = FALSE;
            }
            else
            {
               if( ptArray[n] + nDev < size )
               {
                  nDev = size - ptArray[n];
               }

               ptArray[n] += nDev;  // Apply adjustment.
            }
         }
         else if( s )
         {
            ptArray[n] += nDev;     // Apply adjustment if `s` remains true.
         }
      }
   }

   ReleaseDC( hWndSB, hDC );        // Release device context.
   SendMessage( hWndSB, SB_SETPARTS, ( WPARAM ) nrOfParts, ( LPARAM ) ( LPINT ) ptArray );   // Update parts with new layout.

   hb_retni( nrOfParts );              // Return the updated part count.
}

// Toggles the state of a specific key on the keyboard.
HB_FUNC( KEYTOGGLE )
{
   BYTE  pBuffer[256];                 // Buffer to hold the keyboard state for each key.
   WORD  wKey = hmg_par_WORD( 1 );     // Key code to toggle, passed as a parameter.
   GetKeyboardState( pBuffer );        // Retrieves the current state of each key.

   // Checks if the specified key is currently "on" (1) or "off" (0).
   if( pBuffer[wKey] & 0x01 )
   {
      pBuffer[wKey] &= 0xFE;           // Turns the key "off" by clearing the least significant bit.
   }
   else
   {
      pBuffer[wKey] |= 0x01;           // Turns the key "on" by setting the least significant bit.
   }

   SetKeyboardState( pBuffer );        // Updates the keyboard state to reflect the toggle.
}

// Simulates a key press and release for a specified key.
HB_FUNC( KEYTOGGLENT )
{
   BYTE  wKey = hmg_par_BYTE( 1 );     // Key code to simulate, passed as a parameter.

   // Simulates key press with extended key flag.
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY | 0, 0 );

   // Simulates key release with key-up flag.
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0 );
}

// Sets an icon on a specific item in the status bar.
HB_FUNC( SETSTATUSITEMICON )
{
   HWND     hwnd;
   RECT     rect;
   HICON    hIcon = NULL;
   int      cx;
   int      cy;

#ifndef UNICODE
   LPCSTR   lpIconName = hb_parc( 3 ); // Icon name (string) for ANSI builds.
#else
   LPWSTR   lpIconName = AnsiToWide( ( char * ) hb_parc( 3 ) );   // Converts ANSI to wide string for Unicode.
#endif
   hwnd = hmg_par_raw_HWND( 1 );    // Handle to the status bar, passed as a parameter.

   // Removes the current icon from the specified status bar item.
   DestroyIcon( ( HICON ) SendMessage( hwnd, SB_GETICON, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) 0 ) );

   GetClientRect( hwnd, &rect );    // Gets dimensions of the status bar client area.
   cy = rect.bottom - rect.top - 4; // Sets icon size based on status bar height.
   cx = cy;

   if ( HB_ISCHAR( 3 ) )   // Load icon from resources.
   {
      hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, cx, cy, 0 );
      if( hIcon == NULL )  // If loading from resources failed, try loading from file.
      {
         hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE );
      }
   }
   else if( HB_ISNUM( 4 ) ) // Get hIcon from 4th parameter.
   {
      hIcon = hmg_par_raw_HICON( 4 );
   }

   if( hIcon != NULL )
   {
      SendMessage( hwnd, SB_SETICON, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) hIcon ); // Sets the icon in the status bar.
   }
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpIconName );             // Frees memory if Unicode conversion was used.
#endif
}

// Configures the width of each part/section in the status bar.
HB_FUNC( SETSTATUSBARSIZE )
{
   HLOCAL   hloc;
   LPINT    lpParts;

   HWND     hwndStatus = hmg_par_raw_HWND( 1 );    // Status bar handle.
   int      nParts = ( int ) hb_parinfa( 2, 0 );   // Number of parts defined in the array.
   int      nWidth;
   int      i;

   // Allocates memory to hold the widths of each status bar part.
   hloc = LocalAlloc( LHND, sizeof( int ) * nParts );
   lpParts = ( LPINT ) LocalLock( hloc );

   nWidth = 0; // Cumulative width for each part.

   // Assigns each part width based on the provided array.
   for( i = 0; i < nParts; i++ )
   {
      nWidth = nWidth + HB_PARNI( 2, i + 1 );   // Adds width for current part.
      lpParts[i] = nWidth;
   }

   SendMessage( hwndStatus, SB_SETPARTS, ( WPARAM ) nParts, ( LPARAM ) lpParts );   // Sets the parts on the status bar.
   MoveWindow( hwndStatus, 0, 0, 0, 0, TRUE );  // Redraws the window to apply changes.

   LocalUnlock( hloc ); // Releases the allocated memory.
   LocalFree( hloc );
}

// Updates the position of a progress bar in a status bar item to match the item's current position.
HB_FUNC( REFRESHPROGRESSITEM )   // RefreshProgressItem( HwndStatus, NrItem, hProgress )
{
   HWND  hwndStatus = hmg_par_raw_HWND( 1 ); // Handle to the status bar.
   RECT  rc;

   // Gets the rectangle for the specified status bar item.
   SendMessage( hwndStatus, SB_GETRECT, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) & rc );

   // Adjusts the position of the progress bar within the item.
   SetWindowPos( hmg_par_raw_HWND( 3 ), 0, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE );
}

// Creates a progress bar control within a specified status bar item.
HB_FUNC( CREATEPROGRESSBARITEM ) // CreateProgressBarItem( HwndStatus, NrItem )
{
   HWND  hwndStatus = hmg_par_raw_HWND( 1 ); // Handle to the status bar.
   HWND  hwndProgressBar;
   RECT  rc;
   DWORD Style = WS_CHILD | PBS_SMOOTH;      // Style for a smooth child progress bar.

   // Retrieves the rectangle for the specified item in the status bar.
   SendMessage( hwndStatus, SB_GETRECT, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) & rc );
   if( hb_parni( 3 ) )                 // If visible flag is set, add WS_VISIBLE style.
   {
      Style |= WS_VISIBLE;
   }

   // Creates the progress bar control within the item rectangle.
   if
   (
      (
         hwndProgressBar = CreateWindowEx
            (
               0,                      // No extended styles.
               PROGRESS_CLASS,         // Class name for progress bars.
               ( LPCTSTR ) NULL,       // No title text.
               Style,                  // Defined style.
               rc.top,                 // Top position.
               rc.left,                // Left position.
               rc.right - rc.left,     // Width.
               rc.bottom - rc.top - 1, // Height.
               hwndStatus,             // Parent window is the status bar.
               ( HMENU ) NULL,         // No menu.
               GetInstance(),          // Instance handle.
               ( LPVOID ) NULL         // No additional data.
            )
      ) != NULL
   )
   {
      // Sets the range and initial position of the progress bar.
      SendMessage( hwndProgressBar, PBM_SETRANGE, 0, MAKELONG( hb_parni( 4 ), hb_parni( 5 ) ) );
      SendMessage( hwndProgressBar, PBM_SETPOS, ( WPARAM ) hb_parni( 3 ), 0 );

      hmg_ret_raw_HWND( hwndProgressBar );   // Returns the handle to the new progress bar.
   }
   else  // Returns NULL if creation failed.
   {
      hmg_ret_raw_HWND( NULL );
   }
}

// Sets the position and visibility of a progress bar in a status bar item.
HB_FUNC( SETPOSPROGRESSBARITEM ) // SetPosProgressBarItem( HwndProgressBar, nPos )
{
   HWND  hwndProgressBar = hmg_par_raw_HWND( 1 );  // Handle to the progress bar.

   // Shows or hides the progress bar based on `nPos` (0 to hide, non-0 to show).
   ShowWindow( hwndProgressBar, hb_parni( 2 ) ? SW_SHOW : SW_HIDE );

   // Sets the current position of the progress bar.
   SendMessage( hwndProgressBar, PBM_SETPOS, ( WPARAM ) hb_parni( 2 ), 0 );
}
