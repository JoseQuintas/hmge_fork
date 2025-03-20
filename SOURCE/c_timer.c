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
#include <mgdefs.h>

// INITTIMER
// Initializes a timer for a specific window with a given interval.
// Parameters:
//   1. hwnd - Handle of the window associated with the timer.
//   2. nIDEvent - Unique identifier for the timer within the window's scope.
//   3. uElapse - Time interval in milliseconds between timer events.
// Returns:
//   Logical (TRUE/FALSE) indicating if the timer was successfully created.
HB_FUNC( INITTIMER )
{
   // SetTimer returns the timer ID if successful, 0 otherwise.
   // Convert this result to a logical value and return it.
   hb_retl
   (
      ( UINT ) SetTimer
         (
            hmg_par_raw_HWND( 1 ),  // Window handle associated with the timer
            hmg_par_UINT( 2 ),      // Timer ID
            hmg_par_UINT( 3 ),      // Timer interval in milliseconds
            ( TIMERPROC ) NULL      // No callback function; uses default WM_TIMER message
         ) != 0
   );
}

// KILLTIMER
// Stops and destroys the timer for a specific window and timer ID.
// Parameters:
//   1. hwnd - Handle of the window associated with the timer.
//   2. nIDEvent - Unique identifier of the timer to destroy.
// Returns:
//   Logical (TRUE/FALSE) indicating if the timer was successfully destroyed.
HB_FUNC( KILLTIMER )
{
   hb_retl(
      KillTimer(
         hmg_par_raw_HWND(1),  // Window handle associated with the timer
         hmg_par_UINT(2)       // Timer ID to destroy
      ) != 0
   );
}
