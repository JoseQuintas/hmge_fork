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
/*
 * Author: P.Chornyj <myorg63@mail.ru>
 * revised for build 16.10
*/

#include "minigui.ch"

#ifdef __XHARBOUR__
#xcommand END SWITCH => END
#xcommand OTHERWISE  => DEFAULT
#endif

FUNCTION _SetWindowBKBrush( cWindow, lNoDelete, cBrushStyle, nHatch, aColor, xImage )
/*
 *  Purpose: Sets the background brush for a specified window.
 *
 *  Parameters:
 *      cWindow     : Character string representing the window name.
 *      lNoDelete   : Logical value indicating whether the old brush should be deleted.
 *                    .T. - Do not delete the old brush. The function returns the handle to the old brush.
 *                    .F. - Delete the old brush (default).
 *      cBrushStyle : Character string specifying the brush style.
 *                    "SOLID" - Solid color brush.
 *                    "HATCH" - Hatched brush.
 *                    "PATTERN" - Pattern brush.
 *      nHatch      : Numeric value specifying the hatch style for hatched brushes (e.g., HS_VERTICAL).
 *      aColor      : Array containing the RGB color values (e.g., { 255, 0, 255 } for magenta).
 *      xImage      : Image name for pattern brush.
 *
 *  Return Value:
 *      hBrush      : Handle to the newly created brush.  Returns 0 if an error occurs or if the window is not found.
 *
 *  Notes:
 *      This function retrieves the window handle based on the window name, creates a brush based on the specified style,
 *      sets the brush as the window's background brush, and optionally deletes the old brush.
 *      The function uses the _HMG_aFormHandles and _HMG_aFormBrushHandle arrays to store window handles and brush handles, respectively.
 */
   LOCAL hWnd
   LOCAL hOldBrush
   LOCAL hBrush := 0
   LOCAL nIndex

   __defaultNIL( @lNoDelete, .F. )
   /*
    * Purpose: Sets the default value of lNoDelete to .F. if it is NIL.
    * Explanation: This ensures that if the lNoDelete parameter is not provided, the old brush will be deleted by default.
    */
   __defaultNIL( @cBrushStyle, "SOLID" )
   /*
    * Purpose: Sets the default value of cBrushStyle to "SOLID" if it is NIL.
    * Explanation: This ensures that if the cBrushStyle parameter is not provided, a solid color brush will be created by default.
    */
   __defaultNIL( @nHatch, HS_VERTICAL )
   /*
    * Purpose: Sets the default value of nHatch to HS_VERTICAL if it is NIL.
    * Explanation: This ensures that if the nHatch parameter is not provided, a vertical hatch style will be used by default for hatched brushes.
    */
   __defaultNIL( @aColor, { 255, 0, 255 } )
   /*
    * Purpose: Sets the default value of aColor to { 255, 0, 255 } (magenta) if it is NIL.
    * Explanation: This ensures that if the aColor parameter is not provided, a magenta color will be used by default.
    */
   __defaultNIL( @xImage, "MINIGUI_EDIT_DELETE" )
   /*
    * Purpose: Sets the default value of xImage to "MINIGUI_EDIT_DELETE" if it is NIL.
    * Explanation: This ensures that if the xImage parameter is not provided, the "MINIGUI_EDIT_DELETE" image will be used by default for pattern brushes.
    */

   nIndex := GetFormIndex ( cWindow )
   /*
    * Purpose: Gets the index of the window in the _HMG_aFormHandles array.
    * Explanation: The GetFormIndex function (assumed to be defined elsewhere) retrieves the index of the window based on its name.
    *              This index is used to access the window handle and brush handle in the _HMG_aFormHandles and _HMG_aFormBrushHandle arrays.
    */

   IF nIndex > 0
      hWnd := _HMG_aFormHandles[ nIndex ]
      /*
       * Purpose: Retrieves the window handle from the _HMG_aFormHandles array using the index.
       * Explanation: The _HMG_aFormHandles array (assumed to be a global array) stores the handles of all forms/windows.
       */

      SWITCH Left ( cBrushStyle, 1 )
      /*
       * Purpose: Determines the brush style based on the first character of the cBrushStyle string.
       * Explanation: This switch statement selects the appropriate brush creation function based on the specified brush style.
       */
      CASE "S"
         hBrush := CreateSolidBrush ( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] )
         /*
          * Purpose: Creates a solid color brush.
          * Explanation: The CreateSolidBrush function (assumed to be defined elsewhere) creates a brush with the specified RGB color.
          */
         EXIT

      CASE "H"
         hBrush := CreateHatchBrush ( nHatch, RGB( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] ) )
         /*
          * Purpose: Creates a hatched brush.
          * Explanation: The CreateHatchBrush function (assumed to be defined elsewhere) creates a brush with the specified hatch style and color.
          */
         EXIT

      CASE "P"
         hBrush := CreatePatternBrush ( xImage )
         /*
          * Purpose: Creates a pattern brush.
          * Explanation: The CreatePatternBrush function (assumed to be defined elsewhere) creates a brush with the specified image pattern.
          */
         EXIT

      OTHERWISE
         hBrush := GetWindowBrush ( hWnd )
         /*
          * Purpose: Gets the existing window brush.
          * Explanation: The GetWindowBrush function (assumed to be defined elsewhere) retrieves the current background brush of the window.
          */

      END SWITCH

      IF GetObjectType ( hBrush ) == OBJ_BRUSH
         /*
          * Purpose: Checks if the created object is a brush.
          * Explanation: The GetObjectType function (assumed to be defined elsewhere) returns the type of the object. This check ensures that the brush was created successfully.
          */
         hOldBrush := SetWindowBrush ( hWnd, hBrush )
         /*
          * Purpose: Sets the new brush as the window's background brush and retrieves the old brush.
          * Explanation: The SetWindowBrush function (assumed to be defined elsewhere) sets the background brush of the window to the new brush and returns the handle to the old brush.
          */
         _HMG_aFormBrushHandle[ nIndex ] := hBrush
         /*
          * Purpose: Stores the new brush handle in the _HMG_aFormBrushHandle array.
          * Explanation: The _HMG_aFormBrushHandle array (assumed to be a global array) stores the handles of the brushes associated with each form/window.
          */

         IF lNoDelete
            RETURN hOldBrush
            /*
             * Purpose: Returns the handle to the old brush if lNoDelete is .T..
             * Explanation: This allows the caller to manage the old brush if it should not be deleted.
             */
         ELSE
            DELETE BRUSH hOldBrush
            /*
             * Purpose: Deletes the old brush if lNoDelete is .F..
             * Explanation: This releases the resources associated with the old brush. The DELETE BRUSH command is assumed to be a Harbour command for deleting a brush handle.
             */
         ENDIF
      ENDIF
   ENDIF

RETURN hBrush
/*
 * Purpose: Returns the handle to the newly created brush.
 * Explanation: This allows the caller to use the brush handle for other purposes.
 */
