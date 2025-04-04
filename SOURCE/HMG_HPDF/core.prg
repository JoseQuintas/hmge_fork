#ifdef __XHARBOUR__
#define __SYSDATA__
#define __MINIPRINT__
#endif

#include <hmg.ch>
#include "harupdf.ch"

MEMVAR _HMG_HPDFDATA

STATIC DASH_MODE1 := { 3 }
STATIC DASH_MODE2 := { 3, 7 }
STATIC DASH_MODE3 := { 8, 7, 2, 7 }

FUNCTION _HMG_HPDF_INIT( cPDFFile, nOrientation, nPaperSize, nPaperLength, nPaperWidth )

   DEFAULT nOrientation := 1
   DEFAULT nPaperSize := 1

   PUBLIC _HMG_HPDFDATA := Array( 1 )

   IF ValType( _HMG_HPDFDATA[ 1 ] ) == 'U'
      _HMG_HPDFDATA[ 1 ] := { ;
         NIL, ;                 // 1 PDF Object
         cPDFFile, ;            // 2 PDF File to Save
         0, ;                   // 3 PDF PaperSize
         0, ;                   // 4 PDF PaperWidth
         0, ;                   // 5 PDF PaperHeight
         nOrientation, ;        // 6 PDF Orientation
         NIL, ;                 // 7 PDF Current Page
         'Helvetica', ;         // 8 PDF default font
         12, ;                  // 9 PDF default fontsize
         "StandardEncoding", ;  // 10 PDF default encoding
         {}, ;                  // 11 PDF Outlines Array
         {}, ;                  // 12 PDF Pages Array
         0, ;                   // 13 PDF Current Page Number
         }
      _HMG_HPDFDATA[ 1 ][ 1 ] := HPDF_New()
      IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL
         _HMG_HPDF_Error( 2 )
         RETURN .F.
      ENDIF
      _HMG_HPDFDATA[ 1 ][ 3 ] := nPaperSize
      IF nPaperSize == 256 // user size
         IF _HMG_HPDFDATA[ 1 ][ 6 ] == 1 // orientation portrait
            _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( nPaperWidth )
            _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( nPaperLength )
         ELSE // orientation landscape
            _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( nPaperLength )
            _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( nPaperWidth )
         ENDIF
      ELSE
         _HMG_HPDF_INIT_PAPERSIZE( nPaperSize )
      ENDIF
   ELSE
      _HMG_HPDF_Error( 1 )
      RETURN .F.
   ENDIF

RETURN .T.

FUNCTION _hmg_hpdf_setencoding( cEncoding )

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF cEncoding == "UTF-8"
      HPDF_UseUTFEncodings( _HMG_HPDFDATA[ 1 ][ 1 ] )
   ELSE
      HPDF_SetCurrentEncoder( _HMG_HPDFDATA[ 1 ][ 1 ], cEncoding )
   ENDIF
   _HMG_HPDFDATA[ 1 ][ 10 ] := cEncoding

RETURN NIL

FUNCTION _hmg_hpdf_startdoc()

RETURN NIL

FUNCTION _hmg_hpdf_startpage()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // Page is free
      _HMG_HPDFDATA[ 1 ][ 7 ] := HPDF_AddPage( _HMG_HPDFDATA[ 1 ][ 1 ] )
      IF Empty( _HMG_HPDFDATA[ 1 ][ 7 ] )
         _HMG_HPDF_Error( 9, "Start Page" )
         RETURN NIL
      ELSE
         // add current page to the pages array
         AAdd( _HMG_HPDFDATA[ 1 ][ 12 ], _HMG_HPDFDATA[ 1 ][ 7 ] )
         _HMG_HPDFDATA[ 1 ][ 13 ] := Len( _HMG_HPDFDATA[ 1 ][ 12 ] )
         // set page size
         HPDF_Page_SetWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDFDATA[ 1 ][ 4 ] )
         HPDF_Page_SetHeight( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDFDATA[ 1 ][ 5 ] )
      ENDIF
   ELSE
      _HMG_HPDF_Error( 4 )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_InsertPage( nPage )

   LOCAL oOldPage := NIL
   LOCAL aPages := AClone( _HMG_HPDFDATA[ 1 ][ 12 ] )

   IF nPage > Len( aPages ) .OR. nPage < 1
      _HMG_HPDF_Error( 16 )
      RETURN NIL
   ENDIF
   oOldPage := aPages[ nPage ]
   IF Empty( oOldPage )
      _HMG_HPDF_Error( 16 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // Page is free
      _HMG_HPDFDATA[ 1 ][ 7 ] := HPDF_InsertPage( _HMG_HPDFDATA[ 1 ][ 1 ], oOldPage )
      IF Empty( _HMG_HPDFDATA[ 1, 7 ] )
         _HMG_HPDF_Error( 9, "Insert Page" )
         RETURN NIL
      ELSE
         // insert current page to the pages array
         ASize( aPages, Len( aPages ) + 1 )
         AIns( aPages, nPage )
         aPages[ nPage ] := _HMG_HPDFDATA[ 1, 7 ]
         _HMG_HPDFDATA[ 1 ][ 12 ] := AClone( aPages )
         _HMG_HPDFDATA[ 1 ][ 13 ] := nPage
         HPDF_Page_SetWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDFDATA[ 1 ][ 4 ] )
         HPDF_Page_SetHeight( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDFDATA[ 1 ][ 5 ] )
      ENDIF
   ELSE
      _HMG_HPDF_Error( 4 )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_GoToPage( nPage )

   LOCAL oPage := NIL
   LOCAL aPages := AClone( _HMG_HPDFDATA[ 1 ][ 12 ] )

   IF nPage > Len( aPages ) .OR. nPage < 1
      _HMG_HPDF_Error( 18 )
      RETURN NIL
   ENDIF
   oPage := aPages[ nPage ]
   IF Empty( oPage )
      _HMG_HPDF_Error( 18 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // Page is free
      _HMG_HPDFDATA[ 1 ][ 7 ] := aPages[ nPage ]
      IF Empty( _HMG_HPDFDATA[ 1, 7 ] )
         _HMG_HPDF_Error( 19 )
         RETURN NIL
      ENDIF
      _HMG_HPDFDATA[ 1 ][ 13 ] := nPage
   ENDIF

RETURN NIL

FUNCTION _hmg_hpdf_endpage()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // Page is free
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ELSE
      _HMG_HPDFDATA[ 1 ][ 7 ] := NIL
   ENDIF

RETURN NIL

FUNCTION HMG_HPDF_PageCount()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF

RETURN Len( _HMG_HPDFDATA[ 1 ][ 12 ] )

FUNCTION HMG_HPDF_PageNo()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // Page is free
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

RETURN _HMG_HPDFDATA[ 1 ][ 13 ]

FUNCTION _hmg_hpdf_enddoc()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF HPDF_SaveToFile( _HMG_HPDFDATA[ 1 ][ 1 ], _HMG_HPDFDATA[ 1 ][ 2 ] ) != 0
      _HMG_HPDF_Error( 20, "End Doc" )
   ENDIF
   HPDF_Free( _HMG_HPDFDATA[ 1 ][ 1 ] )
   _HMG_HPDFDATA[ 1 ] := NIL

RETURN NIL

FUNCTION _hmg_hpdf_abortdoc()

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   HPDF_Free( _HMG_HPDFDATA[ 1 ][ 1 ] )
   _HMG_HPDFDATA[ 1 ][ 1 ] := NIL

RETURN NIL

FUNCTION _HMG_HPDF_PRINT( nRow, nCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lUnderline, lStrikeout, lColor, lFont, lSize, cAlign, nAngle )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nTextWidth
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL oFont := NIL
   LOCAL cFont
   LOCAL nRad := 0

   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lUnderline := .F.
   DEFAULT lStrikeout := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT nAngle := 0

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )

   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font

      cFont := HPDF_LoadTTFontFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF
   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ELSE
      HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
      nTextWidth := HPDF_Page_TextWidth( _HMG_HPDFDATA[ 1 ][ 7 ], cText )
      IF nAngle == 0
         DO CASE
         CASE Upper( AllTrim( cAlign ) ) == 'CENTER'
            nxPos -= ( nTextWidth / 2 )
         CASE Upper( AllTrim( cAlign ) ) == 'RIGHT'
            nxPos -= ( nTextWidth )
         ENDCASE
      ELSEIF nAngle == 90
         DO CASE
         CASE Upper( AllTrim( cAlign ) ) == 'CENTER'
            nyPos -= ( nTextWidth / 2 )
         CASE Upper( AllTrim( cAlign ) ) == 'RIGHT'
            nyPos -= ( nTextWidth )
         ENDCASE
      ENDIF
      HPDF_Page_BeginText( _HMG_HPDFDATA[ 1 ][ 7 ] )
      IF nAngle != 0
         nRad := nAngle / 180 * 3.141592 /* Calcurate the radian value. */
         HPDF_Page_SetTextMatrix( _HMG_HPDFDATA[ 1 ][ 7 ], cos( nRad ), sin( nRad ), -sin( nRad ), cos( nRad ), nxPos, nyPos )
      ENDIF
      HPDF_PAGE_TEXTOUT( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, cText )
      HPDF_Page_EndText( _HMG_HPDFDATA[ 1 ][ 7 ] )
      IF lStrikeout .OR. lUnderline
         IF lUnderline // simulate underline
            _HMG_HPDF_LINE( _HMG_HPDF_Pixel2MM( nHeight - nyPos ), _HMG_HPDF_Pixel2MM( nxPos ), _HMG_HPDF_Pixel2MM( nHeight - nyPos + ( nTextWidth * -sin( nRad ) ) ), _HMG_HPDF_Pixel2MM( nxPos + ( nTextWidth * cos( nRad ) ) ), _HMG_HPDF_Pixel2MM( .5 ), nRColor, nGColor, nBColor, .T., lColor )
         ENDIF
         IF lStrikeout // simulate strikeout
            nyPos += ( ( nFontSize / 2.5 ) * cos( nRad ) )
            nxPos -= ( ( nFontSize / 2.5 ) * sin( nRad ) )
            _HMG_HPDF_LINE( _HMG_HPDF_Pixel2MM( nHeight - nyPos ), _HMG_HPDF_Pixel2MM( nxPos ), _HMG_HPDF_Pixel2MM( nHeight - nyPos + ( nTextWidth * -sin( nRad ) ) ), _HMG_HPDF_Pixel2MM( nxPos + ( nTextWidth * cos( nRad ) ) ), _HMG_HPDF_Pixel2MM( .5 ), nRColor, nGColor, nBColor, .T., lColor )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

#define bbox_left   1
#define bbox_top    2
#define bbox_right  3
#define bbox_bottom 4

FUNCTION _HMG_HPDF_MULTILINE_PRINT ( nRow, nCol, nToRow, nToCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lUnderline, lStrikeout, lColor, lFont, lSize, cAlign, nAngle, lWrap, cFit, xVariable )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nToxPos := nxPos + _HMG_HPDF_MM2Pixel( nToCol - nCol )
   LOCAL nToyPos := nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow )
   LOCAL oFont := NIL
   LOCAL cFont
   LOCAL nRad
   LOCAL cTextIn, cTextOut, cLine, nLineSpacingPrev, nLineSpacing, nLineCount, nLineLen, bbox, nAreaHeight, nxLine

   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lUnderline := .F.
   DEFAULT lStrikeout := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT nAngle := 0
   DEFAULT lWrap := .F.
   DEFAULT cFit := ''

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )

   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font

      cFont := HPDF_LoadTTFontFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF
   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ELSE
      IF nAngle != 0
         nRad := nAngle / 180 * 3.141592 /* Calcurate the radian value. */
         HPDF_Page_GSave ( _HMG_HPDFDATA[ 1 ][ 7 ] )
         HPDF_Page_Concat ( _HMG_HPDFDATA[ 1 ][ 7 ], cos( nRad ), sin( nRad ), -sin( nRad ), cos( nRad ), nxPos, nyPos )

         HPDF_Page_BeginText ( _HMG_HPDFDATA[ 1 ][ 7 ] )
         HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
         DO CASE
         CASE Upper( cAlign ) == 'CENTER'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], 0, 0, nToxPos - nxPos, nToyPos - nyPos, cText, HPDF_TALIGN_CENTER, NIL )
         CASE Upper( cAlign ) == 'RIGHT'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], 0, 0, nToxPos - nxPos, nToyPos - nyPos, cText, HPDF_TALIGN_RIGHT, NIL )
         CASE Upper( cAlign ) == 'JUSTIFY'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], 0, 0, nToxPos - nxPos, nToyPos - nyPos, cText, HPDF_TALIGN_JUSTIFY, NIL )
         OTHERWISE
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], 0, 0, nToxPos - nxPos, nToyPos - nyPos, cText, HPDF_TALIGN_LEFT, NIL )
         ENDCASE
         HPDF_Page_EndText ( _HMG_HPDFDATA[ 1 ][ 7 ] )
         HPDF_Page_GRestore ( _HMG_HPDFDATA[ 1 ][ 7 ] )
      ELSE
         nLineSpacingPrev := HPDF_Page_GetTextLeading( _HMG_HPDFDATA[ 1 ][ 7 ] )
         nLineSpacing := nLineSpacingPrev
         bbox := HPDF_Font_GetBBox ( oFont )
         IF nLineSpacing == 0 // set default Line spacing
            nLineSpacing := ( bbox[ bbox_top ] - bbox[ bbox_bottom ] ) / 1000 * nFontSize
         ENDIF

         DO WHILE .T. // Fitting text
            HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
            HPDF_Page_SetTextLeading( _HMG_HPDFDATA[ 1 ][ 7 ], nLineSpacing )

            cTextIn := cText
            cTextOut := ""
            nLineCount := 0

            DO WHILE ! Empty( cTextIn )
               nLineLen := HPDF_Page_MeasureText( _HMG_HPDFDATA[ 1 ][ 7 ], cTextIn, nToxPos - nxPos, ! lWrap, NIL )
               IF nLineLen = 0
                  EXIT
               ENDIF
               nLineCount++
               cLine := Trim( hb_USubStr( cTextIn, 1, nLineLen ) )
               cTextIn := hb_USubStr( cTextIn, nLineLen + 1 )
               cTextOut += cLine + IF( ! Empty( cTextIn ), " ", "" )
            ENDDO

            nAreaHeight := ( ( nLineCount - 1 ) * nLineSpacing /* Lines: 1 .. Last-1. The height of each line equals the line spacing. */ ) + ;
            ( ( bbox[ bbox_top ] - bbox[ bbox_bottom ] ) / 1000 * nFontSize /* The height of the last line equals the default line spacing for this font */ ) + .01 /* area higher than calculated (floating point and HPDF_Page_TextRect issue) */

            IF !( Upper( cFit ) == 'FONTSIZEFIT' ) /* Don't fit font size */ .OR. ;
               nAreaHeight <= nyPos - nToyPos /* Fits */ .OR. ;
               nFontSize <= 4 /* Abandon further reducing the font */
               EXIT
            ENDIF
            nFontSize -= 0.1 // decrease font size
            nLineSpacing := ( bbox[ bbox_top ] - bbox[ bbox_bottom ] ) / 1000 * nFontSize // decrease line spacing
         ENDDO

         cText := cTextOut

         IF Upper( cFit ) == 'HEIGHTFIT'
            nToyPos := nyPos - nAreaHeight
         ENDIF

         xVariable := Round( _HMG_HPDF_Pixel2MM ( nyPos - nToyPos ) + nRow, 1 ) // Return the bottom row "TO nToRow" in mm as a variable by reference.

         HPDF_Page_BeginText( _HMG_HPDFDATA[ 1 ][ 7 ] )
         HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
         DO CASE
         CASE Upper( cAlign ) == 'CENTER'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, nToxPos, nToyPos, cText, HPDF_TALIGN_CENTER, NIL )
         CASE Upper( cAlign ) == 'RIGHT'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, nToxPos, nToyPos, cText, HPDF_TALIGN_RIGHT, NIL )
         CASE Upper( cAlign ) == 'JUSTIFY'
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, nToxPos, nToyPos, cText, HPDF_TALIGN_JUSTIFY, NIL )
         OTHERWISE
            HPDF_Page_TextRect( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, nToxPos, nToyPos, cText, HPDF_TALIGN_LEFT, NIL )
         ENDCASE
         HPDF_Page_EndText( _HMG_HPDFDATA[ 1 ][ 7 ] )

         // restore previous line spacing
         HPDF_Page_SetTextLeading( _HMG_HPDFDATA[ 1 ][ 7 ], nLineSpacingPrev )
         IF lUnderline .OR. lStrikeout
            nyPos -= bbox[ bbox_top ] / 1000 * nFontSize - IF ( lStrikeout, ( nFontSize / 2.5 ), 0 )
            DO WHILE ! Empty( cText )
               nLineLen := HPDF_Page_MeasureText( _HMG_HPDFDATA[ 1 ][ 7 ], cText, nToxPos - nxPos, ! lWrap, NIL )
               IF nLineLen = 0
                  EXIT
               ENDIF
               cLine := Trim( hb_USubStr( cText, 1, nLineLen ) )
               cText := hb_USubStr( cText, nLineLen + 1 )
               nLineLen := HPDF_Page_TextWidth( _HMG_HPDFDATA[ 1 ][ 7 ], cLine )
               nxLine := 0
               DO CASE
               CASE Upper( cAlign ) == 'CENTER'
                  nxLine := ( nToxpos - nxPos - nLineLen ) / 2
               CASE Upper( cAlign ) == 'RIGHT'
                  nxLine := nToxpos - nxPos - nLineLen
               CASE Upper( cAlign ) == 'JUSTIFY'
                  IF ! Empty( cText )
                     nLineLen := nToxpos - nxPos
                  ENDIF
               ENDCASE

               // simulate underline / strikeout
               _HMG_HPDF_LINE ( _HMG_HPDF_Pixel2MM( nHeight - nyPos ), ;
                  _HMG_HPDF_Pixel2MM( nxPos + nxLine ), ;
                  _HMG_HPDF_Pixel2MM( nHeight - nyPos ), ;
                  _HMG_HPDF_Pixel2MM( nxPos + nxLine + nLineLen ), ;
                  _HMG_HPDF_Pixel2MM( .5 ), ;
                  nRColor, nGColor, nBColor, .T., lColor )

               nyPos -= nLineSpacing

            ENDDO
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_GetHeight_MULTILINE_PRINT ( cText, nLen, cFontName, nFontSize, lBold, lItalic, lWrap )

   LOCAL oFont
   LOCAL cFont
   LOCAL nLineSpacing, nLineCount, nLineLen, bbox, nAreaHeight

   DEFAULT cText := ''
   DEFAULT nLen := 0
   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lWrap := .F.

   nLen := _HMG_HPDF_MM2Pixel ( nLen )

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN 0
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN 0
   ENDIF

   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )

   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font

      cFont := HPDF_LOADTTFONTFROMFILE( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN 0
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN 0
   ELSE
      nLineSpacing := HPDF_Page_GetTextLeading( _HMG_HPDFDATA[ 1 ][ 7 ] )
      bbox := HPDF_Font_GetBBox ( oFont )
      IF nLineSpacing == 0 // set default Linespacing, based on: hpdfpago.c -> HPDF_Page_TextRect
         nLineSpacing := ( bbox[ bbox_top ] - bbox[ bbox_bottom ] ) / 1000 * nFontSize
      ENDIF

      HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
      nLineCount := 0

      DO WHILE ! Empty( cText )
         nLineLen := HPDF_Page_MeasureText( _HMG_HPDFDATA[ 1 ][ 7 ], cText, nLen, ! lWrap, NIL )
         IF nLineLen = 0
            EXIT
         ENDIF
         nLineCount++
         cText := hb_USubStr( cText, nLineLen + 1 )
      ENDDO
   ENDIF
   nAreaHeight := ( ( nLineCount - 1 ) * nLineSpacing /* Lines: 1 .. Last-1. The height of each line equals the line spacing */ ) + ;
   ( ( bbox[ bbox_top ] - bbox[ bbox_bottom ] ) / 1000 * nFontSize /* The height of the last line equals the default line spacing for this font */ ) + .01 /* area higher than calculated (floating point and HPDF_Page_TextRect issue) */

RETURN _HMG_HPDF_Pixel2MM ( nAreaHeight )

FUNCTION _HMG_HPDF_IMAGE ( cImage, nRow, nCol, nImageheight, nImageWidth, lStretch )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL oImage, cExt

   DEFAULT lStretch := .F.
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF
   IF File( cImage )
      hb_FNameSplit( cImage, , , @cExt )
      IF Upper( cExt ) == '.PNG'
         oImage := HPDF_LoadPngImageFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cImage )
      ELSE
         oImage := HPDF_LoadJPEGImageFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cImage )
      ENDIF
   ELSE
      _HMG_HPDF_Error( 7 )
      RETURN NIL
   ENDIF
   IF Empty( oImage )
      _HMG_HPDF_Error( 7 )
      RETURN NIL
   ENDIF
   HPDF_Page_DrawImage( _HMG_HPDFDATA[ 1 ][ 7 ], oImage, nxPos, nyPos - _HMG_HPDF_MM2Pixel( nImageHeight ), _HMG_HPDF_MM2Pixel( nImageWidth ), _HMG_HPDF_MM2Pixel( nImageHeight ) )

RETURN NIL

FUNCTION _HMG_HPDF_LINE ( nRow, nCol, nToRow, nToCol, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor, style )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nToxPos := nxPos + _HMG_HPDF_MM2Pixel( nToCol - nCol )
   LOCAL nToyPos := nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow )

   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   IF ValType( style ) != 'U'
      DO CASE
      CASE Upper( style ) == 'HPDFDOTTED'
         _HMG_HPDF_SetDash( 1 )
      CASE Upper( style ) == 'HPDFDASHED'
         _HMG_HPDF_SetDash( 2 )
      CASE Upper( style ) == 'HPDFDASHDOT'
         _HMG_HPDF_SetDash( 3 )
      ENDCASE
   ENDIF
   HPDF_Page_MoveTo( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos )
   HPDF_Page_LineTo( _HMG_HPDFDATA[ 1 ][ 7 ], nToxPos, nToyPos )
   HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   IF ValType( style ) != 'U'
      _HMG_HPDF_SetDash( 0 )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_RECTANGLE ( nRow, nCol, nToRow, nToCol, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor, lFilled )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.
   DEFAULT lFilled := .F.

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      IF lFilled
         HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      ENDIF
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
      IF lFilled
         HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
      ENDIF
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   HPDF_Page_Rectangle( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow ), _HMG_HPDF_MM2Pixel( nToCol - nCol ), _HMG_HPDF_MM2Pixel( nToRow - nRow ) )
   IF lFilled
      HPDF_Page_FillStroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ELSE
      HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_ROUNDRECTANGLE ( nRow, nCol, nToRow, nToCol, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor, lFilled, nRoundness )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nToxPos := nxPos + _HMG_HPDF_MM2Pixel( nToCol - nCol )
   LOCAL nToyPos := nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow )
   LOCAL nRectWidth := nToCol - nCol
   LOCAL nRectHeight := nToRow - nRow

   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.
   DEFAULT lFilled := .F.
   DEFAULT nRoundness := 0
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF
   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      IF lFilled
         HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      ENDIF
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF
   // set line Width
   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
      IF lFilled
         HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
      ENDIF
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   IF nRoundness == 0
      nRoundness := Int( Min( nRectWidth, nRectHeight ) * 10 / 100 )
   ELSE
      IF nRoundness > Int( Min( nRectWidth, nRectHeight ) / 2 )
         _HMG_HPDF_Error( 8 )
         RETURN NIL
      ENDIF
   ENDIF

   nRoundness := _HMG_HPDF_MM2Pixel( nRoundness )

   // start rounded rectangle path
   HPDF_Page_MoveTo( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos + nRoundness, nyPos ) // top line left position
   HPDF_Page_LineTo( _HMG_HPDFDATA[ 1 ][ 7 ], nToxPos - nRoundness, nyPos ) // draw top line
   HPDF_Page_CurveTo2( _HMG_HPDFDATA[ 1 ][ 7 ], nToxPos, nyPos, nToxPos, nyPos - nRoundness ) // draw top right curve
   HPDF_Page_LineTo( _HMG_HPDFDATA[ 1 ][ 7 ], nToxPos, nToyPos + nRoundness ) // draw right line
   HPDF_Page_CurveTo2( _HMG_HPDFDATA[ 1 ][ 7 ], nToxPos, nToyPos, nToxPos - nRoundness, nToyPos ) // draw bottom right curve
   HPDF_Page_LineTo( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos + nRoundness, nToyPos ) // draw bottom line
   HPDF_Page_CurveTo2( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nToyPos, nxPos, nToyPos + nRoundness ) // draw bottom left curve
   HPDF_Page_LineTo( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos - nRoundness ) // draw left line
   HPDF_Page_CurveTo2( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, nxPos + nRoundness, nyPos ) // draw top left curve

   // now start and finish drawing along the path
   IF lFilled
      HPDF_Page_FillStroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ELSE
      HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_CIRCLE( nRow, nCol, nR, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor, lFilled )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.
   DEFAULT lFilled := .F.
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      IF lFilled
         HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      ENDIF
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF
   // set line Width
   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   HPDF_Page_Circle( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, _HMG_HPDF_MM2Pixel( nR ) )
   IF lFilled
      HPDF_Page_FillStroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ELSE
      HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_ELLIPSE( nRow, nCol, nHR, nVR, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor, lFilled )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.
   DEFAULT lFilled := .F.
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      IF lFilled
         HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
      ENDIF
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF
   // set line Width
   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   HPDF_Page_Ellipse( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, _HMG_HPDF_MM2Pixel( nHR ), _HMG_HPDF_MM2Pixel( nVR ) )
   IF lFilled
      HPDF_Page_FillStroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ELSE
      HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_ARC( nRow, nCol, nR, nFromAngle, nToAngle, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF
   // set line Width
   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   HPDF_Page_Arc( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, _HMG_HPDF_MM2Pixel( nR ), nFromAngle, nToAngle )
   HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )

RETURN NIL

FUNCTION _HMG_HPDF_CURVE ( nRow, nCol, nFromRow, nFromCol, nToRow, nToCol, nLineWidth, nRColor, nGColor, nBColor, lWidth, lColor )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nToxPos := nxPos + _HMG_HPDF_MM2Pixel( nToCol - nCol )
   LOCAL nToyPos := nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow )
   LOCAL nFromxPos := _HMG_HPDF_MM2Pixel( nFromCol )
   LOCAL nFromyPos := nHeight - _HMG_HPDF_MM2Pixel( nFromRow )

   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT nLineWidth := 1
   DEFAULT lWidth := .F.
   DEFAULT lColor := .F.

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBSTROKE( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   IF lWidth
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nlineWidth ) )
   ELSE
      HPDF_Page_SetLineWidth( _HMG_HPDFDATA[ 1 ][ 7 ], 1 )
   ENDIF
   HPDF_Page_MoveTo( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos )
   HPDF_Page_CurveTo2( _HMG_HPDFDATA[ 1 ][ 7 ], nFromxPos, nFromyPos, nToxPos, nToyPos )
   HPDF_Page_Stroke( _HMG_HPDFDATA[ 1 ][ 7 ] )

RETURN NIL

FUNCTION _HMG_HPDF_SetPassword( cOwnerPass, cUserPass )

   DEFAULT cUserPass := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   HPDF_SetPassword( _HMG_HPDFDATA[ 1 ][ 1 ], cOwnerPass, cUserPass )

RETURN NIL

FUNCTION _HMG_HPDF_SetCompression( cMode )

   DEFAULT cMode := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   cMode := Upper( cMode )
   DO CASE
   CASE cMode == 'NONE'
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_NONE )
   CASE cMode == 'TEXT'
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_TEXT )
   CASE cMode == 'IMAGE'
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_IMAGE )
   CASE cMode == 'METADATA'
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_METADATA )
   CASE cMode == 'ALL'
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_ALL )
   OTHERWISE
      HPDF_SetCompressionMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_COMP_NONE )
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_SetPermission( cMode )

   DEFAULT cMode := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   cMode := Upper( cMode )
   DO CASE
   CASE cMode == 'READ'
      HPDF_SetPermission( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_ENABLE_READ )
   CASE cMode == 'PRINT'
      HPDF_SetPermission( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_ENABLE_PRINT )
   CASE cMode == 'COPY'
      HPDF_SetPermission( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_ENABLE_COPY )
   CASE cMode == 'EDIT'
      HPDF_SetPermission( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_ENABLE_EDIT )
   CASE cMode == 'EDIT_ALL'
      HPDF_SetPermission( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_ENABLE_EDIT_ALL )
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_SetPageMode( cMode )

   DEFAULT cMode := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   cMode := Upper( cMode )
   DO CASE
   CASE cMode == 'OUTLINE'
      HPDF_SetPageMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_PAGE_MODE_USE_OUTLINE )
   CASE cMode == 'NONE'
      HPDF_SetPageMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_PAGE_MODE_USE_NONE )
   CASE cMode == 'THUMBS'
      HPDF_SetPageMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_PAGE_MODE_USE_THUMBS )
   CASE cMode == 'FULL_SCREEN'
      HPDF_SetPageMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_PAGE_MODE_FULL_SCREEN )
   CASE cMode == 'EOF'
      HPDF_SetPageMode( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_PAGE_MODE_EOF )
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_RootOutline( cTitle, cName, cParent )

   LOCAL aOutlines
   LOCAL oOutline, i

   DEFAULT cParent := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF Len( AllTrim( cTitle ) ) == 0
      _HMG_HPDF_Error( 11 )
      RETURN NIL
   ENDIF
   IF Len( AllTrim( cName ) ) == 0
      _HMG_HPDF_Error( 12 )
      RETURN NIL
   ENDIF
   IF Len( AllTrim( cParent ) ) == 0 // make this a root outline
      oOutline := HPDF_CreateOutline( _HMG_HPDFDATA[ 1 ][ 1 ], NIL, cTitle, HPDF_GetCurrentEncoder() )
      IF Empty( oOutLine )
         _HMG_HPDF_Error( 10 )
         RETURN NIL
      ELSE
         AAdd( _HMG_HPDFDATA[ 1 ][ 11 ], { oOutline, cName } )
      ENDIF
   ELSE
      aOutLines := AClone( _HMG_HPDFDATA[ 1 ][ 11 ] )
      FOR i := 1 TO Len( aOutLines )
         IF Upper( AllTrim( aOutLines[ i, 2 ] ) ) == Upper( AllTrim( cParent ) )
            oOutline := HPDF_CreateOutline( _HMG_HPDFDATA[ 1 ][ 1 ], aOutLines[ i, 1 ], cTitle, HPDF_GetCurrentEncoder() )
            IF Empty( oOutLine )
               _HMG_HPDF_Error( 10 )
               RETURN NIL
            ELSE
               AAdd( _HMG_HPDFDATA[ 1 ][ 11 ], { oOutline, cName } )
            ENDIF
            EXIT
         ENDIF
      NEXT i
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_PageOutline( cTitle, cParent, cName )

   LOCAL aOutlines
   LOCAL oOutline, i
   LOCAL oDestination

   DEFAULT cParent := ''
   DEFAULT cName := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF
   oDestination := HPDF_Page_CreateDestination( _HMG_HPDFDATA[ 1 ][ 7 ] )
   IF Empty( oDestination )
      _HMG_HPDF_Error( 13 )
      RETURN NIL
   ENDIF
   IF Len( AllTrim( cParent ) ) == 0 // No parent make this page root element
      oOutline := HPDF_CreateOutline( _HMG_HPDFDATA[ 1 ][ 1 ], NIL, cTitle, HPDF_GetCurrentEncoder() )
      IF Empty( oOutline )
         _HMG_HPDF_Error( 13 )
         RETURN NIL
      ENDIF
   ELSE
      aOutlines := AClone( _HMG_HPDFDATA[ 1 ][ 11 ] )
      FOR i := 1 TO Len( aOutlines )
         IF Upper( AllTrim( aOutlines[ i, 2 ] ) ) == Upper( AllTrim( cParent ) )
            oOutline := HPDF_CreateOutline( _HMG_HPDFDATA[ 1 ][ 1 ], aOutlines[ i, 1 ], cTitle, HPDF_GetCurrentEncoder() )
            EXIT
         ENDIF
      NEXT i
      IF Empty( oOutline )
         _HMG_HPDF_Error( 14 )
         RETURN NIL
      ENDIF
   ENDIF
   IF Len( AllTrim( cName ) ) > 0
      AAdd( _HMG_HPDFDATA[ 1 ][ 11 ], { oOutline, cName } )
   ENDIF
   HPDF_Outline_SetDestination( oOutline, oDestination )

RETURN NIL

FUNCTION _HMG_HPDF_SetTextAnnot( nRow, nCol, cToolTip, cIcon )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nToRow := nRow + 5
   LOCAL nToCol := nCol + 5
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nToxPos := nxPos + _HMG_HPDF_MM2Pixel( nToCol - nCol )
   LOCAL nToyPos := nyPos - _HMG_HPDF_MM2Pixel( nToRow - nRow )
   LOCAL aRect := { nxPos, nyPos, nToxPos, nToyPos }
   LOCAL oAnnot := NIL

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF
   oAnnot := HPDF_Page_CreateTextAnnot ( _HMG_HPDFDATA[ 1 ][ 7 ], aRect, cTooltip, HPDF_GetCurrentEncoder() )
   IF Empty( oAnnot )
      _HMG_HPDF_Error( 15 )
      RETURN NIL
   ENDIF
   DO CASE
   CASE Upper( AllTrim( cIcon ) ) == "COMMENT"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_COMMENT )
   CASE Upper( AllTrim( cIcon ) ) == "KEY"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_KEY )
   CASE Upper( AllTrim( cIcon ) ) == "NOTE"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_NOTE )
   CASE Upper( AllTrim( cIcon ) ) == "HELP"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_HELP )
   CASE Upper( AllTrim( cIcon ) ) == "NEW_PARAGRAPH"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_NEW_PARAGRAPH )
   CASE Upper( AllTrim( cIcon ) ) == "PARAGRAPH"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_PARAGRAPH )
   CASE Upper( AllTrim( cIcon ) ) == "INSERT"
      HPDF_TextAnnot_SetIcon ( oAnnot, HPDF_ANNOT_ICON_INSERT )
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_SetPageLink( nRow, nCol, cText, nPage, cFontName, nFontSize, nRColor, nGColor, nBColor, cAlign, lColor, lFont, lSize, lBorder, lWidth, nBorderWidth )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nTextWidth
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL oFont := NIL
   LOCAL cFont
   LOCAL aRect
   LOCAL oAnnot := NIL
   LOCAL oDest := NIL
   LOCAL aPages
   LOCAL oLinkPage := NIL

   DEFAULT cFontName := ''
   DEFAULT nFontSize := 12
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT lBorder := .F.
   DEFAULT lWidth := .F.
   DEFAULT nBorderWidth := 0

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   aPages := AClone( _HMG_HPDFDATA[ 1 ][ 12 ] )
   IF nPage > Len( aPages ) .OR. nPage < 1
      _HMG_HPDF_Error( 17 )
      RETURN NIL
   ENDIF

   oLinkPage := aPages[ nPage ]
   IF Empty( oLinkPage )
      _HMG_HPDF_Error( 17 )
      RETURN NIL
   ENDIF
   oDest := HPDF_Page_CreateDestination( oLinkPage )
   IF Empty( oDest )
      _HMG_HPDF_Error( 17 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName ) )

   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font

      cFont := HPDF_LoadTTFontFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF
   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ELSE
      HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
      nTextWidth := HPDF_Page_TextWidth( _HMG_HPDFDATA[ 1 ][ 7 ], cText )
      DO CASE
      CASE Upper( AllTrim( cAlign ) ) == 'CENTER'
         nxPos := nxPos - ( nTextWidth / 2 )
      CASE Upper( AllTrim( cAlign ) ) == 'RIGHT'
         nxPos := nxPos - ( nTextWidth )
      ENDCASE
      HPDF_Page_BeginText( _HMG_HPDFDATA[ 1 ][ 7 ] )
      HPDF_PAGE_TEXTOUT( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, cText )
      aRect := { nxPos - 4, nyPos + nFontsize + 4, nxPos + nTextWidth + 4, nyPos - 4 }
      oAnnot := HPDF_Page_CreateLinkAnnot( _HMG_HPDFDATA[ 1 ][ 7 ], aRect, oDest )
      IF lBorder
         IF nBorderWidth == 0
            nBorderWidth := 1
         ELSE
            nBorderWidth := _HMG_HPDF_MM2Pixel( nBorderWidth )
         ENDIF
         HPDF_LinkAnnot_SetBorderStyle ( oAnnot, nBorderWidth, 0, 0 )
      ELSE
         HPDF_LinkAnnot_SetBorderStyle ( oAnnot, 0, 0, 0 )
      ENDIF
      HPDF_Page_EndText( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_SetURLLink( nRow, nCol, cText, cLink, cFontName, nFontSize, nRColor, nGColor, nBColor, cAlign, lColor, lFont, lSize )

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nTextWidth
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL oFont := NIL
   LOCAL cFont
   LOCAL aRect
   LOCAL oAnnot := NIL
   LOCAL hUrlLink := NIL

   DEFAULT cFontName := ''
   DEFAULT nFontSize := 12
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( _HMG_HPDFDATA[ 1 ][ 7 ], 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName ) )

   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font

      cFont := HPDF_LoadTTFontFromFile( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF
   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ELSE
      HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
      nTextWidth := HPDF_Page_TextWidth( _HMG_HPDFDATA[ 1 ][ 7 ], cText )
      DO CASE
      CASE Upper( AllTrim( cAlign ) ) == 'CENTER'
         nxPos := nxPos - ( nTextWidth / 2 )
      CASE Upper( AllTrim( cAlign ) ) == 'RIGHT'
         nxPos := nxPos - ( nTextWidth )
      ENDCASE
      HPDF_Page_BeginText( _HMG_HPDFDATA[ 1 ][ 7 ] )
      HPDF_PAGE_TEXTOUT( _HMG_HPDFDATA[ 1 ][ 7 ], nxPos, nyPos, cText )
      aRect := { nxPos - 4, nyPos + nFontsize + 4, nxPos + nTextWidth + 4, nyPos - 4 }
      hUrlLink := HPDF_Page_CreateURILinkAnnot( _HMG_HPDFDATA[ 1 ][ 7 ], aRect, cLink )
      IF hUrlLink != NIL
         HPDF_Annotation_SetBorderStyle( hUrlLink, HPDF_BS_UNDERLINED )
      ENDIF
      HPDF_Page_EndText( _HMG_HPDFDATA[ 1 ][ 7 ] )
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_SetLineSpacing( nSpacing )

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF
   HPDF_Page_SetTextLeading( _HMG_HPDFDATA[ 1 ][ 7 ], _HMG_HPDF_MM2Pixel( nSpacing ) )

RETURN NIL

FUNCTION _HMG_HPDF_SetInfo( cAttrib, xValue, cTime )

   LOCAL cValue := ''
   LOCAL dValue := ''
   LOCAL nHours := 0
   LOCAL nMinutes := 0
   LOCAL nSeconds := 0

   DEFAULT cTime := ''
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   DO CASE
   CASE ValType( xValue ) == 'D'
      dValue := xValue
   OTHERWISE
      cValue := xValue
   ENDCASE
   DO CASE
   CASE Upper( AllTrim( cAttrib ) ) == "AUTHOR"
      HPDF_SetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_AUTHOR, cValue )
   CASE Upper( AllTrim( cAttrib ) ) == "CREATOR"
      HPDF_SetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_CREATOR, cValue )
   CASE Upper( AllTrim( cAttrib ) ) == "TITLE"
      HPDF_SetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_TITLE, cValue )
   CASE Upper( AllTrim( cAttrib ) ) == "SUBJECT"
      HPDF_SetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_SUBJECT, cValue )
   CASE Upper( AllTrim( cAttrib ) ) == "KEYWORDS"
      HPDF_SetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_KEYWORDS, cValue )
   CASE Upper( AllTrim( cAttrib ) ) == "DATECREATED"
      IF Len( AllTrim( cTime ) ) > 0
         nHours := Val( SubStr( cTime, 1, 2 ) )
         nMinutes := Val( SubStr( cTime, 4, 2 ) )
         nSeconds := Val( SubStr( cTime, 7, 2 ) )
      ENDIF
      HPDF_SetInfoDateAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_CREATION_DATE, { Year( dValue ), Month( dVAlue ), Day( dValue ), nHours, nMinutes, nSeconds } )
   CASE Upper( AllTrim( cAttrib ) ) == "DATEMODIFIED"
      IF Len( AllTrim( cTime ) ) > 0
         nHours := Val( SubStr( cTime, 1, 2 ) )
         nMinutes := Val( SubStr( cTime, 4, 2 ) )
         nSeconds := Val( SubStr( cTime, 7, 2 ) )
      ENDIF
      HPDF_SetInfoDateAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_MOD_DATE, { Year( dValue ), Month( dVAlue ), Day( dValue ), nHours, nMinutes, nSeconds } )
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_GetInfo( cAttrib )

   LOCAL cRetValue := '', bd, bc

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   DO CASE
   CASE Upper( AllTrim( cAttrib ) ) == "AUTHOR"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_AUTHOR )
   CASE Upper( AllTrim( cAttrib ) ) == "CREATOR"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_CREATOR )
   CASE Upper( AllTrim( cAttrib ) ) == "TITLE"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_TITLE )
   CASE Upper( AllTrim( cAttrib ) ) == "SUBJECT"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_SUBJECT )
   CASE Upper( AllTrim( cAttrib ) ) == "KEYWORDS"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_KEYWORDS )
   CASE Upper( AllTrim( cAttrib ) ) == "DATECREATED"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_CREATION_DATE )
   CASE Upper( AllTrim( cAttrib ) ) == "DATEMODIFIED"
      cRetValue := HPDF_GetInfoAttr( _HMG_HPDFDATA[ 1 ][ 1 ], HPDF_INFO_MOD_DATE )
   ENDCASE
   IF Left( cRetValue, 2 ) == "D:"
      bd := Set( _SET_DATEFORMAT )
      bc := __SetCentury( .T. )
      SET DATE GERMAN
      cRetValue := DToC( SToD( SubStr( cRetValue, 3, 8 ) ) ) + " " + SubStr( cRetValue, 11, 2 ) + ":" + SubStr( cRetValue, 13, 2 ) + ":" + SubStr( cRetValue, 15, 2 )
      __SetCentury( bc )
      Set( _SET_DATEFORMAT, bd )
   ENDIF

RETURN cRetValue

FUNCTION _HMG_HPDF_SetPageLabel( nPage, cStyle, cCase, cPrefix )

   DEFAULT cCase := ""
   DEFAULT cStyle := "DECIMAL"
   DEFAULT nPage := 1
   DEFAULT cPrefix := ""
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   DO CASE
   CASE Upper( AllTrim( cStyle ) ) == "DECIMAL"
      HPDF_AddPageLabel( _HMG_HPDFDATA[ 1 ][ 1 ], nPage, HPDF_PAGE_NUM_STYLE_DECIMAL, nPage, cPrefix )
   CASE Upper( AllTrim( cStyle ) ) == "ROMAN"
      IF Upper( AllTrim( cCase ) ) == "LOWER"
         HPDF_AddPageLabel( _HMG_HPDFDATA[ 1 ][ 1 ], nPage, HPDF_PAGE_NUM_STYLE_LOWER_ROMAN, nPage, cPrefix )
      ELSE
         HPDF_AddPageLabel( _HMG_HPDFDATA[ 1 ][ 1 ], nPage, HPDF_PAGE_NUM_STYLE_UPPER_ROMAN, nPage, cPrefix )
      ENDIF
   CASE Upper( AllTrim( cStyle ) ) == "LETTERS"
      IF Upper( AllTrim( cCase ) ) == "LOWER"
         HPDF_AddPageLabel( _HMG_HPDFDATA[ 1 ][ 1 ], nPage, HPDF_PAGE_NUM_STYLE_LOWER_LETTERS, nPage, cPrefix )
      ELSE
         HPDF_AddPageLabel( _HMG_HPDFDATA[ 1 ][ 1 ], nPage, HPDF_PAGE_NUM_STYLE_UPPER_LETTERS, nPage, cPrefix )
      ENDIF
   ENDCASE

RETURN NIL

FUNCTION _HMG_HPDF_Error( nErr, cExtraInfo )

   LOCAL cErr := ''

   DEFAULT cExtraInfo TO ''
   DO CASE
   CASE nErr == 1
      cErr := 'A PDF document is already initiated.'
   CASE nErr == 2
      cErr := 'PDF document cannot be initialized.'
   CASE nErr == 3
      cErr := 'PDF PAGE cannot be added. PDF document cannot be found.'
   CASE nErr == 4
      cErr := 'A PDF PAGE is already initiated.'
   CASE nErr == 5
      cErr := 'PDF PAGE cannot be found.'
   CASE nErr == 6
      cErr := 'Font cannot be loaded.'
   CASE nErr == 7
      cErr := 'Image cannot be loaded.'
   CASE nErr == 8
      cErr := 'Round curve can not be longer than half of the width/height.'
   CASE nErr == 9
      cErr := 'Can not start new page.'
   CASE nErr == 10
      cErr := 'Can not create new parent outline.'
   CASE nErr == 11
      cErr := 'Outline Title is not valid.'
   CASE nErr == 12
      cErr := 'Outline Name is not valid.'
   CASE nErr == 13
      cErr := 'Can not create Page Outline.'
   CASE nErr == 14
      cErr := 'Can not find Parent Node.'
   CASE nErr == 15
      cErr := 'Tooltip creation error!'
   CASE nErr == 16
      cErr := 'Page not found. New page can not be inserted.'
   CASE nErr == 17
      cErr := 'Link Page not found.'
   CASE nErr == 18
      cErr := 'Selected Page can not be found.'
   CASE nErr == 19
      cErr := 'Page Selection error!'
   CASE nErr == 20
      cErr := 'Pdf creation error!'
   ENDCASE
   cErr += CRLF + cExtraInfo
   msgstop( cErr )

RETURN NIL

FUNCTION _HMG_HPDF_Inch2Pixel( nInches )
RETURN ( nInches * 72 )

FUNCTION _HMG_HPDF_Pixel2Inch( nPixels )
RETURN ( nPixels / 72 )

FUNCTION _HMG_HPDF_MM2Pixel( nMM )
RETURN ( ( nMM / 25.4 ) * 72 )

FUNCTION _HMG_HPDF_Pixel2MM( nPixel )
RETURN ( ( nPixel / 72 ) * 25.4 )

FUNCTION _HMG_HPDF_INIT_PAPERSIZE( nPaperSize )

   LOCAL nTemp

   DO CASE
   CASE nPaperSize == 1 .OR. nPaperSize == -999 // HPDF_PAPER_LETTER or default
      _HMG_HPDFDATA[ 1 ][ 4 ] := 612
      _HMG_HPDFDATA[ 1 ][ 5 ] := 792
   CASE nPaperSize == 2 // HPDF_PAPER_LETTERSMALL
      _HMG_HPDFDATA[ 1 ][ 4 ] := 612
      _HMG_HPDFDATA[ 1 ][ 5 ] := 792
   CASE nPaperSize == 3 // HPDF_PAPER_TABLOID
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 11 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 17 )
   CASE nPaperSize == 4 // HPDF_PAPER_LEDGER
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 17 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 5 // HPDF_PAPER_LEGAL
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 14 )
   CASE nPaperSize == 6 // HPDF_PAPER_STATEMENT
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 5.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
   CASE nPaperSize == 7 // HPDF_PAPER_EXECUTIVE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 5.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
   CASE nPaperSize == 8 // HPDF_PAPER_A3
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 297 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 420 )
   CASE nPaperSize == 9 // HPDF_PAPER_A4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 10 // HPDF_PAPER_A4SMALL
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 11 // HPDF_PAPER_A5
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 148 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 210 )
   CASE nPaperSize == 12 // HPDF_PAPER_B4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 250 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 354 )
   CASE nPaperSize == 13 // HPDF_PAPER_B5
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 185 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 257 )
   CASE nPaperSize == 14 // HPDF_PAPER_FOLIO
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 13 )
   CASE nPaperSize == 15 // HPDF_PAPER_QUARTO
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 215 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 275 )
   CASE nPaperSize == 16 // HPDF_PAPER_10X14
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 10 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 140 )
   CASE nPaperSize == 17 // HPDF_PAPER_11X17
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 11 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 17 )
   CASE nPaperSize == 18 // HPDF_PAPER_NOTE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 19 // HPDF_PAPER_ENV_9
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 3 + ( 7 / 8 ) )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 8 + ( 7 / 8 ) )
   CASE nPaperSize == 20 // HPDF_PAPER_ENV_10
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 4 + ( 1 / 8 ) )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 9.5 )
   CASE nPaperSize == 21 // HPDF_PAPER_ENV_11
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 4.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 10 + ( 3 / 8 ) )
   CASE nPaperSize == 22 // HPDF_PAPER_ENV_12
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 4.75 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 23 // HPDF_PAPER_ENV_14
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11.5 )
   CASE nPaperSize == 24 // HPDF_PAPER_CSHEET
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 17 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 22 )
   CASE nPaperSize == 25 // HPDF_PAPER_DSHEET
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 22 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 34 )
   CASE nPaperSize == 26 // HPDF_PAPER_ESHEET
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 34 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 44 )
   CASE nPaperSize == 27 // HPDF_PAPER_ENV_DL
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 110 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 220 )
   CASE nPaperSize == 28 // HPDF_PAPER_ENV_C5
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 162 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 229 )
   CASE nPaperSize == 29 // HPDF_PAPER_ENV_C3
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 324 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 458 )
   CASE nPaperSize == 30 // HPDF_PAPER_ENV_C4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 229 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 324 )
   CASE nPaperSize == 31 // HPDF_PAPER_ENV_C6
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 114 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 162 )
   CASE nPaperSize == 32 // HPDF_PAPER_ENV_C65
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 114 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 229 )
   CASE nPaperSize == 33 // HPDF_PAPER_ENV_B4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 250 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 353 )
   CASE nPaperSize == 34 // HPDF_PAPER_ENV_B5
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 176 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 250 )
   CASE nPaperSize == 35 // HPDF_PAPER_ENV_B6
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 176 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 125 )
   CASE nPaperSize == 36 // HPDF_PAPER_ENV_ITALY
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 110 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 230 )
   CASE nPaperSize == 37 // HPDF_PAPER_ENV_MONARCH
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 3.875 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 7.5 )
   CASE nPaperSize == 38 // HPDF_PAPER_ENV_PERSONAL
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 3 + ( 5 / 8 ) )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 6.5 )
   CASE nPaperSize == 39 // HPDF_PAPER_FANFOLD_US
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 14 + ( 7 / 8 ) )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 40 // HPDF_PAPER_FANFOLD_STD_GERMAN
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 12 )
   CASE nPaperSize == 41 // HPDF_PAPER_FANFOLD_LGL_GERMAN
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 13 )
   CASE nPaperSize == 42 // HPDF_PAPER_ISO_B4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 250 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 353 )
   CASE nPaperSize == 43 // HPDF_PAPER_JAPANESE_POSTCARD
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 100 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 148 )
   CASE nPaperSize == 44 // HPDF_PAPER_9X11
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 9 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 45 // HPDF_PAPER_10X11
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 10 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 46 // HPDF_PAPER_15X11
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 15 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 47 // HPDF_PAPER_ENV_INVITE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 220 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 220 )
   CASE nPaperSize == 48 // HPDF_PAPER_RESERVED_48
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 49 // HPDF_PAPER_RESERVED_49
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 50 // HPDF_PAPER_LETTER_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 9.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 12 )
   CASE nPaperSize == 51 // HPDF_PAPER_LEGAL_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 9.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 15 )
   CASE nPaperSize == 52 // HPDF_PAPER_TABLOID_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 11.69 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 18 )
   CASE nPaperSize == 53 // HPDF_PAPER_A4_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 9.27 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 12.69 )
   CASE nPaperSize == 54 // HPDF_PAPER_LETTER_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 55 // HPDF_PAPER_A4_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 56 // HPDF_PAPER_LETTER_EXTRA_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 9.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 12 )
   CASE nPaperSize == 57 // HPDF_PAPER_A_PLUS
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 227 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 356 )
   CASE nPaperSize == 58 // HPDF_PAPER_B_PLUS
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 305 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 487 )
   CASE nPaperSize == 59 // HPDF_PAPER_LETTER_PLUS
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 12.69 )
   CASE nPaperSize == 60 // HPDF_PAPER_A4_PLUS
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 330 )
   CASE nPaperSize == 61 // HPDF_PAPER_A5_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 148 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 210 )
   CASE nPaperSize == 62 // HPDF_PAPER_B5_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 182 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 257 )
   CASE nPaperSize == 63 // HPDF_PAPER_A3_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 322 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 445 )
   CASE nPaperSize == 64 // HPDF_PAPER_A5_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 174 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 235 )
   CASE nPaperSize == 65 // HPDF_PAPER_B5_EXTRA
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 201 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 276 )
   CASE nPaperSize == 66 // HPDF_PAPER_A2
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 420 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 594 )
   CASE nPaperSize == 67 // HPDF_PAPER_A3_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 297 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 420 )
   CASE nPaperSize == 68 // HPDF_PAPER_A3_EXTRA_TRANSVERSE
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 322 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 445 )
   CASE nPaperSize == 69 // HPDF_PAPER_DBL_JAPANESE_POSTCARD
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 200 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 148 )
   CASE nPaperSize == 70 // HPDF_PAPER_A6
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 105 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 148 )
   CASE nPaperSize == 71 // HPDF_PAPER_JENV_KAKU2
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 240 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 332 )
   CASE nPaperSize == 72 // HPDF_PAPER_JENV_KAKU2
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 216 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 277 )
   CASE nPaperSize == 73 // HPDF_PAPER_JENV_CHOU3
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 120 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 235 )
   CASE nPaperSize == 74 // HPDF_PAPER_JENV_CHOU4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 90 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 205 )
   CASE nPaperSize == 75 // HPDF_PAPER_LETTER_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 11 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 8.5 )
   CASE nPaperSize == 76 // HPDF_PAPER_A3_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 420 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 297 )
   CASE nPaperSize == 77 // HPDF_PAPER_A4_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 297 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 210 )
   CASE nPaperSize == 78 // HPDF_PAPER_A5_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 210 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 148 )
   CASE nPaperSize == 79 // HPDF_PAPER_B4_JIS_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 364 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 257 )
   CASE nPaperSize == 80 // HPDF_PAPER_B5_JIS_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 257 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 182 )
   CASE nPaperSize == 81 // HPDF_PAPER_B5_JAPANESE_POSTCARD_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 148 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 100 )
   CASE nPaperSize == 82 // HPDF_PAPER_B5_DBL_JAPANESE_POSTCARD_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 148 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 200 )
   CASE nPaperSize == 83 // HPDF_PAPER_A6_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 148 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 105 )
   CASE nPaperSize == 84 // HPDF_PAPER_JENV_KAKU2_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 332 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 240 )
   CASE nPaperSize == 85 // HPDF_PAPER_JENV_KAKU3_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 277 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 216 )
   CASE nPaperSize == 86 // HPDF_PAPER_JENV_CHOU3_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 235 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 120 )
   CASE nPaperSize == 87 // HPDF_PAPER_JENV_CHOU4_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 205 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 90 )
   CASE nPaperSize == 88 // HPDF_PAPER_B6_JIS
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 128 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 182 )
   CASE nPaperSize == 89 // HPDF_PAPER_B6_JIS_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 182 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 128 )
   CASE nPaperSize == 90 // HPDF_PAPER_12X11
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_Inch2Pixel( 12 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_Inch2Pixel( 11 )
   CASE nPaperSize == 91 // HPDF_PAPER_JENV_YOU4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 235 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 105 )
   CASE nPaperSize == 92 // HPDF_PAPER_JENV_YOU4_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 105 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 235 )
   CASE nPaperSize == 93 // HPDF_PAPER_P16K
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 146 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 215 )
   CASE nPaperSize == 94 // HPDF_PAPER_P32K
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 97 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 151 )
   CASE nPaperSize == 95 // HPDF_PAPER_P32KBIG
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 97 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 151 )
   CASE nPaperSize == 96 // HPDF_PAPER_PENV_1
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 102 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 165 )
   CASE nPaperSize == 97 // HPDF_PAPER_PENV_2
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 102 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 176 )
   CASE nPaperSize == 98 // HPDF_PAPER_PENV_3
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 125 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 176 )
   CASE nPaperSize == 99 // HPDF_PAPER_PENV_4
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 110 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 208 )
   CASE nPaperSize == 100 // HPDF_PAPER_PENV_5
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 110 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 220 )
   CASE nPaperSize == 101 // HPDF_PAPER_PENV_6
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 120 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 230 )
   CASE nPaperSize == 102 // HPDF_PAPER_PENV_7
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 160 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 230 )
   CASE nPaperSize == 103 // HPDF_PAPER_PENV_8
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 120 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 208 )
   CASE nPaperSize == 104 // HPDF_PAPER_PENV_9
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 229 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 324 )
   CASE nPaperSize == 105 // HPDF_PAPER_PENV_10
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 324 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 458 )
   CASE nPaperSize == 106 // HPDF_PAPER_P16K_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 215 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 146 )
   CASE nPaperSize == 107 // HPDF_PAPER_P32K_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 151 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 97 )
   CASE nPaperSize == 108 // HPDF_PAPER_P32KBIG_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 151 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 97 )
   CASE nPaperSize == 109 // HPDF_PAPER_PENV_1_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 165 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 102 )
   CASE nPaperSize == 110 // HPDF_PAPER_PENV_2_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 176 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 102 )
   CASE nPaperSize == 111 // HPDF_PAPER_PENV_3_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 176 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 125 )
   CASE nPaperSize == 112 // HPDF_PAPER_PENV_4_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 208 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 110 )
   CASE nPaperSize == 113 // HPDF_PAPER_PENV_5_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 220 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 110 )
   CASE nPaperSize == 114 // HPDF_PAPER_PENV_6_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 230 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 120 )
   CASE nPaperSize == 115 // HPDF_PAPER_PENV_7_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 230 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 160 )
   CASE nPaperSize == 116 // HPDF_PAPER_PENV_8_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 309 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 120 )
   CASE nPaperSize == 117 // HPDF_PAPER_PENV_9_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 324 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 229 )
   CASE nPaperSize == 118 // HPDF_PAPER_PENV_10_ROTATED
      _HMG_HPDFDATA[ 1 ][ 4 ] := _HMG_HPDF_MM2Pixel( 458 )
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDF_MM2Pixel( 324 )
   ENDCASE
   IF _HMG_HPDFDATA[ 1 ][ 6 ] == 2 // landscape
      nTemp := _HMG_HPDFDATA[ 1 ][ 5 ]
      _HMG_HPDFDATA[ 1 ][ 5 ] := _HMG_HPDFDATA[ 1 ][ 4 ]
      _HMG_HPDFDATA[ 1 ][ 4 ] := nTemp
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_SetFont( cFntName, lBold, lItalic )

   LOCAL cFntTmp, cFnt, cFntNoExt

   DEFAULT lBold := .F., lItalic := .F.

   IF Len( AllTrim( cFntName ) ) == 0
      cFntName := _HMG_HPDFDATA[ 1 ][ 8 ]
   ENDIF

   IF UPPER ( cFileExt ( cFntName ) ) == '.TTF' // load ttf font
      cFnt := _HMG_HPDF_ExistInLocal( cFntName )
      IF ! Empty( cFnt )
         RETURN cFnt
      ENDIF
   ENDIF

   cFntTmp := cFileNoExt( cFntName )
   cFntNoExt := cFntTmp
   cFnt := _HMG_HPDF_SeekHaruFonts( cFntTmp, lBold, lItalic )
   IF ! Empty( cFnt )
      RETURN cFnt
   ENDIF

   cFnt := _HMG_HPDF_SeekEquivalences( cFntNoExt, lBold, lItalic )
   IF ! Empty( cFnt )
      RETURN cFnt
   ENDIF

   cFnt := _HMG_HPDF_SeekInLocalOptions( cFntNoExt, lBold, lItalic )
   IF ! Empty( cFnt )
      cFntTmp := _HMG_HPDF_ExistInLocal( cFnt )
      IF ! Empty( cFntTmp )
         RETURN cFntTmp
      ENDIF
   ENDIF

RETURN "Helvetica"

FUNCTION _HMG_HPDF_SeekHaruFonts( cFontName, lBold, lItalic )

   LOCAL aHpdf_Font := { "Courier", ;
      "Courier-Bold", ;
      "Courier-Oblique", ;
      "Courier-BoldOblique", ;
      "Helvetica", ;
      "Helvetica-Bold", ;
      "Helvetica-Oblique", ;
      "Helvetica-BoldOblique", ;
      "Times-Roman", ;
      "Times-Bold", ;
      "Times-Italic", ;
      "Times-BoldItalic", ;
      "Symbol", ;
      "ZapfDingbats" }
   LOCAL nPos, cFntHaru

   IF ( nPos := AScan( aHpdf_Font, {| cFnt | Upper( cFnt ) == Upper( cFontName ) } ) ) > 0
      cFntHaru := aHpdf_Font[ nPos ]

      DO CASE
      CASE "SYMBOL" $ Upper( cFntHaru )
         cFntHaru := "Symbol"
      CASE "ZAPFDINGBATS" $ Upper( cFntHaru )
         cFntHaru := "ZapfDingbats"
      CASE Upper( hb_USubStr( cFntHaru, 1, 5 ) ) == 'TIMES'
         cFntHaru := "Times"
         IF lBold .AND. lItalic
            cFntHaru := cFntHaru + '-BoldItalic'
         ELSEIF lBold
            cFntHaru := cFntHaru + '-Bold'
         ELSEIF lItalic
            cFntHaru := cFntHaru + '-Italic'
         ELSE
            cFntHaru := cFntHaru + '-Roman'
         ENDIF
      OTHERWISE
         IF ( nPos := hb_UAt( '-', cFntHaru ) ) > 0
            cFntHaru := hb_USubStr( cFntHaru, 1, nPos - 1 )
         ENDIF
         IF lBold .AND. lItalic
            cFntHaru := cFntHaru + '-BoldOblique'
         ELSEIF lBold
            cFntHaru := cFntHaru + '-Bold'
         ELSEIF lItalic
            cFntHaru := cFntHaru + '-Oblique'
         ENDIF
      ENDCASE
      RETURN cFntHaru
   ENDIF

RETURN ""

FUNCTION _HMG_HPDF_SeekEquivalences( FONTNAME, lBold, lItalic )

   LOCAL cFldWindows := GetSpecialFolder( CSIDL_FONTS )
   LOCAL cRet := ""

   DEFAULT lBold := .F., lItalic := .F.

   DO CASE
   CASE "COUR" $ Upper( FontName )
      cRet := "cour"
   CASE "TIMES" $ Upper( FontName )
      cRet := "times"
   CASE "ARIAL NARROW" $ Upper( FontName )
      cRet := "arialn"
   CASE "ARIAL" $ Upper( FontName )
      cRet := "arial"
   CASE "VERDANA" $ Upper( FontName )
      cRet := "verdana"
   ENDCASE

   IF ! Empty( cRet )
      IF lBold .AND. lItalic
         cRet := cRet + 'bi.ttf'
      ELSEIF lBold
         IF cRet = "arialn"
            cRet := cRet + 'b.ttf'
         ELSE
            cRet := cRet + 'bd.ttf'
         ENDIF
      ELSEIF lItalic
         cRet := cRet + 'i.ttf'
      ELSE
         cRet := cRet + '.ttf'
      ENDIF
      cRet := cFldWindows + "\" + cRet
      cRet := _HMG_HPDF_ExistInLocal( cRet )
   ENDIF

RETURN cRet

FUNCTION _HMG_HPDF_ExistInLocal( FontName )

   LOCAL cFldWindows := GetSpecialFolder( CSIDL_FONTS )
   LOCAL cFileTTF := cFileNoExt( FontName ) + ".ttf"
   LOCAL cFldLocal := GetCurrentFolder()
   LOCAL cFntTmp := ""

   IF File( FontName )
      RETURN FONTNAME
   ENDIF
   IF File( cFldWindows + "\" + cFileTTF )
      cFntTmp := cFldWindows + "\" + cFileTTF
   ENDIF
   IF File( cFldLocal + "\" + cFileTTF )
      cFntTmp := cFldLocal + "\" + cFileTTF
   ENDIF

RETURN cFntTmp

FUNCTION _HMG_HPDF_SeekInLocalOptions( FONTNAME, lBold, lItalic )

   LOCAL aDirFnts, cRet := ""
   LOCAL cFldWindows, cFileTTF

   DEFAULT lBold := .F., lItalic := .F.

   cFldWindows := GetSpecialFolder( CSIDL_FONTS )
   cFileTTF := cFileNoExt( FontName )
   aDirFnts := ASort( Directory( cFldWindows + "\" + hb_USubStr( cFileTTF, 1, 4 ) + "*.ttf" ),,, {| x, y | Len( x[ 1 ] ) < Len( y[ 1 ] ) } )
   IF Len( aDirFnts ) > 0
      cRet := cFldWindows + "\" + cFileNoExt( aDirFnts[ 1, 1 ] )
      IF lBold .AND. lItalic
         cRet := cRet + 'bi.ttf'
      ELSEIF lBold
         cRet := cRet + 'bd.ttf'
      ELSEIF lItalic
         cRet := cRet + 'i.ttf'
      ELSE
         cRet := cRet + '.ttf'
      ENDIF
   ENDIF

RETURN IF( File( cRet ), cRet, "" )

FUNCTION _HMG_HPDF_SetDash( nMode )

   LOCAL aPtn, nNum := 0, nFase := 0

   DEFAULT nMode := 0
   DO CASE
   CASE nMode == 1
      aPtn := DASH_MODE1 // {3}
      nNum := 1
      nFase := 1
   CASE nMode == 2
      aPtn := DASH_MODE2 // {7,3}
      nNum := 2
      nFase := 2
   CASE nMode == 3
      aPtn := DASH_MODE3 // {8,7,2,7}
      nNum := 4
      nFase := 0
   ENDCASE
   HPDF_Page_SetDash ( _HMG_HPDFDATA[ 1 ][ 7 ], @aPtn, nNum, nFase )

RETURN NIL

FUNCTION _HMG_HPDF_SetOrientation( nMode )

   LOCAL nPaperWidth, nPaperLength

   DEFAULT nMode := 1
   _HMG_HPDFDATA[ 1 ][ 6 ] := nMode
   nPaperWidth := _HMG_HPDFDATA[ 1 ][ 4 ]
   nPaperLength := _HMG_HPDFDATA[ 1 ][ 5 ]
   IF _HMG_HPDFDATA[ 1 ][ 6 ] == 1 // orientation portrait
      IF nPaperWidth > nPaperLength
         _HMG_HPDFDATA[ 1 ][ 4 ] := nPaperLength
         _HMG_HPDFDATA[ 1 ][ 5 ] := nPaperWidth
      ENDIF
   ELSE // orientation landscape
      IF nPaperWidth < nPaperLength
         _HMG_HPDFDATA[ 1 ][ 4 ] := nPaperLength
         _HMG_HPDFDATA[ 1 ][ 5 ] := nPaperWidth
      ENDIF
   ENDIF

RETURN NIL

FUNCTION _HMG_HPDF_SetFontSize( nFontSize )

   DEFAULT nFontSize := 12
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   _HMG_HPDFDATA[ 1 ][ 9 ] := nFontSize

RETURN NIL

FUNCTION _HMG_HPDF_SetFontName( cFontName )

   DEFAULT cFontName := 'Helvetica'
   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   _HMG_HPDFDATA[ 1 ][ 8 ] := cFontName

RETURN NIL

FUNCTION HPDF_SkewText( nRow, nCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lColor, lFont, lSize, cAlign, nAngle1, nAngle2 )

   // Skewing text

   LOCAL nRad1, nRad2, nTextWidth
   LOCAL oFont := NIL
   LOCAL cFont

   LOCAL hPdf := _HMG_HPDFDATA[ 1 ][ 1 ]
   LOCAL hPage := _HMG_HPDFDATA[ 1 ][ 7 ]

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT nAngle1 := 0
   DEFAULT nAngle2 := 0

   IF ! HB_ISNUMERIC( nAngle1 )
      nAngle1 := 0
   ENDIF

   IF ! HB_ISNUMERIC( nAngle2 )
      nAngle2 := 0
   ENDIF

   nRad1 := nAngle1 / 180 * 3.141592 // radian value
   nRad2 := nAngle2 / 180 * 3.141592 // radian value

   IF hPdf == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF hPage == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( hPage, nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( hPage, 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )
   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font
      cFont := HPDF_LoadTTFontFromFile( hPdf, cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( hPdf, cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( hPdf, cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( hPdf, cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ENDIF

   HPDF_Page_SetFontAndSize( hPage, oFont, nFontSize )
   nTextWidth := HPDF_Page_TextWidth( hPage, cText )

   SWITCH Upper( Left( AllTrim( cAlign ), 1 ) )
   CASE "R" ; nxPos := nxPos - nTextWidth ; EXIT
   CASE "C" ; nxPos := nxPos - ( nTextWidth / 2 ) ; EXIT
   ENDSWITCH

   nyPos -= nFontSize

   HPDF_Page_BeginText( hPage )
   HPDF_Page_SetTextMatrix ( hPage, 1, tan ( nRad1 ), tan ( nRad2 ), 1, nxPos, nyPos )
   HPDF_Page_ShowText( hPage, cText )
   HPDF_Page_EndText( hPage )

RETURN NIL

FUNCTION HPDF_ScaleText( nRow, nCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lColor, lFont, lSize, cAlign, nxScale, nyScale )

   // Scaling text

   LOCAL nTextWidth
   LOCAL oFont := NIL
   LOCAL cFont
   LOCAL nxPos
   LOCAL nyPos

   LOCAL hPdf := _HMG_HPDFDATA[ 1 ][ 1 ]
   LOCAL hPage := _HMG_HPDFDATA[ 1 ][ 7 ]

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]

   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]

   nxPos := _HMG_HPDF_MM2Pixel( nCol )
   nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow ) + _HMG_HPDF_MM2Pixel( nFontSize / 3 )

   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT nxScale := 1
   DEFAULT nyScale := 1

   IF hPdf == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF hPage == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( hPage, nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( hPage, 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )
   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font
      cFont := HPDF_LoadTTFontFromFile( hPdf, cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( hPdf, cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( hPdf, cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( hPdf, cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ENDIF

   HPDF_Page_SetFontAndSize( hPage, oFont, nFontSize )
   nTextWidth := HPDF_Page_TextWidth( hPage, cText )

   SWITCH Upper( Left( AllTrim( cAlign ), 1 ) )
   CASE "R" ; nxPos := nxPos - nTextWidth ; EXIT
   CASE "C" ; nxPos := nxPos - ( nTextWidth / 2 ) ; EXIT
   ENDSWITCH

   nyPos -= nFontSize

   HPDF_Page_BeginText( hPage )
   HPDF_Page_SetTextMatrix ( hPage, nxScale, 0, 0, nyScale, nxPos, nyPos )
   HPDF_Page_ShowText( hPage, cText )
   HPDF_Page_EndText( hPage )

RETURN NIL

FUNCTION HPDF_RenderText( nRow, nCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lColor, lFont, lSize, cAlign, nMode, nLineW )

   // Rendering text
   // Modes:
   // HPDF_FILL
   // HPDF_STROKE
   // HPDF_FILL_THEN_STROKE
   // HPDF_FILL_CLIPPING
   // HPDF_STROKE_CLIPPING
   // HPDF_FILL_STROKE_CLIPPING

   LOCAL nTextWidth
   LOCAL oFont := NIL
   LOCAL cFont

   LOCAL hPdf := _HMG_HPDFDATA[ 1 ][ 1 ]
   LOCAL hPage := _HMG_HPDFDATA[ 1 ][ 7 ]

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )

   DEFAULT nLineW := 0.5
   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cAlign := ''
   DEFAULT nMode := HPDF_FILL

   IF ! HB_ISNUMERIC( nMode )
      nMode := HPDF_FILL
   ENDIF

   IF ! HB_ISNUMERIC( nLineW )
      nLineW := 0.5
   ENDIF

   IF hPdf == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN NIL
   ENDIF
   IF hPage == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN NIL
   ENDIF

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( hPage, nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( hPage, 0.0, 0.0, 0.0 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )
   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font
      cFont := HPDF_LoadTTFontFromFile( hPdf, cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( hPdf, cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( hPdf, cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( hPdf, cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ENDIF

   HPDF_Page_SetFontAndSize( hPage, oFont, nFontSize )
   HPDF_Page_SetLineWidth ( hPage, nLineW )
   HPDF_Page_GSave ( hPage )
   HPDF_Page_SetTextRenderingMode ( hPage, nMode )
   nTextWidth := HPDF_Page_TextWidth( hPage, cText )

   SWITCH Upper( Left( AllTrim( cAlign ), 1 ) )
   CASE "R" ; nxPos := nxPos - nTextWidth ; EXIT
   CASE "C" ; nxPos := nxPos - ( nTextWidth / 2 ) ; EXIT
   ENDSWITCH

   nyPos -= nFontSize

   HPDF_Page_BeginText ( hPage )
   HPDF_Page_TextOut ( hPage, nxPos, nyPos, cText )
   HPDF_Page_EndText ( hPage )
   IF nMode = HPDF_FILL_CLIPPING .OR. nMode = HPDF_STROKE_CLIPPING .OR. nMode = HPDF_FILL_STROKE_CLIPPING
      show_stripe_pattern ( hPage, nxPos, nyPos, nTextWidth )
   ENDIF
   HPDF_Page_GRestore ( hPage )

   /* Reset text attributes */
   HPDF_Page_SetTextRenderingMode ( hPage, HPDF_FILL )
   HPDF_Page_SetRGBFill ( hPage, 0, 0, 0 )
   HPDF_Page_SetFontAndSize( hPage, HPDF_GetFont( hPdf, "Helvetica", NIL ), 12 )
   HPDF_Page_SetLineWidth ( hPage, 0.1 )
   HPDF_Page_BeginText ( hPage )
   HPDF_Page_EndText ( hPage )

RETURN NIL

FUNCTION show_stripe_pattern ( hPage, x, y, nTxtW )

   LOCAL iy := 0

   DO WHILE iy < 50
      HPDF_Page_SetRGBStroke ( hPage, 0.0, 0.0, 0.5 )
      HPDF_Page_SetLineWidth ( hPage, 1 )
      HPDF_Page_MoveTo ( hPage, x, y + iy )
      HPDF_Page_LineTo ( hPage, x + nTxtW, y + iy )
      HPDF_Page_Stroke ( hPage )
      iy += 3
   ENDDO
   HPDF_Page_SetLineWidth ( hPage, 2 )

RETURN NIL

FUNCTION HPDF_CircleText( nRow, nCol, cFontName, nFontSize, nRColor, nGColor, nBColor, cText, lBold, lItalic, lColor, lFont, lSize, nRadial, lCircle, cAlign )

   /* text along a circle */
   /* nRow, nCol center coordinates */

   LOCAL /*nAngle1,*/ nAngle2, nRlen
   LOCAL i, nRad1, nRad2, x, y
   LOCAL oFont := NIL
   LOCAL cFont

   LOCAL hPdf := _HMG_HPDFDATA[ 1 ][ 1 ]
   LOCAL hPage := _HMG_HPDFDATA[ 1 ][ 7 ]

   LOCAL nHeight := _HMG_HPDFDATA[ 1 ][ 5 ]
   LOCAL nxPos := _HMG_HPDF_MM2Pixel( nCol )
   LOCAL nyPos := nHeight - _HMG_HPDF_MM2Pixel( nRow )
   LOCAL nPxAng
   LOCAL nRange := 360

   DEFAULT lCircle := .F.
   DEFAULT nRadial := 15
   DEFAULT cFontName := ''
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT nRColor := 0
   DEFAULT nGColor := 0
   DEFAULT nBColor := 0
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lColor := .F.
   DEFAULT lFont := .F.
   DEFAULT lSize := .F.
   DEFAULT cText := ""

   IF ! HB_ISNUMERIC( nRadial )
      nRadial := 15
   ENDIF

   IF ! HB_ISSTRING( cAlign )
      cAlign := ""
   ENDIF

   nRLen := _HMG_HPDF_MM2Pixel( nRadial )

   // set color
   IF lColor
      HPDF_Page_SetRGBFill( hPage, nRColor / 255, nGColor / 255, nBColor / 255 )
   ELSE
      HPDF_Page_SetRGBFill( hPage, 0.0, 0.0, 0.0 )
   ENDIF

   IF lCircle
      IF lColor
         HPDF_Page_SetRGBSTROKE( hPage, nRColor / 255, nGColor / 255, nBColor / 255 )
      ELSE
         HPDF_Page_SetRGBSTROKE( hPage, 0.0, 0.0, 0.0 )
      ENDIF
      HPDF_Page_SetLineWidth ( hPage, 1.5 )
      HPDF_Page_Circle ( hPage, nxPos, nyPos, nRlen + nFontSize )
      HPDF_Page_Circle ( hPage, nxPos, nyPos, nRlen - ( nFontSize / 3 ) )
      HPDF_Page_Stroke ( hPage )
      HPDF_Page_SetLineWidth ( hPage, 1 )
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )
   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font
      cFont := HPDF_LoadTTFontFromFile( hPdf, cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN NIL
      ENDIF
      oFont := HPDF_GetFont( hPdf, cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( hPdf, cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( hPdf, cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN NIL
   ENDIF
   HPDF_Page_SetFontAndSize( hPage, oFont, nFontSize )

   SWITCH Upper( Left( AllTrim( cAlign ), 1 ) )
   CASE "T" ; nRange := 180 ; EXIT // Uppper
   CASE "B" ; nRange := 180 ; EXIT // Bottom
   ENDSWITCH

   // nAngle1 := nRange / Len( cText ) // monospaced fonts
   nAngle2 := 180

   // A better solution for non monospaced fonts.
   nPxAng := nRange / HPDF_Page_TextWidth( hPage, cText )

   HPDF_Page_BeginText ( hPage )
   FOR i = 1 TO Len( cText )
      nRad1 := ( nAngle2 - Int( ( HPDF_Page_TextWidth( hPage, SubStr( cText, i, 1 ) ) * nPxAng ) / 2 ) - 90 ) / 180 * 3.141592
      nRad2 := nAngle2 / 180 * 3.141592

      SWITCH Upper( Left( AllTrim( cAlign ), 1 ) )
      CASE "B"
         x := nxPos + cos( nRad2 ) * ( nRlen + nFontSize * ( 2 / 3 ) )
         y := nyPos - sin( nRad2 ) * ( nRlen + nFontSize * ( 2 / 3 ) )

         HPDF_Page_SetTextMatrix( hPage, cos( nRad1 ), -sin( nRad1 ), sin( nRad1 ), cos( nRad1 ), x, y )
         EXIT

      DEFAULT
         x := nxPos + cos( nRad2 ) * nRlen
         y := nyPos + sin( nRad2 ) * nRlen

         HPDF_Page_SetTextMatrix( hPage, cos( nRad1 ), sin( nRad1 ), -sin( nRad1 ), cos( nRad1 ), x, y )

      ENDSWITCH

      HPDF_Page_ShowText ( hPage, SubStr( cText, i, 1 ) )

      // nAngle2 -= nAngle1  //monospaced fonts

      // A better solution for non monospaced fonts.
      nAngle2 -= ( HPDF_Page_TextWidth( hPage, SubStr( cText, i, 1 ) ) * nPxAng )

   NEXT i
   HPDF_Page_EndText ( hPage )

RETURN NIL

FUNCTION HMG_GetPDFTextWidth ( cText, cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeout )

   LOCAL cFont, oFont, nWidth

   DEFAULT cFontName := ""
   DEFAULT nFontSize := _HMG_HPDFDATA[ 1 ][ 9 ]
   DEFAULT lBold := .F.
   DEFAULT lItalic := .F.
   DEFAULT lUnderline := .F.
   DEFAULT lStrikeout := .F.

   IF _HMG_HPDFDATA[ 1 ][ 1 ] == NIL // PDF object not found!
      _HMG_HPDF_Error( 3 )
      RETURN 0
   ENDIF
   IF _HMG_HPDFDATA[ 1 ][ 7 ] == NIL // PDF Page object not found!
      _HMG_HPDF_Error( 5 )
      RETURN 0
   ENDIF

   // set font
   cFontName := AllTrim( _HMG_HPDF_SetFont( cFontName, lBold, lItalic ) )
   IF UPPER ( cFileExt ( cFontName ) ) == '.TTF' // load ttf font
      cFont := HPDF_LOADTTFONTFROMFILE( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, .T. )
      IF Len( AllTrim( cFont ) ) == 0
         _HMG_HPDF_Error( 6, cFontName )
         RETURN 0
      ENDIF
      oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFont, _HMG_HPDFDATA[ 1 ][ 10 ] )
   ELSE
      IF Upper( AllTrim( cFontName ) ) == "SYMBOL" .OR. Upper( AllTrim( cFontName ) ) == "ZAPFDINGBATS"
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, NIL )
      ELSE
         oFont := HPDF_GetFont( _HMG_HPDFDATA[ 1 ][ 1 ], cFontName, _HMG_HPDFDATA[ 1 ][ 10 ] )
      ENDIF
   ENDIF

   IF oFont == NIL
      _HMG_HPDF_Error( 6, cFontName )
      RETURN 0
   ELSE
      HPDF_Page_SetFontAndSize( _HMG_HPDFDATA[ 1 ][ 7 ], oFont, nFontSize )
      nWidth := _HMG_HPDF_Pixel2MM( HPDF_Page_TextWidth( _HMG_HPDFDATA[ 1 ][ 7 ], cText ) )
   ENDIF

RETURN nWidth

FUNCTION cFileExt( cPathMask )

   LOCAL cExt

   hb_FNameSplit( cPathMask, , , @cExt )

RETURN cExt
