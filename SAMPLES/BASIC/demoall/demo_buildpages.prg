/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
*/

#include "minigui.ch"
#include "i_winuser.ch"
#include "tsbrowse.ch"

FUNCTION demo_BuildPages( xDlg, aOptionList, nLevel, aInitList, aEndList )

   LOCAL xBrowse, aOption, oBrw, aFont := {}, aPageList := {}, xWPage, xButton

   DEFINE FONT Font_1 FONTNAME "Times New Roman" SIZE 11
   DEFINE FONT Font_2 FONTNAME "Times New Roman" SIZE 13 BOLD

   AAdd( aFont, GetFontHandle( "Font_1" ) )
   AAdd( aFont, GetFontHandle( "Font_2" ) )

   DEFINE TBROWSE ( xBrowse := ze_Name( "BROW" ) ) OBJ oBrw AT 5, 2 OF ( xDlg ) ;
         WIDTH GetProperty( xDlg, "ClientWidth" ) ;
         HEIGHT 25 ;
         GRID
   END TBROWSE

   // define pages
   FOR EACH aOption IN aOptionList

      DEFINE WINDOW ( xWPage := ze_Name( "DLG" ) ) ;
         AT 30, 2 ;
         PARENT ( xDlg ) ;
         WIDTH GetProperty( xDlg, "ClientWidth" ) ;
         HEIGHT GetProperty( xDlg, "ClientHeight" ) - 30 ;
         TITLE aOption[ 1 ] ;
         PANEL ;
         BACKCOLOR { 220, 220, 220 } ;
         NOSHOW
      END WINDOW
      AAdd( aPageList, xWPage )

      IF Len( aOption[ 2 ] ) != 0
         demo_BuildPages( xWPage, aOption[ 2 ], nLevel + 1, aInitList )
      ELSE
         IF ValType( aOption[ 3 ] ) == "C"
			   @ 100, 100 BUTTON ( xButton := ze_Name( "BUTTON" ) ) ;
               CAPTION aOption[1] ;
               WIDTH 100 ;
               HEIGHT 20 ;
               ACTION Eval( { || MsgInfo( aOption[1] ) } )
         ELSE
            Eval( aOption[ 3 ], xWPage, aInitList, aEndList )
         ENDIF
      ENDIF
   NEXT

   SetupBrowse( xDlg, xBrowse, aFont, aPageList )

   DrawPage( xDlg, xBrowse, 1, aPageList )

   //AAdd( aInitList, { || DoMethod( xDlg, xBrowse, "SetFocus" ) } )

   (nLevel);(xButton)

   RETURN Nil

FUNCTION DrawPage( xDlg, xBrowse, nNewPage, aPageList )

   LOCAL nPos
   STATIC aList := {}

   nPos := hb_AScan( aList, { | e | e[1] == xDlg .AND. e[2] == xBrowse } )
   IF nPos == 0
      AAdd( aList, { xDlg, xBrowse, 0 } )
      nPos := Len( aList )
   ENDIF

   IF aList[ nPos, 3 ] <> 0
      DoMethod( aPageList[ aList[ nPos, 3 ] ], "hide" )
   END
   aList[ nPos, 3 ] := nNewPage
   DoMethod( aPageList[ aList[ nPos, 3 ] ], "show" )

   RETURN NIL

FUNCTION SetupBrowse( xDlg, xBrowse, aFont, aPageList )

   LOCAL nPos, nCont, aArray := { {} }, xPanel, oBrw

   FOR EACH xPanel IN aPageList
      AAdd( aArray[1], GetProperty( xPanel, "TITLE" ) )
   NEXT
   nPos := GetControlIndex( xBrowse, xDlg )
   IF nPos > 0
      oBrw := _HMG_aControlIds[ nPos ]
   ENDIF

   WITH OBJECT oBrw

      :SetArray( aArray, .T. )

      :nWheelLines  := 0.1
      :nClrLine     := COLOR_GRID
      :lNoChangeOrd := TRUE
      :lCellBrw     := TRUE
      :lNoKeyChar   := TRUE

      :lNoVScroll   := TRUE
      :lNoHScroll   := TRUE

      :nHeightCell  := 25
      :nHeightHead  := 0
      :bChange      := {|| nil }
      :bLDblClick   := {|| nil }

      :SetColor( { 1 },  { {|| RGB( 0, 0, 0 ) } }, )
      :SetColor( { 5 },  { {|| RGB( 0, 0, 0 ) } }, )
      :SetColor( { 11 }, { {|| RGB( 0, 0, 0 ) } }, )

      :SetColor( { 6 }, { {| a, b, c | (a), IF( c:nCell == b, { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) }, ;
         { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) } ) } } )

      :SetColor( { 12 }, { {| a, b, c | (a), IF( c:nCell == b, { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) }, ;
         { RGB( 255, 255, 255 ), RGB( 220, 220, 220 ) } ) } } )

      FOR nCont = 1 TO Len( aPageList )
         FOR EACH nPos IN { nCont }
            :ChangeFont( {|| IF( oBrw:nCell == nPos, aFont[ 2 ], aFont[ 1 ] ) }, nPos, 1 )
         NEXT
      NEXT

      :bTSDrawCell := {| a, b, c | (a), (c), ;
         iif( b:nCell == oBrw:nCell, b:nClrLine := CLR_GRAY, b:nClrLine := COLOR_GRID ) }

      :bOnDraw := { || DrawPage( xDlg, xBrowse, oBrw:nCell, aPageList ) }

   ENDWITH

   SetWindowLong( oBrw:hWnd, GWL_EXSTYLE, WS_EX_STATICEDGE )

   (xDlg); (xBrowse); (oBrw)

   RETURN Nil
