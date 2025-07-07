/*
democalyear.prg
*/

#include "minigui.ch"
#include "hbclass.ch"

FUNCTION DemoCalYear()

   LOCAL xTab, nCont, oButton[ 12, 31 ], aItem, dDate, nYear := 2025, xDlg, oDlg

   SET WINDOW MAIN OFF

   FOR EACH aItem IN oButton
      AFill( aItem, .F. )
   NEXT

   DEFINE WINDOW ( xDlg := Ze_Name( "DLG" )  ) ;
      AT 1, 1 ;
      WIDTH 800 ;
      HEIGHT 600;
      TITLE "2025" ;
      FONT "Arial" SIZE 18
   END WINDOW

   DEFINE TAB ( xTab := Ze_Name( "TAB" ) ) ;
      PARENT ( xDlg ) ;
      AT 080, 20 ;
      WIDTH 504 ;
      HEIGHT 341 ;
      HOTTRACK

   FOR nCont = 1 TO 12

      dDate := Stod( StrZero( nYear, 4 ) + StrZero( nCont, 2 ) + "01" )

      PAGE cMonth( dDate )

      DemoCalMonth( .F., xTab, oButton[ nCont ], dDate )

      END PAGE

   NEXT

   END TAB

   DEFINE BUTTONEX ( ze_Name( "BTN" ) )
      PARENT ( xDlg )
      ROW         500
      COL         30
      WIDTH       100
      HEIGHT      20
      CAPTION     "ShowAll"
      ACTION      Show( oButton, xDlg, xTab)
      FLAT       .T.
      NOXPSTYLE  .T.
   END BUTTONEX

   oDlg := ObjFromStack():new(  xDlg )
   oDlg:Center()
   oDlg:Activate()

   (xTab)

   RETURN Nil

FUNCTION Show( oButton, xDlg, xTab )

   LOCAL cTxt := "", oMonth, oDay, oCheck

   FOR EACH oMonth IN oButton
      FOR EACH oDay IN oMonth
         IF ! Empty( oDay )
            oCheck := ObjFromStack():New( xDlg, oDay )
            IF oCheck:Value
               cTxt += Dtoc( Stod( "2025" + StrZero( oMonth:__EnumIndex(), 2 ) + StrZero( oDay:__EnumIndex(), 2 ) ) ) + " "
            ENDIF
         ENDIF
      NEXT
   NEXT
   MsgBox( cTxt )

   (xDlg);(xTab) // warning -w3 -es2

   RETURN Nil


CLASS ObjFromStack

   VAR aStack INIT {}

   METHOD New( ... )
   METHOD Center() INLINE DoMethod( hb_ArrayToParams( ::aStack ), "Center")
   METHOD Activate() INLINE DoMethod( hb_ArrayToParams( ::aStack ), "Activate")
   METHOD Value( xValue )
ENDCLASS

METHOD New( ... ) CLASS ObjFromStack

   ::aStack := hb_AParams()

   RETURN Self

METHOD Value( xValue ) CLASS ObjFromStack

   IF xValue != Nil
      SetProperty( hb_ArrayToParams( ::aStack ), "nValue", xValue )
   ELSE
      xValue := GetProperty( hb_ArrayToParams( ::aStack ), "Value" )
   ENDIF

   RETURN xValue

