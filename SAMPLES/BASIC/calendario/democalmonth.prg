/*
democalmonth.prg
*/

#include "minigui.ch"

STATIC nRowPos := -1, nColPos := -1

FUNCTION DemoCalMonth( lWithDialog, xParent, oButton, dDate )

   LOCAL nCont, xValue, dDatRef, nMes

   hb_Default( @lWithDialog, .T. )

   SET WINDOW MAIN OFF

   IF oButton == Nil
      oButton := Array(31)
   ENDIF
   IF dDate == Nil
      dDate := Date()
   ENDIF

   dDatRef := dDate - Day( dDate )
   nMes    := Month( dDate )

   nColPos := -1
   nRowPos := -1

   IF lWithDialog
      DEFINE WINDOW ( xParent := Ze_Name( "DLG" )  ) ;
         AT 1, 1 ;
         WIDTH 800 ;
         HEIGHT 600;
         TITLE hb_Dtoc( dDatRef + 1, "YYYY-MM" ) ;
         FONT "Arial" SIZE 18
      END WINDOW
   ENDIF
   FOR nCont = 1 TO 7

      DEFINE LABEL ( ze_Name( "LBL" ) )
         PARENT ( xParent )
         COL 20 + ( ( nCont - 1 ) * 50 )
         ROW 40
         WIDTH 45
         HEIGHT 30
         VALUE Upper( Left( CDOW( Stod( "20250601" ) + nCont ), 3 ) )
      END LABEL

   NEXT
   IF Dow( dDatRef - 1 ) != 7
      FOR nCont = 1 TO Dow( dDatRef - 1 )
         ColPos()
      NEXT
   ENDIF
   FOR nCont = 1 TO 31
      FOR EACH xValue IN { nCont }
         IF Month( dDatRef + xValue ) == nMes
            DEFINE CHECKBOX ( oButton[ nCont ] := ze_Name( "CHK" ) )
               PARENT ( xParent )
               COL ColPos()
               ROW nRowPos
               WIDTH 45
               HEIGHT 30
               CAPTION Ltrim( Str( nCont ) )
            END CHECKBOX

         ENDIF
      NEXT
   NEXT

   IF lWithDialog
      DoMethod( xParent, "CENTER" )
      DoMethod( xParent, "ACTIVATE" )
   ENDIF

   RETURN Nil

STATIC FUNCTION ColPos()

   IF nColPos == -1
      nColPos := 20
      nRowPos := 80
   ELSE
      nColPos += 50
      IF nColPos > 350
         nRowPos += 40 // LINE_HEIGHT
         nColPos := 20
      ENDIF
   ENDIF

   RETURN nColPos

FUNCTION ze_Name( cPrefix )

   STATIC nIndex := 0

   nIndex += 1

   RETURN cPrefix + Ltrim( Str( nIndex ) )
