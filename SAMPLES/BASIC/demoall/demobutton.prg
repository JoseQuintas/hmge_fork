
/*
test codeblock

Based on sample of Valteçom - Uberaba - MG - Brasil
*/

#include "minigui.ch"

// don't initialize values here
STATIC  nRowPos, nColPos

FUNCTION DemoButton( lWithDialog, xDlg )

   LOCAL aList := {}, nCont, nMesa, xButton, bCode

   hb_Default( @lWithDialog, .T. )

   nColPos := -1
   nRowPos := -1

   FOR nCont = 1 TO 35
      AAdd( aList, nCont + 50 )
   NEXT
   IF lWithDialog
      DEFINE WINDOW ( xDlg := "DLGANY" ) ;
         AT 0,0 ;
         WIDTH 1000 ;
         HEIGHT 500 ;
         MAIN;
         TITLE 'Teste de Mesa' ;
         NOSIZE ;
         NOMAXIMIZE
      END WINDOW
   ENDIF

   FOR nCont := 1 TO Len( aList )

      // two variables uniques
      FOR EACH nMesa, xButton IN ;
         { aList[ nCont ] }, ;
         { "Mesa" + StrZero( nCont,2 ) }

         RowPos( xDlg )
         @ nRowPos, nColPos BUTTONEX ( xButton ) ;
            OF ( xDlg ) ;
            CAPTION iif( Mod( nCont, 2 ) == 0, "Mesa ", "Quarto " ) + CRLF + StrZero( nMesa, 2 ) ;
            ICON iif( Mod( nCont, 2 ) == 0, "icotable.ico", "icohouse.ico" ) ;
            BACKCOLOR GREEN GRADIENTFILL { { 1, GREEN,GREEN } } ;
            WIDTH 120 ;
            HEIGHT 60

         bCode := { || MostraMesa( xDlg, nMesa, xButton ) }
         SetProperty( xDlg, xButton, "ACTION", bCode )
      NEXT
   NEXT

   IF lWithDialog
      CENTER WINDOW ( xDlg )
      ACTIVATE WINDOW ( xDlg )
   ENDIF

RETURN Nil

STATIC FUNCTION RowPos( xDlg )

   IF nColPos == -1
      nRowPos := 35
      nColPos := 5
   ELSE
      nColPos += 125
      IF nColPos > GetProperty( xDlg, "width" ) - 200
         nRowPos += 65
         nColPos := 5
      ENDIF
   ENDIF

   RETURN nRowPos

STATIC FUNCTION MostraMesa( xDlg, nMesa, xButton )

   MsgInfo( "Mesa " + StrZero( nMesa, 2 ) + " selecionada" )
   IF ArrayEqual( GetProperty( xDlg, xButton, "BACKCOLOR" ), RED )
      SetProperty( xDlg, xButton, "BACKCOLOR", GREEN )
      SetProperty( xDlg, xButton, "GRADIENTFILL", { { 1, GREEN, GREEN } } )
   ELSE
      SetProperty( xDlg, xButton, "BACKCOLOR", RED )
      SetProperty( xDlg, xButton, "GRADIENTFILL", { { 1, RED, RED } } )
   ENDIF

RETURN Nil

STATIC FUNCTION ArrayEqual( a, b )

   LOCAL nCont

   IF ValType( A ) != "A" .OR. ValType( B ) != "A"
      RETURN .F.
   ENDIF
   IF Len( A ) != Len( B )
      RETURN .F.
   ENDIF
   FOR nCont = 1 TO Len( A )
      IF ! A[ nCont ] == B[ nCont ]
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.
