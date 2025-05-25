/*
 * HMG Embedded Child Window Demo
 * (c) 2002-2010 Roberto Lopez
*/

#include "minigui.ch"

FUNCTION Main

   LOCAL xDlg

   DEFINE WINDOW ( xDlg := ze_Name( "DLG" ) ) ;
         ROW 0 ;
         COL 0 ;
         WIDTH 800 ;
         HEIGHT 600 ;
         TITLE 'Panel Window Demo 2' ;
         WINDOWTYPE MAIN

   END WINDOW

   DoTab( xDlg )

   //DoMethod( xPanel2, xText1, "Setfocus" )

   DoMethod( xDlg, "Center" )

   DoMethod( xDlg, "Activate" )

   RETURN Nil

STATIC FUNCTION DoTab( xDlg, nLevel )

   LOCAL xTab, xPanel

   hb_Default( @nLevel, 1 )

   DEFINE TAB ( xTab := ze_Name( "TAB" ) ) ;
      PARENT ( xDlg ) ;
      AT 10, 10 ;
      WIDTH 500 ;
      HEIGHT 500 ;
      VALUE 1 ;
      TOOLTIP 'Tab Control'

      //PAGE "Page 1"

         //xPanel := DoWindowPanel( xDlg )

         //IF nLevel == 1
         //   DoTab( xPanel, nLevel + 1 )
         //ENDIF

      //END PAGE

      PAGE 'Page 2'

         xPanel := DoWindowPanel( xDlg )

         DoTextLabel( xPanel )

      END PAGE

      PAGE 'Page 3'

         DoRadioGroup()

      END PAGE

      PAGE 'Page 4'

         xPanel := DoWindowPanel( xDlg )

         DoTextLabel( xPanel )
         DoSpinner( xPanel )

      END PAGE

      PAGE 'Page 5'

         DoGrid()

      END PAGE

   END TAB

   RETURN { xTab, xPanel }

STATIC FUNCTION DoTextLabel( xDlg )

   LOCAL xText, xLabel

   DEFINE LABEL ( xLabel := ze_Name( "LABEL" ) )
      PARENT ( xDlg )
      ROW 50
      COL 50
      VALUE 'Panel window...'
      WIDTH 150
   END LABEL

   DEFINE TEXTBOX ( xText := ze_Name( "TEXT" ) )
      PARENT ( xDlg )
      ROW 90
      COL 50
      VALUE "Can do this!"
   END TEXTBOX

   RETURN { xText, xLabel }

STATIC FUNCTION DoGrid()

   LOCAL xGrid, aRows[ 20 ][ 3 ]

   aRows[ 1 ] := { 'Simpson', 'Homer', '555-5555' }
   aRows[ 2 ] := { 'Mulder', 'Fox', '324-6432' }
   aRows[ 3 ] := { 'Smart', 'Max', '432-5892' }
   aRows[ 4 ] := { 'Grillo', 'Pepe', '894-2332' }
   aRows[ 5 ] := { 'Kirk', 'James', '346-9873' }

   @ 50, 50 GRID ( xGrid := ze_Name( "GRID" ) ) ;
      WIDTH 300 ;
      HEIGHT 330 ;
      HEADERS { 'Last Name', 'First Name', 'Phone' } ;
      WIDTHS { 140, 140, 140 } ;
      ITEMS aRows ;
      VALUE 1

   RETURN xGrid

STATIC FUNCTION ze_Name( cPre )

   STATIC nIndex := 0

   nIndex += 1

   RETURN cPre + Ltrim( Str( nIndex ) )

STATIC FUNCTION DoWindowPanel( xParent )

   LOCAL xDlg

   DEFINE WINDOW ( xDlg := ze_Name( "WPANEL" ) )  ;
      PARENT ( xParent ) ;
      ROW 50 ;
      COL 10 ;
      WIDTH 400 ;
      HEIGHT 400 ;
      ; // VIRTUAL WIDTH 600 ;
      ; // VIRTUAL HEIGHT 600 ;
      WINDOWTYPE PANEL
   END WINDOW

   RETURN xDlg

STATIC FUNCTION DoRadioGroup()

   LOCAL xRadio

   DEFINE RADIOGROUP ( xRadio := ze_Name( "RADIO" ) )
      ROW 100
      COL 100
      OPTIONS { '1', '2', '3' }
      VALUE 1
   END RADIOGROUP

   RETURN xRadio

STATIC FUNCTION DoSpinner()

   LOCAL xSpin

   @ 100, 250 SPINNER ( xSpin := "Spinner_1" ) ;
      RANGE 0, 10 ;
      VALUE 5 ;
      WIDTH 100 ;
      TOOLTIP 'Range 0,10' ;
      ON CHANGE PlayBeep()

   RETURN xSpin
