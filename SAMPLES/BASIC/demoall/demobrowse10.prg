/*
 * Browse10 Demo
 * (c) 2005 Jacek Kubica <kubica@wssk.wroc.pl>
 *
 * New property - ColumnWidth(nColumn) for BROWSE/GRID controls
 * New methods for BROWSE/GRID controls
   - ColumnAutoFit(nColumn)  - set column width to best fit regarding to column contents
   - ColumnAutoFitH(nColumn) - set column width to best fit regarding to column header contents
   - ColumnsAutoFit  - set widths of all columns in control to best fit regarding to column contents
   - ColumnsAutoFitH - set widths of all columns in control to best fit regarding to column header contents
*/

#include "minigui.ch"
#include "dbstruct.ch"

Function DemoBrowse10( lWithDialog, xDlg, aInitList, aEndList )

   LOCAL var := 'Test'

	REQUEST DBFCDX , DBFFPT

   hb_Default( @lWithDialog, .T. )
   hb_Default( @aInitList, {} )

	SET CENTURY ON
	SET DELETED ON

	SET BROWSESYNC ON

   IF lWithDialog
	   DEFINE WINDOW (xDlg := "form_1") ;
		   AT 0,0 ;
		   WIDTH 640 HEIGHT 480 ;
		   TITLE 'Browse Demo' ;
		   MAIN NOMAXIMIZE ;
		   ON INIT OpenTables() ;
		   ON RELEASE CloseTables()
      END WINDOW

      ELSE
         AAdd( aInitList, { || OpenTables( xDlg ) } )
         AAdd( aEndList,  { || CloseTables() } )
      ENDIF

		DEFINE MAIN MENU OF (xDlg)
			POPUP 'File'
				ITEM 'Set Browse Value'	ACTION SetProperty( xDlg, "Browse_1", "Value", Val ( InputBox ('Set Browse Value','') ) )
				ITEM 'Get Browse Value'	ACTION MsgInfo( Str( GetProperty( xDlg, "Browse_1", "Value" ) ) )
				ITEM 'Refresh Browse'	ACTION DoMethod( xDlg, "Browse_1", "Refresh" )
				SEPARATOR
				ITEM 'Exit'		ACTION DoMethod( xDlg, "Release" )
			END POPUP
			POPUP 'Help'
				ITEM 'About'		ACTION MsgInfo ("MiniGUI Browse Demo")
			END POPUP

			POPUP 'New Functions Tests'

				ITEM 'GetColumnWidth Column 1 Browse 1 '         //ACTION MsgBox( str( form_1.Browse_1.ColumnWidth(1)) )
				ITEM 'GetColumnWidth Column 2 Browse 1'          //ACTION MsgBox( str( form_1.Browse_1.ColumnWidth(2)) )
				ITEM 'GetColumnWidth Column 3 Browse 1'          //ACTION MsgBox( str( form_1.Browse_1.ColumnWidth(3)) )
				SEPARATOR
				ITEM 'SetColumnWidth Column 1 Browse 1 To 100 '  //ACTION {|| ( form_1.Browse_1.ColumnWidth(1) := 100)}
				ITEM 'SetColumnWidth Column 1 Browse 2 To 100 '  //ACTION {|| ( form_1.Browse_1.ColumnWidth(2) := 100)}
				ITEM 'SetColumnWidth Column 1 Browse 3 To 100 '  //ACTION {|| ( form_1.Browse_1.ColumnWidth(3) := 100)}
				SEPARATOR
				ITEM 'SetColumnWidth Column 1 Browse 1 To Auto'  //ACTION form_1.Browse_1.ColumnAutoFit(1)
				ITEM 'SetColumnWidth Column 1 Browse 1 To AutoH' //ACTION form_1.Browse_1.ColumnAutoFitH(2)
				ITEM 'SetColumnWidth Column 2 Browse 1 To Auto'  //ACTION form_1.Browse_1.ColumnAutoFit(1)
				ITEM 'SetColumnWidth Column 2 Browse 1 To AutoH' //ACTION form_1.Browse_1.ColumnAutoFitH(2)
				SEPARATOR
				ITEM 'Set Auto Width for Browse_1' ACTION DoMethod( xDlg, "Browse_1", "ColumnsAutoFit" )
				ITEM 'Set Auto Width for Browse_2' ACTION DoMethod( xDlg, "Browse_2", "ColumnsAutoFit" )
				ITEM 'Set Auto Width for Browse_3' ACTION DoMethod( xDlg, "Browse_3", "ColumnsAutoFit" )
				SEPARATOR
				ITEM 'Set AutoFit Widths for Browse_1 (header)'  ACTION DoMethod( xDlg, "Browse_1", "ColumnsAutoFitH" )
				ITEM 'Set AutoFit Widths for Browse_2(header)'   ACTION DoMethod( xDlg, "Browse_2", "ColumnsAutoFitH" )
				ITEM 'Set AutoFit Widths for Browse_3(header)'   ACTION DoMethod( xDlg, "Browse_3", "ColumnsAutoFitH" )

			END POPUP
		END MENU

		DEFINE STATUSBAR OF ( xDlg )
			STATUSITEM ''
		END STATUSBAR

		DEFINE TAB Tab_1 ;
         OF ( xDlg ) ;
			AT 0,10 ;
			WIDTH 600 ;
			HEIGHT 400 ;
			VALUE 1 ;
			TOOLTIP 'Tab Control' ;
			ON CHANGE ( DoMethod ( xDlg, "Browse_" + Ltrim( Str( GetProperty( xDlg, "Tab_1", "Value" ) ) ) , "SetFocus" ), ;
			DoMethod( xDlg, "Browse_" + Ltrim( Str( GetProperty( xDlg, "Tab_1", "Value" ) ) ) , "Refresh" ) , ChangeTest(xDlg) )

			PAGE 'Page &1'

				@ 40,20 BROWSE Browse_1	;
               OF ( xDlg ) ;
					WIDTH 560  										;
					HEIGHT 340 										;
					HEADERS { 'Code' , 'First Name' , 'Last Name', 'Birth Date', 'Married' , 'Biography' } ;
					WIDTHS { 150 , 150 , 150 , 150 , 150 , 150 } ;
					WORKAREA &var ;
					FIELDS { 'Test->Code' , 'Test->First' , 'Test->Last' , 'Test->Birth' , 'Test->Married' , 'Test->Bio' } ;
					TOOLTIP 'Browse Test' ;
					ON CHANGE ChangeTest(xDlg) ;
					JUSTIFY { BROWSE_JTFY_LEFT,BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER} ;
					DELETE ;
					LOCK ;
					EDIT INPLACE
			END PAGE

			PAGE 'Page &2'

				@ 40,20 BROWSE Browse_2	;
               OF xDlg ;
					WIDTH 560  										;
					HEIGHT 340 										;
					HEADERS { 'Code' , 'First Name' , 'Last Name', 'Birth Date', 'Married' , 'Biography' } ;
					WIDTHS { 150 , 150 , 150 , 150 , 150 , 150 } ;
					WORKAREA &var ;
					FIELDS { 'Test->Code' , 'Test->First' , 'Test->Last' , 'Test->Birth' , 'Test->Married' , 'Test->Bio' } ;
					TOOLTIP 'Browse Test' ;
					ON CHANGE ChangeTest(xDlg) ;
					JUSTIFY { BROWSE_JTFY_LEFT,BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER} ;
					DELETE ;
					LOCK ;
					EDIT INPLACE
			END PAGE

			PAGE 'Page &3'

				@ 40,20 BROWSE Browse_3	;
               OF ( xDlg ) ;
					WIDTH 560  										;
					HEIGHT 340 										;
					HEADERS { 'Code' , 'First Name' , 'Last Name', 'Birth Date', 'Married' , 'Biography' } ;
					WIDTHS { 150 , 150 , 150 , 150 , 150 , 150 } ;
					WORKAREA &var ;
					FIELDS { 'Test->Code' , 'Test->First' , 'Test->Last' , 'Test->Birth' , 'Test->Married' , 'Test->Bio' } ;
					TOOLTIP 'Browse Test' ;
					ON CHANGE ChangeTest(xDlg) ;
					JUSTIFY { BROWSE_JTFY_LEFT,BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER,BROWSE_JTFY_CENTER} ;
					DELETE ;
					LOCK ;
					EDIT INPLACE
			END PAGE

		END TAB

   IF lWithDialog
	   DoMethod( xDlg, "CENTER" )
      DoMethod( xDlg, "Browse_1", "SetFocus" )
      DoMethod( xDlg, "Activate" )
   ENDIF

RETURN Nil


PROCEDURE OpenTables( xDlg )

	IF ! file("test.dbf")
		CreateTable()
	ENDIF

	USE Test VIA "DBFCDX"
	GOTO TOP

	SetProperty( xDlg, "Browse_1", "Value", RecNo() )

RETURN

PROCEDURE CloseTables()

	USE

RETURN

PROCEDURE ChangeTest(xDlg)

   LOCAL nTab1Value, nBrowseValue

   nTab1Value   := GetProperty( xDlg, "Tab_1", "Value" )
   nBrowseValue := GetProperty( xDlg, 'Browse_' + Ltrim( Str( nTab1Value ) ), "Value" )
	//Form_1.StatusBar.Item(1) := 'RecNo() ' + Alltrim( Str( nBrowseValue ) )
   (nBrowseValue)

RETURN

PROCEDURE CreateTable

   LOCAL aDbf[6][4], i

   aDbf[1][ DBS_NAME ] := "Code"
   aDbf[1][ DBS_TYPE ] := "Numeric"
   aDbf[1][ DBS_LEN ]  := 10
   aDbf[1][ DBS_DEC ]  := 0
   //
   aDbf[2][ DBS_NAME ] := "First"
   aDbf[2][ DBS_TYPE ] := "Character"
   aDbf[2][ DBS_LEN ]  := 25
   aDbf[2][ DBS_DEC ]  := 0
   //
   aDbf[3][ DBS_NAME ] := "Last"
   aDbf[3][ DBS_TYPE ] := "Character"
   aDbf[3][ DBS_LEN ]  := 25
   aDbf[3][ DBS_DEC ]  := 0
   //
   aDbf[4][ DBS_NAME ] := "Married"
   aDbf[4][ DBS_TYPE ] := "Logical"
   aDbf[4][ DBS_LEN ]  := 1
   aDbf[4][ DBS_DEC ]  := 0
   //
   aDbf[5][ DBS_NAME ] := "Birth"
   aDbf[5][ DBS_TYPE ] := "Date"
   aDbf[5][ DBS_LEN ]  := 8
   aDbf[5][ DBS_DEC ]  := 0
        //
   aDbf[6][ DBS_NAME ] := "Bio"
   aDbf[6][ DBS_TYPE ] := "Memo"
   aDbf[6][ DBS_LEN ]  := 10
   aDbf[6][ DBS_DEC ]  := 0
   //

   DBCREATE("Test", aDbf, "DBFCDX")

	USE test VIA "DBFCDX"
	// zap // no exclusive

	FOR i:= 1 TO 100
		APPEND BLANK
		REPLACE code    WITH i
		REPLACE First   WITH 'First Name '+ Str(i)
		REPLACE Last    WITH 'Last Name '+ Str(i)
		REPLACE Married WITH .t.
		REPLACE birth   WITH date()+i-10000
	NEXT i

	INDEX ON field->code TO code

	USE

RETURN
