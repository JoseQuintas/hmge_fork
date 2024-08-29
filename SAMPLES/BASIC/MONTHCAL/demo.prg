/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 640 HEIGHT 480 ;
		TITLE "Month Calendar Control Demo" ;
		ICON "DEMO.ICO" ;
		MAIN ;
		FONT "Arial" SIZE 10

		@ 10,10 MONTHCALENDAR Month_1 ;
			VALUE Date() ;
			TOOLTIP "Month Calendar Control NoToday" ;
			NOTODAY ;
			BACKCOLOR YELLOW ;
			FONTCOLOR BLUE ;
			ON CHANGE {||msginfo("Month_1 Change")} ;
			ON SELECT {||msginfo("Month_1 Select")}

		@ 10,300 BUTTON Button_1 ;
			CAPTION "SET DATE" ;
			ACTION Form_1.Month_1.Value := Date() 

		@ 50,300 BUTTON Button_2 ;
			CAPTION "GET DATE" ;
			ACTION MsgInfo ( GetDate ( Form_1.Month_1.Value ) ) 

		@ 90,300 BUTTON Button_5 ;
			CAPTION "GET FIRST WEEK DAY" ;
			WIDTH 160 ;
			ACTION MsgInfo ( Form_1.Month_1.FirstDayOfWeek ) 

		@ 210,10 MONTHCALENDAR Month_2 ;
			VALUE CTOD( "01/01/2001" ) ;
			FONT "Courier" SIZE 12 ;
			TOOLTIP "Month Calendar Control NoTodayCircle WeekNumbers" ;
			NOTODAYCIRCLE ;
			WEEKNUMBERS ;
			TITLEBACKCOLOR BLACK ;
			TITLEFONTCOLOR YELLOW ;
			TRAILINGFONTCOLOR PURPLE ;
			ON CHANGE {||msginfo("Month_2 Change")} ;
			ON SELECT {||msginfo("Month_2 Select")}

		Form_1.Month_2.FontColor := RED
		Form_1.Month_2.BackColor := YELLOW

		@ 210,300 BUTTON Button_3 ;
			CAPTION "SET DATE" ;
	   		ACTION Form_1.Month_2.Value := CtoD( "01/01/2001" )

		@ 250,300 BUTTON Button_4 ;
			CAPTION "GET DATE" ;
  			ACTION MsgInfo ( GetDate ( Form_1.Month_2.Value ) ) 

		@ 290,300 BUTTON Button_6 ;
			CAPTION "GET FIRST WEEK DAY" ;
			WIDTH 160 ;
			ACTION MsgInfo ( Form_1.Month_2.FirstDayOfWeek )

	END WINDOW

	Form_1.Month_2.FirstDayOfWeek := NToCDoW( 1 ) // Sunday

	CENTER WINDOW Form_1

	ACTIVATE WINDOW Form_1

Return Nil

Static Function GetDate ( dDate )
Local nDay := Day( dDate )
Local nMonth := Month( dDate )
Local nYear := Year( dDate )
Local cRet := ""

	cRet += "Day: "+StrZero( nDay, 2 )
	cRet += Space( 2 )
	cRet += "Month: "+StrZero( nMonth, 2 )
	cRet += Space( 2 )
	cRet += "Year: "+StrZero( nYear, 4 )

Return cRet
