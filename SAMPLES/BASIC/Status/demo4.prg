/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2015 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

Procedure Main
	Local aItemsWidth := { 0, 100, 100, 100 }

	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 800 ;
		HEIGHT 600 ;
		TITLE 'Change Statusbar Item Colors and Align Demo' ;
		MAIN 

		DEFINE STATUSBAR FONT 'MS Sans Serif' SIZE 8

			STATUSITEM 'Fixed Item <&W>idth:' ACTION MsgInfo('Click! 1') FONTCOLOR BLUE
			STATUSITEM 'Item &2' WIDTH aItemsWidth[2] ACTION ChangeItem( 2 ) FONTCOLOR BLUE
			STATUSITEM 'Item &3' WIDTH aItemsWidth[3] ACTION ChangeItem( 3 ) FONTCOLOR BLUE
			STATUSITEM 'Item &4' WIDTH aItemsWidth[4] ACTION ChangeItem( 4 ) FONTCOLOR BLUE

		END STATUSBAR

	END WINDOW

	Form_1.StatusBar.Width(1) := Form_1.StatusBar.Width(1) + 32
	Form_1.StatusBar.Item(1) := Form_1.StatusBar.Item(1) + Str(Form_1.StatusBar.Width(1), 4)

	CENTER WINDOW Form_1

	ACTIVATE WINDOW Form_1

Return

*-------------------------------------------------------------
Procedure ChangeItem( nItem )
*-------------------------------------------------------------

	SET STATUSITEM nItem OF Form_1 FONTCOLOR TO RED
	SET STATUSITEM nItem OF Form_1 BACKCOLOR TO YELLOW
	SET STATUSITEM nItem OF Form_1 ALIGN TO CENTER

Return
