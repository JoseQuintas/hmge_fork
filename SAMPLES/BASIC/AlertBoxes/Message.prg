/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2019 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

Function Main

	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 640 HEIGHT 480 ;
		TITLE 'MiniGUI Alert Boxes Demo (Based Upon a Contribution Of Grigory Filatov)' ;
		ICON 'DEMO.ICO' ;
		MAIN ;
		FONT 'Arial' SIZE 10 

		DEFINE STATUSBAR
			STATUSITEM '[x] Harbour Power Ready!' 
		END STATUSBAR

		DEFINE MAIN MENU 

			POPUP 'AlertBoxes'

				ITEM 'Message Information'	ACTION	AlertInfo( "MessageBox Test" )

				ITEM 'Message Stop'		ACTION	AlertStop( "MessageBox Test" )

				ITEM 'Message Error'		ACTION	AlertExclamation( "MessageBox Test" )

				ITEM 'Message Box with 2 Buttons: Yes and No' ACTION MsgInfo( AlertYesNo( "MessageBox Test", "AlertYesNo" ) )

				ITEM 'Message Box with 2 Buttons: Yes and No reverted' ACTION MsgInfo( AlertYesNo( "MessageBox Test", "AlertYesNo", .T. ) )

				ITEM 'Message Box with 2 Buttons: Ok and Cancel' ACTION MsgInfo( AlertOKCancel( "MessageBox Test", "AlertOKCancel" ) )

				ITEM 'Message Box with 2 Buttons: Retry and Cancel' ACTION MsgInfo( AlertRetryCancel( "MessageBox Test", "AlertRetryCancel" ) )

				ITEM 'Message Box with 3 Buttons: Yes, No and Cancel' ACTION MsgInfo( AlertYesNoCancel( "MessageBox Test", "AlertYesNoCancel" ) )

				ITEM 'Message Box with 3 Buttons: Abort, Retry and Ignore' ACTION MsgBoxDemo()

			    	SEPARATOR	

				ITEM '&Exit'		ACTION Form_1.Release

			END POPUP

			POPUP 'AlertRetryCancel'

				ITEM 'Message Box with 2 Buttons: Retry and Cancel and Exclamation Icon' ;
					ACTION AlertRetryCancel( "This have Exclamation Icon", "With icons...", , "alert.ico" )

				ITEM 'Message Box with 2 Buttons: Retry and Cancel and Focus on Cancel' ;
					ACTION AlertRetryCancel( "This have Focus on CANCEL button", "With focus...", 2 )

				ITEM 'Message Box with 2 Buttons: Retry and Cancel and TopMost Dialog' ;
					ACTION AlertRetryCancel( "This is a TOPMOST DIALOG", "TopMost...", , , , , .T. )

			END POPUP

			POPUP '&Help'

				ITEM '&About'		ACTION AlertInfo ( "MiniGUI Alert Boxes demo", , "demo.ico" )

			END POPUP

		END MENU

	END WINDOW

	CENTER WINDOW Form_1

	ACTIVATE WINDOW Form_1

Return Nil


Function msgboxdemo()
   local nRet := 0

   _HMG_ModalDialogReturn := 0

   while ( nRet := HMG_Alert( "Please choose IGNORE", { "Abort", "Retry", "Ignore" }, "Please, choose..." ) ) == 2
      _HMG_ModalDialogReturn := 0
   enddo

   if nRet == 1

      Tone( 600, 2 )

      Form_1.Release

   endif

Return Nil
