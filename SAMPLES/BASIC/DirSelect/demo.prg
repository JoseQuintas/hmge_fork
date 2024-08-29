/*
   MINIGUI - Harbour Win32 GUI library Demo

   Purpose is to test the function hb_DirScan() which allows scanning of a whole dir with subdirs.
   Also some other interesting HB_* functions to split filename from directory name.
*/

#include "minigui.ch"

FUNCTION Main()

   LOCAL cSMsg := ""
   LOCAL aHeaders := { 'Select', 'Path', 'Name', 'Bytes', 'Date', 'Time', 'Attr' }
   LOCAL aLijst := {}

   DEFINE WINDOW MainForm ;
      AT 90, 90 ;
      WIDTH 920 ;
      HEIGHT 550 ;
      TITLE 'Directory Scan for *.prg files' ;
      MAIN ;
      ON INIT Resize() ;
      ON MAXIMIZE Resize() ;
      ON SIZE Resize()

   DEFINE MAIN MENU of MainForm
      DEFINE POPUP "File"
         MENUITEM "Directory" ACTION DoFolder( "" )
         SEPARATOR
         MENUITEM "Exit" ACTION ThisWindow.Release
      END POPUP
      DEFINE POPUP "Help"
         MENUITEM "About" ACTION About()
      END POPUP
   END MENU

   DEFINE STATUSBAR FONT "Verdana" SIZE 10
      STATUSITEM cSMsg
      CLOCK WIDTH 90
      DATE WIDTH 90
   END STATUSBAR

   @ 005, 005 GRID Grid_1 ;
      WIDTH 1000 HEIGHT 425 ;
      HEADERS aHeaders ;
      WIDTHS { 20, 300, 244, 100, 80, 80, 50 } ;
      ITEMS aLijst ;
      FONT "Courier New" SIZE 10 ;
      FONTCOLOR BLUE ;
      ON DBLCLICK  SelectMe() ;
      LOCKCOLUMNS 1

      @ 440, 50 BUTTON Button_1 CAPTION "Select All" WIDTH 120 HEIGHT 28 ACTION SelectAll()

   END WINDOW

   MainForm.Center
   MainForm.Activate

RETURN NIL
//-----------------------
PROCEDURE About()

   DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 10

   AlertInfo( "hb_DirScan() Function Example;;" + MiniguiVersion() )

   RELEASE FONT DlgFont

RETURN
//-----------------------
PROCEDURE Resize()

   MainForm.Grid_1.Width := MainForm.Width - 25
   MainForm.Grid_1.Height := MainForm.Height - 125

   MainForm.Button_1.Row := MainForm.Height - 110 - GetBorderHeight() / 2

RETURN
//-----------------------
PROCEDURE SelectMe()

   LOCAL aLine := MainForm.Grid_1.Item( This.CellRowIndex )

   MainForm.Grid_1.Item( This.CellRowIndex ) := iif( Empty( aLine[1] ), { "x" }, { " " } )

   MainForm.Grid_1.Refresh()

RETURN
//-----------------------
PROCEDURE SelectAll()

   LOCAL i

   FOR i:=1 TO MainForm.Grid_1.ItemCount
      MainForm.Grid_1.Item( i ) := { "x" }
   NEXT

   MainForm.Grid_1.SetFocus()
   MainForm.Grid_1.Refresh()

RETURN
//------------------------
PROCEDURE DoFolder( cNewDir )

   STATIC cFolder := "", cNewFolder := "", cMsg := ""
   LOCAL cExePath := Left( ExeName(), RAt( "\", ExeName() ) )
   LOCAL aDir
   LOCAL k
   LOCAL aItem

   MainForm.Statusbar.Item( 1 ) := " "

   IF Empty( cNewDir )

      BEGIN INI File ( cExePath + "dirselect.ini" )
         GET cFolder SECTION "General" ENTRY "LastPath"
      END INI

      cNewFolder := GetFolder( "Select a Folder", iif( Empty( cFolder ), cExePath, cFolder ) )
   
   ELSE

      cNewFolder := cNewDir

   ENDIF

   IF !Empty( cNewFolder )

      cFolder := cNewFolder
      BEGIN INI File ( cExePath + "dirselect.ini" )
         SET SECTION "General" ENTRY "LastPath" TO cFolder
      END INI

   ENDIF

   IF !Empty( cFolder )

      IF !( DirChange( cFolder ) == 0 )    // changing to the directory in question to read it
         MsgStop( "Error Changing to " + cFolder )
         RETURN // get outta here - there's nothing left to do 4 u.
      ENDIF

      WAIT WINDOW "Scanning Directories" NOWAIT

      aDir := hb_DirScan( cFolder, "*.prg" )

      MainForm.StatusBar.Item( 1 ) := cFolder + " folder have " + hb_ntos( k := Len( aDir ) ) + " files"

      InkeyGUI( 100 )

      MainForm.Grid_1.DeleteAllItems()

      FOR EACH aItem IN aDir

         MainForm.Grid_1.Additem( { " ", ;
            hb_DirSepAdd( cFolder) + hb_FNameDir( aItem[1] ), ;
            hb_FNameNameExt( aItem[1] ), ;
            Transform( aItem[2], "999,999,999" ), ;
            DToC( aItem[3] ), aItem[4], aItem[5] } )

         cMsg := "Reading " + hb_ntos( Int( hb_enumIndex( aItem ) / k * 100 ) ) + "%"

         WAIT WINDOW cMsg NOWAIT

      NEXT

      InkeyGUI( 100 )

      WAIT CLEAR

      MainForm.Grid_1.Value := 1

   ELSE

      MsgStop( "No Folder Selected" )

   ENDIF

RETURN
