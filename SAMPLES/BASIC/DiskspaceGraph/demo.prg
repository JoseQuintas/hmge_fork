/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2005-2015 Grigory Filatov <gfilatov@inbox.ru>
 */

ANNOUNCE RDDSYS

#include "minigui.ch"
#include "fileio.ch"

PROCEDURE Main
/*
 *  This procedure is the main entry point of the application.
 *  It defines and activates a main window that displays a pie chart
 *  representing disk space usage.
 *
 *  Input parameters: None
 *  Return Value: None
 */

   LOCAL cDisk := CurDrive() + ":\"

   SET DECIMALS TO
   SET FONT TO "Arial", 10

   DEFINE WINDOW Win_1 ;
         AT 0, 0 WIDTH 220 HEIGHT 240 ;
         TITLE cDisk ;
         ICON "HARD" ;
         MAIN NOMAXIMIZE NOSIZE ;
         ON INIT ShowPie( cDisk )

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   Win_1.Center()
   Win_1.Activate()

RETURN


PROCEDURE ShowPie( cDisk )
/*
 *  This procedure creates and displays a pie chart within a specified window,
 *  showing the used and free space of a given disk drive.
 *
 *  Input parameters:
 *      cDisk: A character string representing the disk drive (e.g., "C:\").
 *  Return Value: None
 */

   LOCAL iFree := hb_DiskSpace( cDisk, HB_DISK_FREE ) / ( 1024 * 1024 )
   LOCAL iTotal := hb_DiskSpace( cDisk, HB_DISK_TOTAL ) / ( 1024 * 1024 )
   LOCAL iUsed := iTotal - iFree
   LOCAL lFlag := ( iUsed > iFree )

   DRAW GRAPH IN WINDOW Win_1 ;
      AT 10, 10 ;
      TO 200, 200 ;
      TITLE "" ;
      TYPE PIE ;
      SERIES { iUsed, iFree } ;
      DEPTH 10 ;
      SERIENAMES { "Espacio utilizado", "Espacio libre" } ;
      COLORS { BLUE, FUCHSIA } ;
      3DVIEW ;
      SHOWLEGENDS

   DRAW TEXT IN WINDOW Win_1 ;
      AT iif( lFlag, 60, 90 ), 84 ;
      VALUE "Libre " + hb_ntos( iFree / iTotal * 100 ) + "%" ;
      FONT "MS Sans Serif" SIZE 9 ;
      FONTCOLOR iif( lFlag, WHITE, BLACK ) ;
      TRANSPARENT

RETURN
