/*
demo_buildtabs.prg
*/

#include "minigui.ch"
#include "i_winuser.ch"
#include "tsbrowse.ch"

FUNCTION demo_BuildTabs( xDlg, aOptionList )

   LOCAL aOption, xTab

   DEFINE TAB ( xTab := ze_Name( "DLG" ) ) ;
      OF ( xDlg ) ;
      AT 10, 10 ;
      WIDTH 900 ;
      HEIGHT 600

      FOR EACH aOption IN aOptionList

         PAGE aOption[ 1 ]

            Eval( aOption[ 3 ], xDlg )

         END PAGE

      NEXT

   END TAB

   (xTab)

   RETURN Nil
