/*
demoall.prg
*/

#include "minigui.ch"

STATIC aMenuOptions := {}, nMenuLevel := 0

PROCEDURE Demot

   LOCAL xDlg, aList, aInitList := {}, aEndList := {}

   SET EXCLUSIVE OFF
   SET CENTURY ON

   aList := ze_CreateMenu( aInitList, aEndList )

   DEFINE WINDOW ( xDlg := ze_Name( "DLG" ) ) ;
      AT 0, 0 ;
      WIDTH 1024 ;
      HEIGHT 768 ;
      MAIN;
      TITLE "demoall.prg" ;
      ON INIT Eval( { || ze_ListEval( aInitList ) } ) ;
      ON RELEASE Eval( { || ze_ListEval( aEndList ) } ) ;

   END WINDOW

   demo_BuildTabs( xDlg, aList, 1, aInitList, aEndList )

   DoMethod( xDlg, "CENTER" )
   DoMethod( xDlg, "ACTIVATE" )

   RETURN

STATIC FUNCTION Ze_CreateMenu( aInitList, aEndList )

   MenuOption( "Button",                              { |o| DemoButton( .F., o ) } )
   MenuOption( "None",                                { || Nil } )
   MenuOption( "Button",                              { |o| DemoButton( .F., o ) } )
   MenuOption( "None",                                { || Nil } )
   MenuOption( "Button",                              { |o| DemoButton( .F., o ) } )
   MenuOption( "None",                                { || Nil } )
   MenuOption( "Button",                              { |o| DemoButton( .F., o ) } )
   MenuOption( "None",                                { || Nil } )
   MenuOption( "Button",                              { |o| DemoButton( .F., o ) } )

   (aInitList);(aEndList)

   RETURN aMenuOptions

FUNCTION MenuOption( cCaption, bCode )

   LOCAL nCont, aLastMenu

   aLastMenu := aMenuOptions
   FOR nCont = 1 TO nMenuLevel
      aLastMenu := aLastMenu[ Len( aLastMenu ) ]
      aLastMenu := aLastMenu[ 2 ]
   NEXT
   AAdd( aLastMenu, { ccaption, {}, bCode } )

   RETURN Nil

FUNCTION MenuDrop()

   nMenuLevel++

   RETURN Nil

FUNCTION MenuUndrop()

   nMenuLevel--

   RETURN Nil

FUNCTION ze_name( cTipo )

   STATIC nIndex := 0

   nIndex += 1

   RETURN cTipo + Ltrim( Str( nIndex ) )

FUNCTION ze_ListEval( aInitList )

   LOCAL bCode

   FOR EACH bCode IN aInitList
      Eval( bCode )
   NEXT

   RETURN Nil
