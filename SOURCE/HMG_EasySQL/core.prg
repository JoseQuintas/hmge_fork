/*
 * 'HMG EasySQL' a Simple HMG library To Handle MySql/MariaDB 'Things'
 *
 * *** EXPERIMENTAL CODE ***
 *
 * Copyright 2024 Roberto Lopez <mail.box.hmg@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

*---------------------------------------------------------------------------------------------*

#include 'hmg.ch'
#include "dbinfo.ch"
#include "hbclass.ch"

REQUEST SDDMY, SQLMIX

*---------------------------------------------------------------------------------------------*
CLASS SQL
*---------------------------------------------------------------------------------------------*

   // Data:

   DATA lShowMsgs
   DATA lTrace
   DATA cMsgLang
   DATA cNoQuoteChar

   DATA lError EXPORTED READONLY
   DATA cErrorDesc EXPORTED READONLY

   DATA aMsgs HIDDEN
   DATA cCommandBuffer HIDDEN
   DATA cCommandWhere HIDDEN
   DATA nConnHandle HIDDEN
   DATA cMessageWindowName HIDDEN
   DATA aWorkAreas HIDDEN

   // Methods:

   METHOD New()
   METHOD Connect( cServer, cUser, cPassword, cDatabase )
   METHOD Select( cCommand, cWorkArea )
   METHOD Insert( cTable, aHash )
   METHOD Update( cTable, cWhere, aHash )
   METHOD Delete( cTable, cWhere )
   METHOD Exec()
   METHOD AffectedRows()
   METHOD Disconnect()
   METHOD CloseAreas()
   METHOD Destroy()

   METHOD Field( cField, xExpression ) HIDDEN
   METHOD ShowMessage( cMessage ) HIDDEN
   METHOD HideMessage() HIDDEN

ENDCLASS

*---------------------------------------------------------------------------------------------*
METHOD SQL:New()
*---------------------------------------------------------------------------------------------*

   ::cMsgLang := Upper( Left( Set ( _SET_LANGUAGE ), 2 ) )
   ::aMsgs := {}
   ::aWorkAreas := {}
   ::cCommandBuffer := ''
   ::cCommandWhere := ''
   ::nConnHandle := 0
   ::lError := .F.
   ::cErrorDesc := ''
   ::lShowMsgs := .T.
   ::cMessageWindowName := '_HMG_EASYSQL_'
   ::lTrace := .F.
   ::cNoQuoteChar := '@'

   ::aMsgs := ASize( ::aMsgs, 14 )

   IF ::cMsgLang = 'EN' // English EN

      ::aMsgs[ 1 ] := 'SQL: Connecting...'
      ::aMsgs[ 2 ] := 'Connection Error!'
      ::aMsgs[ 3 ] := 'SQL: Processing...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE Column Data Type Error'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'DELETE cTable Param Type Error'
      ::aMsgs[ 8 ] := 'DELETE cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'UPDATE cTable Param Type Error'
      ::aMsgs[ 10 ] := 'UPDATE cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'UPDATE aHash Param Type Error'
      ::aMsgs[ 12 ] := 'EXEC Undefined Error'
      ::aMsgs[ 13 ] := 'SQL ERROR'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'ES' // Spanish ES

      ::aMsgs[ 1 ] := 'SQL: Conectando...'
      ::aMsgs[ 2 ] := 'Error de Conexi�n'
      ::aMsgs[ 3 ] := 'SQL: Procesando...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE Tipo de datos en columna inv�lido'
      ::aMsgs[ 5 ] := 'INSERT Tipo par�metro cTabla Inv�lido'
      ::aMsgs[ 6 ] := 'INSERT Error tipo par�metro aHash'
      ::aMsgs[ 7 ] := 'DELETE Error tipo par�metro cTabla'
      ::aMsgs[ 8 ] := 'DELETE Error tipo par�metro cWhere'
      ::aMsgs[ 9 ] := 'UPDATE Error tipo par�metro cTabla'
      ::aMsgs[ 10 ] := 'UPDATE Error tipo par�metro cWhere'
      ::aMsgs[ 11 ] := 'UPDATE Error tipo par�metro aHash'
      ::aMsgs[ 12 ] := 'EXEC Error indefinido'
      ::aMsgs[ 13 ] := 'ERROR SQL'
      ::aMsgs[ 14 ] := 'ShowMessage() Error tipo par�metro cMessage'

 /*

  From now, the messagess where Google-translated.
  Sorry for possible mistakes.
  Any help is welcome.

 */

   ELSEIF ::cMsgLang = 'PT' // Portuguese PT

      ::aMsgs[ 1 ] := 'SQL: Conectando...'
      ::aMsgs[ 2 ] := 'Erro de conex�o!'
      ::aMsgs[ 3 ] := 'SQL: Processando...'
      ::aMsgs[ 4 ] := 'Erro de tipo de dados da coluna INSERT/UPDATE'
      ::aMsgs[ 5 ] := 'Erro de tipo de par�metro INSERT cTable'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'DELETE erro de tipo de par�metro cTable'
      ::aMsgs[ 8 ] := 'DELETE cWhere Erro de tipo de par�metro'
      ::aMsgs[ 9 ] := 'ATUALIZAR erro de tipo de par�metro cTable'
      ::aMsgs[ 10 ] := 'ATUALIZAR erro de tipo de par�metro cWhere'
      ::aMsgs[ 11 ] := 'ATUALIZAR erro de tipo de par�metro aHash'
      ::aMsgs[ 12 ] := 'Erro EXEC indefinido'
      ::aMsgs[ 13 ] := 'ERRO SQL'
      ::aMsgs[ 14 ] := 'ShowMessage() Erro de tipo de par�metro'

   ELSEIF ::cMsgLang = 'FR' // French FR

      ::aMsgs[ 1 ] := 'SQL : Connexion...'
      ::aMsgs[ 2 ] := 'Erreur de connexion !'
      ::aMsgs[ 3 ] := 'SQL : Traitement...'
      ::aMsgs[ 4 ] := 'Erreur de type de donn�es de colonne INSERT/UPDATE'
      ::aMsgs[ 5 ] := 'INS�RER une erreur de type de param�tre cTable'
      ::aMsgs[ 6 ] := 'INS�RER une erreur de type de param�tre aHash'
      ::aMsgs[ 7 ] := 'DELETE Erreur de type de param�tre cTable'
      ::aMsgs[ 8 ] := 'DELETE cWhere Erreur de type de param�tre'
      ::aMsgs[ 9 ] := 'UPDATE Erreur de type de param�tre cTable'
      ::aMsgs[ 10 ] := 'MISE � JOUR cWhere Erreur de type de param�tre'
      ::aMsgs[ 11 ] := "MISE � JOUR d'une erreur de type de param�tre aHash"
      ::aMsgs[ 12 ] := 'Erreur EXEC non d�finie'
      ::aMsgs[ 13 ] := 'ERREUR SQL'
      ::aMsgs[ 14 ] := 'Erreur de type de param�tre ShowMessage()'

   ELSEIF ::cMsgLang = 'DE' // German DE

      ::aMsgs[ 1 ] := 'SQL: Verbindung wird hergestellt...'
      ::aMsgs[ 2 ] := 'Verbindungsfehler!'
      ::aMsgs[ 3 ] := 'SQL: Verarbeitung...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE-Spaltendatentypfehler'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'DELETE cTable Param Type Error'
      ::aMsgs[ 8 ] := 'DELETE cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'UPDATE cTable Param Type Error'
      ::aMsgs[ 10 ] := 'UPDATE cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'Fehler beim UPDATE des aHash-Parametertyps'
      ::aMsgs[ 12 ] := 'EXEC Undefinierter Fehler'
      ::aMsgs[ 13 ] := 'SQL-FEHLER'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'IT' // Italian IT

      ::aMsgs[ 1 ] := 'SQL: Connessione...'
      ::aMsgs[ 2 ] := 'Errore di connessione!'
      ::aMsgs[ 3 ] := 'SQL: Elaborazione...'
      ::aMsgs[ 4 ] := 'Errore tipo dati colonna INSERISCI/AGGIORNA'
      ::aMsgs[ 5 ] := 'Errore tipo parametro INSERT cTable'
      ::aMsgs[ 6 ] := 'INSERISCI errore tipo parametro aHash'
      ::aMsgs[ 7 ] := 'ELIMINA Errore tipo parametro cTable'
      ::aMsgs[ 8 ] := 'ELIMINA errore tipo parametro cWhere'
      ::aMsgs[ 9 ] := 'Errore tipo parametro UPDATE cTable'
      ::aMsgs[ 10 ] := 'AGGIORNAMENTO errore tipo parametro cWhere'
      ::aMsgs[ 11 ] := 'Errore tipo parametro AGGIORNAMENTO aHash'
      ::aMsgs[ 12 ] := 'Errore EXEC non definito'
      ::aMsgs[ 13 ] := 'ERRORE SQL'
      ::aMsgs[ 14 ] := 'Errore tipo parametro ShowMessage()'

   ELSEIF ::cMsgLang = 'NL' // Dutch NL

      ::aMsgs[ 1 ] := 'SQL: verbinden...'
      ::aMsgs[ 2 ] := 'Verbindingsfout!'
      ::aMsgs[ 3 ] := 'SQL: verwerking...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE Fout in kolomgegevenstype'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'VERWIJDER cTable parametertypefout'
      ::aMsgs[ 8 ] := 'VERWIJDER cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'UPDATE cTable parametertypefout'
      ::aMsgs[ 10 ] := 'UPDATE cWaar parametertypefout'
      ::aMsgs[ 11 ] := 'UPDATE aHash Param Type Error'
      ::aMsgs[ 12 ] := 'EXEC ongedefinieerde fout'
      ::aMsgs[ 13 ] := 'SQL-FOUT'
      ::aMsgs[ 14 ] := 'ShowMessage() Paramtypefout'

   ELSEIF ::cMsgLang = 'SV' // Swedish SV

      ::aMsgs[ 1 ] := 'SQL: Ansluter...'
      ::aMsgs[ 2 ] := 'Anslutningsfel!'
      ::aMsgs[ 3 ] := 'SQL: Bearbetar...'
      ::aMsgs[ 4 ] := 'INFOGA/UPPDATERA kolumndatatypfel'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'DELETE cTable Param Type Error'
      ::aMsgs[ 8 ] := 'DELETE cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'UPDATERA cTable Param Type Error'
      ::aMsgs[ 10 ] := 'UPPDATERA cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'UPDATE aHash Param Type Error'
      ::aMsgs[ 12 ] := 'EXEC odefinierat fel'
      ::aMsgs[ 13 ] := 'SQL-FEL'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'DA' // Danish DA

      ::aMsgs[ 1 ] := 'SQL: Opretter forbindelse...'
      ::aMsgs[ 2 ] := 'Forbindelsesfejl!'
      ::aMsgs[ 3 ] := 'SQL: Behandler...'
      ::aMsgs[ 4 ] := 'INDS�T/OPDATERE kolonnedatatypefejl'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'DELETE cTable Param Type Error'
      ::aMsgs[ 8 ] := 'SLET cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'OPDATERING cTabel Param Type Fejl'
      ::aMsgs[ 10 ] := 'OPDATERING cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'OPDATERING af aHash Param Type Fejl'
      ::aMsgs[ 12 ] := 'EXEC Udefineret fejl'
      ::aMsgs[ 13 ] := 'SQL-FEJL'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'FI' // Finnish FI

      ::aMsgs[ 1 ] := 'SQL: Yhdistet��n...'
      ::aMsgs[ 2 ] := 'Yhteysvirhe!'
      ::aMsgs[ 3 ] := 'SQL: K�sitell��n...'
      ::aMsgs[ 4 ] := 'LIS��/P�IVITYS sarakkeen tietotyyppivirhe'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'POISTA cTable Param Type Error'
      ::aMsgs[ 8 ] := 'POISTA cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'P�IVITYS cTable Param Type Error'
      ::aMsgs[ 10 ] := 'P�IVITYS cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'P�IVITYS aHash Param Type Error'
      ::aMsgs[ 12 ] := 'M��ritt�m�t�n EXEC-virhe'
      ::aMsgs[ 13 ] := 'SQL-VIRHE'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'NO' // Norwegian NO

      ::aMsgs[ 1 ] := 'SQL: Kobler til...'
      ::aMsgs[ 2 ] := 'Tilkoblingsfeil!'
      ::aMsgs[ 3 ] := 'SQL: Behandler...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE Column Data Type Feil'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'SLETT cTable Param Type Feil'
      ::aMsgs[ 8 ] := 'DELETE cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'UPDATE cTable Param Type Error'
      ::aMsgs[ 10 ] := 'OPPDATERING cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'UPDATE aHash Param Type Feil'
      ::aMsgs[ 12 ] := 'EXEC Udefinert feil'
      ::aMsgs[ 13 ] := 'SQL-FEIL'
      ::aMsgs[ 14 ] := 'ShowMessage() Param Type Error'

   ELSEIF ::cMsgLang = 'HU' // Hungarian HU

      ::aMsgs[ 1 ] := 'SQL: Csatlakoz�s...'
      ::aMsgs[ 2 ] := 'Kapcsol�d�si hiba!'
      ::aMsgs[ 3 ] := 'SQL: Feldolgoz�s...'
      ::aMsgs[ 4 ] := 'INSERT/UPDATE oszlop adatt�pus hiba'
      ::aMsgs[ 5 ] := 'INSERT cTable Param Type Error'
      ::aMsgs[ 6 ] := 'INSERT aHash Param Type Error'
      ::aMsgs[ 7 ] := 'cTable Param Type Error DELETE'
      ::aMsgs[ 8 ] := 'DELETE cWhere Param Type Error'
      ::aMsgs[ 9 ] := 'FRISS�T�S cTable Param Type Error'
      ::aMsgs[ 10 ] := 'FRISS�T�S cWhere Param Type Error'
      ::aMsgs[ 11 ] := 'Friss�tse az aHash param�tert�pus hib�t'
      ::aMsgs[ 12 ] := 'EXEC meghat�rozatlan hiba'
      ::aMsgs[ 13 ] := 'SQL HIBA'
      ::aMsgs[ 14 ] := 'ShowMessage() param�tert�pus hiba'

   ENDIF

   IF .NOT. _IsWindowDefined( ::cMessageWindowName )

      DEFINE WINDOW &( ::cMessageWindowName ) ;
            AT 0, 0 ;
            WIDTH 400 ;
            HEIGHT 40 ;
            TITLE '' ;
            CHILD ;
            NOSHOW ;
            NOSYSMENU ;
            NOCAPTION ;
            BACKCOLOR { 255, 255, 255 }

         DEFINE LABEL MESSAGE
            ROW 03
            COL 10
            WIDTH 380
            HEIGHT 25
            VALUE ''
            CENTERALIGN .T.
            BACKCOLOR { 255, 255, 255 }
            FONTBOLD .T.
         END LABEL

      END WINDOW

      DoMethod( ::cMessageWindowName, 'Center' )

   ENDIF

RETURN Self

*---------------------------------------------------------------------------------------------*
METHOD SQL:Destroy()
*---------------------------------------------------------------------------------------------*

   ::Disconnect()
   ::CloseAreas()

   ::aMsgs := NIL
   ::aWorkAreas := NIL
   ::cCommandBuffer := NIL
   ::cCommandWhere := NIL
   ::nConnHandle := NIL
   ::lError := NIL
   ::cErrorDesc := NIL
   ::lShowMsgs := NIL
   ::cMessageWindowName := NIL
   ::lTrace := NIL

RETURN NIL

*---------------------------------------------------------------------------------------------*
METHOD SQL:CloseAreas()
*---------------------------------------------------------------------------------------------*
   LOCAL i, n

   n := 0

   FOR i := 1 TO Len( ::aWorkAreas )
      IF Select( ::aWorkAreas[ I ] ) <> 0
         CLOSE ( ::aWorkAreas[ I ] )
         n++
      ENDIF
   NEXT i

RETURN n

*---------------------------------------------------------------------------------------------*
METHOD SQL:Connect( cServer, cUser, cPassword, cDatabase )
*---------------------------------------------------------------------------------------------*

   IF ::lShowMsgs
      ::ShowMessage( ::aMsgs[ 1 ] ) // 'SQL: Connecting...'
   ENDIF

   ::nConnHandle := rddInfo( RDDI_CONNECT, { "MYSQL", cServer, cUser, cPassword, cDatabase }, "SQLMIX" )

   IF ValType( ::nConnHandle ) <> 'N'
      ::nConnHandle := 0
   ENDIF

   IF ::nConnHandle == 0
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 2 ] // 'SQL: Connection Error!'
   ENDIF

   IF ::lShowMsgs
      ::HideMessage()
   ENDIF

   IF ::lError .AND. ::lShowMsgs
      MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:Disconnect()
*---------------------------------------------------------------------------------------------*

   ::lError := .F.
   ::cErrorDesc := ''

   rddInfo( RDDI_DISCONNECT,,, ::nConnHandle )

RETURN NIL

*---------------------------------------------------------------------------------------------*
METHOD SQL:Select( cCommand, cWorkArea )
*---------------------------------------------------------------------------------------------*
   LOCAL oError

   ::lError := .F.
   ::cErrorDesc := ''

   IF ::lShowMsgs
      ::ShowMessage( ::aMsgs[ 3 ] ) // 'SQL: Processing...'
   ENDIF

   IF ::lTrace
      hb_MemoWrit( 'trace.log', cCommand, .F. )
   ENDIF

   TRY

      dbUseArea( .T., "SQLMIX", cCommand, cWorkArea,,,, ::nConnHandle )

   CATCH oError

      ::lError := .T.
      ::cErrorDesc := oError:Description

   END

   IF ::lShowMsgs
      ::HideMessage()
   ENDIF

   IF ::lError .AND. ::lShowMsgs
      MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
   ENDIF

   IF ! ::lError
      AAdd( ::aWorkAreas, cWorkArea )
   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:Field( cField, xExpression )
*---------------------------------------------------------------------------------------------*
   LOCAL cExpression, lRaw := .F.

   ::lError := .F.
   ::cErrorDesc := ''

   IF ValType( xExpression ) = 'D'
      cExpression := StrZero( Year( xExpression ), 4 ) + '-' + StrZero( Month( xExpression ), 2 ) + '-' + StrZero( Day( xExpression ), 2 )
   ELSEIF ValType( xExpression ) = 'C'
      cExpression := AllTrim( xExpression )
      IF Left( cExpression, 1 ) = ::cNoQuoteChar
         lRaw := .T.
         cExpression := Right( cExpression, Len( cExpression ) - 1 )
      ENDIF
      cExpression := StrTran( cExpression, "'", "''" )
      cExpression := StrTran( cExpression, "\", "\\" )
   ELSEIF ValType( xExpression ) = 'N'
      cExpression := AllTrim( Str( xExpression ) )
      lRaw := .T.
   ELSEIF ValType( xExpression ) = 'L'
      IF xExpression
         cExpression := '1'
      ELSE
         cExpression := '0'
      ENDIF
      lRaw := .T.
   ELSE
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 4 ] // SQL: Column Data Expression Type Error

      IF ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ENDIF

   IF ! ::lError
      IF lRaw
         ::cCommandBuffer += cField + " = " + cExpression + ' ' + ','
      ELSE
         ::cCommandBuffer += cField + " = " + "'" + cExpression + "'" + ','
      ENDIF
   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:Insert( cTable, aHash )
*---------------------------------------------------------------------------------------------*
   LOCAL aKeys, aValues, i

   IF ValType( cTable ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 5 ] // 'SQL: INSERT cTable Param Type Error'

      IF ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSEIF ValType( aHash ) <> 'H'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 6 ] // 'SQL: INSERT aHash Param Type Error'

      IF ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSE
      ::lError := .F.
      ::cErrorDesc := ''

      ::cCommandBuffer := ''
      ::cCommandWhere := ''

      ::cCommandBuffer += "INSERT INTO " + cTable + " SET "

      aKeys := hb_HKeys( aHash )

      aValues := hb_HValues( aHash )

      FOR i := 1 TO Len( aKeys )
         IF ! ::field( aKeys[ i ], aValues[ i ] )
            EXIT
         ENDIF
      NEXT i

      IF ! ::lError
         ::Exec()
      ENDIF
   ENDIF

RETURN ! ::lError

// ------------------------------------------------------------------------------------------*
METHOD SQL:Delete( cTable, cWhere )
   // ------------------------------------------------------------------------------------------*

   IF ValType( cTable ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 7 ] // 'SQL: DELETE cTable Param Type Error'
      IF ::lError .AND. ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSEIF ValType( cWhere ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 8 ] // 'SQL: DELETE cWhere Param Type Error'
      IF ::lError .AND. ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSE

      ::cCommandBuffer := ''
      ::cCommandWhere := ''

      ::cCommandBuffer += "DELETE FROM " + cTable + " WHERE " + cWhere

      ::Exec()

   ENDIF

RETURN ! ::lError

// ------------------------------------------------------------------------------------------*
METHOD SQL:AffectedRows()
   // ------------------------------------------------------------------------------------------*
   LOCAL nRetVal

   ::lError := .F.
   ::cErrorDesc := ''

   nRetVal := rddInfo( RDDI_AFFECTEDROWS,,, ::nConnHandle )

   IF ValType( nRetVal ) <> 'N'
      nRetVal := 0
      ::lError := .T.
   ENDIF

RETURN nRetVal

// ------------------------------------------------------------------------------------------*
METHOD SQL:Update( cTable, cWhere, aHash )
   // ------------------------------------------------------------------------------------------*
   LOCAL aKeys, aValues, i

   IF ValType( cTable ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 9 ] // 'SQL: UPDATE cTable Param Type Error'
      IF ::lError .AND. ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSEIF ValType( cWhere ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 10 ] // 'SQL: UPDATE cWhere Param Type Error'
      IF ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSEIF ValType( aHash ) <> 'H'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 11 ] // 'SQL: UPDATE aHash Param Type Error'
      IF ::lShowMsgs
         MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
      ENDIF
   ELSE
      ::cCommandBuffer := ''

      ::cCommandWhere := cWhere

      ::cCommandBuffer += "UPDATE " + cTable + " SET "

      aKeys := hb_HKeys( aHash )

      aValues := hb_HValues( aHash )

      FOR i := 1 TO Len( aKeys )
         IF ! ::field( aKeys[ i ], aValues[ i ] )
            EXIT
         ENDIF
      NEXT i

      IF ! ::lError
         ::Exec()
      ENDIF

   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:Exec( cCommand )
*---------------------------------------------------------------------------------------------*
   LOCAL cRDD

   IF ::lShowMsgs
      ::ShowMessage( ::aMsgs[ 3 ] ) // 'SQL:Processing...'
   ENDIF

   cRDD := rddSetDefault()

   rddSetDefault( "SQLMIX" )

   IF ValType( cCommand ) = 'U'

      IF Right( ::cCommandBuffer, 1 ) == ','
         ::cCommandBuffer := Left( ::cCommandBuffer, Len( ::cCommandBuffer ) - 1 )
      ENDIF

      IF .NOT. Empty( ::cCommandWhere )
         ::cCommandBuffer += 'WHERE ' + ::cCommandWhere
      ENDIF

      ::lError := .NOT. rddInfo( RDDI_EXECUTE, ::cCommandBuffer, , ::nConnHandle )

      IF ValType( ::lError ) <> 'L'
         ::lError := .T.
      ENDIF

      IF ::lTrace
         hb_MemoWrit( 'trace.log', ::cCommandBuffer, .F. )
      ENDIF

   ELSE

      ::lError := .NOT. rddInfo( RDDI_EXECUTE, cCommand, , ::nConnHandle )

      IF ValType( ::lError ) <> 'L'
         ::lError := .T.
      ENDIF

      IF ::lTrace
         hb_MemoWrit( 'trace.log', cCommand, .F. )
      ENDIF

   ENDIF

   IF ::lError
      ::cErrorDesc := rddInfo( RDDI_ERROR,,, ::nConnHandle )
      IF Empty( ::cErrorDesc )
         ::cErrorDesc := ::aMsgs[ 12 ]
      ENDIF
   ELSE
      ::cErrorDesc := ''
   ENDIF

   ::cCommandBuffer := ''

   rddSetDefault( cRDD )

   IF ::lShowMsgs
      ::HideMessage()
   ENDIF

   IF ::lError .AND. ::lShowMsgs
      MsgExclamation( ::cErrorDesc, ::aMsgs[ 13 ] )
   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:ShowMessage( cMessage )
*---------------------------------------------------------------------------------------------*

   IF ValType( cMessage ) <> 'C'
      ::lError := .T.
      ::cErrorDesc := ::aMsgs[ 14 ] // 'SQL: ShowMessage Param Type Error'
   ELSE
      ::lError := .F.
      ::cErrorDesc := ''
   ENDIF

   SetProperty( ::cMessageWindowName, 'Message', 'Value', cMessage )

   DoMethod( ::cMessageWindowName, 'Show' )

   IF ::lError .AND. ::lShowMsgs
      MsgExclamation( ::cErrorDesc, ::aMsgs[ 14 ] )
   ENDIF

RETURN ! ::lError

*---------------------------------------------------------------------------------------------*
METHOD SQL:HideMessage()
*---------------------------------------------------------------------------------------------*

   DoMethod( ::cMessageWindowName, 'Hide' )

RETURN NIL
