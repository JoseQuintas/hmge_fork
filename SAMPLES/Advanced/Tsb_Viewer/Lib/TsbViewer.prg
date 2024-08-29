/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр/правка Dbf файла. Опции/свойства по базе. Объекты на окне таблицы
 * View/edit Dbf file. Options/properties by base. Objects on the table window
*/

//#define  _HMG_OUTLOG           // вывод отладки в файл
#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"

#define  SHOW_TITLE  "TsbViewer(c)"
#define  SHOW_VERS   SPACE(5) + "Ver 0.6 - 06.01.22"
///////////////////////////////////////////////////////////////////////////////
FUNCTION TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsbW, aEvent)
   LOCAL cWait, cAlias, nY, nX, nW, nH, lCenter, aBClr
   LOCAL c1Title, c2Title, aTsbPar, aWinPar

   nY      := oWin:nPosY
   nX      := oWin:nPosX
   nW      := oWin:nPosW
   nH      := oWin:nPosH
   lCenter := oWin:lCenter
   aBClr   := oWin:aBackcolor
   cWait   := IIF( oWin:lWait, "WAIT", "NOWAIT" )
   cAlias  := oUse:cAlias
   c1Title := oTsbW:cSupHd1Title
   c2Title := oTsbW:cSupHd2Title

   aTsbPar := { cAlias, c1Title, c2Title }
   aWinPar := { cWait, "", nY, nX, nW, nH, lCenter, aBClr }
   TsbViewer( aTsbPar, aWinPar, oUse, oIndx, oMenu, oTsbW, aEvent)

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION TsbViewer( aTsbPar, aWinPar, oUse, oIndx, oMenu, oTsbW, aEvent)
   //TsbViewer(cWait, cAlias, cDbfCP, c2Title, nY, nX, nW, nH, lCenter)
   LOCAL nWBrw, nHBrw, nYBrw, nXBrw, nWGaps, nHGaps, cForm, oThis, lTsb2
   LOCAL cTitle, cIcon, lModal, oTsb, cSetCP, cSelCdp, cLngSel, lTsb
   LOCAL cWait, cAlias, cDbfCP, c2Title, nY, nX, nW, nH, lCenter, aBClr
   LOCAL nSel, cErr, aPosiW, aEve, cPrev := ALIAS()
   LOCAL oBrw   // наилучшее решение для объекта
   DEFAULT aTsbPar := {}, aWinPar := {}
   DEFAULT oUse := oHmgData(), oIndx := oHmgData()
   DEFAULT oMenu := oHmgData(), oTsbW := oHmgData()
   DEFAULT aEvent := {}

   SET MSGALERT BACKCOLOR TO { 183, 221, 232 }          // for HMG_Alert()

   IF LEN(aWinPar) == 0
      cWait   := "NOWAIT"
      cTitle  := ""
      nY      := nX := 0
      nW      := System.ClientWidth
      nH      := System.ClientHeight
      lCenter := .T.
      aBClr   := Nil
   ELSE
      IF LEN(aWinPar) < 6
         cErr := "Error ! Wrong number of parameters !;"
         cErr += "LEN(aWinPar) == " + HB_NtoS(LEN(aWinPar))
         cErr += " # aWinPar[8];"
         cErr += "aWinPar := { 'NOWAIT', '', nY, nX, nW, nH, .T./.F., COLOR };;"
         AlertStop(cErr + ProcNL(),"ERROR")
      ENDIF
      cWait   := aWinPar[1]
      cTitle  := aWinPar[2]
      nY      := aWinPar[3]
      nX      := aWinPar[4]
      nW      := aWinPar[5]
      nH      := aWinPar[6]
      IF LEN(aWinPar) < 7  ; lCenter := .T.
      ELSE                 ; lCenter := aWinPar[7]
      ENDIF
      IF LEN(aWinPar) < 8  ; aBClr   := Nil
      ELSE                 ; aBClr   := aWinPar[8]
      ENDIF
      IF nW == 0
         nW := System.ClientWidth
      ENDIF
      IF nH == 0
         nH := System.ClientHeight
      ENDIF
   ENDIF
   lCenter := IIF( lCenter == NIL, .F. , lCenter )
   IF LEN(aTsbPar) == 0
      cAlias  := ALIAS()
      cDbfCP  := "???"
      c2Title := SHOW_TITLE
   ELSE
      cAlias  := aTsbPar[1]
      cDbfCP  := aTsbPar[2]
      c2Title := aTsbPar[3]
   ENDIF

   cForm   := "HMG_" + cAlias + "_" + HB_NtoS( _GetId() )
   cIcon   := Icon32TempCreate()      // см. ниже
   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()
   lTsb    := lTsb2 := .T.
   aPosiW  := { 0, 0, 0, 0 }

   IF LEN(cTitle) == 0 .AND. LEN(cAlias) > 0
      cTitle := cAlias + " " + DBINFO( DBI_FULLPATH ) + " " + RddName()
      cTitle += " [" + cSetCP + "/" + cSelCdp + "/" + cLngSel + "]"
   ENDIF

   IF Empty( _HMG_MainHandle )             // --- Main window ---
      AlertStop("There is no MAIN window in the program !;;"+ProcNL()+";"+ProcNL(1))
      RETURN NIL
   ENDIF

   // если было вызвано последнее окно MODAL, то вернёт .T.
   lModal := _HMG_IsModalActive

   IF lModal                               // --- MODAL window ---
      DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle    ;
         ICON cIcon MODAL NOSIZE BACKCOLOR aBClr                       ;
         ON GOTFOCUS myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel)      ; // возврат фокуса на форму
         ON INIT    {|| This.Topmost := .F., iif(oBrw==Nil, Nil, oBrw:Setfocus()) }  ;
         ON RELEASE {|| iif( !Empty(cPrev), dbSelectArea(cPrev), Nil ) }
   ELSE                                    // --- STANDARD window ---
      DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle    ;
         ICON cIcon WINDOWTYPE STANDARD TOPMOST  NOMAXIMIZE NOSIZE     ;
         BACKCOLOR aBClr                                               ;
         ON GOTFOCUS myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel)      ; // возврат фокуса на форму
         ON INIT    {|| This.Topmost := .F., iif(oBrw==Nil, Nil, oBrw:Setfocus()) }  ;
         ON RELEASE {|| iif( !Empty(cPrev), dbSelectArea(cPrev), Nil ) } ;
         ON INTERACTIVECLOSE {|| MG_YesNoQuit() }
   ENDIF

      oThis              := This.Object
      cForm              := This.Name
      This.Cargo         := oHmgData()           // контейнер для ЭТОГО окна
      This.Cargo:cMenu   := "Table"
      This.Cargo:aObjBtn := {}
      This.Cargo:cForm   := cForm
      This.Cargo:oUse    := oUse
      This.Cargo:oIndx   := oIndx
      This.Cargo:oMenu   := oMenu
      This.Cargo:oTsbW   := oTsbW
      nW                 := This.ClientWidth
      nH                 := This.ClientHeight

      // отступ по ширине формы
      IF hb_IsNumeric(oTsbW:nWGaps)
         nWGaps := oTsbW:nWGaps
      ELSE
         nWGaps := GetBorderWidth()
      ENDIF
      // отступ по высоте формы
      IF hb_IsNumeric(oTsbW:nHGaps)
         nHGaps := oTsbW:nHGaps
      ELSE
         nHGaps := GetBorderHeight()
      ENDIF
      // Проверка на ошибки
      IF hb_IsString(oTsbW:cError) .AND. LEN(oTsbW:cError) > 0
         lTsb := .F.  // ошибка
      ENDIF
      IF hb_IsString(oIndx:cError) .AND. LEN(oIndx:cError) > 0
         lTsb := .F.  // ошибка
      ENDIF

      IF lTsb  // если нет ошибки
         aPosiW := TsbButtonMenu(oMenu,nWGaps,nHGaps,oThis)   // меню кнопок для таблицы
      ENDIF

      nYBrw  := nHGaps + aPosiW[1]
      nXBrw  := nWGaps + aPosiW[2]
      nWBrw  := nW - nWGaps*2 + aPosiW[3]
      nHBrw  := nH - nHGaps*2 + aPosiW[4]

      lTsb2 := .F.
      IF HB_ISCHAR( cAlias )
         IF LEN(cAlias) > 0
            nSel := Select(cAlias)
            IF nSel > 0
               dbSelectArea( cAlias )
               lTsb2 := .T.
            ENDIF
         ENDIF
      ENDIF

      IF lTsb .AND. lTsb2 // если нет ошибки
         IF oIndx:nSetOrder # NIL
            DbSetOrder(oIndx:nSetOrder)
         ENDIF
         DbGotop()
         oTsb   := Tbrowse_OnInit(cAlias, cDbfCP, c2Title, oTsbW)  // начальные данные для таблицы
         oBrw   := _TBrowse( oTsb, cAlias, , nYBrw, nXBrw, nWBrw, nHBrw )
         Tbrowse_Customization(oBrw, oTsb)           // донастройка таблицы

         This.Cargo:oBrw  := oBrw                    // запомнить объект таблицы
         oTsb:aColFilter  :=  {}                     // колонки таблицы с фильтром
         oTsb:aColNumFltr :=  {}                     // номера колонок таблицы с фильтром

         DO EVENTS
         oBrw:SetFocus()
         DO EVENTS
      ELSE
        cErr := "ERROR ! " + ProcNL() + ";;"
        cErr += oUse:cError  + ";;"
        cErr += oIndx:cError + ";;"
        cErr += oTsbW:cError
        cErr := ATREPL( ";", cErr , CRLF )

        @ nHGaps, nWGaps LABEL Label_Err WIDTH nW-nWGaps*2 HEIGHT nH-nHGaps*2 ;
          VALUE cErr FONTCOLOR RED BOLD TRANSPARENT //CENTERALIGN

      ENDIF

      IF LEN(aEvent) > 0           // события на окне
         WITH OBJECT This.Object
            FOR EACH aEve IN aEvent
               :Event( aEve[1], aEve[2] )
            NEXT
         END WITH
      ENDIF

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|| iif(oBrw==Nil, ThisWindow.Release,;
                                    iif( oBrw:IsEdit, oBrw:SetFocus(), ThisWindow.Release ) ) }
   END WINDOW

   IF lCenter
      CENTER WINDOW &cForm
   ENDIF

   IF UPPER(cWait) == "NOWAIT"
      ACTIVATE WINDOW &cForm NOWAIT  ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                                 This.Restore , DoEvents() }
   ELSE
      ACTIVATE WINDOW &cForm ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                         This.Restore , DoEvents() }
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel)
   DO EVENTS
   hb_SetCodepage(cSetCP)
   hb_CdpSelect(cSelCdp)
   hb_LangSelect(cLngSel)
   IF LEN(cAlias) > 0
      dbSelectArea( cAlias )
   ENDIF
   DO EVENTS
RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
FUNCTION MG_YesNoQuit()
   LOCAL aColors, aOptions, nRet, lRet, nType, xIcon, nSize, bInit, lClosable
   LOCAL cTitle, aLang, cMsg

   _HMG_ModalDialogReturn := 1
   aLang     := myLangeRes(3)
   cMsg      := aLang[1]
   cTitle    := aLang[2]
   aColors   := { LGREEN, RED   , YELLOW }
   aOptions  := { '&' + aLang[3], '&' + aLang[4] , '&QuitExe' }
   nType     := NIL
   xIcon     := NIL
   nSize     := NIL
   bInit     := {|| This.TopMost := .T. }
   lClosable := .F.

   nRet := HMG_Alert( cMsg, aOptions, cTitle, nType, xIcon, nSize, aColors, bInit, lClosable )
   IF     nRet == 1
      lRet := .T.
   ELSEIF nRet == 2
      lRet := .F.
   ELSEIF nRet == 3
      //DbCloseAll()
      ReleaseAllWindows()
   ENDIF

RETURN lRet

////////////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_OnInit(cAls, c1Title, c2Title, oTsbW)  // начальные данные для таблицы
   LOCAL cTitle, oTsb, cFont1, cFont2, cFont3, cFont5, cFont6, nFS5, nClrFace
   LOCAL nClrFocus1, nClrFocus2, nClrNoFocus1, nClrNoFocus2, nClrSeleF, nClrNoEdit
   DEFAULT c1Title := hb_SetCodepage()

   cFont1 := "Tsb_Normal_" + cAls
   IF Empty( GetFontHandle( cFont1 ) )
      DEFINE FONT &cFont1 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize  //CHARSET 0
   ENDIF
   cFont2 := "Tsb_Bold_" + cAls
   IF Empty( GetFontHandle( cFont2 ) )
      DEFINE FONT &cFont2 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD  //CHARSET 0
   ENDIF
   cFont3 := "Tsb_Italic_" + cAls
   IF Empty( GetFontHandle( cFont3 ) )
      DEFINE FONT &cFont3 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize-4 BOLD ITALIC  //CHARSET 0
   ENDIF
   cFont5 := "Tsb_SuperHd_" + cAls
   IF Empty( GetFontHandle( cFont5 ) )
      nFS5 := INT( _HMG_DefaultFontSize * 1.2 )
      DEFINE FONT &cFont5 FONTNAME _HMG_DefaultFontName SIZE nFS5 BOLD   //CHARSET 0
   ENDIF
   cFont6 := "Tsb_Edit_" + cAls
   IF Empty( GetFontHandle( cFont6 ) )
      DEFINE FONT &cFont6 FONTNAME "Tahoma" SIZE _HMG_DefaultFontSize BOLD   //CHARSET 0
   ENDIF

   oTsb := oHmgData()  // названия переменных смотреть ф-ю _TBrowse()
   // фонты таблицы    1-Cells, 2-Headers, 3-Footers, 4-SpecHeader, 5-SuperHeader, 6-Edit
   oTsb:aFont     := { cFont1 , cFont2   , cFont2   , cFont3      , cFont5       , cFont6  }

   IF hb_IsObject(oTsbW)
      IF oTsbW:cSupHd1Title # NIL
         c1Title := oTsbW:cSupHd1Title
      ENDIF
      IF oTsbW:cSupHd2Title # NIL
         c2Title := oTsbW:cSupHd2Title
      ENDIF
      oTsb:aHead   := oTsbW:aHead
      oTsb:aFoot   := oTsbW:aFoot
      oTsb:aPict   := oTsbW:aPict
      oTsb:aName   := oTsbW:aName
      oTsb:aAlign  := oTsbW:aAlign
      oTsb:aField  := oTsbW:aField
      oTsb:aFSize  := oTsbW:aFSize
      oTsb:aFAlign := oTsbW:aFAlign
      oTsb:aEdit   := oTsbW:aEdit
   ENDIF
   // цвета таблицы
   nClrFace         := GetSysColor( COLOR_BTNFACE )
   oTsb:aBrush      := IIF( oTsbW:aBrush==Nil   , nClrFace, oTsbW:aBrush    )  // под таблицей
   oTsb:nClrNoDbf   := IIF( oTsbW:nClrNoDbf==Nil, nClrFace, oTsbW:nClrNoDbf )  // селектор/нумератор
   oTsb:nForeNoDbf  := CLR_RED                                                 // селектор/нумератор
   nClrNoEdit       := RGB(242,163,167)                                        // шапка/подвал колонок типа "+=^"
   oTsb:nClrNoEdit  := IIF( oTsbW:nClrNoEdit==Nil , nClrNoEdit, oTsbW:nClrNoEdit )
   oTsb:nClr22Bck   := IIF( oTsbW:nClr2Back==Nil  , CLR_GRAY , oTsbW:nClr22Bck   ) // нечетные строки
   oTsb:nClrBackDel := IIF( oTsbW:nClrBackDel==Nil, RGB(50,50,50), oTsbW:nClrBackDel ) // фона удалённых записей
   oTsb:nClrForeDel := IIF( oTsbW:nClrForeDel==Nil, CLR_WHITE , oTsbW:nClrForeDel ) // текст удалённых записей
   oTsb:lShowZebra  := Nil               // НЕТ показа чётная\нечётная строка
   oTsb:nClr1Fore   := IIF( oTsbW:nClr1Fore==Nil  , CLR_BLACK , oTsbW:nClr1Fore  )  // 1 , текст в ячейках таблицы
   oTsb:nClr2Back   := IIF( oTsbW:nClr2Back==Nil  , CLR_WHITE , oTsbW:nClr2Back  )  // 2 , фон   в ячейках таблицы
   oTsb:nClr3Fore   := IIF( oTsbW:nClr3Fore==Nil  , CLR_BLACK , oTsbW:nClr3Fore  )  // 3 , текста шапки таблицы
   oTsb:nClr4Back   := IIF( oTsbW:nClr4Back==Nil  , nClrFace  , oTsbW:nClr4Back  )  // 4 , фона шапки таблицы
   oTsb:nClr9Fore   := IIF( oTsbW:nClr9Fore==Nil  , CLR_RED   , oTsbW:nClr9Fore  )  // 9 , текста подвала таблицы
   oTsb:nClr10Back  := IIF( oTsbW:nClr10Back==Nil , nClrFace  , oTsbW:nClr10Back )  // 10, фона подвала таблицы
   oTsb:nClr16Back  := IIF( oTsbW:nClr16Back==Nil , RGB(183,221,232), oTsbW:nClr16Back )  // 16, фона суперхидера
   oTsb:nClr17Fore  := IIF( oTsbW:nClr17Fore==Nil , CLR_RED   , oTsbW:nClr17Fore )  // 17, текста суперхидера
   oTsb:n1Clr16Back := IIF( oTsbW:n1Clr16Back==Nil , RGB(84,141,212), oTsbW:n1Clr16Back )  // 16, фона суперхидера колонка 1
   oTsb:n1Clr17Fore := IIF( oTsbW:n1Clr17Fore==Nil , CLR_YELLOW, oTsbW:n1Clr17Fore )       // 17, текста суперхидера колонка 1
   // цвета курсора
   nClrFocus1   := IIF( hb_IsNumeric(oTsbW:nClrFocus1  ), oTsbW:nClrFocus1  , -CLR_HRED                       )
   nClrFocus2   := IIF( hb_IsNumeric(oTsbW:nClrFocus2  ), oTsbW:nClrFocus2  , -RGB(1,1,1)                     )
   nClrSeleF    := IIF( hb_IsNumeric(oTsbW:nClrSeleF   ), oTsbW:nClrSeleF   , GetSysColor( COLOR_WINDOWTEXT ) )
   nClrNoFocus1 := IIF( hb_IsNumeric(oTsbW:nClrNoFocus1), oTsbW:nClrNoFocus1, -CLR_BLUE                       )
   nClrNoFocus2 := IIF( hb_IsNumeric(oTsbW:nClrNoFocus2), oTsbW:nClrNoFocus2, -RGB( 128, 225, 225 )           )

   oTsb:aColor := { ;
          { CLR_FOCUSB, {|c,n,b| c := n, iif( b:nCell == n, nClrFocus1, nClrFocus2 ) } }, ;
          { CLR_SELEF , nClrSeleF }, ;
          { CLR_SELEB , {|c,n,b| c := n, iif( b:nCell == n, nClrNoFocus1, nClrNoFocus2 ) } }  ;
         }

   IF hb_IsLogical(oTsbW:lShowZebra)        // показ чётная\нечётная строка
      oTsb:lShowZebra := oTsbW:lShowZebra
   ENDIF
   IF hb_IsNumeric(oTsbW:nClr22Bck)
      oTsb:nClr22Bck := oTsbW:nClr22Bck   // цвет чётная\нечётная row
   ENDIF

   // настройки таблицы
   IF hb_IsLogical(oTsbW:lSelector)
      oTsb:uSelector := oTsbW:lSelector
      IF !oTsbW:lSelector
         oTsb:uSelector := Nil
      ENDIF
   ELSE
      oTsb:uSelector := .T.   // всегда поставить в таблице вирт.колонку SELECTOR
   ENDIF

   IF hb_IsLogical(oTsbW:lColNumber)
      IF oTsbW:lColNumber
         IF hb_IsArray(oTsbW:aColNumber)
            oTsb:aNumber := oTsbW:aColNumber
         ELSE
            oTsb:aNumber := { 1, 40 }  // если нет oTsbW:lColNumber
         ENDIF
      ELSE
         oTsb:aNumber := Nil
      ENDIF
   ELSE
      oTsb:aNumber := { 1, 40 }  // всегда поставить в таблице вирт.колонку ORDKEYNO
   ENDIF

   IF hb_IsLogical(oTsbW:lSuperHead)
      oTsb:lSuperHead := oTsbW:lSuperHead
   ELSE
      oTsb:lSuperHead := .T.   // всегда поставить в таблице суперхидер - мой ключ
   ENDIF

   IF hb_IsLogical(oTsbW:lSpecHd)
      oTsb:lSpecHd := oTsbW:lSpecHd
   ELSE
      oTsb:lSpecHd := .T.   // всегда поставить в таблице нумератор
   ENDIF

   IF hb_IsLogical(oTsbW:lFooting)
      oTsb:lFooting := oTsbW:lFooting
   ELSE
      oTsb:lFooting := .T.   // всегда поставить в таблице подвал
   ENDIF

   IF hb_IsLogical(oTsbW:lDrawDelAll)
      oTsb:lDrawDelAll := oTsbW:lDrawDelAll    // убрать всё: шапку/подвал/суперхидер/нумератор - мой ключ
   ENDIF

   IF hb_IsNumeric(oTsbW:nHeightHead)
      oTsb:nHeightHead := oTsbW:nHeightHead    // высота шапки
   ENDIF

   IF hb_IsLogical(oTsbW:lAdjust)
      oTsb:lAdjust := oTsbW:lAdjust       // убрать добавление пробелов в колонки для растяжки ширины в экран
   ENDIF

   // oTsb:aEdit - массив правки ячеек по колонкам, смотреть функцию _TBrowse()
   // варианты: Nil и  .F.-правка запрещена, .T.-правка разрешена
   // если oTsb:aEdit := oTsbW:aEdit то смотреть приходящий массив
   oTsb:aEdit := IIF( oTsbW:aEdit==Nil, .T., oTsbW:aEdit )
   oTsb:aFoot := IIF( oTsbW:aFoot==Nil, .T., oTsbW:aFoot )

   // -------------------- проверка ------------------
   // В таблице суперхидер/шапка/нумератор/подвал связаны вместе, показ поотдельности невозможен
   //oTsb:lDrawDelAll := .T.   // убрать всё: шапку/подвал/суперхидер/нумератор - мой ключ
   //oTsb:uSelector   := NIL   // убрать в таблице вирт.колонку SELECTOR
   //oTsb:aNumber     := Nil   // убрать в таблице вирт.колонку ORDKEYNO
   //oTsb:lSuperHead  := .F.   // убрать в таблице суперхидер - мой ключ
   //oTsb:lHeading    := .F.   // убрать в таблице шапку - мой ключ - не работает
   //oTsb:lSpecHd     := .F.   // убрать НУМЕРАТОР в таблице
   //oTsb:lFooting    := .T.   // убрать в таблице подвал
   //oTsb:lAdjust     := .F.   // убрать добавление пробелов в колонки для растяжки ширины в экран
   //oTsb:aEdit       := .F.   // запрет правки всех ячеек

   IF hb_IsArray(oTsbW:aWidthCol)
      oTsb:aWidthCol := oTsbW:aWidthCol   // работа с колонками
   ENDIF

   IF hb_IsArray(oTsbW:aRelat)
      oTsb:aRelat  := oTsbW:aRelat  // базы подлючаемые по Set Relation
   ENDIF

   // блок инициализации
   oTsb:bInit     := {|ob,op|
         ob:GetColumn("ORDKEYNO"):hFont := GetFontHandle(op:aFont[4])    // "Italic"
         // это как пример центровки колонок
         AEval(ob:aColumns, {|oc| oc:nAlign  := iif( oc:cFieldTyp $ "DLT^=@", DT_CENTER, oc:nAlign ) }) // это как пример
         AEval(ob:aColumns, {|oc| oc:nFAlign := oc:nAlign }) // это как пример, перенесли Align на Footer
         //AEval(ob:aColumns, {|oc| oc:nWidth  += iif( oc:cFieldTyp $ "T=@", 15, 0 ) }) // увеличили width у TimeStamp
         // поставить правильную ширину для колонок
         //AEval(ob:aColumns, {|oc| iif( oc:cFieldTyp $ "T=@", ( oc:cPicture := "yyyy-mm-dd hh:mm:ss", oc:nWidth := oc:ToWidth(20) ), nil ) })
         //AEval(ob:aColumns, {|oc| iif( oc:cFieldTyp $ "T=@", ( oc:cPicture := "99-99-99 99:99:99", oc:nWidth := oc:ToWidth(22) ), nil ) })
         AEval(ob:aColumns, {|oc| iif( oc:cFieldTyp $ "T=@", ( oc:cPicture := nil, oc:nWidth := oc:ToWidth(24) ), nil ) })

         op:lSpecHd := IIF( op:lSpecHd == Nil, .T., op:lSpecHd ) // если такого ключа нет
         IF op:lSpecHd
            ob:nHeightSpecHd := 16                           // высота спецхидера ENUMERATOR
            ob:lDrawSpecHd   := .T.
         ELSE
            ob:nHeightSpecHd := 0                            // высота спецхидера ENUMERATOR
            ob:lDrawSpecHd   := .F.
         ENDIF
         ob:nHeightCell   += 4                               // добавим пикселей к высоте ячеек
         ob:nHeightHead   := GetFontHeight(op:aFont[2])      // высота шапки
         ob:nHeightFoot   := GetFontHeight(op:aFont[4]) + 6  // высота подвала

         IF hb_IsNumeric(op:nHeightHead)        // не будет работать, без изменения oTsb:bEnd
            ob:nHeightHead := op:nHeightHead    // высота шапки из параметров
         ENDIF

         op:lDrawDelAll := IIF( op:lDrawDelAll == Nil, .F., op:lDrawDelAll ) // если такого ключа нет
         IF op:lDrawDelAll          // мой ключ
            ob:lDrawHeaders  := .F.
            ob:lDrawSpecHd   := .F.
            ob:lFooting      := .F.
            op:lSuperHead    := .F. // мой ключ
            ob:nHeightSuper  := 0   // высота суперхидера
            ob:nHeightSpecHd := 0   // высота нумератора
            ob:nHeightHead   := 0   // высота шапки
            ob:nHeightFoot   := 0   // высота подвала
         ENDIF

         op:lFooting := IIF( op:lFooting == Nil, .T., op:lFooting ) // если такого ключа нет
         IF !op:lFooting
            ob:lFooting := .F.
         ENDIF

         op:lSuperHead := IIF( op:lSuperHead == Nil, .T., op:lSuperHead ) // если такого ключа нет
         IF !op:lSuperHead
            ob:nHeightSuper  := 0   // высота суперхидера
         ELSE
            ob:nHeightSuper  := 10  // высота суперхидера
         ENDIF

         op:lHeading := IIF( op:lHeading == Nil, .T., op:lHeading ) // если такого ключа нет
         // - не работает !!!
         //IF !op:lHeading
         //   ob:nHeightHead  := 0   // высота шапки
         //   ob:lDrawHeaders := .F.
         //ENDIF

         Return Nil
         }

   // Создаём СУПЕРХИДЕР в таблице
   oTsb:cSupHd1    := c1Title             // заголовок 1 суперхидера
   cTitle          := c2Title + "  Alias: " + ALIAS() + " , "
   cTitle          += cFileNoPath(DBINFO(DBI_FULLPATH)) + " , " + RddName()
   oTsb:cSupHd2    := cTitle             // заголовок 2 суперхидера
   oTsb:cSupHdImg1 := "rezerv"           // картинка для суперхидера
   oTsb:cSupHdImg2 := 'MINIGUI_TSB_DSC'  // картинка для суперхидера "ArrDown24"
   oTsb:nMaskBmp   := 0x008800C6
   // основной блок
   oTsb:bBody := {|ob,op|
         Local nFrom, nTo, nI, hBmp1, hBmp2, nOrdKeyNo

         IF ob:lDrawHeaders .AND. ob:nHeightSuper > 0

            ob:aBitMaps := { LoadImage(op:cSupHdImg1),  LoadImage(op:cSupHdImg2) }
            hBmp1       := ob:aBitMaps[1]
            hBmp2       := ob:aBitMaps[2]
            nOrdKeyNo   := ob:nColumn( "ORDKEYNO", .T. )
            nFrom := nTo := nI := 1

            IF nOrdKeyNo > 0
               nTo := nOrdKeyNo
            ENDIF

            IF nOrdKeyNo > 0
               ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:n1Clr17Fore, op:n1Clr16Back ;
                   TITLE " " + op:cSupHd1 /*BITMAP hBmp1*/ HORZ DT_CENTER
               ob:aSuperHead[ nI ][15] := op:nMaskBmp
               ob:aSuperHead[ 1, 7 ]   := oTsb:aFont[3]    // сменить фонт колонки 1
               //ob:aSuperHead[ 1, 4 ]   := CLR_YELLOW       // сменить цвет текста колонки 1
               //ob:aSuperHead[ 1, 5 ]   := RGB(84,141,212)  // сменить цвет фона колонки 1
               nFrom := nTo + 1
               nI    += 1
            ENDIF

            nTo := ob:nColCount()
            ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:nClr17Fore, op:nClr16Back ;
                   TITLE " " + op:cSupHd2 BITMAP hBmp2 HORZ DT_CENTER

            ob:aSuperHead[ nI ][15] := op:nMaskBmp

            ob:nHeightSuper := 28   // высота суперхидера

         ENDIF  // ob:lDrawHeaders

         Return Nil
         }

   // заменить завершающий блок _TBrowse(...) на свой
   // oTsb:bEnd := {|| NIL }

   // Взято из _TBrowse() и исправлено на свой
   oTsb:bEnd := {|ob,op|
                      // нет горизонтального HScroll и есть SELECTOR
                      IF op:uSelector != NIL .and. op:lAdjust == NIL .and. ob:lNoHScroll
                         IF HB_ISBLOCK( op:bAdjColumns )
                            EVal( op:bAdjColumns, ob, op )  // :AdjColumns(...)
                         ENDIF
                      ENDIF
                      IF ob:nLen > ob:nRowCount()           // нужен VScroll
                         ob:ResetVScroll( .T. )
                      ENDIF

                      IF hb_IsNumeric(op:nHeightHead)    // если задали высоту шапки таблицы
                         // не делаем растяжку по дырке внизу таблицы
                      ELSE
                         ob:SetNoHoles()
                      ENDIF

                      ob:SetFocus()
                      Return Nil
                    }

RETURN oTsb

//////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_Customization( oBrw, oTsb )   // донастройка таблицы
   LOCAL oCol, nI, cCol, cTyp, nClrNoDbf, nClrNoEdit, nForeNoDbf
   LOCAL cErr, hFont, cMsg, nJ, aRelat, aVal, aWidthCol := {}
   LOCAL cFld, cName, cAls, nCol, cHead, nSize, nAlgn, cFunc, aFont
   LOCAL nCharSize, nWidth, nZnak, nWCol, lEdit

   // базы подлючаемые по Set Relation
   hFont := oBrw:hFont  // 1-cells font
   IF hb_IsArray(oTsb:aRelat)
      aRelat := oTsb:aRelat
      FOR nI := 1 TO LEN(aRelat)
         aVal  := aRelat[nI]
         cFld  := aVal[1]
         cName := UPPER(aVal[2]) // новое имя поля
         cAls  := aVal[3]
         nSize := aVal[4]
         nAlgn := aVal[5]
         cFunc := aVal[6]
         nCol  := myGetNumbaColumn(oBrw,cFld)
         cHead := oBrw:GetColumn(cFld):cHeading
         IF nCol == 0
            cMsg := 'ERROR on Set Relation !;'
            cMsg += 'There is no such field "'+cFld+'" in the database !;'
            cMsg += 'Array search: ' + HB_ValToExp(aVal) + ';;' + ProcNL()
            AlertStop(cMsg)
         ELSE
            // доступ к редактированию колонки
            IF oTsb:aEdit == Nil            ; lEdit := .F.
            ELSEIF hb_IsArray(oTsb:aEdit)   ; lEdit := oTsb:aEdit[nCol]
            ELSEIF hb_IsLogical(oTsb:aEdit) ; lEdit := oTsb:aEdit
            ELSE                            ; lEdit := .F.
            ENDIF

            ADD COLUMN TO oBrw DATA FieldWBlock(cName, select(cAls))  ;
               HEADER cHead /*FOOTER "+"*/ FIXED NAME &(cName)

            oBrw:DelColumn(cFld)
            oBrw:MoveColumn( oBrw:nColumn(cName), nCol )
            aFont          := oTsb:aFont // передать фонты
            cErr           := 'Array search: ' + HB_ValToExp(aVal) + ';;' + ProcNL()
            oCol           := oBrw:GetColumn(cName)
            oCol:cHeading  := '"'+cHead+'"'
            oCol:nWidth    := GetTextWidth( Nil, REPL("a", nSize), hFont )
            oCol:nAlign    := nAlgn
            oCol:Cargo     := { cFunc, cFld, cErr, aFont, cAls }
            oCol:lEdit     := lEdit
            //oCol:bPrevEdit := {|| myTsbExtern(oBrw:aColumns[oBrw:nCell]:Cargo,oBrw) ,;
            //                      FALSE }
            oCol:bPrevEdit := {|uv,ob| 
                 uv := (ob:cAlias)->( RecNo() ) 
                 myTsbExtern(ob:aColumns[ ob:nCell ]:Cargo, ob) 
                 (ob:cAlias)->( dbGoto(uv) ) 
                 ob:Setfocus() 
                 ob:DrawSelect()  // перерисовать тек.строку курсора
                 ob:GoRight()     // передвинуть курсор вправо
                 DoEvents() 
                 Return .F. 
                 }  
         ENDIF
      NEXT
   ENDIF

   nClrNoEdit := oTsb:nClrNoEdit
   nClrNoDbf  := oTsb:nClrNoDbf       // фон селектор/нумератор/вирт.колонки
   nForeNoDbf := oTsb:nForeNoDbf      // текст селектор/нумератор/вирт.колонки

   // изменение цвета виртуальной колонки
   oCol       := oBrw:GetColumn("ORDKEYNO")
   oCol:nClrBack := nClrNoDbf

   IF hb_IsArray(oTsb:aWidthCol)      // работа с колонками
      aWidthCol := oTsb:aWidthCol
   ENDIF

   // изменение цвета шапки/подвала/спецхидера/нумератора колонок и другие действия
   FOR EACH oCol IN oBrw:aColumns
      IF oBrw:lDrawSpecHd
         oCol:nClrSpcHdBack := nClrNoDbf          // ::aColorsBack[ 18 ]
         oCol:nClrSpcHdFore := nForeNoDbf         // ::aColorsBack[ 19 ]
      ENDIF
      cCol := oCol:cName
      // подвал таблицы - изменить
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         //IF oCol:cName == "ORDKEYNO"
         //   oCol:cSpcHeading := "#"        // см. ниже
         //ENDIF
      ELSE
         oCol:nClrHeadBack := oTsb:nClr4Back     // 4 , фона шапки таблицы
         oCol:nClrHeadFore := oTsb:nClr3Fore     // 3 , текста шапки таблицы
         oCol:nClrFootBack := oTsb:nClr10Back    // 10, фона подвала таблицы
         oCol:nClrFootFore := oTsb:nClr9Fore     // 9 , текста подвала таблицы
         // подвал фонт+центровка
         oCol:hFontFoot    := GetFontHandle(oTsb:aFont[4])
         oCol:nFAlign      := DT_CENTER
         //oCol:cFooting   := cCol
      ENDIF
      cTyp := oCol:cFieldTyp
      IF cTyp $ "+=^"   // Type: [+] [=] [^]
         oCol:nClrHeadBack := nClrNoEdit
         oCol:nClrFootBack := nClrNoEdit
      ENDIF
      // работа с колонками
      nWCol := oCol:nWidth
      IF LEN(aWidthCol) > 0
         FOR nJ := 1 TO LEN(aWidthCol)
            IF cCol == aWidthCol[nJ,1]
               nCharSize := aWidthCol[nJ,2]
               nZnak := 1
               IF nCharSize < 0
                  nCharSize := nCharSize * -1
                  nZnak := -1
               ENDIF
               nWidth := GetTextWidth( Nil, REPL("a", nCharSize), hFont )
               IF nZnak < 0
                  oCol:nWidth := nWCol - nWidth
               ELSE
                  oCol:nWidth := nWCol + nWidth
               ENDIF
               EXIT
            ENDIF
         NEXT
      ENDIF
   NEXT

   // применение блока перенумерации SpecHd во второй виртуальной колонке ORDKEYNO
   EVal( oTsb:bSpecHdEnum, oBrw, oTsb, "#" )

   // для удалённых записей  и т.д
   oBrw:SetColor( {1}, { { |nr,nc,ob| myCellColorFore(nr,nc,ob) } } ) // 1 , текста в ячейках таблицы
   oBrw:SetColor( {2}, { { |nr,nc,ob| myCellColorBack(nr,nc,ob) } } ) // 2 , фона в ячейках таблицы

   // цвет между ячейками
   oBrw:nClrLine := CLR_BLACK //COLOR_GRID

   // Запрет правки колонок типа "+=^"
   AEval(oBrw:aColumns, {|oc| oc:lEdit := iif( oc:cFieldTyp $ "+=^", .F., oc:lEdit )})

   // По центру колонок полей типа "^"
   FOR EACH oCol IN oBrw:aColumns
      IF oCol:cFieldTyp == '^' .OR. oCol:cFieldTyp == '+'
         oCol:bDecode := {|nVal| hb_ntos( nVal ) }
      ENDIF
   NEXT

   // Левый верхний + нижний угол - specialаselector header background color
   oBrw:nClrSelectorHdBack := nClrNoDbf

   //  назначаем на суперхидер отдельную функцию ЭТО ДЕЛАЕМ ПОСЛЕ END TBROWSE
   FOR EACH oCol IN oBrw:aColumns
      // левая /*и правая*/ кнопка мышки для шапки таблицы
      oCol:bHLClicked := {|Ypix,Xpix,nAt,ob| iif( Ypix > ob:nHeightSuper, ;
                           Tbrowse_Header("Header:",Ypix,Xpix,nAt,ob) ,;
                           Tbrowse_SuperHd("Super:",Ypix,Xpix,nAt,ob) ) }
   NEXT

   // правим Super Header
   IF oBrw:lSelector .and. oBrw:nColumn( "ORDKEYNO", .T. ) > 0
      FOR nI := 1 TO Len( oBrw:aSuperHead )  // с первой или со 2-ой колонки менять
          oBrw:aSuperHead[ nI ][2] += 1
      NEXT
   ENDIF

   // снятие фильтра, если нет записей по фильтру - разблокировка шапки колонок
   oBrw:bEvents := {|ob,nmsg|
                     Local nI, oTsb, aFltr, aNFltr, nDel, cFltr
                     If nmsg == WM_LBUTTONUP .and. ob:nLen == 0
                        oTsb := ob:Cargo:oParam
                        nI  := myFilterClear(ob)
                        IF nI == 0
                        ELSEIF nI == 1
                           aFltr  := oTsb:aColFilter
                           aNFltr := oTsb:aColNumFltr
                           nDel   := LEN(aFltr)
                           // удалить условие фильтра для колонки
                           ADel( aFltr , nDel, .T. )
                           ADel( aNFltr, nDel, .T. )
                           cFltr := ""
                           For nI := 1 TO LEN(aFltr)
                              cFltr += aFltr[nI] + IIF(nI==LEN(aFltr),""," .AND. ")
                           Next
                           ob:FilterData(cFltr)
                           oTsb:aColFilter  := aFltr     // колонки таблицы с фильтром
                           oTsb:aColNumFltr := aNFltr    // номера колонок таблицы с фильтром
                        ELSEIF nI == 2
                           oTsb:aColFilter  := {}    // колонки таблицы с фильтром
                           oTsb:aColNumFltr := {}    // номера колонок таблицы с фильтром
                           ob:FilterData()
                        ENDIF
                        ob:SetFocus()
                     EndIf
                     Return Nil
                    }

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myGetNumbaColumn(oBrw,cFld)
   LOCAL nI, oCol, cCol, nCol := 0

   FOR nI := 1 TO Len(oBrw:aColumns)
      oCol := oBrw:aColumns[ nI ]
      cCol := oCol:cName
      IF UPPER(cCol) == UPPER(cFld)
         nCol := nI
         EXIT
      ENDIF
   NEXT

RETURN nCol

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbExtern(aCargo, oBrw)
   LOCAL cMsg, cBlock, nRet, cRun, cFld, cErr, nCell, aFont
   LOCAL oCell, nY, nX, nW, nH, aVal, cForm, cSprAls

   cRun    := aCargo[1]
   cFld    := aCargo[2]
   cErr    := aCargo[3]
   aFont   := aCargo[4]
   cSprAls := aCargo[5]
   nCell   := oBrw:nCell  // Column №
   cForm   := oBrw:cParentWnd
   oCell   := oBrw:GetCellInfo(oBrw:nRowPos)
   nY      := GetProperty(cForm, "Row") + GetTitleHeight()
   nY      += oCell:nRow + oBrw:nHeightHead + 4
   nX      := GetProperty(cForm, "Col") + GetBorderWidth()
   nX      += oCell:nCol
   nW      := oCell:nWidth
   nH      := oCell:nHeight
   aVal    := { nY, nX, aFont, cSprAls, cForm }
   cBlock  := cRun + "(" + HB_ValToExp(aVal) + ")"

   IF !hb_IsFunction( cRun )
      cMsg := "Functions: " + cRun + "() not in the EXE file!;"
      cMsg += "call - " + cErr + ";" + ProcNL()
      HMG_Alert( cMsg, {"&Continue"}, "Error!", ICON_STOP )
      nRet := 0
   ELSE
      nRet := Eval( hb_macroBlock( cBlock ) )
      IF nRet # 0
         IF (oBrw:cAlias)->( RLock() )
            (oBrw:cAlias)->( FIELDPUT( FIELDNUM(cFld), nRet ) )
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
            //SKIP 0
            //DO EVENTS
            //oBrw:DrawSelect()     // перерисовать тек.строку курсора
            //oBrw:GoRight()
         ELSE
            AlertStop("Recording is locked !;" + ProcNL())
         ENDIF
      /*ELSE
         DO EVENTS
         //oBrw:DrawSelect()     // перерисовать тек.строку курсора
         oBrw:DrawLine()
         oBrw:Refresh( .T. )
         //oBrw:DrawSelect(, .T.) */
      ENDIF
   ENDIF
   //oBrw:Setfocus()
   //DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////
// 1 , текст в ячейках таблицы
STATIC FUNCTION myCellColorFore( nAt, nCol, oBrw )
   LOCAL nColor, oTsb, nText, lDel, cCol, nTextNoDb, nClrDel
   Default nAt := 0 , nCol := 0

   oTsb      := oBrw:Cargo:oParam               // получить данные из объекта
   nText     := oTsb:nClr1Fore                  // цвет ячеек таблицы
   nTextNoDb := oTsb:nForeNoDbf                 // цвет фона колонки 1-2/селектор/нумератора
   nClrDel   := oTsb:nClrForeDel                // текст удалённых записей
   lDel      := (oBrw:cAlias)->( DELETED() )    // удалена ли запись ?

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nTextNoDb
   ELSE

      nColor := nText
      // это правило действует всегда
      IF lDel // удалена ли запись ?
         nColor := nClrDel
      ENDIF

   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////////////////
// 2 , фона в ячейках таблицы
STATIC FUNCTION myCellColorBack( nAt, nCol, oBrw )
   LOCAL nColor, lDel, oTsb, nTBC, nTBC2, nClrDel, nBC1Col, cCol
   LOCAL nBCFltr, aColFltr, nJ, lZebra
   Default nAt := 0, nCol := 0

   oTsb     := oBrw:Cargo:oParam               // получить данные из объекта
   nTBC     := oTsb:nClr2Back                  // цвет фона таблицы
   nTBC2    := oTsb:nClr22Bck                  // цвет чётная\нечётная row
   nClrDel  := oTsb:nClrBackDel                // фона удалённых записей
   nBC1Col  := oTsb:nClrNoDbf                  // цвет фона колонки 1-2/селектор/нумератор
   nBCFltr  := CLR_YELLOW                      // цвет фона колонки таблицы с фильтром
   aColFltr := oTsb:aColNumFltr                // номера колонок таблицы с фильтром
   lDel     := (oBrw:cAlias)->( DELETED() )    // удалена ли запись ?
   lZebra   := oTsb:lShowZebra                 // показ чётная\нечётная строка
   lZebra   := IIF( lZebra == NIL, .F., .T. )

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nBC1Col
   ELSE

      IF lZebra                // чётная\нечётная row
         IF oBrw:nAt % 2 == 0
            nColor := nTBC2
         ELSE
            nColor := nTBC
         ENDIF
      ELSE
         nColor := nTBC
      ENDIF

      // если есть фильтр на колонке
      IF LEN(aColFltr) > 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nCol
               nColor := nBCFltr
            ENDIF
         NEXT
      ENDIF

      // это правило действует всегда
      IF lDel                 // удалена ли запись ?
         nColor := nClrDel
      ENDIF

   ENDIF

RETURN nColor

/////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_Header(cMenu,nRowPix,nColPix,nAt,oBrw)
   LOCAL cForm, nCol, oCol, nY, nX, hFont1, hFont2, hFont3, nLine, cNCol, cCol
   LOCAL cMsg, cName, cVirt, cCnr, nCnr, nRzrv, cExit, lVirt, aMenu, nMenu
   LOCAL nJ, lFilter, lChange, cFilter, nDel, cMenu2, nRowPos, nCell, cSpcHd
   LOCAL aFltr, aColFltr, a2Fltr, c1Fltr, c2Fltr, oTsb

   oTsb     := oBrw:Cargo:oParam
   aFltr    := oTsb:aColFilter                        // колонки таблицы с фильтром
   aColFltr := oTsb:aColNumFltr                       // номера колонок таблицы с фильтром
   nRzrv    := nRowPix                                // не использую
   cForm    := oBrw:cParentWnd
   nLine    := nAt                                    // номер строки курсора в таблице
   nCol     := Max(oBrw:nAtColActual( nColPix ), 1 )  // номер активной колонки курсора в таблице
   oCol     := oBrw:aColumns[ nCol ]
   cName    := oCol:cName
   cSpcHd   := oCol:cSpcHeading
   cVirt    := ",ORDKEYNO,SELECTOR,"
   cCnr     := oBrw:aColumns[ nCol ]:cSpcHeading
   nCnr     := Val( cCnr )
   cNCol    := ' "' + hb_ntos(nCnr) + '"'
   nY       := GetProperty(cForm, "Row") + GetTitleHeight()
   nY       += GetMenuBarHeight() + oBrw:nTop
   nX       := GetProperty(cForm, "Col") + GetBorderWidth()
   // возмём координаты от шапки таблицы
   nY       += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nX       += oBrw:nLeft + oCol:oCell:nCol - 4
   hFont1   := GetFontHandle(oTsb:aFont[1])
   hFont2   := GetFontHandle(oTsb:aFont[2])
   hFont3   := GetFontHandle(oTsb:aFont[4])
   cExit    := myLangeRes(1)  // Выход
   aMenu    := myLangeRes(4)  // массив меню - фильтры
   nMenu    := 0
   lFilter  := .F.
   lChange  := .F.
   cFilter  := ""

   IF oBrw:lDrawSpecHd  // есть нумератор
      cNCol := ' "' + hb_ntos(nCnr) + '"'
   ELSE
      cNCol := ' "' + hb_ntos(nCol) + '"'
   ENDIF

   cVirt    := ",ORDKEYNO,SELECTOR,"
   IF cName $ cVirt
      // сделаем отдельное сообщение
      cMsg  := cMenu + "Virtual column: " + hb_ntos(nCol) + " [" + cName + "]"
      lVirt := .T.
   ELSE
      // меню шапки обычных колонок
      cMsg := cMenu + "Column: " + cNCol + " [" + cName + "]"
      lVirt := .F.
   ENDIF

   IF LEN(aColFltr) > 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nCol  // номер активной колонки
            lFilter  := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF lVirt
         MENUITEM aMenu[3] ACTION {|| nMenu := 3 } FONT hFont2  // Удалить ВСЕ фильтры по столбцам
         MENUITEM aMenu[4] ACTION {|| Tbrowse_Zero(oBrw)  } FONT hFont2  // Удалить ВСЕ фильтры по столбцам
      ELSE
         cMenu2 := aMenu[1] + cNCol
         MENUITEM cMenu2 ACTION {|| nMenu := 1, a2Fltr := Tbrowse_MenuFltr(oBrw,cMenu2,cName) } FONT hFont2
         IF lFilter
            MENUITEM aMenu[2] + cNCol ACTION {|| nMenu := 2 } FONT hFont2
         ELSE
            MENUITEM aMenu[2] + cNCol DISABLED FONT hFont2
         ENDIF
         SEPARATOR
            MENUITEM aMenu[3] ACTION {|| nMenu := 3 } FONT hFont2
      ENDIF
      //SEPARATOR
      //MENUITEM cExit ACTION {|| nMenu := 0 } FONT hFont3 NAME 0079
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu == 0                  // ничего не делаем
      lChange := .F.
   ELSEIF nMenu == 1              // ставим фильтр по столбцу
      IF LEN(a2Fltr) > 0  // поставили фильтр по таблице
         lChange := .T.
         nDel    := 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nCol  // номер активной колонки
               nDel := nJ
               EXIT
            ENDIF
         NEXT
         c1Fltr := a2Fltr[1]  // строка фильтра
         c2Fltr := a2Fltr[2]  // строка фильтра, резерв
         IF nDel == 0
            // новое условие фильтра для колонки
            AADD( aFltr   , c1Fltr )
            AADD( aColFltr, nCol )   // номер активной колонки
            cSpcHd += "  [" + hb_ntos(LEN(aFltr)) + "]"
         ELSE
            //  фильтр уже есть
            aFltr[nDel] := c1Fltr
         ENDIF
         oCol:cSpcHeading := cSpcHd
         // картинка в нумераторе колонок таблицы - фильтр  20x20
         //oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hFltrAdd20   }
         // добавить фильтр по другим колонкам, если есть
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 2              // удаляем фильтр по столбцу
      lChange := .T.
      nDel    := 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nCol    // номер активной колонки
            nDel := nJ
            EXIT
         ENDIF
      NEXT
      IF nDel > 0
         // удалить условие фильтра для колонки
         ADel( aFltr   , nDel, .T. )
         ADel( aColFltr, nDel, .T. )
      ENDIF
      IF AT( "[", cSpcHd ) > 0
         cSpcHd := SUBSTR( cSpcHd, 1, AT("[",cSpcHd) - 2 )
      ENDIF
      oCol:cSpcHeading := cSpcHd
      // картинка в нумераторе колонок таблицы - стрелка_вниз 20x20
      //oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
      // сделать очистку фильтра по таблице
      IF LEN(aFltr) == 0
         cFilter := ""
      ELSE
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 3              // удалить ВСЕ фильтры по столбцам
      lChange  := .T.
      aFltr    := {}
      aColFltr := {}
      nCnr     := 0
      FOR EACH oCol IN oBrw:aColumns
         cCol := oCol:cName
         IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
            // пропуск нумерации
         ELSE
            IF oCol:lVisible
               oCol:cSpcHeading := hb_ntos( ++nCnr )
               // картинка в нумераторе колонок таблицы - стрелка_вниз 20x20
               //oCol:uBmpSpcHd   := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
            ENDIF
         ENDIF
      NEXT
      // сделать очистку фильтра по таблице
      cFilter := ""

   ENDIF

   nRowPos := oBrw:nRowPos
   nCell   := oBrw:nCell
   // перечитать шапку и нумератор
   //oBrw:DrawHeaders()
   IF lChange
      // перезаписать значения в контейнер-объект
      oTsb:aColFilter  := aFltr       // колонки таблицы с фильтром
      oTsb:aColNumFltr := aColFltr    // номера колонок таблицы с фильтром

      // oBrw:Reset() - это не надо, уже есть в oBrw:FilterData()
      IF LEN(cFilter) == 0
         oBrw:FilterData()
      ELSE
         oBrw:FilterData( cFilter )         // установка фильтра на базу
      ENDIF
      //mySuperHeaderChange( oBrw, cFilter )  // изменить суперхидер таблицы

      // для управления перестановок колонок (за пределами окна тсб)
      DO EVENTS
      nCell := nCell - 1
      oBrw:GoPos( nRowPos, nCell )          // восстановить курсор в таблице на строке/столбце
      oBrw:GoRight()
   ENDIF

   IF oBrw:nLen == 0
      Tbrowse_Zero(oBrw)
   ENDIF

   DO EVENTS
   oBrw:SetFocus()

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_SuperHd(cMenu,nRowPix,nColPix,nAt,oBrw)
   LOCAL cForm, nCell, nCol, oCol, nY, nX, hFont1, hFont2, hFont3, lReset
   LOCAL cTitle, nRow, nLine, oTsb, aItm, nItm, cItm, cExit, cName, cVirt, lVirt

   oTsb   := oBrw:Cargo:oParam
   aItm   := oTsb:aMenuTest
   cExit  := myLangeRes(1)  // Выход
   aItm   := myLangeRes(2)  // массив меню
   cForm  := oBrw:cParentWnd
   nRow   := oBrw:GetTxtRow(nRowPix)                 // НЕ ТО ! номер строки курсора в таблице
   nCol   := Max(oBrw:nAtColActual( nColPix ), 1 )   // номер активной колонки курсора в таблице
   nCell  := oBrw:nCell                              // номер ячейки в таблице
   oCol   := oBrw:aColumns[ nCol ]
   cName  := oCol:cName
   cVirt  := ",ORDKEYNO,SELECTOR,"
   nLine  := nAt                                     // номер строки курсора в таблице
   cTitle := cMenu
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetMenuBarHeight() + oBrw:nTop
   //nY     += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nX     := GetProperty(cForm, "Col") + GetBorderWidth()
   nX     += oBrw:nLeft + oCol:oCell:nCol - 2
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])
   lReset := .F.    // перепоказ базы

   IF cName $ cVirt // сделаем отдельное меню
      lVirt := .T.
   ELSE
      lVirt := .F.
   ENDIF

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF lVirt
         MENUITEM "codepage RU1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU1251") } FONT hFont2
         MENUITEM "codepage RU866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU866")  } FONT hFont2
         MENUITEM "codepage UA1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA1251") } FONT hFont2
         MENUITEM "codepage UA866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA866")  } FONT hFont2
         //MENUITEM "codepage DEWIN"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"DEWIN")  } FONT hFont2
      ELSE
         FOR EACH cItm IN aItm
            nItm := hb_enumindex(cItm)
            IF cItm == "SEPARATOR"
               SEPARATOR
            ELSE
               MENUITEM cItm ACTION  {|| Tbrowse_MenuSupHd( VAL(This.Name),This.Name,cItm,oBrw) } FONT hFont2 NAME &(StrZero(nItm,4))
            ENDIF
         NEXT
      ENDIF
      //SEPARATOR
      //MENUITEM cExit ACTION {|| Nil } FONT hFont3 NAME 0089
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF lReset  // перепоказ базы
      oBrw:Reset()
      oBrw:GoTop()
   ENDIF

   oBrw:SetFocus()

RETURN NIL


///////////////////////////////////////////////////////////////////////////////////
FUNCTION BtnTsbMenu( nMenu, ow )   // внешний вызов
   LOCAL cForm, nY, nX, hFont1, hFont2, hFont3, lReset
   LOCAL oTsb, aItm, nItm, cItm, cExit, oWnd, oBrw, cObj

   oWnd   := ThisWindow.Object  // объект окна
   cForm  := oWnd:Name
   oBrw   := oWnd:Cargo:oBrw
   oTsb   := oBrw:Cargo:oParam
   cObj   := ow:Name
   cExit  := myLangeRes(1)  // Выход
   aItm   := myLangeRes(2)  // массив меню

   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetProperty(cForm, cObj, "Row") + GetProperty(cForm, cObj, "Height")
   nX     := GetProperty(cForm, "Col") + GetBorderWidth() * 2
   nX     := GetProperty(cForm, cObj, "Col")
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])
   lReset := .F.    // перепоказ базы

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF nMenu == 1
         MENUITEM "codepage RU1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU1251") } FONT hFont2
         MENUITEM "codepage RU866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU866")  } FONT hFont2
         MENUITEM "codepage UA1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA1251") } FONT hFont2
         MENUITEM "codepage UA866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA866")  } FONT hFont2
         //MENUITEM "codepage DEWIN"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"DEWIN")  } FONT hFont2
      ELSE
         FOR EACH cItm IN aItm
            nItm := hb_enumindex(cItm)
            IF cItm == "SEPARATOR"
               SEPARATOR
            ELSE
               MENUITEM cItm ACTION  {|| Tbrowse_MenuSupHd( VAL(This.Name),This.Name,cItm,oBrw) } FONT hFont2 NAME &(StrZero(nItm,4))
            ENDIF
         NEXT
      ENDIF
      SEPARATOR
      MENUITEM cExit ACTION {|| Nil } FONT hFont3 NAME 0089
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF lReset  // перепоказ базы
      oBrw:Reset()
      oBrw:GoTop()
   ENDIF

   oBrw:SetFocus()

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION myFilterClear(oBrw)
   LOCAL cForm, hFont1, hFont2, hFont3, oTsb, cExit, aMenu, nMenu

   oTsb   := oBrw:Cargo:oParam
   cExit  := myLangeRes(1)   // Выход
   aMenu  := myLangeRes(11)  // массив меню
   cForm  := oBrw:cParentWnd
   nMenu  := 0
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      MENUITEM aMenu[1] ACTION  {|| nMenu := 1 } FONT hFont1
      SEPARATOR
      MENUITEM aMenu[2] ACTION  {|| nMenu := 2 } FONT hFont2
      //SEPARATOR
      //MENUITEM cExit    ACTION  {|| nMenu := 0 } FONT hFont3
   END MENU

   _ShowContextMenu(cForm, , , .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

RETURN nMenu

///////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_MenuSupHd( nMenu, cMetka, cMenu, oBrw)
   LOCAL hWnd, cForm, c2 := cMetka

   IF ISOBJECT(oBrw)
      cForm := oBrw:cParentWnd
      hWnd  := GetFormHandle(cForm)
      DBSELECTAREA(oBrw:cAlias)
      IF nMenu == 1
         myDbGetAllUse(cMenu)            // Список открытых БД
      ELSEIF nMenu == 2
         myDbIndexesThis(cMenu)          // Индексы этой базы
      ELSEIF nMenu == 3
         myDbIndexChange(cMenu, oBrw)    // Переключить индекс базы
      ELSEIF nMenu == 4
         myDbRelation(cMenu)             // Set relation этой базы
      ELSEIF nMenu == 5
         myDbFilter(cMenu)               // DbFilter этой базы
      ELSEIF nMenu == 6
         myDbStructure(cMenu)            // Структура этой базы
      ELSEIF nMenu == 8
         myDbWriteCsv(cMenu)             // Выгрузить CSV
      ELSEIF nMenu == 9
         myGetLang(cMenu)                // Какой язык в окне ?
      ELSEIF nMenu == 10
         FontsListAll(cMenu)             // Фонты в программе
      ELSEIF nMenu == 11
         FontsTbrowse(cMenu, oBrw)       // Фонты в таблице
      ELSEIF nMenu == 13
         MsgAbout(cMenu)
      ENDIF
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbGetAllUse(cTitle)
   LOCAL nI, cMsg := "", aAlias := {}, aSelect := {}, aRdd := {}

   hb_waEval( {|| AADD(aAlias, Alias())   } )
   hb_waEval( {|| AADD(aSelect, Select()) } )
   hb_waEval( {|| AADD(aRdd, RddName())   } )

   FOR nI := 1 TO LEN(aAlias)
       cMsg += "Select: " + HB_NtoS(aSelect[nI])
       cMsg += ",  Alias: " + aAlias[nI] + " ,  RddName: " + aRdd[nI] + CRLF
   NEXT
   cMsg += REPL("; ",30)

   AlertInfo( cMsg, cTitle, , , {RED} )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexesThis(cTitle,cParRet)
   LOCAL nI, nSel, nOrder, cAlias, cIndx, aIndx, xRet, cMsg, cVal, aOrdN
   DEFAULT cParRet := "SAY"

   cAlias := ALIAS()
   nSel   := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, cTitle )
      RETURN NIL
   ENDIF

   nOrder := INDEXORD()
   cMsg   := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + ";"
   cMsg   += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cMsg   += "Open indexes: "
   aIndx  := {}
   aOrdN  := {}

   IF nOrder == 0
      cMsg += " (no) !;;"
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' );;'
      FOR nI := 1 TO 200
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += STR(nI,3) + ') - Index file: ' + DBORDERINFO(DBOI_FULLPATH) + ";"
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ");"
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '" ;'
            cMsg += '       FOR index: "' + OrdFor() + '" ' + SPACE(5)
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT  )) + ' );;'
            cVal := STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI)
            cVal := "(" + HB_NtoS(nI) + "), OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]"
            AADD( aIndx, cVal )
            AADD( aOrdN, OrdName(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) // переключить на основной индекс
      cMsg += REPL("-",60) + ";"
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS() + ";"
   ENDIF
   cMsg += "Number of records by index DBOI_KEYCOUNT(?) = " + HB_NtoS(ORDKEYCOUNT()) + ";"
   cMsg += REPL("; ",30)

   IF cParRet == "ADIM"
      xRet := { aIndx, aOrdN }
   ELSEIF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSEIF cParRet == "LOG"
      xRet := HB_NtoS(nOrder)+" , OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]  "
      xRet += "DBOI_KEYCOUNT() = " + HB_NtoS(ORDKEYCOUNT())
   ELSE
      xRet := cMsg
   ENDIF

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexChange(cTitle, oBrw)
   LOCAL aRet, nIndx, cOrd

   aRet := Tbrowse_MenuIndex(cTitle, oBrw)
   IF LEN(aRet) > 0
      nIndx := aRet[1]
      cOrd  := aRet[2]
      DbSelectArea(oBrw:cAlias)
      DbSetOrder(nIndx)
      oBrw:uLastTag := (oBrw:cAlias)->( OrdName(nIndx) )  // без этого индекс слетает
      oBrw:Reset()
      oBrw:Refresh(.T.)
      oBrw:GoTop()
      DO EVENTS
   ENDIF
   oBrw:Setfocus()

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbRelation(cTitle,cParRet)
   LOCAL aDim := {}, nR, aVal, cMsg, cVal
   DEFAULT cParRet := "SAY"

   cMsg := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + ";"
   cMsg += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cVal := ""

   FOR nR := 1 TO 130
      aVal := Relation( nR )
      IF LEN(aVal[1]) > 0
         AADD(aDim, aVal)
         cVal += hb_ValToExp(aVal) + ";"
      ENDIF
      IF LEN(aVal[1]) == 0
         EXIT
      ENDIF
   NEXT
   IF LEN(aDim) == 0
      AADD(aDim, {} )
      cMsg += "No Set relation !;" + CRLF
      cVal := "No Set relation !"
   ENDIF
   cMsg += cVal + REPL("; ",30)

   IF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSE
      cMsg := '"' + ATREPL( ";", cVal , CRLF ) + '"'
   ENDIF

RETURN cMsg

////////////////////////////////////////////////////////////////
STATIC FUNCTION Relation( nRelation )
RETURN { DBRELATION(nRelation), ALIAS(DBRSELECT(nRelation)) }

////////////////////////////////////////////////////////////////
FUNCTION myDbFilter(cTitle,cParRet)
   LOCAL cMsg, cAls := Alias()
   DEFAULT cParRet := "SAY"

   cMsg := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cMsg += 'DbFilter(): "' + (cAls)->( DbFilter() ) + '";;'
   cMsg += REPL("; ",30)

   IF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSE
      cMsg := '"' + (cAls)->( DbFilter() ) + '"'
   ENDIF

RETURN cMsg

////////////////////////////////////////////////////////////////
FUNCTION myDbStructure(cTitle)        // Структура этой базы
   LOCAL cMsg, cFile, cFTxt, cAls, aStru, nI, aVal, cTxt

   cAls  := Alias()
   aStru := DbStruct()
   cFile := DBINFO( DBI_FULLPATH )
   cFTxt := ChangeFileExt( cFile, '.txt' )
   cMsg  := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg  += "Path to the database - " + cFile + ";"
   cMsg  += "File with base structure - " + cFTxt + ";;"
   cMsg  += "     FILE: " + cFileNoPath( cFile ) + ";;"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += "   " + HB_NtoS(nI) + ". "
      cMsg += IIF( nI < 10, " ", "" )
      cMsg += PADR(aVal[1],13)
      cMsg += aVal[2] + PADL( HB_NtoS(aVal[3]), 5 )
      cMsg += " " + PADL( HB_NtoS(aVal[4]), 3 ) + ";"
   NEXT

   cMsg  += ";;{"
   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += IIF( nI == 1 , "", " " )
      cMsg += HB_ValToExp(aVal)
      cMsg += IIF( nI == LEN(aStru) , " ", "," ) + " | ;"
   NEXT
   cMsg += "}; "

   cMsg += CRLF
   cMsg += REPL("--",40) + ";"
   cMsg += SPACE(3) + "aDbf := {};"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cTxt := aVal[1] + '"'
      cMsg += SPACE(3) + 'AADD( aDbf, { "' + PADR(cTxt, 15)
      cMsg += ', "' + aVal[2] + '",' + STR(aVal[3],4)
      cMsg += ',' + STR(aVal[4],2) + ', "Not-show" , .T. } );'
   NEXT
   cMsg += CRLF

   AlertInfo( cMsg, cTitle, , , {RED} )

   cMsg := AtRepl( ";", cMsg, CRLF )
   cMsg := AtRepl( "|", cMsg, ";"  )
   cMsg += CRLF + MiniGuiVersion() + CRLF + ">>"
   HB_MemoWrit(cFTxt, cMsg)

   cMsg := "File created successfully !;"
   cMsg += cFTxt + ";;"
   cMsg += "Open this file ?;"
   IF AlertYesNo(cMsg, , , , , {LGREEN,RED} )
      ShellExecute( 0, "Open", cFTxt,,, 3 )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cTitle)       // Выгрузить CSV
   LOCAL cMsg, cFile, cFCsv, cAls

   cAls  := Alias()
   cFile := DBINFO( DBI_FULLPATH )
   cFCsv := ChangeFileExt( cFile, '.csv' )
   cMsg  := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg  += "Path to the database - " + cFile + ";"
   cMsg  += "Upload file created - " + cFCsv + ";;"
   cMsg  += "Total records in the database = " + HB_NtoS( LASTREC() ) + ";"

   WaitWindow( 'Converting DBF to CSV', .T. )
   GOTO TOP
   COPY TO (cFCsv) DELIMITED // запись в файл
   WaitWindow()

   AlertInfo( cMsg, cTitle, , , {RED} )

   cMsg := "File created successfully !;"
   cMsg += cFCsv + ";;"
   cMsg += "Open this file ?;"
   IF AlertYesNo(cMsg, , , , , {LGREEN,RED} )
      ShellExecute( 0, "Open", cFCsv,,, 3 )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION myGetLang(cTitle,cParRet)      // Какой язык в окне ?
   LOCAL cMsg
   DEFAULT cParRet := "SAY"

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   cMsg += "hb_langName()   = " + hb_langName() + ";"
   cMsg += "hb_langMessage()= " + hb_langMessage() + ";"
   // #define EG_ARG          1
   cMsg += "hb_langErrMsg(1)= " + hb_langErrMsg(1) + ";"

   IF cParRet == "SAY"
      cMsg += REPL("; ",20)
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSEIF cParRet == "DEBUG"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgInfo( cMsg, cTitle )
   ELSE
      cMsg := AtRepl( ";", cMsg, CRLF )
   ENDIF

RETURN cMsg

///////////////////////////////////////////////////////////////////////////////
FUNCTION FontsListAll(cTitle)
   LOCAL cFnt, hFnt, aFnt, cMsg := "", aFonts := {}, n

   FOR n := 1 TO Len( _HMG_aControlNames )
      IF _HMG_aControlType[ n ] == "FONT"
         AAdd( aFonts, { _HMG_aControlNames[ n ], _HMG_aControlHandles[ n ] } )
      ENDIF
   NEXT

   FOR EACH aFnt IN aFonts
       cFnt := aFnt[1]
       hFnt := aFnt[2]
       cMsg += strzero(hb_enumindex(aFnt), 2) + ". " + ;       /*+ cFnt + " : "*/
                 hb_valtoexp( GetFontParam( hFnt ) ) + CRLF
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo( cMsg, cTitle, , , {RED})

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION FontsTbrowse( cTitle, oBrw )
   LOCAL cMsg

   cMsg := "Table alias: " + oBrw:cAlias + ";;"
   cMsg += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";"
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout(cTitle)
   LOCAL cMsg

   cMsg := SHOW_TITLE + SHOW_VERS + ";;"
   cMsg += "(c) 2021 Verchenko Andrey <verchenkoag@gmail.com>;"
   cMsg += "(c) 2021 Sergej Kiselev <bilance@bilance.lv>;;"
   cMsg += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";"
   cMsg += "(c) Grigory Filatov http://www.hmgextended.com;;"
   cMsg += PadC( "This program is Freeware!", 60 ) + ";"
   cMsg += PadC( "Copying is allowed!", 60 ) + ";"

   AlertInfo( cMsg, cTitle, , , {RED} )

RETURN NIL

//////////////////////////////////////////////////////////////////////////
// Меню для фильтра / Filter menu
FUNCTION Tbrowse_MenuFltr(oBrw,cMenu2,cName)
   LOCAL cIco, cTitle, cFont, nFSize, nW, nH, aBColor, nHBtn, nWBtn
   LOCAL aBtnFnt, aFntClr, aRet, nGRow, nGCol, nHLine, nY, nX, nX2, nX3
   LOCAL cCapt, aBtnGrd, aGrOverEx, aGrFillEx, aGrOverOk, aGrFillOk
   LOCAL a3Dim, aValCmb, aValDbf, nWUsl, nWTxt, aUsl, aZn, nWR, aAndOr
   LOCAL nUsl1, nUsl2, cZnak1, cZnak2, nValDb1, nValDb2, nAndOr, nI
   LOCAL nWUsl2, cHelp, aGrOver, aGrFill, cValIs1, cValIs2, cValTyp
   LOCAL hFont, aFont, aSayTxt, hOld, oTsb

   oTsb    := oBrw:Cargo:oParam             // получить данные из объекта
   hFont   := GetFontHandle(oTsb:aFont[2])
   aFont   := GetFontParam(hFont)
   aSayTxt := myLangeRes(5)
   cTitle  := aSayTxt[1]   // Пользовательский фильтр
   cHelp   := aSayTxt[2]   // Необходимо заполнить хотя бы одну строку для фильтра
   aBColor := SILVER       // цвет фона таблицы
   nW      := 690
   nH      := 430
   cIco    := "iSearch48x1"
   cFont   := aFont[1]
   nFSize  := aFont[2] + 2
   aBtnFnt := { "Comic Sans MS", nFSize + 2 }
   aFntClr := { BLACK , YELLOW }
   nWBtn   := 170                        // ширина кнопки
   nHBtn   := 55                         // высота кнопки
   nGRow   := 20                         // отступ сверху/снизу
   nGCol   := 30                         // отступ слева/справа
   nHLine  := nFSize * 2                 // высота строки на форме
   aRet    := {}                         // вернуть фильтр для таблицы
   a3Dim   := ListOneColumn(oBrw,cName)  // значение колонки из базы
   aValCmb := a3Dim[1]
   aValDbf := a3Dim[2]
   cValTyp := a3Dim[3]                   // тип поля
   nValDb1 := nValDb2 := 0
   cValIs1 := cValIs2 := ""
   nAndOr  := 1

   aUsl    := {}
   aZn     := {}
   AADD(aUsl,"                     ")  ;  AADD(aZn,"    ")
   AADD(aUsl," равно (==)          ")  ;  AADD(aZn," == ")
   AADD(aUsl," не равен (#)        ")  ;  AADD(aZn," #  ")
   AADD(aUsl," больше (>)          ")  ;  AADD(aZn," >  ")
   AADD(aUsl," меньше (<)          ")  ;  AADD(aZn," <  ")
   AADD(aUsl," больше и равно (>=) ")  ;  AADD(aZn," >= ")
   AADD(aUsl," меньше и равно (<=) ")  ;  AADD(aZn," <= ")
   // перевод на другие языки
   aSayTxt := myLangeRes(6)
   aUsl[2] := aSayTxt[1]
   aUsl[3] := aSayTxt[2]
   aUsl[4] := aSayTxt[3]
   aUsl[5] := aSayTxt[4]
   aUsl[6] := aSayTxt[5]
   aUsl[7] := aSayTxt[6]

   IF cValTyp $ "CM"
      AADD(aUsl," содержит ($)     ")  ;  AADD(aZn," $ ")
      aUsl[8] := aSayTxt[7]
   ELSEIF cValTyp == "L"
      aUsl := {}
      aZn  := {}
      AADD(aUsl,"            ")  ;  AADD(aZn,"    ")
      AADD(aUsl," равно (==) ")  ;  AADD(aZn," == ")
      aUsl[2] := aSayTxt[1]
   ENDIF

   nWUsl := 0
   FOR nI := 1 TO LEN(aUsl)
     nWTxt := GetTxtWidth( aUsl[nI], nFSize, cFont, .F. )  // получить Width текста
     nWUsl := MAX( nWUsl, nWTxt )
   NEXT
   nWUsl   += 20
   nUsl1   := nUsl2  := 0
   cZnak1  := cZnak2 := ""
   aBtnGrd := { HMG_RGB2n( GRAY ), CLR_WHITE }  // градиент кнопки
   aGrOver := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
   aGrFill := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

   //  для окна типа Modal
   hOld := _HMG_InplaceParentHandle
   _HMG_InplaceParentHandle := ThisWindow.Handle

   DEFINE WINDOW Form_Fltr                 ;
      AT 0, 0 WIDTH nW HEIGHT nH           ;
      TITLE cTitle ICON cIco               ;
      MODAL NOSIZE                         ;
      BACKCOLOR aBColor                    ;
      FONT cFont SIZE nFSize               ;
      ON INIT    {|| ThisOnInit(cValTyp) } ;
      ON RELEASE {|| Nil  }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nGRow
      nX := nGCol

      @ nY, nX LABEL Label_1 WIDTH nW-nGCol*2 HEIGHT nHLine VALUE cMenu2 ;
        FONTCOLOR BLACK SIZE nFSize + 4 TRANSPARENT CENTERALIGN VCENTERALIGN
      nY += This.Label_1.Height + nGRow

      // ------------------ условие 1 --------------------
      @ nY, nX GETBOX GB_Usl1 VALUE "" WIDTH nWUsl HEIGHT nHLine ;
            FONTCOLOR BLACK BACKCOLOR WHITE READONLY

      @ nY, nX COMBOBOXEX Combo_Usl1 WIDTH nWUsl HEIGHT 320  ;
        ITEMS aUsl VALUE nUsl1 IMAGE {} BACKCOLOR SILVER     ;
        ON LISTCLOSE This.Combo_Usl1.Hide  INVISIBLE         ;
        ON CHANGE { || nUsl1 := This.Combo_Usl1.Value    ,;
                       cZnak1 := aZn[nUsl1]              ,;
                       This.GB_Usl1.Value := aUsl[nUsl1] ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWUsl BUTTONEX Btn_Usl1 WIDTH nHLine HEIGHT nHLine                  ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Usl1.Enabled := .F.  ,;
                   This.Combo_Usl1.Show          ,;
                   SetFocus(GetControlHandle("Combo_Usl1", "Form_Fltr")) ,;
                   This.Btn_Usl1.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ выбор значения 1 --------------------
      nX2    := nX + This.Combo_Usl1.Width + nHLine + nGCol
      nWUsl2 := nW - nX - This.Combo_Usl1.Width - nGCol - nGCol - nHLine

      @ nY, nX2 GETBOX GB_ValIs1 VALUE cValIs1 WIDTH nWUsl2-nHLine HEIGHT nHLine ;
        PICTURE REPL("X", 30) FONTCOLOR BLACK BACKCOLOR WHITE                    ;
        ON CHANGE {|| cValIs1 := This.GB_ValIs1.Value }

      @ nY, nX2 COMBOBOXEX Combo_Dbf1 WIDTH nWUsl2 HEIGHT 520 ;
        ITEMS aValCmb VALUE nValDb1 IMAGE {} BACKCOLOR SILVER ;
        ON LISTCLOSE This.Combo_Dbf1.Hide  INVISIBLE          ;
        ON CHANGE { || nValDb1 := This.Combo_Dbf1.Value ,;
                       cValIs1 := aValCmb[nValDb1]      ,;
                       This.GB_ValIs1.Value := cValIs1  ,;
                       This.Label_1.Setfocus }

      @ nY, nX2 + nWUsl2 - nHLine BUTTONEX Btn_Dbf1 WIDTH nHLine HEIGHT nHLine       ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Dbf1.Enabled := .F.  ,;
                   This.Combo_Dbf1.Show          ,;
                   SetFocus(GetControlHandle("Combo_Dbf1", "Form_Fltr")) ,;
                   This.Btn_Dbf1.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ---------------- выбор значения И / ИЛИ -----------------
      nY     += nHLine + nGRow
      nX3    := nX + nGCol * 2
      aAndOr := { 'И', 'ИЛИ' }
      aAndOr := myLangeRes(7)   // перевод на другие языки
      nWR    := GetTxtWidth( aAndOr[2], aBtnFnt[2], aBtnFnt[1], .T. ) + 50

      @ nY, nX3 RADIOGROUP Radio_1  OPTIONS aAndOr             ;
        VALUE nAndOr WIDTH nWR SPACING 5 HORIZONTAL            ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD BACKCOLOR aBColor ;
        ON CHANGE ( nAndOr := This.Radio_1.Value )

      nY  += nHLine + nGRow

      // ------------------ условие 2 --------------------
      @ nY, nX GETBOX GB_Usl2 VALUE cValIs2 WIDTH nWUsl HEIGHT nHLine ;
        FONTCOLOR BLACK BACKCOLOR WHITE READONLY

      @ nY, nX COMBOBOXEX Combo_Usl2 WIDTH nWUsl HEIGHT 320  ;
        ITEMS aUsl VALUE nUsl2 IMAGE {} BACKCOLOR SILVER     ;
        ON LISTCLOSE This.Combo_Usl2.Hide  INVISIBLE         ;
        ON CHANGE { || nUsl2 := This.Combo_Usl2.Value    ,;
                       cZnak2 := aZn[nUsl2]              ,;
                       This.GB_Usl2.Value := aUsl[nUsl2] ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWUsl BUTTONEX Btn_Usl2 WIDTH nHLine HEIGHT nHLine                  ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Usl2.Enabled := .F.  ,;
                   This.Combo_Usl2.Show          ,;
                   SetFocus(GetControlHandle("Combo_Usl2", "Form_Fltr")) ,;
                   This.Btn_Usl2.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ выбор значения 2 --------------------
      //nX2    := nX + This.Combo_Usl2.Width + nHLine + nGCol
      //nWUsl2 := nW - nX - This.Combo_Usl1.Width - nGCol - nGCol - nHLine

      @ nY, nX2 GETBOX GB_ValIs2 VALUE cValIs2 WIDTH nWUsl2-nHLine HEIGHT nHLine ;
        PICTURE REPL("X", 30) FONTCOLOR BLACK BACKCOLOR WHITE                    ;
        ON CHANGE {|| cValIs2 := This.GB_ValIs2.Value }

      @ nY, nX2 COMBOBOXEX Combo_Dbf2 WIDTH nWUsl2 HEIGHT 520 ;
        ITEMS aValCmb VALUE nValDb2 IMAGE {} BACKCOLOR SILVER ;
        ON LISTCLOSE This.Combo_Dbf2.Hide  INVISIBLE          ;
        ON CHANGE { || nValDb2 := This.Combo_Dbf2.Value ,;
                       cValIs2 := aValCmb[nValDb2]      ,;
                       This.GB_ValIs2.Value := cValIs2  ,;
                       This.Label_1.Setfocus }

      @ nY, nX2 + nWUsl2 - nHLine BUTTONEX Btn_Dbf2 WIDTH nHLine HEIGHT nHLine       ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Dbf2.Enabled := .F.  ,;
                   This.Combo_Dbf2.Show          ,;
                   SetFocus(GetControlHandle("Combo_Dbf2", "Form_Fltr")) ,;
                   This.Btn_Dbf2.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ подсказка --------------------
      nY    += nHLine + nGRow*2
      @ nY, nX LABEL Label_2 WIDTH nW-nGCol HEIGHT nHLine VALUE cHelp ;
        FONTCOLOR BLUE TRANSPARENT VCENTERALIGN

      nY        := nH - nGRow - nHBtn
      nX        := nW - nGCol - nWBtn
      cCapt     := myLangeRes(8)   // "Отмена"
      aBtnGrd   := { HMG_RGB2n( {189,30,73} ), CLR_WHITE }  // градиент кнопки
      aGrOverEx := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillEx := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Exit WIDTH nWBtn HEIGHT nHBtn                              ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverEx GRADIENTFILL aGrFillEx                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillEx ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverEx ) ;
        ACTION {|| This.Enabled := .F., Form_Fltr.Release() }

      nX        := nW - nGCol*2 - nWBtn*2
      cCapt     := "Ok"
      aBtnGrd   := { HMG_RGB2n( LGREEN ), CLR_WHITE }  // градиент кнопки
      aGrOverOk := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillOk := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn                                ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverOk GRADIENTFILL aGrFillOk                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillOk ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverOk ) ;
        ACTION {|| This.Enabled := .F. ,;
                   aRet := CollectFilter(cName,cValTyp,cValIs1,cValIs2,cZnak1,cZnak2,nAndOr) ,;  //  поставили фильтр
                   IIF( LEN(aRet)==0, This.Label_1.Setfocus , Form_Fltr.Release() ) ,;
                   This.Btn_Ok.Enabled := .T.  }

   END WINDOW

   CENTER WINDOW   Form_Fltr
   ACTIVATE WINDOW Form_Fltr ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                         This.Restore , DoEvents() }

   //  для окна типа Modal
   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

RETURN aRet

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisOnInit(cValType)

    IF cValType == "L"
       This.GB_Usl2.Hide
       This.Btn_Usl2.Hide
       This.Radio_1.Hide
       This.GB_ValIs2.Hide
       This.Btn_Dbf2.Hide
    ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
//  поставили/собрали фильтр с формы
STATIC FUNCTION CollectFilter(cName,cValType,cValIs1,cValIs2,cZnak1,cZnak2,nAndOr)
   LOCAL cAndOr, lErr, cFilter, cFunc, aRet, cErr, aLang

   cValIs1  := ALLTRIM(cValIs1)
   cValIs2  := ALLTRIM(cValIs2)
   cAndOr   := { ".AND.", ".OR." }[nAndOr]
   aRet     := {}
   lErr     := .F.
   cErr     := ""
   aLang    := myLangeRes(9)   // перевод на другие языки

   IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cZnak2)) == 0 .AND. ;
      LEN(ALLTRIM(cValIs1)) == 0 .AND. LEN(ALLTRIM(cValIs2)) == 0
      // просто выход
   ELSE
      IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
         lErr := .T.  // ошибка
         cErr := aLang[1] //"Нет знака условия в первой строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) == 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
         lErr := .T.  // ошибка
         cErr := aLang[2] //"Нет знака условия во второй строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0 .AND. ;
             LEN(ALLTRIM(cZnak1)) == 0
         lErr := .T.  // ошибка
         cErr := aLang[3] //"Не заполнена первая строка фильтра !"
      ELSE
         cFilter  := ""
         IF cValType $ "CM"
            cFunc := ""
            cName := "ALLTRIM(" + cName + ")"
         ELSEIF cValType $ "=@T"
            cFunc := "CtoT("
         ELSEIF cValType $ "+^N"
            cFunc := "VAL("
         ELSEIF cValType == "D"
            cFunc := "CtoD("
         ELSEIF cValType == "L" .AND. UPPER(cValIs1) == "T"
            cFunc := "!EMPTY("
         ELSEIF cValType == "L" .AND. UPPER(cValIs1) == "F"
            cFunc := "EMPTY("
         ENDIF
         IF ALLTRIM(cZnak1) == "$"
            IF LEN(ALLTRIM(cZnak1)) > 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
               cFilter += "'" + ALLTRIM(cValIs1) + "' $ " + cName
            ENDIF
         ELSE
            IF LEN(ALLTRIM(cZnak1)) > 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
               cFilter += cName + cZnak1 + cFunc + "[" + cValIs1 + "]"
               cFilter += IIF(LEN(cFunc)>0,")","")
            ELSE
               IF cValType $ "CM"
                  cFilter += "LEN( " + cName + " )" + cZnak1 + "0"
               ELSEIF cValType $ "=@T"
                  cFilter += cName + cZnak1 + "CtoT('')"
               ELSEIF cValType $ "+^N"
                  cFilter += cName + cZnak1 + "VAL('0')"
               ELSEIF cValType == "D"
                  cFilter += cName + cZnak1 + "CtoD('')"
               ENDIF
            ENDIF
         ENDIF
         // ------- второе условие ----------
         IF LEN(ALLTRIM(cZnak2)) > 0
            IF ALLTRIM(cZnak2) == "$"
               IF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
                  cFilter += cAndOr
                  cFilter += "'" + ALLTRIM(cValIs2) + "' $ " + cName
               ENDIF
            ELSE
               IF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
                  cFilter += cAndOr
                  cFilter += cName + cZnak2 + cFunc + "[" + cValIs2 + "]"
                  cFilter += IIF(LEN(cFunc)>0,")","")
               ELSE
                  cFilter += cAndOr
                  //cFilter += "LEN( " + cName + " )" + cZnak2 + "0"
                  IF cValType $ "CM"
                     cFilter += "LEN( " + cName + " )" + cZnak2 + "0"
                  ELSEIF cValType $ "=@T"
                     cFilter += cName + cZnak2 + "CtoT('')"
                  ELSEIF cValType $ "+^N"
                     cFilter += cName + cZnak2 + "VAL('0')"
                  ELSEIF cValType == "D"
                     cFilter += cName + cZnak2 + "CtoD('')"
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         aRet := { cFilter , "резерв" }
      ENDIF
   ENDIF

  IF lErr
     //AlertStop("Ошибка в строке фильтра !;;" + cErr, Form_Fltr.Title )
     AlertStop(aLang[4] + ";;" + cErr, Form_Fltr.Title )
  ENDIF

RETURN aRet

////////////////////////////////////////////////////////////////////////////
FUNCTION ListOneColumn(oBrw,cName)
   LOCAL a2Dim, cAls, cType, xVal, lFind, nOrd, nRec, nI, aDim1, aDim2

   a2Dim := {}
   aDim1 := {}
   aDim2 := {}
   cAls  := oBrw:cAlias

   SELECT(cAls)
   nRec := RecNo()
   nOrd := IndexOrd()
   OrdSetFocus(0)
   dbGotop()
   xVal := FIELDGET( FIELDNUM(cName) )
   AADD( a2Dim, { cValToCHAR(xVal), xVal } )
   cType := FIELDTYPE( FIELDNUM(cName) )
   DO WHILE !EOF()
      xVal  := FIELDGET( FIELDNUM(cName) )
      IF cType $ "CM"
      ELSEIF cType $ "=@T"
      ELSEIF cType $ "+^"
      ELSEIF cType == "D"
      ELSEIF cType == "N"
      ELSEIF cType == "L"
      ENDIF
      lFind := .F.
      FOR nI := 1 TO LEN(a2Dim)
         IF xVal == a2Dim[nI,2]
            lFind := .T.
            EXIT
         ENDIF
      NEXT
      IF !lFind
         AADD( a2Dim, { cValToCHAR(xVal), xVal } )
      ENDIF
      SKIP
      DO EVENTS
   ENDDO
   OrdSetFocus(nOrd)
   dbGoto(nRec)

   a2Dim := ASORT( a2Dim,,, { |x, y| x[2] < y[2] } )
   FOR nI := 1 TO LEN(a2Dim)
      AADD( aDim1 , ALLTRIM(a2Dim[nI,1]) )
      AADD( aDim2 , a2Dim[nI,2] )
   NEXT

RETURN { aDim1, aDim2, cType }

//////////////////////////////////////////////////////////////////////////
// Меню для смены индекса / Menu for changing the index
FUNCTION Tbrowse_MenuIndex(cMenu2, oBrw)
   LOCAL cIco, cTitle, cFont, nFSize, nW, nH, aBColor, nHBtn, nWBtn
   LOCAL aBtnFnt, aFntClr, aRet, nGRow, nGCol, nHLine, nY, nX, cVal
   LOCAL cCapt, aBtnGrd, aGrOverEx, aGrFillEx, aGrOverOk, aGrFillOk
   LOCAL nIndex, cHelp, aGrOver, aGrFill, nWCbox, aIndx, aClrStop
   LOCAL hFont, aFont, aSayTxt, hOld, oTsb, aOrdName
   DEFAULT cMenu2 := "Переключить индекс базы"

   oTsb     := oBrw:Cargo:oParam             // получить данные из объекта
   hFont    := GetFontHandle(oTsb:aFont[2])
   aFont    := GetFontParam(hFont)
   aSayTxt  := myLangeRes(10)
   cTitle   := aSayTxt[1]   // Пользовательское меню
   cHelp    := aSayTxt[2]   // Нет открытых индексов по этой базе
   aBColor  := SILVER       // цвет фона таблицы
   nW       := 760
   nH       := 350
   cIco     := "iSearch48x1"
   cFont    := aFont[1]
   nFSize   := aFont[2] + 2
   aBtnFnt  := { "Comic Sans MS", nFSize + 2 }
   aFntClr  := { BLACK , YELLOW }
   nWBtn    := 170                          // ширина кнопки
   nHBtn    := 55                           // высота кнопки
   nGRow    := 20                           // отступ сверху/снизу
   nGCol    := 30                           // отступ слева/справа
   nHLine   := nFSize * 2                   // высота строки на форме
   aRet     := {}                           // вернуть фильтр для таблицы
   aRet     := myDbIndexesThis("","ADIM")   // массив индексов
   aIndx    := aRet[1]
   aOrdName := aRet[2]
   cVal     := ""
   aBtnGrd  := { HMG_RGB2n( GRAY ), CLR_WHITE }  // градиент кнопки
   aGrOver  := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
   aGrFill  := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }
   aClrStop := HMG_n2RGB( CLR_HRED )
   nIndex   := 0

   //  для окна типа Modal
   hOld := _HMG_InplaceParentHandle
   _HMG_InplaceParentHandle := ThisWindow.Handle

   DEFINE WINDOW Form_Index       ;
      AT 0, 0 WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco      ;
      MODAL NOSIZE                ;
      BACKCOLOR aBColor           ;
      FONT cFont SIZE nFSize

      nW     := This.ClientWidth
      nH     := This.ClientHeight
      nY     := nGRow
      nX     := nGCol
      nWCbox := nW - nGCol * 2 - nHLine

      @ nY, nX LABEL Label_1 WIDTH nW-nGCol*2 HEIGHT nHLine VALUE cMenu2 ;
        FONTCOLOR BLACK SIZE nFSize + 4 TRANSPARENT CENTERALIGN VCENTERALIGN
      nY += This.Label_1.Height + nGRow

      @ nY, nX GETBOX GB_Indx VALUE cVal WIDTH nWCbox HEIGHT nHLine    ;
        PICTURE REPL("X", 90) FONTCOLOR BLACK BACKCOLOR WHITE READONLY ;
        ON CHANGE {|| cVal := This.GB_Indx.Value }

      @ nY, nX COMBOBOXEX Combo_Indx WIDTH nWCbox HEIGHT 360  ;
        ITEMS aIndx VALUE nIndex IMAGE {} BACKCOLOR SILVER    ;
        ON LISTCLOSE This.Combo_Indx.Hide  INVISIBLE          ;
        ON CHANGE { || nIndex := This.Combo_Indx.Value ,;
                       cVal   := aIndx[nIndex]         ,;
                       This.GB_Indx.Value := cVal      ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWCbox BUTTONEX Btn_Indx WIDTH nHLine HEIGHT nHLine                 ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Indx.Enabled := .F.  ,;
                   This.Combo_Indx.Show          ,;
                   SetFocus(GetControlHandle("Combo_Indx", "Form_Index")) ,;
                   This.Btn_Indx.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      //  подсказка
      nY  += nHLine + nGRow*2
      @ nY, nX LABEL Label_2 WIDTH 46 HEIGHT 46 VALUE CHR(74) FONT "Wingdings" ;
        SIZE 40 FONTCOLOR aClrStop TRANSPARENT VCENTERALIGN
      nX += This.Label_2.Width + 10

      @ nY, nX LABEL Label_3 WIDTH nW-nGCol*2 HEIGHT 40 VALUE cHelp ;
        FONTCOLOR aClrStop TRANSPARENT VCENTERALIGN

      IF LEN(aIndx) == 0
         This.GB_Indx.Enabled    := .F.
         This.Combo_Indx.Enabled := .F.
         This.Btn_Indx.Enabled   := .F.
      ELSE
         This.Label_2.Hide
         This.Label_3.Hide
      ENDIF

      nY        := nH - nGRow - nHBtn
      nX        := nW - nGCol - nWBtn
      cCapt     := myLangeRes(8)   // "Отмена"
      aBtnGrd   := { HMG_RGB2n( {189,30,73} ), CLR_WHITE }  // градиент кнопки
      aGrOverEx := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillEx := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Exit WIDTH nWBtn HEIGHT nHBtn                              ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverEx GRADIENTFILL aGrFillEx                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillEx ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverEx ) ;
        ACTION {|| This.Enabled := .F., aRet := {}, Form_Index.Release() }

      nX        := nW - nGCol*2 - nWBtn*2
      cCapt     := "Ok"
      aBtnGrd   := { HMG_RGB2n( LGREEN ), CLR_WHITE }  // градиент кнопки
      aGrOverOk := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillOk := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn                                ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverOk GRADIENTFILL aGrFillOk                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillOk ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverOk ) ;
        ACTION {|| This.Enabled := .F. ,;
                   IIF( nIndex == 0, aRet := {}, aRet := { nIndex, aOrdName[nIndex] } ) ,;
                   IIF( LEN(aRet) == 0, This.Label_1.Setfocus, Form_Index.Release() ) ,;
                   Form_Index.Btn_Ok.Enabled := .T.  }

   END WINDOW

   CENTER WINDOW   Form_Index
   ACTIVATE WINDOW Form_Index //ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                              //           This.Restore , DoEvents() }
   //  для окна типа Modal
   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

RETURN aRet

//////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_Zero(oBrw)
   LOCAL oTsb, cTxt, cFont, nRow, nCol, hOld, oWnd, hWnd, aBColor
   LOCAL nH, nW, hFont, aFont, nFSize, cForm, oc1, oc2, nHDown, nHMain

   cForm   := oBrw:cParentWnd
   oTsb    := oBrw:Cargo:oParam             // получить данные из объекта
   hFont   := GetFontHandle(oTsb:aFont[2])
   aFont   := GetFontParam(hFont)
   cFont   := aFont[1]
   cFont   := "Comic Sans MS"
   nFSize  := aFont[2] + 2
   aBColor := { 253,234,218 }              // цвет фона окна
   nHMain  := Getproperty( cForm, "Height" ) - GetBorderHeight() - GetTitleHeight()
   // расчёт по таблице
   oc1     := oBrw:GetCellSize(1, 1)
   oc2     := oBrw:GetCellSize(oBrw:nRowCount(), 1)
   nRow    := oc1:nRow
   nCol    := oc1:nCol + 1
   nW      := GetWindowWidth(oBrw:hWnd) - GetVScrollBarWidth() + 2 // и справа
   nH      := GetWindowHeight(oBrw:hWnd)
   nHDown  := nHMain - nH
   nH      -= ( oBrw:nHeightSuper + oBrw:nHeightHead +  oBrw:nHeightSpecHd )
   nH      -= ( oBrw:nHeightFoot + GetHScrollBarHeight() + nHDown )
   // инфо по таблице
   cTxt    := CRLF + "DbFilter of this base: " + myDbFilter(,"LOG") + CRLF + CRLF
   cTxt    += "Set relation of this base: " + myDbRelation(,"LOG") + CRLF + CRLF
   cTxt    += "Indexes of this base: " + myDbIndexesThis(,"LOG")

   // вызов только с пользовательских окон, нельзя вызывать с контекстного меню
   //hOld  := _HMG_InplaceParentHandle
   //_HMG_InplaceParentHandle := ThisWindow.Handle
   // Внимание окно CHILD не желательно использовать !!!

   DEFINE WINDOW Forma_Zero AT nRow, nCol CLIENTAREA nW, nH MODAL NOCAPTION ;
      BACKCOLOR aBColor ON LOSTFOCUS oWnd:Release()

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle
      nW   := This.ClientWidth
      nH   := This.ClientHeight

      @ 10, 10 LABEL Label_Zero WIDTH nW-20 HEIGHT nH-20 VALUE cTxt ;
        FONT cFont SIZE nFSize TRANSPARENT

      @ nH - 28, 20 LABEL Label_Info AUTOSIZE VALUE "ESC / ENTER - exit" ;
        FONT "Tahoma" SIZE 18 BOLD TRANSPARENT CENTERALIGN VCENTERALIGN

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey( "Forma_Zero" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey( "Forma_Zero" , 0 , VK_RETURN , {|| oWnd:Release() } )

   Forma_Zero.Activate

   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ReopenDbase(oBrw, cCodePage)
   LOCAL cDbf, cFltr, nI, cIndex, aIndx, cFile, nOrd, cAls
   LOCAL lNew, nJ

   cAls  := oBrw:cAlias
   nOrd  := ORDNAME()
   aIndx := {}
   DbSelectArea(cAls)
   cDbf  := DBINFO( DBI_FULLPATH )
   cFltr := (cAls)->( DbFilter() )
   FOR nI := 1 TO 500
      IF LEN(ORDNAME(nI)) == 0
         EXIT
      ELSE
         DBSetOrder(nI)
         cIndex := DBORDERINFO( DBOI_FULLPATH,,ORDNAME(nI) )
         IF LEN(aIndx) == 0
            AADD(aIndx, cIndex )
         ELSE
            FOR nJ := 1 TO LEN(aIndx)
               lNew := .T.
               IF cIndex == aIndx[nJ]
                  lNew := .F.
                  EXIT
               ENDIF
               IF lNew
                 AADD(aIndx, cIndex )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   NEXT
   (cAls)->( dbCloseArea() )

   USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW SHARED
   DO EVENTS
   IF LEN(aIndx) > 0
      FOR nI := 1 TO LEN(aIndx)
         cFile := aIndx[nI]
         ORDLISTADD( cFile )
      NEXT
      DBSetOrder(nOrd)
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION TsbButtonMenu(oMenu,nWGaps,nHGaps,oThis)   // меню кнопок для таблицы
                             // nWGaps,nHGaps - отступы начала ТСБ
   LOCAL aPos, lDebug, nPosWin, lShow, cErr, nHAlign, nVAlign
   LOCAL nHMenu, aObj, nD1, nD2, nD3, nD4

   aPos   := { 0, 0, 0, 0 }
   lDebug := .F.
   lShow  := .T.
   cErr   := ""
   nD1    := nD2 := nD3 := 0

   IF hb_IsLogical(oMenu:lDebug)
      lDebug := oMenu:lDebug   // отладка, показ ошибок
   ENDIF

   IF hb_IsNumeric(oMenu:nPosWin)
      nPosWin := oMenu:nPosWin
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nPosWin := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsNumeric(oMenu:nHAlign)
      nHAlign := oMenu:nHAlign
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nHAlign := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsNumeric(oMenu:nVAlign)
      nVAlign := oMenu:nVAlign
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nVAlign := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aBtnPost)
      nD1 := LEN(oMenu:aBtnPost)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aBtnPost := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aCaption)
      nD2 := LEN(oMenu:aCaption)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aCaption := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aBColor)
      nD3 := LEN(oMenu:aBColor)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aBColor := ??? ;"
      lShow := .F.
   ENDIF

   IF nD1 # nD2 .OR. nD1 # nD3
      cErr += "Error ! Arrays are not equal ! ;;"
      cErr += "        Len(oMenu:aBtnPost) = " + HB_NtoS(nD1) + ";"
      cErr += "        Len(oMenu:aCaption) = " + HB_NtoS(nD2) + ";"
      cErr += "        Len(oMenu:aBColor)  = " + HB_NtoS(nD3) + ";"
      lShow := .F.
   ENDIF

   IF hb_IsLogical(oMenu:lBtnIco)
      IF oMenu:lBtnIco
         IF hb_IsArray(oMenu:aIcon)
            nD4 := LEN(oMenu:aIcon)
            IF nD1 # nD4
               cErr  += "Error ! Len(oMenu:aIcon)  = " + HB_NtoS(nD4) + ";"
               lShow := .F.
            ENDIF
         ELSE
            cErr  += "Error ! IsArray() ! Not - oMenu:aIcon := ??? ;"
            lShow := .F.
         ENDIF
      ENDIF
   ENDIF

   // показ меню без ошибок
   IF lShow
      nHMenu  := IIF( hb_IsNumeric(oMenu:nHMenu), oMenu:nHMenu, 45)  // высота всего меню
      nPosWin := IIF( nPosWin == 0, 1, nPosWin )
      nPosWin := IIF( nPosWin > 4 , 1, nPosWin )
      nPosWin := IIF( nPosWin < 1 , 1, nPosWin )
      IF nPosWin == 1          // TopWindow
         aPos[1] := nHMenu - nHGaps
         aPos[4] := -nHMenu + nHGaps
      ELSEIF nPosWin == 2      // BottomWindow
         aPos[4] := -nHMenu + nHGaps
      ELSEIF nPosWin == 3      // LeftWindow
         aPos[2] := nHMenu - nWGaps
         aPos[3] := -nHMenu + nWGaps
      ELSEIF nPosWin == 4      // RightWindow
         aPos[3] := -nHMenu + nWGaps
      ENDIF

      aObj := Show_Button(oMenu,nWGaps,nHGaps,nPosWin)
      oThis:Cargo:aObjBtn := aObj

   ELSE
      IF lDebug
         cErr += ";;" + ProcNL()
         AlertStop(cErr, "Error !")
      ENDIF
   ENDIF

RETURN aPos

////////////////////////////////////////////////////////////////////////
FUNCTION Show_Button(oMenu,nY,nX,nPosWin)
   LOCAL cForm, nW, nH, aBtnCap, aBtnIco, aBtnClr, aBtnPst, cCapt
   LOCAL cN, cFont, nFSize, lFBold, nJ, aBtnGrd, aGrOver, aGrFill
   LOCAL aFntClr, nwPost, aBtnObj, aFont, aColor, lItalic, nF2Size
   LOCAL lBtnIco, nGBtn, nWBtn, nHBtn, nIndent, nIcoSize, nOnePos
   LOCAL nHAlign, nVAlign, nY2, nX2, nLenBtn, nWBthAll, nHBthAll
   LOCAL lTextVert, lTextLeft, aIco

   aBtnObj := {}
   cForm   := ThisWindow.Name
   nW      := This.ClientWidth
   nH      := This.ClientHeight
   nY2     := nY
   nX2     := nX
   // кнопки
   // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   nHAlign  := IIF( hb_IsNumeric(oMenu:nHAlign), oMenu:nHAlign, DT_LEFT )
   // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   nVAlign  := IIF( hb_IsNumeric(oMenu:nVAlign), oMenu:nVAlign, DT_TOP )
   // Отступ первой кнопки - резерв
   nIndent  := IIF( hb_IsNumeric(oMenu:nIndent), oMenu:nIndent, 0 )
   nWBtn    := oMenu:nWBtn         // ширина кнопки
   nHBtn    := oMenu:nHBtn         // высота кнопки
   nOnePos  := oMenu:nGaps         // отступ кнопки от края окна
   nGBtn    := oMenu:nGapsBtn      // между кнопками по ширине/высоте
   // подсчёт для DT_CENTER
   aBtnCap  := oMenu:aCaption
   nLenBtn  := LEN(aBtnCap)
   nWBthAll := ( nW - nWBtn * nLenBtn - nGBtn * ( nLenBtn - 1 ) ) / 2
   nHBthAll := ( nH - nHBtn * nLenBtn - nGBtn * ( nLenBtn - 1 ) ) / 2

   IF nPosWin = 1                  // TopWindow
      nY := nOnePos
      IF nHAlign == DT_LEFT        // горизонтальные кнопки
      ELSEIF nHAlign == DT_CENTER
         nX := nWBthAll
      ELSEIF nHAlign == DT_RIGHT
         nX := nW - nX2 - nWBtn
      ENDIF
   ELSEIF nPosWin = 2              // BottomWindow
      nY := nH - nHBtn - nOnePos
      IF nHAlign == DT_LEFT        // горизонтальные кнопки
      ELSEIF nHAlign == DT_CENTER
         nX := nWBthAll
      ELSEIF nHAlign == DT_RIGHT
         nX := nW - nX2 - nWBtn
      ENDIF
   ELSEIF nPosWin == 3             // LeftWindow
      nX := nOnePos
      IF nVAlign == DT_TOP         // вертикальные кнопки
      ELSEIF nVAlign == DT_CENTER
         nY := nHBthAll
      ELSEIF nVAlign == DT_BOTTOM
         nY := nH - nY2 - nHBtn
      ENDIF
   ELSEIF nPosWin == 4             // RightWindow
      nX := nW - nWBtn - nOnePos
      IF nVAlign == DT_TOP         // вертикальные кнопки
      ELSEIF nVAlign == DT_CENTER
         nY := nHBthAll
      ELSEIF nVAlign == DT_BOTTOM
         nY := nH - nY2 - nHBtn
      ENDIF
   ENDIF

   IF hb_IsLogical(oMenu:lBtnIco)
      lBtnIco := oMenu:lBtnIco
   ELSE
      lBtnIco := .F.       // F-кнопки без иконок
   ENDIF
   nIcoSize  := oMenu:nIcoSize
   aIco      := IIF( hb_IsArray(oMenu:aIcon), oMenu:aIcon, {} )
   aBtnCap   := oMenu:aCaption
   aBtnIco   := oMenu:aIcon
   aBtnClr   := oMenu:aBColor
   aBtnPst   := oMenu:aBtnPost
   // фонты  для кнопок
   aFntClr   := oMenu:aFClr  //{ BLACK , OLIVE }
   aFont     := oMenu:aFont
   cFont     := aFont[1]     //"Comic Sans MS"
   nFSize    := aFont[2]     //16
   lFBold    := aFont[3]     //.T.
   lItalic   := aFont[4]     //.T.
   nF2Size   := aFont[5]     //32
   // текст для кнопок
   lTextVert := IIF( hb_IsLogical(oMenu:lTextVert), oMenu:lTextVert, .F. )
   lTextLeft := IIF( hb_IsLogical(oMenu:lTextLeft), oMenu:lTextLeft, .F. )

   FOR nJ := 1 TO LEN(aBtnCap)
      cCapt   := StrTran( aBtnCap[nJ], ";" , CRLF )
      aColor  := aBtnClr[nJ]
      aBtnGrd := { HMG_RGB2n( aColor ), CLR_WHITE }  // градиент кнопки
      aGrOver := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFill := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }
      nwPost  := aBtnPst[nJ]
      cN      := 'Btn_' + HB_NtoS(nwPost) //StrZero(nJ, 2)
      AADD( aBtnObj, { cN, aBtnCap[nJ], aColor, nwPost } )

      IF lBtnIco .AND. LEN(aIco) > 0  // кнопки с иконками
         aIco := aBtnIco[nJ]
      ENDIF

      // alt_syntax
      DEFINE BUTTONEX &cN
         ROW           nY
         COL           nX
         WIDTH         nWBtn
         HEIGHT        nHBtn
         CAPTION       cCapt
         ICON          IIF( lBtnIco, aIco[1], Nil )
         FONTNAME      cFont
         FONTSIZE      nFSize
         FONTCOLOR     aFntClr[1]
         FONTBOLD      .F.
         BACKCOLOR     aGrOver
         GRADIENTFILL  aGrFill
         LEFTTEXT      lTextLeft
         VERTICAL      lTextVert
         FLAT          .T.
         NOHOTLIGHT    .F.
         NOXPSTYLE     .T.
         HANDCURSOR    .T.
         NOTABSTOP     .T.
         ONMOUSEHOVER ( myMouseHL(2) )
         ONMOUSELEAVE ( myMouseHL(1) )
         ACTION ( This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) )
         /*ONINIT {|o|   // можно и так делать
                    This.Cargo := oHmgData()  // создать объект (контейнер) для этой кнопки
                    o := This.Cargo
                    // положим на кнопку нужные данные
                    o:nBtn     := nJ
                    o:nPost    := nwPost
                    o:cCapt    := cCapt
                    o:aIco     := aIco
                    o:aBClr    := aColor
                    o:cObj     := cN
                    o:aGrFill  := aGrFill
                    o:aGrOver  := aGrOver
                    o:aFntClr1 := aFntClr[1]
                    o:aFntClr2 := aFntClr[2]
                    o:lBold2   := .T.
                    o:lBold1   := .F.
                    o:nFSize2  := nF2Size       // увеличенный фонт кнопки
                    o:nFSize   := nFSize        // фонт кнопки
                    o:nIcoSize := nIcoSize      // размер иконки
                    o:aIco     := aIco          // 2 иконки кнопки
                    o:lBtnIco  := lBtnIco       // есть/нет иконка на кнопке
                    Return Nil
                   }          // ON INIT надо задавать только блоком кода */
      END BUTTONEX

      This.&(cN).Cargo := oHmgData()
      WITH OBJECT This.&(cN).Cargo
         :nBtn     := nJ
         :nPost    := nwPost
         :cCapt    := cCapt
         :aIco     := aIco
         :aBClr    := aColor
         :cObj     := cN
         :aGrFill  := aGrFill
         :aGrOver  := aGrOver
         :aFntClr1 := aFntClr[1]
         :aFntClr2 := aFntClr[2]
         :lBold2   := .T.
         :lBold1   := .F.
         :nFSize2  := nF2Size       // увеличенный фонт кнопки
         :nFSize   := nFSize        // фонт кнопки
         :nIcoSize := nIcoSize      // размер иконки
         :aIco     := aIco          // 2 иконки кнопки
         :lBtnIco  := lBtnIco       // есть/нет иконка на кнопке
      END WITH

      // сдвиг кнопки
      IF nPosWin = 1 .OR. nPosWin = 2   // TopWindow-BottomWindow
         // горизонтальные кнопки
         IF nHAlign == DT_LEFT .OR. nHAlign == DT_CENTER
            nX += nWBtn + nGBtn
         ELSE
            nX -= ( nWBtn + nGBtn )
         ENDIF
      ELSE                             // LeftWindow-RightWindow
         // вертикальные кнопки
         IF nVAlign == DT_TOP .OR. nVAlign == DT_CENTER
            nY += nHBtn + nGBtn
         ELSE
            nY -= ( nHBtn + nGBtn )
         ENDIF
      ENDIF

      IF lBtnIco .AND. LEN(aIco) > 0  // кнопки с иконками
         // при первом построении изменить размеры иконки
         This.&(cN).ImageWidth  := nIcoSize
         This.&(cN).ImageHeight := nIcoSize
         This.&(cN).Icon        := LoadIconByName( aIco[1], nIcoSize, nIcoSize )
      ENDIF

   NEXT

RETURN aBtnObj

///////////////////////////////////////////////////////////////////
FUNCTION myMouseHL(n)
   LOCAL hIco, o := This.Cargo

   IF n == 2
      This.FontColor    := o:aFntClr2
      This.FontSize     := o:nFSize2
      This.FontBold     := o:lBold2
      This.GradientFill := o:aGrFill
   ELSE
      This.FontColor    := o:aFntClr1
      This.FontSize     := o:nFSize
      This.FontBold     := o:lBold1
      This.GradientOver := o:aGrOver
   ENDIF

   IF o:lBtnIco      // кнопки с иконками
      hIco := LoadIconByName( o:aIco[n], o:nIcoSize, o:nIcoSize )
      This.Icon := hIco
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
// получить Width текста
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
RETURN "Вызов из: " + ProcName( nVal + 1 ) + "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

///////////////////////////////////////////////////////////////////
FUNCTION Icon32TempCreate()
   LOCAL cBuff := "AAABAAEAICAAAAEAIACoEAAAFgAAACgAAAAgAAAAQAAAAAEAIAAAAAAAgBAAAAAAAAAAAAAAAAAAAAAAAAAAAP/DAgL+3wsL+t8FBfzfBQX83wUF/N8LC/rfAAD+3wAA/98NDfrfBwf83wUF/N8FBfzfBQX83wsL+t8AAP7fAAD/3w0N+t8HB/zfBQX83wUF/N8LC/rfAAD+3wAA/98NDfrfBwf83wUF/N8FBfzfBQX83wsL+t8CAv7fAAD/wwAA/99eXuD/wcG+/7u7wP+7u8D/u7vA/8HBvv9dXeD/Cgr8/6urxv+9vcD/u7vA/7u7wP+7u8D/wcG+/11d4P8KCvz/q6vG/729wP+7u8D/u7vA/8HBvv9dXeD/Cgr8/6urxv+9vcD/u7vA/7u7wP+7u8D/wcG+/15e4P8AAP/fAAD/3wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD7/wAA/f8AAP3/AAD9/wAA/f8AAPv/AAD+/wAA//8AAPv/AAD9/wAA/f8AAP3/AAD7/wAA/v8AAP//AAD7/wAA/f8AAP3/AAD9/wAA/f8AAPv/AAD+/wAA/98AAP/fMDDv/2lp3P9gYN//YGDf/2Bg3/9padz/LS3v/wAA//+AgP//hYX//4KC//+Cgv//goL//42N//8+Pv//AAD//4CA//+Fhf//goL//4KC//+Njf//Pj7//wAA//+AgP//hYX//4KC//+Cgv//goL//42N//9BQf//AAD+3wAA/99oaNz/2Ni4/87OvP/Nzb7/zc3A/9bWvP9jY+L/CQn//////////////////////////////////4aG//8FBf////////////////////////////+Ghv//Bgb//////////////////////////////////4uL//8AAP7fAAD/3wAA//8AAP//AAD//wMD9f8DA/z/CQn6/wAA+/8AAPb/AAD//wAA//8AAP//AAD+/wMD9f8LC/3/AAD9/wAA/f8ODvj/AAD//wAA//8AAP//AAD+/wAA8/8AAPP/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2tr6f9hYXL/Xl4A/2BgBP9hYQP/YmIB/2JiE/99fe3/hYX//4WF//9wcIH/WloA/2BgAv9iYgL/YmIC/2JiAP9KSkf/dnb7/4qK//9ycoL/XFwA/2BgDv+EhPP/goL//4CA//+AgP//gID//4yM//9AQP//AAD+3wAA/99oaNz/2NjK/5ycWv9oaAD/ZGQA/2FhAP9hYQD/Y2MA/6ysTv////3//////7+/fv9lZQD/ZGQA/19fAP9fXwD/ZGQA/2dnAP+vr0f//////7+/gP9lZQD/enoA////+////////////////////////////4uL//8AAP7fAAD/3wAA//8AAP//KyuD/2VlAP9nZwH/tbWE/5OTSv9XVwD/W1sA/0hIR/8AAP//KCiD/2JiAP9oaAP/tbWF/7e3iP9xcRL/YGAA/1BQAP8AAP//KCiF/2NjAP9UVAT/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2ho8f9jY3D/ZGQA/2xsCv///////////5GRR/9eXgD/ZmYA/4OD//90dIX/YmIA/3JyFP///////////5GRSP9iYgD/ZmYB/4OD//90dIb/ZGQA/2pqDP91dfH/dnb//3V18/+Dg///gYH//4yM//9AQP//AAD+3wAA/99oaNz/19fL/5ubXf9jYwD/Z2cC////////////traF/1lZAP95eQL//////7+/hv9iYgD/YGAA/25uDf9tbQv/Xl4A/2ZmAP91dQr//////7+/hP9jYwD/aGgA/319DP9/fwD/kJAO////8////////////4uL//8AAP7fAAD/3wAA//8AAP//KyuD/2NjAP9nZwL//v79//////+0tIH/V1cA/1NTAv8AAP//KSmG/2NjAP9gYAD/W1sA/1paAP9gYAD/YmIA/xAQrv8AAP//KiqD/2NjAP9gYAD/WloA/1VVAP9HRwD/AADz/wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2ho8f9jY3D/Y2MA/2hoBP///////////7Cwev9cXAD/ZmYB/4OD//90dIb/YmIA/2hoA/+1tYX/t7eI/3JyEf9kZAD/RkZR/319//91dYP/YmIA/2hoA/+0tIP/ubmG/7S0h/+AgP//fn7//4yM//9AQP//AAD+3wAA/99oaNz/19fL/5ubXf9kZAD/bW0M//r69v/Z2cD/bGwG/2RkAP92dgj//////7+/hP9iYgD/cnIU////////////kZFI/2RkAP97ewD//////7+/hf9iYgD/cnIU////////////////////+////////////4uM//8AAP7fAAD/3wAA//8AAP//KyuD/2VlAP9gYAD/cHAR/2VlAP9cXAD/U1MA/wAAsP8AAP//KSmC/2JiAP9gYAD/bm4N/21tC/9dXQD/X18A/0hICP8AAP//KCiE/2JiAP9gYAD/bW0M/2hoBP9oaAD/W1sO/wAA8/8AAf//AAD//wAA/t8AAP/fMDDv/2ho7v9hYWb/Y2MA/2FhAP9iYgD/ZmYA/2hoAP+hobP/fn7//4GB//9vb3T/XFwA/19fAP9kYwD/ZGQA/19fAP9vbwD/np6v/319//9xcXb/ZWQA/2VlAP9hYQD/W1sA/1paAP9eXgD/fHzz/4yL//9AP///AAD+3wAA/99obNz/2drA/7Cwjv+QkFf/kpJh/5SUYP9hZGv/LC+7//r6/////////////+PjwP+urnT/tbSA/3F0gP86PoD/o6R7/9jYuv///////////+fmwf9ucXT/Oj6A/6amgP+ysoD/sLB9/7m5gf////7//////4uM//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAC//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AA///AAD//wAA//8AAP//AAD//wAA//8AAP//AgX//wAA//8AAP//AAD//wAA//8AAv//AAD//wAA/t8AAP/fMDvv/2lr3P9gYN//YGDf/2Bg3/9paNz/LTbv/wAD//9/gP//g4P//4CA//+AgP//gID//4yM//89O///AAD//3+A//+Dg///gID//4CA//+Mi///PUX//wAA//9/gP//g4P//4CA//+AgP//gID//4yL//9AP///AAD+3wAA/99oaNz/2Ne2/8/Puv/Pz7r/z8+6/9jYtv9lZNz/Cwz//////////////////////////////////4iJ//8ICv////////////////////////////+Iif//CAr//////////////////////////////////4uM//8AAP7fAAD/3wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAv//AAD//wAA/t8AAP/fMDDv/2lp3P9gYN//YGDf/2Bg3/9padz/LSzv/wAC//9/gf//g4P//4CA//+AgP//gID//4yL//89Rv//AAD//3+A//+Dg///gID//4CA//+Mi///PUb//wAA//9/fv//g4P//4CA//+AgP//gID//4yL//9AP///AAD+3wAA/99obNz/2Ni2/8/Puv/Pz7r/z8+6/9jYtv9lZNz/Cw3//////////////////////////////////4iG//8ICf////////////////////////////+Ih///CAn//////////////////////////////////4uL//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDvv/2lq3P9gYN//YGDf/2Bg3/9padz/LSzv/wAA//9/fv//g4P//4CA//+AgP//gID//4yL//89Rv//AAH//3+A//+Dg///gID//4CA//+Mi///PUb//wAB//9/gP//g4P//4CA//+AgP//gID//4yM//9AQP//AAD+3wAA/99oa9z/2Ni2/8/Puv/Pz7r/z8+6/9jXtv9lZ9z/Cw7//////////////////////////////////4iJ//8ICv////////////////////////////+Iif//CAr//////////////////////////////////4uL//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAP//wAA//8AAP//AAD//wAA//8AAP//AAH//wAA//8AAv//AAD//wAA//8AAP//AAD//wAA//8AAP//AAP//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/98AAP/fLjnw/2Vn3f9cW+D/XFzg/1xc4P9lZN3/KzTw/wAA//9bXd//XVze/1pa3/9aWt//Wlrf/2Rj3P8rNPD/AAD//1ta3/9dXd7/Wlrf/1pa3/9kZNz/Kynw/wAA//9bXd//XVze/1pa3/9aWt//Wlrf/2Rk3P8tLe//AAD/3wAA/99ra9v/4+Oz/9XVuP/V1bj/1dW4/+Pjs/9oZ9z/AAD//8zLuv/Z2bb/1dW4/9XVuP/V1bj/4+Oz/2hn3P8AAP//zMy6/9nZtv/V1bj/1dW4/+Pjs/9oZ9z/AAD//8zLuv/Z2bb/1dW4/9XVuP/V1bj/4+Oz/2tr2/8AAP/fAAD/319f3//Jybz/vb3A/729wP+9vcD/yci8/1xg4P8AAP//tLTC/8DAvv+9vcD/vb3A/729wP/Jybz/XFzg/wAA//+0tML/wMC+/729wP+9vcD/ycm8/1xc4P8AAf//tLXC/8DAvv+9vcD/vb3A/729wP/Jybz/X1/f/wAA/98AAP/fAQH//wgI/f8CAv//AgL//wIC//8IB/3/AAv//wAA//8JCfz/AwP+/wIC//8CAv//AgL//wgI/f8AAP//AAD//wkJ/P8DA/7/AgL//wIC//8ICP3/AAD//wAA//8JDfz/AwP+/wIC//8CAv//AgL//wgI/f8BAf//AAD/3wAA/sMAAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP7DAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
   LOCAL cBin, cFile := GetUserTempFolder() + "\MiniGui_2dbf32.ico"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile
