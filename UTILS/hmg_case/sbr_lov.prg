/*
 * This functions is for program generated by HMGCASE
 * developed by Dragan Cizmarevic < dragancesu(at)gmail.com > 
 */
#include <hmg.ch>

STATIC aGrid := {}

FUNCTION lov_func( _dbf, _fld1, _fld2 )

PRIVATE _redova, _red := 0

SELECT &_dbf
_redova = reccount()

if !empty(dbfilter())
   count to _redova for dbfilter()
endif

_redova := min ( _redova, 100 )

PUBLIC aRows [_redova] [4], _RetVal
PUBLIC cPublicSearchString := ""

SET ORDER TO 2
dbgotop()

DO WHILE .not. eof()
   _red++
   _fld1r = str( &_fld1 )
   _fld2r = &_fld2
   aRows [_red]:= { _fld1r, _fld2r, str(_red), str(recno()) }
   
   if _red >= 100
      exit
   endif  
   
   dbskip()
ENDDO

   DEFINE WINDOW Lov_Form_fn ; 
      AT 0,0 ; 
      WIDTH 500 ; 
      HEIGHT 300 ; 
      TITLE "List of Value " ; 
      MODAL 

      @ 20,20 GRID Lov_Grid_fn ; 
         WIDTH 450 ; 
         HEIGHT 220 ; 
         HEADERS {"Key", "Name", "Record", "Recno()" } ; 
         WIDTHS {100,350,10,10} ; 
         ITEMS aRows ; 
         VALUE 1 ; 
         ON DBLCLICK sel_row_fn() ; 
         JUSTIFY { GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT } 

   END WINDOW 

   Lov_Form_fn.Lov_Grid_fn.SetFocus 

   CENTER WINDOW Lov_Form_fn

   ACTIVATE WINDOW Lov_Form_fn

RETURN _Retval 
*:---------------------------------------------------------------
FUNCTION sel_row_fn()

   SET ORDER TO 1 
   _Retval := aRows[ Lov_Form_fn.Lov_Grid_fn.value, 1 ] 
   Lov_Form_fn.Release 

RETURN val(_RetVal) 
*:*******************************************************************
FUNCTION lovs_func( _dbf, _fld1, _fld2 )

PRIVATE _red := 0, _xdbf, _xfld1, _xfld2, _retval

_xdbf := _dbf
_xfld1 := _fld1
_xfld2 := _fld2

   DEFINE WINDOW Lov_Form_fns ; 
      AT 0,0 ; 
      WIDTH 500 ; 
      HEIGHT 300 ; 
      TITLE "List of Value " ; 
      MODAL  

      @ 10, 10 LABEL L1 ; 
         WIDTH 80 ; 
         HEIGHT 20 ; 
         VALUE "Search string:" 

      @ 10, 100 Textbox WhatToSearch ; 
         WIDTH 95 ; 
         HEIGHT 20 ; 
         VALUE "" ; 
         UPPERCASE ;
         ON CHANGE Proc_OnChange_fns()

      @ 40,20 GRID Lov_Grid_fns ; 
         WIDTH 450 ; 
         HEIGHT 200 ; 
         HEADERS {"Key", "Name", "Record", "Recno()" } ; 
         WIDTHS {100,350,10,10} ; 
         ON DBLCLICK select_row_fns() ; 
         JUSTIFY { GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT } 

   END WINDOW 

   Load_aGrid( 0 ) 
   Lov_Form_fns.WhatToSearch.SetFocus 

   CENTER WINDOW Lov_Form_fns

   ACTIVATE WINDOW Lov_Form_fns

RETURN _Retval 
*:**********************************************
FUNCTION Load_aGrid ( _ord )

if _ord = 1
   aGrid := {}
endif

SELECT &_xdbf

SET ORDER TO 2
if _ord = 0
   dbgotop()
endif

_red := 0
DO WHILE .not. eof()
   _red++
   
   _fld1r = str(&_xfld1 )
   _fld2r = &_xfld2
   _fld3r = str(_red)
   _fld4r = str(recno())

   aadd ( aGrid, { _fld1r, _fld2r, _fld3r, _fld4r } )
   
   if _red >= 100
      exit
   endif  
   
   dbskip()
ENDDO
 
   Lov_Form_fns.Lov_Grid_fns.DeleteAllItems()
   FOR i = 1 to LEN(aGrid)
      Lov_Form_fns.Lov_Grid_fns.AddItem(aGrid[i])
   NEXT
   Lov_Form_fns.Lov_Grid_fns.Refresh
      
RETURN Nil
*:------------------------------------------------
FUNCTION Proc_OnChange_fns() 

_key := Lov_Form_fns.WhatToSearch.Value

SELECT &_xdbf
SET ORDER TO 2
dbgotop()
seek _key

if found()

   Load_aGrid( 1 )

else

   _len := len(_key)
   Lov_Form_fns.WhatToSearch.Value := substr(Lov_Form_fns.WhatToSearch.Value,1,_len-1)

endif

Lov_Form_fns.Lov_grid_fns.Value := 1
Lov_Form_fns.Lov_grid_fns.Refresh

RETURN NIL
*:------------------------------------------
FUNCTION select_row_fns()

   SET ORDER TO 1 
   _Retval = aGrid[ Lov_Form_fns.Lov_Grid_fns.value, 1 ] 
   Lov_Form_fns.Release 

RETURN _RetVal
*:********************************************
function year_start()

_datum := '01.01.' + alltrim(str(year(date())))

return ctod(_datum)
*:---------------------------------------
function year_end()

_datum := '31.12.' + alltrim(str(year(date())))

return ctod(_datum)
*:---------------------------------------
Function GetColValue( xObj, xForm, nCol)

   Local nPos:= GetProperty(xForm, xObj, 'Value')
   Local aRet:= GetProperty(xForm, xObj, 'Item', nPos)

Return aRet[nCol] 
