# REPORTS.MOD This Line Will be ignored!
[Declare]80/PREV
SET UNITS MM
SET JOB NAME [Simple Print demo]
SET SPLASH TO ... Por favor espere...    Produ��o em curso
SET PAPERSIZE DMPAPER_A4
define imagelist ILIST1 picture flags
SET CHARSET ANSI_CHARSET
set euro 1;set money 1;set separator ON
(aadd(format,ed_g_pic(Field->incoming)))
set money 0
//set debug list declare
DEFINE BRUSH BP1 STYLE BS_HATCHED COLOR LAVENDER HATCH HS_CROSS
DEFINE BRUSH B1 STYLE BS_HATCHED COLOR AQUA HATCH HS_DIAGCROSS
DEFINE PEN P1 STYLE PS_SOLID WIDTH 1 
DEFINE PEN P12 Ps_INSIDEFRAME WIDTH 0.2 COLOR 0,0,255
DEFINE FONT "xX" NAME [COURIER NEW] SIZE 12 UNDERLINE BOLD
SELECT FONT F0
SET ORIENTATION Portrait
var pizza c ''

(m->Pizza:="Hbprn")
set debug off
/*
msgbox(str(hbprn:UNITS),"Units")
msgbox(str( round(maxrow/lstep,0) ),"MaxRow")

(hbprn:reportdata())
*/
[HEAD]8
SET TEXTCOLOR BLACK
@(1*lstep),12 SAY [Pag.] FONT ARIAL SIZE 10 ;(1*lstep) 25 SAY trans(m->npag,[999]) FONT ARIAL SIZE 10
@8,14 picture ROSA.JPG size 18 17
@(1*lstep+3),105 SAY [INFORME DE ARTISTAS DE CINE] FONT ARIAL SIZE 16 BOLD ITALIC align center
@(1*lstep),178 SAY date() font [COURIER NEW] SIZE 10 BOLD align CENTER
@(2*lstep),178 SAY Time() FONT [COURIER NEW] SIZE 10 BOLD align CENTER
@(2*lstep+4),105 SAY [Segundo titulo] FONT ARIAL SIZE 16 BOLD ITALIC align center
@(4*lstep),11,(4*lstep),203 LINE
@(5*lstep),27 SAY [SIMPLE                   APELLIDO           DOBLE                     INGRESOS] font [COURIER NEW] SIZE 10 BOLD
@(5*lstep),52 SAY (Pizza) font [COURIER NEW] SIZE 20 BOLD
@(6*lstep),11 ,(6*lstep),203 LINE
SET TEXT ALIGN LEFT
@(52*lstep ),130,( ((maxRow)-1 ) ),(maxcol)-1 RECTANGLE pen p1 brush b1
// @(52*lstep ),130,( ((maxRow)-1 ) ),(maxcol)-1 DRAW TEXTpen p1 brush b1
@(19*lstep ) 120 (18*lstep ) 170 RECTANGLE pen p1 brush b1
set units mm //rowcol
//(hbprn:settextalign( 6 ))
//(19*lstep ) 20 (18*lstep ) 70 DRAW TEXT [Report Interpreter 1] STYLE DT_LEFT FONT [XX]
*(hbprn:drawtext((19*lstep ),20,(18*lstep ),70,[Report Interpreter 1],,"F0" ))
(18*lstep ),125 TEXTOUT [Report Interpreter 2] FONT "XX" COLOR BROWN
//[COURIER NEW] SIZE 12 BOLD UNDERLINE
set units MM

//@(55*lstep),133 PRINT [Report Interpreter] FONT [COURIER NEW] SIZE 16 BOLD
@(58*lstep),135 SAY [Generation      ] FONT [COURIER NEW] SIZE 16 BOLD UNDERLINE color lightsalmon
@(58*lstep),170 SAY [IV] FONT [COURIER NEW] SIZE 24 BOLD ITALIC color lightsalmon
set backcolor white
@(2*lstep+3),130,(4*lstep+4),145 DRAW IMAGELIST ILIST1 ICON 15 BACKGROUND WHITE
[BODY]40
@(NLINE*lstep),21 SAY Field->first FONT ARIAL SIZE 10 BOLD align left COLOR BLUE
@(NLINE*lstep),71 SAY Field->code FONT ARIAL SIZE 10 BOLD COLOR [255,0,150] align right
@(NLINE*lstep),74 PRINT Field->LAST FONT ARIAL SIZE 10 BOLD
@(NLINE*lstep),108 MEMOSAY FIELD->BIO len 35 FONT ARIAL SIZE 10 BOLD        color red
@(NLINE*lstep),200 SAY trans(Field->INCOMING,format[1]) color ORANGE FONT ARIAL SIZE 10 BOLD align right
[FEET]4
@((eline+3)*lstep), 9 SAY if(last_pag,"Bye bye [End of DEMO]",[]) FONT ARIAL SIZE 16 BOLD ITALIC color red
[END]
/*
@(52*LSTEP),50,(hbprn:maxRow)+1,(hbprn:maxcol)+1 RECTANGLE pen p1 brush bp1
@2,50,3.8,55 DRAW IMAGELIST ILIST1 ICON 15 BACKGROUND WHITE
*/
