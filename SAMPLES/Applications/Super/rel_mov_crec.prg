/*
  sistema     : superchef pizzaria
  programa    : relat�rio movimenta��o contas a receber
  compilador  : xharbour 1.2 simplex
  lib gr�fica : minigui 1.7 extended
  programador : marcelo neves
*/

#include 'minigui.ch'
#include 'miniprint.ch'
#include 'super.ch'

function relatorio_crec_001()

         define window form_mov_crec;
                at 000,000;
                width 400;
                height 250;
                title 'Contas a Receber por per�odo';
                icon path_imagens+'icone.ico';
                modal;
                nosize

                @ 010,010 label lbl_001;
                          of form_mov_crec;
                          value 'Escolha o intervalo de datas';
                          autosize;
                          font 'tahoma' size 010;
                          bold;
                          fontcolor _preto_001;
                          transparent

                @ 040,010 datepicker dp_inicio;
                          parent form_mov_crec;
                          value date();
                          width 150;
                          height 030;
                          font 'verdana' size 014
                @ 040,170 datepicker dp_final;
                          parent form_mov_crec;
                          value date();
                          width 150;
                          height 030;
                          font 'verdana' size 014

                * linha separadora
                define label linha_rodape
                       col 000
                       row form_mov_crec.height-090
                       value ''
                       width form_mov_crec.width
                       height 001
                       backcolor _preto_001
                       transparent .F.
                end label

                * bot�es
                define buttonex button_ok
                       picture path_imagens+'img_relatorio.bmp'
                       col form_mov_crec.width-255
                       row form_mov_crec.height-085
                       width 150
                       height 050
                       caption 'Ok, imprimir'
                       action relatorio()
                       fontbold .T.
                       tooltip 'Gerar o relat�rio'
                       flat .F.
                       noxpstyle .T.
                end buttonex
                define buttonex button_cancela
                       picture path_imagens+'img_sair.bmp'
                       col form_mov_crec.width-100
                       row form_mov_crec.height-085
                       width 090
                       height 050
                       caption 'Voltar'
                       action form_mov_crec.release
                       fontbold .T.
                       tooltip 'Sair desta tela'
                       flat .F.
                       noxpstyle .T.
                end buttonex

                on key escape action thiswindow.release

         end window

         form_mov_crec.center
         form_mov_crec.activate

         return(nil)
*-------------------------------------------------------------------------------
static function relatorio()

       local x_de  := form_mov_crec.dp_inicio.value
       local x_ate := form_mov_crec.dp_final.value

       local p_linha := 045
       local u_linha := 260
       local linha   := p_linha
       local pagina  := 1

       local x_soma_dia   := 0
       local x_soma_geral := 0
       local x_old_data   := ctod('  /  /  ')

       dbselectarea('contas_receber')
       contas_receber->(ordsetfocus('data'))
       contas_receber->(dbgotop())
	    ordscope(0,dtos(x_de))
	    ordscope(1,dtos(x_ate))
       contas_receber->(dbgotop())

       SELECT PRINTER DIALOG PREVIEW

       START PRINTDOC NAME 'Gerenciador de impress�o'
       START PRINTPAGE

       cabecalho(pagina)
       
       while .not. eof()

             x_old_data := contas_receber->data
             x_soma_dia := x_soma_dia + contas_receber->valor
             
             @ linha,015 PRINT dtoc(contas_receber->data) FONT 'courier new' SIZE 010
             @ linha,040 PRINT acha_cliente(contas_receber->cliente) FONT 'courier new' SIZE 010
             @ linha,120 PRINT acha_forma_recebimento(contas_receber->forma) FONT 'courier new' SIZE 010
             @ linha,160 PRINT trans(contas_receber->valor,'@E 999,999.99') FONT 'courier new' SIZE 010
             
             linha += 5
             
             if linha >= u_linha
                END PRINTPAGE
                START PRINTPAGE
                pagina ++
                cabecalho(pagina)
                linha := p_linha
             endif
             
             @ linha,040 PRINT alltrim(contas_receber->obs) FONT 'courier new' SIZE 010
             @ linha,120 PRINT alltrim(contas_receber->numero) FONT 'courier new' SIZE 010

             linha += 5

             if linha >= u_linha
                END PRINTPAGE
                START PRINTPAGE
                pagina ++
                cabecalho(pagina)
                linha := p_linha
             endif

             contas_receber->(dbskip())

             if contas_receber->data <> x_old_data
                @ linha,100 PRINT 'Subtotal : R$ ' FONT 'courier new' SIZE 010 BOLD
                @ linha,160 PRINT trans(x_soma_dia,'@E 999,999.99') FONT 'courier new' SIZE 010 BOLD
                x_soma_geral := x_soma_geral + x_soma_dia
                x_soma_dia   := 0
                linha += 5
             endif
             
       end

       @ linha,090 PRINT 'Total do per�odo : R$ ' FONT 'courier new' SIZE 010 BOLD
       @ linha,160 PRINT trans(x_soma_geral,'@E 999,999.99') FONT 'courier new' SIZE 010 BOLD

       rodape()
       
       END PRINTPAGE
       END PRINTDOC

	    ordscope(0,0)
	    ordscope(1,0)
       contas_receber->(dbgotop())

       return(nil)
*-------------------------------------------------------------------------------
static function cabecalho(p_pagina)

       local x_de  := form_mov_crec.dp_inicio.value
       local x_ate := form_mov_crec.dp_final.value
       
       @ 007,010 PRINT IMAGE path_imagens+'logotipo.bmp' WIDTH 050 HEIGHT 020 STRETCH
       @ 010,070 PRINT 'CONTAS A RECEBER - PER�ODO' FONT 'courier new' SIZE 018 BOLD
       @ 018,070 PRINT dtoc(x_de)+' at� '+dtoc(x_ate) FONT 'courier new' SIZE 014
       @ 024,070 PRINT 'p�gina : '+strzero(p_pagina,4) FONT 'courier new' SIZE 012

       @ 030,000 PRINT LINE TO 030,205 PENWIDTH 0.5 COLOR _preto_001

       @ 035,015 PRINT 'DATA' FONT 'courier new' SIZE 010 BOLD
       @ 035,040 PRINT 'CLIENTE/' FONT 'courier new' SIZE 010 BOLD
       @ 035,120 PRINT 'FORMA RECEBIMENTO/' FONT 'courier new' SIZE 010 BOLD
       @ 035,160 PRINT 'VALOR R$' FONT 'courier new' SIZE 010 BOLD
       @ 040,040 PRINT 'OBSERVA��O' FONT 'courier new' SIZE 010 BOLD
       @ 040,120 PRINT 'N�MERO' FONT 'courier new' SIZE 010 BOLD
       
       return(nil)
*-------------------------------------------------------------------------------
static function rodape()

       @ 275,000 PRINT LINE TO 275,205 PENWIDTH 0.5 COLOR _preto_001
       @ 276,010 PRINT 'impresso em '+dtoc(date())+' as '+time() FONT 'courier new' SIZE 008

       return(nil)