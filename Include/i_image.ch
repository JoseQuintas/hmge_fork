/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 http://harbourminigui.googlepages.com/

 This program is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 2 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with
 this software; see the file COPYING. If not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 visit the web site http://www.gnu.org/).

 As a special exception, you have permission for additional uses of the text
 contained in this release of Harbour Minigui.

 The exception is that, if you link the Harbour Minigui library with other
 files to produce an executable, this does not by itself cause the resulting
 executable to be covered by the GNU General Public License.
 Your use of that executable is in no way restricted on account of linking the
 Harbour-Minigui library code into it.

 Parts of this project are based upon:

   "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
     Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#xcommand @ <row>,<col> IMAGE <name> ;
   [ID <nId>] ;
   [ <dummy1: OF, PARENT, DIALOG> <parent> ] ;
   PICTURE <filename> ;
   [ <dummy2: ACTION,ON CLICK,ONCLICK> <action> ] ;
   [ ON DBLCLICK <dblclick> ] ;
   [ ON RCLICK <rclick> ] ;
   [ <dummy3: ON MOUSEHOVER, ONMOUSEHOVER> <overproc> ] ;
   [ <dummy4: ON MOUSELEAVE, ONMOUSELEAVE> <leaveproc> ] ;
   [ WIDTH <w> ] ;
   [ HEIGHT <h> ] ;
   [ <stretch: STRETCH> ] ;
   [ <transparent: TRANSPARENT> ] ;
   [ BACKGROUNDCOLOR <backgroundcolor> ] ;
   [ <adjustimage: ADJUSTIMAGE, ADJUST> ] ;
   [ ALPHALEVEL <nAlpha> ] ;
   [ TOOLTIP <tooltip> ] ;
   [ HELPID <helpid> ] ;
   [ <invisible: INVISIBLE> ] ;
   [ ON INIT <bInit> ] ;
 =>;
 _DefineImage ( <(name)>, <(parent)>, <col>, <row>, <filename>, <w>, <h>, <{action}>, <tooltip>, <helpid>, <.invisible.>, <.stretch.>, <backgroundcolor>, <.transparent.>, <.adjustimage.>, <{overproc}>, <{leaveproc}>, <nAlpha>, <nId>, <bInit>, <{dblclick}> , <{rclick}> )


#xcommand @ <row>,<col> IMAGE <name> ;
   [ID <nId>] ;
   [ <dummy1: OF, PARENT, DIALOG> <parent> ] ;
   PICTURE <filename> ;
   [ <dummy2: ACTION,ON CLICK,ONCLICK> <action> ] ;
   [ ON DBLCLICK <dblclick> ] ;
   [ ON RCLICK <rclick> ] ;
   [ <dummy3: ON MOUSEHOVER, ONMOUSEHOVER> <overproc> ] ;
   [ <dummy4: ON MOUSELEAVE, ONMOUSELEAVE> <leaveproc> ] ;
   [ WIDTH <w> ] ;
   [ TOOLTIP <tooltip> ] ;
   [ HEIGHT <h> ] ;
   [ <stretch: STRETCH> ] ;
   WHITEBACKGROUND ;
   [ <transparent: TRANSPARENT> ] ;
   [ <adjustimage: ADJUSTIMAGE, ADJUST> ] ;
   [ ALPHALEVEL <nAlpha> ] ;
   [ HELPID <helpid> ] ;
   [ <invisible: INVISIBLE> ] ;
   [ ON INIT <bInit> ] ;
 =>;
 _DefineImage ( <(name)>, <(parent)>, <col>, <row>, <filename>, <w>, <h>, <{action}>, <tooltip>, <helpid>, <.invisible.>, <.stretch.>, { 255 , 255 , 255 }, <.transparent.>, <.adjustimage.>, <{overproc}>, <{leaveproc}>, <nAlpha>, <nId>, <bInit>, <{dblclick}> , <{rclick}> )


#xcommand REDEFINE IMAGE <name> ;
   ID <nId> ;
   [ <dummy1: OF, PARENT, DIALOG> <parent> ] ;
   PICTURE <filename> ;
   [ <dummy2: ACTION,ON CLICK,ONCLICK> <action> ] ;
   [ ON DBLCLICK <dblclick> ] ;
   [ ON RCLICK <rclick> ] ;
   [ <stretch: STRETCH> ] ;
   [ <whitebg: WHITEBACKGROUND> ] ;
   [ <transparent: TRANSPARENT> ] ;
   [ HELPID <helpid> ] ;
   [ <invisible: INVISIBLE> ] ;
   [ ON INIT <bInit> ] ;
 =>;
 _DefineImage ( <(name)>, <(parent)>, 0, 0, <filename>, 0, 0, <{action}>, , <helpid>, <.invisible.>, <.stretch.>, iif( <.whitebg.>, { 255 , 255 , 255 }, ), <.transparent.>, , , , , <nId> , <bInit> , <{dblclick}> , <{rclick}> )
