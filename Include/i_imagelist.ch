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

#command DEFINE IMAGELIST <name> ;
	[ <dummy1: OF, PARENT> <parent> ] ;
	[ BUTTONSIZE <w> , <h>  ];
	[ IMAGE <aImage> ] ;
	[ IMAGEMASK <aImageMask> ] ;
	[ COLORMASK <aColorMask> ] ;
	[ IMAGECOUNT <ImageCount> ] ;
	[ <mask: MASK> ] ;
	=>;
	_DefineImageList ( <"name"> , <"parent"> , <w>, <h> ,<aImage> , <aImageMask>, <aColorMask> , <ImageCount> , <.mask.> )


#command DRAW IMAGELIST <name> ;
	<dummy1: OF, PARENT> <parent> ;
	AT <row> , <col> ;
	IMAGEINDEX <ImageIndex> ;
	=>;
	_DrawImageFromImageList ( <"name">, <"parent">, <ImageIndex>, <col> , <row> )

#command RELEASE IMAGELIST <name> ;
	<dummy1: OF, PARENT> <parent> ;
	=>;
	_DestroyImageList ( <"name">, <"parent"> )

#command ERASE IMAGE <name> ;
	<dummy1: OF, PARENT> <parent>  ;
	AT <row> , <col> ;
	=>;
	_EraseImage ( <"name">, <"parent">, <col> , <row>)

#command BEGINDRAG IMAGE <name> ;
	<dummy1: OF, PARENT> <parent>  ;
	AT <row> , <col> ;
	IMAGEINDEX <ImageIndex> ;
	=>;
	_BeginDragImage  ( <"name">, <"parent">, <ImageIndex>, <col> , <row>)


#command ENTERDRAG IMAGE AT <row> , <col> ;
	=>;
	_DragEnterImage ( <col> , <row> )  

#command MOVE IMAGE AT <row> , <col> ;
	=>;
	_MoveImage ( <col> , <row> )   

#command ENDDRAG IMAGE ;
	=>;
	_EndDragImage () 


#command ADD IMAGE <image> [ MASK <mask> ] TO <control> OF <parent> ;
	=>;
	DoMethod ( <"parent"> , <"control"> , 'AddImage' , <image> , <mask> )


#command ADD MASKEDIMAGE <image> [ COLOR <aColor> ] TO <control> OF <parent> ;
	=>;
	DoMethod ( <"parent"> , <"control"> , 'AddImageMask' , <image> , <aColor> )


#command DELETE IMAGE <index> FROM <control> OF <parent> ;
	=>;
	DoMethod ( <"parent"> , <"control"> , 'DeleteImage' , <index> )
