*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File QUICKWIN.FOR
*     --------------------------------------------------------------------------
      subroutine initialise QUICKWIN
*	use IFQWIN
      use DFLIB
 	type (qwinfo) qw
 	type (windowconfig) wc
*     return the size and position of the window -------------------------------
 	i4=getwsizeqq(0,QWIN$SIZECURR,qw)
*     return the current window properties -------------------------------------
 	i5=getwindowconfig(wc)
 	wc.title='Calculations ... 'C
*     sets current windows properties ------------------------------------------
 	i5=setwindowconfig(wc)
 	qw.type=QWIN$MAX
*     sets size and position of window -----------------------------------------
 	i4=setwsizeqq(0,qw)
*     sets current background colour for text ----------------------------------
      i4=setbkcolor(0)
*     clear the screen ---------------------------------------------------------
*	call clearscreen ($GCLEARSCREEN)
 	return
 	end
 	
 	
      subroutine change colour of text(Lcolour)
* 	use IFQWIN
      use DFLIB
 	i4=settextcolorrgb(#FFFFFF) ! bright white (11)
 	if ( Lcolour .eq. 10 ) then
 	i4=settextcolorrgb(#00FF00) ! bright green (10)
 	endif
 	if ( Lcolour .eq. 49 ) then
 	i4=settextcolorrgb(#FF0000) ! dull blue (49)
 	endif
 	if ( Lcolour .eq. 11 ) then
 	i4=settextcolorrgb(#FFFF00) ! bright turquoise (11)
 	endif
 	if ( Lcolour .eq. 20 ) then
 	i4=settextcolorrgb(#0000FF) ! bright red (20)
 	endif
 	if ( Lcolour .eq. 13 ) then
 	i4=settextcolorrgb(#FF00FF) ! bright magenta (13)
 	endif
 	if (Lcolour .eq. 14) then
 	i4=settextcolorrgb(#00FFFF) ! bright yellow (14)
 	endif
 	if (Lcolour .eq. 34) then
 	i4=settextcolorrgb(#008080) ! dull yellow (34)
 	endif
 	if (Lcolour .eq. 35) then
 	i4=settextcolorrgb(#C6E7CE) ! dull yellow (35)
 	endif
 	if ( Lcolour .eq. 15 ) then
 	i4=settextcolorrgb(#FFFFFF) ! bright white (15)
 	endif
 	if ( Lcolour .eq. 16 ) then
 	i4=settextcolorrgb(#000000) ! black (99)
 	endif
 	if ( Lcolour .eq. 16 ) then
 	i4=settextcolorrgb(#808080) ! dark grey (16)
 	endif
 	if ( Lcolour .eq. 18 ) then
 	i4=settextcolorrgb(#C0C0C0) ! light grey (18)
 	endif
 	if ( Lcolour .eq. 19 ) then
 	i4=settextcolorrgb(#8080FF) ! light pink (19)
 	endif
 	if ( Lcolour .eq. 12 ) then
 	i4=settextcolorrgb(#0080FF) ! orange (12)
 	endif
 	if ( Lcolour .eq. 21 ) then
 	i4=settextcolorrgb(#000080) ! dull red (21)
      endif
 	if ( Lcolour .eq. 22 ) then
 	i4=settextcolorrgb(#CD919E) ! light blue (22)
 	endif
 	if ( Lcolour .eq. 36 ) then
 	i4=settextcolorrgb(#800080) ! dull magenta (36)
 	endif
 	if ( Lcolour .eq. 37 ) then
 	i4=settextcolorrgb(#808000) ! dull turquoise (37)
 	endif
 	if ( Lcolour .eq. 38 ) then
 	i4=settextcolorrgb(#008000) ! dull green (38)
 	endif
 	if ( Lcolour .eq. 50 ) then
 	i4=settextcolorrgb(#4682B4) ! steel blue (50)
 	endif
 	if ( Lcolour .eq. 51 ) then
 	i4=settextcolorrgb(#00FF7F) ! spring green (51)
 	endif
 	if ( Lcolour .eq. 52 ) then
 	i4=settextcolorrgb(#DAA520) ! golden rod (52)
 	endif
 	if ( Lcolour .eq. 53 ) then
 	i4=settextcolorrgb(#CD5C5C) ! indian red (53)
 	endif
 	if ( Lcolour .eq. 54 ) then
 	i4=settextcolorrgb(#9932CC) ! dark orchid (54)
 	endif
 	if ( Lcolour .eq. 55 ) then
 	i4=settextcolorrgb(#458B74) ! aquamarine (55)
 	endif
 	return
      end


      subroutine write colour of text
* 	use IFQWIN
      use DFLIB
      call change colour of text (11)      
      write(*,*)'colour 11'
      call change colour of text (10) ! bright green      
      write(*,*)'colour 10'
      call change colour of text (49)      
      write(*,*)'colour 49'
      call change colour of text (11)      
      write(*,*)'colour 11'
      call change colour of text (20)      
      write(*,*)'colour 20'
      call change colour of text (13)      
      write(*,*)'colour 13'
      call change colour of text (14)      
      write(*,*)'colour 14'
      call change colour of text (34)      
      write(*,*)'colour 34'
      call change colour of text (35)      
      write(*,*)'colour 35'
      call change colour of text (15)      
      write(*,*)'colour 15'
      call change colour of text (99)      
      write(*,*)'colour 99'
      call change colour of text (16)      
      write(*,*)'colour 16'
      call change colour of text (18)      
      write(*,*)'colour 18'
      call change colour of text (19)      
      write(*,*)'colour 19'
      call change colour of text (12)      
      write(*,*)'colour 12'
      call change colour of text (21)      
      write(*,*)'colour 21'
      call change colour of text (22)      
      write(*,*)'colour 22'
      call change colour of text (36)      
      write(*,*)'colour 36'
      call change colour of text (37)      
      write(*,*)'colour 37'
      call change colour of text (38)      
      write(*,*)'colour 38'
      call change colour of text (50)      
      write(*,*)'colour 50 steel blue'
      call change colour of text (51)      
      write(*,*)'colour 51 spring green'
      call change colour of text (52)      
      write(*,*)'colour 52 golden rod'
      call change colour of text (53)      
      write(*,*)'colour 53 indian red'
      call change colour of text (54)      
      write(*,*)'colour 54 dark orchid'
      call change colour of text (55)      
      write(*,*)'colour 55 aquamarine'
 	return
      end

 

      
