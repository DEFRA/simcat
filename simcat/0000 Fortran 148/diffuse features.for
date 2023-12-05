*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     File Diffuse features.FOR   2011
*     --------------------------------------------------------------------------
*     Calculations for features - Diffuse pollution
*     --------------------------------------------------------------------------
*      1.	Agricultural livestock (25)
*      2.	Agricultural arable (27)  
*      3.	Highway (non-urban) runoff (29)
*      4.	Urban runoff (31)
*      5.	Direct Atmospheric Deposition (i.e. to water surfaces) (33)
*      6.	Natural background (35)
*      7.	Population not covered by STW point inputs (37) ... septic tanks
*      8.	Aggregated CSOs (40)
*      9.	Aggregated sewage works (42)
*     10.	Diffuse mines (46)
*     11.	Birds, boats and angling (48)
*     --------------------------------------------------------------------------

      subroutine turn on river based diffuse pollution
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 13',
     &' - basic)'/77('='))
      endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! diffuse pollution (13)

      KRFPOL13 = JF(KFEAT)
      KRQPOL13 = JQ(KFEAT)

*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL13 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT,IQ
      write(09,3644)KFEAT,IQ
      write(01,3644)KFEAT,IQ
      write( *,3644)KFEAT,IQ
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow to the ',
     &'river (13) ...'/33x,'Check feature number',i6/
     &33x,'Diffuse pollution not switched on ...'/33x,77('-'))
      KRFPOL13 = 0
      KRQPOL13 = 0
	call stop
      return
	endif
*     --------------------------------------------------------------------------
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL13, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Diffuse pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL13 = 0
      KRQPOL13 = 0
      return
      endif
*     --------------------------------------------------------------------------

      IF = KRFPOL13
      IQ = KRQPOL13
      call check for an infeasibly large river flow 

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end







*     --------------------------------------------------------------------------
      subroutine turn off river based diffuse pollution 
      include 'COM.FOR'

      KRFPOL13 = 0
      KRQPOL13 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution (river type - 13) ... '/77('='))
	endif
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970

*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of diffuse pollution...'/77('-'))
*     call get summaries and write out loads and quality (0)
  970 continue

      if ( FLOW(1) .lt. 1.0E9 ) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .1.'/
     &'*** Calculations proceeding ...'/100('*'))
      end



      subroutine turn on river based diffuse pollution from livestock
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 25',
     &' - livestock)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! livestock (25)

      KRFPOL25 = JF(KFEAT)
      KRQPOL25 = JQ(KFEAT)

*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL25 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from livestock ...'/33x,'Check feature number',i6/
     &33x,'Livestock pollution was not switched on ...'/
     &33x,77('-'))
      KRFPOL25 = 0
      KRQPOL25 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL25, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Livestock pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL25 = 0
      KRQPOL25 = 0
      return
      endif

      IF = KRFPOL25
      IQ = KRQPOL25

      call check for an infeasibly large river flow 

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off river based diffuse pollution from livestock 
      include 'COM.FOR'

      KRFPOL25 = 0
      KRQPOL25 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from livestock ... 25'/77('='))
	endif
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970

*     if ( nobigout .le. 0 ) write(01,1010)
*1010 format(/77('-')/'Calculated river quality immediately',
*    &' upstream of end of livestock pollution'/77('-'))
*     call get summaries and write out loads and quality (0)
  970 continue

      if ( FLOW(1) .lt. 1.0E9 ) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .2.'/
     &'*** Calculations proceeding ...'/100('*'))
      end





      subroutine turn on river based diffuse pollution from arable
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 27',
     &' - arable) '/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! arable (27)

      KRFPOL27 = JF(KFEAT)
      KRQPOL27 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL27 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow from arable ...'/
     &33x,'Check feature number',i6/
     &33x,'Arable pollution not switched on ...'/
     &33x,77('-'))
      KRFPOL27 = 0
      KRQPOL27 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL27, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Arable pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL27 = 0
      KRQPOL27 = 0
      return
      endif

      IF = KRFPOL27
      IQ = KRQPOL27
      call check for an infeasibly large river flow 

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end



      subroutine turn off river based diffuse pollution from arable
      include 'COM.FOR'

      KRFPOL27 = 0
      KRQPOL27 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from arable land ... 27'/77('='))
	endif
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970

*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of arable pollution...'/77('-'))
*     call get summaries and write out loads and quality (0)
  970 continue

      if ( FLOW(1) .lt. 1.0E9 ) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .3.'/
     &'*** Calculations proceeding ...'/100('*'))
      end


      subroutine turn on river based diffuse pollution from highways
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 29',
     &' - highway runoff)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! highway runoff (29)

      KRFPOL29 = JF(KFEAT)
      KRQPOL29 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL29 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from highways ...'/33x,'Check feature number',i6/
     &33x,'Highway pollution was not switched on ...'/
     &33x,77('-'))
      KRFPOL29 = 0
      KRQPOL29 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL29, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .eq.   0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Highway pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL29 = 0
      KRQPOL29 = 0
      return
      endif

      IF = KRFPOL29
      IQ = KRQPOL29
      call check for an infeasibly large river flow 

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end



      subroutine turn off river based diffuse pollution from highways 
      include 'COM.FOR'

      KRFPOL29 = 0
      KRQPOL29 = 0
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from highways ... 29'/77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end


      subroutine turn on river based diffuse pollution from urban
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 31',
     &' - urban runoff)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! urban runoff (31)

      KRFPOL31 = JF(KFEAT)
      KRQPOL31 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL31 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from urban runoff ...'/33x,'Check feature number',i6/
     &33x,'Urban pollution was not switched on ...'/
     &33x,77('-'))
      KRFPOL31 = 0
      KRQPOL31 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL31, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Urban pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL31 = 0
      KRQPOL31 = 0
      return
      endif

      IF = KRFPOL31
      IQ = KRQPOL31
      call check for an infeasibly large river flow 

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off river based diffuse pollution from urban 
      include 'COM.FOR'

      KRFPOL31 = 0
      KRQPOL31 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from urban runoff ... 31'/77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end

      
      subroutine turn on diffuse pollution from the atmosphere
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 33',
     &' - atmospheric deposition)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! atmospheric deposition (33)

      KRFPOL33 = JF(KFEAT)
      KRQPOL33 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL33 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from atmospheric pollution ...'/33x,'Check feature number',i6/
     &33x,'Atmospheric pollution pollution was not switched on ...'/
     &33x,77('-'))
      KRFPOL33 = 0
      KRQPOL33 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL33, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Atmospheric pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL33 = 0
      KRQPOL33 = 0
      return
      endif

      IF = KRFPOL33
      IQ = KRQPOL33
      call check for an infeasibly large river flow 

*     ==========================================================================
*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from the atmosphere
      include 'COM.FOR'

      KRFPOL33 = 0
      KRQPOL33 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from the atmosphere ... 33'/77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end


*     calculations for features - natural background (35) ---------------------------
      subroutine turn on diffuse pollution from background
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 35',
     &' - natural background)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! natural background (35)

      KRFPOL35 = JF(KFEAT)
      KRQPOL35 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL35 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from natural background ... 35'/33x,'Check feature number',i6/
     &33x,'Natural background was not switched on ...'/33x,77('-'))
      KRFPOL35 = 0
      KRQPOL35 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL35, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Natural background was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRFPOL35 = 0
      KRQPOL35 = 0
      return
      endif

      IF = KRFPOL35
      IQ = KRQPOL35
      call check for an infeasibly large river flow 

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from background
      include 'COM.FOR'

      KRFPOL35 = 0
      KRQPOL35 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse input from natural background ... 35'/77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end


*     calculations for features - diffuse mines (46) ---------------------------
      subroutine turn on diffuse pollution from mines
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 46',
     &' - mines)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! diffuse mines (46)

      KRFPOL46 = JF(KFEAT)
      KRQPOL46 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL46 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from mines ... 46'/33x,'Check feature number',i6/
     &33x,'Diffuse inputs from mines was not switched on ...'/
     &33x,77('-'))
      KRFPOL46 = 0
      KRQPOL46 = 0
      return
	endif
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRQPOL46 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3244)KFEAT
      write(34,3244)KFEAT
 3244 format(33x,77('-')/
     &33x,'No quality data specified for a diffuse inflow ',
     &'from mines ... 46'/33x,'Check feature number',i6/
     &33x,'Diffuse inputs from mines was not switched on ...'/
     &33x,77('-'))
      KRFPOL46 = 0
      KRQPOL46 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL46, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Diffuse inputs from mines was not not switched on ...'/
     &77('-'))
      write(33,1020)KFEAT
      KRFPOL46 = 0
      KRQPOL46 = 0
      return
      endif

      IF = KRFPOL46
      IQ = KRQPOL46
      call check for an infeasibly large river flow 

*     ==========================================================================
*     Note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from mines
      include 'COM.FOR'

      KRFPOL46 = 0
      KRQPOL46 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse input from mines ... 46'/77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end


*     calculations for features - birds, boats and angling (48) ----------------
      subroutine turn on diffuse pollution from birds
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 48',
     &' - birds, boats and angling)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! birds, boats and angling (48)

      KRFPOL48 = JF(KFEAT)
      KRQPOL48 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL48 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from birds etc ... 48'/33x,'Check feature number',i6/
     &33x,'Diffuse inputs from mines was not switched on ...'/
     &33x,77('-'))
      KRFPOL48 = 0
      KRQPOL48 = 0
      return
	endif
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRQPOL48 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3244)KFEAT
      write(34,3244)KFEAT
 3244 format(33x,77('-')/
     &33x,'No quality data specified for a diffuse inflow ',
     &'from birds etc ... 48'/33x,'Check feature number',i6/
     &33x,'Diffuse inputs from mines was not switched on ...'/
     &33x,77('-'))
      KRFPOL48 = 0
      KRQPOL48 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL48, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Diffuse inputs from birds etc was not not switched on ...'/
     &77('-'))
      write(33,1020)KFEAT
      KRFPOL48 = 0
      KRQPOL48 = 0
      return
      endif

      IF = KRFPOL48
      IQ = KRQPOL48
      call check for an infeasibly large river flow 

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from birds
      include 'COM.FOR'

      KRFPOL48 = 0
      KRQPOL48 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse input from birds, boats and angling ... 48'/
     &77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end



      subroutine turn on diffuse pollution from septic tanks
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 37',
     &' - septic tanks)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! septic tanks (37)

      KRFPOL37 = JF(KFEAT)
      KRQPOL37 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL37 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
      write(09,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from septic tanks ... 37'/33x,'Check feature number',i6/
     &33x,'This type of pollution was not switched on ...'/33x,77('-'))
      KRFPOL37 = 0
      KRQPOL37 = 0
      return
      endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL37, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Septic tank pollution was not not switched on ... 37'/
     &77('-'))
      write(33,1020)KFEAT
      KRFPOL37 = 0
      KRQPOL37 = 0
      return
      endif

      IF = KRFPOL37
      IQ = KRQPOL37
      call check for an infeasibly large river flow 

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from septic tanks
      include 'COM.FOR'

      KRFPOL37 = 0
      KRQPOL37 = 0
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
      write(21,1009)
 1009 format(77('=')/
     &'End of diffuse input from septic tanks ... 37'/77('='))
      endif
      endif
      call check for an infeasibly large river flow 
      return
      end


*     calculations for features - aggregated CSOs (40) -------------------------
      subroutine turn on diffuse pollution from aggregated CSOs
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (river-type ... 40',
     &' - aggregated CSOs)'/77('='))
	endif
	endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! aggregated CSOs (40)

      KRFPOL40 = JF(KFEAT)
      KRQPOL40 = JQ(KFEAT)
*     check data exist on the diffuse flow to be added to the river ------------
      if ( KRFPOL40 .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      write(34,3644)KFEAT
      write(09,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ',
     &'from aggregated CSOs ...'/33x,'Check feature number',i6/
     &33x,'This type of pollution was not switched on ...'/33x,77('-'))
      KRFPOL40 = 0
      KRQPOL40 = 0
      return
	endif
	  
*     check the quality data for the diffuse flow to be added to the river -----
      if ( quolity data ( KRQPOL40, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Aggregated CSOs pollution was not not switched on ...'/
     &77('-'))
      write(33,1020)KFEAT
      KRFPOL40 = 0
      KRQPOL40 = 0
      return
      endif

      IF = KRFPOL40
      IQ = KRQPOL40
      call check for an infeasibly large river flow 

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
	call stop
	endif
 8970 continue

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from aggregated CSOs
      include 'COM.FOR'

      KRFPOL40 = 0
      KRQPOL40 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from aggregated CSOs ... 40'/
     &77('='))
	endif
      call check for an infeasibly large river flow 
      return
      end


*     calculations for features - diffuse effluent discharges ------------------
      subroutine turn on discharge based diffuse pollution 
      include 'COM.FOR'

      call write message that diffuse discharge pollution has started
      KEPOL15 = JQ(KFEAT)
      if ( DISTP .lt. 1.0E-08 ) return

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! discharge based diffuse pollution

      if (FLOW(1) .gt. 1.0E9) then
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .4.'/
     &'*** Calculations proceeding ...'/100('*'))
      endif

      IQ = KEPOL15

      if ( IQ .eq.  0 ) return

      if ( pollution data(IQ,1,1)+0.001 )42,42,51
   42 if ( nobigout .le. 0 ) write(01,12)
   12 format(
     &'---------------------------------------------------------'/
     &'Quality data specified for diffuse pollution do not exist'/
     &'---------------------------------------------------------')
      return
   51 continue

      return
      end


*     switch off for diffuse pollution (effluent type) -------------------------
      subroutine turn off discharge based diffuse pollution
      include 'COM.FOR'

      KEPOL15 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
	endif
 1009 format(77('=')/
     &'End of diffuse pollution (discharge type - 15) ...'/
     &77('='))
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of diffuse pollution...'/77('-'))

*     call get summaries and write out loads and quality (0)
  970 continue

      if (FLOW(1) .lt. 1.0E9) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .5.'/
     &'*** Calculations proceeding ...'/100('*'))
      end


*     calculations for features - aggregated STWs (42) -------------------------
      subroutine turn on diffuse pollution from aggregated STWs
      include 'COM.FOR'

      KEPOL42 = JQ(KFEAT)
      if ( DISTP .lt. 1.0E-08 ) return

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! aggregated STWs (42)

      if ( FLOW(1) .gt. 1.0E9 ) then
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .9.'/
     &'*** Calculations proceeding ...'/100('*'))
      endif

      IQ = KEPOL42

      if ( IQ .eq.  0 ) return

      if ( pollution data(IQ,1,1)+0.001 )42,42,51
   42 if ( nobigout .le. 0 ) write(01,12)
   12 format(
     &'---------------------------------------------------------'/
     &'Quality data specified for aggregated STWs do not exist'/
     &'---------------------------------------------------------')
      return
   51 continue

      return
      end


*     switch off for diffuse pollution from aggregated STWs --------------------
      subroutine turn off diffuse pollution from aggregated STWs
      include 'COM.FOR'

      KEPOL42 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) write(01,1009)
      if ( nobigout .le. 0 ) write(21,1009)
	endif
 1009 format(77('=')/
     &'End of aggregated STWs (discharge type - 43) ... '/
     &77('='))
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of aggregated STWs ...'/77('-'))

*     call get summaries and write out loads and quality (0)
  970 continue

      if (FLOW(1) .lt. 1.0E9) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow more than a billion ',35x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .6.'/
     &'*** Calculations proceeding ...'/100('*'))
      end


*     diffuse sources of pollution (13) (25) (27) etc --------------------------
      subroutine STREAM DIFFUSE
      include 'COM.FOR'
      character *10 BB1(MP10)
      character * 10 Tenchars(MP10) 
      
      call initialise data for mass balance ! stream diffuse
      if ( DINT .lt. 1.0E-8 ) return ! check for xero distance -----------------

      IFDIST = PDRF(IF) ! set the distribution ---------------------------------
      EFM = F(IF,1) !  mean diffuse flow added to river ------------------------
      EF5 = F(IF,2) ! corresponding 95-percentile low flow ---------------------
      EF3=0.0 ! shift flow -----------------------------------------------------
      if ( PDRF(IF) .eq. 3 ) EF3=F(IF,3) ! shift flow --------------------------

      if ( EF5 .gt. 1.0e-08 .and. EF5 .ge. EFM ) then
      suppress10 = suppress10 + 1
      call sort format 2 (EFM,EF5)
      write(01,8151)valchars11,valchars10,IF
      write(33,8151)valchars11,valchars10,IF
 8151 format(33x,77('-')/33x,'*** Illegal data for river flow ... ',
     &'the 95-percentile low flow of ',a10/33x,
     &'*** exceeds the mean of ',a10,3x,'Check the set of flow ',
     &'data number',i6)
	write(01,8152)
	write(33,8152)
 8152 format(33x,'*** The 95-percentile low flow was set to 25% of ',
     &'the mean ... '/33x,77('-'))
      if ( kerror .eq. 1 ) then
 	call change colour of text (37) ! dull turquoise
	write( *,8157)uname(Kfeet),IF
 8157 format('* Zero 95-percentile flow set to 25% of ',
     &'the mean',3x,'...',7x,'at ',a37,4x,'Set number',i7,' .......')
      call set screen text colour
      endif
      F(IF,2) = 0.25 * F(IF,1) 
      EF5 = 0.25 * EFM
      endif
      
      if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08 .and. 
     &     IFDIST .ne. 4 ) then
      call set format for printout (BB1(1),EFM)      
      write(01,8951)BB1(1),FUNIT,IF,IFDIST
 8951 format(33x,77('-')/33x,
     &'*** Zero entered for the 95-percentile low flow ... ',
     &4x,'Mean =',a10,1x,a4/33x,
     &'*** Check the set of flow data number',2i7)
      write(33,8951)BB1(1),IF,IFDIST
	write(01,8952)
 8952 format(33x,77('-')/33x,
     ^'*** Zero 95-percentile low flow set to 1% of ',
     &'the mean ... '/33x,77('-'))
	write(33,8952)
      suppress6 = suppress6 + 1
      if ( suppress6 .lt. 4 .and. kerror .eq. 1 ) then
 	call change colour of text (37) ! dull turquoise
	write( *,8956)uname(Kfeet),IF
 8956 format('* Zero 95-percentile flow set to 1%',
     &' of the mean',4x,'...',7x,'at or d/s of ',a28,3x,
     &'Set number',i7,' .......')
      call set screen text colour
      endif
      if ( suppress6 .eq. 4 .and. kerror .eq. 1 ) then
 	call change colour of text (37) ! dull turquoise
	write( *,8156)uname(Kfeet),IF
 8156 format('* Zero 95-percentile flow set to 1%',
     &' of the mean',4x,'...',7x,'at or d/s of ',a28,3x,'Set number',i7,
     &' and elsewhere ...')
      call set screen text colour
      endif
      F(IF,2) = 0.01 * F(IF,1) 
      EF5 = 0.01 * EFM
      endif
      
*     log-normal distributions of diffuse flow ---------------------------------
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( EFM .ge. 1.0E-10 ) then    
	if ( PDRF(IF) .eq. 2 .or. PDRF(IF) .eq. 3 ) then
      TEST = AMAX1 ( (EF5+EF3), 1.0E-8 )
      GEFS = 2.7057 + 2.0 * ALOG((EFM+EF3)/TEST)
      if ( GEFS .gt. 0.0 ) goto 48
   49 continue
      write(01,9099)IF,EFM,EF5
 9099 format(' ### Flow error for diffuse inflow ... '/
     &' ### Simulation stopped in STREAM DIFFUSE ... ',i6,2f12.5)
      write( *,9099)IF,EFM,EFS
      write(09,9099)IF,EFM,EFS
      write(33,9099)IF,EFM,EFS
      call stop
   48 continue
      GEFS = SQRoot(1026,GEFS)-1.6449
      EFS = EXP(GEFS*GEFS)-1.0
      if (EFS .lt. 1.0E-10) goto 49
      EFS = EFM*SQRoot(1027,EFS)
      GEFM = ALOG(EFM+EF3) - 0.5*GEFS*GEFS
      endif
      endif

*     normal distribution of diffuse flow --------------------------------------
      if ( PDRF(IF) .eq. 1 ) then
      GEFM = EFM
	GEFS = (EFM-EF5)/1.6449
      EFS=GEFS
      endif ! normal distribution of diffuse flow

*     constant diffuse flow ----------------------------------------------------
      if ( PDRF(IF) .eq. 0 ) then
      GEFM = EFM
      GEFS = 0.0
      EFS=GEFS
      endif ! constant diffuse flow

*     non-parametric distributions of diffuse flow -----------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     write(33,3211)
 3211 format(
     &33x,'A non-parametric distribution of diffuse inflow is used')
	endif ! non-parametric distributions of diffuse flow

*     monthly distributions of river flow --------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     write(33,3231)
 3231 format(33x,'Monthly data on diffuse inflow are used ...')
	endif

*     monthly structure for river flow -----------------------------------------
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     write(33,3631)
 3631 format(33x,'The monthly structure on diffuse inflow was ',
     &'used ...')
      endif

*     loop on the determinands -------------------------------------------------
      idfcheck = 0
      jhead = 0
      do 6957 JP = 1, ndet

      call inititialise the stores for the estimates of load 

      if (QTYPE (JP) .EQ. 4) goto 6957
      idfcheck = idfcheck + 1

      IQDIST = PDRC(IQ,JP) ! type of distribution
      ECM = quolity data(IQ,JP,1) ! mean
      ECS = quolity data(IQ,JP,2) ! standard deviation
      EC3 = 0.0 ! shift
      if ( PDRC (IQ,JP) .EQ.3 ) EC3 = quolity data(IQ,JP,3) ! shift

      CO5 = RFCL(JP) ! default for added flow on added quality -----------------
      if ( quolity data (IQ,JP,4) .ne. -9.9 ) then
	CO5 = quolity data (IQ,JP,4) ! added flow on added quality ---------------
      endif ! if ( quolity data (IQ,JP,4) .ne. -9.9 ) --------------------------
      
      CO2 = 1.0 ! default correlation of added flow the main river flow --------
	Ctemp = CO2 ! save this for future use after "mass balance" --------------
*     check for a special correlation coefficient with main river flow ---------
      if ( F (IF,MO) .ne. -9.9 ) CO2 = F (IF,MO)

*     do the mass balance for this determinand ---------------------------------
*     set the "ifdiffuse" to "1" for diffuse pollution -------------------------
      ifdiffuse = 1 ! river type
      call mass balance ! from subroutine stream diffuse
      call accumulate the loads
      ifdiffuse = 0
	CO2 = Ctemp ! restore original value of correlation coefficient ----------

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      xnum = QNUM(IQ,JP)
      if ( JSKIP .EQ. 0 ) then
      call get sampling rates for river quality ! stream diffuse ---------------
     &(FLOW(1),C(JP,1),QUALN(JP),EFM,ECM,xnum)
      endif

 6957 continue

      call accumulate total loads from diffuse sources
	call add up all the loads
*     calculate loads and the summary statistics of load -----------------------
      call load calculation

*     write out the river loads after diffuse sources --------------------------
*     if ( ical13 .eq. 0 )  then
*     call write out the river loads after diffuse sources ---------------------
*     endif
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then 
      xprint = 0.0
      do idet = 1, ndet
	BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)diffuse type,(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse run-off',i3,8X,10A10)
*     endif
      endif
      endif

      do jdet = 1, ndet
	check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
	enddo

      call format the loads for output 4 (Tenchars) 
      
      return
      end



*     compute effect of diffuse pollution --------------------------------------
*     Feature type (15/16) ... discharge type ----------------------------------
      subroutine effluent diffuse
      include 'COM.FOR'
      character *10 BB1(MP10)
	character *10 Tenchars (13)

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! effluent diffuse 15
      if (DINT .lt. 1.0E-8) return

      IQ = KEPOL15 ! data-set --------------------------------------------------
      if ( IQ .lt. 1 ) return

*     check for zero mean flow -------------------------------------------------
*     if ( FE(IQ,1) .lt. 1.0E-8 ) return

*     prepare for mass balance -------------------------------------------------
      IFDIST = PDEF(IQ)

*     flow data ----------------------------------------------------------------
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
      EF3 = 0.0
      if ( PDEF(IQ) .EQ. 3 ) EF3=FE(IQ,3)

*     correlation of discharge flow on river flow ------------------------------
      CO2 = 0.6
*     special non-default correlation ------------------------------------------
      if ( FE (IQ,4) .ne. -9.9 ) CO2 = FE (IQ,4)

*     ==========================================================================
*     non-parametric distributions of discharge flow ---------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 4 ) then
*     write(01,3211)
*     write(09,3211)
*     write(33,3211)
*3211 format(
*    &33x,'A non-parametric distribution of diffuse inflow is used')
*     endif
*     ==========================================================================

*     ==========================================================================
*     Monthly distributions of discharge flow ----------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 5 ) then
*     write(01,3231)
*     write(09,3231)
*     write(33,3231)
*3231 format(33x,'Monthly data on diffuse inflow are used ...')
*	endif
*     ==========================================================================

*     ==========================================================================
*     monthly structure for discharge flow -------------------------------------
*     --------------------------------------------------------------------------
*     if ( PDEF(IQ) .eq. 8 ) then ! monthly structure
*     if ( nobigout .le. 0 ) write(01,3631)
*     write(09,3631)
*     write(33,3631)
*3631 format(33x,'The monthly structure on diffuse inflow was ',
*    &'used ...')
*	endif
*     ==========================================================================

*     loop on determinands -----------------------------------------------------
      MFIRST=0
      jhead = 0

      do 6930 JP = 1, ndet
      if ( qtype (JP) .eq.4 ) goto 6930

      IQDIST = PDEC(IQ,JP) ! type of distribution

*     correlation coefficient for discharge flows on river flows ---------------
*     as usual, coefficient refers to the logged variables ---------------------
	Ctemp = CO2

      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)

      call inititialise the stores for the estimates of load 

*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
*     discharge quality on discharge flow --------------------------------------
      CO5 = EFCL (JP)
*     special correlation (check this) -----------------------------------------
      if ( pollution data (IQ,JP,MO) .ne. -9.9 ) then
	CO5 = pollution data (IQ,JP,MO)
      endif

*     mass balance -------------------------------------------------------------
      ifdiffuse = 2 ! effluent type
      call mass balance ! from effluent diffuse
      call accumulate the loads
      ifdiffuse = 0

	CO2 = CTemp

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      xnum = pnum(iq,jp)
      if ( JSKIP .EQ. 0 ) then
      call get sampling rates for river quality ! effluent diffuse -------------
     &(FLOW(1),C(JP,1),QUALN(JP),EFM,ECM,xnum)
      endif
 6930 continue

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
*     accumulate total loads from diffuse sources ------------------------------
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t15load1(idet,J1) = t15load1(idet,J1) + diffuse load (idet,J1)
      t15load2(idet,J1) = t15load2(idet,J1) + diffuse load (idet,J1)
      endif
	BB1(idet) = '      ....'
	enddo
      enddo

	call add up all the loads

*     calculate the effect of losses on the net accumulated loads --------------
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      prune load (J1) = 1.0
      if ( diffuse load (idet,J1) .lt. 0.0 ) then
      if ( abs ( TGLODE2(idet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(idet,J1)  - diffuse load (idet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(idet,J1) - diffuse load (idet,J1)) 
     &                / TGLODE2(idet,J1)
      endif
	endif
      endif
	enddo

*     trim back the loads for losses through diffuse abstractions -------------- 
      call scale loads after losses
      endif
      enddo

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      xprint = 0.0
      do idet = 1, ndet
	BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse discharge (15)',4X,10A10)
*     endif
      endif
      endif

      do jdet = 1, ndet
	check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
	enddo

      call format the loads for output 4 (Tenchars) 
*     if ( nobigout .le. 0 ) then
*     if ( ical .gt. 3 ) then
*     write(27,4)(Tenchars(jdet),jdet=1,MP10) 
*   4 format('Be   ','Diffuse running total',19x,5x,10A10)
*	endif
*	endif
      
      return
      end


*     compute effect of diffuse pollution --------------------------------------
*     feature type (42/43) ... discharge type ----------------------------------
      subroutine effluent diffuse 42
      include 'COM.FOR'
      character *10 BB1(MP10)
	character *10 Tenchars (13)

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! effluent diffuse 42
      if (DINT .lt. 1.0E-8) return

*     data-set -----------------------------------------------------------------
      IQ = KEPOL42
      if ( IQ .lt. 1 ) return

*     check for zero mean flow -------------------------------------------------
*     if ( FE(IQ,1) .lt. 1.0E-8 ) return

*     prepare for mass balance -------------------------------------------------
      IFDIST = PDEF(IQ)

*     flow data ----------------------------------------------------------------
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
      EF3 = 0.0
      if ( PDEF(IQ) .EQ. 3 ) EF3 = FE(IQ,3)

*     correlation of discharge flow on river flow ------------------------------
      CO2 = 0.6
*     special non-default correlation ------------------------------------------
      if ( FE (IQ,4) .ne. -9.9 ) CO2 = FE (IQ,4)

*     ==========================================================================
*     non-parametric distributions of discharge flow ---------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 4 ) then
*     write(01,3211)
*     write(09,3211)
*     write(33,3211)
*3211 format(
*    &33x,'A non-parametric distribution of diffuse inflow is used')
*	endif
*     ==========================================================================

*     ==========================================================================
*     Monthly distributions of discharge flow ----------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 5 ) then
*     write(01,3231)
*     write(09,3231)
*     write(33,3231)
*3231 format(33x,'Monthly data on diffuse inflow are used ...')
*	endif
*     ==========================================================================

*     ==========================================================================
*     monthly structure for discharge flow -------------------------------------
*     --------------------------------------------------------------------------
*     if ( PDEF(IQ) .eq. 8 ) then ! monthly structure
*     if ( nobigout .le. 0 ) write(01,3631)
*     write(09,3631)
*     write(33,3631)
*3631 format(33x,'The monthly structure on diffuse inflow was ',
*    &'used ...')
*	endif
*     ==========================================================================

*     loop on determinands -----------------------------------------------------
      MFIRST=0
      jhead = 0

      DO 6930 JP = 1, ndet
	if ( qtype (JP) .eq.4 ) goto 6930

      IQDIST = PDEC(IQ,JP) ! type of distribution

*     correlation coefficient for discharge flows on river flows ---------------
*     as usual, coefficient refers to the logged variables ---------------------
	Ctemp = CO2

      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)

      call inititialise the stores for the estimates of load 

*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
      CO5 = EFCL (JP) ! discharge quality on discharge flow
*     special correlation ------------------------------------------------------
      if ( pollution data (IQ,JP,MO) .ne. -9.9 ) then
	CO5 = pollution data (IQ,JP,MO)
      endif

*     mass balance -------------------------------------------------------------
      ifdiffuse = 2 ! effluent type
      call mass balance ! from effluent discharge 42
      call accumulate the loads
      ifdiffuse = 0

	CO2 = CTemp
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      xnum = pnum(iq,jp)
      if ( JSKIP .EQ. 0 ) then
      call get sampling rates for river quality ! effluent diffuse 42 ----------
     &(FLOW(1),C(JP,1),QUALN(JP),EFM,ECM,xnum)
      endif
 6930 continue

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
*     accumulate total loads from diffuse sources ------------------------------

      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t42load1(idet,J1) = t42load1(idet,J1) + diffuse load (idet,J1)
      t42load2(idet,J1) = t42load2(idet,J1) + diffuse load (idet,J1)
      endif
	BB1(idet) = '      ....'
	enddo
      enddo

	call add up all the loads

*     calculate the effect losses on accumulated loads -------------------------
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      prune load (J1) = 1.0
      if ( diffuse load (idet,J1) .lt. 0.0 ) then
      if ( abs ( TGLODE2(idet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(idet,J1)  - diffuse load (idet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(idet,J1) - diffuse load (idet,J1)) 
     &              / TGLODE2(idet,J1)
      endif
	endif
      endif
	enddo

*     trim back the loads for losses through diffuse abstractions -------------- 
      call scale loads after losses
      endif
      enddo

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      xprint = 0.0
      do idet = 1, ndet
	BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse discharge (15)',4X,10A10)
*     endif
      endif
      endif
 
      do jdet = 1, ndet
	check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
	enddo

      call format the loads for output 4 (Tenchars) 
*     if ( nobigout .le. 0 ) then
*     if ( ical .gt. 3 ) then
*	write(27,4)uname(feeture),(Tenchars(jdet),jdet=1,MP10)
*   4 format('0    ',a40,5x,10A10)
*	endif
*	endif
      
      return
      end


*     add in the contribution to reaches from diffuse sources ------------------
      subroutine REACH DIFFUSE
      include 'COM.FOR'
      dimension Yflow(MS),CMT(MP10,MS),dload(mp10),RLC(nprop)
 
      call initialise data for mass balance
      
      call calculate summaries of river flow
      call get summaries of river quality from the shots

*     write out the river loads before the diffuse sources ---------------------
      if ( DINT .lt. 1.0E-4 ) then
      if ( ical13 .eq. 0 )  then
      call write out the river loads before diffuse sources
	endif
 	return     
      endif

*     calculate starting loads and the summary statistics of load --------------
      call load calculation

*     store the starting loads -------------------------------------------------
      do idet = 1, ndet
      dload (idet) = xload (idet,1,i13)
      enddo

*     set the added loads ------------------------------------------------------
      do idet = 1, ndet
      addid load (idet) = 0.0
      enddo

*     pick out the flow data-set for the diffuse inflow ------------------------
      IF = RFDIFF (IREACH)
      if ( IF  .lt.  1 ) return

*     store the current data on flow and quality -------------------------------
      do IS = 1,NS
      Yflow (IS) = FMS (IS)
*     FMS (IS) = 0.0
      do idet = 1, NDET
      CMT (idet,IS) = CMS (idet,IS)
*     CMS (idet,IS) = 0.0
      enddo
      enddo

      if ( PDRF(IF) .eq. -1) then
      if ( ifbatch .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,2192)RNAME(IREACH),IF
 2192 format('* No flow data for ',
     &'diffuse inflow to reach ... ',3x,a16,
     &' ... the set number is ',i6)
      call set screen text colour
      endif
      write(09,2311)RNAME(IREACH),IF
      write(33,2311)RNAME(IREACH),IF
      write(01,2311)RNAME(IREACH),IF
 2311 format(77('-')/
     &'There are no data on flow ',
     &'for the reach-based diffuse inflow for ... ',a16/
     &'The set number is ...',i6/77('-'))
      RFDIFF (IREACH) = 0
      return
      endif

*     quality data-set ---------------------------------------------------------
      IQ = RQDIFF (IREACH)
      if ( IQ .lt. 1 ) return
      if ( PDRC(IQ,1) .eq. -1) then
      if ( KSIM .eq. 0 .or. KSIM .eq. 2 ) then
      suppress00 = suppress00 + 1 ! no data on water quality
	call change colour of text (20) ! bright red
      write( *,2392)RNAME(IREACH),IQ
 2392 format('* No data on water quality for ',
     &'diffuse inflow to reach ... ',3x,a16,
     &' ... the set number is ',i6)
      call set screen text colour
      write(09,2312)RNAME(IREACH),IQ
      write(01,2312)RNAME(IREACH),IQ
      write(33,2312)RNAME(IREACH),IQ
 2312 format(77('-')/
     &'There are no data on water quality ... '/
     &'for the reach-based diffuse inflow for ... ',a16/
     &'The set number is ...',i6/77('-'))
      RQDIFF (IREACH) = 0
      do idet = 1, ndet
      if ( PDRC(IQ,idet) .lt. 0 ) PDRC(IQ,idet) = 0
      quolity data(IQ,idet,1) = 0.0
      quolity data(IQ,idet,2) = 0.0
      enddo
      endif
      endif

*     mean flow ----------------------------------------------------------------
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
	write(01,5282)dint
 5282 format(33x,77('=')/33x,'Various diffuse inflows over the next',
     &f8.2,' kilometres ...'/33x,77('='))
	diffuse heading = 1
      write(01,4522) IF
      write(34,4522) IF
 4522 format(33x,77('=')/33x
     &'Insert the (reach-based) diffuse inflows ... Flow data set:',i5)
      iprime = 1
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3482)valchars11,valchars10,funit
      write(34,3482)valchars11,valchars10,funit
 3482 format(33x,77('-')/
     &33x,'Starting river flow',16x,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)

      do J = 1, ndet
	if ( qtype (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
 1422 format(33x,'Starting ',A11,15x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
	endif
      enddo

      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
	write(34,2082)dint,valchars11,valchars10,funit
 2082 format(33x,77('-')/33x,
     &'Total flow ADDED over',f6.1,' km',5X,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4/33x,77('-'))
      endif ! if ( nobigout .le. 0 )
	endif ! if ( ical13 .eq. 0 )
	endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

      call generate river flow ! for diffuse inputs

      if ( PDRF(IF) .eq. 4 ) then ! non parametric distribution

*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 1970
 1969 continue
	goto 1974
 1970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,7163) flnamesmall(1,icod)
 7163 format(33x,
     &'Non-parametric flow distribution ... File: ',a64/33x,77('-'))
      endif
      endif
 1974 continue

*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
	goto 2974
 2970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
 7363 format(33x,'Monthly distributions ... File: ',a64)
      endif
      endif
 2974 continue

*     note whether the data are monthly structure ------------------------------
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
      do 3969 i = 1, M8
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3970
 3969 continue
	goto 3974
 3970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,3966) FLSTRUCTsmall(1,icod)
 3966 format(33x,
     &'Monthly structure file (type 8) ... File: ',a64)
      endif
      endif ! if ( PDRF(IF) .eq. 8 )
 3974 continue

      if ( MONF .gt. 1 ) call write shots for diffuse inflows

*     inititialise the stores for the estimates of load ------------------------
      do JP = 1, ndet
      do L13 = 1, N13
	NSM (JP,L13) = 0
      ELOAD (JP,L13) = 0.0
	enddo
	enddo

*     generate the shots of diffuse river quality ------------------------------
      do 23 jp = 1, ndet
      if ( Qtype (jp) .eq. 4 ) goto 23
      call generate river quality  ! diffuse river quality

      IQDIST = PDRC(IQ,JP) ! type of distribution

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( ical13 .eq. 0 ) then
      if ( jp .eq. ndetlast ) then
	if ( nobigout .le. 0 ) write(01,2094)IQ
 2094 format(33x,'Quality of diffuse inflow: ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
      endif
	endif
	endif

*     note whether the river quality distribution is non-parametric ------------
      if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then ! non-parametric ------------
*     identify the file with the non-parametric data ---------------------------
      do 5969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 5970
 5969 continue
	goto 5974

 5970 continue
      if ( nobigout .le. 0 ) write(01,2965) flnamesmall(2,icod)
 2965 format(33x,'Non-parametric distribution ... File: ',a64)
      endif
 5974 continue

*     note whether the river quality data are monthly --------------------------
      if ( IQDIST .eq. 5 ) then ! monthly data ---------------------------------
*     identify the file with the monthly data ----------------------------------
      do 4969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, JP+1 ) .eq. IQ ) goto 4970
 4969 continue
	goto 4974
 4970 continue
      if ( nobigout .le. 0 ) write(01,4363) FLMONTHsmall(2,icod)
 4363 format(33x,'Monthly data ... File: ',a64)
      endif ! if ( IQDIST .eq. 5 ) monthly data --------------------------------
 4974 continue

      if ( MONQ .gt. 1) call write shots for quality of diffuse inflows
   23 continue
      
      do 2 IS = 1, NS
	imonth = qmonth (is) ! set the month for this shot

*     flow added over this stretch ---------------------------------------------
*     unit flow multiplied by length of river ----------------------------------
      FDF = FMS(IS) * DINT
*     calculate new flow at end of stretch -------------------------------------
      FMS(IS) = AMAX1 ( 1.0E-09, FDF + Yflow(IS) )

*     ignore negative flows ----------------------------------------------------
      if ( FMS(IS) .lt. 1.0E-09 ) then
	write( *,*) "negative flows in REACH DIFFUSE ..."
	goto 2
      endif

      do idet = 1, ndet
      if ( QTYPE(idet) .ne. 4 ) then

*     contributions from different sources of pollution ------------------------
	do ip = 1, nprop
      RLC(ip) = LMS(ip, idet, IS )
	enddo

      IQDIST = PDRC(IQ,idet) ! type of distribution
*     quality of the added inflow ----------------------------------------------
      QDF = CMS(idet,IS)
*     deal with addition of load but not flow ----------------------------------
*     initialise the flow added from diffuse inflow ----------------------------
	EFMB = FDF

*     check for the data expressed as load -------------------------------------        
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then ! load
      QDF = QDF * DINT
      EFMB = 1.0
      endif     

	if ( FMS(IS) .gt. 1.0e-10 ) then
      CMS(idet,IS)=((CMT(idet,IS)*Yflow(IS))+(QDF*EFMB))/FMS(IS)
      do ip = 1, nprop
      if ( ip .eq. 20 ) then
      LMS(20,idet,IS)=((RLC(20)*Yflow(IS))+(QDF*EFMB))/FMS(IS) ! reach diffuse
      else
      LMS(ip,idet,IS) = RLC(ip) * Yflow(IS) / FMS(IS) ! reach diffuse
      endif
	enddo ! do ip = 1, nprop
      endif ! if ( FMS(IS) .gt. 1.0e-10 )

*     and accumulate the load --------------------------------------------------
      K13 = imonth + 1
	XLD = QDF * EFMB
      ELOAD (idet,I13) = ELOAD (idet,I13) + XLD
      ELOAD (idet,K13) = ELOAD (idet,K13) + XLD
	NSM (idet,I13) = NSM (idet,I13) + 1
	NSM (idet,K13) = NSM (idet,K13) + 1

      addid load (idet) = addid load (idet) + XLD

      endif ! if ( QTYPE(idet) .ne. 4 )
      enddo ! do idet = 1, ndet

    2 continue
      
      do idet = 1, ndet
	if ( qtype (idet) .ne. 4 ) then
          
      do ip = 1, nprop
      do IS = 1, NS
      Yflow(IS) = LMS(ip,idet,IS)
      enddo
      call calculate summaries of contribution (ip, idet, Yflow)
      do IS = 1, NS
      Yflow(IS) = LMS(ip,idet,IS)
      enddo
      call calculate monthly summaries of contribution (ip, idet, Yflow)
      enddo

      addid load (idet) = addid load (idet) / float (NS)
      ELOAD (idet,I13) = ELOAD (idet,I13) / float (NS)
      TDDLOAD1 (idet,I13) = TDDLOAD1 (idet,I13) + eload (idet,I13) 
      TDDLOAD2 (idet,I13) = TDDLOAD2 (idet,I13) + eload (idet,I13) 

      if ( munthly structure .eq. 1 ) then ! fill monthly loads
      do J13 = 2, N13
	if ( NSM(idet,J13) .gt. 0 ) then
      ELOAD (idet,J13) = ELOAD (idet,J13) / float (NSM(idet,J13))
      TDDLOAD1 (idet,J13) = TDDLOAD1 (idet,J13) + eload (idet,J13) 
      TDDLOAD2 (idet,J13) = TDDLOAD2 (idet,J13) + eload (idet,J13) 
      endif
	enddo
	endif ! fill monthly loads
	endif ! if ( qtype (idet) .ne. 4 )
	enddo ! idet = 1, ndet

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      do idet = 1, ndet
      if ( JSKIP .eq. 0 ) then
      if ( QTYPE (idet) .ne. 4 ) then
      xnum = qnum(iq,idet)
      call get sampling rates for river quality ! reach diffuse ----------------
     &(FLOW(1),C(idet,1),QUALN(idet),DINT*F(IF,1),
     &quolity data(IQ,idet,1),xnum)
      endif
      endif
	enddo

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
 3082 format(33x,77('-')/
     &33x,'Resulting river flow',15x,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)
      do J = 1, Ndet
	if ( qtype (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
	write(01,1022)DNAME(J),valchars11,valchars10,UNITS(J)
	write(34,1022)DNAME(J),valchars11,valchars10,UNITS(J)
 1022 format(33x,'Resulting ',A11,14x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
	endif
      enddo
      write(01,6666)
      write(34,6666)
 6666 format(33x,77('-'))
	endif
	endif

*     calculate loads and the summary statistics of load -----------------------
      call load calculation
	call add up all the loads

*     write out the river loads after diffuse sources --------------------------
      if ( ical .eq. 0 .or. ical .gt .3 )  then
      call write out the river loads after diffuse sources
      endif

      return
      end



      subroutine write message that diffuse discharge 
     &pollution has started
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      if ( ical .ne. 2 .and. nobigout .le. 0 ) write(21,1009)
 1009 format(77('=')/
     &'Start of diffuse pollution (discharge type - 15)'/77('='))
	endif
	endif

      return
      end


      subroutine check for an infeasibly large river flow
      include 'COM.FOR'

      if (FLOW(1) .gt. 1.0E9) then
      if ( nobigout .le. 0 ) write(01,7817)UNAME(KFEAT)
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &a40/'*** It looks like an error has occurred .6.'/
     &'*** Calculations proceeding ....')
      endif
	return
	end


      subroutine accumulate total loads from diffuse sources
      include 'COM.FOR'

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      if ( diffuse type .eq. 13 )  then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t13load1(idet,J1) = t13load1(idet,J1) + diffuse load (idet,J1)
      t13load2(idet,J1) = t13load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 25 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      T25LOAD1(idet,J1) = T25LOAD1(idet,J1) + diffuse load (idet,J1)
      T25LOAD2(idet,J1) = T25LOAD2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 27 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t27load1(idet,J1) = t27load1(idet,J1) + diffuse load (idet,J1)
      t27load2(idet,J1) = t27load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 29 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t29load1(idet,J1) = t29load1(idet,J1) + diffuse load (idet,J1)
      t29load2(idet,J1) = t29load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 31 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t31load1(idet,J1) = t31load1(idet,J1) + diffuse load (idet,J1)
      t31load2(idet,J1) = t31load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 33 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t33load1(idet,J1) = t33load1(idet,J1) + diffuse load (idet,J1)
      t33load2(idet,J1) = t33load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 35 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t35load1(idet,J1) = t35load1(idet,J1) + diffuse load (idet,J1)
      t35load2(idet,J1) = t35load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 46 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t46load1(idet,J1) = t46load1(idet,J1) + diffuse load (idet,J1)
      t46load2(idet,J1) = t46load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 48 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t48load1(idet,J1) = t48load1(idet,J1) + diffuse load (idet,J1)
      t48load2(idet,J1) = t48load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif

      if ( diffuse type .eq. 37 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t37load1(idet,J1) = t37load1(idet,J1) + diffuse load (idet,J1)
      t37load2(idet,J1) = t37load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
      if ( diffuse type .eq. 40 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t40load1(idet,J1) = t40load1(idet,J1) + diffuse load (idet,J1)
      t40load2(idet,J1) = t40load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
      if ( diffuse type .eq. 42 ) then
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      t42load1(idet,J1) = t42load1(idet,J1) + diffuse load (idet,J1)
      t42load2(idet,J1) = t42load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
	endif
      enddo

	return
	end
