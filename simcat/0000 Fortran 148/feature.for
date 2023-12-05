*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     --------------------------------------------------------------------------
*     File FEATURE.FOR (4577 lines)
*     --------------------------------------------------------------------------
*     Feature Suite: Sub-routines for Features ...
*     --------------------------------------------------------------------------

      subroutine process the feature (ITYPE)
      include 'COM.FOR'
 
      cut off zero flow = 0.0 
      do idet = 1, ndet
      cut off zero quality (idet) = 0.0 
      enddo
      mark works = 0
	diffuse heading = 0
      
*     initialise the correlation coefficients ----------------------------------
      CO1 = 0.0 ! initialise river flow on river quality -----------------------
      CO2 = 0.0 ! initialise river flow on discharge flow ----------------------
      CO3 = 0.0 ! initialise river flow on discharge quality ! 44444444444444444
      CO4 = 0.0 ! initialise river quality on discharge flow ! 44444444444444444
      CO5 = 0.0 ! initialise discharge (or tributary) flow on discharge quality-
      CO6 = 0.0 ! initialise river quality on discharge quality ! 44444444444444

*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(KFEAT)
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,JP) ! set target for feature -
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass - 1
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5) then
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif
      endif ! if ( JP .eq. 4 )
      enddo
      endif ! if ( IQSreach .gt. 0 )
      RQO(JP) = targit ! use the target for graphs -----------------------------
	MRQO(JP) = MRQS(JP)
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET

*     illegal features ---------------------------------------------------------
      if ( itype .gt. 49 ) then
	if ( nobigout .le. 0 ) write(01,98)ITYPE,UNAME(feeture)
      if ( iscreen .lt. 3 ) write( *,98)ITYPE,UNAME(feeture)
   98 format('*** Illegal Type of Feature (Type =',I3,') ',A40)
      endif

*     monitoring point ----------------------------------------------------------
      if ( itype .eq. 1) then 
      if ( JSKIP .eq. 0 ) call monitoring point
      return
	endif

*     tributary or stream (too small to be a REACH) ----------------------------
      if ( itype .eq. 2) then 
      call tributary
      return
	endif

*     effluent discharges ------------------------------------------------------
*     apply to (3) (5) (12) (39) ----------------------- call effluent discharge
      if ( itype .eq. 3 .or. itype .eq. 5 .or. itype .eq. 12 
     &.or. itype .eq. 39 ) then 
	discharge type = itype
      call effluent discharge
	discharge type = 0
      return
	endif

      if ( itype .eq. 4 ) then ! flow gauge
      if ( JSKIP .eq. 1 ) return
      call river flow gauge
      call get summaries of river quality from the shots
      call get summaries of loads
      return
      endif

      if ( itype .eq. 6 ) then ! interpolation point
      if ( JSKIP .eq. 0 ) call interpolation point
      return
	endif

      if ( itype .eq. 44 ) then ! flow into a lake
      call flow into lake
      return
	endif

      if ( itype .eq. 45 ) then ! flow from a lake
      call flow from lake
      return
	endif

      if ( itype .eq. 24 ) then ! this is a sub-catchment boundary
      call catchment boundary
      return
	endif

*     abstraction (7) ----------------------------------------------------------
      if ( itype .eq. 7 ) then 
      call abstraction of flow
      call get summaries of river quality from the shots
      call get summaries of loads
      return
	endif

*     abstraction (the negative discharge - tributary type - 18) ---------------
      if ( itype .eq. 18 ) then 
      call negative discharge one ! abstraction 18
      call get summaries of river quality from the shots
      call get summaries of loads
      return
	endif ! abstraction (the negative discharge - tributary type)

*     abstraction (the negative discharge - effluent type - 19 ) ---------------
      if ( itype .eq. 19 ) then ! abstraction 19 
      call negative discharge two ! abstraction 19
      call get summaries of river quality from the shots
      call get summaries of loads
      return
	endif ! abstraction (the negative discharge - effluent type)

*     weir ---------------------------------------------------------------------
      if ( itype .eq. 8 ) then 
      call weir
      call get summaries of river quality from the shots
      call get summaries of loads
      return
	endif

*     river regulation point ---------------------------------------------------
      if ( itype .eq. 9 ) then 
      call river regulation
      call get summaries of river quality from the shots
      call get summaries of loads
      return
      endif

*     not used here - upstream boundary ----------------------------------------
      if ( itype .eq. 10 ) return
*     bifurcations -------------------------------------------------------------
      if ( itype .eq. 11 ) return
*     abstraction of river flow distribution -----------------------------------
*     first branch -------------------------------------------------------------
      if ( itype .eq. 20 ) return
*     returned abstraction of river flow distribution --------------------------
*     second branch ------------------------------------------------------------
      if ( itype .eq. 21 ) return 
*     abstraction of effluent flow distribution --------------------------------
*     first branch ...
      if ( itype .eq. 22 ) return 
*     returned abstraction of effluent flow distribution -----------------------
*     second branch ------------------------------------------------------------
      if ( itype .eq. 23 ) return 

*     ==========================================================================
*     switch on diffuse pollution (river type) ---------------------------------
      if ( itype .eq. 13 ) then 
      call turn on river based diffuse pollution
      return
	endif
*     switch off diffuse pollution ---------------------------------------------
      if ( itype .eq. 14 ) then 
      call turn off river based diffuse pollution
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from agricultural livestock (river type) -----
      if ( itype .eq. 25 ) then 
      call turn on river based diffuse pollution from livestock
      return
	endif
*     switch off Diffuse Pollution from agricultural livestock (river type) ----
      if ( itype .eq. 26 ) then 
      call turn off river based diffuse pollution from livestock
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from agricultural arable (river type) --------
      if ( itype .eq. 27 ) then 
      call turn on river based diffuse pollution from arable
      return
	endif
*     switch off diffuse pollution from agricultural arable (river type) -------
      if ( itype .eq. 28 ) then 
      call turn off river based diffuse pollution from arable
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from highways --------------------------------
      if ( itype .eq. 29 ) then 
      call turn on river based diffuse pollution from highways
      return
	endif
*     switch off diffuse pollution from highways -------------------------------
      if ( itype .eq. 30 ) then 
      call turn off river based diffuse pollution from highways
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from urban (river type) ----------------------
      if ( itype .eq. 31 ) then 
      call turn on river based diffuse pollution from urban
      return
	endif
*     switch off diffuse pollution from urban (river type) ---------------------
      if ( itype .eq. 32 ) then 
      call turn off river based diffuse pollution from urban
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from atmospheric deposition ------------------
      if ( itype .eq. 33 ) then 
      call turn on diffuse pollution from the atmosphere
      return
	endif
*     switch off diffuse pollution from atmospheric deposition -----------------
      if ( itype .eq. 34 ) then 
      call turn off diffuse pollution from the atmosphere
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from background ------------------------------
      if ( itype .eq. 35 ) then 
      call turn on diffuse pollution from background
      return
	endif
*     switch off diffuse pollution from background -----------------------------
      if ( itype .eq. 36 ) then 
      call turn off diffuse pollution from background
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from mines -----------------------------------
      if ( itype .eq. 46 ) then 
      call turn on diffuse pollution from mines
      return
	endif
*     switch off diffuse pollution from mines ----------------------------------
      if ( itype .eq. 47 ) then 
      call turn off diffuse pollution from mines
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from birds, boats and angling ----------------
      if ( itype .eq. 48 ) then 
      call turn on diffuse pollution from birds
      return
	endif
*     switch off diffuse pollution from birds, boats and angling ---------------
      if ( itype .eq. 49 ) then 
      call turn off diffuse pollution from birds
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from septic tanks ----------------------------
      if ( itype .eq. 37 ) then 
      call turn on diffuse pollution from septic tanks
      return
	endif
*     switch off diffuse pollution from septic tanks ---------------------------
      if ( itype .eq. 38 ) then 
      call turn off diffuse pollution from septic tanks
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from aggregated CSOs -------------------------
      if ( itype .eq. 40 ) then 
      call turn on diffuse pollution from aggregated CSOs
      return
	endif
*     switch off diffuse pollution from septic tanks ---------------------------
      if ( itype .eq. 41 ) then 
      call turn off diffuse pollution from aggregated CSOs
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution (discharge type) -----------------------------
      if ( itype .eq. 15 ) then 
      call turn on discharge based diffuse pollution
      return
	endif
*     switch off diffuse pollution ---------------------------------------------
      if ( itype .eq. 16 ) then 
      call turn off discharge based diffuse pollution
      return
	endif
*     ==========================================================================
*     switch on diffuse pollution from aggregated STWs -------------------------
      if ( itype .eq. 42 ) then 
      call turn on diffuse pollution from aggregated STWs
      return
	endif
*     switch on diffuse pollution from aggregated STWs -------------------------
      if ( itype .eq. 43 ) then 
      call turn off diffuse pollution from aggregated STWs
      return
	endif
*     ==========================================================================

*     discharge with zero flow -------------------------------------------------
      if ( itype .eq. 17 ) then 
      call zero discharge flow 17
      return
	endif ! ----------------------------------------- discharge with zero flow

      return
      end




      subroutine monitoring point
      include 'COM.FOR'

      IQ = JQ(KFEAT)
      if ( ICAL .eq. 1 ) return

      if ( IPRINT .eq. 1 ) goto 1
      if ( nobigout .le. 0 )write(01,1015)
      if ( nobigout .le. 0 )write(21,1015)
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(34,1015)
 1015 format(
     &'Calculated river quality at this monitoring point ...'/
     &77('-'))
      call get summaries and write out loads and quality (0) ! monitoring point
	call write out monthly river quality (2) ! at monitoring point
	endif

*     store the results for the summaries --------------------------------------
    1 LMONP = LMONP + 1

      if ( LMONP .le. MM ) goto 2
      if ( nobigout .le. 0 ) write(01,8714)
      write(09,8714)
      write(33,8714)
      write(03,8714)
      write( *,8714)
 8714 format(
     &'------------------------------------------------------'/
     &'*** Too many quality monitoring stations ...'/
     &'*** This one will not appear in the summaries ...'/
     &'------------------------------------------------------')
      LMONP = LMONP - 1
*     indicator that there are too many monitoring stations --------------------
      imm = 1
      return

    2 continue

      do J = 1, ndet
      if ( QTYPE (J) .ne. 4) then
      Qstations(LMONP,J,1) = C(J,1)
      Qstations(LMONP,J,2) = C(J,2)
      Qstations(LMONP,J,3) = C(J,3)
      endif
      enddo

*     add to the list of sampling points ---------------------------------------
      LSAMPOINTS(LMONP) = KFEAT

      if ( IQ .eq. 0 ) return

      call compute the mean and percentiles at a monitoring point
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting
      endif

      return
      end





*     interpolation or plotting point ------------------------------------------
      subroutine interpolation point
      include 'COM.FOR'

      do jp =1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	if ( MONQ .gt. 1 ) call write shots for river flow
	endif
      enddo
      
      if ( IPRINT .ne. 1 ) then
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) then
	write(01,1)
      write(21,1)
    1 format(77('-')/
     &'Calculated river quality at this interpolation point ...'/
     &77('-'))
      call get summaries and write out loads and quality (0) ! at interpolation
	call write out monthly river quality (2) ! at interpolation
	endif
      call write data for graph plotting
	endif ! if ( ical13 .eq. 0 )
      endif

      return
      end


*     flow into lake -----------------------------------------------------------
      subroutine flow into lake
      include 'COM.FOR'

      do jp =1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	if ( MONQ .gt. 1 ) call write shots for river flow
	endif
      enddo

      if ( IPRINT .eq. 1 ) return

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,1)
      write(21,1)
    1 format(77('-')/
     &'Calculated river quality for flow into the lake ...'/77('-'))
      call get summaries and write out loads and quality (0) ! at lake inflow
	call write out monthly river quality (2) ! at lake inflow
	endif
      call write data for graph plotting
	endif

      return
      end


*     flow from lake -----------------------------------------------------------
      subroutine flow from lake
      include 'COM.FOR'

      do jp =1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	if ( MONQ .gt. 1 ) call write shots for river flow
	endif
      enddo
      
      if ( IPRINT .eq. 1 ) return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,1)
      write(21,1)
    1 format(77('-')/
     &'Calculated river quality at flow from lake ...'/77('-'))
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at lake outflow
	endif
      call write data for graph plotting
	endif

      return
      end



      subroutine catchment boundary
      include 'COM.FOR'

      do jp =1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	if ( MONQ .gt. 1 ) call write shots for river flow
	endif
      enddo

      if ( ical13 .eq. 1 ) return
*     call sort out the concentrations and loads
      call add up all the loads
      
*     add to the counter of sub-catchments -------------------------------------
      kount bodies = kount bodies + 1
*     check for too many sub-catchments ----------------------------------------
      if ( kount bodies .gt. NUW ) then
      if ( ifbatch .eq. 0 ) then
 	call change colour of text (14) ! bright yellow
      write( *,4511)NUW
      call set screen text colour
 4511 format('Too many sub-catchments for back-tracking ',
     &'up the model ... the limit is ',i4/
     &'The extra sub-catchments are not being tracked ...')
	kount bodies = kount bodies - 1
	endif
	else
*     store the new sub-catchment ----------------------------------------------
*     store the number of the feature for this sub-catchment -------------------
	identify bodies ( kount bodies ) = feeture
	identify reaches (1, kount bodies ) = ireach
	if ( ireach .eq. 1 ) then
	identify reaches (2, kount bodies ) = 0
	identify reaches (3, kount bodies ) = 0
	else
	identify reaches (2, kount bodies ) = -IPLAN(IREACH-1,1)
	identify reaches (3, kount bodies ) = -IPLAN(IREACH-1,2)
	endif
	endif ! kount bodies .gt. NUW

      do 100 jp = 1, ndet
      do 101 j13 = 1, n13
*     store the monthly loads (j13) from upstream sub-catchments ---------------
	twloads ( kount bodies ,jp, j13) = TGLODE2 (jp, j13)
      
*     ( 1) = 'Mean from all discharges (3,12,5,39)'
*     ( 2) = 'Added by sewage effluents (3)'
*     ( 3) = 'Intermittent discharges of sewage (12)'
*     ( 4) = 'Added by industrial discharges (5)'
*     ( 5) = 'Added by mine waters (39)         '
*     ( 6) = 'Diffuse pollution from livestock (25)'
*     ( 7) = 'Diffuse pollution from arable (27)'
*     ( 8) = 'Highway runoff (29)'
*     ( 9) = 'Urban runoff (31)'
*     (10) = 'Atmosphere deposition (33)'
*     (11) = 'Natural background (35)'
*     (12) = 'Septic tanks (37)'
*     (13) = 'Aggregate CSOs (40)'
*     (14) = 'Aggregated STWs (42)'
*     (15) = 'Diffuse mines (46)'
*     (16) = 'Birds, boats and angling (48)'

      twloadsapp ( kount bodies ,jp, j13, 1 ) = TELODE2  (jp, j13)   
      twloadsapp ( kount bodies ,jp, j13, 2 ) = T03LOAD2 (jp, j13) ! sewage effluents (3)
      twloadsapp ( kount bodies ,jp, j13, 3 ) = T12LOAD2 (jp, j13) ! intermittent discharges of sewage (12)
      twloadsapp ( kount bodies ,jp, j13, 4 ) = T05LOAD2 (jp, j13) ! industrial discharges (5) 
      twloadsapp ( kount bodies ,jp, j13, 5 ) = T39LOAD2 (jp, j13) ! mine waters (39)
      twloadsapp ( kount bodies ,jp, j13, 6 ) = T25LOAD2 (jp, j13) ! livestock (25)
      twloadsapp ( kount bodies ,jp, j13, 7 ) = T27LOAD2 (jp, j13) ! arable (27)
      twloadsapp ( kount bodies ,jp, j13, 8 ) = T29LOAD2 (jp, j13) ! highway runoff (29)
      twloadsapp ( kount bodies ,jp, j13, 9 ) = T31LOAD2 (jp, j13) ! urban runoff (31)
      twloadsapp ( kount bodies ,jp, j13, 10) = T33LOAD2 (jp, j13) ! atmosphere deposition (33)
      twloadsapp ( kount bodies ,jp, j13, 11) = T35LOAD2 (jp, j13) ! natural background (35)
      twloadsapp ( kount bodies ,jp, j13, 12) = T37LOAD2 (jp, j13) ! septic tanks (37)
      twloadsapp ( kount bodies ,jp, j13, 13) = T40LOAD2 (jp, j13) ! aggregate CSOs (40)
      twloadsapp ( kount bodies ,jp, j13, 14) = T42LOAD2 (jp, j13) ! aggregated STWs (42)
      twloadsapp ( kount bodies ,jp, j13, 15) = T46LOAD2 (jp, j13) ! diffuse mines (46) 
      twloadsapp ( kount bodies ,jp, j13, 16) = T48LOAD2 (jp, j13) ! birds, boats and angling (48)
      
*     remove the loads from upstream sub-catchments ----------------------------
*     need to subtract all upstream sub-catchments that are in continuity with -
*     this one ---
      if ( kount bodies .gt. 1 ) then
      do 102 ibodies = 1, kount bodies - 1
*     monthly loads from upstream sub-catchments -------------------------------
      twloads (kount bodies, jp, j13) = twloads (kount bodies, jp, j13) 
     &- twloads (ibodies, jp, j13)
      do ip = 1, nprop
*     breakdown of monthly loads from upstream sub-catchments ------------------
      twloadsapp (kount bodies, jp, j13, ip) = 
     &twloadsapp (kount bodies, jp, j13, ip) -
     &twloadsapp (ibodies, jp, j13, ip)
      enddo
  102 continue
      endif
  101 continue 
  100 continue 

      if ( IPRINT .eq. 1 ) return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,1)
      write(21,1)
    1 format(77('-')/
     &'Calculated river quality at this catchment boundary ...'/77('-'))
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at catchment boundary
	endif
      call write data for graph plotting
      endif

      return
      end



*     discharge with zero flow -------------------------------------------------
      subroutine zero discharge flow 17
      include 'COM.FOR'

      if ( IPRINT .eq. 1 )return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,1015)
	write(21,1015)
 1015 format(77('-')/
     &'Calculated river quality at zero flow discharge ...'/77('-'))
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at zero flow discharge
	endif

*     write out the results downstream of tributary for graph-plotting ---------
      call write data for graph plotting
      endif

      return
	end


*     flow gauge ---------------------------------------------------------------
      subroutine river flow gauge
      include 'COM.FOR'

      IF = JF(KFEAT)
      if ( IF .ge. 1 ) then
      NGAUGE = NGAUGE + 1
      if ( NGAUGE .LE. MG ) goto 8821
      if ( nobigout .le. 0 )write(01,8822)
 8822 format('*** Too many flow gauges ..............'/
     &       '*** This one will not appear in the summaries .......')
      NGAUGE = NGAUGE - 1
      return
 8821 continue

      SFL(NGAUGE,1) = FLOW(1) ! mean calculated flow
      SFL(NGAUGE,2) = FLOW(2) ! calculated 95-percentile low flow
      KGAUGES(NGAUGE) = KFEAT

      X(1) = F(IF,2)
      if ( output mode .eq. 1 ) X(1) = F(IF,1)

      if ( IPRINT .eq. 1 ) return

	if ( nobigout .le. 0 ) then
      call sort format 2 (F(IF,1),F(IF,2))
	write(01,3045)valchars10,FUNIT,valchars11,FUNIT
	write(21,3045)valchars10,FUNIT,valchars11,FUNIT
 3045 format(77('=')/
     &'Observed flow in the river ...',26X,'Mean =',a10,1x,a4/
     &38X,'95-percentile low flow =',a10,1x,A4/77('='))
      endif
      else
	if ( nobigout .le. 0 ) then
	write(01,3845)
	write(21,3845)
 3845 format(77('-')/
     &'No flow date were specified for this flow gauge ...'/77('-'))
      endif
      endif

      if ( ical13 .eq. 0 ) then
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at flow gauge
      call write data for graph plotting
      endif

      return
      end



*     small stream or tributary ------------------------------------------------
      subroutine tributary
      include 'COM.FOR'

      call initialise data for mass balance ! tributary

      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
      if ( nobigout .le. 0 ) then
      write(01,1009)
      write(21,1009)
 1009 format('River quality just upstream of tributary ...'/77('-'))
      if ( ical13 .eq. 0 ) then
      write(34,1009)
	endif
	endif
      call get summaries and write out loads and quality (0)

  970 if ( FLOW(1) .lt. 1.0E9 ) goto 70
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
	call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow exceeds a billion ',26x,'...',7x,'at ',
     &a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(01,7817)UNAME(KFEAT)
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .7.'/
     &'*** Calculations proceeding ...'/100('*'))
	flow(1) = 999999.9
	flow(2) = 99999.9

*     flow added to river ------------------------------------------------------
*     identify the code number for the flow data-set ---------------------------
   70 continue
      IF = JF(KFEAT)
      IDIST = PDRF(IF)

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 )  then
      if ( nobigout .le. 0 ) then
      if ( IDIST .gt. 0 ) then
      call sort format 2 (F(IF,1),F(IF,2))
	write(01,2012)valchars10,FUNIT,IF,valchars11,FUNIT
	write(21,2012)valchars10,FUNIT,IF,valchars11,FUNIT
 2012 format('Flow discharged from tributary:',25X,
     &'Mean =',a10,1X,A4/'Number of data set:',i6,
     &21x,'95% exceedence =',a10,1x,a4/77('-'))
      else ! if ( IDIST .gt. 0 )
      call sort format 1 (F(IF,1))
      write(01,2812) valchars10,FUNIT
      write(21,2812) valchars10,FUNIT
 2812 format(77('-')/'Flow discharged from tributary:',25x,
     &'Constant =',a10,1X,A4/77('-'))
      endif ! if ( IDIST equals 0 )
	endif
      endif

*     note whether the distribution of flows is non-parametric =================
      if ( PDRF(IF) .eq. 4  ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
	if ( nobigout .le. 0 ) then
      write(01,9965) flnamesmall(1,icod)
      write(21,9965) flnamesmall(1,icod)
      endif
 9965 format(
     &'These FLOWS are from a non-parametric distribution ... ',
     &'File: ',a64/77('-'))
      goto 8970
      endif
      enddo
      write(01,8920)
      write(21,8920)
      write( *,8920)
      write(09,8920)
      write(33,8920)
 8920 format(/'*** The specified non-parametric data do not exist ...')
	call stop
	endif
 8970 continue

*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then

*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
	if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
      write(01,7920)FLMONTHsmall(1,icod)
      write(21,7920)FLMONTHsmall(1,icod)
      write( *,7920)FLMONTHsmall(1,icod)
      write(09,7920)FLMONTHsmall(1,icod)
      write(33,7920)FLMONTHsmall(1,icod)
 7920 format(/'*** Monthly data specified do not exist ... File: ',a64)
	call stop
 2970 continue
	if ( nobigout .le. 0 ) then
      write(01,1966) FLMONTHsmall(1,icod)
      write(21,1966) FLMONTHsmall(1,icod)
      endif
 1966 format(77('-')/
     &'These flow data were from monthly distributions ... '/
     &'File: ',a64/77('-'))
      endif

*     note whether the data are monthly structure ------------------------------
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do 3969 i = 1, M8
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3970
 3969 continue
 3974 continue
      write( *,*)'Stopped 98654'
      call stop
 3970 continue
	if ( nobigout .le. 0 ) then
      write(01,3966) FLSTRUCT(1,icod)
      write(21,3966) FLSTRUCT(1,icod)
      endif
 3966 format(77('-')/
     &'These flows have a monthly structure (type 8) ',
     &' ... File: ',a64)
      endif ! monthly structure

*     check for valid data on river quality ------------------------------------
      IQ = JQ(KFEAT)
      if ( quolity data (IQ,1,1) + 1.0 .lt. 0.0 ) then
      write(01,1020)IQ,uname(KFEAT)
      write(21,1020)IQ,uname(KFEAT)
 1020 format(/'*** Quality data',I6,' specified for feature',
     &' do not exist ... ',a37)
      call change colour of text (10) ! bright green
      write( *,1920)IQ,uname(KFEAT)
 1920 format('* Quality data',I6,' specified for feature',
     &' do not exist ...  ',a37)
      call set screen text colour
   12 write(01,1006)
 1006 format(/'*** In sub-routine tributary.'/77('-'))
*     call stop
      endif

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 .or. ICAL .eq. 1 ) goto 73
*     if ( nobigout .le. 0 ) write(01,2044)IQ
 2044 format('Quality of tributary flow: data set:',I6)
      call write mean and standard deviation

   73 continue
      IFDIST = PDRF(IF)

*     prepare for mass balance -------------------------------------------------
      if ( F(IF,1) .lt. 1.0E-10 ) goto 47
*     mean flow and 95-percentile low flow -------------------------------------
      EFM = F(IF,1)
      EF5 = F(IF,2)
*     shift flow ---------------------------------------------------------------
      EF3 = 0.0
      if ( PDRF(IF) .eq. 3 ) EF3 = F(IF,3)

      GEFS = 0.0
      GEFM = 0.0

      if ( EF5 .gt. 1.0e-08 .and. EF5 .ge. EFM ) then
      suppress10 = suppress10 + 1
      call sort format 2 (EFM,EF5)
      write(33,8151)valchars11,valchars10,IF
 8151 format(77('-')/'*** 95-percentile low flow of ',a10,
     &' exceeds the mean of ',a10/'*** Check the set of flow ',
     &'data number',i6)
	write(33,8152)
 8152 format('*** The 95-percentile low flow was set to 25% of ',
     &'the mean ... '/77('-'))
      if ( kerror .eq. 1 ) then
 	call change colour of text (37) ! dull turquoise
	write( *,8157)uname(Kfeet),IF
      call set screen text colour
 8157 format('* Zero 95-percentile flow set to 25% of ',
     &'the mean',3x,'...',7x,'at ',a37,4x,'Set number',i7,' .......')
      endif
      F(IF,2) = 0.25 * F(IF,1) 
      EF5 = 0.25 * EFM
      endif
      
      if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08) then
      write(33,8951)IF,uname(Kfeet)
 8951 format(77('-')/'*** Zero 95-percentile low flow ... '/
     &'*** Check set of flow data number',i6,1x,a37/
     &'*** 95-percentile set to 1% of the mean ... '/77('-'))
      if ( kerror .eq. 1 ) then
 	call change colour of text (37) ! dull turquoise
	write( *,8956)uname(Kfeet),IF
      call set screen text colour
 8956 format('* Zero 95-percentile flow set to 1% of ',
     &'the mean',4x,'...',7x,'at ',a37,4x,'Set number',i7,' .......')
      endif
      F(IF,2) = 0.01 * F(IF,1) 
      EF5 = 0.01 * EFM
      endif ! if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08)

*     Compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( PDRF(IF) .eq. 2 .or. PDRF(IF) .eq. 3 ) then
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFS = 2.7057+2.*ALOG((EFM+EF3)/TEST)

      if (GEFS .le. 0.0) then
      write(01,9099)UNAME(KFEAT)
      write( *,9099)UNAME(KFEAT)
      write(09,9099)UNAME(KFEAT)
      write(33,9099)UNAME(KFEAT)
 9099 format(///'#### Flow error for the Feature called - ',A32/
     &'#### Simulation stopped in TRIBUTARY ...')
      call stop
	endif

      GEFS = SQRoot(1044,GEFS) - 1.6449
      EFS = EXP(GEFS*GEFS)-1.0
      if (EFS .le. 1.0E-10) then
      write(01,8099)UNAME(KFEAT)
      write( *,8099)UNAME(KFEAT)
      write(09,8099)UNAME(KFEAT)
      write(33,8099)UNAME(KFEAT)
 8099 format(///'#### Flow error for Feature called - ',A32//
     &'#### Simulation stopped in TRIB ...')
      call stop
	endif
      EFS = EFM*SQRoot(1045,EFS)
      GEFM = ALOG (EFM+EF3) - 0.5*GEFS*GEFS
      endif

      if ( PDRF(IF) .eq. 1 ) then ! normal distribution ------------------------
      GEFM = EFM
      GEFS = (EFM-EF5)/1.6449
      EFS = GEFS
      endif ! if ( PDRF(IF) .eq. 1 ) normal distribution -----------------------

      if ( PDRF(IF) .eq. 0 ) then ! constant flow ------------------------------
      GEFM = EFM
      GEFS = 0.0
      EFS = GEFS
      endif ! constant flow ----------------------------------------------------
      
      do 6957 JP = 1, ndet
*     note that JP number NDET needs to go through Mass Balance ----------------
 	if ( Qtype (JP) .eq. 4 ) goto 6957

      call inititialise the stores for the estimates of load 
      IQDIST = PDRC (IQ,JP) ! type of distribution -----------------------------
      ECM = quolity data (IQ,JP,1) ! mean --------------------------------------
      ECS = quolity data (IQ,JP,2) ! standard deviation ------------------------

      EC3 = 0.0
      if ( PDRC (IQ,JP) .eq. 3) EC3 = quolity data(IQ,JP,3)

*     default correlation for tributary quality on tributary flow --------------
      CO5 = RFCL(JP) 
      if ( quolity data (IQ,JP,4) .ne. -9.9 ) then
	CO5 = quolity data (IQ,JP,4) ! special correlation -----------------------
      endif
      
      CO2 = 1.0 ! default correlation of tributary flow on main river flow -----
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F (IF,MO) ! special correlation ----------

      ifdiffuse = 0
      call mass balance ! from tributary
      call accumulate the loads
      call check the correlation coefficients by regression ! from tributary ---

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .and. qtype (JP) .ne. 4 ) then
      xnum = QNUM(IQ,JP)
      call get sampling rates for river quality ! tributary --------------------
     &(FLOW(1),C(JP,1),QUALN(JP),EFM,ECM,xnum)
	endif

 6957 continue

*     write out the results downstream for graph-plotting ----------------------
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting
      endif
   47 continue

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
	if ( MONF .gt. 1 ) call write shots for river flow
	call calculate summaries of river flow

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 3 .and. KSIM .eq. 0 ) call write out river flow
      if ( ical .ne. 3 ) call write out river flow
	endif

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,1023)
	write(21,1023)
	write(31,1023)
	write(34,1023)
 1023 format(77('-')/'River quality ...'/77('-'))
	endif

      call get summaries and write out loads and quality (0)
	call write out monthly river quality (5) ! at tributary
	endif
      return
      end



      subroutine write out river flow
      include 'COM.FOR'

      call sort format 2 (Flow (1),Flow(2))
      if ( nobigout .le. 0 ) then
	write(01,4)valchars10,FUNIT,valchars11,FUNIT
	write(21,4)valchars10,FUNIT,valchars11,FUNIT
    4 format(77('-')/
     &'Flow in river just downstream ...',23X,'Mean =',a10,
     &1x,A4/46X,'95% exceedence =',a10,1x,A4/77('-'))
      if ( ical13 .eq. 0 ) then
	write(34,4)valchars10,FUNIT,valchars11,FUNIT
	write(31,4)valchars10,FUNIT,valchars11,FUNIT
	endif
      endif
      return
     
      return
	end



      subroutine write mean and standard deviation
      include 'COM.FOR'

      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      if ( nobigout .le. 0 ) then
      write(01,6)
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if ( PDRC(IQ,J) .lt. 6 ) then
      write(01,1022)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      write(21,1022)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
 1022 format(A11,43X,'  Mean =',a10,1x,A4/42x,
     &'Standard deviation =',a10,1x,a4)
	endif

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads   
      write(01,1522)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      write(21,1522)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
 1522 format(A11,40X,'Mean load =',a10,1x,a4/42x,
     &'Standard deviation =',a10,1x,a4)
	endif

      if ( PDRC(IQ,J) .eq. 7 .or. PDRC(IQ,J) .eq. 9 ) then  ! loads  
      write(01,1622)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      write(21,1622)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
 1622 format(A11,40X,'Mean load =',a10,1x,a4/42x,
     &'Standard deviation =',a10,1x,a4)
	endif

      if ( PDRC(IQ,J) .eq. 8 ) then  ! monthly structure  
      write(01,1922)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      write(21,1922)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
 1922 format(A11,43X,'  Mean =',a10,1x,A4/42X,
     &'Standard deviation =',a10,1x,A4)
	endif

*     if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
*     write(01,1965)
*1965 format(77('-')/
*    &'The data for the above determinand are loads not ',
*    &'concentrations ...'/
*    &'They follow a normal distribution ...')
*	endif

*     if ( PDRC(IQ,J) .eq. 7 ) then ! log normal loads
*	write(01,3965)
*3965 format(77('-')/
*    &'The data for the above determinand are loads not ',
*    &'concentrations ...'/
*    &'They follow a log-normal distribution ...')
*	endif


*     note whether the distribution is non-parametric --------------------------
      if ( PDRC(IQ,J) .eq. 4 .or. PDRC(IQ,J) .eq. 9 ) then
*     identify the file --------------------------------------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, J+1 ) .eq. IQ ) goto 1970
 1969 continue
	goto 1974
 1970 continue
	write(01,7965) flnamesmall(2,icod)
	write(21,7965) flnamesmall(2,icod)
 7965 format('Non-parametric distribution ... File: ',a64,19('.'))
      endif
 1974 continue

*     note whether the data are monthly -----------------------------------------
      if ( PDRC(IQ,J) .eq. 5 ) then
*     identify the file with the monthly data -----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, J+1 ) .eq. IQ ) goto 2970
 2969 continue
	goto 2974
 2970 continue
	if ( nobigout .le. 0 ) write(01,1966) FLMONTHsmall(2,icod)
	if ( nobigout .le. 0 ) write(21,1966) FLMONTHsmall(2,icod)
 1966 format(77('-')/
     &'These flows are from monthly distributions ... ',
     &'File: ',a64)
      endif
 2974 continue

*     note whether the data are monthly structure monthly ------------------------
      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly data ------------------------------------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
	goto 3974
 3970 continue
	if ( nobigout .le. 0 ) write(01,1916) FLSTRUCTsmall(2,icod)
	if ( nobigout .le. 0 ) write(21,1916) FLSTRUCTsmall(2,icod)
 1916 format(77('-')/
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif
 3974 continue
      endif
    1 continue

      if ( nobigout .le. 0 ) then
      write(01,6)
      write(21,6)
    6 format(77('-'))
	endif

      return
      end


      subroutine write mean and standard deviation 33
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
      correl = quolity data(IQ,J,4)
      if ( quolity data(IQ,J,4) .lt. -9.91 .or. 
     &     quolity data(IQ,J,4) .gt. -9.89 ) correl = 0.0
          
      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      if ( PDRC(IQ,J) .le. 3 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1822)DNAME(J),valchars11,valchars10,UNITS(J),correl
 1822 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,' ...',f7.3,' (Correlation with flow)')
	endif
      
      if ( PDRC(IQ,J) .eq. 4 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1823)DNAME(J),valchars11,valchars10,UNITS(J),correl
 1823 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric) ...',f7.3,
     &' (Correlation with flow)')
	endif

      if ( PDRC(IQ,J) .eq. 5 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1825)DNAME(J),valchars11,valchars10,UNITS(J),correl
 1825 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly distributions ...',
     &f7.3,' (Correlation with flow)')
	endif

      if ( PDRC(IQ,J) .eq. 6 .or. PDRC(IQ,J) .eq. 7 ) then ! loads 
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1922)DNAME(J),valchars11,valchars10,LUNITS(J),correl
 1922 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Loads) ...',f7.3,
     &' (Correlation with flow)')
	endif
      
      if ( PDRC(IQ,J) .eq. 9 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1923)DNAME(J),valchars11,valchars10,LUNITS(J),correl
 1923 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric loads) ...',
     &f7.3,' (Correlation with flow)')
	endif

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      write(01,1824)DNAME(J),valchars11,valchars10,UNITS(J),correl
 1824 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly structure) ...',f7.3,
     &' (Correlation with flow)')
	endif

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*	write(01,1965)correl
 1965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,'They follow a normal distribution ...',
     &'... correlation = ',f6.3)
	endif
	endif

      if ( PDRC(IQ,J) .eq. 7 ) then ! log normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*	write(01,3965)correl
 3965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,
     &'They follow a log-normal distribution ... correlation = ',f6.3)
	endif
	endif

*     note whether the distribution is non-parametric ------------------------
      if ( PDRC(IQ,J) .eq. 4 .or. PDRC(IQ,J) .eq. 9 ) then
*     identify the file with the non-parametric data -------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, J+1 ) .eq. IQ ) goto 1970
 1969 continue
	goto 1974
 1970 continue
*     write(01,7163) dname(j),flnamesmall(2,icod)
*7163 format(33x,a11,' ... non-parametric ... ',a64)
      endif
 1974 continue

*     note whether the data are monthly ---------------------------------------
      if ( PDRC(IQ,J) .eq. 5 ) then
*     identify the file with the monthly data ---------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, J+1 ) .eq. IQ ) goto 2970
 2969 continue
	goto 2974
 2970 continue
*     write(01,7363) FLMONTHsmall(2,icod)
*7363 format(33x,'These data are from monthly distributions ...',
*    &' File: ',a64)
      endif
 2974 continue

*     note whether the data are monthly structure ------------------------------
      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
	goto 3974
 3970 continue
	write(01,1916) FLSTRUCTsmall(2,icod)
 1916 format(33x,77('.')/33x,
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif !  ! monthly structure
 3974 continue

    1 continue
      endif
      return
      end

      
      subroutine write mean and standard deviation 33a
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      if ( PDRC(IQ,J) .le. 3 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1822)DNAME(J),valchars11,valchars10,UNITS(J)
 1822 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
	endif
      
      if ( PDRC(IQ,J) .eq. 4 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1823)DNAME(J),valchars11,valchars10,UNITS(J)
 1823 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric)')
	endif

      if ( PDRC(IQ,J) .eq. 5 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1825)DNAME(J),valchars11,valchars10,UNITS(J)
 1825 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly distributions)')
	endif

      if ( PDRC(IQ,J) .eq. 6 .or. PDRC(IQ,J) .eq. 7 ) then ! loads  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1829)DNAME(J),valchars11,valchars10,LUNITS(J)
 1829 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Loads)')
	endif
      
      if ( PDRC(IQ,J) .eq. 9 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1723)DNAME(J),valchars11,valchars10,LUNITS(J)
 1723 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric loads)')
	endif

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure   
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      write(01,1824)DNAME(J),valchars11,valchars10,UNITS(J)
 1824 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly structure)')
	endif

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*	if ( nobigout .le. 0 ) write(01,1965)
 1965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,'They follow a normal distribution ...')
	endif
	endif

      if ( PDRC(IQ,J) .eq. 7 ) then ! log normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*	if ( nobigout .le. 0 ) write(01,3965)
 3965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,
     &'They follow a log-normal distribution ...')
	endif
	endif

*     note whether the distribution is non-parametric ------------------------
      if ( PDRC(IQ,J) .eq. 4 .or. PDRC(IQ,J) .eq. 9 ) then
*     identify the file with the non-parametric data -------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, J+1 ) .eq. IQ ) goto 1970
 1969 continue
	goto 1974
 1970 continue
*     write(01,7163) dname(j),flnamesmall(2,icod)
*7163 format(33x,a11,' ... non-parametric ... ',a64)
      endif
 1974 continue

*     note whether the data are monthly ---------------------------------------
      if ( PDRC(IQ,J) .eq. 5 ) then
*     identify the file with the monthly data ---------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, J+1 ) .eq. IQ ) goto 2970
 2969 continue
	goto 2974
 2970 continue
*     write(01,7363) FLMONTHsmall(2,icod)
*7363 format(33x,'These data are from monthly distributions ...',
*    &' File: ',a64)
      endif
 2974 continue

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
	goto 3974
 3970 continue
	write(01,1916) FLSTRUCTsmall(2,icod)
 1916 format(33x,77('.')/33x,
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif
 3974 continue

    1 continue
      endif
      return
      end




*     put data to a file for plotting graphs -----------------------------------
      subroutine write data for graph plotting
      include 'COM.FOR'

      if ( JSKIP .gt. 0 ) return
 
      call calculate summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads

*     loop on the determinands -------------------------------------------------
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4) then
      if ( kdecision(J) .ne. 8 ) then

*     these are the variables for plotting -------------------------------------
*     --------------------------------------------------------------------------
*     CL(1... is the mean concentration
*     CL(2... is the lower confidence limit on the mean concentration
*     CL(3... is the upper confidence limit on the mean
*     CL(4... is the calculated value of the 90-percentile
*     CL(5... is the lower confidence limit on the percentile
*     CL(6... is the upper confidence limit on the percentile
*     CL(7... is the calculated value of the 95-percentile
*     CL(8... is the lower confidence limit on the percentile
*     CL(9... is the upper confidence limit on the percentile
*     CL(10.. is the calculated value of the 99-percentile
*     CL(11.. is the lower confidence limit on this percentile
*     CL(12.. is the upper confidence limit on this percentile
*     --------------------------------------------------------------------------

      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))

*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! within graph plotting routine --------------------
      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))
      endif

*     compute the extra values for partitioned metals --------------------------
   	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
     &(3,J,CP2(1,J),CP2(2,J),CP2(3,J),CP2(4,J),CP2(5,J),CP2(6,J),
     &CP2(7,J),CP2(8,J),CP2(9,J),CP2(10,J),CP2(11,J),CP2(12,J))
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J),
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (2,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J),
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
	endif ! dissolved and solid metal
      endif
      enddo

*     initialise the observed values -------------------------------------------
      do ii = 1, MP10 + 1
      COB (1, ii ) = 0.0
      COB (2, ii ) = 0.0
      COB (3, ii ) = 0.0
      COB (4, ii ) = 0.0
      enddo
      do ii = 1, MP10
      COD (1, ii ) = 0.0
      COD (2, ii ) = 0.0
      COD (3, ii ) = 0.0
      COD (4, ii ) = 0.0
      enddo

      call compute the mean and percentiles at a plotting point

	if ( JT (KFEAT) .eq. 1 ) then
      do JP = 1,MP10
      if ( QTYPE (JP) .ne. 4) then
      COB (1,JP+1) = quolity data ( IQ, JP, 1 )
      COB (1,JP+1) = amax1(0.0,quolity data ( IQ, JP, 1 ))
      endif     
      COB (2,JP+1) = X90 (JP)
      COB (3,JP+1) = X95 (JP)
      COB (4,JP+1) = X99 (JP)
      enddo
      endif

      if ( JT (KFEAT) .eq. 4 ) then
      if ( IF .ge. 1 ) then
      COB (1, 1) = F (IF, 1)
      COB (2, 1) = 99900000.0
      COB (3, 1) = F (IF, 2)
      COB (4, 1) = 99900000.0
      endif
      endif

*     Chris Page 26-03-2007
*     if ( virtualreach .eq. -1 ) then
      JTK = JT(KFEAT)
      if ( JTK .eq. 25 .or. JTK .eq. 27 .or. JTK .eq. 29 .or.  
     &     JTK .eq. 31 .or. JTK .eq. 33 .or. JTK .eq. 35 .or. 
     &     JTK .eq. 37 .or. JTK .eq. 40 .or. JTK .eq. 42 .or.
     &     JTK .eq. 13 .or. JTK .eq. 15 .or. JTK .eq. 46 .or.
     &     JTK .eq. 48 ) goto 1492
     
      write(22,300)jreach(kfeat),Length of main river
     &-adist(jreach(kfeat)),
     &UNAME(KFEAT),RNAME(JREACH(KFEAT)),JT(KFEAT)
  300 format(I4,',',1PE11.4,',',' " D/S ',A40,'","',A16,'",',i4)
      kount22 = kount22 + 1
      write(22,301)'[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4),
     &'[Flow    Obsd]',(COB(JCP,1),JCP=1,4)              

      do j=1,MP10
	if ( QTYPE (J) .ne. 4 ) THEN
*     concentrations -----------------------------------------------------------
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12)
*     loads --------------------------------------------------------------------
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12)
*     observed concentrations --------------------------------------------------
	write(22,303) '[Det',mod(j,10),'ConcObsd]',(COB(JCP,J+1),JCP=1,4)
*     observed loads -----------------------------------------------------------    
	write(22,303) '[Det',mod(j,10),'LoadObsd]',(COD(JCP,J),JCP=1,4)
*     targets ------------------------------------------------------------------
	write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J)
	write(22,304) '[Det',mod(j,10),'LoadTrgt]',0
      endif
      enddo

  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))
 1492 continue
*     endif

*     These are the variables for plotting ...
*     ----------------------------------------------------------
*     CL(1... is the mean
*     CL(2... is the lower confidence limit on the mean
*     CL(3... is the upper confidence limit on the mean
*     CL(4... is the calculated value of the 95-percentile
*     CL(5... is the lower confidence limit on the percentile
*     CL(6... is the upper confidence limit on the percentile
*     CL(7... is the calculated value of the 90-percentile
*     CL(8... is the lower confidence limit on the percentile
*     CL(9... is the upper confidence limit on the percentile
*     CL(10.. is the calculated value of the 99-percentile
*     CL(11.. is the lower confidence limit on this percentile
*     CL(12.. is the upper confidence limit on this percentile

*     =========================================================================
      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(72,2942)UNAME(KFEAT),
     &Length of main river-adist(jreach(kfeat)),
     &FLOW(1),(flowstats (i), i=1,9),(propstats (i), i=1,9)
 2942 format("d-s ",A40,f7.2,10f10.1/
     &4x,'Proportions of effluent ...',30x,9f10.3)

      call write GIS data to channel 42 43 24 and 25
	endif
	endif

      return
      end





*     calculations for features - effluent discharges --------------------------
      subroutine effluent discharge
      include 'COM.FOR'
      character *25 typename
      character *15 namestat
      character *20 run type description,forward calculation
      character *30 dischargedec,targetdec
      real cold(ndet,30)

      call initialise data for mass balance ! effluent discharge

      fupm = Flow(1) ! storage for output to the effluent CSV file -------------
      fup95 = Flow(2)
      
      do idet = 1, ndet ! storage for output to the effluent CSV file ----------
      do j = 1, 30      ! storage for output to the effluent CSV file ----------
      cold(idet,j) = 0.0
      enddo
      enddo
      
      if ( JSKIP .ne. 1 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .eq. 0 ) then
      write(01,1009)
      write(21,1009)
      write(34,1009)
 1009 format('River quality just upstream of effluent discharge ...'
     &/77('-'))

      call get summaries and write out loads and quality (0) ! upstream
      call write out monthly river quality (4) ! at effluent discharge
      call proportion of effluent (4)
	endif
	endif
      endif

      if ( FLOW(1) .gt. 1.0E9 ) then
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('* Flow exceeds a billion ',26x,'...'7x,'at ',
     &a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(01,7817) UNAME(KFEAT)
      write(09,7817) UNAME(KFEAT)
      write(33,7817) UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .8.'/
     &'*** Calculations proceeding ...'/100('*'))
	flow(1) = 999999.9
	flow(2) = 99999.9
      endif

      IQ = JQ(KFEAT) ! identify the set of effluent data

      call assess the need to track the discharge
      call write information on effluent flow 

      if ( pollution data(IQ,1,1)+1.0 .lt. 0.0 ) then
      call change colour of text (20) ! bright red
      write( *,9006)IQ,UNAME(KFEAT)
 9006 format('* Flow/quality data',i6,' specified for feature ',
     &'do not exist ... ',a20)
      call set screen text colour
      write(01,1020)IQ,UNAME(KFEAT)
      write(09,1020)IQ,UNAME(KFEAT)
      write(33,1020)IQ,UNAME(KFEAT)
 1020 format(/'*** Quality data',i6,' specified for feature ',
     &' do not exist ... ',a37)
   12 continue
      write(01,1006)
      write(09,1006)
      write(33,1006)
 1006 format(/'*** Stopped in sub-routine effluent discharge'/80('-'))
      call stop
      endif

*     prepare for mass balance -------------------------------------------------
*     extract the code for the distribution of discharge flow ------------------
      IFDIST = PDEF(IQ)

*     get the summary statistics for the discharge flow ------------------------
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EF3 = 0.0
      if ( PDEF(IQ) .eq. 3 ) EF3 = FE(IQ,3)

*     discharge flow on river flow for sewage treatment works ------------------
      CO2 = 0.6

*     the correlation coefficient is set to zero for industrial effluents ------
      if ( JT(KFEAT) .eq. 5 ) CO2 = 0.0
*     negative discharge (abstraction) -----------------------------------------
      if ( JT(KFEAT) .eq. 19 ) CO2 = 0.0
*     special non-default correlation ------------------------------------------
      if ( FE (IQ,4) .ne. -9.9 ) CO2 = FE (IQ,4)
*     store the upstream flow for extra calculations ---------------------------
      do is = 1, NS
      ufms (IS) = fms (IS)
      dfms (IS) = fms (IS)
      enddo
      
      call write effluent discharge quality

      jp1 = 0
      do 6930 JP = 1, NDET
	if ( qtype (JP) .eq. 4 ) goto 6930
      kdecision(JP) = 1 ! initialise as retain current -------------------------
      do is = 1, NS
      fms (IS) = ufms (IS)
      enddo
      jp1 = jp1 + 1
      CO1 = COD1 (jp) ! replace CO1 with the regression value ------------------
      do is = 1, NS ! store the upstream quality for extra calculations --------
      ucms (JP,IS) = cms (JP,IS)
      do ip = 1, nprop
      ulms (ip,JP,IS) = lms(ip,JP,IS)
	enddo
      enddo

      call get summaries of river quality from the shots
      call inititialise the stores for the estimates of load 

      cold(JP,1) = C(JP,1) ! mean u/s of discharge
      cold(JP,2) = C(JP,2)
      cold(JP,3) = C(JP,3) 
      cold(JP,4) = C(JP,4) 
      cold(JP,5) = C(JP,5) 
      cold(JP,6) = CO1
      call load calculation per determinand ! for upstream river flows
      cold(JP,21) = XLOAD(JP,1,i13) ! mean river load u/s of discharge
      cold(JP,22) = XLOAD(JP,2,i13) ! st.dev of river load u/s of discharge
      
      IQDIST = PDEC(IQ,JP) ! type of distribution ------------------------------
*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
      CO5 = EFCL (JP) ! default - discharge quality on discharge flow ----------
      if ( pollution data (IQ,JP,MO) .ne. -9.9 ) then ! special correlation ----
	CO5 = pollution data (IQ,JP,MO) ! discharge quality on discharge flow ----
      endif

*     pick up the mean and standard deviation ----------------------------------
      ECM = pollution data(IQ,JP,1) ! pick up the mean -------------------------
      ECS = pollution data(IQ,JP,2) ! pick up the standard deviation -----------
      cold(JP,29) = ECM
      cold(JP,30) = ECS
      EC3 = 0.0 ! shift parameter (for 3-parameter log-normal distribution) ----
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      
*     compute the percentile of effluent quality -------------------------------
*     call get effluent quality 95 percentile
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = ECM
      cold(JP,12) = TECX
      cold(JP,18) = ECM ! store final discharge quality - the starting value ---
      cold(JP,19) = ECS ! store final discharge quality - the starting value ---
      cold(JP,20) = TECX ! Dis_conc_95_final

      if ( ical .gt. 6 .and. detype (JP) .ne. 104 ) then
      ifdiffuse = 0
      call mass balance ! from effluent discharge (forward calculation) --------
*     call accumulate the loads
      call check the correlation coefficients by regression ! from effluents ***
      call get summaries of river quality from the shots
      cold(JP,7) = C(JP,1)
      cold(JP,8) = C(JP,2)
      cold(JP,9) = C(JP,3) 
      cold(JP,10) = C(JP,4) 
      cold(JP,11) = C(JP,5) 
      cold(JP,13) = eload(JP,i13) ! current discharge load
      cold(JP,14) = eloads(JP)    ! current discharge load
      cold(JP,27) = eload(JP,i13) ! set final load to current discharge load
      cold(JP,28) = eloads(JP)    ! set final load to current discharge load
      call load calculation per determinand ! set up back calculation
      cold(JP,23) = XLOAD(JP,1,i13) ! mean river quality d/s current discharge
      cold(JP,24) = XLOAD(JP,2,i13) ! st.dev d/s current discharge
      cold(JP,25) = XLOAD(JP,1,i13) ! initialise as mean d/s current discharge
      cold(JP,26) = XLOAD(JP,2,i13) ! initialise st.dev d/s current discharge
      kdecision(JP) = 1 ! initialise as retain current -------------------------
      call load the upstream flow and quality 
 	endif ! if ( ical .gt. 6 ) 
     
*     ##########################################################################
*     Start of block calculating River Needs discharge Standards ---------------
*     **************************************************************************
*     branch according to mode of calculation ----------------------------------
*     the following applies to calculating River Needs Standards ---------------
      if ( ical .lt. 07 ) goto 1833
      if ( SKIPPEFF(KFEAT) .eq. 1 ) then
      kdecision(JP) = 12
      if ( nobigout .le. 0 ) then
      write(01,1715)DNAME(JP)
      write(21,1715)DNAME(JP)
      write(31,1715)DNAME(JP)
 1715 format(110('=')/'Effluent excluded from ',
     &'meeting targets ... ',A11,'...'/
     &'The current quality has been retained ...'/
     &110('='))
      endif
      goto 1833
      endif
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
      kdecision(JP) = 11
      cold(JP,18) = -999.99 ! store final discharge concentration --------------
      cold(JP,19) = -999.99 
      cold(JP,20) = -999.99 ! Dis_conc_95_final
      cold(JP,29) = ECM
      cold(JP,30) = ECS
      if ( nobigout .le. 0 ) then
      write(01,1705)DNAME(JP)
      write(21,1705)DNAME(JP)
      write(31,1705)DNAME(JP)
 1705 format(110('-')/'Effluent quality is expressed as load ',
     &'rather than concentration ... ',A11,'... ',3x,
     &'Current quality retained'/110('-'))
      endif
      goto 1833
      endif ! if ( IQDIST .eq. ... loads 
      if ( ECM .lt. 1.0e-10 ) goto 1833

*     check for a river quality target -----------------------------------------
      Mtargit = 0
      targit = 0.0 ! set the target --------------------------------------------
      if ( detype (JP) .ne. 104 ) then ! do not apply for dissolved oxygen -----
      kdecision(JP) = 1 ! initialise as retain current discharge quality -------
      IQSfeet = IFRQS(feeture) ! identify the target specified for the Feature -
      IQSreach = EQS reach (IREACH,JP)
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( QTYPE (JP) .ne. 4 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP)
      endif
      if ( IQSfeet .gt. 0 ) then
      targit = RQS(IQSfeet,JP)
      endif
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-spepific target
      targit = abs (class limmits (ic,JP))
      endif
      enddo ! do ic = 1, nclass
      endif ! if ( IQSreach .gt. 0 )
      RQO(JP) = targit ! use the target for graphs ---------- effluent discharge
	MRQO(JP) = MRQS(JP) ! summary statistic for the standard -----------------
      Mtargit = MRQS(JP)
      endif ! if ( QTYPE (JP) .ne. 4 ) then
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )

      if ( Mtargit .eq. 0 ) goto 1833

      if ( Mtargit .ne. 1 ) then ! the standard is a percentile ----------------
*     calculate discharge quality needed to meet percentile standards ----------
*     of river quality ---------------------------------------------------------
      call meet river targets set as percentiles ! #############################
      call get effluent quality 95 percentile
      TECX = ECX
      cold(JP,18) = ECM ! store final discharge quality - from 95%ile target ---
      cold(JP,19) = ECS
      cold(JP,20) = TECX ! Dis_conc_95_final
      else ! if ( Mtargit .eq. 1 ) ... the standard is a mean ------------------
*     calculate discharge quality needed to meet mean standards ----------------
*     of river quality ---------------------------------------------------------
      call meet river targets set as averages ! ################################
      call get effluent quality 95 percentile
      if ( kdecision(JP) .ne. 3 ) then
      TECX = ECX
      cold(JP,18) = ECM ! store final discharge quality - from mean target -----
      cold(JP,19) = ECS
      cold(JP,20) = TECX ! Dis_conc_95_final
      else
      cold(JP,18) = TECM ! store final discharge quality - from mean target ----
      cold(JP,19) = TECS
      cold(JP,20) = TECX ! Dis_conc_95_final
      endif
      endif ! if ( Mtargit .ne. 1 )

*     ==========================================================================
*     SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SE
*     ==========================================================================
*     set the Dissolved Oxygen in the effluent ---------------------------------
*     according to the new value calculated for the Biochemical Oxygen Demand --
*     first, store the new Biochemical Oxygen Demand in the effluent -----------        
*     ==========================================================================
      if ( detype (JP) .eq. 101 ) BOD = ECM
*     check for Dissolved Oxygen -----------------------------------------------
*     whether Bad concentration less than Good ---------------------------------
      if ( detype (JP) .eq. 104 .and. ndetBOD .gt. 0 ) then
      if ( qtype (jp) .eq. 3 .and. WEQ(1,JP) .lt. GEQ(1,jp) ) then
*     compare the old Biochemical Oxygen Demand with the starting value --------
*     is the old value worse than the new Biochemical Oxygen Demand? -----------
      if ( pollution data (IQ,ndetBOD,1) .ge. BOD ) then
*     yes, it is worse ---------------------------------------------------------
*     prepare to bring down the Dissolved Oxygen -------------------------------
*     compare the Biochemical Oxygen Demand with good effluent quality (12 mg/l)
      if ( BOD .le. 12.0 ) then
*     interpolate between good and best effluent quality -----------------------
*     good Dissolved Oxygen in the effluent is 7.0 mg/l ------------------------
*     reduce by 0.5 mg/l for each 2 mgl above 2 mg/l ---------------------------
      Tem = Amax1 (0.0, BOD - WEQ(1,JP))
      ECM = Amax1 (WEQ(1,JP), GEQ(1,JP)-0.5 * Tem )
      ECS = 0.4 * ECM
      else ! if (BOD .le. 12.0)
*     BOD is greater than 12 mg/l ----------------------------------------------
*     retain input discharge quality for dissolved oxygen ----------------------
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
*     compute percentile of effluent quality -----------------------------------
      call get effluent quality 95 percentile
      TECX = ECX
      endif ! if (BOD .le. 12.0)
*     ==========================================================================
      if ( nobigout .le. 0 ) write(01,2954)ECM,UNITS(JP),ECS,UNITS(JP),
     &ECX,UNITS(JP)
 2954 format(
     &'........................................',14x,
     &'  Current discharge quality:        Mean =',F9.2,1X,A4/
     &'........................................',14x,
     &10X,'          Standard Deviation =',F9.2,1X,A4/
     &'........................................',14x,
     &10X,'               95-percentile =',F9.2,1X,A4/
     &110('-'))
      
*     BOD is worse than the starting quality (input quality) -------------------
*     retain input discharge quality -------------------------------------------
      else
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
*     compute percentile of effluent quality -----------------------------------
      call get effluent quality 95 percentile
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = ECM
      endif
	endif
	endif ! if ( detype (JP) .eq. 104 )
      endif ! if ( detype (JP) .ne. 104 )
*     ==========================================================================
*     SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SE
*     ==========================================================================

      if ( kdecision(JP) .ne. 8 ) then
      call load calculation per determinand
      cold(JP,25) = XLOAD(JP,1,i13) ! mean d/s of new discharge
      cold(JP,26) = XLOAD(JP,2,i13) ! st.dev d/s of new discharge
      cold(JP,27) = eload(JP,i13)   ! final discharge load
      cold(JP,28) = eloads(JP)      ! final discharge load
      endif

      call accumulate the loads

 1833 continue
*     ##########################################################################
*     End of block calculating River Needs discharge Standards #################
*     ##########################################################################

*     perform the main mass balance calculation with current data===============
      if ( ical .lt. 07 .or. ! back calculation not requested ===================
     & detype (JP) .eq. 104 .or. ! determinand is dissolved oxygen =============
     & kdecision(JP) .eq. 11 .or. ! discharge defined in terms of load data ====
     & kdecision(JP) .eq. 12 ) then ! discharge is excluded ====================
      ifdiffuse = 0
      call load the upstream flow and quality 
      call mass balance ! from effluent discharge (forward calculation) ========
      call accumulate the loads
*     write(120+JP,*)'007 TELODE2 =',TELODE2(JP,i13)
      call check the correlation coefficients by regression ! from effluents ===
      call get summaries of river quality from the shots
      cold(JP,7) = C(JP,1)
      cold(JP,8) = C(JP,2)
      cold(JP,9) = C(JP,3) 
      cold(JP,10) = C(JP,4) 
      cold(JP,11) = C(JP,5) 
      cold(JP,13) = eload(JP,i13) ! current discharge load
      cold(JP,14) = eloads(JP)    ! current discharge load 
      cold(JP,27) = eload(JP,i13) ! set final load to current discharge load
      cold(JP,28) = eloads(JP)    ! set final load to current discharge load
      call load calculation per determinand
      cold(JP,23) = XLOAD(JP,1,i13) ! mean d/s of current discharge
      cold(JP,24) = XLOAD(JP,2,i13) ! st.dev d/s of current discharge
      cold(JP,25) = XLOAD(JP,1,i13) ! mean d/s of current discharge
      cold(JP,26) = XLOAD(JP,2,i13) ! st.dev d/s of current discharge
      endif ! if ( ical .lt. 07 ) ==============================================

      call calculate summaries of river flow

*     calculate the confidence limits on river water quality -------------------
      xnum = PNUM(IQ,JP)
      if ( JSKIP .eq. 0 ) then
      call get sampling rates for river quality ! effluent discharge -----------
     &(FLOW(1),C(JP,1),QUALN(JP),EFM,ECM,xnum)
      endif
      
 6930 continue ! the loop on determinands has finished -------------------------

      if ( MONF .gt. 1 ) call write shots for river flow

*     write out results d/s of discharge for graph plotting --------------------
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! effluent discharge
      endif

      if ( nobigout .le. 0 ) then
	if ( ical .eq. 3 .and. KSIM .eq. 0 ) call write out river flow
	if ( ical .ne. 3 ) call write out river flow
	endif
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,2023)
	write(21,2023)
      write(34,2023)
 2023 format(77('-')/'River quality ...'/77('-'))
	endif
      call get summaries and write out loads and quality (0)
 	call write out monthly river quality (6) ! at effluent discharge
      endif ! if ( ical13 .eq. 0 
      
*     ##########################################################################
*     write details to the EFFLUENT CSV file -----------------------------------
*     ##########################################################################
*     0 forward calculation only
*     2 backward calculation
*     1 above upper bound
*     2 below lower bound
*     3 discharge too small
*     4 poor upstream quality
      run type description = 'Forward calculation'
      Forward calculation = 'Forward calculation'
      if ( ical .eq. 07 ) run type description = 'Back-calculation'
      if ( ical .eq. 08 ) run type description = 'Back-calculation'
      if ( ical .eq. 09 ) run type description = 'Back-calculation'
      
      xdummy = -999.999
      itype = JT(feeture)
      typename = 'Sewage Works'
      if (itype .eq. 05 ) typename = 'Industrial Discharge'
      if (itype .eq. 12 ) typename = 'Intermittent Discharge'
      if (itype .eq. 39 ) typename = 'Mining Discharge'
      
      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then
      istx = mrqs(JP)
      namestat = 'mean'
      if ( istx .eq. 2 ) namestat = '95-percentile'
      if ( istx .eq. 3 ) namestat = '90-percentile'
      if ( istx .eq. 4 ) namestat = '05-percentile'
      if ( istx .eq. 5 ) namestat = '10-percentile'
      if ( istx .eq. 6 ) namestat = '99-percentile'
 

      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(feeture) ! identify any target set for the feature -------
      
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
      endif
      
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( QTYPE (JP) .ne. 4 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS(IQSfeet,JP)
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-specific target
      targit = abs (class limmits (ic,JP))
      endif
      enddo ! do ic = 1, nclass
      endif ! if ( IQSreach .gt. 0 )
	RQO(JP) = targit ! use the target for graphs ----------- effluent dscharge
	MRQO(JP) = MRQS(JP)
	Mtargit = MRQS(JP)
	endif ! if ( QTYPE (JP) .ne. 4 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) 

      dischargedec = 'Option Switched Off' 
      if ( kdecision(JP) .eq. 1 ) dischargedec = 'Retained Current'  
      if ( kdecision(JP) .eq. 2 ) dischargedec = 'River Needs'  
      if ( kdecision(JP) .eq. 3 ) dischargedec = 'Best Achievable'  
      if ( kdecision(JP) .eq. 4 ) dischargedec = 'Worst Allowed'  
      if ( kdecision(JP) .eq. 5 ) dischargedec = 
     &'10% Deterioration in River'
      if ( kdecision(JP) .eq. 6 ) dischargedec = 
     &'10% Deterioration in Discharge'  
      if ( kdecision(JP) .eq. 7 ) dischargedec = 'Imposed Good'  
      if ( kdecision(JP) .eq. 8 ) dischargedec = 'Retained Current'  
      if ( kdecision(JP) .eq. 9 ) dischargedec = 'Retained Current'  
      if ( kdecision(JP) .eq. 10) dischargedec = 'Retained Current'  
      if ( kdecision(JP) .eq. 11) dischargedec = 'Retain Current Load'  
      if ( kdecision(JP) .eq. 12) dischargedec = 'Retained Current'  
      if ( kdecision(JP) .eq. 13) dischargedec = 'Imposed Good'  
      
      if ( kdecision(JP) .eq. 1 ) targetdec = 'No Target'  
      if ( kdecision(JP) .eq. 2 ) targetdec = 'Target Met'  
      if ( kdecision(JP) .eq. 3 ) targetdec = 'Target Not Met'  
      if ( kdecision(JP) .eq. 4 ) targetdec = 'No Threat to Target'  
      if ( kdecision(JP) .eq. 5 ) targetdec = 'No Threat to Target'
      if ( kdecision(JP) .eq. 6 ) targetdec = 'No Threat to Target'  
      if ( kdecision(JP) .eq. 7 ) targetdec = 'Target Not Met'  
      if ( kdecision(JP) .eq. 8 ) targetdec = 'Target Not Met'  
      if ( kdecision(JP) .eq. 9 ) targetdec = 'No Threat to Target'  
      if ( kdecision(JP) .eq. 10) targetdec = 'Trivial Discharge'  
      if ( kdecision(JP) .eq. 11) targetdec = 'Inadequate data'  
      if ( kdecision(JP) .eq. 12) targetdec = 'Excluded'  
      if ( kdecision(JP) .eq. 13) targetdec = 'No Threat to Target'  
      
      changeload = cold(JP,13) - cold(JP,27)
      changerload = cold(JP,23) - cold(JP,25)
      delta1 = amax1 (cold(JP,25),cold(JP,27))
      deltaload = 0.0
      if ( delta1 .gt. 1.0e-8) then
      deltaload = 100.0 * abs(changerload-changeload)/delta1
      endif

      if ( n147 .eq. 1 ) write(36,2)
     &ical,                        ! Run_Type
     &forward calculation,         ! Run_Description 
     &uname(feeture),              ! Feature_Name 
     &feeture,                     ! Feature_No 
     &rname(IREACH),               ! Reach_Name
     &IREACH,                      ! Reach_No 
     &GIScode(feeture),            ! GIS_ref 
     &typename,                    ! Dis_Type_Name 
     &JT(feeture),                 ! Dis_Type_No 
     &dname(JP),                   ! Det_Name 
     &UNITS(JP),                   ! Det_Units
     &JP,                          ! Det_Number
     &targit,                      ! Target
     &namestat,                    ! Stat_Name
     &istx,                        ! Target_type_No
      
     &fupm,                        ! Riv_F_mean_ up 
     &fup95,                       ! Riv_F95_up 
      
     &cold(JP,1),                  ! Riv_qual_mean_up 
     &cold(JP,2),                  ! Riv_qual_sd_up 
     &cold(JP,6),                  ! Riv_CF_Corr 
     &cold(JP,21),                 ! Riv_load_mean_up
     &cold(JP,22),                 ! Riv_load_st_up
      
     &FE(IQ,1),                    ! Dis_f_Mean 
     &FE(IQ,2),                    ! Dis_f_sd 
     &CO2,                         ! Dis_fF_Corr 
     &PDEF(IQ),                    ! Dis_f_dist_type 
      
     &pollution data (IQ,JP,1),    ! Dis_qual_mean_orig 
     &pollution data (IQ,JP,2),    ! Dis_qual_sd_orig 
     &cold(JP,12),                 ! Dis_qual_q95 
     &EFCL(JP),                    ! Dis_cf_Corr 
     &PDEC(IQ,JP),                 ! Dis_c_dist_type ........... 
      
     &cold(JP,29),                 ! Dis_conc_mean_orig 
     &cold(JP,30),                 ! Dis_conc_sd_orig 
     &cold(JP,13),                 ! Dis_load_mean_orig xxxxxxxx 
     &cold(JP,14),                 ! Dis_load_st_orig oooooooooo
      
     &FLOW(1),                     ! Riv_F_mean_ down oooooooooo
     &FLOW(2),                     ! Riv_F95_down oooooooooooooo
     &cold(JP,7),                  ! Riv_qual_mean_down xxxxxxxx
     &cold(JP,8),                  ! Riv_qual_sd_down 
     &cold(JP,10),                 ! Riv_qual_q90_down 
     &cold(JP,9),                  ! Riv_qual_q95_down 
     &cold(JP,11),                 ! Riv_qual_q99_down xxxxxxxxxx
     &cold(JP,23),                 ! Riv_load_mean_down_forw 
     &cold(JP,24),                 ! Riv_load_st_down_forw 
      
     &ICAL,                        ! Res_type_code 
     &run type description,        ! Res_type_desc ... backwards
     &kdecision(JP),               ! Discharge_option
     &targetdec,                   ! Outcome
     &dischargedec,                ! Discharge_decision
      
     &C(JP,1),                     ! Riv_qual_mean_back 
     &C(JP,2),                     ! Riv_qual_sd_back
     &C(JP,4),                     ! Riv_qual_q90_back
     &C(JP,3),                     ! Riv_qual_q95_back 
     &C(JP,5),                     ! Riv_qual_q99_back
     &cold(JP,25),                 ! Riv_load_mean_down_back 
     &cold(JP,26),                 ! Riv_load_st_down_back 
      
     &cold(JP,18),                 ! Dis_conc_mean_final 
     &cold(JP,19),                 ! Dis_conc_sd_final 
     &cold(JP,20),                 ! Dis_conc_95_final 
      
     &cold(JP,27),                 ! Dis_load_mean_final 
     &cold(JP,28),                 ! Dis_load_st_final
     &changeload,                  ! Dis_load_mean_drop
      
     &changerload                  ! Riv_load_mean-down_drop 
      
      if ( n147 .eq. 1 ) write(38,3)
     &ical,                        ! Run_Type
     &forward calculation,       ! Run_Description 
     &uname(feeture),              ! Feature_Name 
     &feeture,                     ! Feature_No 
     &rname(IREACH),               ! Reach_Name
     &IREACH,                      ! Reach_No 
     &GIScode(feeture),            ! GIS_ref 
     &typename,                    ! Dis_Type_Name 
     &JT(feeture),                 ! Dis_Type_No 
     &dname(JP),                   ! Det_Name 
     &UNITS(JP),                   ! Det_Units
     &JP,                          ! Det_Number
     &targit,                      ! Target
     &namestat,                    ! Stat_Name
     &istx,                        ! Target_type_No
      
     &fupm,                        ! Riv_F_mean_ up 
     &fup95,                       ! Riv_F95_up 
      
     &cold(JP,1),                  ! Riv_qual_mean_up 
     &cold(JP,2),                  ! Riv_qual_sd_up 
     &cold(JP,6),                  ! Riv_CF_Corr 
     &cold(JP,21),                 ! Riv_load_mean_up
     &cold(JP,22),                 ! Riv_load_st_up
      
     &FE(IQ,1),                    ! Dis_f_Mean 
     &FE(IQ,2),                    ! Dis_f_sd 
     &CO2,                         ! Dis_fF_Corr 
     &PDEF(IQ),                    ! Dis_f_dist_type 
      
     &pollution data (IQ,JP,1),    ! Dis_qual_mean_orig 
     &pollution data (IQ,JP,2),    ! Dis_qual_sd_orig 
     &cold(JP,12),                 ! Dis_qual_q95 
     &EFCL(JP),                    ! Dis_cf_Corr 
     &PDEC(IQ,JP),                 ! Dis_c_dist_type 
      
     &cold(JP,29),                 ! Dis_conc_mean_orig 
     &cold(JP,30),                 ! Dis_conc_sd_orig 
     &cold(JP,13),                 ! Dis_load_mean_orig ++++++++++++++++++++++++
     &cold(JP,14),                 ! Dis_load_st_orig ++++++++++++++++++++++++++
      
     &FLOW(1),                     ! Riv_F_mean_ down 
     &FLOW(2),                     ! Riv_F95_down 
      
     &cold(JP,7),                  ! Riv_qual_mean_down 
     &cold(JP,8),                  ! Riv_qual_sd_down 
     &cold(JP,10),                 ! Riv_qual_q90_down 
     &cold(JP,9),                  ! Riv_qual_q95_down 
     &cold(JP,11),                 ! Riv_qual_q99_down 
     &cold(JP,23),                 ! Riv_load_mean_down_forw 
     &cold(JP,24),                 ! Riv_load_st_down_forw 
      
     &ICAL,                        ! Res_type_code 
     &run type description,        ! Res_type_desc ... backwards
     &kdecision(JP),               ! Discharge_option
     &targetdec,                   ! Outcome
     &dischargedec,                ! Discharge_decision
      
     &C(JP,1),                     ! Riv_qual_mean_back 
     &C(JP,2),                     ! Riv_qual_sd_back
     &C(JP,4),                     ! Riv_qual_q90_back
     &C(JP,3),                     ! Riv_qual_q95_back 
     &C(JP,5),                     ! Riv_qual_q99_back
     &cold(JP,25),                 ! Riv_load_mean_down_back 
     &cold(JP,26),                 ! Riv_load_st_down_back 
      
     &cold(JP,18),                 ! Dis_conc_mean_final 
     &cold(JP,19),                 ! Dis_conc_sd_final 
     &cold(JP,20),                 ! Dis_conc_95_final 
      
     &cold(JP,27),                 ! Dis_load_mean_final 
     &cold(JP,28),                 ! Dis_load_st_change 
     &changeload,                  ! Dis_load_mean_drop
 
     &changerload,deltaload        ! Riv_load_mean_down_drop

      if ( deltaload .gt. 0.002 ) then
      if ( ical .gt. 06 ) then
      call sort format1 (deltaload)
	call change colour of text (20) ! bright red
      write(*,5566)uname(KFEAT),dname(JP),valchars10
 5566 format('* Difference in change in river and ',
     &'discharge load ...',7x,a40,4x,a11,2x,a10,' %')
      call set screen text colour
      endif ! if ( ical .gt. 06 ) then
      endif ! if ( deltaload .gt. 0.002 )
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo

    2 format(
     &i3,',',                 ! Run_Type
     &a20,',',                ! Run_Description 
     &a37,',',                ! Feature_Name 
     &i6,',',                 ! Feature_No 
     &a16,',',                ! Reach_Name
     &i6,',',                 ! Reach_No 
     &a40,',',                ! GIS_ref 
     &a25,',',                ! Dis_Type_Name 
     &i3,',',                 ! Dis_Type_No 
     &a11,',',                ! Det_Name 
     &a4,',',                 ! Det_Units
     &i3,',',                 ! Det_Number 
     &1pe12.5,',',            ! Target 
     &a15,',',                ! Stat_Name
     &i2,',',                 ! Target_type_No
      
     &1pe12.5,',',            ! Riv_F_mean_up 
     &1pe12.5,',',            ! Riv_F95_up 

     &1pe12.5,',',            ! Riv_qual_mean_up 
     &1pe12.5,',',            ! Riv_qual_sd_up
     &e12.5,',',              ! Riv_CF_Corr 
     &1pe12.5,',',            ! Riv_load_mean_up
     &1pe12.5,',',            ! Riv_load_mean_up

     &1pe12.5,',',            ! Dis_f_Mean 
     &1pe12.5,',',            ! Dis_f_sd
     &e12.5,',',              ! Dis_fF_Corr
     &i2,',',                 ! Dis_f_dist_type

     &1pe12.5,',',            ! Dis_qual_mean_orig 
     &1pe12.5,',',            ! Dis_qual_sd_orig 
     &1pe12.5,',',            ! Dis_qual_q95_orig 
     &e12.5,',',              ! Dis_cf_Corr  
     &i2,',',                 ! Dis_c_dist_type 

     &1pe12.5,',',            ! Dis_conc_mean_orig 
     &1pe12.5,',',            ! Dis_conc_sd_orig 
     &1pe12.5,',',            ! Dis_load_mean_orig 
     &1pe12.5,',',            ! Dis_load_st_orig 

     &1pe12.5,',',            ! Riv_F_mean_down 
     &1pe12.5,',',            ! Riv_F95_down 

     &1pe12.5,',',            ! Riv_qual_mean_down 
     &1pe12.5,',',            ! Riv_qual_sd_down 
     &1pe12.5,',',            ! Riv_qual_q90_down 
     &1pe12.5,',',            ! Riv_qual_q95_down 
     &1pe12.5,',',            ! Riv_qual_q99_down 

     &1pe12.5,',',            ! Riv_load_mean_down_forw 
     &1pe12.5,',',            ! Riv_load_st_down_forw 
     &i2,',',                 ! Res_type_code 
     &a20,',',                ! Res_type_desc 
     &i2,',',                 ! Discharge_option 
     &a30,',',                ! Outcome 
     &a30,',',                ! Discharge_decision 

     &1pe12.5,',',            ! Riv_qual_mean_back 
     &1pe12.5,',',            ! Riv_qual_sd_back  
     &1pe12.5,',',            ! Riv_qual_q90_back  
     &1pe12.5,',',            ! Riv_qual_q95_back 
     &1pe12.5,',',            ! Riv_qual_q99_back  

     &1pe12.5,',',            ! Riv_load_mean_down_back  
     &1pe12.5,',',            ! Riv_load_st_down_back  

     &1pe12.5,',',            ! Dis_conc_mean_final 
     &1pe12.5,',',            ! Dis_conc_sd_final 
     &1pe12.5,',',            ! Dis_conc_95_final 

     &1pe12.5,',',            ! Dis_load_mean_final 
     &1pe12.5,',',            ! Dis_load_st_final 
     &1pe12.5,',',            ! Dis_load_mean_drop

     &1pe12.5,',')            ! Riv_load_mean_down_change
*     ##########################################################################
    3 format(
     &i3,17x,  '     Run_Type'/
     &a20,     '     Run_Description'/ 
     &a20,     '     Feature_Name'/
     &i6,14x,  '     Feature_No'/
     &a16,4x,  '     Reach_Name'/
     &i3,17x,  '     Reach_No'/
     &a20,     '     GIS_ref'/
     &a20,     '     Dis_Type_Name'/
     &i3,17x,  '     Dis_Type_No'/
     &a11,9x,  '     Det_Name'/
     &a4,16x,  '     Det_Units'/
     &i3,17x,  '     Det_Number'/ 
     &f15.3,5x,'     Target'/
     &a15,5x,  '     Stat_Name'/
     &i2,18x,  '     Target_type_No'/
      
     &f15.3,5x,'     Riv_F_mean_up'/
     &f15.3,5x,'     Riv_F95_up'/

     &f15.3,5x,'     Riv_qual_mean_up'/ 
     &f15.3,5x,'     Riv_qual_sd_up'/
     &f8.4,12x,'     Riv_CF_Corr'/
     &f15.3,5x,'     Riv_load_mean_up'/
     &f15.3,5x,'     Riv_load_st_up'/

     &f15.5,5x,'     Dis_f_Mean'/
     &f15.5,5x,'     Dis_f_sd'/
     &f8.4,12x,'     Dis_fF_Corr'/
     &i2,18x,  '     Dis_f_dist_type'/

     &f15.3,5x,'     Dis_qual_mean_orig'/ 
     &f15.3,5x,'     Dis_qual_sd_orig'/
     &f15.3,5x,'     Dis_qual_q95_orig'/
     &f8.4,12x,'     Dis_cf_Corr'/
     &i2,18x,  '     Dis_c_dist_type'/ 

     &f15.3,5x,'     Dis_conc_mean_orig'/
     &f15.3,5x,'     Dis_conc_sd_orig'/
     &f15.3,5x,'     Dis_load_mean_orig'/ 
     &f15.3,5x,'     Dis_load_st_orig'/

     &f15.3,5x,'     Riv_F_mean_down'/
     &f15.3,5x,'     Riv_F95_down'/

     &f15.3,5x,'     Riv_qual_mean_down'/ 
     &f15.3,5x,'     Riv_qual_sd_down'/
     &f15.3,5x,'     Riv_qual_q90_down'/
     &f15.3,5x,'     Riv_qual_q95_down'/
     &f15.3,5x,'     Riv_qual_q99_down'/
     &f15.3,5x,'     Riv_load_mean_down_forw'/
     &f15.3,5x,'     Riv_load_st_down_forw '/

     &i2,18x,  '     Res_type_code'/
     &a20,     '     Res_type_desc'/
     &i2,18x,  '     Discharge_option'/
     &a20,     '     Outcome'/
     &a20,     '     Discharge_decision'/

     &f15.3,5x,'     Riv_qual_mean_back'/
     &f15.3,5x,'     Riv_qual_sd_back'/
     &f15.3,5x,'     Riv_qual_q90_back'/
     &f15.3,5x,'     Riv_qual_q95_back'/
     &f15.3,5x,'     Riv_qual_q99_back'/
     &f15.3,5x,'     Riv_load_mean_down_back'/ 
     &f15.3,5x,'     Riv_load_st_down_back'/ 

     &f15.3,5x,'     Dis_conc_mean_final'/
     &f15.3,5x,'     Dis_conc_sd_final'/
     &f15.3,5x,'     Dis_conc_95_final'/

     &f15.3,5x,'     Dis_load_mean_final'/
     &f15.3,5x,'     Dis_load_st_final'/
     &f15.3,5x,'     Dis_load_mean-drop'/

     &f15.3,5x,'     Riv_load_mean_down_drop',20x,f12.8)
 
      return
      end



*     print out the input data on discharge quality ----------------------------
      subroutine assess the need to track the discharge
      include 'COM.FOR'
*     check whether the discharge should be tracked ----------------------------
*     calculate the proportion of the load added by the discharge --------------
      if ( ical13 .eq. 1 ) return
*     ==========================================================================
      mark works = 0
      biggest = 0.0
      baggest = 0.0
*     =========================================================================F
      if ( FE(IQ,1) .gt. 0.00000001 ) then
      EF = FE(IQ,1)
      do idet = 1, ndet
*     -------------------------------------------------------------------------G
      if ( QTYPE (idet) .ne. 3 .and. QTYPE (idet) .ne. 5 ) then
      if ( detype (idet) .ne. 104 ) then
      EP = 0.0
	FP = 0.0
*     rough estimate of load ---------------------------------------------------
      if ( PDEC (IQ,idet) .eq. 6 .or. PDEC (IQ,idet) .eq. 7 .or.
     &     PDEC (IQ,idet) .eq. 9 ) then
      EL = pollution data(IQ,idet,1)
      else
      EL = EF * pollution data(IQ,idet,1)
      endif
	tergit = RQS (1,idet)
	if ( tergit .lt. 0.000001 ) tergit = 500
*     add to the total load accumulated so far ---------------------------------
      ED = TELODE2(idet,i13) + EL
*     compute the proportion represented by the new load -----------------------
      if ( ED .gt. 0.0000001 ) EP1 = EL / ED
*     compute the river concentration indicated represented by the new load ----
	if ( FLOW(1) .gt. 0.00001 ) FP = (EL / FLOW(1)) / tergit
*     isolate the biggest proportion across all the determinands ---------------
      biggest = ( amax1 ( EP, biggest) )
      baggest = ( amax1 ( FP, baggest) )
      endif ! if ( detype (idet) .ne. 104 )
	endif ! if ( QTYPE (idet) .ne. 3 etc ...
      enddo ! do idet = 1, ndet
      biggest = 100.0 * biggest
      baggest = 100.0 * baggest
      endif ! if ( FE(IQ,1) .gt. 0.00000001 )

*     initially add to the counter of works ------------------------------------
      kount works = kount works + 1
      need works = need works + 1
      mark works = 1
 	call change colour of text (20) ! bright red
      call set screen text colour
*     check whether the load is big enough to track ----------------------------
      if ( baggest .lt. bagset .and. FLOW(1) .gt. 0.0001 .and. 
     &     baggest .gt. 1.0e-8) then
      if ( ifbatch .eq. 0 .and. kerror .eq. 1 ) then
 	call change colour of text (14) ! bright yellow
      call sort format 1 (baggest) ! bright red
      write( *,4631)valchars10
 4631 format('* Next effluent too small for back-tracking ',
     &'impacts (',a10,' %)')
      call set screen text colour
      write(33,4611)UNAME(KFEAT),valchars10
      write(09,4611)UNAME(KFEAT),valchars10
 4611 format(110('-')/'The effluent load too small for back-tracking ',
     &'the impacts up the rivers ... ',a40/
     &'Its contribution is 'a10,' per cent'/110('-'))
	endif !  if ( ifbatch .eq. 0 ) then
	kount works = kount works - 1
	need works = need works - 1
	kill works = kill works + 1
      mark works = 0
	else !  if ( biggest .lt. 0.001 )
      if ( kount works .gt. NUED ) then
*     if ( ifbatch .eq. 99 ) then
*     call change colour of text (12) ! orange
*     write( *,4531)NUED,need works
*4531 format('* Too many effluent discharges for back-tracking ',
*    &'the impacts ... the limit is ',i5,' (Need ',i5,')')
*     call set screen text colour
*     call sort format 1 (baggest)
*     write(33,4511)NUED,need works,valchars10
*     write(01,4511)NUED,need works,valchars10
*     write(09,4511)NUED,need works,valchars10
*4511 format(110('-')/'Too many effluent discharges for back-tracking ',
*    &'the impacts ... the limit is ',i5,' (Need ',i5,')'/
*    &'The next discharge is not being tracked ... it ',
*    &'contributes ',a10,' per cent to downstream quality'/110('-'))
*     endif
	kount works = kount works - 1
      mark works = 0
      else ! if ( kount works .gt. NUED )
	identify works ( kount works ) = feeture
	endif !  if ( kount works .gt. NUED )
*     --------------------------------------------------------------------------
	endif !  if ( biggest .lt. 0.001 )
*     --------------------------------------------------------------------------
      return
      end




*     print out the input data on discharge quality ----------------------------
      subroutine write information on effluent flow 
      include 'COM.FOR'
      
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	write(01,2055)IQ,PDEF(IQ)
	write(21,2055)IQ,PDEF(IQ)
	write(31,2055)IQ,PDEF(IQ)
 2055 format(77('=')/
     &'Flow discharged from works: data set:',I6,' Type:',i2)
	endif

*     note whether the distribution is non-parametric --------------------------
      if ( PDEF(IQ) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 1970
      enddo
	goto 1974
 1970 continue
	if ( nobigout .le. 0 ) then
      write(01,1965) flnamesmall(3,icod)
      write(21,1965) flnamesmall(3,icod)
      write(31,1965) flnamesmall(3,icod)
 1965 format(
     &'Non-parametric distribution ... File: ',a64/77('-'))
      endif
      endif
 1974 continue

      endif

*     note whether the data are monthly distributions --------------------------
      if ( PDEF(IQ) .eq. 5 ) then

*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 1990
      enddo
	goto 1994
 1990 continue

	if ( nobigout .le. 0 ) then
      write(01,1995) FLMONTHsmall(3,icod)
      write(21,1995) FLMONTHsmall(3,icod)
      write(31,1995) FLMONTHsmall(3,icod)
      endif
 1995 format(77('-')/'These data are expressed as ',
     &'monthly distributions ... File: ',a64/77('-'))

*     open and read the file ---------------------------------------------------
      open(11,FILE = flmonth(3,icod), STATUS='OLD')
*     read the file containing the monthly data --------------------------------
      call read monthly data (3,icod) ! discharge flow - type 5

      write(01,4398)
 4398 format(
     &'Monthly input data on discharge flow ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x'  Deviation','           '/77('-'))

      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)

	endif
 1994 continue

    1 continue
      if ( JSKIP .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      call sort format 2 (FE(IQ,1),FE(IQ,1))
	write(01,1012)valchars10,funit,valchars11,funit
	write(21,1012)valchars10,funit,valchars11,funit
	write(31,1012)valchars10,funit,valchars11,funit
 1012 format(77('=')/'Flow discharged as effluent:',20X,
     &' Annual mean =',a10,1x,a4/41X,' Standard Deviation =',a10,
     &1x,a4/77('='))
      endif
      endif
	endif

      return
      end




*     print out the input data on discharge quality ----------------------------
      subroutine write effluent discharge quality
      include 'COM.FOR'

      if ( JSKIP .eq. 1 .or. ical .eq. 1 ) goto 1
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,2055)IQ
      endif
 2055 format('Quality discharged from works: data set:',I6)

*     loop on the determinands -------------------------------------------------
      do 6926 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 6926
*     --------------------------------------------------------------------------
      if ( PDEC(IQ,J) .lt. 4 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      write(21,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
 1924 format(77('-')/a11,44X,' Mean =',a10,1x,A4/
     &42X,'Standard deviation =',a10,1x,A4)
      endif
      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDEC(IQ,J) .lt. 4 )
*     --------------------------------------------------------------------------
*     note whether the data are for loads --------------------------------------
      if ( PDEC(IQ,J) .eq. 6 ) then ! normal loads
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      write(21,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
 8924 format(77('-')/a11,40X,'Mean load =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4)
      endif
      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDEC(IQ,J) .eq. 6 )
*     --------------------------------------------------------------------------
      if ( PDEC(IQ,J) .eq. 7 ) then ! log normal loads
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      write(21,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      endif
      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDEC(IQ,J) .eq. 7 )
*     --------------------------------------------------------------------------

*     note whether the distribution is non-parametric --------------------------
      if ( PDEC(IQ,J) .eq. 4 .or. PDEC(IQ,J) .eq. 9 ) then
*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 2,i, J + 1 ) .eq. IQ ) goto 1970
 1969 continue
	goto 1974
 1970 continue
      if ( PDEC(IQ,J) .eq. 9 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      write(21,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      endif
	write(01,1965) flnamesmall(4,icod)
	write(21,1965) flnamesmall(4,icod)
	write(31,1965) flnamesmall(4,icod)
 1965 format(
     &'Non-parametric distribution ... File: ',a64,19('.')/77('-'))
      endif
      endif
      if ( PDEC(IQ,J) .eq. 4 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      write(21,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 ) then
      write(31,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J)
      endif
 	write(01,1965) flnamesmall(4,icod)
 	write(21,1965) flnamesmall(4,icod)
 	write(31,1965) flnamesmall(4,icod)
      endif
      endif
      endif
 1974 continue

*     note whether the data are monthly (type 5) -------------------------------
      if ( PDEC(IQ,J) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do 1989 i = 1, M8
      icod = i
      if ( iseasp ( 2, i, J + 1 ) .eq. IQ ) goto 1990
 1989 continue
	goto 1994
 1990 continue
	if ( nobigout .le. 0 ) then
      write(01,1966) FLMONTHsmall(4,icod)
      write(21,1966) FLMONTHsmall(4,icod)
      write(31,1966) FLMONTHsmall(4,icod)
      endif
 1966 format(77('-')/'Discharge quality expressed as monthly ',
     &'distributions ... '/
     &'File: ',a64/77('-'))

*     open and read the file ---------------------------------------------------
      open(11,FILE = flmonth(4,icod), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (4,icod) ! discharge quality - type 5

      write(01,4398)
 4398 format(77('-')/
     &'Monthly input data on discharge quality ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)
	endif ! if ( PDEC(IQ,J) .eq. 5 ) ... monthly data
 1994 continue

 6926 continue ! end of loop on determinands -----------------------------------

    1 continue
      return
      end




*     adjust river flow for effect of a constant abstraction -------------------
*     but leave the specified flow in the river --------------------------------
*     --------------------------------------------------------------------------
      subroutine abstraction of flow
      include 'COM.FOR'

*     initialise the abstracted loads ------------------------------------------
      do jdet = 1, ndet
      do L13 = 1, N13
	NSM (jdet,L13) = 0
      ABLOAD (jdet,L13) = 0.0
	enddo
      enddo

*     identify the number of the flow data-set ---------------------------------
      IF = JF(KFEAT)

      if ( JSKIP .ne. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,1001)F(IF,1),FUNIT
      write(21,1001)F(IF,1),FUNIT
 1001 format(77('-')/'Flow abstracted from river:  ',F6.1,1x,a4)
      if ( F(IF,2) .gt. 0.001 ) then
      write(01,1002)F(IF,2),FUNIT
      write(21,1002)F(IF,2),FUNIT
 1002 format(77('-')/10x'"Hands-off" flow:  ',F6.1,1x,a4/77('-'))
      endif
      endif
      endif

      if ( F(IF,1) .lt. 0.00001 ) return

*     calculate the abstracted flow and the resulting flow downstream ----------
      do 1 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot
      ABSFLOW = 0.0
      ABSLOAD = 0.0
      RESFLOW = 0.0
      STAFLOW = 0.0

*     check that the flow is bigger than the "hands-off" flow ------------------
      if ( FMS(IS) .gt. F(IF,2) ) then
      STAFLOW = FMS(IS)
*     flow in river after the abstraction --------------------------------------
      RESFLOW = AMAX1 ( F(IF,2), FMS(IS) - F(IF,1) )
*     the amount of flow abstracted --------------------------------------------
      ABSFLOW = FMS(IS) - RESFLOW
      FMS(IS) = RESFLOW
      endif

*     calculate the abstracted load --------------------------------------------
      do jdet = 1, ndet
      if ( qtype (jdet) .ne. 4 ) then
      ABSLOAD = ABSFLOW * CMS(jdet,IS)   
      do J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
      ABLOAD (jdet,K13) = ABLOAD (jdet,K13) + ABSLOAD
      NSM (jdet,K13) = NSM (jdet,K13) + 1
	enddo
	endif
      enddo
    1 continue
      do jdet = 1, ndet
      if ( qtype (jdet) .ne. 4 ) then
	ABLOAD (JDET,I13) = ABLOAD (JDET,I13) / float(NS)
      do J13 = 2, K13
      if ( NSM(jdet,J13) .gt. 1 ) then
	ABLOAD (JDET,J13) = ABLOAD (JDET,J13) / float(NSM(JDET,J13))
	endif
	enddo
      endif
      enddo

      do jdet = 1 , ndet
      kprune det = jdet
*     accumulate total loads removed by abstractions ===========================
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1)
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1)
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0
      if ( abload(jdet,J1) .gt. 0.0 ) then
      if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(jdet,J1) - abload  (jdet,J1)) 
     &                /  TGLODE2(jdet,J1)
      endif
	endif
      endif ! if ( abload(jdet,J1) .gt. 0.0 )
	enddo ! do J1 = 1, nx

*     trim back the loads for abstractions =====================================
      call scale loads after losses
      enddo

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
      call calculate summaries of river flow
      if ( nobigout .le. 0 ) then
      endif

      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at abstraction
     
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting
      endif

      return
      end


*     river regulation ---------------------------------------------------------
      subroutine river regulation
      include 'COM.FOR'

      IF = JF(KFEAT) ! flow data-set 
      if ( IF .le. 0 ) return
      IQ = JQ(KFEAT) ! quality data-set

      if ( JSKIP .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,4)F(IF,2),FUNIT
    4 format(77('-')/
     &'Flow maintained in river:  ',F6.1,1x,a4/77('-'))
      endif

      if ( IQ .ge. 1 ) then
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2044)IQ
 2044 format(77('-')/
     &'Quality of regulation water: Data Set:',I6)
      call write mean and standard deviation
      endif
      endif

      do L13 = 1, N13
      do JP = 1, ndet
	NSM (JP,L13) = 0
      ELOAD (JP,L13) = 0.0
      enddo
      enddo
    
*     loop on the number of shots ----------------------------------------------
      do 1 is = 1,NS
      imonth = qmonth (is) ! set the month for this shot

*     compute the addition to river flow needed to make up the flow to the -----
*     required level, F(IF,2) --------------------------------------------------
      FADD = AMAX1 ( 0.0, F(IF,2) - FMS(is) )

*     adjust the river quality (if necessary and requested) --------------------
*     check for specified quality data for the inflow---------------------------

      if ( IQ .eq. 0 ) then
      K13 = imonth + 1
      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then

      if ( FADD .gt. 1.0E-10 ) then
	ELOAD (JP,I13) = ELOAD (JP,I13) + FADD * CMS (JP, IS)
 	endif
	NSM (JP,I13) = NSM   (JP,I13) + 1

      if ( FADD .gt. 1.0E-10 ) then
	ELOAD (JP,K13) = ELOAD (JP,K13) + FADD * CMS (JP, IS)
 	endif
	NSM (JP,K13) = NSM   (JP,K13) + 1

	endif
      enddo
	goto 1
	endif

*     here are quality data for the inflow -------------------------------------
*     loop on the determinands -------------------------------------------------
      do 8 JP = 1, ndet
      if ( QTYPE (JP) .eq. 4 ) goto 8
      ECM = quolity data(IQ,JP,1)
      ECS = quolity data(IQ,JP,2)

      IQDIST = PDEC(IQ,JP) ! type of distribution ------------------------------

      EC = ECM
      if ( ECS .lt. 1.0E-10 ) goto 11

      EC3 = 0.0
      if ( IQDIST .eq. 3 ) EC3 = quolity data(IQ,JP,3) ! shift -----------------

*     compute the mean and standard deviation of the logged variables ----------
      call get the summary statistics of discharge quality (ECM, ECS)

      call get the correlated random numbers (IS, R1, R2, R3, R4)

*     quality of added regulation water ----------------------------------------
      EC = RGCALC (R4, ECM)

*     perform mass balance -----------------------------------------------------
   11 continue
      if ( FADD .gt. 1.0E-10 ) then
      fxx = FMS(is) + FADD
	if ( fxx .gt. 1.0e-8 ) then
      CMS(JP,is) = ( CMS(JP,is) * FMS(is) + EC * FADD) / fxx
      
      LMS(17,JP,IS) = ( (FMS(is) * LMS(17,JP,IS)) + (EC * FADD) ) / fxx

*     dilute the remaining contributions ---------------------------------------
      do ip = 6, nprop 
      if ( ip .ne. 17 ) then ! exclude streams (2) from loop
      LMS(IP,JP,IS) = (FMS(is) * LMS(IP,JP,IS)) / (fxx)
      endif
      enddo
	endif ! if ( RF + EF .gt. 1.0e-8 )

	ELOAD (JP,I13) = ELOAD (JP,I13) + FADD * EC
	NSM (JP,I13) = NSM   (JP,I13) + 1
      K13 = imonth + 1
	ELOAD (JP,K13) = ELOAD (JP,K13) + FADD * EC
	NSM (JP,K13) = NSM   (JP,K13) + 1
      endif ! if ( FADD .gt. 1.0E-10 )

    8 continue
    1 continue


      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then
      ELOAD (JP,i13) = ELOAD (JP,i13) / float (NS)
      if ( munthly structure .eq. 1 ) then ! fill monthly loads
      do J13 = 2, N13
      if ( NSM  (JP,J13) .gt. 0 ) then
      ELOAD (JP,J13) = ELOAD (JP,J13) / float (NSM(JP,J13))
	endif
	enddo
	endif
	endif
	enddo


      do is = 1,NS
      current = FMS(is)
*     compute the addition to river flow needed to make up the flow to the -----
*     required level, F(IF,2) --------------------------------------------------
      FADD = AMAX1 ( 0.0, F(IF,2) - FMS(is) )
*     compute the new flow, after regulation -----------------------------------
      FMS(is) = AMAX1 ( current, F(IF,2) )
      enddo


      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
      call calculate summaries of river flow
      if ( nobigout .le. 0 ) write(01,1023)uname(KFEAT)

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(34,1023)uname(KFEAT)
 1023 format(77('-')/
     &'River quality just downstream of Regulation Point ... ',a40/
     &77('-'))
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at regulation point
      endif

      call write data for graph plotting
	endif

      return
      end



*     set the regulation quality -----------------------------------------------
      function RGCALC (R,XM)
      include 'COM.FOR'

      RGCALC = 0.0
*     ------ 0  1  2  3  4  5  6  7  8  9  10 distribution types ---------------
      goto ( 1, 2, 3, 3, 9, 9, 2, 3, 9, 9, 4 ), IQDIST + 1

*     constant value -----------------------------------------------------------
    1 RGCALC = XM
      return

*     normal distribution ------------------------------------------------------
    2 RGCALC = Vnorm (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     log-normal distribution --------------------------------------------------
    3 RGCALC = Vlogn (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     power curve parametric distribution 99999999999999999999999999999999999999
    4 RGCALC = Vlogn (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     non-parametric distribution ----------------------------------------------
    9 continue
      RGCALC = XM
      return
      end



*     weir ---------------------------------------------------------------------
      subroutine weir
      include 'COM.FOR'
      IQ = JQ(KFEAT)
      if ( IPRINT .eq. 1 )return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1015)
 1015 format(77('-')/
     &'Calculated river quality downstream of weir ...'/77('-'))
      call get summaries and write out loads and quality (0)
	call write out monthly river quality (2) ! at weir
	endif
      return
      end



*     initialise variables for mass balance ------------------------------------
      subroutine initialise data for mass balance
      include 'COM.FOR'

      cut off zero flow = 0.0 

*     correlation coefficients for mass balance --------------------------------
      CO1 = 0.0 ! initialise river flow on river quality -----------------------
      CO2 = 0.0 ! initialise river flow on discharge flow ----------------------
      CO3 = 0.0 ! initialise river flow on discharge quality ! 44444444444444444
      CO4 = 0.0 ! initialise river quality on discharge flow ! 44444444444444444
      CO5 = 0.0 ! initialise discharge (or tributary) flow on discharge quality-
      CO6 = 0.0 ! initialise river quality on discharge quality ! 44444444444444

*     initialise ---------------------------------------------------------------
      do jdet = 1, ndet
      kdecision(jdet) = 0
	do j13 = 1,N13
      XLOAD (jdet,1,j13) = 0.0
      XLOAD1 (jdet,1,j13) = 0.0 ! dissolved metal
      XLOAD2 (jdet,1,j13) = 0.0 ! solid metal
	NSM (jdet, j13) = 0
      enddo
	enddo

      do jdet = 1, ndet ! calculate the correlation of flow on quality ---------
      if ( QTYPE (jdet) .ne. 4 ) then      
      kdecision(jdet) = 0
      do is = 1, NS
      Creg(1,IS) = FMS(IS) ! store the upstream river flow for regression ------
      Creg(4,IS) = CMS(jdet,is) ! upstream river quality for regression --------
      enddo
      k1 = 1 ! river flow
      k2 = 4 ! river quality
*     if ( IFDIST .eq. 1 .or. IFDIST .eq. 4 .or. IFDIST .eq. 9 ) then 
*     k1 = -k1 ! set up for normal distributions ! Richard III
*     endif
*     if ( IQDIST .eq. 1 .or. IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then
*     k2 = -k2 ! set up for normal distributions
*     endif
      RRR=0.999
      call regres the calculated shots (k1,k2,RRR)
      COD1 (jdet) = RRR
*     write(33,1)RRR,dname(jdet)
*   1 format(77('-')/
*    &'Shot correlation of river quality on river flow =',F8.4,
*    &' for ',a11/77('-'))
      endif ! if ( QTYPE (jdet) .ne. 4 )
      enddo ! do jdet = 1, ndet
      
      EFM = 0.0
      EFS = 0.0
      EF5 = 0.0
      EF3 = 0.0
      ECM = 0.0
      ECS = 0.0
      EF3 = 0.0
	E80 = 0.0
      return
      end


      subroutine load the upstream flow and quality 
      include 'COM.FOR'
      do is = 1, NS
      cms(JP,IS) = ucms(JP,IS)
	do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      lms(ip,JP,IS) = ulms(ip,JP,IS)
	enddo
      fms(IS) = ufms(IS)
      enddo
	call get summaries of river quality from the shots
	return
	end


      subroutine write GIS data to channel 42 43 24 and 25
      include 'COM.FOR'
	character*3 place

*     write to the GIS file ----------------------------------------------------
      num = JT(KFEAT)
	ireech = jreach(kfeat)
	JX = KFEAT

      place = 'd-s'
	if ( num .eq. 1 .or. num .eq. 4 ) place = 'at '
	if ( num .eq. 6 ) place = 'at '
	if ( num .eq. 25 .or. num .eq. 27 .or. num .eq. 29 ) goto 99
	if ( num .eq. 31 .or. num .eq. 33 .or. num .eq. 35 ) goto 99
	if ( num .eq. 37 .or. num .eq. 40 .or. num .eq. 42 ) goto 99
	if ( num .eq. 46 .or. num .eq. 48 ) goto 99
	if ( num .eq. 13 .or. num .eq. 15 ) goto 99

      write(42,244)ireech,GISCODE(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &COB(1,1),COB(2,1),COB(3,1),COB(4,1)
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River FLOW",',',A4,',',9(1PE11.4,','))

      do J = 1, MP10
      write(42,245)ireech,GISCODE(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),Dname(J),Units(J),
     &(CL(jcp,J),jcp=1,12),(CD(jcp,J),jcp=1,12),RQO(J),
     &confail(J),propeff2(J),
     &COB(1,J+1),COB(2,J+1),COB(3,J+1),COB(4,J+1),
     &(class limmits (ic,J),in class (ic,J),ic=1,NC),
     &Total length dets 00(J),Total length dets 50(J),
     &Total length dets 95(J)
  245 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
     
*     write the extra records for partitioned metals ----------------------------- 
	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
	xzero = 0.0
      write(42,845)ireech,GISCODE(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
      write(42,845)ireech,GISCODE(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
	endif ! dissolved and solid metal
      enddo ! loop on determinands
      
      write(42,247)ireech,GISCODE(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &55(1PE11.4,','),11(i2,','),i4,',')

   99 continue

      GISCODE last = GISCODE(JX)
      return
      end



*     NEGATIVE DISCHARGE ONE (Feature type 18) ---------------------------------
*     adjust river flow for the effect of abstraction --------------------------
*     The abstraction data are specified as mean and 95-percentile -------------
*     (NEGATIVE DISCHARGE TWO  uses mean and standard deviation) ---------------

      subroutine negative discharge one ! abstraction 18
      include 'COM.FOR'
      dimension A1load(MP10,13),A2load(MP10,13)
	dimension abmean(MP10),abstdev(MP10),absq95(MP10)
	dimension abvalues(MP10,MS),Y(MS),abmean2(MP10)

*     initialise the abstracted loads ------------------------------------------
      do jdet = 1, ndet
      A1LOAD(jdet,i13) = 0.0
      A2LOAD(jdet,i13) = 0.0
      abmean(jdet) = 0.0
      abmean2(jdet) = 0.0
	abstdev(jdet) = 0.0
      do is = 1, NS
      abvalues(jdet,IS) = 0.0
      enddo 
      do L13 = 1, N13
	NSM (jdet,L13) = 0
      ABLOAD (jdet,L13) = 0.0
	enddo
      enddo ! initialise the abstracted loads ----------------------------------

*     identify the code number for the river flow data-set ---------------------
      IF = JF(KFEAT)
      if ( IF .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
      if ( iscreen .lt. 3 ) write( *,1020)KFEAT
      write(09,1020)KFEAT
      write(33,1020)KFEAT
 1020 format(/'*** Abstraction data specified for Feature number',I6,
     &' do not exist')
      if ( nobigout .le. 0 ) write(01,1006)
      if ( iscreen .lt. 3 ) write( *,1006)
 1006 format('*** No abstraction made ***'/80('-'))
      return
      endif

*     default correlation of abstracted flow on main river flow ----------------
      CO2 = 0.0 ! correlation of abstracted flow on main river flow ------------
      if ( F (IF,MO) .ne. -9.9 ) CO2 = F (IF,MO) ! special correlation ---------
      call set up the correlation coefficients ! 4444444444444444444444444444444
      IFDIST = PDRF(IF) ! the type of distribution -----------------------------

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      if ( IFDIST .gt. 0 ) then ! constant flow --------------------------------
      call sort format 3 (F(IF,1),F(IF,2),CO2)
      write(01,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
      write(21,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
 2012 format(77('-')/'Flow to be removed from the river:',20X,
     &'  Mean =',a10,1X,A4/
     &46x,'95% exceedence =',a10,1x,a4/
     &46x,'   Correlation =',a10/77('-'))
      else
      call sort format 1 (F(IF,1))
      write(01,2812) valchars10,FUNIT
      write(21,2812) valchars10,FUNIT
 2812 format(77('-')/'Flow to be removed from the river:',18x,
     &'Constant =',a10,1X,A4/77('-'))
      endif
      endif
      endif
      
*     note whether the distribution is non-parametric --------------------------
      if ( IFDIST .eq. 4 ) then ! non-parametric -------------------------------
      if ( nobigout .le. 0 ) write(01,1965) 100.0 * CO2
 1965 format(77('-')/
     &'These data were extracted from a non-parametric distribution ',
     &'...'/'The flows are provided as a river flow data set ...'/
     &'Correlation with main river flow =',f5.2,' per cent ...')
*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 1970
 1969 continue
      write(01,7813) 
      write( *,7813) 
      write(09,7813) 
      write(33,7813) 
 7813 format('There is no non-parametric data file ',
     &'for an abstraction of type 18 ...'/77('-'))
      call stop ! there is no non-parametric data file 
 1970 continue
      if ( nobigout .le. 0 ) write(01,7163) flnamesmall(1,icod)
 7163 Format('File: ',a64/77('-'))
      endif ! non-parametric abstractions

*     note whether the data are monthly ----------------------------------------
      if ( IFDIST .eq. 5 ) then ! monthly --------------------------------------
      if ( nobigout .le. 0 ) write(01,1966)
 1966 format('The abstraction is represented by monthly data ...')
*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
      write(01,7863) 
      write( *,7863) 
      write(09,7863) 
      write(33,7863) 
 7863 format('There is no monthly data file ',
     &'for an abstraction of type 18 ...'/77('-'))
      call stop ! there is no monthly data file
 2970 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
 7363 format('File: ',a64/77('-'))
      endif ! monthly data

      if ( F(IF,1) .gt. 1.0E-10 ) then ! check for non-zero mean

*     deal with for the log-normal distribution of abstracted flows ------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log normal distribution
      EFM = F(IF,1) ! mean abstracted flow (river type)
      EF5 = F(IF,2) ! 95-percentile
      EFS = 0.0 ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3)
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile ------------------------------------------------------------
      TEST = AMAX1 ((EF5+EF3),1.0E-8)
      GEFS = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST)
      if ( GEFS .le. 0.0 ) then
      write( *,9099)UNAME(KFEAT)
      write(01,9099)UNAME(KFEAT)
 9099 format(///'### Flow error for Feature called - ',A32/
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if GEFS .le. 0.0
      GEFS = SQRoot(1046,GEFS) - 1.6449
      EFS = EXP(GEFS*GEFS) - 1.0 ! standard deviation
      if (EFS .le. 1.0E-10) then
      write( *,9899)UNAME(KFEAT)
      write(01,9899)UNAME(KFEAT)
 9899 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if (EFS .le. 1.0E-10)
      EFS = EFM * SQRoot(1047,EFS)
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      GM = GEFM
      GS = GEFS
      RR3 = EF3
      call bias in log normal negative river flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EF5/BS(3)
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1299)BM(3),BS(3)
 1299 format(
     &'Corrections for sampling errors: log-normal removals ',
     &'(river-type)',
     &' ...'/77('-')/'Mean          =',F8.3/'95-percentile =',F8.3)
      call generate removed log normal river flows           
      endif ! if ( MONF .gt. 1 )
      endif ! log normal -------------------------------------------------------

*     deal with for the log-normal distribution of abstracted flows 999999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( IFDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      EFM = F(IF,1) ! mean abstracted flow (river type)
      EF5 = F(IF,2) ! 95-percentile
      EFS = 0.0 ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3)
*     compute mean and standard deviation of logged flows from the mean and 9999
*     95-percentile 999999999999999999999999999999999999999999999999999999999999
      TEST = AMAX1 ((EF5+EF3),1.0E-8)
      GEFS = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST)
      if ( GEFS .le. 0.0 ) then
      write( *,9299)UNAME(KFEAT)
      write(01,9299)UNAME(KFEAT)
 9299 format(///'### Flow error for Feature called - ',A32/
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if GEFS .le. 0.0
      GEFS = SQRoot(1046,GEFS) - 1.6449
      EFS = EXP(GEFS*GEFS) - 1.0 ! standard deviation
      if (EFS .le. 1.0E-10) then
      write( *,9829)UNAME(KFEAT)
      write(01,9829)UNAME(KFEAT)
 9829 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if (EFS .le. 1.0E-10)
      EFS = EFM * SQRoot(1047,EFS)
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      GM = GEFM
      GS = GEFS
      RR3 = EF3
      call bias in log normal negative river flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EF5/BS(3)
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1229)BM(3),BS(3)
 1229 format(
     &'Corrections for sampling errors: log-normal removals ',
     &'(river-type)',
     &' ...'/77('-')/'Mean          =',F8.3/'95-percentile =',F8.3)
      call generate removed log normal river flows           
      endif ! if ( MONF .gt. 1 )
      endif ! if ( IFDIST .eq. 10 ) power curve parametric distribution 99999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999


      if ( IFDIST .eq. 1 ) then ! normal distribution for abstractions
      EFM = F(IF,1) ! mean abstracted flow (river type)
      EF5 = F(IF,2) ! 95-percentile
      EF3 = 0.0 ! shift
      EFS = (EFM-EF5) / 1.6449 ! standard deviation
	if ( nobigout .le. 0 ) then
      call sort format 3 (EFM,EF5,EFS)
      write(01,2512) valchars10,FUNIT,valchars11,FUNIT,
     &valchars12,FUNIT
      write(21,2512) valchars10,FUNIT,valchars11,FUNIT,
     &valchars12,FUNIT
 2512 format(77('-')/'Flow to be removed from the river:',20X,
     &'  Mean =',a10,1X,A4/
     &46x,'95% exceedence =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/77('-'))
      endif ! if ( nobigout .le. 0 )
      GM = EFM
      GS = EFS
      RR3 = EF3
      call bias in normal negative discharge flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3)
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,52)BM(3),BS(3)
   52 format(
     &'Corrections for sampling errors: normal removals'/77('-')/
     &'Mean',15x,'=',f8.3/'Standard deviation =',F8.3)
      call generate removed normal river flows           
      endif ! if ( MONF .gt. 1 )
      endif ! normal distribution 

      if ( IFDIST .eq. 0 ) then ! constant flow --------------------------------
      EFS = 0.0
      EFM = F(IF,1)
      do is = 1, NS ! calculate flows for feature 18
      FTMS (IS) = EFM
      enddo ! calculate flows
      endif ! constant flow ----------------------------------------------------

      if ( IFDIST .eq. 4) then ! non-parametric distribution -------------------
      call set up the shots for non parametric stream flow ! feature 18
      if ( MONF .gt. 1 ) call shots for removed flows
      endif ! non-parametric distribution --------------------------------------

      if ( IFDIST .eq. 5) then ! monthly data ----------------------------------
      call set up monthly data for negative discharge
      if ( MONF .gt. 1 ) call shots for removed flows
      endif ! monthly data -----------------------------------------------------
      
      call list the correlation coefficients

*     if ( MONF .gt. 1 ) call calculate summaries for removed flows

*     prepare to abstract the flows --------------------------------------------
      FACT = -1.0
*     prepare to count the shots where the abstraction is greater than zero ----
	numflows = 0

*     start the abstraction ----------------------------------------------------
      do 2000 IS = 1, NS ! loop through the shots 
      ABSFLOW = 0.0
*     get the correlated random numbers ----------------------------------------
*     call get the correlated random numbers (IS, R1, R2, R3, R4)
*     retrieve the flow of the upstream river ----------------------------------
      RF = FMS (IS)
*     and retrieve the upstream river quality ----------------------------------
*     this if we want to abstract conditionally on river quality ---------------
*     RC = CMS ( JP, IS )

*     get the shot for the abstraction ... the value of EF ---------------------
	EF = FTMS(IS)
*     now do the substraction --------------------------------------------------
      START FLOW = RF
      HANDS OFF = 0.0
      REM FLOW = amax1 (HANDS OFF, RF + FACT * EF)
      EF = START FLOW - REM FLOW
      FMS(IS) = REM FLOW
      if ( EF .gt. 1.0e-10) numflows = numflows + 1

*     calculate the abstracted load --------------------------------------------
      do 3000 jdet = 1, ndet
      if ( QTYPE (jdet) .ne. 4) then 
      do 3050 J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
      ABLOAD(jdet,K13) = ABLOAD (jdet,K13) + EF * CMS(jdet,IS) 
      A1LOAD(jdet,K13) = A1LOAD(jdet,K13) + START FLOW * CMS(jdet,IS)
      A2LOAD(jdet,K13) = A2LOAD(jdet,K13) + FMS(IS) * CMS(jdet,IS)
      NSM (jdet,K13) = NSM (jdet,K13) + 1
 3050 continue

*     prepare to calculate the mean of the abstracted quality ------------------
      if ( EF .gt. 1.0e-10 ) then
      ABMean (jdet) = ABMean (jdet) + CMS(jdet,IS)
      ABStdev(jdet) = ABStdev(jdet) + CMS(jdet,IS) * CMS(jdet,IS)
*     store the abstracted quality ---------------------------------------------
      abvalues(jdet,numflows) = CMS (jdet, numflows)
      endif
	endif
 3000 continue
*     end of loop on determinands ---------------------------------------------- 
 2000 continue
*     end of loop on shots ----------------------------------------------------- 

*     calculate the abstracted load --------------------------------------------
      do 1000 jdet = 1, ndet
      if ( qtype (jdet) .ne. 4 ) then
      ABLOAD (JDET,I13) = ABLOAD (JDET,I13) / float(NS)
      A1LOAD (JDET,I13) = A1LOAD (JDET,I13) / float(NS)
      A2LOAD (JDET,I13) = A2LOAD (JDET,I13) / float(NS)
      do 5000 J13 = 2, K13
      if ( NSM(jdet,J13) .gt. 1 ) then
      ABLOAD (JDET,J13) = ABLOAD (JDET,J13) / float(NSM(JDET,J13))
      A1LOAD (JDET,J13) = A1LOAD (JDET,J13) / float(NSM(JDET,J13))
      A2LOAD (JDET,J13) = A2LOAD (JDET,J13) / float(NSM(JDET,J13))
      endif
 5000 continue
*     calculate the abstracted quality =========================================
      if ( numflows .gt. 2 ) then
      ABStdev(jdet) = SQRoot1(1048, (ABStdev(jdet) 
     &              - ABMean(jdet) * ABMean(jdet) / numflows )
     &              / (numflows - 1) )
      endif
      if ( numflows .gt. 1 ) then
      ABMean (jdet) = ABMean (jdet) / numflows
      ABMean2(jdet) = ABMean (jdet) * numflows / NS
      endif

*     rank the values of abstracted quality for the calculation of percentiles -
      do is = 1,NS
	Y(is) = abvalues(jdet,is)
      enddo
      m90 = min0 (k90, numflows-1)
      if ( numflows .gt. 2 ) then
      do 3020 I = 1, m90
      do 3030 J = I + 1, numflows
      if (Y(I) .gt. Y(J)) goto 3030
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
 3030 continue
 3020 continue
      absq95(jdet) = Y(0.95 * numflows)
      endif
	endif
 1000 continue ! end of loop on determinands -----------------------------------

      do jdet = 1 , ndet
      if ( QTYPE (jdet) .ne. 4 ) then
      kprune det = jdet
*     accumulate total loads removed by abstractions ===========================
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1)
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1)
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0
      if ( abload(jdet,J1) .gt. 0.0 ) then
      if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(jdet,J1) - abload  (jdet,J1)) 
     &                /  TGLODE2(jdet,J1)
      endif
	endif
      endif
*     prune load (J1) = 1.0
*     if ( abload(jdet,J1) .gt. 0.0 ) then
*     if ( abs ( TGLODE1 (jdet,J1) ) .gt. 0.000000001 ) then
*     if ( TGLODE1(jdet,J1) + abload(jdet,J1) .gt. 0.0 ) then
*     prune load (J1) = (TGLODE1(jdet,J1) + abload (jdet,J1)) 
*    &                /  TGLODE1(jdet,J1)
*     endif
*	endif
*     endif
	enddo

*     trim back the loads for abstractions =====================================
      call scale loads after losses
	endif
      enddo

      if ( MONF .gt. 1 ) call write shots for river flow
      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
      call calculate summaries of river flow
      call sort format 2 (FLOW(1),FLOW(2))
      if ( nobigout .le. 0 ) write(01,1013)valchars10,FUNIT,
     &valchars11,FUNIT
 1013 format('River flow downstream of the abstraction:',14X,' Mean =',
     &a10,1x,a4/46X,'95% exceedence =',a10,1x,a4/77('-'))
	xnflow = 100.0 * numflows / NS
      if ( xnflow .lt. 99.9999 ) then
      if ( nobigout .le. 0 ) write(01,1813)xnflow
 1813 format('Time in operation',51x,'=',F6.1,' %'/77('-'))
      endif
      
      do jp = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      if ( nobigout .le. 0 ) write(01,1713)DNAME(jp),Abmean(jp),
     &UNITS(jp),Abstdev(jp),UNITS(jp),
     &absq95(jp),UNITS(jp),abmean2(jp),UNITS(jp)
 1713 format('Abstracted river quality for ',a11,19X,'Mean =',
     &F7.2,1x,a4/
     &39X,'      Standard deviation =',F7.2,1x,a4/
     &39X,'           95-percentile =',F7.2,1x,a4/
     &39X,' (Mean over entire year) =',F7.2,1x,a4/77('-'))
      endif
      enddo ! loop on determinands
      endif ! if (F(IF,1) .gt. 1.0E-10 )
      
*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! negative discharge one

      if ( ical13 .eq. 0 ) then
      call write data for graph plotting
      endif

      return
      end

      
      
*     NEGATIVE DISCHARGE TWO (Feature type 19) ---------------------------------
*     adjust river flow for the effect of abstraction --------------------------
*     The abstraction data are specified as mean and standard deviation --------
*     (NEGATIVE DISCHARGE ONE uses mean and 95-percentile) ---------------------

      subroutine negative discharge two
      include 'COM.FOR'
      dimension A1load(MP10,13),A2load(MP10,13)
	dimension abmean(MP10),abstdev(MP10),absq95(MP10)
	dimension abvalues(MP10,MS),Y(MS),abmean2(MP10)

*     initialise the abstracted loads ------------------------------------------
      do jdet = 1, ndet
      A1LOAD(jdet,i13) = 0.0
      A2LOAD(jdet,i13) = 0.0
      abmean(jdet) = 0.0
      abmean2(jdet) = 0.0
	abstdev(jdet) = 0.0
      do is = 1, NS
      abvalues(jdet,IS) = 0.0
      enddo
      do L13 = 1, N13
	NSM (jdet,L13) = 0
      ABLOAD (jdet,L13) = 0.0
	enddo
      enddo ! initialise the abstracted loads ----------------------------------

*     identify the code number for the effluent data-set -----------------------
      IQ = JQ (KFEAT)
      if ( IQ .lt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
      if ( iscreen .lt. 3 ) write( *,1020)KFEAT
      write(09,1020)KFEAT
      write(33,1020)KFEAT
 1020 format(/'*** Abstraction data specified for Feature number',I6,
     &' do not exist')
      if ( nobigout .le. 0 ) write(01,1006)
      if ( iscreen .lt. 3 ) write( *,1006)
 1006 format('*** No abstraction made ***'/80('-'))
      return
      endif

*     default correlation coefficient
*     correlation of abstracted flow on main river flow
      CO2 = 0.0
*     special correlation ------------------------------------------------------
      if ( FE (IQ,MO) .ne. -9.9 ) CO2 = FE (IQ,MO) ! flow on flow
      call set up the correlation coefficients ! 4444444444444444444444444444444
      IFDIST = PDEF(IQ) ! the type of distribution -----------------------------

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      if ( IFDIST .gt. 0 ) then
      call sort format 3 (FE(IQ,1),FE(IQ,2),CO2)
      write(01,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
      write(21,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
 2012 format(77('-')/'Flow to be removed from the river:',20X,
     &'  Mean =',a10,1X,A4/
     &42x,'Standard deviation =',a10,1x,a4/
     &42x,'       Correlation =',a10/77('-'))
      else
      call sort format 1 (FE(IQ,1))
      write(01,2812) valchars10,FUNIT
      write(21,2812) valchars10,FUNIT
 2812 format(77('-')/'Flow to be removed from the river:',18x,
     &'Constant =',a10,1X,A4/77('-'))
      endif
      endif
      endif

*     note whether the distribution is non-parametric --------------------------
      if ( IFDIST .eq. 4 ) then
      if ( nobigout .le. 0 ) write(01,1965) 100.0 * CO2
 1965 format(77('-')/
     &'These data were extracted from a non-parametric distribution ',
     &'...'/'The flows are provided as an effluent data set ...'/
     &'Correlation with main river flow =',f5.2,' per cent ...')
*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 1970
 1969 continue
      write(01,7813) 
      write( *,7813) 
      write(09,7813) 
      write(33,7813) 
 7813 format('There is no non-parametric data file ',
     &'for an abstraction of type 18 ...'/77('-'))
      call stop ! there is no non-parametric data file
 1970 continue
      if ( nobigout .le. 0 ) write(01,7163) flnamesmall(3,icod)
 7163 Format('File: ',a64/77('-'))
      endif ! non-parametric abstractions

*     note whether the data are monthly ----------------------------------------
      if ( IFDIST .eq. 5 ) then
      if ( nobigout .le. 0 ) write(01,1966)
 1966 format('The abstraction is represented by monthly data ...')
*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 2970
 2969 continue
      write(01,7863) 
      write( *,7863) 
      write(09,7863) 
      write(33,7863) 
 7863 format('There is no monthly data file ',
     &'for an abstraction of type 19 ...'/77('-'))
      call stop ! there is no monthly data file 
 2970 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(3,icod)
 7363 format('File: ',a64/77('-'))
      endif ! monthly data

      if ( FE(IQ,1) .gt. 1.0E-10 ) then ! check for zero flows  

*     deal with for the log-normal distribution of abstracted flows ------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log normal distribution
      EFM = FE(IQ,1) ! mean abstracted flow (effluent type)
      EFS = FE(IQ,2) ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = FE(IQ,3)
*     compute mean and standard deviation of logged flows from the mean and ----
*     standard deviation -------------------------------------------------------
      GEFM = 0.0 ! mean in the log domain
      GEFS = 0.0 ! corresponding standard deviation
      EM3 = EFM+EF3
      if ( EM3 .lt. 1.0e-9 ) then
      write( *,9099)UNAME(KFEAT)
      write(01,9099)UNAME(KFEAT)
 9099 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE TWO')
      call stop
      endif ! if ( EM3 .lt. 1.0e-09 )
      GEFM = ALOG ((EM3*EM3)/SQRoot(1049,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1050, ALOG(1.0+(EFS*EFS)/(EM3*EM3)))
      if (GEFS .le. 0.0) then
      write( *,9899)UNAME(KFEAT)
      write(01,9899)UNAME(KFEAT)
 9899 format(///'### Flow error for Feature called - ',A32//
     &'#### Simulation stopped in NEGATIVE DISCHARGE TWO')
      call stop
      endif ! if ( GEFS .le. 0.0 )
      GM = GEFM
      GS = GEFS
      RR3 = EF3
      call bias in log normal negative discharge flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3) ! mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3) ! standard deviation
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1299)BM(3),BS(3)
 1299 format(
     &'Corrections for sampling errors: log-normal removals (effluent)',
     &' ...'/77('-')/'Mean',15x,'=',F8.3/'Standard deviation =',F8.3)
      call generate removed log normal effluent flows
     *(GRFM,GRFS,FE(IQ,1))
      endif ! if ( MONF .gt. 1 )
      endif ! log normal -------------------------------------------------------

      
*     deal with for the log-normal distribution of abstracted flows 999999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( IFDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      EFM = FE(IQ,1) ! mean abstracted flow (effluent type)
      EFS = FE(IQ,2) ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = FE(IQ,3)
*     compute mean and standard deviation of logged flows from the mean and 9999
*     standard deviation 9999999999999999999999999999999999999999999999999999999
      GEFM = 0.0 ! mean in the log domain
      GEFS = 0.0 ! corresponding standard deviation
      EM3 = EFM+EF3
      if ( EM3 .lt. 1.0e-9 ) then
      write( *,9299)UNAME(KFEAT)
      write(01,9299)UNAME(KFEAT)
 9299 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE TWO')
      call stop
      endif ! if ( EM3 .lt. 1.0e-09 )
      GEFM = ALOG ((EM3*EM3)/SQRoot(1049,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1050, ALOG(1.0+(EFS*EFS)/(EM3*EM3)))
      if (GEFS .le. 0.0) then
      write( *,9829)UNAME(KFEAT)
      write(01,9829)UNAME(KFEAT)
 9829 format(///'### Flow error for Feature called - ',A32//
     &'#### Simulation stopped in NEGATIVE DISCHARGE TWO')
      call stop
      endif ! if ( GEFS .le. 0.0 )
      GM = GEFM
      GS = GEFS
      RR3 = EF3
      call bias in log normal negative discharge flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3) ! mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3) ! standard deviation
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1229)BM(3),BS(3)
 1229 format(
     &'Corrections for sampling errors: log-normal removals (effluent)',
     &' ...'/77('-')/'Mean',15x,'=',F8.3/'Standard deviation =',F8.3)
      call generate removed log normal effluent flows
     *(GRFM,GRFS,FE(IQ,1))
      endif ! if ( MONF .gt. 1 )
      endif ! if ( IFDIST .eq. 10 ) power curve parametric distribution 99999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999

      
      if ( IFDIST .eq. 1 ) then ! normal distribution --------------------------
      EFM = FE(IQ,1) ! mean abstracted flow (effluent type)
      EFS = FE(IQ,2) ! standard deviation
      EF3 = 0.0 ! shift
	if ( nobigout .le. 0 ) then
      call sort format 2 (EFM,EFS)
      write(01,2512) valchars10,FUNIT,valchars11,FUNIT
      write(21,2512) valchars10,FUNIT,valchars11,FUNIT
 2512 format(77('-')/'Flow to be removed from the river:',20X,
     &'  Mean =',a10,1X,A4/
     &42x,'Standard deviation =',a10,1x,a4/77('-'))
      endif ! if ( nobigout .le. 0 )
      GM = EFM
      GS = EFS
      RR3 = EF3
      call bias in normal negative discharge flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3)
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,52)BM(3),BS(3)
   52 format(
     &'Corrections for sampling errors: normal removals (effluent)',
     &' ...'/77('-')/'Mean',15x,'=',f8.3/'Standard deviation =',F8.3)
      call generate removed normal effluent flows (GM,GS,FE(IQ,1))          
      endif ! if ( MONF .gt. 1 )
      endif ! normal distribution ----------------------------------------------
      
      if ( IFDIST .eq. 0 ) then ! constant flow --------------------------------
      EFS = 0.0
      EFM = FE(IQ,1)
      do is = 1, NS ! calculate flows
      FTMS (IS) = EFM
      enddo ! calculate flows
      endif ! constant flow ----------------------------------------------------

      if ( IFDIST .eq. 4) then ! non-parametric distribution -------------------
      call set up the shots for non parametric stream flow ! feature 19
      if ( MONF .gt. 1 ) call shots for removed flows
      endif ! non-parametric distribution --------------------------------------

      if ( IFDIST .eq. 5) then ! monthly data ----------------------------------
      call set up monthly data for negative discharge
      if ( MONF .gt. 1 ) call shots for removed flows
      endif ! monthly data -----------------------------------------------------

      call list the correlation coefficients

*     if ( MONF .gt. 1 ) call calculate summaries for removed flows

*     prepare to abstract the flows --------------------------------------------
      FACT = -1.0
*     prepare to count the shots where the abstraction is greater than zero ----
	numflows = 0

*     start the abstraction ----------------------------------------------------
      do 2000 IS = 1, NS ! loop through the shots
      ABSFLOW = 0.0
*     get the correlated random numbers ----------------------------------------
      call get the correlated random numbers (IS, R1, R2, R3, R4)
*     retrieve the flow of the upstream river ----------------------------------
      RF = FMS (IS)
*     and retrieve the upstream river quality ----------------------------------
*     this if we want to abstract conditionally on river quality ---------------
*     RC = CMS ( JP, IS )

*     get the shot for the abstraction ... the value of EF ---------------------
*     impose correlation between abstracted flow and river flow ----------------
*     R3 = CO2 * R1 + R3 * SQRMB(105, 1.0 - CO2 * CO2 )
*     call get the flows of the stream or discharge ( IS, R3, EF )
	EF = FTMS(IS)
	
*     now do the substraction --------------------------------------------------
      START FLOW = RF
      HANDS OFF = 0.0
      REM FLOW = amax1 (HANDS OFF, RF + FACT * EF)
      EF = START FLOW - REM FLOW
      FMS(IS) = REM FLOW
      if ( EF .gt. 1.0e-10) numflows = numflows + 1

*     calculate the abstracted load --------------------------------------------
      do 3000 jdet = 1, ndet
      if ( QTYPE (jdet) .ne. 4) then 
      do 3050 J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
      ABLOAD(jdet,K13) = ABLOAD (jdet,K13) + EF * CMS(jdet,IS) 
      A1LOAD(jdet,K13) = A1LOAD(jdet,K13) + START FLOW * CMS(jdet,IS)
      A2LOAD(jdet,K13) = A2LOAD(jdet,K13) + FMS(IS) * CMS(jdet,IS)
      NSM (jdet,K13) = NSM (jdet,K13) + 1
 3050 continue

*     prepare to calculate the mean of the abstracted quality ------------------
      if ( EF .gt. 1.0e-10 ) then
      ABMean (jdet) = ABMean (jdet) + CMS(jdet,IS)
      ABStdev(jdet) = ABStdev(jdet) + CMS(jdet,IS) * CMS(jdet,IS)
*     store the abstracted quality ---------------------------------------------
      abvalues(jdet,numflows) = CMS (jdet, numflows)
      endif
	endif
 3000 continue
*     end of loop on determinands ---------------------------------------------- 
 2000 continue
*     end of loop on shots ----------------------------------------------------- 

*     calculate the abstracted load --------------------------------------------
      do 1000 jdet = 1, ndet
      if ( qtype (jdet) .ne. 4 ) then
      ABLOAD (JDET,I13) = ABLOAD (JDET,I13) / float(NS)
      A1LOAD (JDET,I13) = A1LOAD (JDET,I13) / float(NS)
      A2LOAD (JDET,I13) = A2LOAD (JDET,I13) / float(NS)
      do 5000 J13 = 2, K13
      if ( NSM(jdet,J13) .gt. 1 ) then
      ABLOAD (JDET,J13) = ABLOAD (JDET,J13) / float(NSM(JDET,J13))
      A1LOAD (JDET,J13) = A1LOAD (JDET,J13) / float(NSM(JDET,J13))
      A2LOAD (JDET,J13) = A2LOAD (JDET,J13) / float(NSM(JDET,J13))
      endif
 5000 continue
*     calculate the abstracted quality =========================================
      if ( numflows .gt. 2 ) then
      ABStdev(jdet) = SQRoot1(1048, (ABStdev(jdet) 
     &              - ABMean(jdet) * ABMean(jdet) / numflows )
     &              / (numflows - 1) )
      endif
      if ( numflows .gt. 1 ) then
      ABMean (jdet) = ABMean (jdet) / numflows
      ABMean2(jdet) = ABMean (jdet) * numflows / NS
	endif
*     ==========================================================================
*     rank the values of abstracted quality for the calculation of percentiles -
      do is = 1,NS
	Y(is) = abvalues(jdet,is)
      enddo
      m90 = min0 (k90, numflows-1)
      if ( numflows .gt. 2 ) then
      do 3020 I = 1, m90
      do 3030 J = I + 1, numflows
      if (Y(I) .gt. Y(J)) goto 3030
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
 3030 continue
 3020 continue
      absq95(jdet) = Y(0.95 * numflows)
      endif
	endif
 1000 continue ! end of loop on determinands -----------------------------------

      do jdet = 1 , ndet
      if ( QTYPE (jdet) .ne. 4 ) then
      kprune det = jdet
*     accumulate total loads removed by abstractions ===========================
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1)
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1)
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0
      if ( abload(jdet,J1) .gt. 0.0 ) then
      if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(jdet,J1) - abload  (jdet,J1)) 
     &                /  TGLODE2(jdet,J1)
      endif
	endif
      endif
*     prune load (J1) = 1.0
*     if ( abload(jdet,J1) .gt. 0.0 ) then
*     if ( abs ( TGLODE1 (jdet,J1) ) .gt. 0.000000001 ) then
*     if ( TGLODE1(jdet,J1) + abload(jdet,J1) .gt. 0.0 ) then
*     prune load (J1) = (TGLODE1(jdet,J1) + abload (jdet,J1)) 
*    &                /  TGLODE1(jdet,J1)
*     endif
*	endif
*     endif
	enddo

*     trim back the loads for abstractions =====================================
      call scale loads after losses
	endif
      enddo
   
      if ( MONF .gt. 1 ) call write shots for river flow
      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
      call calculate summaries of river flow
      call sort format 2 (FLOW(1),FLOW(2))
      if ( nobigout .le. 0 ) write(01,1013)valchars10,FUNIT,
     &valchars11,FUNIT
 1013 format('River flow downstream of the abstraction:',14X,' Mean =',
     &a10,1x,a4/46X,'95% exceedence =',a10,1x,a4/77('-'))
	xnflow = 100.0 * numflows / NS
      if ( xnflow .lt. 99.9999 ) then
      if ( nobigout .le. 0 ) write(01,1813)xnflow
 1813 format('Time in operation',51x,'=',F6.1,' %'/77('-'))
      endif
	
      do jp = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      if ( nobigout .le. 0 ) write(01,1713)DNAME(jp),Abmean(jp),
     &UNITS(jp),Abstdev(jp),UNITS(jp),
     &absq95(jp),UNITS(jp),abmean2(jp),UNITS(jp)
 1713 format('Abstracted river quality for ',a11,19X,'Mean =',
     &F7.2,1x,a4/
     &39X,'      Standard deviation =',F7.2,1x,a4/
     &39X,'           95-percentile =',F7.2,1x,a4/
     &39X,' (Mean over entire year) =',F7.2,1x,a4/77('-'))
      endif
      enddo ! loop on determinands
      endif ! if ( FE(IQ,1) .gt. 1.0E-10 )

*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! negative discharge two

      if ( ical13 .eq. 0 ) then
      call write data for graph plotting
      endif

      return
      end