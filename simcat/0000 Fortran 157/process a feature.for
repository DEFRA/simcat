*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File: process a feature.for ... 4524 lines  ------------------------------
*     --------------------------------------------------------------------------
*     This file processes the Features in a Reach ------------------------------
*     --------------------------------------------------------------------------
*     This file contains 26 routines. They are called: -------------------------
*     --------------------------------------------------------------------------
*     ...... process the feature (ITYPE)
*     ...... monitoring point
*     ...... interpolation point
*     ...... flow into lake
*     ...... flow from a lake into a river
*     ...... catchment boundary
*     ...... zero discharge flow 17
*     ...... river flow gauge
*     ...... tributary
*     ...... write out river flow
*     ...... write mean and standard deviation
*     ...... write mean and standard deviation 33
*     ...... write mean and standard deviation 33a
*     ...... write data for graph plotting
*     ...... effluent discharge
*     ...... assess the need to track the discharge
*     ...... write information on effluent flow 
*     ...... write effluent discharge quality
*     ...... abstraction of flow
*     ...... fish farm
*     ...... river regulation
*     ...... weir
*     ...... initialise data for mass balance
*     ...... load the upstream flow and quality 
*     ...... write GIS data to channel 42 43 24 and 25
*     ...... negative discharge one ! abstraction 18
*     ...... negative discharge two ! abstraction 19
*     --------------------------------------------------------------------------

      subroutine process the feature (ITYPE)
      include 'COMMON DATA.FOR'

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


*     check for a river quality target TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      targit = 0.0 ! initialise the target -------------------------------------
      IQSfeet = IFRQS(KFEAT)
      do JP = 1, NDET ! loop on the determinands DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      if ( QTYPE (JP) .ne. 4 ) then
      
      IQSreach = EQS reach (IREACH,JP) 
      
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then ! set default target DDDDD
          
      if ( IQSfeet .ne. 999 ) then ! 9999999999999999999999999999999999999999999
      if ( background class .eq. 1 ) then ! set the default target as Class 2 DD
      targit = class limmits2 (2,JP) ! set the default target DDDDDDDDDDDDDDDDDD
      endif ! set the defaults target DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      endif ! 999999999999999999999999999999999999999999999999999999999999999999
      
      if ( IQSfeet .gt. 0 .and. IQSfeet .ne. 999 ) then
      targit = RQS (IQSfeet,JP) ! set target for feature -
      endif
      if ( IQSreach .gt. 0 ) then ! prepare over-write the feature target ------
      if ( IQSfeet .ne. 999) then ! but only if the set is not number 999 999999
      do ic = 1, nclass - 1 ! over-write with reach-specific value -------------
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5) then
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif
      endif ! if ( MRQS(JP) .eq. 4 ) -------------------------------------------
      enddo ! do ic = 1, nclass - 1 ! over-write with reach-specific value -----
      endif ! if ( IQSfeet .ne. 999 ) 999999999999999999999999999999999999999999
      RQO(JP) = targit ! use the target for graphs -----------------------------
      MRQO(JP) = MRQS(JP)
      endif ! if ( IQSreach .gt. 0 ) -------------------------------------------
      endif ! if ( IQSreach .gt. 0 .or. IQSreach .gt. 0) -----------------------
      if ( IQSfeet .eq. 999) then ! but only if the set is not number 999 999999
      targit = 0.0
      SKIPPEFF(KFEAT) = 1
      endif
      
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

*     illegal features ---------------------------------------------------------
      if ( itype .gt. 61 ) then
      if ( nobigout .le. 0 ) write(01,98)ITYPE,UNAME(feeture)
      write(09,98)ITYPE,UNAME(feeture)
      if ( iscreen .lt. 3 ) write( *,98)ITYPE,UNAME(feeture)
   98 format('*** Illegal Type of Feature (Type =',I3,') ',A40)
      endif

*     ==========================================================================
*     monitoring point ---------------------------------------------------------
      if ( itype .eq. 1 ) then ! ---------------------------- a monitoring point 
      if ( JSKIP .eq. 0 ) then 

      call monitoring point
      
      goto 6000 ! return
      endif ! if ( JSKIP .eq. 0 ) 

      endif ! if ( itype .eq. 1 ) ----------------------------- monitoring point
*     ==========================================================================


*     ==========================================================================
*     tributary or stream (too small to be a REACH) ============================
      if ( itype .eq. 2) then 
      call tributary
      goto 6000 ! return
      endif
*     ==========================================================================


      
*     ==========================================================================
*     effluent discharges ======================================================
*     apply to (3) (5) (12) (39) (60) (61) ------------- call effluent discharge
      if ( JT(feeture) .eq. 03 .or. ! ----------------------------- sewage works
     &     JT(feeture) .eq. 05 .or. ! ------------------------- point discharges 
     &     JT(feeture) .eq. 12 .or. ! ------------------ intermittent discharges
     &     JT(feeture) .eq. 39 .or. ! ------------------------- mining discharge
     &     JT(feeture) .eq. 60 .or. ! ---------------------- other Point Sources
     &     JT(feeture) .eq. 61 ) then ! -------------------- private wastewaters 
      discharge type = itype
      zeroEF = 0.0

      call effluent discharge
      
      discharge type = 0
      goto 6000 ! return
      endif
*     ==========================================================================

     
      if ( itype .eq. 4 ) then ! ==================================== flow gauge
      if ( JSKIP .eq. 1 ) return
      call river flow gauge
      call get summaries of river quality from the shots ! flow gauge ----------
      call get summaries of loads
      goto 6000 ! return
      endif ! ======================================================= flow gauge


      if ( itype .eq. 6 ) then ! =========================== interpolation point
      call interpolation point
      goto 6000 ! return
      endif ! ============================================== interpolation point


      if ( itype .eq. 44 ) then ! flow into a lake =============================
      call flow into lake
      goto 6000 ! return
      endif ! ==================================================================

      if ( itype .eq. 45 ) then ! flow from a lake =============================
      call flow from a lake into a river ! -------------------------------- (45)
      goto 6000 ! return
      endif ! ==================================================================


      if ( itype .eq. 24 ) then ! this is a sub-catchment boundary =============
      call catchment boundary
      goto 6000 ! return
      endif ! ==================================================================

      
*     ==========================================================================
*     abstraction (7) ----------------------------------------------------------
      if ( itype .eq. 7 ) then 
      call abstraction of flow ! ----------------------- abstraction of type (7)
      call get summaries of river quality from the shots ! ------abstraction (7)
      call get summaries of loads
      goto 6000 ! return for abstraction (7)
      endif ! if ( itype .eq. 7 ) ============================== abstraction (7)
*     ==========================================================================


*     ==========================================================================
*     abstraction (the negative discharge one - tributary type - 18) -----------
      if ( itype .eq. 18 ) then 
      call negative discharge one ! abstraction 18
      call get summaries of river quality from the shots ! abstraction (18)-----
      call get summaries of loads
      goto 6000 ! return
      endif ! abstraction (the negative discharge - tributary type)
*     ==========================================================================


*     ==========================================================================
*     abstraction (the negative discharge two - effluent type - 19 ) -----------
      if ( itype .eq. 19 ) then ! abstraction 19 
      call negative discharge two ! abstraction 19
      call get summaries of river quality from the shots ! negative discharge 19
      call get summaries of loads
      goto 6000 ! return
      endif ! abstraction (the negative discharge - effluent type)
*     ==========================================================================


*     ==========================================================================
*     weir ---------------------------------------------------------------------
      if ( itype .eq. 8 ) then ! weir
      call weir
      call get summaries of river quality from the shots ! weir (8) ------------
      call get summaries of loads
      goto 6000 ! return
      endif
*     ==========================================================================


*     ==========================================================================
*     river regulation point ---------------------------------------------------
      if ( itype .eq. 9 ) then 
      call river regulation
      call get summaries of river quality from the shots ! regulation point ----
      call get summaries of loads
      goto 6000 ! return
      endif
*     ==========================================================================


*     not used here - upstream boundary ----------------------------------------
      if ( itype .eq. 10 ) goto 6000 ! return ! process the feature
*     bifurcations -------------------------------------------------------------
      if ( itype .eq. 11 ) goto 6000 ! return
*     abstraction of river flow distribution -----------------------------------
*     first branch -------------------------------------------------------------
      if ( itype .eq. 20 ) goto 6000 ! return
*     returned abstraction of river flow distribution --------------------------
*     second branch ------------------------------------------------------------
      if ( itype .eq. 21 ) goto 6000 ! return 
*     abstraction of effluent flow distribution --------------------------------
*     first branch ...
      if ( itype .eq. 22 ) goto 6000 ! return 
*     returned abstraction of effluent flow distribution -----------------------
*     second branch ------------------------------------------------------------
      if ( itype .eq. 23 ) goto 6000 ! return 
      

*     ==========================================================================
*     switch on diffuse pollution (river type) ---------------------------------
      if ( itype .eq. 13 ) then 
      call turn on river based diffuse pollution
      goto 6000 ! return
      endif
*     switch off diffuse pollution ---------------------------------------------
      if ( itype .eq. 14 ) then 
      call turn off river based diffuse pollution
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from agricultural livestock (river type) -----
      if ( itype .eq. 25 ) then 
      call turn on river based diffuse pollution from livestock
      goto 6000 ! return
      endif
*     switch off Diffuse Pollution from agricultural livestock (river type) ----
      if ( itype .eq. 26 ) then 
      call turn off river based diffuse pollution from livestock
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from agricultural arable (river type) --------
      if ( itype .eq. 27 ) then 
      call turn on river based diffuse pollution from arable
      goto 6000 ! return
      endif
*     switch off diffuse pollution from agricultural arable (river type) -------
      if ( itype .eq. 28 ) then 
      call turn off river based diffuse pollution from arable
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from highways --------------------------------
      if ( itype .eq. 29 ) then ! feature type 29
      call turn on river based diffuse pollution from highways
      goto 6000 ! return
      endif
*     switch off diffuse pollution from highways -------------------------------
      if ( itype .eq. 30 ) then ! feature type 30
      call turn off river based diffuse pollution from highways
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from urban (river type) ----------------------
      if ( itype .eq. 31 ) then 
      call turn on river based diffuse pollution from urban
      goto 6000 ! return
      endif
*     switch off diffuse pollution from urban (river type) ---------------------
      if ( itype .eq. 32 ) then 
      call turn off river based diffuse pollution from urban
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from atmospheric deposition ------------------
      if ( itype .eq. 33 ) then 
      call turn on diffuse pollution from the atmosphere
      goto 6000 ! return
      endif
*     switch off diffuse pollution from atmospheric deposition -----------------
      if ( itype .eq. 34 ) then 
      call turn off diffuse pollution from the atmosphere
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from background ------------------------------
      if ( itype .eq. 35 ) then 
      call turn on diffuse pollution from background
      goto 6000 ! return
      endif
*     switch off diffuse pollution from background -----------------------------
      if ( itype .eq. 36 ) then 
      call turn off diffuse pollution from background
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from mines -----------------------------------
      if ( itype .eq. 46 ) then 
      call turn on diffuse pollution from mines
      goto 6000 ! return
      endif
*     switch off diffuse pollution from mines ----------------------------------
      if ( itype .eq. 47 ) then 
      call turn off diffuse pollution from mines
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from birds, boats and angling ----------------
      if ( itype .eq. 48 ) then 
      call turn on diffuse pollution from birds
      goto 6000 ! return
      endif
*     switch off diffuse pollution from birds, boats and angling ---------------
      if ( itype .eq. 49 ) then 
      call turn off diffuse pollution from birds
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from septic tanks ----------------------------
      if ( itype .eq. 37 ) then 
      call turn on diffuse pollution from septic tanks
      goto 6000 ! return
      endif
*     switch off diffuse pollution from septic tanks ---------------------------
      if ( itype .eq. 38 ) then 
      call turn off diffuse pollution from septic tanks
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from aggregated CSOs -------------------------
      if ( itype .eq. 40 ) then 
      call turn on diffuse pollution from aggregated CSOs
      goto 6000 ! return
      endif
*     switch off diffuse pollution from septic tanks ---------------------------
      if ( itype .eq. 41 ) then 
      call turn off diffuse pollution from aggregated CSOs
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution (discharge type) -----------------------------
      if ( itype .eq. 15 ) then 
      call turn on discharge based diffuse pollution
      goto 6000 ! return
      endif
*     switch off diffuse pollution ---------------------------------------------
      if ( itype .eq. 16 ) then 
      call turn off discharge based diffuse pollution
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on diffuse pollution from aggregated STWs -------------------------
      if ( itype .eq. 42 ) then ! aggregated STWs (42) 
      call turn on diffuse pollution from aggregated STWs
      goto 6000 ! return
      endif
*     switch on diffuse pollution from aggregated STWs -------------------------
      if ( itype .eq. 43 ) then 
      call turn off diffuse pollution from aggregated STWs
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on nameless diffuse pollution one ---------------------------------
      if ( itype .eq. 50 ) then 
      call turn on nameless diffuse pollution one
      goto 6000 ! return
      endif
*     switch off nameless diffuse pollution one --------------------------------
      if ( itype .eq. 51 ) then 
      call turn off nameless diffuse pollution one
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on nameless diffuse pollution two ---------------------------------
      if ( itype .eq. 52 ) then 
      call turn on nameless diffuse pollution two
      goto 6000 ! return
      endif
*     switch off nameless diffuse pollution two ---------------------------------
      if ( itype .eq. 53 ) then 
      call turn off nameless diffuse pollution two
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on nameless diffuse pollution three -------------------------------
      if ( itype .eq. 54 ) then 
      call turn on nameless diffuse pollution three
      goto 6000 ! return
      endif
*     switch off nameless diffuse pollution three ------------------------------
      if ( itype .eq. 55 ) then 
      call turn off nameless diffuse pollution three
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on nameless diffuse pollution four --------------------------------
      if ( itype .eq. 56 ) then 
      call turn on nameless diffuse pollution four
      goto 6000 ! return
      endif
*     switch off nameless diffuse pollution four --------------------------------
      if ( itype .eq. 57 ) then 
      call turn off nameless diffuse pollution four
      goto 6000 ! return
      endif
*     ==========================================================================
*     switch on turn off nameless diffuse pollution five -----------------------
      if ( itype .eq. 58 ) then 
      call turn on nameless diffuse pollution five
      goto 6000 ! return
      endif
*     switch off nameless diffuse pollution five -------------------------------
      if ( itype .eq. 59 ) then 
      call turn off nameless diffuse pollution five
      goto 6000 ! return
      endif
*     ==========================================================================

*     discharge with zero flow -------------------------------------------------
      if ( itype .eq. 17 ) then 
      call zero discharge flow 17
      goto 6000 ! return
      endif ! ----------------------------------------- discharge with zero flow

 6000 continue

      return
      end




      subroutine monitoring point
      include 'COMMON DATA.FOR'

      character *13 SET1,SET2
      character *16 SET5

      IQ = JQ(KFEAT) ! set of quality data -------------------------------------

      if ( ICAL .eq. 1 ) return ! from monitoring point ------------------------

      if ( IPRINT .eq. 1 ) goto 1
      if ( nobigout .le. 0 ) then ! ============================================
      if ( ical13 .eq. 0 .or. ical .eq. 3 .or. ical .eq. 4 ) then ! ------------
      write(01,1015)uname(KFEAT) ! ----------------------------------------- OUT
 1015 format(110('=')/
     &'Calculated river quality at this monitoring point ...   At ',
     &a40/110('-'))
      endif ! if ( ical13 .eq. 0 .or. ical .eq. 3 or 4 ) -----------------------
      endif ! if ( nobigout .le. 0 ) ===========================================
      
      call write out loads and quality (0) ! monitoring point
      call write out monthly river quality (2) ! at monitoring point

*     store the results for the summaries --------------------------------------
    1 LMONP = LMONP + 1 ! add to the number of monitoring stations -------------

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
      Qstations(LMONP,J,1) = C(J,1) ! calculated quality at monitoring stations
      Qstations(LMONP,J,2) = C(J,2) ! calculated quality at monitoring stations
      Qstations(LMONP,J,3) = C(J,3) ! calculated quality at monitoring stations
      endif
      enddo

*     add to the list of monitoring points -------------------------------------
      if ( JT(feeture) .eq. 1 ) LSAMPOINTS(LMONP) = KFEAT

      if ( IQ .eq. 0 ) return

      call compute the mean and percentiles at a monitoring point

      ifirst = 0      
      do J = 1, ndet! =========================================================
      if ( QTYPE (J) .ne. 4) then ! ============================================

      a1 = C(J,1)
      s1 = C(J,2)
      n1 = QNUM(IQ,J)
      a2 = quolity data(IQ,J,1)
      s2 = quolity data(IQ,J,2)
      n2 = QNUM(IQ,J)

      call DIFFAV (a1,s1,n1,a2,s2,n2,al1,au1,al2,au2,
     &Pinc,Ydec,A12,S12,AL12,AU12)


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ical .eq. 3 .and. ifirst .eq. 0 ) then ! ----------------------------
      ifirst = 1      
      write(01,2176)
 2176 format(/77('#')/'RESULTS FOR GAP FILLING .... '/77('#'))
      endif ! if ( ical .eq. 3 .and. ifirst .eq. 0 ) ---------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
    
      
      write(01,2076)uname(KFEAT),Dname(J)
      !write(170+J,2076)uname(KFEAT),Dname(J)
 2076 format(/77('#')/
     &'Monitoring station: ',a37,'   ... ',a11/77('-')/23x,
     &7x,'Mean        Confidence     Standard   Number of'/23x,
     &7x,'                          Deviation     Samples')
     
      SET1 = "             "
      SET2 = "             "
      if ( A1. gt. 0.000001 ) call assem(AL1,AU1,SET1)
      if ( A2. gt. 0.000001 ) call assem(AL2,AU2,SET2)

      write(01,2099)A1,SET1,s1,N1,A2,SET2,s2,N2
 2099 format(77('-')/
     &7x,' Calculated   ',F13.2,3x,a13,F13.2,i12/
     &7x,' Observed     ',F13.2,3x,a13,F13.2,i12/77('-'))
     
      A12P = 100.0*(A12)/A1
      AL12P = int(100.0*(AL12)/A1)
      AU12P = int(100.0*(AU12)/A1)

      quolity data(IQ,J,MO+1) = A12P
      quolity data(IQ,J,MO+2) = AL12P
      quolity data(IQ,J,MO+3) = AU12P

      SET5 = "                "
      if ( A1 .gt. 0.000001 .and. A2 .gt. 0.000001 ) then ! ---------------------
      call ASSEM1(AL12P,AU12P,SET5)
      write(01,6399)int(A12P),SET5
 6399 format(5x,' % Change needed ',i12,a16/77('#'))
      else
      write(01,6299)
 6299 format(77('#'))
      endif ! if ( A1 .lt. 0.000001 .or. A2 .lt. 0.000001 ) --------------------
      
      endif ! if ( QTYPE (J) .ne. 4) ===========================================
      enddo ! do J = 1, ndet! ==================================================

      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! monitoring point
      endif

      return
      end





*     interpolation or plotting point ------------------------------------------
      subroutine interpolation point
      include 'COMMON DATA.FOR'

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality ! interpolation
      if ( MONQ .gt. 1 ) call write shots for river flow ! interpolation
      endif
      enddo

      if ( IPRINT .ne. 1 ) then
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(110('-')/
     &'Calculated river quality at this interpolation point ...'/
     &110('-'))
      call write out loads and quality (0) ! at interpolation
      call write out monthly river quality (2) ! at interpolation
      endif
      call write data for graph plotting ! interpolation or plotting point (6)
      endif ! if ( ical13 .eq. 0 )
      endif

      return
      end


      
      
*     flow into lake -----------------------------------------------------------
      subroutine flow into lake
      include 'COMMON DATA.FOR'

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality ! into a lake
      if ( MONQ .gt. 1 ) call write shots for river flow ! into a lake
      endif
      enddo

      if ( IPRINT .eq. 1 ) return

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(77('-')/
     &'Calculated river quality for flow into the lake ...'/77('-'))
      call write out loads and quality (0) ! at lake inflow
      call write out monthly river quality (2) ! at lake inflow
      endif
      call write data for graph plotting ! flow into lake
      endif

      return
      end


*     flow from a lake into a river --------------------------------------- (45)
      subroutine flow from a lake into a river
      include 'COMMON DATA.FOR'

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality ! into a river
      if ( MONQ .gt. 1 ) call write shots for river flow ! into a river
      endif
      enddo
      
      if ( IPRINT .eq. 1 ) return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1)rname(IREACH)
    1 format(110('-')/
     &'Calculated river quality for the flow from: 'a16/
     &110('-'))
      call write out loads and quality (0) ! at lake outflow
      call write out monthly river quality (2) ! at lake outflow
      endif
      call write data for graph plotting ! flow from a lake into a river
      endif

      return
      end



      subroutine catchment boundary
      include 'COMMON DATA.FOR'

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality ! catchment boundary
      if ( MONQ .gt. 1 ) call write shots for river flow ! -- catchment boundary
      endif
      enddo

      if ( ical13 .eq. 1 ) return ! running in calibration mode ----------------
*     call sort out the concentrations and loads
      call add up all the loads ! for all determinands
      
*     add to the counter of sub-catchments -------------------------------------
      kount bodies = kount bodies + 1 !  number of sub-catchments (water bodies)
*     check for too many sub-catchments ----------------------------------------
      if ( kount bodies .gt. NUW ) then
      if ( ifbatch .eq. 0 ) then
       call change colour of text (14) ! bright yellow
      write( *,4511)NUW
      call set screen text colour
 4511 format('Too many sub-catchments for back-tracking ',
     &'up the model ... the limit is ',i4/
     &'The extra sub-catchments are not being tracked ...')
      kount bodies = kount bodies - 1 ! too many sub-catchments ----------------
      endif
      else
          
*     store the new sub-catchment ==============================================
*     store the count of the number of bodies for back-tracking ----------------
      identify bodies ( kount bodies ) = feeture ! the number of the feature ---
      identify reaches (1, kount bodies ) = ireach ! the reach location --------
      if ( ireach .eq. 1 ) then ! it is the first reach 
      identify reaches (2, kount bodies ) = 0 ! no upstream reaches ------------
      identify reaches (3, kount bodies ) = 0 ! no upstream reaches ------------
      else ! add the code numbers of the reaches immediately upstream ----------
	identify reaches (2, kount bodies ) = -IPLAN(IREACH-1,1)
	identify reaches (3, kount bodies ) = -IPLAN(IREACH-1,2)
	endif
      endif ! kount bodies .gt. NUW ============================================


      do 100 jp = 1, ndet ! dddddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( qtype(jp) .eq. 4 ) go to 100 ! dddddddddddddddddddddddddddddddddddddd
      do 101 j13 = 1, n13 ! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
*     store the monthly loads (j13) from upstream sub-catchments ---------------
      twloads ( kount bodies ,jp, j13) = TGLODE2 (jp, j13)
      
*     ( 1) = 'Mean from all discharges (3,12,5,39,60,61)'
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

*     (17) = 'Boundaries and tributaries(2 and 10)'
*     (18) = 'Diffuse input (13)'
*     (19) = 'Diffuse input (15)'
*     (20) = 'Reach diffuse (no Feature code)'
*     (21) = 'Gap filling of flows (no Feature code)'
*     (22) = 'Gap filling of quality (no Feature code)'

*     (23) = 'User-named diffuse input (50)'
*     (24) = 'User-named diffuse input (52)'
*     (25) = 'User-named diffuse input (54)'
*     (26) = 'User-named diffuse input (56)'
*     (27) = 'User-named diffuse input (58)'
*     (28) = 'Other point sources (60)'
*     (29) = 'Private wastewaters (61)'

      twloadsapp ( kount bodies ,jp, j13, 1 ) = TELODE2  (jp, j13)   
      twloadsapp ( kount bodies ,jp, j13, 2 ) = T03LOAD2 (jp, j13) ! sewage effluents (3)
      twloadsapp ( kount bodies ,jp, j13, 3 ) = T12LOAD2 (jp, j13) ! intermittent discharges of sewage (12)
      twloadsapp ( kount bodies ,jp, j13, 4 ) = T05LOAD2 (jp, j13) ! industrial discharges (5) 
      twloadsapp ( kount bodies ,jp, j13, 5 ) = T39LOAD2 (jp, j13) ! mine waters (39)
      
      twloadsapp ( kount bodies ,jp, j13, 28) = T60LOAD2 (jp, j13) ! other discharges (60)
      twloadsapp ( kount bodies ,jp, j13, 29) = T61LOAD2 (jp, j13) ! private waste waters (61)
      
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
      twloadsapp ( kount bodies ,jp, j13, 23) = T50LOAD2 (jp, j13) ! user-defined (50)
      twloadsapp ( kount bodies ,jp, j13, 24) = T52LOAD2 (jp, j13) ! user-defined (52)
      twloadsapp ( kount bodies ,jp, j13, 25) = T54LOAD2 (jp, j13) ! user-defined (54)
      twloadsapp ( kount bodies ,jp, j13, 26) = T56LOAD2 (jp, j13) ! user-defined (56)
      twloadsapp ( kount bodies ,jp, j13, 27) = T58LOAD2 (jp, j13) ! user-defined (58)
      
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
  101 continue ! do 101 j13 = 1, n13 ! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
  100 continue ! do 100 jp = 1, ndet ! ddddddddddddddddddddddddddddddddddddddddd 

      if ( IPRINT .eq. 0 ) then ! ==============================================
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(110('-')/
     &'Calculated river quality at this catchment boundary ...'/
     &110('-'))
      call write out loads and quality (0) ! at catchment boundary
      call write out monthly river quality (2) ! at catchment boundary
      endif
      call write data for graph plotting ! catchment boundary
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif ! if ( IPRINT .eq. 0 ) =============================================

      return
      end



*     discharge with zero flow -------------------------------------------------
      subroutine zero discharge flow 17
      include 'COMMON DATA.FOR'

      if ( IPRINT .eq. 1 ) return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1015)
 1015 format(110('-')/
     &'Calculated river quality at zero flow discharge ...'/110('-'))
      call write out loads and quality (0) ! at zero flow discharge
      call write out monthly river quality (2) ! at zero flow discharge
      endif

*     write out the results downstream of tributary for graph-plotting ---------
      call write data for graph plotting ! discharge with zero flow
      endif

      return
      end


*     flow gauge ---------------------------------------------------------------
      subroutine river flow gauge
      include 'COMMON DATA.FOR'

      IF = JF(KFEAT)
      if ( IF .ge. 1 ) then ! +++++++++++++++++++++++++++++++++++++++++++++++++++
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

      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 2 (F(IF,1),F(IF,2))
      write(01,3045)valchars10,FUNIT,valchars11,FUNIT
 3045 format(77('=')/
     &'Observed flow in the river ...',26X,'Mean =',a10,1x,a4/
     &38X,'95-percentile low flow =',a10,1x,A4/77('='))


      endif ! if ( nobigout .le. 0 ) then ======================================
      else ! if ( IF .ge. 1 ) ++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( nobigout .le. 0 ) then
      write(01,3845)
 3845 format(77('-')/
     &'No flow date were specified for this flow gauge ...'/77('-'))
      endif ! if ( nobigout .le. 0 ) ===========================================
      endif ! if ( IF .ge. 1 ) +++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3145)valchars10,FUNIT,valchars11,FUNIT
 3145 format(77('+')/
     &'Calculated flow in the river ...',24X,'Mean =',a10,1x,a4/
     &38X,'95-percentile low flow =',a10,1x,A4/77('+'))
      endif ! if ( nobigout .le. 0 ) ===========================================

      if ( ical13 .eq. 0 ) then
      call write out loads and quality (0) ! at flow gauge
      call write out monthly river quality (2) ! at flow gauge
      call write data for graph plotting ! river flow gauge
      endif

      return
      end



*     small stream or tributary ------------------------------------------------
      subroutine tributary
      include 'COMMON DATA.FOR'

      call initialise data for mass balance ! -------------------- tributary (2)

      fupmx = Flow(1) ! ------------------------------------------ tributary (2)

      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
      if ( nobigout .le. 0 ) then ! ============================================
      write(01,1009)
 1009 format(/110('-')/
     &'River quality just upstream of tributary ...'/110('-'))
      if ( ical13 .eq. 0 ) then
      write(34,1009)
      endif
      endif ! if ( nobigout .le. 0 ) ===========================================
      call write out loads and quality (0) ! u/s of tributary
*     call write out monthly river quality (5) ! u/s of tributary
      
  970 if ( FLOW(1) .lt. 1.0E9 ) goto 70
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow exceeds a billion ',24x,'...',7x,'at ',
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
      if ( nobigout .le. 0 .and. ical .ne. 3 ) then
      if ( IDIST .gt. 0 ) then
      call sort format 2 (F(IF,1),F(IF,2))
      write(01,2012)valchars10,FUNIT,IF,valchars11,FUNIT
 2012 format('Flow discharged from tributary:',25X,
     &'Mean =',a10,1X,A4/'Number of data set:',i6,
     &21x,'95% exceedence =',a10,1x,a4/77('-'))
      else ! if ( IDIST .gt. 0 ) otherwise
      call sort format 1 (F(IF,1))
      write(01,2812) valchars10,FUNIT
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
      endif
 9965 format(
     &'These flows are from a non-parametric distribution ... ',
     &'File: ',a64/77('-'))
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
      write(33,8920)
 8920 format(/'*** The specified non-parametric data do not exist ...')
      call stop
      endif
 8970 continue

*     note whether the data are monthly ========================================
      if ( PDRF(IF) .eq. 5 ) then ! ============================================
*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
      write(01,7920)FLMONTHsmall(1,icod)
      write( *,7920)FLMONTHsmall(1,icod)
      write(09,7920)FLMONTHsmall(1,icod)
      write(33,7920)FLMONTHsmall(1,icod)
 7920 format(/'*** Monthly data specified do not exist ... File: ',a64)
      call stop
 2970 continue
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      write(01,1966) FLMONTHsmall(1,icod)
 1966 format(33x,77('-')/33x,
     &'These flow data are from monthly distributions ... '/33x,
     &'File: ',a64/33x,77('-'))
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      endif ! if ( PDRF(IF) .eq. 5 ) ! =========================================


*     note whether the data are monthly structure ==============================
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flow ===========
*     identify the file with the monthly structure data ------------------------
      do 3969 i = 1, M8
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3970
 3969 continue
 3974 continue
      write( *,*)'Stopped 98654'
      call stop
 3970 continue
      if ( nobigout .le. 0 .and. ical .ne. 1 ) then ! --------------------------
      write(01,3966) FLSTRUCT(1,icod)
 3966 format(77('-')/
     &'These flows have a monthly structure (type 8) ',
     &' ... File: ',a64)
      endif ! if ( nobigout .le. 0 .and. ical .ne 1 ) --------------------------
      endif ! monthly structure ================================================

*     check for valid data on river quality ------------------------------------
      IQ = JQ(KFEAT)
      if ( quolity data (IQ,1,1) + 1.0 .lt. 0.0 ) then
      write(01,1020)IQ,uname(KFEAT)
      write(33,1020)IQ,uname(KFEAT)
      write(09,1020)IQ,uname(KFEAT)
 1020 format(/'*** Quality data',I6,' specified for feature',
     &' do not exist ... ',a37)
      call change colour of text (10) ! green
      write( *,1920)IQ,uname(KFEAT)
 1920 format('*** Quality data',I6,' specified for feature',
     &' do not exist ...  ',a37)
      call set screen text colour
   12 write(01,1006)
 1006 format(/'*** In ...... tributary.'/77('-'))
      endif

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 .or. ICAL .eq. 1 ) goto 73
      !if ( nobigout .le. 0 ) write(33,2044)IQ,uname(KFEAT)
 2044 format('Quality of tributary flow: data set:',I6,3x,a37)
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
 8151 format(77('-')/
     &'*** 95-percentile low flow of ',a10,
     &' exceeds the mean of ',a10/'*** Check the set of flow ',
     &'data number',i6)
	write(33,8152)
 8152 format('*** The 95-percentile low flow was set to 25% of ',
     &'the mean ... '/77('-'))
      if ( kerror .eq. 1 ) then
      call change colour of text (37) ! dull turquoise
      write( *,8157)uname(Kfeet),IF
 8157 format('*** Zero 95-percentile flow set to 25% of ',
     &'the mean',1x,'...',7x,a37,7x,'Set number',i7,' .......')
      endif
      call set screen text colour
      F(IF,2) = 0.25 * F(IF,1) 
      EF5 = 0.25 * EFM
      endif
      
      if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08) then
      write(33,8951)IF,uname(Kfeet)
 8951 format(77('-')/
     &'*** Zero 95-percentile low flow ... '/
     &'*** Check set of flow data number',i6,' ... for: ',a37/
     &'*** The 95-percentile was set at 1% of the mean ... '/77('-'))
      if ( kerror .eq. 1 ) then
      call change colour of text (37) ! dull turquoise
      write( *,8956)uname(Kfeet),IF
      call set screen text colour
 8956 format('*** Zero 95-percentile flow set at 1% of ', ! tributary
     &'the mean',2x,'...',7x,a37,7x,'Set number',i7,' .......')
      endif
      F(IF,2) = 0.01 * F(IF,1) 
      EF5 = 0.01 * EFM
      endif ! if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08)

*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( PDRF(IF) .eq. 2 .or. PDRF(IF) .eq. 3 ) then
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFStest = 2.7057+2.*ALOG((EFM+EF3)/TEST)

      if (GEFStest .le. 0.0) then
      write(01,9099)UNAME(KFEAT)
      write( *,9099)UNAME(KFEAT)
      write(09,9099)UNAME(KFEAT)
      write(33,9099)UNAME(KFEAT)
 9099 format(///'#### Flow error for the Feature called - ',A32/
     &'#### Simulation stopped in TRIBUTARY ...')
      call stop
      endif

      GEFS = SQRoot(1044,GEFStest) - 1.6449
      EFStest = EXP(GEFS*GEFS)-1.0
      if (EFStest .le. 1.0E-10) then
      write(01,8099)UNAME(KFEAT)
      write( *,8099)UNAME(KFEAT)
      write(09,8099)UNAME(KFEAT)
      write(33,8099)UNAME(KFEAT)
 8099 format(///'#### Flow error for Feature called - ',A32//
     &'#### Simulation stopped in TRIB ...')
      call stop
      endif
      EFS = EFM*SQRoot(1045,EFStest)
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
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
      
      do 6957 JP = 1, ndet ! ===================================================
      if ( Qtype (JP) .eq. 4 ) goto 6957 ! -------------------------------------

      call inititialise the stores for the estimates of load 
      IQDIST = PDRC (IQ,JP) ! type of distribution - STREAM OR TRIBUTARY -------
      ECM = quolity data (IQ,JP,1) ! mean --------------------------------------
      ECS = quolity data (IQ,JP,2) ! standard deviation ------------------------

      EC3 = 0.0
      if ( PDRC (IQ,JP) .eq. 3) EC3 = quolity data(IQ,JP,3)

*     default correlation for tributary quality on tributary flow --------------
      CO5 = RFCL(JP) ! correlation for tributary quality on tributary flow -----
*     --------------------------------------------------------------------------     
      if ( quolity data (IQ,JP,MO) .ne. -9.9 ) then ! over-write the default ---
      CO5 = quolity data (IQ,JP,MO) ! special correlation ----------------------
      endif
      
      CO2 = 1.0 ! default correlation of tributary flow on main river flow -----
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F (IF,MO) ! special correlation ----------

      ifdiffuse = 0

      call mass balance ! from tributary and this determinand ------------------
      call accumulate the loads ! from tributary
      call check the correlation coefficients by regression ! from tributary ---

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 ) then ! ----------------------------------------------
      xnum = QNUM(IQ,JP)
      call get sampling rates for river quality ! tributary --------------------
     &(fupmx,C(JP,1),QUALN(JP),EFM,ECM,xnum,IQDIST)

      endif ! if ( JSKIP .eq. 0 ) ----------------------------------------------
 6957 continue ! do 6957 JP = 1, ndet ==========================================

*     write out the results downstream for graph-plotting ----------------------
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! stream or tributary
      endif
   47 continue

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return ! stream or tributary
      if ( MONF .gt. 1 ) call write shots for river flow ! stream or tributary
      call calculate summaries of river flow

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 3 .and. KSIM .eq. 1 ) call write out river flow
      if ( ical .ne. 3 ) call write out river flow ! stream or tributary
      endif

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1023)
      write(34,1023) ! ---------------------------------------------------- PUR
 1023 format(110('-')/'River quality d/s of tributary ...'/110('-'))
      endif
      call write out loads and quality (0) ! d/s of stream or tributary

      call write out monthly river quality (5) ! d/s of tributary
      endif

      return
      end



      subroutine write out river flow
      include 'COMMON DATA.FOR'

      call sort format 2 (Flow (1),Flow(2))
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then ! ------------------------
      write(01,4)valchars10,FUNIT,valchars11,FUNIT
    4 format(/77('-')/
     &'Flow in river just downstream ...',23X,'Mean =',a10,
     &1x,A4/46X,'95% exceedence =',a10,1x,A4/77('-'))
      write(34,4)valchars10,FUNIT,valchars11,FUNIT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ---------------
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 ) then
      write(31,4)valchars10,FUNIT,valchars11,FUNIT ! ----------------------- EFF
      endif ! if ( JT (feeture) .eq. 03 etc -------------------------------------
      endif ! if ( nobigout .le. 0 .and. ical13 .eq. 0 ) -----------------------
     
      return
      end



      subroutine write mean and standard deviation
      include 'COMMON DATA.FOR'

      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) write(01,6)
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if ( PDRC(IQ,J) .lt. 6 .or. PDRC(IQ,J) .eq. 10 ) then
      if ( ical13 .eq. 0 ) write(01,1022)DNAME(J),valchars10,UNITS(J),
     &valchars11,UNITS(J)
 1022 format(A11,43X,'  Mean =',a10,1x,A4/42x,
     &'Standard deviation =',a10,1x,a4)
      endif

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads   
      if ( ical13 .eq. 0 ) write(01,1522)DNAME(J),valchars10,LUNITS(J),
     &valchars11,LUNITS(J)
 1522 format(A11,40X,'Mean load =',a10,1x,a4/42x,
     &'Standard deviation =',a10,1x,a4)
      endif

      if ( PDRC(IQ,J) .eq. 7 .or. PDRC(IQ,J) .eq. 9 .or. 
     &     PDRC(IQ,J) .eq. 11 ) then ! loads  
      if ( ical13 .eq. 0 ) write(01,1622)DNAME(J),valchars10,LUNITS(J),
     &valchars11,LUNITS(J)
 1622 format(A11,40X,'Mean load =',a10,1x,a4/42x,
     &'Standard deviation =',a10,1x,a4)
      endif

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure for river quality
      if ( ical13 .eq. 0 ) write(01,1922)DNAME(J),valchars10,UNITS(J),
     &valchars11,UNITS(J)
 1922 format(A11,43X,'  Mean =',a10,1x,A4/42X,
     &'Standard deviation =',a10,1x,A4)
      endif

*     if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
*     write(01,1965)
*1965 format(77('-')/
*    &'The data for the above determinand are loads not ',
*    &'concentrations ...'/
*    &'They follow a normal distribution ...')
*     endif

*     if ( PDRC(IQ,J) .eq. 7 ) then ! log normal loads
*     write(01,3965)
*3965 format(77('-')/
*    &'The data for the above determinand are loads not ',
*    &'concentrations ...'/
*    &'They follow a log-normal distribution ...')
*     endif

*     note whether the distribution is non-parametric --------------------------
      if ( PDRC(IQ,J) .eq. 4 .or. PDRC(IQ,J) .eq. 9 ) then
*     identify the file --------------------------------------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, J+1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue
      if ( ical13 .eq. 0 ) write(01,7965) flnamesmall(2,icod)
 7965 format('Non-parametric distribution ... File: ',a64)
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
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1966) 
     &FLMONTHsmall(2,icod)
 1966 format(77('-')/
     &'These flows are from monthly distributions ... ',
     &'File: ',a64)
      endif ! if ( PDRC(IQ,J) .eq. 5 ) 
 2974 continue

*     note whether the data are monthly structure monthly for river quality ------
      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure for river quality --------
*     identify the file with the monthly data for river quality ------------------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
	goto 3974
 3970 continue
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1916) 
     &FLSTRUCTsmall(2,icod)
 1916 format(77('-')/
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif
 3974 continue
      endif
    1 continue

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6)
    6 format(77('-'))
      endif

      return
      end



      subroutine write mean and standard deviation 33
      include 'COMMON DATA.FOR'

      if ( nobigout .le. 0 ) then
          
      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      correl = quolity data (IQ,J,MO)
      if ( correl .lt. -9.8 ) correl = 0.0
      if ( PDRC(IQ,J) .le. 3 .or. PDRC(IQ,J) .eq. 10 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1822)DNAME(J),valchars11,valchars10,UNITS(J)
 1822 format(33x,'Diffuse ',a11,' ... ',11x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
      endif
      endif
      
      if ( PDRC(IQ,J) .eq. 4 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1823)DNAME(J),valchars11,valchars10,UNITS(J)
 1823 format(33x,'Diffuse ',a11,' ... ',11x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric)')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 5 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1825)DNAME(J),valchars11,valchars10,UNITS(J)
 1825 format(33x,'Diffuse ',a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly distributions)')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 6 .or. PDRC(IQ,J) .eq. 7 .or. 
     &     PDRC(IQ,J) .eq. 11 ) then ! loads 
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1922)DNAME(J),valchars11,valchars10,LUNITS(J)
 1922 format(33x,'Diffuse ',a11,' LOADS ',9x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
      endif
      endif
      
      if ( PDRC(IQ,J) .eq. 9 ) then  
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1923)DNAME(J),valchars11,valchars10,LUNITS(J)
 1923 format(33x,'Diffuse ',a11,' ... ',18x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric loads)')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure for river quality ======
      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))
      if ( IMDIST8 .eq. 0 ) then ! the quality is expressed as concentrations --
      if (ical13 .eq. 0 ) then
      write(01,1824)DNAME(J),valchars11,valchars10,UNITS(J)
 1824 format(33x,'Diffuse ',a11,' ... ',11x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly structure)')
      endif
      else ! ! the quality is expressed as loads -------------------------------
      if (ical13 .eq. 0 ) then
      write(01,2824)DNAME(J),valchars11,valchars10,LUNITS(J)
 2824 format(33x,'Diffuse ',a11,' ... ',18x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly structure)')
      endif
      endif ! if ( IMDIST8 .eq. 0 ) --------------------------------------------
      endif ! if ( PDRC(IQ,J) .eq. 8 ) =========================================

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*     if (ical13 .eq. 0 ) write(01,1965)correl
 1965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,'They follow a normal distribution ...',
     &'... correlation = ',f6.3)
      endif
      endif

      if ( PDRC(IQ,J) .eq. 7 .or. PDRC(IQ,J) .eq. 11 ) then ! log normal loads -
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*     if (ical13 .eq. 0 ) write(01,3965)correl
 3965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,
     &'They follow a log-normal distribution ... correlation = ',f6.3)

      call sort format 2 (quolity data(IQ,J,1),quolity data(IQ,J,2))

      if ( PDRC(IQ,J) .eq. 11 ) then ! -----------------------------------------
      write(33,1682)DNAME(J),PDRC(IQ,J),IQ,valchars10,LUNITS(J),
     &valchars11,LUNITS(J),quolity data(IQ,J,3),quolity data(IQ,J,4),
     &quolity data(IQ,J,5),quolity data(IQ,J,6)
 1682 format(110('=')/'Diffuse input from power curve for ',
     &a11/110('-')/'Data-type: 'i5,'     Data-set: 'i5/110('-')/
     &'         Mean load ',a10,1x,a4/        
     &'Standard Deviation ',a10,1x,a4/
     &'       Power Index ',f10.3/
     &'Base Concentration ',f10.3/
     &'Cut-off Percentile ',f10.3/
     &'       Correlation ',f10.3/110('=')/)
      endif ! if ( PDRC(IQ,J) .eq. 11 ) ----------------------------------------

      endif
      endif

*     note whether the distribution is non-parametric --------------------------
      if ( PDRC(IQ,J) .eq. 4 .or. PDRC(IQ,J) .eq. 9 ) then
*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, J+1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue
*     if (ical13 .eq. 0 ) write(01,7163) dname(j),flnamesmall(2,icod)
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
*     if (ical13 .eq. 0 ) write(01,7363) FLMONTHsmall(2,icod)
*7363 format(33x,'These data are from monthly distributions ...',
*    &' File: ',a64)
      endif
 2974 continue

*     note whether the data are monthly structure for river quality ------------
      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure for river quality ------
*     identify the file with the monthly structure data for river quality ------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
      goto 3974
 3970 continue
      if (ical13 .eq. 0 ) then
      write(01,1916) FLSTRUCTsmall(2,icod)
 1916 format(33x,77('.')/33x,
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif
      endif !  ! monthly structure
 3974 continue

    1 continue
      endif
      return
      end

      
      subroutine write mean and standard deviation 33a
      include 'COMMON DATA.FOR'

      if ( nobigout .le. 0 ) then ! --------------------------------------------
      do 1 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 1
      if ( PDRC(IQ,J) .le. 3 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1822)DNAME(J),valchars11,valchars10,UNITS(J)
 1822 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
      endif
      endif
      
      if ( PDRC(IQ,J) .eq. 4 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) write(01,1823)DNAME(J),valchars11,
     &valchars10,UNITS(J)
 1823 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric)')
      endif

      if ( PDRC(IQ,J) .eq. 5 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) write(01,1825)DNAME(J),valchars11,
     &valchars10,UNITS(J)
 1825 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly distributions)')
      endif

      if ( PDRC(IQ,J) .eq. 6 .or. PDRC(IQ,J) .eq. 7 .or. 
     &     PDRC(IQ,J) .eq. 11 ) then ! loads  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1829)DNAME(J),valchars11,valchars10,LUNITS(J)
 1829 format(33x,a11,' LOADS ',17x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
      endif
      endif

      if ( PDRC(IQ,J) .eq. 9 ) then  
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1723)DNAME(J),valchars11,valchars10,LUNITS(J)
 1723 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Non-parametric loads)')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure   
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2))
      if (ical13 .eq. 0 ) then
      write(01,1824)DNAME(J),valchars11,valchars10,UNITS(J)
 1824 format(33x,a11,' ... ',19x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,'   (Monthly structure)')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 6 ) then ! normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*     if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1965)
 1965 format(33x,'The data for the above determinand are loads not ',
     &'concentrations ...'/33x,'They follow a normal distribution ...')
      endif
      endif

      if ( PDRC(IQ,J) .eq. 7 .or. PDRC(IQ,J) .eq. 11 ) then ! log normal loads
      if ( quolity data(IQ,J,1) .gt. 1.0e-10 ) then
*     if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,3965)
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
*     if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,7163) 
*    &dname(j),flnamesmall(2,icod)
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
*     if ( nobigout .le. 0 .and. ical13 .eq. 0 ) 
*    &write(01,7363) FLMONTHsmall(2,icod)
*7363 format(33x,'These data are from monthly distributions ...',
*    &' File: ',a64)
      endif ! if ( PDRC(IQ,J) .eq. 5 )
 2974 continue

      if ( PDRC(IQ,J) .eq. 8 ) then ! monthly structure for river quality ------
*     identify the file with the monthly structure data for river quality ------
      do 2961 i = 1, M9
      icod = i
      if ( istruct ( 1, i, J+1 ) .eq. IQ ) goto 3970
 2961 continue
      goto 3974
 3970 continue
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,1916) FLSTRUCTsmall(2,icod)
 1916 format(33x,77('.')/33x,
     &'These data have a monthly structure ',
     &'(type 8) ... File: ',a64)
      endif
      endif ! if ( PDRC(IQ,J) .eq. 8 ) monthly structure for river quality -----
      
 3974 continue

    1 continue
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      return
      end




*     put data to a file for plotting graphs -----------------------------------
      subroutine write data for graph plotting
      include 'COMMON DATA.FOR'

      if ( JSKIP .gt. 0 ) return

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! graph plotting ------
      call get summaries of loads

*     loop on the determinands -------------------------------------------------
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4) then
      if ( kdecision(J) .ne. 8 ) then

*     these are the variables for plotting -------------------------------------
*     --------------------------------------------------------------------------
*     CL(1... is the mean concentration
*     CL(2... is the lower confidence limit on the mean concentration
*     CL(3... is the upper confidence limit on the mean concentration
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
      if ( detype (J) .ge. 900 ) then ! dissolved and solid metal --------------
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
     &(3,J,CP2(1,J),CP2(2,J),CP2(3,J),CP2(4,J),CP2(5,J),CP2(6,J),
     &CP2(7,J),CP2(8,J),CP2(9,J),CP2(10,J),CP2(11,J),CP2(12,J))
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J), ! dissolved
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J), ! solid
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
      endif ! dissolved and solid metal ----------------------------------------
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

*     provide dats for Chris Page's graphs (26-03-2007 -------------------------
*     if ( virtualreach .eq. -1 ) then
      JTK = JT(KFEAT)
*     exclude these features ---------------------------------------------------
      if ( JTK .eq. 24 .or.  
     &     JTK .eq. 6  .or.
     &     JTK .eq. 25 .or. JTK .eq. 27 .or. JTK .eq. 29 .or.  
     &     JTK .eq. 31 .or. JTK .eq. 33 .or. JTK .eq. 35 .or. 
     &     JTK .eq. 37 .or. JTK .eq. 40 .or. JTK .eq. 42 .or.
     &     JTK .eq. 13 .or. JTK .eq. 15 .or. JTK .eq. 46 .or.
     &     JTK .eq. 14 .or. JTK .eq. 16 .or. JTK .eq. 48 .or.
     &     JTK .eq. 50 .or. JTK .eq. 52 .or. JTK .eq. 54 .or. 
     &     JTK .eq. 56 .or. JTK .eq. 58 .or.
     &     JTK .eq. 51 .or. JTK .eq. 53 .or. JTK .eq. 55 .or. 
     &     JTK .eq. 57 .or. JTK .eq. 59 ) goto 1492  ! #########################
     
      numtype = JT(KFEAT)
      if ( numtype .eq. 60 ) numtype = 5
      if ( numtype .eq. 61 ) numtype = 5

      write(22,300)jreach(kfeat), ! ----- old graph ---- at a feature ------ SGR
     &Length of main river-adist(jreach(kfeat)), ! ----- old graph --------- SGR
     &UNAME(KFEAT),RNAME(JREACH(KFEAT)),numtype ! ------ old graph --------- SGR
  300 format(I4,',',1PE11.4,',',' " D/S ',A40,'","',A16,'",',i4)! ---------- SGR
      kount22 = kount22 + 1 ! number of lines of graph plotting data ------- SGR
     
      write(22,301)'[Flow    Calc]',FLOW(1), ! ------ old graph ------------ SGR
     &flow(3),FLOW(2),flow(4), ! -------------------- old graph ------------ SGR
     &'[Flow    Obsd]',(COB(JCP,1),JCP=1,4) ! ------- old graph ------------ SGR         
      kount22 = kount22 + 1 ! number of lines of graph plotting data ------- SGR
      
*     These are the variables for plotting --------------------------------- SGR
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
*     CL(12.. is the upper confidence limit on this percentile ------------- SGR

      do j = 1, MP10 ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (J) .ne. 4 ) then ! +++++++++++++++++++++++++++++++++++++++++++
*     concentrations ------------------------------------------------------- SGR
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12) ! ---- SGR
*     loads ---------------------------------------------------------------- SGR
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12) ! ---- SGR
*     observed concentrations ---------------------------------------------- SGR
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(COB(JCP,J+1),JCP=1,4) ! -- SGR
*     observed loads ------------------------------------------------------- SGR    
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(COD(JCP,J),JCP=1,4) ! ---- SGR
*     targets -------------------------------------------------------------- SGR
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J) ! ------------------ SGR
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0 ! ----------------------- SGR
  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4)) ! -------------- SGR
  302 format(1x,a4,I1,A9,12(',',1PE11.4)) ! -------------------------------- SGR
  303 format(1x,a4,I1,A9,4(',',1PE11.4)) ! --------------------------------- SGR
  304 format(1x,a4,I1,A9,1(',',1PE11.4)) ! --------------------------------- SGR

      if ( XA(j)+X90(j)+X95(j)+X99(j) .lt. 0.000001 ) then
      write(230+j,305)UNAME(KFEAT),RNAME(JREACH(KFEAT)), !--------------- Gi.CSV
     &jreach(kfeat),
     &JT(KFEAT),Length of main river-adist(jreach(kfeat)),
     &rlength(JREACH(KFEAT)),Length of main river,
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &lconmon(1,j,1,13),lconmon(2,j,1,13)
      else
      write(230+j,305)UNAME(KFEAT),RNAME(JREACH(KFEAT)), ! -------------- Gi.CSV
     &jreach(kfeat),
     &JT(KFEAT),Length of main river-adist(jreach(kfeat)),
     &rlength(JREACH(KFEAT)),Length of main river,
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &lconmon(1,j,1,13),lconmon(2,j,1,13),
     &XA(j),XAl(j),XAu(j),X90(j),X90l(j),X90u(j), ! -- observed values -- Gi.CSV
     &X95(j),X95l(j),X95u(j),X99(j),X99l(j),X99u(j) !  observed values -- Gi.CSV
  305 format('d/s of ',A40,',',A16,',',I4,',',i4,
     &80(',',1PE11.4))
      
      XAl(j) = 0.0
      X90l(j) = 0.0
      X95l(j) = 0.0
      X99l(j) = 0.0
      XAu(j) = 0.0
      X90u(j) = 0.0
      X95u(j) = 0.0
      X99u(j) = 0.0
      
      endif


      endif
      enddo

 1492 continue ! ===============================================================


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
     &FLOW(1),(flowstats (i), i=1,9),(propstats (i),i=1,9)
 2942 format("D/S ",A40,f8.2,10f9.1/
     &4x,'Proportions of effluent flow ...',25x,9f9.2)

      call write GIS data to channel 42 43 24 and 25
      endif ! if ( ical13 .eq. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( noGIS .eq. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      return
      end





*     calculations for features - effluent discharges --------------------------
      subroutine effluent discharge
      include 'COMMON DATA.FOR'
      character *25 typename
      character *15 namestat
      character *20 run type description,forward calculation
      character *30 dischargedec,targetdec
      real cold(ndet,30)

      call initialise data for mass balance ! effluent discharge

      fupm = Flow(1) ! effluent discharge 
      fup95 = Flow(2)
      
      do idet = 1, ndet ! storage for output to the effluent CSV file ----------
      do j = 1, 30      ! storage for output to the effluent CSV file ----------
      cold(idet,j) = 0.0
      enddo
      enddo
      
      if ( JSKIP .ne. 1 ) then ! ===============================================
      if ( ical13 .eq. 0 ) then ! ==============================================
      if ( nobigout .eq. 0 ) then
      write(01,1009)uname(feeture)
      write(31,1009)uname(feeture)
 1009 format(/110('-')/
     &'River quality just upstream of effluent discharge ... ',a37
     &/110('-'))

      call write out loads and quality (0) ! u/s of discharge
      call write out monthly river quality (4) ! at effluent discharge
      call proportion of effluent (4)
      endif
      endif ! if ( ical13 .eq. 0 ) =============================================
      endif ! if ( JSKIP .ne. 1 ) ==============================================

      if ( FLOW(1) .gt. 1.0E9 ) then ! -----------------------------------------
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow exceeds a billion ',24x,'...'7x,'at ',
     &a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(01,7817)UNAME(KFEAT)
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .8.'/
     &'*** Calculations proceeding ...'/100('*'))
      flow(1) = 999999.9
      flow(2) = 99999.9
      endif ! if ( FLOW(1) .gt. 1.0E9 ) ----------------------------------------

      IQ = JQ(KFEAT) ! identify the set of effluent data

      call assess the need to track the discharge
      call write information on effluent flow 

      if ( pollution data(IQ,1,1)+1.0 .lt. 0.0 ) then
      call change colour of text (20) ! bright red
      write( *,9006)IQ,UNAME(KFEAT)
 9006 format('*** Quality data',i6,' specified for feature ',
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
 1006 format(/'*** Stopped in ...... effluent discharge'/80('-'))
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

*     initialise correlation as default values ---------------------------------
*     discharge flow on river flow for sewage treatment works ------------------
      CO2 = 0.6 ! sewage works

*     the correlation coefficient is set to zero for industrial effluents ------
      if ( JT(KFEAT) .eq. 05 ) CO2 = 0.0 ! industrial discharge
      if ( JT(KFEAT) .eq. 39 ) CO2 = 0.0 ! mine waters
      if ( JT(KFEAT) .eq. 60 ) CO2 = 0.0 ! other point sources
      if ( JT(KFEAT) .eq. 61 ) CO2 = 0.0 ! private wastewaters
*     negative discharge (abstraction) -----------------------------------------
      if ( JT(KFEAT) .eq. 19 ) CO2 = 0.0

*     impose special non-default correlation -----------------------------------
      if ( FE (IQ,4) .ne. -9.9 ) CO2 = FE (IQ,4)

      do is = 1, NS
      ufms (IS) = fms (IS) ! store the upstream flows for extra calculations ---
      dfms (IS) = fms (IS) ! intitialise downstream flows for extra calculations
      enddo
      
      call write effluent discharge quality

      jp1 = 0
      do 6930 JP = 1, NDET
      if ( qtype (JP) .eq. 4 ) goto 6930
      cld(JP,1) = -9.9 ! initialise mean quality u/s of the discharge
      cld(JP,3) = -9.9 ! initialise the 95-percentile
      cld(JP,4) = -9.9 ! initialise the 90-percentile
      cld(JP,5) = -9.9 ! initialise the 99-percentile 
      
      kdecision(JP) = 1 ! initialise as "retain current discharge quality" -----
      do is = 1, NS
      fms (IS) = ufms (IS)
      enddo
      jp1 = jp1 + 1
      do is = 1, NS ! store the upstream quality for an extra calculations -----
      ucms (JP,IS) = cms (JP,IS)
      do ip = 1, nprop
      ulms (ip,JP,IS) = lms(ip,JP,IS)
      enddo
      enddo ! do is = 1, NS -- store the upstream quality ----------------------


*     use upstream river quality (CMS) to obtain summary statistics ------------
      call get summaries of river quality from the shots ! SR effluent discharge
      
      call inititialise the stores for the estimates of load 

      cold(JP,1) = C(JP,1) ! mean quality u/s of the discharge
      cold(JP,2) = C(JP,2) ! st.dev for quality u/s of the discharge
      cold(JP,3) = C(JP,3) ! 95-percentile u/s of the discharge
      cold(JP,4) = C(JP,4) ! 90-percentile u/s of the discharge
      cold(JP,5) = C(JP,5) ! 99-percentile u/s of the discharge
      cold(JP,6) = CO1
      cld(JP,1) = C(JP,1) ! mean quality u/s of the discharge
      cld(JP,3) = C(JP,3) ! 95-percentile u/s of the discharge
      cld(JP,4) = C(JP,4) ! 90-percentile u/s of the discharge
      cld(JP,5) = C(JP,5) ! 99-percentile u/s of the discharge 
      
      call load calculation per determinand ! for upstream river flows
      cold(JP,21) = XLOAD(JP,1,i13) ! mean river load u/s of discharge
      cold(JP,22) = XLOAD(JP,2,i13) ! st.dev of river load u/s of discharge
      
      IQDIST = PDEC(IQ,JP) ! type of discharge distribution --------------------
*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
      CO5 = EFCL(JP) ! default - discharge quality on discharge flow -----------
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
      cold(JP,20) = TECX ! "Dis_conc_95_final" - the starting value ------------

      if ( ical .gt. 6 .and. detype (JP) .ne. 104 ) then
      ifdiffuse = 0
      
      call mass balance ! from effluent discharge (initial forward calculation)
      
      call check the correlation coefficients by regression ! from effluents ***
      call get summaries of river quality from the shots
      cold(JP,7) = C(JP,1)
      cold(JP,8) = C(JP,2)
      cold(JP,9) = C(JP,3) 
      cold(JP,10) = C(JP,4) 
      cold(JP,11) = C(JP,5) 
      cold(JP,13) = eload (JP,i13) ! current discharge load - effluent discharge
      cold(JP,14) = eloads (JP)    ! current discharge load - effluent discharge
      cold(JP,27) = eload (JP,i13) ! set final load to current discharge load
      cold(JP,28) = eloads (JP)    ! set final load to current discharge load
      call load calculation per determinand ! set up back calculation
      cold(JP,23) = XLOAD (JP,1,i13) ! mean river load d/s current discharge
      cold(JP,24) = XLOAD (JP,2,i13) ! st.dev d/s current discharge
      cold(JP,25) = XLOAD (JP,1,i13) ! initialise as mean d/s current discharge
      cold(JP,26) = XLOAD (JP,2,i13) ! initialise st.dev d/s current discharge
      kdecision(JP) = 1 !  ! initialise as retain current discharge quality ----
      call load the upstream flow and quality 
      endif ! if ( ical .gt. 6 ) 
     
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     Start of block calculating River Needs Discharge Standards ---------------
*     **************************************************************************
*     branch according to mode of calculation ----------------------------------
*     the following applies to calculating River Needs Standards ---------------
      if ( ical .lt. 07 ) goto 1833
      if ( NOBACK(JP) .eq. 99 ) then ! no calculations required for determinand
      kdecision(JP) = 12 ! retain current quality of effluent
      goto 1833
      endif
      if ( SKIPPEFF(KFEAT) .eq. 1 ) then ! effluent excluded ===================
      kdecision(JP) = 12 ! retain current quality of effluent
      if ( nobigout .le. 0 ) then
      write(01,1715)DNAME(JP) ! -------------------------------------------- OUT
      write(31,1715)DNAME(JP) ! -------------------------------------------- EFF
 1715 format(110('=')/'Effluent excluded from ',
     &'meeting targets ... ',A11,'...'/
     &'The current quality has been retained ...'/
     &110('='))
      endif
      goto 1833
      endif ! if ( SKIPPEFF(KFEAT) .eq. 1 ) ====================================
      
*     check whether to exclude intermitent discharges (12) =====================
      if ( JT(KFEAT) .eq. 12 .and. mrqs(JP) .lt. 6 ) then ! exclude discharge ==
      kdecision(JP) = 12 ! retain current quality of effluent
      if ( nobigout .le. 0 ) then
      istx = mrqs(JP)
      namestat = 'mean'
      if ( istx .eq. 2 ) namestat = '95-percentile'
      if ( istx .eq. 3 ) namestat = '90-percentile'
      if ( istx .eq. 4 ) namestat = '05-percentile'
      if ( istx .eq. 5 ) namestat = '10-percentile'
      if ( istx .eq. 6 ) namestat = '99-percentile'
      write(01,7715)DNAME(JP),namestat ! ----------------------------------- OUT
      write(31,7715)DNAME(JP),namestat ! ----------------------------------- EFF
 7715 format('Intermittent discharge is excluded from ',
     &'meeting the target for: ',A11/
     &'The current discharge quality has been retained ... '/
     &'The downstream river quality target is a ',a15/77('#'))
      endif
*     MRQS - summary statistic used to define river quality targets:
*     1, 2, 3 for mean, 95-percentile and 90-percentile
*     4 and 5 for the 5 and 10-percentile
*     6 for the 99-percentile
      goto 1833
      endif ! if ( JT(KFEAT) .eq. 12 etc ... intermittent discharge excluded ===
      

      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
      kdecision(JP) = 11
      cold(JP,18) = -999.99 ! store final discharge concentration --------------
      cold(JP,19) = -999.99 
      cold(JP,20) = -999.99 ! Dis_conc_95_final
      cold(JP,29) = ECM
      cold(JP,30) = ECS
      if ( nobigout .le. 0 ) then
      write(01,1705)DNAME(JP) ! -------------------------------------------- OUT
      write(31,1705)DNAME(JP) ! -------------------------------------------- EFF
 1705 format(110('-')/'Effluent quality is expressed as load ',
     &'rather than concentration ... ',A11,'... ',3x,
     &'Current quality retained'/110('-'))
      endif
      goto 1833
      endif ! if ( IQDIST .eq. ... loads LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

      !if ( ECM .lt. 1.0e-10 ) goto 1833

*     check for a river quality target -----------------------------------------
      Mtargit = 0 ! initalise as having no target ----------------------------- 
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
      
      if ( skippeff(feeture) .ne. 9999 ) then
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-spepific target
      targit = abs (class limmits (ic,JP))
      endif
      enddo ! do ic = 1, nclass
      endif !
      endif ! if ( IQSreach .gt. 0 )
      
      RQO(JP) = targit ! use the target for graphs ---------- effluent discharge
      MRQO(JP) = MRQS(JP) ! summary statistic for the standard -----------------
      Mtargit = MRQS(JP)
      endif ! if ( QTYPE (JP) .ne. 4 ) then
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
      
     
*     ##########################################################################
      if ( Mtargit .eq. 0 ) then ! there is no target ##########################
      call mass balance ! from effluent discharge (forward calculation) --------
      call check the correlation coefficients by regression ! from effluents ***
      call get summaries of river quality from the shots ! effluents (forward) -

      cold(JP,7) = C(JP,1)
      cold(JP,8) = C(JP,2)
      cold(JP,9) = C(JP,3) 
      cold(JP,10) = C(JP,4) 
      cold(JP,11) = C(JP,5) 
      cold(JP,13) = eload (JP,i13) ! current discharge load - effluent discharge
      cold(JP,14) = eloads (JP)    ! current discharge load - effluent discharge
      cold(JP,27) = eload (JP,i13) ! set final load to current discharge load
      cold(JP,28) = eloads (JP)    ! set final load to current discharge load
      call load calculation per determinand ! set up back calculation
      cold(JP,23) = XLOAD (JP,1,i13) ! mean river quality d/s current discharge
      cold(JP,24) = XLOAD (JP,2,i13) ! st.dev d/s current discharge
      cold(JP,25) = XLOAD (JP,1,i13) ! initialise as mean d/s current discharge
      cold(JP,26) = XLOAD (JP,2,i13) ! initialise st.dev d/s current discharge
      kdecision(JP) = 1 ! initialise as retain current -------------------------
      call load the upstream flow and quality 
      goto 1833
      endif ! if ( Mtargit .eq. 0 ) there is no target #########################
*     ##########################################################################      
      
      
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
*     calculate discharge quality needed to meet percentile standards ~~~~~~~~~~
*     of river quality ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( MTargit .ne. 0 .and. Mtargit .ne. 1 ) then ! standard is a percentile
          
      call meet river targets set as percentiles ! +++++++++++++++++++++++++++++
      
      call get effluent quality 95 percentile
      if ( Kdecision(JP) .eq. 9 ) goto 1833
      TECX = ECX
      cold(JP,18) = ECM ! store final discharge quality - from 95%ile target ---
      cold(JP,19) = ECS
      cold(JP,20) = TECX ! Dis_conc_95_final
      if ( Kdecision(JP) .eq. 9 ) goto 1833
      else ! if ( Mtargit .eq. 1 ) ... the standard is a mean ==================
*     calculate discharge quality needed to meet mean standards ----------------
*     of river quality ---------------------------------------------------------


      call meet river targets set as averages ! ++++++++++++++++++++++++++++++++

      call get effluent quality 95 percentile
      
      if ( kdecision(JP) .eq. 1 ) goto 1833 ! retain current discharge quality -
      if ( kdecision(JP) .ne. 3 ) then !  best achievable has not been installed
      TECX = ECX !  ! best achievable has not been installed -------------------
      cold(JP,18) = ECM ! store final discharge quality - from mean target -----
      cold(JP,19) = ECS ! store standard deviation -----------------------------
      cold(JP,20) = TECX ! "Dis_conc_95_final" - store 95-percentile -----------
      else ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ best achievable has been installed
      cold(JP,18) = TECM ! store best achievable quality mean ~~~~~~~~~~~~~~~~~~
      cold(JP,19) = TECS ! store best achievable standard deviation ~~~~~~~~~~~~
      cold(JP,20) = TECX ! Dis_conc_95_final - store 95-percentile ~~~~~~~~~~~~~
      endif ! if ( kdecision(JP) .ne. 3 )

      endif ! if ( Mtargit .ne. 1 ) ============================================

  
    
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
      if ( qtype (jp) .eq. 3 .and. WEQ(1,JP) .lt. GEQ(1,jp) ) then ! ~~~~~~~~~~~
*     compare the old Biochemical Oxygen Demand with the starting value --------
*     is the old value worse than the new Biochemical Oxygen Demand? -----------
      if ( pollution data (IQ,ndetBOD,1) .ge. BOD ) then ! ~~~~~~~~~~~~~~~~~~~~~
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
      if ( nobigout .le. 0 ) write(31,2954)ECM,UNITS(JP),ECS, ! ------------ EFF
     &UNITS(JP),ECX,UNITS(JP)
 2954 format(
     &'........................................',14x,
     &'  Current discharge quality:        Mean =',F9.2,1X,A4/
     &'........................................',14x,
     &10X,'          Standard Deviation =',F9.2,1X,A4/
     &'........................................',14x,
     &10X,'               95-percentile =',F9.2,1X,A4/
     &110('-'))
*     ==========================================================================
*     BOD is worse than the starting quality (input quality) -------------------
*     retain input discharge quality -------------------------------------------
      else ! if ( pollution data (IQ,ndetBOD,1) .ge. BOD ) then ~~~~~~~~~~~~~~~~
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
      endif ! if ( pollution data (IQ,ndetBOD,1) .ge. BOD ) then ~~~~~~~~~~~~~~~
      endif ! if ( qtype (jp) .eq. 3 .and. WEQ(1,JP) .lt. GEQ(1,jp) ) ~~~~~~~~~~
      endif ! if ( detype (JP) .eq. 104 ) ======================================
      endif ! if ( detype (JP) .ne. 104 ) ======================================
*     ==========================================================================
*     SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SET DO - SE
*     ==========================================================================


      if ( kdecision(JP) .ne. 8 ) then ! ---------------------------------------
      call load calculation per determinand
      cold(JP,25) = XLOAD(JP,1,i13) ! mean d/s of new discharge
      cold(JP,26) = XLOAD(JP,2,i13) ! st.dev d/s of new discharge
      cold(JP,27) = eload(JP,i13)   ! final discharge load -- effluent discharge
      cold(JP,28) = eloads(JP)      ! final discharge load -- effluent discharge
      endif ! if ( kdecision(JP) .ne. 8 ) --------------------------------------

      call accumulate the loads ! after River Needs discharge Standards ========

 1833 continue

     
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     End of block calculating River Needs discharge Standards +++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     ==========================================================================
*     perform the main mass balance calculation with current data ==============
      if ( ical .lt. 07 .or. ! back calculation not requested ==================
*    & NOBACK (JP) .eq. 999 .or. ! =============================================
     & detype (JP) .eq. 104 .or. ! determinand is dissolved oxygen =============
     & kdecision(JP) .eq. 09 .or. ! target not threatened - retained current ===
     & kdecision(JP) .eq. 11 .or. ! discharge defined in terms of load data ====
     & kdecision(JP) .eq. 12 ) then ! discharge is excluded ====================
      ifdiffuse = 0

      call load the upstream flow and quality 
           
      call mass balance ! from effluent discharge (forward calculation) ========
      
      call accumulate the loads ! effluent discharge (forward calculation) =====

      call check the correlation by regression (3) ! from effluents (forward) --
      
      call get summaries of river quality from the shots ! effluents (forward) -

      cold(JP,7) = C(JP,1)
      cold(JP,8) = C(JP,2)
      cold(JP,9) = C(JP,3) 
      cold(JP,10) = C(JP,4) 
      cold(JP,11) = C(JP,5) 
      cold(JP,13) = eload(JP,i13) ! current load ------------ effluent discharge
      cold(JP,14) = eloads(JP)    ! current load ------------ effluent discharge 
      cold(JP,27) = eload(JP,i13) ! --- set final load to current discharge load
      cold(JP,28) = eloads(JP)    ! --- set final load to current discharge load

      call load calculation per determinand
      cold(JP,23) = XLOAD(JP,1,i13) ! mean load d/s of current discharge
      cold(JP,24) = XLOAD(JP,2,i13) ! st.dev d/s of current discharge
      cold(JP,25) = XLOAD(JP,1,i13) ! mean load d/s of current discharge
      cold(JP,26) = XLOAD(JP,2,i13) ! st.dev d/s of current discharge

      endif ! if ( ical .lt. 07 etc ) ==========================================
*     ==========================================================================
 

*     calculate the confidence limits on river water quality -------------------
      if ( JSKIP .eq. 0 ) then ! ===============================================
      xnum = PNUM(IQ,JP)
      call get sampling rates for river quality ! effluent discharges ----------
     &(fupm,C(JP,1),QUALN(JP),EFM,ECM,xnum,IQDIST)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( JSKIP .eq. 0 ) ==============================================
      
 6930 continue ! do 6930 JP = 1, NDET ---- the loop on determinands has finished

      call calculate summaries of river flow

      if ( MONF .gt. 1 ) call write shots for river flow ! effluent discharge

*     write out results d/s of discharge for graph plotting --------------------
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! d/s of effluent discharge
      endif

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 3 .and. KSIM .eq. 1 ) call write out river flow
      if ( ical .ne. 3 ) call write out river flow ! d/s of effluent discharge 
      endif

      if ( ical13 .eq. 0 ) then ! ==============================================
      if ( nobigout .le. 0 ) then ! ============================================
      write(01,2823)uname(feeture) ! --------------------------------------- OUT
      write(31,2823)uname(feeture) ! --------------------------------------- EFF
 2823 format('River quality downstream of the discharge ... ',a37/
     &110('-'))
      write(34,2023)
 2023 format(77('-')/'River quality ...'/77('-'))
      endif
      call write out loads and quality (0) ! d/s of effluent discharge
      call write out monthly river quality (6) ! d/s of effluent discharge

      endif ! if ( ical13 .eq. 0 ) =============================================
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     write details to the EFFLUENT CSV file -----------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      if (itype .eq. 60 ) typename = 'Other Point Sources'
      if (itype .eq. 61 ) typename = 'Private Wastewater'
      
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
      
      changeload = cold(JP,13) - cold(JP,27) ! change in discharge load
      changerload = cold(JP,23) - cold(JP,25) ! change in river load
      delta1 = amax1 (abs(changeload),abs(changerload))
      deltaload = 0.0
      if ( delta1 .gt. 1.0e-8) then ! change in discharge and river loads
      deltaload = 100.0 * (abs(changerload)-abs(changeload))/delta1
      endif

      write(36,2) ! outputs on effluents via the file ............. EFF.CSV file
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
     &cold(JP,13),                 ! Dis_load_mean_orig  
     &cold(JP,14),                 ! Dis_load_st_orig oooooooooo
      
     &FLOW(1),                     ! Riv_F_mean_ down oooooooooo
     &FLOW(2),                     ! Riv_F95_down oooooooooooooo
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
     &cold(JP,28),                 ! Dis_load_st_final
     &changeload,                  ! Dis_load_mean_drop
      
     &changerload                  ! Riv_load_mean-down_drop 
      
      if ( ical .ne. 07 .and. ical .ne. 08 .and. ical .ne. 09 ) then
      if ( deltaload .gt. 0.002 ) then
      if ( ical .gt. 06 ) then
      call sort format1 (deltaload)
      write(33,6566)uname(KFEAT),dname(JP),valchars10 ! -------------------- ERR
 6566 format(/100('-')/'There is a difference in the change in river ',
     &'and discharge load for: ',a40/
     &'For ',a11,' This is ',a10,' %'/100('-')/)
      if ( deltaload .gt. 0.01 ) then ! ----------------------------------------
      call change colour of text (10) ! green
      write( *,5566)uname(KFEAT),dname(JP),valchars10
      call set screen text colour
 5566 format('*** Difference in the change in river & discharge ',
     &'load ...',7x,a40,4x,a11,2x,a10,' %')
      write(09,5568)uname(KFEAT),dname(JP),valchars10 ! -------------------- SCN
      write(31,5568)uname(KFEAT),dname(JP),valchars10 ! -------------------- EFF
 5568 format(110('-')/'Different change in river & discharge ',
     &'load for ',a37,2x,a11,1x,a10,' %'/110('-'))
      endif ! if ( deltaload .gt. 0.01 ) ---------------------------------------
      endif ! if ( ical .gt. 06 ) then
      endif ! if ( deltaload .gt. 0.002 )

      endif ! if ( ical .ne. 9 )
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, ndet

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
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     format statement retained for information --------------------------------
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
*     format statement retained for information --------------------------------
 
      return
      end



*     print out the input data on discharge quality ----------------------------
      subroutine assess the need to track the discharge
      include 'COMMON DATA.FOR'
*     check whether the discharge should be tracked ----------------------------
*     calculate the proportion of the load added by the discharge --------------
      
      if ( ical13 .eq. 1 ) return
      if ( MU .gt. NU ) return ! -----------------------------------------------
      if ( JT(feeture) .eq. 12 ) return
*     ==========================================================================

      mark works = 0
      baggest = 0.0
*     ==========================================================================
      if ( FE(IQ,1) .gt. 0.00000001 ) then ! +++++++++++++++++++++++++++++++++++
      EF = FE(IQ,1)

      do idet = 1, ndet
*     --------------------------------------------------------------------------
      if ( QTYPE (idet) .ne. 3 .and. QTYPE (idet) .ne. 5 ) then
      if ( detype (idet) .ne. 104 ) then
      EP = 0.0
      FP = 0.0 ! initialise the % of target river quality
*     rough estimate of load ---------------------------------------------------
      if ( PDEC (IQ,idet) .eq. 6 .or. PDEC (IQ,idet) .eq. 7 .or.
     &     PDEC (IQ,idet) .eq. 9 .or. PDEC (IQ,idet) .eq. 11 ) then
      EL = pollution data(IQ,idet,1) ! specified discharge load ----------------
      else
      EL = EF * pollution data(IQ,idet,1) ! rough estimate of discharge load --=
      endif
      tergit = RQS(1,idet) ! the river quality target
      if ( tergit .lt. 0.000001 ) tergit = 500 ! set the target to 500
*     add to the total load accumulated so far ---------------------------------
      ED = TELODE2(idet,i13) + EL
*     compute the proportion represented by the new load -----------------------
      if ( ED .gt. 0.0000001 ) EP1 = EL / ED
*     compute the river concentration indicated represented by the new load ----
      if ( FLOW(1) .gt. 0.00001 ) FP = (EL / FLOW(1)) / tergit
*     isolate the biggest proportion across all the determinands ---------------
      baggest = ( amax1 ( FP, baggest) )
      endif ! if ( detype (idet) .ne. 104 )
      endif ! if ( QTYPE (idet) .ne. 3 or 5 ) ----------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
      baggest = 100.0 * baggest ! express as a percentage ----------------------
      endif ! if ( FE(IQ,1) .gt. 0.00000001 ) ++++++++++++++++++++++++++++++++++


*     initially add to the counter of works ------------------------------------
      kountworks = kountworks + 1
      need works = need works + 1
      mark works = 1
      call change colour of text (20) ! bright red
      call set screen text colour
*     check whether the load is big enough to track ============================
      if ( baggest .lt. bagset .and. FLOW(1) .gt. 0.0001 .and. 
     &     baggest .gt. 1.0e-8) then
      if ( ifbatch .eq. 0 ) then ! .and. kerror .eq. 1 ) then
      call change colour of text (14) ! bright yellow
      call sort format 1 (baggest) 
      write( *,4631)valchars10
 4631 format('*** Next effluent is too small for back-tracking ',
     &'its impacts (',a10,' %)')
      call set screen text colour
      write(33,4611)UNAME(KFEAT),valchars10
      write(09,4611)UNAME(KFEAT),valchars10
 4611 format(110('-')/'The effluent load too small for back-tracking ',
     &'the impacts up the rivers ... ',a40/
     &'The contribution is 'a10,' per cent'/110('-'))
      endif !  if ( ifbatch .eq. 0 ) then
      kountworks = kountworks - 1 ! works is too small
      need works = need works - 1 ! works is too small
      kill works = kill works + 1
      mark works = 0
      else ! if ( baggest .lt. bagset etc ======================================
      if  ( kountworks .gt. NUED ) then ! --------------------------------------
      !if ( ifbatch .eq. 0 ) then
      call change colour of text (12) ! orange
      write( *,4531)NUED,need works
 4531 format('*** Too many effluent discharges for back-tracking ',
     &'the impacts ... the limit is ',i5,' (Need ',i5,')')
      call set screen text colour
      call sort format 1 (baggest)
      write(33,4511)NUED,need works,valchars10
      write(01,4511)NUED,need works,valchars10
      write(09,4511)NUED,need works,valchars10
 4511 format(110('-')/'Too many effluent discharges for back-tracking ',
     &'the impacts ... the limit is ',i5,' (Need ',i5,')'/
     &'The next discharge is not being tracked ... it ',
     &'contributes ',a10,' per cent to downstream quality'/110('-'))
      !endif ! if ( ifbatch .eq. 0 )
      kountworks = kountworks - 1 ! too many works
      mark works = 0
      else ! if ( kountworks .gt. NUED ) ---------------------------------------
      identify werks ( kountworks ) = feeture
      endif !  if ( kountworks .gt. NUED ) -------------------------------------
*     --------------------------------------------------------------------------
      endif !  if ( baggest .lt. bagset etc ====================================
*     --------------------------------------------------------------------------

      return
      end




*     print out the input data on discharge quality ----------------------------
      subroutine write information on effluent flow 
      include 'COMMON DATA.FOR'
      
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,2055)IQ,PDEF(IQ),uname(feeture) ! --------------------------- OUT
      write(31,2055)IQ,PDEF(IQ),uname(feeture) ! --------------------------- EFF
 2055 format(/77('=')/
     &'Flow discharged from works: Set:',I6,' Type:',i2,3x,a30)
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
      write(01,1965)flnamesmall(3,icod) ! ---------------------------------- OUT
      write(31,1965)flnamesmall(3,icod) ! ---------------------------------- EFF
 1965 format('Non-parametric distribution ... File: ',a64/77('-'))
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

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,1995) FLMONTHsmall(3,icod)
 1995 format(77('-')/'These data are expressed as ',
     &'monthly distributions ... File: ',a64/77('-'))
      write(31,1995)FLMONTHsmall(3,icod) ! --------------------------------- EFF
      endif

*     open and read the file ---------------------------------------------------
      open(11,FILE = flmonth(3,icod), STATUS='OLD')
*     read the file containing the monthly data --------------------------------
      call read monthly data (3,icod) ! discharge flow - type 5

      if ( ical13 .eq. 0 ) write(01,4398)
 4398 format(
     &'Monthly input data on discharge flow ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift'/ ! discharge flow
     &'  Correlation'/25x'  Deviation','           '/77('-'))

      if ( ical13 .eq. 0 ) write(01,2399)(seas1(i),seas2(i),seas3(i),
     &seas4(i),seas0(i),i=1 ,12)
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
      if ( JSKIP .eq. 0 ) then ! ===============================================
      if ( ical13 .eq. 0 .and. nobigout .le. 0 ) then ! ========================
      call sort format 2 (FE(IQ,1),FE(IQ,2))
      xxxc = FE(IQ,4)
      if ( FE(IQ,4) .eq. -9.9 ) then ! set defaults for correlation ------------
      if ( JT(feeture) .eq. 02 ) xxxc = 1.0
      if ( JT(feeture) .eq. 03 .or. JT(feeture) .eq. 12 ) xxxc = 0.6
      if ( JT(feeture) .eq. 05 .or. JT(feeture) .eq. 39 ) xxxc = 0.0
      if ( JT(feeture) .eq. 60 .or. JT(feeture) .eq. 60 ) xxxc = 0.0
      endif
      if ( JT(feeture) .eq. 12 ) then
      write(01,1212)valchars10,funit,valchars11,funit,xxxc ! --------------- OUT
 1212 format(77('#')/'Flow discharged as effluent:',
     &' Mean of actual discharged flows =',a10,1x,a4/41X,
     &' Standard Deviation =',a10,
     &1x,a4/33X,'Correlation with river flow =',f10.4/77('='))
      write(31,1212)valchars10,funit,valchars11,funit,xxxc ! --------------- EFF
      else
      write(01,1012)valchars10,funit,valchars11,funit,xxxc ! --------------- OUT
 1012 format(77('=')/'Flow discharged as effluent:',20x,
     &' Annual mean =',a10,1x,a4/41X,' Standard Deviation =',a10,
     &1x,a4/33X,'Correlation with river flow =',f10.4/77('='))
      write(31,1012)valchars10,funit,valchars11,funit,xxxc ! --------------- EFF
      endif
      call sort format 1 (FE(IQ,3))
      if ( abs(FE(IQ,3)) .gt. 0.0001 ) then ! ----------------------------------
	write(01,1912)valchars10,funit
      write(31,1912)valchars10,funit ! ------------------------------------- EFF
 1912 format(41X,'              Shift =',a10,1x,a4/77('='))
      do idet = 1,ndet ! -------------------------------------------------------
      enddo ! do idet = 1,ndet ! -----------------------------------------------
      endif ! if ( abs(FE(IQ,3) .gt. 0.0001 ) ----------------------------------
      endif ! if ( ical13 .eq. 0 etc ===========================================
      endif ! if ( JSKIP .eq. 0 ) ==============================================

      return
      end




*     print out the input data on discharge quality ----------------------------
      subroutine write effluent discharge quality
      include 'COMMON DATA.FOR'

      if ( JSKIP .eq. 1 .or. ical .eq. 1 ) return
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(31,2055)IQ,uname(feeture) ! ------------------------------------ EFF
      endif
 2055 format('Quality discharged from works: Set:',I6,3x,a30/77('-'))

*     loop on the determinands -------------------------------------------------
      do 6926 J = 1, ndet
      if ( QTYPE (J) .eq. 4 ) goto 6926
*     --------------------------------------------------------------------------
      if ( PDEC(IQ,J) .lt. 4 ) then
      if ( nobigout .le. 0 ) then
          
*     default correlation coefficients -----------------------------------------
      CO5 = EFCL (J) ! discharge quality on discharge flow
      if ( pollution data (IQ,J,MO) .ne. -9.9 ) then ! over-write the default --
      CO5 = pollution data (IQ,J,MO) ! special correlation ---------------------
      endif

      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(01,1934)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J), ! ---- OUT
     &CO5 ! ---------------------------------------------------------------- OUT
      write(31,1934)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J), ! ---- EFF
     &CO5 ! ---------------------------------------------------------------- EFF
 1934 format(a11,37X,' Annual mean =',a10,1x,A4/
     &42X,'Standard deviation =',a10,1x,A4/
     &29X,'Correlation with discharge flow =',f10.4,/77('='))
     
      goto 6926
      endif
      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDEC(IQ,J) .lt. 4 )
*     --------------------------------------------------------------------------
*     note whether the data are for loads --------------------------------------
      if ( PDEC(IQ,J) .eq. 6 ) then ! normal loads
      if ( nobigout .le. 0 ) then
          
*     default correlation coefficients -----------------------------------------
      CO5 = EFCL (J) ! discharge quality on discharge flow
      if ( pollution data (IQ,J,MO) .ne. -9.9 ) then ! over-write the default --
      CO5 = pollution data (IQ,J,MO) ! special correlation ---------------------      
      endif
      
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,8924)DNAME(J),valchars10,LUNITS(J),valchars11, ! ------------ OUT
     &LUNITS(J),CO5 ! ------------------------------------------------------ OUT
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11, ! ------------ EFF
     &LUNITS(J),CO5 ! ------------------------------------------------------ EFF
 8924 format(77('-')/a11,40X,'Mean load =',a10,1x,a4/
     &42X,'Standard deviation =',a10,1x,A4/
     &29X,'Correlation with discharge flow =',f10.4,/77('='))
      goto 6926
      endif
      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDEC(IQ,J) .eq. 6 )
*     --------------------------------------------------------------------------
      if ( PDEC(IQ,J) .eq. 7 .or. PDEC(IQ,J) .eq. 11 ) then ! log normal loads
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,8924)DNAME(J),valchars10,LUNITS(J),valchars11,LUNITS(J)
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11, ! ------------ EFF
     &LUNITS(J)
      goto 6926
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
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(31,8924)DNAME(J),valchars10,LUNITS(J),valchars11, ! ------------ EFF
     &LUNITS(J)
      endif
	write(01,1965) flnamesmall(4,icod)
	write(31,1965) flnamesmall(4,icod) ! --------------------------------- EFF
 1965 format(
     &'Non-parametric distribution ... File: ',a64/77('-'))
      endif
      endif
      if ( PDEC(IQ,J) .eq. 4 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (pollution data(IQ,J,1),
     &pollution data(IQ,J,2)) 
      write(01,1924)DNAME(J),valchars10,UNITS(J),valchars11,UNITS(J) ! ----- OUT
 1924 format(/77('=')/a11,37X,' Annual mean =',a10,1x,A4/
     &42X,'Standard deviation =',a10,1x,A4/77('='))
      if ( ical .eq. 0 .or. ical .eq. 2. .or. ical .eq. 4 
     &.or. ical .gt. 6 ) then
      write(31,1924)DNAME(J),valchars10,UNITS(J),valchars11, ! ------------- EFF
     &UNITS(J)
      endif
      write(01,1965) flnamesmall(4,icod)
      write(31,1965) flnamesmall(4,icod) ! --------------------------------- EFF
      endif
      endif
      goto 6926
      endif
 1974 continue

*     note whether the data are monthly (type 5) -------------------------------
      if ( PDEC(IQ,J) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do 1989 i = 1, M8
      icod = i
      if ( iseasp ( 2, i, J+1 ) .eq. IQ ) goto 1990
 1989 continue
      goto 1994
 1990 continue
      if ( nobigout .le. 0 ) then
      write(01,1966)dname(J),FLMONTHsmall(4,icod) ! ------------------------ OUT
      write(31,1966)dname(J),FLMONTHsmall(4,icod) ! ------------------------ EFF
      endif
 1966 format(77('+')/'Discharge quality is expressed as monthly ',
     &'distributions ... ',a11/
     &'File: ',a64/77('+'))

*     open and read the file ---------------------------------------------------
      open(11,FILE = flmonth(4,icod), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (4,icod) ! discharge quality - type 5

      if ( nobigout .le. 0 ) then
      write(01,4398)
 4398 format(
*    &'Monthly input data on discharge quality ... ',a11/77('-')/
     &'Month        ','       Mean','   Standard','      Shift'/ ! discharge qual
*    &'  Correlation'/25x,'  Deviation'/77('-'))
     &24x,'  Deviation'/77('+'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),!seas4(i),seas0(i), ! --- quality
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2/!,f13.4,i18/
     &'February ... ',3f11.2/!,f13.4,i18/
     &'March ...    ',3f11.2/!,f13.4,i18/
     &'April ...    ',3f11.2/!,f13.4,i18/
     &'May ...      ',3f11.2/!,f13.4,i18/
     &'June ...     ',3f11.2/!,f13.4,i18/
     &'July ...     ',3f11.2/!,f13.4,i18/
     &'August ...   ',3f11.2/!,f13.4,i18/
     &'September ...',3f11.2/!,f13.4,i18/
     &'October ...  ',3f11.2/!,f13.4,i18/
     &'November ... ',3f11.2/!,f13.4,i18/
     &'December ... ',3f11.2/77('+')/)!,f13.4,i18/77('+')/)
      endif
      endif ! if ( PDEC(IQ,J) .eq. 5 ) ... monthly data
 1994 continue

 6926 continue ! end of loop on determinands -----------------------------------

      return
      end




*     adjust river flow for effect of a constant abstraction -------------------
*     but leave the specified flow in the river --------------------------------
*     --------------------------------------------------------------------------
      subroutine abstraction of flow
      include 'COMMON DATA.FOR'

*     initialise the abstracted loads ------------------------------------------
      do jdet = 1, ndet
      do L13 = 1, N13
      NSM (jdet,L13) = 0 ! number of shots per month
      ABLOAD (jdet,L13) = 0.0
      enddo
      enddo

*     identify the number of the flow data-set ---------------------------------
      IF = JF(KFEAT)

      if ( JSKIP .ne. 1 ) then ! -----------------------------------------------
      if ( nobigout .le. 0 ) then
      write(01,1001)F(IF,1),FUNIT
 1001 format('Flow abstracted from river:  ',F6.1,1x,a4)
      !if ( F(IF,2) .gt. 0.001 ) then
      write(01,1002)F(IF,2),FUNIT
 1002 format(77('-')/10x'"Hands-off" flow:  ',F6.1,1x,a4/77('-'))
      !endif
      endif
      endif ! if ( JSKIP .ne. 1 ) then -----------------------------------------

      if ( F(IF,1) .lt. 0.00001 ) return

 
*     calculate the abstracted flow and the resulting flow downstream ++++++++++
      do 1 IS = 1,NS ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      imonth = qmonth (is) ! set the month for this shot
      ABSFLOW = 0.0
      ABSLOAD = 0.0
      RESFLOW = 0.0
      STAFLOW = 0.0

*     check that the flow is bigger than the "hands-off" flow ==================
      if ( FMS(IS) .gt. F(IF,2) ) then ! =======================================
      STAFLOW = FMS(IS)
*     flow in river after the abstraction --------------------------------------
      RESFLOW = AMAX1 ( F(IF,2), FMS(IS) - F(IF,1) )
*     the amount of flow abstracted --------------------------------------------
      ABSFLOW = FMS(IS) - RESFLOW
      FMS(IS) = RESFLOW ! resulting flow ---------------------------------------
      endif ! if ( FMS(IS) .gt. F(IF,2) ) ======================================

*     calculate the abstracted load ============================================
      do jdet = 1, ndet ! ======================================================
      if ( qtype (jdet) .ne. 4 ) then ! ----------------------------------------
      ABSLOAD = ABSFLOW * CMS(jdet,IS) ! ----------------------- abstracted load    
      do J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
      ABLOAD (jdet,K13) = ABLOAD (jdet,K13) + ABSLOAD
      NSM (jdet,K13) = NSM (jdet,K13) + 1 ! number of shots per month    
      enddo
      endif ! if ( qtype (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1, ndet ================================================
    1 continue ! do 1 IS = 1,NS ++++++++++++++++++++++++++++++++++++++++++++++++
*     calculate the abstracted flow and the resulting flow downstream ++++++++++

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

      do jdet = 1,ndet ! =======================================================
      if ( qtype (jdet) .ne. 4 ) then ! ========================================
      kprune det = jdet ! --------------------------------- set for abstractions
*     accumulate total loads removed by abstractions ===========================
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------   
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1) ! ----- abstractions
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1) ! ----- abstractions
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0 ! ------------------------------------- abstractions 
      if ( abload(jdet,J1) .gt. 0.0 ) then
      if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(jdet,J1) - abload  (jdet,J1)) ! -- abstractions 
     &                /  TGLODE2(jdet,J1)
      endif ! if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) ----------------
      endif ! if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) ---------------
      endif ! if ( abload(jdet,J1) .gt. 0.0 ) ----------------------------------
      enddo ! do J1 = 1, nx ----------------------------------------------------

  
*     trim back the loads  -------------------------------- abstractions of flow
      call scale loads after losses

      endif ! if ( qtype (jdet) .ne. 4 ) =======================================
      enddo ! do jdet = 1 , ndet ===============================================

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return
      call calculate summaries of river flow ! at abstraction
      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 ) return ! ------ abstraction of flow
      if ( nobigout .le. 0 ) then
      if ( ical .ne. 3 ) call write out river flow ! at abstraction
      endif

      call write out loads and quality (0) ! at abstraction
      call write out monthly river quality (2) ! at abstraction
     
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! abstraction
      endif

      return
      end




*     river regulation ---------------------------------------------------------
      subroutine river regulation
      include 'COMMON DATA.FOR'

      IF = JF(KFEAT) ! flow data-set 
      if ( IF .le. 0 ) return
      IQ = JQ(KFEAT) ! quality data-set

      if ( JSKIP .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,4)F(IF,2),FUNIT
    4 format(29x,
     &'Flow to be maintained in the river:  ',F6.1,1x,a4/77('-'))
      endif

      if ( IQ .ge. 1 ) then
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,2044)IQ
 2044 format(
     &'Quality of water added to maintain flows: Data Set:',I6/77('-'))
      call write mean and standard deviation
      endif
      else
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,2844)
 2844 format(77('-')/
     &'Current quality is maintained ...')          
      endif ! if ( IQ .ge. 1 ) then

      do L13 = 1, N13
      do JP = 1, ndet
      NSM (JP,L13) = 0
      ELOAD (JP,L13) = 0.0 ! ---------------------------------- river regulation
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
      ELOAD (JP,I13) = ELOAD (JP,I13) + FADD * CMS (JP, IS) ! - river regulation
      endif
      NSM (JP,I13) = NSM (JP,I13) + 1 ! number of shots per month

      if ( FADD .gt. 1.0E-10 ) then
      ELOAD (JP,K13) = ELOAD (JP,K13) + FADD * CMS (JP, IS) ! - river regulation
      endif
      NSM (JP,K13) = NSM (JP,K13) + 1

      endif
      enddo
      goto 1
      endif

*     there are quality data for the inflow ------------------------------------
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
      call get the summary statistics of discharge quality (ECM, ECS) ! regulate

      call get the correlated random numbers (IS,R1,R2,R3,R4) ! regulation --

*     quality of added regulation water ----------------------------------------
      EC = RGCALC (R4, ECM)

*     perform mass balance -----------------------------------------------------
   11 continue
      if ( FADD .gt. 1.0E-10 ) then
      fxx = FMS(is) + FADD
      if ( fxx .gt. 1.0e-8 ) then
      CMS(JP,is) = ((FMS(is)*CMS(JP,is)) + (EC * FADD)) / fxx
*     include regulation with tributaries (Feature 17) -------------------------      
      LMS(17,JP,IS) = ((FMS(is)*LMS(17,JP,IS)) + (EC*FADD)) / fxx !   regulation
*     include regulation with total concentrations -----------------------------      

*     dilute the remaining contributions ----------------- river flow regulation
      do ip = 6, nprop ! ================================= river flow regulation
      if ( ip .ne. 17 ) then ! exclude streams (2) from loop
      LMS(IP,JP,IS) = (FMS(is) * LMS(IP,JP,IS)) / fxx
      endif
      enddo ! ip = 6, n2prop ===================================================
      endif ! if ( RF + EF .gt. 1.0e-8 )

      ELOAD(JP,I13) = ELOAD(JP,I13) + FADD * EC ! ------------  river regulation 
      NSM (JP,I13) = NSM (JP,I13) + 1 ! number of shots per month
      K13 = imonth + 1
      ELOAD(JP,K13) = ELOAD(JP,K13) + FADD * EC ! ------------  river regulation 
	NSM (JP,K13) = NSM (JP,K13) + 1
      endif ! if ( FADD .gt. 1.0E-10 )

    8 continue
    1 continue

      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then
      ELOAD (JP,i13) = ELOAD (JP,i13) / float (NS) ! ---------- river regulation
      if ( munthly structure .eq. 1 ) then ! ----------- store the monthly loads
      do J13 = 2, N13
      if ( NSM  (JP,J13) .gt. 0 ) then
      ELOAD (JP,J13) = ELOAD (JP,J13) / float (NSM(JP,J13)) ! - river regulation
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
      

      if ( ical13 .eq. 0 ) then ! ==============================================
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      call sort format 2 (Flow (1),Flow(2))
      write(01,2013)valchars10,FUNIT,valchars11,FUNIT
      write(34,2013)valchars10,FUNIT,valchars11,FUNIT
 2013 format(
     &'Flow in river just downstream ...',23X,'Mean =',a10,
     &1x,A4/46X,'95% exceedence =',a10,1x,A4)
      write(01,1023)
      write(34,1023)
 1023 format(110('-')/
     &'River quality just downstream ... '/110('-'))
      call write out loads and quality (0) ! at regulation point
      call write out monthly river quality (2) ! at regulation point
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      call write data for graph plotting ! river regulation
      endif ! if ( ical13 .eq. 0 ) =============================================

      return
      end



*     set the regulation quality -----------------------------------------------
      function RGCALC (R,XM)
      include 'COMMON DATA.FOR'

      RGCALC = 0.0
*     ------ 0  1  2  3  4  5  6  7  8  9 10 11 distribution types -------------
      goto ( 1, 2, 3, 3, 9, 9, 2, 3, 9, 9, 4, 3 ), IQDIST + 1

*     constant value -----------------------------------------------------------
    1 RGCALC = XM
      return

*     normal distribution ------------------------------------------------------
    2 RGCALC = Vnorm (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     log-normal distribution --------------------------------------------------
    3 RGCALC = Vlogn (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     power curve distribution 9999999999999999999999999999999999999999999999999
    4 RGCALC = Vlogn (R, GECS, GECM, EC3, BM(4), BS(4), XM)
      return

*     non-parametric distribution ----------------------------------------------
    9 continue
      RGCALC = XM
      return
      end



*     weir ---------------------------------------------------------------------
      subroutine weir
      include 'COMMON DATA.FOR'
      IQ = JQ(KFEAT)
      if ( IPRINT .eq. 1 )return
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1015)
 1015 format(77('-')/
     &'Calculated river quality downstream of weir ...'/77('-'))
      call write out loads and quality (0) ! at weir
      call write out monthly river quality (2) ! at weir
      endif
      return
      end



*     initialise variables for mass balance ------------------------------------
      subroutine initialise data for mass balance
      include 'COMMON DATA.FOR'

      cut off zero flow = 0.0 

*     correlation coefficients for mass balance --------------------------------
      CO1 = 0.0 ! initialise river flow on river quality -----------------------
      CO2 = 0.0 ! initialise river flow on discharge flow ----------------------
      CO3 = 0.0 ! initialise river flow on discharge quality ! 44444444444444444
      CO4 = 0.0 ! initialise river quality on discharge flow ! 44444444444444444
      CO5 = 0.0 ! initialise discharge (or tributary) flow on discharge quality-
      CO6 = 0.0 ! initialise river quality on discharge quality ! 44444444444444
      
*     scaling for correcting Monte Carlo sampling bias -------------------------
      do j = 1,4
      BM(j) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      BS(j) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      enddo

*     initialise ---------------------------------------------------------------
      do jdet = 1, ndet
      kdecision(jdet) = 0
      do j13 = 1,N13
      XLOAD (jdet,1,j13) = 0.0
      XLOAD1 (jdet,1,j13) = 0.0 ! dissolved metal
      XLOAD2 (jdet,1,j13) = 0.0 ! solid metal
      NSM (jdet, j13) = 0 ! number of shots per month
      enddo
      enddo

      do jdet = 1, ndet ! calculate the correlation of flow on quality ---------
      if ( QTYPE (jdet) .ne. 4 ) then      
      do is = 1, NS
      Creg(1,IS) = FMS(IS) ! store the upstream river flow for regression ------
      Creg(4,IS) = CMS(jdet,is) ! upstream river quality for regression ########
      enddo
      k1 = 1 ! river flow
      k2 = 4 ! river quality
      RRR = 0.999
      call regres the calculated shots (k1,k2,RRR)
      !write(33,1)RRR,dname(jdet)
    1 format(77('-')/
     &'Shot correlation of river quality on river flow =',F8.4,
     &' for ',a11/77('-'))
      endif ! if ( QTYPE (jdet) .ne. 4 )
      enddo ! do jdet = 1, ndet
      
      EFM = 0.0
      EFS = 0.0
      EF5 = 0.0
      EF3 = 0.0
      ECM = 0.0
      ECS = 0.0
      EF3 = 0.0
      
      return
      end


      subroutine load the upstream flow and quality 
      include 'COMMON DATA.FOR'
      do is = 1, NS
      cms(JP,IS) = ucms(JP,IS)
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      lms(ip,JP,IS) = ulms(ip,JP,IS) ! concentrations from  types of feature ---
      enddo
      fms(IS) = ufms(IS)
      enddo
      call get summaries of river quality from the shots ! upstream ------------
      return
      end


      subroutine write GIS data to channel 42 43 24 and 25
      include 'COMMON DATA.FOR'
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
      if ( num .eq. 46 .or. num .eq. 48 .or. num .eq. 50 ) goto 99
      if ( num .eq. 52 .or. num .eq. 54 .or. num .eq. 56 ) goto 99
      if ( num .eq. 58 .or. num .eq. 13 .or. num .eq. 15 ) goto 99

      write(42,244)ireech,GIScode(JX),RNAME(ireech),place, ! ---------- GIS1.CSV
     &UNAME(JX),num,Length of main river-adist(ireech),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &COB(1,1),COB(2,1),COB(3,1),COB(4,1)
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River FLOW",',',A4,',',9(1PE11.4,','))

      do J = 1, MP10
      write(42,245)ireech,GIScode(JX),RNAME(ireech),place,UNAME(JX),num,
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
      write(42,845)ireech,GIScode(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
      write(42,845)ireech,GIScode(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
	endif ! dissolved and solid metal
      enddo ! loop on determinands
      
      write(42,247)ireech,GIScode(JX),RNAME(ireech),place,UNAME(JX),num,
     &Length of main river-adist(ireech),(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),qualn(J),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &5(1PE11.4,','),10(6(1PE11.4,',')),11(i2,','),i4,',')
   99 continue

      GIScode last = GIScode(JX)
      return
      end



*     NEGATIVE DISCHARGE ONE (Feature type 18) ---------------------------------
*     adjust river flow for the effect of abstraction --------------------------
*     The abstraction data are specified as mean and 95-percentile -------------
*     (NEGATIVE DISCHARGE TWO  uses mean and standard deviation) ---------------

      subroutine negative discharge one ! abstraction 18
      include 'COMMON DATA.FOR'
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
      NSM (jdet,L13) = 0 ! number of shots per month
      ABLOAD (jdet,L13) = 0.0
      enddo
      enddo ! initialise the abstracted loads ----------------------------------

*     identify the code number for the river flow data-set ---------------------
      IF = JF(KFEAT)
      if ( IF .lt. 1 .or. IF .gt. NF ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
      if ( iscreen .lt. 3 ) write( *,1020)KFEAT
      write(09,1020)KFEAT
      write(33,1020)KFEAT
 1020 format(/'*** Abstraction data specified for Feature number', ! - type (18)
     &I6,' (Type 18) do not exist')
      if ( nobigout .le. 0 ) write(01,1006)
      if ( iscreen .lt. 3 ) write( *,1006)
 1006 format('*** No abstraction made ***'/80('-'))
      return
      endif

*     default correlation of abstracted flow on main river flow ----------------
      CO2 = 1.0 ! default correlation of abstracted flow on main river flow ----
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F(IF,MO) ! correlation coefficient -------
      call set up the correlation coefficients ! 44444444 negative discharge one
      IFDIST = PDRF(IF) ! the type of distribution -----------------------------

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( IFDIST .gt. 0 ) then ! constant flow --------------------------------
      call sort format 3 (F(IF,1),F(IF,2),CO2)
      write(01,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
 2012 format(77('-')/'Flow to be removed from the river:', ! --- river flow dist
     &20X,'  Mean =',a10,1X,A4/
     &46x,'95% exceedence =',a10,1x,a4/
     &33x,'Correlation with river flow =',a10/77('-'))
      else
      call sort format 1 (F(IF,1))
      write(01,2812) valchars10,FUNIT
 2812 format(77('-')/'Flow to be removed from the river:',18x, ! ------ constant
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
 7363 format('The file is: ',a64/77('-'))
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
      GEFStest = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST)
      if ( GEFStest .le. 0.0 ) then
      write( *,9099)UNAME(KFEAT)
      write(01,9099)UNAME(KFEAT)
 9099 format(///'### Flow error for Feature called - ',A32/
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if GEFStest .le. 0.0
      GEFS = SQRoot(1046,GEFStest) - 1.6449
      EFStest = EXP(GEFS*GEFS) - 1.0 
      if (EFStest .le. 1.0E-10) then
      write( *,9899)UNAME(KFEAT)
      write(01,9899)UNAME(KFEAT)
 9899 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if (EFStest .le. 1.0E-10)
      EFS = EFM * SQRoot(1047,EFStest)
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      GM = GEFM
      GS = GEFS
      RR3 = EF3
      call bias in log normal negative river flows (GM,GS,RR3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EF5/BS(3) ! negative discharge
      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1299)BM(3),BS(3)
 1299 format(
     &'Corrections for sampling errors: log-normal removals ',
     &'(river-type)',
     &' ...'/77('-')/'Mean          =',F8.3/'95-percentile =',F8.3)
      call generate removed log normal river flows           
      endif ! if ( MONF .gt. 1 )
      endif ! log normal -------------------------------------------------------

*     deal with for the log-normal distribution of abstracted flows ------------
      if ( IFDIST .eq. 10 ) then ! power curve distribution --------------------
      EFM = F(IF,1) ! mean abstracted flow (river type)
      EF5 = F(IF,2) ! 95-percentile
      EFS = 0.0 ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3)
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile ------------------------------------------------------------
      TEST = AMAX1 ((EF5+EF3),1.0E-8)
      GEFStest = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST)
      if ( GEFStest .le. 0.0 ) then
      write( *,9299)UNAME(KFEAT)
      write(01,9299)UNAME(KFEAT)
 9299 format(///'### Flow error for Feature called - ',A32/
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if GEFStest .le. 0.0
      GEFS = SQRoot(1046,GEFStest) - 1.6449
      EFStest = EXP(GEFS*GEFS) - 1.0
      if (EFStest .le. 1.0E-10) then
      write( *,9829)UNAME(KFEAT)
      write(01,9829)UNAME(KFEAT)
 9829 format(///'### Flow error for Feature called - ',A32//
     &'### Simulation stopped in NEGATIVE DISCHARGE ONE')
      call stop
      endif ! if (EFStest .le. 1.0E-10)
      EFS = EFM * SQRoot(1047,EFStest)
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
      endif ! if ( IFDIST .eq. 10 ) power curve distribution -------------------
      
      if ( IFDIST .eq. 1 ) then ! normal distribution for abstractions
      EFM = F(IF,1) ! mean abstracted flow (river type)
      EF5 = F(IF,2) ! 95-percentile
      EF3 = 0.0 ! shift
      EFS = (EFM-EF5) / 1.6449 ! standard deviation
      if ( nobigout .le. 0 ) then
      call sort format 3 (EFM,EF5,EFS)
      write(01,2512) valchars10,FUNIT,valchars11,FUNIT,
     &valchars12,FUNIT
 2512 format(77('-')/'Flow to be removed from the river:',20X, ! --- normal dist
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
      if ( MONF .gt. 1 ) call shots for removed flows ! - negative discharge one
      endif ! non-parametric distribution --------------------------------------

      if ( IFDIST .eq. 5) then ! monthly data ----------------------------------
      call set up monthly data for negative discharge
      if ( MONF .gt. 1 ) call shots for removed flows ! - negative discharge one
      endif ! monthly data -----------------------------------------------------
      
      call list the correlation coefficients

*     if ( MONF .gt. 1 ) call calculate summaries of removed flows

*     prepare to abstract the flows --------------------- negative discharge one
      FACT = -1.0
*     prepare to count the shots where the abstraction is greater than zero ----
      numflows = 0

*     start the abstraction ++++++++++++++++++++++++++++++++++++++++++++++++++++
      do 2000 IS = 1, NS ! loop through the shots ++++++++++++++++++++++++++++++
      ABSFLOW = 0.0
*     get the correlated random numbers ----------------------------------------
*     call get the correlated random numbers (IS,R1,R2,R3,R4) ! -ve diacharge
*     retrieve the flow of the upstream river ----------------------------------
      RF = FMS (IS)
*     and retrieve the upstream river quality ----------------------------------
*     this is if we want to abstract conditionally on river quality ------------
*     RC = CMS ( JP, IS )

*     get the shot for the abstraction ... the value of EF ---------------------
      EF = FTMS(IS)
*     now do the substraction --------------------------------------------------
      START FLOW = RF
      HANDS OFF = 0.0
      REM FLOW = amax1 (HANDS OFF, RF + FACT * EF) ! negative discharge one ----
      EF = START FLOW - REM FLOW
      FMS(IS) = REM FLOW
      if ( EF .gt. 1.0e-10) numflows = numflows + 1

*     calculate the abstracted load ============================================
      do 3000 jdet = 1, ndet ! =================================================
      if ( QTYPE (jdet) .ne. 4) then ! =========================================
      do 3050 J13 = 1, N13 ! ---------------------------------------------------
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
      ABLOAD(jdet,K13) = ABLOAD (jdet,K13) + EF * CMS(jdet,IS) 
      A1LOAD(jdet,K13) = A1LOAD(jdet,K13) + START FLOW * CMS(jdet,IS)
      A2LOAD(jdet,K13) = A2LOAD(jdet,K13) + FMS(IS) * CMS(jdet,IS)
      NSM (jdet,K13) = NSM (jdet,K13) + 1
 3050 continue ! do 3050 J13 = 1, N13 ------------------------------------------

*     prepare to calculate the mean of the abstracted quality ------------------
      if ( EF .gt. 1.0e-10 ) then
      ABMean (jdet) = ABMean (jdet) + CMS(jdet,IS)
      ABStdev(jdet) = ABStdev(jdet) + CMS(jdet,IS) * CMS(jdet,IS)
*     store the abstracted quality ---------------------------------------------
      abvalues(jdet,numflows) = CMS (jdet, numflows)
      endif
      endif ! if ( QTYPE (jdet) .ne. 4) ========================================
 3000 continue ! do 3000 jdet = 1, ndet ! ======================================

 2000 continue ! do 2000 IS = 1, NS ++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

      do jdet = 1 , ndet ! =====================================================
      if ( QTYPE (jdet) .ne. 4 ) then ! ========================================
      kprune det = jdet ! ! ----------------------------- negative discharge one
*     accumulate total loads removed by abstractions ==== negative discharge one
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1)
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1)
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0 ! --------------------------- negative discharge one
      if ( abload(jdet,J1) .gt. 0.0 ) then
      if ( abs ( TGLODE2 (jdet,J1) ) .gt. 0.000000001 ) then
      if ( TGLODE2(jdet,J1) - abload(jdet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(jdet,J1) - abload  (jdet,J1)) 
     &                /  TGLODE2(jdet,J1)
      endif
      endif ! if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 )
      endif ! if ( abload(jdet,J1) .gt. 0.0 )
      enddo ! do J1 = 1, nx

      
*     trim back the loads for abstractions -------------- negative discharge one
      call scale loads after losses ! ------------------- negative discharge one
      endif ! if ( QTYPE (jdet) .ne. 4 ) then ==================================
      enddo ! do jdet = 1 , ndet ===============================================

      if ( MONF.gt.1 ) call write shots for river flow !  negative discharge one
      call calculate summaries of river flow  ! --------- negative discharge one
      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 )return ! ---- negative discharge one
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
      call write data for graph plotting ! negative discharge one
      endif

      return
      end

      
      
*     NEGATIVE DISCHARGE TWO (Feature type 19) ---------------------------------
*     adjust river flow for the effect of abstraction --------------------------
*     The abstraction data are specified as mean and standard deviation --------
*     (NEGATIVE DISCHARGE ONE uses mean and 95-percentile) ---------------------

      subroutine negative discharge two ! abstraction 19
      include 'COMMON DATA.FOR'
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
      NSM (jdet,L13) = 0 ! number of shots per month
      ABLOAD (jdet,L13) = 0.0
      enddo
      enddo ! initialise the abstracted loads ----------------------------------

*     identify the code number for the river flow data-set ---------------------
      IF = JF (KFEAT)
      if ( IF .lt. 1 .or. IF .gt. NF ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
      if ( iscreen .lt. 3 ) write( *,1020)KFEAT
      write(09,1020)KFEAT
      write(33,1020)KFEAT
 1020 format(/'*** Abstraction data specified for Feature number', ! - type (19)
     &I6,' (type 19) do not exist')
      if ( nobigout .le. 0 ) write(01,1006)
      if ( iscreen .lt. 3 ) write( *,1006)
 1006 format('*** No abstraction made ***'/80('-'))
      return
      endif

*     default correlation coefficient
*     correlation of abstracted flow on main river flow
      CO2 = 0.0
*     special correlation ------------------------------------------------------
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F(IF,MO) ! correlation coefficient -------
      call set up the correlation coefficients ! ----- negative flow two -------
      IFDIST = PDRF(IF) ! the type of distribution -----------------------------

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      if ( IFDIST .gt. 0 ) then
      call sort format 3 (F(IF,1),F(IF,2),CO2)
      write(01,2012) valchars10,FUNIT,valchars11,FUNIT,valchars12
 2012 format(77('-')/'Flow to be removed from the river:', ! - negative flow two
     &20X,'  Mean =',a10,1X,A4/
     &42x,'Standard deviation =',a10,1x,a4/
     &42x,'       Correlation =',a10/77('-'))
      else
      call sort format 1 (F(IF,1))
      write(01,2812) valchars10,FUNIT
 2812 format(77('-')/'Flow to be removed from the river:',18x, ! ------ constant
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
 7363 format('The file is: ',a64/77('-'))
      endif ! monthly data

      if ( F(IF,1) .gt. 1.0E-10 ) then ! check for zero flows  

*     deal with for the log-normal distribution of abstracted flows ------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log normal distribution
      EFM = F(IF,1) ! mean abstracted flow (effluent type)
      EFS = F(IF,2) ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3)
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
     *(GRFM,GRFS,F(IF,1))
      endif ! if ( MONF .gt. 1 )
      endif ! log normal -------------------------------------------------------

      
*     deal with for the log-normal distribution of abstracted flows ------------
      if ( IFDIST .eq. 10 ) then ! power curve distribution --------------------
      EFM = F(IF,1) ! mean abstracted flow (effluent type)
      EFS = F(IF,2) ! standard deviation
      EF3 = 0.0 ! shift
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3)
*     compute mean and standard deviation of logged flows from the mean and ----
*     standard deviation -------------------------------------------------------
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
     *(GRFM,GRFS,F(IF,1))
      endif ! if ( MONF .gt. 1 )
      endif ! if ( IFDIST .eq. 10 ) power curve distribution -------------------

      
      if ( IFDIST .eq. 1 ) then ! normal distribution --------------------------
      EFM = F(IF,1) ! mean abstracted flow (effluent type)
      EFS = F(IF,2) ! standard deviation
      EF3 = 0.0 ! shift
      if ( nobigout .le. 0 ) then
      call sort format 2 (EFM,EFS)
      write(01,2512) valchars10,FUNIT,valchars11,FUNIT
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
      call generate removed normal effluent flows (GM,GS,F(IF,1))          
      endif ! if ( MONF .gt. 1 )
      endif ! normal distribution ----------------------------------------------
      
      if ( IFDIST .eq. 0 ) then ! constant flow --------------------------------
      EFS = 0.0
      EFM = F(IF,1)
      do is = 1, NS ! calculate flows
      FTMS (IS) = EFM
      enddo ! calculate flows
      endif ! constant flow ----------------------------------------------------

      if ( IFDIST .eq. 4) then ! non-parametric distribution -------------------
      call set up the shots for non parametric stream flow ! feature 19
      if ( MONF .gt. 1 ) call shots for removed flows ! - negative discharge two
      endif ! non-parametric distribution --------------------------------------

      if ( IFDIST .eq. 5) then ! monthly data ----------------------------------
      call set up monthly data for negative discharge
      if ( MONF .gt. 1 ) call shots for removed flows ! - negative discharge two
      endif ! monthly data -----------------------------------------------------

      call list the correlation coefficients

*     if ( MONF .gt. 1 ) call calculate summaries of removed flows

*     prepare to abstract the flows --------------------- negative discharge two
      FACT = -1.0
*     prepare to count the shots where the abstraction is greater than zero ----
      numflows = 0

*     start the abstraction ----------------------------------------------------
      do 2000 IS = 1, NS ! loop through the shots
      ABSFLOW = 0.0
*     get the correlated random numbers ----------------------------------------
      call get the correlated random numbers (IS,R1,R2,R3,R4) ! -ve discharge
*     retrieve the flow of the upstream river ----------------------------------
      RF = FMS (IS)
*     and retrieve the upstream river quality ----------------------------------
*     this if we want to abstract conditionally on river quality ---------------
*     RC = CMS ( JP, IS )

*     get the shot for the abstraction ... the value of EF ---------------------
*     impose correlation between abstracted flow and river flow ----------------
*     R3 = CO2 * R1 + R3 * SQRMB(105, 1.0 - CO2 * CO2 )
*     call get a flow for the stream or discharge ( IS, R3, EF )
      EF = FTMS(IS)
	
*     now do the substraction --------------------------------------------------
      START FLOW = RF
      HANDS OFF = 0.0
      REM FLOW = amax1 (HANDS OFF, RF + FACT * EF) ! negative discharge two ----
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
      endif ! if ( QTYPE (jdet) .ne. 4) ======================================== 
 3000 continue ! end of loop on determinands =================================== 
 2000 continue ! end of loop on shots ++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

      do jdet = 1, ndet ! ======================================================
      if ( QTYPE (jdet) .ne. 4 ) then ! ========================================
      kprune det = jdet ! ------------------------------- negative discharge two
*     accumulate total loads removed by abstractions ==== negative discharge two
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------     
      TBLOAD2(jdet,J1) = TBLOAD2(jdet,J1) - abload(jdet,J1)
      TBLOAD1(jdet,J1) = TBLOAD1(jdet,J1) - abload(jdet,J1)
*     calculate the effect of losses on accumulated loads ======================
      prune load (J1) = 1.0 ! --------------------------- negative discharge two
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
*     endif
*     endif
      enddo

*     trim back the loads for abstractions -------------- negative discharge two
      call scale loads after losses ! ------------------- negative discharge two
      endif ! if ( QTYPE (jdet) .ne. 4 ) then ==================================
      enddo ! do jdet = 1, ndet ================================================
   
      if ( MONF.gt.1 ) call write shots for river flow !  negative discharge two
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
      call load calculation ! negative discharge two ---------------------------

      if ( ical13 .eq. 0 ) then
      call write data for graph plotting ! abstraction
      endif

      return
      end
