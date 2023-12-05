*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality  ....
*     ==========================================================================
*     Written in FORTRAN.  With screen displays in VISUAL BASIC ...
*     ==========================================================================
*     Version 161  (15/11/23) --------------------------------------------------
*     ==========================================================================
*     File read data on features.for ... 2908 lines ----------------------------
*     --------------------------------------------------------------------------
*     This file reads and processes data for Features --------------------------
*     --------------------------------------------------------------------------
*     This file contains 4 subroutines -----------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... read the data on features
*     ...... routine monitoring point
*     ...... turn on diffuse pollution 15
*     ...... turn off diffuse pollution 16 etc
*     --------------------------------------------------------------------------
      
*     read the data on Features ----------------------------------------------
      subroutine read the data on features
      include 'COMMON DATA.FOR'
      character *200 line
      character *1 AU(37)
      integer deleets, diffcount
      
*     initialise variables ---------------------------------------------------
      do kunu = 1, nu
      uname (kunu) = 'FEATURE NOT ENTERED'
      enddo ! do kunu = 1, nu ------------------------------------------------

*     set starting value for the number of Features --------------------------
      feetcount = 0
*     set starting value for the number of Bifurcations ----------------------
      IFURC = 0
*     set starting value for a counter for the number of mistakes ------------
      MISTAKE = 0
      deleets = 0 ! ----------- set starting value for a counter for deletions
      
*     set starting value for the checking diffuse sources to reaches ---------
      diffreachstop = 0

*     add 1 to the counter of number of features -----------------------------
   38 continue
      feetcount = feetcount + 1

   39 call check first four characters of a line of data (IFIN)
      JREACH(feetcount) = 0
      
*     test for the end of the data -------------------------------------------
      jpass = 0

*     test for the end of the data -------------------------------------------
      if ( IFIN .eq. 1 ) then
*     end of data reached ----------------------------------------------------
      jpass = jpass + 1

      MU = feetcount - 1 !  final value for the total number of valid features
      
      if ( ical13 .eq. 0 ) then
      do j = 1,ndet
      if ( QTYPE (j) .ne. 4) then
      write(230+j,252)feetcount ! ------------------------------------- Gi.CSV
      write(240+j,252)feetcount ! ------------------------------------- Wi.CSV
      endif
  252 format(',','Features',',',i6)
      enddo
      endif

      if ( MU .gt. 22999 ) then
      write( *,9611)MU
 9611 format('*** Total number of Features:',I7)
      endif

      if ( MU .gt. NU ) then
      call change colour of text (14) ! bright yellow
      write( *,2828)NU
      call change colour of text (10) ! green
      write(01,2828)NU
      write(09,2828)NU
      write(33,2828)NU
 2828 format(130('*')/'* More Features have been specified for ',
     &'this run than the maximum set up for SIMCAT ...',i8/130('*'))
      call pause ! MU is bigger than NU ======================================    
      endif
      
*     check there is at least one Feature in the SIMCAT model ----------------
      if ( MU .lt. 1 ) then
      write( *,2822)
      write(01,2822)
      write(09,2822)
      write(33,2822)
 2822 format(77('*')/'*** No Features have been specified for ',
     &'this run ... '/
     &'*** (or none of them has passed the validity checks)'/
     &'*** There must at least be one of type 10 ...',
     &' Run stopped ... '/77('*'))
      if (jpass .eq. 1 ) then
      goto 39 
      else ! jpass is .ne. 1
      call stop
      endif !  if jpass .eq. 1

      else ! if MU .gt. 1 )

      kountworks35 = 0 ! count the discharges used for back-tracking -----------
      do im = 1,mu
      m = JT(im)
      if ( m .eq. 03 .or. m .eq. 05 .or. m .eq. 39 .or. m .eq. 60 .or. 
     &     m .eq. 61 ) kountworks35 = kountworks35 + 1
      enddo
                
*     insert extra Plotting Points beyond the last feature ---------------------
      if ( KINT .eq. 1 ) then
      DDDD = DIST (MU)

*     check same reach ---------------------------------------------------------
      if ( RLENGTH(JREACH(MU)) - DIST (MU) .gt. 1.0 ) then
 9991 DDDDD = float (int ( DIST(MU) + 1.00 ) )
      if ( DDDDD .ge. RLENGTH(JREACH(MU)) ) goto 9898
      MU = MU + 1
      UNAME(MU) = "  ....          "
      JT(MU) = 0
      JREACH(MU) = JREACH (MU-1)
      JF(MU) = 0 ! number of the set of flow data
      JQ(MU) = 0 ! number of the set of quality data
      JFCAL(MU) = 0
      JQCAL(MU) = 0
      IFRQS(MU) = IFRQS (MU-1)
      DIST(MU) = DDDDD
      goto 9991
      endif ! if RLENGTH
      endif ! if ( KINT .eq. 1 )

*     scan the features for problems with monitoring points -------------------
      ineed = 0
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 .and. Detype (idet) .ge. 900) ineed = 1
      enddo      
*     there are partitioned metals ----------------------------------------------
      if ( ineed .eq. 1 ) then
      do icheck = 1, MU
      if ( JT (icheck) .eq. 1 .and. JQ (icheck) .ne. 0 ) then
      monpoint = icheck
      jcheck = JQ (icheck)
*     check for multiple uses of this set of water quality data ----------------
      do i = 1, MU
      if ( i .ne. monpoint ) then
      mondata = JQ (i)
*     ############### check need to include bifurcations #######################
      if ( JT (i) .eq. 2 .or. JT (i) .eq. 10 .or.
     &     JT (i) .eq. 45 ) then 
      if ( mondata .gt. 0 .and. mondata .eq. jcheck) then
*     found a duplicate use ----------------------------------------------------
      JQ (mon point) = 0
      write(33,6900)uname( mon point )
      write( *,6900)uname( mon point )
 6900 format(77('*')/
     &'Partitioning of determinands requested ... '/
     &'Data for a Monitoring Point has a set of quality data ',
     &'that is not unique ... '/
     &'Name of Monitoring Point: ',a40/
     &'The Monitoring Point will not use this set of ',
     &'data ... '/77('*'))  
      endif ! if mondata .eq. 2 etc
      endif ! if JT (i) .eq. 2 etc
      endif ! if ( i .ne. monpoint )
      enddo ! do i = 1, MU
*     -------------------------------------------------------------------------  
      endif ! if ( JT (icheck) .eq. 1 etc
      enddo ! do icheck = 1, MU
      endif ! if ( MU .lt. 1 )
*     --------------------------------------------------------------------------

*     end of section on "inserting plotting points" ...
      endif ! if ( ineed .eq. 1 )

 9898 continue
      
*     count the number of features in each reach -------------------------------
      do krchno = negorder, NREACH
      kount reach (krchno) = 0
      enddo
      do keet = 1, feetcount
      if ( Jreach (Keet) .gt. 0 ) then
      Kount Reach (Jreach (Keet)) = Kount Reach (Jreach (Keet)) + 1
      endif
      enddo

*     set up right-adjusted names of features ----------------------------------
      do iu1 = 1, mu
      read(uname(iu1),5411)(AU (i),i=1,37)
 5411 format(37a1)
*     loop on characters -------------------------------------------------------
      do iu2 = 1,37 ! remove interfering characters ----------------------------
      if ( au(iu2) .eq. '"' ) then
      do ju2 = iu2, 36
      au(ju2) = au(ju2+1)
      enddo
      au(37) = ' '
      endif
      enddo ! do iu2 = 1,37
      do iu2 = 1,37
      ju2 = 38 - iu2
      if ( au(ju2) .ne. ' ' ) then
      ku2 = ju2
      goto 5656
      endif
      enddo ! do iu2 = 1,37
 5656 continue  
      ju3 = 37 - ku2
      write(vname(iu1),5411)(' ',i=1,ju3),(AU(i),i=1,ku2)
      write(uname(iu1),5411)(AU(i),i=1,37)
      enddo ! do iu1 = 1, mu

      return
      endif ! if ( IFIN .eq. 1 )
*     end of "last feature" section --------------------------------------------

*     Process the next Feature
*     UNAME    - Name of feature ...
*     JT       - Code number for type of feature...
*                (1) Monitoring point
*                (2) Stream or tributary
*                (3) Sewage treatment works
*                (4) River flow gauge
*                (5) Industrial discharge
*                (6) Plotting point
*                (7) Continuous abstraction
*                (8) Weir
*                (9) Flow regulation point
*               (10) Boundary point - headwaters
*               (11) Old type of bifurcation 
*               (12) Intermittent discharge 
*               (13) Switch on Diffuse Pollution
*               (14) Switch off Diffuse Pollution
*               (15) Switch on Diffuse Pollution
*               (16) Switch off Diffuse Pollution
*               (17) Discharge with zero flow
*               (18) Abstraction (negative discharge - mean and 95-percentile)
*               (19) Abstraction (negative discharge - mean and standard deviation)
*               (20) Bifurcation: abstraction of river flow distribution - first branch
*               (21) Bifurcation: abstraction of river flow distribution - second branch
*               (22) Bifurcation: abstraction of effluent flow distribution - first branch
*               (23) Bifurcation: abstraction of effluent flow distribution - second branch
*               (24) Sub-catchment boundary
*               (25) Switch on  Diffuse Pollution - agriculture (livestock)
*               (26) Switch off Diffuse Pollution - agriculture (livestock)
*               (27) Switch on  Diffuse Pollution - agriculture (arable)
*               (28) Switch off Diffuse Pollution - agriculture (arable)
*               (29) Switch on  Diffuse Pollution - highways
*               (30) Switch off Diffuse Pollution - highways
*               (31) Switch on  Diffuse Pollution - urban 
*               (32) Switch off Diffuse Pollution - urban
*               (33) Switch on  Diffuse Pollution - atmospheric deposition
*               (34) Switch off Diffuse Pollution - atmospheric deposition
*               (35) Switch on  Diffuse Pollution - background
*               (36) Switch off Diffuse Pollution - background
*               (37) Switch on  Diffuse Pollution - septic tanks
*               (38) Switch off Diffuse Pollution - septic tanks
*               (39) Mine Waters
*               (40) Start of Aggregated CSOs
*               (41) End of Aggregated CSOs
*               (42) Start of Aggregated STWs
*               (43) End of Aggregated STWs
*               (44) Flow into a lake
*               (45) Flow from a lake
*               (46) Switch on Diffuse Pollution - mines
*               (47) Switch off Diffuse Pollution - mines
*               (48) Switch on impact of Birds, Boats and Anglers 
*               (49) Switch off impact of Birds, Boats and Anglers 
*               (50) Switch on  User-named diffuse 
*               (51) Switch off User-named diffuse
*               (52) Switch on  User-named diffuse 
*               (53) Switch off User-named diffuse
*               (54) Switch on  User-named diffuse
*               (55) Switch off User-named diffuse
*               (56) Switch on  User-named diffuse
*               (57) Switch off User-named diffuse
*               (58) Switch on  User-named diffuse
*               (59) Switch off User-named diffuse
*               (60) Other point source 
*               (61) Private waste waters
*               (62) Fish Farm
*     JREACH   - Code number of reach on which feature is located...
*     DIST     - Distance from head of reach (km) ...
*     JF       - Code number for river flow data-set......
*     JQ       - Code number for quality data-set (quality data or pollution data).
*     JFCAL    - Code number for river flow data-set (F) used for gap filling...
*                SIMCAT will obtain a fit to these data when ICAL is set to 1...
*     JQCAL    - Code number for quality data-set (quality data) used for gap filling...
*                SIMCAT will obtain a fit to these data when ICAL is set to 3...
*     IFRQS    - Reference numbers for the sets of river quality targets.....
*     GIScode  - Cross reference for GIS
*     --------------------------------------------------------------------------

*     ensure that the array boundaries are not exceeded ------------------------
      if ( feetcount .le. NU ) then ! ++++++++++++++++++++++++++++++++++++++++++
      call extract a line of data (line)
*     read data for the Feature ------------------------------------------------
      read(line,*,END = 8999,ERR = 8999)UNAME(feetcount),JT(feetcount),
     &JREACH(feetcount),
     &DIST(feetcount),JF(feetcount),JQ(feetcount),JFCAL(feetcount),
     &JQCAL(feetcount),IFRQS(feetcount),GIScode(feetcount)
      if ( GIScode(feetcount) .eq. ' ' ) then
      GIScode(feetcount) = 'No GIS code'
      endif
      
      goto 7999
 8999 continue

      read(line,*,ERR = 8321)UNAME(feetcount),JT(feetcount),
     &JREACH(feetcount),
     &DIST(feetcount),JF(feetcount),JQ(feetcount),JFCAL(feetcount),
     &JQCAL(feetcount),IFRQS(feetcount)
      GIScode(feetcount)='No GIS code'

*     exclude feature data for excluded reaches --------------------------------
 7999 continue
      
      if ( negreach .gt. 0 ) then ! a subset of the reaches will be run --------
      if ( Included(JREACH(feetcount)) .ne. 0 ) goto 39
      endif

      IQ = JQ (feetcount)
      IF = JF (feetcount)
      KREACH = JREACH(feetcount)

      if ( RFDIFF(KREACH) .gt. 0 ) then ! check reach diffuse flow =============

      if ( IDIFFREACH .eq. 0 ) then ! -- prepare to switch on the diffuse inflow
      if ( diffreachstop .eq. 0 ) then ! ---------------------------------------
      call set screen text colour ! --------------------------------------------
      write( *,1289)
 1289 format(77('-'))
      call change colour of text (12) ! orange ! -------------------------------
      write( *,8492) rname(KREACH) ! -------------------------------------------
 8492 format('*** A diffuse inflow is specified for Reach: ',a16/ ! ------------
     &'*** But is suppressed for ALL Reaches by the switch', ! -----------------
     &' set in the DAT file     ') ! -------------------------------------------
      call set screen text colour ! --------------------------------------------
      write( *,1289)
      write(01,8493) rname(KREACH) ! -------------------------------------------
      write(09,8493) rname(KREACH) ! -------------------------------------------
      write(33,8493) rname(KREACH) ! -------------------------------------------
      write(03,8493) rname(KREACH) ! -------------------------------------------
 8493 format(77('-')/ ! --------------------------------------------------------
     &'*** A diffuse inflow is specified for Reach: ',a16/ ! -------------------
     &'*** But is suppressed for ALL Reaches by the switch', ! -----------------
     &' set in the DAT file ... '/77('-')) ! -----------------------------------
      diffreachstop = diffreachstop + 1
      endif ! if ( diffreachstop .eq. 0 ) --------------------------------------
      endif ! if ( IDIFFREACH .eq. 0 ) -----------------------------------------
      endif ! if ( RFDIFF(KREACH) .gt. 0 ) =====================================
      
*     identify headwaters that are to have diffuse pollution added =============
      if ( JT(feetcount) .eq. -10 ) then !  this headwater has diffuse additions
      !write(01,4200)rname(KREACH)
 4200 format(//77('=')/'The headwater flow for Reach: ',a16,
     &' ... has diffuse inputs ...'/77('='))
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! the distance is zero ===========
      diffheadqual(KREACH,20,1) = 99 ! note that diffuse pollution is added ====
      JT(feetcount) = 10 ! restore the feature type to 10
      diffcount = 0 ! initialise the counter of diffuse additions
      endif ! if ( DIST(feetcount) .lt. 1.0E-8 ) ===============================
      endif ! if ( JT(feetcount) .eq. -10 ) ====================================
      
*     identify diffuse sources to be added to the headwaters ===================
      if ( diffheadqual(KREACH,20,1) .gt. 0 ) then ! sources indicated #########
      if ( JT(feetcount) .eq. -25 ) then ! agricultural livestock is one +++++++
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
 4201 format('Type of candidate =',i4,f10.2,3x,a20/77('='))
      JT(feetcount) = -JT(feetcount) ! ----------- reset the Feature Type number
      diffcount = diffcount + 1 ! ---------- add to the number of diffuse inputs
      diffheadqual(KREACH,diffcount,1) = IQ ! ---- store the set of quality data
      diffheadqual(KREACH,20,1) = diffcount ! -------- store the sequence number
      diffheadqual(KREACH,diffcount,2) = JT(feetcount) ! -------- store the type
      diffheadqual(KREACH,diffcount,3) = 1.0
      !write(01,4281)JT(feetcount),feetcount,diffcount,
      !&diffheadqual(KREACH,diffcount,3),DIST(feetcount)
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      !write(01,4281)JT(feetcount),feetcount,diffcount,
      !&diffheadqual(KREACH,diffcount,3),DIST(feetcount)
 4281 format('         Distance =',3i4,2f10.2)
      endif ! zero distance found ==============================================
      endif !  +++++++++++++++++++++++++++++++++++ agricultural livestock is one 
      
      if ( JT(feetcount) .eq. -27 ) then ! agricultural arable is one ++++++++++
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      !write(01,4281)JT(feetcount),feetcount,diffcount,
      !&diffheadqual(KREACH,diffcount,3),DIST(feetcount)
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++ agricultural arable is one
      
      if ( JT(feetcount) .eq. -29 ) then ! +++++++++++++++ highway runoff is one
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ! ++++++++++++++++++++++++++++++++++++++++++ highway runoff is one
      
      if ( JT(feetcount) .eq. -31 ) then ! ++++++++++++++++++++++++ urban runoff 
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++++++++++++++++ urban runoff 
      
      if ( JT(feetcount) .eq. -33 ) then ! ++++++++++++++ atmospheric deposition
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++++++ atmospheric deposition
      
      if ( JT(feetcount) .eq. -35 ) then ! ++++++++++++++++++ natural background
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++++++++++ natural background
      
      if ( JT(feetcount) .eq. -37 ) then ! ++++++++++++++++++++++++ septic tanks
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++++++++++++++++ septic tanks
      
      if ( JT(feetcount) .eq. -40 ) then ! +++++++++++++++++++++ aggregated CSOs
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++ aggregated CSOs
      
      if ( JT(feetcount) .eq. -42 ) then ! +++++++++++++ aggregated sewage works
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++ aggregated sewage works
      
      if ( JT(feetcount) .eq. -46 ) then ! +++++++++++++++++++++++ diffuse mines
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++ diffuse mines
      
      if ( JT(feetcount) .eq. -48 ) then ! ++++++++++++ birds, boats and angling
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! +++++++++++++++++++++++++++++++++++++++++ birds, boats and angling
      
      if ( JT(feetcount) .eq. -50) then ! ++++++++++++++++++++ user-defined (50)
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++ user-defined (50)
      
      if ( JT(feetcount) .eq. -52) then ! ++++++++++++++++++++ user-defined (52) 
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++ user-defined (52) 

      if ( JT(feetcount) .eq. -54) then ! ++++++++++++++++++++ user-defined (54) 
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++ user-defined (54)

      if ( JT(feetcount) .eq. -56) then ! ++++++++++++++++++++ user-defined (56)
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++ user-defined (56)

      if ( JT(feetcount) .eq. -58) then ! ++++++++++++++++++++ user-defined (58)
      if ( DIST(feetcount) .lt. 1.0E-8 ) then ! a candidate is identified ======
      !write(01,4201)JT(feetcount),DIST(feetcount),uname(feetcount)
      diffcount = diffcount + 1
      JT(feetcount) = -JT(feetcount)
      diffheadqual(KREACH,diffcount,1) = IQ
      diffheadqual(KREACH,20,1) = diffcount 
      diffheadqual(KREACH,diffcount,2) = JT(feetcount)
      diffheadqual(KREACH,diffcount,3) = 1.0
      if ( DIST(feetcount) .lt. -0.001 ) then ! --------------------------------
      !diffheadqual(KREACH,diffcount,3) = -DIST(feetcount)
      DIST(feetcount) = 0.0
      endif ! ------------------------------------------------------------------
      endif ! zero distance found ==============================================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++ user-defined (58)
           
      else
      
      if ( JT(feetcount) .eq. -25) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -27) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -29) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -31) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -33) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -35) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -37) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -40) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -42) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -46) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -48) then 
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -50) then ! ++++++++++++++++++++ user-defined (50)
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -52) then ! ++++++++++++++++++++ user-defined (52)
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -54) then ! ++++++++++++++++++++ user-defined (54)
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -56) then ! ++++++++++++++++++++ user-defined (56)
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      if ( JT(feetcount) .eq. -58) then ! ++++++++++++++++++++ user-defined (58)
      if ( DIST(feetcount) .lt. -0.001 ) DIST(feetcount) = 0.0
      endif
      
      endif ! a candidate for diffuse addition to headwaters was found #########
            
*     exclude effluents from calculating discharge quality needed to meet ------
*     river targets ------------------------------------------------------------
      skippeff (feetcount) = 0
      if ( JT(feetcount) .lt. 0 ) then
      JT(feetcount) = - JT(feetcount)
      skippeff (feetcount) = 1
      endif
      
      if ( IFRQS(feetcount) .lt. 0 ) then ! check to impose discharge target ---
      IFRQS(feetcount) = - IFRQS(feetcount)
      skippeff (feetcount) = 9999
      endif ! check to impose discharge target over Reach Target ---------------

*     prepare to deal with grouped diffuse inputs ------------------------------
*     one set of flow data is used for several types of diffuse inputs ---------
      add diffconc(feetcount) = 0 !  prepare to deal with grouped diffuse inputs 
      if ( JF(feetcount) .eq. -1 ) then ! no flow is needed for this feature ---
      add diffconc(feetcount) = 1 ! mark this flow as to be ignored ------------
      JF(feetcount) = RFDIFF(KREACH) ! no extra flow is needed -----------------
      do JJP = 1, ndet
      if ( QTYPE (JJP) .ne. 4 ) then
      if ( PDRC (IQ,JJP) .eq. 66 .or. PDRC (IQ,JJP) .eq. 67 .or.
     &     PDRC (IQ,JJP) .eq. 69 .or. PDRC (IQ,JJP) .eq. 611) then
      if ( deleets .eq. 0 ) then
      call change colour of text (20) ! bright red
      !write( *,1722)uname(feetcount),RNAME(KREACH),dname(JJP)
      call set screen text colour
      !call pause
      endif
      !deleets = deleets + 1
      !write(01,1722)uname(feetcount),RNAME(KREACH),dname(JJP)
      !write(09,1722)uname(feetcount),RNAME(KREACH),dname(JJP)
      !write(33,1722)uname(feetcount),RNAME(KREACH),dname(JJP)
 1722 format(77('-')/
     &'*** Problem with diffuse pollution set up with Added ',
     &'Concentration ... '/
     &'*** Feature: ',a33,' ... on Reach: ',a16/
     &'*** The quality has been defined as loads for: 'a11/
     &'*** All such Features have been removed ... '/
     &77('-'))
      !add diffconc(feetcount) = 0
      !JF(feetcount) = 0
      !JQ(feetcount) = 0
      !JREACH(feetcount) = 0
      !feetcount = feetcount - 1
      !goto 38
      endif ! if ( PDRC (IQ,JJP) .eq. 6 .or. PDRC (IQ,JJP) .eq. 7 etc ----------
      endif
      enddo ! do JJP = 1, ndet
      endif ! if ( JF(feetcount) .eq. -1 ) deal with grouped diffuse inputs ----

      if ( add diffconc(feetcount) .eq. 1 ) then ! check reach flow exists =====
      if ( RFDIFF(JREACH(feetcount)) .eq. 0 ) then ! check reach flow exists ===
      call change colour of text (10) ! green ! ================================
      write( *,8495)IREACH,uname(feetcount)  ! =================================
      call set screen text colour ! ============================================
      write(01,8495)IREACH,uname(feetcount)  ! =================================
      write(09,8495)IREACH,uname(feetcount)  ! =================================
      write(33,8495)IREACH,uname(feetcount)  ! =================================
      write(03,8495)IREACH,uname(feetcount)  ! =================================
 8495 format(77('=')/  ! =======================================================
     &'*** No set of diffuse flow data is in the line of ', ! ==================
     &'data for Reach:',i6/ ! ==================================================
     &'*** You have cited these data for use by diffuse pollution ', ! =========
     &'added as Features'/ ! ===================================================
     &'*** Zero flows are assumed for: ',a33/ ! ================================
     &77('=')) ! ===============================================================
      endif ! if ( RFDIFF(JREACH(feetcount)) .eq. 0 ) check reach flow exists ==
      endif ! if ( add diffconc(feetcount) .eq. 0 ) check reach flow exists ====
      
      endif ! if ( feetcount .le. NU ) +++++++++++++++++++++++++++++++++++++++++
      

      if ( feetcount .gt. NU ) then ! too many features ========================
      call change colour of text (20) ! bright red
      write( *,1022)NU
      call set screen text colour
      write(01,1022)NU
      write(09,1022)NU
      write(33,1022)NU
 1022 format(77('-')/
     &'*** Too many Features have been specified ... '/
     &'*** Only the first',I6,' have been retained ... '/
     &77('-'))
      !call pause
      MU = NU ! curtail final value for the total number of valid features ----
      return
      endif ! too many features ===============================================

     
      if ( DIST (feetcount) .lt. -0.0000001 ) then ! check for negative DIST ==
      call change colour of text (38) ! dull green
      write( *,1882)JREACH(feetcount)
 1882 format(
     &'*** Feature entered with negative distance',9x,'...',7x,
     &'This is set to zero ... ',20x,'Reach number ',i6,' .....')
      call set screen text colour
      write(01,1822)uname(feetcount),JREACH(feetcount)
      write(09,1822)uname(feetcount),JREACH(feetcount)
      write(33,1822)uname(feetcount),JREACH(feetcount)
 1822 format(77('-')/
     &'*** A feature is entered with negative distance ... '/
     &'*** This has been reset to zero ... '/
     &'*** Check... ',a37,' on Reach number ',i6/77('-'))
      endif ! check for negative DIST ==========================================
      
*     adjust distances where features have the same distances down the reach ---
      if ( feetcount .gt. 1 ) then
      itemp = feetcount - 1
      if ( JReach (feetcount) .eq. JReach (itemp) ) then
      if ( DIST (feetcount) .eq. DIST (itemp) ) then
      Dist (feetcount) = DIST (feetcount) + 0.000000001
      endif
      endif
      endif

*     check for errors in the distances set for features -----------------------
      if ( feetcount .gt. 1 ) then
*     exclude (44) Flow into a lake and (45) Flow from a lake ------------------
      if ( JT(feetcount) .ne. 44 .and. JT(feetcount) .ne. 45 ) then
      itemp = feetcount - 1
      if ( JReach(feetcount) .eq. JReach(itemp) ) then
      if ( DIST(itemp) - DIST(feetcount) .gt. 0.0000001 ) then
      if ( kerror .eq. 1 ) then
      suppress1 = suppress1 + 1
      if ( suppress1 .le. 6 ) then
      call change colour of text (12) ! orange
      write( *,6492)UNAME(itemp)
 6492 format('*** Error in the sequence of features',
     &14x,'...',7x,'at ',a37)!,4x,'And afterwards ..........')
      call set screen text colour
      endif
      if ( suppress1 .eq. 7 ) then
      call change colour of text (12) ! orange
      write( *,8792)UNAME(itemp)
 8792 format('*** Error in the sequence of features',
     &14x,'...',7x,'at ',a37,4x,'and elsewhere ...........')
      call set screen text colour
      endif
      endif ! if ( kerror .eq. 1 )
      irh = JReach(feetcount)
      if ( ical13 .eq. 0 ) write(01,8412)rname(irh),UNAME(itemp),
     &UNAME(feetcount),DIST (feetcount),DIST (itemp)      
      write(09,8412)rname(irh),UNAME(itemp),UNAME(feetcount),
     &DIST (feetcount),DIST (itemp)      
      write(33,8412)rname(irh),UNAME(itemp),UNAME(feetcount),
     &DIST (feetcount),DIST (itemp)      
 8412 format(/77('-')/
     &'*** Error in the sequencing of features in reach: ',a16,' ... '/
     &'*** Features - ',A40/'***            ',A40/
     &'*** Distance has been changed from',f8.2 ,' to',f8.2/77('-'))
      DIST (feetcount) = DIST (itemp)      
      endif
      endif
      endif
      endif ! if ( feetcount .gt. 1 )

*     insert extra Plotting Points ---------------------------------------------
      if ( feetcount .gt. 1 .and. KINT .eq. 1 ) then

*     number of feature entered before this one --------------------------------
      itemp = feetcount - 1
      DDDD = DIST(feetcount)

*     check same reach ---------------------------------------------------------
      if ( JReach (feetcount) .eq. JReach (itemp) ) then
      if ( DIST (feetcount) - DIST (itemp) .gt. 1.00 ) then
      DDDDD = float (int ( DIST(itemp) + 1.0 ) )
      if ( DDDDD .ge. DDDD ) goto 9911
      if ( DDDDD .ge. RLENGTH(JREACH(feetcount))) goto 9911
      UNAME(feetcount) = "  ....         "
      JT(feetcount) = 0
      JREACH(feetcount) = JREACH(itemp)
      JF(feetcount) = 0
      JQ(feetcount) = 0
      JFCAL(feetcount) = 0
      JQCAL(feetcount) = 0
      IFRQS(feetcount) = IFRQS(itemp)
      DIST(feetcount) = DDDDD

      backspace 02
      goto 38
      endif

      else

      if ( RLENGTH(JREACH(itemp)) - DIST (itemp) .gt. 1.0 ) then

      DDDDD = float (int ( DIST(itemp) + 1.0 ) )
      if ( DDDDD .ge. RLENGTH(JREACH(itemp))) goto 9911
      UNAME(feetcount) = "  .....         "
      JT(feetcount) = 0
      JREACH(feetcount) = JREACH(itemp)
      JF(feetcount) = 0
      JQ(feetcount) = 0
      JFCAL(feetcount) = 0
      JQCAL(feetcount) = 0
      IFRQS(feetcount) = IFRQS(itemp)
      DIST(feetcount) = DDDDD

      backspace 02
      goto 38

      endif
      endif
      endif
 9911 continue

*     perform some general tests -----------------------------------------------
*     test for an illegal Feature Type -----------------------------------------
      IT = JT(feetcount)

*     check for illegal types of feature ---------------------------------------
      if ( IT .le. 0 ) goto 39         
      if ( IT .gt. 62 ) then
      call change colour of text (20) ! bright red ... error message -----------
      write( *,4412)IT,UNAME(feetcount)
      call set screen text colour ! error message ------------------------------
      write(01,4412)IT,UNAME(feetcount)
      write(09,4412)IT,UNAME(feetcount)
      write(33,4412)IT,UNAME(feetcount)
 4412 format(77('-')/
     &'*** Illegal Feature Type .... Code type - ',I4/
     &'*** Feature - ',A40/
     &'*** Run continues but the Feature has been excluded'/
     &77('-'))
      goto 39   
      endif
*     --------------------------------------------------------------------------

*     test for an illegal Reach Code -------------------------------------------
      IREACH = JREACH(feetcount)

      if ( IREACH .le. 0 ) then
      call change colour of text (20) ! bright red
      write( *,4812) feetcount,IREACH,UNAME(feetcount)
 4812 format(
     *'*** Illegal Reach Code for Feature number -',I6/
     &'*** The Reach Code is - ',I6,
     &'    The Feature is  - ',A40/
     &'*** The Feature is excluded ... ')
      call set screen text colour
      write(01,4312) feetcount,IREACH,UNAME(feetcount)
      write(09,4312) feetcount,IREACH,UNAME(feetcount)
      write(33,4312) feetcount,IREACH,UNAME(feetcount)
 4312 format(100('-')/
     &'*** Illegal Reach Code for Feature number -',I6/
     &'*** The Reach Code is - ',I6,
     &'    The Feature is  - ',A40/
     &'*** SIMCAT continues but the Feature has been excluded ... '/
     &100('-'))
      JT(feetcount) = 0
      JREACH(feetcount) = 0
      goto 39
      endif

 6233 continue

*     check that the reach data have been read for this Reach Code -------------
      do 8431 iorder = negorder, nreach
      if ( ireach .eq. ReachCode (iorder) ) goto 8432
 8431 continue

      if ( negreach .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,4912) feetcount,IREACH,UNAME(feetcount)
 4912 format('*** The Reach Code for Feature -',I4,' is not in the ',
     &'Reach data ... '/
     &'* The Reach Code is - ',I4,
     &'   The Feature is - ',A40,'    '/
     &'* The Feature is excluded ... ')
      call set screen text colour
      write(01,4612) feetcount,IREACH,UNAME(feetcount)
      write(09,4612) feetcount,IREACH,UNAME(feetcount)
      write(33,4612) feetcount,IREACH,UNAME(feetcount)
 4612 format(100('-')/
     &'*** The Reach Code for Feature -',I4,' is not in the ',
     &'Reach data ... '/
     &'*** The Reach Code is - ',I4,
     &'   The Feature is - ',A40,'    '/
     &'*** SIMCAT continues but the Feature is excluded ... '/
     &100('-'))
      endif

      goto 39
 8432 continue


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     Check for Gap Filling data for an Upstream Boundary (10) -----------------
*     ############### check need to include bifurcations #######################
      if ( IT .eq. 10 .or. IT .eq. 45 ) then
      if ( JFCAL(feetcount) .ne. 0 .and. ical .eq. 1 ) then
      JFCAL(feetcount) = 0
      write(01,8350)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,8350)UNAME(feetcount)
      write(09,8350)UNAME(feetcount)
      write(33,8350)UNAME(feetcount)
 8350 format(80('*')/
     &'*** An Upstream Boundary has Gap Filling data ',
     &'for river flow ... '/
     &'*** Gap filling suppressed for ', A33/80('*'))
      endif
      if ( JQCAL(feetcount) .ne. 0 .and. ical .eq. 3 ) then
      JQCAL(feetcount) = 0
      write(01,8450)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,8450)UNAME(feetcount)
      write(09,8450)UNAME(feetcount)
      write(33,8450)UNAME(feetcount)
 8450 format(80('*')/
     &'*** An Upstream Boundary has Gap Filling data ',
     &'for river quality ... '/
     &'*** Gap filling suppressed for ', A33/80('*'))
      endif
      endif ! if ( IT .eq. 10 .or. IT .eq. 45 ) --------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     check for illegal references to data -------------------------------------
*     check for legal Code for river flow dataset ------------------------------
      if ( IF .lt. 0 .or. IF .gt. NF) then ! -----------------------------------
      if ( IF .ne. -1 .and. add diffconc(feeture) .ne. 1 ) then ! --------------
      write( *,1021)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(01,1021)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(09,1021)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(33,1021)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
 1021 format(77('-')/
     &'*** Data for a Feature have been entered wrongly ...'/
     &'*** Illegal code for a river flow dataset  ... ',i5/
     &'*** Name of Feature: ',A32,' Number:',I7/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,':',A16,' ...'/
     &'*** The calculations have been STOPPED ...'/77('-'))
      call stop
      endif ! if ( IF .lt. 0 .or. IF .gt. NF) ----------------------------------
      endif ! if ( IF .ne. -1 .and. add diffconc(feeture) .ne. 1 ) -------------

*     check whether the River Flow Dataset has been read in --------------------
      if ( if .ne.  0 .and. IT .ne. 11 .and. ! =================================
     &     IT .ne. 20 .and. IT. ne. 21 .and. ! bifurcation
     &     IT .ne. 22 .and. IT. ne. 23) then ! bifurcation
      if ( if .ne. -1 .and. add diffconc(feeture) .ne. 1 ) then ! ===============
      do 2000 lf = 1, nf
      if ( PDRF(lf) .gt. -999 ) then
      if ( lf .eq. if ) goto 2001
      endif
 2000 continue
      write(01,1821)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      if ( iscreen .lt. 3 ) then
      call change colour of text (20) ! bright red
      write( *,1821)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      call set screen text colour
      endif
      write(09,1821)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(33,1821)IF,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
 1821 format(77('-')/
     &'*** River flow dataset specified for Feature has not ',
     &'been entered ...'/
     &'*** The code number of this data-set was:',i6/
     &'*** Name of Feature: ',A32,' Number:',I5/
     &'*** Located',F6.1,' km. from the head of ',
     &'Reach',I5,' (',A16,')'/77('-'))
 2001 continue
      endif ! if ( if .ne. -1 .and add diffconc(feeture) .ne. 1 ) ==============
      endif ! if ( if .ne.  0 .and. IT .ne. 11 etc =============================


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     check Flow Dataset specified for gap filling -----------------------------
      kf = iabs (jfcal(feetcount))

      if ( kf .gt. NF) then
      write(01,1121)UNAME(feetcount),feetcount,DIST(feetcount),KREACH,
     &             RNAME(KREACH)
      write( *,1121)UNAME(feetcount),feetcount,DIST(feetcount),KREACH,   
     &             RNAME(KREACH)
      write(09,1121)UNAME(feetcount),feetcount,DIST(feetcount),KREACH,
     &             RNAME(KREACH)
      write(33,1121)UNAME(feetcount),feetcount,DIST(feetcount),KREACH,
     &             RNAME(KREACH)
 1121 format(77('-')/
     &'*** Data for a Feature have been entered wrongly ... '/
     &'*** Illegal code for a river flow gap filling dataset'/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ... '/
     &'*** The calculations have been stopped ... '/
     &77('-'))
      call stop
      endif

*     check whether Flow Dataset for gap filling has been read in --------------
      if ( kf .ne. 0 ) then
      do 2160 lf = 1, nf
      if ( PDRF(lf) .gt. -999 ) then	
      if ( lf .eq. iabs (kf) ) goto 2101
      endif
 2160 continue

      write(01,1827)kf,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write( *,1827)kf,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,1827)kf,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,1827)kf,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 1827 format(77('-')/
     &'*** River flow dataset specified for gap filling has ',
     &'not been entered ... '/
     &'***                  ..........            Code:',i6/
     &'*** Name of Feature: ',A32,' Number:',I6/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ... '/77('-'))
 2101 continue
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



      if ( IT .eq. 03 .or. IT .eq. 15 .or. IT .eq. 05  .or.
     &     IT .eq. 12 .or. IT .eq. 17 .or. IT .eq. 39 .or.
     &     IT .eq. 42 .or. IT .eq. 60 .or. IT .eq. 61 .or.
     &     IT .eq. 62 ) then
      if ( IF .ne. 0 .and. kerror .eq. 1 ) then
      suppress00 = suppress00 + 1 ! incorrect flow for a discharge
      call change colour of text (53) ! indian red
      write( *,8281)UNAME(feetcount)
 8281 format(
     &'*** Removed incorrect flow data for an effluent',4x,'...',
     &7x,a40,4x,'Check ERR or OUT files ..')
      call set screen text colour
      write(01,8221)IF,IT,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(09,8221)IF,IT,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
      write(33,8221)IF,IT,UNAME(feetcount),feetcount,DIST(feetcount),
     &KREACH,RNAME(KREACH)
 8221 format(77('-')/
     &'*** Data for a feature have been entered wrongly ... '/
     &'*** The code for a set of river flow data numbered:',i6/
     &'*** Has been entered for an effluent feature of type',i4/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ... '/
     &'*** The code for the flow dataset has been set to zero ...'/
     &77('-'))
      IF = 0
      JF(feetcount) = 0
      PDEF(IQ) = 0
      endif
      endif

*     check for a legal Code for the River Quality Dataset ---------------------
      if ( IT .ne. 03 .and. IT .ne. 15 .and. IT .ne. 05 .and.
     &     IT .ne. 12 .and. IT .ne. 17 .and. IT .ne. 18 .and. 
     &     IT .ne. 19 .and. IT .ne. 20 .and. IT .ne. 21 .and.
     &     IT .ne. 22 .and. IT .ne. 23 .and. IT .ne. 39 .and.
     &     IT .ne. 42 .and. IT .ne. 60 .and. IT .ne. 61 .and.
     &     IT .ne. 62) then

      if ( IQ .lt. 0 .or. IQ .gt. NV ) then
      write(01,1221)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write( *,1221)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,1221)IQ,UNAME(feetcount),fe,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,1221)IQ,UNAME(feetcount),fe,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 1221 format(77('-')/
     &'*** Data for a feature have been entered wrongly ... '/
     &'*** Illegal code for a river quality dataset ... ',i5/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ... '/
     &'*** The calculations have been stopped ... '/
     &77('-'))
      call stop
      endif
      
*     check whether the river quality dataset has been read in -----------------
      if ( iq .ne. 0 ) then ! --------------------------------------------------
      do 2110 lv = 1, nv
      do 2119 idet = 1, ndet
      if ( qtype (idet) .eq. 4 ) goto 2119
      if ( pdrc(lv,idet) .gt. -1 ) then
      if ( lv .eq. iq ) goto 2111
      endif
 2119 continue
 2110 continue
      call change colour of text (20) ! bright red
      write( *,8921)IQ,UNAME(feetcount)
 8921 format('*** River quality dataset (',i5,') has ',
     &'not been entered ...',2x,'for ',a40)
      call set screen text colour
      write(01,8821)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,8821)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,8821)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 8821 format(77('-')/
     &'*** River quality dataset specified for Feature has ',
     &'not been entered ...'/
     &'***                  ..........             Code:',i6/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ...'/77('-'))
      call stop
 2111 continue

      endif ! if ( iq .ne. 0 ) then ! ------------------------------------------
      
      endif

*     check for a legal code for the Discharge Flow/Quality Dataset ------------
*     apply to [3] [5] [12] [39] [15] [16] [42] etc ------ check for legal codes
      if ( IT .eq. 3 .or. IT .eq. 5 .or. IT .eq. 12 .or. IT .eq. 15 .or.
     &     IT .eq. 16 .or. IT .eq. 39 .or. IT .eq. 42 .or.
     &     IT .eq. 60 .or. IT .eq. 61 .or. IT .eq. 62 ) then

      if ( IQ .lt. 0 .or. IQ .gt. NE ) then
      write(01,1227)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write( *,1227)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,1227)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,1227)IQ,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 1227 format(77('-')/
     &'*** Data for a Feature have been entered wrongly ...'/
     &'*** Illegal code for a discharge dataset ...',i4/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ...'/
     &'*** The calculations have been stopped ...'/77('-'))
      call stop
      endif

*     check whether the discharge dataset has been read in ---------------------
      if ( iq .ne. 0 ) then
      do 3110 le = 1, ne
      if ( pdef(le) .gt. -1 ) then
        if ( le .eq. iq ) goto 3111
      endif
 3110 continue
      write(01,8831)IQ,UNAME(feetcount),feetcount,
     &DIST(feetcount),KREACH,RNAME(KREACH)
      call change colour of text (12) ! orange
      write( *,8831)IQ,UNAME(feetcount),feetcount,
     &DIST(feetcount),KREACH,RNAME(KREACH)
      call set screen text colour
      write(09,8831)IQ,UNAME(feetcount),feetcount,
     &DIST(feetcount),KREACH,RNAME(KREACH)
      write(33,8831)IQ,UNAME(feetcount),feetcount,
     &DIST(feetcount),KREACH,RNAME(KREACH)
 8831 format(77('-')/
     &'*** An effluent dataset specified for a Feature has not ',
     &'been entered ... '/'*** Code number of the effluent dataset:',i6/
     &'*** Name of the Feature: ',A32,' Number:',I6/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ...'/
     &'*** The calculations have been halted ...'/77('-'))
      call pause
 3111 continue
      endif

      endif


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     check river quality data specified for gap filling -----------------------
      if ( ical .ne. 1 ) then
      kq = JQCAL(feetcount)
      if ( kq .gt. NV) then
      write(01,1321)UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write( *,1321)UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,1321)UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,1321)UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 1321 format(77('-')/
     &'*** Data for a Feature have been entered wrongly ...'/
     &'*** Illegal river quality dataset for gap filling ...'/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ...'/
     &'*** The calculations have been stopped ...'/77('-'))
      call stop
      endif


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     check whether Quality Dataset for gap filling has been read in -----------
      if ( kq .ne. 0 ) then
      do 4100 lq = 1, nv
      if ( qtype (idet) .eq. 4 ) goto 4199
      if ( pdrc(lq,idet) .gt. -1 ) then
      if ( lq .eq. iabs (kq) ) goto 4101
      endif
 4199 continue
 4100 continue
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      if ( ical .eq. 3 ) then
      call change colour of text (10) ! green
      write( *,8961)IQ,UNAME(feetcount)
 8961 format('*** River quality dataset (',i5,') has ',
     &'not been entered for gap filling ... ',5x,'at ',a40,1x,25('.'))
      call set screen text colour
      write(01,4827)kq,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(09,4827)kq,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
      write(33,4827)kq,UNAME(feetcount),feetcount,DIST(feetcount),
     &             KREACH,RNAME(KREACH)
 4827 format(77('-')/
     &'*** River quality dataset specified for gap filling has ',
     &'not been entered ... '/
     &'***                  ..........            Code:',i6/
     &'*** Name of Feature: ',A32,' Number:',I4/
     &'*** Located',F5.1,' km. from the head of ',
     &'Reach',I5,' (',A16,') ... '/77('-'))
      endif
 4101 continue
      endif
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



*     now perform checks for individual types of feature -----------------------
*     Routine Monitoring Points (1) --------------------------------------------
      if ( IT .eq. 1 ) call routine monitoring point

*     streams and minor tributaries (3) ----------------------------------------
      if ( IT .eq. 2 ) then
      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      suppress17 = suppress17 + 1 ! deleted feature
      call change colour of text (15) ! white
      if ( iscreen .lt. 3 ) write( *,4212)UNAME(feetcount)
      call set screen text colour
 4292 format(
     &'* No valid quality data for a stream ... ',A33)
      write(01,4212)UNAME(feetcount)
      write(09,4212)UNAME(feetcount)
      write(33,4212)UNAME(feetcount)
 4212 format(77('-')/
     &'*** No valid river quality Data-Set Code for a stream    '/
     &'*** Name of Feature: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif

*     if ( QNUM(IQ,1)+QNUM(IQ,2)+QNUM(iq,3) .lt. 0 ) then
      if ( QNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7851)UNAME(feetcount)
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7851)UNAME(feetcount)
      write(09,7851)UNAME(feetcount)
      write(33,7851)UNAME(feetcount)
 7851 format(77('-')/
     &'*** No quality data specified for a stream ...'/
     &'*** Name of Feature: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif

*     check data on river flow -------------------------------------------------
      if ( IF .le. 0 .or. IF .gt. NF ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,4213)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,4283)UNAME(feetcount)
 4283 format(
     &'*** No valid river flow data for a stream',10x,'...',7x,
     &a37,7x,'Feature deleted ... ')
      call set screen text colour
      endif
      write(09,4213)UNAME(feetcount)
      write(33,4213)UNAME(feetcount)
 4213 format(77('-')/
     &'*** No valid river flow Data-Set Code for a stream ... '/
     &'*** Name of Feature: ',A33,'  '/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif

      if ( PDRF(IF) .ne. 6 .or. PDRF(IF) .ne. 7 ) then
      if ( PDRF(IF) .ne. 9 ) then
      if ( PDRF(IF) .ne. 4 ) then
      if ( F(IF,1) .lt. 1.0E-10 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (1)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7154)UNAME(feetcount)
 7154 format(    
     &'*** Zero flow specified for a stream',15x,'...',7x,
     &'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) write(01,7854)UNAME(feetcount)
      write(09,7854)UNAME(feetcount)
      write(33,7854)UNAME(feetcount)
 7854 format(/77('-')/     
     &'*** Zero flow specified for a stream ...'/
     &'*** Name of Feature: ',A37/77('-'))
      endif ! if ( F(IF,1) .lt. 1.0E-10 )
      endif
      endif
      endif


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     check for request for gap filling ----------------------------------------
      if  ( JQCAL(feetcount) .ne. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7154)UNAME(feetcount)
      write(01,7354)UNAME(feetcount)
      write(09,7354)UNAME(feetcount)
      write(33,7354)UNAME(feetcount)
 7354 format(77('-')/'*** Warning ... '/
     &'*** Gap filling requested for a stream  '/
     &'*** The name of the feature is: ',A33/
     &'*** Are you sure this is needed ? '/77('-'))
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      endif

*     sewage treatment works (feature type 3) ----------------------------------
      if ( IT .eq. 3 ) then
*     check for illegal dataset code -------------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,3855)UNAME(feetcount)
      write(09,3855)UNAME(feetcount)
      write(33,3855)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      write( *,3855)UNAME(feetcount)
      call change colour of text (10) ! green
 3855 format(77('-')/
     &'*** No Data-Set specified for sewage discharge ... '/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7862)UNAME(feetcount)
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7862)UNAME(feetcount)
      write(09,7862)UNAME(feetcount)
      write(33,7862)UNAME(feetcount)
 7862 format(77('-')/
     &'*** No quality data specified for a discharge ...'/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif
      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7516)UNAME(feetcount)
      call set screen text colour 
      endif
      !write(09,7516)UNAME(feetcount)
      write(33,7516)UNAME(feetcount)
 7516 format(77('-')/
     &'*** Zero flow has been specified for an effluent discharge'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data -------------------------------------
      if ( IF .ne. 0 ) then
      write(01,3856)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3856)UNAME(feetcount)
      suppress16 = suppress16 + 1 ! unneeded if ( F(IF,1) .lt. 1.0E-10 ) data ignored (5)
      write(09,3856)UNAME(feetcount)
      write(33,3856)UNAME(feetcount)
 3856 format(77('-')/
     &'*** River Flow Data-Set specified for a sewage discharge  '/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif

*     industry (feature type 5) ------------------------------------------------
      if ( IT .eq. 5 ) then
*     check for illegal dataset code -------------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,5855)UNAME(feetcount)
      write(09,5855)UNAME(feetcount)
      write(33,5855)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,5855)UNAME(feetcount)
 5855 format(77('-')/
     &'*** No Data-Set specified for an industrial discharge ... '/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif
      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7892)UNAME(feetcount)
      call set screen text colour 
      endif
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7862)UNAME(feetcount)
      write(09,7862)UNAME(feetcount)
      write(33,7862)UNAME(feetcount)
 7892 format(77('-')/
     &'*** No quality data specified for a discharge ... '/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif
*     --------------------------------------------------------------------------

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
*     if ( iscreen .lt. 3 ) write( *,7516)UNAME(feetcount)
      !write(09,7516)UNAME(feetcount)
      write(33,7516)UNAME(feetcount)
 7596 format(77('-')/
     &'*** Zero flow has been specified for an effluent discharge'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data ...

      if ( IF .ne. 0 ) then
      write(01,2856)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,2856)UNAME(feetcount)
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored (6)
      write(09,2856)UNAME(feetcount)
      write(33,2856)UNAME(feetcount)
 2856 format(77('-')/
     &'*** River Flow Data-Set specified for an industrial discharge'/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif

*     mine water (feature type 39) ---------------------------------------------
      if ( IT .eq. 39 ) then
*     check for an illegal dataset code ----------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,2855)UNAME(feetcount)
      write(09,2855)UNAME(feetcount)
      write(33,2855)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,2855)UNAME(feetcount)
 2855 format(77('-')/
     &'*** No Data-Set specified for a mine water ... '/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,2892)UNAME(feetcount)
      call set screen text colour 
      endif
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7862)UNAME(feetcount)
      write(09,7862)UNAME(feetcount)
      write(33,7862)UNAME(feetcount)
 2892 format(77('-')/
     &'*** No quality data specified for a mine water ... '/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,2596)UNAME(feetcount)
      call set screen text colour 
      endif
      suppress19 = suppress19 + 1 ! unneeded data ignored (39)
      write(09,7516)UNAME(feetcount)
      write(33,7516)UNAME(feetcount)
 2596 format(77('-')/
     &'*** Zero flow has been specified for a mine water (39)'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data -------------------------------------
      if ( IF .ne. 0 ) then
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored
      write(01,2851)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,2851)UNAME(feetcount)
      write(09,2851)UNAME(feetcount)
      write(33,2851)UNAME(feetcount)
 2851 format(77('-')/
     &'*** River Flow Data-Set specified for mine water  '/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif

*     Other Point Sources (feature type 60) ------------------------------------
      if ( IT .eq. 60 ) then
*     check for an illegal dataset code ----------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,6855)UNAME(feetcount)
      write(09,6855)UNAME(feetcount)
      write(33,6855)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,6855)UNAME(feetcount)
 6855 format(77('-')/
     &'*** No Data-Set specified for an other point source (60)'/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,6892)UNAME(feetcount)
 6892 format(77('-')/
     &'*** No quality data specified for an other point source (60) '/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7862)UNAME(feetcount)
      write(09,7862)UNAME(feetcount)
      write(33,7862)UNAME(feetcount)
      goto 39
      endif

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,8516)UNAME(feetcount)
      call set screen text colour 
      endif
      suppress19 = suppress19 + 1 ! unneeded data ignored (60)
      write(09,8516)UNAME(feetcount)
      write(33,8516)UNAME(feetcount)
 8516 format(77('-')/
     &'*** Zero flow has been specified for an other point source (60)'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data -------------------------------------
      if ( IF .ne. 0 ) then
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored
      if ( iscreen .lt. 3 ) write( *,2851)UNAME(feetcount)
      write(01,2051)UNAME(feetcount)
      write(09,2051)UNAME(feetcount)
      write(33,2051)UNAME(feetcount)
 2051 format(77('-')/
     &'*** River Flow Data-Set specified for an other point source'/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif

*     private wastewater (feature type 61) -------------------------------------
      if ( IT .eq. 61 ) then
*     check for an illegal dataset code ----------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7885)UNAME(feetcount)
      write(09,7885)UNAME(feetcount)
      write(33,7885)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7885)UNAME(feetcount)
 7885 format(77('-')/
     &'*** No Data-Set specified for a private wastewater ... '/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) then
      write( *,7898)UNAME(feetcount)
      endif
 7898 format(77('-')/
     &'*** No quality data specified for a private wastewater ... '/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7898)UNAME(feetcount)
      write(09,7898)UNAME(feetcount)
      write(33,7898)UNAME(feetcount)
      goto 39
      endif

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
*     if ( iscreen .lt. 3 ) write( *,6696)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (61)
      write(09,7516)UNAME(feetcount)
      write(33,7516)UNAME(feetcount)
 6696 format(77('-')/
     &'*** Zero flow has been specified for an "other point source"'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data -------------------------------------
      if ( IF .ne. 0 ) then
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored
      write(01,2851)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,9851)UNAME(feetcount)
      write(09,2851)UNAME(feetcount)
      write(33,2851)UNAME(feetcount)
 9851 format(77('-')/
     &'*** River Flow Data-Set specified for an other point source'/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif

      
      
*     fish farms (feature type 62) ---------------------------------------- (62)
      if ( IT .eq. 62 ) then ! ============================================ (62)
*     check for an illegal dataset code ----------------------------------------
      if ( IQ .le. 0 .or. IQ .gt. NE ) then ! ----------------------------------
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7085)UNAME(feetcount)
      write(09,7085)UNAME(feetcount)
      write(33,7085)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7085)UNAME(feetcount)
 7085 format(77('-')/
     &'*** No Data-Set specified for a fish farm ... '/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif ! if ( IQ .le. 0 .or. IQ .gt. NE ) ---------------------------- (62)

      if ( PNUM(IQ,1) .lt. 0 ) then ! ------------------------------------- (62)
      if ( iscreen .lt. 3 ) then
      write( *,7098)UNAME(feetcount)
      endif
 7098 format(77('-')/
     &'*** No quality data specified for a fish farm ... '/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      suppress17 = suppress17 + 1 ! deleted feature
      write(01,7098)UNAME(feetcount)
      write(09,7098)UNAME(feetcount)
      write(33,7098)UNAME(feetcount)
      goto 39
      endif ! if ( PNUM(IQ,1) .lt. 0 ) ------------------------------------ (62)

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then ! ------------ (62) 
*     if ( iscreen .lt. 3 ) write( *,6096)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (62)
      write(09,6096)UNAME(feetcount)
      write(33,6096)UNAME(feetcount)
 6096 format(77('-')/
     &'*** Zero flow has been specified for a "fish farm"'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check for absence of river flow data -------------------------------- (62)
      if ( IF .ne. 0 ) then ! --------------------------------------------- (62)
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored
      write(01,9051)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,9051)UNAME(feetcount)
      write(09,9051)UNAME(feetcount)
      write(33,9051)UNAME(feetcount)
 9051 format(77('-')/
     &'*** River Flow Data-Set specified for an other point source'/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif ! if ( IF .ne. 0 ) -------------------------------------------- (62)
      endif ! fiash farm (62) ============================================= (62)

      
*     interpolation point (6) - plotting points --------------------------------
      if ( IT .eq. 6 ) then
*     if ( IF .eq. 0 .and. feetcount .gt. 15830) goto 39
      if ( IF .ne. 0 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (6)
      write(01,3156)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3156)UNAME(feetcount)
      write(09,3156)UNAME(feetcount)
      write(33,3156)UNAME(feetcount)
 3156 format(77('-')/
     &'*** A river flow dataset is specified for an Interpolation ',
     &'Point ...'/
     &'*** Reference to the dataset has been removed ...'/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( IQ .ne. 0) then
      write(01,3816)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (6)
      if ( iscreen .lt. 3 ) write( *,3816)UNAME(feetcount)
      write(09,3816)UNAME(feetcount)
      write(33,3816)UNAME(feetcount)
 3816 format(77('-')/
     &'*** A river quality dataset is specified for an Interpolation ',
     &'Point ...'/'*** Reference to the dataset has been removed ...'/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      
*     delete plotting points in large models -----------------------------------
*     if ( nreach .gt. 500 ) then
*     JT(feetcount) = 0
*     JREACH(feetcount) = 0
*     goto 39
*     endif
      
      endif ! plotting points (6)

*     flow into lake (44) ------------------------------------------------------
      if ( IT .eq. 44 ) then
      IR = JReach (feetcount) 
*     over-write distance with the length of the reach -------------------------
      DIST (feetcount) = rlength (ir)

      if ( IF .ne. 0 ) then
      write(01,3806)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (44)
      if ( iscreen .lt. 3 ) write( *,3806)UNAME(feetcount)
      write(09,3806)UNAME(feetcount)
      write(33,3806)UNAME(feetcount)
 3806 format(77('-')/
     &'*** A river flow dataset is specified for a Flow into Lake ...'/
     &'*** Reference to the Data-Set was removed ...'/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( IQ .ne. 0) then
      write(01,4816)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (44)
      if ( iscreen .lt. 3 ) write( *,4816)UNAME(feetcount)
      write(09,4816)UNAME(feetcount)
      write(33,4816)UNAME(feetcount)
 4816 format(77('-')/
     &'*** River quality dataset is specified for flow into a Lake ...'/
     &'*** Reference to the was Data-Set removed ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      endif

*     flow from lake and into a river (45) ----------------------------------(45)
      if ( IT .eq. 45 ) then
      IR = JReach (feetcount) 
      !Rlength(IREACH) = 0.0 ! set the length of the receiving reach to zero (45)
      DIST (feetcount) = 0.0
      if ( IF .le. 0 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (45)
      write(01,3196)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3196)UNAME(feetcount)
      write(09,3196)UNAME(feetcount)
      write(33,3196)UNAME(feetcount)
 3196 format(77('-')/
     &'*** No flow dataset is specified for flow from a Lake ...'/
     &'*** Reference to the Data-Set was removed ...'/
     &'*** The name of the feature is: ',A33/77('-'))
*     JF(feetcount) = 0
      endif

      if ( IQ .le. 0) then
      write(01,9816)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (45)
      if ( iscreen .lt. 3 ) write( *,9816)UNAME(feetcount)
      write(09,9816)UNAME(feetcount)
      write(33,9816)UNAME(feetcount)
 9816 format(77('-')/
     &'*** No quality dataset is specified for flow from a Lake ...'/
     &'*** Reference to the was Data-Set was removed ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      endif

*     sub-catchment boundary (24) ----------------------------------------------
      if ( IT .eq. 24 ) then
      if ( IF .ne. 0 ) then
      write(01,2156)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (24)
      if ( iscreen .lt. 3 ) write( *,2156)UNAME(feetcount)
      write(09,2156)UNAME(feetcount)
      write(33,2156)UNAME(feetcount)
 2156 format(77('-')/
     &'*** A River Flow Data-Set is specified for a sub-catchment ',
     &'boundary ....'/'*** Reference to the dataset was removed ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( IQ .ne. 0) then
      write(01,3896)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (24)
      if ( iscreen .lt. 3 ) write( *,3896)UNAME(feetcount)
      write(09,3896)UNAME(feetcount)
      write(33,3896)UNAME(feetcount)
 3896 format(77('-')/
     &'*** River quality dataset is specified for a sub-catchment ',
     &'boundary ...'/'*** Reference to the dataset was removed ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      endif

*     discharge with zero flow (17) --------------------------------------------
      if ( IT .eq. 17 ) then
      if ( IF .ne. 0 ) then
      if ( nobigout .le. 0 ) write(01,3656)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (17)
*     if ( iscreen .lt. 3 ) write( *,3656)UNAME(feetcount)
*     write(09,3656)UNAME(feetcount)
      write(33,3656)UNAME(feetcount)
 3656 format(77('-')/
     &'*** A river flow dataset is given to a Zero-flow ',
     &'Discharge (Feature Type 17) ...'/
     &'*** Reference to the dataset was removed ... '/
     &'*** Name of Feature: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( IQ .ne. 0) then
      if ( nobigout .le. 0 ) write(01,7816)UNAME(feetcount)
*     if ( iscreen .lt. 3 ) write( *,7816)UNAME(feetcount)
*     write(09,7816)UNAME(feetcount)
      suppress16 = suppress16 + 1 ! unneeded effluent data ignored (17)
      write(33,7816)UNAME(feetcount)
 7816 format(77('-')/
     &'*** A quality dataset is given to a Zero-flow ',
     &'Discharge (Feature Type 17) ...'/
     &'*** Reference to the data-set was removed ... '/
     &'*** Name of Feature: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      endif

*     abstraction (7) ----------------------------------------------------------
      if ( IT .eq. 7 ) then
      if ( IQ .ne. 0 ) then
      if ( nobigout .le. 0 ) write(01,3817)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3817)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (7)
      write(09,3817)UNAME(feetcount)
      write(33,3817)UNAME(feetcount)
 3817 format(77('-')/
     &'*** River quality dataset is specified for an ',
     &'Abstraction Point ... '/
     &'*** Feature type 7 - the value should be zero      '/
     &'*** The reference to the dataset was removed for this run '/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
      endif

*     abstraction - the negative discharge (18) --------------------------------
      if ( IT .eq. 18 ) then
*     check data on abstraction ------------------------------------------------
      if ( IQ .ne. 0 ) then
      if ( nobigout .le. 0 ) write(01,3857)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3857)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (18)
      write(09,3857)UNAME(feetcount)
      write(33,3857)UNAME(feetcount)
 3857 format(77('-')/
     &'*** River quality dataset specified for Abstraction Point '/
     &'*** Feature type 18 - the value should be zero      '/
     &'*** The reference to the dataset was removed for this run '/
     &'*** The name of the feature is: ',A33/77('-'))
      JQ(feetcount) = 0
      endif
*     check data on river flow -------------------------------------------------
      if ( IF .le. 0 .or. IF .gt. NF ) then
      if ( nobigout .le. 0 ) write(01,4363)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,4363)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (18)
      write(09,4363)UNAME(feetcount)
      write(33,4363)UNAME(feetcount)
 4363 format(77('-')/
     &'*** No valid flow Data-Set Code for Abstraction ...'/
     &'*** Feature type 18 '/
     &'*** The name of the feature is: ',A33/77('-'))
      endif
      endif ! abstraction - the negative discharge (18)

*     abstraction - the negative discharge (19) --------------------------------
      if ( IT .eq. 19 ) then
*     check data on abstraction (river flow data) ------------------------------
*     flows are mean and standdeviation not and 95-percentile low flow ---------
      if ( IF .le. 0 .or. IF .gt. NF ) then
      write(01,4867)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,4867)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (19)
      write(09,4867)UNAME(feetcount)
      write(33,4867)UNAME(feetcount)
 4867 format(77('-')/
     &'*** No valid flow Data-Set Code for Abstraction ...'/
     &'*** Feature type 19 '/
     &'*** The name of the feature is: ',A33/77('-'))
      endif
      endif ! abstraction - the negative discharge (19)

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     weir or quality adjustment point (8)
      if ( IT .eq. 8 ) then
      if ( DIST(feetcount) .gt. 0.0001 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (8)
      write(01,1407)
      write(09,1407)
      write(33,1407)
      if ( iscreen .lt. 3 ) write( *,1407)
 1407 format(77('-')/
     &'*** Illegal data for a Weir ...                            '/
     &'*** A weir must be located at the head of a Reach ...      '/
     &'*** Calculation stoppped ....                              '/
     &77('-'))
      call stop
      endif

      dist(feetcount) = 0.0001

*     sort out the quality data ------------------------------------------------
*     SIMCAT expects a quality dataset in JQCAL --------------------------------
      if ( JQCAL (feetcount) .ne. 0 ) then

*     dataset found in expected place. Eliminate other place -------------------
      JQ(feetcount) = JQCAL(feetcount)

*     ensure dataset code is negative so as to suppress extrapolation ----------
      if (JQCAL(feetcount) .gt. 0) then
      JQCAL(feetcount) = - JQCAL(feetcount)
      endif
      else
      JQCAL(feetcount) = JQ(feetcount)
      if (JQCAL(feetcount) .eq. 0 ) then
      write(01,1401)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (8)
      if ( iscreen .lt. 3 ) write( *,1401)UNAME(feetcount)
      write(09,1401)UNAME(feetcount)
      write(33,1401)UNAME(feetcount)
 1401 format(77('-')/
     &'*** No river quality data for Weir ...                  '/
     &'*** Name of Feature: ',A33,'  '/
     &'*** The Feature has been deleted .....                  '/
     &77('-'))
      goto 39
      else

*     eliminate the other place ------------------------------------------------
      JQ(feetcount) = 0

*     ensure the dataset code is negative for a weir ---------------------------
      if (JQCAL(feetcount).gt.0) then
          JQCAL(feetcount) = -JQCAL(feetcount)
      endif
      endif
      endif

      if ( IF .ne. 0 ) then
      write(01,5407)UNAME(feetcount)
      write(09,5407)UNAME(feetcount)
      write(33,5407)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (8)
      if ( iscreen .lt. 3 ) write( *,5407)UNAME(feetcount)
 5407 format(77('-')/
     &'*** River Flow Data-Set specified for Weir ....         '/
     &'*** Reference to the dataset has been removed ....     '/
     &'*** Name of Feature: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( JFCAL(feetcount) .ne. 0 ) then
      write(01,5437)UNAME(feetcount)
      write(09,5437)UNAME(feetcount)
      write(33,5437)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (8)
      if ( iscreen .lt. 3 ) write( *,5437)UNAME(feetcount)
 5437       format(77('-')/
     &'*** River Flow Gap Filling Data-Set specified for Weir  '/
     &'*** Reference to the dataset has been removed ....     '/
     &'*** Name of Feature: ',A33/77('-'))
            JFCAL(feetcount) = 0
      endif
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     flow gauge (4) -----------------------------------------------------------
      if ( IT .eq. 4 ) then
      if ( IQ .gt. 0) then
      write(01,7850)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,7850)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (4)
      write(09,7850)UNAME(feetcount)
      write(33,7850)UNAME(feetcount)
 7850 format(77('-')/
     &'*** River quality dataset specified for River Flow      '/
     &'*** Gauge. Reference to Data-Set removed ....           '/
     &'*** Name of Feature: ',A33/77('-'))
       endif

      if ( IF .le. 0 .or. IF .gt. NF ) then
      write(01,4099)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,4099)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (4)
      write(09,4099)UNAME(feetcount)
      write(33,4099)UNAME(feetcount)
 4099 format(77('-')/
     &'*** No valid river flow Data-Set Code for Flow Gauge ... '/
     &'*** Name of Feature: ',A33/77('-'))
      else

      if ( F(IF,1) .lt. 1.0E-10 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7894)UNAME(feetcount)
 7894 format(
     &'*** Zero flow specified for a flow gauge',11x,'...',7x,
     &'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) write(01,7814)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (4)
      write(09,7814)UNAME(feetcount)
      write(33,7814)UNAME(feetcount)
 7814 format(77('-')/
     &'*** Zero flow specified for a flow gauge ...'/
     &'*** Name of Feature: ',A37/77('-'))
      endif


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     insert a flow data set for gap filling -----------------------------------
      if ( iforce gap fill .eq. 1 ) then
      if ( jfcal(feetcount) .eq. 0 ) then
      Iflowset = jf(feetcount)
      if ( F(Iflowset,1) .gt. 1.0E-10 ) then
      jfcal(feetcount) = jf(feetcount)
      endif ! if ( F(Iflowset,1) .gt. 1.0E-10 )
      endif ! if ( jfcal(feetcount) .eq. 0 )
      endif ! if ( iforce gap fill .eq. 1 )
*     insert a flow data set for gap filling -----------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      endif
      endif

*     river regulation point (9) -----------------------------------------------
      if ( IT .eq. 9 ) then !=======================  river regulation point (9)
      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      write(01,6212)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (9)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,6282)UNAME(feetcount)
 6282 format('*** No valid quality data for Regulation ',
     &'Point ... ',a33)
      call set screen text colour
      endif
      write(09,6212)UNAME(feetcount)
      write(33,6212)UNAME(feetcount)
 6212 format(77('-')/
     &'*** No valid river quality Data-Set Code for Regulation ',
     &'Point ... '/
     &'*** River quality is assumed unaffected by regulation ... '/
     &'*** Name of Feature: ',A33/77('-'))
      else
      do iip = 1, NDET
      if ( qtype (iip) .ne. 4 ) then
      if ( PDRC (IQ, iip) .ge. 4 ) then
      if ( nobigout .le. 0 ) write(01,6252)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,6252)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (9)
      write(09,6252)UNAME(feetcount)
      write(33,6252)UNAME(feetcount)
 6252 format(77('-')/
     &'*** Unavailable option for Data-Set Code for Regulation  '/
     &'*** Calculated mean quality will apply ......            '/
     &'*** Name of Feature: ',A33,'   '/
     &'*** The options for a non-parametric or a power ',
     &'distribution are not'/
     &'*** available for the quality of regulation water ...    '/
     &'*** River quality is assumed unaffected by regulation ... '/
     &77('-'))
      endif
      endif
      enddo
      
      if ( QNUM(IQ,1) .lt. 0 ) then ! ===========================================
      if ( iscreen .lt. 3 ) write( *,7853)UNAME(feetcount)
      if ( nobigout .le. 0 ) write(01,7853)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (9)
      write(09,7853)UNAME(feetcount)
      write(33,7853)UNAME(feetcount)
 7853 format(77('-')/
     &'*** No quality data specified for a Regulation Point ...'/
     &'*** Name of Feature: ',A33/77('-'))
      endif ! if ( QNUM(IQ,1) .lt. 0 ) then ====================================
      endif

      if ( IF .le. 0 .or. IF .gt. NF ) then ! ==================================
      if ( nobigout .le. 0 ) write(01,6213)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,6213)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (9)
      write(09,6213)UNAME(feetcount)
      write(33,6213)UNAME(feetcount)
 6213 format(77('-')/
     &'*** No valid river flow Data-Set Code for Regulation ... '/
     &'*** Point .....  No flow will be added ...             '/
     &'*** Name of Feature: ',A33/77('-'))
      endif ! if ( IF .le. 0 .or. IF .gt. NF ) then ============================

      if (F(IF,2) .lt. 1.0E-10) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      suppress19 = suppress19 + 1 ! unneeded data ignored (9)
      write( *,7876)UNAME(feetcount)
 7876 format(
     &'*** Zero flow specified for Regulation Point ',6x,'...',7x,
     &'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) write(01,7856)UNAME(feetcount)
      write(09,7856)UNAME(feetcount)
      write(33,7856)UNAME(feetcount)
 7856 format(/77('-')/
     &'*** Zero flow specified for a Regulation Point ...'/
     &'*** No flow will be added ... '/
     &'*** Name of Feature: ',A37/77('-'))
      endif
      endif ! if ( IT .eq. 9 ) then ! ==============  river regulation point (9)


*     ==========================================================================
*     Upstream Boundary (10)

*     ############### check need to include bifurcations #######################
      if ( IT .eq. 10 .or. IT .eq. 45 ) then
      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      write(01,8250)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,8250)UNAME(feetcount)
      write(09,8250)UNAME(feetcount)
      write(33,8250)UNAME(feetcount)
 8250 format(77('-')/
     &'*** An Upstream Boundary has no river quality dataset ... '/
     &'*** Name of Feature: ',A33/
     &'*** Calculation stopped ... '/77('-'))
      call stop
      endif

*     if ( QNUM(IQ,1)+QNUM(IQ,2)+QNUM(IQ,3) .lt. 0 ) then
      if ( QNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7864)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (10)
      write(01,7864)UNAME(feetcount)
      write(09,7864)UNAME(feetcount)
      write(33,7864)UNAME(feetcount)
 7864 format(77('-')/
     &'*** No quality data specified for an Upstream Boundary ...'/
     &'*** Name of Feature: ',A33/77('-'))
      endif

      if ( IF .eq. 0 ) then
      write(01,8255)UNAME(feetcount) 
      if ( iscreen .lt. 3 ) write( *,8255)UNAME(feetcount)
      write(09,8255)UNAME(feetcount)
      write(33,8255)UNAME(feetcount)
 8255 format(77('-')/
     &'*** Upstream Boundary has no river flow dataset ... '/
     &'*** Name of Feature: ',A33/
     &'*** Calculation stopped ... '/77('-'))
      call stop
      endif

*     the feature must be located at the head of the Reach

      if (DIST(feetcount) .gt. 1.0E-8) then
      write(01,7720)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored (10)
      if ( iscreen .lt. 3 ) write( *,7720)UNAME(feetcount)
      write(09,7720)UNAME(feetcount)
      write(33,7720)UNAME(feetcount)
 7720 format(77('-')/
     &'*** Non-zero distance for Upstream Boundary ... '/
     &'*** This has been set to zero ... '/
     &'*** Name of Feature: ',A33/77('-'))
      DIST(feetcount) = 0.0
      endif
      endif



*     Bifurcation (11) --------------------------------- old type of bifurcation
      if ( IT .eq. 11 ) then
      if ( IF .le.  0 ) then ! there is no specified reach data ----------------
      write(01,2406)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,2406)UNAME(feetcount)
      write(09,2406)UNAME(feetcount)
      write(33,2406)UNAME(feetcount)
 2406 format(77('-')/
     &'*** Error in data for Bifurcation ... '/
     &'*** No valid source given for flow and quality data ... '/
     &'*** Reach Code is needed for fifth item in data record ... '/
     &'*** Name of Feature: ',A33/77('-'))
      call stop
      endif ! if ( IF .le.  0 ) ------------------------------------------------

*     add to the counter of the bifurcations -----------------------------------
      IFURC = IFURC+1
      if ( IFURC .gt. NB ) then
      write(01,2427)
      write(09,2427)
      write(33,2427)
      if ( iscreen .lt. 3 ) write( *,2427)
 2427       format(120('-')/
     &'*** Too many bifurcations ... '/
     &'*** Increase the value of parameter NB ... '/
     &'*** And re-compile SIMCAT ... '/120('-'))
      call stop
      endif

      BIDENT(IFURC) = feetcount

      if ( DIST(feetcount) .lt. 1.0E-08 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (11)
      write(01,2407)JF(feetcount)
      write(09,2407)JF(feetcount)
      write(33,2407)JF(feetcount)
      if ( iscreen .lt. 3 ) write( *,2407)JF(feetcount)
 2407 format(120('-')/
     &'*** Incomplete data for bifurcation ... '/
     &'*** Zero entry in distance field ... '/
     &'*** The distance field is used to give the proportion '/
     &'*** of the flow of Reach number',I6/120('-'))
      endif

      if ( DIST(feetcount) .gt. 100.0 ) then
      write(01,2107)JF(feetcount)
      write(09,2107)JF(feetcount)
      write(33,2107)JF(feetcount)
      if ( iscreen .lt. 3 ) write( *,2107)JF(feetcount)
 2107 format(120('-')/
     &'*** Incorrect data for bifurcation ... '/
     &'*** Entry in distance field exceeds 100 ... '/
     &'*** The distance field is used to give the proportion   '/
     &'*** of the flow of Reach number',I6/
     &'*** Calculation stopped ... '/120('-'))
      call stop
      endif

*     store proportion of flow to be split -------------------------------------
      BSPLIT(IFURC) = DIST(feetcount)
      DIST(feetcount) = 0.0
      endif
*     ==========================================================================


*     ==========================================================================
*     bifurcation (20,21,22,23) ! check reach code entered instead of river flow 
      if ( IT .eq. 20 .or. IT .eq. 21 .or. 
     &     IT .eq. 22 .or. IT .eq. 23 ) then
      if ( IF .le. 0 ) then
      suppress19 = suppress19 + 1 ! unneeded data ignored (20 or 21)
      write(01,5406)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,5406)UNAME(feetcount)
      write(09,5406)UNAME(feetcount)
      write(33,5406)UNAME(feetcount)
 5406 format(77('-')/
     &'*** Error in data for Bifurcation (Feature Type 20 to 23 ... '/
     &'*** No valid source given for flow data ...'/
     &'*** A Reach Code is needed at the fifth item in data record ...'/
     &'*** (A Flow Record is needed at the sixth item ...)'/
     &'*** Name of Feature: ',A33/77('-'))
      call stop
      endif

*     add to counter -----------------------------------------------------------
      IFURC = IFURC+1
      if ( IFURC .gt. NB ) then
      write(01,5427)
      write(09,5427)
      write(33,5427)
      write( *,5427)
 5427 format(77('-')/
     &'*** Too many bifurcations....                           '/
     &'*** Increase value of parameter NB in COM.FOR...        '/
     &'*** And re-compile SIMCAT .....                         '/
     &77('-'))
      call stop
      endif

      BIDENT(IFURC) = feetcount
      if ( DIST(feetcount) .gt. 1.0E-08 ) then
      write(01,5997)JF(feetcount)
      write(09,5997)JF(feetcount)
      write(33,5997)JF(feetcount)
      write( *,5997)JF(feetcount)
 5997 format(77('-')/
     &'*** Error in data for Bifurcation (Feature Types 20 to 23 ...'/
     &'*** Distance field must be zero ...'/
     &'*** Feature must be at the head of the reach ...'/
     &'*** Flow at head is the flow at end Reach number',I6/
     &77('-'))
      call stop
      endif

*     store proportion of flow to be split ---------------------------
      BSPLIT(IFURC) = 1.0
      DIST(feetcount) = 0.0
      endif
*     ====================================================================


*     ====================================================================
*     Bifurcation (20,21,22,23) - check for the diverted flow
      if ( IT .eq. 20 .or. IT .eq. 21 .or. 
     &     IT .eq. 22 .or. IT .eq. 23 ) then
      if ( IQ .le. 0 ) then
      write(01,7406)UNAME(feetcount)
      write( *,7406)UNAME(feetcount)
      write(09,7406)UNAME(feetcount)
      write(33,7406)UNAME(feetcount)
 7406 format(77('-')/
     &'*** Error in data for Bifurcation (Feature Type 20 to 23)'/
     &'*** No valid source given for flow data ...'/
     &'*** Reach Code needed at fifth item in data record ...  '/
     &'*** A Flow Record is needed at the sixth item ...'/
     &'*** Name of Feature: ',A33/77('-'))
      call stop
      endif

*     add to counter

      IFURC = IFURC+1
      if ( IFURC .gt. NB ) then
      write(01,7427)
      write(09,7427)
      write(33,7427)
      write( *,7427)
 7427 format(77('-')/
     &'*** Too many bifurcations....                           '/
     &'*** Increase value of parameter NB in COM.FOR...        '/
     &'*** And re-compile SIMCAT .....                         '/
     &77('-'))
      call stop
      endif

      BIDENT(IFURC) = feetcount

      if ( DIST(feetcount) .gt. 1.0E-08 ) then
      write(01,7997)JF(feetcount)
      write(09,7997)JF(feetcount)
      write(33,7997)JF(feetcount)
      write( *,7997)JF(feetcount)
 7997 format(77('-')/
     &'*** Distance error in data for Bifurcation ',
     &'(Feature Type 22 or 23) ...'/
     &'*** Distance field must be zero ...'/
     &'*** Feature must be at the head of the reach ...'/
     &'*** Flow at head is the flow at end Reach number',I6/
     &77('-'))
      call stop
      endif

*     store proportion of flow to be split
      BSPLIT(IFURC) = 1.0
      DIST(feetcount) = 0.0
      endif

     
*     intermittent discharges of sewage (12) -----------------------------------
      if ( IT .eq. 12 ) then

*     check for illegal dataset code
      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      write(01,4855)UNAME(feetcount)
      suppress17 = suppress17 + 1 ! deleted feature (12)
      write(09,4855)UNAME(feetcount)
      write(33,4855)UNAME(feetcount)
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,4855)UNAME(feetcount)
 4855 format(77('-')/
     &'*** No Data-Set specified for intermittent sewage discharge ...'/
     &'*** Feature deleted ... '/
     &'*** The name of the feature is: ',A33/77('-'))
      call set screen text colour 
      endif
      goto 39
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,4862)UNAME(feetcount)
      write(01,4862)UNAME(feetcount)
      suppress17 = suppress17 + 1 ! deleted feature (17)
      write(09,4862)UNAME(feetcount)
      write(33,4862)UNAME(feetcount)
 4862 format(77('-')/
     &'*** No quality data specified for an intermittent discharge ...'/
     &'*** The name of the feature is: ',A33/
     &'*** Feature deleted ... '/77('-'))
      goto 39
      endif

      if ( PDEF(IQ) .ne. 6 .and. FE(IQ,1) .lt. 1.0E-10) then
*     if ( iscreen .lt. 3 ) write( *,4516)UNAME(feetcount)
      !write(09,4516)UNAME(feetcount)
      write(33,4516)UNAME(feetcount)
 4516 format(77('-')/
     &'*** Zero flow has been specified for an intermittent discharge'/
     &'*** The name of the feature is: ',A33/77('-'))
      endif

*     check that there are no river flow data ...

      if ( IF .ne. 0 ) then
      write(01,3856)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,4856)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored
      write(09,4856)UNAME(feetcount)
      write(33,4856)UNAME(feetcount)
 4856 format(77('-')/
     &'*** River Flow Data-Set specified for intermittent discharge  '/
     &'*** Reference to Data-Set removed....                   '/
     &'*** The name of the feature is: ',A33/77('-'))
      JF(feetcount) = 0
      endif
      endif !  intermittent discharges of sewage (12) --------------------------
*     ==========================================================================


*     ==========================================================================
*     switch on Diffuse Pollution (13), (25), (27) ... etc =====================

      if ( IT .eq. 13 .or. IT .eq. 25 .or. IT .eq. 27 .or. ! +++++++++++++++++++
     &     IT .eq. 29 .or. IT .eq. 31 .or. IT .eq. 33 .or.
     &     IT .eq. 35 .or. IT .eq. 37 .or. IT .eq. 40 .or.
     &     IT .eq. 46 .or. IT .eq. 48 .or. IT .eq. 50 .or.
     &     IT .eq. 52 .or. IT .eq. 54 .or. IT .eq. 56 .or.
     &     IT .eq. 58 ) then
      if ( IQ .le. 0 .or. IQ .gt. NV ) then ! ----------------------------------
      write(01,4263)UNAME(feetcount)
      call change colour of text (34) ! dull yellow
      if ( iscreen .lt. 3 ) then
      write( *,4163)UNAME(feetcount)
 4163 format('*** No valid quality data for Diffuse ',
     &'Pollution (river-type) ... ',A33)
      call set screen text colour
      endif
      suppress19 = suppress19 + 1 ! unneeded data ignored
      write(09,4263)UNAME(feetcount)
      write(33,4263)UNAME(feetcount)
 4263 format(77('-')/
     &'*** No valid river quality Data-Set Code for Diffuse ',
     &'Pollution (river type)'/'*** Feature deleted ... '/
     &'*** Name of Feature: ',A33/77('-'))
      goto 39
      endif ! if ( IQ .le. 0 .or. IQ .gt. NV ) ---------------------------------

      if ( QNUM(IQ,1) .lt. 0 ) then ! ------------------------ number of samples
      write( *,7810)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored
      write(01,7810)UNAME(feetcount)
      write(09,7810)UNAME(feetcount)
      write(33,7810)UNAME(feetcount)
 7810 format(77('-')/
     &'*** No quality data specified for diffuse pollution ...'/
     &'*** Name of Feature: ',A33/77('-'))
      endif ! ------------------------------------------------------------------

      if ( IF .le. 0 .or. IF .gt. NF ) then ! check for flow data ==============
      if ( add diffconc (feetcount) .eq. 0 ) then ! ----------------------------
      write(01,4296)UNAME(feetcount)
      call change colour of text (10) ! green
      if ( iscreen .lt. 3 ) write( *,4896)UNAME(feetcount)
 4896 format('*** No valid data for Diffuse ',
     &'Pollution (river-type) ... ',A33)
      call set screen text colour
      write(09,4296)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored
      write(33,4296)UNAME(feetcount)
 4296 format(77('-')/
     &'*** No valid river flow Data-Set Code for Diffuse ',
     &'Pollution (river-type). Feature deleted ... '/
     &'*** Name of Feature: ',A33/77('-'))
      goto 39
      endif ! if ( add diffconc (feetcount) .eq. 0 ) then ----------------------
      endif ! if ( IF .le. 0 .or. IF .gt. NF ) =================================

      if ( F(IF,1) .lt. 1.0E-10 ) then ! =======================================
      if ( add diffconc (feetcount) .eq. 0 ) then ! ----------------------------
      ihalt diffuse river = ihalt diffuse river + 1
      if ( ihalt diffuse river .le. 3 ) then
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      call change colour of text (10) ! green
      write( *,7256)UNAME(feetcount)
 7256 format(77('-')/
     &'*** Zero flow specified for Diffuse Pollution ... '/
     &'*** Name of Feature: ',A37/77('-'))
      call set screen text colour
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------
      if ( suppress19 .lt. 10 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) 
     &write(01,7156)UNAME(feetcount)
      write(09,7156)UNAME(feetcount)
      write(33,7156)UNAME(feetcount)
 7156 format(/77('-')/
     &'*** Zero flow specified for Diffuse Pollution ... '/
     &'*** Name of Feature: ',A37/77('-'))
      endif
      if ( suppress19 .eq. 9 ) then
      if ( nobigout .le. 0  .and. ical13 .eq. 0) 
     &write(01,7156)UNAME(feetcount)
      write(09,7756)UNAME(feetcount)
      write(33,7756)UNAME(feetcount)
 7756 format('*** And elsewhere ... '/77('-'))
      endif
      suppress19 = suppress19 + 1 ! unneeded data ignored

      endif ! if ( ihalt diffuse river .le. 3 )
      endif ! if ( add diffconc (feetcount) .eq. 0 ) ---------------------------
      endif ! if ( F(IF,1) .lt. 1.0E-10 ) ======================================
      endif ! if ( IT .eq. 13 .or. IT .eq. 25 --- diffuse pollution ++++++++++++

*     switch-off Diffuse Pollution (14) ----------------------------------------
      if ( IT .eq. 14 .or. IT .eq. 26 .or. IT .eq. 28 .or.
     &     IT .eq. 30 .or. IT .eq. 32 .or. IT .eq. 34 .or.
     &     IT .eq. 36 .or. IT .eq. 38 .or. IT .eq. 47 .or.
     &     IT .eq. 49 .or. IT .eq. 51 .or. Tt .eq. 53 .or.
     &     IT .eq. 55 .or. IT .eq. 57 .or. Tt .eq. 59 ) then
      if ( IQ .ne. 0 ) then
*     write(01,5856)UNAME(feetcount)
*     if ( iscreen .lt. 3 ) write( *,5856)UNAME(feetcount)
*     write(09,5856)UNAME(feetcount)
      suppress23 = suppress23 + 1 ! unneeded data ignored
      if ( suppress23 .lt. 8 )then
      write(33,5856)UNAME(feetcount)
 5856 format(77('-')/
     &'*** Quality data-set code specified for an End of',
     &' Diffuse Pollution ...'/'*** Code replaced with zero ... ',
     &'Name of Feature: ',A33/77('-'))
      endif
      JQ(feetcount) = 0
      endif

      if ( IF .ne. 0 ) then
*     write(01,5852)UNAME(feetcount)
*     if ( iscreen .lt. 3 ) write( *,5852)UNAME(feetcount)
*     write(09,5852)UNAME(feetcount)
      suppress23 = suppress23 + 1 ! unneeded data ignored
      if ( suppress23 .lt. 10 ) then
      write(33,5852)UNAME(feetcount)
 5852 format(77('-')/
     &'*** Flow data-set code specified for an End of Diffuse ',
     &'Pollution ...'/'*** Code replaced with zero ... '/
     &'*** Name of Feature: ',A33/77('-'))
      endif
      JF(feetcount) = 0
      endif
      
*     eliminate unnecessary features -------------------------------------------
      dddd = abs ( dist(feetcount) - rlength (Jreach(feetcount)) ) 
      if ( dddd .lt. 0.00001 ) then
      JT(feetcount) = 0
      JREACH(feetcount) = 0
      goto 39
      endif
      
      endif

*     switch on Diffuse Pollution (15 ... 42) ----------------------------------
      if ( IT .eq. 15 .or. IT. eq. 42 ) then
      call turn on diffuse pollution 15
      endif
      goto 38

*     switch off Diffuse Pollution (16 ... 43) ---------------------------------
      if ( IT .eq. 16 .or. IT. eq. 43 ) then
      call turn off diffuse pollution 16

*     eliminate unnecessary features -------------------------------------------
      dddd = abs ( dist(feetcount) - rlength (Jreach(feetcount)) ) 
      if ( dddd .lt. 0.00001 ) then
      JT(feetcount) = 0
      JREACH(feetcount) = 0
      goto 39
      endif
     
      endif
      goto 38

 8321 continue
      write( *,8322)feetcount,line
      write(01,8322)feetcount,line
      write(09,8322)feetcount,line
      write(33,8322)feetcount,line
 8322 format(77('-')/
     &'*** Error in data-sets for Features ... Feature Number:',i5/
     &'*** The data-file has been assembled incorrectly ... '/
     &'*** The text of the line producing this message is:'/
     &a110/77('-'))
      call stop
      end


*     Routine Monitoring Points (1) --------------------------------------------
      subroutine routine monitoring point
      include 'COMMON DATA.FOR'

      if ( IF .ne. 0 ) then
      write(01,7852)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,7852)UNAME(feetcount)
      suppress19 = suppress19 + 1 ! unneeded data ignored
      if ( suppress19 .lt. 11 ) then ! -------------------------------------------
      write(09,7852)UNAME(feetcount)
      write(33,7852)UNAME(feetcount)
 7852 format(77('-')/
     &'*** Flow data-set code specified for a River Quality   '/
     &'*** Monitoring Point. Zero expected here ...           '/
     &'*** Reference to the dataset has been removed ...      '/
     &'*** Name of Feature: ',A33/77('-'))
      endif ! if ( suppress19 .lt. 11 ) ------------------------------------------
      endif


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( JQCAL(feetcount) .gt. 0 .and. JQ(feetcount) .gt. 0 ) then ! ---------
      if ( JQCAL(feetcount) .ne. JQ(feetcount) ) then ! ------------------------
      write(01,7152)UNAME(feetcount),JQCAL(feetcount),JQ(feetcount)
      suppress8 = suppress8 + 1
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      

      
      if ( ical .eq. 3 .or. ical .eq. 4 ) then
      if ( suppress8 .lt. 4 ) then
      call change colour of text (38) ! dull green
      write( *,7854)UNAME(feetcount)
 7854 format('*** Quality data are not the set used to gap fill  ...',
     &'       Feature: ',A34,1x,25('.'))
      else
      if ( suppress8 .eq. 4 ) then
      write( *,7853)UNAME(feetcount)
 7853 format('*** Quality data are not the set used to gap fill  ...',
     &'       Feature: ',a34,1x,'and elsewhere ...........')
      endif     
      call set screen text colour
      endif
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      write(09,7152)UNAME(feetcount),JQCAL(feetcount),JQ(feetcount)
      write(33,7152)UNAME(feetcount),JQCAL(feetcount),JQ(feetcount)
 7152 format(77('-')/
     &'*** The quality data-set is not the same as the ',
     &'gap filling data-set ... '/
     &'*** Are you sure this is what you want? '/
     &'*** Name of Feature: ',A33,i6,' and',i6/77('-'))
      endif ! if ( JQCAL(feetcount) .ne. JQ(feetcount) ) -----------------------
      endif ! if ( JQCAL(feetcount) .gt. 0 .and. JQ(feetcount) .gt. 0 ) --------

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     Insert a quality data set for gap filling ------------------
*     if ( jqcal(feetcount) .eq. 0 ) then
*     jqcal(feetcount) = jq(feetcount)
*     endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

  
      return
      end







*     switch-on Diffuse Pollution (15) -----------------------------------------
      subroutine turn on diffuse pollution 15
      include 'COMMON DATA.FOR'

      if ( IQ .le. 0 .or. IQ .gt. NE ) then
      write(01,4214)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,4214)UNAME(feetcount)
      write(09,4214)UNAME(feetcount)
      write(33,4214)UNAME(feetcount)
 4214 format(77('-')/
     &'*** No valid discharge quality Data-Set Code for       '/
     &'*** Diffuse Pollution. Feature deleted ...'/
     &'*** Name of Feature: ',A33/77('-'))
      return
      endif

      if ( PNUM(IQ,1) .lt. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7256)UNAME(feetcount)
      write(01,7256)UNAME(feetcount)
      write(09,7256)UNAME(feetcount)
      write(33,7256)UNAME(feetcount)
 7256 format(77('-')/
     &'*** No quality data specified for Diffuse Pollution ...'/
     &'*** Name of Feature: ',A33/77('-'))
      endif

      if ( IF .ne. 0 ) then
      write(01,3852)UNAME(feetcount)
      write(09,3852)UNAME(feetcount)
      write(33,3852)UNAME(feetcount)
      if ( iscreen .lt. 3 ) write( *,3852)UNAME(feetcount)
 3852 format(77('-')/
     &'*** River Flow Data-Set specified for Diffuse ',
     &'Pollution (Effluent type - 15) ...'/
     &'*** Reference to Data-Set removed ...'/
     &'*** Name of Feature: ',A33/77('-'))
      JF(feetcount) = 0
      endif

      if ( FE(IQ,1) .lt. 1.0E-10 ) then
      ihalt diff disch = ihalt diff disch + 1
      if ( ihalt diff disch .le. 2 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (10) ! green
      write( *,7596)UNAME(feetcount)
 7596 format('*** Zero flow given to Diffuse ',
     &'Pollution    ... ',A33)
      call set screen text colour
      endif
      write(01,7536)UNAME(feetcount)
      write(09,7536)UNAME(feetcount)
      write(33,7536)UNAME(feetcount)
      endif
 7536 format(77('-')/
     &'*** Zero discharge flow specified for Diffuse ',
     &'Pollution (Effluent type) ... '/
     &'*** Name of Feature: ',A33/77('-'))
      endif

      return
      end



*     switch-off Diffuse Pollution (16) ----------------------------------------
      subroutine turn off diffuse pollution 16
      include 'COMMON DATA.FOR'

      if ( IQ .ne. 0 ) then
*     write(01,5854)UNAME(feetcount)
*     if ( iscreen .lt. 3 ) write( *,5854)UNAME(feetcount)
*     write(09,5854)UNAME(feetcount)
      suppress23 = suppress23 + 1 ! unneeded data ignored
      if ( suppress23 .lt. 8 )then
      write(33,5854)UNAME(feetcount)
 5854 format(77('-')/
     &'*** Quality data-set code specified for an End of ',
     &' Diffuse Pollution ...'/'*** Code replaced with zero ... ',
     &'Name of Feature: ',A33/77('-'))
      endif
      JQ(feetcount) = 0
      endif

      if ( IF .ne. 0 ) then
*     write(01,5853)UNAME(feetcount)
*     if ( iscreen .lt. 3 ) write( *,5853)UNAME(feetcount)
*     write(09,5853)UNAME(feetcount)
      suppress23 = suppress23 + 1 ! unneeded data ignored
      if ( suppress23 .lt. 10 ) then
      write(33,5853)UNAME(feetcount)
 5853 format(77('-')/
     &'*** Flow data-set code specified for an End of Diffuse ',
     &'Pollution ...'/'*** Code replaced with zero ... '/
     &'*** Name of Feature: ',A33/77('-'))
      endif
      JF(feetcount) = 0
      endif

      return
      end