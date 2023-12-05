*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File: process the selected reach.for ... 1529 lines ----------------------
*     --------------------------------------------------------------------------
*     This file processes each Reach in turn (that make up the catchment model -
*     --------------------------------------------------------------------------
*     This file contains 14 subroutines ----------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... reach
*     ...... mix the reaches
*     ...... set up the data for temperature 
*     ...... bias in normal temperature (RCM,RCS)
*     ...... set up the data for suspended solids 
*     ...... bias in log normal suspended solids (RCM,RCS)
*     ...... set loads and proportions to zero
*     ...... initialise the variables for this reach
*     ...... initialise the variables for gap filling
*     ...... check for features out of sequence
*     ...... check the position of the feature (INEXT)
*     ...... write diffuse loads of pollution
*     ...... set river quality targets for plotting
*     ...... accumulate lengths
*     --------------------------------------------------------------------------
      
      subroutine reach
      include 'COMMON DATA.FOR'

*     initialise the variables and write headings for this reach ---------------
      call initialise the variables for this reach
      Length of main river = ADIST(IREACH)
*     call initialise data for mass balance ! reach
      call initialise the variables for gap filling
      call set up the data for temperature 
      call set up the data for suspended solids
      
      iplace = 4 ! ... upstream of discharge

     
*     00000000000000000000000000000000000000000000000000000000000000000000000000
*     process a Reach which has no Features 000000000000000000000000000000000000
      no features = 0
      if ( JREACH (feeture+1) .ne. IREACH ) then ! the Reach has no Features 000
      no features = 1

      call write headings at the start of the reach ! with no features 000000000
      do j = 1, MP10 ! =========================================================
      if ( QTYPE(J) .ne. 4 ) then ! ============================================
      write(230+j,305)RNAME(IREACH), ! --------------- start of reach --- Gi.CSV
     &ireach,0,0, !  ! ------ start of reach - graph plotting ----------- Gi.CSV
     &RLENGTH(IREACH),adist(IREACH), ! ---------------------------------- Gi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop) ! --------------------------------- Gi.CSV
      write(240+j,305)RNAME(IREACH), ! --------------- start of reach --- Wi.CSV
     &ireach,0,0, !  ! ------ start of reach - word reports ------------- Wi.CSV
     &RLENGTH(IREACH),adist(IREACH), ! ---------------------------------- Wi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Wi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Wi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Wi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Wi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop) ! --------------------------------- Wi.CSV
      endif ! if ( QTYPE(J) .ne. 4 ) ===========================================
      enddo ! do j = 1, MP10 ===================================================
      write(33,7361)RNAME(IREACH)
      !write(48,7361)RNAME(IREACH)
 7361 format('There are no Features in the Reach: ',A16)
*     set up the distance for calculations of natural purification etc ---------
      DISTP = RLENGTH(IREACH) ! set distance to next feature as reach length ---
      call accumulate lengths ! for reach with no features ---------------------
      if ( ical13 .eq. 0 ) then
      call write the correlation coefficients ! Reach with no Features 000000000
      call classify (KFEET,1) ! head of Reach with no Features 00000000000000000
      call assess compliance with river targets (KFEET,1) ! head of reach 000000
      endif

      call add diffuse sources and natural purification (0) ! reach: no features
      
      call load calculation ! 0000000000000000000000000000000 reach: no features

      call process the end of the reach ! 00000000000000000000000000000000000000

      DISTP = 0.0 ! 000000000000000000000000000000000000000000000000000000000000
      if ( ical13 .eq. 0 ) then ! 0000000000000000000000000000000000000000000000
      call write the correlation coefficients ! Reach with no Features 000000000
      call classify (KFEET,3) ! end of Reach with no Features 000000000000000000
      call assess compliance with river targets (KFEET,3) ! end of reach 0000000
      endif ! if ( ical13 .eq. 0 ) 000000000000000000000000000000000000000000000
      return ! 00000000000000000000000000000000000000000000000000000000000000000
      endif ! finished a Reach which has no Features 000000000000000000000000000
*     00000000000000000000000000000000000000000000000000000000000000000000000000

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     process a reach which has Features ---------------------------------------
      call set loads and proportions to zero
*     ==========================================================================
*     process the river boundary -----------------------------------------------
*     set the sequence number of the next Feature ------------------------------
      INEXT = feeture + 1
      ityp = JT(INEXT)
      feeture = inext
      bifurcr = 0
      ibifurcr2 = 0

      if ( ityp .eq. 10 ) call river boundary (INEXT)

      
*     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb bifurcation of type 11
      if ( ityp .eq. 11 ) then ! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      
      call bifurcation 11 (INEXT) ! ========= using the specified ratio of flows
      bifurcr = -1 ! ---------------------------------- ! bifurcation of type 11
      ibifurcr2 = -JF(INEXT) ! ------------------------ ! bifurcation of type 11

*     check whether arrays can be cleared because the Reach is not required ----
*     for mixing with downstream reaches ---------------------------------------
      jj2 = -ibifurcr2 ! the reach supplying the bifurcations ------------------
      icount = 0
      do JJ = feeture, MU ! check all reaches for the downstream features ------
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. jj2 ) then
      icount = icount + 1
      endif
      enddo ! do JJ = feeture + 1, MU ------------------------------------------
      if ( icount .eq. 1 ) then
      do istor = 1, KR
      if ( JSTOR(istor) .eq. jj2 ) then
      JSTOR(istor) = 0 ! the reach data for J are no longer needed for mixing -
      IRHOLD(jj2) = 0 ! set number used to store data for reach IFF set to zero 
      goto 5187
      endif
      enddo
      endif
 5187 continue
      
      endif ! if ( ityp .eq. 11 ) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

      
      
      if ( ityp .eq. 20 ) then ! ========================= using river flow data
      bifurcr = JQ(INEXT) ! --------------------------- ! bifurcation of type 20
      itypbir = ityp
      call bifurcation 20 to 23 (INEXT)
      endif ! if ( ityp .eq. 20 )
      if ( ityp .eq. 21 ) then
      bifurcr = JQ(INEXT) ! --------------------------- ! bifurcation of type 21
      itypbir = ityp
      call bifurcation 20 to 23 (INEXT)
      endif ! if ( ityp .eq. 21 ) ! ============================================
      
      if ( ityp .eq. 22 ) then ! ===================== using discharge flow data
      bifurcr = JQ(INEXT) ! --------------------------- ! bifurcation of type 22
      itypbir = ityp
      call bifurcation 20 to 23 (INEXT)
      endif ! if ( ityp .eq. 22 )
      if ( ityp .eq. 23 ) then
      bifurcr = JQ(INEXT) ! --------------------------- ! bifurcation of type 23
      itypbir = ityp
      call bifurcation 20 to 23 (INEXT)
      endif ! if ( ityp .eq. 23 ) ==============================================
*     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb bifurcation
      
      if ( ityp .eq. 44 ) call lake inflow (INEXT)
      if ( ityp .eq. 45 ) call lake outflow (INEXT)
      
      if ( ityp .eq. 45 ) call river boundary (INEXT)

*     ==========================================================================



*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     test whether quality gap filling is required and store the flow and ------
*     quality data for the head of the Reach -----------------------------------
      if ( ical .eq. 3 ) then
      call DUMP (0)
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      call set river quality targets for plotting
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! reach ---------------
      call get summaries of loads
      
      call write headings at the start of the reach
      distp = 0.0 ! distance to next Feature
      DISTR = 0.0
      call classify (INEXT,1) ! Head of Reach with Features --------------------
*     set the distance from the Head of Reach to the first Feature -------------
      DISTR = DIST (INEXT) 
      DISTP = DISTR ! the distance between successive Features -----------------
      call assess compliance with river targets (INEXT,1) ! head of reach ++++++

      do j = 1, MP10 ! =========================================================
      if ( QTYPE(J) .ne. 4 ) then ! ============================================
      write(230+j,305)RNAME(JREACH(KFEAT+1)), ! ------ start of reach --- Gi.CSV
     &JREACH(KFEAT+1),0,0, !  ! ------------------ graph plotting ------- Gi.CSV
     &RLENGTH(JREACH(KFEAT+1)),adist(JREACH(KFEAT+1)), ! ---------------- Gi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop) ! --------------------------------- Gi.CSV
      write(240+j,305)RNAME(JREACH(KFEAT+1)), ! ------ start of reach --- Wi.CSV
     &JREACH(KFEAT+1),0,0, !  -------------------- WORD ----------------- Wi.CSV
     &RLENGTH(JREACH(KFEAT+1)),adist(JREACH(KFEAT+1)), ! ---------------- Wi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Wi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Wi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Wi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Wi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop)  ! -------------------------------- Wi.CSV
  305 format('Start of Reach',',',A16,',',I4,',',i4, ! ------------------ Wi.CSV
     &200(',',1PE11.4)) ! ----------------------------------------------- Wi.CSV
    
      
      write(180+j,395)GIScode(KFEAT+1),RNAME(JREACH(KFEAT+1)), ! ------- DAi.CSV
     &RQO(J),Length of main river,confail(J), ! ------------------------ DAi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! -------------------------- DAi.CSV
     &(totals in classes(iclass,j),iclass=1,nclass), ! ----------------- DAi.CSV
     &RNAME(JREACH(KFEAT+1)) ! ----------------------------------------- DAi.CSV
  395 format(' ',a40,',','111,','Start of Reach',',',A16, ! ------------ DAi.CSV
     &',','% Confidence of failure:',(',Target ... ... ',0pf10.2), ! --- DAi.CSV
     &(',',0pf12.2),(',',0pf12.2,'%'),10(',',f8.2,'%'), ! -------------- DAi.CSV
     &',,,,99999,',',,At: Start of reach: ',a16) ! --------------------- DAi.CSV

      endif ! if ( QTYPE(J) .ne. 4 ) ===========================================
      enddo ! do j = 1, MP10 ===================================================
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     set the distances for gap filling ... the head of the reach from the -----
*     last  point of gap filling -----------------------------------------------
      dcalflow = DISTR
      dcalquality = DISTR
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     now process the next Feature in the sequence on this reach ---------------
 1111 continue

      feeture = inext
      JU = INEXT ! the number of the next Feature ------------------------------
      call check the position of the feature (INEXT)
      call check for features out of sequence
      KFEAT = feeture
*     update the feature counter -----------------------------------------------
      feeture = JU
      
      call accumulate lengths ! for next feature in the reach ------------------

      iplace = 4 ! ... upstream of discharge

      call add diffuse sources and natural purification (0) ! for a feature ----

*     perform classification and assess compliance -----------------------------
      if ( ical13 .eq. 0 ) then ! ==============================================
      KFEET = KFEAT
      if ( JT(KFEET) .ne. 10 .and. JT(KFEET) .ne. 45 ) then ! ------------------
      call classify (KFEET,2) ! at feature
      call assess compliance with river targets (KFEET,2) ! at feature
      endif ! if ( JT (KFEET) .ne. 10 ) ----------------------------------------
     
      do j = 1, MP10
      if ( QTYPE(j) .ne. 4 ) then ! ------------------------------------ DAi.CSV

      itype = JT(KFEET)

      if ( ! not one of these features ========================================= 
*    & itype .ne. 01 .and. itype .ne. 04 .and. itype .ne. 06 .and. ! ===========
     & itype .ne. 01 .and. itype .ne. 06 .and. ! ===========
     & itype .ne. 24 .and. itype .ne. 07 .and. itype .ne. 09 .and.
     & itype .ne. 25 .and. itype .ne. 27 .and. itype .ne. 29 .and.
     & itype .ne. 26 .and. itype .ne. 28 .and. itype .ne. 30 .and.
     & itype .ne. 31 .and. itype .ne. 33 .and. itype .ne. 35 .and.
     & itype .ne. 32 .and. itype .ne. 34 .and. itype .ne. 36 .and.
     & itype .ne. 37 .and. itype .ne. 40 .and. itype .ne. 42 .and.
     & itype .ne. 38 .and. itype .ne. 41 .and. itype .ne. 43 .and.
     & itype .ne. 46 .and. itype .ne. 48 .and. 
     & itype .ne. 47 .and. itype .ne. 49 .and. 
     & itype .ne. 13 .and. itype .ne. 15 .and. itype .ne. 50 .and.
     & itype .ne. 14 .and. itype .ne. 16 .and. itype .ne. 51 .and.
     & itype .ne. 52 .and. itype .ne. 53 .and. 
     & itype .ne. 54 .and. itype .ne. 56 .and. itype .ne. 58 .and.
     & itype .ne. 55 .and. itype .ne. 57 .and. itype .ne. 59 ) then ! ==========
          
      write(180+j,1395)GIScode(KFEET),JT(KFEET),UNAME(KFEET), ! -------- DAi.CSV
     &RNAME(IREACH),RQO(j), ! ------------------------------------------ DAi.CSV
     &Length of main river,confail(j), ! ------------------------------- DAi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! -------------------------- DAi.CSV
     &(totals in classes(iclass,j),iclass=1,nclass),UNAME(KFEET) ! ----- DAi.CSV
 1395 format(' ',a40,',',i4,',',a37,',',A16,
     &',','% Confidence of failure:',(',Target ... ... ',0pf10.2), ! --- DAi.CSV
     &(',',0pf12.2),(',',0pf12.2,'%'),10(',',f8.2,'%'),
     &',,,,99999,',',,At: ',a40)
      endif ! not one of these features ========================================

      endif ! if ( QTYPE(j) .ne. 4 ) ----------------------------------- DAi.CSV
      enddo ! do j = 1, MP10 ------------------------------------------- DAi.CSV
      endif ! if ( ical13 .eq. 0 ) =============================================

      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     do the gap filling -------------------------------------------------------
      if ( ical .gt. 0 ) then ! gap filling is being used ----------------------
      if ( IFEAT1 .gt. 0 ) then ! there is a start feature ---------------------
      if ( JT(IFEAT1) .ne. 10 .or. JT(IFEAT1) .ne. 20 .or. ! -------------------
     &JT(IFEAT1) .ne. 21 .or. JT(IFEAT1) .ne. 22 .or.
     &JT(IFEAT1) .ne. 23 ) then  ! exclude certain features --------------------
      call undertake gap filling (JU) ! headwater features excluded ------------
      endif !  if ( JT(IFEAT1) .ne. 10 etc ..
      else ! if ( IFEAT1 .gt. 0 ) other types of reach -------------------------
      call undertake gap filling (JU) ! for other types of reaches -------------
      endif !  if ( IFEAT1 .eq. 0 )
      endif !  if ( ical .gt. 0 ) gap filling is being used --------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



*     set the distance for the plotting profiles -------------------------------
      Length of main river = ADIST(IREACH)
      if ( lake45 .eq. 0 ) then ! this is not a lake outflow
      Length of main river = DIST(JU) + ADIST(IREACH)
      endif ! this is not a lake outflow
      
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting 2 (JU) ! reach ------------------------
      endif
*     end of the calculation of downstream effects -----------------------------

*     glance forward to the next Feature ---------------------------------------
*     IEND = 0 ! #########################
      if ( JREACH(JU+1) .gt. 0 ) then
      if ( JREACH(JU+1) .ne. IREACH ) IEND = 1
      endif
      if ( JREACH(JU+1) .le. 0 ) goto 20
      if ( JREACH(JU+1) .eq. IREACH ) goto 19
*     this is the last Feature in this Reach -----------------------------------
*     get ready to process the tail of the Reach -------------------------------
   20 DISTR0 = DISTR
      DISTR = RLENGTH(IREACH)
      IEND = 1
      goto 21
*     ==========================================================================  
*     this is not the last feature in this Reach -------------------------------
*     distance from head of Reach to next feature ------------------------------
   19 DISTR0 = DISTR
      DISTR = DIST (feeture+1)
*     ==========================================================================  
*     distance from last feature to next feature -------------------------------
   21 DISTP = DISTR - DISTR0
      DISTP = amax1 (DISTP,0.0)

      call write diffuse loads of pollution

      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     gap filling distances ----------------------------------------------------
      dcalflow = dcalflow  + DISTP
      dcalquality = dcalquality + DISTP
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     identify the Feature Type ... set the number of the Feature --------------
      ITYPE = JT(feeture)
      KFEAT = feeture
      call set river quality targets for plotting
      
*     process the feature (skip the Reach Boundary - done already) =============
      if ( ITYPE .ne. 10 ) then ! for Features other than boundaries ===========
      if ( ical13 .ne. 1 ) JSKIP = 0
      iplace = 4 ! ... upstream of discharge
                
      call details upstream of feature ! =======================================
      feeture = inext
*     --------------------------------------------------------------------------

      iplace = 4 ! ... upstream of discharge
      
      call process the feature (ITYPE)

      mark works = 0
*     ---------------------------------------------------------------------------  
*     perform classification and assess compliance ++++++++++++++++++++++++++++++
      !if ( ical13 .eq. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++
      KFEET = KFEAT
      
*                1 - monitoring station                         x
*                2 - stream or tributary                        -
*                3 - sewage works or sewage discharge           -
*                4 - river flow gauge                           x
*                5 - industrial effluent discharge              -
*                6 - plotting point                             x
*                7 - abstraction (of flow)                      
*                8 - weir                                       
*                    (must be at head of Reach)                 
*                9 - river flow regulation point                
*                    (switched on only in Modes 3-8)            
*               10 - upstream river boundary                    
*               11 - bifurcation                                
*                    (must be at head of Reach)                 
*               12 - intermittent discharge                     -
*               13 - start point for diffuse pollution          x
*               14 - end point for diffuse pollution            x
*                    (river type)                               
*               15 - start point for diffuse pollution          x
*               16 - end point for diffuse pollution            x
*                    (effluent type)                            
*               17 - a feature that has no flow, eg. a future   
*                    effluent discharge in a current model      
*               18 - an abstraction which removes a set         
*                    distribution of flow feature.       
*               19 - as 18 but the distribution to be           
*                    abstracted is entered with the effluent    
*                    data sets                                  
*               20 - bifurcation (first arm)                    
*               21 - bifurcation (second arm)                   
*               22 - bifurcation (first arm)                    
*               23 - bifurcation (second arm)                   
*               24 - boundary of a sub-catchment                x
*               25 - start point for agricultural livestock     x
*               26 - end point for agricultural livestock       x
*               27 - start point for agricultural arable        x
*               28 - end point for agricultural arable          x
*               29 - start point for highway runoff             x
*               30 - end point for highway runoff               x
*               31 - start point for urban runoff               x
*               32 - end point for urban runoff                 x
*               33 - start point for atmospheric deposition     x
*               34 - end point for atmospheric deposition       x
*               35 - start point for natural background         x
*               36 - end point for natural background           x
*               37 - start point for septic tanks               x
*               38 - end point for septic tanks                 x
*               39 - mine water                                 -
*               40 - start point for aggregated CSOs            x
*               41 - end point for aggregated CSOs              x
*               42 - start point for aggregated sewage works    x
*               43 - end point for aggregated sewage works      x
*               44 - flow into a lake                           
*               45 - flow from a lake                           
*               46 - start point for - diffuse mines            x
*               47 - end point for - diffuse mines              x
*               48 - start for - birds, boats and angling       x
*               49 - end point - birds, boats and angling       x
*               50 - start for - "user defined type"            x
*               51 - end point - "user defined type"            x
*               52 - start for - "user defined type"            x
*               53 - end point - "user defined type"            x
*               54 - start for - "user defined type"            x
*               55 - end point - "user defined type"            x
*               56 - start for - "user defined type"            x
*               56 - end point - "user defined type"            x
*               58 - start for - "user defined type"            x
*               59 - end point - "user defined type"            x
*               60 - other point discharges                     -
*               61 - private wastewaters                        -
*               62 - fish farms                                 -


      if ( ! not one of these features ========================================= 
     & itype .ne. 01 .and. itype .ne. 04 .and. itype .ne. 06 .and. ! ===========
     & itype .ne. 24 .and. itype .ne. 07 .and. itype .ne. 09 .and.
     & itype .ne. 25 .and. itype .ne. 27 .and. itype .ne. 29 .and.
     & itype .ne. 26 .and. itype .ne. 28 .and. itype .ne. 30 .and.
     & itype .ne. 31 .and. itype .ne. 33 .and. itype .ne. 35 .and.
     & itype .ne. 32 .and. itype .ne. 34 .and. itype .ne. 36 .and.
     & itype .ne. 37 .and. itype .ne. 40 .and. itype .ne. 42 .and.
     & itype .ne. 38 .and. itype .ne. 41 .and. itype .ne. 43 .and.
     & itype .ne. 18 .and. itype .ne. 19 .and. 
     & itype .ne. 46 .and. itype .ne. 48 .and. 
     & itype .ne. 47 .and. itype .ne. 49 .and. 
     & itype .ne. 13 .and. itype .ne. 15 .and. itype .ne. 50 .and.
     & itype .ne. 14 .and. itype .ne. 16 .and. itype .ne. 51 .and.
     & itype .ne. 52 .and. 
     & itype .ne. 53 .and. 
     & itype .ne. 54 .and. itype .ne. 56 .and. itype .ne. 58 .and.
     & itype .ne. 55 .and. itype .ne. 57 .and. itype .ne. 59 ) then ! ==========
      iplace = 4 ! ... upstream of discharge
      
      call classify (KFEET,4) ! d/s of feature ! ===============================
      
      iplace = 4 ! ... upstream of discharge

      call assess compliance with river targets (KFEET,4) ! d/s of feature =====
      endif ! if ( ! not one of these features =================================

      call load calculation ! ! -------------------------- downstream of feature
      
      call sort out the concentrations and loads ! ------- downstream of feature
      
      call add up all the loads ! for all determinands

      else ! if ( ITYPE .ne. 10 ) process the Reach Boundary ===================
          
      call sort out the concentrations and loads ! Reach Boundary
      call add up all the loads ! for all determinands
      endif ! if ( ITYPE .ne. 10 ) =============================================
      inext = inext + 1 ! ======================================================
*     ==========================================================================  

*     calculations from the last Feature in the Reach to the end of the Reach --
      if ( IEND .eq. 1 ) then
      call accumulate lengths ! for the last feature in the reach --------------

      call add diffuse sources and natural purification (0) ! to end of reach --

      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ical .gt. 0 ) then
      call undertake gap filling (-JU-1) ! last feature in reach
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      call load calculation ! ---------------------------------- at end of reach
      
      call process the end of the reach

      return
      endif
      
*     go back and process the next feature -------------------------------------
      goto 1111

      return
      end

      

*     confluence of reaches ----------------------------------------------------
      subroutine mix the reaches
      include 'COMMON DATA.FOR'
      dimension Y(2,MP10,NS) ! --------------------------- added for Version 161

*     identify the reaches mixing at the confluence ----------------------------
*     reach N will be formed by mixing reaches I and J -------------------------
*     J should be the main reach, I the tributary ------------------------------
      J = IPLAN(IREACH,1) ! the main reach
      I = IPLAN(IREACH,2) ! the tributary
      N = IPLAN(IREACH,3) ! the mix of reaches
            
*     --------------------------------------------------------------------------
      if ( IRHOLD (I) .gt. 0. and. IRHOLD (J) .gt. 0 ) goto 1006
      if ( negreach .eq. 0 ) then
      write(01,1005)
      call change colour of text (12) ! orange
      write( *,1005)
      call set screen text colour
      write(09,1005)
      write(33,1005)
 1005 format(/77('*')/
     &'*** No data for the mixing of Reaches ... '/
     &'*** SIMCAT stopped ... '/77('*')/)
      call stop
      else ! if ( negreach .eq. 0 )
      write(09,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N)
      write(33,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N)
      endif ! if ( negreach .eq. 0 )
*     --------------------------------------------------------------------------

 1006 continue
      
      ISTOR1 = IRHOLD(I) ! mixing
      ISTOR2 = IRHOLD(J) ! mixing


*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr 160      
*     read files containing the data from the two reaches to be mixed ==========
      kworks = 0
      do iworks = 1, kountworks! ============================================== 
      do JP = 1,ndet ! =========================================================
      if ( qtype (JP) .ne. 4 ) then ! ==========================================
      do is = 1,ns
      TELODEshots(iworks,JP,is) = 0.0 
      Y(2,JP,is) = 0.0
      Y(1,JP,is) = 0.0
      enddo ! do is = 1,ns
     
*     read the discharge data for the first reach ------------------------------
      read(300+J,*,end=4002,err=4002)i1,ISTORx,iw,JPx,
     &(Y(2,JP,is),is=1,NS),(FD(ISTOR2,IS),is=1,NS)
      if (i1 .ne. J) goto 4002
      kworks = iworks
      
      do is = 1, ns
      TELODEshots(iw,JP,is) = Y(2,JP,IS)   
      enddo ! do is = 1, ns

      endif ! if ( qtype(JP) .ne. 4 ) ==========================================
      enddo ! do JP = 1, ndet ! ================================================
      enddo ! do iworks = 1, kountworks ========================================
 4002 continue ! end of dealing with the first reach ===========================
      rewind (300+J) ! =========================================================
*     ====================================================================== 160      
      

*     ====================================================================== 160      
*     read the discharges data for the second reach ----------------------------
      rewind (300+I) ! prior to reading tributary to mix with main reach ======= 
      
      do iworks = 1, kountworks ! =============================================
      do JP = 1, ndet ! ========================================================
      if ( qtype(JP) .ne. 4 ) then ! ===========================================
      
      read(300+I,*,end=4001,err=4001)i1,ISTORx,iw,JPx,
     &(Y(1,JPx,is),is=1,ns),(FD(ISTOR1,IS),is=1,NS)
                 
      if (i1 .ne. I) goto 4001
      
      if ( JP .eq. JPx ) then 
      if ( iw .eq. iworks .and. iw .gt. kworks) then 
      do is = 1, ns
      TELODEshots(iworks,JP,is) =  
     &amax1(TELODEshots(iworks,JP,is),Y(1,JP,IS))  
      enddo ! do is = 1, ns
      endif
            
      endif ! if ( JP .eq. JPx ) 
      
      if ( JT(feeture-1) .eq. 21 .or. JT(feeture-1) .eq. 23 .or. !  bifurcations
     &     JT(feeture-1) .eq. 11 ) then ! bbbbbbbbbbbbbbbbbbbbbbbbb bifurcations
      do is = 1, ns ! ----------------------------------------------------------
      TELODEshots(iworks,JP,is) = TELODEshots(iworks,JP,is) 
     &                          + Y(1,JP,IS) ! add ---------
      enddo ! do is = 1, ns ----------------------------------------------------
      
      endif ! if ( JT(feeture) .eq. 21 etc ) then ++++++++++++++++++++++++++++++
     
      if (JP .eq. ndetfirst ) then ! wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
      xp1 = 0.0
      xp2 = 0.0
      if ((TELODEshots(iw,JP,1)+Y(1,JP,1)) .gt. 0.00001 ) then ! ---------------
      xp1 = 100.0*TELODEshots(iw,JP,1)/(TELODEshots(iw,JP,1)+Y(1,JP,1))
      xp2 = 100.0*Y(1,JP,1)/(TELODEshots(iw,JP,1)+Y(1,JP,1))
      endif ! if ((TELODEshots(iw,JP,1)+Y(1,JP,1)) .gt. 0.00001 ) --------------
      endif ! if (JP .eq. ndetfirst ) wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
 
      endif ! if ( qtype(JP) .ne. 4 ) ==========================================
      enddo ! do JP = 1, ndet ==================================================
      enddo ! do iworks = 1, kountworks =========================== second reach       

 4001 continue ! ===============================================================

*     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 
      xxlod1 = 0.0
      xxlod2 = 0.0
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr 160      


      do 1 IS = 1,NS
*     sum the flows ------------------------------------------------------------
      FMS(IS) = FD(ISTOR1,IS) + FD(ISTOR2,IS)
      if ( FMS(IS) .lt. 1.0E-12 ) goto 1

*     proportion of effluent in each shot --------------------------------------
      EFMS(IS) = (EFD(ISTOR1,IS) * FD(ISTOR1,IS)
     &         +  EFD(ISTOR2,IS) * FD(ISTOR2,IS)) / FMS(IS)

*     calculate concentrations by mass balance ---------------------------------
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      RQO(JP) = 0

*     mix the total concentrations ---------------------------------------------
      CMS(JP,IS) = (QD(ISTOR1,JP,IS) * FD(ISTOR1,IS) ! mix the concentrations --
     &           +  QD(ISTOR2,JP,IS) * FD(ISTOR2,IS)) / FMS(IS) ! --------------
            
      do ip = 1, n2prop ! mix the apportionment of the concentrations ==========
      
      CTMS(ip,JP,IS) = (EQD(ip,ISTOR1,JP,IS) * FD(ISTOR1,IS) ! mix reaches =====
     &               +  EQD(ip,ISTOR2,JP,IS) * FD(ISTOR2,IS)) / FMS(IS) ! ======
      
      enddo ! ip = 1, n2prop ! mix the apportionment of the concentrations =====

      CTMS(NTD,JP,IS) = (QD(ISTOR1,JP,IS) * FD(ISTOR1,IS) ! - mix concentrations 
     &                +  QD(ISTOR2,JP,IS) * FD(ISTOR2,IS)) / FMS(IS)
*     --------------------------------------------------------------------------      
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! JP = 1, NDET
    1 continue ! IS = 1,NS


*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr      
      do idet =1, ndet ! .......................................................
      if ( QTYPE (idet) .ne. 4 ) then ! ........................................
      if ( kountworks .gt. 0 ) then ! ..........................................
      do iw = 1, kountworks ! .................................................' 
      call use shots to get mean load for works (iw,3,wloadm) ! ............ 160
      TELOADAV(iw,idet) = wloadm
      enddo ! do iw = 1, kountworks ...........................................
      endif ! if ( kountworks .gt. 0 ) ........................................
      endif ! if ( QTYPE (idet) .ne. 4 ) .......................................
      enddo ! do idet =1, ndet .................................................
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr      

      
      do ip = 1, n2prop ! mix the apportionment of the concentrations ==========
      do JP = 1, ndet ! ========================================================
      if ( QTYPE (JP) .ne. 4 ) then ! ==========================================

*     update the sampling rates ------------------------------------------------
      if ( ip .eq. 19 .or. ip .eq. 14 .or. ip .eq. NTD ) then ! -----------------
*     14 = Aggregated STWs             mix the reaches           Feature Type 42
*     19 = Diffuse input               mix the reaches           Feature Type 15
      QUALNB(JP,IP) = 0.0
      if ( RQUALNB(ISTOR1,JP,IP)+RQUALNB(ISTOR2,JP,IP) .gt. 0.0001) then
      QUALNB(JP,IP) = (RQUALNB(ISTOR1,JP,IP)*CQUALNB(ISTOR1,JP,IP)
     &              +  RQUALNB(ISTOR2,JP,IP)*CQUALNB(ISTOR2,JP,IP))/
     &                (CQUALNB(ISTOR1,JP,IP) + CQUALNB(ISTOR2,JP,IP))
      endif
      endif ! if ( ip .eq. 19 .or. ip .eq. 14 ) ---------------------------------

      endif ! if ( QTYPE (JP) .ne. 4 ) then ! ===================================
      enddo ! do JP = 1, ndet ===================================================
      enddo ! ip = 1, n2prop ! mix the apportionment of the concentrations ======

      
*     update CMcontrib and LMcontrib ============================================
      call update summaries of contribution ! in "mix the reaches" ==============

*     compute loads at the head of the new reach by summing the total loads -----

      do 103 JP = 1, NDET ! =====================================================
      if ( QTYPE (JP) .ne. 4 ) then ! ===========================================
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do 102 J1 = 1, nx 
      TNLOADUP2 (JP,J1) = TNLOADUP3 (ISTOR1,JP,J1) ! gains from natural purif.    
     &                  + TNLOADUP3 (ISTOR2,JP,J1)
      TNLOADDN2 (JP,J1) = TNLOADDN3 (ISTOR1,JP,J1) ! losses from natural purif.  
     &                  + TNLOADDN3 (ISTOR2,JP,J1)      
      TALOADDN2 (JP,J1) = TALOADDN3 (ISTOR1,JP,J1) ! removed by qual gap filling   
     &                  + TALOADDN3 (ISTOR2,JP,J1)
      TILOADDN2 (JP,J1) = TILOADDN3 (ISTOR1,JP,J1) ! removed by flow gap filling   
     &                  + TILOADDN3 (ISTOR2,JP,J1)
      TBLODE2   (JP,J1) = TBLODE3   (ISTOR1,JP,J1) ! removed by abstractions
     &                  + TBLODE3   (ISTOR2,JP,J1)
  102 continue ! do J1 = 1, nx 

      
      if ( bifurcr .le. 0 ) then ! this is not a mixing of bifurcations mmmmmmmm
      if ( kount bodies .gt. 0 ) then ! ----------------------------------------
      do 100 ibodies = 1, kount bodies
*     store the annual loads (i13) from upstream sub-catchments ----------------
      TWLOADS(ibodies,JP,i13) = TWloadsrch(ibodies,ISTOR1,JP)
     &                        + TWloadsrch(ibodies,ISTOR2,JP)

      do ip = 1, n2prop
*     add the breakdown of annual (i13) loads from upstream sub-catchments -----
      TWLOADSapp(ibodies,JP,i13,ip) = 
     &TWloadsrchapp(ibodies,ISTOR1,JP,ip) +
     &TWloadsrchapp(ibodies,ISTOR2,JP,ip) 
      enddo ! ip = 1, nprop
      
  100 continue ! if ( kount bodies .gt. 0 )
      endif ! if ( kount bodies .gt. 0 ) ---------------------------------------
      endif ! if ( bifurcr .le. 0 ) ! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      
      endif ! if ( QTYPE (JP) .ne. 4 ) =========================================
  103 continue ! JP = 1, NDET ! ================================================
      
*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! --------------------------------------------------

*     check whether arrays can be cleared because the Reach is not required ----
*     for mixing with downstream reaches ---------------------------------------
      jj2 = -ibifurcr2 ! the reach supplying the two bifurcations --------------
      do JJ = feeture + 1, MU ! check all reaches for the downstream features --
      if ( Jreach(JJ) .eq. I ) goto 2297 ! look first at Reach J ---------------
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. jj2 ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. jj2 ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. jj2 ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. jj2 ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. jj2 ) goto 2297 ! and bifurcations
      enddo ! do JJ = feeture + 1, MU ------------------------------------------
*     no downstream features are using reach jj2 -------------------------------      
      
      JSTOR(ISTOR2) = 0 ! the reach data for J are no longer needed for mixing -
      IRHOLD(jj2) = 0 !  set number used to store data for reach IFF set to zero
      
      jj2 = -ibifurcr2 ! the reach supplying the two bifurcations --------------
      do JJ = feeture + 1, MU ! check all reaches for the downstream features --
      if ( Jreach(JJ) .eq. J ) goto 2397 ! check IPLAN(IREACH,1) ... (I) .......
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. jj2 ) goto 2397 ! and bifurcations -
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. jj2 ) goto 2397 ! and bifurcations -
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. jj2 ) goto 2397 ! and bifurcations -
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. jj2 ) goto 2397 ! and bifurcations -
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. jj2 ) goto 2397 ! and bifurcations -
      enddo ! do JJ = feeture, MU ----------------------------------------------
       
      JSTOR(ISTOR1) = 0 ! the reach data for I are no longer needed for mixing -
      IRHOLD(jj2) = 0 ! set number used to store data for reach IFF set to zero
      
      if ( bifurcr .eq. -1 ) then
      do istor = 1, KR
      if ( JSTOR(istor) .eq. jj2 ) then
      JSTOR(istor) = 0
      IRHOLD(jj2) = 0
      goto 5436
      endif
      enddo
 5436 continue
      endif ! if ( bifurcr .eq. -1 )
      
      goto 8857
      
 2297 continue
 2397 continue
      
 8857 continue



*     compute the confidence limits on river quality ---------------------------
      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then
      call get sampling rates for river quality ! mix the reaches --------------
     &(FEND(I),QE(JP,I),QDN(JP,I),FEND(J),QE(JP,J),QDN(JP,J),0,0)
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! JP = 1, ndet

      if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N)
      do idet = 1, ndet
      if ( qtype(idet) .ne. 4 ) then
      write(120+idet,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N)
      write(200+idet,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N)
      endif
      enddo
      endif
      endif
 1003 format(//24X,53('=')/
     &24X,'Confluence of Reaches'/
     &24X,53('=')/
     &24X,'            Reach number',I6,' - ',A16/
     &24X,'      joins Reach number',I6,' - ',A16/
     &24X,'    to form Reach number',I6,' - ',A16/
     &24X,53('='))

      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,10)RNAME(N)
   10 format(//140('=')/'Flow shots generated downstream of ',
     &'the confluence ...'/'River flow data for the Head of Reach ',
     &'called ',A16/140('='))
      write(01,13)
   13 format('Generated Monte Carlo shots for river flow .... ')
      write(01,14)(FMS(I),I=1,NS)
   14 format(f7.3,20F7.1)
      write(01,15)
   15 format(140('='))
      endif
      endif ! if ( MONF .gt. 1 ) then

      if ( MONQ .gt. 1 ) then
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      if ( nobigout .le. 0 ) then
      write(01,20)RNAME(N)
   20 format(/140('=')/'Generated data for river quality ',
     &'downstream of confluence ....'/
     &'River quality data for the head ',
     &'of the reach called ',A16/140('='))
      write(01,23)Dname(JP)
   23 format('Generated Monte Carlo shots for river quality ',
     &'data for ',a11/140('='))
      write(01,24)(CMS(JP,I),I=1,NS)
   24 format(f7.2,20F7.2)
      write(01,15)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! JP = 1, NDET
      endif ! if ( MONQ .gt. 1 )

      return
      end


      subroutine set up the data for temperature 
      include 'COMMON DATA.FOR'

      IDIST = PDBC (ireach, 1) ! identify the data on temperature for this reach
      if ( idist .ne. 8 ) idist = 1
      rcm = BMAT (ireach, 1, 1)
      rcs = BMAT (ireach, 1, 2)
      tcorf2 = BMAT (ireach, 1, 3)
      qsam = Bnum (ireach, 1) ! number of samples for temperature

      if ( idist .eq. 1 ) then
      if ( MONQ .gt. 0 ) then 
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (rcm,rcs)
      write(33,11)valchars10,valchars11
      write(01,11)valchars10,valchars11
   11 format(57x,53('-')/
     &57x,'Annual summary statistics entered for temperature ...'/
     &57x,53('-')/
     &94X,'Mean =',a10/80X,'Standard deviation =',a10/57x,53('-'))
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( MONQ .gt. 1 )
      do is = 1, NS ! inialise the values of the shots
      BMS(1,IS) = BMS global (1,IS)
      enddo

      if ( rcm .lt. 1.0E-08) return
      if ( rcs .lt. 1.0E-08) then
      do is = 1, NS
      BMS(1,IS) = rcm
      enddo
      return
      endif
 
*     calculate Monte-Carlo sampling errors for temperature --------------------
      call bias in normal temperature ( RCM, RCS )

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = RCM/BM(2)
      if ( BS(2) .gt. 1.0E-08 ) BS(2) = RCS/BS(2)
      if ( MONQ  .gt. 0 ) then 
      if ( nobigout .le. 0 ) write(01,12)BM(2),BS(2)
      write(33,12)BM(2),BS(2)
   12 format(/77('-')/
     &'Corrections for Monte-Carlo sampling errors: temperature'/
     &77('-')/'Mean          =',F8.3/'95-percentile =',F8.3/77('-'))
      endif

      do 2 is = 1, NS ! sample the distributions
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS) ! default random deviate for temperature
      RR5 = TRAN (IS)
      RR6 = tcorf2 * RR1 + RR5 * SQRMB(126, 1.0 - tcorf2 * tcorf2 )
      BMS(1,is) = Vnorm (RR6,RCS,RCM,0.0,BM(2),BS(2),RCM)

    2 continue ! sample the distributions

      XM = 0.0
      XS = 0.0
      do is = 1, NS
      XM = XM + BMS(1,is)
      XS = XS + BMS(1,is) * BMS(1,is)
      enddo

      XS=(XS-XM*XM/NS)/(NS-1)
      if ( XS .lt. 1.0E-10 ) then
      XS = 0.0
      else
      XS = SQRoot(1962,XS)
      endif
      XM = XM / float (NS)
      BC (1,1) = XM ! mean temperature
      BC (1,2) = XS ! standard devoation
      endif

      !if ( munthly structure .eq. 1 ) then ! set up monthly structure    
      if ( idist .eq. 8 ) then ! monthly structure for temperature
      call generate monthly structure for temperature 8
      else
      call generate monthly structure for temperature 2
      endif
      call calculate monthly summaries of temperature
      !endif

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (BC(1,1),BC(1,2))
      !write(01,10)valchars10,valchars11
   10 format(!77('-')/
     &'Calculated summary statistics for temperature ...'7X,
     &'Mean =',a10,'  Deg'/42X,'Standard deviation =',a10,'  Deg'/
     &77('-')/)
      endif
      endif

      return
      end


*     --------------------------------------------------------------------------
*     Compute Monte-Carlo sampling errors --------------------------------------
*     Normal distributions of river water quality ------------------------------
*     --------------------------------------------------------------------------
      subroutine bias in normal temperature (RCM,RCS)
      include 'COMMON DATA.FOR'

      BM(2) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      BS(2) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS)
      RR5 = TRAN (IS) ! default random deviate for temperature
      RR6 = tcorf2 * RR1 + RR5 * SQRMB(127, 1.0 - tcorf2 * tcorf2 )
      RC = RCM + RR6 * RCS ! temperature
      BM(2) = BM(2) + RC
      BS(2) = BS(2) + RC * RC
    2 continue

      BS(2) = (BS(2)-BM(2)*BM(2)/NS)/(NS-1)

      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2) = SQRoot(1008,BS(2))
      else
      BS(2) = 0.0
      endif

      BM(2) = BM(2) / NS
      
      return
      end
*     ==========================================================================
*     TEMPERATURE - TEMPERATURE - TEMPERATURE - TEMPERATURE - TEMPERATURE  - TEM
*     ==========================================================================







*     ==========================================================================
*     SUSPENDED SOLIDS - SUSPENDED SOLIDS - SUSPENDED SOLIDS - SUSPENDED SOLIDS
*     ==========================================================================
      subroutine set up the data for suspended solids 
      include 'COMMON DATA.FOR'
      
      if (BMAT (ireach, 2, 1) .lt. 0.00001 ) return

*     identify the data on suspended solids for this reach ---------------------
      IDIST = PDBC (ireach, 2) ! identify the data on suspended solids ---------
      if ( idist .ne. 8 ) idist = 2
      rcm = BMAT (ireach, 2, 1)
      rcs = BMAT (ireach, 2, 2)
      tcorf2 = BMAT (ireach, 2, 3)
      qsam = Bnum (ireach, 2) ! number of samples for suspended solids
      RC3 = 0.0
      
      GSCM = ALOG ( rcm / SQRoot(1258, rcm + rcs * rcs ) )
      GSCS = SQRoot(1259, ALOG ( 1.0 + (rcs*rcs )/rcm ) )

*     ==========================================================================
      if ( idist .eq. 2 ) then
      do is = 1, NS ! initialise the values of the shots -----------------------
      RR7 = SRAN (IS)  ! uncorrelated normal deviates for suspended solids ------
      RR1 = FRAN (IS)  
      RR6 = tcorf2 * RR1 + RR7 * SQRMB(111,1.0-tcorf2*tcorf2)
      BMS(2,IS) = exp ( RR6 * GSCS + GSCM )
      enddo

      if ( rcm .lt. 1.0E-08) return ! zero mean
      if ( rcs .lt. 1.0E-08) then ! zero standard deviation
      do is = 1, NS
      BMS(2,IS) = rcm ! set all shots to the mean
      enddo
      return
      endif ! if ( rcs is zero )

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in log normal suspended solids ( RCM, RCS )
      if (BM(2) .gt. 1.0E-08) BM(2) = RCM/BM(2)
      if (BS(2) .gt. 1.0E-08) BS(2) = RCS/BS(2)
      if ( MONQ  .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,12)BM(2),BS(2)
   12 format(/77('-')/
     &'Correction factors for Monte-Carlo sampling errors: suspended',
     &' solids'/77('-')/
     &'Mean          =',F8.3/
     &'95-percentile =',F8.3/77('-'))
      endif

*     mean and standard deviation for logged variables -------------------------
      GRCM = 0.0
      GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122307, ALOG (1.0+(RCS*RCS)/RM3) )
      else
      GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100047,RM3) )
      endif
      endif

*     sample the distributions for suspended solids ----------------------------
      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS)
      RR7 = SRAN (IS)
      RR6 = tcorf2 * RR1 + RR7 * SQRMB(128, 1.0 - tcorf2 * tcorf2 )
      BMS(2,is) = Vlogn (RR6, GRCS,GRCM,RC3,BM(2),BS(2),RCM)
    2 continue
      
      XM = 0.0
      XS = 0.0
      do is = 1, NS
      XM = XM + BMS(2,is)
      XS = XS + BMS(2,is) * BMS(1,is)
      enddo

      XS = (XS-XM*XM/NS)/(NS-1)
      if ( XS .lt. 1.0E-10 ) then
      XS = 0.0
      else
      XS = SQRoot(1962,XS)
      endif
      XM = XM / float (NS)

      BC (2,1) = XM
      BC (2,2) = XS
      
      endif ! if ( idist .eq. 2 )
      if ( idist .eq. 8 ) then ! monthly structure for suspended soilds 
      call generate monthly structure for suspended solids 8
      endif ! monthly structure

      call calculate monthly summaries of suspended solids
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( BC(2,1) .gt. 0.000001 ) then
      call sort format 2 (BC(2,1),BC(2,2))
      !write(01,10)valchars10,valchars11
   10 format(
     &'Calculated summary statistics for suspended solids ...',
     &2X,'Mean =',a10,' mg/l'/42X,'Standard deviation =',
     &a10,' mg/l'/77('-'))
      endif
      endif
      endif

      return
      end


*     Compute Monte-Carlo sampling errors --------------------------------------
*     Normal distributions -----------------------------------------------------
      subroutine bias in log normal suspended solids (RCM,RCS)
      include 'COMMON DATA.FOR'
      
      BM(2) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      BS(2) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------

      RC3=0.0

*     mean and standard deviation for logged variables -------------------------
      GRCM = 0.0
      GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122308, ALOG (1.0+(RCS*RCS)/RM3) )
      else
      GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100048,RM3) )
      endif
      endif

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS)
      RR7 = SRAN (IS)
      RR6 = tcorf2 * RR1 + RR7 * SQRMB(129, 1.0 - tcorf2 * tcorf2 )
      RC = exp (GRCM + RR6 * GRCS)

      BM(2) = BM(2) + RC
      BS(2) = BS(2) + RC * RC
    2 continue

      BS(2) = (BS(2)-BM(2)*BM(2)/NS)/(NS-1)

      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2) = SQRoot(1008,BS(2))
      else
      BS(2) = 0.0
      endif

      BM(2) = BM(2) / NS
     
      return
      end
*     SUSPENDED SOLIDS - SUSPENDED SOLIDS - SUSPENDED SOLIDS - SUSPENDED SOLIDS



      subroutine set loads and proportions to zero
      include 'COMMON DATA.FOR'

      KTYPE = JT (FEETURE+1)
*     check whether this next Feature is a river boundary ----------------------
      if ( KTYPE .eq. 10 .or. KTYPE .eq. 20 .or.
     &     KTYPE .eq. 21 .or. KTYPE .eq. 22 .or.
     &     KTYPE .eq. 23 .or. KTYPE .eq. 45 ) then
      do idet = 1, mp10   
      propeff2 (idet) = 0.0
      do J1 = 1, N13
      TNLOADUP2 (idet,J1) = 0.0
      TNLOADDN2 (idet,J1) = 0.0
      TALOADDN2 (idet,J1) = 0.0
      TILOADDN2 (idet,J1) = 0.0
      TBLODE2   (idet,J1) = 0.0
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments -------------------------------
      TWLOADS(ibodies,idet,J1) = 0.0
      do ip = 1, n2prop
*     breakdown of monthly loads from upstream sub-catchments ------------------
      TWLOADSAPP(ibodies,idet,J1,ip) = 0.0
      enddo ! do ip = 1, n2prop

      enddo ! do ibodies = 1, NUW
      enddo ! do J1 = 1, N13
      do iworks = 1, NUED         
      TELOADAV(iworks,idet) = 0.0
      
      do is = 1, NS ! 33333333333333333333333333333 apportionment of percentiles
      TELODEshots(iworks,idet,is) = 0.0 ! percentiles ---------------------- 160
      enddo ! do is = 1, NS 33333333333333333333333 apportionment of percentiles
      
      enddo ! do iworks = 1, NUED
      enddo ! do idet = 1, mp10 
      endif ! if ( KTYPE .eq. 10 etc
     
      return
      end

      

      subroutine initialise the variables for this reach
      include 'COMMON DATA.FOR'
      
*     write headings for the start of the Reach (apart from the first reach) ---
      if ( nobigout .le. 0 ) then      
      if ( IREACH .ne. jreach(1) ) then
      !if ( IPRINT .ne. 1 ) then
      II1 = REACHCODE2(Ireach)-1
      III = reachcode (II1)
      if ( iplan (III,1 ) .eq. 0 .and. iplan (III,2 ) .ne. 0 
     & .and. iplan (III,3 ) .eq. 0 ) then
      write(01,4891)IREACH,RNAME(IREACH)
 4891 format(//24x,53('=')/
     &24X,'BRANCH TO THE NEXT REACH ...',7x,i18/
     &24x,a16/24X,53('='))
      endif
      if ( iplan (III,1 ) .ne. 0 .and. iplan (III,2 ) .eq. 0 
     & .and. iplan (III,3 ) .eq. 0 ) then
      write(01,1003)III,RNAME(III),
     &IREACH,RNAME(IREACH)
 1003 format(//24X,53('=')/
     &24X,'            Reach number',I6,' - ',A16/
     &24X,'    becomes Reach number',I6,' - ',A16/
     &24X,53('='))
      endif
      !endif
      endif
      endif
      suppress5 = 0
 
*     --------------------------------------------------------------------------
*     set the indicator of the end of this Reach -------------------------------
      IEND = 0
*     set the indicators of the start of this Reach ----------------------------
      ISTART = 1
      KSTART = 1
*     set the null value of SPLIT - the fraction of flow passing down a reach --
*     that is a bifurcation ----------------------------------------------------
      split = 1.0 
      lake45 = 0 ! set the indicator of inflow from a lake 
    
      diffuse heading = 0

      add conc = 0 ! initialise for this reach

      KRFPOL13 = 0
      KRFPOL25 = 0
      KRFPOL27 = 0
      KRFPOL29 = 0
      KRFPOL31 = 0
      KRFPOL33 = 0
      KRFPOL35 = 0
      KRFPOL46 = 0
      KRFPOL48 = 0
      KRFPOL50 = 0
      KRFPOL52 = 0
      KRFPOL54 = 0
      KRFPOL56 = 0
      KRFPOL58 = 0
      KRFPOL37 = 0
      KRFPOL40 = 0
      
      KEPOL15 = 0
      KEPOL42 = 0

      KRAPOL13 = 0
      KRAPOL25 = 0
      KRAPOL27 = 0
      KRAPOL29 = 0
      KRAPOL31 = 0
      KRAPOL33 = 0
      KRAPOL35 = 0
      KRAPOL46 = 0
      KRAPOL48 = 0
      KRAPOL50 = 0
      KRAPOL52 = 0
      KRAPOL54 = 0
      KRAPOL56 = 0
      KRAPOL58 = 0
      KRAPOL37 = 0
      KRAPOL40 = 0

      KRQPOL13 = 0
      KRQPOL25 = 0
      KRQPOL27 = 0
      KRQPOL29 = 0
      KRQPOL31 = 0
      KRQPOL33 = 0
      KRQPOL35 = 0
      KRQPOL46 = 0
      KRQPOL48 = 0
      KRQPOL50 = 0
      KRQPOL52 = 0
      KRQPOL54 = 0
      KRQPOL56 = 0
      KRQPOL58 = 0
      KRQPOL37 = 0
      KRQPOL40 = 0

      cut off zero flow = 0.0 

      do iidet = 1, ndet
      NEXC (iidet) = 0 ! allow extrapolation of gap filling
      cut off zero quality (iidet) = 0.0 
      enddo

*     correlation coefficients for mass balance --------------------------------
      CO1 = 0.0 ! initialise river flow on river quality
      CO2 = 0.0 ! initialise river flow on discharge flow
      CO3 = 0.0 ! initialise river flow on discharge quality
      CO4 = 0.0 ! initialise river quality on discharge flow
      CO5 = 0.0 ! discharge (or tributary) flow on discharge quality
      CO6 = 0.0 ! initialise river quality on discharge quality

      EFM = 0.0
      EFS = 0.0
      EF5 = 0.0
      EF3 = 0.0
      ECM = 0.0
      ECS = 0.0
      EF3 = 0.0

      do kdet = 1, ndet
      class limmits (1,kdet) = 1.0e10
      do ic = 2, nclass
      class limmits (ic,kdet) = 0.0
      enddo
      enddo     

      return
      end
 
      
      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     clear the variables which will be used for gap filling -------------------
      subroutine initialise the variables for gap filling
      include 'COMMON DATA.FOR'
*     flag to indicate the start of Reach (for the gap filling routines) -------       
      ICTOP = 1
      IFEAT1 = 0 ! start feature for section of gap filling --------------------
      IFEAT2 = 0 ! end feature for section of gap filling ----------------------
*     "dcalflow" and e"dcalquality" will be the distance from the head of the
*     Reach or the last point of gap filling...
*     "dcalflow" for flow gap filling, "dcalquality" for quality ---------------
      dcalflow = 0.0
      dcalquality = 0.0
      do 2 IS = 1,NS
*     the flow shots -----------------------------------------------------------
      FMX(IS) = 0.0
*     the quality shots --------------------------------------------------------
      do ip = 1, ndet
      CMX(ip,IS) = 0.0
      enddo
    2 continue
      return
      end
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      



*     check for features out of sequence ---------------------------------------
      subroutine check for features out of sequence
      include 'COMMON DATA.FOR'
      if ( DISTP .lt. - 0.0001 ) then
      if ( jt(feeture) .ne. 10 ) then
      write(01,1008)IREACH,uname(feeture)
      call change colour of text (12) ! orange
      write( *,1008)IREACH,uname(feeture)
      call set screen text colour
      write(09,1008)IREACH,uname(feeture)
      write(33,1008)IREACH,uname(feeture)
 1008 format(77('-')/
     &'*** Features out of sequence in Reach number',I6,' ...'/
     &'*** Correct the error for ... ',a37/77('-'))
	call stop
      endif
      endif
      return
      end
*     --------------------------------------------------------------------------


*     check the distance is not longer than the length of the Reach ------------
      subroutine check the position of the feature (INEXT)
      include 'COMMON DATA.FOR'

      if ( lake45 .eq. 1 ) then ! this is an inflow from a lake 
      DISTR = 0.0
      DISTP = 0.0
      endif ! this is an inflow from a lake 

      if ( DISTR - 0.0001 .gt. RLENGTH(IREACH) ) then
      if ( nobigout .le. 0 ) write(01,1007)INEXT,UNAME(INEXT),
     &IREACH,rname(ireach),rlength(Ireach)
      write( *,1007)INEXT,UNAME(INEXT),IREACH,rname(ireach),
     &rlength(Ireach)
 1007 format(77('-')/
     &'*** The location of Feature number',I5,' ... called ... ',A40/
     &'*** is inconsistent',
     &' with length of Reach number',I6,' ... ',a16/
     &'*** The length of the reach is',f6.1,' km.'/77('-'))
      write(09,1007)INEXT,UNAME(INEXT),IREACH,rlength(Ireach)
      write(33,1007)INEXT,UNAME(INEXT),IREACH,rlength(Ireach)
      write(01,1)
    1 format('*** Correct the error and re-run ...'/77('-'))
      write( *,1)
      write(09,1)
      call stop
      endif
      
      return
      end
*     --------------------------------------------------------------------------



      subroutine write diffuse loads of pollution
      include 'COMMON DATA.FOR'
      if ( Distp .gt. 0.0000001 ) diffuse heading = 0
      if ( KEPOL15  .gt. 0 ) call write diffuse effluent load
      if ( KEPOL42  .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL13 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL25 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL27 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL29 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL31 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL33 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL35 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL46 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL37 .gt. 0 ) call write diffuse effluent load
      if ( KRFPOL40 .gt. 0 ) call write diffuse effluent load
      return
      end
  
      
*     re-set the river quality targets for plotting ----------------------------
      subroutine set river quality targets for plotting
      include 'COMMON DATA.FOR'
      
*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(feeture)
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
 
      if ( IQSreach .gt. 0 ) then
      class limmits (nclass,JP) = 1.0e10
      do icc = 1, nclass
      class limmits (icc,JP) = standards for reach (IQSreach,icc)
      enddo
      endif
      
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,JP) ! set target for feature -
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass - 1
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5 ) then
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif
      endif ! if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5 )
      enddo
      endif ! if ( IQSreach .gt. 0 )
      
      RQO(JP) = targit ! use the target for graphs -----------------------------
      MRQO(JP) = MRQS(JP)
      
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET
      
      return
      end


      subroutine accumulate lengths
      include 'COMMON DATA.FOR'
      
      if ( virtualreach .eq. 1 ) return
      if ( ical13 .eq. 0 ) then
*     accumulate the total length of river subject to calculations -------------
      Total river length = Total river length + distp
      Grand river length = Grand river length + distp
*     accumulate the total length within a sub-catchment -----------------------
      TWlength(kount bodies+1) = TWlength(kount bodies+1) + distp
      endif

      return
      end