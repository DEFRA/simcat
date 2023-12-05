*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     Process each reach ...  mix Reaches --------------------------------------
*     --------------------------------------------------------------------------
      subroutine reach
      include 'COM.FOR'

*     initialise the variables and write headings for this reach ---------------
      call initialise the variables for this reach
*     call initialise data for mass balance ! reach
      call initialise the variables for gap filling
*     call set up the data for temperature 
*     call set up the data for suspended solids 

*     ##########################################################################
*     process a Reach which has no Features ------------------------------------
      no features = 0
      
      if ( JREACH (feeture+1) .ne. IREACH ) then
      no features = 1
      call write headings at the start of the reach
      write(33,7361)RNAME(IREACH)
 7361 format('There are no features in Reach: ',A16)
*     set up the distance for calculations of natural purification etc ---------
      DISTP = RLENGTH(IREACH)
      call accumulate lengths
      if ( ical13 .eq. 0 ) then
      call write the correlation coefficients ! reach with no features ---------
      call classify (KFEET,1) ! head of reach
 	call assess compliance with river targets (KFEET,1) ! head of reach - 0
	endif
      call add diffuse sources and natural purification (0)
      call load calculation
      call process the end of the reach
      DISTP = 0.0
      if ( ical13 .eq. 0 ) then
      call write the correlation coefficients ! reach with no features ---------
      call classify (KFEET,3) ! end of reach
 	call assess compliance with river targets (KFEET,3) ! end of reach - 0
	endif
      return
      endif
*     finished a Reach which has no Features -----------------------------------
*     ##########################################################################

*     ##########################################################################
*     process a reach which has Features ---------------------------------------
      call set loads and proportions to zero
*     ==========================================================================
*     process the river boundary -----------------------------------------------
*     set the sequence number of the next Feature ------------------------------
      INEXT = feeture + 1
	ityp = JT(INEXT)

      feeture = inext
      if ( ityp .eq. 10 ) call river boundary (INEXT)
      if ( ityp .eq. 11 ) call bifurcation 11 (INEXT)
      if ( ityp .eq. 20 ) call bifurcation 20 to 23 (INEXT)
      if ( ityp .eq. 21 ) call bifurcation 20 to 23 (INEXT)
      if ( ityp .eq. 22 ) call bifurcation 20 to 23 (INEXT)
      if ( ityp .eq. 23 ) call bifurcation 20 to 23 (INEXT)
      if ( ityp .eq. 44 ) call lake inflow (INEXT)
      if ( ityp .eq. 45 ) call lake outflow (INEXT)
*     ==========================================================================
*     test whether quality gap filling is required and store the flow and ------
*     quality data for the head of the Reach -----------------------------------
      if ( ical .eq. 3 ) then
      write(33,6274)uname(inext)
 6274 format('018 before call to DUMP (0) ... ',a30)
      call DUMP (0)
      endif
      call set river quality targets for plotting
      call calculate summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads
      call write headings at the start of the reach
      distp = 0.0
	DISTR = 0.0
	if ( lake45 .eq. 0 ) then ! this is not a lake outflow
      call classify (INEXT,1) ! head of reach
*     set the distance from the Head of Reach to the first Feature -------------
      DISTR = DIST (INEXT)
      DISTP = DISTR ! the distance between successive Features -----------------
 	call assess compliance with river targets (INEXT,1) ! head of reach N
*     set the distances for gap filling ... the head of the reach from the -----
*     last  point of gap filling -----------------------------------------------
      dcalflow = DISTR
      dcalquality = DISTR
	endif ! this is not a lake outflow
*     ##########################################################################
*     now process the next Feature in the sequence on this reach ---------------
 1111 continue
      feeture = inext
      JU = INEXT
      call check the position of the feature (INEXT)
      call check for features out of sequence
      KFEAT = feeture
*     update the feature counter -----------------------------------------------
      feeture = JU
      call accumulate lengths
      call add diffuse sources and natural purification (0)

*     perform classification and assess compliance -----------------------------
      if ( ical13 .eq. 0 ) then
*     if ( KFEET .lt. 1 ) KFEET = KFEAT
      KFEET = KFEAT
      if ( JT(KFEET) .ne. 24 .and. JT(KFEET) .ne. 6  .and.
     &     JT(KFEET) .ne. 10 .and. JT(KFEET) .ne. 45 .and.
     &     JT(KFEET) .ne.  1 .and. JT(KFEET) .ne. 4 ) then ! (at some
      call classify (KFEET,2) ! at feature
 	call assess compliance with river targets (KFEET,2) ! at feature
	endif ! if ( JT (KFEET) .ne. 24 )
      endif ! if ( ical13 .eq. 0 )
*     do the gap filling -------------------------------------------------------
      IQ remember = IQ
      
      if ( ical .gt. 0 ) then
      if ( IFEAT1 .gt. 0 ) then
      if ( JT(IFEAT1) .ne. 10 .or. JT(IFEAT1) .ne. 20 .or.
     &JT(IFEAT1) .ne. 21 .or. JT(IFEAT1) .ne. 22 .or.
     &JT(IFEAT1) .ne. 23 ) then
	call undertake gap filling (JU)
      endif !  if ( JT(IFEAT1) .ne. 10 etc ..
      else ! if ( IFEAT1 .gt. 0 )
 	call undertake gap filling (JU)
      endif !  if ( IFEAT1 .eq. 0 )
      endif !  if ( ical .gt. 0 )
      IQ = IQ       

*     set the distance for the plotting profiles -------------------------------
 	Length of main river = ADIST(IREACH)
      if ( lake45 .eq. 0 ) then ! this is not a lake outflow
	Length of main river = DIST(JU) + ADIST(IREACH)
      endif ! this is not a lake outflow
      
      if ( ical13 .eq. 0 ) then
      call write data for graph plotting 2 (JU)
      endif
*     end of the calculation of downstream effects -----------------------------

*     glance forward to the next Feature ---------------------------------------
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
*     gap filling distances ----------------------------------------------------
      dcalflow = dcalflow  + DISTP
      dcalquality = dcalquality + DISTP
*     identify the Feature Type ... set the number of the Feature --------------
      ITYPE = JT(feeture)
      KFEAT = feeture
      call set river quality targets for plotting
*     process the feature (skip the Reach Boundary - done already) -------------
      if ( ITYPE .ne. 10 ) then
      call details upstream of feature
      feeture = inext
*     ==========================================================================  
      JSKIP = 0
      call process the feature (ITYPE)
      mark works = 0
*     ==========================================================================  
*     perform classification and assess compliance -----------------------------
      if ( ical13 .eq. 0 ) then
*     if ( KFEET .lt. 1 ) KFEET = KFEAT
      KFEET = KFEAT
      if ( itype .eq. 1 .or. itype .eq. 4 ) then
      if ( ical .gt. 3 ) then
      call classify (KFEET,4) ! monitoring point
 	call assess compliance with river targets (KFEET,4) ! d/s of feature
      endif
      else
      call classify (KFEET,4) ! 
 	call assess compliance with river targets (KFEET,4) ! d/s of feature
      endif
	endif
      call load calculation
      call sort out the concentrations and loads
      call add up all the loads
*     process the Reach Boundary -----------------------------------------------
      else
      call sort out the concentrations and loads
      call add up all the loads
      endif
 	inext = inext + 1
*     ==========================================================================  

*     calculations from the last Feature in the Reach to the end of the Reach --
      if ( IEND .eq. 1 ) then
      call accumulate lengths
      call add diffuse sources and natural purification (0)
      if ( ical .gt. 0 ) then
	call undertake gap filling (JU-1) ! last feature in reach
	endif
      call load calculation
	call process the end of the reach
      return
      endif

*     go back and process the next feature -------------------------------------
      goto 1111
      return
      end


*     confluence of reaches ----------------------------------------------------
      subroutine mix the reaches
      include 'COM.FOR'

*     identify the reaches mixing at the confluence ----------------------------
*     reach N will be formed by mixing reaches I and J -------------------------
*     J should be the main reach, I the tributary ------------------------------
      N = IPLAN(IREACH,3)
      I = IPLAN(IREACH,2)
      J = IPLAN(IREACH,1)
      
      if ( nobigout .le. 0 ) then
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(39,1003)I,RNAME(I),J,RNAME(J),N,RNAME(N) ! 3333333333333333333333333
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333
      endif

      if ( IRHOLD (I) .gt. 0. and. IRHOLD (J) .gt. 0 ) goto 1006

      if ( negreach .eq. 0 ) then
      write(01,1005)
      write(21,1005)
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

 1006 ISTOR1 = IRHOLD (I)
      ISTOR2 = IRHOLD (J)
      
      do 1 IS = 1,NS
*     sum the flows ------------------------------------------------------------
      FMS(IS) = FD (ISTOR1,IS) + FD (ISTOR2,IS)
      if ( FMS(IS) .lt. 1.0E-12 ) goto 1

*     proportion of effluent in each shot --------------------------------------
      EFMS(IS) = (EFD(ISTOR1,IS) * FD(ISTOR1,IS)
     &         +  EFD(ISTOR2,IS) * FD(ISTOR2,IS)) / FMS(IS)

*     calculate concentrations by mass balance ---------------------------------
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then

      CMS(JP,IS) = (QD(ISTOR1,JP,IS) * FD(ISTOR1,IS)
     &           +  QD(ISTOR2,JP,IS) * FD(ISTOR2,IS)) / FMS(IS)
	do ip = 1, nprop
      LMS(ip,JP,IS) = (EQD(ip,ISTOR1,JP,IS) * FD(ISTOR1,IS) ! mix reaches ------
     &              +  EQD(ip,ISTOR2,JP,IS) * FD(ISTOR2,IS)) / FMS(IS)
	enddo ! ip = 1, nprop
      endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! JP = 1, NDET
    1 continue ! IS = 1,NS

*     compute loads at the head of the new reach -------------------------------
      do 103 JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do 102 J1 = 1, nx      
      TGLODE2 (JP,J1) = TGLODE3 (ISTOR1,JP,J1) + TGLODE3 (ISTOR2,JP,J1)
	TRLODE2 (JP,J1) = TRLOAD3 (ISTOR1,JP,J1) + TRLOAD3 (ISTOR2,JP,J1)
	TELODE2 (JP,J1) = TELOAD3 (ISTOR1,JP,J1) + TELOAD3 (ISTOR2,JP,J1)
	T03LOAD2 (JP,J1) = T03LOAD3 (ISTOR1,JP,J1)    
     &                 + T03LOAD3 (ISTOR2,JP,J1)
	T05LOAD2 (JP,J1) = T05LOAD3 (ISTOR1,JP,J1)    
     &                 + T05LOAD3 (ISTOR2,JP,J1)
	T12LOAD2 (JP,J1) = T12LOAD3 (ISTOR1,JP,J1)    
     &                 + T12LOAD3 (ISTOR2,JP,J1)
	T39LOAD2 (JP,J1) = T39LOAD3 (ISTOR1,JP,J1)    
     &                 + T39LOAD3 (ISTOR2,JP,J1)
	T03LOAD1 (JP,J1) = T03LOAD4 (ISTOR1,JP,J1)    
     &                 + T03LOAD4 (ISTOR2,JP,J1)
	T05LOAD1 (JP,J1) = T05LOAD4 (ISTOR1,JP,J1)    
     &                 + T05LOAD4 (ISTOR2,JP,J1)
	T12LOAD1 (JP,J1) = T12LOAD4 (ISTOR1,JP,J1)    
     &                 + T12LOAD4 (ISTOR2,JP,J1)
	T39LOAD1 (JP,J1) = T39LOAD4 (ISTOR1,JP,J1)    
     &                 + T39LOAD4 (ISTOR2,JP,J1)
	TNLOADUP2 (JP,J1) = TNLOADUP3 (ISTOR1,JP,J1)    
     &                  + TNLOADUP3 (ISTOR2,JP,J1)
	TNLOADDN2 (JP,J1) = TNLOADDN3 (ISTOR1,JP,J1)  
     &                  + TNLOADDN3 (ISTOR2,JP,J1)
      TDDLOAD2  (JP,J1) = TDDLOAD3  (ISTOR1,JP,J1)      
     &                  + TDDLOAD3  (ISTOR2,JP,J1)
	TALOADUP2 (JP,J1) = TALOADUP3 (ISTOR1,JP,J1)    
     &                  + TALOADUP3 (ISTOR2,JP,J1)
	TALOADDN2 (JP,J1) = TALOADDN3 (ISTOR1,JP,J1)   
     &                  + TALOADDN3 (ISTOR2,JP,J1)
      TILOADUP2 (JP,J1) = TILOADUP3 (ISTOR1,JP,J1)    
     &                  + TILOADUP3 (ISTOR2,JP,J1)
	TILOADDN2 (JP,J1) = TILOADDN3 (ISTOR1,JP,J1)   
     &                  + TILOADDN3 (ISTOR2,JP,J1)
      T13LOAD2  (JP,J1) = T13LOAD3  (ISTOR1,JP,J1)     
     &                  + T13LOAD3  (ISTOR2,JP,J1)
      T25LOAD2  (JP,J1) = T25LOAD3  (ISTOR1,JP,J1)     
     &                  + T25LOAD3  (ISTOR2,JP,J1)
      T27LOAD2  (JP,J1) = T27LOAD3  (ISTOR1,JP,J1)     
     &                  + T27LOAD3  (ISTOR2,JP,J1)
      T29LOAD2  (JP,J1) = T29LOAD3  (ISTOR1,JP,J1)     
     &                  + T29LOAD3  (ISTOR2,JP,J1)
      T31LOAD2  (JP,J1) = T31LOAD3  (ISTOR1,JP,J1)    
     &                  + T31LOAD3  (ISTOR2,JP,J1)
      T33LOAD2  (JP,J1) = T33LOAD3  (ISTOR1,JP,J1)     
     &                  + T33LOAD3  (ISTOR2,JP,J1)
      T35LOAD2  (JP,J1) = T35LOAD3  (ISTOR1,JP,J1)     
     &                  + T35LOAD3  (ISTOR2,JP,J1)
      T46LOAD2  (JP,J1) = T46LOAD3  (ISTOR1,JP,J1)     
     &                  + T46LOAD3  (ISTOR2,JP,J1)
      T48LOAD2  (JP,J1) = T48LOAD3  (ISTOR1,JP,J1)     
     &                  + T48LOAD3  (ISTOR2,JP,J1)
      T37LOAD2  (JP,J1) = T37LOAD3  (ISTOR1,JP,J1)     
     &                  + T37LOAD3  (ISTOR2,JP,J1)
      T40LOAD2  (JP,J1) = T40LOAD3  (ISTOR1,JP,J1)     
     &                  + T40LOAD3  (ISTOR2,JP,J1)
	T15LOAD2  (JP,J1) = T15LOAD3  (ISTOR1,JP,J1)     
     &                  + T15LOAD3  (ISTOR2,JP,J1)
      T42LOAD2  (JP,J1) = T42LOAD3  (ISTOR1,JP,J1)     
     &                  + T42LOAD3  (ISTOR2,JP,J1)
      TBLOAD2   (JP,J1) = TBLOAD3   (ISTOR1,JP,J1) 
     &                  + TBLOAD3   (ISTOR2,JP,J1)
      TGLODE1   (JP,J1) = TGLODE4   (ISTOR1,JP,J1)      
     &                  + TGLODE4   (ISTOR2,JP,J1)
	TRLODE1   (JP,J1) = TRLOAD4   (ISTOR1,JP,J1)      
     &                  + TRLOAD4   (ISTOR2,JP,J1)
	TELODE1   (JP,J1) = TELOAD4   (ISTOR1,JP,J1)      
     &                  + TELOAD4   (ISTOR2,JP,J1)
	TNLOADUP1 (JP,J1) = TNLOADUP4 (ISTOR1,JP,J1)    
     &                  + TNLOADUP4 (ISTOR2,JP,J1)
	TNLOADDN1 (JP,J1) = TNLOADDN4 (ISTOR1,JP,J1)  
     &                  + TNLOADDN4 (ISTOR2,JP,J1)
      TDDLOAD1  (JP,J1) = TDDLOAD4  (ISTOR1,JP,J1)      
     &                  + TDDLOAD4  (ISTOR2,JP,J1)
	TALOADUP1 (JP,J1) = TALOADUP4 (ISTOR1,JP,J1)   
     &                  + TALOADUP4 (ISTOR2,JP,J1)
	TALOADDN1 (JP,J1) = TALOADDN4 (ISTOR1,JP,J1)   
     &                  + TALOADDN4 (ISTOR2,JP,J1)
      TILOADUP1 (JP,J1) = TILOADUP4 (ISTOR1,JP,J1)    
     &                  + TILOADUP4 (ISTOR2,JP,J1)
	TILOADDN1 (JP,J1) = TILOADDN4 (ISTOR1,JP,J1)   
     &                  + TILOADDN4 (ISTOR2,JP,J1)
      T13LOAD1  (JP,J1) = T13LOAD4  (ISTOR1,JP,J1)     
     &                  + T13LOAD4  (ISTOR2,JP,J1)
      T25LOAD1  (JP,J1) = T25LOAD4  (ISTOR1,JP,J1)     
     &                  + T25LOAD4  (ISTOR2,JP,J1)
      T27LOAD1  (JP,J1) = T27LOAD4  (ISTOR1,JP,J1)     
     &                  + T27LOAD4  (ISTOR2,JP,J1)
      T29LOAD1  (JP,J1) = T29LOAD4  (ISTOR1,JP,J1)     
     &                  + T29LOAD4  (ISTOR2,JP,J1)
      T31LOAD1  (JP,J1) = T31LOAD4  (ISTOR1,JP,J1)     
     &                  + T31LOAD4  (ISTOR2,JP,J1)
      T33LOAD1  (JP,J1) = T33LOAD4  (ISTOR1,JP,J1)     
     &                  + T33LOAD4  (ISTOR2,JP,J1)
      T35LOAD1  (JP,J1) = T35LOAD4  (ISTOR1,JP,J1)     
     &                  + T35LOAD4  (ISTOR2,JP,J1)
      T46LOAD1  (JP,J1) = T46LOAD4  (ISTOR1,JP,J1)     
     &                  + T46LOAD4  (ISTOR2,JP,J1)
      T48LOAD1  (JP,J1) = T48LOAD4  (ISTOR1,JP,J1)     
     &                  + T48LOAD4  (ISTOR2,JP,J1)
      T37LOAD1  (JP,J1) = T37LOAD4  (ISTOR1,JP,J1)     
     &                  + T37LOAD4  (ISTOR2,JP,J1)
      T40LOAD1  (JP,J1) = T40LOAD4  (ISTOR1,JP,J1)     
     &                  + T40LOAD4  (ISTOR2,JP,J1)
	T15LOAD1  (JP,J1) = T15LOAD4  (ISTOR1,JP,J1)     
     &                  + T15LOAD4  (ISTOR2,JP,J1)
	T42LOAD1  (JP,J1) = T42LOAD4  (ISTOR1,JP,J1)     
     &                  + T42LOAD4  (ISTOR2,JP,J1)
      TBLOAD1   (JP,J1) = TBLOAD4   (ISTOR1,JP,J1) 
     &                  + TBLOAD4   (ISTOR2,JP,J1)
  102 continue ! do J1 = 1, nx 

      if ( kount works .gt. 0 ) then
      do iworks = 1, kount works
*     store the annual loads discharged by upstream effluents ------------------
      TELOADAV(iworks,JP) = TELOADAVrch(iworks,ISTOR1,JP)
     &                    + TELOADAVrch(iworks,ISTOR2,JP)
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      if ( JP .eq. ndshot ) then ! 333333333333333333333333333333333333333333333
      TELOADshots(iworks,JP,is) = TELOADrchshots(iworks,ISTOR1,1,is) ! 333333333
     &                          + TELOADrchshots(iworks,ISTOR2,1,is) ! 333333333
      endif ! if ( JP .eq. ndshot ) ! 333333333333333333333333333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! iworks = 1, kount works
	endif ! if ( kount works .gt. 0 )

      if ( kount bodies .gt. 0 ) then
      do 100 ibodies = 1, kount bodies
*     store the annual loads (i13) from upstream sub-catchments ----------------
	TWLOADS(ibodies,JP, i13) = TWloadsrch(ibodies,ISTOR1,JP)
     &                         + TWloadsrch(ibodies,ISTOR2,JP)

      do ip = 1, nprop
*     breakdown of annual (i13) loads from upstream sub-catchments -------------
      TWLOADSapp(ibodies,JP,i13,ip) = 
     &TWloadsrchapp(ibodies,ISTOR1,JP,ip) +
     &TWloadsrchapp(ibodies,ISTOR2,JP,ip) 
      enddo ! ip = 1, nprop

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      if ( JP .eq. ndshot ) then ! 555555555555555555555555555555555555555555555
      do ip = 1, nprop
      TDLOADshots(ibodies,JP,is,ip) = 
     &TDLOADrchshots(ibodies,ISTOR1,1,is,ip) + ! 5555555555555555555555555555555
     &TDLOADrchshots(ibodies,ISTOR2,1,is,ip) ! 555555555555555555555555555555555
      enddo
      endif ! if ( JP .eq. ndshot ) ! 555555555555555555555555555555555555555555
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

      
  100 continue ! if ( kount bodies .gt. 0 )
      endif ! if ( kount bodies .gt. 0 )
      endif ! if ( QTYPE (JP) .ne. 4 )
  103 continue ! JP = 1, NDET

*     calculate loads and the summary statistics of load -----------------------
      call load calculation

*     check whether arrays can be cleared because the Reach is not required ----
*     for mixing with downstream reaches ---------------------------------------
      jjj = 0
      do JJ = feeture + 1, MU ! check all reaches for the downstream features --
      jjj = jj
      if ( Jreach(JJ) .eq. I ) goto 2297 ! check IPLAN(IREACH,2) ... "I"
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. I ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. I ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. I ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. I ) goto 2297 ! and bifurcations
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. I ) goto 2297 ! and bifurcations
      enddo ! JJ = feeture,MU

      JSTOR(ISTOR1) = 0 ! reach data no longer needed
      IRHOLD (I) = 0

 2297 jjj = 0
      do JJ = feeture + 1, MU ! check all reaches for the downstream features --
      jjj = jj
      if ( Jreach(JJ) .eq. J ) goto 2397 ! check IPLAN(IREACH,1) ... "I" 
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. J ) goto 2397 ! and bifurcations
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. J ) goto 2397 ! and bifurcations
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. J ) goto 2397 ! and bifurcations
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. J ) goto 2397 ! and bifurcations
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. J ) goto 2397 ! and bifurcations
      enddo ! JJ = feeture,MU

      JSTOR (ISTOR2) = 0 ! reach finished with
      IRHOLD (J) = 0
 2397 continue

*     compute the confidence limits on river quality ---------------------------
      if ( nocon .eq. 0 ) then
      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then
      call get sampling rates for river quality ! mix the reaches --------------
     &(FEND(I),QE(JP,I),QDN(JP,I),FEND(J),QE(JP,J),QDN(JP,J))
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! JP = 1, ndet
	endif ! if ( nocon .eq. 0 )

      if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1003)I,RNAME(I),
     &J,RNAME(J),N,RNAME(N)
      if ( nobigout .le. 0 ) write(21,1003)I,RNAME(I),
     &J,RNAME(J),N,RNAME(N)
	endif
 1003 format(24X,53('=')/
     &24X,'Confluence of Reaches'/
     &24X,53('=')/
     &24X,'            Reach number',I6,' - ',A16/
     &24X,'      joins Reach number',I6,' - ',A16/
     &24X,'    to form Reach number',I6,' - ',A16/
     &24X,53('='))

      if ( MONF .eq. 2 ) then
      if ( nobigout .le. 0 ) then
	write(01,10)RNAME(N)
	write(21,10)RNAME(N)
   10 format(//150('=')/'Flow shots generated downstream of ',
     &'the confluence ...'/'River flow data for the Head of Reach ',
     &'called ',A16/150('='))
      write(01,13)
      write(21,13)
   13 format('Generated Monte Carlo shots for river flow .... ')
      write(01,14)(FMS(I),I=1,NS)
      write(21,14)(FMS(I),I=1,NS)
   14 format(f7.3,20F7.1)
      write(01,15)
      write(21,15)
   15 format(150('='))
      endif
	endif

      if ( MONQ .gt. 1 ) then
      do JP = 1, NDET
      if ( QTYPE (JP) .ne. 4 ) then
      if ( nobigout .le. 0 ) then
      write(01,20)RNAME(N)
   20 format(/150('=')/'Generated data for river quality ',
     &'downstream of confluence ....'/
     &'River quality data for the head ',
     &'of the reach called ',A16/150('='))
	write(01,23)Dname(JP)
   23 format('Generated Monte Carlo shots for river quality ',
     &'data for ',a11/150('='))
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
      include 'COM.FOR'

	IDIST = PDBC (ireach, 1) ! identify the data on temperature for this reach
	if ( idist .ne. 8 ) idist = 1
	rcm = BMAT (ireach, 1, 1)
	rcs = BMAT (ireach, 1, 2)
	tcorf2 = BMAT (ireach, 1, 3)
	qsam = Bnum (ireach, 1)

      if ( idist .eq. 1 ) then
      if ( MONQ .gt. 1 ) then
 	if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (rcm,rcs)
      write(33,11)valchars10,valchars11
   11 format(57x,53('-')/
     &57x,'Annual summary statistics entered for temperature ...'/
     &57x,53('-')/
     &94X,'Mean =',a10/80X,'Standard deviation =',a10/57x,53('-'))
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( MONQ .gt. 1 )
      do is = 1, NS ! inialise the values of the shots
	BMS(1,IS) = BMS global (1, IS)
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
      if ( MONQ  .gt. 1 ) then
 	if ( nobigout .le. 0 ) write(01,12)BM(2),BS(2)
   12 format(/77('-')/
     &'Corrections for Monte-Carlo sampling errors: temperature'/
     &77('-')/'Mean          =',F8.3/'95-percentile =',F8.3/77('-'))
      endif

      do 2 is = 1, NS ! sample the distributions
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS)
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

      if ( munthly structure .eq. 1 ) then ! set up monthly structure
      if ( idist .eq. 8 ) then ! monthly structure
      call generate monthly structure for temperature 8
      else
      call generate monthly structure for temperature 2
	endif
	call calculate monthly summaries of temperature
	endif

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (BC(1,1),BC(1,2))
      write(01,10)valchars10,valchars11
      write(21,10)valchars10,valchars11
   10 format('Calculated summary statistics for temperature ...',
     &7X,'Mean =',a10,'  Deg'/42X,'Standard deviation =',a10,'  Deg')
      endif
      endif

	return
	end


*     --------------------------------------------------------------------------
*     Compute Monte-Carlo sampling errors --------------------------------------
*     Normal distributions of river water quality ------------------------------
*     --------------------------------------------------------------------------
      subroutine bias in normal temperature (RCM,RCS)
      include 'COM.FOR'

      BM(2) = 0.0
      BS(2) = 0.0
      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RR1 = FRAN (IS)
	RR5 = TRAN (IS)
      RR6 = tcorf2 * RR1 + RR5 * SQRMB(127, 1.0 - tcorf2 * tcorf2 )
      RC = RCM + RR6 * RCS
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
      include 'COM.FOR'

*     identify the data on suspended solids for this reach ---------------------
	IDIST = PDBC (ireach, 2)
	if ( idist .ne. 8 ) idist = 2
	rcm = BMAT (ireach, 2, 1)
	rcs = BMAT (ireach, 2, 2)
	tcorf2 = BMAT (ireach, 2, 3)
	qsam = Bnum (ireach, 2)
	RC3 = 0.0

*     ==========================================================================
      if ( idist .eq. 2 ) then
      do is = 1, NS ! initialise the values of the shots -----------------------
	BMS(2,IS) = BMS global (2,IS)
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

*     sample the distributions -------------------------------------------------
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

      if ( idist .eq. 8 ) then ! monthly structure
      call generate monthly structure for suspended solids 8
      endif ! monthly structure

	call calculate monthly summaries of suspended solids
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
	if ( BC(2,1) .gt. 0.000001 ) then
      call sort format 2 (BC(2,1),BC(2,2))
	write(01,10)valchars10,valchars11
	write(21,10)valchars10,valchars11
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
      include 'COM.FOR'

      BM(2) = 0.0
      BS(2) = 0.0

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
      include 'COM.FOR'

      KTYPE = JT (FEETURE+1)
*     check whether this next Feature is a river boundary ----------------------
      if ( KTYPE .eq. 10 .or. KTYPE .eq. 20 .or.
     &     KTYPE .eq. 21 .or. KTYPE .eq. 22 .or.
     &     KTYPE .eq. 23 .or. KTYPE .eq. 45 ) then
 	do idet = 1, mp10   
	propeff2 (idet) = 0.0
      do J1 = 1, N13
      TGLODE2 (idet,J1) = 0.0
      TRLODE2 (idet,J1) = 0.0
*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      TELODE2   (idet,J1) = 0.0
      T03LOAD2  (idet,J1) = 0.0
      T05LOAD2  (idet,J1) = 0.0
      T12LOAD2  (idet,J1) = 0.0
	T39LOAD2  (idet,J1) = 0.0
      TNLOADUP2 (idet,J1) = 0.0
      TNLOADDN2 (idet,J1) = 0.0
      TDDLOAD2  (idet,J1) = 0.0
      TALOADUP2 (idet,J1) = 0.0
      TALOADDN2 (idet,J1) = 0.0
      TILOADUP2 (idet,J1) = 0.0
      TILOADDN2 (idet,J1) = 0.0
      T13LOAD2  (idet,J1) = 0.0
      T25LOAD2  (idet,J1) = 0.0
      T27LOAD2  (idet,J1) = 0.0
      T29LOAD2  (idet,J1) = 0.0
      T31LOAD2  (idet,J1) = 0.0
      T33LOAD2  (idet,J1) = 0.0
      T35LOAD2  (idet,J1) = 0.0
      T46LOAD2  (idet,J1) = 0.0
      T48LOAD2  (idet,J1) = 0.0
      T37LOAD2  (idet,J1) = 0.0
      T40LOAD2  (idet,J1) = 0.0
      T15LOAD2  (idet,J1) = 0.0
      T42LOAD2  (idet,J1) = 0.0
      TBLOAD2   (idet,J1) = 0.0
      TGLODE1   (idet,J1) = 0.0
      TRLODE1   (idet,J1) = 0.0
      TELODE1   (idet,J1) = 0.0
      T03LOAD1  (idet,J1) = 0.0
      T05LOAD1  (idet,J1) = 0.0
      T12LOAD1  (idet,J1) = 0.0
      T39LOAD1  (idet,J1) = 0.0
      TNLOADUP1 (idet,J1) = 0.0
      TNLOADDN1 (idet,J1) = 0.0
      TDDLOAD1  (idet,J1) = 0.0
      TALOADUP1 (idet,J1) = 0.0
      TALOADDN1 (idet,J1) = 0.0
      TILOADUP1 (idet,J1) = 0.0
      TILOADDN1 (idet,J1) = 0.0
      T13LOAD1  (idet,J1) = 0.0
      T25LOAD1  (idet,J1) = 0.0
      T27LOAD1  (idet,J1) = 0.0
      T29LOAD1  (idet,J1) = 0.0
      T31LOAD1  (idet,J1) = 0.0
      T33LOAD1  (idet,J1) = 0.0
      T35LOAD1  (idet,J1) = 0.0
      T46LOAD1  (idet,J1) = 0.0
      T48LOAD1  (idet,J1) = 0.0
      T37LOAD1  (idet,J1) = 0.0
      T40LOAD1  (idet,J1) = 0.0
      T15LOAD1  (idet,J1) = 0.0
      T42LOAD1  (idet,J1) = 0.0
      TBLOAD1   (idet,J1) = 0.0
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments -------------------------------
      TWLOADS (ibodies,idet,J1) = 0.0
      do ip = 1, nprop
*     breakdown of monthly loads from upstream sub-catchments ------------------
	TWLOADSAPP (ibodies,idet,J1,ip) = 0.0
      enddo ! do ip = 1, nprop

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      do ip = 1, nprop
      TDLOADshots(ibodies,idet,is,ip) = 0.0 ! 55555555555555555555555555555555555
      enddo
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

      enddo ! do ibodies = 1, NUW
      enddo ! do J1 = 1, N13
      do iworks = 1, NUED
      TELOADAV (iworks,idet) = 0.0
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(iworks,idet,is) = 0.0 ! 33333333333333333333333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! do iworks = 1, NUED
      enddo ! do idet = 1, mp10 
      endif ! if ( KTYPE .eq. 10 etc
     
      return
      end
      

      subroutine initialise the variables for this reach
      include 'COM.FOR'

*     write headings for the start of the Reach (apart from the first reach) ---
      if ( nobigout .le. 0 ) then      
      if ( IREACH .ne. jreach(1) ) then
      if ( IPRINT .ne. 1 ) then
      II1 = REACHCODE2(Ireach)-1
      III = reachcode (II1)
      if ( iplan (III,1 ) .eq. 0 .and. iplan (III,2 ) .ne. 0 
     & .and. iplan (III,3 ) .eq. 0 ) then
      write(01,4891)IREACH,RNAME(IREACH)
      write(21,4891)IREACH,RNAME(IREACH)
 4891 format(24x,53('=')/
     &24X,'BRANCH TO THE NEXT REACH ...',7x,i18/
     &24x,a16/24X,53('='))
      endif
      if ( iplan (III,1 ) .ne. 0 .and. iplan (III,2 ) .eq. 0 
     & .and. iplan (III,3 ) .eq. 0 ) then
      write(01,1003)III,RNAME(III),
     &IREACH,RNAME(IREACH)
      write(21,1003)III,RNAME(III),
     &IREACH,RNAME(IREACH)
 1003 format(24X,53('=')/
     &24X,'            Reach number',I6,' - ',A16/
     &24X,'    becomes Reach number',I6,' - ',A16/
     &24X,53('='))
      endif
      endif
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

      wloadkeep = 0.0

      KRFPOL13 = 0
      KRFPOL25 = 0
      KRFPOL27 = 0
      KRFPOL29 = 0
      KRFPOL31 = 0
      KRFPOL33 = 0
      KRFPOL35 = 0
      KRFPOL46 = 0
      KRFPOL48 = 0
      KRFPOL37 = 0
      KRFPOL40 = 0
      KEPOL15 = 0
      KEPOL42 = 0

      KRQPOL13 = 0
      KRQPOL25 = 0
      KRQPOL27 = 0
      KRQPOL29 = 0
      KRQPOL31 = 0
      KRQPOL33 = 0
      KRQPOL35 = 0
      KRQPOL46 = 0
      KRQPOL48 = 0
      KRQPOL37 = 0
      KRQPOL40 = 0

      cut off zero flow = 0.0 

      do iidet = 1, ndet
      NEXC (iidet) = 0
      cut off zero quality (iidet) = 0.0 
      enddo

*     correlation coefficients for mass balance --------------------------------
      CO1 = 0.0 ! initialise river flow on river quality
      CO2 = 0.0 ! initialise river flow on discharge flow
      CO3 = 0.0 ! initialise river flow on discharge quality
      CO4 = 0.0 ! initialise river quality on discharge flow
      CO5 = 0.0 ! initialise discharge (or tributary) flow on discharge quality
      CO6 = 0.0 ! initialise river quality on discharge quality

      EFM = 0.0
      EFS = 0.0
      EF5 = 0.0
      EF3 = 0.0
      ECM = 0.0
      ECS = 0.0
      EF3 = 0.0
	E80 = 0.0

      do kdet = 1, ndet
      class limmits (1,kdet) = 1.0e10
      do ic = 2, nclass
      class limmits (ic,kdet) = 0.0
      enddo
      enddo

      return
      end
 
      
      
      
*     clear the variables which will be used for gap filling -------------------
      subroutine initialise the variables for gap filling
      include 'COM.FOR'
*     flag to indicate the start of Reach (for the gap filling routines) -------       
      ICTOP = 1
      IFEAT1 = 0
      IFEAT2 = 0
*     "dcalflow" and "dcalquality" will be the distance from the head of the
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



*     check for features out of sequence ---------------------------------------
      subroutine check for features out of sequence
      include 'COM.FOR'
      if ( DISTP .lt. - 0.0001 ) then
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
      return
      end
*     --------------------------------------------------------------------------


*     check the distance is not longer than the length of the Reach ------------
      subroutine check the position of the feature (INEXT)
      include 'COM.FOR'

	if ( lake45 .eq. 1 ) then ! this is an inflow from a lake 
      DISTR = 0.0
	DISTP = 0.0
	endif ! this is an inflow from a lake 

      if ( DISTR - 0.0001 .gt. RLENGTH(IREACH) ) then
      if ( nobigout .le. 0 ) write(01,1007)INEXT,UNAME(INEXT),
     &IREACH,rname(ireach),rlength(Ireach)
      if ( nobigout .le. 0 ) write(21,1007)INEXT,UNAME(INEXT),
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
      include 'COM.FOR'
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
      include 'COM.FOR'
      
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
      include 'COM.FOR'
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
