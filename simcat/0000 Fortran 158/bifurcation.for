*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     File bifurcation.for ... 1301 lines --------------------------------------
*     --------------------------------------------------------------------------
*     This file deals with bifurcations - where a river splits into two separate 
*     streams which re-join further downstream ...
*     --------------------------------------------------------------------------
*     The file contains 2 sub-routines:
*     They are called: ---------------------------------------------------------
*     --------------------------------------------------------------------------
*     ...... bifurcation 20 to 23 (KU) ! Feature types: 20, 21, 22, 23 ----
*     ...... bifurcation 11 (KU)
*     ==========================================================================
      
      subroutine bifurcation 20 to 23 (KU) ! Feature types: 20, 21, 22, 23
      include 'COMMON DATA.FOR'
      dimension abmean(MP10),abstdev(MP10),AXLODE(MP10,13)
      dimension Y(MS)
      dimension xxlode(nprop,13),xxlode00(13)
      
      jtypeB = JT(KU) ! identify the type of bifurcation -----------------------
      
*     identify the flow to be diverted into the new reach ----------------------
      IDENTIFY = JF(KU) ! identify the reach 
      if ( IDENTIFY .eq. 0 ) then ! --------------------------------------------
      write(01,1005)
      write(09,1005)
      write(33,1005)
      call change colour of text (20) ! bright red
      write( *,1005)
      call set screen text colour
 1005 format(/120('*')/
     &'*** No data for the head of a bifurcation (A)'/
     &'*** Calculations stopped ...'/120('*')/)
      call stop
      endif ! if ( IDENTIFY .eq. 0 ) ------------------------------------------

      ISTOR = IRHOLD(IDENTIFY) ! identify the reach supplying the flow --------

*     check whether arrays can be cleared --------------------------------------
      do 2296 JJ = KU, MU ! ----------------------------------------------------
      if ( JJ .ne. KU ) then
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. IDENTIFY ) goto 2297 ! first arm                 
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. IDENTIFY ) goto 2297 ! second arm
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. IDENTIFY ) goto 2297 ! first arm                 
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. IDENTIFY ) goto 2297 ! second arm
      endif
 2296 continue ! do 2296 JJ = KU, MU -------------------------------------------

      JSTOR(ISTOR) = 0   ! bifurcation (20,21) - data finished with 
      IRHOLD(IDENTIFY) = 0 ! bifurcation (20,21) - data finished with 
      
 2297 continue
      ireech = IDENTIFY ! reach supplying the flows to the bifurcation

*     set the initial values for the shots of flow and quality -----------------
      do is = 1, NS
      FMS(IS) = 0.0 ! flow
      Y(IS) = 0.0
      EFMS(IS) = 0.0 ! the proportion of effluent in each shot ----------------
      do JPL = 1, ndet
      CMS (JPL,IS) = 0.0 ! concentrations
      enddo ! loop on determinands
      enddo ! loop on shots ----------------------------------------------------

      do JPL = 1, ndet ! =======================================================
      QDN (JPL,IREACH) = 0.0 ! effective sampling rates at ends of reaches -----
      enddo ! do JPL = 1, ndet =================================================

      
*     fill arrays with the shots from the upstream reach -----------------------     
      do IS = 1, NS ! ----------------------------------------------------------
      FMS(IS) = FD(ISTOR,IS) ! initialise with river flows from upstream reach
      EFMS(IS) = EFD(ISTOR,IS) ! proportion of effluent flow
      do JPL = 1, ndet ! determinands
      CMS(JPL,IS) = QD(ISTOR,JPL,IS) ! river quality
      do ip = 1, n2prop
*     concentrations from various types of feature -----------------------------
      CTMS(ip,JPL,IS) = EQD(ip,ISTOR,JPL,IS) 
      enddo ! do ip = 1, n2prop
      enddo ! do JPL = 1, ndet
      enddo ! do IS = 1, NS ! --------------------------------------------------
      


      
*     ==========================================================================      
*     set the starting values of the loads at the head of the new reach --------
      do JPL = 1, ndet ! loop on the determinands ==============================
      if ( QTYPE (JPL) .ne. 4 ) then ! =========================================
      nx = n13 ! month 13 - the annual results
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------     


*     7777777777777777777777777777777777777777777777777777777777777777777777-158
      TGLODE2 (JPL,J1) = TGLODE3 (ISTOR,JPL,J1) ! -- ISTOR: reach supplying flow   
      TRLODE2 (JPL,J1) = TRLODE3 (ISTOR,JPL,J1) ! -- from boundaries and streams
      TELODE2 (JPL,J1) = TELODE3 (ISTOR,JPL,J1)   
      T03LOAD2 (JPL,J1) = T03LOAD3 (ISTOR,JPL,J1)   
      T05LOAD2 (JPL,J1) = T05LOAD3 (ISTOR,JPL,J1)   
      T12LOAD2 (JPL,J1) = T12LOAD3 (ISTOR,JPL,J1)   
      T39LOAD2 (JPL,J1) = T39LOAD3 (ISTOR,JPL,J1)    
      T60LOAD2 (JPL,J1) = T60LOAD3 (ISTOR,JPL,J1)
      T61LOAD2 (JPL,J1) = T61LOAD3 (ISTOR,JPL,J1)
      T62LOAD2 (JPL,J1) = T62LOAD3 (ISTOR,JPL,J1)
      TDDLOAD2  (JPL,J1) = TDDLOAD3 (ISTOR,JPL,J1)   
      TALOADUP2 (JPL,J1) = TALOADUP3 (ISTOR,JPL,J1) 
      TILOADUP2 (JPL,J1) = TILOADUP3 (ISTOR,JPL,J1) ! ---------------------- 158
      T13LOAD2 (JPL,J1) = T13LOAD3 (ISTOR,JPL,J1)   
      T15LOAD2 (JPL,J1) = T15LOAD3 (ISTOR,JPL,J1)   
      T25LOAD2 (JPL,J1) = T25LOAD3 (ISTOR,JPL,J1)   
      T27LOAD2 (JPL,J1) = T27LOAD3 (ISTOR,JPL,J1)   
      T29LOAD2 (JPL,J1) = T29LOAD3 (ISTOR,JPL,J1)   
      T31LOAD2 (JPL,J1) = T31LOAD3 (ISTOR,JPL,J1)   
      T33LOAD2 (JPL,J1) = T33LOAD3 (ISTOR,JPL,J1)   
      T35LOAD2 (JPL,J1) = T35LOAD3 (ISTOR,JPL,J1)   
      T37LOAD2 (JPL,J1) = T37LOAD3 (ISTOR,JPL,J1)   
      T40LOAD2 (JPL,J1) = T40LOAD3 (ISTOR,JPL,J1)   
      T42LOAD2 (JPL,J1) = T42LOAD3 (ISTOR,JPL,J1)   
      T46LOAD2 (JPL,J1) = T46LOAD3 (ISTOR,JPL,J1)   
      T48LOAD2 (JPL,J1) = T48LOAD3 (ISTOR,JPL,J1)
      T50LOAD2 (JPL,J1) = T50LOAD3 (ISTOR,JPL,J1)   
      T52LOAD2 (JPL,J1) = T52LOAD3 (ISTOR,JPL,J1)   
      T54LOAD2 (JPL,J1) = T54LOAD3 (ISTOR,JPL,J1)   
      T56LOAD2 (JPL,J1) = T56LOAD3 (ISTOR,JPL,J1)   
      T58LOAD2 (JPL,J1) = T58LOAD3 (ISTOR,JPL,J1)
      TGLODE1 (JPL,J1) = TGLODE4 (ISTOR,JPL,J1) !  set loads at the head of new reach   
      TRLODE1 (JPL,J1) = TRLODE4 (ISTOR,JPL,J1) !  set loads at the head of new reach   
      TELODE1 (JPL,J1) = TELODE4 (ISTOR,JPL,J1) !  set loads at the head of new reach   
      T03LOAD1 (JPL,J1) = T03LOAD4 (ISTOR,JPL,J1)   
      T05LOAD1 (JPL,J1) = T05LOAD4 (ISTOR,JPL,J1)   
      T12LOAD1 (JPL,J1) = T12LOAD4 (ISTOR,JPL,J1)   
      T39LOAD1 (JPL,J1) = T39LOAD4 (ISTOR,JPL,J1)
      T60LOAD1 (JPL,J1) = T60LOAD4 (ISTOR,JPL,J1)
      T61LOAD1 (JPL,J1) = T61LOAD4 (ISTOR,JPL,J1)
      T62LOAD1 (JPL,J1) = T62LOAD4 (ISTOR,JPL,J1)
      TDDLOAD1 (JPL,J1) = TDDLOAD4 (ISTOR,JPL,J1)   
      T13LOAD1 (JPL,J1) = T13LOAD4 (ISTOR,JPL,J1)   
      T15LOAD1 (JPL,J1) = T15LOAD4 (ISTOR,JPL,J1)   
      T25LOAD1 (JPL,J1) = T25LOAD4 (ISTOR,JPL,J1)   
      T27LOAD1 (JPL,J1) = T27LOAD4 (ISTOR,JPL,J1)   
      T29LOAD1 (JPL,J1) = T29LOAD4 (ISTOR,JPL,J1)   
      T31LOAD1 (JPL,J1) = T31LOAD4 (ISTOR,JPL,J1)   
      T33LOAD1 (JPL,J1) = T33LOAD4 (ISTOR,JPL,J1)   
      T35LOAD1 (JPL,J1) = T35LOAD4 (ISTOR,JPL,J1)   
      T37LOAD1 (JPL,J1) = T37LOAD4 (ISTOR,JPL,J1)   
      T40LOAD1 (JPL,J1) = T40LOAD4 (ISTOR,JPL,J1)   
      T42LOAD1 (JPL,J1) = T42LOAD4 (ISTOR,JPL,J1)   
      T46LOAD1 (JPL,J1) = T46LOAD4 (ISTOR,JPL,J1) 
      T48LOAD1 (JPL,J1) = T48LOAD4 (ISTOR,JPL,J1) 
      T50LOAD1 (JPL,J1) = T50LOAD4 (ISTOR,JPL,J1) 
      T52LOAD1 (JPL,J1) = T52LOAD4 (ISTOR,JPL,J1) 
      T54LOAD1 (JPL,J1) = T54LOAD4 (ISTOR,JPL,J1) 
      T56LOAD1 (JPL,J1) = T56LOAD4 (ISTOR,JPL,J1) 
      T58LOAD1 (JPL,J1) = T58LOAD4 (ISTOR,JPL,J1)
*     77777777777777777777777777777777777777777777777777777777777777777777777777
      
      TBLODE2 (JPL,J1) = TBLODE3 (ISTOR,JPL,J1) ! load removed by abstractions -
      enddo ! do J1 = 1, nx ----------------------------------------------------
*     --------------------------------------------------------------------------      

      
*     77777777777777777777777777777777777777777777777777777777777777777777777777
      if ( kountworks .gt. 0 ) then ! ==========================================
      do iworks = 1, kountworks
      TELOADAV(iworks,JPL) = TELOADAVrch(iworks,ISTOR,JPL)
      enddo ! do iworks = 1, kountworks ---------------------------------------
      endif ! if ( kountworks .gt. 0 ) then ===================================
*     77777777777777777777777777777777777777777777777777777777777777777777777777

      
      if ( kount bodies .gt. 0 ) then ! ========================================
      do ibodies = 1, kount bodies
*     annual loads (i13) from upstream sub-catchments --------------------------
      TWLOADS(ibodies,JPL, i13) = TWloadsrch(ibodies,ISTOR,JPL) ! from reach ---
      do ip = 1, n2prop ! ------------------------------------------------------
*     breakdown of annual (i13) loads from upstream sub-catchments -------------
      TWLOADSapp (ibodies,JPL,i13,ip) = 
     &TWloadsrchapp (ibodies,ISTOR,JPL,ip)
      enddo ! do ip = 1, n2prop -------------------------------------------------
      enddo ! do ibodies = 1, kount bodies -------------------------------------
      endif ! if ( kount bodies .gt. 0 ) then ==================================

      endif ! if ( QTYPE (JPL) .ne. 4 ) then ===================================
      enddo ! do JPL = 1, ndet --- loop on the determinands ====================
      

      do JJP = 1, ndet
      QDN(JJP,IREACH) = QDN(JJP,IDENTIFY) ! effective sampling rate ------------
      enddo

     
      if ( nobigout .le. 0 ) then
      write(01,1003)jtypeb,IDENTIFY,RNAME(IDENTIFY),IREACH,
     &RNAME(IREACH)
 1003 format(/24X,53('=')/
     &24X,'Start of bifurcation (',i2,') ---'/24x,53('=')/
     &24X,'    part of reach number',I6,' - ',A16/
     &24X,'      forms reach number',I6,' - ',A16/24x,53('='))
      endif
     
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( MONF .gt. 1 ) then ! ================================================
      write(01,10)RNAME(IREACH)
   10 format(//140('=')/'Flow shots that will contribute to the ',
     &'bifurcation (reach) called: ',A16/140('='))
      write(01,14)(FMS(IS),IS = 1,NS)
   14 format(f10.1,20F7.1)
      write(01,15)
   15 format(140('='))
      call calculate summaries of river flow ! bifurcation 20 and 23 -----------
      write(01,29)Flow(1),Flow(2)
   29 Format('Calculated mean flow   =',f8.2/
     &       '95-percentile low flow =',f8.2)       
      write(01,15)
      endif ! if ( MONF .gt. 1 ) ===============================================

      if ( MONQ .ge. 2 ) then ! ================================================
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4) then
      write(01,20)RNAME(IREACH)
   20 format(//140('=')/'River quality shots ',
     &'at the head of the bifurcation (reach)',
     &' called: ',a16/140('='))
      write(01,24)(CMS(JPL,IS),IS = 1,50)
   24 format(15F8.1)
      write(01,15)
      endif
      enddo
      endif ! if ( MONQ .ge. 2 ) ===============================================
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++
      

*     initialise the diverted loads --------------------------------------------
      do jdet = 1, ndet
      do L13 = 1, N13
      NSM (jdet,L13) = 0 ! number of shots per month
      AXLODE (jdet,L13) = 0.0
      enddo
      abmean (jdet) = 0.0
      abstdev (jdet) = 0.0
      enddo


*     flow diverted to the bifurcation -----------------------------------------
      
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      IF = JQ(KU) ! code for the river flow data-set ---------------------------
      endif  
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type -----------
      IF = JQ(KU) ! code for the discharge data data-set -----------------------
      endif
      
      if ( IF .lt. 1 ) then ! check for a zero code number
      if ( nobigout .le. 0 ) write(01,1020)KU ! wowowowowo
      if ( iscreen .lt. 3 ) write( *,1020)KU
      write(09,1020)KU
      write(33,1020)KU
 1020 format(/'*** Diverted flow data specified for Feature number',I6,
     &' do not exist.')
      if ( nobigout .le. 0 ) write(01,1996)
      if ( iscreen .lt. 3 ) write( *,1996)
      write(09,1996)
      write(33,1996)
 1996 format(/'*** No divertion made ***'/77('-'))
      endif ! check for a zero code number
      
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then ! bifurcations 
      if ( nobigout .le. 0 ) then
      call sort format 2 (F(IF,1),F(IF,2))
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      if ( jtypeB .eq. 20 ) then ! river type ----------------------------------
      write(01,2012)valchars10,FUNIT,valchars11,FUNIT
 2012 format(/77('-')/
     &'Flow diverted to the other arm of the bifurcation:',
     &6X,'Mean =',a10,1X,A4/46X,'95% exceedence =',a10,1x,a4)
      else
      write(01,2988)valchars10,FUNIT,valchars11,FUNIT
 2988 format(/77('-')/
     &'Flow diverted to this arm of the bifurcation:',
     &11X,'Mean =',a10,1X,A4/46X,'95% exceedence =',a10,1x,a4/77('-'))
      endif
      endif
      else ! discharge type
      if ( jtypeB .eq. 22 ) then ! discharge type --------------
      write(01,2812)valchars10,FUNIT,valchars11,FUNIT
 2812 format(/77('-')/
     &'Flow diverted to the other arm of the bifurcation:',
     &6X,'Mean =',a10,1X,A4/42X,'Standard deviation =',a10,1x,a4)
      else
      write(01,2862)valchars10,FUNIT,valchars11,FUNIT
 2862 format(/77('-')/
     &'Flow diverted to this arm of the bifurcation:',
     &11X,'Mean =',a10,1X,A4/42X,'Standard deviation =',a10,1x,
     &a4/77('-'))
      endif ! effluent type

      if ( F(IF,2) .gt. 1.0e-09 .and. PDRF(IF) .eq. 3 ) then
      call sort format 1 (F(IF,3))
      write(01,2912)valchars10,FUNIT
 2912 format(55X,'Shift =',a10,1X,A4)
      endif ! print out the shift for a log normal distribution
      write(01,2913)
 2913 format(77('-')) 
      endif ! if ( nobigout .le. 0 )
      endif ! print out diverted flows

*     note whether the distribution of flow is non-parametric ==================
      if ( PDRF(IF) .eq. 4 ) then
      do i = 1, M7 ! identify the file with the non-parametric data ------------
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      if ( nobigout .le. 0 ) write(01,1965) flnamesmall(1,icod)
 1965 format(77('-')/
     &'These flows are from a non-parametric distribution ... ',
     &'File: ',a64/77('-'))
      goto 1970
      endif
      enddo
      if ( nobigout .le. 0 ) write(01,8920)KU
 8920 format(/'*** Non-parametric data specified for ',
     &'Feature number',I6,' do not exist.')
      if ( nobigout .le. 0 ) write(01,8916)
      if ( iscreen .lt. 3 ) write( *,8916)
      write(09,8916)
      write(33,8916)
 8916 format(/'*** No addition made for the bifurcation ***'/
     &77('-'))
      return
      endif ! non-parametric data on river flow ---------------------------------
 1970 continue ! non-parametric data on river flow ------------------------------
      
*     note whether the flow data are monthly ====================================
      if ( PDRF(IF) .eq. 5 ) then
      do i = 1, M8 ! identify the file with the monthly data --------------------
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) then
      if ( nobigout .le. 0 ) write(01,1966) FLMONTHsmall(1,icod)
 1966 format(77('-')/'These flows are from monthly distributions ... ',
     &'File: ',a64/77('-'))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1966) FLMONTHsmall(1,icod)
      enddo
      goto 2213
      endif
      enddo
      if ( nobigout .le. 0 ) write(01,1920)KU
 1920 format(/'*** Monthly abstraction data specified for ',
     &'Feature number',I6,' do not exist.')
      if ( nobigout .le. 0 ) write(01,8996)
      if ( iscreen .lt. 3 ) write( *,8996)
      write(09,8996)
      write(33,8996) ! ---------------------------------------------------- .ERR
 8996 format(/'*** No addition made for the bifurcation ***'/77('-'))
      return
      endif ! monthly data on river flow ---------------------------------------
 2213 continue ! monthly data on river flow ------------------------------------

*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     identify the distribution of the diverted flows --------------------------
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! -------------------------
      IFDIST = PDRF(IF) ! from stored river flow data --------------------------
      if ( F(IF,1) .gt. 1.0E-10 ) then ! check for zero mean flow --------------
      EFM = F(IF,1) ! set the mean abstracted flow -----------------------------
      EF5 = F(IF,2) ! set 95-percentile low abstracted flow
      !EFS = F(IF,2) ! or the standard devistion of abstracted flow
      EF3 = 0.0 ! set the shift flow (for 3 parameter log-normal)
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3) ! set the shift flow 
      endif ! if ( F(IF,1) .gt. 1.0E-10 )
      endif ! if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) ------------------------
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type -----------
      IFDIST = PDEF(IF) ! from stored flow data for effluents ------------------  
      if ( FE(IF,1) .gt. 1.0E-10 ) then ! check for zero mean flow -------------
      EFM = FE(IF,1) ! set the mean abstracted flow ----------------------------
      EFS = FE(IF,2) ! or the standard devistion of abstracted flow
      EF3 = 0.0 ! set the shift flow (for 3 parameter log-normal)
      if ( IFDIST .eq. 3 ) EF3 = FE(IF,3) ! set the shift flow 
      endif
      endif
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      
      GEFM = 0.0
      GEFS = 0.0

*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type bifurcation ++
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log-normal case
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFStest = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST)
      if (GEFStest .le. 0.0) then
      write(01,9099) UNAME(KU)
      write( *,9099) UNAME(KU)
      write(09,9099) UNAME(KU)
      write(33,9099) UNAME(KU)
 9099 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 or 21')
      call stop
      endif ! if (GEFStest .le. 0.0)
      GEFS = SQRoot(103003,GEFStest) - 1.6449 ! standard deviation (log)
      EFStest = EXP(GEFS*GEFS) - 1.0 
      if (EFStest .le. 1.0E-10) then
      write(01,9899) UNAME(KU)
      write( *,9899) UNAME(KU)
      write(09,9899) UNAME(KU)
      write(33,9899) UNAME(KU)
 9899 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 21')
      call stop
      endif ! if EFStest is zero
      EFS = EFM * SQRoot(1031,EFStest) ! standard deviation
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      endif ! if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) - log normal
      endif ! river type (20 and 21)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     

      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( IFDIST .eq. 10 ) then ! power curve distribution --------------------
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFStest = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST) 
      if (GEFStest .le. 0.0) then
      write(01,9299) UNAME(KU)
      write( *,9299) UNAME(KU)
      write(09,9299) UNAME(KU)
      write(33,9299) UNAME(KU)
 9299 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if (GEFStest .le. 0.0)
      GEFS = SQRoot(103003,GEFStest) - 1.6449 ! standard deviation (log)
      EFStest = EXP(GEFS*GEFS) - 1.0 
      if (EFStest .le. 1.0E-10) then
      write(01,9829) UNAME(KU)
      write( *,9829) UNAME(KU)
      write(09,9829) UNAME(KU)
      write(33,9829) UNAME(KU)
 9829 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if EFStest is zero
      EFS = EFM * SQRoot(1031,EFStest) ! standard deviation
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      endif ! if ( IFDIST .eq. 10 ) power curve distribution ------------------- 
      endif ! river type (20 and 21) -------------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr


*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type ===========
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log-normal case -----------
      EM3 = EFM+EF3
      if ( EM3 .gt. 1.0e-9 ) then
      GEFM = ALOG ((EM3*EM3)/SQRoot(1036,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1037,ALOG(1.+(EFS*EFS)/(EM3*EM3)))
      EFStest = EXP(GEFS*GEFS) - 1.0
      if (EFStest .le. 1.0E-10) then
      write(01,9899) UNAME(KU)
      write( *,9899) UNAME(KU)
      write(09,9899) UNAME(KU)
      write(33,9899) UNAME(KU)
      call stop
      endif ! if (EFStest .le. 1.0E-10)
      endif ! if ( EM3 .gt. 1.0e-9 )
      endif ! if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) - log normal
      endif ! if ( jtypeB .eq. 22) ... effluent type ===========================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type ===========
      if ( IFDIST .eq. 10 ) then ! power curve  distribution -------------------
      EM3 = EFM+EF3
      if ( EM3 .gt. 1.0e-9 ) then
      GEFM = ALOG ((EM3*EM3)/SQRoot(1036,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1037,ALOG(1.+(EFS*EFS)/(EM3*EM3)))
      EFStest = EXP(GEFS*GEFS) - 1.0
      if (EFStest .le. 1.0E-10) then
      write(01,9899) UNAME(KU) ! wowowowowowowo
      write( *,9899) UNAME(KU)
      write(09,9899) UNAME(KU)
      write(33,9899) UNAME(KU)
      call stop
      endif ! if ( EFStest .le. 1.0E-10 )
      endif ! if ( EM3 .gt. 1.0e-9 )
      endif ! if ( IFDIST .eq. 10 ) power curve distribution -------------------
      endif ! if ( jtypeB .eq. 22) ... effluent type ===========================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee


      if ( IFDIST .eq. 1 ) then ! normal distribution for bifurcation ----------
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 
      GEFM = EFM ! mean
      GEFS = (EFM-EF5) / 1.6449 ! standard deviation
      EFS = GEFS
      endif ! river type 
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 
      GEFM = EFM ! mean
      GEFS = EFS ! standard deviation
      endif ! effluent type 
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      endif ! normal distribution ----------------------------------------------

      
      if ( IFDIST .eq. 0 ) then ! constant flow --------------------------------
      GEFM = EFM ! mean
      GEFS = 0.0 ! zero standard deviation
      EFS = GEFS
      endif ! if ( IFDIST .eq. 0 ) constant flow -------------------------------

      
      CO2 = 1.0 ! correlation of diverted flow on main river flow --------------
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F(IF,MO) ! special correlation -----------
      endif ! if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) ------------------------
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 22 ) then ! river type --------------
      CO2 = FE(IF,MO) ! special correlation ------------------------------------
      endif ! if ( jtypeB .eq. 22 .or. jtypeB .eq. 22 ) ------------------------
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      
      
*     ==========================================================================
*     generate flow data for bifurcation ---------------------------------------      
*     ==========================================================================
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log normal distribution ---
      RFX3 = F(IF,3)
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      call biasel bifurcation (1, GEFM, GEFS, RFX3) ! bias and log normal
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal bifurcation (GEFM,GEFS,EFM) !        
      endif ! river type -------------------------------------------------------
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type ===========
      call BIASEL bifurcation (0, GEFM, GEFS, RFX3) ! bias and log normal
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EFS/BS(3) ! bias in standard deviation
      call generate log normal bifurcation (GEFM,GEFS,EFM) ! log normal effluent        
      endif ! effluent type ====================================================
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! if ( IFDIST .eq. 2 etc) log normal distribution ------------------
*     ==========================================================================


*     ==========================================================================
      if ( IFDIST .eq. 10 ) then ! -------------------- power curve distribution
      RFX3 = F(IF,3)
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      call biasel bifurcation (1, GEFM, GEFS, RFX3) ! bias
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal river flows !------------------------ power curve           
      endif ! river type -------------------------------------------------------
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type -----------
      call biasel bifurcation (0, GEFM, GEFS, RFX3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EFS/BS(3) ! bias in standard deviation
      call generate log normal bifurcation (GEFM,GEFS,EFM) ! ------- power curve         
      endif ! effluent type ----------------------------------------------------
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! if ( IFDIST .eq. 10 ) ------------------- power curve distribution
*     ==========================================================================

      
      
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      if ( IFDIST .eq. 1) then ! normal distribution ---------------------------
      RFX3 = F(IF,3)
      call bias in normal tributary flows (GEFM, GEFS) 
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate normal distribution river flows         
      endif ! river type 
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate normal effluent flows         
      endif ! effluent type 
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! normal distribution ----------------------------------------------
*     ==========================================================================
      
      
*     ==========================================================================
      if ( PDRF(IF) .eq. 4 ) then ! non-parametric distribution ----------------
      call set up the shots for non parametric stream flow
      endif ! non-parametric distributions -------------------------------------
      if ( PDRF(IF) .eq. 5 ) then ! set up the shots for monthly data ----------
      call set up monthly data for bifurcation
      endif ! monthly data -----------------------------------------------------
*     ==========================================================================

      
      call set up the correlation coefficients ! ---- bifurcation 20, 21, 22, 23
      call list the correlation coefficients ! ------ bifurcation 20, 21, 22, 23

*     calculate the flow diverted to the bifurcation ... loop over all the shots
      naro = 0
      if ( CO2 .lt. -9.0 ) CO2 = 1.0
      do IS = 1, NS
      !imonth = qmonth (is) ! set the month for this shot
      !call get the correlated random numbers (IS,R1,R2,R3,R4)
*     get the shot for the flow ... --------------------------------------------
*     impose correlation between added flow and the river flow -----------------
      !R3 = CO2 * R1 + R3 * SQRMB(102, 1.0 - CO2 * CO2 )
      !call get a flow for the stream or discharge (IS, R3, EF)
      EF = FTMS(IS)
*     the flow diverted to the at the head of bifurcation is EF ----------------
*     retrieve the flow that is to be split ------------------------------------
      Y(IS) = FD(ISTOR,IS)

      if ( jtypeb .eq. 21 .or. jtypeb .eq. 23 ) then ! the diverted flow -------
*     set the diverted flow ... make sure it is less than what is available ----
      FMS(IS) = amax1 (0.0, EF) ! the flow is the diverted flow ----------------
      endif ! if ( jtypeb .eq. 21 .or. jtypeb .eq. 23 ) the diverted flow ------
      if ( jtypeb .eq. 20 .or. jtypeb .eq. 22) then ! the remaining flow -------
*     set the remaining flow ... make sure it is not less than zero ------------
      FMS(IS) = amax1 (0.0, (Y(is) - EF)) ! the flow is the remaining flow -----
      
      
      if ( FMS(IS). lt. 0.0001*Y(IS) ) then ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      FMS(IS) = amax1(0.00001,(0.01*Y(IS)))
      call change colour of text (20) ! bright red ... error message -----------
      write( *,6900)FMS(IS)
 6900 format(50('*')/'*** Zero flow for bifurcation reset to',f12.5/
     &50('*'))  
      call set screen text colour ! error message ------------------------------
      write(09,6901)FMS(IS)
      write(33,6901)FMS(IS)
 6901 format(/50('-')/'*** Zero flow for bifurcation reset to',f12.5/
     &50('-')/)  
      endif ! if ( FMS(IS). lt. 0.0001*Y(IS) ) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      
      endif ! if ( jtypeb .eq. 20 .or. jtypeb .eq. 22) the remaining flow ------
      
      enddo ! do IS = 1, NS ----------------------------------------------------

      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     calculate the load linked to the diverted flow ---------------------------
      numflows = 0 ! counter for shots greater than zero -----------------------
      do IS = 1, NS ! ----------------------------------------------------------
      if ( FMS(IS) .gt. 1.0e-10 ) numflows = numflows + 1
      imonth = qmonth (is) ! set the month for this shot
      do jdet = 1, ndet ! ------------------------------------------------------
      if ( QTYPE (jdet) .ne. 4 ) then ! ----------------------------------------     
      K13 = imonth + 1
      AXLODE (jdet,I13) = AXLODE (jdet,I13) + FMS (IS) * CMS(jdet,IS)
      NSM (jdet,I13) = NSM (jdet,I13) + 1 ! number of shots per month
      AXLODE (jdet,K13) = AXLODE (jdet,K13) + FMS (IS) * CMS(jdet,IS)
      NSM (jdet,K13) = NSM (jdet,K13) + 1
*     prepare to calculate the mean of the diverted quality --------------------
      if ( FMS (IS) .gt. 1.0e-10 ) then
      ABMean (jdet) = ABMean (jdet) + CMS(jdet,IS)
      ABStdev(jdet) = ABStdev(jdet) + CMS(jdet,IS)*CMS(jdet,IS)
      endif
      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1, ndet ------------------------------------------------
      enddo ! do IS = 1, NS --- loop on shots ----------------------------------
      
      
*     calculate the added load -------------------------------------------------
      do 5 jdet = 1, ndet ! ====================================================
      if ( QTYPE (jdet) .ne. 4 ) then ! ========================================     
      AXLODE (JDET,I13) = AXLODE (JDET,I13) / float(NS)
      if ( munthly structure .eq. 1 ) then ! ----------- store the monthly loads
      do J13 = 2, N13
      if ( NSM(jdet,J13) .gt. 1 ) then
      AXLODE (JDET,J13) = AXLODE (JDET,J13) / float(NSM(JDET,J13))
      endif
      enddo
      endif ! if ( munthly structure .eq. 1 )
      if ( ABMean(jdet) .lt.1.0e-09 ) then ! -----------------------------------
      abmean (jdet) = 0.0
      abstdev(jdet) = 0.0
      else
      ABStdev(jdet) = SQRoot3(103399, (ABStdev(jdet) 
     &              - ABMean(jdet) * ABMean(jdet) / numflows )
     &              / (numflows - 1))
      ABMean (jdet) = ABMean (jdet) / numflows
      endif ! if ( ABMean(jdet) .lt.1.0e-09 ) ----------------------------------
      endif ! if ( QTYPE (jdet) .ne. 4 ) =======================================
    5 continue ! do 5 jdet = 1, ndet ===========================================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      if ( MONF .gt. 1 ) call write shots for river flow

*     ==========================================================================      
*     set the starting values of the loads at the head of the new reach --------
      frat = 1.0
      
      do JPL = 1, ndet ! =======================================================
      if ( QTYPE (JPL) .ne. 4 ) then ! =========================================
      
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------      
      xxlode00(J1) = 0.0
      do ip = 1, NPROP ! -------------------------------------------------------
      xxlode(ip,J1) = 0.0
      enddo ! do ip = 1, NPROP ! -----------------------------------------------
      enddo ! do J1 = 1, nx ! --------------------------------------------------

      do IS = 1, NS ! ----------------------------------------------------------
      imonth = qmonth (is) ! set the month for this shot
      xxlode00(1) = xxlode00(1) + FMS(IS)*CMS(JPL,IS)    
      xxlode00(imonth+1) = xxlode00(imonth+1) + FMS(IS)*CMS(JPL,IS) 
      do ip = 1, NPROP ! -------------------------------------------------------
      xxlode(ip,1) = xxlode(ip,1) + FMS(IS)*CTMS(ip,JPL,IS) ! whole year 
      xxlode(ip,imonth+1) = xxlode(ip,imonth+1)+FMS(IS)*CTMS(ip,JPL,IS)
      enddo ! do ip = 1, NPROP -------------------------------------------------
      enddo ! do IS = 1, NS ----------------------------------------------------
 
      


      do k13 = 1, 13
      xxlode00(k13) = xxlode00(k13) / float(NS)   
      do ip = 1, NPROP
      xxlode(ip,k13) = xxlode(ip,k13) / float(NS)
      enddo
      enddo
      
      portload = xxlode(1,1) / TELODE2 (JPL,1)
      LMcontrib(NTD,jpl,1) = xxlode00(1) ! bifurcations 20, 21, 22 and 23 ------
      do ip = 1, NPROP
      LMcontrib(ip,jpl,1) = xxlode(ip,1) ! bifurcations 20, 21, 22 and 23 ------ 
      enddo

      if ( kountworks .gt. 0 ) then
      do iworks = 1, kountworks ! loop on back-tracked works
      TELOADAV(iworks,JPL) = portload*TELOADAV(iworks,JPL)
      enddo ! loop on back-tracked works
      endif ! if ( kountworks .gt. 0 ) then
     
      frat1 = AXLODE(JPL,i13) / LMcontrib(NTD,JPL,1) ! scale the loads @@@@@@@@@
      
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ==========================================================     
 



         
*     7777777777777777777777777777777777777777777777777777777777777777777777 158
      TGLODE2 (JPL,J1) = xxlode00(J1)   ! Total net load
      TRLODE2 (JPL,J1) = xxlode(17,J1)  ! Load from headwaters, streams, etc
      TELODE2 (JPL,J1) = xxlode(01,J1)  ! Mean from all discharges (3,12,5,39)
      T03LOAD2 (JPL,J1) = xxlode(02,J1) ! Added by sewage effluents (3) 
      T05LOAD2 (JPL,J1) = xxlode(04,J1) ! Added by industrial discharges (5)
      T12LOAD2 (JPL,J1) = xxlode(03,J1) ! Intermittent discharges of sewage (12)
      T39LOAD2 (JPL,J1) = xxlode(05,J1) ! Added by mine waters (39)
      T60LOAD2 (JPL,J1) = xxlode(28,J1) ! Other point sources (60)
      T61LOAD2 (JPL,J1) = xxlode(29,J1) ! Private wastewaters (61)
      T62LOAD2 (JPL,J1) = xxlode(32,J1) ! Fish farms (62)
      T25LOAD2 (JPL,J1) = xxlode(06,J1) ! Diffuse pollution from livestock (25)
      T27LOAD2 (JPL,J1) = xxlode(07,J1) ! Diffuse pollution from arable (27)
      T29LOAD2 (JPL,J1) = xxlode(08,J1) ! Highway runoff (29)
      T31LOAD2 (JPL,J1) = xxlode(09,J1) ! Urban runoff (31)
      T33LOAD2 (JPL,J1) = xxlode(10,J1) ! Atmosphere deposition (33)
      T35LOAD2 (JPL,J1) = xxlode(11,J1) ! Natural background (35)
      T37LOAD2 (JPL,J1) = xxlode(12,J1) ! Septic tanks (37)
      T40LOAD2 (JPL,J1) = xxlode(13,J1) ! Aggregate CSOs (40)
      T42LOAD2 (JPL,J1) = xxlode(14,J1) ! Aggregated STWs (42)
      T46LOAD2 (JPL,J1) = xxlode(15,J1) ! Diffuse mines (46)
      T48LOAD2 (JPL,J1) = xxlode(16,J1) ! Birds, boats and angling (48)
      T50LOAD2 (JPL,J1) = xxlode(23,J1) ! User-named diffuse input (50)
      T52LOAD2 (JPL,J1) = xxlode(24,J1) ! User-named diffuse input (52)
      T54LOAD2 (JPL,J1) = xxlode(25,J1) ! User-named diffuse input (54)
      T56LOAD2 (JPL,J1) = xxlode(26,J1) ! User-named diffuse input (56)
      T58LOAD2 (JPL,J1) = xxlode(27,J1) ! User-named diffuse input (58)
      T13LOAD2 (JPL,J1) = xxlode(18,J1) ! Diffuse features (river type) (13)
      T15LOAD2 (JPL,J1) = xxlode(19,J1) ! Diffuse features (effluent type) (15)
      TDDLOAD2 (JPL,J1) = xxlode(20,J1) ! Diffuse inputs tagged to reach data
      TALOADUP2(JPL,J1) = xxlode(21,J1) ! Gap filling on river quality
      TILOADUP2(JPL,J1) = xxlode(22,J1) ! Gap filling for river flows ------ 158
      TGLODE1 (JPL,J1) = frat * TGLODE1 (JPL,J1) ! scale the loads @@@@@@@@@@@@@   
      TRLODE1 (JPL,J1) = frat * TRLODE1 (JPL,J1) ! scale the loads @@@@@@@@@@@@@
      TELODE1 (JPL,J1) = frat * TELODE1 (JPL,J1) ! scale the loads @@@@@@@@@@@@@  
      T03LOAD1 (JPL,J1) = frat * T03LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T05LOAD1 (JPL,J1) = frat * T05LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T12LOAD1 (JPL,J1) = frat * T12LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T39LOAD1 (JPL,J1) = frat * T39LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@
      T60LOAD1 (JPL,J1) = frat * T60LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@
      T61LOAD1 (JPL,J1) = frat * T61LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@
      T62LOAD1 (JPL,J1) = frat * T62LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@
      TDDLOAD1 (JPL,J1) = frat * TDDLOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T13LOAD1 (JPL,J1) = frat * T13LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T15LOAD1 (JPL,J1) = frat * T15LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T25LOAD1 (JPL,J1) = frat * T25LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T27LOAD1 (JPL,J1) = frat * T27LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T29LOAD1 (JPL,J1) = frat * T29LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T31LOAD1 (JPL,J1) = frat * T31LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T33LOAD1 (JPL,J1) = frat * T33LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T35LOAD1 (JPL,J1) = frat * T35LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T37LOAD1 (JPL,J1) = frat * T37LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T40LOAD1 (JPL,J1) = frat * T40LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T42LOAD1 (JPL,J1) = frat * T42LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T46LOAD1 (JPL,J1) = frat * T46LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@ 
      T48LOAD1 (JPL,J1) = frat * T48LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@ 
      T50LOAD1 (JPL,J1) = frat * T50LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T52LOAD1 (JPL,J1) = frat * T52LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T54LOAD1 (JPL,J1) = frat * T54LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@   
      T56LOAD1 (JPL,J1) = frat * T56LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@ 
      T58LOAD1 (JPL,J1) = frat * T58LOAD1 (JPL,J1) ! scale the loads @@@@@@@@@@@ 
*     77777777777777777777777777777777777777777777777777777777777777777777777777
      
      frat = AXLODE (JPL,J1) / TGLODE2 (JPL,J1) ! scale the loads @@@@@@@@@@-158 

      TBLODE2 (JPL,J1) = frat * TBLODE2 (JPL,J1) ! removed by abstractions @@@@@ 
      
*     scale the loads in upstream catchments @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      
      if ( kount bodies .gt. 0 ) then ! scale loads after abstraction @@@@@@@@@@ 
      do ibodies = 1, kount bodies ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
*     monthly loads from upstream sub-catchments -------------------------------
      TWLOADS(ibodies,JPL,J1) = frat * TWLOADS(ibodies,JPL, J1) ! bifurcation @@
      do ip = 1, n2prop ! loop on contributions --------------------------------
*     breakdown of monthly loads from upstream sub-catchments ------------------
      TWLOADSapp(ibodies,JPL,J1,ip) = frat * ! bifurcation @@@@@@@@@@@@@@@@@@@@@
     &TWLOADSapp(ibodies,JPL,J1,ip) ! bifurcation @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      enddo ! do ip = 1, n2prop ---- loop on contributions ---------------------
      enddo ! do ibodies = 1, kount bodies @@@@ loop on water bodies @@@@@@@@@@@
      endif ! if ( kount bodies .gt. 0 ) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      enddo ! do J1 = 1, nx ! ============== loop on months ====================
       
      endif ! if ( QTYPE (JPL) .ne. 4 ) ========================================
      enddo ! do JPL = 1, ndet ======= loop on determinands ====================

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 ) return

      call calculate summaries of river flow ! bifurcation 20 and 23 -----------
      xnflow = 100.0 * numflows / NS
      if ( xnflow .lt. 99.999 ) then
      if ( nobigout .le. 0 ) write(01,1813)xnflow
 1813 format(77('-')/'Time flows are in operation',41x,'=',F6.1, ! ---- 20 to 23
     &' %'/77('-'))
      endif

*     identify the code number for the reach providing the river quality -------
      IQB = JF(KU)
      do JPL = 1, ndet
*     check for excluded determinands ------------------------------------------
      if ( QTYPE (JPL) .ne. 4 ) then
      if ( nobigout .le. -9 ) then
      call sort format 2 (Abmean(JPL),ABStdev(JPL))
      write(01,1713)DNAME(JPL),Abmean(JPL),
     &UNITS(JPL),ABStdev(JPL),UNITS(JPL)
 1713 format('Added river quality for ',a11,21x,'Mean =',
     &a10,1x,a4/
     &43X,'Standard deviation =',a10,1x,a4/77('-'))
      endif
      endif
      enddo ! do JPL = 1, ndet

*     call write data for graph plotting

*     calculate loads and the summary statistics of load -----------------------
      call load calculation
*     accumulate the river derived load ----------------------------------------
      call add up all the loads ! for all determinands

      return
      end

      
      

*     bifurcation ... feature type 11 ------------------------------------------
      subroutine bifurcation 11 (KU)
      include 'COMMON DATA.FOR'
      
      dimension abmean(MP10),abstdev(MP10),AXLODE(MP10,13)
      dimension xxlode(nprop,13),xxlode00(13)
      
*     identify data - loop on the maximum number of bifurcations ---------------
      do 94 IB = 1, NB
      JFURC = IB
      if (KU .eq. BIDENT(IB)) goto 95
   94 continue
      write( *,96)KU,IB,JFURC
      write(09,96)KU,IB,JFURC
      write(33,96)KU,IB,JFURC
   96 format(/
     &'*** Unable to find the data for a bifurcation ...'/
     &'*** Halted in Bifurcation (11) ...',3I5)
      call stop
   95 continue
*     the bifurcation has been identified ... retrieve data on how the flow ----
*     will be split ------------------------------------------------------------    
      SPLIT = BSPLIT(JFURC)

*     identify the Reach giving data for the head of the Reach (Bifurcation) ---
      ireech = JF(KU) ! number of the Reach providing both arms ----------------
      if ( IRHOLD (ireech) .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1005)
      write(09,1005)KU,Ireech,IRHOLD(ireech)
      write(33,1005)KU,Ireech,IRHOLD(ireech)
      write( *,1005)KU,Ireech,IRHOLD(ireech)
 1005 format(/120('*')/
     &'*** No data for the head of a Bifurcation (Type 11) ...'/
     &'*** Calculations stopped ... KU,Ireech,IRHOLD(I)',3i6/120('*')/)
      call stop
      endif

      ISTOR = IRHOLD(ireech) ! Bifurcation (Type 11) ---------------------------

      do 1 IS = 1,NS
*     river flows --------------------------------------------------------------
      FMS (IS) = SPLIT * FD(ISTOR,IS)
*     proportion of effluent ---------------------------------------------------
      EFMS(IS) = EFD(ISTOR,IS) ! proportion of effluent flow
*     river quality ------------------------------------------------------------
      do JPL = 1, ndet
      CMS(JPL,IS) = QD(ISTOR,JPL,IS)
      do ip = 1, n2prop
*     concentrations from various types of feature -----------------------------
      CTMS(ip,JPL,IS) = EQD(ip,ISTOR,JPL,IS)
      enddo
      enddo
    1 continue

      
*     11111111111111111111111111111111111111111111111111111111111111111111111111      
*     set the starting values of the loads at the head of the new reach --------
      frat = 1.0
      
      do JPL = 1, ndet ! =======================================================
      if ( QTYPE (JPL) .ne. 4 ) then ! =========================================
      
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------      
      xxlode00(J1) = 0.0
      do ip = 1, NPROP ! -------------------------------------------------------
      xxlode(ip,J1) = 0.0
      enddo ! do ip = 1, NPROP ! -----------------------------------------------
      enddo ! do J1 = 1, nx ! --------------------------------------------------

      do IS = 1, NS ! ----------------------------------------------------------
      imonth = qmonth (is) ! set the month for this shot
      xxlode00(1) = xxlode00(1) + FMS(IS)*CMS(JPL,IS) ! whole year   
      xxlode00(imonth+1) = xxlode00(imonth+1) + FMS(IS)*CMS(JPL,IS) 
      do ip = 1, NPROP ! -------------------------------------------------------
      xxlode(ip,1) = xxlode(ip,1) + FMS(IS)*CTMS(ip,JPL,IS) ! whole year 
      xxlode(ip,imonth+1) = xxlode(ip,imonth+1)+FMS(IS)*CTMS(ip,JPL,IS)
      enddo ! do ip = 1, NPROP -------------------------------------------------
      enddo ! do IS = 1, NS ----------------------------------------------------
 
      
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee 160
*     ==========================================================================
*     Calculate the loads in shots for individual effluents ====================
*     ==========================================================================
      do iworks = 1, kountworks ! trim the loads from individual effluents ====
      do is = 1, NS ! ----------------------------------------------------------
      !TELODEshots(iworks,JPL,is) = TELODEshots(iworks,JPL,is) 
      !&*FMS(IS)/FD(ISTOR,IS)
      enddo ! do is = 1, NS ----------------------------------------------------
      enddo ! do iworks = 1, kountworks ========================================
*     ==========================================================================
*     eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee 160


      do k13 = 1, 13
      xxlode00(k13) = xxlode00(k13) / float(NS)   
      do ip = 1, NPROP
      xxlode(ip,k13) = xxlode(ip,k13) / float(NS)
      enddo
      enddo
      
      LMcontrib(NTD,jpl,1) = xxlode00(1) ! bifurcation 11 ----------------------
      do ip = 1, NPROP
      LMcontrib(ip,jpl,1) = xxlode(ip,1) ! bifurcation 11 ---------------------- 
      enddo

     
      frat1 = AXLODE(JPL,i13) / LMcontrib(NTD,JPL,1) ! scale the loads @@@@@@@@@
      
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ==========================================================     

      frat = AXLODE (JPL,J1) / LMcontrib(NTD,JPL,1) ! scale the loads @@@@@@@@@@ 

      TBLODE2 (JPL,J1) = frat * TBLODE2 (JPL,J1) ! removed by abstractions @@@@@ 
      
*     scale the loads in upstream catchments @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      
      if ( kount bodies .gt. 0 ) then ! scale loads after abstraction @@@@@@@@@@ 
      do ibodies = 1, kount bodies ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
*     monthly loads from upstream sub-catchments -------------------------------
      TWLOADS(ibodies,JPL,J1) = frat * TWLOADS(ibodies,JPL, J1) ! bifurcation @@
      do ip = 1, n2prop ! loop on contributions --------------------------------
*     breakdown of monthly loads from upstream sub-catchments ------------------
      TWLOADSapp(ibodies,JPL,J1,ip) = frat * ! bifurcation @@@@@@@@@@@@@@@@@@@@@
     &TWLOADSapp(ibodies,JPL,J1,ip) ! bifurcation @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      enddo ! do ip = 1, n2prop ---- loop on contributions ---------------------
      enddo ! do ibodies = 1, kount bodies @@@@ loop on water bodies @@@@@@@@@@@
      endif ! if ( kount bodies .gt. 0 ) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      enddo ! do J1 = 1, nx ! ============== loop on months ====================
       
      endif ! if ( QTYPE (JPL) .ne. 4 ) ========================================
      enddo ! do JPL = 1, ndet ======= loop on determinands ====================

      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 ) return

      call calculate summaries of river flow ! bifurcation 20 and 23 -----------
      xnflow = 100.0 * numflows / NS
      if ( xnflow .lt. 99.999 ) then
      if ( nobigout .le. 0 ) write(01,1813)xnflow
 1813 format(77('-')/'Time FLOWS are in operation',41x,'=',F6.1, ! --- 20 and 23
     &' %'/77('-'))
      endif

*     identify the code number for the reach providing the river quality -------
      IQB = JF(KU)
      do JPL = 1, ndet ! 1111111111111111111111111111111111111111111111111111111
      if ( QTYPE (JPL) .ne. 4 ) then ! check for excluded determinands ---------
      if ( nobigout .le. -9 ) then
      call sort format 2 (Abmean(JPL),ABStdev(JPL))
      write(01,1713)DNAME(JPL),Abmean(JPL),
     &UNITS(JPL),ABStdev(JPL),UNITS(JPL)
 1713 format('Added river quality for ',a11,21x,'Mean =',
     &a10,1x,a4/
     &43X,'Standard deviation =',a10,1x,a4/77('-'))
      endif
      endif ! if ( QTYPE (JPL) .ne. 4 ) ---------------------------------------
      enddo ! do JPL = 1, ndet 1111111111111111111111111111111111111111111111111

      
*     check whether arrays can be cleared --------------------------------------
      do 2296 JJ = KU, MU ! loop though the remaining Feature ------------------
      if ( JJ .ne. KU ) then
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. Ireech ) goto 2297
      endif
 2296 continue
      
      JSTOR(ISTOR) = 0 ! bifurcation (11) data finished with -------------------
      IRHOLD(Ireech) = 0 ! use to store data for a reach that is needed later --
      
 2297 continue

*     compute loads at the head of the new reach -------------------------------
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx      


*     7777777777777777777777777777777777777777777777777777777777777777777777-158     
      TGLODE2 (JPL,J1) = TGLODE3 (ISTOR,JPL,J1) 
      TRLODE2 (JPL,J1) = TRLODE3 (ISTOR,JPL,J1) ! from boundaries and streams  
*     net loads from discharges (3 12 5 39 60 61 62) --------------------TELODE2
      TELODE2 (JPL,J1) = TELODE3 (ISTOR,JPL,J1)   
      T03LOAD2 (JPL,J1) = T03LOAD3 (ISTOR,JPL,J1)   
      T05LOAD2 (JPL,J1) = T05LOAD3 (ISTOR,JPL,J1)   
      T12LOAD2 (JPL,J1) = T12LOAD3 (ISTOR,JPL,J1)   
      T39LOAD2 (JPL,J1) = T39LOAD3 (ISTOR,JPL,J1)
      T60LOAD2 (JPL,J1) = T60LOAD3 (ISTOR,JPL,J1)
      T61LOAD2 (JPL,J1) = T61LOAD3 (ISTOR,JPL,J1)
      T62LOAD2 (JPL,J1) = T62LOAD3 (ISTOR,JPL,J1)
      TDDLOAD2 (JPL,J1) = TDDLOAD3 (ISTOR,JPL,J1)   
      TALOADUP2 (JPL,J1) = TALOADUP3 (ISTOR,JPL,J1) 
      TILOADUP2 (JPL,J1) = TILOADUP3 (ISTOR,JPL,J1) 
      T13LOAD2 (JPL,J1) = T13LOAD3 (ISTOR,JPL,J1)   
      T15LOAD2 (JPL,J1) = T15LOAD3 (ISTOR,JPL,J1)   
      T25LOAD2 (JPL,J1) = T25LOAD3 (ISTOR,JPL,J1)   
      T27LOAD2 (JPL,J1) = T27LOAD3 (ISTOR,JPL,J1)   
      T29LOAD2 (JPL,J1) = T29LOAD3 (ISTOR,JPL,J1)   
      T31LOAD2 (JPL,J1) = T31LOAD3 (ISTOR,JPL,J1)   
      T33LOAD2 (JPL,J1) = T33LOAD3 (ISTOR,JPL,J1)   
      T35LOAD2 (JPL,J1) = T35LOAD3 (ISTOR,JPL,J1)   
      T37LOAD2 (JPL,J1) = T37LOAD3 (ISTOR,JPL,J1)   
      T40LOAD2 (JPL,J1) = T40LOAD3 (ISTOR,JPL,J1)   
      T42LOAD2 (JPL,J1) = T42LOAD3 (ISTOR,JPL,J1)   
      T46LOAD2 (JPL,J1) = T46LOAD3 (ISTOR,JPL,J1)   
      T48LOAD2 (JPL,J1) = T48LOAD3 (ISTOR,JPL,J1)  
      T50LOAD2 (JPL,J1) = T50LOAD3 (ISTOR,JPL,J1)   
      T52LOAD2 (JPL,J1) = T52LOAD3 (ISTOR,JPL,J1)   
      T54LOAD2 (JPL,J1) = T54LOAD3 (ISTOR,JPL,J1)   
      T56LOAD2 (JPL,J1) = T56LOAD3 (ISTOR,JPL,J1)   
      T58LOAD2 (JPL,J1) = T58LOAD3 (ISTOR,JPL,J1)  
      TGLODE1 (JPL,J1) = TGLODE4 (ISTOR,JPL,J1)   
      TRLODE1 (JPL,J1) = TRLODE4 (ISTOR,JPL,J1)   
      TELODE1 (JPL,J1) = TELODE4 (ISTOR,JPL,J1)   
      T03LOAD1 (JPL,J1) = T03LOAD4 (ISTOR,JPL,J1)   
      T05LOAD1 (JPL,J1) = T05LOAD4 (ISTOR,JPL,J1)   
      T12LOAD1 (JPL,J1) = T12LOAD4 (ISTOR,JPL,J1)   
      T39LOAD1 (JPL,J1) = T39LOAD4 (ISTOR,JPL,J1)
      T60LOAD1 (JPL,J1) = T60LOAD4 (ISTOR,JPL,J1)
      T61LOAD1 (JPL,J1) = T61LOAD4 (ISTOR,JPL,J1)
      T62LOAD1 (JPL,J1) = T62LOAD4 (ISTOR,JPL,J1)
      TDDLOAD1 (JPL,J1) = TDDLOAD4 (ISTOR,JPL,J1)   
      T13LOAD1  (JPL,J1) = T13LOAD4  (ISTOR,JPL,J1)   
      T15LOAD1  (JPL,J1) = T15LOAD4  (ISTOR,JPL,J1)   
      T25LOAD1  (JPL,J1) = T25LOAD4  (ISTOR,JPL,J1)   
      T27LOAD1  (JPL,J1) = T27LOAD4  (ISTOR,JPL,J1)   
      T29LOAD1  (JPL,J1) = T29LOAD4  (ISTOR,JPL,J1)   
      T31LOAD1  (JPL,J1) = T31LOAD4  (ISTOR,JPL,J1)   
      T33LOAD1  (JPL,J1) = T33LOAD4  (ISTOR,JPL,J1)   
      T35LOAD1  (JPL,J1) = T35LOAD4  (ISTOR,JPL,J1)   
      T37LOAD1  (JPL,J1) = T37LOAD4  (ISTOR,JPL,J1)   
      T40LOAD1  (JPL,J1) = T40LOAD4  (ISTOR,JPL,J1)   
      T42LOAD1  (JPL,J1) = T42LOAD4  (ISTOR,JPL,J1)   
      T46LOAD1  (JPL,J1) = T46LOAD4  (ISTOR,JPL,J1)   
      T48LOAD1  (JPL,J1) = T48LOAD4  (ISTOR,JPL,J1) 
      T50LOAD1  (JPL,J1) = T50LOAD4  (ISTOR,JPL,J1)   
      T52LOAD1  (JPL,J1) = T52LOAD4  (ISTOR,JPL,J1)   
      T54LOAD1  (JPL,J1) = T54LOAD4  (ISTOR,JPL,J1)   
      T56LOAD1  (JPL,J1) = T56LOAD4  (ISTOR,JPL,J1)   
      T58LOAD1  (JPL,J1) = T58LOAD4  (ISTOR,JPL,J1)  
*     7777777777777777777777777777777777777777777777777777777777777777777777-158     

      TBLODE2 (JPL,J1) = TBLODE3 (ISTOR,JPL,J1)   
      enddo

      if ( kountworks .gt. 0 ) then
      do iworks = 1, kountworks
      TELOADAV(iworks,JPL) = TELOADAVrch(iworks,ISTOR,JPL) * split
      enddo ! do iworks = 1, kountworks
      endif ! if ( kountworks .gt. 0 )

      if ( kount bodies .gt. 0 ) then
      do 5400 ibodies = 1, kount bodies
*     annual (i13) loads from upstream sub-catchments --------------------------
      TWLOADS(ibodies,JPL, i13) = TWloadsrch(ibodies,ISTOR,JPL) * split
      do ip = 1, n2prop
*     breakdown of annual (i13) loads from upstream sub-catchments -------------
      TWLOADSapp(ibodies,JPL, i13, ip) = ! bifurcation -------------------------
     &TWloadsrchapp(ibodies,ISTOR,JPL, ip) * split ! bifurcation ---------------
      enddo

 5400 continue
      endif
      
      endif
      enddo


*     77777777777777777777777777777777777777777777777777777777777777777777777777
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx      
      TGLODE2 (JPL,J1) = TGLODE2(JPL,J1) * Split
      TRLODE2 (JPL,J1) = TRLODE2(JPL,J1) * Split ! - from boundaries and streams
*     net loads from discharges (3 12 5 39 60 61) -----------------------TELODE2
      TELODE2 (JPL,J1) = TELODE2(JPL,J1) * Split
      T03LOAD2 (JPL,J1) = T03LOAD2(JPL,J1) * Split
      T05LOAD2 (JPL,J1) = T05LOAD2(JPL,J1) * Split
      T12LOAD2 (JPL,J1) = T12LOAD2(JPL,J1) * Split
      T39LOAD2 (JPL,J1) = T39LOAD2(JPL,J1) * Split
      T60LOAD2 (JPL,J1) = T60LOAD2(JPL,J1) * Split
      T61LOAD2 (JPL,J1) = T61LOAD2(JPL,J1) * Split
      T62LOAD2 (JPL,J1) = T62LOAD2(JPL,J1) * Split ! ---------------- fish farms
      TDDLOAD2 (JPL,J1) = TDDLOAD2(JPL,J1) * Split
      TALOADUP2 (JPL,J1) = TALOADUP2(JPL,J1) * Split
      TILOADUP2 (JPL,J1) = TILOADUP2(JPL,J1) * Split 
      T13LOAD2 (JPL,J1) = T13LOAD2(JPL,J1) * Split
      T15LOAD2 (JPL,J1) = T15LOAD2(JPL,J1) * Split
      T25LOAD2 (JPL,J1) = T25LOAD2(JPL,J1) * Split
      T27LOAD2 (JPL,J1) = T27LOAD2(JPL,J1) * Split
      T29LOAD2 (JPL,J1) = T29LOAD2(JPL,J1) * Split
      T31LOAD2 (JPL,J1) = T31LOAD2(JPL,J1) * Split
      T33LOAD2 (JPL,J1) = T33LOAD2(JPL,J1) * Split
      T35LOAD2 (JPL,J1) = T35LOAD2(JPL,J1) * Split
      T37LOAD2 (JPL,J1) = T37LOAD2(JPL,J1) * Split
      T40LOAD2 (JPL,J1) = T40LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T42LOAD2 (JPL,J1) = T42LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T46LOAD2 (JPL,J1) = T46LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T48LOAD2 (JPL,J1) = T48LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T50LOAD2 (JPL,J1) = T50LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T52LOAD2 (JPL,J1) = T52LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T54LOAD2 (JPL,J1) = T54LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T56LOAD2 (JPL,J1) = T56LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T58LOAD2 (JPL,J1) = T58LOAD2(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      TGLODE1  (JPL,J1) = TGLODE1(JPL,J1)  * Split ! scale the loads @@@@@@@@@@@
      TRLODE1  (JPL,J1) = TRLODE1(JPL,J1)  * Split ! scale the loads @@@@@@@@@@@
      TELODE1  (JPL,J1) = TELODE1(JPL,J1)  * Split ! scale the loads @@@@@@@@@@@
      T03LOAD1 (JPL,J1) = T03LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T05LOAD1 (JPL,J1) = T05LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T12LOAD1 (JPL,J1) = T12LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T39LOAD1 (JPL,J1) = T39LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T60LOAD1 (JPL,J1) = T60LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T61LOAD1 (JPL,J1) = T61LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      T62LOAD1 (JPL,J1) = T62LOAD1(JPL,J1) * Split ! scale the loads @@@@@@@@@@@
      TDDLOAD1 (JPL,J1) = TDDLOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@@
      T13LOAD1  (JPL,J1) = T13LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T15LOAD1  (JPL,J1) = T15LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T25LOAD1  (JPL,J1) = T25LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T27LOAD1  (JPL,J1) = T27LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T29LOAD1  (JPL,J1) = T29LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T31LOAD1  (JPL,J1) = T31LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T33LOAD1  (JPL,J1) = T33LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T35LOAD1  (JPL,J1) = T35LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T37LOAD1  (JPL,J1) = T37LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T40LOAD1  (JPL,J1) = T40LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T42LOAD1  (JPL,J1) = T42LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T46LOAD1  (JPL,J1) = T46LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T48LOAD1  (JPL,J1) = T48LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T50LOAD1  (JPL,J1) = T50LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T52LOAD1  (JPL,J1) = T52LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T54LOAD1  (JPL,J1) = T54LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T56LOAD1  (JPL,J1) = T56LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      T58LOAD1  (JPL,J1) = T58LOAD1(JPL,J1)  * Split ! scale the loads @@@@@@@@@
      enddo ! J1 = 1, nx
      endif ! if ( QTYPE (JPL) .ne. 4 )
      enddo ! JPL = 1, ndet
*     77777777777777777777777777777777777777777777777777777777777777777777777777


*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! --------------------------------------------------

*     effective sampling rate --------------------------------------------------
      do JJP = 1, ndet
      QDN(JJP,IREACH) = QDN(JJP,Ireech)
      enddo

      if ( IPRINT .eq. 0 ) then ! write details of the bifurcation -------------
      if ( nobigout .le. 0 ) write(01,1003)Ireech,RNAME(Ireech),IREACH,
     &RNAME(IREACH),SPLIT
 1003 format(/24X,53('=')/
     &24X,'Start of bifurcation ---'/24X,53('=')/
     &24X,'    part of reach number',I6,' - ',A16/
     &24X,'      forms reach number',I6,' - ',A16/
     &24X,'        fraction of flow',f6.2/
     &24X,53('='))
      endif ! if ( IPRINT .eq. 0 ) ----------------------------------------------

      if ( MONF-2 )27,28,28
   28 continue
      if ( nobigout .le. 0 ) write(01,10)RNAME(IREACH)
   10 format(//120('=')/'Flow shots for the head of a bifurcation ',
     &'(reach) called:W',A16/120('='))

      if ( nobigout .le. 0 ) write(01,13)
   13 format('Monte Carlo shots for river flow ....')
      if ( nobigout .le. 0 ) write(01,14)(FMS(IS),IS = 1,NS)
   14 format(15F8.2)
      if ( nobigout .le. 0 ) write(01,15)
   15 format(120('='))

   27 if ( MONQ .gt. 1 ) then
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4) then
      if ( nobigout .le. 0 ) then
      write(01,20)RNAME(IREACH)
   20 format(//120('=')/'River quality data ',
     &'at the head of the bifurcation ...   '/
     &'River quality data for the head ',
     &'of the reach called: ',a16/120('='))
      write(01,23)
   23 format('Monte Carlo shots for river quality ....')
      write(01,24)(CMS(JPL,IS),IS = 1,NS)
   24 format(15F8.1)
      write(01,15)
      endif
      endif ! if ( QTYPE (JPL) .ne. 4)
      enddo ! do JPL = 1, ndet
      endif ! if ( MONQ .gt. 1 )

      return
      end