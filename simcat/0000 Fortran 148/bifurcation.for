*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     File BIFURCATION.FOR ... 862 lines
*     --------------------------------------------------------------------------
      subroutine bifurcation 20 to 23 (INEXT) ! Feature types: 20, 21, 22, 23
      include 'COM.FOR'
	dimension abmean(MP10),abstdev(MP10),AXLODE(MP10,13)
      dimension Y(MS)

      jtypeB = JT(INEXT) ! identify the type of bifurcation --------------------
      
*     identify the flow to be diverted into the new reach ----------------------
      IDENTIFY = JF(INEXT) ! identify the reach 
	if ( IDENTIFY .eq. 0 ) then
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
	endif
      ISTOR = IRHOLD (IDENTIFY) ! identify the reach supplying the flow --------

*     check whether arrays can be cleared --------------------------------------
      do 2296 JJ = KU,MU
	if ( jj .ne. KU ) then
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. IDENTIFY ) goto 2297
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. IDENTIFY ) goto 2297
	endif
 2296 continue
      JSTOR (ISTOR) = 0 ! bifurcation (20,21) data finished with 
      IRHOLD (Ireech) = 0 ! bifurcation (20,21) data finished with 
 2297 continue
      
*     set initial values for the shots of flow and quality ---------------------
      do is = 1, NS
      FMS (IS) = 0.0 ! flow
      Y (IS) = 0.0
      EFMS (IS) = 0.0 ! the proportion of effluent in each shot ----------------
      do JPL = 1, ndet
      CMS (JPL,IS) = 0.0 ! concentrations
      enddo ! loop on determinands
      enddo ! loop on shots
      do JPL = 1, ndet
      QDN (JPL,IREACH) = 0.0 ! effective sampling rates at ends of reaches -----
      enddo
      
*     fill arrays with the shots from the upstream reach -----------------------     
      do IS = 1,NS
      FMS(IS) = FD(ISTOR,IS) ! river flows
      EFMS(IS) = EFD(ISTOR,IS) ! proportion of effluent
      do JPL = 1, ndet ! determinands
      CMS(JPL,IS) = QD(ISTOR,JPL,IS) ! river quality
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JPL,IS) = EQD(ip,ISTOR,JPL,IS) 
	enddo
      enddo
      enddo

*     set the starting values of the loads at the head of the new reach --------
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
	nx = n13 ! month 13 - the annual results
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TGLODE2 (JPL,J1) = TGLODE3 (ISTOR,JPL,J1)   
      TRLODE2 (JPL,J1) = TRLOAD3 (ISTOR,JPL,J1)  
	TELODE2 (JPL,J1) = TELOAD3 (ISTOR,JPL,J1)   
	T03LOAD2 (JPL,J1) = T03LOAD3 (ISTOR,JPL,J1)   
	T05LOAD2 (JPL,J1) = T05LOAD3 (ISTOR,JPL,J1)   
	T12LOAD2 (JPL,J1) = T12LOAD3 (ISTOR,JPL,J1)   
	T39LOAD2 (JPL,J1) = T39LOAD3 (ISTOR,JPL,J1)   
      TDDLOAD2  (JPL,J1) = TDDLOAD3 (ISTOR,JPL,J1)   
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
      TBLOAD2 (JPL,J1) = TBLOAD3 (ISTOR,JPL,J1)   
      TGLODE1 (JPL,J1) = TGLODE4 (ISTOR,JPL,J1)   
	TRLODE1 (JPL,J1) = TRLOAD4 (ISTOR,JPL,J1)   
	TELODE1 (JPL,J1) = TELOAD4 (ISTOR,JPL,J1)   
	T03LOAD1 (JPL,J1) = T03LOAD4 (ISTOR,JPL,J1)   
	T05LOAD1 (JPL,J1) = T05LOAD4 (ISTOR,JPL,J1)   
	T12LOAD1 (JPL,J1) = T12LOAD4 (ISTOR,JPL,J1)   
	T39LOAD1 (JPL,J1) = T39LOAD4 (ISTOR,JPL,J1)   
      TDDLOAD1 (JPL,J1) = TDDLOAD4 (ISTOR,JPL,J1)   
	TALOADUP1 (JPL,J1) = TALOADUP4 (ISTOR,JPL,J1) 
      TILOADUP1 (JPL,J1) = TILOADUP4 (ISTOR,JPL,J1) 
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
      TBLOAD1 (JPL,J1) = TBLOAD4 (ISTOR,JPL,J1)   
      enddo ! do J1 = 1, nx
      if ( kount works .gt. 0 ) then
      do iworks = 1, kount works
      TELOADAV(iworks,JPL) = TELOADAVrch(iworks,ISTOR,JPL)
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      if ( JPL .eq. ndshot ) then ! 33333333333333333333333333333333333333333333
      TELOADshots(iworks,JPL,is) = TELOADrchshots(iworks,ISTOR,1,is) ! 333333333
      endif ! 333333333333333333333333333333333333333333333333333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo
 	endif
      if ( kount bodies .gt. 0 ) then
      do 2003 ibodies = 1, kount bodies
*     annual loads (i13) from upstream sub-catchments --------------------------
      TWLOADS(ibodies,JPL, i13) = TWloadsrch(ibodies,ISTOR,JPL)
      do ip = 1, nprop
*     breakdown of annual (i13) loads from upstream sub-catchments -------------
      TWLOADSapp (ibodies,JPL,i13,ip) = 
     &TWloadsrchapp (ibodies,ISTOR,JPL,ip)
      
      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      if ( JPL .eq. ndshot ) then ! 55555555555555555555555555555555555555555555
      TDLOADshots(ibodies,JPL,is,ip) = ! 555555555555555555555555555555555555555
     &TDLOADrchshots(ibodies,ISTOR,1,is,ip) ! 5555555555555555555555555555555555
      endif ! 555555555555555555555555555555555555555555555555555555555555555555
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

      enddo
 2003 continue
 	endif 
      endif
      enddo ! loop on determinands
      
      do JJP = 1, ndet
      QDN(JJP,IREACH) = QDN(JJP,IDENTIFY) ! effective sampling rate ------------
      enddo
     
	if ( nobigout .le. 0 ) then
      write(01,1003)jtypeb,IDENTIFY,RNAME(IDENTIFY),IREACH,
     &RNAME(IREACH)
      write(21,1003)jtypeb,IDENTIFY,RNAME(IDENTIFY),IREACH,
     &RNAME(IREACH)
 1003 format(/24X,53('=')/
     &24X,'Start of bifurcation (',i2,') ---'/24x,53('=')/
     &24X,'    part of reach number',I6,' - ',A16/
     &24X,'      forms reach number',I6,' - ',A16/24x,53('='))
      endif
     
      if ( nobigout .le. 0 ) then
      if ( MONF .ge. 2 ) then
      write(01,10)RNAME(IREACH)
   10 format(//150('=')/'Flow shots that will contribute to the ',
     &'bifurcation (reach) called: ',A16/150('='))
      write(01,14)(FMS(IS),IS = 1,NS)
   14 format(f10.1,20F7.1)
      write(01,15)
   15 format(150('='))
      call calculate summaries of river flow
      write(01,29)Flow(1),Flow(2)
   29 Format('Calculated mean flow   =',f8.2/
     &       '95-percentile low flow =',f8.2)       
      write(01,15)
      endif ! if ( MONF .ge. 2 )
      
      if ( MONQ .ge. 2 ) then
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4) then
      write(01,20)RNAME(IREACH)
   20 format(//150('=')/'River quality shots ',
     &'at the head of the bifurcation (reach)',
     &' called: ',a16/150('='))
      write(01,24)(CMS(JPL,IS),IS = 1,50)
   24 format(15F8.1)
      write(01,15)
      endif
      enddo
      endif ! if ( MONQ .ge. 2 )
      endif ! if ( nobigout .le. 0 )

*     initialise the diverted loads --------------------------------------------
      do jdet = 1, ndet
      do L13 = 1, N13
	NSM (jdet,L13) = 0
      AXLODE (jdet,L13) = 0.0
	enddo
      abmean (jdet) = 0.0
      abstdev (jdet) = 0.0
      enddo

*     flow diverted to the bifurcation -----------------------------------------
      IF = JQ(INEXT) ! code number for the river flow data-set -----------------
      if ( IF .lt. 1 ) then ! check for a zero code number
      if ( nobigout .le. 0 ) write(01,1020)INEXT
      if ( iscreen .lt. 3 ) write( *,1020)INEXT
      write(09,1020)INEXT
      write(33,1020)INEXT
 1020 format(/'*** Diverted flow data specified for Feature number',I6,
     &' do not exist.')
      if ( nobigout .le. 0 ) write(01,1996)
      if ( iscreen .lt. 3 ) write( *,1996)
      write(09,1996)
      write(33,1996)
 1996 format(/'*** No divertion made ***'/77('-'))
      endif ! check for a zero code number
      
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (F(IF,1),F(IF,2))
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      write(01,2012)valchars10,FUNIT,valchars11,FUNIT
      write(21,2012)valchars10,FUNIT,valchars11,FUNIT
 2012 format(/77('-')/
     &'Flow diverted to this arm of the bifurcation:',
     &11X,'Mean =',a10,1X,A4/46X,'95% exceedence =',a10,1x,a4)
      else ! river type
      write(01,2812)valchars10,FUNIT,valchars11,FUNIT
      write(21,2812)valchars10,FUNIT,valchars11,FUNIT
 2812 format(/77('-')/
     &'Flow diverted to this arm of the bifurcation:',
     &11X,'Mean =',a10,1X,A4/42X,'Standard deviation =',a10,1x,a4)
      endif ! effluent type
      if ( F(IF,2) .gt. 1.0e-09 .and. idist .eq. 3 ) then
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
      if ( nobigout .le. 0 ) write(01,8920)INEXT
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
	goto 2213
      endif
      enddo
      if ( nobigout .le. 0 ) write(01,1920)INEXT
 1920 format(/'*** Monthly abstraction data specified for ',
     &'Feature number',I6,' do not exist.')
      if ( nobigout .le. 0 ) write(01,8996)
      if ( iscreen .lt. 3 ) write( *,8996)
      write(09,8996)
      write(33,8996)
 8996 format(/'*** No addition made for the bifurcation ***'/77('-'))
      return
      endif ! monthly data on river flow ---------------------------------------
 2213 continue ! monthly data on river flow ------------------------------------

*     identify the distribution of the diverted flows --------------------------
      IFDIST = PDRF(IF)
      if ( F(IF,1) .gt. 1.0E-10 ) then ! check for zero mean flow --------------
      EFM = F(IF,1) ! set the mean abstracted flow -----------------------------
      EF5 = F(IF,2) ! set 95-percentile low abstracted flow
      EFS = F(IF,2) ! or the standard devistion of abstracted flow
      EF3 = 0.0 ! set the shift flow (for 3 parameter log-normal)
      if ( IFDIST .eq. 3 ) EF3 = F(IF,3) ! set the shift flow 
      GEFM = 0.0
      GEFS = 0.0
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log-normal case
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFS = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST) ! standard deviation
      if (GEFS .le. 0.0) then
      write(01,9099)UNAME(INEXT)
      write( *,9099)UNAME(INEXT)
      write(09,9099)UNAME(INEXT)
      write(33,9099)UNAME(INEXT)
 9099 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if (GEFS .le. 0.0)
      GEFS = SQRoot(103003,GEFS) - 1.6449 ! standard deviation (log)
      EFS = EXP(GEFS*GEFS) - 1.0 ! standard deviation
      if (EFS .le. 1.0E-10) then
      write(01,9899)UNAME(INEXT)
      write( *,9899)UNAME(INEXT)
      write(09,9899)UNAME(INEXT)
      write(33,9899)UNAME(INEXT)
 9899 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if standard deviation is zero
      EFS = EFM * SQRoot(1031,EFS) ! standard deviation
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      endif ! if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) - log normal
      endif ! river type (20 and 21)
*     ##########################################################################      

      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 99999999999999
*     compute mean and standard deviation of logged flows from the mean and 9999
*     95-percentile entered as data 99999999999999999999999999999999999999999999
      if ( IFDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      TEST = AMAX1((EF5+EF3),1.0E-8)
      GEFS = 2.7057 + 2.0 * ALOG ((EFM+EF3)/TEST) ! standard deviation
      if (GEFS .le. 0.0) then
      write(01,9299)UNAME(INEXT)
      write( *,9299)UNAME(INEXT)
      write(09,9299)UNAME(INEXT)
      write(33,9299)UNAME(INEXT)
 9299 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if (GEFS .le. 0.0)
      GEFS = SQRoot(103003,GEFS) - 1.6449 ! standard deviation (log)
      EFS = EXP(GEFS*GEFS) - 1.0 ! standard deviation
      if (EFS .le. 1.0E-10) then
      write(01,9829)UNAME(INEXT)
      write( *,9829)UNAME(INEXT)
      write(09,9829)UNAME(INEXT)
      write(33,9829)UNAME(INEXT)
 9829 format(///'### Flow error for a Feature called - ',A37/
     &          '### Simulation stopped in bifurcation 20 to 23')
      call stop
      endif ! if standard deviation is zero
      EFS = EFM * SQRoot(1031,EFS) ! standard deviation
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      endif ! if ( IFDIST .eq. 10 ) power curve parametric distribution 99999999 
      endif ! river type (20 and 21) 9999999999999999999999999999999999999999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999


      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log-normal case
      EM3 = EFM+EF3
    	if ( EM3 .gt. 1.0e-9 ) then
      GEFM = ALOG ((EM3*EM3)/SQRoot(1036,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1037,ALOG(1.+(EFS*EFS)/(EM3*EM3)))
      EFS = EXP(GEFS*GEFS) - 1.0
      if (EFS .le. 1.0E-10) then
      write(01,9899)UNAME(INEXT)
      write( *,9899)UNAME(INEXT)
      write(09,9899)UNAME(INEXT)
      write(33,9899)UNAME(INEXT)
      call stop
      endif ! if (EFS .le. 1.0E-10)
      endif ! if ( EM3 .gt. 1.0e-9 )
      endif ! if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) - log normal
      endif ! if ( jtypeB .eq. 22) ... effluent type

      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 99999999999
      if ( IFDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      EM3 = EFM+EF3
    	if ( EM3 .gt. 1.0e-9 ) then
      GEFM = ALOG ((EM3*EM3)/SQRoot(1036,EM3*EM3+EFS*EFS))
      GEFS = SQRoot(1037,ALOG(1.+(EFS*EFS)/(EM3*EM3)))
      EFS = EXP(GEFS*GEFS) - 1.0
      if (EFS .le. 1.0E-10) then
      write(01,9899)UNAME(INEXT)
      write( *,9899)UNAME(INEXT)
      write(09,9899)UNAME(INEXT)
      write(33,9899)UNAME(INEXT)
      call stop
      endif ! if (EFS .le. 1.0E-10)
      endif ! if ( EM3 .gt. 1.0e-9 )
      endif ! if ( IFDIST .eq. 10 ) power curve parametric distribution 99999999
      endif ! if ( jtypeB .eq. 22) ... effluent type 999999999999999999999999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999
      
      endif ! if ( F(IF,1) .gt. 1.0E-10 ) 
*     ##########################################################################      

      
      if ( IFDIST .eq. 1 ) then ! normal distribution for bifurcation ----------
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 
      GEFM = EFM ! mean
      GEFS = (EFM-EF5) / 1.6449 ! standard deviation
      EFS = GEFS
      endif ! river type 
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 
      GEFM = EFM ! mean
      GEFS = EFS ! standard deviation
      endif ! effluent type 
      endif ! normal distribution ----------------------------------------------

      if ( IFDIST .eq. 0 ) then ! constant flow --------------------------------
      GEFM = EFM ! mean
      GEFS = 0.0 ! zero standard deviation
      EFS = GEFS
      endif ! if ( IFDIST .eq. 0 ) constant flow -------------------------------

      CO2 = 1.0 ! correlation of abstracted flow on main river flow ------------
      if ( F(IF,MO) .ne. -9.9 ) CO2 = F (IF,MO) ! special correlation
      
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then ! log normal distribution ---
      RFX3 = F(IF,3)
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      call BIASEL bifurcation (1, GEFM, GEFS, RFX3) ! bias
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal river flows           
      endif ! river type -------------------------------------------------------
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type ===========
      call BIASEL bifurcation (0, GEFM, GEFS, RFX3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal birfurcation         
      endif ! effluent type ====================================================
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! if ( IFDIST .eq. 2) log normal distribution ----------------------

      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( IFDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      RFX3 = F(IF,3)
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 99999999999999
      call BIASEL bifurcation (1, GEFM, GEFS, RFX3) ! bias
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal river flows           
      endif ! river type 9999999999999999999999999999999999999999999999999999999
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 99999999999
      call BIASEL bifurcation (0, GEFM, GEFS, RFX3)
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate log normal birfurcation         
      endif ! effluent type 9999999999999999999999999999999999999999999999999999
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! if ( IFDIST .eq. 10 ) power curve parametric distribution 99999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999

      if ( IFDIST .eq. 1) then ! normal distribution ---------------------------
      RFX3 = F(IF,3)
      call bias in normal tributary flows (GEFM, GEFS) 
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type 
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate normal river flows         
      endif ! river type 
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type 
      if ( BM(3) .gt. 1.0E-08 ) BM(3)=EFM/BM(3) ! bias in mean
      if ( BS(3) .gt. 1.0E-08 ) BS(3)=EF5/BS(3) ! bias in Q95
      call generate normal effluent flows         
      endif ! effluent type 
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)
      endif ! normal distribution ----------------------------------------------
      
      if ( PDRF(IF) .eq. 4 ) then ! non-parametric distribution ----------------
      call set up the shots for non parametric stream flow
      endif ! non-parametric distributions -------------------------------------
      
      if ( PDRF(IF) .eq. 5 ) then ! set up the shots for monthly data ----------
	call set up monthly data for bifurcation
      endif ! monthly data -----------------------------------------------------

      call list the correlation coefficients

*     flow diverted to the bifurcation ... loop over all the shots -------------
      do IS = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
*     get the shot for the flow ... --------------------------------------------
*     impose correlation between added flow and the river flow -----------------
      R3 = CO2 * R1 + R3 * SQRMB(102, 1.0 - CO2 * CO2 )
      call get the flows of the stream or discharge ( IS, R3, EF )
*     the flow diverted to the at the head of bifurcation is EF ----------------
*     retrieve the flow that is to be split ------------------------------------
      Y(IS) = FD(ISTOR,IS)
      if ( jtypeb .eq. 21 .or. jtypeb .eq. 23 ) then
*     set the diverted flow ... make sure it is less than what is available ----
      FMS(IS) = amin1 (EF,Y(IS))
      endif ! if ( jtypeb .eq. 21 .or. jtypeb .eq. 23 )
      if ( jtypeb .eq. 20 .or. jtypeb .eq. 22) then
*     set the remaining flow ... make sure it is not less than zero ------------
      FMS(IS) = amax1 (0.0, (Y(is) - EF))
      endif ! if ( jtypeb .eq. 20 .or. jtypeb .eq. 22)
      enddo
      
      if ( MONF .gt. 1 ) call flow shots for bifurcation 20 to 23 
     &(jtypeb)

*     ##########################################################################
*     calculate the load linked to the diverted flow ---------------------------
	numflows = 0 ! counter for shots greater than zero -----------------------
      do IS = 1, NS
      if ( FMS(IS) .gt. 1.0e-10 ) numflows = numflows + 1
      imonth = qmonth (is) ! set the month for this shot
      do jdet = 1, ndet
      if ( QTYPE (jdet) .ne. 4 ) then      
      K13 = imonth + 1
      AXLODE (jdet,I13) = AXLODE (jdet,I13) + FMS (IS) * CMS(jdet,IS)
	NSM (jdet,I13) = NSM (jdet,I13) + 1
      AXLODE (jdet,K13) = AXLODE (jdet,K13) + FMS (IS) * CMS(jdet,IS)
	NSM (jdet,K13) = NSM (jdet,K13) + 1
*     prepare to calculate the mean of the diverted quality --------------------
      if ( FMS (IS) .gt. 1.0e-10 ) then
      ABMean (jdet) = ABMean (jdet) + CMS(jdet,IS)
      ABStdev(jdet) = ABStdev(jdet) + CMS(jdet,IS)*CMS(jdet,IS)
      endif
      endif
      enddo
      enddo ! loop on shots
*     calculate the added load -------------------------------------------------
      do 5 jdet = 1, ndet
      if ( QTYPE (jdet) .ne. 4 ) then      
      AXLODE (JDET,I13) = AXLODE (JDET,I13) / float(NS)
      if ( munthly structure .eq. 1 ) then ! fill monthly loads
      do J13 = 2, N13
      if ( NSM(jdet,J13) .gt. 1 ) then
      AXLODE (JDET,J13) = AXLODE (JDET,J13) / float(NSM(JDET,J13))
      endif
      enddo
	endif
      if ( ABMean(jdet) .lt.1.0e-09 ) then
      abmean (jdet) = 0.0
      abstdev(jdet) = 0.0
	else
      ABStdev(jdet) = SQRoot3(103399, (ABStdev(jdet) 
     &              - ABMean(jdet) * ABMean(jdet) / numflows )
     &              / (numflows - 1))
      ABMean (jdet) = ABMean (jdet) / numflows
      endif
	endif
    5 continue
*     ##########################################################################
      
      if ( MONF .gt. 1 ) call write shots for river flow

*     set the starting values of the loads at the head of the new reach --------
      frat = 1.0
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
      frat1 = AXLODE (JPL,i13) / TGLODE2 (JPL,i13)
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      frat = AXLODE  (JPL,J1) / TGLODE2 (JPL,J1)
      TGLODE2 (JPL,J1) = frat * TGLODE2 (JPL,J1)   
	TRLODE2 (JPL,J1) = frat * TRLODE2 (JPL,J1)  
	TELODE2 (JPL,J1) = frat * TELODE2 (JPL,J1) ! net loads from 3 12 5 and 39
	T03LOAD2 (JPL,J1) = frat * T03LOAD2 (JPL,J1)   
	T05LOAD2 (JPL,J1) = frat * T05LOAD2 (JPL,J1)   
	T12LOAD2 (JPL,J1) = frat * T12LOAD2 (JPL,J1)   
	T39LOAD2 (JPL,J1) = frat * T39LOAD2 (JPL,J1)   
      TDDLOAD2 (JPL,J1) = frat * TDDLOAD2 (JPL,J1)   
	TALOADUP2 (JPL,J1) = frat * TALOADUP2 (JPL,J1) 
      TILOADUP2 (JPL,J1) = frat * TILOADUP2 (JPL,J1) 
      T13LOAD2 (JPL,J1) = frat * T13LOAD2 (JPL,J1)   
	T15LOAD2 (JPL,J1) = frat * T15LOAD2 (JPL,J1)   
      T25LOAD2 (JPL,J1) = frat * T25LOAD2 (JPL,J1)   
	T27LOAD2 (JPL,J1) = frat * T27LOAD2 (JPL,J1)   
	T29LOAD2 (JPL,J1) = frat * T29LOAD2 (JPL,J1)   
	T31LOAD2 (JPL,J1) = frat * T31LOAD2 (JPL,J1)   
	T33LOAD2 (JPL,J1) = frat * T33LOAD2 (JPL,J1)   
	T35LOAD2 (JPL,J1) = frat * T35LOAD2 (JPL,J1)   
	T37LOAD2 (JPL,J1) = frat * T37LOAD2 (JPL,J1)   
	T40LOAD2 (JPL,J1) = frat * T40LOAD2 (JPL,J1)   
	T42LOAD2 (JPL,J1) = frat * T42LOAD2 (JPL,J1)   
	T46LOAD2 (JPL,J1) = frat * T46LOAD2 (JPL,J1)   
	T48LOAD2 (JPL,J1) = frat * T48LOAD2 (JPL,J1)   
      TBLOAD2 (JPL,J1) = frat * TBLOAD2 (JPL,J1)   
      TGLODE1 (JPL,J1) = frat * TGLODE1 (JPL,J1)   
	TRLODE1 (JPL,J1) = frat * TRLODE1 (JPL,J1)   
	TELODE1 (JPL,J1) = frat * TELODE1 (JPL,J1)   
	T03LOAD1 (JPL,J1) = frat * T03LOAD1 (JPL,J1)   
	T05LOAD1 (JPL,J1) = frat * T05LOAD1 (JPL,J1)   
	T12LOAD1 (JPL,J1) = frat * T12LOAD1 (JPL,J1)   
	T39LOAD1 (JPL,J1) = frat * T39LOAD1 (JPL,J1)   
      TDDLOAD1 (JPL,J1) = frat * TDDLOAD1 (JPL,J1)   
	TALOADUP1 (JPL,J1) = frat * TALOADUP1 (JPL,J1) 
      TILOADUP1 (JPL,J1) = frat * TILOADUP1 (JPL,J1) 
      T13LOAD1 (JPL,J1) = frat * T13LOAD1 (JPL,J1)   
	T15LOAD1 (JPL,J1) = frat * T15LOAD1 (JPL,J1)   
      T25LOAD1 (JPL,J1) = frat * T25LOAD1 (JPL,J1)   
	T27LOAD1 (JPL,J1) = frat * T27LOAD1 (JPL,J1)   
	T29LOAD1 (JPL,J1) = frat * T29LOAD1 (JPL,J1)   
	T31LOAD1 (JPL,J1) = frat * T31LOAD1 (JPL,J1)   
	T33LOAD1 (JPL,J1) = frat * T33LOAD1 (JPL,J1)   
	T35LOAD1 (JPL,J1) = frat * T35LOAD1 (JPL,J1)   
	T37LOAD1 (JPL,J1) = frat * T37LOAD1 (JPL,J1)   
	T40LOAD1 (JPL,J1) = frat * T40LOAD1 (JPL,J1)   
	T42LOAD1 (JPL,J1) = frat * T42LOAD1 (JPL,J1)   
	T46LOAD1 (JPL,J1) = frat * T46LOAD1 (JPL,J1) 
	T48LOAD1 (JPL,J1) = frat * T48LOAD1 (JPL,J1) 
      TBLOAD1 (JPL,J1) = frat * TBLOAD1 (JPL,J1)   
      if ( kount bodies .gt. 0 ) then
      do ibodies = 1, kount bodies
*     monthly loads from upstream sub-catchments -------------------------------
 	TWLOADS(ibodies,JPL,J1) = frat * TWLOADS(ibodies,JPL, J1)
      do ip = 1, nprop
*     breakdown of monthly loads from upstream sub-catchments ------------------
 	TWLOADSapp(ibodies,JPL,i13,ip) = frat * 
     &TWLOADSapp(ibodies,JPL,i13,ip)
      enddo

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      TDLOADshots(ibodies,JPL,is,ip) = ! 5555555555555555555555555555555555555555
     &frat1 * TDLOADshots(ibodies,JPL,is,ip) ! 5555555555555555555555555555555555
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

      enddo ! loop on water bodies
 	endif ! if ( kount bodies .gt. 0 )
      enddo ! loop on months
      
      if ( kount works .gt. 0 ) then
      do iworks = 1, kount works ! loop on back-tracked works
      TELOADAV(iworks,JPL) = frat1 * TELOADAV(iworks,JPL)

      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(iworks,JPL,is) = frat1 * TELOADshots(iworks,JPL,is) ! 33333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! loop on back-tracked works
 	endif ! if ( kount works .gt. 0 ) then
      endif ! if ( QTYPE (JPL) .ne. 4 )
      enddo ! loop on determinands
      
      if ( IPRINT .eq. 1 .or. JSKIP .eq. 1 ) return

      call calculate summaries of river flow
	xnflow = 100.0 * numflows / NS
      if ( xnflow .lt. 99.999 ) then
      if ( nobigout .le. 0 ) write(01,1813)xnflow
 1813 format(77('-')/'Time flows are in operation',41x,'=',F6.1,
     &' %'/77('-'))
      endif

*     identify the code number for the reach providing the river quality -------
      IQB = JF(INEXT)
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
      enddo

*     call write data for graph plotting

*     calculate loads and the summary statistics of load -----------------------
      call load calculation
*     accumulate the river derived load ----------------------------------------
      call add up all the loads

      return
      end

      
      
*     bifurcation ... feature type 11 ------------------------------------------
      subroutine bifurcation 11 (KU)
      include 'COM.FOR'

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
      Ireech = JF(KU)
      if ( IRHOLD (Ireech) .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1005)
      write(09,1005)KU,Ireech,IRHOLD(Ireech)
      write(33,1005)KU,Ireech,IRHOLD(Ireech)
      write( *,1005)KU,Ireech,IRHOLD(Ireech)
 1005 format(/120('*')/
     &'*** No data for the head of a Bifurcation (Type 11) ...'/
     &'*** Calculations stopped ... KU,Ireech,IRHOLD(I)',3i6/120('*')/)
      call stop
      endif

      ISTOR = IRHOLD (Ireech)

      do 1 IS = 1,NS
*     river flows --------------------------------------------------------------
      FMS (IS) = SPLIT * FD (ISTOR,IS)
*     proportion of effluent ---------------------------------------------------
      EFMS(IS) = EFD(ISTOR,IS)
*     river quality ------------------------------------------------------------
      do JPL = 1, ndet
      CMS(JPL,IS) = QD(ISTOR,JPL,IS)
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JPL,IS) = EQD(ip,ISTOR,JPL,IS)
	enddo
      enddo
    1 continue

*     check whether arrays can be cleared --------------------------------------
      do 2296 JJ = KU,MU
	if ( jj .ne. KU ) then
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. Ireech ) goto 2297
	endif
 2296 continue
      JSTOR (ISTOR) = 0 ! bifurcation (11) data finished with 
      IRHOLD (Ireech) = 0
 2297 continue

*     compute loads at the head of the new reach -------------------------------
      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TGLODE2 (JPL,J1) = TGLODE3 (ISTOR,JPL,J1)   
	TRLODE2 (JPL,J1) = TRLOAD3 (ISTOR,JPL,J1)  
*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
	TELODE2 (JPL,J1) = TELOAD3 (ISTOR,JPL,J1)   
	T03LOAD2 (JPL,J1) = T03LOAD3 (ISTOR,JPL,J1)   
	T05LOAD2 (JPL,J1) = T05LOAD3 (ISTOR,JPL,J1)   
	T12LOAD2 (JPL,J1) = T12LOAD3 (ISTOR,JPL,J1)   
	T39LOAD2 (JPL,J1) = T39LOAD3 (ISTOR,JPL,J1)   
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
      TBLOAD2 (JPL,J1) = TBLOAD3 (ISTOR,JPL,J1)   
      TGLODE1 (JPL,J1) = TGLODE4 (ISTOR,JPL,J1)   
	TRLODE1 (JPL,J1) = TRLOAD4 (ISTOR,JPL,J1)   
	TELODE1 (JPL,J1) = TELOAD4 (ISTOR,JPL,J1)   
	T03LOAD1 (JPL,J1) = T03LOAD4 (ISTOR,JPL,J1)   
	T05LOAD1 (JPL,J1) = T05LOAD4 (ISTOR,JPL,J1)   
	T12LOAD1 (JPL,J1) = T12LOAD4 (ISTOR,JPL,J1)   
	T39LOAD1 (JPL,J1) = T39LOAD4 (ISTOR,JPL,J1)   
      TDDLOAD1 (JPL,J1) = TDDLOAD4 (ISTOR,JPL,J1)   
	TALOADUP1 (JPL,J1) = TALOADUP4 (ISTOR,JPL,J1) 
      TILOADUP1 (JPL,J1) = TILOADUP4 (ISTOR,JPL,J1) 
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
      TBLOAD1   (JPL,J1) = TBLOAD4   (ISTOR,JPL,J1)   
      enddo

      if ( kount works .gt. 0 ) then
      do iworks = 1, kount works
      TELOADAV(iworks,JPL) = TELOADAVrch(iworks,ISTOR,JPL) * split
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(iworks,JPL,is) = TELOADshots(iworks,JPL,is) * split ! 33333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! do iworks = 1, kount works
	endif ! if ( kount works .gt. 0 )
      
      if ( kount bodies .gt. 0 ) then
      do 5400 ibodies = 1, kount bodies
*     annual (i13) loads from upstream sub-catchments --------------------------
      TWLOADS(ibodies,JPL, i13) = TWloadsrch(ibodies,ISTOR,JPL) * split
      do ip = 1, nprop
*     breakdown of annual (i13) loads from upstream sub-catchments -------------
	TWLOADSapp(ibodies,JPL, i13, ip) = 
     &TWloadsrchapp(ibodies,ISTOR,JPL, ip) * split
      enddo

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      do ip = 1, nprop
      TDLOADshots(ibodies,JPL,is,ip) = ! 555555555555555555555555555555555555555
     &TDLOADshots(ibodies,JPL,is,ip) * split ! 555555555555555555555555555555555
      enddo
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

 5400 continue
	endif
      
      endif
      enddo

      do JPL = 1, ndet
      if ( QTYPE (JPL) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TGLODE2 (JPL,J1) = TGLODE2(JPL,J1) * Split
      TRLODE2 (JPL,J1) = TRLODE2(JPL,J1) * Split
*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      TELODE2 (JPL,J1) = TELODE2(JPL,J1) * Split
      T03LOAD2 (JPL,J1) = T03LOAD2(JPL,J1) * Split
      T05LOAD2 (JPL,J1) = T05LOAD2(JPL,J1) * Split
      T12LOAD2 (JPL,J1) = T12LOAD2(JPL,J1) * Split
      T39LOAD2 (JPL,J1) = T39LOAD2(JPL,J1) * Split
      TDDLOAD2 (JPL,J1) = TDDLOAD2(JPL,J1) * Split
      TALOADUP2 (JPL,J1) = TALOADUP2(JPL,J1) * Split
      TILOADUP2 (JPL,J1) = TILOADUP2(JPL,J1) * Split 
      T13LOAD2  (JPL,J1) = T13LOAD2(JPL,J1)  * Split
      T15LOAD2  (JPL,J1) = T15LOAD2(JPL,J1)  * Split
      T25LOAD2  (JPL,J1) = T25LOAD2(JPL,J1)  * Split
      T27LOAD2  (JPL,J1) = T27LOAD2(JPL,J1)  * Split
      T29LOAD2  (JPL,J1) = T29LOAD2(JPL,J1)  * Split
      T31LOAD2  (JPL,J1) = T31LOAD2(JPL,J1)  * Split
      T33LOAD2  (JPL,J1) = T33LOAD2(JPL,J1)  * Split
      T35LOAD2  (JPL,J1) = T35LOAD2(JPL,J1)  * Split
      T37LOAD2  (JPL,J1) = T37LOAD2(JPL,J1)  * Split
      T40LOAD2  (JPL,J1) = T40LOAD2(JPL,J1)  * Split
      T42LOAD2  (JPL,J1) = T42LOAD2(JPL,J1)  * Split
      T46LOAD2  (JPL,J1) = T46LOAD2(JPL,J1)  * Split
      T48LOAD2  (JPL,J1) = T48LOAD2(JPL,J1)  * Split
      TGLODE1   (JPL,J1) = TGLODE1(JPL,J1)   * Split
      TRLODE1   (JPL,J1) = TRLODE1(JPL,J1)   * Split
      TELODE1   (JPL,J1) = TELODE1(JPL,J1)   * Split
      T03LOAD1 (JPL,J1) = T03LOAD1(JPL,J1) * Split
      T05LOAD1 (JPL,J1) = T05LOAD1(JPL,J1) * Split
      T12LOAD1 (JPL,J1) = T12LOAD1(JPL,J1) * Split
      T39LOAD1 (JPL,J1) = T39LOAD1(JPL,J1) * Split
      TDDLOAD1 (JPL,J1) = TDDLOAD1(JPL,J1)  * Split
      TALOADUP1 (JPL,J1) = TALOADUP1(JPL,J1) * Split
      TILOADUP1 (JPL,J1) = TILOADUP1(JPL,J1) * Split 
      T13LOAD1  (JPL,J1) = T13LOAD1(JPL,J1)  * Split
      T15LOAD1  (JPL,J1) = T15LOAD1(JPL,J1)  * Split
      T25LOAD1  (JPL,J1) = T25LOAD1(JPL,J1)  * Split
      T27LOAD1  (JPL,J1) = T27LOAD1(JPL,J1)  * Split
      T29LOAD1  (JPL,J1) = T29LOAD1(JPL,J1)  * Split
      T31LOAD1  (JPL,J1) = T31LOAD1(JPL,J1)  * Split
      T33LOAD1  (JPL,J1) = T33LOAD1(JPL,J1)  * Split
      T35LOAD1  (JPL,J1) = T35LOAD1(JPL,J1)  * Split
      T37LOAD1  (JPL,J1) = T37LOAD1(JPL,J1)  * Split
      T40LOAD1  (JPL,J1) = T40LOAD1(JPL,J1)  * Split
      T42LOAD1  (JPL,J1) = T42LOAD1(JPL,J1)  * Split
      T46LOAD1  (JPL,J1) = T46LOAD1(JPL,J1)  * Split
      T48LOAD1  (JPL,J1) = T48LOAD1(JPL,J1)  * Split
	enddo ! J1 = 1, nx
	endif ! if ( QTYPE (JPL) .ne. 4 )
	enddo ! JPL = 1, ndet

*     calculate loads and the summary statistics of load -----------------------
      call load calculation

*     effective sampling rate --------------------------------------------------
      do JJP = 1, ndet
      QDN(JJP,IREACH) = QDN(JJP,Ireech)
	enddo

      if ( IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) write(01,1003)Ireech,RNAME(Ireech),IREACH,
     &RNAME(IREACH),SPLIT
	if ( nobigout .le. 0 ) write(21,1003)Ireech,RNAME(Ireech),IREACH,
     &RNAME(IREACH),SPLIT
 1003 format(/24X,53('=')/
     &24X,'Start of bifurcation ---'/24X,53('=')/
     &24X,'    part of reach number',I6,' - ',A16/
     &24X,'      forms reach number',I6,' - ',A16/
     &24X,'        fraction of flow',f6.2/
     &24X,53('='))
      endif

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
	endif
      enddo
      endif

      return
      end




