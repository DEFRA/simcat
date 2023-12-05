*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ....
*     ==========================================================================
*     File: apportion pollution.for ... 2575 lines -----------------------------
*     --------------------------------------------------------------------------
*     This file deals with the breakdown of pollution between sources ---------- 
*     --------------------------------------------------------------------------
*     This file contains sub-routines called
*     --------------------------------------------------------------------------
*          SUBROUTINE proportion of works ==================================== 1
*          Calculate the contributions from individual effluent discharges ...
*          ---------------------------------------------------------------------
*          SUBROUTINE proportion of catchments =============================== 2
*          Calculate the contributions from individual catchments ...
*          =====================================================================

      subroutine proportion of works (kdirect)
      include 'COMMON DATA.FOR'
      character *10 TenAV(MP10),TenAVT(MP10),TenTE(MP10),
     &Ten50 (MP10), ! ----------------------------- apportionment of percentiles
     &Ten90 (MP10), ! ----------------------------- apportionment of percentiles
     &Ten95 (MP10),Ten98 (MP10),Ten99 (MP10),Ten995(MP10),Ten999(MP10), ! ------
     &Ten50t(MP10), ! ----------------------------- apportionment of percentiles
     &Ten90t(MP10), ! ----------------------------- apportionment of percentiles
     &Ten95t(MP10),Ten98t(MP10),Ten99t(MP10),Ten995t(MP10),Ten999t(MP10) ! -----
      
      real total effluent prop (MP10), total effluent load (MP10)
      real xworksAV(MP10),xworksTE(MP10),xworks(MP10)
      real jdentify werks (NUED)
      integer feetfeet
      
*     the following types of Features are not included in the breakdown:
      goto 9843
      if ( JT (feeture) .eq. 06 ) return ! plotting point
      if ( JT (feeture) .eq. 25 ) return ! diffuse pollution from livestock (25)
      if ( JT (feeture) .eq. 27 ) return ! diffuse pollution from arable (27)
      if ( JT (feeture) .eq. 29 ) return ! highway runoff (29)
      if ( JT (feeture) .eq. 31 ) return ! diffuse pollution from urban runoff (31)
      if ( JT (feeture) .eq. 33 ) return ! atmosphere deposition (33)
      if ( JT (feeture) .eq. 35 ) return ! diffuse pollution from natural background (35)
      if ( JT (feeture) .eq. 37 ) return ! diffuse pollution from septic tanks (37)
      if ( JT (feeture) .eq. 39 ) return ! point discharges from mine waters (39)
      if ( JT (feeture) .eq. 40 ) return ! aggregate CSOs (40)
      if ( JT (feeture) .eq. 42 ) return ! aggregated STWs (42)
      if ( JT (feeture) .eq. 48 ) return ! birds, boats and angling (48)
      if ( JT (feeture) .eq. 50 ) return ! user-named diffuse pollution (50)
      if ( JT (feeture) .eq. 52 ) return ! user-named diffuse pollution (52)
      if ( JT (feeture) .eq. 54 ) return ! user-named diffuse pollution (54)
      if ( JT (feeture) .eq. 56 ) return ! user-named diffuse pollution (56)
      if ( JT (feeture) .eq. 58 ) return ! user-named diffuse pollution (58)
 9843 continue
      
*     kdirect = 0 .... the start of the reach
*     kdirect = 1 .... at feature
*     kdirect = 3 .... the end of the reach
*     kdirect = 9 .... the end of the model

      if ( ical13 .eq. 1 ) return
      unamex = 'nothing'
      if ( kdirect .eq. 1 ) feetfeet = feeture
      if ( kdirect .eq. 1 ) unamex = uname(feeture)
      if ( kdirect .eq. 9 ) unamex = 'End of the model ...'
	if ( kdirect .eq. 0 ) then
      write(unamex,1700)rname(IREACH)
 1700 format('start of reach: ',a16)
      jxtype = 111
      endif
	if ( kdirect .eq. 3 ) then
      write(unamex,1701)rname(IREACH)
 1701 format('End of reach: ',a16)
      jxtype = 999
      endif
      ihead = 0

*     ==========================================================================
*     STORED CONTRIBUTIONS OF POLLUTION ========================================
*     ==========================================================================
*     ( 1) = 'Mean from all discharges (3,12,5,39) and (60,61)'
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
*     (16) = 'Birds, boats and angling (48)'
*     (23) = 'User-named (50)'
*     (24) = 'User-named (52)'
*     (25) = 'User-named (54)'
*     (26) = 'User-named (56)'
*     (27) = 'User-named (58)'
*     (28) = '(60)'
*     (29) = '(61)'
*     ==========================================================================

      tot12AV = 0.0
      tot1250 = 0.0
      tot1290 = 0.0
      tot1295 = 0.0
      tot1298 = 0.0
      tot1299 = 0.0
      tot12995 = 0.0
      tot12999 = 0.0
      tot03AV = 0.0
      tot0350 = 0.0
      tot0390 = 0.0
      tot0395 = 0.0
      tot0398 = 0.0
      tot0399 = 0.0
      tot03995 = 0.0
      tot03999 = 0.0
      
      do kdet = 1, mp10
      total effluent load (kdet) = 0.0 ! sum of xworksAV (% of total)
      total effluent prop (kdet) = 0.0 ! sum of xworksTE (TELOADAV
      enddo      
      
*     loop on the number of upstream works WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      do 1000 iworks = 1, kountworks ! WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      iline = 0
      jworks = identify werks (iworks)
      jdentify werks (jworks) = iworks
      xworksd = 0.0

      do idet = 1, MP10 ! ......................................................
      xworksAV (idet) = 0.0 ! 100.0*TELOADAV(works)/TGLODE2(i13) of total load 
      xworksTE (idet) = 0.0 ! 100.0*TELOADAV(works)/TELODE2(i13) effluent load
      xworks (idet) = 0.0
          
*     initialise the proportion of effluent ------------------------------------
      TenAV(idet)   = '     .....' ! % mean of the total load
      TenAVT(idet)  = '     .....' ! % of the total load
      TenTE(idet)   = '     .....' ! % of the total effluent load
      
      Ten50(idet)   = '     .....' ! % load for 50-percentile in the river
      Ten90(idet)   = '     .....' ! % load for 90-percentile in the river
      Ten95(idet)   = '     .....' ! % load for 95-percentile in the river
      Ten98(idet)   = '     .....' ! % load for 98-percentile in the river
      Ten99(idet)   = '     .....' ! % load for 99-percentile in the river
      Ten995(idet)  = '     .....' ! % load for 99.5-percentile in the river
      Ten999(idet)  = '     .....' ! % load for 99.9-percentile in the river
      Ten50t(idet)  = '     .....' ! total 50-percentile load in the river
      Ten90t(idet)  = '     .....' ! total 90-percentile load in the river
      Ten95t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten98t(idet)  = '     .....' ! total 98-percentile load in the river
      Ten99t(idet)  = '     .....' ! total 99-percentile load in the river
      Ten995t(idet) = '     .....' ! total 99.5-percentile load in the river
      Ten999t(idet) = '     .....' ! total 99.9-percentile load in the river
      enddo ! do idet = 1, MP10 ! ..............................................

*     loop on the determinands .................................................
      ndet2 = 0 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      do 1001 idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++
	if ( QTYPE (idet) .eq. 4 ) goto 1001 ! +++++++++++++++++++++++++++++++++++
      ndet2 = ndet2 + 1
      xworksAV (idet) = 0.0 ! % from works of the total load in the river ------
      
	if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then ! ............................
      xworksAV(idet) = 100.0*TELOADAV(iworks,idet)/TGLODE2(idet,i13) ! ---------
      
      if ( n251 .eq. 1 ) then ! PPPPPPPPPPPPPPPPPPP apportionment of percentiles   
      if ( ndshot .eq. idet ) then ! PPPPPPPPPPPPPP apportionment of percentiles    
      call identify the shots for percentiles (idet) ! PPPPPPPPPP of percentiles
      endif !  if ( ndshot .eq. idet ) PPPPPPPPPPPP apportionment of percentiles
      endif ! if ( n251 .eq. 1 PPPPPPPPPPPPPPPPPPPP apportionment of percentiles

      total effluent prop (idet) = total effluent prop (idet) 
     &+ xworksAV(idet)
      
      if ( JT(jworks) .eq. 12 ) then ! ---------------------------- intermittent 
      tot12AV = tot12AV + xworksAV(idet)
      endif ! intermittent 
      if ( JT(jworks) .eq. 3 ) then ! ----------------------------- sewage works  
      tot03AV = tot03AV + xworksAV(idet)
      endif
      call sort format 2 (xworksAV(idet),TGLODE2(idet,i13))
      TenAV(idet) = valchars10 ! xworksAV(idet)
	TenAVT(idet) = valchars11 ! TGLODE2(idet,i13)
      
      xworksTE (idet) = 0.0 ! --------------------- % of the total effluent load
      
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksTE(idet) = 100.0*TELOADAV(iworks,idet)/TELODE2(idet,i13)
	total effluent load (idet) = total effluent load (idet)
*    & + TELOADAV(iworks,idet)
     & + xworksTE(idet)
      
	xworksd = amax1 ( xworksd, xworksTE(idet) )
      call sort format 1 (xworksTE(idet))
	TenTE(idet) = valchars10
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 )
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) ...........................
  
      
      
*     apportionment of percentiles PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      if ( n251 .eq. 1 .and. idet .eq. ndshot ) then ! PPPPPPPPPPPPPPPPPPPPPPPPP
          
      XXLD = CMS(idet,JS50) * FMS(JS50) ! apportionment of percentiles ----- Q50
      xworks (idet) = 0.0 ! % of load for the 50-percentile in the river --- Q50
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS50)/XXLD ! ------------ Q50
      if ( JT(jworks) .eq. 12 ) then ! intermittent PPPPPPPPPPPPPPPPPPPPPPPP Q50
      tot1250 = tot1250 + xworks(idet) ! apportionment of percentiles ------ Q50
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works ------------------------- Q50
      tot0350 = tot0350 + xworks(idet) ! apportionment of percentiles ------ Q50
      endif
      call sort format 2 (xworks(idet),XXLD)
      if ( idet .eq. ndshot ) x50works = xworks(idet)
      Ten50(idet) = valchars10 ! apportionment of percentiles -------------- Q50
      Ten50t(idet) = valchars11 ! apportionment of percentiles ------------- Q50
      endif ! if ( XXLD .gt. 1.0e-15 ) ! apportionment of percentiles ------ Q50

      XXLD = CMS(idet,JS90) * FMS(JS90) ! apportionment of percentiles ----- Q90
      xworks (idet) = 0.0 ! % of load for the 90-percentile in the river --- Q90
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS90)/XXLD ! ------------ Q90
      if ( JT(jworks) .eq. 12 ) then ! intermittent ------------------------ Q90
      tot1290 = tot1290 + xworks(idet) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Q90
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works PPPPPPPPPPPPPPPPPPPPPPPPP Q90
      tot0390 = tot0390 + xworks(idet) ! apportionment of percentiles ------ Q90
      endif
      call sort format 2 (xworks(idet),XXLD)
      if ( idet .eq. 5 ) x90works = xworks(idet)
      Ten90(idet) = valchars10 ! apportionment of percentiles -------------- Q90
      Ten90t(idet) = valchars11 ! apportionment of percentiles ------------- Q90
      endif ! if ( XXLD .gt. 1.0e-15 ) ! apportionment of percentiles ------ Q90
      
      XXLD = CMS(idet,JS95) * FMS(JS95) ! apportionment of percentiles ----- Q95
      xworks (idet) = 0.0 ! % of load for the 95-percentile in the river --- Q95
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS95)/XXLD ! ------------ Q95
      if ( JT(jworks) .eq. 12 ) then ! intermittent PPPPPPPPPPPPPPPPPPPPPPPP Q95
      tot1295 = tot1295 + xworks(idet) ! apportionment of percentiles ------ Q95
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works ------------------------- Q95
      tot0395 = tot0395 + xworks(idet) ! apportionment of percentiles ------ Q95
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten95(idet) = valchars10 ! apportionment of percentiles -------------- Q95
	Ten95t(idet) = valchars11 ! apportionment of percentiles ------------- Q95
      endif ! if ( XXLD .gt. 1.0e-15 )! apportionment of percentiles ------- Q95 

      XXLD = CMS(idet,JS98) * FMS(JS98) ! apportionment of percentiles ----- Q98
      xworks (idet) = 0.0 ! % of load for the 98-percentile in the river --- Q98
 	if ( XXLD .gt. 1.0e-15 ) then ! apportionment of percentiles --------- Q98
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS98)/XXLD ! ------------ Q98
      if ( JT(jworks) .eq. 12 ) then ! intermittent ------------------------ Q98
      tot1298 = tot1298 + xworks(idet) ! apportionment of percentiles ------ Q98
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works ------------------------- Q98
      tot0398 = tot0398 + xworks(idet) ! apportionment of percentiles ------ Q98
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten98(idet) = valchars10 ! apportionment of percentiles -------------- Q98
	Ten98t(idet) = valchars11 ! apportionment of percentiles ------------- Q98
      endif ! if ( XXLD .gt. 1.0e-15 ) apportionment of percentiles -------- Q98

      XXLD = CMS(idet,JS99) * FMS(JS99) ! apportionment of percentiles ----- Q99
      xworks (idet) = 0.0 ! % of load for the 99-percentile in the river --- Q98
 	if ( XXLD .gt. 1.0e-15 ) then ! apportionment of percentiles --------- Q99
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS99)/XXLD ! ------------ Q98
      if ( JT(jworks) .eq. 12 ) then ! intermittent ------------------------ Q98
      tot1299 = tot1299 + xworks(idet) ! apportionment of percentiles ------ Q99
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works PPPPPPPPPPPPPPPPPPPPPPPPP Q99
      tot0399 = tot0399 + xworks(idet) ! apportionment of percentiles ------ Q99
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten99(idet) = valchars10 ! apportionment of percentiles -------------- Q99
	Ten99t(idet) = valchars11 ! apportionment of percentiles ------------- Q99
      endif ! if ( XXLD .gt. 1.0e-15 ) apportionment of percentiles -------- Q99
      
      XXLD = CMS(idet,JS995) * FMS(JS995) ! apportionment of percentiles -- Q995
      xworks (idet) = 0.0 ! % of load for 99.5-percentile in the river ---- Q995
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS995)/XXLD ! ---------- Q995
      if ( JT(jworks) .eq. 12 ) then ! intermittent ----------------------- Q995
      tot12995 = tot12995 + xworks(idet) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Q995
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works PPPPPPPPPPPPPPPPPPPPPPPP Q995
      tot03995 = tot03995 + xworks(idet) ! apportionment of percentiles --- Q995
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten995(idet) = valchars10 ! apportionment of percentiles ------------ Q995
	Ten995t(idet) = valchars11 ! apportionment of percentiles ----------- Q995
      endif ! if ( XXLD .gt. 1.0e-15 ) ! apportionment of percentiles ----- Q995

      XXLD = CMS(idet,JS999) * FMS(JS999) ! apportionment of percentiles -- Q999
      xworks (idet) = 0.0 ! % of load for 99.9-percentile in the river ---- Q999
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS999)/XXLD ! ---------- Q999
      if ( JT(jworks) .eq. 12 ) then ! intermittent ----------------------- Q999
      tot12999 = tot12999 + xworks(idet) ! apportionment of percentiles --- Q999
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works ------------------------ Q999
      tot03999 = tot03999 + xworks(idet) ! apportionment of percentiles --- Q999
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten999(idet) = valchars10 ! apportionment of percentiles ------------ Q999
	Ten999t(idet) = valchars11 ! apportionment of percentiles ----------- Q999
      endif ! if ( XXLD .gt. 1.0e-15 ) ! apportionment of percentiles ----- Q999
      endif ! if ( n251 .eq. 1 .and. idet .eq. ndshot ) ! ----------------- Q999
*     PPPPPPPPPPPPPPPPPP apportionment of percentiles PPPPPPPPPPPPPPPPPPPPPPPPPP

      
*     check that the percentage load does not exceed 100 #######################
	if ( xworksTE (idet) .gt. 100.0001 ) then ! ##############################
	call change colour of text (20) ! bright red ... error message -----------
	write( *,7300)uname(jworks) ! error message ------------------------------
      call set screen text colour ! error message ------------------------------
	write(01,7300)uname(jworks) ! error message ----------------------file OUT
	write(09,7300)uname(jworks) ! error message ----------------------file SCN
	write(33,7300)uname(jworks) ! error message ----------------------file ERR
      write(120+idet,7300)uname(jworks) ! error message ----------------- Ai.ADL
 7300 format(/77('-')/ ! error message -----------------------------------------
     &'ERROR in the calculation of loads attributable to ', ! error message ----
     &'effluents ... '/ ! error message ----------------------------------------
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) ! ----------------------
	write(01,7000)TELOADAV(iworks,idet),dname(idet) ! error message --file OUT
	write(09,7000)TELOADAV(iworks,idet),dname(idet) ! error message --file SCN
      write(120+idet,7000)TELOADAV(iworks,idet),dname(idet) ! error ----- Ai.ADL
	write(33,7000)TELOADAV(iworks,idet),dname(idet) ! ----------------file ERR
 7000 format('   Load from works =',f15.6,' for ',a11) ! -----------------------
	write(01,7001)TELODE2(idet,i13) ! error message ------------------file OUT
	write(09,7001)TELODE2(idet,i13) ! error message ------------------file SCN
      write(120+idet,7001)TELODE2(idet,i13) ! load from works ----------- Ai.ADL
	write(33,7001)TELODE2(idet,i13) ! --------------------------------file ERR
 7001 format('    Total net load =',f15.6) ! error message -------------file OUT
	write(01,7002)xworksTE(idet) ! error message ---------------------file OUT
	write(09,7002)xworksTE(idet) ! error message ---------------------file SCN
      write(120+idet,7002)xworksTE(idet) ! ------------------------------ Ai.ADL
	write(33,7002)xworksTE(idet) !  ! --------------------------------file ERR
 7002 format('        Percentage =',f15.6/77('-')) ! ---------------------------
      call stop ! ##############################################################
      endif ! if ( xworksTE (idet) .gt. 100.0001 ) #############################

      
      
 1001 continue ! end of loop on determinands +++++++++++++++++++++++++++++++++++

 	if ( xworksd .gt. 0.00001 ) then ! ---------------------------------------
      iline = 1 ! --------------------------------------------------------------
 	if ( ihead .ne. 2 ) ihead = 1 ! ------------------------------------------
      endif ! if ( xworksd .gt. 0.00001 ) --------------------------------------
      
*     write heading ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( ihead .eq. 1 ) then ! +++++++++++++++++++++++++++++++++++++++++++++++
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      do idet = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( qtype(idet) .ne. 4 ) then ! +++++++++++++++++++++++++++++++++++++++++
      if ( kdirect .eq. 9 .or. kdirect .eq. 3 ) then
      write(120+idet,23)unamex ! heading -------------------------------- Ai.ADL
   23 format(/140('=')/'At: ',a37/140('=')) ! --------------------------- Ai.ADL
      else
      write(120+idet,2)unamex,dist(feeture),rname(ireach) ! ------------- Ai.ADL
    2 format(/140('-')/'At Feature: ',a37,30x,f7.1,2x, ! ---------------- Ai.ADL
     &'kilometres from the head of Reach: ',a16/140('-')) ! ----- ------- Ai.ADL
      endif
      endif ! if ( qtype(idet) .ne. 4 ) then ! +++++++++++++++++++++++++++++++++
      enddo ! do idet = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++
  
      
      
      if ( n251 .eq. 1 ) then ! PPPPPPPPP apportionment of percentiles PPPPPPPPP
      do idet = 1, ndet ! PPPPPPPPPPP apportionment of percentiles PPPPPPPPPPPPP
      if ( QTYPE (idet) .ne. 4 ) then ! PPPPP apportionment of percentiles PPPPP
      if ( kdirect .ne. 1 ) then ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(140+idet,4843)unamex ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4843 format(//140('=')/'The percentage of percentile concentrations ', ! Ai ADC
     &'supplied by individual discharges at the ',a37/140('-')) ! +++++++ Ai.ADC
      else
      write(140+idet,4443)unamex,rname(ireach) ! PPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4443 format(//140('-')/'The percentage of percentile concentrations ', ! -- ADC
     &'supplied by individual discharges at: ',a37,3x,'on Reach: ',a16/ ! -- ADC
     &140('-')) ! ------------------------------------------------+++++++ Ai.ADC
      endif
      write(140+idet,8776) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 8776 format(46x,'mean   50%tile   90%tile   95%tile   98%tile', ! ------ Ai.ADC
     &'   99%tile 99.5%tile 99.9%tile') ! ------------------------+++++++ Ai.ADC
      write(140+idet,7654) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 7654 format(140('-')) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
*     write(140+idet,4394)JS50,JS90,JS95,JS98,JS99,JS995,JS999 ! -+++++++ Ai.ADC
*4394 format('Monte Carlo shot',33x,'-',10i10) ! -----------------+++++++ Ai.ADC
*     write(140+idet,4294)FLOW(1),FMS(JS50),FMS(JS90),FMS(JS95), ! ------ Ai.ADC
*    &FMS(JS98),FMS(JS99),FMS(JS995),FMS(JS999) ! ----------------+++++++ Ai.ADC
*4294 format('River flow',30x,10f10.2) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,4494)dname(idet),C(idet,1),CMS(idet,JS50), ! PPPPPPP Ai.ADC
     &CMS(idet,JS90),CMS(idet,JS95),CMS(idet,JS98), ! ------------+++++++ Ai.ADC
     &CMS(idet,JS99),CMS(idet,JS995),CMS(idet,JS999) ! -----------+++++++ Ai.ADC
 4494 format('River concentrations: ',a11,7x,10f10.2) ! ----------+++++++ Ai.ADC
      write(140+idet,7654) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      endif ! if ( QTYPE (idet) .ne. 4 ) PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      enddo ! do idet = 1, ndet PPPPPPPPPP apportionment of percentiles PPPPPPPP
      endif ! if ( n251 .eq. 1 ) ! PPPPPPP apportionment of percentiles PPPPPPPP
 
      
      
      ihead = 2 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( ihead .eq. 1 ) ++++++++++++++++++++++++++++++++++++++++++++++

      if ( iline .eq. 1 ) then ! lllllllllllllllllllllllllllllllllllllllllllllll
      iline = 0
      if ( nobigout .le. 0 ) then ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      do jdet = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( qtype(jdet) .ne. 4 ) then ! +++++++++++++++++++++++++++++++++++++++++
      write(120+jdet,3347)uname(jworks),TenAV(jdet),TenTE(jdet) ! ------- Ai.ADL
 3347 format(a40,a10'% of load in the river and',a10, ! ----------------- Ai.ADL
     &'% of the effluent load in the river') ! -------------------------- Ai.ADL
      endif ! if ( qtype(jdet) .ne. 4 ) ++++++++++++++++++++++++++++++++++++++++
      enddo ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          
      if ( feeture .lt. 1 ) then ! #############################################
      pause "feeture is less than zero ..." ! ##################################
      endif ! ##################################################################
      
      if ( feetfeet .gt. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( JT(feetfeet) .ne. 10 ) then ! +++++++++++++++++++++++++++++++++++++++
      if ( kdirect .ne. 3 .and. kdirect .ne. 9 ) then ! ++++++++++++++++++++++++
      do idet = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     if ( JT (jworks) .eq. 3 .or. JT (jworks) .eq. 5 ) then +++++++++++++++++++
      if ( JT (jworks) .eq. 3 ) then ! +++++++++++++++++++++++++++++++++++++++++
          
      arkdist (feetfeet) = Length of main river ! ####################### P1-CSV
      if ( iworks .gt. NUED ) then ! #################################### P1-CSV
      write( *,*)' * Number of works exceeds the current maximum', ! #### P1-CSV
     &iworks,NUED ! ##################################################### P1-CSV
      call pause ! ###################################################### P1-CSV
      endif ! if ( iworks .gt. NUED ) ################################### P1-CSV
      if ( feetfeet .gt. NU ) then ! #################################### P1-CSV
      write( *,*)' * Number of Features the current maximum', ! ######### P1-CSV
     &feetfeet,NU ! ##################################################### P1-CSV
      call pause ! ###################################################### P1-CSV  
      endif ! if ( feetfeet .gt. NU ) ################################### P1-CSV
      
      crossmatch (idet,iworks,feetfeet) = xworksAV(idet) ! ############## P1-CSV
      endif ! if ( JT (iworks) .eq. 3)++++++++++++++++++++++++++++++++++++++++++
      enddo ! do idet = 1, ndet ++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( kdirect .ne. 3 .and. kdirect .ne. 9 ) +++++++++++++++++++++++
      endif ! if ( JT(feetfeet) .ne. 10 ) ++++++++++++++++++++++++++++++++++++++
      endif ! if ( feetfeet .gt. 0 ) +++++++++++++++++++++++++++++++++++++++++++
      
      if ( n251 .eq. 1 ) then ! PPPPPPPP apportionment of percentiles PPPPPPPPPP
      write(140+ndshot,4194)uname(jworks),TenAV(ndshot),Ten50(ndshot), !  Ai.ADC
     &Ten90(ndshot),Ten95(ndshot),Ten98(ndshot), ! PPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     &Ten99(ndshot),Ten995(ndshot),Ten999(ndshot) ! PPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4194 format('% from ',a33,10a10,1x,i6) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      endif ! if ( n251 .eq. 1 ) PPPPP apportionment of percentiles PPPPPPPPPPPP
      endif ! if ( nobigout .le. 0 ) nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

      
	do idet = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	if ( QTYPE (idet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,9099)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rname(IREACH),uname(jworks),xworksAV(idet),Length of main river, ! DDi.CSV
     &xworksTE(idet)!,distp ! ------------------------------------------ DDi.CSV
 9099 format(' ',a40,',',i4,',',a40,',',a16,',', ! --------------------- DDi.CSV
     &'% of the total river load from this discharge:',',',a40, ! ------ DDi.CSV
     &(',',0pf11.2,'%'),',,',0pf12.2,(',',0pf11.2,'%'), ! -------------- DDi.CSV
     &(',,,,,,,,,,,,,,,,',0pf11.2)) ! ---------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
      write(110+idet,9899)GIScode(feeture),unamex,rname(IREACH), ! ------ Di.CSV
     &uname(jworks),xworksAV(idet),jxtype,jxtype,xworksTE(idet) ! ------- Di.CSV
 9899 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di.CSV
     &'% of the total river load from this discharge:',',',a40, ! ------- Di.CSV
     &',,,,,,,,,,,,',(',',0pf12.2),',',i4, ! --------------------  ------ Di.CSV
     &',',i4,(',',0pf12.2)) ! ------------------------------------------- Di.CSV
      endif ! if ( QTYPE (idet) .ne. 4 ) +++++++++++++++++++++++++++++++++++++++
      enddo ! idet = 1, ndet +++++++++++++++++++++++++++++++++++++++++++++++++++
      
      endif ! if ( iline .eq. 1 ) llllllllllllllllllllllllllllllllllllllllllllll
 1000 continue ! end of loop on the number of works WWWWWWWWWWWWWWWWWWWWWWWWWWWW
      
*     print out a summary if any lines were printed ----------------------------
      if ( ihead .eq. 2 ) then
      do kdet = 1, mp10
      TenAV(kdet) = '          '
      TenTE(kdet) = '          '
      enddo ! do kdet = 1, mp10 
      xxx2 = 0.0
      xxx3 = 0.0
      do jdet = 1, ndet
      TenAV(jdet) = '     .....'
      TenTE(jdet) = '     ......'
      TenAVT(jdet) = '     .....'
      if ( total effluent prop (jdet) .gt. 1.0e-20 ) then 
      call sort format 2 (TELODE2(jdet,i13),total effluent prop (jdet))
      TenAV(jdet) = valchars10
      TenAVT(jdet) = valchars11
      
      endif ! if ( total effluent prop (jdet) .gt. 1.0e-20 )
      if ( total effluent load (jdet) .gt. 1.0e-20 ) then
      call sort format 2 (total effluent load (jdet),TGLODE2(jdet,i13))
      TenTE(jdet) = valchars10 ! total effluent load
      TenAVT(jdet) = valchars11 ! total load
      endif ! if ( total effluent load (jdet) .gt. 1.0e-20 )
      enddo ! do jdet = 1, ndet
 
      do jdet = 1, ndet   
      if ( qtype(jdet) .ne. 4 ) then
      if ( kdirect .eq. 9 .or. kdirect .eq. 3 ) then
      write(120+jdet,2446)total effluent prop (jdet),TenTE(jdet),
     &TenAV(jdet),TenAVT(jdet) ! ---------------- Ai.ADL
 2446 format(140('-')/'Totals',35x,f9.1,'%', ! -------------------------- Ai.ADL
     &25x,a10,'%'/140('-')/ ! ------------------------------------------- Ai.ADL
     &'Total effluent load in the river',44x,a10,'    within ',a10/
     &140('=')//) ! ----------------------------------------------------- Ai.ADL
      else
      write(120+jdet,2346)total effluent prop (jdet),TenTE(jdet), ! ----- Ai.ADL
     &TenAV(jdet),TenAVT(jdet) ! ---------------------------------------- Ai.ADL
 2346 format(140('-')/'Totals',35x,f9.1,'%', ! -------------------------- Ai.ADL
     &25x,a10,'%'/140('-')/ ! ------------------------------------------- Ai.ADL
     &'Total effluent load in the river',44x,a10,'    within ',a10/
     &140('=')) ! ------------ Ai.ADL
      endif
      endif ! if ( qtype(jdet) .ne. 4 )
      enddo ! do jdet = 1, ndet



      if ( n251 .eq. 1 ) then ! apportionment of percentiles PPPPPPPPPPPPPPPPPPP
      if ( totav12av .gt. Small ) then ! apportionment of percentiles PPPPPPPPPP
      write(140+ndshot,2546)tot12av,tot1250,tot1290,tot1295, ! PPPPPPPPPP Ai.ADC
     &tot1298,tot1299,tot12995,tot12999 ! apportionment of percentiles -- Ai.ADC
 2546 format(140('-')/ ! apportionment of percentiles PPPPPPPPPPPPPPPPPPPPPPPPPP
     &'Total % contributions from intermittents',10f10.2)
      endif
      write(140+ndshot,3546)tot03av,tot0350,tot0390,tot0395, ! PPPPPPPPPP Ai.ADC
     &tot0398,tot0399,tot03995,tot03999 ! apportionment of percentiles -- Ai.ADC
 3546 format(140('-')/
     &'Total % contributions from sewage works ',10f10.2)
      write(140+ndshot,2246)TenTE(ndshot),Ten50t(ndshot), ! ------+++++++ Ai.ADC
     &Ten90t(ndshot),Ten95t(ndshot), ! 33 apportionment of percentiles -- Ai.ADC
     &Ten98t(ndshot),Ten99t(ndshot),Ten995t(ndshot),Ten999t(ndshot) ! --- Ai.ADC
 2246 format(140('-')/
     &'Total load in the percentile            ',10a10)
      if ( kdirect .eq. 3 .or. kdirect .eq. 9 ) then
      write(140+ndshot,7655) ! --------- apportionment of percentiles --- Ai.ADC
 7655 format(140('=')//)
      else ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+ndshot,7654) ! PPPPPPPPP apportionment of percentiles PPP Ai.ADC
      endif ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      endif ! if ( n251 .eq. 1 ) ! apportionment of percentiles PPPPPPPPPPPPPPPP

      
      
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      if ( nobigout .le. 0 ) then
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,9299)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rname(IREACH),total effluent prop (idet), ! ---------------------- DDi.CSV
     &Length of main river,total effluent load (idet)!,distp ! --------- DDi.CSV
 9299 format(' ',a40,',',i4,',',a40,',',a16,',', ! --------------------- DDi.CSV
     &'% of the total River load from all discharges:', ! -------------- DDi.CSV
     &',Discharges (3 12 5 39 60 61)', ! ------------------------------- DDi.CSV
     &(',',0pf11.2,'%'),',','3 12 5 39 60 61',','0pf12.2, ! ------------ DDi.CSV
     &(',',0pf11.2,'%'),(',,,,,,,,,,,,,,,,',0pf11.2)) ! ---------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
      write(110+idet,7299)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rname(IREACH),total effluent prop (idet),jxtype, ! ---------------- Di.CSV
     &total effluent load (idet) ! -------------------------------------- Di.CSV
 7299 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di.CSV
     &'% of the total river load from all discharges:',',', ! ----------- Di.CSV
     &'Discharges (3 12 5 39 60 61)', ! --------------------------------- Di.CSV
     &',,,,,,,,,,,,',(',',0pf11.2)',',i4, ! ------------=---------------- Di.CSV
     &', 3 12 5 39 60 61',(',',0pf11.2)) ! ------------------------------ Di.CSV
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE (idet) .ne. 4 )
	enddo ! do idet = 1, ndet
      endif ! if ( ihead .eq. 2 ) 

      return
      end

      
      
      
      subroutine proportion of catchments (kdirect) 
      include 'COMMON DATA.FOR'
      character *10 Tenchars1(MP10),Tenchars2(MP10),Tenchars3(MP10)
      
      character *10 TenAV(MP10),TenAVT(MP10),TenTE(MP10)
      character *50 unamex50
      
      real total effluent prop (MP10), total effluent load (MP10)
      real xbodiesAV(MP10),xbodiesTE(MP10),xbods(MP10)

      dimension xbodies (NUW,MP10), xwload (MP10)
      real total bodies prop (MP10), total bodies load (MP10), 
     &total bodies length (MP10)
      real brakedown(NUW,MP10,nprop+1),braketotal(nprop+1) ! nprop = 29
      real totAV(nprop)

      if ( ical13 .eq. 1 ) return
     
*     kdirect = 0 .... start of the reach --------------------------------------
*     kdirect = 1 .... at feature ----------------------------------------------
*     kdirect = 3 .... end of the reach ----------------------------------------
*     kdirect = 9 .... end of the model ----------------------------------------

	unamex = 'nothing'
	unamex50 = 'nothing'
	if ( kdirect .eq. 1 ) then
      unamex = uname(feeture)
      write(unamex50,1279)uname(feeture),rname(IREACH)
 1279 format(a32,'- ',a16)
      endif
	if ( kdirect .eq. 9 ) unamex = 'End of the model'
	if ( kdirect .eq. 0 ) then
      write(unamex50,1276)rname(IREACH)
      write(unamex,1276)rname(IREACH)
 1276 format('Start of reach - ',a16)
      jxtype = 111
      endif
	if ( kdirect .eq. 3 ) then
      write(unamex50,1277)rname(IREACH)
      write(unamex,1277)rname(IREACH)
 1277 format('End of reach - ',a16)
      jxtype = 999
      endif

*     initialise the totals ----------------------------------------------------
      do kdet = 1, mp10
      total bodies load (kdet) = 0.0
      total bodies prop (kdet) = 0.0
      total bodies length (kdet) = 0.0
      enddo      

*     initialise the load in each sub-catchment (xbodies) ----------------------
      do 2507 ibodies = 1, kount bodies
      do 2508 idet = 1, ndet
*     brakedown ( ibodies, idet, nprop + 1) = 0.0
      if ( QTYPE (idet) .ne. 4 ) then
      xbodies (ibodies,idet) = 0.0
      if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
*     loads from upstream sub-catchments ---------------------------------------
      xbodies (ibodies,idet) = TWLOADS (ibodies,idet,i13) ! ------- annual value
      total bodies load (idet) = total bodies load (idet) 
     & + xbodies (ibodies,idet)
      endif
      endif
 2508 continue ! ibodies = 1, kount bodies
 2507 continue ! idet = 1, ndet
      
*     write headings and the loads from each sub-catchment ---------------------
      if ( nobigout .gt. 0 ) return
      ihead = 0
      do 2000 ibodies = 1, kount bodies ! ++++++++++++++++++++++++++++++++++++++
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do 2513 idet = 1, ndet ! .................................................
	if ( qtype (idet) .ne. 4 ) then ! ........................................
      Tenchars1(idet) = '      ....' ! =========================================
	if ( ihead .eq. 0 ) then ! ===============================================
      ihead = 1 ! ==============================================================
      endif ! if ( ihead .eq. 0 ) ==============================================
      xltot = xltot + xbodies (ibodies,idet)
*     loads from upstream sub-catchments ---------------------------------------
      call sort format 1 (xbodies (ibodies,idet))
*     loads from upstream sub-catchments ---------------------------------------
      Tenchars1(idet) = valchars10 ! -------------------- xbodies (ibodies,idet)
      endif ! if ( qtype (idet) .ne. 4 ) .......................................
 2513 continue ! idet = 1, ndet ................................................
      
      do idet = 1, ndet ! ======================================================
	if ( QTYPE (idet) .ne. 4 ) then ! ========================================
      Tenchars2(idet) = '      ....'
	total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
      endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif
      call sort format 2 (xload per km, xper)
      Tenchars2(idet) = valchars10 ! load per kilometre ------------------------
     
      if ( xbodies(ibodies,idet) .gt. 0.0000001 ) then ! -----------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,43)GIScode(feeture),jxtype,unamex, ! -------------- DDi.CSV
     &uname(jbodies),xbodies(ibodies,idet), ! -------------------------- DDi.CSV
     &Length of main river, ! ------------------------------------------ DDi.CSV
     &TWlength(ibodies),xload per km,xper!,distp ! --------------------- DDi.CSV
   43 format(' ',a40,',',i4,',',a40,',',a16,',', ! --------------------- DDi.CSV
     &'Annual mean load from this sub-catchment ',',','Total load', ! -- DDi.CSV 
     &(',',1pe12.4),',24',','0pf12.2,',',2(',',0pf11.2), ! ------------- DDi.CSV 
     &(',',0pf11.2,'%'), ! --------------------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+idet,9943)GIScode(feeture),unamex,uname(jbodies), ! ----- Di.CSV
     &xbodies(ibodies,idet),jxtype, ! ----------------------------------- Di.CSV
     &xper!,TWlength(ibodies),xload per km ! ---------------------------- Di.CSV
 9943 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di CSV
     &'Annual mean load from this sub-catchment (1)',',','Total load', !  Di.CSV
     &',,,,,,,,,,,,',(',',1pe11.4),',',i4,',', ! ------------------------ Di.CSV
     &(',',0pf11.2),6(',',1pe11.4)) ! ----------------------------------- Di.CSV
      
      endif ! if ( xbodies(ibodies,idet) .gt. 0.0000001 ) ----------------------
      endif ! if ( QTYPE (idet) .ne. 4 ) =======================================
      enddo ! do idet = 1, ndet ! ==============================================
 2000 continue ! ibodies = 1, kount bodies +++++++++++++++++++++++++++++++++++++
      
*     write the totals over all sub-catchments ---------------------------------
      catload = 0.0
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (total bodies load (idet))
      catload = catload + total bodies load (idet)
      endif
      enddo
      if ( catload .gt. small ) then ! =========================================
      do idet = 1, ndet ! ======================================================
      if ( qtype (idet) .ne. 4 ) then ! ========================================
      endif ! if ( qtype (idet) .ne. 4 ) =======================================
      enddo ! do idet = 1, ndet ================================================
      endif ! if ( catload .gt. small ) ========================================

      do idet = 1, ndet ! ======================================================
      Tenchars3(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif

      call sort format 1 (total bodies load (idet))
      Tenchars3(idet) = valchars10

      call sort format 2 (xload per km, xper)

      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,813)GIScode(feeture),jxtype,unamex, ! ------------- DDi.CSV
     &total bodies load (idet),Length of main river, ! ----------------- DDi.CSV
     &total bodies length (idet),xload per km,xper ! ------------------- DDi.CSV
  813 format(' ',a40,',',i4,',',a40,',','All sub-catchments',',', ! ---- DDi.CSV
     &'Annual MEAN load from all sub-catchments',',', ! ---------------- DDi.CSV
     &'All sub-catchments', ! ------------------------------------------ DD1.CSV
     &(',',1pe12.4),',24',','0pf12.2,',',2(',',0pf11.2), ! ------------- DD1.CSV
     &(',',0pf11.2),'%', ! --------------------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+idet,9813)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &total bodies load (idet),jxtype,xper ! ---------------------------- Di.CSV
 9813 format(' ',a40,',',a40,',','All sub-catchments',',', ! ------------ Di.CSV
     &'Annual mean load from all sub-catchments (2)', ! ----------------- Di.CSV
     &',','Total load', ! ----------------------------------------------- Di.CSV
     &',,,,,,,,,,,,',(',',1pe11.4),',',i4,',', ! ------------------------ Di.CSV
     &(',',0pf11.2),6(',',1pe11.4)) ! ----------------------------------- Di.CSV

      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo ! do idet = 1, ndet ================================================
*     ==========================================================================


*     ==========================================================================
*     write the load per kilometre per sub-catchment ---------------------------  
      ihead = 0
      if ( catload .gt. small ) then ! =========================================
      do 2001 ibodies = 1, kount bodies ! ======================================
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do idet = 1, ndet
      Tenchars3(idet) = '      ....'
	if ( qtype (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then ! ============-=========================== Ai.ACL
      write(220+idet,62)unamex50,LUNITS(idet) ! to ACL file ------------- Ai.ACL
   62 format(//140('=')/'Load and load per ', ! ------------------------- Ai.ACL
     &'kilometre provided by individual sub-catchments ', ! ------------- Ai.ACL
     &'upstream of:',9x,a50/140('-')/ ! --------------------------------- Ai.ACL
     &56x,'LOAD',6x,'LOAD',9x,'%'/ ! ------------------------------------ Ai.ACL
     &56x,a4,4x,'per KM',6x,'LOAD') ! ----------------------------------- Ai.ACL
      endif ! if ( ihead .eq. 0 ) ======================================= Ai.ACL
      xwrite = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then
      xwrite = xbodies(ibodies,idet)/TWlength(ibodies)
      xltot = xltot + xwrite
      endif
      call sort format 1 (xwrite)
      Tenchars3(idet) = valchars10
      endif
      enddo ! do idet = 1, ndet ------------------------------------------------
      ihead = 1
 2001 continue ! do 2001 ibodies = 1, kount bodies ! ==================== Ai.ACL
*     ==========================================================================
      
      
*     ##########################################################################      
*     ##########################################################################      
      if ( kount bodies .gt. 99999 ) then
      xltot = 0.0
      do 2500 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      do 2501 ibody = 1, kount bodies - 1
      xbodies (kount bodies,idet) = xbodies (kount bodies,idet) 
     &                            - xbodies (ibody,idet) 
 2501 continue
      call sort format 1 (xbodies (kount bodies,idet))
      Tenchars3(idet) = valchars10
      xltot = xltot + xbodies (kount bodies,idet)
      endif
 2500 continue
      endif ! if ( kount bodies .gt. 99999 )
      
      do idet = 1, ndet ! =============================================== Ai.ACL
      if ( qtype (idet) .ne. 4 ) then ! ================================= Ai.ACL
      write(220+idet,1115) ! ------------- loads from catchments -------- Ai.ACL
 1115 format(140('-'))
      endif ! if ( qtype (idet) .ne. 4 ) ================================ Ai.ACL
      enddo ! do idet = 1, ndet ========================================= Ai.ACL
*     ##########################################################################      
*     ##########################################################################      

      endif ! if ( catload .gt. small ) ========================================
*     ==========================================================================




*     ==========================================================================
*     write the load per kilometre over all sub-catchments --------------------- 
      xltot = 0.0
      do idet = 1, ndet
      Tenchars2(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      call sort format 1 (xload per km)
      Tenchars2(idet) = valchars10
      xltot = xltot + xload per km
      endif
      enddo

      
*     ==========================================================================
*     write out the percentages ------------------------------------------------
      do idet = 1, ndet
	xwload (idet) = 0.0
      enddo
      ihead = 0
      if ( catload .gt. small ) then ! =========================================
      do 2504 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do 2502 idet = 1, ndet
      Tenchars3(idet) = '      ....'
	if ( qtype (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
      write(220+idet,92) ! to ACL file - loads from catchments ---------- Ai.ACL
   92 format('The percentage of river load ',
     &'supplied by individual sub-catchments upstream of ',
     &'this point'/140('-')) ! ------------------------------------------ Ai.ACL
	ihead = 1
	endif
      xpload = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.000001 ) then
*     TGLODE2(idet,i13) = running total of net loads down the model
	xpload = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xwload (idet) = xwload (idet) + xpload
      brakedown ( ibodies, idet, nprop + 1) = xpload
      call sort format 1 (xpload)
      Tenchars3(idet) = valchars10
      xltot = xltot + xpload
	endif
	endif
 2502 continue

      if ( xltot .gt. 1.0e-09 ) then ! .................................. Ai.ADL
      do idet = 1, ndet ! =============================================== Ai.ACL
      if ( qtype (idet) .ne. 4 ) then ! ================================= Ai.ACL
      write(220+idet,4)uname(jbodies), ! ---- loads from catchments ----- Ai.ACL
     &Tenchars1(idet),Tenchars2(idet), ! -------------------------------- Ai.ACL
     &Tenchars3(idet),TWLENGTH(ibodies) ! ------------------------------- Ai.ACL
    4 format(5x,a40,5x,3a10,7x,'over',f7.1,' kilometres'/140('-')) ! ---- Ai.ACL
      endif ! if ( qtype (idet) .ne. 4 ) ================================ Ai.ACL
      enddo ! do idet = 1, ndet ========================================= Ai.ACL
      endif ! if ( xltot .gt. 1.0e-09 ) ................................. Ai.ACL
 2504 continue ! do 2504 ibodies = 1, kount bodies

      endif ! if ( catload .gt. small ) ========================================
*     ==========================================================================
*     ==========================================================================

      
      
*     ==========================================================================
*     ==========================================================================
*     ==========================================================================
*     repeat for the breakdown of contributions to sub-catchments --------------   
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
*     (23) = '(50)'
*     (24) = '(52)'
*     (25) = '(54)'
*     (26) = '(56)'
*     (27) = '(58)'
*     --------------------------------------------------------------------------
*     loop on all the types of input and source --------------------------------

      tot24AV = 0.0

*     loop on the number of upstream catchments BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      do 1000 ibodies = 1, kount bodies ! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      iline = 0
      jbodies = identify bodies (ibodies)
      xbodiesd = 0.0
      
*     loop on determinands .....................................................
      do 1001 idet = 1, ndet ! .................................................
	if ( QTYPE (idet) .eq. 4 ) goto 1001 ! ...................................
      
*     initialise the proportions -----------------------------------------------
      TenAV(idet)   = '     .....' ! % mean of the total load
      TenAVT(idet)  = '     .....' ! % of the total load
      TenTE(idet)   = '     .....' ! % of the total load

      xbodiesAV (idet) = 0.0 ! % of the total load
      
	if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
      xbodiesAV(idet) = 100.0*TDLOADAV(ibodies,idet)/TGLODE2(idet,i13)
	total effluent prop (idet) = total effluent prop (idet) 
     &+ xbodiesAV(idet)
      
      if ( JT(jbodies) .eq. 24 ) then ! intermittent 3333333333333333333333333333
      tot24AV = tot24AV + xbodiesAV(idet)
      endif
      
      call sort format 2 (xbodiesAV(idet),TGLODE2(idet,i13))
	TenAV(idet) = valchars10 ! xbodiesAV(idet)
	TenAVT(idet) = valchars11 ! TGLODE2(idet,i13)
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 )
      
      xbodiesTE (idet) = 0.0 ! % of the total effluent load
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then
      xbodiesTE(idet) = 100.0*TDLOADAV(ibodies,idet)/TELODE2(idet,i13)
	total effluent load (idet) = total effluent load (idet)
     & + xbodiesTE(idet)
      xbodiesd = amax1 ( xbodiesd, xbodiesTE(idet) )
      call sort format 1 (xbodiesTE(idet))
	TenTE(idet) = valchars10 ! xbodiesTE(idet)
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 )

*     check that the percentage load does not exceed 100 -----------------------
	if ( xbodiesTE (idet) .gt. 100.0001 ) then
	call change colour of text (20) ! bright red
	write( *,7300)uname(jbodies)
      call set screen text colour
	write(01,7300)uname(jbodies) ! error message ---------------------file OUT
	write(09,7300)uname(jbodies) ! error message ---------------------file SCN
	write(33,7300)uname(jbodies) ! error message ---------------------file ERR
      write(220+idet,7300)uname(jbodies) ! error message ---------------- Ai.ACL
 7300 format(/77('-')/
     &'Error in the calculation of loads attributable to ',
     &'effluents ... '/
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) 
	write(01,7000)TELOADAV(ibodies,idet),dname(idet) ! error message -file OUT
	write(09,7000)TELOADAV(ibodies,idet),dname(idet) ! error message -file SCN
      write(220+idet,7000)TELOADAV(ibodies,idet),dname(idet) ! ---error-- Ai.ACL
	write(33,7000)TELOADAV(ibodies,idet),dname(idet) ! error message -file ERR
 7000 format('   Load from works =',f15.6,' for ',a11) ! -----------------------
      write(01,7001)TELODE2(idet,i13) ! error message ------------------file OUT
      write(09,7001)TELODE2(idet,i13) ! error message ------------------file SCN
      write(220+idet,7001)TELODE2(idet,i13) ! error message ------------- Ai.ACL
      write(33,7001)TELODE2(idet,i13) ! error message ------------------file ERR
 7001 format('    Total net load =',f15.6) ! -----------------------------------
	write(01,7002)xbodiesTE(idet) ! error message --------------------file OUT
	write(09,7002)xbodiesTE(idet) ! error message --------------------file SCN
      write(220+idet,7002)xbodiesTE(idet) ! error message --------------- Ai.ACL
      write(33,7002)xbodiesTE(idet) ! error message --------------------file ERR
 7002 format('        Percentage =',f15.6/77('-'))
      call stop
      endif ! if ( xbodiesTE (idet) .gt. 100.0001 )
 1001 continue ! end of loop on determinands ...................................

 	if ( xbodiesd .gt. 0.00001 ) then
      iline = 1
 	if ( ihead .ne. 2 ) ihead = 1
      endif ! if ( xbodiesd .gt. 0.00001 )

      if ( iline .eq. 1 ) then
      iline = 0 
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,9099)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &uname(jbodies),xbodiesAV(idet),xbodiesTE(idet),distp ! ----------- DDi.CSV
 9099 format(' ',a40,',',a40,',',a40,',', ! ---------------------------- DDi.CSV
     &'% Annual contribution from individual works',',',2(',', ! ------- DDi.CSV
     &1pe11.4),',,,,,,,,,,,,',',3 12 5 39 60 61', ! -------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
      
*     write(110+idet,8099)GIScode(feeture),unamex,uname(jbodies),
*    &dname(idet),xbodiesAV(idet),xbodiesTE(idet),jxtype
*8099 format(' ',a40,',',a40,',',a40,',',
*    &'% Annual contribution from individual works'',',a11,2(',', ! ----- Di.CSV
*    &1pe11.4),',,,,,,,,,,,,',i4,', 3 12 5 39 60 61') ! ----------------- Di.CSV
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo ! idet = 1, ndet
      endif ! if ( iline .eq. 1 )

 1000 continue ! end of loop on the number of works WWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     ==========================================================================
*     ==========================================================================

      
      
*     ==========================================================================
*     ==========================================================================
      do 6000 ip = 1, nprop ! ++++++++++++++++++++++++++++++++++++++++++++++++++
          
      itrap2 = 0
*     initialise the totals ----------------------------------------------------
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then ! ========================================
      total bodies load (idet) = 0.0
      total bodies prop (idet) = 0.0
      total bodies length (idet) = 0.0
      endif
      enddo 
      totAV(ip) = 0.0

*     initialise the load in each sub-catchment (xbodies) for this type of input
      do 4507 ibodies = 1, kount bodies ! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      do 4508 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      xbodiesAV (idet) = 0.0 ! % of the total load ! 666666666666666666666666666
     
      if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then ! check total net load 6666666
      xbodiesAV(idet) = 100.0 * TWLOADSAPP (ibodies,idet,i13,ip)/
     &TGLODE2(idet,i13)
	total bodies prop (idet) = total bodies prop (idet) 
     &+ xbodiesAV(idet)
      totav(ip) = totAV(ip) + xbodiesAV(idet)
      call sort format 2 (xbodiesAV(idet),TGLODE2(idet,i13))
	TenAV(idet) = valchars10
	TenAVT(idet) = valchars11
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) 666666666666666666666666666

      xbodiesTE (idet) = 0.0 ! % of the total effluent load
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then ! ============================
      xbodiesTE(idet) = 100.0 * TWLOADSAPP (ibodies,idet,i13,ip)/
     &TELODE2(idet,i13)
      xbodiesd = amax1 ( xbodiesd, xbodiesTE(idet) )
      call sort format 1 (xbodiesTE(idet))
	TenTE(idet) = valchars10
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) ! =========================

*     brakedown ( ibodies, idet, ip) = 0.0 ! RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      xbodies (ibodies,idet) = 0.0
      if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then ! total of net loads ---------
      xbodies (ibodies,idet) = TWLOADSapp (ibodies,idet,i13,ip)
      total bodies load (idet) = total bodies load (idet) ! + xbodies
     & + xbodies (ibodies,idet)
      if ( xbodies (ibodies,idet) .gt. 0.000001 ) itrap2 = 1
      endif
      endif ! if ( QTYPE (idet) .ne. 4 ) IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4508 continue ! ibodies = 1, kount bodies IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4507 continue ! idet = 1, ndet BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

*     write the headings for each sub-catchment for this type of input load ----
      ihead = 0
      if ( itrap2 .eq. 1 ) then ! xbodies > 0.000001............................ 
      do 4000 ibodies = 1, kount bodies ! ......................................
      jbodies = identify bodies (ibodies)
      do 4513 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	if ( qtype (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
*     write heading for this type of input -------------------------------------
	if ( ihead .eq. 0 ) then
      write(220+idet,4012)nameprop(ip) ! ================================ Ai.ACL
 4012 format(/140('+')/'The river load in individual sub-catchments ',
     &'upstream of this point from: ',a37/140('=')) ! =================== Ai.ACL
      ihead = 1
      endif ! if ( ihead .eq. 0 ) 
*     --------------------------------------------------------------------------
      endif ! if ( QTYPE (idet) .ne. 4 ) IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4513 continue ! idet = 1, ndet IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      
      do 4600 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif
      call sort format 2 (xload per km, xper)
      endif ! if ( QTYPE (idet) .ne. 4 ) IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4600 continue ! idet = 1, ndet IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4000 continue ! ibodies = 1, kount bodies .....................................
      endif ! if ( itrap2 .eq. 1 ) xbodies > 0.000001...........................

      

*     write the data for each sub-catchment for this type of input load --------
*     write to the CSV file that is picked up by SAGIS -------------------------
      do 4800 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 4680 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif
      
      if ( xbodies (ibodies,idet) .gt. 0.0000001 ) then ! ----------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,6643)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &uname(jbodies),nameprop(ip),xbodies (ibodies,idet), ! ------------ DDi.CSV 
     &numprop(ip),Length of main river, ! ------------------------------ DDi.CSV
     &TWlength(ibodies),xload per km,xper! ----------------------------- DDi.CSV
 6643 format(' ',a40,',',i4,',',a40,',',a16,',', ! --------------------- DDi.CSV
     &'Annual MEAN load from this sub-catchment from:',',',a37, ! ------ DDi.CSV
     &(',',1pe12.4),','i4,','0pf12.2,',',2(',',0pf11.2), ! ------------- DDi.CSV
     &(',',0pf11.2),'%', ! --------------------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+idet,8643)GIScode(feeture),unamex,uname(jbodies), ! ----- Di.CSV
     &nameprop(ip),xbodies(ibodies,idet),jxtype,! ----------------------- Di.CSV
     &xper!,TWlength(ibodies),xload per km ! ---------------------------- Di.CSV
 8643 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di.CSV
     &'Annual mean load from this sub-catchment from:',',',a37, ! ------- Di.CSV
     &',,,,,,,,,,,,',(',',1pe11.4),',',i4,',', ! ------------------------ Di.CSV
     &(',',0pf11.2),6(',',1pe11.4)) ! ----------------------------------- Di.CSV
      endif ! if ( xbodies (ibodies,idet) .gt. 0.0000001 ) ---------------------
      endif ! if ( QTYPE (idet) .ne. 4 ) 
 4680 continue ! do 4600 idet = 1, ndet
 4800 continue ! ibodies = 1, kount bodies
      
*     write the data for each sub-catchment for this type of input load --------
*     write to the CSV file picked up by SAGIS ---------------------------------
      if ( itrap2 .eq. 1 ) then ! xbodies > 0.000001 TTTTTTTTTTTTTTTTTTTTTTTTTTT
      do 4880 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      endif
      endif ! if ( QTYPE (idet) .ne. 4 ) IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 4880 continue ! idet = 1, ndet IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      endif ! if ( itrap2 .eq. 1 ) ! xbodies > 0.000001 TTTTTTTTTTTTTTTTTTTTTTTT      
      
      do 6880 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif

      if ( total bodies load (idet) .gt. 0.0000001 ) then ! --------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,4813)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &nameprop(ip),total bodies load (idet),numprop(ip), ! ------------- DDi.CSV
     &Length of main river, ! ------------------------------------------ DDi.CSV
     &total bodies length (idet),xload per km,xper! -------------------- DDi.CSV
 4813 format(' ',a40,',',i4,',',a40,',All sub-catchments',',', ! ------- DDi.CSV
     &'Annual MEAN load from all sub-catchments from:',',',a37, ! ------ DDi.CSV
     &(',',1pe12.4),','i4,','0pf12.2,',',2(',',0pf11.2), ! ------------- DDi.CSV
     &(',',0pf11.2,'%'), ! --------------------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+idet,6813)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &nameprop(ip),total bodies load (idet),jxtype, ! ------------------- Di.CSV
     &xper!,total bodies length (idet),xload per km ! ------------------- Di.CSV
 6813 format(' ',a40,',',a40,',All sub-catchments',',', ! --------------- Di.CSV
     &'Annual mean load from all sub-catchments from:',',',a37, ! ------- Di.CSV
     &',,,,,,,,,,,,',(',',1pe11.4),',',i4,',', ! ------------------------ Di.CSV
     &(',',0pf11.2),6(',',1pe11.4)) ! ----------------------------------- Di.CSV
      endif ! if ( total bodies load (idet) .gt. 0.0000001 ) -------------------
      
      endif ! if ( QTYPE (idet) .ne. 4 ) IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 6880 continue ! idet = 1, ndet IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      
*     write the breakdown over all sub-catchments ------------------------------
      do 4700 idet = 1, ndet

      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      endif
 4700 continue
      
*     write the breakdown to the MON and CSV files -----------------------------
      do 3401 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
	if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      if ( xbodies (ibodies,idet) .lt. 0.0001 ) goto 3401
      Tenchars2(idet) = '      ----'

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
      endif
     
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,5813)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &nameprop(ip),total bodies load (idet),xper, ! -------------------- DDi.CSV
     &total bodies length (idet),xload per km,distp ! ------------------ DDi.CSV
 5813 format(' ',a40,',',i4,',',a40,',Load from sub-catchments',',', ! - DDi.CSV
     &'Total load from all sub-catchments',',',a37, ! ------------------ DDi.CSV
     &(',',1pe11.4),',24',4(',',1pe11.2), ! ---------------------------- DDi.CSV
     &(',,,,,,,,,,,,,',0pf11.2)) ! ------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      endif ! if ( QTYPE (idet) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 3401 continue ! do 3401 idet = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

      if ( kount bodies .gt. 99999 ) then
      do 5500 idet = 1, ndet ! =================================================
      if ( xbodies (ibodies,idet) .lt. 0.0001 ) goto 5500
      do ibody = 1, kount bodies - 1
      xbodies (kount bodies,idet) = xbodies (kount bodies,idet) 
     &                            - xbodies (ibody,idet) 
      enddo ! do ibody = 1, kount bodies - 1
      call sort format 1 (xbodies (kount bodies,idet))
 5500 continue ! do 5501 ibody = 1, kount bodies - 1 ===========================
      endif

*     write the load per kilometre for breakdowns over all sub-catchments ------
      do idet = 1, ndet
      Tenchars2(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      call sort format 1 (xload per km)
      Tenchars2(idet) = valchars10
      endif
      enddo
      
*     write out the percentages of the breakdown supplied by sub-catchments ----
      do idet = 1, ndet 
	xwload (idet) = 0.0
      enddo
      ihead = 0
      if ( itrap2 .eq. 1 ) then ! xbodies > 0.000001 ===========================
      do 5504 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 5502 idet = 1, ndet
      Tenchars3(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
      write(220+idet,5092)nameprop(ip) ! -------------------------------- Ai.ACL
 5092 format(140('=')/'The percentage of the river load ',
     &'supplied by sub-catchments from: ',a37/140('='))
	ihead = 1
	endif

      xpload = 0.0
	if ( total bodies load (idet) .gt. 0.000001 ) then
	xpload = 100.0 * xbodies (ibodies,idet) / total bodies load (idet)
      xwload (idet) = xwload (idet) + xpload 
      endif ! if ( total bodies load (idet) .gt. 0.000001 )
      
      call sort format 1 (xpload)
      Tenchars3(idet) = valchars10
      
      xpload = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.000001 ) then
	xpload = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
*     brakedown ( ibodies, idet, ip) = amax1 ( 0.0, xpload )
      endif ! if ( TGLODE2(idet,i13) .gt. 0.000001 )
	endif ! if ( QTYPE (idet) .ne. 4 )
      
 5502 continue ! idet = 1, ndet
      
      if ( itrap2 .eq. 1 ) then ! xbodies > 0.000001 .................... Ai.ADL
      do idet = 1, ndet   ! --------------------------------------------- Ai.ACL
      if ( qtype (idet) .ne. 4 ) then ! --------------------------------- Ai.ACL
      if ( xbodies (ibodies,idet) .gt. 0.000001 ) then
      xpload = 100.0*xbodies(ibodies,idet)/TWLOADS(ibodies,idet,i13) ! -- Ai.ACL
      call sort format 1 (xpload) ! ------------------------------------- Ai.ACL
      Tenchars3(idet) = valchars10 ! ------------------------------------ Ai.ACL
*     =================================================================== Ai.ACL
      write(220+idet,422)uname(jbodies),Tenchars1(idet), ! -------------- Ai.ACL
     &Tenchars2(idet),Tenchars3(idet),nameprop(ip) ! -------------------- Ai.ACL  
  422 format(5x,a40,5x,3a10,7x,'from ',a36) ! loads from catchments ----- Ai.ACL
*     =================================================================== Ai.ACL
      endif ! if ( xbodies (ibodies,idet) .gt. 0.000001 ) --------------- Ai.ACL
      endif ! if ( qtype (idet) .ne. 4 ) -------------------------------- Ai.ACL
      enddo ! do idet = 1, ndet ----------------------------------------- Ai.ACL
      endif ! if ( itrap2 .eq. 1 ) xbodies > 0.000001 ................... Ai.ADL
 5504 continue ! ibodies = 1, kount bodies
      do idet = 1, ndet ! ----------------------------------------------- Ai.ACL
      if ( qtype (idet) .ne. 4 ) then ! --------------------------------- Ai.ACL
      write(220+idet,1115) ! -------------------------------------------- Ai.ACL
      endif ! if ( qtype (idet) .ne. 4 ) -------------------------------- Ai.ACL
      enddo ! ----------------------------------------------------------- Ai.ACL
      endif ! if ( itrap2 .eq. 1 ) xbodies > 0.000001 ==========================

 6000 continue ! ip = 1, nprop +++++++++++++++++++++++++++++++++++++++++++++++++
           
      
      do idet = 1, ndet ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      if ( QTYPE (idet) .ne. 4 ) then ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      ihedd = 1
         do ip = 2, nprop + 1
         braketotal(ip) = 0.0  
         enddo
      grund total1 = 0.0

      
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      do ibodies = 1, kount bodies ! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX bodies
      rowtotal = 0.0
      jbodies = identify bodies (ibodies)
         do ip = 2, nprop + 1
         braketotal(ip) = braketotal(ip) + brakedown(ibodies,idet,ip)
         enddo
         do ip = 2, 16
         rowtotal = rowtotal + brakedown(ibodies,idet,ip) 
         enddo
      grund total1 = grund total1 + rowtotal 
      xone = amax1(0.0,brakedown(ibodies,idet,nprop+1)-rowtotal)
      xtwo = xone + rowtotal     
      if ( xtwo .gt. 1.0e-9 ) then ! OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      if ( ihedd .eq. 1 ) then ! ###############################################
      ihedd = 0
      endif ! if ( ihedd .eq. 1 ) then #########################################
      grund total2 = 0.0
      do ip = 2, nprop
      grund total2 = grund total2 + brakedown(ibodies,idet,ip)
      enddo
      endif ! if ( xtwo .gt. 1.0e-9 ) OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO     
      enddo ! do ibodies = 1, kount bodies XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX bodies
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*     if ( JT(feeture) .eq. 24 ) then ! +++++++++++++++++++++++++++++++++ Ai.ACL
*     write(220+idet,7970) ! -------------------------------------------- Ai.ACL
*    &(braketotal(ip),ip=2,16),grund total1,xtwo,braketotal(nprop+1)
*7970 format(140('-')/'Totals ...     ',15f6.1,f7.1,f6.1,f8.1/140('='))
*     endif ! if ( JT(feeture) .eq. 24 ) ++++++++++++++++++++++++++++++++ Ai.ACL
*     write(110+idet,7779)GIScode(feeture),unamex,uname(jbodies),
*    &dname(idet),(braketotal(ip),ip=2,16),grund total2,
*    &xone,braketotal(nprop+1)
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      
      if ( TGLODE2(idet,i13) .gt. 0.000001 ) then ! ################ gap filling
	TGX = TGLODE2(idet,i13) ! ################################################
*     TILOADUP2 ... load introduced by gap filling for river flows -------------
      pgf1 = 100.0 * TILOADUP2(idet,i13)/TGX
      pgf2 = 100.0 * TILOADDN2(idet,i13)/TGX
      pgf3 = 100.0 * TALOADUP2(idet,i13)/TGX
      pgf4 = 100.0 * TALOADDN2(idet,i13)/TGX
      pgf5 = abs (pgf1) + abs (pgf2) + abs (pgf3) + abs (pgf4)
      igf = 1
      if ( pgf5 .lt. 1.0e-09) igf = 0
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+idet,8779)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,pgf1,Length of main river ! ------------------------------- DDi.CSV
 8779 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'% Load added by gap filling ...',',for river flows', ! ---------- DDi.CSV
     &(',',1pe11.4),',9999,',0pf12.2, ! -------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,,,,,',0pf11.2)) ! --------------------------------- DDi.CSV
      write(210+idet,8729)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,pgf2,Length of main river ! ------------------------------- DDi.CSV
 8729 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'% Load removed by gap filling ...',',for river flows', ! -------- DDi.CSV
     &(',',1pe11.4),',-9999,',0pf12.2, ! ------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,,,,,',0pf11.2)) ! --------------------------------- DDi.CSV
      write(210+idet,8739)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,pgf3,Length of main river ! ------------------------------- DDi.CSV
 8739 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'% Load added by gap filling ...',',for river quality', ! -------- DDi.CSV
     &(',',1pe11.4),',9999,',0pf12.2, ! -------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,,,,,',0pf11.2)) ! --------------------------------- DDi.CSV
      write(210+idet,8749)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,pgf4,Length of main river ! ------------------------------- DDi.CSV
 8749 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'% Load removed by gap filling ...',',for river quality', ! ------ DDi.CSV
     &(',',1pe11.4),',-9999,',0pf12.2, ! ------------------------------- DDi.CSV
     &(',,,,,,,,,,,,,,,,,',0pf11.2)) ! --------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
     
      write(110+idet,2779)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),pgf1,jxtype  ! ---------------------------------------- Di.CSV
 2779 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load added by gap filling for river flows',',',a11, ! ---------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4) ! ------------------------------ Di.CSV
      write(110+idet,2729)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),pgf2,jxtype  ! ---------------------------------------- Di.CSV
 2729 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load removed by gap filling for river flows',',',a11, ! -------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4) ! ------------------------------ Di.CSV
      write(110+idet,2739)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),pgf3,jxtype  ! ---------------------------------------- Di.CSV
 2739 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load added by gap filling on river quality',',',a11, ! --------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4) ! ------------------------------ Di.CSV
      write(110+idet,2749)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),pgf4,jxtype ! ----------------------------------------- Di.CSV
 2749 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load removed by gap filling for river quality',',',a11, ! ------ Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4)  ! ----------------------------- Di.CSV
	endif ! if ( TGLODE2(idet,i13) .gt. 0.000001 ) ############### gap filling

      endif ! if ( QTYPE (idet) .ne. 4 ) TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      enddo ! do idet = 1, ndet ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      
      return
      end

