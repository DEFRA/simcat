*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Mass balance ...
*     Mix Features into Reaches ...
*     --------------------------------------------------------------------------
*     File name: Mass Balance.FOR (2346 lines)
*     --------------------------------------------------------------------------
*     If IFDIFFUSE equals 1, the discharge is from diffuse river sources ...
*     If IFDIFFUSE equals 2, the discharge is from diffuse effluent sources ...
*     --------------------------------------------------------------------------

      subroutine mass balance
      include 'COM.FOR'
	dimension efflows (NS), EQ(NS), efflows2(NS), RLC(nprop)

      do is = 1, NS ! ------------------ initialise the stores of effluent flows 
	efflows  (is) = 0.0
	efflows2 (is) = 0.0
	ecshots (is) = 0.0
      do id = 1, 4
      Creg(id,IS) = 0.0
      enddo
	enddo ! -------------------------- initialise the stores of effluent flows

*     perform mass balance at a single site ------------------------------------
*     mix a discharge into the river ... or mix a stream into the river --------
      FACT = 0.0 ! direct the addition of flows 
	if ( JP .eq. ndetlast ) FACT = 1.0

      call inititialise the stores for the estimates of load 
      
*     obtain the mean and standard deviation of the logged variables for the ---
*     discharge (or tribuary) flow ... But not (yet) for non-parametric --------
*     distributions or monthly distributions -----------------------------------
	call get the summary statistics of discharge flow
      call set up the shots for non parametric stream flow
      call set up the shots for non parametric discharge flow

*     obtain the mean and standard deviation of logged variables for the -------
*     quality of the discharge (or the tributary) ... But not (yet) for the for 
*     the non-parametric distributions -----------------------------------------
      call get the summary statistics of discharge quality (ECM,ECS)
      
*     use these to calculate correction factors for Monte Carlo errors ---------
      call bias in river or discharge flow and quality (ECM,ECS)
      call write the correction factors for Monte Carlo errors
*     finished for the ordinary distributions ----------------------------------

*     now deal with the non-parametric and monthly distributions ---------------
*     set up the correlation coeffiecients -------------------------------------
      if ( qtype (jp) .ne. 4 ) then ! ------------------------------------------
      if ( IQDIST .ge. 4 ) then ! ----------------------------------------------
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 ) then ! --------------------------
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 ) -------------------------
      endif ! if ( IQDIST .ge. 4 ) ---------------------------------------------
      endif ! if ( qtype (jp) .ne. 4 ) -----------------------------------------

*     call write the correlation coefficients
      call set up data for added flow and quality

      do is = 1, ns ! get the flows for the discharge or tributary -------------
      call get the correlated random numbers ( IS, R1, R2, R3, R4 )
      call get the flows of the stream or discharge ( IS, R3, EF )
	efflows  (is) = EF
	efflows2 (is) = EF
      enddo ! --------------------- got the flows for the discharge or tributary

*     prepare for monthly structure input of loads -----------------------------
	jqdist = 2
	if ( IQDIST .eq. 8 ) jqdist = struct0 (1)

      non zero flow = 0
      non zero load = 0
      non zero conc = 0
      non zero bofc = 0
      non zero both = 0

*     start the mass balance ... loop over all the shots -----------------------
      do 2 IS = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the correlated random numbers ----------------------------------------
      call get the correlated random numbers (IS,R1,R2,R3,R4)
      
      RF = FMS  (IS) ! retrieve the flow of the upstream river
      EFP = EFMS (IS) ! retrieve the proportion of effluent in each shot 
      RC = CMS ( JP, IS ) ! and for the upstream river quality
	do ip = 1, nprop ! and the contributions from sources of pollution
      RLC(ip) = LMS( ip, JP, IS )
	enddo
	EF = efflows (is) ! get the shots for the discharge (or tributary) flow 
      
      call get the quality of the discharge (IS,R4,EC,ECM)
      
      if ( EF .gt. 1.0e-09 ) non zero flow = non zero flow + 1
      if ( EC .gt. 1.0e-09 .and. EF .gt. 1.0e-09 ) then
      non zero both = nonzero both + 1
      endif
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then ! load ---
      non zero conc = non zero conc + 1
      non zero bofc = non zero bofc + 1
      if ( EC .gt. 1.0e-09 ) non zero load = non zero load + 1
      else
      non zero load = non zero load + 1
      if ( EC .gt. 1.0e-09 ) non zero conc = non zero conc + 1
      if ( EC .gt. 1.0e-09 .and. EF .gt. 1.0e-09 ) then
      non zero bofc = non zero bofc + 1
      endif
      endif
     
	EQ (IS) = EC
      
*     prepare to deal with the addition of data expressed as a load ------------
	EFMB = EF
*     for the diffuse inputs the flow data are per kilometre ------------------- 
      if ( ifdiffuse .gt. 0 ) then
	EFMB = EF * dint
	EF = EF * dint
      endif
      
*     deal with data that are expressed as a load ==============================
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 .or. ! load -----
     &     JQDIST .eq. 6 .or. JQDIST .eq. 7 .or. JQDIST .eq. 9 ) then
*     set the flows to 1.0 -----------------------------------------------------
	EFMB = 1.0
*     and scale the "concentrations" -------------------------------------------
      if ( ifdiffuse .gt. 0 ) then 
	EC = EC * dint
	endif ! if ( ifdiffuse .gt. 0 ) ------------------------------------------
      endif ! discharge quality expressed as load ==============================

*     store the quality of the discharge or stream ----------------- Richard III
      if ( QTYPE (JP) .ne. 4 ) then
      ECshots(IS) = EC
	else
      ECshots(IS) = 0.0
      endif
      
*     now do the Mass Balance ##################################################
	if ( RF + EF .gt. 1.0e-8 ) then ! ########################################
      CMS (JP,IS) = ( (RF * RC) + (EFMB * EC) ) / (RF + EF) ! ##################
      endif ! ##################################################################

*     store data for tests based on regression ---------------------------------      
      Creg(1,IS) = RF ! store the upstream river flow --------------------------
      Creg(2,IS) = RC ! upstream quality ---------------------------------------
      Creg(3,IS) = EF ! discharge flow -----------------------------------------
      Creg(4,IS) = EC ! discharge quality --------------------------------------

*     concentrations from various types of feature -----------------------------
*     point source inputs ------------------------------------------------------
	if ( RF + EF .gt. 1.0e-8 ) then
      if ( ifdiffuse .eq. 0 ) then
*     apply to all discharges (3) (5) (12) (39) ---------------- load categories
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39) then
      LMS(1,JP,IS) = ( (RF * RLC(1)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(1,JP,IS) = ( (RF * RLC(1)) ) / (RF + EF) ! dilute
	endif
*     apply to sewage works (3) -------------------------------- load categories
	if ( JT(feeture) .eq. 03) then
      LMS(2,JP,IS) = ( (RF * RLC(2)) + (EFMB * EC) ) / (RF + EF)
      do ip = 2, nprop 
      if ( ip .ne. 2 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
*     apply to intermittent discharges (12) -------------------- load categories
	if ( JT(feeture) .eq. 12) then
      LMS(3,JP,IS) = ( (RF * RLC(3)) + (EFMB * EC) ) / (RF + EF)
      do ip = 2, nprop 
      if ( ip .ne. 3 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
*     apply to industrial discharges (5) ----------------------- load categories
	if ( JT(feeture) .eq. 05 ) then
      LMS(4,JP,IS) = ( (RF * RLC(4)) + (EFMB * EC) ) / (RF + EF)
      do ip = 2, nprop 
      if ( ip .ne. 4 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
*     apply to mine waters (39) -------------------------------- load categories
	if ( JT(feeture) .eq. 39) then
      LMS(5,JP,IS) = ( (RF * RLC(5)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 5 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
*     streams (2)----------------------------------------------- load categories
	if ( JT(feeture) .eq. 02 ) then
      LMS(17,JP,IS) = ( (RF * RLC(17)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 17 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
      endif ! if ( ifdiffuse .eq. 0 )
*     ------------------------------------------------------ point source inputs 

*     diffuse inputs -----------------------------------------------------------
	if ( ifdiffuse .gt. 0 ) then
	if ( diffuse type .gt. 1 ) then
          
	if ( diffuse type .eq. 25 ) then ! ---------------- from livestock farming 
      LMS(6,JP,IS) = ( (RF * RLC(6)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 6 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
      if ( diffuse type .eq. 27) then ! -------------------- from arable farming
      LMS(7,JP,IS) = ( (RF * RLC(7)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 7 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 29) then ! ---------------------from highway runoff 
      LMS(8,JP,IS) = ( (RF * RLC(8)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 8 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 31) then ! ---------------------- from urban runoff
      LMS(9,JP,IS) = ( (RF * RLC(9)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 9 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 33) then ! ------------ from atmospheric deposition
      LMS(10,JP,IS) = ( (RF * RLC(10)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 10 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 35 ) then ! --------------- from natural background
      LMS(11,JP,IS) = ( (RF * RLC(11)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 11 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 37) then ! ---------------------- from septic tanks
      LMS(12,JP,IS) = ( (RF * RLC(12)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 12 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 40) then ! ------------------- from aggregated CSOs 
      LMS(13,JP,IS) = ( (RF * RLC(13)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 13 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 42) then ! ------------------- from aggregated STWs
      LMS(14,JP,IS) = ( (RF * RLC(14)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 14 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 46) then ! --------------------- from diffuse mines 
      LMS(15,JP,IS) = ( (RF * RLC(15)) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 15 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
	endif
	if ( diffuse type .eq. 48) then ! ---------- from birds, boats and angling
      LMS(16,JP,IS) = ( (RF * RLC(16) ) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 16 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
      endif
	if ( diffuse type .eq. 13) then ! --------------- type (13) diffuse inflow
      LMS(18,JP,IS) = ( (RF * RLC(18) ) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 18 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
      endif ! ----------------------------------------- type (13) diffuse inflow
	if ( diffuse type .eq. 15) then ! --------------- type (15) diffuse inflow
      LMS(19,JP,IS) = ( (RF * RLC(19) ) + (EFMB * EC) ) / (RF + EF)
      do ip = 1, nprop 
      if ( ip .ne. 19 ) then ! exclude streams (2) from this loop
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      endif
      enddo
      endif ! ----------------------------------------- type (15) diffuse inflow
      
      endif ! if ( diffuse type .gt. 0 )
      endif ! if ( ifdiffuse .eq. 1 ) --------------------------- diffuse inputs
	endif ! if ( RF + EF .gt. 1.0e-8 )

*     do the flow summation ... do this for the last determinand ---------------
      FMS(IS) = RF + FACT * EF
      dfms(IS) = RF + EF

*     calculate the proportion of effluent in each shot ------------------------
*     apply to (3) (5) (12) (39) ----------------------- proportions of effluent
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39) then
      EFMS(IS) = ( RF * EFP + EFMB ) / (RF + EF)
	else
      EFMS(IS) = ( RF * EFP ) / (RF + EF) ! dilute the effluents --------------
      endif

*     and calculate the load --------------------------------------------------
      if ( QTYPE (JP) .ne. 4 ) then
      K13 = imonth + 1
      if ( ifdiffuse .eq. 0 ) then
      XLD = ECshots(IS) * EFMB
      ELOAD (JP,I13) = ELOAD (JP,I13) + XLD
      ELOADS (JP) = ELOADS (JP) + XLD * XLD
      if ( JT(feeture) .eq. 3 ) ELOAD03 (JP,I13) = ELOAD03 (JP,I13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 5 ) ELOAD05 (JP,I13) = ELOAD05 (JP,I13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 12) ELOAD12 (JP,I13) = ELOAD12 (JP,I13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 39) ELOAD39 (JP,I13) = ELOAD39 (JP,I13) 
     &                                           + XLD
      ELOAD (JP,K13) = ELOAD (JP,K13) + XLD
      if ( JT(feeture) .eq. 3 ) ELOAD03 (JP,K13) = ELOAD03 (JP,K13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 5 ) ELOAD05 (JP,K13) = ELOAD05 (JP,K13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 12) ELOAD12 (JP,K13) = ELOAD12 (JP,K13) 
     &                                           + XLD
	if ( JT(feeture) .eq. 39) ELOAD39 (JP,K13) = ELOAD39 (JP,K13) 
     &                                           + XLD
      endif

      if ( ifdiffuse  .gt. 0 ) then
      if ( RF + EF  .gt. 1.0e-8 ) then
      diffuse load (JP,I13) = diffuse load (JP,I13) + ECshots(IS) * EFMB
      diffuse load (JP,K13) = diffuse load (JP,K13) + ECshots(IS) * EFMB
      endif ! if ( RF + EF  .gt. 1.0e-8 ) then
	endif ! if ( ifdiffuse  .gt. 0 ) then

	NSM (JP,I13) = NSM (JP,I13) + 1
	NSM (JP,K13) = NSM (JP,K13) + 1

      endif

    2 continue ! end of the main loop on shots

      do IS = 1,NS ! ------------------------------------------- check the shots 
      if (CMS (JP,IS) .gt. 1.0e9) then
      xxxxxx = CMS (JP,IS)
      ixxxxx = IS
      CMS(JP,IS) = 5.0 * C (JP,1)
      suppress9c = suppress9c + 1
      if ( suppress9c .lt. 5 ) then
      call sort format 1 (CMS (JP,IS))
      write(01,5282)ixxxxx,dname(JP),xxxxxx,valchars10,uname(feeture)
      write(33,5282)ixxxxx,dname(JP),xxxxxx,valchars10,uname(feeture)
 5282 format(
     &'*** Shot',i5,' greater than a billion for ',a11,'=',
     &f30.0,'     ***'/
     &'*** Re-set to',a10,' at: ',a37)
 	call change colour of text (50) ! steel blue
      write( *,5292)dname(JP),uname(feeture),valchars10
 5292 format(
     &'* Value > 10000000000 for ',a11,14x,'...',7x,
     &'at: ',a37,3x,'re-set to',a10,' .....')
      call set screen text colour
      else
      if ( suppress9c .gt. 9999 ) suppress9c = 0
      endif
      if ( suppress9c .eq. 5 ) then
      call sort format 1 (CMS (JP,IS))
      write(01,5582)ixxxxx,dname(JP),xxxxxx,valchars10,uname(feeture)
      write(33,5582)ixxxxx,dname(JP),xxxxxx,valchars10,uname(feeture)
 5582 format(
     &'*** Shot',i5,' greater than a billion for ',a11,'=',
     &f30.0,'     ***'/
     &'*** Re-set to',a10,' at: ',a37,' and eleswhere')
 	call change colour of text (22) ! light blue
      write( *,5592)dname(JP),uname(feeture)
 5592 format(
     &'* Value > 10000000000 for ',a11,14x,'...',7x,
     &'at: ',a37,3x,'and other cases ........')
      call set screen text colour
      endif
      endif
      enddo ! -------------------------------------------------- check the shots

      if ( n148 .eq. 1 ) then ! assess loads contributing to percentiles 5555555
      call identify the shots for percentiles (JP) ! 333333333333333333333333333
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or. ! 3333333333333333333
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39) then ! 3333333333333333
      if ( mark works .gt. 0 ) then ! save the loads for back-tracking 333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(kount works,JP,IS) = Creg(3,is) * Creg(4,is) ! store loads 333
      enddo ! 333333333333333333333333333333333333333333333333333333333333333333
      endif ! -------------------------------- saved the loads for back-tracking
      endif ! if ( JT(feeture) .eq. 3 etc ! 333333333333333333333333333333333333

*     diffuse inputs 55555555555555555555555555555555555555555555555555555555555
	if ( ifdiffuse .gt. 0 ) then ! 5555555555555555555555555555555555555555555
	if ( diffuse type .gt. 0 ) then ! 5555555555555555555555555555555555555555
	if ( diffuse type .eq. 25 ) ix = 6  ! ------------- from livestock farming 
      if ( diffuse type .eq. 27 ) ix = 7  ! ---------------- from arable farming
	if ( diffuse type .eq. 29 ) ix = 8  ! -----------------from highway runoff 
	if ( diffuse type .eq. 31 ) ix = 9  ! ------------------ from urban runoff
	if ( diffuse type .eq. 33 ) ix = 10 ! -------- from atmospheric deposition
	if ( diffuse type .eq. 35 ) ix = 11 ! ------------ from natural background
	if ( diffuse type .eq. 37 ) ix = 12 ! ------------------ from septic tanks
	if ( diffuse type .eq. 40 ) ix = 13 ! --------------- from aggregated CSOs 
	if ( diffuse type .eq. 42 ) ix = 14 ! --------------- from aggregated STWs
	if ( diffuse type .eq. 46 ) ix = 15 ! ----------------- from diffuse mines 
	if ( diffuse type .eq. 48 ) ix = 16 ! ------ from birds, boats and angling
*	if ( diffuse type .eq. 13 ) ix = 18 ! ---------- from diffuse inflows (13)
*	if ( diffuse type .eq. 15 ) ix = 19 ! ---------- from diffuse inflows (15)
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      TDLOADshots(kount bodies,JP,IS,ix) = Creg(3,is) * Creg(4,is) ! 55555555555
      enddo ! 555555555555555555555555555555555555555555555555555555555555555555
      endif ! if ( diffuse type .gt. 0 ) ! 5555555555555555555555555555555555555
      endif ! if ( ifdiffuse .gt. 0 ) ! 5555555555555555555555555555555555555555
      
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333355555555555555555555
      
*     calculate mean river flow and mean effluent flow -------------------------
      fmean = 0.0
	emean = 0.0
	do is = 1, NS
	fmean = fmean + fms (is)
	emean = emean + efflows2 (is)
	enddo
	fmean = fmean / float (NS) ! mean river flow -----------------------------
	emean = emean / float (NS) ! mean effluent flow --------------------------

      ECX = get effluent percentile (EQ)
      ECM2 = get the mean discharge quality (EQ)

*     calculate the loads ------------------------------------------------------
      if ( QTYPE (JP) .ne. 4 ) then
      if ( ifdiffuse .eq. 0 ) then ! ------- calculate the annual effluent loads
  
      CS = ELOADS (JP)
      CM = ELOAD (JP,I13)
      if ( CS .lt. 1.0E-25 ) then
      CS = 0.0
	else
	if ( CS .gt. 1.0E25 ) then
      CS = 1.0E25
	else
      CS = (CS-CM*CM/NS)/(NS-1)
	if ( CS .lt. 1.0E-20) CS = 1.0E-20
      CS = SQRoot3(321099,CS)
      endif
	endif
      CM = CM/NS
          
	ELOAD (JP,I13) = CM
	ELOADS (JP) = CS
      
	if ( JT(feeture) .eq. 3 ) ELOAD03 (JP,I13) = ELOAD03 (JP,I13) 
     &                                           / float(NS)
	if ( JT(feeture) .eq. 5 ) ELOAD05 (JP,I13) = ELOAD05 (JP,I13) 
     &                                           / float(NS)
	if ( JT(feeture) .eq. 12) ELOAD12 (JP,I13) = ELOAD12 (JP,I13) 
     &                                           / float(NS)
	if ( JT(feeture) .eq. 39) ELOAD39 (JP,I13) = ELOAD39 (JP,I13) 
     &                                           / float(NS)
	endif ! if ( ifdiffuse .eq. 0 ) ----- calculated the annual effluent loads
      if ( ifdiffuse .gt. 0 ) then ! --------------- calculate the diffuse loads
	diffuse load (JP,I13) = diffuse load (JP,I13) / float (NS)
      endif ! if ( ifdiffuse .gt. 0 ) ! ----------- calculated the diffuse loads
      
      if ( munthly structure .eq. 1 ) then ! - calculate  monthly effluent loads
      do J13 = 2, N13 ! -------------------------------- loop through the months
      if ( NSM(JP,J13) .gt. 1 ) then
      if ( ifdiffuse .eq. 0 ) then
	ELOAD (JP,J13) = ELOAD (JP,J13) / float (NSM (JP,J13))
	if ( JT(feeture) .eq. 3 ) ELOAD03 (JP,J13) = ELOAD03 (JP,J13) 
     &                                           / float(NSM (JP,J13))
	if ( JT(feeture) .eq. 5 ) ELOAD05 (JP,J13) = ELOAD05 (JP,J13) 
     &                                           / float(NSM (JP,J13))
	if ( JT(feeture) .eq. 12) ELOAD12 (JP,J13) = ELOAD12 (JP,J13) 
     &                                           / float(NSM (JP,J13))
	if ( JT(feeture) .eq. 39) ELOAD39 (JP,J13) = ELOAD39 (JP,J13) 
     &                                           / float(NSM (JP,J13))
	endif ! if ( ifdiffuse .eq. 0 ) ! ----- calculated  monthly effluent loads
      if ( ifdiffuse .gt. 0 ) then ! ------- calculate the monthly diffuse loads
	diffuse load (JP,J13)  = diffuse load (JP,J13) 
     &                       / float (NSM (JP,J13))
      endif ! if ( ifdiffuse .gt. 0 ) ! --- calculated the monthly diffuse loads
	endif ! if ( NSM(JP,J13) .gt. 1 )
      enddo ! do J13 = 2, N13 ... ---------------------- loop through the months
      endif ! -------------------------------------- calculate the monthly loads

      if ( MONF .gt. 1 ) then
	call write the shots for the flow from the discharge
 	call write the shots for the flow from the stream
	endif
 	if ( MONQ .gt. 1 ) then
	call write shots for discharge quality
      call write shots for stream quality  
 	endif
	endif ! if ( QTYPE (JP) .ne. 4 )

*     calculate the zero discharges of flow or load ----------------------------
      if ( nobigout .le. 0 ) then
      if ( JSKIP .eq. 0 ) then
	if ( ical .ne. 1 ) then
      if ( non zero both .lt. NS ) then
      if ( non zero flow .lt. NS .and. jp .eq. ndetfirst ) then
      pcfflow = 100.0 * (float (non zero flow) / float (NS) )
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      write(01,6522)
 6522 format(77('='))
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,5811) pcfflow
 5811 format('River flow  ... % of year with added flow:',f7.2/77('-'))
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
 6532 format(33x,77('='))
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,5813) pcfflow
 5813 format(33x,'River flow  ... % of year with added flow:',f7.2/
     &33x,77('-'))
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( non zero flow .lt. NS .and. jp .eq. ndetfirst )

      if ( non zero load .lt. NS ) then
      pcfload = 100.0 * (float (non zero load) / float (NS) )
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      write(01,6522)
      jhead = 1
      endif
      write(01,5812) dname(JP),pcfload
 5812 format(a11,' ... % of year with added load:',f7.2)
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,5814) dname(JP),pcfload
 5814 format(33x,a11,' ... % of year with added load:',f7.2)
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( non zero load .lt. NS )

      if ( non zero conc .lt. NS ) then
      pcfconc = 100.0 * (float (non zero conc) / float (NS) )
	if ( pcfflow .gt. 1.0e-8 .and. pcfconc .gt. 1.0e-8 ) then 
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      write(01,6522)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,6822) dname(JP),pcfconc
 6822 format(a11,' ... % of year with added conc:',f7.2)
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
      jhead = 1
      endif ! if ( jhead .eq. 0 )  
      write(01,6824) dname(JP),pcfconc
 6824 format(33x,a11,' ... % of year with added conc:',f7.2)
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( pcfflow .gt. 1.0e-8  etc
	endif ! if ( non zero conc .lt. NS )
      if ( non zero bofc .lt. NS ) then
      pcfbofc = 100.0 * (float (non zero bofc) / float (NS) )
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      write(01,6522)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,7829) dname(JP),pcfbofc
 7829 format(a11,' ... % of year with added load:',f7.2)
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      write(01,7894) dname(JP),pcfbofc
 7894 format(33x,a11,' ... % of year with added load:',f7.2)
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( non zero bofc .lt. NS )

      endif ! if ( non zero both .lt. NS )
      endif ! if ( ical .ne. 1 )
      endif
      endif ! if ( nobigout .le. 0 )
      
      return
      end



*     special mass balance for globally changed discharge data -----------------
*     perform mass balance at a single site ------------------------------------
*     mix a discharge into the river .... or mix a stream into the river -------
      subroutine mass balance quick
      include 'COM.FOR'
	dimension efflows (NS)
      dimension CMS2(NS)

      do is = 1, NS
	efflows(is) = 0.0
	CMS2(is) = 0.0
	enddo

*     Obtain the mean and standard deviation of logged variables for -----------
*     discharge flow ... But not for non-parametric distributions ... ----------
      call get the summary statistics of discharge flow

*     Is there no bias correction for these flows? ----------------- Richard III
*     and discharge quality. But not for non-parametric distributions ----------

      if ( IQDIST .lt. 4 .or. IQDIST .eq. 6 .or. IQDIST .eq. 7 ) then
      call get the summary statistics of discharge quality (ECM,ECS)
      call bias in river or discharge flow and quality (ECM,ECS)
      call write the correction factors for Monte Carlo errors
      endif

*     set up the correlation ....  ( also done in BIAS ) -----------------------
      if ( IQDIST .gt. 4 .and. qtype (jp) .ne. 4 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif

      call set up data for added flow and quality

*     get the flows for the discharge or tributary -----------------------------
      do is = 1, ns
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      call get the flows of the stream or discharge ( IS, R3, EF )
	efflows (is) = EF
	enddo

*     start the mass balance.  Loop over all the shots
      do 2 IS = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     prepare for monthly structure input of loads -----------------------------
	jqdist = 2
	if ( IQDIST .eq. 8 ) then ! monthly structure
	jqdist = struct0 (imonth)
	endif ! monthly structure

*     get the correlated random numbers ----------------------------------------
      call get the correlated random numbers (IS, R1, R2, R3, R4)

*     retrieve the flow of the upstream river ----------------------------------
      RF = FMS (IS)

*     and the upstream river quality -------------------------------------------
      RC = CMS ( JP, IS )

*     get the shots for the discharge (or tributary) ---------------------------
      call get the flows of the stream or discharge ( IS, R3, EF )

*     deal with addition of load but not flow ----------------------------------
	EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then ! load ---
 	EFMB = 1.0
      endif
*     if ( model number in batch .ne. 1 ) 
*    &write( *,*)'003 get the quality of the discharge',IS,JP,EC
      call get the quality of the discharge (IS,R4,EC,ECM)

*     now do the Mass Balance --------------------------------------------------
	if ( RF + EF .gt. 1.0e-8 ) then
      CMS2(IS) = ( (RF * RC) + (EFMB * EC) ) / (RF + EF)
      endif

    2 continue

      call STATCH (CMS2,JP)

      return
      end


      subroutine STATCH(Y,JDET)
      include 'COM.FOR'
      dimension Y(MS)

      C1=0.0
      C2=0.0
      C3=0.0
      C4=0.0
      C5=0.0

   16 continue
      CM=0.0
      CS=0.0
      do IS=1,NS
      CM=CM+Y(IS)
      CS=CS+Y(IS)*Y(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 continue
      CS=SQRoot(1055,CS)
    6 continue
      CM=CM/NS

      goto (10,10,11,99,11,10), QTYPE(JDET)

*     compute 95-percentile ----------------------------------------------------
   10 continue
      do 2 I = 1,k90
      do 3 J = I+1,NS
      if (Y(I) .gt. Y(J)) goto 3
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
    3 continue
    2 continue
      goto 12

*     compute 5-percentile -----------------------------------------------------
   11 continue
      do 22 I=1,k90
      do 23 J=I+1,NS
      if (Y(I) .lt. Y(J)) goto 23
      Ctemp=Y(I)
      Y(I)=Y(J)
      Y(J)=Ctemp
   23 continue
   22 continue
   12 continue

      C1 = amax1 (0.0, CM )
      C2 = CS
      C3 = amax1 (0.0,Y(k95))
      C4 = amax1 (0.0,Y(k90))
      C5 = amax1 (0.0,Y(k99))

   99 continue

      return
      end

*     dompute the moments of the discharge flow distribution -------------------
      subroutine get the summary statistics of discharge flow
      include 'COM.FOR'

      GEFM = 0.0
      GEFS = 0.0
      if ( IFDIST .gt. 3 ) return

*     normal distribution ------------------------------------------------------
      if ( IFDIST .eq. 1 ) then
      GEFM = EFM
      GEFS = EFS
	endif

*     uniform distribution -----------------------------------------------------
      if ( IFDIST .eq. 0 ) then
      GEFM = 0.0
      GEFS = 0.0
	endif

*     log-normal distribution ------------------------------------------
      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then
      EM3 = EFM + EF3
      if ( EM3 .lt. 1.0e-9 ) return
      GEFM = ALOG((EM3*EM3)/SQRoot(1057,EM3*EM3+EFS*EFS))
	TRAK = ALOG(1.+(EFS*EFS)/(EM3*EM3))
	if ( TRAK .gt. 1.0e-20) then
      GEFS = SQRoot(105656,TRAK)
	endif
      endif

      return
      end

*     compute moments of discharge quality -------------------------------------
      subroutine get the summary statistics of discharge quality (XM,XS)
      include 'COM.FOR'
      
      GECM = XM
      GECS = XS
      if ( IQDIST .eq. 1 .or. IQDIST .eq. 6 .or. IQDIST .eq. 9 ) return 
      GECM = 0.0
      GECS = 0.0
      if ( IQDIST .eq. 0 ) return
      EM3 = XM + EC3
      EM4 = EM3 * EM3 + XS * XS
      if (EM4 .lt. 1.0E-20) return
      GECM = ALOG((EM3*EM3)/SQRoot(195854,EM4))
      xxx = ALOG(1.+(XS*XS)/(EM3*EM3))
      if ( xxx .lt. 1.0e-20 ) return
      GECS = SQRoot3(195901,xxx)
      
      return
      end

*     compute square root and check for error ----------------------------------
      function SQRMB(IX,X)
      if ( x .ge. 0.0 ) goto 1
      y = SQRT(1-x)
      write( *,2)X,Y,IX
      write(01,2)X,Y,IX
      write(03,2)X,Y,IX
    2 format('ILLEGAL ROOT IN MASS-BALANCE......',2E13.4,i4)
	call stop
    1 SQRMB = SQRT(X)
      return
      end

*     compute square root and check for error ----------------------------------
      function SQRoot (Jdentity,X)
      if ( x .lt. 1.0E-20) X=1.0E-20
      if ( x .ge. 1.0e20 ) X=1.0E20  
      SQRoot = SQRT(X)
      return
      end

*     compute square root and set to zero --------------------------------------
      function SQRoot1 (Jdentity,X)
	if ( x .lt. 1.0E-20 ) X=1.0E-20
      if ( x .ge. 1.0e20 ) X=1.0E20  
      SQRoot1 = SQRT(X)
      return
      end

*     compute square root and set to zero --------------------------------------
      function SQRoot2 (Jdentity,X,itest)
      itest = 0
	if ( x .lt. 1.0E-20) then
	X = 1.0E-20
	itest = 1
	endif
      if ( X .ge. 1.0e20 ) then
      X = 1.0E20
	itest = 1
      endif
      SQRoot2 = SQRT(X)
      return
      end

*     compute square root and check for error ----------------------------------
      function SQRoot3 (Jdentity,X)
	if ( X .lt. 1.0E-20) X = 1.0E-20
      if ( X .ge. 1.0e20 ) X = 1.0E20  
      SQRoot3 = SQRT(X)
      return
      end


*     compute errors in random samplings of distributions for use as -----------
*     correction factors -------------------------------------------------------
      subroutine bias in river or discharge flow and quality (XM,XS)
      include 'COM.FOR'
      dimension XX(4)

*     3 for flow; 4 for concentration -----------------------------------
      do j = 3,4
      BM(j) = 0.0
      BS(j) = 0.0
      enddo
      do IS = 1, NS
      EFshots(IS) = 0.0
      enddo      

      call set up the correlation coefficients ! 4444444444444444444444444444444

      do 2 IS = 1,NS
      XX(3) = 0.0
	XX(4) = 0.0

      call get the correlated random numbers (IS, R1, R2, R3, R4)

*     ------ 0  1  2  3  4  5  6  7  8  9 10 ------------------------------------
      goto (11,12,13,13,49,49,49,49,49,49,14), IFDIST + 1

*     constant -----------------------------------------------------------------
   11 XX(3) = GEFM
      goto 19

*     normal distribution ------------------------------------------------------
   12 XX(3) = GEFM+R3*GEFS-EF3
      goto 19

*     log-normal distribution --------------------------------------------------
   13 XX(3) = EXP(GEFM+R3*GEFS)-EF3
      goto 19

*     power curve parametric distribution 99999999999999999999999999999999999999
   14 XX(3) = EXP(GEFM+R3*GEFS)-EF3
      
   19 continue

*     store the uncorrected river flows ----------------------------------------
*     use these to calculate the correction factors ----------------------------
      EFshots(IS) = XX(3)

*     non-parametric, seasonal and times series --------------------------------
   49 continue
      
*     concentration or load ----------------------------------------------------
*     ------ 0  1  2  3  4  5  6  7  8  9 10 -----------------------------------
      goto (21,22,23,23,39,39,22,23,39,39,24), IQDIST + 1

*     constant -----------------------------------------------------------------
   21 XX(4) = GECM
      goto 29

*     normal distribution ------------------------------------------------------
   22 XX(4) = GECM+R4*GECS-EC3
      goto 29

*     log-normal distribution --------------------------------------------------
   23 XX(4) = EXP(GECM+R4*GECS)-EC3
      goto 29

*     power curve parametric distribution 99999999999999999999999999999999999999
   24 XX(4) = EXP(GECM+R4*GECS)-EC3

   29 continue
      BM(3) = BM(3) + XX(3)
      BS(3) = BS(3) + XX(3) * XX(3)
      BM(4) = BM(4) + XX(4)
      BS(4) = BS(4) + XX(4) * XX(4)
      
*     non-parametric, seasonal and monthly structure data ----------------------
   39 continue   
    2 continue

*     compute mean and standard deviation --------------------------------------
      do 6 J = 3,4
      BS(J) = (BS(J)-BM(J)*BM(J)/NS)/(NS-1)
      if (BS(J) .gt. 1.0E-10) goto 9
      BS(J) = 0.0
      goto 6
    9 continue
      BS(J) = SQRoot(1057,BS(J))
      BM(J) = BM(J)/NS
    6 continue

      call get statistics for the flows of the discharge or stream

      do is = 1, NS
	EFshots (is) = 0.0
	enddo
      
      if ( feeture .eq. 0 ) return
      if ( jt(feeture) .eq. 2 ) BS(3) = EFlow(2)

*     compute correction factors for discharge flow ----------------------------
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) Bxx = EF5/BS(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3)
*     compute correction factors for discharge quality ----------------------------
      if ( BM(4) .gt. 1.0E-08 ) BM(4) = XM/BM(4)
      if ( BS(4) .gt. 1.0E-08 ) BS(4) = XS/BS(4)
      
	if ( jt(feeture) .eq. 2 ) BS(3) = BXX
      
      return
      end



*     compute percentile of effluent quality -----------------------------------
      subroutine get effluent quality 95 percentile
      include 'COM.FOR'
      dimension EQ(NS)

      ECX = 0.0
      if (ECM .lt. 1.0E-10) return

      call get the summary statistics of discharge quality (ECM,ECS)
      call bias in river or discharge flow and quality (ECM,ECS)

*     set up the correlation ....  (also done in BIAS ) ------------------------
      if ( IQDIST .gt. 4 .and. qtype (jp) .ne. 4 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif

*     set up and write out the shots from the non-parametric distributions -----
      call set up the shots for non parametric discharge flow
      call set up the shots for non parametric discharge quality

*     set up and write out the shots for monthly data --------------------------
      call set up monthly data for discharge flow
      call set up monthly data for discharge quality

*     set up and write out the shots for monthly data --------------------------
      call set up monthly structure for discharge flow 8
      call set up monthly structure for discharge quality

*     sample the distribution --------------------------------------------------
      EX = 0.0
      do IS = 1,NS
      call get the quality of the discharge (IS,R4,EC,ECM)
      EQ(IS) = EC
	EX = EX + EC
      enddo
      
      ECX = get effluent percentile (EQ) ! find the percentile -----------------
      
      if ( MONQ .eq. 2 ) then
      do IS = 1, NS
      ECSHOTS(IS) = EQ(IS)
      enddo
      call write shots for discharge quality
      endif

      return
      end


*     COMPUTE RIVER QUALITY DOWNSTREAM OF CURRENT EFFLUENT QUALITY -------------
      subroutine get river quality downstream of the effluent 
      include 'COM.FOR'
      dimension RQ(MS)

      current downstream quality = 0.0
    
      call get the summary statistics of discharge flow
      call get the summary statistics of discharge quality (ECM,ECS)
      call bias in river or discharge flow and quality (ECM,ECS)
      call set up the correlation coefficients ! 4444444444444444444444444444444

*     sample the distributions -------------------------------------------------
      do 2 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)

      RF = FMS(IS)
      RC = CMS(JP,IS)

*     get the shots for the discharge (or tributary) ---------------------------
      call get the flows of the stream or discharge ( IS, R3, EF )

*     deal with addition of load but not flow ----------------------------------
	EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then ! load ---
 	EFMB = 1.0
	endif

      call get the quality of the discharge (IS,R4,EC,ECM)

*     mass balance -------------------------------------------------------------
      RQ(IS) = ( (RF*RC) + (EFMB*EC) ) / ( RF + EF )
    2 continue

*     compute the statistics of river quality ... find the percentile ----------
      current downstream quality = Get river percentile (RQ)
*     find the mean ------------------------------------------------------------
	if ( MRQS(JP) .eq. 1 ) current downstream quality = 
     &get the mean river quality (RQ)

      return
      end



*     find the percentile for river quality ------------------------------------
      function get river percentile (EQ)
      include 'COM.FOR'
      dimension Q(NS),EQ(NS)
      
      do is = 1,NS
      Q(IS) = EQ(IS)
      enddo

      kkk = k95
	if ( kptarg .eq. 90 ) kkk = k90
	if ( kptarg .eq. 95 .and. detype (JP) .eq. 104 ) kkk = k05
	if ( kptarg .eq. 90 .and. detype (JP) .eq. 104 ) kkk = k10

      if ( detype (JP) .ge. 900 ) then ! dissolved and solid metal
      if ( partition (JP) .gt. 0.01 ) then
      gkay = 10.0 ** partition (JP)
      do IS = 1, NS
      spm = BMS(2,IS)
	totalx = Q(IS)
	diss = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal
	Q(IS) = diss
	enddo
	endif
	endif ! dissolved and solid metal

      do I = 1, kkk
      do 2 J = I+1,NS
      if (Q(I) .gt. Q(J)) goto 2
      TEMP = Q(I)
      Q(I) = Q(J)
      Q(J) = TEMP
    2 continue
      enddo

      get river percentile = Q(kkk)

      return
      end


*     find the percentile for effluent quality ---------------------------------
      function get effluent percentile (EQ)
      include 'COM.FOR'
      dimension Q(NS),EQ(NS)

      do is = 1, NS
      Q(is) = EQ(is)
      enddo
      
      kkk = k95
	if ( detype (JP) .eq. 104 ) kkk = k05

      do I = 1, kkk
      do 2 J = I + 1, NS
      if (Q(I) .gt. Q(J)) goto 2
      TEMP = Q(I)
      Q(I) = Q(J)
      Q(J) = TEMP
    2 continue
      enddo
      
      get effluent percentile = Q(kkk)

      return
      end


*     calculate the average ----------------------------------------------------
      function get the mean river quality (Q)
      include 'COM.FOR'
      dimension Q(NS)
      TEMP = 0.0
      do I = 1,NS
      TEMP = TEMP + Q(I)
      enddo
      if ( detype (JP) .ge. 900 .and. partition (JP) .gt. 0.01 ) then
      TEMP = 0.0
      gkay = 10.0 ** partition (JP)
      do IS = 1, NS
      spm = BMS(2,IS)
	totalx = Q(IS)
	diss = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal
      TEMP = TEMP + diss
      enddo
	endif ! dissolved and solid metal
      get the mean river quality = TEMP/FLOAT(NS)
      return
      end


      function get the mean discharge quality (Q)
      include 'COM.FOR'
      dimension Q(NS)
      TEMP = 0.0
      do I = 1,NS
      TEMP = TEMP+Q(I)
      enddo
      get the mean discharge quality = TEMP/FLOAT(NS)
      return
      end


*     --------------------------------------------------------------------------
*     Routine to solve equation by an iterative scheme -------------------------
*     Each guess is held in Z --------------------------------------------------
*     The two last values of Z are stored in H...
*     The two last values from the equation are held in G...
*     The values in H and G are used to extimate subsequent values
*        of Z....
*     The iterations continue until the calculated value of Z
*        produces the value TARGET when substituted into
*        the equation.
*     --------------------------------------------------------------------------
      subroutine ITRATE (Icheck, TARGET)
      include 'COM.FOR'
      dimension G(2),H(2)

	Icheck = 0
      IT = 0 ! counter of iterations

      call get effluent quality 95 percentile ! first starting evaluation
      H(1) = ECM ! store first guess of the mean effluent quality
      G(1) = ECX ! store the corresponding 95-percentile
      ECM = 0.98 * ECM ! next guess of the mean effluent quality
	ECS = ECV * ECM ! standard deviation
      call get effluent quality 95 percentile ! second evaluation
      H(2) = ECM ! store second guess of of the mean effluent quality
      G(2) = ECX ! store corresponding 95-percentile
      goto 7 ! proceed to the third guess

    2 IT = IT + 1 ! update iteration counter
      if ( IT .gt. 99 ) goto 8

*     get ready to over-write worst of stored results --------------------------
      JJ = 1
      if ( ABS(G(2)-TARGET) .gt. ABS(G(1)-TARGET) )JJ = 2
      H(JJ) = ECM
      G(JJ) = ECX

*     use linear interpolation to get next value of Z --------------------------
    7 ECM = H(2)+(TARGET-G(2))*(H(2)-H(1))/(G(2)-G(1))
      ECM = AMAX1 (1.0e-10,ECM) ! next guess for effluent mean
	ECS = ECV * ECM ! standard deviation

      call get effluent quality 95 percentile ! evaluate function --------------
      R = ECX

*     test for convergence in reaching target ----------------------------------
      CHECK = R/TARGET
      if (CHECK .gt. 0.99995 .and. CHECK .lt. 1.00005) goto 79
      goto 2

    8 continue
      Icheck = 999

   79 TARGET = ECX
      ECS = ECM*ECV

      return
      end


*     write out the details of the non-standard correlations -------------------
      subroutine list the correlation coefficients
      include 'COM.FOR'

      icheck = 0
      ITYPE = JT(feeture) ! identify the Feature Type 

*     mixing of streams with the main river ....
      if ( ITYPE .eq. 2 ) then

*     river quality on river flow ----------------------------------------------
      if ( CO1 .ne. 0.0 .and. CO1 .ne. RFCL(JP) ) then
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write(01,71) CO1, Dname(jp)
   71 format('         Correlation of river flow on river quality = ',
     &F7.4,' for ',a11)
      endif

*     river flow on stream flow ------------------------------------------------
      if ( CO2 .lt. 0.9999 ) then
      if ( icheck .eq. 0 ) icheck = 1
      if ( JP .eq. ndetfirst ) write (1,62) CO2
   62 format('           Correlation of river flow on stream flow = ',
     &F7.4)
      endif

      if ( CO3 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write(01,73) CO3,Dname(jp)
   73 format('        Correlation of river flow on stream quality = ',
     &F7.4,' for ',a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444

      if ( CO4 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      write(01,74) CO4,Dname(jp)
   74 format('        Correlation of river quality on stream flow = ',
     &F7.4,' for ',a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444

      if ( CO5 .ne. 0.0 .and. CO5 .ne. RFCL(JP) ) then
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write(01,75) CO5,Dname(jp)
   75 format('       Correlation of stream flow on stream quality = ',
     &F7.4,' for ',a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444

      if ( CO6 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write(01,76) CO6,Dname(jp)
   76 format('     Correlation of river quality on stream quality = ',
     &F7.4,' for ',a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444
      endif

*     mixing discharges from sewage treatment works ----------------------------
*     or from industry ---------------------------------------------------------
*     apply to (3) (5) (12) (39) ---------------------------------------- mixing
      if ( itype .eq. 3 .or. itype .eq. 5 .or. itype .eq. 12 .or. 
     &itype .eq. 39) then

*     river quality on river flow ----------------------------------------------
      if ( CO1 .ne. 0.0 .and. CO1 .ne. RFCL(JP) ) then
      if ( icheck .eq. 0 ) icheck = 1
      write(01,51) CO1, Dname(jp)
   51 format('        Correlations for river flow on river quality = ',
     &F7.4,3x,a11)
      endif

      if ( itype .eq. 5 ) then
      if ( CO2 .ne. 0.0 ) then
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write (1,72) CO2
   72 format('       Correlations for river flow on discharge flow = ',
     &F7.4)
	endif
	else
      if ( CO2 .ne. 0.6 ) then
      if ( icheck .eq. 0 ) icheck = 1
      if ( nobigout .le. 0 ) write (1,72) CO2
      endif
	endif

      if ( CO3 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      write(01,43) CO3,Dname(jp)
   43 format('    Correlations for river flow on discharge quality = ',
     &F7.4,3x,a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444

      if ( CO4 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      write(01,44) CO4,Dname(jp)
   44 format('    Correlations for river quality on discharge flow = ',
     &F7.4,4x,a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444

      if ( CO5 .ne. 0.0 .and. CO5 .ne. EFCL(JP) ) then
      if ( icheck .eq. 0 ) icheck = 1
      write (01,45)CO5,Dname(jp)
   45 format('Correlations for discharge flow on discharge quality = ',
     &F7.4,3x,a11)
      endif

      if ( CO6 .ne. 0.0 ) then ! 44444444444444444444444444444444444444444444444
      if ( icheck .eq. 0 ) icheck = 1
      write (1,46)CO6,Dname(jp)
   46 format(' Correlations for river quality on discharge quality = ',
     &F7.4,3x,a11)
      endif ! 444444444444444444444444444444444444444444444444444444444444444444
	endif

      if ( icheck .eq. 0 ) then
*     write (01,86)Dname(jp)
   86 format(32x,'Default correlations used for ... ',a11)
	endif

      return
      end



*     set up the shots from monthly data on discharge flow ---------------------
	subroutine set up monthly data for negative discharge
      include 'COM.FOR'

*     if ( jp .eq. ndetfirst ) then
      if ( IFDIST .eq. 5 ) then

*     for a stream -------------------------------------------------------------
	if ( JT (KFEAT) .eq. 18) call generate monthly stream flow data

*     or for effluents ---------------------------------------------------------
      if ( JT (KFEAT) .eq. 19 ) call monthly discharge flow
      endif
*	endif

*     set up shots from monthly data on discharge quality ----------------------
	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 5 ) then

*     the stream ---------------------------------------------------------------
	if ( JT (KFEAT) .eq. 18 ) call monthly stream quality
      call write out the added flow shots

*     effluents ----------------------------------------------------------------
      if ( JT (KFEAT) .eq. 19 ) call monthly discharge quality
      call write out the added flow shots
      endif
	endif

	return
	end




*     retrieve the flow of the discharge or the tributary ----------------------
	subroutine get the flows of the stream or discharge ( IS, R3, EF )
      include 'COM.FOR'
 
*     constant flow ------------------------------------------------------------
      if ( ifdist .eq. 0 ) then
      EF = EFM
	EFshots (IS) = EF
	return
	endif ! if ( ifdist .eq. 0 ) ! constant flow -----------------------------

*     normal distribution ------------------------------------------------------
      if ( ifdist .eq. 1 ) then
      EF = Vnorm ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
	EFshots (IS) = EF
	return
	endif ! if ( ifdist .eq. 1 ) ! normal distribution -----------------------

*     log-normal distribution for effluent quality (2 and 3-parameter) ---------
      if ( ifdist .eq. 2 .or. ifdist .eq. 3 ) then
      if ( munthly structure .eq. 0 ) then
*     effluent type: based on mean and standard deviation ----------------------
      if ( jt(feeture) .eq. 3  .or. jt(feeture) .eq. 5  .or.
     &     jt(feeture) .eq. 22 .or. jt(feeture) .eq. 23 .or.
     &     jt(feeture) .eq. 19 .or. jt(feeture) .eq. 12 .or.
     &     jt(feeture) .eq. 39 ) then
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
	endif
*     river type: based on mean and 95-percentile ------------------------------
      if ( ifdiffuse .eq. 0 ) then
      if ( jt(feeture) .eq. 2  .or. 
     &     jt(feeture) .eq. 20 .or. jt(feeture) .eq. 21 .or.
     &     jt(feeture) .eq. 18 ) then
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
	endif ! non-diffuse additions 
      if ( ifdiffuse .eq. 1 ) then ! river-type diffuse
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
      if ( ifdiffuse .eq. 2 ) then ! effluent type diffuse
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
      endif
	EFshots (IS) = EF
	return
      endif ! if ( munthly structure .eq. 0 ) ----------------------------------
      if ( munthly structure .gt. 0 ) then
      EF = EFshots (IS)
      return
	endif ! if ( munthly structure .gt. 0 ) ----------------------------------
      endif ! if ( ifdist .eq. 2 .or. ifdist .eq. 3 )  log-normal --------------

      
*     power curve parametric distribution (stream or discharge flow) 99999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( ifdist .eq. 10 ) then
*     effluent type: based on mean and standard deviation ----------------------
      if ( jt(feeture) .eq. 3  .or. jt(feeture) .eq. 5  .or.
     &     jt(feeture) .eq. 22 .or. jt(feeture) .eq. 23 .or.
     &     jt(feeture) .eq. 19 .or. jt(feeture) .eq. 12 .or.
     &     jt(feeture) .eq. 39 ) then
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
	endif ! if ( ifdist .eq. 10 ) 99999999999999999999999999999999999999999999
*     river type: based on mean and 95-percentile 999999999999999999999999999999
      if ( ifdiffuse .eq. 0 ) then ! non-diffuse flows 9999999999999999999999999
      if ( jt(feeture) .eq. 2  .or. 
     &     jt(feeture) .eq. 20 .or. jt(feeture) .eq. 21 .or.
     &     jt(feeture) .eq. 18 ) then
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
	endif ! non-diffuse additions 99999999999999999999999999999999999999999999
      if ( ifdiffuse .eq. 1 ) then ! river-type diffuse flows 999999999999999999
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
      if ( ifdiffuse .eq. 2 ) then ! effluent type diffuse flows 999999999999999
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
      endif ! if ( ifdiffuse .eq. 2 ) 999999999999999999999999999999999999999999
	EFshots (IS) = EF ! store the outcome 999999999999999999999999999999999999
	return
      endif ! if ( ifdist .eq. 10 ) power curve parametric distribution 99999999
      endif ! if ( n149 .eq. 1 ) power curve parametric distribution 99999999999


*     non-parametric, monthly or structured distribution -----------------------
      if ( ifdist .eq. 4 .or. ifdist .eq. 5 .or. ifdist .eq. 8) then
      EF = EFshots (IS)
      return
	endif

      if ( ifdist .gt. 9 ) then ! illegal flow distribution --------------------
      EF = EFshots (IS)
	write( *,*)'*** Illegal code for flow distribution ...'
	return
	endif ! if ( ifdist .gt. 8 ) ---------------------------------------------

	end


      subroutine get the quality of the discharge (IS,R4,EC,XMEAN)
      include 'COM.FOR'

      EC = 0.0
	if ( qtype (jp) .eq. 4 ) return
      call get the correlated random numbers (IS, R1, R2, R3, R4)

*     log-normal distribution (2 and 3-parameter) ------------------------------
      if ( IQDIST .eq. 2 .or. IQDIST .eq. 3 ) then
      EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN)
      return
      endif

*     power curve parametric distribution (discharge quality) 999999999999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( IQDIST .eq. 10 ) then ! power curve parametric distribution 999999999
      EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN)
      return
	endif ! type 10 - power curve parametric distribution 99999999999999999999
      endif ! if ( n149 .eq. 1 ) power curve parametric distribution 99999999999

*     normal distribution ------------------------------------------------------
      if ( IQDIST .eq. 1 ) then
      EC = Vnorm ( R4, GECS, GECM, EC3, BM(4),BS(4), XMEAN )
      return
	endif

*     constant quality ---------------------------------------------------------
      if ( IQDIST .eq. 0 ) then
      EC = ECM
      return
	endif

*     normal distribution of loads ---------------------------------------------
      if ( IQDIST .eq. 6 ) then ! normal loads
      EC = Vnorm ( R4, GECS, GECM, EC3, BM(4),BS(4), XMEAN )
      return
	endif

*     log-normal distribution (2 and 3-parameter) - loads ----------------------
      if ( IQDIST .eq. 7 ) then ! log normal loads
      EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN)
      return
	endif

*     non-parametric distribution ----------------------------------------------
      if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then
      EC = ECshots(IS)
      return
	endif

*     monthly distribution -----------------------------------------------------
      if ( IQDIST .eq. 5 ) then
      EC = ECshots(IS)
      return
	endif

      if ( IQDIST .eq. 8 ) then ! monthly structure
      EC = ECshots(IS)
      return
      endif
      
      if ( IQDIST .gt. 9 ) then
      EC = ECshots(IS)
	write( *,*)'*** Illegal code for flow distribution ...'
      call stop
	return
	endif

*     do I need to add distribution types 6,7 or 9 ... loads ? ----- Richard III
	return
	end


      subroutine write out the added flow shots
      include 'COM.FOR'
      if ( JP .eq. ndetfirst ) then
      if ( MONF .gt. 1 ) then
	call write the shots for the flow from the discharge
	call write the shots for the flow from the stream
	endif
 	endif
      return
	end

      subroutine write the correction factors for Monte Carlo errors
      include 'COM.FOR'

      if ( Qtype (JP) .ne. 4 ) then
 	if ( MONF .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,128)BM(3),BS(3)
  128 format(77('-')/'Flows ...',6x
     &'correction factors for Monte-Carlo sampling: ',
     &'log-normal flows ... '/77('-')/
     &'Mean          =',F8.3/
     &'95-percentile =',F8.3/77('-'))
 	endif

      if ( MONQ .gt. 1 ) then
	if ( if diffuse .eq. 0 ) then
	if ( nobigout .le. 0 ) write(01,13)Dname(JP),BM(4),BS(4)
   13 format(77('-')/A11,8x,
     &'Correction factors for sampling errors:'/77('-')/
     &'Mean          =',F8.4,'95-percentile =',F8.4/77('-'))
	else
	if ( nobigout .le. 0 ) write(01,5513)Dname(JP),BM(4),BS(4)
 5513 format(33x,77('-')/33x,A11,8x,
     &'Corrections for sampling errors:'/33x,77('-')/33x,
     &'Mean          =',F8.4,33x,'95-percentile =',F8.4/33x,77('-'))
	endif
	endif
      
      endif
      return
	end

      subroutine write the correlation coefficients
      include 'COM.FOR'
      
      if ( qtype (jp) .eq. 4 ) return
	if ( ical13 .eq. 1 ) return
      if ( nobigout .eq. 1 ) return
      
      check = abs(CO1) + abs(CO2) + abs(CO3) + abs(CO4) + abs(CO5) 
     &                 + abs(CO6)
	if ( check .lt. 0.00001) return
      
	if ( ifdiffuse .eq. 0 ) then
	write(01,7)Dname(jp)
	write(33,7)Dname(jp)
    7 format(77('-')/'Non-zero correlation coefficients for ',
     &a11/77('-'))
      if ( CO1 .lt. - 1.0e-09 .or. CO1 .gt. 1.0e-09 ) then
      write(01,1)CO1
      write(33,1)CO1
    1 format(26x,'        River flow on river quality (CO1) = ',F7.4)
      endif
      if ( CO2 .lt. - 1.0e-09 .or. CO2 .gt. 1.0e-09 ) then
      write(01,2)CO2
      write(33,2)CO2
    2 format(26x,'           Added flow on river flow (CO2) = ',F7.4)
      endif
      if ( CO3 .lt. - 1.0e-09 .or. CO3 .gt. 1.0e-09 ) then
      write(01,3)CO3 ! 44444444
      write(33,3)CO3 ! 44444444
    3 format(26x,'        River flow on added quality (CO3) = ',F7.4) ! 44444444
      endif
      if ( CO4 .lt. - 1.0e-09 .or. CO4 .gt. 1.0e-09 ) then
      write(01,4)CO4 ! 44444444
      write(33,4)CO4 ! 44444444
    4 format(26x,'        River quality on added flow (CO4) = ',F7.4) ! 44444444
      endif
      if ( CO5 .lt. - 1.0e-09 .or. CO5 .gt. 1.0e-09 ) then
      write(01,5)CO5
      write(33,5)CO5
    5 format(26x,'        Added flow on added quality (CO5) = ',F7.4)
      endif
      if ( CO6 .lt. - 1.0e-09 .or. CO6 .gt. 1.0e-09 ) then
      write(01,6)CO6 ! 44444444
      write(33,6)CO6 ! 44444444
    6 format(26x,'     River quality on added quality (CO6) = ',F7.4) ! 44444444
      endif
      write(01,8)
      write(33,8)
    8 format(77('-'))
      else
	write(01,17)Dname(jp)
	write(33,17)Dname(jp)
   17 format(33x,77('-')/33x,'Non-zero correlation coefficients for ',
     &a11/33x,77('-'))
      if ( CO1 .lt. - 1.0e-09 .or. CO1 .gt. 1.0e-09 ) then
      write(01,11)CO1
      write(33,11)CO1
   11 format(59x,'        River flow on river quality (CO1) = ',F7.4)
      endif
      if ( CO2 .lt. - 1.0e-09 .or. CO2 .gt. 1.0e-09 ) then
      write(01,12)CO2
      write(33,12)CO2
   12 format(59x,'           Added flow on river flow (CO2) = ',F7.4)
      endif
      if ( CO3 .lt. - 1.0e-09 .or. CO3 .gt. 1.0e-09 ) then
      write(01,13)CO3 ! 44444444
      write(33,13)CO3 ! 44444444
   13 format(59x,'        River flow on added quality (CO3) = ',F7.4)  ! 44444444
      endif
      if ( CO4 .lt. - 1.0e-09 .or. CO4 .gt. 1.0e-09 ) then
      write(01,14)CO4 ! 44444444
      write(33,14)CO4 ! 44444444
   14 format(59x,'        River quality on added flow (CO4) = ',F7.4)  ! 44444444
      endif
      if ( CO5 .lt. - 1.0e-09 .or. CO5 .gt. 1.0e-09 ) then
      write(01,15)CO5
      write(33,15)CO5
   15 format(59x,'        Added flow on added quality (CO5) = ',F7.4)
      endif
      if ( CO6 .lt. - 1.0e-09 .or. CO6 .gt. 1.0e-09 ) then
      write(01,16)CO6 ! 44444444
      write(33,16)CO6 ! 44444444
   16 format(59x,'     River quality on added quality (CO6) = ',F7.4)  ! 44444444
	endif
      write(01,18)
      write(33,18)
   18 format(33x,77('-'))
	endif
	return
      end

      
      subroutine set up data for added flow and quality
      include 'COM.FOR'

*     set up and write out the shots from the non-parametric distributions -----
      call set up the shots for non parametric stream flow
      call set up the shots for non parametric discharge flow
      call set up the shots for non parametric stream quality
      call set up the shots for non parametric discharge quality

*     set up and write out the shots for monthly data --------------------------
      call set up monthly data for stream flow
      call set up monthly data for discharge flow
      call set up monthly data for stream quality
      call set up monthly data for discharge quality

*     set up and write out the shots for monthly data --------------------------
      call set up monthly structure for stream flow 8
      call set up monthly structure for discharge flow 8
      call set up monthly structure for stream quality
      call set up monthly structure for discharge quality

      if ( munthly structure .eq. 1 ) then ! impose monthly structures on added flows
      call set up monthly structure for stream flow 2
      call set up monthly structure for discharge flow 2
      endif
      
      return
      end
      
      
      subroutine inititialise the stores for the estimates of load 
      include 'COM.FOR'
      do L13 = 1, N13
	NSM (JP,L13) = 0
      ELOAD (JP,L13) = 0.0
      ELOAD03 (JP,L13) = 0.0
      ELOAD05 (JP,L13) = 0.0
      ELOAD12 (JP,L13) = 0.0
      ELOAD39 (JP,L13) = 0.0
	diffuse load (JP,L13) = 0.0
      enddo
      ELOADS (JP) = 0.0
      return
      end

      
      subroutine accumulate the loads         
      include 'COM.FOR'
      
*     accumulate total loads from all effluents ================================
      if ( JT(feeture) .eq.  3 .or. JT(feeture) .eq. 5 .or. 
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TELODE1(JP,J1) = TELODE1(JP,J1) + eload(JP,J1)
*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      TELODE2(JP,J1) = TELODE2(JP,J1) + eload(JP,J1)
      T03LOAD1(JP,J1) = T03LOAD1(JP,J1) + eload03(JP,J1) ! sewage works
      T05LOAD1(JP,J1) = T05LOAD1(JP,J1) + eload05(JP,J1) ! industrial effluents
      T12LOAD1(JP,J1) = T12LOAD1(JP,J1) + eload12(JP,J1) ! intermittent sewage
      T39LOAD1(JP,J1) = T39LOAD1(JP,J1) + eload39(JP,J1) ! mine waters
      T03LOAD2(JP,J1) = T03LOAD2(JP,J1) + eload03(JP,J1)
      T05LOAD2(JP,J1) = T05LOAD2(JP,J1) + eload05(JP,J1)
      T12LOAD2(JP,J1) = T12LOAD2(JP,J1) + eload12(JP,J1)
      T39LOAD2(JP,J1) = T39LOAD2(JP,J1) + eload39(JP,J1)
      call add up all the loads
	enddo ! do J1 = 1, nx
      if ( mark works .gt. 0 ) then ! --------- save the loads for back-tracking
      TELOADAV(kount works,JP) = eload(JP,i13) ! 3333333333333333333333333333333
      endif ! -------------------------------- saved the loads for back-tracking
      endif ! ======================= accumulated total loads from all effluents

*     accumulate total loads from tributaries and headwaters ===================
      if ( JT(feeture) .eq. 2 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TRLODE2(JP,J1) = TRLODE2(JP,J1) + eload(JP,J1)
      TRLODE1(JP,J1) = TRLODE1(JP,J1) + eload(JP,J1)
      enddo
      call add up all the loads
      endif ! ========== accumulated total loads from tributaries and headwaters

      return
      end