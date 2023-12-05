*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     --------------------------------------------------------------------------
*     File: mass balance.for ... 2715 lines ------------------------------------
*     --------------------------------------------------------------------------
*     This file mixes Features (such as effuents) into the Reaches -------------
*     --------------------------------------------------------------------------
*     This file contains 17 sub-routines ---------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... mass balance
*     ...... get the summary statistics of discharge flow
*     ...... get the summary statistics of discharge quality (XM,XS)
*     ...... bias in river or discharge flow and quality (XM,XS)
*     ...... get effluent quality 95 percentile
*     ...... get downstream river quality 
*     ...... ITRATE (Icheck, TARGET)
*     ...... list the correlation coefficients
*     ...... set up monthly data for negative discharge
*     ...... get a flow for the stream or discharge (IS,R3,EF)
*     ...... get the quality of the discharge (IS,R4,EC,XMEAN)
*     ...... write out the added flow shots
*     ...... write the correction factors for Monte Carlo errors
*     ...... write the correlation coefficients
*     ...... set up data for added flow and quality
*     ...... inititialise the stores for the estimates of load 
*     ...... accumulate the loads         
*     --------------------------------------------------------------------------
      
*     --------------------------------------------------------------------------
*     If IFDIFFUSE equals 1, the discharge is from diffuse river sources ...
*     If IFDIFFUSE equals 2, the discharge is from diffuse effluent sources ...
*     --------------------------------------------------------------------------

      subroutine mass balance
      include 'COMMON DATA.FOR'
      dimension efflows(NS),EQ(NS),efflows2(NS),RLC(n2prop),Y(NS)

*     perform mass balance at a single site ------------------------------------
*     mix a discharge into the river ... or mix a stream into the river --------
*     or diffuse inflows into the river ----------------------------------------
    
      IMDIST5 = 0 
      IMDIST8 = 0 

      do is = 1, NS ! ------------------ initialise the stores of effluent flows 
      efflows (is) = 0.0
      efflows2 (is) = 0.0
      ecshots (is) = 0.0
      do id = 1, 4
      Creg(id,IS) = 0.0
      enddo
      enddo ! -------------------------- initialise the stores of effluent flows

      FACT = 0.0 ! initialise control to direct the addition of flows ----------
      if ( JP .eq. ndetlast ) then ! prepare for the last determinand ----------
      FACT = 1.0 ! set up the addition of flow ---------------------------------
      endif ! if ( JP .eq. ndetlast ) ------------------------------------------

      if ( ifdiffuse .gt. 0 ) then ! this is a diffuse input iiiiiiiiiiiiiiiiiii
      if ( add conc .eq. 1 ) FACT = 0.0 ! no flow will be added iiiiiiiiiiiiiiii
      endif ! if ( ifdiffuse .gt. 0) iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

      call inititialise the stores for the estimates of load 

*     obtain the mean and standard deviation of the logged variables for the ---
*     discharge (or tribuary) flow ... But not (yet) for non-parametric --------
*     distributions or monthly distributions -----------------------------------
      call get the summary statistics of discharge flow
      call set up the shots for non parametric stream flow
      call set up the shots for non parametric discharge flow

*     obtain the mean and standard deviation of logged variables for the -------
*     quality of the discharge (or the tributary) ... But not (yet) for the ---- 
*     non-parametric distributions or monthly structures -----------------------
      call get the summary statistics of discharge quality (ECM,ECS) ! --- MASSB
     
*     use these to calculate correction factors for Monte Carlo errors ---------
      call bias in river or discharge flow and quality (ECM,ECS)
      call write the correction factors for Monte Carlo errors
*     finished for the ordinary distributions ----------------------------------

*     now deal with the non-parametric and monthly distributions ---------------
*     set up the correlation coeffiecients -------------------------------------
      if ( qtype (jp) .ne. 4 ) then ! ------------------------------------------
      if ( IQDIST .ge. 4 ) then ! non-parametric distributions------------------
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11) then! -------
      call set up the correlation coefficients !  non-parametric in mass balance
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11) -----
      endif ! if ( IQDIST .ge. 4 ) ---------------------------------------------
      endif ! if ( qtype (jp) .ne. 4 ) -----------------------------------------
      
      if ( ical .lt. 7 ) call write the correlation coefficients ! *************

      call set up data for added flow and quality
      
      do is = 1, ns ! get the flows for the discharge or tributary -------------
                   
      call get the correlated random numbers (IS,R1,R2,R3,R4) ! mass balance

      if ( JT(feeture) .eq. 2 ) then ! treat the discharge as a stream =========
      call get a flow for the stream or discharge (IS,R1,EF)
      else ! treat the discharge as a discharge ================================
      call get a flow for the stream or discharge (IS,R3,EF)
      endif ! if ( JT(feeture) .eq. 2 ) ========================================
      efflows (is) = EF
      efflows2 (is) = EF
      enddo ! do is = 1, ns ------- got the flows for the discharge or tributary
      
*     prepare for monthly structure of input of quality and flow ---------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) jqdist = struct0 (1) ! monthly structure for flows --

      non zero flow = 0
      non zero load = 0
      non zero conc = 0
      non zero bofc = 0
      non zero both = 0
      
      EFMk = 0.0 ! initialise mean flow added as effluent ----------------------
      EFSk = 0.0 ! and initialise the standard deviation -----------------------


*     **************************************************************************
*     start the mass balance ... loop over all the shots -----------------------
      do 2000 IS = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the correlated random numbers ----------------------------------------
 
      R1 = FRAN ( IS )
      JS = jset ( IQ, NS, IS )
      R2 = CRAN ( JP, JS ) 
      R3 = ERAN ( IS )
      R4 = PRAN ( JP, JS )
      
      call get the correlated random numbers (IS,R1,R2,R3,R4) ! mass balance 
     
      RF = FMS(IS) ! retrieve the flow of the upstream river -------------------
  
      EFP = EFMS(IS) ! retrieve the proportion of effluent flow in each shot ---
      RC = CMS(JP,IS) ! and the upstream river quality -------------------------

      do ip = 1, n2prop ! and the contributions from sources of pollution ------
      RLC(ip) = CTMS(ip, JP, IS)
      enddo
      
      EF = efflows(is) ! get the shots for the discharge (or tributary) flow --
      EC = 0.0
      
      if ( ECM .gt. 0.000001 ) then
      if ( JT(feeture) .eq. 2 ) then ! treat the discharge as a stream =========
      call get the quality of the discharge (IS,R4,EC,ECM) ! as a stream =======
      else ! treat it as a discharge ===========================================
      call get the quality of the discharge (IS,R4,EC,ECM) ! as a discharge ====
      endif ! if ( JT(feeture) .eq. 2 ) ========================================
      endif

      if ( EF .gt. 1.0e-09 ) non zero flow = non zero flow + 1
      if ( EC .gt. 1.0e-09 .and. EF .gt. 1.0e-09 ) then
      non zero both = nonzero both + 1
      endif
      
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 ) then ! load 999999999999999999999
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
      EFMB = EF ! set the added flow -------------------------------------------
*     for the diffuse inputs the flow data are per kilometre ------------------- 
      if ( ifdiffuse .gt. 0 ) then ! for diffuse inflows -----------------------
      EFMB = EF * dint
      EF = EF * dint
      endif ! if ( ifdiffuse .gt. 0 ) then ! for diffuse inflows ---------------
      
*     deal with data that are expressed as a load ==============================
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     imdist8 .eq. 7 ) then !                    load LLLLLLLLLLLLLLLLLLLLL
*     for loads set the flows to 1.0 -------------------------------------------
      EFMB = 1.0 ! -------------------------------------
      
*     and for diffuse additions of load scale the "concentrations" LLLLLLLLLLLLL
      if ( ifdiffuse .gt. 0 ) then ! -------------------------------------------
      EC = EC * dint ! mulitiply the load by distance LLLLLLLLLLLLLLLLLLLLLLLLLL
      endif ! if ( ifdiffuse .gt. 0 ) LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      endif ! discharge quality expressed as load ==============================

*     store the quality of the discharge or stream -----------------------------
      if ( QTYPE (JP) .ne. 4 ) then
      ECshots(IS) = EC
      else
      ECshots(IS) = 0.0
      endif ! store the quality of the discharge or stream ---------------------
      
      EFMk = EFMk + EF
      EFSk = EFSk + EF * EF
      
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     now do the Mass Balance +++++++++++++++++++++++++++++++++++++ mass balance
      if ( RF + EF .gt. 1.0e-8 ) then ! +++++++++++++++++++++++++++ mass balance
      if ( add conc .eq. 0 ) then ! flow will be added ++++++++++++ mass balance   
      CMS (JP,IS) = ( (RF * RC) + (EFMB * EC) ) / (RF + EF) ! +++++ mass balance
      else ! if ( add conc .eq. 0 ) +++++++++++++++++++++++++++++++ mass balance
      CMS (JP,IS) = ( (RF * RC) + (EFMB * EC) ) / (RF) ! ++++++++++ mass balance
      endif ! if ( add conc .eq. 0 ) ++++++++++++++++++++++++++++++ mass balance
      endif ! if ( RF + EF .gt. 1.0e-8 ) ++++++++++++++++++++++++++ mass balance
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
*     store data for tests based on regression ---------------------- regression      
      Creg(1,IS) = RF ! store the upstream river flow --------------- regression
      Creg(2,IS) = RC ! upstream quality ---------------------------- regression
      Creg(3,IS) = EF ! discharge flow ------------------------------ regression
      Creg(4,IS) = EC ! discharge quality --------------------------- regression
*     --------------------------------------------------------------- regression      

      
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
*     ------------------------------ add to the total of all point source inputs
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
*     concentrations from various types of feature ========= point source inputs
      if ( RF + EF .gt. 1.0e-8 ) then ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
      if ( ifdiffuse .eq. 0 ) then ! --------------- we have point source inputs
*     apply to all discharges (3 5 12 39 60 61) ------------ point source inputs
      if ( JT(feeture) .eq. 03 .or. JT(feeture) .eq. 05 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61 .or. ! add to total ----
     &     JT(feeture) .eq. 62 ) then
      CTMS(01,JP,IS) = ((RF * RLC(01)) + ! ------ add to total for all effluents
     &                 (EFMB * EC)) / (RF + EF) ! add to total for all effluents
      CTMS(NTD,JP,IS) = ((RF * RLC(NTD)) + ! ------ add to total for all sources
     &                 (EFMB * EC)) / (RF + EF) ! - add to total for all sources
      else ! for other inputs --------------------------------------------------
      CTMS(01,JP,IS) = ( (RF * RLC(1)) ) / (RF + EF) ! ----- dilute other inputs
      endif ! apply to discharges (3 5 12 39 60 61 ) ------- point source inputs
*     add to the totals ------------------------------------ point source inputs
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
     
      
      
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
*     ------------------------- add to the total for types of point source input
*     ==========================================================================
      if ( JT(feeture) .eq. 03 ) idnum = 02 ! ---------------- sewage works (03)
      if ( JT(feeture) .eq. 12 ) idnum = 03 ! ----- intermittent discharges (12)
      if ( JT(feeture) .eq. 05 ) idnum = 04 !  ------ industrial discharges (05)
      if ( JT(feeture) .eq. 39 ) idnum = 05 ! -------- apply to mine waters (39)
      if ( JT(feeture) .eq. 02 ) idnum = 17 ! ------------ apply to streams (02)
      if ( JT(feeture) .eq. 60 ) idnum = 28 ! --------- other Point Sources (60)
      if ( JT(feeture) .eq. 61 ) idnum = 29 ! --------- private wastewaters (61)
*     ---------------------- fish farms dealt with as abstractions -------------
      if ( JT(feeture) .eq. 62 ) idnum = 30 ! ------------------ fish farms (62)
*     ==========================================================================
      if ( JT(feeture) .eq. 03 .or. JT(feeture) .eq. 05 .or. !  point discharges
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or. !  point discharges
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61 .or. !  point discharges
     &     JT(feeture) .eq. 62 ) then ! ----------------------- point discharges
      CTMS(idnum,JP,IS) = ! increase contribution to type of point discharge ---
     &((RF * RLC(idnum)) + (EFMB * EC)) / (RF + EF) ! --------------------------
      do ip = 2, n2prop ! -------------------------------- for all source inputs
      if ( ip .ne. idnum .and. ip .ne. NTD ) then ! -- except the one just added
      oldCTMS = CTMS(IP,JP,IS) ! --------------------------- store the old value
      if ( oldCTMS .gt. 0.0000001 ) then ! -------------------------------------
      CTMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF) ! ----- dilute the other types
      endif ! if ( oldCTMS .gt. 0.0000001 ) ------------------------------------
      endif ! if ( ip .ne. idnum ) ------------------- except the one just added
      enddo ! do ip = 1, n2prop -------------------- for all point source inputs
*     ==========================================================================
      endif ! if ( JT(feeture) .eq. 03 etc =====================================
*     ==========================================================================
*     ------------------------- add to the total for types of point source input
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      
      
      
      
      
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
*     --------------------------------------------- add to the total for streams
*     ==========================================================================
      if ( JT(feeture) .eq. 02 ) then ! --------------------------------- stream 
      CTMS(idnum,JP,IS) = ! ------------ increase the contributions from streams
     &((RF * RLC(idnum)) + (EFMB * EC)) / (RF + EF) ! ------------------- stream
      CTMS(NTD,JP,IS) = ! ------ increase the contribution to total from streams
     &((RF * RLC(NTD)) + (EFMB * EC)) / (RF + EF) ! ------ to total from streams
      do ip = 1, n2prop ! --------------------------------------- for all inputs
      if ( ip .ne. idnum .and. ip .ne. NTD ) then ! -- except the one just added  
      oldCTMS = CTMS(IP,JP,IS) ! --------------------------- store the old value
      if ( CTMS(IP,JP,IS) .gt. 0.0000001 ) then ! ------------------------------
      CTMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF) ! ----- dilute the other types
      endif ! if ( oldCTMS .gt. 0.0000001 ) ------------------------------------
      endif ! if ( ip .ne. idnum ) ------------------- except the one just added
      enddo ! do ip = 1, n2prop -------------------- for all point source inputs
      endif ! if ( JT(feeture) .eq. 02 ) then ! ------------------------- stream 
*     ==========================================================================
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      endif ! if ( ifdiffuse .eq. 0 ) ====================== point source inputs 
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
      

*     types of pollution and types of feature ==================================
*     02: sewage works (03) ----------------------------------------------------
*     03: intermittent discharges (12) -----------------------------------------
*     04: industrial discharges (05) -------------------------------------------
*     05: mine waters (39) -----------------------------------------------------
*     28: user-named discharges (60) -------------------------------------------
*     29: private wastewaters (61) ---------------------------------------------
*     --------------------------------------------------------------------------
*     30: fish farms (62) ------------- dealt with as abstractions -------------
*     --------------------------------------------------------------------------
*     06: livestock farming (25) -----------------------------------------------
*     07: arable farming (27) --------------------------------------------------
*     08: highway runoff (29) --------------------------------------------------
*     09: urban runoff  (31) ---------------------------------------------------
*     10: atmospheric deposition (33) ------------------------------------------
*     11: natural background (35) ----------------------------------------------
*     12: septic tanks (37) ----------------------------------------------------
*     13: aggregated CSOs (40) -------------------------------------------------
*     14: aggregated STWs (42) -------------------------------------------------
*     15: diffuse mines (46) ---------------------------------------------------
*     16: birds, boats and angling (48) ----------------------------------------
*     23: user-named diffuse inflow (50) ---------------------------------------
*     24: user-named diffuse inflow (52) ---------------------------------------
*     25: user-named diffuse inflow (54) ---------------------------------------
*     26: user-named diffuse inflow (56) ---------------------------------------
*     27: user-named diffuse inflow (58) ---------------------------------------
*     17: headwaters and streams (10) (01) -------------------------------------
*     18: unassigned diffuse inflow (13) ---------------------------------------
*     19: diffuse effluent inflow (15) -----------------------------------------
*     20: reach diffuse (00) ---------------------------------------------------
*     21: gap filling for flow (00) --------------------------------------------
*     22: gap filling for quality (00) -----------------------------------------
*     30: not used -------------------------------------------------------------
*     31: sum of all diifuse pollution -----------------------------------------
*     types of feature =========================================================


*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( ifdiffuse .gt. 0 ) then ! --------ddddddddddd--------- diffuse inputs 
      if ( diffuse type .gt. 1 ) then ! -------dddddddddddd------ diffuse inputs
*     ==========================================================================
      if ( diffuse type .eq. 25 ) idnum = 6  ! ----- from livestock farming (25)
      if ( diffuse type .eq. 27 ) idnum = 7  ! -------- from arable farming (27)
      if ( diffuse type .eq. 29 ) idnum = 8  ! -------- from highway runoff (29)
      if ( diffuse type .eq. 31 ) idnum = 9  !----------- from urban runoff (31)
      if ( diffuse type .eq. 33 ) idnum = 10 !  from atmospheric deposition (33)
      if ( diffuse type .eq. 35 ) idnum = 11 ! ---- from natural background (35)
      if ( diffuse type .eq. 37 ) idnum = 12 ! ---------- from septic tanks (37)
      if ( diffuse type .eq. 40 ) idnum = 13 ! ------- from aggregated CSOs (40)
      if ( diffuse type .eq. 42 ) idnum = 14 ! ------- from aggregated STWs (42)
      if ( diffuse type .eq. 46 ) idnum = 15 ! --------- from diffuse mines (46)
      if ( diffuse type .eq. 48 ) idnum = 16 ! --- birds, boats and angling (48)
      if ( diffuse type .eq. 13 ) idnum = 18 ! --- type (13) diffuse inflow (13)
      if ( diffuse type .eq. 15 ) idnum = 19 ! --- type (15) diffuse inflow (15)
      if ( diffuse type .eq. 50 ) idnum = 23 ! ------- type (50) user-named (50)
      if ( diffuse type .eq. 52 ) idnum = 24 ! ------- type (52) user-named (52)
      if ( diffuse type .eq. 54 ) idnum = 25 ! ------- type (54) user-named (54)
      if ( diffuse type .eq. 56 ) idnum = 26 ! ------- type (56) user-named (56)
      if ( diffuse type .eq. 58 ) idnum = 27 ! ------- type (58) user-named (58)
*     ==========================================================================


      if ( add conc .eq. 0 ) then ! ++++++++++++ diffuse inputs with added flows
      if ( EF .lt. 0.0000001 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      CTMS(idnum,JP,IS) = + ! add in the form of extra diffuse load ++++++++++++
     &((RF * RLC(idnum)) + (EFMB * EC)) / (RF) ! +++++++++++++++++++++++++++++++
      CTMS(NTF,JP,IS) = + ! ++++++++++++ add to the total from all diffuse types 
     &((RF * RLC(NTF)) + (EFMB * EC)) / (RF) ! ++++ total from all diffuse types
      CTMS(NTD,JP,IS) = + ! +++++++++++++++++++++++++ add to the total pollution 
     &((RF * RLC(NTD)) + (EFMB * EC)) / (RF) ! ++++++ add to the total pollution
      else ! if ( EF .lt. 0.0000001 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CTMS(idnum,JP,IS) =  ! add the extra contribution to concentration =======
     &((RF * RLC(idnum)) + (EFMB * EC)) / (RF + EF) ! ==========================
      CTMS(NTF,JP,IS) =  !  ============ add to the total from all diffuse types
     &((RF * RLC(NTF)) + (EFMB * EC)) / (RF + EF) ! ============================
      CTMS(NTD,JP,IS) =  ! ========================== add to the total pollution
     &((RF * RLC(NTD)) + (EFMB * EC)) / (RF + EF) ! = add to the total pollution
      endif !  if ( EF .lt. 0.0000001 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else ! no extra flow is added ++++++++++++++++++++++++++++++++++++++++++++
      CTMS(idnum,JP,IS) = + ! add in the form of extra diffuse load ++++++++++++
     &((RF * RLC(idnum)) + (EFMB * EC)) / (RF) ! +++++++++++++++++++++++++++++++
      CTMS(NTF,JP,IS) = +  ! +++++++++++ add to the total from all diffuse types 
     &((RF * RLC(NTF)) + (EFMB * EC)) / (RF) ! +++++++++++++++++++++++++++++++++
      CTMS(NTD,JP,IS) = + ! ! +++++++++++++++++++++++ add to the total pollution 
     &((RF * RLC(NTD)) + (EFMB * EC)) / (RF) ! +++++++++++++++++++++++++++++++++
      endif ! if ( add conc .eq. 0 ) ++++++++++++++++++++++++++++ diffuse inputs


*     dilute the concentrations from the other sources of pollution ------------      
      do ip = 1, nprop ! .......................... loop through the other types
      if ( ip .ne. idnum ) then !  -------------- for other sources of pollution 
      if ( add conc .eq. 0 ) then ! ------------ diffuse inputs with added flows   
      CTMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF) ! ------ dilute this pollution 
      endif ! if ( add conc .eq. 0 ) ........... diffuse inputs with added flows
      endif ! if ( ip .ne. idnum ) -------------- for other sources of pollution
      enddo ! do ip = 1, nprop -------------------- loop through the other types
      
      
*     =============================================--------------===============
*     STORED CONTRIBUTIONS OF LOAD ================ Feature Type ===============
*     =============================================--------------===============
*     ( 1) = 'Mean from all discharges                          '
*     ( 2) = 'Added by sewage effluents                      (3)'
*     ( 3) = 'Intermittent discharges of sewage             (12)'
*     ( 4) = 'Added by industrial discharges                 (5)'
*     ( 5) = 'Added by mine waters                          (39)'
*     (28) = 'Other point sources                           (60)'
*     (29) = 'Private wastewaters                           (61)'
*     (62) = 'Fish farms                                    (62)'
*     --------------------------------------------------------------------------
*     ( 6) = 'Diffuse pollution from livestock              (25)'
*     ( 7) = 'Diffuse pollution from arable                 (27)'
*     ( 8) = 'Highway runoff                                (29)'
*     ( 9) = 'Urban runoff                                  (31)'
*     (10) = 'Atmosphere deposition                         (33)'
*     (11) = 'Natural background                            (35)'
*     (12) = 'Septic tanks                                  (37)'
*     (13) = 'Aggregate CSOs                                (40)'
*     (14) = 'Aggregated STWs                               (42)'
*     (15) = 'Diffuse mines                                 (46)'
*     (16) = 'Birds, boats and angling                      (48)'
*     (18) = 'Diffuse input                                 (13)'
*     (19) = 'Diffuse input                                 (15)'
*     (23) = 'User-named diffuse input                      (50)'
*     (24) = 'User-named diffuse input                      (52)'
*     (25) = 'User-named diffuse input                      (54)'
*     (26) = 'User-named diffuse input                      (56)'
*     (27) = 'User-named diffuse input                      (58)'
*     --------------------------------------------------------------------------
*     (17) = 'Boundaries and tributaries              (2 and 10)'
*     (20) = 'Reach diffuse                                 none'
*     (21) = 'Gap filling of flows                          none'
*     (22) = 'Gap filling of quality                        none'
*     (30) = 'Not used                                          '                                                  '
*     (31) = 'Total of diffuse inputs                           '                                                  '
*     =========================================================================

      

*     ======================================= dilute the total diffuse load (31)
      if ( idnum.lt.06 .or. idnum.eq.17 .or. ! ================================
     &     idnum.eq.20 .or. idnum.eq.21 .or. idnum.eq.22 .or. ! =============== 
     &     idnum.gt.27 ) then ! ================= dilute the total diffuse load
      if ( add conc .eq. 0 ) then ! +++++++++++ diffuse inputs with added flows   
      CTMS(NTF,JP,IS) = (RF * RLC(NTF)) / (RF + EF) ! ============ dilute these
      endif ! +++++++++++++++++++++++++++++++++ diffuse inputs with added flows
      endif ! if ( idnum.lt.06 .or. etc =======================================
      endif ! if ( diffuse type .gt. 0 ) ! =====================================
      endif ! if ( ifdiffuse .eq. 1 ) --------------------------- diffuse inputs
*     ==========================================================================
*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      endif ! if ( RF + EF .gt. 1.0e-8 ) FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
*     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      
      
*     do the flow summation ... do this for the last determinand ---------------
*     FACT us set to 1.0 when JP = NDETLAST
      FMS(IS) = RF + FACT * EF ! update the river flow -------------------------
      dfms(IS) = RF + EF ! store the downstream flow for extra calculations ----

*     calculate the proportion of effluent in each shot ------------------------
*     apply to (3) (5) (12) (39) (60) (61) ------------- proportions of effluent
      if ( JT(feeture) .eq. 03 .or. JT(feeture) .eq. 05 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61) then ! sum flows
      EFMS(IS) = ( RF * EFP + EFMB ) / (RF + EF)
      else
      EFMS(IS) = ( RF * EFP ) / (RF + EF) ! dilute the effluents ---------------
      endif

*     and calculate the load ===================================================
      if ( QTYPE (JP) .ne. 4 ) then ! ==========================================
      K13 = imonth + 1
      if ( ifdiffuse .eq. 0 ) then ! -------------------------------------------
      XLD = ECshots(IS) * EFMB ! load for this shot ----------------------------
      ELOAD(JP,I13) = ELOAD(JP,I13) + XLD ! -- for annual mean ---- mass balance
      ELOAD(JP,K13) = ELOAD(JP,K13) + XLD ! -- for monthly mean --- mass balance
      ELOADS(JP) = ELOADS(JP) + XLD * XLD ! -- for st.dev---------- mass balance
*     ====================================================================== 158
      if ( JT(feeture) .eq. 03) ELOAD03(JP,I13) = ELOAD03(JP,I13) + XLD ! --- MB
      if ( JT(feeture) .eq. 05) ELOAD05(JP,I13) = ELOAD05(JP,I13) + XLD ! --- MB 
      if ( JT(feeture) .eq. 12) ELOAD12(JP,I13) = ELOAD12(JP,I13) + XLD ! --- MB
      if ( JT(feeture) .eq. 39) ELOAD39(JP,I13) = ELOAD39(JP,I13) + XLD ! --- MB
      if ( JT(feeture) .eq. 60) ELOAD60(JP,I13) = ELOAD60(JP,I13) + XLD ! --- MB
      if ( JT(feeture) .eq. 61) ELOAD61(JP,I13) = ELOAD61(JP,I13) + XLD ! --- MB
*     fish farms dealt with as an abstraction ----------------------------------
*     if ( JT(feeture) .eq. 62) ELOAD62(JP,I13) = ELOAD62(JP,I13) + XLD ! --- MB
      if ( JT(feeture) .eq. 03) ELOAD03(JP,K13) = ELOAD03(JP,K13) + XLD ! --- MB
      if ( JT(feeture) .eq. 05) ELOAD05(JP,K13) = ELOAD05(JP,K13) + XLD ! --- MB
      if ( JT(feeture) .eq. 12) ELOAD12(JP,K13) = ELOAD12(JP,K13) + XLD ! --- MB
      if ( JT(feeture) .eq. 39) ELOAD39(JP,K13) = ELOAD39(JP,K13) + XLD ! --- MB
      if ( JT(feeture) .eq. 60) ELOAD60(JP,K13) = ELOAD60(JP,K13) + XLD ! --- MB
      if ( JT(feeture) .eq. 61) ELOAD61(JP,K13) = ELOAD61(JP,K13) + XLD ! --- MB
*     fish farms dealt with as an abstraction ----------------------------------
*     if ( JT(feeture) .eq. 62) ELOAD62(JP,K13) = ELOAD62(JP,K13) + XLD ! --- MB
*     ====================================================================== 158
      endif ! if ( ifdiffuse .eq. 0 ) then -------------------------------------

      if ( ifdiffuse .gt. 0 ) then ! for diffuse additions -----ddddddddd-------
      if ( RF + EF  .gt. 1.0e-8 ) then ! --------------dddddddddd---------------

      if ( add conc .eq. 0 ) then ! ++++++++++++ddddddddddd+++++++++++++++ loads
      diffuse load (JP,I13) = diffuse load (JP,I13) + ECshots(IS) * EFMB
      diffuse load (JP,K13) = diffuse load (JP,K13) + ECshots(IS) * EFMB
      else ! then load was added as flow and concentration
      diffuse load (JP,I13) = diffuse load (JP,I13) + (EFMB * EC)
      diffuse load (JP,K13) = diffuse load (JP,K13) + (EFMB * EC)
      endif ! if ( add conc .eq. 0 ) ++++++++++++++dddddddddd+++++++++++++ loads
      endif ! if ( RF + EF  .gt. 1.0e-8 ) then --------dddddddddd---------------
      endif ! if ( ifdiffuse  .gt. 0 ) for diffuse additions----ddddddddddd-----

      NSM (JP,I13) = NSM (JP,I13) + 1 ! number of shots per month
      NSM (JP,K13) = NSM (JP,K13) + 1
      endif ! if ( QTYPE (JP) .ne. 4 ) =========================================

 2000 continue ! end of the main loop on shots ! *******************************
*     **************************************************************************
      
      
*     check the summary statistics of the added flow ===========================
      EFSK=(EFSK-EFMK*EFMK/NS)/(NS-1)
      if ( EFSK .gt. 1.0e-10 ) then
      EFSK=SQRoot(1024,EFSK)
      else
      EFSK = 0.0
      endif
      EFMK = EFMK / FLOAT(NS)
      call sort format 2 (EFMK,EFSK)
*     write(01,1012)valchars10,funit,valchars11,funit ! -------------------- OUT
*     write(31,1012)valchars10,funit,valchars11,funit ! -------------------- EFF
*     write(150+JP,1012)valchars10,funit,valchars11,funit ! ------------- Di EFF
 1012 format(77('c')/'Flow discharged as effluent:',20X,
     &' Annual mean =',a10,1x,a4/41X,' Standard Deviation =',a10,
     &1x,a4/77('c'))
*     check summary statistics of the added flow ===============================

      
      do IS = 1,NS ! ------------------------------------------- check the shots 
      if (CMS (JP,IS) .gt. 1.0e9) then
      xxxxxx = CMS (JP,IS)
      ixxxxx = IS
      !CMS(JP,IS) = 5.0 * C(JP,1)
      CMS(JP,IS) = quolity data(IQ,JP,1)
      suppress9c = suppress9c + 1
      if ( suppress9c .lt. 5 ) then
      call sort format 1 (CMS (JP,IS))
      write(01,5282)ixxxxx,dname(JP),xxxxxx,IQ,valchars10,
     &uname(feeture),PDRC(IQ,JP)
      write(33,5282)ixxxxx,dname(JP),xxxxxx,IQ,valchars10,
     &uname(feeture),PDRC(IQ,JP),quolity data(IQ,JP,1)
 5282 format(110('*')/
     &'Shot',i5,' is greater than a billion for ',a11,'=',
     &f30.0,'  Set of data:',i5/
     &'It has been re-set to',a10,' at: ',a37/
     &'Type of data =',i3/
     &'Mean =',f8.2/
     &110('-'))
      call change colour of text (20) ! red
      !write( *,5292)dname(JP),uname(feeture),valchars10
 5292 format(
     &'*** Value > 10000000000 for ',a11,12x,'...',7x,
     &'at: ',a37,3x,'re-set to',a10,' .....')
      call set screen text colour
      else
      if ( suppress9c .gt. 9999 ) suppress9c = 0
      endif
      if ( suppress9c .eq. 5 ) then
      call sort format 1 (CMS (JP,IS))
      write(01,5582)ixxxxx,dname(JP),xxxxxx,IQ,valchars10,uname(feeture)
      write(33,5582)ixxxxx,dname(JP),xxxxxx,IQ,valchars10,uname(feeture)
 5582 format(110('*')/
     &'Shot',i5,' is greater than a billion for ',a11,'=',
     &f30.0,'  Set of data:',i5/
     &'*** Re-set to',a10,' at: ',a37,' and eleswhere'/110('*')/)
      call change colour of text (22) ! light blue
      !write( *,5592)dname(JP),uname(feeture)
 5592 format(
     &'*** Value > 10000000000 for ',a11,12x,'...',7x,
     &'at: ',a37,3x,'and other cases .........')
      call set screen text colour
      endif
      endif
      enddo ! -------------------------------------------------- check the shots


     
*     diffuse inputs 555555555555555555555555555555 apportionment of percentiles
      if ( ifdiffuse .gt. 0 ) then ! 55555555555555 apportionment of percentiles
      if ( diffuse type .gt. 0 ) then ! 55555555555 apportionment of percentiles
      if ( diffuse type .eq. 25 ) ix = 6  ! -------- from livestock farming (25) 
      if ( diffuse type .eq. 27 ) ix = 7  ! ----------- from arable farming (27)
      if ( diffuse type .eq. 29 ) ix = 8  ! ------------from highway runoff (29)
      if ( diffuse type .eq. 31 ) ix = 9  ! ------------- from urban runoff (31)
      if ( diffuse type .eq. 33 ) ix = 10 ! --- from atmospheric deposition (33)
      if ( diffuse type .eq. 35 ) ix = 11 ! ------- from natural background (35)
      if ( diffuse type .eq. 37 ) ix = 12 ! ------------- from septic tanks (37)
      if ( diffuse type .eq. 40 ) ix = 13 ! ---------- from aggregated CSOs (40) 
      if ( diffuse type .eq. 42 ) ix = 14 ! ---------- from aggregated STWs (42)
      if ( diffuse type .eq. 46 ) ix = 15 ! ------------ from diffuse mines (46)
      if ( diffuse type .eq. 48 ) ix = 16 ! - from birds, boats and angling (48)
      if ( diffuse type .eq. 13 ) ix = 18 ! ---------- from diffuse inflows (13)
      if ( diffuse type .eq. 15 ) ix = 19 ! ---------- from diffuse inflows (15)
      if ( diffuse type .eq. 50 ) ix = 23 ! --------------- from user-named (50)
      if ( diffuse type .eq. 52 ) ix = 24 ! --------------- from user-named (52)
      if ( diffuse type .eq. 54 ) ix = 25 ! --------------- from user-named (54)
      if ( diffuse type .eq. 56 ) ix = 26 ! --------------- from user-named (56)
      if ( diffuse type .eq. 58 ) ix = 27 ! --------------- from user-named (58)
      endif ! if ( diffuse type .gt. 0 ) ! 55555555 apportionment of percentiles
      endif ! if ( ifdiffuse .gt. 0 ) ! 55555555555 apportionment of percentiles

      
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

*     ====================================================================== 158
      if ( JT(feeture) .eq. 03 ) ELOAD03 (JP,I13) = ELOAD03 (JP,I13) 
     &                                            / float(NS)
      if ( JT(feeture) .eq. 05 ) ELOAD05 (JP,I13) = ELOAD05 (JP,I13) 
     &                                            / float(NS)
      if ( JT(feeture) .eq. 12 ) ELOAD12 (JP,I13) = ELOAD12 (JP,I13) 
     &                                            / float(NS)
      if ( JT(feeture) .eq. 39 ) ELOAD39 (JP,I13) = ELOAD39 (JP,I13) 
     &                                            / float(NS)
      if ( JT(feeture) .eq. 60 ) ELOAD60 (JP,I13) = ELOAD60 (JP,I13) 
     &                                            / float(NS)
      if ( JT(feeture) .eq. 61 ) ELOAD61 (JP,I13) = ELOAD61 (JP,I13) 
     &                                            / float(NS)
*     fish farms dealt with as an abstraction ----------------------------------
*     if ( JT(feeture) .eq. 62 ) ELOAD62 (JP,I13) = ELOAD62 (JP,I13) 
*    &                                            / float(NS)
*     ====================================================================== 158

      endif ! if ( ifdiffuse .eq. 0 ) ----- calculated the annual effluent loads
            
      
      if ( ifdiffuse .gt. 0 ) then ! --------------- calculate the diffuse loads
      diffuse load (JP,I13) = diffuse load (JP,I13) / float (NS)
      endif ! if ( ifdiffuse .gt. 0 ) ! ----------- calculated the diffuse loads
      
      if ( munthly structure .eq. 1 ) then ! - calculate  monthly effluent loads
      do J13 = 2, N13 ! -------------------------------- loop through the months
      if ( NSM(JP,J13) .gt. 1 ) then
      if ( ifdiffuse .eq. 0 ) then
      ELOAD (JP,J13) = ELOAD (JP,J13) / float (NSM (JP,J13))
      if ( JT(feeture) .eq. 03 ) ELOAD03 (JP,J13) = ELOAD03 (JP,J13) 
     &                                            / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 05 ) ELOAD05 (JP,J13) = ELOAD05 (JP,J13) 
     &                                            / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 12 ) ELOAD12 (JP,J13) = ELOAD12 (JP,J13) 
     &                                            / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 39 ) ELOAD39 (JP,J13) = ELOAD39 (JP,J13) 
     &                                            / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 60 ) ELOAD60 (JP,J13) = ELOAD60 (JP,J13) 
     &                                            / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 61 ) ELOAD61 (JP,J13) = ELOAD61 (JP,J13) 
     &                                            / float(NSM (JP,J13))
*     fish farms dealt with as an abstraction ----------------------------------
*     if ( JT(feeture) .eq. 62 ) ELOAD62 (JP,J13) = ELOAD62 (JP,J13) 
*    &                                            / float(NSM (JP,J13))
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

      
*     calculate the zero discharges of flow or load ============================
      if ( nobigout .le. 0 ) then ! ============================================
      if ( JSKIP .eq. 0 ) then ! ===============================================
      if ( ical .ne. 1 ) then ! ================================================
          
      if ( non zero both .lt. NS .and. non zero both .gt. 0 ) then
      if ( non zero flow .lt. NS .and. jp .eq. ndetfirst ) then ! --------------
      pcfflow = 100.0 * (float (non zero flow) / float (NS) )
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      !write(01,6522)
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
      endif ! if ( non zero flow .lt. NS .and. jp .eq. ndetfirst ) -------------

      if ( non zero load .lt. NS ) then ! --------------------------------------
      pcfload = 100.0 * (float (non zero load) / float (NS) )
      if ( ifdiffuse .eq. 0 ) then
      if ( jhead .eq. 0 ) then
      !write(01,6522)
      jhead = 1
      endif
      !if ( ical13 .eq. 0 ) write(01,5812) dname(JP),pcfload
 5812 format(a11,' ... % of year with added load:',f7.2)
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      !if ( ical13 .eq. 0 ) write(01,5814) dname(JP),pcfload
 5814 format(33x,a11,' ... % of year with added load:',f7.2)
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( non zero load .lt. NS ) -------------------------------------


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
      !if ( ical13 .eq. 0 ) write(01,7829) dname(JP),pcfbofc
 7829 format(a11,' ... % of year with added load:',f7.2)
      else ! if ( ifdiffuse .eq. 0 )
      if ( jhead .eq. 0 ) then
      write(01,6532)
      jhead = 1
      endif ! if ( jhead .eq. 0 )
      !if ( ical13 .eq. 0 ) write(01,7894) dname(JP),pcfbofc
 7894 format(33x,a11,' ... % of year with added load:',f7.2)
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( non zero bofc .lt. NS )

      endif ! if ( non zero both .lt. NS )
      
      endif ! if ( ical .ne. 1 ) ===============================================
      endif ! if ( JSKIP .eq. 0 ) ==============================================
      endif ! if ( nobigout .le. 0 ) ===========================================
      
      return
      end

      
      
      
*     compute the moments of the discharge flow distribution -------------------
      subroutine get the summary statistics of discharge flow
      include 'COMMON DATA.FOR'

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
      include 'COMMON DATA.FOR'
            
      
      GECM = XM
      GECS = XS
      if ( IQDIST .eq. 0 ) return
      
      if ( IQDIST .eq. 1 .or. IQDIST .eq. 6 .or. IQDIST .eq. 9 ) return
      
      GECM = 0.0
      GECS = 0.0
      
      EM3 = XM + EC3 ! ------------------------ add  the shift to the mean - EC3
      if ( IQDIST .eq. 7 .or. IQDIST .eq. 11 ) then ! ---- but for data as loads
      EM3 = XM ! do not add in EC3 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      endif ! ------------------------------------------------ for data as loads


*     PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP      
      if ( IQDIST .eq. 10 ) then ! pcpcpc for the power curves for concentration
      EM3 = XM ! do not add in EC3 ccccccccccccccccccccccccccccccccccccccccccccc
      endif ! --------------------------------------------- for data power curve
*     PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP      

      EM4 = EM3 * EM3 + XS * XS
      if (EM4 .lt. 1.0E-20) return
      GECM = ALOG((EM3*EM3)/SQRoot(195854,EM4)) ! mean in the log domain ------
      xxx = ALOG(1.0+(XS*XS)/(EM3*EM3))
      if ( xxx .lt. 1.0e-20 ) return
      GECS = SQRoot3(195901,xxx) ! standard deviation in the log domain --------


*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 11 ) then ! for power distribution of loads plplplplplplp
      avloadp = 0.0 ! initialise the mean load ---------------------------------
      rerkmini11 = 0.0 ! computing summary startistics for power curve ppppppppp
      perkmini11 = 0.01*powermini(IQ,JP) ! cut off percentile pppppppppppppppppp
      rerkmini11 = errx (perkmini11+0.000001) ! deviate pppppppppppppppppppppppp
      baselode = powerbase (IQ,JP) ! the base load of the power curve pppppppppp
      pindex = quolity data (IQ,JP,3) ! power-curve index pppppppppppppppppppppp

*     calculate mean produced by the power curve ===============================
      perk = 0.0
      kperc11 = 0
      do ii = 1, 5000 ! loop through 5000 shots --------------------------------
      perk = perk + 0.0002 ! fraction of 1 for this shot
      rerk = errx (perk) ! get the standard normal deviate for perk
      EC = EXP ( GECM + GECS * rerk ) ! calculate for log-normal distribution pp
      EC = EC ** pindex ! apply the power index pppppppppppppppppppppppppppppppp
      if ( rerk .lt. rerkmini11 ) then ! 
      EC = 0.0
      kperc11 = kperc11 + 1
      endif
      avloadp = avloadp + EC
      enddo !do ii = 1, 5000 ---------------------------------------------------
      avloadp = avloadp / 5000.0 ! mean produced by the power curve ------------
      xkperc = 100.0*float(kperc11)/5000.0
*     ==========================================================================      
*     calculate the resulting 5000 shots =======================================       
      perk = 0.0
      do ii = 1, 5000 ! ========================================================
      perk = perk + 0.0002 ! fraction of 1 for this shot
      rerk = errx (perk) ! get the standard normal deviate for perk
      EC = EXP ( GECM + GECS * rerk ) ! calculate for log-normal distribution pp
      ECxx = EC
      EC = EC ** pindex ! apply the power index pppppppppppppppppppppppppppppppp
      ECaa = EC     
      if ( rerk .lt. rerkmini11 ) then ! 
      EC = 0.0
      ECxx = 0.0
      ECaa = 0.0
      kperc11 = kperc11 + 1
      endif
      if ( avloadp .gt. 0.0001 ) then
      EC = EC * (XM * ((100.0-baselode)/100.0)/avloadp) ! pppppppppppppppppppppp
     &         + XM * baselode/100.0 ! power curve for loads ppppppppppppppppppp
      endif
      enddo ! do ii = 1, 5000 ==================================================

      if ( ecm .gt. 0.000001 ) then
      write(08,2)dname(JP)
      write(01,2)dname(JP)
      write(200+JP,2)dname(JP)
    2 format(77('=')/
     &'Data have been entered as a power curve for loads for ',a11)
      write(08,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(01,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(200+JP,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
    3 format(77('=')/
     &'           Mean load entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'                    The cut-off point =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.2/
     &24x,'The base-load =',f10.2,' (% of the input mean)'/
     &'Mean load produced by the power curve =',f38.2/
     &'          Correlation with river flow =',f10.4/77('='))
           
      if ( avloadp .gt. 99999999.0 ) then ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      suppress15 = suppress15 + 1
      xpr = 1.0
      call sort format 1a (XM)
      call change colour of text (20) ! bright red
      write( *,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr 
      call set screen text colour
      
      !write(09,3390)XM,IQ,XS,dname(JP),quolity data(IQ,JP,3),
      !&100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
 3390 format(77('=')/
     &'(2)        Mean load entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4,
     &' for ',a11/
     &'             Index of the power curve =',f10.4/
     &'                    The cut-off point =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.2,' per cent'/ ! xkperc
     &24x,'The base-load =',f10.2,' (% of the input mean)'/
     &'Mean load produced by the power curve =',f23.2/
     &44x,'This scales the calculated values'/
     &'          Correlation with river flow =',f10.4/77('='))
      
      write(01,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ OUT
      write(09,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ SCN
      write(33,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ ERR
   35 format(/93('~')/
     &'(2) Huge loads produced by power curve for ',a11, ! load ------ ERR & SCN
     &' at ',a35/
     &93('~')/'           Mean load entered as input =',a10,
     &12x,' Data-set:',i6/
     &'                    Power curve index =',f10.4/
     &'Mean load produced by the power curve =',i38/93('~')/
     &' Power curve index has been re-set as =',f10.4/93('#')/)
     
      quolity data(IQ,JP,3) = xpr ! remove the power index
      
      endif ! if ( avloadp .gt. 99999.9 ) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      endif ! if ( ecm .gt. 0.00001 )
      endif ! ( IQDIST .eq. 11 ) for power distribution of loads ppppppppppppppp
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp  

      
      
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 10 ) then ! for power distribution of concentration pcpcp
      avconcp = 0.0
      rerkmini10 = 0.0
      perkmini10 = 0.01*powermini (IQ,JP)
      rerkmini10 = errx (perkmini10+0.000001)
      baseconc = powerbase (IQ,JP)
      pindex = quolity data (IQ,JP,3)
      perk = 0.0
      kperc10 = 0
      do ii = 1, 5000 ! --------------------------------------------------------
      perk = perk + 0.0002
      rerk = errx (perk)
      EC = EXP ( GECM + GECS * rerk )
      EC = EC ** pindex ! use the power index pppppppppppppppppppppppppppppppppp
      
      if ( rerk .lt. rerkmini10 ) then ! 
      EC = 0.0
      kperc10 = kperc10 + 1
      endif
      
      avconcp = avconcp + EC
     
      enddo ! do ii = 1, 5000 --------------------------------------------------

      avconcp = avconcp / 5000.0 ! mean produced by the power curve
      xkperc = 100.0*float(kperc10)/5000.0
      
      write(08,4)dname(JP)
      write(33,4)dname(JP)
    4 format(/77('=')/
     &'Data were entered as a power curve for concentrations for ',a11)
      write(08,5)XM,IQ,XS,pindex,
     &100.0*perkmini10,xkperc,baseconc,avconcp,quolity data(IQ,JP,MO)
      write(33,5)XM,IQ,XS,pindex,
     &100.0*perkmini10,xkperc,baseconc,avconcp,quolity data(IQ,JP,MO)
    5 format(77('=')/
     &'  Mean concentration entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'         The cut-off point (perkmini) =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.1/
     &15x,'The base-concentration =',f10.2,' (% of the input mean)'/
     &'     Mean produced by the power curve =',f10.4,
     &' (scales calculated values)'/
     &'          Correlation with river flow =',f10.4/77('=')/)
      endif ! ( IQDIST .eq. 10 ) for power distribution of concentrations pcpcpc
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp 
      
      
      return
      end

      
      
      
*     compute moments of discharge quality -------------------------------------
      subroutine get the summary statistics of discharge quality 2
     &(jjp,XM,XS,XXP)
      include 'COMMON DATA.FOR'
            
      GECM = XM
      GECS = XS
      XXP = 0.0
      t95 = errx ( 0.95 )
      
*     0 - constant, uniform values
*     1 - a Normal Distribution
*     2 - the Log-Normal Distribution
*     3 - a Three-Parameter Log-Normal Distribution
*     4 - non-parametric distribution
*     5 - monthly data - distribution for each month
*     6 - as 1 but the data (d) and (e) refer to loads
*     7 - as 2 but the data (d) and (e) refer to loads
*     8 - monthly structure
*     9 - non-parametric distribution of loads
*     10 - a power-curve distribution for concentrations
*     11 - a power-curve distribution for loads?
      
      if ( IQDIST .eq. 0 ) then
      XXP = XM
      return
      endif
      
      if ( IQDIST .eq. 1 .or. IQDIST .eq. 6 .or. IQDIST .eq. 9 ) then
      if ( Detype (jjp) .eq. 104 ) then
      XXP = amax1 (0.0, XM - XS * t95)
      else
      XXP = XM + XS * t95
      endif
      return
      endif
      
      GECM = 0.0
      GECS = 0.0
      
      EM3 = XM + EC3 ! ------------------------ add  the shift to the mean - EC3
      if ( IQDIST .eq. 7 .or. IQDIST .eq. 11 ) then ! ---- but for data as loads
      EM3 = XM ! do not add in EC3 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      endif ! ------------------------------------------------ for data as loads
      if ( IQDIST .eq. 10 ) then ! cc and for the power curves for concentration
      EM3 = XM ! do not add in EC3 ccccccccccccccccccccccccccccccccccccccccccccc
      endif ! --------------------------------------------- for data power curve

      EM4 = EM3 * EM3 + XS * XS
      if (EM4 .lt. 1.0E-20) return
      GECM = ALOG((EM3*EM3)/SQRoot(195854,EM4)) ! mean in the log domain ------
      xxx = ALOG(1.0+(XS*XS)/(EM3*EM3))
      if ( xxx .lt. 1.0e-20 ) return
      GECS = SQRoot3(195901,xxx) ! standard deviation in the log domain --------
      
      !GRCS=SQRoot(1004,ALOG(1.0+(RCS*RCS)/RM3)) ! standard deviation -----------

      
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 11 ) then ! for power distribution of loads ppppppppppppp
      avloadp = 0.0 ! initialise the mean load ---------------------------------
      rerkmini11 = 0.0 ! computing summary startistics for power curve ppppppppp
      perkmini11 = 0.01*powermini(IQ,JP) ! cut off percentile pppppppppppppppppp
      rerkmini11 = errx (perkmini11+0.000001) ! deviate pppppppppppppppppppppppp
      baselode = powerbase (IQ,JP) ! the base load of the power curve pppppppppp
      pindex = quolity data (IQ,JP,3) ! power-curve index pppppppppppppppppppppp

      
*     calculate mean produced by the power curve ===============================
      perk = 0.0
      kperc11 = 0
      do ii = 1, 5000 ! loop through 5000 shots --------------------------------
      perk = perk + 0.0002 ! fraction of 1 for this shot
      rerk = errx (perk) ! get the standard normal deviate for perk
      EC = EXP ( GECM + GECS * rerk ) ! calculate for log-normal distribution pp
      EC = EC ** pindex ! apply the power index pppppppppppppppppppppppppppppppp
      if ( rerk .lt. rerkmini11 ) then ! 
      EC = 0.0
      kperc11 = kperc11 + 1
      endif
      avloadp = avloadp + EC
      enddo !do ii = 1, 5000 ---------------------------------------------------
      avloadp = avloadp / 5000.0 ! mean produced by the power curve ------------
      xkperc = 100.0*float(kperc11)/5000.0
*     ==========================================================================      
*     calculate resultinf 5000 shots ===========================================       
      perk = 0.0
      do ii = 1, 5000 ! ========================================================
      perk = perk + 0.0002 ! fraction of 1 for this shot
      rerk = errx (perk) ! get the standard normal deviate for perk
      EC = EXP ( GECM + GECS * rerk ) ! calculate for log-normal distribution pp
      ECxx = EC
      EC = EC ** pindex ! apply the power index pppppppppppppppppppppppppppppppp
      ECaa = EC     
      if ( rerk .lt. rerkmini11 ) then ! 
      EC = 0.0
      ECxx = 0.0
      ECaa = 0.0
      kperc11 = kperc11 + 1
      endif
      if ( avloadp .gt. 0.0001 ) then
      EC = EC * (XM * ((100.0-baselode)/100.0)/avloadp) ! pppppppppppppppppppppp
     &         + XM * baselode/100.0 ! power curve for loads ppppppppppppppppppp
      endif
      enddo ! do ii = 1, 5000 ==================================================


      if ( ecm .gt. 0.000001 ) then
      write(08,2)dname(JP)
      write(01,2)dname(JP)
      write(200+JP,2)dname(JP)
    2 format(77('=')/
     &'Data have been entered as a power curve for loads for ',a11)
      write(08,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(01,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(200+JP,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
    3 format(77('=')/
     &'           Mean load entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'                    The cut-off point =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.2/
     &24x,'The base-load =',f10.2,' (% of the input mean)'/
     &'Mean load produced by the power curve =',f23.2/
     &'          Correlation with river flow =',f10.4/77('='))
     
      if ( avloadp .gt. 1999999.0 ) then ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      suppress15 = suppress15 + 1
      xpr = 1.0
      call sort format 1a (XM)
      call change colour of text (20) ! bright red
      write( *,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr
      call set screen text colour
      
      !write(09,3390)XM,IQ,XS,dname(JP),quolity data(IQ,JP,3),
      !&100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
 3390 format(77('=')/
     &'(3)        Mean load entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4,
     &' for ',a11/
     &'             Index of the power curve =',f10.4/
     &'                    The cut-off point =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.2,' per cent'/ ! xkperc
     &24x,'The base-load =',f10.2,' (% of the input mean)'/
     &'Mean load produced by the power curve =',f23.2/
     &44x,'This scales the calculated values'/
     &'          Correlation with river flow =',f10.4/77('='))
      
      
      write(01,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ OUT
      write(09,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ SCN
      write(33,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr ! ------ ERR
   35 format(/93('~')/
     &'(3) Huge loads produced by power curve for ',a11, ! load ------ ERR & SCN
     &' at ',a35/
     &93('~')/'           Mean load entered as input =',a10,
     &12x,' Data-set:',i6/
     &'                    Power curve index =',f10.4/
     &'Mean load produced by the power curve =',i38/93('~')/
     &' Power curve index has been re-set as =',f10.4/93('#')/)
     
      quolity data(IQ,JP,3) = xpr ! remove the power index
      
      endif ! if ( avloadp .gt. 99999.9 ) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      endif ! if ( ecm .gt. 0.00001 )
      endif ! ( IQDIST .eq. 11 ) for power distribution of loads ppppppppppppppp
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp  
  
    
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 10 ) then ! for power distribution of concentration ppppp
      avconcp = 0.0
      rerkmini10 = 0.0
      perkmini10 = 0.01*powermini (IQ,JP)
      rerkmini10 = errx (perkmini10+0.000001)
      baseconc = powerbase (IQ,JP)
      pindex = quolity data (IQ,JP,3)
      perk = 0.0
      kperc10 = 0
      do ii = 1, 5000 ! --------------------------------------------------------
      perk = perk + 0.0002
      rerk = errx (perk)
      EC = EXP ( GECM + GECS * rerk )
      EC = EC ** pindex ! use the power index pppppppppppppppppppppppppppppppppp
      if ( rerk .lt. rerkmini10 ) then ! 
      EC = 0.0
      kperc10 = kperc10 + 1
      avconcp = avconcp + EC
      endif
      
      enddo ! do ii = 1, 5000 --------------------------------------------------

      avconcp = avconcp / 5000.0 ! mean produced by the power curve
      xkperc = 100.0*float(kperc10)/5000.0
      
      write(08,4)dname(JP)
    4 format(/77('=')/
     &'Data were entered as a power curve for concentrations for ',a11)
      write(08,5)XM,IQ,XS,pindex,
     &100.0*perkmini10,xkperc,baseconc,avconcp,quolity data(IQ,JP,MO)
    5 format(77('=')/
     &'  Mean concentration entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'         The cut-off point (perkmini) =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.1/
     &15x,'The base-concentration =',f10.2,' (% of the input mean)'/
     &'     Mean produced by the power curve =',f10.2,
     &' (scales calculated values)'/
     &'          Correlation with river flow =',f10.4/77('='))
      endif ! ( IQDIST .eq. 10 ) for power distribution of concentrations pppppp
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
 
   
      return
      end

 

     
*     compute moments of discharge quality -------------------------------------
      subroutine get the summary statistics of discharge quality 2a
     &(jjp,XM,XS,XXP)
      include 'COMMON DATA.FOR'
            
      GECM = XM
      GECS = XS
      XXP = 0.0
      t95 = errx ( 0.95 )
      
*     0 - constant, uniform values
*     1 - a Normal Distribution
*     2 - the Log-Normal Distribution
*     3 - a Three-Parameter Log-Normal Distribution
*     4 - non-parametric distribution
*     5 - monthly data - distribution for each month
*     6 - as 1 but the data (d) and (e) refer to loads
*     7 - as 2 but the data (d) and (e) refer to loads
*     8 - monthly structure
*     9 - non-parametric distribution of loads
*     10 - a power-curve distribution for concentrations
*     11 - a power-curve distribution for loads?
      
      if ( IQDIST .eq. 0 ) then
      XXP = XM
      return
      endif
      
      if ( IQDIST .eq. 1 .or. IQDIST .eq. 6 .or. IQDIST .eq. 9 ) then
      if ( Detype (jjp) .eq. 104 ) then
      XXP = amax1 (0.0, XM - XS * t95)
      else
      XXP = XM + XS * t95
      endif
      return
      endif
      
      GECM = 0.0
      GECS = 0.0
      
      EM3 = XM + EC3 ! ------------------------ add  the shift to the mean - EC3
      if ( IQDIST .eq. 7 .or. IQDIST .eq. 11 ) then ! ---- but for data as loads
      EM3 = XM ! do not add in EC3 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      endif ! ------------------------------------------------ for data as loads
      if ( IQDIST .eq. 10 ) then ! cccccccccccccccccccc and for the power curves ###############
      EM3 = XM ! do not add in EC3 ccccccccccccccccccccccccccccccccccccccccccccc
      endif ! --------------------------------------------- for data power curve

      EM4 = EM3 * EM3 + XS * XS
      if (EM4 .lt. 1.0E-20) return
      GECM = ALOG((EM3*EM3)/SQRoot(195854,EM4)) ! mean in the log domain ------
      xxx = ALOG(1.+(XS*XS)/(EM3*EM3))
      if ( xxx .lt. 1.0e-20 ) return
      GECS = SQRoot3(195901,xxx) ! standard deviation in the log domain --------

      if ( Detype (jjp) .eq. 104 ) then
      XXP = EXP ( GECM - GECS * t95 )
      else
      XXP = EXP ( GECM + GECS * t95 )
      endif


*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 11 ) then ! for power distribution of loads ppppppppppppp
      avloadp = 0.0
      rerkmini11 = 0.0 ! computing summary statistics for power curve pppppppppp
      perkmini11 = 0.01*powermini(IQ,JP) ! cut off percentile pppppppppppppppppp
      rerkmini11 = errx (perkmini11+0.000001) ! deviate pppppppppppppppppppppppp
      baselode = powerbase (IQ,JP) ! base load of the power curve pppppppppppppp
      pindex = quolity data (IQ,JP,3)
      perk = 0.0
      kperc11 = 0
      do ii = 1, 5000 ! --------------------------------------------------------
      perk = perk + 0.0002
      rerk = errx (perk)
      EC = EXP ( GECM + GECS * rerk )
      EC = EC ** pindex ! use the power index pppppppppppppppppppppppppppppppppp
      
      if ( rerk .lt. rerkmini10 ) then ! 
      EC = 0.0
      kperc11 = kperc11 + 1
      endif
      avloadp = avloadp + EC

      enddo ! do ii = 1, 5000 --------------------------------------------------
      
      avloadp = avloadp / 5000.0 ! mean produced by the power curve
      xkperc = 100.0*float(kperc11)/5000.0
     
      write(09,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)

      if ( ecm .gt. 0.000001 ) then
      write(08,2)dname(JP)
      write(01,2)dname(JP)
    2 format(/77('=')/
     &'Data were entered as a power curve for loads for ',a11)
      write(08,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(01,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
      write(09,3)XM,IQ,XS,quolity data(IQ,JP,3),
     &100.0*perkmini11,xkperc,baselode,avloadp,quolity data(IQ,JP,MO)
    3 format(77('=')/
     &'           Mean load entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'         The cut-off point (perkmini) =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.5/
     &24x,'The base-load =',f10.2,' (% of the input mean)'/
     &'Mean load produced by the power curve =',f10.2,
     &' (scales calculated values)'/
     &'          Correlation with river flow =',f10.4/77('='))
     
      if ( avloadp .gt. 1999999.0 ) then ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      suppress15 = suppress15 + 1
      xpr = 1.0
      call change colour of text (20) ! bright red
      write( *,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr 
      

      call set screen text colour
      write(09,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr 
      write(33,35)dname(JP),uname(feeture),valchars10,IQ,
     &quolity data(IQ,JP,3),int(avloadp),xpr 
   35 format(/93('~')/
     &'(4) Huge loads produced by power curve for ',a11, ! load 2 ---- ERR & SCN
     &' at ',a35/
     &93('~')/'           Mean load entered as input =',a10,
     &12x,' Data-set:',i6/
     &'                    Power curve index =',f10.4/
     &'Mean load produced by the power curve =',i38/93('~')/
     &'             Power curve index set to =',f10.4/93('#')/)
      quolity data(IQ,JP,3) = 1.0
      endif ! if ( avloadp .gt. 99999.9 ) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      endif ! if ( rcm .gt. 0.00001 )
      endif ! ( IQDIST .eq. 11 ) for power distribution of loads ppppppppppppppp
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp  
      
  
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      if ( IQDIST .eq. 10 ) then ! for power distribution of concentration ppppp
      avconcp = 0.0
      rerkmini10 = 0.0
      perkmini10 = 0.01*powermini (IQ,JP)
      rerkmini10 = errx (perkmini10+0.000001)
      baseconc = powerbase (IQ,JP)
      pindex = quolity data (IQ,JP,3)
      perk = 0.0
      kperc10 = 0
      do ii = 1, 5000 ! --------------------------------------------------------
      perk = perk + 0.0002
      rerk = errx (perk)
      EC = EXP ( GECM + GECS * rerk )
      EC = EC ** pindex ! use the power index pppppppppppppppppppppppppppppppppp
      if ( rerk .lt. rerkmini10 ) then ! 
      EC = 0.0
      kperc10 = kperc10 + 1
      endif
      
      avconcp = avconcp + EC
      
      enddo ! do ii = 1, 5000 --------------------------------------------------
      avconcp = avconcp / 5000.0 ! mean produced by the power curve
      xkperc = 100.0*float(kperc10)/5000.0
      
      write(08,4)dname(JP)
    4 format(/77('=')/
     &'Data were entered as a power curve for concentrations for ',a11)
      write(08,5)XM,IQ,XS,pindex,
     &100.0*perkmini10,xkperc,baseconc,avconcp,quolity data(IQ,JP,MO)
    5 format(77('=')/
     &'  Mean concentration entered as input =',f10.4,
     &12x,' Data-set:',i6/
     &'                   Standard deviation =',f10.4/
     &'             Index of the power curve =',f10.4/
     &'         The cut-off point (perkmini) =',f10.2,'-percentile'/
     &'                     Zeroed shots (%) =',f10.1/
     &15x,'The base-concentration =',f10.2,' (% of the input mean)'/
     &'     Mean produced by the power curve =',f10.2,
     &' (scales calculated values)'/
     &'          Correlation with river flow =',f10.4/77('='))
      endif ! ( IQDIST .eq. 10 ) for power distribution of concentrations pppppp
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp      
      
      return
      end

      
*     compute square root and check for error ----------------------------------
      function SQRMB(IX,X)
      if ( X .ge. 0.0 ) goto 1
      Y = SQRT(1-x)
      write( *,2)X,Y,IX
      write(01,2)X,Y,IX
      write(03,2)X,Y,IX
      write(09,2)X,Y,IX
      write(33,2)X,Y,IX
    2 format('ILLEGAL SQUARE ROOT REQUESTED ...',2E13.4,i4)
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
      include 'COMMON DATA.FOR'
      dimension XX(4)
      dimension Y(NS)

*     3 for flow; 4 for concentration ------------------------------------------
      do j = 3,4
      BM(j) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      BS(j) = 0.0 ! scaling for correcting Monte Carlo sampling bias -----------
      enddo
      Bxx = 0.0
      do IS = 1, NS
      EFshots(IS) = 0.0
      enddo      

      call set up the correlation coefficients ! 4444444444444444444 set up bias

      
*     **************************************************************************
      do 2 IS = 1,NS
      Y(IS) = 0.0
      XX(3) = 0.0
      XX(4) = 0.0

      call get the correlated random numbers (IS,R1,R2,R3,R4) ! bias

*     =======         0 - constant, uniform flow
*     =======         1 - flow follows the Normal Distribution            
*     =======         2 - the Log-Normal Distribution     
*     =======         3 - a Three-Parameter Log-Normal Distribution
*     =======         4 - non-parametric distribution                 
*     =======         5 - monthly data - distribution for each month  
*     =======         8 - monthly structure               

*     ------ 0  1  2  3  4  5  6  7  8  9 10 - IFDIST + 1------------------------
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

*     power curve distribution for discharge flows 99999999999999999999999999999
   14 XX(3) = EXP(GEFM+R3*GEFS)-EF3
      
*     non-parametric, seasonal and times series --------------------------------
   49 continue
      
   19 continue
      Y(IS) = XX(3)

*     store the uncorrected river flows ----------------------------------------
*     use these to calculate the correction factors ----------------------------
      EFshots(IS) = XX(3)
      
*     concentration or load ----------------------------------------------------
*     ------ 0  1  2  3  4  5  6  7  8  9 10 11 - IQDIST + 1 -------------------
      goto (21,22,23,23,39,39,22,23,39,39,24,27), IQDIST + 1

*     constant -----------------------------------------------------------------
   21 XX(4) = GECM
      goto 29

*     normal distribution ------------------------------------------------------
   22 XX(4) = GECM+R4*GECS-EC3
      goto 29

*     log-normal distribution --------------------------------------------------
   23 continue
      if ( JT(feeture) .eq. 2 ) then
      XX(4) = EXP(GECM+R4*GECS)-EC3 ! treat the discharge as a stream ==========
      else
      XX(4) = EXP(GECM+R4*GECS)-EC3
      endif
      goto 29

*     power curve distribution for discharge concentrations 999999999999999999999
   24 XX(4) = EXP(GECM+R4*GECS)-EC3
      goto 29

*     power curve distribution for discharge loads pppppppppppppppppppppppppppppp
   27 XX(4) = EXP(GECM+R4*GECS)

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
      BM(J) = BM(J)/NS
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
      if ( jt(feeture) .eq. 2 ) BS(3) = Flow(2)
      
*     compute correction factors for discharge flow ----------------------------
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = EFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) Bxx = EF5/BS(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = EFS/BS(3)
      
      if ( jt(feeture) .eq. 2 ) BS(3) = BXX
      
*     compute correction factors for discharge quality -------------------------
      if ( BM(4) .gt. 1.0E-08 ) BM(4) = XM/BM(4) 
      
      if ( IQDIST .eq. 7 .or. IQDIST .eq. 11 ) then ! ++++++++++++++++++++++++++
      BS(4) = 1.0 ! ignore adjustments to the standard deviation +++++++++++++++
      else ! fpr all other types of data =======================================
      if ( BS(4) .gt. 1.0E-08 ) BS(4) = XS/BS(4) ! include them ================
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      return
      end



*     compute percentile of effluent quality -----------------------------------
      subroutine get effluent quality 95 percentile
      include 'COMMON DATA.FOR'
      dimension EQ(NS)

      ECX = 0.0
      if (ECM .lt. 1.0E-10) return

      call get the summary statistics of discharge quality (ECM,ECS) 
      call bias in river or discharge flow and quality (ECM,ECS)

*     set up the correlation ....  (also done in BIAS ) ------------------------
      if ( IQDIST .gt. 4 .and. qtype (jp) .ne. 4 ) then
      call set up the correlation coefficients ! 44444444444444444 95 percentile
      endif

*     set up and write out the shots from the non-parametric distributions -----
      call set up the shots for non parametric discharge flow
      call set up the shots for non parametric discharge quality
      
*     set up and write out the shots for monthly data --------------------------
      call set up monthly data for discharge flow
      call set up monthly data for discharge quality

*     set up and write out the shots for monthly data --------------------------
      call set up monthly structure for discharge flow 8
      call set up monthly structure for discharge quality 8

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
      !call write shots for discharge quality
      endif

      return
      end


      
      
*     COMPUTE RIVER QUALITY DOWNSTREAM OF CURRENT EFFLUENT QUALITY -------------
      subroutine get downstream river quality 
      include 'COMMON DATA.FOR'
      dimension RQ(MS) ! downsteam river quality

      current ds quality = 0.0
    
      call get the summary statistics of discharge flow
      call get the summary statistics of discharge quality (ECM,ECS)
      call bias in river or discharge flow and quality (ECM,ECS)
      call set up the correlation coefficients ! 44444444444 d/s of the effluent

*     sample the distributions -------------------------------------------------
      do 2 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS,R1,R2,R3,R4) ! d/s of effluent

      RF = FMS(IS)
      RC = CMS(JP,IS)

*     get the shots for the discharge (or tributary) ---------------------------
      call get a flow for the stream or discharge (IS, R3, EF)

*     deal with addition of load but not flow ----------------------------- LOAD
      EFMB = EF ! --------------------------------------------------------- LOAD
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load ---------------- LOAD
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 ) then ! load ---------------- LOAD
      EFMB = 1.0 ! -------------------------------------------------------- LOAD
      endif ! addition of load but not flow ------------------------------- LOAD

      call get the quality of the discharge (IS,R4,EC,ECM)

*     mass balance -------------------------------------------------------------
      RQ(IS) = ( (RF*RC) + (EFMB*EC) ) / ( RF + EF )
    2 continue

*     compute the statistics of river quality ... find the percentile ----------
      current ds quality = Get river percentile (RQ)
*     find the mean ------------------------------------------------------------
      if ( MRQS(JP) .eq. 1 ) current ds quality = 
     &get the mean river quality (RQ)

      return
      end



*     find the percentile for river quality ------------------------------------
      function get river percentile (RRQ)
      include 'COMMON DATA.FOR'
      dimension Q(NS),RRQ(NS)
      
      do is = 1,NS
      Q(IS) = RRQ(IS)
      enddo

      kkk = k95
      if ( kptarg .eq. 99 ) kkk = k99
      if ( kptarg .eq. 90 ) kkk = k90
      if ( kptarg .eq. 95 .and. detype (JP) .eq. 104 ) kkk = k05
      if ( kptarg .eq. 90 .and. detype (JP) .eq. 104 ) kkk = k10

      if ( detype (JP) .ge. 900 ) then ! dissolved and solid metal
      if ( partishun (JP) .gt. 0.01 ) then
      gkay = 10.0 ** partishun (JP)
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
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
      dimension Q(NS)
      
      TEMP = 0.0
      do I = 1,NS
      TEMP = TEMP + Q(I)
      enddo
      
      if ( detype (JP) .ge. 900 .and. partishun(JP) .gt. 0.01 ) then
      TEMP = 0.0
      gkay = 10.0 ** partishun (JP)
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


      
*     calculate the standard deviation -----------------------------------------
      function get standard deviation of river quality (Q)
      include 'COMMON DATA.FOR'
      dimension Q(NS)
      
      TEM = 0.0
      do I = 1,NS
      TEM = TEM + Q(I)
      TES = TES + Q(I)*Q(I)
      enddo
      
      if ( detype (JP) .ge. 900 .and. partishun(JP) .gt. 0.01 ) then
      TEMP = 0.0
      gkay = 10.0 ** partishun (JP)
      do IS = 1, NS
      spm = BMS(2,IS)
      totalx = Q(IS)
      diss = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal
      TEM = TEM + diss
      TES = TES + diss*diss
      enddo
      endif ! dissolved and solid metal
      
      TES = (TES-TEM*TEM/NS)/(NS-1)
      get standard deviation of river quality = sqrt(TES)
      
      return
      end


*     calculate the standard deviation -----------------------------------------
      function get standard deviation of discharge quality (Q)
      include 'COMMON DATA.FOR'
      dimension Q(NS)
      
      TEM = 0.0
      do I = 1,NS
      TEM = TEM + Q(I)
      TES = TES + Q(I)*Q(I)
      enddo
      
      TES = (TES-TEM*TEM/NS)/(NS-1)
      get standard deviation of discharge quality = sqrt(TES)
      
      return
      end
    
      

      function get the mean discharge quality (Q)
      include 'COMMON DATA.FOR'
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
*     The values in H and G are used to extimate subsequent values of Z....
*     The iterations continue until the calculated value of Z produces the 
*     value TARGET when substituted into the equation.
*     --------------------------------------------------------------------------
      subroutine itrate (Icheck, TARGET)
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'

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
*     apply to (3) (5) (12) (39) (60) (61) ------------------------------ mixing
      if ( itype .eq. 03 .or. itype .eq. 05 .or. itype .eq. 12 .or. 
     &itype .eq. 39 .or. itype .eq. 60 .or. itype .eq. 61 ) then
          
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
      include 'COMMON DATA.FOR'

*     if ( jp .eq. ndetfirst ) then
      if ( IFDIST .eq. 5 ) then

*     for a stream -------------------------------------------------------------
      if ( JT (KFEAT) .eq. 18) call generate monthly stream flow data

*     or for effluents ---------------------------------------------------------
      if ( JT (KFEAT) .eq. 19 ) call monthly discharge flow
      endif
*     endif

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
      subroutine get a flow for the stream or discharge (IS, R3, EF)
      include 'COMMON DATA.FOR'
      
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

*     log-normal distribution (2 and 3-parameter) ==============================
      if ( ifdist .eq. 2 .or. ifdist .eq. 3 ) then ! ===========================
      !if ( munthly structure .eq. 0 ) then ! - no requirement for monthly loads
*     effluent type: based on mean and standard deviation ------------ effluents
      if ( jt(feeture) .eq. 03 .or. jt(feeture) .eq. 05  .or.
     &     jt(feeture) .eq. 22 .or. jt(feeture) .eq. 23 .or.
     &     jt(feeture) .eq. 19 .or. jt(feeture) .eq. 12 .or.
     &     jt(feeture) .eq. 39 .or.
     &     jt(feeture) .eq. 60 .or. jt(feeture) .eq. 61 ) then ! monthly loads -
          
      EM3 = EFM + EF3
      if ( EM3 .lt. 1.0e-9 ) return
      GEFM = ALOG((EM3*EM3)/SQRoot(1057,EM3*EM3+EFS*EFS))
      TRAK = ALOG(1.+(EFS*EFS)/(EM3*EM3))
      if ( TRAK .gt. 1.0e-20) then
      GEFS = SQRoot(105656,TRAK)
      endif

      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
      EFshots (IS) = EF
      endif ! if ( jt(feeture) .eq. 3 ------------------------------------------

*     river type: based on mean and 95-percentile --------- river-type additions
      if ( ifdiffuse .eq. 0 ) then ! --------------------- non-diffuse additions
      if ( jt(feeture) .eq. 2  .or. 
     &     jt(feeture) .eq. 20 .or. jt(feeture) .eq. 21 .or.
     &     jt(feeture) .eq. 18 ) then
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif ! if ( jt(feeture) .eq. 2 ------------------------------------------
      endif !  if ( ifdiffuse .eq. 0 ) ------------------- non-diffuse additions 

      
      if ( ifdiffuse .eq. 1 ) then ! ======================== river-type diffuse 
*     EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 ) == river-type diffuse
      EF = amax1 ( 1.0e-9, EXP(R3*GEFS+GEFM) - EF3 ) ! ====== river-type diffuse
      endif ! if ( ifdiffuse .eq. 1 ) ======================= river-type diffuse
      if ( ifdiffuse .eq. 2 ) then ! ===================== effluent type diffuse
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM ) ! ====== type diffuse
      endif ! if ( ifdiffuse .eq. 2 ) -------------------- effluent type diffuse

      EFshots (IS) = EF ! --------------------------------------- store the flow
      return
      !endif ! if ( munthly structure .eq. 0 ) ----------------------------------

*     ==========================================================================      
      if ( munthly structure .gt. 0 ) then ! ----- requirement for monthly loads
      EF = EFshots (IS)
 7732 format(/77('-')/
     &'get a flow for the stream or discharge .M.'/77('-')/
     &10x,' BM(3) =',f10.5/10x,' BS(3) =',f10.5/10x,'   EF5 =',f10.3/
     &77('-'))
      return
      endif ! if ( munthly structure .gt. 0 ) ---- requirement for monthly loads
*     ==========================================================================      
 
      
      
      endif ! if ( ifdist .eq. 2 .or. ifdist .eq. 3 )  log-normal ==============
*     ==========================================================================



      
*     power curve distribution (stream or discharge flow) ----------------------
      if ( ifdist .eq. 10 ) then ! power curve distribution --------------------
*     effluent type: based on mean and standard deviation ----------------------
      if ( jt(feeture) .eq. 03 .or. jt(feeture) .eq. 05 .or.
     &     jt(feeture) .eq. 22 .or. jt(feeture) .eq. 23 .or.
     &     jt(feeture) .eq. 19 .or. jt(feeture) .eq. 12 .or.
     &     jt(feeture) .eq. 39 .or.
     &     jt(feeture) .eq. 60 .or. jt(feeture) .eq. 61 ) then ! power curves --
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
      endif ! if ( ifdist .eq. 10 ) --------------------------------------------
*     river type: based on mean and 95-percentile ------------------------------
      if ( ifdiffuse .eq. 0 ) then ! non-diffuse flows -------------------------
      if ( jt(feeture) .eq. 2  .or. 
     &     jt(feeture) .eq. 20 .or. jt(feeture) .eq. 21 .or.
     &     jt(feeture) .eq. 18 ) then
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
      endif ! non-diffuse additions --------------------------------------------
      if ( ifdiffuse .eq. 1 ) then ! river-type diffuse flows ------------------
      EF = VALFL ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EF5 )
      endif
      if ( ifdiffuse .eq. 2 ) then ! effluent type diffuse flows ---------------
      EF = Vlogn ( R3, GEFS, GEFM, EF3, BM(3),BS(3), EFM )
      endif ! if ( ifdiffuse .eq. 2 ) ------------------------------------------
      EFshots (IS) = EF ! store the outcome ------------------------------------
      return
      endif ! if ( ifdist .eq. 10 ) power curve distribution -------------------


*     non-parametric, monthly or structured distribution -----------------------
      if ( ifdist .eq. 4 .or. ifdist .eq. 5 .or. ifdist .eq. 8) then
      EF = EFshots (IS)
      return
      endif

      if ( ifdist .gt. 8 ) then ! illegal flow distribution --------------------
      EF = EFshots (IS)
      call change colour of text (14) ! bright yellow
      write( *,99)IQDIST
      call set screen text colour
      write(01,99)IQDIST
      write(09,99)IQDIST
      write(33,99)IQDIST
   99 format(//77('=')/
     &'*** Illegal code for a flow distribution ...',i8/
     &77('='))
      call stop
      endif ! if ( ifdist .gt. 8 ) ---------------------------------------------

      return
      end


      
      
      subroutine get the quality of the discharge (IS,R4,EC,XMEAN)
      include 'COMMON DATA.FOR'
            
      EC = 0.0
      if ( qtype (jp) .eq. 4 ) return
      if ( xmean .lt. 0.0000000001 ) return
      call get the correlated random numbers (IS,R1,R2,R3,R4)

*     log-normal distribution (2 and 3-parameter) ------------------------------
      if ( IQDIST .eq. 2 .or. IQDIST .eq. 3 ) then  
      !if ( JT(feeture) .eq. 2 ) R4 = R5 ! treat the discharge as a stream ======
      EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN)
      return
      endif ! if ( IQDIST .eq. 2 .or. IQDIST .eq. 3 ) --------------------------
            
*     normal distribution ------------------------------------------------------
      if ( IQDIST .eq. 1 ) then
      EC = Vnorm ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN )
      return
      endif

*     constant quality ---------------------------------------------------------
      if ( IQDIST .eq. 0 ) then
      EC = ECM
      return
      endif

*     normal distribution of loads ---------------------------------------------
      if ( IQDIST .eq. 6 ) then ! normal loads
      EC = Vnorm ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN )
      return
      endif


*     lognormal distribution of loads 7777777777777777777777777777777777777-LOAD
      if ( IQDIST .eq. 7 ) then ! log normal loads 777777777777777777777777-LOAD
      EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN)
      return
      endif ! if ( IQDIST .eq. 7 ) 7777777777777777777777777777777777777777-LOAD

      
*     power curve for loads 11-11-11-11-11-11-11-11-11-11-11-11-11-11-11-11-LOAD
      if ( IQDIST .eq. 11 ) then ! log normal power curve for loads  -11-11-LOAD
*     if ( ifdiffuse .ne. 1 ) then ! include the elimination of MC errors --LOAD
      EC = Vlogn ( R4, GECS, GECM, 0.0, BM(4), BS(4), XMEAN)

      pindex = quolity data (IQ,JP,3)
      EX = EC

      if ( R4 .lt. rerkmini11 ) EC = 0.0 ! power curve for loads ppppppppppppppp
      
      if ( avloadp .gt. 0.0001 ) then
      EC = (EC**pindex) * ECM * ((100.0-baselode)/100.0) / avloadp ! ppppppppppp
     &        + ECM * baselode/100.0 ! power curve for loads ppppppppppppppppppp
      endif

      if ( EC .gt. 99999000.0 ) then ! -----------------------------------------
      call change colour of text (20) ! bright red
      write( *,2383)pindex,uname(feeture),rname(IREACH),JQ(feeture),
     &EX**pindex
      call set screen text colour ! error message ------------------------------
      write(33,2383)pindex,uname(feeture),rname(IREACH),JQ(feeture),
     &EX**pindex
      write(09,2383)pindex,uname(feeture),rname(IREACH),JQ(feeture),
     &EX**pindex
 2383 format(96('*')/'A power curve has led to a load that exceeds ',
     &'99 million ... Index =',f8.2/'This is for feature: ',a40,
     &' ... on reach: ',a16/'Data-set:',i6,f81.0/96('*'))
 9383 format(96('*')/'A power curve has led to a load that exceeds ',
     &'99 million ... Index =',f8.2/'This is for feature: ',a40,
     &' ... on reach: ',a16/'Data-set:',i6,i81/96('*'))
      endif ! if ( EC .gt. 9999999.0 ) -----------------------------------------
  
      return ! power curve for loads ppppppppppppppppppppppppppppppppppppppppppp
      endif ! if ( IQDIST .eq. 11 ) -11-11-11-11-11-11-11-11-11-11-11-11-11-LOAD
      

      
*     power curve for concentrations 10-10-10-10-10-10-10-10-10-10-10-10-10-CONC
      if ( IQDIST .eq. 10 ) then ! log normal power curve for concs  -10-10-CONC
*     if ( ifdiffuse .ne. 1 ) then ! include the elimination of MC errors --CONC
      EC = Vlogn ( R4, GECS, GECM, 0.0, BM(4), BS(4), XMEAN)
*     EC = EXP ( GECM + GECS * R4 ) ! without adjustments for errors -10-10-CONC
      pindex = quolity data (IQ,JP,3)
      
      EC=EC**pindex ! power curve for concentrations cccccccccccccccccccccc conc
      
      if ( R4 .lt. rerkmini10 ) EC = 0.0 ! power curve for concentrations c-CONC
      
      EC = EC * XMEAN * ((100.0 - baseconc)/100.0) / avconcp ! cccccccccccc-CONC
     &        + XMEAN * baseconc/100.0 ! power curve for concentrations ccc-CONC
      
      return ! power curve for concentrations ccccccccccccccccccccccccccccc-CONC
      endif ! if ( IQDIST .eq. 10 ) -10-10-10-10-10-10-10-10-10-10-10-10-10-CONC
      

*     power curve distribution (discharge quality) ---- power curve distribution
*     if ( IQDIST .eq. 11 ) then ! power curve (discharge quality) 99999999 LOAD
*     EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN) ! -----------------
*       if ( ifdiffuse .ne. 1 ) then ! include the elimination of MC errors LOAD
*       EC = Vlogn ( R4, GECS, GECM, EC3, BM(4), BS(4), XMEAN) ! ---------------
*       else ! these adjustments affect the results for diffuse loads ----------
*       EC = EXP ( GECM + GECS * R4 ) ! without adjustments for errors 999999999
*         if ( EC3 .gt. 1.0e-6 ) then ! 9999999999999999999999999999999999999999
*         EC = EC ** EC3 ! apply the power curve 9999999999999999999999999999999
*         endif ! 99999999999999999999999999999999999999999999999999999999999999
*       endif ! if ( ifdiffuse .ne. 1 ) 9999999999999999999999999999999999999999
*     return
*     endif ! type 11 999999999999999999999999999999999 power curve distribution
      

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

      if ( IQDIST .eq. 8 ) then ! monthly structure for discharge concentrations 
      EC = ECshots(IS)
      if ( is .lt. 3 ) then
      endif
      return
      endif
      
      if ( IQDIST .gt. 11 ) then
      EC = ECshots(IS)
      call change colour of text (14) ! bright yellow
      write( *,99)IQDIST
      call set screen text colour
      write(01,99)IQDIST
      write(09,99)IQDIST
      write(33,99)IQDIST
   99 format(//77('=')/
     &'*** Illegal code for a quality distribution ...',i8/
     &77('='))
      call stop
      return
      endif

      return
      end


      subroutine write out the added flow shots
      include 'COMMON DATA.FOR'
      if ( JP .eq. ndetfirst ) then
      if ( MONF .gt. 1 ) then
      call write the shots for the flow from the discharge
      call write the shots for the flow from the stream
      endif
      endif
      return
      end

      
      
      
      subroutine write the correction factors for Monte Carlo errors
      include 'COMMON DATA.FOR'

      if ( Qtype (JP) .ne. 4 ) then
      if ( MONF .gt. 1 ) then
      !if ( nobigout .le. 0 ) write(01,128)BM(3),BS(3)
  128 format(77('-')/'Flows ...',6x,
     &'Correction factors for Monte-Carlo sampling ... '/77('-')/
     &'                 Mean BM(3) =',F8.3/
     &'         5-percentile BS(3) =',F8.3/77('-'))
      endif
      !if ( nobigout .le. 0 ) write(33,128)BM(3),BS(3)

      if ( MONQ .gt. 1 ) then
      if ( ifdiffuse .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,13)Dname(JP),BM(4),BS(4)
   13 format(77('-')/A11,8x,
     &'Correction factors for sampling errors:'/77('-')/
     &'Mean          =',F8.4,31x,'95-percentile =',F8.4/77('-'))
      else
      if ( nobigout .le. 0 ) write(01,5513)Dname(JP),BM(4),BS(4)
 5513 format(33x,77('-')/33x,A11,8x,
     &'Corrections for sampling errors:'/33x,77('-')/33x,
     &'Mean          =',F8.4,31x,'95-percentile =',F8.4/33x,77('-'))
      endif
      endif
      
      endif
      return
      end

      
      
      
      subroutine write the correlation coefficients
      include 'COMMON DATA.FOR'
      
      if ( qtype (jp) .eq. 4 ) return
      if ( ical13 .eq. 1 ) return ! running in calibration mode ----------------
      if ( nobigout .eq. 1 ) return
      if ( IPRINT .eq. 1 ) return
      
      check = abs(CO1) + abs(CO2) + abs(CO3) + abs(CO4) + abs(CO5) 
     &                 + abs(CO6)
      if ( check .lt. 0.00001) return
      
      if ( ifdiffuse .eq. 0 ) then ! ===========================================
      write(01,7)Dname(jp),uname(Kfeat)
    7 format(/77('-')/'Non-zero correlation coefficients for ',
     &a11,' at ',a37/77('-'))
      write(01,1)CO1
    1 format(26x,'        River flow on river quality (CO1) = ',F7.4)
      write(01,2)CO2
    2 format(26x,'           Added flow on river flow (CO2) = ',F7.4)
      if ( CO3 .lt. - 1.0e-09 .or. CO3 .gt. 1.0e-09 ) then
      write(01,3)CO3 ! 44444444
    3 format(26x,'        River flow on added quality (CO3) = ',F7.4) ! 44444444
      endif
      if ( CO4 .lt. - 1.0e-09 .or. CO4 .gt. 1.0e-09 ) then
      write(01,4)CO4 ! 44444444
    4 format(26x,'        River quality on added flow (CO4) = ',F7.4) ! 44444444
      endif
      if ( CO5 .lt. - 1.0e-09 .or. CO5 .gt. 1.0e-09 ) then
      write(01,5)CO5
    5 format(26x,'        Added flow on added quality (CO5) = ',F7.4)
      endif
      if ( CO6 .lt. - 1.0e-09 .or. CO6 .gt. 1.0e-09 ) then
      write(01,6)CO6,uname(KFEAT) ! 44444444
    6 format(26x,'     River quality on added quality (CO6) = ',F7.4) ! 44444444
      endif
      write(01,8)
    8 format(77('-'))
      
      else ! if ( ifdiffuse .eq. 0 ) === ifdiffuse > 0 =========================
      !write(01,17)Dname(jp),uname(KFEAT)
   17 format(33x,77('-')/33x,'Non-zero correlation coefficients for ',
     &a11,' at ',a37/33x,77('-'))
      if ( CO1 .lt. - 1.0e-09 .or. CO1 .gt. 1.0e-09 ) then
      !write(01,11)CO1
   11 format(59x,'        River flow on river quality (CO1) = ',F7.4)
      endif
      if ( CO2 .lt. - 1.0e-09 .or. CO2 .gt. 1.0e-09 ) then
      !write(01,12)CO2
   12 format(59x,'           Added flow on river flow (CO2) = ',F7.4)
      endif
      if ( CO3 .lt. - 1.0e-09 .or. CO3 .gt. 1.0e-09 ) then
      !write(01,13)CO3 ! 44444444
   13 format(59x,'        River flow on added quality (CO3) = ',F7.4)  ! 4444444
      endif
      if ( CO4 .lt. - 1.0e-09 .or. CO4 .gt. 1.0e-09 ) then
      !write(01,14)CO4 ! 44444444
   14 format(59x,'        River quality on added flow (CO4) = ',F7.4)  ! 4444444
      endif
      if ( CO5 .lt. - 1.0e-09 .or. CO5 .gt. 1.0e-09 ) then
      !write(01,15)CO5
   15 format(59x,'        Added flow on added quality (CO5) = ',F7.4)
      endif
      if ( CO6 .lt. - 1.0e-09 .or. CO6 .gt. 1.0e-09 ) then
      !write(01,16)CO6 ! 44444444
   16 format(59x,'     River quality on added quality (CO6) = ',F7.4)  ! 4444444
      endif
      !write(01,18)
   18 format(33x,77('-'))
      endif ! if ( ifdiffuse .eq. 0 ) ==========================================
      
      return
      end

      
      subroutine set up data for added flow and quality
      include 'COMMON DATA.FOR'
     
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
      call set up monthly structure for stream quality 8
      call set up monthly structure for discharge quality 8

      if ( munthly structure .eq. 1 ) then !  impose monthly structure on flows
      JTYPE = JT(KFEAT)
      if ( JTYPE .eq. 02 .or. JTYPE .eq. 13 .or. JTYPE .eq. 18 .or.
     &     JTYPE .eq. 20 .or. JTYPE .eq. 21 .or. JTYPE .eq. 25 .or.
     &     JTYPE .eq. 27 .or. JTYPE .eq. 29 .or. JTYPE .eq. 31 .or.
     &     JTYPE .eq. 33 .or. JTYPE .eq. 35 .or. JTYPE .eq. 46 .or.
     &     JTYPE .eq. 48 .or. JTYPE .eq. 50 .or. JTYPE .eq. 52 .or.
     &     JTYPE .eq. 54 .or. JTYPE .eq. 56 .or. JTYPE .eq. 58 .or.
     &     JTYPE .eq. 37 .or. JTYPE .eq. 40 ) then
      call set up monthly structure for stream flow 2
      endif ! if ( JTYPE .eq. 02 etc fffffffffffffffffffffffffffffffffffffffffff 
      
      if ( JTYPE .eq. 03 .or. JTYPE .eq. 05 .or. JTYPE .eq. 12 .or.
     &     JTYPE .eq. 15 .or. JTYPE .eq. 19 .or. JTYPE .eq. 22 .or.
     &     JTYPE .eq. 23 .or. JTYPE .eq. 39 .or. JTYPE .eq. 42 .or.
     &     JTYPE .eq. 60 .or. JTYPE .eq. 61 .or. JTYPE .eq. 62 ) then
      call set up monthly structure for discharge flow 2
      endif ! if ( JTYPE .eq. 03 ddddddddddddddddddddddddddddddddddddddddddddddd
      endif ! if ( munthly structure .eq. 1 ) ==================================
      
      return
      end
      
 
    
 
      subroutine inititialise the stores for the estimates of load
      include 'COMMON DATA.FOR'
      do L13 = 1, N13
      NSM (JP,L13) = 0 ! ----------------------------- number of shots per month
      ELOAD (JP,L13) = 0.0 ! ---------------- initialise the stores of mean load
      diffuse load (JP,L13) = 0.0 ! ------- stores of loads of diffuse pollution

*     ====================================================================== 158
      ELOAD03 (JP,L13) = 0.0 ! --------------------------- initialise the stores
      ELOAD05 (JP,L13) = 0.0 ! --------------------------- initialise the stores
      ELOAD12 (JP,L13) = 0.0 ! --------------------------- initialise the stores
      ELOAD39 (JP,L13) = 0.0 ! --------------------------- initialise the stores
      ELOAD60 (JP,L13) = 0.0 ! --------------------------- initialise the stores
      ELOAD61 (JP,L13) = 0.0 ! --------------------------- initialise the stores
*     fish farms are dealt with as an abstraction ------------------------------
*     ELOAD62 (JP,L13) = 0.0 ! ------------ initialise the stores for fish farms
*     ====================================================================== 158

      enddo
      ELOADS (JP) = 0.0 ! ---------- initialise the stores of standard deviation
      return
      end 

 
     
      subroutine accumulate the loads ! for this determinand         
      include 'COMMON DATA.FOR'

*     accumulate total loads from all effluents ================================
      if ( JT(feeture) .eq. 03 .or. JT(feeture) .eq. 05 .or. 
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61 ) then
      nx = n13
      
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx      
      TELODE1(JP,J1) = TELODE1(JP,J1) + eload(JP,J1) ! total load from effluents 

*     ====================================================================== 158
*     net loads from discharges (3 12 5 39 60 and 61) -------------------TELODE2
      TELODE2(JP,J1) = TELODE2(JP,J1) + eload(JP,J1)
      T03LOAD1(JP,J1) = T03LOAD1(JP,J1) + eload03(JP,J1) ! sewage works
      T05LOAD1(JP,J1) = T05LOAD1(JP,J1) + eload05(JP,J1) ! industrial effluents
      T12LOAD1(JP,J1) = T12LOAD1(JP,J1) + eload12(JP,J1) ! intermittent sewage
      T39LOAD1(JP,J1) = T39LOAD1(JP,J1) + eload39(JP,J1) ! mine waters
      T60LOAD1(JP,J1) = T60LOAD1(JP,J1) + eload60(JP,J1) ! other point sources
      T61LOAD1(JP,J1) = T61LOAD1(JP,J1) + eload61(JP,J1) ! private wastewaters
*     fish farms dealt with as an abstraction ----------------------------------
*     T62LOAD1(JP,J1) = T62LOAD1(JP,J1) + eload62(JP,J1) ! fish farms
      T03LOAD2(JP,J1) = T03LOAD2(JP,J1) + eload03(JP,J1) ! sewage works
      T05LOAD2(JP,J1) = T05LOAD2(JP,J1) + eload05(JP,J1) ! industrial effluents
      T12LOAD2(JP,J1) = T12LOAD2(JP,J1) + eload12(JP,J1) ! intermittent sewage
      T39LOAD2(JP,J1) = T39LOAD2(JP,J1) + eload39(JP,J1) ! mine waters
      T60LOAD2(JP,J1) = T60LOAD2(JP,J1) + eload60(JP,J1) ! other point sources
      T61LOAD2(JP,J1) = T61LOAD2(JP,J1) + eload61(JP,J1) ! private wastewaters
*     fish farms dealt with as an abstraction ----------------------------------
*     T62LOAD2(JP,J1) = T62LOAD2(JP,J1) + eload62(JP,J1) ! fish farms 
*     ====================================================================== 158

      enddo ! do J1 = 1, nx
            
*     ====================================================================== 158
      if ( mark works .gt. 0 ) then ! --------- save the loads for back-tracking
      TELOADAV(kountworks,JP) = eload(JP,i13) ! 33333333333333333333333333333333
      endif ! -------------------------------- saved the loads for back-tracking
*     ====================================================================== 158

      endif ! ======================= accumulated total loads from all effluents
    
      
*     ====================================================================== 158
*     accumulate total loads from tributaries and headwaters ===================
      if ( JT(feeture) .eq. 2 ) then ! accumulate total loads from streams =====
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx      
      TRLODE2(JP,J1) = TRLODE2(JP,J1) + eload(JP,J1) ! -------- from tributaries
      TRLODE1(JP,J1) = TRLODE1(JP,J1) + eload(JP,J1) ! -------- from tributaries
      TGLODE2(JP,J1) = TGLODE2(JP,J1) + eload(JP,J1) ! -------- from tributaries
      TGLODE1(JP,J1) = TGLODE1(JP,J1) + eload(JP,J1) ! -------- from tributaries
      enddo
      endif ! ========== accumulated total loads from tributaries and headwaters
*     ====================================================================== 158

      return
      end