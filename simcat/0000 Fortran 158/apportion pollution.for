*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ....
*     ==========================================================================
*     File: apportion pollution.for ... 2573 lines -----------------------------
*     --------------------------------------------------------------------------
*     This file deals with the breakdown of pollution between sources ---------- 
*     --------------------------------------------------------------------------
*     This file contains sub-routines called
*     --------------------------------------------------------------------------
*     ...... proportion of works and diffuse (kdirect)
*            (calculate the contributions from individual effluent discharges) 
*     ...... proportion of catchments (kdirect) 
*            (calculate the contributions from individual catchments)
*     ...... write out the portion 
*     ==========================================================================

      subroutine proportion of works and diffuse (kdirect)
      include 'COMMON DATA.FOR'
      
*     ==========================================================================     
*     kdirect is 0 .... at the start of the reach
*     kdirect is 1 .... at a feature
*     kdirect is 3 .... at the end of the reach
*     kdirect is 9 .... at the end of the model
*     ==========================================================================
   
*     ==========================================================================
      character *10 TenAV(ndet)
      character *40 GISreach
*     ==========================================================================

      real storedis(NUED)
 
      real total effluent prop (ndet), total effluent load (ndet)
      real xworksAV(ndet),xworksTE(ndet)
      character *16 SET1
      character *11 SET2
      character *06 SET3
      dimension Y(NS)

      real xworks(ndet) ! :::::::::::::::::::::::::::::::::::::::::::::::::: 158

      if ( ical13 .eq. 1 ) return ! exit if running in gap-filling set up ------
      
      if ( kdirect .eq. 9 ) return ! no output for the end of the model --------
      if ( kdirect .eq. 1 ) then ! exclude the following features --------------
      if ( JT(feeture) .gt. 24 .and. JT(feeture) .lt. 39 ) return    
      if ( JT(feeture) .gt. 39 .and. JT(feeture) .lt. 44 ) return
      if ( JT(feeture) .gt. 44 .and. JT(feeture) .lt. 60 ) return
      if ( JT(feeture) .gt. 13 .and. JT(feeture) .lt. 17 ) return
      if ( JT(feeture) .eq. 10 ) return
      endif ! if ( kdirect .eq. 1 ) --------------------------------------------
      
      ihead = 0 ! initialise marker that a heading has been written ------------
      Small = 1.0e-6

      do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(idet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++
      xworksAV(idet) = 0.0 !  % from a works of the total load in the river 
      endif ! if ( QTYPE (idet) .ne. 4 ) ++++++++++++++++++++++++++++++++++
      enddo ! do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++

      do iw = 1,NUED
      storedis(iw) = 0.0
      enddo
      
      unamex = 'nothing' ! initialise
      if ( kdirect .eq. 1 ) unamex = uname(feeture)
      if ( kdirect .eq. 9 ) unamex = 'the end of the model ...'
      if ( kdirect .eq. 0 ) then
      write(unamex,1800)rname(IREACH)
 1800 format('Start of reach: ',a16)
      jxtype = 111 ! start of a reach
      endif ! if ( kdirect .eq. 0 )
      if ( kdirect .eq. 3 ) then
      write(unamex,1801)rname(IREACH)
 1801 format('End of reach: ',a16)
      jxtype = 999 ! end of a reach
      endif ! if ( kdirect .eq. 3 )
*     ==========================================================================    



*     =============================================--------------===============
*     STORED CONTRIBUTIONS OF LOAD ================ Feature Type ===============
*     =============================================--------------===============
*     ( 2) = 'Added by sewage effluents                      (3)'  1 
*     ( 3) = 'Intermittent discharges of sewage             (12)'  2
*     ( 4) = 'Added by industrial discharges                 (5)'  3 
*     ( 5) = 'Added by mine waters                          (39)'  4
*     (28) = 'Other point sources                           (60)'  5
*     (29) = 'Private wastewaters                           (61)'  6
*     (30) = 'Fish farms                                    (62)'  7
*     ( 1) = 'Mean from all discharges      (3,12,5,39,60,61,62)'  8
*     --------------------------------------------------------------------------
*     ( 6) = 'Diffuse pollution from livestock              (25)'  9
*     ( 7) = 'Diffuse pollution from arable                 (27)' 10
*     ( 8) = 'Highway runoff                                (29)' 11
*     ( 9) = 'Urban runoff                                  (31)' 12
*     (10) = 'Atmosphere deposition                         (33)' 13
*     (11) = 'Natural background                            (35)' 14
*     (12) = 'Septic tanks                                  (37)' 15
*     (13) = 'Aggregated CSOs                               (40)' 16
*     (14) = 'Aggregated STWs                               (42)' 17
*     (15) = 'Diffuse mines                                 (46)' 18
*     (16) = 'Birds, boats and angling                      (48)' 19
*     (23) = 'User-named diffuse input                      (50)' 20
*     (24) = 'User-named diffuse input                      (52)' 21
*     (25) = 'User-named diffuse input                      (54)' 22
*     (26) = 'User-named diffuse input                      (56)' 23
*     (27) = 'User-named diffuse input                      (58)' 24
*     (33) = 'Total from diffuse pollutions (NTF=33) (25,27,etc)' 
*     --------------------------------------------------------------------------
*     (17) = 'Boundaries and tributaries              (2 and 10)' 26
*     (18) = 'Diffuse input                         Feature (13)' 27
*     (19) = 'Diffuse input                         Feature (15)' 28
*     (20) = 'Reach diffuse input              (no Feature code)' 29
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (21) = 'Added by gap filling of flows    (no Feature code)'
*     (22) = 'Added by gap filling of quality  (no Feature code)'
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (31) = 'Not used'
*     (32) = 'Grand total (NTD = 32)           (everything)     ' 
*     (33) = 'Total from diffuse pollutions (NTF = 33) (several Feature codes) 
*     ==========================================================================


      if ( kdirect .eq. 1 ) then ! the results are for features ! FFFFFFFFFFFFFF
      if ( 
     &     JT(feeture) .ne. 01 .and. ! monitoring station 
     &     JT(feeture) .ne. 24 .and. ! end of water body
*    &     JT(feeture) .ne. 02 .and. ! stream
     &     JT(feeture) .ne. 04 .and. ! flow gauge
     &     JT(feeture) .ne. 06 .and. ! added plotting point
*    &     JT(feeture) .ne. 07 .and. ! abstraction
*    &     JT(feeture) .ne. 18 .and. ! abstraction
*    &     JT(feeture) .ne. 19 .and. ! abstraction
      
     &     JT(feeture) .ne. 10 .and. ! headwater boundary
*    &     JT(feeture) .ne. 11 .and. ! bifurcation
*    &     JT(feeture) .ne. 20 .and. ! bifurcation (retained)
*    &     JT(feeture) .ne. 21 .and. ! bifurcation (diverted)
*    &     JT(feeture) .ne. 22 .and. ! bifurcation (retained)
*    &     JT(feeture) .ne. 23 .and. ! bifurcation (diverted)
     &     JT(feeture) .ne. 44 .and. ! abstraction to lake
     &     JT(feeture) .ne. 45 .and. ! discharge from lake

     &     JT(feeture) .ne. 05 .and. ! industrial discharge
*    &     JT(feeture) .ne. 12 .and. ! intermittents
     &     JT(feeture) .ne. 39 .and. ! mine waters
     &     JT(feeture) .ne. 60 .and. ! other point sources
*    &     JT(feeture) .ne. 62 .and. ! fish farms
     &     JT(feeture) .ne. 03 ) return ! sewage works
      endif ! if ( kdirect .eq. 1 ) then ! the results are for features FFFFFFFF
*     FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      
      
      
      
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     back-tracking of discharges        
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 001
      
*     ==========================================================================

      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 001
*     ==========================================================================


      
*     the start of a reach .........................................SSSSSSSSSSSS
      if ( kdirect .eq. 0 ) then !  for the start of a reach SSSSSSSSSSSSSSSSSSS        
      if ( kountworks .eq. 0 ) then ! no upstream works at at this point SSS 001
          
      if ( JT(feeture) .eq. 10 .or. JT(feature) .eq. 45 .or. ! =================
*    &     JT(feeture) .eq. 20 .or. JT(feature) .eq. 22 .or.
*    &     JT(feeture) .eq. 21 .or. JT(feature) .eq. 23 .or.
     &     JT(feeture) .eq. 11 ) then
      GISreach = GIScode(feeture) 
      endif ! if ( JT(feeture) .eq. 10 etc =====================================
                 
      do kdet = 1, ndet ! SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 001
      if ( QTYPE(kdet) .ne. 4 ) then ! SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 001

      place = 100.0 ! indicator of the part of code in SIMCAT doing this --- 001
      write(160+kdet,4087)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place
 4087 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 
      endif ! if ( QTYPE(kdet) .ne. 4 ) SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 001
      enddo ! do kdet = 1, ndet SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 001
      endif ! if ( kountworks .eq. 0 ) SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 001

      
*     start of a reach with upstream discharges ............................ 002
      if ( kountworks .gt. 0 ) then ! there are discharges at head of reach  002

      if ( JT(feeture) .eq. 10 .or. JT(feature) .eq. 45 .or. ! =================
     &     JT(feeture) .eq. 11 ) then
      GISreach = GIScode(feeture) 
      endif

      iiworks = 0 ! ----- initialise the count the discharges ! ------------ 002
      do kdet = 1, ndet ! SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      if ( QTYPE(kdet) .ne. 4 ) then ! SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
          
      do 2000 iworks = 1, kountworks ! loop on discharges ======================
      jworks = identify werks (iworks) ! feature code for the discharge ~~~~~~~~
*     ignore intermittent discharges (12) --------------------------------------
      if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ------------- 002
      iiworks = iiworks + 1 ! for Qi.BAK ------------
      
      xworksAV(kdet) = 0.0 ! initialise % from works of the total river load --- 
      if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) then ! ======================
      xworksAV(kdet) = 100.0*TELOADAV(iworks,kdet) / ! .... % of river mean load
     &LMcontrib(NTD,kdet,1) ! % of river ! % of river mean load ................
      
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! ....,............
      endif ! if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001) ================== 002

      storedis(iiworks) = xworksAV(kdet) ! ================================= 002
*     ==========================================================================

      do ifx = 1, kountworks35 ! check the total % ------------ Pi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalL = toatalL + storedis(ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03L = toatal03L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 12 ) then ! intermittent discharges (12)
      toatal12L = toatal12L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05L = toatal05L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39L = toatal39L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60L = toatal60L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61L = toatal61L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62L = toatal62L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks35 ! ---------------------- Pi.CSV

      endif ! if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ----- 002
 2000 continue ! do iworks = 1,kountworks ! ================================ 002
      
      place = 200.0 ! indicator of the part of code in SIMCAT doing this --- 002
      write(160+kdet,4287)feeture,ireach,Length of main river, ! ----------- 002
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place
 4287 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 

      endif ! if ( QTYPE(kdet) .ne. 4 ) SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 002
      enddo ! do kdet = 1, ndet SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 002

      endif ! if ( kountworks .gt. 0 ) SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 002
      endif ! if ( kdirect .eq. 0 ) for the start of a reach SSSSSSS 001 and 002
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 001 and 002
*     SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS


      
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 003 and 004
*     at features within a reach ................................... 003 and 004
      if ( kdirect .eq. 1 ) then ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003 and 004
      if ( feeture .ne. 1 ) then ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003 and 004

*     zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 003
      if ( kountworks .eq. 0 ) then ! no upstream works at at this point FFF 003    
      do kdet = 1, ndet ! WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      if ( QTYPE(kdet) .ne. 4 ) then ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003
      jworks = 0 ! TTTTTTT
 
      place = 300.0 ! indicator of part of code in SIMCAT doing this ------- 003
      write(160+kdet,4387)feeture,ireach,Length of main river,
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place
 4387 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 
      
      endif ! if ( QTYPE(kdet) .ne. 4 ) ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003
      enddo ! do kdet = 1, ndet ! FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003
      endif ! if ( kountworks .eq. 0 ) ! - no u/s works at at this point FFF 003
*     zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 003
      
      
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 004
*     loop on the number of works upstream of the feature FFFFFFFFFFFFFFFFFF 004
*     loop on the determinands +++++++++++++++++++ for the works numbered iworks
      if ( kountworks .gt. 0 ) then ! there are upstream works at this point WWW
      do kdet = 1,ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (kdet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++
      iiworks = 0 ! ----- initialise total works for Qi.BAK ---------------- 004
      
      do 3000 iworks = 1, kountworks ! loop on works WWWWWWWWWWWWWWWWWWWWWWW 004
      jworks = identify werks (iworks) ! feature number for the discharge ~~~~~~
      if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ------------- 004
      iiworks = iiworks + 1 ! for Pi.BAK ----------------------------------- 004

*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     store the contributions from the individual discharges ============ Pi.BAK
      if ( feeture .gt. 0 ) then ! +++++++++++++++++++++++++++++++++++++++++++++
      if ( JT(feeture) .ne. 10 ) then ! ++++++++++++++++++++++++++++++++++++ 004
          
      xworksAV (kdet) = 0.0 ! % from a works of the total load in the river ----
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) then ! total of net loads ===    
      xworksAV(kdet) = 100.0*TELOADAV(iworks,kdet) / ! % of river mean load ....
     &LMcontrib(NTD,kdet,1) ! % of river ! % of river mean load ................
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! .................
      endif ! if ( LMcontrib(01,kdet,1) .gt. 0.00000001 ) ================== 004

      storedis(iiworks) = xworksAV(kdet) ! ================================= 004
 
      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 004
*     ==========================================================================

      do ifx = 1, kountworks ! check the totals --------------- Pi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalL = toatalL + storedis(ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03L = toatal03L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 12 ) then ! intermittent discharges (12)
      toatal12L = toatal12L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05L = toatal05L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39L = toatal39L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60L = toatal60L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61L = toatal61L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62L = toatal62L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks ! ------------------------ Pi CSV

      endif ! if ( JT(feeture) .ne. 10 ) +++++++++++++++++++++++++++++++++++ 004
      endif ! if ( feeture .gt. 0 ) ++++++++++++++++++++++++++++++++++++++++ 004
      endif ! if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ----- 004
 3000 continue ! end of loop on the number of works WWWWWWWWWWWWWWWWWWWWWWWWWWWW  

      place = 400.0 ! indicator of part of code in SIMCAT doing this ------- 004
      write(160+kdet,4487)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL, 
     &toatal12C,toatal62C,place ! --------------------------------------- Pi.CSV
 4487 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 
      
      endif ! if ( QTYPE (kdet) .ne. 4 ) +++++++++++++++++++++++++++++++++++ 004
      enddo ! do kdet = 1, ndet ++++++++++++++++++++++++++++++++++++++++++++ 004

      endif ! if ( kountworks .gt. 0 ) WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 004
     
      endif ! if ( feeture .ne. 1 ) then ! FFFFFFFFFFFFFFFFFFFFFFFFF 003 and 004
      endif ! if ( kdirect .eq. 1 ) FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 003 and 004
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW


      
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 005 and 006
*     at the end of of a reach .................................................
      if ( kdirect .eq. 3 ) then !  for the end of a reach EEEEEEEEEEEEEEEEEEEEE
          
*     zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 005               
      if ( kountworks .eq. 0 ) then ! no upstream works at at this point EEE 005
      jworks = 0 ! TTTTTTT
      do kdet = 1, ndet ! WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 005
      if ( QTYPE(kdet) .ne. 4 ) then ! EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if ( feeture .ne. 1 ) then ! ----------------------------------------- 005
 
        
      storedis(kountworks) = xworksAV(kdet) ! ============================== 005

      place = 500.0 ! indicator of part of code in SIMCAT doing this ------- 005
      write(160+kdet,4587)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place
 4587 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 

 7589 format(i3,4i8,3(1pe11.3),2i4,2(1pe11.2),i2, ! ------------------------ 005
     &'   .......... NO UPSTREAM DISCHARGES ',9x,a37,1x,a16,' 005') ! ------ 005
      
      endif ! if ( feeture .ne. 1 )
      endif ! if ( QTYPE(kdet) .ne. 4 ) EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 005
      enddo ! do kdet = 1, ndet EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 005
      endif ! if ( kountworks .eq. 0 )  ! no u/s works at at this point EEEE 005
*     zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 005              
      
      
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 006      
*     the end of of a reach which has upstream discharges .................. 006
      if ( kountworks .gt. 0 ) then ! there are discharges at end of reach - 006
      do kdet = 1, ndet ! WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 006
      if ( QTYPE(kdet) .ne. 4 ) then ! EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
          
      iiworks = 0 ! ----- initialise the count of the works ! -------------- 006
      do 5000 iworks = 1, kountworks ! ===================================== 006
      jworks = identify werks (iworks) ! feature number for the discharge ~~~~~~
*     ignore intermittent discharges (12) and fish farms (62) -------------- 006
      if ( JT(jworks) .ne. 12 .and. JT(jworks) .ne. 62 ) then ! ------------ 006
      iiworks = iiworks + 1 ! for Qi.BAK ----------------------------------- 006

      xworksAV(kdet) = 0.0 ! initialise % from a works of total river load - 006 
      if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) then ! ================== 006
      xworksAV(kdet) = 100.0*TELOADAV(iworks,kdet) / ! % of river mean load ....
     &LMcontrib(NTD,kdet,1) ! % of river ! % of river mean load ................
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! .................
      endif ! if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) ================= 006

      storedis(iiworks) = xworksAV(kdet) ! ================================= 006
*     ==========================================================================
      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 006
*     ==========================================================================
      
      do ifx = 1, kountworks ! check the totals --------------- Pi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalL = toatalL + storedis(ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03L = toatal03L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05L = toatal05L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39L = toatal39L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60L = toatal60L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61L = toatal61L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62L = toatal62L + storedis(ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks ! ---------- ------------- Pi.CSV

      endif ! if ( JT(jworks) .ne. 12 ) exclude storm overflows ------------ 006
 5000 continue ! do iworks = 1,kountworks ================================== 006

      place = 600.0 ! indicator of part of code in SIMCAT doing this ------- 006
      write(160+kdet,4687)feeture,ireach,Length of main river, ! ----------- 006
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place
 4687 format(2(i6,','),1pe11.3,',',a40,',',a40,',',4000(1pe11.3,',')) 

      endif ! if ( QTYPE(kdet) .ne. 4 ) WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW 006
      enddo ! do kdet = 1, ndet  EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 006
      
      endif ! if ( kountworks .gt. 0 ) ===================================== 006

      endif ! if ( kdirect .eq. 3 ) for the end of a reach EEEEEEEEE 005 and 006
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      

      
*     ==========================================================================
*     initialise totals for discharges -----------------------------------------
      do kdet = 1, ndet ! ------------------------------------------------------
      total effluent load (kdet) = 0.0 
      total effluent prop (kdet) = 0.0 
      enddo ! do kdet = 1, ndet ------------------------------------------------      
*     ==========================================================================


*     wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
*     loop on the number of upstream works -------------------------------------
      if ( kountworks .gt. 0 ) then ! there are upstream works at this point ---
      do 1000 iworks = 1, kountworks ! loop on works ---------------------------
      iline = 0
      jworks = identify werks (iworks) ! feature number for the discharge ~~~~~~
      xworksd = 0.0 ! marker that a heading is needed --------------------------


*     158 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 158
      do idet = 1, ndet ! loop on determinands WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      if ( QTYPE(idet) .ne. 4 ) then ! .........................................
      xworksAV (idet) = 0.0 ! initialise percentage contribution to river load -
      xworksTE (idet) = 0.0 ! initialise percentage of effluent load -----------
      xworks (idet) = 0.0
*     initialise the proportion of effluent ------------------------------------
      TenAV(idet)   = '     .....' ! % mean of the total load
      endif ! if ( QTYPE (idet) .ne. 4 ) WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      enddo ! do idet = 1, ndet ! WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     158 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 158


*     loop on the determinands +++++++++++++++++++ for the works numbered iworks
      do 1001 idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (idet) .eq. 4 ) goto 1001 ! +++++++++++++++++++++++++++++++++++
      xworksAV (idet) = 0.0 ! % from a works of the total load in the river ----
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( LMcontrib(NTD,idet,1) .gt. 0.00000001 ) then ! total of net loads +++  
*     ==========================================================================
      xxxnew = TELOADAV(iworks,idet) ! mean load from this discharge ...........
      xworksAV(idet) = 100.0*xxxnew / ! ................... % of river mean load
     &LMcontrib(NTD,idet,1) ! % of river mean load ......................... 158
*     ==========================================================================               

      total effluent prop (idet) = ! total eff % of the total river load -------
     &total effluent prop (idet) + xworksAV(idet) ! ----------------------------
  
      call sort format 2 (xworksAV(idet),LMcontrib(NTD,idet,1))
      TenAV(idet) = valchars10 ! xworksAV(idet)
      
      xworksTE(idet) = 0.0 ! ------ % from this works of the total effluent load  
      xworksTE(idet) = 100.0*TELOADAV(iworks,idet)/
     &                 TELODE2(idet,i13) ! --------------------------------- 158
      
      xworksd = amax1 ( xworksd, xworksTE(idet) ) ! marker heading is needed ---
      total effluent load (idet) = ! total effluent % of the effluent load ----- 
     &total effluent load (idet) + xworksTE(idet) ! ----------------------------

      endif ! if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) ++++++++++++++++++++++++


*     check that the percentage load does not exceed 100 #######################
      if ( xworksTE(idet) .gt. 100.001 .and. 
     &TELOADAV(iworks,idet) .gt. Small ) then ! ################################
      call change colour of text (20) ! bright red ... error message -----------
      write( *,7313)dname(idet),uname(jworks) ! error message ------------------
7313  format(77('-')/ ! error message ------------------------------------------
     &'ERROR in the calculation of loads attributable to ', ! error message ----
     &'effluents for: ',a11/ ! ------------------------- error message ---------
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) ! ----------------------
      call set screen text colour ! error message ------------------------------
      
      write(09,7300)dname(idet),uname(jworks) ! error message -------------- SCN
      write(33,7300)dname(idet),uname(jworks) ! error message -------------- ERR
      write(120+idet,7300)dname(idet),uname(jworks) ! error message ----- Di.ADL
 7300 format(/77('-')/ ! error message -----------------------------------------
     &'ERROR in the calculation of loads attributable to ', ! error message ----
     &'effluents ... ',a11/ ! ------------------------- error message ----------
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) ! ----------------------
      write(09,7000)TELOADAV(iworks,idet),dname(idet) ! error message - file SCN
      write(120+idet,7000)TELOADAV(iworks,idet),dname(idet) ! error ----- Di.ADL
      write(33,7000)TELOADAV(iworks,idet),dname(idet) ! --------------- file ERR
 7000 format('   Load from works =',f15.3,' for ',a11) ! -----------------------
      write(09,7001)LMcontrib(1,idet,1) ! error message --------------- file SCN
      write(120+idet,7001)LMcontrib(1,idet,1) ! load from works --------- Di.ADL
      write(33,7001)LMcontrib(1,idet,1) ! ----------------------------- file ERR
 7001 format('From all effluents =',f15.3) ! error message ------------ file OUT
      write(09,7002)xworksTE(idet) ! error message -------------------- file SCN
      write(120+idet,7002)xworksTE(idet) ! ------------------------------ Di.ADL
      write(33,7002)xworksTE(idet) !  ! ------------------------------- file ERR
 7002 format('        Percentage =',f15.3/77('-')/) ! --------------------------
      endif ! if ( xworksTE (idet) .gt. 100.001 ) ##############################
      
 1001 continue ! end of loop on determinands +++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      if ( xworksd .gt. 0.000001 ) then ! check need to write a heading --------
      iline = 1 ! --------------------------------------------------------------
      if ( ihead .ne. 2 ) ihead = 1 ! need to write heading --------------------
      endif ! if ( xworksd .gt. 0.00001 ) check need to write heading ----------
 

*     write the heading +++++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
      if ( ihead .eq. 1 ) then ! write the heading ++++++++++++++++++++++ Di.ADL
      if ( nobigout .le. 0 ) then ! +++++++++++++++++++++++++++++++++++++ Di.ADL
      do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
      if ( qtype(idet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++ Di.ADL
      if ( kdirect .eq. 9 .or. kdirect .eq. 3 .or. ! -------------------- Di.ADL
     &     kdirect .eq. 0 ) then ! -------------------------------------- Di.ADL
      write(120+idet,4423)unamex,dname(idet),lunits(idet) ! heading ----- Di.ADL
 4423 format(///140('=')/'Approximate apportionment of works at: ', ! --- Di.ADL
     &a37/140('-')/'Source of ',a11, ! ---------------------------------- Di.ADL
     &18x,'% of the load',4x,'load in'/ ! ------------------------------- Di.ADL
     &39x,' in the river',2x,'the river'/57x,'(',a4,')'/140('=')) ! --=-- Di.ADL
      else ! if ( kdirect .eq. 1 ---------------------------------------- Di.ADL
      write(120+idet,2243)unamex,dist(feeture),rname(ireach), ! --------- Di.ADL
     &dname(idet),lunits(idet) ! ---------------------------------------- Di.ADL
 2243 format(///140('=')/'Approximate apportionment of works ', ! ------- Di.ADL
     &'at Feature: ',a37,2x,f10.1,1x, ! --------------------------------- Di.ADL
     &'km from the head of Reach: ',a16/140('-')/ ! --------------------- Di.ADL
     &'Source of ',a11,18x,'% of the load',4x,'load in'/ ! -------------- Di.ADL
     &39x,' in the river',2x,'the river'/57x,'(',a4,')'/140('=')) ! --=-- Di.ADL

      
      endif ! if ( kdirect .eq. 9 .or. kdirect .eq. 3 ) ----------------- Di.ADL 
      endif ! if ( qtype(idet) .-ne. 4 ) then ! +++++++++++++++++++++++++ Di.ADL
      enddo ! do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++ Di.ADL

      ihead = 2 ! heading has been written ++++++++++++++++++++++++++++++ Di.ADL
      endif ! if ( nobigout .le. 0 ) ++++++++++++++++++++++++++++++++++++ Di.ADL
      endif ! if ( ihead .eq. 1 ) +++++++++++++++++++++++++++++++++++++++ Di.ADL

      
      
      if ( iline .eq. 1 ) then ! llllllllllllllllllllllllllllllllllllllll Di.ADL
      iline = 0 ! ------------------------------------------------------- Di.ADL

      xloadxx = 0.0 ! ------------------------------------------------------ 158
      do IS = 1, NS
      xloadxx = xloadxx + FMS(IS)*CMS(5,IS)    
      enddo
      xloadxx = xloadxx / float(NS) ! -------------------------------------- 158

*     write out the portion from individual works nnnnnnnnnnnnnnnnnnnnnnn Di.ADL
      if ( nobigout .le. 0 ) then ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn Di.ADL
          
      do jdet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
      if ( qtype(jdet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++ Di.ADL
      call sort format 1(TELOADAV(iworks,jdet)) ! ----------------------- Di.ADL
*     -------------------------------------------------------------------------- 
      write(120+jdet,3347)uname(jworks),TenAV(jdet),valchars10 ! -------- Di.ADL
 3347 format(a37,3x,a10' %',1x,a10) ! ----------------------------------- Di.ADL
*     -------------------------------------------------------------------------- 
      endif ! if ( qtype(jdet) .ne. 4 ) +++++++++++++++++++++++++++++++++ Di.ADL
      enddo ! do jdet = 1, ndet +++++++++++++++++++++++++++++++++++++++++ Di.ADL
      
      

      
  
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
*     check the number of works ++++++++++++++++++++++++++++++++++++++++++++++++
      if ( iworks .gt. NUED ) then ! #################################### P1-CSV
      write(09,*)' * Number of works exceeds the current maximum', ! #### P1-CSV
     &iworks,NUED ! ##################################################### P1-CSV
      write( *,*)' * Number of works exceeds the current maximum', ! #### P1-CSV
     &iworks,NUED ! ##################################################### P1-CSV
      call pause ! ###################################################### P1-CSV
      endif ! if ( iworks .gt. NUED ) ################################### P1-CSV
      if ( feeture .gt. NU ) then ! ##################################### P1-CSV
      write( *,*)' * Number of Features exceeds the current maximum', ! # P1-CSV
     &feeture,NU ! ###################################################### P1-CSV
      call pause ! ###################################################### P1-CSV  
      endif ! if ( feeture .gt. NU ) #################################### P1-CSV
*     check the number of works ++++++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      



      
      

      
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
      do jdet = 1, ndet ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( QTYPE (jdet) .ne. 4 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     xworksAV is the % river load from this discharge -------------------------    
*     xworksTE is the % of total effluent load from from this discharge -------- 
      write(110+jdet,9899)GIScode(feeture),unamex,rname(IREACH), ! ------ Di.CSV
     &uname(jworks),(xworksAV(jdet),imon=1,12), ! ----------------------- Di.CSV
     &xworksAV(jdet), ! % of total river load --------------------------- Di.CSV
     &jxtype,jt(jworks), ! ---------------------------------------------- Di.CSV
     &xworksTE(jdet) ! % of total effluent load ------------------------- Di.CSV
 9899 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di.CSV
     &'% of total river load from this discharge ', ! ------------------- Di.CSV
     &'(and % of total discharge load):', ! ----------------------------  Di.CSV
     &',',a40,13(',',1pe10.2),',',i4, ! --------------------------------- Di.CSV
     &',',i4,(',',1pe10.2),14(',',1pe10.2)) ! --------------------------- Di.CSV
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
      endif ! if ( QTYPE (jdet) .ne. 4 ) +++++++++++++++++++++++++++++++++++++++
      enddo ! do jdet = 1, ndet ++++++++++++++++++++++++++++++++++++++++++++++++
*     ##########################################################################

      endif ! if ( nobigout .le. 0 ) nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      endif ! if ( iline .eq. 1 ) llllllllllllllllllllllllllllllllllllllllllllll
      
 1000 continue ! end of loop on the number of works WWWWWWWWWWWWWWWWWWWWWWWWWWWW
      endif ! if ( kountworks .gt. 0 ) WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
           

      

*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
*     print out a summary if any lines were printed +++++++++++++++++++++ Di.ADL
      do jdet = 1, ndet ! ----------------------------------------------- Di.ADL
      TenAV(jdet) = '     .....'
      if ( qtype(jdet) .ne. 4 ) then ! ---------------------------------- Di.ADL
      if ( kountworks .gt. 0 ) write(120+jdet,6666) ! ------------------- Di.ADL
 6666 format(140('='))
      if ( total effluent prop (jdet) .gt. 1.0e-20 ) then 
      call sort format 2 (LMcontrib(1,jdet,1),
     &total effluent prop (jdet))
      TenAV(jdet) = valchars10
      endif ! if ( total effluent prop (jdet) .gt. 1.0e-20 )
      endif ! if ( qtype(jdet) .ne. 4 ) --------------------------------- Di.ADL
      enddo ! do jdet = 1, ndet ! --------------------------------------- Di.ADL
*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

   
           
*     99999999999999999999999999999999999999999999999999999999999999999999999999
*     =================================================================== Di.ADL
      do idet = 1, ndet ! =============================================== Di.ADL  
      if ( qtype(idet) .ne. 4 ) then ! ================================== Di.ADL

      if ( kdirect .ne. 1 ) then ! -------------------------------------- Di.ADL
      write(120+idet,5423)unamex,dname(idet),lunits(idet),units(idet) ! - Di.ADL
 5423 format(/140('=')/'Apportionment of types of pollution at: ',a37/ !- Di.ADL
     &140('-')/'Source of ',a11, ! -------------------------------------- Di.ADL
     &18x,'% of the load',4x,'load in',3x, ! ---------------------------- Di.ADL
     &'-- mean concentration',3x,'--- % of annual mean',3x, ! ----------- Di.ADL
     &'number of'/ ! ---------------------------------------------------- Di.ADL
     &39x,' in the river',2x,'the river',3x,'-- added to the river', ! -- Di.ADL
     &3x,'------ concentration',5x,'samples'/ ! ------------------------- Di.ADL
     &57x,'(',a4,')',3x,'--------- (',a4,') ----   ', ! ----------------- Di.ADL
     &'- added to the river',5x,'-------'/140('=')) ! ------------------- Di.ADL
      else ! if ( kdirect .ne. 1 ) -------------------------------------- Di.ADL
      write(120+idet,5243)unamex,dist(feeture),rname(ireach), ! --------- Di.ADL
     &dname(idet),lunits(idet),units(idet) ! ---------------------------- Di.ADL
 5243 format(/140('=')/'Apportionment at Feature: ',a37,15x,f10.1, ! ---- Di.ADL
     &' kilometres from the head of Reach: ',a16/140('-')/ ! ------------ Di.ADL
     &'Source of ',a11,18x,'% of the load',4x,'load in',3x, ! ----------- Di.ADL
     &'-- mean concentration',3x,'--- % of annual mean',3x, ! ----------- Di.ADL
     &'number of'/ ! ---------------------------------------------------- Di.ADL
     &39x,' in the river',2x,'the river',3x,'-- added to the river', ! -- Di.ADL
     &3x,'------ concentration',5x,'samples'/ ! ------------------------- Di.ADL
     &57x,'(',a4,')',3x,'--------- (',a4,') ----   ', ! ----------------- Di.ADL
     &'- added to the river',5x,'-------'/140('=')) ! ------------------- Di.ADL
      endif ! if ( kdirect .eq. 9 .or. kdirect .eq. 3 ) ----------------- Di.ADL 
      
      
      
*     sewage effluents (3) ---------------------------------------------- Di.ADL 
      ip = 2
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
     
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (03) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 2/03
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     sewage effluents (3) ---------------------------------------------- Di.ADL       


      
*     intermittent discharges of sewage (12) ---------------------------- Di.ADL 
      ip = 3
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (12) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 3/12
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     intermittent discharges of sewage (12) ---------------------------- Di.ADL 

      
      
*     industrial discharges (5) ----------------------------------------- Di.ADL 
      ip = 4
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (05) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 4/05
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     industrial discharges (5) ----------------------------------------- Di.ADL 

      
      
*     other point sources (60) ------------------------------------------ Di.ADL 
      ip = 28
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (60) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 28/60
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     other point sources (60) ------------------------------------------ Di.ADL      

      

*     private wastewaters (61) ------------------------------------------ Di.ADL 
      ip = 29
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (61) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 29/61
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e10 ) --------------------
*     private wastewaters (61) ------------------------------------------ Di.ADL
      
      
      
*     fish farms (62) --------------------------------------------------- Di.ADL 
      ip = 30
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (62) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 30/62
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e10 ) --------------------------
*     fish farms (62) --------------------------------------------------- Di.ADL


      
*     mine waters (39) -------------------------------------------------- Di.ADL 
      ip = 5
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (39) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 5/39
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) --------------------
*     mine waters (39) -------------------------------------------------- Di.ADL 
 
      
     
*     total effluent load in the river (1) ============================== Di.ADL
      ip = 1
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(01,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(01,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,01)) ! number of samples (total eff ) +
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 01
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     total effluent load in the river (1) ============================== Di.ADL

      
     
*     boundaries and tributaries (2, 10) -------------------------------- Di.ADL 
      ip = 17
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (02,10) ++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 2/17
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     boundaries and tributaries (17) ----------------------------------- Di.ADL 
 
     
      
*     reach-type diffuse sources (20) ----------------------------------- Di.ADL 
      ip = 20 ! reach-type diffuse sources (20) ------------------------- Di.ADL
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean ++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation ++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (reach diff) +
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 20
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     reach-type diffuse sources (20) ----------------------------------- Di.ADL 

      
      
*     river-based diffuse pollution (13) -------------------------------- Di.ADL 
      ip = 18
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (13) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 18/13
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. 1.0e-10 ) ------------------------------
*     river-based diffuse pollution (13) -------------------------------- Di.ADL 

      
      
*     effluent-based diffuse pollution (15) ----------------------------- Di.ADL 
      ip = 19
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (15) +++++++++     
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 19/15
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     effluent-based diffuse pollution (15) ----------------------------- Di.ADL 
 
      
      
*     aggregated STWs (42) ---------------------------------------------- Di.ADL 
      ip = 14
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (42) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 14/42
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     aggregate STWs (42) ----------------------------------------------- Di.ADL 

      
      
*     livestock (25) ---------------------------------------------------- Di.ADL 
      ip = 6
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (25) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 6/25
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. 1.0e-10 ) ------------------------------
*     livestock (25) ---------------------------------------------------- Di.ADL 

      
      
*     arable (27) ------------------------------------------------------- Di.ADL 
      ip = 7     
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (27) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 7/27
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) ------------------------
*     arable (27) ------------------------------------------------------- Di.ADL 
      

      
*     highway runoff (29) ----------------------------------------------- Di.ADL 
      ip = 8
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (29) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 8/29
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     highway runoff (29) ----------------------------------------------- Di.ADL 

      
      
*     urban runoff (31) ------------------------------------------------- Di.ADL 
      ip = 9
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (31) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++ 9/31
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. -1.0e-10 ) -----------------------------
*     urban runoff (31) ------------------------------------------------- Di.ADL 

      
      
*     atmospheric deposition (33) --------------------------------------- Di.ADL 
      ip = 10
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (33) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 10/33
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. -1.0e-10 ) -----------------------------
*     atmospheric deposition (33) --------------------------------------- Di.ADL 

      
      
*     natural background (35) ------------------------------------------- Di.ADL 
      ip = 11
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (35) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 11/35
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) ------------------------
*     natural background (35) ------------------------------------------- Di.ADL 

      
      
*     septic tanks (37) ------------------------------------------------- Di.ADL 
      ip = 12
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (37) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 12/37
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     septic tanks (37) ------------------------------------------------- Di.ADL 
 
      
      
*     aggregate CSOs (40) ----------------------------------------------- Di.ADL 
      ip = 13
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (40) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 13/40
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     aggregate CSOs (40) ----------------------------------------------- Di.ADL 

      
      
*     diffuse mines (46) ------------------------------------------------ Di.ADL 
      ip = 15
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (46) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 15/46
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     diffuse mines (46) ------------------------------------------------ Di.ADL 

      
      
*     birds, boats and angling (48) ------------------------------------- Di.ADL 
      ip = 16
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (48) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 16/48
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     birds, boats and angling (48) ------------------------------------- Di.ADL 

      
      
*     user-named diffuse pollution (50) --------------------------------- Di.ADL 
      ip = 23
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (50) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 23/50
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     user-named diffuse pollution (50) --------------------------------- Di.ADL 

      
      
*     user-named diffuse pollution (52) --------------------------------- Di.ADL 
      ip = 24
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (52) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 24/52
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     user-named diffuse pollution (52) --------------------------------- Di.ADL 

      
      
*     user-named diffuse pollution (54) --------------------------------- Di.ADL 
      ip = 25
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (54) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 25/54
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. 1.0e-10 ) ------------------------------
*     user-named diffuse pollution (54) --------------------------------- Di.ADL 

      
      
*     user-named diffuse pollution (56) --------------------------------- Di.ADL 
      ip = 26
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (56) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 26/56
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! LMcontrib(ip,idet,1) .gt. 1.0e-10 ) ----------------------- Di.ADL
*     user-named diffuse pollution (56) --------------------------------- Di.ADL 

      
      
*     user-named diffuse pollution (58) --------------------------------- Di.ADL 
      ip = 27
      if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (58) +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++ 27/58
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) ----------------- Di.ADL
*     user-named diffuse pollution (58) --------------------------------- Di.ADL 


      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     loads added by gap filling of river flows [21] -------------------- Di.ADL 
      ip = 21
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
*     use the sampling rate for "boundaries and tributaries (10) and (2)" ++++++
      xqualn = amax1 (0.0001,QUALNB(idet,17)) ! number of samples  (gap filling)
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 21
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) ------------------ Di.ADL
*     loads added by gap filling of river flows [21] -------------------- Di.ADL 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      

    
      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     loads added by gap filling of river quality [22] ------------------ Di.ADL 
      ip = 22
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then ! --------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
*     use the sampling rate for "boundaries and tributaries (10) and (2)" ++++++
      xqualn = amax1 (0.0001,QUALNB(idet,17)) ! number of samples  (gap filling)
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/C(idet,1)
      XUP = 100.0*XU/C(idet,1)
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 22
     &xqualn)
      
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     loads added by gap filling of river quality [22] ------------------ Di.ADL 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      

     

      
*     tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt  
*     total diffuse additions to the river ------------------------------ Di.ADL 
      ip = NTF
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples : total diff +
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = NINT(100.0*XL/C(idet,1))
      XUP = NINT(100.0*XU/C(idet,1))
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++ NTF
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total diffuse additions to the river ------------------------------ Di.ADL 
*     tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt  
 
     
      
*     oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo      
*     overall totals ---------------------------------------------------- Di.ADL 
      ip = NTD
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = C(idet,1) ! set the mean ++++++++++++++++++++++++++++++++++++++++++++
      XS = C(idet,2) ! set the standard deviation ++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALN(idet)) ! number of samples : total ++++++++++
      QUALNB(idet,ip) = xqualn
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = NINT(100.0*XL/C(idet,1))
      XUP = NINT(100.0*XU/C(idet,1))
      call write out the portion (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++ Di.ADL
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     overall totals ---------------------------------------------------- Di.ADL 
*     oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo      


     
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
*     net loads added by natural purification ----------------------------------
      if ( abs (TNLOADUP2(idet,i13)) .gt. 1.0e-10 ) then
      call sort format 2 (abs(propNPU(idet)),abs(TNLOADUP2(idet,i13)))
      write(120+idet,9235)valchars10,valchars11 ! ----------------------- Di.ADL
 9235 format(140('-')/'Added by natural purification',11x,a10,' %',
     &1x,a10/140('-'))
      endif ! if ( abs (TNLOADUP2(idet,i13)) .gt. 1.0e-10 )
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      
      
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr     
*     total load removed ------------------------------------------------ Di.ADL 
      call sort format 2 (abs(prolosses(idet)),abs(TLOSSES2(idet,i13)))
      write(120+idet,2964)valchars10,valchars11 ! ----------------------- Di.ADL
 2964 format(140('=')/'TOTAL LOAD REMOVED ...',18x,a10,' %',1x, ! ------- Di.ADL
     &a10/140('=')) ! --------------------------------------------------- Di.ADL
*     total load removed ------------------------------------------------ Di.ADL 
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr     

      
      
*     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    
*     loads removed by abstractions ------------------------------------- Di.ADL
      if ( abs (TBLODE2(idet,i13)) .gt. 1.0e-10 ) then
      call sort format 2 (abs(propabs(idet)),abs(TBLODE2(idet,i13)))
      write(120+idet,9938)valchars10,valchars11 ! ----------------------- Di.ADL
 9938 format('Removed by abstractions',17X,a10," %",1x,a10) ! ----------- Di.ADL
      endif ! if ( abs (TBLODE2(idet,i13)) .gt. 1.0e-10 )
*     loads removed by abstractions ------------------------------------- Di.ADL
*     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    

 
      
*     **************************************************************************
*     total loads                                                    ... TGLODE1
*     total load introduced upstream boundaries and tributaries      ... TRLODE1
*     total load from discharges (3 12 5 and 39) and (60, 61 and 62) ... TELODE1
*     total load introduced by clean diffuse sources (Reach)        ... TDDLOAD1
*     net loads                                                      ... TGLODE2
*     net load from discharges (3 12 5 and 39) and (60, 61 and 62)   ... TELODE2
*     net load introduced upstream boundaries and tributaries        ... TRLODE2
*     net load introduced by clean diffuse sources                  ... TDDLOAD2
*     load introduced by natural purification                      ... TNLOADUP2
*     load removed by natural purification                         ... TNLOADDN2
*     **************************************************************************
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     net load removed by gap filling for river flows              ... TILOADDN2
*     net load removed by gap filling for river quality            ... TALOADDN2
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     net load from diffuse features (river flow type)              ... T13LOAD2
*     net load from diffuse features (discharge flow type)          ... T15LOAD2
*     net load removed by abstractions                               ... TBLODE2
*     **************************************************************************
 

      
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
*     loads removed by natural purification ------------------------------------
      if ( abs (TNLOADDN2(idet,i13)) .gt. 1.0e-10 ) then
      call sort format 2 (abs(propnpd(idet)),abs(TNLOADDN2(idet,i13)))
      write(120+idet,9238)valchars10,valchars11 ! ----------------------- Di.ADL
 9238 format('Removed by natural purification',9x,a10,' %',1x,a10) ! ---- Di.ADL
      endif ! if ( abs (TNLOADDN2(idet,i13)) ---------------------------- Di.ADL
*     loads removed by natural purification ------------------------------------
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

      
      
*     TILOADUP2 ... net load introduced by gap filling of river flows ...... 158
*     TILOADDN2 ... net load removed by gap filling for river flows ............ 
*     TALOADUP2 ... net load added by gap filling for river quality ............
*     TALOADDN2 ... net load removed by gap filling for river quality ..........
*     propfrl   ... proportions added by gap filling for river flows (mp10) ....
*     propfra   ... proportions removed by gap filling for flows (mp10) ........
*     propqrl   ... proportions added by gap filling for river quality (mp10) ..
*     propqra   ... proportions removed by gap filling of river quality (mp10) .

      
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg river flow      
*     net load removed by gap filling of river flows ---------------------------
      if ( ical .ne. 1 ) then ! ==================================== gap filling 
      if ( abs (TILOADDN2(idet,i13)) .gt. 1.0e-10 ) then ! ---------------------
      call sort format 2 (abs(propfra(idet)),abs(TILOADDN2(idet,i13)))
      write(120+idet,9246)valchars10,valchars11 ! ----------------------- Di.ADL
 9246 format('Removed by gap filling of river flows',3x,a10,' %', ! ----- Di.ADL
     &1x,a10)
      endif ! if ( abs (TILOADDN2(idet,i13)) .gt. 1.0e-10 ) --------------------
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) ================== gap filling
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg river flow      


      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg river quality      
*     net load removed by gap filling of river quality ------------------ Di.ADL
      if ( abs (TALOADDN2(idet,i13)) .gt. 1.0e-10 ) then
      call sort format 2 (abs(propqra(idet)),abs(TALOADDN2(idet,i13)))
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( plode .gt. 0.00001 ) then ! -----------------------------------------
      write(120+idet,9248)valchars10,valchars11 ! ----------------------- Di.ADL
 9248 format('Removed by gap filling of quality  ',5x,a10,' %', ! ------- Di.ADL
     &1x,a10)
      endif ! if ( plode .gt. 0.00001 ) --------------------------------- Di.ADL
      endif ! if ( abs (TALOADDN2(idet,i13)) .gt. 1.0e-10 ) ------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg river quality          


      if ( abs (TLOSSES2(idet,i13)) .gt. 1.0e-10 ) then
      write(120+idet,9138)
 9138 format(140('='))
      endif ! if ( abs (TLOSSES2(idet,i13)) .gt. 1.0e-10 )
      
      endif
      enddo
*     ==========================================================================
*     99999999999999999999999999999999999999999999999999999999999999999999999999
      
      
      
      
*     DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      do jdet = 1, ndet
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( nobigout .le. 0 ) then ! ============================================
      write(110+jdet,7299)GIScode(feeture),unamex,rname(IREACH), ! ------ Di.CSV
     &total effluent prop (jdet), ! total % of river load --------------- Di.CSV
     &(total effluent prop (jdet),imon = 1,12), ! ----------------------- Di.CSV
     &jxtype,total effluent load (jdet) ! total effluent load ----------- Di.CSV
 7299 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Di.CSV
     &'% of total river load from this discharge ', ! ------------------  Di.CSV
     &'(and % of total discharge load):', ! ----------------------------  Di.CSV
     &',','Discharges (3 12 5 39 60 61 62)', ! -------------------------- Di.CSV
     &13(',',1pe12.2)',',i4, ! ------------------------------------------ Di.CSV
     &', 3 12 5 39 60 61',17(',',1pe12.2)) ! ---------------------------- Di.CSV
      endif ! if ( nobigout .le. 0 ) ===========================================
      endif ! if ( QTYPE (idet) .ne. 4 ) =======================================
      enddo ! do idet = 1, ndet ================================================
*     DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      
      return
      end


      
      
      

      
*     calculate the contributions from individual catchments ===================
      subroutine proportion of catchments (kdirect) 
      include 'COMMON DATA.FOR'
      character *10 Tenchars1(MP10),Tenchars2(MP10),Tenchars3(MP10),
     &              Tenchars4(MP10)
      character *16 SET1,SET2
      
      real total effluent prop(MP10),total effluent load(MP10)
      real xbodiesAV(MP10),xbodiesTE(MP10)

      dimension xbodies(NUW,MP10),xwload(MP10)
      real total bodies prop(MP10),total bodies load(MP10), 
     &total bodies length(MP10)
      real brakedown(NUW,ndet,n2prop) 
      real totAV(n2prop)
      integer iheadet(MP10),usebody(MP10),jwrite(MP10)

*     ==========================================================================
*     kdirect is 0 .... at the start of the reach ------------------------------
*     kdirect is 1 .... at a feature -------------------------------------------
*     kdirect is 3 .... at the end of the reach --------------------------------
*     kdirect is 9 .... at the end of the model --------------------------------
*     ==========================================================================

      if ( ical13 .eq. 1 ) return ! exit if running in gap-filling mode --------
      if ( kdirect .eq. 9 ) return ! no output for the end of the model --------
      if ( kdirect .eq. 1 ) then ! for features --------------------------------
      if ( JT(feeture) .gt. 24 .and. JT(feeture) .lt. 40 ) return
      if ( JT(feeture) .gt. 39 .and. JT(feeture) .lt. 60 ) return
      if ( JT(feeture) .gt. 12 .and. JT(feeture) .lt. 17 ) return
      endif ! if ( kdirect .eq. 1 ) --------------------------------------------

      unamex = 'nothing' ! initialse the name of the loaction ------------------
      if ( kdirect .eq. 1 ) then ! at a feature --------------------------------
      unamex = uname(feeture) ! name of the feature ----------------------------
      endif
      if ( kdirect .eq. 9 ) then
      unamex = 'End of the model'
      endif
      if ( kdirect .eq. 0 ) then
      write(unamex,1276)rname(IREACH)
 1276 format('Start of reach - ',a16)
      jxtype = 111 ! start of reach
      endif
      if ( kdirect .eq. 3 ) then
      write(unamex,1277)rname(IREACH)
 1277 format('End of reach - ',a16)
      jxtype = 999 ! end of reach
      endif
*     ==========================================================================

      

*     deal with a bifurcations bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      xfact = 1.0 ! initialise factor for dealing with bifurcations bbbbbbbbbbbb
      if ( bifurcr .gt. 0 ) then ! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
          
*     divertion defined by a set of river flow data ----------------------------                  
      if ( itypbir .eq. 20 ) then ! reach with the retained flow ===============
      if ( jxtype .eq. 111 ) flowhead = FLOW(1) ! mean flow at start of reach --
      xfact = (flowhead-F(bifurcr,1))/flowhead ! ratio of mean flows bbbbbbbbbbb 
      endif ! if ( itypbir .eq. 20 ) -------------------------------------------
      if ( itypbir .eq. 21 ) then ! reach with the diverted flow ---------------    
      xfact = F(bifurcr,1)/flowhead ! ratio for diverted flow bbbbbbbbbbbbbbbbbb
      endif ! if ( itypbir .eq. 21 ) ===========================================    
      
*     divertion defined by a set of discharge data -----------------------------                  
      if ( itypbir .eq. 22) then ! divertion defined with discharge data =======
      if ( jxtype .eq. 111 ) flowhead = FLOW(1) ! mean flow at start of reach --
      xfact = (flowhead-FE(bifurcr,1))/flowhead ! ratio of mean flows bbbbbbbbbb 
      endif ! if ( itypbir .eq. 22 ) -------------------------------------------
      if ( itypbir .eq. 23 ) then ! divertion defined with discharge data ------  
      xfact = FE(bifurcr,1)/flowhead ! ratio for diverted flow bbbbbbbbbbbbbbbbb
      endif ! if ( itypbir .eq. 23 ) ===========================================

      endif ! if ( bifurcr .gt. 0 ) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
*     deal with bifurcations bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

      
*     initialise the totals ----------------------------------------------------
      do kdet = 1, ndet
      total bodies load (kdet) = 0.0
      total bodies prop (kdet) = 0.0
      total bodies length (kdet) = 0.0
      enddo ! do kdet = 1, ndet ! ----------------------------------------------       

      
*     initialise the total load in each sub-catchment (xbodies) ----------------
      do 2507 ibodies = 1, kount bodies ! --------------------------------------
      do 2508 idet = 1, ndet ! -------------------------------------------------
      if ( QTYPE (idet) .ne. 4 ) then ! ----------------------------------------
      xbodies (ibodies,idet) = 0.0 ! the total load in each sub-catchment ------
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) then ! -------------------------
*     loads from the upstream sub-catchments -----------------------------------
      xbodies(ibodies,idet) = TWLOADS(ibodies,idet,i13) !  annual load from body
      total bodies load(idet) = total bodies load(idet) ! total load from bodies 
     & + xbodies(ibodies,idet)
      endif ! if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) ------------------------
      endif ! if ( QTYPE (idet) .ne. 4 ) ---------------------------------------
 2508 continue ! idet = 1, ndet ------------------------------------------------
 2507 continue ! ibodies = 1, kount bodies--------------------------------------
 
      
*     calculate total length of of the water bodies ----------------------------
      do 2000 ibodies = 1, kount bodies ! ++++++++++++++++++++++++++++++++++++++
      do idet = 1, ndet ! ======================================================
      if ( QTYPE (idet) .ne. 4 ) then ! ========================================
      total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
      endif ! if ( QTYPE (idet) .ne. 4 ) =======================================
      enddo ! do idet = 1, ndet ! ==============================================
 2000 continue ! ibodies = 1, kount bodies +++++++++++++++++++++++++++++++++++++

      
*     calculate totals over all sub-catchments (water bodies) ------------------
      catload = 0.0 ! initialise this total ! ----------------------------------
      do idet = 1, ndet ! ------------------------------------------------------
      if ( QTYPE (idet) .ne. 4 ) then ! ----------------------------------------
      catload = catload + xfact * total bodies load (idet) ! -------------------
      endif ! if ( QTYPE (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
*     ==========================================================================


*     ==========================================================================
*     write a heading  ---------------------------------------------- for Di.ACL 
*     and set up the load per kilometre per sub-catchment ----------------------
*     --------------------------------------------------------------------------
      iheadc = 0
      if ( catload .gt. small ) then ! =========================================
      do 2001 ibodies = 1, kount bodies ! number of sub-catchments =============
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do idet = 1, ndet ! ------------------------------------------------------
      Tenchars3(idet) = '      ....'
      if ( qtype (idet) .ne. 4 ) then
      if ( iheadc .eq. 0 ) then ! hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh Di.ACL
          
*     calculate confidence limits for mean concentration +++++++++++++++++++++++
      XM = C(idet,1) ! mean ++++++++++++++++++++++++++++++++++++++++++++++++++++
      XS = C(idet,2) ! standard deviation ++++++++++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALN(idet)) ! number of samples (03) +++++++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean concentration +++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
*     calculate confidence limits for mean load ++++++++++++++++++++++++++++++++
      XM = XLOAD (idet,1,i13)
      XS = XLOAD (idet,2,i13)
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean load ++++++++++++++++++
      XLP = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit ++++++++++++
      XUP = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit ++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
      call sort format 2 (C(idet,1),XLOAD(idet,1,i13))
      call assem(XL,XU,SET1)
      call assem(XLP,XUP,SET2)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
      
      if ( bifurcr .eq. 0 ) then ! -------------------------------------- Di.ACL
      write(220+idet,1910) ! -------------------------------------------- Di.ACL
 1910 format(128('-')) ! ------------------------------------------------ Di.ACL
      else ! ------------------------------------------------------------ Di.ACL
      write(220+idet,1911)unamex ! --------------------------- heading -- Di.ACL
 1911 format(128('=')/A40, ! --------------------------------- heading -- Di.ACL
     &' ... this is a bifurcation ...'/128('-')) ! ---------------------- Di.ACL
      endif ! if ( bifurcr .eq. 0 ) ------------------------------------- Di.ACL
      if ( kdirect .eq. 3 ) then ! ====================================== Di.ACL
      write(220+idet,5912)RLENGTH(ireach),RNAME(IREACH), ! --- heading -- Di.ACL
     &LENGTH OF MAIN RIVER ! -------------------------------------------- Di.ACL
 5912 format('Location:  ',F5.1, ! -------------------------------------- Di.ACL
     &' km downstream from the head of Reach: ',A16/ ! ------------------ Di.ACL
     &7x,F9.1,' km from the start of the river ') ! --------------------- Di.ACL
      else ! if ( kdirect .eq. 3 ) ====================================== Di.ACL
      write(220+idet,1912)DIST(feeture),RNAME(IREACH), ! ----- heading -- Di.ACL
     &LENGTH OF MAIN RIVER ! -------------------------------------------- Di.ACL
 1912 format('Location:  ',F5.1,' km downstream from the head ', ! ------ Di.ACL
     &'of the Reach called ... ',A16/ ! --------------------------------- Di.ACL
     &7x,F9.1,' km from the start of the river ') ! --------------------- Di.ACL
      endif !  if ( kdirect .eq. 3 ) ==================================== Di.ACL
     
      write(220+idet,1062)Flow(1),funit,valchars10,SET1, ! --- heading -- Di.ACL
     &units(idet),valchars11,SET2,lunits(idet) ! ------------------------ Di.ACL
 1062 format(128('-')/ ! ------------------------------------------------ Di.ACL
     &"            Mean flow in the river = ", ! ------------------------ Di.ACL
     &f10.1,16x,'(',a4,')',8x/ ! ---------------------------------------- Di.ACL
     &"   Mean concentration in the river = ", ! ------------------------ Di.ACL
     &a10,a16,'(',a4,')',8x/ ! ------------------------------------------ Di.ACL
     &"            Mean load in the river = ", ! ------------------------ Di.ACL
     &a10,a16,'(',a4,')'/128('=')) ! ------------------------------------ Di.ACL
      
      if ( bifurcr .eq. 0 ) then ! ====================================== Di.ACL
      write(220+idet,1162)lunits(idet) ! --------------------- heading -- Di.ACL
 1162 format( ! --------------------------------------------------------- Di.ACL
     &'Loads provided by individual water bodies ...'/128('-')/! -------- Di.ACL
     &44x,'Contribution from ... ',20x, ! ------------------------------- Di.ACL
     &'LOAD','   plus or',6x,'KG/D',6x,'LOAD',2x,'length'/ ! ------------ Di.ACL
     &86x,a4,'     minus',4x,'per KM',6x,'  % ',3x,' (km) ') ! ---------- Di.ACL
      else ! if ( bifurcr .eq. 0 ) ====================================== Di.ACL
      write(220+idet,1262)lunits(idet) ! --------------------- heading -- Di.ACL
 1262 format( 'Loads provided to this bifurcation ' ! ------------------- Di.ACL
     &'by upstream water bodies ...'/128('-')/ ! ------------------------ Di.ACL
     &44x,'Contribution from ... ',20x, ! ------------------------------- Di.ACL
     &'LOAD','   plus or',6x,'KG/D',6x,'LOAD',2x,'length'/ ! ------------ Di.ACL
     &86x,a4,'     minus',4x,'per KM',6x,'  % ',3x,' (km) ') ! ---------- Di.ACL
      endif ! if ( bifurcr .eq. 0 ) ===================================== Di.ACL
      endif ! if ( iheadc .eq. 0 ) hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh Di.ACL
      
      xwrite = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then
      xwrite = xbodies(ibodies,idet)/TWlength(ibodies) ! load per kilometre ----
      xltot = xltot + xwrite
      endif
      call sort format 1 (xwrite)
      Tenchars3(idet) = valchars10 !  ! load per kilometre ---------------------
      endif
      enddo ! do idet = 1, ndet ------------------------------------------------
      iheadc = 1
 2001 continue ! do 2001 ibodies = 1, kount bodies ! ==================== Di.ACL
*     ==========================================================================
      
      
      
*     ##########################################################################      
*     ##########################################################################      
      if ( kount bodies .gt. 1 ) then ! there is more than one water body ------
      xltot = 0.0
      do 2500 idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (xbodies (kount bodies,idet))
      Tenchars3(idet) = valchars10
      xltot = xltot + xfact * xbodies (kount bodies,idet) ! total load ---------
      endif ! if ( QTYPE (idet) .ne. 4 )
 2500 continue ! idet = 1, ndet
      endif ! if ( kount bodies .gt. 1 ) ---------------------------------------
      
      do idet = 1, ndet ! =============================================== Di.ACL
      if ( qtype (idet) .ne. 4 ) then ! ================================= Di.ACL
      write(220+idet,1215) ! ------------ ===================== --------- Di.ACL
 1215 format(128('='))
      endif ! if ( qtype (idet) .ne. 4 ) ================================ Di.ACL
      enddo ! do idet = 1, ndet ========================================= Di.ACL
*     ##########################################################################      
*     ##########################################################################      

      endif ! if ( catload .gt. small ) ========================================
*     ==========================================================================



*     ==========================================================================
*     prepare to write the load per kilometre over all the sub-catchments ------ 
      xltot = 0.0
      do idet = 1, ndet ! ------------------------------------------------------
      Tenchars2(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      xload per km = 0.0
      if ( total bodies length (idet) .gt. 0.0001 ) then
      xload per km = xfact * total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
      endif
      call sort format 1 (xload per km)
      Tenchars2(idet) = valchars10
      xltot = xltot + xload per km
      endif ! if ( QTYPE (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
*     ==========================================================================

      
*     ==========================================================================
*     prepare to write out the percentages of total loads ----------------------
      do idet = 1, ndet
      iheadet (idet) = 0 ! initialise marker that heading has been printed -----
      total bodies load (idet) = 0.0 
      enddo
      if ( catload .gt. small ) then ! =========================================
      do idet = 1, ndet
      usebody(idet) = 0 ! ----- initialise total number of bodies that have load 
      enddo
          
      do 2504 ibodies = 1, kount bodies ! ++++++++++++++++++++++++++++++++++++++
      jbodies = identify bodies (ibodies)
      xltot = 0.0 ! intitialise total from water bodies ------------------------
      xmult = 0.0 ! prepare to allow for a bifurcation -------------------------
      
      do 2502 idet = 1, ndet ! =================================================
      if ( qtype (idet) .ne. 4 ) then ! --------------------------------- Di.ACL
      xwload (idet) = 0.0
      brakedown (ibodies,idet,NTD) = 0.0
      if (xbodies(ibodies,idet) .gt. 0.0001) then
      usebody(idet) = usebody(idet) + 1 ! count the bodies that have load  
      endif

      xpload = 0.0 ! initialise percent of load from water body --------- Di.ACL
      
      if ( LMcontrib(NTD,idet,1) .gt. 0.000001 ) then ! check annual total -----
*     LMcontrib(NTD,idet,1) is the running total of loads down the model -------

      xmult = xbodies(ibodies,idet)
      if ( bifurcr .gt. 0 ) then
      xmult = xfact * xbodies(ibodies,idet) ! allow for a bifurcation ----------
      endif
            
      call sort format 1 (xmult) ! total load --------------------------- Di.ACL
      Tenchars1(idet) = valchars10 ! xbodies (ibodies,idet) ------------- Di.ACL    
      total bodies load (idet) = total bodies load (idet) + xmult
      xpload = 100.0 * xmult/LMcontrib(NTD,idet,1) ! -------------------- Di.ACL
      call sort format 1 (xmult/TWLENGTH(ibodies)) ! -------------------- Di.ACL
      Tenchars2(idet) = valchars10 ! load per kilometre ----------------- Di.ACL
      xwload (idet) = xwload (idet) + xpload
      brakedown (ibodies,idet,NTD) = xpload 
      call sort format 1 (xpload)
      Tenchars3(idet) = valchars10 ! percentage load (xpload) ----------- Di.ACL
      xltot = xltot + xpload
      endif ! if ( LMcontrib(NTD,idet,1) .gt. 0.000001 ) ---------------- Di.ACL


*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
      if ( xltot .gt. 1.0e-09 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Di.ACL
      call sort format 1 (LMcontrib(NTD,idet,1)) ! mean total load ------ Di.ACL
      Tenchars1(idet) = valchars10 ! total load ------------------------- Di.ACL    
      call sort format 1 (LMcontrib(NTD,idet,2)) ! standard deviation --- Di.ACL
      Tenchars4(idet) = valchars10 ! total load ------------------------- Di.ACL    
      call sort format 1 (LMcontrib(NTD,idet,1)/LENGTH OF MAIN RIVER) ! - Di.ACL
      Tenchars2(idet) = valchars10 ! total load per km ------------------ Di.ACL    
      call sort format 1 (100.0) ! -------------------------------------- Di.ACL
      Tenchars3(idet) = valchars10 ! total load as % -------------------- Di.ACL 
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = LMcontrib(NTD,idet,1) ! set the mean total load +++++++++++++++++++++
      XS = LMcontrib(NTD,idet,2) ! set the standard deviation ++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      if ( xqualn .lt. 0.0002 )xqualn = 0.0
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      if ( iheadet (idet) .eq. 0 ) then ! write the headings ===================
      call sort format 1 (XDL) ! ---------------------------------------- Di.ACL
      write(220+idet,4) ! --- total load at this point in the river ----- Di.ACL
     &Tenchars1(idet),valchars10, ! ---- mean load plus % confidence ---- Di.ACL
     &Tenchars3(idet) ! ----- percentage load --------------------------- Di.ACL
    4 format('Total load at this location',10x, ! ----------------------- Di.ACL
     &7x,'All inputs',26x,2a10,10x,a10,10x,a10,f8.1) ! ------------------ Di.ACL
      iheadet (idet) = 1 ! the heading has been printed ----------------- Di.ACL
      write(260+idet,1004)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),!uname(jbodies),nameprop(ip), ! -------------------- WBi.CSV
     &Length of main river, ! ------------------------------------------ WBi.CSV
     &LMcontrib(NTD,idet,1),XL,XU,xqualn, ! ------ load ---------------- WBi.CSV
     &100.0,0,unamex  ! ---- percentage load --------------------------- WBi.CSV
 1004 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- WBi.CSV
     &'Total load at this location,',!',', ! --------------------------- WBi.CSV
     &'Everything upstream,','Total load', ! --------------------------- WB1.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &4(','),i4,2(','),a40) ! ------------------------------------------ WBi.CSV

     
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (21) = 'Added by gap filling of flows    (no Feature code)'
*     --------------------------------------------------------------------------
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ggggggggggggggggggg gap filling 
      XM = LMcontrib(21,idet,1) ! the mean load from flow gap-filling ++++++++++
      XS = LMcontrib(21,idet,2) ! the standard deviation +++++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (LMcontrib(21,idet,1)) ! gap-filling for flow -- Di.ACL
      Tenchars1(idet) = valchars10 ! total load ------------------------- Di.ACL 
      xxxp = 100.0*LMcontrib(21,idet,1)/LMcontrib(NTD,idet,1) ! --------- Di.ACL
      call sort format 1 (xxxp) !  ------------- gap-filling for flow --- Di.ACL
      Tenchars2(idet) = valchars10 ! total load ------------------------- Di.ACL 
      call sort format 1 (XDL) ! ---------------------------------------- Di.ACL
      write(220+idet,4001)Tenchars1(idet),valchars10,Tenchars2(idet) ! -- Di.ACL
 4001 format(44x,'Added by gap-filling for river flow ', ! -------------- Di.ACL
     &2a10,10x,a10) ! --------------------------------------------------- Di.ACL
      write(260+idet,6693)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &LMcontrib(21,idet,1),XL,XU,QUALNB(idet,NTD),xxxp,unamex ! -------- WBi.CSV
 6693 format(' ',a40,',',i4,',',a40,',',a20,',Flow gap fillng,', ! ----- WBi.CSV
     &'Upstream,','Added by gap-filling for river flow ', ! ------------ WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (22) = 'Added by gap filling of quality  (no Feature code)'
*     --------------------------------------------------------------------------
      XM = LMcontrib(22,idet,1) ! the mean load from quality gap-filling +++++++
      XS = LMcontrib(22,idet,2) ! the standard deviation +++++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (LMcontrib(22,idet,1)) ! gap-filling for quality Di.ACL
      Tenchars1(idet) = valchars10 ! total load ------------------------- Di.ACL
      call sort format 1 (XDL) ! ---------------------------------------- Di.ACL
      xxxp = 100.0*LMcontrib(22,idet,1)/LMcontrib(NTD,idet,1) ! % of load Di.ACL
      call sort format 1 (xxxp) ! gap-filling for flow ------------------ Di.ACL
      Tenchars2(idet) = valchars10 ! % of total total load -------------- Di.ACL
      write(220+idet,4002)Tenchars1(idet),valchars10,Tenchars2(idet) ! -- Di.ACL
 4002 format(44x,'Added by gap-filling for quality    ', ! -------------- Di.ACL
     &2a10,10x,a10) ! --------------------------------------------------- Di.ACL
      write(260+idet,6694)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &LMcontrib(22,idet,1),XL,XU,QUALNB(idet,NTD),xxxp,unamex ! -------- WBi.CSV
 6694 format(' ',a40,',',i4,',',a40,',',a20,',Quality gap fillng,', ! -- WBi.CSV
     &'Upstream,','Added by gap-filling for river quality ', ! --------- WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
     
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ggggggggggg gap filling 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
     
      endif ! if ( iheadet (idet) .eq. 0 ) =====================================
      endif ! if ( xltot .gt. 1.0e-09 ) ~~~~~~~~~~~~~~~~~~~~~~~ Di.ACL & WBi.CSV
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     

      xmult = total bodies load(idet) ! total load from u/s water bodies  Di.ACL
      call sort format 1 (xmult) ! - total load from u/s water bodies --- Di.ACL 
      Tenchars1(idet) = valchars10 ! total load from u/s water bodies --- Di.ACL  
      xpload = 100.0 * xmult/LMcontrib(NTD,idet,1) ! ----------- percentage load
      call sort format 1 (xpload) ! ---------------------------- percentage load
      Tenchars3(idet) = valchars10 ! --------------------------- percentage load
      
      if ( kount bodies .gt. 1 .and. ! -----------------------------------------
     &usebody(idet) .eq. kount bodies ) then 
      call sort format 1 (xmult/total bodies length (idet)) ! ------ load per km
      Tenchars2(idet) = valchars10 ! ------------------------------- load per km
      call sort format 1 (XDL) ! ------ load per km
*     write: total bodies load; load per km; percentage load ------------ Di.ACL
      write(220+idet,5817)Tenchars1(idet),valchars10,Tenchars2(idet), ! --Di.ACL
     &Tenchars3(idet),total bodies length (idet) ! ---------------------- Di.ACL 
 5817 format(128('-')/'Total from water bodies',13x, ! ------------------ Di.ACL 
     &8x,'All inputs ...'22x,2a10,2a10,f8.1) ! -------------------------- Di.ACL
      endif ! if ( kount bodies .gt. 1 etc -------------------------------------
     
      endif ! if ( qtype (idet) .ne. 4 ) =======================================
 2502 continue ! do 2502 idet = 1, ndet ========================================
 2504 continue ! do 2504 ibodies = 1, kount bodies ! +++++++++++++++++++++++++++

      do idet = 1,ndet
      if ( qtype (idet) .ne. 4 ) write(220+idet,1115)
 1115 format(128('-'))
      enddo
      endif ! if ( catload .gt. small ) ========================================
*     ==========================================================================
*     ==========================================================================

    
*     loop on the number of upstream catchments BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      do 1000 ibodies = 1, kount bodies ! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      iline = 0
      jbodies = identify bodies (ibodies)
      xbodiesd = 0.0
      
*     loop on determinands for this catchment ..................................
      do 1001 idet = 1, ndet ! .................................................
      if ( QTYPE (idet) .eq. 4 ) goto 1001 ! ...................................
      xbodiesAV (idet) = 0.0 ! % of the total load
      
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-15 ) then
      xbodiesAV(idet) = 100.0*TDLOADAV(ibodies,idet)/
     &LMcontrib(NTD,idet,1)
      total effluent prop (idet) = total effluent prop (idet) 
     &+ xbodiesAV(idet)
      call sort format 2 (xbodiesAV(idet),LMcontrib(NTD,idet,1))
      endif ! if ( LMcontrib(1,idet,1) .gt. 1.0e-15 )
      
      xbodiesTE (idet) = 0.0 ! % of the total effluent load
      if ( LMcontrib(1,idet,1) .gt. 1.0e-15 ) then
      xbodiesTE(idet) = 100.0*TDLOADAV(ibodies,idet)/LMcontrib(1,idet,1)
      total effluent load (idet) = total effluent load (idet) +
     &xbodiesTE(idet)
      xbodiesd = amax1 ( xbodiesd, xbodiesTE(idet) )
      endif ! if ( LMcontrib(1,idet,1) .gt. 1.0e-15 )

*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX      
*     check that the percentage load does not exceed 100 -----------------------
      if ( xbodiesTE (idet) .gt. 100.0001 ) then
      call change colour of text (12) ! orange - error message
      write( *,7300)uname(jbodies)
      call set screen text colour
      write(01,7300)uname(jbodies) ! error message ------------------------- OUT
      write(09,7300)uname(jbodies) ! error message ------------------------- SCN
      write(33,7300)uname(jbodies) ! error message ------------------------- ERR

      write(220+idet,7300)uname(jbodies) ! error message ---------------- Di.ACL
 7300 format(/77('-')/
     &'Error in the calculation of loads attributable to ',
     &'effluents ... '/
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) 
      write(01,7000)TELOADAV(ibodies,idet),dname(idet) ! error message ----- OUT
      write(09,7000)TELOADAV(ibodies,idet),dname(idet) ! error message ----- SCN
      write(220+idet,7000)TELOADAV(ibodies,idet),dname(idet) ! x error xx Di.ACL
      write(33,7000)TELOADAV(ibodies,idet),dname(idet) ! error message ----- ERR
 7000 format('   Load from works =',f15.6,' for ',a11) ! -----------------------
      write(01,7001)LMcontrib(1,idet,1) ! error message -------------------- OUT
      write(09,7001)LMcontrib(1,idet,1) ! error message -------------------- SCN
      write(220+idet,7001)LMcontrib(1,idet,1) ! error message ----------- Di.ACL
      write(33,7001)LMcontrib(1,idet,1) ! error message -------------------- ERR
 7001 format('    Total net load =',f15.6) ! -----------------------------------
      write(01,7002)xbodiesTE(idet) ! error message ------------------------ OUT
      write(09,7002)xbodiesTE(idet) ! error message ------------------------ SCN
      write(220+idet,7002)xbodiesTE(idet) ! error message xxxxxxxxxxxxxxx Di.ACL
      write(33,7002)xbodiesTE(idet) ! error message ------------------------ ERR
 7002 format('        Percentage =',f15.6/77('-'))
      call stop
      endif ! if ( xbodiesTE (idet) .gt. 100.0001 )
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  
      
 1001 continue ! end of loop on determinands for this catchment ................-

      if ( xbodiesd .gt. 0.00001 ) then
      iline = 1
      if ( iheadc .ne. 2 ) iheadc = 1
      endif ! if ( xbodiesd .gt. 0.00001 )

      if ( iline .eq. 1 ) then
      iline = 0 
      do idet = 1, ndet ! ======================================================
      if ( QTYPE (idet) .ne. 4 ) then ! ========================================
*     write(110+idet,8099)GIScode(feeture),unamex,uname(jbodies), ! ----- Di.CSV
*    &dname(idet),xbodiesAV(idet),xbodiesTE(idet),jxtype ! -------------- Di.CSV
*8099 format(' ',a40,',',a40,',',a40,',',
*    &'% Annual contribution from individual works'',',a11,2(',', ! ----- Di.CSV
*    &1pe11.4),',,,,,,,,,,,,',i4,', 3 12 5 39 60 61') ! ----------------- Di.CSV
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo ! idet = 1, ndet ! =================================================
      endif ! if ( iline .eq. 1 ) ! ============================================

 1000 continue ! end of loop on the number of upstream catchments BBBBBBBBBBBBBB
*     BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

 
    
      
*     ==========================================================================
*     ==========================================================================
*     repeat for the breakdown of the contributions to the sub-catchments ------  
*     --------------------------------------------------------------------------
*     list of contributions (and Feature codes) --------------------------------
*     ==========================================================================
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
*     (17) = 'Boundaries and tributaries(2 and 10)'
*     (18) = 'Diffuse input (13)'
*     (19) = 'Diffuse input (15)'
*     (20) = 'Reach diffuse (no Feature code)'
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (21) = 'Gap filling of flows (no Feature code)'
*     (22) = 'Gap filling of quality (no Feature code)'
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (23) = 'User-named diffuse input (50)'
*     (24) = 'User-named diffuse input (52)'
*     (25) = 'User-named diffuse input (54)'
*     (26) = 'User-named diffuse input (56)'
*     (27) = 'User-named diffuse input (58)'
*     (28) = 'Other point sources (60)'
*     (29) = 'Private wastewaters (61)'
*     (30) = 'Fish farms (62)'
*     (31) = 'Not used'
*     (32) = 'Grand total (NTD = 32) ... (everything)' 
*     (33) = 'Total from diffuse pollutions ... NTF (several Feature codes) --
*     --------------------------------------------------------------------------
      

*     ==========================================================================
*     loop through each of the types of input ----------------------------------
*     ==========================================================================
      do 6000 iip = 1, n2prop ! ++++++++ number of types tracked for inputs ++++
      ip = seqappbodies (iip) ! set the sequence of inputs +++++++++++++++++++++
      if ( ip .eq. 0 ) goto 6000
      do idet = 1, ndet ! ------------------------------------------------------
      if ( QTYPE(idet) .ne. 4 ) then ! -----------------------------------------
      total bodies load (idet) = 0.0 ! initialise the total load
      total bodies prop (idet) = 0.0 ! initialise total load proportion
      total bodies length (idet) = 0.0 ! initialise length of bodies 
      usebody(idet) = 0 ! ----- initialise total number of bodies that have load 
      jwrite(idet) = 0
      xbodiesAV (idet) = 0.0 ! % of the total load ! 666666666666666666666666666
      endif ! if ( QTYPE(idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet 0 ----------------------------------------------
      do 4800 ibodies = 1, kount bodies ! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      do 4508 idet = 1, ndet ! ddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( QTYPE (idet) .ne. 4 ) then ! dddddddddddddddddddddddddddddddddddddddd
      if ( TWLOADSAPP(ibodies,idet,i13,ip) .gt. 1.0e-8 ) then ! ----------------
      usebody(idet) = usebody(idet) + 1 ! count the bodies that have load ------
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-15 ) then ! check total load --------
      xbodiesAV(idet) = 100.0 * TWLOADSAPP(ibodies,idet,i13,ip)/
     &LMcontrib(NTD,idet,1)
      total bodies prop (idet) = total bodies prop (idet) 
     &+ xbodiesAV(idet)
      if ( jwrite(idet) .eq. 0 ) jwrite(idet) = 1
      endif ! if ( LMcontrib(NTD,idet,1) .gt. 1.0e-15 ) ------------------------
      endif ! if ( TWLOADSAPP(ibodies,idet,i13,ip) .gt. 1.0e-8 ) ---------------

      xbodiesTE (idet) = 0.0 ! % of the total effluent load ====================
      if ( LMcontrib(1,idet,1) .gt. 1.0e-15 ) then ! ===========================
      xbodiesTE(idet) = 100.0 * TWLOADSAPP(ibodies,idet,i13,ip)/
     &LMcontrib(1,idet,1)
      xbodiesd = amax1 ( xbodiesd, xbodiesTE(idet) )
      endif ! if ( LMcontrib(1,idet,1) .gt. 1.0e-15 ) ! ========================
      
      brakedown (ibodies, idet, ip) = 0.0 !  
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-15 ) then ! total of net loads ------
      xxxxxxb = xbodies (ibodies,idet)
      xbodies (ibodies,idet) = TWLOADSapp (ibodies,idet,i13,ip)
      
      total bodies load (idet) = total bodies load (idet) ! - add this body load
     & + xbodies (ibodies,idet)
      
      endif ! if ( LMcontrib(NTD,idet,1) .gt. 1.0e-15 )
      endif ! if ( QTYPE (idet) .ne. 4 ) ddddddddddddddddddddddddddddddddddddddd
 4508 continue ! idet = 1, ndet dddddddddddddddddddddddddddddddddddddddddddddddd
      
      
      
*     write the data for each sub-catchment for this type of input load --------
*     write to the CSV file that is picked up by SAGIS -------------------------
      jbodies = identify bodies (ibodies)
      do 4680 idet = 1, ndet ! ddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( QTYPE (idet) .ne. 4 ) then ! dddddddddddddddddddddddddddddddddddddddd
      xload per km = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then
      xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
      endif
      xper = 0.0
      if ( LMcontrib(NTD,idet,1) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / LMcontrib(NTD,idet,1)
      xper = amax1 ( 0.0, xper )
      endif

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = xbodies (ibodies,idet) ! set the mean load ++++++++++++++++++++++++++
*     set coefficient of variation to the value for the total for this input ===
      xcov = LMcontrib(ip,idet,2)/LMcontrib(ip,idet,1)
      XS = xcov * xbodies (ibodies,idet) ! set the standard deviation ++++++++++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format1 (XDL)
      tenchars1(idet) = valchars10
      if ( xqualn .lt. 0.0002 )xqualn = 0.0
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     

*     write the data for each sub-catchment for this type of input load XXXXXXXX
      if ( xbodies (ibodies,idet) .gt. 0.0000001 ) then ! XXXXXXXXXXXXXXXXXXXXXX
*     =================================================================== Di.ACL
      call sort format 3 (xfact*xbodies(ibodies,idet), ! ---------------- Di.ACL
     &xfact*xload per km,xfact*xper) ! ---------------------------------- Di.ACL
      write(220+idet,8693)uname(jbodies),nameprop(ip),valchars10, ! ----- Di.ACL
     &tenchars1(idet),valchars11,valchars12,TWlength(ibodies) ! --------- Di.ACL
 8693 format(a37,7x,a36,2a10,2a10,f8.1) ! ------------------------------- Di.ACL
      endif ! if ( xbodies (ibodies,idet) .gt. 0.0000001 ) XXXXXXXXXXXXXXXXXXXXX
      write(260+idet,7693)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),uname(jbodies),nameprop(ip), ! --------------------- WBi.CSV
     &Length of main river, ! ------------------------------------------ WBi.CSV
     &xfact*xbodies(ibodies,idet),XLP,XUP,xqualn,xfact*xper, ! --------- WBi.CSV
     &TWlength(ibodies),xfact*xload per km, ! -------------------------- WBi.CSV
     &numprop(ip),unamex ! --------------------------------------------- WBi.CSV
 7693 format(' ',a40,',',i4,',',a40,',',a20,',',a40,',', ! ------------- WBi.CSV
     &'From this water body:',',',a37,(',',0pf12.3), ! ----------------- WBi.CSV
     &4(',',1pe12.3),(',',0pf11.3),'%', ! ------------------------------ WBi.CSV
     &(',',0pf12.3),(',',1pe12.3),2(','),i4,2(','),a40) ! -------------- WBi.CSV
*     ======================================================= Di.ACL and WBi.CSV
      
*     =================================================================== Di.CSV
*     write(110+idet,8643)GIScode(feeture),unamex,uname(jbodies), ! ----- Di.CSV
*    &nameprop(ip),xfact*xbodies(ibodies,idet),jxtype,! ----------------- Di.CSV
*    &xfact*xper,TWlength(ibodies),xfact*xload per km ! ----------------- Di.CSV
 8643 format(' ',a40,',',a40,',',a40,',', ! ----------------------------- Di.CSV
     &'Annual mean load from this water body from:',',',a37, ! ---------- Di.CSV
     &',,,,,,,,,,,,',(',',1pe11.4),',',i4,',', ! ------------------------ Di.CSV
     &(',',0pe11.2),',,,,,,,,,,,',6(',',1pe11.4)) ! --------------------- Di.CSV
*     =================================================================== Di.CSV
      !endif ! if ( xbodies (ibodies,idet) .gt. 0.0000001 ) XXXXXXXXXXXXXXXXXXXX


      endif ! if ( QTYPE (idet) .ne. 4 ) ddddddddddddddddddddddddddddddddddddddd
 4680 continue ! do 4600 idet = 1, ndet dddddddddddddddddddddddddddddddddddddddd


*     ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
*     write the total load for each sub-catchment for this type of input -------
*     --------------------------------------------------------------------------    
     
      do 6880 idet = 1, ndet ! ddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( QTYPE (idet) .ne. 4 ) then ! dddddddddddddddddddddddddddddddddddddddd
      total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
      endif ! if ( QTYPE (idet) .ne. 4 ) ddddddddddddddddddddddddddddddddddddddd
 6880 continue ! idet = 1, ndet dddddddddddddddddddddddddddddddddddddddddddddddd
      
*     write the breakdown over all sub-catchments ------------------------------
      do 3401 idet = 1, ndet ! ddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( QTYPE (idet) .ne. 4 ) then ! dddddddddddddddddddddddddddddddddddddddd
      Tenchars1(idet) = '      ----'
      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      Tenchars2(idet) = '      ----'
      
*     HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
      jbodies = identify bodies (ibodies)
      xload per km = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then ! calkculate load per km -------
      xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
      endif
      xper = 0.0
      if ( LMcontrib(NTD,idet,1) .gt. 0.0001 ) then !  calculate % of total load
      xper = 100.0 * xbodies (ibodies,idet) / LMcontrib(NTD,idet,1)
      xper = amax1 ( 0.0, xper )
      endif
      call sort format 2 (xload per km, xper)
*     HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

      
*     ==========================================================================
*     write the total load per km for breakdowns over all sub-catchments -------
*     ==========================================================================
      xmult = 0.0
      if ( total bodies load(idet) .gt. Small ) then ! ------------------ Di.ACL
      xmult = total bodies load(idet) ! --------------------------------- Di.ACL
      if ( bifurcr .gt. 0 ) xmult = xfact * total bodies load(idet) ! --- Di.ACL
      xload per km = 0.0 ! ---------------------------------------------- Di.ACL
      if ( total bodies length(idet) .gt. Small ) then ! ---------------- Di.ACL
      xload per km = xmult/total bodies length(idet) ! ------------------ Di.ACL
      endif ! if ( total bodies length(idet) .gt. Small ) --------------- Di.ACL
      xper = 100.0 * xmult/LMcontrib(NTD,idet,1) ! ---------------------- Di.ACL
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = total bodies load(idet) ! set the mean load +++++++++++++++++++++++++
*     set coefficient of variation to the value for the total for this input ===
      xcov = LMcontrib(ip,idet,2)/LMcontrib(ip,idet,1) ! =======================
      XS = xcov * total bodies load(idet) ! calculate the standard deviation +++
      xqualn = amax1 (0.0001,QUALNB(idet,ip)) ! number of samples (total) ++++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( kount bodies .eq. ibodies ) then ! --------------------------- Di.ACL
      if ( usebody(idet) .gt. 1 ) then ! -------------------------------- Di.ACL
      call sort format 1 (XDL) ! ---------------------------------------- Di.ACL
      Tenchars1(idet) = valchars10 ! ------------------------------------ Di.ACL
      call sort format 3 (xmult,xload per km,xper) ! -------------------- Di.ACL
      write(220+idet,5814)nameprop(ip),valchars10,Tenchars1(idet), ! ---- Di.ACL
     &valchars11,valchars12,total bodies length (idet) ! ---------------- Di.ACL 
 5814 format(128('-')/'Total from above water bodies for .........', ! -- Di.ACL 
     &1x,a36,4a10,f8.1/128('=')) ! -------------------------------------- Di.ACL 
      else ! if ( usebody(idet) ----------------------------------------- Di.ACL 
      write(220+idet,5835) ! -------------------------------------------- Di.ACL 
 5835 format(128('=')) ! ------------------------------------------------ Di.ACL 
      endif ! if ( usebody(idet) .gt. 1 ) ------------------------------- Di.ACL
      endif ! if ( kount bodies .eq. ibodies ) -------------------------- Di.ACL
      endif ! if ( total bodies load(idet) .gt. Small ) ----------------- Di.ACL
*     ==========================================================================

      endif ! if ( QTYPE (idet) .ne. 4 ) then ! dddddddddddddddddddddddddddddddd
 3401 continue ! do 3401 idet = 1, ndet ! dddddddddddddddddddddddddddddddddddddd
 4800 continue ! ibodies = 1, kount bodies bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb  
      

*     ==========================================================================
 6000 continue ! ip = 1, n2prop ++++ number of types tracked for inputs ++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT      
      do idet = 1, ndet ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      if ( QTYPE (idet) .ne. 4 ) then ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      ihedd = 1
      
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      nnend = 0
      do ibodies = 1, kount bodies ! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX bodies
      rowtotal = 0.0 ! 
      jbodies = identify bodies (ibodies)
      do ip = 2, 16
      rowtotal = rowtotal + brakedown(ibodies,idet,ip) ! 
      enddo
      xtwo = xtwo + rowtotal  ! 
      if ( xtwo .gt. 1.0e-9 ) then !  ! ----------------------------------------
      if ( catload .gt. 0.0000001 .and. nnend .eq. 0 ) then
      nnend = 1
      endif
      if ( ihedd .eq. 1 ) then ! ###############################################
      ihedd = 0
      endif ! if ( ihedd .eq. 1 ) then #########################################
      endif ! if ( xtwo .gt. 1.0e-9 ) ------------------------------------------     
      enddo ! do ibodies = 1, kount bodies XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX bodies
*     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( LMcontrib(NTD,idet,1) .gt. 0.000001 ) then ! ############ gap filling
      TGX = LMcontrib(NTD,idet,1) ! ############################################
      pgf1 = 100.0 * LMcontrib(21,idet,1)/TGX
      pgf2 = 100.0 * TILOADDN2(idet,i13)/TGX
      pgf3 = 100.0 * LMcontrib(22,idet,1)/TGX
      pgf4 = 100.0 * TALOADDN2(idet,i13)/TGX
      pgf5 = abs (pgf1) + abs (pgf2) + abs (pgf3) + abs (pgf4)
      igf = 1
      if ( pgf5 .lt. 1.0e-09) igf = 0
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! =================== gap filling 
      write(110+idet,2779)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),LMcontrib(21,idet,1),jxtype,pgf1  ! ------------------- Di.CSV
 2779 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load added by gap filling for river flows',',',a11, ! ---------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4,1(',,',1pe11.4)) ! -------------- Di.CSV
      write(110+idet,2729)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),TILOADDN2(idet,i13),jxtype,pgf2  ! -------------------- Di.CSV
 2729 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load removed by gap filling for river flows',',',a11, ! -------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4,1(',,',1pe11.4)) ! -------------- Di.CSV
      write(110+idet,2739)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),LMcontrib(22,idet,1),jxtype,pgf3  ! ------------------- Di.CSV
 2739 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load added by gap filling on river quality',',',a11, ! --------- Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4,1(',,',1pe11.4)) ! -------------- Di.CSV
      write(110+idet,2749)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(idet),TALOADDN2(idet,i13),jxtype,pgf4  ! -------------------- Di.CSV
 2749 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'% Load removed by gap filling for river quality',',',a11, ! ------ Di.CSV
     &1(',,,,,,,,,,,,,',1pe11.4),',',i4,1(',,',1pe11.4)) ! -------------- Di.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) then ! =========== gap filling
      endif ! if ( LMcontrib(NTD,idet,1) .gt. 0.000001 ) ########### gap filling
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      endif ! if ( QTYPE (idet) .ne. 4 ) TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      enddo ! do idet = 1, ndet ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
*     TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT      
      
      return
      end

     


      subroutine write out the portion (idet,ip,plode,pconk,XL,XU,
     &XLP,XUP,xqualn)
      include 'COMMON DATA.FOR'
      character *16 SET1
      character *11 SET2
      character *06 SET3
      
      plode = 100.0*LMcontrib(ip,idet,1)/LMcontrib(NTD,idet,1)
      pconk = 100.0*CMcontrib(ip,idet,1)/C(idet,1)
      call sort format 3 (plode,LMcontrib(ip,idet,1),
     &CMcontrib(ip,idet,1))

      call assem(XL,XU,SET1)
      call assem2(XLP,XUP,SET2)
      call assem3(pconk,SET3)
                  
      if ( plode .gt. 0.00001 ) then ! -----------------------------------------
      write(120+idet,7777)nameprop(ip),valchars10,valchars11, ! --------- Di.ADL
     &valchars12,SET1,SET3,SET2,xqualn
 7777 format(a37,3x,a10,' %',
     &1x,a10,a10,1x,a16,a6,' ',a11,' %',f12.2)
      endif

      return
      end
      subroutine confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      
      XL = 0.0
      XU = 0.0
      KDL = 0
      XDL = 0.0
      XLP = 0.0
      XUP = 0.0
      if ( abs(XM) .gt. 0.000001 ) then ! ===========================================
      XM = abs(XM)
      XS = abs(XS)
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit +++++++++++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit +++++++++++++
      XLP = 100.0*XL/XM
      XUP = 100.0*XU/XM
      KDL = int(100.0*(XM - XL)/XM) ! lower confidence limit as a percentage +++
      XDL = abs (XM - XL) ! differnce between mean and condidence limit ++++++++
      endif ! if ( XM .gt. 0.000001 ) ==========================================
      
      return
      end