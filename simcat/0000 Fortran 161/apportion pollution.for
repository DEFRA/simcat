*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ....
*     ==========================================================================
*     File: apportion pollution.for ... 3679 lines -----------------------------
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
*     ...... write out the csv (to the files DAi.CSV) ! -------------------- 161
*     ...... apportion the river percentiles ! ----------------------------- 161
*     ...... rank2 (rank values into ascending order of magnitude) ! ------- 161
*     ...... use shots to get mean load for works ! ------------------------ 161
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
     &,Ten00,Ten50,Ten90,Ten95,Ten98,Ten99,Ten995,Ten999 ! ----------------- 161
      character *40 GISreach
*     ==========================================================================

      real storedis(2,NUED)

      real total effluent prop (ndet), total effluent load (ndet)
      real xworksAV(ndet),xworksTE(ndet)
      real wconcd(ndet),pconcd(ndet),wloadd(ndet) ! ------------------------ 161
      character *16 SET1
      character *11 SET2
      character *06 SET3
      dimension Y(NS)

      dimension xxl(7,9),xmnl(7),IVAL(NS) ! :::::::::::::::::::::::::::::::: 161

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
      pconcd(idet) = 0.0 ! -- % from a works of the total conc in the river 
      endif ! if ( QTYPE (idet) .ne. 4 ) ++++++++++++++++++++++++++++++++++
      enddo ! do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++
      
      do i1 = 1,2
      do iw = 1,NUED
      storedis(i1,iw) = 0.0
      enddo
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
      toatal03C = 0.0 ! initialise total conc from sewage effluents (03)
      toatal12C = 0.0 ! initialise total conc from intermittent discharges (12)
      toatal05C = 0.0 ! industrial total conc from industrial (05)
      toatal39C = 0.0 ! initialise mine discharges (39)
      toatal60C = 0.0 ! initialise other discharges (60)
      toatal61C = 0.0 ! initialise other discharges (61)
      toatal62C = 0.0 ! initialise fish farms (62)
      toatalC = 0.0   ! initialise total concentration from all discharges - 002
      
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
      write(190+kdet,4087)feeture,ireach, ! ----------------------------- Qi.CSV
     &Length of main river,unamex,GISreach,C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place
      write(160+kdet,4087)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
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
      do IS = 1,NS ! loop on the shots -----------------------------------------
      xxxnew = xxxnew + TELODEshots(iworks,kdet,IS) ! sum the load shots .......
      enddo ! do IS = 1,NS -----------------------------------------------------
      xxxnew = xxxnew / float(NS) ! mean load from this discharge ..............
      xworksAV(kdet) = 100.0*xxxnew / ! ................... % of river mean load
     &LMcontrib(NTD,kdet,1) ! % of river mean load .............................
      
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! ....,............
      endif ! if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001) ================== 002
      
      pconcd(kdet) = 0.0 ! % of total mean concentration from this works --- 002
      wconcm = 0.0 ! initialise the mean concentration from this works ----- 002
      wloadm = 0.0 ! initialise the mean load from this works -------------- 002    
      
      do IS = 1, NS ! ----------------------------------------------------------
      if ( FMS(IS) .gt. 0.000001 ) then
      xxcf = TELODEshots(iworks,kdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      endif ! if ( FMS(IS) .gt. 0.000001 ) -------------------------------------
      wloadm = wloadm + TELODEshots(iworks,kdet,IS) 
      enddo ! do IS = 1, NS ----------------------------------------------------
      wconcm = wconcm / NS ! mean concentration from this works ----------------
      
      if ( C(kdet,1) .gt. 0.000001 ) then
      pconcd(kdet) = 100.0*wconcm/C(kdet,1) ! -- % concentration from  works 002
      endif

      storedis(1,iiworks) = pconcd(kdet)
      storedis(2,iiworks) = xworksAV(kdet) ! =============================== 002

*     ==========================================================================
      toatal03C = 0.0 ! initialise total conc from sewage effluents (03)
      toatal12C = 0.0 ! initialise total conc from intermittent discharges (12)
      toatal05C = 0.0 ! industrial total conc from industrial (05)
      toatal39C = 0.0 ! initialise mine discharges (39)
      toatal60C = 0.0 ! initialise other discharges (60)
      toatal61C = 0.0 ! initialise other discharges (61)
      toatal62C = 0.0 ! initialise fish farms (62)
      toatalC = 0.0   ! initialise total concentration from all discharges 
      
      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 002
*     ==========================================================================

      do ifx = 1, kountworks35 ! check the total % ---------- P & Qi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalC = toatalC + storedis(1,ifx) ! all discharges ------ Qi.CSV
      toatalL = toatalL + storedis(2,ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03C = toatal03C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal03L = toatal03L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 12 ) then ! intermittent discharges (12)
      toatal12C = toatal12C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal12L = toatal12L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05C = toatal05C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal05L = toatal05L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39C = toatal39C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal39L = toatal39L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60C = toatal60C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal60L = toatal60L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61C = toatal61C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal61L = toatal61L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62C = toatal62C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal62L = toatal62L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks35 ! -------------------- P & Qi.CSV

      endif ! if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ----- 002
 2000 continue ! do iworks = 1,kountworks ! ================================ 002

      place = 200.0 ! indicator of the part of code in SIMCAT doing this --- 002
      write(190+kdet,4287)feeture,ireach,Length of main river, ! ----------- 002
     &unamex,GIScode(feeture),C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place
      write(160+kdet,4287)feeture,ireach,Length of main river, ! ----------- 002
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
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
      pconcd(kdet) = 0.0 ! ----- % from works of the total conc in the river 003
      jworks = 0 ! TTTTTTT

      storedis(1,kountworks) = pconcd(kdet)
      storedis(2,kountworks) = xworksAV(kdet)

      place = 300.0 ! indicator of part of code in SIMCAT doing this ------- 003
      write(190+kdet,4387)feeture,ireach,Length of main river, ! ----------- 003
     &unamex,GIScode(feeture),C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place
      write(160+kdet,4387)feeture,ireach,Length of main river,
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
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
      iiworks = iiworks + 1 ! for P & Qi.BAK ------------------------------- 004

*     WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
*     store the contributions from the individual discharges ======= Pi & Qi.BAK
      if ( feeture .gt. 0 ) then ! +++++++++++++++++++++++++++++++++++++++++++++
      if ( JT(feeture) .ne. 10 ) then ! ++++++++++++++++++++++++++++++++++++ 004
          
      xworksAV (kdet) = 0.0 ! % from a works of the total load in the river ----
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) then ! total of net loads === 
          
      xxxnew = 0.0 ! initialise mean load from this works ......................
      do IS = 1, NS ! loop of shots --------------------------------------------
      xxxnew = xxxnew + TELODEshots(iworks,kdet,IS) ! sum the load shots .......
      enddo ! do IS = 1, NS ----------------------------------------------------
      xxxnew = xxxnew / float(NS) ! mean load from this discharge ..............
      xworksAV(kdet) = 100.0*xxxnew / ! % of river mean load ...................
     &LMcontrib(NTD,kdet,1) ! % of river ! % of river mean load ................
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! .................
      endif ! if ( LMcontrib(01,kdet,1) .gt. 0.00000001 ) ================== 004

      pconcd(kdet) = 0.0 ! % of total mean concentration
      wconcm = 0.0 ! initialise the mean concentration from this works
      do IS = 1, NS ! ----------------------------------------------------------
      if ( FMS(IS) .gt. 0.000001 ) then
      xxcf = TELODEshots(iworks,kdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      endif ! if ( FMS(IS) .gt. 0.000001 ) -------------------------------------
      wloadm = wloadm + TELODEshots(iworks,kdet,IS) 
      enddo ! do IS = 1, NS ----------------------------------------------------
      wconcm = wconcm / NS ! mean concentration from this works
      if ( C(kdet,1) .gt. 0.000001 ) then
      pconcd(kdet) = 100.0*wconcm/C(kdet,1) ! % concentration from works --- 004
      endif
    
      storedis(1,iiworks) = pconcd(kdet)
      storedis(2,iiworks) = xworksAV(kdet) ! =============================== 004
       
*     ==========================================================================
      toatal03C = 0.0 ! initialise total conc from sewage effluents (03)
      toatal12C = 0.0 ! initialise total conc from intermittent discharges (12)
      toatal05C = 0.0 ! industrial total conc from industrial (05)
      toatal39C = 0.0 ! initialise mine discharges (39)
      toatal60C = 0.0 ! initialise other discharges (60)
      toatal61C = 0.0 ! initialise other discharges (61)
      toatal62C = 0.0 ! initialise fish farms (62)
      toatalC = 0.0   ! initialise total concentration from all discharges 
      
      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 004
*     ==========================================================================

      do ifx = 1, kountworks ! check the totals ------------- P & Qi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalC = toatalC + storedis(1,ifx) ! all discharges ------ Qi.CSV
      toatalL = toatalL + storedis(2,ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03C = toatal03C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal03L = toatal03L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 12 ) then ! intermittent discharges (12)
      toatal12C = toatal12C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal12L = toatal12L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05C = toatal05C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal05L = toatal05L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39C = toatal39C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal39L = toatal39L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60C = toatal60C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal60L = toatal60L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61C = toatal61C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal61L = toatal61L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62C = toatal62C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal62L = toatal62L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks ! ---------------------- P & Qi.CSV

      endif ! if ( JT(feeture) .ne. 10 ) +++++++++++++++++++++++++++++++++++ 004
      endif ! if ( feeture .gt. 0 ) ++++++++++++++++++++++++++++++++++++++++ 004
      endif ! if ( JT(jworks) .ne. 12 ) then ! exclude storm overflows ----- 004
 3000 continue ! end of loop on the number of works WWWWWWWWWWWWWWWWWWWWWWWWWWWW 
      
      place = 400.0 ! indicator of part of code in SIMCAT doing this ------- 004
      write(190+kdet,4487)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place ! --------------------------------------- Pi.CSV
      write(160+kdet,4487)feeture,ireach,Length of main river, ! ! ------ Qi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
     &toatal03L,toatal05L,toatal39L,toatal60L,toatal61L,toatalL,
     &toatal12L,toatal62L,place ! --------------------------------------- Qi.CSV
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
      pconcd(kdet) = 0.0 ! set % from a works of the total conc in river --- 005
        
      storedis(1,kountworks) = pconcd(kdet)
      storedis(2,kountworks) = xworksAV(kdet) ! ============================ 005
      
      place = 500.0 ! indicator of part of code in SIMCAT doing this ------- 005
      write(190+kdet,4587)feeture,ireach,Length of main river, ! -------- Qi.CSV
     &unamex,GIScode(feeture),C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place
      write(160+kdet,4587)feeture,ireach,Length of main river, ! -------- Pi.CSV
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
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
      do IS = 1,NS ! -----------------------------------------------------------
      xxxnew = xxxnew + TELODEshots(iworks,kdet,IS) ! sum the load shots .......
      enddo ! ------------------------------------------------------------------
      xxxnew = xxxnew / float(NS) ! mean load from this discharge ..............
      xworksAV(kdet) = 100.0*xxxnew / ! % of river mean load ...................
     &LMcontrib(NTD,kdet,1) ! % of river ! % of river mean load ................
      if ( xworksAV(kdet) .lt. 1.0-20 ) xworksAV(kdet) = 0.0 ! .................
      endif ! if ( LMcontrib(NTD,kdet,1) .gt. 0.00000001 ) ================= 006
      
      pconcd(kdet) = 0.0 ! % of total mean concentration
      wconcm = 0.0 ! initialise the mean concentration from this works
      do IS = 1, NS ! ----------------------------------------------------------
      if ( FMS(IS) .gt. 0.000001 ) then
      xxcf = TELODEshots(iworks,kdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      endif ! if ( FMS(IS) .gt. 0.000001 ) -------------------------------------
      wloadm = wloadm + TELODEshots(iworks,kdet,IS) 
      enddo ! do IS = 1, NS ----------------------------------------------------
      wconcm = wconcm / NS ! mean concentration from this works
      if ( C(kdet,1) .gt. 0.000001 ) then
      pconcd(kdet) = 100.0*wconcm/C(kdet,1) ! % concentration from works --- 006
      endif

      storedis(1,iiworks) = pconcd(kdet) 
      storedis(2,iiworks) = xworksAV(kdet) ! =============================== 006

*     ==========================================================================
      toatal03C = 0.0 ! initialise total conc from sewage effluents (03)
      toatal12C = 0.0 ! initialise total conc from intermittent discharges (12)
      toatal05C = 0.0 ! industrial total conc from industrial (05)
      toatal39C = 0.0 ! initialise mine discharges (39)
      toatal60C = 0.0 ! initialise other discharges (60)
      toatal61C = 0.0 ! initialise other discharges (61)
      toatal62C = 0.0 ! initialise fish farms (62)
      toatalC = 0.0   ! initialise total concentration from all discharges 
      
      toatal03L = 0.0 ! initialise total load from sewage effluents (03)
      toatal12L = 0.0 ! initialise total load from intermittent discharges (12)
      toatal05L = 0.0 ! industrial total load from industrial (05)
      toatal39L = 0.0 ! initialise mine discharges (39)
      toatal60L = 0.0 ! initialise other discharges (60)
      toatal61L = 0.0 ! initialise other discharges (61)
      toatal62L = 0.0 ! initialise fish farms (62)
      toatalL = 0.0   ! initialise total load from all discharges ---------- 006
*     ==========================================================================

      do ifx = 1, kountworks ! check the totals  ------------ P & Qi.CSV
      kfx = identify werks (ifx)
      if ( JT(kfx) .ne. 12 .and. JT(kfx) .ne. 62 ) then
      toatalC = toatalC + storedis(1,ifx) ! all discharges ------ Qi.CSV
      toatalL = toatalL + storedis(2,ifx) ! all discharges ------ Pi.CSV
      endif
      if ( JT(kfx) .eq. 03 ) then ! sewage works (03)
      toatal03C = toatal03C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal03L = toatal03L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 12 ) then ! intermittent discharges (12)
      toatal12C = toatal12C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal12L = toatal12L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 05 ) then ! industrial discharges (04)
      toatal05C = toatal05C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal05L = toatal05L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 39 ) then ! mine discharges (39)
      toatal39C = toatal39C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal39L = toatal39L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 60 ) then ! other discharges (60)
      toatal60C = toatal60C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal60L = toatal60L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 61 ) then ! other discharges (61)
      toatal61C = toatal61C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal61L = toatal61L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      if ( JT(kfx) .eq. 62 ) then ! fish farms (62)
      toatal62C = toatal62C + storedis(1,ifx) ! ----------------- Qi.CSV
      toatal62L = toatal62L + storedis(2,ifx) ! ----------------- Pi.CSV
      endif
      enddo ! do ifx = 1, kountworks35 ! -------------------- P & Qi.CSV

      endif ! if ( JT(jworks) .ne. 12 ) exclude storm overflows ------------ 006
 5000 continue ! do iworks = 1,kountworks ================================== 006

      place = 600.0 ! indicator of part of code in SIMCAT doing this ------- 006
      write(190+kdet,4687)feeture,ireach,Length of main river, ! ----------- 006
     &unamex,GIScode(feeture),C(kdet,1),
     &(storedis(1,iw),iw=1,kountworks35),
     &toatal03C,toatal05C,toatal39C,toatal60C,toatal61C,toatalC,
     &toatal12C,toatal62C,place
      write(160+kdet,4687)feeture,ireach,Length of main river, ! ----------- 006
     &unamex,GIScode(feeture),LMcontrib(NTD,kdet,1),
     &(storedis(2,iw),iw=1,kountworks35),
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

      do idet = 1, ndet ! ======================================================
      if ( QTYPE (idet) .ne. 4 ) then ! ========================================
      wloadd (idet) = 0.0 ! mean total discharge loads in the river ------------
      endif
      enddo ! ==================================================================
      
*     wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
*     loop on the number of upstream works -------------------------------------
      if ( kountworks .gt. 0 ) then ! there are upstream works at this point ---
      do 1000 iworks = 1, kountworks ! loop on works ---------------------------
      iline = 0
      jworks = identify werks (iworks) ! feature number for the discharge ~~~~~~
      xworksd = 0.0 ! marker that a heading is needed --------------------------

*     loop on the determinands +++++++++++++++++++ for the works numbered iworks
      do 1001 idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (idet) .eq. 4 ) goto 1001 ! +++++++++++++++++++++++++++++++++++
      xworksAV (idet) = 0.0 ! % from a works of the total load in the river ----
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( LMcontrib(NTD,idet,1) .gt. 0.00000001 ) then ! total of net loads +++  
*     ==========================================================================
      xxxnew = 0.0 ! initialise mean load from this discharge ..................
      do IS = 1,NS ! -----------------------------------------------------------
      xxxnew = xxxnew + TELODEshots(iworks,idet,IS) ! sum the load shots .......
      enddo ! ------------------------------------------------------------------
      xxxnew = xxxnew / float(NS) ! mean load from this discharge ..............
      xworksAV(idet) = 100.0*xxxnew / ! ................... % of river mean load
     &LMcontrib(NTD,idet,1) !  % of river mean load ........................ 160
*     ==========================================================================               
      call identify the shots for percentiles (idet) ! PPPPPP of percentiles ...
*     ==========================================================================

      total effluent prop (idet) = ! total eff % of the total river load -------
     &total effluent prop (idet) + xworksAV(idet) ! ----------------------------
  
      call sort format 2 (xworksAV(idet),LMcontrib(NTD,idet,1))
      TenAV(idet) = valchars10 ! xworksAV(idet)
      
      xworksTE(idet) = 0.0 ! ------ % from this works of the total effluent load  

      call use shots to get mean load for works (iworks,idet,wloadm) ! ---------
          
      TELOADAV(iworks,idet) = wloadm ! -----------------------------------------
      
      xworksTE(idet) = 100.0*TELOADAV(iworks,idet)/
     &                 LMcontrib(01,idet,1) ! ------------------------------ 160
      
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
      write(120+idet,4423)unamex,dname(idet),lunits(idet), ! heading ---- Di.ADL
     &units(idet) ! heading --------------------------------------------- Di.ADL
 4423 format(///140('=')/'Apportionment of works at: ', ! --------------- Di.ADL
     &a37/140('-')/'Source of ',a11, ! ---------------------------------- Di.ADL
     &18x,'% of the load',4x,'load in',3x, ! ---------------------------- Di.ADL
     &'-- mean concentration',3x,'--- % of annual mean',3x, ! ----------- Di.ADL
     &'number of      accumulating'/ ! ---------------------------------- Di.ADL
     &39x,' in the river',2x,'the river',3x,'-- added to the river', ! -- Di.ADL
     &3x,'------ concentration',5x,'samples',4x,'discharge load'/ ! ----- Di.ADL
     &57x,'(',a4,')',3x,'--------- (',a4,') ----   ', ! ----------------- Di.ADL
     &'- added to the river',5x,'-------',4x,14('-')/140('=')) ! -------- Di.ADL
      else ! if ( kdirect .eq. 1 ---------------------------------------- Di.ADL
      write(120+idet,2243)unamex,dist(feeture),rname(ireach), ! --------- Di.ADL
     &dname(idet),lunits(idet),units(idet) ! ---------------------------- Di.ADL
 2243 format(///140('=')/'Apportionment of works at Feature: ',a37, ! --- Di.ADL
     &5x,f10.1,2x, ! ---------------------------------------------------- Di.ADL
     &'kilometres from the head of Reach: ',a16/140('-')/ ! ------------- Di.ADL
     &'Source of ',a11,18x,'% of the load',4x,'load in',3x, ! ----------- Di.ADL
     &'-- mean concentration',3x,'--- % of annual mean',3x, ! ----------- Di.ADL
     &'number of      accumulating'/ ! ---------------------------------- Di.ADL
     &39x,' in the river',2x,'the river',3x,'-- added to the river', ! -- Di.ADL
     &3x,'------ concentration',5x,'samples',4x,'discharge load'/ ! ----- Di.ADL
     &57x,'(',a4,')',3x,'--------- (',a4,') ----   ', ! ----------------- Di.ADL
     &'- added to the river',5x,'-------',4x,14('-')/140('=')) ! -------- Di.ADL
      
      endif ! if ( kdirect .eq. 9 .or. kdirect .eq. 3 ) ----------------- Di.ADL 
      endif ! if ( qtype(idet) .-ne. 4 ) then ! +++++++++++++++++++++++++ Di.ADL
      enddo ! do idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++ Di.ADL

      
*     ##########################################################################
      do idet = 1, ndet ! PPPPPPPPPPP apportionment of percentiles PPPPPP Ai.ADC
      if ( QTYPE (idet) .ne. 4 ) then ! apportionment of percentiles PPPP Ai.ADC
      call identify the shots for percentiles (idet) ! PPPPPPPPPP of percentiles
      if ( kdirect .ne. 1 ) then ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,4843)dname(idet),unamex,rname(ireach) ! ------------ Ai.ADC
 4843 format(//140('=')/'The percentage of percentile concentrations ',!- Ai.ADC
     &'of: ',a11/'Supplied by individual discharges at the ', ! --------- Ai.ADC
     &a37,3x,'on Reach: ',a16/140('-')) ! ------------------------------- Ai.ADC
      else ! if ( kdirect .ne. 1 ) PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,4443)dname(idet),unamex,rname(ireach) !------------- Ai.ADC
 4443 format(//140('=')/'The percentage of percentile concentrations ',!  Ai ADC
     &'of: ',a11/'Supplied by individual discharges at: ',a37, ! -------- Ai ADC
     &3x,'on Reach: ',a16/140('-')) ! ----------------------------------- Ai.ADC
      endif ! if ( kdirect .ne. 1 ) PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,8776) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 8776 format(46x,'mean   50%tile   90%tile   95%tile   98%tile', ! PPPPPP Ai.ADC
     &'   99%tile 99.5%tile 99.9%tile') ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,7654) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 7654 format(140('-')) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      endif ! if ( QTYPE (idet) .ne. 4 ) PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      enddo ! do idet = 1, ndet PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
*     ##########################################################################

      ihead = 2 ! heading has been written +++++++++++++++++++++ Di.ADL and .ADC
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++ Di.ADL and .ADC
      endif ! if ( ihead .eq. 1 ) ++++++++++++++++++++++++++++++ Di.ADL and .ADC

      
      
      if ( iline .eq. 1 ) then ! llllllllllllllllllllllllllllllllllllllll Di.ADL
      iline = 0 ! ------------------------------------------------------- Di.ADL

*     write out the portion from individual works nnnnnnnnnnnnnnnnnnnnnnn Di.ADL
      if ( nobigout .le. 0 ) then ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn Di.ADL
          
      do jdet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
      if ( qtype(jdet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++ Di.ADL
          
      wconcd(jdet) = 0.0 ! mean concentration ! --------------------------------
      pconcd(jdet) = 0.0 ! % of total mean concentration of river quality ------

      wconcm = 0.0 ! initialise the mean concentration from this works
      wconcs = 0.0 ! standard deviation
      wloadm = 0.0 ! initialise the mean load from this works
      wloads = 0.0 ! standard deviation
      
      do IS = 1, NS ! ----------------------------------------------------------
      if ( FMS(IS) .gt. 0.000001 ) then
      xxcf = TELODEshots(iworks,jdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      wconcs = wconcs + xxcf*xxcf
      else 
      xxcf = 0.0 
      endif ! if ( FMS(IS) .gt. 0.000001 ) -------------------------------------
      wloadm = wloadm + TELODEshots(iworks,jdet,IS) 
      wloads = wloads + TELODEshots(iworks,jdet,IS) 
     &                * TELODEshots(iworks,jdet,IS)
      enddo ! do IS = 1, NS ----------------------------------------------------
      
      wconcs = (wconcs-wconcm/NS)/(NS-1)
      wloads = (wloads-wloadm/NS)/(NS-1)

      if (wconcs .gt. 1.0E-20) then
      wconcs = SQRoot(341099,wconcs)
      wloads = SQRoot(341099,wloads)
      endif ! if (wconcs .gt. 1.0E-20) 
      wconcm = wconcm / NS ! mean concentration from this works
      wloadm = wloadm / NS ! mean load from this works 
      
      TELOADAV(iworks,jdet) = wloadm

      IQ = JQ(jworks)
      
      pconk = 100.0*wconcm/C(jdet,1)
      wconcd(jdet) = wconcm ! mean concentration from this works ---------------
      pconcd(jdet) = 100.0*wconcm/C(jdet,1) ! percent of mean concentration ---=
      wloadd(jdet) =  wloadm ! mean load from this works -----------------------
 

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XML = wloadm ! set the mean load +++++++++++++++++++++++++++++++++++++++++
      XSL = wloads ! set the standard deviation ++++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,float(pnum(IQ,jdet))) ! number of samples (03) ++++
      SEM = XSL / SQRoot(107334,xqualn) ! standard error +++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XLL = amax1 (0.0, (XML - t95 * SEM) ) ! lower confidence limit +++++++++++
      XUL = amax1 (0.0, (XML + t95 * SEM) ) ! upper confidence limit +++++++++++
      XMPL = 100.0*XML/LMcontrib(NTD,jdet,1) ! concentration as a percentage % +
      XLPL = 100.0*XLL/LMcontrib(NTD,jdet,1) ! lower confidence limit as % +++++
      XUPL = 100.0*XUL/LMcontrib(NTD,jdet,1) ! upper confidence limit as % +++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = wconcm ! set the mean concentration +++++++++++++++++++++++++++++++++
      XS = wconcs ! set the standard deviation +++++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      xqualn = amax1 (0.0001,float(pnum(IQ,jdet))) ! number of samples +++++++++
      SEM = XS / SQRoot(107334,xqualn) ! standard error ++++++++++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit on mean +++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit on mean +++++
      XMP = 100.0*XM/C(jdet,1) ! mean concentration as % of total ++++++++++++++
      XLP = 100.0*XL/C(jdet,1) ! lower confidence limit on this % ++++++++++++++
      XUP = 100.0*XU/C(jdet,1) ! upper confidence limit on this % ++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      call assem(XL,XU,SET1)
      call assem2(XLP,XUP,SET2)
      call assem3(pconk,SET3)
      if ( TELOADAV(iworks,jdet) .gt. -0.0000001 ) then ! ======================
 
      pxxxx = 100.0*TELOADAV(iworks,jdet)/LMcontrib(NTD,jdet,1)
            
      call sort format 4 (wloadm,wconcm,pxxxx,wloadd(jdet)) ! ----------- Di.ADL
*     -------------------------------------------------------------------------- 
      write(120+jdet,3347)uname(jworks),valchars12,valchars10, ! -------- Di.ADL
     &valchars11,SET1,SET3,SET2,pnum(IQ,jdet),valchars13 ! -------------- Di.ADL
 3347 format(a37,3x,a10' %', ! ------------------------------------------ Di.ADL
     &1x,2a10,1x,a16,a6,' ',a11,' %',i12,8x,a10) ! ---------------------- Di.ADL
*     -------------------------------------------------------------------------- 
     
      write(180+jdet,9799)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &rname(IREACH),uname(jworks),Length of main river, ! -------------- DAi.CSV
     &TELOADAV(iworks,jdet),xworksAV(jdet),wloads,! -------------------- DAi.CSV
     &XM,XMP,XS,xqualn,XL,XU,XLP,XUP,xworksTE(jdet), ! ----------------- DAi.CSV
     &jxtype,unamex ! -------------------------------------------------- DAi.CSV
 9799 format(' ',a40,',',i4,',',a40,',',a16,',', ! --------------------- DAi.CSV
     &'Load and concentration from this discharge:',',From: ',a37, ! --- DAi.CSV
     &(',',0pf12.2), ! ------------------------------------------------- DAi.CSV
     &(',',1pe12.4),(',',0pf11.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',1pe12.4),(',',0pf11.2,'%'),(',',1pe12.4),(',',0pf12.2), ! --- DAi.CSV
     &2(',',1pe12.4),3(',',0pf11.2,'%'), ! ----------------------------- DAi.CSV
     &(',,,'),i4,',,,At: ',a40) ! -------------------------------------- DAi.CSV

      endif ! if ( TELOADAV(iworks,jdet) .gt. 0.00001 ) ========================

            
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
*     check the number of works ++++++++++++++++++++++++++++++++++++++++++++++++        
      if ( iworks .gt. NUED ) then ! ############################### Qi & Pi-CSV
      write(09,*)' * Number of works exceeds the current maximum', ! Qi & Pi-CSV
     &iworks,NUED ! ################################################ Qi & Pi-CSV
      write( *,*)' * Number of works exceeds the current maximum', ! Qi & Pi-CSV
     &iworks,NUED ! ################################################ Qi & Pi-CSV
      call pause ! ################################################# Qi & Pi-CSV
      endif ! if ( iworks .gt. NUED ) ############################## Qi & Pi-CSV
      if ( feeture .gt. NU ) then ! ################################ Qi & Pi-CSV
      write( *,*)' * Number of Features exceeds current maximum', !# Qi & Pi-CSV
     &feeture,NU ! ################################################# Qi & Pi-CSV
      call pause ! ################################################# Qi & Pi-CSV 
      endif ! if ( feeture .gt. NU ) ############################### Qi & Pi-CSV
*     check the number of works ++++++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
      
   
      
*     ##########################################################################      
      do i = 1, 7 ! ------------------------------------------------------------
      xmnl(i) = 0.0
      do j = 1, 9
      xxl(i,j) = 0.0
      enddo ! do j = 1, 9
      enddo ! do i = 1, 7 ------------------------------------------------------
           
      
*     ***********************************************************************161
      do IS = 1, NS
      Y(IS) = CMS(jdet,IS) ! shots of river water quality
      enddo
*     Sort array Y of NS values into ascending order of magnitude
*     The previous location of ranked values are stored in IVAL
*     Y     - values to be ranked
*     IVAL  - position of ranked values in pre-ranked Y
      call rank2 (Y,IVAL,NS)
*     ***********************************************************************161           
      
      do iloop = 1,9 ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ten50  = '     .....' ! % load for 50-percentile in the river
      Ten90  = '     .....' ! % load for 90-percentile in the river
      Ten95  = '     .....' ! % load for 95-percentile in the river
      Ten98  = '     .....' ! % load for 98-percentile in the river
      Ten99  = '     .....' ! % load for 99-percentile in the river
      Ten995 = '     .....' ! % load for 99.5-percentile in the river
      Ten999 = '     .....' ! % load for 99.9-percentile in the river
      call identify the shots for percentiles (jdet) ! PPPPPPPPPP of percentiles
      
      JS50  = max0(1,min0 (NS,(K50  - 4 + iloop - 1)))
      JS90  = max0(1,min0 (NS,(K90  - 4 + iloop - 1)))
      JS95  = max0(1,min0 (NS,(K95  - 4 + iloop - 1)))
      JS98  = max0(1,min0 (NS,(K98  - 4 + iloop - 1)))
      JS99  = max0(1,min0 (NS,(K99  - 4 + iloop - 1)))
      JS995 = max0(1,min0 (NS,(K995 - 4 + iloop - 1)))
      JS999 = max0(1,min0 (NS,(K999 - 4 + iloop - 1)))
      
      if ( iloop .eq. 1 ) then ! calculate the mean contribution ============161
      wconcm = 0.0
      do is = 1, NS
      xflow = amax1 (FMS(is),0.000001*FLOW(1))
      xxcf = TELODEshots(iworks,jdet,IS)/xflow ! concentration -----------------
      wconcm = wconcm + xxcf
      enddo ! do is = 1, NS
      
      wconcm = wconcm/NS
      xpercent = 100.0*(wconcm/C(jdet,1)) 
      
      call sort format 1a (xpercent)
      TenAV(jdet) = valchars10
      endif ! if ( iloop .eq. 1 )  ! calculate the mean contribution ===========
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS50))/
     &FMS(IVAL(JS50)))/CMS(jdet,IVAL(JS50))
      xmnl(1) = xmnl(1) + xpercent
      xxl(1,iloop) = xpercent
      call sort format 1a (xpercent)
      ten50 = valchars10  
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS90))/
     &FMS(IVAL(JS90)))/CMS(jdet,IVAL(JS90))
      xmnl(2) = xmnl(2) + xpercent
      xxl(2,iloop) = xpercent
      call sort format 1a (xpercent)
      if ( NS .gt. 49 ) ten90 = valchars10  
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS95))/
     &FMS(IVAL(JS95)))/CMS(jdet,IVAL(JS95))
      xmnl(3) = xmnl(3) + xpercent
      xxl(3,iloop) = xpercent
      call sort format 1a (xpercent)
      if ( NS .gt. 99 ) ten95 = valchars10  
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS98))/
     &FMS(IVAL(JS98)))/CMS(jdet,IVAL(JS98))
      xmnl(4) = xmnl(4) + xpercent
      xxl(4,iloop) = xpercent
      call sort format 1a (xpercent)
      if ( NS .gt. 199 ) ten98 = valchars10 
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS99))/
     &FMS(IVAL(JS99)))/CMS(jdet,IVAL(JS99))
      call sort format 1a (xpercent)
      xmnl(5) = xmnl(5) + xpercent
      xxl(5,iloop) = xpercent
      if ( NS .gt. 499 ) ten99 = valchars10
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS995))/
     &FMS(IVAL(JS995)))/CMS(jdet,IVAL(JS995))
      xmnl(6) = xmnl(6) + xpercent
      xxl(6,iloop) = xpercent
      call sort format 1a (xpercent)
      if ( NS .gt. 999 ) ten995 = valchars10  
      
      xpercent = 100.0*(TELODEshots(iworks,jdet,IVAL(JS999))/
     &FMS(IVAL(JS999)))/CMS(jdet,IVAL(JS999))
      xmnl(7) = xmnl(7) + xpercent
      xxl(7,iloop) = xpercent
      call sort format 1a (xpercent)
      if ( NS .gt. 4999 ) ten999 = valchars10 
         
      if ( xworksAV(jdet) .gt. 0.00001 ) then ! PPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      if ( iloop .eq. 1 ) then
      write(140+jdet,8842)uname(feeture) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 8842 format('Values for NINE ranked Monte Carlo shots that surround ',
     &'each percentile at: ',a37/140('+'))
      endif
      
      write(140+jdet,4194)uname(jworks),TenAV(jdet),Ten50, ! PPPPPPPPPPPP Ai.ADC
     &Ten90,Ten95,Ten98,Ten99,Ten995,Ten999,iloop ! PPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4194 format('% from ',a33,8a10,i7) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      
      endif ! if ( xworksAV(jdet) .gt. 0.00001 ) PPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      enddo ! do iloop = 1,9 ++++++++++++++++++++++++++++++++++++++++++++++++160

      
*     ==========================================================================
      if ( TELOADAV(iworks,jdet) .gt. 0.000001 ) then ! ========================
      do k = 1, 7 ! ------------------------------------------------------------
      do i = 1, 8
      do j = i + 1, 9
      if ( xxl(k,i) .ge. xxl(k,j) ) then
      xtemp = xxl(k,j)
      xxl(k,j) = xxl(k,i)
      xxl(k,i) = xtemp
      endif
      enddo
      enddo
      enddo ! do k = 1, 7 ------------------------------------------------------
      
      call sort format 1a (xxl(1,5))   
      ten50 = valchars10  
      call sort format 1a (xxl(2,5))   
      if ( NS .gt. 49 ) ten90 = valchars10  
      call sort format 1a (xxl(3,5))   
      if ( NS .gt. 99 ) ten95 = valchars10  
      call sort format 1a (xxl(4,5))   
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1a (xxl(5,5))   
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1a (xxl(6,5))   
      if ( NS .gt. 999 ) ten995 = valchars10 
      call sort format 1a (xxl(7,5))   
      if ( NS .gt. 4999 ) ten999 = valchars10
      
      write(140+jdet,64)uname(jworks),
     &TenAV(jdet),ten50,ten90,ten95,ten98,
     &ten99,ten995,ten999 
   64 format("% from ",a33,8a10," median")
     
      do i = 1,7
      xmnl(i) = xmnl(i)/9.0
      enddo

      call sort format 1a (xmnl(1))   
      ten50 = valchars10  
      call sort format 1a (xmnl(2))   
      if ( NS .gt. 49 ) ten90 = valchars10  
      call sort format 1a (xmnl(3))   
      if ( NS .gt. 99 ) ten95 = valchars10  
      call sort format 1a (xmnl(4))   
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1a (xmnl(5))   
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1a (xmnl(6))   
      if ( NS .gt. 999 ) ten995 = valchars10 
      call sort format 1a (xmnl(7))
      if ( NS .gt. 4999 ) ten999 = valchars10

      write(140+jdet,1167)uname(jworks),TenAV(jdet),ten50, ! ------------ Ai.ADC
     &ten90,ten95,ten98,ten99,ten995,ten999 
 1167 format("% from ",a33,8a10,"   mean")
      
      endif ! if ( TELOADAV(iworks,jdet) .gt. 0.000001 .gt. 0.000001 ) =========
*     ==========================================================================
      write(140+jdet,3654) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 3654 format(140('+')) ! =======================================================
*     ==========================================================================
      
      
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     xworksAV is the % river load from this discharge -------------------------    
*     xworksTE is the % of total effluent load from from this discharge -------- 
      write(110+jdet,9899)GIScode(feeture),unamex,rname(IREACH), ! ------ Di.CSV
     &uname(jworks),(xworksAV(jdet),imon=1,12), ! ----------------------- Di.CSV
     &xworksAV(jdet), ! % of total river load --------------------------- Di.CSV
     &jxtype,jt(jworks), ! ---------------------------------------------- Di.CSV
     &xworksTE(jdet), ! % of total effluent load ------------------------ Di.CSV
      
     &wloadd(jdet),xll,xul, ! mean load from this works ------161-------- Di.CSV
     &pconcd(jdet), ! % of mean river cncentration rom this works --161-- Di.CSV
     &wconcd(jdet),xl,xu, ! part of mean river conc ----------161-------- Di.CSV
     &Length of main river ! ---------------------------------161-------- Di.CSV
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++ 2/03
     &xqualn,3)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++ 3/12
     &xqualn,12)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++ 4/05
     &xqualn,5)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 28/60
     &xqualn,60)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 29/61
     &xqualn,61)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 30/62
     &xqualn,62)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++ 5/39
     &xqualn,39)
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++++++ 01
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++++ 2/17
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( LMcontrib(ip,idet,1) .gt. 1.0e-10 ) -------------------------
*     boundaries and tributaries (17) ----------------------------------- Di.ADL 
 
     
      
*     reach-type diffuse sources (20) ----------------------------------- Di.ADL 
      ip = 20 ! reach-type diffuse sources (20) ------------------------- Di.ADL
      if ( LMcontrib(ip,idet,1) .gt. -1.0e-10 ) then
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = CMcontrib(ip,idet,1) ! set the mean +++++++++++++++++++++++++++++++++
      XS = CMcontrib(ip,idet,2) ! set the standard deviation +++++++++++++++++++
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++++++ 20
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 18/13
     &xqualn,13)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 19/15
     &xqualn,15)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 14/42
     &xqualn,42)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++ 6/25
     &xqualn,25)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,27)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,29)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,31)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,33)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,35)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 12/37
     &xqualn,37)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP,
     &xqualn,40)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 15/46
     &xqualn,46)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 16/48
     &xqualn,48)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 23/50
     &xqualn,50)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 24/52
     &xqualn,52)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 25/54
     &xqualn,54)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 26/56
     &xqualn,56)
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
      call write out the csv2 (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++ 27/58
     &xqualn,58)
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++++++ 21
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! ++++++++++ 22
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
      call write out the csv (idet,ip,plode,pconk,XL,XU,XLP,XUP, ! +++++++++ NTF
     &xqualn)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total diffuse additions to the river ------------------------------ Di.ADL 
*     tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt  
 
     
      
*     oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo      
*     overall totals ---------------------------------------- Di.ADL and DAi.CSV 
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
      write(180+idet,1202)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DAi.CSV
     &nameprop(ip),Length of main river, ! ----------------------------- DAi.CSV
     &Xload(idet,1,i13),plode,Xload(idet,2,i13), ! --------------------- DAi.CSV
     &XM,XS,xqualn,XL,XU,pconk,XLP,XUP,unamex ! ------------------------ DAi.CSV
 1202 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load and concentration from:,',a37,(',',0pf12.2), ! ---- total -- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',1pe12.4),(',',1pe12.4), ! ----------------------------------- DAi.CSV
     &(',',0pf12.2),2(',',1pe12.4), ! ---------------------------------- DAi.CSV
     &3(',',0pf12.2,'%'),(',,,,,'),',,At: ',a40)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     overall totals ---------------------------------------- Di.ADL and DAi.CSV 
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
*     load introduced by natural purification                      ... TNLOADUP2
*     load removed by natural purification                         ... TNLOADDN2
*     **************************************************************************
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
*     net load removed by gap filling for river flows              ... TILOADDN2
*     net load removed by gap filling for river quality            ... TALOADDN2
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg      
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
      
     

*     NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN 160
      do idet = 1, ndet   
      if ( QTYPE (idet) .ne. 4 ) then ! ================================= Ai.CSV
      iwrite = 0
      
      if ( kdirect .eq. 1 .or. kdirect .eq. 3 ) then 
      if ( xworksd .gt. 0.00001 ) then 
      iwrite = 0
      call apportion the river percentiles (idet)
      endif
      endif ! if ( kdirect .eq. 1 .or. kdirect .eq. 3 )
     
*     write out the river flows and concentrations RRRRRRRRRRRRRRRRRRRRRRRRRRRRR  
      call identify the shots for percentiles (idet) ! PPPPPPPPPP of percentiles
          
      Ten00 = '     .....' 
      Ten50 = '     .....' 
      Ten90 = '     .....' 
      Ten95 = '     .....' 
      Ten98 = '     .....' 
      Ten99 = '     .....' 
      Ten995 = '     .....' 
      Ten999 = '     .....' 
           
      call sort format 1 (Flow(1))
      ten00 = valchars10 ! copy of mean river flow
      call sort format 1 (FMS(JS50))
      ten50 = valchars10 ! copy of 50-percentile  
      call sort format 1 (FMS(JS90))
      if ( NS .gt. 49  ) ten90 = valchars10 ! copy of 90-percentile  
      call sort format 1 (FMS(JS95))
      if ( NS .gt. 99  ) ten95 = valchars10 ! copy of 95-percentile   
      call sort format 1 (FMS(JS98))
      if ( NS .gt. 199 ) ten98 = valchars10 ! copy of 98-percentile  
      call sort format 1 (FMS(JS99))
      if ( NS .gt. 499 ) ten99 = valchars10 ! copy of 99-percentile 
      call sort format 1 (FMS(JS995))
      if ( NS .gt. 999 ) ten995 = valchars10 ! copy of 99.5-percentile   
      call sort format 1 (FMS(JS999))
      if ( NS .gt. 4999) ten999 = valchars10 ! copy of 99.9-percentile  
      
      if ( xworksd .gt. 0.00001 ) then 
      write(140+idet,4794)dname(idet),unamex,ten00, ! PPPPPPPPPPPPPPPPPPP Ai.ADC     
     &Ten50,Ten90,Ten95,Ten98,Ten99,Ten995,Ten999 ! PPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4794 format(140('=')/'River flows at %-ile concentrations of: ', ! PPPPP Ai.ADC
     &a11,' ... at: ',a37/140('-')/ ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC 
     &46x,'mean   50%tile   90%tile   95%tile   98%tile', ! PPPPPPPPPPPPP Ai.ADC
     &'   99%tile 99.5%tile 99.9%tile'/140('-')/ ! PPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     &'River flows at %-ile concentrations:',4x,8a10) ! PPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,7654) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     
      Ten00 = '     .....' 
      Ten50 = '     .....' 
      Ten90 = '     .....' 
      Ten95 = '     .....' 
      Ten98 = '     .....' 
      Ten99 = '     .....' 
      Ten995 = '     .....' 
      Ten999 = '     .....' 
      
      call sort format 1 (C(idet,1))
      ten00 = valchars10  
      call sort format 1 (CMS(idet,JS50))
      ten50 = valchars10  
      call sort format 1 (CMS(idet,JS90))
      if ( NS .gt. 49  ) ten90 = valchars10  
      call sort format 1 (CMS(idet,JS95))
      if ( NS .gt. 99  ) ten95 = valchars10  
      call sort format 1 (CMS(idet,JS98))
      if ( NS .gt. 199 ) ten98 = valchars10 
      call sort format 1 (CMS(idet,JS99))
      if ( NS .gt. 499 ) ten99 = valchars10
      call sort format 1 (CMS(idet,JS995))
      if ( NS .gt. 999 ) ten995 = valchars10  
      call sort format 1 (CMS(idet,JS999))
      if ( NS .gt. 4999) ten999 = valchars10  
            
      write(140+idet,4894)dname(idet),ten00, ! PPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     &Ten50,Ten90,Ten95,Ten98,Ten99,Ten995,Ten999 ! PPPPPPPPPPPPPPPPPPPPP Ai.ADC
 4894 format('River concentrations: ',a11,7x,8a10) ! PPPPPPPPPPPPPPPPPPPP Ai.ADC
      
      ten00 = '     .....' 
      ten50 = '     .....' 
      ten90 = '     .....' 
      ten95 = '     .....' 
      ten98 = '     .....' 
      ten99 = '     .....' 
      ten995 = '     .....' 
      ten999 = '     .....' 
      
      xlode = 0.0
      do is = 1, NS
      xlode = xlode + CMS(idet,is)*FMs(is)    
      enddo
      xlode = xlode / NS
      
      call identify the shots for percentiles (idet) ! PPPPPPPPPP of percentiles
      call sort format 1 (xlode)
      ten00 = valchars10
      call sort format 1 (CMS(idet,JS50) * FMS(JS50))
      ten50 = valchars10  
      call sort format 1 (CMS(idet,JS90) * FMS(JS90))
      if ( NS .gt. 49  ) ten90 = valchars10  
      call sort format 1 (CMS(idet,JS95) * FMS(JS95))
      if ( NS .gt. 99  ) ten95 = valchars10  
      call sort format 1 (CMS(idet,JS98) * FMS(JS98))
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1 (CMS(idet,JS99) * FMS(JS99))
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1 (CMS(idet,JS995) * FMS(JS995))
      if ( NS .gt. 999 ) ten995 = valchars10  
      call sort format 1 (CMS(idet,JS999) * FMS(JS999))
      if ( NS .gt. 4999) ten999 = valchars10  
      
      write(140+idet,2246)Ten00,Ten50,Ten90, ! PPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     &Ten95,Ten98,Ten99,Ten995,Ten999 ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
 2246 format( ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
     &'Total load in the percentile            ',10a10) ! PPPPPPPPPPPPPPP Ai.ADC
     
      if ( kdirect .eq. 3 .or. kdirect .eq. 9 ) then ! PPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,7655) ! --------- apportionment of percentiles PPPPP Ai.ADC
 7655 format(140('=')/) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      else ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      write(140+idet,7655) ! PPPPPPPPP apportionment of percentiles PPPPP Ai.ADC
      endif ! if ( kdirect .eq. 3 etc PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
      endif ! if ( xworksd .gt. 0.00001 ) then PPPPPPPPPPPPPPPPPPPPPPPPPP Ai.ADC
*     write out the river flows and concentrations RRRRRRRRRRRRRRRRRRRRRRRRR 160  

      
      endif ! if ( total effluent prop (jdet) .gt. Small ) PPPPPPPPPPPPPP Ai.ADC
      enddo ! do idet = 1, ndet
*     NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN 160
      
      
      
*     prepare totals for all works ++++++++++++++++++++++++++++++++++++++ Di.ADL
      do jdet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++ Di.ADL
      if ( qtype(jdet) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++ Di.ADL

      wconcm = 0.0 ! initialise the mean concentration from all works
      wconcs = 0.0 ! standard deviation of concentration from all works
      wloadm = 0.0 ! initialise the mean load from all works
      wloads = 0.0 ! standard deviation of load
      XLL = 0.0    ! lower confidence limit on mean load
      XUL = 0.0    ! upper confidence limit on mean load
      XL  = 0.0    ! lower confidence limit on mean concentration
      XU  = 0.0    ! upper confidence limit on mean concentration
      
      do kworks = 1, kountworks ! ==============================================
      do IS = 1, NS ! ----------------------------------------------------------          
      if ( FMS(IS) .gt. 0.000001 ) then
      xxcf = TELODEshots(kworks,jdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      wconcs = wconcs + xxcf*xxcf
      else 
      xxcf = 0.0 
      endif ! if ( FMS(IS) .gt. 0.000001 ) -------------------------------------
      wloadm = wloadm + TELODEshots(kworks,jdet,IS) 
      wloads = wloads + TELODEshots(kworks,jdet,IS) 
     &                * TELODEshots(kworks,jdet,IS)
      enddo ! do IS = 1, NS ----------------------------------------------------
      enddo ! do kworks = 1, kountworks ! =====================================
      
      wconcs = (wconcs-wconcm/NS)/(NS-1)
      wloads = (wloads-wloadm/NS)/(NS-1)
      
      if (wconcs .gt. 1.0E-20) then
      wconcs = SQRoot(341099,wconcs)
      wloads = SQRoot(341099,wloads)
      endif ! if (wconcs .gt. 1.0E-20) 
      
      wconcm = wconcm / NS ! mean concentration from this works
      wloadm = wloadm / NS ! mean load from this works 
      pconk = 100.0*wconcm/C(jdet,1) ! percent concentration from this works
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XML = wloadm ! set the mean load +++++++++++++++++++++++++++++++++++++++++
      XSL = wloads ! set the standard deviation ++++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++      
      SEM = XSL / SQRoot(107334,QUALNB(jdet,1)) ! standard error +++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XLL = amax1 (0.0, (XML - t95 * SEM) ) ! lower confidence limit +++++++++++
      XUL = amax1 (0.0, (XML + t95 * SEM) ) ! upper confidence limit +++++++++++
      XMPL = 100.0*XML/LMcontrib(NTD,jdet,1) ! concentration as a percentage % +
      XLPL = 100.0*XLL/LMcontrib(NTD,jdet,1) ! lower confidence limit as % +++++
      XUPL = 100.0*XUL/LMcontrib(NTD,jdet,1) ! upper confidence limit as % +++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XM = wconcm ! set the mean concentration +++++++++++++++++++++++++++++++++
      XS = wconcs ! set the standard deviation +++++++++++++++++++++++++++++++++
      t95 = errx ( 0.95 ) ! get the standard normal deviate ! ++++++++++++++++++
*     calculate the standard error of estimate of the mean +++++++++++++++++++++
      SEM = XS / SQRoot(107334,QUALNB(jdet,1)) ! standard error ++++++++++++++++
*     calculate the 95% confidence limits about the mean +++++++++++++++++++++++
      XL = amax1 (0.0, (XM - t95 * SEM) ) ! lower confidence limit on conc +++++
      XU = amax1 (0.0, (XM + t95 * SEM) ) ! upper confidence limit on conc +++++
      XMP = 100.0*XM/C(jdet,1) ! upper confidence limit as % +++++++++++++++++++
      XLP = 100.0*XL/C(jdet,1) ! upper confidence limit as % +++++++++++++++++++
      XUP = 100.0*XU/C(jdet,1) ! upper confidence limit as % +++++++++++++++++++      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 160
      
*     DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      if ( nobigout .le. 0 ) then ! ============================================
      write(110+jdet,7299)GIScode(feeture),unamex,rname(IREACH), ! ------ Di.CSV
     &total effluent prop (jdet), ! total % of river load --------------- Di.CSV
     &(total effluent prop (jdet),imon = 1,12), ! ----------------------- Di.CSV
     &jxtype,total effluent load (jdet) ! total effluent load ----------- Di.CSV
*     160 ================================================================== 160
     &,wloadm,XLL,XUL, ! load from all discharges ----------------------- Di.CSV 
     &XMP, ! % of mean river conc from these works ---------------------- Di.CSV 
     &wconcm,XL,XU, ! mean river concentration from these works --------- Di.CSV  
     &Length of main river ! -------------------------------------------- Di.CSV
*     160 ================================================================== 160 
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
      write(180+idet,9099)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &uname(jbodies),xbodiesAV(idet),xbodiesTE(idet),distp ! ----------- DAi.CSV
 9099 format(' ',a40,',',a40,',',a40,',', ! ---------------------------- DAi.CSV
     &'% Annual contribution from individual works',',',2(',', ! ------- DAi.CSV
     &1pe11.4),',,,,,,,,,,,,',',312539606162', ! ----------------------- DAi.CSV
     &(',,,,,,,,,,,,,',1pf11.2)) ! ------------------------------------- DAi.CSV
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
      write(180+idet,8779)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &rnamex,Length of main river,pgf1 ! ------------------------------- DAi.CSV
 8779 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DAi.CSV
     &'% Load added by gap filling ...',',for river flows', ! ---------- DAi.CSV
     &(','0pf12.2),(',',1pe12.4),14(','),'9999,',2(',',0pf12.2)) ! ----- DAi.CSV
      write(180+idet,8729)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &rnamex,Length of main river,pgf2 ! ------------------------------- DAi.CSV
 8729 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DAi.CSV
     &'% Load removed by gap filling ...',',for river flows', ! -------- DAi.CSV
     &(','0pf12.2),(',',1pe12.4),14(','),'-9999,',2(',',0pf12.2)) ! ---- DAi.CSV
      write(180+idet,8739)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &rnamex,Length of main river,pgf3 ! ------------------------------- DAi.CSV
 8739 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DAi.CSV
     &'% Load added by gap filling ...',',for river quality', ! -------- DAi.CSV
     &(','0pf12.2),(',',1pe12.4),14(','),'9999,',2(',',0pf12.2)) ! ----- DAi.CSV
      write(180+idet,8749)GIScode(feeture),jxtype,unamex, ! ------------ DAi.CSV
     &rnamex,Length of main river,pgf4 ! ------------------------------- DAi.CSV
 8749 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DAi.CSV
     &'% Load removed by gap filling ...',',for river quality', ! ------ DAi.CSV
     &(','0pf12.2),(',',1pe12.4),14(','),'-9999,',2(',',0pf12.2)) ! ---- DAi.CSV

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
     
     

      
      subroutine write out the csv (idet,ip,plode,pconk,XL,XU, ! ------- DAi.CSV
     &XLP,XUP,xqualn)
      include 'COMMON DATA.FOR'
      
      character *16 SET1
      character *11 SET2
      character *06 SET3

      plode = 100.0*LMcontrib(ip,idet,1)/Xload(idet,1,i13)
      pconk = 100.0*CMcontrib(ip,idet,1)/C(idet,1)
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ip .eq. 21 .or. ip .eq. 22 ) then ! ------------------------- DAi.CSV
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ggggggggggggggggggg gap filling 
      write(180+idet,1202)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DAi.CSV
     &nameprop(ip),Length of main river, ! ----------------------------- DAi.CSV
     &LMcontrib(ip,idet,1),plode,LMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &CMcontrib(ip,idet,1),pconk,CMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &xqualn,XL,XU,XLP,XUP,unamex ! ------------------------------------ DAi.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) gggggggggggggggggg gap filling 
      else ! if ( ip .eq. 21 .or. ip .eq. 22 ) --------------------------DAi.CSV
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      write(180+idet,1202)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DAi.CSV
     &nameprop(ip),Length of main river, ! ----------------------------- DAi.CSV
     &LMcontrib(ip,idet,1),plode,LMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &CMcontrib(ip,idet,1),pconk,CMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &xqualn,XL,XU,XLP,XUP,unamex ! ------------------------------------ DAi.CSV
 1202 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load and concentration from:,',a37,(',',0pf12.2), ! ------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',0pf12.2),2(',',1pe12.4), ! ---------------------------------- DAi.CSV
     &2(',',0pf12.2,'%'),(',,,,,'),',,At: ',a40)
      endif ! if ( ip .eq. 21 .or. ip .eq. 22 ) ======================== DAi.CSV

     
      if ( ip .ne. NTD ) then
      plode = 100.0*LMcontrib(ip,idet,1)/LMcontrib(NTD,idet,1)
      pconk = 100.0*CMcontrib(ip,idet,1)/C(idet,1)
      call sort format 3 (plode,LMcontrib(ip,idet,1),
     &CMcontrib(ip,idet,1))
      endif
      
      call assem(XL,XU,SET1)
      call assem2(XLP,XUP,SET2)
      call assem3(pconk,SET3)
            
      return
      end


      subroutine write out the csv2 (idet,ip,plode,pconk,XL,XU, ! ------ DAi.CSV
     &XLP,XUP,xqualn,ktp)
      include 'COMMON DATA.FOR'
      
      character *16 SET1
      character *11 SET2
      character *06 SET3

      plode = 100.0*LMcontrib(ip,idet,1)/Xload(idet,1,i13)
      pconk = 100.0*CMcontrib(ip,idet,1)/C(idet,1)
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ip .eq. 21 .or. ip .eq. 22 ) then ! ------------------------- DAi.CSV
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ggggggggggggggggggg gap filling 
      write(180+idet,1201)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DAi.CSV
     &nameprop(ip),Length of main river, ! ----------------------------- DAi.CSV
     &LMcontrib(ip,idet,1),plode,LMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &CMcontrib(ip,idet,1),pconk,CMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &xqualn,XL,XU,XLP,XUP,unamex ! ------------------------------------ DAi.CSV
 1201 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load and concentration from:,',a37,(',',0pf12.2), ! ------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',0pf12.2),2(',',1pe12.4), ! ---------------------------------- DAi.CSV
     &2(',',0pf12.2,'%'),(',,,,,'),',,At: ',a40)
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) gggggggggggggggggg gap filling 
      else ! if ( ip .eq. 21 .or. ip .eq. 22 ) --------------------------DAi.CSV
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      write(180+idet,1202)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DAi.CSV
     &nameprop(ip),Length of main river, ! ----------------------------- DAi.CSV
     &LMcontrib(ip,idet,1),plode,LMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &CMcontrib(ip,idet,1),pconk,CMcontrib(ip,idet,2), ! --------------- DAi.CSV
     &xqualn,XL,XU,XLP,XUP,ktp,unamex ! -------------------------------- DAi.CSV
 1202 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load and concentration from:,',a37,(',',0pf12.2), ! ------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',1pe12.4),(',',0pf12.2,'%'),(',',1pe12.4), ! ----------------- DAi.CSV
     &(',',0pf12.2),2(',',1pe12.4), ! ---------------------------------- DAi.CSV
     &2(',',0pf12.2,'%'),(',,,,'),i4,',,,At: ',a40)
      endif ! if ( ip .eq. 21 .or. ip .eq. 22 ) ======================== DAi.CSV

     
      if ( ip .ne. NTD ) then
      plode = 100.0*LMcontrib(ip,idet,1)/LMcontrib(NTD,idet,1)
      pconk = 100.0*CMcontrib(ip,idet,1)/C(idet,1)
      call sort format 3 (plode,LMcontrib(ip,idet,1),
     &CMcontrib(ip,idet,1))
      endif
      
      call assem(XL,XU,SET1)
      call assem2(XLP,XUP,SET2)
      call assem3(pconk,SET3)
            
      return
      end
     
      
      
      
      subroutine apportion the river percentiles (idet)
      include 'COMMON DATA.FOR'
      dimension Y(NS),IVAL(NS),xxl(7,9),xmnl(7)
      character *10 TenAV,Ten50,Ten90,Ten95,Ten98,Ten99,Ten995,Ten999 ! --------
          
      do IS = 1, NS
      Y(IS) = CMS(idet,IS) ! store the shots of river water quality
      enddo

*     Sorts array Y of NS values into ascending order of magnitude
*     The previous location of ranked values are stored in IVAL
*     Y     - values to be ranked
*     IVAL  - position of ranked values in pre-ranked Y
      
      call rank2 (Y,IVAL,NS)
      
      do ip = 1, n2prop ! ++++++++++ all types of pollution ++++++++++++++++++++
      do i = 1, 7 ! ------------------------------------------------------------
      xmnl(i) = 0.0
      do j = 1, 9
      xxl(i,j) = 0.0
      enddo ! do j = 1, 9
      enddo ! do i = 1, 7 ------------------------------------------------------
      
      do iloop = 1,9 ! look at 9 shots around each percentile ++++++++++++++++++
          
*     initialise the contributions from the source of pollution ----------------
      TenAV  = '     .....' ! % of the mean river concentration
      Ten50  = '     .....' ! % of the 50-percentile in the river
      Ten90  = '     .....' ! % of the 90-percentile 
      Ten95  = '     .....' ! % of the 95-percentile 
      Ten98  = '     .....' ! % of the 98-percentile 
      Ten99  = '     .....' ! % of the 99-percentile 
      Ten995 = '     .....' ! % of the 99.5-percentile 
      Ten999 = '     .....' ! % of the 99.9-percentile 

      if ( CMcontrib(ip,idet,1) .gt. 0.000001 ) then ! ====================

      JS50 = max0(1,min0 (NS,(K50 - 4 + iloop - 1)))
      JS90 = max0(1,min0 (NS,(K90 - 4 + iloop - 1)))     
      JS95 = max0(1,min0 (NS,(K95 - 4 + iloop - 1)))
      JS98 = max0(1,min0 (NS,(K98 - 4 + iloop - 1)))
      JS99 = max0(1,min0 (NS,(K99 - 4 + iloop - 1)))
      JS995 = max0(1,min0 (NS,(K995 - 4 + iloop - 1)))
      JS999 = max0(1,min0 (NS,(K999 - 4 + iloop - 1)))

      xxl(1,iloop)=100.0*CTMS(ip,idet,IVAL(JS50))/CMS(idet,IVAL(JS50))
      xxl(2,iloop)=100.0*CTMS(ip,idet,IVAL(JS90))/CMS(idet,IVAL(JS90))
      xxl(3,iloop)=100.0*CTMS(ip,idet,IVAL(JS95))/CMS(idet,IVAL(JS95))
      xxl(4,iloop)=100.0*CTMS(ip,idet,IVAL(JS98))/CMS(idet,IVAL(JS98))
      xxl(5,iloop)=100.0*CTMS(ip,idet,IVAL(JS99))/CMS(idet,IVAL(JS99))
      xxl(6,iloop)=100.0*CTMS(ip,idet,IVAL(JS995))/CMS(idet,IVAL(JS995))
      xxl(7,iloop)=100.0*CTMS(ip,idet,IVAL(JS999))/CMS(idet,IVAL(JS999))
      
      xmnl(1) = xmnl(1) + xxl(1,iloop)
      xmnl(2) = xmnl(2) + xxl(2,iloop)
      xmnl(3) = xmnl(3) + xxl(3,iloop)
      xmnl(4) = xmnl(4) + xxl(4,iloop)
      xmnl(5) = xmnl(5) + xxl(5,iloop)
      xmnl(6) = xmnl(6) + xxl(6,iloop)
      xmnl(7) = xmnl(7) + xxl(7,iloop)
      
      call sort format 1a (100.0*CMcontrib(ip,idet,1)/C(idet,1))   
      tenAV = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS50))/CMS(idet,
     &IVAL(JS50)))
      ten50 = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS90))/CMS(idet,
     &IVAL(JS90)))   
      if ( NS .gt. 49 ) ten90 = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS95))/CMS(idet,
     &IVAL(JS95)))   
      if ( NS .gt. 99 ) ten95 = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS98))/CMS(idet,
     &IVAL(JS98)))   
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS99))/CMS(idet,
     &IVAL(JS99)))   
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS995))/CMS(idet,
     &IVAL(JS995)))   
      if ( NS .gt. 499 ) ten995 = valchars10 
      call sort format 1a (100.0*CTMS(ip,idet,IVAL(JS999))/CMS(idet,
     &IVAL(JS999))) 
      if ( NS .gt. 4999) ten999 = valchars10
      
      if ( iloop .eq. 1 ) then
      write(140+idet,8842)uname(feeture)
 8842 format(140('-')/'Values for nine ranked Monte Carlo shots that',
     &' surround each percentile at: ',a37/140('-'))
      endif
      write(140+idet,14)nameprop(ip),
     &tenav,ten50,ten90,ten95,ten98,ten99,ten995,ten999,iloop 
   14 format("% from ",a33,8a10,i7)

      endif ! if ( CMcontrib(ip,idet,1) .gt. 0.000001 ) ===================
      enddo ! do iloop = 1,9 +++++++++++++++++++++++++++++++++++++++++++++++++++
      
      if ( CMcontrib(ip,idet,1) .gt. 0.000001 ) then
      do k = 1, 7
      do i = 1, 8
      do j = i + 1, 9
      if ( xxl(k,i) .ge. xxl(k,j) ) then
      xtemp = xxl(k,j)
      xxl(k,j) = xxl(k,i)
      xxl(k,i) = xtemp
      endif
      enddo
      enddo
      enddo
      
      call sort format 1a (xxl(1,5))   
      ten50 = valchars10  
      call sort format 1a (xxl(2,5))   
      if ( NS .gt. 49  ) ten90 = valchars10  
      call sort format 1a (xxl(3,5))   
      if ( NS .gt. 99  ) ten95 = valchars10  
      call sort format 1a (xxl(4,5))   
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1a (xxl(5,5))   
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1a (xxl(6,5))   
      if ( NS .gt. 999 ) ten995 = valchars10 
      call sort format 1a (xxl(7,5))   
      if ( NS .gt. 4999) ten999 = valchars10
      
      write(140+idet,64)nameprop(ip),
     &tenav,ten50,ten90,ten95,ten98,ten99,ten995,ten999 
   64 format("% from ",a33,8a10," median")
     
      do i = 1,7
      xmnl(i) = xmnl(i)/9.0
      enddo

      call sort format 1a (xmnl(1))   
      ten50 = valchars10  
      call sort format 1a (xmnl(2))   
      if ( NS .gt. 49  ) ten90 = valchars10  
      call sort format 1a (xmnl(3))   
      if ( NS .gt. 99  ) ten95 = valchars10  
      call sort format 1a (xmnl(4))   
      if ( NS .gt. 199 ) ten98 = valchars10  
      call sort format 1a (xmnl(5))   
      if ( NS .gt. 499 ) ten99 = valchars10  
      call sort format 1a (xmnl(6))   
      if ( NS .gt. 999 ) ten995 = valchars10 
      call sort format 1a (xmnl(7))
      if ( NS .gt. 4999) ten999 = valchars10

      write(140+idet,67)nameprop(ip),
     &tenav,ten50,ten90,ten95,ten98,ten99,ten995,ten999 
   67 format("% from ",a33,8a10,"   mean")
      endif ! if ( CMcontrib(ip,idet,1) .gt. 0.000001 ) ===================

      enddo ! do ip = 1, n2prop ++++++++++++++++++++++++++++++++++++++++++++++++
      
      return
      end
      
      
      
*     Sorts array VAL of NVAL values into ascending order of magnitude
*     The previous location of ranked values are stored in IVAL
*     VAL   - Values to be ranked
*     IVAL  - position of ranked values in pre-ranked VAL
*     NVAL  - number of values
      subroutine rank2 (VAL,IVAL,NVAL)
      include 'COMMON DATA.FOR'
      dimension VAL(NVAL),IVAL(NVAL)

      do I = 1, NVAL
      IVAL(I) = I
      enddo
      
      do 30 I = 1, NVAL-1
      XSTORE = VAL(I)
      ISTORE = IVAL(I)
      IX = I

      do J = I+1, NVAL
      if (VAL(J) .gt. XSTORE) then
      IX = J
      XSTORE = VAL(J)
      ISTORE = IVAL(J)
      endif
      enddo

      VAL(IX) = VAL(I)
      IVAL(IX) = IVAL(I)
      VAL(I) = XSTORE
      IVAL(I) = ISTORE
   30 continue

      return
      end

      
      
      subroutine use shots to get mean load for works 
     &(iworks,jdet,wloadm)
      include 'COMMON DATA.FOR'
      
      wconcm = 0.0 ! initialise mean concentration from this works
      wconcs = 0.0 ! standard deviation
      wloadm = 0.0 ! initialise mean load from this works
      wloads = 0.0 ! standard deviation
      
      do IS = 1, NS ! ----------------------------------------------------------
      
      xxcf = TELODEshots(iworks,jdet,IS) / FMS(IS) ! concentration -------------
      wconcm = wconcm + xxcf
      wconcs = wconcs + xxcf*xxcf
      wloadm = wloadm + TELODEshots(iworks,jdet,IS) 
      wloads = wloads + TELODEshots(iworks,jdet,IS) 
     &                * TELODEshots(iworks,jdet,IS)
      enddo ! do IS = 1, NS ----------------------------------------------------
      wconcs = (wconcs-wconcm*wconcm/NS)/(NS-1)
      wloads = (wloads-wloadm*wloadm/NS)/(NS-1)
      if (wconcs .gt. 1.0E-20) then
      wconcs = SQRoot(341099,wconcs)
      wloads = SQRoot(341099,wloads)
      endif ! if (wconcs .gt. 1.0E-20) 
      
      wconcm = wconcm / NS ! mean concentration from this works
      wloadm = wloadm / NS ! mean load from this works 

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