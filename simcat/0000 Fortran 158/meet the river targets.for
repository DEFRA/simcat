*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river --------------
*     ==========================================================================
*     File: TARGET Q95.FOR ... 1491 lines (185 lines of comments) --------------
*     --------------------------------------------------------------------------
*     This file compute effluent quality needed to meet the downstream river ---
*     target set as an annual mean ---------------------------------------------
*     --------------------------------------------------------------------------
*     This file contains 1 ...... -----------------------------------------
*     It is called:
*     --------------------------------------------------------------------------
*     ...... meet mean target in the river
*     --------------------------------------------------------------------------
      
      subroutine meet river targets set as averages
      include 'COMMON DATA.FOR'

      Mtargit = 1 ! the target is a mean --------------------------------------

*     prepare to check for the existence of reach-specific standards -----------
      IQSreach = EQS reach (IREACH,JP)
      
*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
      EC3 = 0.0 ! shift parameter (for 3-parameter log-normal distribution) ----
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX ! store the 95-percentile of effluent quality ------------
      XECM(JP) = ECM ! store the mean effluent quality -------------------------
      
      targit = 0.0 ! set the target - initialise as zero -----------------------
      classobj = 0
      IQSfeet = IFRQS(feeture) ! identify any target set for the feature -------
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( QTYPE (JP) .ne. 4 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) ! set target as Class 2
      classobj = 2
      endif
      
      if ( IQSfeet .gt. 0 ) then
      targit = RQS(IQSfeet,JP)
      if ( classobj .eq. 2 ) then
      do ic = 1, nclass
      xic = abs(class limmits (ic,JP)/targit)
      if ( xic .gt. 999 .and. xic .lt. 1.001 ) classobj = ic
      enddo 
      endif
      endif

      if ( skippeff(feeture) .ne. 9999 ) then ! effluent is not to be skipped ##
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-specific target
      targit = abs (class limmits (ic,JP))
      classobj = ic
      endif
      enddo ! do ic = 1, nclass
      endif ! if ( IQSreach .gt. 0 )
      endif ! if ( skippeff(feeture) .ne. 9999 ) ###############################
      
      RQO(JP) = targit ! use the target for graphs -----------------------------
      MRQO(JP) = MRQS(JP)
      kdecision(JP) = 2 ! target met - imposed river needs permit --------------

      endif ! if ( QTYPE (JP) .ne. 4 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) 
      
      
*     check for a zero river flow ----------------------------------------------
      if ( FLOW(1) .lt. 1.0e-8 ) then
      RATIO = EFM * ECM
      else
*     check for a trivial discharge --------------------------------------------
      RATIO = FLOW(1) * C(JP,1)
      if ( RATIO .gt. 1.0e-8 ) then
      RATIO = EFM * ECM / ( FLOW(1) * C(JP,1) )
      else
      RATIO = 0.0001
      endif ! if ( RATIO .gt. 1.0e-8 )
      endif ! if ( FLOW(1) .lt. 1.0e-8 )

      if ( RATIO .lt. 0.0001 ) then ! discharge is trivial =====================
      kdecision(JP) = 10 ! trivial discharge -- retain current discharge quality 
      if ( nobigout .le. 0 ) then
      call sort format 2 (ECM,ECS)
      write(01,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! -- OUT
      write(31,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! -- EFF
      write(150+JP,1586)DNAME(JP),valchars10,units(JP),valchars11, ! ---- Di EFF
     &units(JP)
 1586 format(110('-')/'Effluent load is too small to consider for ',A11,
     &' the current effluent quality has been retained .....'/110('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! trivial discharge - retain current discharge quality - 
      call get summaries of river quality from the shots ! trivial discharge ---
      return ! return to ...... effluent discharge -------- discharge is trivial
      endif ! if ( RATIO .lt. 0.0001 ) =========================================

      
*     check for no river quality target ========================================
      if ( targit .lt. 1.0e-8 ) then ! =========================================
      kdecision(JP) = 1 ! no target - retain current discharge quality ---------
      if ( nobigout .le. 0 ) then
      call sort format 2 (ECM,ECS)
      write(01,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(31,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! -- EFF
      write(150+JP,1486)DNAME(JP),valchars10,units(JP),valchars11, ! ---- Di EFF
     &units(JP)
 1486 format(77('-')/'No target for ',A11,' .... ',
     &'the current effluent quality has been retained'/77('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! no target -  retain current discharge quality --------
      call get summaries of river quality from the shots ! meet targets --------
      return ! return to ...... effluent discharge ----- discharge has no target
      endif ! if ( targit .lt. 1.0e-8 ) ========================================

      
*     the discharge is not trivial ---------------------------------------------
*     initialise the flag that will mark whether river target cannot be met ----
      IMPOZZ = 0
*     compute the effluent standard needed to meet the river target ------------
      if ( nobigout .le. 0 ) then
      call sort format 1 (targit)

      if ( detype(JP) .lt. 899) then ! +++++++++++++++++++++++++++++++++++++++++
      write(01,1705)DNAME(JP),valchars10,units(JP),uname(feeture) ! -------- OUT
      write(31,1705)DNAME(JP),valchars10,units(JP),uname(feeture)! --------- EFF
      write(150+JP,1705)DNAME(JP),valchars10,units(JP),uname(feeture) ! - Di EFF
 1705 format(/110('=')/'Assess the effluent quality needed to achieve ', ! - EFF
     &'the river target for ',A11/
     &'The target is a mean of: ',a10,1x,a4,
     &' ... downstream of ',a37/110('-'))
      else ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( detype(JP) .eq. 909 ) then ! the target is dissolved not total ======
      write(01,7705)DNAME(JP),valchars10,units(JP) ! ----------------------- OUT
      write(31,7705)DNAME(JP),valchars10,units(JP) ! ----------------------- EFF
      write(150+JP,7705)DNAME(JP),valchars10,units(JP) ! ---------------- Di EFF
 7705 format(/110('=')/'Assess the effluent quality needed to achieve ', ! -----
     &'the river target for ',A11/ ! -------------------------------------------
     &'The target is a mean of: ',a10,1x,a4,' (dissolved)'/110('-')) ! ---------
      endif ! if ( detype(JP) .eq. 999 ) === target is dissolved not total =====
*     ==========================================================================
      if ( detype(JP) .eq. 900 ) then ! the target is total not dissolved ======
      write(01,7745)DNAME(JP),valchars10,units(JP) ! ----------------------- OUT
      write(31,7745)DNAME(JP),valchars10,units(JP) ! ----------------------- EFF
      write(150+JP,7745)DNAME(JP),valchars10,units(JP) ! ---------------- Di EFF
 7745 format(/110('=')/'Assess the effluent quality needed to achieve ', ! - EFF
     &'the river target for ',A11/ ! -------------------------------------------
     &'The target is a mean of: ',a10,1x,a4,' (total)'/110('-')) ! -------------
      endif ! if ( detype(JP) .eq. 909 ) the target is total not dissolved =====
      endif ! if ( detype(JP) .lt. 899) ++++++++++++++++++++++++++++++++++++++++
      
      call get effluent quality 95 percentile

*     write out the details for determinands -----------------------------------
      call sort format 2 (C(JP,1),ECM) ! mean quality
      write(01,1954)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1954)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1954)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1954 format('Upstream river quality:          Mean =',a10,1x,a4,
     &4x,'Current discharge quality:     Mean =',a10,1x,a4)
      call sort format 2 (C(JP,2),pollution data(IQ,JP,2)) ! standard deviation
      write(01,1955)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1955)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1955)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1955 format(19x,'Standard deviation =',a10,1x,a4,
     &21x,'Standard deviation =',a10,1x,a4)
      call sort format 2 (C(JP,3),ECX) 
      write(01,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ OUT
      write(31,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1956 format(24x,'95-percentile =',a10,1x,a4,
     &26x,'95-percentile =',a10,1x,a4)
      call sort format 1 (C(JP,5)) 
      write(01,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP) ! ----------- OUT
      write(31,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP) ! ----------- EFF
      write(150+JP,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP) ! ---- Di EFF
 1959 format(24x,'99-percentile =',a10,1x,a4/110('-'))


      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7500)GIScode(feeture),unamex, ! --- u/s river flow --- Ei.CSV
     &rname(ireach),Flow(1),Flow(2)
 7500 format('X',a40,',',a40,',',a16,',',
     &'Upstream river flow',
     &',',(',',1pe12.4),(',,',1pe12.4),',,,402')
      write(130+JP,7501)GIScode(feeture),unamex, ! ----- u/s quality ---- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,3),JT(KFEAT),Mtargit
 7501 format('X',a40,',',a40,',',a16,',',
     &'Upstream river quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      write(130+JP,7502)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),EFM,EFS,JT(KFEAT),Mtargit
 7502 format('X',a40,',',a40,',',a16,',',
     &'Effluent flow',
     &',',a11,2(',',1pe12.4),',',2(',',i4),',402')
      write(130+JP,7503)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECS,ECX,JT(KFEAT),Mtargit
 7503 format('X',a40,',',a40,',',a16,',',
     &'Effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      write(130+JP,7504)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),targit,JT(KFEAT),Mtargit
 7504 format('X',a40,',',a40,',',a16,',',
     &'River quality standard (mean)',
     &',',a11,(',',1pe12.4),',,',2(',',i4),',402')
      endif
      endif

*     the discharge standard will be set between bounds of 'best' and 'worst' --
*     initilialise for this ----------------------------------------------------
      IBOUND = 0 ! set discharge standards within bounds -----------------------
      
      call meet mean target in the river (IMPOZZ,targit,IBOUND)
*     calculations complete ====================================================

      
      
      
*     ##########################################################################
*     options for when the river target is unachievable ########################
      if ( IMPOZZ .eq. 1 ) then ! ##############################################
*     target cannot be achieved by changing this effluent quality ++++++++++++++
*     check the current effluent quality +++++++++++++++++++++++++++++++++++++++
*     if it is quite good then keep it ... if not then improve to good quality +
      if ( ECM .gt. GEQ(1,JP) ) then ! #########################################
      if ( GEQ(1,JP) .gt. 1.0e-09 ) then ! check "good quality" has been entered
*     over-write calculated effluent quality with "good" quality +++++++++++++++
      TECM = GEQ(1,JP) ! mean "good" quality - river target unachievable +++++++
      TECS = GEQ(2,JP) ! standard deviation ++++++++++++++++++++++++++++++++++++
      TECX = GEQ(3,JP) ! 95-percentile +++++++++++++++++++++++++++++++++++++++++
      XEFF(JP) = TECX !
      XECM(JP) = TECM
      kdecision(JP) = 7 ! target not met - imposed specified good quality ++++++
      IMPOZZ = 77 ! impossible to meet the river target - impose good quality ++

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP), ! - EFF
     &valchars12,UNITS(JP)
      write(150+JP,7386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ---- Di EFF
     &UNITS(JP),valchars12,UNITS(JP)
 7386 format('The river target is unachievable for ',a11,4x,
     &'... imposed good effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))

      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7505)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7505 format('X',a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) then
      endif ! if ( nobigout .le. 0 )
      
*     compute the 95-percentile of effluent quality ----------------------------
      TECM = GEQ(1,JP) ! mean "good" quality -----------------------------------
      TECS = GEQ(2,JP) ! standard deviation ------------------------------------
      ECM = TECM
      ECS = TECS
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = TECM
      call load the upstream flow and quality 
      call mass balance ! calculating the effect of good effluent quality ------
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge ------------------------

      else ! when ( GEQ(1,JP) .le. 0.00001 ) ===================================
*     over-write calculated effluent quality with the current quality ----------

      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 8 ! target not met - retained current discharge quality --
      IMPOZZ = 77 ! impossible to meet the river target - retained current -----

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP), ! - EFF
     &valchars12,UNITS(JP)
      write(150+JP,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ---- Di EFF
     &UNITS(JP),valchars12,UNITS(JP)
 8386 format('The river target is unachievable for ',a11,1x,
     &'... retained current effluent quality:  Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
 7515 format('X',a40,',',a40,',',a16,',',
     &'Retained current effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) 
      endif ! if ( nobigout .le. 0 )
      
*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = ECM
      call load the upstream flow and quality 
      call mass balance ! retain current discharge quality
      call get summaries of river quality from the shots
      
      return ! return to ...... effluent discharge -----------------------------
      endif ! if ( GEQ(1,JP) .gt. 0.00001 ) ====================================
      
      else ! when ( ECM is already better than GEQ(1,JP) ) #####################
*     over-write the calculated effluent quality with the current quality ------
      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      
      call load the upstream flow and quality 
      call mass balance ! retain current discharge quality
      call get summaries of river quality from the shots
      
      kdecision(JP) = 8 ! target not met - retained current discharge quality --
      IMPOZZ = 77 ! ! impossible to meet the target - impose GOOD then current +

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP), ! - EFF
     &valchars12,UNITS(JP)
      write(150+JP,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ---- Di EFF
     &UNITS(JP),valchars12,UNITS(JP)
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )


*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = ECM
      call load the upstream flow and quality 
      call mass balance ! from meet river targets set as averages
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge ------------------------

      endif ! if ( ECM .gt. GEQ(1,JP) ) ########################################
      endif ! if ( IMPOZZ .eq. 1 ) #############################################
*     ##########################################################################
      
      
      
      
*     check that the selected effluent quality is technically feasible ---------
*     constrain the effluent standard to the best achievable where existing ----
*     quality is worse than this -----------------------------------------------
      if ( IMPOZZ .eq. 99 ) then
      kdecision(JP) = 3 ! target not met - imposed best achievable -------------
      
*     check that current quality is not better than the "best achievable" ------
      
      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,1582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,1582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ----------- EFF
     &valchars12,UNITS(JP)
      write(150+JP,1582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ---- Di EFF
     &valchars12,UNITS(JP)
 1582 format('Effluent quality is set as that defined as best',
     &' achievable ...',27x,'Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      endif
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7506)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
      write(130+JP,7506)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7506 format('X',a40,',',a40,',',a16,',',
     &'BEST achievable effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif
      return
      endif ! if ( IMPOZZ .eq. 99 ) --------------------------------------------

      
*     current quality is better than the "best achievable" ---------------------
      if ( IMPOZZ .eq. 95 ) then
      kdecision(JP) = 11 ! target not met - retained current quality -----------      
      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,7582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,7582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ----------- EFF
     &valchars12,UNITS(JP)
      write(150+JP,7582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ---- Di EFF
     &valchars12,UNITS(JP)
 7582 format('Current effluent quality retained ...',
     &' it is better than "best"',27x,'Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      endif
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7576)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
      write(130+JP,7576)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7576 format('X',a40,',',a40,',',a16,',',
     &'Retained current effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif
      return
      endif ! if ( IMPOZZ .eq. 95 ) --------------------------------------------

      
      
*     stop effluent quality being made too poor whilst 777777777777777777777-DET
*     still achieving the river targets 777777777777777777777777777777777777-DET
      if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) then ! 7777777777777777777777-DET
      kdecision(JP) = 4 ! target not threatened - imposed worst defined 7777-DET
      if ( nobigout .le. 0 ) then
      call sort format 2 (targit,TECM)
      write(01,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ OUT
      write(31,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) !  ---- Di EFF
 1388 format('Effluent quality could be very bad yet still meet ',
     &'the river target of ...',22x,a10,1x,a4/110('-')/
     &'Effluent quality will be limited to the worst you have ',
     &'specified ... ',20x,'Mean =',a10,1x,a4)
      call sort format 2 (TECS,TECX)
      write(01,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ OUT
      write(31,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 4388 format(75x,'Standard deviation =',a10,1x,a4/
     &80x,'95-percentile =',a10,1x,a4/110('-'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7507)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
 7507 format('X',a40,',',a40,',',a16,',',
     &'Worst permissible effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402') ! 777777777777777777777777777777-DET
      endif ! if ( ifeffcsv . eq. 1 ) ! 777777777777777777777777777777777777-DET
      endif ! if ( nobigout .le. 0 ) ! 7777777777777777777777777777777777777-DET
      
      
      ifdiffuse = 0
      
      call load the upstream flow and quality 
      
      call mass balance ! ??????????????????????? 
      
      call get summaries of river quality from the shots ! ??????????????
      
      return  ! 777777777777777777777777777777777777777777777777777777777777-DET
      endif ! if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) 777777777777777777777-DET

*     use special policy achieving river targets 888888888888888888888888888-DET
*     but allowing no deterioration 8888888888888888888888888888888888888888-DET
*     in river quality in cases where river is already inside  8888888888888-DET
*     the target with current effluent quality 88888888888888888888888888888-DET
      if ( ICAL .ne. 08 .and. ical .ne. 09 ) return ! 9999999999999999999999-DET
      
*     check the cases where the effluent quality is already 88888888888888888888
*     too good for the river quality target 888888888888888888888888888888888888
*     check whether the calculated effluent quality is laxer 8888888888888888888
*     than the current effluent quality 8888888888888888888888888888888888888888
      if ( pollution data (IQ,JP,1) .gt. TECM ) return ! 88888888888888888888888
*     the proposed effluent quality is laxer than current quality 88888888888888
*     check whether the deterioration in river quality is acceptable 88888888888
*     compute the river quality downstream of the current effluent quality 88888
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) 8888888888888888
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
*     call load the upstream flow and quality ! 88888888888888888888888888888888
      call get downstream river quality


      
*     ######################################################### SECTION NOT USED      
*     the rest of code in this ...... is NOT USED ############# SECTION NOT USED 
*     ######################################################### SECTION NOT USED     
*     check for deterioration less than a target factor such as 10% ### NOT USED
*     NOT USED at present ... needs u/s river quality to be reset ##### NOT USED
*     ######################################################### SECTION NOT USED      
      target factor = 0.00 ! currently set a zero ############# SECTION NOT USED
      target fraction = target factor * current ds quality ! ## SECTION NOT USED
      target gap = targit - current ds quality ! ############## SECTION NOT USED
      if ( target gap .ge. target fraction + 0.00000001) then ! SECTION NOT USED
*     deterioration exceeds the target factor ################# SECTION NOT USED
*     reset the target to give allowed deterioration ########## SECTION NOT USED
      if ( target factor .gt. 0.0 ) then ! #################### SECTION NOT USED
      targit = target fraction + current ds quality ! ######### SECTION NOT USED
      if ( nobigout .le. 0 ) then ! ########################### SECTION NOT USED
      call sort format 2 (current ds quality,targit)
      write(01,1852)valchars10,Units(JP),valchars11,Units(JP) 
      write(31,1852)valchars10,Units(JP),valchars11,Units(JP) ! ------------ EFF
      write(150+JP,1852)valchars10,Units(JP),valchars11,Units(JP) ! ----- Di EFF
 1852 format('Achieving the river target would permit a',
     &' deterioration in river quality ... ',
     &'this has been cut to 10 per cent'/
     &'The quality downstream of the current discharge is ',a10,1x,A4,
     &' ... the new target is  ',a10,1x,A4/110('-'))
*     ######################################################### SECTION NOT USED
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7508)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),current ds quality,Mtargit,
     &targit,Mtargit
 7508 format('X',a40,',',a40,',',a16,',',
     &'Current downstream quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'New mean target',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 ) ######################### SECTION NOT USED
      endif ! if ( nobigout .le. 0 ) ########################## SECTION NOT USED
*     calculate the required discharge quality for this 10% deterioration ######
*     this is TECX ... the discharge standard will not be set between bounds ###
      kdecision(JP) = 5 ! target not threatened - 10% deterioration ### NOT USED
      IBOUND = 1 ! set discharge standards without checking bounds #### NOT USED
      call meet mean target in the river (IMPOZZ,targit,IBOUND) ! 10% det ######
      if ( IBOUND .eq. 1 ) then ! do not look at bounds ####### SECTION NOT USED
      if ( nobigout .le. 0 ) then
      write(01,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP)
      write(31,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP) ! ------- EFF
      write(150+JP,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP) !- Di EFF
 1832 format(110('-')/
     &'Required river quality:           Mean =',F9.2,1x,A4,'  ',
     &'Required effluent quality         Mean =',F9.2,1x,A4/
     &81x,'95-PERCENTILE =',F9.2,1x,A4/110('-'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7509)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),targit,Mtargit,TECM,TECX,Mtargit
 7509 format('X',a40,',',a40,',',a16,',',
     &'Required mean river quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'Required effluent quality',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( IBOUND .eq. 1 ) do not look at bounds ###### SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     set 20% boost to current discharge pollution ############ SECTION NOT USED
      discharge factor = 1.20
      XCM = discharge factor * pollution data(IQ,JP,1)
      XCMKeep = XCM
      XCSKeep = discharge factor * pollution data(IQ,JP,2)
*     ######################################################### SECTION NOT USED
*     check whether the new discharge quality exceeds this #### SECTION NOT USED
      if (TECM .gt. XCM) goto 1892 ! ########################## SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the river target is met ################################# SECTION NOT USED
*     if this is done the deterioration in the river exceeds 10% ###### NOT USED
*     so the effluent standard has been re-calculated to give a 10% ### NOT USED
*     deterioration in river quality ########################## SECTION NOT USED
*     change allows less than a 20% deterioration in effluent quality # NOT USED
*     the calculation of the discharge quality is finished #### SECTION NOT USED
      return
      endif ! if ( target factor .gt. 0.0 ) target deterioration ###### NOT USED
*     the river target is achieved ############################ SECTION NOT USED
*     new effluent standard is tighter than the old or the deterioration =======
*     in the river is less than 10% .... and the deterioration embodied in the =
*     new effluent standard is less than 10% ################## SECTION NOT USED
      endif ! if ( target gap .ge. target fraction + 0.00000001) ###### NOT USED
      return ! ################################################ SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the calculated standard is laxer than the current effluent ###### NOT USED
*     quality, but the deterioration in river quality is acceptable ### NOT USED
*     (less than 10%) ######################################### SECTION NOT USED
*     nonetheless the effluent standard will be constrained to a 20% ## NOT USED
*     deterioration from the current effluent quality ######### SECTION NOT USED
 1892 continue ! ############################################## SECTION NOT USED
*     ######################################################### SECTION NOT USED

      
      
      
      ECM1 = ECM
      ECX1 = ECX
      ECM = XCMKEEP
      ECS = XCSKEEP
*     compute the percentile of effluent quality +++++++++++++++++++++++++++++++
      call get effluent quality 95 percentile
      TECM = ECM ! mean
      TECS = ECS ! standard deviation
      TECX = ECX ! 95-percentile
      XEFF(JP) = TECX ! 95-percentile
      XECM(JP) = TECM ! mean
      kdecision(JP) = 6 ! target not threatened - 10% deterioration ++++++++++++
      if ( nobigout .le. 0 ) then
      write(01,1983)ECM,UNITS(JP),ECX,UNITS(JP)
      write(31,1983)ECM,UNITS(JP),ECX,UNITS(JP) ! -------------------------- EFF
      write(150+JP,1983)ECM,UNITS(JP),ECX,UNITS(JP) ! ------------------- Di EFF
 1983 format(
     &'Achieving the river target or a 10% deterioration would',
     &' permit too bad an effluent quality ...'/110('-')/
     &'Deterioration in effluent quality held at 20% ...',
     &6x,' Resulting effluent quality ...    Mean =',F9.2,1x,A4/
     &72x,'         95-PERCENTILE =',F9.2,1x,A4/110('-'))
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7511)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECX,Mtargit
 7511 format('X',a40,',',a40,',',a16,',',
     &'Deterioration in effluent quality held at 20%',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      return
      end

      
      
*     this routine takes flow and quality data for an effluent and works out ---
*     the effluent quality needed to achieve a set mean of river quality -------
*     downstream ---------------------------------------------------------------
      subroutine meet mean target in the river
     &(IMPOZZ,TRQS,IBOUND)
      include 'COMMON DATA.FOR'
      dimension EQ(NS),RQ(MS),UQ(NS),FQ(NS),UF(NS)
      dimension RLC(nprop)
      character *13 SET1,SET2

      Mtargit = 1 ! the target is a mean ---------------------------------------

      do is = 1, NS
      eq(is) = 0.0
      rq(is) = 0.0
      uq(is) = UCMS(JP,IS) ! load the upstream river quality
      fq(is) = 0.0
      uf(is) = UFMS(IS) ! load the upstream river flow
      do ip = 1, n2prop
      UCTMS(ip,JP,IS) = CTMS(ip,JP,IS )
      enddo
      enddo

*     check for a trivial effluent flow ----------------------------------------
      if ( EFM .lt. 1.0E-15 ) return ! no calculation for trivial diacharge ----
      if ( IMPOZZ .eq. 77 ) goto 8888

*     calculated effluent quality needed to secure river target ----------------
*     initially, set this equal to the input data on discharge quality ---------
*     mean and standard deviation of discharge quality -------------------------
      TECM = pollution data (IQ,JP,1)
      TECS = pollution data (IQ,JP,2)

*     the coefficient of variation ---------------------------------------------
      ECV = 1.0
      if (tecm .gt. 1.0e-8) ECV = TECS/TECM

      call get the summary statistics of discharge flow
      
*     the target river quality is TRQS -----------------------------------------
*     compute the statistics of the upstream river quality ---------------------
      call get summaries of river quality from the shots !-- in meet mean target  
      
*     set the mean, standard deviation and 95-percentile -----------------------
      RCM = C(JP,1) ! mean upstream river quality
      RCS = C(JP,2) ! standard deviation for upstream river quality
      RCV = 0.6 ! the coefficient of variation
      if ( RCM .gt. 0.00001 ) RCV = RCS / RCM  ! the coefficient of variation
      if ( kptarg .eq. 95 ) RCX = C(JP,3) ! the 95-percentile 
*     --------------------------------------------------------------------------
      
      
      
*     For Mode 9 calculations of action to meet river targets 999999999999999999 
*     upstream quality is set at the middle of the class -----------------------
      if ( ical .eq. 09 .and. classobj .gt. 0 ) then ! this is mode 9 ----------
      xlim1 = 0.0
      xlim2 = abs( class limmits (classobj,JP) ) !     upper class limit
      xlim1 = abs( class limmits (classobj - 1,JP) ) ! lower class limit
      xgap = xlim2 - xlim1 
      if ( abs (xgap) .lt. 1.0e-07 ) then
      classobj = 0    
      else
      RCM = xlim1 + classfraction * xgap ! mean for middle of class
      RCS = RCV * RCM ! corresponding standard deviation
      call ASSEM (xlim1,xlim2,SET1)
      write(01,3000)SET1,RCM,RCS
      write(31,3000)SET1,RCM,RCS ! ----------------------------------------- EFF
      write(150+JP,3000)SET1,RCM,RCS ! ---------------------------------- Di EFF
 3000 format(110('=')/
     &'Upstream quality is imposed as the middle of the target class ',
     &'(Run Type 9) ... Limits: ',a13/110('=')/
     &19x,'              Mean =',f10.3/
     &19x,'Standard deviation =',f10.3/
     &110('='))
*     create the river quality data 99999999999999999999999999999999999999999999
      call generate mode 9 river quality (RCM,RCS)
      endif ! if ( abs (xgap) .lt. 1.0e-07 )
      endif ! if ( ical .eq. 09 .and. classobj .gt. 0 ) 99999999 Mode 9 99999999
     
      call get summaries of loads ! of river quality ======= in meet mean target

      RFM = FLOW(1) ! the mean upstream river flow =============================

      
*     compute need for improvement to discharge quality ------------------------
      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then ! 78978978978  
      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)
      endif ! if ( IQDIST .lt. 4 )

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) then ! -----
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) ----
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

*     calculate the effect of current discharge quality ========================
      do 2002 IS = 1,NS ! loop through shots ===================================
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4)
      RF = FMS(IS) ! river flow
      if ( ical .eq. 09 ) then ! 99999999999999999999999999999999999999999999999
      RC = CMS(JP,IS) ! 99999999999999999999999999999999999999999999999999999999
      else ! 9999999999999999999999999999999999999999999999999999999999999999999
      RC = CMS(JP,IS)
      endif ! 999999999999999999999999999999999999999999999999999999999999999999

*     get the shots for the discharge ------------------------------------------
      call get a flow for the stream or discharge (IS, R3, EF)

*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL

*     set the flows to 1.0 -----------------------------------------------------
      EFMB = 1.0
      endif ! if ( IQDIST .eq. 6 ... etc ---------------------------------------

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ (IS) = EC

*     calculate the effect of current discharge quality ------------------------
*     perform the mass balance calculation -------------------------------------
      if ( RF + EF .gt. 1.0e-8 ) then
      RQ (IS) = (RF*RC+EFMB*EQ(IS))/(RF+EF)
      fq (is) = RF + EF
      endif
 2002 continue ! do 2000 IS = 1,NS =============================================
      

      TRCX = get the mean river quality (RQ) ! mean downstream river quality
      TECX = get the mean discharge quality (EQ) ! mean discharge quality

      XEFF(JP) = TECX ! mean discharge quality
*     XECM(JP) = TECM

      if ( nobigout .le. 0 ) then
      call sort format 2 (TRCX,TECX)
      
      
      if ( ical .eq. 9 .and. classobj .gt. 0 ) then ! ==========================
      write(01,3964)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3964)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,3964)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 3964 format('River quality provided:          Mean =',a10,1x,A4,
     &'    by current discharge quality:  Mean =',a10,1x,A4/110('='))
      else ! for ical .eq. 7 or 8 ==============================================
      write(01,3963)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3963)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,3963)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 3963 format('River quality provided:          Mean =',a10,1x,A4,
     &'    by current discharge quality:  Mean =',a10,1x,A4/110('-'))
      endif ! ==================================================================
      
      
      if ( ifeffcsv .eq. 1 ) then ! write CSV file =============================
      write(130+JP,7522)GIScode(feeture),unamex, ! -- current dischage -- Ei.CSV
     &rname(ireach),dname(JP),TRCX,JT(KFEAT),Mtargit
 7522 format('X',a40,',',a40,',',a16,',',
     &'Mean river quality from current discharge', !  mean river target - Ei.CSV
     &',',a11,(',',1pe12.4),',,',2(',',i4),',402')     
      endif ! if ( ifeffcsv .eq. 1 ) ===========================================
      endif ! if ( nobigout .le. 0 )

      
*     check whether current discharge quality meets the river target ===========
      icurrent = 0
      if ( ical .eq. 08 .or. ical .eq. 09 ) then ! for Mode 9 or Mode 9 ========
      if ( TRCX .le. TRQS ) then ! if target is already met ==================== 

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then ! --------------------------
      write(01,892)
      write(31,892) ! ------------------------------------------------------ EFF
      write(150+JP,892) ! ----------------------------------------------- Di EFF
  892 format('The river quality target is achieved without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('-'))
      else ! if ( ical .eq. 08 etc ---------------------------------------------
      write(01,894)
      write(31,894) ! ------------------------------------------------------ EFF
      write(150+JP,894) ! ----------------------------------------------- Di EFF
  894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('='))
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 ) -------------------------
      endif ! if ( nobigout .le. 0 )
      icurrent = 1 ! retained the current discharge quality --------------------
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      
      goto 9095 ! river target is achieved with current discharge quality ------
      
*     if it is quite good then keep it ... if not then improve to good quality %
          
      if ( ECM .gt. GEQ(1,JP) .and. ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &   GEQ(1,JP) .gt. 1.0e-09 ) then ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*     over-write calculated effluent quality with "good" quality %%%%%%%%%%%%%%%
      TECM = GEQ(1,JP) ! mean "good" quality specified for this pollutant
      TECS = GEQ(2,JP) ! standard deviation
      TECX = GEQ(3,JP) ! 95-percentile
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 13 ! target not met - imposed specified good quality ------

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then ! 88888888888888888888888888
      write(01,2386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ OUT
*    &valchars12,UNITS(JP)
      write(31,2386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
*    &valchars12,UNITS(JP)
      write(150+JP,2386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
*    &valchars12,UNITS(JP)
 2386 format(
     &'The river target is already being achieved ',4x,
     &'...... imposed "good" effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('-'))
      else
      write(01,3386)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,3386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 3386 format(
     &'The river target is already being "achieved" ',2x,
     &'...... imposed "good" effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('-'))
          
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 ) 8888888888888888888888888
      
      if ( ifeffcsv . eq. 1 ) then ! write results to the CSV file =============
      write(130+JP,2505)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,JT(KFEAT),MRQS(JP)
 2505 format('X',a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,2(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) ==========================================
      endif ! if ( nobigout .le. 0 ) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      else ! when ( GEQ(1,JP) .le. 0.00001 and ECM .gt. GEQ(1,JP) ) then ! %%%%%
      if ( nobigout .le. 0 ) then ! ============================================
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(01,4892) ! ----------------------------------------------------- OUT
      write(31,4892) ! ----------------------------------------------------- EFF
      write(150+JP,4892) ! ---------------------------------------------- Di EFF
 4892 format('The river quality target is achieved without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('-'))
      else
      write(01,4894)
      write(31,4894) ! ----------------------------------------------------- EFF
      write(150+JP,4894) ! ---------------------------------------------- Di EFF
 4894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('='))
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( nobigout .le. 0 ) ===========================================
      icurrent = 1 ! retained current discharge qulaty -------------------------
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      goto 9095 ! target achieved with current discharge quality ---------------
      endif ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      goto 9095 ! imposed "good" discharge quality %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      endif ! if ( TRCX .le. TRQS )
      endif ! if ( ical .eq. 08 or 09 ) ! ======================================
      endif ! if ( ical .eq. 07, 08 or 09 ) ! 7897897897897897897878978978978978
*     78978978978978978978789789789789787897897897897897897878978978978978789789
      

      
      
      
      IDIL = 0 ! initialise switch for dealing with unachievable targets
      KDIL = 0 ! initialise switch for dealing with unachievable targets
      if ( RCM .gt. 1.0E-20 ) then
*     is the river target stricter than the current upstream river quality ? ---
      if ( RCM .ge. TRQS ) then
      if ( detype (jp) .ne. 104 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (RCM,TRQS)
      write(01,764)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! --- EFF
      write(31,764)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! --- EFF
      write(150+JP,764)DNAME(JP),valchars10,units(JP), ! ---------------- Di EFF
     &valchars11,units(JP) ! -------------------------------------------- Di EFF
  764 format(
     &'WARNING - Upstream river quality for ',a11,' ... ',13x,a10,1x,a4/
     &'is worse than the quality required downstream of the ',
     &'discharge ...',a10,1x,a4/110('-'))
      endif ! if ( nobigout .le. 0 )
*     set the switches ---------------------------------------------------------
      IDIL = 1 ! prepare to try zero quality for the discharge
      KDIL = 1 ! prepare to try zero quality for the discharge
*     prepare to try zero quality for the discharge ----------------------------
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 382
      endif ! if ( detype (jp) .ne. 104 )
      endif ! if ( RCM .ge. TRQS )

      endif ! if ( RCM .gt. 1.0E-20 )
      TRCM = TRQS
      

*     compute the first guess of the required discharge quality ----------------
      TECM = (TRCM * (RFM + EFM) - (RCM * RFM)) / EFM
      if (TECM .lt. 1.0E-10) then
      IDIL = 1 ! prepare to try zero quality for the discharge
      KDIL = 1 ! prepare to try zero quality for the discharge
*     prepare to try zero quality for the discharge ----------------------------
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      endif ! if (TECM .lt. 1.0E-10)
      TECS = ECV*TECM
  382 continue

*     revert to a log-normal distribution --------------------------------------
      IQDIST = 2

*     iteration counter --------------------------------------------------------
      ITER = 0 ! initialise the iteration counter
*     set initial mean quality downstream of the effluent discharge ------------
      TRCX = 1.0E-7
*     starting set of data for the iteration scheme ----------------------------
      TECM0 = RCM
      TRCX0 = RCM
      TECX0 = RCM
      TECM1 = 0.0
      TECX1 = 0.0
      TRCX1 = 0.0

  996 ITER = ITER + 1 ! number of the next iteration 

      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      !call bias in river or discharge flow and quality (TECM,TECS)
      endif
      

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) then ! -----
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) ----
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

      do 2 IS = 1,NS ! =========================================================
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads ---------
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4)
      RF = FMS(IS) ! upstream flow
      RC = CMS(JP,IS) ! upstream quality

*     get the shots for the discharge ------------------------------------------
      call get a flow for the stream or discharge (IS, R3, EF)  
      EFMB = EF
      
*     deal with data expressed as load -----------------------------------------
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
*     set the flows to 1.0 where quality is expressed as load ------------------
      EFMB = 1.0
      endif ! deal with data expressed as load ---------------------------------

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC ! effluent quality for this shot

*     mass balance +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( RF + EF .gt. 1.0e-8 ) then
      RQ(IS) = (RF*RC+EFMB*EQ(IS))/(RF+EF) ! downstream quality ++++++++++++++++
      fq(is) = RF + EF ! downstream flow
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    2 continue ! do 2 IS = 1, NS ===============================================

      TRCX = get the mean river quality (RQ)
      TECX = get the mean discharge quality (EQ)
      XEFF(JP) = TECX ! mean discharge quality

    
*     ==========================================================================
      if ( IDIL .eq. 1 ) then ! a discharge quality of zero has been tried =====
*     a discharge quality of zero has been tried -------------------------------
*     check whether this achieved the target river quality ---------------------
      if ( TRCX .ge. TRQS ) then
      IMPOZZ = 1 ! the river target is unachievable ----------------------------
      kdecision(JP) = 7 ! target not met - imposed specified good quality ------
      TECM = GEQ(1,JP)
      TECS = GEQ(2,JP)
      TECX = GEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      if ( detype (JP) .ne. 104 ) then
      if ( nobigout .le. 0 ) then
*     write(01,802)dname(JP),uname(feeture) ! ------------------------------ OUT
*     write(31,802)dname(JP),uname(feeture) ! ------------------------------ EFF
*     write(150+JP,802)dname(JP),uname(feeture) ! ----------------------- Di EFF
      write(33,802)dname(JP),uname(feeture) ! ------------------------------ ERR
  802 format(/77('-')/'The river quality target for ',a11,
     &' is not achievable without '/
     &'improving river quality upstream of the discharge 'a35/77('-'))
      endif
      endif
      goto 8888 ! if ( IDIL .eq. 1 ) ... unachievable target ===================
      endif
      IDIL = 0 ! initialise switch for dealing with unachievable targets
      endif ! if ( IDIL .eq. 1 ) ===============================================
*     ==========================================================================

      
*     test for convergence -----------------------------------------------------
      CONV = ABS ( (TRCX-TRQS) / TRCX)
      if  (CONV .lt. 0.0001 .and. ITER .gt. 2 ) goto 9095 ! convergence achieved
      
      if ( ITER .gt. 60 ) then ! convergence has not been achieved -------------
      if ( nobigout .le. 0 ) then
      write(01,6000)
      write(03,6000)
      write(09,6000)
      write(33,6000)
      write(31,6000) ! ----------------------------------------------------- EFF
      write(150+JP,6000) ! ---------------------------------------------- Di EFF
 6000 format(110('-')/
     &'Unable to calculate effluent quality ',
     &'that meets the river target ... ',
     &'current effluent quality retained'/110('-'))
*     set quality equal to the current quality ---------------------------------
      ECM = pollution data(IQ,JP,1) ! set mean duscharge quality
      ECS = pollution data(IQ,JP,2)
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX
      TECM = ECM
      TECS = ECS
      XEFF(JP) = TECX
      XECM(JP) = TECM
      endif
      goto 9095 ! quit the iterations ------------------------------------------
      endif ! if ( ITER .gt. 60 ) convergence has not been achieved ------------

      
      if ( ITER .gt. 1 ) goto 890 ! proceed to next iteration 

*     set-up or replace the second set -----------------------------------------
  893 TECM1 = TECM
      TECX1 = TECX
      TRCX1 = TRCX 
      
      goto 891
  890 continue

*     SECOND iteration - store result as second trial --------------------------
      if ( ITER .eq. 2 ) goto 174

*     find which set is nearest ------------------------------------------------
*     replace worst set with the new result ------------------------------------
      if (ABS(TRCX0-TRQS) .lt. ABS(TRCX1-TRQS)) goto 893

*     replace the first set ----------------------------------------------------
  174 continue
      TECM0 = TECM
      TRCX0 = TRCX
      TECX0 = TECX

*     compute the next guess for discharge quality -----------------------------
  891 continue
      TRDEM = TRCX1 - TRCX0
      if (TRDEM .gt. 1.0E-06 .or. TRDEM .lt. -1.0E-06) goto 6432
      TECM = 2.0*TECM
      goto 183
 6432 continue
      TECM = ABS(TECM0+(TRQS-TRCX0)*(TECM1-TECM0)/TRDEM)
      if ( TECM .gt. 0.0 ) goto 183
      TECM = 0.5*AMIN1(TECM0,TECM1)
      if ( TECM .gt. 0.0 ) goto 183

*     mark potential that standard cannot be met ===============================
      if ( KDIL .le. 0 ) then
      IDIL = 1 ! mark potential that standard cannot be met 
      KDIL = 1 ! mark potential that standard cannot be met 
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 996 ! proceed with the next iteration
      endif

      TECM = 0.5*(TECM0+TECM1)

*     test for unstable convergence ============================================
      if (TECM .le. 0.0) then ! ================================================
      write(01,6010)
      write(33,6010)
      write(31,6010) ! ----------------------------------------------------- EFF
      write(150+JP0) ! -------------------------------------------------- Di EFF
 6010 format(//'*** Unstable convergence in [meet mean standard in ',
     &'the river] ...'/'*** Returned current quality ...')
      write(01,997)
      write(31,997) ! ------------------------------------------------------ EFF
      write(150+JP,997) ! ----------------------------------------------- Di EFF
  997 format(110('T'))
      TECM = ECM
      TECS = ECS
      call get effluent quality 95 percentile
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 9095 
      endif ! if (TECM .le. 0.0) ===============================================
      

*     compute standard deviation of effluent quality ---------------------------
*     ... for the next iteration ... -------------------------------------------
  183 TECS = ECV * TECM ! set the standard deviation

      if ( ITER .eq. 1 ) goto 996 ! proceed to the next iteration
*     check whether calculated discharge quality is infeasibly bad =============
      if ( TECM .gt. 300000.0 ) then
      suppress9d = suppress9d + 1
      if ( suppress9d .lt. 6 ) then
      call change colour of text (20) ! bright red
      write( *,8463)uname(feeture),dname(JP)
 8463 format(
     &'*** Calculated discharge quality is huge',11x,'...',7x,
     &'at: ',a37,3x,'for ',a11,10('.'))
      call set screen text colour
      else
      if ( suppress9d .gt. 9999 ) suppress9d = 0
      endif
      if ( suppress9d .eq. 6 ) then
      call change colour of text (20) ! bright red
      write( *,3463)uname(feeture)
 3463 format(
     &'*** Calculated discharge quality is huge',11x,'...',7x,
     &'at: ',a37,3x,'and other cases .........')
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) then
      write(01,8763)units(JP),dname(jp),TRQS,UNITS(JP),TECM,UNITS(JP),
     &TRCX,UNITS(JP)
      write(31,8763)units(JP),dname(jp),TRQS,UNITS(JP),TECM, ! ------------- EFF
     &UNITS(JP),TRCX,UNITS(JP)
      write(150+JP,8763)units(JP),dname(jp),TRQS,UNITS(JP),TECM, ! ------ Di EFF
     &UNITS(JP),TRCX,UNITS(JP)
 8763 format(110('-')/'The calculated discharge quality is bigger ',
     &'than 300000 ',a4,' ... for ',a11/
     &110('-')/'Target river quality:',13x,'MEAN =',F9.2,1x,A4,
     &'  Required discharge quality:       Mean =',F9.0,1x,A4/
     &'Achieved river quality:',4x,'       Mean =',F9.2,1x,A4/110('-'))
      endif

      write(33,8763)dname(jp),TRQS,UNITS(JP),TECM,UNITS(JP),
     &TRCX,UNITS(JP)
      goto 9095 
      else ! if ( TECM .lt. 300000.0 )
*     go to the next iteration =================================================
      goto 996
      endif ! if ( TECM .gt. 300000.0 ) ========================================

      

 9095 continue ! iterations complete - convergence complete - mean targets -----
      
      goto (4291,4291,9996,9999,4291,9996),QTYPE(JP)
 4291 continue
      ECM = TECM
      ECS = TECS

      call get effluent quality 95 percentile
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM

*     special upstream river quality for Mode 9 99999999999999999999999999999999
      if ( ical .eq. 09 ) then
      call load the upstream flow and quality ! 99999999999999999999999999999999
      endif ! 999999999999999999999999999999999999999999999999999999999999999999
            
      do is = 1, NS
      CMS(JP,is) = UCMS(JP,is)
      enddo

      call mass balance ! calculate loads --------------------------------------

      call calculate summaries of river flow


*     for a mean river quality standard ---------------------------------------- 
      if ( nobigout .le. 0 .and. icurrent .eq. 0 ) then ! ######################

      qualnuse = PNUM(IQ,JP) 
      call compute confidence limits 
     &(0.0,iqdist,TECM,TECS,0.0,qualnuse,testql,testqu)
      call ASSEM (testql,testqu,SET1)
      call compute confidence limits 
     &(95.0,iqdist,TECM,TECS,TECX,qualnuse,testql,testqu)
      call ASSEM (testql,testqu,SET2)

      call sort format 2 (Trqs,TECM)
      write(01,4963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET1
      write(31,4963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET1 ! ------- EFF
      write(150+JP,4963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET1 !  Di EFF
 4963 format('TARGET river quality:',12x,'Mean =',a10,1x,A4,2x,
     &'Required discharge quality:'6x'Mean =',a10,1x,A4,4x,a13)
      call sort format 1 (TECS)
      write(01,7963)valchars10,UNITS(JP)
      write(31,7963)valchars10,UNITS(JP) ! --------------------------------- EFF
      write(150+JP,7963)valchars10,UNITS(JP) ! -------------------------- Di EFF
 7963 format(65x,'Discharge standard deviation =',a10,1x,a4)
      call sort format 2 (C(JP,1),TECX)
      if ( ical .ne. 09 ) then ! ===============================================
      write(01,5963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2
      write(31,5963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2 ! ------- EFF
      write(150+JP,5963)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2 !- Di EFF
 5963 format('Achieved river quality:',10x,'Mean =',a10,1x,A4,2x,
     &14x,'Discharge 95-percentile =',a10,1x,A4,4x,a13/110('='))
      xm = 0.0
      do is = 1,NS
      RQ(is) = CMS(JP,is)
      xm = xm + CMS(JP,is)
      enddo
      xm = xm/float(NS)
      
      else ! if ( ical .ne. 09 ) ie eq. 09 =====================================
      write(01,5964)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2
      write(31,5964)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2 ! ------- EFF
      write(150+JP,5964)valchars10,UNITS(JP),valchars11,UNITS(JP),SET2 !- Di EFF
 5964 format('"Achieved" river quality:',8x,'Mean =',a10,1x,A4,2x,
     &14x,'Discharge 95-percentile =',a10,1x,A4,4x,a13/110('='))
      endif ! if ( ical .ne. 09 ) ==============================================


      
      if ( ifeffcsv .eq. 1 ) then ! ============================================
      write(130+JP,7512)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),Trqs,JT(KFEAT),MRQS(JP)
 7512 format('X',a40,',',a40,',',a16,',',
     &'Target mean river quality',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      write(130+JP,7513)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,JT(KFEAT),MRQS(JP)
 7513 format('X',a40,',',a40,',',a16,',',
     &'Required discharge quality',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      write(130+JP,7514)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),JT(KFEAT),Mtargit
 7514 format('X',a40,',',a40,',',a16,',',
     &'Achieved mean river quality',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECX,JT(KFEAT),Mtargit
 7515 format('X',a40,',',a40,',',a16,',',
     &'Discharge 95-percentile',
     &',',a11,1(',,,',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv .eq. 1 ) ===========================================
      endif ! if ( nobigout .le. 0 .and. icurrent .eq. 0 ) #####################
      
      icurrent = 0

*     check the need to set discharge standard between the specified upper +++++
*     and lower bounds +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 9995 if ( IBOUND .eq. 1 ) then ! do not check bounds ++++++++++++++++++++++++++
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 8888
      endif ! do not check bounds ++++++++++++++++++++++++++++++++++++++++++++++

      if ( Kdecision(JP) .eq. 9 ) goto 9996
      
*     set standards within BOUNDS ==============================================     
*     check standard is compatible with the boundaries set for good and bad ----
*     discharge quality --------------------------------------------------------
      if ( TECX .gt. WEQ(3,JP) .or. TECM .gt. WEQ(1,JP) ) then
      TECM = WEQ(1,JP) ! reset mean to the worst quality
      TECS = WEQ(2,JP)
      TECX = WEQ(3,JP) ! reset 95-percentile to the worst quality

      ECM = TECM 
      ECS = TECS
      XEFF(JP) = TECX
      XECM(JP) = TECM

      IMPOZZ = 98 ! used only in Run Type 7 -----------------------------------
      goto 8888 ! re-calulate river quality after over-riding discharge quality
      endif ! if ( TECX .gt. WEQ(3,JP) ----------------------------------------

*     check whether effluent quality is better than best possible --------------
      if ( detype (JP) .ne. 104 ) then
      if ( TECM .gt. BEQ(1,JP) ) goto 1633
      TECM = BEQ(1,JP)
      TECS = BEQ(2,JP)
      TECX = BEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 99 ! effluent quality is better than best possible
      
      if ( ECM .lt. TECM ) then
      TECM = ecm
      TECS = ecs
      TECX = ecx
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 95 ! current effluent quality is better "best"
      endif
      
      else ! if ( detype (JP) = 104 )
      if (TECM .lt. BEQ(1,JP)) goto 1633
      TECM = BEQ(1,JP)
      TECS = BEQ(2,JP)
      TECX = BEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 99 ! effluent quality is better than best possible
      
      if ( ECM .gt. TECM ) then
      TECM = ecm
      TECS = ecs
      TECX = ecx
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 95 ! current effluent quality is better "best"
      endif

      endif ! if ( detype (JP) .ne. 104 ) --------------------------------------
      goto 8888 ! re-calulate river quality after over-riding discharge quality- 
      
      

 1633 TECX = Get effluent percentile (EQ)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 9996   

      
*     ==========================================================================
*     re-calulate river quality after over-riding the values calculated to =====
*     the target river quality =================================================
 8888 continue ! re-calculate river quality after over-riding the values =======
      
      do is = 1, NS
      CMS(jp,is) = UQ(IS) ! load the upstream river quality
      FMS(IS) = UF(IS) ! load the upstream river flow
      do ip = 1, n2prop
      CTMS(ip,JP,IS) = UCTMS(ip,JP,IS) ! concentrations from types of feature ---
      enddo
      enddo

      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)

*     set up the correlation ....  (also done in BIAS ) ========================
      if ( IQDIST .gt. 4 .and. QTYPE (jp) .ne. 4 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif

      call inititialise the stores for the estimates of load 
      call set up data for added flow and quality

      ELOAD (JP,I13) = 0.0
      ELOADS (JP) = 0.0

      do 2222 IS = 1,NS
*     the contributions from different sources of pollution ====================
      do ip = 1, n2prop
      RLC(ip) = CTMS(ip, JP, IS )
      enddo

      imonth = qmonth (is) ! set the month for this shot
*     prepare for monthly structure input of loads =============================
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads ---------
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4)
      RF = FMS(IS)
      RC = CMS(JP,IS)

      call get a flow for the stream or discharge (IS, R3, EF)
*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
      EFMB = 1.0 ! set the flows to 1.0 ----------------------------------------
      endif

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC

*     mass balance for pruning =================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      CMS(JP,IS) = ( RF * RC + EFMB * EQ(IS) ) / (RF + EF) ! pruning
      RQ(IS) = CMS(JP,IS)
      fq(IS) = RF + EF
      dfms(IS) = RF + EF ! store the downstream flow for extra calculations ----
      endif

*     concentrations from various types of feature *****************************
*     point source inputs ======================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      if ( ifdiffuse .eq. 0 ) then
*     apply to all discharges (3) (5) (12) (39) (60) (61) ------ load categories
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61) then
      CTMS(1,JP,IS) = ( (RF * RLC(1)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(1,JP,IS) = ( (RF * RLC(1)) ) / (RF + EF)
      endif
*     apply to sewage works (3) -------------------------------- load categories
      if ( JT(feeture) .eq. 03) then
      CTMS(2,JP,IS) = ( (RF * RLC(2)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(2,JP,IS) = ( (RF * RLC(2)) ) / (RF + EF)
      endif
*     apply to intermittent discharges (12) -------------------- load categories
      if ( JT(feeture) .eq. 12) then
      CTMS(3,JP,IS) = ( (RF * RLC(3)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(3,JP,IS) = ( (RF * RLC(3)) ) / (RF + EF)
      endif
*     apply to industrial discharges (5) ----------------------- load categories
      if ( JT(feeture) .eq. 05 ) then
      CTMS(4,JP,IS) = ( (RF * RLC(4)) + (EFMB * EC) ) / (RF + EF) ! - industrial
      else
      CTMS(4,JP,IS) = ( (RF * RLC(4)) ) / (RF + EF) ! --------------- industrial
      endif
*     apply to mine waters (39) -------------------------------- load categories
      if ( JT(feeture) .eq. 39) then
      CTMS(5,JP,IS) = ( (RF * RLC(5)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(5,JP,IS) = ( (RF * RLC(5)) ) / (RF + EF)
      endif
*     apply to Other Point Sources (60) ------------------------ load categories
      if ( JT(feeture) .eq. 60) then
      CTMS(28,JP,IS) = ( (RF * RLC(28)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(28,JP,IS) = ( (RF * RLC(28)) ) / (RF + EF)
      endif
*     apply to private wastewaters (61) ------------------------ load categories
      if ( JT(feeture) .eq. 61) then
      CTMS(29,JP,IS) = ( (RF * RLC(29)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(29,JP,IS) = ( (RF * RLC(29)) ) / (RF + EF)
      endif
*     dilute the remaining contributions ---------------------------------------
      do ip = 6, nprop-2 ! ##################################### needs attention
      CTMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      enddo
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( RF + EF .gt. 1.0e-8 )
*     ****************************************************** point source inputs 
      FMS(IS) = FQ(IS) ! set the river flow to that d/s of the discharge -------

*     calculate the proportion of effluent flow in each shot -------------------
*     apply to (3) (5) (12) (39) (60) (61) -------- proportions of effluent flow
      EFP = EFMS (IS) ! --retrieve the proportion of effluent flow  in each shot 
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61) then
      EFMS(IS) = ( RF * EFP + EFMB ) / (RF + EF)
      else
      EFMS(IS) = ( RF * EFP ) / (RF + EF)
      endif

*     and accumulate the load --------------------------------------------------
      if ( QTYPE (JP) .ne. 4 ) then
      K13 = imonth + 1
      if ( ifdiffuse .eq. 0 ) then
      XLD = EFMB * EQ(IS) ! discharge load from shot number IS -----------------
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,I13) = ELOAD60 (JP,I13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,I13) = ELOAD61 (JP,I13) 
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,K13) = ELOAD60 (JP,K13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,K13) = ELOAD61 (JP,K13) 
     &                                           + XLD
      endif ! if ( ifdiffuse .eq. 0 )
      NSM (JP,I13) = NSM (JP,I13) + 1
      NSM (JP,K13) = NSM (JP,K13) + 1
      endif ! if ( QTYPE (JP) .ne. 4 ) -----------------------------------------

 2222 continue

*     calculate the loads ======================================================
      if ( QTYPE (JP) .ne. 4 ) then
      if ( kdecision(JP) .ne. 8 ) then
      if ( ifdiffuse .eq. 0 ) then ! ======= calculate the annual effluent loads
  
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,I13) = ELOAD60 (JP,I13) 
     &                                           / float(NS)
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,I13) = ELOAD61 (JP,I13) 
     &                                           / float(NS)
      endif ! if ( ifdiffuse .eq. 0 ) ===== calculated the annual effluent loads
      
      if ( munthly structure .eq. 1 ) then ! = calculate  monthly effluent loads
      do J13 = 2, N13 ! ================================ loop through the months
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,J13) = ELOAD60 (JP,J13) 
     &                                           / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,J13) = ELOAD61 (JP,J13) 
     &                                           / float(NSM (JP,J13))
      endif ! if ( ifdiffuse .eq. 0 ) ======= calculated  monthly effluent loads
      endif ! if ( NSM(JP,J13) .gt. 1 ) ========================================
      enddo ! do J13 = 2, N13 ========================== loop through the months
      endif ! ====================================== calculate the monthly loads
      endif ! if ( kdecision(JP) .ne. 8 )
      endif ! if ( QTYPE (JP) .ne. 4 ) =========================================

 9996 continue  

      if ( kdecision(JP) .ne. 8 ) then
      call load calculation per determinand
      call calculate summaries of river flow
      call calculate summaries of river quality (JP,RQ)
      endif ! if ( kdecision(JP) .ne. 8 )

 9999 return
      end
      
 
      
      
      
*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river --------------
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     --------------------------------------------------------------------------
*     File: TARGET Q95.FOR ... 1522 lines (193 lines of comments) --------------
*     --------------------------------------------------------------------------
*     This file compute effluent quality needed to meet the downstream river ---
*     target set as an annual percentile ---------------------------------------
*     --------------------------------------------------------------------------
*     This file contains 1 ...... -------------=----------------------------------
*     It is called:
*     --------------------------------------------------------------------------
*     ...... meet river targets set as percentiles
*     --------------------------------------------------------------------------

      subroutine meet river targets set as percentiles
      include 'COMMON DATA.FOR'

      Mtargit = 1 ! initialise the target as a mean ---------------------------- 

*     prepare to check for the existence of reach-specific standards -----------
      IQSreach = EQS reach (IREACH,JP)

*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
      EC3 = 0.0 ! shift parameter (for 3-parameter log-normal distribution) ----
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX ! store the 95-percentile of effluent quality ------------
      XECM(JP) = ECM ! store the mean effluent quality -------------------------
      
      targit = 0.0 ! set the target --------------------------------------------
      classobj = 0
      IQSfeet = IFRQS(feeture) ! identify any target set for the feature -------
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( QTYPE (JP) .ne. 4 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      classobj = 2
      endif

      if ( IQSfeet .gt. 0 ) then
      targit = RQS(IQSfeet,JP)
      if ( classobj .eq. 2 ) then
      do ic = 1, nclass
      xic = abs(class limmits (ic,JP)/targit)
      if ( xic .gt. 999 .and. xic .lt. 1.001 ) classobj = ic
      enddo 
      endif
      endif

      
      if ( skippeff(feeture) .ne. 9999 ) then ! ###############################
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-specific target
      targit = abs (class limmits (ic,JP))
      classobj = ic
      endif
      enddo ! do ic = 1, nclass
      endif ! if ( IQSreach .gt. 0 )
      endif ! if ( skippeff(feeture) .ne. 9999 ) ###############################

      
      RQO(JP) = targit ! use the target for graphs -----------------------------
      MRQO(JP) = MRQS(JP)
      kdecision(JP) = 2 ! target met - imposed river needs permit --------------
      endif ! if ( QTYPE (JP) .ne. 4 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) 
      
*     get the summary statistic ... 1 mean ...  2 95-percentile PPPPPPPPPPPPPPPP
*     3 90-percentile ...  4 5-percentile ... 5 10-percentile PPPPPPPPPPPPPPPPPP            
      Mtargit = MRQS(JP) !  set the target PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
*     switch for percentile for river quality targets PPPPPPPPPPPPPPPPPPPPPPPPPP
      kptarg = 95 ! a 95-percentile PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      if ( Mtargit .eq. 3 ) kptarg = 90 ! a 90-percentile PPPPPPPPPPPPPPPPPPPPPP
      if ( Mtargit .eq. 6 ) kptarg = 99 ! a 99-percentile PPPPPPPPPPPPPPPPPPPPPP

*     check for a zero river flow ----------------------------------------------
      if ( FLOW(1) .lt. 1.0e-8 ) then
      RATIO = EFM * ECM
      else
*     check for a trivial discharge --------------------------------------------
      RATIO = FLOW(1) * C(JP,1)
      if ( RATIO .gt. 1.0e-8 ) then
      RATIO = EFM * ECM / ( FLOW(1) * C(JP,1) )
      else
      RATIO = 0.0001
      endif
      endif

      if ( RATIO .lt. 0.0001 ) then
      kdecision(JP) = 10 ! trivial discharge - retain current discharge quality 
      if ( nobigout .le. 0 ) then
      call sort format 2 (ECM,ECS)
      write(01,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(31,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! -- EFF
      write(150+JP,1586)DNAME(JP),valchars10,units(JP),valchars11, ! ---- Di EFF
     &units(JP)
 1586 format(110('-')/'Effluent load is too small to consider for ',A11,
     &' the current effluent quality has been retained .....'/110('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! trivial discharge - retain current discharge quality -
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge -----------------------------
      endif

*     check whether there is no standard ---------------------------------------
      if ( targit .lt. 1.0e-8 ) then
      kdecision(JP) = 1 ! no target - retain current discharge quality ---------
      if ( nobigout .le. 0 ) then
      call sort format 2 (ECM,ECS)
      write(01,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(31,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! -- EFF
      write(150+JP,1486)DNAME(JP),valchars10,units(JP),valchars11, ! ---- Di EFF
     &units(JP)
 1486 format(77('-')/'No target for ',A11,' .... ',
     &'the current effluent quality has been retained'/77('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! no target -  retain current discharge quality ---------
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge ------------------------
      endif ! if ( targit .lt. 1.0e-8 ) ----------------------------------------

*     the discharge is not trivial ---------------------------------------------
*     initialise the flag that will mark whether river target cannot be met ----
      IMPOZZ = 0
*     compute the effluent standard needed to meet the river target ------------
      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 1 (targit)

      write(01,1705)DNAME(JP),kptarg,valchars10,units(JP) ! ---------------- OUT
      write(31,1705)DNAME(JP),kptarg,valchars10,units(JP) ! ---------------- EFF
      write(150+JP,1705)DNAME(JP),kptarg,valchars10,units(JP) ! --------- Di EFF
 1705 format(/110('=')/'Assess the effluent quality needed to achieve ', 
     &'the river target for ',A11/
     &'The target is a',i3,'-percentile of: ',a10,1x,a4/110('-'))
      
      call get effluent quality 95 percentile

*     write out the details for determinands -----------------------------------
      call sort format 2 (C(JP,1),ECM) ! mean quality
      write(01,1954)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1954)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1954)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1954 format('Upstream river quality:          Mean =',a10,1x,a4,
     &4x,'Current discharge quality:     Mean =',a10,1x,a4)
      call sort format 2 (C(JP,2),pollution data(IQ,JP,2)) ! standard deviation
      write(01,1955)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1955)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1955)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1955 format(19x,'Standard deviation =',a10,1x,a4,
     &21x,'Standard deviation =',a10,1x,a4)
      call sort format 1 (C(JP,4)) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(01,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(31,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP EFF
      write(150+JP,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPP Di EFF
 3954 format(24x,'90-percentile =',a10,1x,a4) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      call sort format 2 (C(JP,3),ECX)
      write(01,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ OUT
      write(31,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1956)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1956 format(24x,'95-percentile =',a10,1x,a4,
     &26x,'95-percentile =',a10,1x,a4)
      call sort format 2 (C(JP,5),ECX)
      write(01,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP)
      write(31,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP) ! ----------- EFF
      write(150+JP,1959)valchars10,UNITS(JP)!,valchars11,UNITS(JP) ! ---- Di EFF
 1959 format(24x,'99-percentile =',a10,1x,a4/110('-'))

      if ( ifeffcsv . eq. 1 ) then ! ==================================== Ei.CSV
      write(130+JP,7500)GIScode(feeture),unamex, ! ---- u/s river flow--- Ei.CSV
     &rname(ireach),Flow(1),Flow(2)
 7500 format(' ',a40,',',a40,',',a16,',',
     &'Upstream river flow',
     &',',(',',1pe12.4),(',,',1pe12.4),',,,402')
      write(130+JP,7501)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,3),JT(KFEAT),MRQS(JP)
 7501 format(' ',a40,',',a40,',',a16,',',
     &'Upstream river quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      write(130+JP,7502)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),EFM,EFS,JT(KFEAT),MRQS(JP)
 7502 format(' ',a40,',',a40,',',a16,',',
     &'Effluent flow',
     &',',a11,2(',',1pe12.4),',',2(',',i4),',402')
      write(130+JP,7503)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECS,ECX,JT(KFEAT),MRQS(JP)
 7503 format(' ',a40,',',a40,',',a16,',',
     &'Effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      write(130+JP,7504)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),kptarg,targit,JT(KFEAT),MRQS(JP)
 7504 format(' ',a40,',',a40,',',a16,',',
     &'River quality standard (',i2,'-percentile)',
     &(',',1pe12.4),',,,',2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) =================================== Ei.CSV
      endif ! if ( nobigout .le. 0 ) then ! ====================================

*     the discharge standard will be set between bounds of 'best' and 'worst' --
*     initilialise for this ----------------------------------------------------
      IBOUND = 0 ! set discharge standards within bounds -----------------------
      
      call meet percentile target in the river (IMPOZZ,targit,IBOUND)
*     calculations complete ====================================================

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     options for when the river target is unachievable ++++++++++++++++++++++++
      if ( IMPOZZ .eq. 1 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++
*     target cannot be achieved by changing this effluent quality ++++++++++++++
*     check the current effluent quality +++++++++++++++++++++++++++++++++++++++
*     if it is quite good then keep it ... if not then improve to good quality +
      if ( ECX .gt. GEQ(3,JP) ) then ! +++++++++++++++++++++++++++++++++++++++++
      if ( GEQ(1,JP) .gt. 1.0e-09 ) then ! check "good quality" has been entered
*     over-write calculated effluent quality with "good" quality +++++++++++++++
      TECM = GEQ(1,JP) ! mean "good" quality +++++++++++++++++++++++++++++++++++
      TECS = GEQ(2,JP) ! standard deviation ++++++++++++++++++++++++++++++++++++
      TECX = GEQ(3,JP) ! 95-percentile +++++++++++++++++++++++++++++++++++++++++
      XEFF(JP) = TECX !
      XECM(JP) = TECM
      kdecision(JP) = 7 ! target not met - imposed specified good quality ++++++
      IMPOZZ = 77 ! impossible to meet the river target - impose GOOD ++++++++++

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,7386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ----------- EFF
     &UNITS(JP),valchars12,UNITS(JP)
      write(150+JP,7386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! -=-- Di EFF
     &UNITS(JP),valchars12,UNITS(JP)
 7386 format('The river target is unachievable for ',a11,4x,
     &'... imposed good effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))

      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7505)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7505 format(' ',a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) then
      endif ! if ( nobigout .le. 0 )

*     compute the 95-percentile of effluent quality ----------------------------
      TECM = GEQ(1,JP) ! mean "good" quality -----------------------------------
      TECS = GEQ(2,JP) ! standard deviation ------------------------------------
      ECM = TECM
      ECS = TECS
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = TECM
      call load the upstream flow and quality 
      call mass balance ! calculating the effect of good effluent quality ------
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge -------------------------

      else ! when ( GEQ(1,JP) .le. 0.00001 ) ===================================
*     over-write calculated effluent quality with current quality --------------

      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 8 ! target not met - retained current discharge quality --
      IMPOZZ = 77 ! impossible to meet the river target - retained current -----

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ----------- EFF
     &UNITS(JP),alchars12,UNITS(JP)
      write(150+JP,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ---- Di EFF
     &UNITS(JP),alchars12,UNITS(JP)
 8386 format('The river target is unachievable for ',a11,1x,
     &'... retained current effluent quality:  Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
 7515 format(' ',a40,',',a40,',',a16,',',
     &'Retained current effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) 
      endif ! if ( nobigout .le. 0 )

*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)

*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = ECM
      call load the upstream flow and quality 
      call mass balance ! from meet river targets set as percentiles
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge ------------------------
      endif ! if ( GEQ(1,JP) .gt. 0.00001 ) ====================================

      else ! when ( ECX is already better than GEQ(1,JP) ) +++++++++++++++++++++
*     over-write the calculated effluent quality with the current quality ------
      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      
      kdecision(JP) = 8 ! target not met - retained current discharge quality --
      IMPOZZ = 77 ! ! impossible to meet the target - impose GOOD then current +

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ----------- EFF
     &UNITS(JP),valchars12,UNITS(JP)
      write(150+JP,8386)DNAME(JP),valchars10,UNITS(JP),valchars11, ! ---- Di EFF
     &UNITS(JP),valchars12,UNITS(JP)
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )

      
*     compute the 95-percentile of effluent quality ----------------------------
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX ! 95-percentile of effluent quality ---------------------------
      XEFF(JP) = TECX
      XECM(JP) = ECM
      call load the upstream flow and quality 
      call mass balance ! from meet river targets set as percentiles
      call get summaries of river quality from the shots
      return ! return to ...... effluent discharge ------------------------

      endif ! if ( ECX .gt. GEQ(1,JP) ) ########################################
      endif ! if ( IMPOZZ .eq. 1 ) #############################################
*     ##########################################################################




*     check that the selected effluent quality is technically feasible ---------
*     constrain the effluent standard to the best achievable where existing ----
*     quality is worse than this -----------------------------------------------
      if ( IMPOZZ .eq. 99 ) then
      kdecision(JP) = 3 ! target not met - imposed best achievable -------------
      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,1582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,1582)valchars10,UNITS(JP),valchars11, ! --------------------- EFF
     &UNITS(JP),valchars12,UNITS(JP) ! ------------------------------------- EFF
      write(150+JP,1582)valchars10,UNITS(JP),valchars11, ! ---------------Di EFF
     &UNITS(JP),valchars12,UNITS(JP) ! ---------------------------------- Di EFF
 1582 format('Effluent quality is set as that defined as best',
     &' achievable ...',27x,'Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      endif
      if ( ifeffcsv .eq. 1 ) then
      write(130+JP,7506)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7506 format(' ',a40,',',a40,',',a16,',',
     &'Best achievable effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif
      return
      endif ! if ( IMPOZZ .eq. 99 ) --------------------------------------------

      
      
*     current quality is better than the "best achievable" ---------------------
      if ( IMPOZZ .eq. 95 ) then
      kdecision(JP) = 11 ! target not met - retained current quality -----------      
      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,7582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,7582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ----------- EFF
     &valchars12,UNITS(JP)
      write(150+JP,7582)valchars10,UNITS(JP),valchars11,UNITS(JP), ! ---- Di EFF
     &valchars12,UNITS(JP)
 7582 format('Current effluent quality retained ...',
     &' it is better than "best"',27x,'Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('*'))
      endif
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7576)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),Mtargit
      write(130+JP,7576)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT),MRQS(JP)
 7576 format('X',a40,',',a40,',',a16,',',
     &'Retained current effluent quality',
     &',',a11,3(',',1pe12.4),2(',',i4),',402')
      endif
      return
      endif ! if ( IMPOZZ .eq. 95 ) --------------------------------------------


      
*     stop effluent quality being made too poor whilst 777777777777777777777-DET
*     still achieving the river targets 777777777777777777777777777777777777-DET
      if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) then ! 7777777777777777777777-DET
      kdecision(JP) = 4 ! target not threatened - imposed worst defined 7777-DET
      if ( nobigout .le. 0 ) then
      call sort format 2 (targit,TECM)
      write(01,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! 777777777777-DET
      write(31,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,1388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 1388 format('Effluent quality could be very bad yet still meet ',
     &'the river target of ...',22x,a10,1x,a4/110('-')/
     &'Effluent quality will be limited to the worst you have ',
     &'specified ... ',20x,'Mean =',a10,1x,a4)
      call sort format 2 (TECS,TECX)
      write(01,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! 777777777777-DET
      write(31,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,4388)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 4388 format(75x,'Standard deviation =',a10,1x,a4/
     &80x,'95-percentile =',a10,1x,a4/110('-'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7507)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
 7507 format(' ',a40,',',a40,',',a16,',',
     &'Worst permissible effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402') ! 777777777777777777777777777777-DET
      endif ! if ( ifeffcsv . eq. 1 ) ! 777777777777777777777777777777777777-DET
      endif ! if ( nobigout .le. 0 ) ! 7777777777777777777777777777777777777-DET
      return  ! 777777777777777777777777777777777777777777777777777777777777-DET
      endif ! if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) 777777777777777777777-DET

*     use special policy achieving river targets 888888888888888888888888888-DET
*     but allowing no deterioration 8888888888888888888888888888888888888888-DET
*     in river quality in cases where river is already inside  8888888888888-DET
*     the target with current effluent quality 88888888888888888888888888888-DET
      if ( ICAL .ne. 08 .and. ical .ne. 09 ) return ! 9999999999999999999999-DET
      
*     check the cases where the effluent quality is already 88888888888888888888
*     too good for the river quality target 888888888888888888888888888888888888
*     check whether the calculated effluent quality is laxer 8888888888888888888
*     than the current effluent quality 8888888888888888888888888888888888888888
      if ( pollution data (IQ,JP,1) .gt. TECM ) return ! 88888888888888888888888
*     the proposed effluent quality is laxer than current quality 88888888888888
*     check whether the deterioration in river quality is acceptable 88888888888
*     compute the river quality downstream of the current effluent quality 88888
      ECM = pollution data (IQ,JP,1)
      ECS = pollution data (IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) 8888888888888888
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
*     call load the upstream flow and quality ! 88888888888888888888888888888888
      call get downstream river quality

      
      
*     ######################################################### SECTION NOT USED      
*     the rest of code in this ...... is NOT USED ######## SECTION NOT USED 
*     ######################################################### SECTION NOT USED      
*     check for deterioration less than a target factor such as 10% ### NOT USED 
*     NOT USED at present ... needs u/s river quality to be reset ##### NOT USED 
*     ######################################################### SECTION NOT USED      
      target factor = 0.00 ! currently set a zero ############# SECTION NOT USED
      target fraction = target factor * current ds quality ! ## SECTION NOT USED
      target gap = targit - current ds quality ! ############## SECTION NOT USED
      if ( target gap .ge. target fraction + 0.00000001) then ! SECTION NOT USED
*     deterioration exceeds the target factor ################# SECTION NOT USED
*     reset the target to give allowed deterioration ########## SECTION NOT USED
      if ( target factor .gt. 0.0 ) then ! #################### SECTION NOT USED
      targit = target fraction + current ds quality ! ######### SECTION NOT USED
      if ( nobigout .le. 0 ) then ! ########################### SECTION NOT USED
      call sort format 2 (current ds quality,targit)
      write(01,1852)valchars10,Units(JP),valchars11,Units(JP)
      write(31,1852)valchars10,Units(JP),valchars11,Units(JP) ! ------------ EFF
      write(150+JP,1852)valchars10,Units(JP),valchars11,Units(JP) ! ----- Di EFF
 1852 format('Achieving the river target would permit a',
     &' deterioration in river quality ...',
     &'this has been cut to 10 per cent'/
     &'The quality downstream of the current discharge is ',a10,1x,A4,
     &' ... the new target is  ',a10,1x,A4/110('-'))
*     ######################################################### SECTION NOT USED
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7508)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),current ds quality,Mtargit,
     &targit,Mtargit
 7508 format(' ',a40,',',a40,',',a16,',',
     &'Current downstream quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'New mean target',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 ) ######################### SECTION NOT USED
      endif ! if ( nobigout .le. 0 ) ########################## SECTION NOT USED 
*     calculate the required discharge quality for this 10% deterioration ######
*     this is TECX ... the discharge standard will not be set between bounds ###
      kdecision(JP) = 5 ! target not threatened - 10% deterioration ### NOT USED 
      IBOUND = 1 ! set discharge standards without checking bounds #### NOT USED 
      call meet percentile target in the river (IMPOZZ,targit,IBOUND) !  10% det
      if ( IBOUND .eq. 1 ) then ! do not look at bounds ####### SECTION NOT USED
      if ( nobigout .le. 0 ) then
      write(01,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP)
      write(31,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP) ! ------- EFF
      write(150+JP,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP) !- Di EFF
 1832 format(110('-')/
     &'Required river quality:  95-percentile =',F9.2,1x,A4,'  ',
     &'Required effluent quality         Mean =',F9.2,1x,A4/
     &81x,'95-PERCENTILE =',F9.2,1x,A4/110('-'))
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7509)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),targit,Mtargit,TECM,TECX,Mtargit
 7509 format(' ',a40,',',a40,',',a16,',',
     &'Required mean river quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'Required effluent quality',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( IBOUND .eq. 1 ) do not look at bounds ###### SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     set variable equal to a 20% boost to current discharge pollution  NOT USED
      discharge factor = 1.20
      XCM = discharge factor * pollution data(IQ,JP,1)
      XCMKeep = XCM
      XCSKeep = discharge factor * pollution data(IQ,JP,2)
*     ######################################################### SECTION NOT USED
*     check whether the new discharge quality exceeds this #### SECTION NOT USED
      if (TECM .gt. XCM) goto 1892 ! ########################## SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the river target is met ################################# SECTION NOT USED
*     if this is done the deterioration in the river exceeds 10% ###### NOT USED
*     so the effluent standard has been re-calculated to give a 10% ### NOT USED
*     deterioration in river quality ########################## SECTION NOT USED
*     change allows less than a 20% deterioration in effluent quality # NOT USED
*     the calculation of the discharge quality is finished #### SECTION NOT USED
      return
      endif ! if ( target factor .gt. 0.0 ) target deterioration ###### NOT USED
*     the river target is achieved ############################ SECTION NOT USED
*     new standard is tighter than the old or the deterioration ================
*     in the river is less than 10% .... and the deterioration embodied in the =
*     new effluent standard is less than 10% ################## SECTION NOT USED
      endif ! if ( target gap .ge. target fraction + 0.00000001) ###### NOT USED
      return ! ################################################ SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the calculated effluent standard is laxer than the current effluent ======
*     quality, but the deterioration in river quality is acceptable ### NOT USED
*     (less than 10%) ######################################### SECTION NOT USED
*     nonetheless the effluent standard will be constrained to a 20% ## NOT USED
*     deterioration from the current effluent quality ######### SECTION NOT USED
 1892 continue ! ############################################## SECTION NOT USED
*     ######################################################### SECTION NOT USED

      
      
      
      ECM1 = ECM
      ECX1 = ECX
      ECM = XCMKEEP
      ECS = XCSKEEP
*     compute the percentile of effluent quality +++++++++++++++++++++++++++++++
      call get effluent quality 95 percentile
      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 6 ! target not threatened - 10% deterioration ++++++++++++
      if ( nobigout .le. 0 ) then
      write(01,1983)ECM,UNITS(JP),ECX,UNITS(JP)
      write(31,1983)ECM,UNITS(JP),ECX,UNITS(JP) ! -------------------------- EFF
      write(150+JP,1983)ECM,UNITS(JP),ECX,UNITS(JP) ! ------------------- Di EFF
 1983 format(
     &'Achieving the river target or a 10% deterioration would',
     &' permit too bad an effluent quality ...'/110('-')/
     &'Deterioration in effluent quality held at 20% ...',
     &6x,' Resulting effluent quality ...    Mean =',F9.2,1x,A4/
     &72x,'         95-PERCENTILE =',F9.2,1x,A4/110('-'))
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,7511)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECX,Mtargit
 7511 format(' ',a40,',',a40,',',a16,',',
     &'Deterioration in effluent quality held at 20%',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( ifeffcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      return
      end



*     this routine takes flow and quality data for an effluent and works out ---
*     the effluent quality needed to achieve set percentile of river quality ---
*     downstream ---------------------------------------------------------------
      subroutine meet percentile target in the river
     &(IMPOZZ,TRQS,IBOUND)
      include 'COMMON DATA.FOR'
      dimension EQ(NS),RQ(NS),UQ(NS),FQ(NS),UF(NS) 
      dimension RLC(nprop)
      character *13 SET1,SET2

      Mtargit = 1

      do is = 1, NS
      eq(is) = 0.0
      rq(is) = 0.0
      uq(is) = UCMS(JP,IS) ! load the upstream river quality
      fq(is) = 0.0
      uf(is) = UFMS(IS) ! load the upstream river flow
      do ip = 1, n2prop
      UCTMS(ip,JP,IS) = CTMS(ip,JP,IS )
      enddo
      enddo
      
*     check for trivial effluent flow ------------------------------------------
      if ( EFM .lt. 1.0E-15 ) return
      if ( IMPOZZ .eq. 77 ) goto 8888

*     calculated effluent quality needed to secure river target ----------------
*     initially, set this equal to the input quality ---------------------------
*     mean and standard deviation of discharge quality -------------------------
      TECM = pollution data (IQ,JP,1)
      TECS = pollution data (IQ,JP,2)

*     the coefficient of variation ---------------------------------------------
      ECV = 1.0
      if (tecm .gt. 1.0e-8) ECV = TECS/TECM

      call get the summary statistics of discharge flow
      
*     the target river quality is TRQS -----------------------------------------
*     compute the statistics of the upstream river quality ---------------------
      call get summaries of river quality from the shots

*     set the mean, standard deviation and percentile --------------------------
      RCM = C(JP,1)
      RCS = C(JP,2)
      RCV = 0.6
      
      if ( RCM .gt. 0.00001 ) RCV = RCS / RCM ! 99999999999999999999999999999999
      if ( kptarg .eq. 95 ) RCX = C(JP,3) ! 95-percentile PPPPPPPPPPPPPPPPPPPPPP
      if ( kptarg .eq. 90 ) RCX = C(JP,4) ! 90-percentile PPPPPPPPPPPPPPPPPPPPPP
      if ( kptarg .eq. 99 ) RCX = C(JP,5) ! 99-percentile PPPPPPPPPPPPPPPPPPPPPP

      if ( ical .eq. 09 .and. classobj .gt. 0 ) then ! for Run Type 9 ==========
      xlim1 = 0.0
      xlim2 = abs( class limmits (classobj,JP) )
      xlim1 = abs( class limmits (classobj - 1,JP) )
      xgap = xlim2 - xlim1
      if ( abs (xgap) .lt. 1.0e-07 ) then
      classobj = 0    
      else
      RCX = xlim1 + classfraction * xgap 
      xpoint = 90.0
      Kstat = MRQS (JP)
      if (kstat .eq. 2 ) xpoint = 95.0
      if (kstat .eq. 6 ) xpoint = 99.0
      call change to log domain (RCX, xpoint, 1.0, RCV, RCM, RCS)

      ipoint = xpoint
      call ASSEM (xlim1,xlim2,SET1)
      write(01,3000)SET1,ipoint,RCX,RCM,RCS
      write(31,3000)SET1,ipoint,RCX,RCM,RCS ! ------------------------------ EFF
      write(150+JP,3000)SET1,ipoint,RCX,RCM,RCS ! ----------------------- Di EFF
 3000 format(110('=')/
     &'Upstream quality is imposed as the middle of the target class ',
     &'(Run Type 9) ... Limits: ',a13/110('=')/
     &21x,i5,'-percentile =',f10.3/
     &19x,'              Mean =',f10.3/
     &19x,'Standard deviation =',f10.3/
     &110('='))
*     create the river quality data 99999999999999999999999999999999999999999999
      call generate mode 9 river quality (RCM,RCS)
      endif ! if ( abs (xgap) .lt. 1.0e-07 )
      endif ! if ( ical .eq. 09 .and. classobj .gt. 0 ) 999999999999999999999999

      call get summaries of loads
      
      RFM = FLOW(1) ! the mean river flow

*     compute need for improvement to discharge quality ------------------------
      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then ! 78978978978
      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)
      endif ! if ( IQDIST .lt. 4 )

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) then ! -----
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) ----
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

      do 2002 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4) ! meet target
      RF = FMS(IS)
      if ( ical .eq. 09 ) then ! 99999999999999999999999999999999999999999999999
      RC = CMS(JP,IS) ! 99999999999999999999999999999999999999999999999999999999
      else ! 9999999999999999999999999999999999999999999999999999999999999999999
      RC = CMS(JP,IS)
      endif ! 999999999999999999999999999999999999999999999999999999999999999999

*     get the shots for the discharge ------------------------------------------
      call get a flow for the stream or discharge (IS, R3, EF)

*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL

*     set the flows to 1.0 -----------------------------------------------------
      EFMB = 1.0
      endif ! if ( IQDIST .eq. 6 ... etc ---------------------------------------

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ (IS) = EC

*     mass balance -------------------------------------------------------------
      if ( RF + EF .gt. 1.0e-8 ) then
      RQ (IS) = (RF*RC+EFMB*EQ(IS))/(RF+EF)
      fq (is) = RF + EF
      endif
 2002 continue

      TRCX = get river percentile (RQ)
      TECX = get effluent percentile (EQ)

      XEFF(JP) = TECX
*     XECM(JP) = TECM

      if ( nobigout .le. 0 ) then
      call sort format 2 (TRCX,TECX)

      if ( ical .eq. 09 .and. classobj .gt. 0 ) then
      write(01,3964)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3964)kptarg,valchars10,UNITS(JP),valchars11, ! -------------- EFF
     &UNITS(JP)
      write(150+JP,3964)kptarg,valchars10,UNITS(JP),valchars11, ! ------- Di EFF
     &UNITS(JP)
 3964 format('River quality provided:',i3,'-percentile =',a10,1x,A4,
     &'     by current discharge quality:  Q95 =',a10,1x,A4/110('='))
      else
      write(01,3963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3963)kptarg,valchars10,UNITS(JP),valchars11, ! -------------- EFF
     &UNITS(JP)
      write(150+JP,3963)kptarg,valchars10,UNITS(JP),valchars11, ! ------- Di EFF
     &UNITS(JP)
 3963 format('River quality provided:',i3,'-percentile =',a10,1x,A4,
     &'     by current discharge quality:  Q95 =',a10,1x,A4/110('-'))
      endif

      if ( ifeffcsv .eq. 1 ) then
      write(130+JP,7500)GIScode(feeture),unamex, ! -- CURRENT discharge - Ei.CSV
     &rname(ireach),TRCX,JT(KFEAT),MRQS(JP)
 7500 format(' ',a40,',',a40,',',a16,',',
     &'River quality from current discharge', ! ---- percentile targets - Ei.CSV
     &',',(1pe12.4),',,,',2(',',i4),',402')
      endif ! if ( ifeffcsv .eq. 1 )
      endif ! if ( nobigout .le. 0 )
      
*     check whether current discharge quality meets the river target ===========
      icurrent = 0
      if ( ical .eq. 08 .or. ical .eq. 09 ) then ! =============================
      if ( TRCX .le. TRQS ) then ! if target is already met ==================== 

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then
      write(01,892)
      write(31,892) ! ------------------------------------------------------ EFF
      write(150+JP,892) ! ----------------------------------------------- Di EFF
  892 format('The river quality target is achieved without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('-'))
      else
      write(01,894)
      write(31,894) ! ------------------------------------------------------ EFF
      write(150+JP,894) ! ----------------------------------------------- Di EFF
  894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('='))
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 )

      endif ! if ( nobigout .le. 0 )
      icurrent = 1 ! retained current ------------------------------------------
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      goto 9095 ! target achieved with current discharge quality ---------------
        
*     if it is quite good then keep it ... if not then improve to good quality %
          
      if ( ECM .gt. GEQ(1,JP) .and. ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &   GEQ(1,JP) .gt. 1.0e-09 ) then ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*     over-write calculated effluent quality with "good" quality %%%%%%%%%%%%%%%
      TECM = GEQ(1,JP) ! mean "good" quality
      TECS = GEQ(2,JP) ! standard deviation
      TECX = GEQ(3,JP) ! 95-percentile
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 13 ! target not met - imposed specified good quality ------

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then
      write(01,2386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
      write(31,2386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
*    &valchars12,UNITS(JP)
      write(150+JP,2386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
*    &valchars12,UNITS(JP)
 2386 format(
     &'The river target is already being achieved ',4x,
     &'...... imposed "good" effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('*'))
      else
      write(01,3386)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ------------ EFF
      write(150+JP,3386)valchars10,UNITS(JP),valchars11,UNITS(JP) ! ----- Di EFF
 3386 format(
     &'The river target is already being "achieved" ',2x,
     &'...... imposed good effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('*'))
          
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 ) %%%%%%%%%%%%%%%%%%%%%%%%%
      if ( ifeffcsv . eq. 1 ) then
      write(130+JP,2505)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,TECS,JT(KFEAT),MRQS(JP)
 2505 format(' ',a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,2(',',1pe12.4),2(',',i4),',402')
      endif ! if ( ifeffcsv . eq. 1 ) then %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      endif ! if ( nobigout .le. 0 ) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      else ! when ( GEQ(1,JP) .le. 0.00001 and ECM .gt. GEQ(1,JP) ) then ! %%%%%
      if ( nobigout .le. 0 ) then
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then
      write(01,4892)
      write(31,4892) ! ----------------------------------------------------- EFF
      write(150+JP,4892) ! ---------------------------------------------- Di EFF
 4892 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('*'))
      else
      write(01,4894)
      write(31,4894) ! ----------------------------------------------------- EFF
      write(150+JP,4894) ! ---------------------------------------------- Di EFF
 4894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('*'))
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      icurrent = 1 ! retained current ------------------------------------------
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      goto 9095 ! target achieved with current discharge quality ---------------
      endif
      goto 9095 ! impose "good" discharge quality %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      endif ! if ( TRCX .le. TRQS )
      endif ! if ( ical .eq. 08 or 09 ) ! ======================================
      endif ! if ( ical .eq. 07, 08 or 09 ) ! ==================================

      IDIL = 0 ! initialise switch for dealing with unachievable targets
      KDIL = 0 ! initialise switch for dealing with unachievable targets
      if ( RCM .gt. 1.0E-20 ) then
*     is the river target stricter than the current upstream river quality ? ---
      if ( RCX .ge. TRQS ) then
      if ( detype (jp) .ne. 104 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (RCX,TRQS)
      write(01,764)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! --- OUT
      write(31,764)DNAME(JP),valchars10,units(JP),valchars11,units(JP) ! --- EFF
      write(150+JP,764)DNAME(JP),valchars10,units(JP), ! ---------------- Di EFF
     &valchars11,units(JP) ! -------------------------------------------- Di EFF
  764 format(
     &'WARNING - Upstream river quality for ',a11,' ...  ',a10,1x,a4/
     &'is worse than required downstream of the discharge ...',
     &a10,1x,a4/110('-'))
      endif ! if ( nobigout .le. 0 )
*     set the switches ---------------------------------------------------------
      IDIL = 1 ! prepare to try zero quality for the discharge
      KDIL = 1 ! prepare to try zero quality for the discharge
*     prepare to try zero quality for the discharge ----------------------------
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 382
      endif ! if ( detype (jp) .ne. 104 )
      endif ! if ( RCX .ge. TRQS )

      TRCM = TRQS / ( 1.0 + 1.6449 * RCS / RCM )
      else ! if ( RCM is less then 1.0E-20 )
      TRCM = 0.5 * TRQS ! zero RCM
      endif ! if ( RCM .gt. 1.0E-20 )


*     compute the first guess of the required discharge quality ----------------
      TECM = (TRCM * (RFM + EFM) - (RCM * RFM)) / EFM
      if (TECM .lt. 1.0E-10) then
      IDIL = 1 ! prepare to try zero quality for the discharge
      KDIL = 1 ! prepare to try zero quality for the discharge
*     prepare to try zero quality for the discharge ----------------------------
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      endif ! if (TECM .lt. 1.0E-10)
      TECS = ECV*TECM
  382 continue

*     revert to a log-normal distribution --------------------------------------
      IQDIST = 2

*     iteration counter --------------------------------------------------------
      ITER = 0
*     set initial value for the river percentile downstream of the effluent ----
      TRCX = 1.0E-7
*     starting set of data for iteration scheme --------------------------------
      TECM0 = 0.0
      TRCX0 = 0.0
      TECX0 = 0.0
      TECM1 = 0.0
      TECX1 = 0.0
      TRCX1 = 0.0

  996 ITER = ITER + 1 ! number of the next iteration 

      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      endif

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) then ! -----
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif ! if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 .and. IQDIST .ne. 11 ) ----
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

      do 2 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads ---------
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4) ! meet targ
      RF = FMS(IS)
      RC = CMS(JP,IS)

*     get the shots for the discharge ------------------------------------------
      call get a flow for the stream or discharge (IS, R3, EF)

*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
*     set the flows to 1.0 -----------------------------------------------------
      EFMB = 1.0
      endif

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC

*     mass balance +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( RF + EF .gt. 1.0e-8 ) then
      RQ(IS) = (RF*RC+EFMB*EQ(IS))/(RF+EF) ! +++++++++++++++++++++++++++++++++++
      fq(is) = RF + EF
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    2 continue

      TRCX = get river percentile (RQ)
      TECX = get effluent percentile (EQ)

      XEFF(JP) = TECX

      if ( IDIL .eq. 1 ) then ! ================================================
*     a discharge quality of zero has been tried -------------------------------
*     check whether this achieved the target river quality ---------------------
      if ( TRCX .ge. TRQS ) then
      IMPOZZ = 1 ! the river target is unachievable ----------------------------
      kdecision(JP) = 7 ! target not met - imposed specified good quality ------
      TECM = GEQ(1,JP)
      TECS = GEQ(2,JP)
      TECX = GEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      if ( detype (JP) .ne. 104 ) then
      if ( nobigout .le. 0 ) then
*     write(01,802)dname(JP),uname(feeture) ! ------------------------------ OUT
*     write(31,802)dname(JP),uname(feeture) ! ------------------------------ EFF
*     write(150+JP,802)dname(JP),uname(feeture) ! ----------------------- Di EFF
      write(33,802)dname(JP),uname(feeture) ! ------------------------------ ERR
  802 format(/77('-')/'The river quality target for ',a11,
     &' is not achievable without '/
     &'improving river quality upstream of the discharge 'a35/77('-'))
      endif
      endif
      goto 8888 ! if ( IDIL .eq. 1 ) ... unachievable target ===================
      endif
      IDIL = 0 ! initialise switch for dealing with unachievable targets
      endif ! if ( IDIL .eq. 1 ) ===============================================

*     test for convergence -----------------------------------------------------
      CONV = ABS ( (TRCX-TRQS) / TRCX)
      if  (CONV .lt. 0.0001 ) goto 9095
      if ( ITER .gt. 60 ) then
      if ( nobigout .le. 0 ) then
      write(01,6000)
      write(03,6000)
      write(09,6000)
      write(33,6000)
      write(31,6000) ! ----------------------------------------------------- EFF
      write(150+JP,6000) ! ---------------------------------------------- Di EFF
 6000 format(110('-')/
     &'Unable to calculate effluent quality ',
     &'that meets the river target ... ',
     &'current effluent quality retained'/110('-'))
*     set quality equal to the current quality ---------------------------------
      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3 ) EC3 = pollution data(IQ,JP,3)
      call get effluent quality 95 percentile
      TECX = ECX
      TECM = ECM
      TECS = ECS
      XEFF(JP) = TECX
      XECM(JP) = TECM
      endif
      goto 9095   
      endif

      if ( ITER .eq. 1) then

*     store the result as the first trial --------------------------------------
  893 TECM1 = TECM
      TECX1 = TECX
      TRCX1 = TRCX

*     invent a set of data for the second trial --------------------------------
      FX = 1.0
      if (ABS ( (rcx-trcx) / trcx ) .lt. 0.05) FX = 0.5
      TECM0 = FX * RCM
      TRCX0 = FX * RCX
      TECX0 = TRCX0
      goto 891
      endif ! if ( ITER .gt. 1 ) 

*     SECOND iteration - store result as second trial --------------------------
      if ( ITER .eq. 2) then
      tecm0 = tecm
      TECX0 = TECX
      TRCX0 = TRCX

*     interpolate between trials to get input for next iteration ---------------
      goto 891
      endif

*     THIRD and subsequent iterations ------------------------------------------
*     check which trial was best -----------------------------------------------
      if (ABS (TRCX0-TRQS) .lt. ABS (TRCX1-TRQS) ) then

*     first was best - overwrite with new result -------------------------------
      tecm1 = tecm
      TRCX1 = TRCX
      TECX1 = TECX
      goto 891
      endif

*     the second was best ------------------------------------------------------
      tecm0 = tecm
      TRCX0 = TRCX
      TECX0 = TECX

*     compute input for next trial by interpolating linearly between the -------
*     first and second trial ---------------------------------------------------
  891 continue
      TRDIFF = TRCX1 - TRCX0

      if ( TRDIFF .lt. 0.00001 .and. TRDIFF .gt. -0.00001) then
      TRDIFF = 0.00001
      endif

      tecm = tecm0 + (TRQS-TRCX0) * (tecm1-tecm0) / TRDIFF
      if ( tecm .gt. 0.0 ) goto 183
      tecm = 0.5 * AMIN1( tecm0, tecm1 )
      if ( tecm .gt. 0.0 ) go to 183
      if ( KDIL .lt. 1 ) goto 448
      tecm = 0.5 * (tecm0 + tecm1)
      if ( tecm .gt. 0.0 ) goto 183

  445 continue
      write( *,446)
      write(01,446)
      write(09,446)
      write(33,446)
      write(31,446) ! ------------------------------------------------------ EFF
      write(150+JP,446) ! ----------------------------------------------- Di EFF
  446 format(/'### WARNING ! - Problem is unstable'/
     &' ### Calculation halted ...')
      call stop

  448 IDIL = 1
      KDIL = 1
      TECM = 0.0
      TECS = 0.0
      TECX = 0.0
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 996 ! proceed with the next iteration



*     compute standard deviation of effluent quality ---------------------------
*     ... for the next iteration ... -------------------------------------------
  183 TECS = ECV * TECM ! set the standard deviation

      if ( ITER .eq. 1 ) goto 996 ! proceed to the next iteration
*     check whether calculated discharge quality is infeasibly bad =============
      if ( TECM .gt. 300000.0 ) then
      suppress9d = suppress9d + 1
      if ( suppress9d .lt. 6 ) then
      call change colour of text (19) ! light pink
      write( *,8463)uname(feeture),dname(JP)
 8463 format(
     &'*** Calculated discharge quality is huge',11x,'...',7x,
     &'at: ',a37,3x,'for ',a11,10('.'))
      call set screen text colour
      else
      if ( suppress9d .gt. 9999 ) suppress9d = 0
      endif
      if ( suppress9d .eq. 6 ) then
      call change colour of text (19) ! light pink
      write( *,3463)uname(feeture)
 3463 format(
     &'*** Calculated discharge quality is huge',11x,'...',7x,
     &'at: ',a37,3x,'and other cases .........')
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) then
      write(01,8763)UNITS(JP),dname(jp)!,kptarg,TRQS,UNITS(JP),TECM, ! ------ OUT
    !&UNITS(JP),kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)
      write(31,8763)UNITS(JP),dname(jp)!,kptarg,TRQS,UNITS(JP),TECM, ! ------ EFF
    !&UNITS(JP),kptarg,C(JP,1),UNITS(JP)!,C(JP,3),UNITS(JP)  
      write(150+JP,8763)UNITS(JP),dname(jp)!,kptarg,TRQS,UNITS(JP), ! ---- Di EFF
    !&TECM,UNITS(JP),kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)  
 8763 format(110('=')/'The calculated discharge quality is ',
     &'bigger than 300000 ',a4,' ... for ',a11/110('-'))!/
    !&'TARGET river quality:    ',i2,'-PERCENTILE =',F9.2,1x,A4,
    !&'  Required discharge quality:       Mean =',F9.3,1x,A4/110('-')/
    !&'Achieved river quality:  ',i2,'-percentile =',F9.2,1x,A4,'  ',
    !&13x,' Achieved 95-PERCENTILE =',F9.3,1x,A4/110('='))
      endif
      write(33,8763)UNITS(JP),dname(jp)!,kptarg,TRQS,UNITS(JP),TECM, ! ------ ERR
    !&UNITS(JP),kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)
      goto 9095 
      else ! if ( TECM .lt. 300000.0 )
*     go to the next iteration =================================================
      goto 996
      endif ! if ( TECM .gt. 300000.0 ) ========================================

 9095 continue ! iterations complete - convergence complete - percentiles ------
      
      
      
      goto (4291,4291,9996,9999,9999,9996),QTYPE(JP)
 4291 continue

      ECM = TECM
      ECS = TECS

      call get effluent quality 95 percentile
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM

*     special treatment for Mode 9 999999999999999999999999999999999999999999999
      if ( ical .eq. 09 ) then
      call load the upstream flow and quality ! 99999999999999999999999999999999
      endif ! 999999999999999999999999999999999999999999999999999999999999999999
      
      do is = 1, NS
      CMS(JP,is) = UCMS(JP,is)
      enddo

      call mass balance ! calculate loads --------------------------------------  
      
      call calculate summaries of river flow
      
*     for a percentile river quality standard ---------------------------------- 
      if ( nobigout .le. 0 .and. icurrent .eq. 0 ) then ! ######################
          
      qualnuse = PNUM(IQ,JP) 
      call compute confidence limits 
     &(0.0,iqdist,TECM,TECS,0.0,qualnuse,testql,testqu)
      call ASSEM (testql,testqu,SET1)
      call compute confidence limits 
     &(95.0,iqdist,TECM,TECS,TECX,qualnuse,testql,testqu)
      call ASSEM (testql,testqu,SET2)

      call sort format 2 (Trqs,TECM)
      write(01,4963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET1
      write(31,4963)kptarg,valchars10,UNITS(JP),valchars11, ! -------------- EFF
     &UNITS(JP),SET1
      write(150+JP,4963)kptarg,valchars10,UNITS(JP),valchars11, ! ------- Di EFF
     &UNITS(JP),SET1
 4963 format(
     &'TARGET river quality:   ',i2,'-percentile =',a10,1x,a4,
     &'  Required discharge quality:      Mean =',a10,1x,a4,4x,a13)
      call sort format 1 (TECS)
      write(01,7963)valchars10,UNITS(JP)
      write(31,7963)valchars10,UNITS(JP) ! --------------------------------- EFF
      write(150+JP,7963)valchars10,UNITS(JP) ! -------------------------- Di EEF
 7963 format(65x,'Discharge standard deviation =',a10,1x,a4)
      call sort format 2 (TRCX,TECX)
      if ( ical .ne. 09 ) then ! ===============================================
      write(01,5963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(31,5963)kptarg,valchars10,UNITS(JP),valchars11, ! -------------- EFF
     &UNITS(JP),SET2
      write(150+JP,5963)kptarg,valchars10,UNITS(JP),valchars11, ! ------- Di EFF
     &UNITS(JP),SET2
 5963 format(
     &'Achieved river quality: ',i2,'-percentile =',a10,1x,A4,
     &16x,'Discharge 95-percentile =',a10,1x,A4,4x,a13/110('='))
      
      
      else !  ie eq. 09 ========================================================
      write(01,5964)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(31,5964)kptarg,valchars10,UNITS(JP),valchars11, ! -------------- EFF
     &UNITS(JP),SET2
      write(150+JP,5964)kptarg,valchars10,UNITS(JP),valchars11, ! ------- Di EFF
     &UNITS(JP),SET2
 5964 format('"Achieved" quality: ',i6,'-percentile =',a10,1x,A4,
     &16x,'Discharge 95-percentile =',a10,1x,A4,4x,a13/110('='))
      endif ! if ( ical .ne. 09 ) ==============================================
      

      
      if ( ifeffcsv .eq. 1 ) then ! ============================================
      write(130+JP,7512)GIScode(feeture),unamex, ! --------- TARGET ----- Ei.CSV
     &rname(ireach),dname(JP),Trqs,JT(KFEAT),MRQS(JP)
 7512 format(' ',a40,',',a40,',',a16,',',
     &'Target percentile river quality',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      write(130+JP,7513)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECM,JT(KFEAT)
 7513 format(' ',a40,',',a40,',',a16,',',
     &'Required discharge quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',,402')
      write(130+JP,7514)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),JT(KFEAT),MRQS(JP)
 7514 format(' ',a40,',',a40,',',a16,',',
     &'Achieved percentile river quality',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      write(130+JP,7515)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),TECX,JT(KFEAT),MRQS(JP)
 7515 format(' ',a40,',',a40,',',a16,',',
     &'Discharge 95-percentile',
     &',',a11,1(',',1pe12.4),',,',2(',',i4),',402')
      endif ! if ( ifeffcsv .eq. 1 ) ===========================================
      endif ! if ( nobigout .le. 0 .and. icurrent .eq. 0 ) #####################
      
      icurrent = 0

*     check the need to set discharge standard between the specified upper +++++
*     and lower bounds +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 9995 if ( IBOUND .eq. 1 ) then ! do not check bounds ++++++++++++++++++++++++++
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 8888
      endif ! do not check bounds ++++++++++++++++++++++++++++++++++++++++++++++

      if ( Kdecision(JP) .eq. 9 ) goto 9996

*     set standards within BOUNDS ==============================================     
*     check standard is compatible with the boundaries set for good and bad ----
*     discharge quality --------------------------------------------------------
      if ( TECX .gt. WEQ(3,JP) .or. TECM .gt. WEQ(1,JP) ) then
      TECM = WEQ(1,JP)
      TECS = WEQ(2,JP)
      TECX = WEQ(3,JP)

      ECM = TECM 
      ECS = TECS
      XEFF(JP) = TECX
      XECM(JP) = TECM

      IMPOZZ = 98 ! used only in Run Type 7 -----------------------------------
      goto 8888 ! re-calulate river quality after over-riding discharge quality
      endif ! if ( TECX .gt. WEQ(3,JP) ----------------------------------------

*     check whether effluent quality is better than best possible --------------
      if ( detype (JP) .ne. 104 ) then
      if ( TECX .gt. BEQ(3,JP) ) goto 1633
      TECM = BEQ(1,JP)
      TECS = BEQ(2,JP)
      TECX = BEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 99 ! effluent quality is better than best possible
      
      if ( ECX .lt. TECX ) then
      TECM = ecm
      TECS = ecs
      TECX = ecx
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 95 ! current effluent quality is better "best"
      endif

      else ! if ( detype (JP) = 104 )
      if (TECX .lt. BEQ(3,JP)) goto 1633
      TECM = BEQ(1,JP)
      TECS = BEQ(2,JP)
      TECX = BEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 99 ! effluent quality is better than best possible
      
      if ( ECX .gt. TECX ) then
      TECM = ecm
      TECS = ecs
      TECX = ecx
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 95 ! current effluent quality is better "best"
      endif

      endif ! if ( detype (JP) .ne. 104 ) --------------------------------------
      goto 8888 ! re-calulate river quality after over-riding discharge quality- 

       
     
 1633 continue

      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 9996  

*     ==========================================================================
*     re-calulate river quality after over-riding the values calculated to =====
*     the target river quality =================================================
 8888 continue ! re-calulate river quality after over-riding the values ========

      
      do is = 1, NS
      CMS(jp,is) = UQ(IS) ! load the upstream river quality
      FMS(IS) = UF(IS) ! load the upstream river flow
      do ip = 1, n2prop
      CTMS(ip,JP,IS) = UCTMS(ip,JP,IS) ! concentrations from types of feature ---
      enddo
      enddo

      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)

*     set up the correlation ....  (also done in BIAS ) ========================
      if ( IQDIST .gt. 4 .and. QTYPE (jp) .ne. 4 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif

      call inititialise the stores for the estimates of load 
      call set up data for added flow and quality

      ELOAD (JP,I13) = 0.0
      ELOADS (JP) = 0.0

      do 2222 IS = 1,NS
*     the contributions from different sources of pollution ===================
      do ip = 1, n2prop ! ##################################### needs attention
      RLC(ip) = CTMS(ip, JP, IS )
      enddo

      imonth = qmonth (is) ! set the month for this shot
*     prepare for monthly structure input of loads ============================
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure for input of loads
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS,R1,R2,R3,R4)
      RF = FMS(IS)
      RC = CMS(JP,IS)

      call get a flow for the stream or discharge (IS, R3, EF)
*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 6 .or. JQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     JQDIST .eq. 9 .or. JQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
      EFMB = 1.0 ! set the flows to 1.0 ----------------------------------------
      endif

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC

*     mass balance for pruning =================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      CMS(JP,IS) = ( RF * RC + EFMB * EQ(IS) ) / (RF + EF) ! pruning
      RQ(IS) = CMS(JP,IS)
      fq(IS) = RF + EF
      dfms(IS) = RF + EF ! store the downstream flow for extra calculations ----
      endif


*     concentrations from various types of feature *****************************
*     point source inputs ======================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      if ( ifdiffuse .eq. 0 ) then
*     apply to all discharges (3) (5) (12) (39) (60) (61) ------ load categories
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61) then
      CTMS(1,JP,IS) = ( (RF * RLC(1)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(1,JP,IS) = ( (RF * RLC(1)) ) / (RF + EF)
      endif
*     apply to sewage works (3) -------------------------------- load categories
      if ( JT(feeture) .eq. 03) then
      CTMS(2,JP,IS) = ( (RF * RLC(2)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(2,JP,IS) = ( (RF * RLC(2)) ) / (RF + EF)
      endif
*     apply to intermittent discharges (12) -------------------- load categories
      if ( JT(feeture) .eq. 12) then
      CTMS(3,JP,IS) = ( (RF * RLC(3)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(3,JP,IS) = ( (RF * RLC(3)) ) / (RF + EF)
      endif
*     apply to industrial discharges (5) ----------------------- load categories
      if ( JT(feeture) .eq. 05 ) then
      CTMS(4,JP,IS) = ( (RF * RLC(4)) + (EFMB * EC) ) / (RF + EF) ! - industrial
      else
      CTMS(4,JP,IS) = ( (RF * RLC(4)) ) / (RF + EF) ! --------------- industrial
      endif
*     apply to mine waters (39) -------------------------------- load categories
      if ( JT(feeture) .eq. 39) then
      CTMS(5,JP,IS) = ( (RF * RLC(5)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(5,JP,IS) = ( (RF * RLC(5)) ) / (RF + EF)
      endif
      
*     apply to Other Point Sources (60) ------------------------ load categories
      if ( JT(feeture) .eq. 60) then
      CTMS(28,JP,IS) = ( (RF * RLC(28)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(28,JP,IS) = ( (RF * RLC(28)) ) / (RF + EF)
      endif
*     apply to private wastewaters (61) ------------------------ load categories
      if ( JT(feeture) .eq. 61) then
      CTMS(29,JP,IS) = ( (RF * RLC(29)) + (EFMB * EC) ) / (RF + EF)
      else
      CTMS(29,JP,IS) = ( (RF * RLC(29)) ) / (RF + EF)
      endif

      
*     dilute the remaining contributions ---------------------------------------
      do ip = 6, nprop-2 
      CTMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      enddo
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( RF + EF .gt. 1.0e-8 )
*     ****************************************************** point source inputs 
      FMS(IS) = FQ(IS) ! set flow to that d/s of discharge

*     calculate the proportion of effluent flow in each shot -------------------
*     apply to (3) (5) (12) (39) (60) (61) ------------- proportions of effluent
      EFP = EFMS (IS) ! ------- retrieve the proportion of effluent in each shot
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 .or.
     &     JT(feeture) .eq. 60 .or. JT(feeture) .eq. 61 ) then
      EFMS(IS) = ( RF * EFP + EFMB ) / (RF + EF)
      else
      EFMS(IS) = ( RF * EFP ) / (RF + EF)
      endif

*     and accumulate the load --------------------------------------------------
      if ( QTYPE (JP) .ne. 4 ) then
      K13 = imonth + 1
      if ( ifdiffuse .eq. 0 ) then
      XLD = EFMB * EQ(IS) ! discharge load from shot number IS -----------------
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,I13) = ELOAD60 (JP,I13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,I13) = ELOAD61 (JP,I13) 
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,K13) = ELOAD60 (JP,K13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,K13) = ELOAD61 (JP,K13) 
     &                                           + XLD
      endif ! if ( ifdiffuse .eq. 0 )
      NSM (JP,I13) = NSM (JP,I13) + 1 ! number of shots per month
      NSM (JP,K13) = NSM (JP,K13) + 1
      endif ! if ( QTYPE (JP) .ne. 4 ) -----------------------------------------

 2222 continue

*     calculate the loads ======================================================
      if ( QTYPE (JP) .ne. 4 ) then
      if ( kdecision(JP) .ne. 8 ) then
      if ( ifdiffuse .eq. 0 ) then ! ======= calculate the annual effluent loads
  
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,I13) = ELOAD60 (JP,I13) 
     &                                           / float(NS)
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,I13) = ELOAD61 (JP,I13) 
     &                                           / float(NS)
      endif ! if ( ifdiffuse .eq. 0 ) ===== calculated the annual effluent loads
      
      if ( munthly structure .eq. 1 ) then ! = calculate  monthly effluent loads
      do J13 = 2, N13 ! ================================ loop through the months
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
      if ( JT(feeture) .eq. 60) ELOAD60 (JP,J13) = ELOAD60 (JP,J13) 
     &                                           / float(NSM (JP,J13))
      if ( JT(feeture) .eq. 61) ELOAD61 (JP,J13) = ELOAD61 (JP,J13) 
     &                                           / float(NSM (JP,J13))
      endif ! if ( ifdiffuse .eq. 0 ) ======= calculated  monthly effluent loads
      endif ! if ( NSM(JP,J13) .gt. 1 ) ========================================
      enddo ! do J13 = 2, N13 ========================== loop through the months
      endif ! ====================================== calculate the monthly loads
      endif ! if ( kdecision(JP) .ne. 8 )
      endif ! if ( QTYPE (JP) .ne. 4 ) =========================================

 9996 continue  

      if ( kdecision(JP) .ne. 8 ) then
      call load calculation per determinand
      call calculate summaries of river flow
      call calculate summaries of river quality (JP,RQ)
      endif ! if ( kdecision(JP) .ne. 8 )

 9999 return 
      end