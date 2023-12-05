*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river --------------
*     ==========================================================================
*     file name: Targetq95.FOR    1274 lines -----------------------------------
*     compute effluent quality needed to meet the downstream river target ------
*     --------------------------------------------------------------------------
      subroutine meet river targets set as percentiles
      include 'COM.FOR'

      Mtargit = 1

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
      ic = RQS(IQSfeet,JP)
      if ( ic .lt. nclass ) classobj = ic
      else
      classobj = 0
      endif
      endif
      
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then ! use reach-spepific target
      targit = abs (class limmits (ic,JP))
      classobj = ic
      endif
      enddo ! do ic = 1, nclass
      endif ! if ( IQSreach .gt. 0 )
	RQO(JP) = targit ! use the target for graphs -----------------------------
	MRQO(JP) = MRQS(JP)
      kdecision(JP) = 2 ! target met - imposed river needs permit --------------
	endif ! if ( QTYPE (JP) .ne. 4 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) 
      
*     get the summary statistic ... 1 mean ...  2 95-percentile PPPPPPPPPPPPPPPP
*     3 90-percentile ...  4 5-percentile ... 5 10-percentile PPPPPPPPPPPPPPPPPP            
      Mtargit = MRQS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
*     switch for percentile for river quality targets PPPPPPPPPPPPPPPPPPPPPPPPPP
      kptarg = 95 ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      if ( Mtargit .eq. 3 ) kptarg = 90 ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

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
      write(21,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(31,1586)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
 1586 format(110('-')/'Effluent load is too small to consider for ',A11,
     &' the current effluent quality has been retained .....'/110('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! trivial discharge - retain current discharge quality 
      call accumulate the loads
      call get summaries of river quality from the shots
      return ! return to subroutine effluent discharge -------------------------
      endif

*     check for no standard ----------------------------------------------------
      if ( targit .lt. 1.0e-8 ) then
      kdecision(JP) = 1 ! no target - retain current discharge quality ---------
      if ( nobigout .le. 0 ) then
      call sort format 2 (ECM,ECS)
      write(01,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(21,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
      write(31,1486)DNAME(JP),valchars10,units(JP),valchars11,units(JP)
 1486 format(77('-')/'No target for ',A11,' .... ',
     &'the current effluent quality has been retained'/77('-')/
     &42x,'              Mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      ifdiffuse = 0
      call mass balance ! no target -  retain current discharge quality ---------
      call accumulate the loads
      call get summaries of river quality from the shots
      return
      endif ! if ( targit .lt. 1.0e-8 ) ----------------------------------------

*     the discharge is not trivial ---------------------------------------------
*     initialise the flag that will mark whether river target cannot be met ----
      IMPOZZ = 0
*     compute the effluent standard needed to meet the river target ------------
      if ( nobigout .le. 0 ) then
      write(01,1705)DNAME(JP)
      write(21,1705)DNAME(JP)
      write(31,1705)DNAME(JP)
 1705 format(110('-')/'Effluent quality needed to achieve river ',
     &'quality target for ',A11,'...'/110('-'))
      call get effluent quality 95 percentile

*     write out the details for determinands -----------------------------------
      call sort format 2 (C(JP,1),ECM) ! mean quality
      write(01,1954)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,1954)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1954)valchars10,UNITS(JP),valchars11,UNITS(JP)
 1954 format('Upstream river quality:          Mean =',a10,1x,a4,
     &4x,'Current discharge quality:     Mean =',a10,1x,a4)
      call sort format 2 (C(JP,2),pollution data(IQ,JP,2)) ! standard deviation
      write(01,1955)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,1955)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1955)valchars10,UNITS(JP),valchars11,UNITS(JP)
 1955 format(19x,'Standard deviation =',a10,1x,a4,
     &21x,'Standard deviation =',a10,1x,a4)
      call sort format 1 (C(JP,4)) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(01,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(21,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      write(31,3954)valchars10,UNITS(JP) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
 3954 format(24x,'90-percentile =',a10,1x,a4) ! PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      call sort format 2 (C(JP,3),ECX)
      write(01,1956)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,1956)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1956)valchars10,UNITS(JP),valchars11,UNITS(JP)
 1956 format(24x,'95-percentile =',a10,1x,a4,
     &26x,'95-percentile =',a10,1x,a4/110('-'))

      if ( effcsv . eq. 1 ) then
      write(130+JP,7500)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),Flow(1),Flow(2)
 7500 format(a40,',',a40,',',a16,',',
     &'Upstream river flow',
     &',',a11,2(',',1pe12.4),',,,','402')
      write(130+JP,7501)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,3),JT(KFEAT)
 7501 format(a40,',',a40,',',a16,',',
     &'Upstream river quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      write(130+JP,7502)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),EFM,EFS,JT(KFEAT)
 7502 format(a40,',',a40,',',a16,',',
     &'Effluent flow',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      write(130+JP,7503)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),ECM,ECS,ECX,JT(KFEAT)
 7503 format(a40,',',a40,',',a16,',',
     &'Effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      write(130+JP,7504)GIScode(feeture),unamex,
     &rname(ireach),kptarg,dname(JP),targit,0,0,Mtargit
 7504 format(a40,',',a40,',',a16,',',
     &'River quality standard (',i2,'-percentile)',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      endif
      endif

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
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 7 ! target not met - imposed specified good quality ------
      IMPOZZ = 77 ! impossible to meet the river target - impose GOOD ----------

      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(21,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,7386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
 7386 format('The river target is unachievable for ',a11,4x,
     &'... imposed good effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('-'))

      if ( effcsv . eq. 1 ) then
      write(130+JP,7505)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,TECX,JT(KFEAT)
 7505 format(a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      endif ! if ( effcsv . eq. 1 ) then
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
      call accumulate the loads
      call get summaries of river quality from the shots
      return ! return to subroutine effluent discharge -------------------------

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
      write(21,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
 8386 format('The river target is unachievable for ',a11,1x,
     &'... retained current effluent quality:  Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('-'))
      if ( effcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
 7515 format(a40,',',a40,',',a16,',',
     &'Retained current effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      endif ! if ( effcsv . eq. 1 ) 
      endif ! if ( nobigout .le. 0 )

      call load the upstream flow and quality 
      
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
      call mass balance ! from meet river targets set as averages
      call accumulate the loads
      call get summaries of river quality from the shots
      return ! return to subroutine effluent discharge -------------------------
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
      write(21,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,8386)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      if ( effcsv . eq. 1 ) then
      write(130+JP,7515)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
      endif ! if ( effcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )

      call load the upstream flow and quality 
      
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
      call mass balance ! from meet river targets set as averages
      call accumulate the loads
      call get summaries of river quality from the shots
      return ! return to subroutine effluent discharge -------------------------

      endif ! if ( ECX .gt. GEQ(1,JP) ) ++++++++++++++++++++++++++++++++++++++++
      endif ! if ( IMPOZZ .eq. 1 ) +++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




*     check that the selected effluent quality is technically feasible ---------
*     constrain the effluent standard to the best achievable where existing ----
*     quality is worse than this -----------------------------------------------
      if ( IMPOZZ .eq. 99 ) then
      kdecision(JP) = 3 ! target not met - imposed best achievable -------------
      if ( nobigout .le. 0 ) then
      call sort format 3 ( TECM,TECS,TECX )
      write(01,1582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(21,1582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
      write(31,1582)valchars10,UNITS(JP),valchars11,UNITS(JP),
     &valchars12,UNITS(JP)
 1582 format('Effluent quality is set as that defined as best',
     &' achievable ...',27x,'Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
     &80x,'95-PERCENTILE =',a10,1x,A4/110('-'))
      endif
      if ( effcsv . eq. 1 ) then
      write(130+JP,7506)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
 7506 format(a40,',',a40,',',a16,',',
     &'Best achievable effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      endif
      return
      endif ! if ( IMPOZZ .eq. 99 ) --------------------------------------------

*     stop effluent quality being made too poor whilst still achieving the 77777
*     river targets 777777777777777777777777777777777777777777777777777777777777
      if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) then
      kdecision(JP) = 4 ! target not threatened - imposed worst defined --------
      if ( nobigout .le. 0 ) then
      call sort format 2 (targit,TECM)
      write(01,1388)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,1388)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,1388)valchars10,UNITS(JP),valchars11,UNITS(JP)
 1388 format('Effluent quality could be very bad yet ',
     &'still meet the river target of ...',22x,a10,1x,a4/110('-')/
     &'Effluent quality imposed as the worst you have specified ... ',
     &28x,'Mean =',a10,1x,a4)
      call sort format 2 (TECS,TECX)
      write(01,4388)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,4388)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,4388)valchars10,UNITS(JP),valchars11,UNITS(JP)
 4388 format(75x,'Standard deviation =',a10,1x,a4/
     &80x,'95-percentile =',a10,1x,a4/110('-'))
      if ( effcsv . eq. 1 ) then
      write(130+JP,7507)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,TECX,Mtargit
 7507 format(a40,',',a40,',',a16,',',
     &'Worst permissible effluent quality',
     &',',a11,3(',',1pe12.4),',',i4,',402')
      endif ! if ( effcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
      return
      endif ! if ( IMPOZZ .eq. 98 .and. ICAL .eq. 07 ) 7777777777777777777777777

*     use special policy achieving river targets but allowing no deterioration 8
*     in river quality in cases where river is already inside the target with 88
*     current effluent quality 8888888888888888888888888888888888888888888888888
      if ( ICAL .ne. 08 .and. ical .ne. 09 ) return ! 99999999999999999999999999
      
*     check the cases where the effluent quality is already too good for the 888
*     river quality target 88888888888888888888888888888888888888888888888888888
*     check whether the calculated effluent quality is laxer than the current 88
*     effluent quality 888888888888888888888888888888888888888888888888888888888
      if ( pollution data (IQ,JP,1) .gt. TECM ) return
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
      call get river quality downstream of the effluent

*     ##########################################################################      
*     Rest of code in this subroutine is NOT USED ############################## 
*     ######################################################### SECTION NOT USED      
*     check for deterioration less than a target factor such as 10% ############
*     NOT USED at present ... needs u/s river quality to be reset ##############
*     ######################################################### SECTION NOT USED      
      target factor = 0.00 ! currently set a zero ############# SECTION NOT USED
      target fraction = target factor * current downstream quality ! ## NOT USED
      target gap = targit - current downstream quality ! ###### SECTION NOT USED
      if ( target gap .ge. target fraction + 0.00000001) then ! SECTION NOT USED
*     deterioration exceeds the target factor ################# SECTION NOT USED
*     reset the target to give allowed deterioration ########## SECTION NOT USED
      if ( target factor .gt. 0.0 ) then ! #################### SECTION NOT USED
      targit = target fraction + current downstream quality ! # SECTION NOT USED
      if ( nobigout .le. 0 ) then
      call sort format 2 (current downstream quality,targit)
      write(01,1852)valchars10,Units(JP),valchars11,Units(JP)
      write(21,1852)valchars10,Units(JP),valchars11,Units(JP)
      write(31,1852)valchars10,Units(JP),valchars11,Units(JP)
 1852 format('Achieving the river target would permit a',
     &' deterioration in river quality ...',
     &'this has been cut to 10 per cent'/
     &'The quality downstream of the current discharge is ',a10,1x,A4,
     &' ... the new target is  ',a10,1x,A4/110('-'))
*     ######################################################### SECTION NOT USED
      if ( effcsv . eq. 1 ) then
      write(130+JP,7508)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),current downstream quality,Mtargit,
     &targit,Mtargit
 7508 format(a40,',',a40,',',a16,',',
     &'Current downstream quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'New mean target',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      endif ! if ( effcsv . eq. 1 ) ########################### SECTION NOT USED
      endif ! if ( nobigout .le. 0 ) ########################## SECTION NOT USED
*     calculate the required discharge quality for this 10% deterioration ######
*     this is TECX ... the discharge standard will not be set between bounds ###
      kdecision(JP) = 5 ! target not threatened - 10% deterioration ############
      IBOUND = 1 ! set discharge standards without checking bounds #############
      call meet percentile target in the river (IMPOZZ,targit,IBOUND) !  10% det
      if ( IBOUND .eq. 1 ) then ! do not look at bounds ####### SECTION NOT USED
      if ( nobigout .le. 0 ) then
      write(01,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP)
      write(21,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP)
      write(31,1832)targit,UNITS(JP),TECM,UNITS(JP),TECX,UNITS(JP)
 1832 format(110('-')/
     &'Required river quality:  95-percentile =',F9.2,1x,A4,'  ',
     &'Required effluent quality         Mean =',F9.2,1x,A4/
     &81x,'95-PERCENTILE =',F9.2,1x,A4/110('-'))
      if ( effcsv . eq. 1 ) then
      write(130+JP,7509)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),targit,Mtargit,TECM,TECX,Mtargit
 7509 format(a40,',',a40,',',a16,',',
     &'Required mean river quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402'/
     &'Required effluent quality',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( effcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( IBOUND .eq. 1 ) do not look at bounds ###### SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     set a variable equal to a 20% boost to current discharge pollution #######
      discharge factor = 1.20
      XCM = discharge factor * pollution data(IQ,JP,1)
      XCMKeep = XCM
      XCSKeep = discharge factor * pollution data(IQ,JP,2)
*     ######################################################### SECTION NOT USED
*     check whether the new discharge quality exceeds this #####################
      if (TECM .gt. XCM) goto 1892 ! ########################## SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the river target is met ##################################################
*     if this is done the deterioration in the river exceeds 10% ###############
*     so the effluent standard has been re-calculated to give a 10% ############
*     deterioration in river quality ###########################################
*     this change allows less than a 20% deterioration in effluent quality #
*     the calculation of the discharge quality is finished #####################
      return
      endif ! if ( target factor .gt. 0.0 ) target deterioration ###############
*     the river target is achieved ############################ SECTION NOT USED
*     the new effluent standard is tighter than the old or the deterioration ###
*     in the river is less than 10% .... and the deterioration embodied in the #
*     new effluent standard is less than 10% ################## SECTION NOT USED
      endif ! if ( target gap .ge. target fraction + 0.00000001) ###############
      return ! ################################################ SECTION NOT USED
*     ######################################################### SECTION NOT USED
*     the calculated effluent standard is laxer than the current effluent ######
*     quality, but the deterioration in river quality is acceptable ############
*     (less than 10%) ######################################### SECTION NOT USED
*     nonetheless the effluent standard will be constrained to a 20% ###########
*     deterioration from the current effluent quality ######### SECTION NOT USED
 1892 continue ! ############################################## SECTION NOT USED
*     ######################################################### SECTION NOT USED

      
      
      
      ECM1 = ECM
      ECX1 = ECX
      ECM = XCMKEEP
      ECS = XCSKEEP
*     compute the percentile of effluent quality ###############################
      call get effluent quality 95 percentile
      TECM = ECM
      TECS = ECS
      TECX = ECX
      XEFF(JP) = TECX
      XECM(JP) = TECM
      kdecision(JP) = 6 ! target not threatened - 10% deterioration ###########
      if ( nobigout .le. 0 ) then
      write(01,1983)ECM,UNITS(JP),ECX,UNITS(JP)
      write(21,1983)ECM,UNITS(JP),ECX,UNITS(JP)
      write(31,1983)ECM,UNITS(JP),ECX,UNITS(JP)
 1983 format(
     &'Achieving the river target or a 10% deterioration would',
     &' permit too bad an effluent quality ...'/110('-')/
     &'Deterioration in effluent quality held at 20% ...',
     &6x,' Resulting effluent quality ...    Mean =',F9.2,1x,A4/
     &72x,'         95-PERCENTILE =',F9.2,1x,A4/110('-'))
*     ##########################################################################
      if ( effcsv . eq. 1 ) then
      write(130+JP,7511)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),ECM,ECX,Mtargit
 7511 format(a40,',',a40,',',a16,',',
     &'Deterioration in effluent quality held at 20%',
     &',',a11,2(',',1pe12.4),',,',i4,',402')
      endif ! if ( effcsv . eq. 1 )
      endif ! if ( nobigout .le. 0 )
*     ##########################################################################

      return
      end



*     this routine takes flow and quality data for an effluent and works out ---
*     the effluent quality needed to achieve set percentile of river quality ---
*     downstream ---------------------------------------------------------------
      subroutine meet percentile target in the river
     &(IMPOZZ,TRQS,IBOUND)
      include 'COM.FOR'
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
      do ip = 1, nprop
      ULMS(ip,JP,IS) = LMS(ip,JP,IS )
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
      if ( RCM .lt. 0.00001 ) RCV = RCS / RCM ! 99999999999999999999999999999999
      if ( kptarg .eq. 95 ) RCX = C(JP,3) ! 95-percentile PPPPPPPPPPPPPPPPPPPPPP
      if ( kptarg .eq. 90 ) RCX = C(JP,4) ! 90-percentile PPPPPPPPPPPPPPPPPPPPPP
      if ( kptarg .eq. 99 ) RCX = C(JP,5) ! 99-percentile PPPPPPPPPPPPPPPPPPPPPP

      if ( n150 .eq. 1 ) then
      if ( ical .eq. 09 .and. classobj .gt. 1 ) then
      xlim1 = 0.0
      xlim2 = abs( class limmits (classobj,JP) )
      xlim1 = abs( class limmits (classobj - 1,JP) )
      xgap = xlim2 - xlim1
      if ( abs (xgap) .lt. 1.0e-07 ) then
      classobj = 0    
      else
      RCX = xlim1 + 0.5 * xgap
      xpoint = 90.0
      if (kstat .eq. 2 ) xpoint = 95.0
      if (kstat .eq. 6 ) xpoint = 99.0
      call change to log domain (RCX, xpoint, 1.0, RCV, RCM, RCS)

      ipoint = xpoint
	call ASSEM (xlim1,xlim2,SET1)
      write(01,3000)SET1,ipoint,RCX,RCM,RCS
      write(21,3000)SET1,ipoint,RCX,RCM,RCS
      write(31,3000)SET1,ipoint,RCX,RCM,RCS
 3000 format(110('=')/
     &'Upstream quality imposed as the middle of the target class ',
     &'(Run Type 9) ... Limits: ',a13/110('=')/
     &21x,i5,'-percentile =',f10.3/
     &19x,'              Mean =',f10.3/
     &19x,'Standard deviation =',f10.3/
     &110('='))
*     create the river quality data 99999999999999999999999999999999999999999999
      call generate mode 9 river quality (RCM,RCS)
      endif ! if ( abs (xgap) .lt. 1.0e-07 )
      endif ! if ( ical .eq. 09 .and. classobj .gt. 1 ) 999999999999999999999999
      endif ! if ( n150 .eq. 1 ) 99999999999999999999999999999999999999999999999

      call get summaries of loads
      
      RFM = FLOW(1) ! the mean river flow

*     compute need for improvement to discharge quality ------------------------
      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then ! 99999999999
      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)
      endif ! if ( IQDIST .lt. 4 )

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

      do 2002 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS, R1, R2, R3, R4)
      RF = FMS(IS)
      if ( ical .eq. 09 ) then ! 99999999999999999999999999999999999999999999999
      RC = CMS(JP,IS) ! 99999999999999999999999999999999999999999999999999999999
      else ! 9999999999999999999999999999999999999999999999999999999999999999999
      RC = CMS(JP,IS)
      endif ! 999999999999999999999999999999999999999999999999999999999999999999

*     get the shots for the discharge ------------------------------------------
      call get the flows of the stream or discharge ( IS, R3, EF )

*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 .or.
     &     JQDIST .eq. 6 .or. JQDIST .eq. 7 .or. JQDIST .eq. 9 ) then
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

      TRCX = Get river percentile (RQ)
      TECX = Get effluent percentile (EQ)
*     TECM = Get the mean discharge quality (EQ)
      XEFF(JP) = TECX
*     XECM(JP) = TECM

      if ( nobigout .le. 0 ) then
      call sort format 2 (TRCX,TECX)
      if ( ical .eq. 9 .and. classobj .gt. 1 ) then
      write(01,3964)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,3964)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3964)valchars10,UNITS(JP),valchars11,UNITS(JP)
 3964 format('"Achieved" river quality:  Percentile =',a10,1x,A4,
     &'  from current discharge quality:   Q95 =',a10,1x,A4/110('='))
      else
      write(01,3963)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(21,3963)valchars10,UNITS(JP),valchars11,UNITS(JP)
      write(31,3963)valchars10,UNITS(JP),valchars11,UNITS(JP)
 3963 format('Achieved river quality:    Percentile =',a10,1x,A4,
     &'  from current discharge quality:   Q95 =',a10,1x,A4/110('-'))
      endif
      if ( effcsv .eq. 1 ) then
      write(130+JP,7500)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TRCX,TECX
 7500 format(a40,',',a40,',',a16,',',
     &'River quality from current discharge',
     &',',a11,2(',',1pe12.4),',,,','402')
      endif ! if ( effcsv .eq. 1 )
      endif ! if ( nobigout .le. 0 )

*     check whether current discharge quality meets the river target ===========
      icurrent = 0
      if ( ical .eq. 08 .or. ical .eq. 09 ) then ! 99999999999999999999999999999
      if ( TRCX .le. TRQS ) then

      if ( n151 .lt. 99 ) then ! retain current discharge quality ---------------  
      if ( nobigout .le. 0 ) then
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then
      write(01,892)
      write(21,892)
      write(31,892)
  892 format('The river quality target is achieved without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('-'))
      else
      write(01,894)
      write(21,894)
      write(31,894)
  894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('='))
      endif
      endif
      icurrent = 1
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      goto 9095 ! target achieved with current discharge quality ---------------
      endif ! if ( n151 .lt. 99 ) then ! retain current discharge quality -------
        
      if ( n151 .eq. 99 ) then ! seek to impose "good" discharge quality %%%%%%%% 
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
      write(21,2386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
      write(31,2386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
 2386 format(
     &'The river target is already being achieved ',4x,
     &'...... imposed "good" effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('-'))
      else
      write(01,3386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
      write(21,3386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
      write(31,3386)valchars10,UNITS(JP),valchars11,UNITS(JP)
*    &valchars12,UNITS(JP)
 3386 format(
     &'The river target is already being "achieved" ',2x,
     &'...... imposed good effluent quality:   Mean =',a10,1x,A4/
     &75x,'Standard deviation =',a10,1x,A4/
*    &80x,'95-PERCENTILE =',a10,1x,A4/
     &110('-'))
          
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 ) %%%%%%%%%%%%%%%%%%%%%%%%%
      if ( effcsv . eq. 1 ) then
      write(130+JP,2505)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,TECS,JT(KFEAT)
 2505 format(a40,',',a40,',',a16,',',
     &'Imposed good effluent quality',
     &',',a11,2(',',1pe12.4),',',i4,',402')
      endif ! if ( effcsv . eq. 1 ) then %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      endif ! if ( nobigout .le. 0 ) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      else ! when ( GEQ(1,JP) .le. 0.00001 and ECM .gt. GEQ(1,JP) ) then ! %%%%%
      if ( nobigout .le. 0 ) then
      if ( ical .eq. 08 .or. classobj .eq. 0 ) then
      write(01,4892)
      write(21,4892)
      write(31,4892)
 4892 format('The river quality target is achieved without',
     &' improving the effluent discharge ....... ',
     &'current quality retained'/110('-'))
      else
      write(01,4894)
      write(21,4894)
      write(31,4894)
 4894 format('The river quality target is "achieved" without',
     &' improving the effluent discharge ..... ',
     &'current quality retained'/110('='))
      endif ! if ( ical .eq. 08 .or. classobj .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      icurrent = 1 ! retained current ------------------------------------------
      kdecision(JP) = 9 ! target not threatened - retained current -------------
      goto 9095 ! target achieved with current discharge quality ---------------
      endif
      goto 9095 ! impose "good" discharge quality %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      endif ! if ( n151 .eq. 99 ) impose "good" discharge quality %%%%%%%%%%%%%%%
      
      
      endif ! if ( TRCX .le. TRQS )
      endif ! if ( ical .eq. 08 or 09 ) ! 99999999999999999999999999999999999999
      endif ! if ( ical .eq. 07, 08 or 09 ) ! 9999999999999999999999999999999999

      IDIL = 0 ! initialise switch for dealing with unachievable targets
      KDIL = 0 ! initialise switch for dealing with unachievable targets
      if ( RCM .gt. 1.0E-20 ) then
*     is the river target stricter than the current upstream river quality ? ---
      if ( RCX .ge. TRQS ) then
      if ( detype (jp) .ne. 104 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (RCX,TRQS)
      write(01,764)DNAME(JP),valchars10,valchars11
      write(21,764)DNAME(JP),valchars10,valchars11
      write(35,764)DNAME(JP),valchars10,valchars11
      write(31,764)DNAME(JP),valchars10,valchars11
  764 format(
     &'WARNING - Upstream river quality for ',a11,' ... ',13x,a10/
     &'is worse than the quality required downstream of the ',
     &'discharge ...',a10/110('-'))
      endif ! if ( nobigout .le. 0 )
*     set the switches ---------------------------------------------------------
      IDIL = 1
      KDIL = 1
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
      IDIL = 1
      KDIL = 1
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

  996 ITER = ITER + 1

      if ( IQDIST .lt. 4 ) then
      call get the summary statistics of discharge quality (TECM,TECS)
      call bias in river or discharge flow and quality (TECM,TECS)
      endif

*     set up the correlation ....  (also done in BIAS) -------------------------
      if ( IQDIST .ge. 4 .and. Qtype (jp) .ne. 4 ) then
      if ( IQDIST .ne. 6 .and. IQDIST .ne. 7 ) then
      call set up the correlation coefficients ! 4444444444444444444444444444444
      endif
      endif

      call set up data for added flow and quality
      call inititialise the stores for the estimates of load 

      do 2 IS = 1,NS
      imonth = qmonth (is) ! set the month for this shot

*     prepare for monthly structure input of loads -----------------------------
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS, R1, R2, R3, R4)
      RF = FMS(IS)
      RC = CMS(JP,IS)

*     get the shots for the discharge ------------------------------------------
      call get the flows of the stream or discharge ( IS, R3, EF )

*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 .or.
     &     JQDIST .eq. 6 .or. JQDIST .eq. 7 .or. JQDIST .eq. 9 ) then
*     set the flows to 1.0 -----------------------------------------------------
      EFMB = 1.0
      endif

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC

*     mass balance #############################################################
      if ( RF + EF .gt. 1.0e-8 ) then
      RQ(IS) = (RF*RC+EFMB*EQ(IS))/(RF+EF) ! ###################################
      fq(is) = RF + EF
      endif ! ##################################################################

    2 continue

      TRCX = Get river percentile (RQ)
      TECX = Get effluent percentile (EQ)
*     TECM = Get the mean discharge quality (EQ)
      XEFF(JP) = TECX
*     XECM(JP) = TECM

      if ( IDIL .eq. 1 ) then
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
*     write(01,802)
*     write(21,802)
*     write(31,802)
  802 format('The river quality target is not achievable without ',
     &'improving river quality upstream of the discharge ...'/110('-'))
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
      write(21,6000)
      write(03,6000)
      write(09,6000)
      write(33,6000)
      write(31,6000)
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
      write(21,446)
      write(09,446)
      write(33,446)
      write(31,446)
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
  183 TECS = ECV * TECM

      if ( ITER .eq. 1 ) goto 996 ! proceed to the next iteration
*     check whether calculated discharge quality is infeasibly bad =============
      if ( TECM .gt. 300000.0 ) then
      suppress9d = suppress9d + 1
      if ( suppress9d .lt. 6 ) then
      call change colour of text (20) ! bright red
      write( *,8463)uname(feeture),dname(JP)
 8463 format(
     &'* Calculated discharge quality is huge',13x,'...',7x,
     &'at: ',a37,3x,'for ',a11,10('.'))
      call set screen text colour
      else
      if ( suppress9d .gt. 9999 ) suppress9d = 0
      endif
      if ( suppress9d .eq. 6 ) then
      call change colour of text (20) ! bright red
      write( *,3463)uname(feeture)
 3463 format(
     &'* Calculated discharge quality is huge',13x,'...',7x,
     &'at: ',a37,3x,'and other cases .........')
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) then
      write(01,8763)dname(jp),kptarg,TRQS,UNITS(JP),TECM,UNITS(JP),
     &kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)
      write(21,8763)dname(jp),kptarg,TRQS,UNITS(JP),TECM,UNITS(JP),
     &kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)
      write(31,8763)dname(jp),kptarg,TRQS,UNITS(JP),TECM,UNITS(JP),
     &kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)  
 8763 format(110('-')/'The calculated discharge quality is ',
     &'bigger than 300000 ... for ',a11/110('-')/
     &'TARGET river quality:    ',i2,'-PERCENTILE =',F9.2,1x,A4,
     &'  Required discharge quality:       Mean =',F9.3,1x,A4/
     &'Achieved river quality:  ',i2,'-percentile =',F9.2,1x,A4,'  ',
     &13x,'            95-PERCENTILE =',F9.3,1x,A4/110('-'))
      endif
      write(33,8763)dname(jp),kptarg,TRQS,UNITS(JP),TECM,UNITS(JP),
     &kptarg,C(JP,1),UNITS(JP),C(JP,3),UNITS(JP)
      goto 9095 ! ???????? differs from mean which has 9095 (9995)
      else ! if ( TECM .lt. 300000.0 )
*     go to the next iteration -------------------------------------------------
      goto 996
      endif ! if ( TECM .gt. 300000.0 )

 9095 continue ! iterations complete - convergence complete --------------------

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

      call mass balance ! calculate loads --------------------------------------  
*     call accumulate the loads

      call calculate summaries of river flow
      call calculate summaries of river quality (JP,RQ)

*     for a percentile river quality standard ---------------------------------- 
      if ( nobigout .le. 0 .and. icurrent .eq. 0 ) then
          
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
      write(21,4963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET1
      write(31,4963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET1
 4963 format(
     &'TARGET river quality:   ',i2,'-percentile =',a10,1x,a4,
     &'  Required discharge quality:      Mean =',a10,1x,a4,4x,a13)
      call sort format 2 (TRCX,TECX)
      if ( ical .ne. 09 ) then
      write(01,5963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(21,5963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(31,5963)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
 5963 format(
     &'Achieved river quality: ',i2,'-percentile =',a10,1x,A4,
     &15x,'           95-PERCENTILE =',a10,1x,a4,4x,a13/110('-'))
      else
      write(01,5964)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(21,5964)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
      write(31,5964)kptarg,valchars10,UNITS(JP),valchars11,UNITS(JP),
     &SET2
 5964 format(
     &'"Achieved" quality: ',i6,'-percentile =',a10,1x,A4,
     &15x,'           95-PERCENTILE =',a10,1x,a4,4x,a13/110('='))
      endif

      if ( effcsv .eq. 1 ) then
      write(130+JP,7512)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),Trqs,MRQS(JP)
 7512 format(a40,',',a40,',',a16,',',
     &'TARGET percentile river quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      write(130+JP,7513)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECM,JT(KFEAT)
 7513 format(a40,',',a40,',',a16,',',
     &'Required discharge quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      write(130+JP,7514)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),C(JP,1),MRQS(JP)
 7514 format(a40,',',a40,',',a16,',',
     &'Achieved percentile river quality',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      write(130+JP,7515)GIScode(feeture),unamex,
     &rname(ireach),dname(JP),TECX,JT(KFEAT)
 7515 format(a40,',',a40,',',a16,',',
     &'Discharge 95-percentile',
     &',',a11,1(',',1pe12.4),',,,',i4,',402')
      endif
      endif
      icurrent = 0

*     check the need to set discharge standard between the specified upper +++++
*     and lower bounds +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 9995 if ( IBOUND .eq. 1 ) then ! do not check bounds ++++++++++++++++++++++++++
      XEFF(JP) = TECX
      XECM(JP) = TECM
      goto 8888
      endif ! do not check bounds ++++++++++++++++++++++++++++++++++++++++++++++

*     set standards within BOUNDS ==============================================     
*     check standard is compatible with the boundaries set for good and bad ----
*     discharge quality --------------------------------------------------------
      if ( TECX .gt. WEQ(3,JP) .or. TECM .gt. WEQ(1,JP) ) then
      TECM = WEQ(1,JP)
      TECS = WEQ(2,JP)
      TECX = WEQ(3,JP)
      ECM = TECM ! Richard III
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
      IMPOZZ = 99
      else ! if ( detype (JP) = 104 )
      if (TECX .lt. BEQ(3,JP)) goto 1633
      TECM = BEQ(1,JP)
      TECS = BEQ(2,JP)
      TECX = BEQ(3,JP)
      XEFF(JP) = TECX
      XECM(JP) = TECM
      IMPOZZ = 99
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
      do ip = 1, nprop
      LMS(ip,JP,IS) = ULMS(ip,JP,IS)
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
      do ip = 1, nprop
      RLC(ip) = LMS(ip, JP, IS )
      enddo

      imonth = qmonth (is) ! set the month for this shot
*     prepare for monthly structure input of loads ============================
      jqdist = 2
      if ( IQDIST .eq. 8 ) then ! monthly structure
      jqdist = struct0 (imonth)
      endif ! monthly structure

      call get the correlated random numbers (IS, R1, R2, R3, R4)
      RF = FMS(IS)
      RC = CMS(JP,IS)

      call get the flows of the stream or discharge ( IS, R3, EF )
*     deal with data expressed as load -----------------------------------------
      EFMB = EF
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 .or.
     &     JQDIST .eq. 6 .or. JQDIST .eq. 7 .or. JQDIST .eq. 9 ) then
      EFMB = 1.0 ! set the flows to 1.0 ----------------------------------------
      endif

      call get the quality of the discharge (IS,R4,EC,TECM)
      EQ(IS) = EC

*     mass balance for pruning =================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      CMS(JP,IS) = ( RF * RC + EFMB * EQ(IS) ) / (RF + EF) ! pruning
      RQ(IS) = CMS(JP,IS)
      fq(IS) = RF + EF
      dfms(IS) = RF + EF
      endif

*     concentrations from various types of feature *****************************
*     point source inputs ======================================================
      if ( RF + EF .gt. 1.0e-8 ) then
      if ( ifdiffuse .eq. 0 ) then
*     apply to all discharges (3) (5) (12) (39) ---------------- load categories
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39) then
      LMS(1,JP,IS) = ( (RF * RLC(1)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(1,JP,IS) = ( (RF * RLC(1)) ) / (RF + EF)
      endif
*     apply to sewage works (3) -------------------------------- load categories
      if ( JT(feeture) .eq. 03) then
      LMS(2,JP,IS) = ( (RF * RLC(2)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(2,JP,IS) = ( (RF * RLC(2)) ) / (RF + EF)
      endif
*     apply to intermittent discharges (12) -------------------- load categories
      if ( JT(feeture) .eq. 12) then
      LMS(3,JP,IS) = ( (RF * RLC(3)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(3,JP,IS) = ( (RF * RLC(3)) ) / (RF + EF)
      endif
*     apply to industrial discharges (5) ----------------------- load categories
      if ( JT(feeture) .eq. 05 ) then
      LMS(4,JP,IS) = ( (RF * RLC(4)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(4,JP,IS) = ( (RF * RLC(4)) ) / (RF + EF)
      endif
*     apply to mine waters (39) -------------------------------- load categories
      if ( JT(feeture) .eq. 39) then
      LMS(5,JP,IS) = ( (RF * RLC(5)) + (EFMB * EC) ) / (RF + EF)
      else
      LMS(5,JP,IS) = ( (RF * RLC(5)) ) / (RF + EF)
      endif
*     dilute the remaining contributions ---------------------------------------
      do ip = 6, nprop 
      LMS(IP,JP,IS) = (RF * RLC(IP)) / (RF + EF)
      enddo
      endif ! if ( ifdiffuse .eq. 0 )
      endif ! if ( RF + EF .gt. 1.0e-8 )
*     ****************************************************** point source inputs 
      FMS(IS) = FQ(IS) ! set flow to that d/s of discharge

*     calculate the proportion of effluent in each shot ------------------------
*     apply to (3) (5) (12) (39) ----------------------- proportions of effluent
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39) then
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
      ELOAD (JP,K13) = ELOAD (JP,K13) + XLD
      if ( JT(feeture) .eq. 3 ) ELOAD03 (JP,K13) = ELOAD03 (JP,K13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 5 ) ELOAD05 (JP,K13) = ELOAD05 (JP,K13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 12) ELOAD12 (JP,K13) = ELOAD12 (JP,K13) 
     &                                           + XLD
      if ( JT(feeture) .eq. 39) ELOAD39 (JP,K13) = ELOAD39 (JP,K13) 
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
