*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: transformations.for ... 3739 lines ---------------------------------
*     --------------------------------------------------------------------------
*     This file deals with natural purification and certain types of diffuse ---
*     pollution ----------------------------------------------------------------
*     --------------------------------------------------------------------------
*     This file contains 9 subroutines -----------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... add diffuse sources and natural purification (icome)
*     ...... natural purification
*     ...... scale loads after losses (from natural purification)
*     ...... scale loads after an abstraction 
*     ...... sort format 1 (v1)
*     ...... sort format 2 (v1,v2)
*     ...... sort format 3 (v1,v2,v3)
*     ...... fetch (fff,textx)
*     ...... write resulting flow and quality
*     --------------------------------------------------------------------------
      
      subroutine add diffuse sources and natural purification (icome)
      include 'COMMON DATA.FOR'
      dimension RQ(NS) ! temporary store for water quality

      KFEET = KFEAT
      KEEPTYPE = JT (KFEET)
      ifdiffuse = 1

      do jdet = 1, ndet
      check sub total (jdet) = 0.0 ! sub-totals of load
      enddo

      iprime = 0

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
      endif
      enddo


*     straight forward use of this routine - not for Gap Filling ===============
      if ( icome .eq. 0 ) then ! normal use - not for Gap Filling nnnnnnnnnnnnnn
      if ( iend .eq. 0 ) then
      if ( istart .eq. 1 ) then
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( no features .eq. 0 ) then
      write(01,2200) RNAME(IREACH),vname(KFEET)
 2200 format(//110('=')/'Stretch: ',a16,1x,'from the start of the ',
     &'Reach',17x,'to ',a37/110('='))
      else
      write(01,2230)RNAME(IREACH)
 2230 format (//110('=')/'Stretch: ',a16,1x,'from the start of the ',
     &'Reach ...'/110('='))
*    &24x,' to the end of the Reach'/110('='))
      endif ! if ( no features .eq. 0 )
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      istart = 0
      no features = 0
      else ! if ( istart .eq. 1 )
	
      if ( nobigout .le. 0 ) then ! ============================================
      if ( DIST(feeture) .lt. 1.0e-08 ) then ! ---------------------------------  
      if ( JT (feeture) .eq. 13 ) goto 2099 ! Diffuse input (13)
      if ( JT (feeture) .eq. 15 ) goto 2099 ! Diffuse input (15)
      if ( JT (feeture) .eq. 25 ) goto 2099 ! Livestock (25)
      if ( JT (feeture) .eq. 27 ) goto 2099 ! Arable (25)
      if ( JT (feeture) .eq. 29 ) goto 2099 ! Highway runoff (29)
      if ( JT (feeture) .eq. 31 ) goto 2099 ! Urban runoff (31)
      if ( JT (feeture) .eq. 33 ) goto 2099 ! Atmosphere deposition (33)
      if ( JT (feeture) .eq. 35 ) goto 2099 ! Natural background (35)
      if ( JT (feeture) .eq. 37 ) goto 2099 ! Septic tanks (37)
      if ( JT (feeture) .eq. 40 ) goto 2099 ! Aggregated CSOs (40)
      if ( JT (feeture) .eq. 42 ) goto 2099 ! Aggregated STWs (42)
      if ( JT (feeture) .eq. 46 ) goto 2099 ! Diffuse mines (46)
      if ( JT (feeture) .eq. 48 ) goto 2099 ! Birds, boats and angling (48)
      if ( JT (feeture) .eq. 50 ) goto 2099 ! User-named diffuse input (50)
      if ( JT (feeture) .eq. 52 ) goto 2099 ! User-named diffuse input (52)
      if ( JT (feeture) .eq. 54 ) goto 2099 ! User-named diffuse input (54)
      if ( JT (feeture) .eq. 56 ) goto 2099 ! User-named diffuse input (56)
      if ( JT (feeture) .eq. 58 ) goto 2099 ! User-named diffuse input (58)
      endif ! if ( DIST(feeture) .lt. 1.0e-08 ) --------------------------------
      if ( ical .ne. 3 ) then ! ------------------------------------------------
      write(01,2000) RNAME(IREACH), uname (KFEET-1),vname (KFEET)
 2000 format(/////110('=')/'Stretch: ',a16,' from: ',a37,' to ',a37/ ! ----- OUT
     &110('='))
      endif ! ------------------------------------------------------------------
      endif ! if ( nobigout .le. 0 ) then ======================================
 2099 continue     

      endif ! if ( istart .eq. 1 )
      endif ! if ( iend .eq. 0 )

      if ( iend .eq. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,2100) RNAME(IREACH), uname (KFEET)
 2100 format (///110('=')/'Stretch: ',a16,' from: ',a37,17x,
     &' to the end of the Reach'/110('='))
      endif
      endif ! if ( iend .eq. 1 )
      endif ! if ( icome .eq. 0 ) normal use - not for Gap Filling nnnnnnnnnnnnn

      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     ==========================================================================
*     use of this routine for Gap Filling ======================================
      if ( icome .eq. 1001 ) then ! ============================================
      if ( iend .eq. 0 ) then
      if ( kstart .eq. 1 ) then
      write(09,2210) RNAME(IREACH), vname (KFEET+1)
 2210 format (/110('#')/'Stretch: ',a16,1x,'from the start of the ',
     &'Reach to:',1x,a37/110('#'))
      kstart = 0
      else
      write(09,2010) RNAME(IREACH), uname (KFEET),vname (KFEET+1)
 2010 format (110('+')/'Stretch: ',a16,' from ',a37,' to  ',a37/
     &110('+'))
      endif
      endif
      if ( iend .eq. 1 ) then
      write(09,2110) RNAME(IREACH), uname (KFEET)
 2110 format (110('+')/'Stretch: ',a16,' from: ',a37,
     &17x,' to the end of the Reach'/110('+'))
      endif
      endif ! if ( icome .eq. 1001 ) then ! ====================================
*     ==========================================================================
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



      DISX = DISTP
      DISTX = DISTP
      DINT = DISTP

*     the stretch will be split into INTD intervals (set to 1 initially) -------
      INTD = 1

*     add in the reach-type diffuse inflows ------------------------------------
      if ( IDIFFREACH .gt. 0 ) call reach diffuse ! ----------------------------

      if ( lake45 .eq. 1 ) DINT = 1.0 ! check for inflow from a lake -----------
      DINTP = DINT

      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (13)
*     add in diffuse pollution (river type - unassigned) ------------------ (13)
*     --------------------------------------------------------------------- (13)
      add conc = 0 ! -------------------------------------------------------(13)
      if ( KRAPOL13 .ge. 1 ) add conc = 1 ! --------------------------------(13)
      if ( KRFPOL13 .gt. 0 .and. KRQPOL13 .gt. 0 ) then ! ----------------- (13)
      IQ = KRQPOL13
      IF = KRFPOL13
      JTKEEP = JT(KFEET)
      JT(KFEET) = 13
      diffuse type = 13
      if ( DINT .gt. 1.0e-07 ) then ! ------------------------------------- (13)
      call calculate summaries of river flow ! ---------- diffuse pollution (13)
      call get summaries of river quality from the shots ! -------- diffuse (13)

      if ( ical13 .eq. 0 ) then ! ==============================================
      if ( nobigout .le. 0 ) then ! ============================================
      if ( diffuse heading .eq. 0 ) then ! -------------------------------- (13)
      call sort format 1 (dint)
      write(01,5282)valchars10
 5282 format(33x,77('=')33x,'Various diffuse inflows over the next',
     &a10,' kilometres ...'/33x,77('='))
      diffuse heading = 1
      endif ! if ( diffuse heading .eq. 0 ) ------------------------------- (13)

      write(01,7482)IF
      write(34,7482)IF
 7482 format(33x,77('=')/33x,
     &'Insert river-type diffuse inflows (13) ... ',
     &'Flow data set:',I6)

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit ! ------------------------- (13)
      write(34,1482)valchars11,valchars10,funit
 1482 format(33x,77('-')/
     &33x,'Starting river flow',16x,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)
      do J = 1, ndet
	if ( QTYPE (J) .ne. 4 ) then ! ===========================================
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
 1422 format(33x,'Starting ',A11,15x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
      endif ! if ( QTYPE (J) .ne. 4 ) then =====================================
      enddo
      endif ! if ( nobigout .le. 0 ) ===========================================
      endif ! if ( ical13 .eq. 0 ) =============================================
*     add in diffuse pollution (river type - unassigned) ------------------ (13)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then ! ===========================
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( diffuse heading .eq. 0 ) then ! -------------------------------------
      write(01,5282)dint
      diffuse heading = 1
      endif ! if ( diffuse heading .eq. 0 ) ------------------------------------
      
      iprime = 1

      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (13)
      write(34,2082)valchars12,valchars11,valchars10,funit
 2082 format(33x,
     &'Flow added over ',a10,' km',6x,'95%tle =',a10,3x, ! --------- reach-based
     &'Mean =',a10,1x,a4/33x,77('-')) ! ----------------------------------- (13)
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) ==========================

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then ! ============================================
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4190
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
 8940 format(/'*** The specified non-parametric data do not exist ...',
     &' File: ',a64)
	call stop
 4190 if ( nobigout .le. 0 ) then
      write(01,4104) flnamesmall(1,icod)
 4104 format(33x,'These flows are from a NON-PARAMETRIC ',
     &'distribution ...  File: ',a64/33x,77('='))
      endif
      endif ! non-parametric data ==============================================

*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
	if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
 8920 format(/'*** Specified monthly data do not exist ...',
     &' File: ',a64)
	call stop
 2970 if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( ical .ne. 1 ) then ! ------------------------------------------------
      write(01,7363) FLMONTHsmall(1,icod)
 7363 format(33x,'These flows are from monthly distributions ...',
     &' File: ',a64/33x,77('='))
      endif ! if ( ical .ne. 1 ) then ! ----------------------------------------
      endif ! if ( nobigout .le. 0 ) then ! ------------------------------------
      endif ! monthly data

      if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flows
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3170
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
 7920 format(/'*** Specified monthly structure data do not exist ...',
     &' File: ',a64)
	call stop
 3170 if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( ical .ne. 1 ) then
      write(01,3961) FLSTRUCTsmall(1,icod)
 3961 format(33x,
     &'These flows have monthly structure (type 8) ',
     &'... File: ',a64/33x,77('='))
      endif ! if ( ical .ne. 1 )
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      endif !  monthly structure

      if ( IQ .gt. 0 ) then
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,2044)IQ
 2044 format(33x,'Quality of this diffuse inflow (13): ',
     &'Data Set:',I6/33x,77('-'))
      endif
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
      endif
      
      call stream diffuse ! ----------------------------- diffuse pollution (13)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ---------- diffuse pollution (13)
      call get summaries of river quality from the shots !diffuse pollution (13)
      
      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then 
      
      call write resulting flow and quality ! ----------------------------- (13)
      
      !if ( distp .gt. 0.000001 .and. ical.ne.1 ) write(01,6388)
 6388 format(33x,77('='))
      
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) --------------------- (13)
      endif ! if ( DINT .gt. 1.0e-07 ) ------------------------------------ (13)
      if ( lake45 .eq. 1 ) KRFPOL13 = 0
      endif ! if ( KRFPOL13 .gt. 0 .and. KRQPOL13 .gt. 0 ) ---------------- (13)
*     ##########################################################################
      
      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (25)
*     add in diffuse pollution (livestock river type - 25) ---------------- (25)
*     --------------------------------------------------------------------- (25)
      add conc = 0 ! -------------------------------------------------------(25)
      if ( KRAPOL25 .ge. 1 ) add conc = 1 ! --------------------------------(25)
      if ( KRFPOL25 .gt. 0 .and. KRQPOL25 .gt. 0 ) then ! ----------------- (25)
      IQ = KRQPOL25
      IF = KRFPOL25
      JTKEEP = JT(KFEET)
      JT(KFEET) = 25
      diffuse type = 25
      if ( DINT .gt. 1.0e-07 ) then ! ------------------------------------- (25)
      call calculate summaries of river flow

      call get summaries of river quality from the shots ! diffuse (25)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif

      write(01,2944)IF
      write(34,2944)IF
 2944 format(33x,77('+')/33x,'Diffuse inflow (25) from livestock ',
     &'farming: Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (25)
      write(34,2082)dint,valchars11,valchars10,funit
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 
*     --------------------------------------------------------------------- (25)


*     --------------------------------------------------------------------- (25)
*     note whether the distribution is non-parametric --------------------- (25)
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------- (25)
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4090
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4090 continue
      write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------- (25)


*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8970
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8970 continue
      write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flows
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3970
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
	call stop
 3970 continue
      write(01,3961) FLSTRUCTsmall(1,icod)
      endif ! if ( PDRF(IF) .eq. 8 ) ... monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2094)IQ
 2094 format(33x,'Quality of diffuse inflow from livestock (25): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
      
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

*     add in diffuse pollution (livestock river type - 25) ---------------- (25)
      call stream diffuse ! ------------------------------- livestock river (25)
      
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ------------ livestock river (25)
      
      call get summaries of river quality from the shots ! diffuse (25) --- (25)
      
      call write resulting flow and quality ! ----------------------------- (25)

      endif ! if ( DINT .gt. 1.0e-07 ) ------------------------------------ (25)
      if ( lake45 .eq. 1 ) KRFPOL25 = 0
      endif ! if ( KRFPOL25 .gt. 0 .and. KRQPOL25 .gt. 0 ) ---------------- (25)
*     ---------------------------- diffuse pollution (livestock river type) (25)
*     ##########################################################################

      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (27)
*     add in diffuse pollution (arable river type - 27) ------------------- (27)
*     --------------------------------------------------------------------- (27)
      add conc = 0 ! -------------------------------------------------------(27)
      if ( KRAPOL27 .ge. 1 ) add conc = 1 ! --------------------------------(27)
      if ( KRFPOL27 .gt. 0 .and. KRQPOL27 .gt. 0 ) then ! ----------------- (27)
      IQ = KRQPOL27
      IF = KRFPOL27

      JTKEEP = JT(KFEET)
      JT(KFEET) = 27
      diffuse type = 27
      if ( DINT .gt. 1.0e-07 ) then ! ------------------------------------- (27)
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (27)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif

      write(01,4944)IF
      write(34,4944)IF
 4944 format(33x,77('+')/33x,'Diffuse inflow (27) from arable ',
     &'farming: Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (27)
      write(34,2082)dint,valchars11,valchars10,funit
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 
      
*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 8090
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
	call stop
 8090 if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
	endif !  if ( PDRF(IF) .eq. 4 ) 
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
	if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8270
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
	call stop
 8270 if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif !  if ( PDRF(IF) .eq. 5 )
*     ==========================================================================

*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (27)
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flows
*     identify the file with the monthly structure data ------------------- (27)
      do i = 1, M9
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3940
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3940 if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif !  if ( PDRF(IF) .eq. 8 ) ! monthly structure
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (27)

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,4994)IQ
 4994 format(33x,'Quality of diffuse inflow from arable (27): ',
     &'Data Set:',I6/33x,77('-'))
      endif
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

*     add in diffuse pollution (arable river type (27) -------------------- (27)
      call stream diffuse ! ---------------------------------- arable river (27)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! --------------- arable river (27)
      call get summaries of river quality from the shots ! -------- diffuse (27)
      call write resulting flow and quality ! ----------------------------- (27)
      
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL27 = 0
      endif ! if ( KRFPOL27 .gt. 0 .and. KRQPOL27 .gt. 0 ) ---------------- (27)
*     ------------------------------- diffuse pollution (arable river type) (27)
*     ##########################################################################
     
      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (29)
*     add in highway runoff (river type - 29) ----------------------------- (29)
*     --------------------------------------------------------------------- (29)
      add conc = 0 ! -------------------------------------------------------(29)
      if ( KRAPOL29 .ge. 1 ) add conc = 1 ! --------------------------------(29)
      if ( KRFPOL29 .gt. 0 .and. KRQPOL29 .gt. 0 ) then ! ----------------- (29)
      IQ = KRQPOL29
      IF = KRFPOL29
      JTKEEP = JT(KFEET)
      JT(KFEET) = 29
      diffuse type = 29
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (29)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,2244)IF
      write(34,2244)IF
 2244 format(33x,77('+')/33x,'Diffuse inflow (29) from highway ',
     &'runoff: Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J) ! ------------- (29)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (29)
      write(34,2082)dint,valchars11,valchars10,funit
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4030
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4030 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif !  if ( PDRF(IF) .eq. 4 ) 
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8170
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8170 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif !  if ( PDRF(IF) .eq. 5 )
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 5970
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 5970 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! if ( PDRF(IF) .eq. 8 ) ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,2994)IQ
 2994 format(33x,'Quality of diffuse inflow from highway runoff (29): ',
     &'Data Set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
*     --------------------------------------------------------------------- (29)

*     add in highway runoff (river type - 29) ----------------------------- (29)
*     --------------------------------------------------------------------- (29)
      call stream diffuse ! -------------------------------- highway runoff (29)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ------------- highway runoff (29)
      call get summaries of river quality from the shots ! - highway runoff (29)
      
      call write resulting flow and quality ! ----------------------------- (29)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL29 = 0
      endif ! if ( KRFPOL29 .gt. 0 .and. KRQPOL29 .gt. 0 ) - highway runoff (29)
*     ----------------------------------------- highway runoff (river type) (29)
*     ##########################################################################

      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (31)
*     add in urban runoff (river type - 31) ------------------------------- (31)
*     --------------------------------------------------------------------- (31)
      add conc = 0 ! -------------------------------------------------------(31)
      if ( KRAPOL31 .ge. 1 ) add conc = 1 ! --------------------------------(31)
      if ( KRFPOL31 .gt. 0 .and. KRQPOL31 .gt. 0 ) then ! ----------------- (31)
      IQ = KRQPOL31
      IF = KRFPOL31
      JTKEEP = JT(KFEET)
      JT(KFEET) = 31
      diffuse type = 31
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (31)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,1244)IF
      write(34,1244)IF
 1244 format(33x,77('+')/33x,'Diffuse inflow (31) from urban runoff: ',
     &'Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (31)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (31)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 


*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4020
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4020 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     Note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8925
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8925 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3910
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
	  call stop
 3910 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,1994)IQ
 1994 format(33x,'Quality of diffuse inflow from urban runoff (31): ',
     &'Data Set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) 


*     add in urban runoff (river type - 31) ------------------------------- (31)
      call stream diffuse ! ---------------------------------- urban runoff (31)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! --------------- urban runoff (31)
      call get summaries of river quality from the shots ! --- urban runoff (31)
      
      call write resulting flow and quality ! ----------------------------- (31)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL31 = 0
      endif ! if ( KRFPOL31 .gt. 0 .and. KRQPOL31 .gt. 0 ) ---------------- (31)
*     ------------------------------------------- urban runoff (river type) (31)
*     ##########################################################################
      
      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (33)
*     add in atmospheric deposition(river type - 33) ---------------------- (33)
*     --------------------------------------------------------------------- (33)
      add conc = 0 ! -------------------------------------------------------(33)
      if ( KRAPOL33 .ge. 1 ) add conc = 1 ! --------------------------------(33)
      if ( KRFPOL33 .gt. 0 .and. KRQPOL33 .gt. 0 ) then ! ----------------- (33)
      IQ = KRQPOL33
      IF = KRFPOL33
      JTKEEP = JT(KFEET)
      JT(KFEET) = 33
      diffuse type = 33
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (33)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,1884)IF
      write(34,1884)IF
 1884 format(33x,77('+')/33x,'Diffuse inflow (33) from atmospheric ',
     &'deposition: Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (33)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (33)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 


*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4009
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4009 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8975
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8975 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3975
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3975 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,3994)IQ
 3994 format(33x,'Quality of diffuse inflow from atmospheric ',
     &'deposition (33): data set:',I6/33x,77('-'))
      endif
      
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! add in atmospheric deposition (33) ------------ (33)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----- atmospheric deposition (33)

      call get summaries of river quality from the shots ! -------- diffuse (33)
      
      call write resulting flow and quality ! ----------------------------- (33)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL33 = 0
      endif ! if ( KRFPOL33 .gt. 0 .and. KRQPOL33 .gt. 0 ) ---------------- (33)
*     ---------------------------------- atmospheric deposition(river type) (33)
*     ##########################################################################



*     ##########################################################################
*     --------------------------------------------------------------------- (35)
*     add in natural background (river type - 35) ------------------------- (35)
*     --------------------------------------------------------------------- (35)
      add conc = 0 ! -------------------------------------------------------(35)
      if ( KRAPOL35 .ge. 1 ) add conc = 1 ! --------------------------------(35)
      if ( KRFPOL35 .gt. 0 .and. KRQPOL35 .gt. 0 ) then ! ----------------- (35)
      IQ = KRQPOL35
      IF = KRFPOL35
      JTKEEP = JT(KFEET)
      JT(KFEET) = 35
      diffuse type = 35

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (35)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,1824)IF
      write(34,1824)IF
 1824 format(33x,77('+')/33x,'Diffuse inflow (35) from natural ',
     &'background: data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (35)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (35)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 


*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4093
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4093 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8972
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8972 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3972
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3972 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,3924)IQ
 3924 format(33x,'Quality of diffuse inflow from natural ',
     &'background (35): data set:',I6/33x,77('-'))
      endif
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
      
      call stream diffuse ! --------------------- add in natural background (35)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! --------- natural background (35)
      call get summaries of river quality from the shots ! -------- natural (35)
      
      call write resulting flow and quality ! ---------- natural background (35)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL35 = 0
      endif ! if ( KRFPOL35 .gt. 0 .and. KRQPOL35 .gt. 0 ) ---------------- (35)
*     ------------------------------------- natural background (river type) (35)
*     ##########################################################################


      
*     ##########################################################################
*     --------------------------------------------------------------------- (46)
*     add in diffuse mines (river type - 46) ------------------------------ (46)
*     --------------------------------------------------------------------- (46)
      add conc = 0 ! -------------------------------------------------------(46)
      if ( KRAPOL46 .ge. 1 ) add conc = 1 ! --------------------------------(46)
      if ( KRFPOL46 .gt. 0 .and. KRQPOL46 .gt. 0 ) then ! ----------------- (46)
      IQ = KRQPOL46
      IF = KRFPOL46
      JTKEEP = JT(KFEET)
      JT(KFEET) = 46
      diffuse type = 46

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (46)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,6824)IF
      write(34,6824)IF
 6824 format(33x,77('+')/33x,'Diffuse inflow (46) from mines: ',
     &'Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (46)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (46)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4893
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4893 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8978
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8978 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3978
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3978 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,3984)IQ
 3984 format(33x,'Quality of diffuse inflow from mines (46): ',
     &'Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! --------------------- add in diffuse from mines (46)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! --------- diffuse from mines (46)
      call get summaries of river quality from the shots ! --- mine diffuse (46)
      
      call write resulting flow and quality ! ---------- diffuse from mines (46)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL46 = 0
      endif ! if ( KRFPOL46 .gt. 0 .and. KRQPOL46 .gt. 0 ) ---------------- (46)
*     ------------------------------------------ diffuse mines (river type) (46)
*     ##########################################################################

      
      
*     ##########################################################################
*     --------------------------------------------------------------------- (48)
*     add in bird, boats and angling (river type - 48) -------------------- (48)
*     --------------------------------------------------------------------- (48)
      add conc = 0 ! -------------------------------------------------------(48)
      if ( KRAPOL48 .ge. 1 ) add conc = 1 ! --------------------------------(48)
      if ( KRFPOL48 .gt. 0 .and. KRQPOL48 .gt. 0 ) then ! ----------------- (48)
      IQ = KRQPOL48
      IF = KRFPOL48
      JTKEEP = JT(KFEET)
      JT(KFEET) = 48
      diffuse type = 48

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (48)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,7824)IF
      write(34,7824)IF
 7824 format(33x,77('+')/33x,'Diffuse inflow (48) from ',
     &'birds, boats and angling: ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
	if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (48)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (48)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5893
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5893 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1978
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1978 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6978
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 6978 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,7984)IQ
 7984 format(33x,'Quality of diffuse inflow from birds, ',
     &'boats and angling (48): ','Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! --------------- add in birds, boats and angling (48)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! --- birds, boats and angling (48)
      call get summaries of river quality from the shots ! -------- angling (48)
      
      call write resulting flow and quality ! ---- birds, boats and angling (48)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL48 = 0
      endif ! if ( KRFPOL48 .gt. 0 .and. KRQPOL48 .gt. 0 ) ---------------- (48)
*     ------------------------------- birds, boats and angling (river type) (48)
*     ##########################################################################      
      
      
      
*     ##########################################################################      
*     --------------------------------------------------------------------- (50)
*     add .......................... (river type - 50) -------------------- (50)
*     --------------------------------------------------------------------- (50)
      add conc = 0 ! -------------------------------------------------------(50)
      if ( KRAPOL50 .ge. 1 ) add conc = 1 ! --------------------------------(50)
      if ( KRFPOL50 .gt. 0 .and. KRQPOL50 .gt. 0 ) then ! ----------------- (50)
      IQ = KRQPOL50
      IF = KRFPOL50
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 50
	diffuse type = 50

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (50)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,7504)IF
      write(34,7504)IF
 7504 format(33x,77('+')/33x,'Diffuse inflow (50) .....',
     &'......................... ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (50)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (50)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5503
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5503 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1508
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1508 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6508
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 6508 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6504)IQ
 6504 format(33x,'Quality of diffuse inflow from ....... ',
     &'User-defined (50): ','Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ----------------------------------------------- (50)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----------------- user-named (50)
      call get summaries of river quality from the shots ! ................ (50)
      
      call write resulting flow and quality ! ------------------ user-named (50)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL50 = 0
      endif ! if ( KRFPOL50 .gt. 0 .and. KRQPOL50 .gt. 0 ) ---------------- (50)
*     -------------------------------------------------------- (river type) (50)
*     ##########################################################################      
      
      
      
*     ##########################################################################      
*     --------------------------------------------------------------------- (52)
*     add .......................... (river type - 52) -------------------- (52)
*     --------------------------------------------------------------------- (52)
      add conc = 0 ! -------------------------------------------------------(52)
      if ( KRAPOL52 .ge. 1 ) add conc = 1 ! --------------------------------(52)
      if ( KRFPOL52 .gt. 0 .and. KRQPOL52 .gt. 0 ) then ! ----------------- (52)
      IQ = KRQPOL52
      IF = KRFPOL52
      JTKEEP = JT(KFEET)
      JT(KFEET) = 52
      diffuse type = 52

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (52)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,7524)IF
      write(34,7524)IF
 7524 format(33x,77('+')/33x,'Diffuse inflow (52) .....',
     &'......................... ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (52)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (52)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5523
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5523 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1528
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1528 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6528
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 6528 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6524)IQ
 6524 format(33x,'Quality of diffuse inflow from ....... ',
     &'User-defined (52): ','Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ----------------------------------------------- (52)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----------------- user-named (52)
      call get summaries of river quality from the shots ! ................ (52)
      
      call write resulting flow and quality ! ------------------ user-named (52)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL52 = 0

      endif ! if ( KRFPOL52 .gt. 0 .and. KRQPOL52 .gt. 0 ) ---------------- (52)
*     -------------------------------------------------------- (river type) (52)
*     ##########################################################################      
      
      
      
*     ##########################################################################      
*     --------------------------------------------------------------------- (54)
*     add .......................... (river type - 54) -------------------- (54)
*     --------------------------------------------------------------------- (54)
      add conc = 0 ! -------------------------------------------------------(54)
      if ( KRAPOL54 .ge. 1 ) add conc = 1 ! --------------------------------(54)
      if ( KRFPOL54 .gt. 0 .and. KRQPOL54 .gt. 0 ) then ! ----------------- (54)
      IQ = KRQPOL54
      IF = KRFPOL54
      JTKEEP = JT(KFEET)
      JT(KFEET) = 54
      diffuse type = 54

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (54)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,7544)IF
      write(34,7544)IF
 7544 format(33x,77('+')/33x,'Diffuse inflow (54) .....',
     &'......................... ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (54)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (54)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5543
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5543 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1548
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1548 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
	if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6548
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 6548 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6544)IQ
 6544 format(33x,'Quality of diffuse inflow from ....... ',
     &'User-defined (54): ','Data set:',I6/33x,77('-'))
       endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ----------------------------------------------- (54)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----------------- user-named (54)
      call get summaries of river quality from the shots ! ................ (54)
      
      call write resulting flow and quality ! ------------------ user-named (54)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL54 = 0
      endif ! if ( KRFPOL54 .gt. 0 .and. KRQPOL54 .gt. 0 ) ---------------- (54)
*     -------------------------------------------------------- (river type) (54)
*     ##########################################################################      


      
*     ##########################################################################      
*     --------------------------------------------------------------------- (56)
*     add .......................... (river type - 56) -------------------- (56)
*     --------------------------------------------------------------------- (56)
      add conc = 0 ! -------------------------------------------------------(56)
      if ( KRAPOL56 .ge. 1 ) add conc = 1 ! --------------------------------(56)
      if ( KRFPOL56 .gt. 0 .and. KRQPOL56 .gt. 0 ) then ! ----------------- (56)
      IQ = KRQPOL56
      IF = KRFPOL56
      JTKEEP = JT(KFEET)
      JT(KFEET) = 56
      diffuse type = 56

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow

      call get summaries of river quality from the shots ! diffuse (56)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,7564)IF
      write(34,7564)IF
 7564 format(33x,77('+')/33x,'Diffuse inflow (56) .....',
     &'......................... ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (56)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (56)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5563
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5563 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1568
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1568 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6568
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod) ! monthly structure data do not exist
      write( *,7920) FLSTRUCTsmall(1,icod) ! monthly structure data do not exist
      write(09,7920) FLSTRUCTsmall(1,icod) ! monthly structure data do not exist
	call stop
 6568 continue ! +++++++++++++++++++++++++++++++ monthly structure data do exist
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6564)IQ
 6564 format(33x,'Quality of diffuse inflow from ....... ',
     &'User-defined (56): ','Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ------------------------------------ user-named (56)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----------------- user-named (56)
      call get summaries of river quality from the shots ! ................ (56)
      
      call write resulting flow and quality ! ------------------ user-named (56)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL56 = 0
      endif ! if ( KRFPOL56 .gt. 0 .and. KRQPOL56 .gt. 0 ) ----- user-named (56)
*     ---------------------------------------------------------- user-named (56)
*     ##########################################################################
      
      
      
*     ##########################################################################      
*     --------------------------------------------------------------------- (58)
*     add .......................... (river type - 58) -------------------- (58)
*     --------------------------------------------------------------------- (58)
      add conc = 0 ! -------------------------------------------------------(58)
      if ( KRAPOL58 .ge. 1 ) add conc = 1 ! --------------------------------(58)
      if ( KRFPOL58 .gt. 0 .and. KRQPOL58 .gt. 0 ) then ! ----------------- (58)
      IQ = KRQPOL58
      IF = KRFPOL58
      JTKEEP = JT(KFEET)
      JT(KFEET) = 58
      diffuse type = 58

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (58)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,7584)IF
      write(34,7584)IF
 7584 format(33x,77('+')/33x,'Diffuse inflow (58) .....',
     &'......................... ','Data Set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (58)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (58)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------- (58)
*     note whether the distribution is non-parametric --------------------- (58)
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------- (58)
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 5583
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 5583 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif ! if ( PDRF(IF) .eq. 4 ) -------------------------------------- (58)
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then ! --------------------------------------------
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 1588
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 1588 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif ! if ( PDRF(IF) .eq. 5 ) -------------------------------------------
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure --------------------------
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 6588
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 6588 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure ------------------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,6584)IQ
 6584 format(33x,'Quality of diffuse inflow from ....... ',
     &'User-defined (58): ','Data set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ----------------------------------------------- (58)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! ----------------- user named (58)
      call get summaries of river quality from the shots ! ................ (58)
      
      call write resulting flow and quality ! ----------------------------- (58)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL58 = 0
      endif ! if ( KRFPOL58 .gt. 0 .and. KRQPOL58 .gt. 0 ) ---------------- (58)
*     -------------------------------------------------------- (river type) (58)
*     ##########################################################################      

      
      
*     ##########################################################################      
*     --------------------------------------------------------------------- (37)
*     add in septic tanks (river type - 37) ------------------------------- (37)
*     --------------------------------------------------------------------- (37)
      add conc = 0 ! -------------------------------------------------------(37)
      if ( KRAPOL37 .ge. 1 ) add conc = 1 ! --------------------------------(37)
      if ( KRFPOL37 .gt. 0 .and. KRQPOL37 .gt. 0 ) then ! ----------------- (37)
      IQ = KRQPOL37
      IF = KRFPOL37
      JTKEEP = JT(KFEET)
      JT(KFEET) = 37
      diffuse type = 37
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (37)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,1784)IF
      if ( nobigout .le. 0 ) write(34,1784)IF
 1784 format(33x,77('+')/33x,'Diffuse inflow (37) from septic tanks: ',
     &'Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (37)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (37)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4091
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4091 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 8971
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 8971 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3907
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3907 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,3794)IQ
 3794 format(33x,'Quality of diffuse inflow from septic tanks (37): ',
     &'Data Set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! --------------------------- add in septic tanks (37)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (37)

      call write resulting flow and quality ! ----------------------------- (37)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL37 = 0
      endif ! if ( KRFPOL37 .gt. 0 .and. KRQPOL37 .gt. 0 ) ---------------- (37)
*     ------------------------------------------- septic tanks (river type) (37) 
*     ##########################################################################      
      


*     ##########################################################################
*     --------------------------------------------------------------------- (40)
*     add in aggregated CSOs (river type - 40) ---------------------------- (40)
*     --------------------------------------------------------------------- (40)
      add conc = 0 ! -------------------------------------------------------(40)
      if ( KRAPOL40 .ge. 1 ) add conc = 1 ! --------------------------------(40)
      if ( KRFPOL40 .gt. 0 .and. KRQPOL40 .gt. 0 ) then ! ----------------- (40)
      IQ = KRQPOL40
      IF = KRFPOL40
      JTKEEP = JT(KFEET)
      JT(KFEET) = 40
      diffuse type = 40
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (40)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,3784)IF
      write(34,3784)IF
 3784 format(33x,77('+')/33x,'Diffuse inflow (40) from aggregated ',
     &'CSOs: Flow data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet ! ---------------------------------------------------------
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo ! do J = 1, ndet ---------------------------------------------------
      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),dint)
      write(01,2082)valchars12,valchars11,valchars10,funit ! -------------- (40)
      write(34,2082)valchars12,valchars11,valchars10,funit ! -------------- (40)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     --------------------------------------------------------------------------
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 4991
      enddo
      write(01,8940) flnamesmall(1,icod)
      write( *,8940) flnamesmall(1,icod)
      write(09,8940) flnamesmall(1,icod)
      call stop
 4991 continue
      if ( nobigout .le. 0 ) write(01,4104) flnamesmall(1,icod)
      endif
*     --------------------------------------------------------------------------

*     ==========================================================================
*     Note whether the data are monthly ----------------------------------------
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 9971
      enddo
      write(01,8920) FLMONTHsmall(1,icod)
      write( *,8920) FLMONTHsmall(1,icod)
      write(09,8920) FLMONTHsmall(1,icod)
      call stop
 9971 continue
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
      endif
*     ==========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3967
      enddo
      write(01,7920) FLSTRUCTsmall(1,icod)
      write( *,7920) FLSTRUCTsmall(1,icod)
      write(09,7920) FLSTRUCTsmall(1,icod)
      call stop
 3967 continue
      if ( nobigout .le. 0 ) write(01,3961)FLSTRUCTsmall(1,icod)
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,3714)IQ
 3714 format(33x,'Quality of diffuse inflow from aggregated ',
     &'CSOs (40): data set:',I6/33x,77('-'))
      endif
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

      call stream diffuse ! ------------------------ add in aggregated CSOs (40)
      JT(KFEET) = JTKEEP
      diffuse type = 0 ! ------------------ after adding in aggregated CSOs (40)

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (40)
      
      call write resulting flow and quality ! ----------------------------- (40)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL40 = 0
      endif ! if ( KRFPOL40 .gt. 0 .and. KRQPOL40 .gt. 0 ) ---------------- (40)
*     ---------------------------------------- aggregated CSOs (river type) (40)
*     ##########################################################################
      

      
*     ##########################################################################
*     add in diffuse pollution (effluent type) ---------------------------- (15)
      if ( KEPOL15  .gt. 0 ) then
      IQ = KEPOL15
      JTKEEP = JT(KFEET)
      JT(KFEET) = 15
      diffuse type = 15

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! -------- diffuse (15)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif ! if ( diffuse heading .eq. 0 )
      write(01,5944)IQ
      write(34,5944)IQ
 5944 format(33x,77('+')/33x,'Discharge-type diffuse inflow (15)',
     &': data set:',I6)
      iprime = 1

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit ! ------------------------- (15)
      write(34,1482)valchars11,valchars10,funit ! ------------------------- (15)
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*FE(IQ,1),DINT*FE(IQ,2),DINT)
      write(01,3210)valchars11,valchars10,funit,valchars12 ! -------------- (15)
      write(34,3210)valchars11,valchars10,funit,valchars12 ! -------------- (15)
 3210 format(33x,77('-')/
     &33x,'Total flow added',19x, ! --------------------------------------- (15)
     &'St.dev =',a10,3x,'Mean =',a10,1x,a4,2x,a10,' km'/33x,77('-'))

      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 
*     --------------------------------------------------------------------- (15)

*     ===================================================================== (15)
*     note whether the distribution is non-parametric ===================== (15)
      if ( PDEF(IQ) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 4396
      enddo
      write(01,8940) flnamesmall(3,icod)
      write( *,8940) flnamesmall(3,icod)
      write(09,8940) flnamesmall(3,icod)
      call stop
 4396 if ( nobigout .le. 0 ) write(01,4104) flnamesmall(3,icod)
	endif !  if ( PDEF(IQ) .eq. 4 )
*     ==========================================================================

*     ==========================================================================
*     note whether the data are monthly ----------------------------------- (15)
      if ( PDEF(IQ) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
      if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 4976
      enddo
      write(01,8920) FLMONTHsmall(3,icod)
      write( *,8920) FLMONTHsmall(3,icod)
      write(09,8920) FLMONTHsmall(3,icod)
      call stop
 4976 if ( nobigout .le. 0 ) then 
      write(01,7363) FLMONTHsmall(3,icod)
      endif
      endif !  if ( PDEF(IQ) .eq. 5 ) 
*     ===================================================================== (15)

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDEF(IQ) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
      if ( istruct ( 2, i, 1 ) .eq. IQ ) goto 5976
      enddo
      write(01,7920) FLSTRUCTsmall(3,icod)
      write( *,7920) FLSTRUCTsmall(3,icod)
      write(09,7920) FLSTRUCTsmall(3,icod)
      call stop
 5976 if ( nobigout .le. 0 ) write(01,3961) FLSTRUCTsmall(3,icod)
      endif !  if ( PDEF(IQ) .eq. 8 )
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ (15)


*     --------------------------------------------------------------------- (15)
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,2194)IQ
 2194 format(33x,'Quality of diffuse effluent (15): ',
     &'data set:',I6/33x,77('-'))
      endif
      call write mean and standard deviation 33a
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
*     --------------------------------------------------------------------- (15)



*     add in diffuse pollution (effluent type) ---------------------------- (15)
      call effluent diffuse 15 ! ------------------------------------------ (15)
      JT(KFEET) = JTKEEP
      diffuse type = 0 ! ---------- after diffuse pollution (effluent type) (15)

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! diffuse (15)
      
      call write resulting flow and quality ! ----------------------------- (15)

      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KEPOL15 = 0
      endif ! if ( KEPOL15  .gt. 0 )
*     ----------------------------------- diffuse pollution (effluent type) (15)
*     ********************************************************************* (15)




*     ********************************************************************* (42)
*     add in aggregated STWs (effluent type) ------------------------------ (42)
      if ( KEPOL42 .gt. 0 ) then

      IQ = KEPOL42
      JTKEEP = JT(KFEET)
      JT(KFEET) = 42
      diffuse type = 42

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! -------- diffuse (42)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( diffuse heading .eq. 0 ) then
      write(01,5282)distp
      diffuse heading = 1
      endif
      write(01,5994)IQ
      write(34,5994)IQ
 5994 format(33x,77('-')/33x,'Aggregated STWs (42)',
     &': flow and quality data set:',I6)

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit ! ------------------------- (42)
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 3 (DINT*FE(IQ,1),DINT*FE(IQ,2),DINT)
      write(01,3210)valchars11,valchars10,funit,valchars12 ! -------------- (42)
      write(34,3210)valchars11,valchars10,funit,valchars12 ! -------------- (42)

      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) --------------------- (42)

*     ----------------------------------------------------- aggregated STWs (42)
*     note whether the distribution is non-parametric ===================== (42)
      if ( PDEF(IQ) .eq. 4 ) then
*     identify the file with the non-parametric data ====================== (42)
      do i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 4390
      enddo
      write(01,8940) flnamesmall(3,icod)
      write( *,8940) flnamesmall(3,icod)
      write(09,8940) flnamesmall(3,icod)
      call stop
 4390 if ( nobigout .le. 0 ) write(01,4104) flnamesmall(3,icod)
      endif ! if ( PDEF(IQ) .eq. 4 ) ====================================== (42)

*     note whether the data are monthly ----------------------------------- (42)
      if ( PDEF(IQ) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------- (42)
      do i = 1, M8
      icod = i
      if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 4970
      enddo
      write(01,8920) FLMONTHsmall(3,icod)
      write( *,8920) FLMONTHsmall(3,icod)
      write(09,8920) FLMONTHsmall(3,icod)
      call stop
 4970 if ( nobigout .le. 0 ) then
      write(01,7363) FLMONTHsmall(3,icod)
      endif
      endif ! if ( PDEF(IQ) .eq. 5 )

      if ( PDEF(IQ) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------- (42)
      do i = 1, M9
      icod = i
      if ( istruct ( 2, i, 1 ) .eq. IQ ) goto 5975
      enddo
      write(01,7920) FLSTRUCTsmall(3,icod)
      write( *,7920) FLSTRUCTsmall(3,icod)
      write(09,7920) FLSTRUCTsmall(3,icod)
      call stop
 5975 if ( nobigout .le. 0 ) write(01,3961) FLSTRUCTsmall(3,icod) ! ------- (42)
      endif ! monthly structure
*     ----------------------------------------------------- aggregated STWs (42)
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,2894)IQ
 2894 format(33x,'Quality of diffuse aggregated STWs (42): ',
     &'Data Set:',I6/33x,77('-'))
      endif

      call write mean and standard deviation 33a
      endif ! aggregated STWs --------------------------------------------- (42)

*     add in aggregated STWs (effluent type) ------------------------------ (42)

      call effluent diffuse 42
      
      JT(KFEET) = JTKEEP
      diffuse type = 0
      
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! -------- diffuse (42)
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      
      call write resulting flow and quality ! ----------------------------- (42)

      write(01,2992)
 2992 format(33x,77('='))
      write(34,2992)
      endif ! if ( ical13 .eq. 0 ) 
      endif ! if ( nobigout .le. 0 ) 
*     --------------------------------------------------------------------- (42)
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KEPOL42 = 0
      endif ! if ( KEPOL42  .gt. 0 ) ! ------------------------------------ (42)
*     **************************************************************************

            
      
*     check for xero distance and for inflow from a lake -----------------------
      if ( lake45 .eq. 1 ) DINT = 0.0

      do jp =1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
      endif
      enddo

      if ( IPUR .gt. 0 ) then
      call natural purification
      endif ! if ( IPUR .gt. 0 )
      
      call calculate summaries of river flow
      
      do jp = 1, ndet
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
      do is = 1, NS
      RQ(is) = CMS(JP,IS) ! store river quality --------------------------------
      enddo
      call calculate summaries of river quality (JP,RQ) 
      endif ! if ( QTYPE (jp) .ne. 4 )
      enddo ! do jp = 1, ndet

      JT (KFEET) = KEEPTYPE
      ifdiffuse = 0
      add conc = 0 ! reset control on addition of concentration without flow ---

      return
      end




*     natural purification -----------------------------------------------------
      subroutine natural purification
      include 'COMMON DATA.FOR'
      dimension half life (mp10) 
      dimension oldshots (mp10, ms)

*     storage for the decay rates ----------------------------------------------
      real kay(mp10), halt(mp10), kay0(mp10)
      real KBOD

*     check for a zero distance and return if one is found ---------------------
      if ( dint .lt. 1.0E-8 ) return

*     compute the loads prior to natural purification --------------------------
      call load calculation ! ----------------------------- natural purification

*     store the starting values of the loads -----------------------------------
      do idet = 1, ndet
      do is = 1, ns
      old shots (idet, is ) = xshots ( idet,is )
      enddo
      enddo ! do idet = 1, ndet
      
      call load calculation ! ----------------------------- natural purification

*     set the decay rates ... loop on all the determinands ---------------------
      do JDET = 1, MP10
*     skip the dummy determinands ----------------------------------------------
      if ( QTYPE (JDET) .ne. 4 ) then
*     intitialise the store for the half-life ----------------------------------
      half life (jdet) = 0.0
*     set the global (catchment-wide) rate constant ----------------------------
      Kay0 (jdet) = RATE (jdet)
*     quality will decay towards the value specified for HALT ------------------
      Halt (jdet) = QBASE (jdet)
*     over-write these with the reach values (if specified for this reach) -----
      if ( Rchkay(jdet,IREACH) .gt. 1.0E-10 ) then
      Kay0(jdet) = Rchkay(jdet,IREACH)
      endif
*     a negative reach value indicates conservative behaviour in this reach ----
      if ( Rchkay(jdet,IREACH) .lt. -0.9 ) Kay0 (Jdet) = 0.0
      endif
      enddo ! do JDET = 1, MP10

*     all the rate constants have been set -------------------------------------
*     prepare to compute the average velocity and the time-of-travel -----------
      avel = 0.0
      avel95 = 0.0
      atim = 0.0
      atim95 = 0.0

      call calculate summaries of river flow

*     check for a request to overule the default -------------------------------
*     use the expression ... const * flow ** const -----------------------------
*     set the multiplier ... the velocity at mean flow for this reach ----------
      AA = RCHA (IREACH)

*     obtain the exponent for this reach ---------------------------------------
      BB = RCHB (IREACH)
*     set the default velocities - 33 kilometres per day -----------------------
      velo = 33.0
      velo95 = 33.0
      TIME = 0.0

*     calculate the default time-of-travel in days - distance divided by velocity
      TIME = dint / velo
      time95 = dint / velo

*     resort to default if AA is zero or negative ------------------------------
      if ( AA .lt. 1.0E-10 ) goto 998
*     resort to default if BB is zero ------------------------------------------
      if ( BB .lt. 1.0E-10 .and. BB .gt. -1.0E-10 ) goto 998
*     revert to default if product is zero -------------------------------------
      AB = AA * BB
      if ( AB .lt. 1.0001  .and .AB .gt.   0.9999 ) goto 998

*     compute the ratio of the 95-percentile flow to the mean flow -------------
      FF = FLOW(2) / FLOW(1)
*     compute the velocity of flow at the 95-percentile low flow ---------------
      velo95 = AA * FF ** BB
*     compute the time of travel at the 95-percentile low flow -----------------
      time95 = 24.0 * dint / velo95

  998 continue


*     apply the rate constants =================================================
      do 9 IS = 1, NS ! loop on all the shots ==================================
*     skip round zero flows ----------------------------------------------------
      if ( FMS(IS) .lt. 1.0E-08 ) goto 9

*     compute the time-of-travel -----------------------------------------------
*     the default is 33 kilometres per day -------------------------------------
      velo = 33.0
      TIME = dint / velo

*     resort to default if AA is zero or negative ------------------------------
      if ( AA .lt. 1.0E-10 ) goto 18
*     resort to default if BB is zero ------------------------------------------
      if ( BB .lt. 1.0E-10 .and. BB .gt. -1.0E-10 ) goto 18
      AB = AA * BB
*     revert to default if product is zero -------------------------------------
      if ( AB .lt. 1.0001  .and .AB .gt.   0.9999 ) goto 18
*     compute ratio of the shot flow to the mean flow --------------------------
      FF = FMS (IS) / FLOW(1)
*     compute the velocity of flow ---------------------------------------------
      velo = AA * FF ** BB
*     compute the time of travel -----------------------------------------------
      time = dint / velo
   18 continue

*     accumulate for the average velocity and average time-of-travel -----------
      avel = avel + velo
      atim = atim + time

*     set the temperature ... this will be used to adjust rate constants -------
      TD = BMS global (1, IS)
      Solids = BMS global (2, IS)
      TD = BMS (1, IS)
      Solids = BMS (2, IS)

*     ==========================================================================
*     loop on all the determinands for this shot ...
*     ==========================================================================
*     do Biochemical Oxygen Demand first ...
*     then ammonia ...
*     then dissolved oxygen ...
*     then the parents and daughters ...
*     then the rest ...
*     ==========================================================================


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ BOD
      do 5008 JDET = 1, NDET
*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     isolate Biochemical Oxygen Demand (detype = 101) for special treatment ---
*     and data -----------------------------------------------------------------
      if ( Detype (jdet) .eq. 101 ) then ! --------------------------------- BOD

*     check the determinand is to be degraded ...
*     -----------------------------------------------------------------------Two
      if ( QTYPE (jdet) .eq. 2 ) then

*     adjust the decay rates for temperature ------------------------------- BOD
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow ---------------------------------------- BOD
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0

*     old, pre-decay, value of the concentration --------------------------- BOD
      BOD0 = CMS(JDET,IS)

*     store the decay rate for use in the Dissolved Oxygen calculations ---- BOD
*     note that this excludes the adjustment for Biochemical Oxygen Demand - BOD
*     concentration -------------------------------------------------------- BOD
      KBOD = kay (jdet)

*     the rate constant depends on the Biochemical Oxygen Demand ----------- BOD
      kay (jdet) = kay (jdet) * ( 0.9090 + 0.01819 * BOD0 )
      EBOD = 1.0
      if ( kay (jdet) .gt. 0.0 ) then
      EBOD = exp (- kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif

*     scale the loads for the effect of decay ------------------------------ BOD
      old CMS = CMS(JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = amin1 ( CMS(JDET,IS),
     &(halt (jdet) + (BOD0 - halt (jdet) ) * EBOD))
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS ! decay -------
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS(JDET,IS) = 0.0
      endif

*     save this halt value for the calculations for Dissolved Oxygen --------BOD
      HaltBOD = Halt (jdet)

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 101 ) ---------------------------------BOD
      endif ! if ( QTYPE (JDET) .ne. 1 )
      endif ! if ( QTYPE (JDET) .ne. 4 )
 5008 continue   
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ BOD

      
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ AMM
*     now do ammonia ...
      do 5009 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

      if ( detype (jdet) .eq. 103 ) then ! ----------------------------------AMM

*     initialise the amount to be added to nitrate --------------------------AMM
      Ammonia loss = 0.0

      if ( QTYPE (jdet) .eq. 2 ) then

*     adjust the decay rate for temperature ---------------------------------AMM
      Kay (jdet) = Kay0 (jdet) * 1.072 ** ( TD - 20.0)

*     check for potential underflow -----------------------------------------AMM
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
      AMM0 = CMS(JDET,IS)
      EAMM = 1.0

      if ( kay (jdet) .gt. 0.0 ) then
      EAMM = exp( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif

      cnew ammonia = amin1 ( CMS(JDET,IS), 
     &(halt (jdet) + ( AMM0 - halt (jdet) ) * EAMM))

*     save the Ammonia loss to add to nitrate -------------------------------AMM
*     ammonia loss = amax1 (0.0, (CMS(jdet,is) - cnew ammonia ))
      old CMS = CMS(JDET,IS)
	if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = amin1 ( CMS(JDET,IS), cnew ammonia )
      do ip = 1, nprop
*     concentrations from various types of feature --------------------------AMM
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature --AMM
      enddo
      CMS(JDET,IS) = 0.0
      endif

      endif
      endif

      endif
      endif
 5009 continue ! ammonia ----------------------------------------------------AMM
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++AMM

      
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++DO
*     now isolate dissolved oxygen for special treatment ---------------------DO
      do 5010 jdet = 1, ndet
      if ( QTYPE (JDET) .ne. 4 ) then
      if ( QTYPE (JDET) .ne. 1 ) then ! skip conservative determinands -------DO
      if ( detype (jdet) .eq. 104 ) then ! identify dissolved oxygen ---------DO
          
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(jdet) .eq. 3 ) then ! check that reaeration is requested ----DO
*     adjust the decay rates for temperature ---------------------------------DO
      Kay (jdet) = Kay0 (jdet)  * 1.024 ** ( TD - 20.0)
*     check for potential underflow ------------------------------------------DO
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
*     saturation value for Dissolved Oxygen ----------------------------------DO
      SAT = DO Saturation (TD)
*     extract the current value for Dissolved Oxygen -------------------------DO
      DOXY = CMS(jdet,IS)
*     set the default for the reararation constant ---------------------------DO
      EAIR = 1.0
      if ( Kay (Jdet) .gt. 0.0 ) then
      EAIR = EXP ( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / Kay (jdet)
      endif
      JA=0
      JB=0
*     use the value for Biochemical Oxygen Demand decay from above ---------- DO
      if ( ABS( ( Kay(jdet) - KBOD ) * TIME ) .lt. 1.0E-06 ) JB = 1
      if ( JB .eq. 0 ) QBOD = ( EBOD - EAIR ) / (Kay(jdet) - KBOD)
      if ( JB .eq. 1 ) QBOD = - TIME * EAIR
*     re-aeration towards the saturation value ++++++++++++++++++++++++++++++ DO
      Dzero = Amax1 ( 0.0, SAT - DOXY )
      !Dzero = SAT - DOXY 
      if (Dzero .ge. 0.0 ) then ! +++++++++++++++++++++++++++++++++++++++++++ DO
      BODEF = amax1 ( 0.0, ( BOD0 - HaltBOD ) )
      Dtee = amax1 ( 0.0, Kay(jdet) * QBOD * BODEF + dzero * EAIR )
      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-5 ) then
      CMS (JDET,IS) = Amax1 ( 0.0, (SAT - Dtee))

      do ip = 1, nprop
*     adjust concentrations from various types of feature +++++++++++++++++++ DO
      if (ip .eq. 2 ) old LMS = LMS(ip,JDET,IS)
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -- DO
      enddo
      CMS (JDET,IS) = 0.0
      endif ! if ( old CMS .gt. 1.0e-5 ) ++++++++++++++++++++++++++++++++++++ DO
      endif ! if (Dzero .ge. 0.0 ) ++++++++++++++++++++++++++++++++++++++++++ DO
*     ----------------------------------------------------------------------- DO
      endif ! if ( QTYPE (jdet) .eq. 3 ) determinand type 3 for Diss. Oxygen -DO
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     now look at determinand type 2 for Dissolved Oxygen ====================DO
      if ( QTYPE (jdet) .eq. 2 ) then
*     use the Dissolved Oxygen Slot as ordinary degradable determinand adjust DO
*     the decay rate for temperature =========================================DO
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow ==========================================DO
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
      ANY0 = CMS (JDET,IS)
      EANY = 1.0

      if ( Kay (jdet) .gt. 0.0 ) then
      EANY = exp ( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / Kay (jdet)
      endif

      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS (JDET,IS) = amin1 ( CMS(JDET,IS),
     &(halt (jdet) + ( ANY0 - halt (jdet) ) * EANY))
      do ip = 1, nprop
*     concentrations from various types of feature ===========================DO
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature ---DO
      enddo
      CMS (JDET,IS) = 0.0
      endif
      endif ! if ( QTYPE (jdet) .eq. 2 ) type 2 within dissolved oxygen ======DO
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      

      endif ! if ( detype (jdet) .eq. 104 )
      endif ! if ( QTYPE (JDET) .ne. 1 )
      endif ! if ( QTYPE (JDET) .ne. 4 )
 5010 continue ! end of dissolved oxygen -------------------------------------DO



*     parent substance ...

      do 5228 JDET = 1, NDET
*     ---------------------------------------------------------------- Henry VII
*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then

*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

*     isolate parent substance (detype = 200 for special treatment -------------
      if ( Detype (jdet) .eq. 200 ) then ! -------------------- parent substance
*     initialise the amount to be added to the daughter substance --------------
      PLOSS = 0.0

*     check the determinand is to be degraded ----------------------------------
      if ( QTYPE (jdet) .eq. 2 ) then

*     adjust the decay rates for temperature -----------------------------------
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)
*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0

*     old, pre-decay, value of the concentration -------------------------------
      BAD0 = CMS(JDET,IS)
      EBAD = 1.0

      if ( kay (jdet) .gt. 0.0 ) then
      EBAD = exp (- kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif ! if ( kay (jdet) .gt. 0.0 )

      Cnew = halt (jdet) + (BAD0 - halt (jdet) ) * EBAD

*     save PLOSS to add to the daughter substance ------------------------------
      Ploss = amax1 (0.0, (CMS(jdet,is) - Cnew ))

      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = amin1 ( CMS(JDET,IS), Cnew )
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS(JDET,IS) = 0.0
      endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 200 ) ----------------- daughter substance

      endif ! if ( QTYPE (JDET) .ne. 1 )
      endif ! if ( QTYPE (JDET) .ne. 4 )

 5228 continue ! parent determinand       


*     ==========================================================================
*     Daughter substance ...
*     ==========================================================================
      do 5310 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
      if ( Detype (jdet) .eq. 201 ) then ! ------------------ daughter substance 

*     extract the current value and add the results for the decay of the parent 
      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = CMS(JDET,IS) + PLOSS
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS(JDET,IS) = 0.0
      endif

      if ( QTYPE (jdet) .eq. 2 ) then
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
      ANY0 = CMS (JDET,IS)
      EANY = 1.0

      if ( Kay (jdet) .gt. 0.0 ) then
      EANY = exp ( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / Kay (jdet)
      endif ! if ( Kay (jdet) .gt. 0.0 )

      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS (JDET,IS) = amin1 ( CMS(JDET,IS),
     &(halt (jdet) + ( ANY0 - halt (jdet) ) * EANY))
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS (JDET,IS) = 0.0
      endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 201 ) ! --------------- daughter substance
      endif ! if ( QTYPE (JDET) .ne. 4 )

 5310 continue ! do 5310 jdet = 1, ndet


*     ==========================================================================
*     Nitrate ... add results for decay of ammonia -----------------------------
*     ==========================================================================
      do jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET)  .ne. 4 ) then

*     identify nitrate ------------------------------------------------- nitrate
      if ( Detype (jdet) .eq. 106 ) then ! ----------------------------- nitrate

*     extract the current value and add the results for the decay of ammonia ---
      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = CMS(JDET,IS) + Ammonia loss
      do ip = 1, nprop
*     concentrations from various types of feature --------------------- nitrate
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS(JDET,IS) = 0.0
      endif

      if ( QTYPE (jdet) .eq. 2 ) then
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)
*     check for potential underflow
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
      ANY0 = CMS (JDET,IS)
      EANY = 1.0
      if ( Kay (jdet) .gt. 0.0 ) then
      EANY = exp ( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / Kay (jdet)
      endif ! if ( Kay (jdet) .gt. 0.0 )

      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS (JDET,IS) = amin1 ( CMS(JDET,IS),
     &(halt (jdet) + ( ANY0 - halt (jdet) ) * EANY))
      do ip = 1, nprop
*     concentrations from various types of feature --------------------- nitrate
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS (JDET,IS) = 0.0
      endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 106 )

      endif ! if ( QTYPE (JDET)  .ne. 4 )
      enddo ! do jdet = 1, ndet ---------------------------------------- nitrate


*     all other degradable pollutants ------------------------------------------
*     adjust the Rate Constants for Temperature --------------------------------
      do 5011 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 ========
      if ( QTYPE (JDET) .ne. 4 ) then ! ========================================

*     skip the decay for conservative determinands - QTYPE is 1 ================
      if ( QTYPE (JDET) .ne. 1 ) then ! ========================================

      if ( detype (jdet) .ne. 104 ) then
      if ( detype (jdet) .ne. 103 ) then
      if ( detype (jdet) .ne. 101 ) then
      if ( detype (jdet) .ne. 200 ) then
      if ( detype (jdet) .ne. 201 ) then

      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0

*     old value ----------------------------------------------------------------
      BAD0 = CMS(JDET,IS)

      EBAD = 1.0

      if ( Kay (jdet) .gt. 0.0 ) then
      EBAD = exp (- kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif

      old CMS = CMS (JDET,IS)
      if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = amin1 ( CMS(JDET,IS),
     &(halt (jdet) + (BAD0 - halt (jdet) ) * EBAD))
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
      LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
      enddo
      else
      do ip = 1, nprop
      LMS(ip,JDET,IS) = 0.0 ! concentrations from various types of feature -----
      enddo
      CMS(JDET,IS) = 0.0
      endif

      endif ! if ( detype (jdet) .ne. 201 ) ------------------------------------
      endif
      endif
      endif
      endif
      endif ! if ( QTYPE (JDET) .ne. 1 ) =======================================
      endif ! if ( QTYPE (JDET) .ne. 4 ) =======================================

 5011 continue ! end of the loop on all the determinands
    9 continue ! end of the loop on all the shots


*     compute the average travel time and velocity -----------------------------
      avel = avel / float (NS)
      atim = 24.0 * atim / float (NS)

      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      half life (idet) = 24.0 * half life (idet) / float (NS)
      endif
      enddo

      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! =============================
      if ( nobigout .le. 0 ) write(34,2000) dint
*     if ( nobigout .le. 0 ) write(27,2000) dint
 2000 format(33x,77('-')/33x,
     &'Length of this stretch = ',f7.2,' kilometres (km)')
      if ( dint .gt. 0.000001) then ! ------------------------------------------
      write(34,2300) avel, atim
*     write(27,2300) avel, atim
 2300 format (
     &59x,'          Average velocity of flow = ',f7.2,' km/day'/
     &59x,'            Average time of travel = ',f7.2,'  hours')

      if ( nobigout .le. 0 ) write(34,2030) velo95, 24.0*time95
*     if ( nobigout .le. 0 ) write(27,2030) velo95, 24.0*time95
 2030 format(
     &59x,'Velocity at 95-percentile low flow = ',f7.2,' km/day '/
     &59x,'   Corresponding time of travel = ',f7.2,'  hours '/
     &33x,77('-'))
      endif ! if ( dint .gt. 0.000001 ) ----------------------------------------
      endif
      
      !endif ! if ( qtype(jper) .gt. 1 ) etc 
      !enddo ! do jper = 1,ndet

*     if ( ical .ge. 3 ) write(34,2111)
 2111 format(110('-')/
     &'Details of Gap Filling for flows follow later ....'/
     &110('-'))

      if ( dint .gt. 0.000001 ) then ! +++++++++++++++++++++++++++++++++++++++++     
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! +++++++++++++++++++++++++++++
      if ( ipur .ne. 0 .and. nobigout .le. 0 ) then ! ==========================
      write(34,4542)
 4542 format(110('-')/
     &'Changes in river quality added by Natural Purification ...'/
     &110('-'))
      endif
*     --------------------------------------------------------------------------
      do idet = 1,ndet ! =======================================================
      if ( qtype(idet) .ne. 0 .and. qtype(idet) .ne. 4 ) then ! ================
      if ( half life(idet) .gt. 0.000001 ) then ! ------------------------------
      redu = 100.0 * (1.0 - exp ( -kay0(idet) * atim/24.0 ))
      if ( nobigout .le. 0 ) write(34,2200)  Dname (idet), 
     &half life (idet), redu
 2200 format ('Average half-life for ',a11,' =',f6.1,' hours',
     &' ... average loss =',f7.2,' %')
      endif ! if ( half life (idet) .gt. 0.000001 ) ----------------------------
      endif ! if ( qtype(idet) .ne. 0 etc ======================================
      enddo ! do idet = 1,ndet =================================================

      endif ! if ( ical13 .eq. 0 ) +++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( dint .gt. 0.000001 ++++++++++++++++++++++++++++++++++++++++++

      if ( ipur .ne. 0) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(34,4562)
 4562 format(110('-'))
      endif
      endif

*     calculate loads and the summary statistics of load -----------------------

      call load calculation ! ----------------------------- natural purification

      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      do J13 = 1, N13
      NSM (idet,J13) = 0
      xupload (idet,J13) = 0.0
      xdownload (idet,J13) = 0.0
      enddo
*     loop on the shots --------------------------------------------------------
      do is = 1, ns ! ----------------------------------------------------------
      imonth = qmonth (is) ! set the month for this shot
      K13 = imonth + 1
      NSM (idet,K13) = NSM (idet,K13) + 1
*     calculate the difference in load caused by natural purification ----------
      xdiff = xshots (idet,is) - old shots (idet,is)
*     accumulate the mean load "added" by natural purification -----------------
      if ( xdiff .ge. 1.0e-10 ) then
      xupload (idet,I13) = xupload (idet,I13) + xdiff
      xupload (idet,K13) = xupload (idet,K13) + xdiff
      else
      xdownload (idet,I13) = xdownload (idet,I13) + xdiff
      xdownload (idet,K13) = xdownload (idet,K13) + xdiff
      endif
      enddo ! do is = 1, ns ----------------------------------------------------

*     calculate the average loads ----------------------------------------------
      xdownload (idet,i13) = xdownload (idet,i13) / float (ns)
      xupload (idet,i13) = xupload (idet,i13) / float (ns)
      xnet = xupload (idet,i13) + xdownload (idet,i13) 

      if ( xnet .gt. 0.00001) then
      xupload (idet,i13) = xnet
      xdownload (idet,i13) = 0.0
      else
      xdownload (idet,i13) = xnet
      xupload (idet,i13) = 0.0
      endif

      TNLOADDN2 (idet,i13) = TNLOADDN2 (idet,i13) + xdownload (idet,i13)
      TNLOADDN1 (idet,I13) = TNLOADDN1 (idet,I13) + xdownload (idet,I13)
      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      if ( NSM  (idet,J13) .gt. 0 ) then
      xdownload (idet,J13) = xdownload (idet,J13)/ float (NSM(idet,J13))
      xupload (idet,J13) = xupload (idet,J13)/ float (NSM(idet,J13)) 
      xnet = xupload (idet,J13) + xdownload (idet,J13)

      if ( xnet .gt. 0.00001) then
      xupload (idet,J13) = xnet
      xdownload (idet,J13) = 0.0
      else
      xdownload (idet,J13) = xnet
      xupload (idet,J13) = 0.0
      endif

      TNLOADDN2 (idet,J13) = TNLOADDN2 (idet,J13) + xdownload (idet,J13) 
      TNLOADDN1 (idet,J13) = TNLOADDN1 (idet,J13) + xdownload (idet,J13) 
      endif
      enddo
      endif ! fill monthly loads
      endif
      enddo ! do idet = 1, ndet

      call add up all the loads ! for all determinands

*     calculate the effect of losses on the accumulated loads ------------------
      do idet = 1, ndet ! ------------------------------------------------------
      if ( QTYPE (idet) .ne. 4 ) then ! ----------------------------------------
      kprune det = idet ! identify the determinand for load pruning ------------
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads

      do J1 = 1, 1 ! -------------------------------------- natural purification 
      prune load (J1) = 1.0 ! ----------------------------- natural purification
      if ( abs ( TGLODE2(idet,J1 ) ) .gt. 0.000000001 ) then
      if ( xdownload (idet,J1) .lt. 0.0 ) then
      if ( TGLODE2(idet,J1) + xdownload (idet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(idet,J1) + xdownload (idet,J1)) 
     &                /  TGLODE2(idet,J1)
      endif
      endif ! if ( xdownload (idet,J1) .lt. 0.0 )
      endif ! if ( abs ( TGLODE2(idet,J1 )
      enddo ! do J1 = 1, nx

*     trim back the loads for losses on natural purification -------------------
      call scale loads after losses ! losses on natural purification  ----------

      endif ! if ( QTYPE (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------


*     gains in natural purification --------------------------------------------
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      TNLOADUP2 (idet,J1) = TNLOADUP2 (idet,J1) + xupload (idet,J1)   
      enddo
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo ! do idet = 1, ndet

*     write out the river loads after natural purification ---------------------
      if ( ical13 .eq. 0 )  then
      call write out the river loads after natural purification
      endif

      return
      end

      
      
*     compute saturated concentration of Dissolved Oxygen ----------------------
      function DO Saturation (T)
      DO Saturation = 14.652-0.41022*T+0.007991*T*T-0.000077774*T*T*T
      return
      end



*     ##########################################################################
*     trim back the loads for natural purification -----------------------------
      subroutine scale loads after losses ! from natural purification ----------
      include 'COMMON DATA.FOR'

*     trim annual loads from individual works ----------------------------------
      if ( kountworks .gt. 0 .and. kprune det .gt. 0 ) then
      do iworks = 1, kountworks
      TELOADAV(iworks,kprune det) = ! prune the load
     &TELOADAV(iworks,kprune det) * prune load (i13)

      if ( n251 .eq. 1 ) then ! 3333333333333333333 apportionment of percentiles
      do is = 1, NS ! 33333333333333333333333333333 apportionment of percentiles
      if ( kprune det .eq. ndshot ) TELOADshots(iworks,kprune det,is) =
     &TELOADshots(iworks,kprune det,is) * prune load (i13) ! 3333333333333333333
      enddo ! do is = 1, NS 33333333333333333333333 apportionment of percentiles
      endif ! if ( n251 .eq. 1 ) ! 3333333333333333 apportionment of percentiles

      enddo ! do iworks = 1, kountworks
      endif ! if ( kountworks .gt. 0 .and. kprune det .gt. 0 )
      
*     ==========================================================================
*     trim annual loads from individual catchments =============================
      if ( kount bodies .gt. 0 ) then ! ========================================
      do ibodies = 1, kount bodies ! ===========================================
*     annual loads (i13) from upstream sub-catchments --------------------------
      TWLOADS(ibodies,kprune det,i13) = TWLOADS(ibodies,kprune det,i13) 
     &                                * prune load (i13)

      do ip = 1, nprop ! scale loads after losses ------------------------------
*     breakdown of annual loads (i13) from upstream sub-catchments -------------
      TWLOADSapp(ibodies,kprune det,i13,ip) = 
     &TWLOADSapp(ibodies,kprune det,i13,ip) * prune load (i13)
      enddo ! do ip = 1, nprop -------------------------------------------------

      enddo ! do ibodies = 1, kount bodies ! ===================================
      endif ! if ( kount bodies .gt. 0 ) =======================================
*     ==========================================================================


*     ##########################################################################
*     trim any total load added by natural purification ========================
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------      

*     prune the load added by natural purification ----------------------------
      if ( prune load (J1) .lt. 1.0 ) then ! -----------------------------------
*     prune gains from natural purification ------------------------------------
      if ( TNLOADUP2(kprune det,J1) .gt. 0.0 ) then ! if there are gains ------- 
      TNLOADUP2(kprune det,J1) = TNLOADUP2(kprune det,J1)
     &                         * prune load (J1)
      endif ! if ( TNLOADUP2(kprune det,J1)
      endif ! if ( prune load (J1) .lt. 1.0 ) ----------------------------------

*     ====================================================================== 158
      if ( TRLODE2(kprune det,J1) .gt. 0.0 ) then
      TRLODE2(kprune det,J1) = TRLODE2(kprune det,J1) * prune load (J1)
      endif
*     net loads from all discharges (3 12 5 and 39) ---------------------TELODE2
      if ( TELODE2(kprune det,J1) .gt. 0.0 ) then
      TELODE2(kprune det,J1) = TELODE2(kprune det,J1) * prune load (J1)
      endif
      if ( T03LOAD2(kprune det,J1) .gt. 0.0 ) then
      T03LOAD2(kprune det,J1) = T03LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T05LOAD2(kprune det,J1) .gt. 0.0 ) then
      T05LOAD2(kprune det,J1) = T05LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T12LOAD2(kprune det,J1) .gt. 0.0 ) then
      T12LOAD2(kprune det,J1) = T12LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T39LOAD2(kprune det,J1) .gt. 0.0 ) then
      T39LOAD2(kprune det,J1) = T39LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T60LOAD2(kprune det,J1) .gt. 0.0 ) then
      T60LOAD2(kprune det,J1) = T60LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T61LOAD2(kprune det,J1) .gt. 0.0 ) then
      T61LOAD2(kprune det,J1) = T61LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     prune load introduced to reaches by clean diffuse sources ----------------
      if ( TDDLOAD2(kprune det,J1) .gt. 0.0 ) then
      TDDLOAD2(kprune det,J1) = TDDLOAD2(kprune det,J1) 
     &                        * prune load (J1)
      endif
*     prune load introduced by Gap Filling for river flows ----------------- 157 
      if ( TILOADUP2(kprune det,J1) .gt. 0.0 ) then
      TILOADUP2(kprune det,J1) = TILOADUP2(kprune det,J1)
     &                         * prune load (J1)
      endif ! if ( TILOADUP2(kprune det,J1)
*     prune load introduced by Gap Filling for river quality -------------------
      if ( TALOADUP2(kprune det,J1) .gt. 0.0 ) then
      TALOADUP2(kprune det,J1) = TALOADUP2(kprune det,J1) 
     &                         * prune load (J1)
      endif ! if ( TALOADUP2(kprune det,J1) .gt. 0.0 
*     ====================================================================== 157
*     prune load from diffuse features (river flow type) -----------------------
      if ( T13LOAD2(kprune det,J1) .gt. 0.0 ) then
      T13LOAD2(kprune det,J1) = T13LOAD2(kprune det,J1) 
     &                        * prune load (J1)
      endif
*     prune load from diffuse features (discharge flow type) -------------------
      if ( T15LOAD2(kprune det,J1) .gt. 0.0 ) then
      T15LOAD2(kprune det,J1) = T15LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     ====================================================================== 157
*     prune load from livestock diffuse features (river flow type) -------------
      if ( T25LOAD2(kprune det,J1) .gt. 0.0 ) then
      T25LOAD2(kprune det,J1) = T25LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     prune load from arable diffuse features (river flow type) ------------
      if ( T27LOAD2(kprune det,J1) .gt. 0.0 ) then
      T27LOAD2(kprune det,J1) = T27LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T29LOAD2(kprune det,J1) .gt. 0.0 ) then
      T29LOAD2(kprune det,J1) = T29LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T31LOAD2(kprune det,J1) .gt. 0.0 ) then
      T31LOAD2(kprune det,J1) = T31LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T33LOAD2(kprune det,J1) .gt. 0.0 ) then
      T33LOAD2(kprune det,J1) = T33LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T35LOAD2(kprune det,J1) .gt. 0.0 ) then
      T35LOAD2(kprune det,J1) = T35LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     ====================================================================== 157
      if ( T40LOAD2(kprune det,J1) .gt. 0.0 ) then
      T40LOAD2(kprune det,J1) = T40LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     prune load from diffuse features (river flow type) ------------------ (42)
      if ( T42LOAD2(kprune det,J1) .gt. 0.0 ) then ! ------ aggregated STWS (42)
      T42LOAD2(kprune det,J1) = T42LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T46LOAD2(kprune det,J1) .gt. 0.0 ) then
      T46LOAD2(kprune det,J1) = T46LOAD2(kprune det,J1)
     &                        * prune load (J1)
  	endif
      if ( T48LOAD2(kprune det,J1) .gt. 0.0 ) then
      T48LOAD2(kprune det,J1) = T48LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     ====================================================================== 157
      if ( T50LOAD2(kprune det,J1) .gt. 0.0 ) then
      T50LOAD2(kprune det,J1) = T50LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     prune load from diffuse features (river flow type) -----------------------
      if ( T52LOAD2(kprune det,J1) .gt. 0.0 ) then
      T52LOAD2(kprune det,J1) = T52LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T54LOAD2(kprune det,J1) .gt. 0.0 ) then
      T54LOAD2(kprune det,J1) = T54LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T56LOAD2(kprune det,J1) .gt. 0.0 ) then
      T56LOAD2(kprune det,J1) = T56LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
      if ( T58LOAD2(kprune det,J1) .gt. 0.0 ) then
      T58LOAD2(kprune det,J1) = T58LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif

      if ( T37LOAD2(kprune det,J1) .gt. 0.0 ) then
      T37LOAD2(kprune det,J1) = T37LOAD2(kprune det,J1)
     &                        * prune load (J1)
      endif
*     ==========================================================================
*     prune load from those added by natural purification ----------------------
      if ( TNLOADUP2(kprune det,J1) .gt. 0.0 ) then
      TNLOADUP2(kprune det,J1) = TNLOADUP2(kprune det,J1)
     &                         * prune load (J1)
      endif
      !endif ! if ( prune load (J1) .lt. 1.0 )
      enddo ! do J1 = 1, nx

      return
      end


      subroutine sort format 1 (v1)
      include 'COMMON DATA.FOR'
      character *10 valfetch
      valfetch = '       - '
      valchars10 = '       - '
      call fetch ( v1, valfetch)
      valchars10 = valfetch
      return
      end
      
      subroutine sort format 1a (v1)
      include 'COMMON DATA.FOR'
      character *10 valfetch
      valfetch = '       - '
      valchars10 = '       - '
      call fetch 2 (v1, valfetch)
      valchars10 = valfetch
      return
      end

      
      subroutine sort format 2 (v1,v2)
      include 'COMMON DATA.FOR'
      character *10 valfetch
      valchars10 = '       - '
      valchars11 = '       - '
      valfetch = '       - '
      call fetch ( v1, valfetch)
      valchars10 = valfetch
      valfetch = '       - '
      call fetch ( v2, valfetch)
      valchars11 = valfetch
      return
      end

      subroutine sort format 3 (v1,v2,v3)
      include 'COMMON DATA.FOR'
      character *10 valfetch
      valfetch = '       - '
      valchars10 = '       - '
      valchars11 = '       - '
      valchars12 = '       - '
      call fetch ( v1, valfetch)
      valchars10 = valfetch
      call fetch ( v2, valfetch)
      valchars11 = valfetch
      call fetch ( v3, valfetch)
      valchars12 = valfetch
      return
      end

      
      subroutine fetch (fff,textx)
      character *10 textx

      neg = 0
      if ( fff .lt. 0.0 ) neg = 1
      
      if ( neg .eq. 0 ) then ! positive vallue +++++++++++++++++++++++++++++++++
*     greater than 1,000,000 ---------------------------------------------------
      if (fff .gt. 999999.45 ) then
      write(textx,1) fff
    1 format(1pe10.3)
      goto 99
      endif
      
*     less than 0.000000002 ----------------------------------------------------
      if (fff .lt. 0.00000002) then
      write(textx,2) int (fff)
      goto 99
      endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (fff .gt. 9999.45 ) then
      write(textx,2) int (fff)
    2 format(i10)
      goto 99
      endif
*     less than 10,000 and greater than 10 -------------------------------------
      if (fff .gt. 9.99) then
      write(textx,3) fff
    3 format(F10.1)
      goto 99
      endif
*     less than 10 and greater than 1 -----------------------------------------
      if (fff .gt. 0.999) then       
      write(textx,4) fff
    4 format(F10.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if (fff .gt. 0.0999) then       
      write(textx,5) fff
    5 format(F10.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
      if (fff .gt. 0.00999) then       
      write(textx,6) fff
    6 format(f10.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
      if (fff .gt. 0.000999) then               
      write(textx,7) fff
    7 format(f10.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
      if (fff .gt. 0.00000999) then               
      write(textx,8) fff
    8 format(f10.6)
      goto 99
      endif
*     less than 0.00001 and greater than 0.000001 ---------------------------------
      if (fff .gt. 0.0000000999) then               
      write(textx,9) fff
    9 format(f10.7)
      goto 99
      endif
*     less than 0.000001 and greater than 0.0000001 +++++++++++++++++++++++++++++
*     if (fff .gt. 0.0000000999) then               
*     write(textx,10) fff
*  10 format(f10.8)
*     goto 99
*     endif

*     less than 0.000001 and greater than 0.000001 +++++++++++++++++++++++++++++
*     if (fff .gt. 0.00000000999) then               
*     write(textx,11) fff
*  11 format(f10.9)
*     goto 99
*     endif
*     zero to 1.e-8 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      write(textx,2) int (fff)
      
      else ! -------------------------------------------------------------------
      
*     less than than -1,000,000 ------------------------------------------------
      if (fff .lt. -999999.45 ) then
      write(textx,1) fff
      goto 99
      endif
*     less than -0.000000002 ---------------------------------------------------
      if (fff .gt. -0.000000002) then
      write(textx,2) int (fff)
      goto 99
      endif
*     less that -1,000,000 and less than -10,000 ----------------------------
      if (fff .lt. -9999.45 ) then
      write(textx,2) int (fff)
      goto 99
      endif
*     less than -10,000 and greater than -10 -----------------------------------
      if (fff .lt. -9.99) then
      write(textx,3) fff
      goto 99
      endif
*     less than -10 and greater than -1 ----------------------------------------
      if (fff .lt. -0.999) then       
      write(textx,4) fff
      goto 99
      endif
*     less than -1 and greater than -0.1 ---------------------------------------
      if (fff .lt. -0.0999) then       
      write(textx,5) fff
      goto 99
      endif
*     less than -0.1 and greater than -0.01 ------------------------------------
      if (fff .lt. -0.00999) then       
      write(textx,6) fff
      goto 99
      endif
*     less than -0.0100 and greater than -0.001 --------------------------------
      if (fff .lt. -0.000999) then               
      write(textx,7) fff
      goto 99
      endif
*     less than -0.001 and greater than -0.00001 -------------------------------
      if (fff .lt. -0.00000999) then               
      write(textx,8) fff
      goto 99
      endif
*     less than -0.00001 and greater than -0.00001 -----------------------------
      if (fff .lt. -0.000000999) then               
      write(textx,9) fff
      goto 99
      endif
*     less than -0.000001 and greater than -0.000001 ---------------------------
      if (fff .lt. -0.0000000999) then               
      write(textx,9) fff
      goto 99
      endif
*     less than 0.000001 and greater than -0.000001 ----------------------------
*     if (fff .gt. 0.00000000999) then               
*     write(textx,11) fff
*     goto 99
*     endif
*     zero to 1.e-8 ------------------------------------------------------------
      write(textx,2) int (fff)
      endif
         
   99 continue 

      return
      end
      
      
      
      
      subroutine fetch 2 (fff,textx)
      character *10 textx
      
      !if (fff .lt. 999999.995 ) then
      write(textx,3) fff
    3 format(F10.2)
      return
      !else
      !write(textx,1) fff
    1 format(1pe10.3)
      !return
      !endif
              
      !return
      end
 


      subroutine write resulting flow and quality
      include 'COMMON DATA.FOR'
      
      if ( nobigout .gt. 0 ) return
      
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,4)valchars11,valchars10,funit
      write(34,4)valchars11,valchars10,funit
    4 format(33x,77('-')/33x,
     &'Resulting river flow',15X,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)

      do J = 1, Ndet
      if ( QTYPE (J) .ne. 4 ) then ! -------------------------------------------
          
      idp = PDRC(IQ,J)
      if ( diffuse type .eq. 15 .or. diffuse type .eq. 40 )
     &     idp = PDEC(IQ,J)
      
      call sort format 2 (C(J,1),C(J,2))
      if ( C(J,1) .gt. 1.0 ) then ! ============================================
      write(01,1)DNAME(J),valchars11,valchars10,UNITS(J),idp
      write(34,1)DNAME(J),valchars11,valchars10,UNITS(J),idp
    1 format(33x,'Resulting ',A11,14x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,' ...  Data-type:',i3)
      else ! if ( C(J,1) .gt. 1.0 ) then =======================================

      if ( C(J,1) .le. 0.0000001 ) then ! for a zero mean ----------------------
      write(01,2)DNAME(J),valchars10,UNITS(J),idp
      write(34,2)DNAME(J),valchars10,UNITS(J),idp
    2 format(33x,'Resulting ',A11,35x,'Mean =',a10,1x,a4,
     &' ---  Data-type:',i3)
      else ! for a non-zero mean -----------------------------------------------
      write(01,3)DNAME(J),valchars11,valchars10,UNITS(J),idp
      write(34,3)DNAME(J),valchars11,valchars10,UNITS(J),idp
    3 format(33x,'Resulting ',A11,14x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4,' ...  Data-type:',i3)
      endif ! if ( C(J,1) .lt. 0.0000001 ) then --------------------------------
      endif ! if ( C(J,1) .gt. 1.0 ) ===========================================
      endif ! if ( QTYPE (J) .ne. 4 ) ------------------------------------------
      enddo ! do J = 1, ndet ===================================================
      write(01,6631)
 6631 format(33x,77('+'))
      
      if ( distp .gt. 0.000001 ) then ! ========================================
      if ( diffuse type .eq. 15 .or. diffuse type .eq. 40 )
     &write(01,5)diffuse type
    5 format(33x,77('-'),2i4)
      endif ! if ( distp .gt. 0.000001 ) =======================================

      return
      end