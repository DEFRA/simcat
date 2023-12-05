*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Natural Purification Suite....   3006
*     --------------------------------------------------------------------------
      subroutine add diffuse sources and natural purification (icome)
      include 'COM.FOR'
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
      if ( icome .eq. 0 ) then
      if ( iend .eq. 0 ) then
      if ( istart .eq. 1 ) then
      if ( nobigout .le. 0 ) then
	if ( no features .eq. 1 ) then
      write(01,2200) RNAME(IREACH), vname (KFEET)
      write(21,2200) RNAME(IREACH), vname (KFEET)
 2200 format (//110('=')/'Stretch: ',a16,6x,'From the start of the ',
     &'Reach to:',11x,a37/110('='))
	else
      write(01,2230) RNAME(IREACH)
      write(21,2230) RNAME(IREACH)
 2230 format (//110('=')/'Stretch: ',a16,6x,'From the start of the ',
     &'Reach ...',
     &24x,' to the end of the Reach'/110('='))
	endif
	endif
      istart = 0
	no features = 0
	else ! if ( istart .eq. 1 )
	
      if ( nobigout .le. 0 ) then
      if ( DIST(feeture) .lt. 1.0e-08 ) then   
      if ( JT (feeture) .eq. 25 ) goto 2099
      if ( JT (feeture) .eq. 27 ) goto 2099
      if ( JT (feeture) .eq. 29 ) goto 2099
      if ( JT (feeture) .eq. 31 ) goto 2099
      if ( JT (feeture) .eq. 33 ) goto 2099
      if ( JT (feeture) .eq. 35 ) goto 2099
      if ( JT (feeture) .eq. 37 ) goto 2099
      if ( JT (feeture) .eq. 40 ) goto 2099
      if ( JT (feeture) .eq. 42 ) goto 2099
      if ( JT (feeture) .eq. 46 ) goto 2099
      if ( JT (feeture) .eq. 48 ) goto 2099
      endif
      
      write(01,2000) RNAME(IREACH), uname (KFEET-1),vname (KFEET)
      write(21,2000) RNAME(IREACH), uname (KFEET-1),vname (KFEET)
 2000 format (//110('=')/'Stretch: ',a16,' from: ',a37,' to ',a37/
     &110('='))
      endif
 2099 continue     
	endif ! if ( istart .eq. 1 )
	endif ! if ( iend .eq. 0 )
      if ( iend .eq. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,2100) RNAME(IREACH), uname (KFEET)
      write(21,2100) RNAME(IREACH), uname (KFEET)
 2100 format (//110('=')/'Stretch: ',a16,' from: ',a37,
     &17x,' to the end of the Reach'/110('='))
	endif
	endif ! if ( iend .eq. 1 )
      endif ! if ( icome .eq. 0 )

*     use of this routine for Gap Filling ======================================
      if ( icome .eq. 1001 ) then
      if ( iend .eq. 0 ) then
      if ( kstart .eq. 1 ) then
      write(09,2210) RNAME(IREACH), vname (KFEET+1)
 2210 format (/110('#')/'Stretch: ',a16,6x,'From the start of the ',
     &'Reach to:',15x,a37/110('#'))
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
      endif

*     if ( distp .le. 0.0 ) return
      DISX = DISTP
      DISTX = DISTP
      DINT = DISTP

*     the stretch will be split into INTD intervals (set to 1 initially) -------
      INTD = 1

*     add in the reach-type diffuse inflows ------------------------------------
      if ( IDIFF .gt. 0 ) call REACH DIFFUSE

*     check for inflow from a lake ---------------------------------------------
      if ( lake45 .eq. 1 ) DINT = 1.0
	DINTP = DINT

      if ( KRFPOL13 .gt. 0 ) then ! add in diffuse pollution (river type - 13)
      IQ = KRQPOL13
      IF = KRFPOL13
      JTKEEP = JT(KFEET)
      JT(KFEET) = 13
	diffuse type = 13
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow ! diffuse pollution (13)
      call get summaries of river quality from the shots ! diffuse pollution (13)

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)dint
	write(34,5282)dint
 5282 format(33x,77('=')/33x,'Various diffuse inflows over the next',
     &f8.2,' kilometres ...'/33x,77('='))
	diffuse heading = 1
	endif

      write(01,3482)IF
      write(34,3482)IF
 3482 format(33x,77('=')/33x,
     &'Insert river-type diffuse inflows (13) ... ',
     &'Flow data set:',I6)

      call sort format 2 (FLOW(1),FLOW(2))
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
 1482 format(33x,77('-')/
     &33x,'Starting river flow',16x,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)
      do J = 1, ndet
	if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
 1422 format(33x,'Starting ',A11,15x,'St.dev =',a10,3x,
     &'Mean =',a10,1x,a4)
	endif
      enddo
      endif ! if ( nobigout .le. 0 )
	endif ! if ( ical13 .eq. 0 )
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) then
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)dint
	diffuse heading = 1
	endif
      
      iprime = 1

      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
	write(34,2082)dint,valchars11,valchars10,funit
 2082 format(33x,77('-')/33x,
     &'Total flow ADDED over',f6.1,' km',5X,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4/33x,77('-'))
      endif ! if ( nobigout .le. 0 ) 
	endif ! if ( ical13 .eq. 0 )
	endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 )

*     note whether the distribution is non-parametric ==========================
      if ( PDRF(IF) .eq. 4 ) then
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
 2970 if ( nobigout .le. 0 ) then
      write(01,7363) FLMONTHsmall(1,icod)
 7363 format(33x,'These flows are from monthly distributions ...',
     &' File: ',a64/33x,77('='))
      endif
      endif ! monthly data

      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
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
 3170 if ( nobigout .le. 0 ) then
      write(01,3961) FLSTRUCTsmall(1,icod)
 3961 format(33x,
     &'These flows have monthly structure (type 8) ',
     &'... File: ',a64/33x,77('='))
      endif
      endif !  monthly structure

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2044)IQ
 2044 format(33x,'Quality of this diffuse inflow (13): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! diffuse pollution (13)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
 
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
 3082 format(33x,77('-')/
     &33x,'Resulting river flow',15X,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)

      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
 	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
 1022 format(33x,'Resulting ',A11,14x,'St.dev =',F10.2,3x,
     &'Mean =',F10.2,1x,a4)
	else

	if ( C(J,1) .lt. 0.0000001 ) then
      write(01,4322)DNAME(J),C(J,1),UNITS(J)
      write(34,4322)DNAME(J),C(J,1),UNITS(J)
 4322 format(33x,'Resulting ',A11,35x,'Mean =',F10.5,1x,a4)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
 1922 format(33x,'Resulting ',A11,14x,'St.dev =',F10.5,3x,
     &'Mean =',F10.5,1x,a4)
	endif ! if ( C(J,1) .lt. 0.0000001 ) then
	endif ! if ( C(J,1) .gt. 1.0 )
	endif ! if ( QTYPE (J) .ne. 4 )
      enddo

      if ( distp .gt. 0.000001 ) write(01,6388)
 6388 format(33x,77('='))
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL13 = 0
      endif ! end for diffuse pollution (river type - 13)
*     -------------------------------------- diffuse pollution (river type - 13)


*     --------------------------------------------------------------------------
*     add in diffuse pollution (livestock river type - 25) ---------------------
*     ------------------------------------------------------------------------25
      if ( KRFPOL25 .gt. 0 ) then
      IQ = KRQPOL25
      IF = KRFPOL25
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 25
	diffuse type = 25
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots


      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
 	diffuse heading = 1
	endif

      write(01,2944)IF
	write(34,2944)IF
 2944 format(33x,77('=')/33x,'Diffuse inflow (25) from livestock ',
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
      write(01,2082)dint,valchars11,valchars10,funit
	write(34,2082)dint,valchars11,valchars10,funit
      endif ! if ( nobigout .le. 0 )
	endif ! if ( ical13 .eq. 0 )
	endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 
*     ------------------------------------------------------------------------25


*     -------------------------------------------------------------------------X
*     note whether the distribution is non-parametric --------------------------
      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ---------------------------
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
*     ------------------------------------------------------------------------25


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
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
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
      endif ! monthly structure
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2094)IQ
 2094 format(33x,'Quality of diffuse inflow from livestock (25): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

*     add in diffuse pollution (livestock river type - 25) -------------------25
	call STREAM DIFFUSE ! livestock river (25)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL25 = 0
  	endif ! if ( KRFPOL25 .gt. 0 )
*     ---------------------------- diffuse pollution (livestock river type - 25)



*     --------------------------------------------------------------------------
*     add in diffuse pollution (arable river type - 27) ------------------------
*     ------------------------------------------------------------------------27
      if ( KRFPOL27 .gt. 0 ) then
      IQ = KRQPOL27
      IF = KRFPOL27
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 27
	diffuse type = 27
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif

      write(01,4944)IF
	write(34,4944)IF
 4944 format(33x,77('=')/33x,'Diffuse inflow (27) from arable ',
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
      write(01,2082)dint,valchars11,valchars10,funit
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

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
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
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,4994)IQ
 4994 format(33x,'Quality of diffuse inflow from arable (27): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

*     add in diffuse pollution (arable river type - 27) ------------------------
	call STREAM DIFFUSE ! arable river type - 27
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL27 = 0
  	endif ! if ( KRFPOL27 .gt. 0 )
*     ------------------------------- diffuse pollution (arable river type - 27)



*     add in highway runoff (river type - 29) ----------------------------------
      if ( KRFPOL29 .gt. 0 ) then
      IQ = KRQPOL29
      IF = KRFPOL29
      JTKEEP = JT(KFEET)
  	JT(KFEET) = 29
	diffuse type = 29
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,2244)IF
      write(34,2244)IF
 2244 format(33x,77('=')/33x,'Diffuse inflow (29) from highway ',
     &'runoff: Flow data set:',I6)
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
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,2994)IQ
 2994 format(33x,'Quality of diffuse inflow from highway runoff (29): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
*     ------------------------------------------------------------------------29

*     add in highway runoff (river type - 29) ----------------------------------
*     ------------------------------------------------------------------------29
	call STREAM DIFFUSE ! highway runoff (29)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
      write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
      write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL29 = 0
  	endif ! if ( KRFPOL29 .gt. 0 )
*     ----------------------------------------- highway runoff (river type - 29)




*     --------------------------------------------------------------------------
*     add in urban runoff (river type - 31) ------------------------------------
*     ------------------------------------------------------------------------31
      if ( KRFPOL31 .gt. 0 ) then
      IQ = KRQPOL31
      IF = KRFPOL31
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 31
	diffuse type = 31
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,1244)IF
      write(34,1244)IF
 1244 format(33x,77('=')/33x,'Diffuse inflow (31) from urban runoff: ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,1994)IQ
 1994 format(33x,'Quality of diffuse inflow from urban runoff (31): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) 


*     add in urban runoff (river type - 31) ------------------------------------
	call STREAM DIFFUSE ! urban runoff (31)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
      write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
      write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL31 = 0
  	endif ! if ( KRFPOL31 .gt. 0 )
*     ------------------------------------------- urban runoff (river type - 31)




*     add in atmospheric deposition(river type - 33) ---------------------------
      if ( KRFPOL33 .gt. 0 ) then
      IQ = KRQPOL33
      IF = KRFPOL33
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 33
	diffuse type = 33
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,1884)IF
      write(34,1884)IF
 1884 format(33x,77('=')/33x,'Diffuse inflow (33) from atmospheric ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,3994)IQ
 3994 format(33x,'Quality of diffuse inflow from atmospheric ',
     &'deposition (33): data set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in atmospheric deposition (33)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL33 = 0
  	endif ! if ( KRFPOL33 .gt. 0 )
*     ----------------------------------- atmospheric deposition(river type - 33)





*     --------------------------------------------------------------------------
*     add in natural background (river type - 35) ------------------------------
*     ------------------------------------------------------------------------35
      if ( KRFPOL35 .gt. 0 ) then
      IQ = KRQPOL35
      IF = KRFPOL35
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 35
	diffuse type = 35

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,1824)IF
      write(34,1824)IF
 1824 format(33x,77('=')/33x,'Diffuse inflow (35) from natural ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,3924)IQ
 3924 format(33x,'Quality of diffuse inflow from natural ',
     &'background (35): data set:',I6/33x,77('-'))
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in natural background (35)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! natural background (35)
      call get summaries of river quality from the shots ! natural background (35)
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL35 = 0
      endif ! if ( KRFPOL35 .gt. 0 )
*     ------------------------------------- natural background (river type - 35)
      


*     add in diffuse mines (river type - 46) -----------------------------------
      if ( KRFPOL46 .gt. 0 ) then
      IQ = KRQPOL46
      IF = KRFPOL46
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 46
	diffuse type = 46

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,6824)IF
      write(34,6824)IF
 6824 format(33x,77('=')/33x,'Diffuse inflow (46) from mines: ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,3984)IQ
 3984 format(33x,'Quality of diffuse inflow from mines (46): ',
     &'Data set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in diffuse from mines (46)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL46 = 0
  	endif ! if ( KRFPOL46 .gt. 0 )
*     ------------------------------------------ diffuse mines (river type - 46)


*     add in bird, boats and angling (river type - 48) -------------------------
      if ( KRFPOL48 .gt. 0 ) then
      IQ = KRQPOL48
      IF = KRFPOL48
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 48
	diffuse type = 48

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,7824)IF
      write(34,7824)IF
 7824 format(33x,77('=')/33x,'Diffuse inflow (48) from ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,7984)IQ
 7984 format(33x,'Quality of diffuse inflow from birds, ',
     &'boats and angling (48): ','Data set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in birds, boats and angling (48)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow ! birds, boats and angling (48)
      call get summaries of river quality from the shots ! birds, boats and angling (48)
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL48 = 0
  	endif ! if ( KRFPOL48 .gt. 0 )
*     ------------------------------- birds, boats and angling (river type - 48)



*     add in septic tanks (river type - 37) ------------------------------------
      if ( KRFPOL37 .gt. 0 ) then
      IQ = KRQPOL37
      IF = KRFPOL37
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 37
	diffuse type = 37
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,1784)IF
      if ( nobigout .le. 0 ) write(34,1784)IF
 1784 format(33x,77('=')/33x,'Diffuse inflow (37) from septic tanks: ',
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
      call sort format 2 (DINT*F(IF,1),DINT*F(IF,2))
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,3794)IQ
 3794 format(33x,'Quality of diffuse inflow from septic tanks (37): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33
      endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in septic tanks (37)
      JT(KFEET) = JTKEEP
      diffuse type = 0

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL37 = 0
  	endif ! if ( KRFPOL37 .gt. 0 )
*     ------------------------------------------- septic tanks (river type - 37) 



*     add in aggregated CSOs (river type - 40) ---------------------------------
      if ( KRFPOL40 .gt. 0 ) then
      IQ = KRQPOL40
      IF = KRFPOL40
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 40
	diffuse type = 40
      
      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,3784)IF
      write(34,3784)IF
 3784 format(33x,77('=')/33x,'Diffuse inflow (40) from aggregated ',
     &'CSOs: Flow data set:',I6)
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
      write(01,2082)dint,valchars11,valchars10,funit
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
      if ( nobigout .le. 0 ) write(01,3714)IQ
 3714 format(33x,'Quality of diffuse inflow from aggregated ',
     &'CSOs (40): data set:',I6/33x,77('-'))
      call write mean and standard deviation 33
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )

	call STREAM DIFFUSE ! add in aggregated CSOs (40)
      JT(KFEET) = JTKEEP
      diffuse type = 0 ! after adding in aggregated CSOs (40)

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
      if ( distp .gt. 0.000001 ) write(01,6388)
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KRFPOL40 = 0
  	endif ! if ( KRFPOL40 .gt. 0 )
*     ---------------------------------------- aggregated CSOs (river type - 40)




*     add in diffuse pollution (effluent type) -------------------------------15
      if ( KEPOL15  .gt. 0 ) then

	IQ = KEPOL15
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 15
	diffuse type = 15

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

      if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then	
	if ( diffuse heading .eq. 0 ) then
	write(01,5282)distp
	diffuse heading = 1
	endif
      write(01,5944)IQ
      write(34,5944)IQ
 5944 format(33x,77('=')/33x,'Discharge-type diffuse inflow (15)',
     &': data set:',I6)
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
      call sort format 2 (DINT*FE(IQ,1),DINT*FE(IQ,2))
	write(01,3210)DINT,valchars11,valchars10,funit
 3210 format(33x,77('-')/
     &33x,'Total flow ADDED over',f6.1,' km:',4x,
     &'St.dev =',a10,3x,'Mean =',a10,1x,A4/33x,77('-'))
	write(34,3210)DINT,valchars11,valchars10,funit

      endif ! if ( ical13 .eq. 0 )
	endif ! if ( nobigout .le. 0 )
	endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 
*     ------------------------------------------------------------------------15

*     ========================================================================15
*     note whether the distribution is non-parametric ==========================
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
*     note whether the data are monthly ----------------------------------------
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
 4976 if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(3,icod)
      endif !  if ( PDEF(IQ) .eq. 5 ) 
*     ==========================================================================

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
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     -------------------------------------------------------------------------X
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2194)IQ
 2194 format(33x,'Quality of diffuse effluent (15): ',
     &'data set:',I6/33x,77('-'))
      call write mean and standard deviation 33a
	endif !  if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 )
*     ------------------------------------------------------------------------15



*     add in diffuse pollution (effluent type) --------------------------------15
	call EFFLUENT DIFFUSE
      JT(KFEET) = JTKEEP
      diffuse type = 0 ! after diffuse pollution (effluent type) (15)

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
	write(01,6388)
	write(34,6388)
	endif ! if ( ical13 .eq. 0 )
	endif ! if ( nobigout .le. 0 )
	endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KEPOL15 = 0
      endif ! if ( KEPOL15  .gt. 0 )
*     ---------------------------------------- diffuse pollution (effluent type)






*     add in aggregated STWs (effluent type) -----------------------------------
      if ( KEPOL42  .gt. 0 ) then

	IQ = KEPOL42
      JTKEEP = JT(KFEET)
 	JT(KFEET) = 42
	diffuse type = 42

      if ( DINT .gt. 1.0e-07 ) then
      call calculate summaries of river flow
      call get summaries of river quality from the shots

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
      write(01,1482)valchars11,valchars10,funit
      write(34,1482)valchars11,valchars10,funit
      do J = 1, ndet
	if ( QTYPE (J) .ne. 4 ) then
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      write(34,1422)DNAME(J),valchars11,valchars10,UNITS(J)
      endif
      enddo
      call sort format 2 (DINT*FE(IQ,1),DINT*FE(IQ,2))
	write(01,3290)DINT,valchars11,valchars10,funit
 3290 format(33x,77('-')/
     &33x,'Total flow ADDED over',f6.1,' km:',4x,
     &'St.dev =',a10,3x,'Mean =',a10,1x,A4/33x,77('-'))
      write(34,3290)DINT,valchars11,valchars10,funit

      endif ! if ( nobigout .le. 0 )
	endif ! if ( ical13 .eq. 0 )
	endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) 

*     -----------------------------------------------------------Aggregated STWs

*     note whether the distribution is non-parametric ==========================
      if ( PDEF(IQ) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 4390
      enddo
      write(01,8940) flnamesmall(3,icod)
      write( *,8940) flnamesmall(3,icod)
      write(09,8940) flnamesmall(3,icod)
	call stop
 4390 if ( nobigout .le. 0 ) write(01,4104) flnamesmall(3,icod)
	endif ! if ( PDEF(IQ) .eq. 4 )

*     note whether the data are monthly ----------------------------------------
      if ( PDEF(IQ) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do i = 1, M8
      icod = i
	if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 4970
      enddo
      write(01,8920) FLMONTHsmall(3,icod)
      write( *,8920) FLMONTHsmall(3,icod)
      write(09,8920) FLMONTHsmall(3,icod)
	call stop
 4970 if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(3,icod)
      endif ! if ( PDEF(IQ) .eq. 5 )

      if ( PDEF(IQ) .eq. 8 ) then ! monthly structure
*     identify the file with the monthly structure data ------------------------
      do i = 1, M9
      icod = i
	if ( istruct ( 2, i, 1 ) .eq. IQ ) goto 5975
      enddo
      write(01,7920) FLSTRUCTsmall(3,icod)
      write( *,7920) FLSTRUCTsmall(3,icod)
      write(09,7920) FLSTRUCTsmall(3,icod)
	call stop
 5975 if ( nobigout .le. 0 ) write(01,3961) FLSTRUCTsmall(3,icod)
      endif ! monthly structure
      
*     -----------------------------------------------------------Aggregated STWs
      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( nobigout .le. 0 ) write(01,2894)IQ
 2894 format(33x,'Quality of diffuse aggregated STWs (42): ',
     &'Data Set:',I6/33x,77('-'))
      call write mean and standard deviation 33a
	endif ! aggregated STWs --------------------------------------------------

*     add in aggregated STWs (effluent type) ---------------------------------42
	call EFFLUENT DIFFUSE 42
      JT(KFEET) = JTKEEP
      diffuse type = 0
      
      call calculate summaries of river flow
      call get summaries of river quality from the shots
      
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3082)valchars11,valchars10,funit
      write(34,3082)valchars11,valchars10,funit
      do J = 1, Ndet
	if ( QTYPE (J) .ne. 4 ) then
	if ( C(J,1) .gt. 1.0 ) then
	write(01,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1022)DNAME(J),C(J,2),C(J,1),UNITS(J)
	else
	write(01,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	write(34,1922)DNAME(J),C(J,2),C(J,1),UNITS(J)
	endif
	endif
      enddo
	write(01,2992)
 2992 format(33x,77('='))
	write(34,2992)
	endif ! if ( ical13 .eq. 0 ) 
	endif ! if ( nobigout .le. 0 ) 
*     ------------------------------------------------------------------------02
      endif ! if ( DINT .gt. 1.0e-07 )
      if ( lake45 .eq. 1 ) KEPOL42 = 0
      endif ! if ( KEPOL42  .gt. 0 )

*     check for xero distance and for inflow from a lake -----------------------
      if ( lake45 .eq. 1 ) DINT = 0.0

      do jp =1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	endif
      enddo
      if ( IPUR   .gt. 0 ) call natural purification
      call calculate summaries of river flow
      do jp = 1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
      do is = 1, NS
      RQ(is) = CMS(JP,IS) ! store river quality --------------------------------
      enddo
      call calculate summaries of river quality (JP,RQ) 
      endif
      enddo

	JT (KFEET) = KEEPTYPE
      ifdiffuse = 0

      return
      end




*     natural purification -----------------------------------------------------
      subroutine natural purification
      include 'COM.FOR'
      dimension half life (mp10) 
	dimension oldshots (mp10, ms)

*     storage for the decay rates ----------------------------------------------
	real kay(mp10), halt(mp10), kay0(mp10)
      real KBOD

*     check for a zero distance and return if one is found ---------------------
      if ( DINT .lt. 1.0E-8 ) return

*     compute the loads prior to natural purification --------------------------
      call load calculation

*     store the starting values of loads ---------------------------------------
      do idet = 1, ndet
      do is = 1, ns
      old shots (idet, is ) = xshots ( idet,is )
      enddo
      enddo ! do idet = 1, ndet
      
      call load calculation

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
      if ( Rchkay(jdet,IREACH) .gt. 1.0E-10 ) 
     &              Kay0(jdet) = Rchkay(jdet,IREACH)
*     a negative reach value indicates conservative behaviour in this reach ----
      if ( Rchkay(jdet,IREACH) .lt. -0.9 ) Kay0 (Jdet) = 0.0
	endif
      enddo ! do JDET = 1, MP10

*     all the rate constants have been set -------------------------------------
*     prepare to compute the average velocity and the time-of-travel -----------
      AVEL = 0.0
	avel95 = 0.0
      ATIM = 0.0
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
      TIME = DINT / velo
	time95 = DINT / velo

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
      time95 = 24.0 * DINT / velo95

  998 continue

*     apply the rate constants ...
*     loop on all the shots ...

      do 9 IS = 1, NS
*     skip round zero flows ----------------------------------------------------
      if ( FMS(IS) .lt. 1.0E-08 ) goto 9

*     compute the time-of-travel -----------------------------------------------
*     the default is 33 kilometres per day -------------------------------------
      velo = 33.0
      TIME = DINT / velo

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
      time = DINT / velo
   18 continue

*     accumulate for the average velocity and average time-of-travel -----------
      AVEL = AVEL + velo
      ATIM = ATIM + time

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

      do 5008 JDET = 1, NDET
*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

*     ##########################################################################
*     isolate Biochemical Oxygen Demand (detype = 101) for special treatment ---
*     and data -----------------------------------------------------------------

      if ( Detype (jdet) .eq. 101 ) then ! ----------------------------------BOD

*     check the determinand is to be degraded ...
*     -----------------------------------------------------------------------Two
      if ( QTYPE (jdet) .eq. 2 ) then

*     adjust the decay rates for temperature -----------------------------------
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0

*     old, pre-decay, value of the concentration -------------------------------
      BOD0 = CMS(JDET,IS)

*     store the decay rate for the Dissolved Oxygen calculations ---------------
*     note that this excludes the adjustment for Biochemical Oxygen Demand -----
*     concentration ------------------------------------------------------------
      KBOD = kay (jdet)

*     the rate constant depends on the Biochemical Oxygen Demand ---------------
      kay (jdet) = kay (jdet) * ( 0.9090 + 0.01819 * BOD0 )

      EBOD = 1.0

      if ( kay (jdet) .gt. 0.0 ) then
      EBOD = exp (- kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif

*     scale the loads for the effect of decay ----------------------------------
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
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS(JDET,IS) = 0.0
	endif

*     save this halt value for the calculations for Dissolved Oxygen -----------
      HaltBOD = Halt (jdet)

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 101 )
      endif ! if ( QTYPE (JDET) .ne. 1 )
	endif ! if ( QTYPE (JDET) .ne. 4 )
 5008 continue   

*     ##########################################################################
*     now do ammonia ...
      do 5009 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

      if ( detype (jdet) .eq. 103 ) then

*     initialise the amount to be added to nitrate -----------------------------
      Ammonia loss = 0.0

      if ( QTYPE (jdet) .eq. 2 ) then

*     adjust the decay rate for temperature ------------------------------------
      Kay (jdet) = Kay0 (jdet) * 1.072 ** ( TD - 20.0)

*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
      AMM0 = CMS(JDET,IS)
      EAMM = 1.0

      if ( kay (jdet) .gt. 0.0 ) then
      EAMM = exp( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / kay (jdet)
      endif

      cnew ammonia = amin1 ( CMS(JDET,IS), 
     &(halt (jdet) + ( AMM0 - halt (jdet) ) * EAMM))

*     save the Ammonia loss to add to nitrate ----------------------------------
*     ammonia loss = amax1 (0.0, (CMS(jdet,is) - cnew ammonia ))
      old CMS = CMS(JDET,IS)
	if ( old CMS .gt. 1.0e-20 ) then
      CMS(JDET,IS) = amin1 ( CMS(JDET,IS), cnew ammonia )
      do ip = 1, nprop
*     concentrations from various types of feature -----------------------------
	LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
	enddo
	else
      do ip = 1, nprop
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS(JDET,IS) = 0.0
	endif

      endif
      endif

      endif
	endif
 5009 continue ! ammonia ------------------------------------------------------

*     now isolate dissolved oxygen for special treatment -----------------------
      do 5010 jdet = 1, ndet
      if ( QTYPE (JDET) .ne. 4 ) then
      if ( QTYPE (JDET) .ne. 1 ) then ! skip decay for conservative determinands
      if ( detype (jdet) .eq. 104 ) then ! identify dissolved oxygen -----------
      if ( QTYPE (jdet) .eq. 3 ) then ! check that reaeration is requested -----
*     adjust the decay rates for temperature -----------------------------------
      Kay (jdet) = Kay0 (jdet)  * 1.024 ** ( TD - 20.0)
*     check for potential underflow --------------------------------------------
      if ( ABS (Kay (jdet)) .lt. 1.0E-10 ) Kay (jdet) = 0.0
*     saturation value for Dissolved Oxygen ------------------------------------
      SAT = DO Saturation (TD)
*     extract the current value for Dissolved Oxygen ---------------------------
      DOXY = CMS(JDET,IS)
*     set the default for the reararation constant -----------------------------
      EAIR = 1.0
      if ( Kay (Jdet) .gt. 0.0 ) then
      EAIR = EXP ( - Kay (jdet) * TIME )
      half life (jdet) = half life (jdet) + 0.69 / Kay (jdet)
      endif
      JA=0
      JB=0
*     use the value for Biochemical Oxygen Demand decay from above -------------
      if ( ABS( ( Kay(jdet) - KBOD ) * TIME ) .lt. 1.0E-06 ) JB = 1
      if ( JB .eq. 0 ) QBOD = ( EBOD - EAIR ) / (Kay(jdet) - KBOD)
      if ( JB .eq. 1 ) QBOD = - TIME * EAIR
*     re-aeration towards the saturation value ---------------------------------
      Dzero = Amax1 ( 0.0, SAT - DOXY )
      Dzero = SAT - DOXY 
      if (Dzero .ge. 0.0 ) then
      BODEF = amax1 ( 0.0, ( BOD0 - HaltBOD ) )
      Dtee = amax1 ( 0.0, Kay(2) * QBOD * BODEF + dzero * EAIR )
      old CMS = CMS (JDET,IS)
	if ( old CMS .gt. 1.0e-5 ) then
      CMS (JDET,IS) = Amax1 ( 0.0, (SAT - Dtee))
      do ip = 1, nprop
*     adjust concentrations from various types of feature ----------------------
      if (ip .eq. 2 ) old LMS = LMS(ip,JDET,IS)
	LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
	enddo
	else
      do ip = 1, nprop
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS (JDET,IS) = 0.0
	endif
      endif ! if (Dzero .ge. 0.0 )
*     --------------------------------------------------------------------------
	endif ! if ( QTYPE (jdet) .eq. 3 ) determinand type 3 for Diss. Oxygen ---

*     now look at determinand type 2 for Dissolved Oxygen ======================
      if ( QTYPE (jdet) .eq. 2 ) then
*     use the Dissolved Oxygen Slot as ordinary degradable determinand adjust ==
*     the decay rate for temperature ===========================================
      Kay (jdet) = Kay0 (jdet) * 1.047 ** ( TD - 20.0)

*     check for potential underflow ============================================
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
*     concentrations from various types of feature =============================
	LMS(ip,JDET,IS) = LMS(ip,JDET,IS) * CMS(JDET,IS) / old CMS
	enddo
	else
      do ip = 1, nprop
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS (JDET,IS) = 0.0
	endif
      endif ! if ( QTYPE (jdet) .eq. 2 ) type 2 within dissolved oxygen ========

      endif ! if ( detype (jdet) .eq. 104 )
      endif ! if ( QTYPE (JDET) .ne. 1 )
      endif ! if ( QTYPE (JDET) .ne. 4 )
 5010 continue ! end of dissolved oxygen ---------------------------------------



*     Parent substance ...

      do 5228 JDET = 1, NDET
*     ---------------------------------------------------------------- Henry VII
*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then

*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

*     isolate parent substance (detype = 200 for special treatment -------------
      if ( Detype (jdet) .eq. 200 ) then
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
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS(JDET,IS) = 0.0
	endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 200 )

      endif ! if ( QTYPE (JDET) .ne. 1 )
	endif ! if ( QTYPE (JDET) .ne. 4 )

 5228 continue ! parent determinand       


*     ==========================================================================
*     Daughter substance ...
*     ==========================================================================
      do 5310 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then
      if ( Detype (jdet) .eq. 201 ) then

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
	LMS(ip,JDET,IS) = 0.0
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
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS (JDET,IS) = 0.0
	endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 201 )
      endif ! if ( QTYPE (JDET) .ne. 4 )

 5310 continue ! do 5310 jdet = 1, ndet



*     ==========================================================================
*     Nitrate ... add results for decay of ammonia -----------------------------
*     ==========================================================================
      do jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET)  .ne. 4 ) then

*     identify nitrate ------------------------------------------------- nitrate
      if ( Detype (jdet) .eq. 106 ) then

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
	LMS(ip,JDET,IS) = 0.0
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
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS (JDET,IS) = 0.0
	endif ! if ( old CMS .gt. 1.0e-20 )

      endif ! if ( QTYPE (jdet) .eq. 2 )
      endif ! if ( Detype (jdet) .eq. 106 )

      endif ! if ( QTYPE (JDET)  .ne. 4 )
      enddo ! do jdet = 1, ndet ... nitrate





*     all other degradable pollutants ------------------------------------------
*     adjust the Rate Constants for Temperature --------------------------------
      do 5011 jdet = 1, ndet

*     skip the decay process for any excluded determinands - QTYPE is 4 --------
      if ( QTYPE (JDET) .ne. 4 ) then

*     skip the decay for conservative determinands - QTYPE is 1 ----------------
      if ( QTYPE (JDET) .ne. 1 ) then

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
	LMS(ip,JDET,IS) = 0.0
	enddo
      CMS(JDET,IS) = 0.0
	endif

      endif
	endif
	endif
      endif
	endif
	endif
	endif

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

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(34,2000) dint
*     if ( nobigout .le. 0 ) write(27,2000) dint
 2000 format (110('-')/14x,
     &'Length of this stretch = ',f7.2,' kilometres (km)')

      if ( dint .gt. 0.000001) then
      if ( nobigout .le. 0 ) write(34,2300) avel, atim
*     if ( nobigout .le. 0 ) write(27,2300) avel, atim
 2300 format (
     &'            Average velocity of flow = ',f7.2,' km/day',
     &'      ... Average time of travel = ',f7.2,' hours  ')

      if ( nobigout .le. 0 ) write(34,2030) velo95, 24.0*time95
*     if ( nobigout .le. 0 ) write(27,2030) velo95, 24.0*time95
 2030 format (
     &'  Velocity at 95-percentile low flow = ',f7.2,' km/day ',
     &'.. Corresponding time of travel = ',f7.2,' hours  ')
	endif
      endif

*     if ( ical .ge. 3 ) write(34,2111)
 2111 format(110('-')/
     &'Details of Gap Filling for flows follow later ....'/
     &110('-'))

      if ( dint .gt. 0.000001 ) then      
      if ( ical13 .eq. 0 ) then
      if ( ipur .ne. 0) then
	if ( nobigout .le. 0 ) write(34,4542)
 4542 format(110('-')/
     &'Changes in river quality added by Natural Purification ...'/
     &110('-'))
      endif
*     --------------------------------------------------------------------------
      do idet = 1,ndet
      if ( QTYPE (idet) .ne. 4 ) then
      if ( half life (idet)  .gt. 0.0001 ) then
	redu = 100.0 * (1.0 - exp ( -kay0(idet) * atim/24.0 ))
      if ( nobigout .le. 0 ) write(34,2200)  Dname (idet), 
     &half life (idet), redu
 2200 format ('Average half-life for ',a11,' =',f6.1,' hours',
     &' ... average loss =',f7.2,' per cent')
      endif
      endif
      enddo
*     --------------------------------------------------------------------------
      endif
	endif

      if ( ipur .ne. 0) then
      if ( ical13 .eq. 0 ) then
	if ( nobigout .le. 0 ) write(34,4562)
 4562 format(110('-'))
	endif
      endif

*     calculate loads and the summary statistics of load -----------------------
      call load calculation

      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      do J13 = 1, N13
	NSM (idet,J13) = 0
	xupload (idet,J13) = 0.0
	xdownload (idet,J13) = 0.0
	enddo
*     loop on the shots --------------------------------------------------------
      do is = 1, ns
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
	enddo
*     calcuate the average loads -----------------------------------------------
      xdownload (idet,I13) = xdownload (idet,I13) / float (ns)
      xupload (idet,I13) = xupload (idet,I13) / float (ns)
	xnet = xupload (idet,i13) + xdownload (idet,i13) 

      if ( xnet .gt. 0.00001) then
	xupload (idet,i13) = xnet
      xdownload (idet,i13) = 0.0
	else
	xdownload (idet,i13) = xnet
	xupload (idet,i13) = 0.0
      endif

      TNLOADDN2 (idet,I13) = TNLOADDN2 (idet,I13) + xdownload (idet,I13)
      TNLOADDN1 (idet,I13) = TNLOADDN1 (idet,I13) + xdownload (idet,I13)
      if ( munthly structure .eq. 1 ) then ! fill monthly loads
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
	enddo


      call add up all the loads


*     calculate the effect of losses on the accumulated loads ------------------
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      kprune det = idet
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      prune load (J1) = 1.0
      if ( abs ( TGLODE2(idet,J1 ) ) .gt. 0.000000001 ) then
      if ( xdownload (idet,J1) .lt. 0.0 ) then
      if ( TGLODE2(idet,J1) + xdownload (idet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(idet,J1) + xdownload (idet,J1)) 
     &                /  TGLODE2(idet,J1)
      endif
	endif ! if ( xdownload (idet,J1) .lt. 0.0 )
      endif ! if ( abs ( TGLODE2(idet,J1 )
	enddo ! do J1 = 1, nx

*     trim back the loads for losses on natural purification  ------------------
      call scale loads after losses
	endif ! if ( QTYPE (idet) .ne. 4 )
	enddo ! do idet = 1, ndet

      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
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



*     trim back the loads for natural purification -----------------------------
      subroutine scale loads after losses
      include 'COM.FOR'

*     trim annual loads from individual works ----------------------------------
      if ( kount works .gt. 0 .and. kprune det .gt. 0 ) then
	do iworks = 1, kount works
      TELOADAV(iworks,kprune det) = ! prune the load
     &TELOADAV(iworks,kprune det) * prune load (i13)
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(iworks,kprune det,is) = ! prune the load ! 3333333333333333333
     &TELOADshots(iworks,kprune det,is) * prune load (i13) ! 3333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333

      enddo ! do iworks = 1, kount works
      endif ! if ( kount works .gt. 0 .and. kprune det .gt. 0 )
      
*     trim annual loads from individual waters ---------------------------------
      if ( kount bodies .gt. 0 ) then
	do ibodies = 1, kount bodies
*     annual loads (i13) from upstream sub-catchments --------------------------
      TWLOADS(ibodies,kprune det,i13) = TWLOADS(ibodies,kprune det,i13) 
     &                                * prune load (i13)
      do ip = 1, nprop
*     breakdown of annual loads (i13) from upstream sub-catchments -------------
      TWLOADSapp(ibodies,kprune det,i13,ip) = 
     &TWLOADSapp(ibodies,kprune det,i13,ip) * prune load (i13)
      
      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      TDLOADshots(ibodies,kprune det,is,ip) = ! prune the load ! 555555555555555
     &TDLOADshots(ibodies,kprune det,is,ip) * prune load (i13) ! 555555555555555
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555
      
      enddo ! do ip = 1, nprop

      if ( n148 .eq. 1 ) then ! 333333333333333333333333333355555555555555555555
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      do ip = 1, nprop ! 5555555555555555555555555555555555555555555555555555555
      TDLOADshots(ibodies,kprune det,is,ip) = ! prune the load ! 555555555555555
     &TDLOADshots(ibodies,kprune det,is,ip) * prune load (i13) ! 555555555555555
      enddo ! 555555555555555555555555555555555555555555555555555555555555555555
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333355555555555555555

	enddo 
	endif

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      if ( prune load (J1) .lt. 1.0 ) then
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
*     prune load introduced to reaches by clean diffuse sources ----------------
      if ( TDDLOAD2(kprune det,J1) .gt. 0.0 ) then
      TDDLOAD2(kprune det,J1) = TDDLOAD2(kprune det,J1) 
     &                        * prune load (J1)
  	endif
*     prune load introduced by Gap Filling for river flows --------------------- 
      if ( TILOADUP2(kprune det,J1) .gt. 0.0 ) then
      TILOADUP2(kprune det,J1) = TILOADUP2(kprune det,J1)
     &                         * prune load (J1)
  	endif
*     prune load introduced by Gap Filling for river quality -------------------
      if ( TALOADUP2(kprune det,J1) .gt. 0.0 ) then
      TALOADUP2(kprune det,J1) = TALOADUP2(kprune det,J1) 
     &                         * prune load (J1)
  	endif
*     ==========================================================================
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
*     prune load from diffuse features (discharge flow type) -------------------
      if ( T15LOAD2(kprune det,J1) .gt. 0.0 ) then
      T42LOAD2(kprune det,J1) = T42LOAD2(kprune det,J1)
     &                        * prune load (J1)
  	endif
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
      if ( T46LOAD2(kprune det,J1) .gt. 0.0 ) then
      T46LOAD2(kprune det,J1) = T46LOAD2(kprune det,J1)
     &                        * prune load (J1)
  	endif
      if ( T48LOAD2(kprune det,J1) .gt. 0.0 ) then
      T48LOAD2(kprune det,J1) = T48LOAD2(kprune det,J1)
     &                        * prune load (J1)
  	endif
      if ( T37LOAD2(kprune det,J1) .gt. 0.0 ) then
      T37LOAD2(kprune det,J1) = T37LOAD2(kprune det,J1)
     &                        * prune load (J1)
	endif
      if ( T40LOAD2(kprune det,J1) .gt. 0.0 ) then
      T40LOAD2(kprune det,J1) = T40LOAD2(kprune det,J1)
     &                        * prune load (J1)
  	endif
*     ==========================================================================
*     prune load from those added by natural purification ----------------------
      if ( TNLOADUP2(kprune det,J1) .gt. 0.0 ) then
      TNLOADUP2(kprune det,J1) = TNLOADUP2(kprune det,J1)
     &                         * prune load (J1)
  	endif
	endif ! if ( prune load (J1) .lt. 1.0 )
	enddo ! do J1 = 1, nx

      return
	end


      subroutine sort format 1 (v1)
      include 'COM.FOR'
      character *10 valfetch
      valfetch = '       - '
      valchars10 = '       - '
      call fetch ( v1, valfetch)
      valchars10 = valfetch
      return
      end
      
      subroutine sort format 2 (v1,v2)
      include 'COM.FOR'
      character *10 valfetch
      valfetch = '       - '
      valchars10 = '       - '
      valchars11 = '       - '
      call fetch ( v1, valfetch)
      valchars10 = valfetch
      call fetch ( v2, valfetch)
      valchars11 = valfetch
      return
      end

      subroutine sort format 3 (v1,v2,v3)
      include 'COM.FOR'
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

*     greater than 1,000,000 ---------------------------------------------------
      if (fff .gt. 999999.45 ) then
      write(textx,1) fff
    1 format(1pe10.3)
      goto 99
	endif
*     less than 0.000000002 ----------------------------------------------------
      if (fff .lt. 0.000000002) then
      write(textx,2) int (fff)
      goto 99
	endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (fff .gt. 9999.45 ) then
      write(textx,2) int (fff)
    2 format(i10)
      goto 99
	endif
*     less than 10,000 and greater than 100 ------------------------------------
      if (fff .gt. 9.99) then
      write(textx,3) fff
    3 format(F10.1)
      goto 99
	endif
*     less than 100 and greater than 1 -----------------------------------------
      if (fff .gt. 0.999) then       
      write(textx,4) fff
    4 format(F10.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if (fff .gt. 0.0999) then       
      write(textx,5) fff
    5 format(6x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
	if (fff .gt. 0.00999) then       
      write(textx,6) fff
    6 format(5x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
	if (fff .gt. 0.000999) then               
      write(textx,7) fff
    7 format(4x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if (fff .gt. 0.00000999) then               
      write(textx,8) fff
    8 format(3x,f7.6)
      goto 99
      endif
*     less than 0.00001 and greater than 0.00001 ---------------------------------
	if (fff .gt. 0.000000999) then               
      write(textx,9) fff
    9 format(2x,f8.7)
      goto 99
      endif
*     less than 0.000001 and greater than 0.000001 ---------------------------------
	if (fff .gt. 0.0000000999) then               
      write(textx,10) fff
   10 format(1x,f9.8)
      goto 99
      endif
*     less than 0.000001 and greater than 0.000001 ---------------------------------
*     if (fff .gt. 0.00000000999) then               
*     write(textx,11) fff
*  11 format(f10.9)
*     goto 99
*     endif
*     zero to 1.e-8 ------------------------------------------------------------
      write(textx,2) int (fff)
   19 format(4x,1pe6.0)
   99 continue 

      return
      end