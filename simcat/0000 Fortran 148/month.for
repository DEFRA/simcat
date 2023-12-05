*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File Name: MONTH.FOR    3654
*     --------------------------------------------------------------------------
      subroutine write out monthly river quality (iplace)
*     iplace 1 ... start of reach
*     iplace 3 ... end of reach
*     iplace 2 ... at feature
*     iplace 4 ... upstream of discharge
*     iplace 5 ... downstream of tributary
*     iplace 6 ... downstream of discharge
      include 'COM.FOR'
	dimension XL(13),XU(13)
      dimension XADD(13)
	character *10 Tenchars (13)

*     if ( munthly structure .eq. 0 ) return
	jxtype = 0
	zero check = 0.0
	unamex = "nothing"
	rnamex = "nothing"

      do i = 1, n13
      Tenchars (i) = ' .........'
      enddo

      call calculate summaries of river flow
	call get summaries of river quality from the shots
      call get summaries of loads

      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))
      call load calculation
      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))

*     compute the extra values for partitioned metals ----------------------------
   	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid
     &(3,J,CP2(1,J),CP2(2,J),CP2(3,J),CP2(4,J),CP2(5,J),CP2(6,J),
     &CP2(7,J),CP2(8,J),CP2(9,J),CP2(10,J),CP2(11,J),CP2(12,J))
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J),
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (2,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J),
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
	endif ! dissolved and solid metal
      endif
      enddo
*     --------------------------------------------------------------------------

*     calculate the flow statistics --------------------------------------------
      t95 = errx ( 0.95 )
*     loop on the months -------------------------------------------------------
      do 36 imon = 1, 12
      GEFM = 0.0
      GEFS = 0.0
*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, fmon(1,imon) )
*     check for a zero mean ----------------------------------------------------
      if (A .lt. 1.0E-08) fmon(2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (fmon(2,imon) .lt. 1.0E-08) goto 36
*     set the standard deviation -----------------------------------------------
      S = fmon(2,imon)
*     calculate the 95-percentile low flow -------------------------------------
      GEFM = ALOG((A*A)/SQRoot(1057,A*A+S*S))
	TRAK = ALOG(1.+(S*S)/(A*A))
	if ( TRAK .gt. 1.0e-20) then
      GEFS = SQRoot(105657,TRAK)
	endif
      fmon(2,imon) = EXP(GEFM-t95*GEFS)
   36 continue
*     --------------------------------------------------------------------------
*     calculate summary statistics for temperature -----------------------------
      do 32 imon = 1, 12
*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, tmon(1,imon) )
      if (A .lt. 1.0E-08) tmon(2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (tmon(2,imon) .lt. 1.0E-08) goto 32
*     set the standard deviation -----------------------------------------------
      S = tmon(2,imon)
   32 continue
*     --------------------------------------------------------------------------
*     calculate summary statistics for suspended solids ------------------------
      do 52 imon = 1, 12
*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, smon(1,imon) )
      if (A .lt. 1.0E-08) smon(2,imon) = 0.0
*     set the standard deviation -----------------------------------------------
      if (smon(2,imon) .lt. 1.0E-08) goto 52
      S = smon(2,imon)
   52 continue
*     --------------------------------------------------------------------------

*     set the values for printing out the type of feature etc ------------------
	unamex = uname(feeture)
      jxtype = JT(feeture)
	if ( JT(feeture) .eq. 10 ) then
	unamex = 'Start of Reach'
	jxtype = 111
	endif
      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
	jxtype = 999
      endif
	rnamex = rname (IREACH)
	if ( iplace .eq. 4 ) rnamex = 'Upstream of works'
*     --------------------------------------------------------------------------

*     skip this routine if doing gap filling -----------------------------------
      if ( ical13 .eq. 1 ) return

      call write out monthly flows and temperature (iplace)

      do 6612 jper = 1, ndet ! loop on determinands ----------------------------
      if ( QTYPE (jper) .ne. 4) then ! check for excluded determinands ---------
      do imon = 1, 12
      XL(imon) = 0.0
      XU(imon) = 0.0
      enddo
      XL(13) = CL(2,jper) ! retrieve the values of the confidence limits -------
      XU(13) = CL(3,jper) ! retrieve the values of the confidence limits -------

      print check = 0.0 ! initialise values used for checks --------------------
      cprint = 2.0 ! initialise the value of the biggest monthly value ---------
      do 42 imon = 1, 12 ! loop on months --------------------------------------
      cprint = amax1 (cprint, cmon(jper,1,imon))
      A = amax1 ( 0.0, cmon(jper,1,imon) ) ! mean ------------------------------
      XL(imon) = A
      XU(imon) = A
      print check = print check + A
*     check for a zero mean ----------------------------------------------------
      if ( A .lt. 1.0E-08 ) cmon(jper,2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if ( cmon(jper,2,imon ) .lt. 1.0E-08) goto 42
*     set the standard deviation -----------------------------------------------
      S = cmon(jper,2,imon)
*     calculate confidence limits ----------------------------------------------
      qqq = amax1 ( 0.05, qualn(jper) / 12.0 )
      SEM = S/SQRoot3(107280,qqq)
      XL(imon) = amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon) = amax1 (A,(A+t95*SEM))
   42 continue
      print check = print check / 12.0

*     write out tables of monthly concentrations -------------------------------
      if ( nobigout .le. 0 ) then
      if ( cprint .gt. 1.0 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),cmon(jper,1,i))
	enddo
	endif
      call set format for printout(Tenchars (13),C(jper,1))
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Means for ',a11,9x,13a10)
      
      if ( C(jper,1) .gt. 1.0e-8 ) then
    
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),cmon(jper,2,i))
	enddo
	endif
      call set format for printout(Tenchars (13),C(jper,2))
      write(100+jper,1001)(Tenchars(i),i=1,13)
 1001 format('Standard deviation            ',13a10)
      
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 13
      call set format for printout(Tenchars(i),XL(i))
	enddo
	endif
      write(100+jper,1005)(Tenchars(i),i=1,13)
 1005 format('Lower confidence limit on mean',13a10)
      
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 13 
      call set format for printout(Tenchars(i),XU(i))
	enddo
	endif
      write(100+jper,1006)(Tenchars(i),i=1,13)
 1006 format('Upper confidence limit on mean',13a10)
      call write line (jper)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      
      endif ! if ( cprint .gt. 1.0 )
      endif
*     ----------------------------- written out tables of monthly concentrations
     
*     write out comma-separated monthly concentrations -------------------------
      write(110+jper,1300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(cmon(jper,1,i),i=1,12),C(jper,1),jxtype
 1300 format(a40,',',a40,',',a20,',','Means for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,1301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(cmon(jper,2,i),i=1,12),C(jper,2),jxtype
 1301 format(a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1305)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jxtype
 1305 format(a40,',',a40,',',a20,',','Lower confidence limit for ',','
     &a11,13(',',1pe12.4),',',i4)
      write(110+jper,1306)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jxtype
 1306 format(a40,',',a40,',',a20,',','Upper confidence limit for ',',',
     &a11,13(',',1pe12.4),',',i4)
*     ----------------------- written out comma-separated monthly concentrations
      
      if ( Detype (jper) .ge. 900 ) then ! dissolved and solid metal -----------
	call write out monthly partitions two (iplace, jper)
	call write out monthly partitions one (iplace, jper)
      endif ! dissolved and solid metal ----------------------------------------

      call write out the monthly apportionment (jper)
      call write out comma separated monthly apportionment (jper)
      
*     --------------- finished tables of % contribution of monthly effluent load
*     comma-separated % contribution of monthly effluent load ------------------

 	do 3056 ip = 1, 16
      if ( ip .eq. 1 ) then
      write(110+jper,1296)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1296 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (3 12 5 and 39)',',',a37,
     &13(',',1pe12.4),',',i4,', 3,12, 5,39')
	endif
 	if ( ip .eq. 2 ) then
      write(110+jper,1297)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1297 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (3)',',',a37,
     &13(',',1pe12.4),',',i4,', 3')
	endif
 	if ( ip .eq. 3 ) then
      write(110+jper,1298)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1298 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (12)',',',a37,
     &13(',',1pe12.4),',',i4,',12')
	endif
 	if ( ip .eq. 4 ) then
      write(110+jper,1200)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1200 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (5)',',',a37,
     &13(',',1pe12.4),',',i4,', 5')
	endif
 	if ( ip .eq. 5 ) then
      write(110+jper,1201)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1201 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (39)',',',a37,
     &13(',',1pe12.4),',',i4,',39')
	endif
 	if ( ip .eq. 6 ) then
      write(110+jper,1202)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1202 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (25)',',',a37,
     &13(',',1pe12.4),',',i4,',25')
	endif
 	if ( ip .eq. 7 ) then
      write(110+jper,1203)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1203 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (27)',',',a37,
     &13(',',1pe12.4),',',i4,',27')
	endif
 	if ( ip .eq. 8 ) then
      write(110+jper,1204)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1204 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (29)',',',a37,
     &13(',',1pe12.4),',',i4,',29')
	endif
 	if ( ip .eq. 9 ) then
      write(110+jper,1205)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1205 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (31)',',',a37,
     &13(',',1pe12.4),',',i4,',31')
	endif
 	if ( ip .eq. 10 ) then
      write(110+jper,1206)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1206 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (33)',',',a37,
     &13(',',1pe12.4),',',i4,',33')
	endif
 	if ( ip .eq. 11 ) then ! from natural background (35) --------------------
      write(110+jper,1207)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1207 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (35)',',',a37,
     &13(',',1pe12.4),',',i4,',35')
	endif
 	if ( ip .eq. 12 ) then
      write(110+jper,1208)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1208 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (37)',',',a37,
     &13(',',1pe12.4),',',i4,',37')
	endif
 	if ( ip .eq. 13 ) then
      write(110+jper,1209)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1209 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (40)',',',a37,
     &13(',',1pe12.4),',',i4,',40')
	endif
 	if ( ip .eq. 14 ) then
      write(110+jper,1210)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1210 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (42)',',',a37,
     &13(',',1pe12.4),',',i4,',42')
	endif
 	if ( ip .eq. 15 ) then ! from diffuse mines (46) -------------------------
      write(110+jper,1211)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1211 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (46)',',',a37,
     &13(',',1pe12.4),',',i4,',46')
	endif ! from diffuse mines (46) ------------------------------------------
 	if ( ip .eq. 16 ) then
      write(110+jper,1212)GIScode(feeture),unamex,rnamex,nameprop(ip),
     &(llmon2(ip,jper,1,i),i=1,13),jxtype
 1212 format(a40,',',a40,',',a20,',','% monthly breakdown ',
     &'of contribution to annual mean (48)',',',a37,
     &13(',',1pe12.4),',',i4,',48')
      endif
 3056 continue
*     finished comma-separated % contribution of monthly effluent load ---------

*     write(100+jper,3096)(xadd(i),i=1,13)
 3096 format(150('-'),2x,8('-')/'Sum of percentage means',7x,13f10.2)

*     calculate days exceeding annual mean etc ---------------------------------
      if ( QTYPE (jper) .ne. 4 ) then
      do im = 1, n13
      exceedences50 (jper, im) = 0.0
      exceedences90 (jper, im) = 0.0
      exceedences95 (jper, im) = 0.0
	enddo
*     non-partitioned (ordinary) chemicals) ------------------------------------
      if ( detype (jper) .lt. 900 ) then

      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do im = j1, n13

*     ==========================================================================
	if ( im .lt. 13 ) then  
      if ( imonth .eq. im ) then	 
	if ( CMS(jper,is) .gt. C(jper,1) + 1.0e-09 ) then
      exceedences50 (jper, im) = exceedences50 (jper, im) + 1.0
	endif
	if ( CMS(jper,is) .gt. C(jper,4) + 1.0e-09 ) then
      exceedences90 (jper, im) = exceedences90 (jper, im) + 1.0
	endif
	if ( CMS(jper,is) .gt. C(jper,3) + 1.0e-09 ) then
      exceedences95 (jper, im) = exceedences95 (jper, im) + 1.0
	endif
	endif
	else
	if ( CMS(jper,is) .gt. C(jper,1) + 1.0e-09 ) then
      exceedences50 (jper, im) = exceedences50 (jper, im) + 1.0
	endif
	if ( CMS(jper,is) .gt. C(jper,4) + 1.0e-09 ) then
      exceedences90 (jper, im) = exceedences90 (jper, im) + 1.0
	endif
	if ( CMS(jper,is) .gt. C(jper,3) + 1.0e-09 ) then
      exceedences95 (jper, im) = exceedences95 (jper, im) + 1.0
	endif
	endif

	enddo
	enddo
      else ! partitioned chemicals
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      do im = 1, 13 
	if ( im .lt. 13 ) then  
      if ( imonth .eq. im ) then	 
	if ( PMS1(is) .gt. C(jper,1) + 1.0e-09 ) then
      exceedences50 (jper, im) = exceedences50 (jper, im) + 1.0
	endif
	if ( PMS1(is) .gt. C(jper,4) + 1.0e-09 ) then
      exceedences90 (jper, im) = exceedences90 (jper, im) + 1.0
	endif
	if ( PMS1(is) .gt. C(jper,3) + 1.0e-09 ) then
      exceedences95 (jper, im) = exceedences95 (jper, im) + 1.0
	endif
	endif
	else
	if ( PMS1(is) .gt. C(jper,1) + 1.0e-09 ) then
      exceedences50 (jper, im) = exceedences50 (jper, im) + 1.0
	endif
	if ( PMS1(is) .gt. C(jper,4) + 1.0e-09 ) then
      exceedences90 (jper, im) = exceedences90 (jper, im) + 1.0
	endif
	if ( PMS1(is) .gt. C(jper,3) + 1.0e-09 ) then
      exceedences95 (jper, im) = exceedences95 (jper, im) + 1.0
      endif
	endif
	enddo
	enddo
      endif ! partitioned chemicals
*     ==========================================================================
*     --------------------------------------------------------------------------
      do im = 1, 12
	if ( NSM(jper, im+1) .gt. 0 ) then
      exceedences50 (jper,im) = 100.0 * exceedences50 (jper,im)
     &                                / NSM(jper, im+1)
      exceedences90 (jper,im) = 100.0 * exceedences90 (jper,im)
     &                                / NSM(jper, im+1)
      exceedences95 (jper,im) = 100.0 * exceedences95 (jper,im)
     &                                / NSM(jper, im+1)
	endif
	enddo
      exceedences50 (jper,13) = 100.0 * exceedences50 (jper,13)/NS
      exceedences90 (jper,13) = 100.0 * exceedences90 (jper,13)/NS
      exceedences95 (jper,13) = 100.0 * exceedences95 (jper,13)/NS
*     --------------------------------------------------------------------------

*     write out tables of exceedence -------------------------------------------
      if ( nobigout .le. 0 ) then
      if ( exceedences50(jper,13) .gt. 1.0e-12 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences50 (jper,i))
	enddo
	endif
      call set format for printout(Tenchars(13),exceedences50(jper,13))
      write(100+jper,4001)(Tenchars (i), i = 1,13)
 4001 format(150('-'),2x,8('-')/'% days exceeding annual mean  ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences90 (jper,i))
	enddo
	endif ! set up print out
      call set format for printout(Tenchars(13),exceedences90(jper,13))
      write(100+jper,4101)(Tenchars (i), i = 1,13)
 4101 format('% days exceeding annual Q90   ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences95 (jper,i))
	enddo
	endif
      call set format for printout(Tenchars(13),exceedences95(jper,13))
      write(100+jper,4201)(Tenchars (i), i = 1,13)
 4201 format('% days exceeding annual Q95   ',13a10)
*     call write line (jper)
      endif ! if ( exceedences50(jper,13) .gt. 1.0e-12 )
      endif
*     --------------------------------------------------------------------------
      endif ! if ( QTYPE (jper) .ne. 4 )

*     write out comma-separated tables of exceedence ---------------------------
      write(110+jper,4401)GIScode(feeture),unamex,rnamex,
     &dname(jper),(exceedences50 (jper,im), im = 1,13),jxtype
 4401 format(a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual mean',',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,4402)GIScode(feeture),unamex,rnamex,
     &dname(jper),(exceedences90 (jper,im), im = 1,13),jxtype
 4402 format(a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual Q90',',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,4403)GIScode(feeture),unamex,rnamex,
     &dname(jper),(exceedences95 (jper,im), im = 1,13),jxtype
 4403 format(a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual Q95',',',a11,13(',',1pe12.4),',',i4)


*     prepare to write out loads -----------------------------------------------     
*     loop on months -----------------------------------------------------------
      if ( QTYPE (jper) .ne. 4 ) then
      do 33 imon = 1, n13
*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, lmon(jper,1,imon) )
      XL(imon) = A
      XU(imon) = A
*     check for zero mean ------------------------------------------------------
      if (A .lt. 1.0E-08) lmon(jper,2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (lmon(jper,2,imon) .lt. 1.0E-08) goto 33
*     set the standard deviation -----------------------------------------------
      S = lmon(jper,2,imon)
*     calculate confidence limits ----------------------------------------------
      qqq = amax1 ( 0.05, qualn(jper) / 12.0 )
      if ( imon .eq. 13 ) qqq = qualn(jper)
      SEM=S/SQRoot3(107281,qqq)
      XL(imon)=amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon)=amax1 (A,(A+t95*SEM))
   33 continue
*     --------------------------------------------------------------------------


      
   
      
*     write out tables of monthly loads ----------------------------------------
      if ( nobigout .le. 0 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),lmon(jper,1,i))
	enddo
	endif
      call set format for printout(Tenchars (13),lmon(jper,1,13))
      write(100+jper,2004)dname(jper),(Tenchars (i),i=1,13)
 2004 format(150('-'),2x,8('-')/'Mean loads for ',a11,4x,13a10)
      if ( lmon(jper,1,i) .gt. 1.0 e-12 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),lmon(jper,2,i))
      enddo
	endif
      call set format for printout(Tenchars (13),lmon(jper,2,13))
      write(100+jper,2001)(Tenchars (i),i=1,13)
 2001 format('Standard deviation            ',13A10)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),XL(i))
	enddo
	endif
      call set format for printout(Tenchars (13),XL(13))
      write(100+jper,2005)(Tenchars (i),i=1,13)
 2005 format('Lower confidence limit        ',13a10)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),XU(i))
	enddo
	endif
      call set format for printout(Tenchars (13),XU(13))
      write(100+jper,2006)(Tenchars (i),i=1,13)
 2006 format('Upper confidence limit        ',13A10)
      call write line (jper)
      endif ! if ( lmon(jper,1,i) .gt. 1.0 e-12 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE (jper) .ne. 4 )

*     write out comma-separated monthly loads ----------------------------------
      write(110+jper,2404)GIScode(feeture),unamex,
     &rnamex,dname(jper),(lmon(jper,1,i),i=1,13),jxtype
 2404 format(a40,',',a40,',',a20,',','Mean loads for',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,2401)GIScode(feeture),unamex,
     &rnamex,dname(jper),(lmon(jper,1,i),i=1,13),jxtype
 2401 format(a40,',',a40,',',a20,',','Standard deviations of loads',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,2501)GIScode(feeture),unamex,
     &rnamex,dname(jper),(XL(i),i=1,13),jxtype
 2501 format(a40,',',a40,',',a20,',','Lower limit on load',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,2506)GIScode(feeture),unamex,
     &rnamex,dname(jper),(XU(i),i=1,13),jxtype
 2506 format(a40,',',a40,',',a20,',','Upper limit on load',
     &',',a11,13(',',1pe12.4),',',i4)
*     finished comma-separated monthly loads -----------------------------------
      
      call write the monthly data in tables (jper)
      call write the monthly data in comma separated form (jper,iplace)
*     --------------------------------------------------------------------------
      endif
 6612 continue
*     end of loop on determinands ==========================================6612

      return
      end





*     --------------------------------------------------------------------------
      subroutine write out monthly partitions one (iplace, jper)
      include 'COM.FOR'
	dimension XL(13),XU(13)
      character *10 Tenchars(13)

*     loop on months -----------------------------------------------------------
      t95 = errx ( 0.95 )
      print check = 0.0
      cprint = 2.0
      do 42 imon = 1, 13
      cprint = amax1 (cprint, p1mon(jper,1,imon))

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, p1mon(jper,1,imon) )
      XL(imon) = A
      XU(imon) = A
      print check = print check + A
*     check for zero mean ------------------------------------------------------
      if (A .lt. 1.0E-08) p1mon(jper,2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (p1mon(jper,2,imon) .lt. 1.0E-08) goto 42
*     set the standard deviation -----------------------------------------------
      S = p1mon(jper,2,imon)
*     calculate confidence limits ----------------------------------------------
      qqq = amax1 ( 0.05, qualn(jper) / 12.0 )
      if ( imon .eq. 13 ) qqq = qualn(jper)
      SEM=S/SQRoot(107282,qqq)
      XL(imon)=amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon)=amax1 (A,(A+t95*SEM))
   42 continue
      print check = print check / 12.0
*     --------------------------------------------------------------------------


*     --------------------------------------------------------------------------
	if ( iplace .ne. 4 ) then
	rnamex = rname (IREACH)
	else
	rnamex = 'Upstream of works'
	endif
*     --------------------------------------------------------------------------
	if ( JT(feeture) .ne. 10 ) then
	unamex = uname(feeture)
	else
	unamex = 'Start of Reach'
      endif
*     --------------------------------------------------------------------------
 

*     --------------------------------------------------------------------------
      if ( nobigout .le. 0 ) then
      if ( cprint .gt. 1.0 ) then
      call set format for printout(Tenchars(13),cpart1(jper,1))
      do i = 1, 12
      call set format for printout(Tenchars(i),p1mon(jper,1,i))
      enddo
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Total for ',a11,9x,13a10)
*     if ( cpart1(jper,1) .gt. 1.0e-12 ) then
      call set format for printout(Tenchars(13),cpart1(jper,2))
      do i = 1, 12
      call set format for printout(Tenchars(i),p1mon(jper,2,i))
      enddo
      if ( print check .gt. 1.0e-08 ) write(100+jper,1001)
     &(Tenchars(i),i=1,13)
 1001 format('Standard deviation            ',13a10)
      do i = 1, 13
      call set format for printout(Tenchars(i),XL(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1005)
     &(Tenchars(i),i=1,13)
 1005 format('Lower limit on total          ',13a10)
      do i = 1, 13
      call set format for printout(Tenchars(i),XU(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1006)
     &(Tenchars(i),i=1,13)
 1006 format('Upper limit on total          ',13a10)
      call write line (jper)
*     endif ! if ( cpart1(jper,1) .gt. 1.0e-12 )
      else
      call set format for printout(Tenchars(13),cpart1(jper,1))
      do i = 1, 12
      call set format for printout(Tenchars(i),p1mon(jper,1,i))
      enddo
      write(100+jper,1880)dname(jper),(Tenchars(i),i=1,13)
 1880 format('Total for ',a11,9x,13a10)
*     if ( cpart1(jper,1) .gt. 1.0e-12 ) then
      call set format for printout(Tenchars(13),cpart1(jper,2))
      do i = 1, 12
      call set format for printout(Tenchars(i),p1mon(jper,2,i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1881)
     &(Tenchars(i),i=1,13)
 1881 format('Standard deviation            ',13a10)
      do i = 1, 13
      call set format for printout(Tenchars(i),XL(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1805)
     &(Tenchars(i),i=1,13)
 1805 format('Lower limit on dissolved      ',13a10)
      do i = 1, 13
      call set format for printout(Tenchars (i),XU(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1806)
     &(Tenchars(i),i=1,13)
 1806 format('Upper limit on dissolved      ',13a10)
      call write line (jper)
*     endif ! if ( cpart1(jper,1) .gt. 1.0e-12 )
      endif ! if ( cprint .gt. 1.0 )

      if ( iplace .ne. 3 ) then
      write(110+jper,1300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 1300 format(a40,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,1301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 1301 format(a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1305)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1305 format(a40,',',a40,',',a20,',','Lower limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1306)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1306 format(a40,',',a40,',',a20,',','Upper limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
	endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(110+jper,1400)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 1400 format(a40,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,1401)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 1401 format(a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1405)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1405 format(a40,',',a40,',',a20,',','Lower limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1406)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1406 format(a40,',',a40,',',a20,',','Upper limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
	endif
	endif
*     --------------------------------------------------------------------------

      return
      end



*     --------------------------------------------------------------------------
      subroutine write out monthly partitions two (iplace, jper)
      include 'COM.FOR'
	dimension XL(13),XU(13)
      character *10 Tenchars(MP10)

*     loop on months -----------------------------------------------------------
      t95 = errx ( 0.95 )
      print check = 0.0
      cprint = 2.0
      do 42 imon = 1, 13
      cprint = amax1 (cprint, P2mon(jper,1,imon))

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, P2mon(jper,1,imon) )
      XL(imon) = A
      XU(imon) = A
      print check = print check + A
*     check for zero mean ------------------------------------------------------
      if (A .lt. 1.0E-08) P2mon(jper,2,imon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (P2mon(jper,2,imon) .lt. 1.0E-08) goto 42
*     set the standard deviation -----------------------------------------------
      S = P2mon(jper,2,imon)
*     calculate confidence limits ----------------------------------------------
*     divide number of samples by 12 -------------------------------------------
      qqq = amax1 ( 0.05, qualn(jper) / 12.0 )
      if ( imon .eq. 13 ) qqq = qualn(jper)
      SEM=S/SQRoot3(107283,qqq)
      XL(imon)=amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon)=amax1 (A,(A+t95*SEM))
   42 continue
      print check = print check / 12.0
*     --------------------------------------------------------------------------
 
*     --------------------------------------------------------------------------
	if ( iplace .ne. 4 ) then
	rnamex = rname (IREACH)
	else
	rnamex = 'Upstream of works'
	endif
*     --------------------------------------------------------------------------
	if ( JT(feeture) .ne. 10 ) then
	unamex = uname(feeture)
	else
	unamex = 'Start of Reach'
	endif
*     --------------------------------------------------------------------------
      
*     --------------------------------------------------------------------------
      if ( nobigout .le. 0 ) then
      if ( cprint .gt. 1.0 ) then
          
      call set format for printout(Tenchars(13),cpart2(jper,1))
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,1,i))
      enddo
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Solid for ',a11,9x,13a10)
 
      call set format for printout(Tenchars(13),cpart2(jper,2))
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,2,i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1001)
     &(Tenchars(i),i=1,13)
 1001 format('Standard deviation            ',13a10)
     
      do i = 1, 13
      call set format for printout(Tenchars(i),XL(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1005)
     &(Tenchars(i),i=1,13)
 1005 format('Lower limit on solid          ',13a10)
     
      do i = 1, 13
      call set format for printout(Tenchars(i),XU(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1006)
     &(Tenchars(i),i=1,13)
 1006 format('UPPER limit on solid          ',13a10)
      call write line (jper)
      else

      call set format for printout(Tenchars(13),cpart2(jper,1))
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,1,i))
      enddo
      write(100+jper,1880)dname(jper),(Tenchars(i),i=1,13)
 1880 format('Solid for ',a11,9x,13a10)
      
      call set format for printout(Tenchars(13),cpart2(jper,2))
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,2,i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1881)
     &(Tenchars(i),i=1,13)
 1881 format('Standard deviation            ',13a10)
     
      do i = 1, 13
      call set format for printout(Tenchars(i),XL(i))
      enddo
      if ( print check .gt. 1.0e-09 ) write(100+jper,1805)
     &(Tenchars(i),i=1,13)
 1805 format('Lower limit on solid          ',13a10)
      if ( print check .gt. 1.0e-09 ) write(100+jper,1806)
     &(Tenchars(i),i=1,13)
 1806 format('Upper limit on solid          ',13a10)
      call write line (jper)
	endif

      if ( iplace .ne. 3 ) then
      write(110+jper,1300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 1300 format(a40,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,1301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 1301 format(a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1305)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1305 format(a40,',',a40,',',a20,',','Lower limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1306)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1306 format(a40,',',a40,',',a20,',','Upper limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
      endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(110+jper,1400)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 1400 format(a40,',',a40,',',a20,',','PARTS for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1401)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 1401 format(a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1405)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1405 format(a40,',',a40,',',a20,',','Lower limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,1406)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1406 format(a40,',',a40,',',a20,',','Upper limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
	endif
      endif
*     --------------------------------------------------------------------------

      return
      end




      subroutine calculate monthly summaries of contribution (ip,JDET,Y)
      include 'COM.FOR'
      dimension Y(MS)

      do i = 1, n13
      llmon(ip,JDET,1,i) = 0.0
      llmon(ip,JDET,2,i) = 0.0
	enddo

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13 ! loop on the months --------------------------------
      CM=0.0 ! initialise mean
      CS=0.0 ! standard deviation
	N2=0   ! and numer of shots in the month

      do is = 1, NS ! loop on all shots
      imon = qmonth (is) ! set the month for this particular shot --------------
	if ( jmon .lt. 13 ) then
	if ( imon .eq. jmon ) then ! select month
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
	else
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/N2)/(N2-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 CS=SQRoot(1599,CS)
    6 CM=CM/N2

      llmon(ip,JDET,1,jmon) = amax1 ( 0.0, CM )
      llmon(ip,JDET,2,jmon) = CS
      llmon2(ip,JDET,1,jmon) = amax1 ( 0.0, CM )
      llmon2(ip,JDET,2,jmon) = CS

   88 continue ! do 88 jmon = j1, n13
      return
      end






      subroutine calculate monthly summaries of temperature
      include 'COM.FOR'
      dimension Y(MS)

      do i = 1, NS
      Y(i) = BMS (1,i)
	enddo

      do i = 1, n13
      tmon(1,i) = 0.0
      tmon(2,i) = 0.0
	enddo

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13

      CM=0.0
      CS=0.0
	N2=0.0

      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
	if ( jmon .lt. 13 ) then
	if ( imon .eq. jmon ) then
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
	else
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/N2)/(N2-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 CS=SQRoot(301099,CS)
    6 CM=CM/N2

      tmon(1,jmon) = amax1 ( 0.0, CM )
      tmon(2,jmon) = CS

   88 continue

      CM=0.0
      CS=0.0

      do IS = 1, NS
      CM = CM + Y(IS)
      CS = CS + Y(IS)*Y(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 15
      CS=0.0
      goto 16
   15 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 19
      CS=0.0
      goto 16
   19 CS=SQRoot(301699,CS)
   16 CM=CM/NS

      BC(1,1) = amax1 ( 0.0, CM )
      BC(1,2) = CS

      return
      end




      subroutine calculate monthly summaries of suspended solids
      include 'COM.FOR'
      dimension Y(MS)

      do i = 1, NS
      Y(i) = BMS (2,i)
	enddo

      do i = 1, n13
      smon(1,i) = 0.0
      smon(2,i) = 0.0
	enddo

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13

      CM=0.0
      CS=0.0
	N2=0.0

      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
	imon = qmonth (is)
	if ( jmon .lt. 13 ) then
	if ( imon .eq. jmon ) then
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
	else
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
	N2 = N2 + 1
	endif
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/N2)/(N2-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 CS=SQRoot(311099,CS)
    6 CM=CM/N2

      smon(1,jmon) = amax1 ( 0.0, CM )
      smon(2,jmon) = CS

   88 continue

      CM=0.0
      CS=0.0

      do IS = 1, NS
      CM = CM + Y(IS)
      CS = CS + Y(IS)*Y(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 15
      CS=0.0
      goto 16
   15 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 19
      CS=0.0
      goto 16
   19 CS=SQRoot(301699,CS)
   16 CM=CM/NS

      BC(2,1) = amax1 ( 0.0, CM )
      BC(2,2) = CS

      return
      end


      subroutine write the monthly data in comma separated form 
     &(jper,ipt)
      include 'COM.FOR'

	unamex = uname(feeture)

      if ( ipt .eq. 3 ) unamex = 'End of Reach  '
      if ( ipt .eq. 1 ) unamex = 'Start of Reach'
*     write out the monthly data in "commma separated" form --------------------
      write(110+jper,9180)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TRLODE2 (jper,J13),J13=2,N13),
     &TRLODE2(jper,1),999
 9180 format(a40,',',a40,',',a16,',',
     &'Contributions from boundaries and streams (2 and 10)',
     &',',a11,13(',',1pe12.4),',',i4,', 2,10')
      write(110+jper,9181)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TDDLOAD2 (jper,J13),J13=2,N13),
     &TDDLOAD2(jper,1),999
 9181 format(a40,',',a40,',',a16,',',
     &'Diffuse inflow (reach-based)',',',a11,13(',',1pe12.4),
     &',',i4,',500')
      write(110+jper,9182)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TNLOADUP2 (jper,J13),J13=2,N13),
     &TNLOADUP2(jper,1),999
 9182 format(a40,',',a40,',',a16,',',
     &'Added by natural purification',',',a11,
     &13(',',1pe12.4),',',i4,',600')
      write(110+jper,9186)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TILOADUP2 (jper,J13),J13=2,N13),
     &TILOADUP2(jper,1),999
 9186 format(a40,',',a40,',',a16,',',
     &'Added by flow gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',700')
      write(110+jper,9184)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TALOADUP2 (jper,J13),J13=2,N13),
     &TALOADUP2(jper,1),999
 9184 format(a40,',',a40,',',a16,',',
     &'Added by quality gap filling     ',
     &',',a11,13(',',1pe12.4),',',i4,',800')
*     net loads from discharges (3 12 5 and 39) *************************TELODE2
      write(110+jper,9188)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TELODE2 (jper,J13),J13=2,N13),
     &TELODE2(jper,1),999
 9188 format(a40,',',a40,',',a16,',',
     &'Net input from all types of discharges (3 12 5 and 39)',
     &',',a11,13(',',1pe12.4),',',i4,', 3,12, 5,39')
*     net load from sewage works (3) **********************************T03LOAD2
      write(110+jper,9928)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T03LOAD2 (jper,J13),J13=2,N13),
     &T03LOAD2(jper,1),999
 9928 format(a40,',',a40,',',a16,',',
     &'Net input from sewage works (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3')
      write(110+jper,9929)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T12LOAD2 (jper,J13),J13=2,N13),
     &T12LOAD2(jper,1),999
 9929 format(a40,',',a40,',',a16,',',
     &'Net input from intermittent discharges (12)',',',a11,
     &13(',',1pe12.4),',',i4,',12')
*     industrial discharges (5) ***************************************T05LOAD2
      write(110+jper,9951)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T05LOAD2 (jper,J13),J13=2,N13),
     &T05LOAD2(jper,1),999
 9951 format(a40,',',a40,',',a16,',',
     &'Net input from industrial discharges (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5')
*     mine waters (39) ************************************************T39LOAD2
      write(110+jper,9930)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T39LOAD2 (jper,J13),J13=2,N13),
     &T39LOAD2(jper,1),999
 9930 format(a40,',',a40,',',a16,',',
     &'Net input from mine waters (39)',',',a11,
     &13(',',1pe12.4),',',i4,',39')
      write(110+jper,9909)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T13LOAD2 (jper,J13),J13=2,N13),
     &T13LOAD2 (jper,1),999
 9909 format(a40,',',a40,',',a16,',',
     &'Net contributions from diffuse inflow (river-type) (13)',
     &',',a11,13(',',1pe12.4),',',i4,',13')
      write(110+jper,9910)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T25LOAD2 (jper,J13),J13=2,N13),
     &T25LOAD2 (jper,1),999
 9910 format(a40,',',a40,',',a16,',',
     &'Net contributions from diffuse pollution from livestock (25)',
     &',',a11,13(',',1pe12.4),',',i4,',25')
      write(110+jper,9911)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T27LOAD2 (jper,J13),J13=2,N13),
     &T27LOAD2 (jper,1),999
 9911 format(a40,',',a40,',',a16,',',
     &'Net contributions from diffuse pollution from arable land (27)',
     &',',a11,13(',',1pe12.4),',',i4,',27')
      write(110+jper,9912)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T29LOAD2 (jper,J13),J13=2,N13),
     &T29LOAD2 (jper,1),999
 9912 format(a40,',',a40,',',a16,',',
     &'Net contributions from diffuse pollution from highways (29)',
     &',',a11,13(',',1pe12.4),',',i4,',29')
      write(110+jper,9913)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T31LOAD2 (jper,J13),J13=2,N13),
     &T31LOAD2 (jper,1),999
 9913 format(a40,',',a40,',',a16,',',
     &'Net contributions from diffuse pollution from urban land (31)',
     &',',a11,13(',',1pe12.4),',',i4,',31')
      write(110+jper,9914)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T33LOAD2 (jper,J13),J13=2,N13),
     &T33LOAD2 (jper,1),999
 9914 format(a40,',',a40,',',a16,',',
     &'Net contribution from atmospheric deposition (33)',',',a11,
     &13(',',1pe12.4),',',i4,',33')
      write(110+jper,9915)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T35LOAD2 (jper,J13),J13=2,N13),
     &T35LOAD2 (jper,1),999
 9915 format(a40,',',a40,',',a16,',',
     &'Net contribution specified as natural background (35)',',',a11,
     &13(',',1pe12.4),',',i4,',35')
      write(110+jper,9916)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T37LOAD2 (jper,J13),J13=2,N13),
     &T37LOAD2(jper,1),999
 9916 format(a40,',',a40,',',a16,',',
     &'Net contribution from diffuse pollution from septic tanks (37)',
     &',',a11,13(',',1pe12.4),',',i4,',37')
      write(110+jper,9116)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T40LOAD2 (jper,J13),J13=2,N13),
     &T40LOAD2(jper,1),999
 9116 format(a40,',',a40,',',a16,',',
     &'Net contribution from diffuse pollution from aggregated ',
     &'CSOs (40)',',',a11,13(',',1pe12.4),',',i4,',40')
      write(110+jper,9922)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T42LOAD2 (jper,J13),J13=2,N13),
     &T42LOAD2 (jper,1),999
 9922 format(a40,',',a40,',',a16,',',
     &'Net contribution specified as aggregated STWs (42)',',',a11,
     &13(',',1pe12.4),',',i4,',42')
      write(110+jper,9925)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T46LOAD2 (jper,J13),J13=2,N13),
     &T46LOAD2 (jper,1),999
 9925 format(a40,',',a40,',',a16,',',
     &'Net contribution specified as diffuse mines (46)',',',a11,
     &13(',',1pe12.4),',',i4,',46')
      write(110+jper,9225)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T48LOAD2 (jper,J13),J13=2,N13),
     &T48LOAD2 (jper,1),999
 9225 format(a40,',',a40,',',a16,',',
     &'Net contribution specified as birds boats and angling (48)',
     &',',a11,13(',',1pe12.4),',',i4,',48')
      write(110+jper,9917)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T15LOAD2 (jper,J13),J13=2,N13),
     &T15LOAD2(jper,1),999
 9917 format(a40,',',a40,',',a16,',',
     &'Net contribution fromd iffuse inflow (effluent-type) (15)',
     &',',a11,13(',',1pe12.4),',',i4,',15')
      write(110+jper,9919)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TGLODE2 (jper,J13),J13=2,N13),
     &TGLODE2 (jper,1),999
 9919 format(a40,',',a40,',',a16,',',
     &'Net TOTAL of all loads',',',a11,13(',',1pe12.4),',',i4,',999')
      write(110+jper,9918)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TBLOAD2 (jper,J13),J13=2,N13),
     &TBLOAD2(jper,1),999
 9918 format(a40,',',a40,',',a16,',',
     &'Load lost to abstractions (9)',',',a11,
     &13(',',1pe12.4),',',i4,', 7, 18, 19')
      write(110+jper,9183)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TNLOADDN2 (jper,J13),J13=2,N13),
     &TNLOADDN2(jper,1),999
 9183 format(a40,',',a40,',',a16,',',
     &'Load lost to natural purification',',',a11,13(',',1pe12.4),',',i4
     &,',601')
      write(110+jper,9187)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TILOADDN2 (jper,J13),J13=2,N13),
     &TILOADDN2(jper,1),999
 9187 format(a40,',',a40,',',a16,',',
     &'Load lost by flow gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',701')
      write(110+jper,9185)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TALOADDN2 (jper,J13),J13=2,N13),
     &TALOADDN2(jper,1),999
 9185 format(a40,',',a40,',',a16,',',
     &'Load lost by quality gap filling     ',
     &',',a11,13(',',1pe12.4),',',i4,',801')
      write(110+jper,9195)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(TLOSSES (jper,J13),J13=2,N13),
     &TLOSSES(jper,1),999
 9195 format(a40,',',a40,',',a16,',',
     &'TOTAL LOAD REMOVED',
     &',',a11,13(',',1pe12.4),',',i4,',802')

*     **************************************************************************
      write(110+jper,9948)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T03LOAD1 (jper,J13),J13=2,N13),
     &T03LOAD1(jper,1),999
 9948 format(a40,',',a40,',',a16,',',
     &'TOTAL INPUT BY SEWAGE WORKS (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3')
*     **************************************************************************
      write(110+jper,9949)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T12LOAD1 (jper,J13),J13=2,N13),
     &T12LOAD1(jper,1),999
 9949 format(a40,',',a40,',',a16,',',
     &'TOTAL INPUT BY INTERMITTENT DISCHARGES (12)',',',a11,
     &13(',',1pe12.4),',',i4,',12')
*     **************************************************************************
      write(110+jper,9950)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T05LOAD1 (jper,J13),J13=2,N13),
     &T05LOAD1(jper,1),999
 9950 format(a40,',',a40,',',a16,',',
     &'TOTAL INPUT BY INDUSTRIAL DISCHARGES (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5')
*     **************************************************************************
      write(110+jper,9989)GIScode(feeture),unamex,
     &rname(ireach),dname(jper),(T39LOAD1 (jper,J13),J13=2,N13),
     &T39LOAD1(jper,1),999
 9989 format(a40,',',a40,',',a16,',',
     &'TOTAL INPUT FROM MINE WATERS (39)',',',a11,
     &13(',',1pe12.4),',',i4,',39')
*     **************************************************************************
      
      return
      end

      
      
      subroutine write the monthly data in tables (jper)
      include 'COM.FOR'
	character *10 Tenchars (13)
     
      if ( QTYPE (jper) .ne. 4 ) return
      if ( nobigout .gt. 0 ) return

      do idet = 1, n13
      Tenchars(idet) = ' .........'
	enddo

	if ( abs (TGLODE2 (jper,i13)) .gt. 0.000001) then
      write(100+jper,3022)dname(jper)
 3022 format(///150('-'),2x,8('-')/
     &'Apportionment of the net loads for ',a11,'...'/150('-'),
     &2x,8('-'))
      endif
      
*     running total load introduced by upstream boundaries and tributaries -----
      if ( abs (TRLODE2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TRLODE2 (jper,i+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TRLODE2 (jper,i13))
      write(100+jper,3000)(Tenchars (i),i=1,13)
 3000 format('Boundaries and streams (2,10) ',13a10)
      endif
*     total load introduced by clean diffuse sources -------------------TDDLOAD2
      if ( abs (TDDLOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TDDLOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TDDLOAD2 (jper,i13))
      write(100+jper,3001)(Tenchars (i),i=1,13)
 3001 format('Diffuse (reach-based)',9x,13a10)
      endif
*     added by natural purification -----------------------------------TNLOADUP2
      if ( abs (TNLOADUP2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TNLOADUP2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TNLOADUP2 (jper,i13))
      write(100+jper,3002)(Tenchars (i),i=1,13)
 3002 format('Added by natural purification ',13a10)
      endif

*     added by flow gap filling ----------------------------------------TILOADUP2
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),TILOADUP2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TILOADUP2 (jper,i13))
      write(100+jper,3006)(Tenchars (i),i=1,13)
 3006 format('Added by flow gap filling     ',13a10)
      endif

*     added by quality gap filling for --------------------------------TALOADUP2
      if ( ical .gt. 3 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TALOADUP2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TALOADUP2 (jper,i13))
      write(100+jper,3004)(Tenchars (i),i=1,13)
 3004 format('Added by quality gap filling  ',13a10)
      endif

*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),TELODE2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TELODE2 (jper,i13))
*     write(100+jper,3008)(Tenchars (i),i=1,13)
 3008 format('Net discharges (3 12 5 and 39)',13a10)
*     net load from sewage works (3) -----------------------------------T03LOAD2
      if ( abs (T03LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),T03LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T03LOAD2 (jper,i13))
      write(100+jper,3028)(Tenchars (i),i=1,13)
 3028 format('Sewage works (3)   ',11x,13a10)
      endif
*     intermittent discharges (12) ------------------------------------T12LOAD2
      if ( abs (T12LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),T12LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T12LOAD2 (jper,i13))
      write(100+jper,3029)(Tenchars (i),i=1,13)
 3029 format('Intermittent discharges (12)',2x,13a10)
      endif
*     industrial discharges (5) ------------------------------------------------
      if ( abs (T05LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T05LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T05LOAD2 (jper,i13))
      write(100+jper,3051)(Tenchars (i),i=1,13)
 3051 format('Industrial discharges (5)   ',2x,13a10)
      endif
*     mine waters (39) ---------------------------------------------------------
      if ( abs (T39LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T39LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T39LOAD2 (jper,i13))
      write(100+jper,3030)(Tenchars (i),i=1,13)
 3030 format('Mine waters (39)            ',2x,13a10)
      endif
*     diffuse (river-type) (13) ------------------------------------------------
      if ( abs (T13LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T13LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T13LOAD2 (jper,i13))
      write(100+jper,3009)(Tenchars (i),i=1,13)
 3009 format('Diffuse (river-type) (13)     ',13a10)
      endif
*     livestock (25) -----------------------------------------------------------
      if ( abs (T25LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T25LOAD2 (jper,I+1))
	enddo
      endif
      call set format for printout(Tenchars (13),T25LOAD2 (jper,i13))
      write(100+jper,3010)(Tenchars (i),i=1,13)
 3010 format('Livestock (25)                ',13a10)
      endif
*     arable (27) --------------------------------------------------------------
      if ( abs (T27LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T27LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T27LOAD2 (jper,i13))
      write(100+jper,3011)(Tenchars (i),i=1,13)
 3011 format('Arable (27)                   ',13a10)
      endif
*     highway (29) -------------------------------------------------------------
      if ( abs (T29LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T29LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T29LOAD2 (jper,i13))
      write(100+jper,3012)(Tenchars (i),i=1,13)
 3012 format('Highway (29)                  ',13a10)
      endif
*     urban (31) ---------------------------------------------------------------
      if ( abs (T31LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T31LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T31LOAD2 (jper,i13))
      write(100+jper,3013)(Tenchars (i),i=1,13)
 3013 format('Urban (31)                    ',13a10)
      endif
*     atmosphere (33) ----------------------------------------------------------
      if ( abs (T33LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T33LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T33LOAD2 (jper,i13))
      write(100+jper,3014)(Tenchars (i),i=1,13)
 3014 format('Atmosphere (33)               ',13a10)
      endif
*     natural background (35) --------------------------------------------------
      if ( abs (T35LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T35LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T35LOAD2 (jper,i13))
      write(100+jper,3015)(Tenchars (i),i=1,13)
 3015 format('Natural background (35)       ',13a10)
      endif
*     septic tanks (37) --------------------------------------------------------
      if ( abs (T37LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T37LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T37LOAD2 (jper,i13))
      write(100+jper,3016)(Tenchars (i),i=1,13)
 3016 format('Septic tanks (37)             ',13a10)
      endif
*     Aggregate CSOs (40) ------------------------------------------------------
      if ( abs (T40LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T40LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T40LOAD2 (jper,i13))
      write(100+jper,3216)(Tenchars (i),i=1,13)
 3216 format('Aggregate CSOs (40)           ',13a10)
      endif
*     Diffuse (effluent-type) (15) --------------------------------------------
      if ( abs (T15LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T15LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T15LOAD2 (jper,i13))
      write(100+jper,3017)(Tenchars (i),i=1,13)
 3017 format('Diffuse (effluent-type) (15)  ',13a10)
      endif
*     aggregate STWs (42) ------------------------------------------------------
      if ( abs (T42LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T42LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T42LOAD2 (jper,i13))
      write(100+jper,3817)(Tenchars (i),i=1,13)
 3817 format('Aggregate STWs (42)           ',13a10)
      endif
*     Diffuse mines (46) -------------------------------------------------------
      if ( abs (T46LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T46LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T46LOAD2 (jper,i13))
      write(100+jper,3815)(Tenchars (i),i=1,13)
 3815 format('Diffuse mines (46)            ',13a10)
      endif
*     Birds, boats and angling (48) --------------------------------------------
      if ( abs (T48LOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T48LOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),T48LOAD2 (jper,i13))
      write(100+jper,3885)(Tenchars (i),i=1,13)
 3885 format('Birds, boats and angling (48) ',13a10)
      endif
*     Headwaters and tributaries (10) -------------------------------------------
      if ( abs (TRLODE2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TRLODE2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TRLODE2 (jper,i13))
      write(100+jper,3865)(Tenchars (i),i=1,13)
 3865 format('Headwater and streams (10)    ',13a10)
      endif

*     total--------------------------------------------------------------TGLODE2
      if ( abs(TGLODE2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TGLODE2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TGLODE2 (jper,i13))
      call write line (jper)
      write(100+jper,3019)(Tenchars (i),i=1,13)
 3019 format('Total                         ',13a10)
      endif
      call write line (jper)
*     --------------------------------------------------------------------------


*     lost to abstractions ----------------------------------------------TBLOAD2
      if ( abs (TBLOAD2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TBLOAD2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TBLOAD2 (jper,i13))
      write(100+jper,3018)(Tenchars (i),i=1,13)
 3018 format('Abstractions (9)      ',8x,13a10)
      endif 

*     lost by natural purification ------------------------------------TNLOADDN2
      if ( abs (TNLOADDN2 (jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TNLOADDN2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TNLOADDN2 (jper,i13))
      write(100+jper,3003)(Tenchars (i),i=1,13)
 3003 format('Lost by natural purification  ',13a10)
	endif

*     if ( abs (TILOADDN2 (jper,i13)) .gt. 0.000001) then
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TILOADDN2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TILOADDN2 (jper,i13))
      write(100+jper,3007)(Tenchars (i),i=1,13)
 3007 format('Lost by flow gap filling      ',13a10)
      endif

*     if ( abs (TALOADDN2 (jper,i13)) .gt. 0.000001) then
      if ( ical .gt. 3 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TALOADDN2 (jper,I+1))
	enddo
	endif
      call set format for printout(Tenchars (13),TALOADDN2 (jper,i13))
      write(100+jper,3005)(Tenchars (i),i=1,13)
 3005 format('Lost by quality gap filling   ',13a10)
      endif

      if ( abs (TLOSSES (jper,i13)) .gt. 0.000001) then
      call set format for printout(Tenchars (13),TLOSSES (jper,i13))
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TLOSSES (jper,I+1))
	enddo
	endif ! set up print out
      call set format for printout(Tenchars (13),TLOSSES (jper,i13))
      call write line (jper)
      write(100+jper,3905)(Tenchars (i),i=1,13)
 3905 format('Total losses ...',14x,13a10)
      call write line (jper)
	write(100+jper,6905)
 6905 format(///)
      endif ! if ( abs (TLOSSES (jper,i13)) .gt. 0.000001)

      do idet = 1, ndet
      Tenchars(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	Tenchars(idet) = dna(idet)
      endif
      enddo
	write(120+jper,4166)(Tenchars(idet),idet = 1, ndet)
 4166 format(//150('=')/'Percentages of net load from all inputs ',
     &10(6x,a4)/150('='))
      write(120+jper,4806)

	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop10(jper)) .gt. 1.0e-10 ) then
      call sort format 3  (prop10(jper),propRT(jper),propNPU(jper))
      write(120+jper,8930)valchars10,valchars11,valchars12
 8930 format('+',4x,'Boundaries and tributaries (2, 10 and 12)',
     &5x,10A10)
      endif
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propRT(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propRT(jper)
 7831 format(f10.4)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,9163)Tenchars(jper)
 9163 format('*',4x,'Reach-type diffuse sources',19X,10A10)
      endif
*     ==========================================================================
*     introduced by natural purification .....
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propNPU(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propNPU(jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,9103)Tenchars(jper)
 9103 format('*',4x,'Added by natural purification',16X,10A10)
      endif

*     =========================================================================
*     loads added by gap filling for flows ----------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
	write(Tenchars(jper),7831)propfrl(jper)
      endif
	if ( kp .eq. 1 ) write(120+jper,8923)Tenchars(jper)
 8923 format('*',4x,'Added by gap filling of flows',16x,10A10)
*     ==========================================================================
*     loads added by gap filling for quality -----------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 ) kp = 1
	write(Tenchars(jper),7831)propqrl(jper)
      endif
	if ( kp .eq. 1 ) write(120+jper,8224)Tenchars(jper)
 8224 format('*',4x,'Added by gap filling of quality',14x,10A10)
*     ==========================================================================


*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propeff2(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propeff2 (jper)
      endif
	if ( kp .eq. 1 ) then
*     write(120+jper,8363)Tenchars(jper)
 8363 format(5X,'Point discharges (3, 12 and 5)',15X,10A10)
*     if ( kp .eq. 1 ) write(120+jper,4826)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propeff3(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propeff3 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8303)Tenchars(jper)
 8303 format('*',4x,'Sewage treatment works (3)',19X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propeff12(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propeff12 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8312)Tenchars(jper)
 8312 format('*',4x,'Intermittent discharges (12)',17X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propeff5(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propeff5 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8305)Tenchars(jper)
 8305 format('*',4x,'Industrial discharges (5)',20x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propeff39(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propeff39 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8395)Tenchars(jper)
 8395 format('*',4x,'Mine waters (39)         ',20X,10A10)
      write(120+jper,4826)
 4826 format(150('-'))
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop13(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop13 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8366)Tenchars(jper)
 8366 format('*',4x,'River-based diffuse pollution (13)',11X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop15(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop15 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,2366)Tenchars(jper)
 2366 format('*',4x,'Effluent-based diffuse pollution (15)',8X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop42(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop42 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8966)Tenchars(jper)
 8966 format('*',4x,'Aggregate STWs (42)                  ',8X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop25(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop25 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8364)Tenchars(jper)
 8364 format('*',4x,'Livestock (25)',31X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop27(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop27 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8365)Tenchars(jper)
 8365 format('*',4x,'Arable (27)',34X,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop29(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop29 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8367)Tenchars(jper)
 8367 format('*',4x,'Highway runoff (29)',26x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop31(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop31 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8368)Tenchars(jper)
 8368 format('*',4x,'Urban runoff (31)',28x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop33(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop33 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8369)Tenchars(jper)
 8369 format('*',4x,'Atmospheric deposition (33)',18x,10A10)
      endif
*     ==========================================================================
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop35(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop35 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8069)Tenchars(jper)
 8069 format('*',4x,'Natural background (35)',22x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop46(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop46 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,4069)Tenchars(jper)
 4069 format('*',4x,'Diffuse mines (46)',27x,10A10)
      endif
*     ==========================================================================
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop48(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop48 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,4669)Tenchars(jper)
 4669 format('*',4x,'Birds, boats and angling (48)',16x,10A10)
      endif
*     ==========================================================================
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop10(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop10 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,4869)Tenchars(jper)
 4869 format('*',4x,'Headwaters and streams (10)  ',16x,10A10)
      endif
	kp = 0
*     ==========================================================================
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop37(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop37 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8370)Tenchars(jper)
 8370 format('*',4x,'Septic tanks (37)',28x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prop40(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prop40 (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8270)Tenchars(jper)
 8270 format('*',4x,'Aggregate CSOs (40)',26x,10A10)
      endif
*     ==========================================================================
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propall(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propall (jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,4136)Tenchars(jper)
 4136 format(150('-')/'TOTAL NET LOAD (%) ...',28x,10A10)
      write(120+jper,4806)
      endif
*     ==========================================================================
	kp = 0
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prolosses(jper)) .gt. 1.0e-10 ) kp = 1
      endif
	if ( kp .eq. 1 ) write(120+jper,4106)
 4106 format(150('=')/'Percentages of the total load that have ',
     &'been ...'/150('='))
*     ==========================================================================
*     loads removed by abstractions --------------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propabs(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propabs(jper)
      endif
	if ( kp .eq. 1 ) write(120+jper,8246)Tenchars(jper)
 8246 format(5x,'Removed by abstractions (7, 18 and 19)',7x,10A10)
*     =========================================================================
*     loads removed by natural purification ------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      Tenchars(jper) = '         0'
	if ( abs (propNPD(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propNPD(jper)
      endif
      write(120+jper,1931)Tenchars(jper)
 1931 format(5x,'Removed by natural purification',14x,10A10)
*     =========================================================================
*     loads removed by gap filling for flows ----------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
*	if ( abs (propfra(jper)) .gt. 1.0e-10 ) kp = 1
	if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
	write(Tenchars(jper),7831)propfra(jper)
      endif
	if ( kp .eq. 1 ) write(120+jper,8913)Tenchars(jper)
 8913 format(5X,'Removed by gap filling of flows',14x,10A10)
*     ==========================================================================
*     loads removed by gap filling for quality --------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
*	if ( abs (propqra(jper)) .gt. 1.0e-10 ) kp = 1
	if ( ical .gt. 3 ) kp = 1
	write(Tenchars(jper),7831)propqra(jper)
      endif
	if ( kp .eq. 1 ) write(120+jper,8964)Tenchars(jper)
 8964 format(5X,'Removed by gap filling of quality',12x,10A10)
	if ( kp .eq. 1 ) write(120+jper,4806)
 4806 format(150('='))
*     ==========================================================================
*     total losses of load -----------------------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (prolosses(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)prolosses(jper)
      endif
	if ( kp .eq. 1 ) then
      write(120+jper,8914)Tenchars(jper)
 8914 format(150('-')/'TOTAL % LOSSES ',35x,10a10/150('-'))
      write(120+jper,4806)
      endif
*     =========================================================================


*     ==========================================================================
	kp = 0
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (proadds(jper)) .gt. 1.0e-10 ) kp = 1
      endif
*	if ( kp .eq. 1 ) write(120+jper,4896)
 4896 format(150('=')/'Percentages of the total load that have ',
     &'been'/150('='))
*     =========================================================================
*     loads added by gap filling for flows ------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propfrl(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propfrl(jper)
      endif
*	if ( kp .eq. 1 ) write(120+jper,8923)Tenchars(jper)
*     ==========================================================================
*     loads added by gap filling for quality -----------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (propqrl(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)propqrl(jper)
      endif
*	if ( kp .eq. 1 ) write(120+jper,8224)Tenchars(jper)
*     ==========================================================================
*     total additions of load --------------------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (proadds(jper)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars(jper),7831)proadds(jper)
      endif
	if ( kp .eq. 1 ) then
*     write(120+jper,8974)Tenchars(jper)
 8974 format(150('-')/'TOTAL % ADDITIONS ',32x,10a10)
*	write(120+jper,4806)
      endif
*     =========================================================================



*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	write(120+jper,4886)
 4886 format(///150('=')/'Total added loads (without allowing for ',
     &'any losses)'/150('='))
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from headwaters and tributaries etc --------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TRLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TRLODE1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,3930)Tenchars(jper)
 3930 format('*',4x,'Boundaries and tributaries (2, 10 and 12)',
     &4x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from reach-based diffuse sources -----------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TDDLOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TDDLOAD1(jper,i13))
      endif
 	if ( kp .eq. 1 ) write(120+jper,4930)Tenchars(jper)
 4930 format('*',4x,'Reach-type diffuse sources',19x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loads added by natural purification --------------------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TNLOADUP1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TNLOADUP1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4931)Tenchars(jper)
 4931 format(5X,'Added by natural purification',16X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river flows -------------------- 
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),TILOADUP1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,2943)Tenchars(jper)
 2943 format('*',4x,'Added by gap filling of flow',17x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river quality ------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),TALOADUP1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,2944)Tenchars(jper)
 2944 format('*',4x,'Added by gap filling of quality',14X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by discharges of effluent (types 3, 12 and 5) -----
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TELODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TELODE1(jper,i13))
      endif
*	if ( kp .eq. 1 ) write(120+jper,8983)Tenchars(jper)
 8983 format(5X,'Total from discharges (types 3, 12 and 5)',4x,10A10)
*	if ( kp .eq. 1 ) write(120+jper,4826)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads introduced by discharges of effluent (3) ------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T03LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T03LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2733)Tenchars(jper)
 2733 format('*',4x,'Total from sewage effluents (3)',14x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads introduced by intermittent discharges of sewage (12)
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T12LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T12LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2739)Tenchars(jper)
 2739 format('*',4x,'Intermittent discharges of sewage (12)',7x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from industrial discharges (5) ----------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T05LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T05LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1933)Tenchars(jper)
 1933 format('*',4x,'Total from industrial discharges (5)',9x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T39LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T39LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1993)Tenchars(jper)
 1993 format('*',4x,'Total from mine waters (39)      ',12x,10A10)
	if ( kp .eq. 1 ) write(120+jper,4826)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from diffuse features: river flow type (13) ---------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T13LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T13LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8113)Tenchars(jper)
 8113 format('*',4x,'River-type diffuse pollution (13)',12X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from diffuse features: discharge type (15) ----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T15LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T15LOAD1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,8233)Tenchars(jper)
 8233 format('*',4x,'Effluent-type diffuse pollution (15)',9X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from diffuse pollution from livestock (25) ----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T25LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T25LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8125)Tenchars(jper)
 8125 format('*',4x,'Diffuse pollution from livestock (25)',8X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from diffuse pollution from arable (27) -------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T27LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T27LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8127)Tenchars(jper)
 8127 format('*',4x,'Diffuse pollution from arable (27)',11X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from highway runoff (29) ----------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T29LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T29LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8129)Tenchars(jper)
 8129 format('*',4x,'Highway runoff (29)',26X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from urban runoff (31) ------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T31LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T31LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8131)Tenchars(jper)
 8131 format('*',4x,'Urban runoff (31)',28X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from atmospheric deposition (33) --------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T33LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T33LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8133)Tenchars(jper)
 8133 format('*',4x,'Atmospheric deposition (33)',18x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from natural background (35) ------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T35LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T35LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8193)Tenchars(jper)
 8193 format('*',4x,'Natural background (35)',22x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from Aggregated STWs (42) ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T42LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T42LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4193)Tenchars(jper)
 4193 format('*',4x,'Aggregated STWs (42)',25x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from diffuse mines (46) -----------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T46LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T46LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4593)Tenchars(jper)
 4593 format('*',4x,'Diffuse mines (46)',27x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from birds, boats and angling (48) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T48LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T48LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4793)Tenchars(jper)
 4793 format('*',4x,'Birds, boats and angling (48)',16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from headwaters and streams (10) --------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TRLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TRLODE1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4893)Tenchars(jper)
 4893 format('*',4x,'Headwaters and streams (10)',16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from septic tanks (37) ------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T37LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T37LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8137)Tenchars(jper)
 8137 format('*',4x,'Septic tanks (37)',28x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from aggregated CSOs (40) ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (T40LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T40LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8337)Tenchars(jper)
 8337 format('*',4x,'Aggregate CSOs (40)',26x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	if ( kp .eq. 1 ) write(100+jper,4806) ! total ----------------------------
	kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TGLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TGLODE1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1593)Tenchars(jper)
 1593 format(150('-')/'TOTAL    ',41x,10A10)
	if ( kp .eq. 1 ) write(120+jper,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads from abstractions -----------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TBLOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TBLOAD1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,8246)Tenchars(jper)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! loads removed by natural purification ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TNLOADDN1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TNLOADDN1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1931)Tenchars(jper)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads introduced by gap filling for river flows -----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 .and. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),TILOADDN1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,8913)Tenchars(jper)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total loads introduced by gap filling for river quality ---------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),TALOADDN1(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,8964)Tenchars(jper)
	if ( kp .eq. 1 ) write(120+jper,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0 ! total losses of load --------------------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
	if ( abs (TLOSSES(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TLOSSES(jper,i13))
      endif
	if ( kp .eq. 1 ) write(120+jper,7964)Tenchars(jper)
 7964 format('TOTAL LOAD REMOVED ...',28x,10A10)
	if ( kp .eq. 1 ) write(120+jper,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      return
      end
      






      subroutine write out monthly flows and temperature (iplace)
      include 'COM.FOR'
	character *10 Tenchars (13)
     
      if ( nobigout .gt. 0 ) return

      do idet = 1, n13
      Tenchars(idet) = ' .........'
	enddo

	do 3399 jper = 1, ndet
      if ( QTYPE (jper) .ne. 4 ) then
      if ( iplace .eq. 1 ) then
      write(100+jper,2011)
 2011 format('Monthly data at the head of the reach ...')
      endif
      if ( iplace .eq. 2 ) then
      write(100+jper,2111)
 2111 format('Monthly data at feature ... ')
      endif
      if ( iplace .eq. 3 ) then
      write(100+jper,2211)
 2211 format(/150('-')/'Monthly data at the end of the reach ...')
      endif
      if ( iplace .eq. 4 ) then
      write(100+jper,2311)uname(feeture)
 2311 format(/150('-'),2x,8('-'),1x,12('-')/
     &'MONTHLY DATA UPSTREAM OF DISCHARGE ... ',a37)
      endif
      if ( iplace .eq. 5 ) then
      write(100+jper,2411)uname(feeture)
 2411 format('Monthly data downstream of tributary ... ',a37)
      endif
      if ( iplace .eq. 6 ) then
      write(100+jper,2511)uname(feeture)
 2511 format(/150('-'),2x,8('-'),1x,12('-')/
     &'MONTHLY DATA DOWNSTREAM OF DISCHARGE ... ',a37)
	endif

      write(100+jper,2000)(names of months (i),i = 1, 12)
 2000 format(150('-'),2x,8('-'),1x,12('-')/
     &'Summary statistic             ',
     &12(1x,a9),'  All year Running mean'/150('-'),
     &2x,8('-'),1x,12('-'))

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),fmon(1,i))
	enddo
	endif
      call set format for printout(Tenchars (13),FLOW(1))

      write(100+jper,1100)(Tenchars (i),i=1,13)
 1100 format('Monthly mean river flow       ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),fmon(2,i))
	enddo
	endif
      call set format for printout(Tenchars (13),FLOW(2))

      write(100+jper,1101)(Tenchars (i),i=1,13)
 1101 format('95-percentile                 ',13a10)
      call write line (jper)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),tmon(1,i))
	enddo
	endif
      call set format for printout(Tenchars (13),BC(1,1))

      write(100+jper,1800)(Tenchars (i),i=1,13)
 1800 format('Monthly mean temperature      ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),tmon(2,i))
	enddo
	endif
      call set format for printout(Tenchars (13),BC(1,2))

      write(100+jper,1801)(Tenchars (i),i=1,13)
 1801 format('Standard deviation            ',13a10)

      call write line (jper)

      xprint = 0.0
	do i = 1, 12
      xprint = xprint + smon(1,i)
      enddo
      if ( xprint .gt. 0.00001 ) then

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),smon(1,i))
	enddo
	endif
      call set format for printout(Tenchars (13),BC(2,1))

      write(100+jper,1900)(Tenchars (i),i=1,13)
 1900 format('Mean suspended solids         ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),smon(2,i))
	enddo
	endif
      call set format for printout(Tenchars (13),BC(2,2))

      write(100+jper,1901)(Tenchars (i),i=1,13)
 1901 format('Standard deviation            ',13a10)
      call write line (jper)
	endif ! if ( xprint .gt. 0.00001 )


      write(110+jper,8100)GIScode(feeture),unamex,rnamex,
     &(fmon(1,i),i=1,12),FLOW(1),jxtype
 8100 format(a40,',',a40,',',a20,',',
     &'Monthly mean river flow       ',',','River flow',
     &13(',',1pe12.4),',',i4)
      write(110+jper,8101)GIScode(feeture),unamex,rnamex,
     &(fmon(2,i),i=1,12),FLOW(2),jxtype
 8101 format(a40,',',a40,',',a20,',',
     &'95-percentile                 ',',','River flow',
     &13(',',1pe12.4),',',i4)
      write(110+jper,8800)GIScode(feeture),unamex,rnamex,
     &(tmon(1,i),i=1,12),BC(1,1),jxtype
 8800 format(a40,',',a40,',',a20,',',
     &'Monthly mean temperature      ',',','Temperature',
     &13(',',1pe12.4),',',i4)
      write(110+jper,8801)GIScode(feeture),unamex,rnamex,
     &(tmon(2,i),i=1,12),BC(1,2),jxtype
 8801 format(a40,',',a40,',',a20,',',
     &'Standard deviation            ',',','Temperature',
     &13(',',1pe12.4),',',i4)
      write(110+jper,8900)GIScode(feeture),unamex,rnamex,
     &(smon(1,i),i=1,12),BC(2,1),jxtype
 8900 format(a40,',',a40,',',a20,',',
     &'Mean suspended solids         ',',','Suspended solids',
     &13(',',1pe12.4),',',i4)
      write(110+jper,8901)GIScode(feeture),unamex,rnamex,
     &(smon(2,i),i=1,12),BC(2,2),jxtype
 8901 format(a40,',',a40,',',a20,',',
     &'Standard deviation            ',',','Suspended solids',
     &13(',',1pe12.4),',',i4)
      endif
 3399 continue
      return
      end
      
      
      subroutine write out the monthly apportionment (jper)
      include 'COM.FOR'
      character *10 llmonc(nprop,MP10,2,13),llmonc2(nprop,MP10,1,13),
     &runchar

*     iplace 1 ... start of reach
*     iplace 3 ... end of reach
*     iplace 2 ... at feature
*     iplace 4 ... upstream of discharge
*     iplace 5 ... downstream of tributary
*     iplace 6 ... downstream of discharge
      if ( QTYPE (jper) .eq. 4 ) return

*     calculate % contribution of monthly effluent load to the annual mean -----
      nprap = nprop ! 5555555555555555555555555555555555555555555555555555555555
      do 3500 ip = 1, nprap ! different types of features
      xml = 0.0
      do im = 1, 12 ! sum the monthly means
      xml = xml + llmon2(ip,jper,1,im) * fraction of year (im)
      enddo ! do im = 1, 12
	xml = xml * 365.0
      
      do im = 1, 12
	if ( xml .gt. 0.000001 ) then
      llmon2(ip,jper,1,im) = 100.0 * days in months (im) * 
     &llmon2(ip,jper,1,im) / xml
	else
      llmon2(ip,jper,1,im) = 0.0
      endif
      enddo
      
      if ( im .eq. 13 ) then
	if ( xml .gt. 0.000001 ) then
      llmon2(ip,jper,1,im) = 100.0 * 365.0 * 
     & llmon2(ip,jper,1,im) / xml
	else
      llmon2(ip,jper,1,im) = 0.0
	endif
      endif
 3500 continue 

      running mean = 0.0

      zero check = 0.0
      do 3000 ip = 1, nprop
	do im = 1, 13
      zero check = amax1 ( zero check, llmon(ip,jper,1,im) )
      call set format for printout
     &(llmonc(ip,jper,1,im),llmon(ip,jper,1,im))
      call set format for printout
     &(llmonc(ip,jper,2,im),llmon(ip,jper,2,im))
      call set format for printout
     &(llmonc2(ip,jper,1,im),llmon2(ip,jper,1,im))
	enddo
 3000 continue 

      do 2000 ip = 1, nprop ! 55555555555555555555555555555555555555555555555555
*     --------------------------------------------------------------------------
*     output for features ----------------------------------------------------+1
      if ( nobigout .le. 0 ) then
	if ( zero check .gt. 1.0e-09 ) then
*     mean from all discharges -------------------------------------------------
	if ( ip .eq. 1 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      write(100+jper,1090)(llmonc(ip,jper,1,i),i=1,13)
 1090 format('Mean from all discharges      ',13a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
 1692 format('Standard deviation            ',13a10)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
 1098 format('% to annual mean ...          ',13a10)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
 1398 format('% to in-river mean ...        ',13f10.2)
      endif
      call write line (jper)
	endif
*     mean from sewage works (3) -----------------------------------------------
	if ( ip .eq. 2 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1190)(llmonc(ip,jper,1,i),i=1,13),runchar
 1190 format('Mean from sewage works (3)    ',13a10,a10)
      write(120+jper,8190)dname(jper),llmonc(ip,jper,1,13),runchar
 8190 format(///100('-')/'Apportionment of the annual mean load for ',
     &a11,' ... and the running total'/
     &100('-')/'Mean from sewage works (3)    ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from intermittents (12) ---------------------------------------------
	if ( ip .eq. 3 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1295)(llmonc(ip,jper,1,i),i=1,13),runchar
 1295 format('Mean from intermittents (12)  ',13a10,a10)
      write(120+jper,8295)llmonc(ip,jper,1,13),runchar
 8295 format('Mean from intermittents (12)  ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from industry (5) ---------------------------------------------------
	if ( ip .eq. 4 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1390)(llmonc(ip,jper,1,i),i=1,13),runchar
 1390 format('Mean from industry (5)        ',13a10,a10)
      write(120+jper,8390)llmonc(ip,jper,1,13),runchar
 8390 format('Mean from industry (5)        ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from mine waters (39) -----------------------------------------------
	if ( ip .eq. 5 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1490)(llmonc(ip,jper,1,i),i=1,13),runchar
 1490 format('Mean from mine waters (39)    ',13a10,a10)
      write(120+jper,8490)llmonc(ip,jper,1,13),runchar
 8490 format('Mean from mine waters (39)    ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from livestock farming (25) ----------------------------------------------
	if ( ip .eq. 6 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,3490)(llmonc(ip,jper,1,i),i=1,13),runchar
 3490 format('From livestock farming (25)   ',13a10,a10)
      write(120+jper,3499)llmonc(ip,jper,1,13),runchar
 3499 format('From livestock farming (25)   ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from arable farming (27) -------------------------------------------------
	if ( ip .eq. 7 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,4490)(llmonc(ip,jper,1,i),i=1,13),runchar
      write(120+jper,4490)llmonc(ip,jper,1,13),runchar
 4490 format('Mean from arable farming (27) ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from highway runoff (29) -------------------------------------------------
	if ( ip .eq. 8 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5490)(llmonc(ip,jper,1,i),i=1,13),runchar
 5490 format('Mean from highway runoff (29) ',13a10,a10)
      write(120+jper,5499)llmonc(ip,jper,1,13),runchar
 5499 format('Mean from highway runoff (29) ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from urban runoff (31) ---------------------------------------------------
	if ( ip .eq. 9 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1690)(llmonc(ip,jper,1,i),i=1,13),runchar
 1690 format('Mean from urban runoff (31)   ',13a10,a10)
      write(120+jper,1699)llmonc(ip,jper,1,13),runchar
 1699 format('Mean from urban runoff (31)   ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from atmospheric deposition (33) -----------------------------------------
	if ( ip .eq. 10 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1790)(llmonc(ip,jper,1,i),i=1,13),runchar
 1790 format('Atmospheric deposition (33)   ',13a10,a10)
      write(120+jper,1799)llmonc(ip,jper,1,13),runchar
 1799 format('Atmospheric deposition (33)   ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from natural background (35) ---------------------------------------------
	if ( ip .eq. 11 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1890)(llmonc(ip,jper,1,i),i=1,13),runchar
 1890 format('From natural background (35)  ',13a10,a10)
      write(120+jper,1899)llmonc(ip,jper,1,13),runchar
 1899 format('From natural background (35)  ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from septic tanks (37) ---------------------------------------------------
	if ( ip .eq. 12 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1892)(llmonc(ip,jper,1,i),i=1,13),runchar
 1892 format('Mean from septic tanks (37)   ',13a10,a10)
      write(120+jper,8892)llmonc(ip,jper,1,13),runchar
 8892 format('Mean from septic tanks (37)   ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from aggregated CSOs (40) ------------------------------------------------
	if ( ip .eq. 13 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1893)(llmonc(ip,jper,1,i),i=1,13),runchar
 1893 format('Mean from aggregated CSOs (40)',13a10,a10)
      write(120+jper,8893)llmonc(ip,jper,1,13),runchar
 8893 format('Mean from aggregated CSOs (40)',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from aggregated STWs (42) ------------------------------------------------
	if ( ip .eq. 14 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1894)(llmonc(ip,jper,1,i),i=1,13),runchar
 1894 format('Mean from aggregated STWs (42)',13a10,a10)
      write(120+jper,8894)llmonc(ip,jper,1,13),runchar
 8894 format('Mean from aggregated STWs (42)',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from diffuse mines (46) --------------------------------------------------
	if ( ip .eq. 15 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1895)(llmonc(ip,jper,1,i),i=1,13),runchar
 1895 format('Mean from diffuse mines (46)  ',13a10,a10)
      write(120+jper,8895)llmonc(ip,jper,1,13),runchar
 8895 format('Mean from diffuse mines (46)  ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from birds, boats and angling (48) ---------------------------------------
	if ( ip .eq. 16 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6895)(llmonc(ip,jper,1,i),i=1,13),runchar
 6895 format('Mean: birds, boats and angling',13a10,a10)
      write(120+jper,6899)llmonc(ip,jper,1,13),runchar
 6899 format('Mean: birds, boats and angling',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
      
      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
          
*     from headwaters and streams (48) 55555555555555555555555555555555555555555
      if ( ip .eq. 17 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,7995)(llmonc(ip,jper,1,i),i=1,13),runchar
 7995 format('Mean: headwaters and streams  ',13a10,a10)
      write(120+jper,6995)llmonc(ip,jper,1,13),runchar
 6995 format('Mean: headwaters and streams  ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! if ( ip .eq. 17 ) 55555555555555555555555555555555555555555555555
      
*     from diffuse inputs (13) 555555555555555555555555555555555555555555555555
	if ( ip .eq. 18 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,7925)(llmonc(ip,jper,1,i),i=1,13),runchar
 7925 format('Mean from diffuse inputs (13) ',13a10,a10)
      write(120+jper,6925)llmonc(ip,jper,1,13),runchar
 6925 format('Mean from diffuse inputs (13) ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! from diffuse inputs (13) 5555555555555555555555555555555555555555
      
*     from diffuse inputs (15) 555555555555555555555555555555555555555555555555
	if ( ip .eq. 19 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6926)(llmonc(ip,jper,1,i),i=1,13),runchar
 6926 format('Mean from diffuse inputs (15) ',13a10,a10)
      write(120+jper,7926)llmonc(ip,jper,1,13),runchar
 7926 format('Mean from diffuse inputs (15) ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! from diffuse inputs (15) 55555555555555555555555555555555555555555
      
*     from reach diffuse (20) 55555555555555555555555555555555555555555555555555
	if ( ip .eq. 20 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6986)(llmonc(ip,jper,1,i),i=1,13),runchar
 6986 format('Reach diffuse (20)            ',13a10,a10)
      write(120+jper,7986)llmonc(ip,jper,1,13),runchar
 7986 format('Reach diffuse (20)            ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif ! from reach diffuse (20) 555555555555555555555555555555555555555555
      call write line (jper)
      endif ! from reach diffuse (20) 555555555555555555555555555555555555555555
      
*     from gap filling for flows (21) 555555555555555555555555555555555555555555
	if ( ip .eq. 21 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6911)(llmonc(ip,jper,1,i),i=1,13),runchar
 6911 format('Mean from gap filling (21)    ',13a10,a10)
      write(120+jper,7911)llmonc(ip,jper,1,13),runchar
 7911 format('Mean from gap filling (21)    ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif ! from gap filling for flows (21) 5555555555555555555555555555555555
      call write line (jper)
      endif ! from gap filling for flows (21) 5555555555555555555555555555555555
      
*     from gap filling for flows (22) 555555555555555555555555555555555555555555
*	if ( ip .eq. 22 .and. llmon(ip,jper,1,13) .gt. 1.0e-12 ) then
	if ( ip .eq. 22 ) then !  ! from gap filling for flows (22) 55555555555555
      running mean = running mean + llmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6941)(llmonc(ip,jper,1,i),i=1,13),runchar
 6941 format('Mean from gap filling (22)    ',13a10,a10)
      write(120+jper,7941)llmonc(ip,jper,1,13),runchar
 7941 format('Mean from gap filling (22)    ',13a10,a10)
	if ( llmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonc(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonc2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(llmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*llmon(ip,jper,1,13)/C(jper,1))
      endif ! from gap filling for flows (22) 5555555555555555555555555555555555
      call write line (jper)
      endif ! from gap filling for flows (22) 5555555555555555555555555555555555

      endif ! if ( n148 .eq. 1 ) 55555555555555555555555555555555555555555555555

      endif ! if ( zero check .gt. 1.0e-09 )
      endif
 2000 continue 
      write(120+jper,6692)
 6692 format(100('-'))
      
      return
      end

      
      subroutine write line (jper)     
      include 'COM.FOR'
      write(100+jper,1)
    1 format(150('-'),2x,8('-'))
      return
      end

      
      subroutine write out comma separated monthly apportionment (jper)
      include 'COM.FOR'
      dimension xadd(13)
      character *10 Tenchars(13)
      
      do i = 1, 13
      xadd(i) = 0.0
      enddo
      nprap = nprop ! 5555555555555555555555555555555555555555555555555555555555
      if ( n146 .eq. 2 ) nprap = 16 ! 555555555555555555555555555555555555555555
      do 4500 ip = 1, nprap ! 17, 18 and 19 are excluded 55555555555555555555555     
      if ( ip .gt. 1 ) then
      do i = 1, 13
      xadd(i) = xadd(i) + llmon(ip,jper,1,i)
      enddo
      endif

	if ( ip .eq. 1 ) then
      write(110+jper,1290)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 1290 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from all upstream discharges (3 12 5 and 39)',
     &',',a11,13(',',1pe12.4),',',i4,', 3,12, 5,39')
	endif
	if ( ip .eq. 2 ) then
      write(110+jper,2290)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 2290 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from sewage works (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3')
	endif
	if ( ip .eq. 3 ) then
      write(110+jper,3290)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 3290 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from intermittent discharges (12)',',',a11,
     &13(',',1pe12.4),',',i4,',12')
	endif
	if ( ip .eq. 4 ) then
      write(110+jper,4290)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 4290 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from industry (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5')
	endif
	if ( ip .eq. 5 ) then
      write(110+jper,5290)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 5290 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from mine waters (39)',
     &',',a11,13(',',1pe12.4),',',i4,',39')
	endif
	if ( ip .eq. 6 ) then
      write(110+jper,3690)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 3690 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from livestock farming (25)',',',a11,
     &13(',',1pe12.4),',',i4,',25')
      endif
	if ( ip .eq. 7 ) then
      write(110+jper,4690)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 4690 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from arable farming (27)',',',a11,
     &13(',',1pe12.4),',',i4,',27')
      endif
	if ( ip .eq. 8 ) then
      write(110+jper,5690)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 5690 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from highway runoff (29)',
     &',',a11,13(',',1pe12.4),',',i4,',29')
      endif
	if ( ip .eq. 9 ) then
      write(110+jper,6690)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6690 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from urban runoff (31)',
     &',',a11,13(',',1pe12.4),',',i4,',31')
      endif
	if ( ip .eq. 10 ) then
      write(110+jper,6790)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6790 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total ',
     &'load from atmospheric deposition (33)',
     &',',a11,13(',',1pe12.4),',',i4,',33')
      endif
*     from natural background (35) ---------------------------------------------
	if ( ip .eq. 11 ) then
      write(110+jper,6890)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6890 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from natural background (35)',
     &',',a11,13(',',1pe12.4),',',i4,',35')
      endif
	if ( ip .eq. 12 ) then
      write(110+jper,6892)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6892 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from septic tanks (37)    ',',',a11,
     &13(',',1pe12.4),',',i4,',37')
      endif
	if ( ip .eq. 13 ) then
      write(110+jper,6893)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6893 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from Aggregated CSOs (40)    ',',',a11,
     &13(',',1pe12.4),',',i4,',40')
      endif
	if ( ip .eq. 14 ) then
      write(110+jper,6894)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6894 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from Aggregated STWs (42)    ',',',a11,
     &13(',',1pe12.4),',',i4,',42')
      endif
*     from diffuse mines (46) --------------------------------------------------
	if ( ip .eq. 15 ) then
      write(110+jper,6895)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6895 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from total load from Diffuse Mines (46)      ',',',a11,
     &13(',',1pe12.4),',',i4,',46')
      endif
	if ( ip .eq. 16 ) then
      write(110+jper,6195)GIScode(feeture),unamex,rnamex,
     &dname(jper),(llmon(ip,jper,1,i),i=1,13),jxtype
 6195 format(a40,',',a40,',',a20,',','Monthly mean concentrations ',
     &'from load from birds boats and angling (48) ',',',a11,
     &13(',',1pe12.4),',',i4,',48')
      endif
 4500 continue 

      do i = 1, 12
      Tenchars (i) = ' .........'
	enddo
      if ( nobigout .le. 0 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),xadd(i))
	enddo
	endif
      call set format for printout(Tenchars(13),xadd(13))
      write(100+jper,3096)(Tenchars(i),i=1,13)
 3096 format('SUM OF MEAN CONTRIBUTIONS',5x,13a10)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars(i),cmon(jper,1,i))
      enddo
      endif
      call set format for printout(Tenchars(13),C(jper,1))
      write(100+jper,1040)(Tenchars(i),i=1,13)
 1040 format('TOTAL IN-RIVER MEAN ... ',6x,13a10)

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      xxxx = abs (C(jper,1)-xadd(13)) / C(jper,1)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      if ( xxxx .gt. 1.0e-6 ) then
      call set format for printout(Tenchars(i),cmon(jper,1,i)-xadd(i))
      else
      call set format for printout(Tenchars(i),0.0)
      endif
      enddo
      endif
      if ( C(jper,1) .gt. 1.0e-8 ) then
      if ( xxxx .gt. 1.0e-6 ) then
      call set format for printout(Tenchars(13),C(jper,1)-xadd(13))
      else
      call set format for printout(Tenchars(13),0.0)
      endif
      write(100+jper,1140)(Tenchars(i),i=1,13)
 1140 format('MEAN LEFT UNACCOUNTED ..... ',2x,13a10)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      
      if ( detype(jper) .ne. 104 ) then ! exclude dissolved oxygen
      if ( C(jper,1) .gt. 1.0e-6 .and. flow(1) .gt. 1.0e-7 ) then
      xcheck = abs (100.0*(C(jper,1)-xadd(13))/C(jper,1))
      if ( xcheck .gt. 0.01 ) then
	call change colour of text (10) ! bright green
      write( *,1005) xcheck,dname(jper)
      write(09,1005) xcheck,dname(jper)
      call set screen text colour
 1005 format(
     &'* Breakdown differs from measured by',f8.2,' % for ',a11)
      endif
      endif
      endif ! if ( detype(per) .ne. 104 )

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      if ( xxxx .gt. 1.0e-6 ) then
      call set format for printout(Tenchars(i),100.0*(
     &(cmon(jper,1,i)-xadd(i))/cmon(jper,1,i) ))
      else
      call set format for printout(Tenchars(i),0.0)
      endif
      enddo
      endif
      if ( C(jper,1) .gt. 1.0e-8 ) then
      if ( xxxx .gt. 1.0e-6 ) then
      call set format for printout(Tenchars(13),100.0*(
     &(C(jper,1)-xadd(13))/C(jper,1) ))
      else
      call set format for printout(Tenchars(13),0.0)
      endif
      write(100+jper,1240)(Tenchars(i),i=1,13)
 1240 format('% OTHER SOURCES ...',11x,13a10)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      endif ! if ( n148 .eq. 1 ) 55555555555555555555555555555555555555555555555
      endif ! if ( nobigout .le. 0 )
      
*     -------------- finished writing out commma-separated monthly apportionment

      return
      end