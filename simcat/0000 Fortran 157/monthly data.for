*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File: MONTH.FOR ... 4834 lines -------------------------------------------
*     --------------------------------------------------------------------------
*     This file deals with monthly data ----------------------------------------
*     --------------------------------------------------------------------------
*     This file contains 12 sub-routines ---------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... write out monthly river quality (iplace)
*     ...... write out monthly partitions one (iplace, jper)
*     ...... write out monthly partitions two (iplace, jper)
*     ...... calculate monthly summaries of contribution (ip,JDET,Y)
*     ...... calculate monthly summaries of temperature
*     ...... calculate monthly summaries of suspended solids
*     ...... write the monthly data in comma separated form ! 210 and 110 --
*     ...... write the monthly data in tables (jper)
*     ...... write out monthly flows and temperature (iplace)
*     ...... write out the monthly apportionment (jper,iplace)
*     ...... write line (jper)     
*     ...... write out comma separated monthly apportionment (jper)
*     --------------------------------------------------------------------------
      
      subroutine write out monthly river quality (iplace)
*     iplace 1 ... start of reach
*     iplace 3 ... end of reach
*     iplace 2 ... at feature
*     iplace 4 ... upstream of discharge
*     iplace 5 ... downstream of tributary
*     iplace 6 ... downstream of discharge
      include 'COMMON DATA.FOR'
      dimension XL(13),XU(13)
      real lcontot(13)

*     dimension XADD(13)
      character *10 Tenchars (13)
      character *01 G(40)

      if ( munthly structure .eq. 9999 ) return !  no requirement for monthly loads
      
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

      call load calculation ! monthly data -------------------------------------

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
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J), ! dissolved
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J), ! solid 
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
      endif ! dissolved and solid metal
      endif
      enddo
*     --------------------------------------------------------------------------

*     calculate the flow statistics --------------------------------------------
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0))
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      else ! calculate monthly percentiles of river flow

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
      endif
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
      if ( JT(feeture) .eq. 10 .or. JT(feeture) .eq. 45 ) then
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
      if ( ical13 .eq. 1 ) return ! running in calibration mode ----------------

      call write out monthly flows and temperature (iplace)

      do 6612 jper = 1, ndet ! loop on determinands ----------------------------
      if ( QTYPE (jper) .ne. 4) then ! check for excluded determinands ---------

      do imon = 1, 12
      XL(imon) = 0.0
      XU(imon) = 0.0
      enddo
      XL(13) = CL(2,jper) ! retrieve the values of the confidence limits -------
      XU(13) = CL(3,jper) ! retrieve the values of the confidence limits -------

      print check = 0.0 ! initialise check for need to print -------------------
      cprint = 2.0 ! initialise the value of the biggest monthly value ---------
      do 42 imon = 1, 12 ! loop on months ---------- calculate confidence limits
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

      qqq = amax1 ( 0.05, qualn(jper) / 12.0 ) ! calculate the number of samples
      SEM = S/SQRoot3(107280,qqq)
      XL(imon) = amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon) = amax1 (A,(A+t95*SEM))
      
   42 continue ! loop on months -------------------- calculate confidence limits
      
      print check = print check / 12.0

*     write out tables of monthly concentrations ------------------------ Di.MON
      if ( nobigout .le. 0 ) then ! ------------------------------------- Di.MON
      if ( cprint .gt. 1.0 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),cmon(jper,1,i))
      enddo
      endif ! if ( munthly structure .eq. 1 )
      call set format for printout(Tenchars (13),C(jper,1))
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13) ! ------------ Di.MON
 1000 format('Means for ',a11,9x,13a10)
      
      if ( C(jper,1) .gt. 1.0e-8 ) then ! ------------------------------- Di.MON
      if ( munthly structure .eq. 1 ) then ! ---------------------------- Di.MON
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
      
      if ( munthly structure .eq. 1 ) then ! set up print out ------------------
      do i = 1, 13 
      call set format for printout(Tenchars(i),XU(i))
	enddo
	endif ! if ( munthly structure .eq. 1 ) ----------------------------------
      write(100+jper,1006)(Tenchars(i),i=1,13)
 1006 format('Upper confidence limit on mean',13a10)
      call write line (jper)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      
      endif ! if ( cprint .gt. 1.0 )
      endif
*     ----------------------------- written out tables of monthly concentrations
     
      
      
*     write out comma-separated monthly concentrations -------------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,1300)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),C(jper,1),Length of main river, ! -------------------- DDi.CSV
     &(cmon(jper,1,i),i=1,12)!,distp,RQO(jper),MRQO(jper) ! ------------ DDi.CSV
 1300 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'Mean concentration',',',a11, ! ---------------------------------- DDi.CSV
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4), ! --------------- DDi.CSV
     &(',',0pf11.2),(',',1pe12.4),(',',i4)) ! -------------------------- DDi.CSV
      write(210+jper,1301)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),C(jper,2),Length of main river,
     &(cmon(jper,2,i),i=1,12)!,distp
 1301 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      write(210+jper,1305)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),XL(13),Length of main river,(XL(i),i=1,12)!,distp ! -- DDi.CSV
 1305 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower confidence limit for mean',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      write(210+jper,1306)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),XL(13),Length of main river,(XU(i),i=1,12)!,distp ! -- DDi.CSV
 1306 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper confidence limit for mean',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+jper,2300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(cmon(jper,1,i),i=1,12),C(jper,1),jxtype
 2300 format(' ',a40,',',a40,',',a20,',','Mean concentrations for ',',',
     &a11,13(',',1pe12.4),',',i4)
      write(110+jper,2301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(cmon(jper,2,i),i=1,12),C(jper,2),jxtype
 2301 format(' ',a40,',',a40,',',a20,',','Standard deviations for ',
     &'concentrations',',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,2305)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(XL(i),i=1,13),jxtype
 2305 format(' ',a40,',',a40,',',a20,',','Lower confidence limit for ',
     &'mean concentrations',','a11,13(',',1pe12.4),',',i4) ! ------------ Di.CSV
      write(110+jper,2306)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(XU(i),i=1,13),jxtype
 2306 format(' ',a40,',',a40,',',a20,',','Upper confidence limit for ',
     &'mean concentrations',',',a11,13(',',1pe12.4),',',i4) ! ----------- Di.CSV
*     ----------------------- written out comma-separated monthly concentrations
      
      if ( detype (jper) .ge. 900 ) then ! dissolved and solid metal -----------
	call write out monthly partitions two (iplace, jper)
	call write out monthly partitions one (iplace, jper)
      endif ! dissolved and solid metal ----------------------------------------

      !call write out the monthly apportionment (jper,iplace) ! ################
      !call write out comma separated monthly apportionment (jper) ! ###########

*     --------------- finished tables of % contribution of monthly effluent load
*     comma-separated % contribution of monthly effluent load ------------------

*     write comma-seprated breakdown of annual mean concentrations =============

      do im = 1, 13 ! 17, 18 and 19 are excluded 555555555555555555555555
      lcontot(im) = 0.0 ! initialise sums of mean concentrations --------------- 
      enddo

      do 3056 iiip = 1, 23 ! 17, 18 and 19 are excluded 555555555555555555555555
      ip = jorder(iiip) ! sequencing used for output of types of pollution ----- 
      if ( ip .gt. 1 ) then ! add this to the sum of contributions -------------
      do im = 1, 13 ! types 17, 18 and 19 are excluded from the sums -----------
      lcontot(im) = lcontot(im) + lconmon(ip,jper,1,im) !sum mean concentrations 
      enddo ! do im = 1, 13 ----------------------------------------------------
      endif ! if ( ip .gt. 1 ) -------------------------------------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      if ( ip .eq. 1 ) then
      write(210+jper,1296)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 1296 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (3 12 5 and 39)',',',a37,
     &(',',1pe12.4),',','3 12 5 39 60 61',','0pf12.2',,,,',
     &12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
 	if ( ip .eq. 2 ) then
      write(210+jper,1297)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distP
 1297 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (3)',',',a37,
     &(',',1pe12.4),',','3',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
 	if ( ip .eq. 3 ) then
      write(210+jper,1298)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 1298 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (12)',',',a37,
     &(',',1pe12.4),',','12',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
	endif
 	if ( ip .eq. 4 ) then
      write(210+jper,1200)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 1200 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (5)',',',a37,
     &(',',1pe12.4),',','5',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
 	if ( ip .eq. 5 ) then
      write(210+jper,1201)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 1201 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (39)',',',a37,
     &(',',1pe12.4),',39',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
      
  	if ( ip .eq. 28 ) then
      write(210+jper,2801)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 2801 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (60)',',',a37,
     &(',',1pe12.4),',60',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
  	if ( ip .eq. 29 ) then
      write(210+jper,2901)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distp
 2901 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (61)',',',a37,
     &(',',1pe12.4),',61',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
      endif
  	!if ( ip .eq. 17 ) then ! ----------------------- 210 ------------ DDi.CSV
      !write(210+jper,2991)GIScode(feeture),jxtype,unamex,rnamex, ! ---- DDi.CSV
      !&nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
      !&(lconmon(ip,jper,1,i),i=1,12)
      !2991 format(' ',a40,',',i4,',',a40,',',a20,',',
      !&'Contributions to mean concentrations (10)',',',a37,
      !&(',',1pe12.4),',10',','0pf12.2',,,,',12(',',1pe12.4))
      !endif
     
 	if ( ip .eq. 6 ) then
      write(210+jper,1202)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)!,distP
 1202 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (25)',',',a37,
     &(',',1pe12.4),',25',','0pf12.2',,,,',12(',',1pe12.4),
     &(',',0pf11.2)) ! ------------------------------------------------- DDi.CSV
	endif
 	if ( ip .eq. 7 ) then
      write(210+jper,1203)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1203 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (27)',',',a37,
     &(',',1pe12.4),',27',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 8 ) then
      write(210+jper,1204)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1204 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (29)',',',a37,
     &(',',1pe12.4),',29',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 9 ) then
      write(210+jper,1205)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1205 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (31)',',',a37,
     &(',',1pe12.4),',31',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 10 ) then
      write(210+jper,1206)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1206 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (33)',',',a37,
     &(',',1pe12.4),',33',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 11 ) then ! from natural background (35) --------------------
      write(210+jper,1207)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1207 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (35)',',',a37,
     &(',',1pe12.4),',35',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 12 ) then
      write(210+jper,1208)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1208 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (37)',',',a37,
     &(',',1pe12.4),',37',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 13 ) then
      write(210+jper,1209)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1209 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (40)',',',a37,
     &(',',1pe12.4),',40',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 14 ) then
      write(210+jper,1210)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1210 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (42)',',',a37,
     &(',',1pe12.4),',42',','0pf12.2',,,,',12(',',1pe12.4))
	endif
 	if ( ip .eq. 15 ) then ! from diffuse mines (46) -------------------------
      write(210+jper,1211)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1211 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (46)',',',a37,
     &(',',1pe12.4),',46',','0pf12.2',,,,',12(',',1pe12.4))
	endif ! from diffuse mines (46) ------------------------------------------
 	if ( ip .eq. 16 ) then
      write(210+jper,1212)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1212 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (48)',',',a37,
     &(',',1pe12.4),',48',','0pf12.2',,,,',12(',',1pe12.4))
      endif
  
*     ==========================================================================
      if ( ip .eq. 23 ) then
      write(210+jper,1250)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1250 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (50)',',',a37,
     &(',',1pe12.4),',50',','0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 24 ) then
      write(210+jper,1252)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1252 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (52)',',',a37,
     &(',',1pe12.4),',52',','0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 25 ) then
      write(210+jper,1254)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1254 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (54)',',',a37,
     &(',',1pe12.4),',54',','0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 26 ) then
      write(210+jper,1256)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1256 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (56)',',',a37,
     &(',',1pe12.4),',56',','0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 27 ) then
      write(210+jper,1258)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),lconmon(ip,jper,1,13),Length of main river,
     &(lconmon(ip,jper,1,i),i=1,12)
 1258 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Contributions to mean concentrations (58)',',',a37,
     &(',',1pe12.4),',58',','0pf12.2',,,,',12(',',1pe12.4))
      endif
      endif ! 777777777777777777777777777777777777777777777777777777777777777777


      if ( ip .eq. 1 ) then
      write(110+jper,3296)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3296 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (3 12 5 and 39)',',',a37,
     &13(',',1pe12.4),',',i4,', 3 12 5 39 60 61') ! --------------------- Di.CSV
      endif
      
 	if ( ip .eq. 2 ) then
      write(110+jper,3297)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3297 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (3)',',',a37,
     &13(',',1pe12.4),',',i4,', 3') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 3 ) then ! added by sewage effluents (3)
      write(110+jper,3298)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3298 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (12)',',',a37,
     &13(',',1pe12.4),',',i4,',12') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 4 ) then
      write(110+jper,3200)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3200 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (5)',',',a37,
     &13(',',1pe12.4),',',i4,', 5') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 5 ) then
      write(110+jper,3201)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3201 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (39)',',',a37,
     &13(',',1pe12.4),',',i4,',39') ! ----------------------------------- Di.CSV
      endif

      if ( ip .eq. 28 ) then
      write(110+jper,8233)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 8233 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (60)',',',a37,
     &13(',',1pe12.4),',',i4,',60') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 29 ) then
      write(110+jper,3232)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3232 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (61)',',',a37,
     &13(',',1pe12.4),',',i4,',61') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 17 ) then ! ---------------- % monthly --------------  Di.CSV
*     write(110+jper,3233)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
*    &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3233 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (10)',',',a37,
     &13(',',1pe12.4),',',i4,',10') ! ----------------------------------- Di.CSV
      endif

      if ( ip .eq. 6 ) then ! diffuse pollution from livestock (25)
      write(110+jper,3202)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3202 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (25)',',',a37,
     &13(',',1pe12.4),',',i4,',25') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 7 ) then ! diffuse pollution from arable (27)
      write(110+jper,3203)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3203 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (27)',',',a37,
     &13(',',1pe12.4),',',i4,',27') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 8 ) then
      write(110+jper,3204)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3204 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (29)',',',a37,
     &13(',',1pe12.4),',',i4,',29') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 9 ) then
      write(110+jper,3205)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3205 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (31)',',',a37,
     &13(',',1pe12.4),',',i4,',31') ! ----------------------------------- Di.CSV
	  endif
      
      if ( ip .eq. 10 ) then
      write(110+jper,3206)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3206 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (33)',',',a37,
     &13(',',1pe12.4),',',i4,',33') ! ----------------------------------- Di.CSV
      endif
      
 	if ( ip .eq. 11 ) then ! from natural background (35) --------------------
      write(110+jper,3207)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3207 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (35)',',',a37,
     &13(',',1pe12.4),',',i4,',35') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 12 ) then ! from septic tanks (37) ------------------- Di.CSV
      write(110+jper,3208)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------- Di.CSV
 3208 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (37)',',',a37,
     &13(',',1pe12.4),',',i4,',37') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 13 ) then ! from aggregate CSOs (40) ----------------- Di.CSV
      write(110+jper,3209)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------- Di.CSV
 3209 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (40)',',',a37,
     &13(',',1pe12.4),',',i4,',40') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 14 ) then ! from aggregate STWs (42) ----------------- Di.CSV
      write(110+jper,3210)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------- Di.CSV
 3210 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (42)',',',a37,
     &13(',',1pe12.4),',',i4,',42') ! ----------------------------------- Di.CSV
      endif ! from aggregate STWs (42) ---------------------------------- Di.CSV
      
      if ( ip .eq. 15 ) then ! from diffuse mines (46) ------------------ Di.CSV
      write(110+jper,3211)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------- Di.CSV
 3211 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (46)',',',a37,
     &13(',',1pe12.4),',',i4,',46') ! ----------------------------------- Di.CSV
      endif ! from diffuse mines (46) ----------------------------------- Di.CSV
      
      if ( ip .eq. 16 ) then ! from birds, boats and angling (48) ------- Di.CSV
      write(110+jper,3212)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3212 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (48)',',',a37,
     &13(',',1pe12.4),',',i4,',48') ! ----------------------------------- Di.CSV
      endif ! from birds, boats and angling (48) ------------------------ Di.CSV
  
      if ( ip .eq. 23 ) then
      write(110+jper,3250)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3250 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (50)',',',a37,
     &13(',',1pe12.4),',',i4,',50') ! ----------------------------------- Di.CSV
      endif
      
      if ( ip .eq. 24 ) then
      write(110+jper,3252)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3252 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (52)',',',a37,
     &13(',',1pe12.4),',',i4,',52') ! ----------------------------------- Di.CSV
      endif
      
 	if ( ip .eq. 25 ) then
      write(110+jper,3254)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3254 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (54)',',',a37,
     &13(',',1pe12.4),',',i4,',54') ! ----------------------------------- Di.CSV
      endif
      
 	if ( ip .eq. 26 ) then
      write(110+jper,3256)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3256 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (56)',',',a37,
     &13(',',1pe12.4),',',i4,',56') ! ----------------------------------- Di.CSV
      endif
      
 	if ( ip .eq. 27 ) then
      write(110+jper,3558)GIScode(feeture),unamex,rnamex,nameprop(ip), !  Di.CSV
     &(lconmon(ip,jper,1,i),i=1,13),jxtype ! ---------------------------  Di.CSV
 3558 format(' ',a40,',',a40,',',a20,',',
     &'Contributions to mean concentrations (58)',',',a37,
     &13(',',1pe12.4),',',i4,',58') ! ----------------------------------- Di.CSV
      endif

*     ==========================================================================

 3056 continue
      
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,8258)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &lcontot(13),Length of main river,(lcontot(i),i=1,12)
 8258 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Total of above contributions to mean concentrations',',',
     &'All forms of pollution',
     &(',',1pe12.4),',9090',','0pf12.2',,,,',12(',',1pe12.4))
     
      write(210+jper,8260)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &C(jper,1)-lcontot(13),Length of main river, ! -------------------- DDi.CSV
     &((cmon(jper,1,i)-lcontot(i)),i=1,12) ! --------------------------- DDi.CSV
 8260 format(' ',a40,',',i4,',',a40,',',a20,',', ! --------------------- DDi.CSV
     &'Remaining contributions to mean concentrations',',', ! ---------- DDi.CSV
     &'Headwaters and streams etc', ! ---------------------------------- DDi.CSV
     &(',',1pe12.4),',2 10 13 15,'0pf12.2',,,,',12(',',1pe12.4)) ! ----- DDi.CSV
     
      xxx = 0.0
      if ( C(jper,1) .gt. 0.0000001 ) then
      write(210+jper,8261)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &100.0*(C(jper,1)-lcontot(13))/C(jper,1),Length of main river, ! -- DDi.CSV
     &(100.0*(cmon(jper,1,i)-lcontot(i))/cmon(jper,1,i),i=1,12) ! ------ DDi.CSV
 8261 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'% Remaining contributions to mean concentrations',',',
     &'Headwaters and streams etc',
     &(',',0pf12.2,'%'),',2 10 13 15,'0pf12.2',,,,',12(',',0pf12.2,'%'))!DDi.CSV
      else
      write(210+jper,8261)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &xxx,Length of main river,(xxx,i=1,12) ! -------------------------- DDi.CSV
      endif
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
 
      write(110+jper,8558)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(lcontot(i),i=1,13),jxtype ! -------------------------------------- Di.CSV
 8558 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'Total of above contributions to mean concentrations',',', ! ------ Di.CSV
     &'All forms of pollution', ! --------------------------------------- Di.CSV
     &13(',',1pe12.4),',',i4,',800') ! ---------------------------------- Di.CSV

      write(110+jper,8560)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &((cmon(jper,1,i)-lcontot(i)),i=1,13),jxtype ! --------------------- Di.CSV
 8560 format(' ',a40,',',a40,',',a20,',', ! ----------------------------- Di.CSV
     &'Remaining contributions to mean concentrations',',', ! ----------- Di.CSV
     &'Headwaters and streams etc', ! ----------------------------------- Di.CSV
     &13(',',1pe12.4),',',i4,',802') ! ---------------------------------- Di.CSV

      xxx = 0.0
      if ( C(jper,1) .gt. 0.0000001 ) then
      write(110+jper,8561)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(100.0*(cmon(jper,1,i)-lcontot(i))/cmon(jper,1,i),i=1,13), ! ------ Di.CSV
     &jxtype ! ---------------------------------------------------------- Di.CSV
 8561 format(' ',a40,',',a40,',',a20,',' ! ---------------------------------- Di.CSV
     &'% Remaining contributions to mean concentrations',',', ! --------- Di.CSV
     &'Headwaters and streams etc', ! ----------------------------------- Di.CSV
     &13(',',0pf12.2),',',i4,',803') ! ---------------------------------- Di.CSV
      else
      write(110+jper,8561)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(xxx,i=1,13),jxtype ! --------------------------------------------- Di.CSV
      endif

*     finished comma-seprated breakdown of annual mean concentrations ==========
*     ==========================================================================


*     calculate days exceeding annual mean etc ---------------------------------
      if ( QTYPE (jper) .ne. 4 ) then
      do im = 1, n13
      exceedences50 (jper, im) = 0.0
      exceedences90 (jper, im) = 0.0
      exceedences95 (jper, im) = 0.0
      enddo
*     non-partitioned (ordinary) chemicals) ------------------------------------
      if ( detype (jper) .lt. 900 ) then

      do is = 1, NS ! loop on shots --------------------------------------------
      imonth = qmonth (is) ! set the month for this shot -----------------------

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
      enddo ! do im = 1, 12 ++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      exceedences50 (jper,13) = 100.0 * exceedences50 (jper,13)/NS
      exceedences90 (jper,13) = 100.0 * exceedences90 (jper,13)/NS
      exceedences95 (jper,13) = 100.0 * exceedences95 (jper,13)/NS

*     write out tables of exceedence +++++++++++++++++++++++++++++++++++++++++++
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( exceedences50(jper,13) .gt. 1.0e-12 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences50 (jper,i))
      enddo
      endif
      call set format for printout(Tenchars(13),exceedences50(jper,13))
      write(100+jper,4001)(Tenchars (i), i = 1,13)
 4001 format(30('-'),13(2x,8('-'))/'% days exceeding mean conc ...',
     &13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences90 (jper,i))
      enddo
      endif ! set up print out
      call set format for printout(Tenchars(13),exceedences90(jper,13))
      write(100+jper,4101)(Tenchars (i), i = 1,13)
 4101 format('% exceeding annual Q90 conc   ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),exceedences95 (jper,i))
      enddo
      endif
      call set format for printout(Tenchars(13),exceedences95(jper,13))
      write(100+jper,4201)(Tenchars (i), i = 1,13)
 4201 format('% exceeding annual Q95 conc   ',13a10)
*     call write line (jper)
      endif ! if ( exceedences50(jper,13) .gt. 1.0e-12 )
      endif
*     --------------------------------------------------------------------------
      endif ! if ( QTYPE (jper) .ne. 4 )

*     write out comma-separated tables of exceedence ---------------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,4401)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),exceedences50(jper,13),Length of main river,
     &(exceedences50 (jper,im), im = 1,12)
 4401 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'% days exceeding annual mean concentration',',',a11,
     &(',',0pf12.2,'%'),',',','0pf12.2',,,,',12(',',0pf12.2,'%')) ! ---- DDi.CSV
      write(210+jper,4402)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),exceedences90(jper,13),Length of main river,
     &(exceedences90 (jper,im), im = 1,12)
 4402 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'% days exceeding annual 90-percentile',',',a11,
     &(',',0pf12.2,'%'),',',','0pf12.2',,,,',12(',',0pf12.2,'%')) ! ---- DDi.CSV
      write(210+jper,4403)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),exceedences95(jper,13),Length of main river,
     &(exceedences95 (jper,im), im = 1,12)
 4403 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'% days exceeding annual 95-percentile',',',a11,
     &(',',0pf12.2,'%'),',',','0pf12.2',,,,',12(',',0pf12.2,'%')) ! ---- DDi.CSV
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      write(110+jper,7401)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(exceedences50(jper,im), im = 1,13),jxtype
 7401 format(' ',a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual mean concentration',',',a11,13(',',0pf12.2),',',i4, ! ----- Di.CSV
     &',804')
      write(110+jper,7402)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(exceedences90(jper,im), im = 1,13),jxtype
 7402 format(' ',a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual 90-percentile',',',a11,13(',',0pf12.2),',',i4,! ----------- Di.CSV
     &',805')
      write(110+jper,7403)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(exceedences95(jper,im), im = 1,13),jxtype
 7403 format(' ',a40,',',a40,',',a20,',','% days in months exceeding ',
     &'annual 95-percentile',',',a11,13(',',0pf12.2),',',i4, ! ---------- Di.CSV
     &',806')

      call write out comma separated monthly apportionment (jper)

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
 2004 format(30('-'),13(2x,8('-'))/'Mean loads for ',a11,4x,13a10)
      if ( lmon(jper,1,i) .gt. 1.0 e-12 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),lmon(jper,2,i))
      enddo
      endif
      call set format for printout(Tenchars (13),lmon(jper,2,13))
      write(100+jper,2001)(Tenchars (i),i=1,13)
 2001 format('Standard deviations of load   ',13A10)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),XL(i))
      enddo
      endif
      call set format for printout(Tenchars (13),XL(13))
      write(100+jper,2005)(Tenchars (i),i=1,13)
 2005 format('Lower limits on loads         ',13a10)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),XU(i))
      enddo
      endif
      call set format for printout(Tenchars (13),XU(13))
      write(100+jper,2006)(Tenchars (i),i=1,13)
 2006 format('Upper limits on loads         ',13A10)
      call write line (jper)
      endif ! if ( lmon(jper,1,i) .gt. 1.0 e-12 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE (jper) .ne. 4 )

*     finished comma-separated monthly loads -----------------------------------

      call write the monthly data in tables (jper)
      call write the monthly data in comma separated form (jper,iplace)

*     write out comma-separated monthly loads ----------------------------------
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,2404)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),lmon(jper,1,13),Length of main river,
     &(lmon(jper,1,i),i=1,12)
 2404 format(' ',a40,',',i4,',',a40,',',a20,',','Mean loads for:',',',
     &a11,(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,2401)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),lmon(jper,2,13),Length of main river,
     &(lmon(jper,2,i),i=1,12)
 2401 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation for loads:',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,2501)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),XL(13),Length of main river,(XL(i),i=1,12)
 2501 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower confidence limit on mean loads:',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,2506)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),XU(13),Length of main river,(XU(i),i=1,12)
 2506 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper confidence limit on mean loads:',',',a11,
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      endif ! 777777777777777777777777777777777777777777777777777777777777777777
      
      write(110+jper,3404)GIScode(feeture),unamex,
     &rnamex,dname(jper),(lmon(jper,1,i),i=1,13),jxtype
 3404 format(' ',a40,',',a40,',',a20,',','Mean loads for:',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3401)GIScode(feeture),unamex,
     &rnamex,dname(jper),(lmon(jper,2,i),i=1,13),jxtype
 3401 format(' ',a40,',',a40,',',a20,',',
     &'Standard deviations of loads:',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3501)GIScode(feeture),unamex,
     &rnamex,dname(jper),(XL(i),i=1,13),jxtype
 3501 format(' ',a40,',',a40,',',a20,',',
     &'Lower confidence limits on mean loads:',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3506)GIScode(feeture),unamex,
     &rnamex,dname(jper),(XU(i),i=1,13),jxtype
 3506 format(' ',a40,',',a40,',',a20,',',
     &'Upper confidence limits on mean loads:',
     &',',a11,13(',',1pe12.4),',',i4)
*     --------------------------------------------------------------------------

      endif ! if ( QTYPE (jper) .ne. 4) then ! check for excluded determinands -

 6612 continue
*     end of loop on determinands ==========================================6612

      return
      end





*     --------------------------------------------------------------------------
      subroutine write out monthly partitions one (iplace, jper)
      include 'COMMON DATA.FOR'
	dimension XL(13),XU(13)
      character *10 Tenchars(13)

*     loop on months -----------------------------------------------------------
      t95 = errx ( 0.95 )
      print check = 0.0
      cprint = 2.0
      do 42 imon = 1, 13 ! =====================================================
      cprint = amax1 (cprint, p1mon(jper,1,imon))

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, p1mon(jper,1,imon) ) ! dissolved metal
      XL(imon) = A
      XU(imon) = A
      print check = print check + A
*     check for zero mean ------------------------------------------------------
      if (A .lt. 1.0E-08) p1mon(jper,2,imon) = 0.0 ! dissolved metal
*     check for zero standard deviation ----------------------------------------
      if (p1mon(jper,2,imon) .lt. 1.0E-08) goto 42 ! dissolved metal
*     set the standard deviation -----------------------------------------------
      S = p1mon(jper,2,imon) ! dissolved metal
*     calculate confidence limits ----------------------------------------------
      qqq = amax1 ( 0.05, qualn(jper) / 12.0 )
      if ( imon .eq. 13 ) qqq = qualn(jper)
      SEM=S/SQRoot(107282,qqq)
      XL(imon)=amax1 (0.0, amin1 (A,(A-t95*SEM))) ! dissolved metal
      XU(imon)=amax1 (A,(A+t95*SEM)) ! dissolved metal
   42 continue ! do 42 imon = 1, 13 ============================================
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
      if ( nobigout .le. 0 ) then ! ============================ dissolved metal
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
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,1300)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 1300 format(' ',a40,',',i4,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(210+jper,1301)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 1301 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation for ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1305)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1305 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower limit on dissolved ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1306)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1306 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper limit on dissolved ',',',a11,13(',',1pe12.4),',',i4)
	endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(210+jper,1400)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 1400 format(' ',a40,',',i4,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(210+jper,1401)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 1401 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation for ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1405)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1405 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower limit on dissolved ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1406)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1406 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper limit on dissolved ',',',a11,13(',',1pe12.4),',',i4)
      endif
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      if ( iplace .ne. 3 ) then
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 3300 format(' ',a40,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 3301 format(' ',a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3305)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 3305 format(' ',a40,',',a40,',',a20,',','Lower limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3306)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 3306 format(' ',a40,',',a40,',',a20,',','Upper limit on dissolved ',
     &',',a11,13(',',1pe12.4),',',i4)
	endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(110+jper,3400)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture)
 3400 format(' ',a40,',',a40,',',a20,',','PARTS for ',',',a11, ! -------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      write(110+jper,3401)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture)
 3401 format(' ',a40,',',a40,',',a20,',','Standard deviation for ', ! --- Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3405)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XL(i),i=1,13),jt(feeture)
 3405 format(' ',a40,',',a40,',',a20,',','Lower limit on dissolved ', ! - Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3406)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XU(i),i=1,13),jt(feeture)
 3406 format(' ',a40,',',a40,',',a20,',','Upper limit on dissolved ', ! - Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      endif

      endif
*     --------------------------------------------------------------------------

      return
      end



*     --------------------------------------------------------------------------
      subroutine write out monthly partitions two (iplace, jper)
      include 'COMMON DATA.FOR'
      dimension XL(13),XU(13)
      character *10 Tenchars(13)

*     loop on months -----------------------------------------------------------
      t95 = errx ( 0.95 )
      print check = 0.0 ! solid metal - initialise check for need to print
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
          
      call set format for printout(Tenchars(13),cpart2(jper,1)) ! -------- solid
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,1,i)) ! -----=-- solid
      enddo
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Solid for ',a11,9x,13a10)
 
      call set format for printout(Tenchars(13),cpart2(jper,2)) ! ----=--- solid
      do i = 1, 12
      call set format for printout(Tenchars(i),p2mon(jper,2,i)) ! -------- solid
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

      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      if ( iplace .ne. 3 ) then
      write(210+jper,1300)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 1300 format(' ',a40,',',i4,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(210+jper,1301)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 1301 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation for ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1305)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1305 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower limit on solid ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1306)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1306 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper limit on solid ',',',a11,13(',',1pe12.4),',',i4)
      endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(210+jper,1400)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 1400 format(' ',a40,',',i4,',',a40,',',a20,',','PARTS for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1401)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 1401 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation for ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1405)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 1405 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Lower limit on solid ',',',a11,13(',',1pe12.4),',',i4)
      write(210+jper,1406)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 1406 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Upper limit on solid ',',',a11,13(',',1pe12.4),',',i4)
      endif
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      if ( iplace .ne. 3 ) then
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 3300 format(' ',a40,',',a40,',',a20,',','PARTS for ',',',a11,
     &13(',',1pe12.4),',',i4)
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,
     &dname(jper),(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 3301 format(' ',a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3305)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XL(i),i=1,13),jt(feeture)
 3305 format(' ',a40,',',a40,',',a20,',','Lower limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
      write(110+jper,3306)GIScode(feeture),unamex,rnamex,
     &dname(jper),(XU(i),i=1,13),jt(feeture)
 3306 format(' ',a40,',',a40,',',a20,',','Upper limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4)
      endif

      if ( iplace .eq. 3 ) then
      unamex = 'End of Reach'
      write(110+jper,3400)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture)
 3400 format(' ',a40,',',a40,',',a20,',','PARTS for ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3401)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 3401 format(' ',a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3405)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XL(i),i=1,13),jt(feeture)
 3405 format(' ',a40,',',a40,',',a20,',','Lower limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3406)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XU(i),i=1,13),jt(feeture)
 3406 format(' ',a40,',',a40,',',a20,',','Upper limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      endif



      endif
*     --------------------------------------------------------------------------

      return
      end




      subroutine calculate monthly summaries of contribution (ip,JDET,Y)
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do i = 1, n13
      loadmon(ip,JDET,1,i) = 0.0
      loadmon(ip,JDET,2,i) = 0.0
      enddo

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13 ! loop on the months --------------------------------
      CM = 0.0 ! initialise mean concentration
      CS=0.0 ! standard deviation
      N2=0   ! and number of shots in the month
      CLM = 0.0      
      CLS = 0.0      

      do is = 1, NS ! loop on all shots ========================================
      imon = qmonth (is) ! set the month for this particular shot --------------
      if ( jmon .lt. 13 ) then ! we are dealing with individual months ---------
      if ( imon .eq. jmon ) then ! select month mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
      N2 = N2 + 1
      CLM = CLM + Y(IS) * FMS(IS) ! sum the loads ------------------------------
      CLS = CLS + Y(IS) * FMS(IS) * Y(IS) * FMS(IS)
      endif ! if ( imon .eq. jmon ) mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      else ! we are dealing with the whole year --------------------------------
      CM = CM + Y(is)
      CS = CS + Y(is)*Y(is)
      N2 = N2 + 1
      CLM = CLM + Y(IS) * FMS(IS) ! sum the loads ------------------------------
      CLS = CLS + Y(IS) * FMS(IS) * Y(IS) * FMS(IS)
      endif
      enddo ! do is = 1, NS ! loop on all shots ================================

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/N2)/(N2-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 CS=SQRoot(1599,CS)
    6 CM=CM/N2
      
      CLM2 = CLM
      CLM = CLM /N2
      
      if (CLM .gt. 1.0E-10) goto 15
      CLS=0.0
      goto 16
   15 CLS=(CLS-CLM2*CLM2/N2)/(N2-1)
      if (CLS .gt. 1.0E-20) goto 19
      CLS=0.0
      goto 16
   19 CLS=SQRoot(1599,CLS)
   16 continue
      
      loadmon(ip,JDET,1,jmon) = amax1 ( 0.0, CLM )
      loadmon(ip,JDET,2,jmon) = CLS
      
      lconmon(ip,JDET,1,jmon) = amax1 ( 0.0, CM )
      lconmon(ip,JDET,2,jmon) = CS
      
   88 continue ! do 88 jmon = j1, n13 ... loop on the months

      return
      end






      subroutine calculate monthly summaries of temperature
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do i = 1, NS
      Y(i) = BMS (1,i)
      enddo

      do i = 1, n13
      tmon(1,i) = 0.0 ! monthly summaries of temperature
      tmon(2,i) = 0.0 ! monthly summaries of temperature
      enddo

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13

      CM = 0.0
      CS = 0.0
      N2 = 0.0

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

      tmon(1,jmon) = amax1 ( 0.0, CM ) ! mean monthly temperature
      tmon(2,jmon) = CS ! standard deviation for temperature

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

      BC(1,1) = amax1 ( 0.0, CM ) ! mean temperature
      BC(1,2) = CS ! standard deviation for temperature

      return
      end




      subroutine calculate monthly summaries of suspended solids
      include 'COMMON DATA.FOR'
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

      BC(2,1) = amax1 ( 0.0, CM ) ! mean suspended solids
      BC(2,2) = CS ! standard deviation for suspended solids

      return
      end




      subroutine write the monthly data in comma separated form ! 210 and 110 --
     &(jper,ipt)
      include 'COMMON DATA.FOR'

      unamex = uname(feeture)
      if ( ipt .eq. 3 ) unamex = 'End of Reach  '
      if ( ipt .eq. 1 ) unamex = 'Start of Reach'
      !rnamex = rname (IREACH)
      !if ( ipt .eq. 4 ) rnamex = 'Upstream of works'

*     net loads from discharges (3 12 5 and 39) *************************TELODE2
      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,9188)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(1),TELODE2(jper,1),Length of main river,
     &(TELODE2(jper,J13),J13=2,N13)
 9188 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from all types of discharges (3 12 5 39 60 and 61)',
     &',',a37,(',',1pe12.4),',','3 12 5 39 60 61',
     &','0pf12.2',,,,',12(',',1pe12.4))
*     net load from sewage works (3) ***********************************T03LOAD2
      write(210+jper,9928)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(2),T03LOAD2(jper,1),Length of main river,
     &(T03LOAD2(jper,J13),J13=2,N13)
 9928 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Sewage Works (3)',',',a37,
     &(',',1pe12.4),',','3',','0pf12.2',,,,',12(',',1pe12.4))
*     net load from intermittent discharges (12) ***********************T12LOAD2
      write(210+jper,9929)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(3),T12LOAD2(jper,1),Length of main river,
     &(T12LOAD2(jper,J13),J13=2,N13)
 9929 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Intermittent Discharges (12)',',',a37,
     &(',',1pe12.4),',12',','0pf12.2',,,,',12(',',1pe12.4))
*     industrial discharges (5) ****************************************T05LOAD2
      write(210+jper,9951)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(4),T05LOAD2(jper,1),Length of main river,
     &(T05LOAD2(jper,J13),J13=2,N13)
 9951 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Industrial Discharges (5)',',',a37,
     &(',',1pe12.4),', 5',','0pf12.2',,,,',12(',',1pe12.4))
*     mine waters (39) *************************************************T39LOAD2
      write(210+jper,9730)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(5),T39LOAD2(jper,1),Length of main river,
     &(T39LOAD2(jper,J13),J13=2,N13)
 9730 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Mine Waters (39)',',',a37,
     &(',',1pe12.4),',39',','0pf12.2',,,,',12(',',1pe12.4))
*     Other Point Sources (60) *****************************************T60LOAD2
      write(210+jper,9630)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(28),T60LOAD2(jper,1),Length of main river,
     &(T60LOAD2(jper,J13),J13=2,N13)
 9630 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from "Other" Point Sources (60)',',',a37,
     &(',',1pe12.4),',60',','0pf12.2',,,,',12(',',1pe12.4))
*     private wastewaters (61) *****************************************T61LOAD2
      write(210+jper,9230)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(29),T61LOAD2(jper,1),Length of main river,
     &(T61LOAD2(jper,J13),J13=2,N13)
 9230 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Private Wastewaters (61)',',',a37,
     &(',',1pe12.4),',61',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9909)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(18),T13LOAD2(jper,1),Length of main river,
     &(T13LOAD2(jper,J13),J13=2,N13)
 9909 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Diffuse Inflow (river-type) (13)',',',a37,
     &(',',1pe12.4),',13',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9910)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(6),T25LOAD2(jper,1),Length of main river,
     &(T25LOAD2(jper,J13),J13=2,N13)
 9910 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Livestock (25)',',',a37,
     &(',',1pe12.4),',25',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9911)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(7),T27LOAD2(jper,1),Length of main river,
     &(T27LOAD2(jper,J13),J13=2,N13)
 9911 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Arable Land (27)',',',a37,
     &(',',1pe12.4),',27',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9912)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(8),T29LOAD2(jper,1),Length of main river,
     &(T29LOAD2(jper,J13),J13=2,N13)
 9912 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Highways (29)',',',a37,
     &(',',1pe12.4),',29',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9913)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(9),T31LOAD2(jper,1),Length of main river,
     &(T31LOAD2(jper,J13),J13=2,N13)
 9913 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Urban Land (31)',',',a37,
     &(',',1pe12.4),',31',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9914)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(10),T33LOAD2(jper,1),Length of main river,
     &(T33LOAD2(jper,J13),J13=2,N13)
 9914 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Atmospheric Deposition (33)',',',a37,
     &(',',1pe12.4),',33',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9915)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(11),T35LOAD2(jper,1),Length of main river,
     &(T35LOAD2(jper,J13),J13=2,N13)
 9915 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Natural Background (35)',',',a37,
     &(',',1pe12.4),',35',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9916)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(12),T37LOAD2(jper,1),Length of main river,
     &(T37LOAD2(jper,J13),J13=2,N13)
 9916 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Septic Tanks (37)',',',a37,
     &(',',1pe12.4),',37',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9116)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(13),T40LOAD2(jper,1),Length of main river,
     &(T40LOAD2(jper,J13),J13=2,N13)
 9116 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from diffuse pollution from Aggregated CSOs (40)',
     &',',a37,(',',1pe12.4),',40',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9922)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(14),T42LOAD2(jper,1),Length of main river,
     &(T42LOAD2(jper,J13),J13=2,N13)
 9922 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load specified as Aggregated STWs (42)',',',a37,
     &(',',1pe12.4),',42',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9925)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(15),T46LOAD2(jper,1),Length of main river,
     &(T46LOAD2(jper,J13),J13=2,N13)
 9925 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Diffuse Mines (46)',',',a37,
     &(',',1pe12.4),',46',','0pf12.2',,,,',12(',',1pe12.4))
     
      write(210+jper,9225)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(16),T48LOAD2(jper,1),Length of main river,
     &(T48LOAD2(jper,J13),J13=2,N13)
 9225 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load specified as Birds boats and angling (48)',',',a37,
     &(',',1pe12.4),',48',','0pf12.2',,,,',12(',',1pe12.4))
    
      write(210+jper,9505)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(23),T50LOAD2(jper,1),Length of main river,
     &(T50LOAD2(jper,J13),J13=2,N13)
 9505 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from sources specified by User (50)',',',a37,
     &(',',1pe12.4),',50',','0pf12.2',,,,',12(',',1pe12.4))
     
      write(210+jper,9525)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(24),T52LOAD2(jper,1),Length of main river,
     &(T52LOAD2(jper,J13),J13=2,N13)
 9525 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from sources specified by User (52)',',',a37,
     &(',',1pe12.4),',52',','0pf12.2',,,,',12(',',1pe12.4))
     
      write(210+jper,9545)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(25),T54LOAD2(jper,1),Length of main river,
     &(T54LOAD2(jper,J13),J13=2,N13)
 9545 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from sources specified by User (54)',',',a37,
     &(',',1pe12.4),',54',','0pf12.2',,,,',12(',',1pe12.4))
      
      write(210+jper,9565)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(26),T56LOAD2(jper,1),Length of main river,
     &(T56LOAD2(jper,J13),J13=2,N13)
 9565 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from sources specified by User (56)',',',a37,
     &(',',1pe12.4),',45',','0pf12.2',,,,',12(',',1pe12.4))

      write(210+jper,9585)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(27),T58LOAD2(jper,1),Length of main river,
     &(T58LOAD2(jper,J13),J13=2,N13)
 9585 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from sources specified by User (58)',',',a37,
     &(',',1pe12.4),',58',','0pf12.2',,,,',12(',',1pe12.4))

      nameprop(17) = 'Headwaters (10)'
      nameprop(18) = 'Diffuse input (13)'
      nameprop(19) = 'Diffuse input (15)'
      nameprop(20) = 'Reach diffuse'
      nameprop(21) = 'Gap filling of flows'
      nameprop(22) = 'Gap filling of quality'

      write(210+jper,9917)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(19),T15LOAD2(jper,1),Length of main river,
     &(T15LOAD2(jper,J13),J13=2,N13)
 9917 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Diffuse Inflow (effluent-type) (15)',',',a37,
     &(',',1pe12.4),',15',','0pf12.2',,,,',12(',',1pe12.4))
     
      write(210+jper,9180)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(17),TRLODE2(jper,1),Length of main river,
     &(TRLODE2(jper,J13),J13=2,N13)
 9180 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Boundaries and Streams (2 and 10)',',',a37,
     &(',',1pe12.4),',','2 10 9',','0pf12.2',,,,',12(',',1pe12.4))
*     --------------------------------------------------------------------------
      write(210+jper,9181)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(20),TDDLOAD2(jper,1),Length of main river,
     &(TDDLOAD2(jper,J13),J13=2,N13)
 9181 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load from Diffuse Inflow (reach-based)',',',a37,
     &(',',1pe12.4),',500',','0pf12.2',,,,',12(',',1pe12.4))
*     --------------------------------------------------------------------------
      write(210+jper,9182)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TNLOADUP2(jper,1),Length of main river,
     &(TNLOADUP2(jper,J13),J13=2,N13)
 9182 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load added by natural purification',',',
     &(',',1pe12.4),',600',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9186)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(21),TILOADUP2(jper,1),Length of main river,
     &(TILOADUP2(jper,J13),J13=2,N13)
 9186 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load added by flow gap filling',',',a37,
     &(',',1pe12.4),',700',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9184)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,nameprop(22),TALOADUP2(jper,1),Length of main river,
     &(TALOADUP2(jper,J13),J13=2,N13)
 9184 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net load added by quality gap filling',',',a37,
     &(',',1pe12.4),',800',','0pf12.2',,,,',12(',',1pe12.4))
      

      write(210+jper,9919)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TGLODE2(jper,1),Length of main river,
     &(TGLODE2(jper,J13),J13=2,N13)
 9919 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Net TOTAL of all loads',',',
     &(',',1pe12.4),',999',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9918)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TBLOAD2(jper,1),Length of main river,
     &(TBLOAD2(jper,J13),J13=2,N13)
 9918 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load lost to abstractions (7)',',',
     &(',',1pe12.4),',','7 18 19',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9183)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TNLOADDN2(jper,1),Length of main river,
     &(TNLOADDN2(jper,J13),J13=2,N13)
 9183 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load lost to natural purification',',',
     &(',',1pe12.4),',601',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9187)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TILOADDN2(jper,1),Length of main river,
     &(TILOADDN2(jper,J13),J13=2,N13)
 9187 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load lost by flow gap filling',',',
     &(',',1pe12.4),',701',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9185)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TALOADDN2(jper,1),Length of main river,
     &(TALOADDN2(jper,J13),J13=2,N13)
 9185 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Load lost by quality gap filling',
     &',',(',',1pe12.4),',801',','0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,9195)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,TLOSSES2(jper,1),Length of main river,
     &(TLOSSES2(jper,J13),J13=2,N13)
 9195 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD REMOVED',
     &',',(',',1pe12.4),',802',','0pf12.2',,,,',12(',',1pe12.4))

*     **************************************************************************
      write(210+jper,9948)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T03LOAD1(jper,1),Length of main river,
     &(T03LOAD1 (jper,J13),J13=2,N13)
 9948 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL SEWAGE WORKS (3)',',',
     &(',',1pe12.4),', 3',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      write(210+jper,9949)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T12LOAD1(jper,1),Length of main river,
     &(T12LOAD1 (jper,J13),J13=2,N13)
 9949 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL INTERMITTENT DISCHARGES (12)',',',
     &(',',1pe12.4),',12',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      write(210+jper,9950)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T05LOAD1(jper,1),Length of main river,
     &(T05LOAD1 (jper,J13),J13=2,N13)
 9950 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL INDUSTRIAL DISCHARGES (5)',',',
     &(',',1pe12.4),',5',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      write(210+jper,9989)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T39LOAD1(jper,1),Length of main river,
     &(T39LOAD1 (jper,J13),J13=2,N13)
 9989 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL MINE WATERS (39)',',',
     &(',',1pe12.4),',39',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      write(210+jper,9789)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T60LOAD1(jper,1),Length of main river,
     &(T60LOAD1 (jper,J13),J13=2,N13)
 9789 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL "OTHER" POINT SOURCES (60)',',',
     &(',',1pe12.4),',60',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      write(210+jper,9689)GIScode(feeture),jxtype,unamex, ! ------------ DDi.CSV
     &rnamex,T61LOAD1(jper,1),Length of main river,
     &(T61LOAD1 (jper,J13),J13=2,N13)
 9689 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL PRIVATE WASTEWATERS (61)',',',
     &(',',1pe12.4),',61',','0pf12.2',,,,',12(',',1pe12.4))
*     **************************************************************************
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

*     write out the monthly data in "commma separated" form --------------------
*     net loads from discharges (3 12 5 and 39) *************************TELODE2
      write(110+jper,8188)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(TELODE2 (jper,J13),J13=2,N13),
*     &rname(ireach),dname(jper),(TELODE2 (jper,J13),J13=2,N13),
     &TELODE2(jper,1),jxtype
 8188 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all types of discharges (3 12 5 and 39)',
     &',',a11,13(',',1pe12.4),',',i4,', 3 12 5 39 60 61')
*     net load from sewage works (3) ***********************************T03LOAD2
      write(110+jper,8928)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(T03LOAD2 (jper,J13),J13=2,N13),
     &T03LOAD2(jper,1),jxtype
 8928 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all sewage works (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3')
      write(110+jper,8929)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T12LOAD2 (jper,J13),J13=2,N13),
     &T12LOAD2(jper,1),jxtype
 8929 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all intermittent discharges (12)',',',
     &a11,13(',',1pe12.4),',',i4,',12')
*     industrial discharges (5) ***************************************T05LOAD2
      write(110+jper,8951)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T05LOAD2 (jper,J13),J13=2,N13),
     &T05LOAD2(jper,1),jxtype
 8951 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all industrial discharges (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5')
*     mine waters (39) ************************************************T39LOAD2
      write(110+jper,8930)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T39LOAD2 (jper,J13),J13=2,N13),
     &T39LOAD2(jper,1),jxtype
 8930 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all mine waters (39)',',',a11,
     &13(',',1pe12.4),',',i4,',39')
*     other discharges (60) *******************************************T60LOAD2
      write(110+jper,8961)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T60LOAD2 (jper,J13),J13=2,N13),
     &T60LOAD2(jper,1),jxtype
 8961 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all "Other" Point Sources (60)',',',a11,
     &13(',',1pe12.4),',',i4,',60') ! ----------------------------------- Di.CSV
*     private waters (61) *********************************************T61LOAD2
      write(110+jper,8960)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T61LOAD2 (jper,J13),J13=2,N13),
     &T61LOAD2(jper,1),jxtype
 8960 format(' ',a40,',',a40,s',',a20,',',
     &'Net inputs to load from all Private Wastewaters (61)',',',a11,
     &13(',',1pe12.4),',',i4,',61')
      write(110+jper,8909)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T13LOAD2 (jper,J13),J13=2,N13),
     &T13LOAD2 (jper,1),jxtype
 8909 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse inflow (river-type) (13)',
     &',',a11,13(',',1pe12.4),',',i4,',13')
      write(110+jper,8910)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T25LOAD2 (jper,J13),J13=2,N13),
     &T25LOAD2(jper,1),jxtype
 8910 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from livestock (25)',
     &',',a11,13(',',1pe12.4),',',i4,',25')
      write(110+jper,8911)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T27LOAD2 (jper,J13),J13=2,N13),
     &T27LOAD2 (jper,1),jxtype
 8911 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from arable land (27)',
     &',',a11,13(',',1pe12.4),',',i4,',27')
      write(110+jper,8912)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T29LOAD2 (jper,J13),J13=2,N13),
     &T29LOAD2 (jper,1),jxtype
 8912 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from highways (29)',
     &',',a11,13(',',1pe12.4),',',i4,',29')
      write(110+jper,8913)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T31LOAD2 (jper,J13),J13=2,N13),
     &T31LOAD2 (jper,1),jxtype
 8913 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from urban land (31)',
     &',',a11,13(',',1pe12.4),',',i4,',31')
      write(110+jper,8914)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T33LOAD2 (jper,J13),J13=2,N13),
     &T33LOAD2 (jper,1),jxtype
 8914 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from atmospheric deposition (33)',',',a11,
     &13(',',1pe12.4),',',i4,',33')
      write(110+jper,8915)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T35LOAD2 (jper,J13),J13=2,N13),
     &T35LOAD2 (jper,1),jxtype
 8915 format(' ',a40,',',a40,',',a20,',','Net inputs to load specified',
     &' as natural background (35)',',',a11,
     &13(',',1pe12.4),',',i4,',35')
      write(110+jper,8916)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T37LOAD2 (jper,J13),J13=2,N13),
     &T37LOAD2(jper,1),jxtype
 8916 format(' ',a40,',',a40,',',a20,',','Net inputs to load from ',
     &'diffuse pollution from septic tanks (37)',
     &',',a11,13(',',1pe12.4),',',i4,',37')
      write(110+jper,8116)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T40LOAD2 (jper,J13),J13=2,N13),
     &T40LOAD2(jper,1),jxtype
 8116 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from from aggregated ',
     &'CSOs (40)',',',a11,13(',',1pe12.4),',',i4,',40')
      write(110+jper,8922)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T42LOAD2 (jper,J13),J13=2,N13),
     &T42LOAD2 (jper,1),jxtype
 8922 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from aggregated STWs (42)',',',a11,
     &13(',',1pe12.4),',',i4,',42')
      write(110+jper,8925)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T46LOAD2 (jper,J13),J13=2,N13),
     &T46LOAD2 (jper,1),jxtype
 8925 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse mines (46)',',',a11,
     &13(',',1pe12.4),',',i4,',46')
      write(110+jper,8225)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T48LOAD2 (jper,J13),J13=2,N13),
     &T48LOAD2 (jper,1),jxtype
 8225 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load specified as ',
     &'from birds boats and angling (48)',
     &',',a11,13(',',1pe12.4),',',i4,',48')
      write(110+jper,8505)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T50LOAD2 (jper,J13),J13=2,N13),
     &T50LOAD2 (jper,1),jxtype
 8505 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (50)',
     &',',a11,13(',',1pe12.4),',',i4,',50')
      write(110+jper,8525)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T52LOAD2 (jper,J13),J13=2,N13),
     &T52LOAD2 (jper,1),jxtype
 8525 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (52)',
     &',',a11,13(',',1pe12.4),',',i4,',52')
      write(110+jper,8545)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T54LOAD2 (jper,J13),J13=2,N13),
     &T54LOAD2 (jper,1),jxtype
 8545 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (54)',
     &',',a11,13(',',1pe12.4),',',i4,',54')
      write(110+jper,8565)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T56LOAD2 (jper,J13),J13=2,N13),
     &T56LOAD2 (jper,1),jxtype
 8565 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (56)',
     &',',a11,13(',',1pe12.4),',',i4,',56')
      write(110+jper,8585)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T58LOAD2 (jper,J13),J13=2,N13),
     &T58LOAD2 (jper,1),jxtype
 8585 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (58)',
     &',',a11,13(',',1pe12.4),',',i4,',58')
      write(110+jper,8917)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T15LOAD2 (jper,J13),J13=2,N13),
     &T15LOAD2(jper,1),jxtype
 8917 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse inflows (effluent-type) (15)',
     &',',a11,13(',',1pe12.4),',',i4,',15')
      write(110+jper,8180)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TRLODE2 (jper,J13),J13=2,N13),
     &TRLODE2(jper,1),jxtype
 8180 format(' ',a40,',',a40,',',a20,',','Net inputs to load ',
     &'from boundaries and streams (2 and 10)',
     &',',a11,13(',',1pe12.4),',',i4,', 2 10')
      write(110+jper,8181)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TDDLOAD2 (jper,J13),J13=2,N13),
     &TDDLOAD2(jper,1),jxtype
 8181 format(' ',a40,',',a40,',',a20,',','Net inputs to load ',
     &'from diffuse inflow (reach-based)',',',a11,13(',',1pe12.4),
     &',',i4,',500')
      write(110+jper,8182)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TNLOADUP2 (jper,J13),J13=2,N13),
     &TNLOADUP2(jper,1),jxtype
 8182 format(' ',a40,',',a40,',',a20,',','Net inputs to load ',
     &'added by natural purification',',',a11,
     &13(',',1pe12.4),',',i4,',600')
      write(110+jper,8186)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TILOADUP2 (jper,J13),J13=2,N13),
     &TILOADUP2(jper,1),jxtype
 8186 format(' ',a40,',',a40,',',a20,',','Net inputs to load ',
     &'added by flow gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',700')
      write(110+jper,8184)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TALOADUP2 (jper,J13),J13=2,N13), 
     &TALOADUP2(jper,1),jxtype
 8184 format(' ',a40,',',a40,',',a20,',','Net inputs to load ',
     &'added by quality gap filling     ',
     &',',a11,13(',',1pe12.4),',',i4,',800')
     
      write(110+jper,8919)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TGLODE2 (jper,J13),J13=2,N13),
     &TGLODE2 (jper,1),jxtype
 8919 format(' ',a40,',',a40,',',a20,',',
     &'Net TOTAL of all loads',',',a11,13(',',1pe12.4),',',i4,',999')
      write(110+jper,8918)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TBLOAD2 (jper,J13),J13=2,N13),
     &TBLOAD2(jper,1),jxtype
 8918 format(' ',a40,',',a40,',',a20,',',
     &'Load lost to abstractions (7)',',',a11, ! ------------------------ Di.CSV
     &13(',',1pe12.4),',',i4,', 7 18 19')
      write(110+jper,8183)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TNLOADDN2 (jper,J13),J13=2,N13),
     &TNLOADDN2(jper,1),jxtype
 8183 format(' ',a40,',',a40,',',a20,',',
     &'Load lost to natural purification',',',a11,13(',',1pe12.4),',',i4
     &,',601')
      write(110+jper,8187)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TILOADDN2 (jper,J13),J13=2,N13),
     &TILOADDN2(jper,1),jxtype
 8187 format(' ',a40,',',a40,',',a20,',',
     &'Load lost by flow gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',701')
      write(110+jper,8185)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TALOADDN2 (jper,J13),J13=2,N13),
     &TALOADDN2(jper,1),jxtype
 8185 format(' ',a40,',',a40,',',a20,',',
     &'Load lost by quality gap filling     ',
     &',',a11,13(',',1pe12.4),',',i4,',801')
      write(110+jper,8195)GIScode(feeture),unamex,
     &rnamex,dname(jper),(TLOSSES2 (jper,J13),J13=2,N13),
     &TLOSSES2(jper,1),jxtype
 8195 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD REMOVED',
     &',',a11,13(',',1pe12.4),',',i4,',802')

*     **************************************************************************
      write(110+jper,8948)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T03LOAD1 (jper,J13),J13=2,N13),
     &T03LOAD1(jper,1),jxtype
 8948 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL SEWAGE WORKS (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3')
*     **************************************************************************
      write(110+jper,8949)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T12LOAD1 (jper,J13),J13=2,N13),
     &T12LOAD1(jper,1),jxtype
 8949 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL INTERMITTENT DISCHARGES (12)',',',a11,
     &13(',',1pe12.4),',',i4,',12')
*     **************************************************************************
      write(110+jper,8950)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T05LOAD1 (jper,J13),J13=2,N13),
     &T05LOAD1(jper,1),jxtype
 8950 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT BY ALL INDUSTRIAL DISCHARGES (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5')
*     **************************************************************************
      write(110+jper,8989)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T39LOAD1 (jper,J13),J13=2,N13),
     &T39LOAD1(jper,1),jxtype
 8989 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL MINE WATERS (39)',',',a11,
     &13(',',1pe12.4),',',i4,',39')
*     **************************************************************************
      write(110+jper,8969)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T60LOAD1 (jper,J13),J13=2,N13),
     &T39LOAD1(jper,1),jxtype
 8969 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL OTHER DISCHARGES (60)',',',a11,
     &13(',',1pe12.4),',',i4,',60')
*     **************************************************************************
      write(110+jper,8979)GIScode(feeture),unamex,
     &rnamex,dname(jper),(T39LOAD1 (jper,J13),J13=2,N13),
     &T39LOAD1(jper,1),jxtype
 8979 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL PRIVATE WASTEWATERS (61)',',',
     &a11,13(',',1pe12.4),',',i4,',61')
*     **************************************************************************
      
      return
      end

      
      
      subroutine write the monthly data in tables (jper)
      include 'COMMON DATA.FOR'
      character *10 Tenchars (13)
     
      if ( QTYPE (jper) .ne. 4 ) return
      if ( nobigout .gt. 0 ) return

      do idet = 1, n13
      Tenchars(idet) = ' .........'
      enddo

      if ( abs (TGLODE2(jper,i13)) .gt. 0.000001) then
      write(100+jper,3022)dname(jper)
 3022 format(///30('-'),13(2x,8('-'))/
     &'Apportionment of the net loads for ',a11,'...'/
     &30('-'),13(2x,8('-')))
      endif
      
*     running total load introduced by upstream boundaries and tributaries -----
      if ( abs (TRLODE2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TRLODE2(jper,i+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TRLODE2(jper,i13))
      write(100+jper,3000)(Tenchars (i),i=1,13)
 3000 format('Boundaries and streams (2,10) ',13a10)
      endif
*     total load introduced by clean diffuse sources -------------------TDDLOAD2
      if ( abs (TDDLOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TDDLOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TDDLOAD2(jper,i13))
      write(100+jper,3001)(Tenchars (i),i=1,13)
 3001 format('Diffuse (reach-based)',9x,13a10)
      endif
*     added by natural purification -----------------------------------TNLOADUP2
      if ( abs (TNLOADUP2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TNLOADUP2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TNLOADUP2(jper,i13))
      write(100+jper,3002)(Tenchars (i),i=1,13)
 3002 format('Added by natural purification ',13a10)
      endif

*     added by flow gap filling ----------------------------------------TILOADUP2
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),TILOADUP2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TILOADUP2(jper,i13))
      write(100+jper,3006)(Tenchars (i),i=1,13)
 3006 format('Added by flow gap filling     ',13a10)
      endif

*     added by quality gap filling for --------------------------------TALOADUP2
      if ( ical .gt. 3 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TALOADUP2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TALOADUP2(jper,i13))
      write(100+jper,3004)(Tenchars (i),i=1,13)
 3004 format('Added by quality gap filling  ',13a10)
      endif

*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),TELODE2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TELODE2(jper,i13))
*     write(100+jper,3208)(Tenchars (i),i=1,13)
 3208 format('Net discharges (3 12 5 and 39)',13a10)
     
*     net load from sewage works (3) -----------------------------------T03LOAD2
      if ( abs (T03LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),T03LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T03LOAD2(jper,i13))
      write(100+jper,3028)(Tenchars (i),i=1,13)
 3028 format('Sewage works (3)   ',11x,13a10)
      endif
      
*     intermittent discharges (12) ------------------------------------T12LOAD2
      if ( abs (T12LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars (i),T12LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T12LOAD2(jper,i13))
      write(100+jper,3029)(Tenchars (i),i=1,13)
 3029 format('Intermittent discharges (12)',2x,13a10)
      endif
      
*     industrial discharges (5) ----------------------------------------T05LOAD2
      if ( abs (T05LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T05LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T05LOAD2(jper,i13))
      write(100+jper,3051)(Tenchars (i),i=1,13)
 3051 format('Industrial discharges (5)   ',2x,13a10)
      endif
      
*     mine waters (39) -------------------------------------------------T39LOAD2
      if ( abs (T39LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T39LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T39LOAD2(jper,i13))
      write(100+jper,3030)(Tenchars (i),i=1,13)
 3030 format('Mine waters (39)            ',2x,13a10)
      endif
      
*     other Point Sources (60) -----------------------------------------T60LOAD2
      if ( abs (T60LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T60LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T60LOAD2(jper,i13))
      write(100+jper,3930)(Tenchars (i),i=1,13)
 3930 format('"Other" Point Sources (60)    ',2x,13a10)
      endif
      
*     private wastwaters (61) ------------------------------------------T61LOAD2
      if ( abs (T61LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T61LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T61LOAD2(jper,i13))
      write(100+jper,3830)(Tenchars (i),i=1,13)
 3830 format('Private wastewaters (61)    ',2x,13a10)
      endif
*     diffuse (river-type) (13) --------------------------------------- T13LOAD2
      if ( abs (T13LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T13LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T13LOAD2(jper,i13))
      write(100+jper,3009)(Tenchars (i),i=1,13)
 3009 format('Diffuse (river-type) (13)     ',13a10)
      endif
      
*     livestock (25) -------------------------------------------------- T25LOAD2
      if ( abs (T25LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T25LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T25LOAD2(jper,i13))
      write(100+jper,3010)(Tenchars (i),i=1,13)
 3010 format('Livestock (25)                ',13a10)
      endif
      
*     arable (27) ----------------------------------------------------- T27LOAD2
      if ( abs (T27LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T27LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T27LOAD2(jper,i13))
      write(100+jper,3011)(Tenchars (i),i=1,13)
 3011 format('Arable (27)                   ',13a10)
      endif
      
*     highway (29) ---------------------------------------------------- T29LOAD2
      if ( abs (T29LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T29LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T29LOAD2(jper,i13))
      write(100+jper,3012)(Tenchars (i),i=1,13)
 3012 format('Highway (29)                  ',13a10)
      endif
      
*     urban (31) -----------------------------------------------=------ T31LOAD2
      if ( abs (T31LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T31LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T31LOAD2(jper,i13))
      write(100+jper,3013)(Tenchars (i),i=1,13)
 3013 format('Urban (31)                    ',13a10)
      endif
      
*     atmosphere (33) ------------------------------------------------- T33LOAD2
      if ( abs (T33LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T33LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T33LOAD2(jper,i13))
      write(100+jper,3014)(Tenchars (i),i=1,13)
 3014 format('Atmosphere (33)               ',13a10)
      endif
      
*     natural background (35) --------------------------------------------------
      if ( abs (T35LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T35LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T35LOAD2(jper,i13))
      write(100+jper,3015)(Tenchars (i),i=1,13)
 3015 format('Natural background (35)       ',13a10)
      endif
      
*     septic tanks (37) --------------------------------------------------------
      if ( abs (T37LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T37LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T37LOAD2(jper,i13))
      write(100+jper,3016)(Tenchars (i),i=1,13)
 3016 format('Septic tanks (37)             ',13a10)
      endif
      
*     aggregate CSOs (40) ------------------------------------------------------
      if ( abs (T40LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T40LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T40LOAD2(jper,i13))
      write(100+jper,3216)(Tenchars (i),i=1,13)
 3216 format('Aggregate CSOs (40)           ',13a10)
      endif
      
*     diffuse (effluent-type) (15) --------------------------------------------
      if ( abs (T15LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T15LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T15LOAD2(jper,i13))
      write(100+jper,3017)(Tenchars (i),i=1,13)
 3017 format('Diffuse (effluent-type) (15)  ',13a10)
      endif
      
*     aggregate STWs (42) ------------------------------------------------------
      if ( abs (T42LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T42LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T42LOAD2(jper,i13))
      write(100+jper,3817)(Tenchars (i),i=1,13)
 3817 format('Aggregate STWs (42)           ',13a10)
      endif
      
*     diffuse mines (46) -------------------------------------------------------
      if ( abs (T46LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T46LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T46LOAD2(jper,i13))
      write(100+jper,3815)(Tenchars (i),i=1,13)
 3815 format('Diffuse mines (46)            ',13a10)
      endif
      
*     birds, boats and angling (48) --------------------------------------------
      if ( abs (T48LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T48LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T48LOAD2(jper,i13))
      write(100+jper,3885)(Tenchars (i),i=1,13)
 3885 format('Birds, boats and angling (48) ',13a10)
      endif
      
*     ------------------------ (50) --------------------------------------------
      if ( abs (T50LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T50LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T50LOAD2(jper,i13))
      write(100+jper,3505)(Tenchars (i),i=1,13)
 3505 format('---------------------- (50) ',13a10)
      endif
      
*     ------------------------ (52) --------------------------------------------
      if ( abs (T52LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T52LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T52LOAD2(jper,i13))
      write(100+jper,3525)(Tenchars (i),i=1,13)
 3525 format('---------------------- (52) ',13a10)
      endif
      
*     ------------------------ (54) --------------------------------------------
      if ( abs (T54LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T54LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T54LOAD2(jper,i13))
      write(100+jper,3545)(Tenchars (i),i=1,13)
 3545 format('---------------------- (54) ',13a10)
      endif
      
*     ------------------------ (56) --------------------------------------------
      if ( abs (T56LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T56LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T56LOAD2(jper,i13))
      write(100+jper,3565)(Tenchars (i),i=1,13)
 3565 format('---------------------- (56) ',13a10)
      endif
      
*     ------------------------ (58) --------------------------------------------
      if ( abs (T58LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T58LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T58LOAD2(jper,i13))
      write(100+jper,3585)(Tenchars (i),i=1,13)
 3585 format('---------------------- (58) ',13a10)
      endif

      
*     Headwaters and tributaries (10) -------------------------------------------
      if ( abs (TRLODE2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TRLODE2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TRLODE2(jper,i13))
      write(100+jper,3865)(Tenchars (i),i=1,13)
 3865 format('Headwater and streams (10)    ',13a10)
      endif

*     total--------------------------------------------------------------TGLODE2
      if ( abs(TGLODE2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TGLODE2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TGLODE2(jper,i13))
      call write line (jper)
      write(100+jper,3019)(Tenchars (i),i=1,13)
 3019 format('Total                         ',13a10)
      endif
      call write line (jper)
*     --------------------------------------------------------------------------


*     lost to abstractions ----------------------------------------------TBLOAD2
      if ( abs (TBLOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TBLOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TBLOAD2(jper,i13))
      write(100+jper,3018)(Tenchars (i),i=1,13)
 3018 format('Abstractions (7)      ',8x,13a10)
      endif 

*     lost by natural purification ------------------------------------TNLOADDN2
      if ( abs (TNLOADDN2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TNLOADDN2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TNLOADDN2(jper,i13))
      write(100+jper,3003)(Tenchars (i),i=1,13)
 3003 format('Lost by natural purification  ',13a10)
      endif

*     if ( abs (TILOADDN2(jper,i13)) .gt. 0.000001) then
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TILOADDN2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TILOADDN2(jper,i13))
      write(100+jper,3007)(Tenchars (i),i=1,13)
 3007 format('Lost by flow gap filling      ',13a10)
      endif

*     if ( abs (TALOADDN2(jper,i13)) .gt. 0.000001) then
      if ( ical .gt. 3 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TALOADDN2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TALOADDN2(jper,i13))
      write(100+jper,3005)(Tenchars (i),i=1,13)
 3005 format('Lost by quality gap filling   ',13a10)
      endif

      if ( abs (TLOSSES2(jper,i13)) .gt. 0.000001) then
      call set format for printout(Tenchars (13),TLOSSES2(jper,i13))
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TLOSSES2(jper,I+1))
      enddo
      endif ! set up print out
      call set format for printout(Tenchars (13),TLOSSES2(jper,i13))
      call write line (jper)
      write(100+jper,3905)(Tenchars (i),i=1,13)
 3905 format('Total losses ...',14x,13a10)
      call write line (jper)
      write(100+jper,6905)
 6905 format(///)
      endif ! if ( abs (TLOSSES2(jper,i13)) .gt. 0.000001)

      do idet = 1, ndet
      Tenchars(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      Tenchars(idet) = dna(idet)
      endif
      enddo
      write(120+jper,4166)(Tenchars(idet),idet = 1, ndet)
 4166 format(//140('=')/'Percentages of net load from all inputs ',
     &10(6x,a4)/140('='))
      write(120+jper,4806)

      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop10(jper)) .gt. 1.0e-10 ) then
      call sort format 3  (prop10(jper),propRT(jper),propNPU(jper))
      write(120+jper,8930)valchars10,valchars11,valchars12 ! ------------ Di.ADL
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
      write(120+jper,9163)Tenchars(jper) ! ------------------------------ Di.ADL
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
      write(120+jper,9103)Tenchars(jper) ! ------------------------------ Di.ADL
 9103 format('*',4x,'Added by natural purification',16X,10A10)
      endif

*     ==========================================================================
*     loads added by gap filling for flows -------------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
      write(Tenchars(jper),7831)propfrl(jper)
      endif
      if ( kp .eq. 1 ) write(120+jper,8923)Tenchars(jper) ! ------------- Di.ADL
 8923 format('*',4x,'Added by gap filling of flows',16x,10A10)
*     ==========================================================================
*     loads added by gap filling for quality -----------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 ) kp = 1
      write(Tenchars(jper),7831)propqrl(jper)
      endif
      if ( kp .eq. 1 ) write(120+jper,8224)Tenchars(jper) ! ------------- Di.ADL
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
      write(120+jper,8363)Tenchars(jper) ! ------------------------------ Di.ADL
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
 4826 format(140('-'))
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (propeff60(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)propeff60 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,8390)Tenchars(jper)
 8390 format('*',4x,'Other Point Sources (60) ',20X,10A10)
      write(120+jper,4826)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (propeff61(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)propeff61 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,8290)Tenchars(jper) ! ------------------------------ Di.ADL
 8290 format('*',4x,'Private wastewaters (61) ',20X,10A10)
      write(120+jper,4826)
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
      kp = 0
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
      kp = 0
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
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop50(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop50 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4950)nameprop(23),Tenchars(jper)
 4950 format('*',4x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop52(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop52 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4952)nameprop(24),Tenchars(jper)
 4952 format('*',4x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop54(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop54 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4954)nameprop(25),Tenchars(jper)
 4954 format('*',4x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop56(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop56 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4956)nameprop(26),Tenchars(jper)
 4956 format('*',4x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop58(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop58 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4958)nameprop(26),Tenchars(jper)
 4958 format('*',4x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop10(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)prop10 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,4869)Tenchars(jper)
 4869 format('*',4x,'Headwaters and streams (10)  ',16x,10A10)
      endif
*     ==========================================================================
      kp = 0
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
 4136 format(140('-')/'TOTAL NET LOAD (%) ...',28x,10A10)
      write(120+jper,4806)
 4806 format(140('='))
      endif
*     ==========================================================================
      kp = 0
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prolosses(jper)) .gt. 1.0e-10 ) kp = 1
      endif
      if ( kp .eq. 1 ) write(120+jper,4106)
 4106 format(140('=')/'Percentages of the total load that have ',
     &'been ...'/140('='))
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
      write(120+jper,1931)Tenchars(jper) ! ----------------------------- Di.ADL
 1931 format(5x,'Removed by natural purification',14x,10A10) ! --------- Di.ADL
*     =========================================================================


*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     loads removed by gap filling for flows ----------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
*     if ( abs (propfra(jper)) .gt. 1.0e-10 ) kp = 1
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
*     if ( abs (propqra(jper)) .gt. 1.0e-10 ) kp = 1
      if ( ical .gt. 3 ) kp = 1
      write(Tenchars(jper),7831)propqra(jper)
      endif
      if ( kp .eq. 1 ) write(120+jper,8964)Tenchars(jper)
 8964 format(5X,'Removed by gap filling of quality',12x,10A10)
      if ( kp .eq. 1 ) write(120+jper,4806)
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


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
 8914 format(140('-')/'TOTAL % LOSSES ',35x,10a10/140('-'))
      write(120+jper,4806)
      endif
*     =========================================================================


*     ==========================================================================
      kp = 0
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (proadds(jper)) .gt. 1.0e-10 ) kp = 1
      endif
*     if ( kp .eq. 1 ) write(120+jper,4896)
 4896 format(140('=')/'Percentages of the total load that have ',
     &'been'/140('='))
*     =========================================================================

*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     loads added by gap filling for flows ------------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (propfrl(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)propfrl(jper)
      endif
*     if ( kp .eq. 1 ) write(120+jper,8923)Tenchars(jper) ! ------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     loads added by gap filling for quality -----------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (propqrl(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)propqrl(jper)
      endif
*     if ( kp .eq. 1 ) write(120+jper,8224)Tenchars(jper) ! ------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     total additions of load --------------------------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (proadds(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)proadds(jper)
      endif
      if ( kp .eq. 1 ) then
*     write(120+jper,8974)Tenchars(jper) ! ------------------------------ Di.ADL
 8974 format(140('-')/'TOTAL % ADDITIONS ',32x,10a10)
*     write(120+jper,4806)
      endif
*     ==========================================================================



*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      write(120+jper,4886)
 4886 format(///140('=')/'Total added loads (without allowing for ',
     &'any losses)'/140('='))


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from headwaters and tributaries etc --------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TRLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TRLODE1(jper,i13)) ! -- Di.ADL
      endif
      if ( kp .eq. 1 ) write(120+jper,3530)Tenchars(jper) ! ------------- Di.ADL
 3530 format('*',4x,'Boundaries and tributaries (2, 10 and 12)',
     &4x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from reach-based diffuse sources -----------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TDDLOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TDDLOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4930)Tenchars(jper) ! ------------- Di.ADL
 4930 format('*',4x,'Reach-type diffuse sources',19x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     total loads introduced by gap filling for river flows -------------------- 
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),TILOADUP1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2943)Tenchars(jper) ! ------------- Di.ADL
 2943 format('*',4x,'Added by gap filling of flow',17x,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     total loads introduced by gap filling for river quality ------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),TALOADUP1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2944)Tenchars(jper) ! ------------- Di.ADL
 2944 format('*',4x,'Added by gap filling of quality',14X,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loads added by natural purification --------------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TNLOADUP1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TNLOADUP1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4931)Tenchars(jper) ! ------------- Di.ADL
 4931 format(5X,'Added by natural purification',16X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by discharges of effluent (types 3, 12 and 5) -----
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TELODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TELODE1(jper,i13))
      endif
*     if ( kp .eq. 1 ) write(120+jper,8983)Tenchars(jper) ! ------------- Di.ADL
 8983 format(5X,'Total from discharges (types 3, 12 and 5)',4x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads introduced by discharges of effluent (3) ------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T03LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T03LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2733)Tenchars(jper) ! ------------- Di.ADL
 2733 format('*',4x,'Total from sewage effluents (3)',14x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads introduced by intermittent discharges of sewage (12)
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T12LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T12LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,2739)Tenchars(jper) ! ------------- Di.ADL
 2739 format('*',4x,'Intermittent discharges of sewage (12)',7x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from industrial discharges (5) ----------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T05LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T05LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1933)Tenchars(jper) ! ------------- Di.ADL
 1933 format('*',4x,'Total from industrial discharges (5)',9x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T39LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T39LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1993)Tenchars(jper) ! ------------- Di.ADL
 1993 format('*',4x,'Total from mine waters (39)      ',12x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T60LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T60LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,6993)Tenchars(jper) ! ------------- Di.ADL
 6993 format('*',4x,'Total from Other Point Sources (60)',10x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T61LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T61LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,7993)Tenchars(jper) ! ------------- Di.ADL
 7993 format('*',4x,'Total from private wastewaters (61)',10x,10A10)
	if ( kp .eq. 1 ) write(120+jper,4826)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from diffuse features: river flow type (13) ---------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T13LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T13LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8113)Tenchars(jper) ! ------------- Di.ADL
 8113 format('*',4x,'River-type diffuse pollution (13)',12X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from diffuse features: discharge type (15) ----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T15LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T15LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8233)Tenchars(jper) ! ------------- Di.ADL
 8233 format('*',4x,'Effluent-type diffuse pollution (15)',9X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from diffuse pollution from livestock (25) ----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T25LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T25LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8125)Tenchars(jper) ! ------------- Di.ADL
 8125 format('*',4x,'Diffuse pollution from livestock (25)',8X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from diffuse pollution from arable (27) -------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T27LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T27LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8127)Tenchars(jper) ! ------------- Di.ADL
 8127 format('*',4x,'Diffuse pollution from arable (27)',11X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from highway runoff (29) ----------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T29LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T29LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8129)Tenchars(jper) ! ------------- Di.ADL
 8129 format('*',4x,'Highway runoff (29)',26X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from urban runoff (31) ------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T31LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T31LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8131)Tenchars(jper) ! ------------- Di.ADL
 8131 format('*',4x,'Urban runoff (31)',28X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from atmospheric deposition (33) --------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T33LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T33LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8133)Tenchars(jper) ! ------------- Di.ADL
 8133 format('*',4x,'Atmospheric deposition (33)',18x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from natural background (35) ------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T35LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T35LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8193)Tenchars(jper) ! ------------- Di.ADL
 8193 format('*',4x,'Natural background (35)',22x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from Aggregated STWs (42) ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T42LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T42LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4193)Tenchars(jper) ! ------------- Di.ADL
 4193 format('*',4x,'Aggregated STWs (42)',25x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from diffuse mines (46) -----------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T46LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T46LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4593)Tenchars(jper) ! ------------- Di.ADL
 4593 format('*',4x,'Diffuse mines (46)',27x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from birds, boats and angling (48) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T48LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T48LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4793)Tenchars(jper) ! ------------- Di.ADL
 4793 format('*',4x,'Birds, boats and angling (48)',16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! (50) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T50LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T50LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4850)nameprop(23),Tenchars(jper) !  Di.ADL
 4850 format('*',4x,a37,16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! (52) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T52LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T52LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4852)nameprop(24),Tenchars(jper) !  Di.ADL
 4852 format('*',4x,a37,16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! (54) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T54LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T54LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4854)nameprop(25),Tenchars(jper) !  Di.ADL
 4854 format('*',4x,a37,16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! (56) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T56LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T56LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4856)nameprop(26),Tenchars(jper) !  Di.ADL
 4856 format('*',4x,a37,16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! (58) ------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T58LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T58LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,4858)nameprop(27),Tenchars(jper) !  Di.ADL
 4858 format('*',4x,a37,16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from headwaters and streams (10) --------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TRLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TRLODE1(jper,i13)) ! -- Di.ADL
      endif
      if ( kp .eq. 1 ) write(120+jper,4893)Tenchars(jper) ! ------------- Di.ADL
 4893 format('*',4x,'Headwaters and streams (10)  ',16x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from septic tanks (37) ------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T37LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T37LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8137)Tenchars(jper) ! ------------- Di.ADL
 8137 format('*',4x,'Septic tanks (37)',28x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from aggregated CSOs (40) ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T40LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T40LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8337)Tenchars(jper) ! ------------- Di.ADL
 8337 format('*',4x,'Aggregate CSOs (40)',26x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( kp .eq. 1 ) write(100+jper,4806) ! total ----------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TGLODE1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TGLODE1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1593)Tenchars(jper) ! ------------- Di.ADL
 1593 format(140('-')/'TOTAL    ',41x,10A10)
      if ( kp .eq. 1 ) write(120+jper,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! total loads from abstractions -----------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TBLOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TBLOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8246)Tenchars(jper) ! ------------- Di.ADL
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! loads removed by natural purification ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TNLOADDN1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TNLOADDN1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1931)Tenchars(jper) ! ------------- Di.ADL
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! total loads introduced by gap filling for river flows -----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 .and. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),TILOADDN1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8913)Tenchars(jper) ! ------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! total loads introduced by gap filling for river quality ---------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),TALOADDN1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8964)Tenchars(jper) ! ------------- Di.ADL
      if ( kp .eq. 1 ) write(120+jper,4806) ! --------------------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      kp = 0 ! total losses of load --------------------------------------------
      
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TLOSSES2(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TLOSSES2(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,7964)Tenchars(jper) ! ------------- Di.ADL
 7964 format('TOTAL LOAD REMOVED ...',28x,10A10)
      if ( kp .eq. 1 ) write(120+jper,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      return
      end
      






      subroutine write out monthly flows and temperature (iplace)
      include 'COMMON DATA.FOR'
      character *10 Tenchars (13)
     
      if ( nobigout .gt. 0 ) return

      do idet = 1, n13
      Tenchars(idet) = ' .........'
      enddo

      do 3399 jper = 1, ndet ! =================================================
      if ( QTYPE (jper) .ne. 4 ) then ! ========================================
      if ( iplace .eq. 1 ) then
      write(100+jper,2011)rname(ireach),dname(jper)
 2011 format('Monthly data at the head of the reach ...',a20,
     &17x,' for: ',a11)
      endif
      if ( iplace .eq. 2 ) then
      write(100+jper,2111)uname(feeture),dname(jper)
 2111 format('Monthly data at feature ... ',a37,13x,' for: ',a11)
      endif
      if ( iplace .eq. 3 ) then
      write(100+jper,2211)rname(ireach),dname(jper)
 2211 format(/124('=')/'Monthly data at the end of the reach ...',a20,
     &18x,' for: ',a11)
      endif
      if ( iplace .eq. 4 ) then
      write(100+jper,2311)uname(feeture),dname(jper)
 2311 format(/124('=')/'Monthly data UPSTREAM of discharge ... ',a37,
     &2x,' for: ',a11)
      endif
      if ( iplace .eq. 5 ) then
      write(100+jper,2411)uname(feeture),dname(jper)
 2411 format('Monthly data downstream of tributary ... ',a37,
     &' for: ',a11)
      endif
      if ( iplace .eq. 6 ) then
      write(100+jper,2511)uname(feeture),dname(jper)
 2511 format('Monthly data DOWNSTREAM of discharge ... ',a37,
     &' for: ',a11)
      endif

      write(100+jper,2000)(names of months (i),i = 1, 12)
 2000 format(30('-'),13(2x,8('-'))/
     &'Summary statistic             ',
     &12(1x,a9),'  All year Running mean'/30('-'),13(2x,8('-')))
      
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),fmon(1,i))
      enddo
      endif
      call set format for printout(Tenchars (13),FLOW(1))

      write(100+jper,1100)(Tenchars (i),i=1,13)
 1100 format('Monthly mean river flow       ',13a10)

      if ( munthly structure .eq. 1 ) then ! set up print out -------------------
      do i = 1, 12
      call set format for printout(Tenchars (i),fmon(2,i))
      enddo
      endif ! if ( munthly structure .eq. 1 ) -----------------------------------
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

      if ( munthly structure .eq. 1 ) then ! set up print out ------------------
      do i = 1, 12
      call set format for printout(Tenchars (i),smon(2,i))
      enddo
      endif ! if ( munthly structure .eq. 1 ) ----------------------------------
      call set format for printout(Tenchars (13),BC(2,2))

      write(100+jper,1901)(Tenchars (i),i=1,13)
 1901 format('Standard deviation            ',13a10)
      call write line (jper)
      endif ! if ( xprint .gt. 0.00001 )

      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      write(210+jper,8100)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &FLOW(1),Length of main river,(fmon(1,i),i=1,12)
 8100 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean river flow       ',',','River flow',
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,8101)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &FLOW(2),Length of main river,(fmon(2,i),i=1,12)
 8101 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'95-percentile low flow         ',',','River flow',
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,8800)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &BC(1,1),Length of main river,(tmon(1,i),i=1,12)
 8800 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean temperature      ',',','Temperature',
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,8801)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &BC(1,2),Length of main river,(tmon(2,i),i=1,12)
 8801 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation            ',',','Temperature',
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,8900)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &BC(2,1),Length of main river,(smon(1,i),i=1,12)
 8900 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean suspended solids         ',',','Suspended solids', ! ------- DDi.CSV
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      write(210+jper,8901)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &BC(2,2),Length of main river,(smon(2,i),i=1,12)
 8901 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Standard deviation            ',',','Suspended solids',
     &(',',1pe12.4),',,'0pf12.2',,,,',12(',',1pe12.4))
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      if (munthly structure .eq. 1 ) then
      write(110+jper,9100)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(fmon(1,i),i=1,12),FLOW(1),jxtype ! ------------------------------- Di.CSV
 9100 format(' ',a40,',',a40,',',a20,',',
     &'Mean river flows      ',',','River flow', ! ---------------------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9100)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(FLOW(1),i=1,13),jxtype ! ----------------------------------------- Di.CSV
      endif
     
      if (munthly structure .eq. 1 ) then
      write(110+jper,9101)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(fmon(2,i),i=1,12),FLOW(2),jxtype ! ------------------------------- Di.CSV
 9101 format(' ',a40,',',a40,',',a20,',',
     &'95-percentile low flows       ',',','River flow', ! -------------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9101)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(FLOW(2),i=1,13),jxtype ! ----------------------------------------- Di.CSV
      endif
      
      if (munthly structure .eq. 1 ) then
      write(110+jper,9800)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(tmon(1,i),i=1,12),BC(1,1),jxtype ! temperature
 9800 format(' ',a40,',',a40,',',a20,',',
     &'Mean temperatures     ',',','Temperature', ! --------------------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9800)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(BC(1,1),i=1,13),jxtype ! temperature
      endif
      
      if (munthly structure .eq. 1 ) then
      write(110+jper,9801)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(tmon(2,i),i=1,12),BC(1,2),jxtype ! temperature
 9801 format(' ',a40,',',a40,',',a20,',',
     &'Standard deviations           ',',','Temperature', ! ------------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9801)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(BC(1,2),i=1,13),jxtype ! temperature
      endif
           
      if (munthly structure .eq. 1 ) then
      write(110+jper,9900)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(smon(1,i),i=1,12),BC(2,1),jxtype ! suspended solids
 9900 format(' ',a40,',',a40,',',a20,',',
     &'Mean suspended solids  ',',','Suspended solids', ! --------------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9900)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(BC(2,1),i=1,13),jxtype ! suspended solids
      endif
     
      if (munthly structure .eq. 1 ) then
      write(110+jper,9901)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(smon(2,i),i=1,12),BC(2,2),jxtype ! suspended solids
 9901 format(' ',a40,',',a40,',',a20,',',
     &'Standard deviations           ',',','Suspended solids', ! -------- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,9901)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(BC(2,2),i=1,13),jxtype ! suspended solids
      endif

      endif ! if ( QTYPE (jper) .ne. 4 ) =======================================
 3399 continue ! do 3399 jper = 1, ndet ========================================
      return
      end
      
      
      
      subroutine write out the monthly apportionment (jper,iplace)
      include 'COMMON DATA.FOR'
      character *10 llmonchar(nprop,MP10,2,13),
     &llmonchar2(nprop,MP10,1,13),runchar

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
      llmonchar(ip,jper,1,im) = '..........'
      llmonchar(ip,jper,2,im) = '..........'
      xml = xml + lconmon(ip,jper,1,im) * fraction of year (im)
      enddo ! do im = 1, 12
      xml = xml * 365.0

      do im = 1, 12
      if ( xml .gt. 0.000001 ) then
      lconmon(ip,jper,1,im) = 100.0 * days in months (im) * 
     &lconmon(ip,jper,1,im) / xml
      else
      lconmon(ip,jper,1,im) = 0.0
      endif
      enddo
      
      if ( im .eq. 13 ) then
      if ( xml .gt. 0.000001 ) then
      lconmon(ip,jper,1,im) = 100.0 * 365.0 * 
     & lconmon(ip,jper,1,im) / xml
      else
      lconmon(ip,jper,1,im) = 0.0
      endif
      endif
      
 3500 continue 

      running mean = 0.0

      zero check = 0.0
      do 3000 ip = 1, nprop
      do im = 1, 13
      zero check = amax1 ( zero check, loadmon(ip,jper,1,im) )
      call set format for printout ! --------------------- loadmon(ip,jper,1,im)
     &(llmonchar(ip,jper,1,im),loadmon(ip,jper,1,im))
      call set format for printout ! --------------------- loadmon(ip,jper,2,im)
     &(llmonchar(ip,jper,2,im),loadmon(ip,jper,2,im))
      call set format for printout ! --------------------- lconmon(ip,jper,1,im)
     &(llmonchar2(ip,jper,1,im),lconmon(ip,jper,1,im))
      enddo
 3000 continue 
      
      
*     **************************************************************************      
      
      do 2000 iiip = 1, nprop ! 555555555555555555555555555555555555555555555555
      ip = propsequence(iiip)

*     --------------------------------------------------------------------------
*     output for features ------------------------------------------------------
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( ip .eq. 1) then ! write the headings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
      if ( iplace .eq. 4  ) then ! ---------------------------------------------
      write(120+jper,8690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 8690 format(///100('=')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... Upstream of ',a37/100('-')) ! --------------------------- Di.ADL
      write(100+jper,8290)uname(feeture) ! ++++++++++++++++++++++++++++++ Di.MON
 8290 format(160('-')/'Apportionment of mean load', ! +++++++++++++++++++ Di.MON
     &' ... Upstream of ',a37/160('-')) ! +++++++++++++++++++++++++++++++ Di.MON
      endif ! if ( iplace .eq. 4  ) -------------------------------------------- 

      if ( iplace .eq. 6 ) then  ! ---------------------------------------------
      write(120+jper,2690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 2690 format(///100('-')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... Downstream of ',a37/100('-')) ! ------------------------- Di.ADL
*     write(100+jper,2890)uname(feeture),dname(jper) ! ++++++++++++++++++ Di.MON
 2890 format(///140('=')/'Apportionment of mean load', ! ++++++++++++++++ Di.MON
     &' ... Downstream of ',a37,2x,'for: ',a11/140('-')) ! ++++++++++++++ Di.MON
      endif ! if ( iplace .eq. 6 ) --------------------------------------------- 

      if ( iplace .eq. 4 .or. ip .eq. 6 ) then ! ===============================     
      write(120+jper,3690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 3690 format(///100('-')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... at ',a37/100('-')) ! ------------------------------------ Di.ADL
      write(100+jper,3698)uname(feeture)
 3698 format(160('-')/'Apportionment of mean load',
     &' ... at ',a37/160('-'))
      write(120+jper,1691)
*     write(120+jper,1691) ! -------------------------------------------- Di.ADL
 1691 format(
     &31x,'  contribution          accumulating'/
     &31x,' to the annual          total annual'/
     &31x,'     mean load             mean load'/100('-'))
      endif ! if ( iplace .ne. 4 .and. iplace .ne. 6 ) =========================
      endif ! if ( ip .eq. 1 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if ( zero check .gt. 1.0e-09 ) then !zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

          
*     mean from all discharges =================================================
      if ( ip .eq. 1 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then ! ----------
      write(100+jper,1090)(llmonchar(ip,jper,1,i),i=1,13)
 1090 format('Mean load from all discharges ',13a10)
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
 1692 format('Standard deviation of load    ',13a10)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
 1098 format('% to annual mean load ...     ',13a10)
      
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*(loadmon(ip,jper,1,13)/C(jper,1)))
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/XML),
     &i=1,12),(100.0*(loadmon(ip,jper,1,13)/C(jper,1)))
 1398 format('% to in-river mean load ...   ',13f10.2)
     
      write(100+jper,8398)(loadmon(ip,jper,1,i),
     &i=1,12),loadmon(ip,jper,1,13)
 8398 format('LOADMON  ...                  ',13f10.2)
     
      write(100+jper,7398)(cmon(jper,1,i),i=1,12),C(jper,1)
 7398 format('CMON ...                      ',13f10.2)
      endif ! if ( ip .eq. 1 etc -----------------------------------------------
*     all discharges ===========================================================
      
     
*     mean from sewage works (3) ========================================== (03)
      if ( ip .eq. 2 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1190)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1190 format('Mean from sewage works (3)    ',13a10,a10)
      write(120+jper,8190)llmonchar(ip,jper,1,13),runchar
 8190 format('Sewage works (3)              ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! if ( ip .eq. 2 ---------------------------------------------------
*     mean from sewage works (3) ========================================== (03)
   
   
*     mean from intermittents (12) ======================================== (12)
      if ( ip .eq. 3 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1295)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1295 format('Mean from intermittents (12)  ',13a10,a10)
      write(120+jper,8295)llmonchar(ip,jper,1,13),runchar
 8295 format('Intermittents (12)            ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from intermittents (12) ======================================== (12)


*     mean from industry (5) ============================================== (05)
      if ( ip .eq. 4 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1390)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1390 format('Mean load from industry (5)   ',13a10,a10)
      write(120+jper,7390)llmonchar(ip,jper,1,13),runchar
 7390 format('Industrial discharges (5)     ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from industry (5) ============================================== (05)


*     mean from mine waters (39) ========================================== (39)
      if ( ip .eq. 5 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1490)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1490 format('Mean from mine waters (39)    ',13a10,a10)
      write(120+jper,8490)llmonchar(ip,jper,1,13),runchar
 8490 format('Mine waters (39)              ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from mine waters (39) ========================================== (39)

      
*     mean from Other Point Sources (60) ================================== (60)
      if ( ip .eq. 28 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5490)(llmonchar(ip,jper,1,i),i=1,13),runchar
 5490 format('Mean from other pt sources (60)',13a10,a10)
      write(120+jper,4490)llmonchar(ip,jper,1,13),runchar
 4490 format('"Other" Point Sources (60)       ',5x,a10,12x,a10)  
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from Other Point Sources (60) ================================== (60)


*     mean from private wastewaters (61) ================================== (61)
      if ( ip .eq. 29 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5491)(llmonchar(ip,jper,1,i),i=1,13),runchar
 5491 format('Mean from private WWTW (61)   ',13a10,a10)
      write(120+jper,4491)llmonchar(ip,jper,1,13),runchar
 4491 format('Private wastewaters (61)      ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean from private wastewaters (61) ================================== (61)


*     mean load from headwaters (10) ====================================== (10)
      if ( ip .eq. 17 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5492)(llmonchar(ip,jper,1,i),i=1,13),runchar
 5492 format('Mean load from headwaters (10)',13a10,a10)
      write(120+jper,4492)llmonchar(ip,jper,1,13),runchar
 4492 format('Mean load from headwaters (10)',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      lmon(JDET,1,jmon) = amax1 ( 0.0, CM )
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/lmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/lmon(jper,1,13))
      endif
      call write line (jper)
      endif
*     mean load from headwaters (10) ====================================== (10)


*     mean load from reach diffuse ---------------------------------------------
      if ( ip .eq. 20 .and. loadmon(ip,jper,1,13) .gt. 1.0e-08 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5112)(llmonchar(ip,jper,1,i),i=1,13),runchar
 5112 format('Mean load from reach diffuse  ',13a10,a10)
      write(120+jper,4112)llmonchar(ip,jper,1,13),runchar
 4112 format('Reach diffuse inflow          ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     mean load from reach diffuse ---------------------------------------------


*     from livestock farming (25) ========================================= (25)
      if ( ip .eq. 6 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,3490)(llmonchar(ip,jper,1,i),i=1,13),runchar
 3490 format('From livestock farming (25)   ',13a10,a10)
      write(120+jper,3499)llmonchar(ip,jper,1,13),runchar
 3499 format('Livestock farming (25)        ',5x,a10,12x,a10,
     &' ... loadmon ... llmonchar')
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from livestock farming (25) ========================================= (25)


*     from arable farming (27) ============================================ (27)
      if ( ip .eq. 7 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6490)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6490 format('Mean from arable farming (27) ',13a10,a10)
      write(120+jper,4499)llmonchar(ip,jper,1,13),runchar
 4499 format('Arable farming (27)           ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from arable farming (27) ============================================ (27)


*     from highway runoff (29) ============================================ (29)
      if ( ip .eq. 8 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,5496)(llmonchar(ip,jper,1,i),i=1,13),runchar
 5496 format('Mean from highway runoff (29) ',13a10,a10)
      write(120+jper,5499)llmonchar(ip,jper,1,13),runchar
 5499 format('Highway runoff (29)           ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from highway runoff (29) ============================================ (29)


*     from urban runoff (31) ============================================== (31)
      if ( ip .eq. 9 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1690)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1690 format('Mean from urban runoff (31)   ',13a10,a10)
      write(120+jper,1699)llmonchar(ip,jper,1,13),runchar
 1699 format('Urban runoff (31)             ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from urban runoff (31) ============================================== (31)


*     from atmospheric deposition (33) ==================================== (33)
      if ( ip .eq. 10 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1790)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1790 format('Atmospheric deposition (33)   ',13a10,a10)
      write(120+jper,1799)llmonchar(ip,jper,1,13),runchar
 1799 format('Atmospheric deposition (33)   ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from atmospheric deposition (33) ==================================== (33)


*     from natural background (35) ======================================== (35)
      if ( ip .eq. 11 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1890)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1890 format('From natural background (35)  ',13a10,a10)
      write(120+jper,1899)llmonchar(ip,jper,1,13),runchar
 1899 format('Natural background (35)       ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from natural background (35) ======================================== (35)


*     from septic tanks (37) ============================================== (37)
      if ( ip .eq. 12 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1892)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1892 format('Mean from septic tanks (37)   ',13a10,a10)
      write(120+jper,8892)llmonchar(ip,jper,1,13),runchar
 8892 format('Septic tanks (37)             ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from septic tanks (37) ============================================== (37)


*     from aggregated CSOs (40) =========================================== (40)
      if ( ip .eq. 13 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1893)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1893 format('Mean from aggregated CSOs (40)',13a10,a10)
      write(120+jper,8893)llmonchar(ip,jper,1,13),runchar
 8893 format('Aggregated CSOs (40)          ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from aggregated CSOs (40) =========================================== (40)


*     from aggregated STWs (42) =========================================== (42)
      if ( ip .eq. 14 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1894)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1894 format('Mean from aggregated STWs (42)',13a10,a10)
      write(120+jper,8894)llmonchar(ip,jper,1,13),runchar
 8894 format('Aggregated STWs (42)          ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from aggregated STWs (42) =========================================== (42)


*     from diffuse mines (46) ============================================= (46)
      if ( ip .eq. 15 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,1895)(llmonchar(ip,jper,1,i),i=1,13),runchar
 1895 format('Mean from diffuse mines (46)  ',13a10,a10)
      write(120+jper,8895)llmonchar(ip,jper,1,13),runchar
 8895 format('Diffuse mines (46)            ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from diffuse mines (46) ============================================= (46)


*     from birds, boats and angling (48) ================================== (48)
      if ( ip .eq. 16 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6895)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6895 format('Mean: birds, boats and angling',13a10,a10)
      write(120+jper,6899)llmonchar(ip,jper,1,13),runchar
 6899 format('Birds, boats and angling (48) ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from birds, boats and angling (48) ================================== (48)
  
    
*     user defined (50) =================================================== (50)
      if ( ip .eq. 23 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6850)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6850 format('Mean: ',a24,13a10,a10)
      write(120+jper,6851)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6851 format(a24,11x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     user defined (50) =================================================== (50)
 

*     user defined (52) =================================================== (52)
      if ( ip .eq. 24 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6852)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6852 format('Mean: ',a24,13a10,a10)
      write(120+jper,6853)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6853 format(a24,11x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     user defined (52) =================================================== (52)
 

*     user defined (54) =================================================== (54)
      if ( ip .eq. 25 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6854)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6854 format('Mean: ',a24,13a10,a10)
      write(120+jper,6855)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6855 format(a24,11x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     user defined (54) =================================================== (54)


*     user defined (56) =================================================== (56)
      if ( ip .eq. 26 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6856)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6856 format('Mean: ',a24,13a10,a10)
      write(120+jper,6857)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6857 format(a24,11x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     user defined (56) =================================================== (56)


*     user defined (58) =================================================== (58)
      if ( ip .eq. 27 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6858)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6858 format('Mean: ',a24,13a10,a10)
      write(120+jper,6859)nameprop(ip),llmonchar(ip,jper,1,13),runchar
 6859 format(a24,11x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     user defined (58) =================================================== (58)

      
*     from headwaters and streams (48) ============================= (10 and (2)
      if ( n251 .eq. 1 ) then ! 5555555555555555555 apportionment of percentiles
      if ( ip .eq. 17 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,7995)(llmonchar(ip,jper,1,i),i=1,13),runchar
 7995 format('Mean: headwaters and streams  ',13a10,a10)
      write(120+jper,6995)llmonchar(ip,jper,1,13),runchar
 6995 format('Headwaters and streams        ',5x,a10,12x,a10)

      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! if ( ip .eq. 17 ) 5555555555555555555 apportionment of percentiles
*     from headwaters and streams (48) ============================= (10 and (2)
   
   
*     from diffuse inputs (13) ============================================ (13)
      if ( ip .eq. 18 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,7925)(llmonchar(ip,jper,1,i),i=1,13),runchar
 7925 format('Mean from diffuse inputs (13) ',13a10,a10)
      write(120+jper,6925)llmonchar(ip,jper,1,13),runchar
 6925 format('Diffuse inputs (13)           ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif ! from diffuse inputs (13)===================================== (13)
*     from diffuse inputs (13) ============================================ (13)

      
*     from diffuse inputs (15) ============================================ (15)
      if ( ip .eq. 19 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6926)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6926 format('Mean from diffuse inputs (15) ',13a10,a10)
      write(120+jper,7926)llmonchar(ip,jper,1,13),runchar
 7926 format('Diffuse inputs (15)           ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif
      call write line (jper)
      endif
*     from diffuse inputs (15) ============================================ (15)
 
     
*     from reach diffuse (20) ============================================= (20)
      if ( ip .eq. 20 .and. loadmon(ip,jper,1,13) .gt. 1.0e-12 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6986)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6986 format('Reach diffuse (20)            ',13a10,a10)
      write(120+jper,7986)llmonchar(ip,jper,1,13),runchar
 7986 format('Reach diffuse (20)            ',5x,a10,12x,a10)
	if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif ! from reach diffuse (20) 555555555555555555555555555555555555555555
      call write line (jper)
      endif ! from reach diffuse (20) 555555555555555555555555555555555555555555
*     from reach diffuse (20) ============================================= (20)
  
    
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     from gap filling for flows (21) ===================================== (21)
      if ( ip .eq. 21 .and. loadmon(ip,jper,1,13) .gt. 1.0e-08 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6911)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6911 format('Mean from gap filling (21)    ',13a10,a10)
      write(120+jper,7911)llmonchar(ip,jper,1,13),runchar
 7911 format('Gap filling (21)              ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif ! from gap filling for flows (21) 55555 apportionment of percentiles
      call write line (jper)
      endif ! from gap filling for flows (21) 5555555555555555555555555555555555
*     from gap filling for flows (21) ===================================== (21)
*     --------------------------------------------------------------------------          
*     from gap filling for river quality (22) ============================= (22)
      if ( ip .eq. 22 .and. loadmon(ip,jper,1,13) .gt. 1.0e-08) then ! 555555555
      running mean = running mean + loadmon(ip,jper,1,13)
      call set format for printout(runchar,running mean)
      write(100+jper,6941)(llmonchar(ip,jper,1,i),i=1,13),runchar
 6941 format('Mean from gap filling (22)    ',13a10,a10)
      write(120+jper,7941)llmonchar(ip,jper,1,13),runchar
 7941 format('Gap filling (22)              ',5x,a10,12x,a10)
      if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) then
      write(100+jper,1692)(llmonchar(ip,jper,2,i),i=1,13)
      write(100+jper,1098)(llmonchar2(ip,jper,1,i),i=1,13)
      write(100+jper,1398)(100.0*(loadmon(ip,jper,1,i)/cmon(jper,1,i)),
     &i=1,12),(100.0*loadmon(ip,jper,1,13)/C(jper,1))
      endif ! if ( loadmon(ip,jper,1,i13) .gt. 1.0e-09 ) 55555555555555555555555
      call write line (jper)
      endif ! if ( ip .eq. 22 ) ... from gap filling (22) 5555555555555555555555
*     from gap filling for river quality (22) ============================= (22)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg  


      endif ! if ( n251 .eq. 1 ) 555555555555555555 apportionment of percentiles
      
      endif ! if ( zero check .gt. 1.0e-09 ) zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      endif ! if ( nobigout .le. 0 ) ++++++++++++++++++++++++++++++++++++++++++++

 2000 continue ! do 2000 iiip = 1, nprop ! 5555555555555555555555555555555555555

      write(100+jper,2098) ! -------------------------------------------- Di.MON
 2098 format(136('='))
      call set format for printout(runchar,TGLODE2(jper,i13))
      write(120+jper,6692)runchar ! ------------------------------------- Di.ADL
 6692 format(100('-')/
     &'Total mean load               ',5x,a10,'   ... TGLODE'/100('='))
      
      return
      end

      
      
      subroutine write line (jper)     
      include 'COMMON DATA.FOR'
      write(100+jper,1)
    1 format(30('-'),13(2x,8('-')))
      return
      end

      
      subroutine write out comma separated monthly apportionment (jper)
      include 'COMMON DATA.FOR'
      dimension xadd(13)
      character *10 Tenchars(13)
      
      do i = 1, 13
      xadd(i) = 0.0
      enddo
      nprap = nprop ! 5555555555555555555555555555555555555555555555555555555555

      do 4500 iiip = 1, 23 ! 18 and 19 etc are excluded 555555555555555555555555
      ip = jorder(iiip) 
      if ( ip .gt. 1 ) then
      do i = 1, 13
      xadd(i) = xadd(i) + loadmon(ip,jper,1,i)
      enddo
      endif

      if ( n257 .eq. 1 ) then ! 777777777777777777777777777777777777777777777777
      if ( ip .eq. 1 ) then
      write(210+jper,1290)GIScode(feeture),jxtype,unamex,rnamex, ! ------ Di.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river, ! -------- Di.CSV
     &(loadmon(ip,jper,1,i),i=1,12)
 1290 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all discharges (3 12 5 39 60 61)',',',a37
     &,(',',1pe12.4),',3 12 5 39 60 61,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 2 ) then
      write(210+jper,2290)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 2290 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all Sewage Works (3)',',',a37,
     &(',',1pe12.4),',3,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 3 ) then
      write(210+jper,3290)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     7(loadmon(ip,jper,1,i),i=1,12)
 3290 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all Intermittent Discharges (12)',',',a37,
     &(',',1pe12.4),',12,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 4 ) then
      write(210+jper,4290)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 4290 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all Industrial Discharges (5)',',',a37,
     &(',',1pe12.4),',5,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      
      if ( ip .eq. 5 ) then
      write(210+jper,5290)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),
     &Length of main river,(loadmon(ip,jper,1,i),i=1,12)
 5290 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all Mine Waters (39)',',',a37,
     &(',',1pe12.4),',39,'0pf12.2',,,,',12(',',1pe12.4))
      endif

      if ( ip .eq. 28 ) then
      write(210+jper,6090)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6090 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all "Other" Discharges (60)',',',a37,
     &(',',1pe12.4),',60,'0pf12.2',,,,',12(',',1pe12.4))
      endif

      if ( ip .eq. 29 ) then
      write(210+jper,6190)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6190 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from all Private Wastewaters (61)',',',a37,
     &(',',1pe12.4),',61,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      
      if ( ip .eq. 17 ) then ! ----------------------------------------- DDi.CSV
      write(210+jper,6990)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6990 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Headwaters etc (10)',',',a37,
     &(',',1pe12.4),',10,'0pf12.2',,,,',12(',',1pe12.4))
      endif

      if ( ip .eq. 6 ) then
      write(210+jper,3690)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 3690 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads load from Livestock Farming (25)',',',a37,
     &(',',1pe12.4),',25,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      
      if ( ip .eq. 7 ) then
      write(210+jper,4690)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 4690 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from from Arable Farming (27)',',',a37,
     &(',',1pe12.4),',27,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 8 ) then
      write(210+jper,5690)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 5690 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from from Highway Runoff (29)',',',a37,
     &(',',1pe12.4),',29,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 9 ) then
      write(210+jper,6690)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6690 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Urban Runoff (31)',',',a37,
     &(',',1pe12.4),',31,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 10 ) then
      write(210+jper,6790)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6790 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Atmospheric Deposition (33)',',',a37,
     &(',',1pe12.4),',33,'0pf12.2',,,,',12(',',1pe12.4))
      endif
*     from natural background (35) ---------------------------------------------
      if ( ip .eq. 11 ) then
      write(210+jper,6890)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6890 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Natural Background (35)',',',a37,
     &(',',1pe12.4),',35,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 12 ) then
      write(210+jper,6892)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6892 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Septic Tanks (37)',',',a37,
     &(',',1pe12.4),',37,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 13 ) then
      write(210+jper,6893)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6893 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Aggregated CSOs (40)    ',',',a37,
     &(',',1pe12.4),',40,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 14 ) then
      write(210+jper,6894)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6894 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Aggregated STWs (42)',',',a37,
     &(',',1pe12.4),',42,'0pf12.2',,,,',12(',',1pe12.4))
      endif
*     from diffuse mines (46) --------------------------------------------------
      if ( ip .eq. 15 ) then
      write(210+jper,6895)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6895 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Diffuse Mines (46)',',',a37,
     &(',',1pe12.4),',46,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 16 ) then
      write(210+jper,6195)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river, ! ------- DDi.CSV
     &(loadmon(ip,jper,1,i),i=1,12) ! ---------------------------------- DDi.CSV
 6195 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from Birds Boats and Angling (48)',',',a37,
     &(',',1pe12.4),',48,'0pf12.2',,,,',12(',',1pe12.4))
      endif
     
*     ==========================================================================
      if ( ip .eq. 23 ) then
      write(210+jper,6150)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river, ! ------- DDi.CSV
     &(loadmon(ip,jper,1,i),i=1,12) ! ---------------------------------- DDi.CSV
 6150 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from User-defined (50)',',',a37,
     &(',',1pe12.4),',50,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 24 ) then
      write(210+jper,6152)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river, ! ------- DDi.CSV
     &(loadmon(ip,jper,1,i),i=1,12) ! ---------------------------------- DDi.CSV
 6152 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from User-defined (52)',',',a37,
     &(',',1pe12.4),',52,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 25 ) then
      write(210+jper,6154)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6154 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from User-defined (54)',',',a37,
     &(',',1pe12.4),',54,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 26 ) then
      write(210+jper,6156)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6156 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from User-defined (56)',',',a37,
     &(',',1pe12.4),',56,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      if ( ip .eq. 27 ) then
      write(210+jper,6158)GIScode(feeture),jxtype,unamex,rnamex, ! ----- DDi.CSV
     &nameprop(ip),loadmon(ip,jper,1,13),Length of main river,
     &(loadmon(ip,jper,1,i),i=1,12)
 6158 format(' ',a40,',',i4,',',a40,',',a20,',',
     &'Mean loads from User-defined (58)',',',a37,
     &(',',1pe12.4),',58,'0pf12.2',,,,',12(',',1pe12.4))
      endif
      endif ! 777777777777777777777777777777777777777777777777777777777777777777

      if ( ip .eq. 1 ) then
      write(110+jper,1690)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 1690 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from all upstream discharges (3 12 5 and 39)',
     &',',a11,13(',',1pe12.4),',',i4,', 3 12 5 39 60 61') ! ------------- Di.CSV
      endif
      if ( ip .eq. 2 ) then
      write(110+jper,2690)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 2690 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from sewage works (3)',',',a11,
     &13(',',1pe12.4),',',i4,', 3') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 3 ) then
      write(110+jper,4490)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 4490 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from intermittent discharges (12)',',',a11,
     &13(',',1pe12.4),',',i4,',12') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 4 ) then
      write(110+jper,8690)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 8690 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from industry (5)',',',a11,
     &13(',',1pe12.4),',',i4,', 5') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 5 ) then
      write(110+jper,5890)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 5890 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from mine waters (39)',
     &',',a11,13(',',1pe12.4),',',i4,',39') ! --------------------------- Di.CSV
      endif
      if ( ip .eq. 28 ) then
      write(110+jper,5693)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 5693 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from other discharges (60)',
     &',',a11,13(',',1pe12.4),',',i4,',60') ! --------------------------- Di.CSV
      endif
      if ( ip .eq. 29 ) then
      write(110+jper,5694)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 5694 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from private discharges (61)',
     &',',a11,13(',',1pe12.4),',',i4,',61') ! --------------------------- Di.CSV
      endif
*     if ( ip .eq. 17 ) then ! ----------- monthly mean ---- 110 -------- Di.CSV
*     write(110+jper,5094)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
*    &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
*5094 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
*    &'from headwaters etc (10)',
*    &',',a11,13(',',1pe12.4),',',i4,',10') ! --------------------------- Di.CSV
*     endif ! ----------------------------------------------------------- Di.CSV
      if ( ip .eq. 6 ) then
      write(110+jper,3691)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 3691 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from livestock farming (25)',',',a11,
     &13(',',1pe12.4),',',i4,',25') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 7 ) then
      write(110+jper,4691)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 4691 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from arable farming (27)',',',a11,
     &13(',',1pe12.4),',',i4,',27') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 8 ) then
      write(110+jper,5691)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 5691 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from highway runoff (29)',
     &',',a11,13(',',1pe12.4),',',i4,',29') ! --------------------------- Di.CSV
      endif
      if ( ip .eq. 9 ) then
      write(110+jper,6691)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6691 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from urban runoff (31)',
     &',',a11,13(',',1pe12.4),',',i4,',31') ! --------------------------- Di.CSV
      endif
      if ( ip .eq. 10 ) then
      write(110+jper,6791)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6791 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from atmospheric deposition (33)',
     &',',a11,13(',',1pe12.4),',',i4,',33') ! --------------------------- Di.CSV
      endif
*     from natural background (35) -------------------------------------- Di.CSV
      if ( ip .eq. 11 ) then
      write(110+jper,6891)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6891 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from natural background (35)',
     &',',a11,13(',',1pe12.4),',',i4,',35') ! --------------------------- Di.CSV
      endif
      if ( ip .eq. 12 ) then
      write(110+jper,6192)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6192 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from septic tanks (37)    ',',',a11,
     &13(',',1pe12.4),',',i4,',37') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 13 ) then
      write(110+jper,6193)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6193 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from Aggregated CSOs (40)    ',',',a11,
     &13(',',1pe12.4),',',i4,',40') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 14 ) then
      write(110+jper,6194)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6194 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from Aggregated STWs (42)    ',',',a11,
     &13(',',1pe12.4),',',i4,',42') ! ----------------------------------- Di.CSV
      endif
*     from diffuse mines (46) ------------------------------------------- Di.CSV
      if ( ip .eq. 15 ) then
      write(110+jper,6095)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 6095 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from Diffuse Mines (46)      ',',',a11,
     &13(',',1pe12.4),',',i4,',46') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 16 ) then
      write(110+jper,7195)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 7195 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from boats and angling (48) ',',',a11,
     &13(',',1pe12.4),',',i4,',48') ! ----------------------------------- Di.CSV
      endif
*     ==========================================================================
      if ( ip .eq. 23 ) then
      write(110+jper,7150)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 7150 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from type of source specified by user (50)',',',a11,
     &13(',',1pe12.4),',',i4,',50') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 24 ) then
      write(110+jper,7152)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 7152 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from type of source specified by user (52)',',',a11,
     &13(',',1pe12.4),',',i4,',52') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 25 ) then
      write(110+jper,7154)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 7154 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from type of source specified by user (54)',',',a11,
     &13(',',1pe12.4),',',i4,',54') ! ----------------------------------- Di.CSV
      endif
      if ( ip .eq. 26 ) then
      write(110+jper,7156)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 7156 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from type of source specified by user (56)',',',a11,
     &13(',',1pe12.4),',',i4,',56') ! ----------------------------------- Di.CSV
      endif
	if ( ip .eq. 27 ) then
      write(110+jper,7158)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype ! ---------------- Di.CSV
 7158 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from type of source specified by user (58)',',',a11, ! ----------- Di.CSV
     &13(',',1pe12.4),',',i4,',58') ! ----------------------------------- Di.CSV
      endif

*     ==========================================================================
 4500 continue ! do 4500 iiip = 1, 23 555555555555555555555555555555555555555555

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
 3096 format('ALL MEAN CONTRIBUTIONS OF LOAD',13a10/160('-'))
*     &13('  ',8('-'))
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars(i),cmon(jper,1,i))
      enddo
      endif
      call set format for printout(Tenchars(13),C(jper,1))
      write(100+jper,1040)(Tenchars(i),i=1,13)
 1040 format('TOTAL IN-RIVER MEAN CONC.',5x,13a10)

      if ( n251 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
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
      call change colour of text (10) ! green
      write( *,1005) xcheck,dname(jper)
      call set screen text colour
 1005 format(
     &'* Breakdown differs from measured by',f8.2,' % for ',a11)
      write(09,1005) xcheck,dname(jper)
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
      endif ! if ( n251 .eq. 1 ) 55555555555555555555555555555555555555555555555
      endif ! if ( nobigout .le. 0 )
      
*     -------------- finished writing out commma-separated monthly apportionment

      return
      end
