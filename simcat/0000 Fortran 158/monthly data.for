*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File: MONTH.FOR ... 4302 lines -------------------------------------------
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
      dimension XL(13),XU(13),XR(13)
      real lcontot(13)

      character *10 Tenchars (13)

*     line removed at the request of SAGIS
      !if ( munthly structure .eq. 0 ) return ! no requirement for monthly loads
      
      jxtype = 0
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
      fmon(2,imon) = EXP(GEFM-t95*GEFS) ! removed because of 110 update
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

      t95 = errx ( 0.95 )
      do 6612 jper = 1, ndet ! loop on determinands ----------------------------
      if ( QTYPE (jper) .ne. 4) then ! check for excluded determinands ---------

      do imon = 1, 12
      XL(imon) = 0.0
      XU(imon) = 0.0
      enddo
      XL(13) = CL(2,jper) ! retrieve the values of the confidence limits -------
      XU(13) = CL(3,jper) ! retrieve the values of the confidence limits -------

      print check = 0.0 ! initialise check for need to print -------------------

      if ( munthly structure .ne. 1 ) then 
          do i = 1,12
          cmon(jper,1,i) = C(jper,1)
          cmon(jper,2,i) = C(jper,2)
          enddo
      endif
      
      do 42 imon = 1, 12 ! loop on months ---------- calculate confidence limits
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
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout3(Tenchars(i),cmon(jper,1,i))
      enddo
      endif ! if ( munthly structure .eq. 1 )
      call set format for printout3(Tenchars (13),C(jper,1))
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13) ! ------------ Di.MON
 1000 format('Mean ',a11,4x,13a8)
      
      if ( C(jper,1) .gt. 1.0e-8 ) then ! ------------------------------- Di.MON
      if ( munthly structure .eq. 1 ) then ! ---------------------------- Di.MON
      do i = 1, 12
      xxperc = 100.0*((XU(i)-cmon(jper,1,i))/cmon(jper,1,i))
      write(Tenchars(i),3009)nint(xxperc)
 3009 format(i7)
      enddo
      endif ! if ( munthly structure .eq. 1 ) --------------------------- Di.MON
      xxperc = 100.0*((XU(13)-C(jper,1))/C(jper,1))
      write(Tenchars(13),3009)nint(xxperc)
      write(100+jper,1006)(Tenchars(i),i=1,13) ! ------------------------ Di.MON
 1006 format('Plus or minus ...   ',13(a7,'%')) ! ----------------------- Di.MON
      call write line2 (jper)
      endif ! if ( C(jper,1) .gt. 1.0e-8 ) ------------------------------ Di.MON
      
      endif ! if ( nobigout .le. 0 ) ------------------------------------ Di.MON
*     ------------------------ have written out tables of monthly concentrations

      if ( iplace .eq. 1 ) write(unamex,1276)rname(IREACH)
 1276 format('Start of reach: ',a16)
      if ( iplace .eq. 3 ) write(unamex,1277)rname(IREACH)
 1277 format('End of reach: ',a16)

      
*     write out comma-separated monthly concentrations -------------------------
      if ( munthly structure .eq. 1 ) then
      write(110+jper,2300)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(cmon(jper,1,i),i=1,12),C(jper,1),jxtype,
     &Length of main river
 2300 format(' ',a40,',',a40,',',a20,',','Mean concentrations for ',',',
     &a11,13(',',1pe12.4),',',i4,',,,,,,,,,,',0pf11.3) ! ---------------- Di.CSV
      else
      write(110+jper,2300)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(C(jper,1),i=1,13),jxtype,
     &Length of main river
      endif
      
      if ( munthly structure .eq. 1 ) then
      write(110+jper,2301)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(cmon(jper,2,i),i=1,12),C(jper,2),jxtype
 2301 format(' ',a40,',',a40,',',a20,',','Standard deviations for ',
     &'concentrations',',',a11,13(',',1pe12.4),',',i4) ! ---------------- Di.CSV
      else
      write(110+jper,2301)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(C(jper,2),i=1,13),jxtype
      endif
           
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

      do im = 1, 13 
      lcontot(im) = 0.0 ! initialise sums of mean concentrations --------------- 
      enddo

      do 3056 iiip = 1, 27 ! 17, 18 and 19 are excluded 555555555555555555555555
      ip = jorder(iiip) ! sequencing used for output of types of pollution ----- 
      if ( ip .gt. 1 ) then ! add this to the sum of contributions -------------
      do im = 1, 13 ! types 17, 18 and 19 are excluded from the sums -----------
      lcontot(im) = lcontot(im) + lconmon(ip,jper,1,im) !sum mean concentrations 
      enddo ! do im = 1, 13 ----------------------------------------------------
      endif ! if ( ip .gt. 1 ) -------------------------------------------------

*     =============================================--------------===============
*     STORED CONTRIBUTIONS OF LOAD ================ Feature Type ===============
*     =============================================--------------===============
*     ( 1) = 'Mean from all discharges (3, 12, 5, 39, 60 and 61)'
*     ( 2) = 'Added by sewage effluents                      (3)'
*     ( 3) = 'Intermittent discharges of sewage             (12)'
*     ( 4) = 'Added by industrial discharges                 (5)'
*     ( 5) = 'Added by mine waters                          (39)'
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
*     (17) = 'Boundaries and tributaries              (2 and 10)'
*     (18) = 'Diffuse input                                 (13)'
*     (19) = 'Diffuse input                                 (15)'
*     (20) = 'Reach diffuse                    (no Feature code)'
*     (21) = 'Gap filling of flows             (no Feature code)'
*     (22) = 'Gap filling of quality           (no Feature code)'
*     (23) = 'User-named diffuse input                      (50)'
*     (24) = 'User-named diffuse input                      (52)'
*     (25) = 'User-named diffuse input                      (54)'
*     (26) = 'User-named diffuse input                      (56)'
*     (27) = 'User-named diffuse input                      (58)'
*     (28) = 'Other point sources                           (60)'
*     (29) = 'Private wastewaters                           (61)'
*     (30) = 'Fish farms                                    (62)'
*     (NTD) = 'Grand total                          (everything)'
*     (NTF) = 'Total from diffusing pollutions         (several)'
*     ==========================================================================

     

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
      do i = 1,13
      XR(i) = 0.0
      enddo
      
      if ( C(jper,1) .gt. 0.0000001 ) then
      if ( cmon(jper,1,i) .gt. 0.0000001 ) then
      do i = 1,13
      XR(i) = 100.0*(cmon(jper,1,i)-lcontot(i))/cmon(jper,1,i)
      enddo
      endif
          
      write(110+jper,8561)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &(XR(i),i=1,13), ! ------ Di.CSV
     &jxtype ! ---------------------------------------------------------- Di.CSV
 8561 format(' ',a40,',',a40,',',a20,',' ! ------------------------------ Di.CSV
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

          
*     ========================================================================== 
*     =========================== check for non-partitioned (ordinary) chemicals
*     ========================================================================== 
      if ( detype (jper) .lt. 900 ) then !  non-partitioned (ordinary) chemicals
      do im = 1,13
      exceedences50 (jper, im) = 0.0 
      exceedences90 (jper, im) = 0.0
      exceedences95 (jper, im) = 0.0
      enddo ! do im = 1,13
          
      do is = 1, NS ! loop on shots --------------------------------------------
      imonth = qmonth (is) ! set the month for this shot -----------------------

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do im = j1, 13
*     ===================================== non-partitioned (ordinary) chemicals
      if ( im .lt. 13 ) then ! -------------------------------------------------
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
      endif ! if ( imonth .eq. im ) --------------------------------------------
      else ! if ( im .ge. 13 ) -------------------------------------------------
      if ( CMS(jper,is) .gt. C(jper,1) + 1.0e-09 ) then
      exceedences50 (jper, im) = exceedences50 (jper, im) + 1.0
      endif
      if ( CMS(jper,is) .gt. C(jper,4) + 1.0e-09 ) then
      exceedences90 (jper, im) = exceedences90 (jper, im) + 1.0
      endif
      if ( CMS(jper,is) .gt. C(jper,3) + 1.0e-09 ) then
      exceedences95 (jper, im) = exceedences95 (jper, im) + 1.0
      endif
      endif ! if ( im .lt. 13 ) ------------------------------------------------
      enddo ! do im = j1, 13 ---------------------------------------------------
      enddo ! do is = 1, NS loop on shots --------------------------------------
      
      if ( Detype(jper) .eq. 104 ) then ! for dissolved oxygen ooooooooooooooooo
      exceedences50(jper,13) = 100.0*(NS - exceedences50(jper,13))/NS ! oooooooo
      exceedences90(jper,13) = 100.0*(NS - exceedences90(jper,13))/NS ! oooooooo
      exceedences95(jper,13) = 100.0*(NS - exceedences95(jper,13))/NS ! oooooooo
      do im = 1, 12 ! oooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      XN = NS*days in months(im)/365.0 ! number of shots per month ooooooooooooo
      exceedences50(jper,im) = 100.0*(XN - exceedences50(jper,im))/XN ! oooooooo
      exceedences90(jper,im) = 100.0*(XN - exceedences90(jper,im))/XN ! oooooooo
      exceedences95(jper,im) = 100.0*(XN - exceedences95(jper,im))/XN ! oooooooo
      enddo ! do im = 1, 12 oooooooooooooooooooooooooooooooooooooooooooooooooooo   
      else ! if ( Detype(jper) .eq. 104 ) dissolved oxygen ooooooooooooooooooooo
      exceedences50(jper,13) = 100.0*(exceedences50(jper,13))/NS ! 
      exceedences90(jper,13) = 100.0*(exceedences90(jper,13))/NS ! 
      exceedences95(jper,13) = 100.0*(exceedences95(jper,13))/NS ! 
      do im = 1, 12 ! 
      XN = NS*days in months(im)/365.0 ! number of shots per month 
      exceedences50(jper,im) = 100.0*(exceedences50(jper,im))/XN ! 
      exceedences90(jper,im) = 100.0*(exceedences90(jper,im))/XN ! 
      exceedences95(jper,im) = 100.0*(exceedences95(jper,im))/XN ! 
      enddo ! do im = 1, 12    
      endif ! if ( Detype(jper) .eq. 104 ) 
      
*     write out tables of exceedence -------------------------------------------
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( exceedences50(jper,13) .gt. 1.0e-8 ) then
      if ( Detype(jper) .eq. 104 ) then
      write(100+jper,4421)(nint(exceedences50(jper,i)),i=1,13) ! -------- Di.MON
 4421 format('% below mean        ',13i8)
      write(100+jper,4501)(nint(exceedences90(jper,i)),i=1,13) ! -------- Di.MON
 4501 format('% below Q10 conc.   ',13i8)
      write(100+jper,4601)(nint(exceedences95(jper,i)),i=1,13) ! -------- Di.MON
 4601 format('% below Q05 conc.   ',13i8)
      call write line2 (jper)
      else ! if ( Detype(jper) .eq. 104 )
      write(100+jper,4001)(nint(exceedences50(jper,i)),i=1,13) ! -------- Di.MON
 4001 format('% above mean        ',13i8)
      write(100+jper,4101)(nint(exceedences90(jper,i)),i=1,13) ! -------- Di.MON
 4101 format('% above Q90 conc.   ',13i8)
      write(100+jper,4101)(nint(exceedences95(jper,i)),i=1,13) ! -------- Di.MON
 4201 format('% above Q95 conc.   ',13i8)
      call write line2 (jper)
      endif ! if ( Detype(jper) .eq. 104 )
      endif ! if ( exceedences50(jper,13) .gt. 1.0e-8 ) -----------------------
      endif ! if ( nobigout .le. 0 ) ===========================================
      endif ! if ( detype (jper) .lt. 900 ) non-partitioned (ordinary) chemicals
*     ===================================== non-partitioned (ordinary) chemicals 
*     ==========================================================================
      

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( detype (jper) .ge. 900 ) then ! +++++++++++++++ partitioned chemicals
      do i3 = 1, 3 ! loop on mean Q90 and Q95 ++++++++++++ partitioned chemicals
      do im = 1, 13
      exceedences50 (jper, im) = 0.0 
      exceedences90 (jper, im) = 0.0
      exceedences95 (jper, im) = 0.0
      enddo ! do im = 1, 13
       
      if ( i3 .eq. 1 ) then
          compx1 = C(jper,1) ! total mean
          compx4 = C(jper,4) ! total mean
          compx3 = C(jper,3) ! total mean
      endif
      if ( i3 .eq. 2 ) then
          compx1 = cpart2(jper,1) ! solid mean
          compx4 = cpart2(jper,4) ! solid Q90
          compx3 = cpart2(jper,3) ! solid Q95
      endif
      if ( i3 .eq. 3 ) then
          compx1 = cpart1(jper,1) ! dissolved mean
          compx4 = cpart1(jper,4) ! dissolved Q90
          compx3 = cpart1(jper,3) ! dissolved Q95
      endif

      do is = 1, NS ! ++++++++++++++++++++++++++++++++++++ partitioned chemicals
      im = qmonth (is) ! set the month for this shot +++++++++++++++++++++++++++
      valm = CMS(jper,is)              ! total shot
      if ( i3 .eq. 2 ) valm = PMS2(jper,is) ! solid shot
      if ( i3 .eq. 3 ) valm = PMS1(jper,is) ! dissolved shot
      
      
      if ( valm .gt. compx1 + 1.0e-09 ) then ! ++++++++++++++++++++++++++++ mean
      exceedences50 (jper,im) = exceedences50 (jper,im) + 1.0 ! months +++++++++
      exceedences50 (jper,13) = exceedences50 (jper,13) + 1.0 ! whole year +++++
      endif ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ mean
      if ( valm .gt. compx4 + 1.0e-09 ) then ! +++++++++++++++++++ 90-percentile
      exceedences90 (jper,im) = exceedences90 (jper,im) + 1.0 ! months +++++++++
      exceedences90 (jper,13) = exceedences90 (jper,13) + 1.0 ! whole year +++++
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++ 90-percentile
      if ( valm .gt. compx3 + 1.0e-09 ) then ! +++++++++++++++++++ 95-percentile
      exceedences95 (jper,im) = exceedences95 (jper,im) + 1.0 ! months +++++++++
      exceedences95 (jper,13) = exceedences95 (jper,13) + 1.0 ! whole year +++++
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++ 95-percentile
      enddo ! do is = 1, NS ++++++++++++++++++++++++++++++ partitioned chemicals

      do im = 1, 12
      if ( NSM(jper,im+1) .gt. 0 ) then
      exceedences50(jper,im) = 100.0 * exceedences50(jper,im)
     &                                / NSM(jper,im+1)
      exceedences90(jper,im) = 100.0 * exceedences90(jper,im)
     &                                / NSM(jper,im+1)
      exceedences95(jper,im) = 100.0 * exceedences95(jper,im)
     &                                / NSM(jper,im+1)
      endif
      enddo ! do im = 1, 12 ++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      exceedences50(jper,13) = 100.0 * exceedences50(jper,13)/NS
      exceedences90(jper,13) = 100.0 * exceedences90(jper,13)/NS
      exceedences95(jper,13) = 100.0 * exceedences95(jper,13)/NS
      
*     write out tables of exceedence +++++++++++++++++++++++++++++++++++++++++++
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( exceedences50(jper,13) .gt. 1.0e-12 ) then
      if ( i3 .eq. 1 ) then
      write(100+jper,5001)(nint(exceedences50(jper,i)),i=1,13) ! ++++++++ Di.MON
 5001 format('% above mean total  ',13i8)
      write(100+jper,5101)(nint(exceedences90(jper,i)),i=1,13) ! ++++++++ Di.MON
 5101 format('% above Q90 total   ',13i8)
      write(100+jper,6201)(nint(exceedences95(jper,i)),i=1,13) ! ++++++++ Di.MON
 6201 format('% above Q95 total   ',13i8)
      endif
      if ( i3 .eq. 2 ) then
      write(100+jper,5501)(nint(exceedences50(jper,i)),i=1,13) ! ++++++++ Di.MON
 5501 format('% above mean solid  ',13i8)
      write(100+jper,5601)(nint(exceedences90(jper,i)),i=1,13) ! ++++++++ Di.MON
 5601 format('% above Q90 solid   ',13i8)
      write(100+jper,6601)(nint(exceedences95(jper,i)),i=1,13) ! ++++++++ Di.MON
 6601 format('% above Q95 solid   ',13i8)
      endif
      if ( i3 .eq. 3 ) then
      write(100+jper,5701)(nint(exceedences50(jper,i)),i=1,13) ! ++++++++ Di.MON
 5701 format('% above mean diss.  ',13i8)
      write(100+jper,5801)(nint(exceedences90(jper,i)),i=1,13) ! ++++++++ Di.MON
 5801 format('% above Q90 diss.   ',13i8)
      write(100+jper,6801)(nint(exceedences95(jper,i)),i=1,13) ! ++++++++ Di.MON
 6801 format('% above Q95 diss,   ',13i8)
      endif
      call write line2 (jper)
      endif ! if ( exceedences50(jper,13) .gt. 1.0e-12 ) +++++++++++++++++++++++
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++
      enddo ! do i3 = 1, 3 +++++++++++++++++++++++++++++++ partitioned chemicals
      endif ! if ( detype (jper) .ge. 900 ) ++++++++++++++ partitioned chemicals 
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      

*     --------------------------------------------------------------------------
      endif ! if ( QTYPE (jper) .ne. 4 )

      if ( iplace .eq. 1 ) write(unamex,1276)rname(IREACH)
      if ( iplace .eq. 3 ) write(unamex,1277)rname(IREACH)

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

      !call write out comma separated monthly apportionment (jper)

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
      if ( lmon(jper,1,13) .gt. 0.000001 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout3(Tenchars (i),lmon(jper,1,i))
      enddo
      endif
      call set format for printout3(Tenchars (13),lmon(jper,1,13)) ! 20('-'),13(2x,6('-'))
      write(100+jper,2014)lunits(jper),(Tenchars (i),i=1,13)
 2014 format('Mean load (',a4,')',4x,13a8)

      if ( munthly structure .eq. 1 ) then ! set up print out ------------------
      do i = 1, 13 
      xxperc = 100.0*((XU(i)-lmon(jper,1,i))/lmon(jper,1,i))
      write(Tenchars(i),3009)nint(xxperc)
      enddo
      endif ! if ( munthly structure .eq. 1 ) ----------------------------------

      write(100+jper,1106)(Tenchars(i),i=1,13)
 1106 format('Plus or minus ...   ',13(a7,'%')) ! ----------------------- Di.MON

      endif ! if ( lmon(jper,1,i) .gt. 1.0 e-12 )
      write(100+jper,1107)
 1107 format(124('='))
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE(jper) .ne. 4 )
      
      call write out the monthly apportionment (jper,iplace) ! #################

      call write the monthly data in tables (jper)
      call write the monthly data in comma separated form (jper,iplace)

      write(110+jper,3404)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(lmon(jper,1,i),i=1,13),jxtype
 3404 format(' ',a40,',',a40,',',a20,',','Mean total loads for:',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3401)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(lmon(jper,2,i),i=1,13),jxtype
 3401 format(' ',a40,',',a40,',',a20,',',
     &'Standard deviations for total loads:',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3501)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XL(i),i=1,13),jxtype
 3501 format(' ',a40,',',a40,',',a20,',',
     &'Lower confidence limits on mean total loads:',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      write(110+jper,3506)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XU(i),i=1,13),jxtype
 3506 format(' ',a40,',',a40,',',a20,',',
     &'Upper confidence limits on mean total loads:',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
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
      print check = 0.0 ! dissolved metal - initialise check for need to print -
      
      do 42 imon = 1, 13 ! =====================================================

      if ( munthly structure .ne. 1 ) then
      P1mon(jper,1,imon) = P1mon(jper,1,13)
      P1mon(jper,2,imon) = P1mon(jper,2,13)
      endif

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
      if ( C(jper,1) .gt. 1.0e-8 ) then ! ------------------------------- Di.MON
      call set format for printout3(Tenchars(13),cpart1(jper,1))
      do i = 1, 12
      call set format for printout3(Tenchars(i),p1mon(jper,1,i)) ! --- dissolved  
      enddo
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Diss. ',a11,3x,13a8) ! ---------------------- dissolved mean metal
      
*     if ( cpart1(jper,1) .gt. 1.0e-12 ) then
      !call set format for printout3(Tenchars(13),cpart1(jper,2))
      !do i = 1, 12
      !call set format for printout3(Tenchars(i),p1mon(jper,2,i)) ! -- dissolved 
      !enddo
      !if ( print check .gt. 1.0e-08 ) write(100+jper,1001)
      !&(Tenchars(i),i=1,13)
      !1001 format('Standard deviation  ',13a8) ! ----------- standard deviation
      !do i = 1, 13
      !call set format for printout3(Tenchars(i),XL(i))
      !enddo
      !if ( print check .gt. 1.0e-09 ) write(100+jper,1005) ! ----------- Di.MON
      !&(Tenchars(i),i=1,13)
      !1005 format('Lower limit on diss.',13a8) ! ------- lower confidence limit
      !do i = 1, 13
      !call set format for printout3(Tenchars(i),XU(i))
      !enddo
      !if ( print check .gt. 1.0e-09 ) write(100+jper,1006) ! ----------- Di.MON
      !&(Tenchars(i),i=1,13)
      !1006 format('Upper limit on diss.',13a8) ! ------- upper confidence limit
      !call write line2 (jper)
*     endif ! if ( cpart1(jper,1) .gt. 1.0e-12 )

      if ( munthly structure .eq. 1 ) then ! ---------------------------- Di.MON
      do i = 1, 12
      xxperc = 100.0*((XU(i)-p1mon(jper,1,i))/p1mon(jper,1,i)) 
      write(Tenchars(i),3009)nint(xxperc)
 3009 format(i7)
      enddo
      endif ! if ( munthly structure .eq. 1 ) --------------------------- Di.MON
      xxperc = 100.0*((XU(13)-p1mon(jper,1,13))/p1mon(jper,1,13))
      write(Tenchars(13),3009)nint(xxperc)
      write(100+jper,1206)(Tenchars(i),i=1,13) ! ------------------------ Di.MON
 1206 format('Plus or minus ...   ',13(a7,'%')) ! ----------------------- Di.MON
      call write line2 (jper)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      
      
      
      if ( iplace .ne. 3 ) then
          
      if ( munthly structure .eq. 1 ) then
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(p1mon(jper,1,i),i=1,12),cpart1(jper,1),jt(feeture) ! ---- dissolved metal
 3300 format(' ',a40,',',a40,',',a20,',','DISSOLVED part for ', ! ------- Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      else
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(cpart1(jper,1),i=1,13),jt(feeture) ! -------------------- dissolved metal
      endif
     
      if ( munthly structure .eq. 1 ) then
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(p1mon(jper,2,i),i=1,12),cpart1(jper,2),jt(feeture) ! ---- dissolved metal
 3301 format(' ',a40,',',a40,',',a20,',','Standard deviation for ', ! --- Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      else
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(cpart1(jper,2),i=1,13),jt(feeture) ! --------------------- dissolved metal
      endif
    
      write(110+jper,3305)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XL(i),i=1,13),jt(feeture)
 3305 format(' ',a40,',',a40,',',a20,',','Lower limit on dissolved ', ! - Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
     
      write(110+jper,3306)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XU(i),i=1,13),jt(feeture)
 3306 format(' ',a40,',',a40,',',a20,',','Upper limit on dissolved ', ! - Di.CSV
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
     
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
      
      do 42 imon = 1, 13

      if ( munthly structure .ne. 1 ) then
      P2mon(jper,1,imon) = P2mon(jper,1,13)
      P2mon(jper,2,imon) = P2mon(jper,2,13)
      endif
     
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
      if ( C(jper,1) .gt. 0.000001 ) then
      call set format for printout3(Tenchars(13),cpart2(jper,1)) ! ------- solid
      do i = 1, 12
      call set format for printout3(Tenchars(i),p2mon(jper,1,i)) ! ------- solid
      enddo
      write(100+jper,1000)dname(jper),(Tenchars(i),i=1,13)
 1000 format('Solid ',a11,3x,13a8)
      

      call set format for printout3(Tenchars(13),cpart2(jper,2)) ! ------- solid
      do i = 1, 12
      call set format for printout3(Tenchars(i),p2mon(jper,2,i)) ! ------- solid
      enddo
      !if ( print check .gt. 1.0e-09 ) write(100+jper,1001)
      !&(Tenchars(i),i=1,13)
 1001 format('Standard deviation  ',13a8)
      
      
      !do i = 1, 13
      !call set format for printout3(Tenchars(i),XL(i))
      !enddo
      !if ( print check .gt. 1.0e-09 ) write(100+jper,1005)
      !&(Tenchars(i),i=1,13)
      !1005 format('Lower limit         ',13a8)
      !do i = 1, 13
      !call set format for printout3(Tenchars(i),XU(i))
      !enddo
      !if ( print check .gt. 1.0e-09 ) write(100+jper,1006)
      !&(Tenchars(i),i=1,13)
      !1006 format('Upper limit         ',13a8)
      !call write line2 (jper)
          
      if ( munthly structure .eq. 1 ) then ! ---------------------------- Di.MON
      do i = 1, 12
      xxperc = 100.0*((XU(i)-p2mon(jper,1,i))/p2mon(jper,1,i))
      write(Tenchars(i),3009)nint(xxperc)
 3009 format(i7)
      enddo
      endif ! if ( munthly structure .eq. 1 ) --------------------------- Di.MON
      xxperc = 100.0*((XU(13)-p2mon(jper,1,13))/p2mon(jper,1,13))
      write(Tenchars(13),3009)nint(xxperc)
      write(100+jper,1206)(Tenchars(i),i=1,13) ! ------------------------ Di.MON
 1206 format('Plus or minus ...   ',13(a7,'%')) ! ----------------------- Di.MON
      call write line2 (jper)
      endif ! if ( C(jper,1) .gt. 0.000001 )
      if ( iplace .ne. 3 ) then
          
      if ( munthly structure .eq. 1 ) then
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(P2mon(jper,1,i),i=1,12),cpart2(jper,1),jt(feeture) ! ------------- Di.CSV
 3300 format(' ',a40,',',a40,',',a20,',','SOLID part for ',',',a11, ! --- Di.CSV
     &13(',',1pe12.4),',',i4) ! ----------------------------------------- Di.CSV
      else
      write(110+jper,3300)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(cpart2(jper,1),i=1,13),jt(feeture)
      endif
     
      if ( munthly structure .eq. 1 ) then
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(P2mon(jper,2,i),i=1,12),cpart2(jper,2),jt(feeture)
 3301 format(' ',a40,',',a40,',',a20,',','Standard deviation for ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
      else
      write(110+jper,3301)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(cpart2(jper,2),i=1,13),jt(feeture)
      endif
     
      write(110+jper,3305)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XL(i),i=1,13),jt(feeture)
 3305 format(' ',a40,',',a40,',',a20,',','Lower limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV

      write(110+jper,3306)GIScode(feeture),unamex,rnamex,dname(jper), ! - Di.CSV
     &(XU(i),i=1,13),jt(feeture)
 3306 format(' ',a40,',',a40,',',a20,',','Upper limit on solid ',
     &',',a11,13(',',1pe12.4),',',i4) ! --------------------------------- Di.CSV
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

      if ( munthly structure .eq. 0 ) then
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0))
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      do i = 1, 13
      loadmon(ip,JDET,1,i) = LMcontrib(ip,JDET,1)
      loadmon(ip,JDET,2,i) = LMcontrib(ip,JDET,2)
      lconmon(ip,JDET,1,i) = CMcontrib(ip,JDET,1)
      lconmon(ip,JDET,2,i) = CMcontrib(ip,JDET,2)
      enddo
      endif
      return ! no requirement for monthly data 
      endif


      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13 ! loop on the months --------------------------------
      CM = 0.0 ! initialise mean concentration
      CS = 0.0 ! standard deviation
      CLM = 0.0 ! initialise mean load      
      CLS = 0.0 ! standard deviation      
      N2=0   ! and number of shots in the month

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
      
      if (CLM .gt. 1.0E-10) goto 15
      CLS = 0.0
      goto 16
   15 CLS = (CLS-CLM*CLM/N2)/(N2-1)
      if (CLS .gt. 1.0E-20) goto 19
      CLS = 0.0
      goto 16
   19 CLS = SQRoot(1599,CLS)
   16 CLM = CLM/N2
      
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

      if ( munthly structure .eq. 0 ) then
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0)) ! for river flow
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      do ii = 1, 12
      tmon(1,ii) = CM
      tmon(2,ii) = CS
      enddo
      endif
      endif

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
      CS = 0.0
      goto 6
    5 CS = (CS-CM*CM/N2)/(N2-1)
      if (CS .gt. 1.0E-20) goto 9
      CS = 0.0
      goto 6
    9 CS = SQRoot(311099,CS)
    6 CM = CM/N2

      smon(1,jmon) = amax1 ( 0.0, CM )
      smon(2,jmon) = CS

   88 continue

      CM = 0.0
      CS = 0.0

      do IS = 1, NS
      CM = CM + Y(IS)
      CS = CS + Y(IS)*Y(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 15
      CS = 0.0
      goto 16
   15 CS = (CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 19
      CS = 0.0
      goto 16
   19 CS = SQRoot(301699,CS)
   16 CM = CM/NS

      BC(2,1) = amax1 ( 0.0, CM ) ! mean suspended solids
      BC(2,2) = CS ! standard deviation for suspended solids

      if ( munthly structure .ne. 1 ) then
      do ii = 1, 12
      smon(1,ii) = amax1 ( 0.0, BC(2,1) )
      smon(2,ii) = BC(2,2)         
      enddo
      endif

      return
      end




      subroutine write the monthly data in comma separated form ! 210 and 110 --
     &(jper,ipt)
      include 'COMMON DATA.FOR'
      real lcontot(13)

      unamex = uname(feeture)
      if ( ipt .eq. 3 ) unamex = 'End of Reach  '
      if ( ipt .eq. 1 ) unamex = 'Start of Reach'
      !rnamex = rname (IREACH)
      !if ( ipt .eq. 4 ) rnamex = 'Upstream of works'

      if ( ipt .eq. 1 ) write(unamex,1276)rname(IREACH)
 1276 format('Start of reach: ',a16)
      if ( ipt .eq. 3 ) write(unamex,1277)rname(IREACH)
 1277 format('End of reach: ',a16)


*     ==========================================================================
*     write out the monthly data in "commma separated" form --------------------
*     ==========================================================================

*     net loads from discharges (3 12 5 and 39 etc) --------------- LMcontrib 01
      call munthly split (01,jper,lcontot,CS,CML,CMU)
      write(110+jper,8188)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(01,jper,1),jxtype ! ------------------------------------- Di.CSV
 8188 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all types of discharges (3 12 5 etc)',
     &',',a11,13(',',1pe12.4),',',i4,',3 12 5 39 60 61') ! -------------- Di.CSV
*     net loads from discharges (3 12 5 and 39 etc) --------------- LMcontrib 01
  

*     net load from sewage works (3) ------------------------------ LMcontrib 02
      call munthly split (02,jper,lcontot,CS,CML,CMU)
      write(110+jper,8928)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(02,jper,1),jxtype ! ------------------------------------- Di.CSV
 8928 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all sewage works (3)',
     &',',a11,13(',',1pe12.4),',',i4,',03') ! --------------------------- Di.CSV
*     net load from sewage works (3) ------------------------------ LMcontrib 02
     
     
*     net load from all intermittent discharges (12) -------------- LMcontrib 03
      call munthly split (03,jper,lcontot,CS,CML,CMU)
      write(110+jper,8929)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(03,jper,1),jxtype
 8929 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all intermittent discharges (12)',
     &',',a11,13(',',1pe12.4),',',i4,',12') ! --------------------------- Di.CSV
*     net load from all intermittent discharges (12) -------------- LMcontrib 03


*     industrial discharges (5) ----------------------------------- LMcontrib 04
      call munthly split (04,jper,lcontot,CS,CML,CMU)
      write(110+jper,8951)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(04,jper,1),jxtype ! ------------------------------------- Di.CSV
 8951 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all industrial discharges (5)',',',a11,
     &13(',',1pe12.4),',',i4,',05') ! ----------------------------------- Di.CSV
*     industrial discharges (5) ----------------------------------- LMcontrib 04


*     mine waters (39) -------------------------------------------- LMcontrib 05
      call munthly split (05,jper,lcontot,CS,CML,CMU)
      write(110+jper,8930)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(05,jper,1),jxtype ! ------------------------------------- Di.CSV
 8930 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all mine waters (39)',',',a11,
     &13(',',1pe12.4),',',i4,',39') ! ----------------------------------- Di.CSV
*     mine waters (39) -------------------------------------------- LMcontrib 05

     
*     other discharges (60) --------------------------------------- LMcontrib 28
      call munthly split (28,jper,lcontot,CS,CML,CMU)
      write(110+jper,8961)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(28,jper,1),jxtype ! ------------------------------------- Di.CSV
 8961 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from all "Other" Point Sources (60)',',',a11,
     &13(',',1pe12.4),',',i4,',60') ! ----------------------------------- Di.CSV
*     other discharges (60) --------------------------------------- LMcontrib 28

     
*     private wastewaters (61) ------------------------------------ LMcontrib 29
      call munthly split (29,jper,lcontot,CS,CML,CMU)
      write(110+jper,8960)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(29,jper,1),jxtype ! ------------------------------------- Di.CSV
 8960 format(' ',a40,',',a40,s',',a20,',',
     &'Net inputs to load from all Private Wastewaters (61)',',',a11,
     &13(',',1pe12.4),',',i4,',61') ! ----------------------------------- Di.CSV
*     private waste waters (61) ----------------------------------- LMcontrib 29
     

*     fish farms (62) --------------------------------------------- LMcontrib 30
      call munthly split (30,jper,lcontot,CS,CML,CMU) ! fish farms
      write(110+jper,8962)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(30,jper,1),jxtype ! ------------------------------------- Di.CSV
 8962 format(' ',a40,',',a40,s',',a20,',',
     &'Net inputs to load from all Fish Farms (62)',',',a11,
     &13(',',1pe12.4),',',i4,',61') ! ----------------------------------- Di.CSV
*     fish farms (62) --------------------------------------------- LMcontrib 30
     

*     diffuse inflow (river-type) (13) ---------------------------- LMcontrib 18
      call munthly split (18,jper,lcontot,CS,CML,CMU)
      write(110+jper,8909)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(18,jper,1),jxtype ! ------------------------------------- Di.CSV
 8909 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse inflow (river-type) (13)',
     &',',a11,13(',',1pe12.4),',',i4,',13') ! --------------------------- Di.CSV
*     diffuse inflow (river-type) (13) ---------------------------- LMcontrib 18

     
*     diffuse pollution from livestock (25) ----------------------- LMcontrib 06
      call munthly split (06,jper,lcontot,CS,CML,CMU)
      write(110+jper,8910)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(06,jper,1),jxtype ! ------------------------------------- Di.CSV
 8910 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from livestock (25)',
     &',',a11,13(',',1pe12.4),',',i4,',25') ! --------------------------- Di.CSV
*     diffuse pollution from livestock (25) ----------------------- LMcontrib 06

     
*     diffuse pollution from arable land (27) --------------------- LMcontrib 07
      call munthly split (07,jper,lcontot,CS,CML,CMU)
      write(110+jper,8911)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV 
     &LMcontrib(07,jper,1),jxtype ! ------------------------------------- Di.CSV
 8911 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from arable land (27)',
     &',',a11,13(',',1pe12.4),',',i4,',27') ! --------------------------- Di.CSV
*     diffuse pollution from arable land (27) --------------------- LMcontrib 07

     
*     diffuse pollution from highways (29) ------------------------ LMcontrib 08
      call munthly split (08,jper,lcontot,CS,CML,CMU)
      write(110+jper,8912)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(08,jper,1),jxtype ! ------------------------------------- Di.CSV
 8912 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from highways (29)',
     &',',a11,13(',',1pe12.4),',',i4,',29') ! --------------------------- Di.CSV
*     diffuse pollution from highways (29) ------------------------ LMcontrib 08

     
*     diffuse pollution from urban land (31) ---------------------- LMcontrib 09
      call munthly split (09,jper,lcontot,CS,CML,CMU)
      write(110+jper,8913)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(09,jper,1),jxtype ! ------------------------------------- Di.CSV
 8913 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse pollution from urban land (31)',
     &',',a11,13(',',1pe12.4),',',i4,',31') ! --------------------------- Di.CSV
*     diffuse pollution from urban land (31) ---------------------- LMcontrib 09

     
*     diffuse pollution from atmospheric deposition (33) ---------- LMcontrib 10
      call munthly split (10,jper,lcontot,CS,CML,CMU)
      write(110+jper,8914)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(10,jper,1),jxtype ! ------------------------------------- Di.CSV
 8914 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from atmospheric deposition (33)',',',a11,
     &13(',',1pe12.4),',',i4,',33') ! ----------------------------------- Di.CSV
*     diffuse pollution from atmospheric deposition (33) ---------- LMcontrib 10

     
*     diffuse pollution from natural background (35) -------------- LMcontrib 11
      call munthly split (11,jper,lcontot,CS,CML,CMU)
      write(110+jper,8915)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(11,jper,1),jxtype ! ------------------------------------- Di.CSV
 8915 format(' ',a40,',',a40,',',a20,',','Net inputs to load specified',
     &' as natural background (35)',',',a11,
     &13(',',1pe12.4),',',i4,',35') ! ----------------------------------- Di.CSV
*     diffuse pollution from natural background (35) -------------- LMcontrib 11

     
*     diffuse pollution from septic tanks (37) -------------------- LMcontrib 12
      call munthly split (12,jper,lcontot,CS,CML,CMU)
      write(110+jper,8916)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(12,jper,1),jxtype ! ------------------------------------- Di.CSV
 8916 format(' ',a40,',',a40,',',a20,',','Net inputs to load from ',
     &'diffuse pollution from septic tanks (37)',
     &',',a11,
     &13(',',1pe12.4),',',i4,',37') ! ----------------------------------- Di.CSV
*     diffuse pollution from septic tanks (37) -------------------- LMcontrib 12
     

*     diffuse pollution from aggregated CSOs (40) ----------------- LMcontrib 13
      call munthly split (13,jper,lcontot,CS,CML,CMU)
      write(110+jper,8116)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(13,jper,1),jxtype ! ------------------------------------- Di.CSV
 8116 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from from aggregated CSOs (40)',',',a11,
     &13(',',1pe12.4),',',i4,',40') ! ----------------------------------- Di.CSV
*     diffuse pollution from aggregated CSOs (40) ----------------- LMcontrib 13

     
*     diffuse pollution from aggregated STWs (42) ----------------- LMcontrib 14
      call munthly split (14,jper,lcontot,CS,CML,CMU)
      write(110+jper,8922)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(14,jper,1),jxtype ! ------------------------------------- Di.CSV
 8922 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from aggregated STWs (42)',',',a11,
     &13(',',1pe12.4),',',i4,',42') ! ----------------------------------- Di.CSV
*     diffuse pollution from aggregated STWs (42) ----------------- LMcontrib 14

     
*     diffuse pollution from diffuse mines (46) ------------------- LMcontrib 15
      call munthly split (15,jper,lcontot,CS,CML,CMU)
      write(110+jper,8925)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(15,jper,1),jxtype ! ------------------------------------- Di.CSV
 8925 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse mines (46)',',',a11,
     &13(',',1pe12.4),',',i4,',46') ! ----------------------------------- Di.CSV
*     diffuse pollution from diffuse mines (46) ------------------- LMcontrib 15

     
*     diffuse pollution from birds boats and angling -------------- LMcontrib 16
      call munthly split (16,jper,lcontot,CS,CML,CMU)
      write(110+jper,8225)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(16,jper,1),jxtype ! ------------------------------------- Di.CSV
 8225 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load specified as ',
     &'from birds boats and angling (48)',
     &',',a11,13(',',1pe12.4),',',i4,',48') ! --------------------------- Di.CSV
*     diffuse pollution from birds boats and anglin --------------- LMcontrib 16

     
*     diffuse pollution from source specified by User (50) -------- LMcontrib 23
      call munthly split (23,jper,lcontot,CS,CML,CMU)
      write(110+jper,8505)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(23,jper,1),jxtype ! ------------------------------------- Di.CSV
 8505 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (50)',
     &',',a11,13(',',1pe12.4),',',i4,',50') ! --------------------------- Di.CSV
*     diffuse pollution from source specified by User (50) -------- LMcontrib 23

     
*     diffuse pollution from source specified by User (52) -------- LMcontrib 24
      call munthly split (24,jper,lcontot,CS,CML,CMU)
      write(110+jper,8525)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(24,jper,1),jxtype ! ------------------------------------- Di.CSV
 8525 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (52)',
     &',',a11,13(',',1pe12.4),',',i4,',52') ! --------------------------- Di.CSV
*     diffuse pollution from source specified by User (52) -------- LMcontrib 24

     
*     diffuse pollution from source specified by User (54) -------- LMcontrib 25
      call munthly split (25,jper,lcontot,CS,CML,CMU)
      write(110+jper,8545)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(25,jper,1),jxtype ! ------------------------------------- Di.CSV
 8545 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (54)',
     &',',a11,13(',',1pe12.4),',',i4,',54') ! --------------------------- Di.CSV
*     diffuse pollution from source specified by User (54) -------- LMcontrib 25

     
*     diffuse pollution from source specified by User (56) -------- LMcontrib 26
      call munthly split (26,jper,lcontot,CS,CML,CMU)
      write(110+jper,8565)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(26,jper,1),jxtype ! ------------------------------------- Di.CSV
 8565 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (56)',
     &',',a11,13(',',1pe12.4),',',i4,',56') ! --------------------------- Di.CSV
*     diffuse pollution from source specified by User (56) -------- LMcontrib 26

     
*     diffuse pollution from source specified by User (58) -------- LMcontrib 27
      call munthly split (27,jper,lcontot,CS,CML,CMU)
      write(110+jper,8585)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(27,jper,1),jxtype ! ------------------------------------- Di.CSV
 8585 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from type of source specified by User (58)',
     &',',a11,13(',',1pe12.4),',',i4,',58') ! --------------------------- Di.CSV
*     diffuse pollution from source specified by User (58) -------- LMcontrib 27

     
*     diffuse pollution from inflows (effluent-type) (15) --------- LMcontrib 19
      call munthly split (19,jper,lcontot,CS,CML,CMU)
      write(110+jper,8917)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(19,jper,1),jxtype ! ------------------------------------- Di.CSV
 8917 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse inflows (effluent-type) (15)',
     &',',a11,13(',',1pe12.4),',',i4,',15') ! --------------------------- Di.CSV
*     diffuse pollution from inflows (effluent-type) (15) --------- LMcontrib 19


*     diffuse pollution from boundaries and streams (2 and 10) ---- LMcontrib 17
      call munthly split (17,jper,lcontot,CS,CML,CMU)
      write(110+jper,8180)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(17,jper,1),jxtype ! ------------------------------------- Di.CSV
 8180 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from boundaries and streams (2 and 10)',
     &',',a11,13(',',1pe12.4),',',i4,',2 10 13 15') ! ------------------- Di.CSV
*     diffuse pollution from boundaries and streams (2 and 10) ---- LMcontrib 17


*     diffuse pollution from diffuse inflow (reach-based) --------- LMcontrib 20
      call munthly split (20,jper,lcontot,CS,CML,CMU)
      write(110+jper,8181)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(20,jper,1),jxtype ! ------------------------------------- Di.CSV
 8181 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load from diffuse inflow (reach-based)',',',a11,
     &13(',',1pe12.4),',',i4,',500') ! ---------------------------------- Di.CSV
*     diffuse pollution from diffuse inflow (reach-based) --------- LMcontrib 20


*     added by natural purification ---------------------------------- TNLOADUP2
      write(110+jper,8182)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(TNLOADUP2(jper,J13),J13=2,N13), ! ------------- Di.CSV
     &TNLOADUP2(jper,1),jxtype ! ---------------------------------------- Di.CSV
 8182 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load added by natural purification',',',a11,
     &13(',',1pe12.4),',',i4,',600') ! ---------------------------------- Di.CSV
*     added by natural purification ---------------------------------- TNLOADUP2

      
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg LMcontrib 21
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ===== added by flow gap filling 
      call munthly split (21,jper,lcontot,CS,CML,CMU)
      write(110+jper,8186)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(21,jper,1),jxtype ! ------------------------------------- Di.CSV
 8186 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load added by flow gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',700') ! ---------------------------------- Di.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) ==== added by flow gap filling
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg LMcontrib 21
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg LMcontrib 22
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! == added by quality gap filling 
      call munthly split (22,jper,lcontot,CS,CML,CMU)
      write(110+jper,8184)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(22,jper,1),jxtype ! ------------------------------------- Di.CSV
 8184 format(' ',a40,',',a40,',',a20,',',
     &'Net inputs to load added by quality gap filling',
     &',',a11,13(',',1pe12.4),',',i4,',800') ! -------------------------- Di.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) = added by quality gap filling
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg LMcontrib 22
      
      
*     total of all loads ------------------------------------------ LMcontrib 30
      call munthly split (NTD,jper,lcontot,CS,CML,CMU) ! total loads
      write(110+jper,8919)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(NTD,jper,1),jxtype ! ------------------------------------ Di.CSV
 8919 format(' ',a40,',',a40,',',a20,',',
     &'Net total of all loads',',',a11,
     &13(',',1pe12.4),',',i4,',999') ! ---------------------------------- Di.CSV
*     total of all loads ------------------------------------------ LMcontrib 30

     
*     load lost to natural purification ----------------------------------------
      write(110+jper,8918)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(TBLODE2(jper,J13),J13=2,N13), ! --------------- Di.CSV
     &TBLODE2(jper,1),jxtype ! ------------------------------------------ Di.CSV
 8918 format(' ',a40,',',a40,',',a20,',',
     &'Load lost to abstractions (7)',',',a11, ! ------------------------ Di.CSV
     &13(',',1pe12.4),',',i4,', 7 18 19') ! ----------------------------- Di.CSV
*     load lost to natural purification ----------------------------------------

     
*     load lost to abstractions ------------------------------------------------
      write(110+jper,8183)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(TNLOADDN2(jper,J13),J13=2,N13), ! ------------- Di.CSV
     &TNLOADDN2(jper,1),jxtype ! ---------------------------------------- Di.CSV
 8183 format(' ',a40,',',a40,',',a20,',',
     &'Load lost to natural purification',',',a11,
     &13(',',1pe12.4),',',i4,',601') ! ---------------------------------- Di.CSV
*     load lost to abstractions ------------------------------------------------


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! ====== lost by flow gap filling 
      write(110+jper,8187)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),!(TILOADDN2(jper,J13),J13=2,N13), ! ------------ Di.CSV
     &TILOADDN2(jper,1),jxtype ! ---------------------------------------- Di.CSV
 8187 format(' ',a40,',',a40,',',a20,',',
     &'Load lost by flow gap filling',
     &',',a11,13(',',1pe12.4),',',i4,',701') ! -------------------------- Di.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) ! ====lost by flow gap 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( ical .eq. 4 .or. ical .eq. 6 ) then ! === lost by quality gap filling  
      write(110+jper,8185)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),!(TALOADDN2(jper,J13),J13=2,N13), ! ------------ Di.CSV
     &TALOADDN2(jper,1),jxtype ! ---------------------------------------- Di.CSV
 8185 format(' ',a40,',',a40,',',a20,',',
     &'Load lost by quality gap filling',',',a11,
     &13(',',1pe12.4),',',i4,',801') ! ---------------------------------- Di.CSV
      endif ! if ( ical .eq. 4 .or. ical .eq. 6 ) != lost by quality gap filling
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      
*     total load removed -------------------------------------------------------
      write(110+jper,8195)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(TLOSSES2(jper,J13),J13=2,N13), ! -------------- Di.CSV
     &TLOSSES2(jper,1),jxtype ! ----------------------------------------- Di.CSV
 8195 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD REMOVED',',',a11,
     &13(',',1pe12.4),',',i4,',802') ! ---------------------------------- Di.CSV
*     total load removed -------------------------------------------------------

     

     
*     **************************************************************************     
      call munthly split (02,jper,lcontot,CS,CML,CMU)
      write(110+jper,8948)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(2,jper,1),jxtype,numprop(2) ! --------------------------- Di.CSV
 8948 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL SEWAGE WORKS (3)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (03,jper,lcontot,CS,CML,CMU)
      write(110+jper,8949)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(3,jper,1),jxtype,numprop(3)
 8949 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL INTERMITTENT DISCHARGES (12)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (04,jper,lcontot,CS,CML,CMU)
      write(110+jper,8950)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(4,jper,1),jxtype,numprop(4)
 8950 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL INDUSTRIAL DISCHARGES (5)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (05,jper,lcontot,CS,CML,CMU)
      write(110+jper,8989)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(5,jper,1),jxtype,numprop(5)
 8989 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL MINE WATERS (39)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (28,jper,lcontot,CS,CML,CMU)
      write(110+jper,8969)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(28,jper,1),jxtype,numprop(28)
 8969 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL OTHER DISCHARGES (60)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (29,jper,lcontot,CS,CML,CMU)
      write(110+jper,8979)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(29,jper,1),jxtype,numprop(29)
 8979 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL PRIVATE WASTEWATERS (61)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
*     **************************************************************************
      call munthly split (30,jper,lcontot,CS,CML,CMU) ! fish farms
      write(110+jper,8629)GIScode(feeture),unamex, ! -------------------- Di.CSV
     &rnamex,dname(jper),(lcontot(J13),J13=1,12), ! --------------------- Di.CSV
     &LMcontrib(30,jper,1),jxtype,numprop(29)
 8629 format(' ',a40,',',a40,',',a20,',',
     &'TOTAL LOAD INPUT FROM ALL FISH FARMS (62)',',',a11,
     &13(',',1pe12.4),',',i4,',',i2) ! ---------------------------------- Di.CSV
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

      if ( abs (LMcontrib(NTD,jper,1)) .gt. 0.000001) then
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

*     added by flow gap filling ---------------------------------------TILOADUP2
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

*     added by quality gap filling for ------------------------------- TALOADUP2
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

*     net loads from all discharges (3 12 5 etc) ----------------------- TELODE2
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
  
*     fish farms (62) --------------------------------------------------T62LOAD2
      if ( abs (T62LOAD2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),T62LOAD2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),T62LOAD2(jper,i13))
      write(100+jper,3833)(Tenchars (i),i=1,13)
 3833 format('Fish farms          (62)    ',2x,13a10)
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
      write(100+jper,3719)(Tenchars (i),i=1,13)
 3719 format('Total                         ',13a10)
      endif
      call write line (jper)
*     --------------------------------------------------------------------------


*     lost to abstractions ----------------------------------------------TBLODE2
      if ( abs (TBLODE2(jper,i13)) .gt. 0.000001) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout(Tenchars (i),TBLODE2(jper,I+1))
      enddo
      endif
      call set format for printout(Tenchars (13),TBLODE2(jper,i13))
      write(100+jper,3618)(Tenchars (i),i=1,13)
 3618 format('Abstractions (7)      ',8x,13a10)
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
      

      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (prop10(jper)) .gt. 1.0e-10 ) then
      call sort format 3  (prop10(jper),propRT(jper),propNPU(jper))
      write(120+jper,8930)valchars10,valchars11,valchars12 ! ------------ Di.ADL
 8930 format('+',4x,'Boundaries and tributaries (2 and 10)',
     &9x,10A10)
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
      write(Tenchars(jper),7831)propeff2(jper)
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
      if ( abs (propeff62(jper)) .gt. 1.0e-10 ) kp = 1
      write(Tenchars(jper),7831)propeff62 (jper)
      endif
      if ( kp .eq. 1 ) then
      write(120+jper,6290)Tenchars(jper) ! ------------------------------ Di.ADL
 6290 format('*',4x,'Fish farms (62) ',20X,10A10)
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
 3530 format('*',4x,'Boundaries and tributaries (2 and 10)',
     &8x,10A10)
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
      kp = 0 ! total loads introduced by gap filling for river flows
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 .or. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),LMcontrib(21,jper,1))
      endif
      if ( kp .eq. 1 ) write(120+jper,2943)Tenchars(jper) ! ------------- Di.ADL
 2943 format('*',4x,'Added by gap filling of flow',17x,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     total loads introduced by gap filling for river quality ------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),LMcontrib(22,jper,1))
      endif
      if ( kp .eq. 1 ) write(120+jper,2944)Tenchars(jper) ! ------------- Di.ADL
 2944 format('*',4x,'Added by gap filling of quality',14X,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loads added by natural purification --------------------------------------
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TNLOADUP2(jper,i13)) .gt. 1.0e-10 ) kp = 1 ! ------------ Di.ADL
      call set format for printout(Tenchars(jper),TNLOADUP2(jper,i13))
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
      kp = 0
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (T62LOAD1(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),T62LOAD1(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,7903)Tenchars(jper) ! ------------- Di.ADL
 7903 format('*',4x,'Total from fish farms (62)',10x,10A10)
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
      kp = 0 ! loads removed by abstractions -----------------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TBLODE2(jper,i13)) .gt. 1.0e-10 ) kp = 1 ! -------------- Di.ADL
      call set format for printout(Tenchars(jper),TBLODE2(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8246)Tenchars(jper) ! ------------- Di.ADL
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      kp = 0 ! loads removed by natural purification ---------------------------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( abs (TNLOADDN2(jper,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars(jper),TNLOADDN2(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,1931)Tenchars(jper) ! ------------- Di.ADL
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! total loads introduced by gap filling for river flows -----------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 .and. ical .eq. 2 ) kp = 1
      call set format for printout(Tenchars(jper),TILOADDN2(jper,i13))
      endif
      if ( kp .eq. 1 ) write(120+jper,8913)Tenchars(jper) ! ------------- Di.ADL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! total loads introduced by gap filling for river quality ---------
      Tenchars(jper) = '      ....'
      if ( QTYPE (jper) .ne. 4 ) then
      if ( ical .gt. 3 ) kp = 1
      call set format for printout(Tenchars(jper),TALOADDN2(jper,i13))
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
      character *08 Tenchars (13)
     
      if ( nobigout .gt. 0 ) return

      if ( ical13 .eq. 0 ) then ! =============================================
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      if ( C(ic,1) .gt. 0.00001 ) then
      write(100+ic,1910)UNAME(feeture),feeture, ! ------------------------- MON
     &IREACH,DIST(feeture),RNAME(IREACH)
 1910 format(///110('=')/A40,4X,'Feature No',I6,
     &3X,'Reach No',I6/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! --------------- MON
     &'of the Reach called ',A16/110('-'))
      endif
      endif ! if ( QTYPE (ic) .ne. 4 )
      endif
      enddo ! do ic = 1, ndet
      endif ! if ( ical .eq. 0 ) ===============================================

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

      write(100+jper,2000)
 2000 format(124('-')/20x,5x,'Jan',5x,'Feb',5x,'Mar',5x,'Apr',5x,'May',
     &5x,'Jun',5x,'Jul',5x,'Aug',5x,'Sep',5x,'Oct',5x,'Nov',5x,'Dec',
     &4x,'Year'/20('-'),13(2x,6('-')))
      
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout3(Tenchars(i),fmon(1,i))
      enddo
      endif
      call set format for printout3(Tenchars(13),FLOW(1))

      write(100+jper,1100)(Tenchars (i),i=1,13)
 1100 format('Mean river flow     ',13a8)

      if ( munthly structure .eq. 1 ) then ! set up print out -------------------
      do i = 1, 12
      call set format for printout3(Tenchars(i),fmon(2,i))
      enddo
      endif ! if ( munthly structure .eq. 1 ) -----------------------------------
      call set format for printout3(Tenchars(13),FLOW(2))
      write(100+jper,1101)(Tenchars (i),i=1,13)
 1101 format('95-percentile       ',13a8)
      call write line2 (jper)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      call set format for printout3(Tenchars(i),tmon(1,i)) ! temperature
      enddo
      endif
      !call set format for printout3(Tenchars (13),BC(1,1)) ! temperature
      !write(100+jper,1800)(Tenchars(i),i=1,13) ! temperature
 1800 format('Mean temperature    ',13a8) ! temperature
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      !call set format for printout3(Tenchars(i),tmon(2,i)) ! temperature
      enddo
      endif
      !call set format for printout3(Tenchars(13),BC(1,2)) ! temperature
      !write(100+jper,1801)(Tenchars(i),i=1,13) ! temperature
 1801 format('Standard deviation  ',13a8)
      !call write line2 (jper)
      xprint = 0.0
      do i = 1, 12
      xprint = xprint + smon(1,i)
      enddo
      if ( xprint .gt. 0.00001 ) then
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1, 12
      !call set format for printout3(Tenchars(i),smon(1,i)) ! suspended solids
      enddo
      endif
      !call set format for printout3(Tenchars(13),BC(2,1)) ! suspended solids
      !write(100+jper,1900)(Tenchars (i),i=1,13) ! suspended solids
 1900 format('Suspended solids    ',13a8)

      if ( munthly structure .eq. 1 ) then ! set up print out ------------------
      do i = 1, 12
      !call set format for printout3(Tenchars(i),smon(2,i)) ! suspended solids
      enddo
      endif ! if ( munthly structure .eq. 1 ) ----------------------------------
      !call set format for printout3(Tenchars(13),BC(2,2)) ! suspended solids
      !write(100+jper,1901)(Tenchars (i),i=1,13) ! suspended solids
 1901 format('Standard deviation  ',13a8)
      !call write line2 (jper)
      endif ! if ( xprint .gt. 0.00001 )

      if ( iplace .eq. 1 ) write(unamex,1276)rname(IREACH)
 1276 format('Start of reach: ',a16)
      if ( iplace .eq. 3 ) write(unamex,1277)rname(IREACH)
 1277 format('End of reach: ',a16)

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
      character *10 runchar

*     iplace 1 ... start of reach
*     iplace 3 ... end of reach
*     iplace 2 ... at feature
*     iplace 4 ... upstream of discharge
*     iplace 5 ... downstream of tributary
*     iplace 6 ... downstream of discharge

      if ( QTYPE (jper) .eq. 4 ) return
      if ( loadmon(NTD,jper,1,i13) .lt. 1.0e-8 ) return

*     % contribution of monthly effluent load to the annual mean ---------------
      running mean = 0.0

      zero check = 0.0
      do 3000 ip = 1, nprop
      do im = 1, 13
      zero check = amax1 ( zero check, loadmon(ip,jper,1,im) )
      enddo
 3000 continue 
      
      
*     **************************************************************************      
      
      do 2000 iiip = 1, nprop ! 555555555555555555555555555555555555555555555555
      ip = propsequence(iiip)

*     --------------------------------------------------------------------------
*     output for features ------------------------------------------------------
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( ip .eq. 1) then ! write the headings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~=
          
      if ( iplace .eq. 4 ) then
*     write(120+jper,8690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 8690 format(///100('=')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... Upstream of ',a37/100('-')) ! --------------------------- Di.ADL
*     write(120+jper,1691) ! -------------------------------------------- Di.ADL
*     write(100+jper,8290)uname(feeture),dname(jper) ! ++++++++++++++++++ Di.MON
 8290 format(///136('=')/'Apportionment of mean load', ! ++++++++++++++++ Di.MON
     &' ... Upstream of ',a37,4x,'for: ',a11/136('-')) ! ++++++++++++++++ Di.MON
      endif
      if ( iplace .eq. 6 ) then
*     write(120+jper,2690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 2690 format(///100('-')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... Downstream of ',a37/100('-')) ! ------------------------- Di.ADL
*     write(120+jper,1691) ! -------------------------------------------- Di.ADL
*     write(100+jper,2890)uname(feeture),dname(jper) ! ++++++++++++++++++ Di.MON
 2890 format(///140('=')/'Apportionment of mean load', ! ++++++++++++++++ Di.MON
     &' ... Downstream of ',a37,2x,'for: ',a11/140('-')) ! ++++++++++++++ Di.MON
      endif ! if ( iplace .eq. 6 ) --------------------------------------------- 
      
      write(100+jper,3698)uname(feeture),dname(jper) ! ++++++++++++++++++ Di.MON
 3698 format(///136('=')/'Apportionment of mean load', ! ++++++++++++++++ Di.MON
     &' ... at ',a37,2x,'for: ',a11/136('-')) ! +++++++++++++++++++++++++ Di.MON
      write(100+jper,2600) ! ++++++++++++++++++++++++++++++++++++++++++++ Di.MON
 2600 format(32x,5x,'Jan',5x,'Feb',5x,'Mar',5x,'Apr',5x,'May',
     &5x,'Jun',5x,'Jul',5x,'Aug',5x,'Sep',5x,'Oct',5x,'Nov',5x,'Dec',
     &4x,'Year'/32('-'),13(2x,6('-')))
      
      if ( iplace .ne. 4 .and. iplace .ne. 6 ) then
*     write(120+jper,3690)dname(jper),uname(feeture) ! ------------------ Di.ADL
 3690 format(///100('-')/'Apportionment of load for ... ', ! ------------ Di.ADL
     &a11,' ... at ',a37/100('-')) ! ------------------------------------ Di.ADL
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
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif ! if ( ip .eq. 1 etc -----------------------------------------------
*     all discharges ===========================================================
  
    
*     mean from sewage works (3) ========================================== (03)
      if ( ip .eq. 2 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif ! if ( ip .eq. 2 ---------------------------------------------------
*     mean from sewage works (3) ========================================== (03)
  
    
*     mean from intermittents (12) ======================================== (12)
      if ( ip .eq. 3 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     mean from intermittents (12) ======================================== (12)
 
  
*     mean from industry (5) ============================================== (05)
      if ( ip .eq. 4 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     mean from industry (5) ============================================== (05)
   
   
*     mean from mine waters (39) ========================================== (39)
      if ( ip .eq. 5 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     mean from mine waters (39) ========================================== (39)

      
*     mean from Other Point Sources (60) ================================== (60)
      if ( ip .eq. 28 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     mean from Other Point Sources (60) ================================== (60)

      
*     mean from private wastewaters (61) ================================== (61)
      if ( ip .eq. 29 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     mean from private wastewaters (61) ================================== (61)
   
   
*     from livestock farming (25) ========================================= (25)
      if ( ip .eq. 6 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from livestock farming (25) ========================================= (25)
     
 
*     from arable farming (27) ============================================ (27)
      if ( ip .eq. 7 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from arable farming (27) ============================================ (27)

      
*     from highway runoff (29) ============================================ (29)
      if ( ip .eq. 8 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from highway runoff (29) ============================================ (29)
  
    
*     from urban runoff (31) ============================================== (31)
      if ( ip .eq. 9 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from urban runoff (31) ============================================== (31)

      
*     from atmospheric deposition (33) ==================================== (33)
      if ( ip .eq. 10 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from atmospheric deposition (33) ==================================== (33)
      
      
*     from natural background (35) ======================================== (35)
      if ( ip .eq. 11 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from natural background (35) ======================================== (35)

      
*     from septic tanks (37) ============================================== (37)
      if ( ip .eq. 12 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from septic tanks (37) ============================================== (37)

      
*     from aggregated CSOs (40) =========================================== (40)
      if ( ip .eq. 13 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from aggregated CSOs (40) =========================================== (40)

      
*     from aggregated STWs (42) =========================================== (42)
      if ( ip .eq. 14 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from aggregated STWs (42) =========================================== (42)

      
*     from diffuse mines (46) ============================================= (46)
      if ( ip .eq. 15 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from diffuse mines (46) ============================================= (46)

      
*     from birds, boats and angling (48) ================================== (48)
      if ( ip .eq. 16 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from birds, boats and angling (48) ================================== (48)

      
*     user defined (50) =================================================== (50)
      if ( ip .eq. 23 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     user defined (50) =================================================== (50)
 
      
*     user defined (52) =================================================== (52)
      if ( ip .eq. 24 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     user defined (52) =================================================== (52)


*     user defined (54) =================================================== (54)
      if ( ip .eq. 25 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     user defined (54) =================================================== (54)

      
*     user defined (56) =================================================== (56)
      if ( ip .eq. 26 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     user defined (56) =================================================== (56)

      
*     user defined (58) =================================================== (58)
      if ( ip .eq. 27 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     user defined (58) =================================================== (58)

          
*     from headwaters and streams (48) ============================== (10 and 2)
      if ( ip .eq. 17 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif ! if ( ip .eq. 17 ) ===================================== (10 and 2)
*     from headwaters and streams (48) ============================== (10 and 2)
 
      
*     from diffuse inputs (13) ============================================ (13)
      if ( ip .eq. 18 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif ! from diffuse inputs (13)===================================== (13)
*     from diffuse inputs (13) ============================================ (13)
      
     
*     from diffuse inputs (15) ============================================ (15)
      if ( ip .eq. 19 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from diffuse inputs (15) ============================================ (15)
 
     
*     from reach diffuse (20) ============================================= (20)
      if ( ip .eq. 20 .and. loadmon(ip,jper,1,13) .gt. 1.0e-8 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif ! from reach diffuse (20) ===================================== (20)


     
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     from gap filling for flows (21) ===================================== (21)
      if ( ip .eq. 21 .and. loadmon(ip,jper,1,13) .gt. 1.0e-08 ) then
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif 
*     from gap filling for flows (21) ===================================== (21)
*     --------------------------------------------------------------------------          
*     from gap filling for river quality (22) ============================= (22)
      if ( ip .eq. 22 .and. loadmon(ip,jper,1,13) .gt. 1.0e-08) then ! 555555555
      running mean = running mean + loadmon(ip,jper,1,13)
      call write report 100 (jper,ip)
*     call write report 120 (iplace,jper,ip)
      endif
*     from gap filling for river quality (22) ============================= (22)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg  
      
      

      endif ! if ( zero check .gt. 1.0e-09 ) zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      endif ! if ( nobigout .le. 0 ) ++++++++++++++++++++++++++++++++++++++++++++

 2000 continue ! do 2000 iiip = 1, nprop ! 5555555555555555555555555555555555555
 
      write(100+jper,2098) ! -------------------------------------------- Di.MON
 2098 format(136('='))
      
      call set format for printout(runchar,LMcontrib(NTD,jper,1))
*     write(120+jper,6692)runchar ! ------------------------------------- Di.ADL
 6692 format(100('-')/'Total mean load',18x,a10/100('='))
      
      return
      end 


    
      subroutine write line (jper)     
      include 'COMMON DATA.FOR'
      write(100+jper,1)
    1 format(30('-'),13(2x,8('-')))
      return
      end

      subroutine write line2 (jper)     
      include 'COMMON DATA.FOR'
      write(100+jper,1)
    1 format(20('-'),13(2x,6('-')))
      return
      end
      
      subroutine write line4 (jper)     
      include 'COMMON DATA.FOR'
      character *10 runchar     
      call set format for printout(runchar,running mean)
      write(100+jper,1)runchar
    1 format(32('-'),11(2x,6('-')),6x,a10)
      return
      end
      
      
      subroutine write line3 (jper)     
      include 'COMMON DATA.FOR'
      write(100+jper,1)
    1 format(32('-'),13(2x,6('-')))
      return
      end

      
      
      subroutine write report 100 (jper,ip) ! +++++++++++++++++++++++++++ Di.MON
      include 'COMMON DATA.FOR'
      character *10 llmonchar(nprop,2,13),llmonchar2(nprop,1,13),runchar

      do im = 1, 13
      call set format for printout3 ! -------------------- loadmon(ip,jper,1,im)
     &(llmonchar(ip,1,im),loadmon(ip,jper,1,im)) ! ------------------ mean loads
      call set format for printout3 ! -------------------- loadmon(ip,jper,2,im)
     &(llmonchar(ip,2,im),loadmon(ip,jper,2,im)) ! --------- standard deviations
      call set format for printout3 ! -------------------- lconmon(ip,jper,1,im)
     &(llmonchar2(ip,1,im),lconmon(ip,jper,1,im)) ! --------- mean concenrations
      enddo

      call set format for printout3(runchar,running mean)      

      write(100+jper,1)nameprop(ip),(llmonchar(ip,1,i),i=1,13)
    1 format('Mean: ',a26,13a8) ! mean load =============================== (50)
      write(100+jper,2)(llmonchar(ip,2,i),i=1,13) ! +++++++++++++++++++++ Di.MON
    2 format('Standard deviation              ',13a8)
      write(100+jper,3)
     &(100.0*lconmon(ip,jper,1,i)/cmon(jper,1,i),i=1,12), ! +++++++++++++ Di.MON
     &100.0*lconmon(ip,jper,1,13)/cmon(jper,1,13)
    3 format('% of mean river concentration   ',13(f7.1,'%'))
      write(100+jper,4)
     &(100.0*loadmon(ip,jper,1,i)/lmon(jper,1,i),i=1,12), ! +++++++++++++ Di.MON
     &100.0*loadmon(ip,jper,1,13)/lmon(jper,1,13)
    4 format('% of river mean daily load ...  ',13(f7.1,'%'))
      write(100+jper,5)
    5 format(32('-'),13(2x,6('-')))
      
      return
      end


      subroutine write report 120 (iplace,jper,ip) ! -------------------- Di.ADL
      include 'COMMON DATA.FOR'
      character *10 runchar1,runchar2
            
      call set format for printout3 (runchar1,LMcontrib(ip,jper,1))      
      call set format for printout3 (runchar2,running mean)      

      if ( ip .eq. 1 ) then
      write(120+jper,3498)nameprop(ip),runchar1 ! ----------------------- Di.ADL
 3498 format(a30,5x,a10/100('-'))
      else
      write(120+jper,3499)nameprop(ip),runchar1,runchar2 ! -------------- Di.ADL
 3499 format(a30,5x,a10,12x,a10)
      endif
      
      return
      end

      
      
      
      subroutine write out comma separated monthly apportionment (jper)
      include 'COMMON DATA.FOR'
      dimension xadd(13)
      character *10 Tenchars(13)
      
      do i = 1, 13
      xadd(i) = 0.0
      enddo

      do 4500 iiip = 1, 24 ! 18 and 19 etc are excluded 555555555555555555555555
      ip = jorder(iiip) 
      if ( ip .gt. 1 ) then ! after the first discharge ------------------------
      do i = 1, 13
      xadd(i) = xadd(i) + loadmon(ip,jper,1,i)
      enddo
      endif

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
      if ( ip .eq. 17 ) then ! ----------- monthly mean ----------------- Di.CSV
      write(110+jper,5094)GIScode(feeture),unamex,rnamex, ! ------------- Di.CSV
     &dname(jper),(loadmon(ip,jper,1,i),i=1,13),jxtype
 5094 format(' ',a40,',',a40,',',a20,',','Mean loads ', ! --------------- Di.CSV
     &'from headwaters etc (10)',
     &',',a11,13(',',1pe12.4),',',i4,',10') ! --------------------------- Di.CSV
      endif ! ----------------------------------------------------------- Di.CSV
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
      !call set format for printout(Tenchars(13),xadd(13))
      !write(100+jper,3096)(Tenchars(i),i=1,13)
 !3096 format('ALL MEAN CONTRIBUTIONS OF LOAD',13a10/160('-'))
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      call set format for printout(Tenchars(i),cmon(jper,1,i))
      enddo
      endif
      !call set format for printout(Tenchars(13),C(jper,1))
      !write(100+jper,1040)(Tenchars(i),i=1,13)
 !1040 format('TOTAL IN-RIVER MEAN CONC.',5x,13a10)

      xxxx = abs (C(jper,1)-xadd(13)) / C(jper,1)
      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      if ( xxxx .gt. 1.0e-6 ) then
      !call set format for printout(Tenchars(i),cmon(jper,1,i)-xadd(i))
      else
      !call set format for printout(Tenchars(i),0.0)
      endif
      enddo
      endif
      if ( C(jper,1) .gt. 1.0e-8 ) then
      if ( xxxx .gt. 1.0e-6 ) then
      !call set format for printout(Tenchars(13),C(jper,1)-xadd(13))
      else
      !call set format for printout(Tenchars(13),0.0)
      endif
      !write(100+jper,1140)(Tenchars(i),i=1,13)
 1140 format('MEAN LEFT UNACCOUNTED ..... ',2x,13a10)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      
      if ( detype(jper) .ne. 104 ) then ! exclude dissolved oxygen
      if ( C(jper,1) .gt. 1.0e-6 .and. flow(1) .gt. 1.0e-7 ) then
      xcheck = abs (100.0*(C(jper,1)-xadd(13))/C(jper,1))
      if ( xcheck .gt. 0.01 ) then
      call change colour of text (10) ! green
      !write( *,1005) xcheck,dname(jper)
      call set screen text colour
 1005 format(
     &'* Breakdown differs from measured by',f8.2,' % for ',a11)
      !write(09,1005) xcheck,dname(jper)
      endif
      endif
      endif ! if ( detype(per) .ne. 104 )

      if ( munthly structure .eq. 1 ) then ! set up print out
      do i = 1,12
      if ( xxxx .gt. 1.0e-6 ) then
      !call set format for printout(Tenchars(i),100.0*(
      !&(cmon(jper,1,i)-xadd(i))/cmon(jper,1,i) ))
      else
      call set format for printout(Tenchars(i),0.0)
      endif
      enddo
      endif
      if ( C(jper,1) .gt. 1.0e-8 ) then
      if ( xxxx .gt. 1.0e-6 ) then
      !call set format for printout(Tenchars(13),100.0*(
      !&(C(jper,1)-xadd(13))/C(jper,1) ))
      else
      !call set format for printout(Tenchars(13),0.0)
      endif
      !write(100+jper,1240)(Tenchars(i),i=1,13)
 1240 format('% OTHER SOURCES ...',11x,13a10)
      endif ! if ( C(jper,1) .gt. 1.0e-8 )
      endif ! if ( nobigout .le. 0 )
      
*     -------------- finished writing out commma-separated monthly apportionment

      return
      end
      

      
      
      subroutine munthly split (ip,jdet,CM,CS,XL,XU)
      include 'COMMON DATA.FOR'
      dimension CM(12),CS(12),XK(12),XL(13),XU(13)
      
      do im = 1, 12 ! loop on months
      CM(im) = 0.0  ! monthly mean
      XL(im) = 0.0  ! lower confidence limit on monthly mean
      XU(im) = 0.0  ! upper confidence limit on monthly mean
      CS(im) = 0.0  ! monthly standard deviation
      XK(im) = 0.0  ! number of samples for each month
      enddo
      
      if ( munthly structure .eq. 0 ) then
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0))
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      do i = 1, 13
      CM(i) = LMcontrib(ip,JDET,1)
      CS(i) = LMcontrib(ip,JDET,2)
      enddo
      endif
      return ! no requirement for monthly data 
      endif
 
      do IS = 1, NS ! set up the shots for this type of load -------------------
      YY(IS) = CTMS(ip,jdet,IS) * FMS(IS)
      enddo

*     compute the monthly means and standard deviations ------------------------
      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
      CM (imon) = CM (imon) + YY(IS)
      CS (imon) = CS (imon) + YY(IS) * YY(IS)
      XK (imon) = XK (imon) + 1.0
      enddo
      do imon = 1, 12
      CS(imon)=(CS(imon)-CM(imon)*CM(imon)/XK(imon))/(XK(imon)-1.0)
      if ( CS(imon) .gt. 1.0e-10 ) then
      CS(imon) = SQRoot(1924,CS (imon))
      else
      CS(imon) = 0.0
      endif
      CM(imon) = CM(imon) / XK(imon)
      if ( CS(imon) / CM(imon) .lt. 0.001) then
      CS(imon) = 0.0
      endif
      
      A = CM(imon)
      S = CS(imon)
      qqq = amax1 ( 0.05, QUALNB(jdet,ip) / 12.0 )
      SEM = S/SQRoot3(107280,qqq)
      t95 = errx(0.95)
      XL(imon) = amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(imon) = amax1 (A,(A+t95*SEM))
      
      enddo
*     --------------------------------------------------------------------------
      
      A = LMcontrib(ip,jdet,1)
      S = LMcontrib(ip,jdet,2)
      qqq = amax1 ( 0.05, qualn(jdet) )
      SEM = S/SQRoot3(107280,qqq)
      XL(13) = amax1 (0.0, amin1 (A,(A-t95*SEM)))
      XU(13) = amax1 (A,(A+t95*SEM))

      return
      end