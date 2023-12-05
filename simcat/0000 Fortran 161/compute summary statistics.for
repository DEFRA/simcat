*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With screen displays in VISUAL BASIC ...
*     --------------------------------------------------------------------------
*     File Name: compute summary statisics.for ... 5074 lines ------------------
*     ==========================================================================
*     Version 161  (15/11/23) --------------------------------------------------
*     ==========================================================================
*     This file does calulations of summary statistics--------------------------
*     --------------------------------------------------------------------------
*     This file contains 30 sub-routines ---------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... write out loads and quality (IBR)
*     ...... write out dissolved metal (IBR, jper) ! dissolved metal
*     ...... write out solid metal (IBR, jper) ! solid metal
*     ...... compute statistics for plots of river quality 
*     ...... load statistics
*     ...... compute the mean and percentiles at a monitoring point
*     ...... compute the mean and percentiles at a plotting point
*     ...... calculate percentiles from mean and standard deviation
*     ...... calculate summaries of river flow
*     ...... calculate summaries of removed flows
*     ...... calculate monthly summaries of river flow
*     ...... statistics for monthly river flows (kwrite, CM1, CM2 )
*     ...... get statistics for the flows of the discharge or stream
*     ...... get summaries of river quality from the shots ! ---------------
*     ...... identify the shots for percentiles (JDET) ! 3333333333333333333
*     ...... calculate summaries of river quality (K50,JDET,Y)
*     ...... calculate summaries of contribution (ip,JDET,Y,ixx)
*     ...... calculate monthly summaries of quality (JDET,Y)
*     ...... change to log domain (DCP, PER, D1, S1, DCM, DCS)
*     ...... calculate compliance with mean standard (
*     ...... get summaries of loads
*     ...... calculate monthly summaries of loads (JDET,Y)
*     ...... ASSEM (X,Y,LINE)
*     ...... ASSEM1 (X,Y,LINE)
*     ...... get sampling rates for river quality 
*     ...... load calculation
*     ...... load calculation per determinand
*     ...... calculate compliance with percentile standard
*     ...... compute confidence limits 
*     ...... DIFFAV (a1,d1,n1,a2,d2,n2,al1,au1,al2,au2,etc
*     --------------------------------------------------------------------------
*     ...... check the correlation by regression
*     ...... regres the calculated shots (K1,K2,RRR)
*     --------------------------------------------------------------------------
      
      subroutine write out loads and quality (IBR)
      include 'COMMON DATA.FOR'
      logical exists

*     storage for writing out confidence limits for mean and the percentiles ---
      character *13 SET1,SET2,SET3,SET4,SET5
      
*     compute summary statistics from shots ------------------------------------
      call calculate summaries of river flow
      call calculate monthly summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads

*     loop on determinands -----------------------------------------------------
      do 12 jper = 1, ndet

*     check for excluded determinands ------------------------------------------
      if ( QTYPE(jper) .eq. 4 ) goto 12

*     initialise percentiles and their confidence limits -----------------------
      dcp95 = 0.0
      dcpl95 = 0.0
      dcpu95 = 0.0
      dcp90 = 0.0
      dcpl90 = 0.0
      dcpu90 = 0.0
      dcp99 = 0.0
      dcpl99 = 0.0
      dcpu99 = 0.0

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, C(jper,1) )
      S = C(jper,2)
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

*     check for zero mean ------------------------------------------------------
      if (C(jper,1) .lt. 1.0E-08) then
      C(jper,2) = 0.0
      S = C(jper,2)
      goto 10
      endif

*     check for zero standard deviation ----------------------------------------
      if (C(jper,2) .lt. 1.0E-08 .or. QTYPE(jper) .eq. 0) then
*     set the standard deviation -----------------------------------------------
      S = C(jper,2)
*     coefficient of variation -------------------------------------------------
      CoV = S / C(jper,1)
      goto 10
      endif


*     switch between Log-Normal and Normal according to determinand type -------
      goto (30,30,31,30,30,30), QTYPE(jper)

*     the data follow the Log-Normal Distribution ------------------------------
   30 continue
      GM = alog ((A*A)/SQRoot(1069,A*A+S*S))
      GS = SQRoot(107014,alog(1.+(S*S)/(A*A)))

*     percentiles assuming a Log-Normal Distribution
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )
      FF = 1.0

*     switch to 5-percentile etc ... for Dissolved Oxygen ----------------------
      if (QTYPE (jper) .eq. 03 .or. QTYPE (jper) .eq. 05 ) FF=-FF

   23 dcp95 = exp  ( GM + FF * t95 * GS )
      dcp90 = exp  ( GM + FF * t90 * GS )
      dcp99 = exp  ( GM + FF * t99 * GS )

*     calculate confidence limits ... calculate standard normal deviates --------
   22 continue

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence Limits ---------------------------------------------
      dcpl90 = amax1  (0.0, exp (GM+FF*to90*GS)-dcp90+C(jper,4))
      dcpl95 = amax1  (0.0, exp (GM+FF*to95*GS)-dcp95+C(jper,3))
      dcpl99 = amax1  (0.0, exp (GM+FF*to99*GS)-dcp99+C(jper,5))

*     pessimistic confidence Limits --------------------------------------------
      dcpu90 = amax1  (0.0, exp (GM+FF*tp90*GS)-dcp90+C(jper,4))
      dcpu95 = amax1  (0.0, exp (GM+FF*tp95*GS)-dcp95+C(jper,3))
      dcpu99 = amax1  (0.0, exp (GM+FF*tp99*GS)-dcp99+C(jper,5))

   28 continue
      SEM = S/SQRoot(107284,QUALN(jper))
      XL = amax1 (0.0,(A-FF*t95*SEM))
      XU = amax1 (0.0,(A+FF*t95*SEM))

      if ( QTYPE (jper) .ne. 3 .and. QTYPE (jper) .ne. 5 ) then
      call assem(XL,XU,SET1)
      else
      call assem(XU,XL,SET1)     
      endif
      
      SES = S / sqrt ( 2.0 * (QUALN(jper) - 1.0) )
      XL = amax1 (0.0, (S - t95 * SES) )
      XU = amax1 (0.0, (S + t95 * SES) )
      call assem(XL,XU,SET5)

      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)

      goto 10

*     normal Distribution ------------------------------------------------------
   31 GM=A
      GS=S
*     percentiles assuming a Normal Distribution -------------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )
      FF=1.0

*     switch to 5 and 10-percentile --------------------------------------------
      if ( QTYPE (jper) .eq. 03 .or. QTYPE (jper) .eq. 05 ) FF=-FF

   43 dcp95 = GM + FF * t95 * GS
      dcp90 = GM + FF * t90 * GS
      dcp99 = GM + FF * t99 * GS

*     calculate confidence limits. Calculate standard normal deviates ----------
   42 continue
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence limits ---------------------------------------------
      dcpl90 = amax1 (0.0,(GM+FF*to90*GS-dcp90+C(jper,4)))
      dcpl95 = amax1 (0.0,(GM+FF*to95*GS-dcp95+C(jper,3)))
      dcpl99 = amax1 (0.0,(GM+FF*to99*GS-dcp99+C(jper,5)))

*     pessimistic confidence limits --------------------------------------------
      dcpu90 = amax1 (0.0,(GM+FF*tp90*GS-dcp90+C(jper,4)))
      dcpu95 = amax1 (0.0,(GM+FF*tp95*GS-dcp95+C(jper,3)))
      dcpu99 = amax1 (0.0,(GM+FF*tp99*GS-dcp99+C(jper,5)))
      goto 28
   10 continue
      

      if ( nobigout .le. 0 ) then ! ############################################
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! +++++++++++++++++++++++++++++
      goto (3000,3000,3004,3000,3004,3000),qtype(jper) ! -----------------------
      
 3000 continue ! for most types of determinand ---------------------------------
*     --------------------------------------------------------------------------
      call sort format 2 (A,S) ! mean and standard deviation ===================
      if ( JQCAL(feeture) .gt. 0 ) then ! ======================================
      if ( JT(feeture) .ne. 1 ) then ! ---------------------------------- Ci.GAP
      write(170+jper,6638) ! -------------------------------------------- Ci.GAP
 6638 format(/'Quality gap-filling has been requested for a feature ', !  Ci.GAP
     &'that is not a monitoring point ...'/) ! -------------------------- Ci.GAP
      endif ! if ( JT(feeture) .ne. 1 ) --------------------------------- Ci.GAP
      if ( IBR .eq. 3 ) then ! at start or end of reach ------------------------
      write(170+jper,2094)DNAME(jper),valchars10,UNITS(jper),SET1 ! ----- Ci.GAP
 2094 format(A11,44X,' Mean =',a10,1X,A4,4x,a13) ! ---------------------- Ci.GAP
      write(170+jper,1284) ! -------------------------------------------- Ci.GAP
      else ! at a feature ------------------------------------------------------
      write(170+jper,2094)DNAME(jper),valchars10,UNITS(jper),SET1 ! ----- Ci.GAP
      endif ! if ( IBR .eq. 3 ) -------------------------------------------------
      endif ! if ( JQCAL(feeture) .gt. 0 =======================================
 
      
      if ( Detype (jper) .lt. 900) then ! tttttttttttttttttttttttttttttttttttttt
      write(01,1094)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- OUT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ----------- EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or. ! ----------- EFF
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then ! exclude start and finish of a reach ----------EFF 
      write(31,1094)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- EFF
      write(150+jper,1094)DNAME(jper),valchars10,UNITS(jper),SET1 ! ----- Di EFF
 1094 format(a11,44X,' Mean =',a10,1x,a4,4x,a13) ! -----------------------------
      
 
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr        
      if ( kreplace(jper) .eq. 1 ) then ! upstream quality has been replaced rrr
      if ( ifeffcsv .eq. 1 .and. JT (feeture) .ne. 12 ) then ! =================
          
      if ( ICAL .eq. 9 .and. IBR .eq. 6 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~ Mode 9
      
      if ( detype (jper) .lt. 900 ) then ! =====================================
      ipl = kptarg
      if ( MRQS(jper) .eq. 3 ) ipl = 90
      if ( MRQS(jper) .eq. 6 ) ipl = 99
      write(130+jper,7514)GIScode(feeture),uname(feeture), ! ------------ Ei.CSV
     &dname(jper),C(jper,1),C(jper,2),C(jper,icp), ! -------------------- Ei.CSV
     &ipl,JT(feeture),uname(feeture)
 7514 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',',
     &'RESULTING d/s RIVER QUALITY', ! ---------------------------------- Ei.CSV
     &',',a11,3(',',1pe12.4),',',i2,'-percentile',(',',i4),',',a35) ! --- Ei.CSV
      endif ! if ( detype (jper) .lt. 900 ) then ! =============================
 
      goto 8088
      if ( detype (jper) .eq. 909 ) then ! ===================== dissolved metal
      write(130+jper,7584)GIScode(feeture),uname(feeture), ! ------------ Ei.CSV
     &dname(jper),cpart1(jper,1),cpart1(jper,2),cpart1(jper,icp),
     &kptarg,JT(feeture),uname(feeture)
 7584 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',', ! ------ dissolved metal
     &'RESULTING d/s RIVER QUALITY', ! ------------------------- dissolved metal
     &',',a11,' (dissolved)',3(',',1pe12.4),',',i2,'-percentile', ! ----- E1.CSV
     &(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
     
      write(130+jper,7594)GIScode(feeture),uname(feeture), ! ------------ Ei.CSV
     &dname(jper),C(jper,1),C(jper,2),C(jper,icp),
     &kptarg,JT(feeture),uname(feeture)
 7594 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',', ! ---------- total metal
     &'RESULTING d/s RIVER QUALITY', ! ----------------------------- total metal
     &',',a11,' (total)',3(',',1pe12.4),',',i2,'-percentile', ! --------- Ei.CSV
     %(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
      endif ! if ( detype (jper) .eq. 909 ) ==================== dissolved metal

      !kreplace(jper) = 0
 8088 continue
      
      endif ! if ( ICAL .eq. 9 .and. IBR .eq. 6 ) ~~~~~~~~~~~~~~~~~~~~~~~ Mode 9
      endif ! if ( ifeffcsv .eq. 1 etc ) =======================================
      endif ! if ( kreplace(jper) .eq. 1 ) =====================================
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr        

      endif ! if ( IBR .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc ------------------------------------
     
      
*     metals in dissolved and solid forms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else ! if ( Detype (jper) .lt. 900) it is > 899 tttttttttttttttttttttttttt
          
      write(01,3094)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- OUT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! =========== EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or. ! ----------- EFF
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then 
      write(31,3094)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- EFF
      write(150+jper,3094)DNAME(jper),valchars10,UNITS(jper),SET1 ! ----- Di EFF
 3094 format(a11,' (total)',36X,' Mean =',a10,1x,a4,4x,a13) ! ------------------
      endif ! if ( IBR .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc ====================================

      if ( A .lt. 1.0e-9 ) then ! ------------------------------------------ EFF
      write(01,1284) ! write a line -------------- ------------------------- OUT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ----------- EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,1284) ! ----------------------------------------------------- EFF
      write(150+jper,1284) ! -------------------------------------------- Di EFF
      endif ! if ( IBR .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc ------------------------------------
      write(170+jper,1284) ! -------------------------------------------- Ci.GAP
      goto 11
      endif ! if ( A .lt. 1.0e-9 ) ---------------------------------------------
      
      
      if ( kreplace(jper) .eq. 1 ) then ! kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
      if ( ifeffcsv .eq. 1 .and. JT (feeture) .ne. 12 ) then ! =================
      if ( ICAL .eq. 9 .and. IBR .eq. 6 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
      !write(130+jper,7516)GIScode(feeture),uname(feeture), ! ----------- Ei.CSV
      !&dname(jper),C(jper,1),C(jper,2),C(jper,icp),
      !&kptarg,JT(feeture),uname(feeture)
 7516 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',',
     &'RESULTING d/s RIVER QUALITY', ! ================================ not used
     &',',a11,3(',',1pe12.4),',',i2,'-percentile',(',',i4),',',a35) ! --- Ei.CSV

      if ( detype (jper) .eq. 909 ) then ! ===================== dissolved metal
      write(130+jper,9584)GIScode(feeture),uname(feeture), ! ------------ Ei.CSV
     &dname(jper),cpart1(jper,1),cpart1(jper,2),cpart1(jper,icp),
     &kptarg,JT(feeture),uname(feeture)
 9584 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',',
     &'RESULTING d/s RIVER QUALITY', ! ========================= dissolved metal
     &',',a11,' (dissolved)',3(',',1pe12.4),',',i2,'-percentile', ! ----- E1.CSV
     &(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
     
      write(130+jper,9594)GIScode(feeture),uname(feeture), ! ------------ Ei.CSV
     &dname(jper),C(jper,1),C(jper,2),C(jper,icp),
     &kptarg,JT(feeture),uname(feeture)
 9594 format(' ',a40,',',a40,',',
     &'Remove the adjustment to u/s river quality',',',
     &'RESULTING d/s RIVER QUALITY', ! ============================= total metal
     &',',a11,' (total)',3(',',1pe12.4),',',i2,'-percentile', ! --------- Ei.CSV
     %(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
     
      endif ! if ( detype (jper) .eq. 909 ) -------------------- dissolved metal

      endif ! if ( ICAL .eq. 9 .and. IBR .eq. 6 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( ifeffcsv .eq. 1 etc ) =======================================
      endif ! if ( kreplace(jper) .eq. 1 ) kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk

      endif !  ! if ( Detype (jper) .lt. 900) tttttttttttttttttttttttttttttttttt
*     ==========================================================================
     
      
*     standard deviation =======================================================
      write(01,3194)valchars11,UNITS(jper),set5 ! -------------------------- OUT
 3194 format(42X,'Standard deviation =',a10,1X,A4,4x,a13) ! ---------------- EFF
*     write(200+jper,3194)valchars11,UNITS(jper),set5 ! ------------------Di OUT
*     write(170+jper,3194)valchars11,UNITS(jper),set5 ! ----------------- Ci.GAP
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ----------- EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,3194)valchars11,UNITS(jper),set5 ! -------------------------- EFF
      write(150+jper,3194)valchars11,UNITS(jper),set5 ! ------------------Di EFF
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( JT (feeture) .eq. 03,05,12 etc -------------------------- EFF
*     ==========================================================================
     
     
*     90 and 95-percentile =====================================================
      call sort format 2 (C(jper,4),C(jper,3)) ! 90 and 90 percentiles ========= 
      write(01,1194)valchars10,UNITS(jper),SET3, ! ------------------------- OUT
     &valchars11,UNITS(jper),SET2 ! ---------------------------------------- OUT
 1194 format(
     &47x,'90-percentile =',a10,1X,A4,4x,a13/
     &47x,'95-percentile =',a10,1X,A4,4x,a13)
      if ( KSIM .eq. 1 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++
      write(170+jper,1194)valchars10,UNITS(jper),SET3, ! ---------------- Ci.GAP
     &valchars11,UNITS(jper),SET2 ! ------------------------------------- Ci.GAP
      write(170+jper,1284) ! -------------------------------------------- Ci.GAP
      endif ! if ( KSIM .eq. 1 ) +++++++++++++++++++++++++++++++++++++++++++++++
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ----------- EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or. ! ----------- EFF
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,1194)valchars10,UNITS(jper), ! ------------------------------ EFF
     &SET3,valchars11,UNITS(jper),SET2 ! ----------------------------------- EFF
      write(150+jper,1194)valchars10,UNITS(jper), ! ----------------------Di EFF
     &SET3,valchars11,UNITS(jper),SET2 ! ---------------------------------Di EFF
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( JT (feeture) .eq. 03,05,12 etc -------------------------- EFF
*     ==========================================================================

     
*     99-percentile ============================================================
      call sort format 1 (C(jper,5)) ! 99-percentile ===========================
      write(01,1294)valchars10,UNITS(jper),SET4 ! -------------------------- OUT
 1294 format(42X,'     99-percentile =',a10,1X,A4,4x,a13)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ----------- EFF
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or. ! ----------- EFF
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or. ! ----------- EFF
     &     JT (feeture) .eq. 62 ) then ! ----------------------------------- EFF
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,1294)valchars10,UNITS(jper),SET4 ! -------------------------- EFF
      write(31,1284) ! ----------------------------------------------------- EFF
      write(150+jper,1294)valchars10,UNITS(jper),SET4 ! ------------------Di EFF
      write(150+jper,1284) ! ---------------------------------------------Di EFF
 1284 format(110('-'))
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( JT (feeture) .eq. 03,05,12 etc -------------------------- EFF
*     ==========================================================================
      
      goto 11
*     --------------------------------------------------------------------------



*     ==========================================================================
 3004 continue ! for types of determinand like Dissolved Oxygen
*     --------------------------------------------------------------------------
 
      call sort format 2 (A,S) ! mean and standard deviation ===================
      write(01,1494)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- OUT
 1494 format(A11,44X,' Mean =',a10,1X,A4,4x,a13) ! ------------------------- OUT

      if ( JQCAL(feeture) .gt. 0 ) then ! ======================================
      if ( JT(feeture) .ne. 1 ) then ! ---------------------------------- Ci.GAP
      write(170+jper,4638) ! -------------------------------------------- Ci.GAP
 4638 format(/'Quality gap-filling has been requested for a feature ', !  Ci.GAP
     &'that is not a monitoring point ...'/) ! -------------------------- Ci.GAP
      endif ! if ( JT(feeture) .ne. 1 ) --------------------------------- Ci.GAP
      !write(170+jper,2494)DNAME(jper),valchars10,UNITS(jper),SET1 ! ---- Ci.GAP
 2494 format(A11,44X,' Mean =',a10,1X,A4,4x,a13) ! ---------------------- Ci.GAP
      endif ! if ( JQCAL(feeture) .gt. 0 =======================================

      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,2794)DNAME(jper),valchars10,UNITS(jper),SET1 ! -------------- EFF
      write(150+jper,2794)DNAME(jper),valchars10,UNITS(jper),SET1 ! ------Di EFF
 2794 format(A11,44X,' Mean =',a10,1X,A4,4x,a13) ! ------------------------- EFF
      endif ! if ( IBR .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03,05,12 etc ------------------------------
      if ( A .lt. 1.0e-9 ) goto 11
      write(01,4094)valchars11,UNITS(jper),set5
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,4094)valchars11,UNITS(jper),set5 ! -------------------------- EFF
      write(150+jper,4094)valchars11,UNITS(jper),set5 ! ----------------- Di EFF
 4094 format(42X,'Standard deviation =',a10,1X,A4,4x,a13)
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( JT (feeture) .eq. 03,05,12 etc ------------------------------
*     ==========================================================================


*     5 and 10-percentile ======================================================
      call sort format 2 (C(jper,4),C(jper,3)) 
      write(01,2194)valchars10,UNITS(jper),SET3,
     &valchars11,UNITS(jper),SET2
 2194 format(42X,'     10-percentile =',a10,1X,A4,4x,a13/
     &42X,'      5-percentile =',a10,1X,A4,4x,a13)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,2194)valchars10,UNITS(jper),SET3, ! ------------------------- EFF
     &valchars11,UNITS(jper),SET2
      write(150+jper,2194)valchars10,UNITS(jper),SET3, ! ------------------- EFF
     &valchars11,UNITS(jper),SET2
      endif ! if ( IBR .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03,05,12 etc ------------------------------
*     ==========================================================================


*     1-percentile =============================================================
      call sort format 1 (C(jper,5))
      write(01,2694)valchars10,UNITS(jper),SET4
 2694 format(47X,' 1-percentile =',a10,1X,A4,4x,a13)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,2694)valchars10,UNITS(jper),SET4 ! -------------------------- EFF
      write(150+jper,2694)valchars10,UNITS(jper),SET4 ! ----------------- Di EFF
      write(31,1284) ! ----------------------------------------------------- EFF
      write(150+jper,1284) ! ---------------------------------------------Di EFF
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( JT (feeture) .eq. 03,05,12 etc ------------------------------
*     ==========================================================================
     
      
      
   11 continue
      
      write(01,8000)
 8000 format(110('-'))
      endif ! if ( ical13 .eq. 0 ) +++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( nobigout .le. 0 ) ###########################################



*     non-parametric distributions --------------------------------------------- 
      if ( QTYPE (jper) .eq. 03 .or. C(jper,1) .gt. -1.0E-25) goto 32

   40 if ( ical13 .eq. 0 ) write(01,1854)DNAME(jper),A,UNITS(jper)
 1854 format(54X,A11,F7.2,1X,A4)

   32 continue

      
*     ==========================================================================
      if ( IQ .gt. 0 ) then ! ==================================================
      if ( IBR .eq. 1 ) then ! -------------------------------------------------
      if ( PDRC (IQ,jper) .eq. 4 .or. PDRC (IQ,jper) .eq. 9 ) then ! -----------
*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, jper+1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue
      if ( nobigout .le. 0 .and. ical .ne. 1 ) write(01,1965) 
     &flnamesmall(2,icod)
 1965 format(
     &'From a non-parametric distribution ... File: ',a64)
      if ( ical13 .eq. 0 ) write(01,8000)

 1974 continue
      endif ! if ( PDRC (IQ,jper) .eq. 4 ) --------- non-parametric distribution
      endif ! if ( IBR .eq. 1 ) ------------------------------------------------

      
*     identify the file with the monthly quality data --------------------------
      if ( PDRC(IQ,jper) .eq. 5 .and. IBR .eq. 1 ) then
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) write(01,2965)DNAME(jper),uname(feeture)
 2965 format(77('-')/
     &'The river quality data used at the head of Reach were ',
     &'extracted from monthly'/'distributions for ... ',a11,' at ',a37)
      endif

      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, jper + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code was found for the datafile ---------------------------------
      write( *,3)
      if ( ical13 .eq. 0 ) write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = flmonth(2,icod), EXIST = exists )
      if ( .NOT. exists) then ! ------------------------------------------------
      write( *,7863) FLMONTHsmall(2,icod)
      if ( ical13 .eq. 0 ) write(01,7863) FLMONTHsmall(2,icod)
      write(09,7863) FLMONTHsmall(2,icod)
      write(33,7863) FLMONTHsmall(2,icod)
 7863 Format(77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      endif ! if ( .NOT. exists) -----------------------------------------------

      if ( nobigout .le. 0 .and. ica13 .eq. 0 ) then ! -------------------------
      write(01,7963) FLMONTHsmall(2,icod)
 7963 format('The monthly data file is ',a64)
      if ( ical13 .eq. 0 ) write(01,4398)Dname(jp)
 4398 format(77('-')/
     &'Monthly data on river quality for ... ',a11/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))

      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),
     &seas0(i),i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18)
      write(01,8000)
  
      endif ! if ( nobigout .le. 0 .and. ica13 .eq. 0 ) ------------------------
      endif ! if ( PDRC (IQ,jper) .eq. 5 .and. IBR .eq. 1 ) --------------------
      endif ! if ( IQ .gt. 0 ) =================================================
*     ==========================================================================
      
      

      JP = jper
      if ( MONQ .gt. 1 ) call write shots for river quality 

      if ( Detype (jper) .ge. 900 ) then ! dissolved and solid metal
      call write out dissolved metal (IBR, jper)
      call write out solid metal (IBR, jper)
      endif ! dissolved and solid metal

   12 continue

      return
      end

      
      
      
      subroutine write out dissolved metal (IBR, jper) ! dissolved metal
      include 'COMMON DATA.FOR'
      logical exists
*     storage for writing out confidence limits for mean and the percentiles ---
      character *13 SET1,SET2,SET3,SET4,SET5

*     check for excluded determinands ------------------------------------------
      if ( QTYPE (jper) .ne. 4 ) then

*     initialise percentiles and their confidence limits -----------------------
      dcp95 = 0.0
      dcpl95 = 0.0
      dcpu95 = 0.0
      dcp90 = 0.0
      dcpl90 = 0.0
      dcpu90 = 0.0
      dcp99 = 0.0
      dcpl99 = 0.0
      dcpu99 = 0.0

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, cpart1(jper,1) )
      S = amax1 ( 0.0, cpart1(jper,2) )
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

*     check for zero mean ------------------------------------------------------
      if ( cpart1 (jper,1) .lt. 1.0E-08 ) then ! dissolved metal
      cpart1 (jper,2) = 0.0
      S = cpart1 (jper,2)
      goto 10
      endif ! dissolved metal

*     check for zero standard deviation ----------------------------------------
      if ( cpart1 (jper,2) .lt. 1.0E-08 ) goto 10

*     set the standard deviation -----------------------------------------------
      S = cpart1 (jper,2)

*     coefficient of variation -------------------------------------------------
      CoV = S / cpart1 (jper,1)

*     switch between Log-Normal and Normal according to determinand type -------
      goto (30,30,31,30,30,30), QTYPE(jper)

*     the data follow the Log-Normal Distribution ------------------------------
   30 continue
      GM = alog ((A*A)/SQRoot(1069,A*A+S*S))
      GS = SQRoot(107015,alog(1.+(S*S)/(A*A)))

*     percentiles assuming a Log-Normal Distribution --------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )
      FF = 1.0

*     switch to 5-percentile etc ... for Dissolved Oxygen ----------------------
      if ( QTYPE(jper) .eq. 03 .or. QTYPE (jper) .eq. 5 ) FF=-FF

   23 dcp95 = exp  ( GM + FF * t95 * GS )
      dcp90 = exp  ( GM + FF * t90 * GS )
      dcp99 = exp  ( GM + FF * t99 * GS )

*     calculate confidence limits. Calculate standard normal deviates ----------
   22 continue

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence limits ---------------------------------------------
      dcpl90 = amax1 (0.0, exp (GM+FF*to90*GS)-dcp90 + cpart1 (jper,4) ) ! dissolved metal
      dcpl95 = amax1 (0.0, exp (GM+FF*to95*GS)-dcp95 + cpart1 (jper,3) )
      dcpl99 = amax1 (0.0, exp (GM+FF*to99*GS)-dcp99 + cpart1 (jper,5) )

*     pessimistic confidence limits --------------------------------------------
      dcpu90 = amax1 (0.0, exp (GM+FF*tp90*GS)-dcp90 + cpart1 (jper,4) ) ! dissolved metal
      dcpu95 = amax1 (0.0, exp (GM+FF*tp95*GS)-dcp95 + cpart1 (jper,3) )
      dcpu99 = amax1 (0.0, exp (GM+FF*tp99*GS)-dcp99 + cpart1 (jper,5) )

   28 continue
      SEM = S/SQRoot(107285,QUALN(jper))

      XL = amax1 (0.0,(A-FF*t95*SEM))
      XU = amax1 (0.0,(A+FF*t95*SEM))

      if ( QTYPE (jper) .ne. 3 .and. QTYPE (jper).ne.5 ) then
      call assem(XL,XU,SET1)
      else
      call assem(XU,XL,SET1)     
      endif
      
      SES = S / sqrt ( 2.0 * (QUALN(jper) - 1.0) )
      XL = amax1 (0.0, (S - t95 * SES) )
      XU = amax1 (0.0, (S + t95 * SES) )
      call assem(XL,XU,SET5)

      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)
      goto 10

*     normal distribution ------------------------------------------------------
   31 GM=A
      GS=S

*     percentiles assuming a normal distribution -------------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )

      FF=1.0

*     switch to 5 and 10-percentile --------------------------------------------
      if ( QTYPE (jper) .eq. 03 .or. QTYPE (jper) .eq. 5 ) FF=-FF

   43 dcp95 = GM + FF * t95 * GS
      dcp90 = GM + FF * t90 * GS
      dcp99 = GM + FF * t99 * GS

*     calculate confidence limits. Calculate standard normal deviates ----------
   42 continue
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence limits ---------------------------------------------
      dcpl90 = amax1 (0.0,(GM+FF*to90*GS-dcp90 + cpart1 (jper,4) )) ! dissolved metal
      dcpl95 = amax1 (0.0,(GM+FF*to95*GS-dcp95 + cpart1 (jper,3) ))
      dcpl99 = amax1 (0.0,(GM+FF*to99*GS-dcp99 + cpart1 (jper,5) ))

*     pessimistic confidence limits --------------------------------------------
      dcpu90 = amax1 (0.0,(GM+FF*tp90*GS-dcp90 + cpart1 (jper,4) )) ! dissolved metal
      dcpu95 = amax1 (0.0,(GM+FF*tp95*GS-dcp95 + cpart1 (jper,3) ))
      dcpu99 = amax1 (0.0,(GM+FF*tp99*GS-dcp99 + cpart1 (jper,5) ))
      goto 28
   10 continue

      if ( nobigout .le. 0 ) then
      goto (3000,3000,3004,3000,3004,3000),qtype(jper)

 3000 continue
      call sort format 2 (A,S) 
      if ( ical13 .eq. 0 ) write(01,1094)DNAME(jper),
     &valchars10,UNITS(jper),SET1,valchars11,UNITS(jper),SET5
 1094 format(a11,' (dissolved)',33x,'Mean =',a10,1X,A4,4x,a13/
     &42X,'Standard deviation =',a10,1X,A4,4x,a13)
	if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,1094)DNAME(jper),valchars10, ! ------------------------------ EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5
      write(150+jper,1094)DNAME(jper),valchars10, ! ----------------------Di EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif ! if ( JT (feeture) .eq. 03 etc ------------------------------------
      call sort format 2 (cpart1(jper,4),cpart1(jper,3)) 
      if ( ical13 .eq. 0 ) write(01,1194)valchars10,UNITS(jper),SET3,
     &valchars11,UNITS(jper),SET2
 1194 format(47X,'90-percentile =',a10,1X,A4,4x,a13/
     &47X,'95-percentile =',a10,1X,A4,4x,a13)
	if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( IBR .ne. 3 ) then ! -------------------------------------------------
      write(31,1194)valchars10,UNITS(jper), ! ------------------------------ EFF
     &SET3,valchars11,UNITS(jper),SET2
      write(150+jper,1194)valchars10,UNITS(jper), ! ----------------------Di EFF
     &SET3,valchars11,UNITS(jper),SET2
      endif ! if ( IBR .ne. 3 ) ------------------------------------------------
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif ! if ( JT (feeture) .eq. 03 ) --------------------------------------
      call sort format 1 (cpart1(jper,5)) 
      if ( ical13 .eq. 0 ) write(01,1294)valchars10,UNITS(jper),SET4
 1294 format(47X,'99-percentile =',a10,1X,A4,4x,a13/110('-'))
	if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( IBR .ne. 3 ) then
      write(31,1294)valchars10,UNITS(jper),SET4 ! -------------------------- EFF
      write(150+jper,1294)valchars10,UNITS(jper),SET4 ! ------------------Di EFF
      endif
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif
      goto 11
       
      
 3004 continue ! dissolved oxygen etc ------------------------------------------
      if ( ical13 .eq. 0 ) then ! ==============================================
      call sort format 2 (A,S) 
      write(01,1094)DNAME(jper),valchars10,
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5
      write(31,1094)DNAME(jper),valchars10, ! ------------------------------ EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5 ! ----------------------- EFF
      write(150+jper,1094)DNAME(jper),valchars10, ! --------------------- Di EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5 ! ---------------------Di EFF
      call sort format 2 (cpart1(jper,4),cpart1(jper,3)) 
      write(01,2194)valchars10,UNITS(jper),SET3,
     &valchars11,UNITS(jper),SET2
 2194 format(47X,'10-percentile =',a10,1X,A4,4x,a13/
     &47X,' 5-percentile =',a10,1X,A4,4x,a13)
      write(31,2194)valchars10,UNITS(jper),SET3, ! ------------------------- EFF
     &valchars11,UNITS(jper),SET2 ! ---------------------------------------- EFF
      write(150+jper,2194)valchars10,UNITS(jper),SET3, ! ---------------- Di EFF
     &valchars11,UNITS(jper),SET2
      call sort format 1 (cpart1(jper,5)) 
      write(01,3294)valchars10,UNITS(jper),SET4
 3294 format(47X,' 1-percentile =',a10,1X,A4,4x,a13/110('-'))
      write(31,3294)valchars10,UNITS(jper),SET4 ! -------------------------- EEF
      write(150+jper,3294)valchars10,UNITS(jper),SET4 ! ----------------- Di EFF
      endif ! if ( ical13 .eq. 0 ) =============================================

   11 continue
      endif

*     non-parametric distributions --------------------------------------------- 
      if (QTYPE(jper) .eq. 03 .or. cpart1(jper,1) .gt. -1.0E-25) goto 32

   40 if ( ical13 .eq. 0 ) write(01,1854)DNAME(jper),A,UNITS(jper)
 1854 format(54X,A11,F7.2,1X,A4)

   32 continue
      if ( IQ .gt. 0 ) then
      if ( IBR .eq. 1 ) then
      if ( PDRC (IQ,jper) .eq. 4 .or. PDRC (IQ,jper) .eq. 9 ) then

*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, jper+1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1965) 
     &flnamesmall(2,icod)
 1965 format(
     &'Non-parametric distribution ... File: ',a64/77('-'))
 1974 continue
      endif
      endif

*     identify the file with the monthly data ----------------------------------
      if ( PDRC(IQ,jper) .eq. 5 .and. IBR .eq. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,2965)
 2965 format(77('-')/
     &'The river quality data for the head of Reach were ',
     &'extracted from'/'twelve monthly distributions ...')

      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, jper + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in monthly river quality data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = flmonth(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7863) FLMONTHsmall(2,icod)
      write(01,7863) FLMONTHsmall(2,icod)
      write(09,7863) FLMONTHsmall(2,icod)
      write(33,7863) FLMONTHsmall(2,icod)
 7863 Format(77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) write(01,7963)dname(jper),
     &FLMONTHsmall(2,icod)
 7963 Format('The monthly data file is ',a64)
      if ( ical13 .eq. 0 ) write(01,4398)dname(jp)
 4398 format(77('-')/
     &'Monthly data on river quality ... ',a11/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))

      if ( ical13 .eq. 0 ) write(01,2399)(seas1(i),seas2(i),seas3(i),
     &seas4(i),seas0(i), i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)

      endif ! if ( nobigout .le. 0 )
      endif ! if ( PDRC(IQ,jper) .eq. 5 .and. IBR .eq. 1 )
      endif ! if ( IQ .gt. 0 )

      JP = jper
      if ( MONQ .gt. 1 ) call write shots for river quality 
      endif

      return
      end


      subroutine write out solid metal (IBR, jper) ! solid metal
      include 'COMMON DATA.FOR'
      logical exists

*     storage for writing out confidence limits for mean and the percentiles ---
      character *13 SET1,SET2,SET3,SET4,SET5

*     check for excluded determinands ------------------------------------------
      if ( QTYPE (jper) .ne. 4 ) then

*     initialise percentiles and their confidence limits -----------------------
      dcp95 = 0.0
      dcpl95 = 0.0
      dcpu95 = 0.0
      dcp90 = 0.0
      dcpl90 = 0.0
      dcpu90 = 0.0
      dcp99 = 0.0
      dcpl99 = 0.0
      dcpu99 = 0.0

*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, cpart2 (jper,1) ) ! solid metal
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

*     check for zero mean ------------------------------------------------------
      if ( cpart2 (jper,1) .lt. 1.0E-08 ) then ! solid metal
      cpart2 (jper,2) = 0.0
      S = cpart2 (jper,2)
      goto 10
      endif ! solid metal

*     check for zero standard deviation ----------------------------------------
      if ( cpart2 (jper,2) .lt. 1.0E-08 ) goto 10

*     set the standard deviation -----------------------------------------------
      S = cpart2 (jper,2) ! solid metal

*     coefficient of variation -------------------------------------------------
      CoV = S / cpart2 (jper,1) ! solid metal

*     switch between Log-Normal and Normal according to determinand type -------
      goto (30,30,31,30,30,30), QTYPE(jper)

*     the data follow the Log-Normal Distribution ------------------------------
   30 continue
      GM = alog ((A*A)/SQRoot(1069,A*A+S*S))
      GS = SQRoot(107016,alog(1.+(S*S)/(A*A)))

*     percentiles assuming a Log-Normal Distribution --------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )
      FF = 1.0

*     switch to 5-percentile etc ... for Dissolved Oxygen ----------------------
      if ( QTYPE (jper) .eq. 03 .or. QTYPE (jper) .eq. 5 ) FF=-FF

   23 dcp95 = exp  ( GM + FF * t95 * GS )
      dcp90 = exp  ( GM + FF * t90 * GS )
      dcp99 = exp  ( GM + FF * t99 * GS )

*     calculate confidence limits. Calculate standard normal deviates ----------
   22 continue

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence limits ---------------------------------------------
      dcpl90 = amax1  (0.0, exp (GM+FF*to90*GS)-dcp90 + cpart2 (jper,4))
      dcpl95 = amax1  (0.0, exp (GM+FF*to95*GS)-dcp95 + cpart2 (jper,3))
      dcpl99 = amax1  (0.0, exp (GM+FF*to99*GS)-dcp99 + cpart2 (jper,5))

*     pessimistic confidence limits --------------------------------------------
      dcpu90 = amax1  (0.0, exp (GM+FF*tp90*GS)-dcp90 + cpart2 (jper,4))
      dcpu95 = amax1  (0.0, exp (GM+FF*tp95*GS)-dcp95 + cpart2 (jper,3))
      dcpu99 = amax1  (0.0, exp (GM+FF*tp99*GS)-dcp99 + cpart2 (jper,5))

   28 continue
      SEM = S/SQRoot(107286,QUALN(jper))

      XL = amax1 (0.0,(A-FF*t95*SEM))
      XU = amax1 (0.0,(A+FF*t95*SEM))

      if ( QTYPE (jper) .ne. 3 .and. QTYPE (jper) .ne. 5 ) then
      call assem(XL,XU,SET1)
      else
      call assem(XU,XL,SET1)     
      endif
      
      SES = S / sqrt ( 2.0 * (QUALN(jper) - 1.0) )
      XL = amax1 (0.0, (S - t95 * SES) )
      XU = amax1 (0.0, (S + t95 * SES) )
      call assem(XL,XU,SET5)

      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)
      goto 10

*     normal distribution ------------------------------------------------------
   31 GM=A
      GS=S

*     percentiles assuming a normal distribution -------------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )

      FF=1.0

*     switch to 5 and 10-percentile --------------------------------------------
      if ( QTYPE (jper) .eq. 03 .or. QTYPE (jper) .eq. 5 ) FF=-FF

   43 dcp95 = GM + FF * t95 * GS
      dcp90 = GM + FF * t90 * GS
      dcp99 = GM + FF * t99 * GS

*     calculate confidence limits. Calculate standard normal deviates ----------
   42 continue
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

      to95 = tshift (QUALN(jper),t95,-FF)
      tp95 = tshift (QUALN(jper),t95, FF)
      to90 = tshift (QUALN(jper),t90,-FF)
      tp90 = tshift (QUALN(jper),t90, FF)
      to99 = tshift (QUALN(jper),t99,-FF)
      tp99 = tshift (QUALN(jper),t99, FF)

*     optimistic confidence limits ---------------------------------------------
      dcpl90 = amax1 (0.0,(GM+FF*to90*GS-dcp90 + cpart2 (jper,4) ))
      dcpl95 = amax1 (0.0,(GM+FF*to95*GS-dcp95 + cpart2 (jper,3) ))
      dcpl99 = amax1 (0.0,(GM+FF*to99*GS-dcp99 + cpart2 (jper,5) ))

*     pessimistic confidence limits --------------------------------------------
      dcpu90 = amax1 (0.0,(GM+FF*tp90*GS-dcp90 + cpart2 (jper,4) ))
      dcpu95 = amax1 (0.0,(GM+FF*tp95*GS-dcp95 + cpart2 (jper,3) ))
      dcpu99 = amax1 (0.0,(GM+FF*tp99*GS-dcp99 + cpart2 (jper,5) ))
      goto 28
   10 continue

      if ( nobigout .le. 0 ) then
      goto (3000,3000,3004,3000,3004,3000),qtype(jper)

 3000 continue
      if ( ical13 .eq. 0 ) then ! ==============================================
      call sort format 2 (A,S) 
      write(01,1094)DNAME(jper),valchars10,
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5
 1094 format(A11,' (solid)',36x,' Mean =',a10,1X,A4,4x,a13/
     &42X,'Standard deviation =',a10,1X,A4,4x,a13)
	if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,1094)DNAME(jper),valchars10, ! ------------------------------ EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5 ! ----------------------- EFF
      write(150+jper,1094)DNAME(jper),valchars10, ! ----------------------Di EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper),SET5 
      endif
      endif
      call sort format 2 (cpart2(jper,4),cpart2(jper,3)) 
      write(01,1194)valchars10,UNITS(jper),SET3,
     &valchars11,UNITS(jper),SET2
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,1194)valchars10,UNITS(jper), ! ------------------------------ EFF
     &SET3,valchars11,UNITS(jper),SET2
      write(150+jper,1194)valchars10,UNITS(jper), ! --------------------- Di EFF
     &SET3,valchars11,UNITS(jper),SET2
 1194 format(47X,'90-percentile =',a10,1X,A4,4x,a13/
     &47X,'95-percentile =',a10,1X,A4,4x,a13)
      endif 
      endif
      call sort format 1 (cpart2(jper,5)) 
      write(01,1294)valchars10,UNITS(jper),SET4
 1294 format(47X,'99-percentile =',a10,1X,A4,4x,a13/110('-'))
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .ne. 3 ) then
      write(31,1294)valchars10,UNITS(jper),SET4 ! -------------------------- EFF
      write(150+jper,1294)valchars10,UNITS(jper),SET4 ! ----------------- Di EFF
      endif
      endif
      endif ! if ( ical13 .eq. 0 ) =============================================
      goto 11

 3004 continue
      call sort format 2 (A,S) 
      if ( ical13 .eq. 0 ) then ! ==============================================
      write(01,1094)DNAME(jper),valchars10,
     &UNITS(jper),SET1,valchars11,UNITS(jper)
      write(31,1094)DNAME(jper),valchars10, ! ------------------------------ EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper) ! ---------------------------- EFF
      write(150+jper,1094)DNAME(jper),valchars10, ! --------------------- Di EFF
     &UNITS(jper),SET1,valchars11,UNITS(jper)
      call sort format 2 (cpart1(jper,4),cpart1(jper,3)) 
      write(01,2194)valchars10,UNITS(jper),
     &SET3,valchars11,UNITS(jper),SET2
 2194 format(47X,'10-percentile =',a10,1X,A4,4x,a13/
     &47X,' 5-percentile =',a10,1X,A4,4x,a13)
      write(31,2194)valchars10,UNITS(jper), ! ------------------------------ EFF
     &SET3,valchars11,UNITS(jper),SET2
      call sort format 1 (cpart1(jper,5)) 
      write(01,3294)valchars10,UNITS(jper),SET4
 3294 format(47X,' 1-percentile =',a10,1X,A4,4x,a13/110('-'))
      write(31,3294)valchars10,UNITS(jper),SET4 ! -------------------------- EFF
      write(150+jper,3294)valchars10,UNITS(jper),SET4 ! ----------------- Di EFF
      endif ! if ( ical13 .eq. 0 ) =============================================
   11 continue
      endif

*     non-parametric distributions --------------------------------------------- 
      if ( QTYPE(jper) .eq. 03 .or. 
     &cpart2(jper,1) .gt. -1.0E-25) goto 32

   40 if ( ical13 .eq. 0 ) write(01,1854)DNAME(jper),A,UNITS(jper)
 1854 format(54X,A11,F7.2,1X,A4)

   32 continue
      if ( IQ .gt. 0 ) then
      if ( IBR .eq. 1 ) then
      if ( PDRC (IQ,jper) .eq. 4 .or. PDRC (IQ,jper) .eq. 9 ) then

*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, jper+1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1965) 
     &flnamesmall(2,icod)
 1965 format(
     &'Non-parametric distribution ... File: ',a64/77('-'))
 1974 continue
      endif
      endif

*     identify the file with the monthly data ----------------------------------
      if ( PDRC(IQ,jper) .eq. 5 .and. IBR .eq. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 )write(01,2965)
 2965 format(77('-')/
     &'The river quality data for the head of Reach were ',
     &'extracted from'/'twelve monthly distributions ...')

      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, jper + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      if ( ical13 .eq. 0 ) write(01,3)
      write(09,3)
      write(33,3)
    3 Format(77('-')/'*** Error in monthly river quality data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = flmonth(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7863) FLMONTHsmall(2,icod)
      if ( ical13 .eq. 0 ) write(01,7863) FLMONTHsmall(2,icod)
      write(09,7863) FLMONTHsmall(2,icod)
      write(33,7863) FLMONTHsmall(2,icod)
 7863 Format(77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif

      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) write(01,7963) dname(jper),
     &FLMONTHsmall(2,icod)
 7963 Format('The monthly data file is ',a64)
      if ( ical13 .eq. 0 ) write(01,4398)Dname(jp)
 4398 format(77('-')/
     &'Monthly data on river quality ... ',a11/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))

      if ( ical13 .eq. 0 ) write(01,2399)(seas1(i),seas2(i),seas3(i),
     &seas4(i),seas0(i), i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)

      endif
      endif
      endif

      JP = jper
      if ( MONQ .gt. 1 ) call write shots for river quality 

      endif

      return
      end




*     compute statistics for profile plots of river quality --------------------
      subroutine compute statistics for plots of river quality 
     &(JPART,JDET,XM,XL,XU,D90p,D90L,D90U,D95p,D95L,D95U,D99p,D99L,D99U)
*     --------------------------------------------------------------------------
*     XM  ... is the mean
*     XL   .. is the lower confidence limit on the mean
*     XU   .. is the upper confidence limit on the mean
*     DCP ... is the calculated value of the 95-percentile
*     DCPL... is the lower confidence limit on the percentile
*     DCPU... is the upper confidence limit on the percentile
*     D90 ... is the calculated value of the 90-percentile
*     D90L... is the lower confidence limit on the percentile
*     D90U... is the upper confidence limit on the percentile
*     D99  .. is the calculated value of the 99-percentile
*     D99L... is the lower confidence limit on this percentile
*     D99U... is the upper confidence limit on this percentile
*     --------------------------------------------------------------------------
      include 'COMMON DATA.FOR'

      D95 = 0.0
      D90 = 0.0
      D99 = 0.0
      D95L = 0.0
      D95U = 0.0
      D90L = 0.0
      D90U = 0.0
      D99L = 0.0
      D99U = 0.0
      XM = 0.0 ! annual mean
      XL = 0.0 ! lower confidence limit annual mean
      XU = 0.0 ! upper confidence limit annual mean
      D95P = 0.0 ! calculated percentile
      D90P = 0.0 ! calculated percentile
      D99P = 0.0 ! calculated percentile

      A = amax1 ( 0.0, C( JDET, 1 )) ! annual mean
      S = amax1 ( 0.0, C( JDET, 2 )) ! standard deviation
      if ( detype (JDET) .ge. 900 ) then ! dissolved and solid metal
      if ( JPART .eq. 2 ) then ! dissolved metal
      A = amax1 ( 0.0, cpart1 ( JDET, 1 )) ! dissolved metal
      S = amax1 ( 0.0, cpart1 ( JDET, 2 )) ! dissolved metal
      endif ! dissolved metal
      if ( JPART .eq. 3 ) then ! solid metal
      A = amax1 ( 0.0, cpart2 ( JDET, 1 )) ! solid metal
      S = amax1 ( 0.0, cpart2 ( JDET, 2 )) ! solid metal
      endif ! solid metal
      endif ! dissolved and solid metal

*     check for a zero calculated mean -----------------------------------------
      if ( A .lt. 1.0E-08 .or. S .lt. 1.0e-10 ) then	
      S = 0.0 ! standard deviation
*     initialise percentiles to the calculated mean ----------------------------
      D95P = A
      D95L = A
      D95U = A
      D90P = A
      D90L = A
      D90U = A
      D99P = A
      D99L = A
      D99U = A
      XM = A
      XL = A
      XU = A
      return
      endif

*     start the calculations for the percentiles -------------------------------
*     get the calculated values ------------------------------------------------
      D95P = C (JDET, 3)
      D90P = C (JDET, 4)
      D99P = C (JDET, 5)
      if ( detype (JDET) .ge. 900 ) then ! dissolved and solid metal
      if ( JPART .eq. 2 ) then ! dissolved metal
      D95P = cpart1 (JDET, 3) ! dissolved metal
      D90P = cpart1 (JDET, 4) ! dissolved metal
      D99P = cpart1 (JDET, 5) ! dissolved metal
      endif ! dissolved metal
      if ( JPART .eq. 3 ) then ! solid metal
      D95P = cpart2 (JDET, 3) ! solid metal
      D90P = cpart2 (JDET, 4) ! solid metal
      D99P = cpart2 (JDET, 5) ! solid metal
      endif ! solid metal
      endif ! dissolved and solid metal 
      
      XM = A ! set the mean
      t95 = errx ( 0.95 ) ! get the standard normal deviate
      t90 = errx ( 0.90 ) ! get the standard normal deviate
      t99 = errx ( 0.99 ) ! get the standard normal deviate

*     start the calculation for the mean ---------------------------------------
*     calculate the standard error of estimate of the mean ---------------------
      xqualn = amax1 ( 0.05, QUALN(JDET) )
      SEM = S / SQRoot( 107333, xqualn )
*     calculate the 95% confidence limits about the mean -----------------------
      XL = amax1 (0.0, (A - t95 * SEM) )
      XU = amax1 (0.0, (A + t95 * SEM) )

*     ==========================================================================
*     log-normal distribution --------------------------------------------------
      GM = alog ( (A*A) / SQRoot(10778,A*A+S*S) )
      GS = SQRoot(100775, alog ( 1.0 + (S*S) / (A*A) ) )
*     95-percentile ------------------------------------------------------------
      if ( QTYPE (Jdet) .ne. 3 .and. qtype(JDET) .ne. 5 ) then
*     calculate the 95-percentile assuming a Log-normal Distribution -----------
      D95 = exp ( GM + t95 * GS )
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t95, -1.0)
      TU = tshift ( QUALN(JDET), t95,  1.0)
*     calculate the Optimistic Confidence Limit --------------------------------
      D95L = amax1 (0.0, D95P - (D95 - exp ( GM + TL * GS)) )
*     calculate the Pessimistic Confidence Limit -------------------------------
      D95U = amax1 (0.0, D95P + (exp ( GM + TU * GS) - D95) )
*     calculate the 90-percentile ----------------------------------------------
      D90 = exp ( GM + t90 * GS )
      TL = tshift ( QUALN(JDET), t90, -1.0)
      TU = tshift ( QUALN(JDET), t90,  1.0)
      D90L = amax1 (0.0, D90P - (D90 - exp ( GM + TL * GS)) )
      D90U = amax1 (0.0, D90P + (exp ( GM + TU * GS) - D90) )
      D99 = exp ( GM + t99 * GS )
      TL = tshift ( QUALN(JDET), t99, -1.0)
      TU = tshift ( QUALN(JDET), t99,  1.0)
      D99L = amax1 (0.0, D99P - (D99 - exp ( GM + TL * GS)) )
      D99U = amax1 (0.0, D99P + (exp ( GM + TU * GS) - D99) )
*     --------------------------------------------------------------------------
      endif
*     5-percentile -------------------------------------------------------------
      if ( QTYPE (Jdet) .eq. 03 .or. QTYPE (JDET) .eq. 5 ) then
      D95 = exp ( GM - t95 * GS )
      TL = tshift ( QUALN(JDET), t95,  1.0)
      TU = tshift ( QUALN(JDET), t95, -1.0)
      D95L = amax1 (0.0, D95P - (D95 - exp ( GM - TL * GS)) )
      D95U = amax1 (0.0, D95P + (exp ( GM - TU * GS) - D95) )
      D90 = exp ( GM - t90 * GS )
      TL = tshift ( QUALN(JDET), t90,  1.0)
      TU = tshift ( QUALN(JDET), t90, -1.0)
      D90L = amax1 (0.0, D90P - (D90 - exp ( GM - TL * GS)) )
      D90U = amax1 (0.0, D90P + (exp ( GM - TU * GS) - D90) )
      D99 = exp ( GM - t99 * GS )
      TL = tshift ( QUALN(JDET), t99,  1.0)
      TU = tshift ( QUALN(JDET), t99, -1.0)
      D99L = amax1 (0.0, D99P - (D99 - exp ( GM - TL * GS)) )
      D99U = amax1 (0.0, D99P + (exp ( GM - TU * GS) - D99) )
      endif
*     ==========================================================================

*     ==========================================================================
*     normal distributions -----------------------------------------------------
      GM = A
      GS = S
*     95-percentile ------------------------------------------------------------
*     if ( QTYPE (Jdet) .ne. 3 .and. QTYPE (JDET) .ne. 5 ) then
      if ( QTYPE (Jdet) .gt. 999 ) then
*     calculate the 95-percentile ----------------------------------------------
      D95 = GM + t95 * GS
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t95, -1.0 )
      TU = tshift ( QUALN(JDET), t95,  1.0 )
*     calculate the Optimistic and Pessimistic Confidence Limits ---------------
      D95L = amax1 (0.0,( GM+TL*GS-D95+D95P) )
      D95U = amax1 (0.0,( GM+TU*GS-D95+D95P) )
      D90 = GM + t90 * GS
      TL = tshift ( QUALN(JDET), t90, -1.0 )
      TU = tshift ( QUALN(JDET), t90,  1.0 )
      D90L = amax1 (0.0,( GM+TL*GS-D90+D90P) )
      D90U = amax1 (0.0,( GM+TU*GS-D90+D90P) )
      D99 = GM + t99 * GS
      TL = tshift ( QUALN(JDET), t99, -1.0 )
      TU = tshift ( QUALN(JDET), t99,  1.0 )
      D99L = amax1 (0.0,( GM+TL*GS-D99+D99P) )
      D99U = amax1 (0.0,( GM+TU*GS-D99+D99P) )
      endif
*     calculate the 5-percentile ----------------------------------------------
      if ( QTYPE (JDET) .eq. 5 ) then
      D95 = GM - t95 * GS
      TL = tshift ( QUALN(JDET), t95,  1.0 )
      TU = tshift ( QUALN(JDET), t95, -1.0 )
      D95L = amax1 (0.0,( GM-TL*GS-D95+D95P) )
      D95U = amax1 (0.0,( GM-TU*GS-D95+D95P) )
      D90 = GM - t90 * GS
      TL = tshift ( QUALN(JDET), t90,  1.0 )
      TU = tshift ( QUALN(JDET), t90, -1.0 )
      D90L = amax1 (0.0,( GM-TL*GS-D90+D90P) )
      D90U = amax1 (0.0,( GM-TU*GS-D90+D90P) )
      D99 = GM - t99 * GS
      TL = tshift ( QUALN(JDET), t99,  1.0 )
      TU = tshift ( QUALN(JDET), t99, -1.0 )
      D99L = amax1 (0.0,( GM-TL*GS-D99+D99P) )
      D99U = amax1 (0.0,( GM-TU*GS-D99+D99P) )
      endif
*     ==========================================================================
 
      return
      end







*     compute statistics on load for profile plots of river quality ------------
      subroutine load statistics
     &(JPART,JDET,XM,XL,XU,D90p,D90L,D90U,D95p,D95L,D95U,D99p,D99L,D99U)
*     --------------------------------------------------------------------------
*     XM  ... is the mean load
*     XL   .. is the lower confidence limit on the mean load
*     XU   .. is the upper confidence limit on the mean load
*     D90 ... is the calculated value of the 90-percentile load
*     D90L... is the lower confidence limit on the percentile load
*     D90U... is the upper confidence limit on the percentile load
*     DCP ... is the calculated value of the 95-percentile load
*     D95L... is the lower confidence limit on the percentile load
*     D95U... is the upper confidence limit on the percentile load
*     D99  .. is the calculated value of the 99-percentile load load
*     D99L... is the lower confidence limit on this percentile load
*     D99U... is the upper confidence limit on this percentile load
*     --------------------------------------------------------------------------
      include 'COMMON DATA.FOR'

      D90 = 0.0
      D90L = 0.0
      D90U = 0.0
      D95 = 0.0
      D95L = 0.0
      D95U = 0.0
      D99 = 0.0
      D99L = 0.0
      D99U = 0.0
      XM = 0.0 ! mean load
      XL = 0.0
      XU = 0.0
      D90P = 0.0 ! calculated 90-percentile
      D95P = 0.0 ! calculated 95-percentile
      D99P = 0.0 ! calculated 99-percentile

*     set A to the calculated mean load ------------------------------------------
      A = amax1 ( 0.0, Xload (JDET,1,i13) )
      S = amax1 ( 0.0, Xload (JDET,2,i13) )
*     compute the extra values for partitioned metals ----------------------------
      if ( detype (JDET) .ge. 900 ) then ! dissolved and solid metal
      if ( JPART .eq. 2 ) then ! dissolved metal
      A = amax1 ( 0.0, Xload1 (JDET,1,i13) )
      S = amax1 ( 0.0, Xload1 (JDET,2,i13) )
      endif ! dissolved metal
      if ( JPART .eq. 3 ) then ! solid metal
      A = amax1 ( 0.0, Xload2 (JDET,1,i13) )
      S = amax1 ( 0.0, Xload2 (JDET,2,i13) )
      endif ! solid metal
      endif ! dissolved and solid metal
*     --------------------------------------------------------------------------

*     check for a zero calculated mean load-------------------------------------
      if ( A .lt. 1.0E-08 .or. S .lt. 1.0e-10 ) then	
      S = 0.0
*     initialise all the percentiles of load to the calculated mean ------------
      D90 = A
      D90L = A
      D90U = A
      D95 = A
      D95L = A
      D95U = A
      D99 = A
      D99L = A
      D99U = A
      XM = A
      XL = A
      XU = A
      return
      endif

*     start the calculations for the percentiles -------------------------------
*     get the calculated values ------------------------------------------------
      D90P = Xload (JDET,4,i13)
      D95P = Xload (JDET,3,i13)
      D99P = Xload (JDET,5,i13)
      if ( detype (JDET) .ge. 900 ) then ! dissolved and solid metal
      if ( JPART .eq. 2 ) then ! dissolved metal
      D90P = Xload1 (JDET,3,i13) ! dissolved metal
      D95P = Xload1 (JDET,3,i13) ! dissolved metal
      D99P = Xload1 (JDET,3,i13) ! dissolved metal
      endif ! dissolved metal
      if ( JPART .eq. 3 ) then ! solid metal
      D90P = Xload2 (JDET,3,i13) ! solid metal
      D95P = Xload2 (JDET,3,i13) ! solid metal
      D99P = Xload2 (JDET,3,i13) ! solid metal
      endif ! solid metal
      endif ! dissolved and solid metal 
      
      XM = A ! set the mean
      t90 = errx ( 0.90 ) ! get the standard normal deviate
      t95 = errx ( 0.95 ) ! get the standard normal deviate
      t99 = errx ( 0.99 ) ! get the standard normal deviate

*     --------------------------------------------------------------------------
*     start the calculation for the mean ---------------------------------------
*     calculate the standard error of estimate of the mean ---------------------
      xqualn = amax1 (0.05, QUALN(JDET))
      SEM = S / SQRoot(107334,xqualn)
*     calculate the 95% confidence limits about the mean -----------------------
      XL = amax1 (0.0, (A - t95 * SEM) )
      XU = amax1 (0.0, (A + t95 * SEM) )
*     --------------------------------------------------------------------------

      JQDIST = 2 
      if ( QTYPE(JDET) .eq. 3 ) JQDIST = 1

*     ==========================================================================
*     log-normal distribution --------------------------------------------------
      if ( JQDIST .eq. 2 .or. JQDIST .eq. 3 ) then
      GM = alog ( (A*A) / SQRoot(1000,A*A+S*S) )
      GS = SQRoot(1075, alog ( 1.0 + (S*S) / (A*A) ) )
      if ( QTYPE (Jdet) .ne. 3 .and. QTYPE (JDET) .ne. 5 ) then
*     calculate the 90-percentile assuming a Log-normal Distribution -----------
      D90 = exp ( GM + t90 * GS )
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t90, -1.0)
      TU = tshift ( QUALN(JDET), t90,  1.0)
      
*     calculate the Optimistic Confidence Limit --------------------------------
      D90L = amax1 (0.0, D90P - (D90 - exp ( GM + TL * GS)) )
      D90U = amax1 (0.0, D90P + (exp ( GM + TU * GS) - D90) )
*     calculate the 95-percentile assuming a Log-normal Distribution -----------
      D95 = exp ( GM + t95 * GS )
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t95, -1.0)
      TU = tshift ( QUALN(JDET), t95,  1.0)
*     calculate the Optimistic Confidence Limit --------------------------------
      D95L = amax1 (0.0, D95P - (D95 - exp ( GM + TL * GS)) )
*     calculate the Pessimistic Confidence Limit -------------------------------
      D95U = amax1 (0.0, D95P + (exp ( GM + TU * GS) - D95) )
      D99 = exp ( GM + t99 * GS )
      TL = tshift ( QUALN(JDET), t99, -1.0)
      TU = tshift ( QUALN(JDET), t99,  1.0)
      D99L = amax1 (0.0, D99P - (D99 - exp ( GM + TL * GS)) )
      D99U = amax1 (0.0, D99P + (exp ( GM + TU * GS) - D99) )
      endif
*     5-percentile -------------------------------------------------------------
      if ( QTYPE (Jdet) .eq. 3 .or. QTYPE (JDET) .eq. 5 ) then
      D90 = exp ( GM - t90 * GS )
      TL = tshift ( QUALN(JDET), t90,  1.0)
      TU = tshift ( QUALN(JDET), t90, -1.0)
      D90L = amax1 (0.0, D90P - (D90 - exp ( GM - TL * GS)) )
      D90U = amax1 (0.0, D90P + (exp ( GM - TU * GS) - D90) )
      D95 = exp ( GM - t95 * GS )
      TL = tshift ( QUALN(JDET), t95,  1.0)
      TU = tshift ( QUALN(JDET), t95, -1.0)
      D95L = amax1 (0.0, D95P - (D95 - exp ( GM - TL * GS)) )
      D95U = amax1 (0.0, D95P + (exp ( GM - TU * GS) - D95) )
      D99 = exp ( GM - t99 * GS )
      TL = tshift ( QUALN(JDET), t99,  1.0)
      TU = tshift ( QUALN(JDET), t99, -1.0)
      D99L = amax1 (0.0, D99P - (D99 - exp ( GM - TL * GS)) )
      D99U = amax1 (0.0, D99P + (exp ( GM - TU * GS) - D99) )
      endif
      endif
*     ==========================================================================

*     ==========================================================================
*     normal distributions -----------------------------------------------------
      if ( JQDIST .eq. 1 ) then
      GM=A
      GS=S
      if ( QTYPE(Jdet) .ne. 3 .and. QTYPE(JDET) .ne. 5 ) then
*     calculate the 90-percentile ----------------------------------------------
      D90 = GM + t90 * GS
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t90, -1.0 )
      TU = tshift ( QUALN(JDET), t90,  1.0 )

*     calculate the Optimistic and Pessimistic Confidence Limits ---------------
      D90L = amax1 (0.0,( GM+TL*GS-D90+D90P) )
      D90U = amax1 (0.0,( GM+TU*GS-D90+D90P) )
*     calculate the 95-percentile ----------------------------------------------
      D95 = GM + t95 * GS
*     calculate confidence limits. Calculate standard normal deviates ----------
      TL = tshift ( QUALN(JDET), t95, -1.0 )
      TU = tshift ( QUALN(JDET), t95,  1.0 )
*     calculate the Optimistic and Pessimistic Confidence Limits ---------------
      D95L = amax1 (0.0,( GM+TL*GS-D95+D95P) )
      D95U = amax1 (0.0,( GM+TU*GS-D95+D95P) )
      D99 = GM + t99 * GS
      TL = tshift ( QUALN(JDET), t99, -1.0 )
      TU = tshift ( QUALN(JDET), t99,  1.0 )
      D99L = amax1 (0.0,( GM+TL*GS-D99+D99P) )
      D99U = amax1 (0.0,( GM+TU*GS-D99+D99P) )
      endif
*     calculate the 5-percentile ----------------------------------------------
      if ( QTYPE (Jdet) .eq. 3 .or. QTYPE (JDET) .eq. 5 ) then
      D90 = GM - t90 * GS
      TL = tshift ( QUALN(JDET), t90,  1.0 )
      TU = tshift ( QUALN(JDET), t90, -1.0 )
      D90L = amax1 (0.0,( GM-TL*GS-D90+D90P) )
      D90U = amax1 (0.0,( GM-TU*GS-D90+D90P) )
      D95 = GM - t95 * GS
      TL = tshift ( QUALN(JDET), t95,  1.0 )
      TU = tshift ( QUALN(JDET), t95, -1.0 )
      D95L = amax1 (0.0,( GM-TL*GS-D95+D95P) )
      D95U = amax1 (0.0,( GM-TU*GS-D95+D95P) )
      D99 = GM - t99 * GS
      TL = tshift ( QUALN(JDET), t99,  1.0 )
      TU = tshift ( QUALN(JDET), t99, -1.0 )
      D99L = amax1 (0.0,( GM-TL*GS-D99+D99P) )
      D99U = amax1 (0.0,( GM-TU*GS-D99+D99P) )
      endif
      endif
*     ==========================================================================
      
      return
      end






*     compute the observed mean and percentiles at a monitoring point ----------
      subroutine compute the mean and percentiles at a monitoring point
      include 'COMMON DATA.FOR'

*     storage for writing out confidence limits for the mean and the percentiles
      character *13 SET1,SET2,SET3,SET4

      if ( IPRINT .ne. 1 ) then
      if ( nobigout .le. 0 ) then ! --------------------------------------------
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      write(01,9015)uname(feeture)
 9015 format(
     &'Observed river quality at this monitoring point ... ',a35/ ! -------- OUT
     &110('='))
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      if ( ical .eq. 3 ) then ! ------------------------------------------------
      write(01,9025)uname(feeture)
 9025 format(/110('=')/
     &'Observed river quality at this monitoring point ... ',a35/ ! -------- OUT
     &110('='))
      endif ! ical .eq. 3 ) ----------------------------------------------------
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
      endif ! if ( IPRINT .ne. 1 )


*     identify the quality data-set. This is number IQ -------------------------
      IQ = JQ(feeture)

*     loop on all the determinands ---------------------------------------------
      do 1 JP = 1, NDET
*     skip any excluded determinands -------------------------------------------
      if ( QTYPE (JP) .eq. 4 ) goto 1
*     initialise the summary statistics ----------------------------------------
      A = 0.0
      S = 0.0
      C95 = 0.0
      C90 = 0.0
      C99 = 0.0
      X95(JP) = 0.0
      X90(JP) = 0.0
      X99(JP) = 0.0
      XA(JP) = 0.0
      XL = 0.0
      XU = 0.0

*     initialise the confidence limits ----------------------------------------
      dcpl95 = 0.0
      dcpu95 = 0.0
      dcpl90 = 0.0
      dcpu90 = 0.0
      dcpl99 = 0.0
      dcpu99 = 0.0

*     obtain the normal deviates for each percentile ---------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )

*     get the number of samples ------------------------------------------------
      SAM = float(QNUM(IQ,JP))

*     prepare to calculate confidence limits -----------------------------------
*     intialise the store for the results --------------------------------------
      SET1 = '             '
      SET2 = '             '
      SET3 = '             '
      SET4 = '             '
      SET5 = '             '

*     first look at log-normal and log-normal distributions --------------------
*     non-parametric distributions are dealt with later ------------------------
      if ( PDRC (IQ,JP) .lt. 4 ) then

*     check for zero mean ------------------------------------------------------
      if ( quolity data(IQ,JP,1) .lt. 1.0E-08 ) then
      quolity data(IQ,JP,2) = 0.0
      goto 1
      endif

*     set the mean and initialise the percentiles ------------------------------
      A = amax1 ( 0.0, quolity data(IQ,JP,1) )
      C90 = A
      C95 = A
      C99 = A
      X90(JP) = A
      X95(JP) = A
      X99(JP) = A
      XA(JP) = A
      XL = A
      XU = A
      dcpl90 = A
      dcpl95 = A
      dcpl99 = A
      dcpu90 = A
      dcpu95 = A
      dcpu99 = A

      if ( nobigout .le. 0 ) then ! ===========================================
      if ( PDRC (IQ,JP) .eq. 0 ) then ! ---------------------------------------
      if ( IPRINT .ne. 1 ) then
      call sort format 1 (A)
      write(01,7500)DNAME(JP),valchars10,UNITS(JP),SET1
 7500 format(A11,36X,'Constant mean =',a10,1X,A4,4x,a13)
      goto 1
      endif
      endif ! if ( PDRC (IQ,JP) .eq. 0 ) ---------------------------------------
      endif ! if ( nobigout .le. 0 ) ===========================================

      
*     the log-normal distribution LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      if ( PDRC (IQ,JP) .gt. 1 ) then ! log-normal distribution LLLLLLLLLLLLLLLL
*     check for zero standard deviation ----------------------------------------
      if ( quolity data (IQ,JP,2) .lt. 1.0E-08 ) goto 1
*     get the standard deviation -----------------------------------------------
      S = quolity data(IQ,JP,2)
*     compute the logged variables ---------------------------------------------
      GM = alog ((A*A)/SQRoot(1088,A*A+S*S))
      GS = SQRoot(1089,alog(1.+(S*S)/(A*A)))


*     SECTION FOR HIGH LOG-NORMAL PERCENTILES ----------------------------------
*     check for low percentile standards ---------------------------------------
      if ( QTYPE (jp) .ne. 3 .and. QTYPE (jp) .ne. 5 ) then
*     compute and store the log-normal high percentiles ------------------------
      C95 = exp ( GM + t95 * GS )
      C90 = exp ( GM + t90 * GS )
      C99 = exp ( GM + t99 * GS )
      X90(JP) = C90
      X95(JP) = C95
      X99(JP) = C99
      XA(JP) = A

*     factor for high percentiles ----------------------------------------------
      FF = 1.0
      to95 = tshift (SAM,t95,-FF)
      tp95 = tshift (SAM,t95, FF)
      to90 = tshift (SAM,t90,-FF)
      tp90 = tshift (SAM,t90, FF)
      to99 = tshift (SAM,t99,-FF)
      tp99 = tshift (SAM,t99, FF)

*     optimistic Confidence Limits on high log-normal percentiles --------------
      dcpl90 = amax1 (0.0, exp (GM+FF*to90*GS))
      dcpl95 = amax1 (0.0, exp (GM+FF*to95*GS))
      dcpl99 = amax1 (0.0, exp (GM+FF*to99*GS))

*     pessimistic Confidence Limits on high log-normal percentiles -------------
      dcpu90 = amax1 (0.0, exp (GM+FF*tp90*GS))
      dcpu95 = amax1 (0.0, exp (GM+FF*tp95*GS))
      dcpu99 = amax1 (0.0, exp (GM+FF*tp99*GS))

*     standard error in the mean -----------------------------------------------
      SEM=S/SQRoot(1087,SAM)

*     confidence limits on the mean --------------------------------------------
      XL=amax1 (0.0,(A-FF*t95*SEM))
      XU=amax1 (0.0,(A+FF*t95*SEM))

*     arrange to print out the confidence limits -------------------------------
      call assem(XL,XU,SET1)
      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)

      if ( nobigout .le. 0 ) then
      if ( IPRINT .ne. 1 ) then
      if ( ical13 .eq. 0 .or. ical. eq. 3 ) then
      call sort format 2 (A,C90)
      

      if ( DETYPE (JP) .lt. 900 ) then ! --------------------------- total metal
      write(01,4500)DNAME(JP),valchars10,UNITS(JP),SET1
 4500 format(A11,45X,'Mean =',a10,1X,A4,4x,a13)
      endif

      if ( DETYPE (JP) .eq. 900 ) then ! ----------------------- dissolved metal
      write(01,5500)DNAME(JP),valchars10,UNITS(JP),SET1
 5500 format(a11,'(dissolved)',34x,'Mean =',a10,1X,A4,4x,a13)
      endif ! if ( DETYPE (JP) .eq. 909 ) ---------------------- dissolved metal

      if ( DETYPE (JP) .eq. 900 ) then ! --------------------------- total metal
      write(01,5501)DNAME(JP),valchars10,UNITS(JP),SET1
 5501 format(a11,'(total)',38x,'Mean =',a10,1X,A4,4x,a13) ! ------- 
      endif ! if ( DETYPE (JP) .eq. 900 ) -------------------------- total metal
      
      write(01,4501)valchars11,UNITS(JP),SET3
 4501 format(36X,'Log-normal 90-percentile =',a10,1X,A4,4x,a13)
      call sort format 2 (C95,C99)
      write(01,4502)valchars10,UNITS(JP),SET2
 4502 format(36X,'Log-normal 95-percentile =',a10,1X,A4,4x,a13)
      write(01,4503)valchars11,UNITS(JP),SET4
 4503 format(36X,'Log-normal 99-percentile =',a10,1X,A4,4x,a13/77('='))
      endif
      endif
      endif

*     SECTION ON LOW PERCENTILES -----------------------------------------------
      else ! if ( QTYPE (jp) .ne. 3 .and. QTYPE (jp) .ne. 5 )

*     compute and store the log-normal low percentiles -------------------------
      C95 = amax1 ( 0.0, exp ( GM - t95 * GS ))
      C90 = amax1 ( 0.0, exp ( GM - t90 * GS ))
      C99 = amax1 ( 0.0, exp ( GM - t99 * GS ))
      X95(JP) = C95
      X90(JP) = C90
      X99(JP) = C99
      XA(JP) = A

      FF = -1.0

      to95 = tshift (SAM,t95,-FF)
      tp95 = tshift (SAM,t95, FF)
      to90 = tshift (SAM,t90,-FF)
      tp90 = tshift (SAM,t90, FF)
      to99 = tshift (SAM,t99,-FF)
      tp99 = tshift (SAM,t99, FF)

*     optimistic Confidence Limits on low log-normal percentiles ---------------
      dcpl90 = amax1  (0.0, exp (GM+FF*to90*GS))
      dcpl95 = amax1  (0.0, exp (GM+FF*to95*GS))
      dcpl99 = amax1  (0.0, exp (GM+FF*to99*GS))

*     pessimistic Confidence Limits on low log-normal percentiles --------------
      dcpu90 = amax1  (0.0, exp (GM+FF*tp90*GS))
      dcpu95 = amax1  (0.0, exp (GM+FF*tp95*GS))
      dcpu99 = amax1  (0.0, exp (GM+FF*tp99*GS))

      SEM=S/SQRoot(1086,SAM)

      XL=amax1 (0.0,(A-FF*t95*SEM))
      XU=amax1 (0.0,(A+FF*t95*SEM))
      if ( nobigout .le. 0 ) then
      call assem(XL,XU,SET1)
      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)

      if ( IPRINT .ne. 1 ) then
      if ( ical13 .eq. 0 ) then
      call sort format 2 (A,C90)
      write(01,4000)DNAME(JP),valchars10,UNITS(JP),SET1
      write(01,4001)valchars11,UNITS(JP),SET3
      call sort format 2 (C95,C99)
      write(01,4002)valchars10,UNITS(JP),SET2
      write(01,4003)valchars11,UNITS(JP),SET4
 4000 format(A11,45X,'Mean =',a10,1X,A4,4x,a13)
 4001 format(36X,'Log-normal 10-percentile =',a10,1X,A4,4x,a13)
 4002 format(36X,' Log-normal 5-percentile =',a10,1X,A4,4x,a13)
 4003 format(36X,' Log-normal 1-percentile =',a10,1X,A4,4x,a13/77('='))
      endif
      endif
      endif

*     end of section on high and low percentiles -------------------------------
      endif

      endif ! if ( PDRC (IQ,JP) .gt. 1 ) log-normal distribution LLLLLLLLLLLLLLL
*     end of section on log-normal distributions LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL



*     section for normal distribution NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
      if ( PDRC (IQ,JP) .eq. 1 ) then ! normal distribution NNNNNNNNNNNNNNNNNNNN
*     check for non-zero data --------------------------------------------------
      if ( quolity data(IQ,JP,1) .lt. 1.0E-08 ) goto 1

*     set the mean -------------------------------------------------------------
      A = amax1 ( 0.0, quolity data(IQ,JP,1))
      C95 = A
      C90 = A
      C99 = A
      X90(JP) = A
      X95(JP) = A
      X99(JP) = A
      XA(JP) = A

      if ( quolity data (IQ,JP,2) .lt. 1.0E-08 ) goto 1

*     get the standard deviation -----------------------------------------------
      S = quolity data(IQ,JP,2)

*     get the number of samples ------------------------------------------------
      SAM = QNUM(IQ,JP)

*     set the variables --------------------------------------------------------
      GM = A
      GS = S

*     section on high normal percentiles ---------------------------------------
      if ( QTYPE(jp) .ne. 3 .and. QTYPE(jp) .ne.5 ) then

*     compute the normal percentiles -------------------------------------------
      C95 = A + t95 * S
      C90 = A + t90 * S
      C99 = A + t99 * S
      X95(JP) = C95
      X90(JP) = C90
      X99(JP) = C99
      XA(JP) = A

      FF = 1.0

      to95 = tshift (SAM,t95,-FF)
      tp95 = tshift (SAM,t95, FF)
      to90 = tshift (SAM,t90,-FF)
      tp90 = tshift (SAM,t90, FF)
      to99 = tshift (SAM,t99,-FF)
      tp99 = tshift (SAM,t99, FF)

*     Normal Optimistic Confidence Limits on high normal percentiles -----------
      dcpl90 = amax1 (0.0,(GM+FF*to90*GS))
      dcpl95 = amax1 (0.0,(GM+FF*to95*GS))
      dcpl99 = amax1 (0.0,(GM+FF*to99*GS))

*     Normal Pessimistic Confidence Limits on high normal percentiles ----------
      dcpu90 = amax1 (0.0,(GM+FF*tp90*GS))
      dcpu95 = amax1 (0.0,(GM+FF*tp95*GS))
      dcpu99 = amax1 (0.0,(GM+FF*tp99*GS))

      SEM=S/SQRoot(1090,SAM)
      XL=amax1 (0.0,(A-FF*t95*SEM))
      XU=amax1 (0.0,(A+FF*t95*SEM))

      if ( nobigout .le. 0 ) then
      call assem(XL,XU,SET1)
      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)

      if ( ical13 .eq. 0 ) then
      if ( IPRINT .ne. 1 ) then
      call sort format 2 (A,C90)
      write(01,613)DNAME(JP),valchars10,UNITS(JP),SET1,
     &valchars11,UNITS(JP),SET3
  613 format(A11,45X,'Mean =',a10,1X,A4,4x,a13/
     &36X,'    Normal 90-percentile =',a10,1X,A4,4x,a13)
      call sort format 2 (C95,C99)
      write(01,623)valchars10,UNITS(JP),SET2,
     &valchars11,UNITS(JP),SET4
  623 format(36X,'    Normal 95-percentile =',a10,1X,A4,4x,a13/
     &36X,'    Normal 99-percentile =',a10,1X,A4,4x,a13/77('='))
      endif
      endif
      endif

*     section on low normal percentiles ----------------------------------------
      else ! if ( QTYPE (jp) .ne. 3 .and. QTYPE (jp) .ne.5 )

*     compute the low normal percentiles ---------------------------------------
      C95 = amax1  ( 0.0, A - t95 * S )
      C90 = amax1  ( 0.0, A - t90 * S )
      C99 = amax1  ( 0.0, A - t99 * S )
      X95(JP) = C95
      X90(JP) = C90
      X99(JP) = C99
      XA(JP) = A

      FF = -1.0

      to95 = tshift (SAM,t95,-FF)
      tp95 = tshift (SAM,t95, FF)
      to90 = tshift (SAM,t90,-FF)
      tp90 = tshift (SAM,t90, FF)
      to99 = tshift (SAM,t99,-FF)
      tp99 = tshift (SAM,t99, FF)

*     Normal Optimistic Confidence Limits on low percentiles -------------------
      dcpl90 = amax1 (0.0,(GM+FF*to90*GS))
      dcpl95 = amax1 (0.0,(GM+FF*to95*GS))
      dcpl99 = amax1 (0.0,(GM+FF*to99*GS))

*     Normal Pessimistic Confidence Limits on low percentiles ------------------
      dcpu90 = amax1 (0.0,(GM+FF*tp90*GS))
      dcpu95 = amax1 (0.0,(GM+FF*tp95*GS))
      dcpu99 = amax1 (0.0,(GM+FF*tp99*GS))

      SEM=S/SQRoot(1091,SAM) ! standard error
      XL=amax1 (0.0,(A-FF*t95*SEM)) ! lower confidence limit
      XU=amax1 (0.0,(A+FF*t95*SEM)) ! upper confidence limit

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      call assem(XL,XU,SET1)
      call assem(dcpl90,dcpu90,SET3)
      call assem(dcpl95,dcpu95,SET2)
      call assem(dcpl99,dcpu99,SET4)
      if ( IPRINT .ne. 1 ) then
      call sort format 2 (A,C90)
      write(01,6513)DNAME(JP),valchars10,UNITS(JP),SET1,
     &valchars11,UNITS(JP),SET3
 6513 format(A11,45X,'Mean =',a10,1X,A4,4x,a13/
     &36X,'    Normal 10-percentile =',a10,1X,A4,4x,a13)
      call sort format 2 (C95,C99)
      write(01,6523)valchars10,UNITS(JP),SET2,
     &valchars11,UNITS(JP),SET4
 6523 format(36X,'     Normal 5-percentile =',a10,1X,A4,4x,a13/
     &36X,'     Normal 1-percentile =',a10,1X,A4,4x,a13/77('='))
      endif ! if ( IPRINT .ne. 1 ) 
      endif ! if ( nobigout .le. 0 )

*     end of section on high and low percentiles -------------------------------
      endif ! if ( QTYPE (jp) .ne. 3 .and. QTYPE (jp) .ne.5 )
      endif ! end of section on normal distributions ---------------------------
      endif ! end of sections on both normal and log-normal distributions-------
      
      XAl(JP) = XL
      XAu(JP) = XU 
      X90l(JP) = dcpl90
      X90u(JP) = dcpu90 
      X95l(JP) = dcpl95
      X95u(JP) = dcpu95 
      X99l(JP) = dcpl99
      X99u(JP) = dcpu99 

*     now deal with non-parametric distributions -------------------------------
      if ( PDRC (IQ,JP) .eq. 4 .or. PDRC (IQ,JP) .eq. 9 ) then

*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue

      if ( nobigout .le. 0 ) then
      write(01,1965) flnamesmall(2,icod)
 1965 format('The observed quality data follow a non-para',
     &'metric distribution ...'/'File: ',a64/77('='))
      endif ! if ( nobigout .le. 0 ) then

      if ( IPRINT .ne. 1 ) then

*     set the mean .....  this was calculated earlier --------------------------
      A = amax1 ( 0.0, quolity data(IQ,JP,1) )
      S = quolity data(IQ,JP,2) ! get the standard deviation -------------------
      SAM = QNUM(IQ,JP) ! get the number of samples ----------------------------

*     get the 95-percentile (or the 5 percentile) ------------------------------
*     this was set in "get non parametric file for river quality" (READ.FOR) ---
      C95 = quolity data(IQ,JP,3)
      X95(JP) = C95 ! store the 95-percentile
      XA(JP) = A ! store the mean

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      if ( QTYPE (jp) .ne. 3 .or. QTYPE (jp) .ne. 5 ) then
      call sort format 2 (A,C95)
      write(01,87)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP)
   87 format(A11,45X,'Mean =',a10,1X,A4/
     &32X,'Non-parametric 95-percentile =',a10,1X,A4/77('='))
      else
      write(01,54)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP)
   54 format(A11,45X,'Mean =',a10,1X,A4/
     &32X,' Non-parametric 5-percentile =',a10,1X,A4/77('-'))
      endif ! if ( QTYPE (jp) .ne. 3 .or. QTYPE (jp) .ne. 5 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( IPRINT .ne. 1 ) then
      endif ! end of non-parametric distributions ------------------------------
 1974 continue

*     now deal with the monthly distributions ----------------------------------
      if ( PDRC (IQ,JP) .eq. 5 ) then
*     identify the file with the monthly data ----------------------------------
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, JP+1 ) .eq. IQ ) goto 2970
 2969 continue
      goto 2794
 2970 continue

      if ( nobigout .le. 0 ) then
      write(01,2965) flnamesmall(2,icod)
 2965 format('The observed quality data follow monthly',
     &'sets of data ...'/'File: ',a64/77('='))
      endif ! if ( nobigout .le. 0 )

      if ( IPRINT .ne. 1 ) then
*     set the mean .....  this was calculated earlier --------------------------
      A = amax1 ( 0.0, quolity data(IQ,JP,1) )
      S = quolity data(IQ,JP,2) ! get the standard deviation -------------------
      SAM = QNUM(IQ,JP) ! get the number of samples ----------------------------

*     get the 95-percentile (or the 5 percentile) ------------------------------
*     this was set in "get non parametric file for river quality" (READ.FOR) ---
      C95 = quolity data(IQ,JP,3)
      X95(JP) = C95 ! store the 95-percentile
      XA(JP) = A ! store the mean

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      if ( QTYPE (jp) .ne. 3 .or. QTYPE (jp) .ne. 5 ) then
      call sort format 2 (A,C95)
      write(01,187)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP)
  187 format(A11,45X,'Mean =',a10,1X,A4/
     &40x,'95-percentile =',a10,1X,A4/77('='))
      else
      write(01,154)DNAME(JP),valchars10,UNITS(JP),valchars11,UNITS(JP) ! --- OUT
  154 format(A11,45X,'Mean =',a10,1X,A4/
     &49X,'5-percentile =',a10,1X,A4/77('-'))
      endif ! if ( QTYPE (jp) .ne. 3 .or. QTYPE (jp) .ne. 5 )
      endif ! if ( nobigout .le. 0 )
      endif ! if ( IPRINT .ne. 1 ) 
      endif ! end of non-parametric distributions
 2794 continue

      if ( MONQ .gt. 1 ) call write shots for river quality 

    1 continue ! end of loop on determinands -----------------------------------

      return
      end








      subroutine compute the mean and percentiles at a plotting point
      include 'COMMON DATA.FOR'

*     identify the quality data-set. This is number IQ -------------------------
      IQ = JQ(feeture)

*     loop on all the determinands ---------------------------------------------
      do 1 JP = 1, NDET

*     skip any excluded determinands -------------------------------------------
      if ( QTYPE (JP) .eq. 4 ) goto 1

*     initialise the summary statistics ----------------------------------------
      A = 0.0
      S = 0.0
      C95 = 0.0
      C90 = 0.0
      C99 = 0.0
      Xp95(JP) = 0.0
      Xp90(JP) = 0.0
      Xp99(JP) = 0.0

*     obtain the deviates ------------------------------------------------------
      t90 = errx ( 0.90 )
      t95 = errx ( 0.95 )
      t99 = errx ( 0.99 )

*     first look at log-normal, normal etc -------------------------------------
*     non-parametric distributions are dealt with later ------------------------
      if ( IQ .gt. 0 ) then
      if ( PDRC (IQ,JP) .lt. 4 ) then

      if ( PDRC (IQ,JP) .gt. 1 ) then ! log-normal -----------------------------

*     check for negative data --------------------------------------------------
      if ( quolity data(IQ,JP,1) .lt. 1.0E-08 ) then
      quolity data(IQ,JP,2) = 0.0
      goto 1
      endif

      A = amax1 ( 0.0, quolity data(IQ,JP,1) ) ! set the mean ------------------
      C95 = A
      C90 = A
      C99 = A
      Xp90(JP) = A
      Xp95(JP) = A
      Xp99(JP) = A
      XpA(JP) = A
      if ( quolity data (IQ,JP,2) .lt. 1.0E-08 ) goto 1

      S = quolity data(IQ,JP,2) ! get the standard deviation -------------------
      SAM = QNUM(IQ,JP) ! get the number of samples ----------------------------

*     compute the logged variables ---------------------------------------------
      GM = alog ( (A*A) / SQRoot(1092,A*A+S*S) )
      GS = SQRoot(1093, alog ( 1.0 + (S*S) / (A*A) ) )

      if ( detype (jp) .ne. 104 ) then
*     compute the log-normal percentiles ---------------------------------------
      C95 = exp ( GM + t95 * GS )
      C90 = exp ( GM + t90 * GS )
      C99 = exp ( GM + t99 * GS )
      Xp95(JP) = C95
      Xp90(JP) = C90
      Xp99(JP) = C99
      XpA(JP) = A
      else
*     compute the low percentiles ----------------------------------------------
      C95 = amax1 ( 0.0, exp ( GM - t95 * GS ))
      C90 = amax1 ( 0.0, exp ( GM - t90 * GS ))
      C99 = amax1 ( 0.0, exp ( GM - t99 * GS ))
      Xp95(JP) = C95
      Xp90(JP) = C90
      Xp99(JP) = C99
      XpA(JP) = A

      endif
      endif ! log-normal -------------------------------------------------------

      if ( PDRC (IQ,JP) .eq. 1 ) then ! the normal distribution ----------------

*     check for non-zero data --------------------------------------------------
      if ( quolity data(IQ,JP,1) .lt. 1.0E-08 ) goto 1

*     set the mean -------------------------------------------------------------
      A = amax1 ( 0.0, quolity data(IQ,JP,1))
      C95 = A
      C90 = A
      C99 = A
      Xp90(JP) = A
      Xp95(JP) = A
      Xp99(JP) = A
      XpA(JP) = A
      if ( quolity data (IQ,JP,2) .lt. 1.0E-08 ) goto 1

*     get the standard deviation -----------------------------------------------
      S = quolity data(IQ,JP,2)

*     get the number of samples ------------------------------------------------
      SAM = QNUM(IQ,JP)

*     compute the logged variables ---------------------------------------------
      GM = A
      GS = S
      if ( detype (jp) .ne. 104 ) then

*     compute the normal percentiles -------------------------------------------
      C95 = A + t95 * S
      C90 = A + t90 * S
      C99 = A + t99 * S
      Xp95(JP) = C95
      Xp90(JP) = C90
      Xp99(JP) = C99
      XpA(JP) = A
      else

*     compute the low normal percentiles ---------------------------------------
      C95 = amax1 ( 0.0, A - t95 * S )
      C90 = amax1 ( 0.0, A - t90 * S )
      C99 = amax1 ( 0.0, A - t99 * S )
      Xp95(JP) = C95
      Xp90(JP) = C90
      Xp99(JP) = C99
      XpA(JP) = A

      endif
      endif
      endif

*     now deal with non-parametric distributions -------------------------------
      if ( PDRC(IQ,JP) .eq. 4 .or. PDRC(IQ,JP) .eq. 9 ) then

*     identify the file with the non-parametric data ---------------------------
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 1970
 1969 continue
      goto 1974
 1970 continue

      if ( IPRINT .ne. 1 ) then
      A = amax1 ( 0.0, quolity data(IQ,JP,1) ) ! get the mean -------------------
      S = quolity data(IQ,JP,2) ! get the standard deviation --------------------
      SAM = QNUM(IQ,JP) ! get the number of samples -----------------------------
      C95 = quolity data(IQ,JP,3) ! get the 95-percentile (or the 5 percentile) -
*     this was set in "get non parametric file for river quality" (READ.FOR) ----
      Xp95(JP) = C95
      XpA(JP) = A
      endif ! if ( IPRINT .ne. 1 )

 1974 continue ! end of non-parametric distributions ----------------------------

      endif ! if ( PDRC(IQ,JP) .eq. 4 .or. PDRC(IQ,JP) .eq. 9 )
      endif ! if ( IQ .gt. 0 )
    1 continue
      
      return
      end




*     calculate the percentiles from mean and standard deviation ---------------
*     used to compute summary statistics at monitoring points ------------------
*     these data are used as targets for gap filling ---------------------------
      subroutine calculate percentiles from mean and standard deviation
     &(JDET,A,S,Q)
      include 'COMMON DATA.FOR'

      A = 0.0 ! initialise the mean
      S = 0.0 ! initialise the standard deviation
      Q = 0.0 ! initialise the 95-percentile

*     check for non-zero data --------------------------------------------------
      if ( quolity data(IQ,JDET,1) .lt. 1.0E-08) then
      quolity data(IQ,JDET,2) = 0.0
      return
      endif ! check for non-zero data ------------------------------------------

      IDIST = PDRC(IQ,JDET) ! identify the statistical distribution ------------
      
      if ( idist .eq. 0 ) then ! contant quality -------------------------------
      A = amax1 ( 0.0, quolity data(IQ,JDET,1) ) ! set the mean
      S = 0.0 ! set the standard deviation to zero
      Q = A ! set the 95-percentile to the mean
      return
      endif ! contant quality --------------------------------------------------

*     compute the t-statistic for the 95-percentile ----------------------------
      t95 = errx ( 0.95 )

      if ( idist .eq. 1 ) then ! normal distribution ---------------------------
      A = amax1 ( 0.0, quolity data(IQ,JDET,1) ) ! set the mean
      S = 0.0 ! initialise the standard deviation
      Q = A ! initialise the 95-percentile
      if (quolity data(IQ,JDET,2) .lt. 1.0E-08) return
      S = quolity data(IQ,JDET,2) ! set the standard deviation
      GM = A
      GS = S
      Q = ( GM + t95 * GS ) ! set the 95-percentile
      if ( QTYPE (JDET) .eq. 3 .or. QTYPE (JDET) .eq. 5 ) then
      Q = amax1 (0.0, A - t95 * S ) ! set the 5-percentile
      endif ! set the 5-percentile 
      return
      endif ! normal distribution ----------------------------------------------

      if ( idist .eq. 2 .or. idist .eq. 3 ) then ! log-normal distribution -----
      A = amax1 ( 0.0, quolity data(IQ,JDET,1) ) ! set the mean
      S = 0.0 ! initialise the standard deviation
      Q = A ! initialise the 95-percentile
      if (quolity data(IQ,JDET,2) .lt. 1.0E-08) return
      S = quolity data(IQ,JDET,2) ! set the standard deviation
*     compute mean and standard deviation in the log domain
      GM = alog ( (A*A) / SQRoot(1095,A*A+S*S) )
      GS = SQRoot(1096, alog ( 1.0 + (S*S) / (A*A) ) )
      Q = exp ( GM + t95 * GS ) ! set the 95-percentile
*     for low percentiles for things like dissolved oxygen ---------------------
      if ( QTYPE (JDET) .eq. 3 .or. QTYPE (JDET) .eq. 5 ) then
      Q = amax1 (0.0, exp ( GM - t95 * GS ) ) ! set the 5-percentile
      endif ! set the 5-percentile
      return
      endif! log-normal distribution -------------------------------------------

*     non-parametric and other distributions -----------------------------------
      if ( idist .ge. 4 ) then
      A = amax1 ( 0.0, quolity data(IQ,JDET,1) )
      S = quolity data(IQ,JDET,2)
      Q = quolity data(IQ,JDET,3)
      endif ! non-parametric and other distributions ---------------------------

      return
      end


*     calculate summary statistics for flow shots ------------------------------
      subroutine calculate summaries of river flow
      include 'COMMON DATA.FOR'
      dimension Y(MS),YE(MS)

      do IS = 1,NS
      Y(IS) = FMS (IS) ! current shots for river flow
      YE(IS) = EFMS (IS) ! proportion of effluent in each shot of river flow
      enddo
      
      do ii = 1, NM
      flow(ii) = 0.0
      enddo
      do ii = 1, 9
      flowstats (ii) = 0.0
      propstats (ii) = 0.0
      enddo

      FM = 0.0
      FS = 0.0
      do IS = 1,NS
      FM = FM + Y(IS)
      FS = FS + Y(IS) * Y(IS)
      enddo

      do 2 I = 1,NS-1
      do 3 J = I+1,NS
      if ( Y(I) .lt. Y(J) ) goto 3
      FX = Y(I)
      Y(I) = Y(J)
      Y(J) = FX
      FX = YE(I)
      YE(I) = YE(J)
      YE(J) = FX
    3 continue
    2 continue

      FS = (FS-FM*FM/NS)/(NS-1)
      if ( FS .gt. 1.0E-10 ) goto 98
      FS = 0.0
      goto 86
   98 continue
      FS = SQRoot(1097,FS)
   86 continue
      FM = FM/float(NS)

      flow(1) = FM ! mean
      flow(5) = FS ! standard deviation
      flow(2) = amax1(Y(k95),0.0) ! calculated 95 percentile of low river flow -
      flow(3) = amax1(Y(k90),0.0) ! calculated 90 percentile of low river flow -
      flow(4) = amax1(Y(k99),0.0) ! calculated 99 percentile of low river flow -

      flowstats (1) = amax1(Y(kf99),0.0)
      flowstats (2) = amax1(Y(kf95),0.0)
      flowstats (3) = amax1(Y(kf90),0.0)
      flowstats (4) = amax1(Y(kf80),0.0)
      flowstats (5) = amax1(Y(kf50),0.0)
      flowstats (6) = amax1(Y(kf20),0.0)
      flowstats (7) = amax1(Y(kf10),0.0)
      flowstats (8) = amax1(Y(kf05),0.0)
      flowstats (9) = amax1(Y(kf01),0.0)

      propstats (1) = amax1(YE(kf99),0.0) ! proportions of effluent flow
      propstats (2) = amax1(YE(kf95),0.0)
      propstats (3) = amax1(YE(kf90),0.0)
      propstats (4) = amax1(YE(kf80),0.0)
      propstats (5) = amax1(YE(kf50),0.0)
      propstats (6) = amax1(YE(kf20),0.0)
      propstats (7) = amax1(YE(kf10),0.0)
      propstats (8) = amax1(YE(kf05),0.0)
      propstats (9) = amax1(YE(kf01),0.0)

      return
      end

      
      
*     version of "calculate summaries of river flow" for tributaries ...
      subroutine calculate summaries of removed flows
      include 'COMMON DATA.FOR'
      dimension Y(MS),YE(MS)

      do IS = 1,NS
      Y(IS) = FTMS (IS) ! shots for a negative discharge
      YE(IS) = EFMS (IS) ! proportion of effluent in each shot of river flow
      enddo

      do ii = 1, 5
      FLOW(ii) = 0.0
      enddo
      do ii = 1, 9
      flowstats (ii) = 0.0
      propstats (ii) = 0.0
      enddo

      FM = 0.0
      FS = 0.0
      do IS = 1,NS
      FM = FM + Y(IS)
	FS = FS + Y(IS) * Y(IS)
      enddo

      do 2 I = 1, NS-1    
      do 3 J = I+1, NS
      if ( Y(I) .lt. Y(J) ) goto 3
      FX = Y(I)
      Y(I) = Y(J)
      Y(J) = FX
      FX = YE(I)
      YE(I) = YE(J)
      YE(J) = FX
    3 continue
    2 continue

      FS = (FS-FM*FM/NS)/(NS-1)
      if ( FS .gt. 1.0E-10 ) goto 98
      FS = 0.0
      goto 86
   98 continue
      FS = SQRoot(1097,FS)
   86 continue
      FM = FM/float(NS)

      flow(1) = FM ! mean
      flow(5) = FS ! standard deviation
      flow(2) = amax1(Y(k95),0.0) ! calculated 95 percentile of low river flow -
      flow(3) = amax1(Y(k90),0.0) ! calculated 90 percentile of low river flow -
      flow(4) = amax1(Y(k99),0.0) ! calculated 99 percentile of low river flow -

      flowstats (1) = amax1(Y(kf99),0.0)
      flowstats (2) = amax1(Y(kf95),0.0)
      flowstats (3) = amax1(Y(kf90),0.0)
      flowstats (4) = amax1(Y(kf80),0.0)
      flowstats (5) = amax1(Y(kf50),0.0)
      flowstats (6) = amax1(Y(kf20),0.0)
      flowstats (7) = amax1(Y(kf10),0.0)
      flowstats (8) = amax1(Y(kf05),0.0)
      flowstats (9) = amax1(Y(kf01),0.0)

      propstats (1) = amax1(YE(kf99),0.0) ! proportions of effluent flow
      propstats (2) = amax1(YE(kf95),0.0)
      propstats (3) = amax1(YE(kf90),0.0)
      propstats (4) = amax1(YE(kf80),0.0)
      propstats (5) = amax1(YE(kf50),0.0)
      propstats (6) = amax1(YE(kf20),0.0)
      propstats (7) = amax1(YE(kf10),0.0)
      propstats (8) = amax1(YE(kf05),0.0)
      propstats (9) = amax1(YE(kf01),0.0)

      return
      end



      subroutine calculate monthly summaries of river flow
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do IS = 1,NS
      Y(IS) = FMS(IS)
      enddo

      do i = 1, n13
      fmon(1,i) = 0.0
      fmon(2,i) = 0.0
      enddo
      
      if ( munthly structure .eq. 0 ) then
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0)) ! for river flow
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      do ii = 1, 12
      fmon(1,ii) = flow(1)
      fmon(2,ii) = flow(2)
      enddo
      endif
      return ! no requirement for monthly data 
      endif

      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13
      FM = 0.0
      FS = 0.0
      N2 = 0.0

      do IS = 1,NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
      if ( jmon .lt. 13 ) then
      if ( imon .eq. jmon ) then
      FM = FM + Y(IS)
      FS = FS + Y(IS) * Y(IS)
      N2 = N2 + 1
      endif
      else
      FM = FM + Y(IS)
      FS = FS + Y(IS) * Y(IS)
      N2 = N2 + 1
      endif
      enddo

      FS = (FS-FM*FM/NS)/(N2-1)
      if (FS .gt. 1.0E-10) goto 98
      FS = 0.0
      goto 86
   98 continue
      FS = SQRoot(1097,FS)
   86 continue
      FM = FM/float(N2)

      fmon(1,jmon) = amax1 ( 0.0, FM )
      fmon(2,jmon) = FS
      
*     mean ---------------------------------------------------------------------
      A = amax1 ( 0.0, fmon(1,jmon) )
*     check for a zero mean ----------------------------------------------------
      if (A .lt. 1.0E-08) fmon(2,jmon) = 0.0
*     check for zero standard deviation ----------------------------------------
      if (fmon(2,jmon) .lt. 1.0E-08) goto 36
*     set the standard deviation -----------------------------------------------
      S = fmon(2,jmon)
*     calculate the 95-percentile low flow -------------------------------------
      GEFM = ALOG((A*A)/SQRoot(1057,A*A+S*S))
      TRAK = ALOG(1.+(S*S)/(A*A))
      if ( TRAK .gt. 1.0e-20) then
      GEFS = SQRoot(105657,TRAK)
      endif
      fmon(2,jmon) = EXP(GEFM-t95*GEFS)
   36 continue
      
   88 continue
      
      return
      end



*     calculate summary statistics for flow shots ------------------------------
      subroutine statistics for monthly river flows (kwrite,CM1,CM2)
      include 'COMMON DATA.FOR'
      dimension YA(MS),YM(MS)
      dimension FM(12),F5(12),NFK(12)

      CM1 = 0.0 ! initialise annual mean
      CM2 = 0.0 ! initialise annual 95-percentile low flow
      
*     copy the shots -----------------------------------------------------------
      do IS = 1,   NS
      YA(IS) = YY(IS)
      YM(IS) = YY(IS)
      enddo
      

*     initialise the monthly means and 95-percentiles --------------------------
      do imon = 1, 12
      FM (imon) = 0.0
      F5 (imon) = 0.0
      NFK(imon) = 0 ! the number of monthly samples
      enddo

*     compute the monthly means ------------------------------------------------
      do IS = 1, NS ! ----------------------------------------------------------
*     set the month for this shot ----------------------------------------------
      imon = qmonth (IS)
      FM (imon) = FM (imon)  + YM(IS)
      NFK (imon) = NFK (imon) + 1
      enddo ! do IS = 1, NS ----------------------------------------------------
      do imon = 1, 12
      FM (imon) = FM (imon) / float(NFK(imon))
      enddo

*     loop on the months =======================================================
      do 4 jmon = 1, 12 ! ======================================================
      knum = 0
      do IS = 1, NS ! ----------------------------------------------------------
*     set the month for this shot ----------------------------------------------
      imon = qmonth (IS)
      if ( jmon .eq. imon ) then
      knum = knum + 1
*     store the flows for this month -------------------------------------------
      YM (knum) = YA (IS)
      endif
      enddo ! do IS = 1, NS ----------------------------------------------------


      
*     sequence the flows in the month ------------------------------------------
      do I = 1, knum-1
      do J = I + 1, knum
      if ( YM(I) .gt. YM(J) ) then
      FX = YM(I)
      YM(I) = YM(J)
      YM(J) = FX
      endif
      enddo
      enddo
      
      km95 = nint(0.05*float(NFK(jmon)))
      if ( km95 .le. 0 ) km95 = 1
      F5(jmon) = amax1( YM(km95), 0.0 )
      if ( NFK(jmon) .le. 0 ) then
      FM(jmon)=0.0
      F5(jmon)=0.0
      endif
      
    4 continue ! do 4 jmon = 1, 12 ---- loop on months =========================

      if ( kwrite .eq. 1 ) then
      if ( MONF .gt. 1 ) then
      !call write shots for river flow
      !write(01,4398)uname(feeture)
      !write(09,4398)uname(feeture)
      !write(33,4398)uname(feeture)
 4398 format(50('+')/
     &'Generated data on flows ...'/'At: ',a40/50('-'))
      !&'Month        ','       Mean','         5%','       Days'/
      !&46('-'))
      !if (ical13 .eq. 0 ) write(01,8599)(FM (imon),F5 (imon),
      !&NFK(imon),imon = 1,12)
      !write(09,8599)(FM(imon),F5(imon),NFK(imon),imon = 1,12)
      !write(33,8599)(FM(imon),F5(imon),NFK(imon),imon = 1,12)
      !&NFK(imon),imon = 1,12)
 8599 format(
     &'January ...  ',2f11.4,i11/
     &'February ... ',2f11.4,i11/
     &'March ...    ',2f11.4,i11/
     &'April ...    ',2f11.4,i11/
     &'May ...      ',2f11.4,i11/
     &'June ...     ',2f11.4,i11/
     &'July ...     ',2f11.4,i11/
     &'August ...   ',2f11.4,i11/
     &'September ...',2f11.4,i11/
     &'October ...  ',2f11.4,i11/
     &'November ... ',2f11.4,i11/
     &'December ... ',2f11.4,i11)
      endif ! if ( MONF .gt. 1 )
      endif

      nadd = 0
      yadd = 0.0
      do imon = 1, 12
      if ( NFK (imon) .gt. 0 ) then
      yadd = yadd + FM (imon) * NFK(imon)
      nadd = nadd + NFK(imon)
      endif
      enddo
      yadd = yadd / NS

*     calculate the overall mean -----------------------------------------------
      FX = 0.0
      do IS = 1, NS
      FX = FX + YA(IS)
      enddo
      FLOW(1) = FX / float(NS) ! mean

      do I = 1,   NS - 1
      do J = I+1, NS
      if ( YA(I) .gt. YA(J) ) then
      FX = YA(I)
      YA(I) = YA(J)
      YA(J) = FX
      endif
      enddo
      enddo
      k95 = 0.05 * NS ! 95-percentile
      FLOW(2) = amax1( YA(k95), 0.0 ) ! 95-percentile

      CM1 = FLOW(1)
      CM2 = FLOW(2)

      !if ( kwrite .eq. 1 ) then
      if ( MONF .gt. 1 ) then ! ------------------------------------------------ 
      !write(01,4119)FLOW(1),FLOW(2),NS
      !write(09,4119)FLOW(1),FLOW(2),NS
      write(33,4119)FLOW(1),FLOW(2),NS
 4119 format('Annual ...   ',2f11.4,i11/50('+'))
      endif ! ------------------------------------------------------------------
      !endif

      return
      end



*     get statistics for the flow shots of the discharge or stream -------------
      subroutine get statistics for the flows of the discharge or stream
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do IS = 1, NS
      Y(IS) = EFshots(IS)
      enddo

      FM = 0.0
      FS = 0.0
      do IS = 1,NS
      FS = FS + Y(IS) * Y(IS)
      FM = FM + Y(IS)
      enddo

      do 2 I=1,k90
      do 3 J=I+1,NS
      if (Y(I) .lt. Y(J)) goto 3
      FX=Y(I)
      Y(I)=Y(J)
      Y(J)=FX
    3 continue
    2 continue

      FS = (FS-FM*FM/NS)/(NS-1)
      if (FS .gt. 1.0E-10) goto 98
      FS = 0.0
      goto 86
   98 continue
      FS = SQRoot(1098,FS)
   86 continue
      FM = FM/float(NS)

      EFLOW(2) = FS
      EFLOW(1) = FM
      EFLOW(3) = amax1(Y(k90),0.0)
      EFLOW(4) = amax1(Y(k99),0.0)
      
      FLOW(5) = FS
      FLOW(1) = FM
      FLOW(2) = amax1(Y(k95),0.0)
      FLOW(3) = amax1(Y(k90),0.0)
      FLOW(4) = amax1(Y(k99),0.0)

      return
      end



*     calculate the summary statistics from the shots --------------------------
      subroutine get summaries of river quality from the shots ! ---------------
      include 'COMMON DATA.FOR'
      dimension Y(MS)
            
      do 1000 JDET = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (JDET) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++
      
          
*     00000000000000000000000000000000000000000000000000000000000000000000000000
*     check for results bigger than a billion 0000000000000000000000000000000000
      do IS = 1, NS ! loop on shots 00000000000000000000000000000000000000000000
      Y(IS) = CMS(JDET,IS)
      
      if ( Y(IS) .gt. 1.0e9 ) then ! error detected 0000000000000000000000000000
      xxxxxx = Y(IS)
      ixxxxx = IS
      Y(IS) = C(JDET,1)
      call sort format 2 (xxxxxx,FMS(IS)) 
      write(01,5282)ixxxxx,dname(jdet),valchars10,valchars11,C(JDET,1) ! --- OUT
      write(31,5282)ixxxxx,dname(jdet),valchars10,valchars11,C(JDET,1) ! --- EFF
      write(33,5282)ixxxxx,dname(jdet),valchars10,valchars11,C(JDET,1) ! --- ERR
      write(150+jdet,5282)ixxxxx,dname(jdet),valchars10,valchars11, ! --- Di EFF
     &C(JDET,1)
 5282 format(
     &'*** SHOT',i5,' greater than a billion for ',a11,'= ',
     &a10,' ...  river flow = ',a10/
     &'*** Re-set quality to a value of',f10.3)
      CMS(JDET,IS) = C(JDET,1)
      endif ! if ( Y(IS) .gt. 1.0e9 ) 000000000000000000000000000000000000000000
      enddo ! do IS = 1, NS ... loop on shots 0000000000000000000000000000000000
*     00000000000000000000000000000000000000000000000000000000000000000000000000

      do IS = 1, NS ! loop on shots 00000000000000000000000000000000000000000000
      Y(IS) = CMS(JDET,IS)
      enddo
      
      call calculate summaries of river quality (33,JDET,Y)

      do IS = 1,NS
      Y(IS) = CMS(JDET,IS)
      enddo
      call calculate monthly summaries of quality (JDET,Y)
       
      endif ! if ( QTYPE(JDET) .ne. 4 ) ++++++++++++++++++++++++++++++++++++++++
 1000 continue ! do 1000 JDET = 1, ndet XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      
      return
      end

      
      
*     333333333333333333333333333333333333333333333 apportionment of percentiles
      subroutine identify the shots for percentiles (JDET) ! 3333333333333333333
      include 'COMMON DATA.FOR'
      dimension Y(MS),JY(MS) ! 33333333333333333333 apportionment of percentiles
      
      do IS = 1,NS
      Y(IS) = CMS(JDET,IS)
      enddo

      JS50 = NS
      JS90 = NS
      JS95 = NS
      JS98 = NS
      JS99 = NS ! 333333333333333333333333333333333 apportionment of percentiles
      JS995 = NS
      JS999 = NS
      do 1 I = 1, NS
      ymax = -999.9
      do J = 1, NS
      if (Y(J) .gt. ymax) then
      jmax = J
      ymax = Y(J)
      endif
      JY(I) = jmax
      enddo
      Y(jmax) = -Y(jmax)
    1 continue
      
      JS50 = JY(NS/2)
      JS90 = JY(K90)
      JS95 = JY(K95)
      JS98 = JY(K98)
      JS99 = JY(K99) ! 3333333333333333333333333333 apportionment of percentiles
      JS995 = JY(K995)
      JS999 = JY(K999)

      return
      end ! 333333333333333333333333333333333333333 apportionment of percentiles
*     333333333333333333333333333333333333333333333 apportionment of percentiles

      
      

      subroutine calculate summaries of river quality (KSO,JDET,Y)
      include 'COMMON DATA.FOR'
      dimension Y(MS),Pa(MS),Pb(MS),YO(MS)

      do IS = 1,NS ! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      Pa(IS) = 0.0 ! for dissolved metal
      Pb(IS) = 0.0 ! for solid metal
      enddo ! do IS = 1,NS mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      do i = 1, 5
      C(JDET,i) = 0.0
      enddo
      if ( QTYPE (JDET) .eq. 4 ) return

      CM = 0.0
      CS = 0.0
      do IS = 1,NS
      YO(IS) = Y(IS)
      CM = CM + Y(IS)
      CS = CS + Y(IS) * Y(IS)
      enddo

      
*     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      if ( detype (jdet) .ge. 900 ) then ! ----------- dissolved and solid metal
      if ( partishun (jdet) .gt. 0.01 ) then
      gkay = 10.0 ** partishun (jdet)
      do IS = 1, NS
      spm = BMS(2,IS) ! ----------------------------- shots for suspended solids
      totalx = Y(IS)
      Pa(IS) = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! ------- dissolved metal
      Pb(IS) = totalx - Pa(IS) ! ----------------------------------- solid metal
      PMS1(jdet,IS) = Pa(IS) ! --------------------------------- dissolved metal
      PMS2(jdet,IS) = Pb(IS) ! ------------------------------------- solid metal
      enddo
      else
      do IS = 1, NS
      Pa(IS) = Y(is) ! dissolved metal
      Pb(IS) = 0.0 ! solid metal
      enddo
      endif ! if ( partishun (jdet) .gt. 0.01 ) --------------------------------
      endif ! if ( detype (jdet) .ge. 900 ) ========== dissolved and solid metal
*     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      
      if ( CS .lt. 1.0E-25 ) then 
      CS = 0.0
      else
      if ( CS .gt. 1.0E25 ) then
      CS = 1.0E25
      else
      CS = (CS-CM*CM/NS)/(NS-1)
      if ( CS .lt. 1.0E-20) CS = 1.0E-20
      CS = SQRoot3(321099,CS) ! standard deviation
      endif
      endif
      CM = CM/NS ! annual mean
      
      goto (10,10,11,99,11,10), QTYPE(JDET)
*     compute 95-percentile ----------------------------------------------------
   10 continue
      do 2 I=1,k90
      do 3 J=I+1,NS
      if (Y(I) .gt. Y(J)) goto 3
      Ctemp=Y(I)
      Y(I)=Y(J)
      Y(J)=Ctemp
    3 continue
    2 continue
      goto 12

*     compute 5-percentile -----------------------------------------------------
   11 continue
      do 22 I=1,k90
      do 23 J=I+1,NS
      if (Y(I) .lt. Y(J)) goto 23
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
   23 continue
   22 continue

   12 continue
      C(JDET,1) = amax1 ( 0.0, CM )
      C(JDET,2) = CS
      C(JDET,3) = amax1 (0.0,Y(k95))
      C(JDET,4) = amax1 (0.0,Y(k90))
      C(JDET,5) = amax1 (0.0,Y(k99))
      
   99 continue
      
      do IS = 1,NS 
      Y(IS) = YO(IS)
      enddo
      
      
*     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      do i = 1, 5 ! summary statistics for partitions
      cpart1 (JDET,i) = 0.0 ! dissolved metal
      cpart2 (JDET,i) = 0.0 ! solid metal
      enddo ! summary statistics for partitions

*     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal mmmmmmmmmmm
      PM1=0.0
      PS1=0.0
      PM2=0.0
      PS2=0.0
      do IS=1,NS
      PX1 = Pa(IS) ! dissolved metal
      PX2 = Pb(IS) ! solid metal
      PM1 = PM1 + PX1
      PS1 = PS1 + PX1*PX1
      PM2 = PM2 + PX2
      PS2 = PS2 + PX2*PX2
      enddo

      PS1=(PS1-PM1*PM1/NS)/(NS-1)
      if (PS1 .lt. 1.0E-20) then
      PS1=0.0
      else
      PS1=SQRoot(331099,PS1)
      endif
      PM1=PM1/NS

      PS2=(PS2-PM2*PM2/NS)/(NS-1)
      if (PS2 .lt. 1.0E-20) then
      PS2=0.0
      else
      PS2=SQRoot(1099,PS2)
      endif
      PM2=PM2/NS

      do I = 1,k90
      do J = I+1,NS
      if ( Pa(I) .lt. Pa(J) ) then ! dissolved metal
      Ctemp = Pa(I)
      Pa(I) = Pa(J)
      Pa(J) = Ctemp
      endif ! dissolved metal
      if ( Pb(I) .lt. Pb(J) ) then ! solid metal
      Ctemp = Pb(I)
      Pb(I) = Pb(J)
      Pb(J) = Ctemp
      endif ! solid metal
      enddo
      enddo

      cpart1 (JDET,1) = amax1 ( 0.0, PM1) ! dissolved metal
      cpart1 (JDET,2) = PS1 ! dissolved metal
      cpart1 (JDET,3) = amax1 ( 0.0, Pa(k95)) ! dissolved metal
      cpart1 (JDET,4) = amax1 ( 0.0, Pa(k90)) ! dissolved metal
      cpart1 (JDET,5) = amax1 ( 0.0, Pa(k99)) ! dissolved metal
      cpart2 (JDET,1) = amax1 ( 0.0, PM2) ! solid metal
      cpart2 (JDET,2) = PS2 ! solid metal
      cpart2 (JDET,3) = amax1 ( 0.0, Pb(k95)) ! solid metal
      cpart2 (JDET,4) = amax1 ( 0.0, Pb(k90)) ! solid metal
      cpart2 (JDET,5) = amax1 ( 0.0, Pb(k99)) ! solid metal
      
      endif ! dissolved and solid metal mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
*     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      
      return
      end


      subroutine update summaries of contribution 
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do idet = 1, ndet ! ======================================================
      if ( qtype (idet) .ne. 4 ) then ! ========================================
          
      do IP = 1, n2prop ! loop through all types of feature tracked for inputs ~
      do IS = 1, NS ! ----------------------------------------------------------
      if ( IP .eq. NTD ) then
      Y(IS) = CMS(idet,IS) ! concentrations in river ---------------------------      
      else
      Y(IS) = amax1(0.0,CTMS(IP,idet,IS)) ! concentrations from types of feature
      endif
      enddo ! do IS = 1, NS ---------------------------------------------------- 
      
      call calculate summaries of contribution (IP,idet,Y,0) ! the subroutine

      do IS = 1, NS ! ----------------------------------------------------------
      if ( IP .eq. NTD ) then
      Y(IS) = CMS(idet,IS) ! river concentrations ------------------------------      
      else
      Y(IS) = CTMS(IP,idet,IS) ! concentrations from various types of feature --
      endif
      enddo ! do IS = 1, NS ----------------------------------------------------

      call calculate monthly summaries of contribution (ip,idet,Y)
            
      enddo ! loop through all types of feature tracked for inputs ~~~~~~~~~~~~~
      
      endif ! if ( qtype (idet) .ne. 4 ) =======================================
      enddo ! idet = 1, ndet ===================================================
 
      return
      end



      subroutine calculate summaries of contribution (ip,JDET,Y,ixx)
      include 'COMMON DATA.FOR'
      dimension Y(MS)
      
      CM=0.0
      CS=0.0
      CLM=0.0
      CLS=0.0
      
      CMcontrib(ip,JDET,1) = 0.0 ! mean
      CMcontrib(ip,JDET,2) = 0.0 ! standard deviation
      CMcontrib(ip,JDET,3) = 0.0 ! 90-percentile
      CMcontrib(ip,JDET,4) = 0.0 ! 95-percentile
      CMcontrib(ip,JDET,5) = 0.0 ! 99-percentile
      LMcontrib(ip,JDET,1) = 0.0 ! mean load
      LMcontrib(ip,JDET,2) = 0.0 ! standard deviation
      
      CTOT = 0.0 ! average concentration
      TOTL = 0.0 ! average load

      do IS = 1, NS
      CM = CM+Y(IS)
      CS = CS+Y(IS)*Y(IS)
      CLM = CLM+(Y(IS)*FMS(IS))
      CLS = CLS+(Y(IS)*FMS(IS))*(Y(IS)*FMS(IS))
      CTOT = CTOT+CMS(JDET,IS)
      TOTL = TOTL+(CMS(JDET,IS)*FMS(IS))
      enddo

      if (CS .lt. 1.0E-10) then
      CS=0.0
      else
      CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) then
      CS=SQRoot(341099,CS)
      endif
      endif
      CM=CM/NS

      if (CLS .lt. 1.0E-10) then
      CLS=0.0
      else
      CLS=(CLS-CLM*CLM/NS)/(NS-1)
      if (CLS .gt. 1.0E-20) then
      CLS=SQRoot(341099,CLS)
      endif
      endif
      CLM=CLM/float(NS)
     
      CTOT=CTOT/NS ! average concentration
      TOTL=TOTL/NS ! average load

      goto (10,10,11,99,11,10), QTYPE(JDET)

*     compute 95-percentile ----------------------------------------------------
   10 continue
      do I=1,k90
      do 3 J=I+1,NS
      if (Y(I) .gt. Y(J)) goto 3
      Ctemp=Y(I)
      Y(I)=Y(J)
      Y(J)=Ctemp
    3 continue
      enddo

      goto 12

*     compute 5-percentile -----------------------------------------------------
   11 continue
      do I=1,k90
      do 23 J=I+1,NS
      if (Y(I) .lt. Y(J)) goto 23
      Ctemp=Y(I)
      Y(I)=Y(J)
      Y(J)=Ctemp
   23 continue
      enddo

   12 continue

      CMcontrib(ip,JDET,1) = amax1 (0.0,CM)
      CMcontrib(ip,JDET,2) = CS
      CMcontrib(ip,JDET,3) = amax1 (0.0,Y(k95))
      CMcontrib(ip,JDET,4) = amax1 (0.0,Y(k90))
      CMcontrib(ip,JDET,5) = amax1 (0.0,Y(k99))
      
      LMcontrib(ip,JDET,1) = amax1 (0.0,CLM)
      LMcontrib(ip,JDET,2) = CLS

   99 continue
      
      return
      end



      subroutine calculate monthly summaries of quality (JDET,Y)
      include 'COMMON DATA.FOR'
      dimension Y(MS),Pa(MS),Pb(MS)

      if ( QTYPE (jdet) .eq. 4 ) return
      
      do i = 1, 13
      cmon(JDET,1,i) = 0.0 ! monthly means
      cmon(JDET,2,i) = 0.0 ! monthly standard deviations
      enddo

      !if ( munthly structure .eq. 0 ) then ! no requirement for monthly loads
      !xcheck = (float(NS)/365.0 - int(float(NS)/365.0))
      !if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      !do i = 1, 13
      !cmon(JDET,1,i) = C(JDET,1)
      !cmon(JDET,2,i) = C(JDET,2)
      !p1mon(JDET,1,i) = CP1(JDET,1)
      !p1mon(JDET,2,i) = CP1(JDET,2)
      !p2mon(JDET,1,i) = CP2(JDET,1)
      !p2mon(JDET,2,i) = CP2(JDET,2)
      !enddo
      !endif
      !return ! no requirement for monthly data 
      !endif

      do jmon = 1, 13
      CM = 0.0
      CS = 0.0
      N2 = 0.0

      do is = 1, NS
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

      if (CS .lt. 1.0E-25 ) then
      CS=0.0
      else
      if (CS .gt. 1.0E25 ) then
      CS=1.0E25
      else
      CS=(CS-CM*CM/N2)/(N2-1)
      CS=SQRoot3(670099,CS)
      endif
      endif
      CM=CM/N2
      cmon(JDET,1,jmon) = amax1 ( 0.0, CM )
      cmon(JDET,2,jmon) = CS
      enddo

      do i = 1, n13
      p1mon(JDET,1,i) = 0.0 ! dissolved metal
      p1mon(JDET,2,i) = 0.0
      P2mon(JDET,1,i) = 0.0 ! solid metal
      P2mon(JDET,2,i) = 0.0
      enddo

      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal
      do is = 1, NS
      Pa(IS) = Y(IS) ! initialise dissolved metal
      Pb(IS) = Y(IS) ! initialise solid metal
      enddo

      if ( partishun (jdet) .gt. 0.01 ) then
      gkay = 10.0 ** partishun (jdet)
      do IS = 1, NS
      spm = BMS(2,IS) ! ----------------------------- shots for suspended solids
      totalx = Pa(IS)
      Pa(IS) = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal
      Pb(IS) = totalx - Pa(IS) ! remaining solid metal
      enddo
      else
      do IS = 1, NS
      Pa(IS) = Y(is) ! dissolved metal
      Pb(IS) = 0.0 ! solid metal
      enddo
      endif ! if ( partishun (jdet) .gt. 0.01 )
      endif ! dissolved and solid metal

      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal
      j1 = n13
      if ( munthly structure .eq. 1 ) j1 = 1
      do 88 jmon = j1, n13
      PM1 = 0.0 ! dissolved metal
      PS1 = 0.0 ! dissolved metal
      PM2 = 0.0 ! solid metal
      PS2 = 0.0 ! solid metal
      NP1 = 0
      NP2 = 0
      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
      if ( jmon .lt. 13 ) then
      if ( imon .eq. jmon ) then
      PX1 = Pa(IS) ! dissolved metal
      PX2 = Pb(IS) ! solid metal
      PM1 = PM1+PX1
      PS1 = PS1+PX1*PX1
      PM2 = PM2+PX2
      PS2 = PS2+PX2*PX2
      NP1 = NP1 + 1
      NP2 = NP2 + 1
      endif
      else
      PX1 = Pa(IS) ! dissolved metal
      PX2 = Pb(IS) ! solid metal
      PM1 = PM1+PX1
      PS1 = PS1+PX1*PX1
      PM2 = PM2+PX2
      PS2 = PS2+PX2*PX2
      NP1 = NP1 + 1
      NP2 = NP2 + 1
      endif
      enddo

      if (PS1 .lt. 1.0E-10) then
      PS1=0.0
      else
      PS1=(PS1-PM1*PM1/NP1)/(NP1-1)
      if (PS1 .lt. 1.0E-20) then
      PS1=0.0
      else
      PS1=SQRoot(361099,PS1)
      endif
      endif
      PM1=PM1/NP1

      if (PS2 .lt. 1.0E-10) then
      PS2=0.0
      else
      PS2=(PS2-PM2*PM2/NP2)/(NP2-1)
      if (PS2 .lt. 1.0E-20) then
      PS2=0.0
      else
      PS2=SQRoot(371099,PS2)
      endif
      endif
      PM2=PM2/NP2

      p1mon(JDET,1,jmon) = amax1 ( 0.0, PM1) ! dissolved metal
      p1mon(JDET,2,jmon) = PS1 ! dissolved metal
      P2mon(JDET,1,jmon) = amax1 ( 0.0, PM2)
      P2mon(JDET,2,jmon) = PS2
   88 continue
      
      endif ! if ( detype (jdet) .ge. 900 ) ========== dissolved and solid metal

      return
      end

      

*     estimate mean and standard deviation for percentile ----------------------
*     parametric methods - Log-normal distribution -----------------------------
      subroutine change to log domain (DCP, PER, D1, S1, DCM, DCS)

      if (DCP .le. 1.0e-09 ) then
      DCM = 0.0
      DCS = 0.0
      return
      endif

      if (DCP .lt. 1.0e-09 ) then
      DCM = 0.0
      DCS = 0.0
      return
      endif
      
      if (S1 .lt. 1.0e-09 ) then
      DCM = D1
      DCS = S1
      return
      endif

      COV = 1.0e-10
      if ( D1 .gt. 1.0e-10) then
      COV = S1/D1
      endif

      GDCP = ALOG (DCP)

*     standard normal deviate --------------------------------------------------
      T0 = errx ( 0.01*PER )
      GDCP = ALOG (DCP)
      Brak = 1.0 + COV * COV

*     standard deviation of the logged variables -------------------------------
      GDCS = 0.0
      if (ALOG(BRAK) .gt. 1.0e-20 ) then
      GDCS = SQRoot(110046, ALOG(BRAK) )
      endif

*     mean in the log domain ---------------------------------------------------
      GDCM = GDCP - T0 * GDCS

      DCM = exp ( GDCM ) *  SQRoot(110246, BRAK )
      BRAK2 = EXP (GDCS*GDCS) - 1.0
      if ( BRAK2 .gt. 1.0e-20 ) then
      DCS = DCM * SQRoot(110146, BRAK2 )
      endif
      
      return
      end


      
*     estimate compliance with mean standard ===================================
      subroutine calculate compliance with mean standard (
     &dcm,dcs,EN,DCC,ex,DCP,DCPL,DCPU,KU,iidet)
      include 'COMMON DATA.FOR'
  
      if ( DCC .lt. 1.0e-20 .or. DCC .gt. 1.0e6 ) return

*     initialise confidence that the standard was failed -----------------------
      ex = 0.0
*     set small number for use in tests for zero -------------------------------
      small number = 1.0E-20
*     set number of samples to a minimum of 3 ----------------------------------
      if ( EN .lt. 3.0 ) EN = 3.0
*     test for zero ------------------------------------------------------------
      if ( dcm .lt. small number ) return
    
*     values of the deviate greater than 4.0 will be assumed to cover 100% of -- 
*     the distribution ---------------------------------------------------------
      big deviate = 4.0
    
      GDCM = DCM ! mean
      DCP = DCM 
      GDCS = DCS / SQRoot(1104,EN) ! standard error

*     calculate confidence limits ----------------------------------------------
      T0 = TDIST1(EN-1.0,0.95) ! obtain t-statistic
      DCPL = GDCM - T0 * GDCS ! lower confidence limit
      DCPU = GDCM + T0 * GDCS ! upper confidence limit
      
*     take the opportunity to check for strange numbers 
      if ( DCM .gt. 999999.0 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
*     batch runs ---------------------------------------------------------------
      if ( chekk infeezible .lt. 3 ) then
      chekk infeezible = chekk infeezible + 1 	
      suppress14 = suppress14 + 1
      call change colour of text (10) ! green
      write( *,8306)uname(KU)
 8306 format('*** Encountered infeasibly huge concentrations',5x,
     &'...',7x,'at ',a40,1x,25('.'))
      call set screen text colour
      write(09,8306)
      write(33,8366)
 8366 format('Encountered infeasibly big concentrations in assessing ',
     &'compliance ...'/'Check the ERR file for details ')
      write(01,8366)
      endif
      else ! non batch runs
      call change colour of text (20) ! bright red ... error message -----------
      write( *,8866)dname(iidet),DCM,DCS,int(EN),DCC
      call set screen text colour
      write(33,8866)dname(iidet),DCM,DCS,int(EN),DCC
      write(01,8866)dname(iidet),DCM,DCS,int(EN),DCC
      write(09,8866)dname(iidet),DCM,DCS,int(EN),DCC
 8866 format(/110('=')/'Infeasibly big concentrations in assessing ',
     &'compliance for ',a11/
     &'This might be caused by adding load to ',
     &'near-zero river flows ...  '/
     &'WARNING: mean quality =',F19.1/
     &'   Standard deviation =',F19.1/
     &'    Number of samples =',i19/
     &'        Mean standard =',F19.4/110('=')) ! warning message
 	endif ! if ( ifbatch .eq. 1 )
	endif ! if ( DCM .gt. 999999.0 )

*     compare area of Error Distribution of estimate of mean with the standard -
*     this is an estimate of the probability that the standard has been achieved

*     calculate the deviate and associated probability point -------------------

      Z = DCC - DCP
      if ( Z .lt. small number .and. Z .gt. -small number) then
      ex = 50.0
      return
      endif

      T probability = Z/GDCS
    
*     check size of probability ------------------------------------------------
      if ( T probability .gt.  big deviate ) then
      EX = 0.0
      if ( QTYPE (iidet) .eq. 3 .or. QTYPE (iidet) .eq. 5 ) EX = 100.0
      return
      endif

      if ( T probability .lt. -big deviate ) then
      EX = 100.0
      if ( QTYPE (iidet) .eq. 3 .or. QTYPE (iidet) .eq. 5 ) EX = 0.0
      return
      endif

      EX = 100.0 * ( 1.0 - TDIST2 (EN-1.0, T probability) )
*     check for standards that are to be exceeded ------------------------------
      if ( QTYPE (iidet) .eq. 3 .or. QTYPE (iidet) .eq. 5 ) 
     &EX = 100.0-EX

      return
      end


*     Normal Probability Function ----------------------------------------------
      Function Errx (x)
      p = 2.0 * x - 1.0
      if ( x .lt. 0.5 ) p = -p
      xx = errinv ( p ) * 1.41421
      Errx = sign ( xx, x - 0.5 )
      return
      end

*     Inverse Error Function ---------------------------------------------------
      Function errinv ( c )
      Double precision y(7)
      data Y/0.00D+00,0.842700793,0.995322265,0.999977910,0.999999984,
     &       1.00D+00,1.00D+00/

      if ( c .ge. 1.0 ) then
      errinv = 6.0
      goto 99
      endif

      if ( c .le. 0.0 ) then
      errinv = 6.0
      goto 99
      endif

      do 50 ii = 1, 7
      i = ii
      if ( Y(ii) .eq. c ) then
      errinv = float ( i - 1 )
      goto 99
      endif

      if ( Y(ii) .gt. c ) goto 70
   50 continue
   70 xc = i - 2

      do k = 1, 20
      A = errf ( xc )
      temp = xc + (c - A) * ( 0.886226925 * exp ( xc**2 ) )
      B = errf ( temp )
      z = c - B
      xc = temp
      if ( z .lt. 1.0e-10 ) goto 80
      enddo
   80 errinv = temp
   99 return
      end


*     calculations for the errors function -------------------------------------
      function errf (w)
      double precision a(25), b(30), f
      data a/16443152242714.D-13,-9049760497548.D-13,
     &643570883797.D-13,196418177368.D-13,-1244215694.D-13,
     &-9101941905.D-13,-1796219835.D-13,139836786.D-13,
     &164789417.D-13,39009267.D-13,-893145.D-13,-3747896.D-13,
     &1298818.D-13,136773.D-13,77107.D-13,46810.D-13,
     &11844.D-13,-5.D-13,-1384.D-13,-652.D-13,145.D-13,
     &10.D-13,24.D-13,11.D-13,2.D-13/

      m = 24
      x = abs ( w )
      if ( x - 0.01 ) 1,2,2

    1 xerr = 2.0 / ( 3.0 * 1.77245385 ) * x * ( 3.0 - x**2 )
      goto 6

    2 z = ( x - 1.0 ) / ( x + 1.0 )

      do i = 1, 30
      b(i) = 0.0
      enddo

      do i = 1, m
      m1 = ( m + 1 ) - i
      b ( m1 ) = 2.0 * z * b( m1 + 1 )-b( m1 + 2 ) + a( m1 + 1 )
      enddo

      f = -b(2) + z * b(1) + 0.5 * a(1)
      xerr = 1.0 - ( 1.0 / 1.77245385 ) * (exp (-(x**2))) * f

      if ( x - 0.01 ) 6,7,7
    6 cerr = 1.0 -xerr
      goto 5

    7 cerr = ( 1.0/1.77245385 ) * (exp(-(x**2))) * f
    5 if ( w ) 9,8,8
    8 errf = xerr
      goto 13

    9 errf = cerr
   13 return
      end



*     calculate the summary statistics of loads --------------------------------
      subroutine get summaries of loads
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do 1 JDET = 1, ndet
      if ( QTYPE (JDET) .eq. 4 ) goto 1
      do IS = 1,NS
      Y(IS) = CMS(JDET,IS) * FMS(IS) ! calculate load of shot
      enddo
      call calculate monthly summaries of loads (JDET,Y)
    1 continue
      
      return
      end
*     --------------------------------------------------------------------------
      subroutine calculate monthly summaries of loads (JDET,Y)
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      do i = 1, 13
      lmon(JDET,1,i) = 0.0
      lmon(JDET,2,i) = 0.0
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
    9 CS=SQRoot(381099,CS)
    6 CM=CM/N2

      lmon(JDET,1,jmon) = amax1 ( 0.0, CM )
      lmon(JDET,2,jmon) = CS

   88 continue ! do 88 jmon = j1, n13
      
      if ( munthly structure .eq. 0 ) then
      xcheck = (float(NS)/365.0 - int(float(NS)/365.0))
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      do ii = 1, 12
      lmon(JDET,1,ii) = lmon(JDET,1,n13)
      lmon(JDET,2,ii) = lmon(JDET,2,n13)
      enddo
      endif
      return ! no requirement for monthly data 
      endif
      
      return
      end



      subroutine assem(X,Y,LINE)
      character *13LINE
      character *1A(13)
      LINE='             '

      ip = 1
      if ( X .lt. 20.) ip = 2
      if ( X .lt. 1.5) ip = 3
      if ( X .lt. 0.15) ip = 4
      if ( X .lt. 0.015) ip = 5
      if ( X .lt. 0.0015) ip = 6
      if ( X .lt. 0.00015) ip = 7

      goto (4,9,10,20,30,30,30,9,9),Ip

    4 L=INT(0.5+X)
      K=INT(0.5+Y)
      write(LINE,1)L,K
    1 format('(',I5,'-',I5,')')
      goto 11

    9 if ( X .gt. 50.0 .or. Y .gt. 50.0 ) goto 4
      write(LINE,12)X,Y
   12 format('(',F5.1,'-',F5.1,')')
      goto 11

   10 if ( X .gt. 5.0 .or. Y .gt. 5.0 ) goto 9
      write(LINE,13)X,Y
   13 format('(',F5.2,'-',F5.2,')')
      goto 11

   20 if ( X .gt. 0.5 .or. Y .gt. 0.5 ) goto 10
      write(LINE,23)X,Y
   23 format('(',F5.3,'-',F5.3,')')
      goto 11

   30 if  (X .gt. 0.05 .or. Y .gt. 0.05 ) goto 20
      write(LINE,33)X,Y
   33 format('(',F5.4,'-',F5.4,')')

   11 read(LINE,8)A
    8 format(13A1)

      do 2 I = 2, 13
      if ( A(I) .ne. ' ' ) goto 3
      A(I) = A(I-1)
      A(I-1) = ' '
    2 continue

    3 continue
      if (A(8).NE.' ') goto 5
      do J=8,12
      A(J)=A(J+1)
      enddo
      A(13)=' '
      goto 3
    5 continue

      do 50 I = 1, 13
	if ( A(13) .ne. ' ') goto 51
      do K = 1, 12
      L = 13 - K + 1
      A(L) = A(L-1)
      enddo
      A(1) = ' '
   50 continue
   51 continue

      write(LINE,7)A
    7 format(13A1)
      
      return
      end

      
      
      subroutine assem1(X,Y,LINE)
      character *16LINE
      character *1A(16)
      
      LINE='             '

    4 L=INT(0.5+X)
      K=INT(0.5+Y)
      write(LINE,1)L,K
    1 format('(',I5,' to ',I5,')')
      goto 11

    9 if ( X .gt. 50.0 .or. Y .gt. 50.0 ) goto 4
      write(LINE,12)X,Y
   12 format('(',F5.1,' to ',F5.1,')')
      goto 11

   10 if ( X .gt. 5.0 .or. Y .gt. 5.0 ) goto 9
      write(LINE,13)X,Y
   13 format('(',F5.2,' to ',F5.2,')')
      goto 11

   20 if ( X .gt. 0.5 .or. Y .gt. 0.5 ) goto 10
      write(LINE,23)X,Y
   23 format('(',F5.3,' to ',F5.3,')')
      goto 11

   30 if  (X .gt. 0.05 .or. Y .gt. 0.05 ) goto 20
      write(LINE,33)X,Y
   33 format('(',F5.4,' to ',F5.4,')')

   11 read(LINE,8)A
    8 format(16A1)

      do 2 I = 2, 16
      if ( A(I) .ne. ' ' ) goto 3
      A(I) = A(I-1)
      A(I-1) = ' '
    2 continue
    3 continue
      
      if (A(11).NE.' ') goto 5
      do J=11,16
      A(J)=A(J+1)
      enddo
      A(16)=' '
      goto 3
    5 continue

      do 50 I = 1, 16
	if ( A(16) .ne. ' ') goto 51
      do K = 1, 15
      L = 16 - K + 1
      A(L) = A(L-1)
      enddo
      A(1) = ' '
   50 continue
   51 continue

      write(LINE,7)A
    7 format(16A1)
      
      return
      end


      subroutine assem2(X,Y,LINE)
      character *11LINE
      character *1A(11)
      
      LINE='         '

      if ( X .gt. 10.0 .or. Y .gt. 10.0 ) then
      L=INT(0.5+X)
      K=INT(0.5+Y)
      write(LINE,1)L,K
    1 format('(',i3,' -',i4,')')
      goto 11
      endif
      
      if ( X .gt. 1.0 .and. Y .gt. 10.0 ) then
      K=INT(0.5+Y)
      write(LINE,10)X,Y
   10 format('(',F3.1,' -',i4,')')
      goto 11
      endif

      if ( X .gt. 1.0 .or. Y .gt. 1.0 ) then
      write(LINE,12)X,Y
   12 format('(',F3.1,' -',F4.1,')')
      goto 11
      endif

      if ( X .gt. 0.1 .and. Y .gt. 1.1 ) then
      write(LINE,13)X,Y
   13 format('(',F3.2,' -',F4.1,')')
      goto 11
      endif

      write(LINE,14)X,Y
   14 format('(',F3.2,' -',F4.2,')')

   11 read(LINE,8)A
    8 format(11A1)

      do 2 I = 2, 11
      if ( A(I) .ne. ' ' ) goto 3
      A(I) = A(I-1)
      A(I-1) = ' '
    2 continue
    3 continue
      
      if (A(11).NE.' ') goto 5
      do J=8,11
      A(J)=A(J+1)
      enddo
      A(11)=' '
      goto 3
    5 continue

      do 50 I = 1, 11
      if ( A(11) .ne. ' ') goto 51
      do K = 1, 10
      L = 11 - K + 1
      A(L) = A(L-1)
      enddo
      A(1) = ' '
   50 continue
   51 continue

      write(LINE,7)A
    7 format(11A1)
      
      return
      end
      
      
      subroutine assem3(X,LINE)
      character *6LINE
      
      LINE='      '
      write(LINE,1)X
    1 format(f6.1)
      if ( X .lt. 1.0 )write(LINE,2)X 
    2 format(f6.2)
      
      return
      end


      
*     routine to return T-STATISTIC FROM THE SHIFTED T-DISTRIBUTION ------------

*     the returned value is used to clalculate a control level which if compared 
*     with the standard ensures that there is a probability of "PROB" of true 
*     failure...
      function tshift (EN,KPROB,SIGN)
      real LAMBDA,KPROB
      
*     EN is number of samples --------------------------------------------------
*     KPROB defines the normal deviate for the percentile ----------------------
*     SIGN will be -1,0 for 5 and 10-percentiles -------------------------------

      EF = EN - 1.0 ! degrees of freedom 
      RN = SQRoot(1106,EN)
      EF2 = 2.0 * EF

*     NON-CENTRALITY PARAMETER FOR SHIFTED T-DISTRIBUTION ----------------------
      DELTA = RN * KPROB

*     VARIABLE FOR LOOK-UP TABLE FOR LAMBDA ------------------------------------
      ETA = SIGN*DELTA/SQRoot(1107,EF2*(1.0+DELTA*DELTA/EF2))
      LAMBDA = TABLE1(ETA,EF)
      TSHIFT = SIGN*(SIGN*DELTA+LAMBDA*SQRoot(1108,1.0+DELTA*DELTA/EF2
     &       - LAMBDA*LAMBDA/EF2))/(RN*(1.0-LAMBDA*LAMBDA/EF2))
      
      return
      end
      
      



      function TABLE1(X,Y)
      dimension U(25),V(11),A(11,25)
      DATA V/2.,3.,4.,5.,6.,7.,8.,9.,16.,36.,144./
      DATA U/-1.00,-0.95,-0.90,-0.85,-0.80,-0.75,-0.70,-0.65,-0.60,
     &-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,
     &0.9,1.0/
      DATA A/1.4616,1.5039,1.5277,1.5431,1.5542,1.5625,1.5691,
     &       1.5744,1.5952,1.6141,1.6307,
     &       1.4648,1.5037,1.5262,1.5411,1.5519,1.5601,1.5666,
     &       1.5720,1.5930,1.6124,1.6298,
     &       1.4798,1.5108,1.5302,1.5435,1.5534,1.5610,1.5671,
     &       1.5721,1.5924,1.6116,1.6293,
     &       1.5047,1.5242,1.5388,1.5497,1.5580,1.5646,1.5700,
     &       1.5745,1.5931,1.6115,1.6290,
     &       1.5362,1.5422,1.5511,1.5588,1.5651,1.5704,1.5749,
     &       1.5786,1.5951,1.6121,1.6290,
     &       1.5707,1.5631,1.5658,1.5700,1.5742,1.5779,1.5813,
     &       1.5843,1.5980,1.6133,1.6293,
     &       1.6049,1.5853,1.5819,1.5826,1.5845,1.5867,1.5889,
     &       1.5910,1.6017,1.6149,1.6297,
     &       1.6364,1.6075,1.5986,1.5960,1.5957,1.5963,1.5973,
     &       1.5985,1.6060,1.6170,1.6304,
     &       1.6631,1.6285,1.6152,1.6096,1.6072,1.6063,1.6062,
     &       1.6065,1.6108,1.6194,1.6312,
     &       1.6984,1.6641,1.6456,1.6357,1.6300,1.6265,1.6244,
     &       1.6230,1.6213,1.6251,1.6333,
     &       1.7101,1.6888,1.6701,1.6582,1.6505,1.6453,1.6417,
     &       1.6390,1.6322,1.6313,1.6359,
     &       1.7045,1.7023,1.6874,1.6758,1.6675,1.6614,1.6569,
     &       1.6535,1.6437,1.6378,1.6387,
     &       1.6892,1.7065,1.6979,1.6883,1.6805,1.6744,1.6696,
     &       1.6658,1.6525,1.6442,1.6417,
     &       1.6700,1.7040,1.7025,1.6959,1.6894,1.6839,1.6793,
     &       1.6755,1.6611,1.6503,1.6447,
     &       1.6501,1.6970,1.7024,1.6994,1.6947,1.6902,1.6862,
     &       1.6828,1.6682,1.6558,1.6477,
     &       1.6312,1.6876,1.6991,1.6995,1.6969,1.6937,1.6905,
     &       1.6875,1.6739,1.6607,1.6504,
     &       1.6141,1.6769,1.6934,1.6970,1.6965,1.6947,1.6924,
     &       1.6901,1.6780,1.6647,1.6529,
     &       1.5990,1.6660,1.6864,1.6927,1.6941,1.6936,1.6923,
     &       1.6907,1.6807,1.6678,1.6551,
     &       1.5861,1.6553,1.6785,1.6872,1.6902,1.6909,1.6906,
     &       1.6897,1.6819,1.6699,1.6568,
     &       1.5751,1.6452,1.6705,1.6808,1.6853,1.6870,1.6875,
     &       1.6873,1.6818,1.6711,1.6581,
     &       1.5661,1.6361,1.6625,1.6741,1.6796,1.6823,1.6834,
     &       1.6838,1.6805,1.6714,1.6588,
     &       1.5590,1.6280,1.6549,1.6673,1.6736,1.6769,1.6786,
     &       1.6795,1.6782,1.6707,1.6590,
     &       1.5536,1.6210,1.6479,1.6607,1.6674,1.6712,1.6734,
     &       1.6746,1.6750,1.6692,1.6586,
     &       1.5497,1.6153,1.6416,1.6544,1.6613,1.6654,1.6678,
     &       1.6693,1.6710,1.6667,1.6576,
     &       1.5470,1.6160,1.6362,1.6487,1.6556,1.6597,1.6622,
     &       1.6638,1.6664,1.6635,1.6560/

      if (Y .lt. 2.0) then
      write(33,*)'Error in Function TABLE1/tshift ...',Y
      write(01,*)'Error in Function TABLE1/tshift ...',Y
      write(09,*)'Error in Function TABLE1/tshift ...',Y
      TABLE1 = 0.0
      return
      endif
      
      IU=1
      do 10 I=1,25
      if (U(I) .gt. X) goto 11
      IU=I
   10 continue
   11 JU=IU+1

      if ( Y .gt. 9.0 ) goto 12
      IV=INT(Y)-1
      goto 13
   12 IV=1
      do 14 I=8,11
      if (V(I) .gt. Y) goto 17
      IV=I
   14 continue

   13 if ( IU .gt. 1 .and. IU .lt. 25 ) goto 16
      TABLE1 = A(IU,IV)
      return
   16 P=(X-U(IU))/(U(JU)-U(IU))
      TABLE1 = (1.0-P)*A(IV,IU)+P*A(IV,JU)
      return

   17 JV=IV+1
      A00=A(IV,IU)
      A01=A(IV,JU)
      A10=A(JV,IU)
      A11=A(JV,JU)

      P=(X-U(IU))/(U(JU)-U(IU))
      T=(12.0/SQRoot(1112,Y)-12.0/SQRoot(1109,V(IV)))/(12.0
     &/SQRoot(1110,V(JV))-12.0/SQRoot(1111,V(IV)))
      TABLE1 = (1.0-P)*(1.0-T)*A00+T*(1.0-P)*A10+T*P*A11+(1.0-T)*P*A01

      return
      end


*     Evaluation of cumulative probabilities and percentage points of the ------
*     Student's T-distrinution -------------------------------------------------
*     --------------------------------------------------------------------------
*     Two-tail test
*     --------------------------------------------------------------------------
*     evaluate the T-statistic, given the percentage point ---------------------
      function tdist1 (EN,P)
      Q = P
      if (P .lt. 0.5) Q = 1.0 - P
      Q= 2.0 * Q - 1.0
      tdist1 = tdist (1, EN, Q)
      if (P .lt. 0.5) tdist1=-tdist1
      return
      end


*     evaluate the percentage point, given the T-statistic ---------------------
      function tdist2(EN,T)
      P = tdist( 0, EN, T)
      P = 0.5 * ( 1.0 + P )
      if (T .lt. 0.0) P = 1.0 - P
      tdist2 = P
      return
      end

      function tdist(IX,A,X)
      DATA A2/.5/
      A1 = A/2.
      if ( IX .eq. 1 ) goto 20
      Y = X*X
      Y = Y/(A+Y)
      tdist = BETA(0,Y,A2,A1)
      return
   20 if ( ABS(X) .lt. 0.5 ) goto 40
      FX=BETA(1,1.-X,A1,A2)
      FX=amax1 (FX,A*1.E-30)
      tdist=SQRoot(1113,(1./FX-1.)*A)
      return
   40 FX=BETA(1,X,A2,A1)
      tdist=SQRoot(1114,(FX/(1.-FX))*A)
      return
      end

*     incomplete BETA function and its inverse ---------------------------------
      function BETA(IND,X,A,B)
      if ( X .gt. 0.0 ) goto 4
      BETA=0.0
      return
    4 if ( X .lt. 1.0 ) goto 6
      BETA=1.0
      return
    6 continue
      CAB=CGAM(A+B)-CGAM(A)-CGAM(B)-.5*alog((A+B)*6.28318531)
      if ( IND ) 10,10,20
   10 EP=CAB+A*alog(X*(1.+B/A))+B*alog((1.-X)*(1.+A/B))
      if (X-A/(A+B))12,12,14
   12 BETA=ZI(X,A,B)*exp (EP+.5*alog(B/A))
      return
   14 BETA=1.-ZI(1.-X,B,A)*exp (EP+.5*alog(A/B))
      return
   20 if ( X-0.5 )22,22,24
   22 QZ=alog(X)
      IGO=1
      AA=A
      BB=B
      goto 26
   24 QZ=alog(1.0-X)
      IGO=2
      AA=B
      BB=A
   26 XT=AA/(AA+BB)
      CABB=CAB+.5*alog(BB/AA)+AA*alog(1.+BB/AA)+BB*alog(1.+AA/BB)
      do 40 NC=1,100
      ZZ=ZI(XT,AA,BB)
      QX=CABB+AA*alog(XT)+BB*alog(1.-XT)+alog(ZZ)
      XC=(QZ-QX)*(1.0-XT)*ZZ/AA
      XC=amax1 (XC,-0.99)
      XC=AMIN1(XC,0.5/XT-0.5)
      XT=XT*(1.0+XC)
      if (ABS(XC)-1.E-6)42,40,40
   40 continue
   42 goto (44,46),IGO
   44 BETA=XT
      return
   46 BETA=1.-XT
      return
      end

*     AUXILLARY SUBPROGRAMS FOR 'BETA' -----------------------------------------
      function ZI(X,A,B)
      FN=.7*(alog(15.+A+B))**2+amax1 (X*(A+B)-A,0.)
      N=INT(FN)
      C=1.-(A+B)*X/(A+2.*FN)
      ZI=2./(C+SQRoot(1116,C**2-4.*FN*(FN-B)*X/(A+2.*FN)**2))
      do J=1,N
      FN=N+1-J
      A2N=A+2.*FN
      ZI=(A2N-2.)*(A2N-1.-FN*(FN-B)*X*ZI/A2N)
      ZI=1./(1.-(A+FN-1.)*(A+FN-1.+B)*X/ZI)
      enddo
      return
      end

      function CGAM(A)
      AA=A
      CAC=0.0
      if (A-2.)2,8,8
    2 if (A-1.)4,6,6
    4 CAC=-2.+(A+.5)*alog(1.+1./A)+(A+1.5)*alog(1.+1./(A+1.))
      AA=A+2.
      goto 8
    6 CAC=-1.+(A+.5)*alog(1.+1./A)
      AA=A+1.0
    8 CA=2.269489/AA
      CA=0.52560647/(AA+1.0115231/(AA+1.5174737/(AA+CA)))
      CA=.083333333/(AA+.033333333/(AA+.25238095/(AA+CA)))
      CGAM=CA+CAC
      return
      end

      
      
*     update the integrated sampling rate (QUALN) ------------------------------
      subroutine get sampling rates for river quality ! ------------------------
     &(RFMn,RCMn,QUALNset,EFMn,ECMn,ECN,JDIST,KMIX)
      
      Dd1 = RFMn * RCMn
      if ( Dd1 .lt. 0.00001 .and. KMIX .eq. 0 ) then
      QUALNset = ECN
      return
      endif
      
      if ( JDIST .eq. 6 .or. JDIST .eq. 7 .or. JDIST .eq. 11 ) then
      Dd2 = ECMn 
      else
      Dd2 = EFMn * ECMn
      endif
 
      Dd3 = Dd1 + Dd2
      if (Dd3 .lt. 1.0E-8) return
      
      qqq = ( QUALNset * Dd1 + ECN * Dd2) / Dd3
      QUALNset = amin1 (amax1 (5.9,qqq), 500.0)

      return ! -----------------------------------------------------------------
      end ! --------------------------------------------------------------------
     
      
*     update the integrated sampling rate (QUALN) ------------------------------
      subroutine calculate sampling rates for river quality  
     &(RFMn,RCMn,QUALNset,EFMn,ECMn,ECN,JDIST) ! -------------------------------
      
      Dd1 = RFMn * RCMn
      
      if ( JDIST .eq. 6 .or. JDIST .eq. 7 .or. JDIST .eq. 11 ) then
      Dd2 = ECMn 
      else
      Dd2 = EFMn * ECMn
      endif
 
      Dd3 = Dd1 + Dd2
      if (Dd3 .lt. 1.0E-8) return
      
      qqq = ( QUALNset * Dd1 + ECN * Dd2) / Dd3
      QUALNset = amin1 (amax1 (5.9,qqq), 500.0)

      return ! -----------------------------------------------------------------
      end ! --------------------------------------------------------------------
 
      
      
      

     
      
*     calculate loads and the summary statistics of load -----------------------
      subroutine load calculation
      include 'COMMON DATA.FOR'
      dimension Y(MS),Pa(MS),Pb(MS)

      do 1 jdet = 1, NDET ! ====================================================
      if ( QTYPE (jdet) .eq. 4 ) goto 1
      if ( kdecision (jdet) .eq. 8 ) goto 1
      
*     initialise ---------------------------------------------------------------
      do j13 = 1,N13
      XLOAD (jdet,1,j13) = 0.0
      XLOAD1 (jdet,1,j13) = 0.0 ! dissolved metal
      XLOAD2 (jdet,1,j13) = 0.0 ! solid metal
      NSM (jdet, j13) = 0 ! number of shots per month
      enddo

      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      k13 = imonth + 1
      
      Y(IS) = CMS(jdet,IS)

      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal ~~~~~~~~~~~
      if ( partishun (jdet) .gt. 0.01 ) then
      Pa(IS) = Y(IS) ! for dissolved metal
      Pb(IS) = Y(IS) ! for solid metal
      gkay = 10.0 ** partishun (jdet)
      spm = BMS(2,IS) ! ----------------------------- shots for suspended solids
      totalx = Pa(IS)
      Pa(IS) = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal
      Pb(IS) = totalx - Pa(IS) ! solid metal
      PMS1(jdet,IS) = Pa(IS) ! dissolved metal
      PMS2(jdet,IS) = Pb(IS) ! solid metal
      XADDL1 = Pa(IS) * FMS(IS) ! calculate dissolved metal load of shot
      XADDL2 = Pb(IS) * FMS(IS) ! calculate solid metal load of shot
      XLOAD1 (jdet,1,i13) = XLOAD1 (jdet,1,i13) + XADDL1
      XLOAD1 (jdet,2,i13) = XLOAD1 (jdet,2,i13) + XADDL1 * XADDL1
      XLOAD1 (jdet,1,k13) = XLOAD1 (jdet,1,k13) + XADDL1
      XLOAD1 (jdet,2,k13) = XLOAD1 (jdet,2,k13) + XADDL1 * XADDL1
      XLOAD2 (jdet,1,i13) = XLOAD2 (jdet,1,i13) + XADDL2
      XLOAD2 (jdet,2,i13) = XLOAD2 (jdet,2,i13) + XADDL2 * XADDL2
      XLOAD2 (jdet,1,k13) = XLOAD2 (jdet,1,k13) + XADDL2
      XLOAD2 (jdet,2,k13) = XLOAD2 (jdet,2,k13) + XADDL2 * XADDL2 
      else
      Pa(IS) = Y(is) * FMS(IS) ! dissolved metal
      Pb(IS) = 0.0 ! solid metal
      endif
      endif ! dissolved and solid metal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if ( Y(IS).gt. 0.0000001 .or. FMS (IS).gt. 0.000001 ) then
      Y(IS) = CMS(jdet,IS) * FMS(IS) ! calculate load of each shot
      else
      Y(IS) = 0.0
      endif
      
      xshots(jdet,IS) = Y(IS)
      XADDL = Y(IS)
      
      XLOAD (jdet,1,i13) = XLOAD (jdet,1,i13) + XADDL
      XLOAD (jdet,2,i13) = XLOAD (jdet,2,i13) + XADDL * XADDL
      XLOAD (jdet,1,k13) = XLOAD (jdet,1,k13) + XADDL
      XLOAD (jdet,2,k13) = XLOAD (jdet,2,k13) + XADDL * XADDL
      NSM (jdet,k13) = NSM(jdet,k13) + 1 ! number of shots per month
      enddo ! loop on shots

      XLOAD (jdet,2,i13) = ( XLOAD (jdet,2,i13) - XLOAD (jdet,1,i13)
     &                   *   XLOAD (jdet,1,i13)/NS)/(NS-1)
      if ( XLOAD (jdet,2,i13) .gt. 1.0e-10 ) then
      XLOAD (jdet,2,i13) = SQRoot(1118, XLOAD (jdet,2,i13) )
      else
      XLOAD (jdet,2,i13) = 0.0
      endif
*     calculate the mean load --------------------------------------------------
      XLOAD (jdet,1,i13) = XLOAD (jdet,1,i13) / float (NS)

      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      if ( NSM(jdet,J13) .gt. 1 ) then
      XLOAD (jdet,1,J13) = XLOAD (jdet,1,J13) / float (NSM(jdet,J13))
      endif
      enddo
      endif ! fill monthly loads

      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal
      if ( partishun (jdet) .gt. 0.01 ) then
      XLOAD1 (jdet,2,i13) = ( XLOAD1 (jdet,2,i13) - XLOAD1 (jdet,1,i13)
     &                    *   XLOAD1 (jdet,1,i13)/NS)/(NS-1)
      if ( XLOAD1 (jdet,2,i13) .gt. 1.0e-10 ) then
      XLOAD1 (jdet,2,i13) = SQRoot(1118, XLOAD1 (jdet,2,i13) )
      else
      XLOAD2 (jdet,2,i13) = 0.0 ! for dissolved metal
      endif
      XLOAD2 (jdet,2,i13) = ( XLOAD2 (jdet,2,i13) - XLOAD2 (jdet,1,i13)
     &                    *   XLOAD2 (jdet,1,i13)/NS)/(NS-1)
      if ( XLOAD2 (JDET,2,i13) .gt. 1.0e-10 ) then
      XLOAD2 (jdet,2,i13) = SQRoot(1118, XLOAD2 (jdet,2,i13) )
      else
      XLOAD2 (jdet,2,i13) = 0.0! for solid metal
      endif
*     calculate the mean load of partitioned metals ----------------------------
      XLOAD1 (jdet,1,i13) = XLOAD1 (jdet,1,i13) / float (NS) ! dissolved metal
      XLOAD2 (jdet,1,i13) = XLOAD2 (jdet,1,i13) / float (NS) ! for solid metal
      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      if ( NSM(jdet,J13) .gt. 1 ) then
      XLOAD1 (jdet,1,J13) = XLOAD1 (jdet,1,J13) / float (NSM(jdet,J13)) ! dissolved metal
      XLOAD2 (jdet,1,J13) = XLOAD2 (jdet,1,J13) / float (NSM(jdet,J13))! for solid metal
      endif
      enddo
      endif ! fill monthly loads
      endif ! if ( partishun (jdet) .gt. 0.01 )
      endif ! dissolved and solid metal

*     compute percentiles of load ----------------------------------------------
      do 2 I = 1,k90
      do J = I + 1,NS
      if (Y(I) .lt. Y(J)) then
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
      endif
      enddo
    2 continue
      Xload (jdet,3,i13) = amax1 (0.0,Y(k95))
      Xload (jdet,4,i13) = amax1 (0.0,Y(k90))
      Xload (jdet,5,i13) = amax1 (0.0,Y(k99))

*     compute percentiles of load for partitioned metals -----------------------
      if ( detype (jdet) .ge. 900 ) then ! dissolved and solid metal
      if ( partishun (jdet) .gt. 0.01 ) then
      do 3 I = 1,k90
      do J = I + 1,NS
      if (Pa(I) .lt. Pa(J)) then ! for dissolved metal
      Ctemp = Pa(I)
      Pa(I) = Pa(J)
      Pa(J) = Ctemp
      endif ! for dissolved metal
      if (Pb(I) .lt. Pb(J)) then ! for solid metal
      Ctemp = Pb(I)
      Pb(I) = Pb(J)
      Pb(J) = Ctemp
      endif ! for solid metal
      enddo
    3 continue
      Xload1 (jdet,3,i13) = amax1 (0.0,Pa(k95)) ! for dissolved metal
      Xload1 (jdet,4,i13) = amax1 (0.0,Pa(k90)) ! for dissolved metal
      Xload1 (jdet,5,i13) = amax1 (0.0,Pa(k99)) ! for dissolved metal
      Xload2 (jdet,3,i13) = amax1 (0.0,Pb(k95)) ! for solid metal
      Xload2 (jdet,4,i13) = amax1 (0.0,Pb(k90)) ! for solid metal
      Xload2 (jdet,5,i13) = amax1 (0.0,Pb(k99)) ! for solid metal
      endif ! if ( partition coeffiecient > 0.01 )
      endif ! dissolved and solid metal
      
    1 continue ! do 1 jdet = 1, NDET ! ========

      return
      end


      
*     calculate loads and the summary statistics of load -----------------------
      subroutine load calculation per determinand
      include 'COMMON DATA.FOR'
      dimension Y(MS),Pa(MS),Pb(MS)
      
*     initialise ---------------------------------------------------------------
      do j13 = 1,N13
      XLOAD (JP,1,j13) = 0.0 ! initialise loads
      XLOAD1 (JP,1,j13) = 0.0 ! dissolved metal
      XLOAD2 (JP,1,j13) = 0.0 ! solid metal
      NSM (JP, j13) = 0 ! number of shots per month
      enddo
      
      if ( QTYPE (JP) .eq. 4 ) return
      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      k13 = imonth + 1
      
      Y(IS) = CMS(JP,IS) ! concentration

      
      if ( detype (JP) .ge. 900 ) then ! dissolved and solid metal DDDDDDDDDDDDD
      if ( partishun (JP) .gt. 0.01 ) then ! DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      Pa(IS) = Y(IS) ! for dissolved metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      Pb(IS) = Y(IS) ! for solid metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      gkay = 10.0 ** partishun (JP) ! DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      spm = BMS(2,IS) ! ----------------------------- shots for suspended solids
      totalx = Pa(IS)
      Pa(IS) = totalx / (1.0 + ((spm*gkay)/1000000.0)) ! dissolved metal DDDDDDD
      Pb(IS) = totalx - Pa(IS) ! solid metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      PMS1(JP,IS) = Pa(IS) ! dissolved metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      PMS2(JP,IS) = Pb(IS) ! solid metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      XADDL1 = Pa(IS) * dfms(IS) ! calculate dissolved metal load of shot DDDDDD
      XADDL2 = Pb(IS) * dfms(IS) ! calculate solid metal load of shot DDDDDDDDDD
      XLOAD1 (JP,1,i13) = XLOAD1 (JP,1,i13) + XADDL1
      XLOAD1 (JP,2,i13) = XLOAD1 (JP,2,i13) + XADDL1 * XADDL1
      XLOAD1 (JP,1,k13) = XLOAD1 (JP,1,k13) + XADDL1
      XLOAD1 (JP,2,k13) = XLOAD1 (JP,2,k13) + XADDL1 * XADDL1
      XLOAD2 (JP,1,i13) = XLOAD2 (JP,1,i13) + XADDL2
      XLOAD2 (JP,2,i13) = XLOAD2 (JP,2,i13) + XADDL2 * XADDL2
      XLOAD2 (JP,1,k13) = XLOAD2 (JP,1,k13) + XADDL2
      XLOAD2 (JP,2,k13) = XLOAD2 (JP,2,k13) + XADDL2 * XADDL2 
      else ! DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      Pa(IS) = Y(is) * dfms(IS) ! dissolved metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      Pb(IS) = 0.0 ! solid metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      endif ! if ( partishun (JP) .gt. 0.01 ) DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      endif ! dissolved and solid metal DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD

      
      Y(IS) = CMS(JP,IS) * dfms(IS) ! calculate load of shot
      xshots (JP,IS) = Y(IS)
      XADDL = Y(IS)


      XLOAD (JP,1,i13) = XLOAD (JP,1,i13) + XADDL
      XLOAD (JP,2,i13) = XLOAD (JP,2,i13) + XADDL * XADDL
      XLOAD (JP,1,k13) = XLOAD (JP,1,k13) + XADDL
      XLOAD (JP,2,k13) = XLOAD (JP,2,k13) + XADDL * XADDL
      NSM (JP,k13) = NSM(JP,k13) + 1 ! number of shots per month
      enddo ! loop on shots
      
      XLOAD (JP,2,i13) = ( XLOAD (JP,2,i13) - XLOAD (JP,1,i13)
     &                   *   XLOAD (JP,1,i13)/NS)/(NS-1)
      if ( XLOAD (JP,2,i13) .gt. 1.0e-10 ) then
      XLOAD (JP,2,i13) = SQRoot(1118, XLOAD (JP,2,i13) )
      else
      XLOAD (JP,2,i13) = 0.0
      endif
*     calculate the annual mean load --------------------------------------------
      XLOAD (JP,1,i13) = XLOAD (JP,1,i13) / float (NS)

      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      if ( NSM(JP,J13) .gt. 1 ) then
      XLOAD (JP,1,J13) = XLOAD (JP,1,J13) / float (NSM(JP,J13))
      endif
      enddo
      endif ! fill monthly loads

      if ( detype (JP) .ge. 900 ) then ! dissolved and solid metal
      if ( partishun (JP) .gt. 0.01 ) then
      XLOAD1 (JP,2,i13) = ( XLOAD1 (JP,2,i13) - XLOAD1 (JP,1,i13)
     &                   *   XLOAD1 (JP,1,i13)/NS)/(NS-1)
      if ( XLOAD1 (JP,2,i13) .gt. 1.0e-10 ) then
      XLOAD1 (JP,2,i13) = SQRoot(1118, XLOAD1 (JP,2,i13) )
      else
      XLOAD2 (JP,2,i13) = 0.0 ! for dissolved metal
      endif
      XLOAD2 (JP,2,i13) = ( XLOAD2 (JP,2,i13) - XLOAD2 (JP,1,i13)
     &                   *   XLOAD2 (JP,1,i13)/NS)/(NS-1)
      if ( XLOAD2 (JP,2,i13) .gt. 1.0e-10 ) then
      XLOAD2 (JP,2,i13) = SQRoot(1118, XLOAD2 (JP,2,i13) )
      else
      XLOAD2 (JP,2,i13) = 0.0! for solid metal
      endif
*     calculate the mean load of partitioned metals ----------------------------
      XLOAD1 (JP,1,i13) = XLOAD1 (JP,1,i13) / float (NS) ! for dissolved metal
      XLOAD2 (JP,1,i13) = XLOAD2 (JP,1,i13) / float (NS) ! for solid metal
      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      if ( NSM(JP,J13) .gt. 1 ) then
      XLOAD1 (JP,1,J13) = XLOAD1 (JP,1,J13) / float (NSM(JP,J13)) ! for dissolved metal
      XLOAD2 (JP,1,J13) = XLOAD2 (JP,1,J13) / float (NSM(JP,J13))! for solid metal
      endif
      enddo
      endif ! fill monthly loads
      endif ! if ( partishun (JP) .gt. 0.01 )
      endif ! dissolved and solid metal

*     compute percentiles of load ----------------------------------------------
      do 2 I = 1,k90
      do J = I + 1,NS
      if (Y(I) .lt. Y(J)) then
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
      endif
      enddo
    2 continue
      Xload (JP,3,i13) = amax1 (0.0,Y(k95))
      Xload (JP,4,i13) = amax1 (0.0,Y(k90))
      Xload (JP,5,i13) = amax1 (0.0,Y(k99))

*     compute percentiles of load for partitioned metals -----------------------
      if ( detype (JP) .ge. 900 ) then ! dissolved and solid metal
      if ( partishun (JP) .gt. 0.01 ) then
      do 3 I = 1,k90
      do J = I + 1,NS
      if (Pa(I) .lt. Pa(J)) then ! for dissolved metal
      Ctemp = Pa(I)
      Pa(I) = Pa(J)
      Pa(J) = Ctemp
      endif ! for dissolved metal
      if (Pb(I) .lt. Pb(J)) then ! for solid metal
      Ctemp = Pb(I)
      Pb(I) = Pb(J)
      Pb(J) = Ctemp
      endif ! for solid metal
      enddo
    3 continue
      Xload1 (JP,3,i13) = amax1 (0.0,Pa(k95)) ! for dissolved metal
      Xload1 (JP,4,i13) = amax1 (0.0,Pa(k90)) ! for dissolved metal
      Xload1 (JP,5,i13) = amax1 (0.0,Pa(k99)) ! for dissolved metal
      Xload2 (JP,3,i13) = amax1 (0.0,Pb(k95)) ! for solid metal
      Xload2 (JP,4,i13) = amax1 (0.0,Pb(k90)) ! for solid metal
      Xload2 (JP,5,i13) = amax1 (0.0,Pb(k99)) ! for solid metal
      endif ! if ( partition coeffiecient > 0.01 )
      endif ! dissolved and solid metal
      
      return
      end

      

*     estimate compliance with a percentile standard ---------------------------
*     special version for any percentile ---------------------------------------
      subroutine calculate compliance with percentile standard
     &(DCM,DCS,EN,DCC,PER,EX,DCP,DCPL,DCPU,KU,iidet)
      include 'COMMON DATA.FOR'

      small number = 1.0E-20 ! for use in tests for zero ------------------------
      EX = 0.0 ! initialise confidence that standard was failed -----------------
      if ( DCM .lt. 1.0e-20 ) return

*     take opportunity to check for strange numbers 
      if ( DCM .gt. 999999.0 .or. DCS .gt. 999999.0 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( chekk infeezible .lt. 3 ) then
      chekk infeezible = chekk infeezible + 1
      call change colour of text (10) ! green
      write( *,8306)uname(KU)
 8306 format('*** Encountered infeasibly big concentrations',6x,'...',
     &7x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      else
*     non batch runs -----------------------------------------------------------
      call change colour of text (20) ! bright red ... error message -----------
      write( *,8866)dname(iidet),DCM,DCS,int(EN),DCC
      call set screen text colour
      write(33,8866)dname(iidet),DCM,DCS,int(EN),DCC
      write(01,8866)dname(iidet),DCM,DCS,int(EN),DCC
      write(09,8866)dname(iidet),DCM,DCS,int(EN),DCC
 8866 format(/110('=')/'Infeasibly big concentrations in assessing ',
     &'compliance for ',a11/
     &'This is probably caused by adding load to ',
     &'near-zero river flows ...  '/
     &'WARNING: mean quality =',F19.1/
     &'   Standard deviation =',F19.1/
     &'    Number of samples =',I19/
     &'        Mean standard =',F19.4/110('=')) ! warning message
      !call stop
      endif ! if ( ifbatch .eq. 1 )
      endif

      if ( dcm .lt. small number ) return
      
      if ( dcs/dcm .lt. 0.0001 ) then
      DCP = DCM
      DCPL = DCM
      DCPU = DCM
      return
      endif
      
*     values of the deviate greater than 'F' will be assumed to ----------------
*     cover 100% of the distribution -------------------------------------------
      big deviate = 4.0
      if ( EN .lt. 3.0 ) EN = 3.0 ! set number of samples to a minimum of 3
      D = errx ( 0.01 * per ) ! standard normal deviate for the percentile

*     mean of the logged variables ---------------------------------------------
      GDCM = alog(DCM/(SQRoot(120065,1.0+DCS*DCS/(DCM*DCM))))
    
*     standard deviation of the logged variables -------------------------------
      BRAK = ALOG(1.0+DCS*DCS/(DCM*DCM))
      if ( BRAK .gt. 1.0e-20 ) then
      GDCS = SQRoot(120146,BRAK)
      endif
    
*     percentile assuming a log-normal distribution ----------------------------
      T0 = D
      DCP = exp ( GDCM + T0 * GDCS )

*     calculate confidence limits ----------------------------------------------
      TL = TSHIFT ( EN, T0, -1.0 )
      TU = TSHIFT ( EN, T0,  1.0 )
      DCPL = exp ( GDCM + TL * GDCS )
      DCPU = exp ( GDCM + TU * GDCS )
*     standard errors ----------------------------------------------------------
      SL = ALOG(DCPL/DCP)/(-errx(0.95))
      SU = ALOG(DCPU/DCP)/( errx(0.95))
      IF ( DCC .lt. small number ) return

*     compare area of Error Distribution that is less than the standard --------
*     this is an estimate of the probability that the standard has been achieved
      if ( DCC .gt. DCP ) goto 95
    
*     calculations where standard is smaller than estimate of the percentile ---
      EX = 100.0
      SS = SL
    
*     standard normal deviate --------------------------------------------------
      Z = DCC/DCP
      if ( Z .lt. small number) goto 32
      T statistic = ALOG(Z)/SL
    
*     CHECK PROBABILITY IS NOT LARGER THAN 99.95 PERCENT -----------------------
      if ( ABS(T statistic) .gt. big deviate ) goto 32
      goto 93
    
*     calculations where the standard is bigger than the estimate of the -------
*     percentile ---------------------------------------------------------------
   95 EX = small number
      SS = SU
      Z = DCC/DCP
      if ( Z .lt. small number ) goto 32
      T statistic = alog(Z) / SU
      if ( abs(T statistic) .gt. big deviate ) goto 32

   93 Probability = TDIST2 (9999., T statistic )
      EX = 100.0 * ( 1.0 - Probability )
    
   32 continue

      if ( per .lt. 50.0 ) EX = 100.0 - EX
      return
      end



*     distribution function of PHI ---------------------------------------------
      function func(X)
      common GPHIM,GPHISD,Crunch
      func=1.+EXP(-GPHIM-GPHISD*X*1.4142135)
      I=Crunch
      goto(100,101,102),I
  100 func=1./func
      return
  101 func=1./(func*func)
      return
  102 func=1./(func*func*func)
      return
      end

*     integration
      function GAHER(func)
      dimension A(5),H(5)
      data A/0.34290133,1.0366108,1.7566836,2.5327317,3.4361591/
      data H/0.61086263,0.24013861,0.03387439,0.0013436457,
     &0.0000076404329/
      SUM=0.0
      DO 1 J=1,5
      V=A(J)
    2 Z=func(V)
      SUM=SUM+H(J)*Z
      if (V .lt. 1.0E-20) goto 1
      V=-V
      goto 2
    1 continue
      GAHER=SUM
      return
      end


      subroutine compute confidence limits 
     &(perc,idist,XM,XS,XP,qualn,XL,XU)

      XL = 0.0
      XU = 0.0
      if ( idist .eq. 0 ) return

*     check for a zero calculated mean -----------------------------------------
      if ( XM .lt. 1.0e-10 .or. XS .lt. 1.0e-10 ) then	
      XS = 0.0 ! set standard deviation to zero --------------------------------
      XL = A ! set percentiles as the calculated mean --------------------------
      XU = A
      return
      endif

      A = XM
      S = XS

*     calculations for the mean (perc = zero ) ---------------------------------
      if ( perc .gt. -0.00001 .and. perc .lt. 0.00001 ) then
      t = errx ( 0.95 )
      xqualn = amax1 ( 0.05, QUALN )
      SEM = S / SQRoot( 107333, xqualn )
*     calculate the 95% confidence limits about the mean -----------------------
      XL = amax1 (0.0, (A - t * SEM) )
      XU = amax1 (0.0, (A + t * SEM) )
      return
      endif ! calculations for the mean (perc = zero ) -------------------------

*     log-normal distribution and percentiles ----------------------------------
      if ( idist .eq. 2 .or. idist .eq. 3 .or. idist .eq. 4) then
      GM = alog ( (A*A) / SQRoot(10778,A*A+S*S) )
      GS = SQRoot(100775, alog ( 1.0 + (S*S) / (A*A) ) )
      if ( perc .ge. 49.999 .and. perc .lt. 99.9999 ) then ! high percentiles --
      t = errx ( 0.01 * perc )
      DP = exp ( GM + t * GS )
      TL = tshift ( QUALN, t, -1.0)
      TU = tshift ( QUALN, t,  1.0)
      XL = amax1 (0.0, XP - (DP - exp ( GM + TL * GS)) ) ! confidence limits ---
      XU = amax1 (0.0, XP + (exp ( GM + TU * GS) - DP) ) ! confidence limits ---
      endif ! high percentiles -------------------------------------------------
      if ( perc .gt. 0.0001 .and. perc .lt. 49.9999 ) then ! low percentiles ---
      t = - errx ( 0.01 * perc )
      DP = exp ( GM + t * GS )
      TL = -tshift ( QUALN, t,  1.0)
      TU = -tshift ( QUALN, t, -1.0)
      XL = amax1 (0.0, DP - (DP - exp ( GM + TL * GS)) ) ! confidence limits ---
      XU = amax1 (0.0, DP + (exp ( GM + TU * GS) - DP) ) ! confidence limits ---
      endif ! low percentiles --------------------------------------------------
      endif ! log-normal distribution and percentiles -------------------------- 

*     normal distributions -----------------------------------------------------
      if ( idist .eq. 1 ) then
      GM = A
      GS = S
      if ( perc .ge. 49.9999 .and. perc .lt. 99.9999 ) then
      t = errx ( 0.01 * perc )
*     calculate the percentile assuming a normal distribution ------------------
      DP = GM + t * GS
*     calculate confidence limits ----------------------------------------------
      TL = tshift ( QUALN, t, -1.0)
      TU = tshift ( QUALN, t,  1.0)
      XL = amax1 (0.0, XP - (DP - ( GM + TL * GS)) )
      XU = amax1 (0.0, XP + (( GM + TU * GS) - DP) )
      endif
*     low percentiles ----------------------------------------------------------
      if ( perc .gt. 0.0001 .and. perc .lt. 49.9999 ) then
      t = - errx ( 0.01 * perc )
      DP = GM + t * GS
      TL = -tshift ( QUALN, t,  1.0)
      TU = -tshift ( QUALN, t, -1.0)
      XL = amax1 (0.0, DP - (DP - ( GM + TL * GS)) )
      XU = amax1 (0.0, DP + (( GM + TU * GS) - DP) )
      endif
      endif 
     
      return
      end
     
      
      
      subroutine diffav (a1,d1,n1,a2,d2,n2,al1,au1,al2,au2,
     &Pinc,Ydec,A12,S12,AL12,AU12) 
      
      Pinc = 0.0
      Ydec = 0.0
      A12 = 0.0
      S12 = 0.0
      AL12 = 0.0
      AU12 = 0.0

      E1 = N1 ! number of samples ------------------------------------ calulated
      V1 = (E1-1.0) * D1*D1 / E1
      T0 = TDIST1(E1-1.0,0.95) ! t-statistic for 95%
      S1 = D1/SQRT(E1) ! the standard error
      AL1 = amax1(0.0,A1-S1*T0) ! the lower confidence limit
      AU1 = A1+S1*T0 ! the upper confidence limit -------------------- calulated

      E2 = N2 ! number of samples ===================================== observed
      V2 = (E2-1.0) * D2*D2/ E2
      T0 = TDIST1(E2-1.0,0.95) ! t-statistic for 95%
      S2 = D2/SQRT(E2) ! the standard error
      AL2 = amax1(0.0,A2-S2*T0) ! lower confidence limit
      AU2 = A2+S2*T0 ! upper confidence limit ========================= observed
   
      Q = (E1*V1+E2*V2)/(E1+E2-2.0)
      W = SQRT(Q)
      Q1 = V1/E1 ! compute degrees of freedom
      Q2 = V2/E2
      Q3 = Q1+Q2
      Q4 = Q1/Q3
      Q5 = Q2/Q3
      Q6 = Q4*Q4/(E1-1.0)
      Q7 = Q5*Q5/(E2-1.0)
      D = 1.0/(Q6+Q7)
      W = W * SQRT(1.0/E1+1.0/E2)
      Pinc = 100.0*TDIST2(D,(A2-A1)/(W))
      Ydec = 100.0*TDIST2(D,(A1-A2)/(W))
      
      A12 = A2-A1
      V1 = D1*D1
      V2 = D1*D2
      S12 = sqrt(V1/E1+V2/E2)
      E12 = E1 + E2 - 2.0
      T0 = TDIST1(E12,0.95)
      AL12 = A12 - T0*W
      AU12 = A12 + T0*W
         
      return
      end
     

*     **************************************************************************
      subroutine check the correlation by regression (kk)
      include 'COMMON DATA.FOR'
      
      if ( kk .eq. 10 ) then
      k1 = 1 ! river flow
      k2 = 2 ! river quality
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      !if ( nobigout .le. 0 ) write(33,1)dname(JP),uname(KFEAT),
      !&RRR,CO1
    1 format(77('-')/'Correlation calculated by ',
     &'regression ... ',a11,' at ',a37/77('-')/
     &'     River flow on river quality =',F7.3,' (',f7.3,')',
     &' CO1')
      endif
      endif ! if ( kk .eq. 10 )

      if ( kk .eq. 2 ) then
      k1 = 1 ! river flow
      k2 = 3 ! added flow
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( abs (RRR-CO2) .gt. 0.045 ) then
      write(33,2)uname(KFEAT),RRR,CO2
    2 format(/77('-')/'Correlation checked by regression at ',
     &a37/77('-')/'Added flow on river flow =',
     &F7.3,' (',f7.3,' expected)'/77('-')/
     &77('-')/'Differences will occur if limited monthly data ',
     &'are imposed ...'/77('-')/)
      endif ! if ( abs (RRR-CO2) .gt. 0.045 )
      endif ! if ( ical13 .eq. 0 )
      endif ! if ( kk .eq. 2 )
      
      k1 = 1 ! river flow
      k2 = 4 ! discharge quality
      !call regres the calculated shots (k1,k2,RRR)
      !if ( ical13 .eq. 0 ) then 
      !if ( nobigout .le. 0 ) write(33,3)RRR
    3 format('     Added quality on river flow =',F7.3)
      !endif
    
      k1 = 3 ! discharge flow
      k2 = 2 ! river quality
      !call regres the calculated shots (k1,k2,RRR)
      !if ( ical13 .eq. 0 ) then 
      !if ( nobigout .le. 0 ) write(33,4)RRR
    4 format('Added flow on river quality =',F7.3)
      !endif
    
      k1 = 3 ! discharge flow
      k2 = 4 ! discharge quality
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( abs (RRR-CO5) .gt. 0.045 ) then
      write(33,5)dname(JP),uname(KFEAT),RRR,CO5
    5 format(/77('-')/'Correlation checked for ... ',
     &a11,' at ',a37/77('-')/'Added quality on added flow =',
     &F7.3,' (',f7.3,' expected)'/
     &77('-')/'Differences will occur if limited monthly data ',
     &'are imposed ...'/77('-')/)
      endif
      endif
    
      k1 = 4 ! discharge quality
      k2 = 2 ! river quality
      !call regres the calculated shots (k1,k2,RRR)
      !if ( ical13 .eq. 0 ) then 
      !if ( nobigout .le. 0 ) write(33,7)RRR
    7 format('  Added quality on river quality =',F7.3/77('-'))
      !endif
    
      return
      end
 
      
      
      subroutine regres the calculated shots (K1,K2,RRR)
      include 'COMMON DATA.FOR'
      dimension xmb(NS),ymb(NS)

      log1 = 1
      log2 = 1
      
      if ( K1 .lt. 0 ) then 
      log1 = 0
      K1 = -K1
      endif
      if ( K2 .lt. 0 ) then
      log2 = 0
      K2 = -K2
      endif

      sumx = 0.0
      sumy = 0.0
      do is = 1, NS   
      if (log1 .eq. 1 ) then
      ymb(is) = alog (amax1(1.0e-10,Creg(K1,is)))
      else
      ymb(is) = Creg(K1,is)
      endif
      if (log2 .eq. 1 ) then
      xmb(is) = alog (amax1(1.0e-10,Creg(K2,is)))
      else
      xmb(is) = Creg(K2,is)
      endif
      sumx = sumx + xmb(is)
      sumy = sumy + ymb(is)
      enddo
      ybar = sumy / float (NS)  
      xbar = sumx / float (NS)  
      
      if ( ybar .lt. -23.0 .and. xbar .lt. -23.0 ) then
      RRR = 0.0
      else
      SXY = 0.0
      SXX = 0.0
      SYY = 0.0
      do i = 1, float (NS)
      YY1 = ymb(i) - ybar
      XX1 = xmb(i) - xbar
      SXY = SXY + YY1 * XX1
      SXX = SXX + XX1 * XX1
      SYY = SYY + YY1 * YY1
      enddo
      RRR = SXY / SQRoot (194443, SXX * SYY)
      endif
   
      return 
      end
         
********************************************************************************