*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ...
*     ==========================================================================
*     File assess compliance.for    1408 lines ---------------------------------
*     --------------------------------------------------------------------------
*     This file deals with the classification of rivers such as under the Water 
*     Framework Directive
*     --------------------------------------------------------------------------
*     This file contains 5 sub-routines 
*     They are called: ---------------------------------------------------------
*     --------------------------------------------------------------------------
*     ...... assess compliance with river targets (KU,kplace)
*     ...... classify (KU,kplace)
*     ...... write the mean standard (ic,idet,targit,kplace)
*     ...... write the percentile standard (ic,idet,targit,xpoint,kplace)
*     ...... write headings and sum compliance distances (kplace)
*     --------------------------------------------------------------------------

      subroutine assess compliance with river targets (KU,kplace)
      include 'COMMON DATA.FOR'
      dimension ifailtest50 (NP), ifailtest95 (NP) 
      character *13 SET1
      character *10 tenstan
      character *2 anu

*     KPLACE is (1) head of reach ....
*               (3) end of reach ....
*               (2) at feature ....
*               (4) d/s of feature ....

      if ( ical13 .eq. 1 ) return 

      if ( DIST(feeture) .lt. 1.0e-08 ) then   
      if ( JT (feeture) .eq. 25 ) return
      if ( JT (feeture) .eq. 27 ) return
      if ( JT (feeture) .eq. 29 ) return
      if ( JT (feeture) .eq. 31 ) return
      if ( JT (feeture) .eq. 33 ) return
      if ( JT (feeture) .eq. 35 ) return
      if ( JT (feeture) .eq. 37 ) return
      if ( JT (feeture) .eq. 40 ) return
      if ( JT (feeture) .eq. 42 ) return ! aggregated STWs (42)
      if ( JT (feeture) .eq. 46 ) return
      if ( JT (feeture) .eq. 48 ) return
      if ( JT (feeture) .eq. 50 ) return
      if ( JT (feeture) .eq. 52 ) return
      if ( JT (feeture) .eq. 54 ) return
      if ( JT (feeture) .eq. 56 ) return
      if ( JT (feeture) .eq. 58 ) return
      endif
      
*     identify any targets entered for the feature number "KU" -----------------
      IQSfeet = IFRQS(KU)

*     check for the existence of reach-specific standards ----------------------
      IQSreach = 0
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      if ( EQS reach (IREACH,idet) .gt. 0 ) then
      IQSreach = EQS reach (IREACH,idet)
      endif
      endif
      enddo ! do idet = 1, ndet
      
      if ( background class .eq. 0 .and. IQSfeet .lt. 1 .and.
     &IQSreach .eq. 0 ) return

*     do the compliance test ... loop on all the determinands ------------------
*     initialise confidence of failure across all determinands -----------------
      exall = 0.0
      jprime = 0
      do 10 idet = 1, ndet
      if ( QTYPE (idet) .eq. 4) goto 10
      
      targit = 0.0 ! initialise the target -------------------------------------
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,idet) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,idet) ! set target for feature
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value rrrrrrr
      do ic = 1, nclass - 1 ! ==================================================
      if ( MRQS(idet). ne. 4 .and. MRQS(idet). ne. 5) then ! -------------------
      if ( class limmits (ic,idet) .lt. -1.0e-8 ) then ! -----------------------
      targit = abs (class limmits (ic,idet))
      endif ! if ( class limmits (ic,idet) .lt. -1.0e-8 ) ----------------------
      endif ! if ( MRQS(idet). ne. 4 etc ---------------------------------------
      enddo ! do ic = 1, nclass - 1 ============================================
      endif ! if ( IQSreach .gt. 0 ) rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      
      RQO(idet) = targit ! use the target for graphs ---------------------------
      
      if ( targit .lt. 1.0e-10 ) goto 10

*     store the mean and standard deviation ------------------------------------
      COMPM = C(idet,1)
      COMPS = C(idet,2)
      if ( DETYPE (idet) .eq. 900 .or.
     &     DETYPE (idet) .eq. 909 ) then ! dissolved metal
      COMPM = cpart1 (idet,1) ! dissolved metal
      COMPS = cpart1 (idet,2) ! dissolved metal
      endif ! dissolved metal

*     initialise the flags for failure -----------------------------------------
*     failure with at least 50% confidence -------------------------------------
      ifailtest50 (idet) = 0
*     failure with at least 95% confidence -------------------------------------
      ifailtest95 (idet) = 0
*     confidence of failure for the determinand --------------------------------
      confail (idet) = 0.0 ! confidence of failure
      EX = 0.0 ! initial value of confidence of failure

      if ( qtype (idet) .eq. 4 ) goto 10

*     identify the summary statistic for the river quality target --------------
*     Kstat is 1, 2, 3 for mean, 95-percentile and 90-percentile ---------------
*     Kstat is 4 and 5 for the 5 and 10-percentile -----------------------------
*     Kstat is 6 for the 99-percentile -----------------------------------------
      call check summary statistic for standard (idet)
      Kstat = MRQS (idet)
      lstat = 1
      if ( kstat .gt. 0 .and. kstat .lt. 7 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( kstat .eq. 1 ) lstat = 1 ! mean
      if ( kstat .eq. 2 ) lstat = 3 ! 95-percentile
      if ( kstat .eq. 3 ) lstat = 4 ! 90-percentile
      if ( kstat .eq. 4 ) lstat = 3 ! 5-percentile
      if ( kstat .eq. 5 ) lstat = 4 ! 10-percentile
      if ( kstat .eq. 6 ) lstat = 5 ! 99-percentile
      else ! if ( kstat .gt. 0 .and. kstat .lt. 7 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      kstat = 0
*     set 95-percentile target if it is not set with the determinand data ------
      if ( output mode .eq. 0 ) then ! 95-percentile ===========================
      kstat = 2
      lstat = 3
      endif ! 95-percentile target =============================================
*     mean target if not set with determinand data +++++++++++++++++++++++++++++
      if ( output mode .eq. 1 ) then ! mean target =============================
      kstat = 1
      lstat = 1
      endif ! mean target ======================================================
*     90-percentile target if not set with determinand data ++++++++++++++++++++
      if ( output mode .eq. 2 ) then ! 90-percentile ===========================
      kstat = 3
      lstat = 3
      endif ! 90-percentile target =============================================
*     99-percentile target if not set with determinand data ++++++++++++++++++++
      if ( output mode .eq. 3 ) then ! 99-percentile ===========================
      kstat = 6
      lstat = 5
      endif ! 99-percentile target =============================================
      endif ! if ( kstat .gt. 0 .and. kstat .lt. 7 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if ( DETYPE (idet) .eq. 900 .or.
     &     DETYPE (idet) .eq. 909 ) then ! dissolved metal
      test = cpart1 (idet, lstat ) ! dissolved metal
      else 
      test = C (idet, lstat )
      endif 

      if ( jprime .eq. 0 ) then
      call write headings and sum compliance distances (kplace)
      endif
      jprime = 1
      
      testq = 0.0
      qualnuse = qualn(idet) ! set up the effective sampling rate --------------
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
      if ( kstat .gt. 0 .and. kstat .lt. 7 ) then ! ++++++++++++++++++++++++++++
          
*     standards expressed as a mean ----------------------------------------mean
      if ( kstat .eq. 1 ) then ! ===========================================mean
      call write the mean standard (01, idet, targit)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the mean standard (31, idet, targit)
      endif ! JT (feeture) .eq. 03 
      call calculate compliance with mean standard (COMPM,COMPS,
     &qualnuse,targit,EX,testq,testql,testqu,KU,idet)
      testq = COMPM
      testql = amax1 (0.0,testql)
      endif ! if ( kstat .eq. 1 ) ==========================================mean
  
    
*     95-percentile target ===================================================95
      if ( kstat .eq. 2 ) then
      xpoint=95.0
      call write the percentile standard (01,idet,targit,xpoint) ! -----------95
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the percentile standard (31, idet, targit, xpoint) ! -------95
      endif ! JT (feeture) .eq. 03 
      if ( COMPS/COMPM .gt. 0.0001 .and. COMPM .gt. 0.0000001 ) then
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------95
     &tests,qualnuse,targit,xpoint,EX,testq,testql,testqu,KU,idet)
      endif ! if ( COMPS/COMPM .gt. 0.0001
      endif ! if ( kstat .eq. 2 ) ============================================95


*     99-percentile target ===================================================99
      if ( kstat .eq. 6 ) then
      xpoint=99.0
      call write the percentile standard (01, idet, targit, xpoint) ! --------99
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the percentile standard (31, idet, targit, xpoint) ! -------99
      endif
      if ( COMPS/COMPM .gt. 0.0001 .and. COMPM .gt. 0.0000001 ) then
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------99
     &tests,qualnuse,targit,xpoint,EX,testq,testql,testqu,KU,idet)
      endif ! if ( COMPS/COMPM .gt. 0.0001
      endif ! if ( kstat .eq. 6 ) ============================================99


*     90-percentile target ===================================================90
      if ( kstat .eq. 3 ) then
      xpoint=90.0
      call write the percentile standard (01, idet, targit, xpoint) ! --------90
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the percentile standard (31, idet, targit, xpoint) ! -------90
      endif
      if ( COMPS/COMPM .gt. 0.0001 .and. COMPM .gt. 0.0000001 ) then
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------90
     &tests,qualnuse,targit,xpoint,EX,testq,testql,testqu,KU,idet)
      endif ! if ( COMPS/COMPM .gt. 0.0001
      endif ! if ( kstat .eq. 3 ) ============================================90

      
*     5-percentile target ====================================================05
      if ( kstat .eq. 4 ) then
      xpoint=5.0
      call write the percentile standard (01, idet, targit, xpoint) ! --------05
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the percentile standard (31, idet, targit, xpoint) ! -------05
      endif
*     estimate mean and standard deviation for percentile --------------------05
      if ( COMPS/COMPM .gt. 0.0001 .and. COMPM .gt. 0.0000001 ) then
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------05
     &tests,qualnuse,targit,xpoint,EX,testq,testql,testqu,KU,idet)
      endif ! if ( COMPS/COMPM .gt. 0.0001
      endif ! if ( kstat .eq. 4 ) ============================================05

      
*     10-percentile target ===================================================10
      if ( kstat .eq. 5 ) then
      xpoint=10.0
      call write the percentile standard (01, idet, targit, xpoint) ! --------10
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !call write the percentile standard (31, idet, targit, xpoint) ! -------10
      endif
*     estimate mean and standard deviation for percentile --------------------10
      if ( COMPS/COMPM .gt. 0.0001 .and. COMPM .gt. 0.0000001 ) then
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------10
     &tests,qualnuse,targit,xpoint,EX,testq,testql,testqu,KU,idet)
      endif ! if ( COMPS/COMPM .gt. 0.0001
      endif ! if ( kstat .eq. 5 ) 10-percentile target =======================10
      
      
      if ( targit .gt. 1.0e-10) then ! =========================================
      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 1 (targit)
      tenstan = valchars10
      call assem (testql,testqu,SET1)
      call sort format 3 (testq,qualn(idet),EX)
      
      if ( testq .gt. 0.000001 ) then ! ========================================

      if ( kstat .eq. 1 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ===============
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      !write(01,701)dname(idet),tenstan,units(idet) ! ---------------- OUT
      write(31,701)dname(idet),tenstan,units(idet) ! ----------------- EFF
  701 format(a11,30x,'      Mean standard =',a10,1x,a4)
      endif ! if ( JT (feeture) .eq. 03 etc ====================================
     
      else ! if ( kstat .eq. 1 ) else ------------------------------------------ 
      anu = '95'
      if ( kstat .eq. 3 ) anu = '90'
      if ( kstat .eq. 4 ) anu = '05'
      if ( kstat .eq. 5 ) anu = '10'
      if ( kstat .eq. 6 ) anu = '99'
      
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ===============
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      write(31,702)dname(idet),anu,tenstan,units(idet) ! ------------------- EFF
  702 format(a11,27x,a2,'-percentile standard =',a10,1x,a4)
      endif ! if ( JT (feeture) .eq. 03 etc ====================================
     
      endif ! if ( kstat .eq. 1 ) ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      write(01,70)valchars10,units(idet),SET1,valchars12 ! ----------------- OUT
   70 format(46x,'Test statistic =',a10,1x,a4,4x,a13/ 
     &21x,'Confidence that the standard was failed =',a10,' %'/110('~'))
      
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ~~~~~~~~~~~~~~~
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,270)valchars10,units(idet),SET1,valchars12 ! ---------------- EFF
  270 format(46x,'Test statistic =',a10,1x,a4,4x,a13/ 
     &21x,'Confidence that the standard was failed =',a10,' %'/110('~'))
      endif ! if ( kplace .ne. 1 .and. kplace .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
      else ! if ( testq .gt. 0.000001 ) ========================================
          
      write(01,78)dname(idet),tenstan,units(idet),valchars12 ! ------------- OUT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! eeeeeeeeeeeeeee
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,78)valchars10,units(idet),SET1,valchars12 ! ----------------- EFF
   78 format(46x,'Test statistic =',a10,1x,a4,4x,a13/
     &21x,'Confidence that the standard was failed =',a10,' %'/110('*'))
      endif ! if ( kplace .ne. 1 .and. kplace .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc 
      endif ! if ( testq .gt. 0.000001 ) =======================================
      endif ! if ( nobigout .le. 0 ) ===========================================
      confail (idet) = ex
      
      endif ! if ( targit .gt. 1.0e-10) ========================================

      if ( targit  .gt. 1.0e-10 ) then
      if ( kstat .gt. 0 ) then
      if ( ex .gt. 50.0 ) ifailtest50 (idet) = 1
      if ( ex .gt. 95.0 ) ifailtest95 (idet) = 1
      ee = ex
      cc = 100.0 - conf of class(2)
      endif
      endif

*     sum lengths of river NOT failing at 50 per cent confidence ~~~~~~~~~~~~~~~
      if ( kplace .ne. 4 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( ifailtest50 (idet) .eq. 0 ) then
      Total length dets 00 (idet) = Total length dets 00 (idet) + distp
      endif
*     sum lengths of river failing with 50 per cent confidence -----------------
      if ( ifailtest50 (idet) .eq. 1 ) then
      Total length dets 50 (idet) = Total length dets 50 (idet) + distp
      endif
*     sum lengths of river failing with 95 per cent confidence -----------------
      if ( ifailtest95 (idet) .eq. 1 ) then
      Total length dets 95 (idet) = Total length dets 95 (idet) + distp
      endif 
      endif ! if ( kplace .ne. 4 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      endif ! if ( kstat .gt. 0 .and. kstat .lt. 7 ) +++++++++++++++++++++++++++
      

*     list compliance for each determinand -------------------------------------
      if ( ical13 .eq. 0  ) then ! if not setting up gap filling
      if ( nobigout .le. 0 ) then
      if ( kplace .eq. 2 .or. kplace .eq. 4 ) then ! -- not the end of the reach
      write(30,3106) uname(KU),Dname(idet), ! ------------------------------ TGT
     &Total river length,Total RQS length 1,test,targit,MRQS(idet),
     &Total length dets 00(idet),Total length dets 50(idet),
     &Total length dets 95(idet),EX
 3106 format(a32,a16,2f6.1,f7.2,f7.2,i2,3f9.3,f11.2,' %')
      endif ! if ( kplace .ne. 2 or 4 ) not the end of the reach
      
      if ( kplace .eq. 3 ) then ! ========================= the end of the reach
      write(30,3266) Rname(IREACH),Dname(idet), ! -------------------------- TGT
     &Total river length,Total RQS length 1,test,targit,MRQS(idet),
     &Total length dets 00(idet),Total length dets 50(idet),
     &Total length dets 95(idet),EX
 3266 format('End of reach: ',a16,2x,a16,2f6.1,f7.2,f7.2,i2,3f9.3,
     &f11.2,' %')
      endif ! if ( kplace .ne. 3 ) ======================== the end of the reach
      
      if ( kplace .eq. 1 ) then ! ======================= the start of the reach
      write(30,3166) Rname(IREACH),Dname(idet), ! -------------------------- TGT
     &Total river length,Total RQS length 1,test,targit,MRQS(idet),
     &Total length dets 00(idet),Total length dets 50(idet),
     &Total length dets 95(idet),EX
 3166 format('Start reach: ',a16,3x,a16,2f6.1,f7.2,f7.2,i2,3f9.3,
     &f11.2,' %')
      endif ! if ( kplace .eq. 1 ) ====================== the start of the reach 
      
      endif ! if ( nobigout .le. 0 )
      exall = amax1 ( exall,  ex )
      endif !  if ( ical13 .eq. 0  )
   10 continue ! do 10 idet = 1, ndet ... ================= loop on determinands

      if ( kplace .ne. 4 ) then

*     the assessment is done - all the determinands have been processed---------
*     sum the total length in failure for any determinand ----------------------
      ifailany = 0
      do 2200 idet = 1, ndet ! -------------------------------------------------
      if ( qtype (idet) .ne. 4 ) then
      if ( ifailany .eq. 0 ) then
*     exclude determinands from the overall totals -----------------------------
      !if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 2200
      !if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 2200
      !if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 2200
      !if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 2200
      !if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 2200

*     face value failures ------------------------------------------------------
      if ( ifailtest50 (idet) .eq. 1 ) then
      Total length 50 = Total length 50 + distp
      goto 2206
      endif ! if ( ifailtest50 (idet) .eq. 1 )
      endif ! if ( ifailany .eq. 0 )
      endif ! if ( qtype (idet) .ne. 4 )
 2200 continue ! idet = 1, ndet ------------------------------------------------
 2206 continue

*     now sum the compliant length ---------------------------------------------
      if ( ifailany .eq. 0 ) then
      Total length 00 = Total length 00 + distp
      endif ! if ( ifailany .eq. 0 )

*     statistically significant failures ---------------------------------------
*     sum the total length in failure for any determinand ----------------------
      do 48 idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
*     exclude determinands from the overall totals -----------------------------
      !if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 48
      !if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 48
      !if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 48
      !if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 48
      !if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 48
      if ( ifailtest95(idet) .eq. 1 ) then
      Total length 95 = Total length 95 + distp
      goto 49
      endif ! if ( ifailtest95(idet) .eq. 1 )
      endif ! if ( qtype (idet) .ne. 4 )
   48 continue ! loop on determinands
   49 continue
      endif ! if ( kplace .ne. 4 )

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(30,26) Total river length, ! ------------ TGT
     &Total length 2, 
     &Total length 00,Total length 50, Total length 95,exall
   26 format(116('-')/37x,'All...',f11.1,f6.1,16x,3f9.3,f11.3,
     &' %'/116('-'))
      endif ! if ( ical13 .eq. 0 )
      
      write(01,5449)
 5449 format(/)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ---------------
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( IBR .eq. 3 ) write(31,5549)
 5549 format(110('+')/)
      endif

      return
      end   





*     put the rivers into classes ----------------------------------------------
      subroutine classify (KU,kplace)
      include 'COMMON DATA.FOR'
      dimension worse than (nclass, MP10)
      dimension better than (nclass, MP10)
      
*     KPLACE is (1) for head of Reach ....
*               (3) for end of Reach ....
*               (2) at (or immediately upstream) of Feature ....
*               (4) downstream of Feature ....

      real in or better (nclass, MP10)
      real in or worse (nclass, MP10)
      integer class at 50, Face determinand
      integer noclass (MP10)

      if ( ical13 .eq. 1 ) return ! --------------------------------------------

      if ( virtualreach .eq. 1 ) return
      if ( background class .eq. 0 .and. IQSfeet .lt. 1 .and.
     &IQSreach .eq. 0 ) return

      
      !if ( background class .eq. 0 ) return
   
      if ( DIST(feeture) .lt. 1.0e-08 ) then   
      if ( JT (feeture) .eq. 25 ) return
      if ( JT (feeture) .eq. 27 ) return
      if ( JT (feeture) .eq. 29 ) return
      if ( JT (feeture) .eq. 31 ) return
      if ( JT (feeture) .eq. 33 ) return
      if ( JT (feeture) .eq. 35 ) return
      if ( JT (feeture) .eq. 37 ) return
      if ( JT (feeture) .eq. 40 ) return
      if ( JT (feeture) .eq. 42 ) return ! aggregated STWs (42)
      if ( JT (feeture) .eq. 46 ) return
      if ( JT (feeture) .eq. 48 ) return
      if ( JT (feeture) .eq. 50 ) return
      if ( JT (feeture) .eq. 52 ) return
      if ( JT (feeture) .eq. 54 ) return
      if ( JT (feeture) .eq. 56 ) return
      if ( JT (feeture) .eq. 58 ) return
      endif


*     identify any targets entered for the feature number "KU" -----------------
      IQSfeet = IFRQS(KU)
      limits in place = 0
      
*     initialise the face value to class one -----------------------------------
      Face value = 1
      Face confidence = 100.0
      Face determinand = 0
      do idet = 1, ndet
      Face Value dets (idet) = 1
      enddo
      if ( kstat2 .eq. 0 ) return ! check summary statistics have been defined -

*     the face value class across all determinands -----------------------------
*     set this initially to class 1 --------------------------------------------
      class at 50 = 1
      
*     check for a standard -----------------------------------------------------
      IQSreach = 0
      do kdet = 1, ndet ! ------------------------------------------------------
      do ic = 1, nclass
*     class limmits (ic,kdet) = 0.0
      enddo
      if ( qtype (kdet) .ne. 4 ) then
      if ( EQS reach (IREACH, kdet) .gt. 0 ) then
      IQSreach = EQS reach (IREACH, kdet)
      endif
      endif
      enddo ! loop on kdet -----------------------------------------------------
      
      if ( IQSfeet .eq. 0 .and. IQSreach .eq. 0 ) return ! #####################

*     set up the heading -------------------------------------------------------
      if ( nobigout .le. 0 ) then
      if ( kplace .eq. 2 ) then ! at the Feature 
      write(48,1010)UNAME(feeture),feeture,IREACH,
     &DIST(feeture),RNAME(IREACH)
      write(01,1010)UNAME(feeture),feeture,IREACH,
     &DIST(feeture),RNAME(IREACH)
 1010 format(/110('=')/'Classification at: ',A40,'Feature:',I6,
     &8x,'Reach:',I6/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ----- classification
     &'of the Reach called ',A16)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      write(31,1910)UNAME(feeture),DIST(feeture),RNAME(IREACH) ! ----------- EFF
 1910 format(//////110('=')/'Classification upstream of: ',A40/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ----- classification
     &'of the Reach called ',A16)
      do idet = 1, ndet
      if ( qtype(idet) .ne. 4 .and. noclass(idet) .eq. 1 ) then
      if ( classlimmits2 (1,idet) .gt. 0.0000001 ) then
 1940 format(/////110('+')/'Classification upstream of: ',A40,
     &' ... on reach ',a16/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ----- classification
     &'of the reach ...')
      else
 1941 format(/////110('+')/'Upstream of: ',A40,
     &' ... on reach ',a16/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ----- classification
     &'of the reach ...')
      endif
      endif ! if ( qtype(idet) .ne. 4 etc
      enddo ! do idet = 1, ndet
      endif
      endif ! if ( kplace .eq. 2 ) ------------------------------ at the Feature
      
*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( kplace .eq. 4 ) then ! ddddddddddddddddddd  downstream of the Feature
	write(48,1820)UNAME(feeture),feeture,IREACH, ! ----------------------- WFD
     &DIST(feeture),RNAME(IREACH)
	write(01,1820)UNAME(feeture),feeture,IREACH,
     &DIST(feeture),RNAME(IREACH)
 1820 format(/110('=')/'Classification d/s of: ',A40,'Feature:',I6,
     &2x,'Reach:',I6/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ----- classification
     &'of the Reach called ',A16)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! -----------------
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      write(31,8820)UNAME(feeture),RNAME(IREACH) ! - output for effluents -- EFF
 8820 format(/110('=')/'Classification downstream of ',A40,
     &' ... Reach: ',a21)
      endif ! if ( JT (feeture) .eq. 3 etc -------------------------------------
      endif ! if ( kplace .eq. 4 ) ddddddd immediately downstream of the Feature
*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      
      if ( kplace .eq. 1 ) then ! =============================== start of reach
	write(48,3910)RNAME(IREACH)
	write(01,3910)RNAME(IREACH)
 3910 format(/110('=')/'Classification at the head of the Reach ',
     &'called ',A16)
	endif ! =================================================== start of reach
      if ( kplace .eq. 3 ) then
	write(48,1919)RNAME(IREACH)
	write(01,1919)RNAME(IREACH)
 1919 format(/110('=')/'Classification at the end of the Reach ',
     &'called ',A16/110('-'))
	write(48,1939)RLENGTH(IREACH)
	write(01,1939)RLENGTH(IREACH)
 1939 format('Location:  ',F5.1,' km from the head of the Reach')
	endif
      
      write(48,1043)
      write(01,1043)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ===============
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,1043) ! ----------------------------------------------------- EFF
 1043 format(110('=')/
     &16x,'....... Confidence of class (%) .......'/110('=')/
     &16x,'High     Good  Moderate    Poor      Bad',
     &5x,'Class boundaries ',22('.'),' Statistic'/110('='))
      endif ! if ( kplace .ne. 1 .and. kplace .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 ========================================
      endif ! if ( nobigout .le. 0 ) set up the headings =======================

*     initialise the confidence of class ---------------------------------------
      do iclass = 1, nclass
	conf in class (iclass) = 0.0
	conf of class (iclass) = 0.0
      do jjdet = 1, ndet
	worse than (iclass,jjdet) = 0.0
	better than (iclass,jjdet) = 0.0
      enddo ! do jjdet = 1, ndet
	enddo ! do iclass = 1, nclass
	conf in class (1) = 100.0
      
*     sum the total length of classified river ---------------------------------
	Total class 1 = Total class 1 + distp
      if ( ifbatch .eq. 1 ) then ! this is a batch run =========================
	Grand class 1 = Grand class 1  + distp
      endif ! if ( ifbatch .eq. 1 ) ============================================

*     sum the length of classified river - if the river length has a target ----
      !if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then ! #######################
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loop on the determinands +++++++++++++++++++++++++++++++++++++++++++++++++
      do 10 idet = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++++++++++
      limits in place = 0 ! initialise for this determinand  
      if ( qtype (idet) .eq. 4 ) goto 10 ! +++++++++++++++++++++++++++++++++++++
      IQSreach = 0
      class limmits (1,idet) = 1.0e10 ! initialise limit on class 1 ------------
      zerolims = 0.0
      do ic = 2, nclass
      class limmits (ic,idet) = 0.0 ! initialise limits of the other classes ---
      zerolims = zerolims + class limmits2 (ic,idet)
      enddo
      
      if ( background class .eq. 1 .and. ! set up background class limits ------
     &    zerolims .gt. 0.000001 ) then ! set up background class limits -------
      limits in place = 1 ! there are class limits for this determinand --------
      do ic = 1, nclass
      class limmits (ic,idet) = class limmits2 (ic,idet)
      enddo
      endif ! ------------------------------------------------------------------
      
      if ( EQS reach (IREACH,idet) .gt. 0 ) then !  use limits set for the reach
      limits in place = 1 ! there are class limits -----------------------------
      IQSreach = EQS reach (IREACH, idet)
      class limmits (nclass,idet) = 1.0e10 ! initialise the last class limit ---
      do ic = 1, nclass - 1 ! set the limits as those specified for the reach --
      
      class limmits (ic,idet) = standards for reach (IQSreach,ic) ! ------------ 
      
      if ( ic .gt. 1 .and. ic .lt. nclass .and.
     &class limmits (ic,idet) .gt. 1.0e09 ) then
      goto 10
      endif

      enddo ! do ic = 1, nclass - 1
      endif ! if ( EQS reach (IREACH, idet) .gt. 0 ) ---------------------------

*     store the mean and standard deviation ------------------------------------
      COMPM = C (idet,1)
      COMPS = C (idet,2)
      if ( DETYPE (idet) .eq. 900 .or.
     &     DETYPE (idet) .eq. 909 ) then ! dissolved metal
      COMPM = cpart1 (idet,1) ! dissolved metal
	COMPS = cpart1 (idet,2) ! dissolved metal
      endif ! if ( DETYPE (idet) .eq. 900 etc dissolved metal ------------------

      if ( limits in place .eq. 0 ) goto 10
*     ignore the determinands excluded from classification system --------------
*     if ( Detype (idet) .eq. 100 ) goto 10 ! chloride
      if ( Detype (idet) .eq. 102 ) goto 10 ! TOC
*     if ( Detype (idet) .eq. 106 ) goto 10 ! nitrate
      if ( Detype (idet) .eq. 200 ) goto 10 ! parent
      if ( Detype (idet) .eq. 201 ) goto 10 ! daughter
      if ( Detype (idet) .eq. 900 ) goto 10 ! dissolved
      if ( Detype (idet) .eq. 909 ) goto 10 ! dissolved
*     exclude determinands from overall classification -------------------------
      if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 10 !2900
      if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 10 !2900
      if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 10 !2900
      if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 10 !2900
      if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 10 !2900
      if ( Face determinand .eq. 0 ) Face determinand = idet
 2900 continue

*     identify the summary statistic for the class limits ...
*     Kstat is 1, 2, 3 for mean, 95-percentile and 90-percentile ...
*     Kstat is 4 and 5 for the 5 and 10-percentile ...
*     Kstat is 6 for the 99-percentile ...
*     --------------------------------------------------------------------------
*     identify the type of summary statistic -----------------------------------
      call check summary statistic for standard (idet)
      Kstat = MRQS (idet) ! already constrained to values from 1 to 6

*     ignore the determinands with no river quality target ---------------------
*     identify the type of summary statistic -----------------------------------
      if ( kstat .gt. 0 .or. kstat .lt. 7 ) then ! =============================
      lstat = 1
      if ( kstat .eq. 1 ) lstat = 1
      if ( kstat .eq. 2 ) lstat = 3
      if ( kstat .eq. 3 ) lstat = 4
      if ( kstat .eq. 4 ) lstat = 3
      if ( kstat .eq. 5 ) lstat = 4
      if ( kstat .eq. 6 ) lstat = 5
*     retrieve the test statistic ---------------------------------------------- 
      test = C (idet, lstat )
 
      else ! if ( kstat .gt. 0 .or. kstat .lt. 7 ) =============================
      kstat = 0

*     95-percentile class limits -----------------------------------------------
      if ( output mode .eq. 0 ) kstat = 2
      if ( output mode .eq. 0 ) lstat = 3
*     mean class limits --------------------------------------------------------
      if ( output mode .eq. 1 ) kstat = 1
      if ( output mode .eq. 1 ) lstat = 1
*     90-percentile class limits -----------------------------------------------
      if ( output mode .eq. 2 ) kstat = 3
      if ( output mode .eq. 2 ) lstat = 3
*     99-percentile class limits -----------------------------------------------
      if ( output mode .eq. 3 ) kstat = 6
      if ( output mode .eq. 3 ) lstat = 5
      test = C (idet, lstat )
      endif ! if ( kstat .gt. 0 .or. kstat .lt. 7 ) ============================

      targit = 0.0 ! set the target --------------------------------------------
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,idet) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,idet) ! set target for feature
*     over-write this target with any reach-specific value entered -------------
      if ( IQSreach .gt. 0 ) then
      do ic = 1, nclass
      if ( class limmits (ic,idet) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,idet))
      endif
      enddo
      endif ! if ( IQSreach .gt. 0 )
      RQO(idet) = targit ! use the targets for graphs --------------------------
      
      testq = 0.0 ! initialise for classifications -----------------------------

*     loop on all the classes for this determinand -----------------------------
      do 76 iclass = 1, nclass
      ex = 0.0 ! initial value of confidence of failure
      if ( abs (class limmits (iclass,idet)) .gt. 99999999.9 ) goto 1176
*     skip classes if the class limit is zero ----------------------------------
      if ( abs ( class limmits (iclass,idet)) .lt. 1.0e-08 ) goto 1176
      targit = abs (class limmits (iclass,idet))

*     for class limits expressed as a mean -------------------------------------
      if ( kstat .eq. 1 ) then
*     call write the mean standard (48, idet, targit)
      call calculate compliance with mean standard (COMPM,COMPS,
     &qualn(idet),targit,EX,testq,testql,testqu,KU,idet)
      endif ! for class limits expressed as a mean -----------------------------

*     95-percentile class limits -----------------------------------------------
      if ( kstat .eq. 2 ) then
	xpoint=95.0
*     call write the percentile standard (48, idet, targit, xpoint)
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------95
     &tests,qualn(idet),targit,xpoint,EX,testq,testql,testqu,KU)
	endif ! 95-percentile class limits ---------------------------------------

*     99-percentile class limits -----------------------------------------------
      if ( kstat .eq. 6 ) then
	xpoint=99.0
*     call write the percentile standard (48, idet, targit, xpoint)
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------99
     &tests,qualn(idet),targit,xpoint,EX,testq,testql,testqu,KU)
	endif ! 99-percentile class limits ---------------------------------------

*     90-percentile class limits -----------------------------------------------
      if ( kstat .eq. 3 ) then
	xpoint=90.0
*     call write the percentile standard (48, idet, targit, xpoint)
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------90
     &tests,qualn(idet),targit,xpoint,EX,testq,testql,testqu,KU)
	endif ! 90-percentile class limits ---------------------------------------

*     5-percentile class limits ------------------------------------------------
      if ( kstat .eq. 4 ) then
	xpoint=5.0
*     call write the percentile standard (48, idet, targit, xpoint)
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------05
     &tests,qualn(idet),targit,xpoint,EX,testq,testql,testqu,KU)
	endif ! 5-percentile class limits ----------------------------------------

*     10-percentile class limits -----------------------------------------------
      if ( kstat .eq. 5 ) then
	xpoint=10.0
      call change to log domain (test,xpoint,COMPM,COMPS,testm,tests)
      call calculate compliance with percentile standard (testm, ! -----------10
     &tests,qualn(idet),targit,xpoint,EX,testq,testql,testqu,KU)
	endif ! 10-percentile class limits ---------------------------------------

      if ( targit .gt. 1.0e-20 ) then
      if ( nobigout .le. 0 ) then
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ================
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
	!write(31,77)COMPM,COMPS ! -------------------------------------------- EFF  
   77 format(
     &24x,'                                   Mean =',F12.3/
     &24x,'                     Standard deviation =',F12.3)
      endif ! if ( JT (feeture) .eq. 03 etc =====================================
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ================
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      !write(31,70)testq,testql,testqu,qualn(idet),EX ! --------------------- EFF
   70 format(
     &24x,'                         Test statistic =',F12.3/
     &24x,'                 Lower Confidence limit =',F12.3/
     &24x,'                 Upper Confidence limit =',F12.3/
     &24x,'                         Sample numbers =',F12.3/
     &24x,'Confidence that the standard was failed =',F12.3,' per cent'/
     &77('-'))
      endif ! if ( JT (feeture) .eq. 03 etc =====================================
      endif ! if ( nobigout .le. 0 )
      endif ! if ( targit .gt. 1.0e-20 ) 

 1176 continue
      
*     store the calculated confidence of class ---------------------------------
      worse than (iclass,idet) = EX
      better than (iclass,idet) = 100.0 - EX
      in or better (iclass, idet) = better than (iclass,idet)
      in class (iclass,idet) = EX

   76 continue ! end of the loop on classes 

      in or worse (1,idet) = 100.0
      do ic = 2, nclass
      in or worse (ic,idet) = worse than (ic-1,idet)
      enddo

*     the probablity of "worse" than the worst class is zero ------------------
      worse than   (nclass,idet) = 0.0
*     the probability if "in or better than the worst class is 100 per cent ---
      in or better (nclass,idet) = 100.0

*     initialise the face value class -----------------------------------------
      class at 50 = 0
      if ( limits in place .eq. 1 ) then ! ------------------------------------
      in class (1,idet) = in or better (1,idet) 
      in class (2,idet) = in or better (2,idet) - in or better (1,idet)
      in class (3,idet) = in or better (3,idet) - in or better (2,idet) 
      in class (4,idet) = in or better (4,idet) - in or better (3,idet) 
      in class (5,idet) = 100.0 - in or better (4,idet) 
      do iclass = 1,nclass
      in class (iclass,idet) = amax1 ( 0.0, in class (iclass,idet) )
      enddo ! do iclass = 1,nclass
      endif ! if ( limits in place .eq. 1 ) ------------------------------------

      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 1 (testq)
      if ( limits in place .eq. 1 ) then ! there are class limits --------------
      write(48,2221)Dname(idet),(in class(iclass,idet),
     &iclass = 1,nclass),(Class limmits (ii,idet), ii = 1,nclass - 1),
     &valchars10
      write(01,2221)Dname(idet),(in class(iclass,idet),
     &iclass = 1,nclass),(Class limmits (ii,idet), ii = 1,nclass - 1),
     &valchars10
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ===============
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,2221)Dname(idet),(in class(iclass,idet), ! ------------------ EFF
     &iclass = 1,nclass),(abs(Class limmits (ii,idet)),
     &ii = 1,nclass-1),valchars10
 2221 format(a11,5f9.2,4x,4f10.2,a10)

      if ( EQS reach (IREACH,idet) .eq. 0 ) then
      write(150+idet,1046)RQO(idet) ! ----------------------------------- Di EFF
 1046 format(110('=')/
     &16x,'....... CONFIDENCE OF CLASS (%) ........ Reach target:',
     &f10.2/110('=')/
     &16x,'High     Good  Moderate    Poor      Bad',
     &'     Class boundaries ',22('.'),' Statistic'/110('='))
      else
      write(150+idet,1047)RQO(idet) ! ----------------------------------- Di EFF
 1047 format(110('=')/
     &16x,'....... CONFIDENCE OF CLASS (%) ........ Reach target:',
     &f10.2/110('=')/
     &16x,'High     Good  Moderate    Poor      Bad',
     &'     Class boundaries ',22('.'),' Statistic'/110('='))
      endif ! if ( EQS reach (IREACH,idet) .eq. 0 )
      endif ! if ( kplace .ne. 1 .and. kplace .ne. 2 )
      
      endif ! if ( JT (feeture) .eq. 03 etc ====================================
      endif ! if ( limits in place .eq. 1 ) there are class limits -------------

      if ( limits in place .eq. 0 ) then ! there are no class limits ~~~~~~~~~~~
	write(48,2241)Dname(idet),(in class(iclass,idet),
     &iclass = 1,nclass),valchars10
	write(01,2241)Dname(idet),(in class(iclass,idet),
     &iclass = 1,nclass),valchars10
 2241 format(A11,5f9.2,4x,4('         -'),f10.3)
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. !================
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,2841)Dname(idet),(in class(iclass,idet), ! ------------------ EFF
     &iclass = 1,nclass),valchars10
 2841 format(A11,5f9.2,4x,4('         -'),f10.3)
      endif ! if ( kplace .ne. 1 .and. kplace .ne. 3 )
      endif ! if ( JT (feeture) .eq. 03 etc ====================================
      endif ! if ( limits in place .eq. 0 ) there are no class limits ~~~~~~~~~~
      endif ! if ( nobigout .le. 0 ) ===========================================
      
*     scan the classes and find the Face value Class for this determinand ------
*     set this equal to "class at 50" ------------------------------------------
      class at 50 = 1
      Face Value dets (idet) = 1

      do iclass = 1, nclass -1
      jclass = nclass - iclass + 1
      if ( in or worse (jclass,idet) .gt. 50.0 ) then
      class at 50 = jclass 
      Face Value dets (idet) = jclass
      goto 8099
      endif
      enddo ! do iclass = 1, nclass -1

 8099 continue

*     check whether this determinand replaces previous ones in determining the -
*     face value class ---------------------------------------------------------

*     exclude determinands from overall classification -------------------------
      if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 1020
      if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 1020
      if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 1020
      if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 1020
      if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 1020
      
      if ( class at 50 .gt. Face value ) then
      Face value = class at 50
      Face confidence = in or worse (Face value,idet)
      Face determinand = idet
      else
      if ( class at 50 .eq. Face value ) then ! --------------------------------
      if ( in or worse (Face value,idet) .gt. Face confidence ) then
      Face value = class at 50
      Face confidence = in or worse (Face value,idet)
      Face determinand = idet
      endif
      endif ! if ( class at 50 .eq. Face value ) -------------------------------

      endif

 1020 continue

*     face value class has now been updated for this determinand ---------------
      if ( Face determinand .eq. 0 ) Face determinand = idet

   10 continue ! end of loop on the determinands +++++++++++++++++++++++++++++++
      
     
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(31,2166) ! ----------------------------------------------------- EFF
 2166 format(110('='))
      endif
      endif
      
      if ( Face determinand .eq. 0 ) then
      if ( suppress4 .lt. 0 .and. kerror .eq. 1 ) then
      suppress4 = suppress4 + 1
      call change colour of text (10) ! green
      write( *,8411)uname(KU)
 8411 format('*** NO determinands for classification',13x,'...',7x,
     &'at ',a40,1x,25('.'))
      call set screen text colour 
      endif
      if ( suppress4 .lt. 0 .and. kerror .eq. 1 ) then
      suppress4 = suppress4 + 1
      call change colour of text (10) ! green
      write( *,8491)uname(KU)
 8491 format('*** No determinands for classification',13x,'...',7x,
     &'at ',a40,' Check .ERR for others ...')
      call set screen text colour
      endif
      Face determinand = 1
      endif

*     the loop on all the determinands is now finished -------------------------
*     the element which sets the Class is "Face determinand" .... --------------
*     the overall confidence will be stored in "conf in class" .... ------------
*     initialise "conf in class" to the values for the Face determinand --------

      do iclass = 1, nclass
      conf in class (iclass) = in class (iclass, Face determinand)
      conf of class (iclass) = in or better (iclass, Face determinand)
      enddo

*     now look at confidence of the classes which are worse than "Face Value" --
*     and compare "Face determinand" with other determinands .... --------------
*     look for cases where other element give more confidence than the ---------
*     "Face determinand" that the class is worse than "Face Value" -------------

*     first check that the assigned class is not the worst possible (Class 5) --
*     if this is so there is no class worse than "Face Value" ------------------

*     ==========================================================================
      if ( Face Value .lt. nclass ) then

*     loop on classes which are worse than the assigned class, "Face Value" ----

      do 2001 iclass = Face Value, nclass

*     confidence that class is ICLASS (ICLASS is worse than "Face Value" -------
*     when based on Face determinand) ------------------------------------------

      xmas = conf in class (iclass)
      jdet = Face determinand

*     check whether other determinands have bigger confidence of exceeding -----
*     "Face Value" than "Face determinand" ... loop on these -------------------
*     other determinands and find the one which has biggest confidence ---------
*     this will be labelled JDET -----------------------------------------------

      do 2000 idet = 1, ndet
      if ( qtype (idet) .eq. 4 ) goto 2000
      if ( Detype (idet) .eq. 100 )  goto 2000 ! chloride
      if ( Detype (idet) .eq. 102 )  goto 2000 ! TOC
*     if ( Detype (idet) .eq. 106 )  goto 2000
      if ( Detype (idet) .eq. 200 )  goto 2000
      if ( Detype (idet) .eq. 201 )  goto 2000

*     exclude Determinands from overall classification -------------------------
      if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 2000
      if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 2000
      if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 2000
      if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 2000
      if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 2000

*     skip Face determinand ----------------------------------------------------

      if ( Face determinand .eq. idet ) goto 2000
      if ( in class (iclass, idet) .le. xmas ) goto 2000

*     idet has bigger confidence than "Face determinand" -----------------------

      zmas = in class (iclass, idet)
      JDET = idet

 2000 continue

*     confirm that JDET has been reset  ...
      if ( jdet .ne. Face determinand ) then

*     JDET has bigger confidence of giving ICLASS (exceeding Face Value)
*     than Face determinand ....

*     re-assign confidence of ICLASS from Face determinand to JDET ...
      conf in class (iclass) = zmas

*     compute the size of the excess of the confidence for JDET
*     compared with the confidence from Face determinand ...
      ymas = zmas - xmas

*     subtract excess from other classes, starting with the best ...
      do 3000 jclass = 1, Face value
      if ( ymas .gt. conf in class (jclass) ) then
      ymas = ymas - conf in class (jclass)
      conf in class (jclass) = 0.0
      else
      conf in class (jclass) = conf in class (jclass) - ymas
      ymas = 0.0
      goto 3001
      endif
 3000 continue
 3001 continue

      endif

*     proceed to next class, the next value of ICLASS --------------------------
 2001 continue 

      endif
*     ==========================================================================

*     Now examine the classes which are better than the Face-value Class
*     It may be that these need adjustment because of other elements
*     We shall look for discontinuities on the sequence of confidence levels
*     from Class 1 to the Class better than Face Value ...

*     This is necessary only when Face Value is worse than Class 2 ...

      if ( Face value .gt. 2 ) then

*     Scan the classes which are better than the Face Value class ...
      need scan = 0

      do iclass = 1, Face value - 1
*     Check that each of these has lower confidence than the next worse Class 
      if ( conf in class (iclass) .gt. 
     &     conf in class (iclass + 1) ) need scan = 1   
      enddo

*     All Classes up to (Face value - 1) have less confidence than the next worse Class 
*     No discontinuities have been found ...

      if ( need scan .eq. 1 ) then ! +++++++++++++++++++++++++++++++++++++++++++

*     Found a Class which has more confidence than the next worse Class
*     This is a potential discontinuity ...
*     Find the element which determines the class "Face Value minus 1" ...

      FACUP1 = Face value - 1
      xp = 0.0
      jdet = 1

      do 2300 idet = 1, ndet

      if ( qtype (idet) .eq.   4 )  goto 2300
      if ( Detype (idet) .eq. 100 )  goto 2300
      if ( Detype (idet) .eq. 102 )  goto 2300
      if ( Detype (idet) .eq. 106 )  goto 2300
      if ( Detype (idet) .eq. 200 )  goto 2300
      if ( Detype (idet) .eq. 201 )  goto 2300

*     Exclude Determinads from overall classification --------------------------
      if ( exclude BOD .eq. 1 .and. Detype (idet) .eq. 101 ) goto 2300
      if ( exclude NO3 .eq. 1 .and. Detype (idet) .eq. 106 ) goto 2300
      if ( exclude PO4 .eq. 1 .and. Detype (idet) .eq. 105 ) goto 2300
      if ( exclude DOX .eq. 1 .and. Detype (idet) .eq. 104 ) goto 2300
      if ( exclude AMM .eq. 1 .and. Detype (idet) .eq. 103 ) goto 2300
 
      if ( in class (FACUP1 , idet) .gt. xp ) then ! ---------------------------
      xp = in class (FACUP1 , idet)
      JDET = idet
      endif ! if ( in class (FACUP1 , idet) .gt. xp ) --------------------------
 2300 continue

*     ...this element is number JDET ... the confidence of class FACUP1 is XP ...
*     ...check whether JDET gives more confidence of FACUP1 than the Face Value determinand ...

      if ( xp .gt. in class (FACUP1 , Face determinand) ) then

*     ...JDET gives less confidence of FACUP1 than the Face Value determinand ... 
*     ...Compute the confidence that class is Face Value or worse ...

      yp = 0.0
      do iclass = Face value, nclass
      yp = yp + conf in class (iclass)
      enddo
      yp = 100.0 - yp
    
      conf in class (FACUP1) = xp

      ymas = -100.0
      do iclass = 1, nclass ! --------------------------------------------------
      ymas = ymas + conf in class (iclass)
      enddo ! ------------------------------------------------------------------

*     compute the size of the excess of the confidence for JDET
*     compared with the confidence from Face determinand ...

*     .........subtract excess from other classes, starting with the best ...
      do 3100 jclass = 1, Face value ! =========================================
      if ( ymas .gt. conf in class (jclass) ) then ! ---------------------------
      ymas = ymas - conf in class (jclass)
      conf in class (jclass) = 0.0
      else ! if ( ymas .gt. conf in class (jclass) ) ---------------------------
      conf in class (jclass) = conf in class (jclass) - ymas
      ymas = 0.0
      goto 3101
      endif ! if ( ymas .gt. conf in class (jclass) ) --------------------------
 3100 continue ! ===============================================================
 3101 continue

*     ...This confidence will be allocated to Classes 1 to FACUP1 - 1
*     ...Loop on the Classes better than FACUP1

*     do iclass = 1, FACUP1 - 1

*...........confidence of ICLASS is now determined by JDET ...
*     conf in class (iclass) = in class (iclass, JDET)
*     yp = yp - in class (iclass, JDET)
*     enddo

*........and adjust confidence for FACUP1

*     conf in class (FACUP1) = yp

      endif ! if ( need scan .eq. 1 ) then ! +++++++++++++++++++++++++++++++++++
      endif
      endif

*     ==========================================================================
*     sum the per cent of lengths in each class for each determinand -----------
      do iclass = 1, nclass
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      if ( kplace .ne. 4 ) then
      totals in classes (iclass, idet) = 
     &totals in classes (iclass, idet) + distp  
     & * 0.01 * in class (iclass, idet)
      endif
      endif
      enddo

*     sum the per cent of lengths in each class over all determinands ... 
      if ( kplace .ne. 4 ) then ! ----------------------------------------------
      totals over all (iclass) = 
     &totals over all (iclass) + distp
     & * 0.01 * conf in class (iclass)
      endif ! if ( kplace .ne. 4 ) then ----------------------------------------

      enddo

      if ( nobigout .le. 0 ) then ! --------------------------------------------
      write(48,1388)(conf in class(iclass),
     &iclass=1,nclass),Face value,Dname(Face determinand)
 1388 format(110('-')/'Overall....',5f9.2,8x,' Face value =',i2,
     &' (',a11,')')
      write(48,2621) ! ----------------------------------------------------- WFD
 2621 format(110('=')/'Accumulated lengths in classes (kilometres) ...',
     &21x,'Total length'/110('='))
      do 2312 idet = 1, ndet
      if ( qtype (idet) .eq. 4 ) goto 2312
      if ( Detype (idet) .eq. 100 ) goto 2312 ! Chloride
      if ( Detype (idet) .eq. 102 ) goto 2312 ! TOC
      if ( Detype (idet) .eq. 106 ) goto 2312 ! Nitrate
      if ( Detype (idet) .eq. 200 ) goto 2312 ! parent
      if ( Detype (idet) .eq. 201 ) goto 2312 ! daughter
      if ( kplace .ne. 4 ) then
      totallength = 0.0
      do iclass = 1, nclass
      totallength = totallength + totals in classes (iclass, idet)
      enddo
      endif
      write(48,1488)(totals in classes (iclass, idet), 
     &iclass = 1, nclass), totallength, Dname(idet)
 1488 format('           ',5f9.2,f24.2,16x,' (',a11,')')
 2312 continue
      write(48,2587)
      write(01,2587)
 2587 format(110('='))
      endif ! if ( nobigout .le. 0 ) -------------------------------------------
*     ==========================================================================
      
      !endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) ######################
      return
      end



      subroutine write the mean standard (ic, idet, targit)
      include 'COMMON DATA.FOR'
      
      if ( nobigout .gt. 0 ) return
      if ( targit .lt. 1.0e-20 .or. targit .gt. 1.0e6 ) return
      call sort format 1 (targit)
      
      if ( Detype(idet) .lt. 900 ) then ! ++++++++++++++++++++++++++++++++++++++
      if ( ic .eq. 31 .and. kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(ic,1)dname(idet),valchars10,units(idet)
    1 format(a11,36x,'Mean standard =',a10,1x,a4)
      endif ! if ( ic idet.eq. 31 etc ------------------------------------------

      else ! if ( Detype(idet) .ge. 900 ++++++++++++++++++++++++++++++++++++++++
      if ( Detype(idet) .eq. 909 ) then ! --------------------------------------
      if ( ic .eq. 31 .and. kplace .ne. 1 .and. kplace .ne. 3 ) then
      write(ic,2)dname(idet),valchars10,units(idet)
    2 format('(Dissolved) ',a11,24x,'Mean standard =',a10,1x,a4)
      endif ! if ( ic .eq. 31 etc )
      !if ( ic .eq. 01 .and. kplace .ne. 1 .and. kplace .ne. 3 ) then
      if ( ic .eq. 01 ) then
      write(ic,2)dname(idet),valchars10,units(idet)
!   2 format('(Dissolved) ',a11,24x,'Mean standard =',a10,1x,a4)
      endif ! if ( ic idet.eq. 31 )     
      endif ! if ( Detype(idet) .eq. 909 ) -------------------------------------

      if ( Detype(idet) .eq. 900 ) then ! --------------------------------------
      write(ic,3)dname(idet),valchars10,units(idet)
    3 format('(Total) ',a11,28x,'Mean standard =',a10,1x,a4)
      if ( ic .eq. 31 .and. kplace .ne. 1 ) then
      write(150+idet,3)dname(idet),valchars10,units(idet) ! ------------- Di EFF
      endif ! if ( ic idet.eq. 31 )
      !if ( ic .eq. 01 .and. kplace .ne. 1 ) then
      endif ! if ( Detype(idet) .eq. 900 ) -------------------------------------
      endif ! if ( Detype(idet) .lt. 900 ) +++++++++++++++++++++++++++++++++++++
      
      return
      end
 

     
      subroutine write the percentile standard (ic,idet,targit,xpoint)
      include 'COMMON DATA.FOR'
      
      if ( nobigout .gt. 0 ) return
      if ( targit .lt. 1.0e-20 .or. targit .gt. 1.0e6 ) return
      ipoint = xpoint
      call sort format 1 (targit)
      if ( ic. ne. 31 ) then ! -------------------------------------------------
      write(ic,1)dname(idet),ipoint,valchars10,units(idet)
    1 format(a11,24x,i5,'-percentile standard =',a10,1x,a4)
      endif ! if ( ic .eq. 31 ) ------------------------------------------------
      return
      end
      
      
      subroutine write headings and sum compliance distances (kplace)
      include 'COMMON DATA.FOR'
      
      if ( nobigout .le. 0 ) then ! ############################################
      if ( kplace .ne. 2 .and. kplace .ne. 4 .and.
     &     kplace .ne. 3 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(01,7009)
 7009 format(/110('~')/'Assessment of compliance with standards ',
     &70('.')/110('~'))
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      if ( kplace .ne. 1 ) then
      write(31,7009) ! ----------------------------------------------------- EFF
      endif ! if ( kplace .ne. 1 )
      endif ! if ( JT (feeture) .eq. 03 etc ------------------------------------
      endif ! if ( kplace .ne. 2 .and. kplace .ne. 4 ) ~~~~~~~~~~~~~~~~~~~~~~~~~


*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      if ( kplace .eq. 4 ) then ! location is downstream of feature dddddddddddd
      write(01,7809)UNAME(feeture),Length of main river ! ------------------ OUT
 7809 format(110('+')/'Assessment of compliance d/s of ', ! ---------------- OUT
     &a40,f10.1,' km'/110('-')) ! ------------------------------------------ OUT
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ---------------
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      write(31,7809)UNAME(feeture),Length of main river ! ------------------ EFF
      endif ! if ( JT (feeture) .eq. 3 etc -------------------------------------
      endif ! if ( kplace .eq. 4 ) ddddddddddddddddddddddddddddddddddddddddddddd
*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      
      
      if ( kplace .eq. 2 ) then ! location is upstream of the feature uuuuuuuuuu
      write(01,7509)UNAME(feeture) ! --------------------------------------- OUT
 7509 format(//110('-')/'Assessment of compliance with standards u/s ',
     &'of ',a40/110('-'))
	if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61) then
      write(31,8509)UNAME(feeture) ! --------------------------------------- EFF
 8509 format(110('-')/'Assessment of compliance upstream of ',
     &a40/110('-'))
      endif ! JT (feeture) .eq. 03 etc =========================================
      endif ! if ( kplace .eq. 2 ) location is upstream of the feature uuuuuuuuu
      
      endif ! if ( nobigout .le. 0 ) ###########################################

      if ( kplace .ne. 4 ) then
*     total length of river with a target --------------------------------------
      Total RQS length 1 = Total RQS length 1 + distp
*     total length of river with a river quality target ------------------------
      Total length 2 = Total length 2 + distp
      endif ! if ( kplace .ne. 4 )

      return
      end