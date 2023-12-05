*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: set up data.FOR ... 2095 lines -------------------------------------
*     --------------------------------------------------------------------------
*     This file supports START THE CALCULATION.FOR.  It write, checks and sets -
*     up data ...
*     --------------------------------------------------------------------------
*     This file contains 15 ......s ---------------------------------------
*     They are called: ---------------------------------------------------------
*     --------------------------------------------------------------------------
*     ...... write river flows
*     ...... write data on river quality (MV)
*     ...... write effluent data
*     ...... write river targets
*     ...... write the features
*     ...... set up default random normal deviates
*     ...... set the uncorrelated values of the normal deviates 
*     ...... get the correlated random numbers (IS,R1,R2,R3,R4)
*     ...... reset random number starters
*     ...... initialise variables
*     ...... set determinand codes (idet)
*     ...... check first four characters of a line of data (IFIN)
*     ...... check for reach data on background (KREACH,LTEST) 
*     ...... extract a line of data (line)
*     --------------------------------------------------------------------------
      
      subroutine write river flows
      include 'COMMON DATA.FOR'

*     4 - non-parametric distribution
*     5 - monthly distribution
*     8 - monthly structure
      numb5 = 0
      numb8 = 0

      call river flow correlation ! sort out correlation

      write(08,1)FUNIT
    1 format(77('-')/'Data for River Flow (',a4,')'/77('-')/
     &'  Code           (1) Mean '/
     &' Number          (2) 95%-exceedence '/
     &' of Set          (3) Type of Data'//
     &'                 (1)       (2)   (3)    ',
     &'Used by the following feature ...'/77('-'))

      do IF = 1, NMKF
      IVECT = MKF(IF)   
      if (MKF(IF) .gt. 0 .and. PDRF(IVECT).ne. -999) then

      ifeet1 = 0
      do ift = 1, feetcount
      if (JF(ift) .eq. IVECT ) then
      ifeet1 = ift
      goto 4
      endif
      enddo
          
    4 call sort format 2 (F(IVECT,1),F(IVECT,2))
      if ( ifeet1 .gt. 0 ) then
      write(08,2)MKF(IF),valchars10,valchars11,PDRF(IVECT),uname(ifeet1)
    2 format(I6,4X,a10,a10,i5,5x,a37)
      else
      write(08,3)MKF(IF),valchars10,valchars11,PDRF(IVECT)
    3 format(I6,4X,a10,a10,i5,5x,'None ...')
      endif
      
      if ( PDRF(IF) .eq. 5 ) numb5 = numb5 + 1
      if ( PDRF(IF) .eq. 8 ) numb8 = numb8 + 1
      endif
      enddo
      write(08,5)
    5 format(77('-'))

      return
      end



*     write out sets of data on river quality ----------------------------------
      subroutine write data on river quality (MV)
      include 'COMMON DATA.FOR'

      if ( ICAL .eq. 1 ) return ! write river quality

*     5 - monthly distribution
*     8 - monthly structure
      numb5 = 0
      numb8 = 0

      do IP = 1, NDET ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      if ( qtype(IP) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      write(08,9)DNAME(IP),UNITS(IP)
    9 format(//77('-')/'Data for river quality: ',a10,' (',a4,')')
      write(08,7)
    7 format(77('-')/
     &'    Code                 (1) Mean'/
     &'  Number                 (2) Standard Deviation'/
     &'  of set                 (3) Number of Samples'/
     &'                         (4) Type of Data'/77('-')/
     &12x,'(1)       (2)  (3)   (4)    ',
     &'Used by the following feature ...'/77('-'))

      do IQ = 1, NMKQ ! =========================================================
      IVECT = MKQ(IQ)   
      if (MKQ(IQ) .gt. 0 .and. PDRC(IVECT,IP).ne. -999) then
      ifeet1 = 0
      do iqt = 1, feetcount
      if (JQ(iqt) .eq. IVECT ) then
      ifeet1 = iqt
      goto 4
      endif
      enddo
          
    4 call sort format 2 (quolity data(IVECT,IP,1),
     &quolity data(IVECT,IP,2))
      if ( ifeet1 .gt. 0 ) then
      write(08,11)MKQ(IQ),valchars10,valchars11,QNUM(IVECT,IP),
     &PDRC(IVECT,IP),uname(ifeet1)
   11 format(I5,2a10,I5,I5,5x,a37)
      else
      write(08,12)MKQ(IQ),valchars10,valchars11,QNUM(IVECT,IP),
     &PDRC(IVECT,IP)
   12 format(I5,2a10,I5,I5,5x,'None ...')
      endif
      if ( PDRC(IVECT,IP) .eq. 5 ) numb5 = numb5 + 1
      if ( PDRC(IVECT,IP) .eq. 8 ) numb8 = numb8 + 1
      endif ! ==================================================================
      enddo ! IQ = 1, NMKQ =====================================================
      write(08,8)
    8 format(77('-'))
      
      endif ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      enddo ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      
      return
      end


*     write out sets of data on effluent flow and quality ----------------------
      subroutine write effluent data
      include 'COMMON DATA.FOR'
      
      if ( ICAL .eq. 1 .or. MKE(1) .eq. 0 ) return
*     5 - monthly distribution
*     8 - monthly structure
      numb5 = 0
      numb8 = 0

      write(08,1)Funit
    1 format(////77('-')/
     &'Flow Data for Discharges (',a4,')'/77('-')/
     &'   Code                    (1) Mean'/
     &' Number                    (2) Standard Deviation'/
     &' of Set                    (3) Number of Samples'/
     &'                           (4) Type of Data'/77('-')/
     &10X,'  (1)       (2)  (3)   (4)    ',
     &'Used by the following feature ...'/77('-'))
      
      do IE = 1, NMKE ! ----------------------------------------- discharge flow
      IESET = MKE(IE) ! set of the effluent data ---------------- discharge flow
      if (IESET .gt. 0) then
      ifeet1 = 0
      do iet = 1, feetcount ! ----------------------------------- discharge flow
      it = JT(iet) ! type of feature ---------------------------- discharge flow
      if ( it .eq. 03 .or. it .eq. 05 .or. it .eq. 12 .or.
     &     it .eq. 15 .or. it .eq. 39 .or. it .eq. 42 .or.
     &     it .eq. 60 .or. it .eq. 61 .or. it .eq. 62) then
      if (JQ(iet) .eq. IESET ) then
      ifeet1 = iet
      goto 13
      endif
      endif ! if ( it .eq. 3 .or. ------------------------------- discharge flow
      enddo ! do iet = 1, feetcount ----------------------------- discharge flow
   13 call sort format 2 (FE(IESET,1),FE(IESET,2))
      if ( ifeet1 .gt. 0 ) then
      write(08,5)IESET,valchars10,valchars11,ENUM(IESET),PDEF(IESET),
     &UNAME(ifeet1)
    5 format(i4,1X,2a10,I5,i5,5x,a37)
      else
      write(08,12)IESET,valchars10,valchars11,ENUM(IESET),PDEF(IESET)
   12 format(i4,1X,2a10,I5,i5,5x,'None ...')
      if (PDEF(IESET) .eq. 5) numb5 = numb5 + 1
      if (PDEF(IESET) .eq. 8) numb8 = numb8 + 1
      endif ! if ( ifeet1 .gt. 0 ) ------------------------------ discharge flow
      endif ! if (IESET .gt. 0) ------------------------------------------------
      enddo ! do IE = 1, NMKE --------------------------------------- discharges
      write(08,3)
    3 format(77('-'))

      do IP = 1, ndet ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      if ( qtype(IP) .ne. 4 ) then ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      
      write(08,4)DNAME(IP),UNITS(IP)
    4 format(////77('-')/'Water quality data for discharges: ',a11,
     &' (',a4,')'/77('-')/'   Code                    (1) Mean '/
     &' Number                    (2) Standard Deviation '/
     &' of Set                    (3) Number of Samples'/
     &'                           (4) Type of Data'/77('-')/
     &10X,'  (1)       (2)  (3)   (4)    ',
     &'Used by the following feature ...'/77('-'))
      
      do IE = 1, NMKE ! ====================================== discharge quality
      IESET = MKE(IE) ! set of the effluent data ------------- discharge quality
      if (IESET .gt. 0) then
      ifeet1 = 0
      do iet = 1, feetcount ! -------------------------------- discharge quality
      it = JT(iet) ! type of feature ------------------------- discharge quality
      if ( it .eq. 3 .or. it .eq. 5 .or. it .eq. 12 .or.
     &     it .eq. 15 .or. it .eq. 39 .or. it .eq. 42 .or.
     &     it .eq. 60 .or. it .eq. 61 .or. it .eq. 62 ) then
      if (JQ(iet) .eq. IESET ) then
      ifeet1 = iet
      goto 14
      endif
      endif ! if ( it .eq. 3 .or. ---------------------------- discharge quality
      enddo ! do iet = 1, feetcount -------------------------- discharge quality
      
   14 call sort format 2 (pollution data(IESET,IP,1),
     &pollution data(IESET,IP,2))
      if ( ifeet1 .gt. 0 ) then
      write(08,16)IESET,valchars10,valchars11,PNUM(IESET,IP),
     &PDEC(IESET,IP),UNAME(ifeet1)
   16 format(i4,1X,2a10,I5,i5,5x,a37)
      else
      write(08,15)IESET,valchars10,valchars11,PNUM(IESET,IP),
     &PDEC(IESET,IP)
   15 format(i4,1X,2a10,I5,i5,5x,'None ...')
      if (PDEC(IESET,IP) .eq. 5) numb5 = numb5 + 1
      if (PDEC(IESET,IP) .eq. 8) numb8 = numb8 + 1
      endif ! if ( ifeet1 .gt. 0 ) --------------------------- discharge quality
      endif ! if (IESET .gt. 0) ------------------------------------------------
      enddo ! do IE = 1, NMKE ================================ discharge quality
      write(08,6)
    6 format(77('-'))
      endif ! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      enddo ! do IE = 1, NMKE IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
      
      return
      end


*     write out sets of data on river quality targets --------------------------
      subroutine write river targets
      include 'COMMON DATA.FOR'

      if ( ICAL .eq. 1 ) return ! write river targets
      
*     initialise data on river quality targets for plotting -------------------- 
      do JP = 1, ndet
*     if ( qtype(JP) .ne. 4 ) RQO(JP) = RQS(1,JP)
      enddo

      if ( NQ .eq. 0 ) return
      
      if ( NOTARG .eq. 1) return

      npx1 = 1
      npx4 = 4
      npx = 4
      if ( npx4 .gt. ndet) npx4=ndet

      XXX = 0.0
      do IQ = 1, NQ
      do IP = npx1, npx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      enddo

      if ( XXX .ge. 1.0E-10 ) then
      write(08,8261)
 8261 format(//77('-')/
     &'River Quality Targets'/77('-')/
     &'   Code                 Concentrations'/
     &' Number                 --------------'/' of Set')

      write(08,8000)(DNAME(IP),IP=npx1,npx4)
 8000 format(77('-')/15X,4(3x,A11,2x))
      write(08,1915)(UNITS(IP),IP=npx1,npx4)
 1915 format(14X,4('      ',A4,' ',5X))
      write(08,4102)
 4102 format(77('-'))

      do 8264 IQ = 1, NQ
      XXX = 0.0
      do IP = npx1,npx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      if  (XXX .lt. 1.0E-10 ) goto 8264
      if ( npx4 .eq. npx ) then
      write(08,8265)IQ,(RQS(IQ,IP),IP=npx1,npx4)
 8265 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-1 ) then
      write(08,8365)IQ,(RQS(IQ,IP),IP=npx1,npx4)
 8365 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-2 ) then
      write(08,8465)IQ,(RQS(IQ,IP),IP=npx1,npx4)
 8465 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-3 ) then
      write(08,8565)IQ,(RQS(IQ,IP),IP=npx1,npx4)
 8565 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
 8264 continue

      write(08,4002)
 4002 format(77('-'))
      else
      write(08,8461)
 8461 format(//77('-')/
     &'No River Quality Targets were entered '/77('-'))
      endif

 2000 continue
      if ( NPx4 .eq. NDET .or. NDET .le. 4 ) return

      npx1= npx1 + 4
      npx4= npx4 + 4
      if ( npx4 .gt. ndet) npx4=ndet

      write(08,8261)
      write(08,8000)(DNAME(IP),IP=npx1,NPx4)
      write(08,1915)(UNITS(IP),IP=npx1,NPx4)
      write(08,4102)

      do IQ = 1, NQ
      XXX = 0.0
      do IP = npx1, NPx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      if ( XXX .gt. 1.0E-10 ) write(08,8265)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
      enddo
      write(08,4002)
      goto 2000

      return
      end


*     write out data on features -----------------------------------------------
      subroutine write the features
      include 'COMMON DATA.FOR'

      mode05 = 0
      mode08 = 0

*     check the use of data on river flows -------------------------------------
      do i = 1, MU
      IF = JF(i)
      if ( IF .gt. 0 ) then
      if ( PDRF(IF) .eq. 5 ) mode05 = mode05 + 1
      if ( PDRF(IF) .eq. 8 ) mode08 = mode08 + 1
      endif
      enddo

*     check the use of data on disharge flow -----------------------------------
      do i = 1, MU
      IQ = JQ(i)
      IF = JF(i)
      if ( IQ .gt. 0 .and. IF .eq. 0 .and. JT(i) .ne. 1) then
      if ( PDEF(IQ) .eq. 5 ) mode05 = mode05 + 1
      if ( PDEF(IQ) .eq. 8 ) mode08 = mode08 + 1
      endif
      enddo

*     check the use of data on river quality -----------------------------------
      do i = 1, MU
      IQ = JQ(i)
      IF = JF(i)
      do j = 1, ndet
      if ( IQ .gt. 0 .and. IF .gt. 0 ) then
      if ( qtype (j) .ne. 4 .and. JT(i) .ne. 1 ) then
      if ( PDRC(IQ,j) .eq. 5 ) mode05 = mode05 + 1
      if ( PDRC(IQ,j) .eq. 8 ) mode08 = mode08 + 1
      endif
      endif
      enddo
      enddo

*     check the use of the data on discharge river quality ---------------------
      do i = 1, MU
      IQ = JQ(i)
      IF = JF(i)
      do j = 1, ndet
      if (IQ .gt. 0 .and. IF .eq. 0 .and. JT(i) .ne. 1) then
      if (qtype (j) .ne. 4) then
      if (PDEC(IQ,j) .eq. 8) mode05 = mode05 + 1
      if (PDEC(IQ,j) .eq. 8) mode08 = mode08 + 1
      endif
      endif
      enddo
      enddo

*     distribution types 5 or 8 - monthly structure ----------------------------
      if ( mode08 .gt. 0 .or. mode05 .gt. 0 ) then ! ===========================
          
      if ( munthly structure .eq. 2 ) then ! ================================= 2
      write( *,4966)
 4966 format(100('='))
      call change colour of text (11) ! cyan
      write( *,4976)
 4976 format(
     &'*** Some data have a monthly structure (Types 5 or 8) ...',
     &16x,23x,'****'/
     &'*** SIMCAT will NOT impose default monthly structures ', 
     &'on annual data     ',23x,'****'/
     &'*** To change this set the "monthly structure" switch to "1" ',
     &'...',9x,23x,'****')
      call set screen text colour
      write( *,4966)
      write(01,3966)
      write(33,3966)
      write(08,3966)
      write(09,3966)
 3966 format(77('-')/
     &'*** Some data have a monthly structure (Types 5 or 8) ...',
     &16x,'****'/
     &'*** SIMCAT will NOT impose default monthly structures ', 
     &'on annual data     ****'/
     &'*** To change this set the "monthly structure" switch to "1" ',
     &'...',9x,'****'/ 77('-'))
*     check the number of shots ============================================== 2      
      xcheck = float (NS) / 365.0
      icheck = xcheck
      xcheck = xcheck - icheck 
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++ 2
      NUS = 365 * (icheck + 1)
      if ( NUS .gt. MS) then
      xcheck = float (MS) / 365.0
      icheck = xcheck
      NUS = 365 * icheck
      endif
      call change colour of text (10) ! green
      write( *,3866)NS,NUS
      call set screen text colour
      write(01,3266)NS,NUS
      write(33,3266)NS,NUS
      write(08,3266)NS,NUS
      write(09,3266)NS,NUS
      NS = NUS
      call set indices for percentiles etc ! ------------------ writing features
      endif ! if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) +++++++++++++ 2
      endif ! if ( munthly structure .eq. 2 ) ================================ 2

      if ( munthly structure .eq. 0 ) then ! --------- monthly data not expected
      !write( *,3957)
 3957 format(77('='))
      call change colour of text (12) ! orange
      write( *,3956)
 3956 format(
     &'*** Distributions have been given a monthly structure ',
     &'(Types 5 or 8)',5x,'****'/
     &'*** SIMCAT will impose default monthly structure on ', 
     &'annual flow data',5x,'****'/
     &'*** To stop this set the "monthly structure" switch to "2"',
     &15x,'****')
      call set screen text colour
      write( *,3957)
      write(01,3916)
      write(33,3916)
      write(08,3916)
      write(09,3916)
 3916 format(77('-')/
     &'*** Distributions have been given a monthly structure ',
     &'(Types 5 or 8)     ****'/
     &'*** SIMCAT will impose default monthly structure on ', 
     &'annual flow data     ****'/
     &'*** To stop this set the "monthly structure" switch to "2" ',
     &14x,'****'/ 77('-'))
*     having noted the use of type 5 or 8 data - impose a monthly structure on - 
*     annual data --------------------------------------------------------------
      munthly structure = 1 ! reset from zero ----------- data type 5 or 8 found
*     check the number of shots ================================================      
      xcheck = float (NS) / 365.0
      icheck = xcheck
      xcheck = xcheck - icheck 
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then ! ++++++++++++++++
      NUS = 365 * (icheck + 1)
      if ( NUS .gt. MS) then
      xcheck = float (MS) / 365.0
      icheck = xcheck
      NUS = 365 * icheck
      endif
      call change colour of text (10) ! green
      write( *,3866)NS,NUS
 3866 format(77('-')/
     &'*** Data entered with a monthly structure (Types 5 or 8)',
     &17x,'****'/
     &'*** The number of shots is',i6,' ... This was changed to',i6,
     &11x,'****'/77('-'))
      call set screen text colour
      write(01,3266)NS,NUS
      write(33,3266)NS,NUS
      write(08,3266)NS,NUS
      write(09,3266)NS,NUS
 3266 format(/77('-')/
     &'Data used that have a monthly structure ... '/
     &'The number of shots is',i6,
     &' ... this should be a multiple of 365 ... '/
     &'The number of shots has been re-set to',i6/77('-')/) 
      NS = NUS
      call set indices for percentiles etc ! ------------------ writing features
      endif ! if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) +++++++++++++++
      endif ! if ( munthly structure .eq. 0 ) ==================================
      
      endif ! if ( mode08 .gt. 0 .or. mode05 .gt. 0 ) then ! ===================

*     ensure names of features are all unique ----------------------------------
      do itemp = 1, MU
      write(TITLE,3743)UNAME(itemp),itemp
 3743 format(A40,I4)
      write(UNAME(itemp),3744)TITLE
 3744 format(A44)
      enddo

      write(08,1036)MU,funit
 1036 format(//100('-')/'List of Features ...'i7/100('-')/
     &'Type  Name of Feature                    ',
     &'Flow (',A4,')          Flow and Quality   Reach'/
     &'----  ---------------                    ',
     &'-----------          Data-sets          -----'/
     &'                                         ',
     &'Mean  95-percentile ----------------'/
     &'                                          ',
     &'     or standard                    '/
     &'                                          ',
     &'     deviation                      ')

      write(08,1002)
 1002 format(100('-'))

      do 81 itemp = 1, MU ! loop on features
      IT = JT(itemp)

*     Branch to feature type:

*          (1) Monitoring point
*          (2) Stream or tributary
*          (3) Sewage treatment works
*          (4) River flow gauge
*          (5) Industrial discharge
*          (6) Plotting point
*          (7) Continuous abstraction
*          (8) Weir
*          (9) Flow regulation point
*         (10) Boundary point
*         (11) Bifurcation
*         (12) Intermittent discharge (12)
*         (13) Switch on Diffuse Pollution
*         (14) Swtich off Diffuse Pollution
*         (15) Switch on Diffuse Pollution
*         (16) Switch off Diffuse Pollution
*         (17) Discharge with zero flow
*         (18) Abstraction (negative discharge - mean and 95-percentile)
*         (19) Abstraction (negative discharge - mean and standard deviation)
*         (20) Bifurcation: abstraction of river flow distribution - first branch
*         (21) Bifurcation: abstraction of river flow distribution - second branch
*         (22) Bifurcation: abstraction of effluent flow distribution - first branch
*         (23) Bifurcation: abstraction of effluent flow distribution - second branch
*         (24) Sub-catchment boundary
*         (25) Switch on Diffuse Pollution  - agriculture (livestock)
*         (26) Switch off Diffuse Pollution - agriculture (livestock)
*         (27) Switch on Diffuse Pollution  - agriculture (arable)
*         (28) Switch off Diffuse Pollution - agriculture (arable)
*         (29) Switch on Diffuse Pollution  - highways
*         (30) Switch off Diffuse Pollution - highways
*         (31) Switch on Diffuse Pollution  - urban 
*         (32) Switch off Diffuse Pollution - urban
*         (33) Switch on Diffuse Pollution  - atmospheric deposition
*         (34) Switch off Diffuse Pollution - atmospheric deposition
*         (35) Switch on Diffuse Pollution  - background
*         (36) Switch off Diffuse Pollution - background
*         (46) Switch on Diffuse Pollution  - mines
*         (47) Switch off Diffuse Pollution - mines
*         (48) Switch on Diffuse Pollution  - birds, boats and angling
*         (49) Switch off Diffuse Pollution - birds, boats and angling
*         (37) Switch on Diffuse Pollution  - septic tanks
*         (38) Switch off Diffuse Pollution - septic tanks
*         (39) Mine waters
*         (40) Switch on Aggregated CSOs
*         (41) Switch off Aggregated CSOs
*         (42) Switch on Aggregated sewage works
*         (43) Switch off Aggregated sewage works
*         (44) Flow into a lake
*         (45) Flow from a lake and into a river
*         (50) Switch on User-named diffuse input
*         (51) Switch off User-named diffuse input
*         (52) Switch on User-named diffuse input 
*         (53) Switch off User-named diffuse input 
*         (54) Switch on -named diffuse input 
*         (55) Switch off -named diffuse input 
*         (56) Switch on User-named diffuse input 
*         (57) Switch off User-named diffuse input 
*         (58) Switch off User-named diffuse input 
*         (59) Switch on User-named diffuse input 
*         (60) Other point sources
*         (61) Private wastewaters


*     tributary, stream or upstream boundary --------------------------------------
      if ( it .eq. 2 .or. it .eq. 10 .or. it .eq. 45 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
 1238 format(I4,2X,A32,F7.2,F9.2,3x,i9,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     diffuse inputs -----------------------------------------------------------
      if ( it .eq. 25 .or. it .eq. 27 .or. 
     &     it .eq. 29 .or. it .eq. 31 .or.
     &     it .eq. 33 .or. it .eq. 35 .or.
     &     it .eq. 46 .or. it .eq. 15 .or. 
     &     it .eq. 40 .or. it .eq. 42 .or. it .eq. 48 .or.
     &     it .eq. 50 .or. it .eq. 52 .or. it .eq. 54 .or.
     &     it .eq. 56 .or. it .eq. 58 .or. 
     &     it .eq. 37 .or. it .eq. 13 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif
*     end of diffuse inputs ----------------------------------------------------      
      if ( it .eq. 26 .or. it .eq. 28 .or. 
     &     it .eq. 30 .or. it .eq. 32 .or.
     &     it .eq. 34 .or. it .eq. 36 .or.
     &     it .eq. 41 .or. it .eq. 43 .or.
     &     it .eq. 47 .or. it .eq. 16 .or. it .eq. 49 .or.
     &     it .eq. 51 .or. it .eq. 53 .or. it .eq. 55 .or.
     &     it .eq. 57 .or. it .eq. 59 .or. 
     &     it .eq. 38 .or. it .eq. 14) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     effluent discharge -------------------------------------------------------
*     apply to [3] [5] [12] [39] [15] [42] [60] [61]  [62} ------------- writing
      if ( it .eq. 03 .or. it .eq. 05 .or. it .eq. 12 .or.
     &     it .eq. 15 .or. it .eq. 39 .or. it .eq. 42 .or.
     &     it .eq. 60 .or. it .eq. 61 .or. it .eq. 62 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NE ) then
      write(08,1038)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),'-',IQ,
     &rname(jreach(itemp))
 1038 format(I4,2X,A32,F7.2,F9.2,11x,a1,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif
      if ( it .eq. 16 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     monitoring point ---------------------------------------------------------
      if ( it .eq. 1 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      write(08,1138)it,UNAME(itemp),'-',IQ,rname(jreach(itemp))
 1138 format(I4,2X,A32,6x,'-',8x,'-',11x,a1,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     interpolation point or discharge with zero flow --------------------------
      if ( IT .eq. 6 ) then
      write(08,2738)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
 2738 format(I4,2x,A32,6x,a1,8x,a1,11x,a1,8x,a1,6x,a16)
      endif
      if ( IT .eq. 14 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      if ( IT .eq. 16 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      if ( IT .eq. 17) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     flow into lake -----------------------------------------------------------
      if ( IT .eq. 44) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     flow gauge ---------------------------------------------------------------
      if ( IT .eq. 4 ) then
      IF = JF(itemp)
      if ( IF .gt. 0 .and. IF .le. NF ) then
      write(08,7338)it,UNAME(itemp),F(IF,1),F(IF,2),IF,
     &rname(jreach(itemp))
 7338 format(I4,2X,A32,F7.2,F9.2,3x,i9,8x,'-',6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
 2138 format(I4,2x,A32,6x,a1,8x,a1,11x,a1,8x,a1,6x,a16)
      endif
      endif

*     abstraction --------------------------------------------------------------
      if ( IT .eq. 7 ) then
      IF = JF(itemp)
      if ( IF .lt. 1 .or. IF .gt. NF ) then
      write(08,2238)it,UNAME(itemp),IF,rname(jreach(itemp))
 2238 format(I4,2X,A32,6X,'-',8X,'-',3x,i9,8x,'-',6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     abstraction (the negative discharge - river flow type (18) ---------------
      if ( IT .eq. 18 ) then
      IF = JF(itemp)
      if ( IF .gt. 0 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     abstraction (the negative discharge - effluent type (19) -----------------
      if ( IT .eq. 19 ) then
      IQ = JQ(itemp)  
      if ( IQ .gt. 0 .and. IQ .le. NE ) then
      write(08,1838)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1838 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     weir ---------------------------------------------------------------------
      if ( IT .eq. 8 ) then
      IQ = JQCAL(itemp)
      if ( IQ .gt. 1 .and. IQ .le. NV ) then
      write(08,7138)it,UNAME(itemp),IQ,rname(jreach(itemp))
 7138 format(I4,2X,A32,6X,'-',8X,'-',12X,9x,I9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     flow regulation point ----------------------------------------------------
      if ( IT .eq. 9) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1058)it,UNAME(itemp),F(IF,2),IF,IQ,rname(jreach(itemp))
 1058 format(I4,2X,A32,6X,'-',F9.2,3x,i9,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     three types of bifurcation -----------------------------------------------
      if ( IT .eq. 11 ) then
      write(08,2630)it,UNAME(itemp),rname(jreach(itemp))
 2630 format(I4,2X,A32,6X,'-',8X,'-',20x,'-',6x,a16)
      endif
      if ( IT .eq. 20 .or. IT .eq. 22 ) then
      write(08,1638)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1638 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      endif
      if ( IT .eq. 21 .or. IT .eq. 23 ) then
      write(08,1637)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1637 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      endif

   81 continue

      write(08,1022)
 1022 format(100('-')//)

      return
      end


*     set up arrays of default random normal deviates --------------------------
      subroutine set up default random normal deviates
      include 'COMMON DATA.FOR'
      dimension IR2(MP10), IR4(MP10)

*     river flow ---------------------------------------------------------------
      IR1 = FSTART
*     effluent flow ------------------------------------------------------------
      IR3 = ESTART
*     river temperature --------------------------------------------------------
      IRT = CSTART
*     river suspended solids ---------------------------------------------------
      IRS = USTART

*     loop on determinands and set the random number starters ------------------
      do jdet = 1, ndet
*     river quality ------------------------------------------------------------
      IR2(jdet) = DSTART(jdet) ! random number starters for river quality ------
*     effluent quality ---------------------------------------------------------
      IR4(jdet) = PSTART(jdet) ! random number starters for effluent quality ---
      enddo


*     loop on shots ... set the uncorrelated values of the normal deviates -----
*     set the values of FRAN, ERAN, CRAN, PRAN ---------------------------------
*     set the uncorrelated values of the normal deviates      
      kstop = 0
      do is = 1, NS
      call set the uncorrelated values of the normal deviates 
     &(IR1, IR2, IR3, IR4, IS, IRT, IRS )
      enddo
*     call write shots for river flow FRAN

*     test and adjust the shots for random errors ... river quality ------------
      do jp = 1, ndet
      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = CRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      !write(09,289) CM1, CM2
  289 Format('Calculated CRAN =',f12.6,8x,
     &       '           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25
      do is = 1,NS
      CRAN ( JP, IS ) = (CRAN ( JP, IS ) - CM1) 
      if ( CRAN ( JP, IS ) .gt. 0.0 ) then
      CRAN ( JP, IS )= CRAN ( JP, IS ) - add
      else
      CRAN ( JP, IS )= CRAN ( JP, IS ) + add
      endif
      enddo

      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = CRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo
*     write(01,289) CM1, CM2
      enddo

*     test the and adjust the shots ... discharge quality ----------------------
      do jp = 1, ndet
      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = PRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
*     if ( nobigout .le. 0 ) write(01,239) CM1, CM2
  239 Format('Calculated PRAN =',f12.6,8x,
     &'           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25

      do is = 1,NS
      PRAN ( JP, IS ) = (PRAN ( JP, IS ) - CM1) 
      if ( PRAN ( JP, IS ) .gt. 0.0 ) then
      PRAN ( JP, IS )= PRAN ( JP, IS ) - add
      else
      PRAN ( JP, IS )= PRAN ( JP, IS ) + add
      endif
      enddo

      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = PRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo
*     if ( nobigout .le. 0 ) write(01,239) CM1, CM2
      enddo

*     test the and adjust the shots ... discharge flow -------------------------
      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = ERAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
*     if ( nobigout .le. 0 ) write(01,259) CM1, CM2
  259 Format('Calculated ERAN =',f12.6,8x,
     &'           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25

      do is = 1,NS
      ERAN (IS) = (ERAN (IS) - CM1) 

      if ( ERAN (IS) .gt. 0.0 ) then
      ERAN (IS)= ERAN (IS) - add
      else
      ERAN (IS)= ERAN (IS) + add
      endif

      enddo

      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = ERAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo

*     test and adjust the shots for random errors ... river flow ---------------
      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = FRAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
*     if ( nobigout .le. 0 )write(01,299) CM1, CM2
  299 Format('Calculated FRAN =',f12.6,10x,
     &'...  Standard deviation =',f12.6)   
*     if ( MONF .gt. 1 ) call write shots for river flow FRAN
      do k = 1, 5
      add = (CM2 - 1.0) * 1.25
      do is = 1,NS
      FRAN (IS) = (FRAN (IS) - CM1) 
      if ( FRAN (IS) .gt. 0.0 ) then
      FRAN (IS)= FRAN (IS) - add
      else
      FRAN (IS)= FRAN (IS) + add
      endif
      enddo

      CM1 = 0.0
      CM2 = 0.0
      do is = 1, NS
      xx = FRAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
      enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
      enddo
     
      return
      end


*     set the uncorrelated values of the normal deviates -----------------------
      subroutine set the uncorrelated values of the normal deviates 
     &(IR1, IR2, IR3, IR4, IS, IRT, IRS )
      include 'COMMON DATA.FOR'
      
      dimension RR2(MP10), RR4(MP10)
      dimension IR2(MP10), IR4(MP10)

      RR1 = GAS1 (IR1) ! uncorrelated normal deviates for river flow -----------
      RR3 = GAS3 (IR3) ! uncorrelated normal deviates for discharge flow -------
      do jdet= 1, ndet
      call gas2dets (IR2(jdet),jdet,GAS2)      
      RR2(jdet) = GAS2 ! uncorrelated deviates for river quality -
      call gas4dets (IR4(jdet),jdet,GAS4)      
      RR4(jdet) = GAS4 
      enddo
      RR5 = GAS5 (IRT) ! temperature -------------------------------------------
      RR7 = GAS7 (IRS) ! suspended solids --------------------------------------

*     store the values ---------------------------------------------------------
      FRAN (IS) = RR1 ! uncorrelated normal deviates for river flow ------------
      ERAN (IS) = RR3 ! uncorrelated normal deviates for discharge flow --------
      do idet = 1, ndet ! ------------------------------------------------------
      CRAN (idet,IS) = RR2 (idet) ! uncorrelated deviates for river quality ----
      PRAN (idet,IS) = RR4 (idet) ! uncorrelated deviates for discharge quality-
      enddo ! ------------------------------------------------------------------

*     impose correlation between temperature and river flow --------------------
      TRAN (IS) = RR5 ! default random deviate for temperature
      RR6 = TCORF * RR1 + RR5 * SQRMB(125, 1.0 - TCORF * TCORF )
      TRAN (IS) = RR6 ! for temperature
      BMS global (1, IS) = TDEG + TSDEV * RR6
      
*     store for suspended solids -----------------------------------------------
      SRAN (IS) = RR7 ! uncorrelated normal deviates for suspended solids ------
      RR1 = FRAN (IS)
      RR6 = spcorrfs * RR1 + RR7 * SQRMB(111,1.0-spcorrfs*spcorrfs)
      
      GSCM = ALOG ( TDEG / SQRoot(1258, TDEG + TSDEV * TSDEV ) )
      GSCS = SQRoot(1259, ALOG ( 1.0 + (TSDEV*TSDEV )/TDEG ) )
      BMS global (2, IS) = exp ( RR6 * GSCS + GSCM )

      return
      end


*     get random numbers and impose correlation --------------------------------
      subroutine get the correlated random numbers (IS,R1,R2,R3,R4)
      include 'COMMON DATA.FOR'
      
      R1 = FRAN ( IS )
      JS = jset ( IQ, NS, IS )
      R2 = CRAN ( JP, JS )
      R3 = ERAN ( IS )
      R4 = PRAN ( JP, JS )
      
      call impose correlation 4 (R1,R2,R3,R4) ! 44444444444444444444444444444444
      
      return
      end


*     random number starters ---------------------------------------------------
      subroutine reset random number starters
      include 'COMMON DATA.FOR'

      FSTART = -5119 ! river flows (default)
      ESTART = -1849 ! effluent flows (default) 
      CSTART = -1937 ! temperature
      USTART = -3937 ! suspended solids

      DSTART(1) = -1649 ! river quality (default values)
      if ( MP10 .gt. 1 ) DSTART(2) = -7849
      if ( MP10 .gt. 2 ) DSTART(3) = -2849
      if ( MP10 .gt. 3 ) DSTART(4) = -3849
      if ( MP10 .gt. 4 ) DSTART(5) = -5849
      if ( MP10 .gt. 5 ) DSTART(6) = -6849
      if ( MP10 .gt. 6 ) DSTART(7) = -9849
      if ( MP10 .gt. 7 ) DSTART(8) = -2249
      if ( MP10 .gt. 8 ) DSTART(9) = -4449
      if ( MP10 .gt. 9 ) DSTART(10) = -8649

      PSTART(1) = -1749 ! effluent quality (default values)
      if ( MP10 .gt. 1 ) PSTART(2) = -5149
      if ( MP10 .gt. 2 ) PSTART(3) = -6249
      if ( MP10 .gt. 3 ) PSTART(4) = -2480
      if ( MP10 .gt. 4 ) PSTART(5) = -1894 
      if ( MP10 .gt. 5 ) PSTART(6) = -3349
      if ( MP10 .gt. 6 ) PSTART(7) = -4849
      if ( MP10 .gt. 7 ) PSTART(8) = -6242
      if ( MP10 .gt. 8 ) PSTART(9) = -9441
      if ( MP10 .gt. 9 ) PSTART(10) = -5649

      return
      end



*     set the starting values of variables -------------------------------------
      subroutine initialise variables
      include 'COMMON DATA.FOR'

      chekk infeezible = 0
      ihalt diff disch = 0
      maxistor = 0
      
      kerror = 0
      feeture = 0
      KFEAT = 0

      nostruct = 0
      ITER = 0 ! iteration counter for calculating required discharge quality
      
      KSIM = 0 ! counter for iterations of gap filling -------------------------
      MAXNITZ = 20 ! maximum number of iterations in Gap Filling ---------------
      JUgap = 0 ! number of a Feature used for Gap Filling 
      
      kountd1 = 0
      kountd2 = 0
      iqmon = 0
      
      struckd = 0
      tempd = 0
      seasd = 0
      NONPD = 0

      isupprezz = 0
      icp = 0
      do isup = 1, MP10
      kreplace (isup) = 0
      do itipe = 1, 9
      suppress (itipe,isup) = 0
      enddo
      enddo
      suppress1 = 0 ! illegal data for river flow 
      suppress3 = 0 ! huge change in flow 
      suppress4 = 0 ! no determinands for classification
      suppress5 = 0 ! error reading flow gap filling data
      suppress6 = 0 ! zero 95-percentile flow set to 1% of mean
      suppress7 = 0 ! unnecessary monthly structures
      suppress8 = 0 ! quality data not set for gap fill
      suppress9 = 0 ! failure to set up monthly structure
      suppress9a = 0 ! failure to set up monthly structure for temperature
      suppress9b = 0 ! failure to set up monthly structure for suspended solids
      suppress9c = 0 ! calculated discharge quality exceeds a huge number
      suppress9d = 0 ! calculated discharge quality exceeds a huge number
      suppress10 = 0 ! illegal data for river flow 
      suppress11 = 0 ! flow exceeds a billion 
      suppress12 = 0 ! problems with monthly structure 
      suppress13 = 0 ! zero entered as the 95-percentile river flow
      suppress14 = 0 ! infeasibly huge concentration 
      suppress15 = 0 ! huge loads produced by powe curve data on load 
      suppress16 = 0 ! unneeded effluent data ignored
      suppress17 = 0 ! deleted feature
      suppress18 = 0 ! zero variation in river quality
      suppress19 = 0 ! unneeded data ignored
      suppress20 = 0 ! infeasible correlation coefficients
      suppress21 = 0 ! no data for gap filling
      suppress22 = 0 ! data cannot be used to add diffuse pollution headwaters
      suppress23 = 0 ! unneeded data ignored
      suppress00 = 0 ! all the others
      
      xfact = 1.0 ! multiplier used to adjust catchment loads for bifurcations
      bifurcr = 0 ! indicator that the reach is a bifurcation
      ibifurcr2 = 0 ! indicator that the reach is a bifurcation
      flowhead = 0.0 ! flow at the head of a bifurcation
      itypbir = 0 ! type of bifurcation (20, 21, 22, etc)
      do iu = 1, NU
      uname(iu) = '.............'
      JT(iu) = 0
      JREACH(iu) = 0
      DIST(iu) = 0.0
      JF(iu) = 0
      JQ(iu) = 0
      JFCAL(iu) = 0
      JQCAL(iu) = 0
      IFRQS(iu) = 0 ! numbers for set of river quality targets for each feature
      GIScode(iu) = 'GIS code' 
      enddo
      
      do ir = 1, MR
      ADIST (ir) = 0.0
      enddo

*     initialise a control of messages about monthly data ----------------------
      jstructure message = 0

*     set the number of classes ------------------------------------------------
      nclass = 5

      IFDIST = 0
      IQDIST = 0
      if ( masterdata .eq. 1 .and. master set used .eq. 1) then
      masterIPRINT = IPRINT
      master output mode = output mode
      masterNS = NS
      masterIPUR = IPUR
      masterKINT = KINT
      endif

*     if "ifbatch" is 1 this is a batch run ------------------------------------
      if ( ifbatch .eq. 1 .and. master set used .eq. 1 ) then
      if ( model number in batch .gt. 1 ) then
      IPRINT = masterIPRINT
      output mode = master output mode
      NS = masterNS
      IPUR = masterIPUR
      KINT = masterKINT
      endif
      endif

      call set indices for percentiles etc ! ------------------------ initialise

*     indicator that there are too many monitoring stations --------------------
      imm = 0

*     set the correlation coefficients -----------------------------------------
*     these are default values which are overwritten by the values in ----------
*     the variables RFCL and EFCL in Suite FEATURE.FOR (prior to ---------------
*     Mass Balance.FOR) --------------------------------------------------------
*     or values read in for a particular site .... (this not yet done) ---------

      CO1 = 0.0 ! river flow on river quality
      CO2 = 0.0 ! river flow on discharge flow
      CO3 = 0.0 ! river flow on discharge quality ! 4444444444444444444444444444
      CO4 = 0.0 ! river quality on discharge flow ! 4444444444444444444444444444
      CO5 = 0.0 ! discharge flow on discharge quality
      CO6 = 0.0 ! river quality on discharge quality ! 4444444444444444444444444

      LMONP = 0 ! counter for sampling points ----------------------------------
      NGAUGE = 0 ! counter for flow gauges -------------------------------------
      NREACH = 0 ! eventually NREACH will hold the number of Reaches -----------
      NDET = 0 ! eventually NDET will hold the number of determinands ----------

      do if = 1, nf
*     distribution indicators for river flows ----------------------------------
      PDRF (if) = -999
      do imon = 1, MO
*     summary statistics for river flows ---------------------------------------
      F(if,1) = 0.0
      enddo
      enddo

      do ii = 1, 5
      FLOW(ii) = 0.0
      enddo

*     initialise shots ---------------------------------------------------------
      do is = 1, MS
      FMS(IS) = 0.0
      FMX(IS) = 0.0
      EFMS(IS) = 0.0 ! proportions of effluent flow in river flow --------------
      ECshots(IS) = 0.0 ! effluent quality
      do idet = 1, MP10
      CMS(idet,IS) = 0.0
      CMX(idet,IS) = 0.0
      do ip = 1, n2prop
      CTMS(ip,iDET,IS) = 0.0 ! concentrations from various types of feature -----
      UCTMS(ip,iDET,IS) = 0.0 !  u/s concentrations from various types of feature 
      enddo
      enddo
      enddo
      
      do idet = 1, MP10
      do imom = 1,5
      C(idet,imom) = 0.0
      enddo
      enddo

*     distribution indicators for discharge flows ------------------------------
      do ie = 1, ne
      PDEF(ie) = -1 ! ----------------------------------------------- initialise
      do imon = 1, MO
      FE(ie,imon) = 0.0      
      enddo
      enddo

*     distribution indicators for river quality --------------------------------
      do iv = 1, nv
      do ip = 1, mp10
      PDRC (iv,ip) = -1 ! types of sets of data on river quality ---------------
      do imon = 1, MO + 3
      quolity data(iv,ip,imon) = 0.0
      enddo
      powerbase(iv,ip) = 0.0
      powermini(iv,ip) = 0.0
      enddo
      enddo
      
      do ip = 1, n2prop
      do idet = 1, mp10
      do ii = 1,5    
      CMcontrib(ip,idet,ii) = 0.0
      enddo
      do ii = 1,2    
      LMcontrib(ip,idet,ii) = 0.0
      enddo
      enddo
      enddo

*     storage for data on non-parametric distributions -------------------------
      do 6 k = 1, M7

*     river flows --------------------------------------------------------------
      flname(1,k) = '            '
      flnamesmall(1,k) = '            '
      flsequence(1,m7) = 0
*     river quality ------------------------------------------------------------
      flname(2,k) = '            '
      flnamesmall(2,k) = '            '
      flsequence(2,m7) = 0
*     discharge flow -----------------------------------------------------------
      flname(3,k) = '            '
      flnamesmall(3,k) = '            '
      flsequence(3,m7) = 0
*     discharge quality --------------------------------------------------------
      flname(4,k) = '            '
      flnamesmall(4,k) = '            '
      flsequence(4,m7) = 0

      do j = 1, MP10 + 1
      idenp(1,k,j) = 0
      idenp(2,k,j) = 0
      enddo

    6 continue

*     storage for monthly data -------------------------------------------------
      do 96 k = 1, M8
      flmonth(1,k) = 'File not read'
      flmonth(2,k) = 'File not read'
      flmonth(3,k) = 'File not read'
      flmonth(4,k) = 'File not read'
      flmonthsmall(1,k) = 'File not read'
      flmonthsmall(2,k) = 'File not read'
      flmonthsmall(3,k) = 'File not read'
      flmonthsmall(4,k) = 'File not read'
      do j = 1, MP10 + 1
      iseasp(1,k,j) = 0
      iseasp(2,k,j) = 0
      enddo
   96 continue

*     storage for monthly structure data ---------------------------------------
      do 99 k = 1, M9
      FLSTRUCT(1,k) = 'File not read'
      FLSTRUCT(2,k) = 'File not read'
      FLSTRUCT(3,k) = 'File not read'
      FLSTRUCT(4,k) = 'File not read'
      FLSTRUCTsmall(1,k) = 'File not read'
      FLSTRUCTsmall(2,k) = 'File not read'
      FLSTRUCTsmall(3,k) = 'File not read'
      FLSTRUCTsmall(4,k) = 'File not read'
      do j = 1, MP10 + 1
      istruct(1,k,j) = 0
      istruct(2,k,j) = 0
      enddo
   99 continue

      do 9 JP = 1,MP10 ! loop on number of determinands ------------------------
      QUALN(JP) = 0.0 ! initial values of running sampling rates ---------------
      QNAT(JP) = 0.0 ! per cent changes in quality brought about by gap fillin -
      do IV = 1, NV ! loop on number of sets of quality data for rivers --------
      QNUM(IV,JP) = -1 ! initial values of sampling rates ----------------------
      enddo
      do ip = 1, n2prop ! loop on number of types of feature -------------------
      QUALNB(JP,ip) = 0.0 ! initial sampling rates for types of feature --------
      enddo
    9 continue

      do JP = 1,MP10 ! .........................................................
      do IV = 1,NV
      powerbase(IV,JP) = 0.0
      powermini(IV,JP) = 0.0
      do JM = 1,MO
*     arrays for storing quality data for rivers and streams -------------------
      quolity data(IV,JP,JM)=-1.0E10
      enddo
      enddo
      do IE = 1,NE
      PDEC(IE,JP) = -1999 ! distribution indicators for discharge quality ------
      do JM = 1,MO
*     arrays for storing quality data for effluents ----------------------------
      pollution data(IE,JP,JM)=-1.0E10 ! storing quality data for effluents ----
      enddo ! do JM = 1,MO
      enddo ! do IE = 1,NE
      enddo ! do JP = 1,MP10 ...................................................
      
      do IE=1,NE ! .............................................................
      ENUM(IE) = -1
      do JP = JP, MP10
      PNUM(IE,JP) = -1
      enddo ! do JP = JP, MP10
      enddo ! do IE=1,NE .......................................................

*     background river quality data for reaches --------------------------------
      do iip = 1,3 ! ...........................................................
      do IR = 1,MR
      BNUM(IR,iip) = -1 
      PDBC(IR,iip) = 1
      do IM = 1,3
      BMAT(IR,iip,IM) = -1.0E10
      enddo ! do IM = 1,3
      enddo ! do IR = 1,MR
      enddo ! do iip = 1,3 .....................................................

      do IR = 1,MR
      do idet = 1, MP10
      QDN(idet,IR) = 0.0 ! sampling rates at end of reaches --------------------
      enddo
      enddo ! do IR = 1,MR

      number of stds = 0 ! marker that a reach has specific standards ----------
      do IR = 1, MR
      do idet = 1, MP10
      EQS reach (IR,idet) = 0 ! standards specified for individual reaches -----
      enddo
      do ic = 1, NC
      standards for reach (IR,ic) = 1.0e10
      enddo
      enddo ! do IR = 1, MR
     
*     river quality targets ----------------------------------------------------
      do ID = 1, MP10 ! ........................................................
      do IQ = 1, NQ
      RQS(IQ,ID) = 0.0
      enddo
*     Type of summary statistic for river quality targets. These will be -------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile-             
      MRQS(ID) = -99 ! initialise code for summary statistic for river targets -
      enddo ! do ID = 1, MP10 ..................................................

*     initialise river quality targets for graph plotting ---------------------- 
      do IQ = 1, MP10
      RQO(IQ) = 0.0
*     Type of summary statistic for river quality targets. These will be -------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile-             
      MRQO(IQ) = 0
      enddo ! do IQ = 1, MP10


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     pre-set the gap filling switches -----------------------------------------
      do itemp = 1,NU
      KFCAL(itemp) = 0
      KQCAL(itemp) = 0
      SKIPPEFF(itemp) = 0 ! device for ignoring discharges in Modes 7 and 8 ----
*     data-sets for gap filling ------------------------------------------------
      JFUSER(itemp) = 0
      JQUSER(itemp) = 0
      enddo ! do itemp = 1,NU
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



*     arrays for storing data for the end of a reach ---------------------------
      do 6629 IR = 1, KR
      JSTOR(IR) = 0 ! initialise store for reaches needed downstream -----------
      do JP = 1,MP10
      do J1 = 1, N13
      TNLOADUP3(IR,JP,J1) = 0.0
      TNLOADDN3(IR,JP,J1) = 0.0
      TALOADDN3(IR,JP,J1) = 0.0
      TILOADDN3(IR,JP,J1) = 0.0
      TBLODE3  (IR,JP,J1) = 0.0
      enddo ! do J1 = 1, N13
      
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments at the ends of reaches --------
      TWloadsrch(ibodies,IR,JP) = 0.0
      do ip = 1, n2prop
*     breakdown of monthly loads from sub-catchments at ends of reaches --------
      TWloadsrchapp(ibodies,IR,JP,ip) = 0.0
      enddo ! do ip = 1, nprop
      enddo ! do ibodies = 1, NUW
      enddo ! do JP = 1,MP10

      do ip = 1, nprop
      nbodyprop(ip) = 0 ! nunbers of features in water bodies
      enddo
      
      do IS = 1, MS
      FD(IR,IS) = 0.0
*     proportion of effluent ---------------------------------------------------
      EFD(IR,IS) = 0.0
      do JP = 1,MP10
      QD(IR,JP,IS) = 0.0
      do ip = 1, n2prop
      EQD(ip,IR,JP,IS) = 0.0
      enddo ! do ip = 1, n2prop
      enddo ! do JP = 1,MP10
      enddo ! do IS = 1, MS

 6629 continue ! do 6629 IR = 1, KR

      do IREACH = 1,MR
      IRHOLD (IREACH) = 0 ! used to store the data for a reach that is needed --
      enddo ! do IREACH = 1,MR

      do IP = 1, NP
      DNAME(IP)='.......    '   
      enddo

 9899 continue

*     number of determinands ---------------------------------------------------
      NDET = 0
      NP = 0

*     initialise QTYPE and Detype ----------------------------------------------
      do jp = 1, MP10
      Detype (jp) = 0
      Qtype (jp) = 4
      units (jp) = "----"
      enddo

      NONPD = 0 ! number of data-sets for non-parametric distributions ---------
      seasd = 0 ! number of data-sets for monthly data -------------------------

*     initialise the face value to class one -----------------------------------
      Face value = 0
      Face confidence = 999.0
      Face determinand = 0
      do idet = 1, MP10
      Face Value dets (idet) = 0
      enddo

*     intialise the diffuse pollution to be added to headwaters ================
      do ir = 1, MR
      do j = 1, 20
      diffheadqual(ir,j,1) = 0
      diffheadqual(ir,j,2) = 0
      diffheadqual(ir,j,3) = 0
      diffheadqual(ir,j,4) = 0
      enddo
      enddo ! intialise the diffuse pollution to be added to headwaters ========

*     sequence of output for apportioning of types of polution =================
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
*     (33) = 'Total from diffuse pollutions (NTF)    (25,27,etc)' 25
*     -------------------------------------diq-------------------------------------
*     (17) = 'Boundaries and tributaries              (2 and 10)' 26
*     (18) = 'Diffuse input                         Feature (13)' 27
*     (19) = 'Diffuse input                         Feature (15)' 28
*     (20) = 'Reach diffuse input              (no Feature code)' 29
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (21) = 'Added by gap filling of flows    (no Feature code)'
*     (22) = 'Added by gap filling of quality  (no Feature code)'
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     (32) = 'Grand total (NTD,32)                     (everything)' 30
*     ==========================================================================

      seqappbodies (01) = 02 ! sewage works
      seqappbodies (02) = 03 ! intermittent discharges of sewage (12)
      seqappbodies (03) = 04 ! industrial discharges (5)
      seqappbodies (04) = 05 ! mine waters (39)
      seqappbodies (05) = 28 ! other point sources (60)
      seqappbodies (06) = 29 ! private wastewaters (61)
      seqappbodies (07) = 30 ! fish farms (62)
      seqappbodies (08) = 01 ! all discharges (3,12,5,39,60,61,62)
      seqappbodies (09) = 06 ! diffuse pollution from livestock (25)
      seqappbodies (10) = 07 ! diffuse pollution from arable (27)
      seqappbodies (11) = 08 ! highway runoff (29)
      seqappbodies (12) = 09 ! urban runoff (31)
      seqappbodies (13) = 10 ! atmosphere deposition (33)
      seqappbodies (14) = 11 ! natural background (35)
      seqappbodies (15) = 12 ! septic tanks (37)
      seqappbodies (16) = 13 ! aggregated CSOs (40)
      seqappbodies (17) = 14 ! aggregated STWs (42)
      seqappbodies (18) = 15 ! diffuse mines (46)
      seqappbodies (19) = 16 ! Birds, boats and angling (48)
      seqappbodies (20) = 23 ! User-named diffuse input (50)
      seqappbodies (21) = 24 ! User-named diffuse input (52)
      seqappbodies (22) = 25 ! User-named diffuse input (54)
      seqappbodies (23) = 26 ! User-named diffuse input (56)
      seqappbodies (24) = 27 ! User-named diffuse input (58)
      seqappbodies (25) = 33 ! total from diffuse pollutions
      seqappbodies (26) = 17 ! boundaries (10) and tributaries (2)
      seqappbodies (27) = 18 ! diffuse input (13)
      seqappbodies (28) = 19 ! diffuse input (15)
      seqappbodies (29) = 20 ! reach diffuse input
      seqappbodies (30) = 21 ! added by gap filling of flows
      seqappbodies (31) = 22 ! added by gap filling of quality
      seqappbodies (32) = 32 ! grand totals

      return
      end






*     set up code numbers for determinands -------------------------------------
*     and assign correlation coefficients --------------------------------------
      subroutine set determinand codes (idet)
      include 'COMMON DATA.FOR'
 
      ndetBOD = 0
      if ( qtype (idet) .ne. 4 ) then

*     default correlation coefficients -----------------------------------------
      RFCL(idet) = 0.0
      EFCL(idet) = 0.0
      
      QZEROGAP(idet) = amax1 (QZEROGAP(idet), 0.00000001)

*     chloride -----------------------------------------------------------------
      if (DNA(idet) .eq. 'CL  ' ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' CL ' ) Detype (idet) = 100
      if (DNA(idet) .eq. '  CL' ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'CL '  ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' CL'  ) Detype (idet) = 100
      if (DNA(idet) .eq. 'CL'   ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'Cl  ' ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' Cl ' ) Detype (idet) = 100
      if (DNA(idet) .eq. '  Cl' ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'Cl '  ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' Cl'  ) Detype (idet) = 100
      if (DNA(idet) .eq. 'Cl'   ) Detype (idet) = 100 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 100 ) then
      RFCL(idet) = -0.3 ! chloride
      EFCL(idet) = -0.1 ! chloride 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! chloride

*     Biochemical Oxygen Demand ------------------------------------------------
      if (DNA(idet) .eq. 'BOD ' ) Detype (idet) = 101 
      if (DNA(idet) .eq. ' BOD' ) Detype (idet) = 101 
      if (DNA(idet) .eq. 'BOD'  ) Detype (idet) = 101 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 101 ) then
      RFCL(idet) = 0.3 ! BOD
      EFCL(idet) = 0.3 ! BOD
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! BOD

*     TOC ----------------------------------------------------------------------
      if (DNA(idet) .eq. 'TOC ' ) Detype (idet) = 102 
      if (DNA(idet) .eq. ' TOC' ) Detype (idet) = 102 
      if (DNA(idet) .eq. 'TOC'  ) Detype (idet) = 102 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 102 ) then
      RFCL(idet) = 0.3 ! TOC 
      EFCL(idet) = 0.3 ! TOC 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! TOC

*     ammonia ------------------------------------------------------------------
      if (DNA(idet) .eq. 'AMMN' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' AMM' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AmmN' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' Amm' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'Ammn' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4 ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' NH4' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AMM ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3 ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' NH3' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3N' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4+' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AMM'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'Amm'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3'  ) Detype (idet) = 103 
*     assign default correlation coefficients ----------------------------------
      if ( detype (idet) .eq. 103 ) then
      RFCL(idet) = 0.35 ! ammonia
      EFCL(idet) = 0.4 ! ammonia 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      call change colour of text (20) ! bright red
      write( *,1)dname(idet)
      call set screen text colour
    1 format('*** Determinand type corrected to 2',16x,'...',7x,a11,
     &32x,25('.'))
      endif
      endif ! ammonia

*     dissolved oxygen ---------------------------------------------------------
      if (DNA(idet) .eq. 'DO  ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. '  DO' ) Detype (idet) = 104
      if (DNA(idet) .eq. ' DO ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DOX ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. ' DOX' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O.' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DO '  ) Detype (idet) = 104 
      if (DNA(idet) .eq. ' DO'  ) Detype (idet) = 104
      if (DNA(idet) .eq. 'DO'   ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DOX'  ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O.' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O'  ) Detype (idet) = 104 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 104 ) then
      RFCL(idet) = 0.3 ! dissolved oxygen
      EFCL(idet) = 0.0 ! dissolved oxygen 
      endif ! dissolved oxygen

*     tests --------------------------------------------------------------------
      if (DNA(idet) .eq. ' DT1' ) Detype (idet) = 365 
      if (DNA(idet) .eq. ' DT2' ) Detype (idet) = 365 
      if (DNA(idet) .eq. ' DT3' ) Detype (idet) = 365 
      if (DNA(idet) .eq. ' DT4' ) Detype (idet) = 365 
      if (DNA(idet) .eq. ' DT5' ) Detype (idet) = 365 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 365 ) then
      RFCL(idet) = -0.3 ! phosphate 
      EFCL(idet) = -0.1 ! phosphate 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! tests
      
      
*     phosphate ----------------------------------------------------------------
      if (DNA(idet) .eq. 'PO4P' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PO4 ' ) Detype (idet) = 105 
      if (DNA(idet) .eq. ' PO4' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'SRP ' ) Detype (idet) = 105 
      if (DNA(idet) .eq. ' SRP' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'TOTP' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PHOS' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PO4'  ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'SRP'  ) Detype (idet) = 105 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 105 ) then
      RFCL(idet) = -0.3 ! phosphate 
      EFCL(idet) = -0.1 ! phosphate 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! phosphate

*     nitrate ------------------------------------------------------------------
      if (DNA(idet) .eq. 'NO3N' ) Detype (idet) = 106
      if (DNA(idet) .eq. ' NO3' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'TON ' ) Detype (idet) = 106
      if (DNA(idet) .eq. ' TON' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'NO3 ' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'NO3'  ) Detype (idet) = 106
      if (DNA(idet) .eq. 'TON'  ) Detype (idet) = 106
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 106 ) then
      RFCL(idet) = 0.6 ! nitrate
      EFCL(idet) = -0.2 ! nitrate 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write( *,1)dname(idet)
      endif
      endif ! nitrate ----------------------------------------------------------

*     parent substances (where losses are added to another substance -----------
      if (DNA(idet) .eq. 'PAR ' ) Detype (idet) = 200 ! parent
      if (DNA(idet) .eq. ' PAR' ) Detype (idet) = 200 ! parent
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 200 ) then
      RFCL(idet) = 0.0 ! parent substances
      EFCL(idet) = 0.0 ! parent substances 
      endif ! parent substances

*     daughter substances (which receive additions from losses from a "parent")
      if (DNA(idet) .eq. 'DAU ' ) Detype (idet) = 201 ! daughter
      if (DNA(idet) .eq. ' DAU' ) Detype (idet) = 201 ! daughter
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 201 ) then
      RFCL(idet) = 0.0 ! daughter substances
      EFCL(idet) = 0.0 ! daughter substances 
      endif ! daughter substances ----------------------------------------------

*     identify the positioning of BOD ------------------------------------------
      if ( detype (idet) .eq. 101 ) ndetBOD = idet
      
      endif ! if ( qtype (idet) .ne. 4 )
      
      return
      end









*     check first four characters of a line of input data ----------------------
*     skip comment lines -------------------------------------------------------

      subroutine check first four characters of a line of data (IFIN)
      character * 4 Line
      IFIN = 0
    1 read(02,2,end=9)Line
    2 format(A4)
      if ( Line .eq. '====' ) goto 1
      if ( Line .eq. ' ===' ) goto 1
      if ( Line .eq. '----' ) goto 1
      if ( Line .eq. "####" ) goto 1
      if ( Line .eq. '****' ) then
      IFIN = 1
      return
      endif
      backspace 02
      return
    9 write( *,8)
      write(01,8)
      write(09,8)
      write(31,8)
    8 format(77('-')/
     &'*** Unexpected end to data file ...'/
     &'*** Incomplete data on targets ?'/
     &'*** No terminating line of asterisks ?'/77('-'))
      call stop
      end





*     check for and read reach-specific data for temperature, standards etc ----
*     check first four characters of a line of input data ----------------------
*     skip comment lines -------------------------------------------------------
      subroutine check for reach data on background (KREACH,LTEST) 
      include 'COMMON DATA.FOR'
      character *1 char(20)
      character *20 tname
      dimension value(4)
	
      ii1=0
      ii2=0
      ii3=0
      ii4=0
      do ii = 1, 4
      value(ii) = 0.0
      enddo
	
    1 read(02,7,end=9)line of data
    7 format(a200)
      call leadblanks2 ! eliminate leading blanks from a line of data
      read(line of data,2,end=9)(char(i),i=1,20)
    2 format(20a1) 
      
*     check for background data ------------------------------------------------
      do ichar = 1, 10
      if ( char(ichar) .ne. ' ' ) then
      if ( char(ichar) .eq. '0' .or. char(ichar) .eq. '1' .or.
     &     char(ichar) .eq. '2' .or. char(ichar) .eq. '3' .or.
     &     char(ichar) .eq. '4' .or. char(ichar) .eq. '5' .or.
     &     char(ichar) .eq. '6' .or. char(ichar) .eq. '7' .or.
     &     char(ichar) .eq. '8' .or. char(ichar) .eq. '9' ) then
      backspace 02
      LTEST = 1
      return
      endif
      jchar = ichar
      if ( char(ichar) .eq. "'" ) goto 3
      endif
      enddo
      
    3 continue
*     temperature --------------------------------------------------------------
      if ( char(jchar+1) .eq. 't' .or. char(jchar+1) .eq. 'T' ) ii1 = 1
      if ( char(jchar+2) .eq. 'e' .or. char(jchar+2) .eq. 'E' ) ii2 = 1
      if ( char(jchar+3) .eq. 'm' .or. char(jchar+3) .eq. 'M' ) ii3 = 1
      if ( char(jchar+4) .eq. 'p' .or. char(jchar+4) .eq. 'P' ) ii4 = 1
*     suspended solids ---------------------------------------------------------
      if ( char(jchar+1) .eq. 's' .or. char(jchar+1) .eq. 'S' ) ii1 = 2
      if ( char(jchar+2) .eq. 'u' .or. char(jchar+2) .eq. 'U' ) ii2 = 1
      if ( char(jchar+3) .eq. 's' .or. char(jchar+3) .eq. 'S' ) ii3 = 1
      if ( char(jchar+4) .eq. 'p' .or. char(jchar+4) .eq. 'P' ) ii4 = 1
*     reach-specific standards for river water quality --------------------------
      if ( char(jchar+1) .eq. 's' .or. char(jchar+1) .eq. 'S' ) ii1 = 2
      if ( char(jchar+2) .eq. 't' .or. char(jchar+2) .eq. 'T' ) ii2 = 2
      if ( char(jchar+3) .eq. 'a' .or. char(jchar+3) .eq. 'A' ) ii3 = 2
      if ( char(jchar+4) .eq. 'n' .or. char(jchar+4) .eq. 'N' ) ii4 = 1
      
      ii1 = ii1 * ii2 * ii3 * ii4 

      if ( ii1 .eq. 0 ) then
      LTEST = 1
      return
      endif
      
*     ===========================================temperature or suspended solids
      if ( ii1 .eq. 1 .or. ii1 .eq. 2 ) then  
      backspace 02
      read(02,*,err=96,end=9)tname,idist
      if ( idist .eq. 0 ) idist = 1
      if ( idist .eq. 3 ) idist = 2
*     normal or log normal distributions ---------------------------------------
      if ( idist .eq. 1 .or. idist .eq. 2 ) then
      backspace 02
      read(02,*,err=16,end=9)tname,idist,value(1),value(2),value(3),
     &value(4)
      goto 26
   16 continue
      value(4) = 36.0
      backspace 02
      read(02,*,err=36,end=9)tname,idist,value(1),value(2),value(3)
      goto 26
   36 continue
      value(3) = -0.6
      backspace 02
      read(02,*,err=36,end=9)tname,idist,value(1),value(2)
   26 continue
*     check and correct default correlation ------------------------------------
      if ( value(3) .gt. 1.0 .or. value(3) .lt. -1.0 ) then
      value(3) = -0.6
      endif
*     check for implausible data -----------------------------------------------
      if ( idist .gt. 0 ) then
      if ( value(2) .lt. 0.00000001 .and. value(1) .gt. 0.000001 ) then
      suppress18 = suppress18 + 1 ! zero variation in temperature etc
      if ( ii1 .eq. 1 ) write(33,6317)
 6317 format(/77('-')/
     &'*** Implausible (zero) standard deviation for river ', ! temperature ----
     &'temperature ... '/77('-'))
      if ( ii1 .eq. 2 ) write(33,6347)
 6347 format(/77('-')/
     &'*** Implausible (zero) standard deviation for river ', ! suspended solids
     &'suspended solids ... ')
      if ( toveride .eq. 1 ) then
      value(2) = 0.25 * value(1)
      call sort format 1 (value(1))
      write(33,6288)valchars10
 6288 format('*** Over-written as 25% of the mean of ',a10)
      endif
      write(33,6399)
 6399 format(77('-'))
      endif
      endif ! if ( idist .gt. 0 )
*     load river quality data into SIMCAT's storage ----------------------------
      do JM = 1, 3
      BMAT ( KREACH, ii1, JM ) = value(JM)
      enddo
*     set the distribution to normal -------------------------------------------
      PDBC ( KREACH, ii1) = idist
      BNUM ( KREACH, ii2) = value(4)
      return
      endif
*     --------------------------------------- normal or log normal distributions

*     monthly structure distribution for background ----------------------------
      if ( idist .eq. 8 ) then ! for background
      backspace 02
      call get monthly structure file for background (KREACH, ii1)  
      call set up default random normal deviates
      return   
      endif
      endif ! temperature or suspended solids
   
*     read the reach-specific standards for river water quality ================
      if ( ii1 .eq. 8 ) then ! from the number of characters identified --------
      number of stds = number of stds + 1 ! the reach has specific standards ---
      backspace 02
      read(02,*,err=86,end=9)tname,iidet,iclass,(value(i),i=1,iclass)
      EQS reach (KREACH,iidet) = number of stds ! for this reach and determinand
      if ( iclass .eq. 1 ) then
      Value(iclass) = - abs (value(iclass)) ! river quality target for the reach
      endif
      icheck = 0
      do i = 1, iclass ! loop on classes ... look for target 
      if ( value(i) .lt. -1.0e-10 .and. icheck .eq. 0 ) icheck = i 
      standards for reach (number of stds, i) = abs (value(i)) ! target found
      enddo
      if ( icheck .eq. 0 ) then ! set target to Class 2 by default -------------
      standards for reach (number of stds, 2) = 
     & - standards for reach (number of stds, 2)
      value(2) = -value(2)
      else
      standards for reach (number of stds, icheck) = ! target class ------------
     & - standards for reach (number of stds, icheck)
      endif
      
      return
      endif   
*     ================ read the reach-specific standards for river water quality
      
      return

*     --------------------------------------------------------------------------
   96 continue
      write( *,18)
   18 format(77('=')/
     &'*** Error in reading data on background for reaches ...'/
     &'*** Check the name of the reach is not "Temperature" etc ...'/
     &77('='))
      backspace 02
      call close down ! on error in reading data on background for reaches
      return
   86 continue
      write( *,78)
   78 format(77('=')/
     &'*** Error in reading data on reach-specific standards ...'/
     &77('='))
      backspace 02
      return
*     --------------------------------------------------------------------------
    9 continue
      write( *,8)
    8 format(77('-')/
     &'*** Unexpected end to the SIMCAT main data file ...'/
     &77('-'))
      call stop
      end
*     --------------------------------------------------------------------------




      subroutine extract a line of data (line)
      character * 200 Line
      IFIN=0
    1 read(02,2)Line
    2 format(A200)
      return
      end