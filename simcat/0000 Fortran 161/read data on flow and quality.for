*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: read data on flow and quality.for ... 4263 lines -------------------
*     --------------------------------------------------------------------------
*     This file reads, checks and sets up data on flow and quality -------------
*     --------------------------------------------------------------------------
*     This file contains 18 ......s ---------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... read river flows ! -------------------------------------------
*     ...... read data on river quality (MV) ! ----------------------------
*     ...... read effluent data ! -----------------------------------------
*     ...... read river targets ! -----------------------------------------
*     ...... read the non parametric file on river flow ! ---------- type 4
*     ...... get monthly file for river flow ! --------------------- type 5
*     ...... get monthly structure file for river flow ! ----------- type 8
*     ...... get monthly structure file for river quality ! -------- type 8 
*     ...... get monthly structure file for background ! ----------- type 8   
*     ...... get a non parametric file for river quality ! --------- type 4
*     ...... get monthly file for river quality ! ------------------ type 5
*     ...... get non parametric file for discharge flow ! ---------- type 4
*     ...... get monthly file for discharge flow ! ----------------- type 5
*     ...... get monthly structure file for discharge flow ! ------- type 8
*     ...... get non parametric file for discharge quality ! ------- type 4
*     ...... get non parametric file for discharge load ! ---------- type 4
*     ...... get monthly file for discharge quality ! -------------- type 5
*     ...... get monthly structure file for discharge quality ! ---- type 8
*     --------------------------------------------------------------------------

*     read the data on river flow ----------------------------------------------
      subroutine read river flows

*     --------------------------------------------------------------------------
*     types of distributions for river flow ------------------------------------
*     --------------------------------------------------------------------------
*                       0 - constant
*                       1 - normal
*                       2 - log-normal
*                       3 - three-parameter log-normal
*                       4 - non-parametric distribution
*                       5 - monthly distribution
*                       8 - monthly structure
*                      10 - power curve distribution 99999999 not used 999999999
*     --------------------------------------------------------------------------
      
      include 'COMMON DATA.FOR'

      ihalt diffuse river = 0
      ihalt diffuse effluent = 0

*     initialise checker for duplicates ----------------------------------------
      do IV = 1, MT
      MK(IV) = 0
      MKF(IV) = 0
      enddo
      NMKF = 0 ! ---------------------------------------------------- river flow

*     read a line of river flow data -------------------------------------------
 7814 call check first four characters of a line of data (IFIN)

*     check for the end of the river flow data ---------------------------------
      if ( IFIN .eq. 1 ) return
      NMKF = NMKF + 1 ! --------------------------------------------- river flow

*     read the reference number for river flow dataset -------------------------
      read(02,*, ERR = 8313 ) IVECT

*     test for a valid reference number for the set of river flow data ---------
*     ensure that the array boundaries are not exceeded ------------------------
      if ( IVECT .lt. 0 ) goto 7814     
      if ( IVECT .gt. NF ) then
      write( *,7807)IVECT
      write(01,7807)IVECT
      write(09,7807)IVECT
 7807 format(77('-')/
     &'*** A river flow data-set has an illegal code number '/
     &'*** The number is',I6,'                                '/
     &'*** The dataset has been removed ...'/77('-'))
      goto 7814
      endif
*     check for duplicate reference numbers ========================= river flow
      if ( MK(IVECT) .ne. 0 ) then ! ------- check duplicates ------- river flow
      call change colour of text (20) ! bright red
      write( *,7898)IVECT
 7898 format(
     &'*** River flow data specified more than ',
     &'once',7x,'...',7x,'Set number',i7,' .......',19x,25('.'))
      call set screen text colour
      write(01,7808)IVECT
      write(09,7808)IVECT
      write(33,7808)IVECT
 7808 format(77('-')/
     &'*** A river flow data-set has been specified more than ',
     &'once ... '/'*** Only the first is retained ... ',
     &'Check dataset number ',i6/77('-'))
      goto 7814
      endif ! if ( MK(IVECT) .ne. 0 ) ======= check duplicates ====== river flow

*     add 1 to the river flow counter ------------------------------------------
      MK(IVECT) = MK(IVECT)+1 ! ----------- check duplicates -------- river flow
      MKF(NMKF) = IVECT ! ------------------------------------------- river flow
      backspace 02

*     read the code which defines the distribution of river flow ---------------
      read(02,*, ERR = 8313 ) IVECT, jpd

*     check for an illegal distribution code -----------------------------------
      if ( jpd .lt. 0 .or. jpd .gt. 9 .or. jpd .eq. 6 .or. ! 999999999999999999
     &jpd .eq. 7 .or. jpd .eq. 9 ) then ! 99999999999999999999999999999999999999
      write( *,8851)IVECT
      write(01,8851)IVECT
      write(09,8851)IVECT
      write(33,8851)IVECT
 8851 format(77('-')/
     &'*** Illegal code for type of distribution for river flow'/
     &'*** Check dataset number',i6/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif
      backspace 02

*     special treatment for non-parametric distributions (ie. jpd = 4) ---------
      if ( jpd .eq. 4) then
      call read the non parametric file on river flow
      goto 7814
      endif
      
*     special treatment for monthly data (ie. jpd = 5) for river flows ---------
      if ( jpd .eq. 5 ) then
      call get monthly file for river flow
      goto 7814
      endif
      
*     special treatment for monthly structure (ie. jpd = 8) --------------------
      if ( jpd .eq. 8 ) then ! monthly structure for river flow ----------------
      call get monthly structure file for river flow ! monthly structure -------
      call set up default random normal deviates
      goto 7814
      endif ! monthly structure

*     --------------------------------------------------------------------------
*     other distributions for river flow ---------------------------------------
*     --------------------------------------------------------------------------
*     IVECT      - label for set of river flow data ----------------------------
*     PDRF       - code for type of distribution -------------------------------
*                       0 - constant
*                       1 - normal
*                       2 - log-normal
*                       3 - three-parameter log-normal
*                       4 - non-parametric distribution
*                       5 - monthly distribution
*                       8 - monthly structure
*                      10 - power curve distribution 99999999 not used 999999999
*     F          - mean and 95-percentile low flow ...
*                - for Feature Types (2),(8), (10), (18) ...
*                - the interpretation varies with the Feature Type
*                - for Feature Type (7) uniform abstraction and Hands-Off Flow
*                - for Feature Type (9) maintained flow
*     XXXXX      - the name of the site (not used within SIMCAT) ...
*     --------------------------------------------------------------------------

      read(02,*,ERR = 8313) IVECT,PDRF(IVECT),(F(IVECT,JM),JM = 1,MO)

*     look for illegal codes for distributions ---------------------------------
 2314 continue
      if ( PDRF (IVECT) .lt. 0 .or. PDRF (IVECT) .gt. 9 ) then
      suppress10 = suppress10 + 1
      write(33,8551)IVECT
 8551 format(77('-')/
     &'*** Illegal data for river flow ... ',
     &'*** Check dataset number',i6/77('-'))
      endif

*     check for negative data --------------------------------------------------
      if ( F(IVECT,1) .lt. -0.00000001 
     &.or. F(IVECT,2) .lt. -0.00000001 ) then
     	call change colour of text (20) ! bright red
      write( *,6387)IVECT
      call set screen text colour
      write(33,6387)IE
 6387 format(77('-')/
     &'*** Negative data for river flow '/
     &'*** Check dataset number',i6/77('-'))
      call stop
      endif
      
*     remove shift for a 2-parameter log-normal distribution -------------------
      if ( PDRF (IVECT) .eq. 2 ) then
      F(IVECT,3) = 0.0
      endif
      
*     look for log-normal distributions ----------------------------------------
      if ( PDRF (IVECT) .eq. 2 .or. PDRF (IVECT) .eq. 3 ) then
      if ( F(IVECT,2) .gt. F(IVECT,1) ) then
      call sort format 2 (F(IVECT,2),F(IVECT,1))
      write(08,8151)valchars10,valchars11,IVECT ! -------------------------- INP
 8151 format(77('-')/
     &'*** A 95-percentile low flow of ',a10,4x,
     &'has a mean of  ',a10/'*** Check set of flow ',
     &'data numbered ',i6/77('-'))
      write(08,8152) ! ----------------------------------------------------- INP
 8152 format(
     &'SIMCAT may change the 95-percentile ... '/77('-'))
      endif
     
      if ( F(IVECT,3) .lt. 1.0e-10 ) then
      F(IVECT,3) = 0.0
      PDRF (IVECT) = 2
      endif

      if ( F(IVECT,1) .lt. 1.0e-10 .and. ! suppress illegal data ---------------
     &F(IVECT,2) .lt. 1.0e-10 ) then
      F(IVECT,1) = 0.0 
      F(IVECT,2) = 0.0 
      PDRF (IVECT) = 0 
      endif ! suppress illegal data --------------------------------------------

      if ( F(IVECT,2) .lt. 1.0e-08 .and. F(IVECT,1) .gt. 1.0e08 ) then
      write(33,8858)IVECT
 8858 format(77('-')/'*** A 95-percentile low flow is entered as zero'/
     &'*** Check set number',i6)
*	write(33,8852)
 8852 format(
     &'SIMCAT may change the 95-percentile ... ',
     &'and explain later ... '/77('-'))
      endif
      endif

*     power curve distribution (river flow) 99999999999 not used 999999999999999
      if ( PDRF (IVECT) .eq. 10 ) then ! power curve distribution 99999999999999
      if ( F(IVECT,2) .gt. F(IVECT,1) ) then ! 9999999999 not used 9999999999999
      call sort format 2 (F(IVECT,2),F(IVECT,1)) ! 99999999 not used 99999999999
      write(33,8651)valchars10,valchars11,IVECT ! 99999999 not used 999999999999
 8651 format(77('-')/'*** A 95-percentile low flow of ',a10,4x, ! 99999999999999
     &'has a mean of  ',a10/'*** Check set of flow ', ! 999999999999999999999999
     &'data numbered ',i6/77('-')) ! 99999999 not used 9999999999999999999999999
*	write(33,8662) ! 99999999 not used 999999999 not used 99999999999999999999
 8662 format( ! 99999999 not used 999999999 not used 9999999 not used 9999999999
     &'SIMCAT may change the 95-percentile ... ', ! 99999999 not used 9999999999
     &'and explain later ... '/77('-')) ! 999999 not used 99999 not used 9999999
      endif ! 999999999999 not used 999999999999999999999 not used 9999999999999
      if ( F(IVECT,2) .lt. 1.0e-08 .and. F(IVECT,1) .gt. 1.0e08 ) then ! 9999999
      write(33,8658)IVECT ! 99999999999 not used 99999999999 not used 9999999999
 8658 format(77('-')/'*** A 95-percentile low flow is entered as zero'/ ! 999999
     &'*** Check set number',i6) ! 99999 not used 99999999 not used 999999999999
*	write(33,8652) ! 9999 not used 999999 not used 999999 not used 99999999999
 8652 format( ! 9999 not used 999999 not used 999999 not used 999999999999999999
     &'SIMCAT may change the 95-percentile ... ', ! 99999 not used 9999999999999
     &'and explain later ... '/77('-')) ! 99999999 not used 9999 not used 999999
      endif ! 999999999999 not used 9999999 not used 999999 not used 99999999999
      endif ! if ( PDRF (IVECT) .eq. 10 ) power curve distribution 9999999999999

*     check the flow data for illegal distributions ----------------------------
      if ( PDRF(IVECT) .eq. 6 .or. PDRF(IVECT) .eq. 7 .or. ! loads -------------
     &     PDRF(IVECT) .eq. 9 .or. ! ! --------------------- loads -------------
     &     PDRF(IVECT) .gt. 9 ) then !------------------------------------------
      suppress10 = suppress10 + 1
      write(01,8661)IVECT
      write(33,8661)IVECT
 8661 format(77('-')/
     &'*** Illegal data for river flow ... '/
     &'*** The data are expressed as distributions of loads ...'/
     &'*** Check dataset number',i6,
     &' ... the flows have been set to zero ...'/77('-'))
      PDRF(IVECT) = 0
      F( IVECT, 1 ) = 0.0
      F( IVECT, 2 ) = 0.0
      F( IVECT, 3 ) = 0.0
      F( IVECT, 4 ) = -9.9
      endif

*     check and correct default correlation ------------------------------------
      if ( F(IVECT,MO) .gt. 1.0 .or. F(IVECT,MO) .lt. -1.0 ) then
      F(IVECT,MO) = -9.9
      endif

      goto 7814
      return

 8313 write( *,8314)IVECT
      write(01,8314)IVECT
      write(09,8314)IVECT
      write(33,8314)IVECT
 8314 format(77('-')/
     &'*** Error in reading river flow data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end

      
      

*     read the data on river quality -------------------------------------------
      subroutine read data on river quality (MV)
      include 'COMMON DATA.FOR'

*     --------------------------------------------------------------------------
*     types of distributions for river quality ---------------------------------
*     --------------------------------------------------------------------------
*                       0 - constant
*                       1 - normal
*                       2 - log-normal
*                       3 - three-parameter log-normal
*                       4 - non-parametric distribution
*                       5 - monthly distribution
*                       6 - normal distribution of loads
*                       7 - log-normal distribution of loads
*                       8 - monthly structure 
*                       9 - non-parametric distribution of loads
*                      10 - power curve distribution for concentrations 99999999
*                      11 - power curve distribution for loads  
*     --------------------------------------------------------------------------

*     reset the counters (used to detect duplicate data-sets) ------------------
      do IV = 1,MT
      MK(IV) = 0
      MKQ(IV) = 0
      enddo
      NMKQ = 0 ! ------------------------------------------------- river quality

      MV = 0 ! intialise the counter of data-sets for river quality ------------
      KE = -9 ! switch for testing for a new data-set for river quality --------
      kt1 = 0 ! marker to prevent duplicate warning messages -------------------
*     set the number of determinands (for checking all records are read in) ----
      MPAR = NDET
      KPAR = MPAR + 1

*     read a data-set for river quality ----------------------------------------
 6814 continue

      NUMV = 0 ! initialise code number of data-set for river quality ----------
      NUMP = 0 ! initialise code number of the determinand ---------------------
      IMDIST8 = 0
      IMDIST5 = 0
      call check first four characters of a line of data (IFIN)

*     test for the last data-set on the SIMCAT datafile ------------------------
      if ( IFIN .eq. 1 ) goto 16
      NMKQ = NMKQ + 1 ! ------------------------------------------ river quality

*     read the code number of data-set for river quality -----------------------
      read(02,*, ERR = 8311 ) NUMV

*     ensure that SIMCAT's array boundaries are not exceeded -------------------
      if ( NUMV .le. 0 ) goto 6814
      if ( NUMV .gt. NV ) then ! -----------------------------------------------
      write(01,1007)NUMV
      write( *,1007)NUMV
      write(09,1007)NUMV
      write(33,1007)NUMV
 1007 format(77('-')/
     &'*** River quality data-set has an illegal label (',I5,')'/
     &'*** It has been ignored ...'/77('-'))
      goto 6814
      endif ! if ( NUMV .gt. NV ) ----------------------------------------------
      
      backspace 02

*     read the code number for the determinand for river quality ---------------
      read(02,*, ERR = 8311 ) NUMVX, NUMP
      
      if ( NUMP .gt. MP10 ) then ! ---------------------------------------------
      write(01,9324)NUMV
      write( *,9324)NUMV
      write(09,9324)NUMV
      write(33,9324)NUMV
      goto 6814 
      endif ! if ( NUMP .gt. MP10 ) --------------------------------------------

      if ( NUMVX .ne. KE ) then ! - a new river quality dataset is being started

      if ( KE .gt. 0 ) then ! check for missing data for the previous one ------
      NUMV = KE
      if ( MK(NUMV) .gt. 0 ) then ! ------------------------------ river quality
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      call change colour of text (20) ! bright red
      write( *,9884)NUMV
 9884 format('*** Missing determinand for river quality ',
     &'data ... set number',i3/'*** Calculations stopped ...')
      call set screen text colour
      write(01,9824)NUMV
      write(09,9824)NUMV
      write(33,9824)NUMV
 9824 format(77('-')/
     &'*** Missing determinand for river quality ... ',
     &'set number',i3/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif ! if ( qtype (idet) .ne. 4 )
      enddo ! do idet = 1, ndet
      endif ! if ( MK(NUMV) .gt. 0 ) ----------------------------- river quality
      endif ! if ( KE .gt. 0 ) check for missing data for determinands ---------
      
      NUMV = NUMVX
      MKQ(NMKQ) = NUMV ! ----------------------------------------- river quality
          
*     new dataset starting. Check data are complete for the last one -----------
      if ( KPAR .le. MPAR ) then
      call change colour of text (20) ! bright red
      write( *,9224)NUMV
      call set screen text colour
      write(01,9224)NUMV
      write(09,9224)NUMV
      write(33,9224)NUMV
 9224 format(77('-')/
     &'*** Missing determinand records for river quality ',
     &'dataset ...'/'*** Calculations stopped ... ',
     &' Check dataset number',i6/77('-')//)
      call stop
      endif

      KPAR = 1

*     check for duplicate dataset code numbers for river quality ---------------
      if ( MK(NUMV) .gt. 0 ) then ! ----- check for duplicates --- river quality
      !if ( kt1 .eq. 0 .and. ical .ne. 1 .and. kerror .eq. 1 ) then
      if ( kt1 .eq. 0 .and. kerror .eq. 1 ) then
      call change colour of text (12) ! orange
      write( *,1098)NUMV
 1098 format('*** River quality data-set listed more ',
     &'than once',3x,'...'7x,'Set number',i6,28x,25('.'))
      call set screen text colour
      write(09,1008)NUMV
      write(33,1008)NUMV
      write(01,1008)NUMV
 1008 format(77('-')/
     &'*** A river quality data-set (',I5,') is specified more ',
     &'than once ...'/ 
     &'*** (Or has data for too many determinands) ...'/ 
     &'*** Only the first is retained ...'/77('-'))
      endif
      kt1 = kt1 + 1
      kountd1 = kountd1 + 1
      KPAR = MPAR
      goto 6814
      else
      kt1 = 0
      endif ! if ( MK(NUMV) .gt. 0 ) ---- check for duplicates --- river quality
      
      if ( KPAR .gt. MPAR .and. NUMP .le. MPAR ) then
      write(01,9324)NUMV
      write( *,9324)NUMV
      write(09,9324)NUMV
      write(33,9324)NUMV
 9324 format(77('-')/
     &'*** Too many determinand records for river quality ',
     &'data-set ...'/
     &'*** The extra records have been ignored ... '/
     &'*** Check the dataset number',i6/77('-'))
      goto 6814
      endif

*     set marker to indicate reading of records within a single dataset --------
      KE = NUMV
*     reset determinand/record counter for this dataset for river quality ------
      KPAR = 1
      endif ! if ( NUMV .ne. KE )

*     check for an illegal determinand code for river quality ------------------
      if ( NUMP .lt. 1 .or. NUMP .gt. MP10 ) then
      write(01,9007)NUMP,NUMV
      write(09,9007)NUMP,NUMV
      write(33,9007)NUMP,NUMV
      write( *,9007)NUMP,NUMV
 9007 format(77('-')/
     &'*** A river quality data-set has an illegal determinand code',
     &' (',I3,')'/'*** Dataset is number ',I3,
     &' .... calculations stopped.'//77('-'))
      call stop
      endif
      backspace 02

*     read the first three lines of data on river quality ----------------------
      read(02,*, ERR = 8311 ) NUMV, NUMP, jpd
      
*     check for an illegal distribution code for river quality -----------------
      if ( jpd .lt. 0 .or. jpd .gt. 11 ) then ! 99999999999999999999999999999999
      write( *,8555)NUMV
      write(01,8555)NUMV
      write(09,8555)NUMV
      write(33,8555)NUMV
 8555 format(77('-')/
     &'*** Illegal code for type of distribution for river ',
     &'quality ...'/ 
     &'*** Check the data-set number',i6/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif
      backspace 02

*     check any requests for non-parametric distributions for river quality ----
      if ( jpd .eq. 4 .or. jpd .eq. 9 ) then
      call get a non parametric file for river quality
      goto 6824
      endif

*     special treatment for monthly data (jpd = 5) for river quality ------------
      if ( jpd .eq. 5 ) then
      call get monthly file for river quality ! type 5
      goto 6824
      endif

*     special treatment for monthly structure (jpd = 8) for river quality ------
      if ( jpd .eq. 8 ) then ! monthly structure for river quality -------------
      call get monthly structure file for river quality ! type 8
      call set up default random normal deviates
      goto 6824
      endif

*     --------------------------------------------------------------------------
*     distributions for river quality ------------------------------------------
*     --------------------------------------------------------------------------
*     NUMV   - label for the set of river quality data -------------------------
*     NUMP   - code number for determinand -------------------------------------
*     PDRC   - code for type of data -------------------------------------------
*                    0 - constant
*                    1 - normal
*                    2 - log-normal
*                    3 - three-parameter log-normal
*                    4 - non-parametric distribution
*                    5 - monthly data
*                    6 - normal distribution of LOADS
*                    7 - log-normal distribution of LOADS
*                    8 - monthly structure 
*                    9 - non-parametric distribution of LOADS
*                   10 - power curve distribution for concentrations 99999999999
*                   11 - power curve distribution for LOADS 
*     X(1)   - mean ------------------------------------------------------------
*     X(2)   - standard deviation ----------------------------------------------
*     X(3)   - shift (for 3-parameter) -----------------------------------------
*     QNUM   - number of samples used for X ------------------------------------
*     XXXXX  - name of sampling point (not used by program) --------------------
*     --------------------------------------------------------------------------

      if ( jpd .ne. 11 .and. jpd .ne. 10 ) then
      read(02,*, ERR = 8311 ) NUMV,NUMP,PDRC(NUMV,NUMP),
     &(X(JM),JM = 1,MO),QNUM(NUMV,NUMP)
      else ! the distibution is a power curve pppppppppppp power curve ppppppppp
      read(02,*, ERR = 8311 ) NUMV,NUMP,PDRC(NUMV,NUMP), ! ppppppppppppppppppppp
     &(X(JM),JM = 1,MO-1),X(MO+1),X(MO+2),X(MO),QNUM(NUMV,NUMP) ! pppppppppppppp
      if ( X(1) .lt. 0.000001 ) then ! ppppppppppppppppppppppppppppppppppppppppp
      X(1) = 0.0 ! pppppppppppppppppp power curve pppppppppppppppppppppppppppppp
      X(2) = 0.0 ! pppppppppppppppppp power curve pppppppppppppppppppppppppppppp
      if ( X(2) .lt. 0.000001 ) then ! zero standard deviation ! ppppppppppppppp
      PDRC(NUMV,NUMP) = 0 ! change to type 0 ppppppppppp power curve ppppppppppp
      X(2) = 0.0 ! pppppppppppppppppp power curve pppppppppppppppppppppppppppppp
      X(3) = 0.0 ! pppppppppppppppppppppppppppppppp power curve pppppppppppppppp
      endif ! pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
      endif ! pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
      endif ! if ( jpd .ne. 11 or 10 ) pppppppppppp power curve pppppppppppppppp

*     check for duplicate data for determinand for river quality ---------------
      if ( quolity data(NUMV,NUMP,1) .gt. -1.0e-8 ) then
      write( *,9424)NUMV
      write(01,9424)NUMV
      write(09,9424)NUMV
      write(33,9424)NUMV
 9424 format(77('-')/
     &'*** Duplicate determinand records for river quality ',
     &'dataset number',i6,' ...'/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif ! if ( quolity data(NUMV,NUMP,1) -----------------------------------

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) X(MO) = -9.9
*     check and correct default correlation ------------------------------------

*     check for implausible data -----------------------------------------------
      if ( Qtype (nump) .ne. 4 .and. PDRC(NUMV,NUMP) .ne. 11 ) then 
      if ( X(2) .lt. 0.00000001 .and. X(1) .gt. 0.000001 ) then
      if ( PDRC(NUMV,NUMP) .ne. 0 ) then
      suppress18 = suppress18 + 1 ! zero variation in river quality
      if ( suppress18 .lt. 10 ) then
      write(33,6317)dname(NUMP),NUMV,PDRC(NUMV,NUMP)
 6317 format(/77('-')/
     &'*** Implausible (zero) standard deviation for river ', ! quality --------
     &'quality for ',a11/'*** Check dataset number',i6,' ... Type',i4)
      endif
      if ( toveride .eq. 1 ) then
      if ( detype (NUMP) .eq. 104 ) then
      x(2) = 0.25 * x(1)
      call sort format1 (X(1))
      if ( suppress18 .lt. 10 ) write(33,6288)valchars10
 6288 format('*** Over-written as 25% of the mean of ',
     &a10/77('-'))
      else
      x(2) = 0.5 * x(1)
      call sort format1 (X(1))
      if ( suppress18 .lt. 10 ) write(33,6688)valchars10
 6688 format('*** Over-written as 50% of the mean of ',
     &a10/77('-'))
      endif
      endif
      if ( suppress18 .eq. 9 ) then
      write(33,6357)
 6357 format('*** And elsewhere ... ')
      write(33,6299)
 6299 format(77('-'))
      endif
      endif
      endif
      endif

*     check for negative data --------------------------------------------------
      if ( Qtype (nump) .ne. 4 ) then
      if ( X(2) .lt. -0.00000001 .or. X(1) .lt. -0.00000001 ) then
      call change colour of text (20) ! bright red
      write( *,6387)dname(NUMP),NUMV
      call set screen text colour
      write(33,6387)dname(NUMP),NUMV
 6387 format(77('-')/
     &'*** Negative concentrations for river quality ',
     &'quality for ',a11/
     &'*** Check dataset number',i6/77('-'))
      call stop
      endif
      endif
      
      if ( PDRC(NUMV,NUMP) .eq. 2 .and. abs (X(3)) .gt. 1.0e-11 ) then
      X(3) = 0.0
      endif
      if ( PDRC(NUMV,NUMP) .eq. 3 .and. abs (X(3)) .lt. 1.0e-11 ) then
      PDRC (NUMV,NUMP) = 2
      endif
      if ( PDRC(NUMV,NUMP) .eq. 2 .and. abs (X(2)) .lt. 1.0e-11 ) then
      PDRC (NUMV,NUMP) = 0
      endif
      if ( PDRC(NUMV,NUMP) .eq. 1 .and. abs (X(2)) .lt. 1.0e-11 ) then
      PDRC (NUMV,NUMP) = 0
      endif

*     load river quality data into SIMCAT's storage ----------------------------
      do JM = 1, MO
      quolity data ( NUMV, NUMP, JM ) = X(JM) ! mean etc -----------------------
      enddo ! ------------------------------------------------------------------
      
      if ( PDRC(NUMV,NUMP) .eq. 11 ) then ! power curve data for loads ppppppppp
      powerbase(NUMV,NUMP) = X(MO+1)
      powermini(NUMV,NUMP) = X(MO+2)
      endif ! ( PDRC(NUMV,NUMP) .eq. 11 ) ! power curve data for loads ppppppppp

      if ( PDRC(NUMV,NUMP) .eq. 10 ) then ! power curve for concentration pppppp
      powerbase(NUMV,NUMP) = X(MO+1)
      powermini(NUMV,NUMP) = X(MO+2) 
      endif ! ( PDRC(NUMV,NUMP) .eq. 10 ) ! power curve for concentration pppppp

 6824 continue

*     flag the entry of this code number (so as to detect duplicates) ----------
      if ( NUMP .eq. 1) MK(NUMV) = MK(NUMV) + 1 ! ----- detect duplicates ------
*     truncate number of samples to 8 (for confidence limits) ------------------
*     also an indicator that valid data have been read -------------------------
      if ( QNUM (NUMV,NUMP) .lt. 8 ) then ! --- over-write low number of samples
      write(33,3271)NUMV,dname(NUMP)
 3271 format(/77('-')/
     &'Low number of samples over-written as 8 for river quality ',
     &'data-set:',i10/
     &'Determinand: ',a11/77('-'))    
      endif
      if ( QNUM (NUMV,NUMP) .gt. 500 ) then
      write(33,3371)NUMV,dname(NUMP)
 3371 format(/77('-')/
     &'High number of samples over-written as 24 for river quality ',
     &'data-set:',i8/   
     &'Determinand: ',a11/77('-'))    
      endif
      
      QNUM (NUMV,NUMP) = MAX0 (8, QNUM (NUMV,NUMP) )
*     overwrite high numbers (for confidence limits) ---------------------------
      if ( QNUM (NUMV,NUMP) .gt. 500 ) then
      QNUM (NUMV,NUMP) = 24 ! ---------------- over-write high number of samples
      endif

*     count (add 1 to) the total number of river quality data-sets -------------
      if ( NUMP .eq. 1) MV = MV + 1

*     get ready for the next record/determinand for river quality --------------
      KPAR = KPAR + 1
      if ( KPAR .gt. MPAR ) KE = -9
      goto 6814

*     all the river quality datasets have been entered -------------------------
*     set the final total of river quality datasets ----------------------------
   16 if ( MV .ne. 1) then ! ---------------------------------------------------
      MV = MV - 1

      if ( KPAR .le. MPAR ) then ! =============================================
      call change colour of text (20) ! bright red
      write( *,9624)KE
      call set screen text colour
      write(01,9624)KE
      write(09,9624)KE
      write(33,9624)KE
 9624 format(77('-')/
     &'*** Missing determinand records for river quality ',
     &'dataset ...'/'*** Calculations stopped ... ',
     &' Check dataset number',i6/77('-'))
      call stop
      endif ! if ( KPAR .lt. MPAR ) ============================================
      endif ! if ( MV .ne. 1) --------------------------------------------------
   
*     over-write data for excluded determinands --------------------------------
      do idet = 1, MP10
      if ( Qtype (idet) .eq. 4 ) then
      do iv = 1, NV
      do im = 1, mo + 2
      quolity data (iv,idet,im) = 0.0
      enddo
      QNUM (iv,idet) = 0
      enddo
      endif
      enddo

*     check for duplicate dataset code numbers for river quality ---------------
      if ( kountd1 .gt. 5 ) then
      if ( kerror .eq. 1 ) then
      call change colour of text (12) ! orange
      write( *,1998)kountd1
 1998 format(
     &'*** Data-sets on river quality have the same code  ...',
     &7x,'These total',i6,' sets',22x,25('.'))
      call set screen text colour
      write(09,1908)kountd1
      write(33,1908)kountd1
      write(01,1908)kountd1
 1908 format(77('-')/
     &'*** There are',i6,' river quality data-sets with the ',
     &'same code!'/77('-'))
      endif ! if ( kerror .eq. 1 ) ---------------------------------------------
      endif ! if ( kountd1 .gt. 5 ) ===========================================
*     check for duplicate dataset code numbers for river quality ---------------
      if ( kountd2 .gt. 5 ) then ! =============================================
      if ( kerror .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,6998)kountd2
 6998 format(
     &'*',I6,' effluent data-sets have the same code!')
      call set screen text colour
      write(09,6908)kountd2
      write(33,6908)kountd2
      write(01,6908)kountd2
 6908 format(77('-')/
     &'*** There are',I6,' effluent data-sets with the ',
     &'same code!'/77('-'))
      endif ! if ( kerror .eq. 1 ) ---------------------------------------------
      endif ! if ( kountd2 .gt. 5 ) ============================================

      return

 8311 call change colour of text (14) ! bright yellow
      write( *,8312)NUMV,NUMP,jpd
      write(01,8312)NUMV,NUMP,jpd
      write(09,8312)NUMV,NUMP,jpd
      write(33,8312)NUMV,NUMP,jpd
 8312 format(77('-')/
     &'*** Error in reading the river quality data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Dataset number (and the next two items):',i6,'  (',2i6,'   )'
     &/77('-'))
      backspace(02)
      read(02,8313)line of data
 8313 format(a80)
      call change colour of text (10) ! green
      write( *,8314)line of data
      call set screen text colour
      write(01,8314)line of data
      write(09,8314)line of data
      write(33,8314)line of data
 8314 format('The text of the line that produced this message is:'/
     &77('-')/a80/77('-'))
      call stop
      end





*     read the data on discharge flow and quality ------------------------------
      subroutine read effluent data

*     --------------------------------------------------------------------------
*     distributions of effluent flow and quality
*     --------------------------------------------------------------------------
*                       0 - constant
*                       1 - normal
*                       2 - log-normal
*                       3 - three-parameter log-normal
*                       4 - non-parametric distribution
*                       5 - monthly distributions
*                       6 - normal distribution of loads
*                       7 - log-normal distribution of loads
*                       8 - monthly structure 
*                      10 - power curve distribution 999999999999999999999999999
*                      11 - power curve distribution of loads 999999999999999999
*     --------------------------------------------------------------------------
      
      include 'COMMON DATA.FOR'

*     reset the counters -------------------------------------------------------
      kounter = 0
      do IV = 1, MT
      MK(IV) = 0
      MKE(IV) = 0
      enddo
      NMKE = 0 ! ----------------------------------------------------- discharge

*     the switch for testing for a new dataset for discharge flow and quality --
      KE = -9
      kt1 = 0
*     for discharges, flow is a determinand ... set the counter ----------------
      MPAR = NDET + 1
      KPAR = MPAR
      
   25 call check first four characters of a line of data (IFIN)
      
*     test for the last effluent flow/quality dataset on the SIMCAT datafile ---
      if ( IFIN .eq. 1 ) goto 26

*     read the code number of dataset for discharge flow and quality -----------
      read(02,*, ERR = 8316 ) IE ! code number of dataset for discharge data ---

*     ensure that the array boundaries are not exceeded ------------------------
      if ( IE .le.  0 ) goto 25
      if ( IE .gt. NE ) then
      write(01,1013)IE
      write( *,1013)IE
      write(09,1013)IE
      write(33,1013)IE
 1013 format(77('-')/
     &'*** Effluent data set has illegal label (',I5,')'/
     &'*** The set has been ignored by SIMCAT ...'/77('-'))
      goto 25
      endif
      backspace 02

      read(02,*, ERR = 8316 ) IEX,NUMP
      
      if ( nump .gt. MP10 ) then
      write(01,9324)IE
      write( *,9324)IE
      write(09,9324)IE
      write(33,9324)IE
 9324 format(77('-')/
     &'*** Too many determinand records for an effluent ',
     &'data-set ...'/
     &'*** The extra records have been ignored ... '/
     &'*** Check the dataset number',i6/77('-'))
      goto 25
      endif
    
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     check for new dataset for discharge flow and quality +++++++++++++++++++++
      if ( IEX .ne. KE ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++

      if ( KE .gt. 0 ) then ! check for missing data for determinands ----------
      IE = KE
      if ( MK(IE) .gt. 0 ) then ! check for missing data for determinands ------
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
     	call change colour of text (20) ! bright red
      write( *,9884)IE
 9884 format('*** Missing determinand for effluent quality ',
     &'data ... set number',i3/'* Calculations stopped ...')
      call set screen text colour
      write(01,9824)IE
      write(09,9824)IE
      write(33,9824)IE
 9824 format(77('-')/
     &'*** Missing determinand for river quality ',
     &'data number',i3/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif ! if ( qtype (idet) .ne. 4 )
      enddo ! do idet = 1, ndet
      endif ! if ( MK(IE) .gt. 0 ) check for missing data for determinands -----
      endif ! if ( KE .gt. 0 ) check for missing data for determinands ----===--

*     new dataset starting ... check data are complete for the last one --------
      if ( KPAR .lt. MPAR ) then
      write(01,9214)IE,NUMP,KE
      write( *,9214)IE,NUMP,KE
      write(09,9214)IE,NUMP,KE
      write(33,9214)IE,NUMP,KE
 9214 format(77('-')/'*** Missing discharge data ...'/
     &'*** Check dataset number:',i6,' ... Determinand:',2i4/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif
      KPAR = 1

*     check for duplicate dataset code numbers (effluent flow/quality) =========
      if ( MK(IE) .gt. 0 ) then ! check for duplicate data for discharges ======
      if ( kt1 .eq. 101 ) then ! -----------------------------------------------
      call change colour of text (12) ! orange
      write( *,1898)IE
 1898 format('*** An effluent data-set (',I5,') is specified more ',
     &'than 100 times!') 
      call set screen text colour
      endif ! if ( kt1 .eq. 101 ) ----------------------------------------------
      if ( kt1 .eq. 0 ) then ! ---------------------- check for duplicate labels
      call change colour of text (12) ! orange
      write( *,1814)IE
 1814 format(
     &'*** Duplicate label for effluent data-set',10x,'...',
     &7x,'Set number'i6)
      call set screen text colour

      if ( ical13 .eq. 0 ) write(01,1014)IE
      write(09,1014)IE
      write(33,1014)IE
 1014 format(/77('-')/
     &'*** Duplicate label for effluent data-set number:',I6,' ...'/
     &'*** (Or data have been added for too many determinands) ...'/
     &'*** Only the first is retained ...'/77('-'))   
      endif ! if ( kt1 .eq. 0 ) --------------------- check for duplicate labels
      kt1 = kt1 + 1
      kountd2 = kountd2 + 1
      KPAR = MPAR
      goto 25
      else
      kt1 = 0
      endif ! if ( MK(IE) .gt. 0 ) check for duplicate data -- discharges ======

      if ( KPAR .gt. MPAR .and. NUMP .le. MPAR ) then
      write(01,9524)
      write( *,9524)
      write(09,9524)
      write(33,9524)
 9524 format(77('-')/
     &'*** DUPLICATE determinand records for discharge ',
     &'dataset number',i3,' ...'/'*** Calculations stopped ...'/
     &77('-'))
      call stop
      endif

*     set marker to indicate reading of records within a single dataset --------
      KE = IE

*     reset determinand/record counter for this dataset ------------------------
      KPAR = 1
      endif ! if ( IEX .ne. KE ) +++++++++++++++++++++++++++++++++++++++++++++++
*     check for new dataset for discharge flow and quality +++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      

      
*     check for an illegal determinand code for discharge flow and quality -----
      if ( NUMP .lt. 0 .or. NUMP .gt. NDET ) then
      write( *,9117)IE,nump
      write(01,9117)IE,nump
      write(09,9117)IE,nump
      write(33,9117)IE,nump
 9117 format(77('-')/
     &'*** Discharge data-set has an illegal determinand ',
     &'code (',I3,')'/
     &'*** Set ',i3,' has been ignored ... '/77('-'))
      !call stop
      endif
      backspace 02

*     discharge flow -----------------------------------------------------------
      if ( KPAR .eq. 1 ) then
      NMKE = NMKE + 1 ! ---------------------------------------------- discharge
      read(02,*, ERR = 4929 ) IE, NUMP, jpd

*     check that the determinand code is zero (this is required for flow) ------
      if ( NUMP .ne. 0 ) then
      write( *,8156) IE
      write(01,8156) IE
      write(09,8156) IE
      write(33,8156) IE
 8156 format(77('-')/
     &'*** Non-zero determinand code for discharge flow ...'/
     &'*** Re-set to zero. Check dataset number',i6/
     &'*** Look at the first record in the dataset ...'/
     &'*** and the second number in that record ...'/77('-'))
      NUMP = 0
      endif

*     check for illegal distribution code for discharge flow -------------------
      if ( jpd .lt. 0 .or. jpd .gt. 11 ) then ! 99999999999999999999999999999999
      write( *,8856) IE
      write(01,8856) IE
      write(09,8856) IE
      write(33,8856) IE
 8856 format(77('-')/
     &'*** Illegal code for type of distribution for discharge ',
     &'flow ...'/'*** Check dataset number',i6, 
     &' ... calculations stopped ...'/77('-'))
      call stop
      endif

*     check the flow data for illegal distributions ----------------------------
      if ( jpd .eq. 6 .or. jpd .eq. 7 .or. jpd .eq. 9 
     &                .or. jpd .eq. 11 ) then ! loads 99999999999999999999999999
      kounter = kounter + 1
      write(33,8661)IE
      if ( kounter .eq. 1 ) then
      write(01,8661)IE
      write( *,8611)IE
 8661 format(77('-')/
     &'*** Illegal data for discharge flow ... '/
     &'*** The data are expressed as distributions of LOADS ...'/
     &'*** Check dataset number',i6,
     &' ... distribution changed ...'/77('-'))
      endif
      if ( kounter .eq. 2 ) then
      write(01,8611)IE
      write(31,8611)IE
 8611 format(77('-')/
     &'*** Illegal data for discharge flow ... '/
     &'*** The data are expressed as distributions of LOADS ...'/
     &'*** Check dataset number',i6,
     &' ... distribution changed ...'/
     &'*** Similar messages have been suppressed ... ',
     &'Check the ERR file for details'/77('-'))
      endif
      if ( jpd .eq. 6 ) jpd = 1
      if ( jpd .eq. 7 ) jpd = 2
      if ( jpd .eq. 11 ) jpd = 2
*     call stop
      endif
     
*     add 1 to the discharge counter for discharge flow ------------------------
      if ( NUMP .eq. 0 ) then
      MK(IE) = MK(IE) + 1 ! ------------------------------------- discharge data
      MKE(NMKE) = IE ! ------------------------------------------ discharge data
      endif
      backspace 02

*     check any requests for non-parametric distributions for discharge flow ---
      if ( jpd .eq. 4 ) then
      call get non parametric file for discharge flow
      KPAR = KPAR + 1 
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     special treatment for monthly data (ie. jpd = 5) -------------------------
      if ( jpd .eq. 5 ) then
      call get monthly file for discharge flow ! type 5
      KPAR = KPAR + 1
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     special treatment for monthly structure (ie. jpd = 8) --------------------
      if ( jpd .eq. 8) then ! monthly structure for discharg flow --------------
      call get monthly structure file for discharge flow ! type 8 --------------
      call set up default random normal deviates
      KPAR = KPAR + 1
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     --------------------------------------------------------------------------
*     read effluent flow data ...  other distributions
*     --------------------------------------------------------------------------
*     PDEF       - code for type of probability distribution for effluent flow
*                       0 - constant
*                       1 - normal
*                       2 - log-normal
*                       3 - three-parameter log-normal
*                       4 - non-parametric distribution
*                       5 - monthly distributions
*                       6 - normal distribution of loads
*                       7 - log-normal distribution of loads
*                       8 - monthly structure 
*                       9 - non-parametric distribution of loads
*                      10 - power curve distribution 999999999999999999999999999
*                      11 - power curve distribution of loads 999999999999999999
*     --------------------------------------------------------------------------

      read(02,*, ERR = 8326 ) IE,NUMP,PDEF(IE),
     &(FE(IE,JM),JM = 1,MO),JNUM

*     check and correct default correlation ------------------------------------
      if ( FE(IE,MO) .gt. 1.0 ) then
     	!call change colour of text (20) ! bright red
      !write( *,1386)IE
      !call set screen text colour
      write(08,1386)IE
      write(33,1386)IE
 1386 format(77('-')/
     &'*** Incorrect correlation coeffiecient ... '/
     &'*** Value reset to the default ...'/
     &'*** Check dataset number:',i6/77('-'))
      endif
      
      if ( FE(IE,MO) .gt. 1.0 .or. FE(IE,MO) .lt. -1.0 ) then
      FE(IE,MO) = -9.9
      endif

*     check for negative data --------------------------------------------------
      if ( FE(IE,1) .lt. -0.00000001 
     &.or. FE(IE,2) .lt. -0.00000001 ) then
     	call change colour of text (20) ! bright red
      write( *,6386)IE
      call set screen text colour
      write(33,6386)IE
 6386 format(77('-')/
     &'*** Negative data for discharge flow ... '/
     &'*** Check effluent dataset number:',i6/77('-'))
      call stop
      endif

      ENUM(IE) = JNUM
      if ( ENUM(IE) .le. 1 ) ENUM(IE) = 365

*     get ready for next record/determinand ------------------------------------
      KPAR = KPAR+1
      if ( KPAR .gt. MPAR ) KE = -9

*     ==========================================================================
*     note whether the data are for loads --------------------------------------
      if ( PDEF(IE) .eq. 6 ) then ! normal distribution of loads ---------------
      if ( nobigout .le. 0 ) write(01,3965)IE
      write(33,3965)IE
      write( *,3965)IE
 3965 format(77('-')/
     &'Illegal type 6 distribution for effluent flow ... ',
     &'Record Number 'i5/
     &'Changed this to a normal distribution ...')
      PDEF(IE) = 1 ! revert to normal (distribution of loads) ------------------
      endif
      if ( PDEF(IE) .eq. 7 .or. PDEF(IE) .eq. 11 ) then !  distribution of loads
      if ( nobigout .le. 0 ) write(01,4965)IE
      write(33,4965)IE
 4965 format(77('-')/
     &'Illegal type 7 distribution for effluent flow ...'/
     &'Record Number 'i5/
     &'Changed this to a log-normal distribution ....')
      PDEF(IE) = 2 ! revert to log-normal distribution of loads ---------------- 
      endif ! if ( PDEF(IE) .eq. 7 .or. 11) lognormal distribution of loads ----
*     ==========================================================================
      
      
*     check cases with zero shift ==============================================
      if ( PDEF(IE) .eq. 2 .and. abs (FE(IE,3)) .gt. 1.0e-11 ) then
      FE(IE,3) = 0.0
      endif
*     check cases with zero mean ===============================================
      if ( PDEF(IE) .eq. 3 .and. abs (FE(IE,1)) .lt. 1.0e-11 ) then
      FE(IE,2) = 0.0
      FE(IE,3) = 0.0
      PDEF(IE) = 0 ! revert to a constant --------------------------------------
      endif
*     remove unrequired shift ==================================================      
      if ( PDEF(IE) .eq. 3 .and. abs (FE(IE,3)) .lt. 1.0e-11 ) then
      PDEF (IE) = 2 ! revert from 3 to 2-parameter log-normal ------------------
      endif
*     check cases with zero standard deviation =================================
      if ( PDEF(IE) .eq. 2 .and. abs (FE(IE,2)) .lt. 1.0e-11 ) then
      PDEF(IE) = 0 ! zero standard deviation - set as a constant ---------------
      endif
      if ( PDEF(IE) .eq. 1 .and. abs (FE(IE,2)) .lt. 1.0e-11 ) then
      PDEF(IE) = 0 ! zero standard deviation - set as a constant ---------------
      endif

*     go back to get the next line of data (discharge flow and quality) --------
      goto 25
      endif

*     discharge quality
      if ( KPAR .gt. 1 ) then
      read(02,*, ERR = 4729 ) IE, NUMP, jpd

*     check for an illegal distribution code for discharge quality -------------
      if ( jpd .lt. 0 .or. jpd .gt. 9 ) then ! ---------------------------------
      write( *,5388) jpd,IE,NUMP
      write(01,5388) jpd,IE,NUMP
      write(09,5388) jpd,IE,NUMP
      write(33,5388) jpd,IE,NUMP
 5388 format(77('-')/
     &'*** Illegal distribution for discharge dataset ... ',
     &'type number:',i3/
     &'*** Check dataset number',i6,'  Determinand number',i2/
     &'*** Calculations stopped ...'/77('-'))
      call stop
      endif ! if ( jpd .lt. 0 .or. jpd .gt. 9 ) --------------------------------
      backspace 02

*     check any requests for non-parametric distributions (discharge quality) --
      if ( jpd .eq. 4 .or. jpd .eq. 10 ) then ! 99999999999999999999999999999999
      call get non parametric file for discharge quality
      KPAR = KPAR + 1 
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     check any requests for monthly data for discharge quality ----------------
      if ( jpd .eq. 5 ) then ! monthly data for discharge quality --------------
      call get monthly file for discharge quality ! type 5
      KPAR = KPAR + 1 
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     check any requests for monthly structure for discharge quality -----------
      if ( jpd .eq. 8 ) then ! monthly structure for discharge quality ---------
      call get monthly structure file for discharge quality ! type 8
      call set up default random normal deviates
      KPAR = KPAR + 1 
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     check any requests for non-parametric distributions (discharge quality) --
      if ( jpd .eq. 9 ) then
      call get non parametric file for discharge load
      KPAR = KPAR + 1 
      if ( KPAR .gt. MPAR ) KE = -9
      goto 25
      endif

*     other distributions for discharge quality --------------------------------
      read(02,*, ERR = 8336 )IE,NUMP,PDEC(IE,NUMP),
     &(X(JM),JM = 1,MO),JNUM
      
*     check for duplicate data for determinand ---------------------------------
      if ( pollution data(IE,NUMP,1) .gt. -1.0e-08 ) then
      write( *,9624)IE,dname(NUMP),pollution data(IE,NUMP,1)
      write(01,9624)IE,dname(NUMP),pollution data(IE,NUMP,1)
      write(09,9624)IE,dname(NUMP),pollution data(IE,NUMP,1)
      write(33,9624)IE,dname(NUMP),pollution data(IE,NUMP,1)
 9624 format(100('-')/
     &'*** Duplicate determinand records for the discharge ',
     &'dataset number',i3,' ...'/
     &'*** Determinand: ',a11,' Mean =',f10.2/
     &'*** Calculations stopped ...'/100('-'))
      call stop
      endif

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

*     check for implausible data -----------------------------------------------
      if ( Qtype (nump) .ne. 4 ) then
      if ( X(2) .lt. 0.00000001 .and. X(1) .gt. 0.000001 ) then
      if ( PDEC(IE,NUMP) .ne. 0 ) then
      suppress00 = suppress00 + 1 ! zero standard deviation for discharge flow
      write(33,6317)dname(NUMP),IE
 6317 format(/77('-')/
     &'*** Implausible (zero) standard deviation for discharge ', ! quality ----
     &'quality for ',a11/
     &'*** Check dataset number',i6/77('-'))
      if ( toveride .eq. 1 ) then
      x(2) = 0.5 * x(1)
      call sort format1 (X(1))
      write(33,6288)valchars10
 6288 format('*** Over-written as 50% of the mean of ',a10/77('-'))
      endif
      endif
      endif
      endif

*     check for negative data --------------------------------------------------
      if ( Qtype (nump) .ne. 4 ) then
      if ( X(2) .lt. -0.00000001 .or. X(1) .lt. -0.00000001 ) then
      call change colour of text (20) ! bright red
      write( *,6387)dname(NUMP),IE
      call set screen text colour
      write(33,6387)dname(NUMP),IE
 6387 format(77('-')/
     &'*** Negative concentrations for discharge ',
     &'quality for ',a11/
     &'*** Check dataset number',i6/77('-'))
      call stop
      endif
      endif

      if ( PDEC(IE,NUMP) .eq. 2 .and. abs (X(3)) .gt. 1.0e-11 ) then
      X(3) = 0.0
      endif
      if ( PDEC(IE,NUMP) .eq. 3 .and. abs (X(3)) .lt. 1.0e-11 ) then
      PDEC(IE,NUMP) = 2
      endif
      if ( PDEC(IE,NUMP) .eq. 2 .and. abs (X(2)) .lt. 1.0e-11 ) then
      PDEC(IE,NUMP) = 0
      endif
      if ( PDEC(IE,NUMP) .eq. 1 .and. abs (X(2)) .lt. 1.0e-11 ) then
      PDEC(IE,NUMP) = 0
      endif

*     place the discharge quality data into SIMCAT's storage -------------------
      do JM = 1,MO
      pollution data (IE,NUMP,JM) = X(JM)
      enddo
      PNUM (IE,NUMP) = JNUM

*     truncate number of samples to 4 (for confidence limits) ------------------
      PNUM(IE,NUMP) = MAX0( 4,PNUM(IE,NUMP))

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( PNUM(IE,NUMP) .gt. 500 ) then
      PNUM (IE,NUMP) = 12
      endif

*     get ready for next record/determinand ------------------------------------
      KPAR = KPAR + 1
      if ( KPAR .gt. MPAR ) KE = -9

*     go back to get the next line of data -------------------------------------
      goto 25
      endif

*     all the effluent flow/quality data-sets have now been read ---------------
   26 continue

*     over-write data for excluded determinands --------------------------------
      do 6515 IE = 1, NE
      do 6516 idet = 1, MP10
      if ( QTYPE (idet) .ne. 4 ) goto 6516
      do im = 1, mo
      pollution data (IE,idet,im) = 0.0
      enddo
      PNUM(IE,idet) = 0
 6516 continue
 6515 continue
      return

 4929 write( *,4932)IE
      write(01,4932)IE
      write(09,4932)IE
      write(33,4932)IE
 4932 format(77('-')/
     &'*** Error in data for discharge flow distributions ... '/
     &'*** Calculations halted ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop

 4729 write( *,4732)IE
      write(01,4732)IE
      write(09,4732)IE
      write(33,4732)IE
 4732 format(77('-')/
     &'*** Error in data for effluent quality distributions ...'/
     &'*** Calculations halted ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop

 8316 write( *,8317)IE
      write(01,8317)IE
      write(09,8317)IE
      write(33,8317)IE
 8317 format(77('-')/
     &'*** Error in effluent flow/quality data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop

 1566 write( *,1168)flnamesmall(4,nonpd)
      write(01,1168)flnamesmall(4,nonpd)
      write(09,1168)flnamesmall(4,nonpd)
      write(33,1168)flnamesmall(4,nonpd)
 1168 format(77('-')/
     &'*** Error in non-parametric discharge quality data ... '/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8326 write( *,8327)IE
      write(01,8327)IE
      write(09,8327)IE
      write(33,8327)IE
 8327 format(77('-')/
     &'*** Error in reading non-parametric discharge flow data (a)'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop

 8336 write( *,8337)IE
      write(01,8337)IE
      write(09,8337)IE
      write(33,8337)IE
 8337 format(77('-')/
     &'*** Error in reading discharge quality data ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i5/77('-')/)
      call stop
      end



*     read the data on the river quality targets -------------------------------
      subroutine read river targets
      include 'COMMON DATA.FOR'
      
      notarg = 0 ! this will count the number of sets of river targets ---------
      background class = 0 ! the river class set as a default standard ---------
      do kdet = 1, ndet
      class limmits2 (1,kdet) = 1.0e10 ! background class limits ---------------
      do ic = 2, nclass
      class limmits2 (ic,kdet) = 0.0 ! background class limits ---------------
      enddo
      enddo

*     read a set of river targets ----------------------------------------------
    1 continue
      call check first four characters of a line of data (IFIN)
      if ( IFIN .ne. 1 ) then
      read(02,*, ERR = 1 )IQ

*     ensure that the array boundaries are not exceeded ------------------------
      if ( iabs(IQ) .gt. NQ ) then  
      write( *,8296)
      write(01,8296)
      write(09,8296)
      write(33,8296)
 8296 format(/77('-')/
     &'*** Illegal reference number for set of data on river '/
     &'quality targets ...'/77('-'))
      call stop
      endif

      backspace 02

*     read the data on targets -------------------------------------------------
      read(02,*, ERR = 1 ) IQ, (RQS(iabs(IQ),iip),iip = 1,NP)

      iclarse = 0

      notarg = notarg + 1 ! a set of targets has been found --------------------
      if ( IQ .le. 0 ) then ! targets are background class limits -------------- 
      IQ = -IQ
      iclarse = IQ
      background class = 1
      endif

*     erase targets for determinands not being used ----------------------------
      do iip = 1, NP ! =========================================================
      if ( QTYPE (iip) .eq. 4 ) then
      RQS (IQ,iip) = 0.0
      else
      if ( iclarse .gt. 0 ) then ! targets are part of classification ----------
      if ( iclarse .gt. NC ) then ! check for illegal class number -------------
          
      else
      class limmits2 (iclarse, iip) = RQS (IQ,iip)
      endif ! if ( iclarse .gt. NC )
      endif ! if ( iclarse .gt. 0 )
      endif ! if ( QTYPE (iip) .eq. 4 ) 
      
      enddo ! do iip = 1, NP ===================================================
      goto 1 ! go back and read the next set of targets ------------------------

*     end of targets reached ---------------------------------------------------
      endif
      
      if ( notarg .eq. 0 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (20) ! bright red
      write( *,9384)
 9384 format('*** No data were entered for river targets ... '/)
      call set screen text colour
      endif
      if ( nobigout .le. 0 ) write(01,9334)
      write(09,9334)
      write(33,9334)
 9334 format(77('-')/
     &'No data were entered for river targets ... '/77('-'))
      notarg = 1
      endif
      return

 8321 write( *,9394)IQ
      write(01,9394)IQ
      write(09,9394)IQ
      write(33,9394)IQ
 9394 format(/77('-')/
     &'*** Error in reading data on river quality targets ... '/
     &'*** The data-file has been assembled incorrectly ...   '/
     &'*** Check dataset number',i6/77('-'))
      call stop

      end




*     read non-parametric data on river flow -----------------------------------
      subroutine read the non parametric file on river flow ! ----------- type 4
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of non-parametric data-sets -------------------------
      NONPD = NONPD + 1

*     check there is no violation of SIMCAT's storage --------------------------
      if ( NONPD .gt. M7 ) then
      write( *,9862)M7
      write(01,9862)M7
      write(09,9862)M7
      write(33,9862)M7
 9862 format(77('-')/
     &'*** Too many data-sets are non-parametric ...'/
     &'*** Maximum (M7) is',I6/
     &'*** Maximum exceeded whilst reading river flow data ...'/
     &'*** Increase value of parameter - M7 in COM.FOR ...'/77('-'))
      endif

      read(02,*,ERR = 8313) IVECT, PDRF(IVECT), Flname(1,NONPD),
     &F(IVECT,MO),uname(ivect)

*     store the dataset number -------------------------------------------------
      IDENP (1,NONPD,1) = IVECT

*     check and correct default correlation ------------------------------------
      if ( F(IVECT,MO) .gt. 1.0 .or. F(IVECT,MO) .lt. -1.0 ) then
      F(IVECT,MO) = -9.9
      endif

*     add directory to file name -----------------------------------------------
      call add folder for non parametric data file (1)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = Flname(1,nonpd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(Flname(1,nonpd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(Flname(1,nonpd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = Flname(1,nonpd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,8563) flnamesmall(1,nonpd)
      call set screen text colour
 8563 format('*** Error in flow data: ',
     &'non-parametric file does not exist ... ',a64)
      write(01,7563) flnamesmall(1,nonpd)
      write(09,7563) flnamesmall(1,nonpd)
      write(33,7563) flnamesmall(1,nonpd)
 7563 format(77('-')/
     &'*** Error in river flow data ...'/
     &'*** Non-parametric distribution ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      
      
      

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(1,nonpd), STATUS='OLD') ! non-par river flow

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7566) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(1,nonpd) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(1,nonpd)
      write(01,7166) flnamesmall(1,nonpd)
      write(09,7166) flnamesmall(1,nonpd)
      write(33,7166) flnamesmall(1,nonpd)
 7166 format(77('-')/
     &'*** Error in river flow data ....                      '/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(1,nonpd)
      write(01,7167) flnamesmall(1,nonpd)
      write(09,7167) flnamesmall(1,nonpd)
      write(33,7167) flnamesmall(1,nonpd)
 7167 format(77('-')/
     &'*** Error in river flow data ....                      '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif
*     --------------------------------------------------------------------------

      backspace (12)
      read(12, *, ERR=7566) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(2,nonpd) = 1
      endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(1,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
      endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
      enddo
      if ( imark .gt. 1 ) then
      if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd(imark) + rfnpfd(imark - 1))
      else
      cut off zero flow = rfnpfd(nprf)
      endif
      endif
*     write(01,2376)cut off zero flow,flnamesmall(1,nonpd)
 2376 format('Cut off percentile for river flow =',f12.6,1x,a64)

*     compute mean and 5-percentile --------------------------------------------
      rfm = 0.0
      rf5 = 0.0
      do i = 1, nprf
      rfm = rfm + rfnpvl (i)
      enddo
      rfm = rfm / float (nprf)

*     set mean flow (as indicator that valid flow data have been read) ---------
      F(ivect,1) = rfm
*     set 95-percentile flow ---------------------------------------------------
      call get non parametric shot ( -1.644854, rf5 )
      F(ivect,2) = rf5
      
      MKF(IVECT) = IVECT
      return

 7566 write( *,7168)flnamesmall(1,nonpd)
      write(01,7168)flnamesmall(1,nonpd)
      write(09,7168)flnamesmall(1,nonpd)
      write(33,7168)flnamesmall(1,nonpd)
 7168 format(77('-')
     &'*** Error in river flow data ....                      '/
     &'*** Error in reading non-parametric data ...           '/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)IVECT
      write(01,8314)IVECT
      write(09,8314)IVECT
      write(33,8314)IVECT
 8314 format(77('-')/
     &'*** Error in reading river flow data-sets....          '/
     &'*** The data-file has been assembled incorrectly.....     '/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end




*     read the monthly data on river flow ------------------------------- type 5
      subroutine get monthly file for river flow ! ---------------------- type 5
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets --------------------------------
      SEASD = SEASD + 1

*     check there is no violation of SIMCAT's storage --------------------------
      if ( SEASD .gt. M8 ) then
      write( *,9262)M8
      write(01,9262)M8
      write(09,9262)M8
      write(33,9262)M8
 9262 format(77('-')/
     &'*** Too many data-sets are monthly ...'/
     &'*** Maximum (M8) is',I6/
     &'*** Maximum exceeded whilst reading river flow data ... '/
     &77('-'))
      endif

*     read the line of giving the monthly data ---------------------------------
      read(02,*,ERR = 8313 ,END = 8313) IVECT, PDRF(IVECT), 
     &flmonth(1,seasD),F(IVECT,MO),uname(ivect)

*     store the dataset number -------------------------------------------------
      ISEASP (1,seasD,1) = IVECT

*     check and correct default correlation ------------------------------------
      if ( F(IVECT,MO) .gt. 1.0 .or. F(IVECT,MO) .lt. -1.0 ) then
      F(IVECT,MO) = -9.9
      endif

*     add the folder name to the file name -------------------------------------
      call add folder for monthly data file (1)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = flmonth(1,SEASD), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(flmonth(1,SEASD),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(flmonth(1,SEASD),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = flmonth(1,SEASD), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,7163) FLMONTHsmall(1,SEASD)
      call set screen text colour
      write(01,7163) FLMONTHsmall(1,SEASD)
      write(09,7163) FLMONTHsmall(1,SEASD)
      write(33,7163) FLMONTHsmall(1,SEASD)
 7163 format(77('-')/
     &'*** Error in monthly river flow data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      
      
*     open the file of monthly data --------------------------------------------
      open(11,FILE = flmonth(1,seasd), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (1,seasd) ! river flow - type 5

      xtemp1 = 0.0
      xtemp2 = 0.0

*     loop on the number of months ---------------------------------------------
      do 32 i = 1, 12

*     monthly mean and 95-percentile low flow ----------------------------------
      xm = seas1 (i)
      r5 = seas2 (i)
      xs = 0.0

      if ( xm .gt. 1.0e-10 ) then
      if ( r5 / xm .lt. 0.9999 ) then

*     shift flow ---------------------------------------------------------------
      r3 = seas3 (i)

*     compute the mean and standard deviation for the logged variables ---------
      GS = 0.0
      GM = 0.0
      if ( R5 + R3 .gt. 1.0e-9 ) then
      GS=SQRoot(1058,2.7057+2.*ALOG((xm+r3)/(r5+r3)))-1.6449
      endif
      GM=ALOG(xm+r3)-.5*GS*GS

*     calculate the standard deviation for this month --------------------------
      xs = xm * SQRoot3( 105906, EXP ( GS * GS) - 1.0 )
      endif

*     accumulate for the annual mean (over all 12 months) ----------------------
      xtemp1 = xtemp1 + xm * fraction of year(i)

*     accumulate for the variance (to calculate annual 95-percentile low flow) -
      xtemp2 = xtemp2 + (xs/xm) * fraction of year (i)
      endif
   32 continue
*     end of the loop on months ------------------------------------------------

*     compute the annual mean and the percentiles ------------------------------
      xm = xtemp1
      xt = xtemp2
      xt = xt * xm
      x5 = 0.0
      x1 = 0.0
      x10= 0.0

      if ( xm * xm + xt * xt .gt. 1.0e-10) then
      GM = ALOG ( xm * xm / SQRoot(106042, xm * xm + xt * xt ) )
      GS = SQRoot(1061, ALOG ( 1.0 + ( xt * xt ) / ( xm * xm) ) )

      x5 = amax1 ( exp ( gm - 1.6449 * gs ), 0.001 * xm )
      x1 = amax1 ( exp ( gm - 2.3263 * gs ), 0.001 * xm )
      x10= amax1 ( exp ( gm - 1.2815 * gs ), 0.001 * xm )
      endif

*     store the annual mean and the 95-percentile low flow ---------------------
      F ( ivect, 1 ) = xm
      F ( ivect, 2 ) = x5
      
      MKF(IVECT) = IVECT
      return

 7566 write( *,7168) FLMONTHsmall(1,seasd)
      write(01,7168) FLMONTHsmall(1,seasd)
      write(09,7168) FLMONTHsmall(1,seasd)
      write(33,7168) FLMONTHsmall(1,seasd)
 7168 format(77('-')/
     &'*** Error in river flow data ...'/
     &'*** Error in reading monthly datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)IVECT
      write(01,8314)IVECT
      write(09,8314)IVECT
      write(33,8314)IVECT
 8314 format(77('-')/
     &'*** Error in reading monthly river flow data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
			
      end




*     read the monthly structure on river flow -------------------------- type 8
      subroutine get monthly structure file for river flow ! ------------ type 8
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     check the number of shots ================================================ 
      NSold = NS
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
      NS = NUS
      call set indices for percentiles etc ! ------------------------ river flow
      endif ! if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) +++++++++++++++
      
      do is = 1, NS
      YY(is) = 0.0
      enddo

*     add 1 to the counter of monthly structure sets ---------------------------
      struckd = struckd + 1 ! for river flow

*     check there is no violation of SIMCAT's storage --------------------------
      if ( struckd .gt. M9 ) then
      write( *,9262)M9
      write(01,9262)M9
      write(09,9262)M9
      write(33,9262)M9
 9262 format(77('-')/
     &'*** Too many data-sets on monthly structure ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading river flow data ...'/
     &'*** Increase value of M9 in COM.FOR ...'/77('-'))
      endif

*     read the line of giving the monthly structure data -----------------------
      read(02,*,ERR = 8313) IVECT, PDRF(IVECT), FLSTRUCT(1,struckd),
     &F(IVECT,MO),uname(ivect)

*     store the dataset numbers for the monthly structure data -----------------
      istruct (1,struckd,1) = IVECT ! for river flow

*     check and correct default correlation ------------------------------------
      if ( F(IVECT,MO) .gt. 1.0 .or. F(IVECT,MO) .lt. -1.0 ) then
      F(IVECT,MO) = -9.9
      endif

*     add the folder name to the file name -------------------------------------
      call add folder for monthly structure file (1)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = FLSTRUCT(1,struckd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(FLSTRUCT(1,struckd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ======== river flow ========= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(FLSTRUCT(1,struckd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = FLSTRUCT(1,struckd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,7163) FLSTRUCTsmall(1,struckd)
      call set screen text colour
      write(01,7163) FLSTRUCTsmall(1,struckd)
      write(09,7163) FLSTRUCTsmall(1,struckd)
      write(33,7163) FLSTRUCTsmall(1,struckd)
 7163 format(77('-')/
     &'*** Error in monthly structure river of flow data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      

*     get the file containing the monthly structure data -----------------------
      open(11,FILE = FLSTRUCT(1,struckd), STATUS='OLD')
      
      call read monthly structure river flow data 8 ! type 8
     &(1,0,struckd,tmean,t95,t3,tcorr,itest12)

      close (11)

*     check for zero 95-percentile low flows -----------------------------------
      do i = 1,12
      if ( struct2(i) .lt. 0.005 * struct1(i) ) then
      struct2(i) = 0.005 * struct1(i)
      endif
      enddo

*     check need to revert to annual data --------------------------------------
      if ( itest12 .eq. 12 ) then
      PDRF (IVECT) = 2
      F (IVECT,1) = tmean
      F (IVECT,2) = t95
      F (IVECT,3) = t3
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then
      tcorr = -9.9
      endif
      F (IVECT,4) = tcorr
      return
      endif

      F(IVECT,1) = tmean
      F(IVECT,2) = t95
      F(IVECT,3) = t3
      F(IVECT,4) = tcorr
      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- FRAN
      
*     sample the distributions of data on river flow ---------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      
      YY(is) = 0.0

      RFM = struct1 (imonth) ! mean flow ---------------------------------------
      if ( RFM .gt. 1.0e-9 ) then
      RF5 = t95 * RFM / tmean ! 95-percentile low flow -------------------------
      RF3 = struct3 (imonth) ! shift river flow --------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
      rex = 1.0e-9
      if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(3119,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG (RFM+RF3) - 0.5*GRFS*GRFS

      fdms = FRAN function (is) ! get the random normal deviate ----------------
*     compute the value of the shot --------------------------------------------
      YY(is) = EXP ( GRFM + fdms * GRFS) - RF3
      endif
      enddo
      
      call statistics for monthly river flows (1, CM1, CM2 )

      do imon = 1, 12
      BSM(imon) = tmean / FLOW(1)  
      BSS(imon) = t95   / FLOW(2)  
      enddo

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river flow ---------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      YY(is) = 0.0

      RFM = struct1 (imonth) * BSM(1) ! mean flow ------------------------------
      if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) * BSS(1) ! 95-percentile low flow -----------------
      RF5 = amin1 (RF5,0.99*RFM)
      RF3 = struct3 (imonth) ! shift river flow --------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
      rex = 1.0e-9
      if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3

      GRFS = SQRoot2 (321944,2.7057+2.*ALOG((RFM+RF3)/(rex)), itest) 
     &     - 1.6449

      if ( itest .eq. 1 ) then
      write( *,7263) FLSTRUCTsmall(1,struckd),RFM,RF5,BSM(1),BSS(1)
      write(01,7263) FLSTRUCTsmall(1,struckd),RFM,RF5,BSM(1),BSS(1)
      write(09,7263) FLSTRUCTsmall(1,struckd),RFM,RF5,BSM(1),BSS(1)
      write(33,7263) FLSTRUCTsmall(1,struckd),RFM,RF5,BSM(1),BSS(1)
 7263 format(77('-')/
     &'*** Error in setting up monthly structure for river flow ',
     &'data ...'/
     &'*** Reverted to annual log-normal annual data ...',
     &' File: ',a64/
     &'*** With imposed general monthly structure ...'/
     &'*** Mean =',f12.4,'  95-percentile =',f12.4/
     &'*** Scale mean =',f12.4,'  Scale 95-percentile =',f12.4/77('-'))
      PDRF (IVECT) = 2
      F (IVECT,1) = tmean
      F (IVECT,2) = t95
      F (IVECT,3) = t3
      call stop
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then
      tcorr = -9.9
      endif
      F (IVECT,4) = tcorr
      return
      endif

      GRFM = ALOG(RFM+RF3) - 0.5*GRFS*GRFS

      fdms = FRAN function (is) ! get the random normal deviate ----------------
*     compute the value of the shot --------------------------------------------
      YY(is) = VALFL month ( fdms,GRFS,GRFM,RF3)
      YY(is) = amax1 (0.0, YY(is))
      endif
      enddo

      call statistics for monthly river flows (2, CM1, CM2 )

      BSM(1) = BSM(1) * tmean  / CM1 
      BSS(1) = BSS(1) * t95    / CM2 

      rat1 = CM1 / tmean
      rat2 = CM2 / t95

      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
      endif
 5558 continue
      
      call statistics for monthly river flows (1, CM1, CM2 )
      
      if ( iterate .gt. 99 ) then
      suppress9 = suppress9 + 1 ! failure to set up monthly flows
      if ( ifbatch .ne. 1 ) then
      call change colour of text (12) ! orange
      write( *,5866) FLSTRUCTsmall(1,struckd)
 5866 format('*** Failed to set up the monthly ',
     &'structure for river flow ... FILE: ',a64)
      call set screen text colour
      endif
      write(01,5266) FLSTRUCTsmall(1,struckd),100.0*rat2
      write(09,5266) FLSTRUCTsmall(1,struckd),100.0*rat2
      write(33,5266) FLSTRUCTsmall(1,struckd),100.0*rat2
 5266 format(77('-')/'*** Failure in setting up the monthly ',
     &'structure for river flow ...'/
     &'*** FILE: ',a64/
     &'*** The fit to the annual data is ...',f8.1,' percent ...'/
     &'*** Reverted to the annual data ... '/77('-'))
      PDRF (IVECT) = 2
      F (IVECT,1) = tmean
      F (IVECT,2) = t95
      F (IVECT,3) = t3
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then
      tcorr = -9.9
      endif
      F (IVECT,4) = tcorr
      NS = NSold
      call set indices for percentiles etc ! ------------------------ river flow
      return
      endif ! if ( iterate .gt. 99 ) -------------------------------------------

      call statistics for monthly river flows (0, CM1, CM2 )
      do is = 1, NS
      FMS(is) = YY(IS)
      enddo
      NS = NSold ! replace the number of shots
      call set indices for percentiles etc ! ------------------------ river flow
      
      MKF(IVECT) = IVECT
      return
      
 1000 write( *,7168) FLSTRUCTsmall(1,struckd)
      write(01,7168) FLSTRUCTsmall(1,struckd)
      write(09,7168) FLSTRUCTsmall(1,struckd)
      write(33,7168) FLSTRUCTsmall(1,struckd)
 7168 format(77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading monthly structure datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)IVECT
      write(01,8314)IVECT
      write(09,8314)IVECT
      write(33,8314)IVECT
 8314 format(77('-')/
     &'*** Error in reading monthly structure river flow data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end





*     read the monthly structure on river quality ----------------------- type 8
      subroutine get monthly structure file for river quality ! --------- type 8 
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly structure data-sets ----------------------
      struckd = struckd + 1 ! for river quality

*     check there is no violation of SIMCAT's storage --------------------------
      if ( struckd .gt. M9 ) then
      write( *,9262)M9
      write(01,9262)M9
      write(09,9262)M9
      write(33,9262)M9
 9262 format(77('-')/'*** Too many data-sets monthly structure ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading river quality data ...'/
     &'*** Increase value of parameter - M9 in COM.FOR ...'/77('-'))
      endif

*     read the line of giving the monthly structure data -----------------------
      read(02,*,ERR = 8313) NUMV,NUMP,PDRC(NUMV,NUMP),
     &FLSTRUCT(2,struckd),X(MO)

*     store the dataset number -------------------------------------------------
      istruct (1,struckd,NUMP+1) = NUMV

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

*     add the folder name to the file name -------------------------------------
      call add folder for monthly structure file (2)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = FLSTRUCT(2,struckd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      call change colour of text (20) ! bright red
      call set screen text colour
      read(FLSTRUCT(2,struckd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ======== river quality ====== add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(FLSTRUCT(2,struckd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = FLSTRUCT(2,struckd), EXIST = exists )
      if ( .NOT. exists) then ! ===================== file not in the NPD folder
      call change colour of text (20) ! bright red
      write( *,7163) FLSTRUCTsmall(2,struckd)
      call set screen text colour
      write(01,7163) FLSTRUCTsmall(2,struckd)
      write(09,7163) FLSTRUCTsmall(2,struckd)
      write(33,7163) FLSTRUCTsmall(2,struckd)
 7163 format(77('-')/
     &'*** ERROR in monthly structure of river quality data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
          

*     get the file containing the monthly structure ----------------------------
      open(11,FILE = FLSTRUCT(2,struckd), STATUS='OLD') ! ----------- DAT folder
      

      call read monthly structure ! --------------------- river quality - type 8
     &(2,0,struckd,tmean,tstdev,t3,tcorr,itest12,1)
      close (11)

*     check the need to revert to the annual data ------------------------------
      if ( itest12 .eq. 12 ) then
      PDRC (NUMV, NUMP) = itdist ! revert to annual type -----------------------
      quolity data (NUMV, NUMP, 1) = tmean ! mean ------------------------------
      quolity data (NUMV, NUMP, 2) = tstdev ! standard deviation ---------------
      quolity data (NUMV, NUMP, 3) = t3 ! shift --------------------------------
      QNUM(NUMV,NUMP) = MAX0( 12, QNUM(NUMV,NUMP) )
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then !  correct the correlation
      tcorr = -9.9
      endif
      quolity data (NUMV, NUMP, MO) = tcorr ! set correlation ------------------
      return
      endif ! if ( itest12 .eq. 12 ) check the need to revert to the annual data

      quolity data (NUMV, NUMP, 1) = tmean
      quolity data (NUMV, NUMP, 2) = tstdev
      quolity data (NUMV, NUMP, 3) = t3
      quolity data (NUMV, NUMP, 4) = tcorr
      QNUM(NUMV,NUMP) = MAX0( 12, QNUM(NUMV,NUMP) )
      return

 1000 write( *,7168) FLSTRUCTsmall(1,struckd)
      write(01,7168) FLSTRUCTsmall(1,struckd)
      write(09,7168) FLSTRUCTsmall(1,struckd)
      write(33,7168) FLSTRUCTsmall(1,struckd)
 7168 format(77('-')/
     &'*** Error in river quality data ...'/
     &'*** Error in reading a monthly structure datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)NUMV
      write(01,8314)NUMV
      write(09,8314)NUMV
      write(33,8314)NUMV
 8314 format(77('-')/
     &'*** ERROR in reading monthly structure river quality data ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      write(01,6511)numv,nump,PDRC(NUMV,NUMP),FLSTRUCTsmall(2,struckd),
     &MO,X(MO)
 6511 format('NUMV,NUMP =',3i8,3x,a30,i4,f9.3)
      call stop
      end



*     read the monthly structure data on temperature -------------------- type 8
      subroutine get monthly structure file for background ! ------------ type 8   
     &(KREACH,ldet)
      include 'COMMON DATA.FOR'
      logical exists
      character *20 tname
      character *1 a(136),b(136)

*     add 1 to the counter of monthly structure data-sets for temperature ------
      TEMPD = TEMPD + 1

*     check there is no violation of SIMCAT's storage --------------------------
      if ( TEMPD .gt. M10 ) then
      write( *,9262)M10
      write(01,9262)M10
      write(09,9262)M10
      write(33,9262)M10
 9262 format(110('-')/
     &'*** Too many data-sets monthly structure on background ...',
     &' The maximum is',I3/
     &'*** Maximum exceeded whilst reading background data ... '/
     &'*** Temperature or suspended solids for reaches ...'/
     &'*** Increase value of parameter - M10 in COM.FOR ...'/110('-'))
      endif

*     read the line of giving the monthly data ---------------------------------
      read(02,*,ERR = 8313) tname,PDBC(Kreach,ldet),fltemp(ldet,TEMPD)

*     store the dataset number -------------------------------------------------
      ITEMPP (1,TEMPD,ldet) = KREACH

*     add the folder name to the file name -------------------------------------
      call add folder to the name of the file (ldet)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = fltemp(ldet,TEMPD), EXIST = exists )
      
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      call change colour of text (20) ! bright red
      call set screen text colour
      read(fltemp(ldet,TEMPD),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ======== river quality ====== add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(fltemp(ldet,TEMPD),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = fltemp(ldet,TEMPD), EXIST = exists )
      if ( .NOT. exists) then ! ===================== file not in the NPD folder
      call change colour of text (20) ! bright red
      write( *,7163) fltemp(ldet,TEMPD)
      call set screen text colour
      write(01,7163) fltemp(ldet,TEMPD)
      write(09,7163) fltemp(ldet,TEMPD)
      write(33,7163) fltemp(ldet,TEMPD)
 7163 format(77('-')/
     &'*** ERROR in monthly structure of background data ...'/
     &'*** Temperature or suspended solids for reaches ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      

*     get the file containing the monthly structure data -----------------------
      open(11,FILE = fltemp(ldet,TEMPD), STATUS='OLD')

      call read monthly structure background data ! type 8 - get monthly 
     &(ldet,1,tmean,tstdev,t3,tcorr,itest12)

*     check need to revert to annual data --------------------------------------
      if ( itest12 .eq. 12 ) then
      PDBC (kreach, ldet) = 2
      BMAT (kreach, ldet, 1) = tmean
      BMAT (kreach, ldet, 2) = tstdev
      BNUM (kreach, ldet) = MAX0( 12, BNUM (kreach, ldet) )
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then
      tcorr = -9.9
      endif
      BMAT (kreach, ldet, 3) = tcorr
      return
      endif

      BMAT (kreach, ldet, 1) = tmean
      BMAT (kreach, ldet, 2) = tstdev
      BMAT (kreach, ldet, 3) = tcorr
      BNUM (kreach, ldet) = MAX0( 12, BNUM (kreach, ldet) )
      return

 1000 write( *,7168) fltempsmall(ldet,TEMPD)
      write(01,7168) fltempsmall(ldet,TEMPD)
      write(09,7168) fltempsmall(ldet,TEMPD)
      write(33,7168) fltempsmall(ldet,TEMPD)
 7168 format(77('-')/'*** Error in background data for reaches ...'/
     &'*** Temperature or suspended solids for reaches ...'/
     &'*** Error in reading a monthly structure datafile ...'/
     &'*** Check the file ',a30/77('-'))
      call stop

 8313 write( *,8314)KREACH
      write(01,8314)KREACH
      write(09,8314)KREACH
      write(33,8314)KREACH
 8314 format(77('-')/
     &'*** Error in reading monthly structure background data ...'/
     &'*** Temperature or suspended solids for reaches ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check data for reach number number',i6/77('-'))
      call stop
      end






*     read non-parametric data on river quality ------------------------- type 4
      subroutine get a non parametric file for river quality ! ---------- type 4
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)
      
*     add 1 to the counter of non-parametric data-sets -------------------------
      NONPD = NONPD + 1

      if ( NONPD .gt. M7 ) then ! there are too many data-sets -----------------
      write( *,9462)M7
      write(01,9462)M7
      write(09,9462)M7
      write(33,9462)M7
 9462 format(77('-')/
     &'*** Too many data-sets are non-parametric ...'/
     &'*** Maximum (M7) is',I6/
     &'*** Maximum exceeded whilst reading river quality data '/
     &77('-'))
      endif
      
      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1311) NUMV,NUMP,PDRC(NUMV,NUMP),
     &Flname(2,NONPD),X(MO),QNUM(NUMV,NUMP)
      goto 2314
 1311 read(line of data,*,ERR = 8313) NUMV,NUMP,PDRC(NUMV,NUMP),
     &Flname(2,NONPD),X(MO)
 2314 continue
      
      if ( qtype (nump) .eq. 4 ) return
*     store the dataset number -------------------------------------------------
      IDENP (1,NONPD, NUMP+1 ) = NUMV

*     add directory to file name ----------------------------- in the DAT folder
      call add folder for non parametric data file (2) ! ----- in the DAT folder

*     check whether the datafile exists ---------------------- in the DAT folder 
      inquire( FILE = Flname(2,nonpd), EXIST = exists ) ! ---- in the DAT folder
      
      
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      call change colour of text (20) ! bright red
      call set screen text colour
      read(Flname(2,nonpd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(Flname(2,nonpd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = Flname(2,nonpd), EXIST = exists )
      if ( .NOT. exists) then ! ===================== file not in the NPD folder
      call change colour of text (20) ! bright red
      write( *,8563) flnamesmall(2,nonpd)
 8563 format('*** Error in river quality data: ',
     &'non-parametric data file does not exist ... ',a64)
      call set screen text colour
      write(01,7563) flnamesmall(2,nonpd)
      write(09,7563) flnamesmall(2,nonpd)
      write(33,7563) flnamesmall(2,nonpd)
 7563 format(77('-')/'*** Error in river quality data ... '/
     &'*** Non-parametric distribution ... '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      if ( ifbatch .eq. 1 ) return
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      

*     open the file ================================== file is in the NPD folder
      open(12,FILE = Flname(2,nonpd), STATUS='OLD') !  file is in the NPD folder
*     read the file ================================== file is in the NPD folder
      read(12, *, ERR=7566) nprf  ! ================== file is in the NPD folder
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(2,nonpd) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ?
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(2,nonpd)
      write(01,7166) flnamesmall(2,nonpd)
      write(09,7166) flnamesmall(2,nonpd)
      write(33,7166) flnamesmall(2,nonpd)
 7166 format(77('-')/
     &'*** Error in river quality data ....                   '/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(2,nonpd)
      write(01,7167) flnamesmall(2,nonpd)
      write(09,7167) flnamesmall(2,nonpd)
      write(33,7167) flnamesmall(2,nonpd)
 7167 format(77('-')/
     &'*** Error in river quality data .....                  '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7566) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(2,nonpd) = 1
      endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(2,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
      endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (nump) = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
      enddo
      if ( imark .gt. 1 ) then
      if ( imark .le. nprf ) then
      cut off zero quality (nump) = 0.5 * (rfnpfd(imark) 
     &+ rfnpfd(imark - 1))
      else
      cut off zero quality (nump) = rfnpfd(nprf)
      endif
      endif
      if ( cut off zero quality (nump) .gt. 1.0e-09 ) then
*     write(01,2376)cut off zero quality (nump),flnamesmall(2,nonpd)
 2376 format('Cut off percentile for river quality =',f12.6,3x,a64)
      endif
      IQ = NUMV
      JP = NUMP

*     compute mean and standard deviation --------------------------------------
      rcm = 0.0
      rcs = 0.0
      do i = 1, nprf
      rcm = rcm + rfnpvl (i)
      rcs = rcs + rfnpvl (i) * rfnpvl (i)
      enddo

      rcs = (rcs-rcm*rcm/nprf)/(nprf-1)
      if ( rcs .lt. 1.0E-10 ) then
      rcs = 0.0
      else
      rcs = SQRoot(1062,rcs)
      endif
      rcm = rcm / float (nprf)

*     set mean (as indicator that valid data have been read) -------------------
      quolity data (NUMV, NUMP, 1) = rcm
*     set standard deviation (as indicator that valid data have been read) -----
      quolity data (NUMV, NUMP, 2) = rcs
      
*     compute an estimate of the 95-percentile ---------------------------------
*     use the Weibull percentile point -----------------------------------------
      if ( Qtype (NUMP) .ne. 3 .and. Qtype(NUMP) .ne. 5 ) then
      ind95 = amin0( nprf, int ( 0.95 * (nprf+1) ) )
      else
      ind95 = amax0( 1, nprf + 1 - int ( 0.95 * (nprf + 1) ) )
      endif
      quolity data (NUMV, NUMP, 3) = rfnpvl ( ind95 )

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif
      quolity data (NUMV, NUMP, MO) = X(MO)

*     truncate number of samples to 12 (for confidence limits) -----------------
      QNUM(NUMV,NUMP) = MAX0( 12, QNUM(NUMV,NUMP) )

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( QNUM(NUMV,NUMP) .gt. 500 ) then
      QNUM(NUMV,NUMP) = 12
      endif

*     get ready for the next record for river quality --------------------------
      KPAR = KPAR+1
      if ( KPAR .gt. MPAR ) KE = -9
      return

 7566 write( *,7168)flnamesmall(2,nonpd)
      write(01,7168)flnamesmall(2,nonpd)
      write(09,7168)flnamesmall(2,nonpd)
      write(33,7168)flnamesmall(2,nonpd)
 7168 format(77('-')/'*** Error in river quality data ...'/
     &'*** Error in reading non-parametric datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)NUMV
      write(01,8314)NUMV
      write(09,8314)NUMV
      write(33,8314)NUMV
 8314 format(77('-')/
     &'*** Error in reading monthly river quality data-sets ... '/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end




*     read the monthly data on river quality ---------------------------- type 5
      subroutine get monthly file for river quality ! ------------------- type 5
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets --------------------------------
      SEASD = SEASD + 1
      
*     check there is no violation of SIMCAT's storage --------------------------
      if ( SEASD .gt. M8 ) then
      write( *,9269)M8
      write(01,9269)M8
      write(09,9269)M8
      write(33,9269)M8
 9269 format(77('-')/
     &'*** Too many data-sets are monthly ... '/
     &'*** Maximum (M8) is',I6/
     &'*** Maximum exceeded whilst reading river quality data  '/
     &77('-'))
      endif

*     read the line of monthly data on river quality ---------------------------
      read(02,*,ERR = 8313,END = 8313) NUMV, NUMP, PDRC(NUMV,NUMP),
     &flmonth(2,seasD),X(MO)

      quolity data (NUMV, NUMP, MO) = X(MO)

*     store the dataset number -------------------------------------------------
      ISEASP (1,SEASD,NUMP+1) = NUMV

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif
      quolity data (NUMV, NUMP, MO) = X(MO) ! correlation --------------------------

*     add the folder name to the file name -------------------------------------
      call add folder for monthly data file (2) ! river quality - type 5


*     check whether the datafile exists ----------------------------------------
      inquire( FILE = flmonth(2,SEASD), EXIST = exists )

      if ( .NOT. exists) then ! ========================= look in the NPD folder
      call change colour of text (20) ! bright red
      call set screen text colour
      read(flmonth(2,SEASD),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      write(flmonth(2,SEASD),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = flmonth(2,SEASD), EXIST = exists )
      if ( .NOT. exists) then ! ===================== file not in the NPD folder
      call set screen text colour
      call change colour of text (20) ! bright red
      write( *,7167) FLMONTHsmall(2,SEASD)
      call set screen text colour
      write(01,7167) FLMONTHsmall(2,SEASD)
      write(09,7167) FLMONTHsmall(2,SEASD)
      write(33,7167) FLMONTHsmall(2,SEASD)
 7167 format(77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      if ( ifbatch .eq. 1 ) return
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================

          
          

*     open the file ------------------------------------------------------------
      open(11,FILE = flmonth(2,SEASD), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (2,SEASD) ! river quality - type 5

      xtemp1 = 0.0
      xtemp2 = 0.0
      do i = 1, 12
      xtemp1 = xtemp1 + seas1(i)
      xtemp2 = xtemp2 + seas2(i) 
      enddo

      quolity data (NUMV,NUMP,1) = xtemp1 / 12.0
      quolity data (NUMV,NUMP,2) = xtemp2 / 12.0
      return

 7566 write( *,7168) FLMONTHsmall(2,seasd)
      write(01,7168) FLMONTHsmall(2,seasd)
      write(09,7168) FLMONTHsmall(2,seasd)
      write(33,7168) FLMONTHsmall(2,seasd)
 7168 format(77('-')/'*** Error in river quality data ...'/
     &'*** Error in reading monthly datafile ... '/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314) numv
      write(01,8314) numv
      write(09,8314) numv
      write(33,8314) numv
 8314 format(77('-')/
     &'*** Error in reading monthly river quality data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end








*     non-parametric distributions for discharge flow ------------------- type 4
      subroutine get non parametric file for discharge flow ! ----------- type 4
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of non-parametric data-sets -------------------------
      NONPD = NONPD + 1

      if ( NONPD .gt. M7 ) then
      write( *,9482)M7
      write(01,9482)M7
      write(09,9482)M7
      write(33,9482)M7
 9482 format(77('-')/
     &'*** Too many data-sets are non-parametric ...'/
     &'*** Maximum (M7) is',I6/
     &'*** Maximum exceeded whilst reading discharge flow data'/
     &'*** Increase value of Parameter - M7 in COM.FOR ... '/77('-'))
      endif

      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1312,END = 1312) IE,NUMP,PDEF(IE),
     &Flname(3,NONPD),FE(IE,MO),JNUM
      goto 2314
 1312 continue
      read(line of data,*,ERR = 8136,END = 8136) IE,NUMP,PDEF(IE),
     &Flname(3,NONPD),FE(IE,MO)
      JNUM = 24
 2314 continue

*     set mean flow (to act as flag that data have been entered) ---------------
      FE(IE,1) = 0.0
      FE(IE,2) = 0.0

*     check and correct default correlation ------------------------------------
      if ( FE(IE,MO) .gt. 1.0 .or. FE(IE,MO) .lt. -1.0 ) then
      FE(IE,MO) = -9.9
      endif

      ENUM(IE) = JNUM
      if ( ENUM(IE) .le. 1 ) ENUM(IE) = 365

*     store the dataset number -------------------------------------------------
      IDENP (2,NONPD,1) = IE

*     add directory to file name -----------------------------------------------
      call add folder for non parametric data file (3)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = Flname(3,nonpd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(Flname(3,nonpd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(Flname(3,nonpd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = Flname(3,nonpd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,8563) flnamesmall(3,nonpd)
 8563 format('*** Error in discharge flow data: '/
     &'non-parametric data file does not exist ... ',a64)
      call set screen text colour
      write(01,1563) flnamesmall(3,nonpd)
      write(09,1563) flnamesmall(3,nonpd)
      write(33,1563) flnamesmall(3,nonpd)
 1563 format(77('-')/
     &'*** Error in non-parametric discharge flow data (9)'/
     &'*** Non-parametric distribution ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(3,nonpd), STATUS='OLD') ! non-par discharge flow

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7566) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(3,nonpd) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,1166) flnamesmall(3,nonpd)
      write(01,1166) flnamesmall(3,nonpd)
      write(09,1166) flnamesmall(3,nonpd)
      write(33,1166) flnamesmall(3,nonpd)
 1166 format(77('-')/
     &'*** Error in non-parametric discharge flow data (10)'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,1167) flnamesmall(3,nonpd)
      write(01,1167) flnamesmall(3,nonpd)
      write(09,1167) flnamesmall(3,nonpd)
      write(33,1167) flnamesmall(3,nonpd)
 1167 format(77('-')/
     &'*** Error in non-parametric discharge flow data (11)'/
     &'*** Too few data values specified for non-parametric ...  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
      read(12, *, ERR=1566) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(3,nonpd) = 1
      endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(3,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
      endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
      enddo
      if ( imark .gt. 1 ) then
      if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd(imark) + rfnpfd(imark - 1))
      else
      cut off zero flow = rfnpfd(nprf)
      endif
      endif
*     write(01,2376)cut off zero flow,flnamesmall(3,nonpd)
 2376 format('Cut off zero for discharge flow =',f12.6,1x,a64)

*     compute mean and standard deviation --------------------------------------
      efm = 0.0
      efs = 0.0
      do i = 1, nprf
      efm = efm + rfnpvl (i)
      efs = efs + rfnpvl (i) * rfnpvl (i)
      enddo
      efs=(efs-efm*efm/nprf)/(nprf-1)
      if (efs .lt. 1.0E-10) then
      efs = 0.0
      else
      efs = SQRoot(1065,efs)
      endif
      efm = efm / float (nprf)

*     set mean (as indicator that valid data have been read) -------------------
      FE (IE, 1) = efm

*     set standard deviation (as indicator that valid data have been read) -----
      FE (IE, 2) = efs

*     ready for the next record (discharge flow & quality) ---------------------
      KPAR = KPAR+1
      if ( KPAR .gt. MPAR ) KE = -9
      return

 1566 continue
      write(01,1168) flnamesmall(3,nonpd)
      write(09,1168) flnamesmall(3,nonpd)
      write(33,1168) flnamesmall(3,nonpd)
 1168 format(77('-')/
     &'*** Error in non-parametric discharge flow data (12)'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))

 8136 write( *,8137)IE
      write(01,8137)IE
      write(09,8137)IE
      write(33,8137)IE
 8137 format(77('-')'*** Error in effluent flow data-sets ... '/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Error is for a non-parametric distribution ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop

 7566 continue
      write( *,7168) flnamesmall(3,nonpd)
      write(01,7168) flnamesmall(3,nonpd)
      write(09,7168) flnamesmall(3,nonpd)
      write(33,7168) flnamesmall(3,nonpd)
 7168 format(77('-')/
     &'*** Error in non-parametric discharge flow data (13)'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      write( *,2968) flnamesmall(3,nonpd),nprf
      write(09,2968) flnamesmall(3,nonpd),nprf
      write(01,2968) flnamesmall(3,nonpd),nprf
      write(33,2968) flnamesmall(3,nonpd),nprf
 2968 format(77('-')/
     &'*** Error in non-parametric discharge flow data (14) (NPAREF)'/
     &'*** Error in reading non-parametric data ... '/
     &'*** Check the file ',a64/
     &'*** Number of data values =',i7/77('-'))
      call stop
      end






*     read the monthly data on the discharge flow ----------------------- type 5
      subroutine get monthly file for discharge flow ! ------------------ type 5
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets --------------------------------
      SEASD = SEASD + 1

*     check there is no violation of SIMCAT's storage --------------------------
      if ( SEASD .gt. M8 ) then
      write( *,9262)M8
      write(01,9262)M8
      write(09,9262)M8
      write(33,9262)M8
 9262 format(77('-')/'*** Too many data-sets are monthly ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading discharge flow data'/
     &'*** Increase value of Parameter - M8 in COM.FOR ...'/77('-'))
      endif

*     read the line of monthly data --------------------------------------------
      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1312,END = 1312) IE,NUMP,PDEF(IE),
     &flmonth(3,seasD),FE(IE,MO),JNUM
      goto 2314

 1312 continue
      read(line of data,*,ERR = 7566,END = 7566) IE,NUMP,PDEF(IE),
     &flmonth(3,seasD),FE(IE,MO)
      jnum = 365
      call change colour of text (20) ! bright red
      write( *,8764) FLMONTH(3,SEASD)
 8764 format('*** Number of samples not set for effluent ',
     &'flow    ...',7x,'set to 365 for ...',26x,'FILE: ',a64)
      call set screen text colour
      write(01,7763) FLMONTH(3,SEASD)
      write(09,7763) FLMONTH(3,SEASD)
 7763 format(77('-')/'*** Potential error in monthly effluent ',
     &'flow data ...'/
     &'*** Missing item on sample numbers was set to 365 for the ',
     &'file named ... '/'*** ',a64/
     &'*** Add item on number of samples for to this file ... '/77('-'))
 2314 continue

*     store the dataset number -------------------------------------------------
      ISEASP (2,SEASD,NUMP+1) = IE

*     check and correct default correlation ------------------------------------
      if ( FE(IE,MO) .gt. 1.0 .or. FE(IE,MO) .lt. -1.0 ) then
      FE(IE,MO) = -9.9
      endif
      
      ENUM(IE) = JNUM
      if ( ENUM(IE) .le. 1 ) ENUM(IE) = 365

*     add the folder name to the file name -------------------------------------
      call add folder for monthly data file (3)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = FLMONTH(3,seasd), EXIST = exists )
      
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(FLMONTH(3,seasd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(FLMONTH(3,seasd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = FLMONTH(3,seasd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,7163) FLMONTHsmall(3,seasd)
      call set screen text colour
      write(01,7163) FLMONTHsmall(3,seasd)
      write(09,7163) FLMONTHsmall(3,seasd)
      write(33,7163) FLMONTHsmall(3,seasd)
 7163 format(77('-')/'*** Error in monthly effluent flow data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      
      

*     open the file ------------------------------------------------------------
      open(11,FILE = FLMONTH(3,seasd), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (3,seasd) ! discharge flow - type 5
      close (11)

*     write(01,4398)
*4398 format(//77('-')/
*    &'Monthly data on discharge flows ... '/77('-')/
*    &'Month        ','       Mean','   Standard','      Shift',
*    &'  Correlation'/25x,'  Deviation'/77('-'))

*     write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
*    &i=1,12)
*2399 format(
*    &'January ...  ',3f11.2,f13.4,i18/
*    &'February ... ',3f11.2,f13.4,i18/
*    &'March ...    ',3f11.2,f13.4,i18/
*    &'April ...    ',3f11.2,f13.4,i18/
*    &'May ...      ',3f11.2,f13.4,i18/
*    &'June ...     ',3f11.2,f13.4,i18/
*    &'July ...     ',3f11.2,f13.4,i18/
*    &'August ...   ',3f11.2,f13.4,i18/
*    &'September ...',3f11.2,f13.4,i18/
*    &'October ...  ',3f11.2,f13.4,i18/
*    &'November ... ',3f11.2,f13.4,i18/
*    &'December ... ',3f11.2,f13.4,i18/77('-')/)

      xtemp1 = 0.0
      xtemp2 = 0.0
      do i = 1, 12
      xtemp1 = xtemp1 + seas1(i)
      xtemp2 = xtemp2 + seas2(i) 
      enddo

      FE ( IE, 1 ) = xtemp1 / 12.0
      FE ( IE, 2 ) = xtemp2 / 12.0 
      return

 7566 write( *,7168) FLMONTHsmall(3,seasd)
      write(01,7168) FLMONTHsmall(3,seasd)
      write(09,7168) FLMONTHsmall(3,seasd)
      write(33,7168) FLMONTHsmall(3,seasd)
 7168 format(77('-')/'*** Error for effluent flow data ...',
     &'in reading in the line of data ...'/
     &'*** Check line containing the file name ',a64/77('-'))
      call stop

 8313 write( *,8314) IE
      write(01,8314) IE
      write(09,8314) IE
      write(33,8314) IE
 8314 format(77('-')/
     &'*** Error in reading monthly effluent flow data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end







*     read the monthly structure data on the discharge flow ------------- type 8
      subroutine get monthly structure file for discharge flow ! -------- type 8
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets --------------------------------
      struckd = struckd + 1 ! for discharge flow

*     check there is no violation of SIMCAT's storage --------------------------
      if ( struckd .gt. M8 ) then
      write( *,9262)M8
      write(01,9262)M8
      write(09,9262)M8
      write(33,9262)M8
 9262 format(77('-')/'*** Too many monthly structure data-sets ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading discharge flow data'/
     &'*** Increase value of Parameter - M8 in COM.FOR ... '/77('-'))
      endif

*     read the line of monthly structure data ---------------------------------
      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1312) IE,NUMP,PDEF(IE),
     &FLSTRUCT(3,struckd),FE(IE,MO),JNUM
      goto 2314
 1312 read(line of data,*,ERR = 7566) IE,NUMP,PDEF(IE),
     &FLSTRUCT(3,struckd),FE(IE,MO)
      jnum = 24
 2314 continue

*     look for illegal codes for distributions ---------------------------------
      if ( PDEF(IE) .lt. 0 .or. PDEF(IE) .gt. 9 ) then
      write(33,8551)IE
 8551 format(77('-')/'*** Illegal data for discharge flow ...',
     &'*** Check dataset number',i6/77('-'))
      endif

*     store the dataset number -------------------------------------------------
      istruct (2,struckd,NUMP+1) = IE

*     check and correct the default correlation --------------------------------
      if ( FE(IE,MO) .gt. 1.0 .or. FE(IE,MO) .lt. -1.0 ) then
      FE(IE,MO) = -9.9
      endif

      ENUM(IE) = JNUM
      if ( ENUM(IE) .le. 1 ) ENUM(IE) = 365

*     add the folder name to the file name -------------------------------------
      call add folder for monthly structure file (3)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = FLSTRUCT(3,struckd), EXIST = exists )
      
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(FLSTRUCT(3,struckd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(FLSTRUCT(3,struckd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = FLSTRUCT(3,struckd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,7163) FLSTRUCTsmall(3,struckd)
      call set screen text colour
      write(01,7163) FLSTRUCTsmall(3,struckd)
      write(09,7163) FLSTRUCTsmall(3,struckd)
      write(33,7163) FLSTRUCTsmall(3,struckd)
 7163 format(77('-')/'*** Error in monthly structure of effluent ',
     &'flow data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
    
      

*     open the file ------------------------------------------------------------
      open(11,FILE = FLSTRUCT(3,struckd), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly structure ! effluent flow - type 8
     &(3,0,struckd,tmean,tstdev,t3,tcorr,itest12,1)
      close (11)

*     check need to revert to annual data --------------------------------------
      if ( itest12 .eq. 12 ) then
      PDEF(IE) = 2 ! revert from monthly to annual data ------------------------
      FE(IE,1) = tmean
      FE(IE,2) = tstdev
      FE(IE,3) = t3
      if (tcorr .gt. 1.0 .or. tcorr .lt. -1.0) then
      tcorr = -9.9
      endif
      FE(IE,4) = tcorr
      return
      endif

*     write(01,4398)
*4398 format(//77('-')/
*    &'Monthly structure on discharge flows ... '/77('-')/
*    &'Month        ','       Mean','   Standard','      Shift',
*    &'  Correlation'/25x,'  Deviation'/77('-'))

*     write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
*    &i=1,12)
*2399 format(
*    &'January ...  ',3f11.2,f13.4,i18/
*    &'February ... ',3f11.2,f13.4,i18/
*    &'March ...    ',3f11.2,f13.4,i18/
*    &'April ...    ',3f11.2,f13.4,i18/
*    &'May ...      ',3f11.2,f13.4,i18/
*    &'June ...     ',3f11.2,f13.4,i18/
*    &'July ...     ',3f11.2,f13.4,i18/
*    &'August ...   ',3f11.2,f13.4,i18/
*    &'September ...',3f11.2,f13.4,i18/
*    &'October ...  ',3f11.2,f13.4,i18/
*    &'November ... ',3f11.2,f13.4,i18/
*    &'December ... ',3f11.2,f13.4,i18/77('-')/)

      FE( IE, 1 ) = tmean
      FE( IE, 2 ) = tstdev
      return

 7566 write( *,7168) FLSTRUCTsmall(3,struckd)
      write(01,7168) FLSTRUCTsmall(3,struckd)
      write(09,7168) FLSTRUCTsmall(3,struckd)
      write(33,7168) FLSTRUCTsmall(3,struckd)
 7168 format(77('-')/'*** Error for effluent flow data ... ',
     &'in reading monthly structure datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314)IE
      write(01,8314)IE
      write(09,8314)IE
      write(33,8314)IE
 8314 format(77('-')/
     &'*** Error in reading monthly structure effluent flow data ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end







*     non-parametric distributions ... for discharge quality ------------ type 4
      subroutine get non parametric file for discharge quality ! -------- type 4
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of non-parametric data-sets -------------------------
      NONPD = NONPD + 1

      if ( NONPD .gt. M7 ) then
      write( *,9483)
      write(01,9483)
      write(09,9483)
      write(33,9483)
 9483 format(77('-')/'*** Too many data-sets are non-parametric ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading discharge quality  '/
     &'*** Increase value of Parameter - M7 in COM.FOR ...'/77('-'))
      endif

      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1336) IE,NUMP,PDEC(IE,NUMP),
     &Flname(4,NONPD),X(MO),JNUM
      goto 2336
 1336 read(line of data,*,ERR = 8316) IE,NUMP,PDEC(IE,NUMP),
     &Flname(4,NONPD),X(MO)
      JNUM = 24

 2336 PNUM(IE,NUMP) = JNUM

      if ( qtype (nump) .eq. 4 ) return

*     truncate number of samples to 4 (for confidence limits) ------------------
      PNUM(IE,NUMP) = MAX0( 4,PNUM(IE,NUMP))

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( PNUM(IE,NUMP) .gt. 500 ) then
      PNUM (IE,NUMP) = 12
      endif

      pollution data (IE,NUMP,1) = 0.0
      pollution data (IE,NUMP,2) = 0.0

*     store the dataset number -------------------------------------------------
      IDENP (2,NONPD,NUMP+1) = IE

*     add directory to file name -----------------------------------------------
      call add folder for non parametric data file (4)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = Flname(4,nonpd), EXIST = exists )
      
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(Flname(4,nonpd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(Flname(4,nonpd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = Flname(4,nonpd), EXIST = exists )

      if ( .NOT. exists) then
      call change colour of text (20) ! bright red
      write( *,8563) flnamesmall(4,nonpd)
 8563 format('*** Error in discharge quality data: '/
     &'non-parametric data file does not exist ... ',a64)
      call set screen text colour
      write(01,7563) flnamesmall(4,nonpd)
      write(09,7563) flnamesmall(4,nonpd)
      write(33,7563) flnamesmall(4,nonpd)
 7563 format(77('-')/'*** Error in discharge quality data ... '/
     &'*** Non-parametric distribution ...  '/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      
      

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(4,nonpd), STATUS='OLD') ! non-par discharge quality

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7566) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(4,nonpd) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(4,nonpd)
      write(01,7166) flnamesmall(4,nonpd)
      write(09,7166) flnamesmall(4,nonpd)
      write(33,7166) flnamesmall(4,nonpd)
 7166 format(77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7967) flnamesmall(4,nonpd)
      write(01,7967) flnamesmall(4,nonpd)
      write(09,7967) flnamesmall(4,nonpd)
      write(33,7967) flnamesmall(4,nonpd)
 7967 format(77('-')/
     &'*** Error in non-parametric discharge quality data ... '/
     &'*** Too few data values specified for non-parametric    '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7566) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(4,nonpd) = 1
      endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(4,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
      endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (nump) = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
      enddo
      if ( imark .gt. 1 ) then
      if ( imark .le. nprf ) then
      cut off zero quality (nump) = 0.5 * (rfnpfd(imark) 
     &+ rfnpfd(imark - 1))
      else
      cut off zero quality (nump) = rfnpfd(nprf)
      endif
      endif
*     write(01,2376)cut off zero quality (nump),
*    &flnamesmall(4,nonpd),IE,NUMP
 2376 format('Cut off zero for DISCHARGE quality =',f12.6,1x,a64,2i4)

*     compute mean and standard deviation --------------------------------------
      ecm = 0.0
      ecs = 0.0
      do i = 1, nprf
      ecm = ecm + rfnpvl (i)
      ecs = ecs + rfnpvl (i) * rfnpvl (i)
      enddo

      ecs=(ecs-ecm*ecm/nprf)/(nprf-1)

      if (ecs .lt. 1.0E-10) then
      ecs = 0.0
      else
      ecs = SQRoot(1067,ecs)
      endif
      ecm = ecm / float (nprf)

*     set mean (as indicator that valid data have been read) -------------------
      pollution data (IE, NUMP, 1) = ecm

*     set standard deviation (as indicator that valid data have been read) -----
      pollution data (IE, NUMP, 2) = ecs

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

      pollution data (IE, NUMP, MO) = X(MO)

*     ready for the next record (discharge flow and quality) -------------------
      KPAR = KPAR+1
      if ( KPAR .gt. MPAR ) KE = -9
      return

 7566 write( *,7168) flnamesmall(4,nonpd)
      write(01,7168) flnamesmall(4,nonpd)
      write(09,7168) flnamesmall(4,nonpd)
      write(33,7168) flnamesmall(4,nonpd)
 7168 format(77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8316 write( *,8317)IE
      write(01,8317)IE
      write(09,8317)IE
      write(33,8317)IE
 8317 format(77('-')/'*** Error in effluent quality data-sets ...'/
     &'*** The data-file has been assembled incorrectly.....'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end





*     non-parametric distributions ... for discharge quality ------------ type 4
      subroutine get non parametric file for discharge load ! ----------- type 4
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of non-parametric data-sets -------------------------
      NONPD = NONPD + 1

      if ( NONPD .gt. M7 ) then
      write( *,9483)
      write(01,9483)
      write(09,9483)
      write(33,9483)
 9483 format(77('-')/'*** Too many data-sets are non-parametric ...'/
     &'*** The maximum is',I3/
     &'*** Maximum exceeded whilst reading discharge load  '/
     &'*** Increase value of Parameter - M7 in COM.FOR ...'/77('-'))
      endif

      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1336) IE,NUMP,PDEC(IE,NUMP),
     &Flname(4,NONPD),X(MO),JNUM
      goto 2336
 1336 read(line of data,*,ERR = 8316) IE,NUMP,PDEC(IE,NUMP),
     &Flname(4,NONPD),X(MO)
      JNUM = 24

 2336 PNUM(IE,NUMP) = JNUM

*     truncate number of samples to 4 (for confidence limits) ------------------
      PNUM(IE,NUMP) = MAX0( 4,PNUM(IE,NUMP))

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( PNUM(IE,NUMP) .gt. 500 ) then
      PNUM (IE,NUMP) = 12
      endif

      pollution data (IE,NUMP,1) = 0.0
      pollution data (IE,NUMP,2) = 0.0

*     store the dataset number -------------------------------------------------
      IDENP (2,NONPD,NUMP+1) = IE

*     add directory to file name -----------------------------------------------
      call add folder for non parametric data file (4)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = Flname(4,nonpd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(Flname(4,nonpd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(Flname(4,nonpd),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = Flname(4,nonpd), EXIST = exists )

      if ( .NOT. exists) then      
      call change colour of text (20) ! bright red
      write( *,7563) flnamesmall(4,nonpd)
      call set screen text colour
      write(01,7563) flnamesmall(4,nonpd)
      write(09,7563) flnamesmall(4,nonpd)
      write(33,7563) flnamesmall(4,nonpd)
 7563 format(77('-')/'*** Error in discharge load data ... '/
     &'*** Non-parametric distribution ...  '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...  '/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(4,nonpd), STATUS='OLD') ! non-par discharge load  

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7566) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(4,nonpd) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ?
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(4,nonpd)
      write(01,7166) flnamesmall(4,nonpd)
      write(09,7166) flnamesmall(4,nonpd)
      write(33,7166) flnamesmall(4,nonpd)
 7166 format(77('-')/
     &'*** Error in non-parametric discharge load data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7967) flnamesmall(4,nonpd)
      write(01,7967) flnamesmall(4,nonpd)
      write(09,7967) flnamesmall(4,nonpd)
      write(33,7967) flnamesmall(4,nonpd)
 7967 format(77('-')/
     &'*** Error in non-parametric discharge load data ... '/
     &'*** Too few data values specified for non-parametric    '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7566) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
      nprf = - nprf
      flsequence(4,nonpd) = 1
      endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(4,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
      endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (nump) = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
      enddo
      if ( imark .gt. 1 ) then
      if ( imark .le. nprf ) then
      cut off zero quality (nump) = 0.5 * (rfnpfd(imark) 
     &+ rfnpfd(imark - 1))
      else
      cut off zero quality (nump) = rfnpfd(nprf)
      endif
      endif
*     write(01,2376)cut off zero quality (nump),flnamesmall(4,nonpd)
 2376 format('Cut off zero for discharge load =',f12.6,1x,a64)

*     compute mean and standard deviation --------------------------------------
      ecm = 0.0
      ecs = 0.0
      do i = 1, nprf
      ecm = ecm + rfnpvl (i)
      ecs = ecs + rfnpvl (i) * rfnpvl (i)
      enddo

      ecs=(ecs-ecm*ecm/nprf)/(nprf-1)

      if (ecs .lt. 1.0E-10) then
      ecs = 0.0
      else
      ecs = SQRoot(1067,ecs)
      endif

      ecm = ecm / float (nprf)

*     set mean (as indicator that valid data have been read) -------------------
      pollution data (IE, NUMP, 1) = ecm

*     set standard deviation (as indicator that valid data have been read) -----
      pollution data (IE, NUMP, 2) = ecs

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

      pollution data (IE, NUMP, MO) = X(MO)

*     ready for the next record (discharge flow and quality) -------------------
      KPAR = KPAR+1
      if ( KPAR .gt. MPAR ) KE = -9
      return

 7566 write( *,7168)flnamesmall(4,nonpd)
      write(01,7168)flnamesmall(4,nonpd)
      write(09,7168)flnamesmall(4,nonpd)
      write(33,7168)flnamesmall(4,nonpd)
 7168 format(77('-')/
     &'*** Error in non-parametric discharge load data ...'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8316 write( *,8317)IE
      write(01,8317)IE
      write(09,8317)IE
      write(33,8317)IE
 8317 format(77('-')/'*** Error in effluent load data-sets ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end






*     read the monthly data on discharge quality ------------------------ type 5
      subroutine get monthly file for discharge quality ! --------------- type 5
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets --------------------------------
      SEASD = SEASD + 1

*     check there is no violation of SIMCAT's storage --------------------------
      if ( SEASD .gt. M8 ) then
      write( *,9262)M8
      write(01,9262)M8
      write(09,9262)M8
      write(33,9262)M8
 9262 format(77('-')/'*** Too many data-sets are monthly ...'/
     &'*** Maximum (M8) is',I6/
     &'*** Maximum exceeded whilst reading discharge quality ...'/
     &77('-'))
      endif

*     read the line of monthly data for discharge quality ----------------------
      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1312, END = 1312) IE,NUMP, 
     &PDEC(IE,NUMP),flmonth(4,seasD),X(MO),JNUM
      goto 2314
 1312 continue
      read(line of data,*,ERR = 7566, END = 7566 ) IE,NUMP, 
     &PDEC(IE,nump),flmonth(4,seasD),X(MO)
      JNUM = 0
 2314 continue

      PNUM(IE,NUMP) = JNUM

*     truncate number of samples to 4 (for confidence limits)
      PNUM(IE,NUMP) = MAX0( 4,PNUM(IE,NUMP))

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( PNUM(IE,NUMP) .gt. 500 ) then
      PNUM (IE,NUMP) = 12
      endif

      pollution data(IE,NUMP,1) = 0.0
      pollution data(IE,NUMP,2) = 0.0

*     store the dataset number -------------------------------------------------
      ISEASP (2,SEASD,NUMP+1) = IE

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

      pollution data (IE, NUMP, MO) = X(MO)

*     add the folder name to the file name
      call add folder for monthly data file (4)

*     check whether the datafile exists
      inquire( FILE = flmonth(4,SEASD), EXIST = exists )

      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(flmonth(4,SEASD),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(flmonth(4,SEASD),3477)(a(i),i=1,136) !========  search the NPD folder
      inquire( FILE = flmonth(4,SEASD), EXIST = exists )

      if ( .NOT. exists) then      
      call change colour of text (20) ! bright red
      write( *,7163) FLMONTHsmall(4,SEASD)
      call set screen text colour
      write(01,7163) FLMONTHsmall(4,SEASD)
      write(09,7163) FLMONTHsmall(4,SEASD)
      write(33,7163) FLMONTHsmall(4,SEASD)
 7163 format(77('-')/'*** Error in monthly effluent quality data ...'/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      

 
*     open the file ------------------------------------------------------------
      open(11,FILE = flmonth(4,SEASD), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (4,seasd) ! discharge quality - type 5

      xtemp1 = 0.0
      xtemp2 = 0.0
      do i = 1, 12
      xtemp1 = xtemp1 + seas1(i)
      xtemp2 = xtemp2 + seas2(i) 
      enddo

      pollution data (IE,NUMP,2) = xtemp2 / 12.0
      pollution data (IE,NUMP,1) = xtemp1 / 12.0
      return

 7566 write( *,7168) FLMONTHsmall(4,seasd)
      write(01,7168) FLMONTHsmall(4,seasd)
      write(09,7168) FLMONTHsmall(4,seasd)
      write(33,7168) FLMONTHsmall(4,seasd)
 7168 format(77('-')/'*** Error in effluent quality data ...'/
     &'*** Error in reading monthly datafile ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 8313 write( *,8314) IE
      write(01,8314) IE
      write(09,8314) IE
      write(33,8314) IE
 8314 format(77('-')/
     &'*** Error in reading monthly effluent quality ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end





*     read the monthly structure data on discharge quality -------------- type 8
      subroutine get monthly structure file for discharge quality ! ----- type 8
      include 'COMMON DATA.FOR'
      logical exists
      character *1 a(136),b(136)

*     add 1 to the counter of monthly data-sets ------------------------------------
      struckd = struckd + 1 ! for discharge quality

*     check there is no violation of SIMCAT's storage ------------------------------
      if ( struckd .gt. M9 ) then
      write( *,9262) struckd,M9
      write(01,9262) struckd,M9
      write(09,9262) struckd,M9
      write(33,9262) struckd,M9
 9262 format(77('-')/'*** Too many data-sets for monthly ',
     &'structure ... '/
     &'*** Maximum (M9) is',I6/
     &'*** Maximum exceeded whilst reading discharge quality ...'/
     &77('-'))
      endif

*     read the line of monthly structure
      read(02,4433)line of data
 4433 format(a200) 
      read(line of data,*,ERR = 1312) IE, NUMP, PDEC(IE,NUMP),
     &FLSTRUCT(4,struckd),X(MO),JNUM
      goto 2314
 1312 read(line of data,*,ERR = 7566) IE, NUMP, PDEC(IE,nump),
     &FLSTRUCT(4,struckd),X(MO)
      JNUM = 0

 2314 continue
      PNUM(IE,NUMP) = JNUM

*     truncate number of samples to 4 (for confidence limits) ------------------
      PNUM(IE,NUMP) = MAX0( 4,PNUM(IE,NUMP))

*     overwrite high numbers (for confidence limits) ---------------------------
      if ( PNUM(IE,NUMP) .gt. 500 ) then
      PNUM (IE,NUMP) = 12
      endif

      pollution data (IE,NUMP,1) = 0.0
      pollution data (IE,NUMP,2) = 0.0

*     store the dataset number -------------------------------------------------
      istruct (2,struckd,NUMP+1) = IE

*     check and correct default correlation ------------------------------------
      if ( X(MO) .gt. 1.0 .or. X(MO) .lt. -1.0 ) then
      X(MO) = -9.9
      endif

      pollution data (IE, NUMP, MO) = X(MO)

*     add the folder name to the file name -------------------------------------
      call add folder for monthly structure file (4)

*     check whether the datafile exists ----------------------------------------
      inquire( FILE = FLSTRUCT(4,struckd), EXIST = exists )
      
      if ( .NOT. exists) then ! ========================= look in the NPD folder
      read(FLSTRUCT(4,struckd),3477)(a(i),i=1,136)
 3477 format(136a1)
      do i = 1, 136
      b(i) = a(i)
      enddo
      
      jend = 0
      jslash = 0
      do i = 1,136
      j = 137 - i
      if ( jend .eq. 0 .and. a(j).ne.' ' ) jend = j
      if ( a(j) .eq. '\' ) then
      jslash = j 
      goto 3478
      endif
      enddo
 3478 continue
      a ( jslash + 1 ) =  'n' ! ============================= add the NPD folder
      a ( jslash + 2 ) =  'p' ! ============================= add the NPD folder
      a ( jslash + 3 ) =  'd' ! ============================= add the NPD folder
      a ( jslash + 4 ) =  '\' ! ============================= add the NPD folder       
      do i = 1,jend
      a ( jslash + i + 4 ) =  b ( jslash + i ) 
      enddo
      
      write(FLSTRUCT(4,struckd),3477)(a(i),i=1,136) !====  search the NPD folder
      inquire( FILE = FLSTRUCT(4,struckd), EXIST = exists )

      if ( .NOT. exists) then      
      call change colour of text (20) ! bright red
      write( *,7163) FLSTRUCTsmall(4,struckd)
      call set screen text colour
      write(01,7163) FLSTRUCTsmall(4,struckd)
      write(09,7163) FLSTRUCTsmall(4,struckd)
      write(33,7163) FLSTRUCTsmall(4,struckd)
 7163 format(77('-')/
     &'*** Error in monthly structure of effluent quality data ... '/
     &'*** The data file does not exist ... ',a64/77('-'))
      call stop
      endif ! ======================================= file not in the NPD folder
      endif ! ==================================================================
      
      

*     open the file containing the monthly data ------------------------- type 8
      open(11,FILE = FLSTRUCT(4,struckd), STATUS='OLD')

*     read the file containing the monthly data ------------------------- type 8
      call read monthly structure ! effluent quality -------------------- type 8
     &(4,0,struckd,tmean,tstdev,t3,tcorr,itest12,1)
      close (11)

*     check need to revert to annual data --------------------------------------
      if ( itest12 .eq. 12 ) then
      PDEC(IE,NUMP) = 2 ! lognormal distribution -------------------------------
      pollution data (IE,NUMP,1) = tmean
      pollution data (IE,NUMP,2) = tstdev
      pollution data (IE,NUMP,3) = t3
      if ( tcorr .gt. 1.0 .or. tcorr .lt. -1.0 ) then
      tcorr = -9.9
      endif
      pollution data (IE, NUMP, 4) = tcorr
      return
      endif

      xtemp1 = 0.0
      xtemp2 = 0.0
      do i = 1, 12
      xtemp1 = xtemp1 + struct1(i)
      xtemp2 = xtemp2 + struct2(i) 
      enddo

      pollution data (IE,NUMP,2) = xtemp2 / 12.0
      pollution data (IE,NUMP,1) = xtemp1 / 12.0
      return

 7566 write( *,7168) FLSTRUCTsmall(4,struckd)
      write(01,7168) FLSTRUCTsmall(4,struckd)
      write(09,7168) FLSTRUCTsmall(4,struckd)
      write(33,7168) FLSTRUCTsmall(4,struckd)
 7168 format(77('-')/'*** Error in effluent quality data ...'/
     &'*** Error in reading monthly structure data ...'/
     &'*** Check the file ... ',a64/77('-'))
      call stop

 8313 write( *,8314)IE
      write(01,8314)IE
      write(09,8314)IE
      write(33,8314)IE
 8314 format(77('-')/
     &'*** Error in reading monthly effluent quality ...'/
     &'*** The data-file has been assembled incorrectly ... '/
     &'*** Check dataset number',i6/77('-'))
      call stop
      end