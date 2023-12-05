*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: commence the calculation.for ... 5079 lines ------------------------
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC ...
*     ==========================================================================
*     Version 161  (15/11/23) --------------------------------------------------
*     ==========================================================================
*     Compiling with Intel(R) Visual Fortran Compiler 17.0.1.143 [IA-32]........  
*     ==========================================================================
*     The entire SIMCAT contains 482 Sub-routines and 53 Functions in 28 files -
*     It also has 72000 lines of code.  These include lines of 11550 comments --
*     ==========================================================================
*     The files are ".FOR" items.  There names are:
*     --------------------------------------------------------------------------
*     commence the calculations
*     read data on flow and quality
*     read the data on features
*     process each reach in turn
*     process the selected reach
*     process a feature
*     diffuse features     
*     mass balance
*     transformations
*     meet the river targets
*     assess compliance 
*     apportion pollution
*     bifurcation
*     common data
*     compute summary statistics
*     data dictionary
*     generate data
*     generate mass balance data
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     gap fill creation
*     gap fill use
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     monthly data
*     random numbers for Monte Carlo
*     set up data
*     set up files
*     switch options
*     change colours
*     ==========================================================================
*     --------------------------------------------------------------------------
*     This file starts the calculations - it reads and checks the data ---------
*     --------------------------------------------------------------------------
*     This file contains 34 sub-routines ---------------------------------------
*     They are called: ---------------------------------------------------------
*     --------------------------------------------------------------------------
*     ...... generate sequence of reaches
*     ...... generate next reach
*     ...... write reach targets
*     ...... initial track
*     ...... close down all the output files
*     ...... create extra data files
*     ...... write final messages for model run
*     ...... set screen text colour
*     ...... check the number of shots is not too big
*     ...... check the number of shots for monthly data
*     ...... set indices for percentiles etc
*     ...... read temperature data
*     ...... sort out determinands
*     ...... write file and screen headings
*     ...... write batch heading
*     ...... set the constraints on discharge quality
*     ...... read the reaches
*     ...... write GIS descriptors ! for the outout file used by SAGIS 
*     ...... check for a reduced run
*     ...... read the data on the determinands 
*     ...... set the gap filling switches
*     ...... sort out data for length profiles
*     ...... initialise for the run
*     ...... close down
*     ...... pause
*     ...... stop
*     ...... write headings for mode 7
*     ...... write headings for mode 8
*     ...... write headings for mode 9
*     ...... check summary statistic for standard (ldet)
*     ...... summarise errors
*     ...... write headings for the effluent CSV file
*     ...... create_directory (newDirPath)
*     --------------------------------------------------------------------------

*     --------------------------------------------------------------------------
*     This file contains and calls the following subroutines -------------------
*     --------------------------------------------------------------------------
*     .... initialise for the run **********************************************
*     .... read the data on the determinands ***********************************
*     .... read the reaches ****************************************************
*     .... read river flows ****************************************************
*     .... read data on river quality (MV) *************************************
*     .... river quality correlation *******************************************
*     .... read effluent data **************************************************
*     .... read river targets **************************************************
*     .... read data on the features *******************************************
*     .... start the main calculations ! start the main calculations ***********     
*     .... generate sequence of reaches ****************************************
*     .... generate next reach *************************************************
*     --------------------------------------------------------------------------
*     .... It also uses subtoutines in other files -----------------------------
*     --------------------------------------------------------------------------
*     .... initial track
*     .... switches and data ! set the values of the switches 
*     .... attach SIMCAT data file for catchment
*     .... check ical is legal etc
*     .... attach files and sort out gap filling
*     .... read temperature data
*     .... initialise variables
*     .... reset random number starters
*     .... create extra data files ! create data files for GIS output, etc ----
*     .... sort out determinands
*     .... set up default random normal deviates
*     .... write file and screen headings
*     .... check summary statistic for standard (idet)
*     .... set the constraints on discharge quality
*     .... check for a reduced run
*     .... write GIS descriptors ! to the file used by SAGIS  ------------------
*     .... deal with non standard correlation for effluent flow
*     .... deal with non standard correlation for effluent quality
*     .... write river targets
*     .... write reach targets
*     .... write the features
*     .... write river flows
*     .... write data on river quality (MV)
*     .... write effluent data
*     .... set the gap filling switches
*     .... sort out data for length profiles ! needed for plotting -------------
*     .... write final messages for model run
*     .... proportion of effluent at model end 
*     .... summarise errors
*     .... close down ! all model runs are finished ----------------------------
*     .... set loads and proportions to zero
*     .... write headings for the effluent CSV file
*     .... set indices for percentiles etc ! ------ check shots for monthly data 
*     .... lose folder ! eliminate the name of a folder from a filename 
*     .... check first four characters of a line of data (IFIN)
*     .... set determinand codes (ndet)
*     --------------------------------------------------------------------------
      
*     --------------------------------------------------------------------------
*     the names of variables are set out in the file called "data dictionary"
*     --------------------------------------------------------------------------
*     the names of variables used to set dimensions of arrays:
*     values are set in common.for
*     --------------------------------------------------------------------------
*     MR = maximum number of reaches -----------------------------------
*     NU = maxumum number of features ----------------------------------
*     NF = maximum number of sets of flow data for rivers --------------
*     NV = maximum number of sets of river quality ---------------------
*     NE = maximum number of sets of data for effluents ----------------
*     MS = number of Monte-Carlo shots ---------------------------------
*     KR = number of reach data-sets kept in store ---------------------
*     NM = number of summary statistics for distributions --------------
*     MP10 = number of pollutants --------------------------------------
*     NQ = number of data sets on river quality targets ----------------
*     MM = number of quality monitoring stations -----------------------
*     NUED = number of discharges for back-tracking --------------------
*     NUW = maximum number of sub-catchments ---------------------------
*     M7 = data-sets for non-parametric distributions ------------------
*     MO = statistics entered as flow and quality data -----------------
*     M8 = number of data-sets for monthly data ------------------------
*     M9 = number of data-sets for monthly structure data --------------
*     M10 = number of data-sets for temperature data -------------------
*     NB = maximum number of bifurcations ------------------------------
*     MT = size of utility array ( Maximum of NE, etc) -----------------
*     MG = maximum number of flow gauges -------------------------------
*     MPRF = maximum data for non-parametric distribution --------------
*     NC = maximum classes in the classification system ----------------
*     NPROP number of types of feature tracked for inputs --------------
*     N2PROP and that adds totals of diffuse features and fish farms ---
      
*     ==========================================================================
*     some of the input and output channels ------------------------------------
*     ==========================================================================
*     ..01   main and detailed output:                              filename.OUT
*     ..02   main data file:                                                .DAT
*     ..03   report on gaps betweeen onserved and calculated                .CAL
*     ..08   input data:                                                    .INP
*     ..09   copy of screen output:                                         .SCN
*     ..22   results for graph plotting (VISUAL BASIC):                     .SGR
*     ..27   results on loads:                                              .LOD
*     ..30   results on compliance with target (Batch runs):                .TGT 
*     ..33   record of possible errors found:                               .ERR 
*     ..36   effluent data (CSV file)                                    EFF.CSV
*     ..42   output for GIS:                                            GIS1.CSV
*     ..48   output on classification:                                      .WFD
*     ..72   output on river flows:                                         .FLO
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     ..74   working file for flow gap filling:                             .FCL
*     ..75   working file for quality gap filling:                          .QCL
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     ..23   RUN161.TMP (names of files passed to SIMCAT)                   .TMP
*     ==========================================================================
*     101-110   output for months                                         Di.MON
*     131-140   output for effluents                                      Ei.CSV
*     111-120   output for GIS                                            Di.CSV
*     --------------------------------------------------------------------------
*     181-190   apportionment of load and concentration    ... 161 ...   DAi.CSV
*               including apportionment between works      ... 161 ...   DAi.CSV         
*     121-130   apportionment of load and concentration                   Di.ADL
*               including apportionment between works                     Di.ADL         
*     --------------------------------------------------------------------------
*     141-150   output on apportionment to types of pollution ... 161 ... Di.ADC
*               percentile apportionment                      ... 161 ... Di.ADC
*     --------------------------------------------------------------------------
*     221-230   apportionment between water bodies                        Di.ACL
*     261-270   apportionment between water bodies                       WBi.CSV
*     --------------------------------------------------------------------------
*     161-170   apportionment of river loads from discharges              Pi.CSV
*     191-200   apportionment of river concentrations from discharges     Qi.CSV
*     171-180   assistance with calibration                               Ci.GAP
*     231-240   data for graph plotting                                   Gi.CSV
*     241-250   data for use in writing reports in WORD                   Wi.CSV
*     ==========================================================================

      program SIMCAT ! #########################################################
      include 'COMMON DATA.FOR' ! insert the common block   
      logical result
      character *01 a(10)
      
*     controls for next version ================================================ 
      correctcorrel = 0 ! correct correlation for impacts of monthly data ######  
      
      call initialise for the run
      open(23,file='RUN161.TMP')
      read(23,*)Line of data
*     if "ifbatch" stays at 1 then this is a batch run -------------------------
      if ( Line of data .eq. 'SINGLE' ) ifbatch = 0
*     initialise the starting values for tracking compliance with targets in ---
*     batch mode ... and initialise total loads and total flows ----------------
      call initial track
      
      if ( ifbatch .eq. 0 ) then
      call change colour of text (11) ! bright turquoise blue
      write( *,9045)
 9045 format(//4x,'STARTING 161'///20x,'STARTING 161'///
     &36x,'STARTING 161'///52x,'STARTING 161'///68x,'STARTING 161'///
     &84x,'STARTING 161'///100x,'STARTING 161'///)     
      call set screen text colour
      endif
      
      call switches and data ! set the values of the switches ------------------
      read(23,230,END=5501) folder
      call attach simcat data file for catchment
      backspace 23
      backspace 23
      call use replacement switches

      if ( ifbatch .eq. 1 ) then ! this is a batch run bbbbbbbbbbbbbbbbbbbbbbbbb
      call write batch heading
      if ( master set used .eq. 1 ) then ! -------------------------------------
      call change colour of text (49) ! dull blue
      krite1 = 1
      datfilename = datname
      call lose folder ! eliminate the name of a folder from a filename 
      write( *,1110) datfilename
      write(09,1110) datfilename
      write(01,1110) datfilename
      write(33,1110) datfilename
 1110 format('The first DAT file ',
     &'is used to guide the rest       ...'6x,a40)
      endif ! if ( master set used .eq. 1 ) ------------------------------------
      endif ! if ( ifbatch .eq. 1 ) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      if ( kswitch .eq. 1 ) then
      call change colour of text (22) ! light blue
      write( *,7963)'Switches.GPD' 
 7963 Format('Used the supporting file on data switches',10x,
     &'... ',6x,a12)
      call set screen text colour
      endif ! if ( kswitch .eq. 1 ) 

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     start the run of the model ----------------------------------------------- 
*     or in batch mode ... start the next model (catchment) run in the batch --- 
*     of models .. and come back to this point to start the next model run in -- 
*     the batch sequence -------------------------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 5500 continue

*     increase the counter by one (of the number of models (catchments) in this 
*     (batch run) --------------------------------------------------------------
      model number in batch = model number in batch  +  1
*     initialise variable for directing the cycle of runs (through gap filling)-
*     for the next model (catchment) in the batch run --------------------------
      jcycle = 0
      
*     some of the data for the first model will apply to all the other models in 
*     the batch run identify this master file ... the first one read in --------
      masterdata = 0
      if ( ifbatch .eq. 1 .and. model number in batch .eq. 1 ) then
      if ( master set used .eq. 1 ) masterdata = 1
      endif

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     start the cycle (sequence) of runs for this model in the batch ...........
*     and loop through the gap filling options where these were requested ......
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     read in the name of the folder holding the data files --------------------
      read(23,230,END=5501) folder
  230 format(a100)
      call attach simcat data file for catchment
      
      call add folder for output 0 ! produced by SIMCAT -------------------- 161
      
      result = makedirqq(output_folder) ! create the directory ------------- 161

*     --------------------------------------------------------------------------
*     prepare to read the value of ICAL ----------------------------------------
*     this dictates the form of the calculation done for this data-file --------
*     --------------------------------------------------------------------------
*     === 0 ... mass balance of input data...

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     === 1 ... compute changes to river flows giving exact fit...
*     === 2 ... apply computed changes to flows (from ICAL=1)...
*               extrapolating and interpolating...
*     === 3 ... as 2 but also computing the changes in quality
*               needed fo give a perfect fit...
*     === 4 ... apply computed changes to flow (ICAL=1) and
*               computed changes to quality (from ICAL=3)...
*               extrapolating and interpolating...
*               Standard run for "What-if" simulations...
*               or "trial-and-error' assessments of options to change
*               river quality...
*     === 5 ... run 1 followed by run 3 ...
*     === 6 ... run 1 followed by runs 3 and 4 ...
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

*     === 7 ... compute effluent standards needed to meet river targets...
*     === 8 ... as 7 but with more emphasis on no deterioration ...
*     === 9 ... as 8 but using mid-point of target class as upstream quality ---
*     --------------------------------------------------------------------------
      read(23,24,ERR=7199) ical ! read the value of ICAL from -------------- TMP
   24 format(i3)
      
      call check ical is legal etc
      
      if ( ical .eq. 3 ) JSKIP = 1

      if ( ifbatch .eq. 1 ) call read the global data file
      
      kcycle = ICAL
*     suppress gap filling in modes 7, 8 and 9 ---------------------------------
      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then
      if ( kcycle .eq. 6 ) kcycle = 4
      endif ! ------------------------------------------------------------------
      
      call attach files and sort out gap filling

      if ( kswitch .eq. 1 ) then
      write(09,8963)'Switches.GPD',exclude BOD ! --------------------------- SCN
      write(01,8963)'Switches.GPD',exclude BOD ! --------------------------- OUT 
      write(33,8963)'Switches.GPD',exclude BOD ! --------------------------- ERR
 8963 Format(77('-')/'Used the supporting file on data switches',
     &10x,'... ',6x,a12,i4)
      endif ! if ( kswitch .eq. 1 ) 

*     perform the next cycle in the sequence of runs for this particular DAT ---
*     (re-)read the title for this model (catchment) from the DAT file ---------
 6009 call check first four characters of a line of data (IFIN)
      read(02,9913) TITLE
 9913 format(A70)
      call leadblanks ! eliminate leading blanks from a run title
      call check first four characters of a line of data (IFIN)


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     prepare for next (gap filling) cycle for this model ----------------------
*     the value of "jcycle" was set to at zero ---------------------------------
      jcycle = jcycle + 1
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      !kerror = 0
      kj = kcycle * jcycle 
      if ( kj .eq. 18 .or. kj .eq. 10 ) kerror = 1
      if ( kj .eq. 0 ) kerror = 1
      endif ! batch run
*     check whether consecutive cycles have been requested ---------------------
*     these cover the sequences of gap filling through flow and quality --------
      if ( kcycle .eq. 5 .or. kcycle .eq. 6) then
      if ( jcycle .eq. 1 ) ICAL = 1
      if ( jcycle .eq. 2 ) ICAL = 3
      if ( jcycle .eq. 3 ) ICAL = 4
      endif
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      
      call set screen text colour

*     variables no longer used --------------------------------------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) munthly structure ! switch: impose monthly structure
      if ( munthly structure .gt. 1 ) munthly structure = 2 ! ---- from DAT file
*     value of 1 imposes monthly structure on annual flow data -----------------
*     reset to 0 if the DAT file contains no sets of sata of type 5 or 8 -------
*     number of shots set to a multiple of 365 if value = 1 --------------------
      
      call check first four characters of a line of data (IFIN)
      
      classfraction = 0.50 ! % of target class set as u/s quality for Run Type 9
      read(02,*,ERR = 7500) icf ! % of target class set for Run Type 9 ---------
      if ( icf .gt. 0 .and. icf .lt. 101 ) then
      classfraction = 0.01 * icf ! % of target class set as u/s quality --------
      endif

*     variable to suppress output ----------------------------------------------
*     set to 0 to retain all the output ----------------------------------------
*     set to 1 to exclude output on non-effluent features ----------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) IPRINT
      
*     switch for mean or percentile modes --------------------------------------
*        0 - 95-percentile output to screen
*        1 - mean output to screen
*        2 - 90-percentile output to screen and graphs
*        3 - 99-percentile output to screen and graphs
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) output mode
      if ( output mode .gt. 3 ) output mode = 1

      ical13 = 1 ! calibration mode --------------------------------------------
      if ( ical .ne. 1 .and. ical .ne. 3 ) then
      ical13 = 0 ! not calibration mode ----------------------------------------
      write(22,*) TITLE ! write to the SGR file ---------------------------- SGR
      write(22,*) output mode ! write to the SGR file ---------------------- SGR
      endif
      
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) NS ! number of shots -------------------------------
      call check the number of shots is not too big ! for SIMCAT ---------------
      call check first four characters of a line of data (IFIN)
      call read temperature data
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) FUNIT ! read the units for flow --------------------
      
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) IDIFFREACH ! read switch for reach diffuse flow  ---
      IDIFFREACH = MIN(1,MAX(0,IDIFFREACH))
     
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) IPUR ! read the switch for natural purification ----
      IPUR = MIN(1,MAX(0,IPUR))
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500) KINT ! switch for adding interpolation -------------
      KINT = MAX(0,KINT) ! switch for adding interpolation ---------------------

      call initialise variables
      call reset random number starters
      call read the data on the determinands 

      call create extra data files ! create data files for GIS output, etc ----
      call sort out determinands
      call set up default random normal deviates
      
      call write file and screen headings
      
      do idet = 1, ndet
      call check summary statistic for standard (idet)
      if ( masterdata .eq. 1 ) then
      masterMRQS(idet) = MRQS(idet)
      endif
      enddo ! do idet = 1, ndet
     
      call set the constraints on discharge quality
      call read the reaches
      call check for a reduced run
      call write GIS descriptors ! to the file used by SAGIS  ------------------
      call read river flows
      call read data on river quality (MV)
      call river quality correlation
      call read effluent data
      call deal with non standard correlation for effluent flow
      call deal with non standard correlation for effluent quality

      if ( munthly structure .eq. 1 ) then !  - then check monthly data provided
      if ( struckd .eq. 0 .and. seasd .eq. 0 ) then ! - no monthly data provided
      write(01,3602) ! .OUT file
      write(09,3602) ! .SCN file
      write(33,3602) ! .ERR file
 3602 format(77('-'))
      write(01,3666) ! .OUT file
      write(09,3666) ! .SCN file
      write(33,3666) ! .ERR file
      write(08,3666) ! .INP file
      write(01,3602) ! .OUT file
      write(09,3602) ! .SCN file
      write(33,3602) ! .ERR file
      write(08,3602) ! .INP file
      do idet = 1, ndet    
      if ( qtype(idet) .ne. 4 ) then
      write(100+idet,3666)
      write(100+idet,3602)
      endif
      enddo
 3666 format(
     &'*** Your SIMCAT data is set up to expect monthly structured ',
     &'data        '/
     &'*** No such data have been specified',15x,'...'/
     &'*** Monthly structure will still be imposed on annual',
     &' flow data    '/
     &'*** Change the setting to zero to suppress this',4x,'...')
      call set screen text colour
      write( *,3602)
      call change colour of text (14) ! bright yellow
      write( *,3676)
 3676 format(
     &'*** Your data expects monthly sets',17x,'...'/
     &'*** None have been specified      ',17x,'...'/
     &'*** Monthly structure will be set on annual',
     &' flows  ...'/
     &'*** Change the setting to zero to suppress this',4x,'...')
      call set screen text colour
      write( *,3602)
      endif ! if ( struckd .eq. 0 .and. seasd .eq. 0 ) -------------------------
      endif ! if ( munthly structure .eq. 1 ) ----------------------------------
      
      call read river targets
      call write river targets
      call write reach targets
      call read the data on features
      call write the features
      call write river flows
      call write data on river quality (MV)
      call write effluent data
      call set the gap filling switches
      call sort out data for length profiles ! needed for plotting -------------

      call start the main calculations ! start the main calculations -----------     


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     test for repeat runs on this particular model (data file) along the ------
*     gap filling sequence for flow and quality --------------------------------
*     that is where ICAL was set to 5 (1 and 3) or 6 (1, 3 and 6 ) -------------
*     then go to 9009 if this sequence is finished for this model ... ----------

*     test for a one-cycle option ( where "kcycle" does not equal to 5 or 6 ) --
      if ( kcycle .ne. 5 .and. kcycle .ne. 6 ) goto 9009

*     continue with the other options (ical was set to 5 or 6) -----------------
      if ( jcycle .eq. 0 ) goto 9009 ! one pass - no gap filling ---------------
      if ( kcycle .eq. 5 .and. jcycle .eq. 2 ) goto 9009 ! two cycles ----------
      if ( kcycle .eq. 6 .and. jcycle .eq. 3 ) goto 9009 ! three cycles --------

*     continue to the next cycle ... rewind the DAT file -----------------------
      rewind 02 ! rewind the DAT file ------------------------------------------
      goto 6009
*     end of the cycle in the sequence for this particular DAT file ------------
 9009 continue
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      
*     dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
*     write output on the load and concentration in the river provided by dddddd
*     individual discharges dddddddddddddddddddddddddddddddddddddddddddddddddddd
      iwerks = 0    
      do iworks = 1, kountworks ! +++++++++++++++++++++++++++++++++++++++++++++
      jworks = identify werks (iworks)
      if ( JT(jworks) .eq. 03 .or. JT(jworks) .eq. 05 .or. 
     &     JT(jworks) .eq. 39 .or. JT(jworks) .eq. 60 .or.
     &     JT(jworks) .eq. 61 ) then ! =========================================
      iwerks = iwerks + 1
      identify werks (iwerks) = identify werks (iworks)
      endif ! if ( JT (jworks) .eq. 03 or 05 etc ===============================
      enddo ! do iworks = 1, kountworks ++++++++++++++++++++++++++++++++++++++++
                
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     CSV output on apportionment of load from discharges ++++++++++ Pi & Qi.CSV
      do idet = 1, ndet ! loop on determinands +++++++++++++++++++++ Pi & Qi.CSV
      if ( qtype (idet) .ne. 4 .and. ical .ne. 1 ) then ! ++++++++++ Pi & Qi.CSV
          
*     write details of the headings for concentrations +++++++++++++++++++++++++
*     CSV output on concentrations from discharges (Qi.CSV) +++++++++++++ Qi.CSV
      write(190+idet,6822)(uname(identify werks(ikk)), ! ---------------- Qi.CSV
     &ikk=1,kountworks35), ! -------------------------------------------- Qi.CSV
     &'% river conc from Type 3 sources ', ! ---------------------------- Qi.CSV
     &'% river conc from Type 5 sources ', ! ---------------------------- Qi.CSV
     &'% river conc from Type 39 sources', ! ---------------------------- Qi.CSV
     &'% river conc from Type 60 sources', ! ---------------------------- Qi.CSV
     &'% river conc from Type 61 sources', ! ---------------------------- Qi.CSV
     &'total % from all these sources   ', ! ---------------------------- Qi.CSV
     &'% river conc from Type 12 sources', ! ---------------------------- Qi.CSV
     &'% river conc from Type 62 sources', ! ---------------------------- Qi.CSV
     &'Code for SIMCAT 161              '  ! ---------------------------- Qi.CSV
 6822 format('Reference,','Reach,','Distance,', ! ----------------------- Qi.CSV
     & 'Location,','GIS Code,','Mean river concentration,', ! ----------- Qi.CSV
     &4000(a33,',')) ! -------------------------------------------------- Qi.CSV
      write(190+idet,6829)(JT(identify werks(ikk)), ! ------------------- Qi.CSV
     &ikk=1,kountworks35),3,5,39,60,61 ! -------------------------------- Qi.CSV
      write(190+idet,6827)('% of mean river concentration', ! ----------- Qi.CSV
     &ikk=1,kountworks35+8) ! ------------------------------------------- Qi.CSV
 6827 format(',,,,','Data:,','Mean river concentration,' ! -------------- Qi.CSV
     &4000(a29,',')) ! -------------------------------------------------- Qi.CSV 
            
*     write details of the headings for loads ++++++++++++++++++++++++++++++++++
*     CSV output on loads from discharges (Pi.CSV) ++++++++++++++++++++++ Pi.CSV
      write(160+idet,6821)(uname(identify werks(ikk)), ! ---------------- Pi.CSV
     &ikk=1,kountworks35), ! -------------------------------------------- Pi.CSV
     &'% river load from Type 3 sources ', ! ---------------------------- Pi.CSV
     &'% river load from Type 5 sources ', ! ---------------------------- Pi.CSV
     &'% river load from Type 39 sources', ! ---------------------------- Pi.CSV
     &'% river load from Type 60 sources', ! ---------------------------- Pi.CSV
     &'% river load from Type 61 sources', ! ---------------------------- Pi.CSV
     &'total % from all these sources   ', ! ---------------------------- Pi.CSV
     &'% river load from Type 12 sources', ! ---------------------------- Pi.CSV
     &'% river load from Type 62 sources', ! ---------------------------- Pi.CSV
     &'from---- SIMCAT 161              '  ! ---------------------------- Pi.CSV
 6821 format('Reference,','Reach,','Distance,', ! ----------------------- Pi.CSV
     & 'Location,','GIS Code,','Mean river load,', ! -------------------- Pi.CSV
     &4000(a33,',')) ! -------------------------------------------------- Pi.CSV
      write(160+idet,6829)(JT(identify werks(ikk)), ! ------------------- Pi.CSV
     &ikk=1,kountworks35),3,5,39,60,61 ! -------------------------------- Pi.CSV
 6829 format(',,,,,','Source Type,',4000(i6,',')) ! --------------------- Pi.CSV 
      write(160+idet,6828)('% of mean river load', ! -------------------- Pi.CSV
     &ikk=1,kountworks35+8) ! ------------------------------------------- Pi.CSV
 6828 format(',,,,','Data:,','Mean river load,',4000(a20,',')) ! -------- Pi.CSV 
      
      endif ! if ( qtype (idet) .ne. 4 ) =========================== Pi & Qi.CSV
      enddo ! do idet = 1, ndet ==================================== Pi & Qi.CSV 
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Pi.CSV
*     apportion of river load to individual discharges ------------------ Pi.CSV
*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Pi.CSV

      
      do J = 1, ndet ! ================================================== Di.CSV
      if ( qtype(J) .ne. 4 ) then ! ===================================== Di.CSV
      write(110+J,6228) dname(J),units(J),lunits(J) ! output file ------- Di.CSV
 6228 format(/a11,',',a4,',',a4)
      write(110+J,6696) ! output file  ---------------------------------- Di.CSV
 6696 format('$Headings'/
     &'The following items can occur in columns ...'/
     &'  A: GIS code for the location'/
     &'  B: location (eg the feature)'/
     &'  C: name of the reach'/
     &'  D: description of the row of data'/ 
     &'  E: type of data'/ 
     &'  F: mean for January,for flow,for quality,for load'/
     &'  G: mean for February'/
     &'  H: mean for March'/
     &'  I: mean for April'/
     &'  J: mean for May'/
     &'  K: mean for June'/
     &'  L: mean for July'/
     &'  M: mean for August'/
     &'  N: mean for September'/
     &'  O: mean for October'/
     &'  P: mean for November'/
     &'  Q: mean for December'/
     &'  R: annual mean'/
     &'  S: feature type at this location,
     &and 111 for start of Reach,or 999 for the end'/
     &'  T: feature type(s) the contribution'/
     &'  U: % contribution to the total discharge load'/
     &'  V: contribution to mean, total discharge load'/ ! ----------------- 161
     &'  W: lower confidence limit for V'/
     &'  X: upper confidence limit for V'/
     &'  Y: % contribution to annual mean, river concentration'/
     &'  Z: contribution to annual mean, river concentration'/
     &' AA: lower confidence limit'/
     &' AB: upper confidence limit'/
     &' AC: length of upstream river') ! ----------------------------------- 161
      endif ! if ( qtype(j) .ne. 0 ) ===========================================
      enddo ! do j = 1, ndet ===================================================


      call write final messages for model run
      if ( ifbatch .eq. 1 ) call proportion of effluent at model end 
      
      if ( ifbatch .eq. 1 .and. need works .gt. NUED ) then
      call change colour of text (12) ! orange
      write( *,4531)NUED,need works,kill works
 4531 format('*** Too many discharges for back-tracking',10x,
     &'...',7x,'the maximum is ',i5,' (need',i5,')',12x,
     &'(ignored',i5,')')
      call set screen text colour
      endif

*     return to seek a new DAT file for the next model in batch mode -----------
          
      isupprezz = suppress1 + suppress3 + suppress4
     &+ suppress5  + suppress6  + suppress7  + suppress8  + suppress9
     &+ suppress10 + suppress11 + suppress12 + suppress13 + suppress14
     &+ suppress16 + suppress17 + suppress18 + suppress19 + suppress20
     &+ suppress21 + suppress22 + suppress00 + suppress9a + suppress9b 
     &+ suppress15


      if ( ifbatch .eq. 1 ) then ! this is a batch run -------------------------
      if ( isupprezz .gt. 0 ) then
      call change colour of text (12) ! orange
      write(valchars10,27)isupprezz
   27 format(i10)
      read(valchars10,28)(a(i),i=1,10)
   28 format(10a1)
      do i = 1,10
      if ( a(i) .eq. ' ' .and. a(i+1) .eq. ' ' ) a(i) = '.'
      enddo
      write( *,26)(a(i),i=2,10),datfilename
   26 Format('*** WARNINGS about data ',9a1,18x,'...',6x,
     &a40,5x,'Check ERR file ...')
      call set screen text colour
      write( *,29)
   29 Format(130('-'))
      else
      if ( ical .eq. 1 .or. ical .eq. 2 ) write( *,29)
      endif ! if ( isupprezz .gt. 0 )

      if ( isupprezz .gt. 0) then ! --------------------------------------------
      write(09,38) isupprezz
      write(01,38) isupprezz
   38 format(24x,'Number of WARNINGS about data:',i13/
     &24x,'Check the .ERR file for details ...'/24x,53('#'))
      call summarise errors
      else ! if ( supprezz .eq. 0) ---------------------------------------------
      write(09,30) isupprezz
      write(01,30) isupprezz
   30 format(24x,'Number of WARNINGS about data:',i13)
      endif ! if ( isupprezz .gt. 0) -------------------------------------------

*     delete the storage of loads =================================== batch runs
      do ireach = 1, NREACH ! ==================================================
      kreach = ReachCode (ireach)
      close (300+kreach,status='delete') ! delete storage for batch runs --- LOD    
      enddo ! do ireach = 1, NREACH ============================================
      result = deldirqq(output_folder) ! - delete the folder holding these files
*     delete the storage of loads =======================]=========== batch runs

      goto 5500

      else ! this is not a batch run
      if ( isupprezz .gt. 0) then ! --------------------------------------------
      write(09,38) isupprezz
      call summarise errors
      endif
      endif ! ! if ( ifbatch .eq. 1 )
      
 5501 continue ! all the model runs are finshed for this batch run -------------

      close (23) ! RUN---.TMP

      call close down ! all model runs are finished ----------------------------

 7500 write( *,7573)
      write(01,7573)
      write(08,7573)
      write(09,7573)
      write(33,7573)
 7573 format(/77('-')/
     &'*** Error in the general data at the head of the '/
     &'SIMCAT datafile ...'/77('-'))
      call stop

 7199 write( *,7973)
      write(01,7973)
      write(08,7973)
      write(09,7973)
      write(33,7973)
 7973 format(/77('-')/'*** Error in reading RUN161.TMP ...'/77('-'))
      call stop

 8318 write( *,8379)
      write(01,8379)
      write(08,8379)
      write(09,8379)
      write(33,8379)
 8379 format(/73('-')/
     &'*** Error in data-sets for intermittent discharges ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &73('-'))
      call stop

 7519 write( *,8389)
      if ( nobigout .le. 0 ) write(01,8389)
      write(09,8389)
      write(33,8389)
 8389 format(/77('-')
     &'*** Error in processing extra rate constants for reaches ...'/
     &73('-'))
      call stop

      end



*     convert the new way of structuring reaches into the old form -------------
      subroutine generate sequence of reaches
      include 'COMMON DATA.FOR'
      integer OrderThis,OrderOther

*     initialise the values setting out the plan for the river reaches ---------
      do ir = 1, nreach
      do kk = 1, 3
      iplan ( ir, kk ) = 0
      enddo
      enddo

      do 1 kk = 0, nreach
      if ( kk .gt. 0 ) then
      k = ReachCode(kk)
      else
      k = 0
      endif
      if ( k .gt. 0 ) then
      NFIN(k,1) = -1
      NFIN(k,2) = -1
      endif
    1 continue

      do kk = 1, nreach
      k = ReachCode(kk)
      nextr = NextReach(k)
      if (nextr .gt. 0) then
      if ( NFIN(nextr,1) .eq. -1 ) then
      NFIN(nextr,1) = k
      else
      if ( NFIN(nextr,2) .eq. -1 ) then
      NFIN(nextr,2) = k
      endif
      endif 
      endif
      enddo

      do 3 j = 1, nreach
      k = ReachCode(j)
      nextr = NextReach(k) ! next reach
      IDONE = 0
      if ( nextr .le. 0 ) goto 5
      if ( NFIN(nextr,1) .ne. -1 .and. NFIN(nextr,2) .ne. -1 ) then
      NumThis = k
      NumOther = NFIN(nextr,1) + NFIN(nextr,2) - k
      do jj = 1, nreach
      if ( ReachCode(jj) .eq. NumThis ) then
      OrderThis = jj
      endif
      if ( ReachCode(jj) .eq. NumOther ) then
      OrderOther = jj
      endif
      enddo
      if ( OrderThis .lt. OrderOther ) then

*     branch to a new reach ----------------------------------------------------
      IPLAN(k,1) = 0        
      IPLAN(k,2) = ReachCode(j+1)
      IPLAN(k,3) = 0
      IDONE = 1
      endif
      endif
      if ( IDONE .eq. 0 ) then
      if ( NFIN(nextr,2) .eq. -1 ) then

*     straight continuation of from one reach to another -----------------------        
      IPLAN(k,1) = NextReach(k)
      IPLAN(k,2) = 0
      IPLAN(k,3) = 0
      do ju = 1, 4
      if ( NextReach(k) .eq. JREACH(ju) ) then
      if ( JT(JU) .eq. 10 .or. JT(JU) .eq. 45 ) then
      IPLAN(k,1) = 0
      IPLAN(k,2) = NextReach(k)
      IPLAN(k,3) = 0
      goto 3
      endif
      endif
      enddo
	
      else

*     mix two reaches ----------------------------------------------------------
      IPLAN(k,1) = NFIN(NextReach(k),1) + NFIN(NextReach(k),2) - k
      IPLAN(k,2) = k
      IPLAN(k,3) = NextReach(k)
      endif	        
      endif
      
    3 continue
    5 continue

      return
      end



      subroutine generate next reach
      include 'COMMON DATA.FOR'

      call sort format 2 (TDEG,TSDEV)
      write(08,11)valchars10,valchars11
   11 format(77('-')/
     &'Global summary statistics entered for temperature ...'/77('-')/
     &'          Annual mean =',a10,
     &'   Standard deviation =',a10/77('-'))

      write(08,2)
    2 format(//77('-')/
     &'Reaches ...'/
     &77('-')/
     &9x,'Name of reach     Sets of data for    Next reach'/
     &9x,'                  diffuse inputs of             '/
     &9x,'                  flow and quality              '/
     &77('-'))

      do jj = 1, nreach
      j = ReachCode(jj)
      IF = RFDIFF (j)
      IQ = RQDIFF (j)

*     straight continuation ----------------------------------------------------
      if ( IPLAN(j,1) .gt. 0 .and. IPLAN(j,2) .eq. 0 
     &                       .and. IPLAN(j,3) .eq. 0 )then
      NextReach(j) = IPLAN(j,1)
      write(08,1)j,rname(j),IF,IQ,rname(IPLAN(j,1))
      endif

*     branch to new reach ------------------------------------------------------
      if ( IPLAN(j,1) .eq. 0 .and. IPLAN(j,2) .gt. 0 
     &                       .and. IPLAN(j,3) .eq. 0 )then
      NextReach(j) = IPLAN(j,2)
      write(08,1)j,rname(j),IF,IQ,rname(IPLAN(j,2))
    1 format(i6,3x,a16,5x,2i6,5x,a16)
      endif

*     mixing of reaches -------------------------------------------------------- 
      if ( IPLAN(j,1) .gt. 0 .and. IPLAN(j,2) .gt. 0 
     &                       .and. IPLAN(j,3) .gt. 0 )then
      NextReach(IPLAN(j,1)) = IPLAN(j,3)
      NextReach(IPLAN(j,2)) = IPLAN(j,3)
      write(08,3)j,rname(j),IF,IQ,rname(IPLAN(j,3))
    3 format(i6,3x,a16,5x,2i6,5x,a16,5x,a16)
      endif

*     final reach --------------------------------------------------------------
      if ( IPLAN(j,1) .eq. 0 .and. IPLAN(j,2) .eq. 0 
     &.and. IPLAN(j,3) .eq. 0 )then
      write(08,1)j,rname(j),IF,IQ
      NextReach(j) = 0
      endif

      if ( IPLAN(j,1) .lt. 0 ) then
      write(08,1)j,rname(j),IF,IQ
      NextReach(j) = 0
      endif
      
      enddo

      write(08,4)
    4 format(77('-')//)

      return
      end

      
      subroutine write reach targets
      include 'COMMON DATA.FOR'

      if ( ical .eq. 1 ) return ! reach targets

      IQSreach = 0
      do jj = 1, nreach
      IREACH = ReachCode(jj)
*     check for the existence of any reach-specific standards ----------------------
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      if ( EQS reach (IREACH,idet) .gt. 0 ) then
      IQSreach = EQS reach (IREACH,idet)
      endif
      endif
      enddo ! do idet = 1, ndet
      enddo ! do jj = 1, nreach
      if ( IQSreach .eq. 0 ) return
      
      write(08,2) ! -------------------------------------------------------- INP
    2 format(//85('-')/
     &'Standards defined for particular reaches ...',
     &' a negative value indicates the target'/ 85('-')/
     &9x,'Name of reach        Determinand   ',
     &'      High      Good  Moderate      Poor'/
     &9x,'-------------        -----------   ',
     &'      ----      ----  --------      ----'/
     &85('-'))
      
      do jj = 1, nreach ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IREACH = ReachCode(jj)

*     check for the existence of reach-specific standards ----------------------
      IQSreach = 0
      do idet = 1, ndet ! ------------------------------------------------------
      if ( qtype (idet) .ne. 4 ) then ! ----------------------------------------
      if ( EQS reach (IREACH,idet) .gt. 0 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IQSreach = EQS reach (IREACH,idet)
      endif ! if ( EQS reach (IREACH,idet) .gt. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( qtype (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
      
      if ( IQSreach .gt. 1 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      do idet = 1, ndet ! ======================================================
      if ( qtype (idet) .ne. 4 ) then ! ========================================
      if ( EQS reach (IREACH, idet) .gt. 0 ) then ! ----------------------------
      nst = EQS reach (IREACH, idet)
      class limmits (nclass,idet) = 1.0e10
      ncc = nclass-1
      do ic = 1, nclass - 1 ! --------------------------------------------------
      class limmits (ic,idet) = standards for reach (nst,ic)   
      enddo ! ------------------------------------------------------------------
      if ( standards for reach (nst,ic) .gt. 1.0e9 ) then ! ....................
      ncc = 1
      write(08,11)IREACH,rname(IREACH),dname(idet),
     &class limmits(1,idet)      
   11 format(i6,3x,a16,5x,a11,3x,40('.'),f10.2)
      else ! ................................................................... 
      write(08,1)IREACH,rname(IREACH),dname(idet),
     &(class limmits (ic,idet), ic = 1, ncc)      
    1 format(i6,3x,a16,5x,a11,3x,10f10.2)
      endif ! if ( standards for reach (nst,ic) ................................
      endif ! if ( EQS reach (IREACH, idet) .gt. 0 ) ---------------------------
      endif ! if ( qtype (idet) .ne. 4 ) =======================================
      enddo ! do idet = 1, ndet ================================================
      endif ! if ( IQSreach .gt. 1 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      enddo ! do jj = 1, nreach ! ++++++++++++++++++++++++++++++++++++++++++++++
      
      write(08,4)
    4 format(85('-')//)

      return
      end
      
      
      

      subroutine initial track
      include 'COMMON DATA.FOR'

*     set the counter for the number of models run in this batch run -----------
      model number in batch = 0
      
*     switch off (set "master set used" to zero) the use of a master set of data
*     in a batch run allow each data file in the batch run to use its own number
*     of shots, determinands, etc, rather than constraining them to match those 
*     for the run --------------------------------------------------------------
*     this is a device used in testing new versions of SIMCAT on long lists of - 
*     data files ---------------------------------------------------------------
*     master set used = 1 ... use a master set of data from the first set of ---
*     data ---------------------------------------------------------------------
      master set used = 0 ! do not use a master set of data --------------------

      if ( model number in batch .eq. 1 ) then
      if ( master set used .eq. 1 ) then
      call change colour of text (18) ! light grey
      write( *,95)
   95 format(130('-')/
     &'Using the first set of data to overwrite data on which ',
     &'determinands included etc ...') 
      else
      call change colour of text (18) ! light grey
      write( *,96)
   96 format(130('-')/'TEST MODE: using each set of data unchanged ...')
      endif
      endif

      struckd = 0
      tempd = 0
      seasd = 0
      NONPD = 0

      call set loads and proportions to zero

      return
      end



      subroutine close down all the output files
      include 'COMMON DATA.FOR'
      
      close (01) ! main output (.OUT)
      close (03) ! comparison of observed and calculated (.CAL)
      close (08) ! tables in input data (.INP)
      close (09) ! copy of screen output (.SCN)
      close (74) ! data for flow gap filling (.FCL)
      close (75) ! data for quality gap filling (.QCL)

      close (11) ! files containing inputs of monthly data 
      close (12) ! files containing inputs of non-parametric data
      close (22) ! old style visual basic graphics (.SGR)
      close (27) ! report on load (.LOD)
      close (30) ! compliance with targets (.TGT)
      close (31) ! outputs on effluents (.EFF)
      close (33) ! outputs on errors (.ERR)
      close (36) ! outputs on effluents via the EFF.CSV file
            
      close (42) ! GIS data for SAGIS (GIS1.SCV)
      close (48) ! output on classification (.WFD)
      close (72) ! river flows (.FLO)
      
      do ichan = 1,10 ! 
      close (100+ichan) ! ------ monthly data --------------------------- Di.MON
      close (110+ichan) ! ------ used by SAGIS -------------------------- Di.CSV
      close (180+ichan) ! ------ used by GIS - compliance etc ---------- DAi.CSV
      close (260+ichan) ! ------ apportionment between water bodies ---- WBi.CSV
      close (190+ichan) ! ------ apportionment of conc from discharges -- Qi.CSV
      close (200+ichan) ! ------ diffuse pollution ---------------------- Di.OUT
      close (160+ichan) ! ------ apportionment of load from discharges -- Pi.CSV 
      close (120+ichan) ! ------ apportionment of loads from discharge -- Di.ADL
      close (170+ichan) ! ------ output from gap filling ---------------- Di.GAP
      close (220+ichan) ! ------ loads from catchments ------------------ Di.ACL
      close (230+ichan) ! ------ data for graphs ------------------------ Gi.CSV
      close (240+ichan) ! ------ word reports --------------------------- Wi.CSV
      close (140+ichan) ! ------ percentile apportionment --------------- Di.ADC
      close (150+ichan) ! ------ output for discharges ------------------ Di.EFF
      close (130+ichan) ! xxxxxxxxxxxx if ( ifeffcsv . eq. 1 ) xxxxxxxxxx Ei.CSV
      enddo
      
*     delete the storage of loads ==============================================
      do ireach = 1, NREACH ! ==================================================
      kreach = ReachCode (ireach)
      close (300+ireach,status='delete') ! delete storage of loads -------- .LOD   
      enddo ! do ireach = 1, NREACH ============================================
*     delete the storage of loads ==============================================
            
*     deal with a file that refused to be deleted ==============================
      write(fname,99)output_folder 
   99 format(a136)
      ireach = 609
      call trunkout ('L','O','D') ! --------------------------------------------
      open(609, file = fname, status='unknown', iostat=ierr)
      if ( ierr .eq. 0) close(609, status='delete') ! ==========================

      result = deldirqq(output_folder) ! delete folder that held these files ---

      close (004) ! working file (.WOK)
      close (005) ! working file (.WAK)
      close (007) ! working file (.WUK)
            
      close (23)  ! RUN---.TMP - contains names of files passed to SIMCAT
      close (488) ! Globalpolicydata.GPD
      close (493) ! Switches.GPD
      
*     rewind the data file for the present (batch) run ... FILENAME.DAT --------
      rewind 02
      return
      end

      
      
*     create extra data files for the catchment and model ----------------------
      subroutine create extra data files
      include 'COMMON DATA.FOR'

*     file for output for determinads .MON for this data file ------ 101,102 MON
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
 8001 format(a136)
      call trunk6 (kay)
      open(100+kay,FILE=FNAME) ! +++++++++++++++++++++++++++++++++++ 101,102 MON
      write(100+kay,*)' '
      endif
      enddo
      
*     file for output on apportionment for determinands ------------ 121,122 ADL
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk9 (kay) ! sort out a file for output on apportionment
      open(120+kay,FILE=FNAME) ! +++++++++++++++ ADL files +++++++++ 121,122 ADL
      write(120+kay,*)' '
      endif
      enddo

*     file for output on apportionment for determinands ------------ 141,142 ADC
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk10 (kay) ! apportionment of percentiles ++++++++++++ 141,142 ADC 
      open(140+kay,FILE=FNAME) ! +++++++++++++++++++++++++++++++++++ 141,142 ADC
      write(140+kay,*)' '
      endif
      enddo ! ++++++++++++++++++ apportionment of percentiles ++++++ 141,142 ADC

*     file for output on apportionment for determinands ------------ 151,152 EFF
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk17 (kay) ! output for discharges +++++++++++++++++++ 151,152 EFF 
      open(150+kay,FILE=FNAME) ! +++++++++++++++++++++++++++++++++++ 151,152 EFF
      write(150+kay,*)' '
      endif
      enddo ! ++++++++++++++++++ apportionment of percentiles ++++++ 151,152 EFF

*     file for output on apportionment to catchments --------------- 221,222 ACL
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk14 (kay) ! -------- apportionment to catchments ---- 221,222 ACL
      open(220+kay,FILE=FNAME) ! +++++++++++++++++++++++++++++++++++ 221,222 ACL
      write(220+kay,*)' '
      endif
      enddo ! --------------- apportionment to catchments ---------- 221,222 ACL
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     file for output for determinands --C1.GAP for this file GGGGGG 171,172 GAP
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk12 (kay) ! assistance with calibration GGGGGGGGGGGGG 171,172 GAP
      open(170+kay,FILE=FNAME) ! GGGGGGGGGGGG--C1.GAP GGGGGGGGGGGGGG 171,172 GAP
      write(170+kay,*)' '
      endif
      enddo ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG 171,172 GAP
      
      if ( ical .eq. 4 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG 171,172 GAP
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      !write(170+idet,5000)dname(idet)
 5000 format(110('-')/
     &'Advice on calibration for ',a11/110('-')/
     &'Loads added '/110('-')/
     &'      Mean   5%-tile  95%-tile  Standard  Distance',3x,
     &'Feature',30x,'Reach'/
     &'      ....   .......  ........ Deviation           ',
     &/110('-'))
      endif
      enddo
      endif ! if ( ical .eq. 4 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG 171,172 GAP
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     file for output on for determinands Di.CSV for this data file ++++ 111,112
      do kay = 1, ndet ! +++++++++++++++++++++++++++++++++++++++++++ 111,112,etc 
      if ( QTYPE (kay) .ne. 4 ) then ! +++++++++++++++++++++++++++++ 111,112,etc 
      write (fname,8001) datname
      call trunk7 (kay) ! ++++++++++++++++++++++++ Di.CSV ++++++++++ 111,112,etc
      open(110+kay,FILE=FNAME) ! +++++++++++++++++ Di.CSV ++++++++++ 111,112,etc
      write(110+kay,*)' '
      backspace(110+kay)
      write(110+kay,8755)bday,bmon,IYEAR,ical ! --------- headings ------ Di.CSV
 8755 format('SIMCAT',',','Version 161  ',',', ! ------------------------ Di.CSV
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',
     &'Type of data,',
     &'  January, February,    March,    April,      May,     June,',
     &'     July,   August,September,  October, November, December,',  
     &'   Annual,  Feature,   Source,', 
     &' % total disch load,', 
     &'disch load, lower conf, upper conf,', ! ----------------------------- 161
     &' % of river conc,disch river conc, lower conf, upper conf,', ! ------ 161
     &' length of u/s river') ! -------------------------------------------- 161
      endif ! if ( QTYPE (kay) .ne. 4 ) ! ------------------------------- Di.CSV
      enddo ! do kay = 1, Ndet ! ---------------------------------------- Di.CSV
      
*     file for output on for determinands ++++++++++++++++++++++++++++++ DAi.CSV
      do kay = 1, Ndet ! ----------------------------------------------- DAi.CSV
      if ( QTYPE (kay) .ne. 4 ) then ! --------------------------------- DAi.CSV 
      write (fname,8001) datname
      call trunk27 (Kay) ! add stem ++++++++++++++++++++++++++++++++++++ DAi.CSV
      open(180+kay,FILE=FNAME) ! +++++++++++++++++++++++++++++++++++++++ DAi.CSV
      write(180+kay,*)' '
      backspace(180+kay)
      write(180+kay,8110)!Title ! -------------------------------------- DAi.CSV
 8110 format('GIS data,Feature code,Name of feature,', ! --------------- DAi.CSV 
     &'Name of Reach,TYPE OF DATA,TYPE OF DATA,RESULTS,', ! ------------ DAi.CSV 
     &',,,,,,,,,,,,,,','Type of pollution',',,,Location') ! ------------ DAi.CSV 
      write(180+kay,8410) ! -------------------------------------------- DAi.CSV
 8410 format(
     &',,,,River flow:,,Distance,', ! ---------------------------------- DAi.CSV
     &'Annual mean,95-percentile'/
      
     &',,,,River concentration:,,Distance,', ! ------------------------- DAi.CSV
     &'Annual mean,Standard,Lower conf.,Upper conf.'/ ! ---------------- DAi.CSV
     &',,,,,,,,deviation,limit on mean,limit on mean'//
      
     &',,,,Load in the river:,,Distance,', ! --------------------------- DAi.CSV
     &'Annual mean,Standard,Lower conf.,Upper conf.'/ ! ---------------- DAi.CSV
     &',,,,,,,,deviation,limit on mean,limit on mean'//
*     ------------------------------------------------------------------ DAi.CSV
     &',,,,Net load from:,Type of pollution,Distance,
     &Annual mean,,,,,,,,,,,,,,,,'// ! --------------------------------- DAi.CSV
           
     &',,,,% Confidence of failure,,Distance,', ! ---------------------- DAi.CSV
     &'% CONFIDENCE,% Confidence,% Confidence,% Confidence,',
     &'% Confidence,% Confidence,% of Length,% of Length,',
     &'% of Length,% of Length,% of Length'/
     &7(','),'OF FAILED,',
     &'of high class,of good class,of moderate,of poor class,',
     &'of bad class,',
     &'in high class,in good class,in moderate,in poor class,',
     &'in bad class'/
     &7(','),'STANDARD,'//
     &',,,,% days exceeding annual mean concentration,,Distance,', ! --- DAi.CSV
     &'Annual,January,February,March,April,May,June,July,',
     &'August,September,October,November,December'//
*     ------------------------------------------------------------------ DAi.CSV      
     &',,,,Load and concentration from:,Type of pollution,Distance,', !- DAi.CSV    
     &'Contribution,% Contribution,Standard,', ! ----------------------- DAi.CSV
     &'Contribution,% Contribution,Standard,', ! ----------------------- DAi.CSV
     &'Number of,Lower conf.,Upper conf.,', ! -------------------------- DAi.CSV
     &'% Lower conf.,% Upper conf.,'/ ! -------------------------------- DAi.CSV
     &7(','),'to mean load,to mean load,deviation,', ! ----------------- DAi.CSV
     &'to mean,to mean,deviation,', ! ---------------------------------- DAi.CSV
     &'samples,limit on mean,limit on mean,', ! ------------------------ DAi.CSV
     &'limit on mean,limit on mean,'/ ! -------------------------------- DAi.CSV
     &7(','),',,,', ! -------------------------------------------------- DAi.CSV
     &'concentration,concentration,concentration,', ! ------------------ DAi.CSV
     &',load,load,', ! ------------------------------------------------- DAi.CSV
     &'concentration,concentration,'// ! ------------------------------- DAi.CSV
      
*     ------------------------------------------------------------------ DAi.CSV
     &',,,,Load and concentration from this discharge:,', ! ------------ DAi.CSV
     &'Name of discharge,Distance,', !---------------------------------- DAi.CSV
     &'Contribution,% Contribution,Standard,', ! ----------------------- DAi.CSV
     &'Contribution,% Contribution,Standard,', ! ----------------------- DAi.CSV
     &'Number of,Lower conf.,Upper conf.,', ! -------------------------- DAi.CSV
     &'% Lower conf.,% Upper conf.,% of total'/ ! ---------------------- DAi.CSV
      
     &7(','),'to mean load,to mean load,deviation,', ! ----------------- DAi.CSV
     &'to mean,to mean ,deviation,', ! --------------------------------- DAi.CSV
     7'samples,limit on mean,limit on mean,', ! ------------------------ DAi.CSV
     &'limit on mean,limit on mean,discharge'/ ! ----------------------- DAi.CSV
     &7(','),',,,', ! -------------------------------------------------- DAi.CSV
     &'concentration,concentration,concentration,', ! ------------------ DAi.CSV
     &',load,load,', ! ------------------------------------------------- DAi.CSV
     &'concentration,concentration,load'//) ! -------------------------- DAi.CSV
*     ------------------------------------------------------------------ DAi.CSV
      
      endif ! if ( QTYPE (kay) .ne. 4 ) -------------------------------- DAi.CSV 
      enddo ! do kay = 1, Ndet ----------------------------------------- DAi.CSV


*     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx WBi,CSV
*     file for output on for determinands WBi.CSV for this data file --- WBi.CSV
      do kay = 1, Ndet ! ----------------------------------------------- WBi.CSV
      if ( QTYPE (kay) .ne. 4 ) then ! --------------------------------- WBi.CSV 
      write (fname,8001) datname
      call trunk28 (Kay) ! add stem ------------------------------------ WBi.CSV
      open(260+kay,FILE=FNAME) ! --------------------------------------- WBi.CSV
      write(260+kay,*)' '
      backspace(260+kay)
      write(260+kay,8610) ! -------------------------------------------- WBi.CSV
 8610 format('GIS data,Feature code,Name of feature,', ! --------------- WBi.CSV 
     &'Name of Reach,Source of data,', ! ------------------------------- WBi.CSV 
     &'Load from:,Source of load,Distance,', ! ------------------------- WBi.CSV    
     &'Annual mean load,Lower confidence limit,', ! -------------------- WBi.CSV
     &'Upper confidence limit,Number of samples,', ! -----=------------- WBi.CSV
     %'% of total load,', ! -------------------------------------------- WBi.CSV
     &'Length of water body,Load per km.', ! --------------------------- WBi.CSV
     &2(','),'Feature code for load,',(','),'Name of feature') ! ------- WBi.CSV
      endif ! if ( QTYPE (kay) .ne. 4 ) -------------------------------- WBi.CSV 
      enddo ! do kay = 1, Ndet ----------------------------------------- WBi.CSV
*     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx WBi,CSV


*     CSV output on apportionment of load from discharges +++++++++++++++ Pi.CSV
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk4 (Kay) ! ++++++++++++++++++++++++ Pi.CSV ++++++++++ 161,162,etc
      open(160+kay,FILE=FNAME) ! +++++++++++++++++ Pi.CSV ++++++++++ 161,162,etc
      write(160+kay,*)' '
      backspace(160+kay)
      endif
      enddo ! ++++++++++++++++++++++++++++++++++++ Pi.CSV ++++++++++ 161,162,etc

*     CSV output on apportionment of load from discharges +++++++++++++++ Qi.CSV
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk37 (Kay) ! +++++++++++++++++++++++ Qi.CSV ++++++++++ 191,192,etc
      open(190+kay,FILE=FNAME) ! +++++++++++++++++ Qi.CSV ++++++++++ 191,192,etc
      write(190+kay,*)' '
      backspace(190+kay)
      endif
      enddo ! ++++++++++++++++++++++++++++++++++++ Qi.CSV ++++++++++ 191,192,etc
  
*     output on diffuse pollution for each determinand ++++++++++++++++++ Di.OUT
      do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk39 (Kay) ! +++++++++++++++++++++++ Di.OUT ++++++++++ 201,202,etc
      open(200+kay,FILE=FNAME) ! +++++++++++++++++ Di.OUT ++++++++++ 201,202,etc
      write(200+kay,*)' '
      backspace(200+kay)
      endif
      enddo ! ++++++++++++++++++++++++++++++++++++ Di.CSV ++++++++++ 201,202,etc

*     files for effluent csv output .CSV for this data file ++++++++ 131,132,etc
      if ( ifeffcsv . eq. 1 ) then ! +++++++++++++ Ei.CSV ++++++++++ 131,132,etc
      do kay = 1, Ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname ! ++++++++++++++++++++++++++++++++++++++ Ei.CSV
      call trunk8 (Kay) ! ++++++++++++++++++++++++ Ei.CSV ++++++++++ 131,132,etc
      open(130+kay,FILE=FNAME) ! +++++++++++++++++ Ei.CSV ++++++++++ 131,132,etc
      write(130+kay,*)' '
      backspace(130+kay)
      call leadblanks ! eliminate leading blanks from a run title
      write(130+kay,8255)bday,bmon,IYEAR,ical,dname(kay) ! ++++++++++++++ Ei.CSV
 8255 format('SIMCAT',',','Version 161  ',',', ! Ei.CSV ++++++++++++ 131,132,etc
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a11,
     &',','Annual Mean',',','Standard Dev.',',','Percentile',
     &',','Description',',','Discharge type',',','Name of Discharge')
      endif
      enddo ! do kay = 1, Ndet
      endif ! ++++++++++++++++++++++++++++++++++++ Ei.CSV ++++++++++ 131,132,etc

      
*     files for csv output for graph plotting  -------- Gi.CSV ----- 231,232,etc
      do kay = 1, Ndet ! ------------------------------------------------ Gi.CSV
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk15 (Kay) ! +++++++++++++++++++++++ Gi.CSV ++++++++++ 231,232,etc
      open(230+kay,FILE=FNAME) ! --------- graph plotting --------------- Gi.CSV
      write(230+kay,*)' ' ! ---------------- graph plotting ------------- Gi.CSV
      backspace(230+kay)
      call leadblanks ! eliminate leading blanks from a run title
      endif
      enddo ! do kay = 1, Ndet
      
*     files for csv output for WORD reports  ---------- Wi.CSV ----- 241,242,etc
      do kay = 1, Ndet ! ------------------------------------------------ Wi.CSV
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk16 (Kay) ! +++++++++++++++++++++++ Wi.CSV ++++++++++ 241,242,etc
      open(240+kay,FILE=FNAME) ! ----------- word reports --------------- Wi.CSV
      write(240+kay,*)' ' ! ---------------- word reports --------------- Wi.CSV
      backspace(240+kay)
      call leadblanks ! eliminate leading blanks from a run title
      endif
      enddo ! do kay = 1, ndet
      
*     file for outputs to effluents .EFF ... for this data file --------------31
      write (fname,8001) datname
      call trunk ('E','F','F') ! add stem to an output file
      open(31,FILE=FNAME) ! outputs on effluents .........................31 EFF
      write(31,*)' '

*     file for outputs to effluents .... EFF.CSV ... for this data file ------36
      write (fname,8001) datname
      call trunk3 ! add stem for output file for effluents ... .EFF
      open(36,FILE=FNAME) ! outputs on effluents via the EFF.CSV file --------36
      call write headings for the effluent CSV file

*     file for output on river flows for this data file .FLO -----------------72
      write (fname,8001) datname
      call trunk ('F','L','O') ! add stem to output file for river flows -----72
      open(72,FILE=FNAME) ! output on river flows -------------------------- FLO 
      write(72,*)' '

*     file for output on classification for this data file -------------------48
      write (fname,8001) datname
      call trunk ('W','F','D') ! output on classification ------------------ WFD
      open(48,FILE=FNAME) ! output on classification ----------------------- WFD
      write(48,*)' '

      return
      end


      subroutine write final messages for model run
      include 'COMMON DATA.FOR'

      call gettim ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP2 = LAP2-BIGLAPSE
      if ( LAP2 .lt. 0 ) LAP2 = LAP2 + 86400
      IMIN = LAP2/60
      ISEC = LAP2-60*IMIN
      if ( iscreen .lt. 3 ) write( *,1) IMIN,ISEC
      write(09,1)IMIN,ISEC
      write(33,1)IMIN,ISEC
    1 format(94('-'),' Lapsed time:',i6,' minutes and ',i2,' seconds')

      return
      end


      subroutine check ical is legal etc
      include 'COMMON DATA.FOR'

      kerror = 1
      if ( ifbatch .eq. 0 ) close (23)
      iscreen = 0 ! control the amount of information written to the screen ----
      call getdat ( IYEAR, JMONTH, IDAY ) ! obtain the date --------------------
      call gettim ( IHR, IMIN, ISEC, IHUN ) ! obtain the time ------------------
      LAPSE = IHR*3600+IMIN*60+ISEC ! prepare to compute the time taken for run-
      
      nogap = 1 ! initialise no gap filling
      if ( ical .eq. -7 .or. ical .eq. -8 .or. 
     &     ical .eq. -9 ) then
      ical = -ical
      nogap = 0 ! apply gap filling
      endif
      if ( ical .eq. 4 ) nogap = 0 ! apply gap filling for mode 4
      
      if ( ICAL .lt. 0 .or. ICAL .gt. 9) then
      if ( iscreen .lt. 3 ) write( *,1)
    1 format(/'*** The value must lie between zero and 9 ...'//
     &'*** Please enter the correct value for the option ',
     &'required ...')
      call stop
      endif

      ITEST = MAX0 (NV, NE)
      ITEST = MAX0 (ITEST,NE)
      if ( ITEST .gt. MT ) then
      write( *,2)ITEST-1
    2 format(/77('-')/
     &'The variable called MT in COM.FOR must exceed',I7/77('-'))
      call stop
      endif

      return
      end

      
      subroutine set screen text colour
      include 'COMMON DATA.FOR'

      if ( ical .eq. 01 ) then
     	call change colour of text (13) ! bright magenta
      endif
      if ( ical .eq. 02 ) then
     	call change colour of text (22) ! light blue
      endif
      if ( ical .eq. 03 ) then
	    call change colour of text (11) ! cyan
      endif
      if ( ical .eq. 00 ) then
        call change colour of text (15) ! white
      endif
      if ( ical .eq. 04 ) then
        call change colour of text (15) ! white
      endif
      if ( ical .eq. 07 ) then
        call change colour of text (50) ! brown
      endif
      if ( ical .eq. 08 ) then
        call change colour of text (19) ! light pink
      endif
      if ( ical .eq. 09 ) then
        call change colour of text (12) ! orange
      endif

      return
      end


      subroutine check the number of shots is not too big
      include 'COMMON DATA.FOR'

      if ( NS .gt. MS) then
      call change colour of text (10) ! green
      write( *,1)MS
    1 format('*** Too many shots ...',29x,'...',7x,'number cut to',I6,
     &25x,25('.'))
      call set screen text colour
      write(09,2)MS
      write(33,2)MS
      if ( iscreen .lt. 3 ) write(01,2)MS
    2 format(77('-')/'*** Too many shots ...'/
     &'*** The number has been cut to',I6,' ...'/77('-')/)
      NS = MS
      endif

      NS = MAX0(5,NS)

      return
      end
      

      subroutine check the number of shots for monthly data
      include 'COMMON DATA.FOR'

*     check for monthly structure ----------------------------------------------
      if ( munthly structure .eq. 1 ) then ! check the number of shots ---------          
          
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
      call set up default random normal deviates
      call set indices for percentiles etc ! ------ check shots for monthly data 
                    
      call set screen text colour
      write( *,1)
    1 format(77('-'))
      call change colour of text (10) ! green
      write( *,2)NS
    2 format(
     &'*** Your data is set up to expect monthly ',
     &'sets     ...'/
     &'*** The number of shots has been re-set to',i6,'   ...')
      call set screen text colour
      write( *,1)
      
      write(01,3)NSold,NS
      write(09,3)NSold,NS
      write(33,3)NSold,NS
      write(08,3)NSold,NS ! ----------------------------------------------- .INP
      do idet = 1, ndet    
      if ( qtype(idet) .ne. 4 ) then
      write(100+idet,3)NSold,NS
      endif
      enddo
    3 format(77('-')/
     &'*** Your SIMCAT data is set up to expect monthly structured ',
     &'data ...',5x,'****'/
     &'*** The specified number of shots is',i6,' ...',27x,'****'/
     &'*** This should be a multiple of 365 ...',33x,'****'/
     &'*** The number of shots has been re-set to',i6,' ...',21x,'****'/
     &77('-')) 
      endif ! if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) +++++++++++++++

      endif ! if ( munthly structure .eq. 1 ) ----------------------------------

      return
      end

      
      
      subroutine set indices for percentiles etc
      include 'COMMON DATA.FOR'

*     index for obtaining percentile from an ordered list of NS items ----------
      k95 = 0.05 * NS ! 95-percentile
      if ( k95 .eq. 0 ) k95 = 1
      k90 = 0.10 * NS ! 90-percentile
      if ( k90 .eq. 0 ) k90 = 1
      k80 = 0.20 * NS ! 80-percentile
      if ( k80 .eq. 0 ) k80 = 1
      k50 = 0.50 * NS ! 50-percentile
      if ( k50 .eq. 0 ) k50 = 1
      k98 = max0 (1, int(0.02 * float(NS))) ! 98-percentile
      if ( k98 .eq. 0 ) k98 = 1
      k99 = max0 (1, int(0.01 * float(NS))) ! 99-percentile
      if ( k99 .eq. 0 ) k99 = 1
      k995 = max0 (1, int(0.005 * NS)) ! 99.5-percentile
      if ( k995 .eq. 0 ) k995 = 1
      k999 = max0 (1, int(0.001 * NS)) ! 99.9-percentile
      if ( k999 .eq. 0 ) k999 = 1
      k05 = 0.95 * NS ! 5-percentile
      if ( k05 .eq. 0 ) k05 = 1
      k01 = 0.99 * NS ! 1-percentile
      if ( k01 .eq. 0 ) k01 = 1
      k10 = 0.90 * NS ! 10-percentile
      if ( k10 .eq. 0 ) k10 = 1

*     percentile for river quality targets -------------------------------------
      kptarg = 95 
      if ( output mode .eq. 2 ) kptarg = 90
      if ( output mode .eq. 3 ) kptarg = 99
 
*     index for obtaining 95-percentile from an ordered list of NS items -------
      kf95 = 0.05 * NS
      if ( kf95 .eq. 0 ) kf95 = 1
*     index for obtaining 90-percentile from an ordered list of NS items -------
      kf90 = 0.10 * NS
      if ( kf90 .eq. 0 ) kf90 = 1
*     index for obtaining 90-percentile from an ordered list of NS items...
      kf80 = 0.20 * NS
      if ( kf80 .eq. 0 ) kf90 = 1
*     index for obtaining 99-percentile from an ordered list of NS items...
      kf99 = 0.01 * NS
      if ( kf99 .eq. 0 ) kf99 = 1
*     index for obtaining 5-percentile from an ordered list of NS items...
      kf01 = 0.99 * NS
      if ( kf01 .eq. 0 ) kf01 = NS
      kf05 = 0.95 * NS
      if ( kf05 .eq. 0 ) kf05 = NS
      kf10 = 0.90 * NS
      if ( kf10 .eq. 0 ) kf10 = NS
      kf20 = 0.80 * NS
      if ( kf20 .eq. 0 ) kf20 = NS
      kf50 = 0.50 * NS
      if ( kf50 .eq. 0 ) kf50 = NS

      return
      end


      subroutine read temperature data
      include 'COMMON DATA.FOR'

      read(02,*,ERR = 7500)TDEG ! mean temperature
      Tsdev = 3.0
      Tcorf = -0.6
      QD90 Temperature = Tdeg + 1.2815 * TSdev
      backspace 02

      read(02,*,ERR = 7598) TDEG,TSDEV ! mean and standard deviation
      Tcorf =-0.6
      backspace 02

      read(02,*,ERR = 7599) TDEG,TSDEV,TCORF
      goto 7597

 7598 TSDEV = 3.0
      Tcorf = -0.6
      goto 7597
      read(02,*,ERR = 7500)TDEG
  
 7599 TCORF = -0.6
 7597 continue
      
      return

 7500 write( *,7573)
      if ( nobigout .le. 0 ) write(01,7573)
      write(09,7573)
      write(33,7573)
 7573 format(/77('-')/
     &'*** Error in the general data at the head of the ',
     &'SIMCAT datafile ...'/
     &'*** Error in reading the data on temperature ...'/77('-'))
      call stop
      end


      subroutine sort out determinands
      include 'COMMON DATA.FOR'

      ndetCP = 0
      do j = 1, ndet
      if ( QTYPE (j) .ne. 4 ) ndetCP = ndetCP + 1
      enddo
      if ( ical13 .eq. 0 ) then
      write(22,*)ndetCP ! -------------------------------------------------- SGR
      endif ! if ( ical13 .eq. 0 )

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( ifbatch .ne. 1 ) then ! output file ------------------------ GIS1.CSV
      write(42,5299) TITLE ! output file ------------------------------ GIS1.CSV
 5299 format('$Title'/'"',a70,'"') ! output file ---------------------- GIS1.CSV
      write(42,5699) datname ! output file ---------------------------- GIS1.CSV
 5699 format('"',a70,'","Data file ..."',',"Single run  ..."')
      write(42,5228) MP10 ! output file .GIS1.CSV ------------------------------
 5228 format(i4,',"Number of determinands ..."')
      !write(42,5699) datname ! output file --------------------------- GIS1.CSV
!5899 format('"',a70,'","Data file ..."',',"Part of batch run ..."')
      endif ! if ( ifbatch .ne. 1 ) --- output file ------------------- GIS1.CSV

      endif ! if ( ical13 .eq. 0 ) 
      endif ! if ( noGIS .eq. 0 ) ----------------------------------------------

      do 73 j = 1,MP10
      if ( ical13 .eq. 0 ) then
      if ( dname (j) .eq. '           ' ) dname(j) = '...........'
      if ( QTYPE(j) .ne. 4 ) then
      write(22,222)QTYPE(j),dname(j),MRQS(j) ! ----------------------------- SGR
  222 format(i3,1x,'"',a11,'"',i6) ! --------------------------------------- SGR
      endif
      if ( noGIS .eq. 0 ) then ! -----------------------------------------------
      write(42,242)j,qtype(j),dname(j),MRQS(j)
  242 format(i3,',',i3,',','"',a11,'"',',',i6,',"Determinand number,
     & Type of determinand,
     & Name of the determinand, Code number of set of river quality ',
     &'targets"')
      endif
      endif ! if ( ical13 .eq. 0 )
   73 continue

      if ( Ndet .le. 3 ) QTYPE (4) = 4
      if ( Ndet .le. 2 ) QTYPE (3) = 4

      if ( nDET .le. 0) then

      if ( nobigout .le. 0 ) write(01,2263)
      write( *,2213)
 2213 format(/77('-')/
     &'*** No data on determinands read from data file ...'/
     &'*** Calculations halted ...'/77('-'))
      write(09,2263)
      write(33,2263)
 2263 format(/77('-')/
     &'*** No data on determinands read from data file ...'/
     &'*** Calculations halted ...'/77('-'))
      call stop
      endif

      return
      end


      subroutine write file and screen headings
      include 'COMMON DATA.FOR'
      character *1 a1(2)

      write(bh,6411)IHR
 6411 format(i2)
 6453 format(a2)
      read(bh,6451)a1(1),a1(2)
 6451 format(2a1)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bh,6451)a1(1),a1(2)
 
      write(bmin,6411)IMIN
      read(bmin,6451)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bmin,6451)a1(1),a1(2)
      
      call change colour of text(18) ! light grey   
      if ( ifbatch .eq. 0 ) write( *,2963)bday,bmon,IYEAR,BH,bmin
      write(01,2963)bday,bmon,IYEAR,BH,bmin
      write(08,2963)bday,bmon,IYEAR,BH,bmin
      write(09,2963)bday,bmon,IYEAR,BH,bmin
 2963 format(77('-')/
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X,
     &'Date: ',a2,'/',a2,'/',I4/'Version 161  ', ! -----------------------------
     &'(Tony Warn 15/11/23)',18x,'...', ! --------------------------------------
     &5x,'  Time:  ',4x,a2,'.',a2) ! -------------------------------------------


      if ( ical .ne. 1 ) write(03,1963)bday,bmon,IYEAR,BH,bmin ! -------- Di.MON
 1963 format(77('-')/
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X,
     &'Date: ',a2,'/',a2,'/',I4/'Version 161  ', ! -----------------------------
     &'(Tony Warn 15/11/23)',18x,'...', ! --------------------------------------
     &5x,'  Time:  ',4x,a2,'.',a2) ! -------------------------------------------
      if ( ical13 .eq. 0 ) then ! ==============================================
      do ic = 1, ndet ! ========================================================
      if ( nobigout .le. 0 ) then ! ============================================
      if ( QTYPE (ic) .ne. 4 ) then ! ==========================================
      write(100+ic,1963)bday,bmon,IYEAR,BH,bmin ! ----------------------- Di.MON
      write(100+ic,4972)
 4972 format(77('-'))
      write(120+ic,2963)bday,bmon,IYEAR,BH,bmin
      write(120+ic,4972)
      write(220+ic,2963)bday,bmon,IYEAR,BH,bmin
      write(220+ic,4972)
      write(170+ic,2963)bday,bmon,IYEAR,BH,bmin ! ----------------------- Ci.GAP
      write(170+ic,4972) ! ---------------------------------------------- Ci.GAP
      write(140+ic,2963)bday,bmon,IYEAR,BH,bmin ! ----------------+++++++ Ai.ADC
      write(140+ic,4972) ! ---------------------------------------+++++++ Ai.ADC          
      write(150+ic,2963)bday,bmon,IYEAR,BH,bmin ! ----------------+++++++ Di.EFF
      write(150+ic,4972) ! ---------------------------------------+++++++ Di.EFF          
      write(200+ic,2963)bday,bmon,IYEAR,BH,bmin ! ----------------+++++++ Di.OUT
      write(200+ic,4972) ! ---------------------------------------+++++++ Di.OUT          
      endif ! if ( QTYPE (ic) .ne. 4 ) =========================================
      endif ! if ( nobigout .le. 0 ) ===========================================
      enddo ! do ic = 1, ndet ==================================================
      else !  ! if ( ical13 .eq. 0 ) ===========================================
      do ic = 1, ndet ! ================================================= Ci.GAP
      if ( QTYPE (ic) .ne. 4 ) then ! ==========================================
      write(170+ic,2963)bday,bmon,IYEAR,BH,bmin ! ----------------------- Ci.GAP
      write(170+ic,4972) ! ---------------------------------------------- Ci.GAP
      endif ! if ( QTYPE (ic) .ne. 4 ) =========================================
      enddo ! =========================================================== Ci.GAP
      endif ! if ( ical13 .eq. 0 ) =============================================
      write(33,2963)bday,bmon,IYEAR,BH,bmin
      if ( ical13 .eq. 0 ) then
      write(27,2963)bday,bmon,IYEAR,BH,bmin
      write(30,2963)bday,bmon,IYEAR,BH,bmin ! ------------------------------ TGT
      write(31,2963)bday,bmon,IYEAR,BH,bmin ! ------------------------------ EFF
      write(48,2963)bday,bmon,IYEAR,BH,bmin
      write(72,2963)bday,bmon,IYEAR,BH,bmin
      endif !  if ( ical13 .eq. 0 )

      if ( nobigout .le. 0 ) then
      write(01,4972)
      if ( ical .ne. 1 ) write(03,4972)
      endif
      if ( iscreen .lt. 3 ) write( *,4972)
      write(08,4972)
      write(09,4972)
      write(33,4972)
      if ( ical13 .eq. 0 ) then
      write(27,4972) 
      write(30,4972)
      write(31,4972)
      write(48,4972)
      write(72,4972)
      endif ! if ( ical13 .eq. 0 )

*     write out the title on the data file =====================================
      write(09,7941)TITLE
      write(08,7941)TITLE 
      if ( ical .ne. 1 ) write(03,7941)TITLE ! ---------------------------- .CAL
      if ( nobigout .le. 0 ) then ! ============================================
      do ic = 1, ndet ! ========================================================
      if ( QTYPE (ic) .ne. 4 ) then
      if ( ical .ne. 3 .and. ical .ne. 1 ) then ! ==============================
      write(120+ic,7941)TITLE ! ----------------------------------------- Di.ADL
      write(100+ic,7941)TITLE ! - --------------------------------------- Di.MON
      write(140+ic,7941)TITLE ! ----------------------------------------- Di.ADC
      write(150+ic,7941)TITLE ! ----------------------------------------- Di.EFF
      write(220+ic,7941)TITLE ! ----------------------------------------- Di.ACL
      write(200+ic,7941)TITLE ! ----------------------------------------- Di.OUT
      endif ! if ( ical .ne. 3 .and. ical .ne. 1 ) =============================
      write(170+ic,7941)TITLE ! ----------------------------------------- Ci.GAP
      endif ! if ( QTYPE (ic) .ne. 4 ) 
      enddo ! do ic = 1, ndet ==================================================
      endif ! if ( nobigout .le. 0 ) ===========================================

      write(33,7941)TITLE
      if ( ical13 .eq. 0 ) then
      write(27,7941)TITLE
      write(30,7941)TITLE
      write(31,7941)TITLE ! ------------------------------------------------ EFF
      write(48,7941)TITLE
      write(72,7941)TITLE
      endif !  if ( ical13 .eq. 0 )
 7941 format(A70)

      if ( nobigout .le. 0 ) write(01,7941)TITLE

      
      
*     BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      if ( ifbatch .eq. 1 ) then ! this is a batch run =========================
      datfilename = datname
      call lose folder ! eliminate the name of a folder from a filename 
      call set screen text colour
      
      if ( ical .eq. 0 ) then
      write( *,9044)model number in batch,Datfilename
 9044 format(/130('-')/
     &'Basic runs in batch ... ',4x,'Number:',i5,11x,'...',
     &6x,a40/130('-'))
      endif 


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg    
      if ( calspecial .eq. 1 ) then ! ==========================================
*     renew gap filling --------------------------------------------------------
      if ( ical .eq. 1) then
      write( *,2344)model number in batch,Datfilename
 2344 format(/130('-')/ 
     &'Fill gaps for river flows',13x,i6,6x,' ... ',
     &5x,a40/130('-'))
      endif ! if ( ical .eq. 1 ) -----------------------------------------------
      if ( ical .eq. 3 ) then ! ------------------------------------------------
      write( *,2345)model number in batch,Datfilename
 2345 format(/130('-')/   
     &'Fill gaps for river quality',11x,i6,6x,' ... ',
     &5x,a40/130('-'))
      rewind 74
      endif ! if ( ical .eq. 3 ) -----------------------------------------------
      endif ! if ( calspecial .eq. 1 ) =========================================

      if ( ical .eq. 2 ) then ! ------------------------------------------------
      write( *,2365)model number in batch,Datfilename
 2365 format(/130('-')/
     &'Run the gap-filled flow model',9x,i6,6x' ... ',
     &5x,a40/130('-'))
      rewind 74
      endif ! if ( ical .eq. 2 ) -----------------------------------------------
      if ( ical .eq. 4 ) then ! ------------------------------------------------
      write( *,8544)model number in batch,Datfilename
 8544 format(/130('-')/
     &'Run the gap-filled model',7x,
     &i13,6x,' ... ',5x,a40/130('-'))
      rewind 74
      rewind 75
      endif ! if ( ical .eq. 4 ) -----------------------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg    


*     77777777777777777777777777777777777777777777777777777777777777777777777777
*     mode 7 - set standards for effluents -------------------------------------
      if ( ical .eq. 07 .and. ifbatch .eq. 1 ) then ! --------------------------
      call change colour of text (12) ! orange
      if ( no gap filling 78 .eq. 1 ) then
      write( *,8331)model number in batch,Datfilename
 8331 format(/130('-')/
     &'Set actions to meet targets',8x,i9,6x,' ... ',
     &5x,a40/130('-'))

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      else
      write( *,8631)model number in batch,Datfilename
 8631 format(/130('-')/
     &'Set actions to meet targets',8x,i9,6x,' ... ',
     &5x,a40,5x,'With gap filling ........'/130('-'))
      write( *,5621)
 5621 format('Existing gap-filling files will be used ...'/ ! ------------------
     &77('='))
      rewind 74
      rewind 75
      endif ! if ( no gap filling 78 .eq. 1 )
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      call change colour of text (15) ! bright white
      endif ! if ( ical .eq. 07 .and. ifbatch .eq. 1 ) -------------------------
*     77777777777777777777777777777777777777777777777777777777777777777777777777

      
*     88888888888888888888888888888888888888888888888888888888888888888888888888
*     mode 8 - set standards for effluents -------------------------------------
      if ( ical .eq. 08 .and. ifbatch .eq. 1 ) then
      call change colour of text (19) ! light pink

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( no gap filling 78 .eq. 1 ) then
      write( *,8332)model number in batch,Datfilename
 8332 format(/130('-')/
     &'Set actions (with no deterioration)',i9,6x,' ...',
     &6x,a40/130('-'))
      else
      write( *,8632)model number in batch,Datfilename
 8632 format(/130('-')/
     &'Set actions (with no deterioration)',i9,6x,' ...',
     &6x,a40,5x,'With gap filling ........'/130('-'))
      write( *,5621)
      rewind 74
      rewind 75
      endif ! if ( no gap filling 78 .eq. 1 )
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      call change colour of text (15) ! bright white
      endif
*     88888888888888888888888888888888888888888888888888888888888888888888888888


*     99999999999999999999999999999999999999999999999999999999999999999999999999      
*     mode 9 - set standards for effluents -------------------------------------
      if ( ical .eq. 09 .and. ifbatch .eq. 1 ) then
      call change colour of text (50) ! light pink

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      if ( no gap filling 78 .eq. 1 ) then
      write( *,8337)model number in batch,Datfilename
 8337 format(/130('-')/
     &'Set actions (imposing u/s quality)',i8,8x,' ...',
     &6x,a40/130('-'))
      else
      write( *,8638)model number in batch,Datfilename
 8638 format(/130('-')/
     &'Set actions (imposing u/s quality)',i8,8x,' ...',
     &6x,a40,5x,'With gap filling ........'/130('-'))
      write( *,5621)
      rewind 74
      rewind 75
      endif ! if ( no gap filling 78 .eq. 1 )
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     99999999999999999999999999999999999999999999999999999999999999999999999999      

      
      call change colour of text (15) ! bright white
      endif
      endif ! if ( ifbatch .eq. 1 ) ============================================
*     BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

      
      
      call check the number of shots for monthly data

      if ( iscreen .lt. 3 ) write( *,8371)NS
 8371 format(55x,'Number of Shots -',I5)
      if ( nobigout .le. 0 ) then
      write(01,8271)NS
 8271 format(77('-')/55x,'Number of Shots -',I5)
      do ic = 1,ndet
      if ( QTYPE (ic) .ne. 4 ) write(170+ic,8271)NS ! ------------------- Ci.GAP
      enddo
      endif ! if ( nobigout .le. 0 )
      write(09,8271)NS
      write(08,8271)NS
      write(33,8271)NS
      if ( ical .ne. 1 ) write(03,8271)NS
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      write(27,8271)NS
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      write(100+ic,8271)NS
      write(120+ic,8271)NS
      write(220+ic,8271)NS
      write(140+ic,8271)NS ! -------------------------------------+++++++ Ai.ADC
      write(150+ic,8271)NS ! -------------------------------------+++++++ Di.EFF
      write(200+ic,8271)NS ! -------------------------------------+++++++ Di.OUT
      endif ! if ( nobigout .le. 0 )
      enddo ! do ic = 1, ndet
      write(30,8271)NS
      write(31,8271)NS
      write(48,8271)NS
      write(72,8271)NS
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------

      if ( iscreen .lt. 3 ) write( *,7983)
 7983 format(77('-'))
      if ( nobigout .le. 0 ) then
      write(01,7983)
      do ic = 1,ndet
      if ( QTYPE (ic) .ne. 4 ) write(170+ic,7983) ! --------------------- Ci.GAP
      enddo
      endif ! if ( nobigout .le. 0 )
      write(08,7983)
      write(09,7983)
      write(33,7983)
      if ( ical. ne. 1 ) write(03,7983)
      if ( ical13 .eq. 0 ) then
      write(27,7983)
      do ic = 1, ndet
      if ( nobigout .le. 0 .and.QTYPE (ic) .ne. 4 ) then
      write(100+ic,7983)
      write(120+ic,7983)
      write(220+ic,7983)
      write(140+ic,7983) ! ---------------------------------------+++++++ Ai.ADC
      write(150+ic,7983) ! ---------------------------------------+++++++ Di.EFF
      write(200+ic,7983) ! ---------------------------------------+++++++ Di.OUT
      endif
      enddo ! do ic = 1, ndet
      write(30,7983)
      write(31,7983)
      write(48,7983)
      write(72,7983)
      endif ! if ( ical13 .eq. 0 )


      goto(6500,6501,6502,6503,6504,6599,6599,6507,6508,6509),ICAL+1
 
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     straightforward run with no gap filling -------------------------------- 0
 6500 continue
      if ( iscreen .lt. 3 ) write( *,6670)ICAL
 6670 format(10x,'Run type =',I2,' (Basic Simulation)'/77('-')) ! ------------ 0
      if ( nobigout .le. 0 ) then
      write(01,6670)ICAL
      endif
      write(03,6670)ICAL
      write(09,6670)ICAL
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4) then
      write(100+ic,6670)ICAL
      write(120+ic,6670)ICAL
      write(220+ic,6670)ICAL
      write(140+ic,6670)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(150+ic,6670)ICAL ! -----------------------------------+++++++ Di.EFF
      write(170+ic,6670)ICAL ! -----------------------------------+++++++ Ci.GAP
      write(200+ic,6670)ICAL ! -----------------------------------+++++++ Di.OUT
      endif
      enddo
      write(27,6670)ICAL
      write(30,6670)ICAL
      write(31,6670)ICAL
      write(33,6670)ICAL
      write(48,6670)ICAL
      write(72,6670)ICAL
      goto 6599
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     gap filling for river flows -------------------------------------------- 1
 6501 continue
      if ( iscreen .lt. 3 ) write( *,6671)ICAL
 6671 format(15x,'Run type =',I2,
     &' (set up gap filling for river flow)',/77('-')) ! --------------------- 1
      write(09,6671)ICAL
      write(33,6671)ICAL
      write(01,6671)ICAL
      do ic = 1,ndet
      if ( QTYPE (ic) .ne. 4 ) write(170+ic,6671)ICAL ! ----------------- Ci.GAP
      enddo
      goto 6599

 6502 continue
      if ( iscreen .lt. 3 ) write( *,6672)ICAL
 6672 format(12x,'Run type =',I2,
     &' (using filled gaps for river flow)'/77('-')) ! ----------------------- 2
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE(ic) .ne. 4 ) then
      write(100+ic,6672)ICAL
      write(120+ic,6672)ICAL
      write(220+ic,6672)ICAL
      write(170+ic,6672)ICAL ! ------------------------------------------ Ci.GAP
      write(140+ic,6672)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(150+ic,6672)ICAL ! -----------------------------------+++++++ Di.EFF
      write(200+ic,6672)ICAL ! -----------------------------------+++++++ Di.OUT
      endif
      enddo
      write(33,6672)ICAL
      if ( nobigout .le. 0 ) write(01,6672)ICAL
      write(03,6672)ICAL
      write(31,6672)ICAL ! ------------------------------------------------- EFF
      write(09,6672)ICAL
      write(27,6672)ICAL
      goto 6599

*     gap filling for river quality ------------------------------------------ 3
 6503 continue
      if ( iscreen .lt. 3 ) then
      call change colour of text (11) ! cyan
      write( *,6673)ICAL
 6673 format(11x,'Run type =',I2,' (Set up gap filling for river ', ! -------- 3
     &'quality)'/77('-'))
      call change colour of text (15) ! white
      write( *,7983)
      endif
      if ( nobigout .le. 0 ) write(01,6673)ICAL
      write(03,6673)ICAL
      write(09,6673)ICAL
      write(33,6673)ICAL
*     write(31,6673)ICAL ! ------------------------------------------------- EFF
      do ic = 1,ndet
      if ( QTYPE (ic) .ne. 4 ) write(170+ic,6673)ICAL ! ----------------- Ci.GAP
      enddo

      if ( output mode .eq. 2 ) then
      if ( iscreen .lt. 3 ) write( *,6683)
      write(09,6683)
 6683 format(13x,'This fits to the mean and 95-percentiles',
     &' of river quality ...')
      if ( nobigout .le. 0 ) write(01,6683)
      write(27,6683)
      endif

      if ( output mode .eq. 3 ) then
      if ( iscreen .lt. 3 ) write( *,6283)
      write(09,6283)
      if ( nobigout .le. 0 ) write(01,6283)
      if ( nobigout .le. 0 ) write(03,6283)
      write(27,6283)
      endif
 6283 format(11x,'This fits to the mean and 95-percentiles',
     &' of river quality ...')

      if ( iscreen .lt. 3 ) write( *,6892)ICAL
      write(09,6892)ICAL
      write(33,6892)ICAL
      if ( nobigout .le. 0 ) write(01,6892)ICAL
      if ( nobigout .le. 0 ) write(03,6892)ICAL
 6892 format(11x,'(Graphical output may be misleading in Run Type',
     &I2,')'/77('-'))
      goto 6599

 6504 if ( iscreen .lt. 3 ) write( *,6674)ICAL
 6674 format(12x,'Run type =',I2,' (Run the gap-filled ', ! ------------------ 4
     &'model)'/77('-'))
      if ( nobigout .le. 0 ) write(01,6674)ICAL
      write(03,6674)ICAL
      write(27,6674)ICAL
      write(30,6674)ICAL
      write(09,6674)ICAL
      write(33,6674)ICAL
      write(48,6674)ICAL
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      write(100+ic,6674)ICAL
      write(120+ic,6674)ICAL
      write(220+ic,6674)ICAL
      write(170+ic,6674)ICAL ! ------------------------------------------ Ci.GAP
      write(140+ic,6674)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(150+ic,6674)ICAL ! -----------------------------------+++++++ Di.EFF
      write(200+ic,6674)ICAL ! -----------------------------------+++++++ Di.OUT
      endif
      enddo
      write(72,6674)ICAL

      if ( output mode .eq. 2 ) then
      if ( iscreen .lt. 3 ) write( *,6683)
      write(09,6683)
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4) then
      write(100+ic,6683)
      write(120+ic,6683)
      write(220+ic,6683)
      write(170+ic,6683) ! ---------------------------------------------- Ci.GAP
      write(140+ic,6683) ! ---------------------------------------+++++++ Ai.ADC
      write(150+ic,6683) ! ---------------------------------------+++++++ Di.EFF
      write(200+ic,6683) ! ---------------------------------------+++++++ Di.OUT
      endif
      enddo
      if ( nobigout .le. 0 ) write(01,6683)
      write(27,6683)
      endif

      if ( output mode .eq. 3 ) then
      if ( iscreen .lt. 3 ) write( *,6283)
      write(09,6283)
      do ic = 1, ndet ! --------------------------------------------------------
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      write(100+ic,6283)
      write(120+ic,6283)
      write(220+ic,6283)
      write(170+ic,6283)
      write(140+ic,6283) ! ---------------------------------------+++++++ Ai.ADC
      write(150+ic,6283) ! ---------------------------------------+++++++ Di.EFF
      write(200+ic,6283) ! ---------------------------------------+++++++ Di.OUT
      endif
      enddo ! do ic = 1, ndet --------------------------------------------------
      if ( nobigout .le. 0 ) write(01,6283)
      write(27,6283)
      endif
      goto 6599
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     mode 7 - set standards for effluents -------------------------------------
 6507 continue
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      call write headings for mode 7
      goto 6599
      endif
      endif
      enddo

*     mode 8 - set standards for effluents -------------------------------------
 6508 continue
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      call write headings for mode 8
      goto 6599
      endif
      endif
      enddo

*     mode 9 - set standards for effluents -------------------------------------
 6509 continue
      do ic = 1, ndet
      if ( nobigout .le. 0 .and. QTYPE (ic) .ne. 4 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      call write headings for mode 9
      goto 6599
      endif
      endif
      enddo

 6599 continue
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! -----------------------------
      do jper = 1,ndet
      !if ( qtype(jper) .ne. 4 ) write(170+jper,6974)
 6974 format(/'---------------------------------------------------'/
     &        'Report on Diffuse Inputs and Natural Purification'/
     &        '---------------------------------------------------')
      enddo ! do jper = 1,ndet
      endif ! if ( ical13 .eq. 0 .or. ical .eq. 3 ------------------------------

      if ( ical13 .eq. 0 ) then

      do ic = 1, ndet ! --------------------------------------------------------
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6714) Dname(ic) ! ----------- Di.MON
 6714 format(/43('-')/'Report on monthly data for ',a11/43('-'))
      endif
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6774) Dname(ic) ! ----------- Di.ADL
 6774 format(/77('-')/'Apportionment of loads and concentrations ',
     &' for: ',a11/77('-'))
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6724) Dname(ic) ! ----------- Di.ADC
 6724 format(/77('-')/'Contributions to percentile concentrations ',
     &'for: ',a11/77('-'))
      if ( QTYPE (ic) .ne. 4 ) write(220+ic,6734) Dname(ic) ! ----------- Di.ACL
 6734 format(/77('-')/'Apportionment between sub-catchments ',
     &'for ',a11/77('-'))
      enddo ! do ic = 1, ndet --------------------------------------------------
      endif ! if ( ical13 .eq. 0 )
      write(09,6964)
 6964 format(/77('-')/
     &'Copy of information displayed on the screen ...'/77('-')/)
      write(33,6914)
 6914 format(/77('-')/'Report on errors and warnings ...'/77('-')/)
      if ( ical13 .eq. 0 ) then !=========================================== EFF
      write(31,6984) ! ----------------------------------------------------- EFF
 6984 format(/77('-')/'Report on discharges of effluent for all ', ! ------- EFF
     &'determinands ... '/77('-')/)
      do ic = 1, ndet ! ---------------------------------------------------- EFF
      if ( QTYPE(ic) .ne. 4 ) then
      write(150+ic,6989) dname(ic) ! -------------------------------------Di EFF
 6989 format(/77('-')/'Report on discharges of effluent for ',a11, ! ---- Di EFF
     &' ...'/77('-')/)
      write(200+ic,6089) dname(ic) ! ------------------------------------ Di OUT
 6089 format(/77('-')/'Report on diffuse pollution for ',a11, ! --------- Di OUT
     &' ...'/77('-')/)
      
      if ( Detype(ic) .eq. 900 ) then
      write(150+ic,6332)dname(ic)
 6332 format(77('=')/'The river quality standards apply to total ',
     &a11/'Total is the sum of solid and dissolved ...'/77('='))
      endif
      if ( Detype(ic) .eq. 909 ) then
      write(150+ic,6334)dname(ic)
 6334 format(77('=')/'The river quality standards apply to dissolved ',
     &a11,' ... and not total'/77('='))
      endif
      
      if ( Detype(ic) .eq. 104 ) then
      write(150+ic,6832)dname(ic)
 6832 format(77('=')/'Effluent quality will not be tuned to meet ',
     &'a downstream river quality target '/'for ',a11,' ... by ',
     &'setting a permit limit for dissolved oxygen itself ...'/77('='))
      endif
      if ( Detype(ic) .eq. 106 ) then
      write(150+ic,6833)dname(ic)
 6833 format(77('=')/'Effluent quality will not be tuned to meet ',
     &'a downstream river quality target '/'for ',a11,' ... by ',
     &'setting a permit limit for nitrate itself ...'/77('='))
      endif
     
      endif ! if ( QTYPE(ic) .ne. 4 ) ------------------------------------------
      enddo ! do ic = 1, ndet --------------------------------------------------
      endif ! if ( ical13 .eq. 0 ) ! ===========================================

*     if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( ical13 .eq. 0 ) then
      write(27,6994) ! ----------------------------------------------------- LOD
 6994 format(/'------------------------------------'/
     &        'Report on loads for all determinands'/
     &        '------------------------------------')

*     model number in batch - this is counter for the number of models run in --
*     this batch run -----------------------------------------------------------
      if ( model number in batch .eq. 1 ) then
*     write(27,6294)
 6294 format(/'--------------------------------------------------- '/
     &        'Report on Pollution Loads (suppressed in this mode) '/
     &        '--------------------------------------------------- ')
      endif
      endif ! if ( ical13 .eq. 0 )

      if ( ical13 .eq. 0 ) then
      write(30,8994)
 8994 format(/'-----------------------------------'/
     &        'Report on compliance with standards'/
     &        '-----------------------------------')
      endif ! if ( ical13 .eq. 0 )

      if ( ical13 .eq. 0 ) then
      
      write(48,8174)
 8174 format(/'----------------------------------------------------'/
     &        'Report on classification - Water Framework Directive'/
     &        '----------------------------------------------------'/)
      write(72,8784)
 8784 format(/77('-')/
     &'Report on river flows ... giving for each point ... '/
     &'... the percentiles running from 1 to 99 per cent ...'/
     &'... followed by the proportion of effluent in each of these ',
     &'percentiles ...'/
     &77('-')//)
      endif ! if ( ical13 .eq. 0 )

      if ( ical13 .eq. 0 ) then
      write(30,8966)
 8966 format(/116('-')/37x,
     &'Determinand  D',
     &'ist  Dist  Value   Target   Passed   Failed   Failed   ',
     &'Confidence'/56x,
     &'with     in       in   length   length   length   ',
     &'        of'/54x,
     &'target  river    river             (50%)    (95%)   ',
     &'   failure'/116('-'))
      endif ! if ( ical13 .eq. 0 )

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !if ( ical13 .eq. 0 .or. ical .eq. 3 ) then
      if ( IDIFFREACH .eq. 0 ) then
      write(33,6905)
      do jper = 1,ndet
      if ( qtype(jper) .ne. 4 ) write(170+jper,6905) ! ------------------ Ci.GAP
 6905 format(/77('-')/'Global diffuse inflows have been excluded ...'/
     &77('-'))
      enddo
      else
      write(33,6906)
      do jper = 1,ndet
      if ( qtype(jper) .ne. 4 ) write(170+jper,6906) ! ------------------ Ci.GAP
 6906 format(/77('-')/'Global diffuse inflows have been included ...'/
     &77('-'))
      enddo
      endif ! if ( IDIFFREACH .eq. 0 )
      !endif ! if ( ical13 .eq. 0 )
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( ical13 .eq. 0 .or. ical .eq. 3) then ! ==============================
      do idet = 1, ndet ! ------------------------------------------------------
      if ( qtype (idet) .ne. 4 ) then ! ----------------------------------------
      if ( ipur .eq. 0 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(170+idet,6915) ! -------------------------------------------- Ci.GAP
 6915 format('Global natural purification has been excluded ... '/
     &77('-')//)
      else
      write(170+idet,6916) ! -------------------------------------------- Ci.GAP
 6916 format('Global natural purification has been included ...'/
     &77('-')/
     &'Determinand    Rate Constant    Baseline Quality'/77('-'))
      write(170+idet,6917) Dname(idet),rate(idet), ! -------------------- Ci.GAP
     &qbase(idet),units(idet)
 6917 format(a11,f10.3,' 1/days ',f11.3,1x,a4/77('-')/)
      endif ! if ( ipur .eq. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( qtype (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
      endif ! if ( ical13 .eq. 0 .or. ical .eq. 3) =============================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,4590)
      write(08,4590)
 4590 format(77('-')/
     &'The river system is divided up into Reaches. ',
     &'Flow gauges, discharges,'/'monitoring stations, ',
     &'small tributaries, abstractions etc are called ',
     &'Features.'/
     &'The calculations start at an upstream boundary ',
     &'and proceed downstream from'/'Feature to Feature, ',
     &'accounting for the effects of Features, adding in'/
     &'contributions from diffuse sources and catering ',
     &'for Natural Purification ...'/77('-')/)

      return
      end


      subroutine write batch heading
      include 'COMMON DATA.FOR'
      character *1 a1(2)

      write(bh,6411)IHR
 6411 format(i2)
 6453 format(a2)
      read(bh,6451)a1(1),a1(2)
 6451 format(2a1)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bh,6451)a1(1),a1(2)
 
      write(bmin,6411)IMIN
      read(bmin,6451)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bmin,6451)a1(1),a1(2)

      write(bday,6411)iday
      read(bday,6451)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bday,6451)a1(1),a1(2)

      write(bmon,6411)JMONTH
      read(bmon,6451)a1(1),a1(2)
      if ( a1(1) .eq. ' ' ) a1(1) = '0'
      write(bmon,6451)a1(1),a1(2)
      
      write( *,2963)bday,bmon,IYEAR,BH,bmin ! ----------------------------------
 2963 format(77('-')/ ! --------------------------------------------------------
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X, ! -----------
     &'Date: ',a2,'/',a2,'/',I4/'Version 161  ', ! -----------------------------
     &'(Tony Warn 15/11/23)',18x,'...', ! --------------------------------------
     &7x,'Time:  ',4x,a2,'.',a2/77('-')) ! -------------------------------------
      
      return
      end




      subroutine set the constraints on discharge quality
      include 'COMMON DATA.FOR'
      if ( ical13 .eq. 1 ) return ! running in gap-fillng mode ----------------

      do JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then

*     if the worst discharge quality is entered as zero set up a default -------
      if ( WEQ(1,jp) .lt. 0.0001 ) then 
      WEQ(1,jp) = 999999.0
      if ( Detype (jp) .eq. 100 ) WEQ(1,jp) = 500.0 ! chloride
      if ( Detype (jp) .eq. 102 ) WEQ(1,jp) = 300.0 ! TOC
      if ( Detype (jp) .eq. 200 ) WEQ(1,jp) = 500.0 ! parent
      if ( Detype (jp) .eq. 201 ) WEQ(1,jp) = 500.0 ! daughter
      if ( Detype (jp) .eq. 101 ) WEQ(1,jp) = 24.0  ! BOD
      if ( Detype (jp) .eq. 106 ) WEQ(1,jp) = 15.0  ! nitrate
      if ( Detype (jp) .eq. 105 ) WEQ(1,jp) = 8.0   ! phosphate
      if ( Detype (jp) .eq. 104 ) WEQ(1,jp) = 0.0   ! dissolved oxygen
      if ( Detype (jp) .eq. 103 ) WEQ(1,jp) = 20.0  ! ammonia
      endif

*     worst effluent quality ---------------------------------------------------
      if ( WEQ(1,JP) .gt. 1.0E-8 ) then
      ecm = WEQ(1,JP)
      ecs = 0.6 * ecm ! assume coefficient of variation of 0.6
      gecm = ALOG (ecm*ecm/SQRoot(1230,ecm*ecm+ecs*ecs))
      gecs = SQRoot(1232,ALOG(1.+(ecs*ecs)/(ecm*ecm)))
      WEQ(3,JP) = EXP (gecm+1.6449*gecs) ! 95-percentile
      WEQ(1,JP) = ecm ! mean
      WEQ(2,JP) = ecs ! standard deviation
      endif ! if ( WEQ(1,JP) .gt. 1.0E-8 )
  
*     good effluent quality ----------------------------------------------------
      if ( detype (JP) .ne. 104 ) then ! not dissolved oxygen ------------------
      if ( GEQ(1,JP) .gt. 1.0E-8 ) then
      ecm = GEQ(1,JP)
      ecs = 0.6 * ecm ! assume coefficient of variation of 0.6
      gecm = ALOG (ecm*ecm/SQRoot(1230,ecm*ecm+ecs*ecs))
      gecs = SQRoot (1232,ALOG(1.+(ecs*ecs)/(ecm*ecm)))
      GEQ(3,JP) = EXP (gecm+1.6449*gecs) ! 95-percentile
      GEQ(1,JP) = ecm ! mean
      GEQ(2,JP) = ecs ! standard deviation
      endif ! if ( GEQ(1,JP) .gt. 1.0E-8 )
      endif ! if ( detype (JP) .ne. 104 ) not dissolved oxygen -----------------

*     best effluent quality ----------------------------------------------------
      if ( BEQ(1,JP) .gt. 1.0E-8 ) then
      ecm = BEQ(1,JP)
      ecs = 0.6 * ecm
      gecm = ALOG (ecm*ecm/SQRoot(1230,ecm*ecm+ecs*ecs))
      gecs = SQRoot (1232,ALOG(1.+(ecs*ecs)/(ecm*ecm)))
      BEQ(3,JP) = EXP (gecm+1.6449*gecs)
      BEQ(1,JP) = ecm
      BEQ(2,JP) = ecs
      endif ! if ( BEQ(1,JP) .gt. 1.0E-8 )
      
      endif
      enddo
      return
      end



*     read the data on the Reaches ---------------------------------------------
      subroutine read the reaches
      include 'COMMON DATA.FOR'
      character *1 spaces(136)
      character *200 line
      character *1 DUM1,DUM2
      real temkay(MP10) ! temporary storage of rate constants ------------------

      do ir = 1, MR
      rname(ir) = 'None ......'
      do kk = 1, 3
      iplan ( ir, kk ) = 0
      enddo
      enddo
      month write 1 = 0

*     skip the lines of comments -----------------------------------------------
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 7527
    
*     NEGREACH is an indicator that only a subset of the reaches will be run ---
*     a zero value means the whole model will be run ---------------------------
*     a zero reach number will indicate a truncated run ------------------------
*     set to zero initially ...
      negreach = 0
      negreach2 = 0

  103 continue

*     increase the reach counter by one ----------------------------------------
      NREACH = NREACH + 1

*     ensure that SIMCAT's array boundaries for reaches are not exceeded -------
      if ( nREACH .gt. MR) then
      write( *,1430) MR
 1430 format(
     &'*** You have entered too many Reaches for the current '/
     &'*** size of SIMCAT. The current size is ... ',i6,' reaches ...'/
     &'*** You must reduce number of reaches ...'/
     &'*** Or get a larger version of SIMCAT ...'/77('-'))
      call stop
      endif

*     --------------------------------------------------------------------------
*     Names of variables for Reaches ...
*     --------------------------------------------------------------------------
*     KREACH  - Code number for the Reach;
*     RNAME   - Name of the Reach;
*     RLENGTH - Length of the Reach (km);
*     RF      - Reference number for the river flow data
*     RQ      - Reference number for river quality
*     IPLAN   - Shows which Reach is to be processed next
*             - Gives the structure of the catchment
*     RFDIFF  - River flow data-set for diffuse inflow
*     RQDIFF  - Quality data-set for diffuse inflow
*     Rchkay  - Rate constants
*     --------------------------------------------------------------------------

 1429 continue

*     set all the rate contants for reaches to zero ----------------------------
      do i = 1, MP10
      temkay(i) = 0.0
      enddo
*     look at the next line of data --------------------------------------------
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 15
      call extract a line of data (Line)

*     test for the end of the data on Reaches ----------------------------------
      read(line,5200)spaces
 5200 format(136a1)
      if (spaces(1) .eq. '#' .and. spaces(4) .eq. '#' ) goto 15
      if (spaces(1) .eq. '*' .and. spaces(4) .eq. '*' ) goto 15
      if (spaces(1) .eq. '=' .and. spaces(4) .eq. '=' ) goto 103

*     add three x's to the end of the line in order to generate an error ------- 
*     and so re-read the line in different formats -----------------------------
      write(line,5201) (spaces(i),i=1,133)
 5201 format(133a1,'x','x','x')

*     read the number of the reach ---------------------------------------------
      read(line,*,ERR = 7507) KREACH
*     check for a reach with a negative reach number ...                                
*     this indicates that only part of the model is to be run ...                      
      if (kreach .lt. 0 ) then ! ***********************************************
*     only part of the model will be run ---------------------------------------
      kreach = - kreach ! rub out the negative sign ----------------------------
      do jjj=1,9
      if ( spaces(jjj) .eq. '-') then
      spaces(jjj) = ' '
      goto 1229
      endif 
      enddo ! do jjj=1,9
 1229 continue
      if ( negreach .gt. 0 .and. negreach2 .eq. 0 ) then 
*     store the number of the reach from which the model run will end ----------
      negreach2 = kreach
      else
*     store the number of the reach from which the model run will start --------
      negreach = kreach
      endif
      endif ! if (kreach .lt. 0 ) ! ********************************************


*     write the spaces to a line of data ---------------------------------------
      write(line,5201) (spaces(i),i=1,133)

*     exclude reaches with numbers that are too large for SIMCAT's storage -----
      if ( KREACH .gt. MR ) then
      if ( nobigout .le. 0 ) write(01,7372) KREACH
      write(08,7376) KREACH,MR
      write(09,7376) KREACH,MR
      write(33,7376) KREACH,MR
      write( *,7376) KREACH,MR
 7376 format(/
     &'---------------------------------------------------------- '/
     &'*** Illegal Reach Code: ',I6/
     &'*** The maximum is      ',I6/
     &'---------------------------------------------------------- ')
      call stop
      endif

      if ( KREACH .le. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7872) KREACH
 7872 format(
     &'*** Illegal Reach Code: ',I6,
     &' ... the Reach is excluded ...')
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,7372) KREACH
      write(08,7372) KREACH
      write(09,7372) KREACH
      write(33,7372) KREACH
 7372 format(/77('-')/
     &'*** Illegal Reach Code: ',I4/
     &'*** SIMCAT continues but the Reach is excluded ...'/77('-'))
      goto 103
      endif

*     store the reach code for later checks (on Features) ----------------------
      ReachCode (nreach) = kreach
      REACHCODE2 (kreach) = nreach
      
*     check the format for the reach structure data ----------------------------
*     first try the old (and preferred) format - using the array IPLAN ---------
      NewFormat = 99

      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3)
      
*     a negative value in IPLAN excludes the reach -----------------------------
      if ( IPLAN( KREACH,1 ) .lt. 0 ) goto 1429

*     mark this reach as the old (preferred) structure of reach data -----------
      NewFormat = 0

*     No error. The reach structure is in the old format.  Proceed to read -----
      goto 9500 ! to read the details of the reach -----------------------------

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     error found on reading the reach structure ... the reach structure is in -
*     the new format -----------------------------------------------------------

 7506 continue
      
*     mark this line as new structure ------------------------------------------
      NewFormat = 1
      IPLAN(kreach,1) = 0

      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2
      
*     negative value for NextReach excludes the Reach --------------------------
      if ( NextReach(KREACH) .lt. 0 ) goto 1429

 9500 continue ! read the reach data -------------------------------------------

*     initialise the values of the reach rate constants ------------------------
      do i = 1,10
      temkay(i) = 3333.9
      enddo ! ------------------------------------------------------------------

*     check for an error in the reach number -----------------------------------
      if ( KREACH .lt. 1 ) goto 9999

*     no error found - check for an error in determining the reach structure --
      if ( NewFormat .eq. 99 ) goto 9999

*     prepare to test for three or ten rate constants --------------------------
      Newkay = 99
      
*     ==========================================================================
*     read with the old (and preferred) structure of reaches -------------------
      if ( NewFormat .eq. 0 ) then

*     check for three rate contants plus four schematic items in this line -----
      read(line,*,ERR = 7501)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,7)
      Newkay = 17 ! items have been read ---------------------------------------
      goto 8699

*     check for three rate contants plus two schematic items in this line ------
 7501 continue
      read(line,*,ERR = 7502)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,5)
      Newkay = 15 ! items have been read ---------------------------------------
      goto 8699

*     check for zero rate contants plus four schematic items in this line ------
 7502 continue

      read(line,*,ERR = 7523)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,4)
      Newkay = 14 ! items have been read ---------------------------------------
      goto 8699

*     check for three rate contants plus no schematic items in this line -------
 7523 continue
      read(line,*,ERR = 7504)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,3)
      Newkay = 13 ! items have been read ---------------------------------------
      goto 8699

*     check for zero rate contants plus two schematic items in this line -------
 7504 continue

      read(line,*,ERR = 7505)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,2)
      Newkay = 12 ! items have been read ---------------------------------------
      goto 8699

*     check for zero rate contants plus no schematic items in this line --------
 7505 continue

      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      Newkay = 10
      goto 8699
      endif ! if ( NewFormat .eq. 0 ) old (preferred) reach structure ----------
*     ==========================================================================
      
*     check than an assignment of rate constants has been made -----------------
      if ( Newformat .eq. 0 .and. Newkay .eq. 99 ) goto 9997

*     ==========================================================================
*     prepare to do the same task but in the format for the reach structure ----
*     that is based on "next reach x x"
      Newkay = 99
      if ( NewFormat .eq. 1 ) then
*     check for three rate contants plus four schematic items in this line -----
      read(line,*,ERR = 7581)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,7)
      Newkay = 17 ! items have been read ---------------------------------------
      goto 8699

*     check for three rate contants plus two schematic items in this line ------
 7581 continue

      read(line,*,ERR = 7583)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,5)
      Newkay = 15
      goto 8699

*     check for zero rate contants plus four schematic items in this line ------
 7583 continue
      read(line,*,ERR = 7584)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,4)
      Newkay = 14
      goto 8699

*     check for three rate contants plus no schematic items in this line -------
 7584 continue
      read(line,*,ERR = 7582)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,3)
      Newkay = 13
      goto 8699

*     check for zero rate contants plus two schematic items in this line --------
 7582 continue

      read(line,*,ERR = 7585)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,2)
      Newkay = 12
      goto 8699

*     check for zero rate contants plus no schematic items in this line --------
 7585 continue
      
      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      Newkay = 10
      endif ! if ( NewFormat .eq. 1 ) - new structure --------------------------
*     ==========================================================================


*     check an assignment has been made ----------------------------------------
      if ( Newkay .eq. 99 ) goto 9999

*     check data structure variables are legal ---------------------------------
*     we now know the reach structure (the value of NewFormat) -----------------
*     and the details about how many rate rate constants have been entered -----
*     now read all the reach data for real -------------------------------------

 8699 continue
      
      if ( NewFormat .eq. 0 ) then ! use the old and preferred structure -------

*     if Newkay equals 17 there are only three rate constants and they are -----
*     read from the main line of reach data ------------------------------------
      if ( Newkay .eq. 17) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 15 there are only three rate constants and they are -----
*     read from the main line of reach data ------------------------------------
      if ( Newkay .eq. 15 ) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 13 there are only three rate constants and they are -----
*     read from the main line of reach data ------------------------------------
      if ( Newkay .eq. 13) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 14 there are ten rate constants and they are ------------
*     read from the main line of reach data ------------------------------------
      if ( Newkay .eq. 14) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
      endif

*     if Newkay equals 12 there are ten rate constants and they are read as ----
*     a new line of data -------------------------------------------------------
      if ( Newkay .eq. 12) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,2)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
      endif

*     if Newkay equals 10 there are ten rate constants and they are read as ----
*     a new line of data -------------------------------------------------------
      if ( Newkay .eq. 10) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
      endif
      endif ! if Newkay equals 10 there are ten rate constants -----------------
*     ==========================================================================
      

*     read data in the new format for reach structure --------------------------
      if ( NewFormat .eq. 1 ) then

*     if Newkay equals 17 there are only three rate constants ------------------
      if ( Newkay .eq. 17) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 15 there are only three rate constants ------------------
      if ( Newkay .eq. 15 ) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 13 there are only three rate constants ------------------
      if ( Newkay .eq. 13) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
      Xrate = Rchkay(4,KREACH)
      Rchkay(4,KREACH) = Rchkay(3,KREACH)
      Rchkay(3,KREACH) = Xrate
      endif

*     if Newkay equals 14 there are up to nine rate constants ------------------
      if ( Newkay .eq. 14) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
      endif

*     if Newkay equals 12 there are up to nine rate constants ------------------
      if ( Newkay .eq. 12) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
      endif

*     if Newkay equals 10 there are up to nine rate constants ------------------
      if ( Newkay .eq. 10) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
*     RFDIFF ...... river flow data-set for diffuse inflow to reaches ----------
*     RQDIFF ...... river quality data-set for diffuse inflow to reaches -------
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
*     RCHA ........ time-of-travel - the velocity at the mean flow -------------
*     RCHB ........ time-of-travel - the exponent ------------------------------ 
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet) ! read the rate constants
      endif
      endif ! if ( NewFormat .eq. 1 )

 9600 continue

*     set defaults for temperature ---------------------------------------------
      BMAT (kreach, 1, 1) = TDEG
      BMAT (kreach, 1, 2) = TSDEV
      BMAT (kreach, 1, 3) = TCORF

*     check whether data have been added on the background quality for the reach
*     or for standards for the reach... look at the next line of data ----------
      LTEST = 0
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------0
      call check for reach data on background (KREACH,LTEST)
      if ( LTEST .eq. 0 ) then ! 11111111111111111111111111111111111111111111111
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------1
      call check for reach data on background (KREACH,LTEST)
      if ( LTEST .eq. 0 ) then ! 22222222222222222222222222222222222222222222222
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ---==-------2
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 33333333333333333333333333333333333333333333333
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------3
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 44444444444444444444444444444444444444444444444
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------4
      call check for reach data on background (KREACH,LTEST)

      if ( LTEST .eq. 0 ) then ! 55555555555555555555555555555555555555555555555
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------5
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 66666666666666666666666666666666666666666666666
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ---=--------6
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 77777777777777777777777777777777777777777777777
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------7
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 88888888888888888888888888888888888888888888888
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------8
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 99999999999999999999999999999999999999999999999
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data ------------9
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 10101010101010101010101010101010101010101010101
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data -----------10
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 11111111111111111111111111111111111111111111111
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data -----------11
      call check for reach data on background (KREACH,LTEST)
      
      if ( LTEST .eq. 0 ) then ! 12121212121212121212121212121212121212121212121
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35 ! the line is a comment, not data -----------12
      call check for reach data on background (KREACH,LTEST)
      
      endif ! if ( LTEST .eq. 0 ) 1212121212121212121212121212121212121212121212
      endif ! if ( LTEST .eq. 0 ) 1111111111111111111111111111111111111111111111
      endif ! if ( LTEST .eq. 0 ) 1010101010101010101010101010101010101010101010
      endif ! if ( LTEST .eq. 0 ) 9999999999999999999999999999999999999999999999
      endif ! if ( LTEST .eq. 0 ) 8888888888888888888888888888888888888888888888
      endif ! if ( LTEST .eq. 0 ) 7777777777777777777777777777777777777777777777
      endif ! if ( LTEST .eq. 0 ) 6666666666666666666666666666666666666666666666
      endif ! if ( LTEST .eq. 0 ) 5555555555555555555555555555555555555555555555
      endif ! if ( LTEST .eq. 0 ) 4444444444444444444444444444444444444444444444
      endif ! if ( LTEST .eq. 0 ) 3333333333333333333333333333333333333333333333
      endif ! if ( LTEST .eq. 0 ) 2222222222222222222222222222222222222222222222
      endif ! if ( LTEST .eq. 0 ) 1111111111111111111111111111111111111111111111

*     go back for the data for the next reach ----------------------------------
      goto 103

*     reports on the errors in reading the reach data --------------------------
 9999 continue
      write( *,3100) Newkay,KREACH,Newformat
      if ( nobigout .le. 0 ) write(01,3100) Newkay,KREACH,Newformat
      write(03,3100) Newkay,KREACH,Newformat
      write(08,3100) Newkay,KREACH,Newformat
      write(09,3100) Newkay,KREACH,Newformat
      write(33,3100) Newkay,KREACH,Newformat
 3100 format(/'*** Error in reading a line of reach data ...'/
     &'*** Unable to detect the correct structure of reaches ...'/
     &'*** Number of rate constants =',i6/
     &'*** Number of reach          =',2i6) 
      call stop

 9998 continue
      write( *,3190) Newkay
      if ( nobigout .le. 0 ) write(01,3100) Newkay
      write(03,3190) Newkay
      write(08,3190) Newkay
      write(09,3190) Newkay
      write(33,3190) Newkay
 3190 format(/'*** Error in reading a line of old-style reach data ...'/
     &'*** Unable to detect the correct structure of reaches ...'/
     &'*** Number of rate constants =',i3) 
      call stop

 9997 continue
      write( *,3105)
      if ( nobigout .le. 0 ) write(01,3105)
      write(03,3105)
      write(08,3105)
      write(09,3105)
      write(33,3105)
 3105 format(/
     &'*** Error in reading the reach rate constants ...',
     &' are there enough?'/)
      call stop

*     all the reaches have been read ===========================================
   15 continue

*     set the number of reaches read in for for this run -----------------------
	nreach = nreach - 1

   35 continue

*     format the structure of the model ----------------------------------------
	if (NewFormat.eq.1) then
	call generate sequence of reaches
      endif  
      return

 7507 continue
	call change colour of text (20) ! bright red
      write( *,7778)
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,7778)
      write(08,7778)
      write(09,7778)
      write(33,7778)
 7778 format(/77('-')/
     &'*** Error in reading the data on Reaches ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Calculations halted ...'/
     &77('-'))
      call close down ! on error in reading data

 7527 continue
      write( *,7578)
      if ( nobigout .le. 0 ) write(01,7578)
      write(08,7578)
      write(09,7578)
      write(33,7578)
 7578 format(/
     &'-------------------------------------------------------'/
     &'*** No data on Reaches ...'/
     &'*** The data-file has been assembled incorrectly ...'/
     &'*** Calculations halted ...'/
     &'-------------------------------------------------------')
      call stop
      return
      end



      subroutine write gis descriptors ! for the file used by SAGIS ------------
      include 'COMMON DATA.FOR'

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
          
      write(42,5296) ! output file ------------------------------------ GIS1.CSV
 5296 format('$Items'/
     &'"The following items occur for each record ..."'/
*	jreach(kfeat),DIS-adist(jreach(kfeat)) - 2 items
     &'"  1: the code number of the reach (A)"'/
     &'"  2: the GIS code for the location (B)"'/
     &'"  3: the name of the reach (C)"'/
     &'"  4: the name of the feature or a mark for the end of ',
     &'reach (D)"'/
     &'"  5: indication of downstream or upstream of feature (E)"'/
     &'"  6: the code number for the type of feature - or 99 for ',
     &'the end of reach (F)"'/
     &'"  7: the distance from the head of the reach (G)"'/
     &'"  8: the type of data (H)"'/
     &'"  9: the units for the data (I)"'/
     &'"Subsequent items in the record depend on the type of data"'/
     &'"For records for river flow ... 8 items"'/
*     FLOW(1),FLOW(3),FLOW(2),FLOW(4) - ---------------------------------------
     &'" 10: the mean river flow (J)"'/
     &'" 11: the 90-percentile of river flow (K)"'/
     &'" 12: the 95-percentile of river flow (L)"'/
     &'" 13: the 99-percentile of river flow (M)"'/
     &'"And for some features such as flow gauges ..."'/
*     COB(1...COB(2... COB(3... COB(4... ---------------------------------------
*     --------------------------------------------------------------------------
     &'" 14: the observed mean river flow (N)"'/	 
     &'" 15: 90-percentile of observed river flow (O)"'/	 
     &'" 16: 95-percentile of observed river flow (P)"'/	 
     &'" 17: 99-percentile of observed river flow (Q)"'/	 
     &'"For river quality ... 42 items"'/
*     Dname(J),Units(J),Lunits(J),
     &'"  8: the name of the determinand (H)"'/
     &'"  9: the units of concentration (I)"'/
*     CL(jcp,J),jcp=1,12) 12 items -------------------------------------------
     &'" 10: the calculated value of the mean concentration (J)"'/
     &'" 11: the lower confidence limit on the estimate of the ',
     &'mean (K)"'/
     &'" 12: the upper confidence limit on the estimate of the ',
     &'mean (L)"'/
     &'" 13: calculated value of the 90-percentile concentration (M)"'/
     &'" 14: lower confidence limit on the percentile (N)"'/
     &'" 15: upper confidence limit on the percentile (O)"'/
     &'" 16: calculated value of the 95-percentile concentration (P)"'/
     &'" 17: lower confidence limit on the percentile (Q)"'/
     &'" 18: upper confidence limit on the percentile (R)"'/
     &'" 19: calculated value of the 99-percentile concentration (S)"'/
     &'" 20: lower confidence limit on this percentile (T)"'/
     &'" 21: upper confidence limit on this percentile (U)"'/
*     &(CD(jcp,J),jcp=1,12) 12 items -------------------------------------------
     &'" 22: the calculated value of the mean load (V)"'/
     &'" 23: lower confidence limit on the estimate of the mean ',
     &'load (W)"'/
     &'" 24: upper confidence limit on the estimate of the mean ',
     &'load (X)"'/
     &'" 25: calculated value of the 90-percentile load (Y)"'/
     &'" 26: lower confidence limit on the 90-percentile load (Z)"'/
     &'" 27: upper confidence limit on the 90-percentile load (AA)"'/
     &'" 28: calculated value of the 95-percentile load (AB)"'/
     &'" 29: lower confidence limit on the 95-percentile load (AC)"'/
     &'" 30: upper confidence limit on the 95-percentile load (AD)"'/
     &'" 31: calculated value of the 99-percentile load (AE)"'/
     &'" 32: lower confidence limit on this 99-percentile load (AF)"'/
     &'" 33: upper confidence limit on this 99-percentile load (AG)"'/
*     &RQO(J)-------------------------------------------------------------------
     &'" 34: river water quality standard (AH)"'/
*     &confail(J),--------------------------------------------------------------
     &'" 35: per cent confidence of failure of standard (AI)"'/
*     &propeff2(J)--------------------------------------------------------------
     &'" 36: per cent of load from effluent discharges (AJ)"'/
     &'"And, relevant for features such as monitoring stations ... "'/
*     (COB(1,J),COB(2,J),COB(3,J),COB(4,J)--------------------------------------
     &'" 38: any observed value of the mean concentration (AK)"'/
     &'" 39: observed value of the 90-percentile concentration (AL)"'/
     &'" 40: observed value of the 95-percentile concentration (AM)"'/
     &'" 41: observed value of the 99-percentile concentration (AN)"'/
*     &(in class (ic,J),ic=1,NC),
     &'" 42: standard for High (AO)"'/
     &'" 43: the confidence that the class is High (AP)"'/
     &'" 44: standard for  Good (AQ)"'/
     &'" 45: the confidence that the class is Good (AR)"'/
     &'" 46: standard for  Moderate (AS)"'/
     &'" 47: the confidence that the class is Moderate (AT)"'/
     &'" 48: standard for  Poor (AU)"'/
     &'" 49: the confidence that the class is Poor (AV)"'/
     &'" 50: standard for  Bad (AW)"'/
     &'" 51: the confidence that the class is Bad (AX)"'/
*     &Total length dets 00(J),Total length dets 50(J) ... ---------------------
     &'" 52: label for next three items (AY)"'/
     &'" 53: length of upstream river complying with 50% ',
     &'confidence (AZ)"'/
     &'" 54: length of river failing with 50% confidence (AV)"'/
     &'" 55: length of river failing with 95% cent confidence (AW)"'/
*     --------------------------------------------------------------------------
     &'" 1 - 7 and 8 - 55 are then repeated in records for 9 more ',
     &'determinands"'/
     &'"For overall confidence of class across all determinands ',
     &'... 55 items"'/
*    &(conf in class (ic), ic=1,NC) --------------------------------------------
     &'"  8: label - confidence of class (H)"'/
     &'"  9: label - per cent (I)"'/
     &'" 10: the confidence that the overall class is High (J)"'/
     &'" 11: the confidence that the overall class is Good (K)"'/
     &'" 12: the confidence that the overall class is Moderate (L)"'/
     &'" 13: the confidence that the overall class is Poor (M)"'/
     &'" 14: the confidence that the overall class is Bad (N)"'/
*    &((in class (ic,J),ic=1,NC),J=1,MP10) -------------------------------------
     &'" 15: the confidence that the class is High for determinand ',
     &'1 (O)"'/
     &'" 16: the confidence that the class is Good for determinand ',
     &'1 (P)"'/
     &'" 17: the confidence that the class is Moderate (Q)"'/
     &'" 18: the confidence that the class is Poor (R)"'/
     &'" 19: the confidence that the class is Bad (S)"'/
     &'" 20: the effective number of samples (T)"'/
     &'" Items 15 - 20 are then repeated in this record for 9 ',
     &'more determinands (U) to (BV)"'/
     &'" 74: the face value class across all determinands (BW)"'/
     &'" 75: the face value class for determinand 1 (BX)"'/
     &'" Item 75 is then repeated for the other 9 ',
     &'determinands (BY) to (CG)"'/
     &'" 76: the model number in a batch run (CH)"'/
     &'$Records')
      
      endif
      endif

      return
      end



      subroutine check for a reduced run
      include 'COMMON DATA.FOR'
      integer stReach,numlist,curpos,test

*     check for a reduced run ... only some reaches will be modelled ...        [A]
*     **************************************************************************[A]
*     This routine populates the array INCLUDED with 0 (True) if the reach is
*     to be included.  And -1 (False) if it is not included ....

      call generate next reach

      NumIncluded = nreach

      if ( negreach .gt. 0 ) then
          
      do 1 kk = 0, nreach
      if ( kk .gt. 0 ) then
      k = ReachCode(kk)
      else
      k = 0
      endif
      if ( k .gt. 0 ) then
      NFIN(k,1) = -1 ! initialise
      NFIN(k,2) = -1
      endif
    1 continue
      
      if (nextr .gt. 0) then
      if ( NFIN(nextr,1) .eq. -1 ) then
      NFIN(nextr,1) = k
      else
      if ( NFIN(nextr,2) .eq. -1 ) then
      NFIN(nextr,2) = k
      endif
      endif 
      endif

      do 2 kk = 1, nreach ! loop through all the reaches =======================
      k = ReachCode(kk) ! the code number of this reach ------------------------
      nextr = NextReach(k) ! the code number of the next reach -----------------
      if ( nextr .ge. 0 ) then ! there is a next reach -------------------------
      if ( NFIN(k,1) .eq. -1 ) then ! reset the first value of NFIN ... --------
      NFIN(k,1) = nextr ! ... to the code number of the next reach -------------
      else ! check the second possibility of a next reach ----------------------
      if ( NFIN(k,2). eq. -1 ) then !  reset the second value of NFIN ... ------
      NFIN(k,2) = nextr ! ... to the code number of this next reach ------------
      endif ! if ( NFIN(nextr,2). eq. -1 ) -------------------------------------
      endif ! if ( NFIN(nextr,1) .eq. -1 ) -------------------------------------
      endif ! if ( nextr .ge. 0 ) then ! if there is a next reach --------------
    2 continue ! do 2 kk = 1, nreach ===========================================
         
      do jj = 1,nreach ! -------------------------------------------------------
      j = ReachCode(jj)
      Included(j) = -1
      IncludedList(j) = 0
      enddo ! do jj = 1,nreach -------------------------------------------------

      stReach = Negreach ! starting reach
      enReach = Negreach2 ! final reach
      numList = 1
      curpos = 1
      Included(stReach) = 0 ! label the starting reach
      Included(enReach) = 0 ! label the final reach
      IncludedList(1) = stReach
      
      iadd = 0
      icheck = negreach
 4343 if (iplan (icheck,1) .gt. 0) iadd = iplan(icheck,1)
      if (iplan (icheck,2) .gt. 0) iadd = iplan(icheck,2)
      if (iplan (icheck,3) .gt. 0) iadd = iplan(icheck,3)
      if ( iadd .gt. 0 ) then
      Included(iadd) = 0
      icheck = iadd
      iadd = 0
      if ( icheck .ne. negreach2 ) goto 4343
      endif
      
      go to 6016 ! #############################################################
      irech = stReach
      do ! ---------------------------------------------------------------------
      do 4 k=1,1  !2
      test = nfin(irech, k)
      if (test .gt. 0) then
      curpos = curpos + 1
      Included(test) = 0
      IncludedList(curpos) = test
      irech = test
      endif
    4 continue
      
      numList = numList + 1
      if ( numList .gt. curpos) then 
      exit
      endif
      stReach = IncludedList(numList)
      enddo ! ------------------------------------------------------------------
 6016 continue ! ################################################################
      
      
      
*     Set IPLAN for furthest downstream reach to 0,0,0
      if ( negreach2 .eq. 0 ) then
      iplan(negreach,1)=0
      iplan(negreach,2)=0
      iplan(negreach,3)=0
      else
      iplan(negreach2,1)=0
      iplan(negreach2,2)=0
      iplan(negreach2,3)=0
      endif

*     Set IPLAN for all excluded reaches to 0,0,0
      do jj=1,NREACH
      j=ReachCode(jj)
      if ( Included(j) .ne. 0 ) then
      iplan(j,1) = 0
      iplan(j,2) = 0
      iplan(j,3) = 0
      endif
      enddo

      NumIncluded = NumList - 1

      goto 7373
      
*     ==========================================================================[8]
*     a reduced SIMCAT run has been requested.  The first reach is now number   [8]
*     NEGREACH initialise the store of the numbers of the reaches to be         [8]
*     included in this run ...                                                  [8]
*     ==========================================================================[8]
*     check for the selection of a "middle" reach ...                           [8]
*     if this happens proceed with a full run of SIMCAT ...                     [8]
*     ==========================================================================[8]
*     0,x,0  the next Reach will be a branch to Reach number x                  [8]
*     x,0,0  the next Reach will be a straight continuation to Reach number x   [8] 
*     z,y,x  the next Reach, number x, will be formed by mixing z and y         [8]
*     ==========================================================================[8]

      do 4273 ir = 1, nreach
      krr = reachcode (ir)

*     the specified reach is formed from another other reach ...
      if ( IPLAN (krr,1) .eq. negreach .and.
     &IPLAN (krr,2) .eq. 0 .and. IPLAN (krr,1) .eq. 0 ) then
      negreach = 0
      endif

      if ( IPLAN (krr,3) .eq. negreach ) then

*     the specified reach is formed from other reaches ...
      negreach = 0
      goto 5999
      endif

 4273 continue 

*     a reduced SIMCAT run has been requested.  The first reach is now number 
*     NEGREACH initialise the store of the numbers of the reaches to be included
*     in this run ... store the first reach ...

      numreaches = 1
      reachstore(numreaches) = negreach

      do ir = 2, nreach
      reachstore(ir) = 0
      enddo
      do ig = 1, nreach
      ir=reachcode(ig)
      enddo

*     identify this reach ---------------------------------------------------------
      negorder = 0
      do ir = 1, MR
      if ( reachcode(ir) .eq. negreach ) negorder = ir
      enddo

*     --------------------------------------------------------------------------[a]
*     now find which other reaches that are to be included in the truncated run [a]
*     --------------------------------------------------------------------------[a]
*     0,x,0  the next Reach will be a branch to Reach number x                  [a]
*     x,0,0  the next Reach will be a straight continuation to Reach number x   [a] 
*     z,y,x  the next Reach, number x, will be formed by mixing z and y         [a]
*     --------------------------------------------------------------------------[a]
*     case where the truncation starts with the first reach ...                 [a]
*     --------------------------------------------------------------------------[a]

      if ( negorder .eq. 1 ) then
      m2 = reachcode (1)

*     --------------------------------------------------------------------------[a]
*     check whether the next reach is a branch to a new tributary ...           [a]
*     --------------------------------------------------------------------------[a]
      megreach = negreach

 6429 continue

      if ( iplan (megreach,1) .ne. 0 ) then
*     --------------------------------------------------------------------------[a]
*     the next reach is a continuation ... include it ...                       [a]
*     --------------------------------------------------------------------------[a]
      numreaches = numreaches + 1
      reachstore(numreaches) = iplan (negreach,1)
      megreach = iplan (negreach,1)
      goto 6429

      else

      do 6341 irr = 1, nreach
      m2 = reachcode (irr)
      IPLAN (m2,1) = 0
      IPLAN (m2,2) = 0
      IPLAN (m2,3) = 0 
      if ( irr .ne. 1 ) rname (m2) = 'Dead ....' 
 6341 continue
      endif

      goto 5090
      endif

*     --------------------------------------------------------------------------[a]
*     end of case where truncation is the first reach ...                       [a] 
*     --------------------------------------------------------------------------[a]

*     --------------------------------------------------------------------------[b]
*     case where the truncation does not start with the first reach ...         [b]
*     --------------------------------------------------------------------------[b]
*     first set up the starting reach  ... then look for the other reaches ...  [b]
*     --------------------------------------------------------------------------[b]
      krr = negreach

*     --------------------------------------------------------------------------[b]
*     check for a mixing of reaches downstream of NEGREACH ...                  [b]
*     --------------------------------------------------------------------------[b]
      if ( IPLAN (krr,1)*IPLAN (krr,2)*IPLAN (krr,3) .gt. 0 ) then

*     --------------------------------------------------------------------------[b]
*     this means that only this reach is to be run ...                          [b]
*     --------------------------------------------------------------------------[b]
*     blank out all the other reaches ...                                       [b]
*     --------------------------------------------------------------------------[b]
      m1 = reachcode (negorder)

      do 6391 nrr = 1, nreach
      m2 = reachcode (nrr)
      IPLAN (m2,1) = 0
      IPLAN (m2,2) = 0
      IPLAN (m2,3) = 0 
      if ( m1 .ne. m2 ) rname (m2) = 'Dead ....' 
 6391 continue

      do 2438 krr = 1, nreach
      ir = reachcode (krr)
 2438 continue

      goto 5090
      endif
*     --------------------------------------------------------------------------[b]

*     --------------------------------------------------------------------------[b]
*     0,x,0  the next Reach will be a branch to Reach number x                  [b]
*     x,0,0  the next Reach will be a straight continuation to Reach number x   [b] 
*     z,y,x  the next Reach, number x, will be formed by mixing z and y         [b]
*     --------------------------------------------------------------------------[b]

*     --------------------------------------------------------------------------[b]
*     check whether the next reach is a branch to a new tributary ...           [b]
*     --------------------------------------------------------------------------[b]

      megreach = negreach
 7429 continue

      if ( iplan (megreach,1) .ne. 0 .and. 
     &     iplan (megreach,2) .eq. 0 .and.
     &     iplan (megreach,3) .eq. 0 ) then

*     --------------------------------------------------------------------------[b]
*     the next reach is a continuation ... include it ...                       [b]
*     --------------------------------------------------------------------------[b]
      numreaches = numreaches + 1
      reachstore(numreaches) = iplan (megreach,1)
      megreach = iplan (megreach,1)

      goto 7429

      endif

*     --------------------------------------------------------------------------[b]
*     the next reach is a branch ... include it ...                             [b]
*     --------------------------------------------------------------------------[b]
*     --------------------------------------------------------------------------[b]
*     0,x,0  the next Reach will be a branch to Reach number x                  [b]
*     x,0,0  the next Reach will be a straight continuation to Reach number x   [b] 
*     z,y,x  the next Reach, number x, will be formed by mixing z and y         [b]
*     --------------------------------------------------------------------------[b]

 7729 continue
      if ( iplan (megreach,1) .eq. 0 .and. 
     &iplan (megreach,3) .eq. 0 ) then
      numreaches = numreaches + 1
      reachstore(numreaches) = iplan (megreach,2)
      megreach = iplan (megreach,2)
      goto 7729
      endif

*     --------------------------------------------------------------------------[b]
*     check for a mixing of reaches downstream of MEGREACH ...                  [b]
*     --------------------------------------------------------------------------[b]
      if ( IPLAN (megreach,1) * IPLAN (megreach,2)
     &                        * IPLAN (megreach,3) .gt. 0 ) then

      if ( IPLAN (megreach,1) .lt. negreach .or.
     &     IPLAN (megreach,2) .lt. negreach )  then

*     --------------------------------------------------------------------------[b]
*     blank out all the other reaches ...                                       [b]
*     --------------------------------------------------------------------------[b]
      do m2 = megreach, reachcode(nreach)
      IPLAN (m2,1) = 0
      IPLAN (m2,2) = 0
      IPLAN (m2,3) = 0 
      if ( m2 .ne. megreach ) rname (m2) = 'Dead ....' 
      enddo

      goto 5090
      endif

      numreaches = numreaches + 1
      reachstore(numreaches) = iplan (megreach,3)
      megreach = iplan (megreach,3)

      goto 7429
      endif 

 5090 continue

*     -------------------------------------------------------------------------[w]
*     now trim excess reaches ...                                              [w]
*     -------------------------------------------------------------------------[w]
      nstart = 0
      nfinish = 0
      istop = 0

*     -------------------------------------------------------------------------[w]
*     delete early reaches  ...                                                [w]
*     -------------------------------------------------------------------------[w]
      do 5077 ir = 1, nreach
      krr = reachcode (ir)
      if ( istop .eq. 0 .and. rname (krr) .eq. 'Dead ....' ) then
      nstart = nstart + 1	     
      goto 5077
      else
      istop = 1
      endif
      if ( istop .eq. 1 .and. rname (krr) .eq. 'Dead ....' ) then
      nfinish = nfinish + 1
      endif
 5077 continue

      nreach = nreach - nfinish 
      endif
*     **************************************************************************

 5999 continue

      call generate next reach
	
      negorder = max0 ( 1, negorder )	 !  next reach 
      do 1515 ncp = negorder, negorder

      kreach = ReachCode(ncp)

*     test for an illegal Reach Code -------------------------------------------
      do 8710 K = 1,3  
      irch = IPLAN (KREACH,K)
      if ( irch .eq. 0 ) goto 8710
      if ( irch .le. 0 .or. irch .gt. MR) then

      write(01,4772) KREACH, irch
      write(09,4772) KREACH, irch
      write(33,4772) KREACH, irch
      write( *,4772) KREACH, irch
 4772 format(/77('*')/
     &'*** ILLEGAL Reach Code in defining the sequence of Reaches'/
     &'*** Reach record affected: ',I6/
     &'*** Illegal Reach Code: ',I6,' ???'/
     &'*** Calculations stopped ...'/77('*'))
      call stop
      endif
 8710 continue

*     all reaches have been read in. Set the REACH counter ---------------------
 1515 continue

      if ( negorder .gt. 0 ) then
      do j=negorder,nreach
      kreach=ReachCode(j)
      enddo
      endif

 7373 continue

      if (NegReach.gt.0) then
      NextReach(NegReach)=0
      endif

      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      write(22,*)NumIncluded ! --------------------------------------------- SGR
      do j = 1,MP10 ! ----------------------------------------------------------
      if ( qtype(j) .ne. 4 ) then ! --------------------------------------------
      write(230+j,257)TITLE ! -------------- graph plotting ------------- Gi.CSV
      write(240+j,257)TITLE ! -------------- word reports --------------- Wi.CSV
  257 format(a70,',',',',',',',','dist',',','dist',',','dist',',',
     &'mean',',','lcl',',','ucl',',',
     &' q90',',','lcl',',','ucl',',',
     &' q95',',','lcl',',','ucl',',',
     & 'q99',',','lcl',',','ucl',',',
     &'load',',','lcl',',','ucl',',',
     &' q90',',','lcl',',','ucl',',',
     &' q95',',','lcl',',','ucl',',',
     &' q99',',','lcl',',','ucl',',',
     &'flow',',',
     &' q90',',',
     &' q95',',',
     &' q99',',',
     &'targ',',','conf',',',
     &'high',',','good',',','mod',',','poor',',','bad',',',
*     ( 1) = 'Mean from all discharges (3,12,5,39,60,61,62)'
*     ( 2) = 'Added by sewage effluents (3)'
*     ( 3) = 'Intermittent discharges of sewage (12)'
*     ( 4) = 'Added by industrial discharges (5)'
*     ( 5) = 'Added by mine waters (39)         '
*     ( 6) = 'Diffuse pollution from livestock (25)'
*     ( 7) = 'Diffuse pollution from arable (27)'
*     ( 8) = 'Highway runoff (29)'
*     ( 9) = 'Urban runoff (31)'
*     (10) = 'Atmosphere deposition (33)'
*     (11) = 'Natural background (35)'
*     (12) = 'Septic tanks (37)'
*     (13) = 'Aggregate CSOs (40)'
*     (14) = 'Aggregated STWs (42)'
*     (15) = 'Diffuse mines (46)'
*     (16) = 'Birds, boats and angling (48)'
*     (23) = 'User-named diffuse input (50)'
*     (24) = 'User-named diffuse input (52)'
*     (25) = 'User-named diffuse input (54)'
*     (26) = 'User-named diffuse input (56)'
*     (27) = 'User-named diffuse input (58)'
*     (28) = 'Other point sources (60)'
*     (29) = 'Private wastewaters (61)'
*     (30) = 'Fish farms (62)'
*      Apportionment of mean river loads =======================================
     &'LM01',',','LM02',',','LM03',',','LM04',',','LM05',',','LM06',',',
     &'LM07',',','LM08',',','LM09',',','LM10',',','LM11',',','LM12',',',
     &'LM13',',','LM14',',','LM15',',','LM16',',','LM17',',','LM18',',',
     &'LM19',',','LM20',',','LM21',',','LM22',',','LM23',',','LM24',',',
     &'LM25',',','LM26',',','LM27',',','LM28',',','LM29',',','LM30',',',
     &'LM31',',','LM32',',','LM33',',',
*      Apportionment of mean river concentrations ==============================
     &'CM01',',','CM02',',','CM03',',','CM04',',','CM05',',','CM06',',',
     &'CM07',',','CM08',',','CM09',',','CM10',',','CM11',',','CM12',',',
     &'CM13',',','CM14',',','CM15',',','CM16',',','CM17',',','CM18',',',
     &'CM19',',','CM20',',','CM21',',','CM22',',','CM23',',','CM24',',',
     &'CM25',',','CM26',',','CM27',',','CM28',',','CM29',',','CM30',',',
     &'CM31',',','CM32',',','CM33',',',
     &' obs',',','lcl',',','ucl',',', ! observed mean and confidence limits
     &' q90',',','lcl',',','ucl',',',
     &' q95',',','lcl',',','ucl',',',
     &' q99',',','lcl',',','ucl',',','change',',','lower',',','upper')

      write(230+j,252)dname(j),Units(J),QTYPE(j),MRQS(j),nreach, ! ------ Gi.CSV
     &lunits(J) ! ------------------------------------------------------- Gi.CSV
      write(240+j,252)dname(j),Units(J),QTYPE(j),MRQS(j),nreach, ! ------ Wi.CSV
     &lunits(J) ! ------------------------------------------------------- Wi.CSV
  252 format(a11,','a4,',',i6,',',i6,',',i6,',',a4,',',
     &'  7',',','  8',',','  9',',',' 10',',',' 11',',',
     &' 12',',',' 13',',',' 14',',',' 15',',',' 16',',',' 17',',',
     &' 18',',',' 19',',',' 20',',',' 21',',',' 22',',',' 23',',',
     &' 24',',',' 25',',',' 26',',',' 27',',',' 28',',',' 29',',',
     &' 30',',',' 31',',',' 32',',',' 33',',',' 34',',',' 35',',',
     &' 36',',',' 37',',',' 38',',',' 39',',',' 40',',',' 41',',',
     &' 42',',',' 43',',',' 44',',',' 45',',',' 46',',',' 47',',',
     &' 48',',',' 49',',',' 50',',',' 51',',',' 52',',',' 53',',',
     &' 54',',',' 55',',',' 56',',',' 57',',',' 58',',',' 59',',',
     &' 60',',',' 61',',',' 62',',',' 63',',',' 64',',',' 65',',',
     &' 66',',',' 67',',',' 68',',',' 69',',',' 70',',',' 71',',',
     &' 72',',',' 73',',',' 74',',',' 75',',',' 76',',',' 77',',',
     &' 78',',',' 79',',',' 80',',',' 81',',',' 82',',',' 83',',',
     &' 84',',',' 85',',',' 86',',',' 87',',',' 88',',',' 89',',',
     &' 90',',',' 91',',',' 92',',',' 93',',',' 94',',',' 95',',',
     &' 96',',',' 97',',',' 98',',',' 99',',','100',',','101',',',
     &'102',',','103',',','104',',','105',',','106',',','107',',',
     &'108',',','109',',','110',',','111',',','112',',','113',',',
     &'114',',','115',',','116',',','117',',','118',',','119',',',
     &'120',',','121',',','122',',','123')

      endif
      enddo
      endif

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      write(42,5291)nreach ! output file .GIS1.CSV -----------------------------

 5291 format('$Reaches'/
     &i4,',"Number of reaches that follow"')
      endif
      endif

*     now examine the sequence of Reaches --------------------------------------
      do 8701 ir = negorder, NREACH
      KREACH = ReachCode(ir)
      if (KREACH .gt. 0 ) then
      if ( Included(kreach) .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      write(22,221)kreach,rname(kreach),rlength(kreach), ! ----------------- SGR
     &             NextReach(kreach) ! ------------------------------------- SGR
*     DO NOT CHANGE THE FOLLOWING - PLOTTING WILL FAIL ##################### SGR
*     DO NOT CHANGE DESPITE "REMARK" ON COMPILATION ######################## SGR
  221 format(i4,',',a16,',',1pe10.4,',',i4) ! ------------------------------ SGR

      nexrch = 0
      if ( IPLAN(kreach,1) + IPLAN(kreach,3) .eq. 0 ) then
      nexrch = IPLAN(kreach,2)
      endif
      if ( IPLAN(kreach,2) + IPLAN(kreach,3) .eq. 0 ) then
      nexrch = IPLAN(kreach,1)
      endif
      if ( IPLAN(kreach,1) * IPLAN(kreach,2) .gt. 0 ) then
      nexrch = IPLAN(kreach,3)
      endif
      
      do j = 1,MP10 ! write data on Reaches for Gi.CSV and Wi.CSV ==============
      if ( qtype(j) .ne. 4 ) then
      write(230+j,251)rname(kreach),kreach,nexrch, ! -------------------- Gi.CSV
     &rlength(kreach),IPLAN(kreach,1),IPLAN(kreach,2),IPLAN(kreach,3)
      write(240+j,251)rname(kreach),kreach,nexrch, ! -------------------- Wi.CSV
     &rlength(kreach),IPLAN(kreach,1),IPLAN(kreach,2),IPLAN(kreach,3)
  251 format(',',a16,',',i4,',',i4,',',1pe11.4,',',i4,',',i4,',',i4,',')
      endif
      enddo ! do j = 1,MP10  ===================================================

      
      if ( noGIS .eq. 0 ) then
      write(42,241)kreach,rname(kreach), ! output file .GIS1.CSV ---------------
     &rlength(kreach),NextReach(kreach) ! output file .GIS1.CSV ----------------
  241 format(i4,',"',a16,'",',e11.4,',',i4,',"Number of reach, ',
     &'name of reach, length of reach, number of next reach"')
      endif
      endif
      endif
      endif

      do 8702 K = 1,3
      irch = IPLAN (KREACH,K)
      if ( irch .eq. 0 ) goto 8702
      if ( irch .le. 0 .or. irch .gt. MR) then
      if ( nobigout .le. 0 ) write(01,4772) KREACH, irch
      write(08,4772) KREACH, irch
      write(09,4772) KREACH, irch
      write(33,4772) KREACH, irch
      write( *,4772) KREACH, irch
      call stop
      endif
 8702 continue

 8701 continue

      kreach = 0

*     set indicator for the end of the simulation ------------------------------
      if ( negReach .eq. 0 ) then
        IPLAN (ReachCode(NREACH),1) = -1
      else
        IPLAN (NegReach,1) = -1
      endif

      return
      end

      
      
      
      subroutine read the data on the determinands 
      include 'COMMON DATA.FOR'

  113 NDET = NDET + 1

*     QTYPE      - type of determinand
*     DNAME      - name of determinand
*     DNA        - short name
*     UNITS      - units
*     RATE       - rate constant for exponential decay
*     QBASE      - baseline quality for extrapolation of exponential decay
*     QDIFFGAP   - quality of diffuse inflows added with gap filling
*     QZEROGAP   - baseline quality for extrapolation of exponential
*                  decay during gap filling
*     WEQ        - worst permissible effluent quality
*     BEQ        - best possible effluent quality
*     GEQ        - definition of good effluent quality
*     MRQS       - summary statistic for the water quality targets
*                  1 mean                 
*                  2 95-percentile     
*                  3 90-percentile           
*                  4 5-percentile       
*                  5 10-percentile              
*                  6 99-percentile              
*     Partishun  - partition coefficient

      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 223
      if ( NDET .gt. MP10 ) then
          NDET = NDET - 1
          call extract a line of data (line of data)
          goto 113
      endif

*     check for negative value QTYPE, and exclude this determinand -------------
      read(02,*,ERR=7509)QTYPE(NDET)
      if ( QTYPE (NDET) .lt. 0 ) then
      NDET = NDET - 1
      goto 113
      endif
      backspace(2)

*     ensure SIMCAT's array boundaries are not exceeded ------------------------
      if ( NDET .gt. MP10 ) then
      if ( nobigout .le. 0 ) write(01,6222)MP10
      write(08,6222) MP10
      write(09,6222) MP10
      write(33,6222) MP10
      if ( iscreen .lt. 3 ) write( *,6222)MP10
 6222 format(/77('-')/
     &'Too many determinands ...        '/ 
     &'The number has been cut to', I3/77('-'))
      goto 223
      endif

*     try and read the new form of determinand data ----------------------------
      call extract a line of data (line of data)

*     with summary statistic - MRQS, coefficient of variation ------------------
*     correlation coefficient and partion data -------------------- 2 more items
      read(line of data,*,END=9516)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET),
     &Partishun(ndet)  ! partition coefficient ---------------------------------
      goto 8515

*     with summary statistic - MRQS --------------------------------------------
 9516 continue
      read(line of data,*,END=8516)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET)
      Partishun(NDET) = 0.0
      goto 8515

*     try and read old form of data (without summary statistic information) ----
 8516 continue
      read(line of data,*,ERR=7509)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET)
      Partishun(NDET) = 0.0
      MRQS(NDET) = 1 ! annual mean
 8515 continue
      
      call set determinand codes (ndet)
      
      if ( abs (Partishun(NDET)) .gt. 0.0001 ) Detype (Ndet) = 909
      
      if ( Partishun(NDET) .lt. -0.0001 ) then
      Detype (NDET) = 909 ! dissolved metal
      Partishun(NDET) = abs (Partishun(NDET))
      endif
      
      NOBACK(NDET) = 0
      if ( GEQ(1,NDET) .lt. -0.000001 ) then
      NOBACK(NDET) = 99
      endif
      
*     use master data (from first batch data file )to exclude or include the ---
*     determinand from all other batch runs ------------------------------------
      if ( masterdata .eq. 1 ) then
      masterBEQ(1,NDET) = BEQ(1,NDET)
      masterGEQ(1,NDET) = GEQ(1,NDET)
      masterWEQ(1,NDET) = WEQ(1,NDET)
      masterQTYPE(NDET) = QTYPE (NDET)
      masterdetype (NDET) = detype (NDET)
      masterMRQS(NDET) = MRQS(NDET)
      endif

      if ( ifbatch .eq. 1 .and. master set used .eq. 1 ) then
      if ( model number in batch .gt. 1 ) then
      QTYPE (NDET) = masterQTYPE(NDET)
      BEQ(1,NDET) = masterBEQ(1,NDET)
      GEQ(1,NDET) = masterGEQ(1,NDET) 
      WEQ(1,NDET) = masterWEQ(1,NDET) 
      detype (NDET) = masterdetype (NDET)
      MRQS(NDET) = masterMRQS(NDET)
      endif
      endif
      
      

*     exclude unwanted determinands (QTYPE = 4) --------------------------------
      if ( QTYPE (NDET) .lt. 1 .or. QTYPE (NDET) .gt. 5) 
     &QTYPE (NDET) = 4

      if ( QTYPE (NDET) .eq. 4 ) then
      DNA(NDET) = '    '
      UNITS(NDET) = '    '
      LUNITS(NDET) = '    '
      RATE(NDET) = 0.0
      QBASE(NDET) = 0.0
      QDIFFGAP(NDET) = 0.0
      QZEROGAP(NDET) = 1.0e-12
      do jj = 1, 3
      WEQ(jj,NDET) = 0.0
      GEQ(jj,NDET) = 0.0
      BEQ(jj,NDET) = 0.0
      enddo
      MRQS(NDET) = 0 ! initialise indicator for statistic for river target -----
      endif

*     adjust MRQS for determinands where the standard must be exceeded ---------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile              
      if ( QTYPE (NDET) .eq. 03 .or. QTYPE (NDET) .eq. 05 ) then
      if ( MRQS(NDET) .eq.  2 ) MRQS(NDET) = 4 ! annual 05-percentile
      if ( MRQS(NDET) .eq.  3 ) MRQS(NDET) = 5 ! annual 10-percentile
      endif
      
      kstat2 = 0 ! register whether summary statistics have been specified -----
      do idet = 1, ndet
      if ( qtype(idet) .ne. 4 ) then
      kstat2 = kstat2 + MRQS (idet)
      endif
      enddo ! register whether summary statistics have been specified ----------

*     go back and read the next determinand ------------------------------------
      goto 113

*     all the determinands have been read --------------------------------------
  223 continue

*     set the number of determinands -------------------------------------------
      NDET = NDET - 1
      NP = NDET

*     identify the last determinand --------------------------------------------
      ndetlast = 0
      do idet = 1,ndet
      if ( QTYPE (idet) .ne. 4 ) ndetlast = idet
      enddo

*     identify the first active determinand ------------------------------------
      ndetfirst = 0
      do idet = 1,ndet
      if ( QTYPE (idet) .ne. 4 ) then
      ndetfirst = idet
      goto 5432
      endif
      enddo
 5432 continue
  
*     set the units for load ---------------------------------------------------
      do idet = 1,ndet
      if ( QTYPE (idet) .ne. 4 ) then
      lunits (idet) = 'LOAD' ! initialise --------------------------------------

      if ( funit .eq. 'TCMD' .or. funit .eq. 'Ml/d') then ! ====================
      if ( units (idet) .eq. 'mg/l' ) then
      lunits (idet) = ' T/D'
      endif
      if ( units (idet) .eq. 'ug/l' ) then
      lunits (idet) = 'KG/D'
      endif
      endif ! if ( funit .eq. 'TCMD' .or. funit .eq. 'Ml/d') ===================

      if ( funit .eq. 'm3/d' ) then ! ==========================================
      if ( units (idet) .eq. 'mg/l' ) then
      lunits (idet) = 'KG/D'
      endif
      if ( units (idet) .eq. 'ug/l' ) then
      lunits (idet) = ' G/D'
      endif
      endif ! if ( funit .eq. 'm3/d' ) =========================================

      endif
      enddo
      return

 7509 write( *,7710)
      if ( nobigout .le. 0 ) write(01,7710)
      write(08,7710)
      write(09,7710)
      write(33,7710)
 7710 format(/
     &'-------------------------------------------------------'/
     &'*** Error in reading data on determinands ....         '/
     &'-------------------------------------------------------')
      call stop
      end



*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      subroutine set the gap filling switches
      include 'COMMON DATA.FOR'

      ngorder = max0 ( 1, negorder)
      do 4144 IREACH = ngorder, NREACH ! loop on reaches

*     set the gap filling switches ---------------------------------------------
*     set the switch to "1" downstream of the last gap filling point -----------
*     and leave it at zero at and upstream of the last gap filling point -------

*     set the gap filling switches for river flow ------------------------------
      do itemp = 1, MU
      JUU = MU - itemp + 1
      if ( JREACH(JUU) .eq. IREACH ) then
      if ( iabs ( JFCAL(JUU) ) .gt. 0 ) goto 3701
      KFCAL(JUU) = 1
      endif
      enddo
 3701 continue

*     set the gap filling switches for river quality ---------------------------
      do 4147 itemp = 1, MU
      JUU = MU - itemp + 1
      KQCAL(JUU) = 0
      if (JREACH(JUU) .ne. IREACH) goto 4147
      if ( iabs ( JQCAL(JUU) ) .gt. 0) goto 4144
      KQCAL(JUU) = 1
 4147 continue
 4144 continue

      return
      end
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg




      subroutine sort out data for length profiles
      include 'COMMON DATA.FOR'

      BDIST = 0.0 ! initialise length of upstream river
      ITRAK = ReachCode (negorder) ! the first reach
      LIMB = 1

*     indicate reaches which form the main spine of the catchment --------------
*     this profile will go to the SGR file -------------------------------------
      do 8900 IREACH2 = negorder, NREACH
      IREACH = reachcode(IREACH2)
	     
*     test for a straight continuation =========================================
      if ( IPLAN(IREACH,2) .eq. 0 ) then ! a straight continuation
      if ( LIMB .eq. 0 ) then
      BDIST = 0.0 ! length of upstream river
      else
      BDIST = BDIST + RLENGTH(IREACH) ! length of upstream river
      endif ! if ( LIMB .eq. 0 )
*     set to ITRAK the number of the receiving reach ---------------------------
      ITRAK = IPLAN(IREACH,1) ! next river
*     ADIST - the length of main river upstream of start of this Reach ---------
*     used for assembling data for graphs (km) ---------------------------------
      if ( ITRAK .gt. 0 ) ADIST (ITRAK) = BDIST
      goto 8900
      endif ! straight continuation ============================================

*     test for branch to the start of a new reach===============================
      if ( IPLAN(IREACH,1) .eq. 0 ) then ! test for branch to new reach ========
      LIMB = 0
      ITRAK2 = IPLAN(IREACH,2)
      ADIST (ITRAK2) = 0.0
      goto 8900
      endif ! if ( IPLAN(IREACH,1) .eq. 0 ) ... test for branch to new reach ===

*     mixing occurs ============================================================
      INDx = IPLAN(IREACH,1)
      ITRAK = IPLAN(IREACH,3) ! the resulting reach
      LIMB = 1
      BDIST = BDIST + RLENGTH(INDx)
      if ( ITRAK .gt. 0 ) ADIST(ITRAK) = BDIST
      goto 8900
      

      INDx = IPLAN(IREACH,2)
      ITRAK = IPLAN(IREACH,3) ! resulting reach
      LIMB = 1
      BDIST = BDIST + RLENGTH(INDx)
      if ( ITRAK .gt. 0 ) ADIST (ITRAK) = BDIST

 8900 continue
      return
      end





      subroutine initialise for the run
      include 'COMMON DATA.FOR'

      call initialise QUICKWIN ! set up the screen 
      call getdat ( IYEAR, JMONTH, IDAY ) ! obtain the date
      call gettim ( IHR, IMIN, ISEC, IHUN ) ! obtain the time
*     prepare to compute the time taken for the run ----------------------------
      BIGLAPSE = IHR*3600+IMIN*60+ISEC ! count the seconds ---------------------
      ifbatch = 1
      krite1 = 0
      ifeffcsv = 1 ! set to 1 to switch on csv output for effluents by -- Ei.CSV
      Grand river length = 0.0 ! over all batch runs ----
      model number in batch = 0

      do i = 1, 500
      if ( i .ne. 6 ) close (i)
      enddo
      
      return
      end


      subroutine close down
      include 'COMMON DATA.FOR'
      logical exists

      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( exists ) then  
      open(488,FILE = Globaldataname, STATUS = 'OLD')
      close (848, status='delete')    
      endif

      call gettim ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP3 = LAP2-BIGLAPSE
      if ( LAP3 .lt. 0 ) LAP3 = LAP3 + 86400
      IMIN2 = LAP3/60
      ISEC2 = LAP3-60*IMIN2

      isupprezz = suppress1 + suppress3 + suppress4
     &+ suppress5  + suppress6  + suppress7  + suppress8  + suppress9
     &+ suppress10 + suppress11 + suppress12 + suppress13 + suppress14
     &+ suppress16 + suppress17 + suppress18 + suppress19
     &+ suppress20 + suppress00 + suppress9a + suppress9b
     &+ suppress15

      call change colour of text (35) ! white
      !write( *,4974)
 4974 format(130('-'))
      if ( ifbatch .eq. 0 .and. need works .gt. NUED ) then
      call change colour of text (12) ! orange
      write( *,4531)NUED,need works,kill works
 4531 format('*** Too many discharges for back-tracking',10x,
     &'...',7x,'the maximum is ',i5,' (need',i5,')',12x,
     &'(ignored',i5,')')
      call set screen text colour
      write(08,4531) NUED,need works,kill works
      endif
      
      if ( ifbatch .eq. 1) then ! ++++++++++++++++++++++++++++++++++++++++++++++
      if ( IDIFFREACH .eq. 0 ) then ! ==========================================
      call change colour of text (12) ! orange ! ===============================
      write( *,8492) ! =========================================================
 8492 format(/'*** Diffuse inflows have been suppressed for Reaches', ! ========
     &' by the switch set in the DAT file ... ') ! =============================
      endif ! if ( IDIFFREACH .eq. 0 ) =========================================
      endif ! if ( ifbatch .eq. 1) +++++++++++++++++++++++++++++++++++++++++++++

      if ( ifbatch .eq. 1) then ! ++++++++++++++++++++++++++++++++++++++++++++++
      call change colour of text (49) ! dull blue
      !write( *,4972)Grand river length 
 4972 format('Total length =',f11.1,' kilometres')
      call change colour of text (10) ! green
      write( *,45)imin2,isec2
   45 Format(/'All runs completed ... ',
     &'Close this window to return to the main screen ...  ',
     &' Lapsed time:',i6,' minutes and',i3,' seconds .........')
      else ! ifbatch .ne. 1
      call change colour of text (10) ! green
      write( *,25)
   25 Format('Calculations completed ... ',
     &'Close this window to return to the main screen ...'/
     &'The ERR (and OUT and SCN) files contain comments on ',
     &'the DATA ...')
      call change colour of text (20) ! dull magenta (36)
      endif ! if ( ifbatch .eq. 1) +++++++++++++++++++++++++++++++++++++++++++++

      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '

      call message of completion ! from subroutine close down

      call close down all the output files ! from subroutine close down

      do while (.TRUE.)
      end do
      end
      
      
      subroutine pause
      include 'COMMON DATA.FOR'
      
      call change colour of text (10) ! green
      pause 'Press "Enter" to continue ..........'
      write(*,*)'Continuing ...'
      call set screen text colour

      return
      end
      
      
      subroutine stop
      include 'COMMON DATA.FOR'
      logical exists

      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( exists ) then  
      open(488,FILE = Globaldataname, STATUS = 'OLD')
      close (488, status='delete')    
      endif
      
      call close down all the output files ! used for write read data ----------

      call gettim ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP3 = LAP2-BIGLAPSE
      if ( LAP3 .lt. 0 ) LAP3 = LAP3 + 86400
      IMIN2 = LAP3/60
      ISEC2 = LAP3-60*IMIN2

      write( *,4974) ! write in green 
 4974 format(130('-'))
      if ( ifbatch .eq. 1) then ! for a set of batch runs ----------------------
      call change colour of text (11) ! bright turquoise blue
      write( *,4972)Grand river length
 4972 format('Total length =',f11.1,' kilometres')
      call change colour of text (12) ! orange
      write( *,45)imin2,isec2
   45 Format('Batch run of calculations halted ... ',
     &'Close this window to return to the main screen ...  ',
     &' Lapsed time:',i6,' minutes and',i3,' seconds .........')
      else ! non-batch runs ----------------------------------------------------
      call change colour of text (12) ! orange
      write( *,25)
   25 Format('Calculation halted ... ',
     &'close this window to return to the main screen ...'/
     &'The ERR (and OUT and SCN) files contain comments on ',
     &'the data ...')
      endif ! if ( ifbatch .eq. 1) for a set of batch runs ---------------------
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '

      call message of completion ! from subroutine stop
      do ichan = 1,500
      close (ichan) 
      enddo

      do while (.TRUE.)
      end do

      end
     
     
      subroutine message of completion
      include 'COMMON DATA.FOR'

      write (fname,99) datname
      call trunk ('E','N','D')
      open(909,FILE=FNAME) ! ---------------------------------------------- .END
   99 format(a136)

      write(909,1063)bday,bmon,IYEAR,BH,bmin ! 222222222222222222222222222222222
 1063 format(77('-')/ ! heading 222222222222222222222222222222222222222222222222
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7x, ! 22222222222
     &'Date: ',a2,'/',a2,'/',I4/'Version 161  ', ! 22222222222222222222222222222
     &'(Tony Warn 30/10/23)',18x,'...', ! 22222222222222222222222222222222222222
     &5x,'  Time:  ',4x,a2,'.',a2) ! 2222222222222222222222222222222222222222222
      write(909,1064)
 1064 format(77('-')/'Indicator that the run has finished ...'/
     &77('#'))
      close (909)
	
      return
      end


      subroutine write headings for mode 7
      include 'COMMON DATA.FOR'

      if ( iscreen .lt. 3 ) then
*     check for the existence of a file for flow gap filling -------------------
      if ( no gap filling 78 .eq. 0 ) then
*     it exists ----------------------------------------------------------------
      write( *,6677)ICAL
      write( *,5621)
      else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
      write( *,6777)ICAL
      endif
      endif

      if ( nobigout .le. 0 ) then ! ============================================
*     check for the existence of a file for flow gap filling -------------------
      if ( no gap filling 78 .eq. 0 ) then ! ===================================
*     it exists ----------------------------------------------------------------
      write(01,6677)ICAL
      write(03,6677)ICAL
      write(09,6677)ICAL
      write(33,6677)ICAL
 6677 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      do ic = 1, ndet
      if (qtype(ic) .ne. 4 ) write(200+ic,6777)ICAL
      enddo

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      write(01,5621)
      write(03,5621)
      write(09,5621)
      write(33,5621)
 5621 format(12x,'Existing gap-filling files will be used ...'/
     &77('='))
      do ic = 1, ndet
      if (qtype(ic) .ne. 4 ) write(200+ic,5621)ICAL
      enddo
*     ---------------------------------------------------------------- it exists
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
      write(01,6777)ICAL
      write(03,6777)ICAL
      write(09,6777)ICAL
      write(33,6777)ICAL
 6777 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      do ic = 1, ndet
      if (qtype(ic) .ne. 4 ) write(200+ic,6777)ICAL
      enddo
      endif ! if ( no gap filling 78 .eq. 0 ) ==================================
*     -------------------------------------------------------- it does not exist 
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     munthly structure --------------------------------------------------------
      do ic = 1, ndet ! ========================================================
      if ( QTYPE (ic) .ne. 4 ) then ! ==========================================
*     check for the existence of a file for flow gap filling -------------------
      if ( no gap filling 78 .eq. 0 ) then
*     it exists ----------------------------------------------------------------
      write(100+ic,6677)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6677)ICAL ! ------------------------------------------ Ai.ADL
      write(150+ic,6677)ICAL ! ------------------------------------------ Di.EFF
      write(140+ic,6677)ICAL ! ------------------------------------------ Ai.ADC
      write(170+ic,6677)ICAL ! ------------------------------------------ Ci.GAP
      write(100+ic,5621) ! ---------------------------------------------- Di.MON
      write(120+ic,5621) ! ---------------------------------------------- Ai.ADL
      write(150+ic,5621) ! ---------------------------------------------- Di.EFF
      write(140+ic,5621) ! ---------------------------------------------- Ai.ADC
      write(170+ic,5621) ! ---------------------------------------------- Ci.GAP
      else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
      write(100+ic,6777)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6777)ICAL ! ------------------------------------------ Ai.ADL
      write(150+ic,6777)ICAL ! ------------------------------------------ Di EFF
      write(140+ic,6777)ICAL ! ------------------------------------------ Ai.ADC
      write(170+ic,6677)ICAL ! ------------------------------------------ Ci.GAP
      endif ! ------------------------------------------------ it does not exist 
      endif ! if ( QTYPE (ic) .ne. 4 ) =========================================
      enddo ! do ic = 1, ndet ! ================================================
      endif ! if ( nobigout .le. 0 ) ===========================================

*     check for the existence of a file for flow gap filling -------------------
      if ( no gap filling 78 .eq. 0 ) then ! it exists -------------------------
      write(27,6677)ICAL
      write(30,6677)ICAL
      write(31,6677)ICAL ! ------------------------------------------------- EFF
      write(48,6677)ICAL
      write(72,6677)ICAL
      else ! "no gap filling 78" equals 1 --------------------------------------
      write(27,6777)ICAL
      write(30,6777)ICAL
      write(31,6777)ICAL
      write(48,6777)ICAL
      write(72,6777)ICAL
      endif ! ------------------------------------------------------------------

      return
      end
 
 
      
      subroutine write headings for mode 8
      include 'COMMON DATA.FOR'
      
      if ( iscreen .lt. 3 ) then
      if ( no gap filling 78 .eq. 0 ) then
      write( *,6678)ICAL
      write( *,5621)
      else ! "no gap filling 78" equals 1
      write( *,6778)ICAL
      endif
      endif ! if ( iscreen .lt. 3 )
 
      if ( nobigout .le. 0 ) then
      if ( no gap filling 78 .eq. 0 ) then
      write(01,6678)ICAL
      write(03,6678)ICAL
      write(09,6678)ICAL
      write(33,6678)ICAL
 6678 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      write(01,5621)
      write(03,5621)
      write(09,5621)
      write(33,5621)
 5621 format(12x,'Existing gap-filling files will be used ...'/
     &77('='))
      else ! "no gap filling 78" equals 1
      write(01,6778)ICAL
      write(03,6778)ICAL
      write(09,6778)ICAL
      write(33,6778)ICAL
 6778 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      endif ! if ( no gap filling 78 ... 

      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      if ( no gap filling 78 .eq. 0 ) then
      write(100+ic,6678)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6678)ICAL ! -----------------------------------+++++++ Ai.ADL
      write(150+ic,6678)ICAL ! ------------------------------------------ Di EFF
      write(140+ic,6678)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(170+ic,6678)ICAL ! -----------------------------------+++++++ Ci.GAP
      write(100+ic,5621) ! ---------------------------------------------- Di.MON
      write(120+ic,5621) ! ---------------------------------------+++++++ Ai.ADL
      write(150+ic,5621) ! ---------------------------------------------- Di EFF
      write(140+ic,5621) ! ---------------------------------------+++++++ Ai.ADC
      write(170+ic,5621) ! ---------------------------------------+++++++ Ci.GAP      
      else ! "no gap filling 78" equals 1
      write(100+ic,6778)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6778)ICAL ! -----------------------------------+++++++ Ai.ADL
      write(150+ic,6778)ICAL ! ------------------------------------------ Di EFF
      write(140+ic,6778)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(170+ic,6678)ICAL ! -----------------------------------+++++++ Ci.GAP
      endif
      endif
      enddo ! do ic = 1, ndet
      endif ! if ( nobigout .le. 0 )

      if ( no gap filling 78 .eq. 0 ) then
      write(27,6678)ICAL
      write(30,6678)ICAL
      write(31,6678)ICAL
      write(48,6678)ICAL
      write(72,6678)ICAL
      else ! "no gap filling 78" equals 1
      write(27,6778)ICAL
      write(30,6778)ICAL
      write(31,6778)ICAL
      write(48,6778)ICAL
      write(72,6778)ICAL
      endif

      return
      end



    
      subroutine write headings for mode 9
      include 'COMMON DATA.FOR'
      
      if ( iscreen .lt. 3 ) then
      if ( no gap filling 78 .eq. 0 ) then
      write( *,6678)ICAL
      write( *,5621)
      else ! "no gap filling 78" equals 1
      write( *,6778)ICAL
      endif
      endif ! if ( iscreen .lt. 3 )
 
      if ( nobigout .le. 0 ) then
      if ( no gap filling 78 .eq. 0 ) then ! ===================================
      write(01,6678)ICAL
      write(03,6678)ICAL
      write(09,6678)ICAL
      write(33,6678)ICAL
 6678 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      write(01,5621)
      write(03,5621)
      write(09,5621)
      write(33,5621)
 5621 format(12x,'Existing gap-filling files will be used ...'/
     &77('='))
      else ! "no gap filling 78" equals 1
      write(01,6778)ICAL
      write(03,6778)ICAL
      write(09,6778)ICAL
      write(33,6778)ICAL
 6778 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
      endif ! if ( no gap filling 78 ===========================================

      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      if ( no gap filling 78 .eq. 0 ) then
      write(100+ic,6678)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6678)ICAL ! -----------------------------------+++++++ Ai.ADL
      write(150+ic,6778)ICAL ! ------------------------------------------ Di.EFF
      write(140+ic,6678)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(170+ic,6678)ICAL ! -----------------------------------+++++++ Ci.GAP
      write(100+ic,5621) ! ---------------------------------------------- Di.MON
      write(120+ic,5621) ! ---------------------------------------+++++++ Ai.ADL
      write(150+ic,5621) ! ---------------------------------------------- Di.EFF
      write(140+ic,5621) ! ---------------------------------------+++++++ Ai.ADC
      write(170+ic,5621) ! ---------------------------------------+++++++ Ci.GAP
      else ! "no gap filling 78" equals 1
      write(100+ic,6778)ICAL ! ------------------------------------------ Di.MON
      write(120+ic,6778)ICAL ! -----------------------------------+++++++ Ai.ADL
      write(150+ic,6778)ICAL ! ------------------------------------------ Di.EFF
      write(140+ic,6778)ICAL ! -----------------------------------+++++++ Ai.ADC
      write(170+ic,6678)ICAL ! -----------------------------------+++++++ Ci.GAP
      endif
      endif ! if ( QTYPE (ic) .ne. 4 )
      enddo ! do ic = 1, ndet
      endif ! if ( nobigout .le. 0 )

      if ( no gap filling 78 .eq. 0 ) then
      write(27,6678)ICAL
      write(30,6678)ICAL
      write(31,6678)ICAL
      write(48,6678)ICAL
      write(72,6678)ICAL
      else ! "no gap filling 78" equals 1
      write(27,6778)ICAL
      write(30,6778)ICAL
      write(31,6778)ICAL
      write(48,6778)ICAL
      write(72,6778)ICAL
      endif

      return
      end


      subroutine check summary statistic for standard (ldet)
      include 'COMMON DATA.FOR'
      
      if ( ical13 .eq. 1 ) return ! running in gap-filling mode ----------------

      if ( qtype (ldet) .eq. 4 ) return
      
      if ( MRQS(ldet) .lt. 1 .or. MRQS(ldet) .gt. 6 ) then
      suppress00 = suppress00 + 1 ! no statistic for standards
      call change colour of text (22) ! light blue
      
      if ( detype (ldet) .eq. 103 .or. detype (ldet) .eq. 101 ) then
      write( *,1)DNAME(ldet)
    1 format('*** No statistic defined for standards for',9x,'...',
     &7x,a11,27x,'90-percentile assumed ...')
      call set screen text colour
      write(09,2)DNAME(ldet)
      write(01,2)DNAME(ldet)
      write(33,2)DNAME(ldet)
    2 format(77('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'90-percentile assumed by default ...'/77('-'))
      MRQS(ldet) = 3 ! 90-percentile
      return
      endif
      
      if ( detype (ldet) .eq. 104 ) then
      write( *,5)DNAME(ldet)
    5 format('*** No statistic defined for standards for',9x,'...',
     &7x,a11,33x,'10-percentile assumed ...')
      call set screen text colour
      write(33,6)DNAME(ldet)
      write(09,6)DNAME(ldet)
      write(01,6)DNAME(ldet)
    6 format(77('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'10-percentile assumed by default ...'/77('-'))
      MRQS(ldet) = 5 ! 10-percentile
      return
      endif

      if ( detype (ldet) .eq. 106 ) then
      write( *,7)DNAME(ldet)
    7 format('*** No statistic defined for standards for',9x,'...',
     &7x,a11,33x,'95-percentile assumed ...')
      call set screen text colour
      write(09,8)DNAME(ldet)
      write(01,8)DNAME(ldet)
      write(33,8)DNAME(ldet)
    8 format(77('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'95-percentile assumed by default ...'/77('-'))
      MRQS(ldet) = 2 ! 95-percentile
      return
      endif
      
      write( *,3)DNAME(ldet)
    3 format('*** No statistic defined for standards for',9x,'...',
     &7x,a11,33x,'Annual mean assumed .....')
      call set screen text colour
      write(09,4)DNAME(ldet)
      write(01,4)DNAME(ldet)
      write(33,4)DNAME(ldet)
    4 format(77('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'Annual mean assumed by default ...'/77('-'))
      MRQS(ldet) = 1 ! annual mean
      endif
      
      return
      end
      
      
      
      subroutine summarise errors
      include 'COMMON DATA.FOR'
      
      write(08,30)nreach,MU,NDET,NMKF,int(float(NMKQ)/ndet),kountworks,
     &NMKE,NS
   30 format(///48('-')/'Summary of the numbers of sets of data ...'/
     &48('-')/
     &'Reaches:                       ',i7/
     &'Features:                      ',i7/
     &'Number of pollutants:          ',I7/
     &'Sets of data on river flow:    ',i7/
     &'Sets of data for river quality:',i7/
     &'Number of point discharges:    ',I7/
     &'Sets of data for discharges:   ',i7/
     &'Number of Monte Carlo shots:   ',i7/
     &48('-')//)

      
      write(08,3)isupprezz ! ----------------------------------------------- INP
    3 format(//100('-')/'* Total number of warnings about data     ',
     &16x,I10/100('-'))
      
      write(33,33)isupprezz ! ---------------------------------------------- ERR
   33 format(/'* Total number of warnings about data     ',16x,I10)
      if ( suppress1 .gt. 0 ) then
      write(08,4)suppress1
    4 format('* The sequence of features (1)            ',16x,I10)
      write(33,4)suppress1
      endif
      if ( suppress3 .gt. 0 ) then
      write(08,5)suppress3
    5 format('* Huge change in flow (3)                 ',16x,I10)
      write(33,5)suppress3
      endif
      if ( suppress4 .gt. 0 ) then
      write(08,6)suppress4
    6 format('* No determinands for classification (4)  ',16x,I10)
      write(33,6)suppress4
      endif
      if ( suppress5 .gt. 0 ) then
      write(08,7)suppress5
    7 format('* Error reading flow gap filling data (5) ',16x,I10)
      write(33,7)suppress5
      endif
      if ( suppress6 .gt. 0 ) then
      write(08,8)suppress6
    8 format('* Zero 95-percentile flow set to 1% (6)   ',16x,I10)
      write(33,8)suppress6
      endif
      if ( suppress7 .gt. 0 ) then
      write(08,9)suppress7
    9 format('* Unnecessary monthly structures (7)      ',16x,I10)
      write(33,9)suppress7
      endif
      if ( suppress8 .gt. 0 ) then
      write(08,10)suppress8
   10 format('* Quality data not set for gap fill (8)   ',16x,I10)
      write(33,10)suppress8
      endif
      if ( suppress9 .gt. 0 ) then
      write(08,11)suppress9
   11 format('* Problems in the monthly structure (9)   ',16x,I10)
      write(33,11)suppress9
      endif
      if ( suppress9a .gt. 0 ) then
      write(08,12)suppress9a
   12 format('* Problems in the monthly temperature (9a)',16x,I10)
      write(33,12)suppress9a
      endif
      if ( suppress9b .gt. 0 ) then
      write(08,13)suppress9b
   13 format('* Problems in the monthly sus.solids (9b) ',16x,I10)
      write(33,13)suppress9b
      endif
      if ( suppress10 .gt. 0 ) then
      write(08,14)suppress10
   14 format('* Illegal data for river flow (10)        ',16x,I10)
      write(33,14)suppress10
      endif
      if ( suppress11 .gt. 0 ) then
      write(08,15)suppress11
   15 format('* Flow exceeds a billion (11)             ',16x,I10)
      write(33,15)suppress11
      endif
      if ( suppress14 .gt. 0 ) then
      write(08,16)suppress14
   16 format('* Infeasibly huge concentration (14)      ',16x,I10)
      write(33,16)suppress14
      endif
      if ( suppress15 .gt. 0 ) then
      write(08,45)suppress15
   45 format('* Problems with power-curve loada (15)    ',16x,I10)
      write(33,45)suppress15
      endif
      if ( suppress12 .gt. 0 ) then
      write(08,17)suppress12
   17 format('* Problems with monthly structure (12)    ',16x,I10)
      write(33,17)suppress12
      endif
      if ( suppress13 .gt. 0 ) then
      write(08,18)suppress13
   18 format('* Zero 95-percentile river flow (13)      ',16x,I10)
      write(33,18)suppress13
      endif
      if ( suppress17 .gt. 0 ) then
      write(08,19)suppress17
   19 format('* Deleted feature (17)                    ',16x,I10)
      write(33,19)suppress17
      endif
      if ( suppress18 .gt. 0 ) then
      write(08,20)suppress18
   20 format('* Zero variation in river quality (18)    ',16x,I10)
      write(33,20)suppress18
      endif
      if ( suppress16 .gt. 0 ) then
      write(08,21)suppress16
   21 format('* Unneeded effluent data ignored (16)     ',16x,I10)
      write(33,21)suppress16
      endif
      if ( suppress19 .gt. 0 ) then
      write(08,22)suppress19
   22 format('* Unneeded data ignored (19)              ',16x,I10)
      write(33,22)suppress19
      endif
      if ( suppress20 .gt. 0 ) then
      write(08,23)suppress20
   23 format('* Infeasible correlation coefficients (20)',16x,I10)
      write(33,23)suppress20
      endif
      if ( suppress21 .gt. 0 ) then
      write(08,24)suppress21
   24 format('* No data for gap filling (21)            ',16x,I10)
      write(33,24)suppress21
      endif
      if ( suppress22 .gt. 0 ) then
      write(08,25)suppress22
   25 format('* Data cannot be used to add diffuse ',
     &'pollution headwaters (22)',I6)
      write(33,25)suppress22
      endif
      if ( suppress00 .gt. 0 ) then
      write(08,26)suppress00
   26 format('* All the others (00)                     ',16x,I10)
      write(33,26)suppress00
      endif
      
      write(33,27)maxistor
   27 format(/'* Number of stored reaches                ',16x,I10)
      maxistor = 0
      
      if ( isupprezz .gt. 0 ) write(08,35)
   35 format(100('-'))

      return
      end
      
      
      subroutine write headings for the effluent CSV file
      include 'COMMON DATA.FOR'
   
      write(36,1) ! write headings for the file ..........,,,,,......... EFF.CSV
    1 format(
     &'Run_Type',',',
     &'Run_Description',',',
     &'Feature_Name',',',
     &'Feature_Number',',',
     &'Reach_Name',',',
     &'Reach_Number',',',
     &'GIS_ref',',',
     &'Dis_Type_Name',',',
     &'Dis_Type_No',',',
     &'Det_Name',',',
     &'Det_Units',',',
     &'Det_Number',',',
     &'Target',',',
     &'Stat_Name',',',
     &'Target_type_No',',',
      
     &'Riv_Fmean_up',',', 
     &'Riv_F95_up',',',

     &'Riv_Qual_mean_up',',', 
     &'Riv_Qual_sd_up',',',
     &'Riv_CF_Corr',',', 
     &'Riv_Load_mean_up',',',
     &'Riv_Load_st_up',',', 

     &'Dis_FMean',',', 
     &'Dis_Fsd',',',
     &'Dis_Ff_Corr',',',
     &'Dis_F_dist_type',',',

     &'Dis_Qual_mean_orig',',', 
     &'Dis_Qual_sd_orig',',',
     &'Dis_qual_q95_orig',',',

     &'Dis_cf_Corr',',',  
     &'Dis_c_dist_type',',', 

     &'Dis_Conc_mean_orig',',',
     &'Dis_Conc_sd_orig',',',
*    &'Dis_Conc_q95_orig',',',
     &'Dis_Load_mean_orig',',',
     &'Dis_Load_st_orig',',',

     &'Riv_Fmean_down',',', 
     &'Riv_F95_down',',',
     &'Riv_Qual_mean_down',',',
     &'Riv_Qual_sd_down',',',
     &'Riv_Qual_q90_down',',',
     &'Riv_Qual_q95_down',',',
     &'Riv_Qual_q99_down',',',
     &'Riv_Load_mean_down_forw',',',
     &'Riv_Load_st_down_forw',',',

     &'Res_type_code',',',
     &'Res_type_desc',',',
     &'Discharge_option',',',
     &'Outcome',',',
     &'Discharge_decision',',',

     &'Riv_Qual_mean_back',',',
     &'Riv_Qual_sd_back',',',
     &'Riv_Qual_q90_back',',',
     &'Riv_Qual_q95_back',',',
     &'Riv_Qual_q99_back',',',
     &'Riv_Load_mean_down_back',',',
     &'Riv_Load_st_down_back',',',

     &'Dis_Conc_mean_final',',',
     &'Dis_Conc_sd_final',',',
     &'Dis_Conc_95_final',',',
     &'Dis_Load_mean_final',',',
     &'Dis_Load_st_final',',',
     &'Dis_Load_mean_drop',',',
     &'Riv_Load_mean_drop')
      
      return
      end
      
      
      subroutine create_directory (newDirPath)
    ! Author:  Jess Vriesema
    ! Date:    Spring 2011
    ! Purpose: Creates a directory at ./newDirPath

      implicit none
      character(len=*), intent(in) :: newDirPath
      character(len=256)           :: mkdirCmd
      logical                      :: dirExists

*     Check if the directory exists first
*     inquire( file=trim(newDirPath)//'/.', exist=dirExists )  ! Works with gfortran, but not ifort
      inquire( directory=newDirPath, exist=dirExists )         ! Works with ifort, but not gfortran

      if (dirExists) then
      write ( *,*) "Directory already exists: '"//trim(newDirPath)//"'"
      else
      mkdirCmd = 'mkdir -p '//trim(newDirPath)
      write( *,'(a)') "Creating new directory: '"//trim(mkdirCmd)//"'"
      call system( mkdirCmd )
      endif
      end ! sub-routine create_directory