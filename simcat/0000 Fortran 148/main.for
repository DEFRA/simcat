*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     Version 14.9  - 11/11/14 -------------------------------------------------
*     ==========================================================================
*     Summary of the FORTRAN code. 4000 lines in this file ---------------------
*     441 Sub-routines and 53 Functions in 31 files ----------------------------
*     72000 lines of code including 11550 comments -----------------------------
*     ==========================================================================
*     This is the file MAIN.FOR ------------------------------------------------
*     It hold the program that reads and checks the data -----------------------
*     ==========================================================================
      program SIMCAT
      include 'COM.FOR' ! insert the common block 
	character *01 a(10)
*     ==========================================================================
*     some of the input and output channels ------------------------------------
*     ==========================================================================
*     ..01   main and detailed output:                   filename.OUT
*     ..02   main data file:                                     .DAT
*     ..03   summary output and report on gap filling:           .SUM
*     ..08   input data:                                         .INP
*     ..09   copy of screen output:                              .SCN
*     ..22   results for graph plotting (VISUAL BASIC):          .SGR
*     ..27   results on loads:                                   .LOD
*     ..30   results on compliance with target (Batch runs):     .TGT 
*     ..33   record of possible errors found:                    .ERR 
*     ..34   information on natural purification:                .PUR 
*     ..35   output on water quality planning:                    WQP
*     ..36                                                       .CSV
*     ..37   output on water quality planning:                    WQC
*     ..38                                                        AAA
*     ..40   results on apportionment                            .APT 
*     ..42   output for GIS                                      .GIS
*     ..44   output for GIS                                      .GI2
*     ..48   output output on classification:                    .WFD
*     ..64   output on costs                                     .COS
*     ..72   output on river flows                               .FLO
*     ..74   working file for flow gap filling:                  .FCL
*     ..75   working file for quality gap filling:               .QCL
*     ..23   RUNDAT.TMP (names of files passed to SIMCAT)        .TMP
*     ==========================================================================

*     controls for version numbers stated in headings ++++++++++++++++++++++++++ 
      n146 = 2 ! for the last general release +++++++++++++++++++++++++++++ 14.7
      n147 = 1 ! introduction of the effluent data file +++++++++++++++++++ 14.8
      n148 = 0 ! apportionment for percentiles ++++++++++++++++++++++++++++ 14.9
      n149 = 0 ! parametric replacement for non-parametric distributions  + 14.9
      n150 = 1 ! add Mode 9 (simulate EA calculations) +++++++++++++++++++++++++
      n151 = 1 ! update to Run Type 8 +++++++++++++++++++++++++++++++++++ + 14.8
      if ( n147 .eq. 1 ) n146 = 3 ! +++++++++++++++++++++++++++++++++++++++ 14.8
      if ( n147 .eq. 1 .and. n148 .eq. 1 ) n146 = 4 ! +++++++++++++++++++++ 14.9
      ndshot = 5 ! determinand to be apportioned for percentiles +++++++++++++++
      
      call initialise for the run
      open(23,file='RUNDAT.TMP')
	read(23,*)Line of data
*     if "ifbatch" stays at 1 then this is a batch run -------------------------
	if ( Line of data .eq. 'SINGLE' ) ifbatch = 0
*     initialise the starting values for tracking compliance with targets in ---
*     batch mode ... and initialise total loads and total flows ----------------
      call initial track
      call switches and data ! set the values of the switches ------------------
      read(23,230,END=5501)folder
      call attach SIMCAT data file for catchment
      backspace 23
      backspace 23
      call use replacement switches

      if ( ifbatch .eq. 1 ) then ! this is a batch run
      call write batch heading
	if ( master set used .eq. 1 ) then
	call change colour of text (49) ! dull blue
      krite1 = 1
	datfilename = datname
	call lose folder
      write( *,1110) datfilename
 1110 format('The first DAT file ',
     &'is used to guide the rest       ...'6x,a40)
      endif
      endif ! if ( ifbatch .eq. 1 )
      if ( kswitch .eq. 1 ) then
	call change colour of text (22) ! light blue
      write( *,7963)'Switches.GPD' 
 7963 Format('Used the supporting file on data switches',10x,
     &'... ',6x,a12)
      call set screen text colour
      endif ! if ( kswitch .eq. 1 ) 

*     ##########################################################################
*     start the run of the model ----------------------------------------------- 
*     or in batch mode ... start the next model (catchment) run in the batch --- 
*     of models .. and come back to this point to start the next model run in -- 
*     the batch sequence -------------------------------------------------------
*     ##########################################################################
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

*     ##########################################################################
*     start the cycle (sequence) of runs for this model in the batch ...........
*     and loop through the gap filling options where these were requested ......
*     ##########################################################################

*     read in the name of the folder holding the data files --------------------
      read(23,230,END=5501)folder
  230 format(a100)
      call attach SIMCAT data file for catchment

*     --------------------------------------------------------------------------
*     prepare to read the value of ICAL ----------------------------------------
*     this dictates the form of the calculation done for this data-file...
*     --------------------------------------------------------------------------
*     === 0 ... mass balance of input data...
*     === 1 ... compute changes to river flows giving exact fit...
*     === 2 ... apply computed changes to flows (from ICAL=1)...
*               extrapolating and interpolating...
*     === 3 ... as 2 but also computing the changes in quality
*               needed fo give a perfect fit...
*     === 4 ... Apply computed changes to flow (ICAL=1) and
*               computed changes to quality (from ICAL=3)...
*               extrapolating and interpolating...
*               Standard run for "What-if" simulations...
*               or "trial-and-error' assessments of options to change
*               river quality...
*     === 5 ... run 1 followed by run 3 ...
*     === 6 ... run 1 followed by runs 3 and 4 ...
*     === 7 ... compute effluent standards needed to meet river targets...
*     === 8 ... as 7 but with more emphasis on no deterioration ...
*     === 9 ... as 8 but using mid-point of target class as upstream quality ---
*     --------------------------------------------------------------------------
      read(23,24,ERR=7199)ical ! read the value of ICAL from RUNDAT.TMP --------
   24 format(i3)
      call check ical is legal etc

      if ( ifbatch .eq. 1 ) call read the global data file
      call check for a special file of standards
      
      kcycle = ICAL
*     suppress gap filling in modes 7 and 8 ------------------------------------
      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then
      if ( kcycle .eq. 6 ) kcycle = 4
      endif ! --------------------------------------------------------------
      
      call attach files and sort out gap filling

*     perform the next cycle in the sequence of runs for this particular DAT ---
*     (re-)read the title for this model (catchment) from the DAT file ---------
 6009 call check first four characters of a line of data (IFIN)
      read(02,9913) TITLE
 9913 format(A70)
	call leadblanks
      call check first four characters of a line of data (IFIN)

*     prepare for next (gap filling) cycle for this model ----------------------
*     the value of "jcycle" was set to at zero ---------------------------------
      jcycle = jcycle + 1
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      kerror = 0
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
      
      call set screen text colour

*     variable for suppressing calculation of confidence intervals -------------
*     set to 1 for suppression -------------------------------------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)nocon

*     switch to remove output of tables of input data --------------------------
*     set to 1 to remove tables ------------------------------------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)No tables

*     variable to suppress output ----------------------------------------------
*     set to 0 to retain all the output ----------------------------------------
*     set to 1 to exclude output on non-effluent features ----------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)IPRINT
      
*     switch for mean or percentile modes --------------------------------------
*        0 - 95-percentile output to screen
*        1 - mean output to screen
*        2 - 90-percentile output to screen and graphs
*        3 - 99-percentile output to screen and graphs
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)output mode
	If ( output mode .gt. 3 ) output mode = 1

      ical13 = 1
      if ( ical .ne. 1 .and. ical .ne. 3 ) then
      ical13 = 0
      write(22,*) TITLE ! write to the SGR file --------------------------------
      write(22,*) output mode ! write to the SGR file --------------------------
      endif
      
*     request the imposition of a monthly structure onto annual data -----------
*     will be switched to 1 anyway if type 8 data are entered ------------------
*     switched to 0 if NS not a multiple of 365 (and there are no type 8 data) -
      munthly structure = 1 ! starting value



      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)NS ! number of shots --------------------------------
      call check the number of shots
      call check first four characters of a line of data (IFIN)
      call read temperature data
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)FUNIT ! read the units for flow ---------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)IDIFF ! read the switch for diffuse sources ---------
      IDIFF = MIN(1,MAX(0,IDIFF))
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)IPUR ! read the switch for natural purification -----
      IPUR = MIN(1,MAX(0,IPUR))
*     switch for gap filling ---------------------------------------------------
      call check first four characters of a line of data (IFIN)
      read(02,*,ERR = 7500)KINT
      KINT = MAX(0,KINT)

      call initialise variables
      call reset random number starters
      call read the data on the determinands 
      call create extra data files ! create data files for GIS output, etc ----
      call check the number of shots 2
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
      call write GIS descriptors
      call read river flows
      call write river flows
      call read data on river quality (MV)
      call river quality correlation
      call write data on river quality (MV)
      call read effluent data
      call deal with non standard correlation for effluent flow
      call deal with non standard correlation for effluent quality
      call write effluent data
      call read river targets
      call write river targets
      call write reach targets
      call read the features
      call write the features
      call set the gap filling switches
      call sort out data for length profiles ! needed for plotting -------------

      call start the main calculations ! start the main calculations -----------

*     test for repeats runs on this particular model (data file) along the -----
*     gap filling sequence for flow and quality --------------------------------
*     that is where ICAL was set to 5 or 6 -------------------------------------
*     go to 9009 if this sequence is is finished for this model ... ------------

*     test for a one-cycle option ( where "kcycle" does not equal to 5 or 6 ) --
      if ( kcycle .ne. 5 .and. kcycle .ne. 6 ) goto 9009

*     continue with the other options (ical was set to 5 or 6) -----------------
      if ( jcycle .eq. 0 ) goto 9009 !  one pass - no gap filling --------------
      if ( kcycle .eq. 5 .and. jcycle .eq. 2 ) goto 9009 ! two cycles ----------
      if ( kcycle .eq. 6 .and. jcycle .eq. 3 ) goto 9009 ! three cycles --------

*     continue to the next cycle ... rewind the DAT file -----------------------
      rewind 02
      goto 6009
*     end of the cycle in the sequence for this particular DAT file ------------
 9009 continue

      call write final messages for model run
      if ( ifbatch .eq. 1 ) call proportion of effluent at model end 
      
      if ( ifbatch .eq. 1 .and. need works .gt. NUED ) then
      call change colour of text (12) ! orange
      write( *,4531)NUED,need works,kill works
 4531 format('* Too many discharges for back-tracking',12x,
     &'...',7x,'the maximum is ',i5,' (need',i5,')',12x,
     &'(ignored',i5,')')
      call set screen text colour
      endif

*     return to seek a new DAT file for the next model in batch mode -----------
      if ( ifbatch .eq. 1 ) then ! this is a batch run
          
      isuppress = suppress1 + suppress3 + suppress4
     &+ suppress5  + suppress6  + suppress7  + suppress8  + suppress9
     &+ suppress10 + suppress11 + suppress12 + suppress13 + suppress14
     &+ suppress16 + suppress17 + suppress18 + suppress19
     &+ suppress20 + suppress00 + suppress9a + suppress9b
      if ( isuppress .gt. 0 ) then
      call change colour of text (36) ! dull magenta (36)
      write(valchars10,27)isuppress
   27 format(i10)
      read(valchars10,28)(a(i),i=1,10)
   28 format(10a1)
      do i = 1,10
      if ( a(i) .eq. ' ' .and. a(i+1) .eq. ' ' ) a(i) = '.'
      enddo
      write( *,26)(a(i),i=2,10),datfilename
   26 Format('* WARNINGS about data ',9a1,20x,'...',7x,
     &'in',a40,2x,'Check SCN or ERR files ..')
      call summarise errors (isuppress)
      endif ! if ( isuppress .gt. 0 )
      goto 5500
      endif ! if ( ifbatch .eq. 1 )

 5501 continue ! all the model runs are finshed for this batch run -------------

      close (23)
	call close down
	call close down all the output files

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
 7973 format(/77('-')/'*** Error in reading RUNDAT.TMP ...'/77('-'))
      call stop

 8318 write( *,8379)
      write(01,8379)
      write(08,8379)
      write(09,8379)
      write(33,8379)
 8379 format(/73('-')/
     &'*** Error in data-sets for intermittent discharges ...'/
     &'*** Data-file has been assembled incorrectly ...'/
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
      include 'COM.FOR'
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
      do m = 1, 2
      NFIN(k,m) = -1
      enddo
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
	include 'COM.FOR'

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
     &6x,'Name of reach     Sets of data for    Next reach'/
     &6x,'                  diffuse inputs of             '/
     &6x,'                  flow and quality              '/
     &77('-'))

      do jj = 1, nreach
	j = ReachCode(jj)
      IF = RFDIFF (j)
      IQ = RFDIFF (j)

*     straight continuation ----------------------------------------------------
	if ( IPLAN(j,1) .gt. 0 .and. IPLAN(j,2) .eq. 0 
     &                       .and. IPLAN(j,3) .eq. 0 )then
	NextReach(j) = IPLAN(j,1)
      write(08,1)j,rname(j),IF,IQ,rname(IPLAN(j,1))
      endif

*     branch to new reach ------------------------------------------------------
	if ( IPLAN(j,1) .eq. 0 .and. IPLAN(j,2) .gt. 0 
     &                       .and. IPLAN(j,3) .eq. 0 )then
*	NextReach(j) = 0
 	NextReach(j) = IPLAN(j,2)
      write(08,1)j,rname(j),IF,IQ,rname(IPLAN(j,2))
    1 format(i3,3x,a16,5x,2i6,5x,a16)
      endif

*     mixing of reaches -------------------------------------------------------- 
	if ( IPLAN(j,1) .gt. 0 .and. IPLAN(j,2) .gt. 0 
     &                       .and. IPLAN(j,3) .gt. 0 )then
	NextReach(IPLAN(j,1)) = IPLAN(j,3)
	NextReach(IPLAN(j,2)) = IPLAN(j,3)
      write(08,3)j,rname(j),IF,IQ,rname(IPLAN(j,3))
    3 format(i3,3x,a16,5x,2i6,5x,a16,5x,a16)
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
	include 'COM.FOR'

      if ( ICAL .eq. 1 ) return

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
      
      write(08,2)
    2 format(//82('-')/
     &'Standards defined for particular reaches ...',
     &' a negative value indicates the target'/ 82('-')/
     &6x,'Name of reach        Determinand   ',
     &'      High      Good  Moderate      Poor'/
     &6x,'-------------        -----------   ',
     &'      ----      ----  --------      ----'/
     &82('-'))
      
      do jj = 1, nreach
      IREACH = ReachCode(jj)

*     check for the existence of reach-specific standards ----------------------
      IQSreach = 0
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      if ( EQS reach (IREACH,idet) .gt. 0 ) then
      IQSreach = EQS reach (IREACH,idet)
      endif
      endif
      enddo
      if ( IQSreach .gt. 1 ) then

      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then
      if ( EQS reach (IREACH, idet) .gt. 0 ) then
      nst = EQS reach (IREACH, idet)
      class limmits (nclass,idet) = 1.0e10
      do ic = 1, nclass - 1
      class limmits (ic,idet) = standards for reach (nst,ic)   
      enddo
      write(08,1)IREACH,rname(IREACH),dname(idet),
     &(class limmits (ic,idet), ic = 1, nclass-1)
    1 format(i3,3x,a16,5x,a11,3x,10f10.2)
      endif ! if ( EQS reach (IREACH, idet) .gt. 0 )
      endif ! if ( qtype (idet) .ne. 4 )
      enddo ! do idet = 1, ndet
      endif ! if ( IQSreach .gt. 1 )
      enddo ! do jj = 1, nreach
      
      write(08,4)
    4 format(82('-')//)

      return
	end
      
      
      

	subroutine initial track
	include 'COM.FOR'

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
      master set used = 0

      if ( model number in batch .eq. 1 ) then
      if ( master set used .eq. 1 ) then
 	call change colour of text (16) ! dark grey
      write( *,95)
   95 format(130('-')/
     &'Using the first set of data to overwrite data on which ',
     &'determinands included etc ...') 
	else
 	call change colour of text (16) ! dark grey
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
	include 'COM.FOR'
      
      close (01) ! main output
	close (21) ! tidy output
      close (03) ! summaries of output
      close (08) ! tables in input data
      close (09) ! copy of screen output
      close (74) ! flow gap filling
      close (75) ! quality gap filling

      close (11) ! temporary file ... for example, non-parametric data
      close (12) ! temporary file ... for example, monthly data
      close (22) ! visual basic graphics
      close (27) ! report on load
      close (30) ! compliance with targets
      close (31) ! outputs on effluents
      close (33) ! outputs on errors
      close (39) ! apportionment of loads in 95-percentile concentration
      close (40) ! apportionment of total load
      close (44) ! output on apportionment
      close (42) ! GIS data
      close (64) ! cost data
      close (72) ! river flows

*     rewind the data file for the present (batch) run ... FILENAME.DAT --------
      rewind 02
      return
      end

*     create extra data files for the catchment and model ----------------------
      subroutine create extra data files
	include 'COM.FOR'

*     file for output for determinads .MON for this data file --------101,102,etc
      ichannel = 100
	do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
 8001 format(a136)
      call trunk6 (kay)
	open(ichannel+kay,FILE=FNAME)
      write(ichannel+kay,*)' '
      endif
      enddo
      
*     file for output on apportionment .APT for this data file ----------121,122,etc
      ichannel = 120
	do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk9 (kay)
	open(ichannel+kay,FILE=FNAME)
      write(ichannel+kay,*)' '
      endif
      enddo

*     file for output for determinads .CTM for this data file -----------141,142,etc
      ichannel = 140
	do kay = 1, ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk10 (kay)
	open(ichannel+kay,FILE=FNAME)
      write(ichannel+kay,*)' '
      endif
	enddo

*     file for output on for determinads .CSV for this data file ------------111
      ichannel = 110
	do kay = 1, Ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk7 (Kay)
	open(ichannel+kay,FILE=FNAME)
	write(ichannel+kay,*)' '
	backspace(ichannel+kay)
      if ( n146 .eq. 2 ) then ! heading 2222222222222222222222222222222222222222
      write(ichannel+kay,8200)bday,bmon,IYEAR,ical,Title ! 222222222222222222222
      endif ! if ( n146 .eq. 2 ) then ! heading 22222222222222222222222222222222
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
      write(ichannel+kay,8250)bday,bmon,IYEAR,ical,Title
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
      write(ichannel+kay,8255)bday,bmon,IYEAR,ical,Title
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444
      endif
	enddo

*     file for output on discharges .CSV for this data file -----------------131
      if ( effcsv . eq. 1 ) then
      ichannel = 130
	do kay = 1, Ndet
      if ( QTYPE (kay) .ne. 4 ) then
      write (fname,8001) datname
      call trunk8 (Kay)
	open(ichannel+kay,FILE=FNAME)
 	write(ichannel+kay,*)' '
	backspace(ichannel+kay)
      call leadblanks
      if ( n146 .eq. 2 ) then ! heading 2222222222222222222222222222222222222222
      write(ichannel+kay,8200)bday,bmon,IYEAR,ical,Title ! 222222222222222222222
 8200 format('SIMCAT',',','Version 14.7 ',',', ! 2222222222222222222222222222222
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',') ! 2222222222
      endif ! if ( n146 .eq. 2 ) then ! heading 22222222222222222222222222222222
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
      write(ichannel+kay,8250)bday,bmon,IYEAR,ical,Title
 8250 format('SIMCAT',',','Version 14.8e',',', ! 3333333333333333333333333333333
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',')
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
      write(ichannel+kay,8255)bday,bmon,IYEAR,ical,Title
 8255 format('SIMCAT',',','Version 14.9 ',',', ! 4444444444444444444444444444444
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',')
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444
      endif
	enddo
      endif
      
*     file for output on apportionment .APT for this data file ---------------40
      write (fname,8001) datname
      call trunk ('A','P','T')
	open(40,FILE=FNAME) ! output on apportionment 
	write(40,*)' '
      
*     file for output on apportionment .AP2 for this data file --4444444444---41
      write (fname,8001) datname
      call trunk ('A','P','2') ! 44444444444444444444444444444444444444444444444
	open(41,FILE=FNAME) ! output on apportionment 4444444444444444444444444444
	write(41,*)' ' ! 444444444444444444444444444444444444444444444444444444444

*     file for output on apportionment .A95 for this data file 444444444444---39
      write (fname,8001) datname
      call trunk ('A','9','5') ! 44444444444444444444444444444444444444444444444
	open(39,FILE=FNAME) ! output on apportionment 4444444444444444444444444444
	write(39,*)' ' ! 444444444444444444444444444444444444444444444444444444444

*     file for output on apportionment .APC for this data file ---------------44
      write (fname,8001) datname
      call trunk ('A','P','C')
	open(44,FILE=FNAME) ! output on apportionment
	write(44,*)' '

*     file for outputs to effluents .EFF ... for this data file --------------31
      write (fname,8001) datname
      call trunk ('E','F','F')
	open(31,FILE=FNAME) ! outputs on effluents
	write(31,*)' '

*     file for outputs to effluents .AAA ... for this data file --------------38
      if ( n147 .eq. 1 ) then ! create .AAA file -----------------------------38
      write (fname,8001) datname
      call trunk ('A','A','A')
	open(38,FILE=FNAME) ! outputs on effluents via the AAA file ------------38
      endif ! if ( n147 .eq. 1 ) create .AAA file ----------------------------38

*     file for outputs to effluents .... EFF.CSV ... for this data file ------36
      if ( n147 .eq. 1 ) then ! create EFF.CSV file --------------------------36
      write (fname,8001) datname
      call trunk3 
	open(36,FILE=FNAME) ! outputs on effluents via the EFF.CSV file --------36
      call write headings for the effluent CSV file
      endif ! if ( n147 .eq. 1 ) create EFF.CSV file -------------------------36

*     file for output on river flows for this data file .FLO -----------------72
      write (fname,8001) datname
      call trunk ('F','L','O') ! output on river flows
	open(72,FILE=FNAME)
	write(72,*)' '

*     file for output on classification for this data file -------------------48
      write (fname,8001) datname
      call trunk ('W','F','D')
	open(48,FILE=FNAME) ! output on classification
	write(48,*)' '

*     file for output on natural purification .PUR ---------------------------34
      write (fname,8001) datname
      call trunk ('P','U','R')
	open(34,FILE=FNAME) ! output on natural purification
	write(34,*)' '
      
*     file for output on water quality planning .WQP -------------------------35
      write (fname,8001) datname
      call trunk ('W','Q','P')
	open(35,FILE=FNAME) ! output on water quality planning
	write(35,*)' '

*     file for output on water quality planning .WQC -------------------------37
      write (fname,8001) datname
      call trunk ('W','Q','C')
	open(37,FILE=FNAME)
	write(37,*)' '

	return
	end


      subroutine write final messages for model run
	include 'COM.FOR'

      call GETTIM ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP2 = LAP2-BIGLAPSE
      if ( LAP2 .lt. 0 ) LAP2 = LAP2 + 86400
      IMIN = LAP2/60
      ISEC = LAP2-60*IMIN
      if ( iscreen .lt. 3 ) write( *,1)IMIN,ISEC
      write(09,1)IMIN,ISEC
      write(33,1)IMIN,ISEC
    1 format(88('-'),' Lapsed time:',i6,' minutes and ',i2,' seconds')

      return
	end


      subroutine check ical is legal etc
	include 'COM.FOR'

      kerror = 1
      if ( ifbatch .eq. 0 ) close (23)
      ISCREEN = 0 ! control the amount of information written to the screen ----
      call GETDAT ( IYEAR, JMONTH, IDAY ) ! obtain the date --------------------
      call GETTIM ( IHR, IMIN, ISEC, IHUN ) ! obtain the time ------------------
      icancel1 = 12 * (iyear) + jmonth
      icancel2 = 12 * 2014 + 7
      if ( icancel1 .gt. icancel2 .and. N146 .eq. 2 ) then
*     stop "000008 000008 000008 000018 000018 0000FF"
      endif
      LAPSE = IHR*3600+IMIN*60+ISEC ! prepare to compute the time taken for run-

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
	include 'COM.FOR'

      if ( ical .eq. 01 ) then
     	call change colour of text (13) ! bright magenta
	endif
      if ( ical .eq. 02 ) then
     	call change colour of text (22) ! light blue
	endif
      if ( ical .eq. 03 ) then
	call change colour of text (11) ! cyan
	endif
      if ( ical .eq. 0 ) then
	call change colour of text (15) ! white
	endif
      if ( ical .eq. 0 ) then
	call change colour of text (35) ! white
	endif
      if ( ical .eq. 07 ) then
	call change colour of text (12) ! orange
	endif
      if ( ical .eq. 08 ) then
	call change colour of text (19) ! light pink
	endif
      if ( ical .eq. 09 ) then
	call change colour of text (50) ! brown
	endif

      return
	end


      subroutine check the number of shots
	include 'COM.FOR'

      if ( NS .gt. MS) then
	call change colour of text (21) ! dull red
      write( *,1)MS
    1 format('* Too many shots ...',31x,'...',7x,'number cut to',I6,
     &25x,25('.'))
      call set screen text colour
      write(09,2)MS
      write(33,2)MS
      if ( iscreen .lt. 3 ) write(01,2)MS
    2 format(77('-')/10x,'*** Too many shots ...'/
     &'*** The number has been cut to',I6,' ...'/77('-')/)
      NS = MS
      endif

      NS = MAX0(5,NS)

      return
      end
      

      subroutine check the number of shots 2
	include 'COM.FOR'

*     monthly structure --------------------------------------------------------
      if ( munthly structure .eq. 1 ) then ! check number of shots
      xcheck = float (NS) / 365.0
      icheck = xcheck
	xcheck = xcheck - icheck 
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then
      write(01,3666)NS
      write(21,3666)NS
      if ( model number in batch .eq. 1 ) then
      if ( jstructure message .eq. 0 ) then
      if ( master set used .eq. 1 ) then
	call change colour of text (20) ! bright red
      write( *,3666)NS
 3666 format(120('-')/
     &'* SIMCAT is set up to expect monthly structured data ... ',
     &'the specified number of shots is',i6/
     &'* This should be a multiple of 365 ... ',
     &'the requested monthly structure is suppressed ...'/
     &'* But will be put back if type 8 data are entered later ...'/
     &120('-')) 
      call set screen text colour
      endif
      jstructure message = 1
	endif
	endif
      write(33,3666)NS
      write(09,3666)NS
      munthly structure = 0 ! NS is not a multiple of 365
	endif
      endif

      return
	end

      
      
      subroutine set indices for percentiles
	include 'COM.FOR'

*     index for obtaining percentile from an ordered list of NS items ----------
      k95 = 0.05 * NS ! 95-percentile
      if ( k95 .eq. 0 ) k95 = 1
      k90 = 0.10 * NS ! 90-percentile
      if ( k90 .eq. 0 ) k90 = 1
      k98 = max0 (1, int(0.02 * NS)) ! 98-percentile
      if ( k98 .eq. 0 ) k98 = 1
      k99 = max0 (1, int(0.01 * NS)) ! 99-percentile
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
      kptarg = 95 ! Richard III ... output mode
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
	include 'COM.FOR'

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
	include 'COM.FOR'

      ndetCP=0
	do j=1,ndet
	if ( QTYPE (j) .ne. 4 ) ndetCP=ndetCP+1
      enddo
      if ( ical13 .eq. 0 ) then
      write(22,*)ndetCP
      endif

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      if ( ifbatch .ne. 1 ) then 
	write(42,5299) TITLE
 5299 format('$Title'/
     &'"',a70,'"')
	write(42,5699) datname
 5699 format('"',a70,'","Data file ..."',',"Single run  ..."')
      write(42,5228) MP10
 5228 format(i4,',"Number of determinands ..."')
      endif

      if ( ifbatch .eq. 1 ) then ! this is a batch run 
	write(42,5299) TITLE ! GIS ... for individual model IIIIIIIIIIIIIIIIIIIIII
	write(42,5899) datname ! GIS ... for individual model IIIIIIIIIIIIIIIIIIII
 5899 format('"',a70,'","Data file ..."',',"Part of batch run ..."')
      write(42,5228) MP10 ! GIS ... for individual model IIIIIIIIIIIIIIIIIIIIIII
      endif

      endif
      endif

      do 73 j=1,MP10
      if ( ical13 .eq. 0 ) then
      if ( dname (j) .eq. '           ' ) dname(j) = '...........'
      if ( QTYPE(j) .ne. 4 ) write(22,222)QTYPE(j),dname(j),MRQS(j)
  222 format(i3,1x,'"',a11,'"',i6)
      if ( noGIS .eq. 0 ) then
      write(42,242)j,qtype(j),dname(j),MRQS(j)
  242 format(i3,',',i3,',','"',a11,'"',',',i6,',"Determinand number,
     & Type of determinand,
     & Name of the determinand, Code number of set of river quality ',
     &'targets"')
      endif
      endif
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
	include 'COM.FOR'
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

      if ( n146 .eq. 2 ) then ! heading 2222222222222222222222222222222222222222
      if ( ifbatch .eq. 0 ) write( *,1063)bday,bmon,IYEAR,BH,bmin ! 222222222222
      write(01,1063)bday,bmon,IYEAR,BH,bmin ! 2222222222222222222222222222222222
      write(08,1063)bday,bmon,IYEAR,BH,bmin ! 2222222222222222222222222222222222
      write(09,1063)bday,bmon,IYEAR,BH,bmin ! 2222222222222222222222222222222222
 1063 format(77('-')/                       ! 2222222222222222222222222222222222
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X,  ! 2222222222
     &'Date: ',a2,'/',a2,'/',I4/'VERSION 14.7  ', ! 2222222222222222222222222222
     &'(Tony Warn 11/11/14)',17x,'...', ! 22222222222222222222222222222222222222
     &5x,'  Time:  ',4x,a2,'.',a2) ! 2222222222222222222222222222222222222222222
      endif ! if ( n146 .eq. 2 ) 22222222222222222222222222222222222222222222222
      
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
      call change colour of text(16)      
      if ( ifbatch .eq. 0 ) write( *,1963)bday,bmon,IYEAR,BH,bmin
      write(01,1963)bday,bmon,IYEAR,BH,bmin
      write(08,1963)bday,bmon,IYEAR,BH,bmin
      write(09,1963)bday,bmon,IYEAR,BH,bmin
 1963 format(77('-')/
     &'SIMCAT Model for Planning River Water Quality',6x,'... ',4X,
     &'  Date: ',a2,'/',a2,'/',I4/'VERSION 14.8e ', ! 333333333333333333333333333
     &'(Tony Warn 11/11/14)',17x,'...', ! 33333333333333333333333333333333333333
     &5x,'  Time:  ',4x,a2,'.',a2) ! 3333333333333333333333333333333333333333333
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
      call change colour of text(16)      
      if ( ifbatch .eq. 0 ) write( *,2963)bday,bmon,IYEAR,BH,bmin
      write(01,2963)bday,bmon,IYEAR,BH,bmin
      write(08,2963)bday,bmon,IYEAR,BH,bmin
      write(09,2963)bday,bmon,IYEAR,BH,bmin
 2963 format(77('-')/
     &'SIMCAT Model for Planning River Water Quality',6x,'... ',4X,
     &'  Date: ',a2,'/',a2,'/',I4/'VERSION 14.9 ', ! 444444444444444444444444444
     &'(Tony Warn 11/11/14)',18x,'...', ! 44444444444444444444444444444444444444
     &5x,'  Time:  ',4x,a2,'.',a2) ! 4444444444444444444444444444444444444444444
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444

      if ( ical13 .eq. 0 ) then
      write(03,1963)bday,bmon,IYEAR,BH,bmin
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      write(100+ic,1963)bday,bmon,IYEAR,BH,bmin
      write(100+ic,4972)
      write(120+ic,1963)bday,bmon,IYEAR,BH,bmin
      write(120+ic,4972)
      write(140+ic,1963)bday,bmon,IYEAR,BH,bmin
      write(140+ic,4972)
 4972 format(77('-'))
      endif
      endif
	enddo
	write(35,1963)bday,bmon,IYEAR,BH,bmin
	endif
      write(33,1963)bday,bmon,IYEAR,BH,bmin
      if ( ical13 .eq. 0 ) then
      write(27,1963)bday,bmon,IYEAR,BH,bmin
      write(21,1963)bday,bmon,IYEAR,BH,bmin
	write(34,1963)bday,bmon,IYEAR,BH,bmin
      write(30,1963)bday,bmon,IYEAR,BH,bmin
      write(31,1963)bday,bmon,IYEAR,BH,bmin
      write(37,1963)bday,bmon,IYEAR,BH,bmin
      write(39,1963)bday,bmon,IYEAR,BH,bmin ! 3333333333333333333333333333333333
      write(40,1963)bday,bmon,IYEAR,BH,bmin
      write(41,1963)bday,bmon,IYEAR,BH,bmin
      write(44,1963)bday,bmon,IYEAR,BH,bmin
      write(48,1963)bday,bmon,IYEAR,BH,bmin
      write(72,1963)bday,bmon,IYEAR,BH,bmin
      endif !  if ( ical13 .eq. 0 )

      if ( nobigout .le. 0 ) then
      write(01,4972)
      write(21,4972)
      endif
      if ( iscreen .lt. 3 ) write( *,4972)
      write(08,4972)
      write(09,4972)
      write(33,4972)
      if ( ical13 .eq. 0 ) then
      write(27,4972)
      write(03,4972)
      write(34,4972)
      write(35,4972)
      write(30,4972)
      write(31,4972)
      write(39,4972) ! 333333333333333333333333333333333333333333333333333333333
      write(40,4972)
      write(41,4972)
      write(44,4972)
      write(48,4972)
      write(72,4972)
      endif

*     write out the title on the data file =====================================
      write(09,7941)TITLE
      write(08,7941)TITLE
      if ( ical13 .eq. 0 ) then
      write(03,7941)TITLE
	write(34,7941)TITLE
	write(35,7941)TITLE
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,7941)TITLE
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,7941)TITLE
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,7941)TITLE
      endif
	enddo
	endif 
      write(33,7941)TITLE
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(21,7941)TITLE
      write(27,7941)TITLE
      write(30,7941)TITLE
      write(31,7941)TITLE
      write(39,7941)TITLE ! 3333333333333333333333333333333333333333333333333333
      write(40,7941)TITLE
      write(41,7941)TITLE
      write(44,7941)TITLE
      write(48,7941)TITLE
      write(72,7941)TITLE
	endif !  if ( ical13 .eq. 0 )
 7941 format(A70)

      if ( nobigout .le. 0 ) write(01,7941)TITLE

      if ( ifbatch .eq. 1 ) then ! this is a batch run =========================
	datfilename=datname
	call lose folder

	if ( ical .eq. 0 ) then
	write( *,9044)model number in batch,Datfilename
 9044 format('Run the SIMCAT model',18x,i6,7x,'...',6x,a40,5x,25('-'))
	endif 

      if ( calspecial .eq. 1 ) then
*     renew gap filling --------------------------------------------------------
	if ( ical .eq. 1) write( *,2344)model number in batch,Datfilename
 2344 format('Fill gaps for river flows',13x,i6,6x,' ... ',
     &5x,a40,5x,25('-'))
	if ( ical .eq. 3) then
	write( *,2345)model number in batch,Datfilename
 2345 format('Fill gaps in water quality',12x,i6,6x' ... ',
     &5x,a40,5x,25('-'))
	rewind 74
	endif
	endif

	if ( ical .eq. 2 ) then
	write( *,2365)model number in batch,Datfilename
 2365 format('Run the gap-filled flow model',9x,i6,6x' ... ',
     &5x,a40,5x,25('-'))
	rewind 74
	endif
	if ( ical .eq. 4 ) then
	write( *,8544)model number in batch,Datfilename
 8544 format('Run the gap-filled model',7x,
     &i13,6x,' ... ',5x,a40,5x,25('-'))
	rewind 74
	rewind 75
	endif

*     mode 7 - set standards for effluents -------------------------------------
	if ( ical .eq. 07 .and. ifbatch .eq. 1 ) then
	call change colour of text (12) ! orange
      if ( no gap filling 78 .eq. 1 ) then
	write( *,8331)model number in batch,Datfilename
 8331 format('Set permits to meet targets',8x,i9,6x,' ... ',
     &5x,a40,5x,25('-'))
      else
	write( *,8631)model number in batch,Datfilename
 8631 format('Set permits to meet targets',8x,i9,6x,' ... ',
     &5x,a40,5x,'With gap filling ........')
	rewind 74
	rewind 75
      endif
     	call change colour of text (15) ! bright white
	endif

*     mode 8 - set standards for effluents -------------------------------------
	if ( ical .eq. 08 .and. ifbatch .eq. 1 ) then
	call change colour of text (19) ! light pink
      if ( no gap filling 78 .eq. 1 ) then
	write( *,8332)model number in batch,Datfilename
 8332 format('Set permits (with no deterioration)',i9,6x,' ...',
     &6x,a40,5x,25('-'))
      else
	write( *,8632)model number in batch,Datfilename
 8632 format('Set permits (with no deterioration)',i9,6x,' ...',
     &6x,a40,5x,'With gap filling ........')
	rewind 74
	rewind 75
      endif
     	call change colour of text (15) ! bright white
	endif

*     mode 9 - set standards for effluents -------------------------------------
	if ( ical .eq. 09 .and. ifbatch .eq. 1 ) then
	call change colour of text (50) ! light pink
      if ( no gap filling 78 .eq. 1 ) then
	write( *,8337)model number in batch,Datfilename
 8337 format('Set permits (using EA procedures)',i9,8x,' ...',
     &6x,a40,5x,25('-'))
      else
	write( *,8638)model number in batch,Datfilename
 8638 format('Set permits (using EA procedures)',i9,8x,' ...',
     &6x,a40,5x,'With gap filling ........')
	rewind 74
	rewind 75
      endif
     	call change colour of text (15) ! bright white
	endif
      endif ! if ( ifbatch .eq. 1 ) ============================================

      
      if ( iscreen .lt. 3 ) write( *,8271)NS
 8271 format(55x,'Number of Shots -',I5)
      if ( nobigout .le. 0 ) write(01,8271)NS
      write(09,8271)NS
      write(08,8271)NS
      write(33,8271)NS
      if ( ical13 .eq. 0 ) then
      write(27,8271)NS
      write(03,8271)NS
      if ( nobigout .le. 0 ) write(21,8271)NS
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,8271)NS
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,8271)NS
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,8271)NS
      endif
	enddo
      write(30,8271)NS
      write(31,8271)NS
      write(37,8271)NS
      write(39,8271)NS
      write(40,8271)NS
      write(41,8271)NS
      write(44,8271)NS
      write(48,8271)NS
      write(72,8271)NS
	endif

      if ( iscreen .lt. 3 ) write( *,7983)
 7983 format(77('-'))
      if ( nobigout .le. 0 ) write(01,7983)
      write(08,7983)
      write(09,7983)
      write(33,7983)
      if ( ical13 .eq. 0 ) then
      write(34,7983)
      write(35,7983)
      write(27,7983)
      write(03,7983)
      if ( nobigout .le. 0 ) write(21,7983)
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,7983)
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,7983)
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,7983)
      endif
	enddo
      write(30,7983)
      write(31,7983)
      write(37,7983)
      write(39,7983)
      write(40,7983)
      write(41,7983)
      write(44,7983)
      write(48,7983)
      write(72,7983)
      endif

      goto(6500,6501,6502,6503,6504,6599,6599,6507,6508,6509),ICAL+1
 
*     ##########################################################################
*     straightforward run with no gap filling ----------------------------------
 6500 continue
      if ( iscreen .lt. 3 ) write( *,6670)ICAL
 6670 format(10x,'Run type =',I2,' (Base Simulation - No Gap',
     &' Filling)'/77('-'))
      if ( nobigout .le. 0 ) write(01,6670)ICAL
      if ( nobigout .le. 0 ) write(21,6670)ICAL
      write(03,6670)ICAL
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6670)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6670)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6670)ICAL
      endif
	enddo
      write(09,6670)ICAL
      write(27,6670)ICAL
      write(30,6670)ICAL
      write(33,6670)ICAL
      write(34,6670)ICAL
      write(35,6670)ICAL
      write(37,6670)ICAL
      write(39,6670)ICAL
      write(40,6670)ICAL
      write(41,6670)ICAL
      write(44,6670)ICAL
      write(48,6670)ICAL
      write(72,6670)ICAL

      goto 6599
*     ##########################################################################

*     gap filling for river flows ----------------------------------------------
 6501 continue
      if ( iscreen .lt. 3 ) write( *,6671)ICAL
 6671 format(15x,'Run type =',I2,' (With gap filling for river flow)',
     &/77('-'))
      write(09,6671)ICAL
      write(33,6671)ICAL
      if ( nobigout .le. 0 ) write(01,6671)ICAL
      goto 6599

 6502 continue
      if ( iscreen .lt. 3 ) write( *,6672)ICAL
 6672 format(6x,'Run type =',I2,' (With filled gaps for river flow',
     &' - check the results)'/77('-'))
      write(09,6672)ICAL
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6672)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6672)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6672)ICAL
      endif
	enddo
      write(33,6672)ICAL
      if ( nobigout .le. 0 ) write(01,6672)ICAL
      write(27,6672)ICAL
      goto 6599

*     gap filling for river quality --------------------------------------------
 6503 continue
      if ( iscreen .lt. 3 ) write( *,6673)ICAL
 6673 format(11x,'Run type =',I2,' (Gap Filling of River Quality)'
     &/77('-'))
      if ( nobigout .le. 0 ) write(01,6673)ICAL
      write(09,6673)ICAL
      write(33,6673)ICAL

      if ( output mode .eq. 2 ) then
      if ( iscreen .lt. 3 ) write( *,6683)
      write(09,6683)
 6683 format(13x'This fits to the mean and 95-percentiles',
     &' of river quality ...')
      if ( nobigout .le. 0 ) write(01,6683)
      write(27,6683)
      endif

      if ( output mode .eq. 3 ) then
      if ( iscreen .lt. 3 ) write( *,6283)
      write(09,6283)
      if ( nobigout .le. 0 ) write(01,6283)
      if ( nobigout .le. 0 ) write(21,6283)
      write(27,6283)
      endif
 6283 format(11x,'This fits to the mean and 95-percentiles',
     &' of river quality ...')

      if ( iscreen .lt. 3 ) write( *,6892)ICAL
      write(09,6892)ICAL
      if ( nobigout .le. 0 ) write(01,6892)ICAL
 6892 format(7x,'(Graphical output may be misleading in Run Type',
     &I2,')'/77('-'))
      goto 6599

 6504 if ( iscreen .lt. 3 ) write( *,6674)ICAL
 6674 format(12x,'Run type =',I2,' (run the gap-filled ',
     &'model)'/77('-'))
      if ( nobigout .le. 0 ) write(01,6674)ICAL
      if ( nobigout .le. 0 ) write(21,6674)ICAL
      write(27,6674)ICAL
      write(30,6674)ICAL
      write(09,6674)ICAL
      write(33,6674)ICAL
	write(34,6674)ICAL
	write(35,6674)ICAL
	write(37,6674)ICAL
	write(03,6674)ICAL
	write(48,6674)ICAL
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6674)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6674)ICAL
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6674)ICAL
      endif
	enddo
      write(39,6674)ICAL
	write(40,6674)ICAL
	write(41,6674)ICAL
	write(44,6674)ICAL
	write(72,6674)ICAL

      if ( output mode .eq. 2 ) then
      if ( iscreen .lt. 3 ) write( *,6683)
      write(09,6683)
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6683)
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6683)
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6683)
      endif
	enddo
      if ( nobigout .le. 0 ) write(01,6683)
      if ( nobigout .le. 0 ) write(21,6683)
      write(27,6683)
      endif

      if ( output mode .eq. 3 ) then
      if ( iscreen .lt. 3 ) write( *,6283)
      write(09,6283)
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6283)
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6283)
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6283)
      endif
	enddo
      if ( nobigout .le. 0 ) write(01,6283)
      if ( nobigout .le. 0 ) write(21,6283)
      write(27,6283)
      endif
      goto 6599

*     mode 7 - set standards for effluents -------------------------------------
 6507 continue
      call write headings for mode 7
      goto 6599

*     mode 8 - set standards for effluents -------------------------------------
 6508 continue
      call write headings for mode 8
      goto 6599

*     mode 9 - set standards for effluents -------------------------------------
 6509 continue
      call write headings for mode 9

 6599 continue

      write(09,4974)
 4974 format(77('-'))
*     write(27,8974)
 8974 format(125('-'))
 8975 format(100('-'))

      if ( ical13 .eq. 0 )write(34,6974)
 6974 format(/'---------------------------------------------------'/
     &        'Report on Diffuse Inputs and Natural Purification'/
     &        '---------------------------------------------------')
      if ( ical13 .eq. 0 )write(35,6174)
 6174 format(/'--------------------------------'/
     &        'Report on Water Quality Planning'/
     &        '--------------------------------')
      if ( ical13 .eq. 0 )write(37,6374)
 6374 format(/'--------------------------------'/
     &        'Report on Water Quality Planning'/
     &        '--------------------------------')

      if ( ical13 .eq. 0 ) then
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) write(100+ic,6714) Dname(ic)
 6714 format(/43('-')/'Report on monthly data for ',a11/43('-'))
      if ( QTYPE (ic) .ne. 4 ) write(120+ic,6784) Dname(ic)
 6784 format(/60('-')/'Report on apportionment for ',a11/60('-')//)
      if ( QTYPE (ic) .ne. 4 ) write(140+ic,6794) Dname(ic)
 6794 format(/60('-')/'Report on apportionment for ',a11/60('-')//)
      endif
	enddo
	endif
      write(33,6914)
 6914 format(/77('-')/'Report on errors and warnings ...'/77('-')/)

*     if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( ical13 .eq. 0 ) then
      write(27,6994)
 6994 format(/'-------------------------'/
     &        'Report on Pollution Loads'/
     &        '-------------------------')

*     model number in batch - this is counter for the number of models run in --
*     this batch run -----------------------------------------------------------
      if ( model number in batch .eq. 1 ) then
*     write(27,6294)
 6294 format(/'--------------------------------------------------- '/
     &        'Report on Pollution Loads (suppressed in this mode) '/
     &        '--------------------------------------------------- ')
      endif
      endif

      if ( ical13 .eq. 0 ) then
      write(30,8994)
 8994 format(/'-----------------------------------'/
     &        'Report on compliance with standards'/
     &        '-----------------------------------')
      endif

      if ( ical13 .eq. 0 ) then
          
*     if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(39,8874) ! 333333333333333333333333333333333333333333333333333333333
 8874 format(/150('-')/ ! 333333333333333333333333333333333333333333333333333333
     &'Report on apportionment - the per cent of the load from ',
     &'catchments contributing to percentile concentrations'/150('-'))
      write(41,8274) ! 333333333333333333333333333333333333333333333333333333333
 8274 format(/150('-')/ ! 333333333333333333333333333333333333333333333333333333
     &'Report on apportionment - the per cent of the load from ',
     &'discharges contributing to percentile concentrations'/150('-'))
*     endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333
         
      write(40,8774)
 8774 format(/116('-')/
     &'Report on apportionment - the per cent of the load from ',
     &'discharges'/116('-'))
      write(44,8574)
 8574 format(/77('-')/
     &'Report on apportionment - the loads from ',
     &'sub-catchments'/77('-'))
      write(48,8174)
 8174 format(/'----------------------------------------------------'/
     &        'Report on classification - Water Framework Directive'/
     &        '----------------------------------------------------'/)
      write(72,8784)
 8784 format(/77('-')/
     &'Report on river flows ... giving for each point ... '/
     &'The percentiles running from 1 to 99 per cent ...'/
     &'Followed by the proportion of effluent in each of these ',
     &'percentiles ...'/
     &77('-')//)
      endif
*     endif

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

*     ##########################################################################
      if ( ical13 .eq. 0 ) then
      if ( idiff .eq. 0 ) then
      write(34,6905)
 6905 format('Global diffuse inflows have been excluded'/77('-'))
      else
      write(34,6906)
 6906 format('Global diffuse inflows have been included'/77('-'))
      endif
	endif
*     ##########################################################################

*     ##########################################################################
      if ( ical13 .eq. 0 ) then
      if ( ipur .eq. 0 ) then
      write(34,6915)
 6915 format('Global natural purification has been excluded'/77('-')//)
      else
      write(34,6916)
 6916 format('Global natural purification has been included'/77('-')/
     &'Determinand    Rate Constant    Baseline Quality'/77('-'))
      do idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) write(34, 6917) Dname(idet),
     &rate(idet),qbase(idet),units(idet)
 6917 format(a11,f10.3,' 1/days ',f11.3,1x,a4,'       ')
      enddo
      write(34,6956)
 6956 format('--------------------------------------------------- '/)
      endif
      endif
*     ##########################################################################


      if ( no tables .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,4590)
      write(08,4590)
	endif
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
	include 'COM.FOR'
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

      if ( n146 .eq. 2 ) then ! heading 2222222222222222222222222222222222222222
	write( *,1063)bday,bmon,IYEAR,BH,bmin ! 2222222222222222222222222222222222
 1063 format(77('-')/ ! 22222222222222222222222222222222222222222222222222222222
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X, ! 22222222222
     &'Date: ',a2,'/',a2,'/',I4/'Version 14.7  ', ! 2222222222222222222222222222
     &'(Tony Warn 11/11/14)',17x,'...', ! 22222222222222222222222222222222222222
     &7x,'Time:  ',4x,a2,'.',a2) ! 222222222222222222222222222222222222222222222
      endif ! if ( n146 .eq. 2 ) then ! heading 22222222222222222222222222222222
      
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
 	write( *,1963)bday,bmon,IYEAR,BH,bmin ! 3333333333333333333333333333333333
 1963 format(77('-')/ ! 33333333333333333333333333333333333333333333333333333333
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X, ! 33333333333
     &'Date: ',a2,'/',a2,'/',I4/'Version 14.8e ', ! 3333333333333333333333333333
     &'(Tony Warn 11/11/14)',17x,'...', ! 33333333333333333333333333333333333333
     &7x,'Time:  ',4x,a2,'.',a2) ! 333333333333333333333333333333333333333333333
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
 	write( *,2963)bday,bmon,IYEAR,BH,bmin ! 4444444444444444444444444444444444
 2963 format(77('-')/ ! 44444444444444444444444444444444444444444444444444444444
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7X, ! 44444444444
     &'Date: ',a2,'/',a2,'/',I4/'Version 14.9  ', ! 4444444444444444444444444444
     &'(Tony Warn 11/11/14)',17x,'...', ! 44444444444444444444444444444444444444
     &7x,'Time:  ',4x,a2,'.',a2) ! 444444444444444444444444444444444444444444444
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444

      return
      end




      subroutine set the constraints on discharge quality
      include 'COM.FOR'
      if ( ical13 .eq. 1 ) return

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
      ecs = 0.6 * ecm
      gecm = ALOG (ecm*ecm/SQRoot(1230,ecm*ecm+ecs*ecs))
      gecs = SQRoot(1232,ALOG(1.+(ecs*ecs)/(ecm*ecm)))
      WEQ(3,JP) = EXP (gecm+1.6449*gecs)
      WEQ(1,JP) = ecm
      WEQ(2,JP) = ecs
      endif ! if ( WEQ(1,JP) .gt. 1.0E-8 )
  
*     good effluent quality ----------------------------------------------------
      if ( detype (JP) .ne. 104 ) then
      if ( GEQ(1,JP) .gt. 1.0E-8 ) then
      ecm = GEQ(1,JP)
      ecs = 0.6 * ecm
      gecm = ALOG (ecm*ecm/SQRoot(1230,ecm*ecm+ecs*ecs))
      gecs = SQRoot (1232,ALOG(1.+(ecs*ecs)/(ecm*ecm)))
      GEQ(3,JP) = EXP (gecm+1.6449*gecs)
      GEQ(1,JP) = ecm
      GEQ(2,JP) = ecs
      endif ! if ( GEQ(1,JP) .gt. 1.0E-8 )
      endif ! if ( detype (JP) .ne. 104 )

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
	include 'COM.FOR'
      character *1 spaces(136)
      character *200 line
	character *1 DUM1,DUM2
	real temkay(MP10)

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
	if (kreach .lt. 0 ) then

*     only part of the model will be run ***************************************
      kreach = - kreach
*     rub out the negative sign ************************************************
      do jjj=1,9
      if ( spaces(jjj) .eq. '-') then
	spaces(jjj) = ' '
	goto 1229
      endif 
      enddo
 1229 continue
*     store the number of the reach from which the model run will start ********
      negreach = kreach
      endif

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
     &'* Illegal Reach Code: ',I6,
     &' ... the Reach is excluded ...')
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,7372) KREACH
      write(08,7372) KREACH
      write(09,7372) KREACH
      write(33,7372) KREACH
 7372 format(/
     &'---------------------------------------------------------- '/
     &'*** Illegal Reach Code: ',I4/
     &'*** SIMCAT continues but the Reach is excluded ...'/
     &'---------------------------------------------------------- ')
      goto 103
      endif

*     store the reach code for later checks (on Features) ----------------------
      ReachCode (nreach) = kreach
      REACHCODE2 (kreach) = nreach
      
*     check the format for the reach structure data ----------------------------
*     first try the old format - using the array IPLAN -------------------------
      NewFormat = 99
      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3)

*     negative value in IPLAN excludes the reach -------------------------------
      if ( IPLAN( KREACH,1 ) .lt. 0 ) goto 1429

*     mark this line as having the old structure of reach data -----------------
      NewFormat = 0

*     No error. The reach structure is in the old format.  Proceed to read -----
      goto 9500

*     ##########################################################################
*     error found on reading the reach structure ... the reach structure is in -
*     the new format -----------------------------------------------------------

 7506 continue

*     mark this line as new structure ------------------------------------------
      NewFormat = 1
 	IPLAN(kreach,1)=0

      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2

*     negative value for NextReach excludes the Reach --------------------------
      if ( NextReach(KREACH) .lt. 0 ) goto 1429

 9500 continue

*     initialise the values of the reach rate constants ------------------------
      do i=1,10
      temkay(i)=3333.9
      enddo

*     check for an error in the reach number -----------------------------------
	if ( KREACH .lt. 1 ) goto 9999

*     no error found - check for an error in determining the reach structure ---
      if ( NewFormat .eq. 99 ) goto 9999

*	prepare to test for three or ten rate constants --------------------------
      Newkay = 99

*     read with the old structure of reaches -----------------------------------
      if ( NewFormat .eq. 0 ) then

*     check for three rate contants plus four schematic items ------------------
      read(line,*,ERR = 7501)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,7)
      Newkay = 17
      goto 8699

*     check for three rate contants plus two schematic items -------------------
 7501 continue
      read(line,*,ERR = 7502)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,5)
      Newkay = 15
      goto 8699

*     check for zero rate contants plus four schematic items -------------------
 7502 continue
      read(line,*,ERR = 7523)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,4)
      Newkay = 14
      goto 8699

*     check for three rate contants plus no schematic items --------------------
 7523 continue
      read(line,*,ERR = 7504)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,3)
      Newkay = 13
      goto 8699

*     check for zero rate contants plus two schematic items ------------------------
 7504 continue
      read(line,*,ERR = 7505)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,2)
      Newkay = 12
      goto 8699

*     check for zero rate contants plus no schematic items -------------------------
 7505 continue
      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      Newkay = 10
      goto 8699
	endif

*     check an assignment of rate constants has been made --------------------------
      if ( Newformat .eq. 0 .and. Newkay .eq. 99 ) goto 9997

*     prepare to do the same task but in the new format for the reach structure ----
      Newkay = 99
      if ( NewFormat .eq. 1 ) then
      read(line,*,ERR = 7581)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,7)
      Newkay = 17
      goto 8699

 7581 continue
      read(line,*,ERR = 7583)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,5)
      Newkay = 15
      goto 8699

 7583 continue
      read(line,*,ERR = 7584)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,4)
      Newkay = 14
      goto 8699

 7584 continue
      read(line,*,ERR = 7582)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,3)
      Newkay = 13
      goto 8699

 7582 continue
      read(line,*,ERR = 7585)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),(temkay(i),i=1,2)
      Newkay = 12
      goto 8699

 7585 continue
      read(line,*,ERR = 7506)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      Newkay = 10
	endif

*     check an assignment has been made ----------------------------------------
      if ( Newkay .eq. 99 ) goto 9999

*     check data structure variables are legal ---------------------------------
*     we now know the reach structure (the value of NewFormat) -----------------
*     and the details about how many rate rate constants have been entered -----
*     now read all the reach data for real -------------------------------------

 8699 continue
      if ( NewFormat .eq. 0 ) then

*     if Newkay equals 17 there are only three rate constants ------------------
      if ( Newkay .eq. 17) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
	Xrate = Rchkay(4,KREACH)
	Rchkay(4,KREACH) = Rchkay(3,KREACH)
	Rchkay(3,KREACH) = Xrate
	endif

*     if Newkay equals 15 there are only three rate constants ------------------
      if ( Newkay .eq. 15 ) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
	Xrate = Rchkay(4,KREACH)
	Rchkay(4,KREACH) = Rchkay(3,KREACH)
	Rchkay(3,KREACH) = Xrate
	endif

*     if Newkay equals 13 there are only three rate constants ------------------
      if ( Newkay .eq. 13) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH),
     &Rchkay(2,KREACH),Rchkay(4,KREACH),Rchkay(3,KREACH)
	Xrate = Rchkay(4,KREACH)
	Rchkay(4,KREACH) = Rchkay(3,KREACH)
	Rchkay(3,KREACH) = Xrate
	endif

*     if Newkay equals 14 there are ten rate constants -------------------------
      if ( Newkay .eq. 14) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif

*     if Newkay equals 12 there are ten rate constants -------------------------
      if ( Newkay .eq. 12) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif

*     if Newkay equals 10 there are ten rate constants -------------------------
      if ( Newkay .eq. 10) then
      read(line,*,ERR = 9998)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &(IPLAN(KREACH,K),K = 1,3),RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif
	endif

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

*     if Newkay equals 14 there are ten rate constants -------------------------
      if ( Newkay .eq. 14) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif

*     if Newkay equals 12 there are ten rate constants -------------------------
      if ( Newkay .eq. 12) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif

*     if Newkay equals 10 there are ten rate constants -------------------------
      if ( Newkay .eq. 10) then
      read(line,*,ERR = 9999)KREACH,RNAME(KREACH),RLENGTH(KREACH),
     &NextReach(kreach),DUM1,DUM2,RFDIFF(KREACH),
     &RQDIFF(KREACH),RCHA(KREACH),RCHB(KREACH)
      read(02,*,ERR = 9997)(Rchkay(i,KREACH),i=1,Ndet)
	endif
	endif

*     all the data have been read for this reach -------------------------------
 9600 continue

*     set defaults for temperature ---------------------------------------------
      BMAT (kreach, 1, 1) = TDEG
      BMAT (kreach, 1, 2) = TSDEV
      BMAT (kreach, 1, 3) = TCORF

*     check whether data have been added on the background quality for the reach
*     look at the next line of data --------------------------------------------
      LTEST = 0
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35
      call check for reach data on background (KREACH,LTEST)
      if ( LTEST .eq. 0 ) then
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35
      call check for reach data on background (KREACH,LTEST)
      if ( LTEST .eq. 0 ) then
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35
      call check for reach data on background (KREACH,LTEST)
      if ( LTEST .eq. 0 ) then
      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 35
      call check for reach data on background (KREACH,LTEST)
      endif
      endif
      endif
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
      write( *,7778)
      if ( nobigout .le. 0 ) write(01,7778)
      write(08,7778)
      write(09,7778)
      write(33,7778)
 7778 format(/
     &'-------------------------------------------------------'/
     &'*** Error in reading data on Reaches ...'/
     &'*** The Data-file has been assembled incorrectly ...'/
     &'*** Calculations halted ...'/
     &'-------------------------------------------------------')
      call close down

 7527 continue
      write( *,7578)
      if ( nobigout .le. 0 ) write(01,7578)
      write(08,7578)
      write(09,7578)
      write(33,7578)
 7578 format(/
     &'-------------------------------------------------------'/
     &'*** No data on Reaches ...'/
     &'*** Data-file has been assembled incorrectly ...'/
     &'*** Calculations halted ...'/
     &'-------------------------------------------------------')
      call stop
      return
	end


      subroutine write GIS descriptors
	include 'COM.FOR'

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      write(42,5296)

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
     &'" Items 15 - 19 are then repeated in this record for 9 ',
     &'more determinands (T) to (BL)"'/
*    &Face Value, (Face Value dets (J),J=1,MP10),model number in batch ---------
     &'" 64: the face value class across all determinands (BM)"'/
     &'" 65: the face value class for determinand 1 (BN)"'/
     &'" Item 65 is then repeated for the other 9 ',
     &'determinands (BO) to (BW)"'/
     &'" 75: the model number in a batch run (BX)"'/
     &'$Records')
	endif
      endif

      return
	end



      subroutine check for a reduced run
	include 'COM.FOR'
	integer stReach,numlist,curpos,test

*     check for a reduced run ... only some reaches will be modelled ...        [A]
*     **************************************************************************[A]
*     This routine populates the array INCLUDED with 0 (True) if the reach is
*     included and -1 (False) if it is not included ....
      
	NumIncluded = nreach

      if ( negreach .gt. 0 ) then

      do 1 kk=0, nreach
	if (kk.gt.0) then
	k = ReachCode(kk)
	else
	k = 0
      endif
	do 1 m=1,2
	NFIN(k,m) = -1
    1 continue

	do 2 kk=1,nreach
	k = ReachCode(kk)
	nextr = NextReach(k)
	if ( nextr .ge. 0 ) then
	if ( NFIN(nextr,1) .eq. -1) then
	NFIN(nextr,1)=k
      else
	if ( nFIN(nextr,2). eq. -1) then
	NFIN(nextr,2) = k
      endif
      endif 
      endif
    2 continue
   
      do jj = 1,nreach
	j=ReachCode(jj)
      Included(j) = -1
      IncludedList(j) = 0
      enddo

      stReach=Negreach
      numList = 1
      curpos = 1
      Included(stReach) = 0
      IncludedList(1) = stReach
   
      do
      do 4 k=1,2
      test = nfin(stReach, k)
      if (test .gt. 0) then
      curpos = curpos + 1
      Included(test) = 0
      IncludedList(curpos) = test
      endif
    4 continue
      numList = numList + 1
      if ( numList .gt. curpos) then 
	exit
      endif
      stReach = IncludedList(numList)
      enddo

*     Set IPLAN for furthest downstream reach to 0,0,0
	iplan(negreach,1)=0
	iplan(negreach,2)=0
	iplan(negreach,3)=0

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

      if ( ical13 .eq. 0 ) then
      write(22,*)NumIncluded
      endif

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      write(42,5291)nreach
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
      write(22,221)kreach,rname(kreach),rlength(kreach),
     &             NextReach(kreach)
  221 format(i4,',',a16,',',e10.4,',',i4)
      if ( noGIS .eq. 0 ) then
      write(42,241)kreach,rname(kreach),rlength(kreach),
     &             NextReach(kreach)
  241 format(i4,',"',a16,'",',e10.4,',',i4,',"Number of reach, ',
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
      include 'COM.FOR'

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
*     Partition  - partition coefficient

      call check first four characters of a line of data (IFIN)
      if ( IFIN .eq. 1 ) goto 223

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
      write(08,6222)MP10
      write(09,6222)MP10
      write(33,6222)MP10
      if ( iscreen .lt. 3 ) write( *,6222)MP10
 6222 format(/77('-')/
     &'Too many determinands ...        '/ 
     &'The number has been cut to', I3/77('-'))
      goto 223
	endif

*     try and read the new form of determinand data ----------------------------
      call extract a line of data (line of data)

*     with summary statistic - MRQS, coefficient of variation ------------------
*     correlation coefficient and partion data ---------------------------------
      read(line of data,*,END=9518)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET),
     &Dcoeff(ndet),    ! coefficient of variation ------------------------------
     &Dcorr(ndet),     ! correlation with river flow ---------------------------
     &Partition(ndet)  ! partition coefficient ---------------------------------
      goto 8515

*     with summary statistic - MRQS and partion data ---------------------------
 9518 continue
      read(line of data,*,END=9517)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET),
     &Dcoeff(ndet),    ! coefficient of variation ------------------------------
     &Dcorr(ndet)     ! correlation with river flow ---------------------------
	Partition(Ndet) = 0.0
      goto 8515

*     with summary statistic - MRQS and partion data ---------------------------
 9517 continue
      read(line of data,*,END=9516)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET),Partition(ndet)
      Dcoeff(ndet) = 0.6 ! coefficient of variation ----------------------------
      Dcorr(ndet) = 999.9  ! correlation with river flow -------------------------
      goto 8515

*     with summary statistic - MRQS --------------------------------------------
 9516 continue
      read(line of data,*,END=8516)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET),MRQS(NDET)
      Dcoeff(ndet) = 0.6 ! coefficient of variation ---------------------------
      Dcorr(ndet) = 999.9  ! correlation with river flow ------------------------
	Partition(Ndet) = 0.0
      goto 8515

*     try and read old form of data (without summary statistic information) ----
 8516 continue
      read(line of data,*,ERR=7509)QTYPE(NDET),DNAME(NDET),DNA(NDET),
     &UNITS(NDET),RATE(NDET),QBASE(NDET),QDIFFGAP(NDET),QZEROGAP(NDET),
     &WEQ(1,NDET),BEQ(1,NDET),GEQ(1,NDET)
	Partition(Ndet) = 0.0

 8515 continue
      
      call set determinand codes (ndet)
      
      if ( abs (Partition(NDET)) .gt. 0.0001 ) Detype (Ndet) = 900
      if ( Partition(NDET) .lt. -0.0001 ) then
      Detype (Ndet) = 909 ! dissolved metal
      Partition(NDET) = abs (Partition(NDET))
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
	MRQS(NDET) = 0
	endif

*     adjust MRQS for determinands where the standard must be exceeded ---------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile              
      if ( QTYPE (NDET) .eq. 03 .or. QTYPE (NDET) .eq. 05 ) then
      if ( MRQS(NDET) .eq.  2 ) MRQS(NDET) = 4
      if ( MRQS(NDET) .eq.  3 ) MRQS(NDET) = 5
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

*     count number of active determinands ----------------------------------------
      NDETA = 0
	do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) NDETA = NDETA + 1
	enddo

*     identify the last determinand ----------------------------------------------
      ndetlast = 0
	do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) ndetlast = idet
	enddo

*     identify the first active determinand --------------------------------------
      ndetfirst = 0
	do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
	ndetfirst = idet
	goto 5432
	endif
	enddo
 5432 continue
  
*     set the units for load -----------------------------------------------------
	do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
	lunits (idet) = 'LOAD'

	if ( funit .eq. 'TCMD' .or. funit .eq. 'Ml/d') then
	if ( units (idet) .eq. 'mg/l' ) then
	lunits (idet) = ' T/D'
	endif
	if ( units (idet) .eq. 'ug/l' ) then
	lunits (idet) = 'KG/D'
	endif
      endif

	if ( funit .eq. 'm3/d' ) then
	if ( units (idet) .eq. 'mg/l' ) then
	lunits (idet) = 'KG/D'
	endif
	if ( units (idet) .eq. 'ug/l' ) then
	lunits (idet) = ' G/D'
	endif
      endif

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


      subroutine set the gap filling switches
      include 'COM.FOR'

	ngorder = max0 ( 1, negorder)
      do 4144 IREACH = ngorder, NREACH ! loop on reaches

*     set the gap filling switches ---------------------------------------------
*     set the switch to "1" downstream of the last gap filling point -----------
*     and leave it at zero at and upstream of the last gap filling point -------

*     set the gap filling switches for river flow ------------------------------
      do itemp = 1,MU
      JUU = MU -itemp + 1
      if ( JREACH(JUU) .eq. IREACH ) then
      if ( iabs ( JFCAL(JUU) ) .gt. 0 ) goto 3701
      KFCAL(JUU) = 1
	endif
      enddo
 3701 continue

*     set the gap filling switches for river quality ---------------------------
      do 4147 itemp = 1,MU
      JUU = MU - itemp + 1
      KQCAL(JUU) = 0
      if (JREACH(JUU) .ne. IREACH) goto 4147
      if ( iabs ( JQCAL(JUU) ) .gt. 0) goto 4144
      KQCAL(JUU) = 1
 4147 continue
 4144 continue

      return
	end




      subroutine sort out data for length profiles
      include 'COM.FOR'

      BDIST = 0.0 ! initialise length of upstream river
      ITRAK = ReachCode (negorder) ! first reach
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
*	set to ITRAK the number of the receiving reach ---------------------------
      BDIST = BDIST + RLENGTH(IREACH) ! length of upstream river
	endif
      ITRAK = IPLAN(IREACH,1) ! next river
*     ADIST - the length of main river upstream of start of this Reach ---------
*     used for assembling data for graphs (km) ---------------------------------
      if ( ITRAK .gt. 0 ) ADIST (ITRAK) = BDIST
      goto 8900
      endif ! straight continuation

*     test for branch to the start of a new reach===============================
      if ( IPLAN(IREACH,1) .eq. 0 ) then
      LIMB = 0
      ITRAK2 = IPLAN(IREACH,2)
      ADIST (ITRAK2) = 0.0
      goto 8900
      endif !  test for branch 

*     mixing occurs ============================================================
      if ( IPLAN(IREACH,1) .eq. ITRAK ) then  
      INDx = IPLAN(IREACH,1)
      ITRAK = IPLAN(IREACH,3) ! resulting reach
      LIMB = 1
      BDIST = BDIST + RLENGTH(INDx)
      if ( ITRAK .gt. 0 ) ADIST (ITRAK) = BDIST
      goto 8900
      endif ! if ( IPLAN(IREACH,1) .eq. ITRAK )

      if ( IPLAN(IREACH,2) .eq. ITRAK ) then
      LIMB = 1
      INDx = IPLAN(IREACH,2)
      ITRAK = IPLAN(IREACH,3) ! resulting reach
      BDIST = BDIST + RLENGTH(INDx)
      if ( ITRAK .gt. 0 ) ADIST (ITRAK) = BDIST
      goto 8900
      endif ! if ( IPLAN(IREACH,2) .eq. ITRAK

 8900 continue
      return
	end





      subroutine initialise for the run
      include 'COM.FOR'

      call initialise QUICKWIN ! set up the screen 
      call GETDAT ( IYEAR, JMONTH, IDAY ) ! obtain the date
      call GETTIM ( IHR, IMIN, ISEC, IHUN ) ! obtain the time
*     prepare to compute the time taken for the run ----------------------------
      BIGLAPSE = IHR*3600+IMIN*60+ISEC ! 
      ifbatch = 1
      krite1 = 0
      effcsv = 0 ! switch on effluent csv output by setting to 1
      do i = 1, 150
	if ( i .ne. 6 ) close (i)
	enddo
      return
	end


      subroutine close down
      include 'COM.FOR'
      logical exists

      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( exists ) then  
      open(188,FILE = Globaldataname, STATUS = 'OLD')
      close (188, status='delete')    
	endif

      call GETTIM ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP3 = LAP2-BIGLAPSE
      if ( LAP3 .lt. 0 ) LAP3 = LAP3 + 86400
      IMIN2 = LAP3/60
      ISEC2 = LAP3-60*IMIN2

      isuppress = suppress1 + suppress3 + suppress4
     &+ suppress5  + suppress6  + suppress7  + suppress8  + suppress9
     &+ suppress10 + suppress11 + suppress12 + suppress13 + suppress14
     &+ suppress16 + suppress17 + suppress18 + suppress19
     &+ suppress20 
     &+ suppress00 + suppress9a + suppress9b

      call change colour of text (35) ! white
      write( *,4974)
 4974 format(130('-'))
      if ( ifbatch .eq. 0 .and. need works .gt. NUED ) then
      call change colour of text (12) ! orange
      write( *,4531)NUED,need works,kill works
 4531 format('* Too many discharges for back-tracking',12x,
     &'...',7x,'the maximum is ',i5,' (need',i5,')',12x,
     &'(ignored',i5,')')
      call set screen text colour
      endif
      if ( ifbatch .eq. 1) then
      call change colour of text (49) ! dull blue
      write( *,4972)Grand river length
 4972 format('Total length =',f11.1,' kilometres')
      call change colour of text (10) ! bright green
      write( *,45)imin2,isec2
   45 Format('Calculation completed ... ',
     &'Close this window to return to the main screen ...  ',
     &' Lapsed time:',i6,' minutes and',i3,' seconds .........')
      else ! ifbatch .ne. 1
      call change colour of text (10) ! bright green
      write( *,25)
   25 Format('Calculations completed ... ',
     &'Close this window to return to the main screen ...'/
     &'The ERR (and OUT and SCN) files contain comments on ',
     &'the DATA ...')
      if ( isuppress .gt. 0 ) then
      call change colour of text (36) ! dull magenta (36)
      write( *,26)isuppress
   26 Format('* Number of WARNINGS about data:',i13)
      call summarise errors (isuppress)
      endif ! if ( isuppress .gt. 0 )
      endif ! if ( ifbatch .eq. 1)
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '
      write( *,*)' '

      call message of completion

	do ichan = 1,160
 	close (ichan) 
      enddo

      do while (.TRUE.)
      end do
      end
      
      
      subroutine stop
      include 'COM.FOR'
      logical exists

      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( exists ) then  
      open(188,FILE = Globaldataname, STATUS = 'OLD')
      close (188, status='delete')    
	endif

      call GETTIM ( IHR, IMIN, ISEC, IHUN )
      LAP2 = IHR*3600+IMIN*60+ISEC
      LAP3 = LAP2-BIGLAPSE
      if ( LAP3 .lt. 0 ) LAP3 = LAP3 + 86400
      IMIN2 = LAP3/60
      ISEC2 = LAP3-60*IMIN2

      write( *,4974) ! write in green 
 4974 format(130('-'))
      if ( ifbatch .eq. 1) then
      call change colour of text (49) ! dull blue
      write( *,4972)Grand river length
 4972 format('Total length =',f11.1,' kilometres')
      call change colour of text (20) ! bright red
      write( *,45)imin2,isec2
      call change colour of text (20) ! bright red
   45 Format('Calculation halted ... ',
     &'Close this window to return to the main screen ...  ',
     &' Lapsed time:',i6,' minutes and',i3,' seconds .........')
      else
      call change colour of text (20) ! bright red
      write( *,25)
   25 Format('Calculations halted ... ',
     &'close this window to return to the main screen ...'/
     &'The ERR (and OUT and SCN) files contain comments on ',
     &'the data ...')
      endif
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

      call message of completion

	do ichan = 1,160
 	close (ichan) 
      enddo

      do while (.TRUE.)
      end do

      end
     
     
      subroutine message of completion
      include 'COM.FOR'

      write (fname,99) datname
      call trunk ('E','N','D')
	open(09,FILE=FNAME)
   99 format(a136)

      if ( n146 .eq. 2 ) then ! 222222222222222222222222222222222222222222222222
      write(09,1063)bday,bmon,IYEAR,BH,bmin ! 2222222222222222222222222222222222
 1063 format(77('-')/ ! heading 222222222222222222222222222222222222222222222222
     &'SIMCAT Model for Planning River Water Quality',6x,'...',7x, ! 22222222222
     &'Date: ',a2,'/',a2,'/',I4/'VERSION 14.7  ', ! 2222222222222222222222222222
     &'(Tony Warn 11/11/14)',17x,'...', ! 22222222222222222222222222222222222222
     &5x,'  Time:  ',4x,a2,'.',a2) ! 2222222222222222222222222222222222222222222
      endif ! if ( n146 .eq. 2 ) then ! 2222222222222222222222222222222222222222
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
      write(09,1963)bday,bmon,IYEAR,BH,bmin ! 3333333333333333333333333333333333
 1963 format(77('-')/ ! 33333333333333333333333333333333333333333333333333333333
     &'SIMCAT Model for Planning River Water Quality',6x,'... ',4x, ! 3333333333
     &'  Date: ',a2,'/',a2,'/',I4/'VERSION 14.8e ', ! 33333333333333333333333333
     &'(Tony Warn 11/11/14)',17x,'...', ! 33333333333333333333333333333333333333
     &5x,'  Time:  ',4x,a2,'.',a2) ! 3333333333333333333333333333333333333333333
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
      write(09,2963)bday,bmon,IYEAR,BH,bmin ! 4444444444444444444444444444444444
 2963 format(77('-')/ ! 44444444444444444444444444444444444444444444444444444444
     &'SIMCAT Model for Planning River Water Quality',6x,'... ',4x, ! 4444444444
     &'  Date: ',a2,'/',a2,'/',I4/'VERSION 14.9  ', ! 44444444444444444444444444
     &'(Tony Warn 11/11/14)',17x,'...', ! 44444444444444444444444444444444444444
     &5x,'  Time:  ',4x,a2,'.',a2) ! 4444444444444444444444444444444444444444444
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444
      write(09,1064)
 1064 format(77('-')/'Indicator that the run has finished ...'/
     &77('#'))
      close (09)
	
      return
      end



      subroutine write headings for mode 7
      include 'COM.FOR'

      if ( iscreen .lt. 3 ) then
*     check for the existence of a file for flow gap filling -------------------
	if ( no gap filling 78 .eq. 0 ) then
*     it exists ----------------------------------------------------------------
	write( *,6677)ICAL
	else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
	write( *,6777)ICAL
	endif
      endif

      if ( nobigout .le. 0 ) then
*     check for the existence of a file for flow gap filling -------------------
	if ( no gap filling 78 .eq. 0 ) then
*     it exists ----------------------------------------------------------------
	write(01,6677)ICAL
	write(03,6677)ICAL
      write(09,6677)ICAL
	write(21,6677)ICAL
      write(33,6677)ICAL
 6677 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
*     ---------------------------------------------------------------- it exists
	else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
	write(01,6777)ICAL
	write(03,6777)ICAL
      write(09,6777)ICAL
	write(21,6777)ICAL
	write(33,6777)ICAL
 6777 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &29x,' (No gap filling)'/77('-'))
	endif
*     -------------------------------------------------------- it does not exist 

*     munthly structure --------------------------------------------------------
      do ic = 1, ndet
*     QTYPE --------------------------------------------------------------------
	if ( QTYPE (ic) .ne. 4 ) then
*     check for the existence of a file for flow gap filling -------------------
	if ( no gap filling 78 .eq. 0 ) then
*     it exists ----------------------------------------------------------------
      write(100+ic,6677)ICAL ! --------------------------------------- it exists
      write(120+ic,6677)ICAL ! --------------------------------------- it exists
      write(140+ic,6677)ICAL ! --------------------------------------- it exists
	else ! "no gap filling 78" equals 1
*     it does not exist --------------------------------------------------------
      write(100+ic,6777)ICAL
      write(120+ic,6777)ICAL
      write(140+ic,6777)ICAL
	endif
*     -------------------------------------------------------- it does not exist 
	endif
*     -------------------------------------------------------------------- QTYPE 
	enddo
	endif

*     check for the existence of a file for flow gap filling -------------------
	if ( no gap filling 78 .eq. 0 ) then ! it exists -------------------------
      write(27,6677)ICAL
      write(30,6677)ICAL
      write(31,6677)ICAL
      write(39,6677)ICAL
      write(40,6677)ICAL
      write(41,6677)ICAL
      write(44,6677)ICAL
      write(48,6677)ICAL
      write(72,6677)ICAL
	else ! "no gap filling 78" equals 1 --------------------------------------
      write(27,6777)ICAL
      write(30,6777)ICAL
      write(31,6777)ICAL
      write(39,6777)ICAL
      write(40,6777)ICAL
      write(44,6777)ICAL
      write(48,6777)ICAL
      write(72,6777)ICAL
      endif ! ------------------------------------------------------------------

      return
      end
 
 
      
      subroutine write headings for mode 8
      include 'COM.FOR'
      
      if ( iscreen .lt. 3 ) then
	if ( no gap filling 78 .eq. 0 ) then
	write( *,6678)ICAL
	else ! "no gap filling 78" equals 1
	write( *,6778)ICAL
	endif
      endif ! if ( iscreen .lt. 3 )
 
      if ( nobigout .le. 0 ) then
 	if ( no gap filling 78 .eq. 0 ) then
	write(01,6678)ICAL
	write(03,6678)ICAL
	write(21,6678)ICAL
      write(09,6678)ICAL
      write(33,6678)ICAL
 6678 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
 	else ! "no gap filling 78" equals 1
	write(01,6778)ICAL
	write(03,6778)ICAL
	write(21,6778)ICAL
      write(09,6778)ICAL
      write(33,6778)ICAL
 6778 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &29x,' (No gap filling)'/
     &77('-'))
 	endif ! if ( no gap filling 78 ... 

      do ic = 1, ndet
	if ( QTYPE (ic) .ne. 4 ) then
	if ( no gap filling 78 .eq. 0 ) then
      write(100+ic,6678)ICAL
      write(120+ic,6678)ICAL
      write(140+ic,6678)ICAL
	else ! "no gap filling 78" equals 1
      write(100+ic,6778)ICAL
      write(120+ic,6778)ICAL
      write(140+ic,6778)ICAL
	endif
	endif
	enddo

	endif ! if ( nobigout .le. 0 )

	if ( no gap filling 78 .eq. 0 ) then
      write(27,6678)ICAL
      write(30,6678)ICAL
      write(31,6678)ICAL
      write(39,6678)ICAL
      write(40,6678)ICAL
      write(41,6678)ICAL
      write(41,6678)ICAL
      write(44,6678)ICAL
      write(48,6678)ICAL
      write(72,6678)ICAL
	else ! "no gap filling 78" equals 1
      write(27,6778)ICAL
      write(30,6778)ICAL
      write(31,6778)ICAL
      write(39,6778)ICAL
      write(40,6778)ICAL
      write(44,6778)ICAL
      write(48,6778)ICAL
      write(72,6778)ICAL
      endif

      return
      end

      
      subroutine write headings for mode 9
      include 'COM.FOR'
      
      if ( iscreen .lt. 3 ) then
	if ( no gap filling 78 .eq. 0 ) then
	write( *,6678)ICAL
	else ! "no gap filling 78" equals 1
	write( *,6778)ICAL
	endif
      endif ! if ( iscreen .lt. 3 )
 
      if ( nobigout .le. 0 ) then
 	if ( no gap filling 78 .eq. 0 ) then
	write(01,6678)ICAL
	write(03,6678)ICAL
	write(21,6678)ICAL
      write(09,6678)ICAL
      write(33,6678)ICAL
 6678 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &77('-'))
 	else ! "no gap filling 78" equals 1
	write(01,6778)ICAL
	write(03,6778)ICAL
	write(21,6778)ICAL
      write(09,6778)ICAL
      write(33,6778)ICAL
 6778 format(12x,'Run type =',I2,' (Set standards for effluents)'/
     &29x,' (No gap filling)'/
     &77('-'))
 	endif ! if ( no gap filling 78 ... 

      do ic = 1, ndet
	if ( QTYPE (ic) .ne. 4 ) then
	if ( no gap filling 78 .eq. 0 ) then
      write(100+ic,6678)ICAL
      write(120+ic,6678)ICAL
      write(140+ic,6678)ICAL
	else ! "no gap filling 78" equals 1
      write(100+ic,6778)ICAL
      write(120+ic,6778)ICAL
      write(140+ic,6778)ICAL
	endif
	endif
	enddo

	endif ! if ( nobigout .le. 0 )

	if ( no gap filling 78 .eq. 0 ) then
      write(27,6678)ICAL
      write(30,6678)ICAL
      write(31,6678)ICAL
      write(39,6678)ICAL
      write(40,6678)ICAL
      write(41,6678)ICAL
      write(41,6678)ICAL
      write(44,6678)ICAL
      write(48,6678)ICAL
      write(72,6678)ICAL
	else ! "no gap filling 78" equals 1
      write(27,6778)ICAL
      write(30,6778)ICAL
      write(31,6778)ICAL
      write(39,6778)ICAL
      write(40,6778)ICAL
      write(44,6778)ICAL
      write(48,6778)ICAL
      write(72,6778)ICAL
      endif

      return
      end


      subroutine check summary statistic for standard (ldet)
      include 'COM.FOR'
      
      if ( ical13 .eq. 1 ) return

      if ( qtype (ldet) .eq. 4 ) return
      if ( MRQS(ldet) .lt. 1 .or. MRQS(ldet) .gt. 6 ) then
      suppress00 = suppress00 + 1 ! no statistic for standards
	call change colour of text (21) ! dull red
      
      if ( detype (ldet) .eq. 103 .or. detype (ldet) .eq. 101 ) then
      write( *,1)DNAME(ldet)
    1 format('* No statistic defined for standards',15x,'...',
     &7x,'Determinand: ',a11,20x,'90-percentile assumed ...')
      call set screen text colour
      write(09,2)DNAME(ldet)
      write(01,2)DNAME(ldet)
      write(33,2)DNAME(ldet)
    2 format(120('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'90-percentile assumed by default ...'/120('-'))
      MRQS(ldet) = 3
      return
      endif
      
      if ( detype (ldet) .eq. 104 ) then
      write( *,5)DNAME(ldet)
    5 format('* No statistic defined for standards for',11x,'...',
     &7x,'Determinand: ',a11,20x,'10-percentile assumed ...')
      call set screen text colour
      write(09,6)DNAME(ldet)
      write(01,6)DNAME(ldet)
      write(33,6)DNAME(ldet)
    6 format(120('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'10-percentile assumed by default ...'/120('-'))
      MRQS(ldet) = 5
      return
      endif

      if ( detype (ldet) .eq. 106 ) then
      write( *,7)DNAME(ldet)
    7 format('* No statistic defined for standards',15x,'...',
     &7x,'Determinand: ',a11,20x,'95-percentile assumed ...')
      call set screen text colour
      write(09,8)DNAME(ldet)
      write(01,8)DNAME(ldet)
      write(33,8)DNAME(ldet)
    8 format(120('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'95-percentile assumed by default ...'/120('-'))
      MRQS(ldet) = 2
      return
      endif

      write( *,3)DNAME(ldet)
    3 format('* No statistic defined for standards',15x,'...',
     &7x,'Determinand: ',a11,20x,'Annual mean assumed .....')
      call set screen text colour
      write(09,4)DNAME(ldet)
      write(01,4)DNAME(ldet)
      write(33,4)DNAME(ldet)
    4 format(120('-')/
     &'No summary statistics defined for standards for ... ',a11/
     &'Annual mean assumed by default ...'/120('-'))
      MRQS(ldet) = 1
      endif
      
      return
      end
      
      subroutine summarise errors (isuppress)
      include 'COM.FOR'
      
      write(08,*)'* Total number of warnings about data     ',isuppress
      if ( suppress1 .gt. 0 ) then
      write(08,*)'* The sequence of features (1)            ',suppress1
      endif
      if ( suppress3 .gt. 0 ) then
      write(08,*)'* Huge change in flow (3)                 ',suppress3
      endif
      if ( suppress4 .gt. 0 ) then
      write(08,*)'* No determinands for classification (4)  ',suppress4
      endif
      if ( suppress5 .gt. 0 ) then
      write(08,*)'* Error reading flow gap filling data (5) ',suppress5
      endif
      if ( suppress6 .gt. 0 ) then
      write(08,*)'* Zero 95-percentile flow set to 1% (6)   ',suppress6
      endif
      if ( suppress7 .gt. 0 ) then
      write(08,*)'* Unnecessary monthly structures (7)      ',suppress7
      endif
      if ( suppress8 .gt. 0 ) then
      write(08,*)'* Quality data not set for gap fill (8)   ',suppress8
      endif
      if ( suppress9 .gt. 0 ) then
      write(08,*)'* Problems in the monthly structure (9)   ',suppress9
      endif
      if ( suppress9a .gt. 0 ) then
      write(08,*)'* Problems in the monthly temperature (9a)',suppress9a
      endif
      if ( suppress9b .gt. 0 ) then
      write(08,*)'* Problems in the monthly sus.solids (9b) ',suppress9b
      endif
      if ( suppress10 .gt. 0 ) then
      write(08,*)'* Illegal data for river flow (10)        ',suppress10
      endif
      if ( suppress11 .gt. 0 ) then
      write(08,*)'* Flow exceeds a billion (11)             ',suppress11
      endif
      if ( suppress14 .gt. 0 ) then
      write(08,*)'* Infeasibly huge concentration (14)      ',suppress14
      endif
      if ( suppress12 .gt. 0 ) then
      write(08,*)'* Problems with monthly structure (12)    ',suppress12
      endif
      if ( suppress13 .gt. 0 ) then
      write(08,*)'* Zero 95-percentile river flow (13)      ',suppress13
      endif
      if ( suppress17 .gt. 0 ) then
      write(08,*)'* Deleted feature (17)                    ',suppress17
      endif
      if ( suppress18 .gt. 0 ) then
      write(08,*)'* Zero variation in river quality (18)    ',suppress18
      endif
      if ( suppress16 .gt. 0 ) then
      write(08,*)'* Unneeded effluent data ignored (16)     ',suppress16
      endif
      if ( suppress19 .gt. 0 ) then
      write(08,*)'* Unneeded data ignored (19)              ',suppress19
      endif
      if ( suppress20 .gt. 0 ) then
      write(08,*)'* Infeasible correlation coefficients (20)',suppress20
      endif
      if ( suppress00 .gt. 0 ) then
      write(08,*)'* All the others (00)                     ',suppress00
      endif
      
      write(08,*)'* Number of stored reaches                ',maxistor
      maxistor = 0

      return
      end
      
      
      subroutine write headings for the effluent CSV file
      include 'COM.FOR'
   
      if ( n147 .eq. 1 ) write(36,1) ! write headings for the CSV file ---------
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
