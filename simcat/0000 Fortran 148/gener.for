*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     GENERATION OF FLOW AND QUALITY DATA FOR HEAD OF THE REACH   5553
*     --------------------------------------------------------------------------
      subroutine generate river flow 
      include 'COM.FOR'

*     initialise the values of the shots for river flow ------------------------
      do is = 1, NS
      FMS(IS) = 0.0
	enddo
      do ii = 1, 5
      FLOW(ii) = 0.0
      enddo

*     trap the use of the normal distribution for river flow -------------------
      if ( PDRF (IF) .eq. 1 ) then
      call change colour of text (20) ! bright red
      if ( iscreen .lt. 3 ) write( *,8051)IF
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,8051)IF
      write(09,8051)IF
      write(33,8051)IF
 8051 format(77('-')/
     &'*** Unusual data for river flow.  Normal distribution '/
     &'*** Check dataset number',i6/77('-'))
      endif
*     check for illegal data on river flow -------------------------------------
      if ( F(IF,2) .gt. F(IF,1) ) then
      suppress00 = suppress00 + 1 ! 95-percentile low flow exceeds the mean
      call change colour of text (20) ! bright red
      if ( iscreen .lt. 3 ) write( *,8951)IF
 8951 format('* 95-percentile low flow of exceeds the mean ... set ',
     &'number',i6)
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,8151)F(IF,2),F(IF,1),IF
      call sort format 2 (F(IF,2),F(IF,1))
      write(09,8151)valchars10,valchars11,IF
      write(33,8151)valchars10,valchars11,IF
 8151 format(77('-')/'*** A 95-percentile low flow of ',a10,
     &' exceeds the mean of ',a10/'*** Check the set of flow ',
     &'data number',i6/77('-'))
	write(33,8152)
	if ( nobigout .le. 0 ) write(01,8152)
      write(33,8152)
 8152 format('*** The 95-percentile low flow was set to 25% of ',
     &'the mean ... '/77('-'))
      F(IF,2) = 0.25 * F(IF,1) 
      endif
      if ( F(IF,2) .lt. 1.0e-08 .and. F(IF,1) .gt. 1.0e08 ) then
      suppress13 = suppress13 + 1
      call change colour of text (37) ! dull turquoise
      if ( iscreen .lt. 3 ) write( *,8959)IF
 8959 format('* Zero 95-percentile flow set to 1% of ',
     &'the mean',4x,'...',7x,'Set number',i7,' .......')
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,8958)IF
      write(09,8958)IF
      write(33,8958)IF
 8958 format(77('-')/'*** Problem with data for river flow ... '/
     &'*** The 95-percentile low flow is zero ....'/
     &'*** Check the set of flow data number',i6/77('-'))
	write(33,8952)
	if ( nobigout .le. 0 ) write(01,8152)
      write(33,8952)
 8952 format('*** The 95-percentile low flow was set to 1% of ',
     &'the mean ... '/77('-'))
      F(IF,2) = 0.01 * F(IF,1) 
      endif

*     identify the type of distribution ----------------------------------------
      IDIST = PDRF(IF)

*     check for zero flows -----------------------------------------------------
      if ( F(IF,1) .lt. 1.0E-08 ) return
*     constant flow ------------------------------------------------------------
      if ( idist .eq. 0 ) call generate constant river flows
*     normal distribution ------------------------------------------------------
      if ( idist .eq. 1 ) call generate normal river flows
*     log-normal distribution (2-parameter version) ----------------------------
      if ( idist .eq. 2 ) call generate log normal river flows
*     log-normal distribution (3-parameter version) ----------------------------
      if ( idist .eq. 3 ) call generate log normal river flows
*     non-parametric distribution ----------------------------------------------
      if ( idist .eq. 4 ) call generate non parametric river flows
*     monthly data-sets --------------------------------------------------------
      if ( idist .eq. 5 ) call generate monthly river flow data
*     distribution type 6 - loads - acting as type 1 (normal) ------------------
      if ( idist .eq. 6 ) call generate normal river flows
*     distribution type 7 - loads - acting as type 1 (log normal) --------------
      if ( idist .eq. 7 ) call generate log normal river flows
      if ( idist .eq. 8 ) then ! monthly structure -----------------------------
      call generate monthly structure for river flows 8 ! type 8
      endif ! if ( idist .eq. 8 ) ----------------------------------------------
      
*     power curve parametric distribution (river flow) 9999999999999999999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( idist .eq. 10 ) call generate log normal river flows ! 99999999999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999

      return
      end









*     provide a random sample from a non-parametric distribution ---------------
      subroutine get non parametric shot (rnpar,RFN)
      include 'COM.FOR'
      integer U

*     rfnpvl  is an array of class marks ---------------------------------------
*     rfnpfd  is an array of cumulative frequencies ----------------------------
*     nprf    is the number of classes -----------------------------------------
*     rnpar   is the uniform variate -------------------------------------------
*     RFN     is the returned variate ------------------------------------------

*     convert 'rnpar' from normal deviate to uniform deviate -------------------
      rnparx = tdist2 ( 999999.0, rnpar )
      
*     prepare to deal with values beyond the first and the last values
*     entered as data ....
*     we shall interpolate between the last point and a point half the last
*     interval beyond the last point

*     calculate the first interval ---------------------------------------------
      CLINT1 = rfnpvl (2) - rfnpvl (1)

*     TAU is the minimum - the first value less half the interval --------------
      TAU = rfnpvl (1) - CLINT1 / 2.

*     the last interval --------------------------------------------------------
      CLINT2 = rfnpvl (nprf) - rfnpvl (nprf-1)

*     THETA is the maximum - last value plus half the interval
      THETA = rfnpvl (nprf) + CLINT2 / 2.

*     calculate the gradients of these tails of the distribution
      GRAD1 = ( rfnpvl(1) - TAU  ) /  rfnpfd (1)
      GRAD2 = ( THETA - rfnpvl (nprf) ) / (1. - rfnpfd (nprf) )

*     locate this point on the cumulative frequency distribution.
      if ( rnparx .le. rfnpfd(1) ) then
      RFN = GRAD1 * rnparx + TAU
      else if ( rnparx .ge. rfnpfd(nprf) ) then
      RFN = GRAD2 * rnparx + THETA - GRAD2
      else

      CL = float(nprf)
      LL = 1
      U = nprf
      i10 = 0

   10 i10 = i10 + 1

      if ( i10 .eq. mprf ) then
      write( *,*)'Error in random sampling ...'
      write(01,*)'Error in random sampling ...'
      write(09,*)'Error in random sampling ...'
      write(33,*)'Error in random sampling ...'
      call stop
      endif

      cxx = 0.5 * float (U + LL)

      I = INT (cxx + 0.5)

      if ( rnparx .le. rfnpfd(I) .and. rnparx .ge. rfnpfd(I - 1) ) then

      Y1 = rfnpvl(I - 1)
      Y2 = rfnpvl(I)
      X1 = rfnpfd(I - 1)
      X2 = rfnpfd(I)

      RFN = (rnparx - X1) * (Y2 - Y1) / (X2 - X1) + Y1

      else
      if ( rnparx .gt. rfnpfd(I) ) LL = I
      if ( rnparx .lt. rfnpfd(I) ) U = I
      goto 10
      endif

      endif

      RFN = amax1 ( 0.0, RFN )

      return
      end


*     generate data on river quality -------------------------------------------
      subroutine generate river quality
      include 'COM.FOR'

      do IS = 1, NS
      CMS(JP,IS) = 0.0
      enddo

      if ( quolity data ( IQ, JP, 1 ) .lt. 1.0E-09) then
      quolity data ( IQ, JP, 1 ) = 1.0E-09
      quolity data ( IQ, JP, 2 ) = 1.0E-09
      endif
      IDIST = PDRC (IQ,JP) 

*     constant quality ---------------------------------------------------------
      if ( idist .eq. 0 ) call generate constant river quality

*     normal distribution ------------------------------------------------------
      if ( idist .eq. 1 ) call generate normal river quality

*     log-normal distribution (2-parameter) ------------------------------------
      if ( idist .eq. 2 ) call generate log normal river quality

*     log-normal distribution (3-parameter) ------------------------------------
      if ( idist .eq. 3 ) call generate log normal river quality

*     non-parametric distribution ----------------------------------------------
      if ( idist .eq. 4 .or. idist .eq. 9 ) 
     &call generate non parametric river quality

*     monthly data -------------------------------------------------------------
      if ( idist .eq. 5 ) call monthly river quality

*     normal distribution of loads ---------------------------------------------
      if ( idist .eq. 6 ) call generate normal river quality

*     log-normal distribution of loads (2 or 3-parameter) ----------------------
      if ( idist .eq. 7 ) call generate log normal river quality

      if ( idist .eq. 8 ) then ! monthly structure
	call generate monthly structure for river quality (0)
      endif
      
*     power curve parametric distribution (river quality) 9999999999999999999999
      if ( n149 .eq. 1 ) then ! power curve parametric distribution 999999999999
      if ( idist .eq. 9 ) call generate log normal river quality ! 9999999999999
      endif ! if ( n149 .eq. 1 ) 99999999999999999999999999999999999999999999999

      if ( idist .gt. 10 ) return
      if ( idist .lt. 0 ) stop 'generate river quality'

      return
      end

*     generate shots for river flow for the head of reach ----------------------
      subroutine generate log normal river flows
      include 'COM.FOR'

      if ( munthly structure .eq. 1 ) then ! impose monthly structure on log normal flows
      call impose monthly structure for river flows (0) ! log normal river flows
      return
      endif

      RFM = F(IF,1) ! mean flow
      RF5 = F(IF,2) ! 95-percentile low flow
      RF3 = 0.0     ! shift flow 
      if ( PDRF(IF) .eq. 3 ) RF3 = F(IF,3)
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN
      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1001,2.7057+2.*ALOG((RFM+RF3)/(REX)))-1.6449
      GRFM = ALOG (RFM+RF3) - 0.5 * GRFS*GRFS

*     calculate the Monte-Carlo sampling errors for log-normal river flows -----
      call bias in log normal river flows (GRFM, GRFS)
      if ( BM(1) .gt. 1.0E-08 ) BM(1)=RFM/BM(1)
      if ( BS(1) .gt. 1.0E-08 ) BS(1)=RF5/BS(1)
      if ( MONF .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,12)BM(1),BS(1)
   12 format(/77('-')/
     &'Correction factors for Monte-Carlo sampling: log-normal flows',
     &' ... '/77('-')/
     &'Mean          =',F8.3/'95-percentile =',F8.3/77('-'))
      endif

*     write(01,2399)(FRAN function (is), is = 1, NS)
*2399 format('River flow:    ',10f12.4)
      
      do is = 1, NS ! calculate log normal river flows
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = VALFL ( fdln, GRFS, GRFM,RF3, BM(1), BS(1), RF5 )
      FMS (IS) = amax1 ( 0.0, FMS(IS) )
      enddo ! calculate flows

      if ( MONF .gt. 1 ) call write shots for river flow
      return
      end
      
      subroutine generate removed log normal river flows
      include 'COM.FOR'

      if ( munthly structure .eq. 1 ) then ! impose monthly structure on log normal flow removals
      call impose monthly structure for river flows (1) ! log normal river flow removals
      return
      endif

      RFM = F(IF,1) ! mean flow
      RF5 = F(IF,2) ! 95-percentile low flow
      GRF3 = 0.0    ! shift flow -----------------------------------------------
      if ( PDRF(IF) .eq. 3 ) GRF3 = F(IF,3)
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN
      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1001,2.7057+2.0 * ALOG ((RFM+RF3)/(REX))) - 1.6449
      GRFM = ALOG (RFM+RF3) - 0.5 * GRFS*GRFS
      GRF5 = RF5

      do is = 1, NS ! calculate log normal river flows
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FTMS (is) = VALFL ( fdln, GRFS, GRFM, GRF3, BM(3), BS(3), GRF5 )
      FTMS (IS) = amax1 ( 0.0, FTMS(IS) )
      enddo ! calculate flows
      if ( MONF .gt. 1 ) call shots for removed flows
      return
      end
      
      subroutine generate removed normal river flows
      include 'COM.FOR'

*     if ( munthly structure .eq. 1 ) then ! impose monthly structure on normal removed flows
*     call impose monthly structure for river flows (1) ! normal river flow removals
*     return
*     endif

      GEFM = F(IF,1) ! mean flow -----------------------------------------------
      GEF5 = F(IF,2) ! 95-percentile -------------------------------------------
      GEF3 = F(IF,3) ! shift flow ----------------------------------------------
      GEFS = ( GEFM - GEF5 ) / 1.6449 ! standard deviation ---------------------
      do is = 1, NS ! calculate normal flow removals
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FTMS (is) = Vnorm ( fdln, GEFS, GEFM, GEF3, BM(3), BS(3), GEFM )
      FTMS (IS) = amax1 ( 0.0, FTMS(IS) )
      enddo ! calculate normal flows

      if ( MONF .gt. 1 ) call shots for removed flows
      return
      end

      subroutine generate removed log normal effluent flows
     &(GM,GS,RM)
      include 'COM.FOR'

*     if ( munthly structure .eq. 1 ) then ! impose monthly structure on log normal flow removals
*     call impose monthly structure for river flows (1) ! log normal effluent removals
*     return
*     endif

*     create the  corrected shots -------------------------------------------------
      do is = 1, NS ! calculate log normal effluent flows
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = Vlogn ( R3, GS, GM, RF3, BM(3), BS(3), RM )
      FTMS (IS) = amax1 ( 0.0, FTMS(IS) )
      enddo
      if ( MONF .gt. 1 ) call shots for removed flows

      return
      end
      
      subroutine generate removed normal effluent flows
     &(GM,GS,RM)
      include 'COM.FOR'

*     if ( munthly structure .eq. 1 ) then ! impose monthly structure on log normal flow removals
*     call impose monthly structure for river flows (1) ! normal effluent flow removals
*     return
*     endif

*     create the  corrected shots -------------------------------------------------
      do is = 1, NS ! calculate log normal effluent flows
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = Vnorm ( R3, GS, GM, 0.0, BM(3), BS(3), RM ) ! Richard III 0.0
      FTMS (IS) = amax1 ( 0.0, FTMS(IS) )
      enddo
      if ( MONF .gt. 1 ) call shots for removed flows

      return
      end
      
      

      subroutine generate log normal birfurcation 
      include 'COM.FOR'

      if ( munthly structure .eq. 1 ) then ! impose monthly structure for bifurcation
      call impose monthly structure for river flows (0) ! log normal bifurcation
      return
      endif

      RFM = F(IF,1) ! mean flow
      RF5 = F(IF,2) ! 95-percentile low flow
      RF3 = 0.0     ! shift flow 
      if ( PDRF(IF) .eq. 3 ) RF3 = F(IF,3)
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN
      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
      RM3 = (RFM + RF3) * (RFM + RF3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RFS/RFM .gt. 0.001 ) then
      GRFM = ALOG ( RM3 / SQRoot(100045,RM3+RFS*RFS) )
      GRFS = SQRoot(122305, ALOG (1.0+(RFS*RFS)/RM3) )
	else
	GRFS = 0.0
      GRFM = ALOG ( RM3 / SQRoot(100045,RM3) )
      endif
	endif
      
*     calculate the Monte-Carlo sampling errors for log-normal effluent flows --
      call bias in log normal effluent flows (GRFM, GRFS)
      if ( BM(1) .gt. 1.0E-08 ) BM(1)=RFM/BM(1)
      if ( BS(1) .gt. 1.0E-08 ) BS(1)=RFS/BS(1)
      if ( MONF .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,12)BM(1),BS(1)
   12 format(/77('-')/
     &'Corrections for Monte-Carlo sampling: ',
     &'removal of log-normal flows ... '/77('-')/
     &'Calculated mean    =',F8.3/
     &'Standard deviation =',F8.3/77('-'))
	endif

*     create the  corrected shots ---------------------------------------------
      do is = 1, NS ! calculate log normal effluent flows
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = Vlogn ( fdln, GRFS, GRFM, RF3, BM(1), BS(1), RFM )
      FMS (IS) = amax1 ( 0.0, FMS(IS) )
      enddo

      return
      end


      subroutine generate normal effluent flows
      include 'COM.FOR'

*     if ( munthly structure .eq. 1 ) then ! impose monthly structure on normal effluent flows
*     call impose monthly structure for river flows (0) ! normal effluent flows
*     return
*     endif

      GRFM = F(IF,1) ! mean flow ------------------------------------------------ 
      GRF5 = F(IF,2) ! standard deviation ---------------------------------------
      GRF3 = F(IF,3) ! shift flow -----------------------------------------------
      GRFS = ( GRFM - GRF5 ) / 1.6449
      
*     calculate the Monte-Carlo sampling errors for log-normal river flow ------
      call bias in normal effluent flows (GRFM, GRFS) 
      if ( BM(3) .gt. 1.0E-08 ) BM(3) = GRFM/BM(3)
      if ( BS(3) .gt. 1.0E-08 ) BS(3) = GRFS/BS(3)
      if ( MONF .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,12)BM(3),BS(3)
   12 format(/77('-')/
     &'Correction factors for Monte-Carlo sampling: ',
     &'normal effluent flows ... '/77('-')/
     &'Calculated mean    =',F8.3/
     &'Standard deviation =',F8.3/77('-'))
	endif

*     create the  corrected shots ----------------------------------------------
      do is = 1, NS ! calculate normal effluent flows
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = Vnorm ( fdln, GRFS, GRFM, GRF3, BM(3), BS(3), GRFM )
      FMS (IS) = amax1 ( 0.0, FMS(IS) )
      enddo

      return
      end


*     generate flow shots for head of reach ... normal distribution ------------
      subroutine generate normal river flows
      include 'COM.FOR'

      GRFM = F(IF,1) ! mean flow
      GRF5 = F(IF,2) ! 95-percentile low flow
      RF3 = F(IF,3)  ! shift
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN
      GRFS = (GRFM - GRF5) / 1.6449 ! standard deviation

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in normal river flows (GRFM,GRFS)

      if ( BM(1) .gt. 1.0E-08 ) BM(1)=GRFM/BM(1)
      if ( BS(1) .gt. 1.0E-08 ) BS(1)=GRF5/BS(1)
      if ( MONF .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,12)BM(1),BS(1)
   12 format(/
     &'Corrections for Monte-Carlo sampling: normal river flows'/
     &77('-')/'Mean                   =',F8.3/
     &'95-percentile low flow =',F8.3/77('-'))
	endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      fdnd = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = VALFN2 ( fdnd, GRFS, GRFM, RF3 )
      FMS (IS) = amax1(0.0, FMS(IS))
      enddo

      if ( MONF .gt. 1 ) call write shots for river flow
      return
      end


*     generate flow shots for head of reach - Constant flow --------------------
      subroutine generate constant river flows
      include 'COM.FOR'

*     mean flow ----------------------------------------------------------------
      RFM = F(IF,1)
      do I=1,NS
      FMS (I)= RFM
      enddo
      if ( MONF .gt. 1 ) call write shots for river flow
      return
      end


*     compute the Monte Carlo sampling errors for river flow -------------------
*     this is for log-normally distributed river flow --------------------------
      subroutine bias in log normal river flows ( GRFM, GRFS )
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FMS(IS) = 0.0
      enddo      
      
      do is = 1, NS ! calculate log normal river flows
      imonth = qmonth (is) ! set the month for this shot
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = EXP ( GRFM + fdln * GRFS) - RF3      
      enddo
      if ( MONF .gt. 1 ) call write shots for river flow
    
      call calculate summaries of river flow
      
      BM(1) = FLOW(1)
      BS(1) = FLOW(2)
      return
      end

*     compute the Monte Carlo sampling errors for river flow -------------------
*     this is for log-normally distributed river flow --------------------------
      subroutine bias in log normal river flows two ( GRFM, GRFS )
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      
      do is = 1, NS ! calculate log normal river flows
      imonth = qmonth (is) ! set the month for this shot
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FTMS (is) = EXP ( GRFM + fdln * GRFS) - RF3      
      enddo
      if ( MONF .gt. 1 ) call write shots for river flow
    
      call calculate summaries for removed flows
      
      BM(1) = FLOW(1)
      BS(1) = FLOW(2)
      return
      end

      subroutine bias in normal flow removals
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      
      do is = 1, NS ! calculate normal flow removals
      imonth = qmonth (is) ! set the month for this shot
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FTMS (is) = GEFM + fdln * GEFS - RF3      
      enddo
      if ( MONF .gt. 1 ) call shots for removed flows
    
      call calculate summaries for removed flows
      
      BM(1) = FLOW(1)
      BS(1) = FLOW(2)
      return
      end
      
      
*     compute the Monte Carlo sampling errors for river flow -------------------
*     this is for log-normally distributed river flow --------------------------
      subroutine bias in log normal effluent flows ( GRFM, GRFS )
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FMS(IS) = 0.0
      enddo      
      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = EXP ( GRFM + fdln * GRFS) - RF3      
      enddo
    
      call calculate summaries of river flow
      
      BM(1) = FLOW(1)
      BS(1) = FLOW(5)
      return
      end
      
      subroutine bias in normal effluent flows ( GRFM, GRFS )
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FMS(IS) = 0.0
      enddo      
      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      fdln = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = GRFM + fdln * GRFS - RF3      
      enddo
    
      call calculate summaries of river flow
      
      BM(1) = FLOW(1)
      BS(1) = FLOW(5)
      return
      end

      
      
      
      
      
*     compute the Monte Carlo sampling errors for river flow ...
*     this is for normally distributed river flow ....
*     --------------------------------------------------------------------------
      subroutine bias in normal river flows ( GRFM, GRFS )
      include 'COM.FOR'
      BM(1) = 0.0
      BS(1) = 0.0
      do IS = 1, NS
      FMS (IS) = 0.0
      enddo      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the random normal deviate ----------------------------------------BIAS
      fdnd = FRAN function (is) ! get the random normal deviate ----------------
      FMS (is) = GRFM + fdnd * GRFS - RF3
      enddo
      call calculate summaries of river flow
      BM(1) = FLOW(1)
      BS(1) = FLOW(2)
      return
      end
      
*     compute the Monte Carlo sampling errors for a tributary (Feature) -------
*     this is for log-normally distributed river flow -------------------------
      subroutine bias in log normal negative river flows (GM,GS,R3)
      include 'COM.FOR'
      BM(3) = 0.0
      BS(3) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = EXP ( GM + R3 * GS) - R3
      enddo
      call calculate summaries for removed flows
      BM(3)=FLOW(1) ! mean
      BS(3)=FLOW(2) ! 95-percentile
      return
      end
     
*     compute the Monte Carlo sampling errors for a tributary (Feature) -------
*     this is for log-normally distributed river flow -------------------------
      subroutine bias in log normal negative discharge flows (GM,GS,R3)
      include 'COM.FOR'
      BM(3) = 0.0
      BS(3) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = EXP ( GM + R3 * GS ) - RF3
      enddo
      call calculate summaries for removed flows
      BM(3)=FLOW(1) ! mean
      BS(3)=FLOW(5) ! standard deviation
      return
      end
      
      subroutine bias in normal negative discharge flows (GM,GS,R3)
      include 'COM.FOR'
      BM(3) = 0.0
      BS(3) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = GM + R3 * GS - R3
      enddo
      call calculate summaries for removed flows
      BM(3)=FLOW(1) ! mean
      BS(3)=FLOW(5) ! standard deviation
      return
      end
*     --------------------------------------------------------------------------
*     compute the Monte Carlo sampling errors for a bifurcation ----------------
*     this is for log-normally distributed river flow --------------------------
*     --------------------------------------------------------------------------
      subroutine BIASEL bifurcation (IFE, GM, GS, RFX3)
      include 'COM.FOR'
      BM(3) = 0.0
      BS(3) = 0.0
      do IS = 1, NS
      YY(IS) = FMS (IS)
      enddo      
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
*     impose correlation between added flow and the river flow -----------------
      R3 = CO2 * R1 + R3 * SQRMB(102, 1.0 - CO2 * CO2 )
      FMS (is) = EXP ( GM + R3 * GS) - RFX3
      enddo
      if ( MONF .gt. 1 ) call flow shots for bifurcation
      call calculate summaries of river flow
      BM(3) = FLOW(1)
      if ( IFE .eq. 1 ) then 
      BS(3) = FLOW(2)
      else
      BS(3) = FLOW(5)
      endif
      do IS = 1, NS
      FMS(IS) = YY(IS)
      enddo      
      return
      end
*     --------------------------------------------------------------------------
*     compute the Monte Carlo sampling errors for a tributary (Feature) --------
*     this is for normally distributed river flow ------------------------------
*     --------------------------------------------------------------------------
      subroutine bias in normal tributary flows ( GRFM, GRFS)
      include 'COM.FOR'
      BM(3) = 0.0
      BS(3) = 0.0
      do IS = 1, NS
      FTMS(IS) = 0.0
      enddo      
      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      call get the correlated random numbers (IS, R1, R2, R3, R4)
      FTMS (is) = GRFM + R3 * GRFS - RF3
    2 continue
      call calculate summaries for removed flows
      BM(3) = FLOW(1)
      BS(3) = FLOW(2)
      return
      end
*     --------------------------------------------------------------------------
*     compute values of randomly selected variables ----------------------------
      function VALFL month (R,GS,GM,A3)
      value = EXP(R*GS+GM) - A3
      VALFL month = value
      return
      end
*     --------------------------------------------------------------------------
*     compute values of randomly selected variables ----------------------------
      function Vlogn month (R,GS,GM,A3,AM,AS)
      if ( AS .lt. 1.0e-15 ) then
      Vlogn month = AM
	return
 	endif
      value = EXP(R*GS+GM) - A3
      Vlogn month = value
      return
      end
*     --------------------------------------------------------------------------
*     compute values of randomly selected variables ----------------------------
      function Vnorm month (R,GS,GM,A3,AM,AS)
      if ( AS .lt. 1.0e-15 ) then
      Vnorm month = AM
	return
 	endif
      value = R*GS+GM - A3
      Vnorm month = value
      return
      end
*     --------------------------------------------------------------------------
      function VALFL (R, GS, GM, FSH, BM, B5, R5 ) ! log normal
      X = amax1 ( 1.0e-9, EXP(R*GS+GM) - FSH )
      if ( X .gt. 1.2 * R5 ) X = 1.0004 * BM * X
      if ( X .lt. 1.2 * R5 ) X = B5 * X
	VALFL = X
      return
      end
*     --------------------------------------------------------------------------
      function VALFN2 (R,GS,GM,FSH) ! Richard III
      VALFN2 = R * GS + GM - FSH
      if ( VALFN2 .lt. 1.0E-10 ) VALFN2 = 1.0E-10
      return
      end
      function VALFN (R, GS, GM, FSH, BM, B5, R5 ) ! Richard III not used
      X = amax1 ( 1.0e-9, (R*GS+GM) - FSH )
      if ( X .gt. 1.2 * R5 ) X = 1.0004 * BM * X
      if ( X .lt. 1.2 * R5 ) X = B5 * X
	VALFN = X
      return
      end


*     generate quality shots for head of reach ... Log-Normal Distribution -----
      subroutine generate log normal river quality
      include 'COM.FOR'

      if ( MONQ .gt. -1 ) then
      if ( nobigout .le. 0 ) then
*     write(01,10)DNAME(JP)
*     write(33,10)DNAME(JP)
*  10 format(77('-')/
*    &'Generated river quality data for ',A11/77('-'))

      call sort format 2 (quolity data(IQ,JP,1),
     &quolity data(IQ,JP,2))
      if ( PDRC (IQ,JP) .ne. 7 ) then
*     write(01,11)valchars10,UNITS(JP),valchars11,UNITS(JP)
*     write(33,11)valchars10,UNITS(JP),valchars11,UNITS(JP)
*  11 format('Input data:',10X,'              Mean =',a10,1x,a4/
*    &       '           ',10X,'Standard deviation =',a10,1x,a4)
*     if ( PDRC (IQ,JP) .ne. 3 ) goto 318
*     call sort format 1 (quolity data(IQ,JP,3))
*     write(01,317)valchars10,UNITS(JP)
*     write(33,317)valchars10,UNITS(JP)
* 317 format('           ',10x,'             Shift =',a10,1x,a4)
* 318 continue
      else
      write(01,81)valchars10,LUNITS(JP),valchars11,LUNITS(JP)
*     write(33,81)valchars10,LUNITS(JP),valchars11,LUNITS(JP)
   81 format('Input data:',10X,'         Mean load =',a10,1x,a4/
     &       '           ',10X,'Standard deviation =',a10,1x,a4)

      if ( PDRC (IQ,JP) .eq. 3 ) then
      call sort format 1 (quolity data(IQ,JP,3))
      write(01,387)valchars10,LUNITS(JP)
*     write(33,387)valchars10,LUNITS(JP)
  387 format('           ',10x,'             Shift =',a10,1x,a4)
      endif
      endif
      endif

      endif

      GRCM = 0.0
      GRCS = 0.0
      RCM = quolity data(IQ,JP,1) ! mean ---------------------------------------
      RCS = quolity data(IQ,JP,2) ! standard deviation -------------------------
      RC3 = quolity data(IQ,JP,3) ! shift --------------------------------------

      if ( RCS .lt. 1.0E-08 ) then
      do IS = 1, NS
      CMS(JP,IS) = RCM
      enddo
      goto 56
      endif

      RC3 = 0.0 ! shift quality ------------------------------------------------
      if ( PDRC(IQ,JP) .eq. 3) RC3 = quolity data(IQ,JP,3)
      RM3 = (RCM + RC3) * (RCM + RC3)
      spcorrRFRC = quolity data (IQ,JP,4) ! river flow on river quality --- QRAN
*     write(01,7522)spcorrRFRC
*7522 format(77('=')/'Correlation of river flow on river quality:',
*    &f8.4/77('='))
      GRCM=ALOG(RM3/SQRoot(1003,(RM3+RCS*RCS))) ! mean of logged data ----------
      GRCS=SQRoot(1004,ALOG(1.0+(RCS*RCS)/RM3)) ! standard deviation -----------

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in log normal river quality ( GRCM, GRCS )

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = RCM/BM(2)
      if ( BS(2) .gt. 1.0E-08 ) BS(2) = RCS/BS(2)
      if ( MONQ .gt. 1 ) then
*	if ( nobigout .le. 0 ) write(01,12)BM(2),BS(2)
*     if ( nobigout .le. 0 ) write(33,12)BM(2),BS(2)
   12 format(77('-')/
     &'Corrections for Monte-Carlo sampling: log-normal quality'/
     &77('-')/'Mean          =',F8.4/
     &'95-percentile =',F8.4/77('-'))
      endif ! if ( MONQ .gt. -1 ) then
      
*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdln = QRAN function ( JP, js, is )
      CMS(JP,is) = Vlogn (qdln,GRCS,GRCM,RC3,BM(2),BS(2),RCM)
      enddo

   56 continue
      if ( MONQ .gt. 1 ) call write generated shots for river quality
      return
      end


*     generate quality shots for Mode 9 ... Log-Normal Distribution 999999999999
      subroutine generate mode 9 river quality (R9M,R9S)
      include 'COM.FOR'

      GRCM = 0.0
      GRCS = 0.0

      if ( R9S .lt. 1.0E-08 ) then
      do IS = 1, NS
      CMS(JP,IS) = R9M
      enddo
      goto 56
      endif

      RM3 = R9M * R9M
      spcorrRFRC = CO1 ! correlation of river flow on river quality 999999999999
      GRCM=ALOG(RM3/SQRoot(1003,(RM3+R9S*R9S))) ! mean of logged data ----------
      GRCS=SQRoot(1004,ALOG(1.0+(R9S*R9S)/RM3)) ! standard deviation -----------

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in log normal river quality ( GRCM, GRCS )

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = RCM/BM(2)
      if ( BS(2) .gt. 1.0E-08 ) BS(2) = RCS/BS(2)
      if ( MONQ .gt. 1 ) then
*	if ( nobigout .le. 0 ) write(21,12)BM(2),BS(2)
*     if ( nobigout .le. 0 ) write(33,12)BM(2),BS(2)
   12 format(77('-')/
     &'Corrections for Monte-Carlo sampling: log-normal quality'/
     &77('-')/'Mean          =',F8.4/
     &'95-percentile =',F8.4/77('-'))
      endif ! if ( MONQ .gt. -1 ) then
      
*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdln = QRAN function ( JP, js, is )
      CMS(JP,is) = Vlogn (qdln,GRCS,GRCM,RC3,BM(2),BS(2),R9M)
      enddo

   56 continue
*     if ( MONQ .gt. -81 ) call write generated shots for river quality
      return
      end



*     generate quality shots for head of reach ------------- normal distribution
      subroutine generate normal river quality
      include 'COM.FOR'

      if ( MONQ .gt. 1 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (quolity data(IQ,JP,1),
     &quolity data(IQ,JP,2))
 	write(01,10)DNAME(JP)
   10 format(77('-')/'Generated river quality data for ',A11/77('-'))
      if ( PDRC (IQ,JP) .ne. 6 ) then
      write(01,11)valchars10,UNITS(JP),valchars11,UNITS(JP)
   11 format('Input data:',10X,'              Mean =',a10,1x,a4/
     &       '           ',10X,'Standard deviation =',a10,1x,a4)
      if ( PDRC (IQ,JP) .ne. 3) goto 318
      call sort format 1 (quolity data(IQ,JP,3))
      write(01,317)valchars10,UNITS(JP)
  317 format('           ',10x,'             Shift =',a10,1x,a4)
  318 continue
      else
      write(01,81)valchars10,LUNITS(JP),valchars11,LUNITS(JP)
   81 format('Input data:',10X,'         Mean load =',a10,1x,a4/
     &       '           ',10X,'Standard deviation =',a10,1x,a4)
      if ( PDRC (IQ,JP) .ne. 3) goto 388
      call sort format 1 (quolity data(IQ,JP,3))
      write(01,387)valchars10,LUNITS(JP)
  387 format('           ',10x,'             Shift =',a10,1x,a4)
  388 continue
      endif
      endif
      endif

*     mean and standard deviation ----------------------------------------------
      GRCM = 0.0
      GRCS = 0.0
      GRCM = quolity data ( IQ, JP, 1 )
      if ( GRCM .lt. 1.0E-8 ) goto 56
      GRCS = quolity data ( IQ, JP, 2 )
      RC3 = quolity data(IQ,JP,3)

      if ( GRCS .lt. 1.0E-8 ) then
      do IS=1,NS
      CMS(JP,IS) = AMAX1(1.0e-8,GRCM)
      enddo
      goto 56
      endif

      RC3 = 0.0 !  shift quality -----------------------------------------------
      if ( PDRC(IQ,JP) .eq. 3) RC3 = quolity data(IQ,JP,3)
      spcorrRFRC = quolity data (IQ,JP,4) ! river flow on river quality --- QRAN

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in normal river quality ( GRCM, GRCS )

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = GRCM/BM(2)
      if ( BS(2) .gt. 1.0E-08 ) BS(2) = GRCS/BS(2)

      if ( MONQ .gt. 1 ) then
	if ( nobigout .le. 0 ) write(01,12)BM(2),BS(2)
   12 format(77('-')/
     &'Corrections for Monte-Carlo sampling errors: normal quality'/
     &77('-')/'Mean               =',F8.3/
     &'Standard deviation =',F8.3/77('-'))
	endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdnd = QRAN function ( JP, js, is )
      CMS(JP,is) = Vnorm (qdnd,GRCS,GRCM,RC3,BM(2),BS(2),GRCM)
      enddo

      if ( MONQ .gt. 1 ) call write generated shots for river quality
   56 continue

      return
      end




*     generate quality shots for head of reach ---------------- constant quality
      subroutine generate constant river quality
      include 'COM.FOR'

      if ( MONQ .gt. 1 ) then
      if ( nobigout .le. 0 ) then
      call sort format 1 (quolity data(IQ,JP,1))
      write(01,10)RNAME(IREACH)
   10 format(/77('-')/'Generated data for head of reach ',
     &'called ... ',A16/77('-'))
      write(01,11)valchars10,UNITS(JP)
   11 format('Input data:           ',10X,'Mean =',a10,1x,a4/)
      endif
      endif

*     mean flow ----------------------------------------------------------------
      RCM = quolity data(IQ,JP,1)
      RCS = 0.0
      RC3 = 0.0

      do i = 1, NS
      CMS(JP,i) = RCM
      enddo

      if ( MONQ .gt. 1 ) call write generated shots for river quality
      return
      end







*     non-parametric distribution of river quality -----------------------------
      subroutine generate non parametric river quality
      logical exists
      include 'COM.FOR'

      if ( MONQ .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,51)DNAME(JP)
   51 format(77('-')/
     &'Generated RIVER quality data for ',A11,
     &'(Non-parametric distribution)   '/
     &77('-'))
      if ( nobigout .le. 0 ) then
      call sort format 2 (quolity data(IQ,JP,1),
     &quolity data(IQ,JP,2))
      write(01,52)valchars10,UNITS(JP),valchars11,UNITS(JP)
   52 format('Input data:',23x,'              Mean =',a10,1x,a4/
     &       '           ',23x,'Standard deviation =',a10,1x,a4/
     &77('-'))
      endif
      endif

*     calculate Monte-Carlo sampling errors ------------------------------------
      call bias in non parametric river quality

      RCM = quolity data(IQ,JP,1)
      RCS = quolity data(IQ,JP,2)
	jdet = JP

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = RCM/BM(2)
      if ( BS(2) .gt. 1.0E-08 ) BS(2) = RCS/BS(2)
      if ( MONQ .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,42)BM(2),BS(2)
   42 format(11x,
     &'Correction factors for Monte-Carlo sampling errors: non-para',
     &'metric'/77('-')/
     $'Mean               =',F8.3/
     &'Standard deviation =',F8.3/77('-'))
      endif

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue
*     no valid code found for the datafile -------------------------------------

      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(
     &77('-')/
     &'*** Error in river quality data ...'/
     &'*** No valid code for the non-parametric dataset ...'/
     &77('-'))
      call stop

*     valid code found ... check datafile exists -------------------------------
    2 continue
      inquire( FILE = Flname(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(2,icod)
      write(01,7163) flnamesmall(2,icod)
      write(09,7163) flnamesmall(2,icod)
      write(33,7163) flnamesmall(2,icod)
 7163 Format(/77('-')/'*** Error in river quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
      if ( ICAL .gt. 3 .and. ICAL .ne. 0 ) then
      if ( nobigout .le. 0 ) write(01,7863) flnamesmall(2,icod)
      write(33,7863) flnamesmall(2,icod)
 7863 format(77('-')/'Non-parametric river quality data ... '/
     &'File: ',a64/77('-'))
      endif
      endif

*     get the file containing the non-parametric data --------------------------
      open(12,FILE = Flname(2,icod), STATUS='OLD')

*     read the file containing the non-parametric data -------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
*     --------------------------------------------------------------------------
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(2,icod)
      write(01,7166) flnamesmall(2,icod)
      write(09,7166) flnamesmall(2,icod)
      write(33,7166) flnamesmall(2,icod)
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(2,icod)
      write(01,7167) flnamesmall(2,icod)
      write(09,7167) flnamesmall(2,icod)
      write(33,7167) flnamesmall(2,icod)
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

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
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &+ rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif

      if ( MONQ .ge. 99 ) then
      write(01,20)DNAME(JP)
   20 format(//120('=')/'Ordered data entered for a non-parametric ',
     &'distribution ',a11/120('='))
      write(01,24)(rfnpvl(IS),IS = 1,nprf)
   24 format(15F8.3)
      write(01,55)
   55 format(120('='))
      endif
      
*	if ( cut off zero quality (JP) .gt. 1.0e-09 ) then
*	write(01,2376)cut off zero quality (JP),flnamesmall(2,icod)
*2376 format('Cut off percentile for river quality =',f12.6,1x,a64)
*	endif



*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdnp = QRAN function ( JP, js, is )
      call get non parametric shot ( qdnp, RC )
	CMS ( JP, IS ) = VALPQ ( RC, RCM, BM(2), 1.0 )
      enddo
*     --------------------------------------------------------------------------

      if ( MONQ .gt. 1 ) call write generated shots for river quality

*     note whether the distribution is non-parametric --------------------------
      if ( PDRC(IQ,JP) .eq. 4 .or. PDRC(IQ,JP) .eq. 9 ) then
*	if ( nobigout .le. 0 ) write(01,1965)dname(JP)
 1965 format(77('-')/
     &'The river quality data for: ',a11,' ...'/
     &'... are from a non-parametric distribution '/77('-'))
	endif
*     --------------------------------------------------------------------------

      return

 7500 write( *,7168) flnamesmall(2,icod)
      write(01,7168) flnamesmall(2,icod)
      write(09,7168) flnamesmall(2,icod)
      write(33,7168) flnamesmall(2,icod)
      call stop

 7166 Format(/77('-')/
     &'*** Error in river quality data ....                   '/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
 7167 Format(/77('-')/
     &'*** Error in river quality data ....                   '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
 7168 Format(77('-')/
     &'*** Error in river quality data ....                   '/
     &'*** Error in reading non-parametric data ...           '/
     &'*** Check the file ',a64/77('-'))

      end










*     --------------------------------------------------------------------------
*     compute Monte-Carlo sampling errors for river quality ...
*     log-normal distributions ...
*     --------------------------------------------------------------------------
      subroutine bias in log normal river quality (GRCM,GRCS)
      include 'COM.FOR'
      BM(2) = 0.0
      BS(2) = 0.0
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
*     get the random normal deviate ----------------------------------------BIAS
      qdln = QRAN function ( JP, js, is )
      RC = EXP(GRCM+qdln*GRCS)-RC3
      BM(2) = BM(2)+RC
      BS(2) = BS(2)+RC*RC
      enddo
      BS(2) = (BS(2)-BM(2)*BM(2)/NS)/(NS-1)
      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2) = SQRoot(1005,BS(2))
      else
      BS(2) = 0.0
      endif
      BM(2) = BM(2)/NS
      return
      end







*     Compute Monte-Carlo sampling errors for river flows ...
*     Non-parametric distributions ...
*     --------------------------------------------------------------------------
      subroutine bias in non parametric river flow
      include 'COM.FOR'
      logical exists

      BM(1) = 0.0
      BS(1) = 0.0

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in river flow data ...'/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     Valid code found. Check datafile exists ----------------------------------

    2 continue
      Inquire( FILE = Flname(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(1,icod)
      write(01,7163) flnamesmall(1,icod)
      write(09,7163) flnamesmall(1,icod)
      write(33,7163) flnamesmall(1,icod)
 7163 Format(/77('-')/
     &'*** Error in river flow data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
	call stop
      else
*     write(01,7863) flnamesmall(1,icod)
*     write(09,7863) flnamesmall(1,icod)
*     write(33,7863) flnamesmall(1,icod)
*7863 format(77('-')/
*    &'Non-parametric input river flow data ... ',
*    &'File: ',a64/77('-'))
      endif

*     get the file containing the non-parametric data --------------------------
      open(12,FILE = Flname(1,icod), STATUS='OLD')

*     read the file containing the non-parametric data -------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
*     --------------------------------------------------------------------------
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(1,icod)
      write(01,7166) flnamesmall(1,icod)
      write(09,7166) flnamesmall(1,icod)
      write(33,7166) flnamesmall(1,icod)
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(1,icod)
      write(01,7167) flnamesmall(1,icod)
      write(09,7167) flnamesmall(1,icod)
      write(33,7167) flnamesmall(1,icod)
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

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
*     --------------------------------------------------------------------------
*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
	imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
	else
      cut off zero flow = rfnpfd (nprf)
	endif
	endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      fdnp = FRAN function (is) ! get the random normal deviate ----------------
      call get non parametric shot ( fdnp, RF )
      BM (1) = BM (1) + RF
      BS (1) = BS (1) + RF * RF
      enddo
      BS(1)=(BS(1)-BM(1)*BM(1)/NS)/(NS-1)
      if ( BS(1) .gt. 1.0e-10 ) then
      BS(1) = SQRoot(1006,BS(1))
      else
      BS(1) = 0.0
      endif
      BM(1)=BM(1)/NS

      return

 7500 write( *,7168) flnamesmall(1,icod)
      write(01,7168) flnamesmall(1,icod)
      write(09,7168) flnamesmall(1,icod)
      write(33,7168) flnamesmall(1,icod)
      call stop

 7166 Format(/77('-')/
     &'*** Error in river flow data ....                      '/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
 7167 Format(/77('-')/
     &'*** Error in river flow data ....                      '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
 7168 Format(77('-')/
     &'*** Error in river flow data ....                      '/
     &'*** Error in reading non-parametric data ...           '/
     &'*** Check the file ',a64/77('-'))
      end




*     non-parametric distribution of river flow --------------------------------
      subroutine generate non parametric river flows
      logical exists
      include 'COM.FOR'

*     mean and 95-percentile low flow ------------------------------------------
      FM = F(IF,1)
      FS = F(IF,2)

      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (FM,FS)
      write(01,51)
   51 format(77('-')/
     &'Generated river flow data (non-parametric) ...'/77('-'))
      write(01,52) valchars10, FUNIT, valchars11, FUNIT
   52 format('Input data:',36x,'         Mean =',a10,1x,a4/
     &       '           ',36x,'95-percentile =',a10,1x,a4/77('-'))
      endif
      endif

      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN

*     calculate the Monte-Carlo sampling errors --------------------------------
      call bias in non parametric river flow

      if (BM(1) .gt. 1.0E-08) BM(1) = FM/BM(1)
      BS(1) = 1.0

      if ( MONF .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,42)BM(1)
   42 format(
     &'Corrections for Monte-Carlo sampling errors: non-parametric ',
     &'distribution'//77('-')'Mean          =',F8.3/77('-'))
	endif

      do 1 i = 1, M7 ! identify the data file
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in river flow data ...'/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ---------------------------------
    2 continue
      inquire( FILE = Flname(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(1,icod)
      write(01,7163) flnamesmall(1,icod)
      write(09,7163) flnamesmall(1,icod)
      write(33,7163) flnamesmall(1,icod)
 7163 Format(/77('-')/
     &'*** Error in river flow data ... '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ... '/77('-'))
      call stop
      else
*     write(01,7863) flnamesmall(1,icod)
*     write(09,7863) flnamesmall(1,icod)
*     write(33,7863) flnamesmall(1,icod)
*7863 format(77('-')/'Non-parametric input river flow data ... ',
*    &'File: ',a64/77('-'))
      endif

*     get the file containing the non-parametric data --------------------------
      open(12,FILE = Flname(1,icod), STATUS='OLD')

*     read the file containing the non-parametric data -------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(1,icod)
      write(01,7166) flnamesmall(1,icod)
      write(09,7166) flnamesmall(1,icod)
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(1,icod)
      write(01,7167) flnamesmall(1,icod)
      write(09,7167) flnamesmall(1,icod)
      write(33,7167) flnamesmall(1,icod)
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
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
*     --------------------------------------------------------------------------
*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
	imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
	else
      cut off zero flow = rfnpfd (nprf)
	endif
	endif
*	write(01,2376)cut off zero flow,flnamesmall(1,icod)
*2376 format('CUT off percentile for river flow =',f12.6,1x,a64)

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      fdnp = FRAN function (is) ! get the random normal deviate ----------------
      call get non parametric shot ( fdnp, RF )
      FMS (IS) = VALPF ( RF, FM, BM(1) )
      FMS (IS) = amax1(0.0, FMS(IS))
      enddo

      if ( MONF .gt. 1 ) call write shots for river flow
            
      return

 7500 write( *,7168) flnamesmall(1,icod)
      write(01,7168) flnamesmall(1,icod)
      write(09,7168) flnamesmall(1,icod)
      write(33,7168) flnamesmall(1,icod)
      call stop

 7166 Format(/77('-')/
     &'*** Error in non-parametric river flow data ... '/
     &'*** Too many data values specified for the non-parametric '/
     &'*** distribution. Check the file ',a64/77('-'))

 7167 Format(/77('-')/
     &'*** Error in non-parametric river flow data ... '/
     &'*** Too few data values specified for the non-parametric'/
     &'*** distribution. Check the file ',a64/77('-'))

 7168 Format(/77('-')/
     &'*** Error in non-parametric river flow data ... '/
     &'*** Error in reading the non-parametric data ... '/
     &'*** Check the file ',a64/77('-'))

      end












*     compute Monte-Carlo sampling errors --------------------------------------
*     normal distributions of river water quality ------------------------------
      subroutine bias in normal river quality (GRCM,GRCS)
      include 'COM.FOR'

      BM(2) = 0.0
      BS(2) = 0.0
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdnd = QRAN function ( JP, js, is )
      RC = GRCM + qdnd * GRCS - RC3
      BM(2) = BM(2)+RC
      BS(2) = BS(2)+RC*RC
      enddo
      BS(2) = (BS(2)-BM(2)*BM(2)/NS)/(NS-1)
      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2) = SQRoot(1008,BS(2))
      else
      BS(2) = 0.0
      endif
      BM(2) = BM(2)/NS
      return
      end



*     compute the Monte-Carlo sampling errors ----------------------------------
      subroutine bias in non parametric river quality
      include 'COM.FOR'
      logical exists

      BM(2) = 0.0
      BS(2) = 0.0

*     identify the file with the non-parametric data ---------------------------

      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------

      write( *,3)IQ
      write(01,3)IQ
      write(09,3)IQ
      write(33,3)IQ
    3 format(
     &77('-')/
     &'*** Error in river quality data ...',i5/
     &'*** No valid code for the non-parametric dataset ....     '/
     &77('-'))
      call stop

*     Valid code found. Check datafile exists ...

    2 continue

      Inquire( FILE = Flname(2,icod), EXIST = exists )

      if ( .NOT. exists) then
      write( *,7163) flnamesmall(2,icod)
      write(01,7163) flnamesmall(2,icod)
      write(09,7163) flnamesmall(2,icod)
      write(33,7163) flnamesmall(2,icod)
 7163 Format(/77('-')/'*** Error in river quality data ...  '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
	call stop
      else
      if ( ICAL .gt. 3 .and. ICAL .ne. 0 ) then
      if ( nobigout .le. 0 ) write(01,7863) flnamesmall(2,icod)
      write(33,7863) flnamesmall(2,icod)
 7863 format(77('-')/'Non-parametric river quality data ... '/
     &'File: ',a64/77('-'))
      endif
      endif

*     get the file containing the non-parametric data --------------------------
      open(12,FILE = Flname(2,icod), STATUS='OLD')

*     read the file containing the non-parametric data -------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
*     --------------------------------------------------------------------------
      kprf = nprf

*     check the number of data points - are there too many ?

      if ( nprf .gt. mprf) then
      write( *,7166)flnamesmall(2,icod)
      write(01,7166)flnamesmall(2,icod)
      write(09,7166)flnamesmall(2,icod)
      write(33,7166)flnamesmall(2,icod)
      call stop
      endif

*     check the number of data points - are there too few ?

      if ( nprf .lt. 5 ) then
      write( *,7167)flnamesmall(2,icod)
      write(01,7167)flnamesmall(2,icod)
      write(09,7167)flnamesmall(2,icod)
      write(33,7167)flnamesmall(2,icod)
      call stop
      endif
      backspace (12)

      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
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
*     --------------------------------------------------------------------------
*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &    + rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif
*	if ( cut off zero quality (JP) .gt. 1.0e-09 ) then
*	write(01,2376)cut off zero quality (JP),flnamesmall(2,icod)
*2376 format('Cut off percentile for river quality =',f12.6,1x,a64)
*     endif

*     sample the distributions ---------------------------------------------BIAS
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      js = JSET ( IQ, NS, is )
      qdnp = QRAN function ( JP, js, is )
      call get non parametric shot ( qdnp, RC )
	BM (2) = BM (2) + RC
	BS (2) = BS (2) + RC * RC
      enddo

      BS(2)=(BS(2)-BM(2)*BM(2)/NS)/(NS-1)
      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2)=SQRoot(1007,BS(2))
      else
      BS(2) = 0.0
      endif
      BM(2)=BM(2)/NS

      return

 7500 write( *,7168) flnamesmall(1,icod)
      write(01,7168) flnamesmall(1,icod)
      write(09,7168) flnamesmall(1,icod)
      write(33,7168) flnamesmall(1,icod)
      call stop

 7166 Format(77('-')/
     &'  *** Error in river quality data ....                   '/
     &'  *** Too many data values specified for non-parametric  '/
     &'  *** distribution. Check the file ',a64/77('-'))
 7167 Format(77('-')/
     &'  *** Error in river quality data .....                  '/
     &'  *** Too few data values specified for non-parametric   '/
     &'  *** distribution. Check the file ',a64/77('-'))
 7168 Format(77('-')/
     &'  *** Error in river quality data .......                '/
     &'  *** Error in reading non-parametric data ...           '/
     &'  *** Check the file ',a64/77('-'))

      end


*     compute values of randomly selected variables ----------------------------
      function Vlogn (R,GS,GM,RC3,BM,BS,RCM)
      XVAL = EXP ( GM + GS * R ) - RC3
      XVlogn = ( BM * XVAL - RCM ) * BS + RCM
*     negat = 0
*     if ( XVlogn .lt. -0.001 ) negat = 1
      if ( XVlogn .lt. (0.001*RCM) ) XVlogn = 0.001*RCM
      if ( Xval .gt. 0.0 ) then
      if ( XVlogn .gt. (2.0 * XVAL) ) XVlogn = XVAL
      endif
      Vlogn = Xvlogn
*     if ( negat .eq. 1 ) write(33,1000)Vlogn,RCM,XVAL,r
*     if ( negat .eq. 1 ) write(09,1000)Vlogn,RCM,XVAL,r
*1000 format('*** A negative shot for river quality has been ',
*    &'set to ',2f12.4,3e16.7)
      return
      end

      function VALPQ (RC, RM, BM, BS )
      if ( RC .lt. 1.0e-16 ) then
	VALPQ = 0.0
	else
      VAL = RC
      VALPQ = AMAX1 (0.0,(BM*VAL-RM)*BS+RM)
      if ( VALPQ .lt. 0.001 * RM ) VALPQ = VAL
      if ( VALPQ .gt. 2.0 * VAL ) VALPQ = VAL
      endif
	return
	end

      function VALPF ( RF, FM, BM )
      if ( RF .lt. 1.0e-16 ) then
      VALPF = 0.0
	else
      VAL = RF
      VALPF = BM * VAL
      if ( VALPF .lt. 0.001 * FM ) VALPF = VAL
      if ( VALPF .gt. 2.0   * VAL ) VALPF = VAL
      endif
      return
      end


      function Vnorm (R,GS,GM,RC3,BM,BS,RCM)
*     compute value of shot ----------------------------------------------------
      VAL = amax1 ( 1.0e-8, ( GM + GS * R - RC3 ) )
*     correct for Monte carlo sampling errors ----------------------------------
      Vnorm = amax1 ( 1.0e-8, ( BM * VAL - RCM ) * BS + RCM )
      if (Vnorm .lt. 0.001*RCM) Vnorm=0.001*RCM
	if (Val .gt. 0.0 ) then
      if (Vnorm .gt.(2.0*VAL) ) Vnorm = VAL
      endif
      return
      end


      subroutine write shots for river quality 
      include 'COM.FOR'
      if ( QTYPE (JP) .eq. 4 ) return

      call get summaries of river quality from the shots

      if ( nobigout .le. 0 ) then
	if ( C(JP,1) .gt. 1.0e-8 ) then
      write(01,49)
   49 format(/150('-'))
	write(01,13) dname(jp)
   13 format('Current shots for river river quality for: ',a11)
      write(01,19)
   19 format(150('-'))
      write(01,14) (CMS(JP,IS),IS=1,NS)
   14 format(f10.2,20F7.2)
      write(01,19)
	write(01,29) C(JP,1), C(JP,2)
   29 format(62x,'Calculated annual mean quality =',f8.3,12x,
     &'...  Standard deviation =',f8.3)       
      write(01,19)
      endif
	endif

      return
      end




      subroutine write generated shots for river quality
      include 'COM.FOR'

      if ( QTYPE (JP) .eq. 4 ) return
      if ( nobigout .le. 0 ) then
      write(01,13) dname(jp)
      write(33,13) dname(jp)
   13 format(150('-')/'Generated shots for river quality for: ',
     &a11/150('-'))
      write(01,14) (CMS(JP,IS),IS=1,NS)
      write(33,14) (CMS(JP,IS),IS=1,NS)
   14 format(f10.2,20F7.2)
      write(01,19)
      write(33,19)
   19 format(150('-'))
      endif

      call get summaries of river quality from the shots

	if ( nobigout .le. 0 ) then
      write(01,29) C(JP,1), C(JP,2)
      write(33,29) C(JP,1), C(JP,2)
   29 format(70x,'Calculated mean quality =',f8.3,
     &       '           ...  Standard deviation =',f8.3)       
      write(01,19)
      write(33,19)
      endif

      return
      end







*     write out the shots for quality of diffuse inflows -----------------------
      subroutine write shots for quality of diffuse inflows
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
	write(01,15)
	write(33,15)
   15 format(150('-'))
      write(01,13) dname(jp)
      write(33,13) dname(jp)
   13 format('Shots for the quality of the diffuse inflows: ',a11)
      write(01,15)
      write(33,15)
      write(01,14) (CMS(JP,IS),IS=1,NS)
      write(33,14) (CMS(JP,IS),IS=1,NS)
   14 format(f10.2,20F7.2)
      write(01,15)
      write(33,15)
      endif

      return
      end





      subroutine write shots for river flow
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
      write(01,13)
   13 format(150('-')/'Current shots for the flow in the main river'/
     &150('-'))
      write(01,14)(FMS(IS),IS=1,NS)
   14 format(f10.1,20F7.1)
      write(01,19)
   19 format(150('-'))
      endif

      call calculate summaries of river flow
	if ( nobigout .le. 0 ) then
      write(01,29)Flow(1),Flow(2)
   29 Format('Calculated mean flow   =',f8.2/
     &       '95-percentile low flow =',f8.2)       
      write(01,22)
   22 format(150('-'))
      endif

      return
      end

      subroutine write shots for river flow two
      include 'COM.FOR'

      call calculate summaries for removed flows

      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(150('-')/'Current shots for the abstraction'/
     &150('-'))
      write(01,2)(FTMS(IS),IS=1,NS)
    2 format(f10.1,20F7.1)
      write(01,3)Flow(1),Flow(2)
    3 format(150('-')/'Calculated mean flow   =',f8.2/
     &'95-percentile low flow =',f8.2/150('-'))
      endif

      return
      end


      subroutine write shots for river flow FRAN
      include 'COM.FOR'

      do is = 1, NS
      FMS(IS)=FRAN(IS)
      enddo

      call calculate summaries of river flow
	if ( nobigout .le. 0 ) write(01,29)Flow(1),Flow(2)
   29 Format('Calculated mean flow   =',f8.2/
     &       '95-percentile low flow =',f8.2)       
      if ( nobigout .le. 0 ) write(01,22)
   22 format(150('-'))
      return
      end


      subroutine write shots for diffuse flow
      include 'COM.FOR'
      if ( nobigout .le. 0 ) write(01,19)
   19 format(159('-'))
      call calculate summaries of river flow
      return
      end


*     special version for abstractions (negative discharges) -------------------
      subroutine shots for removed flows
      include 'COM.FOR'

      call calculate summaries for removed flows
      
      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(150('-')/'Shots for the removed flows'/150('-'))
      write(01,2)(FTMS(IS),IS=1,NS)
    2 format(f10.1,20F7.1)
      write(01,3)Flow(1),Flow(2),Flow(5)
    3 format(150('-')/'Calculated mean removed flow   =',f8.2/
     &'95-percentile low removed flow =',f8.2/
     &'Standard deviation             =',f8.2/150('-'))
      endif
      
      return
      end

*     special version for abstractions (negative discharges) -------------------
      subroutine shots for diffuse flows
      include 'COM.FOR'
      
      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(150('-')/'Shots for the diffuse inflows'/150('-'))
      write(01,2)(FTMS(IS),IS=1,NS)
    2 format(f10.1,20F7.1)
      write(01,3)Flow(1),Flow(2),Flow(5)
    3 format(150('-')/'Calculated mean diffuse flow   =',f8.2/
     &'95-percentile low removed flow =',f8.2/
     &'Standard deviation             =',f8.2/150('-'))
      endif
      
      return
      end


*     special version for bifurcations -----------------------------------------
      subroutine flow shots for bifurcation 20 to 23 (jtypeB)
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
      write(01,13)
      write(21,13)
   13 format(150('-')/'Corrected shots for the diverted flow ...'/
     &150('-'))
      write(01,14)(FMS(IS),IS=1,NS)
      write(21,14)(FMS(IS),IS=1,NS)
   14 format(f10.1,20F7.1)
      write(01,19)
      write(21,19)
   19 format(150('-'))
      endif

      call calculate summaries of river flow
      if ( nobigout .le. 0 ) then
      if ( jtypeB .eq. 20 .or. jtypeB .eq. 21 ) then ! river type --------------
      write(01,29)Flow(1),Flow(2)
      write(21,29)Flow(1),Flow(2)
   29 format(64x,'Calculated mean diverted flow =',f8.2,10x,
     &'... 95-percentile low flow =',f8.2)  
      endif ! river type -------------------------------------------------------
      if ( jtypeB .eq. 22 .or. jtypeB .eq. 23 ) then ! effluent type ===========
      write(01,39)Flow(1),Flow(5)
      write(21,39)Flow(1),Flow(5)
   39 format(64x,'Calculated mean diverted flow =',f8.2,10x,
     &'    ... Standard deviation =',f8.2)  
      endif ! effluent type ====================================================
      write(01,19)
      write(21,19)
      endif
      return
      end

      subroutine flow shots for bifurcation
      include 'COM.FOR'

      if ( nobigout .le. 0 ) then
      write(01,1)
    1 format(150('-')/'Uncorrected shots for the diverted flow ...'/
     &150('-'))
      write(01,2)(FMS(IS),IS=1,NS)
    2 format(f10.1,20F7.1)
      write(01,3)
    3 format(150('-'))
      endif

      call calculate summaries of river flow
      if ( nobigout .le. 0 ) then
      
      if ( JT(feeture) .eq. 20 .or. JT(feeture) .eq. 21 ) then
      write(01,4)Flow(1),Flow(2)
    4 format(64x,'Calculated mean diverted flow =',f8.2,10x,
     &'... 95-percentile low flow =',f8.2) 
      else
      write(01,5)Flow(1),Flow(5)
    5 format(64x,'Calculated mean diverted flow =',f8.2,10x,
     &'   ... Standard deviation =',f8.2)
      endif
      
      write(01,3)
      endif
      return
      end

      subroutine write shots for diffuse inflows
      include 'COM.FOR'
      if ( nobigout .le. 0 ) write(01,21)
      if ( nobigout .le. 0 ) write(01,13)
   13 format('Shots for the ADDED diffuse inflows ...')
      if ( nobigout .le. 0 ) write(01,15)
   15 format(150('-'))
      if ( nobigout .le. 0 ) write(01,14)(distp*FMS(IS),IS=1,NS)
   14 format(f10.2,20F7.2)
      if ( nobigout .le. 0 ) write(01,21)
   21 format(150('-'))

      call calculate summaries of river flow

      if ( nobigout .le. 0 ) write(01,29)distp*Flow(1),distp*Flow(2)
   29 format(66x,'Calculated mean added flow =',f8.2,3x
     &       '... 95-percentile low added flow =',f8.2)       
      if ( nobigout .le. 0 ) write(01,21)
      return
      end

*     Find the sequence number for the generation of random shots --------------
*     This is needed to ensure no spurious correlations between levels of ------
*     each determinand at a site -----------------------------------------------
*     procedure recommended by Paul Crocket (24 September 1992)
*     This also means that each data set for river BOD, for example, has a -----
*     unique set of random numbers ---------------------------------------------

      function JSET ( jstart, jmax, ival )

*     jstart is set to IQ, jmax is usually NS, IVAL is element (IS)
      jj = jstart - 1 + ival

    2 continue
      if ( jj .le. jmax ) goto 1
      jj = jj - jmax
      goto 2
    1 continue

      jset = jj

      return
      end





*     compute Monte Carlo sampling errors for monthly river flow data ----------
      subroutine bias in monthly river flows (imon)
      include 'COM.FOR'
	dimension Y(NS)

      BSM (imon) = 1.0
      BSS (imon) = 1.0
      
      do IS = 1, NS
      Y(IS) = 0.0
      enddo

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      jmonth = imonth
      if ( jmonth .ne. imon ) goto 2
      RFM = seas1 (jmonth) ! mean flow 
      RF5 = seas2 (jmonth) ! 95-percentile low flow
      RF3 = seas3 (jmonth) ! shift flow ----------------------------------------
      spcorrRFaf = seas4 (jmonth) ! correlation of flow on added flow - for FRAN
      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1021,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      fdmm = FRAN function (is) ! get the random normal deviate ------------BIAS
      Y (is) = EXP ( GRFM + fdmm * GRFS) - RF3
    2 continue

	FM1 = 0.0
      FM2 = 0.0
	NUM = 0
	NUM95 = 0

      do IS = 1 , NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
	if ( jmonth .eq. imon ) then
      NUM = NUM + 1
	FM1 = FM1 + Y(IS)
	endif
      enddo

	NUM95 = amax1( 1.0, 0.05 * float (NUM))

      do 9 I = 1,num95
      do 8 J = I + 1 , NUM
      if (Y(I) .lt. Y(J)) goto 8
      FX = Y(I)
      Y(I) = Y(J)
      Y(J) = FX
    8 continue
    9 continue

      FM2 = Y (NUM95)
      FM1 = FM1 / FLOAT(NUM)

      BSM(imon) = FM1
      BSS(imon) = FM2

      return
      end




*     generate monthly data for river and tributary flow -----------------------
      subroutine generate monthly river flow data
      include 'COM.FOR'
      logical exists
	dimension Y(NS)

      mass = 0
*     initialise the values of the shots ---------------------------------------
      do is = 1, NS
	Y(is) = 0.0
	EFshots(is) = 0.0
	enddo

*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------
      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue
*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in river flow data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = flmonth(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163)FLMONTHsmall(1,icod)
      write(01,7163)FLMONTHsmall(1,icod)
      write(09,7163)FLMONTHsmall(1,icod)
      write(33,7163)FLMONTHsmall(1,icod)
 7163 Format(/77('-')/
     &'*** Error in river flow data (monthly)...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
*     if ( JP .eq. ndetlast .or. mass .eq. 0 ) then
*     write(01,7863)FLMONTHsmall(1,icod) 
*     write(33,7863)FLMONTHsmall(1,icod)
*7863 format(///77('0')/'Monthly data on river flow ... ',
*    &'File: ',a64/77('0'))
*	endif
	endif

*     write(01,4398)
*4398 format(
*    &77('0')/
*    &'Monthly data on river flows ...'/77('0')/
*    &'Month        ','       Mean','        95%','      Shift',
*    &'  Correlation'/25x,'  exeedence'/77('0'))

*     get the file containing the monthly data ---------------------------------
      open(11,FILE = flmonth(1,icod), STATUS='OLD')
      call read monthly data (1,icod) ! river flow - type 5
 
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow -------- for FRAN

*     calculate the Monte-Carlo sampling errors for monthly data on river flow -
*     loop through the months --------------------------------------------------
	do 13 imon = 1, 12
*     special version of BIAS for monthly data ---------------------------------
      call bias in monthly river flows (imon)

      if (BSM(imon) .gt. 1.0E-08) BSM(imon) = seas1(imon)/BSM(imon)
      if (BSS(imon) .gt. 1.0E-08) then
      BSS(imon) = seas2(imon)/BSS(imon)
      else
      BSS(imon) = 1.0
      endif

*     if ( MONF .gt. 1 ) then
*     if ( imon .eq. 1 ) then
*     if ( nobigout .le. 0 ) write(01,12) ! Richard III
*  12 format(/77('o')/
*    &'Correction factors for sampling errors on river flow data:'/
*    &77('o'))
*	endif
*     if ( nobigout .le. 0 ) write(01,92)imon,BSM(imon),BSS(imon)  
*  92 format('Monthly Mean',i3,' =',F8.3,
*    &'        95-percentile =',F8.3,' ')
*	endif
   13 continue ! do 13 imon = 1, 12
*     write(01,93)
*  93 format(77('o')/)

*     create the distribution of river flow -----------------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     mean and 95-percentile low flow ------------------------------------------
      RFM = seas1 (imonth)
      RF5 = seas2 (imonth)
*     Shift flow ---------------------------------------------------------------
      RF3 = seas3 (imonth)
*     special correlation coefficient ... flow on added flow -------------------
      spcorrFf = seas4 (imonth)

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1245,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG (RFM+RF3) - 0.5*GRFS*GRFS

*     get the random normal deviate --------------------------------------------
      fdmm = FRAN function (is) ! get the random normal deviate ----------------
*     compute the value of the flow shot ---------------------------------------
      Y(is) = VALFL ( fdmm, GRFS,GRFM,RF3, BSM(imonth),
     &                                   BSS(imonth), RF5 )
      Y(is) = amax1(0.0, Y(is))
   22 continue

      if ( mass .eq. 0 ) then
      do is = 1, NS
      FMS (is) = Y (is)
      enddo
      if ( MONF .gt. 1 ) call write shots for river flow
	else
      do is = 1, NS
	EFshots (is) = Y (is)
	enddo
      endif

      return

 7500 write( *,7168)FLMONTHsmall(1,icod)
      write(01,7168)FLMONTHsmall(1,icod)
      write(09,7168)FLMONTHsmall(1,icod)
      write(33,7168)FLMONTHsmall(1,icod)
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading monthly data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop
      end





*     generate monthly structure on river flow ---------------------------------
      subroutine generate monthly structure for river flows 8 ! type 8
      include 'COM.FOR'
      logical exists

	if ( F(IF,1) .lt. 1.0e-09 ) return
      do is = 1, NS
	YY(is) = 0.0
	enddo

*     identify the file with the monthly data ----------------------------------
      do 1 i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in river flow data ...'/
     &'*** No valid code for the monthly structure ...'/77('-'))
      call stop

*     valid code found ... check the datafile exists --------------------------
    2 continue
      Inquire( FILE = FLSTRUCT(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLSTRUCTsmall(1,icod)
      write(01,7163) FLSTRUCTsmall(1,icod)
      write(09,7163) FLSTRUCTsmall(1,icod)
      write(33,7163) FLSTRUCTsmall(1,icod)
 7163 Format(/77('-')/'*** Error in river flow data ... '/
     &'*** Monthly structure file does not exist ...',a64/
     &'*** Run halted ... '/77('-'))
	call stop
      else
*     write(01,7863) FLSTRUCTsmall(1,icod)
      if ( JP .eq. ndetlast ) then
*     write(33,7863) FLSTRUCTsmall(1,icod)
 7863 format(77('-')/
     &'Monthly structure on river flows ... ',
     &'File: ',a64/77('-'))
	endif
      endif

*     get the file containing the monthly structure ----------------------------
      open(11,FILE = FLSTRUCT(1,icod), STATUS='OLD')

*     read the file containing the monthly structure ---------------------------
      call read monthly structure river flow data ! type 8
     &(1,0,icod,tmean,t95,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4311) FLSTRUCTsmall(1,icod)
      write(33,4311) FLSTRUCTsmall(1,icod)
      write(01,4311) FLSTRUCTsmall(1,icod)
      write(09,4311) FLSTRUCTsmall(1,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a64/77('*'))
	endif

      ktest = 0
	do i = 1, 12
	if ( struct0 (i) .ne. 2 ) then
      ktest = ktest + 1
	struct0 (i) = 2
	endif
	enddo
	if ( ktest .gt. 0 ) then
      if ( ifbatch .eq. 0 ) then
      write( *,4011) FLSTRUCTsmall(1,icod)
	endif
      write(33,4011) FLSTRUCTsmall(1,icod)
      if ( nobigout .le. 0 ) write(01,4011) FLSTRUCTsmall(1,icod)
      write(09,4011) FLSTRUCTsmall(1,icod)
 4011 format(77('*')/
     &'*** Monthly structures have been over-written as ',
     &'log-normal ...'/'*** File: ',a64/77('*'))
	endif

      spcorrRFaf = tcorr ! correlation of flow on added flow ---------- for FRAN

*     sample the distributions of data on river flow ---------------------------
      do 24 is = 1, NS
      YY(is) = 0.0
      imonth = qmonth (is) ! set the month for this shot
*     mean and 95-percentile low flow ------------------------------------------
      RFM = struct1 (imonth)
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = t95 *  RFM / tmean
*     shift flow ---------------------------------------------------------------
      RF3 = struct3 (imonth)
*     special correlation coefficient ... flow on added flow -------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(3019,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      fdms8 = FRAN function (is) ! get the random normal deviate ---------------

*     compute the value of the shot --------------------------------------------
      YY(is) = EXP ( GRFM + fdms8 * GRFS) - RF3
	endif
   24 continue

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4228)tmean,t95,NS
*4228 format('True flow ...',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean / FLOW(1)  
      BSS(imon) = t95 / FLOW(2)  
	enddo
*     write(01,4399)BSM(1),BSS(1)
*     write(33,4399)BSM(1),BSS(1)
*4399 format('Bias ...',2f12.6)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river flow ---------------------------
      do 22 is = 1, NS
      YY(is) = 0.0
      imonth = qmonth (is) ! set the month for this shot
*     mean and 95-percentile low flow ------------------------------------------
      RFM = struct1 (imonth) * BSM(1)
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) * BSS(1)
*     shift flow ---------------------------------------------------------------
      RF3 = struct3 (imonth)
*     special correlation coefficient ... flow on added flow -------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(101933,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      fdms8 = FRAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = VALFL month ( fdms8,GRFS,GRFM,RF3)
      YY(is) = amax1 (0.0, YY(is))
	endif
   22 continue
*     --------------------------------------------------------------------------

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4128)tmean,t95,NS
*4128 format('True flow ...',2f11.2,i11/46('-'))
*     write(33,4528)CM1,CM2
*4528 format('CM        ...',2f11.2/46('-'))

      BSM(1) = BSM(1) * tmean  / CM1 
      if ( CM2 .gt. 0.000001 ) then 
      if ( t95 / CM2 .lt. 99.9 ) then
	BSS(1) = BSS(1) * t95    / CM2
      endif
      endif 
*     write(33,4699)BSM(1),BSS(1),iterate
*4699 format('Correction river flows 8',2f11.5,i7)

      rat1 = CM1 / tmean
	rat2 = CM2 / t95
*     write(33,4199)rat1,rat2,iterate
*4199 format('Convergence ',2f11.5,i7)

      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue

      if ( iterate .gt. 99 ) then
      if ( ifbatch .ne. 1 ) then 
  	call change colour of text (14) ! bright yellow
	write( *,5216)FLSTRUCTsmall(1,icod)
 5216 format('Unable to set up the specified ',
     &'monthly structure of river flow ... ',a64)
      call set screen text colour
	endif
	write(01,5266)FLSTRUCTsmall(1,icod)
	write(09,5266)FLSTRUCTsmall(1,icod)
	write(33,5266)FLSTRUCTsmall(1,icod)
 5266 format(100('-')/'Unable to set up the specified ',
     &'monthly structure of river flow ... ',a64/100('-'))
	endif

      do is = 1, NS
      FMS (is) = YY(is)
      enddo
      if ( MONF .gt. 1 ) call write shots for river flow

      call statistics for monthly river flows (0, CM1, CM2 )

      if ( MONF .gt. 1 ) call write shots for river flow
      return

 7500 write( *,7168) FLSTRUCTsmall(1,icod)
      write(01,7168) FLSTRUCTsmall(1,icod)
      write(09,7168) FLSTRUCTsmall(1,icod)
      write(33,7168) FLSTRUCTsmall(1,icod)
      call stop
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading data for monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end



*     generate monthly structure on data for river flow ------------------------
      subroutine impose monthly structure for river flows (idir) ! type 2
      include 'COM.FOR'

	if ( F(IF,1) .lt. 1.0e-09 ) return
      do is = 1, NS
	YY(is) = 0.0
      EFshots(is) = 0.0
	enddo

	tmean = F(IF,1) ! annual mean
	t95 = F(IF,2) ! 95-percentile
	if ( PDRF(IF) .eq. 3 ) t3 = F(IF,3) ! shift

      if ( t95 .gt. tmean ) then
	if ( idfcheck .eq. 1 ) then
      if ( nobigout .le. 0 ) write(01,8151)t95,IF,tmean
      if ( kerror .eq. 1 .and. ifbatch .ne. 1 ) then
	call change colour of text (21) ! dull red
      write( *,8851)
 8851 format('* Unworkable river flow data found when ',
     &'imposing monthly structure ...')
      call set screen text colour
      endif ! if ( kerror .eq. 1 ) then
      write(09,8151)t95,IF,tmean
      write(33,8151)t95,IF,tmean
 8151 format(77('-')/'*** Unworkable river flow data found when ',
     &'imposing a monthly structure ...'/
     &'*** The 95-percentile low flow of ',f11.5,' in data set',i5/
     &'*** is too big for a mean of ',f11.5/
     &'*** 95-percentile reset to 10% of the mean ...'/77('-'))
	endif
      suppress12 = suppress12 + 1
	t95 = 0.1 * tmean
	t3 = 0.0
	endif

	tcorr = F(IF,4) ! correlation coefficient --------------------------------

      do i = 1,12
*     struct0(i) = 2 ! log normal distribution
      struct3(i) = 0.0 ! initialise shifts
	struct4(i) = -9.9 ! initialise correlation
      enddo
  
      struct1( 1) = 1.9 ! imposed monthly mean for January
      struct1( 2) = 1.6
      struct1( 3) = 1.4
      struct1( 4) = 1.2
      struct1( 5) = 0.6
      struct1( 6) = 0.5
      struct1( 7) = 0.3
      struct1( 8) = 0.4
      struct1( 9) = 0.4
      struct1(10) = 0.8
      struct1(11) = 1.2
      struct1(12) = 1.7 ! imposed monthly mean for December

      tratio = t95 / tmean ! ratio of annual 95-percentile to mean -------------

*     compare the ratio with the smallest average monthly flow ----------------- 
	if ( tratio + 0.5 .gt. struct1 ( 7) ) then
	tadd = tratio - struct1 (7) + 0.2
      struct1( 1) = struct1( 1) - tadd
      struct1( 2) = struct1( 2) - tadd
      struct1( 3) = struct1( 3) - tadd
      struct1( 4) = struct1( 4) - tadd
      struct1( 5) = struct1( 5) + tadd
      struct1( 6) = struct1( 6) + tadd
      struct1( 7) = struct1( 7) + tadd
      struct1( 8) = struct1( 8) + tadd
      struct1( 9) = struct1( 9) + tadd
      struct1(10) = struct1(10) + tadd
      struct1(11) = struct1(11) - tadd
      struct1(12) = struct1(12) - tadd
	endif

      test1 = 0.0
      do imon = 1,12
      test1 = test1 + struct1(imon) - 1.0
 	enddo
 
	struct2( 1) = 0.4
      struct2( 2) = 0.4
      struct2( 3) = 0.4
      struct2( 4) = 0.4
      struct2( 5) = 0.4
      struct2( 6) = 0.4
      struct2( 7) = 0.4
      struct2( 8) = 0.4
      struct2( 9) = 0.4
      struct2(10) = 0.4
      struct2(11) = 0.4
      struct2(12) = 0.4

*     set the means ... set the 95-percentiles ---------------------------------
      do i = 1, 12
      struct1 (i) = struct1 (i) * tmean
      struct2 (i) = struct1 (i) * struct2 (i)
	enddo

      test2 = 0.0

      call check and correct monthly structure correlation coefficients

*     test the monthly data ----------------------------------------------------
      if ( test1 .gt. 0.0001 .or. test1 .lt. -0.0001 ) then
      write(01,2000)test1
      write( *,2000)test1
      write(09,2000)test1
      write(33,2000)test1
 2000 Format(/77('-')/
     &'*** Error in the monthly structure for river flow ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ...',f12.6)
      do i = 1,12
      write(01,2101)i,struct1(i)
 2101 format(i4,f12.4)
	enddo
      call stop
	endif

      if ( test2 .gt. 0.0001 .or. test2 .lt. -0.001 ) then
      write(01,2001)test2
      write( *,2001)test2
      write(09,2001)test2
      write(33,2001)test2
 2001 Format(/77('-')/
     &'*** Error in the monthly structure for river flow ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly percentiles ...',f12.6/77('-'))
	endif

*     correct monthly data for the number of days in a month -------------------
      do imon = 1, 12
      struct1(imon) = (365.0/12.0) * struct1(imon) / 
     &                days in months (imon)
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
      enddo
      if ( monf .gt. 1 ) then
      if ( idir .eq. 0 ) then ! added flows
      write(01,3245) ! added flows
 3245 format(35('-')/'Imposed monthly data on river flow'/35('-'))
      else
      write(01,3945) ! removed flows
 3945 format(35('-')/'Imposed monthly data on removals'/35('-'))
      endif
      write(01,2398)
 2398 format('Month',15x,'Mean',8x,'95%'/26x,'exeedence'/35('-'))
      write(01,8599)(struct1(i),struct2(i),i=1 ,12)
 8599 format(
     &'January ...  ',2f11.2/
     &'February ... ',2f11.2/
     &'March ...    ',2f11.2/
     &'April ...    ',2f11.2/
     &'May ...      ',2f11.2/
     &'June ...     ',2f11.2/
     &'July ...     ',2f11.2/
     &'August ...   ',2f11.2/
     &'September ...',2f11.2/
     &'October ...  ',2f11.2/
     &'November ... ',2f11.2/
     &'December ... ',2f11.2/35('-'))
      endif
      
      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- FRAN

*     sample the distributions of data on river flow ---------------------------
      do 24 is = 1, NS
      EFshots(is) = 0.0
      YY(is) = 0.0
      imonth = qmonth (is) ! set the month for this shot
      RFM = struct1 (imonth) ! mean flow ---------------------------------------
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) ! mean and 95-percentile low flow -----------------
      RF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(2019,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      fdms2 = FRAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = EXP ( GRFM + fdms2 * GRFS) - RF3
	endif
   24 continue

      call statistics for monthly river flows (1, CM1, CM2 )
      
*     write(01,4228)tmean,t95,NS
*4228 format('True flow ...',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean / FLOW(1)  
      BSS(imon) = t95 / FLOW(2)  
	enddo
*     write(01,4399)BSM(1),BSS(1)
*4399 format('Bias ...',2f12.6)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1
*     sample the distributions of data on river flow ---------------------------
      do 22 is = 1, NS
      YY(is) = 0.0
      imonth = qmonth (is) ! set the month for this shot
*     mean and 95-percentile low flow ------------------------------------------
      RFM = struct1 (imonth) * BSM(1)
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) * BSS(1)
*     shift flow ---------------------------------------------------------------
      RF3 = struct3 (imonth)
      if ( RF5 .gt. RFM ) then
	RF5 = 0.99 * RFM
	endif
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- FRAN
      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(3019,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      fdms2 = FRAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = VALFL month ( fdms2,GRFS,GRFM,RF3)
      YY(is) = amax1 (0.0, YY(is))
	endif
   22 continue

      call statistics for monthly river flows (1, CM1, CM2 )
*     write(01,4128)tmean,t95,NS
*4128 format('TRUE FLOW ...',2f11.2,i11/46('-'))

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * t95    / CM2 
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction ...  ',2f11.5,i7)

      rat1 = CM1 / tmean
	rat2 = CM2 / t95
*     write(01,4199)rat1,rat2,iterate
*4199 format('Convergence ... ',2f11.5,i7)

      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue

      if ( iterate .eq. 100 ) then
      suppress12 = suppress12 + 1
      if ( JSKIP .eq. 0 ) then
      if ( idfcheck .eq. 1 .and. ical .ne. 3 ) then
  	call change colour of text (14) ! bright yellow
	if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
      write( *,5466)IF,dname(JP)
 5466 format('* UNABLE to set up an imposed ',
     &'monthly structure on river flow:',i7,1x,a11)
      call set screen text colour
      endif
	write(33,5266)IF
 5266 format(100('-')/'Unable to set up the imposed ',
     &'monthly structure of river flow set:',i7/100('-'))
	write(33,5966)iterate
 5966 format(77('-')/'Iterations in setting up the imposed ',
     &'monthly structure of river flow ...',i4/100('-'))
	endif
	endif
	endif ! if ( iterate .eq. 100 ) then

      do is = 1, NS
      if ( idir .eq. 0 ) then ! added flows
	FMS (is) = YY(is)
      EFshots(is) = YY(is)
      else ! removals
	FTMS (is) = YY(is) 
      endif
      enddo
      if ( monf .gt. 1 ) then
	if ( ifdiffuse .eq. 0 ) then
      if ( idir .eq. 0 ) then ! added flows
      call write shots for river flow
      else
      call write shots for river flow two
      endif
	else
      call write shots for diffuse flow
      endif
      endif

      call statistics for monthly river flows (1, CM1, CM2 )
      if ( monf .gt. 1 ) then
*     write(01,4499)CM1,CM2
*     write(01,4699)BSM(1),BSS(1),iterate
*4499 format('Bias ...',2f12.6)
*     write(01,4128)tmean,t95,NS
      endif
      return

 7500 write( *,7168)
      write(01,7168)
      write(09,7168)
      write(33,7168)
      call stop
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading monthly structure ...'/
     &77('-'))
      end



*     generate monthly structure on river quality ------------------------------
      subroutine generate monthly structure for river quality (mass)
      include 'COM.FOR'
      logical exists

      do is = 1, NS
	YY(is) = 0.0
	enddo

*     identify the file with the monthly monthly structure on river quality ----
      do 1 i = 1, M9
      icod = i
      if ( istruct ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code is found for the data file ---------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** ERROR in river quality data ...'/
     &'*** No valid code for the times series dataset ...'/77('-'))
      call stop

*     a valid code has been found. Check the datafile exists -------------------
    2 continue
      Inquire( FILE = FLSTRUCT(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLSTRUCTsmall(2,icod)
      write(01,7163) FLSTRUCTsmall(2,icod)
      write(09,7163) FLSTRUCTsmall(2,icod)
      write(33,7163) FLSTRUCTsmall(2,icod)
 7163 Format(/77('-')/'*** Error in river quality data ...'/
     &'*** Monthly structure file does not exist ...',a64/
     &'*** Run halted ...'/77('-'))
	call stop
*     else
*     write(01,7863) dname(JP),FLSTRUCTsmall(2,icod)
*     write(33,7863) dname(JP),FLSTRUCTsmall(2,icod)
*7863 format(77('-')/
*    &'Monthly structure for river quality ... ',a11,
*    &' File: ',a30/77('-'))
      endif

*     get the file containing the monthly monthly structure --------------------
      open(11,FILE = FLSTRUCT(2,icod), STATUS='OLD')

*     read the file containing the monthly structure ---------------------------
      call read monthly structure ! river quality - type 8
     &(2,0,icod,tmean,tstdev,t3,tcorr,itest12)

	if ( itest12 .eq. 12 ) then
      write( *,4311) FLSTRUCTsmall(2,icod)
      write(33,4311) FLSTRUCTsmall(2,icod)
      write(01,4311) FLSTRUCTsmall(2,icod)
      write(09,4311) FLSTRUCTsmall(2,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a30/77('*'))
	endif

*     overall special correlation coefficient ... added flow on added quality --
      spcorrRFRC = tcorr ! river flow on river quality -------------------- QRAN

*     set up a first set of shots for subsequent adjustment --------------------
*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot
      RCM = struct1 (imonth) ! mean quality ------------------------------------
      RCS = struct2 (imonth) ! standard deviation ------------------------------
      RC3 = struct3 (imonth) ! shift -------------------------------------------
      spcorrRFRC = struct4 (imonth) ! river flow on river quality --------- QRAN

      GRCM = 0.0 ! mean for logged variables -----------------------------------
      GRCS = 0.0 ! standard deviation for logged variables ---------------------
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122305, ALOG (1.0+(RCS*RCS)/RM3) )
	else
	GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3) )
      endif
	endif

      call get the correlated random numbers (IS,R1,R2,R3,R4)

      js = JSET ( IQ, NS, is )
      qdms = QRAN function ( JP, js, is ) ! get the random normal deviate ------
*     compute the value of the shot --------------------------------------------
      YY(is) = exp ( qdms * GRCS + GRCM ) - RC3
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 0, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True quality ',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(09,4399)BSM(1),BSS(1)
*4399 format('Bias ---     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot
      RCM = struct1 (imonth) * BSM (1) ! mean quality --------------------------
	if ( RCM .gt. 1.0e-9 ) then
      RCS = struct2 (imonth) * BSS (1) ! standard deviation --------------------
      RC3 = struct3 (imonth) ! shift -------------------------------------------
      spcorrRFRC = struct4 (imonth) ! river flow on river quality --------- QRAN

      GRCM = 0.0 ! mean for logged variables -----------------------------------
      GRCS = 0.0 ! standard deviation for logged variables ---------------------
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122304, ALOG (1.0+(RCS*RCS)/RM3) )
	else
	GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100044,RM3) )
      endif
	endif

*     get the random normal deviate --------------------------------------------
      js = JSET ( IQ, NS, is )
      qdms = QRAN function ( JP, js, is )
*     compute the value of the shot --------------------------------------------
      YY(is) = Vlogn month ( qdms,GRCS,GRCM,RC3,RCM,RCS)
      YY(is) = amax1 (0.0, YY(is))
	endif ! if ( RCM .gt. 1.0e-9 )
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 0, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True quality ',2f11.2,i11/46('-'))
*     write(01,4499)CM1,CM2
*4499 format('Bias ...',2f12.6)
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction river quality 2',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
      suppress12 = suppress12 + 1
	if ( kerror .eq. 1 .and. ifbatch .ne. 1 ) then
	call change colour of text (22) ! light blue
	write( *,5866) FLSTRUCTsmall(2,icod)
 5866 format('* Failed to set up monthly structure of river ',
     &'quality ... ',a64)
      call set screen text colour
	endif
	write(33,5266) FLSTRUCTsmall(2,icod)
 5266 format(100('-')/'Failure in setting up the specified ',
     &'monthly structure of river quality ... ',a64/100('-'))
	endif
      call statistics for monthly river quality (0, 0, CM1, CM2 )
  
      if ( mass .eq. 0 ) then
      do is = 1, NS
      CMS(JP,IS) = YY(is)
	enddo
      if ( MONQ .gt. 1 ) call write shots for river quality
	else
      do is = 1, NS
      ECshots(is) = YY(is)
	enddo
      endif
      
      return

 7500 write( *,7168) FLSTRUCTsmall(2,icod)
      write(01,7168) FLSTRUCTsmall(2,icod)
      write(09,7168) FLSTRUCTsmall(2,icod)
      write(33,7168) FLSTRUCTsmall(2,icod)
      call stop
 7168 Format(/77('-')/'*** Error in river quality data ...'/
     &'*** Error in reading monthly data for monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end






*     generate monthly structure on temperature --------------------------------
      subroutine generate monthly structure for temperature 8
      include 'COM.FOR'
      logical exists

      do is = 1, NS
	YY(is) = 0.0
	enddo

*     identify the file with the monthly monthly structure on temperature for --
*     the current reach --------------------------------------------------------
      do 1 i = 1, M10
      icod = i
      tempd = i
      if ( itempp ( 1, i, 1 ) .eq. IREACH ) goto 2
    1 continue

*     no valid code was found for the data file --------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** ERROR in temperature data ...'/
     &'*** No valid code for the times series dataset ...'/77('-'))
      call stop

*     a valid code has been found. Check the datafile exists -------------------
    2 continue
      inquire( FILE = fltemp(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) fltempsmall(1,icod)
      write(01,7163) fltempsmall(1,icod)
      write(09,7163) fltempsmall(1,icod)
      write(33,7163) fltempsmall(1,icod)
 7163 Format(/77('-')/'*** Error in temperature data ...'/
     &'*** Monthly structure file does not exist ...',a64/
     &'*** Run halted ...'/77('-'))
	call stop
      else
*     write(09,7863) fltempsmall(1,icod)
 7863 format(77('-')/
     &'Monthly structure on temperature ... ',
     &'File: ',a30/77('-'))
      endif

*     get the file containing the monthly monthly structure --------------------
      open(11,FILE = fltemp(1,icod), STATUS='OLD')

*     read the file that contains the monthly structure for temperature --------
      call read monthly structure background data ! type 8 ... temperature -----
     &(1,0,tmean,tstdev,t3,tcorr,itest12)

	if ( itest12 .eq. 12 ) then
      write( *,4311) fltempsmall(1,icod)
      write(33,4311) fltempsmall(1,icod)
      write(01,4311) fltempsmall(1,icod)
      write(09,4311) fltempsmall(1,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection for ',
     &'temperature ...'/'*** File: ',a30/77('*'))
	endif

*     overall special correlation coefficient ----------------------------------
*     added flow on added temperature ------------------------------------------
      spcorrft = tcorr

*     set up a first set of shotes for subsequent adjustment ------------------- 
*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

*     mean quality and standard deviation --------------------------------------
      RCM = struct1 (imonth)
      RCS = struct2 (imonth)
*     shift --------------------------------------------------------------------
      RC3 = struct3 (imonth)

*     correlation coefficient ... added flow on added temperature --------------
      spcorrft = struct4 (imonth)

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR5 = TRAN (IS)
      RR6 = spcorrft * RR1 + RR5 * SQRMB(108,1.0-spcorrft*spcorrft)
*     compute the value of the shot --------------------------------------------
      YY(is) = RR6 * RCS + RCM
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 0, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True quality ',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(01,4399)BSM(1),BSS(1)
*4399 format('Bias ---     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

*     mean river quality -------------------------------------------------------
      RCM = struct1 (imonth) * BSM (1)
	if ( RCM .gt. 1.0e-9 ) then
      RCS = struct2 (imonth) * BSS (1)

*     correlation coefficient ... added flow on added temperature --------------
      spcorrft = struct4 (imonth)

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR5 = TRAN (IS)
      RR6 = spcorrft * RR1 + RR5 * SQRMB(109,1.0-spcorrft* spcorrft)

*     compute the value of the shot --------------------------------------------
      YY(is) = Vnorm month ( RR6,RCS,RCM,RC3,RCM,RCS)
      YY(is) = amax1 (0.0, YY(is))
	endif
      enddo

*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 1, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True temperature ',2f11.2,i11/52('-'))
*     write(01,4499)CM1,CM2
*4499 format('Bias for temperature ...   ',2f11.5)
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction for temperature',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
      suppress12 = suppress12 + 1
	if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
	call change colour of text (22) ! light blue
	write( *,5266)
 5266 format('* Failed to set up the structure for temperature',
     &' ...')
      call set screen text colour
	endif
	endif
      call statistics for monthly river quality (0, 1, CM1, CM2 )
  
      do is = 1, NS
	BMS(1,IS) = YY(is)
	enddo

      return

 7500 write( *,7168) fltempsmall(1,icod)
      write(01,7168) fltempsmall(1,icod)
      write(09,7168) fltempsmall(1,icod)
      write(33,7168) fltempsmall(1,icod)
      call stop
 7168 Format(/77('-')/'*** Error in temperature data ...'/
     &'*** Error in reading monthly structure ...'/
     &'*** Check the file ',a30/77('-'))
      end




      subroutine generate monthly structure for temperature 2
      include 'COM.FOR'

      do is = 1, NS
	YY(is) = 0.0
	enddo

      tmean = TDEG ! the mean temperature in all the rivers and streams
	tstdev = TSDEV ! the corresponding standard deviation for temperature
	t3 = 0.0 ! shift
	tcorr = tcorf ! the correlation coefficient between temperature and flow

	BC (1,1) = tmean
	BC (1,2) = tstdev

      do i = 1,12 ! set all the monthly data at the annual values
      struct1(i) = tmean
	struct2(i) = tstdev
      struct3(i) = t3
	struct4(i) = tcorr
	enddo
      tratio = tstdev / tmean

*     prepare to impose a monthly structure using the following ratios ---------
      struct1( 1) = 0.75 ! January
      struct1( 2) = 0.85
      struct1( 3) = 0.90
      struct1( 4) = 1.00
      struct1( 5) = 1.10
      struct1( 6) = 1.15 
      struct1( 7) = 1.25
      struct1( 8) = 1.30
      struct1( 9) = 1.10
      struct1(10) = 1.00
      struct1(11) = 0.85
      struct1(12) = 0.75

*     overall special correlation coefficient - flow on added temperature ------
      spcorrft = tcorr

*     set up a first set of shots for subsequent adjustment -------------------- 
*     sample the distributions of data on temperature --------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

      RCM = struct1 (imonth) * tmean ! mean temperature
      RCS = struct1 (imonth) * tstdev ! standard deviation
      RC3 = struct1 (imonth) * t3 ! shift
      spcorrft = struct4 (imonth) ! correlation ... added flow on temperature

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR5 = TRAN (IS)
      RR6 = spcorrft * RR1 + RR5 * SQRMB(108,1.0-spcorrft*spcorrft)
      YY(is) = RR6 * RCS + RCM ! compute the value of the shot
      enddo

      call statistics for monthly river quality (0, 1, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True temperature   ',2f11.2,i11/52('-'))
*     write(01,4499)CM1,CM2
*4499 format('Calculated (1)     ',2f11.5)
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(01,4399)BSM(1),BSS(1)
*4399 format('Correction (1)     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

*     mean river quality -------------------------------------------------------
      RCM = struct1 (imonth) * BSM (1) * tmean
	if ( RCM .gt. 1.0e-9 ) then
      RCS = struct1 (imonth) * BSS (1) * tstdev
      RC3 = struct1 (imonth) * t3

      spcorrft = struct4 (imonth) ! correlation coefficient ... flow on temperature

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR5 = TRAN (IS)
      RR6 = spcorrft * RR1 + RR5 * SQRMB(109,1.0-spcorrft* spcorrft)
*     compute the value of the shot --------------------------------------------
      YY(is) = Vnorm month ( RR6,RCS,RCM,RC3,RCM,RCS)
      YY(is) = amax1 (0.0, YY(is))
	endif
      enddo

*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 1, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True temperature   ',2f11.2,i11/52('-'))
*     write(01,4409)CM1,CM2
*4409 format('Calculated 2)      ',2f11.5)
      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction (2)     ',2f11.5,i7)

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
	if ( ifbatch .ne. 1 ) write( *,5266)
 5266 format(77('-')/
     &'Failed to set up a monthly structure for temperature',
     &' ...'/77('-'))
	endif
      call statistics for monthly river quality (0, 1, CM1, CM2 )
  
      do is = 1, NS
 	BMS(1,IS) = YY(is)
 	enddo

      return
      end


      subroutine generate monthly structure for suspended solids 8
      include 'COM.FOR'
      logical exists

      do is = 1, NS
	YY(is) = 0.0
	enddo

*     identify the file with the monthly monthly structure on temperature ------
      do 1 i = 1, M10
      icod = i
      tempd = i
      if ( itempp ( 1, i, 2 ) .eq. IREACH ) goto 2
    1 continue

*     no valid code is found for the data file ---------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** ERROR in suspended solids data ...'/
     &'*** No valid code entered for the set of data ...'/77('-'))
      call stop

*     A valid code has been found. Check the datafile exists -------------------
    2 continue
      Inquire( FILE = fltemp(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) fltempsmall(2,icod)
      write(01,7163) fltempsmall(2,icod)
      write(09,7163) fltempsmall(2,icod)
      write(33,7163) fltempsmall(2,icod)
 7163 Format(/77('-')/'*** Error in suspended solids data ...'/
     &'*** Monthly structure file does not exist ...',a64/
     &'*** Run halted ...'/77('-'))
	call stop
      else
*     write(01,7863) fltempsmall(2,icod)
*     write(09,7863) fltempsmall(2,icod)
 7863 format(77('-')/
     &'Monthly structure on suspended solids ... ',
     &'File: ',a30/77('-'))
      endif

*     get the file containing the monthly monthly structure --------------------
      open(11,FILE = fltemp(2,icod), STATUS='OLD')

*     read the file containing the monthly structure for suspended solids ------
      call read monthly structure background data ! type 8 - suspended solids --
     &(2,0,tmean,tstdev,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4311) fltempsmall(1,icod)
      write(33,4311) fltempsmall(1,icod)
      write(01,4311) fltempsmall(1,icod)
      write(09,4311) fltempsmall(1,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection for ',
     &'suspended solids...'/
     &'*** File: ',a30/77('*'))
	endif

*     overall special correlation coefficient ----------------------------------
*     added flow on added suspended solids -------------------------------------
      spcorrfs = tcorr

*     set up a first set of shotes for subsequent adjustment ------------------- 
*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

*     mean quality and standard deviation --------------------------------------
      RCM = struct1 (imonth)
      RCS = struct2 (imonth)
*     shift --------------------------------------------------------------------
      RC3 = struct3 (imonth)

*     correlation coefficient for added flow on added suspended solids ---------
      spcorrfs = struct4 (imonth)

*     mean and standard deviation for logged variables -------------------------
      GRCM = 0.0
      GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122305, ALOG (1.0+(RCS*RCS)/RM3) )
	else
	GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3) )
      endif
	endif

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR7 = SRAN (IS)
      RR6 = spcorrfs * RR1 + RR7 * SQRMB(110,1.0-spcorrfs*spcorrfs)

*     compute the value of the shot --------------------------------------------
      YY(is) = exp (RR6 * GRCS + GRCM)
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 2, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True quality ',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(01,4399)BSM(1),BSS(1)
*4399 format('Bias ===     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
	YY(is) = 0.0
	imonth = qmonth (is) ! set the month for this shot

*     mean river quality -------------------------------------------------------
      RCM = struct1 (imonth) * BSM (1)
	if ( RCM .gt. 1.0e-9 ) then
      RCS = struct2 (imonth) * BSS (1)
	RC3 = struct3 (imonth)

*     correlation coefficient for added flow on added suspended solids ---------
      spcorrfs = struct4 (imonth)

*     mean and standard deviation for logged variables -------------------------
      GRCM = 0.0
      GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)
      if ( RM3 .gt. 1.0e-9) then
      if ( RCS/RCM .gt. 0.001 ) then
      GRCM = ALOG ( RM3 / SQRoot(100045,RM3+RCS*RCS) )
      GRCS = SQRoot(122306, ALOG (1.0+(RCS*RCS)/RM3) )
	else
	GRCS = 0.0
      GRCM = ALOG ( RM3 / SQRoot(100046,RM3) )
      endif
	endif

*     get the random normal deviate --------------------------------------------
      RR1 = FRAN (IS)
	RR7 = SRAN (IS)
      RR6 = spcorrfs * RR1 + RR7 * SQRMB(111,1.0-spcorrfs*spcorrfs)

*     compute the value of the shot --------------------------------------------
      YY(is) = Vlogn month ( RR6,GRCS,GRCM,RC3,RCM,RCS)
	endif
      enddo

*     --------------------------------------------------------------------------
      call statistics for monthly river quality (0, 2, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True quality ',2f11.2,i11/46('-'))
*     write(01,4499)CM1,CM2
*4499 format('Bias ...',2f12.6)
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction for suspended solids ',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
      suppress12 = suppress12 + 1
	if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
	call change colour of text (12) ! orange
	write( *,5266)
 5266 format('* Failure to set up a monthly structure for suspended',
     &' solids ...')
      call set screen text colour
      endif
	endif
      call statistics for monthly river quality (0, 2, CM1, CM2 )
  
      do is = 1, NS
	BMS(2,IS) = YY(is) ! shots for suspended solids
      enddo

      return

 7500 write( *,7168) fltempsmall(2,icod)
      write(01,7168) fltempsmall(2,icod)
      write(09,7168) fltempsmall(2,icod)
      write(33,7168) fltempsmall(2,icod)
      call stop
 7168 Format(/77('-')/'*** Error in suspended solids data ...'/
     &'*** Error in reading monthly data for monthly structure ...'/
     &'*** Check the file ',a30/77('-'))
      end







*     special version of STATC for monthly (monthly) river quality data --------
*     calculation of mean and standard deviation for river quality -------------
*     from the values of the shots stored in CMS -------------------------------

      subroutine STATsC ( imon, CM1, CM2 )
      include 'COM.FOR'
      dimension Y(MS)

      CM1 = 0.0
      CM2 = 0.0
      NUM = 0

      do IS = 1 , NS
      imonth = qmonth (is) ! set the month for this shot
      jmonth = imonth
      if ( jmonth .eq. imon ) then
      NUM = NUM + 1
      Y(NUM) = CMS ( JP, IS )
      CM1 = CM1 + Y(NUM)
      CM2 = CM2 + Y(num) * Y(num)
      endif
      enddo

      CM2=(CM2-CM1*CM1/NUM)/(NUM-1)

      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1024,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NUM)

      return
      end







*     --------------------------------------------------------------------------
*     special version of STATC for monthly (monthly) river quality data ....
*     calculation of mean and standard deviation for river quality ...
*     from the values of the shots stored in CMS ....
*     --------------------------------------------------------------------------

      subroutine statistics for monthly river quality (iout, iname, 
     &CM1, CM2 )
      include 'COM.FOR'
      dimension YA(MS),YM(MS)
      dimension CM(12), CS(12), XFK (12)

*     copy the shots -----------------------------------------------------------
      do IS = 1, NS
      YA(IS) = YY(IS)
      YM(IS) = YY(IS)
      enddo
*     --------------------------------------------------------------------------

*     initialise monthly means and 95-percentiles ------------------------------
      do imon = 1, 12
      CM (imon) = 0.0
	CS (imon) = 0.0
	XFK (imon) = 0.0
	enddo

*     compute the monthly means and standard deviations ------------------------
      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (IS)
      CM (imon) = CM (imon) + YM(IS)
      CS (imon) = CS (imon) + YM(IS) * YM(IS)
      XFK (imon) = XFK (imon) + 1.0
      enddo
      do imon = 1, 12
      if ( XFK(imon) .gt. 1.99 ) then
      CS(imon)=(CS(imon)-CM(imon)*CM(imon)/XFK(imon))/(XFK(imon)-1.0)
      if ( CS(imon) .gt. 1.0e-10 ) then
      CS(imon) = SQRoot(1924,CS (imon))
      else
      CS(imon) = 0.0
      endif
      CM(imon) = CM(imon) / XFK(imon)
      if ( CS(imon) / CM(imon) .lt. 0.001) then
      CS(imon) = 0.0
      endif
	endif
      enddo
*     --------------------------------------------------------------------------

      if ( iout .eq. 1 ) then
      if ( nobigout .le. 0 ) then
	if ( iname .eq. 0 ) then 
      write(01,4398)dname(jp)
 4398 format(52('-')/
     &'Generated data for ',a11/52('-')/
     &'Month        ',6x,'       Mean','   Standard','     Number',
     &/32x,'deviation',4x,'of days'/52('-'))
	endif
	if ( iname .eq. 1 ) then 
      write(01,4298)
 4298 format(52('-')/
     &'Generated data for temperature ...'/52('-')/
     &'Month        ',6x,'       Mean','   Standard','     Number',
     &/32x,'deviation',4x,'of days'/52('-'))
	endif
	if ( iname .eq. 2 ) then 
      write(01,4198)
 4198 format(52('-')/
     &'Generated data for suspended solids ...'/52('-')/
     &'Month        ',6x,'       Mean','   Standard','     Number',
     &/32x,'deviation',4x,'of days'/52('-'))
	endif

	write(01,8599)(CM (imon),CS (imon),
     &int(XFK(imon)),imon = 1,12)
 8599 format(
     &'January ...  ',6x,2f11.2,i11/
     &'February ... ',6x,2f11.2,i11/
     &'March ...    ',6x,2f11.2,i11/
     &'April ...    ',6x,2f11.2,i11/
     &'May ...      ',6x,2f11.2,i11/
     &'June ...     ',6x,2f11.2,i11/
     &'July ...     ',6x,2f11.2,i11/
     &'August ...   ',6x,2f11.2,i11/
     &'September ...',6x,2f11.2,i11/
     &'October ...  ',6x,2f11.2,i11/
     &'November ... ',6x,2f11.2,i11/
     &'December ... ',6x,2f11.2,i11)
      endif
      endif

      nadd = 0
      yadd = 0.0
      do imon = 1, 12
	if ( XFK (imon) .gt. 0.00001 ) then
      yadd = yadd + CM (imon) * XFK(imon)
	nadd = nadd + XFK(imon)
      endif
	enddo

	yadd = yadd / NS

*     calculate the annual mean and standard deviation -------------------------
      CM1 = 0.0
      CM2 = 0.0
      do is = 1 , NS
      CM1 = CM1 + YM(is)
      CM2 = CM2 + YM(is) * YM(is)
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(2024,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
*     --------------------------------------------------------------------------

      if ( iout .eq. 1 ) then
      if ( nobigout .le. 0 ) then
	if ( iname .eq. 0 ) then 
      write(01,4119)dname(jp),CM1,CM2,NS
 4119 format(52('-')/'Annual ',a11,f12.2,f11.2,i11/52('-'))
	endif
	if ( iname .eq. 1 ) then 
      write(01,4219)CM1,CM2,NS
 4219 format(52('-')/'Annual temperature',f12.2,f11.2,i11/52('-'))
	endif
	if ( iname .eq. 2 ) then 
      write(01,4319)CM1,CM2,NS
 4319 format(52('-')/'Annual susp.solids',f12.2,f11.2,i11/52('-'))
	endif
	endif
      endif

      return
      end








*     --------------------------------------------------------------------------
*     Special version of STATC for monthly (monthly) discharge flow data ....
*     Calculation of mean and standard deviation for discharge flow  ...
*     From the values of the shots stored in EFshots ....
*     --------------------------------------------------------------------------
      subroutine statistics for monthly discharge flows (iout,CM1,CM2)
      include 'COM.FOR'
      dimension YA(MS),YM(MS)
      dimension CM(12), CS(12), XFK (12)

*     copy the shots -----------------------------------------------------------
      do IS = 1,   NS
      YA (IS) = EFshots (IS)
      YM (IS) = EFshots (IS)
      enddo
*     --------------------------------------------------------------------------

*     initialise monthly means and 95-percentiles ------------------------------
      do imon = 1, 12
      CM (imon) = 0.0
	CS (imon) = 0.0
	XFK (imon) = 0.0
	enddo
*     --------------------------------------------------------------------------

*     compute the monthly means and standard deviations ------------------------
      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
      CM (imon) = CM (imon) + YM(IS)
      CS (imon) = CS (imon) + YM(IS) * YM(IS)
	XFK (imon) = XFK (imon) + 1.0
      enddo
      do imon = 1, 12
      CS(imon)=(CS(imon)-CM(imon)*CM(imon)/XFK(imon))/(XFK(imon)-1.0)
      if ( CS(imon) .gt. 1.0e-10 ) then
      CS(imon) = SQRoot(1924,CS (imon))
      else
      CS(imon) = 0.0
      endif
      CM(imon) = CM(imon) / XFK(imon)
      if ( CS(imon) / CM(imon) .lt. 0.001) then
      CS(imon) = 0.0
      endif
      enddo
*     --------------------------------------------------------------------------

      if ( iout .eq. 1 ) then
      write(01,4398)
 4398 format(46('-')/
     &'Monthly data on discharge flow ...'/46('-')/
     &'Month        ','       Mean','   Standard','     Number',
     &/26x,'deviation',4x,'of days'/46('-'))

	write(01,8599)(CM (imon),CS (imon),
     &int(XFK(imon)),imon = 1,12)
 8599 format(
     &'January ...  ',2f11.2,i11/
     &'February ... ',2f11.2,i11/
     &'March ...    ',2f11.2,i11/
     &'April ...    ',2f11.2,i11/
     &'May ...      ',2f11.2,i11/
     &'June ...     ',2f11.2,i11/
     &'July ...     ',2f11.2,i11/
     &'August ...   ',2f11.2,i11/
     &'September ...',2f11.2,i11/
     &'October ...  ',2f11.2,i11/
     &'November ... ',2f11.2,i11/
     &'December ... ',2f11.2,i11)
      endif

      do imon = 1,12
      Cmonth (imon) = CM (imon)
	Cstdev (imon) = CS (imon)
	enddo

      nadd = 0
      yadd = 0.0
      do imon = 1, 12
	if ( XFK (imon) .gt. 0.00001 ) then
      yadd = yadd + CM (imon) * XFK(imon)
	nadd = nadd + XFK(imon)
      endif
	enddo

	yadd = yadd / NS

*     calculate the annual mean and standard deviation -------------------------
      CM1 = 0.0
      CM2 = 0.0
      do is = 1 , NS
      CM1 = CM1 + YM(is)
      CM2 = CM2 + YM(is) * YM(is)
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(2024,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)

      if ( iout .eq. 1 ) then
      write(01,4119)CM1,CM2,NS
 4119 format(46('-')/'Annual        ',f10.2,f11.2,i11/46('-'))
      endif


*     calculate the annual mean and standard deviation -------------------------
      CMA = 0.0
      do imon = 1 , 12
      CMA = CMA + CM (imon)
      enddo
      CMA = CMA / 12.0

      return
      end







*     --------------------------------------------------------------------------
*     Special version of STATC for monthly discharge quality data ....
*     Calculation of mean and standard deviation for discharge quality  ...
*     From the values of the shots stored in ECshots ....
*     --------------------------------------------------------------------------

      subroutine statistics for monthly discharge quality (iout,CM1,CM2)
      include 'COM.FOR'
      dimension YA(MS),YM(MS)
      dimension CM(12), CS(12), XFK (12)

*     copy the shots -----------------------------------------------------------
      do IS = 1,NS
      YA(IS) = ECshots(IS)
      YM(IS) = ECshots(IS)
      enddo
*     --------------------------------------------------------------------------

*     initialise monthly means and 95-percentiles ------------------------------
      do imon = 1, 12
      CM (imon) = 0.0
	CS (imon) = 0.0
	XFK (imon) = 0.0
	enddo
*     --------------------------------------------------------------------------

*     compute the monthly means and standard deviations ------------------------
      do IS = 1, NS
*     set the month for this shot ----------------------------------------------
      imon = qmonth (is)
      CM (imon) = CM (imon) + YM(IS)
      CS (imon) = CS (imon) + YM(IS) * YM(IS)
	XFK (imon) = XFK (imon) + 1.0
      enddo
      do imon = 1, 12
      CS(imon)=(CS(imon)-CM(imon)*CM(imon)/XFK(imon))/(XFK(imon)-1.0)
      if ( CS(imon) .gt. 1.0e-10 ) then
      CS(imon) = SQRoot(1924,CS (imon))
      else
      CS(imon) = 0.0
      endif
      CM(imon) = CM(imon) / XFK(imon)
      if ( CS(imon) / CM(imon) .lt. 0.001) then
      CS(imon) = 0.0
      endif
      enddo
*     --------------------------------------------------------------------------

      if ( iout .eq. 1 ) then
      write(01,4398)
 4398 format(
     &'Monthly data on discharge quality ...'/46('-')/
     &'Month        ','       Mean','   Standard','     Number',
     &/26x,'deviation',4x,'of days'/46('-'))

	write(01,8599)(CM (imon),CS (imon),
     &int(XFK(imon)),imon = 1,12)
 8599 format(
     &'January ...  ',2f11.2,i11/
     &'February ... ',2f11.2,i11/
     &'March ...    ',2f11.2,i11/
     &'April ...    ',2f11.2,i11/
     &'May ...      ',2f11.2,i11/
     &'June ...     ',2f11.2,i11/
     &'July ...     ',2f11.2,i11/
     &'August ...   ',2f11.2,i11/
     &'September ...',2f11.2,i11/
     &'October ...  ',2f11.2,i11/
     &'November ... ',2f11.2,i11/
     &'December ... ',2f11.2,i11)
      endif

      do imon = 1,12
      Cmonth (imon) = CM (imon)
	Cstdev (imon) = CS (imon)
	enddo

      nadd = 0
      yadd = 0.0
      do imon = 1, 12
	if ( XFK (imon) .gt. 0.00001 ) then
      yadd = yadd + CM (imon) * XFK(imon)
	nadd = nadd + XFK(imon)
      endif
	enddo

	yadd = yadd / NS

*     calculate the annual mean and standard deviation -------------------------
      CM1 = 0.0
      CM2 = 0.0
      do is = 1 , NS
      CM1 = CM1 + YM(is)
      CM2 = CM2 + YM(is) * YM(is)
      enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(2024,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)

      if ( iout .eq. 1 ) then
      write(01,4119)CM1,CM2,NS
 4119 format(46('-')/'Annual days   ',f10.2,f11.2,i11/46('-'))
      endif


*     calculate the annual mean and standard deviation -------------------------
      CMA = 0.0
      do imon = 1 , 12
      CMA = CMA + CM (imon)
      enddo
      CMA = CMA / 12.0

      return
      end


      subroutine check and correct monthly correlation coefficients
      include 'COM.FOR'
      do i = 1,12
	if ( seas4(i) .lt. -9.91 .or. seas4(i) .gt. -9.89 ) then  
      if ( seas4(i) .lt. -1.0  .or. seas4(i) .gt.   1.0 ) then
	write(33,1)seas4(i)
    1 format(99('-')/ 
     &'A correlation coefficient for monthly data ',
     &'has been corrected from ...',f8.4,' to zero ...'/99('-'))
	seas4(i) = -9.9 
	endif
      endif
	enddo
      do i = 1,12
	if ( seas4(i) .gt. -9.91 .and. seas4(i) .lt. -9.89 ) then 
      seas4(i) = 0.0 
      endif
	enddo 
      return
	end


      subroutine check and correct monthly structure correlation
     &coefficients
      include 'COM.FOR'
      do i = 1,12
	if ( struct4(i) .lt. -9.91 .or. struct4(i) .gt. -9.89 ) then  
      if ( struct4(i) .lt. -1.0  .or. struct4(i) .gt.   1.0 ) then
	write(33,1)struct4(i)
    1 format(99('-')/ 
     &'A correlation coefficient for monthly structured data ',
     &'has been corrected from..',f8.4,' to zero ...'/99('-'))
	struct4(i) = -9.9 
	endif
      endif
	enddo
	do i = 1,12
	if ( struct4(i) .gt. -9.91 .and. struct4(i) .lt. -9.89 ) then 
	struct4(i) = 0.0 
      endif 
	enddo
      return
	end



      subroutine read monthly data (kind0,icod)
      include 'COM.FOR'

      do i = 1,12
      seas0(i) = 2
      enddo

      read(11, *, ERR=7400,END=7400) (seas0(i),i=1 ,12),
     &(seas1(i),i=1 ,12),(seas2(i),i=1 ,12),
     &(seas3(i),i=1, 12),(seas4(i),i=1, 12)
      call check and correct monthly correlation coefficients
      close (11)
      return

 7400 continue
	rewind 11
*     set log-normal distributions by default ----------------------------------
      do i = 1,12
      seas0(i) = 2
	enddo
      read(11, *, ERR=7500) (seas1(i),i=1 ,12),
     &(seas2(i),i=1 ,12),(seas3(i),i=1, 12), (seas4(i),i=1, 12)
      call check and correct monthly correlation coefficients
      close (11)
      return
    
 7500 write( *,7168)FLMONTHsmall(kind0,icod)
      write(01,7168)FLMONTHsmall(kind0,icod)
      write(09,7168)FLMONTHsmall(kind0,icod)
      write(33,7168)FLMONTHsmall(kind0,icod)
      close (11)
      call stop

 7168 Format(/77('-')/
     &'*** Error in reading the monthly data ...'/
     &'*** Check the file ',a64/77('-'))
	end







*     --------------------------------------------------------------------------
      subroutine read monthly structure river flow data ! type 8
     &(kind0,iout,icod,tmean,t95,t3,tcorr,itest)
      include 'COM.FOR'

*     if ( nobigout .le. 0 ) iout = 1
      
*     set log-normal distributions by default ----------------------------------
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo

      read(11, *, ERR=7400)itdist,tmean,t95,t3,tcorr
      read(11, *, ERR=7400,END=7400) (struct0(i),i=1 ,12),
     &(struct1(i),i=1 ,12),(struct2(i),i=1 ,12),
     &(struct3(i),i=1, 12),(struct4(i),i=1, 12)
      goto 7777
 7400 continue
	rewind 11
      read(11, *, ERR=7400)itdist,tmean,t95,t3,tcorr
      read(11, *, ERR=7500) (struct1(i),i=1 ,12),
     &(struct2(i),i=1 ,12),(struct3(i),i=1, 12), (struct4(i),i=1, 12)
 7777 continue

      if ( itdist .eq. 2 .or. itdist .eq. 3 ) then
      if ( t95 .gt. tmean ) then
      suppress12 = suppress12 + 1
      write(01,8151)IF,t95,tmean,FLSTRUCTsmall(kind0,icod)

      if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
	call change colour of text (21) ! dull red
      write( *,8851)
 8851 format('* Unworkable river flow data found when ',
     &'imposing monthly structure ...')
      call set screen text colour
      endif
      call sort format 2 (t95,tmean)
      write(09,8151)IF,valchars10,valchars11,FLSTRUCTsmall(kind0,icod)
      write(33,8151)IF,valchars10,valchars11,FLSTRUCTsmall(kind0,icod)
 8151 format(77('-')/'*** Unworkable data on monthly structures for ',
     &'river flow...',i8/
     &'The 95-percentile low flow of ',a10/
     &'The 95-percentile was reset to 10% of the mean ...'/
     &'*** exceeds the mean of ',a10' ... ','File: ',a64/77('-'))
	t95 = 0.1 * tmean
	call stop
	endif
	endif

*     test for a zero value of the 95-percentile -------------------------------
      if ( t95 .lt. 0.000001 * tmean ) then
      t95 = 0.0001 * tmean
      write(01,2850) FLSTRUCTsmall(kind0,icod)
      if ( ifbatch .ne. 1 ) write( *,2850)FLSTRUCTsmall(kind0,icod)
      write(09,2850) FLSTRUCTsmall(kind0,icod)
      write(33,2850) FLSTRUCTsmall(kind0,icod)
 2850 Format(77('-')/'*** Unworkable data on monthly structures for ',
     &'river flow...'/
     &'*** Zero annual 95-percentile set to 0.0001% of annual mean ...'/
     &'*** Reverted to the annual data ... ',
     &'File: ',a64/77('-'))
	endif

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if (struct1(imon) .lt. 0.0000001 ) itest = itest + 1
	enddo
	if ( itest .eq. 12 ) then
      suppress12 = suppress12 + 1
      write(01,2050)FLSTRUCTsmall(kind0,icod)
      if ( ifbatch .eq. 0 ) then
      call change colour of text (10) ! bright green
      write( *,2251)FLSTRUCTsmall(kind0,icod)
 2251 Format('* Unnecessary monthly structures ...',15x,'...',7x,
     &'Eg: ',a64)
      call set screen text colour
	endif
      write(09,2050)FLSTRUCTsmall(kind0,icod)
      write(33,2050)FLSTRUCTsmall(kind0,icod)
 2050 Format(77('-')/'*** Unnecessary structure for river ',
     &'flow ... the monthly adjustments are zero '/
     &'*** Reverted to the annual data ... ',
     &'file: ',a64/77('-'))
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo
	return
	endif

      call check and correct monthly structure correlation coefficients
      close (11)

*     ##########################################################################
      if ( iout .eq. 1 ) then
      call sort format 2 (tmean,t95)
	write(01,3045)valchars10,funit,valchars11,funit
 3045 format(/77('-')/'Annual flow ...',41x,'Mean =',a10,1x,a4/
     &38X,'95-percentile low flow =',a10,1x,a4)
	endif
*     ##########################################################################

*     test the monthly data ----------------------------------------------------
      test1 = 0.0
      test2 = 0.0
      do imon = 1,12
      test1 = test1 + struct1(imon)
      test2 = test2 + struct2(imon)
	enddo

      if ( test1 .gt. 0.0001 .or. test1 .lt. -0.001 ) then
      write(01,2000)test1,FLSTRUCTsmall(kind0,icod)
      write( *,2000)test1,FLSTRUCTsmall(kind0,icod)
      write(09,2000)test1,FLSTRUCTsmall(kind0,icod)
      write(33,2000)test1,FLSTRUCTsmall(kind0,icod)
 2000 Format(/77('-')/
     &'*** Error in the monthly structure for river flow ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ...',f12.6/
     &'*** Check the file ',a64/77('-'))
      call stop
	endif

      if ( test2 .gt. 0.0001 .or. test2 .lt. -0.001 ) then
      write(01,2001)test2,FLSTRUCTsmall(kind0,icod)
      write( *,2001)test2,FLSTRUCTsmall(kind0,icod)
      write(09,2001)test2,FLSTRUCTsmall(kind0,icod)
      write(33,2001)test2,FLSTRUCTsmall(kind0,icod)
 2001 Format(/77('-')/
     &'*** Error in the monthly structure for river flow ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly percentiles ...',f12.6/
     &'*** Check the file ',a64/77('-'))
      call stop
	endif

*     set the means ... set the standard deviations to zero --------------------
      do imon = 1,12
	struct1(imon) = tmean + struct1(imon)
      struct2(imon) = t95 + struct2(imon)
	enddo

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if (struct1(imon) .lt. 0.0000001 ) itest = itest + 1
	enddo
	if ( itest .eq. 12 ) then
      write(01,2150) FLSTRUCTsmall(kind0,icod)
      write( *,2150) FLSTRUCTsmall(kind0,icod)
      write(09,2150) FLSTRUCTsmall(kind0,icod)
      write(33,2150) FLSTRUCTsmall(kind0,icod)
 2150 Format(77('-')/'*** Unusable monthly structure ...'/
     &'*** Some monthly means are zero ',
     &'... reverted to the annual data ... '/
     &'*** File: ',a64/77('-'))
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo
	return
	endif
 
*     correct monthly data for the number of days in a month -------------------
      do imon = 1, 12
      struct1(imon) = (365.0/12.0) * struct1(imon) / 
     &                days in months (imon)
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo
      if ( iout .eq. 1 ) then
      write(01,3245) FLSTRUCTsmall(kind0,icod)
 3245 format(77('-')/'Monthly data on river flow (corrected for ',
     &'the numbers of days in months)'/'File: ',a64/77('-'))
      write(01,2398)
 2398 format('Month',15x,'Mean',8x,'95%',6x,'Shift',2x,
     &'Correlation',11x,'Type of'/26x,'exeedence',30x,
     &'distribution'/77('-'))
*     write(01,8599)(struct1(i),struct2(i),struct3(i),struct6(i),
*    &struct0(i),i=1,12)
 8599 format(
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
*     --------------------------------------------------------------------------
      return
     
 7500 write( *,7168) FLSTRUCTsmall(kind0,icod)
      write(01,7168) FLSTRUCTsmall(kind0,icod)
      write(09,7168) FLSTRUCTsmall(kind0,icod)
      write(33,7168) FLSTRUCTsmall(kind0,icod)
      close (11)
      call stop
 7168 Format(/77('-')/
     &'*** Error in reading the monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
	end






*     read data that are set up as a monthly structure from monthly data -------
      subroutine read monthly structure ! river & effluent flow & quality ---- 8
     &(kind0,iout,icod,tmean,tstdev,t3,tcorr,itest)
      include 'COM.FOR'

*     set a log-normal distributions by default --------------------------------
      do i = 1, 12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo

*     read the data file -------------------------------------------------------
      read(11, *, ERR=7400)itdist,tmean,tstdev,t3,tcorr
      read(11, *, ERR=7400,END=7400)(struct0(i),i=1 ,12) ! type of distribution
      read(11, *, ERR=7400,END=7400)(struct1(i),i=1 ,12) ! mean
      read(11, *, ERR=7400,END=7400)(struct2(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct3(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct4(i),i=1 ,12) ! correlation coefficient
      goto 7777
 7400 continue
	rewind 11
      read(11, *, ERR=7500,END=7700)itdist,tmean,tstdev,t3,tcorr
      read(11, *, ERR=7500,END=7700)(struct1(i),i=1 ,12),
     &(struct2(i),i=1 ,12),(struct3(i),i=1, 12), (struct4(i),i=1, 12)
      do i = 1, 12
      struct0(i) = 2
	enddo
 7777 continue

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if (abs(struct1(imon)) .lt. 0.0000001 ) itest = itest + 1
	enddo
	if ( itest .eq. 12 ) then
      write(33,2050) FLSTRUCTsmall(kind0,icod)
      if ( kind0 .eq. 2 ) suppress7 = suppress7 + 1
      if ( suppress7 .lt. 5 .and. ical .ne. 3 ) then
      write(01,2050) FLSTRUCTsmall(kind0,icod)
      if ( ifbatch .eq. 0 ) then
      call change colour of text (10) ! bright green
      write( *,2250) FLSTRUCTsmall(kind0,icod)
 2250 Format('* Unnecessary monthly structures ...',15x,'...',7x,
     &'Eg: ',a64)
      call set screen text colour
	endif
      write(09,2050) FLSTRUCTsmall(kind0,icod)
 2050 Format(77('-')/'*** Unnecessary monthly structure ... ',
     &'the adjustments of mean are zero '/
     &'*** Reverted to the annual data ... ',
     &'FILE: ',a64/77('-'))
      if ( suppress7 .eq. 4 ) then
      write(01,2350) 
      write(09,2350) 
 2350 Format('*** Plus other files ...'/77('-'))
      endif
      endif
      
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo
	return
	endif

      call check and correct monthly structure correlation coefficients
      close (11)

*     test the monthly data are compatible with the annual data ----------------
      test1 = 0.0
      do imon = 1,12
      test1 = test1 + struct1(imon)
	enddo
      if ( test1 .gt. 0.0001 .or. test1 .lt. -0.001 ) then
      write(01,2000)test1,FLSTRUCTsmall(kind0,icod),kind0
      write( *,2000)test1,FLSTRUCTsmall(kind0,icod),kind0
      write(09,2000)test1,FLSTRUCTsmall(kind0,icod),kind0
      write(33,2000)test1,FLSTRUCTsmall(kind0,icod),kind0
 2000 Format(/77('-')/'*** Error in the monthly structure ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ... the surplus is',f12.6/
     &'*** Check the file ',a64,i3/77('-'))
      call stop
	endif

*     set the monthly distributions of load to equal those for the annual data -
      if ( itdist .eq. 6 .or. itdist .eq. 7 ) then
      do imon = 1, 12
	struct0(imon) = itdist
	enddo
	endif

*     set the means ... set the standard deviations to zero --------------------
      do imon = 1,12
	struct1(imon) = tmean + struct1(imon)
      struct2(imon) = 0.0
	enddo

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if (struct1(imon) .lt. 0.0000001 ) itest = itest + 1
	enddo
	if ( itest .eq. 12 ) then
      write(01,2150) FLSTRUCTsmall(kind0,icod)
      write( *,2150) FLSTRUCTsmall(kind0,icod)
      write(09,2150) FLSTRUCTsmall(kind0,icod)
      write(33,2150) FLSTRUCTsmall(kind0,icod)
 2150 Format(77('-')/'*** Unusable monthly structure for data ...'/
     &'*** Some monthly means are zero ',
     &'... reverted to the annual data ... '/
     &'*** File: ',a64/77('-'))
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo
	return
	endif

*     check the standard deviation implied by the monthly means ----------------
      test1 = 0.0
      test2 = 0.0
      do imon = 1, 12
      test1 = test1 + struct1(imon)
      test2 = test2 + struct1(imon) * struct1(imon)
	enddo
      test2=(test2-test1*test1/12.0)/(12.0-1.0)
      if ( test2 .gt. 1.0e-10 ) then
      test2=SQRoot(1267,test2)
      else
      test2 = 0.0
      endif
      test1 = test1 / 12.0
      if ( 1.05 * test2 .gt. tstdev ) then
      suppress9 = suppress9 + 1 ! failure to set up monthly structure
      if ( suppress9 .lt. 7 .and. ical .ne. 3 .and. ical .ne. 1 ) then
      call sort format 2 (tstdev,1.20 * test2)
      call change colour of text (34) ! dull yellow
      write( *,2393)FLSTRUCTsmall(kind0,icod)
 2393 Format(
     &'* Problem in the monthly structure',17x,'...',7x,'for: ',a30,
     &9x,'st.deviation changed ....')
      call set screen text colour
      endif
	if ( suppress9 .eq. 7 .and. ical .ne. 3 ) then
      call change colour of text (12) ! orange
      write( *,2292) 
 2292 Format('* Multiple warnings on monthly structures',10x,'...',
     &7x,'have been suppressed',24x,25('.'))
      call set screen text colour
      endif ! if ( suppress9 .eq. 8 )
      write(01,2201)valchars10,valchars11,FLSTRUCTsmall(kind0,icod),
     &kind0
      write(09,2201)valchars10,valchars11,FLSTRUCTsmall(kind0,icod),
     &kind0
      write(33,2201)valchars10,valchars11,FLSTRUCTsmall(kind0,icod),
     &kind0
 2201 Format(110('-')/
     &'*** There is a problem in the monthly structure ...'/
     &'*** The monthly averages imply more variation than the ',
     &'figure given for the annual standard deviation ...'/
     &'*** This standard deviation has been ',
     &'over-written    ...'/
     &'*** Replaced ',a10,' with ',a10,
     &' ... Check the file ',a30,i6/110('-'))
      tstdev = 1.20 * test2
	endif

*     correct monthly data for the number of days in a month -------------------
*     set the monthly standard deviations using the annual coeffiecient of -----
*     variation ----------------------------------------------------------------
      do imon = 1, 12
      struct1(imon) = (365.0/12.0) * struct1(imon) / 
     &                days in months (imon)
	struct2(imon) = tstdev * struct1(imon) / tmean  
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo
      if ( iout .eq. 1 ) then
	write(01,3245)
 3245 format(/77('-')/'Monthly data (corrected for the numbers of ',
     &'days in months)'/77('-'))
      write(01,2398)
 2398 format('Month',15x,'Mean',3x,'Standard',6x,'Shift',2x,
     &'Correlation',11x,'Type of'/26x,'deviation',30x,
     &'distribution'/77('-'))
      write(01,2399)(struct1(i),struct2(i),struct3(i),struct6(i),
     &struct0(i),i=1,12)
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
     &'December ... ',3f11.2,f13.4,i18/77('-'))
	endif

*     calculate the coeffiecient of variance for the monthly data --------------
*     sum the squares of the monthly means -------------------------------------
      den = 0.0
      do imon = 1,12
*     monthly mean -------------------------------------------------------------
	aan = struct1 (imon)
*	aan = aan * days in months (imon) * 12.0 / 365.0
 	den = den + aan * aan
      enddo

      ss = tstdev * tstdev  +  tmean * tmean 
      cov = SQRoot (1622, ( 12.0 * ss / den ) )  -  1.0

*     calculate the standard deviations for each month -------------------------
      do imon = 1,12
*     the monthly mean ---------------------------------------------------------
      A01 = tmean + struct1 (imon)
      S01 = A01 * cov
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo

      if ( iout .eq. 1 ) then
      call sort format 2 (tmean,tstdev)
      write(01,6045)FLSTRUCTsmall(kind0,icod),valchars10,valchars11
 6045 format('File: ',a64/77('-')/
     &'Annual values',43X,'Mean =',a10/
     &42x,'Standard deviation =',a10)
      call sort format 1 (cov*tmean)
	write(01,6245)valchars10
 6245 format(34x,'Monthly standard deviation =',a10/77('-'))

      write(01,6398)
 6398 format('Monthly values',6x,'Mean    Calculated',
     &3x,'Shift',2x,'Correlation',11x,'Type of'/
     &29x,' standard',27x,'distribution'/
     &29x,'deviation'/77('-'))
      write(01,6599)(struct1(i),struct2(i),struct3(i),struct6(i),
     &struct0(i),i=1 ,12)
 6599 format(
     &'January ...  ',f11.2,f14.2,f8.2,f13.4,i18/
     &'February ... ',f11.2,f14.2,f8.2,f13.4,i18/
     &'March ...    ',f11.2,f14.2,f8.2,f13.4,i18/
     &'April ...    ',f11.2,f14.2,f8.2,f13.4,i18/
     &'May ...      ',f11.2,f14.2,f8.2,f13.4,i18/
     &'June ...     ',f11.2,f14.2,f8.2,f13.4,i18/
     &'July ...     ',f11.2,f14.2,f8.2,f13.4,i18/
     &'August ...   ',f11.2,f14.2,f8.2,f13.4,i18/
     &'September ...',f11.2,f14.2,f8.2,f13.4,i18/
     &'October ...  ',f11.2,f14.2,f8.2,f13.4,i18/
     &'November ... ',f11.2,f14.2,f8.2,f13.4,i18/
     &'December ... ',f11.2,f14.2,f8.2,f13.4,i18/77('-')/)
	endif
      return

*     error messages -----------------------------------------------------------    
 7500 write( *,7168) FLSTRUCTsmall(kind0,icod)
      write(01,7168) FLSTRUCTsmall(kind0,icod)
      write(09,7168) FLSTRUCTsmall(kind0,icod)
      write(33,7168) FLSTRUCTsmall(kind0,icod)
 7168 Format(/77('-')/
     &'*** Error in reading the monthly structure for data ...'/
     7'*** Attempting to revert to annual structure ...'/
     &'*** Check the file ',a64/77('-'))
      close (11)
	itest12 = 12
      return

 7700 write( *,7468) FLSTRUCTsmall(kind0,icod)
      write(01,7468) FLSTRUCTsmall(kind0,icod)
      write(09,7468) FLSTRUCTsmall(kind0,icod)
      write(33,7468) FLSTRUCTsmall(kind0,icod)
 7468 Format(/77('-')/
     &'*** Error in reading the monthly structure for ',
     &'data ...'/'*** Reached the end of the file ...'/
     &'*** Have you set the right code for the type of data ...'/
     7'*** Attempting to revert to annual structure ...'/
     &'*** Check the file ',a64/77('-'))
      close (11)
	itest12 = 12
      return
      end

      
*     read data that are set up as a monthly structure from monthly data -------
      subroutine read monthly structure background data ! type 8
     &(kindb,iout,tmean,tstdev,t3,tcorr,itest)
      include 'COM.FOR'

*     set a normal distributions by default ------------------------------------
      do i = 1,12
      struct0(i) = 1
	enddo
*     read the data file -------------------------------------------------------
      read(11, *, ERR=7400)itdist,tmean,tstdev,t3,tcorr
      read(11, *, ERR=7400,END=7400)(struct0(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct1(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct2(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct3(i),i=1 ,12)
      read(11, *, ERR=7400,END=7400)(struct4(i),i=1 ,12)
      goto 7777
 7400 continue
	rewind 11
      read(11, *, ERR=7400,END=7700)itdist,tmean,tstdev,t3,tcorr
      read(11, *, ERR=7500,END=7700) (struct1(i),i=1 ,12),
     &(struct2(i),i=1 ,12),(struct3(i),i=1, 12), (struct4(i),i=1, 12)
      do i = 1,12
      struct0(i) = 1
	enddo
 7777 continue
      call check and correct monthly structure correlation coefficients
      close (11)

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if (struct1(imon) .lt. 0.0000001 ) itest = itest + 1
      enddo
	if ( itest .eq. 12 ) then
	if ( month write 1 .lt. 3 ) then
      if ( kindb .eq. 1 ) then
      write(01,2950) fltempsmall(kindb,tempd)
 2950 Format(77('-')/'*** Unnecessary monthly structures for ',
     &'data on temperature ...'/
     &'*** The monthly adjustments zero ...'/
     &'*** Reverted to the annual data ... ',
     &'see the ERR file for details ... '/
     &'*** File: ',a64/77('-'))
      else
      write(01,2959) fltempsmall(kindb,tempd)
 2959 Format(77('-')/'*** Unnecessary monthly structures for ',
     &'data on suspended solids ...'/
     &'*** The monthly adjustments are all zero ...'/
     &'*** Reverted to the annual data ... ',
     &'see the ERR file for details ... '/
     &'*** File: ',a64/77('-'))
      endif
      month write 1 = month write 1 + 1
      endif
      if ( ifbatch .eq. 0 ) then
      call change colour of text (12) ! orange
      if ( kindb .eq. 1 ) then
      write( *,2650) fltempsmall(kindb,tempd)
 2650 Format('* Unnecessary monthly structure ...',16x,'...',7x,
     &'for temperature',29x,'File: ',a40)
      else
      write( *,2659) fltempsmall(kindb,tempd)
 2659 Format('* Unnecessary monthly structure ...',16x,'...',7x,
     &'for suspended solids',24x,'File: ',a40)
      endif
      call set screen text colour
	endif
      write(09,2050) fltempsmall(kindb,tempd)
      write(33,2050) fltempsmall(kindb,tempd)
 2050 Format(77('-')/'*** Unnecessary monthly structure for ',
     &'background data ... '/
     &'*** The monthly adjustments are zero ',
     &'... reverted to the annual data ... '/
     &'*** File: ',a64/77('-'))
	return
	endif

*     set a standard deviations by default -------------------------------------
      do i = 1,12
      struct2(i) = tstdev
	enddo

*     test the monthly data are compatible with the annual data ----------------
      test1 = 0.0
      do imon = 1,12
      test1 = test1 + struct1(imon)
	enddo
      if ( test1 .gt. 0.0001 .or. test1 .lt. -0.001 ) then
      if ( kindb .eq. 1 ) then
      write(01,2000) test1,fltempsmall(kindb,tempd)
      write( *,2000) test1,fltempsmall(kindb,tempd)
      write(09,2000) test1,fltempsmall(kindb,tempd)
      write(33,2000) test1,fltempsmall(kindb,tempd)
 2000 Format(/77('-')/
     &'*** Error in the monthly structure for temperature ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ... the surplus is',f12.6/
     &'*** Check the file ',a30/77('-'))
      else
      write(01,2900) test1,fltempsmall(kindb,tempd)
      write( *,2900) test1,fltempsmall(kindb,tempd)
      write(09,2900) test1,fltempsmall(kindb,tempd)
      write(33,2900) test1,fltempsmall(kindb,tempd)
 2900 Format(/77('-')/
     &'*** Error in the monthly structure for suspended solids ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ... the surplus is',f12.6/
     &'*** Check the file ',a30/77('-'))
      endif
      call stop
	endif

*     set the means ... set the standard deviations to zero --------------------
      do imon = 1,12
	struct1(imon) = tmean + struct1(imon)
      struct2(imon) = 0.0
	enddo

*     test the monthly data are non zero ---------------------------------------   
      itest = 0
      do imon = 1,12
      if ( struct1(imon) .lt. 0.0000001 ) itest = itest + 1
	enddo
	if ( itest .eq. 12 ) then
      write(01,2150) fltempsmall(kindb,tempd)
      write( *,2150) fltempsmall(kindb,tempd)
      write(09,2150) fltempsmall(kindb,tempd)
      write(33,2150) fltempsmall(kindb,tempd)
 2150 Format(77('-')/'*** Unusable monthly structure for background ',
     &'data ...'/
     &'*** Some monthly means are zero ...'/
     &'*** Reverted to the annual data ... ',
     &'see the ERR file for details ... '/
     &'*** File: ',a64/77('-'))
      do i = 1,12
      struct0(i) = 2
      struct1(i) = 0.0
      struct2(i) = 0.0
      struct3(i) = 0.0
      struct4(i) = -9.9
	enddo
	return
	endif

*     check the standard deviation implied by the monthly means ----------------
      test1 = 0.0
      test2 = 0.0
      do imon = 1, 12
      test1 = test1 + struct1(imon)
      test2 = test2 + struct1(imon) * struct1(imon)
	enddo
      test2=(test2-test1*test1/12.0)/(12.0-1.0)
      if ( test2 .gt. 1.0e-10 ) then
      test2=SQRoot(1267,test2)
      else
      test2 = 0.0
      endif
      test1 = test1 / 12.0
      if ( 1.10 * test2 .gt. tstdev ) then
      call sort format 2 (tstdev,1.20 * test2)
      if ( kindb .eq. 1 ) then ! temperature
      if ( nobigout .le. 0 ) then
      write(01,2291) valchars10,valchars11,fltempsmall(kindb,tempd)
      endif
      write(33,2291) valchars10,valchars11,fltempsmall(kindb,tempd)
      write(09,2291) valchars10,valchars11,fltempsmall(kindb,tempd)
 2291 Format(77('-')/
     &'*** Problem in the MONTHLY STRUCTURE',
     &' for temperature ...'/
     &'*** Monthly averages are incompatible with ',
     &'the annual standard deviation'/
     &'*** This standard deviation has been ',
     &'over-written ...'/'*** Replaced ',a10,' with ',a10,
     &' ... ','File: ',a30/77('-'))
      endif ! temperature
      if ( kindb .eq. 2 ) then ! suspended solids
      if ( nobigout .le. 0 ) then
      write(01,2891) valchars10,valchars11,fltempsmall(kindb,tempd)
      endif
      write(33,2891) valchars10,valchars11,fltempsmall(kindb,tempd)
      write(09,2891) valchars10,valchars11,fltempsmall(kindb,tempd)
 2891 Format(77('-')/
     &'*** Problem in the MONTHLY STRUCTURE',
     &' for suspended solids ...'/
     &'*** Monthly averages are incompatible with ',
     &'the annual standard deviation'/
     &'*** This standard deviation has been ',
     &'over-written ...'/'*** Replaced ',a10,' with ',a10,
     &' ... ','File: ',a30/77('-'))
      endif ! suspended solids
      if ( iout .eq. 0 ) then ! output requested
      if ( kindb .eq. 1 ) then ! temperature
      suppress9a = suppress9a + 1 ! failure to set up monthly temperature
*	if ( suppress9a .lt. 6 .or. MOD(suppress9,60) .eq. 0 ) then
 	if ( suppress9a .lt. 6 ) then
      if ( ical13 .eq. 0 ) then
      call change colour of text (34) ! dull yellow
      call sort format 2 (tstdev,1.20 * test2)
      write( *,2393) 
 2393 Format(
     &'* Problem in the monthly structure',17x,'...',7x,
     &'for temperature',29x,'st.deviation changed ....')
      call set screen text colour
      endif
      endif
      endif ! temperature
      if ( kindb .eq. 2 ) then ! suspended solids
      suppress9b = suppress9b + 1 ! failure to set up monthly solids
	if ( suppress9b .lt. 6 .and. ical .ne. 3 ) then
      if ( ical13 .eq. 0 ) then
      call change colour of text (34) ! dull yellow
      call sort format 2 (tstdev,1.20 * test2)
      write( *,2991)
 2991 Format(
     &'* Problem in the monthly structure',17x,'...',7x,
     &'for suspended solids',24x,'st.deviation changed ....')
      call set screen text colour
      endif
      endif
      endif ! suspended solids
	if ( suppress9a .eq. 6 .and. ical .ne. 3 ) then
      call change colour of text (12) ! orange
      write( *,2292) 
 2292 Format('* Multiple warnings on monthly temperature',9x,'...',
     &7x,'have been suppressed',24x,25('.'))
      call set screen text colour
      endif ! if ( suppress9a .eq. 8 )
	if ( suppress9b .eq. 6 .and. ical .ne. 3 ) then
      call change colour of text (12) ! orange
      write( *,2392) 
 2392 Format('* Multiple warnings on monthly suspended solids',4x,'...',
     &7x,'have been suppressed',24x,25('.'))
      call set screen text colour
      endif ! if ( suppress9b .eq. 8 )
      endif ! if ( iout .eq. 0 )
	tstdev = 1.20 * test2
	endif ! if ( 1.10 * test2 .gt. tstdev )

*     correct monthly data for the number of days in a month -------------------
*     set the monthly standard deviations to the annual values -----------------
      do imon = 1, 12
      struct1(imon) = (365.0/12.0) * struct1(imon) / 
     &                days in months (imon)
	struct2(imon) = tstdev * struct1(imon) / tmean  
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo
      if ( iout .eq. 1 .and. nobigout .le. 0 ) then
      if ( kindb .eq. 1 ) then ! temperature
 	write(33,3245)
 3245 format(/77('-')/'Monthly temperatures (corrected for the ',
     &'numbers of days in months)'/77('-'))
      else
	write(33,3945)
 3945 format(/77('-')/'Monthly suspended solids (corrected for the ',
     &'numbers of days in months)'/77('-'))
      endif
      write(33,2398)
 2398 format('Month',15x,'Mean',3x,'Standard',6x,'Shift',2x,
     &'Correlation',11x,'Type of'/26x,'deviation',30x,
     &'distribution'/77('-'))
      write(33,2399)(struct1(i),struct2(i),struct3(i),struct6(i),
     &struct0(i),i=1,12)
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

*     calculate the coeffiecient of variance for the monthly data --------------
*     sum the squares of the monthly means -------------------------------------
      den = 0.0
      do imon = 1,12
*     monthly mean -------------------------------------------------------------
	aan = struct1 (imon)
*	aan = aan * days in months (imon) * 12.0 / 365.0
 	den = den + aan * aan
      enddo

      ss = tstdev * tstdev  +  tmean * tmean 
      cov = SQRoot (1622, ( 12.0 * ss / den ) )  -  1.0

*     calculate the standard deviations for each month -------------------------
      do imon = 1,12
*     the monthly mean ---------------------------------------------------------
      A01 = tmean + struct1 (imon)
      S01 = A01 * cov
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo

      if ( iout .eq. 1 ) then
*	write(01,6045)FLSTRUCTsmall(kindb,icod),tmean,tstdev,cov*tmean
 6045 format(77('-')/a77/77('-')/
     &'Annual values                  ',28X,'Mean =',F7.1/
     &45x,'Standard deviation =',F7.1/
     &37x,'Monthly standard deviation =',F7.1/77('-'))

*     write(01,6398)
 6398 format('Monthly values',6x,'Mean    Calculated',
     &3x,'Shift',2x,'Correlation',11x,'Type of'/
     &29x,' standard',27x,'distribution'/
     &29x,'deviation'/77('-'))
*     --------------------------------------------------------------------------
*     write(01,6599)(struct1(i),struct2(i),struct3(i),struct6(i),struct0(i),
*    &i=1 ,12)
 6599 format(
     &'January ...  ',f11.2,f14.2,f8.2,f13.4,i18/
     &'February ... ',f11.2,f14.2,f8.2,f13.4,i18/
     &'March ...    ',f11.2,f14.2,f8.2,f13.4,i18/
     &'April ...    ',f11.2,f14.2,f8.2,f13.4,i18/
     &'May ...      ',f11.2,f14.2,f8.2,f13.4,i18/
     &'June ...     ',f11.2,f14.2,f8.2,f13.4,i18/
     &'July ...     ',f11.2,f14.2,f8.2,f13.4,i18/
     &'August ...   ',f11.2,f14.2,f8.2,f13.4,i18/
     &'September ...',f11.2,f14.2,f8.2,f13.4,i18/
     &'October ...  ',f11.2,f14.2,f8.2,f13.4,i18/
     &'November ... ',f11.2,f14.2,f8.2,f13.4,i18/
     &'December ... ',f11.2,f14.2,f8.2,f13.4,i18/77('-')/)
	endif
      return
    
 7500 write( *,7168) fltempsmall(kindb,tempd)
      write(01,7168) fltempsmall(kindb,tempd)
      write(09,7168) fltempsmall(kindb,tempd)
      write(33,7168) fltempsmall(kindb,tempd)
      close (11)
      call stop
 7168 Format(/77('-')/
     &'*** Error in reading the monthly structure for ',
     &'temperature ...'/
     &'*** Check the file ',a30/77('-'))
 7700 write( *,7468) fltempsmall(kindb,tempd)
      write(01,7468) fltempsmall(kindb,tempd)
      write(09,7468) fltempsmall(kindb,tempd)
      write(33,7468) fltempsmall(kindb,tempd)
      close (11)
      call stop
 7468 Format(/77('-')/
     &'*** Error in reading the monthly structure for ',
     &'temperature ...'/
     &'*** Reached the end of the file ...'/
     &'*** Have you set the right code for the type of data ...'/
     &'*** Check the file ',a30/77('-'))
	end


*     get random normal deviate for river flow  --------------------------------
      function FRAN function (JS)
      include 'COM.FOR'
      FRAN function = FRAN (JS)
      return
      end

*     get random normal deviate for river quality ------------------------------
*     imposing the correct correlation -----------------------------------------
      function QRAN function ( JDET,JS,IS )
      include 'COM.FOR'
      itest = 0
      cr = spcorrRFRC ! river flow on river quality ---------------- inside QRAN
*     check there is no special correlation ------------------------------------
      if ( cr .lt. -9.899 .and. cr .gt. -9.901 ) itest = 1
      if ( cr .lt.  0.001 .and. cr .gt. -0.001 ) itest = 1
      if ( itest .eq. 1 ) then
      xx = CRAN (JDET,JS)
      else
*     non-standard correlation -------------------------------------------------
      RRS = FRAN (IS)
      xx = cr * RRS + CRAN (JDET,JS) * SQRMB(115, 1.0 - cr*cr )
      endif
      QRAN function = xx
      return
      end

      function qmonth ( I )
      include 'COM.FOR'

	if ( i .eq. 1 ) then
      shot day = 1
	kmunth = 1
      Qmonth = 1
	lmunth = 1
	else
	shot day = shot day  + 1
      lmunth = kmunth

      if ( shot day .le. days in months (lmunth) ) then
 	Qmonth = lmunth
	else
	Qmonth = lmunth + 1
	kmunth = lmunth + 1
	lmunth = lmunth + 1
	shot day = 1
	if (kmunth .gt. 12 ) then
	kmunth = 1
	lmunth = 1
	Qmonth = 1 
	endif
	endif

	endif

	return
	end

*     monthly data for tributary quality ---------------------------------------
      subroutine monthly river quality
      include 'COM.FOR'
      logical exists

      mass = 0
      if ( mass .eq. 1 ) then

*     mean and standard deviation ----------------------------------------------
      RRCM = quolity data (IQ,JP,1)
	RRCS = quolity data (IQ,JP,2)

      if ( MONQ .gt. 1 ) then
      write(01,51)Dname(Jp)
   51 format(77('-')/
     &'Generated river quality data: ',a11,' (monthly data)'/77('-'))
      call sort format 2 (RRCM, RRCS)
      write(01,52) valchars10, UNITs(JP), valchars11, UNITs(JP)
   52 format('Input data:',26x,'        Mean =',a10,1x,a4/
     &28x,'   Standard deviation =',a10,1x,a4/77('-'))
      endif
      endif

*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------
      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
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
      Inquire( FILE = flmonth(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLMONTHsmall(2,icod)
      write(01,7163) FLMONTHsmall(2,icod)
      write(09,7163) FLMONTHsmall(2,icod)
      write(33,7163) FLMONTHsmall(2,icod)
 7163 Format(/77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
      if ( ifdiffuse .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,7863) Dname(jp),
     &FLMONTHsmall(2,icod)
*     write(33,7863) Dname(jp),FLMONTHsmall(2,icod)
 7863 format(77('-')/
     &'Monthly data on river quality ... ',a11/
     &'File: ',a64/77('-'))
      endif
      endif

*     open the file of monthly data --------------------------------------------
      open(11,FILE = flmonth(2,icod), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (2,icod) ! river quality - type 5

*     special correlation coefficient ... added flow on added quality ----------
      spcorrRFRC = quolity data (IQ,JP,4) ! ------------------- QRAN (duplicate)

      if ( mass .eq. 1 ) then
      if ( ifdiffuse .eq. 0 ) then
      write(01,4398)
 4398 format(
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
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
      if ( ifdiffuse .eq. 1 ) then
*     write(01,4388)
 4388 format(33x,45('-')/33x,'Month        ','       Mean   Standard',
     &' Deviation'/33x,45('-'))
*     write(01,2389)(seas1(i),seas2(i),i=1 ,12)
 2389 format(
     &33x,'January ...  ',2f11.2/
     &33x,'February ... ',2f11.2/
     &33x,'March ...    ',2f11.2/
     &33x,'April ...    ',2f11.2/
     &33x,'May ...      ',2f11.2/
     &33x,'June ...     ',2f11.2/
     &33x,'July ...     ',2f11.2/
     &33x,'August ...   ',2f11.2/
     &33x,'September ...',2f11.2/
     &33x,'October ...  ',2f11.2/
     &33x,'November ... ',2f11.2/
     &33x,'December ... ',2f11.2/)
      endif
	endif

*     calculate the Monte-Carlo sampling errors for the monthly data on river --
*     quality.  Loop through the months ----------------------------------
      do imon = 1, 12

*     special version of BIAS for monthly data ---------------------------------
      call bias in monthly stream quality (imon)

      if (BSM(imon) .gt. 1.0E-08) BSM(imon) = seas1(imon)/BSM(imon)
      if (BSS(imon) .gt. 1.0E-08) then
          BSS(imon) = seas2(imon)/BSS(imon)
      else
          BSS(imon) = 1.0
      endif

      if ( MONQ .gt. 1 ) then

      if ( imon .eq. 1 ) then
      if ( nobigout .le. 0 ) write(01,12)
   12 format(/
     &'Correction factors for sampling errors on stream qua',
     &'lity data:')
      write(01,92)imon,BSM(imon),BSS(imon)
   92 format('Monthly Mean',i3,' =',F8.3,
     &'   Standard deviation =',F8.3)
      endif
	endif
      enddo

*     sample the distributions -------------------------------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RCM = seas1 (imonth) ! mean quality --------------------------------------
      RCS = seas2 (imonth) ! standard deviation --------------------------------
      RC3 = seas3 (imonth) ! shift ---------------------------------------------
      spcorrRFRC = seas4 (imonth) ! river flow on river quality ----------- QRAN

	GRCM = 0.0
	GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)

*     mean and standard deviation of logged data -------------------------------
      if ( RM3 .gt. 1.0e-9) then
      GRCM = ALOG ( RM3 / SQRoot(1000,RM3+RCS*RCS) )
      GRCS = SQRoot(1025, ALOG (1.0+(RCS*RCS)/RM3) )
      endif

*     get the normal deviate for monthly stream quality ------------------------
      js = JSET ( IQ, NS, is )
      qdmm = QRAN function ( JP, js, is )
*     compute the value of the shot --------------------------------------------
      if ( mass .eq. 1 ) then
      ECshots(is) = Vlogn (qdmm, GRCS, GRCM, RC3,
     &BSM(imonth),BSS(imonth),RCM)
	else
      CMS(JP,is) = Vlogn (qdmm, GRCS,GRCM,RC3,
     &BSM(imonth),BSS(imonth),RCM)
      endif

   22 continue

      if ( MONQ .gt. 1 ) call write shots for river quality 
      return

 7500 write( *,7168) FLMONTHsmall(2,icod)
      write(01,7168) FLMONTHsmall(2,icod)
      write(09,7168) FLMONTHsmall(2,icod)
      write(33,7168) FLMONTHsmall(2,icod)
 7168 Format(/77('-')/'*** Error in river quality data ...'/
     &'*** Error in reading monthly data ...'/
     &'*** Check the file ',a64/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),i=1 ,12)
      write( *,2399)(seas1(i),seas2(i),seas3(i),seas4(i),i=1 ,12)
      call stop

      end







*     upstream boundary --------------------------------------------------------
      subroutine river boundary (KU)
      include 'COM.FOR'

*     flow data for the head of the reach --------------------------------------
      IF = JF(KU)
      do IS = 1, NS
      FMS(IS) = 0.0
      enddo
      do ii = 1, 5
      FLOW(ii) = 0.0
      enddo

*     set up the river quality targets -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(KU)
      do JP = 1, NDET
      RQO(JP) = 0.0
	MRQO(JP) = 0
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
      
      if ( IQSreach .gt. 0 ) then
      class limmits (nclass,JP) = 1.0e10
      do icc = 1, nclass
      class limmits (icc,JP) = standards for reach (IQSreach,icc)
      enddo
      endif

      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif ! if ( background class .eq. 1 )
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,JP) ! set target for feature -
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5) then
      do ic = 1, nclass - 1
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif ! if ( class limmits (ic,JP) .lt. -1.0e-8 )
      enddo ! do ic = 1, nclass - 1
      endif ! if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5)
      endif ! if ( IQSreach .gt. 0 )
      RQO(JP) = targit ! use the target for graphs -----------------------------
	MRQO(JP) = MRQS(JP)
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET

*     generate flow data at head of Reach --------------------------------------
      call generate river flow ! at the head of the reach

*     generate river quality data for head of Reach ----------------------------
      IQ = JQ (KU)

      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      write(01,14)RNAME(IREACH),IQ
      write(09,14)RNAME(IREACH),IQ
      write(33,14)RNAME(IREACH),IQ
      write( *,14)RNAME(IREACH),IQ
   14 format(77('-')/'*** Illegal quality data for head',
     &' of ',A16/'*** SIMULATION TERMINATED'/
     &'*** Value entered is',i5/77('-'))
      call stop
      endif

      do JP = 1, ndet

*     initialise the concentrations --------------------------------------------
      do IS = 1, NS
      CMS(JP,IS) = 0.0
      do IP = 1, NPROP
      LMS(IP,JP,IS) = 0.0
      ULMS(IP,JP,IS) = 0.0
      enddo
	enddo

      call inititialise the stores for the estimates of load 

*     set the default correlation coefficients ... river quality on river flow -
      CO1 = RFCL(JP)
*     special correlation ... river quality on river flow ----------------------
      if ( quolity data (IQ,JP,4) .ne. -9.9 ) then
	CO1 = quolity data (IQ,JP,4)
	endif
      spcorrRFRC = CO1 ! river flow on river quality ---------------------- QRAN
      if ( QTYPE (JP) .ne. 4 ) then
      QUALN(JP) = QNUM(IQ,JP) ! effective sampling rate ------------------------
*     generate river quality data ----------------------------------------------
      call generate river quality ! at the head of the reach
      
      if ( F(IF,1) .gt. 1.0e-10 ) then
      do is = 1, NS
      LMS(17,JP,IS) = CMS(JP,IS)
      enddo
      endif

      endif
      enddo

      call get summaries of river quality from the shots
*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! at the head of the reach

*     accumulate the river derived load ----------------------------------------
      do jdet = 1, NDET
      if ( QTYPE (jdet) .ne. 4 ) then
	nx = n13   
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TRLODE2(jdet,J1) = TRLODE2(jdet,J1) + XLOAD (jdet,1,J1)
      TRLODE1(jdet,J1) = TRLODE1(jdet,J1) + XLOAD (jdet,1,J1)
      enddo
      endif
      enddo

      call add up all the loads
      return
      end

      
*     lake boundary ------------------------------------------------------------
      subroutine lake inflow (KU)
      include 'COM.FOR'

*     flow data for the input to the lake --------------------------------------
      IF = JF(KU)
*     reset the river quality targets ------------------------------------------
      do ID=1, ndet
      RQO(ID) = 0.0
*     Type of summary statistic for river quality targets. These will be -------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile              
	MRQO(ID) = 0
      enddo
*     --------------------------------------------------------------------------
*     set up the river quality targets ----------------------------------------- 
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(KU)
      do JP = 1, NDET
      RQO(JP) = 0.0
	MRQO(JP) = 0
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,JP) ! set target for feature -
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass - 1
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5) then
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif
      endif ! if ( JP .eq. 4 )
      enddo
      endif ! if ( IQSreach .gt. 0 )
      RQO(JP) = targit ! use the target for graphs -----------------------------
	MRQO(JP) = MRQS(JP)
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET
*     --------------------------------------------------------------------------

      call generate river flow ! lake inflow

*     generate river quality data ----------------------------------------------
      IQ = JQ (KU)

      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      write(01,14)RNAME(IREACH),IQ
      write(09,14)RNAME(IREACH),IQ
      write(33,14)RNAME(IREACH),IQ
      write( *,14)RNAME(IREACH),IQ
   14 format(77('-')/'*** Illegal quality data for head',
     &' of lake ',A16/'*** SIMULATION TERMINATED'/
     &'*** Value entered is',i5/77('-'))
      call stop
      endif

      do JP = 1, ndet

*     initialise the concentrations --------------------------------------------
      do IP = 1, NPROP
      do IS = 1, NS
      CMS(JP,IS) = 0.0
      LMS(IP,JP,IS) = 0.0
      ULMS(IP,JP,IS) = 0.0
      enddo
	enddo

      call inititialise the stores for the estimates of load 

*     set the default correlation coefficients ... river quality on river flow -
      CO1 = RFCL(JP)
*     special correlation ... river quality on river flow ----------------------
      if ( quolity data (IQ,JP,4) .ne. -9.9 ) then
	CO1 = quolity data (IQ,JP,4)
	endif
	spcorrRFRC = CO1 ! river flow on river quality ---------------------- QRAN

      if ( QTYPE (JP) .ne. 4 ) then
      QUALN(JP) = QNUM(IQ,JP) ! effective sampling rate ------------------------
*     generate river quality data ----------------------------------------------
      call generate river quality ! lake inflow
      endif
      enddo

      call get summaries of river quality from the shots
*     calculate loads and the summary statistics of load -----------------------
      call load calculation

*     accumulate the river derived load ----------------------------------------
      do jdet = 1, NDET
      if ( QTYPE (jdet) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TRLODE2(jdet,J1) = TRLODE2(jdet,J1) + XLOAD (jdet,1,J1)
      TRLODE1(jdet,J1) = TRLODE1(jdet,J1) + XLOAD (jdet,1,J1)
      enddo
      endif
      enddo

      call add up all the loads

*     flow to river from lake --------------------------------------------------
      write(01,2000)rname(IREACH)
 2000 format(120('=')/
     &'Calculations for ',a16/120('=')/
     &'Flowing from the river into the lake ...')
      call sort format 2 (Flow(1), Flow(2))
      write(01,2001)valchars10,valchars11
 2001 format(120('=')/
     &'Inflow to river                   Mean =',a10/
     &'               95-percentile lows flow =',a10/120('='))

      return
      end



      subroutine lake outflow (KU)
      include 'COM.FOR'

      lake45 = 1 ! set the indicator of flow from a lake
	Rlength(IREACH) = 0.0 ! set the length of the receiving reach to zero
      IF = JF(KU) ! flow data for the output from the lake ---------------------
      do ID=1, ndet ! reset the river quality targets --------------------------
      RQO(ID) = 0.0
	MRQO(ID) = 0
      enddo

*     --------------------------------------------------------------------------
*     set up the river quality targets ----------------------------- Richard III
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(KU)
      do JP = 1, NDET
      RQO(JP) = 0.0
	MRQO(JP) = 0
      if ( QTYPE (JP) .ne. 4 ) then
      IQSreach = EQS reach (IREACH,JP)
      if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,JP) 
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,JP) ! set target for feature -
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass - 1
      if ( MRQS(JP). ne. 4 .and. MRQS(JP). ne. 5) then
      if ( class limmits (ic,JP) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,JP))
      endif
      endif ! if ( JP .eq. 4 )
      enddo
      endif ! if ( IQSreach .gt. 0 )
      RQO(JP) = targit ! use the target for graphs -----------------------------
	MRQO(JP) = MRQS(JP)
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET
*     --------------------------------------------------------------------------

      call generate river flow ! lake outflow

*     generate river quality data ----------------------------------------------
      IQ = JQ (KU)

      if ( IQ .le. 0 .or. IQ .gt. NV ) then
      write(01,14)RNAME(IREACH),IQ
      write(09,14)RNAME(IREACH),IQ
      write(33,14)RNAME(IREACH),IQ
      write( *,14)RNAME(IREACH),IQ
   14 format(77('-')/'*** Illegal quality data for flow',
     &' from a lake ',A16/'*** SIMULATION TERMINATED'/
     &'*** Value entered is',i5/77('-'))
      call stop
      endif

      do JP = 1, ndet

      do IP = 1, NPROP
      do IS = 1, NS
      CMS(JP,IS) = 0.0 ! initialise the concentrations -------------------------
      LMS(IP,JP,IS) = 0.0
      ULMS(IP,JP,IS) = 0.0
      enddo
	enddo

      call inititialise the stores for the estimates of load 

*     set the default correlation coefficients ... river quality on river flow -
      CO1 = RFCL(JP)
*     special correlation ... river quality on river flow ----------------------
      if ( quolity data (IQ,JP,4) .ne. -9.9 ) then
	CO1 = quolity data (IQ,JP,4)
	endif
	spcorrRFRC = CO1 ! river flow on river quality ---------------------- QRAN

      if ( QTYPE (JP) .ne. 4 ) then
      QUALN(JP) = QNUM(IQ,JP) ! effective sampling rate ------------------------
*     generate river quality data ----------------------------------------------
      call generate river quality ! lake outflow

      do IS = 1, NS
      LMS(17,JP,IS) = CMS(JP,IS)
      enddo
      
      endif
      enddo

      call get summaries of river quality from the shots
*     calculate loads and the summary statistics of load -----------------------
      call load calculation

*     accumulate the river derived load ----------------------------------------
      do jdet = 1, NDET
      if ( QTYPE (jdet) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TRLODE2(jdet,J1) = TRLODE2(jdet,J1) + XLOAD (jdet,1,J1)
      TRLODE1(jdet,J1) = TRLODE1(jdet,J1) + XLOAD (jdet,1,J1)
      enddo
      endif
      enddo

      call add up all the loads

*     flow to river from lake --------------------------------------------------
      write(01,2000)rname(IREACH)
      write(21,2000)rname(IREACH)
 2000 format(120('=')/
     &'Calculations for ',a16/120('=')/
     &'Flows from the lake into the river ...')
      call sort format 2 (Flow(1), Flow(2))
      write(01,2001)valchars10,valchars11
      write(21,2001)valchars10,valchars11
 2001 format(120('=')/
     &'Outflow from river                Mean =',a10/
     &'               95-percentile lows flow =',a10/120('='))

      return
      end
