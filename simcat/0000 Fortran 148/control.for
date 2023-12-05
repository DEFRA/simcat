*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ....
*     ==========================================================================
*     written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     direct the order of processing the Reaches -------------------------------
*     process each Reach ...         Mix Reaches ...   5869
*     --------------------------------------------------------------------------

      subroutine start the main calculations
      include 'COM.FOR'

      IREACH = 0
	start reach = 0
      feeture = 0
	INEXT = 0
      call initialise the running totals of load
      call initialise classification
      
*     33333333333333333333333333333333333333333333333333333333333333333333333333
      if ( n148 .eq. 1 ) call initialise the loads for works and bodies ! 333333
*     33333333333333333333333333333333333333333333333333333333333333333333333333
      
*     identify the first reach ------------------------------------------------- 
      k = 1
	do
	kk = ReachCode(k)
	if ( Included(kk) .eq. 0 ) then
	exit
	endif
	k = k + 1
      enddo
      IREACH = ReachCode(k)

*     arrive here and then process the next reach ------------------------------
   19 continue
      call identify virtual reaches
      call REACH ! process the reach

*     check if this is the final reach (kfinal will be zero) -------------------
      kfinal = iplan(ireach,1) + iplan(ireach,2) + iplan(ireach,3) 
	if ( kfinal .eq. 0 ) goto 89 ! for the final reach

*     prepare for the next reach -----------------------------------------------
*     test for a confluence of reaches -----------------------------------------
      if ( IPLAN(IREACH,3) .gt. 0 ) goto 12 ! for a confluence of reaches

*     ==========================================================================
*     test for a branch to a new headwater reach -------------------------------
      if ( IPLAN(IREACH,2) .le. 0 ) goto 13
*     yes, this is a head water reach ------------------------------------------
      call initialise the running totals of load
      IPLAN(IREACH,2) = -IPLAN(IREACH,2)
*     identify the number of the headwater reach -------------------------------      
      IREACH = -IPLAN(IREACH,2)
*     set the number of the next feature ---------------------------------------
      JNEXT = feeture + 1
	start reach = 0
*     check for illegal data for this reach ------------------------------------
*     this reach must start with feature type 10 or 11 -------------------------
      if (JT(JNEXT) .ne. 10 .and. JT(JNEXT) .ne. 11 .and. 
     &    JT(JNEXT) .ne. 20 .and. JT(JNEXT) .ne. 21 .and.
     &    JT(JNEXT) .ne. 45 .and.
     &    JT(JNEXT) .ne. 22 .and. JT(JNEXT) .ne. 23 ) then

      call change colour of text (20) ! bright red
      write( *,1275) ireach,rname(ireach)
 1275 format(
     &'* Missing Feature for Reach number ... ',i7,4x,a16)
      write( *,2875)
 2875 format(
     &'* No flow and quality data for the Head of a Reach whose ',
     &'top forms an upstream boundary of the catchment ...       '/
     &'* (The first feature on the Reach must be Type',
     &' 10 or 45 etc ... )')
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,1075) ireach,rname(ireach)
      if ( nobigout .le. 0 ) write(21,1075) ireach,rname(ireach)
      write(09,1075) ireach,rname(ireach)
      write(33,1075) ireach,rname(ireach)
 1075 format(110('-')/
     &'*** Missing Feature for Reach number ... ',i7,1x,a16)
      write(09,1875)
      write(01,1875)
      write(33,1875)
 1875 format(110('-')/
     &'*** No flow and quality data for the Head of a Reach whose ',
     &'top forms an upstream boundary of the catchment ...       '/
     &'*** (The first feature on the Reach must be Type',
     &' 10 or 45 etc .... )'/110('-'))

      if ( ifbatch .eq. 1 ) then ! this is a batch run
      return
      else
      call stop
      endif
      endif
*     the reach looks correct --------------------------------------------------
      goto 19 ! go back and process the headwater reach 
*     ========================================================== headwater reach


*     ==========================================================================
*     test for a straight continuation of reaches ( go to 16 ) ----------------- 
*     or the final reach if IPLAN equals zero ( go to 15 ) ---------------------
   13 if ( IPLAN(IREACH,1) ) 15,15,16
*     yes, this is a straight continuation -------------------------------------
   16 continue ! straight continuation
      start reach = 1
      IREACH = IPLAN(IREACH,1)
*     check for illegal reach data ---------------------------------------------
      if ( JREACH (feeture + 1) .eq. IREACH ) then
      JNEXT = feeture + 1
      if ( ( JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 45 ) .and.    
     &Kount Reach (IREACH) .gt. 0 ) then
      IPLAN(IREACH,2) = IPLAN(IREACH,1)
      IPLAN(IREACH,1) = 0
	call change colour of text (20) ! bright red
      write( *,1905)IREACH,rname(IREACH),JT(JNEXT),Uname(JNEXT)
 1905 format(
     &'* Illegal data for Reach number',i6,' ... ',a16/
     &'* A straight continuation has data for the head of a Reach ',
     &'instead of taking data from end of an upstream Reach ...'/
     &'* The first feature on this Reach should ',
     &'not be Type',i3,') ... ',a37)
      call set screen text colour
      write(01,1005)IREACH,rname(IREACH),JT(JNEXT),Uname(JNEXT)
      write(09,1005)IREACH,rname(IREACH),JT(JNEXT),Uname(JNEXT)
      write(33,1005)IREACH,rname(IREACH),JT(JNEXT),Uname(JNEXT)
 1005 format(120('-')/
     &'*** Illegal data for Reach number',i6,' ... ',a16/
     &'*** A straight continuation has data for the head of a Reach ',
     &'instead of taking data from end of an upstream Reach ...'/
     &'*** The first feature on this Reach should ',
     &'not be Type',i3,') ... ',a37/120('-'))
      endif
      endif
      
      I = IREACH
      jjj = 0
      do JJ = feeture + 1, MU ! check all reaches for the downstream features --
      jjj = jj
      if ( Jreach(JJ) .eq. IREACH ) goto 2397 ! check IPLAN(IREACH,1)  
      if ( JT(JJ) .eq. 11 .and. JF(JJ) .eq. IREACH ) goto 2397 ! bifurcations
      if ( JT(JJ) .eq. 20 .and. JF(JJ) .eq. IREACH ) goto 2397 ! bifurcations
      if ( JT(JJ) .eq. 21 .and. JF(JJ) .eq. IREACH ) goto 2397 ! bifurcations
      if ( JT(JJ) .eq. 22 .and. JF(JJ) .eq. IREACH ) goto 2397 ! bifurcations
      if ( JT(JJ) .eq. 23 .and. JF(JJ) .eq. IREACH ) goto 2397 ! bifurcations
      enddo ! JJ = feeture,MU

      ISTOR2 = IRHOLD (IREACH)
      JSTOR (ISTOR2) = 0 ! the data for this reach are no longer needed --------
      IRHOLD (IREACH) = 0
*	write(08,5623)IREACH,rname(IREACH)
*5623 format('wipe IPLAN(IREACH,1) for Reach:',i4,1x,a16)
 2397 continue

      goto 89 ! finished the straight continuation 

*     confluence of reaches ----------------------------------------------------
   12 continue
      call mix the reaches
      IPLAN(IREACH,1) = - IPLAN(IREACH,1)
      IPLAN(IREACH,2) = - IPLAN(IREACH,2)
      IPLAN(IREACH,3) = - IPLAN(IREACH,3)
      IREACH = - IPLAN(IREACH,3)
      start reach = 2

*     check for illegal reach data ---------------------------------------------
      JNEXT = feeture + 1

*     ##### check with test data ###############################################
      if ( JREACH(JNEXT) .eq. IREACH .and. 
     &( JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 45 .or.    
     &  JT(JNEXT) .eq. 20 .or. JT(JNEXT) .eq. 21 .or. 
     &  JT(JNEXT) .eq. 22 .or. JT(JNEXT) .eq. 23 )) then
      if ( kerror .eq. 1 ) then
	call change colour of text (20) ! bright red
      write( *,1406)rname(IREACH)
      call set screen text colour
      endif
 1406 format('* Illegal data for a Reach ...',21x,'...',7x,
     &'Reach: ',a16,21x,'Calculation continues ...')

 8411 format('* No determinands for classification',15x,'...',7x,
     &'at ',a40,1x,25('.'))

      write(01,1006)rname(IREACH)
      write(09,1006)rname(IREACH)
      write(33,1006)rname(IREACH)
 1006 format(77('-')/
     &'*** Illegal data for a Reach downstream of the confluence of ',
     &'other Reaches ...'/
     &'*** (First feature on Reach should not be Type 10,45,',
     &'20,21,22 or 23 ... )'/
     &'*** Simulation halted at reach: ',a16/110('-'))
      return
      endif

*     store river quality statistics for graph plotting ------------------------
   89 continue
      call calculate summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads

      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))

*     calculate loads and the summary statistics of load -----------------------
      call load calculation
      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))

*     compute the extra values for partitioned metals ----------------------------
   	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
     &(3,J,CP2(1,J),CP2(2,J),CP2(3,J),CP2(4,J),CP2(5,J),CP2(6,J),
     &CP2(7,J),CP2(8,J),CP2(9,J),CP2(10,J),CP2(11,J),CP2(12,J))
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J),
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J),
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
	endif ! dissolved and solid metal
      endif
      enddo

*     ADIST   - length of main river upstream of start of Reach ----------------
*             - this is used for assembling data for graphs --------------------

      Length of main river = ADIST(IREACH)

      if ( ical13 .eq. 0 ) then
      write(22,300)ireach,0,
     &' " Start of Reach                             "',
     &RNAME(JREACH(KFEAT+1)),0
      kount22 = kount22 + 1
      write(22,301) '[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4),
     &              '[Flow    Obsd]',(0,JCP=1,4)              
      do j=1,MP10
	if ( QTYPE(J) .ne. 4 ) then
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12)
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12)
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4)
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4)
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J)
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0
      endif
      enddo
      endif

  300 format(I4,',',1PE11.4,',',A47,',"',A16,'",',i4)
  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))

      if ( kfinal .eq. 0 ) goto 15
*     process the reach --------------------------------------------------------
      goto 19

*     the last reach has been processed ----------------------------------------
*     summarise the results ----------------------------------------------------
   15 continue

*     write out the loads at the end of the run --------------------------------
      if ( ical13 .eq. 0 ) then
      if ( iscreen .lt. 3 ) then
	call change colour of text (49) ! dull blue
*     total length of river subject to calculations ----------------------------
      write( *,7399)Total river length
 7399 format(5x,'Total river length ',7x,f12.1)
      call set screen text colour
      endif
      endif
      
      call outsum at end of run
      if ( ical13 .eq. 0 ) then
      call list summary tables
      endif

      if ( ical .eq. 1 ) write(74,5000)
      if ( ical .eq. 3 ) write(75,5000)
 5000 format(123('*'))

      return
      end



*     summarise results --------------------------------------------------------
      subroutine list summary tables
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,3926)
      if ( ical .ne. 2 ) write(21,3926)
 3926 format(/24X,53('#')/
     &24X,'CALCULATIONS FINISHED FOR ALL REACHES ....     '/
     &24X,53('#'))
      endif
	endif

*     list details of flow gauges ----------------------------------------------
      if ( IPRINT .eq. 1 ) return
      if ( NGAUGE .eq. 0 ) goto 2000
      if ( NGAUGE .lt. MU ) goto 2001
      write(01,2002)
      write(03,2002)
      write(33,2002)
      write( *,2002)
 2002 format('*** Illegal summary of data......'/
     &'*** CALCULATIONS HALTED....   ')
      call stop
 2001 continue
      write(03,1031)
 1031 format(/77('-')/'Summary of Results for Flow Balance'/
     &77('-')/'Name of Gauging Station         ',
     &'Calculated Flow Data',6X,'Observed Flow Data'/
     &38X,' Mean       95',11X,'Mean       95'/
     &38X,'    Percentile',11X,'   Percentile'/77('-'))

      ipr = 0
	ipr2 = 0
      do 1 I = 1,NGAUGE

      IGAUGE = IABS(KGAUGES(I))
      if ( IGAUGE.gt.0 .and. IGAUGE .le. MU ) goto 2004
      if ( nobigout .le. 0 ) write(01,2005)
      write(03,2005)
 2005 format('*** ILLEGAL ASSEMBLY OF DATA FOR SUMMARIES....'/
     &       '*** CALCULATIONS HALTED.......')
      goto 1

 2004 IF = JF(igauge)
      MARK = ' '
      if ( JFCAL(igauge) .eq. 0 ) then
	MARK = '*'
      ipr = 1
	endif
      if ( JF(igauge) .eq. 0 ) then
	MARK = 'X'
      ipr = 2
	endif
      write(03,2)MARK,UNAME(igauge),SFL(I,1),SFL(I,2),F(IF,1),F(IF,2)
    2 format(A1,A32,1X,2F9.2,6X,2F9.2,' ')
    1 continue

      write(03,1701)
 1701 format(77('-'))
      if ( ical .ne. 0 .and. ipr .eq. 1) write(03,1917)
 1917 format('  * - Gap filling not selected')
      if ( ical .ne. 0 .and. ipr .eq. 2) write(03,1947)
 1947 format('  X - No flow data specified')

      if ( iforce gap fill .eq. 1 ) then
      write(03,1967)
 1967 format('Gap filling has been forced in at all ',
     &'flow gauges with specified data')
	endif

*     list details of river quality monitoring points --------------------------
 2000 if ( ical .eq. 1 ) return
      do 9 JP = 1, ndet
      if ( QTYPE (JP) .ne. 4 ) then

      ipr = 0

      write(03,1050)DNAME(JP)
 1050 format(/86('-')/'Summary of Results for Quality ',
     &'Monitoring Stations...... ',A11/
     &86('-')/'Name of Sampling Point            Reach',
     &19X,'Calculated          Observed'/58X,
     &'    Values            Values'/86('-'))

      if (JP .ne. 4) then
      write(03,6092)
 6092 format(
     &55X,'Mean     95%-     Mean     95%-'/
     &55X,'         tile              tile'/86('-'))
      else
      write(03,1055)
 1055 format(
     &55X,'Mean       5%     Mean       5%'/
     &55X,'         tile              tile'/86('-'))
      endif

      do 3 I = 1, LMONP
      itemps = LSAMPOINTS(I)
      IQ = JQ(itemps)
      if ( IQ .eq. 0 ) goto 3

*     calculate the percentiles from mean and standard deviation ---------------
*     used to compute summary statistics at monitoring points ------------------
      call calculate percentiles from mean and standard deviation
     &(JP,DCM,DCS,DCP)

      MARK=' '
      if (JQCAL(itemps) .eq.  0) MARK='*'
      if (JQCAL(itemps) .eq.  0) ipr = 1
      if (JQCAL(itemps) .eq. -1) MARK='+'
      if (JQCAL(itemps) .eq. -1) ipr = 2

      write(03,7944)MARK,UNAME(itemps),RNAME(JREACH(itemps)),
     &Qstations(I,JP,1),Qstations(I,JP,3),DCM,DCP
 7944 format(A1,A32,1X,A16,F9.3,F9.3,F9.3,F9.3)
    3 continue

      write(03,1001)
 1001 format(86('-'))
      if ( ical .ne. 0 .and. ipr .eq. 1 ) write(03,1917)
      if ( ical .eq. 4 .and. ipr .eq. 2 ) write(03,7917)
 7917 format('  + Imperfect gap filling')

	endif

    9 continue

      return
      end



*     write out the headings for the start of the Reach ------------------------
      subroutine write headings at the start of the reach
      include 'COM.FOR'
      character *121 LINE
      dimension kpt(MP10)

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( IPRINT .eq. 0 ) then

*     check for bifurcations ---------------------------------------------------
	if ( jt(kfeat + 1) .eq. 20 .or. jt(kfeat + 1) .eq. 22) then
	write(01,8965)
 8965 format(77('-')/
     &'The flow data for the head of this Reach are those left after ',
     &'diversion to a'/'bifurcation '/77('-'))
	endif
	if ( jt(kfeat + 1) .eq. 21 .or. jt(kfeat + 1) .eq. 23) then
*	write(01,8975)
 8975 format(77('-')/
     &'The flow data for the head of this reach are a diversion ',
     &'through to a bifurcation'/77('-'))
	endif ! bifurcations

*     check for unusual data on river flow -------------------------------------
	if ( Flow(2) .gt. 0.8 * Flow(1) ) then
      call sort format 2 (Flow (1),Flow(2))
	write(33,2003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,FUNIT,valchars11,FUNIT
 2003 format(/77('-')/,A16,7x,'IMPLAUSIBLE DATA ON RIVER FLOW',6x,
     &'Reach Number',I6/77('-')/
     &'Length of Reach: ',F6.1,' km',5X,
     &' Flow at head of Reach:  Mean =',a10,1x,a4/39X,
     &'95-percent exceedence =',a10,1x,a4/77('-'))
	endif

      call write preliminary headings for the reach
	endif

      if ( MONF .gt. 1 ) call write shots for river flow
      
*     identify the file with the monthly flow data -----------------------------
      if ( IF .gt. 0 ) then
      if ( PDRF(IF) .eq. 5 ) then
	if ( nobigout .le. 0 ) then

      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
	write(01,3965) 
 3965 format(77('-')/
     &'Error in flow data for the head of the reach ...'/
     &'The flow data are stated as derived from ',
     &'monthly distributions ... '/
     &'But there is no data file ...'/77('-'))
      call stop
 2970 continue
 
	write(01,2965) flmonthsmall(1,icod)
 2965 format(/77('-')/
     &'The flow data for the head of this Reach were sampled from ',
     &'monthly ...'/'distributions ... File: ',a64/77('-'))

	write(01,4398)
 4398 format(
     &'Month        ','       Mean','        95%','      Shift',
     &'  Correlation'/26x,'  exeedence'/77('-'))

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
      endif
      endif

 2974 continue

*     write out the data on river river quality --------------------------------
      if ( ical .ne. 1 ) then
      if ( nobigout .le. 0 ) then
      write(01,1004)RNAME(IREACH)
      write(21,1004)RNAME(IREACH)
      endif
 1004 format(77('=')/'River quality at the head of the Reach named: ',
     &a16/77('='))
      call get summaries and write out loads and quality (1) ! at head of reach
      if ( ical .ne. 3 ) then
	call write out monthly river quality (1) ! at head of reach
      endif
      endif

*     write the heading for the tabular data -----------------------------------
      if ( IREACH .eq. JREACH(1) ) then
*     heading for 90, 99 and 95-percentile mode --------------------------------
      if ( output mode .ne. 1 ) then
      do ipt = 1, ndet
      kpt(ipt) = kptarg
      if ( Qtype (ipt) .eq. 03 .or. Qtype (ipt) .eq. 05 ) then
	kpt(ipt) = 100 - kpt(ipt)
      endif
      enddo

      if ( iscreen .ne. 3 ) then
      write( *,1494)(DNA(IP),IP=1, ndet)
      write( *,5494)FUNIT,(UNITS(IP),IP=1, ndet)
      write( *,1404)(kpt(IP),IP=1, ndet)
      write( *,1484)
 1494 format(38X,92('-')/37X,'  Dist   Flow   ',10(A4,3X))
 5494 format(39X,'(km)  ',12(1x,A4,2x))
 1404 format(/37X,'  ----     5%    ',10(i2,'%    '))
 1484 format(38X,92('-'))
      endif

      write(09,1494)(DNA(IP),IP=1, ndet)
      write(09,5494)FUNIT,(UNITS(IP),IP=1, ndet)
      write(09,1404)(kpt(IP),IP=1, ndet)
      write(09,1484)
	write(33,4288)
 4288 format(77('-')/)
      write(33,1494)(DNA(IP),IP=1, ndet)
      write(33,5494)FUNIT,(UNITS(IP),IP=1, ndet)
      write(33,1404)(kpt(IP),IP=1, ndet)
      write(33,1484)

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(72,1434)
 1434 format(45('-'),2x,104('-')/47x,'Dist   Flow ',92('-'))
      write(72,5434)FUNIT
 5434 format('Location ...',33X,'  (km)   ',A4,' ',92('-'))
      write(72,1934)
 1934 format(45X,'                                            '/
     &45X,'  ----      Mean        1%        5%       10%       20%',
     &'       50%       80%       90%       95%       99%')
      write(72,1384)
 1384 format(45('-'),2x,104('-'))
      endif
      endif !  if ( ical13 .eq. 0 )
      endif

*     heading for mean mode ----------------------------------------------------
      if ( output mode .eq. 1 ) then

      if ( iscreen .ne. 3 ) then
      write( *,9944)(DNA(IP),IP=1, ndet)
      write( *,9444)FUNIT,(UNITS(IP),IP=1, ndet)
      write( *,9244)
 9944 format(38X,92('-')/37X,'  Dist   Flow   ',10(A4,3X))
 9444 format(37X,'  (km) '10('  ',A4,' '))
 9244 format(/37X,'  ----   ------------ mean',67('-')/38X,92('-'))
	endif

      write(09,9944)(DNA(IP),IP=1, ndet)
      write(09,9444)FUNIT,(UNITS(IP),IP=1, ndet)
      write(09,9244)
      write(33,9944)(DNA(IP),IP=1, ndet)
      write(33,9444)FUNIT,(UNITS(IP),IP=1, ndet)
      write(33,9244)

	if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(72,9964)
 9964 format(46X,'-----------------------------------------',
     &'-----------------------------------------'/
     &45X,'  Dist   Flow   ')
      write(72,9464)FUNIT
 9464 format(47x,'(km)   'A4)
      write(72,1934)
      write(72,1384)
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )
	endif ! if ( output mode .eq. 1 )

*     heading for load report --------------------------------------------------
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(27,9544)(DNA(IP),IP=1, ndet)
      if ( nobigout .le. 0 ) write(27,6544)FUNIT
 9544 format(39X,111('-')/
     &37X,'  Dist   Flow      ',10(A4,6X))
 6544 format(
     &37X,'  (km)  (',A4,')  ------------ (load units) -----------',
     &'-------------------- (load units) --------------------------'/
     &37X,'                                                      '/
     &37X,'  ----   -------------------------- mean -------------',
     &'----------------------------mean---------------------------')
	endif
      endif

      LINE='
     &                                    '

      if ( output mode .eq. 0 ) NMORQ = 3
      if ( output mode .eq. 1 ) NMORQ = 1
      if ( output mode .eq. 2 ) NMORQ = 4
      if ( output mode .eq. 3 ) NMORQ = 5

      call set up output of calculated means or percentiles
      call set up flowa
      call set up flow95

      if ( start reach .eq. 0 ) then
      if ( output mode .ne. 1 ) then
      write(LINE,7500)RNAME(IREACH),ADIST(IREACH),Flowchars(2),
     &(Sevenchars(jdet),jdet=1,ndet)
 7500 format(2X,'Head of ',A16,10X,F7.1,11A7)
      else
      write(LINE,7500)RNAME(IREACH),ADIST(IREACH),Flowchars(1),
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
 	call change colour of text (11) ! cyan
	if ( iscreen .lt. 3 ) write( *,7511)LINE
      call set screen text colour
	endif

      if ( start reach .eq. 1 ) then
      if ( output mode .ne. 1 ) then
      write(LINE,7600)RNAME(IREACH),ADIST(IREACH),Flowchars(2),
     &(Sevenchars(jdet),jdet=1,ndet)
 7600 format(2X,'Continue with ',A16,4X,F7.1,11A7)
      else
      write(LINE,7600)RNAME(IREACH),ADIST(IREACH),Flowchars(1),
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
 	call change colour of text (11) ! cyan
	if ( iscreen .lt. 3 ) write( *,7511)LINE
      call set screen text colour
	endif

      if ( start reach .eq. 2 ) then
      if ( output mode .ne. 1 ) then
      write(LINE,7700)RNAME(IREACH),ADIST(IREACH),Flowchars(2),
     &(Sevenchars(jdet),jdet=1,ndet)
 7700 format(2X,'Mix to form ',A16,6X,F7.1,11A7)
      else
      write(LINE,7700)RNAME(IREACH),ADIST(IREACH),Flowchars(1),
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
 	call change colour of text (11) ! cyan
	if ( iscreen .lt. 3 ) write( *,7511)LINE
      call set screen text colour
	endif ! if ( start reach .eq. 2 )

 7523 continue

      write(09,7511)LINE ! mix of reaches
	write(33,7511)LINE ! mix of reaches
 7511 format(A121)

      call write out loads at the head of the reach
*     calculate and output the percentage of loads from various sources --------
      call outsum at start of reach 

      return
      end


      
      

      subroutine write data for graph plotting 2 (JU)
      include 'COM.FOR'
	character*6 cptest
	character*3 place

      if ( JSKIP .gt. 0 ) return

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads

      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then

      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))

*     calculate loads and the summary statistics of load -----------------------
      call load calculation

      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))

*     compute the extra values for partitioned metals ----------------------------
   	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
     &(3,J,CP2(1,J),CP2(2,J),CP2(3,J),CP2(4,J),CP2(5,J),CP2(6,J),
     &CP2(7,J),CP2(8,J),CP2(9,J),CP2(10,J),CP2(11,J),CP2(12,J))
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J),
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J),
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
	endif ! dissolved and solid metal
      endif
      enddo

*     the following statements are included to identify gap filling points -----
      read(uname(ju)(1:6),'(a6)')cptest	
	if (cptest .eq. '  ....') then
	num = 99
      else
	num = jt(ju)
      endif

      kount22 = kount22 + 1
      write(22,300)ireach,Length of main river-adist(ireach),
     &UNAME(JU),RNAME(IREACH),num
  300 format(I4,',',1PE11.4,',',' " U/S ',A40,'","',A16,'",',i4)
      write(22,301)'[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4),
     &             '[Flow    Obsd]',(0,JCP=1,4)              

      do j=1,MP10
	if ( QTYPE(J) .ne. 4 ) THEN
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12)
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12)
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4)
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4)
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J)
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0
      endif
      enddo

  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))
      
      if ( num .ne. 4 .and. num .ne. 1 ) then
      if ( num .ne. 6 ) then

      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then

      place = 'U/S'
	if ( num .eq. 10 ) place = 'at '
	if ( num .eq. 25 .or. num .eq. 27 .or. num .eq. 29 ) place = 'at '
	if ( num .eq. 31 .or. num .eq. 33 .or. num .eq. 35 ) place = 'at '
	if ( num .eq. 37 .or. num .eq. 40 .or. num .eq. 42 ) place = 'at '
	if ( num .eq. 46 .or. num .eq. 48 ) place = 'at '
	if ( num .eq. 13 .or. num .eq. 15 ) place = 'at '

      write(42,244)ireach,GISCODE(JU),RNAME(IREACH),place,UNAME(JU),num,
     &Length of main river-adist(ireach),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &0.0,0.0,0.0,0.0
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River flow",',',A4,',',9(1PE11.4,','))

      do J=1,MP10
      write(42,245)ireach,GISCODE(JU),RNAME(IREACH),place,UNAME(JU),num,
     &Length of main river-adist(ireach),
     &Dname(J),Units(J),
     &(CL(jcp,J),jcp=1,12),(CD(jcp,J),jcp=1,12),RQO(J),
     &confail(J),propeff2(J),
     &0.0,0.0,0.0,0.0,
     &(class limmits (ic,J),in class (ic,J),ic=1,NC),
     &Total length dets 00(J),Total length dets 50(J),
     &Total length dets 95(J)
  245 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
*     write the extra records for partitioned metals ----------------------------- 
	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
	xzero = 0.0
      write(42,845)ireach,GISCODE(JU),RNAME(IREACH),place,UNAME(JU),num,
     &Length of main river-adist(ireach),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
      write(42,845)ireach,GISCODE(JU),RNAME(IREACH),place,UNAME(JU),num,
     &Length of main river-adist(ireach),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
	endif ! dissolved and solid metal
      enddo ! loop on determinands

      write(42,247)ireach,GISCODE(JU),RNAME(IREACH),place,UNAME(JU),num,
     &Length of main river-adist(ireach),(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &55(1PE11.4,','),11(i2,','),i4,',')

      GISCODE last = GISCODE(JU)

      endif
 	endif

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(72,2844)UNAME(JU),
     &Length of main river-adist(ireach),FLOW(1),(flowstats (i), i=1,9)
 2844 format("U/S ",A40,f7.2,10f10.1)
      endif

      endif
	endif

      return
      end



*     complete calculations for the end of the Reach ---------------------------
      subroutine process the end of the reach
      include 'COM.FOR'
      character *121 LINE
	character * 3 place
	character *40 End of Reach

      if ( IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      write(01,1026)IREACH,RNAME(IREACH)
      if ( ical13 .eq. 0 ) then
      write(21,1026)IREACH,RNAME(IREACH)
 1026 format(//110('=')/'End of reach number',I6,' (',A16,')'/110('='))
      endif
      endif
	endif

      if ( ical .ne. 1 ) then
      if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1927)
 1927 format('River quality at the end of the Reach ...'/77('-'))
      if ( ical13 .eq. 0 ) then
      write(21,1927)
      endif
      endif
      call get summaries and write out loads and quality (0) ! at end of reach
      if ( ical .ne. 2 .or. ical .ne. 3 ) then
	call write out monthly river quality (3) ! at end of reach
	endif ! if ( ical .ne. 2 .or. ical .ne. 3 )
      endif

      call calculate summaries of river flow

	if ( nobigout .eq. 0 ) then
      call sort format 2 (Flow (1),Flow(2))
      write(01,1027)valchars10,FUNIT,valchars11,FUNIT
 1027 format('River flow at the end of the Reach ... ',
     &17X,'Mean =',a10,1x,A4/39X,
     &'95-percent exceedence =',a10,1x,A4/77('='))
      if ( ical13 .eq. 0 ) then
	write(21,1027)valchars10,FUNIT,valchars11,FUNIT
      endif
      if ( ical13 .eq. 0 ) then
      call classify (KFEET,3) ! end of reach
 	call assess compliance with river targets (KFEET,3) ! end of reach
      endif
	endif

      if ( MONF .gt. 1 ) call write shots for river flow
      do jp = 1, ndet
	if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality 
	endif
      enddo
      endif

*     store any data needed for mixing Reaches together later on ---------------
*     loop through all the Reaches ... see if reach number IREACH needs to be --
*     stored for later use -----------------------------------------------------
*     first loop through all the Features, checking for bifurcations -----------
      do 2296 JJ = feeture, MU
*     feature types 11, 20, 21, 22 and 23 are bifurcations ---------------------
      if ( JT(JJ) .eq. 11 .or. JT(JJ) .eq. 20 .or. 
     &JT(JJ) .eq. 21 .or. JT(JJ) .eq. 22 .or. JT(JJ) .eq. 23 ) then
*     bifurcation found .... check if reach IREACH is need for this ------------
      if ( JF(JJ) .eq. IREACH) then ! store bifurcation
*     the current Reach will be used to form other Reaches ---------------------
*     scan through the stores for flow and quality data and find an empty one --
      do ISTOR = 1, KR
      if ( JSTOR (ISTOR) .eq. 0 ) then ! store bifurcation data ----------------
*     an empty store has been found ...  load it up with data ------------------
      IRHOLD (IREACH) = ISTOR ! an empty store has been found for a bifurcation
      JSTOR (ISTOR) = IREACH ! store reach for use as a bifurcation ------------
      maxistor = amax0 (maxistor,istor)
      goto 2298
      endif
      enddo
      write(01,2391)RNAME(IREACH),KR
      write( *,2391)RNAME(IREACH),KR
      write(09,2391)RNAME(IREACH),KR
      write(33,2391)RNAME(IREACH),KR
 2391 format(/77('-')/'*** Unable to store data for a Reach ... ',A16/
     &'*** So that they can be used to form downstream bifurcation ...'/
     &'*** Increase dimensions of JSTOR, FD and QD beyond ',I6/77('-'))
      call stop
      endif
      endif
 2296 continue     
 2298 continue

*     now check whether this Reach will be used to form any other Reach --------
*     loop on all reaches ------------------------------------------------------
      do 2096 KREACH = IREACH, MR
*     check whether another reach is using IREACH ------------------------------
      if ( IPLAN (KREACH,1) .eq. IREACH .or. 
     &     IPLAN (KREACH,2) .eq. IREACH .or. 
     &     IPLAN (KREACH,3) .eq. IREACH ) then 
*    &     IPLAN (KREACH,3) .eq. IREACH .or. 
*    &     IPLAN (IREACH,1) .gt. 0 ) then ! or a straight continuation of IREACH
*     the current Reach is used to form other Reaches --------------------------
*     scan through the stores for flow and quality data ------------------------
*     and find one which is empty ----------------------------------------------
      do ISTOR = 1, KR
      if ( JSTOR (ISTOR) .eq. 0 ) then ! store reach data for use downstream ---
      IRHOLD (IREACH) = ISTOR ! an empty store has been found ------------------
      JSTOR (ISTOR) = IREACH ! load it up with data ----------------------------
	goto 2098
      endif ! if ( JSTOR (ISTOR) .eq. 0 )
      enddo ! do ISTOR = 1, KR
*     no empty store has been found --------------------------------------------
      write(08,*)'KREACH =',KREACH
      write(08,*)'IREACH =',IREACH
      write(08,*)'IPLAN (KREACH,1) =',IPLAN (KREACH,1)
      write(08,*)'IPLAN (KREACH,2) =',IPLAN (KREACH,2)
      write(08,*)'IPLAN (IREACH,1) =',IPLAN (IREACH,1)
      write(01,2091)RNAME(IREACH),KR
      write( *,2091)RNAME(IREACH),KR
      write(09,2091)RNAME(IREACH),KR
      write(08,2091)RNAME(IREACH),KR
      write(33,2091)RNAME(IREACH),KR
 2091 format(77('-')/'*** Unable to store data for a Reach ... ',A16/
     &'*** So that they can be used to form downstream Reaches  ...'/
     &'*** Larger version of SIMCAT required ... '/
     &'*** Or a rearrangement of reaches ... '/
     &I6/77('-'))
      write(08,6299)(JSTOR (ISTOR),Istor = 1, KR)
 6299 format(20i6)
      call stop
	endif ! if ( IPLAN (KREACH,1) etc
 2096 continue ! do 2096 KREACH = 1, MR
 2098 continue ! completed the search for storage space 

*     reaches together ---------------------------------------------------------
*     finished the loop through all the Reaches --------------------------------
*     now store the data in this space -----------------------------------------
      ISTOR = IRHOLD (IREACH)
      if ( ISTOR .eq. 0 ) goto 2099

      do 2501 JPL = 1, NDET
      if ( QTYPE (JPL) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do 2509 J1 = 1, nx    
      TGLODE3   (ISTOR,JPL,J1) = TGLODE2   (JPL,J1)
	TRLOAD3   (ISTOR,JPL,J1) = TRLODE2   (JPL,J1)
	TELOAD3   (ISTOR,JPL,J1) = TELODE2   (JPL,J1)
	T03LOAD3  (ISTOR,JPL,J1) = T03LOAD2  (JPL,J1)
	T05LOAD3  (ISTOR,JPL,J1) = T05LOAD2  (JPL,J1)
	T12LOAD3  (ISTOR,JPL,J1) = T12LOAD2  (JPL,J1)
	T39LOAD3  (ISTOR,JPL,J1) = T39LOAD2  (JPL,J1)
	TNLOADUP3 (ISTOR,JPL,J1) = TNLOADUP2 (JPL,J1)
	TNLOADDN3 (ISTOR,JPL,J1) = TNLOADDN2 (JPL,J1)
      TDDLOAD3  (ISTOR,JPL,J1) = TDDLOAD2  (JPL,J1)
	TALOADUP3 (ISTOR,JPL,J1) = TALOADUP2 (JPL,J1)
	TALOADDN3 (ISTOR,JPL,J1) = TALOADDN2 (JPL,J1)
      TILOADUP3 (ISTOR,JPL,J1) = TILOADUP2 (JPL,J1)
	TILOADDN3 (ISTOR,JPL,J1) = TILOADDN2 (JPL,J1)
      T13LOAD3  (ISTOR,JPL,J1) = T13LOAD2  (JPL,J1)
	T15LOAD3  (ISTOR,JPL,J1) = T15LOAD2  (JPL,J1)
      T25LOAD3  (ISTOR,JPL,J1) = T25LOAD2  (JPL,J1)
	T27LOAD3  (ISTOR,JPL,J1) = T27LOAD2  (JPL,J1)
	T29LOAD3  (ISTOR,JPL,J1) = T29LOAD2  (JPL,J1)
	T31LOAD3  (ISTOR,JPL,J1) = T31LOAD2  (JPL,J1)
	T33LOAD3  (ISTOR,JPL,J1) = T33LOAD2  (JPL,J1)
	T35LOAD3  (ISTOR,JPL,J1) = T35LOAD2  (JPL,J1)
	T37LOAD3  (ISTOR,JPL,J1) = T37LOAD2  (JPL,J1)
	T40LOAD3  (ISTOR,JPL,J1) = T40LOAD2  (JPL,J1)
	T42LOAD3  (ISTOR,JPL,J1) = T42LOAD2  (JPL,J1)
	T46LOAD3  (ISTOR,JPL,J1) = T46LOAD2  (JPL,J1)
	T48LOAD3  (ISTOR,JPL,J1) = T48LOAD2  (JPL,J1)
	TBLOAD3   (ISTOR,JPL,J1) = TBLOAD2   (JPL,J1)
      TGLODE4   (ISTOR,JPL,J1) = TGLODE1   (JPL,J1)
	TRLOAD4   (ISTOR,JPL,J1) = TRLODE1   (JPL,J1)
	TELOAD4   (ISTOR,JPL,J1) = TELODE1   (JPL,J1)
	T03LOAD4  (ISTOR,JPL,J1) = T03LOAD1  (JPL,J1)
	T05LOAD4  (ISTOR,JPL,J1) = T05LOAD1  (JPL,J1)
	T12LOAD4  (ISTOR,JPL,J1) = T12LOAD1  (JPL,J1)
	T39LOAD4  (ISTOR,JPL,J1) = T39LOAD1  (JPL,J1)
	TNLOADUP4 (ISTOR,JPL,J1) = TNLOADUP1 (JPL,J1)
	TNLOADDN4 (ISTOR,JPL,J1) = TNLOADDN1 (JPL,J1)
      TDDLOAD4  (ISTOR,JPL,J1) = TDDLOAD1  (JPL,J1) ! Reach-type diffuse sources 
	TALOADUP4 (ISTOR,JPL,J1) = TALOADUP1 (JPL,J1)
	TALOADDN4 (ISTOR,JPL,J1) = TALOADDN1 (JPL,J1)
      TILOADUP4 (ISTOR,JPL,J1) = TILOADUP1 (JPL,J1)
	TILOADDN4 (ISTOR,JPL,J1) = TILOADDN1 (JPL,J1)
      T13LOAD4  (ISTOR,JPL,J1) = T13LOAD1  (JPL,J1)
	T15LOAD4  (ISTOR,JPL,J1) = T15LOAD1  (JPL,J1)
      T25LOAD4  (ISTOR,JPL,J1) = T25LOAD1  (JPL,J1)
	T27LOAD4  (ISTOR,JPL,J1) = T27LOAD1  (JPL,J1)
	T29LOAD4  (ISTOR,JPL,J1) = T29LOAD1  (JPL,J1)
	T31LOAD4  (ISTOR,JPL,J1) = T31LOAD1  (JPL,J1)
	T33LOAD4  (ISTOR,JPL,J1) = T33LOAD1  (JPL,J1)
	T35LOAD4  (ISTOR,JPL,J1) = T35LOAD1  (JPL,J1)
	T37LOAD4  (ISTOR,JPL,J1) = T37LOAD1  (JPL,J1)
	T40LOAD4  (ISTOR,JPL,J1) = T40LOAD1  (JPL,J1)
	T42LOAD4  (ISTOR,JPL,J1) = T42LOAD1  (JPL,J1)
	T46LOAD4  (ISTOR,JPL,J1) = T46LOAD1  (JPL,J1)
	T48LOAD4  (ISTOR,JPL,J1) = T48LOAD1  (JPL,J1)
	TBLOAD4   (ISTOR,JPL,J1) = TBLOAD1   (JPL,J1)
 2509 continue

      if ( kount works .gt. 0 ) then
      do iworks = 1, kount works
      TELOADAVrch(iworks,ISTOR,JPL) = TELOADAV(iworks,JPL)
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      if ( JPL .eq. ndshot ) then ! 33333333333333333333333333333333333333333333
      TELOADrchshots(iworks,ISTOR,1,is) = TELOADshots(iworks,JPL,is) ! 333333333
      endif ! 333333333333333333333333333333333333333333333333333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! do iworks = 1, kount works
	endif ! if ( kount works .gt. 0 )

      if ( kount bodies .gt. 0 ) then
      do 105 ibodies = 1, kount bodies
      TWloadsrch(ibodies,ISTOR,JPL) = TWLOADS(ibodies,JPL, i13)
      do ip = 1, nprop
*     store the breakdown of annual loads (i13) from upstream sub-catchments ---
      TWloadsrchapp (ibodies,ISTOR,JPL,ip) = 
     &TWLOADSapp (ibodies,JPL, i13,ip)
      enddo

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      if ( JPL .eq. ndshot ) then ! 55555555555555555555555555555555555555555555
      do ip = 1, nprop
      TDLOADrchshots(ibodies,ISTOR,1,is,ip) = 
     &TDLOADshots(ibodies,JPL,is,ip) ! 55555555555555555555555555555555555555555
      enddo
      endif ! if ( JPL .eq. ndshot ) 5555555555555555555555555555555555555555555
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

  105 continue
	endif
  
      endif
 2501 continue     

      do IS=1,NS
      FD (ISTOR,IS) = FMS (IS) ! store the river flows -------------------------
      EFD(ISTOR,IS) = EFMS(IS) ! the proportion of effluent in each shot -------
      do J = 1, ndet ! store the river quality data ----------------------------
      QD(ISTOR,J,IS) = CMS(J,IS)
	do ip = 1, nprop
      EQD(ip,ISTOR,J,IS) = LMS(ip,J,IS)
	enddo
      enddo    
      enddo

 2099 continue

*     store the river quality statistics for graph plotting --------------------
*     set the distance from start of the river to the end of this Reach --------

      Length of main river = RLENGTH(IREACH) + ADIST(IREACH)

      call get summaries of river quality from the shots
      call get summaries of loads

*     loop on the determinands - write out the data ----------------------------
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))

*     calculate loads and the summary statistics of load -----------------------
      call load calculation
      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))

*     compute the extra values for partitioned metals ----------------------------
   	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
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

*     write out the data for graph plotting to file 22 -------------------------
*     if ( virtualreach .eq. -1 ) then
      if ( ical13 .eq. 0 ) then
      write(22,300)ireach,Length of main river-adist(ireach),
     &' " End of Reach                               "',RNAME(IREACH),0
  300 format(I4,',',1PE11.4,',',A47,',"',A16,'",',i4)
      kount22 = kount22 + 1
      write(22,301)'[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4),
     &            '[Flow    Obsd]',(0,JCP=1,4)              

      do j=1,MP10
	if ( QTYPE(J) .ne. 4 ) THEN
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12)
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12)
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4)
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4)
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J)
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0
	endif
      enddo
      endif

  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))
*     endif
      
      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then
      num =999
	End of Reach = "End of reach"

      place = 'at '

      write(42,244)ireach,GISCODE last,RNAME(IREACH),place,End of Reach,
     &num,Length of main river-adist(ireach),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &0.0,0.0,0.0,0.0
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River flow",',',A4,',',9(1PE11.4,','))

      do J=1,MP10
      write(42,245)ireach,GISCODE last,RNAME(IREACH),place,End of Reach,
     &num,Length of main river-adist(ireach),
     &Dname(J),Units(J),
     &(CL(jcp,J),jcp=1,12),(CD(jcp,J),jcp=1,12),RQO(J),
     &confail(J),propeff2(J),
     &0.0,0.0,0.0,0.0,
     &(class limmits (ic,J),in class (ic,J),ic=1,NC),
     &Total length dets 00(J),Total length dets 50(J),
     &Total length dets 95(J)
  245 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
*     write the extra records for partitioned metals ----------------------------- 
	if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
	xzero = 0.0
      write(42,845)ireach,GISCODE last,RNAME(IREACH),place,End of Reach,
     &num,Length of main river-adist(ireach),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
      write(42,845)ireach,GISCODE last,RNAME(IREACH),place,End of Reach,
     &num,Length of main river-adist(ireach),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
	endif ! dissolved and solid metal
      enddo ! loop on determinands

  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))

      write(42,247)ireach,GISCODE last,RNAME(IREACH),place,End of Reach,
     &num,Length of main river-adist(ireach),
     &(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &55(1PE11.4,','),11(i2,','),i4,',')

      endif
	endif

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(72,2642)RNAME(IREACH),distp,
     &FLOW(1),(flowstats (i), i=1,9)
     .,(propstats (i), i=1,9)
 2642 format('End of Reach - ',A16,13x,f7.2,10f10.1/
     &4x,'Proportions of effluent ...',30x,9f10.3)
      write(72,1384)
 1384 format(45('-'),2x,104('-'))
      endif
      endif

      LINE='                                '

      if ( output mode .eq. 0 ) NMORQ = 3
      if ( output mode .eq. 1 ) NMORQ = 1
      if ( output mode .eq. 2 ) NMORQ = 4
      if ( output mode .eq. 3 ) NMORQ = 5

      call set up output of calculated means or percentiles
      call set up flowa
      call set up flow95

      if ( output mode .ne. 1 ) then
      write(LINE,9895)RNAME(IREACH),Length of main river,Flowchars(2),
     &(Sevenchars(jdet),jdet=1,ndet)
 9895 format(5X,'End of Reach: ',A16,F8.1,11A7)
      else
      write(LINE,9895)RNAME(IREACH),Length of main river,Flowchars(1),
     &(Sevenchars(jdet),jdet=1,ndet)
      endif

	call change colour of text (22) ! light blue
      if ( iscreen .ne. 3 ) write( *,7511)LINE
      call set screen text colour
      write(09,7511)LINE ! end of reach
      write(33,7511)LINE ! end of reach
 7511 format(A121)

      line of data = line

      call write out loads at the end of the reach

*     store flow and quality statistics for the end of the Reach ---------------
*     These are required for the calculation of the Effective Sampling Rate ----
*     (in subroutine "get effective sampling rate") ----------------------------
*     and so for the calculation of confidence limits (in COMP)-----------------

      FEND(IREACH)=FLOW(1)

      do JDET= 1, ndet
      QE(JDET,IREACH) = C(JDET,1)
      QDN(JDET,IREACH) = QUALN(JDET)
      enddo

      if ( ical13 .eq. 0 ) then
      if ( IPRINT .eq. 0 ) then
	if ( nobigout .le. 0 ) then
      write(01,3926)IREACH,RNAME(IREACH)
	if ( ical .ne. 2 ) write(21,3926)IREACH,RNAME(IREACH)
 3926 format(24X,53('=')/
     &24X,'CALCULATIONS COMPLETED FOR THIS REACH ...',i12/
     &24X,'Name of reach: ',A16/24X,53('='))
      endif
      endif
	endif

*     calculate and output the percentage of loads from various sources
*     call outsum at end of reach
      return
      end



      subroutine details upstream of feature
      include 'COM.FOR'

      call calculate summaries of river flow

*     if ( IPRINT .ne. 99999 ) then
 	if ( JT(feeture) .ne. 3 .or. JT(feeture) .ne. 5 ) then 

      if ( nobigout .le. 0 ) then
      if ( iprime .eq. 1 ) then
      iprime = 0
	endif
      if ( DIST(feeture) .gt. 0.0001 ) then
	write(01,1010)UNAME(feeture),feeture,
     &IREACH,DIST(feeture),RNAME(IREACH)
      if ( ical13 .eq. 0 ) then
      write(21,1010)UNAME(feeture),feeture,
     &IREACH,DIST(feeture),RNAME(IREACH)
      endif
      endif ! if ( DIST(feeture) .gt. 0.0001 )
      endif ! if ( nobigout .le. 0 )
      if ( ical13 .eq. 0 ) then  
      do ic = 1, ndet
      if ( nobigout .le. 0 ) then
      if ( QTYPE (ic) .ne. 4 ) then
      write(100+ic,1010)UNAME(feeture),feeture,
     &IREACH,DIST(feeture),RNAME(IREACH)
      endif ! if ( QTYPE (ic) .ne. 4 )
      endif
	enddo ! do ic = 1, ndet
 	endif ! if ( ical .eq. 0 
      if ( ical13 .eq. 0 ) then  
      if ( nobigout .le. 0 ) then
      if ( ical .eq. 0 .or. ical .eq. 4 ) write(34,1010)UNAME(feeture),
     &feeture,IREACH,DIST(feeture),RNAME(IREACH)
 1010 format(/110('-')/A40,4X,'Feature No',I6,
     &3X,'Reach No',I6/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ',
     &'of the Reach called ',A16/110('-'))
	if ( JT (feeture) .eq. 3 .or. JT (feeture) .eq. 5 ) then
      if ( IFRQS(KFEAT) .gt. -10 ) then
      if ( ical .eq. 0 .or. ical .eq. 4 ) write(31,1010)UNAME(feeture),
     &feeture,IREACH,DIST(feeture),RNAME(IREACH)
 	endif
      endif

      endif
 	endif ! if ( ical13 .eq. 0 )

      if ( nobigout .le. 0 ) then
*     if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then ! 99999999999
      if ( ical13 .eq. 0 ) then ! 9999999999999999999999999999999999999999999999
*     apply to (3) (5) ------------------------- setting permit limits in mode 7
	if ( JT (feeture) .eq. 3 .or. JT (feeture) .eq. 5 ) then
      if ( IFRQS(KFEAT) .gt. -10 ) then
      write(31,1010)UNAME(feeture),feeture,IREACH,DIST(feeture),
     &RNAME(IREACH)
 	endif
      endif
 	endif ! if ( ical .eq. 07, 08 or 09 ) ! 9999999999999999999999999999999999
      endif ! if ( nobigout .le. 0 ) 

      call write flow in river just upstream

      if ( MONF .gt. 1 ) call write shots for river flow

      endif
*	endif

      return
      end


      
      
      subroutine write flow in river just upstream
      include 'COM.FOR'
      
      if ( dist(feeture). gt. 0.00001 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (Flow (1),Flow(2))
      write(01,1)valchars10,FUNIT,valchars11,FUNIT
	write(21,1)valchars10,FUNIT,valchars11,FUNIT
      if ( ical13 .eq. 0 ) then
	write(34,1)valchars10,FUNIT,valchars11,FUNIT
    1 format(
     &'Flow in river just upstream ...',25X,'Mean =',a10,
     &1x,A4/46X,'95% exceedence =',a10,1x,A4/77('-'))
      endif
	endif
      return
      endif ! if ( dist(feature). gt. 0.00001 )

      return
      end

      subroutine write out loads at the head of the reach ! to channel 27 ------
      include 'COM.FOR'
      character *181 LINEX
      character * 10 Tenchars(MP10)

      if ( JT (feeture) .eq. 10 ) return
      call prepare loads for print out (Tenchars)
      write(LINEX,1)RNAME(IREACH),ADIST(IREACH),FLOW(1),
     &(Tenchars(jdet),jdet=1,ndet)
    1 format(2X,'Start of ',A16,9X,F7.1,F7.1,10A10)
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
 	write(27,2)LINEX ! RICHARD III RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
	endif
      endif
    2 format(A181)
      return
      end

      subroutine write out loads at the end of the reach
      include 'COM.FOR'
      character * 10 Tenchars(MP10)

      call prepare loads for print out (Tenchars)
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(27,1)RNAME(IREACH),Length of main river,
     &(Tenchars(jdet),jdet=1,ndet)
    1 format(150('=')/'End of Reach: ',A16,F12.1,6x,10A10)
	write(27,5)
    5 format(150('='))
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )

      call write loads at this point in the catchment (3) ! at the end of the reach
      if ( nobigout .le. 0 ) call proportion of effluent end 
      
	if ( kount works  .gt. 0 ) then
	jxtype = 111
      if ( ical13 .eq. 0 ) then
	call proportion of works (3) ! at the end of the reach
      do idet = 1, ndet
      if ( nobigout .le. 0 ) then
	if ( QTYPE (idet) .ne. 4 ) then
      if ( n148 .eq. 1 ) write(140+idet,5) ! 55555555555555555555555555555555555
      endif
      endif
      enddo
	endif
	endif
      
	if ( kount bodies .gt. 0 ) then
      call proportion of catchments (3) ! at the end of the reach
      endif

      return
      end

*     write out the river loads ... to channel 27 ------------------------------
*     write out the percentage of loads from various sources ...  channel 40, 41
      subroutine write out the river loads
      include 'COM.FOR'
      character * 10 Tenchars(MP10)

*     skip this routine if the feature is a headwaters of the reach ------------
      if ( JT (feeture) .eq. 10 ) return
*     skip this routine if the distance is zero --------------------------------
*     unless the feature is a sub-catchment boundary ---------------------------
      if ( distp .lt. 0.000001 .and. JT (feeture) .ne. 24 ) return  

      if ( ical13 .eq. 0 ) then
 	if ( nobigout .le. 0 ) then
      call prepare loads for print out (Tenchars)
      write(27,1)UNAME(feeture),Length of main river,
     &(Tenchars(jdet),jdet=1,ndet)
    1 format(//150('-')/'At ',A31,F8.1,6x,10A10)
      write(44,10)UNAME(feeture),Length of main river,
     &(Tenchars(jdet),jdet=1,ndet)
   10 format('At ',A31,F8.1,8x,10A10)
	write(27,2)
    2 format(150('-'))
      endif
*     calculate and output the percentage of loads from various sources --------
	call outsum at this feature
	endif
      
      return
      end

*     write out the river loads after natural purification ....
      subroutine write out the river loads after natural purification
      include 'COM.FOR'
      character * 10 Tenchars(MP10)
      call prepare loads for print out (Tenchars)
      call format the loads for output 4 (Tenchars) 
      return
      end

*     write out the river loads after diffuse sources --------------------------
      subroutine write out the river loads after diffuse sources
      include 'COM.FOR'
      character * 10 Tenchars(MP10),Tenchars1(MP10)

      call prepare loads for print out (Tenchars)
      
      do jdet = 1, MP10
      Tenchars1(jdet) = '          '
      enddo
      
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	wload = addid load(jdet)
      call set format for printout(Tenchars1(jdet),wload)
      endif
      enddo

      call format the loads for output 4 (Tenchars) 

      return
      end

      subroutine write out the river loads before diffuse sources
      include 'COM.FOR'
      character * 10 Tenchars(MP10)

      call prepare loads for print out (Tenchars)
      call format the loads for output 4 (Tenchars) 
      if ( ical13 .eq. 0 ) then
	if ( nobigout .le. 0 ) then
	if ( distp .gt. 1.0e-9 ) then
	write(27,1)Distp
    1 format('Loads added to the river over the next',f6.1,
     &' kilometres'/150('-'))
      write(27,5)(Tenchars(jdet),jdet=1,ndet)
    5 format(5X,'Load before adding diffuse sources',13x,10A10)
      endif
      endif
      endif
      return
      end

*     write out the river loads after diffuse sources --------------------------
      subroutine write diffuse effluent load
      include 'COM.FOR'
*     character *181 LINEX
*     character * 10 Tenchars(MP10)
      
*     call prepare loads for print out (Tenchars)
*     write(LINEX,9495)Length of main river,flow(1),
*    &(Tenchars(jdet),jdet=1,ndet)
*9495 format('After diffuse discharge loads ... ',F9.1,F8.1,10A10)
*     if ( ical13 .eq. 0 ) then
*     if ( nobigout .le. 0 ) write(27,7511)LINEX
*7511 format(A181)
*     endif
      return
      end

      subroutine write loads after gap filling of river flows 2
      include 'COM.FOR'
*     character *181 LINEX
      character * 10 Tenchars(MP10) 

      xltot = 0.0
      do jdet = 1, ndet
	check sub total (jdet) = xupload (jdet,i13)
      xltot = xltot + xupload (jdet,i13)
	enddo
      if ( xltot .lt. 1.0e-09) return
      
*     call format the loads for output 2 (Tenchars)
*     write(LINEX,1)(Tenchars(jdet),jdet=1,ndet) 
*   1 format(5X,'Gap-inflows (additions) ...',18x,10A10)
*     if ( ical13 .eq. 0 ) then
*     if ( nobigout .le. 0 ) write(27,2)LINEX
*   2 format(A181)
*     endif
      call format the loads for output 4 (Tenchars) 
      return
      end
      
      subroutine write loads after gap filling of river flows 3
      include 'COM.FOR'
*     character *181 LINEX
      character * 10 Tenchars(MP10)

      xltot = 0.0
      do jdet = 1, ndet
	check sub total (jdet) = check sub total (jdet) 
     &                       + xdownload (jdet,i13)
      xltot = xltot + xdownload (jdet,i13)
	enddo
      if ( abs (xltot) .lt. 1.0e-09) return

*     call format the loads for output 3 (Tenchars)
*     write(LINEX,1)(Tenchars(jdet),jdet=1,ndet) 
*   1 format(5X,'Gap-inflows (removals) ... ',18x,10A10)
*     if ( ical13 .eq. 0 ) then
*     if ( nobigout .le. 0 ) write(27,2)LINEX
*   2 format(A181)
*     endif
      call format the loads for output 4 (Tenchars) 

      return
      end

      subroutine write loads after gap filling of river quality 2
      include 'COM.FOR'
*     character *181 LINEX
      character * 10 Tenchars(MP10)

      xltot = 0.0
      do jdet = 1, ndet
	check sub total (jdet) = check sub total (jdet) 
     &                       + xupload (jdet,i13)
      xltot = xltot + xupload (jdet,i13)
	enddo
      if ( xltot .lt. 1.0e-09) return

*     call format the loads for output 2 (Tenchars)
*     write(LINEX,1)(Tenchars(jdet),jdet=1,ndet) 
*   1 format(5X,'Gap-quality (additions) ... ',18x,10A10)
*     if ( ical .gt. 3 ) then
*     if ( nobigout .le. 0 ) write(27,2)LINEX
*   2 format(A181)
*     endif
      call format the loads for output 4 (Tenchars) 
      
      return
      end

      subroutine write loads after gap filling of river quality 3
      include 'COM.FOR'
*     character *181 LINEX
      character * 10 Tenchars(MP10)

      xltot = 0.0
      do jdet = 1, ndet
	if ( QTYPE (jdet) .ne. 4 ) then
      xltot = xltot + xdownload (jdet,i13)
	check sub total (jdet) = check sub total (jdet) 
     &                       + xdownload (jdet,i13)
	endif
	enddo
*     if ( abs (xltot) .gt. 1.0e-09) then
*     call format the loads for output 3 (Tenchars)
*     write(LINEX,1)(Tenchars(jdet),jdet=1,ndet) 
*   1 format(5X,'Gap-quality (removals) ... ',18x,10A10)
*     if ( ical .gt. 3 ) then
*     if ( nobigout .le. 0 ) write(27,2)LINEX
*   2 format(A181)
*     endif
*     endif
      call format the loads for output 4 (Tenchars) 
      
      return
      end

      subroutine prepare loads for print out (Tenchars1)
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do jdet = 1, MP10
      Tenchars1(jdet) ='          '
      enddo
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE(jdet) .ne. 4 ) then
      call set format for printout(Tenchars1(jdet),Xload (jdet,1,i13))
      endif
      enddo

      return
      end

      subroutine format the loads for output 2 (Tenchars1)
      include 'COM.FOR'
      character *10 Tenchars1(MP10)
 
      do jdet = 1, MP10
      Tenchars1(jdet) ='          '
      enddo

      do 1 jdet = 1, ndet
      Tenchars1(jdet) = '      ....'

      if ( QTYPE(jdet) .ne. 4 ) then
      Tenchars1(jdet) ='       0.0'
      wload = xupload (jdet,i13)
	if ( abs(wload) .gt. 9.9 ) then
	write(Tenchars1(jdet),7531) wload
 7531 format(F10.1)
      goto 9999
      endif

	if ( abs(wload) .gt. 0.99 ) then
	write(Tenchars1(jdet),7532) wload
 7532 format(F10.2)
      goto 9999
      endif

	if ( abs(wload) .gt. 0.099 ) then
	write(Tenchars1(jdet),7533) wload
 7533 format(F10.3)
      goto 9999
      endif

	if ( abs(wload) .gt. 0.0099 ) then
	write(Tenchars1(jdet),7536) wload
 7536 format(F10.4)
      goto 9999
      endif

	if ( abs(wload) .gt. 0.00099 ) then
	write(Tenchars1(jdet),7537) wload
 7537 format(F10.5)
      goto 9999
      endif

	if ( abs(wload) .gt. 0.00000099 ) then
	write(Tenchars1(jdet),7534) wload
 7534 format(F10.6)
      endif
      
	endif

 9999 continue
    1 continue
      return
      end

      subroutine format the loads for output 3 (Tenchars1)
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do jdet = 1, MP10
      Tenchars1(jdet) ='          '
      enddo
      do 1 jdet = 1, ndet
      Tenchars1(jdet) = '      ....'

      if ( QTYPE(jdet) .ne. 4 ) then
      Tenchars1(jdet) ='       0.0'

      wload = xdownload (jdet,i13)
	if ( abs(wload) .gt. 9.9 ) then
	write(Tenchars1(jdet),7531) wload
 7531 format(F10.1)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.99 ) then
	write(Tenchars1(jdet),7532) wload
 7532 format(F10.2)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.099 ) then
	write(Tenchars1(jdet),7533) wload
 7533 format(F10.3)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.0099 ) then
	write(Tenchars1(jdet),7536) wload
 7536 format(F10.4)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.00099 ) then
	write(Tenchars1(jdet),7537) wload
 7537 format(F10.5)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.00000099 ) then
	write(Tenchars1(jdet),7534) wload
 7534 format(F10.6)
      endif
      
	endif

 9999 continue
    1 continue
      return
      end

      subroutine format the loads for output 4 (Tenchars1) 
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do jdet = 1, MP10
      Tenchars1(jdet) ='          '
      enddo

      wloadkeep = 0.0
      do 1 jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE(jdet) .ne. 4 ) then
      Tenchars1(jdet) ='       0.0'
      wload = check sub total (jdet)
      wloadkeep = wloadkeep + check sub total (jdet)

	if ( abs(wload) .gt. 9.9 ) then
	write(Tenchars1(jdet),7531) wload
 7531 format(F10.1)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.99 ) then
	write(Tenchars1(jdet),7532) wload
 7532 format(F10.2)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.099 ) then
	write(Tenchars1(jdet),7533) wload
 7533 format(F10.3)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.0099 ) then
	write(Tenchars1(jdet),7536) wload
 7536 format(F10.4)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.00099 ) then
	write(Tenchars1(jdet),7537) wload
 7537 format(F10.5)
      goto 9999
      endif
	if ( abs(wload) .gt. 0.00000099 ) then
	write(Tenchars1(jdet),7534) wload
 7534 format(F10.6)
      endif
      
	endif

 9999 continue
    1 continue
      return
      end

*     write out the loads at the end of the run --------------------------------
      subroutine outsum at end of run
      include 'COM.FOR'

      call write loads at this point in the catchment (9) ! at end of the run --

      if ( kount works  .gt. 0 ) then
	jxtype = 999
      if ( ical13 .eq. 0 ) then
	call proportion of works (9) ! end of the run
      do idet = 1, ndet
      if ( nobigout .le. 0 ) then
	if ( QTYPE (idet) .ne. 4 ) then
      if ( n148 .eq. 1 ) write(140+idet,5) ! 55555555555555555555555555555555555
    5 format(150('='))
      endif
      endif
      enddo
      endif
	endif

      if ( kount bodies .gt. 0 ) then
      call proportion of catchments (9) ! at the end of the run
      endif
      
      call add up all the loads

      return
      end

*     sum the loads added to the river -----------------------------------------
      subroutine add up all the loads
      include 'COM.FOR'

      do i = 1, ndet
      if ( qtype (i) .ne. 4 ) then
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      

*     net total loads --------------------------------------------------TGLODE2 
*     net load introduced upstream boundaries and tributaries ----------TRLODE2
      TGLODE2(i,J1) = amax1 ( 0.0, TRLODE2(i,J1) ) 
*     net load from point discharges of effluent -----------------------TELODE2
     &              + amax1 ( 0.0, TELODE2(i,J1) )
*     net load introduced by clean diffuse sources --------------------TDDLOAD2
     &              + amax1 ( 0.0, TDDLOAD2(i,J1) )
*     net load introduced by natural purification --------------------TNLOADUP2
     &              + amax1 ( 0.0, TNLOADUP2(i,J1) )
*     net load added by gap filling for river river quality ----------TALOADUP2
     &              + amax1 ( 0.0, TALOADUP2(i,J1) )
*     net load introduced by gap filling for river flows -------------TILOADUP2
     &              + amax1 ( 0.0, TILOADUP2(i,J1) )
*     net load from diffuse features (river flow type) ----------------T13LOAD2
     &              + amax1 ( 0.0, T13LOAD2(i,J1) )
*     net load from diffuse features (livestock) ------------------------------
     &              + amax1 ( 0.0, T25LOAD2(i,J1) )
*     net load from diffuse features (arable) ---------------------------------
     &              + amax1 ( 0.0, T27LOAD2(i,J1) )
     &              + amax1 ( 0.0, T29LOAD2(i,J1) )
     &              + amax1 ( 0.0, T31LOAD2(i,J1) )
     &              + amax1 ( 0.0, T33LOAD2(i,J1) )
     &              + amax1 ( 0.0, T35LOAD2(i,J1) )
     &              + amax1 ( 0.0, T37LOAD2(i,J1) )
     &              + amax1 ( 0.0, T40LOAD2(i,J1) )
     &              + amax1 ( 0.0, T46LOAD2(i,J1) )
     &              + amax1 ( 0.0, T48LOAD2(i,J1) )
*     total load from diffuse features (discharge flow type) --------- T15LOAD2
     &              + amax1 ( 0.0, T42LOAD2(i,J1) )
     &              + amax1 ( 0.0, T15LOAD2(i,J1) )
      TLOSSES(i,J1) = TBLOAD2 (i,J1) 
     &              + TALOADDN2 (i,J1)
     &              + TILOADDN2 (i,J1)
     &              + TNLOADDN2 (i,J1)
      TLOSSES1(i,J1) = TBLOAD1 (i,J1) 
     &               + TALOADDN1 (i,J1)
     &               + TILOADDN1 (i,J1)
     &               + TNLOADDN1 (i,J1)

*     total loads ------------------====--------------------------------TGLODE1 
*     load introduced upstream boundaries and tributaries --------------TRLODE1
      TGLODE1(i,J1) = amax1 ( 0.0, TRLODE1(i,J1) ) 
*     total load from point discharges of effluent ---------------------TELODE1
     &              + amax1 ( 0.0, TELODE1(i,J1) )
*     total load introduced by Reach-type diffuse sources -------------TDDLOAD1
     &              + amax1 ( 0.0, TDDLOAD1(i,J1) )
*     load introduced by natural purification ------------------------TNLOADUP1
     &              + amax1 ( 0.0, TNLOADUP1(i,J1) )
*     load added by gap filling for river river quality --------------TALOADUP1
     &              + amax1 ( 0.0, TALOADUP1(i,J1) )
*     load introduced by gap filling for river flows -----------------TILOADUP1
     &              + amax1 ( 0.0, TILOADUP1(i,J1) )
*     load from diffuse features (river flow type) --------------------T13LOAD1
     &              + amax1 ( 0.0, T13LOAD1(i,J1) )
*     load from diffuse features (livestock) ----------------------------------
     &              + amax1 ( 0.0, T25LOAD1(i,J1) )
*     load from diffuse features (arable) -------------------------------------
     &              + amax1 ( 0.0, T27LOAD1(i,J1) )
     &              + amax1 ( 0.0, T29LOAD1(i,J1) )
     &              + amax1 ( 0.0, T31LOAD1(i,J1) )
     &              + amax1 ( 0.0, T33LOAD1(i,J1) )
     &              + amax1 ( 0.0, T35LOAD1(i,J1) )
     &              + amax1 ( 0.0, T37LOAD1(i,J1) )
     &              + amax1 ( 0.0, T40LOAD1(i,J1) )
     &              + amax1 ( 0.0, T46LOAD1(i,J1) )
     &              + amax1 ( 0.0, T48LOAD1(i,J1) )
*     total load from diffuse features (discharge flow type) ----------T15LOAD1
     &              + amax1 ( 0.0, T42LOAD1(i,J1) )
     &              + amax1 ( 0.0, T15LOAD1(i,J1) )
*    &              + amax1 ( 0.0, TBLOAD1(i,J1)  )
*    &              + amax1 ( 0.0, TALOADDN1(i,J1))
*    &              + amax1 ( 0.0, TILOADDN1(i,J1))
*    &              + amax1 ( 0.0, TNLOADDN1(i,J1))

	enddo

      endif
      enddo
      return
      end
      

*     calculate and output the percentage of loads from various sources
      subroutine outsum at this feature
      include 'COM.FOR'

      call write loads at this point in the catchment (1) ! at this feature
      
      if ( nobigout .le. 0 ) call proportion of effluent (0)
      
	if ( kount works  .gt. 0 ) then
	jxtype = jt(feeture)
      if ( ical13 .eq. 0 ) then
	call proportion of works (1) ! at this feature
      endif ! if ( ical13 .eq. 0 )
	endif ! if ( kount works  .gt. 0 )
      
	if ( kount bodies .gt. 0 ) then
      call proportion of catchments (1) ! at this feature
      endif
      
      return
      end

*     calculate and output the percentage of loads from various sources --------
      subroutine outsum at start of reach 
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(27,1000)RNAME(IREACH)
 1000 format(/150('=')/'Head of reach: ',A16/150('='))
      write(44,1010)RNAME(IREACH),(dna(idet),idet = 1, ndet)
 1010 format(/150('=')/'Head of reach: ',A16,19x,'Mean total',
     &' loads for determinands ...'/150('-')/50x,10(6x,a4))
      write(44,1020)
 1020 format(150('='))
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )

      call write loads at this point in the catchment (0) ! at the start of the reach
      if ( nobigout .le. 0 ) call proportion of effluent start 
	if ( kount works  .gt. 0 ) then
	jxtype = 111
      if ( ical13 .eq. 0 ) then
	call proportion of works (0) ! at start of the reach
      endif ! if ( ical13 .eq. 0 )
	endif ! if ( kount works  .gt. 0 )
      
	if ( kount bodies .gt. 0 ) then
      call proportion of catchments (0) ! at start of the reach
      endif

      return
      end


*     calculate and output the percentage of loads from various sources at the -
*     end of the reach and at the end of the modelled catchment ----------------
      subroutine write loads at this point in the catchment (kdirect)
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      call add up all the loads

*     initialise the values of the proportions of load -------------------------
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      prop10    (idet) = 0.0
      propRT    (idet) = 0.0
      propNPU   (idet) = 0.0
      propeff2  (idet) = 0.0
      propeff3  (idet) = 0.0
      propeff12 (idet) = 0.0
      propeff5  (idet) = 0.0
      propeff39 (idet) = 0.0
      prop13    (idet) = 0.0
      prop15    (idet) = 0.0
      prop42    (idet) = 0.0
      prop25    (idet) = 0.0
      prop27    (idet) = 0.0
      prop29    (idet) = 0.0
      prop31    (idet) = 0.0
      prop33    (idet) = 0.0
      prop35    (idet) = 0.0
      prop46    (idet) = 0.0
      prop48    (idet) = 0.0
      prop37    (idet) = 0.0
      prop40    (idet) = 0.0
      propabs   (idet) = 0.0
      propNPD   (idet) = 0.0
      propfra   (idet) = 0.0
      propqra   (idet) = 0.0
      propfrl   (idet) = 0.0
      propqrl   (idet) = 0.0
      prolosses (idet) = 0.0
      proadds   (idet) = 0.0
*     calculate the proportions of load ----------------------------------------
      if ( TGLODE2(idet,i13) .gt. 0.0 ) then
      prop10    (idet) = 100.0 * TRLODE2(idet,i13)/TGLODE2 (idet,i13)
      propRT    (idet) = 100.0 * TDDLOAD2(idet,i13)/TGLODE2 (idet,i13)
      propNPU   (idet) = 100.0 * TNLOADUP2(idet,i13)/TGLODE2 (idet,i13)
      propfrl   (idet) = 100.0 * TILOADUP2(idet,i13)/TGLODE2 (idet,i13)
      propqrl   (idet) = 100.0 * TALOADUP2(idet,i13)/TGLODE2 (idet,i13)
      propeff2  (idet) = 100.0 * TELODE2(idet,i13)/TGLODE2 (idet,i13)
      propeff3  (idet) = 100.0 * T03LOAD2(idet,i13)/TGLODE2 (idet,i13)
      propeff12 (idet) = 100.0 * T12LOAD2(idet,i13)/TGLODE2 (idet,i13)
      propeff5  (idet) = 100.0 * T05LOAD2(idet,i13)/TGLODE2 (idet,i13)
      propeff39 (idet) = 100.0 * T39LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop13    (idet) = 100.0 * T13LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop15    (idet) = 100.0 * T15LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop42    (idet) = 100.0 * T42LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop25    (idet) = 100.0 * T25LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop27    (idet) = 100.0 * T27LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop29    (idet) = 100.0 * T29LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop31    (idet) = 100.0 * T31LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop33    (idet) = 100.0 * T33LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop35    (idet) = 100.0 * T35LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop46    (idet) = 100.0 * T46LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop48    (idet) = 100.0 * T48LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop37    (idet) = 100.0 * T37LOAD2(idet,i13)/TGLODE2 (idet,i13)
      prop40    (idet) = 100.0 * T40LOAD2(idet,i13)/TGLODE2 (idet,i13)
*     sum the proportions of load ----------------------------------------------
      propall   (idet) = prop10(idet)
     &                 + propNPU(idet)
     &                 + propfrl(idet)
     &                 + propqrl(idet)
     &                 + propeff2(idet)
     &                 + propRT(idet)
     &                 + prop13(idet)
     &                 + prop15(idet)
     &                 + prop25(idet)
     &                 + prop27(idet)
     &                 + prop29(idet)
     &                 + prop31(idet)
     &                 + prop33(idet)
     &                 + prop35(idet)
     &                 + prop46(idet)
     &                 + prop48(idet)
     &                 + prop37(idet)
     &                 + prop40(idet)
     &                 + prop42(idet)
      propabs   (idet) = 100.0 * TBLOAD2  (idet,i13)/TGLODE2 (idet,i13)
      propNPD   (idet) = 100.0 * TNLOADDN2(idet,i13)/TGLODE2 (idet,i13)
      propfra   (idet) = 100.0 * TILOADDN2(idet,i13)/TGLODE2 (idet,i13)
      propqra   (idet) = 100.0 * TALOADDN2(idet,i13)/TGLODE2 (idet,i13)
      prolosses (idet) = propNPD (idet) + propfra (idet) 
     &                 + propqra (idet) + propabs (idet) 
      proadds (idet) = propfrl (idet) 
     &               + propqrl (idet) 
      endif
	endif
      enddo

      if ( ical .gt. 0 .and. ical .lt. 4 ) return
      if ( nobigout .gt. 0 ) return
      
*     write the headings =======================================================
      if ( kdirect .eq. 0 ) then ! the start of the reach ----------------------
      write(27,8952)rname(ireach)
 8952 format(//150('=')/'Summary and origins of net total loads at ', ! start
     &'the start of the reach ... ',a16/150('=')/
     &48x,' ......Net',' .........',' ... Total'/
     &48x,' ....Total',' ..... (%)',' .... Load'/
     &48x,' .....Load',' .........',' .........'/
     &150('='))
      
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(120+ic,8952)rname(ireach)
	endif
	enddo
      endif ! the start of the reach -------------------------------------------
      
      if ( kdirect .eq. 1 ) then ! other points in of the reach ----------------
      write(27,8939)rname(ireach),uname(feeture)
 8939 format(//150('=')/'Summary and origins of net total loads at ', ! point
     &'this point in reach: ',a16,' .... at: ',a37/150('=')/
     &48x,' ......Net',' .........',' ... Total'/
     &48x,' ....Total',' ..... (%)',' .... Load'/
     &48x,' .....Load',' .........',' .........'/
     &150('='))
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(120+ic,8939)rname(ireach),uname(feeture)
	endif
	enddo
      endif ! other points in of the reach -------------------------------------
      
      if ( kdirect .eq. 3 ) then ! the end of the reach ------------------------
      write(27,8932)rname(ireach)
 8932 format(//150('=')/'Summary and origins of net total loads at ', ! end
     &'the end of the reach ... ',a16/150('=')/
     &48x,' ......Net',' .........',' ... Total'/
     &48x,' ....Total',' ..... (%)',' .... Load'/
     &48x,' .....Load',' .........',' .........'/
     &150('='))
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(120+ic,8932)rname(ireach)
	endif
	enddo
      endif ! the end of the reach ---------------------------------------------
      
      if ( kdirect .eq. 9 ) then ! the end of the entire model -----------------
      write(27,8962)
 8962 format(////150('-')/'Summary and origins of net total loads at ',
     &'the end of the modelled catchment ...'/150('-')/
     &48x,' ......Net',' .........',' ... Total'/
     &48x,' ....Total',' ..... (%)',' .... Load'/
     &48x,' .....Load',' .........',' .........'/
     &150('-'))
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(120+ic,8962)
	endif
	enddo
      endif ! the end of the entire model --------------------------------------
*     =========================================================== write headings 

      kp = 0 ! net total loads from boundaries and tributaries -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TRLODE2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TRLODE2(jdet,i13),prop10(jdet),
     &TRLODE1(jdet,i13))
      write(120+jdet,3930)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
 	if ( kp .eq. 1 ) write(27,3930)(Tenchars1(jdet),jdet=1,ndet)
 3930 format('+',2x,'Boundaries and tributaries (2, 10 and 12)',
     &4x,10A10)
      
      kp = 0 ! net total loads from Reach-type diffuse sources ------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TDDLOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TDDLOAD2(jdet,i13),propRT(jdet),
     &TDDLOAD1(jdet,i13))
      write(120+jdet,4930)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
 	if ( kp .eq. 1 ) write(27,4930)(Tenchars1(jdet),jdet=1,ndet)
 4930 format('+',2x,'Reach-type diffuse sources',19x,10A10)
      
      kp = 0 ! loads added by natural purification -----------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TNLOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TNLOADUP2(jdet,i13),propNPU(jdet),
     &TNLOADUP1(jdet,i13))
      write(120+jdet,8931)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8931)(Tenchars1(jdet),jdet=1,ndet)
 8931 format('+',2x,'Added by natural purification',16X,10A10)
      
      kp = 0 ! loads introduced by gap filling for river flows -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TILOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TILOADUP2(jdet,i13),propfrl(jdet),
     &TILOADUP1(jdet,i13))
      write(120+jdet,2943)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,2943)(Tenchars1(jdet), jdet=1,ndet)
 2943 format('+',2x,'Added by gap filling of flow',17x,10A10)
      
      kp = 0 ! loads introduced by gap filling of river quality ----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TALOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TALOADUP2(jdet,i13),propqrl(jdet),
     &TALOADUP1(jdet,i13))
      write(120+jdet,2944)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,2944)(Tenchars1(jdet), jdet=1,ndet)
 2944 format('+',2x,'Added by gap filling of quality',14X,10A10)
      
      kp = 0 ! net loads introduced by all discharges of effluent --------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TELODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TELODE2(jdet,i13),propeff2(jdet),
     &TELODE1(jdet,i13))
*     write(120+jdet,3930)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
*	if ( kp .eq. 1 ) write(27,8933)(Tenchars1(jdet), jdet=1,ndet)
 8933 format(5X,'Net from discharges (types 3, 12 and 5)',6x,10A10)
*     if ( kp .eq. 1 ) write(27,8972)
*8972 format(5x,'(Net of upstream losses ...)')
*	if ( kp .eq. 1 ) write(27,4826)
 4826 format(150('-'))
      
      kp = 0  ! net loads from sewage effluents (3) ----------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T03LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T03LOAD2(jdet,i13),propeff3(jdet),
     &T03LOAD1(jdet,i13))
      write(120+jdet,4733)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4733)(Tenchars1(jdet),jdet=1,ndet)
 4733 format('+',2x,'Net total from sewage effluents (3)',10x,10A10)
      
      kp = 0 ! net loads from intermittent discharges of sewage (12) -----------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T12LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T12LOAD2(jdet,i13),propeff12(jdet),
     &T12LOAD1(jdet,i13))
      write(120+jdet,4739)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4739)(Tenchars1(jdet),jdet=1,ndet)
 4739 format('+',2x,'Net intermittent discharges of sewage (12)   ',
     &10A10)
      
      kp = 0 ! net loads from industrial discharges (5) ---------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T05LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T05LOAD2(jdet,i13),propeff5(jdet),
     &T05LOAD1(jdet,i13))
      write(120+jdet,5933)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,5933)(Tenchars1(jdet),jdet=1,ndet)
 5933 format('+',2x,'Net total from industrial discharges (5)',5x,10A10)
      
      kp = 0 ! net loads from mine waters (39) ---------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T39LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T39LOAD2(jdet,i13),propeff39(jdet),
     &T39LOAD1(jdet,i13))
      write(120+jdet,8039)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8039)(Tenchars1(jdet),jdet=1,ndet)
 8039 format('+',2x,'Net total from mine waters (39)',14x,10A10)
	if ( kp .eq. 1 ) write(27,4826)
      
      kp = 0 ! net loads from River-type diffuse pollution (13) -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T13LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T13LOAD2(jdet,i13),prop13(jdet),
     &T13LOAD1(jdet,i13))
      write(120+jdet,8113)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8113)(Tenchars1(jdet),jdet=1,ndet)
 8113 format('+',2x,'River-type diffuse pollution (13)',12X,10A10)
      
      kp = 0 ! net loads from Effluent-type diffuse pollution (15) -------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T15LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T15LOAD2(jdet,i13),prop15(jdet),
     &T15LOAD1(jdet,i13))
      write(120+jdet,8233)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8233)(Tenchars1(jdet),jdet=1,ndet)
 8233 format('+',2x,'Effluent-type diffuse pollution (15)',9X,10A10)
      
      kp = 0 ! net loads from Diffuse pollution from livestock (25) ------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T25LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T25LOAD2(jdet,i13),prop25(jdet),
     &T25LOAD1(jdet,i13))
      write(120+jdet,8125)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8125)(Tenchars1(jdet),jdet=1,ndet)
 8125 format('+',2x,'Diffuse pollution from livestock (25)',8X,10A10)
      
      kp = 0 ! net loads from Diffuse pollution from arable (27) ---------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T27LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T27LOAD2(jdet,i13),prop27(jdet),
     &T27LOAD1(jdet,i13))
      write(120+jdet,8127)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8127)(Tenchars1(jdet),jdet=1,ndet)
 8127 format('+',2x,'Diffuse pollution from arable (27)',11X,10A10)
      
      kp = 0 ! net loads from Highway runoff (29) ------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T29LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T29LOAD2(jdet,i13),prop29(jdet),
     &T29LOAD1(jdet,i13))
      write(120+jdet,8129)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8129)(Tenchars1(jdet),jdet=1,ndet)
 8129 format('+',2x,'Highway runoff (29)',26X,10A10)
      
      kp = 0 ! net loads from 'Urban runoff (31) -------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T31LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T31LOAD2(jdet,i13),prop31(jdet),
     &T31LOAD1(jdet,i13))
      write(120+jdet,8131)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8131)(Tenchars1(jdet),jdet=1,ndet)
 8131 format('+',2x,'Urban runoff (31)',28X,10A10)
      
      kp = 0 ! net loads from Atmospheric deposition (33) ----------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T33LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T33LOAD2(jdet,i13),prop33(jdet),
     &T33LOAD1(jdet,i13))
      write(120+jdet,8133)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8133)(Tenchars1(jdet),jdet=1,ndet)
 8133 format('+',2x,'Atmospheric deposition (33)',18x,10A10)
      
      kp = 0 ! net loads from natural background (35) --------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T35LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T35LOAD2(jdet,i13),prop35(jdet),
     &T35LOAD1(jdet,i13))
      write(120+jdet,8135)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8135)(Tenchars1(jdet),jdet=1,ndet)
 8135 format('+',2x,'Natural background (35)',22x,10A10)
      
      kp = 0 ! net total loads from Aggregated STWs (42) -----------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T42LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T42LOAD2(jdet,i13),prop42(jdet),
     &T42LOAD1(jdet,i13))
      write(120+jdet,8142)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8142)(Tenchars1(jdet),jdet=1,ndet)
 8142 format('+',2x,'Aggregated STWs (42)',25x,10A10)
      
      kp = 0 ! net total loads from diffuse mines (46) -------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T46LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T46LOAD2(jdet,i13),prop46(jdet),
     &T46LOAD1(jdet,i13))
      write(120+jdet,8146)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8146)(Tenchars1(jdet),jdet=1,ndet)
 8146 format('+',2x,'Diffuse mines (46)',27x,10A10)
      
      kp = 0 ! net total loads from birds, boats and angling (48) --------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T48LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T48LOAD2(jdet,i13),prop48(jdet),
     &T48LOAD1(jdet,i13))
      write(120+jdet,8148)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8148)(Tenchars1(jdet),jdet=1,ndet)
 8148 format('+',2x,'Birds, boats and angling (48)',16x,10A10)
      
      kp = 0 ! total loads from septic tanks (37) ------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T37LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T37LOAD2(jdet,i13),prop37(jdet),
     &T37LOAD1(jdet,i13))
      write(120+jdet,8137)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8137)(Tenchars1(jdet),jdet=1,ndet)
 8137 format('+',2x,'Septic tanks (37)',28x,10A10)
      
      kp = 0 ! net total loads from Aggregate CSOs (40) ------------------------
      do jdet = 1, ndet 
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T40LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (T40LOAD2(jdet,i13),prop40(jdet),
     &T40LOAD1(jdet,i13))
      write(120+jdet,8140)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8140)(Tenchars1(jdet),jdet=1,ndet)
 8140 format('+',2x,'Aggregate CSOs (40)',26x,10A10)
      
      if ( kdirect .eq. 0 ) then ! totals at the start of the reach ------------
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TGLODE2(jdet,i13),propall(jdet),
     &TGLODE1(jdet,i13))
      write(120+jdet,1443)valchars10,valchars11,valchars12
 1443 format(150('-')/'TOTALS ...',38x,3A10/150('-'))
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8443)(Tenchars1(jdet), jdet=1,ndet)
 8443 format(150('-')/'NET TOTAL LOADS at the start of the reach',
     &7x,10A10/150('='))
      write(01,8843)(Tenchars1(jdet), jdet=1,ndet)
 8843 format(/150('=')/'NET TOTAL LOADS at the start of reach',
     &3x,10A10/150('='))
      endif
      endif ! start of the reach -----------------------------------------------
      
      if ( kdirect .eq. 1 ) then ! at this point -------------------------------
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TGLODE2(jdet,i13),propall(jdet),
     &TGLODE1(jdet,i13))
      write(120+jdet,1443)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8553)(Tenchars1(jdet), jdet=1,ndet)
 8553 format(150('-')/'NET TOTAL LOADS at this point ',
     &18x,10A10)
      write(01,8858)uname(feeture),(Tenchars1(jdet), jdet=1,ndet)
 8858 format(/150('=')/'NET TOTAL LOADS at ',a21,10A10/150('='))
      endif
      endif ! at this point -----------------------------------------------------
      
      if ( kdirect .eq. 3 ) then ! at the end of the reach ----------------------
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TGLODE2(jdet,i13),propall(jdet),
     &TGLODE1(jdet,i13))
      write(120+jdet,1443)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8333)(Tenchars1(jdet), jdet=1,ndet)
 8333 format(150('-')/'NET TOTAL LOADS at the end of the reach ',
     &8x,10A10/150('='))
      write(01,8833)(Tenchars1(jdet), jdet=1,ndet)
 8833 format(/150('=')/'NET TOTAL LOADS at the end of the reach ',
     &10A10/150('='))
      
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TGLODE2(jdet,i13),propall(jdet),
     &TGLODE1(jdet,i13))
      write(120+jdet,8093)valchars10,valchars11,valchars12
 8093 format(150('-')/'TOTAL LOADS at the end of the reach',3x,
     &10x,3A10/150('='))
      Tenchars1(jdet) = valchars10
      endif
      enddo

      endif
      endif ! at the end of the reach -------------------------------------------
      
      if ( kdirect .eq. 9 ) then ! at the end of the model ----------------------
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TGLODE2(jdet,i13),propall(jdet),
     &TGLODE1(jdet,i13))
      write(120+jdet,8943)valchars10,valchars11,valchars12
 8943 format(150('-')/'TOTAL LOADS at the end of the model',3x,
     &10x,3A10/150('='))
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8343)(Tenchars1(jdet), jdet=1,ndet)
 8343 format(150('-')/'NET TOTAL LOADS at the end of the model',
     &9x,10A10/150('='))
      write(01,8883)(Tenchars1(jdet), jdet=1,ndet)
 8883 format(/150('=')/'NET TOTAL LOADS at the end of the model',3x,
     &10A10/150('='))
      endif
      endif ! at the end of the model ------------------------------------------
      
 	if ( kp .eq. 1 ) write(27,7334)
 	if ( kp .eq. 1 ) write(01,7334)
 7334 format(150('-'))

	kp = 0
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (TLOSSES(idet,i13)) .gt. 1.0e-10 ) kp = 1
	if ( kp .eq. 1 ) write(120+idet,4196)
      if ( kp .eq. 1 ) write(120+idet,4806)
      endif
      enddo
      
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	Tenchars1(idet) = dna(idet)
      endif
      enddo
	if ( kp .eq. 1 ) write(27,4196)(Tenchars1(idet),idet = 1, ndet)
 	if ( kp .eq. 1 ) write(01,4196)(Tenchars1(idet),idet = 1, ndet)
 4196 format('Total loads that have been ...',18x,
     &10(6x,a4))
      if ( kp .eq. 1 ) write(27,4806)
      if ( kp .eq. 1 ) write(01,4806)

      kp = 0 ! total loads from abstractions -----------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TBLOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TBLOAD2(jdet,i13),propabs(jdet),
     &TBLOAD1(jdet,i13))
      write(120+jdet,8246)valchars10,valchars11,valchars12
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8246)(Tenchars1(jdet),jdet=1,ndet)
 8246 format('-',2x,'Removed by abstractions (7, 18 and 19)',7x,10A10)
      write(01,8846)(Tenchars1(jdet),jdet=1,ndet)
 8846 format('Removed by abstractions (7, 18 and 19)',2X,10A10)
      endif
*     loads removed by natural purification ------------------------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      Tenchars1(jdet) = '         0'
	if ( abs (TNLOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TNLOADDN2(jdet,i13),propNPD(jdet),
     &TNLOADDN1(jdet,i13))
      write(120+jdet,1931)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
*     if ( kp .eq. 1 ) then
      write(27,1931)(Tenchars1(jdet),jdet=1,ndet)
 1931 format('-',2x,'Removed by natural purification',14x,10A10)
      write(01,9938)(Tenchars1(jdet),jdet=1,ndet)
 9938 format('Removed by natural purification',9X,10A10)
*     endif
*     loads removed by gap filling for flows -----------------------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TILOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TILOADDN2(jdet,i13),propfra(jdet),
     &TILOADDN1(jdet,i13))
      write(120+jdet,8913)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8913)(Tenchars1(jdet), jdet=1,ndet)
 8913 format('-',2x,'Removed by gap filling of flows',14x,10A10)
      write(01,9913)(Tenchars1(jdet), jdet=1,ndet)
 9913 format('Removed by gap filling of flows',9x,10A10)
      endif
*     total loads removed by gap filling of river quality ----------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TALOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TALOADDN2(jdet,i13),propqra(jdet),
     &TALOADDN1(jdet,i13))
      write(120+jdet,8964)valchars10,valchars11,valchars12
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8964)(Tenchars1(jdet), jdet=1,ndet)
 8964 format('-',2x,'Removed by gap filling of quality',12x,10A10)
      write(27,9964)(Tenchars1(jdet), jdet=1,ndet)
 9964 format('Removed by gap filling of quality',7x,10A10)
      endif
*     total losses of load -----------------------------------------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      Tenchars1(jdet) = '         0'
	if ( abs (TLOSSES1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 3 (TLOSSES(jdet,i13),prolosses(jdet),
     &TLOSSES1(jdet,i13))
      write(120+jdet,2964)valchars10,valchars11,valchars12
 2964 format(150('-')/'TOTAL LOAD REMOVED ...',26x,3A10/150('='))
      Tenchars1(jdet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,7964)(Tenchars1(jdet), jdet=1,ndet)
 7964 format(150('-')/'TOTAL LOAD REMOVED ...',26x,10A10)
      write(01,7764)(Tenchars1(jdet), jdet=1,ndet)
 7764 format(150('-')/'TOTAL LOAD REMOVED ...',18x,10A10)
      write(27,7334)
      write(01,7334)
      endif

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	Tenchars1(idet) = dna(idet)
      endif
      enddo
      write(27,4266)(Tenchars1(idet),idet = 1, ndet)
 4266 format(150('=')/'Percentages of net load from all inputs ',8x,
     &10(6x,a4))
	write(01,4166)(Tenchars1(idet),idet = 1, ndet)
 4166 format(150('=')/'Percentages of net load from all inputs ',
     &10(6x,a4))
      write(27,4806)
      write(01,4806)
      
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop10(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop10(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,3933)(Tenchars1(idet), idet=1,ndet)
 3933 format('+',2x,'Boundaries and tributaries (2, 10 and 12)',
     &4x,10A10)
      write(01,8930)(Tenchars1(idet), idet=1,ndet)
 8930 format('Boundaries and tributaries (2, 10, 12)',2x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propRT(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propRT(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,9163)(Tenchars1(idet), idet=1,ndet)
 9163 format('+',2x,'Reach-type diffuse sources',19X,10A10)
      write(01,9863)(Tenchars1(idet), idet=1,ndet)
 9863 format('Reach-type diffuse sources',14X,10A10)
      endif
*     ==========================================================================
*     introduced by natural purification .....
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propNPU(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propNPU(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,9103)(Tenchars1(idet), idet=1,ndet)
 9103 format('+',2x,'Added by natural purification',16X,10A10)
      write(01,9803)(Tenchars1(idet), idet=1,ndet)
 9803 format('Added by natural purification',11X,10A10)
      endif
*     =========================================================================
*     loads added by gap filling for flows ----------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propfrl(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propfrl(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8923)(Tenchars1(idet), idet=1,ndet)
 8923 format('+',2x,'Added by gap filling of flows',16x,10A10)
*     ==========================================================================
*     loads added by gap filling for quality -----------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propqrl(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propqrl(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8224)(Tenchars1(idet), idet=1,ndet)
 8224 format('+',2x,'Added by gap filling of quality',14x,10A10)
*     ==========================================================================

*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propeff2(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff2 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
*	if ( kp .eq. 1 ) then
*     write(27,8363)(Tenchars1(idet), idet=1,ndet) 
*8363 format(5X,'Point discharges (3, 12 and 5)',15X,10A10)
*	if ( kp .eq. 1 ) write(27,4826)
*     endif
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propeff3(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff3 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8303)(Tenchars1(idet), idet=1,ndet)
 8303 format('+',2x,'Sewage treatment works (3)',19X,10A10)
      write(01,8848)(Tenchars1(idet), idet=1,ndet)
 8848 format('Sewage treatment works (3)',14X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propeff12(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff12 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8312)(Tenchars1(idet), idet=1,ndet)
 8312 format('+',2x,'Intermittent discharges (12)',17X,10A10)
      write(01,8342)(Tenchars1(idet), idet=1,ndet)
 8342 format('Intermittent discharges (12)',12X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propeff5(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff5 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8305)(Tenchars1(idet), idet=1,ndet)
 8305 format('+',2x,'Industrial discharges (5)',20x,10A10)
      write(01,8345)(Tenchars1(idet), idet=1,ndet)
 8345 format('Industrial discharges (5)',15x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propeff39(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff39 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8395)(Tenchars1(idet), idet=1,ndet)
 8395 format('+',2x,'Mine waters (39)         ',20X,10A10)
      write(01,8895)(Tenchars1(idet), idet=1,ndet)
 8895 format('Mine waters (39)         ',15x,10A10)
	write(27,4826)
	write(01,4826)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop13(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop13 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8366)(Tenchars1(idet), idet=1,ndet)
 8366 format('+',2x,'River-based diffuse pollution (13)',11X,10A10)
      write(01,8866)(Tenchars1(idet), idet=1,ndet)
 8866 format('River-based diffuse pollution (13)',6X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop15(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop15 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,2366)(Tenchars1(idet), idet=1,ndet)
 2366 format('+',2x,'Effluent-based diffuse pollution (15)',8X,10A10)
      write(01,2866)(Tenchars1(idet), idet=1,ndet)
 2866 format('Effluent-based diffuse pollution (15)',3X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop42(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop42 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8966)(Tenchars1(idet), idet=1,ndet)
 8966 format('+',2x,'Aggregate STWs (42)                  ',8X,10A10)
      write(01,8986)(Tenchars1(idet), idet=1,ndet)
 8986 format('Aggregate STWs (42)                  ',3X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop25(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop25 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8364)(Tenchars1(idet), idet=1,ndet)
 8364 format('+',2x,'Livestock (25)',31X,10A10)
      write(01,8864)(Tenchars1(idet), idet=1,ndet)
 8864 format('Livestock (25)',26X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop27(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop27 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8365)(Tenchars1(idet), idet=1,ndet)
 8365 format('+',2x,'Arable (27)',34X,10A10)
      write(01,8865)(Tenchars1(idet), idet=1,ndet)
 8865 format('Arable (27)',29X,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop29(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop29 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8367)(Tenchars1(idet), idet=1,ndet)
 8367 format('+',2x,'Highway runoff (29)',26x,10A10)
      write(01,8867)(Tenchars1(idet), idet=1,ndet)
 8867 format('Highway runoff (29)',21x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop31(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop31 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8368)(Tenchars1(idet), idet=1,ndet)
 8368 format('+',2x,'Urban runoff (31)',28x,10A10)
      write(01,8868)(Tenchars1(idet), idet=1,ndet)
 8868 format('Urban runoff (31)',23x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop33(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop33 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8369)(Tenchars1(idet), idet=1,ndet)
 8369 format('+',2x,'Atmospheric deposition (33)',18x,10A10)
      write(01,8869)(Tenchars1(idet), idet=1,ndet)
 8869 format('Atmospheric deposition (33)',13x,10A10)
      endif
*     ==========================================================================
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop35(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop35 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
 	if ( kp .eq. 1 ) then
      write(27,8069)(Tenchars1(idet), idet=1,ndet)
 8069 format('+',2x,'Natural background (35)',22x,10A10)
      write(01,8269)(Tenchars1(idet), idet=1,ndet)
 8269 format('Natural background (35)',17x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop46(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop46 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,4069)(Tenchars1(idet), idet=1,ndet)
 4069 format('+',2x,'Diffuse mines (46)',27x,10A10)
      write(01,4869)(Tenchars1(idet), idet=1,ndet)
 4869 format('Diffuse mines (46)',22x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop48(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop48 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,4669)(Tenchars1(idet), idet=1,ndet)
 4669 format('+',2x,'Birds, boats and angling (48)',16x,10A10)
      write(01,4899)(Tenchars1(idet), idet=1,ndet)
 4899 format('Birds, boats and angling (48)',11x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop37(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop37 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8370)(Tenchars1(idet), idet=1,ndet)
 8370 format('+',2x,'Septic tanks (37)',28x,10A10)
      write(01,8870)(Tenchars1(idet), idet=1,ndet)
 8870 format('Septic tanks (37)',23x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prop40(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop40 (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8270)(Tenchars1(idet), idet=1,ndet)
 8270 format('+',2x,'Aggregate CSOs (40)',26x,10A10)
      write(01,8880)(Tenchars1(idet), idet=1,ndet)
 8880 format('Aggregate CSOs (40)',21x,10A10)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propall(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propall (idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,4136)(Tenchars1(idet), idet=1,ndet)
 4136 format(150('-')/'TOTAL NET LOAD (%) ...',26x,10A10)
      write(01,4936)(Tenchars1(idet), idet=1,ndet)
 4936 format(150('-')/'TOTAL NET LOAD (%) ...',18x,10A10)
      write(01,4806)
      write(27,4806)
      endif
*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prolosses(idet)) .gt. 1.0e-10 ) kp = 1
      endif
      enddo
	if ( kp .eq. 1 ) write(27,4106)
 4106 format(150('=')/'Percentages of the total load that have ',
     &'been ...'/150('='))
*     ==========================================================================
*     loads removed by abstractions --------------------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propabs(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propabs(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8246)(Tenchars1(idet), idet=1,ndet)
*     =========================================================================
*     loads removed by natural purification ------------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propNPD(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propNPD(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,1931)(Tenchars1(idet), idet=1,ndet)
*     =========================================================================
*     loads removed by gap filling for flows ----------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propfra(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propfra(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8913)(Tenchars1(idet), idet=1,ndet)
*     ==========================================================================
*     loads removed by gap filling for quality --------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propqra(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propqra(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8964)(Tenchars1(idet), idet=1,ndet)
	if ( kp .eq. 1 ) write(27,4806)
 4806 format(150('='))
*     ==========================================================================
*     total losses of load -----------------------------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (prolosses(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prolosses(idet))
	Tenchars1(idet) = valchars10
      endif
      enddo
	if ( kp .eq. 1 ) then
      write(27,8914)(Tenchars1(idet), idet=1,ndet)
 8914 format(150('-')/'TOTAL % LOSSES ',33x,10a10)
	write(27,4806)
      endif
*     =========================================================================

*     ==========================================================================
	kp = 0
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (proadds(idet)) .gt. 1.0e-10 ) kp = 1
      endif
      enddo
*	if ( kp .eq. 1 ) write(27,4896)
 4896 format(150('=')/'Percentages of the total load that have ',
     &'been'/150('='))
*     =========================================================================
*     total loads added by gap filling for flows ------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propfrl(idet)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars1(idet),7831)propfrl(idet)
 7831 format(f10.4)
      endif
      enddo
*	if ( kp .eq. 1 ) write(27,8923)(Tenchars1(idet), idet=1,ndet)
*     ==========================================================================
*     loads added by gap filling for quality -----------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (propqrl(idet)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars1(idet),7831)propqrl(idet)
      endif
      enddo
*	if ( kp .eq. 1 ) write(27,8224)(Tenchars1(idet), idet=1,ndet)
*     ==========================================================================
*     ==========================================================================
*     total additions of load --------------------------------------------------
	kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
	if ( abs (proadds(idet)) .gt. 1.0e-10 ) kp = 1
	write(Tenchars1(idet),7831)proadds(idet)
      endif
      enddo
	if ( kp .eq. 1 ) then
*     write(27,8974)(Tenchars1(idet), idet=1,ndet) 
 8974 format(150('-')/'TOTAL % ADDITIONS ',32x,10a10)
*	write(27,4806)
      endif
*     =========================================================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      write(27,4886)
 4886 format('Total added loads (without allowing for ',
     &'any losses)'/150('='))
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from headwaters and tributaries etc --------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TRLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TRLODE1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,3930)(Tenchars1(jdet), jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from Reach-type diffuse sources ------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TDDLOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TDDLOAD1(jdet,i13))
      endif
      enddo
 	if ( kp .eq. 1 ) write(27,4930)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads added by Natural Purification --------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TNLOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TNLOADUP1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4931)(Tenchars1(jdet),jdet=1,ndet)
 4931 format(5X,'Added by natural purification',16X,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river flows -------------------- 
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TILOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TILOADUP1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,2943)(Tenchars1(jdet), jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river quality ------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TALOADUP1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TALOADUP1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,2944)(Tenchars1(jdet), jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by discharges of effluent (types 3, 12 and 5) -----
      kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TELODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TELODE1(jdet,i13))
      endif
      enddo
*	if ( kp .eq. 1 ) write(27,8983)(Tenchars1(jdet), jdet=1,ndet)
 8983 format('-',2x,'Total from discharges (types 3, 12 and 5)',
     &4x,10A10)
*	if ( kp .eq. 1 ) write(27,4826)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from intermittent discharges of sewage effluents (3) ---------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T03LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T03LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,2733)(Tenchars1(jdet),jdet=1,ndet)
 2733 format('+',2x,'Total from sewage effluents (3)',14x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from intermittent discharges of sewage (12) ------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T12LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T12LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,2739)(Tenchars1(jdet),jdet=1,ndet)
 2739 format('+',2x,'Intermittent discharges of sewage (12)',7x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from industrial discharges (5) -------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T05LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T05LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,1933)(Tenchars1(jdet),jdet=1,ndet)
 1933 format('+',2x,'Total from industrial discharges (5)',9x,10A10)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from diffuse features: mine waters (39) ----------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T39LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T39LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,1993)(Tenchars1(jdet),jdet=1,ndet)
 1993 format('+',2x,'Total from mine waters (39)      ',12x,10A10)
	if ( kp .eq. 1 ) write(27,4826)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from diffuse features - river flow type (13) -----------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T13LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T13LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8113)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from diffuse features - discharge type (15) ------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T15LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T15LOAD1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8233)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from agricultural livestock (25) -----------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T25LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T25LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8125)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from agricultural arable (27) --------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T27LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T27LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8127)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from highway (non-urban) runoff (29) -------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T29LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T29LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8129)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from urban runoff (31) ---------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T31LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T31LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8131)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from direct atmospheric Ddeposition (33) ---------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T33LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T33LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8133)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from natural background (35) ---------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T35LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T35LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8135)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from aggregated STWs (42) ------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T42LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T42LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8142)(Tenchars1(jdet),jdet=1,ndet)
*     --------------------------------------------------------------------------
*     total total loads from diffuse mines (46) --------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T46LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T46LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8146)(Tenchars1(jdet),jdet=1,ndet)
*     --------------------------------------------------------------------------
*     total loads from birds, boats and angling (48) -----------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T48LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T48LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8148)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from septic tanks (37) ---------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T37LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T37LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8137)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from aggregated CSOs (40) ------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (T40LOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),T40LOAD1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8140)(Tenchars1(jdet),jdet=1,ndet)
	if ( kp .eq. 1 ) write(27,4806) ! total 
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TGLODE1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TGLODE1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,1593)(Tenchars1(jdet),jdet=1,ndet)
 1593 format(150('-')/'TOTAL ...',39x,10A10)
	if ( kp .eq. 1 ) write(27,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads from abstractions --------------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TBLOAD1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TBLOAD1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8246)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loads removed by natural purification ------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TNLOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TNLOADDN1(jdet,i13))
      endif
      enddo
      if ( kp .eq. 1 ) write(27,1931)(Tenchars1(jdet),jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river flows -------------------- 
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TILOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TILOADDN1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8913)(Tenchars1(jdet), jdet=1,ndet)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total loads introduced by gap filling for river quality ------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TALOADDN1(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TALOADDN1(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,8964)(Tenchars1(jdet),jdet=1,ndet)
	if ( kp .eq. 1 ) write(27,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     total losses of load -----------------------------------------------------
	kp = 0
      do jdet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (TLOSSES(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout(Tenchars1(jdet),TLOSSES(jdet,i13))
      endif
      enddo
	if ( kp .eq. 1 ) write(27,7964)(Tenchars1(jdet), jdet=1,ndet)
	if ( kp .eq. 1 ) write(27,4806)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	return
	end


      subroutine proportion of effluent (iplace)
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

	if ( iplace .ne. 4 ) then
	rnamex = rname (IREACH)
	else
	rnamex = 'Upstream of works'
	endif

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      propeff2 (idet) = 0.0 ! initialise the proportion of effluent ------------
	if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = '    0.0000'
	if ( TELODE2 (idet,i13) .gt. 1.0e-10 ) then
	if ( TGLODE2 (idet,i13) .gt. 1.0e-10 ) then
	propeff2 (idet) = 100.0 * TELODE2(idet,i13) / TGLODE2(idet,i13)
	write(Tenchars1(idet),1) propeff2 (idet)
    1 format(F10.4)
	endif
	endif

	endif
      enddo

*     write information to the apportionment file for this feature -------------
*     if ( ical13 .eq. 0 ) then
*     if ( iplace .ne. 4 ) then
*     write(40,3)Uname(feeture),(Tenchars1(jdet), jdet=1,ndet)
*   3 format(a40,10A10)
*     endif
*     endif

      jxtype = JT(feeture)
	if ( JT(feeture) .ne. 10 ) then
	unamex = uname(feeture)
	else
	unamex = 'Start of Reach'
      jxtype = 111
	endif

      if ( ical .gt. 3 .and. ical .eq. 0 ) then
      do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
      write(110+idet,9099)GIScode(feeture),unamex,rnamex,dname(idet),
     &propeff2(idet),jxtype
 9099 format(a40,',',a40,',',a20,',','Annual % load from all effluents',
     &',',a11,1(',',1pe11.4),',,,,,,,,,,,,,',i4)
      endif
      enddo
      endif ! if ( ical .gt. 3 .and. ical .eq. 0 )

      return
	end

*     write the proportions of effluent at the end of the reach ----------------
      subroutine proportion of effluent end 
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'

*     initialise and calculate the proportion of effluent ----------------------
      propeff2 (idet) = 0.0
	if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = '    0.0000'
	if ( TELODE2 (idet,i13) .gt. 1.0e-10 ) then
	if ( TGLODE2 (idet,i13) .gt. 1.0e-10 ) then
	propeff2 (idet) = 100.0 * TELODE2(idet,i13) / TGLODE2(idet,i13)
	write(Tenchars1(idet),1) propeff2 (idet)
    1 format(F10.4)
	endif
	endif
	endif
      enddo

*     write information to the apportionment file for this feature -------------
      if ( ical13 .eq. 0 ) then
*     write(40,3)RNAME(IREACH),(Tenchars1(jdet), jdet=1,ndet)
*   3 format('End of reach    ',a16,8x,10A10)
*     write(40,6344)
*6344 format(150('-'))
      do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
      write(110+idet,9099)GIScode(feeture),rname(ireach),dname(idet),
     &propeff2(idet),999
 9099 format(a40,',','End of reach',',',a16,',',
     &'Annual % load from all effluents',',',a11,1(',',1pe11.4),
     &',,,,,,,,,,,,,',i4)
      endif
      enddo
      endif

      return
	end

      subroutine proportion of effluent at model end 
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do idet = 1, MP10
      Tenchars1(idet) = '          '
	enddo

      do idet = 1, ndet
      Tenchars1(idet) = '      ----'
      propeff2 (idet) = 0.0 ! initialise proportion of effluent ----------------
	if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = '    0.0000'
	if ( TELODE2 (idet,i13) .gt. 1.0e-10 ) then
	if ( TGLODE2 (idet,i13) .gt. 1.0e-10 ) then
	propeff2 (idet) = 100.0 * TELODE2(idet,i13) / TGLODE2(idet,i13)
	write(Tenchars1(idet),1) propeff2 (idet)
    1 format(F10.4)
	endif
	endif

	endif
      enddo

      return
	end

*     write the proportions of effluent at the start of the reach --------------
      subroutine proportion of effluent start 
      include 'COM.FOR'
      character *10 Tenchars1(MP10)

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      propeff2 (idet) = 0.0 ! initialise proportion of effluent
	if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = '    0.0000'
	if ( TELODE2 (idet,i13) .gt. 1.0e-10 ) then
	if ( TGLODE2 (idet,i13) .gt. 1.0e-10 ) then
      propeff2 (idet) = 100.0 * TELODE2(idet,i13) / TGLODE2(idet,i13)
  	write(Tenchars1(idet),1) propeff2 (idet)
    1 format(F10.4)
	endif
	endif
	endif ! if ( QTYPE (idet) .ne. 4 )
      enddo

*     write information to the apportionment file for this feature -------------
      if ( ical13 .eq. 0 ) then
      do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
      write(110+idet,9099)GIScode(feeture),rname(ireach),dname(idet), ! 22222222
     &propeff2(idet),111
 9099 format(a40,',','Start of Reach',',',a16,',',
     &'Annual % load from all effluents',',',a11,1(',',1pe11.4),
     &',,,,,,,,,,,,,',i4)
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo
      endif ! if ( ical13 .eq. 0 )

      return
	end

      subroutine sort out the concentrations and loads
      include 'COM.FOR'
      character *121 LINE
      character * 7 A1
      character *10 BB1(MP10)
      character * 6 cptest

*     write out the change in percentile caused by gap filling -----------------
*     initialise the arrays that will hold the values --------------------------
      do jdet = 1,MP10
      Sevenchars(jdet) = '      -'
      B44(jdet) = '      -'
      enddo

*     write out the percentage changes in quality brought about by gap filling -
      do jdet = 1,ndet
      if ( QTYPE (jdet) .ne. 4 .and. QNAT(jdet) .ne. -99 ) then
      write(Sevenchars(jdet),7535)QNAT(jdet)
 7535 format(I7)
      endif
      enddo
	if ( iscreen .lt. 3) then
      if ( ical .eq. 3 .and. JQCAL(feeture) .ne. 0 ) then
	write( *,1751)(Sevenchars(jdet),jdet=1,ndet)
 1751 format(5X,'% change in mean quality',21x,10A7,')')
	write(09,1751)(Sevenchars(jdet),jdet=1,ndet)
	write(33,1751)(Sevenchars(jdet),jdet=1,ndet)
	endif
	endif       

      call set up output of calculated means or percentiles
      call set up flowa
      call set up flow95

*     write out the calculated quality at this point ---------------------------
      if ( output mode .ne. 1 ) then
      write(LINE,9495)UNAME(feeture),Length of main river,
     &Flowchars(2),(Sevenchars(jdet),jdet=1,ndet)
     
      read(LINE(6:11),'(a6)')cptest	

*     write out the quality at this point --------------------------------------
      if ( iscreen .eq. 0 .and. cptest.ne.'  ....' ) then
      if ( feetcount .gt. 500 ) then
      if ( JT (feeture) .ne. 6  .and. JT (feeture) .ne. 16 .and.
     &     JT (feeture) .ne. 26 .and. JT (feeture) .ne. 28 .and.
     &     JT (feeture) .ne. 30 .and. JT (feeture) .ne. 32 .and.
     &     JT (feeture) .ne. 34 .and. JT (feeture) .ne. 36 .and.
     &     JT (feeture) .ne. 47 .and. JT (feeture) .ne. 49 .and.
     &     JT (feeture) .ne. 38 .and.
     &     JT (feeture) .ne. 41 .and. JT (feeture) .ne. 43 ) then
      write( *,7511)LINE
      endif
      else
      write( *,7511)LINE
      endif
      endif
      write(09,7511)LINE ! feature - calculated quality
      write(33,7511)LINE ! feature - calculated quality
 7511 format(A121)

*     exclude writing quality in flow gap filling runs -------------------------
      if ( ical .gt. 2 .or. ical .eq. 0 ) then

*     write for the monitoring points ------------------------------------------
      if ( JT(feeture) .eq. 1 ) then

      call set up output of calculated means or percentiles

      if ( output mode .eq. 1 ) then
      write(LINE,9295)(Sevenchars(jdet),jdet=1,ndet)
 9295 format(5X,'(calculated mean quality        ',13x,10A7,')')
      write(LINE,9195)(B44(jdet),jdet=1,ndet)
      if ( iscreen .lt. 3 ) write( *,7511)LINE
      write(09,7511)LINE ! feature - calculated mean quality
      write(33,7511)LINE ! feature - calculated mean quality
      endif

      if ( output mode .eq. 2 ) then
      write(LINE,9395)(Sevenchars(jdet),jdet=1,ndet)
 9395 format(5X,'(calculated 90 and 10-percentiles',12x,10A7,')')
      write(LINE,9195)(B44(jdet),jdet=1,ndet)
 9195 format(5X,'(calculated 95 and 5-percentiles',13x,10A7,')')
      if ( iscreen .lt. 3 ) write( *,7511)LINE
      write(09,7511)LINE ! feature - calculated percentile quality
      write(33,7511)LINE ! feature - calculated percentile quality
      endif

      if ( output mode .eq. 3 ) then
      write(LINE,9195)(B44(jdet),jdet=1,ndet)
      if ( iscreen .lt. 3 ) write( *,7511)LINE
      write(09,7511)LINE ! feature - calculated percentile quality
      write(33,7511)LINE ! feature - calculated percentile quality
      endif
      endif
      endif
      endif

*     mean mode - write calculated values for this point -----------------------
      if ( output mode .eq. 1 ) then
      write(LINE,9495)UNAME(feeture),Length of main river,
     &flowchars(1),(Sevenchars(jdet),jdet=1,ndet)
 9495 format(5X,A31,1x,F6.1,11A7)
      if ( output mode .ne. 2 .and. output mode .ne. 3 ) then
      if ( iscreen .lt. 3 ) write( *,7511)LINE
      write(09,7511)LINE ! feature - calculated mean quality
      write(33,7511)LINE ! feature - calculated mean quality
      endif
      endif

*     write loads from effluent discharges -------------------------------------
*     apply to (3) (5) (12) (39) ------------------------------- write out loads
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. 
     &     JT(feeture) .eq. 39 ) then

      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      call set format for printout (BB1(jdet),eload(jdet,i13))
      endif
      enddo
*     if ( ical13 .eq. 0 ) then
*     if ( nobigout .le. 0 ) then
*	write(27,8932)(BB1(jdet),jdet=1,ndet)
*8932 format(72('- ')/
*    &5X,'Load introduced by the next discharge ',4X,10A10)
*     write(27,5281)
*5281 format(72('- '))
*     endif
*     endif
      endif

*     write loads from tributaries =============================================
      if ( JT(feeture) .eq. 2  .or. JT(feeture) .eq. 21 .or. 
     &     JT(feeture) .eq. 23 ) then
      kp = 1
      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
	if ( abs (eload(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout (BB1(jdet),eload(jdet,i13))
      endif
      enddo
*     if ( ical13 .eq. 0 ) then
*     if ( nobigout .le. 0 ) then ! Richard III RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
*	if ( kp .eq. 1 ) write(27,8332)(BB1(jdet),jdet=1,ndet)
*8332 format(5X,'Loads added by the next tributary ',11X,10A10)
*     if ( kp .eq. 1 ) write(27,6384)
*6384 format(72('- '))
*     endif ! RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
*     endif
      endif

*     ##########################################################################
*     write loads from river regulation point ==================================
      if ( JT(feeture) .eq. 9 ) then
      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      call set format for printout (BB1(jdet),eload(jdet,i13))
*     accumulate total loads from tributaries and headwaters ===================
	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1
      do J1 = 1, nx      
      TRLODE2(jdet,J1) = TRLODE2(jdet,J1) + eload(jdet,J1)
	enddo
      endif
      enddo
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(27,5932)(BB1(jdet),jdet=1,ndet)
 5932 format(5X,'(loads introduced by regulation)',13X,10A10)
      if ( nobigout .le. 0 ) write(27,6984)
 6984 format(150('-'))
      endif

      endif

*     write out the loads removed by abstractions =============================
      if ( JT(feeture) .eq. 7  .or. JT(feeture) .eq. 18 .or. 
     &     JT(feeture) .eq. 19 .or. JT(feeture) .eq. 20 .or.
     &     JT(feeture) .eq. 22 ) then

      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      call set format for printout(BB1(jdet),ABLOAD(jdet,i13))
	endif
      enddo
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(27,5952)(BB1(jdet),jdet=1,ndet)
 5952 format(5X,'Load removed by abstraction ...',14X,10A10)
      write(27,4952)
 4952 format(72('- '))
      endif
      endif
      endif
*     ==========================================================================
*     ##########################################################################

*     ##########################################################################
*     write effluent quality required to meet target river quality -------------
*     only relevant in Modes 7, 8 and 9 ----------------------------------------
      if ( ical .gt. 6 ) then
*     only needed for effluent discharges --------------------------------------
*     apply to (3) (5) (12) (39) -------------------setting limits on discharges
      if ( JT(feeture) .eq. 3 .or. JT(feeture) .eq. 5 .or.
     &     JT(feeture) .eq. 12 .or. JT(feeture) .eq. 39 ) then
*     number of data-set for river flow ----------------------------------------
      IF = JQ(feeture)
*     check flow is not trivial ------------------------------------------------
      if ( FE(IF,1) .gt. 0.0001 ) then
          
      call set up XECM 
      if ( iscreen .lt. 3 ) then
	call change colour of text (20) ! bright red
      write( *,1933)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      endif
      write(09,1933)(Sevenchars(jdet),jdet=1,ndet)
      write(33,1933)(Sevenchars(jdet),jdet=1,ndet)
 1933 format(5X,'Required mean effluent quality',15x,10A7,')')
      call set up XEFF 
      if ( iscreen .lt. 3 ) then
	call change colour of text (20) ! bright red
      write( *,1932)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      endif
      write(09,1932)(Sevenchars(jdet),jdet=1,ndet)
      write(33,1932)(Sevenchars(jdet),jdet=1,ndet)
 1932 format(5X,'Required 95%-tile effluent quality',11x,10A7,')')
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 ) -----------------------
      endif ! if ( FE(IF,1) .gt. 0.0001 ) --------------------------------------
      endif
*     ##########################################################################

*     ##########################################################################
*     write out the observed river quality -------------------------------------
      if ( ICAL .le. 09 .and. ICAL .ne. 1 ) then
      if ( JT(feeture) .eq. 1 ) then
*     indicator that there are too many monitoring stations --------------------
*     if ( Imm .eq. 0 ) then
      do jdet = 1, MP10
      Sevenchars(jdet) = '      -'
      enddo
*     write out observed annual means for gap filling points -------------------
      if ( JQCAL(feeture) .ne. 0 ) then
	call change colour of text (16) ! dark grey
      if ( output mode .eq. 1 ) then
      call set up xA
      if ( iscreen .lt. 3 ) write( *,9682)(Sevenchars(jdet),jdet=1,ndet)
 9682 format(5X,'Observed mean values ',24X,10A7)
      write(09,9632)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9632)(Sevenchars(jdet),jdet=1,ndet)
 9632 format(5X,'... observed mean values ',20X,10A7)
	endif !  if ( output mode .eq. 1 )
*     write out observed 95 and 5 percentiles ----------------------------------
      call set up x95
      if ( iscreen .lt. 3 ) write( *,9832)(Sevenchars(jdet),jdet=1,ndet)
 9832 format(5X,'Observed 95 or 5-percentiles ',16X,10A7)
      write(09,9732)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9732)(Sevenchars(jdet),jdet=1,ndet)
 9732 format(5X,'... observed 95 or 5-percentiles ',12X,10A7)
*     90 and 10 percentiles ----------------------------------------------------
      if ( output mode .eq. 2 ) then
      call set up x90
      if ( iscreen .lt. 3 ) write( *,9432)(Sevenchars(jdet),jdet=1,ndet)
      write(09,9432)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9432)(Sevenchars(jdet),jdet=1,ndet)
 9432 format(5X,'... observed 90 or 10-percentiles',12X,10A7)
	endif
*     99 and 1 percentiles -----------------------------------------------------
      if ( output mode .eq. 3 ) then
      call set up x99
      if ( iscreen .lt. 3 ) write( *,9982)(Sevenchars(jdet),jdet=1,ndet)
      write(09,9982)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9982)(Sevenchars(jdet),jdet=1,ndet)
 9982 format(5X,'... observed 99 or 1-percentiles ',12X,10A7)
      endif !  if ( output mode .eq. 3 )
*     --------------------------------------------------------------------------
      call set screen text colour
	endif !  if ( JQCAL(feeture) .ne. 0 )
      endif !  if ( JT(feeture) .eq. 1 )
      endif !  if ( ICAL .lt. 09 .and. ICAL .ne. 1 )
*     ##########################################################################

*     write out the measured 90-percentiles of river quality -------------------
*     needed only in Modes 0, 2, 3 and 4 ---------------------------------------
      if ( ical .eq. 3 .and. output mode .eq. 2 ) then
      if ( JT(feeture) .eq. 1 ) then
*     indicator that there are too many monitoring stations --------------------
      if ( Imm .eq. 0 ) then
      call set up x90
	call change colour of text (16) ! dark grey
      if ( iscreen .lt. 3 ) write( *,9332)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      write(09,9332)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9332)(Sevenchars(jdet),jdet=1,ndet)
 9332 format(5X,'... measured 90 and 10-percentiles    ',7X,10A7)
      endif
      endif
      endif

*     write out the measured 99-percentiles of river quality -------------------
*     needed only in Modes 0, 2, 3 and 4 ---------------------------------------
      if ( ( ICAL .eq. 3 .or. ICAL .eq. 4 ) 
     &.and. output mode .eq. 3 ) then
      if ( JT(feeture) .eq. 1 ) then
*     indicator that there are too many monitoring stations --------------------
      if ( Imm .eq. 0 ) then
      call set up x99
	call change colour of text (16) ! dark grey
      if ( iscreen .lt. 3 ) write( *,9939)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      write(09,9939)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9939)(Sevenchars(jdet),jdet=1,ndet)
 9939 format(5X,'... measured 99 and 1-percentiles     ',7X,10A7)
      endif
      endif
      endif

*     write out the measured river flow ----------------------------------------
      if ( ICAL .le. 4. and. JT(feeture) .eq. 4 ) then
      jkf = JFcal(KFEAT)
      A1 = '      -'
      if (jkf .gt. 0 ) then
      X(1) = F(jkf,2)
      if ( output mode .eq. 1 ) X(1) = F(jkf,1)
      if ( pdrf(jkf) .eq. -1 ) goto 8356
      if ( X(1) .gt. 9.99) then
      write(A1,7931) X(1)
 7931 format(F7.1)
      else
      if ( X(1) .gt. 0.999) then               
      write(A1,7933) X(1)
 7933 format(F7.2)
      else
      if ( X(1) .gt. 0.0999) then               
      write(A1,7993) X(1)
 7993 format(3x,F4.3)
      else
      if ( X(jdet) .gt. 0.00999) then               
      write(A1,7934) X(1)
 7934 format(2x,F5.4)
      else
      write(A1,7976) X(1)
 7976 format(F7.4)
      endif
      endif
      endif
      endif
      if ( x(1) .gt. 999.9) then
      write(A1,8081) int(x(1))
 8081 format(i7)
      endif
	call change colour of text (16) ! dark grey
	if ( iscreen .lt. 3 ) write( *,9878)A1
 9878 format(5X,'Measured river flow',19x,A7)
      call set screen text colour
	write(09,9878)A1

      else ! when ( jfk .le. 0 )
      if ( ical .eq. 0 ) then
      write(09,5632)A1
      write(33,5632)A1
 5632 format(5X,'... measured river flow',15x,A7)
      endif ! if ( ical .eq. 0 )
      endif ! if ( jfk .gt. 0 ) 
      endif ! if ( ical .le. 4. and. JT(feeture) .eq. 4 )
 8356 continue

*     write out the loads at this feature --------------------------------------
      if ( ical13 .eq. 0 ) then
      if ( JT(KFEAT) .ne. 248 ) then ! Richard III
      call write out the river loads
      endif
      endif
      
      return
      end

      subroutine set up output of calculated means or percentiles
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      B44(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if (C(jdet,NMORQ) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) C(jdet,NMORQ)
    1 format(1pe7.0)
      goto 99
	endif

*     less than 0.000000002 ----------------------------------------------------
      if (C(jdet,NMORQ) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (C(jdet,NMORQ))
      goto 99
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (C(jdet,NMORQ) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (C(jdet,NMORQ))
    2 format(i7)
      goto 99
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if (C(jdet,NMORQ) .gt. 9.99) then
      write(Sevenchars(jdet),3) C(jdet,NMORQ)
    3 format(F7.1)
      goto 99
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if (C(jdet,NMORQ) .gt. 0.999) then       
      write(Sevenchars(jdet),4) C(jdet,NMORQ)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if (C(jdet,NMORQ) .gt. 0.0999) then       
      write(Sevenchars(jdet),5) C(jdet,NMORQ)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if (C(jdet,NMORQ) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) C(jdet,NMORQ)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if (C(jdet,NMORQ) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) C(jdet,NMORQ)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if (C(jdet,NMORQ) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) C(jdet,NMORQ)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) C(jdet,NMORQ)
    9 format(1x,1pe6.0)
   99 continue 

*     greater than 1,000,000 ---------------------------------------------------
      if (C(jdet,3) .gt. 999999.45 ) then
      write(B44(jdet),1) C(jdet,3)
      goto 98
	endif

*     less than 0.000000002 ---------------------------------------------------
      if (C(jdet,3) .lt. 0.000000002) then
      write(B44(jdet),2) int (C(jdet,3))
      goto 98
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (C(jdet,3) .gt. 9999.45 ) then
      write(B44(jdet),2) int (C(jdet,3))
      goto 98
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if (C(jdet,3) .gt. 9.99) then
      write(B44(jdet),3) C(jdet,3)
      goto 98
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if (C(jdet,3) .gt. 0.999) then       
      write(B44(jdet),4) C(jdet,3)
      goto 98
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if (C(jdet,3) .gt. 0.0999) then       
      write(B44(jdet),5) C(jdet,3)
      goto 98
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if (C(jdet,3) .gt. 0.00999) then       
      write(B44(jdet),6) C(jdet,3)
      goto 98
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if (C(jdet,3) .gt. 0.000999) then               
      write(B44(jdet),7) C(jdet,3)
      goto 98
      endif

*     less than 0.001 and greater than 0.00001 ---------------------------------
	if (C(jdet,3) .gt. 0.00000999) then               
      write(B44(jdet),8) C(jdet,3)
      goto 98
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(B44(jdet),9) C(jdet,3)
   98 continue 

      endif
      enddo
      return
	end

      subroutine set up x95
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

      jkl = JQ(feeture)
      if ( jkl .gt. 0 ) then
      if ( PDRC(jkl,1) .ne. -1 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if ( X95(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) X95(jdet)
    1 format(1pe7.0)
      goto 99
	endif

*     less than 0.000000002 ----------------------------------------------------
      if ( X95(jdet) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (X95(jdet))
      goto 99
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( X95(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (X95(jdet))
    2 format(i7)
      goto 99
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( X95(jdet) .gt. 9.99 ) then
      write(Sevenchars(jdet),3) X95(jdet)
    3 format(F7.1)
      goto 99
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( X95(jdet) .gt. 0.999 ) then       
      write(Sevenchars(jdet),4) X95(jdet)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( X95(jdet) .gt. 0.0999 ) then       
      write(Sevenchars(jdet),5) X95(jdet)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( X95(jdet) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) X95(jdet)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( X95(jdet) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) X95(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( X95(jdet) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) X95(jdet)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) X95(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
	end

      subroutine set up xa
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

      jkl = JQ(feeture)
      if ( jkl .gt. 0 ) then
      if ( PDRC(jkl,1) .ne. -1 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if ( Xa(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) Xa(jdet)
    1 format(1pe7.0)
      goto 99
	endif

*     less than 0.000000002 ----------------------------------------------------
      if ( Xa(jdet) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (Xa(jdet))
      goto 99
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( Xa(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (Xa(jdet))
    2 format(i7)
      goto 99
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( Xa(jdet) .gt. 9.99) then
      write(Sevenchars(jdet),3) Xa(jdet)
    3 format(F7.1)
      goto 99
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( Xa(jdet) .gt. 0.999) then       
      write(Sevenchars(jdet),4) Xa(jdet)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( Xa(jdet) .gt. 0.0999) then       
      write(Sevenchars(jdet),5) Xa(jdet)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( Xa(jdet) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) Xa(jdet)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( Xa(jdet) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) Xa(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( Xa(jdet) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) Xa(jdet)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) Xa(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
	end

      subroutine set up x99
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

      jkl = JQ(feeture)
      if ( jkl .gt. 0 ) then
      if ( PDRC(jkl,1) .ne. -1 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if ( x99(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) x99(jdet)
    1 format(1pe7.0)
      goto 99
	endif

*     less than 0.000000002 ----------------------------------------------------
      if ( x99(jdet) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (x99(jdet))
      goto 99
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( x99(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (x99(jdet))
    2 format(i7)
      goto 99
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( x99(jdet) .gt. 9.99) then
      write(Sevenchars(jdet),3) x99(jdet)
    3 format(F7.1)
      goto 99
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( x99(jdet) .gt. 0.999) then       
      write(Sevenchars(jdet),4) x99(jdet)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( x99(jdet) .gt. 0.0999) then       
      write(Sevenchars(jdet),5) x99(jdet)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( x99(jdet) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) x99(jdet)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( x99(jdet) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) x99(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( x99(jdet) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) x99(jdet)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) x99(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
	end

      subroutine set up x90
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

      jkl = JQ(feeture)
      if ( jkl .gt. 0 ) then
      if ( PDRC(jkl,1) .ne. -1 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if ( x90(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) x90(jdet)
    1 format(1pe7.0)
      goto 99
	endif

*     less than 0.000000002 ----------------------------------------------------
      if ( x90(jdet) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (x90(jdet))
      goto 99
	endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( x90(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (x90(jdet))
    2 format(i7)
      goto 99
	endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( x90(jdet) .gt. 9.99) then
      write(Sevenchars(jdet),3) x90(jdet)
    3 format(F7.1)
      goto 99
	endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( x90(jdet) .gt. 0.999) then       
      write(Sevenchars(jdet),4) x90(jdet)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( x90(jdet) .gt. 0.0999) then       
      write(Sevenchars(jdet),5) x90(jdet)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( x90(jdet) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) x90(jdet)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( x90(jdet) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) x90(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( x90(jdet) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) x90(jdet)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) x90(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
	end

      subroutine set up XEFF
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then  
      if ( pollution data(IF,jdet,1) .gt. 1.0E-08 ) then

*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(feeture)
      do JP = 1, NDET
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
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET

      if ( targit .gt. 0.000001 ) then 

*     greater than 1,000,000 ---------------------------------------------------
      if ( XEFF(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) XEFF(jdet)
    1 format(1pe7.0)
      goto 99
	endif
*     less than 0.000000002 ----------------------------------------------------
      if ( XEFF(jdet) .lt. 0.000000002 ) then
      write(Sevenchars(jdet),2) int (XEFF(jdet))
      goto 99
	endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( XEFF(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (XEFF(jdet))
    2 format(i7)
      goto 99
	endif
*     less than 10,000 and greater than 100 ------------------------------------
      if ( XEFF(jdet) .gt. 9.99 ) then
      write(Sevenchars(jdet),3) XEFF(jdet)
    3 format(F7.1)
      goto 99
	endif
*     less than 100 and greater than 1 -----------------------------------------
      if ( XEFF(jdet) .gt. 0.999 ) then       
      write(Sevenchars(jdet),4) XEFF(jdet)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if ( XEFF(jdet) .gt. 0.0999 ) then       
      write(Sevenchars(jdet),5) XEFF(jdet)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( XEFF(jdet) .gt. 0.00999 ) then       
      write(Sevenchars(jdet),6) XEFF(jdet)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( XEFF(jdet) .gt. 0.000999 ) then               
      write(Sevenchars(jdet),7) XEFF(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( XEFF(jdet) .gt. 0.00000999 ) then               
      write(Sevenchars(jdet),8) XEFF(jdet)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) XEFF(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif ! if ( targit .gt. 0.000001 )
      endif
      endif
      
      enddo
      return
      end


      subroutine set up XECM
      include 'COM.FOR'

      do jdet = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then  
      if ( pollution data(IF,jdet,1) .gt. 1.0E-08 ) then
          
*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
      IQSfeet = IFRQS(feeture)
      do JP = 1, NDET
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
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
	endif ! if ( QTYPE (JP) .ne. 4 )
      enddo ! do JP = 1, NDET

      if ( targit .gt. 0.000001 ) then 

*     greater than 1,000,000 ---------------------------------------------------
      if ( XECM(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) XECM(jdet)
    1 format(1pe7.0)
      goto 99
	endif
*     less than 0.000000002 ----------------------------------------------------
      if ( XECM(jdet) .lt. 0.000000002 ) then
      write(Sevenchars(jdet),2) int (XECM(jdet))
      goto 99
	endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( XECM(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (XECM(jdet))
    2 format(i7)
      goto 99
	endif
*     less than 10,000 and greater than 100 ------------------------------------
      if ( XECM(jdet) .gt. 9.99 ) then
      write(Sevenchars(jdet),3) XECM(jdet)
    3 format(F7.1)
      goto 99
	endif
*     less than 100 and greater than 1 -----------------------------------------
      if ( XECM(jdet) .gt. 0.999 ) then       
      write(Sevenchars(jdet),4) XECM(jdet)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if ( XECM(jdet) .gt. 0.0999 ) then       
      write(Sevenchars(jdet),5) XECM(jdet)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( XECM(jdet) .gt. 0.00999 ) then       
      write(Sevenchars(jdet),6) XECM(jdet)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( XECM(jdet) .gt. 0.000999 ) then               
      write(Sevenchars(jdet),7) XECM(jdet)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( XECM(jdet) .gt. 0.00000999 ) then               
      write(Sevenchars(jdet),8) XECM(jdet)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) XECM(jdet)
    9 format(1x,1pe6.0)
   99 continue 

      endif ! if ( targit .gt. 0.000001 )
      endif
      endif
      
      enddo
      return
	end



      subroutine set up flowa
      include 'COM.FOR'

      flowchars(1) = '      -' ! initialise
      if ( JT(feeture) .eq. 7 ) return
      
*     greater than 1,000,000 ---------------------------------------------------
      if ( flow(1) .gt. 999999.45 ) then
      write(flowchars(1),1) flow(1)
    1 format(1pe7.0)
      goto 99
	endif
*     less than 0.000000002 ----------------------------------------------------
      if ( flow(1) .lt. 0.000000002) then
      write(flowchars(1),2) int (flow(1))
      goto 99
	endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (flow(1) .gt. 9999.45 ) then
      write(flowchars(1),2) int (flow(1))
    2 format(i7)
      goto 99
	endif
*     less than 10,000 and greater than 100 ------------------------------------
      if (flow(1) .gt. 9.99) then
      write(flowchars(1),3) flow(1)
    3 format(F7.1)
      goto 99
	endif
*     less than 100 and greater than 1 -----------------------------------------
      if (flow(1) .gt. 0.999) then       
      write(flowchars(1),4) flow(1)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if (flow(1) .gt. 0.0999) then       
      write(flowchars(1),5) flow(1)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
	if (flow(1) .gt. 0.00999) then       
      write(flowchars(1),6) flow(1)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
	if (flow(1) .gt. 0.000999) then               
      write(flowchars(1),7) flow(1)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if (flow(1) .gt. 0.00000999) then               
      write(flowchars(1),8) flow(1)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(flowchars(1),9) flow(1)
    9 format(1x,1pe6.0)
   99 continue 

      return
	end

      subroutine set up flow95
      include 'COM.FOR'

      flowchars(2) = '      -' ! initialise
      if ( JT(feeture) .eq. 7 ) return
*     greater than 1,000,000 ---------------------------------------------------
      if ( flow(2) .gt. 999999.45 ) then
      write(flowchars(2),1) flow(2)
    1 format(1pe7.0)
      goto 99
	endif
*     less than 0.000000002 ----------------------------------------------------
      if ( flow(2) .lt. 0.000000002 ) then
      write(flowchars(2),2) int (flow(2))
      goto 99
	endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( flow(2) .gt. 9999.45 ) then
      write(flowchars(2),2) int (flow(2))
    2 format(i7)
      goto 99
	endif
*     less than 10,000 and greater than 100 ------------------------------------
      if ( flow(2) .gt. 9.99 ) then
      write(flowchars(2),3) flow(2)
    3 format(F7.1)
      goto 99
	endif
*     less than 100 and greater than 1 -----------------------------------------
      if ( flow(2) .gt. 0.999 ) then       
      write(flowchars(2),4) flow(2)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if ( flow(2) .gt. 0.0999 ) then       
      write(flowchars(2),5) flow(2)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
	if ( flow(2) .gt. 0.00999 ) then       
      write(flowchars(2),6) flow(2)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
	if ( flow(2) .gt. 0.000999 ) then               
      write(flowchars(2),7) flow(2)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
	if ( flow(2) .gt. 0.00000999 ) then               
      write(flowchars(2),8) flow(2)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(flowchars(2),9) flow(2)
    9 format(1x,1pe6.0)
   99 continue 

      return
	end

      subroutine write preliminary headings for the reach
      include 'COM.FOR'

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      call sort format 2 (flow(1),flow(2))
      write(01,1003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit
 1003 format(///110('=')/'Calculation for the reach named: ',
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x,
     &' Flow at head of Reach:  Mean =',a10,1x,a4/30X,
     &'         95-percent exceedence =',a10,1x,a4/77('='))
      write(21,1003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit
*     ==========================================================================
      if ( ical13 .eq. 0 ) then
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(100+ic,1203)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH)
 1203 format(///110('=')/'Calculation for the reach named: ',
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x/110('='))
      endif
	enddo
	endif
      endif
	endif
*     ==========================================================================
      call set up the data for temperature 
      call set up the data for suspended solids 

*     identify any file with the non-parametric data ---------------------------
      if ( IF .gt. 0 ) then
      if ( PDRF(IF) .eq. 4 ) then
      do 1969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 1970
 1969 continue
	goto 1974
 1970 continue
      if ( ical13 .eq. 0 ) then
	if ( nobigout .le. 0 ) write(01,1965) flnamesmall(1,icod)
 1965 format(77('-')/
     &'The flow data for the head of this Reach were sampled from ',
     &'a non-parametric'/'distribution ... File: ',a64/77('-'))
      endif
 1974 continue
	endif
      endif

      return
	end
      
      subroutine set format for printout(tench,xnum)
      character *10 tench
      tench = '       ---'
	if ( iabs(int(xnum)) .gt. 999999999 ) then
	write(tench,10)xnum
   10 format(1pe10.3)
	return
      endif
	if ( abs(xnum) .gt. 999.9 ) then
	write(tench,9)int(xnum)
    9 format(I10)
	return
      endif
	if ( abs(xnum) .gt. 9.9 ) then
	write(tench,1)xnum
    1 format(F10.1)
	return
      endif
	if ( abs(xnum) .gt. 0.99 ) then
	write(tench,2)xnum
    2 format(F10.2)
	return
      endif
	if ( abs(xnum) .gt. 0.099 ) then
	write(tench,3)xnum
    3 format(F10.3)
	return
      endif
	if ( abs(xnum) .gt. 0.099 ) then
	write(tench,4)xnum
    4 format(F10.4)
	return
      endif
	if ( abs(xnum) .gt. 0.099 ) then
	write(tench,5)xnum
    5 format(F10.5)
      return
      endif
	if ( abs(xnum) .gt. 0.0099 ) then
	write(tench,6)xnum
    6 format(F10.6)
      return
      endif
	if ( abs(xnum) .lt. 0.0000001 ) then
	write(tench,7)xnum
    7 format(F10.1)
      return
      endif
 	write(tench,8)xnum
    8 format(F10.7)
      return
      end

      subroutine initialise the running totals of load
      include 'COM.FOR'

*     initialise the running totals of load ------------------------------------
      do ii = 1, ndet
      check total (ii) = 0.0

      do J1 = 1, N13
      
*     total loads --------------------------------------------------------------
      TGLODE2(ii,J1) = 0.0
      TGLODE1(ii,J1) = 0.0

*     from point discharges of effluent ----------------------------------------
      TELODE1 (ii,J1) = 0.0
      T03LOAD1(ii,J1) = 0.0
      T05LOAD1(ii,J1) = 0.0
      T12LOAD1(ii,J1) = 0.0
      T39LOAD1(ii,J1) = 0.0
*     net loads from discharges (3 12 5 and 39) -------------------------TELODE2
      TELODE2(ii,J1) = 0.0

      T03LOAD2(ii,J1) = 0.0
      T05LOAD2(ii,J1) = 0.0
      T12LOAD2(ii,J1) = 0.0
      T39LOAD2(ii,J1) = 0.0

*     net from boundaries and tributaries -------------------------------TRLODE2
      TRLODE2(ii,J1) = 0.0
*     total from boundaries and tributaries -----------------------------TRLODE1
      TRLODE1(ii,J1) = 0.0

*     total load introduced by natural purification ----------------------------
      TNLOADUP2(ii,J1) = 0.0
      TNLOADUP1(ii,J1) = 0.0

*     total load removed by natural purification -------------------------------
      TNLOADDN2(ii,J1) = 0.0
      TNLOADDN1(ii,J1) = 0.0

*     total load introduced to reaches by clean diffuse sources ----------------
      TDDLOAD2(ii,J1) = 0.0
      TDDLOAD1(ii,J1) = 0.0

*     total loads introduced by gap filling for river flows --------------------
      TILOADUP2(ii,J1) = 0.0
      TILOADUP1(ii,J1) = 0.0

*     total loads removed by gap filling for river flows -----------------------
      TILOADDN2(ii,J1) = 0.0

*     total load introduced by gap filling for river quality -------------------
      TALOADUP2(ii,J1) = 0.0
      TALOADUP1(ii,J1) = 0.0

*     total load removed by gap filling for river quality ----------------------
      TALOADDN2(ii,J1) = 0.0
      TALOADDN1(ii,J1) = 0.0

*     total loads from diffuse features (river flow type) ----------------------
      T13LOAD2(ii,J1) = 0.0
      T13LOAD1(ii,J1) = 0.0

*     total loads from diffuse features (discharge flow type) ------------------
      T15LOAD2(ii,J1) = 0.0
      T15LOAD1(ii,J1) = 0.0

*     total loads from livestock diffuse features (river flow type) ------------
      T25LOAD2(ii,J1) = 0.0
      T25LOAD1(ii,J1) = 0.0

*     total loads from arable diffuse features (river flow type) ---------------
      T27LOAD2(ii,J1) = 0.0
      T27LOAD1(ii,J1) = 0.0
      T29LOAD2(ii,J1) = 0.0
      T29LOAD1(ii,J1) = 0.0
      T31LOAD2(ii,J1) = 0.0
      T31LOAD1(ii,J1) = 0.0
      T33LOAD2(ii,J1) = 0.0
      T33LOAD1(ii,J1) = 0.0
      T35LOAD2(ii,J1) = 0.0
      T35LOAD1(ii,J1) = 0.0
      T37LOAD2(ii,J1) = 0.0
      T37LOAD1(ii,J1) = 0.0
      T40LOAD2(ii,J1) = 0.0
      T40LOAD1(ii,J1) = 0.0
      T42LOAD2(ii,J1) = 0.0
      T42LOAD1(ii,J1) = 0.0
      T46LOAD2(ii,J1) = 0.0
      T46LOAD1(ii,J1) = 0.0
      T48LOAD2(ii,J1) = 0.0
      T48LOAD1(ii,J1) = 0.0

*     total removed by abstractions --------------------------------------------
      TBLOAD2 (ii,J1) = 0.0
      TBLOAD1 (ii,J1) = 0.0
	enddo ! 1, N13
      enddo ! 1, ndet

*     initialise the values of the proportions of load -------------------------
      do idet = 1, ndet
      prop10    (idet) = 0.0
      propRT    (idet) = 0.0
      propNPU   (idet) = 0.0
      propeff2  (idet) = 0.0
      propeff3  (idet) = 0.0
      propeff12 (idet) = 0.0
      propeff5  (idet) = 0.0
      propeff39 (idet) = 0.0
      prop13    (idet) = 0.0
      prop15    (idet) = 0.0
      prop42    (idet) = 0.0
      prop25    (idet) = 0.0
      prop27    (idet) = 0.0
      prop29    (idet) = 0.0
      prop31    (idet) = 0.0
      prop33    (idet) = 0.0
      prop35    (idet) = 0.0
      prop46    (idet) = 0.0
      prop48    (idet) = 0.0
      prop37    (idet) = 0.0
      prop40    (idet) = 0.0
      propabs   (idet) = 0.0
      propNPD   (idet) = 0.0
      propfra   (idet) = 0.0
      propqra   (idet) = 0.0
      propfrl   (idet) = 0.0
      propqrl   (idet) = 0.0
      prolosses (idet) = 0.0
      proadds   (idet) = 0.0
      propall   (idet) = 0.0
      enddo

      return
      end
      
      subroutine initialise the loads for works and bodies
      include 'COM.FOR'

*     initialise the running totals of load ------------------------------------
      do ii = 1, ndet
      check total (ii) = 0.0

      do J1 = 1, N13
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments -------------------------------
	TWLOADS (ibodies,ii,J1) = 0.0 ! initialise the running totals of load
      do ip = 1, nprop
*     breakdown of monthly loads from upstream sub-catchments ------------------
	TWLOADSAPP (ibodies,ii,J1,ip) = 0.0 ! initialise the running totals 
      enddo ! 1, nprop

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do is = 1, NS ! 5555555555555555555555555555555555555555555555555555555555
      do ip = 1, nprop
      TDLOADshots(ibodies,ii,is,ip) = 0.0 ! 555555555555555555555555555555555555
      enddo
      enddo ! do is = 1, NS 5555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555

      enddo ! 1, NUW
	enddo ! 1, N13

      do iworks = 1, NUED
      TELOADAV (iworks,ii) = 0.0 ! monthly loads from upstream effluents
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      TELOADshots(iworks,ii,is) = 0.0 ! 3333333333333333333333333333333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
      enddo ! iworks = 1, NUED
      enddo ! 1, ndet
      
      do ibodies = 1, NUW
	TWlength (ibodies) = 0.0
      enddo

      return
	end

      subroutine initialise classification
      include 'COM.FOR'

      if ( No tables .le. 0 ) then
      if ( nobigout .le. 0 ) write(01,4590)
      if ( nobigout .le. 0 ) write(21,4590)
 4590 format(//77('=')/
     &'THE MAIN CALCULATIONS FOLLOW ...  '/77('='))
      if ( nobigout .le. 0 ) write(01,4891) RNAME(jreach(1))
      if ( nobigout .le. 0 ) write(21,4891) RNAME(jreach(1))
 4891 format('Calculations for the first reach ... ',A16/77('='))
      endif

      if ( JT(1) .ne. 10 .and. JT(1) .ne. 11 ) then
      write(01,1475) rname(ireach)
      write(21,1475) rname(ireach)
      write( *,1475) rname(ireach)
      write(09,1475) rname(ireach)
      write(33,1475) rname(ireach)
      write(72,1475) rname(ireach)
 1475 format(77('-')/'*** Missing Feature for Reach - ',a16/
     &'*** No flow and quality data for the Head of a Reach whose ',
     &'top forms an'/'*** upstream boundary of the catchment. The ',
     &'first feature on the Reach must'/'*** be Type 10 or ',
     &'Type 11 ... SIMCAT has stopped .... '/77('-'))
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      return
      else
      call stop
      endif
      endif

*     set the counter of the number of effluent discharges ---------------------
      kount works = 0
      need works = 0
      kill works = 0
*     set the counter of the number of sub-catchments --------------------------
      kount bodies = 0

*     initialise the starting values for tracking compliance with targets ------
*     these track for this particular model run (say within a batch) -----------
      Total RQS length 1 = 0.0 ! total river length with target river quality --
	Total length 2 = 0.0 ! total length with an objective
	Total length 00 = 0.0 ! total length of compliant river
      Total length 50 = 0.0 ! total failed with 50 per cent confidence
	Total length 95 = 0.0 ! total failed with 95 per cent confidence
	
	Total river length = 0.0 ! total length of river subject to calculations

*     and for the individual determinands --------------------------------------
      do idet = 1, ndet
      Total length dets 00  (idet) = 0.0
	Total length dets 50  (idet) = 0.0
	Total length dets 95  (idet) = 0.0
      enddo

*     and for the classifications ----------------------------------------------
      do iclass = 1, nclass
      do idet = 1, ndet
	total length in classes (iclass, idet) = 0.0
      enddo
	total length over all (iclass) = 0.0
      enddo

      return
	end

      subroutine identify virtual reaches
      include 'COM.FOR'
      character *07 virt
      virtualreach = 0
      virt = "Good   "
	Write(virt,2177)Rname(ireach)
 2177 format(a7)
      if (virt .eq. "virtual" ) Virtualreach = 1
      if (virt .eq. "Virtual" ) Virtualreach = 1
      if (virt .eq. "VIRTUAL" ) Virtualreach = 1
      return
	end
