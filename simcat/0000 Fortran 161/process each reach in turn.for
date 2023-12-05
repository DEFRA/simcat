*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....       
*     ==========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================
*     File: process each reach in turn.for ... 6306 lines ----------------------
*     --------------------------------------------------------------------------
*     This file directs the order of processing the Reaches that make up the ---
*     catchment model ... it chooses the next Reach that is to be processed ----
*     --------------------------------------------------------------------------
*     This file contains 50 subroutines (and calls others 265 times ) ----------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... start the main calculations
*     ...... list summary tables
*     ...... write headings at the start of the reach
*     ...... write data for graph plotting 2 (JU)
*     ...... process the end of the reach
*     ...... details upstream of feature
*     ...... write flow in river just upstream
*     ...... write out loads at the end of the reach
*     ...... write out the river loads
*     ...... write out the river loads after natural purification
*     ...... write out the river loads after diffuse sources
*     ...... write out the river loads before diffuse sources
*     ...... write diffuse effluent load
*     ...... write loads after gap filling of river flows 2
*     ...... write loads after gap filling of river flows 3
*     ...... write loads after gap filling of river quality A
*     ...... write loads after gap filling of river quality B
*     ...... prepare loads for print out (Tenchars1)
*     ...... format the loads for output 2 (Tenchars1)
*     ...... format the loads for output 3 (Tenchars1)
*     ...... format the loads for output 4 (Tenchars1) 
*     ...... outsum at end of run
*     ...... add up all the loads
*     ...... outsum at this feature
*     ...... outsum at start of reach 
*     ...... write loads at this point in the catchment (kdirect)
*     ...... proportion of effluent (iplace)
*     ...... proportion of effluent end 
*     ...... proportion of effluent at model end 
*     ...... proportion of effluent start 
*     ...... sort out the concentrations and loads
*     ...... set up output of calculated means or percentiles
*     ...... set up x95
*     ...... set up XA arrays (iz)
*     ...... set up x99
*     ...... set up x90
*     ...... set up xTARGETS (notargit) ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
*     ...... set up XEFF
*     ...... set up XECM
*     ...... set up flowa
*     ...... set up flow95
*     ...... write preliminary headings for the reach
*     ...... set format for printout(tench,xnum)
*     ...... set format for printout2 (ninth,xnum)
*     ...... initialise the running totals of load
*     ...... initialise the loads for works and bodies
*     ...... initialise classification
*     ...... identify virtual reaches
*     ...... set up statistics (jds,xxx)
*     --------------------------------------------------------------------------

      subroutine start the main calculations
      include 'COMMON DATA.FOR'

      IREACH = 0 ! initialise
      start reach = 0 ! initialise
      feeture = 0 ! initialise
      INEXT = 0
      nostruct = 0
      if ( munthly structure .eq. 2 ) munthly structure = 0

      call initialise the running totals of load ! start of calculation

      call initialise classification etc

      call initialise the loads for works and bodies
      
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
      
      lastREACH2 = lastREACH ! --------------------------------------------- 160
      lastREACH = IREACH ! ------------------------------------------------- 160

      call identify virtual reaches
 
      call reach ! process the reach -------------------------------------------

*     check if this is the final reach (kfinal will then be zero) --------------
      kfinal = iplan(ireach,1) + iplan(ireach,2) + iplan(ireach,3) 
      if ( kfinal .eq. 0 ) goto 89 ! finished the final reach ==================
*     --------------------------------------------------------------------------
     
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
*     prepare for the next reach -----------------------------------------------
*     test for a confluence of reaches -----------------------------------------
*     --------------------------------------------------------------------------
      if ( IPLAN(IREACH,3) .gt. 0 ) then ! two reaches are to be mixed ---------
      ISTOR1 = IRHOLD (iplan(ireach,1)) ! first reach for mixing
      ISTOR2 = IRHOLD (iplan(ireach,2)) ! second reach for mixing

      ISTOR1 = iplan(ireach,1) ! first reach for mixing
      ISTOR2 = iplan(ireach,2) ! second reach for mixing
      
      goto 12
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      

*     ==========================================================================
*     test for a straight continuation to another reach ------------------------
      if ( IPLAN(IREACH,2) .le. 0 ) goto 13 ! this is straight continuation ----
*     ==========================================================================     

*     ==========================================================================
*     otherwise this is a branch to a new headwater reach ----------------------
*     --------------------------------------------------------------------------           
      
      call initialise the running totals of load ! new headwater reach----------

      IPLAN(IREACH,2) = -IPLAN(IREACH,2)
*     identify the number of the headwater reach -------------------------------      
      IREACH = -IPLAN(IREACH,2)
*     set the number of the next feature ---------------------------------------
      JNEXT = feeture + 1 ! number of the next feature for new headwater reach
      start reach = 0 ! for new headwaters
*     check for illegal data for this reach ------------------------------------
*     this reach must start with feature type 10, 11 etc -----------------------
      if ((JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 11 .or. 
     &     JT(JNEXT) .eq. 20 .or. JT(JNEXT) .eq. 21 .or.
     &     JT(JNEXT) .eq. 45 .or.
     &     JT(JNEXT) .eq. 22 .or. JT(JNEXT) .eq. 23) .and.
     &     IREACH .eq. JREACH(JNEXT) ) then
          
      else !  missing feature type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call change colour of text (20) ! bright red
      write( *,1275) ireach,rname(ireach)
 1275 format(/110('*')/
     &'* Missing Feature for Reach number ... ',i7,4x,a16)
      write( *,2875)
 2875 format(
     &'* No flow and quality data for the Head of a Reach whose ',
     &'top forms an upstream boundary of the catchment ...       '/
     &'* (The first feature on the Reach must be Type',
     &' 10, 11 or 45 etc ... )'/110('*')/)
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,1075) ireach,rname(ireach)
      write(09,1075) ireach,rname(ireach)
      write(33,1075) ireach,rname(ireach)
 1075 format(/110('*')/
     &'*** Missing Feature for Reach number ... ',i7,1x,a16)
      write(09,1875)
      write(01,1875)
      write(33,1875)
 1875 format(110('-')/
     &'*** No flow and quality data for the Head of a Reach whose ',
     &'top forms an upstream boundary of the catchment ...       '/
     &'*** (The first feature on the Reach is usually Type',
     &' 10, 11 or 45 etc .... )'/
     &'*** Headwater flows have been set to zero'/110('*')/)
      flow(1) = 0.0
      flow(2) = 0.0
      flow(3) = 0.0
      flow(4) = 0.0
      do is = 1, NS
      FMS(IS) = 0.0
      do jdet = 1, ndet
      CMS(JDET,IS) = 0.0
      enddo
      enddo ! do is = 1, NS missing feature type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      endif ! if ((JT(JNEXT) .eq. 10 etc ---------------------------------------
*     the reach looks correct --------------------------------------------------

      goto 19 ! go back (and process the headwater reach) ---------------------- 
*     ========================================================== headwater reach
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
      

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
*     test for a straight continuation of reaches ( if so go to 16 ) ----------- 
*     or the final reach if IPLAN equals zero ( go to 15 ) ---------------------
   13 continue
      if ( IPLAN(IREACH,1) ) 15,15,16 ! check for straight continuation --------
*     yes, this is a straight continuation -------------------------------------
   16 continue ! straight continuation -----------------------------------------
      start reach = 1 ! indicator of a straight continuation -------------------
      IREACH = IPLAN(IREACH,1) ! next reach is a straight continuation -- IREACH
      do istor = 1, KR
      if ( JSTOR(istor) .eq. I ) goto 5407
      enddo
 5407 continue

      
*     check for illegal reach data ccccccccccccccccccccccccccccccccccccccccccccc
      if ( JREACH (feeture + 1) .eq. IREACH ) then ! ccccccccccccccccccccccccccc
      JNEXT = feeture + 1
      if ( ( JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 45 ) .and.    
     &Kount Reach (IREACH) .gt. 0 ) then
      IPLAN(IREACH,2) = IPLAN(IREACH,1)
      IPLAN(IREACH,1) = 0
      call change colour of text (20) ! bright red
      write( *,1905)IREACH,rname(IREACH),JT(JNEXT),Uname(JNEXT)
 1905 format(
     &'*** Illegal data for Reach number',i6,' ... ',a16/
     &'*** A straight continuation has data for the head of a Reach ',
     &'instead of taking data from end of an upstream Reach ...'/
     &'*** The first feature on this Reach should ',
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
      endif ! if ( JREACH (feeture + 1) .eq. IREACH ) ! cccccccccccccccccccccccc
*     check for illegal reach data ccccccccccccccccccccccccccccccccccccccccccccc
      
      
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
      enddo ! JJ = feeture, MU
      
      ISTOR2 = IRHOLD (IREACH)
      JSTOR (ISTOR2) = 0 ! the data for this reach are no longer needed --------
      IRHOLD (IREACH) = 0 ! used to store the data for a reach that is needed --
      
 2397 continue

      goto 89 ! finished the straight continuation 


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
*     confluence of reaches ----------------------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
   12 continue
      
      call mix the reaches

      J = IPLAN(IREACH,1)
      I = IPLAN(IREACH,2)
      IPLAN(IREACH,1) = -IPLAN(IREACH,1)
      IPLAN(IREACH,2) = -IPLAN(IREACH,2)
      IPLAN(IREACH,3) = -IPLAN(IREACH,3)
      IREACH = - IPLAN(IREACH,3)
      do istor = 1, KR
      if ( JSTOR(istor) .eq. J ) goto 5406
      enddo
 5406 continue
      do istor = 1, KR
      if ( JSTOR(istor) .eq. I ) goto 5405
      enddo
 5405 continue
          
      start reach = 2 ! continue with mixed reach ------------------------------

*     check for illegal reach data ---------------------------------------------
      JNEXT = feeture + 1

*     ##### check this section with test data ############################ CHECK
      if ( JREACH(JNEXT) .eq. IREACH .and. 
     &( JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 45 .or.
     &  JT(JNEXT) .eq. 11 .or.
     &  JT(JNEXT) .eq. 20 .or. JT(JNEXT) .eq. 21 .or. 
     &  JT(JNEXT) .eq. 22 .or. JT(JNEXT) .eq. 23 )) then
      if ( kerror .eq. 1 ) then ! ----------------------------------------------
      call change colour of text (20) ! bright red
      write( *,1406)rname(IREACH)
 1406 format('*** Illegal data for a Reach ...',19x,'...',7x,
     &'Reach: ',a16,21x,'Calculation continues ...')
      call set screen text colour
      endif ! if ( kerror .eq. 1 ) ---------------------------------------------
      write(01,1006)rname(IREACH)
      write(09,1006)rname(IREACH)
      write(33,1006)rname(IREACH)
 1006 format(77('-')/
     &'*** Illegal data for a Reach downstream of the confluence of ',
     &'other Reaches ...'/
     &'*** (First feature on Reach should not be Type 10,11,45,',
     &'20,21,22 or 23 ... )'/
     &'*** Simulation halted at reach: ',a16/110('-'))
      return
      endif ! if ( JREACH(JNEXT) .eq. IREACH  ############################ CHECK
*     ##########################################################################

      
*     store the river quality statistics for graph plotting ------------------------
   89 continue

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! start the calculation     
      call get summaries of loads

      do J = 1, ndet ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J), ! dissolved
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J), ! solid
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
      endif ! dissolved and solid metal
      endif ! if ( QTYPE (J) .ne. 4 ) 

      if ( QTYPE (J) .ne. 4 ) then ! ===========================================
      IQSreach = EQS reach (IREACH,J) ! river quality target for Reaches =======
      if ( IQSreach .gt. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,J) 
      RQO(J) = targit
      endif ! if ( background class .eq. 1 ) -----------------------------------
     
      do icc = 1, nclass ! .....................................................
      class limmits (icc,J) = standards for reach (IQSreach,icc)
      enddo ! ..................................................................
      
      do ic = 1, nclass - 1 ! ==================================================
      if ( MRQS(J). ne. 4 .and. MRQS(J). ne. 5) then ! -------------------------
      if ( class limmits (ic,J) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,J))
      endif
      endif ! if ( MRQS(J). ne. 4 .and. MRQS(J). ne. 5) ------------------------
      enddo ! do ic = 1, nclass - 1 ============================================
      endif ! if ( IQSreach .gt. 0 ) +++++++++++++++++++++++++++++++++++++++++++
      
      RQO(J) = targit ! use the target for graphs ------------------------------
      MRQO(J) = MRQS(J)
      endif ! if ( QTYPE (J) .ne. 4 ) ==========================================
      
      enddo ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*     ADIST   - length of main river upstream of the start of the Reach --------
*             - this is used for assembling data for graphs etc ----------------

      Length of main river = ADIST(IREACH)
      
      if ( ical13 .eq. 0 ) then
      write(22,300)ireach,0, ! ---------- start of reach ------------------- SGR
     &' " Start of Reach                             "',
     &RNAME(JREACH(KFEAT+1)),0
      write(22,301) '[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4), ! ---- SGR
     &              '[Flow    Obsd]',(0,JCP=1,4) ! ------------------------- SGR

      do j=1,MP10 ! ============================================================
	if ( QTYPE(J) .ne. 4 ) then
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12) ! ---- SGR
*    &,(0,JCP=1,4),(CD(JCP,J),JCP=1,12),(0,JCP=1,4)
*    &,FLOW(1),flow(3),FLOW(2),flow(4),(0,JCP=1,4)
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12) ! ---- SGR
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',0 ! RQO(J) ! -------------- SGR
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0 ! ----------------------- SGR

  300 format(I4,',',1PE11.4,',',A47,',"',A16,'",',i4)
  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))
      
      endif
      enddo ! do j=1,MP10 ==== loop on determinands ============================
      endif
      
      if ( kfinal .eq. 0 ) goto 15 ! then this has been the last reach
*     process the reach --------------------------------------------------------
      
      goto 19 ! go back and process the next reach -----------------------------

*     the last reach has been processed ========================================
*     summarise the results ====================================================
   15 continue

*     write out the loads at the end of the run --------------------------------
      if ( ical13 .eq. 0 ) then
      if ( iscreen .lt. 3 ) then
      call change colour of text (22) ! light blue
*     total length of river subject to calculations ----------------------------
      write( *,7399)Total river length
 7399 format(5x,'Total length of rivers',4x,f12.1)
      call set screen text colour
      endif
      endif
      
      call outsum at end of run

      if ( ical .ne. 1 ) then
      call list summary tables
      endif
      
      if ( ical .eq. 1 ) write(74,5000)
 5000 format(123('*'))
      if ( ical .eq. 1 .or. ical .eq. 3 ) write( *,5010)
 5010 format(130('-'))
      
      return
      end



*     summarise results --------------------------------------------------------
      subroutine list summary tables
      include 'COMMON DATA.FOR'
      character *16 SET5
      character *09 ninth(4)

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,3926)
 3926 format(/24X,53('#')/
     &24X,'CALCULATIONS FINISHED FOR ALL REACHES ....     '/
     &24X,53('#'))
      endif
      endif
      if ( IPRINT .eq. 1 ) return

*     list details of river quality monitoring points ==========================
      if ( ical .ne. 1 ) then !  list details of river quality monitoring points 
      if ( lmonp .gt. 0 ) then ! there are monitoring stations =================
      do 9 JP = 1, ndet ! ======================================================
      if ( QTYPE (JP) .ne. 4 ) then ! ==========================================
      ipr = 0 ! intialise the marker

      write(03,1050)DNAME(JP)
 1050 format(/123('-')/
     &'Summary of Results for Quality Monitoring Stations...... ',
     &A11/123('-')/
     &'Name of Sampling Point            Reach',
     &10X,'Calculated  Observed   ----- % Change Needed',
     &02X,'   Calculated       Observed'/
     &49X,'      Mean      Mean   ---------------------',
     &02X,'95-percentile  95-percentile'/123('-'))
      
      
      do 3 I = 1, LMONP ! loop through the number of moitoring stations
      itemps = LSAMPOINTS(I)
      IQ = JQ(itemps)
      if ( IQ .eq. 0 ) goto 3

*     calculate the percentiles from mean and standard deviation ---------------
*     used to compute summary statistics at monitoring points ------------------
      call calculate percentiles from mean and standard deviation
     &(JP,DCM,DCS,DCP)

      MARK=' '
      if (JQCAL(itemps) .eq.  0) MARK='*' ! no quality calibration requested ---
      if (JQCAL(itemps) .eq.  0) ipr = 1 
      if (JQCAL(itemps) .eq. -1) MARK='+' 
      if (JQCAL(itemps) .eq. -1) ipr = 2
      
      call ASSEM1(quolity data(IQ,JP,MO+2),quolity data(IQ,JP,MO+3),
     &SET5)

      call set format for printout2(ninth(1),Qstations(I,JP,1))
      call set format for printout2(ninth(2),DCM)
      call set format for printout2(ninth(3),Qstations(I,JP,3))
      call set format for printout2(ninth(4),DCP)

      if ( JQCAL(itemps) .gt. 0 ) then    
      write(03,7944)MARK,UNAME(itemps),RNAME(JREACH(itemps)),
     &ninth(1),ninth(2),int(quolity data(IQ,JP,MO+1)),SET5,
     %ninth(3),ninth(4)
 7944 format(A1,A32,1X,A16,a9,1x,a9,i8,a16,6x,a9,6x,a9)
      endif 
      
    3 continue

      write(03,1001)
 1001 format(123('-'))
      if ( ical .ne. 0 .and. ipr .eq. 1 ) write(03,1917)
      if ( ical .eq. 4 .and. ipr .eq. 2 ) write(03,7917)
 7917 format('  + Imperfect gap filling')

      endif ! if ( QTYPE (JP) .ne. 4 ) =========================================
    9 continue ! do 9 JP = 1, ndet =============================================
      endif ! if ( lmonp .gt. 0 ) ! ============================================
      endif ! if ( ical .ne. 1 ) ======================================= quality   

  
*     list the details of the flow gauges ================================ flow
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
 1917 format('* Gap filling not selected')
      if ( ical .ne. 0 .and. ipr .eq. 2) write(03,1947)
 1947 format('X No flow data specified')

      if ( iforce gap fill .eq. 1 ) then
      write(03,1967)
 1967 format('Gap filling has been forced in at all ',
     &'flow gauges with specified data')
      endif
 2000 continue

      return
      end



*     write out the headings for the start of the Reach ------------------------
      subroutine write headings at the start of the reach
      include 'COMMON DATA.FOR'
      character *170 LINE
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
*     write(01,8975)
 8975 format(77('-')/
     &'The flow data for the head of this reach are a diversion ',
     &'through to a bifurcation'/77('-'))
      endif ! bifurcations -----------------------------------------------------

*     check for unusual data on river flow -------------------------------------
      if ( Flow(2) .gt. 0.8 * Flow(1) ) then
      call sort format 2 (Flow(1),Flow(2))
      write(33,2003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,FUNIT,valchars11,FUNIT
 2003 format(/77('-')/,A16,7x,'IMPLAUSIBLE DATA ON RIVER FLOW',6x,
     &'Reach Number',I6/77('-')/
     &'Length of Reach: ',F6.1,' km',5X,
     &' Flow at head of Reach:  Mean =',a10,1x,a4/38X, ! ------------------- ERR
     &'95-percentile low flow =',a10,1x,a4/77('-'))
      endif ! if ( Flow(2) .gt. 0.8 * Flow(1) ) --------------------------------

      call write preliminary headings for the reach ! ==========================
      
      endif ! if ( IPRINT .eq. 0 ) =============================================

      if ( MONF .gt. 1 ) call write shots for river flow ! ---- at head of reach
      
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
      
      do IS = 1, NS
      accum load (IS) = 0.0  
      enddo

*     write out the data on river quality --------------------------------------
      if ( nobigout .le. 0 ) then ! ============================================
      if ( ical13 .eq. 0 ) then
      write(01,1004)RNAME(IREACH)
 1004 format(110('=')/'River quality at the head of the Reach named: ',
     &a16/110('='))
      endif ! if ( ical13 .eq. 0 )
      if ( ical .ne. 3 ) then
      call write out loads and quality (3) ! at head of reach
      call write out monthly river quality (1) ! at head of reach
      endif ! if ( ical .ne. 3 ) -----------------------------------------------
      endif ! if ( nobigout .le. 0 ) ===========================================
      

*     write the heading for the tabular data -----------------------------------
      if ( IREACH .eq. JREACH(1) ) then
*     heading for 90, 99 and 95-percentile mode --------------------------------
      if ( output mode .ne. 1 ) then ! ----------------- heading for percentiles
      do ipt = 1, ndet
      kpt(ipt) = kptarg
      if ( Qtype (ipt) .eq. 03 .or. Qtype (ipt) .eq. 05 ) then
      kpt(ipt) = 100 - kpt(ipt)
      endif
      enddo
      
      do IP = 1, ndet
      sevenchars(IP) = '      -'
      if ( QTYPE (IP) .ne. 4 ) then
      write(sevenchars(IP),6280)kpt(IP)
 6280 format(i5,'-p')
      endif
      enddo

      if ( iscreen .ne. 3 ) then
      call change colour of text (15) ! white      
      write( *,9344)('-------',i=1,ndet)
      write( *,1494)(DNA(IP),IP=1,ndet)
 1494 format(37X,'  Dist   Flow   ',10(A4,3X))
      write( *,5494)FUNIT,(UNITS(IP),IP=1,ndet)
 5494 format(39X,'(km)   ',11(A4,3x))
      write( *,1404)(sevenchars(IP),IP=1,ndet)
 1404 format(37X,'  ----    5-p',10a7)
      call set screen text colour
      write( *,9344)('-------',i=1,ndet)
      endif

      write(09,9344)('-------',i=1,ndet)
      write(09,1494)(DNA(IP),IP=1, ndet)
      write(09,5494)FUNIT,(UNITS(IP),IP=1, ndet)
      write(09,1404)(sevenchars(IP),IP=1, ndet)
      write(09,9344)('-------',i=1,ndet)
      write(33,4288)
 4288 format(77('-')/)
      write(33,9344)('-------',i=1,ndet)
      write(33,1494)(DNA(IP),IP=1, ndet)
      write(33,5494)FUNIT,(UNITS(IP),IP=1, ndet)
      write(33,1404)(sevenchars(IP),IP=1, ndet)
      write(33,9344)('-------',i=1,ndet)

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(72,1434)
 1434 format(45('-'),2x,104('-')/47x,'Dist   Flow ',92('-'))
      write(72,5434)FUNIT
 5434 format('Location ...',33X,'  (km)   ',A4,' ',92('-'))
      write(72,1934)
 1934 format(45X,'                                            '/
     &45X,'  ----      Mean       1%       5%      10%      20%',
     &'      50%      80%      90%      95%      99%')
      write(72,1384)
 1384 format(45('-'),2x,95('-'))
      endif
      endif !  if ( ical13 .eq. 0 )
      endif ! ----------------------------------------- headings for percentiles

*     heading for mean mode ----------------------------------------------------
      do IP = 1, ndet
      sevenchars(IP) = '       '
      if ( QTYPE (IP) .ne. 4 ) then
      sevenchars(IP) = '   mean'
      endif
      enddo
      if ( output mode .eq. 1 ) then ! ------------------- heading for mean mode
      if ( iscreen .ne. 3 ) then  
      call change colour of text (15) ! white      
      write( *,9344)('-------',i=1,ndet)
      write( *,9944)(DNA(IP),IP=1, ndet)
 9944 format(37X,'  Dist   Flow   ',10(A4,3X))
      write( *,9444)FUNIT,(UNITS(IP),IP=1, ndet)
 9444 format(37X,'  (km) '11(2x,A4,1x))
      write( *,9244)(sevenchars(i),i=1,ndet)
 9244 format(37X,'  ----   mean',10a7/38X,92('-'))
      write( *,9344)('-------',i=1,ndet)
 9344 format(39X,11('-'),10a7)
      call set screen text colour
      endif

      write(09,9344)('-------',i=1,ndet)
      write(09,9944)(DNA(IP),IP=1, ndet)
      write(09,9444)FUNIT,(UNITS(IP),IP=1, ndet)
      write(09,9244)(sevenchars(IP),IP=1,ndet)
      write(09,9344)('-------',i=1,ndet)
      write(33,9344)('-------',i=1,ndet)
      write(33,9944)(DNA(IP),IP=1, ndet)
      write(33,9444)FUNIT,(UNITS(IP),IP=1, ndet)
      write(33,9244)(sevenchars(IP),IP=1,ndet)
      write(33,9344)('-------',i=1,ndet)
      
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
      endif ! if ( output mode .eq. 1 ) !  ! ------------- heading for mean mode
      
*     heading for load report --------------------------------------------------
      if ( ical13 .eq. 0 ) then
      !if ( nobigout .le. 0 ) write(27,9544)(DNA(IP),IP=1, ndet)
      !if ( nobigout .le. 0 ) write(27,6544)FUNIT
 9544 format(39X,101('-')/
     &37X,'  Dist   Flow      ',9(A4,6X))
 6544 format(
     &37X,'  (km)  (',A4,')  ------------ (load units) -----------',
     &'---------- (load units) --------------------------'/
     &37X,'                                                      '/
     &37X,'  ----   -------------------------- mean -------------',
     &'------------------mean---------------------------')
      endif
      endif

      LINE='
     &                                    '

      if ( output mode .eq. 0 ) NMORQ = 3
      if ( output mode .eq. 1 ) NMORQ = 1
      if ( output mode .eq. 2 ) NMORQ = 4
      if ( output mode .eq. 3 ) NMORQ = 5

      call set up output of calculated means or percentiles ! ---- head of reach
      call set up flowa ! ---------------------------------------- head of reach
      call set up flow95 ! --------------------------------------- head of reach
      
      
      if ( start reach .eq. 0 ) then ! a branch to a new headwaters ============
      if ( output mode .ne. 1 ) then
      write(LINE,7500)RNAME(IREACH),ADIST(IREACH),Flowchars(2), !  head of reach
     &(Sevenchars(jdet),jdet=1,ndet)
 7500 format(2X,'Head of ',A16,10X,F7.1,11A7)
      else
      write(LINE,7500)RNAME(IREACH),ADIST(IREACH),Flowchars(1), !  head of reach
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
      call change colour of text (11) ! cyan
      if ( iscreen .lt. 3 ) write( *,7511)LINE ! ----------------- head of reach
      call set screen text colour
      endif ! if ( start reach .eq. 0 ) then ! a branch to a new headwaters ====

      
      if ( start reach .eq. 1 ) then ! a straight continaution ================= 
      if ( output mode .ne. 1 ) then
      write(LINE,7600)RNAME(IREACH),ADIST(IREACH),Flowchars(2), !  head of reach
     &(Sevenchars(jdet),jdet=1,ndet)
 7600 format(2X,'Continue with ',A16,4X,F7.1,11A7)
      else
      write(LINE,7600)RNAME(IREACH),ADIST(IREACH),Flowchars(1), !  head of reach
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
      call change colour of text (11) ! cyan
      if ( iscreen .lt. 3 ) write( *,7511)LINE ! ----------------- head of reach
      call set screen text colour
      endif ! if ( start reach .eq. 1 )  a straight continaution ===============
      
      
      if ( start reach .eq. 2 ) then ! a mixing of reaches ====================
          
      IR1 = IPLAN(LASTREACH,1)
      IR2 = IPLAN(LASTREACH,2)
      IR3 = IPLAN(LASTREACH,3)
      
      if (IR1+IR2+IR3 .lt. 0 ) then ! -----------------------------------------
      LASTREACH = LASTREACH2
      if ( IPLAN(LASTREACH,1) .lt. 0 ) IR1 = -IR1
      if ( IPLAN(LASTREACH,2) .lt. 0 ) IR2 = -IR2
      if ( IPLAN(LASTREACH,3) .lt. 0 ) IR3 = -IR3
      endif   
      
      if ( output mode .ne. 1 ) then
      write(LINE,7700)RNAME(IREACH),ADIST(IREACH),Flowchars(2), ! Mixed Reaches
     &(Sevenchars(jdet),jdet=1,ndet)
 7700 format(2X,'Mix to form ',A16,6X,F7.1,11A7)
      else
      write(LINE,7700)RNAME(IREACH),ADIST(IREACH),Flowchars(1), ! Mix of Reaches
     &(Sevenchars(jdet),jdet=1,ndet)
      endif
      call change colour of text (11) ! cyan
      if ( iscreen .lt. 3 ) write( *,7511)LINE ! ---------------- Mix of Reaches
      call set screen text colour
      endif ! if ( start reach .eq. 2 ) a mixing of reaches ====================

 7523 continue

      write(09,7511)LINE 
      write(33,7511)LINE 
 7511 format(A121)

*     calculate and output the percentage of loads from various sources --------
      call outsum at start of reach ! output of loads 

      return
      end


      
      

      subroutine write data for graph plotting 2 (JU)
      include 'COMMON DATA.FOR'
      character*6 cptest
      character*3 place
      
      if ( JSKIP .gt. 0 ) return

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! write graphs
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
      call load statistics (2,J,CD1(1,J),CD1(2,J),CD1(3,J),CD1(4,J), ! dissolved
     &CD1(5,J),CD1(6,J),CD1(7,J),CD1(8,J),CD1(9,J),CD1(10,J),CD1(11,J),
     &CD1(12,J)) ! dissolved metal
      call load statistics (3,J,CD2(1,J),CD2(2,J),CD2(3,J),CD2(4,J), ! solid
     &CD2(5,J),CD2(6,J),CD2(7,J),CD2(8,J),CD2(9,J),CD2(10,J),CD2(11,J),
     &CD2(12,J)) ! solid metal
      endif ! dissolved and solid metal
      endif
      enddo

*     the following statements are included to identify gap filling points -----
      read(uname(ju)(1:6),'(a6)')cptest	
      if (cptest .eq. '  ....') then
      numtype = 99
      else
      numtype = jt(ju)
      if ( numtype .eq. 60 ) numtype = 5
      if ( numtype .eq. 61 ) numtype = 5
      if ( numtype .eq. 62 ) numtype = 5
      endif

      if ( numtype .lt. 80 ) then ! less than the biggest Feature Type Number --
      
      plotlength = Length of main river - adist(ireach)
      
      if ( plotlength .lt. 1.0e-08 ) then ! ------------------------------------ 
      plotlength = 0.0
      endif ! if ( plotlength .lt. 1.0e-08 ) ----------------------------------- 
      
      JTK = JT(ju)
      if ( JTK .eq. 25 .or. JTK .eq. 27 .or. JTK .eq. 29 .or.  
     &     JTK .eq. 26 .or. JTK .eq. 28 .or. JTK .eq. 30 .or. 
     &     JTK .eq. 31 .or. JTK .eq. 33 .or. JTK .eq. 35 .or. 
     &     JTK .eq. 32 .or. JTK .eq. 34 .or. JTK .eq. 36 .or. 
     &     JTK .eq. 37 .or. JTK .eq. 40 .or. JTK .eq. 42 .or.
     &     JTK .eq. 38 .or. JTK .eq. 41 .or. JTK .eq. 43 .or.
     &     JTK .eq. 13 .or. JTK .eq. 15 .or. JTK .eq. 46 .or.
     &     JTK .eq. 14 .or. JTK .eq. 16 .or. JTK .eq. 47 .or.
     &     JTK .eq. 48 .or. JTK .eq. 50 .or. JTK .eq. 52 .or. 
     &     JTK .eq. 49 .or. JTK .eq. 51 .or. JTK .eq. 53 .or. 
     &     JTK .eq. 54 .or. JTK .eq. 56 .or. JTK .eq. 58 .or.
     &     JTK .eq. 55 .or. JTK .eq. 57 .or. JTK .eq. 59 ) goto 3493

      write(22,300)ireach,plotlength, ! ----------- at a feature ----------- SGR
     &UNAME(JU),RNAME(IREACH),numtype ! ------------------------------------ SGR
  300 format(I4,',',1PE11.4,',',' " U/S ',A40,'","',A16,'",',i4)
      write(22,301)'[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4), ! ----- SGR
     &             '[Flow    Obsd]',(0,JCP=1,4) ! -------------------------- SGR               
  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4)) ! -------------- SGR

      do j = 1,MP10 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      
*     CL(1... is the mean concentration
*     CL(2... is the lower confidence limit on the mean concentration
*     CL(3... is the upper confidence limit on the mean concentration
*     CL(4... is the calculated value of the 90-percentile
*     CL(5... is the lower confidence limit on the percentile
*     CL(6... is the upper confidence limit on the percentile
*     CL(7... is the calculated value of the 95-percentile
*     CL(8... is the lower confidence limit on the percentile
*     CL(9... is the upper confidence limit on the percentile
*     CL(10.. is the calculated value of the 99-percentile
*     CL(11.. is the lower confidence limit on this percentile
*     CL(12.. is the upper confidence limit on this percentile
          
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12) ! ---- SGR
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12) ! ---- SGR
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J) ! ------------------ SGR
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0 ! ----------------------- SGR
  302 format(1x,a4,I1,A9,12(',',1PE11.4)) ! -------------------------------- SGR
  303 format(1x,a4,I1,A9,4(',',1PE11.4)) ! --------------------------------- SGR
  304 format(1x,a4,I1,A9,1(',',1PE11.4)) ! --------------------------------- SGR
      endif ! if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++
      enddo ! do j = 1,MP10 ! ++++++++++++===+++++++++++++++++++++++++++++++++++

 3493 continue
      
      if ( JTK .eq. 25 .or. JTK .eq. 27 .or. JTK .eq. 29 .or. ! --------- Gi.CSV 
     &     JTK .eq. 26 .or. JTK .eq. 28 .or. JTK .eq. 30 .or. ! --------- Gi.CSV 
     &     JTK .eq. 31 .or. JTK .eq. 33 .or. JTK .eq. 35 .or. ! --------- Gi.CSV
     &     JTK .eq. 32 .or. JTK .eq. 34 .or. JTK .eq. 36 .or. ! --------- Gi.CSV
     &     JTK .eq. 37 .or. JTK .eq. 40 .or. JTK .eq. 42 .or. ! --------- Gi.CSV
     &     JTK .eq. 38 .or. JTK .eq. 41 .or. JTK .eq. 43 .or. ! --------- Gi.CSV
     &     JTK .eq. 13 .or. JTK .eq. 15 .or. JTK .eq. 46 .or. ! --------- Gi.CSV
     &     JTK .eq. 14 .or. JTK .eq. 16 .or. JTK .eq. 47 .or. ! --------- Gi.CSV
     &     JTK .eq. 48 .or. JTK .eq. 50 .or. JTK .eq. 52 .or. ! --------- Gi.CSV
     &     JTK .eq. 49 .or. JTK .eq. 51 .or. JTK .eq. 53 .or. ! --------- Gi.CSV
     &     JTK .eq. 54 .or. JTK .eq. 56 .or. JTK .eq. 58 .or. ! --------- Gi.CSV
     &     JTK .eq. 55 .or. JTK .eq. 57 .or. JTK .eq. 59 ) goto 7493

      if ( JTK .eq. 24 .or. JTK .eq. 6 ) goto 7493 ! -------------------- Gi.CSV

      do j = 1,MP10 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( numtype .ne. 1 ) then ! ====================================== Gi.CSV
      if ( JT(KFEAT) .eq. 10 .or. JT(KFEAT) .eq. 45 ) then ! ============ Gi.CSV
      else ! ============================================================ Gi.CSV
      if ( XA(j)+X90(j)+X95(j)+X99(j) .lt. 0.000001 ) then ! ============ Gi.CSV
      write(230+j,305)UNAME(JU),RNAME(IREACH), ! ----- graph plotting --- Gi.CSV
     &ireach,JT(JU),plotlength,rlength(IREACH), ! ----------------------- Gi.CSV
     &adist(ireach)+plotlength, ! --------------------------------------- Gi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop)  ! -------------------------------- Gi.CSV
      else ! ------ add observed values --------------------------------- Gi.CSV
      write(230+j,305)UNAME(JU),RNAME(IREACH), ! ----- graph plotting --- Gi.CSV
     &ireach,JT(JU),plotlength,rlength(IREACH), ! ----------------------- Gi.CSV
     &adist(ireach)+plotlength, ! --------------------------------------- Gi.CSV
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &XA(j),XAl(j),XAu(j),X90(j),X90l(j),X90u(j), ! -- observed values -- Gi.CSV
     &X95(j),X95l(j),X95u(j),X99(j),X99l(j),X99u(j) !  observed values -- Gi.CSV
  305 format(A40,',',A16,',',I4,',',i4,200(',',1PE11.4)) ! -------------- Gi.CSV
      endif ! if ( XA(j)+X90(j)+X95(j)+X99(j) .lt. 0.000001 ) =========== Gi.CSV
      endif ! ( JT(KFEAT) .eq. 10 .or. JT(KFEAT) .eq. 45 ) then ! ======= Gi.CSV
      endif ! if ( numtype .ne. 1 ) ===================================== Gi.CSV
      endif ! if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++
      enddo ! do j = 1,MP10 ! ++++++++++++===+++++++++++++++++++++++++++++++++++
      
 7493 continue 

      
      if ( JTK .eq. 25 .or. JTK .eq. 27 .or. JTK .eq. 29 .or. ! ========= Wi.CSV  
     &     JTK .eq. 26 .or. JTK .eq. 28 .or. JTK .eq. 30 .or. ! ========= Wi.CSV 
     &     JTK .eq. 31 .or. JTK .eq. 33 .or. JTK .eq. 35 .or. ! ========= Wi.CSV
     &     JTK .eq. 32 .or. JTK .eq. 34 .or. JTK .eq. 36 .or. ! ========= Wi.CSV
     &     JTK .eq. 37 .or. JTK .eq. 40 .or. JTK .eq. 42 .or. ! ========= Wi.CSV
     &     JTK .eq. 38 .or. JTK .eq. 41 .or. JTK .eq. 43 .or. ! ========= Wi.CSV
     &     JTK .eq. 13 .or. JTK .eq. 15 .or. JTK .eq. 46 .or. ! ========= Wi.CSV
     &     JTK .eq. 14 .or. JTK .eq. 16 .or. JTK .eq. 47 .or. ! ========= Wi.CSV
     &     JTK .eq. 48 .or. JTK .eq. 50 .or. JTK .eq. 52 .or. ! ========= Wi.CSV
     &     JTK .eq. 49 .or. JTK .eq. 51 .or. JTK .eq. 53 .or. ! ========= Wi.CSV
     &     JTK .eq. 54 .or. JTK .eq. 56 .or. JTK .eq. 58 .or. ! ========= Wi.CSV
     &     JTK .eq. 55 .or. JTK .eq. 57 .or. JTK .eq. 59 ) goto 1493 ! == Wi.CSV
      
      do j = 1,MP10 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( numtype .ne. 1 ) then ! -------------------------------------- Wi.CSV
      if ( XA(j)+X90(j)+X95(j)+X99(j) .lt. 0.000001 ) then ! ------------ Wi.CSV
      !write(240+j,705)'control 1'
      !write(240+j,705)UNAME(JU),RNAME(IREACH), ! ------ word reports --- Wi.CSV
    !&ireach,JT(JU),plotlength,rlength(IREACH), ! ----------------------- Wi.CSV
    !&adist(ireach)+plotlength, ! --------------------------------------- Wi.CSV
    !&(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Wi.CSV
    !&FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Wi.CSV
    !&(in class(iclass,j),iclass=1,nclass), ! --------------------------- Wi.CSV
    !&(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Wi.CSV
    !&(CMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Wi.CSV
      else ! if ( XA(j)+X90(j)+X95(j)+X99(j) .gt. 0.000001 ) ------------ Wi.CSV 
      !write(240+j,705)'control 2'
      !write(240+j,705)UNAME(JU),RNAME(IREACH), ! ------ word reports --- Wi.CSV
    !&ireach,JT(JU),plotlength,rlength(IREACH), ! ----------------------- Wi.CSV
    !&adist(ireach)+plotlength, ! --------------------------------------- Wi.CSV
    !&(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Wi.CSV
    !&FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Wi.CSV
    !&(in class(iclass,j),iclass=1,nclass), ! --------------------------- Wi.CSV
    !&(LMcontrib(ip,J,1),ip=1,n2prop),  ! ------------------------------- Wi.CSV
    !&(CMcontrib(ip,J,1),ip=1,n2prop),  ! ------------------------------- Wi.CSV
    !&XA(j),XAl(j),XAu(j),X90(j),X90l(j),X90u(j), ! -- observed values -- Wi.CSV
    !&X95(j),X95l(j),X95u(j),X99(j),X99l(j),X99u(j) !  observed values -- Wi.CSV
  705 format(A40,',',A16,',',I4,',',i4,200(',',1PE11.4)) ! -------------- Wi.CSV
      endif ! if ( XA(j)+X90(j)+X95(j)+X99(j) .lt. 0.000001 ) =========== Wi.CSV
      endif ! if ( numtype .ne. 1 ) ===================================== Wi.CSV
      endif ! if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++
      enddo ! do j = 1,MP10 ! ++++++++++++===+++++++++++++++++++++++++++++++++++
 1493 continue ! ======================================================== Wi.CSV
      
      do j = 1,MP10 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      XA(j) = 0.0
      X90(j) = 0.0
      X95(j) = 0.0
      X99(j) = 0.0
      XAl(j) = 0.0
      X90l(j) = 0.0
      X95l(j) = 0.0
      X99l(j) = 0.0
      XAu(j) = 0.0
      X90u(j) = 0.0
      X95u(j) = 0.0
      X99u(j) = 0.0
      endif ! if ( QTYPE(J) .ne. 4 ) then ! ++++++++++++++++++++++++++++++++++++
      enddo ! do j = 1,MP10 ! ++++++++++++===+++++++++++++++++++++++++++++++++++
      
      endif ! if ( numtype .lt. 80 ) +++++++++++++++++++++++++++++++++++++++++++ 
      
      
      if ( numtype .ne. 4 .and. numtype .ne. 1 ) then
      if ( numtype .ne. 6 ) then
      if ( noGIS .eq. 0 ) then
      if ( ical13 .eq. 0 ) then

      place = 'U/S'
      if ( numtype .eq. 10 ) place = 'at '
      if ( numtype .eq. 25 .or. numtype .eq. 27 ) place = 'at '
      if ( numtype .eq. 29 ) place = 'at '
      if ( numtype .eq. 31 .or. numtype .eq. 33 ) place = 'at '
      if ( numtype .eq. 35 ) place = 'at '
      if ( numtype .eq. 37 .or. numtype .eq. 40 ) place = 'at '
      if ( numtype .eq. 42 ) place = 'at '
      if ( numtype .eq. 46 .or. numtype .eq. 48 ) place = 'at '
      if ( numtype .eq. 13 .or. numtype .eq. 15 ) place = 'at '
      if ( numtype .eq. 50 .or. numtype .eq. 52 ) place = 'at '
      if ( numtype .eq. 54 ) place = 'at '
      if ( numtype .eq. 56 .or. numtype .eq. 58 ) place = 'at '

      write(42,244)ireach,GIScode(JU),RNAME(IREACH),place, ! ---------- GIS1.CSV
     &UNAME(JU),numtype,Length of main river-adist(ireach),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &0.0,0.0,0.0,0.0
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River flow",',',A4,',',9(1PE11.4,','))

      do J=1,MP10
      write(42,245)ireach,GIScode(JU),RNAME(IREACH),place, ! ---------- GIS1.CSV
     &UNAME(JU),numtype,Length of main river-adist(ireach),
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
*     write the extra records for partitioned metals --------------------------- 
      if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      xzero = 0.0
      write(42,845)ireach,GIScode(JU),RNAME(IREACH),place, ! ---------- GIS1.CSV
     &UNAME(JU),numtype,Length of main river-adist(ireach),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))
      write(42,845)ireach,GIScode(JU),RNAME(IREACH),place,UNAME(JU),
     &numtype,Length of main river-adist(ireach),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
      endif ! dissolved and solid metal
      enddo ! loop on determinands

      write(42,247)ireach,GIScode(JU),RNAME(IREACH),place,UNAME(JU),
     &numtype,Length of main river-adist(ireach),
     &(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),qualn(J),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &5(1PE11.4,','),10(6(1PE11.4,',')),11(i2,','),i4,',')

      GIScode last = GIScode(JU)

      endif
      endif

*     write to the flow output file ! ------------------------------------------
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(72,2844)UNAME(JU), ! ------------------- .FLO
     &Length of main river-adist(ireach),FLOW(1),(flowstats (i), i=1,9)
 2844 format("U/S ",A40,f8.2,10f9.1)
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------

      endif
      endif

      return
      end



*     complete calculations for the end of the Reach ---------------------------
      subroutine process the end of the reach
      include 'COMMON DATA.FOR'
      
      character *170 LINE
      character * 3 place
      character *40 End of Reach
      character *7 prugress1,prugress2,prugress3,prugress4

      if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1026)IREACH,RNAME(IREACH)
 1026 format(//110('=')/'End of reach number',I6,' (',A16,')'/110('='))
      endif
      endif

      do IS = 1, NS
      accum load (IS) = 0.0  
      enddo

      if ( ical .ne. 1 ) then
      if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then
      write(01,1927)
 1927 format('River quality at the end of the Reach ...'/110('-'))
      endif
      call write out loads and quality (3) !  at end of reach
      if ( ical .ne. 2 .or. ical .ne. 3 ) then
      call write out monthly river quality (3) ! at end of reach
      endif ! if ( ical .ne. 2 .or. ical .ne. 3 )
      endif
      endif

      call calculate summaries of river flow

      if ( nobigout .eq. 0 ) then ! ============================================
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! -----------------------------
      call sort format 2 (flow (1), flow(2))
      if ( flow(1) .lt. 0.0000001 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(01,1227)valchars10,FUNIT
 1227 format(110('=')/'River flow at the end of the Reach ... ',
     &17X,'Mean =',a10,1x,A4)
      do jper = 1,ndet
      if ( qtype(jper).ne.4 ) write(170+jper,1227)valchars10,FUNIT
      enddo
      else
      write(01,1027)valchars10,FUNIT,valchars11,FUNIT
 1027 format(110('=')/'River flow at the end of the Reach ... ',
     &17X,'Mean =',a10,1x,A4/38X,
     &'95-percentile low flow =',a10,1x,A4)
      endif ! if ( flow(1) .lt. 0.0000001 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------

      if ( flow(1) .lt. 0.0000001 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(01,3)
    3 format(110('='))
      else
      call sort format 2 (Flow (3),Flow(4))
      write(01,2)valchars10,FUNIT,valchars11,FUNIT
    2 format(
     &46x,'90% exceedence =',a10,1x,A4/
     &46x,'99% exceedence =',a10,1x,A4/110('='))
      endif ! if ( flow(1) .lt. 0.0000001 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      call classify (KFEET,3) ! end of reach
      call assess compliance with river targets (KFEET,3) ! end of reach +++++++
      endif ! if ( nobigout .eq. 0 ) then ! ====================================

      if ( MONF .gt. 1 ) call write shots for river flow ! ----- at end of reach
      do jp = 1, ndet ! --------------------------------------------------------
      if ( QTYPE (jp) .ne. 4 ) then
      if ( MONQ .gt. 1 ) call write shots for river quality ! !  at end of reach 
      endif
      enddo ! do jp = 1, ndet --------------------------------------------------

      endif ! if ( ical .ne. ) -------------------------------------------------

*     store any data needed for mixing Reaches together later on ---------------
*     loop through all the Reaches ... see if reach number IREACH needs to be --
*     stored for later use -----------------------------------------------------


*     first loop through all the Features, checking for bifurcations bbbbbbbbbbb
      do 2296 JJ = feeture, MU ! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
*     feature types 11, 20, 21, 22 and 23 are bifurcations bbbbbbbbbbbbbbbbbbbbb
      if ( JT(JJ) .eq. 11 .or. JT(JJ) .eq. 20 .or. ! bbbbbbbbbbbbbbbbbbbbbbbbbbb
     &JT(JJ) .eq. 21 .or. JT(JJ) .eq. 22 .or. JT(JJ) .eq. 23 ) then ! bbbbbbbbbb
*     bifurcation found .... check if reach IREACH is need for this bbbbbbbbbbbb
      if ( JF(JJ) .eq. IREACH) then ! store bifurcation bbbbbbbbbbbbbbbbbbbbbbbb
*     the current Reach will be used to form other Reaches ---------------------         
          
*     scan through the stores for flow and quality data and find an empty one ss
      do 3296 ISTOR = 1, KR ! ssssssssssssssssssssssssssssssssssssssssssssssssss
      if ( JSTOR(ISTOR) .eq. 0 ) then ! store the bifurcation data sssssssssssss
*     an empty store has been found ...  load it up with data ssssssssssssssssss
      IRHOLD(IREACH) = ISTOR !   an empty store has been found for a bifurcation
      JSTOR(ISTOR) = IREACH ! store reach for use as a bifurcation sssssssssssss
      maxistor = amax0 (maxistor,istor) ! ssssssssssssssssssssssssssssssssssssss
 
      goto 9080
      JJJ = JT(feeture)
*     write the discharge shots for storage bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 300 
      write(fname,99)output_folder ! bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb bifurcation
      call trunkout ('L','O','D') ! --------------------------------------------
      open(300+IREACH,FILE=fname) ! store loads for for a bifurcation bbbbbb LOD 
      do iworks = 1, kountworks ! wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
      do JPL = 1, NDET ! loop on determinands ==================================
      if ( QTYPE(JPL) .ne. 4 ) then ! ------------------------------------------
      write(300+IREACH,4329)IREACH,istor,iworks,jpl, ! branch of bifurcation 300
     &(TELODEshots(iworks,JPL,is),is=1,NS),(FMS(is),is=1,NS) ! wwwwwwwwwwwww 300
      endif ! if ( QTYPE(JPL) .ne. 4 ) -----------------------------------------
      enddo ! do JPL = 1, NDET =================================================
      enddo ! do iworks = 1, kountworks wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
      rewind (300+IREACH) ! after writing data for branch of bifurcation wwwwwww
*     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 300
 9080 continue

      goto 2296 ! ==============================================================
      
      endif ! if ( JSTOR (ISTOR) .eq. 0 ) store the bifurcation data sssssssssss
 3296 continue ! do ISTOR = 1, KR ssssssssssssssssssssssssssssssssssssssssssssss

      write(01,2391)RNAME(IREACH),KR
      write( *,2391)RNAME(IREACH),KR
      write(09,2391)RNAME(IREACH),KR
      write(33,2391)RNAME(IREACH),KR
 2391 format(/77('-')/'*** Unable to store data for a Reach ... ',A16/
     &'*** So that they can be used to form downstream bifurcation ...'/
     &'*** Increase dimensions of JSTOR, FD and QD beyond ',I6/77('-'))
      call stop
      
      endif ! if ( JF(JJ) .eq. IREACH) bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      endif ! if ( JT(JJ) .eq. 11 etc bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
 2296 continue ! do 2296 JJ = feeture, MU ... checked for bifurcations bbbbbbbbb    


*     now check whether this Reach will be used to form any other Reach --------
*     loop on all reaches ------------------------------------------------------
      do 2096 KREACH = IREACH, MR
*     check whether another reach is using IREACH ------------------------------
      if ( IPLAN (KREACH,1) .eq. IREACH .or. 
     &     IPLAN (KREACH,2) .eq. IREACH .or. 
     &     IPLAN (KREACH,3) .eq. IREACH ) then 
*     the current Reach is used to form other Reaches --------------------------
*     scan through the stores for flow and quality data ------------------------
*     and find one which is empty ----------------------------------------------
      do ISTOR = 1, KR ! -------------------------------------------------------
      if ( JSTOR(ISTOR) .eq. 0 ) then ! store reach data for use downstream ----
      IRHOLD(IREACH) = ISTOR ! an empty store has been found -------------------
      JSTOR(ISTOR) = IREACH ! load it up with data -----------------------------
      maxistor = amax0 (maxistor,istor) ! ssssssssssssssssssssssssssssssssssssss
      goto 2098
      endif ! if ( JSTOR (ISTOR) .eq. 0 ) --------------------------------------
      enddo ! do ISTOR = 1, KR -------------------------------------------------
*     no empty store has been found --------------------------------------------
      write( *,2091)RNAME(IREACH),KR
      write(09,2091)RNAME(IREACH),KR
      write(33,2091)RNAME(IREACH),KR
      write(01,2091)RNAME(IREACH),KR
      write(08,2091)RNAME(IREACH),KR
 2091 format(/77('*')/'*** Unable to store data for a Reach ... ',A16/
     &'*** So that they can be used to form downstream Reaches ...'/
     &'*** Larger version of SIMCAT required ... '/
     &'*** Or a rearrangement of reaches ... '/
     &'*** Maximum number (KR) is currently ',I6/77('*')/)
      write(09,6299)(istor,JSTOR(ISTOR),Istor = 1, KR)
      write(33,6299)(istor,JSTOR(ISTOR),Istor = 1, KR)
      write(08,6299)(istor,JSTOR(ISTOR),Istor = 1, KR)
 6299 format(2i6)
      call stop
      endif ! if ( IPLAN (KREACH,1) etc
 2096 continue ! do 2096 KREACH = 1, MR
 2098 continue ! completed the search for storage space 
      

*     ==========================================================================
*     write discharge data to a file to be picked up for mixing downstream =====
*     ==========================================================================
      JJJ = JT(feeture)
      ISTOR = IRHOLD(IREACH)
      if ( ISTOR .eq. 0 ) goto 2099
      if ( ISTOR .eq. -999 ) goto 2099
      
*     write the discharge shots for storage rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr 300
      write(fname,99)output_folder
   99 format(a136)
      call trunkout ('L','O','D')
      KREACH = ReachCode (ireach)
      open(300+IREACH,FILE=fname) ! open file to store loads for reaches rrr LOD
     
      do iworks = 1, kountworks ! loop through the number of works =============
      do JPL = 1, NDET ! loop on determinands ----------------------------------
      if ( QTYPE (JPL) .ne. 4 ) then ! -----------------------------------------
      
      if ( start reach .eq. 0 ) then ! ! for a new headwaters ==================
*     check whether the works is on this reach ---------------------------------
      if ( JREACH(identify werks(iworks)) .ne. IREACH ) then ! ooooooooooooooooo
*     it is not on this reach ..................................................
      if ( JT (feeture) .eq. 10 .or. ! then this is a headwaters ...............
     &     JT (feeture) .eq. 45 ) then ! then this is a headwaters .............
*     exclude the works from this reach ........................................
      else ! it is not a headwaters ............................................
*     add the works to output file .............................................
      write(300+IREACH,4329)IREACH,istor,iworks,jpl, ! not headwaters ..........
     &(TELODEshots(iworks,JPL,is),is=1,NS),(FMS(is),is=1,NS) ! ............. 300
      endif ! if ( JT (feeture) .eq. 10 ) hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
      else ! the works is actually on this reach ...............................
      write(300+IREACH,4329)IREACH,istor,iworks,jpl, ! works is on this reach ..
     &(TELODEshots(iworks,JPL,is),is=1,NS),(FMS(is),is=1,NS) ! ............. 300
      endif ! if ( JREACH(identify werks(iworks)) .ne. IREACH ) oooooooooooooooo
      
      else ! where reach is not a headwaters  ==================================          
      if ( start reach .eq. 1 ) then ! for a straight continuation sssssssssssss
      write(300+IREACH,4329)IREACH,istor,iworks,jpl, ! straight continuation sss
     &(TELODEshots(iworks,JPL,is),is=1,NS),(FMS(is),is=1,NS) ! sssssssssssss 300
      else  
      write(300+IREACH,4329)IREACH,istor,iworks,jpl, ! other reaches mmmmmmmmmmm
     &(TELODEshots(iworks,JPL,is),is=1,NS),(FMS(is),is=1,NS) ! mmmmmmmmmmmmm 300
 4329 format(i5,3(',',i5),10000(',',1pe18.6))
      endif ! if ( start reach .eq. 1 ) ========================================
      endif ! if ( start reach .eq. 0 ) ========================================

      endif ! if ( QTYPE (JPL) .ne. 4 )
      enddo ! do JPL = 1, NDET ... loop on determinands ------------------------
      enddo ! do iworks = 1, kountworks ========================================
      
      rewind (300+IREACH) ! after writing data for this reach rrrrrrrrrrrrrrrrrr
 6999 continue
*     ==========================================================================
*     written discharge data to a file to be picked up for maixing downstream ==
*     rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr      
 
      
*     ==========================================================================
*     now place other data in the storage space 
*     ==========================================================================
      do 2501 JPL = 1, NDET ! ==================================================
      if ( QTYPE (JPL) .ne. 4 ) then ! =========================================
          
*     store the sampling rates -------------------------------------------------
      do ip = 1, n2prop  ! loop through types of pollution-----------------------
*     14  = Aggregated STWs                 end of the reach          Feature 42
*     19  = Diffuse input                   end of the reach          Feature 15
*     NTD = Total                           end of the reach          
      if ( ip .eq. 19 .or. ip .eq. 14 .or. ip .eq. NTD ) then ! ----------------
      RQUALNB(ISTOR,JPL,IP) = QUALNB(JPL,IP) ! number of samples ---------------
      CQUALNB(ISTOR,JPL,IP) = CMcontrib(IP,JPL,1) ! mean concentration ---------      
      endif ! if ( ip .eq. 19 .or. ip .eq. 14 ) --------------------------------
      enddo ! do ip = 1, n2prop ! ----------------------------------------------

      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do 2509 J1 = 1, nx    
      TNLOADUP3 (ISTOR,JPL,J1) = TNLOADUP2 (JPL,J1) ! added by nat purification
      TNLOADDN3 (ISTOR,JPL,J1) = TNLOADDN2 (JPL,J1) ! lost by nat purification
      TALOADDN3 (ISTOR,JPL,J1) = TALOADDN2 (JPL,J1) ! lost by qual gap filling
      TILOADDN3 (ISTOR,JPL,J1) = TILOADDN2 (JPL,J1) ! lost by flow gap filling
      TBLODE3   (ISTOR,JPL,J1) = TBLODE2   (JPL,J1) ! lost by abstraction
 2509 continue
      


      if ( kount bodies .gt. 0 ) then ! ----------------------------------------
      do 105 ibodies = 1, kount bodies ! ---------------------------------------
      TWloadsrch(ibodies,ISTOR,JPL) = TWLOADS(ibodies,JPL,i13)
      do ip = 1, n2prop ! -------------------------------------------------------
*     store the breakdown of annual loads (i13) from upstream sub-catchments ---
      TWloadsrchapp (ibodies,ISTOR,JPL,ip) = 
     &TWLOADSapp (ibodies,JPL,i13,ip)
      enddo ! do ip = 1, n2prop -------------------------------------------------
  105 continue ! do 105 ibodies = 1, kount bodies ------------------------------
      endif ! if ( kount bodies .gt. 0 ) ---------------------------------------     
  
      endif ! if ( QTYPE (JPL) .ne. 4 ) ========================================
 2501 continue ! do 2501 JPL = 1, NDET =========================================
*     ==========================================================================

      
      do IS = 1, NS ! ==========================================================
      FD(ISTOR,IS) = FMS (IS) ! store the river flows ==========================
      EFD(ISTOR,IS) = EFMS(IS) ! the proportion of effluent flow in each shot ==
      do jdet = 1, ndet ! store the river quality data -------------------------
      QD(ISTOR,jdet,IS) = CMS(jdet,IS)
      do ip = 1, n2prop ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      EQD(ip,ISTOR,jdet,IS) = CTMS(ip,jdet,IS) ! contributions from feature type
      enddo ! do ip = 1, n2prop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      enddo ! do jdet = 1, ndet ! ----------------------------------------------
      enddo ! do IS = 1, NS ! ==================================================


 2099 continue

*     store the river quality statistics for graph plotting --------------------
*     set the distance from start of the river to the end of this Reach --------
      Length of main river = RLENGTH(IREACH) + ADIST(IREACH)
      IR1 = IPLAN(LASTREACH,1)
      IR2 = IPLAN(LASTREACH,2)
      IR3 = IPLAN(LASTREACH,3)

      if ( IR1+IR2+IR3 .lt. 1 ) then ! -----------------------------------------
      LASTREACH = LASTREACH2    
      endif
      IR1 = -IPLAN(LASTREACH,1)
      IR2 = -IPLAN(LASTREACH,2)
      IR3 = -IPLAN(LASTREACH,3)

      call get summaries of river quality from the shots ! end of a reach ------
      call get summaries of loads

*     loop on the determinands - write out the data ----------------------------
      do J = 1, ndet
      if ( QTYPE (J) .ne. 4 ) then
      call compute statistics for plots of river quality
     &(1,J,CL(1,J),CL(2,J),CL(3,J),CL(4,J),CL(5,J),CL(6,J),
     &CL(7,J),CL(8,J),CL(9,J),CL(10,J),CL(11,J),CL(12,J))

*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! --------------------------------------------------
      call load statistics (1,J,CD(1,J),CD(2,J),CD(3,J),CD(4,J),CD(5,J),
     &CD(6,J),CD(7,J),CD(8,J),CD(9,J),CD(10,J),CD(11,J),CD(12,J))

*     compute the extra values for partitioned metals --------------------------
      if ( detype (J) .ge. 900 ) then ! dissolved and solid metal
      call compute statistics for plots of river quality ! dissolved metal
     &(2,J,CP1(1,J),CP1(2,J),CP1(3,J),CP1(4,J),CP1(5,J),CP1(6,J),
     &CP1(7,J),CP1(8,J),CP1(9,J),CP1(10,J),CP1(11,J),CP1(12,J))
      call compute statistics for plots of river quality ! solid metal
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


*     write out the data for graph plotting to file 22 and 230, 231 etc --------
*     if ( virtualreach .eq. -1 ) then
      if ( ical13 .eq. 0 ) then ! pppppppppppppppppppppppppppppppppppppppppppppp
      write(22,300)ireach,Length of main river-adist(ireach), ! end of reach SGR
     &' " End of Reach                               "',RNAME(IREACH),0
  300 format(I4,',',1PE11.4,',',A47,',"',A16,'",',i4)
      write(22,301)'[Flow    Calc]',FLOW(1),flow(3),FLOW(2),flow(4), ! ----- SGR
     &            '[Flow    Obsd]',(0,JCP=1,4) ! --------------------------- SGR          

      
      do j = 1, MP10 ! ppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
      if ( QTYPE(J) .ne. 4 ) then ! pppppppppppppppppppppppppppppppppppppppppppp
          
      write(22,302) '[Det',mod(j,10),'ConcCalc]',(CL(JCP,J),JCP=1,12) ! ---- SGR
      write(22,302) '[Det',mod(j,10),'LoadCalc]',(CD(JCP,J),JCP=1,12) ! ---- SGR
      write(22,303) '[Det',mod(j,10),'ConcObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,303) '[Det',mod(j,10),'LoadObsd]',(0,JCP=1,4) ! ------------- SGR
      write(22,304) '[Det',mod(j,10),'ConcTrgt]',RQO(J) ! ------------------ SGR
      write(22,304) '[Det',mod(j,10),'LoadTrgt]',0 ! ----------------------- SGR
  301 format(' ',A14,4(',',1PE11.4)/' ',A14,4(',',1PE11.4))
  302 format(1x,a4,I1,A9,12(',',1PE11.4))
  303 format(1x,a4,I1,A9,4(',',1PE11.4))
  304 format(1x,a4,I1,A9,1(',',1PE11.4))

      write(230+j,305)RNAME(IREACH),ireach, ! ----- end of reach -------- Gi.CSV
     &999,rlength(IREACH), ! -------------------------------------------- Gi.CSV
     &rlength(IREACH),adist(ireach)+rlength(IREACH), ! ------------------ Gi.CSV    
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Gi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Gi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Gi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Gi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop) ! --------------------------------- Gi.CSV
      write(240+j,305)RNAME(IREACH),ireach, ! ----- end of reach -------- Wi.CSV
     &999,rlength(IREACH), ! -------------------------------------------- Wi.CSV
     &rlength(IREACH),adist(ireach)+rlength(IREACH), ! ------------------ Wi.CSV    
     &(CL(JCP,J),JCP=1,12),(CD(JCP,J),JCP=1,12), ! ---------------------- Wi.CSV
     &FLOW(1),flow(3),FLOW(2),flow(4),RQO(J),confail(J), ! -------------- Wi.CSV
     &(in class(iclass,j),iclass=1,nclass), ! --------------------------- Wi.CSV
     &(LMcontrib(ip,J,1),ip=1,n2prop), ! -------------------------------- Wi.CSV
     &(CMcontrib(ip,J,1),ip=1,n2prop) ! --------------------------------- Wi.CSV
  305 format('End of Reach',',',A16,',',I4,',',i4, ! -------------------- Wi.CSV
     &200(',',1PE11.4)) ! ----------------------------------------------- Wi.CSV
      
      endif ! if ( QTYPE(J) .ne. 4 ) ppppppppppppppppppppppppppppppppppppppppppp
      enddo ! do j = 1, MP10 ppppppppppppppppppppppppppppppppppppppppppppppppppp
      endif ! if ( ical13 .eq. 0 ) ppppppppppppppppppppppppppppppppppppppppppppp
      
      if ( noGIS .eq. 0 ) then ! ===============================================
      if ( ical13 .eq. 0 ) then
      numtype =999
      End of Reach = "End of reach"

      place = 'at '

      write(42,244)ireach,GIScode last,RNAME(IREACH),place,End of Reach,
     &numtype,Length of main river-adist(ireach),funit,
     &FLOW(1),flow(3),FLOW(2),flow(4),
     &0.0,0.0,0.0,0.0
  244 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"River flow",',',A4,',',9(1PE11.4,','))
     
     
      do J=1,MP10 ! ============================================================
      write(42,245)ireach,GIScode last,RNAME(IREACH),place,End of Reach,
     &numtype,Length of main river-adist(ireach),
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
      write(42,845)ireach,GIScode last,RNAME(IREACH),place,End of Reach,
     &numtype,Length of main river-adist(ireach),
     &"Dissolved..","Part",
     &(CP1(jcp,J),jcp=1,12),(CD1(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
      write(42,845)ireach,GIScode last,RNAME(IREACH),place,End of Reach,
     &numtype,Length of main river-adist(ireach),
     &"Solid......","Part",
     &(CP2(jcp,J),jcp=1,12),(CD2(jcp,J),jcp=1,12),(xzero,jcp=1,7),
     &(xzero,ic=1,NC),(xzero,ic=1,NC),(xzero,jcp=1,3)
      endif ! dissolved and solid metal
      enddo ! loop on determinands =============================================

  845 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),
     &a11,',','"',A4,'",',12(1PE11.4,','),19(1PE11.4,','),
     &10(1PE11.4,','),"Total lengths",',',3(1PE11.4,','))

      write(42,247)ireach,GIScode last,RNAME(IREACH),place,End of Reach,
     &numtype,Length of main river-adist(ireach),
     &(conf in class (ic),ic=1,NC),
     &((in class (ic,J),ic=1,NC),qualn(J),J=1,MP10),Face Value,
     &(Face Value dets (J),J=1,MP10),model number in batch
  247 format(i4,',',' "',a40,'"',',','"',A16,'"',',',a3,',',A40,'",
     &',i4,',',(f10.4,','),"Confidence in class",',"Per cent",',
     &5(1PE11.4,','),10(6(1PE11.4,',')),11(i2,','),i4,',')

      endif ! if ( noGIS .eq. 0 ) ==============================================
      endif

      if ( ical13 .eq. 0 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( nobigout .le. 0 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( ICAL .eq. 0 .or. ICAL .eq. 2 .or. ICAL .eq. 4 ) then ! ==============
      write(72,2642)RNAME(IREACH),distp, ! -------------------------------- .FLO
     &FLOW(1),(flowstats(i),i=1,9),(propstats(i),i=1,9)
 2642 format('End of Reach - ',A16,13x,f8.2,10f9.1/
     &4x,'Proportions of effluent flow ...',25x,9f9.2)
      write(72,1384)
 1384 format(45('-'),2x,95('-'))
      else ! ===================================================================
      write(72,2647)RNAME(IREACH),distp, ! -------------------------------- .FLO
     &FLOW(1),(flowstats(i),i=1,9)
 2647 format('End of Reach - ',A16,13x,f8.2,10f9.1)
      write(72,1384)
      endif ! if ( ICAL .eq. 0 etc =============================================
      endif ! if ( nobigout .le. 0 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( ical13 .eq. 0 ) ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      LINE='                                '

      if ( output mode .eq. 0 ) NMORQ = 3
      if ( output mode .eq. 1 ) NMORQ = 1
      if ( output mode .eq. 2 ) NMORQ = 4
      if ( output mode .eq. 3 ) NMORQ = 5

      call set up output of calculated means or percentiles ! ----- end of reach
      
      call set up flowa ! ----------------------------------------- end of reach
      call set up flow95 ! ---------------------------------------- end of reach
      
      prugress1 = '       '
      prugress2 = '-------'
      prugress3 = ' comple'
      prugress4 = 'te     '
      xpru = 100.0*float(KFEAT)/float(MU)
      write(prugress2,5400)xpru
 5400 format(f6.1,'%')

*     write out an indication of confidence ====================================
      if ( output mode .ne. 1 ) then ! for percentile output mode --------------
      write(LINE,9895)RNAME(IREACH),Length of main river, ! End of Reach
     &Flowchars(2),(Sevenchars(jdet),jdet=1,ndet),prugress1,
     &prugress1,prugress1,prugress2,prugress3,prugress4
 9895 format(5X,'End of Reach: ',A16,F8.1,11A7,6A7)! 5,19,35,43,
      else ! or for mean output mode -------------------------------------------
      write(LINE,9895)RNAME(IREACH),Length of main river, ! End of Reach
     &Flowchars(1),(Sevenchars(jdet),jdet=1,ndet),prugress1,prugress1,
     &prugress1,prugress2,prugress3,prugress4
      do jdet = 1,ndet
      if ( qtype(jdet) .ne. 4 .and. ical. ne. 1 ) then
      write(170+jdet,7866)Length of main river, ! End of Reach ---------- Ci.GAP
     &Flowchars(1),funit
 7866 format(110('-')/'Length of reach:',F7.1,' km',7x, ! --------------- Ci.GAP
     &'Flow at end of reach:  Mean =   ',a7,1x,a4,17x,a16)
      write(170+jdet,7867)dname(jdet),Sevenchars(jdet), ! End of Reach -- Ci.GAP
     &units(jdet)
 7867 format(a11,19x,'Quality at end of reach:  Mean =',3x, ! ----------- Ci.GAP
     &a7,1x,a4/110('+'))
      endif ! if ( qtype(jdet) .ne. 4 .and. ical. ne. 1 )
      enddo ! do jdet = 1,ndet
      endif ! if ( output mode .ne. 1 ) ----------------------------------------
      
      call change colour of text (22) ! light blue
      if ( iscreen .ne. 3 ) write( *,7511)LINE ! ------------------ end of reach
      call set screen text colour
      write(09,7611)LINE ! -----------------------------------------end of reach
 7611 format(a107)
      write(33,7511)LINE ! ---------------------------------------- end of reach
 7511 format(A155)

*     write out an indication of confidence ====================================
      do idet = 1,ndet ! -------------------------------------------------------
      Sevenchars(idet) = '      -'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( C(idet,1) .gt. 1.0e-7 ) then
      write(Sevenchars(idet),2517)
     &nint(100.0*(CL(3,idet)-C(idet,1))/C(idet,1))
 2517 format(I6,'%')
      endif
      endif    
      enddo ! ------------------------------------------------------------------
      
      if ( ifbatch .eq. 0 ) then
      call change colour of text (12) ! orange
      if ( ical13 .eq. 0 ) write( *,2725)(Sevenchars(idet),idet=1,ndet)
      call set screen text colour
      endif
      if ( ical13 .eq. 0 ) write(09,2725)(Sevenchars(idet),idet=1,ndet)
      write(33,2725)(Sevenchars(idet),idet=1,ndet)
 2725 format(5X,'% Errors in mean',29x,10a7)
*     write out an indication of confidence =======Sewage effluents (3)=========
     
*     write out an indication of confidence ====================================
      if ( output mode .ne. 1 ) then
      write(LINE,1895)RNAME(IREACH),Length of main river, ! End of Reach
     &Flowchars(2),(Sevenchars(jdet),jdet=1,ndet),prugress1,prugress1,
     &prugress1,prugress2,prugress3,prugress4
 1895 format(5X,'End of Reach: ',A16,F8.1,17A7)! 5,19,35,43,
      else
      write(LINE,9895)RNAME(IREACH),Length of main river, ! End of Reach
     &Flowchars(1),(Sevenchars(jdet),jdet=1,ndet),prugress1,prugress1,
     &prugress1,prugress2,prugress3,prugress4
      endif

      call change colour of text (22) ! light blue
      !if ( iscreen .ne. 3 ) write( *,7511)LINE ------------------- end of Reach
      call set screen text colour
      !write(09,1511)LINE ! end of reach
      !write(33,1511)LINE ! end of reach
      !1511 format(A155)
*     write out an indication of confidence ====================================
      

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
      !if ( IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,3926)IREACH,RNAME(IREACH)
 3926 format(24X,53('=')/
     &24X,'CALCULATIONS COMPLETED FOR THIS REACH ...',i12/
     &24X,'Name of reach: ',A16/24X,53('='))
      endif
      !endif
      endif

*     calculate and output the percentage of loads from various sources
*     call outsum at end of reach
      return
      end



      subroutine details upstream of feature
      include 'COMMON DATA.FOR'

      call calculate summaries of river flow

*     if ( JT(feeture) .eq. 3 ) return
*     if ( JT(feeture) .eq. 5 ) return
      
      if ( nobigout .le. 0 ) then
      if ( DIST(feeture) .gt. 0.0001 ) then
      if ( KSIM .le. 1 ) write(01,1010) ! ---------------------------------- OUT
     &UNAME(feeture),feeture,IREACH,DIST(feeture),RNAME(IREACH) ! ---------- OUT
 1010 format(////110('=')/a40,4X,'Feature No',I6, ! ------------------------ OUT
     &3X,'Reach No',i6/110('-')/ ! ----------------------------------------- OUT
     &'Location:  ',f5.1,' km downstream from the head ', ! ---------------- OUT
     &'of the Reach called ...... ',a16/110('-')) ! ------------------------ OUT
      endif ! if ( DIST(feeture) .gt. 0.0001 )
      endif ! if ( nobigout .le. 0 )
      
      if ( nobigout .le. 0 ) then  ! =========== output for natural purification 
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! =============================
      if ( ical .ne. 2 ) then ! ================================================
      do jper = 1,ndet ! =======================================================
      if ( QTYPE(jper).ne. 4 ) then ! ==========================================
      if ( KSIM .eq. 1 .and. DIST(feeture) .gt. 0.001 ) then ! -----------------
      write(170+jper,1710) ! output for natural purification ------------ Ci.GAP
     &UNAME(feeture),feeture,IREACH,DIST(feeture),RNAME(IREACH) ! ------- Ci.GAP
 1710 format(////110('=')/a40,4X,'Feature No',I6, ! --------------------- Ci.GAP
     &3X,'Reach No',i6/110('-')/ ! -------------------------------------- Ci.GAP
     &'Location:  ',f5.1,' km downstream from the head ', ! ------------- Ci.GAP
     &'of the Reach called ...... ',a16/110('-')) ! --------------------- Ci.GAP
      endif ! if ( KSIM .le. 1 .and. DIST(feeture) .gt. 0.001 ) ----------------
      endif ! if ( QTYPE(jper).ne. 4 ) =========================================
      enddo ! do jper = 1,ndet =================================================
      endif ! if ( ical. .ne. 2 ) ! ============================================
      endif ! if ( ical .eq. 0 .or. ical .eq. 3 ) ==============================
      endif ! if ( nobigout .le. 0 )  ========== output for natural purification

      if ( nobigout .le. 0 ) then ! ============================================
      call write flow in river just upstream
      if ( MONF .gt. 1 ) call write shots for river flow ! at feature
          
      if ( ical13 .eq. 0 ) then ! ==============================================
*     apply to (3) (5) etc --------------------- setting permit limits in mode 7
      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or.
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
      !write(31,2910)UNAME(feeture),feeture,IREACH,DIST(feeture), ! -------- EFF
      !&RNAME(IREACH),UNAME(feeture) ! ------------------------------------- EFF
 2910 format(/110('-')/a40,4X,'Feature No',I6,
     &3X,'Reach No',i6/110('-')/
     &'Location:  ',f5.1,' km downstream from the head ', ! ---------------- EFF
     &'of the Reach called ',a16/!110('-')/
     &'River quality upstream of the discharge ... ',a30/
     &110('-'))
      !do jper = 1,ndet
      !if ( qtype(jper).ne.4 ) write(150+jper,2910)UNAME(feeture), ! ----- Di EFF
      !&feeture,IREACH,DIST(feeture),RNAME(IREACH)!,UNAME(feeture)
      !enddo
      !do jper = 1,ndet
      !if ( qtype(jper).ne.4 ) write(170+jper,2910)UNAME(feeture), ! ----- Ci.GAP
      !&feeture,IREACH,DIST(feeture),RNAME(IREACH),UNAME(feeture)
      !enddo
      endif ! if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 etc ! --------
      endif ! if ( ical13 .eq. 0 ) then ! ======================================
      endif ! if ( nobigout .le. 0 ) =========================================== 

      !call write flow in river just upstream
      !if ( MONF .gt. 1 ) call write shots for river flow

      return
      end


      
      
      subroutine write flow in river just upstream
      include 'COMMON DATA.FOR'
      
      !if ( dist(feeture). gt. 0.00001 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( nobigout .le. 0 ) then ! ============================================

      if ( ical13 .eq. 0 ) then ! ==============================================
      call sort format 2 (Flow (1),Flow(2))
      write(01,1)valchars10,FUNIT,valchars11,FUNIT

      if ( JT (feeture) .eq. 03 .or. JT (feeture) .eq. 05 .or. ! ---------------
     &     JT (feeture) .eq. 12 .or. JT (feeture) .eq. 39 .or.
     &     JT (feeture) .eq. 60 .or. JT (feeture) .eq. 61 .or.
     &     JT (feeture) .eq. 62 ) then
          
      write(31,1910)UNAME(feeture),feeture, ! ------------------------------ EFF
     &IREACH,DIST(feeture),RNAME(IREACH) ! --------------------------------- EFF
 1910 format(///110('=')/A40,4X,'Feature No',I6,
     &3X,'Reach No',I6/110('-')/
     &'Location:  ',F5.1,' km downstream from the head ', ! ---------------- EFF
     &'of the Reach called ',A16/110('-'))

      write(31,1)valchars10,FUNIT,valchars11,FUNIT ! ----------------------- EFF
    1 format(/110('-')/'Flow in river just upstream ...',25X, ! ------------ EFF
     &'Mean =',a10,1x,A4/46X,'95% exceedence =',a10,1x,A4) ! --------------- EFF
      write(31,3) ! -------------------------------------------------------- EFF
    3 format(110('-'))
      
      do JP = 1,ndet ! ! ------------------------------------------------ Di EFF
      if ( qtype(JP) .ne. 4 ) then ! ------------------------------------ Di EFF
      write(150+JP,1)valchars10,FUNIT,valchars11,FUNIT ! ---------------- Di EFF
      write(150+JP,3) ! ------------------------------------------------- Di EFF
         
*     write the data and results to the CSV files VVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      if ( ifeffcsv . eq. 1 ) then ! ==================================== Ei.CSV
      write(130+JP,7500)GIScode(feeture),uname(feeture), ! u/s river flow Ei.CSV
     &rname(ireach),Flow(1),Flow(2) ! ----------------------------------- Ei.CSV
 7500 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Ei.CSV
     &'Upstream river flow',',',(',',1pe12.4),(',,',1pe12.4),',', ! ----- Ei.CSV
     &'05-percentile') !  u/s river flow -------------------------------- Ei.CSV
      
      ipc = 3
      ipl = 95
      if ( detype(JP) .lt. 900 ) then ! ========================================
      if ( MRQS(JP) .ne. 4 .and. MRQS(JP) .ne. 5 ) then ! ======================
      if ( MRQS(JP) .eq. 3 ) then ! 90-percentile
      ipl = kp95targ
      ipc = 3
      endif
      if ( MRQS(JP) .eq. 3 ) then ! 90-percentile
      ipl = 90
      ipc = 4
      endif
      if ( MRQS(JP) .eq. 6 ) then ! 99-percentile
      ipl = 99
      ipc = 5
      endif

      write(130+JP,7501)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,ipc),ipl, ! ---------- Ei.CSV
     &JT(KFEAT),uname(feeture) ! ---------------------------------------- Ei.CSV
 7501 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Ei.CSV
     &'Upstream river quality',',',a11,3(',',1pe12.4),',', ! ------------ Ei.CSV
     &i2,'-percentile',(',',i4),',',a35) ! ------------ u/s quality ----- Ei.CSV
      else ! if ( MRQS(JP) .ne. 4 .and. MRQS(JP) .ne. 5 ) ======================
      if ( MRQS(JP) .eq. 4 ) ! =================================================
     &write(130+JP,7601)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,3), ! ---------------- Ei.CSV
     &JT(KFEAT),uname(feeture) ! ---------------------------------------- Ei.CSV
 7601 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Ei.CSV
     &'Upstream river quality',',',a11,3(',',1pe12.4),',', ! ------------ Ei.CSV
     &'05-percentile',(',',i4),',',a35) ! ------------ u/s quality ------ Ei.CSV
      if ( MRQS(JP) .eq. 5 ) ! =================================================
     &write(130+JP,7701)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,4), ! ---------------- Ei.CSV
     &JT(KFEAT),uname(feeture) ! ---------------------------------------- Ei.CSV
 7701 format(' ',a40,',',a40,',',a16,',', ! ----------------------------- Ei.CSV
     &'Upstream river quality',',',a11,3(',',1pe12.4),',', ! ------------ Ei.CSV
     &'10-percentile',(',',i4),',',a35) ! ------------ u/s quality ------ Ei.CSV        
      endif ! if ( MRQO(JP) .ne. 4 .and. MRQO(JP) .ne. 5 ) ============== E1.CSV 
      endif ! if ( detype(JP) .lt. 909 ) ================================ Ei.CSV
      
      if ( detype(JP) .eq. 909 ) then ! the target is dissolved not total ======
      ipl = 95
      ipc = 3
      if ( MRQS(JP) .eq. 3 ) then ! 90-percentile
      ipl = 90
      ipc = 4
      endif
      if ( MRQS(JP) .eq. 6 ) then ! 99-percentile
      ipl = 99
      ipc = 5
      endif    
      
      write(130+JP,7511)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),cpart1(JP,1),cpart1(JP,2),
     &cpart1(JP,ipc),ipl,JT(KFEAT),uname(feeture)
 7511 format(' ',a40,',',a40,',',a16,',',
     &'Upstream river quality',',',a11,' (dissolved)',3(',', ! ---------- Ei.CSV
     &1pe12.4),',', ! --------------------------------------------------- Ei.CSV
     &i2,'-percentile',(',',i4),',',a35) ! ------------ u/s quality ----- Ei.CSV          
      write(130+JP,7512)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,ipc),ipl,
     &JT(KFEAT),uname(feeture)
      endif ! if ( detype(JP) .eq. 909 ) ================================ Ei.CSV
 
      if ( detype(JP) .eq. 900 ) then ! the target is total not dissolved ======
      ipl = 95
      ipc = 3
      if ( MRQS(JP) .eq. 3 ) then ! 90-percentile
      ipl = 90
      ipc = 4
      endif
      if ( MRQS(JP) .eq. 6 ) then ! 99-percentile
      ipl = 99
      ipc = 5
      endif    
      write(130+JP,7512)GIScode(feeture),uname(feeture), ! u/s quality -- Ei.CSV
     &rname(ireach),dname(JP),C(JP,1),C(JP,2),C(JP,ipc),ipl,
     &JT(KFEAT),uname(feeture)
 7512 format(' ',a40,',',a40,',',a16,',',
     &'Upstream river quality',',',a11,' (total)',3(',', ! -------------- Ei.CSV
     &1pe12.4),',', ! --------------------------------------------------- Ei.CSV
     &i2,'-percentile',(',',i4),',',a35) ! ------------ u/s quality ----- Ei.CSV          
      endif ! if ( detype(JP) .eq. 900 ) ================================ Ei.CSV

      endif ! if ( ifeffcsv . eq. 1 ) =================================== Ei.CSV

      IQ = JQ(KFEAT) ! 
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
      write(130+JP,7502)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),EFM,EFS,JT(KFEAT),uname(feeture)
 7502 format(' ',a40,',',a40,',',a16,',','Effluent flow', ! ------------- Ei.CSV
     &',',2(',',1pe12.4),',,,',i4,',',a35) ! ---------------------------- Ei.CSV
     
      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
      
      if ( detype(JP) .le. 899) then ! +++++++++++++++++++++++++++++++++++++++++
      write(130+JP,7503)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECS,JT(KFEAT),uname(feeture)
 7503 format(' ',a40,',',a40,',',a16,',',
     &'Current effluent quality',',',a11,2(',',1pe12.4),',,', ! --------- Ei.CSV
     &(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
      endif ! if ( detype(JP) .le. 899) ++++++++++++++++++++++++++++++++++++++++

      if ( detype(JP) .gt. 899) then ! +++++++++++++++++++++++++++++++++++++++++
      write(130+JP,7533)GIScode(feeture),unamex, ! ---------------------- Ei.CSV
     &rname(ireach),dname(JP),ECM,ECS,JT(KFEAT),uname(feeture)
 7533 format(' ',a40,',',a40,',',a16,',',
     &'Current effluent quality',',',a11,' (total)',2(',',
     &1pe12.4),',,', ! --------- Ei.CSV
     &(',',i4),',',a35) ! ----------------------------------------------- Ei.CSV
      endif ! if ( detype(JP) .gt. 899) ++++++++++++++++++++++++++++++++++++++++

*     VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
     
      endif ! if ( qtype(JP) .ne. 4 ) ----------------------------------- Di EFF
      enddo ! do JP = 1,ndet ! do JP = 1,ndet ! ------------------------- Di EFF
      endif ! if ( JT (feeture) .eq. 3 etc -------------------------------------

      
      call sort format 2 (Flow(3),Flow(4))
      write(01,2)valchars10,FUNIT,valchars11,FUNIT
    2 format(
     &46x,'90% exceedence =',a10,1x,A4/
     &46x,'99% exceedence =',a10,1x,A4/110('-'))
      endif ! if ( ical13 .eq. 0 ) =============================================

      if ( ical .eq. 3 .or. ical .eq. 4 ) then ! ======================== Ci.GAP
      if (JT (feeture) .ne. 4 ) then ! ================================== Ci.GAP
      call sort format 2 (Flow (1),Flow(2))
      do jper = 1,ndet ! ------------------------------------------------ Ci.GAP
      if ( qtype(jper) .ne. 4 .and. KSIM .eq. 1 ) then ! ---------------- Ci GAP
      write(170+jper,11)valchars10,FUNIT, ! ----------------------------- Ci.GAP
     &valchars11,FUNIT ! ------------------------------------------------ Ci.GAP
      endif ! if ( qtype(jper) .ne. 4 etc ------------------------------- Ci.GAP
      enddo ! do jper = 1,ndet ------------------------------------------ Ci.GAP
   11 format('Flow in river just upstream ...',25X,'Mean =',a10, ! ------ Ci.GAP
     &1x,A4/46X,'95% exceedence =',a10,1x,A4/110('-'))
      do jdet = 1, ndet ! ----------------------------------------------- Ci GAP
      if ( qtype(jdet) .ne. 4 .and. KSIM .eq. 1 ) then ! ---------------- Ci.GAP
      call sort format 1 (C(jdet,1)) 
      write(170+jdet,4411)dname(jdet),valchars10,UNITS(jdet) ! ---------- Ci.GAP
 4411 format(
     &'Quality in river just upstream ... ',a11,10x,'Mean =',a10,1x,a4
     &/110('-'))
      endif ! if ( qtype(jdet) .ne. 4 .and. KSIM .eq. 1 ) --------------- Ci.GAP
      enddo ! do jdet = 1, ndet ! --------------------------------------- Ci GAP
      endif ! if (JT (feeture) .ne. 4 ) ================================= Ci.GAP
      endif ! if ( ical .eq. 3 ) ======================================== Ci.GAP
            
      endif ! if ( nobigout .le. 0 ) ===========================================
      !endif ! if ( dist(feature). gt. 0.00001 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      return
      end

      
      subroutine write out loads at the end of the reach
      include 'COMMON DATA.FOR'
      character * 10 Tenchars(MP10)

      call prepare loads for print out (Tenchars)
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(27,1)RNAME(IREACH),Length of main river, ! --------------------- LOD
     &(Tenchars(jdet),jdet=1,ndet)
    1 format(140('=')/'End of Reach: ',A16,F12.1,6x,10A10)
      write(27,5)
    5 format(140('='))
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ical13 .eq. 0 )

      call write loads at this point in the catchment (3) ! at end of the reach
      if ( nobigout .le. 0 ) call proportion of effluent end 

      if ( kountworks .gt. 0 ) then
      jxtype = 999
      if ( ical13 .eq. 0 ) then
      call proportion of works and diffuse (3) ! at the end of the reach
      endif
      endif

      if ( kount bodies .gt. 0 ) then
      jxtype = 999
      call proportion of catchments (3) ! at the end of the reach
      endif

      return
      end

      
      
      
*     write out the river loads ... to channel 27 ------------------------------
*     write out the percentage of loads from various sources ...  channel 40, 41
      subroutine write out the river loads
      include 'COMMON DATA.FOR'
      
*     skip this routine if the feature is a headwaters of the reach ------------
*     or input from a lake to the start of a reach -----------------------------
      if ( JT (feeture) .eq. 10 .or. JT (feeture) .eq. 45 ) return ! loads
*     skip this routine if the distance is zero --------------------------------
*     unless the feature is a sub-catchment boundary ---------------------------

      if ( ical13 .eq. 0 ) then
*     calculate and output the percentage of loads from various sources --------
      call outsum at this feature
      endif
      
      return
      end
      
      
      

*     write out the river loads after natural purification ....
      subroutine write out the river loads after natural purification
      include 'COMMON DATA.FOR'
      character * 10 Tenchars(MP10)
      
      call prepare loads for print out (Tenchars)
      call format the loads for output 4 (Tenchars) 
      
      return
      end

      
      
      
*     write out the river loads after diffuse sources --------------------------
      subroutine write out the river loads after diffuse sources
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
      character * 10 Tenchars(MP10)

      call prepare loads for print out (Tenchars)
      call format the loads for output 4 (Tenchars) 
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( distp .gt. 1.0e-9 ) then
      write(27,1)Distp
    1 format('Loads added to the river over the next',f6.1,
     &' kilometres'/140('-'))
      write(27,5)(Tenchars(jdet),jdet=1,ndet)
    5 format(5X,'Load before adding diffuse sources',13x,10A10)
      endif
      endif
      endif
      return
      end

*     write out the river loads after diffuse sources --------------------------
      subroutine write diffuse effluent load
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
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

      
      subroutine write loads after gap filling of river quality A
      include 'COMMON DATA.FOR'
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

      
      subroutine write loads after gap filling of river quality B
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
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
      include 'COMMON DATA.FOR'
      character *10 Tenchars1(MP10)

      do jdet = 1, MP10
      Tenchars1(jdet) ='          '
      enddo

      do 1 jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE(jdet) .ne. 4 ) then
      Tenchars1(jdet) ='       0.0'
      wload = check sub total (jdet)

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
      include 'COMMON DATA.FOR'
      
      call write loads at this point in the catchment (9) ! at end of the run --

      if ( kountworks  .gt. 0 ) then
      jxtype = 999
      if ( ical13 .eq. 0 ) then
      call proportion of works and diffuse (9) ! end of the run
      endif
      endif

      if ( kount bodies .gt. 0 ) then
      call proportion of catchments (9) ! at the end of the run
      endif
      
      call add up all the loads ! for all determinands

      return
      end

      
      
      
*     sum the loads added to the river -----------------------------------------
      subroutine add up all the loads ! for all determinands
      include 'COMMON DATA.FOR'

      do i = 1, ndet ! =========================================================
      if ( qtype (i) .ne. 4 ) then ! ===========================================
      nx = n13 ! this equals 13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ----------------------------------------------------------     
      TLOSSES2(i,J1) = TBLODE2 (i,J1) ! net total load removed by abstractions
     &               + TALOADDN2 (i,J1) ! removed by gap filling on quality
     &               + TILOADDN2 (i,J1) ! removed by gap filling for flows 
     &               + TNLOADDN2 (i,J1) ! removed by natural purification
      enddo ! do J1 = 1, nx ! --------------------------------------------------

      xxLM = 0.0
      do is = 1, NS
      xxLM = xxLM + FMS(IS)*CMS(i,IS)  
      enddo ! do is = 1, NS
      LMcontrib(NTD,i,1) = xxLM/float(NS) 
      
      endif ! if ( qtype (i) .ne. 4 ) ! ========================================
      enddo ! do i = 1, ndet ! =================================================

      return
      end
      
      

*     calculate and output the percentage of loads from various sources
      subroutine outsum at this feature
      include 'COMMON DATA.FOR'

      call write loads at this point in the catchment (1) ! at this feature ....

      if ( nobigout .le. 0 ) call proportion of effluent (0)
      
      jxtype = jt(feeture)
          
      call proportion of works and diffuse (1) ! at this feature
      
      if ( kount bodies .gt. 0 .and. JT(feeture) .eq. 24 ) then ! ==============
      call proportion of catchments (1) ! at this feature

      do ip = 1,n2prop
      nbodyprop(ip) = 0 ! number of feature type in a current water body
      enddo

      endif ! if ( kount bodies .gt. 0 .and. JT(feeture) .eq. 24 ) =============
     
      return
      end
      
      

*     calculate and output the percentage of loads from various sources --------
      subroutine outsum at start of reach 
      include 'COMMON DATA.FOR'      

      call write loads at this point in the catchment (0) ! at start of reach --

      if ( nobigout .le. 0 ) call proportion of effluent start 
      !if ( kountworks .gt. 0 ) then
      jxtype = 111

      if ( ical13 .eq. 0 ) then
      call proportion of works and diffuse (0) ! at the start of the reach
      endif ! if ( ical13 .eq. 0 )
      !endif ! if ( kountworks .gt. 0 )
      
      if ( kount bodies .gt. 0 ) then
      call proportion of catchments (0) ! at the start of the reach
      endif

      return
      end




*     calculate and output the percentage of loads from various sources at the -
*     end of the reach and at the end of the modelled catchment ----------------
      subroutine write loads at this point in the catchment (kdirect)
      include 'COMMON DATA.FOR'
      character *10 Tenchars1(MP10)
      
      call add up all the loads ! for all determinands

*     calculate the proportions of load ========================================
      do idet = 1, ndet ! ======================================================
      if ( QTYPE (idet) .ne. 4 ) then ! ========================================
*     initialise the values of the proportions of load -------------------------
      prop10    (idet) = 0.0
      propRT    (idet) = 0.0
      propNPU   (idet) = 0.0
      propeff2  (idet) = 0.0
      propeff3  (idet) = 0.0
      propeff12 (idet) = 0.0
      propeff5  (idet) = 0.0
      propeff39 (idet) = 0.0
      propeff60 (idet) = 0.0
      propeff61 (idet) = 0.0
      propeff62 (idet) = 0.0
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
      prop50    (idet) = 0.0
      prop52    (idet) = 0.0
      prop54    (idet) = 0.0
      prop56    (idet) = 0.0
      prop58    (idet) = 0.0
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
      if ( LMcontrib(NTD,idet,1) .gt. 0.0 ) then
      XLMcont = LMcontrib(NTD,idet,1) + TNLOADUP2(idet,i13)
      prop10    (idet) = 100.0 * LMcontrib(17,idet,1)/XLMcont
      propRT    (idet) = 100.0 * LMcontrib(20,idet,1)/XLMcont
      propNPU   (idet) = 100.0 * TNLOADUP2(idet,i13)/XLMcont 
      propfrl   (idet) = 100.0 * LMcontrib(21,idet,1)/XLMcont
      propqrl   (idet) = 100.0 * LMcontrib(22,idet,1)/XLMcont
      propeff2  (idet) = 100.0 * LMcontrib(01,idet,1)/XLMcont ! total effluents
      propeff3  (idet) = 100.0 * LMcontrib(02,idet,1)/XLMcont ! sewage works
      propeff12 (idet) = 100.0 * LMcontrib(03,idet,1)/XLMcont ! intermittent
      propeff5  (idet) = 100.0 * LMcontrib(04,idet,1)/XLMcont ! industrial
      propeff39 (idet) = 100.0 * LMcontrib(05,idet,1)/XLMcont ! mine waters
      propeff60 (idet) = 100.0 * LMcontrib(28,idet,1)/XLMcont ! others
      propeff61 (idet) = 100.0 * LMcontrib(29,idet,1)/XLMcont ! private works
      propeff62 (idet) = 100.0 * LMcontrib(30,idet,1)/XLMcont ! fish farms
      prop13    (idet) = 100.0 * LMcontrib(18,idet,1)/XLMcont
      prop15    (idet) = 100.0 * LMcontrib(19,idet,1)/XLMcont
      prop42    (idet) = 100.0 * LMcontrib(14,idet,1)/XLMcont
      prop25    (idet) = 100.0 * LMcontrib(06,idet,1)/XLMcont
      prop27    (idet) = 100.0 * LMcontrib(07,idet,1)/XLMcont
      prop29    (idet) = 100.0 * LMcontrib(08,idet,1)/XLMcont
      prop31    (idet) = 100.0 * LMcontrib(09,idet,1)/XLMcont
      prop33    (idet) = 100.0 * LMcontrib(10,idet,1)/XLMcont
      prop35    (idet) = 100.0 * LMcontrib(11,idet,1)/XLMcont
      prop46    (idet) = 100.0 * LMcontrib(15,idet,1)/XLMcont
      prop48    (idet) = 100.0 * LMcontrib(16,idet,1)/XLMcont
      prop50    (idet) = 100.0 * LMcontrib(23,idet,1)/XLMcont
      prop52    (idet) = 100.0 * LMcontrib(24,idet,1)/XLMcont
      prop54    (idet) = 100.0 * LMcontrib(25,idet,1)/XLMcont
      prop56    (idet) = 100.0 * LMcontrib(26,idet,1)/XLMcont
      prop58    (idet) = 100.0 * LMcontrib(27,idet,1)/XLMcont
      prop37    (idet) = 100.0 * LMcontrib(12,idet,1)/XLMcont
      prop40    (idet) = 100.0 * LMcontrib(13,idet,1)/XLMcont

 
*     sum the proportions of load ----------------------------------------------
      propall (idet) = prop10(idet) ! 24 additions
     &               + propRT(idet)
     &               + propNPU(idet)
     &               + propfrl(idet)
     &               + propqrl(idet)
     &               + propeff2(idet) ! total efflients
     &               + prop13(idet)
     &               + prop15(idet)
     &               + prop42(idet)
     &               + prop25(idet)
     &               + prop27(idet)
     &               + prop29(idet)
     &               + prop31(idet)
     &               + prop33(idet)
     &               + prop35(idet)
     &               + prop46(idet)
     &               + prop48(idet)
     &               + prop50(idet)
     &               + prop52(idet)
     &               + prop54(idet)
     &               + prop56(idet)
     &               + prop58(idet)
     &               + prop37(idet)
     &               + prop40(idet)


      propall(idet) = 0.0
      propall(idet) = propall(idet)+ prop10(idet)
      propall(idet) = propall(idet)+ propRT(idet)
      propall(idet) = propall(idet)+ propNPU(idet)
      propall(idet) = propall(idet)+ propfrl(idet)
      propall(idet) = propall(idet)+ propqrl(idet)
      propall(idet) = propall(idet)+ propeff2(idet) ! total effluents
      propall(idet) = propall(idet)+ prop13(idet)
      propall(idet) = propall(idet)+ prop15(idet)
      propall(idet) = propall(idet)+ prop42(idet)
      propall(idet) = propall(idet)+ prop25(idet)
      propall(idet) = propall(idet)+ prop27(idet)
      propall(idet) = propall(idet)+ prop29(idet)
      propall(idet) = propall(idet)+ prop31(idet)
      propall(idet) = propall(idet)+ prop33(idet)
      propall(idet) = propall(idet)+ prop35(idet)
      propall(idet) = propall(idet)+ prop46(idet)
      propall(idet) = propall(idet)+ prop48(idet)
      propall(idet) = propall(idet)+ prop50(idet)
      propall(idet) = propall(idet)+ prop52(idet)
      propall(idet) = propall(idet)+ prop54(idet)
      propall(idet) = propall(idet)+ prop56(idet)
      propall(idet) = propall(idet)+ prop58(idet)
      propall(idet) = propall(idet)+ prop37(idet)
      propall(idet) = propall(idet)+ prop40(idet)

      propabs   (idet) = 100.0 * TBLODE2  (idet,i13)/
     &                           LMcontrib(NTD,idet,1)
      propNPD   (idet) = 100.0 * TNLOADDN2(idet,i13)/
     &                           LMcontrib(NTD,idet,1)
      propfra   (idet) = 100.0 * TILOADDN2(idet,i13)/
     &                           LMcontrib(NTD,idet,1)
      propqra   (idet) = 100.0 * TALOADDN2(idet,i13)/
     &                           LMcontrib(NTD,idet,1)
      prolosses (idet) = propNPD (idet) + propfra (idet) 
     &                 + propqra (idet) + propabs (idet) 
      proadds (idet) = propfrl (idet) 
     &               + propqrl (idet) 

      endif
      endif ! if ( QTYPE (idet) .ne. 4 ) =======================================
      enddo ! calculate the proportions of load ================================

      if ( ical .eq. 1 .or. ical .eq. 3 ) return  
      if ( nobigout .gt. 0 ) return

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = dna(idet)
      endif
      enddo

*     write the headings =======================================================
      if ( kdirect .eq. 0 ) then ! the start of the reach ----------------------
      write(27,8952)rname(ireach),(Tenchars1(idet),idet = 1, ndet) ! ------- LOD
 8952 format(////140('=')/'Summary and origins of net ', ! ---- start of a reach
     &'total loads at the start of the reach ... ',a16/140('-')/
     &48x,10(6x,a4))
      write(27,8953)(lunits(idet),idet = 1, ndet)
 8953 format(48x,10(6x,a4))
      write(27,1020)
 1020 format(140('='))
      endif ! the start of the reach -------------------------------------------
      
      if ( kdirect .eq. 1 ) then ! other points in the reach -------------------
      !if ( DIST(feeture) .lt. 1.0e-08 ) then
      notype = JT(feeture)
      if ( notype .gt. 24 .and. notype .lt. 39) return
      if ( notype .gt. 39 .and. notype .lt. 42) return
      if ( notype .gt. 45 .and. notype .lt. 60) return
      !endif

      write(27,8939)rname(ireach),uname(feeture), ! ------------------------ LOD
     &(Tenchars1(idet),idet = 1, ndet)
 8939 format(////140('=')/'Summary and origins of net ', ! -------- at a feature
     &'total loads at this point in reach: ',a16,' .... at: ',a37/
     &140('-')/48x,10(6x,a4))
      write(27,8953)(lunits(idet),idet = 1, ndet)
      write(27,1020)
      endif ! other points in of the reach -------------------------------------
     
      if ( kdirect .eq. 3 ) then ! the end of the reach ------------------------
      write(27,8932)rname(ireach),(Tenchars1(idet),idet = 1, ndet) ! ------- LOD
 8932 format(////140('=')/'Summary and origins of net ', ! ------ end of a reach
     &'total loads at the end of the reach ... ',a16/140('-')/
     &48x,10(6x,a4))
      write(27,8953)(lunits(idet),idet = 1, ndet)
      write(27,1020)
      endif ! the end of the reach ---------------------------------------------

      if ( kdirect .eq. 9 ) then ! the end of the entire model -----------------
      write(27,8962),(Tenchars1(idet),idet = 1, ndet) ! -------------------- LOD
 8962 format(////140('-')/'Summary and origins of net ', ! ---- end of the model 
     &'total loads at the end of the modelled catchment ...'/140('-')/
     &48x,10(6x,a4))
      write(27,8953)(lunits(idet),idet = 1, ndet)
      write(27,1020)
      endif ! the end of the entire model --------------------------------------
*     =========================================================== write headings 

      kp = 0 ! net total loads from boundaries and tributaries -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(17,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(17,jdet,1)) ! --------------------------- LOD
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,3930)(Tenchars1(jdet),jdet=1,ndet) ! ------- LOD
 3930 format('+',2x,'Boundaries and tributaries (2 and 10)',
     &8x,10A10)
      
      kp = 0 ! net total loads from Reach-type diffuse sources ------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(20,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(20,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4930)(Tenchars1(jdet),jdet=1,ndet)
 4930 format('+',2x,'Reach-type diffuse sources',19x,10A10)
      
      kp = 0 ! loads added by natural purification -----------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (TNLOADUP2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (TNLOADUP2(jdet,i13))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8931)(Tenchars1(jdet),jdet=1,ndet) ! ------- LOD
 8931 format('+',2x,'Added by natural purification',16X,10A10) ! ----------- LOD
     

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! loads introduced by gap filling for river flows -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(21,jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(21,jdet,i13))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,2943)(Tenchars1(jdet), jdet=1,ndet)
 2943 format('+',2x,'Added by gap filling of flow',17x,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      kp = 0 ! loads introduced by gap filling of river quality ----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(22,jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(22,jdet,i13))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,2944)(Tenchars1(jdet), jdet=1,ndet)
 2944 format('+',2x,'Added by gap filling of quality',14X,10A10)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      
      kp = 0 ! net loads introduced by all discharges of effluent --------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(01,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(01,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
*     if ( kp .eq. 1 ) write(27,8933)(Tenchars1(jdet), jdet=1,ndet)
*8933 format(5X,'Net from discharges (types 3 12 5 39 etc)',6x,10A10)
*     if ( kp .eq. 1 ) write(27,8972)
*8972 format(5x,'(Net of upstream losses ...)')
*     if ( kp .eq. 1 ) write(27,4826)
 4826 format(140('='))

      kp = 0  ! net loads from sewage effluents (3) ----------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(02,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(02,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4733)(Tenchars1(jdet),jdet=1,ndet)
 4733 format('+',2x,'Net total from sewage effluents (3)',10x,10A10)
      
      kp = 0 ! net loads from intermittent discharges of sewage (12) -----------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(03,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(03,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4739)(Tenchars1(jdet),jdet=1,ndet)
 4739 format('+',2x,'Net intermittent discharges of sewage (12)   ',
     &10A10)
      
      kp = 0 ! net loads from industrial discharges (05) -----------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(04,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(04,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,5933)(Tenchars1(jdet),jdet=1,ndet)
 5933 format('+',2x,'Net total from industrial discharges (5)',5x,10A10)
      
      kp = 0 ! net loads from mine waters (39) ---------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(05,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(05,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8039)(Tenchars1(jdet),jdet=1,ndet)
 8039 format('+',2x,'Net total from mine waters (39)',14x,10A10)
      
      kp = 0 ! net loads from Other Point Sources (60) -------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(28,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(28,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8639)(Tenchars1(jdet),jdet=1,ndet)
 8639 format('+',2x,'Net total from "Other" point sources (60)',
     &4x,10A10)

      kp = 0 ! net loads from private wastewaters waters (61) ------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(29,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(29,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8739)(Tenchars1(jdet),jdet=1,ndet)
 8739 format('+',2x,'Net total from private wastewaters (61)',6x,10A10)

      kp = 0 ! net loads from fish farms (62) ----------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(30,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(30,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,6239)(Tenchars1(jdet),jdet=1,ndet)
 6239 format('+',2x,'Net total from fish farms (62)         ',6x,10A10)

      kp = 0 ! net loads from River-type diffuse pollution (13) -----------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(18,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(18,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8113)(Tenchars1(jdet),jdet=1,ndet)
 8113 format('+',2x,'River-type diffuse pollution (13)',12X,10A10)
      
      kp = 0 ! net loads from Effluent-type diffuse pollution (15) -------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(19,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(19,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8233)(Tenchars1(jdet),jdet=1,ndet)
 8233 format('+',2x,'Effluent-type diffuse pollution (15)',9X,10A10)
      
      kp = 0 ! net loads from Diffuse pollution from livestock (25) ------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(06,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(06,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8125)(Tenchars1(jdet),jdet=1,ndet)
 8125 format('+',2x,'Diffuse pollution from livestock (25)',8X,10A10)
      
      kp = 0 ! net loads from Diffuse pollution from arable (27) ---------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(07,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(07,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8127)(Tenchars1(jdet),jdet=1,ndet)
 8127 format('+',2x,'Diffuse pollution from arable (27)',11X,10A10)
      
      kp = 0 ! net loads from Highway runoff (29) ------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(08,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(08,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8129)(Tenchars1(jdet),jdet=1,ndet)
 8129 format('+',2x,'Highway runoff (29)',26X,10A10)
      
      kp = 0 ! net loads from 'Urban runoff (31) -------------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(09,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(09,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8131)(Tenchars1(jdet),jdet=1,ndet)
 8131 format('+',2x,'Urban runoff (31)',28X,10A10)
      
      kp = 0 ! net loads from Atmospheric deposition (33) ----------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(10,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(10,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8133)(Tenchars1(jdet),jdet=1,ndet)
 8133 format('+',2x,'Atmospheric deposition (33)',18x,10A10)
      
      kp = 0 ! net loads from natural background (35) --------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(11,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(11,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8135)(Tenchars1(jdet),jdet=1,ndet)
 8135 format('+',2x,'Natural background (35)',22x,10A10)
      
      kp = 0 ! net total loads from Aggregated STWs (42) -----------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(14,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(14,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8142)(Tenchars1(jdet),jdet=1,ndet)
 8142 format('+',2x,'Aggregated STWs (42)',25x,10A10)

      kp = 0 ! net total loads from diffuse mines (46) -------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(15,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(15,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8146)(Tenchars1(jdet),jdet=1,ndet)
 8146 format('+',2x,'Diffuse mines (46)',27x,10A10)
      
      kp = 0 ! net total loads from birds, boats and angling (48) --------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(16,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(16,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8148)(Tenchars1(jdet),jdet=1,ndet)
 8148 format('+',2x,'Birds, boats and angling (48)',16x,10A10)
 
      kp = 0 ! ------------------------------- user-named diffuse pollution (50)
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(23,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(23,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8350)nameprop(23),
     &(Tenchars1(jdet),jdet=1,ndet)
 8350 format('+',2x,a37,8x,10A10) ! ---------- user-named diffuse pollution (50)

      kp = 0 ! ------------------------------- user-named diffuse pollution (52)
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(24,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(24,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8352)nameprop(24),
     &(Tenchars1(jdet),jdet=1,ndet)
 8352 format('+',2x,a37,8x,10A10) ! ---------- user-named diffuse pollution (52)

      kp = 0 ! ------------------------------- user-named diffuse pollution (54) 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(25,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(25,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8354)nameprop(25),
     &(Tenchars1(jdet),jdet=1,ndet)
 8354 format('+',2x,a37,8x,10A10) ! ---------- user-named diffuse pollution (54)

      kp = 0 ! ------------------------------- user-named diffuse pollution (56)
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(26,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(26,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8356)nameprop(26),
     &(Tenchars1(jdet),jdet=1,ndet)
 8356 format('+',2x,a37,8x,10A10) ! ---------- user-named diffuse pollution (56)

      kp = 0 ! ------------------------------- user-named diffuse pollution (58)
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(27,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(27,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8358)nameprop(27),
     &(Tenchars1(jdet),jdet=1,ndet)
 8358 format('+',2x,a37,8x,10A10) ! ---------- user-named diffuse pollution (58)

      kp = 0 ! total loads from septic tanks (37) ------------------------- (37)
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(12,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(12,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8137)(Tenchars1(jdet),jdet=1,ndet) ! ------- LOD
 8137 format('+',2x,'Septic tanks (37)',28x,10A10)
      
      kp = 0 ! net total loads from Aggregate CSOs (40) ------------------- (40)
      do jdet = 1, ndet ! ------------------------------------------------------
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then ! ----------------------------------------
      if ( abs (LMcontrib(13,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(13,jdet,1))
      Tenchars1(jdet) = valchars10
      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1, ndet ------------------------------------------------
      if ( kp .eq. 1 ) write(27,8140)(Tenchars1(jdet),jdet=1,ndet) ! ------- LOD
 8140 format('+',2x,'Aggregate CSOs (40)',26x,10A10)

*     ==========================================================================      
      if ( kdirect .eq. 0 ) then ! write totals at the start of the reach ======
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(NTD,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(NTD,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then ! -------------------------------------------------- 
      write(27,8443)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8443 format(140('-')/'NET TOTAL LOADS at the start of reach',
     &11x,10A10)
      write(27,4826)
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
      endif ! start of the reach ===============================================
      
      if ( kdirect .eq. 1 ) then ! at this point ================= at this point
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(NTD,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(NTD,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,8553)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8553 format(140('-')/'NET TOTAL LOADS at this point ',18x,10A10)
      write(27,4826)
      endif
      endif ! at this point ======================================= at this point

      if ( kdirect .eq. 3 ) then ! at the end of the reach ======================
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(NTD,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(NTD,jdet,1))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,8333)(Tenchars1(jdet), jdet=1,ndet) ! ----------------------- LOD
 8333 format(140('-')/'NET TOTAL LOADS at the end of the reach ',
     &8x,10A10)
      write(27,4826)
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
      endif ! at the end of the reach ==========================================

      if ( kdirect .eq. 9 ) then ! at the end of the model =====================
      kp = 0 
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (LMcontrib(NTD,jdet,1)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (LMcontrib(NTD,jdet,1))
      Tenchars1(jdet) = valchars10
      endif ! if ( QTYPE (jdet) .ne. 4 )
      enddo ! do jdet = 1, ndet
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,8343)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8343 format(140('-')/'NET TOTAL LOADS at the end of the model',
     &9x,10A10)
      write(27,4826)
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
      endif ! at the end of the model ==========================================


      kp =  0 ! check for losses of load for a determinand ---------------------
      do idet = 1, ndet ! ------------------------------------------------------
      if ( QTYPE (idet) .ne. 4 ) then ! ----------------------------------------
      if ( abs (TLOSSES2(idet,i13)) .gt. 1.0e-8 ) kp = 1 ! load has been lost --
      endif ! if ( QTYPE (idet) .ne. 4 ) ---------------------------------------
      enddo ! do idet = 1, ndet ------------------------------------------------
            
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = dna(idet)
      endif
      enddo
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,4296)(Tenchars1(idet),idet = 1, ndet) ! --------------------- LOD
 4296 format(/140('=')/'Loads that have been ...',24x,10(6x,a4))
      write(27,4826)
      endif ! if ( kp .eq. 1 ) ------------------------------------------------- 

      kp = 0 ! check total loads from abstractions -----------------------------
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (TBLODE2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 2 (TBLODE2(jdet,i13),propabs(jdet))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then ! there is an abstracted load ----------------------
      write(27,8246)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8246 format('-',2x,'Removed by abstractions (7, 18 and 19)',7x,10A10)
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
      
      do jdet = 1,ndet ! ------------------------------------------------- Di.ACL
      if ( QTYPE (jdet) .ne. 4 ) then ! ---------------------------------- Di.ACL
      ihead = 0
      if ( kdirect .eq. 0 ) then ! ----------------------- at the start of reach
      if (JT(feeture) .eq. 10 .or. JT(feeture) .eq. 11 .or. 
     &    JT(feeture) .eq. 20 .or. JT(feeture) .eq. 21 .or.
     &    JT(feeture) .eq. 45 .or.
     &    JT(feeture) .eq. 22 .or. JT(feeture) .eq. 23) then   
      else
      ihead = 1
      write(220+jdet,2267)rname(ireach),lunits(jdet) ! ================== Di.ACL
 2267 format(////128('=')/'At the start of the reach: ',a16,43x, ! ====== Di.ACL
     &'LOAD   plus or'/86x,a4,'     minus'/128('-')) ! ================== Di.ACL
      endif
      endif ! if ( kdirect .eq. 0 ) ! -------------------- at the start of reach
      
      if ( kdirect .eq. 3 ) then ! --------------------- at the end of the reach
      ihead = 1
      write(220+jdet,2167)rname(ireach),lunits(jdet) ! ================== Di.ACL
 2167 format(////128('=')/'At the end of the reach: ',a16,45x, ! ======== Di.ACL
     &'LOAD   plus or'/86x,a4,'     minus'/128('-')) ! ================== Di.ACL
      endif ! if ( kdirect .eq. 3 ) ! ------------------ at the end of the reach
      
*     for features that are water bodies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( JT(feeture) .eq. 24 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~ at a water body
      if ( kdirect .ne. 0 .and. kdirect. ne. 3 ) then
      ihead = 1
      write(220+jdet,2268)uname(feeture),rname(ireach),lunits(jdet) ! === Di.ACL
 2268 format(////128('=')/'At: ',a35,' on reach: ',a16,20x, ! =========== Di.ACL
     &'LOAD   plus or'/86x,a4,'     minus'/128('-')) ! ================== Di.ACL
      endif ! if ( kdirect .ne. 0 .and. kdirect. ne. 3 ) ~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( JT(feeture) .eq. 24 ) then ! ~~~~~~~~~~~~~~~~ at a water body
      
      
      
*     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
*     loads lost by abstraction aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      if ( ihead .eq. 1 ) then ! -----------------------------------------------
      XM = TBLODE2(jdet,i13) ! the mean load removed by abstractions +++++++++++
      xcov = LMcontrib(NTD,jdet,2)/LMcontrib(NTD,jdet,1)
      XS = xcov*TBLODE2(jdet,i13) ! the standard deviation +++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(jdet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (XDL)
      write(220+jdet,2262)Tenchars1(jdet),valchars10 !=================== Di.ACL
 2262 format('Load removed by upstream abstractions (7, 18 and 19)',28x,
     &2a10)
      endif ! if ( ihead .eq. 1 ) ----------------------------------------------
      
      if ( JT(feeture) .eq. 24 ) then ! ---------- at a water body ----- WBi.CSV
      xxxp = 100.0*TBLODE2(jdet,i13)/LMcontrib(NTD,jdet,1)
      write(260+jdet,6692)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &TBLODE2(jdet,i13),XL,XU,QUALNB(jdet,NTD),xxxp,unamex ! ----------- WBi.CSV
 6692 format(' ',a40,',',i4,',',a40,',',a20,',Abstractions,', ! -------- WBi.CSV
     &'Upstream,','Load removed by abstractions ', ! ------------------- WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV
      endif ! if ( JT(feeture) .eq. 24 ) -------- at a water body ------ WBi.CSV

      endif ! if ( kp .eq. 1 ) -------------------------------------------------
      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
*     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     
      
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
*     loads removed by natural purification ------------------------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      Tenchars1(jdet) = '         0'
      if ( abs (TNLOADDN2(jdet,i13)) .gt. 1.0e-8 ) kp = 1
      call sort format 2 (TNLOADDN2(jdet,i13),propNPD(jdet))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,1931)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 1931 format('-',2x,'Removed by natural purification',14x,10A10)
      if ( JT(feeture) .eq. 24 .or. 
     &kdirect .eq. 0 .or. kdirect .eq. 3 ) then
      do jdet = 1,ndet
      if ( QTYPE (jdet) .ne. 4 ) then ! ----------------------------------------
      if ( ihead .eq. 1 ) then ! -----------------------------------------------
      XM = TNLOADDN2(jdet,i13) ! the mean load removed by natural purification -
      xcov = LMcontrib(NTD,jdet,2)/LMcontrib(NTD,jdet,1)
      XS = xcov*TNLOADDN2(jdet,i13) ! the standard deviation +++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(jdet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (XDL)
      write(220+jdet,5262)Tenchars1(jdet),valchars10 ! ================== Di.ACL
 5262 format('Removed by natural purification',49x,2a10) ! ============== Di.ACL
      endif ! if ( ihead .eq. 1 ) then -----------------------------------------
           
      xxxp = 100.0*TNLOADDN2(jdet,i13)/LMcontrib(NTD,jdet,1)
      write(260+jdet,6693)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &TNLOADDN2(jdet,i13),XL,XU,QUALNB(jdet,NTD),xxxp,unamex ! --------- WBi.CSV
 6693 format(' ',a40,',',i4,',',a40,',',a20,',Natural purification,', !  WBi.CSV
     &'Upstream,','Removed by natural purification ', ! ---------------- WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV

      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
      endif ! if ( JT(feeture) .eq. 24 
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
*     nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
      
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     loads removed by gap filling for flows -----------------------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (TILOADDN2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 2 (TILOADDN2(jdet,i13),propfra(jdet))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,8913)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8913 format('-',2x,'Removed by gap filling of flows',14x,10A10)
      if ( JT(feeture) .eq. 24 .or. 
     &kdirect .eq. 0 .or. kdirect .eq. 3 ) then
      do jdet = 1,ndet ! -------------------------------------------------------
      if ( QTYPE (jdet) .ne. 4 ) then ! ----------------------------------------
          
      XM = TILOADDN2(jdet,i13) ! the mean load from flow gap-filling +++++++++++
      xcov = LMcontrib(NTD,jdet,2)/LMcontrib(NTD,jdet,1)
      XS = xcov*TILOADDN2(jdet,i13) ! the standard deviation +++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(jdet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (XDL)
      write(220+jdet,5962)Tenchars1(jdet),valchars10 ! ================== Di.ACL
 5962 format('Removed by gap filling of flows',49x,2a10) ! ============== Di.ACL
            
      xxxp = 100.0*TILOADDN2(jdet,i13)/LMcontrib(NTD,jdet,1)
      write(260+jdet,6695)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &TILOADDN2(jdet,i13),XL,XU,QUALNB(jdet,NTD),xxxp,unamex ! --------- WBi.CSV
 6695 format(' ',a40,',',i4,',',a40,',',a20,',Flow gap filling,', ! ---- WBi.CSV
     &'Upstream,','Removed by gap filling of flows ', ! ---------------- WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV

      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
      endif ! if ( JT(feeture) .eq. 24 etc -------------------------------------
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     total loads removed by gap filling of river quality ----------------------
      kp = 0
      do jdet = 1, ndet
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (TALOADDN2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 2 (TALOADDN2(jdet,i13),propqra(jdet))
      Tenchars1(jdet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,8964)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 8964 format('-',2x,'Removed by gap filling of quality',12x,10A10)
      if ( JT(feeture) .eq. 24 .or. 
     &kdirect .eq. 0 .or. kdirect .eq. 3 ) then
      do jdet = 1,ndet
      if ( QTYPE (jdet) .ne. 4 ) then
          
      XM = TALOADDN2(jdet,i13) ! the mean load from flow gap-filling +++++++++++
      xcov = LMcontrib(NTD,jdet,2)/LMcontrib(NTD,jdet,1)
      XS = xcov*TALOADDN2(jdet,i13) ! the standard deviation +++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(jdet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (XDL)
      write(220+jdet,5963)Tenchars1(jdet),valchars10 ! ================== Di.ACL
 5963 format('Removed by gap filling of quality',47x,2a10)
      
      xxxp = 100.0*TALOADDN2(jdet,i13)/LMcontrib(NTD,jdet,1)
      write(260+jdet,6696)GIScode(feeture),jxtype,unamex, ! ------------ WBi.CSV
     &rname(IREACH),Length of main river, ! ---------------------------- WBi.CSV
     &TALOADDN2(jdet,i13),XL,XU,QUALNB(jdet,NTD),xxxp,unamex ! --------- WBi.CSV
 6696 format(' ',a40,',',i4,',',a40,',',a20,',Quality gap filling,', ! - WBi.CSV
     &'Upstream,','Removed by gap filling of quality ', ! -------------- WBi.CSV
     &(','0pf12.3),4(',',1pe12.3),(',',0pf11.3,'%'), ! ----------------- WBi.CSV
     &',,,,,,',a40) ! -------------------------------------------------- WBi.CSV

      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
      endif !if ( JT(feeture) .eq. 24 etc --------------------------------------
      endif ! if ( kp .eq. 1 ) -------------------------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



*     tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
*     total losses of load ttttttttttttttttttttttttttttttttttttttttttttttttttttt
      kp = 0
      do jdet = 1, ndet ! ======================================================
      Tenchars1(jdet) = '      ....'
      if ( QTYPE (jdet) .ne. 4 ) then ! ----------------------------------------
      Tenchars1(jdet) = '         0'
      if ( abs (TLOSSES2(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call sort format 2 (TLOSSES2(jdet,i13),prolosses(jdet))
      Tenchars1(jdet) = valchars10
      endif ! if ( QTYPE (jdet) .ne. 4 ) ---------------------------------------
      enddo ! do jdet = 1, ndet ================================================
      
      if ( kp .eq. 1 ) then ! --------------------------------------------------
      write(27,7964)(Tenchars1(jdet),jdet=1,ndet) ! ------------------------ LOD
 7964 format(140('-')/'TOTAL LOAD REMOVED ...',26x,10A10)
      write(27,7334)
 7334 format(140('='))
      if ( JT(feeture) .eq. 24 .or. 
     &kdirect .eq. 0 .or. kdirect .eq. 3 ) then
      do jdet = 1,ndet
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( ihead .eq. 1 ) then
      XM = TLOSSES2(jdet,i13) ! total losses +++++++++++++++++++++++++++++++++++
      xcov = LMcontrib(NTD,jdet,2)/LMcontrib(NTD,jdet,1)
      XS = xcov*TLOSSES2(jdet,i13) ! the standard deviation ++++++++++++++++++++
      xqualn = amax1 (0.0001,QUALNB(jdet,NTD)) ! number of samples (total) +++++
      call confidence limits (XM,XS,xqualn,XL,XU,XLP,XUP,KDL,XDL)
      call sort format 1 (XDL)
      write(220+jdet,5964)Tenchars1(jdet),valchars10 ! ================== Di.ACL
 5964 format(128('-')/'Total load removed upstream ...',17x,32x, ! ====== Di.ACL
     &2a10) ! =========================================================== Di.ACL
      endif ! if ( ihead .eq. 1 )
      endif
      enddo
      endif ! if ( JT(feeture) .eq. 24
      endif ! if ( kp .eq. 1 ) --------------------------------------------------

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      Tenchars1(idet) = dna(idet)
      endif
      enddo
      write(27,4266)(Tenchars1(idet),idet = 1,ndet) ! ---------------------- LOD
 4266 format(/140('=')/'Percentages of NET load from all inputs ',8x,
     &10(6x,a4))
      write(27,4166)('(%)',idet = 1, ndet)
 4166 format(39('-'),9x,10(7x,a3))
      write(27,4806)
   
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
      write(27,8863)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8863 format('+',2x,'Boundaries and tributaries (2, 10)',11X,10A10)
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
      write(27,9163)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 9163 format('+',2x,'Reach-type diffuse sources',19X,10A10) ! percentages ------
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
      
      if ( kp .eq. 1 ) write(27,9103)(Tenchars1(idet),idet=1,ndet) ! ------ LOD
 9103 format('+',2x,'Added by natural purification',16X,10A10) ! ---------- LOD
      if ( kp .eq. 0 ) write(27,9103)(Tenchars1(idet),idet=1,ndet) ! ------ LOD


*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
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
      if ( kp .eq. 1 ) write(27,8923)(Tenchars1(idet),idet=1,ndet) ! ------- LOD
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
      if ( kp .eq. 1 ) write(27,8224)(Tenchars1(idet),idet=1,ndet) ! ------ LOD
 8224 format('+',2x,'Added by gap filling of quality',14x,10A10)
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



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
*     if ( kp .eq. 1 ) then
*     write(27,8363)(Tenchars1(idet), idet=1,ndet) 
*8363 format(5X,'Point discharges (3, 12 and 5)',15X,10A10)
*     if ( kp .eq. 1 ) write(27,4826)
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
      write(27,8303)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8303 format('+',2x,'Sewage treatment works (3)',19X,10A10)
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
      write(27,8312)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8312 format('+',2x,'Intermittent discharges (12)',17X,10A10)
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
      write(27,8305)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8305 format('+',2x,'Industrial discharges (5)',20x,10A10)
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
      write(27,8395)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8395 format('+',2x,'Mine waters (39)         ',20X,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (propeff60(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff60 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4395)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 4395 format('+',2x,'"Other" Point Sources (60) ',18X,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (propeff61(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff61(idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4495)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 4495 format('+',2x,'Private wastewaters (61) ',20X,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (propeff62(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propeff62(idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,6495)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 6495 format('+',2x,'Fish farms (62)          ',20X,10A10)
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
      write(27,8366)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8366 format('+',2x,'River-based diffuse pollution (13)',11X,10A10)
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
      write(27,2366)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 2366 format('+',2x,'Effluent-based diffuse pollution (15)',8X,10A10)
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
      write(27,8966)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8966 format('+',2x,'Aggregate STWs (42)                  ',8X,10A10)
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
      write(27,8364)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8364 format('+',2x,'Livestock (25)',31X,10A10)
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
      write(27,8365)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8365 format('+',2x,'Arable (27)',34X,10A10)
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
      write(27,8367)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8367 format('+',2x,'Highway runoff (29)',26x,10A10)
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
      write(27,8368)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8368 format('+',2x,'Urban runoff (31)',28x,10A10)
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
      write(27,8369)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8369 format('+',2x,'Atmospheric deposition (33)',18x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop35(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop35 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,8069)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8069 format('+',2x,'Natural background (35)',22x,10A10)
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
      write(27,4069)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 4069 format('+',2x,'Diffuse mines (46)',27x,10A10)
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
      write(27,4669)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 4669 format('+',2x,'Birds, boats and angling (48)',16x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop50(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop50 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4650)nameprop(23),(Tenchars1(idet),idet=1,ndet) ! ----------- LOD
 4650 format('+',2x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop52(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop52 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4652)nameprop(24),(Tenchars1(idet),idet=1,ndet) ! ----------- LOD
 4652 format('+',2x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop54(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop54 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4654)nameprop(25),(Tenchars1(idet),idet=1,ndet) ! ----------- LOD
 4654 format('+',2x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop56(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop56 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4656)nameprop(26),(Tenchars1(idet),idet=1,ndet) ! ----------- LOD
 4656 format('+',2x,a37,8x,10A10)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prop58(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (prop58 (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) then
      write(27,4658)nameprop(27),(Tenchars1(idet),idet=1,ndet) ! ----------- LOD
 4658 format('+',2x,a37,8x,10A10)
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
      write(27,8370)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8370 format('+',2x,'Septic tanks (37)',28x,10A10)
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
      write(27,8270)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8270 format('+',2x,'Aggregate CSOs (40)',26x,10A10)
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
      write(27,4136)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 4136 format(140('-')/'TOTAL NET LOAD (%) ...',26x,10A10)
      write(27,4806)
      endif
*     ==========================================================================
      kp = 0
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (prolosses(idet)) .gt. 1.0e-10 ) kp = 1
      endif
      enddo
      if ( kp .eq. 1 ) write(27,4106)uname(feeture) ! ---------------------- LOD
 4106 format(/140('=')/'Percentages of the total load that have ',
     &'been removed ... at: ',a37/140('='))
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
      if ( kp .eq. 1 ) write(27,8246)(Tenchars1(idet),idet=1,ndet) ! ------- LOD
*     ==========================================================================
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
      if ( kp .eq. 1 ) write(27,1932)(Tenchars1(idet),idet=1,ndet) ! ------ LOD
 1932 format('-',2x,'Removed by natural purification',14x,10A10) ! -------- LOD


*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
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
      if ( kp .eq. 1 ) write(27,8913)(Tenchars1(idet),idet=1,ndet) ! ------- LOD
*     ==========================================================================
*     loads removed by gap filling for quality ---------------------------------
      kp = 0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      if ( abs (propqra(idet)) .gt. 1.0e-10 ) kp = 1
      call sort format1 (propqra(idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( kp .eq. 1 ) write(27,8964)(Tenchars1(idet),idet=1,ndet) ! ------ LOD
      if ( kp .eq. 1 ) write(27,4806) ! ----===========-------------------- LOD
 4806 format(140('='))
*     ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg



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
      write(27,8914)(Tenchars1(idet),idet=1,ndet) ! ------------------------ LOD
 8914 format(140('-')/'TOTAL % LOSSES ',33x,10a10)
      write(27,4806)
      endif
*     ==========================================================================
      
      return
      end


      subroutine proportion of effluent (iplace)
      include 'COMMON DATA.FOR'

      if ( iplace .ne. 4 ) then
      rnamex = rname (IREACH)
      else
      rnamex = 'Upstream of works'
      endif

      do idet = 1, ndet
      propeff2 (idet) = 0.0 ! initialise the proportion of effluent ------------
      if ( QTYPE (idet) .ne. 4 ) then
      if ( LMcontrib(01,idet,1) .gt. 1.0e-10 ) then
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) then
      propeff2 (idet) = 100.0 * LMcontrib(01,idet,1) / 
     &                          LMcontrib(NTD,idet,1)
      endif
      endif
      endif
      enddo

     
      jxtype = JT(feeture)
      if ( JT(feeture) .ne. 10 ) then
      unamex = uname(feeture)
      else
      unamex = 'Start of Reach'
      jxtype = 111
      endif

      return
      end

      
      
*     write the proportions of effluent at the end of the reach ----------------
      subroutine proportion of effluent end 
      include 'COMMON DATA.FOR'

      do idet = 1, ndet

*     initialise and calculate the proportion of effluent ----------------------
      propeff2 (idet) = 0.0
      if ( QTYPE (idet) .ne. 4 ) then
      if ( LMcontrib(01,idet,1) .gt. 1.0e-10 ) then
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) then
      propeff2(idet) = 100.0 * LMcontrib(01,idet,1) / 
     &                         LMcontrib(NTD,idet,1) 
      endif
      endif
      endif
      enddo

      return
      end

      subroutine proportion of effluent at model end 
      include 'COMMON DATA.FOR'

      do idet = 1, ndet
      propeff2 (idet) = 0.0 ! initialise proportion of effluent ----------------
      if ( QTYPE (idet) .ne. 4 ) then
      if ( LMcontrib(01,idet,1) .gt. 1.0e-10 ) then
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) then
      propeff2 (idet) = 100.0 * LMcontrib(01,idet,1) / 
     &                          LMcontrib(NTD,idet,1) ! at model end
      endif
      endif
      endif
      enddo

      return
      end



*     write the proportions of effluent at the start of the reach --------------
      subroutine proportion of effluent start 
      include 'COMMON DATA.FOR'

      do idet = 1, ndet
      propeff2 (idet) = 0.0 ! initialise proportion of effluent
      if ( QTYPE (idet) .ne. 4 ) then
      if ( LMcontrib(01,idet,1) .gt. 1.0e-10 ) then
      if ( LMcontrib(NTD,idet,1) .gt. 1.0e-10 ) then
      propeff2 (idet) = 100.0*LMcontrib(01,idet,1)/LMcontrib(NTD,idet,1) ! start
      endif
      endif
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo

      return
      end

      
      
      
      
      subroutine sort out the concentrations and loads
      include 'COMMON DATA.FOR'
      character *148 LINE
      character * 7 A1
      character *10 BB1(MP10)
      character * 6 cptest
      character *13 set1,set2
      
*     write out the change in percentile caused by gap filling -----------------
      
*     initialise the arrays that will hold the values --------------------------
      do jdet = 1,MP10 ! -------------------------------------------------------
      Sevenchars(jdet) = '      -'
      B44(jdet) = '      -'
      enddo ! do jdet = 1,MP10 -------------------------------------------------

      
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     write out the percentage changes in quality brought about by gap filling -
      do jdet = 1,ndet ! -------------------------------------------------------
      if ( QTYPE(jdet) .ne. 4 .and. QNAT(jdet) .ne. -99 ) then ! ---------------
      write(Sevenchars(jdet),7535)QNAT(jdet)
 7535 format(I7)
      endif ! if ( QTYPE (jdet) .ne. 4 .and. QNAT(jdet) .ne. -99 ) -------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      call set up output of calculated means or percentiles ! ----- at a feature
      call set up flowa ! ----------------------------------------- at a feature
      call set up flow95 ! ---------------------------------------- at a feature

     
      
*     ##########################################################################      
*     write out the calculated flow and quality at this point ##################
      if ( output mode .ne. 1 ) then ! ############ deal with percentile quality
      write(LINE,9495)UNAME(feeture),Length of main river, !  percentile quality
     &flowchars(2),(Sevenchars(jdet),jdet=1,ndet)
*     ##########################################################################
    
      do jper = 1,ndet ! ------------------------------------ percentile quality
      if ( qtype(jper) .ne. 4 ) then ! -----------------------------------------
      write(170+jper,5379)UNAME(feeture),Length of main river, ! -------- Ci.GAP
     &Flowchars(2),Sevenchars(jper)
 5379 format(5x,A31,1x,F6.1,A7)
      endif ! if ( qtype(jper) .ne. 4 ) ----------------------------------------
      enddo ! do jper = 1,ndet ------------------------------ percentile quality
      
*     write out the quality at this point --------------------------------------
      read(LINE(6:11),'(a6)')cptest	

      if ( iscreen .eq. 0 .and. cptest .ne. '  ....' ) then ! ==================
      if ( feetcount .gt. 500 ) then ! restrict the amount of output to screen -
      if ( JT (feeture) .ne. 6  .and. JT (feeture) .ne. 16 .and.
     &     JT (feeture) .ne. 26 .and. JT (feeture) .ne. 28 .and.
     &     JT (feeture) .ne. 30 .and. JT (feeture) .ne. 32 .and.
     &     JT (feeture) .ne. 34 .and. JT (feeture) .ne. 36 .and.
     &     JT (feeture) .ne. 47 .and. JT (feeture) .ne. 49 .and.
     &     JT (feeture) .ne. 38 .and.
     &     JT (feeture) .ne. 41 .and. JT (feeture) .ne. 43 ) then
      write( *,7511)LINE ! ------------- feature - calculated percentile quality
      endif ! if ( JT (feeture) .ne. 6 etc -------------------------------------
      else ! if ( feetcount .gt. 500 ) -----------------------------------------
      write( *,7511)LINE ! ------------- feature - calculated percentile quality
      endif ! if ( feetcount .gt. 500 ) ----------------------------------------
      endif ! if ( iscreen .eq. 0 .and. cptest .ne. '  ....' ) =================
      
      write(09,7511)LINE ! feature - calculated percentile quality --------- SCN
      write(33,7511)LINE ! feature - calculated percentile quality --------- ERR
 7511 format(A121)
      
*     exclude writing quality in flow gap filling runs -------------------------
      if ( ical .gt. 2 .or. ical .eq. 0 ) then ! +++++++++++++++++++++++++++++++

*     write for the monitoring points ++++++++++++++++++++++++++++++++++++++++++
      if ( JT(feeture) .eq. 1 ) then ! +++++++++++++++++++++++++++++++++++++++++
      call set up output of calculated means or percentiles ! + monitoring point

      endif ! if ( JT(feeture) .eq. 1 ) for monitoring points ++++++++++++++++++
      endif ! if ( ical .gt. 2 .or. ical .eq. 0 ) ++++++++++++++++++++++++++++++
      endif ! if ( output mode .ne. 1 ) then ! ########### deal with percentiles
*     ##########################################################################      

      
      
*     ##########################################################################      
*     mean mode - write calculated mean values for this point ++++++++++++++++++
      if ( output mode .eq. 1 ) then ! ================== deal with mean quality
      write(LINE,9495)UNAME(feeture),Length of main river, ! -- for mean quality 
     &flowchars(1),(Sevenchars(jdet),jdet=1,ndet)
 9495 format(5x,A31,1x,F6.1,11A7)
      call sort format 2 (Flow(1),Flow(2))

      do jper = 1,ndet ! -------------------------------------------------------
      if ( qtype(jper) .ne. 4 ) then ! -----------------------------------------
      if ( ical .eq. 2 .or. ical .eq. 1 ) then
      if ( JT(feeture) .eq. 4 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(170+jper,1679)Valchars10,Funit, ! --------------------------- Ci.GAP
     &Valchars11,Funit,UNAME(feeture)
 1679 format('Calculated river flow ...',31x,'Mean =',a10,1x,a4/ ! ------ Ci.GAP
     &38x,'95-percentile low flow =',a10,1x,a4,4x,a35/110('='))
      endif ! if ( JT(feeture) .eq. 4 ) 
      endif ! if ( ical .eq. 2 etc ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      !if ( ical .eq. -2 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !write(170+jper,5679)Length of main river,Flowchars(1),Funit, ! --- Ci.GAP
      !&Sevenchars(jper),units(jper),UNAME(feeture)
      !5679 format('Mean flow and quality at', ! ------------------------ Ci.GAP
      !&F7.1,' km',11x,a7,1x,a4,8x,a7,1x,a4,4x,a35)
      !endif ! if ( ical .eq. 2 ) ~~~~~~~~~~~~~a35~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
      endif ! if ( qtype(jper) .ne. 4 ) ----------------------------------------
      enddo ! do jper = 1,ndet -------------------------------------------------
      else ! =================== deal with percentile quality ##################
      !do jper = 1,ndet ! ------------------------------------------------------
      !if ( qtype(jper) .ne. 4 ) then ! ----------------------------------------
      !if ( ical .eq. -2 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !write(170+jper,5679)Length of main river,Flowchars(1),Funit, ! --- Ci.GAP
      !&Sevenchars(jper),units(jper),UNAME(feeture) ! ------------------- Ci.GAP
      !endif ! if ( ical .eq. 2 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
      !endif ! if ( qtype(jper) .ne. 4 ) ---------------------------------------
      !enddo ! do jper = 1,ndet ------------------------------------------------
      endif ! if ( output mode .eq. 1 ) ################# deal with mean quality

*     mean mode - write calculated values for this point +++++++++++++++++++++++
*     ##########################################################################      
     
   
   
*     ##########################################################################      
*     calculations for modes that are not 90 or 99 ppercentiles ++++++++++++++++
      if ( output mode .ne. 2 .and. output mode .ne. 3 ) then ! ================
      if ( output mode .ne. 0 ) then
      if ( iscreen .lt. 3 ) write( *,7511)LINE
      write(09,7511)LINE ! feature - calculated mean quality
      write(33,7511)LINE ! feature - calculated mean quality
      endif
      endif ! if ( output mode .ne. 2 .and. output mode .ne. 3 ) ===============


*     write loads from effluent discharges -------------------------------------
*     apply to (3) (5) (12) (39) (60) (61) (62) ---------------- write out loads
      if ( JT(feeture) .eq. 03 .or. ! ----------------------------- sewage works
     &     JT(feeture) .eq. 05 .or. ! ------------------------- point discharges 
     &     JT(feeture) .eq. 12 .or. ! ------------------ intermittent discharges
     &     JT(feeture) .eq. 39 .or. ! ------------------------- mining discharge
     &     JT(feeture) .eq. 60 .or. ! ---------------------- Other Point Sources
     &     JT(feeture) .eq. 61 .or. ! ---------------------- private wastewaters 
     &     JT(feeture) .eq. 62 ) then ! ------------------------------ fish farm 
          
      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      call set format for printout (BB1(jdet),eload(jdet,i13)) ! ---------- .LOD
      endif
      enddo
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(27,8932)(BB1(jdet),jdet=1,ndet) ! ----------------------------- .LOD
 8932 format(////70('- ')/
     &'(loads to be introduced by the next discharge)',2x,10A10)
      write(27,5281)
 5281 format(70('- '))
      endif
      endif
      endif ! if ( JT(feeture) .eq. 3 etc ) ------------------------------------

*     write loads from tributaries =============================================
      if ( JT(feeture) .eq. 2  .or. JT(feeture) .eq. 21 .or. ! =================
     &     JT(feeture) .eq. 23 ) then ! ========================================
      kp = 1
      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      if ( abs (eload(jdet,i13)) .gt. 1.0e-10 ) kp = 1
      call set format for printout (BB1(jdet),eload(jdet,i13))
      endif
      enddo
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then 
      if ( kp .eq. 1 ) write(27,8332)(BB1(jdet),jdet=1,ndet)
 8332 format(////70('- ')/
     &'(loads to be added by the next tributary)',7X,10A10)
      if ( kp .eq. 1 ) write(27,6384)
 6384 format(70('- '))
      endif 
      endif
      endif ! if ( JT(feeture) .eq. 2 ==========================================

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     write loads from river regulation point ==================================
      if ( JT(feeture) .eq. 9 ) then ! =========================================
      do jdet = 1 , ndet
      BB1(jdet) = '         -'
      if ( QTYPE (jdet) .ne. 4 ) then
      call set format for printout (BB1(jdet),eload(jdet,i13))
      endif
      enddo
      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( nobigout .le. 0 ) then ! ............................................
      write(27,5932)(BB1(jdet),jdet=1,ndet)
 5932 format(/70('- ')/
     &'(loads introduced by the next river regulation)',1x,10A10)
      write(27,6984)
 6984 format(70('- '))
      endif ! if ( nobigout .le. 0 ) ...........................................
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------------
      endif ! if ( JT(feeture) .eq. 9 ) ========================================

*     write out the loads removed by abstractions ==============================
      if ( JT(feeture) .eq. 7  .or. JT(feeture) .eq. 18 .or.  ! ================
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
 5952 format(////70('- ')/
     &'(loads to be removed by the next abstraction)',3X,10A10)
      write(27,4952)
 4952 format(72('- '))
      endif
      endif
      endif ! if ( JT(feeture) .eq. 7, 18, 19 etc ==============================
*     ==========================================================================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     write effluent quality required to meet target river quality -------------
*     only relevant in Modes 7, 8 and 9 ----------------------------------------
      if ( ical .eq. 0 .or. ical .eq. 4 .or. ical .gt. 6 ) then
*     only needed for effluent discharges --------------------------------------
*     apply to (3) (5) (12) (39) (60) (61) -------- setting limits on discharges
      if ( JT(feeture) .eq. 03 .or. ! ----------------------------- sewage works
     &     JT(feeture) .eq. 05 .or. ! ------------------------- point discharges 
     &     JT(feeture) .eq. 12 .or. ! ------------------ intermittent discharges
     &     JT(feeture) .eq. 39 .or. ! ------------------------- mining discharge
     &     JT(feeture) .eq. 60 .or. ! ---------------------- Other Point Sources
     &     JT(feeture) .eq. 61 ) then ! -------------------- private wastewaters 
*    &     JT(feeture) .eq. 62 ) then ! ----------------------------- fish farms
          
      notargit = 0
      call set up Xtargets (notargit)
     
      do jdet = 1, ndet
      B44(jdet) = '      -'
      if ( Qtype(jdet) .ne. 4 ) then
      if ( MRQS(jdet) .eq. 1 ) B44(jdet) = '   mean'
      if ( MRQS(jdet) .eq. 2 ) B44(jdet) = '    Q95'
      if ( MRQS(jdet) .eq. 3 ) B44(jdet) = '    Q90'
      if ( MRQS(jdet) .eq. 4 ) B44(jdet) = '    Q05'
      if ( MRQS(jdet) .eq. 5 ) B44(jdet) = '    Q10'
      if ( MRQS(jdet) .eq. 6 ) B44(jdet) = '    Q99'
*     =======      1, 2, 3 for mean, 95-percentile and 90-percentile        2
*     =======      4 and 5 for the 5 and 10-percentile                      2
*     =======      6 for the 99-percentile                                  2
      endif
      enddo
      
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      call change colour of text (10) ! green
      write( *,8033)('-------',jdet=1,ndet+2)
      !write( *,8993)(B44(jdet),jdet=1,ndet)
      !write( *,8933)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      endif ! if ( iscreen .lt. 3 ) then ---------------------------------------
      write(09,8033)('-------',jdet=1,ndet+2)
      write(33,8033)('-------',jdet=1,ndet+2)
 8033 format(5x,31('-'),12a7)
      
      if ( ifbatch .eq. 0 ) write( *,8993)(DNA(jdet),jdet=1,ndet)
      write(09,7993)(DNA(jdet),jdet=1,ndet)
      !write(31,7993)(DNA(jdet),jdet=1,ndet)
      write(33,7993)(DNA(jdet),jdet=1,ndet)
 7993 format(5x,'Determinand ...          ',20x,10(3x,A4))
      write(09,8993)(B44(jdet),jdet=1,ndet)
      write(33,8993)(B44(jdet),jdet=1,ndet)
 8993 format(5x,'Form of standard ...     ',20x,10A7)
      
      if ( ifbatch .eq. 0 ) write( *,8933)(Sevenchars(jdet),jdet=1,ndet)
      write(09,8933)(Sevenchars(jdet),jdet=1,ndet)
      write(33,8933)(Sevenchars(jdet),jdet=1,ndet)
 8933 format(5x,'Targets for river quality',20x,10A7)
      
      do jdet = 1, ndet ! set up the upstream summary statistics ===============
      Sevenchars(jdet) = '      -'   
      if ( Qtype(jdet) .ne. 4 ) then ! =========================================
      if ( MRQS(jdet) .gt. 0 .and. MRQS(jdet) .lt. 7 ) then
      if ( MRQS(jdet) .eq. 1 ) call set up statistics (jdet,CLD(jdet,1)) ! mean
      if ( MRQS(jdet) .eq. 2 ) call set up statistics (jdet,CLD(jdet,3)) ! q95
      if ( MRQS(jdet) .eq. 3 ) call set up statistics (jdet,CLD(jdet,4)) ! q90
      if ( MRQS(jdet) .eq. 4 ) call set up statistics (jdet,CLD(jdet,3)) ! q05
      if ( MRQS(jdet) .eq. 5 ) call set up statistics (jdet,CLD(jdet,4)) ! q10
      if ( MRQS(jdet) .eq. 6 ) call set up statistics (jdet,CLD(jdet,5)) ! q99
      endif
      endif ! if ( Qtype(jdet) .ne. 4 ) then ===================================
      enddo ! do jdet = 1, ndet ============== set up upsteam summary statistics 

      if ( ifbatch .eq. 0 ) write( *,7973)(Sevenchars(jdet),jdet=1,ndet)
      write(09,7973)(Sevenchars(jdet),jdet=1,ndet)
      write(33,7973)(Sevenchars(jdet),jdet=1,ndet)
 7973 format(5x,'Upstream river quality ',22x,10A7)
      call set screen text colour
      
      do jdet = 1, ndet ! set up the downstream summary statistics =============
      Sevenchars(jdet) = '      -'   
      if ( Qtype(jdet) .ne. 4 ) then ! -----------------------------------------
      if ( MRQS(jdet) .gt. 0 .and. MRQS(jdet) .lt. 7 ) then
      if ( MRQS(jdet) .eq. 1 ) call set up statistics (jdet,C(jdet,1)) ! mean
      if ( MRQS(jdet) .eq. 2 ) call set up statistics (jdet,C(jdet,3)) ! q95
      if ( MRQS(jdet) .eq. 3 ) call set up statistics (jdet,C(jdet,4)) ! q90
      if ( MRQS(jdet) .eq. 4 ) call set up statistics (jdet,C(jdet,3)) ! q05
      if ( MRQS(jdet) .eq. 5 ) call set up statistics (jdet,C(jdet,4)) ! q10
      if ( MRQS(jdet) .eq. 6 ) call set up statistics (jdet,C(jdet,5)) ! q99
      endif
      endif ! if ( Qtype(jdet) .ne. 4 ) then ===================================
      enddo ! do jdet = 1, ndet ============ set up downsteam summary statistics

      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      call change colour of text (10) ! green
      write( *,8973)(Sevenchars(jdet),jdet=1,ndet)
      write( *,8033)('-------',jdet=1,ndet+2)
      call set screen text colour
      endif ! if ( iscreen .lt. 3 ) then ---------------------------------------
      write(09,8973)(Sevenchars(jdet),jdet=1,ndet)
      write(33,8973)(Sevenchars(jdet),jdet=1,ndet)
 8973 format(5x,'Downstream river quality ',20x,10A7)
      write(09,8033)('-------',jdet=1,ndet+2)
      write(33,8033)('-------',jdet=1,ndet+2)
      endif ! if ( JT(feeture) .eq. 3 etc ======================================
     
     
      if ( ical .gt. 6 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++
*     number of data-set for river flow ----------------------------------------
      IF = JQ(feeture)
*     check flow is not trivial ------------------------------------------------
      if ( FE(IF,1) .gt. 0.0001 .and. notargit .eq. 1 ) then
      call set up xecm 
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      call change colour of text (20) ! bright red
      write( *,1933)(Sevenchars(jdet),jdet=1,ndet)
      call set screen text colour
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------
      write(09,1933)(Sevenchars(jdet),jdet=1,ndet)
      write(33,1933)(Sevenchars(jdet),jdet=1,ndet)
 1933 format(5X,'Required mean effluent quality',15x,10A7)
      call set up xeff 
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      call change colour of text (20) ! bright red
      write( *,1932)(Sevenchars(jdet),jdet=1,ndet)
      write( *,8033)('-------',jdet=1,ndet+2)
      call set screen text colour
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------      
      write(09,1932)(Sevenchars(jdet),jdet=1,ndet)
      write(33,1932)(Sevenchars(jdet),jdet=1,ndet)
 1932 format(5X,'Required 95%-tile effluent quality',11x,10A7)
      write(09,8033)('-------',jdet=1,ndet+2)
      write(33,8033)('-------',jdet=1,ndet+2)
      endif ! if ( FE(IF,1) .gt. 0.0001 ) --------------------------------------
      endif ! if ( ical .gt. 6 ) +++++++++++++++++++++++++++++++++++++++++++++++

      endif ! if ( ical .eq. 0 .or. ical .eq. 4 .or. ical .gt. 6 ) =============
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     write out the observed river quality -------------------------------------
      if ( ICAL .le. 09 .and. ICAL .ne. 1 ) then ! =============================
      if ( JT(feeture) .eq. 1 ) then ! #########################################
*     indicator that there are too many monitoring stations --------------------
*     if ( Imm .eq. 0 ) then
      do jdet = 1, MP10
      Sevenchars(jdet) = '      -'
      enddo
      
*     write out observed annual means for gap filling points ###################
      if ( JQCAL(feeture) .ne. 0 ) then ! ######################################
      call change colour of text (18) ! light grey
      if ( output mode .eq. 1 ) then ! =========================================
      call set up xa arrays (iz)
      if ( iz .eq. 1 ) then
      if ( iscreen .lt. 3 ) then
      write( *,8034)('=======',jdet=1,ndet+2)
 8034 format(5x,31('='),12a7)
      write( *,9682)(Sevenchars(jdet),jdet=1,ndet)
 9682 format(5X,'Observed mean values ',24X,10A7)
      endif
      
      do jper = 1,ndet ! =======================================================
      if ( qtype(jper) .ne. 4 ) then ! =========================================
*     calculate the percentile of measured quality -----------------------------
*     this is what SIMCAT aims to fit to ---------------------------------------
      call calculate percentiles from mean and standard deviation ! ------------
     &(jper,DCM,DCS,DCP)
      
      a11 = C(jper,1)
      s11 = C(jper,2)
      n11 = QNUM(IQ,jper)
      a22 = quolity data(IQ,jper,1)
      s22 = quolity data(IQ,jper,2)
      n22 = QNUM(IQ,jper)
      call diffav (a11,s11,n11,a22,s22,n22,al1,au1,al2,au2,
     &Pinc,Ydec,A12,S12,AL12,AU12)
      
      SET1 = "             "
      SET2 = "             "
      if ( a11. gt. 0.000001 ) call assem(AL1,AU1,SET1)
      if ( a22. gt. 0.000001 ) call assem(AL2,AU2,SET2)
      
      call sort format 1 (a22)
      write(170+jper,9142)valchars10,units(jper),SET2 ! ----------------- Ci.GAP
 9142 format('Observed quality at this point ... ', ! ------------------- Ci.GAP
     &21x,'Mean =',a10,1x,a4,4x,a13/110('=')) ! ------------------------- Ci.GAP
      endif ! if ( qtype(jper) .ne. 4 ) ========================================
      enddo ! do jper = 1,ndet ! ===============================================
            
      write(09,8034)('=======',jdet=1,ndet+2)
      write(33,8034)('=======',jdet=1,ndet+2)
      write(09,9632)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9632)(Sevenchars(jdet),jdet=1,ndet)
 9632 format(5X,'... observed mean values ',20X,10A7)
      endif ! if ( iz .eq. 1 ) =================================================
      endif ! if ( output mode .eq. 1 ) ++++++++++++++++++++++++++++++++++++++++
      
*     write out observed 95 and 5 percentiles ++++++++++++++++++++++++++++++++++
      if ( output mode .eq. 0 .and. iz .eq. 0 ) then ! +++++++++++++++++++++++++
      call set up x95
      if ( iscreen .lt. 3 ) then
      write( *,8034)('=======',jdet=1,ndet+2)
      write( *,9832)(Sevenchars(jdet),jdet=1,ndet)
 9832 format(5X,'Observed 95 or 5-percentiles ',16X,10A7)
      write( *,8034)('=======',jdet=1,ndet+2)
      endif
      write(09,8034)('=======',jdet=1,ndet+2)
      write(09,9732)(Sevenchars(jdet),jdet=1,ndet)
      write(33,8034)('=======',jdet=1,ndet+2)
      write(33,9732)(Sevenchars(jdet),jdet=1,ndet)
 9732 format(5X,'... observed 95 or 5-percentiles ',12X,10A7)
      endif ! if ( output mode .eq. 0 .and. iz .eq. 0 ) ++++++++++++++++++++++++
      
*     write out observed 90 and 10 percentiles +++++++++++++++++++++++++++++++++
      if ( output mode .eq. 2 .and. iz .eq. 1 ) then ! +++++++++++++++++++++++++
      call set up x90
      if ( iscreen .lt. 3 ) then
      write( *,8034)('=======',jdet=1,ndet+2)
      write( *,9432)(Sevenchars(jdet),jdet=1,ndet)
      endif
      write(09,8034)('=======',jdet=1,ndet+2)
      write(09,9432)(Sevenchars(jdet),jdet=1,ndet)
      write(33,8034)('=======',jdet=1,ndet+2)
      write(33,9432)(Sevenchars(jdet),jdet=1,ndet)
 9432 format(5X,'... observed 90 or 10-percentiles',12X,10A7)
      endif ! if ( output mode .eq. 2 .and. iz .eq. 1 ) ++++++++++++++++++++++++
      
*     write out observed 99 and 1 percentiles ++++++++++++++++++++++++++++++++++
      if ( output mode .eq. 3  .and. iz.eq. 1 ) then ! +++++++++++++++++++++++++
      call set up x99
      if ( iscreen .lt. 3 ) then
      write( *,8034)('=======',jdet=1,ndet+2)
      write( *,9982)(Sevenchars(jdet),jdet=1,ndet)
      endif
      write(09,8034)('=======',jdet=1,ndet+2)
      write(09,9982)(Sevenchars(jdet),jdet=1,ndet)
      write(33,8034)('=======',jdet=1,ndet+2)
      write(33,9982)(Sevenchars(jdet),jdet=1,ndet)
 9982 format(5X,'... observed 99 or 1-percentiles ',12X,10A7)
      endif !  if ( output mode .eq. 3 .and. iz.eq. 1 ) ++++++++++++++++++++++++
*     ==========================================================================

      if ( iscreen .lt. 3 .and. iz.eq. 1 ) then ! ------------------------------
      write( *,8034)('=======',jdet=1,ndet+2)
      endif

      write(09,8034)('=======',jdet=1,ndet+2)
      write(33,8034)('=======',jdet=1,ndet+2)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     write out the measured 90-percentiles of river quality +++++++++++++++++++
*     needed only in Modes 0, 2, 3 and 4 +++++++++++++++++++++++++++++++++++++++
      if ( output mode .eq. 2 .and. iz.eq. 1 ) then ! ++++++++++++++++++++++++++
*     indicator that there are too many monitoring stations --------------------
      if ( Imm .eq. 0 ) then ! -------------------------------------------------
      call set up x90
      if ( iscreen .lt. 3 ) write( *,9332)(Sevenchars(jdet),jdet=1,ndet)
      write(09,9332)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9332)(Sevenchars(jdet),jdet=1,ndet)
 9332 format(5X,'... measured 90 and 10-percentiles    ',7X,10A7)
      endif ! if ( Imm .eq. 0 ) ------------------------------------------------
      endif ! output mode .eq. 2 .and. iz .eq. 1 ) +++++++++++++++++++++++++++++

*     write out the measured 99-percentiles of river quality +++++++++++++++++++
*     needed only in Modes 0, 2, 3 and 4 +++++++++++++++++++++++++++++++++++++++
      if ( output mode .eq. 3 .and. iz .eq. 1 ) then ! +++++++++++++++++++++++++
*     indicator that there are too many monitoring stations --------------------
      !if ( Imm .eq. 0 ) then ! ------------------------------------------------
      call set up x99
      if ( iscreen .lt. 3 ) write( *,9939)(Sevenchars(jdet),jdet=1,ndet)
      write(09,9939)(Sevenchars(jdet),jdet=1,ndet)
      write(33,9939)(Sevenchars(jdet),jdet=1,ndet)
 9939 format(5X,'... measured 99 and 1-percentiles     ',7X,10A7)
      !endif ! if ( Imm .eq. 0 ) -----------------------------------------------
      endif ! if ( output mode .eq. 3 .and. iz .eq. 1 ) ++++++++++++++++++++++++
      
      call set screen text colour
      endif ! if ( JQCAL(feeture) .ne. 0 ) #####################################
      endif ! if ( JT(feeture) .eq. 1 ) ########################################
      endif ! if ( ICAL .lt. 09 .and. ICAL .ne. 1 ) ============================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


*     write out the measured river flow ----------------------------------------
      !if ( ICAL .le. 4. and. JT(feeture) .eq. 4 ) then ! ======================
      if ( JT(feeture) .eq. 4 ) then ! =========================================
      jkf = JFcal(KFEAT)
      A1 = '      -'
      if (jkf .gt. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++++
      X(1) = F(jkf,2)
      if ( output mode .eq. 1 ) X(1) = F(jkf,1)
      if ( PDRF(jkf) .eq. -999 ) goto 8356
      if ( X(1) .gt. 9.99) then ! ..............................................
      write(A1,7931) X(1)
 7931 format(F7.1)
      else ! if ( X(1) .gt. 9.99) ..............................................
      if ( X(1) .gt. 0.999) then  ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^              
      write(A1,7933) X(1)
 7933 format(F7.2)
      else ! if ( X(1) .gt. 0.999) ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if ( X(1) .gt. 0.0999) then  ! *******************************************              
      write(A1,7693) X(1)
 7693 format(3x,F4.3)
      else! if ( X(1) .gt. 9.99) ***********************************************
      if ( X(jdet) .gt. 0.00999) then ! """"""""""""""""""""""""""""""""""""""""             
      write(A1,7934) X(1)
 7934 format(2x,F5.4)
      else ! if ( X(jdet) .gt. 0.00999) """"""""""""""""""""""""""""""""""""""""
      write(A1,7976) X(1)
 7976 format(F7.4)
      endif ! if ( X(jdet) .gt. 0.00999) """""""""""""""""""""""""""""""""""""""
      endif ! if ( X(1) .gt. 0.0999) *******************************************
      endif ! ! if ( X(1) .gt. 0.999) ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      endif ! if ( X(1) .gt. 9.99) .............................................
      if ( x(1) .gt. 999.9) then ! ---------------------------------------------
      write(A1,8081) int(x(1)) ! -----------------------------------------------
 8081 format(i7) ! -------------------------------------------------------------
      endif ! if ( x(1) .gt. 999.9) --------------------------------------------
      call change colour of text (18) ! light grey
      if ( iscreen .lt. 3 ) write( *,9878)A1
 9878 format(5X,'Measured river flow',19x,A7)
      call set screen text colour
      write(09,9878)A1
      write(33,9878)A1

      else ! when ( jfk .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++++++
      write(09,5632)A1
      write(33,5632)A1
 5632 format(5X,'... measured river flow',15x,A7)
      endif ! if ( jfk .gt. 0 ) ++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( JT(feeture) .eq. 4 ) ========================================
 8356 continue

*     write out the loads at this feature --------------------------------------
      if ( ical13 .eq. 0 ) then
      call write out the river loads ! at this feature
      endif

      return
      end

     
      
      
      
      subroutine set up output of calculated means or percentiles
      include 'COMMON DATA.FOR'

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
    9 format(1pe7.0)
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
      include 'COMMON DATA.FOR'

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
    9 format(1pe7.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
      end


      
      subroutine set up XA arrays (iz)
      include 'COMMON DATA.FOR'

      zeroobs = 0.0
      iz = 1
      do jdet = 1, MP10
      if ( QTYPE (jdet) .ne. 4 ) zeroobs = zeroobs + XA(jdet)
      enddo
      if ( zeroobs .lt. 0.0000001) iz = 0
      
      do jdet = 1, MP10

*     initialise the arrays ----------------------------------------------------
      Sevenchars(jdet) = '      -'
      if ( QTYPE (jdet) .ne. 4 ) then

      jkl = JQ(feeture)
      if ( jkl .gt. 0 ) then
      if ( PDRC(jkl,1) .ne. -1 ) then

*     greater than 1,000,000 ---------------------------------------------------
      if ( XA(jdet) .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) XA(jdet)
    1 format(1pe7.0)
      goto 99
      endif

*     less than 0.000000002 ----------------------------------------------------
      if ( XA(jdet) .lt. 0.000000002) then
      write(Sevenchars(jdet),2) int (XA(jdet))
      goto 99
      endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( XA(jdet) .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (XA(jdet))
    2 format(i7)
      goto 99
      endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( XA(jdet) .gt. 9.99) then
      write(Sevenchars(jdet),3) XA(jdet)
    3 format(F7.1)
      goto 99
      endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( XA(jdet) .gt. 0.999) then       
      write(Sevenchars(jdet),4) XA(jdet)
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( XA(jdet) .gt. 0.0999) then       
      write(Sevenchars(jdet),5) XA(jdet)
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
      if ( XA(jdet) .gt. 0.00999) then       
      write(Sevenchars(jdet),6) XA(jdet)
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
      if ( XA(jdet) .gt. 0.000999) then               
      write(Sevenchars(jdet),7) XA(jdet)
    7 format(1x,f6.5)
      goto 99
      endif

*     less than 0.001 and greater than 0.00001 ---------------------------------
      if ( XA(jdet) .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) XA(jdet)
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) XA(jdet)
    9 format(1pe7.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
      end


      subroutine set up x99
      include 'COMMON DATA.FOR'

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
    9 format(1pe7.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
      end


      subroutine set up x90
      include 'COMMON DATA.FOR'

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
    9 format(1pe7.0)
   99 continue 

      endif
      endif
      endif
      enddo
      return
      end


      subroutine set up xtargets (notargit) ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      include 'COMMON DATA.FOR'

      IQSfeet = IFRQS(feeture)
      notargit = 0
      
*     check for a river quality target -----------------------------------------
      do jdet = 1, NDET
      Sevenchars(jdet) = '      -'
      targit = 0.0 ! set the target --------------------------------------------
      
      if ( QTYPE (jdet) .ne. 4 .and. IQSfeet .gt. 0 ) then ! -------------------
      targit = RQS (IQSfeet,jdet) ! target for feature -------------------------
      notargit = 1
      
      IQSreach = EQS reach (IREACH,jdet)
      if ( IQSreach .gt. 0 ) then
      if ( background class .eq. 1 ) then ! set the default target as Class 2 --
      targit = class limmits2 (2,jdet) 
      notargit = 1
      endif
      if ( IQSfeet .gt. 0 ) targit = RQS (IQSfeet,jdet) ! set target for feature
      if ( IQSreach .gt. 0 ) then ! over-write with reach-specific value -------
      do ic = 1, nclass - 1
      if ( MRQS(jdet). ne. 4 .and. MRQS(jdet). ne. 5) then
      if ( class limmits (ic,jdet) .lt. -1.0e-8 ) then
      targit = abs (class limmits (ic,jdet))
      notargit = 1
      endif
      endif ! if ( MRQS(jdet). ne. 4 etc ---------------------------------------
      enddo
      endif ! if ( IQSreach .gt. 0 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )
          
*     greater than 1,000,000 ---------------------------------------------------
      if ( targit .gt. 999999.45 ) then
      write(Sevenchars(jdet),1) targit
    1 format(1pe7.0)
      goto 99
      endif

*     less than 0.000000002 ----------------------------------------------------
      if ( targit .lt. 0.000000002) then
      !write(Sevenchars(jdet),2) int (targit)
      goto 99
      endif

*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( targit .gt. 9999.45 ) then
      write(Sevenchars(jdet),2) int (targit)
    2 format(i7)
      goto 99
      endif

*     less than 10,000 and greater than 100 ------------------------------------
      if ( targit .gt. 9.99) then
      write(Sevenchars(jdet),3) targit
    3 format(F7.1)
      goto 99
      endif

*     less than 100 and greater than 1 -----------------------------------------
      if ( targit .gt. 0.999) then       
      write(Sevenchars(jdet),4) targit
    4 format(F7.2)
      goto 99
      endif

*     less than 1 and greater than 0.1 -----------------------------------------
      if ( targit .gt. 0.0999) then       
      write(Sevenchars(jdet),5) targit
    5 format(3x,F4.3)
      goto 99
      endif

*     less than 0.1 and greater than 0.01 --------------------------------------
      if ( targit .gt. 0.00999) then       
      write(Sevenchars(jdet),6) targit
    6 format(2x,F5.4)
      goto 99
      endif

*     less than 0.0100 and greater than 0.001 ----------------------------------
      if ( targit .gt. 0.000999) then               
      write(Sevenchars(jdet),7) targit
    7 format(1x,f6.5)
      goto 99
      endif
      
*     less than 0.001 and greater than 0.00001 ---------------------------------
      if ( targit .gt. 0.00000999) then               
      write(Sevenchars(jdet),8) targit
    8 format(f7.6)
      goto 99
      endif
     
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(jdet),9) targit
    9 format(1pe7.0)
   99 continue 

      endif ! if ( QTYPE (jdet) .ne. 4 )
      enddo ! do jdet = 1, NDET
      
      return
      end ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      
      
      subroutine set up xeff
      include 'COMMON DATA.FOR'

      IQSfeet = IFRQS(feeture)

      do JP = 1,MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(JP) = '      -'
      if ( QTYPE (JP) .ne. 4 ) then  
      if ( pollution data(IF,JP,1) .gt. 1.0E-08 ) then

*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
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
      endif 
      enddo ! do ic = 1, nclass - 1
      endif ! if ( IQSreach .gt. 0 )
      endif ! if ( IQSfeet .gt. 0 .or. IQSreach .gt. 0 )

      if ( targit .gt. 0.000001 ) then 

*     greater than 1,000,000 ---------------------------------------------------
      if ( XEFF(JP) .gt. 999999.45 ) then
      write(Sevenchars(JP),1) XEFF(JP)
    1 format(1pe7.0)
      goto 99
      endif
*     less than 0.000000002 ----------------------------------------------------
      if ( XEFF(JP) .lt. 0.000000002 ) then
      write(Sevenchars(JP),2) int (XEFF(JP))
      goto 99
      endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( XEFF(JP) .gt. 9999.45 ) then
      write(Sevenchars(JP),2) int (XEFF(JP))
    2 format(i7)
      goto 99
      endif
*     less than 10,000 and greater than 100 ------------------------------------
      if ( XEFF(JP) .gt. 9.99 ) then
      write(Sevenchars(JP),3) XEFF(JP)
    3 format(F7.1)
      goto 99
      endif
*     less than 100 and greater than 1 -----------------------------------------
      if ( XEFF(JP) .gt. 0.999 ) then       
      write(Sevenchars(JP),4) XEFF(JP)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if ( XEFF(JP) .gt. 0.0999 ) then       
      write(Sevenchars(JP),5) XEFF(JP)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
      if ( XEFF(JP) .gt. 0.00999 ) then       
      write(Sevenchars(JP),6) XEFF(JP)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
      if ( XEFF(JP) .gt. 0.000999 ) then               
      write(Sevenchars(JP),7) XEFF(JP)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
      if ( XEFF(JP) .gt. 0.00000999 ) then               
      write(Sevenchars(JP),8) XEFF(JP)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(JP),9) XEFF(JP)
    9 format(1pe7.0)
   99 continue 

      endif ! if ( targit .gt. 0.000001 )
      endif ! if ( pollution data(IF,JP,1) .gt. 1.0E-08 )
      endif ! if ( QTYPE (JP) .ne. 4 )
      
      enddo
      return
      end


      subroutine set up xecm
      include 'COMMON DATA.FOR'

      IQSfeet = IFRQS(feeture)
      
      do JP = 1, MP10
*     initialise the arrays ----------------------------------------------------
      Sevenchars(JP) = '      -'
      if ( QTYPE (JP) .ne. 4 ) then  
      
      if ( pollution data(IF,JP,1) .gt. 1.0E-08 ) then
          
*     check for a river quality target -----------------------------------------
      targit = 0.0 ! set the target --------------------------------------------
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

      if ( targit .gt. 0.000001 ) then 

*     greater than 1,000,000 ---------------------------------------------------
      if ( XECM(JP) .gt. 999999.45 ) then
      write(Sevenchars(JP),1) XECM(JP)
    1 format(1pe7.0)
      goto 99
      endif
*     less than 0.000000002 ----------------------------------------------------
      if ( XECM(JP) .lt. 0.000000002 ) then
      write(Sevenchars(JP),2) int (XECM(JP))
      goto 99
      endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if ( XECM(JP) .gt. 9999.45 ) then
      write(Sevenchars(JP),2) int (XECM(JP))
    2 format(i7)
      goto 99
      endif
*     less than 10,000 and greater than 100 ------------------------------------
      if ( XECM(JP) .gt. 9.99 ) then
      write(Sevenchars(JP),3) XECM(JP)
    3 format(F7.1)
      goto 99
      endif
*     less than 100 and greater than 1 -----------------------------------------
      if ( XECM(JP) .gt. 0.999 ) then       
      write(Sevenchars(JP),4) XECM(JP)
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if ( XECM(JP) .gt. 0.0999 ) then       
      write(Sevenchars(JP),5) XECM(JP)
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
      if ( XECM(JP) .gt. 0.00999 ) then       
      write(Sevenchars(JP),6) XECM(JP)
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
      if ( XECM(JP) .gt. 0.000999 ) then               
      write(Sevenchars(JP),7) XECM(JP)
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
      if ( XECM(JP) .gt. 0.00000999 ) then               
      write(Sevenchars(jdet),8) XECM(JP)
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(Sevenchars(JP),9) XECM(JP)
    9 format(1pe7.0)
   99 continue 

      endif ! if ( targit .gt. 0.000001 )
      endif ! if ( pollution data(IF,JP,1) .gt. 1.0E-08 )
      endif
      
      enddo ! do JP = 1, MP10
      return
      end



      subroutine set up flowa
      include 'COMMON DATA.FOR'

      flowchars(1) = '      -' ! initialise
      !if ( JT(feeture) .eq. 7 ) return
      
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
    9 format(1pe7.0)
   99 continue 

      return
      end



      subroutine set up flow95
      include 'COMMON DATA.FOR'

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
    9 format(1pe7.0)
   99 continue 

      return
      end

      
      
      subroutine write preliminary headings for the reach
      include 'COMMON DATA.FOR'

      if ( ical .eq. 3 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++
      if ( nobigout .le. 0 ) then ! ============================================
      call sort format 2 (flow(1),flow(2))
      do jper = 1,ndet
      if ( qtype(jper) .ne. 4 ) then
      if ( flow(1). gt. 0.00000001 ) then ! ------------------------------------
      write(170+jper,7003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit
      else
      write(170+jper,7013)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit
      endif ! if ( flow(1). gt. 0.00000001 ) -----------------------------------
      endif ! if ( qtype(jper).ne.4 )
      enddo ! do jper = 1,ndet
      endif !if ( nobigout .le. 0 ) then ! =====================================
      endif ! if ( ical13 .eq. 0 ) then ! ++++++++++++++++++++++++++++++++++++++
      
      if ( nobigout .le. 0 ) then ! ============================================
      if ( ical13 .eq. 0 .or. ical .eq. 1 ) then ! +++++++++++++++++++++++++ GAP
      call sort format 2 (flow(1),flow(2))
      do jper = 1, ndet
      if ( qtype(jper) .ne. 4 ) then
      if ( flow(1). gt. 0.00000001 ) then ! ----------------------------- Ci.GAP
      write(170+jper,7003)RNAME(IREACH), ! ------------------------------ Ci.GAP
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit ! -------- Ci.GAP
 7003 format(/////110('=')/'Calculation for the reach named: ', ! ------- Ci.GAP
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x,
     &' Flow at head of Reach:  Mean =',a10,1x,a4/30X, ! ---------------- Ci.GAP
     &'        95-percentile low flow =',a10,1x,a4/110('='))
      else
      write(170+jper,7013)RNAME(IREACH), ! ------------------------------ Ci.GAP
     &IREACH,RLENGTH(IREACH),valchars10,funit
 7013 format(/////110('=')/'Calculation for the reach named: ', ! ------- Ci.GAP
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x,
     &' Flow at head of Reach:  Mean =',a10,1x,a4)
      endif ! if ( flow(1). gt. 0.00000001 ) ---------------------------- Ci.GAP
      endif ! if ( qtype(jper).ne.4 )
      enddo ! do jper = 1, ndet
      endif ! if ( ical13 .eq. 0 .or. ical .eq. 1 ) then ! +++++++++++++++++ GAP
      
      if ( ical13 .eq. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++
      !call sort format 2 (flow(1),flow(2))
      if ( flow(1). gt. 0.00000001 ) then ! ------------------------------------
      !if ( MONF .gt. 1 ) call write shots for river flow
      write(01,1003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit
 1003 format(///110('=')/'Calculation for the reach named: ', ! ------------ OUT
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x,
     &' Flow at head of Reach:  Mean =',a10,1x,a4/30X, ! ------------------- OUT
     &'        95-percentile low flow =',a10,1x,a4)
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1003)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit,valchars11,funit
      endif
      enddo
      else ! -------------------------------------------------------------------
      !if ( MONF .gt. 1 ) call write shots for river flow
      write(01,1013)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit
 1013 format(///110('=')/'Calculation for the reach named: ', ! ------------ OUT
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x,
     &' Flow at head of Reach:  Mean =',a10,1x,a4) ! ----------------------- OUT
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1013)RNAME(IREACH),
     &IREACH,RLENGTH(IREACH),valchars10,funit
      endif
      enddo
      endif ! if ( flow(1). gt. 0.00000001 ) -----------------------------------

      if ( flow(1). gt. 0.00000001 ) then ! ==================================== 
      call sort format 2 (Flow (3),Flow(4))
      write(01,2)valchars10,FUNIT,valchars11,FUNIT
    2 format(
     &46x,'90% exceedence =',a10,1x,A4/
     &46x,'99% exceedence =',a10,1x,A4/110('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2)valchars10,FUNIT,valchars11,FUNIT
      endif
      enddo
      else
      write(01,3)
    3 format(110('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,3)
      enddo
      endif ! if ( flow(1). gt. 0.00000001 ) ===================================

*     ==========================================================================
      do ic = 1, ndet
      if ( QTYPE (ic) .ne. 4 ) then
      write(100+ic,1203)RNAME(IREACH), ! ----------------------------------- MON
     &IREACH,RLENGTH(IREACH)
 1203 format(///110('=')/'Calculation for the reach named: ', ! ------------ MON
     &A16,43X,'Reach Number',I6/110('=')/
     &'Length of Reach: ',F6.1,' km',5x/110('='))
      endif
      enddo
      endif ! if ( ical13 .eq. 0 ) +++++++++++++++++++++++++++++++++++++++++++++ 
      endif ! if ( nobigout .le. 0 ) =========================================== 
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
   10 format(1pe10.2)
      return
      endif
      if ( abs(xnum) .gt. 999.9 ) then
      write(tench,9)int(xnum)
    9 format(I10)
      return
      endif
      if ( abs(xnum) .gt. 99.9 ) then
      write(tench,2)xnum
    2 format(F10.1)
      return
      endif
      if ( abs(xnum) .gt. 9.99 ) then
      write(tench,3)xnum
    3 format(F10.2)
      return
      endif
      
*     if less than 10 and greater than 1 ---------------------------------------
      if ( abs(xnum) .gt. 0.999 ) then
      write(tench,4)xnum
    4 format(F10.2)
      return
      endif
      
*     if less than 1 and greater than 0.1 --------------------------------------
      if ( abs(xnum) .gt. 0.0999 ) then
      write(tench,5)xnum
    5 format(F10.3)
      return
      endif
      
*     if less than 0.1 and greater than 0.01 -----------------------------------
      if ( abs(xnum) .gt. 0.0099 ) then
      write(tench,6)xnum
    6 format(F10.5)
      return
      endif
      
*     if less than 0.01 and greater than 0.00001 -------------------------------
      if ( abs(xnum) .gt. 0.00001 ) then
      write(tench,7)xnum
    7 format(F10.5)
      else
      write(tench,9)int(xnum)
      endif
      
      return
      end
      
      
      subroutine set format for printout2 (ninth,xnum)
      character *9 ninth
      ninth = '      ---'
      if ( iabs(int(xnum)) .gt. 99999999 ) then
      write(ninth,10)xnum
   10 format(1pe9.2)
      return
      endif
      if ( abs(xnum) .gt. 149.9 ) then
      write(ninth,9)int(xnum)
    9 format(I9)
      return
      endif
      if ( abs(xnum) .gt. 9.9 ) then
      write(ninth,2)xnum
    2 format(F9.1)
      return
      endif
      if ( abs(xnum) .gt. 0.999 ) then
      write(ninth,4)xnum
    4 format(F9.2)
      return
      endif
      if ( abs(xnum) .gt. 0.0999 ) then
      write(ninth,5)xnum
    5 format(F9.3)
      return
      endif
      if ( abs(xnum) .gt. 0.0099 ) then
	  write(ninth,6)xnum
    6 format(F9.4)
      return
      endif
      if ( abs(xnum) .gt. 0.00099 ) then
      write(ninth,7)xnum
    7 format(F9.5)
      return
      endif
      
      write(ninth,8)xnum+0.00000001
    8 format(F9.6)
      
      return
      end

      
      
      subroutine set format for printout3 (eighth,xnum)
      character *8 eighth
      eighth = '     ---'
      if ( iabs(int(xnum)) .gt. 99999999 ) then
      write(eighth,10)xnum
   10 format(1pe8.1)
      return
      endif
      if ( abs(xnum) .gt. 149.9 ) then
      write(eighth,9)int(xnum)
    9 format(I8)
      return
      endif
      if ( abs(xnum) .gt. 9.9 ) then
      write(eighth,2)xnum
    2 format(F8.1)
      return
      endif
      if ( abs(xnum) .gt. 0.999 ) then
      write(eighth,4)xnum
    4 format(F8.2)
      return
      endif
      if ( abs(xnum) .gt. 0.0999 ) then
      write(eighth,5)xnum
    5 format(F8.3)
      return
      endif
      if ( abs(xnum) .gt. 0.0099 ) then
      write(eighth,6)xnum
    6 format(F8.4)
      return
      endif
      write(eighth,7)xnum
    7 format(F8.5)
      
      return
      end


      
      subroutine initialise the running totals of load
      include 'COMMON DATA.FOR'

*     initialise the running totals of load ------------------------------------
      do ii = 1, ndet
      check total (ii) = 0.0

      do J1 = 1, N13

*     total load introduced by natural purification ----------------------------
      TNLOADUP2(ii,J1) = 0.0

*     total load removed by natural purification -------------------------------
      TNLOADDN2(ii,J1) = 0.0

*     total loads removed by gap filling for river flows -----------------------
      TILOADDN2(ii,J1) = 0.0

*     total load removed by gap filling for river quality ----------------------
      TALOADDN2(ii,J1) = 0.0

*     total removed by abstractions --------------------------------------------
      TBLODE2 (ii,J1) = 0.0
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
      propeff60 (idet) = 0.0
      propeff61 (idet) = 0.0
      propeff62 (idet) = 0.0
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
      prop50    (idet) = 0.0
      prop52    (idet) = 0.0
      prop54    (idet) = 0.0
      prop56    (idet) = 0.0
      prop58    (idet) = 0.0
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

      do iworks = 1, NUED ! ================================================ 160 
      do JP = 1,ndet ! ---------------------------------------------------------
      do is = 1,ns
      TELODEshots(iworks,JP,is) = 0.0
      enddo ! do is = 1,ns -----------------------------------------------------
      enddo ! do JP = 1,ndet ---------------------------------------------------
      enddo ! do iworks = 1,NUED =========================================== 160
      
      return
      end
      
      
      
      subroutine initialise the loads for works and bodies
      include 'COMMON DATA.FOR'

*     initialise the running totals of load ------------------------------------
      do ii = 1, ndet
      check total (ii) = 0.0

      do J1 = 1, N13
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments -------------------------------
      TWLOADS(ibodies,ii,J1) = 0.0 ! initialise the running totals of load
      do ip = 1, n2prop
*     breakdown of monthly loads from upstream sub-catchments - -----------------
      TWLOADSAPP(ibodies,ii,J1,ip) = 0.0 ! initialise the running totals 
      enddo ! do ip = 1, n2prop
      enddo ! do ibodies = 1, NUW
      enddo ! do J1 = 1, N13


*     load shots from sub-catchments -------------------------------------------
      do ibodies = 1, NUW
      do is = 1, 2000
      TABODYshots1(ibodies,ii,is) = 0.0
      TABODYshots2(ibodies,ii,is) = 0.0
      TABODYshots3(ibodies,ii,is) = 0.0
      enddo ! is = 1, MS
      enddo ! do ibodies = 1, NUW
*     --------------------------------------------------------------------------


      do iworks = 1, NUED
      TELOADAV(iworks,ii) = 0.0 !--------- average loads from upstream effluents
      do is = 1, NS ! 33333333333333333333333333333 apportionment of percentiles
      TELODEshots(iworks,ii,is) = 0.0 ! ------------------ shots from discharges
      enddo ! do is = 1, NS 33333333333333333333333 apportionment of percentiles
      
      enddo ! iworks = 1, NUED
      enddo ! 1, ndet
      
      do ibodies = 1, NUW
      TWlength (ibodies) = 0.0 ! length of river in a water body
      enddo

      return
      end

      
      
      subroutine initialise classification etc
      include 'COMMON DATA.FOR'

      if ( nobigout .le. 0 ) then ! --------------------------------------------
      write(01,4590)
 4590 format(////77('=')/
     &'THE MAIN CALCULATIONS FOLLOW ...  '/77('='))
      write(01,4891) RNAME(jreach(1))
 4891 format('Calculations for the first reach ... ',a16/77('='))
      do ic = 1,ndet
      if ( QTYPE(ic) .ne. 4 ) then
      write(200+ic,1891) RNAME(jreach(1))
 1891 format(//77('=')/'Calculations for the first reach ... ',
     &a16/77('='))
      endif
      enddo
      endif ! if ( nobigout .le. 0 ) -------------------------------------------

      if (JT(JNEXT) .eq. 10 .or. JT(JNEXT) .eq. 11 .or. 
     &    JT(JNEXT) .eq. 20 .or. JT(JNEXT) .eq. 21 .or.
     &    JT(JNEXT) .eq. 45 .or.
     &    JT(JNEXT) .eq. 22 .or. JT(JNEXT) .eq. 23) then

      write(01,1475) rname(ireach)
      write( *,1475) rname(ireach)
      write(09,1475) rname(ireach)
      write(33,1475) rname(ireach)
      write(72,1475) rname(ireach)
 1475 format(77('-')/'*** Missing Feature for Reach - ',a16/
     &'*** No flow and quality data for the Head of a Reach whose ',
     &'top forms an'/'*** upstream boundary of the catchment. The ',
     &'first feature on the Reach must'/'*** be Type 10, 11 or ',
     &'45 etc ... SIMCAT has stopped .... '/77('-'))
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      return
      else
      call stop
      endif
      endif

*     set the counter of the number of effluent discharges ---------------------
      kountworks = 0 ! at the start of calculations ----------------------------
      need works = 0
      kill works = 0
*     set the counter of the number of sub-catchments --------------------------
      kount bodies = 0 ! at the start of the calculations ----------------------

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
      totals in classes (iclass, idet) = 0.0
      enddo
      totals over all (iclass) = 0.0
      enddo

      return
      end

      
      
      subroutine identify virtual reaches
      include 'COMMON DATA.FOR'
      character *07 virt
      virtualreach = 0
      virt = "Good   "
      write(virt,2177)Rname(ireach)
 2177 format(a7)
      if (virt .eq. "virtual" ) Virtualreach = 1
      if (virt .eq. "Virtual" ) Virtualreach = 1
      if (virt .eq. "VIRTUAL" ) Virtualreach = 1
      return
      end
      
      

      subroutine set up statistics (jds,xxx)
      include 'COMMON DATA.FOR'

      Sevenchars(jds) = '      -' ! initialise
      
*     greater than 1,000,000 ---------------------------------------------------
      if ( xxx .gt. 999999.45 ) then
      write(Sevenchars(jds),1) xxx
    1 format(1pe7.0)
      goto 99
      endif
*     less than 0.000000002 ----------------------------------------------------
      if ( xxx .lt. 0.000000002) then
      write(Sevenchars(jds),2) int (xxx)
      goto 99
      endif
*     less that 1,000,000 and greater than 10,000 ------------------------------
      if (xxx .gt. 9999.45 ) then
      write(Sevenchars(jds),2) int (xxx)
    2 format(i7)
      goto 99
      endif
*     less than 10,000 and greater than 100 ------------------------------------
      if (xxx .gt. 9.99) then
      write(Sevenchars(jds),3) xxx
    3 format(F7.1)
      goto 99
      endif
*     less than 100 and greater than 1 -----------------------------------------
      if (xxx .gt. 0.999) then       
      write(Sevenchars(jds),4) xxx
    4 format(F7.2)
      goto 99
      endif
*     less than 1 and greater than 0.1 -----------------------------------------
      if (xxx .gt. 0.0999) then       
      write(Sevenchars(jds),5) xxx
    5 format(3x,F4.3)
      goto 99
      endif
*     less than 0.1 and greater than 0.01 --------------------------------------
      if (xxx .gt. 0.00999) then       
      write(Sevenchars(jds),6) xxx
    6 format(2x,F5.4)
      goto 99
      endif
*     less than 0.0100 and greater than 0.001 ----------------------------------
      if (xxx .gt. 0.000999) then               
      write(Sevenchars(jds),7) xxx
    7 format(1x,f6.5)
      goto 99
      endif
*     less than 0.001 and greater than 0.00001 ---------------------------------
      if (xxx .gt. 0.00000999) then               
      write(Sevenchars(jds),8) xxx
    8 format(f7.6)
      goto 99
      endif
*     zero to 1.e-5 ------------------------------------------------------------
      write(flowchars(1),9) xxx
    9 format(1pe7.0)
   99 continue 

      return
      end