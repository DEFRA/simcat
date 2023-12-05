*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File Name: GAP FILL CREATION.FOR ... 4087 lines
*     ==========================================================================
*     This file deals with gap filling - calculating the differences between
*     SIMCAT's calculations and the measured distributions of river flows and 
*     river quality ... 
*     --------------------------------------------------------------------------
*     This file contains 16 sub-routines, including:
*     --------------------------------------------------------------------------
*          ...... undertake gap filling ====================================== 1
*          It uses the following to perform gap filling ...
*          ---------------------------------------------------------------------
*          ...... flow gap filling =========================================== 2
*          Compute the diffuse inflows needed to fit observed river flow ...
*          ---------------------------------------------------------------------
*          ...... flowfit ==================================================== 3
*          Calculate the adjustments needed for flow gap filling ...
*          ---------------------------------------------------------------------
*          ...... rank ======================================================= 4
*          Sort arrays VAL into ascending order of magnitude. Store the previous
*          location of ranked values ...
*          ---------------------------------------------------------------------
*          ...... perform gap filling for quality ============================ 5
*          Compute adjustments to loads needed to produce a fit with the quality
*          observed at a monitoring point ...
*          ---------------------------------------------------------------------
*          ...... add the loads from flow gap filling ======================== 6
*          Add in the gap-filled inflows ...
*          ---------------------------------------------------------------------
*          ...... add the gap filled river flows ============================= 7
*          Add in the diffuse inflows calculated by gap filling. These will have 
*          been calculated in ROUTINE [flow gap filling] under ICAL=1 ...
*          ---------------------------------------------------------------------
*          ...... extrapolate gap filled river flows ========================= 8
*          Add in the effects of gap filling. These will have been calculated  
*          by ROUTINE [flow gap filling].  This action applies the effects 
*          downstream of the gap filling point (flow gauge) ... 
*          ---------------------------------------------------------------------
*          ...... gap fill simulation ======================================== 9
*          ---------------------------------------------------------------------
*          ...... dump ====================================================== 10
*          ---------------------------------------------------------------------
*          ...... reload ==================================================== 11
*          ---------------------------------------------------------------------
*          ...... quality fit =============================================== 12
*          ---------------------------------------------------------------------
*          ...... calfit ==================================================== 13
*          ---------------------------------------------------------------------
*          ...... insert quality gap filling ================================ 14
*          ---------------------------------------------------------------------
*          ...... extrapolate quality gap filling =========================== 15
*          Add in the effects of gap filling. These will have been calculated  
*          by ROUTINE [perform gap filling for quality].  This action applies  
*          the effects downstream of the monitoring point ... 
*          ---------------------------------------------------------------------
*          ...... statload ================================================== 16
*          ---------------------------------------------------------------------
*          =====================================================================

      subroutine undertake gap filling (JU)
      include 'COMMON DATA.FOR'
      dimension oldshots(mp10,ms),Y(MS),OLDc(mp10),oldL(mp10)

      krach = 0 ! ---------------------------------------------------------- 160
      if ( JU .lt. 0 ) then
      krach = JU    
      JU = - JU
      endif ! -------------------------------------------------------------- 160

      if ( ical .eq. 07 .or. ical .eq. 08 .or. ical .eq. 09 ) then ! 99999999999
      if ( no gap filling 78 .eq. 1 ) return
      endif

      goto (1,2,3,4,4,4,4,4,4), ICAL ! 99999999999999999999999999999999999999999

*     compute the diffuse inflows needed to fit observed river flow -----------1
*     distributions -----------------------------------------------------------1
    1 call flow gap filling (JU) ! --------------------------------------------1
      return ! ----------------------------------------------------------------1

*     add in the gap filled inflows -------------------------------------------2
    2 continue
      call add the loads from flow gap filling (JU,0) ! gap filled inflows ----2
      call get summaries of river quality from the shots ! gap filling --------2
      call update summaries of contribution ! after additions from gap filling 2 
      return ! ----------------------------------------------------------------2

*     add in the gap filled inflows and ---------------------------------------3
*     compute adjustments to the loads needed to produce a fit with the -------3
*     quality observed at monitoring points -----------------------------------3
    3 continue ! ical = 3 gap filling for quality -----------------------------3

      call add the loads from flow gap filling (JU,0) ! ical = 3 --------------3
      if ( JQCAL(JU) .gt. 0 ) then ! check for gap filling data ---------------3
      JUgap = JU ! feature is used for Gap Filling ----------------------------3
      endif ! if ( JQCAL(JU) .gt. 0 ) -----------------------------------------3
      call perform gap filling for quality (JU) ! at feature number JU --------3
      return ! ----------------------------------------------------------------3

*     add in the gap filled inflows and quality -------------------------------4
    4 continue ! ical = 4 include gap filling for flow and quality ------------4

      call add the loads from flow gap filling (JU,0) ! -----------------------4
      call get summaries of river quality from the shots ! gap filling --------4
      call update summaries of contribution ! after additions from gap filling 4 
            
*     add river quality calculated for gap filling ----------------------------4
*     compute the mean loads before gap filling of quality --------------------4
      call load calculation ! -------------------------------------------------4

      do idet = 1, ndet ! -----------------------------------------------------4
      if ( qtype(idet) .ne. 4 ) then
      OLDC(idet) = C(idet,1)
      OLDL(idet) = LMcontrib(NTD,idet,1)
      do is = 1, ns
      old shots(idet,is) = xshots(idet,is)
      enddo
      endif ! if ( qtype(idet) .ne. 4 )
      enddo ! do idet = 1, ndet -----------------------------------------------4

*     add in the effects of gap filling ---------------------------------------4
      call insert quality gap filling (JU) ! after quality filling ------------4
      call update summaries of contribution ! after quality gap filling -------4

      do idet = 1,ndet ! ======================================================4
      if ( qtype(idet) .ne. 4 ) then ! ========================================4
      xchan = 1.0
      if ( oldL(idet) .gt. 0.0000001 ) then
      xchan = LMcontrib(NTD,idet,1)/oldL(idet)
      endif
      do ip = 1, n2prop ! breakdown of loads from upstream catchments ---------4
      twloadsapp(1,idet,i13,ip) = xchan * twloadsapp(1,idet,i13,ip) ! ---------4
      twloadsapp(2,idet,i13,ip) = xchan * twloadsapp(2,idet,i13,ip) ! ---------4
      enddo ! breakdown of loads from upstream catchments ---------------------4
      endif ! if ( qtype(idet) .ne. 4 ) ========================================
      enddo ! do idet = 1,ndet =================================================
      
      call get summaries of river quality from the shots ! after gap filling --4

      IQNAT = 0
      do idet = 1, ndet
      if ( qtype(idet) .ne. 4 ) then
      if ( dist(feeture) .gt. 0.0001 ) then
      if ( C(idet,1) .gt. 1.0e-10 ) then
      xxxQ = 100.0*(OLDC(idet)/AMAX1(1.0E-10,C(idet,1))-1.0)
      QNAT(idet)=INT(0.5+xxxQ)
      QNAT(idet)=min0(99999,QNAT(idet))
      if ( krach .eq. 0 ) then
      write(170+idet,1488)xxxQ,uname(feeture)
 1488 format('Change in mean quality from gap filling =',
     &f8.2,' %','     At ',a35)
      else
      write(170+idet,1388)xxxQ,rname(IREACH)
 1388 format('Change in mean quality from gap filling =',
     &f8.2,' %','     At the end of reach ... ',a16)
      endif
      IQNAT = IQNAT + iabs(QNAT(idet))
      endif
      endif ! if ( C(jdet,1) .gt. 1.0e-10 ) -----------------------------------4
      endif ! if ( qtype(idet) .ne. 4 )
      enddo ! do idet = 1, ndet
      
      if ( IQNAT .gt. 0 ) then    
      do idet = 1,ndet ! ------------------------------------------------------4
      Sevenchars(jdet) = '      -'
      if ( QTYPE(idet) .ne. 4 .and. QNAT(idet) .ne. -99 ) then ! --------------4
      write(Sevenchars(idet),7535)QNAT(idet)
 7535 format(I7)
      endif ! if ( QTYPE (idet) .ne. 4 .and. QNAT(idet) .ne. -99 ) ------------4
      enddo ! do jdet = 1,ndet ------------------------------------------------4

      call change colour of text (14) ! bright yellow
      write( *,1751)(Sevenchars(idet),idet=1,ndet)
      call set screen text colour
      write(09,1751)(Sevenchars(idet),idet=1,ndet)
      write(33,1751)(Sevenchars(idet),idet=1,ndet)
 1751 format(5X,'Gap Fill: % change in mean quality',11x,10a7)
      endif
      
      call load calculation ! compute the mean load after gap filling ---------4
      
*     compute the difference in mean load from gap filling ********************4
      do 37 idet = 1, ndet ! **************************************************4
      if ( qtype (idet) .ne. 4 ) then ! ***************************************4

      do J13 = 1, N13
      NSM (idet,J13) = 0
      xupload (idet,J13) = 0.0
      xdownload (idet,J13) = 0.0
      enddo

      do is = 1, ns
      imonth = qmonth (is) ! set the month for this shot
      do J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13

*     difference between the new and the old shot -----------------------------4
      xdiff = xshots (idet,is) - oldshots (idet,is)

      if ( xdiff .ge. 1.0e-10 ) then
*     accumulate the mean load added by gap filling ---------------------------4
      xupload (idet,K13) = xupload (idet,K13) + xdiff
      else
*     accumulate the mean load lost by gap filling ----------------------------4
      xdownload (idet,K13) = xdownload (idet,K13) + xdiff
      endif
      NSM (idet,K13) = NSM (idet,K13) + 1
      enddo
      enddo

      xdownload (idet,i13) = xdownload (idet,i13) / float (ns)
      xupload (idet,i13) = xupload (idet,i13) / float (ns)
      xnet = xupload (idet,i13) + xdownload (idet,i13) 

*     =========================================================================4
      if ( xnet .gt. 0.00001) then
      xupload (idet,i13) = xnet
      xdownload (idet,i13) = 0.0
      else
      xdownload (idet,i13) = xnet
      xupload (idet,i13) = 0.0
      endif
*     =========================================================================4

      TALOADDN2 (idet,i13) = TALOADDN2 (idet,i13)
     &                     + xdownload  (idet,i13)

      if ( munthly structure .eq. 1 ) then ! --------------- store monthly loads
      do J13 = 2, N13
      xupload (idet,J13) = xupload (idet,J13)/float(NSM (idet,J13))
      xdownload (idet,J13) = xdownload (idet,J13)/float(NSM (idet,J13))
      xnet = xupload (idet,J13) + xdownload (idet,J13)

*     =========================================================================4
      if ( xnet .gt. 0.00001) then
      xupload (idet,J13) = xnet
      xdownload (idet,J13) = 0.0
      else
      xdownload (idet,J13) = xnet
      xupload (idet,J13) = 0.0
      endif
*     =========================================================================4

      TALOADDN2 (idet,J13) = TALOADDN2 (idet,J13) !  load removed by gap filling
     &+ xdownload (idet,J13) 
      enddo
      endif ! fill monthly loads

      endif ! if ( qtype (idet) .ne. 4 ) **************************************4
   37 continue ! do 37 idet = 1, ndet *****************************************4
*     *************************************************************************4
      
      call update summaries of contribution
      
      call add up all the loads ! for all determinands

*     calculate the effect on accumulated loads -------------------------------4

      call add up all the loads ! for all determinands ------------------------4
      
      call write loads after gap filling of river quality A ! -----------------4
      call write loads after gap filling of river quality B ! -----------------4

      return
      end






*     gap filling for river flows ----------------------------------------------
      subroutine flow gap filling (JU)
      include 'COMMON DATA.FOR'
      dimension IRANK(MS),WORK(MS)

*     ==========================================================================
*     check for the end of the reach -------------------------------------------
      if ( IEND .eq. 1 ) then
*     extrapolate the corrections to the end of the Reach ----------------------
      if ( NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU) ! the end of the Reach ------
      endif
      return
      endif
*     ==========================================================================

*     do the headings for the file for flow gap filling ----------------------74
      write(74,8558)
 8558 format(120('*'))
      jcode = JREACH(JU)
      if ( iforce gap fill .eq. 1 ) jcode = - jcode
      write(74,8559)Jcode,DISTP,UNAME(JU),RNAME(JREACH(JU))
 8559 format(I5,F12.6,30X,A44,1X,A16)

*     check if there is any flow data for the flow gap filling -----------------
      IF = IABS ( JFCAL(JU) )

*     if JFCAL(JU) equals zero SIMCAT will extrapolate if required -------------
*     but no new gap filling is required at this Feature -----------------------
      if ( IF .lt. 1 ) then
      if ( KFCAL(JU) .gt. 0 .and. NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU) ! at this Feature -----------
      endif
      return
      endif

*     no flow data specified for gap filling -----------------------------------
      if ( IF .gt. 0 ) then
*     set the target mean flow for gap filling  --------------------------------
      FM target = F(IF,1)
      endif

*     check for non-zero flows -------------------------------------------------
      if ( FM target .lt. 1.0E-10 ) then
      if ( KFCAL(JU) .gt. 0 .and. NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU) ! FM less than zero ----------
      endif
      return
      endif

*     check if downstream extrapolation is required ----------------------------
*     JFCAL(JU) less than zero supresses downstream extrapolation --------------
*     JFCAL(JU) greater than zero allows extrapolation -------------------------
      if suppress flow = 0
      NEXF = 0
      IFF = JFCAL(JU)
      if ( IFF .lt. 0 ) then
*     extrapolation not required ... turn on switches --------------------------
      if suppress flow = 1
      NEXF = 1
      endif
*     suppress if the gap filling distance is too small ------------------------
      if ( dcalflow .lt. 1.0 ) NEXF=1

*     store the current current calculated flow shot values in FMX -------------
      do IS = 1, NS
      FMX(IS) = FMS(IS)
      enddo

      call calculate summaries of river flow
      FM current = FLOW(1)
      FP current = FLOW(2)

*     Set the 95-percentile gap filling target --------------------------------2
      FP target = F(IF,2)
*     check for non-parametric river flow data --------------------------------3
*     if ( PDRF(IF) .ne. 4 ) then
*     if suppress flow = 1
*     NEXF = 1
*     else
      if ( FP target .lt. 1.0E-10 ) then
      call change colour of text (10) ! green
      write( *,8957)uname(JU)
 8957 format(
     &'*** ILLEGAL flow for gap filling ... ',
     &14x,'...',7x,'at ',A40,1x,'Zero has been re-set',5('.'))
      call set screen text colour
      if ( nobigout .le. 0 ) write(01,8557)
      write(09,8557)
      write(33,8557)
 8557 format(77('-')/
     &'*** ILLEGAL flow data for gap filling ... '/
     &'*** A zero low flow is not allowed ... '/
     &'*** This has been reset to 0.1% of the mean '/77('-'))
      FP target = 0.001 * FM target
      F(IF,2) = FP target
      if suppress flow = 1
      NEXF = 1
      if ( nobigout .le. 0 ) write(01,8754)F(IF,1),FUNIT,F(IF,2),FUNIT,
     &FM current,FUNIT,FP current,FUNIT
 8754 format(/22x,55('+')/
     &22x,'Gap filling for river flow ...'/22x,55('+')/
     &33x,'          Required mean flow =',F9.2,1x,A4/
     &33x,'      Required 95-percentile =',F9.2,1x,A4/
     &33x,'           Current mean flow =',F9.2,1x,A4/
     &33x,'       Current 95-percentile =',F9.2,1x,A4)
      else ! if ( FP target .lt. 1.0e-10 )
      if ( nobigout .le. 0 ) write(01,8754)F(IF,1),FUNIT,F(IF,2),FUNIT,
     &FM current,FUNIT,FP current,FUNIT
      endif ! !   if ( FP target .lt. 1.0e-10 )
*     endif

*     generate correction values for each shot as function of river distance --- 

*     first use FLOWFIT to generate the target shots and rank them according to- 
*     the current shots --------------------------------------------------------
      call flowfit (JU,IRANK)

*     compute mean and 95-percentile of the target shots -----------------------
      call calculate summaries of river flow

*     factor for making current mean equal the required mean -------------------
      FM = amin1 ( 999.9, FM target/FM current )

*     scale the flows to give the required mean --------------------------------
      do IS = 1, NS
      FMS(IS) = FMX(IS) * FM
      enddo

      call calculate summaries of river flow

*     scale flows to give the required 95-percentile ---------------------------
*     first compute the factor -------------------------------------------------
      if ( flow(2) .lt. 0.0001*flow(1) ) then
      flow(2) = 0.0001 * flow(1)
      endif
      FP = amin1 ( 999.9, FP target/FLOW(2) )

*     then take a copy of the shots --------------------------------------------
      do IS=1,NS
      WORK(IS)=FMS(IS)
      enddo

*     sort the flows into ascending order --------------------------------------
      call rank (WORK,IRANK,NS)

      do 9048 IS=1,NS
*     adjust low flow shots ----------------------------------------------------

      if ( FP .gt. 1.0 .and. FP .lt. 2.5 ) then
*     scale down ---------------------------------------------------------------
      if ( 0.95 * work (is) .gt. FP target ) goto 9048
      XTRA = (FP-1.0) * WORK(IS)
      WORK(IS) = WORK(IS) + XTRA

      else
*     scale up -----------------------------------------------------------------
      if ( IS .gt. k90 ) goto 9048
      XTRA = (FP-1.0) * WORK(IS)
      WORK(IS) = WORK(IS) + XTRA
      endif

*     compensate by adding to high flow shots. This will preserve the mean -----

      KS = NS + 1 - IS
      WORK(KS) = WORK(KS) - XTRA
 9048 continue

*     put flows back in the original sequence ----------------------------------
      do IS=1,NS
      KS = IRANK(IS)
      FMS(KS) = WORK(IS)
      enddo

      call calculate summaries of river flow

      Fcheck1 = amin1 ( 999.9, Flow(1) / F(IF,1) )
      Fcheck2 = amin1 ( 999.9, Flow(2) / F(IF,2) )


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if (Fcheck2 .lt. 0.99 ) then
      FP = amin1 ( 999.9, 1.0 / Fcheck2 )

*     then take a copy of the shots --------------------------------------------

      do IS=1,NS
      WORK(IS)=FMS(IS)
      enddo

*     Sort the flows into ascending order --------------------------------------
      call rank (WORK,IRANK,NS)

      do 9948 IS=1,NS

*     adjust low flow shots ----------------------------------------------------

      if ( FP .gt. 1.0 .and. FP .lt. 2.5 ) then
*     scale down ---------------------------------------------------------------
      if ( 0.95 * work (is) .gt. FP target ) goto 9948
      XTRA = (FP-1.0) * WORK(IS)
      WORK(IS) = WORK(IS) + XTRA

      else
*     scale up -----------------------------------------------------------------
      If ( IS .gt. k90 ) goto 9948
      XTRA = (FP-1.0) * WORK(IS)
      WORK(IS) = WORK(IS) + XTRA
      endif

*     compensate by adding to high flow shots. This will preserve the mean -----

      KS=NS+1-IS
      WORK(KS)=WORK(KS)-XTRA
 9948 continue

*     put flows back in the original sequence ----------------------------------
      do IS=1,NS
      KS = IRANK(IS)
      FMS(KS) = WORK(IS)
      enddo

*     call calculate summaries of river flow ! ----------------------------------

      Fcheck1 = amin1 ( 999.9, Flow(1) / F(IF,1) )
      Fcheck2 = amin1 ( 999.9, Flow(2) / F(IF,2) )

      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     set initial value for the control of extrapolation of adjustments to river 
*     flow ...  zero means it will happen --------------------------------------
      NEXF=0

      if ( nobigout .le. 0 ) write(01,9648)FM,FP
 9648 format(22x,55('+')/
     &22x,'   Fit attempted by scaling the data by -',F9.3/
     &22x,'                  ...  and low flows by -',F9.3/22x,55('+'))
      if ( FM .lt. 0.20 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress3 .lt. 1 ) then
      call change colour of text (10) ! green
      write( *,3502)uname(JU)
 3502 format('*** Huge change in flow demanded',19x,'...',7x,
     &'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( suppress3 .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,3582)uname(JU)
 3582 format('*** Huge change in flow demanded',19x,'...',6x,
     &' at ',a40,1x,'and at other gauges .....')
      call set screen text colour
      endif
      endif
      suppress3 = suppress3 + 1
      write(01,3522)uname(JU)
      write(09,3522)uname(JU)
      write(33,3522)uname(JU)
      write(03,3522)uname(JU)
 3522 format(110('*')/
     &'*** A large (80%) reduction in ',
     &'river flow has been requested by gap filling ...'/
     &'*** The results may be wrong or misleading for ... ',
     &a40/110('*')) 
      endif
      if ( FM .gt. 5.0 ) then
      ifm = FM
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress3 .lt. 1 ) then
      call change colour of text (10) ! green
      write( *,3502)uname(JU)
      call set screen text colour
      endif
      if ( suppress3 .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,3582)uname(JU)
      call set screen text colour
      endif
      endif
      suppress3 = suppress3 + 1
      write(01,3552)ifm,uname(JU)
      write(09,3552)ifm,uname(JU)
      write(33,3552)ifm,uname(JU)
      write(03,3552)ifm,uname(JU)
 3552 format(120('*')/
     &'*** A huge change is needed to match ',
     &'the gauged river flows ...'/
     &'*** The change is ',i11,'-fold'/
     &'*** Extrapolation suppressed downstream of ... ',a40/120('*')) 
      NEXF=1
      endif


      if ( NEXF .eq. 0 ) then
      if ( if suppress flow .eq. 1 ) then
      if ( nobigout .le. 0 ) write(01,3381)UNAME(JU)
      do jper = 1,ndet
      if (qtype(jper) .ne. 4 ) write(170+jper,3381)UNAME(JU)
 3381 format(77('-')/
     &'*** The distance to a flow gap filling point is too ...'/
     &'*** short for safe extrapolation downstream ...'/
     &'*** Extrapolation suppressed downstream of ...'/
     &'*** ',A40/77('-'))
      enddo ! do jper = 1,ndet
      NEXF = 1
      endif ! if ( if suppress flow .eq. 1 )
      endif ! if ( NEXF .eq. 0 )

      call calculate summaries of river flow

*     compute the set of adjustments needed to obtain a fit --------------------
*     except where the distance is zero ----------------------------------------
*     (In units of flow per kilometre ... ) ------------------------------------

      FACT = dcalflow ! set the distance over which the corrections apply ------
      if ( dcalflow .lt. 1.0e-10 ) FACT=1.0
      do IS = 1, NS
      if ( FMS(IS) .gt. 1.0e-10 ) then
      FMX(IS) = (FMS(IS) - FMX(IS ))/FACT ! change per kilometre ---------------
      if ( FMX(IS) .gt. 0.0 ) then
      FMX(IS)=amin1(9999.9,FMX(IS))
      FMX(IS)=amax1(0.0001,FMX(IS))
      endif
      if ( FMX(IS) .le. 0.0 ) then
      FMX(IS)=amax1(-9999.9,FMX(IS))
      FMX(IS)=amin1(-0.0001,FMX(IS))
      endif
      endif
      enddo ! do IS = 1, NS
      dcalflow = 0.0

      write(74,8560)NEXF
 8560 format('FLOW',116(1H*),I2)
      write(74,8448)(FMX(IS),IS=1,NS)
 8448 format(10(1PE18.10))
      return

 8195 continue

      call calculate summaries of river flow

      if (KFCAL(JU) .gt. 0 .and. NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU) ! end of flow filling routine
      endif

      call calculate summaries of river flow

      return
      end







      subroutine flowfit (JU,IFCX)

*     calculates adjustments for flow gap filling ------------------------------
*     --------------------------------------------------------------------------
*     FLOWFIT generates values from the specified flow distribution.
*     These are stored in FMS. Checks are then carried out to ensure
*     that the  generated values are greater than zero.
*     The flows are then ranked in ascending order of magnitude.
*     The ranking order of the simulated values is then calculated.
*     This allows the generated flows to be paired with the simulated
*     flow of the same rank.
*     --------------------------------------------------------------------------
      include 'COMMON DATA.FOR'
      dimension IFCX(MS),WORK1(MS),WORK2(MS)

*     generate the target values of river flow ---------------------------------
      call generate river flow ! target river flow for gap filling
*     check generated flow values are greater than zero ------------------------
*     set any negative values to zero ------------------------------------------
      IZ=0
      do IS=1,NS
      if ( FMS(IS) .lt. 1.0E-10 ) then
      FMS(IS)=1.0E-10
      IZ=IZ+1
      endIF
      enddo

*     output error message if too many values are reset to zero ----------------
      IZ = (IZ/NS)*100
      if ( IZ .gt. 5 ) THEN
      if ( nobigout .le. 0 ) write(01,998) IZ,UNAME(JU)
      write( *,998) IZ,UNAME(JU)
      write(33,998) IZ,UNAME(JU)
  998 format(120('-')/
     &'Warning - ',I3,'% of the generated flows are less than zero ',
     &'just upstream of: ',A40/
     &'Check the specified statistical distribution of river flow ...'/
     &120('-'))
      endif

*     put simulated and monitored values in dummy array -------------------------

      do IS = 1, NS
      WORK1(IS) = FMX(IS)
      WORK2(IS) = FMS(IS)
      enddo

*     sort and rank monitored values --------------------------------------------
      call rank (WORK2,IFCX,NS)
*     sort and rank simulated values --------------------------------------------
      call rank (WORK1,IFCX,NS)

*     pair up the generated flow with simulated flow of same rank ---------------
*     This means overwriting the current values in FMS --------------------------

*     WORK2 - ranked generated values
*     IFCX  - position of ranked simulated value in FMX array

      do IS = 1, NS
      IX = IFCX(IS)
      FMS(IX) = WORK2(IS)
      enddo

      return
      end






*     Sorts array VAL of NVAL values into ascending order of magnitude
*     The previous location of ranked values are stored in IVAL
*     VAL   - Values to be ranked
*     IVAL  - position of ranked values in pre-ranked VAL
*     NVAL  - number of values
      subroutine rank (VAL,IVAL,NVAL)
      include 'COMMON DATA.FOR'
      dimension VAL(NS),IVAL(NS)

      do I = 1, NVAL
      IVAL(I) = I
      enddo

      do 30 I = 1, NVAL-1
      XSTORE = VAL(I)
      ISTORE = IVAL(I)
      IX = I

      do J=I+1,NVAL
      if (VAL(J) .lt. XSTORE) then
      IX = J
      XSTORE = VAL(J)
      ISTORE = IVAL(J)
      endif
      enddo

      VAL(IX) = VAL(I)
      IVAL(IX) = IVAL(I)
      VAL(I) = XSTORE
      IVAL(I) = ISTORE
   30 continue

      return
      end





*     gap filling for river quality -------------------------------------------3
      subroutine perform gap filling for quality (JU) ! -----------------------3
      include 'COMMON DATA.FOR'
      common /cal/ KOUNT(MP10,MS)
      integer ZQUAL(MP10),NSKIP(MP10),not fitted(MP10),numbits(MP10)
      double precision CMT(MP10,MS)
      dimension DMS(2,MP10,MS),CLS(MP10,MS)
      double precision DKY(2,MP10,MS),DEKAY(MP10,MS)
      double precision DEKB,DEKC,DEKB1,DEKB2
      dimension ishotzero(MP10),ishotdone(MP10)

      ngapdets = 0
      do jdet = 1, ndet
      NSKIP(jdet) = 0 ! initialise the completion of gap filling
      QNAT(jdet) = 0 ! initialise required % change in quality
      if ( QTYPE (jdet) .eq. 4) ngapdets = ngapdets + 1
      enddo
      
      kcheck = 0
      maxNITZ = 20 ! maximum number of iterations in Gap Filling ---------------

      if  (IEND .eq. 1 ) then ! check for the end of the reach 33333333333333333
      call extrapolate quality gap filling (JU,DISTP) ! to end of the reach 3333
      return
      endif ! end of reach

*     send details of feature to the quality gap filling (.QCL) file -----------
      write(75,8558)
 8558 format(123(1H*))
      write(75,8559)JREACH(JU),DISTP,UNAME(JU),RNAME(JREACH(JU))
 8559 format(I4,F12.6,30X,A44,1X,A16)

      IQ = IABS(JQCAL(JU)) ! quality data-set to be fitted ---------------------
      IQMON = IQ

      if ( IQ .eq. 0 ) goto 1995 ! check for a gap filling point ---------------
*     check for extrapolation downstream of the gap filling point --------------
      if ( JQCAL(JU) .le. 0 ) then ! extrapolation will be suppressed
      do iidet = 1, ndet
      NEXC (iidet) = 1 ! suppress extrapolation of gap filling
      enddo
      endif ! if ( JQCAL(JU) .le. 0 )
      if ( JQCAL(JU) .gt. 0 ) then ! extrapolation will happen
      do iidet = 1, ndet
      NEXC (iidet) = 0 ! allow downstream extrapolation ------------------------
      enddo
      endif ! if ( JQCAL(JU) .gt. 0 )

*     initialise the arrays ----------------------------------------------------
      do IS = 1,NS
      do jdet = 1, ndet
      DEKAY(jdet,IS) = 0.0 ! gap filling constants
      CLS(jdet,IS) = 1.0e-20 ! simulated values for the previous iteration -----
      CMX(jdet,IS) = 1.0e-20 ! the loads to be stored in the QCL file ----------
      do iip = 1,2
      DKY(iip,jdet,IS) = 0.0 ! stored gap filling constants --------------------
      DMS(iip,jdet,IS) = 0.0 ! the results from this ---------------------------
      enddo ! do iip = 1,2
      enddo ! do jdet = 1, ndet
      enddo ! do IS = 1,NS

      call get summaries of river quality from the shots ! gap filling ---------

      
*     ==========================================================================
      if ( FLOW(1) .lt. 1.0e-08 ) then ! =======================================
      if ( nobigout .le. 0 ) write(01,4481)
      if ( iscreen .lt. 3 ) write( *,4481)
 4481 format(77('-')/
     &'*** Zero flow at quality gap filling point ... ',
     &'gap filling suppressed'/77('-'))
      write(33,4481)
      write(33,4581)UNAME(JU)
      do jper = 1,ndet ! -------------------------------------------------------
      if (qtype(jper) .ne. 4 ) then ! ------------------------------------------
      write(170+jper,4481)
      write(170+jper,4581)UNAME(JU)
 4581 format(77('-')/'*** Name of Feature ... ',A30/77('-'))
      endif ! if (qtype(jper) .ne. 4 ) -----------------------------------------
      enddo ! do jper = 1,ndet -------------------------------------------------
      return
      endif ! if ( FLOW(1) .lt. 1.0e-08 ) ======================================
*     ==========================================================================

      
      do 4000 jdet = 1, ndet ! take each determinand in turn -------------------
      ishotdone(jdet) = NS ! initialise number of shots still to be set --------
      if ( QTYPE (jdet) .eq. 4) goto 4000
      
*     ==========================================================================
*     EXCLUDE CERTAIN TYPES OF DATA ============================================
      if ( PDRC (IQMON,jdet) .eq. 6 .or. ! exclude certain distributions =======
     &PDRC (IQMON,jdet) .eq. 7 .or. PDRC (IQMON,jdet) .eq. 11 .or.
     &PDRC (IQMON,jdet) .eq. 9 ) then ! loads
      call change colour of text (10) ! green
      write( *,8693)dname(jdet),uname(JU)
 8693 format('*** Gap filling suppressed for ',
     &a11,9x,'...',7x,'at ',a40,' data expressed as loads')
      call set screen text colour
      write(01,8533)dname(jdet),uname(JU)
      write(33,8533)dname(jdet),uname(JU)
      write(09,8533)dname(jdet),uname(JU)
      write(170+jdet,8533)dname(jdet),uname(JU)
 8533 format(93('=')/
     &'#### A request for gap filling has been suppressed for ',
     &'... ',a11,' at ',a40/ 
     &'#### The quality data are specified as loads ...'/93('='))
      ishotdone(jdet) = -777 ! suppress gap filling - data are loads -----------
      do is = 1, NS
      KOUNT(jdet,IS) = -777 ! suppress gap filling - data are loads ------------
      enddo
      NSKIP(jdet) = 1 ! gap filling suppressed - data are loads ----------------
      NEXC (jdet) = 1 ! suppress extrapolation - data are loads ----------------
      ngapdets = ngapdets + 1
      if ( ngapdets .eq. ndet ) then ! -----------------------------------------
      suppress21 = suppress21 + 1
      if ( suppress21 .lt. 7 ) then
      call change colour of text (10) ! green
      write( * ,9788)UNAME(JU)
      call set screen text colour
      endif
      if ( suppress21 .eq. 7 ) then
      call change colour of text (10) ! green
      write( * ,9768)
      call set screen text colour
      endif
      return
      endif ! if ( ngapdets .eq. ndet ) ----------------------------------------
      goto 4000
      endif ! if ( PDRC (IQMON,jdet) .eq. 6 .or. ===============================
      if ( PDRC (IQMON,jdet) .eq. 4 .or. 
     &PDRC (IQMON,jdet) .eq. 5 .or.
     &PDRC (IQMON,jdet) .eq. 8 ) then 
      write(01,8543)dname(jdet),uname(JU)
      write(33,8543)dname(jdet),uname(JU)
      write(09,8543)dname(jdet),uname(JU)
      write(170+jdet,8543)dname(jdet),uname(JU) ! GGGGGGGGGGGGGGGGGG gap filling
 8543 format(93('=')/ ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
     &'#### A request for gap filling has been suppressed for ',
     &'... ',a11,' at ',a40/ 
     &'#### The quality data are specified as non-parametric ',
     &'distributions etc ...'/93('='))
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress (1,jdet) .lt. 1 ) then
      call change colour of text (10) ! green
      write( *,8593)dname(jdet),uname(JU)
 8593 format('*** Gap filling suppressed for ',
     &a11,9x,'...',7x,'at ',a40,' non-parametric etc ',6('.'))
      call set screen text colour
      endif ! if ( suppress (1,jdet) .lt. 1 )
      endif ! if ( ifbatch .eq. 1 )
      suppress (1,jdet) = suppress (1,jdet) + 1
      ishotdone(jdet) = -777 ! suppress gap filling - non-parametric data ------
      do is = 1, NS
      KOUNT(jdet,IS) = -777 ! suppress gap filling - non-parametric data -------
      enddo
      NSKIP(jdet) = 1 ! gap filling suppressed - non-parametric data -----------
      NEXC (jdet) = 1 ! suppress extrapolation - non-parametric data -----------
      ngapdets = ngapdets + 1
      if ( ngapdets .eq. ndet ) then
      suppress21 = suppress21 + 1
      if ( suppress21 .lt. 7 ) then
      call change colour of text (10) ! green
      write( * ,9788)UNAME(JU)
      call set screen text colour
      endif
      if ( suppress21 .eq. 7 ) then
      call change colour of text (10) ! green
      write( * ,9768)
      call set screen text colour
      endif
      return
      endif ! if ( ngapdets .eq. ndet ) ----------------------------------------
      goto 4000
      endif ! if ( PDRC (IQMON,jdet) .eq. 4 ) ==================================
*     EXCLUDE CERTAIN TYPES OF DATA ============================================
*     ==========================================================================

     
     
*     cater for weirs.  Use only Dissolved Oxygen (Detype = 104) for Weirs -----
      if (JT(JU) .eq. 8 .and. detype (jdet) .ne. 104) goto 4000
*     also skip if the quality is negative -------------------------------------
      if (quolity data(IQ,jdet,1) .lt. -1.0E-08) goto 4000

      do IS = 1, NS ! store current SIMCAT shots in the array CLS -------- Weirs
      CLS(jdet,IS) = CMS(jdet,IS)
      enddo ! store current SIMCAT shots in the array CLS ----------------------

*     calculate the percentile of measured quality -----------------------------
*     this is what SIMCAT aims to fit to --------------------------------- Weirs
      call calculate percentiles from mean and standard deviation 
     &(jdet,DCM,DCS,DCP)


*     ==========================================================================
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( detype (jdet) .ne. 104 ) then ! not dissolved oxygen ================
      if ( detype (jdet) .ne. 909 ) then ! not dissolved metal =================
      write(170+jdet,8099)DNAME(jdet),uname(JU),DCM, ! ------------------ Ci.GAP
     &UNITS(jdet),DCP,UNITS(jdet) ! ------------------------------------- Ci.GAP
 8099 format(/////33x,77('#')/33x, ! ------------------------------------ Ci.GAP
     &'The next round of gap-filling for water quality starts ...'/ ! --- Ci.GAP
     &33x,77('#')/33x, ! ------------------------------------------------ Ci.GAP
     &'Gap filling for ... ',a11,' towards ',a35/33x,77('-')/33x, ! ----- Ci.GAP
     &24x,'Required mean at that point =',F9.2,1X,a4/33x, ! ------------- Ci.GAP
     &29X,'Required 95-percentile =',F9.2,1X,a4/33x,77('#')/// ! -------- Ci.GAP
     &110('-')) ! ------------------------------------------------------- Ci.GAP
      else ! then for dissolved metal ==========================================
      write(33,8499)DNAME(jdet),uname(JU),IQ,DCM, ! dissolved metal -------- ERR
     &UNITS(jdet),DCP,UNITS(jdet) ! dissolved metal
 8499 format(///77('#')/
     &'Gap filling for ... ',A11,' towards ',a35/77('-')/ ! diss. metal ---- ERR
     &'QUALITY DATA SET:',I6,3X,'REQUIRED MEAN (dissolved) =',F9.2,1X,
     &a4/33x,17X,'REQUIRED 95-PERCENTILE (dissolved) =',F9.2,1X,A4)
      write(170+jdet,8199)DNAME(jdet),uname(JU),IQ,DCM, ! dissolved metal -- GAP
     &UNITS(jdet),DCP,UNITS(jdet) ! dissolved metal --------------------- Ci.GAP
 8199 format(/////33x,77('#')/
     &'Gap filling for ... ',A11,' towards ',a35/77('-')/33x, ! diss. metal  GAP
     &'QUALITY DATA SET:',I6,3X,'REQUIRED MEAN (dissolved) =',F9.2,1X,
     &A4/33x,
*    &17X,'    STANDARD DEVIATION (dissolved) =',F9.2,1X,A4/
     &17X,'REQUIRED 95-PERCENTILE (dissolved) =',F9.2,1X,A4/
     733x,77('#')///)
      endif ! if ( detype (jdet) .ne. 909 )  not dissolved metal ===============
      endif ! if ( detype (jdet) .ne. 104 )  not dissolved oxygen ==============
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++
*     ==========================================================================


*     ==========================================================================
      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( QTYPE (jdet) .eq. 3 .or. QTYPE (jdet) .eq. 5 ) then ! DO ----- Ci.GAP
      write(170+jdet,6099)DNAME(jdet),uname(JU),IQ,DCM, ! diss.oxygen --- Ci.GAP
*    &UNITS(jdet),DCS,UNITS(jdet),DCP,UNITS(jdet) ! dissolved oxygen ---- Ci.GAP
     &UNITS(jdet),DCP,UNITS(jdet) ! ---------- dissolved oxygen --------- Ci.GAP
 6099 format(/////33X,77('#')/33x, ! --------- dissolved oxygen --------- Ci.GAP
     &'The next round of gap-filling for dissolved oxygen starts ...'/ !  Ci.GAP
     &33x,77('#')/33x, ! --------------------- dissolved oxygen --------- Ci.GAP
     &'Gap filling for ... ',A11,' towards ',a35/33X,77('-')/33X, ! DO -- Ci.GAP
     &'QUALITY DATA SET:',I6,15X,'Required mean =',F9.2,1X,A4/33X,
*    &29X,'    Standard deviation =',F9.2,1X,A4/33X,
     &29X,' Required 5-percentile =',F9.2,1X,A4/33X,77('#')/// ! --- DO - Ci.GAP
     &110('-'))  ! --------------------------- dissolved oxygen --------- Ci.GAP
      endif ! if ( QTYPE (jdet) .eq. 3 .or. QTYPE (jdet) .eq. 5 ) ------- Ci.GAP
      endif ! if ( nobigout .le. 0 ) +++++++++++++++++++++++++++++++++++++++++++
*     ==========================================================================

     
*     compute the effect on quality of gap filling -----------------------------
*     DCM is the required quality ----------------------------------------------
*     C(jdet,1) and C(jdet,3) are the quality before gap filling ---------------
*     QNAT is the required % change in quality ---------------------------------
      if ( C(jdet,1) .gt. 1.0e-10 ) CC1 = C(jdet,1) ! calculated mean quality --
      if ( C(jdet,3) .gt. 1.0e-10 ) CC2 = C(jdet,3) ! calculated 95-percentile -
      if ( C(jdet,1) .gt. 1.0e-10 ) then
      QNAT(jdet)=INT(0.5+100.0*(DCM/AMAX1(1.0E-10,C(jdet,1))-1.0))
      QNAT(jdet)=min0(99999,QNAT(jdet))
      endif ! if ( C(jdet,1) .gt. 1.0e-10 ) ------------------------------------

*     compute the array of target values. These are ranked on the values of ----
*     the current shots --------------------------------------------------------


*     ==========================================================================
*     first check for a zero target river quality ------------------------------
      if ( DCP .lt. 1.0e-10 ) then
      suppress (4,jdet) = suppress (4,jdet) + 1
      if ( nobigout .le. 0 ) write(01,6781)DNAME(jdet),UNAME(JU)
      write(33,6781)DNAME(jdet),UNAME(JU)
      write(09,6781)DNAME(jdet),UNAME(JU)
      write(170+jdet,6781)DNAME(jdet),UNAME(JU) ! GGGGGGGGGGGGGGGGGG gap filling
 6781 format(110('*')/
     &'*** The measured quality specified for gap filling is ',
     &'zero ...'/
     &'*** This could cause problems with gap filling ...'/
     &'*** Gap filling suppressed for ',a11,' at Feature: ',a40/
     &110('*'))
      if ( iscreen .lt. 3 ) then
      call change colour of text (19) ! light pink
      write( * ,6788)DNAME(jdet),UNAME(JU)
 6788 format('*** The target for gap filling is zero',13x,'...',7x,
     &'action suppressed for ',a11,11x,'at: ',a40)
      call set screen text colour
      endif
      do is = 1, NS
      KOUNT(jdet,IS) = -777 ! suppress gap filling - quality is zero -----------
      enddo
      NSKIP(jdet) = 1 ! suppress gap filling - quality is zero -----------------
      ishotdone(jdet) = -777 ! suppress gap filling - quality is zero ----------
      NEXC (jdet) = 1 ! suppress extrapolation - quality is zero ---------------
      ngapdets = ngapdets + 1
      if ( ngapdets .eq. ndet ) then ! -----------------------------------------
      suppress21 = suppress21 + 1
      if ( suppress21 .lt. 7 ) then
      call change colour of text (10) ! green
      write( * ,9788)UNAME(JU)
 9788 format('*** No usable data for gap filling',17x,'...',7x,
     &'at ',a37,4x,'action suppressed .......')
      call set screen text colour
      endif
      if ( suppress21 .eq. 7 ) then
      call change colour of text (10) ! green
      write( * ,9768)
 9768 format('*** No usable data for gap filling',17x,'...',7x,
     &'action suppressed',27x,'at lots of places .......')
      call set screen text colour
      endif
      return
      endif ! if ( ngapdets .eq. ndet ) ----------------------------------------
      goto 4000
      endif ! if ( DCP .lt. 1.0e-10 )
*     ==========================================================================

      
*     ==========================================================================
*     check for zero quality for SIMCAT's calculations -------------------------
      ZQUAL (jdet) = 0
      if ( C(jdet,1) .lt. 1.0e-10 ) then
      C (jdet,1) = 0.99*DCM
      C (jdet,2) = 0.99*DCS
      C (jdet,3) = 0.99*DCP
      ZQUAL (jdet) = 1
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress (5,jdet) .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,5582)DNAME(jdet),UNAME(JU)
 5582 format('*** Calculated upstream quality is zero for ',
     &a11,6x,'at ',a40,1x,'and elsewhere ...........')
      call set screen text colour
      endif
      endif
      suppress (5,jdet) = suppress (5,jdet) + 1
      if ( nobigout .le. 0 ) write(01,6741)DNAME(jdet),UNAME(JU)
      write(170+jdet,6741)DNAME(jdet),UNAME(JU)
      write(33,6741)DNAME(jdet),UNAME(JU)
      write(09,6741)DNAME(jdet),UNAME(JU)
      write(170+jdet,6741)DNAME(jdet),UNAME(JU) ! GGGGGGGGGGGGGGGGGG gap filling
      call change colour of text (12) ! orange
      if ( iscreen .lt. 3 ) write( * ,6741)DNAME(jdet),UNAME(JU)
 6741 format(110('-')/
     &'*** The calculated upstream river quality is zero for ',a11,
     &' ... '/'*** Extrapolation suppressed downstream of ',
     &A40/110('-'))
      call set screen text colour    
      NEXC (jdet) = 1 ! upstream river quality is zero
      endif ! if ( C(jdet,1) .lt. 1.0e-10 )
*     ==========================================================================

      
      
*     checks finished.  Now proceed with the calculations ----------------------
*     calculate the QUALITY values for gap filling -----------------------------
*     generate values from the monitored quality distribution ------------------
*     sort the generated values into an ascending order of magnitude -----------
*     calculate the ranked order of the current simulated values ---------------
*     pair generated values with simulated values of the same rank -------------
      call quality fit (jdet,DCM,DCS,DCP,CLS,CMT)

      
      if (DCM .gt. 1.0e-8) then ! ==============================================
      CC1 = (C(jdet,1) - CC1)/CC1 
      CC2 = (C(jdet,3) - CC2)/CC2 
      CC1 = 100.0*CC1
      CC2 = 100.0*CC2
      !if ( CC1 .lt. 99999.0 ) then 
      !if ( nobigout .le. 0 ) write(09,6)int(CC1),DNAME(jdet),int(CC2)
      !6 format(77('-')/
      !&'A fit was attempted by changing data by ',i6,' % for ',a11/
      !&'              ... and the percentile by ',i6,' %'/
      !&77('#'))
      !endif
      endif ! if (DCM .gt. 1.0e-8) then ========================================

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( int(CC1) .lt. -80.0 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress (2,jdet) .lt. 1 ) then
      call change colour of text (10) ! green
      write( *,3582)int(CC1),Dname(jdet),UNAME(JU)
 3582 format('*** Reduction of',i6,' % needed to fit ',a11,7x,'...',
     &7x,'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( suppress (2,jdet) .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,4582)int(CC1),Dname(jdet),UNAME(JU)
 4582 format('*** Reduction of',i6,' % needed to fit ',a11,7x,'...',
     &7x,'at ',A40,' and other places ',8('.'))
      call set screen text colour
      endif
      endif
      suppress (2,jdet) = suppress (2,jdet) + 1
      write(01,3522)int(CC1),Dname(jdet),UNAME(JU)
      write(09,3522)int(CC1),Dname(jdet),UNAME(JU)
      write(33,3522)int(CC1),Dname(jdet),UNAME(JU)
      write(170+jdet,3522)int(CC1),Dname(jdet),UNAME(JU)
 3522 format(110('*')/
     &'*** A large (',i5,' % ) reduction of ',
     &'river quality has been requested by gap filling ...'/
     &'*** The results may be wrong or misleading for ... ',a11,
     &' at ',a40/110('*'))  
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( CC1 .gt. 500.0 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress (3,jdet) .lt. 1 ) then
      call change colour of text (10) ! green
      write( *,3682)Dname(jdet),UNAME(JU)
 3682 format('*** Gross increase needed to fit ',
     &a11,7x,'...',7x,'at ',A40,1x,25('.'))
      call set screen text colour
      write(01,3682)Dname(jdet),UNAME(JU)
      write(33,3682)Dname(jdet),UNAME(JU)
      write(170+jdet,3682)Dname(jdet),UNAME(JU)
      endif
      if ( suppress (3,jdet) .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,5682)Dname(jdet),UNAME(JU)
 5682 format('*** Gross increase needed to fit ',
     &a11,7x,'...',7x,'at ',A40,' and elsewhere ...........')
      call set screen text colour
      write(01,5682)Dname(jdet),UNAME(JU)
      write(33,5682)Dname(jdet),UNAME(JU)
      write(170+jdet,5682)Dname(jdet),UNAME(JU)
      endif
      endif
      suppress (3,jdet) = suppress (3,jdet) + 1
      write(01,3622)Dname(jdet),UNAME(JU)
      write(09,3622)Dname(jdet),UNAME(JU)
      write(33,3622)Dname(jdet),UNAME(JU)
      write(170+jdet,3622)Dname(jdet),UNAME(JU)
 3622 format(/110('-')/
     &'*** A huge (>5-fold) change in river quality is needed to ',
     &'match the measured water quality for ',a11/
     &'*** Extrapolation suppressed downstream of ',A40/110('-'))  
      NEXC (jdet) = 1 ! huge change in quality needed --------------------------
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     suppress extrapolation if fit not applied to more than 1 kilometre -------
      if ( dcalquality .le. 1.0 ) then
      if ( jdet .eq. ndetfirst ) then
      if ( nobigout .eq.  0 ) write(01,4381)UNAME(JU)
      write(33,4381)UNAME(JU)
 4381 format(110('-')/
     &'*** The distance to a quality gap filling point is ',
     &'too short for safe extrapolation downstream ... '/
     &'*** Extrapolation suppressed downstream of ',A40/110('-'))
      endif
*     flag to suppress the downstream extrapolation of corrections -------------
      do iidet = 1, ndet
      NEXC (iidet) = 1 ! distance is too short ---------------------------------
      enddo
      endif ! if ( dcalquality .le. 1.0 ) --------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      
*     ==========================================================================
*     compute the gap filling corrections per kilometre ========================
      FACT = dcalquality ! set the distance over which the corrections apply --- 
      if (dcalquality .lt. 1.0e-06) FACT = 1.0

      do 4452 IS = 1,NS ! loop on all the shots for the determinand, jdet ------
      KOUNT(jdet,IS) = 0 ! initialise the iteration counter for gap filling ----

*     change in concentration needed to secure at fit --------------------------
*     negative value indicates a loss of load ----------------------------------
      CONC = CMT(jdet,IS) - CLS(jdet,IS)

*     test for a neglible change -----------------------------------------------
      if (CONC .lt. 1.0E-12 .and. CONC .gt. -1.0E-12) then
      CMX(jdet,IS) = 0.0
      goto 4452
      endif ! --------------------------------------- test for a neglible change

*     proceed for conservative and degradable pollutants -----------------------
*     and jump to the end of the loop for Qtype equal to 4 or 6 ----------------
      goto (4461,4462,4461,4452,4461,4452),QTYPE(jdet)
*     type 1 and 3 determinands ------------------------------------------------
*     for a gain in load ... the value of CMX will be positive -----------------
 4461 CMX(jdet,IS) = FMS(IS) * CONC / FACT
      DEKAY(jdet,IS) = CMX(jdet,IS)
      goto 4452

*     type 2 determinands ------------------------------------------------------
*     check for gain in load (per kilometre) -----------------------------------
*     treat these gains like type 1 and 3 --------------------------------------
 4462 if (CONC .gt. -1.0e-13) goto 4461
*     arrive here for losses in load for type 2 pollutants ---------------------
*     losses are modelled as exponential decay as a function of distance -------
*     set new and old quality --------------------------------------------------
      CNEW = CMT(jdet,IS)
      COLD = CLS(jdet,IS)
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     check first not to ask too much of gap filling ---------------------------
      if ( COLD .le. 1.0e-9 .and. kcheck .lt. 4 ) then
      kcheck = kcheck + 1
*     call warning
      write( *,2176)DNAME(jdet),COLD,IS
      write(01,2176)DNAME(jdet),COLD,IS
      write(09,2176)DNAME(jdet),COLD,IS
      write(170+jdet,2176)DNAME(jdet),COLD,IS
 2176 format(/120('*')/
     &'*** Failure in gap filling ... Determinand: ',A11,' ...'/
     &'*** Concentration = ',1PE11.4,' ... Shot number',I6,' ...'/
     &'*** SIMCAT stopped ...',
     &' Gap filling is being asked to account for too ',
     &'big a difference ...'/
     &'*** Check pollution loads and/or Rate Constants ...'/120('*'))
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     compute the adjustment and store it in CMX ----------- lost concentrations
*     the intermediate value will be stored in DEKAY ---------------------------
*     decay will be indicated by negative values of CMX ------------------------
      CMX(jdet,IS) = -1.0E-20 ! first set all the initial values to zero -------
      DEKAY(jdet,IS) = -1.0E-20
      CRAT = CNEW / COLD ! ratio of new and old concentration
      if (CRAT .lt. 1.0E-10) CRAT = 1.0E-10
*     compute exponential rate of decay ========================================
      DEKAY(jdet,IS) = alog(CRAT) / FACT ! decay per kilometre -----------------
      if ( DEKAY(jdet,IS) .gt. -1.0E-20 ) DEKAY(jdet,IS) = -1.0E-20
      CMX(jdet,IS) = DEKAY(jdet,IS)
*     end of processing for losses in load for type 2 pollutants ===============
*     ==========================================================================
 4452 continue ! end of the loop on shots --------------------------------------
 4000 continue ! a main loop on determinands ends here -------------------------


*     The next section calculates the correction factor for each determinand ---
*     The contents of the main arrays are given below
*        CMT - contains the target values
*        CMX - contains the adjustments
*        CMS - contains simulated adjusted values
*        CLS - contains simulated values for the previous iteration

*     initialise variables used in the iterative scheme ------------------------
      KSIM = 0 ! initialise counter for the number of iterations for quality ---
      do jdet = 1,ndet
      numbits(idet) = 0 ! number of iterations for each determinand -------------    
      enddo
      ctarx = 0.9999 ! initialise the target precision for each shot -----------
      PTEST = 999.9 ! initialise percent of total unfitted shots ---------------
      ptestx = 999.0 ! initialise the previous percent of unfitted shots -------
      do jdet = 1, ndet
      ishotzero(jdet) = 0 ! initialise the number of zero results --------------
      enddo
      JBEST = 1
      JWORST = 2

*     start the iterations ... or start the next iteration +++++++++++++++++++++
 6009 continue ! ++++++++++++++++++++++++++++++++++++++ start the next iteration
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      never fitted = 0 ! initialise the total number for shots not fitted ++++++
      KTEST = 0 ! initialise the total number of shots across all determinands +
      do 7001 jdet = 1, ndet ! loop on the determinands ========================
      if ( QTYPE (jdet) .ne. 4 ) then ! ========================================
      not fitted (jdet) = NS
      if ( NSKIP(jdet) .ne. 1 ) then ! gap filling is not suppressed -----------
      never fitted = never fitted + NS ! initialise number of shots not fitted -
      endif
      KTEST = KTEST + NS ! sum the number of shots across all determinands -----
      if ( ishotdone(jdet) .eq. -777 ) then ! suppress gap filling -------------
      ishotdone(jdet) = 0 ! mark all shots as completed (loads etc) ------------
      do is = 1, NS ! suppress gap filling for all the shots -------------------
      KOUNT(jdet,IS) = -777 ! suppress gap filling because it's finished -------
      enddo ! suppress gap filling for all the shots ---------------------------
      else ! gap filling is not suppressed -------------------------------------
      ishotdone(jdet) = NS ! mark shots uncompleted at start of iteration ------
      do is = 1, NS ! check all the shots for complete convergence -------------
      CTEST = CMT(jdet,IS) ! the target for the shot ---------------------------
      if ( CTEST .lt. 1.0e-08 ) CTEST = 1.0e-08
      CTEST = CMS(jdet,IS)/CTEST ! ratio of estimate to target -----------------
      if (CTEST .gt. ctarx .and. CTEST .lt. 2.0 - ctarx ) then
      KOUNT(jdet,IS) = -777 ! suppress gap filling - convergence is achieved ---
      never fitted = never fitted - 1 ! reduce number for shots not fitted -----
      not fitted(jdet) = not fitted(jdet) - 1
      ishotdone(jdet) = ishotdone(jdet) - 1	
      endif
      enddo ! loop on shots ----------------------------------------------------
      endif ! ( ishotdone(jdet) .eq. -777 ) ------------------------------------ 
      endif ! if ( QTYPE (jdet) .ne. 4 ) =======================================
 7001 continue ! loop on determinands ==========================================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      

      kopped out = 0 ! initialise sum of zero results for all determinands -----
      PTEST = 100.0 * never fitted / FLOAT(KTEST) !  percent of shots not fitted 
     
*     simulate the section of river using the calculated adjustments -----------
*     the results will be used to alter the adjustments ------------------------
      KFEAT = JU
      
      KSIM = KSIM + 1 ! add one to the number of simulations for quality -------
      call gap fill simulation (JU) ! from "perform gap filling for quality" 333
      call get summaries of river quality from the shots ! gap filling ---------
      
      !if ( KSIM .gt. 9 ) ctarx = 0.999 ! slacken the target
      !if ( KSIM .gt. 18 ) ctarx = 0.98 ! slacken the target again

*     the quality calculated in [GAP FILL SIMULATION] is stored in CMS ---------
*     now compare this with the required values in CMT -------------------------
*     and adjust the factors in DEKAY, if necessary ----------------------------

      do 8001 jdet = 1, ndet ! ================================================= 
      if ( QTYPE(jdet) .eq. 4 .or. NSKIP(jdet) .eq. 1 ) goto 8001
*     ==========================================================================
*     for a weir do dissolved oxygen only --------------------------------------
      if (JT(JU) .eq. 8 .and. detype (jdet) .ne. 104) goto 8001 !  for weir & DO
      if ( ishotdone(jdet) .ne. 0 ) numbits(jdet) = KSIM ! this is not DO ------
*     if (quolity data(IQ,jdet,1) .lt. -1.0E-08)  goto 8001
*     ==========================================================================
      do 8002 IS = 1, NS ! loop on this determinand's shots --------------------
      if (KOUNT(jdet,IS) .eq. -777) goto 8002 ! proceed to the next shot -------
      CTEST = CMT(jdet,IS) ! the target for the shot ---------------------------
      if ( CTEST .lt. 1.0e-08 ) CTEST = 1.0e-08
      CTEST = CMS(jdet,IS)/CTEST ! ratio of estimate to target -----------------
      if (CTEST .gt. ctarx .and. CTEST .lt. 2.0 - ctarx ) then
      KOUNT(jdet,IS) = -777 ! suppress gap filling - convergence is achieved ---
      not fitted(jdet) = not fitted(jdet) - 1
      never fitted = never fitted - 1 ! reduce number for shots not fitted -----
      ishotdone(jdet) = ishotdone(jdet) - 1 ! record the shot as completed -----
      endif
 8002 continue ! do 8002 IS = 1, NS --------------------------------------------
 8001 continue ! do 8001 jdet = 1, ndet ========================================
      
      PTEST = 100.0 * never fitted / FLOAT(KTEST) !  percent of shots not fitted 
      call details upstream of feature ! gap filling for water quality
      

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      KTEST = 0 ! initialise the total number of shots across all determinands -
      do 6001 jdet = 1, ndet 
      if ( QTYPE (jdet) .eq. 4 ) goto 6001
      KTEST = KTEST + NS ! sum the number of shots across all determinands -----
      if ( NSKIP (jdet) .eq. 1 ) goto 6001 ! gap filling is suppressed ---------
      if ( ishotdone(jdet) .eq. -777 ) goto 6001 ! not for this determinand ----
      if ( ishotdone(jdet) .eq. 0 ) goto 6001 ! completed for this determinand -

*     ==========================================================================
*     Weirs. Do Dissolved oxygen only ------------------------------------------
      if (JT(JU) .eq. 8 .and. detype (jdet) .ne. 104) goto 6001
*     if (quolity data(IQ,jdet,1) .lt. -1.0E-08)  goto 6001
*     ==========================================================================

      negdiv = 0
      do 6002 IS = 1, NS ! loop on this determinand's shots --------------------
      if (KOUNT(jdet,IS) .eq. -777) then ! skip a shots that is finished with --
*     ishotdone(jdet) = ishotdone(jdet) - 1 ! record the shot as completed -----
      goto 6002 ! proceed to the next shot -------------------------------------
      endif ! end of the check for a completed shot ----------------------------

      if ( ndetBOD .gt. 0 ) then ! do dissolved oxygen when BOD is finished ----
      if ( DETYPE (jdet) .eq. 104 ) then ! dissolved oxygen --------------------
      if ( KOUNT(ndetBOD,IS) .ne. -777 ) goto 6002 ! proceed to the next shot --
      endif
      endif ! do dissolved oxygen when BOD is finished -------------------------

*     we have a shot that has not yet been fitted ------------------------------
      KOUNT(jdet,IS) = KOUNT(jdet,IS) + 1 ! update the iteration counter -------
      if ( KOUNT(jdet,IS) .eq. 1 ) then ! check for the first iteration --------
      DKY(1,jdet,IS) = DEKAY(jdet,IS) ! store the first gap filling constant ---
      DMS(1,jdet,IS) = CMS(jdet,IS)   ! store the first result from this -------
*     compute the gap filling constant for the second iteration ----------------
*     separate the linear and exponential adjustments --------------------------
      if ( QTYPE (jdet) .eq. 1 .or. QTYPE (jdet) .eq. 3 ) goto 9913
      if ( DEKAY(jdet,IS) .lt. 0.0) goto 8913
*     Linear adjustments. Check whether the last estimate was too big ----------
 9913 if (CMS(jdet,IS) .lt. CMT(jdet,IS)) goto 6022
*     The last estimate was too big. Change the constant -----------------------
      if ( DEKAY(jdet,IS) .gt. 0.0) DEKAY(jdet,IS)=0.9*DEKAY(jdet,IS)
      if ( DEKAY(jdet,IS) .le. 0.0) DEKAY(jdet,IS)=1.1*DEKAY(jdet,IS)
      goto 6104 ! store the new adjustment
*     the last estimate was too small ... increase the constant ----------------
 6022 if ( DEKAY(jdet,IS) .gt. 0.0) DEKAY(jdet,IS)=1.1*DEKAY(jdet,IS)
      if ( DEKAY(jdet,IS) .le. 0.0) DEKAY(jdet,IS)=0.9*DEKAY(jdet,IS)
      goto 6104 ! store the new adjustment
*     Exponential adjustments. Check whether the last estimate was too small ---
 8913 if (CMS(jdet,IS) .gt. CMT(jdet,IS)) goto 8922
*     the last estimate was too small. Decrease the exponential decay ----------
      DEKAY(jdet,IS) = 0.9 * DEKAY(jdet,IS)
      goto 6104 
*     the last estimate was too big. Increase the exponential decay ------------
 8922 DEKAY(jdet,IS) = 1.1 * DEKAY(jdet,IS)
 6104 continue
      DKY(2,jdet,IS) = DEKAY(jdet,IS) ! store the gap filling constant ---------
      DMS(2,jdet,IS) = CMS(jdet,IS)   ! store the result from this -------------
      goto 6004 ! store the adjustment for the second iteration ----------------
      endif ! first iteration --------------------------------------------------

      JBEST = 1 ! choose which of the last two estimates to retain -------------
      JWORST = 2
      CCCC1 = ABS (CMT(jdet,IS)-DMS(JBEST,jdet,IS))
      CCCC2 = ABS (CMT(jdet,IS)-DMS(JWORST,jdet,IS))
      if (CCCC2 .lt. CCCC1) then
      JBEST = 2 ! estimate number JBEST is best and will be retained -----------
      JWORST = 1
      endif ! best estimates chosen --------------------------------------------
      DMS(JWORST,jdet,IS) = CMS(jdet,IS) ! store last result -------------------
      DKY(JWORST,jdet,IS) = CMX(jdet,IS) ! store last gap filling constant -----



*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     first check for a zero divisor +++++++++++++++++++++++++++++++++++++++++++
      TEST = DMS(2,jdet,IS) - DMS(1,jdet,IS) ! difference between estimates ++++
      if (TEST .gt. -1.0E-10 .and. TEST .lt. 1.0E-10) then
      if ( KOUNT(jdet,IS) .ne. -777 ) then
      negdiv = negdiv + 1
      perone = 100.0 * ( 1.0 - (DMS(1,jdet,IS) / CMT(jdet,IS)))
      pertwo = 100.0 * ( 1.0 - (DMS(2,jdet,IS) / CMT(jdet,IS)))
      if ( negdiv .lt. 5 ) then
      write(33,1372)DNAME(jdet),KOUNT(jdet,IS),IS,CMT(jdet,IS),
     &DKY(1,jdet,IS),DMS(1,jdet,IS),perone,
     &DKY(2,jdet,IS),DMS(2,jdet,IS),pertwo
 1372 format(120('*')/
     &'Zero divisor in gap filling for ',A11,63x,
     &'Iteration:',I4/120('-')/
     &'Number of Shot',i13,    '    Target       ',1pe14.7/
     &'Constant one  ',1pe13.6,'    Result one   ',1pe14.7,
     &' (',1pe12.4,'% )'/
     &'Constant two  ',1pe13.6,'    Result two   ',1pe14.7,
     &' (',1pe12.4,'% )'/120('*'))
      endif
      DEK0 = DEKAY(jdet,IS) ! despite the zero attempt another guess +++++++++++
      DEK1 = DKY(1,jdet,IS)
      DEK2 = DKY(2,jdet,IS)
      DEK3 = DEK2 - DEK1
      DEKAY(jdet,IS) = DEKAY(jdet,IS) + 25.0 * DEK3
      KOUNT(jdet,IS) = KOUNT(jdet,IS) + 1 ! for zero divisor +++++++++++++++++++
      goto 6004 ! store the new adjustment +++++++++++++++++++++++++++++++++++++
      endif ! if ( KOUNT(jdet,IS) .ne. -777 ) ++++++++++++++++++++++++++++++++++
      endif ! if (TEST is zero ) +++++++++++++++++++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     Now test whether to give up the search for a fit for this shot +++++++++++
*     Have the two current estimates been reset below the minimum quality? +++++
      if ( QZEROGAP (jdet) .gt. 1.0e-12 ) then
      if ( DMS(1,jdet,IS) .le. 0.00000001 .and.
     &     DMS(2,jdet,IS) .le. 0.00000001 ) then
      !if ( DMS(1,jdet,IS) .le. QZEROGAP (jdet) .and.
      !&     DMS(2,jdet,IS) .le. QZEROGAP (jdet)) then
      if ( KOUNT(jdet,IS) .ne. -777 ) then
      if ( ishotzero(jdet) .eq. 0 ) then ! -------------------------------------
      if ( suppress (7,jdet) .lt. 4 ) then ! -----------------------------------
      xcmt = CMT(jdet,IS)
      call sort format 3 (TEST,QZEROGAP(jdet),xcmt)
      write(33,6372)DNAME(jdet),valchars10,valchars11,valchars12,IS
      write(01,6372)DNAME(jdet),valchars10,valchars11,valchars12,IS
      write(09,6372)DNAME(jdet),valchars10,valchars11,valchars12,IS
 6372 format(/110('-')/
     &'NEGATIVE or sub-minimum concentration(s) are needed in ',
     &'the river if you want a complete fit ...'/
     &'Determinand... ',A11,' needs a value of ',a10,
     &' (The minimum is ',a10,')'/
     &'to approach a value of',a10,' (Shot Number ',I5,')'/
     &110('-'))
      endif ! if ( suppress (7,jdet) .lt. 4 ) ----------------------------------
      ishotzero(jdet) = ishotzero(jdet) + 1 ! add to number of zero results ++++
      suppress (7,jdet)= suppress (7,jdet) + 1
      else ! if ( ishotzero (jdet) .eq. 0 ) ------------------------------------
      xcmt = CMT(jdet,IS)
      call sort format 3 (TEST,QZEROGAP(jdet),xcmt)
      write(33,6379)DNAME(jdet),valchars10,valchars11,valchars12,IS
      write(09,6379)DNAME(jdet),valchars10,valchars11,valchars12,IS
 6379 format('Determinand... ',A11,' needs a value of',A10,
     &' (The minimum is ',A10,')'/'to approach a value of',A10,
     &' (Shot Number ',I5,')'/110('-'))
      ishotzero (jdet) = ishotzero (jdet) + 1 ! add to number of zero results ++
*     KOUNT(jdet,IS) = -777 ! suppress gap filling +++++++++++++++++++++++++++++
      endif ! if ( ishotzero (jdet) .eq. 0 ) +++++++++++++++++++++++++++++++++++
      endif ! if ( KOUNT(jdet,IS) .ne. -777 ) ++++++++++++++++++++++++++++++++++
      endif ! if (DMS(1,jdet,IS) .le. QZEROGAP (jdet) ++++++++++++++++++++++++++
      endif ! if ( QZEROGAP (jdet) .gt. 0.0001 ) +++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( IS .eq. 99043 .and. jdet .eq. 1 ) then ! ++++++++++++++++++++++++++++
      DEKB1 = ( CMT(jdet,IS) - DMS(1,jdet,IS) )
     &*       ( DKY(2,jdet,IS) - DKY(1,jdet,IS) ) 
     &/       ( DMS(2,jdet,IS) - DMS(1,jdet,IS) )
      DEKB2 = ( CMT(jdet,IS) - DMS(2,jdet,IS) )
     &*       ( DKY(1,jdet,IS) - DKY(2,jdet,IS) )  
     &/       ( DMS(1,jdet,IS) - DMS(2,jdet,IS) )
      perone = 100.0 * ( 1.0 - (DMS(1,jdet,IS) / CMT(jdet,IS)))
      pertwo = 100.0 * ( 1.0 - (DMS(2,jdet,IS) / CMT(jdet,IS)))
      write(09,8372)DNAME(jdet),KOUNT(jdet,IS),IS,CMT(jdet,IS),
     &DKY(1,jdet,IS),DMS(1,jdet,IS),perone,DEKB1,
     &DKY(2,jdet,IS),DMS(2,jdet,IS),pertwo,DEKB2
 8372 format(120('-')/
     &'Progress in gap filling for ',A11,67x,'Iteration:',I4
     &/120('-')/
     &'Number of shot ',i12,   '    Target       ',1pe14.7/
     &'Constant one  ',1pe13.6,'    Result one   ',1pe14.7,
     &' (',1pe12.4,'% )   Correction: ',1pe14.6/
     &'Constant two  ',1pe13.6,'    Result two   ',1pe14.7,
     &' (',1pe12.4,'% )   Correction: ',1pe14.6/120('-'))
      endif ! if ( IS .eq. 99043 .and. jdet .eq. 4 ) +++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      
*     interpolate to obtain the next estimate ----------------------------------
*     the divisor is not zero ... compute the change to the correction ---------
      DEKC = 0.0
      divi = DMS(JWORST,jdet,IS) - DMS(JBEST,jdet,IS)
      short = CMT(jdet,IS) - DMS(JBEST,jdet,IS)
      condiff = DKY(JWORST,jdet,IS) - DKY(JBEST,jdet,IS)
      DEKB = short * condiff / divi

      perone = 100.0 * ( 1.0 - (DMS(1,jdet,IS) / CMT(jdet,IS)))
      pertwo = 100.0 * ( 1.0 - (DMS(2,jdet,IS) / CMT(jdet,IS)))
      if ( zqual (jdet) .ne. 1 ) then
      if ( perone .lt.  0.00001 .and. perone .gt. -0.00001 ) then
      DEKB = DEKB * 0.2
      endif
      if ( pertwo .lt.  0.00001 .and. pertwo .gt. -0.00001 ) then
      DEKB = DEKB * 0.2
      endif
      endif

*     separate the types of determinands ---------------------------------------
      goto (6475,6475,6475,6002,6475,6002), QTYPE (jdet) ! check ???????????????
 5472 continue ! degradable pollutants (type 2) --------------------------------
      if (DEKAY(jdet,IS) .lt. -1.0e-9) then ! deal with the loss ---------------
      DEKC = DKY(JBEST,jdet,IS) + DEKB
      
      endif ! deal with the loss -----------------------------------------------
 6478 continue ! gains of degradable pollutants --------------------------------
      DX1 = DKY(JBEST,jdet,IS)
      DX2 = DX1 + DEKB
      DEKC = DX2
      
      if ( DX1 .gt.  1.0e-09 .and. DX2 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      if ( DX1 .gt.  1.0e-09 .and. DX2 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      if ( DX1 .gt.  1.0e-09 .and. DX2 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      if ( DX2 .gt.  1.0e-09 .and. DX1 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      if ( DX2 .gt.  1.0e-09 .and. DX1 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      if ( DX2 .gt.  1.0e-09 .and. DX1 .lt. -1.0e-09 ) then
      DEKB = 0.3 * DEKB
      DX2 = DX1 + DEKB
      DEKC = DX2
      endif
      goto 6476 ! proceed to make the adjustment -------------------------------

 6475 continue ! losses and gains of determinand type 1 - conservative ---------
      DEKC = DKY(JBEST,jdet,IS) + DEKB
      goto 6476 ! proceed to make the adjustment -------------------------------

 6578 continue ! type 3 is dealt with here -------------------------------------
      DEKC = DKY(JBEST,jdet,IS) + DEKB * 0.5
      goto 6476 ! proceed to the the make adjustment ---------------------------

 6476 continue ! set the next adjustment ---------------------------------------

*     check for a switch in sign -----------------------------------------------
      if ( DEKAY(jdet,IS) .gt. 1.0E-10 .and. DEKC .lt. -1.0E-10 ) then
      DEKC = 0.0001 * DEKC ! apply a smaller change ----------------------------
      DEKAY(jdet,IS) = DEKC
      goto 5476 ! proceed to the the make adjustment ---------------------------
      endif ! check for a switch in sign ---------------------------------------

      TEST = DEKC - DEKAY(jdet,IS)
      if ( TEST .gt. -1.0E-10 .and. TEST .lt. 1.0E-10 ) then
      DEKC = DEKAY(jdet,IS) + DEKB * 0.02
      endif

*     ==========================================================================
 5476 DEKAY(jdet,IS) = DEKC ! ==================================================
*     ==========================================================================

*     halt after maxNITZ iterations, even if convergence fails -----------------
      if (KOUNT(jdet,IS) .gt.  -1 .and. 
     &    KOUNT(jdet,IS) .lt. maxNITZ) goto 6004 ! proceed with new adjustment -
      
      write(33,6006)DNAME(jdet),IS,UNAME(JU)
 6006 format(120('-')/
     &'Incomplete convergence in gap filling for ',a11,32x,
     &'  Shot Number: ',i7/'Feature:     ',A30,7x,
     &' ... the calculation is proceeding with the best estimate',
     &/120('-'))
      write(33,6606)DMS(1,jdet,IS),DMS(2,jdet,IS),CMT(jdet,IS)
 6606 format('Last two estimates:',2f12.6,3x,
     &       '... Required quality:',f12.6/120('-'))
      KOUNT(jdet,IS) = -777 ! suppress gap filling for this shot from here on --
      goto 6002 ! proceed to the next shot

 6004 continue ! store the new adjustment --------------------------------------
      CMX(jdet,IS) = DEKAY(jdet,IS)

 6002 continue ! end of the loop on shots --------------------------------------

      if ( ishotzero(jdet) .gt. 0 ) then ! check shots set to zero ------------
      write(09,6892)Dname(jdet),ishotzero(jdet),NS
      write(01,6892)Dname(jdet),ishotzero(jdet),NS
      write(33,6892)Dname(jdet),ishotzero(jdet),NS
 6892 format(110('-')/a11,
     &': Number of shots reset to the specified minimum quality = ',I4,
     &' out of',i6/110('-'))
      endif ! check shots set to zero ------------------------------------------

*     check for incomplete convergence for this determinand --------------------
*     add the number of unfitted shots -----------------------------------------
      kopped out = kopped out + ishotzero (jdet) ! sum zero results ------------
 6001 continue ! end of the loop on determinands -------------------------------
      
*     monitor and report on convergence ----------------------------------------
*     calculate percent of shots not yet fitted --------------------------------
      PTEST = 999.99 ! initialise the  percent of unfitted shots ---------------
      if ( KTEST .gt. 0 ) then 
      PTEST = 100.0 * never fitted / FLOAT(KTEST) ! percent of shots not fitted 

      if ( kopped out .gt. 0 ) then ! check for zero results -------------------
      write(09,6901)KSIM,PTEST,kopped out,KTEST
      write(01,6901)KSIM,PTEST,kopped out,KTEST
      write(33,6901)KSIM,PTEST,kopped out,KTEST
 6901 format(4x,' [ ***',I4,' Gap filling monitor = ',F6.2,
     &' % ... Imperfect shots =',i4,' in',i5,'  *** ]')
      do jdet = 1,ndet ! -------------------------------------------------------
      if ( qtype(jdet).ne.4 ) then ! -------------------------------------------
      PTEST = 100.0 * not fitted(jdet) / FLOAT(NS) ! percent of shots not fitted 
      write(170+jdet,6901)KSIM,PTEST,kopped out,KTEST
      endif
      enddo ! do jdet = 1,ndet -------------------------------------------------
      endif ! check for zero results -------------------------------------------
      
      if ( iscreen .lt. 3 ) then
      if ( kopped out .gt. 0 ) then
      write(09,6921)KSIM,PTEST
      write(33,6901)KSIM,PTEST,kopped out,KTEST
      do jdet = 1,ndet ! -------------------------------------------------------
      if ( qtype(jdet).ne.4 ) then ! -------------------------------------------
      PTEST = 100.0 * not fitted(jdet) / FLOAT(NS) ! percent of shots not fitted 
      write(170+jdet,6921)KSIM,PTEST
 6921 format(i8,7x,' Remaining gap =',F8.2,' %')
      endif ! if ( qtype(jdet).ne.4 ) ------------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
      
      else ! checked for zero results ------------------------------------------
      call change colour of text (34) ! dull yellow ... no zero results --------
      if ( ptest .gt. 0.0001 ) then ! print final result =======================
      if ( KSIM .eq. maxNITZ ) then ! ==========================================
      call change colour of text (20) ! bright red ... zero results ------------
      write( *,6922)100.0-PTEST ! percent of fitted shots ----------------------
 6922 format(5x,'Shots fitted by gap filling',F11.2,' %')
      call set screen text colour
      write(09,6933)100.0-PTEST ! percent of fitted shots ----------------------
 6933 format(/5x,'Shots fitted by gap filling',F11.2,' %')
      endif ! if ( KSIM .eq. maxNITZ ) =========================================
      endif ! if ( ptest .lt. 0.0001 ) print final result ======================
      
      !if ( KSIM .eq. maxNITZ ) then ! iterations equal maximum required ========
      !write(33,6923)PTEST,KSIM
      !6923 format('Remaining gap =',F8.2,' % after exceeding',i3,
      !&' iterations ...')
      !endif ! if ( KSIM .eq. maxNITZ ) =========================================
      
      ptestx = ptest ! store the percent of fitted shots for future use --------
      endif ! if ( kopped out .gt. 0 )
      endif ! if ( iscreen .lt. 3 )

      if ( PTEST .gt. 999.0 ) goto 8995 ! finished

*     check for remaining unconverged shots and iterations +++++++++++++++++++++
      if ( never fitted .gt. 0 .and. KSIM .lt. maxNITZ ) then
      goto 6009 ! ------------------------------------- start the next iteration
      endif
      !if ( never fitted .eq. 0 .or. KSIM .ge. maxNITZ ) then
      if ( KSIM .eq. maxNITZ ) never fitted = -999 ! ------- iterations complete
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     iterations complete ------------------------------------------------------
      if ( ifbatch .eq. 0 ) then
      write(01,7921)100.0-PTEST
      write(33,7921)100.0-PTEST
 7921 format(5x,77('#')/5x,'Shots matched by gap filling =',F7.2,' % ',
     &' (covering all determinands)'/5x,77('#'))
      do jdet = 1,ndet ! -------------------------------------------------------
      if ( qtype(jdet) .ne. 4 ) then ! -----------------------------------------
      ptest = ((NS - not fitted(jdet))/float(NS))*100.0
      write(170+jdet,3921)dname(jdet),PTEST,numbits(jdet) ! ------------- Ci.GAP
 3921 format(/110('#')/'Shots matched for ',a11,' by gap filling =',
     &F7.2,' % ',19x,'Number of iterations required:'i5/110('#')/)
      if ( KSIM .eq. 1 ) KSIM = 2
      endif ! if ( qtype(jdet) .ne. 4 ) ----------------------------------------
      enddo ! do jdet = 1,ndet -------------------------------------------------
      endif

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( KSIM .eq. maxNITZ ) then ! count any incomplete shots ---------------
      do jdet = 1, ndet
      if ( qtype(jdet) .ne. 4 ) then
      kount bad shots = 0
      do IS = 1, NS
      if ( KOUNT(jdet,IS) .ne. -777 ) 
     &kount bad shots = kount bad shots + 1
      enddo
      if ( kount bad shots .gt. 0 ) then ! print zero divisors -----------------
      if ( iscreen .lt. 3 ) then
      call change colour of text (14) ! bright yellow
      write( *,6956)DNAME(jdet),kount bad shots
 6956 format(5x,'Zero divisors for ',a11,' ... check the ERR file',i8)
      call set screen text colour
      endif
      write(01,6856)DNAME(jdet),kount bad shots,NS
      write(33,6856)DNAME(jdet),kount bad shots,NS
      write(09,6856)DNAME(jdet),kount bad shots,NS
      write(170+jdet,6856)DNAME(jdet),kount bad shots,NS
 6856 format(110('-')/'Zero divisors in gap filling for ',a11,
     &' ... check the ERR file for details ...'/
     &'SIMCAT used the best estimates for',i4,
     &' shots out of',i5,' ... '/110('-'))
      endif ! if ( kount bad shots .gt. 0 ) then  
      endif ! if ( qtype(jdet) .ne. 4 )
      enddo ! do jdet = 1, ndet
      endif ! if ( KSIM .eq. maxNITZ ) finished counting incomplete shots ------
      
      ICTOP = 0 ! switch off the indicator for the beginning of a reach --------
*     store the Flow and Quality Shots in preparation for the next gap filling -
*     downstream. These shots are picked up by RELOAD in [GAP FILL SIMULATION] -
      call DUMP (1)
      endif ! if ( KTEST .gt. 0 ) 

*     store the results on the quality gap filling file (.QCL) -----------------
      do J = 1, ndet
      if ( qtype(J) .ne. 4 ) then
      write(75,4562)DNAME(J),dcalquality,NEXC(J)
 4562 format('QUALITY',6(1H*),A11,7(1H*),' NATURAL PURIFICATION',
     &58('-'),F10.2,I2)
      write(75,8448)(CMX(J,IS),IS=1,NS)
 8448 format(10(1PE18.10))
      endif
      enddo
      
      

*     "dcalquality" is the distance from the head of the reach or the last ----
*     point of gap filling ... reset to zero -----------------------------------
 8995 continue
      dcalquality = 0.0

*     gap filling complete -----------------------------------------------------
      goto 2995

*     check for a feature downstream of last gap filling Point in Reach --------
 1995 continue

      if (KQCAL(JU) .eq. 0 ) goto 2995 ! last gap filling point not reached ----

*     check whether changes will be extrapolated downstream --------------------
*     if ( NEXC(J) .ne. 0) goto 2995
      if ( FLOW(1) .lt. 1.0E-08 ) then
      if ( nobigout .le. 0 ) write(01,5481)UNAME(JU)
      call change colour of text (10) ! green
      if ( iscreen .lt. 3 ) write( *,5481)UNAME(JU)
      call set screen text colour
      write(09,5481)UNAME(JU)
      write(33,5481)UNAME(JU)
      do jper = 1, ndet
      if ( qtype(jper).ne.4) write(170+jper,5481)UNAME(JU)
 5481 format(93('-')/
     &'*** Zero flow after a gap filling point ...'/
     &'*** Extrapolation suppressed downstream of Feature ... ',
     &A40/93('-'))
      enddo
      return
      endif

*     extrapolate changes for Features downstream of Gap Filling Point wowowowowowo
      call extrapolate quality gap filling (JU,DISTP) ! 333333333333333333333333333

 2995 continue

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! gap filling ------------

      return
      end




      subroutine add the loads from flow gap filling (JU,JSTOPF1)
      include 'COMMON DATA.FOR'
      dimension oldshots(mp10,ns)
      
*     compute the mean load before the gap filling of flow ---------------------
      call load calculation ! return with load shots ... xshots ----------------

      do jdet = 1, ndet
      do is = 1, ns
      old shots (jdet,is) = xshots (jdet,is) ! store the old quality shots -----
      enddo
      enddo

      xupflow = 0.0
      xdownflow = 0.0

      call add the gap filled river flows (JU,JSTOPF1)

      fxxx = 0.0 ! calculate the new mean flow ...
      do is = 1, NS 
      fxxx = fxxx + FMS(is)
      enddo
      fxxx = fxxx / float (NS)
     
      call load calculation ! calculate xload etc ...

      do 27 idet = 1, ndet ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( qtype (idet) .ne. 4 ) then ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
      do is = 1, ns ! calculate the change in the load shots -------------------
      xshots(idet,is) = xshots(idet,is) - oldshots(idet,is) ! --- change in load
      enddo ! do is = 1, ns ----------------------------------------------------

      do J13 = 1, N13
      NSM (idet,J13) = 0
      xupload (idet,J13) = 0.0
      xdownload (idet,J13) = 0.0
      enddo ! do J13 = 1, N13
      
*     accumulate the mean load added by gap filling ----------------------------
      do  is = 1, ns
      imonth = qmonth (is) ! set the month for this shot

      do J13 = 1, N13
      K13 = J13
      if ( J13 .gt. 1 ) K13 = imonth + 1
      if ( K13 .gt. N13 ) K13 = N13
 
      if ( xshots (idet, is) .ge. 1.0e-10 ) then
      xupload (idet,K13) = xupload (idet,K13) + xshots (idet, is)
      else
      xdownload (idet,K13) = xdownload (idet,K13) + xshots (idet, is)
      endif
      NSM (idet,K13) = NSM (idet,K13) + 1
      enddo ! do J13 = 1, N13
      enddo ! do  is = 1, ns

      xdownload (idet,i13) = xdownload (idet,i13) / float (ns)
      xupload (idet,i13) = xupload (idet,i13) / float (ns)
      xnet = xupload (idet,i13) + xdownload (idet,i13) 

      if ( xnet .gt. 0.00001) then
      xupload (idet,i13) = xnet
      xdownload (idet,i13) = 0.0
      else
      xdownload (idet,i13) = xnet
      xupload (idet,i13) = 0.0
      endif

      TILOADDN2 (idet,i13) = TILOADDN2 (idet,i13) ! net load
     &                     + xdownload  (idet,i13) 

      if ( munthly structure .eq. 1 ) then ! ----------- store the monthly loads
      do J13 = 2, N13
      if ( NSM  (idet,J13) .gt. 0 ) then
      xupload    (idet,J13) = xupload    (idet,J13) 
     &                      / float (NSM (idet,J13))
      xdownload  (idet,J13) = xdownload  (idet,J13) 
     &                      / float (NSM (idet,J13))
      xnet = xupload (idet,J13) + xdownload (idet,J13)

      if ( xnet .gt. 0.00001) then
      xupload (idet,J13) = xnet
      xdownload (idet,J13) = 0.0
      else
      xdownload (idet,J13) = xnet
      xupload (idet,J13) = 0.0
      endif

*     net load removed by gap filling for river flows --------------------------
      TILOADDN2 (idet,J13) = TILOADDN2 (idet,J13) 
     &                     + xdownload (idet,J13)
      
      endif ! if ( NSM  (idet,J13) .gt. 0 )
      enddo ! do J13 = 2, N13
      endif ! fill monthly loads

      kprune det = idet

*     ====================================================================== 158
      nx = n13 ! ----------------------------------------------------------- 158
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx ! ------------------------------------------------------ 158
      prune load (J1) = 1.0 ! --------------------------------- flow gap filling
      if ( abs ( TGLODE2(idet,J1) ) .gt. 0.000000001 ) then
      if ( xdownload (idet,J1) .lt. -0.0000001 ) then
      if ( TGLODE2(idet,J1) + xdownload (idet,J1) .gt. 0.0 ) then
      prune load (J1) = ( TGLODE2(idet,J1) + xdownload (idet,J1) )
     &                /   TGLODE2(idet,J1)
      endif
      endif
      endif
      enddo ! do J1 = 1, nx ------------------------------------------------ 158
*     ====================================================================== 158

      call scale loads after an abstraction ! -------- from abstractions of flow

*     ====================================================================== 158
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 ! no need for monthly loads
      do J1 = 1, nx      
      tiloadup2 (idet,J1) = tiloadup2 (idet,J1) + xupload (idet,J1) ! :::::: 158
      enddo
*     ====================================================================== 158

      endif ! if ( QTYPE (idet) .ne. 4 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   27 continue ! idet = 1, ndet ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      
      call add up all the loads ! for all determinands
      call update summaries of contribution      
      call write loads after gap filling of river flows 2
      call write loads after gap filling of river flows 3

      return
      end

      
      
      
*     add in the diffuse inflows calculated by gap filling ---------------------
*     these will have been calculated in flow gap filling under ICAL=1 ---------
*     they are read in from channel 74 -----------------------------------------
      subroutine add the gap filled river flows (JU,JSTOPF)
      include 'COMMON DATA.FOR'
      character *136 RECORD
      character *044 FEATNAM
      character *1 char1,char2,char3
      character *4 char4
      dimension jfzero(MP10)

      
      JSTOPF1 = JSTOPF ! set to zero
      call set screen text colour
*     ==========================================================================
*     special procedure for section at end of Reach and downstream of the last -
*     Feature in the Reach -----------------------------------------------------
      if ( IEND .eq. 0 ) goto 6603
 6609 if ( NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU) ! at end of Reach -----------
      endif
      goto 9000 ! return
 6603 continue
      if ( KFCAL(JU) .eq. 1 ) goto 6609

*     prepare to read record from channel 74 for gap filling for river flow ----
      rewind 74 ! rewind the data file -----------------------------------------
      read(74,1001,end=9993,ERR=9993)RECORD ! read the next line ---------------
      rewind 74     
      
 1000 continue ! prepare to read the next line
      do is = 1, NS
      FMX(IS) = 0.0
      enddo
      goto 8888
      
 9009 rewind 74
      
 8888 read(74,1001,end=9993,ERR=9993)RECORD ! read the next line ---------------
 1001 format(A136)

*     pick out first, fourth and eighth characters -----------------------------
      read(record,5300) char1,char2,char3
 5300 format (a1,2x,a1,3x,a1 )
      if ( char3 .eq. '*' ) goto 1000
      if ( char1 .eq. 'F' ) goto 1000
      if ( char2 .eq. '.' ) goto 1000

*     a descriptive heading has been found -------------------------------------
*     DP is the distance from the last Feature ---------------------------------
      backspace 74
      read(74,*,ERR=9998) LREACH, DP
      if ( LREACH .lt. 0 ) LREACH = -LREACH
      if ( KINT .ne. 0 ) DP = DINT
      if ( LREACH .ne. IREACH ) goto 1000

*     read the name of the Feature ---------------------------------------------
      backspace 74
      read(74,1005)FEATNAM
 1005 format(47X,A44)
      if ( FEATNAM .ne. UNAME(JU) ) goto 1000

*     data for a Feature have been found ---------------------------------------
*     check whether this Feature is a gap filling point ------------------------

      read(74,1001,end=9999,ERR=9998)RECORD
      read(RECORD,5300)char1,char2

      if ( char1 .eq. '*' ) goto 2000
*     check whether Feature is a gap filling point for flow --------------------

      read(RECORD,5301) char4
 5301 format (a4)
      if ( char4 .eq. 'FLOW' ) goto 2090

*     the Feature is not a gap filling point -----------------------------------
      write(01,1011)
      write(33,1011)
      write( *,1011)
 1011 format(//'*** Error in assembling the flow gap filling file....'/
     &         '*** Prime gap filling site has no flow data...')
      call stop

*     the Feature is a point for gap filling point of flow --------------------
 2090 continue

*     if the distance, DP is zero it must be set to 1.0 ------------------------
*     this forces in the corrections -------------------------------------------
      if (JFUSER(JU) .eq. 0 .and. DP .lt. 1.0e-6) DP = 1.0

*     read indicator for extrapolation of flows --------------------------------
      backspace 74
      read(74,4422,ERR=3093)NEXF
 4422 format(126X,I2)
      JFUSER(JU)=1
 3000 continue
      
*     read the flow adjustments ------------------------------------------------
      read(74,8448,ERR=8118)(FMX(IS),IS=1,NS) ! read the flow adjustments ------
 8448 format(10(1PE18.10))
      
*     change the flows by gap filling ------------------------------------------
*     at the same time work out the average effect of gap filling of flows -----
      avacfl = 0.0 ! prepare to calculate the mean added flow ------------------
      kchk = 0 ! initialise counter of zero flows ------------------------------
      do 1006 IS = 1,NS ! ------------------------------------------------------
      avacfl = avacfl + FMX(IS) ! sum the net total addition -------------------
      FMS(IS) = FMS(IS) + DP * FMX(IS) ! add (or remove) the flow adjustment ---
      if ( FMS(IS) .lt. 1.0E-10 ) then ! check for zero calculated flow --------
      FMS(IS) = 1.0e-10 ! zero flow has been encountered and re-set to zero ----
      kchk = kchk + 1 ! count the zeroes ---------------------------------------
      endif ! if ( FMS(IS) .lt. 1.0E-10 ) --------------------------------------
 1006 continue ! loop on shots -------------------------------------------------
 
      
      if ( KSIM .le. 1 ) then ! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      if ( kchk .gt. 0 ) then ! check for zero flows zzzzzzzzzzzzzzzzzzzzzzzzzzz
      call change colour of text (20) ! bright red zzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      write( *,9286)kchk ! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
 9286 format(77('-')/
     &'*** Negative river flows calculated by gap filling ...'/
     &'*** Number of shots affected =',i6,
     &' ... these have been set to zero ...'/77('-'))
      call set screen text colour
      if ( kchk .lt. 10 ) then
      write(01,9286)kchk
      write(09,9286)kchk
      write(33,9286)kchk
      do jper = 1, ndet
      if ( qtype(jper) .ne. 4 ) then    
      write(170+jper,9286)kchk
      endif
      enddo
      else
      write(01,9186)kchk
      write(09,9186)kchk
      write(33,9186)kchk
      do jper = 1, ndet
      if ( qtype(jper) .ne. 4 ) then    
      write(170+jper,9186)kchk
      endif
      enddo
 9186 format(/110('-')/
     &'*** Negative river flow calculated by Gap Filling ... '/
     &'*** Number of shots affected =',i6,
     &'... these have been set to zero ...'/
     &'*** This may mean lead to less than exact ',
     &'agreement through Gap Filling for water quality ...'/
     &'*** The addition of load through Gap Filling ',
     &'may produce high values because of the zero '/
     &'*** dilution ... ',
     &' The remedy is to sort out the flow balance ...'/
     &110('-'))
      endif
      endif ! if ( kchk .lt. 1 ) zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      endif ! if ( KSIM .le. 0 ) zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

      
      avacfl = avacfl / float (NS) ! calculate the average flow ----------------
      dclfl = avacfl * DP ! average added over DP kilometres -------------------
      call sort format 3 (dclfl,avacfl,DP)

      if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++++++++++
      if ( KSIM .eq. 1 ) then ! ++++++++++++++++++++++++++++++++++++++++++++++++
          
      if ( ical .eq. 3 .or. ical .eq. 2 .or. ical .eq. 1 ) then ! ~~~~~~~~~~~~~~
      if ( ical .ne. 3 ) write(01,4122)KSIM ! ------------------------------ OUT
 4122 format(/110('~')/'Iteration number',i4) ! ---------------------------- OUT
      write(01,4522)valchars12,valchars10,funit,uname(feeture) ! ----------- OUT
 4522 format(110('~')/'Interpolation of flow ',
     &'for ',a10,' km ... towards the d/s flow gauge (gap filling)'/
     &'The resulting change in mean flow is ',5x,
     &a10,1x,a4,20x,' at ',a35/110('~')) ! --------------------------------- OUT
      
      do jper = 1,ndet ! ===== report on gap filling for river flow u/s of gauge
      if ( qtype(jper) .ne. 4 ) then ! =========================================
      if ( ical .eq. 1 ) write(170+jper,4622)KSIM ! --------------------- Ci.GAP
 4622 format(/110('~')/'Iteration number',i4) ! ------------------------- Ci.GAP
      
      if ( DIST(feeture) .gt. 0.001 ) then ! ---------------------------- Ci.GAP
      write(170+jper,4523)valchars11, ! --------------------------------- Ci.GAP
     &funit,valchars12,valchars10,funit ! ------------------------------- Ci.GAP      
 4523 format(33x, ! ----------------------------------------------------- Ci.GAP
     &'Effects of gap filling on the annual mean river flow ...'/ !------ Ci.GAP     
     &33x,'(Upstream of the gauge that is being targeted ...)'/33x, ! --- Ci.GAP
     &77('-')/33x,'Added 'a10,1x,a4,'/km over ',a10,' km ...      ', ! -- Ci.GAP
     &'an added ',a10,1x,a4/33x,77('=')) ! ------------------------------ Ci.GAP
      endif ! if ( DIST(feeture) .gt. 0.001 ) --------------------------- Ci.GAP
      endif ! if ( qtype(jper).ne. 4 ) =========================================
      enddo ! do jper = 1,ndet ===== report on gap filling for flow u/s of gauge
      endif ! if ( ical .eq. 3 .or. ical .eq. 2 .or. ical .eq. 1 ) ~~~~~~~~~~~~~ 
      
      else ! if ( KSIM .eq. 1 ) ++++++++++++++++++++++++++++++++++++++++++++++++
          
      call sort format 3 (dclfl,avacfl,DP) ! ---------------------------- Ci.GAP
      do jper = 1,ndet ! ===== report on gap filling for river flow d/s of gauge
      if ( qtype(jper) .ne. 4 ) then ! =========================================
      if ( KSIM .eq. 1 ) write(170+jper,4582)KSIM, ! -------------------- Ci.GAP
     &valchars12,valchars11,funit,valchars10,funit ! -------------------- Ci.GAP
 4582 format(/110('+')/'Iteration number',i4/110('-')/ ! ---------------- Ci.GAP
     &'Interpolation of river flow towards the d/s river flow gauge ',
     &'for ',a10,' kilometres ... (Gap Filling)'/
     &'Average flow added by gap filling at ',
     &a10,1x,a4,'/km leads to ',a10,1x,a4/110('+')/)
      endif ! if ( qtype(jper) .ne. 4 ) ========================================
      enddo ! do jper = 1,ndet ===== report on gap filling for flow d/s of gauge
      
      endif ! if ( KSIM .eq. 1 ) +++++++++++++++++++++++++++++++++++++++++++++++
      endif ! if ( nobigout .le. 0 ) then ! ++++++++++++++++++++++++++++++++++++

      
      do JP = 1, ndet
      jfzero(JP) = 0 ! =========================================================
      do j13 = 1, N13
      NSM(JP,j13) = 0
      enddo
      enddo
      
      do 2008 IS = 1, NS ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
*     RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      if (FMX(IS) .lt. 1.0e-10) then ! this flow is removed by gap-filling RRRRR
      FOLD = FMS(IS) - DP*FMX(IS) ! and the old flow exceeds the new -----------
      xdownflow = xdownflow + DP*FMX(IS) ! total removed flow RRRRRRRRRRRRRRRRRR     
      do 3518 JP = 1, ndet ! ============================================= RRRRR
      if ( qtype (JP) .ne. 4 ) then ! ==================================== RRRRR
      
*     calculate the removed load ===============================================
      ABLOAD(JP,i13) = ABLOAD(JP,1) + DP*FMX(IS)*CMS(JP,IS) ! removed load -----
      NSM(JP,i13) = NSM(JP,i13) + 1
      K13 = qmonth(is) + 1

      ABLOAD(JP,K13) = ABLOAD(JP,K13) + DP*FMX(IS)*CMS(JP,IS) 
      NSM(JP,K13) = NSM(JP,K13) + 1
      
      endif ! if ( qtype (JP) .ne. 4 ) =========================================
 3518 continue! do 3018 JP = 1, ndet ===========================================
      endif ! if (FMX(IS) .lt. 1.0e-10) ----------------------------------------      
*     RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR



*     AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      if (FMX(IS) .gt. 1.0e-10) then ! flow has been added AAAAAAAAAAAAAAAAAAAAA
      xupflow = xupflow + DP*FMX(IS) ! sum the added flows AAAAAAAAAAAAAAAAAAAAA
      FOLD = AMAX1 ( 0.0, FMS(IS) - DP*FMX(IS) ) ! flow before the addition AAAA         
*     add in the extra load AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      do 2018 JP = 1, ndet ! add in the extra load --------AAAAAAAAAAAAAAAAAAAAA
      if ( qtype (JP) .ne. 4 ) then ! ------------------------------------------
      temp = 0.0 ! initalise added load ----------------------------------------
      if ( detype (JP) .ne. 104 ) then ! not dissolved oxygen AAAAAAAAAAAAAAAAAA
      temp = QDIFFGAP(JP) ! quality of diffuse inflows added by gap filling AAAA
      if ( temp .gt. 1.2 * C(JP,1) ) then ! ------------------------------------
      jfzero(JP) = jfzero(JP) + 1
      JSTOPF1 = JSTOPF1 + 1
      endif ! if ( temp .gt. 1.2 * C(JP,1) ) -----------------------------------
      endif ! if ( detype (JP) .ne. 104 ) AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      
      xfload = FMX(IS) * DP * temp ! load added by gap filling AAAAAAAAAAAAAAAAA
     
      old CMS = CMS(JP,IS) ! old concentration ! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      CMS(JP,IS)=(CMS(JP,IS)*FOLD+xfload)/FMS(IS) !  add effect on river quality
  
      do ip = 1, n2prop ! add to CTMS for gap filling of flow at gauge AAAAAAAAA
      oldCTMS = CTMS(ip,JP,IS) ! old value of CTMS 
      if ( ip .eq. 21 ) then ! for gap filling of added flows AAAAAAAAAAAAAAAAAA
*     mass balance to add in the contribution from gap filling of flow (21) AAAA
      CTMS(ip,JP,IS) = ((CTMS(ip,JP,IS)*FOLD) + xfload)/FMS(IS) ! new AAAAAAAAAA
      else ! otherwise, for other types of pollution ---------------------------
*     dilute the concentrations of all the other contributions AAAAAAAAAAAAAAAAA
      if ( ip .eq. NTD ) then ! for total river concentration AAAAAAAAAAAAAAAAAA
      CTMS(ip,JP,IS) = ((CTMS(ip,JP,IS) * FOLD) + xfload )/FMS(IS) ! AAAAAAAAAAA
      else ! dilute all other contrinutions AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * FOLD / FMS(IS) !  dilute everything else
      endif
      endif ! if ( ip ... for gap filling of flows added at flow gauge AAAAAAAAA
      enddo ! do ip = 1, n2prop ... add effects to CTMS from gap filling of flow
      endif ! if ( qtype (JP) .ne. 4 ) AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 2018 continue ! do 2018 JP = 1, ndet  ! add in the extra load AAAAAAAAAAAAAAAAA
      endif ! if (FMX(IS) .gt. 1.0e-10) ! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      
 2008 continue ! do 2008 IS = 1, NS ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
      call update summaries of contribution ! to CMcontrib and LMcontrib --------

      
      
*     calculate the annual load removed by the gap filling of flows ===========2
      do 1611 jdet = 1, ndet ! ================================================2
      if ( qtype (jdet) .ne. 4 ) then ! ----------------------------------------
      prune load (I13) = 1.0  
      ABLOAD(jdet,I13) = ABLOAD(jdet,I13) / float(NS)
      
      if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 ) then ! ............2
      if ( LMcontrib(NTD,jdet,1) + abload(jdet,I13) .gt. 0.0 ) then
      prune load(I13) = (LMcontrib(NTD,jdet,1) + abload(jdet,I13)) ! ..........2
     &                /  LMcontrib(NTD,jdet,1)
      endif
      endif ! if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 ) ...........2
      
      do J13 = 2, N13 ! repeat for monthly data for losses of flow ............2
      prune load (J13) = 1.0  
      if ( NSM(jdet,J13) .gt. 1 ) then
      ABLOAD(jdet,J13) = ABLOAD(jdet,J13) / float(NSM(jdet,J13))
      
      if ( abload(jdet,J13) .lt. 0.0 ) then ! .......... losses of flow .......2
      if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 ) then ! ............2
      if ( LMcontrib(NTD,jdet,1) + abload(jdet,J13) .gt. 0.0 ) then
      prune load(J13) = (LMcontrib(NTD,jdet,1) + abload(jdet,J13)) ! ..........2 
     &                /  LMcontrib(NTD,jdet,1)
      endif 
      endif ! if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 ) ...........2
      endif ! if ( abload(jdet,J13) .lt. 0.0 ) losses of flow .................2
      
      endif ! if ( NSM(jdet,J13) .lt. 1 ) .....................................2
      enddo ! do J13 = 2, K13 repeat for monthly data for losses of flow ......2

      
*     trim annual loads from individual catchments =============================
      if ( kount bodies .gt. 0 ) then ! ========================================
      do ibodies = 1, kount bodies ! ===========================================
*     annual loads (i13) from upstream sub-catchments -------------------------2
      TWLOADS(ibodies,jdet,i13) = ! scale load after losses of flow -----------2
     &TWLOADS(ibodies,jdet,i13) * prune load (i13) ! scale loads --------------2

      do ip = 1, n2prop ! scale contributions after losses of flow ------------2
*     breakdown of annual loads (i13) from upstream sub-catchments ------------2
          
      TWLOADSapp(ibodies,jdet,i13,ip) = ! reduce loads after losses of flow ---2
     &TWLOADSapp(ibodies,jdet,i13,ip) * prune load (i13) ! reduce loads -------2
      
      enddo ! do ip = 1, n2prop -----------------------------------------------2
      enddo ! do ibodies = 1, kount bodies ! ==================================2
      endif ! if ( kount bodies .gt. 0 ) ======================================2

      endif ! if ( qtype (jdet) .ne. 4 ) =======================================
 1611 continue ! do 1611 jdet = 1, ndet ========================================

      
      do JP = 1, ndet ! wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww test for a problem
      if ( qtype (JP) .ne. 4 ) then ! wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
      if ( jfzero(JP) .gt. 0 .and. KSIM .ge. MAXNITZ ) then ! -------------------
      call change colour of text (12) ! orange
      write( *,8644)dname(JP),jfzero(JP)
 8644 format('* Quality of the flow added by gap-filling is worse ',    
     &'than that in the receiving river. Values for ',a11,i6)
      call set screen text colour
      write(09,8644)dname(JP),jfzero(JP)
      write(33,8644)dname(JP),jfzero(JP)
      endif ! if ( jfzero(JP) .gt. 0 ) then ------------------------------------
      endif ! if ( qtype (JP) .ne. 4 ) then ------------------------------------
      enddo ! do JP = 1, Ndet wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
      
      
      xdownflow = xdownflow / float (NS)
      xupflow = xupflow / float (NS)
      goto 9000 ! return

*     This Feature is not a gap filling point for flow -------------------------
*     Find the Point providing data for this Feature ...
*     This will be downstream of the current Feature ...
*     Read on through FCALIB (channel 74) to find it ...
*     The next record must hold descriptive data for the next Feature ...

 2000 continue
      if (DP .lt. 1.0E-10) goto 9000 ! return
 2006 read(74,1001,end=9996,ERR=9998)RECORD
      read(RECORD,5300) char1,char2
      if ( char1 .ne. ' ' ) goto 2006
      if ( char2 .eq. '.' ) goto 2006

*     the next record has been found to hold descriptive data ------------------
 2001 Backspace 74
      read(74,*)LREACH
      if ( LREACH .lt. 0 ) LREACH = -LREACH
      Backspace 74
      read(74,1005)FEATNAM
      if (LREACH .ne. IREACH) goto 9996

*     is this Feature the gap filling point for flow? --------------------------
      read(74,1001,end=9997,ERR=9998)RECORD
      read(RECORD,5300) char1
      if ( char1 .eq. '*' ) goto 2006

*     yes it is ----------------------------------------------------------------
      read(RECORD,5301) char4
      if ( char4 .ne. 'FLOW' ) goto 9000 ! return
      do 6114 KUSER = JU, MU
      if ( FEATNAM .ne. UNAME(KUSER) ) goto 6114
      JFUSER(KUSER) = 1
      goto 6115
 6114 continue
 6115 continue
      goto 3000
 9000 continue

      rewind 74
      return

 3093 write(01,2051)
      write( *,2051)
      write(09,2051)
      write(33,2051)
 2051 format('*** Error in reading NEXF from FLOW gap filling ...')
      call stop

 8118 continue
      write(01,8107)
      write(09,8107)
      write(33,8107)
      write( *,8107)
 8107 format(/'*** Error in reading the flow adjustments ',
     &'for gap filling ...')
      call stop

 9998 write(33,3900)FEATNAM
      write(01,3900)FEATNAM
      write(09,3900)FEATNAM
      write(33,3900)FEATNAM
 3900 format(/
     &'*** Error in reading the gap filling data for quality ...'/
     &'*** Current Feature ... ',A40/
     &'*** CALCULATION continues ...')
      if ( suppress5 .lt. 3 ) then
      suppress5 = suppress5 + 1
      call change colour of text (22) ! light blue
      write( *,4993)FEATNAM
 4993 format('*** Error in reading flow gap filling data ',
     &' ... ',15x,'for ',A40)
      endif
      call set screen text colour
      return

 9993 write(01,3961)
      call change colour of text (20) ! bright red
      write( *,3961)
      call set screen text colour
      write(09,3961)
      write(33,3961)
 3961 format(/'*** NO FLOW GAP DATA  ... ',
     &'Reverted to a basic run for flow data ...'/) 
      ical = 0
      call pause
      return      

 9997 write(01,3901)UNAME(JU),FEATNAM
      write( *,3901)UNAME(JU),FEATNAM
      write(09,3901)UNAME(JU),FEATNAM
      write(33,3901)UNAME(JU),FEATNAM
 3901 format(//'*** NO FLOW GAP FILLING POINT FOR FEATURE ...'/
     &         '*** Current Feature ... ',A40/
     &         '*** SEARCHED AS FAR AS ... ',A40/
     &         '*** CALCULATION STOPPED ...')
      call stop

 9999 continue ! reached the end of reading gap fill data for river flow =======
      !return
      call change colour of text (20) ! bright red 
      write( *,3993)UNAME(JU)
 3993 format('*** No gap filling of flow',25x,'...',7x,'at ',A40,
     &' error in sequencing of features')
      call set screen text colour
      write(01,3903)UNAME(JU)
      write(09,3903)UNAME(JU)
      write(33,3903)UNAME(JU)
      do jper = 1,ndet
      if ( qtype(jper).ne. 4 ) write(170+jper,3903)UNAME(JU)
      enddo
 3903 format(/123('-')/
     &'No gap filling of river flows for the Feature called ',A40/
     &'The gap filling file is out of step with the file now being ',
     &'run ... '/
     &'Features have been added or removed since gap filling ...'/ 
     &'Gap filling of flows has not been done at this point ...'/
     &123('-')/
     &'**** This can also occur when a Reach has gap filling ',
     &'for river quality ...'/
     &'**** but no gap filling for river flow'/
     &'**** Repeat the gap filling with a flow gap filling point ',
     &'in the Reach ...'/123('-'))
      return

 9996 continue
      
      return
      end

      
      
*     Add in the effects of gap filling ----------------------------------------
*     These will have been calculated by Sub-routine FLOW gap filling ----------
*     This applies the effects downstream of the gap filling point -------------
      subroutine extrapolate gap filled river flows (JU)
      include 'COMMON DATA.FOR'
      character *40UNAM
      dimension jfzero(MP10)  

      UNAM='Tail of Reach............               '
      if ( IEND .eq. 0 )UNAM=UNAME(JU)

      if ( IEND .eq. 1 ) goto 6401
      if ( KFCAL(JU) .eq. 0 ) return
 6401 continue
      if ( DISTP .lt. 1.0E-06 ) return

      JSTOPF2 = 0
      
*     add in the "extra" flows -------------------------------------------------
*     at the same time work out the average effect of gap filling --------------
      avacfl = 0.0
      
      do JP = 1, ndet ! ========================================================
      jfzero(JP) = 0 ! =========================================================
      enddo ! ==================================================================
      do 2008 IS = 1, NS ! =====================================================
      avacfl = avacfl + FMX(IS) ! sum of the flows from all shots --------------
      if (FMX(IS) .lt. 1.0E-10) goto 2008 ! leaks will not be extrapolated =====
      FOLD = FMS(IS) ! store the starting flow for this shot -------------------
      FMS(IS) = FMS(IS) + DISTP * FMX(IS) ! calculate the new flow -------------

      do 2018 JP = 1, ndet ! exrapolare the gains AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      if ( qtype (JP) .ne. 4 ) then ! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      if ( detype (JP) .ne. 104 ) then ! but not dissolved oxygen AAAAAAAAAAAAAA
      temp = QDIFFGAP(JP) ! concentration added by gap filling flows AAAAAAAAAAA
      if ( temp .gt. 1.2 * C(JP,1) ) then !  is it bigger than the current mean?
      jfzero(JP) = jfzero(JP) + 1 ! --- number of shots 20% bigger than the mean
      JSTOPF2 = JSTOPF2 + 1
      endif ! ------------------------------------------------------------------
      endif ! if ( detype (JP) .ne. 104 ) not dissolved oxygen =================

      xfload = FMX(IS) * DISTP * temp ! - loads added for this shot and distance
      CMS(JP,IS)=(CMS(JP,IS)*FOLD+xfload)/FMS(IS) ! resulting concentration ----

      do ip = 1, n2prop ! === add to CTMS from extrapolating gap filling of flow
      if ( ip .eq. 21 .or. ip .eq. NTD ) then ! ====== add from flow gap-filling
*     mass balance to add to the contribution to (21) ==========================
      CTMS(ip,JP,IS) = ((CTMS(ip,JP,IS) * FOLD) + xfload ) / FMS(IS)
      else ! for other types of contribution ... 
*     dilute the concentrations other contributions to other totals AAAAAAAAAAAA
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * FOLD / FMS(IS) ! dilute contribution AAA
      endif ! if ( ip .eq. 21 ) for gap filling of flows - extrapolation =======
      enddo ! do ip = 1, n2prop === add effects to CTMS from gap filling of flow
      endif ! if ( qtype(JP) .ne. 4 ) AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 2018 continue ! do 2018 JP = 1, ndet AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 2008 continue ! do 2008 IS = 1, NS AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

      call update summaries of contribution ! to CMcontrib and LMcontrib - EXTRAP

      do JP = 1, ndet ! ========================================================
      if ( qtype (JP) .ne. 4 ) then ! ------------------------------------------
      if ( jfzero(JP) .gt. 0 .and. KSIM .ge. MAXNITZ) then ! -------------------
      call change colour of text (19) ! light pink
      write( *,8644)dname(JP),jfzero(JP)
 8644 format('* Quality of the flow added by gap-filling is worse ',
     &'than that in the receiving river. Values for ',a11,i6)
      call set screen text colour
      write(09,8644)dname(JP),jfzero(JP)
      write(33,8644)dname(JP),jfzero(JP)
      endif ! if ( jfzero(JP) .gt. 0 ) then ------------------------------------
      endif ! if ( qtype (JP) .ne. 4 ) then ------------------------------------
      enddo ! do JP = 1, ndet ==================================================

      avacfl = avacfl / float (NS) ! mean flow added per kilometre
      dcfcl = avacfl * distp !       mean flow added
      call sort format 3 (dcfcl,avacfl,distp)
      if ( nobigout .le. 0 ) then ! ============================================
      if ( KSIM .eq. 1 .and. avacfl .gt. 0.0001 ) then ! ------------------------
      !if ( ical .gt. 3 .or. ical .eq. 2 .or. ical .eq. 1 ) then
      !call sort format 3 (dcfcl,avacfl,distp)
      write(09,4522)valchars12,valchars11,funit,valchars10,funit
      write(01,4522)valchars12,valchars11,funit,valchars10,funit
      write(33,4522)valchars12,valchars11,funit,valchars10,funit
 4522 format(110('-')/
     &'Extrapolation of river flow downstream of flow gauge for  ',
     &8x,a10,' km ... (gap filling)'/
     &'Average flow added by gap filling at ',
     &a10,1x,a4,'/km leads to ',a10,1x,a4/110('+'))
      !endif
      endif ! if ( KSIM .eq. 1 .and. avacfl .gt. 0.0001 ) ----------------------
      
      !if ( ical .eq. 3 .or. ical .eq. 4 ) then ! ------------------------------
      do jdet = 1, ndet ! ------------------------------------------------------
      if ( qtype(jdet) .ne. 4 ) then ! -----------------------------------------
      if ( ical.eq. 3 .and. KSIM .eq. 1 ) then ! -------------------------------
      write(170+jdet,4523)valchars11, ! --------------------------------- Ci.GAP
     &funit,valchars12,valchars10,funit ! ------------------------------- Ci.GAP    
 4523 format(33x, ! ----------------------------------------------------- Ci.GAP
     &'DOWNSTREAM effects of gap filling on the annual mean ', ! -------- Ci.GAP     
     &'river flow ...'/ ! ----------------------------------------------- Ci.GAP
     &33x,'(Downstream of the gauge that provided the target for ', ! --- Ci.GAP
     &'flow ...)'/33x,! ------------------------------------------------- Ci.GAP
     &77('-')/33x,'Added 'a10,1x,a4,'/km over ',a10,' km ........ ', ! -- Ci.GAP
     &'an added ',a10,1x,a4/33x,77('+')) ! ------------------------------ Ci.GAP
      endif ! if ( ical.eq. 3 .and. KSIM .lt.4 ) -------------------------------
      endif ! if ( qtype .ne. 4 ) ----------------------------------------------
      enddo ! do jdet = 1, ndet ------------------------------------------------
      !endif ! if ( ical .eq. 3 .or. ical .eq. 4 ) -----------------------------
      endif ! if ( nobigout .le. 0 ) ===========================================

      if ( MONF .gt. 1 ) call write shots for river flow ! gap filling

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! gap filling ---------
      call get summaries of loads ! gap filling

 2805 return
      end

      
      
*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File Name: GAP FILL USE.FOR ... 1415 lines (255 comments)  
*     ==========================================================================
*     The suite calculates the quality between Features IFEAT1 and IFEAT2 ...
*     The data for upstream of IFEAT1 are picked up from [RELOAD] ...
*     The results from [gap fill simulation] are passed back to 
*     [perform gap filling for quality]. Here they are compared with the 
*     observed data ... and factors are adjusted ...
*     --------------------------------------------------------------------------
*     This file includes 16 SUB-ROUTINES, including:
*     --------------------------------------------------------------------------
*          ...... gap fill simulation =================================== 1
*          Calculate the effects of attempts at gap filling ...
*          ---------------------------------------------------------------------
*          ...... DUMP ================================================== 2
*          Store the shots for gap filling points ... These are picked up for 
*          the iterative cycle within ROUTINE [gap fill simulation] ...
*          ---------------------------------------------------------------------
*          ...... RELOAD ================================================ 3
*          Pick up the flow and quality data stored by ROUTINE [DUMP] ...
*          Pass the data back to ROUTINE [gap fill simulation] ...
*          ---------------------------------------------------------------------
*          ...... quality fit =========================================== 4
*          Calculates QUALITY values for gap filling ...
*          Generates values from a monitored quality distribution ...
*          Sorts generated values into ascending order of magnitude ...
*          Calculates ranked order of current simulated values '''
*          Pairs generated values with simulated values of the same rank ...
*          ---------------------------------------------------------------------
*          ...... CALFIT ================================================ 5
*          Take a set of shots from ROUTINE quality fit ...
*          Adjust them to have a set mean and 95-percentile ...
*          ---------------------------------------------------------------------
*          ...... insert quality gap filling ============================ 6
*          Add in the calculated effects of gap filling for river quality ------
*          ---------------------------------------------------------------------
*          ...... extrapolate quality gap filling ======================== 7
*          Extrapolate the changes calculated by gap filling as calculated by 
*          gap filling within ROUTINE [perform gap filling for quality] ...
*          Apply the effects downstrream of the gap filling point ...
*          ---------------------------------------------------------------------
*          ...... STATLOAD ============================================== 8
*          Calculate summary statistics for quality ...
*          =====================================================================

      subroutine gap fill simulation (JU) ! 333333333333333333333333333333333333
      include 'COMMON DATA.FOR'
      common /cal/ KOUNT(MP10,MS)
      dimension JSTOPn(MP10)
     
      do idet = 1, ndet
      JSTOPn(idet) = 0  
      enddo

      IFEAT = IFEAT1 ! starting feature 3333333333333333333333333333333333333333
      IFEAT2 = JU ! finishing feature 333333333333333333333333333333333333333333
      
*     check whether the calculations start at the head of the reach ------------
*     this is controlled by the value of ICTOP ---------------------------------
*     ICTOP is set in [REACH] just before [DUMP] is called ---------------------
*     if ICTOP equals 1. This is the head of the Reach -------------------------
      JCTOP = ICTOP

      JZERO = 0
      if ( ICTOP .eq. 1 ) then
      DCHECK = DIST(IFEAT2)
      if ( DCHECK .lt. 1.0E-06 ) JZERO = 1
      endif

*     fetch data for starting point --------------------------------------------
*     (start of the Reach or the last gap filling Point) -----------------------
      call reload

      if ( JCTOP .eq. 0 ) then
      DISTART = DIST(IFEAT)
      else
      DISTART = 0.0
      endif

      increase = 1
      if ( IFEAT1 .eq. IFEAT 2 ) increase = 0 ! 3333333333333333333333333333333333333
      IFEAT = IFEAT - increase 

      if ( JT(IFEAT1) .eq. 10 .or. JT(IFEAT1) .eq. 20 .or.
     &     JT(IFEAT1) .eq. 21 .or. JT(IFEAT1) .eq. 22 .or.
     &     JT(IFEAT1) .eq. 23 .or. JT(IFEAT1) .eq. 45) 
     &IFEAT = IFEAT + 1

*     update counter for the next feature --------------------------------------
   90 IFEAT = IFEAT + increase
      if ( IFEAT .gt. MU ) return
 
*     distance from last Feature -----------------------------------------------
      DISTP = DIST(IFEAT) - DISTART
      if ( DISTP .gt. 9999999.0 ) write( *,*)'Ugh ...6'
      
      DISTART = DIST(IFEAT)

*     diffuse sources and river chemistry --------------------------------------
*     if ( JT(IFEAT) .ne. 10 ) then
      KFEAT = IFEAT

      call add diffuse sources and natural purification (1)

*     add in the inflows from flow gap filling ---------------------------------
      if ( DISTP .gt. 1.0E-08 ) then
      call change colour of text (10) ! green
      call add the gap filled river flows (IFEAT,0)
      call set screen text colour
      endif 

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     calculate quality between Features IFEAT1 and IFEAT2 from latest estimates
*     from quality gap filling -------------------------------------------------
      if ( JZERO .eq. 1 ) DISTP = 1.0
      JZERO = 0
      if ( DISTP .gt. 1.0E-08 ) then !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      do jdet = 1, ndet ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ( QTYPE (JDET) .ne. 4 .and. ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     &    PDRC (IQMON,JDET) .ne. 6 .and. PDRC (IQMON,JDET) .ne. 7 .and.
     &    PDRC (IQMON,JDET) .ne. 9 .and. PDRC (IQMON,JDET) .ne. 4 .and. 
     &    PDRC (IQMON,JDET) .ne. 5 .and. PDRC (IQMON,JDET) .ne. 8 ) then

      do 1008 IS = 1, NS ! loop on shots (GAP FILL SIMULATION) 3333333
      if ( FMS(IS) .lt. 1.0E-08) goto 1098

*     extract the constants governing the changes to quality -------------------
      CONC = CMX(JDET,IS)
      
      if (CONC .lt. 1.0E-13 .and. CONC .gt. -1.0E-13) then
      KOUNT(JDET,IS) = -777
      goto 1008 ! gap filling is not needed for this shot ----------------------
      endif

      goto (1018,1019,1018,1008,1018,1008),QTYPE(JDET) !  GAP FILL SIMULATION 33

*     losses of type 2 determinand - eg. BOD and Ammonia +++++++++++++++++++++++
 1019 if ( CMX(JDET,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains -------------
      if ( CMS(JDET,IS) .gt. 1.0e-10 ) then
      CMS(JDET,IS)=CMS(JDET,IS)*EXP(CMX(JDET,IS)*DISTP) ! exponential decay ----
      else
      CMS(JDET,IS) = 0.0 ! set to zero -----------------------------------------
      endif
      goto 1008 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     losses of type 2 determinand - eg chloride -------------------------------
 1018 if ( CMX(JDET,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains -------------
      
      old CMS = CMS (JDET,IS) ! store the old concentration --------------------
      temp = DISTP * CMX(JDET,IS) / FMS(IS) ! calculate reduced concentration --
      CMS(JDET,IS) = old CMS + temp ! reduce the concentration -----------------
      
*     ==========================================================================
*     check for a negative concentration =======================================
      if ( CMS(JDET,IS) .gt. 1.0E-12 ) goto 1098
      fault = CMS(JDET,IS)
      CMS(JDET,IS) = AMAX1(1.0e-10,CMS(JDET,IS)) ! adjust this -----------------
      if ( JSTOPn(JDET) .lt. 1 .and. KSIM .eq. 1) then
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      jstopn(JDET) = jstopn(JDET) + 1 
      if ( KSIM .eq. 1 ) then
      call change colour of text (20) ! bright red
      write( *,9783)DNAME(JDET)!,fault,is
 9783 format(5x,'Negative concentrations needed for Gap Filling ',
     &'for ',A11)!,f11.3,i5)
      call set screen text colour
      endif
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------
      write(33,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
      write(170+JDET,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
      write(01,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
      write(09,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
 9413 format(/110('+')/A40/110('+')/
     &'Negative or sub-minimum concentration(s) would be needed for a ',
     &'complete fit for ',A11/
*    &'You could review your data. ',
*    &'And check the fit in the .CAL file. It may be good enough.'/
     &110('-')/'First shot affected =',4x,i8,
     &'  Computed quality =',F12.4,
     &'     ... The value was reset to  ',F12.4)
      write(33,9573)
      write(170+jdet,9573)
      write(01,9573)
      write(09,9573)
 9573 format(110('+')/
     &'This indicates that Gap Filling is being asked to ',
     &'explain too big a difference between the observed and '/
     &'calculated results. '/110('+'))
*    &'Perhaps a discharge quality is too poor for the observed',
*    &' river quality downstream ...'/
      endif ! if ( JSTOPn(JDET) .eq. 0 )
*     ================================ end of check for a negative concentration
      goto 1008 

*     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 1017 continue ! gains of pollutant for type 1 or type 2 determinands +++++++++++
      old CMS = CMS (JDET,IS)
      CMS(JDET,IS) = CMS(JDET,IS) + DISTP * CMX(JDET,IS) / FMS(IS)
      
 1098 continue ! jump to here if flow of quality is zero -----------------------
 1008 continue ! end of the loop covering NS shots for this determinand ========

      endif ! if ( QTYPE (JDET) .ne. 4 etc ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      enddo ! do jdet = 1, ndet ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if ( DISTP .gt. 1.0E-08 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      KFEAT = IFEAT

*     suppress unnecessary output and calculations -----------------------------
      JSKIP = 1
      feeture = IFEAT

      if ( JT(IFEAT) .ne. 10 .and. JT(IFEAT) .ne. 1 ) then
      call details upstream of feature ! gap fill simulation -------------------
      call process the feature (-JT(IFEAT)) ! gap-filling
      endif

      if ( IFEAT .eq. IFEAT2 ) goto 99 ! 333333333333333333333333333333333333333
   11 JCTOP=0
      
      call get summaries of river quality from the shots ! gap filling sim -----

      goto 90
   99 continue
      
      call get summaries of river quality from the shots
      
      return
      end



*     store the shots for gap filling points -----------------------------------
*     these are picked up for the iterative cycle within [GAP FILL SIMULATION] -
      subroutine dump (iopt)
      include 'COMMON DATA.FOR'

      if ( iopt .eq. 0 ) IFEAT1 = feeture ! 333333333333333333333333333333333333
      if ( iopt .eq. 1 ) IFEAT1 = IFEAT2 ! 3333333333333333333333333333333333333

      do 10 IS = 1, NS
      FDUMP(IS)=FMS(IS)
      DO 10 IDET = 1, ndet
      CDUMP(IDET,IS) = CMS(IDET,IS)
   10 continue

      KRFPOL13x = KRFPOL13
      KRFPOL25x = KRFPOL25
      KRFPOL27x = KRFPOL27
      KRFPOL29x = KRFPOL29
      KRFPOL31x = KRFPOL31
      KRFPOL33x = KRFPOL33
      KRFPOL35x = KRFPOL35
      KRFPOL46x = KRFPOL46
      KRFPOL48x = KRFPOL48
      KRFPOL37x = KRFPOL37
      KRFPOL40x = KRFPOL40

      KRAPOL13x = KRAPOL13
      KRAPOL25x = KRAPOL25
      KRAPOL27x = KRAPOL27
      KRAPOL29x = KRAPOL29
      KRAPOL31x = KRAPOL31
      KRAPOL33x = KRAPOL33
      KRAPOL35x = KRAPOL35
      KRAPOL46x = KRAPOL46
      KRAPOL48x = KRAPOL48
      
      KRAPOL50x = KRAPOL50
      KRAPOL52x = KRAPOL52
      KRAPOL54x = KRAPOL54
      KRAPOL56x = KRAPOL56
      KRAPOL58x = KRAPOL58

      KRAPOL37x = KRAPOL37
      KRAPOL40x = KRAPOL40

      KEPOL15x = KEPOL15
      KEPOL42x = KEPOL42

      KRQPOL13x = KRQPOL13
      KRQPOL25x = KRQPOL25
      KRQPOL27x = KRQPOL27
      KRQPOL29x = KRQPOL29
      KRQPOL31x = KRQPOL31
      KRQPOL33x = KRQPOL33
      KRQPOL35x = KRQPOL35
      KRQPOL46x = KRQPOL46
      KRQPOL48x = KRQPOL48
      KRQPOL50x = KRQPOL50
      KRQPOL52x = KRQPOL52
      KRQPOL54x = KRQPOL54
      KRQPOL56x = KRQPOL56
      KRQPOL58x = KRQPOL58
      KRQPOL37x = KRQPOL37
      KRQPOL40x = KRQPOL40

      return
      end



*     pick up the flow and quality data stored in DUMP -------------------------
*     pass the data back to [GAP FILL SIMULATION] 333333333333333333333333333333
      subroutine reload
      include 'COMMON DATA.FOR'

      DO 10 IS = 1, NS
      FMS(IS) = FDUMP(IS)
      do IDET = 1, ndet
      CMS(IDET,IS) = CDUMP(IDET,IS)
      enddo
   10 continue

      KRFPOL13 = KRFPOL13x
      KRFPOL25 = KRFPOL25x
      KRFPOL27 = KRFPOL27x
      KRFPOL29 = KRFPOL29x
      KRFPOL31 = KRFPOL31x
      KRFPOL33 = KRFPOL33x
      KRFPOL35 = KRFPOL35x
      KRFPOL46 = KRFPOL46x
      KRFPOL48 = KRFPOL48x
      KRFPOL50 = KRFPOL50x
      KRFPOL52 = KRFPOL52x
      KRFPOL54 = KRFPOL54x
      KRFPOL56 = KRFPOL56x
      KRFPOL58 = KRFPOL58x
      KRFPOL37 = KRFPOL37x
      KRFPOL40 = KRFPOL40x

      KRAPOL13 = KRAPOL13x
      KRAPOL25 = KRAPOL25x
      KRAPOL27 = KRAPOL27x
      KRAPOL29 = KRAPOL29x
      KRAPOL31 = KRAPOL31x
      KRAPOL33 = KRAPOL33x
      KRAPOL35 = KRAPOL35x
      KRAPOL46 = KRAPOL46x
      KRAPOL48 = KRAPOL48x
      
      KRAPOL50 = KRAPOL50x
      KRAPOL52 = KRAPOL52x
      KRAPOL54 = KRAPOL54x
      KRAPOL56 = KRAPOL56x
      KRAPOL58 = KRAPOL58x
      
      KRAPOL37 = KRAPOL37x
      KRAPOL40 = KRAPOL40x

      KEPOL15 = KEPOL15x
      KEPOL42 = KEPOL42x

      KRQPOL13 = KRQPOL13x
      KRQPOL25 = KRQPOL25x
      KRQPOL27 = KRQPOL27x
      KRQPOL29 = KRQPOL29x
      KRQPOL31 = KRQPOL31x
      KRQPOL33 = KRQPOL33x
      KRQPOL35 = KRQPOL35x
      KRQPOL46 = KRQPOL46x
      KRQPOL48 = KRQPOL48x
      KRQPOL37 = KRQPOL37x
      KRQPOL40 = KRQPOL40x

      call get summaries of river quality from the shots
      return
      end


      subroutine quality fit (JDET,DM,DS,DP,CLS,CMT)
*     --------------------------------------------------------------------------
*     calculates QUALITY values for gap filling --------------------------------
*     --------------------------------------------------------------------------
*       JDET  - determinand number
*         DM  - mean required river quality
*         DS  - standard deviation of required river quality
*        CLS  - pre-calibrated simulated value
*        CMT  - ranked target values
*      ISVAL  - position of simulated values in ranked order
*     --------------------------------------------------------------------------
*     Generates values from monitored quality distribution ---------------------
*     Sorts generated values into ascending order of magnitude -----------------
*     Calculates ranked order of current simulated values ----------------------
*     Pairs generated values with simulated values of the same rank ------------

      include 'COMMON DATA.FOR'
      dimension CLS(MP10,MS),WORK1(MS),WORK2(MS),WORK3(MS),
     &ISVAL1(MS),ISVAL2(MS)
      double precision CMT(MP10,MS)

*     generate the values for the target river quality -------------------------
*     these shots will be placed in CMS ----------------------------------------
      JP=JDET
      call generate river quality ! target river quality

*     put simulated and generated values into the dummy arrays -----------------
      do IS = 1, NS
      WORK1(IS) = CLS(JP,IS) ! the values calculated by SIMCAT -----------------
      WORK2(IS) = CMS(JP,IS) ! the generated values for the target quality -----
      enddo

      call rank (WORK2,ISVAL2,NS) ! sort and rank the generated target values --
      call rank (WORK1,ISVAL1,NS) ! sort and rank SIMCAT's calculated values ---

*     pair up generated quality with simulated quality of same rank ------------
*     this means overwriting the current values in CMT -------------------------
*     WORK2 - ranked generated values
*     ISVAL - gives the position of ranked simulated value in CLS array
      do IS = 1, NS
      CMT(JP,ISVAL1(IS)) = CMS(JP,ISVAL2(IS))
      enddo

      do IS = 1, NS
      WORK3(IS) = CMT(JP,IS)
      enddo

*     adjust the shots in CMT so that they have a given mean and 95-percentile -
      call calfit (JP,DM,DS,DP,CMT)

      if ( JP .eq. 5556 ) then
      write(33,1205)
 1205 format(100('-')/
     &'Rank   Current    IS    Ranked',
     &'       CMS    IS    Ranked       CMT       CMT'/
     &'  --    SIMCAT    --   current',
     &'    target    --    target    target    update'/100('-'))
      do is = 1,NS
      IX = ISVAL2(IS)
      write(33,1255)is,CLS(JP,IS),ISVAL1(IS),WORK1(IS),
     &CMS(JP,IS),ISVAL2(IS),WORK2(IS),WORK3(IS),CMT(JP,IS),CMX(JP,IS)
 1255 format(i4,f10.4,i6,f10.4,f10.4,i6,f10.4,f10.4,3f10.4)
      enddo
      write(33,1205)
      endif

      return
      end




*     Take a set of shots from ROUTINE quality fit ...
*     and adjust them to have a set mean and 95-percentile ...
      subroutine calfit (JDET,DM,DS,DP,CMT)
      include 'COMMON DATA.FOR'
      dimension Y(MS),WORK(MS),IRANK(MS)
      double precision CMT(MP10,MS)

*     check for zero mean quality ----------------------------------------------
      if (DM .lt. 1.0E-10) then
      do IS = 1, NS
      CMT(JDET,IS) = DM
      enddo
      return
      endif

*     compute the ratio of percentile to mean ----------------------------------
      DT = DP / DM

*     check for a uniform constant quality -------------------------------------
      if ( PDRC (IQMON,JP) .eq. 1 .or. PDRC (IQMON,JP) .eq. 2 .or. 
     &     PDRC (IQMON,JP) .eq. 3 ) then
      if ( qtype (JDET) .ne. 3 ) then
      if (DT .gt. 1.002) goto 11
      else
      if (DT .lt. 0.998) goto 11
      endif
*     the river quality is uniform ---------------------------------------------
      DM = AMAX1 (0.0 , DM )
      do IS = 1, NS
      CMT(JDET,IS) = DM
      enddo
      if ( nobigout .le. 0 ) write(01,8099)
 8099 format(77('-'))
      return
      endif

*     quality is non-zero and variable -----------------------------------------
   11 continue
      DT = DS

*     load the array of target shots into the array Y --------------------------
      do IS = 1, NS
      Y(IS) = CMT(JDET,IS)
      enddo

*     compute the summary statistics. This will re-order part of Y -------------
      call calculate summaries of river quality (0,JDET,Y)
*     scaling factor for making current mean equal the required mean -----------
      DMM = DM / C(JDET,1)
*     scale the generated shots to give the required mean ----------------------
      do IS = 1, NS
      CMT(JDET,IS) = CMT(JDET,IS) * DMM
*     store the new shots in Y -------------------------------------------------
      Y(IS) = CMT(JDET,IS)
      enddo

      ITRIM=0

*     calculate the new summary statistics -------------------------------------
 2000 continue
      call calculate summaries of river quality (0,JDET,Y)

*     re-load Y ----------------------------------------------------------------
      do IS = 1, NS
      Y(IS) = CMT(JDET,IS)
      enddo

*     scale the quality shots to give the required 95-percentile ---------------
*     first compute the scaling factor -----------------------------------------
      DIV = AMAX1(1.0E-9,C(JDET,3))
*     ratio of required percentile to actual -----------------------------------
      DPP = DP/DIV
*     now do the 5-percentiles for Type 3 substances ---------------------------
*     difference between required percentile to actual -------------------------
      if ( QTYPE (JDET) .eq. 3 ) DPP = DP - C(JDET,3)
      if ( QTYPE (JDET) .eq. 5 ) DPP = DP - C(JDET,3)

*     sort the target shots into ascending order -------------------------------
      do IS = 1, NS
      WORK(IS) = Y(IS)
      enddo

      call rank (WORK,IRANK,NS)

*     this variable will accumulate the total adjustments to shots -------------
      TXTRA=0.0

*     separate Dissolved Oxygen from the rest ----------------------------------
      if ( QTYPE (JDET) .ne. 3 .and. QTYPE (JDET) .ne. 5 ) then

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     the rest of the determinands ---------------------------------------------
      do is = NS-k95+1, NS
*     adjust the bad quality shots (the high values) ---------------------------
      XTRA = ( DPP - 1.0 ) * WORK(IS)
      WORK(IS) = WORK(IS) + XTRA
*     accumulate the total correction in TXTRA ---------------------------------
      TXTRA = TXTRA + XTRA
      enddo
*     spread these corrections over all the rest of the shots ------------------
*     compute the adjustment required to each shot -----------------------------
*     SIMCAT will try to change each shot by the same absolute quantity --------
      XTRA = TXTRA / float ( NS - k95 )

*     loop on the best 95 per cent of the shots --------------------------------
      do IS = 1, NS - k95

*     compute the value of the new shot ----------------------------------------
      XNEW = WORK(IS) - XTRA

*     check that this does not give a negative result --------------------------
      if ( XNEW .gt. 0.0 ) goto 9849

*     the result would be negative ---------------------------------------------
*     try reducing the value of the shot by 70% --------------------------------
*     first compute 70% of the value of the shot -------------------------------
      XIMP = 0.7 * WORK(IS)

*     this failure to remove XTRA from this shot will mean that ----------------
*     the changes to the remaining shots must be bigger ------------------------
*     ... bigger by an amount XIMP per shot ------------------------------------
      XIMP = XIMP / float (NS - k95 - IS )

*     add this to XTRA ---------------------------------------------------------
      XTRA = XTRA + XIMP

*     assign new value to shot (70% reduction) ---------------------------------

      XNEW = WORK(IS) - XIMP
 9849 WORK(IS) = XNEW
      enddo

*     check the percentile has not been altered so much as to change -----------
*     the ranked sequence ------------------------------------------------------

      if ( work(ns-k95+1) .lt. work(ns-k95) ) then
      do IS = 1, NS
      KS = IRANK(IS)
      CMT(JDET,KS) = WORK(IS)
      Y(KS) = WORK(IS)
      enddo
      ITRIM = ITRIM + 1
      if ( ITRIM .lt. 10 ) goto 2000
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      else

*     **************************************************************************
*     now do substances where low values are bad (like dissolved oxygen) -------
      do IS = 1, k95
*     adjust the bad quality shots ---------------------------------------------
*     DPP is the difference between required percentile to actual --------------
      XTRA = DPP
      WORK(IS) = Amax1 ( 0.0001, WORK(IS) + XTRA )
*     accumulate the total correction to be applied to the remaining shots -----
      TXTRA = TXTRA + XTRA
      enddo

*     spread these corrections over the rest of the shots ----------------------
      XTRA = TXTRA / float( NS - k95 )
      do IS = k95 + 1, NS
      XNEW = WORK(IS) - XTRA
      if ( XNEW .gt. 0.0 ) goto 4849
      XIMP = 0.7 * ( WORK(IS) - 0.0 )
      XIMP = XIMP / float( NS - k95 - IS )
      XTRA = XTRA     + XIMP
      XNEW = WORK(IS) - XIMP
 4849 WORK(IS) = Amax1 ( 0.0001, XNEW )
      enddo

*     check the percentile has not been altered so much as to change -----------
*     the ranked sequence ------------------------------------------------------
      if (work(k95+1).lt.work(k95)) then
      do IS = 1, NS
      KS = IRANK(IS)
      CMT(JDET,KS) = WORK(IS)
      Y(KS) = WORK(IS)
      enddo
      ITRIM = ITRIM + 1
      if ( ITRIM .lt. 10 ) goto 2000
      endif
*     **************************************************************************
      endif

*     put quality shots back into the original sequence ------------------------
      do IS = 1, NS
      KS = IRANK(IS)
      CMT(JDET,KS) = WORK(IS)
      enddo

*     compute the summary statistics -------------------------------------------
      call calculate summaries of river quality (0,JDET,WORK)

      return
      end



*     add in the calculated effects of gap filling for river quality -----------
      subroutine insert quality gap filling (JU)
      include 'COMMON DATA.FOR'
      character *136 RECORD
      character *044 FEATNAM
      character *11 DDNAM,char11
      character *1 char1,char2,char3
      character *4 char4
      dimension avacqu(2,MP10),nsacqu(2,MP10),extra load(NS)
      dimension JSTOPn(MP10)

      do idet = 1, ndet
      JSTOPn(idet) = 0  
      enddo 

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     special procedure for section at end of Reach downstream of the last -----
*     feature on the Reach ... Check for the end of the Reach ------------------
      if ( IEND .ne. 0) then ! the reach has ended -----------------------------
*     add in the effects of gap filling ----------------------------------------
      DQ = distp
      call extrapolate quality gap filling (JU,DQ) ! to the end of the reach 444
      goto 9000 ! return
      endif ! if ( IEND .ne. 0) ------------------------------------------------
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     the reach has not ended --------------------------------------------------
      if ( KQCAL(JU) .eq. 1 ) then ! the last gap filling point has been reached
      DQ = distp
      call extrapolate quality gap filling (JU,DQ) ! 444444444444444444444444444
      goto 9000 ! return
      endif


*     read the record for gap filling of river quality from Channel 75 ---------
      rewind 75
 1000 continue
      read(75,1001,end=9999,ERR=9998)RECORD
 1001 format(A136)

*     pick out the first, fourth and eighth characters in the records ----------
      read(record,5300) char1,char2,char3
 5300 format (a1,2x,a1,3x,a1 )
      if ( char3 .eq. '*' ) goto 1000
      if ( char1 .eq. 'Q' ) goto 1000
      if ( char2 .eq. '.' ) goto 1000

*     a descriptive heading has been found -------------------------------------
*     DQ is the distance from the last Feature ---------------------------------
      backspace 75
      read(75,*)LREACH,DQ
      if (LREACH .ne. IREACH) goto 1000

*     read name of Feature -----------------------------------------------------
      backspace 75
      read(75,1005)FEATNAM
 1005 format(46X,A44)
      if ( FEATNAM .ne. UNAME(JU) ) goto 1000

*     the data the for current feature have been found -------------------------
*     is this Feature a gap filling point ? ------------------------------------
      read(75,1001,end=9999,ERR=9998)RECORD
      read(RECORD,5300) char1
      if ( char1 .eq. '*') goto 2000 ! not a station #####

*     yes...it is a prime gap filling point ------------------------------------
      read(record,5301)char4
 5301 format (a4)
      if ( char4 .eq. 'QUAL' ) goto 4004
      write(01,1011)
      write(33,1011)
      write( *,1011)
      write(09,1011)
 1011 format(//'*** ERROR IN ASSEMBLING THE GAP FILLING FILE....'//
     &         '*** PRIME GAP FILLING POINT HAS NO DATA ON ',
     &         'QUALITY.....')
      call stop

*     YES.....it is a gap filling Point for river quality ----------------------
 4004 continue
      
      if ( KINT .ne. 0 ) DQ = DINT

*     if the distance, DQ, is zero it must be changed to 1.0 -------------------
*     this forces in the adjustments -------------------------------------------
      if ( JQUSER(JU) .eq. 0 .and. DINT .lt. 1.0E-6 .and.
     &    DQ .lt. 1.0E-10 ) then
      DQ = 0.0
      endif

      JQUSER(JU) = 1

*     if this is a Secondary Point we must not add corrections where the -------
*     distance, DQ, is zero ----------------------------------------------------
      if ( DQ .lt. 1.0E-10 ) goto 9000 ! return

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loop on determinands -----------------------------------------------------

      limit printing of messages = 0
      do 1575 JP = 1, ndet ! ---------------------------------------------------
      if ( qtype(JP) .eq. 4 ) goto 1575 ! --------------------------------------
      DDNAM=DNAME(JP)
      read(record,5303) char11,NEXC(JP)
 5303 format(13x,a11,96x,i2)
      if ( char11 .eq. DDNAM ) goto 3301
      write(01,3801)DNAME(JP),char11,FEATNAM
      write(170+JP,3801)DNAME(JP),char11,FEATNAM
      write( *,3801)DNAME(JP),char11,FEATNAM
      write(09,3801)DNAME(JP),char11,FEATNAM
      write(33,3801)DNAME(JP),char11,FEATNAM
 3801 format(/77('-')/
     &'*** No gap filling data for ',A11,1x,a11/
     &'*** For the Feature named ',A40/
     &'*** Calculation stopped ...'/77('-'))
      call stop

*     read the adjustments to river quality ------------------------------------
 3301 continue
      read(75,8448,err=4499)(CMX(JP,IS),IS=1,NS) ! read adjustments to quality
 8448 format(10(1PE18.10))
      if ( QTYPE (JP) .eq. 4 ) goto 1576

      do IS = 1, NS
      extra load (IS) = 0.0  
      enddo

*     apply the quality adjustments to each shot -------------------------------
*     also, work out the average effect of gap filling -------------------------
      avacqu (1,JP) = 0.0 ! gains in concentration per determinand -------------
      avacqu (2,JP) = 0.0 ! losses in concentration per determinand ------------
      nsacqu (1,JP) = 0 ! number of shots giving a gain ------------------------
      nsacqu (2,JP) = 0 ! number of shots giving a loss ------------------------
      
      
      
*     ££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££      
*     ££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££
      do 1008 IS = 1, NS ! loop on number of shots (extrapolate gap filling) £££
*     compare the flow with the 99-percentile ----------------------------------
      !if ( FMS(IS) .lt. flow (4) + 1.0E-12 ) goto 1008 ! compare with 99%-tile -

*     check for xero effect in load --------------------------------------------
      if ( CMX(JP,IS) .gt. -1.0E-12 .and.
     &     CMX(JP,IS) .lt.  1.0E-12 ) goto 1008 ! load is zero -----------------

      goto (1018,1019,1018,1008,1018,1008),QTYPE(JP) ! - extrapolate gap filling 

*     type 2 determinand - eg. BOD and Ammonia ===================== extrapolate
 1019 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! check for gain in load ========
*     load has been lost for this shot -----------------------------------------
      if ( CMS(JP,IS) .gt. 1.0e-10 ) then ! concentration exceeds zero ~~~~~~~~~
      temp = CMS(JP,IS)*EXP( CMX(JP,IS) * DQ ) ! new concentration ------------- 
      old = temp - CMS(JP,IS) ! change in concentration ------------------------
      old CMS = CMS(JP,IS) ! starting concentration ----------------------------
      CMS(JP,IS) = temp ! update the concentration -----------------------------
      do ip = 1, n2prop ! look on all contributions of concentrations ==========
*     concentrations from various types of feature etc -------------------------
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! reduction -------
      enddo ! do ip = 1, n2prop ================================================
      else ! if the concentration is negative ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CMS(JP,IS) = 0.0 ! set it to zero ----------------------------------------
      endif ! if ( CMS(JP,IS) .gt. 1.0e-10 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      avacqu (2,JP) = avacqu (2,JP) + old ! ########################### NOT USED
      nsacqu (2,JP) = nsacqu (2,JP) + 1 ! ############################# NOT USED
      goto 1088 ! calculate effects on discharges ------------------------------
*     losses of determinand ======================================== extrapolate     
      
*     type 1 determinand - eg. chloride etc ======================== extrapolate
 1018 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! and check for gains at 1017 ---
*     losses of determinand of type 1 - conservative ==================== losses
      old CMS = CMS(JP,IS) ! store the old concentation shot -------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! this will be a negative value ---------
      CMS(JP,IS) = AMAX1 (0.0, old CMS + temp ) ! reduce the concentration -----
      
      if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced ------- losses
      do ip = 1, n2prop ! new concentration is less than the old one ---- losses
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! scale CTMS ------
      enddo ! do ip = 1, n2prop ... new concentration is less than the old one -
      endif ! if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced ------
      temp = old CMS - CMS(JP,IS)
      avacqu ( 2, JP ) = avacqu ( 2, JP ) + temp ! losses of concentration -----
      nsacqu ( 2, JP ) = nsacqu ( 2, JP ) + 1 ! number of shots with losses ----
      goto 1088 ! calculate effects on discharges ------------------------------
*     losses of determinand ================================= extrapolate losses

 1017 continue ! gains of pollutant -------------------------- extrapolate gains
*     calculate the gain in concentration when the load CMX is added to the ----
*     river flow. FMS ----------------------------------------------------------
      extra load (IS) = DQ * CMX(JP,IS) ! compute the total load over distance -
      accum load (IS) = accum load (IS) + extra load (IS) ! accumulate ---------      
      oldCMS = CMS(JP,IS) ! old concentration ----------------------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! extra concentration from gap filling --
      CMS(JP,IS) = CMS(JP,IS) + temp
      CTMS(22,JP,IS) = CTMS(22,JP,IS) + temp

      avacqu ( 1, JP ) = avacqu ( 1, JP ) + temp ! gains of concentration 
      nsacqu ( 1, JP ) = nsacqu ( 1, JP ) + 1 ! number of shots with gains
*     gains of determinand =================================== extrapolate gains

      
*     ##########################################################################      
 1088 continue ! trim the annual loads from individual works -------------------

 1008 continue ! end of the loop  NS shots for this determinand ======== 
*     £££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££      
*     £££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££      

      goto 3998 ! £££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££
      
      
      do 3008 IS = 1, NS ! loop on number of shots (INSERT QUALITY GAP FILLING)
      if (FMS(IS) .lt. 1.0E-08) goto 3008 ! if flow is zero
      
*     apply the gap filled adjustment to this shot -----------------------------
      CONC = CMX(JP,IS) ! store the gap fill load adjustment for this shot -----
*     skip zero adjustmgents ---------------------------------------------------
      if (CONC .lt. 1.0E-13 .and. CONC .gt. -1.0E-13) goto 3008 ! - this is zero

      goto (3018,3019,3018,3008,3018,3008),QTYPE(JP) ! - insert quality gap fill
 
 3018 continue ! for type 1 determinands (such as Chloride etc ------------------
      oldCMS = CMS(JP,IS) ! store the old concentration ------------------------
      CMS(JP,IS) = oldCMS + DQ * CMX(JP,IS) / FMS(IS) ! new concentration ------
      goto 3229
      
 3019 continue ! for type 2 determinands (BOD, Ammonia etc) --------------------
      if ( CMX(JP,IS) .gt. 1.0e-10) goto 3018 ! this is an increase in load ----
*     proceed with the reduction on load ---------------------------------------
      oldCMS = CMS(JP,IS) ! store the old concentration ------------------------
      CMS(JP,IS) = oldCMS * EXP(DQ * CMX(JP,IS)) ! new concentration -----------

 3229 continue 
      CMS(JP,IS) = amax1(0.0,CMS(JP,IS))
      CTMS(22,JP,IS) = CTMS(22,JP,IS) - oldCMS + CMS(JP,IS) ! add to CTMS ------
      CTMS(NTD,JP,IS) = CTMS(NTD,JP,IS) - oldCMS + CMS(JP,IS)     
      
*     check for a drop in concentration ========================================
      if ( CMS(JP,IS) .lt. old CMS ) then ! ====================================
      do ip = 1, n2prop ! the new concentration is less than the old one -------
      if ( ip .eq. NTD ) old CTMS30 = CTMS(ip,JP,IS)
      enddo ! do ip = 1, n2prop ------------------------------------------------
      
      oldCTMS17 = 0.0 
      oldCTMS22 = 0.0 

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     check whether the resulting concentration is negative ++++++++++++++++++++
      if ( CMS(JP,IS) .le. 1.0E-25 ) then ! it is negative +++++++++++++++++++++
      jstopn(JP) = jstopn(JP) + 1 
      fault = CMS(JP,IS)
      CMS(JP,IS) = AMAX1(1.0e-10,CMS(JP,IS))
      if ( iscreen .lt. 3 ) then ! ---------------------- negative concentration
      if ( JSTOPn(JP) .eq. 1 ) then ! ------------------- negative concentration
      call change colour of text (20) ! bright red
      write( *,9434)DNAME(JP)
 9434 format(
     &'* NEGATIVE (or sub-minimum) concentrations are needed for a',
     &' complete fit for ',A11)
      call set screen text colour
      endif ! if ( JSTOPn(JP) .eq. 1 ) ------------------ negative concentration
      endif ! if ( iscreen .lt. 3 ) --------------------- negative concentration
      if ( JSTOPn(JP) .eq. 1 ) then ! write a heading --- negative concentration
      write(33,9413)UNAME(JU),DNAME(JP),IS,fault,CMS(JP,IS)
 9413 format(/110('-')/A40/110('-')/
     &'Negative or sub-minimum concentration(s) would be needed for a ',
     &'complete fit for ',A11/ ! ------------------------ negative concentration
     &110('-')/'First shot affected =',4x,i8,
     &'  Computed quality =',F12.4,
     &'         The value was reset to  ',F12.4)
      write(33,9573) ! ---------------------------------- negative concentration
 9573 format(110('-')/
     &'This indicates that gap filling is being asked to ',
     &'explain too big a difference between the observed and '/
     &'calculated results. ',
     &'Perhaps a discharge quality is too poor for the observed',
     &' river quality downstream ...'/110('-'))
      endif ! if ( JSTOPn(JP) .eq. 1 ) ------------------ negative concentration
      do ip = 1, n2prop ! new concentration is less than the old one -----------
      if ( ip .eq. 17 ) oldCTMS17 = CTMS(ip,JP,IS)
      if ( ip .eq. 22 ) oldCTMS22 = CTMS(ip,JP,IS)
      CTMS(ip,JP,IS) = 0.0 ! concs from various types of feature ---------------
      enddo ! do ip = 1, n2prop ------------------------------------------------
      goto 3008 ! ------------------- having dealt with a negative concentration
      endif ! if ( CMS(JP,IS) .le. 1.0E-25 ) +++++++++ end of check for negative
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      goto 3008 ! have dealt with a drop in concentration ----------------------
      endif ! if ( CMS(JP,IS) .lt. old CMS ) ===================================

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
      if ( CMS(JP,IS) .ge. oldCMS ) then ! concentration has increased +++++++++
*     extra loads are added in linearly with river length ----------------------
*     this mechanism is applied to gains of load -------------------------------

*     calculate the gain in concentration when the load CMX is added to the ----
*     river flow ... FMS -------------------------------------------------------
      extra load (IS) = DQ * CMX(JP,IS) ! compute the total load over distance -
      accum load (IS) = accum load (IS) + extra load (IS) ! accumulate---------

      if ( FMS (IS) .lt. 1.0e-06 ) then ! check for zero flow FFFFFFFFFFFFFFFFFF
      if (limit printing of messages .eq. 0 ) then ! ---------------------------
      limit printing of messages = 1
      write(33,8633)
      if ( ifbatch .eq. 0 ) then
      write( *,8633) IS
 8633 format(77('-')/'Near zero flow encountered in gap filling ',
     &'for flow ...',I4/'Loads added by gap filling for quality ',
     &'have been set to zero ...'/77('-'))
      temp = 0.0
      endif
      endif ! if (limit printing of messages .eq. 0 ) --------------------------
      else ! if ( FMS (IS) .lt. 1.0e-06 ) flow is not zero FFFFFFFFFFFFFFFFFFFFF
      temp = extra load (IS) / FMS(IS) ! compute the added concentration 
      endif ! if ( FMS (IS) .lt. 1.0e-06 ) check the flow FFFFFFFFFFFFFFFFFFFFFF

      endif ! if ( CMS(JP,IS) .gt. 1.0E-25 ) positive conentration +++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      

 3008 continue ! end of the loop covering NS shots for this determinand ======== 
*     ==========================================================================
 3998 continue ! £££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££
      
      
*     write out results that may help with calibration ------------------------- 
      if ( ical .eq. 4 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
*     write(170+JP,3708)UNAME(KFEAT),dname(JP),distp
*3708 format(100('-')/'Feature: ',a37,' for ',a11,' over',f7.2,' km'/
*    &100('-'))
*     if ( JSTOPn(JP) .gt. 0 ) then
*     write(170+JDET,6692)JSTOPn(JP),NS
*6692 format('Number of shots affected = ',I4,' out of',i6/80('-'))
*     endif
      endif ! if ( ical .eq. 4 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling

      if ( JSTOPn(JP) .gt. 10 ) write(09,6192)Dname(JP),JSTOPn(JP),NS
      if ( JSTOPn(JP) .gt. 0 ) write(33,6192)Dname(JP),JSTOPn(JP),NS
 6192 format(110('-')/
     &'Negative or sub-minimum concentrations(s) would be needed',
     &' for a complete fit for ',A11/
     &'Number of shots affected = ',I4,' out of',i6/110('-'))

*     list the effects imposed by natural purification -------------------------
      do 6632 ii = 1 , 2
      if ( nsacqu (ii,JP) .gt. 0 ) then
      avacqu (ii,JP) = avacqu (ii,JP) / float(nsacqu(ii,JP))
      dava = avacqu (ii,JP) / DQ
      pern = 100.0 * float (nsacqu(ii,JP)) / float (NS)
      endif
      if ( dint .gt. 0.000001 ) then
      if ( nobigout .le. 0 ) then
      if ( ii .eq. 1 .and. nsacqu (ii,JP) .gt. 0 ) then


*     calculate the extra load =================================================
*     call statload (extra load,CC1,CC2,CC3,CC4)
*     write(170+JP,8522) CC1, CC3, CC4, CC2,distp,UNAME(KFEAT),
*    &IABS(JQCAL(KFEAT))
*8522 format('     Load gained by gap filling =',4f9.1,1x,f6.2,3x,
*    &a37,i5)
*     call statload (extra load,CC1,CC2,CC3,CC4)
*     CC1 = CC1/DQ
*     CC3 = CC3/DQ
*     CC4 = CC4/DQ
*     CC2 = CC2/DQ
*     write(170+JP,8823) CC1, CC3, CC4, CC2, dist(KFEAT),
*    &UNAME(KFEAT),RNAME(JREACH(KFEAT))!,DQ
*8823 format(4f10.2,f10.2,3x,a37,a16,f10.2,f10.2)

*     call statload (accum load,CC1,CC2,CC3,CC4)
*     CC1 = CC1/DQ
*     CC3 = CC3/DQ
*     CC4 = CC4/DQ
*     CC2 = CC2/DQ
*     write(170+JP,8523) CC1, CC3, CC4, CC2, dist(KFEAT),
*    &UNAME(KFEAT),RNAME(JREACH(KFEAT)),DQ
*8523 format(4f10.2,f10.2,3x,a37,a16,f10.2,f10.2)

      if ( IABS(JQCAL(KFEAT)) .gt. 0 ) then
      do IS = 1, NS
      accum load (IS) = 0.0 ! initalise accumulated load ------------------------
      enddo
      endif

*     write(170+JP,9522) avacqu(ii,JP),units(JP),pern
*9522 format('Mean quality gained by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)')
*     write(170+JP,4522) avacqu(ii,JP),units(JP),pern,dname(jp)
*4522 format('Average quality gain introduced by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)

      else
      if ( nsacqu (ii,JP) .gt. 0 ) then
      if ( kprint .eq. 0 ) then
      !write(170+JP,4692) 
 4692 format(
     &'Interpolation of river quality to downstream gap filling Point ',
     &47('-'))
      kprint = 1
      endif

*     write(170+JP,6552) -avacqu(ii,JP),units(JP),pern
*6552 format('Average quality loss introduced by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)')
      !write(170+JP,4552) -avacqu(ii,JP),units(JP),pern,dname(jp)
 4552 format('Average quality loss introduced by gap filling ',
     &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)

      endif
      endif
      endif
      endif
 6632 continue

 1576 read(75,1001,end=9999,ERR=9998)RECORD

 1575 continue ! end of loop on determinands +++++++++++++++++++++++++++++++++++

      goto 9000 ! return

*     NO.....IT IS NOT A PRIME gap filling POINT ...
*     WE NEED TO FIND THE PRIME POINT PROVIDING DATA FOR THIS POINT ...
*     IT WILL BE DOWNSTREAM OF THE CURRENT FEATURE ...
*     READ ON THROUGH THE GAP FILLING FILE ...
*     NEXT RECORD MUST CONTAIN DESCRIPTIVE DATA FOR NEXT FEATURE

 2000 continue

      if ( DQ .lt. 1.0E-10 ) goto 9000

 2006 continue
      read(75,1001,end=9996,ERR=9998)RECORD
      read(record,5300)char1,char2 ! read 1st and 4th characters
      if (char1 .ne. ' ' ) goto 2006
      if (char2 .ne. '.' ) goto 2006

*     IT'S ALL RIGHT...THE NEXT LINE CONTAINS DESCRIPTIVE DATA -----------------
 2001 Backspace 75
      Backspace 75
      Backspace 75
      read(75,*)LREACH
      Backspace 75
      read(75,1005)FEATNAM
      if (LREACH .ne. IREACH) goto 9966

*     is this Feature the Primary Point? ---------------------------------------
      read(75,1001,end=9997,ERR=9998)RECORD
      read(record,5300)char1
      if (char1 .eq. '*' ) goto 2006

*     yes it is ----------------------------------------------------------------
      read(record,5301) char4
      if ( char4 .ne. 'QUAL' ) goto 2006
      do 6214 KUSER=JU,MU
      if (featnam .eq. uname(KUSER) ) goto 6214
      JQUSER(KUSER)=1
      goto 6215
 6214 continue
 6215 continue
      goto 4004
 9000 continue ! return
      rewind 75
      return

 9990 write(01,8081)FEATNAM
      write(33,8081)FEATNAM
      write( *,8081)FEATNAM
      write(09,8081)FEATNAM
 8081 format(/
     &'*** Error in reading NEXQ in ROUTINE perform quality ',
     &'gap filling ... '/A40)
      call stop

 9998 if ( nobigout .le. 0 ) write(01,3900)
      write(33,3900)
      write(01,3900)
      write( *,3900)
      write(09,3900)
 3900 format(/'*** Error in gap filling data for river quality ...'/
     &'*** CALCULATION STOPPED ...')
      call stop

 9997 continue
      write(33,3901)UNAME(JU),FEATNAM
      write( *,3901)UNAME(JU),FEATNAM
      write(01,3901)UNAME(JU),FEATNAM
      write(09,3901)UNAME(JU),FEATNAM
      if ( nobigout .le. 0 ) write(01,3901)UNAME(JU),FEATNAM
 3901 format(//'*** NO QUALITY GAP FILLING POINT',
     &' FOR THIS FEATURE ...'/'*** CURRENT FEATURE ... ',A40/
     &'*** SEARCHED AS FAR AS ... ',A40/
     &'*** CALCULATION STOPPED ...')
      call stop

*     ==========================================================================
*     no data found for the application of gap filling =========================
 9999 continue
      return
      call change colour of text (34) ! dull yellow
      write( *,3993)UNAME(JU)
 3993 format(
     &'* No gap filling for quality',23x,'...',7x,'at ',A40,
     &' error in sequencing of features')
      call set screen text colour
      write(01,3903)UNAME(JU)
      write(09,3903)UNAME(JU)
      write(33,3903)UNAME(JU)
 3903 format(123('-')/
     &'No data on gap filling for river quality for ...',A40/
     &'The gap filling file is out of step with the file now being ',
     &'run ... '/
     &'Features have been added of removed since gap filling ...'/ 
     &'Gap filling has not been done at this point ...'/123('-'))
      rewind 75
      return
*     ==========================================================================

 9996 continue
      if ( nobigout .le. 0 ) write(01,3905)
      write(33,3905)
 3905 format('*** The next feature has no downstream point for ',
     &'gap filling for river quality ...')
      rewind 75
      return

 9966 continue
      write(33,3905)
 3925 format('*** The next feature has no Gap Filling point for ',
     &'river quality ...')
      rewind 75
      return


 4499 continue
      write( *,4498)
      write(01,4498)
      write(09,4498)
      write(33,4498)
 4498 format(/
     &'### error in reading quality gap filling data ...###'/)
	write( *,4497)(CMX(JP,IS),IS =1,NS)
 4497 format(10e12.3)
      call stop

      end




*     extrapolate the changes calculated by gap filling -------------------------
*     as calculated by gap filling within "perform gap filling for quality" -----
*     THIS ROUTINE APPLIES THE EFFECTS DOWNSTREAM OF THE GAP FILLING POINT ------
      subroutine extrapolate quality gap filling (JU,DQ)
      include 'COMMON DATA.FOR'
      dimension avacqu(2,MP10),nsacqu(2,MP10 ),extra load(MS)
      dimension avlcqu(2,MP10),nslcqu(2,MP10 )

      character *40UNAM
      
      UNAM = 'End of Reach                            '
      if ( IEND .eq. 0 ) UNAM = UNAME(JU)

      if ( IEND .eq. 1 ) goto 7211
      if ( KQCAL(JU) .eq. 0 ) return
 7211 if ( DQ .lt. 1.0E-06 ) return ! today

      call calculate summaries of river flow

      
*     loop on determinands -----------------------------------------------------
      do 1000 JP = 1, ndet ! ###################################################
      kprint = 0

*     apply the adjustments to each shot ---------------------------------------
*     also, work out the average effect of gap filling -------------------------
      avacqu (1,JP) = 0.0 ! gains in concentration per determinand -------------
      avacqu (2,JP) = 0.0 ! losses in concentration per determinand ------------
      nsacqu (1,JP) = 0 ! number of shots giving a gain ------------------------
      nsacqu (2,JP) = 0 ! number of shots giving a loss ------------------------
      avlcqu (1,JP) = 0.0 ! gains in load per determinand ----------------------
      avlcqu (2,JP) = 0.0 ! losses in load per determinand ---------------------
      nslcqu (1,JP) = 0 ! number of shots giving a gain in load-----------------
      nslcqu (2,JP) = 0 ! number of shots giving a loss in load-----------------

      if ( QTYPE(JP) .eq. 4 ) goto 1000 ! ######################################
      if ( NEXC(JP) .eq. 1) goto 1000 ! ########################################

      do IS = 1, NS
      extra load (IS) = 0.0  
      enddo

*     ££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££
      do 1008 IS = 1, NS ! loop on number of shots (extrapolate gap filling) ---
      if ( FMS(IS) .lt. flow (4) + 1.0E-12 ) goto 1008 ! compare with 99%-tile -

*     check for xero effect in load --------------------------------------------
      if ( CMX(JP,IS) .gt. -1.0E-12 .and.
     &     CMX(JP,IS) .lt.  1.0E-12 ) goto 1008

      goto (1018,1019,1018,1008,1018,1008),QTYPE(JP) ! - extrapolate gap filling 

*     type 2 determinand - eg. BOD and Ammonia =================================
 1019 if ( CMX(JP,IS) .gt. 1.0E-12 ) goto 1017 ! check for gain in load ========
*     load has been lost for this shot -----------------------------------------
      if ( CMS(JP,IS) .gt. 1.0e-12 ) then ! concentration exceeds zero ~~~~~~~~~
      temp = CMS(JP,IS)*EXP( CMX(JP,IS) * DQ ) ! new concentration ------------- 
      old = temp - CMS(JP,IS) ! change in concentration ------------------------
      old CMS = CMS(JP,IS) ! starting concentration ----------------------------
      CMS(JP,IS) = temp ! update the concentration -----------------------------
      do ip = 1, n2prop ! look on all contributions of concentrations ==========
*     concentrations from various types of feature etc -------------------------
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! reduction -------
      enddo ! do ip = 1, n2prop ================================================
      else ! if the concentration is negative ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CMS(JP,IS) = 0.0 ! set it to zero ----------------------------------------
      endif ! if ( CMS(JP,IS) .gt. 1.0e-10 ) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      avacqu(2,JP) = avacqu(2,JP) + old ! total losses of concentration ########
      nsacqu(2,JP) = nsacqu(2,JP) + 1 ! number of shots with losses ############
      avlcqu(2,JP) = avlcqu(2,JP) + old*FMS(IS) ! total losses of load #########
      nslcqu(2,JP) = nslcqu(2,JP) + 1 ! number of shots with losses ############
      goto 1088 ! calculate effects on discharges and water bodies -------------
*     losses of determinand ====================================================
          
*     type 1 determinand - eg. chloride etc ====================================
 1018 if ( CMX(JP,IS) .gt. 1.0E-12 ) goto 1017 ! check for gains ---------------
*     load has been lost for this shot -----------------------------------------
      old CMS = CMS(JP,IS) ! store the old concentation shot -------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! this will be a negative value ---------
      CMS(JP,IS) = AMAX1 (0.0, old CMS + temp ) ! reduce the concentration -----
      if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced --------------
      do ip = 1, n2prop ! new concentration is less than the old one -----------
      if ( ip .eq. 17 ) oldCTMS17 = CTMS(ip,JP,IS)
      if ( ip .eq. 22 ) oldCTMS22 = CTMS(ip,JP,IS)
      CTMS(ip,JP,IS) = CTMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! scale CTMS ------
      enddo
      endif ! if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced ------
      temp = CMS(JP,IS) - old CMS ! new minus old concentration ----------------
      avacqu(2,JP) = avacqu(2,JP) + temp ! add losses of concentration ---------
      nsacqu(2,JP) = nsacqu(2,JP) + 1 ! number of shots with losses ------------
      avlcqu(2,JP) = avlcqu(2,JP) + temp * FMS(IS) ! losses of load ------------
      nslcqu(2,JP) = nslcqu(2,JP) + 1 ! number of shots with losses ------------
      goto 1088 ! calculate effects on discharges ------------------------------

 1017 continue ! gains of pollutant --------------------------------------------
*     EXTRA LOAD IS ADDED A LINEARLY WITH RIVER LENGTH =========================
*     THIS MECHANISM IS APPLIED TO GAINS OF LOAD FROM QUALITY GAP FILLING ======
      
*     calculate the gain in concentration when the load CMX is added to the ----
*     river flow by quality gap filling ... FMS --------------------------------
      extra load (IS) = DQ * CMX(JP,IS) ! compute the total load over distance -
      accum load (IS) = accum load (IS) + extra load (IS) ! accumulate this ----      

      oldCMS = CMS(JP,IS) ! old concentration ----------------------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! extra concentration from gap filling --
      CMS(JP,IS) = CMS(JP,IS) + temp ! set the new concentration ---------------
      CTMS(22,JP,IS) = CTMS(22,JP,IS) + temp ! add to total from gap filling ---

      avacqu(1,JP) = avacqu(1,JP) + temp ! gains of concentration -------------- 
      nsacqu(1,JP) = nsacqu(1,JP) + 1 ! number of shots with gains -------------
      avlcqu(1,JP) = avlcqu(1,JP) + temp * FMS(IS) ! gains of load ------------- 
      nslcqu(1,JP) = nslcqu(1,JP) + 1 ! number of shots with gains -------------

      
*     ##########################################################################      
 1088 continue ! trim the annual loads from individual works ===================
 1008 continue ! end of the loop covering NS shots for this determinand ======== 

      
*     ##########################################################################     
*     trim the annual loads lost from individual water bodies ==================
      
      avacqu(1,JP) = avacqu(1,JP) / float(NS) ! ----------------------
      avacqu(2,JP) = avacqu(2,JP) / float(NS) ! ----------------------
      avlcqu(1,JP) = avlcqu(1,JP) / float(NS) ! ----------------------
      avlcqu(2,JP) = avlcqu(2,JP) / float(NS) ! ----------------------
      
      if ( abs ( LMcontrib(NTD,JP,1) ) .gt. 0.000000001 ) then
      if ( LMcontrib(NTD,JP,1) + avlcqu(2,JP) .gt. 0.0 ) then
      prune load(I13) = (LMcontrib(NTD,JP,1) + avlcqu(2,JP)) 
     &                /  LMcontrib(NTD,JP,1)
      endif
      endif ! if ( abs ( LMcontrib(NTD,jdet,1) ) .gt. 0.000000001 )

      if ( kount bodies .gt. 0 ) then ! ========================================
      do ibodies = 1, kount bodies ! ===========================================
      do ip = 1, n2prop ! scale contributions after losses ---------------------
*     breakdown of annual loads (i13) from upstream sub-catchments -------------
      TWLOADSapp(ibodies,JP,i13,ip) = ! reduce loads after losses --------------
     &TWLOADSapp(ibodies,JP,i13,ip) * prune load(I13) ! reduce loads -----------
      enddo ! do ip = 1, n2prop ------------------------------------------------
      enddo ! do ibodies = 1, kount bodies =====================================
      endif ! if ( kount bodies .gt. 0 ) =======================================
*     ##########################################################################     

      
      
      if ( ical .eq. 4 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
*     calculate the extra load =================================================
*     call staload (extra load,CC1,CC2,CC3,CC4)
*     write(170+JP,8522) CC1, CC3, CC4, CC2,distp,UNAME(KFEAT),
*    &IABS(JQCAL(KFEAT))
*8522 format('     Load gained by gap filling =',4f9.1,1x,f6.2,3x,
*    &a37,i5)
*     call statload (extra load,CC1,CC2,CC3,CC4)
*     CC1 = CC1/DQ
*     CC3 = CC3/DQ
*     CC4 = CC4/DQ
*     CC2 = CC2/DQ
*     DDDx = DIST(JU)
*     if ( UNAM .eq. 'End of Reach' ) DDDx = RLENGTH(JREACH(JU))
*     write(170+JP,8583) CC1, CC3, CC4, CC2, DDDx, UNAM,
*     &RNAME(JREACH(JU))!,DQ
*8583 format(4f10.2,f10.2,3x,a37,a16,f10.2)

      DDDx = DIST(JU)
      if ( UNAM .eq. 'End of Reach' ) DDDx = RLENGTH(JREACH(JU))
      call statload (accum load,CC1,CC2,CC3,CC4)
      CC1 = CC1/DQ
      CC3 = CC3/DQ
      CC4 = CC4/DQ
      CC2 = CC2/DQ
*     write(170+JP,8523) CC1, CC3, CC4, CC2, DDDx, UNAM,
*    &RNAME(JREACH(JU)),DQ
 8523 format(4f10.2,f10.2,3x,a37,a16,f10.2)
      endif ! if ( ical .eq. 4 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling


*     list the effects imposed by gap filling ++++++++++++++++++++++++++++++++++
      do 6632 ii = 1,2 ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( nsacqu (ii,JP) .gt. 0 ) then
      avacqu (ii,JP) = (avacqu (ii,JP)*float(NS))/float(nsacqu(ii,JP))
      dava = avacqu (ii,JP) / DQ ! ---------------------------------------------
      pern = 100.0 * float (nsacqu(ii,JP)) / float (NS)
      endif ! if ( nsacqu (ii,JP) .gt. 0 )

      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then ! -----------------------------
      if ( ii .eq. 1 .and. nsacqu (ii, JP) .gt. 0 ) then ! =====================
      if ( kprint .eq. 0 ) then
      write(170+JP,8533) 
 8533 format(110('+')/
     &'Extrapolation per kilometre downstream of Gap Filling Point ',
     &'for river water quality'/110('-'))
      kprint = 1
      endif ! if ( kprint .eq. 0 )
      write(170+JP,4552)avacqu(ii,JP),units(JP),pern,dname(jp) ! -------- Ci.GAP
 4552 format(
     &'Average quality gain introduced by gap filling ',
     &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)
      endif ! if ( ii .eq. 1 .and. nsacqu (ii, JP) .gt. 0 ) then ! =============

      if ( ii .eq. 2 .and. nsacqu (ii,JP) .gt. 0 ) then ! ======================
      if ( kprint .eq. 0 ) then
      write(170+JP,8533) 
      kprint = 1
      endif ! if ( kprint .eq. 0 )
      write(170+JP,4552) -avacqu(ii,JP),units(JP),pern,dname(JP) ! ------ Ci.GAP
      endif ! if ( ii .eq. 2 .and. nsacqu (ii,JP) .gt. 0 )
      endif ! if ( ical13 .eq. 0 etc -------------------------------------------

 6632 continue ! do 6632 ii = 1,2 ++++++++++++++++++++++++++++++++++++++++++++++
      
      if ( dint .gt. 0.000001 ) then
      if ( ical13 .eq. 0 .or. ical .eq. 3 ) then
      write(170+JP,7255)
 7255 format(110('+'))
      endif
      endif
      !if ( ical .gt. 3 ) write(170+JP,6532) ! ------ Ci.GAP
 6532 format(/)

 1000 continue !do 1000 JP = 1, ndet ###########################################

      call get summaries of river quality from the shots ! gap filling ---------
      call get summaries of loads

      return
      end


      
      subroutine statload (Y,CC1,CC2,CC3,CC4)
      include 'COMMON DATA.FOR'
      dimension Y(MS)

      CC1=0.0
      CC2=0.0
      CC3=0.0
      CC4=0.0
      CC5=0.0

      CM=0.0
      CS=0.0
      do IS=1,NS
      CM=CM+Y(IS)
      CS=CS+Y(IS)*Y(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 continue
      CS=SQRoot(1055,CS)
    6 continue
      CM=CM/NS

      CC1 = amax1 (0.0, CM )
      CC2 = CS

*     compute 95-percentile ----------------------------------------------------
      do 2 I = 1,k90
      do 3 J = I+1,NS
      if (Y(I) .gt. Y(J)) goto 3
      Ctemp = Y(I)
      Y(I) = Y(J)
      Y(J) = Ctemp
    3 continue
    2 continue
      CC4 = amax1 (0.0,Y(k95))

*     compute 5-percentile -----------------------------------------------------
      do 22 I=1,k90
      do 23 J=I+1,NS
      if (Y(I) .lt. Y(J)) goto 23
      Ctemp=Y(I)
      Y(I)=Y(J)
      Y(J)=Ctemp
   23 continue
   22 continue
      CC3 = amax1 (0.0,Y(k95))

      return
      end