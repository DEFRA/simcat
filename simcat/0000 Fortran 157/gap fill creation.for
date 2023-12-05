*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File Name: GAP FILL CREATION.FOR ... 2303 lines (324 comments)
*     ==========================================================================
*     This file deals with gap filling - calculating the differences between
*     SIMCAT's calculations and the measured distributions of river flows and 
*     river quality ... 
*     --------------------------------------------------------------------------
*     The file includes 8 sub-routines:
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
*          =====================================================================

      subroutine undertake gap filling (JU)
      include 'COMMON DATA.FOR'
      dimension oldshots (mp10, ms)

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
      return

*     add in the gap filled inflows and ---------------------------------------3
*     compute adjustments to the loads needed to produce a fit with the -------3
*     quality observed at monitoring points -----------------------------------3
    3 call add the loads from flow gap filling (JU,0) ! -----------------------3
      call perform gap filling for quality (JU) ! -----------------------------3

      JP = 1 ! ----------------------------------------------------------------3
      return ! ----------------------------------------------------------------3

*     add in the gap filled inflows and quality -------------------------------4
    4 continue ! ical = 4 include gap filling for flow and quality ------------4
           
      call add the loads from flow gap filling (JU,0) ! -----------------------4
      call get summaries of river quality from the shots ! gap filling --------4
      
*     add river quality calculated for gap filling ----------------------------4
*     compute the mean loads before gap filling of quality --------------------4
      JP = 1 ! ----------------------------------------------------------------4

*     add river quality --------------------------------------------------------
*     compute the mean loads before gap filling of quality ---------------------
      call load calculation

      do idet = 1, ndet
      do is = 1, ns
      old shots (idet, is ) = xshots ( idet,is )
      enddo
      enddo

*     add in the effects of gap filling ----------------------------------------
      call insert quality gap filling (JU)
      call get summaries of river quality from the shots ! Richard III
      
*     compute the mean load after gap filling ----------------------------------
      call load calculation

*     compute the difference in mean load from gap filling ---------------------
      do 37 idet = 1, ndet
      if ( qtype (idet) .ne. 4 ) then

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
      xdiff = xshots (idet,is ) - oldshots (idet,is )

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
      TALOADDN1 (idet,i13) = TALOADDN1 (idet,i13)
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
      TALOADDN1 (idet,J13) = TALOADDN1 (idet,J13) 
     &+ xdownload (idet,J13) 
      enddo
      endif ! fill monthly loads

      endif ! if ( qtype (idet) .ne. 4 ) **************************************4
   37 continue ! do 37 idet = 1, ndet *****************************************4
*     *************************************************************************4
    
      call add up all the loads ! for all determinands

*     calculate the effect on accumulated loads -------------------------------4
      do idet = 1,ndet
      if ( qtype (idet) .ne. 4 ) then
      kprune det = idet

      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      prune load (J1) = 1.0
      if ( abs ( TGLODE2(idet,J1) ) .gt. 0.000000001 ) then
      if ( xdownload (idet,J1) .lt. 0.0 ) then
      if ( TGLODE2(idet,J1) + xdownload (idet,J1) .gt. 0.0 ) then
      prune load (J1) = (TGLODE2(idet,J1) + xdownload (idet,J1))
     &                /  TGLODE2(idet,J1)
      endif
      endif
      endif
      enddo

*     trim back the loads for losses from gap filling --------------------------
      call scale loads after losses
      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      taloadup2 (idet,J1) = taloadup2 (idet,J1) 
     &                    + xupload (idet,J1)   
      taloadup1 (idet,J1) = taloadup1 (idet,J1) 
     &                    + xupload (idet,J1)   
      enddo
      endif
      enddo

      call add up all the loads
      call write loads after gap filling of river quality 2 ! -----------------4
      call write loads after gap filling of river quality 3 ! -----------------4

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
      write(34,3381)UNAME(JU)
 3381 format(77('-')/
     &'*** The distance to a flow gap filling point is too ...'/
     &'*** short for safe extrapolation downstream ...'/
     &'*** Extrapolation suppressed downstream of ...'/
     &'*** ',A40/77('-'))
      NEXF=1
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
      integer ZQUAL(MP10),NSKIP(MP10)
      double precision CMT(MP10,MS)
      dimension DMS(2,MP10,MS),CLS(MP10,MS)
      double precision DKY(2,MP10,MS),DEKAY(MP10,MS)
      double precision DEKB,DEKC,DEKB1,DEKB2
      dimension Ishot zero (MP10), Ishot done (MP10)

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
      write(34,4481)
      write(34,4581)UNAME(JU)
 4581 format(77('-')/'*** Name of Feature ... ',A30/77('-'))
      return
      endif ! if ( FLOW(1) .lt. 1.0e-08 ) ======================================
*     ==========================================================================

      
      do 4000 jdet = 1, ndet ! take each determinand in turn -------------------
      Ishot done (jdet) = NS ! initialise number of shots still to be set ------
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
      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      write(170+jdet,8533)dname(jdet),uname(JU)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
 8533 format(93('=')/
     &'#### A request for gap filling has been suppressed for ',
     &'... ',a11,' at ',a40/ 
     &'#### The quality data are specified as loads ...'/93('='))
      Ishot done (jdet) = -777 ! suppress gap filling - data are loads ---------
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
      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      write(170+jdet,8543)dname(jdet),uname(JU)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
 8543 format(93('=')/
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
      Ishot done (jdet) = -777 ! suppress gap filling - non-parametric data ----
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
      if ( detype (jdet) .ne. 104 ) then
      if ( detype (jdet) .ne. 9090 ) then ! ????????????????????????????????????
      if ( nobigout .le. 0 ) write(01,8099)DNAME(jdet),
     &uname(JU),IQ,DCM,
     &UNITS(jdet),DCS,UNITS(jdet),DCP,UNITS(jdet)
 8099 format(///77('#')/
     &'Gap filling for ... ',A11,' at ',a30/77('-')/
     &'QUALITY DATA SET:',I6,15X,'Required mean =',F9.2,1X,A4/
     &29X,'    Standard deviation =',F9.2,1X,A4/
     &29X,'Required 95-percentile =',F9.2,1X,A4)
      else
      if ( nobigout .le. 0 ) write(01,8499)DNAME(jdet),
     &uname(JU),IQ,DCM,
     &UNITS(jdet),DCS,UNITS(jdet),DCP,UNITS(jdet)
      write(33,8499)DNAME(jdet),
     &uname(JU),IQ,DCM, ! dissolved metal
     &UNITS(jdet),DCS,UNITS(jdet),DCP,UNITS(jdet) ! dissolved metal
 8499 format(///77('#')/
     &'Gap filling for ... ',A11,' at ',a30/77('-')/
     &'QUALITY DATA SET:',I6,3X,'REQUIRED MEAN (dissolved) =',F9.2,1X,
     &A4/17X,'    STANDARD DEVIATION (dissolved) =',F9.2,1X,A4/
     &17X,'REQUIRED 95-PERCENTILE (dissolved) =',F9.2,1X,A4)
      endif ! dissolved metal
      endif
*     ==========================================================================
      if ( QTYPE (jdet) .eq. 3 .or. QTYPE (jdet) .eq. 5 ) then
      if ( nobigout .le. 0 ) write(01,6099)DNAME(jdet),
     &uname(JU),IQ,DCM,
     &UNITS(jdet),DCS,UNITS(jdet),DCP,UNITS(jdet)
 6099 format(///77('#')/
     &'Gap filling for ... ',A11,' at ',a30/77('-')/
     &'QUALITY DATA SET:',I6,15X,'Required mean =',F9.2,1X,A4/
     &29X,'    Standard deviation =',F9.2,1X,A4/
     &29X,' Required 5-percentile =',F9.2,1X,A4)
      endif ! if ( QTYPE (jdet) .eq. 3 .or. QTYPE (jdet) .eq. 5 )
*     ==========================================================================

      
*     compute the effect on quality of gap filling -----------------------------
*     DCP is the required quality ----------------------------------------------
*     C(jdet,3) is the quality before gap filling ------------------------------
*     QNAT is the required % change in quality ---------------------------------
      if ( C(jdet,1) .gt. 1.0e-10 ) CC1 = 100.0/C(jdet,1)
      if ( C(jdet,3) .gt. 1.0e-10 ) CC2 = 100.0/C(jdet,3)
      if ( C(jdet,1) .gt. 1.0e-10 ) then
      QNAT(jdet)=INT(0.5+100.0*(DCM/AMAX1(1.0E-10,C(jdet,1))-1.0))
      QNAT(jdet)=min0(99999,QNAT(jdet))
      endif

*     compute the array of target values. These are ranked on the values of ----
*     the current shots --------------------------------------------------------


*     ==========================================================================
*     first check for a zero target river quality ------------------------------
      if ( DCP .lt. 1.0e-10 ) then
      suppress (4,jdet) = suppress (4,jdet) + 1
      if ( nobigout .le. 0 ) write(01,6781)DNAME(jdet),UNAME(JU)
      write(33,6781)DNAME(jdet),UNAME(JU)
      write(09,6781)DNAME(jdet),UNAME(JU)
      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      write(170+jdet,6781)DNAME(jdet),UNAME(JU)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
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
      Ishot done (jdet) = -777 ! suppress gap filling - quality is zero --------
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
      write(34,6741)DNAME(jdet),UNAME(JU)
      write(33,6741)DNAME(jdet),UNAME(JU)
      write(09,6741)DNAME(jdet),UNAME(JU)
      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      write(170+jdet,6741)DNAME(jdet),UNAME(JU)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      if ( iscreen .lt. 3 ) write( * ,6741)DNAME(jdet),UNAME(JU)
 6741 format(110('-')/
     &'*** The calculated upstream river quality is zero for ',a11,
     &' ... '/'*** Extrapolation suppressed downstream of: ',
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
      CC1 = CC1 * C(jdet,1)
      CC2 = CC2 * C(jdet,3)
      if ( nobigout .le. 0 ) write(01,6)CC1,CC2
    6 format(77('-')/
     &'A fit was attempted by scaling data by ',F6.1,' per cent'/
     &'             ... and the percentile by ',F6.1,' per cent'/
     &77('#'))
      endif ! if (DCM .gt. 1.0e-8) then ========================================

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( CC1 .lt. 20.0 ) then
      if ( ifbatch .eq. 1 ) then ! this is a batch run
      if ( suppress (2,jdet) .lt. 1 ) then
	call change colour of text (10) ! green
	write( *,3582)Dname(jdet),UNAME(JU)
 3582 format('*** Huge reduction needed to fit ',a11,7x,'...',
     &7x,'at ',A40,1x,25('.'))
      call set screen text colour
      endif
      if ( suppress (2,jdet) .eq. 1 ) then
	call change colour of text (10) ! green
	write( *,4582)Dname(jdet),UNAME(JU)
 4582 format('*** Huge reduction needed to fit ',
     &a11,7x,'...',7x,'at ',A40,' and other places ',8('.'))
      call set screen text colour
      endif
      endif
      suppress (2,jdet) = suppress (2,jdet) + 1
	write(01,3522)Dname(jdet),UNAME(JU)
	write(09,3522)Dname(jdet),UNAME(JU)
	write(33,3522)Dname(jdet),UNAME(JU)
 3522 format(110('*')/
     &'*** A large (more than an 80%) reduction of ',
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
      endif
      if ( suppress (3,jdet) .eq. 1 ) then
      call change colour of text (10) ! green
      write( *,5682)Dname(jdet),UNAME(JU)
 5682 format('*** Gross increase needed to fit ',
     &a11,7x,'...',7x,'at ',A40,' and elsewhere ...........')
      call set screen text colour
      endif
      endif
      suppress (3,jdet) = suppress (3,jdet) + 1
      write(01,3622)Dname(jdet),UNAME(JU)
      write(09,3622)Dname(jdet),UNAME(JU)
      write(33,3622)Dname(jdet),UNAME(JU)
      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      write(170+jdet,3622)Dname(jdet),UNAME(JU)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
 3622 format(110('*')/
     &'*** A huge (more than 5-fold) change in ',
     &'river quality is needed to match'/
     &'*** the measured water quality for ',a11/
     &'*** Extrapolation suppressed downstream of: ',A40/110('*'))  
      NEXC (jdet) = 1 ! huge change in quality needed --------------------------
      endif
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     suppress extrapolation if fit not applied to more than 1 kilometre -------
      if ( dcalquality .le. 1.0 ) then
      if ( jdet .eq.  ndetfirst ) then
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
      goto (4461,4461,4461,4452,4461,4452), QTYPE (jdet)
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
      write(34,2176)DNAME(jdet),COLD,IS
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
      ctarx = 0.9999 ! initialise the target precision for each shot -----------
      PTEST = 999.9 ! initialise percent of total unfitted shots ---------------
      ptestx = 999.0 ! initialise the previous percent of unfitted shots -------
      do jdet = 1, ndet
      Ishot zero (jdet) = 0 ! initialise the number of zero results ------------
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
      if ( NSKIP(jdet) .ne. 1 ) then
      never fitted = never fitted + NS ! initialise number of shots not fitted -
      endif
      KTEST = KTEST + NS ! sum the number of shots across all determinands -----
      if ( Ishot done (jdet) .eq. -777 ) then ! suppress gap filling -----------
      Ishot done (jdet) = 0 ! mark all shots as completed (loads etc) ----------
      do is = 1, NS ! suppress gap filling for all the shots -------------------
      KOUNT(jdet,IS) = -777 ! suppress gap filling because it's finished -------
      enddo ! suppress gap filling for all the shots ---------------------------
      else ! gap filling is not suppressed -------------------------------------
      Ishot done (jdet) = NS ! mark shots uncompleted at start of iteration ----
      do is = 1, NS ! check all the shots for complete convergence -------------
      CTEST = CMT(jdet,IS) ! the target for the shot ---------------------------
      if ( CTEST .lt. 1.0e-08 ) CTEST = 1.0e-08
      CTEST = CMS(jdet,IS)/CTEST ! ratio of estimate to target -----------------
      if (CTEST .gt. ctarx .and. CTEST .lt. 2.0 - ctarx ) then
      KOUNT(jdet,IS) = -777 ! suppress gap filling - convergence is achieved ---
      never fitted = never fitted - 1 ! reduce number for shots not fitted -----
      Ishot done (jdet) = Ishot done (jdet) - 1	
      endif
      enddo ! loop on shots ----------------------------------------------------
      endif ! if ( Ishot done (jdet) 
      endif ! if ( QTYPE (jdet) .ne. 4 ) =======================================
 7001 continue ! loop on determinands ==========================================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      

      kopped out = 0 ! initialise sum of zero results for all determinands -----
      PTEST = 100.0 * never fitted / FLOAT(KTEST) ! percent of shots not fitted 
     
*     simulate the section of river using the calculated adjustments -----------
*     the results will be used to alter the adjustments ------------------------
      KFEAT = JU
     
      call GAP FILL SIMULATION (JU) ! from "perform gap filling for quality" 333
      call get summaries of river quality from the shots ! gap filling ---------

      KSIM = KSIM + 1 ! add one to the number of simulations -------------------
      if ( KSIM .gt. 9 ) ctarx = 0.999 ! slacken the target
      if ( KSIM .gt. 18 ) ctarx = 0.98 ! slacken the target again

*     the quality calculated in [GAP FILL SIMULATION] is stored in CMS ---------
*     now compare this with the required values in CMT -------------------------
*     and adjust the factors in DEKAY, if necessary ----------------------------

      do 8001 jdet = 1, ndet ! ================================================= 
      if ( QTYPE (jdet) .eq. 4 .or. NSKIP (jdet) .eq. 1 ) goto 8001
*     ==========================================================================
*     for a weir do dissolved oxygen only --------------------------------------
      if (JT(JU) .eq. 8 .and. detype (jdet) .ne. 104)   goto 8001
*     if (quolity data(IQ,jdet,1) .lt. -1.0E-08)  goto 8001
*     ==========================================================================
      do 8002 IS = 1, NS ! loop on this determinand's shots --------------------
      if (KOUNT(jdet,IS) .eq. -777) goto 8002 ! proceed to the next shot -------
      CTEST = CMT(jdet,IS) ! the target for the shot ---------------------------
      if ( CTEST .lt. 1.0e-08 ) CTEST = 1.0e-08
      CTEST = CMS(jdet,IS)/CTEST ! ratio of estimate to target -----------------
      if (CTEST .gt. ctarx .and. CTEST .lt. 2.0 - ctarx ) then
      KOUNT(jdet,IS) = -777 ! suppress gap filling - convergence is achieved ---
      never fitted = never fitted - 1 ! reduce number for shots not fitted -----
      Ishot done (jdet) = Ishot done (jdet) - 1 ! record the shot as completed -
      endif
 8002 continue ! do 8002 IS = 1, NS --------------------------------------------
 8001 continue ! do 8001 jdet = 1, ndet ========================================
      
      PTEST = 100.0 * never fitted / FLOAT(KTEST) ! percent of shots not fitted 
      
      

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      KTEST = 0 ! initialise the total number of shots across all determinands -
*     never fitted = 0
      do 6001 jdet = 1, ndet 
      if ( QTYPE (jdet) .eq. 4 ) goto 6001
      KTEST = KTEST + NS ! sum the number of shots across all determinands -----
      if ( NSKIP (jdet) .eq. 1 ) goto 6001
      if ( Ishot done (jdet) .eq. -777 ) goto 6001 ! not for this determinand --
      if ( Ishot done (jdet) .eq. 0 ) goto 6001 ! not for this determinand -----

*     ==========================================================================
*     Weirs. Do Dissolved oxygen only ------------------------------------------
      if (JT(JU) .eq. 8 .and. detype (jdet) .ne. 104)   goto 6001
*     if (quolity data(IQ,jdet,1) .lt. -1.0E-08)  goto 6001
*     ==========================================================================

      negdiv = 0
      do 6002 IS = 1, NS ! loop on this determinand's shots --------------------
      if (KOUNT(jdet,IS) .eq. -777) then ! skip a shots that is finished with --
*     Ishot done (jdet) = Ishot done (jdet) - 1 ! record the shot as completed -
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
      write(09,1372)DNAME(jdet),KOUNT(jdet,IS),IS,CMT(jdet,IS),
     &DKY(1,jdet,IS),DMS(1,jdet,IS),perone,
     &DKY(2,jdet,IS),DMS(2,jdet,IS),pertwo
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
      if (DMS(1,jdet,IS) .le. QZEROGAP (jdet) .and.
     &    DMS(2,jdet,IS) .le. QZEROGAP (jdet)) then
      if ( KOUNT(jdet,IS) .ne. -777 ) then
      if ( Ishot zero (jdet) .eq. 0 ) then
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
      suppress (7,jdet)= suppress (7,jdet) + 1
      endif
      else
      xcmt = CMT(jdet,IS)
      call sort format 3 (TEST,QZEROGAP(jdet),xcmt)
      write(33,6379)DNAME(jdet),valchars10,valchars11,valchars12,IS
      write(09,6379)DNAME(jdet),valchars10,valchars11,valchars12,IS
 6379 format('Determinand... ',A11,' needs a value of',A10,
     &' (The minimum is ',A10,')'/'to approach a value of',A10,
     &' (Shot Number ',I5,')'/110('-'))
      Ishot zero (jdet) = Ishot zero (jdet) + 1 ! number of zero results +++++++
*     KOUNT(jdet,IS) = -777 ! suppress gap filling +++++++++++++++++++++++++++++
      endif ! if ( Ishot zero (jdet) .eq. 0 ) ++++++++++++++++++++++++++++++++++
      endif ! if ( KOUNT(jdet,IS) .ne. -777 ) ++++++++++++++++++++++++++++++++++
      endif ! if (DMS(1,jdet,IS) .le. QZEROGAP (jdet) ++++++++++++++++++++++++++
      endif ! if ( QZEROGAP (jdet) .gt. 0.0001 ) +++++++++++++++++++++++++++++++
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( IS .eq. 99043 .and. jdet .eq. 1 .and. n251 .eq. 1 ) then ! ++++++++++
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
      endif ! if ( IS .eq. 99043 .and. jdet .eq. 4 .and. n251 .eq. 1 ) +++++++++
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

      if ( Ishot zero (jdet) .gt. 0 ) then ! check shots set to zero -----------
      write(09,6892)Dname(jdet),Ishot zero (jdet),NS
      write(01,6892)Dname(jdet),Ishot zero (jdet),NS
      write(33,6892)Dname(jdet),Ishot zero (jdet),NS
 6892 format(110('-')/a11,
     &': Number of shots reset to the specified minimum quality = ',I4,
     &' out of',i6/110('-'))
	endif ! check shots set to zero ------------------------------------------

*     check for incomplete convergence for this determinand --------------------
*     add the number of unfitted shots -----------------------------------------
*     never fitted = never fitted + Ishot done (jdet) ! shots not fitted -------
      kopped out = kopped out + Ishot zero (jdet) ! sum zero results -----------
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
      endif ! check for zero results -------------------------------------------
      
      if ( iscreen .lt. 3 ) then
      if ( kopped out .gt. 0 ) then
      write(09,6921)KSIM,PTEST
      write(33,6901)KSIM,PTEST,kopped out,KTEST
      write(01,6921)PTEST
      write(09,6921)PTEST
 6921 format(7x,' Remaining gap',F8.2,' %')
      else ! checked for zero results ------------------------------------------
      call change colour of text (34) ! dull yellow ... no zero results --------
      if ( ptest .lt. ptestx .and. ptest .ge. 0.0001 
     & .and. KSIM .gt. 1 ) then
      !write( *,6921)PTEST ! percent of fitted shots ----------------------------
      !write(33,6921)PTEST ! percent of fitted shots ----------------------------
      endif ! if ( ptest .lt. ptestx )
          
 7778 if ( ptest .lt. 0.0001 ) then ! print final result ... a success ---------
      call change colour of text (10) ! green
      write( *,6921)PTEST ! percent of fitted shots ----------------------------
      call set screen text colour
      else ! print final result ... a failure ----------------------------------
      if ( KSIM .eq. maxNITZ ) then
      call change colour of text (20) ! bright red
      write( *,6921)PTEST ! percent of fitted shots ----------------------------
      call set screen text colour
      endif ! if ( KSIM .eq. maxNITZ )
      endif ! if ( ptest .lt. 0.0001 ) ! print final result --------------------
      
      write(01,6921)PTEST
      write(09,6921)PTEST
      write(33,6921)PTEST
      ptestx = ptest ! store the percent of fitted shots for future use --------
      endif ! if ( kopped out .gt. 0 )
      endif ! if ( iscreen .lt. 3 )

      if ( PTEST .gt. 999.0 ) goto 8995 ! finished

*     check for remaining unconverged shots and iterations +++++++++++++++++++++
      if ( never fitted .gt. 0 .and. KSIM .lt. maxNITZ ) goto 6009
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     iterations complete ------------------------------------------------------
      if ( ifbatch .eq. 0 ) then
 	write(33,7921)KSIM,PTEST,kopped out,KTEST
 7921 format(4x,' [ ***',I4,' GAP FILLING MONITOR = ',F6.2,
     &' % ... Imperfect shots =',i4,' in',i5,'  *** ]')
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
 6956 format(8x,'Zero divisors for ',a11,' ... check the ERR file',i8)
      call set screen text colour
 	endif
      write(01,6856)DNAME(jdet),kount bad shots,NS
      write(33,6856)DNAME(jdet),kount bad shots,NS
      write(09,6856)DNAME(jdet),kount bad shots,NS
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
      write(75,4562)DNAME(J),dcalquality,NEXC(J)
 4562 format('QUALITY',6(1H*),A11,7(1H*),' NATURAL PURIFICATION',
     &58('-'),F10.2,I2)
      write(75,8448)(CMX(J,IS),IS=1,NS)
 8448 format(10(1PE18.10))
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
      write(34,5481)UNAME(JU)
 5481 format(93('-')/
     &'*** Zero flow after a gap filling point ...'/
     &'*** Extrapolation suppressed downstream of Feature ... ',
     &A40/93('-'))
      return
      endif

*     extrapolate changes for Features downstream of Gap Filling Point wowowowowowo
      call extrapolate quality gap filling (JU,DISTP) ! 333333333333333333333333333

 2995 continue

      call calculate summaries of river flow
      call get summaries of river quality from the shots

      return
      end




      subroutine add the loads from flow gap filling (JU,JSTOPF1)
      include 'COMMON DATA.FOR'
      dimension dload(mp10), oldshots (mp10, ms)

*     compute the mean load before the gap filling of flow ---------------------
      call load calculation ! return with load shots ... xshots ----------------

      do jdet = 1, ndet
      do is = 1, ns
      old shots (jdet, is ) = xshots ( jdet,is ) ! store the old load shots ----
      enddo
      enddo

      do idet = 1, ndet
      dload (idet) = xload (idet,1,i13)
      enddo

      xupflow = 0.0
	xdownflow = 0.0

      call add the gap filled river flows (JU,JSTOPF1)

      fxxx = 0.0
	do is = 1, NS
      fxxx = fxxx + FMS(is)
	enddo
	fxxx = fxxx / float (NS)

      call load calculation 

      do jdet = 1, ndet
      do is = 1, ns
      xshots (jdet,is ) = xshots (jdet,is ) - oldshots (jdet,is )
      enddo
      enddo

      do 27 idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then

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
      xupload   (idet,K13) = xupload   (idet,K13) + xshots (idet, is)
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

      TILOADDN2 (idet,i13) = TILOADDN2 (idet,i13) 
     &                     + xdownload  (idet,i13) 
      TILOADDN1 (idet,i13) = TILOADDN1 (idet,i13) 
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

      TILOADDN2 (idet,J13) = TILOADDN2 (idet,J13) 
     &                     + xdownload (idet,J13)
      TILOADDN1 (idet,J13) = TILOADDN1 (idet,J13) 
     &                     + xdownload (idet,J13)
	endif ! if ( NSM  (idet,J13) .gt. 0 )
      enddo ! do J13 = 2, N13
	endif ! fill monthly loads
	endif ! if ( QTYPE (idet) .ne. 4 )
   27 continue ! idet = 1, ndet

	call add up all the loads

*     calculate the effect on accumulated loads --------------------------------
      do idet = 1,ndet
	if ( QTYPE (idet) .ne. 4 ) then
      kprune det = idet

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      prune load (J1) = 1.0
      if ( abs ( TGLODE2(idet,J1) ) .gt. 0.000000001 ) then
      if ( xdownload (idet,J1) .lt. 0.0 ) then
      if ( TGLODE2(idet,J1) + xdownload (idet,J1) .gt. 0.0 ) then
      prune load (J1) = ( TGLODE2(idet,J1) + xdownload (idet,J1) )
     &                /   TGLODE2(idet,J1)
      endif
	endif
      endif
	enddo

      call scale loads after losses ! trim back the loads for gap filling ------

	nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      tiloadup2 (idet,J1) = tiloadup2 (idet,J1) + xupload (idet,J1)
      tiloadup1 (idet,J1) = tiloadup1 (idet,J1) + xupload (idet,J1)
      enddo
	endif ! if ( QTYPE (idet) .ne. 4 )
      enddo

      call add up all the loads
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

      JSTOPF1 = JSTOPF ! set to zero
      call set screen text colour
*     ==========================================================================
*     special procedure for section at end of Reach and downstream of the last -
*     Feature in the Reach -----------------------------------------------------
      if ( IEND .eq. 0 ) goto 6603
 6609 if ( NEXF .eq. 0 ) then
      call extrapolate gap filled river flows (JU)
      endif
      goto 9000
 6603 continue
      if ( KFCAL(JU) .eq. 1 ) goto 6609

*     read record from channel 74 for gap filling for river flow ---------------
      rewind 74
 1000 continue

      do is = 1, NS
      FMX(IS) = 0.0
      enddo
 
      read(74,1001,end=9999,ERR=9998)RECORD
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

*     the Feature is a gap filling point for flow ------------------------------
 2090 continue

*     If the distance, DP is zero it must be set to 1.0 ------------------------
*     This forces in the corrections -------------------------------------------

      if (JFUSER(JU) .eq. 0 .and. DP .lt. 1.0e-6) DP = 1.0

*     read indicator for extrapolation -----------------------------------------
      backspace 74
      read(74,4422,ERR=3093)NEXF
 4422 format(126X,I2)
      JFUSER(JU)=1
 3000 continue

*     read the flow adjustments ------------------------------------------------
      read(74,8448,ERR=8118)(FMX(IS),IS=1,NS)
 8448 format(10(1PE18.10))

*     change the flows ---------------------------------------------------------
*     at the same time work out the average effect of gap filling of flows -----
      avacfl = 0.0 ! prepare to calculate the mean added flow
      kchk = 0
      do 1006 IS=1,NS
      avacfl = avacfl + FMX(IS) ! sum the net total addition
      FOLD = FMS (IS) ! store the value of the flow before adjustment
      FMS(IS) = FMS(IS) + DP * FMX(IS) ! add the flow adjustment
      
      if ( FMS(IS) .lt. 1.0E-10 ) then ! check for zero calculated flow ---------
      FMS(IS) = 1.0e-10 ! zero flow has been encountered ------------------------
      if ( KSIM .eq. 0 ) then
      if ( kchk .lt. 1 ) then
      kchk = kchk + 1
      if ( nobigout .le. 0 ) write(01,9186)
	call change colour of text (20) ! bright red
      if ( iscreen .lt. 3 ) write( *,9286)
 9286 format('*** Negative river flow calculated ',
     &'by GAP FILLING ... ',51x,'Check ERR file'/
     &'*** Review the data ...')
      call set screen text colour
      write(33,9186)
      endif
      endif
 9186 format(120('-')/
     &'*** Negative river flow calculated by GAP FILLING ... '/
     &'*** In making adjustments to river flow calculated ',
     &'by GAP FILLING negative values have been ',
     &'calculated ...'/
     &'*** These have been set to zero ...'/
     &'*** This may mean that it is impossible to secure exact ',
     &'agreement through Gap Filling ...',
     &'You should review the data ...'/
     &'*** This can occur if you have not dealt properly with ',
     &'the flow balance ...'/
     &'*** More seriously, the addition of load through Gap Filling ',
     &'may produce high values because of the zero '/
     &'*** dilution ... ',
     &' The remedy is to first sort out the flow balance ...'/
     &120('-'))
      endif ! if ( FMS(IS) .lt. 1.0E-10 )
 1006 continue

      avacfl = avacfl / float (NS) ! calculate the average flow
      dclfl = avacfl * DP ! average added over DP kilometres

      if ( nobigout .le. 0 ) then
      if ( ical .eq. 3 .and. KSIM .eq. 0 ) then
      call sort format 2 (dclfl,avacfl)
      write (01, 4522) DP,valchars10,funit,valchars11,funit
	endif
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
      call sort format 2 (dclfl,avacfl)
      write (34, 4522) DP,valchars10,funit,valchars11,funit
 4522 format(110('-')/
     &'Interpolation of river flow to downstream river flow gauge ...',
     &f6.1,' kilometres ',30('-')/
     &'Average flow change added by gap filling  ',5x,
     &'=',a10,1x,a4,' ... or',a10,1x,a4,' per km ',17('-')/110('-'))
      endif
      endif
      
      do 2008 IS = 1, NS
      if (FMX(IS) .lt. 1.0e-10) then
	xdownflow = xdownflow + DP * FMX(IS)
      else
      xupflow = xupflow + DP * FMX(IS)
      endif

*     losses of flow will skip the mass balance --------------------------------
      if (FMX(IS) .lt. 1.0e-10) goto 2008

      FOLD = AMAX1 ( 0.0, FMS(IS) - DP*FMX(IS) ) ! flow before the adjustment --

*     add in the extra load ----------------------------------------------------
      do 2018 JP = 1, ndet
      if ( qtype (JP) .ne. 4 ) then
      if ( detype (JP) .ne. 104 ) then
      temp = QDIFFGAP(JP) ! quality of diffuse inflows added by gap filling ----
      if ( temp .gt. 1.2 * C(JP,1) .and. JSTOPF1 .eq. JSTOPF ) then
 	call change colour of text (20) ! bright red
      call sort format 2 (temp,C(JP,1))
      write( *,8044)valchars10,valchars11,dname(JP)
 8044 format('*** Quality of flow gap-fill worse than river',
     &6x,'...',7x,'Values:',2a10,17x,'For: ',a11,9('.'))
      call set screen text colour
      write(33,8044)valchars10,valchars11,dname(JP)
      JSTOPF1 = JSTOPF1 + 1
      endif
      endif ! if ( detype (JP) .ne. 104 )
      
      xfload = FMX(IS) * DP * temp
      CMS(JP,IS)=(CMS(JP,IS)*FOLD+xfload)/FMS(IS)
      
      xlms22 = 0.0
      xlms21 = 0.0
      xlms17 = 0.0
      
      do ip = 1, nprop
      if ( ip .eq. 21 ) then
      xlms21 = LMS(ip,JP,IS)
*     mass balance to add to the contribution from (22)
      LMS(ip,JP,IS) = ((LMS(ip,JP,IS) * FOLD) + xfload ) / FMS(IS)
      else
*     dilute the concentrations of all the other contributions
      if ( ip .eq. 17 ) xlms17 = LMS(ip,JP,IS)
      if ( ip .eq. 22 ) xlms22 = LMS(ip,JP,IS)
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * FOLD / FMS(IS) ! dilute gap fill
      endif
      enddo
      
      endif ! if ( qtype (JP) .ne. 4 )
 2018 continue
 2008 continue
      
      xdownflow = xdownflow / float (NS)
      xupflow = xupflow / float (NS)
      goto 9000

*     This Feature is not a gap filling point for flow -------------------------
*     Find the Point providing data for this Feature ...
*     This will be downstream of the current Feature ...
*     Read on through FCALIB (channel 74) to find it ...
*     The next record must hold descriptive data for the next Feature ...

 2000 continue
      if (DP .lt. 1.0E-10) goto 9000
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
      if ( char4 .ne. 'FLOW' ) goto 9000
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

 9997 write(01,3901)UNAME(JU),FEATNAM
      write( *,3901)UNAME(JU),FEATNAM
      write(09,3901)UNAME(JU),FEATNAM
      write(33,3901)UNAME(JU),FEATNAM
 3901 format(//'*** NO GAP FILLING POINT FOR FEATURE ...'/
     &         '*** Current Feature ... ',A40/
     &         '*** SEARCHED AS FAR AS ... ',A40/
     &         '*** CALCULATION STOPPED ...')
      call stop

 9999 continue
      return
      call change colour of text (49) ! dull blue
      write( *,3993)UNAME(JU)
 3993 format('*** No gap filling of flow',25x,'...',7x,'at ',A40,
     &' error in sequencing of features')
      call set screen text colour
      write(01,3903)UNAME(JU)
      write(09,3903)UNAME(JU)
      write(33,3903)UNAME(JU)
      write(34,3903)UNAME(JU)
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
      goto 9000
      end

      
      
*     Add in the effects of gap filling ----------------------------------------
*     These will have been calculated by Sub-routine FLOW gap filling ----------
*     This applies the effects downstream of the gap filling point -------------
      subroutine extrapolate gap filled river flows (JU)
      include 'COMMON DATA.FOR'
      character *40UNAM

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

      do 2008 IS = 1, NS
      avacfl = avacfl + FMX(IS)
      
      if (FMX(IS) .lt. 1.0E-10) goto 2008 ! leaks will not be extrapolated ------

      FOLD = FMS(IS)
      FMS(IS) = FMS(IS) + DISTP * FMX(IS)

      do 2018 JP = 1, ndet
      if ( qtype (JP) .ne. 4 ) then
      if ( detype (JP) .ne. 104 ) then
      temp = QDIFFGAP(JP) ! quality of diffuse inflows added by gap filling ----
      if ( temp .gt. 1.2 * C(JP,1) .and. JSTOPF2 .eq. 0 ) then
 	call change colour of text (20) ! bright red
      call sort format 2 (temp,C(JP,1))
      write( *,8044)valchars10,valchars11,dname(JP)
 8044 format('*** QUALITY of flow gap-fill worse than river',
     &6x,'...',7x,'Values:',2a10,17x,'For: ',a11,9('.'))
      call set screen text colour
      write(33,8044)valchars10,valchars11,dname(JP)
      JSTOPF2 = JSTOPF2 + 1
      endif
      endif ! if ( detype (JP) .ne. 104 )

      xfload = FMX(IS) * DISTP * temp
      CMS(JP,IS)=(CMS(JP,IS)*FOLD+xfload)/FMS(IS)

      do ip = 1, nprop
      if ( ip .eq. 21 ) then
      xlms21 = LMS(ip,JP,IS)
*     mass balance to add to the contribution from (21)
      LMS(ip,JP,IS) = ((LMS(ip,JP,IS) * FOLD) + xfload ) / FMS(IS)
      else
*     dilute the concentrations of all the other contributions
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * FOLD / FMS(IS) ! dilute gap fill
      endif
      enddo
      
      endif ! if (QTYPE (JP) .ne. 4)
 2018 continue
 2008 continue

      avacfl = avacfl / float (NS)
      dcfcl = avacfl * distp
      if ( nobigout .le. 0 ) then
      if ( ical .eq. 3 .and. KSIM .eq. 0 ) then
	write (01, 4522) distp, dcfcl,funit,avacfl,funit
 4522 format(110('-')/
     &'Extrapolation of river flow downstream of river flow gauge ...',
     &f6.1,' kilometres ',30('-')/
     &'Average flow change introduced by gap filling  ',
     &'=',f10.3,1x,a4,' ... or',f10.3,1x,a4,' per km ',17('-')/110('-'))
	endif
      if ( ical .gt. 3 .or. ical .eq. 2 ) then
	write (34, 4522) distp,dcfcl,funit,avacfl,funit
      endif
	endif

      if ( MONF .gt. 1 ) call write shots for river flow

      call calculate summaries of river flow
      call get summaries of river quality from the shots
      call get summaries of loads

 2805 return
      end
