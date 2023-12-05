*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File Name: GAP FILL USE.FOR ... 1412 lines (249 comments)  1192
*     ==========================================================================
*     The suite calculates the quality between Features IFEAT1 and IFEAT2 ...
*     The data for upstream of IFEAT1 are picked up from [RELOAD] ...
*     The results from [gap fill simulation] are passed back to 
*     [perform gap filling for quality]. Here they are compared with the 
*     observed data ... and factors are adjusted ...
*     --------------------------------------------------------------------------
*     The file includes 8 SUBROUTINES:
*     --------------------------------------------------------------------------
*          SUBROUTINE gap fill simulation ==================================== 1
*          Calculate the effects of attempts at gap filling ...
*          ---------------------------------------------------------------------
*          SUBROUTINE DUMP =================================================== 2
*          Store the shots for gap filling points ... These are picked up for 
*          the iterative cycle within SUBROUTINE [gap fill simulation] ...
*          ---------------------------------------------------------------------
*          SUBROUTINE RELOAD ================================================= 3
*          Pick up the flow and quality data stored by SUBROUTINE [DUMP] ...
*          Pass the data back to SUBROUTINE [gap fill simulation] ...
*          ---------------------------------------------------------------------
*          SUBROUTINE quality fit ============================================ 4
*          Calculates QUALITY values for gap filling ...
*          Generates values from a monitored quality distribution ...
*          Sorts generated values into ascending order of magnitude ...
*          Calculates ranked order of current simulated values '''
*          Pairs generated values with simulated values of the same rank ...
*          ---------------------------------------------------------------------
*          SUBROUTINE CALFIT ================================================= 5
*          Take a set of shots from subroutine quality fit ...
*          Adjust them to have a set mean and 95-percentile ...
*          ---------------------------------------------------------------------
*          SUBROUTINE insert quality gap filling ============================= 6
*          Add in the calculated effects of gap filling for river quality ------
*          ---------------------------------------------------------------------
*          SUBROUTINE extrapolate quality gap filling ======================== 7
*          Extrapolate the changes calculated by gap filling as calculated by 
*          gap filling within SUBROUTINE [perform gap filling for quality] ...
*          Apply the effects downstrream of the gap filling point ...
*          ---------------------------------------------------------------------
*          SUBROUTINE STATLOAD =============================================== 8
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
      call RELOAD
      !write(33,5499)C(1,1),uname(JU)
 5499 format('Mean after RELOAD in GAP FILL SIMULATION =',
     &f12.5,' ... ',a30)

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
*	endif

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
      if ( DISTP .gt. 1.0E-08 ) then

      do jdet = 1, ndet
      if ( QTYPE (JDET) .ne. 4 .and. 
     &    PDRC (IQMON,JDET) .ne. 6 .and. PDRC (IQMON,JDET) .ne. 7 .and.
     &    PDRC (IQMON,JDET) .ne. 9 .and. PDRC (IQMON,JDET) .ne. 4 .and. 
     &    PDRC (IQMON,JDET) .ne. 5 .and. PDRC (IQMON,JDET) .ne. 8 ) then

      do 1008 IS = 1, NS ! loop on number of shots (GAP FILL SIMULATION) 3333333
      if ( FMS(IS) .lt. 1.0E-08) goto 1098

*     extract the constants governing the changes to quality -------------------
      CONC = CMX(JDET,IS)
      
      if (CONC .lt. 1.0E-13 .and. CONC .gt. -1.0E-13) then
	KOUNT(JDET,IS) = -777
      goto 1008 ! gap filling is not needed for this shot ----------------------
      endif

      goto (1018,1018,1018,1008,1018,1008), QTYPE(JDET) ! GAP FILL SIMULATION 33

*     losses of type 2 determinand - eg. BOD and Ammonia +++++++++++++++++++++++
 1019 if ( CMX(JDET,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains -------------
	if ( CMS(JDET,IS) .gt. 1.0e-10 ) then
      CMS(JDET,IS)=CMS(JDET,IS)*EXP(CMX(JDET,IS)*DISTP)
	else
      CMS(JDET,IS) = 0.0
      endif
      goto 1008 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 1018 if ( CMX(JDET,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains -------------
      old CMS = CMS (JDET,IS) ! store the old concentration --------------------
      temp = DISTP * CMX(JDET,IS) / FMS(IS) ! calculate reduced concentration --
      CMS(JDET,IS) = old CMS + temp ! reduce the concentration -----------------
      
*     ==========================================================================
*     check for a negative concentration =======================================
      if ( CMS(JDET,IS) .gt. 1.0E-12 ) goto 1098
      fault = CMS(JDET,IS)
      CMS(JDET,IS) = AMAX1(1.0e-10,CMS(JDET,IS)) ! adjust this -----------------
      if ( JSTOPn(JDET) .lt. 1 ) then
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      jstopn(JDET) = jstopn(JDET) + 1 
	if ( KSIM .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,9783)DNAME(JDET),fault,is
 9783 format(8x,'Negative concentrations needed for Gap Filling ',
     &'for ',A11,f11.3,i5)
      call set screen text colour
      endif
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------
      write(09,9483)DNAME(JDET),fault
      write(01,9483)DNAME(Jdet),fault
 9483 format(98('-')/
     &'*** Negative concentration(s) are needed for a ',
     &'complete fit for ',A11,f11.3,'     ***'/98('-'))
      write(33,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
      write(01,9413)UNAME(JU),DNAME(JDET),
     &IS,fault,CMS(JDET,IS)
 9413 format(110('-')/'Feature: ',A40/110('-')/
     &'Negative or sub-minimum concentration(s) are needed for a ',
     &'complete fit for ',A11/
     &'You could review your data. ',
     &'And check the fit in the .SUM file. It may be good enough.'/
     &110('-')/'First shot affected =',4x,i8,
     &'  Computed quality =',F12.4,
     &'     ... The value was reset to  ',F12.4)
      write(33,9573)
      write(01,9573)
 9573 format(110('-')/
     &'This indicates that Gap Filling is being asked to ',
     &'explain too big a difference between the observed and '/
     &'calculated results. ',
     &'Perhaps a discharge quality is too poor for the observed',
     &' river quality downstream ...'/110('-'))
      endif ! if ( JSTOPn(JDET) .eq. 0 )
*     ================================ end of check for a negative concentration
      goto 1008 

 1017 continue ! gains of pollutant --------------------------------------------
      old CMS = CMS (JDET,IS)
      CMS(JDET,IS) = CMS(JDET,IS) + DISTP * CMX(JDET,IS) / FMS(IS)
      
 1098 continue ! jump to here if flow of quality is zero -----------------------
      
 1008 continue ! end of the loop covering NS shots for this determinand --------

      endif ! if ( QTYPE (JDET) .ne. 4 
      enddo ! do jdet = 1, ndet
      endif ! if ( DISTP .gt. 1.0E-08 )

      KFEAT = IFEAT

*     suppress unnecessary output and calculations -----------------------------
      JSKIP = 1
      feeture = IFEAT

      if ( JT(IFEAT) .ne. 10 ) then
*     call details upstream of feature
      
      call process the feature (JT(IFEAT))
      endif

      if ( IFEAT .eq. IFEAT2 ) goto 99 ! 333333333333333333333333333333333333333
   11 JCTOP=0
      
      call get summaries of river quality from the shots

      !write(33,5899)C(1,1),uname(IFEAT)
15899 format('Mean inside gap fill simulation          =',f12.5,
     &' ... ',a30)
      goto 90
   99 continue
      
      call get summaries of river quality from the shots
      
      return
      end



*     store the shots for gap filling points -----------------------------------
*     these are picked up for the iterative cycle within [GAP FILL SIMULATION] -
      subroutine DUMP (iopt)
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



*     Pick up the flow and quality data stored in DUMP -------------------------
*     Pass the data back to [GAP FILL SIMULATION] 333333333333333333333333333333
      subroutine RELOAD
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

      call RANK (WORK2,ISVAL2,NS) ! sort and rank the generated target values --
      call RANK (WORK1,ISVAL1,NS) ! sort and rank SIMCAT's calculated values ---

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
      call CALFIT(JP,DM,DS,DP,CMT)

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




*     Take a set of shots from subroutine quality fit ...
*     and adjust them to have a set mean and 95-percentile ...
      subroutine CALFIT(JDET,DM,DS,DP,CMT)
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
      call calculate summaries of river quality (JDET,Y)
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
      call calculate summaries of river quality (JDET,Y)

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

      call RANK (WORK,IRANK,NS)

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
      call calculate summaries of river quality (JDET,WORK)

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
      dimension avacqu ( 2, MP10 ), nsacqu ( 2, MP10 ), extra load (MS)
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
      endif
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

      if (KINT .ne. 0 ) DQ = DINT

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
      kprint = 0

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     loop on determinands -----------------------------------------------------

      limit printing of messages = 0
      DO 1575 JP = 1, ndet
      DDNAM=DNAME(JP)
      read(record,5303) char11,NEXC(JP)
 5303 format(13x,a11,96x,i2)
      if ( char11 .eq. DDNAM ) goto 3301
      write(01,3801)DNAME(JP),char11,FEATNAM
      write(34,3801)DNAME(JP),char11,FEATNAM
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
      read(75,8448,err=4499)(CMX(JP,IS),IS=1,NS)
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

      do 1008 IS = 1, NS ! loop on number of shots (INSERT QUALITY GAP FILLING)
      if (FMS(IS) .lt. 1.0E-08) goto 1008

*     apply the gap filled adjustment ------------------------------------------
      CONC = CMX(JP,IS) ! store the gap fill adjustment for this shot ----------
*     skip zero adjustments ----------------------------------------------------
      if (CONC .lt. 1.0E-13 .and. CONC .gt. -1.0E-13) goto 1008

      goto (1018,1018,1018,1008,1018,1008), QTYPE (JP) ! insert quality gap fill

*     losses of type 2 determinand - eg. BOD and Ammonia #######################
 1019 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains ###############
	if ( CMS(JP,IS) .gt. 1.0e-10 ) then ! ====================================
      temp = CMS(JP,IS) * EXP ( CMX(JP,IS) * DQ )
      old CMS = CMS (JP,IS)
      CMS (JP,IS) = temp ! store the adjusted quality ++++++++++++++++++++++++++
      do ip = 1, nprop
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! decay - gap filling
      enddo
      else ! if ( CMS(JP,IS) .gt. 1.0e-10 ) ====================================
      CMS(JP,IS) = 0.0
	endif ! if ( CMS(JP,IS) .gt. 1.0e-10 ) ===================================
      avacqu (2,JP) = avacqu (2,JP) + old CMS ! accumulate net loss ############  wowowowowow
      nsacqu (2,JP) = nsacqu (2,JP) + 1
      goto 1008 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     loss of type 1 determinand - eg. Chloride --------------------------------
 1018 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains ---------------
      old CMS = CMS (JP,IS)
      temp = DQ * CMX(JP,IS) / FMS(IS) ! calculate the reduced concentration ---
      CMS(JP,IS) = old CMS + temp ! reduce the concentration -------------------
      
      oldLMS17 = 0.0
      oldLMS22 = 0.0 ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     check for negative concentration =========================================
      if ( CMS(JP,IS) .le. 1.0E-25 ) then
      jstopn(JP) = jstopn(JP) + 1 
      fault = CMS(JP,IS)
      CMS(JP,IS) = AMAX1(1.0e-10,CMS(JP,IS))
      if ( iscreen .lt. 3 ) then ! ---------------------------------------------
      if ( JSTOPn(JP) .eq. 1 ) then ! ------------------------------------------
	call change colour of text (20) ! bright red
      write( *,9434)DNAME(JP)
 9434 format(
     &'* NEGATIVE (or sub-minimum) concentrations are needed for a',
     &' complete fit for ',A11)
      call set screen text colour
      endif ! if ( JSTOPn(JP) .eq. 1 ) -----------------------------------------
      endif ! if ( iscreen .lt. 3 ) --------------------------------------------
      
      if ( JSTOPn(JP) .eq. 1 ) then
      write(09,9484)DNAME(JP)
 9484 format(98('-')/
     &'*** NEGATIVE or sub-minimum concentrations(s) are needed for a',
     &' complete fit for ',A11,'    ***'/98('-'))
      write(33,9413)UNAME(JU),DNAME(JP),IS,fault,CMS(JP,IS)
 9413 format(/110('-')/'Feature: ',A40/110('-')/
     &'Negative or sub-minimum concentration(s) are needed for a ',
     &'complete fit for ',A11/
     &'You could review your data. ',
     &'And check the fit in the .SUM file. It may be good enough.'/
     &110('-')/'First shot affected =',4x,i8,
     &'  Computed quality =',F12.4,
     &'         The value was reset to  ',F12.4)
      write(33,9573)
 9573 format(110('-')/
     &'This indicates that gap filling is being asked to ',
     &'explain too big a difference between the observed and '/
     &'calculated results. ',
     &'Perhaps a discharge quality is too poor for the observed',
     &' river quality downstream ...'/110('-'))
      endif ! if ( JSTOPn(JP) .eq. 1 ) ---------------------

      do ip = 1, nprop ! new concentration is less than the old one ------------
      if ( ip .eq. 17 ) oldLMS17 = LMS(ip,JP,IS)
      if ( ip .eq. 22 ) oldLMS17 = LMS(ip,JP,IS)
      LMS(ip,JP,IS) = 0.0 ! concs from various types of feature ----------------
      enddo

      goto 1008 
      endif ! check for negative concentration =================================
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


	if ( CMS(JP,IS) .lt. old CMS ) then ! ====================================
      do ip = 1, nprop ! new concentration is less than the old one ------------
      if ( ip .eq. 17 ) oldLMS17 = LMS(ip,JP,IS)
      if ( ip .eq. 22 ) oldLMS17 = LMS(ip,JP,IS)
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! scale LMS ---------
      enddo
 1200 format(i4,12f10.5,'   Losses')
      endif ! if ( CMS(JP,IS) .lt. old CMS ) ===================================

      avacqu (2,JP) = avacqu (2,JP) + temp
      nsacqu (2,JP) = nsacqu (2,JP) + 1
      goto 1008

*     extra loads are added in linearly with river length ----------------------
*     this mechanism is applied to gains of load -------------------------------
 1017 continue ! gains of pollutant --------------------------------------------

      oldLMS17 = 0.0
      oldLMS22 = 0.0 ! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

*     calculate the gain in concentration when the load CMX is added to the ----
*     river flow ... FMS -------------------------------------------------------
      extra load (IS) = DQ * CMX(JP,IS) ! compute the total load over distance -
      accum load (IS) = accum load (IS) + extra load (IS) ! accumulate ---------

      if ( FMS (IS) .lt. 1.0e-06 ) then
      if (limit printing of messages .eq. 0 ) then
      limit printing of messages = 1
	write(33,8633)
      if ( ifbatch .eq. 0 ) then
      write( *,8633) IS
 8633 format(77('-')/'Near zero flow encountered in gap filling ',
     &'for flow ...',I4/'Loads added by gap filling for quality ',
     &'have been set to zero ...'/77('-'))
      temp = 0.0
      endif
	endif
	else ! if ( FMS (IS) .lt. 1.0e-06 )
      temp = extra load (IS) / FMS(IS) ! compute the added concentration 
      jdet = JP
	endif

      old CMS = CMS (JP,IS)
      CMS(JP,IS) = CMS(JP,IS) + temp ! new concentration from addition
      old LMS = LMS (22,JP,IS)
      LMS(22,JP,IS) = LMS(22,JP,IS) + temp ! new concentration for 22

      avacqu (1,JP) = avacqu (1,JP) + temp
      nsacqu (1,JP) = nsacqu (1,JP) + 1

 1008 continue ! ===============================================================

*     write out results that may help with calibration ------------------------- 

      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      if ( ical .eq. 4 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
*     write(170+JP,3008)UNAME(KFEAT),dname(JP),distp
*3008 format(100('-')/'Feature: ',a37,' for ',a11,' over',f7.2,' km'/
*    &100('-'))
*     if ( JSTOPn(JP) .gt. 0 ) then
*     write(170+JDET,6692)JSTOPn(JP),NS
*6692 format('Number of shots affected = ',I4,' out of',i6/80('-'))
*     endif
      endif ! if ( ical .eq. 4 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling

      if ( JSTOPn(JP) .gt. 0 ) write(09,6192)JSTOPn(JP),NS,Dname(JP)
      if ( JSTOPn(JP) .gt. 0 ) write(33,6192)JSTOPn(JP),NS,Dname(JP)
 6192 format(
     &'Number of shots affected = ',I4,' out of',i6,2x,a11/98('-'))

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

      if ( kprint .eq. 0 ) then
      write (34,4592) 
 4592 format(
     &'Interpolation of river quality to downstream gap filling Point ',
     &47('-'))
	kprint = 1
      endif

      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
*     calculate the extra load =================================================
*     call STATLOAD (extra load,CC1,CC2,CC3,CC4)
*     write(170+JP,8522) CC1, CC3, CC4, CC2,distp,UNAME(KFEAT),
*    &IABS(JQCAL(KFEAT))
*8522 format('     Load gained by gap filling =',4f9.1,1x,f6.2,3x,
*    &a37,i5)
      call STATLOAD (extra load,CC1,CC2,CC3,CC4)
      CC1 = CC1/DQ
      CC3 = CC3/DQ
      CC4 = CC4/DQ
      CC2 = CC2/DQ
      write(170+JP,8823) CC1, CC3, CC4, CC2, dist(KFEAT),
     &UNAME(KFEAT),RNAME(JREACH(KFEAT))!,DQ
 8823 format(4f10.2,f10.2,3x,a37,a16,f10.2,f10.2)

*     call STATLOAD (accum load,CC1,CC2,CC3,CC4)
*     CC1 = CC1/DQ
*     CC3 = CC3/DQ
*     CC4 = CC4/DQ
*     CC2 = CC2/DQ
*     write(170+JP,8523) CC1, CC3, CC4, CC2, dist(KFEAT),
*    &UNAME(KFEAT),RNAME(JREACH(KFEAT)),DQ
*8523 format(4f10.2,f10.2,3x,a37,a16,f10.2,f10.2)
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling

      if ( IABS(JQCAL(KFEAT)) .gt. 0 ) then
      do IS = 1, NS
      accum load (IS) = 0.0  
      enddo
      endif

*     write(170+JP,9522) avacqu(ii,JP),units(JP),pern
*9522 format('Mean quality gained by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)')
*     write (34,4522) avacqu(ii,JP),units(JP),pern,dname(jp)
*4522 format('Average quality gain introduced by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)

      else
      if ( nsacqu (ii,JP) .gt. 0 ) then
      if ( kprint .eq. 0 ) then
      write (34,4692) 
 4692 format(
     &'Interpolation of river quality to downstream gap filling Point ',
     &47('-'))
	kprint = 1
	endif

*     write(170+JP,6552) -avacqu(ii,JP),units(JP),pern
*6552 format('Average quality loss introduced by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)')
*     write (34,4552) -avacqu(ii,JP),units(JP),pern,dname(jp)
*4552 format('Average quality loss introduced by gap filling ',
*    &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)

      endif
      endif
	endif
	endif
 6632 continue

 1576 read(75,1001,end=9999,ERR=9998)RECORD

 1575 continue ! end of loop on determinands +++++++++++++++++++++++++++++++++++


      if ( dint .gt. 0.000001 ) write(34,7255)
 7255 format(110('-'))
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
      write(34,8081)FEATNAM
      write( *,8081)FEATNAM
      write(09,8081)FEATNAM
 8081 format(/
     &'*** Error in reading NEXQ in SUBROUTINE perform quality ',
     &'gap filling ... '/A40)
      call stop

 9998 if ( nobigout .le. 0 ) write(01,3900)
      write(33,3900)
      write(34,3900)
      write(01,3900)
      write( *,3900)
      write(09,3900)
 3900 format(/'*** Error in gap filling data for river quality ...'/
     &'*** CALCULATION STOPPED ...')
      call stop

 9997 continue
      write(33,3901)UNAME(JU),FEATNAM
      write(34,3901)UNAME(JU),FEATNAM
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
      write(34,3903)UNAME(JU)
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
      dimension avacqu ( 2, MP10 ), nsacqu ( 2, MP10 ), extra load (MS)

      CHARACTER *40UNAM

      UNAM = 'End of Reach                            '
      if ( IEND .eq. 0 ) UNAM=UNAME(JU)

      if ( IEND .eq. 1 ) goto 7211
      if ( KQCAL(JU) .eq. 0 ) return
 7211 if ( DQ .lt. 1.0E-06 ) return ! today

      call calculate summaries of river flow

*     loop on determinands -----------------------------------------------------
      kprint = 0
      do 1000 JP = 1, ndet

*     apply the adjustments to each shot ---------------------------------------
*     also, work out the average effect of gap filling -------------------------
      avacqu ( 1, JP ) = 0.0 ! gains in load per determinand -------------------
      avacqu ( 2, JP ) = 0.0 ! losses in load per determinand ------------------
      nsacqu ( 1, JP ) = 0 ! number of shots giving a gain ---------------------
      nsacqu ( 2, JP ) = 0 ! number of shots giving a loss ---------------------

      if ( QTYPE(JP) .eq. 4 ) goto 1000
      if ( NEXC(JP) .eq. 1) goto 1000

      do IS = 1, NS
      extra load (IS) = 0.0  
      enddo

      do 1008 IS = 1, NS ! loop on number of shots (extrapolate gap filling) ---
      if ( FMS(IS) .lt. flow (4) + 1.0E-12 ) goto 1008 ! compare with 99%-tile -

*     check for xero effect ----------------------------------------------------
      if ( CMX(JP,IS) .gt. -1.0E-12
     & .and. CMX(JP,IS) .lt. 1.0E-12 ) goto 1008

      goto (1018,1018,1018,1008,1018,1008),QTYPE (JP) !  extrapolate gap filling 

*     losses of type 2 determinand - eg. BOD and Ammonia ############## NOT USED
 1019 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! check for gains ###### NOT USED
      if ( CMS(JP,IS) .gt. 1.0e-10 ) then ! ########################### NOT USED
      temp = CMS(JP,IS)*EXP( CMX(JP,IS) * DQ ) ! ###################### NOT USED
      old = temp - CMS(JP,IS) ! ####################################### NOT USED
      old CMS = CMS(JP,IS) ! ########################################## NOT USED
      CMS (JP,IS) = temp ! ############################################ NOT USED
      do ip = 1, nprop ! ############################################## NOT USED
*     concentrations from various types of feature #################### NOT USED
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! ########## NOT USED
      enddo ! ######################################################### NOT USED
      else ! ########################################################## NOT USED
      CMS(JP,IS) = 0.0 ! ############################################## NOT USED
      endif ! ######################################################### NOT USED
      avacqu (2,JP) = avacqu (2,JP) + old ! ########################### NOT USED
      nsacqu (2,JP) = nsacqu (2,JP) + 1 ! ############################# NOT USED
      goto 1008 ! ##################################################### NOT USED

*     losses of determinand ====================================================
 1018 if ( CMX(JP,IS) .gt. 1.0E-10 ) goto 1017 ! and check for gains -----------
      old CMS = CMS(JP,IS) ! store the old concentation ------------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! this will be a negative value ---------
      CMS(JP,IS) = AMAX1 (0.0, old CMS + temp ) ! reduce the concentration -----

	if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced --------------
      do ip = 1, nprop ! new concentration is less than the old one ------------
      if ( ip .eq. 17 ) old LMS17 = LMS(ip,JP,IS)
      if ( ip .eq. 22 ) old LMS22 = LMS(ip,JP,IS)
      LMS(ip,JP,IS) = LMS(ip,JP,IS) * CMS(JP,IS) / old CMS ! scale LMS ---------
      enddo
      endif ! if ( CMS(JP,IS) .lt. old CMS ) then ! the level has reduced ------

      temp = old CMS - CMS(JP,IS)
      avacqu ( 2, JP ) = avacqu ( 2, JP ) + temp
      nsacqu ( 2, JP ) = nsacqu ( 2, JP ) + 1
      goto 1008

*     EXTRA LOAD IS ADDED A LINEARLY WITH RIVER LENGTH =========================
*     THIS MECHANISM IS APPLIED TO GAINS OF LOAD ===============================
 1017 continue ! gains of pollutant --------------------------------------------

*     calculate the gain in concentration when the load CMX is added to the ----
*     river flow ... FMS -------------------------------------------------------
      extra load (IS) = DQ * CMX(JP,IS) ! compute the total load over distance -
      accum load (IS) = accum load (IS) + extra load (IS) ! accumulate ---------      

      oldCMS = CMS(JP,IS) ! old concentration ----------------------------------
      temp = DQ * CMX(JP,IS) / FMS(IS) ! extra load from gap filling --------
      CMS(JP,IS) = CMS(JP,IS) + temp
      LMS(22,JP,IS) = LMS(22,JP,IS) + temp

*     temp = CMS(JP,IS) - oldCMS
      avacqu ( 1, JP ) = avacqu ( 1, JP ) + temp
      nsacqu ( 1, JP ) = nsacqu ( 1, JP ) + 1

 1008 continue

      if ( n253 .eq. 1 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      if ( ical .eq. 4 ) then ! GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
*     calculate the extra load =================================================
*     call STATLOAD (extra load,CC1,CC2,CC3,CC4)
*     write(170+JP,8522) CC1, CC3, CC4, CC2,distp,UNAME(KFEAT),
*    &IABS(JQCAL(KFEAT))
*8522 format('     Load gained by gap filling =',4f9.1,1x,f6.2,3x,
*    &a37,i5)
      call STATLOAD (extra load,CC1,CC2,CC3,CC4)
      CC1 = CC1/DQ
      CC3 = CC3/DQ
      CC4 = CC4/DQ
      CC2 = CC2/DQ
      DDDx = DIST(JU)
      if ( UNAM .eq. 'End of Reach' ) DDDx = RLENGTH(JREACH(JU))
      write(170+JP,8583) CC1, CC3, CC4, CC2, DDDx, UNAM,
     &RNAME(JREACH(JU))!,DQ
 8583 format(4f10.2,f10.2,3x,a37,a16,f10.2)

      DDDx = DIST(JU)
      if ( UNAM .eq. 'End of Reach' ) DDDx = RLENGTH(JREACH(JU))
      call STATLOAD (accum load,CC1,CC2,CC3,CC4)
      CC1 = CC1/DQ
      CC3 = CC3/DQ
      CC4 = CC4/DQ
      CC2 = CC2/DQ
*     write(170+JP,8523) CC1, CC3, CC4, CC2, DDDx, UNAM,
*    &RNAME(JREACH(JU)),DQ
 8523 format(4f10.2,f10.2,3x,a37,a16,f10.2)
      endif ! if ( ical .eq. 4 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling
      endif ! if ( n253 .eq. 1 ) GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG gap filling

*     list the effects imposed by gap filling ----------------------------------
      do 6632 ii = 1 , 2
      if ( nsacqu (ii,JP) .gt. 0 ) then
      avacqu (ii,JP) = avacqu (ii,JP) / float(nsacqu(ii,JP))
      dava = avacqu (ii,JP) / DQ ! ---------------------------------------------
      pern = 100.0 * float (nsacqu(ii,JP)) / float (NS)
      endif

      if ( ical13 .eq. 0 ) then ! ----------------------------------------------
      if ( ii .eq. 1 .and. nsacqu (ii, JP) .gt. 0 ) then
      if ( kprint .eq. 0 ) then
*     write (09, 8522) 
      write (34, 8533) 
 8533 format(110('-')/
     &'Extrapolation per kilometre downstream of Gap Filling Point '/
     &110('-'))
	kprint = 1
	endif 
*     write (09, 4522) avacqu(ii,JP),units(JP),pern,dname(jp)
      write (34, 4522) avacqu(ii,JP),units(JP),pern,dname(jp)
 4522 format(
     &'Average quality gain introduced by gap filling ',
     &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)
      endif

      if ( ii .eq. 2 .and. nsacqu (ii,JP) .gt. 0 ) then
      if ( kprint .eq. 0 ) then
      write (34, 8533) 
	kprint = 1
	endif 
*     write (09, 4552) -avacqu(ii,JP),units(JP),pern,dname(jp)
      write (34, 4552) -avacqu(ii,JP),units(JP),pern,dname(jp)
 4552 format(
     &'Average quality loss introduced by gap filling ',
     &'=',f10.3,1x,a4,' (',f5.1,' % shots)   ',a11)
      endif
	endif ! if ( ical13 .eq. 0 ) ---------------------------------------------

 6632 continue
 1000 continue

      if ( dint .gt. 0.000001 ) then
      if ( ical13 .eq. 0 ) then
*	write(09,7255)
      write(34,7255)
 7255 format(110('-'))
      endif
      endif

      if ( ical .gt. 3 ) write(34,6532)
 6532 format(/)

      call get summaries of river quality from the shots
      call get summaries of loads

      return
      end


      
      subroutine STATLOAD(Y,CC1,CC2,CC3,CC4)
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

