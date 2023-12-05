*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ...
*     ==========================================================================
*     File: diffuse features.for   3048 ----------------------------------------
*     --------------------------------------------------------------------------
*     Calculations for features - Diffuse pollution
*     --------------------------------------------------------------------------
*     This file contains 46 sub-routines 
*     They are called: ---------------------------------------------------------
*     --------------------------------------------------------------------------
*     ...... turn on river based diffuse pollution ! ----------------- (13)
*     ...... turn off river based diffuse pollution ! ---------------- (15)
*     ...... turn on river based diffuse pollution from livestock ! -- (25)
*     ...... turn off river based diffuse pollution from livestock ! - (26) 
*     ...... turn on river based diffuse pollution from arable ! ----- (27)
*     ...... turn off river based diffuse pollution from arable ! ---- (28)
*     ...... turn on river based diffuse pollution from highways ! --- (29)
*     ...... turn off river based diffuse pollution from highways ! -- (30) 
*     ...... turn on river based diffuse pollution from urban ! ------ (31)
*     ...... turn off river based diffuse pollution from urban ! ----- (32) 
*     ...... turn on diffuse pollution from the atmosphere ! --------- (33)
*     ...... turn off diffuse pollution from the atmosphere ! -------- (34)
*     ...... turn on diffuse pollution from background ! ------------- (35)
*     ...... turn off diffuse pollution from background ! ------------ (36)
*     ...... turn on diffuse pollution from mines ! ------------------ (46)
*     ...... turn off diffuse pollution from mines ! ----------------- (47)
*     ...... turn on diffuse pollution from birds ! ------------------ (48)
*     ...... turn off diffuse pollution from birds ! ----------------- (49)
*     ...... turn on diffuse pollution from septic tanks ! ----------- (37)
*     ...... turn off diffuse pollution from septic tanks ! ---------- (38)
*     ...... turn on diffuse pollution from aggregated CSOs ! -------- (40)
*     ...... turn off diffuse pollution from aggregated CSOs ! ------- (41)
*     ...... turn on discharge based diffuse pollution ! ------------- (15)
*     ...... turn off discharge based diffuse pollution ! ------------ (16)
*     ...... turn on diffuse pollution from aggregated STWs ! -------- (42)
*     ...... turn off diffuse pollution from aggregated STWs ! ------- (43)
*     ...... turn on nameless diffuse pollution one ! ---------------- (50)
*     ...... turn off nameless diffuse pollution one ! --------------- (51)
*     ...... turn on nameless diffuse pollution two ! ---------------- (52)
*     ...... turn off nameless diffuse pollution two ! --------------- (53)
*     ...... turn on nameless diffuse pollution three ! -------------- (54)
*     ...... turn off nameless diffuse pollution three ! ------------- (55)
*     ...... turn on nameless diffuse pollution four ! --------------- (56)
*     ...... turn off nameless diffuse pollution four ! -------------- (57)
*     ...... turn on nameless diffuse pollution five ! --------------- (58)
*     ...... turn off nameless diffuse pollution five ! -------------- (59)
*     ...... stream diffuse ! add in the diffuse inflows ------------------
*     ...... effluent diffuse 15 ! ----------------------------------- (15)
*     ...... effluent diffuse 42 ! ----------------------------------- (42)
*     ...... reach diffuse ! diffuse inflows set with Reach data ----------
*     ...... write message that diffuse discharge pollution has started ---
*     ...... check for an infeasibly large river flow
*     ...... accumulate total loads from diffuse sources
*     ...... check for nonparametric distribution ! =======================
*     ...... check the quality data (KRF,KRQ,KRA) ! -----------------------
*     ...... check the flow data (KRF,KRQ,KRA) ! --------------------------
*     --------------------------------------------------------------------------
*     This file deals with calculations for diffuse pollution.  These are:
*     --------------------------------------------------------------------------
*       Agricultural livestock (25/26)
*       Agricultural arable (27/28)  
*       Highway (non-urban) runoff (29/30)
*       Urban runoff (31/32)
*       Direct Atmospheric Deposition (i.e. to water surfaces) (33/34)
*       Natural background (35/36)
*       Population not covered by STW point inputs (37/38) ... septic tanks
*       Aggregated CSOs (40/41)
*       Aggregated sewage works (42/43)
*       Diffuse mines (46/47)
*       Birds, boats and angling (48/50)
*       User-named diffuse pollution (50/51)
*       User-named diffuse pollution (52/53)
*       User-named diffuse pollution (54/55)
*       User-named diffuse pollution (56/57)
*       User-named diffuse pollution (58/59)
*       River based diffuse pollution (13/14)
*       Discharge based diffuse pollution (15/16
*     --------------------------------------------------------------------------

      subroutine turn on river based diffuse pollution ! ------------------ (13)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (13)
      
      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then ! --------------------------- (13)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 13',
     &' - unassigned)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 13',
     &' - unassigned)'/
     &'Added as a step up in concentration or load ', ! ------------------- (13)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif ! if ( add diffconc (KFEAT) .eq. 0 ) then ! ------------------- (13)
      endif ! if ( nobigout .le. 0 ) then --------------------------------- (13)
      endif ! if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) --------------------- (13)
      endif

      call initialise data for mass balance ! ----------- diffuse pollution (13)

      KRFPOL13 = JF(KFEAT)
      KRQPOL13 = JQ(KFEAT)
      KRAPOL13 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! no flow added ----- (13)
      
      call check the flow data (KRFPOL13,KRQPOL13,KRAPOL13) ! ------------- (13)
      if ( KRFPOL13 .eq. 0 .and. KRQPOL13 .eq. 0 
     &                     .and. KRAPOL13 .eq. 0 ) return ! --------------- (13) 

      call check the quality data (KRFPOL13,KRQPOL13,KRAPOL13) ! ---------- (13)
      if ( KRFPOL13 .eq. 0 .and. KRQPOL13 .eq. 0 
     &                     .and. KRAPOL13 .eq. 0 ) return ! --------------- (13) 
      
      IF = KRFPOL13
      IQ = KRQPOL13
      
      call check for an infeasibly large river flow ! ===================== (13)
      call check for nonparametric distribution ! ========================= (13)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end




*     --------------------------------------------------------------------------
      subroutine turn off river based diffuse pollution ! ----------------- (13)
      include 'COMMON DATA.FOR'

      KRFPOL13 = 0
      KRQPOL13 = 0
      KRAPOL13 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution (river type - 13) ... '/77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end



      subroutine turn on river based diffuse pollution from livestock ! --- (25)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (25)
 
      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then ! --------------------------- (25)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 25',
     &' - livestock)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 25',
     &' - livestock)'/
     &'Added as a step up in concentration or load ', ! ------------------- (25)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif ! if ( add diffconc (KFEAT) .eq. 0 ) then ! ------------------- (25)
      endif ! if ( nobigout .le. 0 )
      endif
      endif

      call initialise data for mass balance ! ------------------- livestock (25)

      KRFPOL25 = JF(KFEAT)
      KRQPOL25 = JQ(KFEAT)
      KRAPOL25 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (25)
      
      call check the flow data (KRFPOL25,KRQPOL25,KRAPOL25) ! ------------- (25)
      if ( KRFPOL25 .eq. 0 .and. KRQPOL25 .eq. 0 
     &                     .and. KRAPOL25 .eq. 0 ) return ! --------------- (25) 

      call check the quality data (KRFPOL25,KRQPOL25,KRAPOL25) ! ---------- (25)
      if ( KRFPOL25 .eq. 0 .and. KRQPOL25 .eq. 0 
     &                     .and. KRAPOL25 .eq. 0 ) return ! --------------- (25) 

      IF = KRFPOL25
      IQ = KRQPOL25

      call check for an infeasibly large river flow ! ===================== (25)
      call check for nonparametric distribution ! ========================= (25)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off river based diffuse pollution from livestock ! -- (26) 
      include 'COMMON DATA.FOR'

      KRFPOL25 = 0
      KRQPOL25 = 0
      KRAPOL25 = 0
      
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from livestock ... 25'/77('='))
      endif

      call check for an infeasibly large river flow 
      
      return
      end





      subroutine turn on river based diffuse pollution from arable ! ------ (27)
      include 'COMMON DATA.FOR'
      
      add conc = 0 !  control on the addition of concentration without flow (27)

      if ( ical13 .eq. 0 ) then ! ----------------------------------------- (27)
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (27)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 27',
     &' - arable)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(//77('=')/
     &'Start of diffuse pollution (river-type ... 27',
     &' - arable)'/77('#')/
     &'Added as a step up in concentration or load ', ! ------------------- (27)
     &'but adding no further flows'/77('#'))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif ! if ( ical13 .eq. 0 ) ---------------------------------------- (27)

*     initialise the mass balance variables ------------------------ arable (27)
      call initialise data for mass balance ! ---------------------- arable (27)

      KRFPOL27 = JF(KFEAT)
      KRQPOL27 = JQ(KFEAT)
      KRAPOL27 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (27)

      call check the flow data (KRFPOL27,KRQPOL27,KRAPOL27) ! ------------- (27)
      if ( KRFPOL27 .eq. 0 .and. KRQPOL27 .eq. 0 
     &                     .and. KRAPOL27 .eq. 0 ) return ! --------------- (27) 
	  
      call check the quality data (KRFPOL27,KRQPOL27,KRAPOL27) ! ---------- (27)
      if ( KRFPOL27 .eq. 0 .and. KRQPOL27 .eq. 0 
     &                     .and. KRAPOL27 .eq. 0 ) return ! --------------- (27) 

      IF = KRFPOL27
      IQ = KRQPOL27
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (27)

      IFDIST = PDRF(IF)
*     call write data for graph plotting
      
      return
      end



      subroutine turn off river based diffuse pollution from arable ! ----- (28)
      include 'COMMON DATA.FOR'

      KRFPOL27 = 0
      KRQPOL27 = 0
      KRAPOL27 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from arable land ... 27'/77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


      subroutine turn on river based diffuse pollution from highways ! ---- (29)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (29)     
      
      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (29)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 29',
     &' - highway runoff)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 29',
     &' - highway runoff)'/
     &'Added as a step up in concentration or load ', ! ------------------- (29)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

      call initialise data for mass balance ! -------------- highway runoff (29)

      KRFPOL29 = JF(KFEAT)
      KRQPOL29 = JQ(KFEAT)
      KRAPOL29 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (29)

      call check the flow data (KRFPOL29,KRQPOL29,KRAPOL29) ! ------------- (29)
      if ( KRFPOL29 .eq. 0 .and. KRQPOL29 .eq. 0 
     &                     .and. KRAPOL29 .eq. 0 ) return ! --------------- (29) 
	  
      call check the quality data (KRFPOL29,KRQPOL29,KRAPOL29) ! ---------- (29)
      if ( KRFPOL29 .eq. 0 .and. KRQPOL29 .eq. 0 
     &                     .and. KRAPOL29 .eq. 0 ) return ! --------------- (29) 

      IF = KRFPOL29
      IQ = KRQPOL29
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (29)

      IFDIST = PDRF(IF)
*     call write data for graph plotting ! ================================ (29)

      return
      end



      subroutine turn off river based diffuse pollution from highways ! --- (30) 
      include 'COMMON DATA.FOR'

      KRFPOL29 = 0
      KRQPOL29 = 0
      KRAPOL29 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from highways ... 29'/77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


      subroutine turn on river based diffuse pollution from urban ! ------- (31)
      include 'COMMON DATA.FOR'
      
      add conc = 0 ! ------------------------------------------------------ (31)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then ! --------------------------- (31)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 31',
     &' - urban runoff)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 31',
     &' - urban runoff)'/
     &'Added as a step up in concentration or load ', ! ------------------- (31)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! ---------------- urban runoff (31)

      KRFPOL31 = JF(KFEAT)
      KRQPOL31 = JQ(KFEAT)
      KRAPOL31 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! no flow added ----- (31)

      call check the flow data (KRFPOL31,KRQPOL31,KRAPOL31) ! ------------- (31)
      if ( KRFPOL31 .eq. 0 .and. KRQPOL31 .eq. 0 
     &                     .and. KRAPOL31 .eq. 0 ) return ! --------------- (31) 
  
      call check the quality data (KRFPOL31,KRQPOL31,KRAPOL31) ! ---------- (31)
      if ( KRFPOL31 .eq. 0 .and. KRQPOL31 .eq. 0 
     &                     .and. KRAPOL31 .eq. 0 ) return ! --------------- (31) 

      IF = KRFPOL31
      IQ = KRQPOL31
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (31)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off river based diffuse pollution from urban ! ------ (32) 
      include 'COMMON DATA.FOR'

      KRFPOL31 = 0
      KRQPOL31 = 0
      KRAPOL31 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from urban runoff ... 31'/77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end

      
      subroutine turn on diffuse pollution from the atmosphere ! ---------- (33)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (33)      
      
      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (33)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 33',
     &' - atmospheric deposition)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 33',
     &' - atmospheric deposition)'/
     &'Added as a step up in concentration or load ', ! ------------------- (33)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------- (33)
      call initialise data for mass balance ! ------ atmospheric deposition (33)

      KRFPOL33 = JF(KFEAT)
      KRQPOL33 = JQ(KFEAT)
      KRAPOL33 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1

      call check the flow data (KRFPOL33,KRQPOL33,KRAPOL33) ! ------------- (33)
      if ( KRFPOL33 .eq. 0 .and. KRQPOL33 .eq. 0 
     &                     .and. KRAPOL33 .eq. 0 ) return ! --------------- (33) 
  
      call check the quality data (KRFPOL33,KRQPOL33,KRAPOL33) ! ---------- (33)
      if ( KRFPOL33 .eq. 0 .and. KRQPOL33 .eq. 0 
     &                     .and. KRAPOL33 .eq. 0 ) return ! --------------- (33) 

      IF = KRFPOL33
      IQ = KRQPOL33
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (33)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from the atmosphere ! --------- (34)
      include 'COMMON DATA.FOR'

      KRFPOL33 = 0
      KRQPOL33 = 0
      KRAPOL33 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from the atmosphere ... 33'/77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


*     calculations for features - natural background  --------------------- (35)
      subroutine turn on diffuse pollution from background ! -------------- (35)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (35)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (35)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 35',
     &' - natural background)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 35',
     &' - natural background)'/
     &'Added as a step up in concentration or load ', ! ------------------- (35)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------- (35)
      call initialise data for mass balance ! ---------- natural background (35)

      KRFPOL35 = JF(KFEAT)
      KRQPOL35 = JQ(KFEAT)
      KRAPOL35 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (35)

      call check the flow data (KRFPOL35,KRQPOL35,KRAPOL35) ! ------------- (35)
      if ( KRFPOL35 .eq. 0 .and. KRQPOL35 .eq. 0 
     &                     .and. KRAPOL35 .eq. 0 ) return ! --------------- (35) 
	  
      call check the quality data (KRFPOL35,KRQPOL35,KRAPOL35) ! ---------- (35)
      if ( KRFPOL35 .eq. 0 .and. KRQPOL35 .eq. 0 
     &                     .and. KRAPOL35 .eq. 0 ) return ! --------------- (35) 

      IF = KRFPOL35
      IQ = KRQPOL35
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (35)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from background ! ------------- (36)
      include 'COMMON DATA.FOR'

      KRFPOL35 = 0
      KRQPOL35 = 0
      KRAPOL35 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from natural background ... 35'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1709) ! ---------------------- Di.OUT
 1709 format(//77('=')/
     &'End of diffuse input from natural background ... 35'/77('='))
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


*     calculations for features - diffuse mines --------------------------- (46)
      subroutine turn on diffuse pollution from mines ! ------------------- (46)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (46)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (46)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 46',
     &' - mines)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 46',
     &' - mines)'/
     &'Added as a step up in concentration or load ', ! ------------------- (46)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! --------------- diffuse mines (46)

      KRFPOL46 = JF(KFEAT)
      KRQPOL46 = JQ(KFEAT)
      KRAPOL46 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (46)

      call check the flow data (KRFPOL46,KRQPOL46,KRAPOL46) ! ------------- (46)
      if ( KRFPOL46 .eq. 0 .and. KRQPOL46 .eq. 0 
     &                     .and. KRAPOL46 .eq. 0 ) return ! --------------- (46) 
      
      call check the quality data (KRFPOL46,KRQPOL46,KRAPOL46) ! ---------- (46)
      if ( KRFPOL46 .eq. 0 .and. KRQPOL46 .eq. 0 
     &                     .and. KRAPOL46 .eq. 0 ) return ! --------------- (46) 

      IF = KRFPOL46
      IQ = KRQPOL46
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (46)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from mines ! ------------------ (47)
      include 'COMMON DATA.FOR'

      KRFPOL46 = 0
      KRQPOL46 = 0
      KRAPOL46 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from mines ... 46'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009) ! ---------------------- Di.OUT
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


*     calculations for features - birds, boats and angling ---------------- (48)
      subroutine turn on diffuse pollution from birds ! ------------------- (48)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (48)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (48)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 48',
     &' - birds, boats and angling)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 48',
     &' - birds, boats and angling)'/
     &'Added as a step up in concentration or load ', ! ------------------- (48)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! ---- birds, boats and angling (48)

      KRFPOL48 = JF(KFEAT)
      KRQPOL48 = JQ(KFEAT)
      KRAPOL48 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (48)

      call check the flow data (KRFPOL48,KRQPOL48,KRAPOL48) ! ------------- (48)
      if ( KRFPOL48 .eq. 0 .and. KRQPOL48 .eq. 0 
     &                     .and. KRAPOL48 .eq. 0 ) return ! --------------- (48) 
  
      call check the quality data (KRFPOL48,KRQPOL48,KRAPOL48) ! ---------- (48)
      if ( KRFPOL48 .eq. 0 .and. KRQPOL48 .eq. 0 
     &                     .and. KRAPOL48 .eq. 0 ) return ! --------------- (48) 

      IF = KRFPOL48
      IQ = KRQPOL48
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (48)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from birds ! ------------------ (49)
      include 'COMMON DATA.FOR'

      KRFPOL48 = 0
      KRQPOL48 = 0
      KRAPOL48 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from birds, boats and angling ... 48'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009) ! ---------------------- Di.OUT
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end



      subroutine turn on diffuse pollution from septic tanks ! ------------ (37)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (37)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (37)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 37',
     &' - septic tanks)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 37',
     &' - septic tanks)'/
     &'Added as a step up in concentration or load ', ! ------------------- (37)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! ---------------- septic tanks (37)

      KRFPOL37 = JF(KFEAT)
      KRQPOL37 = JQ(KFEAT)
      KRAPOL37 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (37)

      call check the flow data (KRFPOL37,KRQPOL37,KRAPOL37) ! ------------- (37)
      if ( KRFPOL37 .eq. 0 .and. KRQPOL37 .eq. 0 
     &                     .and. KRAPOL37 .eq. 0 ) return ! --------------- (37) 
  
      call check the quality data (KRFPOL37,KRQPOL37,KRAPOL37) ! ---------- (37)
      if ( KRFPOL37 .eq. 0 .and. KRQPOL37 .eq. 0 
     &                     .and. KRAPOL37 .eq. 0 ) return ! --------------- (37) 

      IF = KRFPOL37
      IQ = KRQPOL37
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (37)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off diffuse pollution from septic tanks ! ----------- (38)
      include 'COMMON DATA.FOR'

      KRFPOL37 = 0
      KRQPOL37 = 0
      KRAPOL37 = 0
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from septic tanks ... 37'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009) ! ---------------------- Di.OUT
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


*     calculations for features - aggregated CSOs ------------------------- (40)
      subroutine turn on diffuse pollution from aggregated CSOs ! --------- (40)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (40)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (40)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 40',
     &' - aggregated CSOs)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 40',
     &' - aggregated CSOs)'/
     &'Added as a step up in concentration or load ', ! ------------------- (40)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009) ! ---------------------------------------------- Di.OUT
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! ------------- aggregated CSOs (40)

      KRFPOL40 = JF(KFEAT)
      KRQPOL40 = JQ(KFEAT)
      KRAPOL40 = add diffconc(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (40)

      call check the flow data (KRFPOL40,KRQPOL40,KRAPOL40) ! ------------- (40)
      if ( KRFPOL40 .eq. 0 .and. KRQPOL40 .eq. 0 
     &                     .and. KRAPOL40 .eq. 0 ) return ! --------------- (40) 
	  
      call check the quality data (KRFPOL40,KRQPOL40,KRAPOL40) ! ---------- (40)
      if ( KRFPOL40 .eq. 0 .and. KRQPOL40 .eq. 0 
     &                     .and. KRAPOL40 .eq. 0 ) return ! --------------- (40) 

      IF = KRFPOL40
      IQ = KRQPOL40
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (40)

      IFDIST = PDRF(IF)
*     call write data for graph plotting ! ================================ (40)

      return
      end


      subroutine turn off diffuse pollution from aggregated CSOs ! -------- (41)
      include 'COMMON DATA.FOR'

      KRFPOL40 = 0
      KRQPOL40 = 0
      KRAPOL40 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(77('=')/
     &'End of diffuse pollution from aggregated CSOs ... 40'/
     &77('='))
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end


*     calculations for features - diffuse effluent discharges ------------- (15)
      subroutine turn on discharge based diffuse pollution ! -------------- (15)
      include 'COMMON DATA.FOR'

      call write message that diffuse discharge pollution has started
      KEPOL15 = JQ(KFEAT)
      if ( DISTP .lt. 1.0E-08 ) return ! discharge based diifuse pollution -----

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! discharge based diffuse pollution

      if (FLOW(1) .gt. 1.0E9) then
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow more than a billion ',33x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .4.'/
     &'*** Calculations proceeding ...'/100('*'))
      endif

      IQ = KEPOL15

      if ( IQ .eq.  0 ) return

      if ( pollution data(IQ,1,1)+0.001 )42,42,51
   42 if ( nobigout .le. 0 ) write(01,12)
   12 format(
     &'---------------------------------------------------------'/
     &'Quality data specified for diffuse pollution do not exist'/
     &'---------------------------------------------------------')
      return
   51 continue

      return
      end


*     switch off for diffuse pollution (effluent type) ---------------------(16)
      subroutine turn off discharge based diffuse pollution ! ------------- (16)
      include 'COMMON DATA.FOR'

      KEPOL15 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      endif
 1009 format(77('=')/
     &'End of diffuse pollution (discharge type - 15) ...'/
     &77('='))
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of diffuse pollution...'/77('-'))

*     call write out loads and quality (0) ! u/s of end of diffuse pollution
  970 continue

      if (FLOW(1) .lt. 1.0E9) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow more than a billion ',33x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .5.'/
     &'*** Calculations proceeding ...'/100('*'))
      end


*     calculations for features - aggregated STWs ------------------------- (42)
      subroutine turn on diffuse pollution from aggregated STWs ! --------- (42)
      include 'COMMON DATA.FOR'

      KEPOL42 = JQ(KFEAT)
      if ( DISTP .lt. 1.0E-08 ) return

*     initialise the mass balance variables ------------------------------- (42)
      call initialise data for mass balance ! aggregated STWs ------------- (42)

      if ( FLOW(1) .gt. 1.0E9 ) then
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow more than a billion ',33x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .9.'/
     &'*** Calculations proceeding ...'/100('*'))
      endif

      IQ = KEPOL42

      if ( IQ .eq.  0 ) return

      if ( pollution data(IQ,1,1)+0.001 )42,42,51
   42 if ( nobigout .le. 0 ) write(01,12)
   12 format(
     &'---------------------------------------------------------'/
     &'Quality data specified for aggregated STWs do not exist'/
     &'---------------------------------------------------------')
      return
   51 continue

      return
      end



*     switch off for diffuse pollution from aggregated STWs --------------- (43)
      subroutine turn off diffuse pollution from aggregated STWs ! -------- (43)
      include 'COMMON DATA.FOR'

      KEPOL42 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
      endif
 1009 format(77('=')/
     &'End of aggregated STWs (discharge type - 43) ... '/
     &77('='))
      if ( JSKIP .eq. 1 .or. IPRINT .eq. 1 .or. ICAL .eq. 1 ) goto 970
*     if ( nobigout .le. 0 ) write(01,1010)
 1010 format(/77('-')/'Calculated river quality immediately',
     &' upstream of end of aggregated STWs ...'/77('-'))

*     call write out loads and quality (0) ! upstream of aggregated STWs
  970 continue

      if (FLOW(1) .lt. 1.0E9) return
      write(01,7817)UNAME(KFEAT)
      if ( kerror .eq. 1 ) then
      if ( suppress11 .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,7877)UNAME(KFEAT)
 7877 format('*** Flow more than a billion ',33x,'at ',a40,1x,25('.'))
      call set screen text colour
      endif
      suppress11 = suppress11 + 1
      endif
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &A40/'*** It looks like an error has occurred .6.'/
     &'*** Calculations proceeding ...'/100('*'))
      end


      
*     diffuse sources of pollution (13) (25) (27) etc --------------------------
      subroutine stream diffuse
      include 'COMMON DATA.FOR'
      character *10 BB1(MP10)
      character * 10 Tenchars(MP10) 
      
      fupmx = Flow(1) ! stream diffuse

*     initialise the mass balance variables ------------------------------------
*     call initialise data for mass balance ! stream diffuse
      if ( DINT .lt. 1.0E-8 ) return ! check for xero distance -----------------

      IFDIST = PDRF(IF) ! set the distribution for added flow ------------------
      EFM = F(IF,1) !  mean diffuse flow added to river ------------------------
      EF5 = F(IF,2) ! corresponding 95-percentile low flow ---------------------
      EF3 = 0.0 ! shift flow ---------------------------------------------------
      if ( PDRF(IF) .eq. 3 ) EF3 = F(IF,3) ! shift flow ------------------------

      if ( EF5 .gt. 1.0e-08 .and. EF5 .ge. EFM ) then
      suppress10 = suppress10 + 1
      call sort format 2 (EFM,EF5)
      write(01,8151)valchars11,valchars10,IF
      write(33,8151)valchars11,valchars10,IF
 8151 format(77('-')/'*** Illegal data for river flow ... ',
     &'the 95-percentile low flow of ',a10/
     &'*** exceeds the mean of ',a10,3x,'Check the set of flow ',
     &'data number',i6)
      write(01,8152)
      write(33,8152)
 8152 format('*** The 95-percentile low flow was set to 25% ',
     &'of the mean ... '/77('-'))
      if ( kerror .eq. 1 ) then
      call change colour of text (37) ! dull turquoise
      write( *,8157)uname(Kfeet),IF
 8157 format('*** Strange 95-percentile flow set to 25% of ',
     &'mean  ... ',6x,a37,7x,'Set number',i7,' .......')
      call set screen text colour
      endif
      F(IF,2) = 0.25 * F(IF,1) 
      EF5 = 0.25 * EFM
      endif
      
      if ( EFM .gt. 1.0e-08 .and. EF5 .lt. 1.0e-08 .and. ! =====================
     &     IFDIST .ne. 4 ) then ! ==============================================
      call set format for printout (BB1(1),EFM)      
      write(01,8951)BB1(1),FUNIT,IF,IFDIST
      write(33,8951)BB1(1),FUNIT,IF,IFDIST
 8951 format(77('-')/
     &'*** Zero entered for the 95-percentile low flow ... ',
     &4x,'Mean =',a10,1x,a4/
     &'*** Check the set of flow data number',2i7)
      write(01,8952)
      write(33,8952)
 8952 format(77('-')/
     &'*** Zero 95-percentile low flow set to 1% of ',
     &'the mean ... '/77('-'))
      suppress6 = suppress6 + 1
      if ( suppress6 .lt. 4 .and. kerror .eq. 1 ) then
      call change colour of text (37) ! dull turquoise
      write( *,8956)uname(Kfeet),IF
 8956 format('*** Zero 95-percentile flow set to 1%',
     &' of the mean',2x,'...',7x,a37,7x,
     &'Set number',i7,' .......')
      call set screen text colour
      endif
      if ( suppress6 .eq. 4 .and. kerror .eq. 1 ) then
      call change colour of text (37) ! dull turquoise
      write( *,8156)uname(Kfeet),IF
 8156 format('*** Zero 95-percentile flow set to 1%',
     &' of the mean',2x,'...',13x,a28,3x,'Set number',i7,
     &' Check .ERR for others ...')
      call set screen text colour
      endif
      F(IF,2) = 0.01 * F(IF,1) 
      EF5 = 0.01 * EFM
      endif ! ================================================================== 
      
*     log-normal distributions of diffuse flow ---------------------------------
*     compute mean and standard deviation of logged flows from the mean and ----
*     95-percentile entered as data --------------------------------------------
      if ( EFM .ge. 1.0E-10 ) then    
      if ( PDRF(IF) .eq. 2 .or. PDRF(IF) .eq. 3 ) then
      TEST = AMAX1 ( (EF5+EF3), 1.0E-8 )
      GEFStest = 2.7057 + 2.0 * ALOG((EFM+EF3)/TEST)
      if ( GEFStest .gt. 0.0 ) goto 48
   49 continue
      write(01,9099)IF,EFM,EF5
 9099 format(' ### Flow error for diffuse inflow ... '/
     &' ### Simulation stopped in STREAM DIFFUSE ... ',i6,2f12.5)
      write( *,9099)IF,EFM,EFS
      write(09,9099)IF,EFM,EFS
      write(33,9099)IF,EFM,EFS
      call stop
   48 continue
      GEFS = SQRoot(1026,GEFStest) - 1.6449 ! standard deviation (log)
      EFStest = EXP(GEFS*GEFS) - 1.0
      if (EFStest .lt. 1.0E-10) goto 49
      EFS = EFM*SQRoot(1027,EFStest) ! standard deviation
      GEFM = ALOG (EFM+EF3) - 0.5 * GEFS * GEFS
      EM3 = EFM
      endif
      endif ! if ( EFM .ge. 1.0E-10 ) then

*     normal distribution of diffuse flow --------------------------------------
      if ( PDRF(IF) .eq. 1 ) then
      GEFM = EFM
      GEFS = (EFM-EF5)/1.6449
      EFS = GEFS
      endif ! normal distribution of diffuse flow

*     constant diffuse flow ----------------------------------------------------
      if ( PDRF(IF) .eq. 0 ) then
      GEFM = EFM
      GEFS = 0.0
      EFS=GEFS
      endif ! constant diffuse flow

*     non-parametric distributions of diffuse flow -----------------------------
*     if ( PDRF(IF) .eq. 4 ) then
*     write(33,3211)
*3211 format(
*    &33x,'A non-parametric distribution of diffuse inflow is used')
*     endif ! non-parametric distributions of diffuse flow

*     monthly distributions of river flow --------------------------------------
*     if ( PDRF(IF) .eq. 5 ) then
*     write(33,3231)
*3231 format(33x,'Monthly data on diffuse inflow are used ...')
*     endif

*     monthly structure for river flow -----------------------------------------
*     if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flow
*     write(33,3631)
*3631 format(33x,'The monthly structure on diffuse inflow was ',
*    &'used ...')
*     endif

*     loop on the determinands -------------------------------------------------
      idfcheck = 0
      jhead = 0

      do 6957 JP = 1, ndet
      if (QTYPE (JP) .EQ. 4) goto 6957

      call inititialise the stores for the estimates of load
      
      idfcheck = idfcheck + 1

      IQDIST = PDRC(IQ,JP) ! type of distribution --------------- STREAM DIFFUSE
      
      ECM = quolity data(IQ,JP,1) ! mean
      ECS = quolity data(IQ,JP,2) ! standard deviation
      if ( PDRC (IQ,JP) .EQ. 11) ECS = quolity data(IQ,JP,2) !standard deviation 
      if ( PDRC (IQ,JP) .EQ. 10) ECS = quolity data(IQ,JP,2) !standard deviation 
      
      EC3 = 0.0 ! shift
      if ( PDRC (IQ,JP) .EQ. 3 ) EC3 = quolity data(IQ,JP,3) ! ----------- shift
      if ( PDRC (IQ,JP) .EQ. 7 ) EC3 = quolity data(IQ,JP,3) ! -- scaling factor
      if ( PDRC (IQ,JP) .EQ. 11) EC3 = quolity data(IQ,JP,3) ! ----- power index 
      if ( PDRC (IQ,JP) .EQ. 10) EC3 = quolity data(IQ,JP,3) ! ----- power index 

      CO5 = RFCL(JP) ! default for added flow on added quality -----------------
      if ( quolity data (IQ,JP,MO) .ne. -9.9 ) then
      CO5 = quolity data (IQ,JP,MO) ! added flow on added quality --------------
      endif ! if ( quolity data (IQ,JP,MO) .ne. -9.9 ) -------------------------
      
      CO2 = 1.0 ! default correlation of added flow the main river flow --------
      Ctemp = CO2 ! save this for future use after "mass balance" --------------
*     check for a special correlation coefficient with main river flow ---------
      if ( F (IF,MO) .ne. -9.9 ) CO2 = F (IF,MO)

*     do the mass balance for this determinand ---------------------------------
*     set the "ifdiffuse" to "1" for diffuse pollution -------------------------
      ifdiffuse = 1 ! river type

      idnum = 0
      if ( diffuse type .eq. 25 ) idnum = 6  ! ----- from livestock farming (25)
      if ( diffuse type .eq. 27 ) idnum = 7  ! -------- from arable farming (27)
      if ( diffuse type .eq. 29 ) idnum = 8  ! -------- from highway runoff (29)
      if ( diffuse type .eq. 31 ) idnum = 9  !----------- from urban runoff (31)
      if ( diffuse type .eq. 33 ) idnum = 10 !  from atmospheric deposition (33)
      if ( diffuse type .eq. 35 ) idnum = 11 ! ---- from natural background (35)
      if ( diffuse type .eq. 37 ) idnum = 12 ! ---------- from septic tanks (37)
      if ( diffuse type .eq. 40 ) idnum = 13 ! ------- from aggregated CSOs (40)
      if ( diffuse type .eq. 42 ) idnum = 14 ! ------- from aggregated STWs (42)
      if ( diffuse type .eq. 46 ) idnum = 15 ! --------- from diffuse mines (46)
      if ( diffuse type .eq. 48 ) idnum = 16 ! --- birds, boats and angling (48)
      if ( diffuse type .eq. 13 ) idnum = 18 ! ------------- diffuse inflow (13)
      if ( diffuse type .eq. 15 ) idnum = 19 ! ------------- diffuse inflow (15)
      if ( diffuse type .eq. 50 ) idnum = 23 ! ----------------- user-named (50)
      if ( diffuse type .eq. 52 ) idnum = 24 ! ----------------- user-named (52)
      if ( diffuse type .eq. 54 ) idnum = 25 ! ----------------- user-named (54)
      if ( diffuse type .eq. 56 ) idnum = 26 ! ----------------- user-named (56)
      if ( diffuse type .eq. 58 ) idnum = 27 ! ----------------- user-named (58)
      usCMcont = CMcontrib(idnum,JP,1) ! ---------- the upstream value for idnum
      usCMcontNTF = CMcontrib(NTF,JP,1) ! --- store the upstream value for total

      call mass balance ! ----------- from SR STREAM DIFFUSE for 13,25,27,50 etc

      call update summaries of contribution ! diffuse inputs: 13,25,27,35,50 etc

      !call check the correlation by regression (6) ! from diffuse -
      call accumulate the loads ! from all stream diffuse 13, 25, 27 etc -------

      ifdiffuse = 0
      CO2 = Ctemp ! restore original value of correlation coefficient ----------

      xnum = QNUM(IQ,JP)
      
      if ( JSKIP .eq. 0 .and. qtype (JP) .ne. 4 ) then ! =======================
      
      xqualn = QUALN(JP)
      call get sampling rates for river quality ! all diffuse (25, 27, etc) ---N
     &(fupmx,C(JP,1),QUALN(JP),dint*EFM,ECM,xnum,IQDIST,0)
      
      xqualn = QUALNB(JP,idnum)
      call get sampling rates for river quality ! all diffuse (25, 27, etc) ---B
     &(fupmx,usCMcont,QUALNB(JP,idnum),dint*EFM,ECM,xnum,IQDIST,1)
      
      idnum2 = NTF
      xqualn = QUALNB(JP,idnum2)
      call get sampling rates for river quality ! total diffuse -------------- B
     &(fupmx,usCMcontNTF,QUALNB(JP,idnum2),dint*EFM,ECM,xnum,IQDIST,1)
      
      xxCMcont = 0.0
      xxCScont = 0.0
      do IS = 1,NS
      xxCMcont = xxCMcont + CTMS(idnum2,JP,IS)
      xxCScont = xxCScont + CTMS(idnum2,JP,IS)*CTMS(idnum2,JP,IS)   
      enddo
      CMcontrib(idnum2,JP,1) = xxCMcont/float(NS)
      CMcontrib(idnum2,JP,2) = xxCMcont/float(NS)
      
      endif ! if ( JSKIP .eq. 0 .and. qtype (JP) .ne. 4 ) ======================
      
      call calculate summaries of river flow ! stream diffuse ------------------
      call get summaries of river quality from the shots ! stream diffuse ------

 6957 continue ! loop on the determinands ======================================
     
      call accumulate total loads from diffuse sources ! =======================
      call add up all the loads ! for all determinands
*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! stream diffuse -----------------------------------
*     call update summaries of contribution ! -------------- from stream diffuse

*     write out the river loads after diffuse sources --------------------------
*     if ( ical13 .eq. 0 )  then
*     call write out the river loads after diffuse sources ! from stream diffuse
*     endif
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then 
      xprint = 0.0
      do idet = 1, ndet
      BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)diffuse type,(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse run-off',i3,8X,10A10)
*     endif
      endif
      endif

      do jdet = 1, ndet
      check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
      enddo

      call format the loads for output 4 (Tenchars) 
      
      return
      end



*     compute effect of diffuse pollution --------------------------------------
*     feature type (15/16) ... discharge type ----------------------------------
      subroutine effluent diffuse 15 ! ------------------------------------ (15)
      include 'COMMON DATA.FOR'
      character *10 BB1(MP10)
      character *10 Tenchars (13)

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! effluent diffuse 15
      
      fupmx = flow(1) ! effluent diffuse
      
      if (DINT .lt. 1.0E-8) return

      IQ = KEPOL15 ! data-set --------------------------------------------------
      if ( IQ .lt. 1 ) return

*     check for zero mean flow -------------------------------------------------
*     if ( FE(IQ,1) .lt. 1.0E-8 ) return

*     prepare for mass balance -------------------------------------------------
      IFDIST = PDEF(IQ)

*     flow data ----------------------------------------------------------------
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
      EF3 = 0.0
      if (PDEF(IQ) .eq. 3) EF3 = FE(IQ,3)

*     correlation of discharge flow on river flow ------------------------------
      CO2 = 0.6 ! effluent diffuse
*     special non-default correlation ------------------------------------------
      if (FE(IQ,4) .ne. -9.9) CO2 = FE (IQ,4)

*     ==========================================================================
*     non-parametric distributions of discharge flow ---------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 4) then
*     write(01,3211)
*     write(09,3211)
*     write(33,3211)
*3211 format(
*    &33x,'A non-parametric distribution of diffuse inflow is used')
*     endif
*     ==========================================================================

*     ==========================================================================
*     Monthly distributions of discharge flow ----------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 5) then
*     write(01,3231)
*     write(09,3231)
*     write(33,3231)
*3231 format(33x,'Monthly data on diffuse inflow are used ...')
*     endif
*     ==========================================================================

*     ==========================================================================
*     monthly structure for discharge flow -------------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 8) then ! monthly structure for river flow
*     if (nobigout .le. 0) write(01,3631)
*     write(09,3631)
*     write(33,3631)
*3631 format(33x,'The monthly structure on diffuse inflow was ',
*    &'used ...')
*     endif
*     ==========================================================================

*     loop on determinands -----------------------------------------------------
      MFIRST=0
      jhead = 0

      do 6930 JP = 1, ndet
      if ( qtype (JP) .eq. 4 ) goto 6930

      IQDIST = PDEC(IQ,JP) ! type of distribution

*     correlation coefficient for discharge flows on river flows ---------------
*     as usual, coefficient refers to the logged variables ---------------------
      Ctemp = CO2

      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)
      xnum = pnum(IQ,JP)

      call inititialise the stores for the estimates of load 

*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
*     discharge quality on discharge flow --------------------------------------
      CO5 = EFCL (JP)
*     special correlation (check this) -----------------------------------------
      if ( pollution data (IQ,JP,MO) .ne. -9.9 ) then ! over-write the default -
      CO5 = pollution data (IQ,JP,MO) ! special correlation --------------------
      endif

*     mass balance -------------------------------------------------------------
      ifdiffuse = 2 ! effluent type of diffuse
      idnum = 19 ! effluent diffuse is the category for apportionment
      
      usCMcont = CMcontrib(idnum,JP,1) ! store the upstream value for idnum
      usCMcontNTF = CMcontrib(NTF,JP,1) ! store the upstream value for total

      call mass balance ! ----------------------------- from effluent diffuse 15
      call accumulate the loads ! --------------------- from effluent diffuse 15

      call get sampling rates for river quality ! all diffuse (25, 27, etc) ---B
     &(fupmx,usCMcont,QUALNB(JP,idnum),dint*EFM,ECM,xnum,IQDIST,1)

      ifdiffuse = 0

      CO2 = CTemp

      call calculate summaries of river flow
      call get summaries of river quality from the shots ! effluent diffuse ----

      xnum = pnum(iq,jp)
      if ( JSKIP .EQ. 0 ) then
      call get sampling rates for river quality ! effluent diffuse (15) --------
     &(fupmx,C(JP,1),QUALN(JP),EFM,ECM,xnum,IQDIST,0)
      endif
 6930 continue

      nx = n13 ! =========================================================== 158
      if ( munthly structure .eq. 0 ) nx = 1 ! no requirement for monthly loads
      do J1 = 1, nx      
*     accumulate total loads from diffuse sources ------------------------------
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t15load1(idet,J1) = t15load1(idet,J1) + diffuse load (idet,J1)
      t15load2(idet,J1) = t15load2(idet,J1) + diffuse load (idet,J1)
      endif
      BB1(idet) = '      ....'
      enddo
      enddo ! do idet = 1, ndet ! ========================================== 158

      call add up all the loads ! for all determinands

      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      xprint = 0.0
      do idet = 1, ndet
      BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse discharge (15)',4X,10A10)
*     endif
      endif
      endif

      do jdet = 1, ndet
      check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
      enddo

      call format the loads for output 4 (Tenchars) 
*     if ( nobigout .le. 0 ) then
*     if ( ical .gt. 3 ) then
*     write(27,4)(Tenchars(jdet),jdet=1,MP10) 
*   4 format('Be   ','Diffuse running total',19x,5x,10A10)
*     endif
*     endif
      
      return
      end

  

    

*     compute effect of diffuse pollution --------------------------------------
*     feature type (42/43) ... discharge type ----------------------------------
      subroutine effluent diffuse 42 ! ------------------------------------ (42)
      include 'COMMON DATA.FOR'
      character *10 BB1(MP10)
      character *10 Tenchars (13)

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance ! effluent diffuse 42

      fupmx = flow(1) ! effluent diffuse 42
      
      if (DINT .lt. 1.0E-8) return

*     data-set -----------------------------------------------------------------
      IQ = KEPOL42
      if ( IQ .lt. 1 ) return

*     check for zero mean flow -------------------------------------------------
*     if ( FE(IQ,1) .lt. 1.0E-8 ) return

*     prepare for mass balance -------------------------------------------------
      IFDIST = PDEF(IQ)

*     flow data ----------------------------------------------------------------
      EFM = FE(IQ,1)
      EFS = FE(IQ,2)
      EF3 = 0.0
      if (PDEF(IQ) .EQ. 3) EF3 = FE(IQ,3)

*     correlation of discharge flow on river flow ------------------------------
      CO2 = 0.6 ! effluent diffuse 42
*     special non-default correlation ------------------------------------------
      if (FE(IQ,4) .ne. -9.9) CO2 = FE (IQ,4)

*     ==========================================================================
*     non-parametric distributions of discharge flow ---------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 4) then
*     write(01,3211)
*     write(09,3211)
*     write(33,3211)
*3211 format(
*    &33x,'A non-parametric distribution of diffuse inflow is used')
*     endif
*     ==========================================================================

*     ==========================================================================
*     Monthly distributions of discharge flow ----------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 5) then
*     write(01,3231)
*     write(09,3231)
*     write(33,3231)
*3231 format(33x,'Monthly data on diffuse inflow are used ...')
*     endif
*     ==========================================================================

*     ==========================================================================
*     monthly structure for discharge flow -------------------------------------
*     --------------------------------------------------------------------------
*     if (PDEF(IQ) .eq. 8) then ! monthly structure for river flow
*     if (nobigout .le. 0) write(01,3631)
*     write(09,3631)
*     write(33,3631)
*3631 format(33x,'The monthly structure on diffuse inflow was ',
*    &'used ...')
*     endif
*     ==========================================================================

*     loop on determinands -----------------------------------------------------
      MFIRST=0
      jhead = 0

      DO 6930 JP = 1, ndet
      if ( qtype (JP) .eq. 4 ) goto 6930

      IQDIST = PDEC(IQ,JP) ! type of distribution

*     correlation coefficient for discharge flows on river flows ---------------
*     as usual, coefficient refers to the logged variables ---------------------
      Ctemp = CO2

      ECM = pollution data(IQ,JP,1)
      ECS = pollution data(IQ,JP,2)
*     shift parameter (for 3-parameter log-normal distribution) ----------------
      EC3 = 0.0
      xnum = pnum(iq,jp)
      
      if ( PDEC(IQ,JP) .eq. 3) EC3 = pollution data(IQ,JP,3)

      call inititialise the stores for the estimates of load 

*     default correlation coefficients -----------------------------------------
*     as usual, the coefficient refers to the logged variables -----------------
      CO5 = EFCL (JP) ! discharge quality on discharge flow
*     special correlation ------------------------------------------------------
      if ( pollution data (IQ,JP,MO) .ne. -9.9 ) then ! over-write the default -
      CO5 = pollution data (IQ,JP,MO) ! special correlation --------------------
      endif

*     mass balance -------------------------------------------------------------
      ifdiffuse = 2 ! effluent type
      
      idnum = 14 ! diffuse aggregated STW is the category for apportionment
      
      usCMcont = CMcontrib(idnum,JP,1) ! store the upstream value for idnum
      usCMcontNTF = CMcontrib(NTF,JP,1) ! store the upstream value for total

      call mass balance ! ------------------- from diffuse effluent discharge 42

      call accumulate the loads ! ----------- from diffuse effluent discharge 42
      
      call get sampling rates for river quality ! all diffuse (25, 27, etc) ---B
     &(fupmx,usCMcont,QUALNB(JP,idnum),dint*EFM,ECM,xnum,IQDIST,1)

      ifdiffuse = 0

      CO2 = CTemp
      call calculate summaries of river flow
      call get summaries of river quality from the shots ! effluent diffuse 42 -

      if ( JSKIP .EQ. 0 ) then
      call get sampling rates for river quality ! effluent diffuse (42) --------
     &(fupmx,C(JP,1),QUALN(JP),EFM,ECM,xnum,IQDIST,0)
      endif
 6930 continue

      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads

*     accumulate total loads from diffuse sources ========================== 158
      do J1 = 1, nx ! ====================================================== 158      

      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t42load1(idet,J1) = t42load1(idet,J1) + diffuse load (idet,J1)
      t42load2(idet,J1) = t42load2(idet,J1) + diffuse load (idet,J1)
      endif
      BB1(idet) = '      ....'
      enddo ! do idet = 1, ndet
      enddo ! do J1 = 1, nx ! ============================================== 158

      call add up all the loads ! for all determinands


      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      xprint = 0.0
      do idet = 1, ndet
      BB1(idet) = '      ....'
      if ( QTYPE (idet) .ne. 4 ) then
      call set format for printout(BB1(idet),diffuse load (idet,i13))
      xprint = xprint + abs ( diffuse load (idet,i13) )
      endif
      enddo
*     if ( xprint .gt. 0.0000001 ) then
*     write(27,8999)(BB1(idet),idet=1,ndet) 
*8999 format(5X,'Loads now added by diffuse discharge (42)',4X,10A10)
*     endif
      endif
      endif
 
      do jdet = 1, ndet
      check sub total (jdet) = check sub total (jdet) 
     &                       + diffuse load (jdet,i13)
      enddo

      call format the loads for output 4 (Tenchars) 
*     if ( nobigout .le. 0 ) then
*     if ( ical .gt. 3 ) then
*     write(27,4)uname(feeture),(Tenchars(jdet),jdet=1,MP10)
*   4 format('0    ',a40,5x,10A10)
*     endif
*     endif
      
      return
      end


      
      
      
*     add in the contribution to reaches from diffuse sources ------------------
      subroutine reach diffuse ! diffuse inflows set with Reach data -----------
      include 'COMMON DATA.FOR'
      dimension Yflow(MS),CMT(MP10,MS),dload(mp10),RLC(n2prop)

      call initialise data for mass balance ! in reach diffuse -----------------

      call calculate summaries of river flow
      
      call get summaries of river quality from the shots ! reach diffuse -------
      
      fupmx = flow(1) ! mean river flow to receive reach diffuse ---------------
      
*     write out the river loads before the diffuse sources ---------------------
      if ( DINT .lt. 1.0E-4 ) then
      if ( ical13 .eq. 0 )  then
      write(01,5682)uname(feeture),DINT
 5682 format(100('~')/'to ... ',a37,' ... a stretch of ',
     &f8.2,' kilometres of river ...'/100('~'))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,5682)uname(feeture),DINT ! --------------------------- Di.EFF
      endif
      enddo
      call write out the river loads before diffuse sources ! ------------------
      endif
      return     
      endif ! if ( DINT .lt. 1.0E-4 ) ------------------------------------------
            
      if ( JT(feeture) .ne. 04 .and. JT(feeture) .ne. 01 .and.
     &     JT(feeture) .ne. 24 .and. JT(feeture) .ne. 09) then
      JP = 1
      call generate monthly structure for stream flows 2
      endif

*     calculate the starting loads and the summary statistics of load ----------
      call load calculation ! before diffuse inflows ---------------------------

*     store the starting loads for the diffuse inflow --------------------------
      do idet = 1, ndet
      dload (idet) = xload (idet,1,i13)
      enddo

*     initialise the added loads ------------------------------------------------------
      do idet = 1, ndet
      addid load (idet) = 0.0 ! added loads ------------------------------------
      enddo

*     pick out the flow data-set for the diffuse inflow ------------------------
      IF = RFDIFF (IREACH)
      if ( IF .eq. 0 ) return 

*     store the current data on flow and quality -------------------------------
      do IS = 1,NS
      Yflow (IS) = FMS (IS) ! store the current river flows --------------------
      FMS (IS) = 0.0 ! initialise the reach diffuse inflows -------------------- 
      do idet = 1, NDET
      CMT (idet,IS) = CMS (idet,IS) ! store the current river river quality ----
      CMS (idet,IS) = 0.0
      enddo
      enddo ! do IS = 1,NS -----------------------------------------------------

      if ( PDRF(IF) .eq. -999 ) then ! +++++++++++++++++++++++++++++++++++++++++
      if ( ifbatch .eq. 0 ) then
      call change colour of text (20) ! bright red
      write( *,2192)RNAME(IREACH),IF
 2192 format('*** No flow data for ',
     &'diffuse inflow to reach ...',2x,a16,
     &' ... the set number is ',i6)
      call set screen text colour
      endif
      write(09,2311)RNAME(IREACH),IF
      write(33,2311)RNAME(IREACH),IF
      write(01,2311)RNAME(IREACH),IF
 2311 format(77('-')/
     &'There are no data on flow ',
     &'for the reach-based diffuse inflow for ... ',a16/
     &'The set number is ...',i6/77('-'))
      RFDIFF (IREACH) = 0
      return
      endif ! if ( PDRF(IF) .eq. -999 ) +++++++++++++++++++++++++++++++++++++++
      
*     quality data-set for reach diffuse inflow +++++++++++++++++++++++++++++++
      if ( RQDIFF (IREACH) .lt. 1 .and. RFDIFF (IREACH) .gt. 0 ) then
      call change colour of text (20) ! bright red
      write( *,5392)RNAME(IREACH),RFDIFF(IREACH)
 5392 format(120('=')/
     &'*** Zero code number for data on water quality for ',
     &'the diffuse inflow to the Reach ... ',a16/
     &'*** This is despite the data on flow having a non-zero code ',
     &'number of',i6/
     &'*** The flow code number is set to zero for this reach ...'/
     &120('='))
      call set screen text colour
      RFDIFF (IREACH) = 0    
      write(09,5392)RNAME(IREACH),RFDIFF(IREACH)
      write(01,5392)RNAME(IREACH),RFDIFF(IREACH)
      write(03,5392)RNAME(IREACH),RFDIFF(IREACH)
      write(27,5392)RNAME(IREACH),RFDIFF(IREACH)
      endif ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     quality data-set for reach diffuse inflow ++++++++++++++++++++++++++++++++
      IQ = RQDIFF (IREACH)
      if ( IQ .lt. 1 ) return
      if ( PDRC(IQ,1) .eq. -1) then ! no data have been entered ++++++++++++++++
      if ( KSIM .eq. 0 ) then
      suppress00 = suppress00 + 1 ! no data on water quality
      call change colour of text (20) ! bright red
      write( *,2392)RNAME(IREACH),IQ
 2392 format('*** No data on water quality for ',
     &'diffuse inflow to reach ... ',3x,a16,
     &' ... the set number is ',i6)
      call set screen text colour
      write(09,2312)RNAME(IREACH),IQ
      write(01,2312)RNAME(IREACH),IQ
      write(33,2312)RNAME(IREACH),IQ
 2312 format(77('-')/
     &'There are no data on water quality ... '/
     &'for the reach-based diffuse inflow for ... ',a16/
     &'The set number is ...',i6/77('-'))
      RQDIFF (IREACH) = 0
      do idet = 1, ndet
      if ( PDRC(IQ,idet) .lt. 0 ) PDRC(IQ,idet) = 0
      quolity data(IQ,idet,1) = 0.0
      quolity data(IQ,idet,2) = 0.0
      quolity data(IQ,idet,3) = 0.0
      enddo
      endif
      endif ! if ( PDRC(IQ,1) .eq. -1) +++++++++++++++++++++++++++++++++++++++++

*     mean flow ----------------------------------------------------------------
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then ! ===========================
      if ( nobigout .le. 0 ) then ! ============================================
      if ( ical13 .eq. 0 ) then ! ==============================================
      call sort format 1 (dint)
      write(01,5282)valchars10
 5282 format(33x,77('~')/33x,'Various diffuse inflows over the next ',
     &a10,' kilometres ...'/33x,77('~'))
      diffuse heading = 1
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,5282)valchars10
      enddo
      write(01,4522)IF ! --------------------------------------------------- OUT
 4522 format(33x,77('+')/33x
     &'Insert the (reach-based) diffuse inflows ... Flow data set:',i5)
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,4522)IF ! -------------------- Di.OUT
      enddo
      call sort format 2 (FLOW(1),FLOW(2))
      write(01,3482)valchars11,valchars10,funit
 3482 format(33x,77('=')/
     &33x,'Starting river flow',16x,'95%tle =',a10,3x,'Mean =',
     &a10,1x,A4)
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,3482)valchars11,valchars10,funit ! ------------------- Di.OUT
      endif
      enddo

      do J = 1, ndet ! ---------------------------------------------------------
      if ( qtype(J) .ne. 4 ) then ! --------------------------------------------
      call sort format 2 (C(J,1),C(J,2))
      write(01,1422)dname(J),valchars11,valchars10,units(J) ! ------ reach-based
 1422 format(33x,'Starting ',A11,15x,'St.dev =',a10,3x, ! ---------- reach-based
     &'Mean =',a10,1x,a4)
      write(200+J,1422)dname(J),valchars11,valchars10,units(J) ! --- reach-based
      endif ! if ( qtype(J) .ne. 4 ) -------------------------------------------
      enddo ! do J = 1, ndet ---------------------------------------------------

      call sort format 3 (DINT*F(IF,1),DINT*F(IF,2),DINT)
      write(01,2182)valchars12,valchars11,valchars10,funit ! ------- reach-based
 2182 format(33x,
     &'Flow added over ',a10,' km',6x,'95%tle =',a10,3x, ! --------- reach-based
     &'Mean =',a10,1x,A4/33x,77('-'))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2182)valchars12,valchars11,valchars10,funit ! --- reach-based
      endif
      enddo
      endif ! if ( ical13 .eq. 0 ) =============================================
      endif ! if ( nobigout .le. 0 ) ===========================================
      endif ! if ( JSKIP .eq. 0 .AND. IPRINT .eq. 0 ) ==========================


*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( ical .eq. 3 .or. ical .eq. 4 ) then ! =========================== GAP
      do jper = 1,ndet ! -------------------------------------------------------
      if ( qtype(jper) .ne. 4 ) then ! -----------------------------------------
      if ( KSIM .eq. 1 ) then ! ------------------------------------------------
      write(170+jper,7562)uname(feeture) ! ------------------------------ Ci.GAP
 7562 format( ! --------------------------------------------------------- Ci.GAP
     &'Iterations for gap filling for WATER QUALITY have reached ', ! --- Ci.GAP
     &'feature ....... ',a37/110('-')) ! -------------------------------- Ci.GAP
      
      if ( IDIFFREACH .eq. 1 .and. ical .ne. 3 ) then ! ----------------- Ci.GAP
      write(170+jper,4562)dint,uname(KFEAT),RFDIFF(IREACH) ! ------------ Ci.GAP
 4562 format(33x,77('-')/ ! --------------------------------------------- Ci.GAP
     &33x,'Insert (reach-based) diffuse inflows over',f6.1,
     &' kilometres ...'/33x,'Up to Feature: ',a40,
     &3x,'Using data set:',i4/33x,77('-'))
      endif ! if ( IDIFFREACH .eq. 1 )
      endif ! if ( KSIM .eq. 1 ) -----------------------------------------------
      endif ! if (qtype(jper) .ne. 4 ) -----------------------------------------
      enddo ! do jper = 1,ndet -------------------------------------------------
      endif ! if ( ical .eq. 3 ) ! ========================================= GAP
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      call generate river flow ! for the diffuse input

      if ( PDRF(IF) .eq. 4 ) then ! a non parametric distribution is used nnnnnn
*     identify the file with the non-parametric data nnnnnnnnnnnnnnnnnnnnnnnnnnn
      do 1969 i = 1, M7
      icod = i
      if ( idenp (1,i,1) .eq. IF ) goto 1970
 1969 continue
      goto 1974
 1970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,7163) flnamesmall(1,icod)
 7163 format(33x,
     &'Non-parametric flow distribution ... File: ',a64/33x,77('-'))
      endif
      endif ! if ( PDRF(IF) .eq. 4 ) nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
 1974 continue ! nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

      
*     note whether the data are monthly mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      if ( PDRF(IF) .eq. 5 ) then
*     identify the file with the monthly data mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      do 2969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2970
 2969 continue
      goto 2974
 2970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,7363) FLMONTHsmall(1,icod)
 7363 format(33x,'Monthly distributions ... File: ',a64)
      endif
      endif ! if ( PDRF(IF) .eq. 5 ) mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
 2974 continue ! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      
*     note whether the data are monthly structure ssssssssssssssssssssssssssssss
      if ( PDRF(IF) .eq. 8 ) then ! monthly structure for river flow sssssssssss
      do 3969 i = 1, M8
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 3970
 3969 continue
      goto 3974
 3970 continue
      if ( ical13 .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,3966) FLSTRUCTsmall(1,icod)
 3966 format(33x,
     &'Monthly structure file (type 8) ... File: ',a64)
      endif
      endif ! if ( PDRF(IF) .eq. 8 ) sssssssssssssssssssssssssssssssssssssssssss
 3974 continue ! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      

      if ( MONF .gt. 1 ) call write shots for diffuse inflows

*     inititialise the stores for the estimates of load ------------------------
      do JP = 1, ndet
      do L13 = 1, N13
      NSM(JP,L13) = 0 ! number of shots per month
      eload(JP,L13) = 0.0 ! -------------------------------------- reach diffuse
      enddo
      enddo ! ------------------------------------------------------------------
    
*     generate the shots of diffuse river quality ---------------- reach diffuse
      do 23 jp = 1, ndet
      if ( Qtype (jp) .eq. 4 ) goto 23
      call generate river quality  ! diffuse river quality
      IQDIST = PDRC(IQ,JP) ! type of distribution - REACH DIFFUSE

      if ( IPRINT .ne. 1 .and. JSKIP .ne. 1 .and. ICAL .ne. 1 ) then
      if ( ical13 .eq. 0 ) then
      if ( jp .eq. ndetlast ) then
      if ( nobigout .le. 0 ) then
      write(01,2094)IQ,IQDIST
 2094 format(33x,'Quality of diffuse inflow: ',
     &'Data Set:',I6,'  Data-type:',i3/33x,77('-'))
      write(200+jp,2094)IQ,IQDIST
      call write mean and standard deviation 33
      endif
      endif
      endif
      endif

*     note whether the river quality distribution is non-parametric ------------
      if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then ! non-parametric ------------
*     identify the file with the non-parametric data ---------------------------
      do 5969 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 5970
 5969 continue
      goto 5974

 5970 continue
      if ( nobigout .le. 0 ) write(01,2965) flnamesmall(2,icod)
 2965 format(33x,'Non-parametric distribution ... File: ',a64)
      endif ! if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) --------------------------
 5974 continue

*     note whether the river quality data are monthly --------------------------
      if ( IQDIST .eq. 5 ) then ! monthly data ---------------------------------
*     identify the file with the monthly data ----------------------------------
      do 4969 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, JP+1 ) .eq. IQ ) goto 4970
 4969 continue
      goto 4974
 4970 continue
      if ( nobigout .le. 0 .and. ical .ne.1 ) then ! ---------------------------
      write(01,4363)FLMONTHsmall(2,icod)
 4363 format(33x,'Monthly data ... File: ',a64)
      endif ! if ( nobigout .le. 0 .and. ical .ne.1 ) --------------------------
      endif ! if ( IQDIST .eq. 5 ) monthly data --------------------------------
 4974 continue

      if ( MONQ .gt. 1) call write shots for quality of diffuse inflows
      !call write shots for quality of diffuse inflows
   23 continue
      
      do 2 IS = 1, NS ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      imonth = qmonth (is) ! set the month for this shot

*     flow added over this stretch ---------------------------------------------
*     unit flow multiplied by length of river ----------------------------------
      FDF = FMS(IS) * DINT ! flow added for this shot --------------------------
*     calculate new flow at end of the stretch ---------------------------------
      FMS(IS) = AMAX1 ( 1.0E-09, FDF + Yflow(IS) )
*     ignore negative flows ----------------------------------------------------
      if ( FMS(IS) .lt. 1.0E-09 ) then
      write( *,*) "negative flows in REACH DIFFUSE ..."
      write(09,*) "negative flows in REACH DIFFUSE ..."
      write(33,*) "negative flows in REACH DIFFUSE ..."
      goto 2
      endif
      
*     the resulting flow has now been calculated and stored in FMS -------------

      do idet = 1, ndet ! loop through determinands ============================
      if ( QTYPE(idet) .ne. 4 ) then ! =========================================
      
      IQDIST = PDRC(IQ,idet) ! type of distribution for basic diffuse quality --
      QDF = CMS(idet,IS) ! quality of the added inflow for this shot -----------
*     initialise the flow added from diffuse inflow ----------------------------
      EFMB = FDF ! the flow added for this shot --------------------------------
*     deal with addition of load but not flow LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
*     check for data expressed as load LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL        
      if ( IQDIST .eq. 6 .or. IQDIST .eq.  7 .or.   ! load LLLLLLLLLLLLLLLLLLLLL
     &     IQDIST .eq. 9 .or. IQDIST .eq. 11 ) then ! load LLLLLLLLLLLLLLLLLLLLL
      QDF = QDF * DINT ! load LLLLLLLLLLLLLLLLLLLLLLL for data expressed as load
      EFMB = 1.0 ! ----  set flow to 1.0 LLLLLLLLLLLL for data expressed as load
      endif ! LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL check for data expressed as load      


*     ==========================================================================
      if ( FMS(IS) .gt. 1.0e-10 ) then ! ========================= reach diffuse
*     calculate the quality after the addition of the reach diffuse ------------        
      CMS(idet,IS) = ((CMT(idet,IS)*Yflow(IS))+(QDF*EFMB))/FMS(IS) ! -----------
*     ==========================================================================
          
*     pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp          
*     starting contributions from sources of diffuse pollution for this shot ppp
      xsum = 0.0
      do ip = 1, n2prop ! loop on all types of diffuse pollution ddddddddddddddd
      RLC(ip) = CTMS(ip, idet, IS) ! u/s level for this shot and source dddddddd
      if ( ip .eq. 20 .or. ip .eq. NTD ) then !for this type of diffuse addition
*     add to the total contributions for this type of diffuse ------------------
      CTMS(ip,idet,IS)=((RLC(ip)*Yflow(IS))+(QDF*EFMB))/FMS(IS) !  reach diffuse
      xsum = xsum + CTMS(ip,idet,IS)
      else ! of if not type 20 dilute the concentartions for others ------------
      CTMS(ip,idet,IS) = RLC(ip) * Yflow(IS) / FMS(IS) ! --------- reach diffuse
      endif ! if ( ip .eq. 20 ) ------------------------------------------------
      enddo ! do ip = 1, n2prop ---------------------------------- reach diffuse
      endif ! if ( FMS(IS) .gt. 1.0e-10 ) ! =====================  reach diffuse

      
*     and accumulate the load --------------------------------------------------
      K13 = imonth + 1
      XLD = QDF * EFMB
      eload(idet,I13) = eload(idet,I13) + XLD ! ------------------ reach diffuse
      eload(idet,K13) = eload(idet,K13) + XLD ! ------------------ reach diffuse
      NSM (idet,I13) = NSM (idet,I13) + 1 ! number of shots per month
      NSM (idet,K13) = NSM (idet,K13) + 1
      addid load(idet) = addid load(idet) + XLD ! ------------------ added loads

      endif ! if ( QTYPE(idet) .ne. 4 ) ========================================
      enddo ! do idet = 1, ndet ================================================
    2 continue ! do 2 IS = 1, NS +++++++++++++++++++++++++++++++++++++++++++++++
       
      do idet = 1, ndet ! ======================================================
      if ( qtype (idet) .ne. 4 ) then
      idnum = 20
      do IS = 1, NS
      YFLOW(IS) = CTMS(idnum,idet,IS) ! concentrations from types of feature ---
      enddo
      
      call calculate summaries of contribution (idnum,idet,YFLOW,0) ! ----- dead
      
      do IS = 1, NS
      YFLOW(IS) = CTMS(ip,idet,IS) ! concentrations from types of feature ------
      enddo
      call calculate monthly summaries of contribution
     &(idnum,idet,YFLOW)
      
      endif ! if ( qtype (idet) .ne. 4 )
      enddo ! idet = 1, ndet ! =================================================
   
*     call update summaries of contribution ! --------------- from reach diffuse

      do JP = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( JSKIP .eq. 0 .and. qtype (JP) .ne. 4 ) then ! +++++++++++++++++++++++
      xnum = QNUM(IQ,JP)
      idnum = 20
      EFM = DINT*F(IF,1)
      ECM = quolity data(IQ,JP,1)
      call get sampling rates for river quality ! reach diffuse ----------------
     &(fupmx,CMcontrib(idnum,JP,1),
     &QUALNB(JP,idnum),EFM,ECM,xnum,IQDIST,1)
      endif ! if ( JSKIP .eq. 0 ) ++++++++++++++++++++++++++++++++++++++++++++++
      enddo ! do JP = 1, ndet ! ++++++++++++++++++++++++++++++++++++++++++++++++

      do idet = 1, ndet ! ======================================================
      if ( qtype (idet) .ne. 4 ) then
      addid load (idet) = addid load (idet) / float (NS) ! --------- added loads
      ELOAD(idet,I13) = ELOAD(idet,I13) / float (NS) ! ----------- reach diffuse
      TDDLOAD1(idet,I13) = TDDLOAD1(idet,I13) + eload(idet,I13) !  reach diffuse
      TDDLOAD2(idet,I13) = TDDLOAD2(idet,I13) + eload(idet,I13) !  reach diffuse 
      if ( munthly structure .eq. 1 ) then ! ----------- store the monthly loads
      do J13 = 2, N13
      if ( NSM(idet,J13) .gt. 0 ) then
      ELOAD(idet,J13) = ELOAD(idet,J13) / float (NSM(idet,J13)) !  reach diffuse
      TDDLOAD1(idet,J13) = TDDLOAD1(idet,J13) + eload(idet,J13) !  reach diffuse
      TDDLOAD2(idet,J13) = TDDLOAD2(idet,J13) + eload(idet,J13) !  reach diffuse 
      endif
      enddo
      endif ! fill monthly loads
      endif ! if ( qtype (idet) .ne. 4 )
      enddo ! idet = 1, ndet ===================================================

      call calculate summaries of river flow ! ------------------- reach diffuse
      call get summaries of river quality from the shots ! ------- reach diffuse
      call write resulting flow and quality ! -------------------- reach diffuse

      do idet = 1, ndet
      if ( JSKIP .eq. 10000 ) then
      if ( QTYPE (idet) .ne. 4 ) then
      xnum = QNUM (iq,idet)
      call get sampling rates for river quality ! ---------------- reach diffuse
     &(fupmx,C(idet,1),QUALN(idet),DINT*F(IF,1),
     &quolity data(IQ,idet,1),xnum,IQDIST,0)
      endif
      endif
      enddo
      
*     calculate loads and the summary statistics of load -----------------------
      call load calculation ! after diffuse inflows ----------------------------
      call add up all the loads ! for all determinands

*     write out the river loads after diffuse sources --------------------------
      if ( ical .eq. 0 .or. ical .gt .3 )  then
      call write out the river loads after diffuse sources
      endif

      return
      end



      subroutine write message that diffuse discharge 
     &pollution has started
      include 'COMMON DATA.FOR'

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (discharge type - 15)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1709)
 1709 format(///77('=')/
     &'Start of diffuse pollution (discharge type - 15)'/77('='))
      endif
      enddo
      endif
      endif

      return
      end


      subroutine check for an infeasibly large river flow
      include 'COMMON DATA.FOR'

      if (FLOW(1) .gt. 1.0E9) then
      if ( nobigout .le. 0 ) write(01,7817)UNAME(KFEAT)
      write(09,7817)UNAME(KFEAT)
      write(33,7817)UNAME(KFEAT)
 7817 format(100('*')/'*** Mean flow exceeds a billion at ',
     &a40/'*** It looks like an error has occurred .6.'/
     &'*** Calculations proceeding ....')
      endif
      return
      end


      subroutine accumulate total loads from diffuse sources ! ============= 158
      include 'COMMON DATA.FOR'

      nx = n13
      if ( munthly structure .eq. 0 ) nx = 1 !  no requirement for monthly loads
      do J1 = 1, nx      
      if ( diffuse type .eq. 13 )  then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t13load1(idet,J1) = t13load1(idet,J1) + diffuse load (idet,J1)
      t13load2(idet,J1) = t13load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 25 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      T25LOAD1(idet,J1) = T25LOAD1(idet,J1) + diffuse load (idet,J1)
      T25LOAD2(idet,J1) = T25LOAD2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 27 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t27load1(idet,J1) = t27load1(idet,J1) + diffuse load (idet,J1)
      t27load2(idet,J1) = t27load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 29 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t29load1(idet,J1) = t29load1(idet,J1) + diffuse load (idet,J1)
      t29load2(idet,J1) = t29load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 31 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t31load1(idet,J1) = t31load1(idet,J1) + diffuse load (idet,J1)
      t31load2(idet,J1) = t31load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 33 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t33load1(idet,J1) = t33load1(idet,J1) + diffuse load (idet,J1)
      t33load2(idet,J1) = t33load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 35 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t35load1(idet,J1) = t35load1(idet,J1) + diffuse load (idet,J1)
      t35load2(idet,J1) = t35load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 46 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t46load1(idet,J1) = t46load1(idet,J1) + diffuse load (idet,J1)
      t46load2(idet,J1) = t46load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
*     --------------------------------------------------------------------------
      if ( diffuse type .eq. 48 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t48load1(idet,J1) = t48load1(idet,J1) + diffuse load (idet,J1)
      t48load2(idet,J1) = t48load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif
      
      if ( diffuse type .eq. 50) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t50load1(idet,J1) = t50load1(idet,J1) + diffuse load (idet,J1)
      t50load2(idet,J1) = t50load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 52) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t52load1(idet,J1) = t52load1(idet,J1) + diffuse load (idet,J1)
      t52load2(idet,J1) = t52load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 54) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t54load1(idet,J1) = t54load1(idet,J1) + diffuse load (idet,J1)
      t54load2(idet,J1) = t54load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 56) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t56load1(idet,J1) = t56load1(idet,J1) + diffuse load (idet,J1)
      t56load2(idet,J1) = t56load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 58) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t58load1(idet,J1) = t58load1(idet,J1) + diffuse load (idet,J1)
      t58load2(idet,J1) = t58load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 37 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t37load1(idet,J1) = t37load1(idet,J1) + diffuse load (idet,J1)
      t37load2(idet,J1) = t37load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 40 ) then
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t40load1(idet,J1) = t40load1(idet,J1) + diffuse load (idet,J1)
      t40load2(idet,J1) = t40load2(idet,J1) + diffuse load (idet,J1)
      endif
      enddo
      endif

      if ( diffuse type .eq. 42 ) then ! aggregated STWs (42)
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      t42load1(idet,J1) = t42load1(idet,J1) + diffuse load (idet,J1)
      t42load2(idet,J1) = t42load2(idet,J1) + diffuse load (idet,J1)
      endif

      enddo
      endif
      enddo

      return
      end ! ================================================================ 158

      
      
      
*     ==========================================================================
*     check whether the distribution is non-parametric =========================
      subroutine check for nonparametric distribution ! ========================
      include 'COMMON DATA.FOR'

      if ( PDRF(IF) .eq. 4 ) then
*     identify the file with the non-parametric data ===========================
      do i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) then
      goto 8970
      endif
      enddo
      write(01,8920)
      write( *,8920)
      write(09,8920)
 8920 format(/'*** Non-parametric flow data specified do not exist.')
      call stop
      endif
 8970 continue
      
      return
      end

      subroutine check the quality data (KRF,KRQ,KRA) ! ------------------------
      include 'COMMON DATA.FOR'
      
      if ( quolity data ( KRQ, 1, 1 ) + 1.0 .le. 0.0 ) then
      if ( nobigout .le. 0 ) write(01,1020)KFEAT
 1020 format(/'*** Quality data specified for feature number',I6,
     &' do not exist ...'/
     &'*** Diffuse pollution was not not switched on ...'/77('-'))
      write(33,1020)KFEAT
      KRF = 0
      KRQ = 0
      KRA = 0
      return
      endif
      
      end

*     check data exist on the diffuse flow to be added -------------------------
      subroutine check the flow data (KRF,KRQ,KRA) ! ---------------------------
      include 'COMMON DATA.FOR'
      
      if ( KRF .lt. 1 ) then 
      if ( add conc .eq. 0 ) then           
      if ( nobigout .le. 0 ) write(01,3644)KFEAT
      do jper = 1, ndet
      if (qtype(jper) .ne. 4 ) write(170+jper,3644)KFEAT
 3644 format(33x,77('-')/
     &33x,'No flow data specified for a diffuse inflow ', 
     &'from livestock ...'/33x,'Check feature number',i6/
     &33x,'Livestock pollution was not switched on ...'/
     &33x,77('-'))
      enddo
      
      KRF = 0
      KRQ = 0
      KRA = 0
      return
      endif ! if ( add conc .eq. 0 ) -------------------------------------------
      endif ! if ( KRF .lt. 1 ) then ! -----------------------------------------
      
      end

      
      
      
*     calculations for features - nameless diffuse pollution -------------- (50)
      subroutine turn on nameless diffuse pollution one ! ----------------- (50)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (50)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (50)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 50',
     &' - nameless diffuse pollution one)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009)
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 50',
     &' - nameless diffuse pollution 1) '/
     &'Added as a step up in concentration or load ', ! ------------------- (50)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009)
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance !  nameless diffuse pollution 1 (50)

      KRFPOL50 = JF(KFEAT)
      KRQPOL50 = JQ(KFEAT)
      KRAPOL50 = add diffconc(KFEAT)
      nameprop(23) = UNAME(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (50)

      call check the flow data (KRFPOL50,KRQPOL50,KRAPOL50) ! ------------- (50)
      if ( KRFPOL50 .eq. 0 .and. KRQPOL50 .eq. 0 
     &                     .and. KRAPOL50 .eq. 0 ) return ! --------------- (50) 
  
      call check the quality data (KRFPOL50,KRQPOL50,KRAPOL50) ! ---------- (50)
      if ( KRFPOL50 .eq. 0 .and. KRQPOL50 .eq. 0 
     &                     .and. KRAPOL50 .eq. 0 ) return ! --------------- (50) 

      IF = KRFPOL50
      IQ = KRQPOL50
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (50)

      IFDIST = PDRF(IF)
      
*     call write data for graph plotting

      return
      end


      subroutine turn off nameless diffuse pollution one ! ---------------- (51)
      include 'COMMON DATA.FOR'

      KRFPOL50 = 0
      KRQPOL50 = 0
      KRAPOL50 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(/77('=')/
     &'End of diffuse input from nameless diffuse pollution 1 ... 50'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009)
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end

      
      
      
*     calculations for features - nameless diffuse pollution 2 ------------ (52)
      subroutine turn on nameless diffuse pollution two ! ----------------- (52)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (52)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (52)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 52',
     &' - nameless diffuse pollution 2)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009)
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 52',
     &' - nameless diffuse pollution 2) '/
     &'Added as a step up in concentration or load ', ! ------------------- (52)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009)
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance !  nameless diffuse pollution 2 (52)

      KRFPOL52 = JF(KFEAT)
      KRQPOL52 = JQ(KFEAT)
      KRAPOL52 = add diffconc(KFEAT)
      nameprop(24) = UNAME(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (52)

      call check the flow data (KRFPOL52,KRQPOL52,KRAPOL52) ! ------------- (52)
      if ( KRFPOL52 .eq. 0 .and. KRQPOL52 .eq. 0 
     &                     .and. KRAPOL52 .eq. 0 ) return ! --------------- (52) 
  
      call check the quality data (KRFPOL52,KRQPOL52,KRAPOL52) ! ---------- (52)
      if ( KRFPOL52 .eq. 0 .and. KRQPOL52 .eq. 0 
     &                     .and. KRAPOL52 .eq. 0 ) return ! --------------- (52) 

      IF = KRFPOL52
      IQ = KRQPOL52
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (52)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off nameless diffuse pollution two ! ---------------- (53)
      include 'COMMON DATA.FOR'

      KRFPOL52 = 0
      KRQPOL52 = 0
      KRAPOL52 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(/77('=')/
     &'End of diffuse input from nameless diffuse pollution 2 ... 52'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009)
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end

      
      
      
      
*     calculations for features - nameless diffuse pollution 3 ------------ (54)
      subroutine turn on nameless diffuse pollution three ! --------------- (54)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (54)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (54)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 54',
     &' - nameless diffuse pollution 3)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009)
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 54',
     &' - nameless diffuse pollution 3) '/
     &'Added as a step up in concentration or load ', ! ------------------- (54)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009)
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance !  nameless diffuse pollution 3 (54)

      KRFPOL54 = JF(KFEAT)
      KRQPOL54 = JQ(KFEAT)
      KRAPOL54 = add diffconc(KFEAT)
      nameprop(25) = UNAME(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (54)

      call check the flow data (KRFPOL54,KRQPOL54,KRAPOL54) ! ------------- (54)
      if ( KRFPOL54 .eq. 0 .and. KRQPOL54 .eq. 0 
     &                     .and. KRAPOL54 .eq. 0 ) return ! --------------- (54) 
  
      call check the quality data (KRFPOL54,KRQPOL54,KRAPOL54) ! ---------- (54)
      if ( KRFPOL54 .eq. 0 .and. KRQPOL54 .eq. 0 
     &                     .and. KRAPOL54 .eq. 0 ) return ! --------------- (54) 

      IF = KRFPOL54
      IQ = KRQPOL54
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (54)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off nameless diffuse pollution three ! -------------- (55)
      include 'COMMON DATA.FOR'

      KRFPOL54 = 0
      KRQPOL54 = 0
      KRAPOL54 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from nameless diffuse pollution 3 ... 54'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009)
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end

      
      
*     calculations for features - nameless diffuse pollution 4 ------------ (56)
      subroutine turn on nameless diffuse pollution four ! ---------------- (56)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (56)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (56)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 56',
     &' - nameless diffuse pollution 4)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009)
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 56',
     &' - nameless diffuse pollution 4) '/
     &'Added as a step up in concentration or load ', ! ------------------- (56)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009)
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance !  nameless diffuse pollution 4 (56)

      KRFPOL56 = JF(KFEAT)
      KRQPOL56 = JQ(KFEAT)
      KRAPOL56 = add diffconc(KFEAT)
      nameprop(26) = UNAME(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! no flow added ----- (56)

      call check the flow data (KRFPOL56,KRQPOL56,KRAPOL56) ! ------------- (56)
      if ( KRFPOL56 .eq. 0 .and. KRQPOL56 .eq. 0 
     &                     .and. KRAPOL56 .eq. 0 ) return ! --------------- (56) 
  
      call check the quality data (KRFPOL56,KRQPOL56,KRAPOL56) ! ---------- (56)
      if ( KRFPOL56 .eq. 0 .and. KRQPOL56 .eq. 0 
     &                     .and. KRAPOL56 .eq. 0 ) return ! --------------- (56) 

      IF = KRFPOL56
      IQ = KRQPOL56
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (56)

      IFDIST = PDRF(IF)
*     call write data for graph plotting

      return
      end


      subroutine turn off nameless diffuse pollution four ! --------------- (57)
      include 'COMMON DATA.FOR'

      KRFPOL56 = 0
      KRQPOL56 = 0
      KRAPOL56 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from nameless diffuse pollution 4 ... 56'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009)
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end

      
      
      
*     calculations for features - nameless diffuse pollution 5 ------------ (58)
      subroutine turn on nameless diffuse pollution five ! ---------------- (58)
      include 'COMMON DATA.FOR'

      add conc = 0 ! ------------------------------------------------------ (58)

      if ( ical13 .eq. 0 ) then
      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      if ( add diffconc (KFEAT) .eq. 0 ) then  ! -------------------------- (58)
      write(01,1009)
 1009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 58',
     &' - nameless diffuse pollution 4)'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,1009)
      endif
      enddo
      else
      write(01,2009)
 2009 format(/77('=')/
     &'Start of diffuse pollution (river-type ... 58',
     &' - nameless diffuse pollution 5) '/
     &'Added as a step up in concentration or load ', ! ------------------- (58)
     &'but adding no further flows'/77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) then
      write(200+ic,2009)
      endif
      enddo
      endif
      endif
      endif
      endif

*     initialise the mass balance variables ------------------------------------
      call initialise data for mass balance !  nameless diffuse pollution 5 (58)

      KRFPOL58 = JF(KFEAT)
      KRQPOL58 = JQ(KFEAT)
      KRAPOL58 = add diffconc(KFEAT)
      nameprop(27) = UNAME(KFEAT)
      if ( add diffconc (KFEAT) .ge. 1 ) add conc = 1 ! ------------------- (58)

      call check the flow data (KRFPOL58,KRQPOL58,KRAPOL58) ! ------------- (58)
      if ( KRFPOL58 .eq. 0 .and. KRQPOL58 .eq. 0 
     &                     .and. KRAPOL58 .eq. 0 ) return ! --------------- (58) 
  
      call check the quality data (KRFPOL58,KRQPOL58,KRAPOL58) ! ---------- (58)
      if ( KRFPOL58 .eq. 0 .and. KRQPOL58 .eq. 0 
     &                     .and. KRAPOL58 .eq. 0 ) return ! --------------- (58) 

      IF = KRFPOL58
      IQ = KRQPOL58
      
      call check for an infeasibly large river flow 
      call check for nonparametric distribution ! ========================= (58)

      IFDIST = PDRF(IF)
*     call write data for graph plotting ! ================================ (58)

      return
      end


      subroutine turn off nameless diffuse pollution five ! --------------- (59)
      include 'COMMON DATA.FOR'

      KRFPOL58 = 0
      KRQPOL58 = 0
      KRAPOL58 = 0

      if ( JSKIP .eq. 0 .and. IPRINT .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,1009)
 1009 format(77('=')/
     &'End of diffuse input from nameless diffuse pollution 5 ... 58'/
     &77('='))
      do ic = 1,ndet
      if ( qtype(ic) .ne. 4 ) write(200+ic,1009)
      enddo
      endif
      endif
      
      call check for an infeasibly large river flow 
      
      return
      end