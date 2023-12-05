*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river --------------
*     ==========================================================================
*     File switch option.for ... 393 ------------------------------------------------
*     --------------------------------------------------------------------------
*     This file deals with switches for options within the calculation ---------
*     --------------------------------------------------------------------------
*     This file contains 3 subroutines -----------------------------------------
*     They are called:
*     --------------------------------------------------------------------------
*     ...... switches and data
*     ...... set initial values
*     ...... use replacement switches
*     --------------------------------------------------------------------------

      subroutine switches and data
      include 'COMMON DATA.FOR'
      character *1 a1(2)

      write(bday,6411)iday
      read(bday,6451)a1(1),a1(2)
	if ( a1(1) .eq. ' ' ) a1(1) = '0'
	write(bday,6451)a1(1),a1(2)
      write(bmon,6411)JMONTH
      read(bmon,6451)a1(1),a1(2)
	if ( a1(1) .eq. ' ' ) a1(1) = '0'
	write(bmon,6451)a1(1),a1(2)
 6411 format(i2)
 6451 format(2a1)
      
      call set initial values

*     request the imposition of a monthly structure onto annual data -----------
*     will be switched to 1 anyway if type 8 data are entered ------------------
*     switched to 0 if NS not a multiple of 365 (and there are no type 8 data) -
      munthly structure = 1 ! the starting value - expect monthly structure ----


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     force in the flow Gap Filling against the flow data entered for the 
*     feature ... This is the default value - this will be over-written by the 
*     value read from the file of global policy data (filename.GPD) that can be 
*     set up for a Batch Run
	iforce gap fill = 0
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     produce the huge GIS output files when noGIS equals 0 ... 
*     this is the default value - this will be over-written by the value read 
*     from the file of global policy data (filename.GPD) that can be set up for 
*     a Batch Run
      noGIS = 0 ! produce GIS output files

*     produce full output if nobigout equals 0 ...
*     this is the default value - this will be over-written by the value read 
*     from the file of global policy data (filename.GPD) that can be set up for 
*     a Batch Run
      nobigout = 0 ! produce large output

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     SIMCAT can be set up to run any number of models (catchments) in batch ---
*     mode.  
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*     exclude determinands from the overall classification by setting the ------
*     following to "1" ---------------------------------------------------------
      exclude BOD = 0
      exclude NO3 = 1 
      exclude PO4 = 0
      exclude AMM = 0
      exclude DOX = 0

*     apply corrections to data if set to 1 ... --------------------------------
*     eliminate zero standard deviations etc -----------------------------------
*     details of the changes are written to the ERR file -----------------------
      toveride = 1

      bagset = 0.001 ! cut-off point for including a discharge in back-tracking
      
 5555 continue

      chekk infeezible = 0

      MONQ = 0 ! monitoring output for river quality (if MONQ is 2)
	MONF = 0 ! monitoring output for river flow (if MONF is 2 )
      
*     sequencing used for output files (111, 112 etc .CSV) ---------------------         
      jorder(1) = 1   ! All discharges (3 12 5 39 60 61)
      jorder(2) = 2   ! Sewage effluents (3)
      jorder(3) = 3   ! Intermittent sewage (12)
      jorder(4) = 4   ! Industrial discharges (5)
      jorder(5) = 5   ! Mine waters (39)
      jorder(6) = 28  ! "Other" Point Sources (60)
      jorder(7) = 29  ! Private wastewaters (61)   
      jorder(8) = 6   ! Livestock farming (25)
      jorder(9) = 7   ! Arable farming (27)
      jorder(10) = 8  ! Highway runoff (29)
      jorder(11) = 9  ! Urban runoff (31)
      jorder(12) = 10 ! Atmosphere deposition (33)
      jorder(13) = 11 ! Natural background (35)
      jorder(14) = 12 ! Septic tanks (37)
      jorder(15) = 13 ! Aggregated CSOs (40)
      jorder(16) = 14 ! Aggregated STWs (42)
      jorder(17) = 15 ! Diffuse mines (46)
      jorder(18) = 16 ! Birds - boats - angling (48)
      jorder(19) = 23 ! User-named (50)
      jorder(20) = 24 ! User-named (52)
      jorder(21) = 25 ! User-named (56)
      jorder(22) = 26 ! User-named (56)
      jorder(23) = 27 ! User-named (58)
      !jorder(24) = 17 ! Headwaters (10)

      !nameprop(18) = 'Diffuse input (13)'
      !nameprop(19) = 'Diffuse input (15)'
      !nameprop(20) = 'Reach diffuse'
      !nameprop(21) = 'Gap filling of flows'
      !nameprop(22) = 'Gap filling of quality'

      return
      end


      subroutine set initial values
      include 'COMMON DATA.FOR'

	NONPD = 0 ! counter of non-parametric data-sets
      seasd = 0 ! counter of monthly data-sets
      struckd = 0 ! counter of monthly structures
	tempd = 0 ! counter of monthly structures for temperature

      elapsed days ( 1) = 31
      elapsed days ( 2) = 59
      elapsed days ( 3) = 90
      elapsed days ( 4) = 120
      elapsed days ( 5) = 151
      elapsed days ( 6) = 181
      elapsed days ( 7) = 212
      elapsed days ( 8) = 243
      elapsed days ( 9) = 273
      elapsed days (10) = 304
      elapsed days (11) = 334
      elapsed days (12) = 365

      days in months ( 1) = 31
      days in months ( 2) = 28
      days in months ( 3) = 31
      days in months ( 4) = 30
      days in months ( 5) = 31
      days in months ( 6) = 30
      days in months ( 7) = 31
      days in months ( 8) = 31
      days in months ( 9) = 30
      days in months (10) = 31
      days in months (11) = 30
      days in months (12) = 31

      do imon = 1,12
      fraction of year (imon) = days in months (imon) / 365.0
      enddo

      KRFPOL13 = 0
      KRFPOL25 = 0
      KRFPOL27 = 0
      KRFPOL29 = 0
      KRFPOL31 = 0
      KRFPOL33 = 0
      KRFPOL35 = 0
      KRFPOL46 = 0
      KRFPOL48 = 0
      KRFPOL50 = 0
      KRFPOL52 = 0
      KRFPOL54 = 0
      KRFPOL56 = 0
      KRFPOL58 = 0
      KRFPOL37 = 0
      KRFPOL40 = 0

      KRAPOL13 = 0
      KRAPOL25 = 0
      KRAPOL27 = 0
      KRAPOL29 = 0
      KRAPOL31 = 0
      KRAPOL33 = 0
      KRAPOL35 = 0
      KRAPOL46 = 0
      KRAPOL48 = 0
      KRAPOL50 = 0
      KRAPOL52 = 0
      KRAPOL54 = 0
      KRAPOL56 = 0
      KRAPOL58 = 0
      KRAPOL37 = 0
      KRAPOL40 = 0
      
      KEPOL15 = 0
      KEPOL42 = 0

      KRAPOL25 = 0
      KRAPOL27 = 0

      KRQPOL13 = 0
      KRQPOL25 = 0
      KRQPOL27 = 0
      KRQPOL29 = 0
      KRQPOL31 = 0
      KRQPOL33 = 0
      KRQPOL35 = 0
      KRQPOL46 = 0
      KRQPOL48 = 0
      KRQPOL50 = 0
      KRQPOL52 = 0
      KRQPOL54 = 0
      KRQPOL56 = 0
      KRQPOL58 = 0
      KRQPOL37 = 0
      KRQPOL40 = 0

      CO1 = 0.0 ! correlation: river flow on river quality
      CO2 = 0.0 ! river flow on discharge flow
      CO3 = 0.0 ! river flow on discharge quality ! 4444444444444444444444444444
      CO4 = 0.0 ! river quality on discharge flow ! 4444444444444444444444444444
      CO5 = 0.0 ! discharge (or tributary) flow on discharge quality
      CO6 = 0.0 ! river quality on discharge quality ! 4444444444444444444444444

      KFEAT = 0
      KFEET = 0

      DO 10 IS=1,NS
      FDUMP(IS)=0.0
      DO 10 IDET=1, ndet
      CDUMP(IDET,IS)=0.0
   10 continue

      ifdiffuse = 0

      names of months ( 1) = '  January'
      names of months ( 2) = ' February'
      names of months ( 3) = '    March'
      names of months ( 4) = '    April'
      names of months ( 5) = '      May'
      names of months ( 6) = '     June'
      names of months ( 7) = '     July'
      names of months ( 8) = '   August'
      names of months ( 9) = 'September'
      names of months (10) = '  October'
      names of months (11) = ' November'
      names of months (12) = ' December'

      i13 = 1 ! the index in the number of 13 that hold the annual total
      j13 = 1
      n13 = 13 ! number of months in the year plus one

      nameprop( 1) = 'All discharges (3 12 5 39 60 61)'
      nameprop( 2) = 'Sewage effluents (3)'
      nameprop( 3) = 'Intermittent sewage (12)'
      nameprop( 4) = 'Industrial discharges (5)'
      nameprop( 5) = 'Mine waters (39)         '
      nameprop( 6) = 'Livestock farming (25)'
      nameprop( 7) = 'Arable farming (27)'
      nameprop( 8) = 'Highway runoff (29)'
      nameprop( 9) = 'Urban runoff (31)'
      nameprop(10) = 'Atmospheric deposits (33)'
      nameprop(11) = 'Natural background (35)'
      nameprop(12) = 'Septic tanks (37)'
      nameprop(13) = 'Aggregated CSOs (40)'
      nameprop(14) = 'Aggregated STWs (42)'
      nameprop(15) = 'Diffuse mines (46)'
      nameprop(16) = 'Birds & boats etc (48)'
      nameprop(17) = 'Headwaters and streams (10 2)'
      nameprop(18) = 'Diffuse input (13)'
      nameprop(19) = 'Diffuse input (15)'
      nameprop(20) = 'Reach diffuse'
      nameprop(21) = 'Gap filling of flows'
      nameprop(22) = 'Gap filling of quality'
      nameprop(23) = 'User-named (50)'
      nameprop(24) = 'User-named (52)'
      nameprop(25) = 'User-named (54)'
      nameprop(26) = 'User-named (56)'
      nameprop(27) = 'User-named (58)'
      nameprop(28) = 'Other Point Sources (60)'
      nameprop(29) = 'Private wastewaters (61)'
 
      numprop( 1) = 0
      numprop( 2) = 3 ! feature type code linked to nameprop  
      numprop( 3) = 12
      numprop( 4) = 5
      numprop( 5) = 39
      numprop( 6) = 25
      numprop( 7) = 27
      numprop( 8) = 29
      numprop( 9) = 31
      numprop(10) = 33
      numprop(11) = 35
      numprop(12) = 37
      numprop(13) = 40
      numprop(14) = 42
      numprop(15) = 46
      numprop(16) = 48
      numprop(17) = 10
      numprop(18) = 13
      numprop(19) = 15
      numprop(20) = 0
      numprop(21) = 0
      numprop(22) = 0
      numprop(23) = 50
      numprop(24) = 52
      numprop(25) = 54
      numprop(26) = 56
      numprop(27) = 58
      numprop(28) = 60
      numprop(29) = 61

      propsequence(1) = 1
      propsequence(2) = 2
      propsequence(3) = 3
      propsequence(4) = 4
      propsequence(5) = 5
      propsequence(6) = 28
      propsequence(7) = 29
      propsequence(8) = 6
      propsequence(9) = 7
      propsequence(10) = 8
      propsequence(11) = 9
      propsequence(12) = 10
      propsequence(13) = 11
      propsequence(14) = 12
      propsequence(15) = 13
      propsequence(16) = 14
      propsequence(17) = 15
      propsequence(18) = 16
      propsequence(19) = 23
      propsequence(20) = 24
      propsequence(21) = 25
      propsequence(22) = 26
      propsequence(23) = 27
      propsequence(24) = 18
      propsequence(25) = 19
      propsequence(26) = 17
      propsequence(27) = 20
      propsequence(28) = 21
      propsequence(29) = 22
      
      return
      end

      
      
      subroutine use replacement switches
      include 'COMMON DATA.FOR'
      logical exists
      
*     check for a file containing various switches -----------------------------
      Switchesname = 'Switches.GPD'
      
      kswitch = 0
      call add folder for switches
      inquire( FILE = Switchesname, EXIST = exists )
      if ( exists ) then
      kswitch = 1

*     the file of "switches" exists ... read it --------------------------------
      open(493,FILE = FNAME) ! SWITCHES.GPD ------------------------------------
*     set up a master set of data based on the first run in a batch ------------
      read(493,*,err=8888)master set used ! set up a master set of data ---- GPD
      
      masterdata = 0
      if ( ifbatch .eq. 1 .and. model number in batch .eq. 1 ) then
      if ( master set used .eq. 1 ) masterdata = 1
      endif

      read(493,*,err=8888)iforce gap fill ! force in the flow gap-filling -- GPD
      read(493,*,err=8888)noGIS ! produce GIS output files when set to 0 --- GPD
      read(493,*,err=8888)nobigout ! produce full output if set to 0 ------- GPD
      read(493,*,err=8888)ix     
      read(493,*,err=8888)exclude BOD ! from classification ---------------- GPD
      read(493,*,err=8888)exclude PO4 ! from classification ---------------- GPD
      read(493,*,err=8888)exclude AMM ! from classification ---------------- GPD
      read(493,*,err=8888)exclude DOX ! from classification ---------------- GPD
      read(493,*,err=8888)toveride ! switch to correct strange data -------- GPD
      read(493,*,err=8888)bagset ! cut-off for discharge in back-tracking -- GPD
      bagset = amax1 ( 0.0001, bagset )
      close (493) ! Switches.GPD
      return
 8888 continue
      call change colour of text (10) ! green
      write( *,7533)
 7533 format('# Error in reading Switches.gpd ... ',
     &'Calculations stopped ...')
      call stop
      endif ! if ( SWITCHES.GPD exists ) then

      return
      end