*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river --------------
*     ==========================================================================
*     Switches --------------------------------------------------------------260

      subroutine switches and data
      include 'COM.FOR'
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
      munthly structure = 1 ! starting value

*     force in the flow Gap Filling against the flow data entered for the 
*     feature ... This is the default value - this will be over-written by the 
*     value read from the file of global policy data (filename.GPD) that can be 
*     set up for a Batch Run
	iforce gap fill = 0

*     produce the huge GIS output files when noGIS equals 0 ... 
*     this is the default value - this will be over-written by the value read 
*     from the file of global policy data (filename.GPD) that can be set up for 
*     a Batch Run
      noGIS = 0

*     produce full output if nobigout equals 0 ...
*     this is the default value - this will be over-written by the value read 
*     from the file of global policy data (filename.GPD) that can be set up for 
*     a Batch Run
	nobigout = 0

*     ##########################################################################
*     SIMCAT can be set up to run any number of models (catchments) in batch ---
*     mode.  
*     ##########################################################################

*     exclude determinands from the overall classification by setting the ------
*     following to "1" ---------------------------------------------------------
      exclude BOD = 0
	exclude NO3 = 1 
      exclude PO4 = 0
      exclude AMM = 0
      exclude DOX = 1

*     Apply corrections to data if set to 1 ... --------------------------------
*     Eliminate zero standard deviations etc -----------------------------------
*     Details of the changes are written to the ERR file -----------------------
      tony warn overide = 1

      bagset = 0.001 ! cut-off point for including a discharge in back-tracking
      
 5555 continue

      check for infeasible quality = 0

      MONQ = 0 ! monitoring output for river quality (if MONQ is 2)
	MONF = 0 ! monitoring output for river flow (if MONF is 2 )

      return
	end


      subroutine set initial values
      include 'COM.FOR'

	NONPD = 0 ! counter of non-parametric data-sets
	seasd = 0 ! counter of monthly data-sets
      struckd = 0 ! counter of monthly structures
	tempd = 0 ! counter of monthly structures for temperature

      elapsed days in year ( 1) = 31
      elapsed days in year ( 2) = 59
      elapsed days in year ( 3) = 90
      elapsed days in year ( 4) = 120
      elapsed days in year ( 5) = 151
      elapsed days in year ( 6) = 181
      elapsed days in year ( 7) = 212
      elapsed days in year ( 8) = 243
      elapsed days in year ( 9) = 273
      elapsed days in year (10) = 304
      elapsed days in year (11) = 334
      elapsed days in year (12) = 365

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
	KRFPOL37 = 0
	KRFPOL40 = 0
	KEPOL15 = 0
	KEPOL42 = 0

      KRQPOL13 = 0
      KRQPOL25 = 0
      KRQPOL27 = 0
      KRQPOL29 = 0
      KRQPOL31 = 0
      KRQPOL33 = 0
      KRQPOL35 = 0
      KRQPOL46 = 0
      KRQPOL48 = 0
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

      i13 = 1
	j13 = 1
	n13 = 13 ! number of months in the year plus one

      nameprop( 1) = 'All discharges (3 12 5 39)'
      nameprop( 2) = 'Sewage effluents (3)'
      nameprop( 3) = 'Intermittent sewage (12)'
      nameprop( 4) = 'Industrial discharges (5)'
      nameprop( 5) = 'Mine waters (39)         '
      nameprop( 6) = 'Livestock farming (25)'
      nameprop( 7) = 'Arable farming (27)'
      nameprop( 8) = 'Highway runoff (29)'
      nameprop( 9) = 'Urban runoff (31)'
      nameprop(10) = 'Atmosphere deposition (33)'
      nameprop(11) = 'Natural background (35)'
      nameprop(12) = 'Septic tanks (37)'
      nameprop(13) = 'Aggregated CSOs (40)'
      nameprop(14) = 'Aggregated STWs (42)'
      nameprop(15) = 'Diffuse mines (46)'
      nameprop(16) = 'Birds - boats - angling (48)'
      nameprop(17) = 'Headwaters (10)'
      nameprop(18) = 'Diffuse input (13)'
      nameprop(19) = 'Diffuse input (15)'
      nameprop(20) = 'Reach diffuse'
      nameprop(21) = 'Gap filling of flows'
      nameprop(22) = 'Gap filling of quality'
       
      return
	end

      
      
      subroutine use replacement switches
      include 'COM.FOR'
      logical exists
      
*     check for a file containing various switches -----------------------------
      Switchesname = 'Switches.GPD'
      
      kswitch = 0
      call add folder for switches
      inquire( FILE = Switchesname, EXIST = exists )
      if ( exists ) then
      kswitch = 1

*     the file of "switches" exists ... read it --------------------------------
	open(193,FILE = FNAME)
*     read(193,*,err=8888) munthly structure 
      read(193,*,err=8888) master set used
      
      masterdata = 0
 	if ( ifbatch .eq. 1 .and. model number in batch .eq. 1 ) then
 	if ( master set used .eq. 1 ) masterdata = 1
      endif

	read(193,*,err=8888)iforce gap fill
      read(193,*,err=8888)noGIS
	read(193,*,err=8888)nobigout

	read(193,*,err=8888)ix
      
      read(193,*,err=8888)exclude BOD
      read(193,*,err=8888)exclude PO4
      read(193,*,err=8888)exclude AMM
      read(193,*,err=8888)exclude DOX
      read(193,*,err=8888)tony warn overide
      read(193,*,err=8888)bagset
      bagset = amax1 ( 0.0001, bagset )
      close (193)
	return
 8888 continue
	call change colour of text (21) ! dull red
      write( *,7533)
 7533 format('# Error in reading Switches.gpd ... ',
     &'Calculations stopped ...')
	call stop
      endif

      return
      end