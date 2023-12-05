*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File name: set up files.for ... 2430 lines -------------------------------
*     --------------------------------------------------------------------------
*     It sets up files for input and output of data 
*     --------------------------------------------------------------------------
*     This file contains 35 subroutines ........................................
*     They are called:
*     --------------------------------------------------------------------------
*     ...... attach SIMCAT data file for catchment
*     ...... read the global data file
*     ...... attach files and sort out gap filling
*     ...... blank ! eliminate blanks in the filenames --------------------
*     ...... leadblanks ! eliminate leading blanks from a run title
*     ...... leadblanks2 ! eliminate leading blanks from a line of data
*     ...... lose folder ! eliminate the name of a folder from a filename 
*     ...... blank middle ! eliminate blanks in the filenames (not used)
*     ...... blank middle 2 ! eliminate blanks in the filenames (not used)
*     ...... add folder for non parametric data file (ichan)
*     ...... add folder for monthly data file (ichan)
*     ...... add folder for monthly structure file (ichan)
*     ...... add folder to the name of the file (ichan)
*     ...... river flow correlation
*     ...... river quality correlation
*     ...... deal with non standard correlation for effluent flow
*     ...... deal with non standard correlation for effluent quality
*     ...... trunk (ch1,ch2,ch3) ! add stem to an output file
*     ...... trunk6 (kay) ! add a stem for an output file for determinands
*     ...... trunk9 (kay) ! sort out a file for output on apportionment
*     ...... trunk4 (kay) ! CSV output on apportionment of load from discharges
*     ...... trunk10 (kay) ! CSV file for apportionment of percentiles ..... 160
*     ...... trunk14 (kay) ! add stem for output file for apportionment .ACL
*     ...... trunk12 (kay) ! add stem for file - assistance with calibration
*     ...... trunk7 (kay) ! add stem for output file for GIS ... .CSV
*     ...... trunk27 (kay) ! add stem for output file for GIS .. DAi.CSV ... 160
*     ...... trunk8 (kay) ! add stem for output file ... Ei.CSV
*     ...... trunk15 (kay) ! add stem for output for graphs ... Gi.CSV
*     ...... trunk16 (kay) ! add stem for output for WORD reports ... Wi.CSV
*     ...... trunk17 (kay) ! add stem for o/p for discharges .... Di.EFF ... 
*     ...... trunk2 ! add stem for output file .GIS1.CSV
*     ...... trunk3 ! add stem for output file for effluents ... .EFF
*     ...... add folder for globaldata
*     ...... add folder for switches
*     ...... add folder for output
*     --------------------------------------------------------------------------

*     attach the main SIMCAT data file -----------------------------------------
      subroutine attach simcat data file for catchment
      include 'COMMON DATA.FOR'
      logical exists

*     read the name of the data file -------------------------------------------
      read(23,3,err=9999,end=9999)DatName
    3 format(a136)
      datfilename = DatName
      call lose folder ! eliminate the name of a folder from a filename 

      write (fname,99) datname
   99 format(a136)
      call trunk ('E','N','D') ! add stem to an output file
      open(09,FILE = FNAME )

      inquire ( FILE = FNAME, EXIST = exists )
      if ( exists ) then  
      open(09,FILE = FNAME, STATUS = 'OLD')
      close (09, status='delete')    
      endif

      inquire ( FILE = DatName, EXIST = exists )
      if ( .NOT. exists ) then
      write( *,5) DatName
    5 format(/
     &10X,'  --------------------------------------------------- '/
     &10X,'  The following data file: '/
     &20x,a136/
     &10X,'  does not exist.  Run halted ...'/   
     &10X,'  --------------------------------------------------- ')
      call stop
      endif
      open(02,FILE = DatName, STATUS = 'OLD')
      
      return

 9999 write( *,9998)
 9998 format('*** Error in reading RUN161.TMP '/
     &'*** Calculation stopped ...')
      call stop
      end

      
      

*     attach the file on global data changes -----------------------------------
      subroutine read the global data file
      include 'COMMON DATA.FOR'
      logical exists

      iscreen = 3

      if ( master set used .eq. 1 .and.
     &krite1 .eq. 0 .and. model number in batch .eq. 1) then
      krite1 = 1
      call change colour of text (49) ! dull blue
      write( *,2110) 
 2110 format('The first DAT file ',
     &'is used to guide the rest       ...')
      call set screen text colour
      endif
      

*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      calspecial = 1
      Globaldataname='Globalpolicydata.GPD'
      call add folder for globaldata
      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( .NOT. exists ) return

      open(488,FILE = Globaldataname, STATUS = 'OLD')

*     specify the use of the existing gap filling files if [calspecial] = 0 ----
*     otherwise renew the gap filling ------------------------------------------
      calspecial = 0
      read(488,*,ERR=999)calspecial
*     retain and use the existing files for gap filling ------------------------
      if ( calspecial .eq. 0 ) then
      if ( ical .eq. 5 .or. ical .eq. 6 ) then
      ical = 4
      endif
      endif

*     renew the files for gap filling ------------------------------------------
      if ( ical .lt. 07 ) then
      if ( calspecial .eq. 1 ) ical = 6
      endif

*     Specify forcing in of gap filling of river flows iforce gap fill = 1
*     Gap filling will happen even if there is no specified extra file of 
*     flow data with the feature.  The data for the specified monitoring station 
*     will be used
*     ==========================================================================
      iforce gap fill = 0
      read(488,*,ERR=999)iforce gap fill
      if ( ical .eq. 0 ) iforce gap fill = 0
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


*     produce large output files when nobigout = 0 -----------------------------
*     especially the files of GIS data -----------------------------------------
*     ==========================================================================
      nobigout = 0 ! produce large output ............ read the global data file
      noGIS = nobigout

      close (488) ! Globalpolicydata.GPD
      return

  999 continue
      write( *,900)
  900 format('Error in reading the file Global policy data.GPD ...')
      call stop
      end



*     ==========================================================================
*     Attach files =============================================================
*     ==========================================================================
*     Output channels ....... from data-file .....                  filename.DAT
*     ==========================================================================
*     01   main and detailed output:                                filename.OUT
*     02   main data file:                                          filename.DAT
*     03   summary output and report on gap filling:                filename.CAL
*     08   input data:                                              filename.INP
*     09   copy of screen output:                                   filename.SCN
*     22   results for old graph plotting:                          filename.SGR
*     27   results on loads:                                        filename.LOD
*     30   results on compliance with targets:                      filename.TGT 
*     31   effluents:                                               filename.EFF 
*     33   record of possible errors found:                         filename.ERR 
*     36   outputs on effluents!                                filename-EFF.CSV
*     42   output for GIS:                                     filename-GIS1.CSV
*     48   output on classification:                                filename.WFD
*     72   output on river flows:                                   filename.FLO
*     74   working file for flow gap filling:                       filename.FCL
*     75   working file for quality gap filling:                    filename.QCL
*     101-110   output for months:                               filename-Di.MON
*     131-140   output for effluents:                            filename-Ei.CSV
*     111-120   output for GIS:                                  filename-Di.CSV
*     181-190   apportionment of load and concentration:        filename-DAi.CSV
*     191-200   apportionment of river concentrations from discharges     Qi.CSV
*     121-130   apportionment of load and concentration:         filename-Di.ADL
*     141-150   apportionment for river concentrations:          filename-Di.ADC  
*     151-160   output for individual discharge:                 filename-Di.EFF
*     221-230   apportionment to catchments:                     filename-Di.ACL
*     161-170   apportionment of load from discharges:           filename-Pi.CSV
*     171-180   assistance with calibration:                     filename-Ci.GAP
*     231-240   data for graph plotting:                         filename-Gi.CSV
*     241-250   data for use in reports in WORD:                 filename-Wi.CSV
*     261-270   apportionment between water bodies:             filename-WBi.CSV
*     ==========================================================================
*     23 RUN161.TMP
*     ==========================================================================

      subroutine attach files and sort out gap filling
      include 'COMMON DATA.FOR'
      character *01 a(132)
      character *132 RECORD
      logical exists
      
*     open the main output file ----------------------------------------------01
      write (fname,99) datname
   99 format(a136)
      call trunk ('O','U','T') ! add stem to an output file
      open(01,FILE=FNAME) ! open the main output file ------------------------01
      write(01,*)' '
      
*     input file -------------------------------------------------------------08
      write (fname,99) datname
      call trunk ('I','N','P') ! add stem to an output file
      open(08,FILE=FNAME) ! input file .INP ----------------------------------08

*     working file used for developing SIMCAT -------------------------------004
      write (fname,99) datname
      call trunk ('W','O','K') ! add stem to an output file
      open(004,FILE=FNAME) ! input file .WOK --------------------------------004
      write(004,*)' '
*     second working file used for developing SIMCAT ------------------------005
      write (fname,99) datname
      call trunk ('W','A','K') ! add stem to an output file
      open(005,FILE=FNAME) ! input file .WAK --------------------------------005
      write(005,*)' '
*     third working file used for developing SIMCAT -------------------------007
      write (fname,99) datname
      call trunk ('W','U','K') ! add stem to an output file
      open(007,FILE=FNAME) ! input file .WUK --------------------------------007
      write(007,*)' '
*     ==========================================================================
      
*     open screen output file ------------------------------------------------09
      write (fname,99) datname
      call trunk ('S','C','N') ! add stem to an output file
      open(09,FILE=FNAME) ! open screen output file .SCN ---------------------09

*     open summary output file -----------------------------------------------03
      write (fname,99) datname
      call trunk ('C','A','L') ! add stem to an output file
      open(03,FILE=FNAME) ! open summary output file .CAL --------------------03
      write(03,*)' '

*     open output file for the results of calculations of load ---------------27
      write (fname,99) datname
      call trunk ('L','O','D') ! add stem to an output file
      open(27,FILE=FNAME) ! results of calculations of load .LOD -------------27
      write(27,*)' '

*     file for outputs to errors .ERR ... for this data file -----------------33
      write (fname,99) datname
      call trunk ('E','R','R') ! add stem to an output file
      open(33,FILE=FNAME) ! outputs on errors .ERR ---------------------------33

*     file for outputs to .GIS ... GIS1.CSV ----- SAGIS --------------------- 42
      write (fname,99) datname
      call trunk2 ! add stem for output file .GIS1.CSV
      open(42,FILE=FNAME) ! GIS ... output file .GIS1.CSV ------------------- 42
      write(42,*)' '
      backspace(42)
      if ( noGIS .eq. 0 ) then
      write(42,8255)bday,bmon,IYEAR ! output file .GIS1.CSV ----------------- 42
 8255 format('SIMCAT',',','Version 161  ',',', ! ---------------------------- 42
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',')
      endif

*     open output file for the results of calculations of compliance with --- 30
*     targets ----------------------------------------------------------------30
      write (fname,99) datname
      call trunk ('T','G','T') ! add stem to an output file
      open(30,FILE=FNAME) ! output file for targets --------------------------30
      write(30,*)' '

*     open the file for Visual Basic graphics --------------------------------22
      write (fname,99) datname
      call trunk ('S','G','R') ! add stem to an output file
      open(22,FILE=FNAME) ! open the file for Visual Basic graphics .SGR -----22


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     open the gap filling files --------------------------------- .FCL and .QCL
*     prepare to open the working file for flow gap filling --------------- .FCL
      write (fname,99) datname
      call trunk ('F','C','L') ! add stem to an output file
      read(Fname,7)(a(i),i=1,132)
    7 format(132a1)
*     change the name of the FCL file to that of the Base File -----------------
      if ( ical .eq. 04 .or. ical .eq. 07 .or. ical .eq. 08 
     &                  .or. ical .eq. 09) then
      i = 0
   64 i = i + 1      
      if ( i .eq. 125 ) goto 63
      if ( a(i) .eq. '_' .and. a(i+1) .eq. 'M' ) then
      if ( a(i+2) .eq. '.' ) then
      if ( a(i+3) .eq. 'F' .and. a(i+4) .eq. 'C' ) then
      do jj = i, i + 8
      a(jj) = a(jj+2)
      enddo
      goto 63
      endif
      endif
      endif
      goto 64
   63 continue       
      write (Fname,7)(a(i),i=1,132) ! write the name of the gap-filling file ---
      endif ! if ( ical .eq. 07 , 08 or 09 ) -----------------------------------



*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
*     assume by default that gap filling is required in modes 7, 8 and 9 =======
      no gap filling 78 = 0 ! initialise gap filling as required ===============
*     check whether the working file for flow gap filling exists ===============
      inquire( FILE = Fname, EXIST = exists )
*     allow modes 4, 7, 8 and 9 to operate without gap filling =================
      if ( .NOT. exists .or. nogap .eq. 1 ) then ! no gap filling ==============
      if ( ical .eq. 04 .or. ical .eq. 07 .or. ical .eq. 08 
     &                  .or. ical .eq. 09 ) then
      no gap filling 78 = 1 ! no gap filling will be done ======================
      if ( ical .eq. 4 ) ical = 0
      return !  do not open the files .FCL and .QCL ============================
      endif ! if ( ical .eq. 04, 07, 08 or 09 ) ================================
      endif ! if ( .NOT. exists) ===============================================
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
 

     
*     open the working file for gap filling of river flows ------- 74 ----- .FCL
      if ( ical .ne. 0 ) open(74,FILE = Fname) ! gap filling of flows --------74

*     check whether gap filling is to be forced --------------------------------
      if ( ical .eq. 4 ) then 
      read(74,1001,end=9997,ERR=9998)RECORD
 1001 format(A132)
      read(74,*,ERR=9998)Lcheck
      if ( Lcheck .lt. 0 ) then
      iforce gap fill = 1
      else
      iforce gap fill = 0
      endif
      rewind 74
      endif

*     prepare to open the working file for quality gap filling -----------------
      write (fname,99) datname
      call trunk ('Q','C','L') ! add stem to an output file
      read(Fname,7)(a(i),i=1,132)

      if ( ical .eq. 04 .or. ical .eq. 07 .or. ical .eq. 08 ! ==================
     &                  .or. ical .eq. 09 ) then
      i = 0
   74 i = i + 1      
      if ( i .eq. 125 ) goto 73
      if ( a(i) .eq. '_' .and. a(i+1) .eq. 'M' ) then
      if ( a(i+2) .eq. '.' ) then
      if ( a(i+3) .eq. 'Q' .and. a(i+4) .eq. 'C' ) then
      do j = i, i+8
      a(j) = a(j+2)
      enddo
      goto 73
      endif
      endif
      endif
      goto 74
   73 continue       
      endif ! if ( ical .eq. 04, 07, 08 or 09 ) ================================
      write (Fname,7)(a(i),i=1,132) 
      
*     open the working file for gap filling of river quality ----- 75 ----- .QCL
      if ( ical .ne. 0 ) open(75,FILE = fname) ! gap filling of quality ------75
      return

  999 continue

      return


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
 9998 write(33,3901)
      write(01,3901)
      write(09,3901)
      write( *,3901)
 3901 format('*** Error in setting up gap filling for river ',
     &'flows ...'/'*** CACLULATION STOPPED.....')
      rewind 74
      call stop
            
 9997 continue
      write(33,3900)
      write(01,3900)
      write(09,3900)
 3900 format('*** There is no gap filling file for river ',
     &'flows ... a basic run will be performed ...')
 	call change colour of text (22) ! light blue
      write( *,3910)
 3910 format('*** There is no gap filling file for river ',
     &'flows   ...       a basic run will be performed ...')
      call set screen text colour
      rewind 74
      ical = 0
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


      return
      end


*     eliminate blanks in the filenames ----------------------------------------
      subroutine blank ! eliminate blanks in the filenames ---------------------
      include 'COMMON DATA.FOR'
      character*1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      a(137)=' '
   
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 9
      if ( a(j) .eq. ' ' ) then
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
      endif
    1 continue
    9 continue

      write (fname,5) (a(i),i=1,136)
      return
      end

*     eliminate leading blanks -------------------------------------------------
      subroutine leadblanks ! eliminate leading blanks from a run title
      include 'COMMON DATA.FOR'
      character *1   a(70)

      read(title,5) (a(i),i=1,70)
    5 format(70A1)
      a(70)=' '
 
    7 continue
      if (a(1) .ne. ' ') goto 9
      do k=1,69
      a(k) =a (k+1)
      enddo
      goto 7
    9 continue

      write (title,5) (a(i),i=1,70)
      return
      end
      
*     eliminate leading blanks -------------------------------------------------
      subroutine leadblanks2 ! eliminate leading blanks from a line of data
      include 'COMMON DATA.FOR'
      character *1   a(200)

      read(line of data,5) (a(i),i=1,200)
    5 format(200A1)
      a(200) = ' '
 
    7 continue
      if (a(1) .ne. ' ') goto 9
      do k=1,199
      a(k) = a(k+1)
      enddo
      goto 7
    9 continue

      write (line of data,5) (a(i),i=1,200)
      return
      end


*     eliminate the name of a folder from a filename ---------------------------
      subroutine lose folder ! eliminate the name of a folder from a filename 
      include 'COMMON DATA.FOR'
      character *1 a(130),b(130)

      read(datfilename,5) (a(i),i=1,130)
    5 format(130A1)

      do i=1,130
      j = 130-i+1
      b(i) = a(j)
      enddo

      write(datfilename,5) (b(i),i=1,130)
    7 continue
       
      if ( b(1) .ne. ' ' ) goto 9
      do k = 1, 129
      b(k) = b(k+1)
      enddo
      b(130) = ' '
      goto 7
    9 continue

      i = 0
    1 i = i + 1
      if ( i .eq. 131 ) goto 4
      if ( b(i) .ne. '\' ) goto 1
      do k = i, 130
      b(k) = ' '
      enddo
    4 continue

      do k = 1, i
      j = i - k + 1
      a(k) = b(j)
      enddo

      do k = i + 1, 130
      a(k) = ' '
      enddo

      write (datfilename,5) (a(j),j=1,130)
      return
      end

      
      
*     eliminate blanks in the filenames  (not used) ----------------------------
      subroutine blank middle ! eliminate blanks in the filenames (not used)
      include 'COMMON DATA.FOR'
      character*1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137) = ' '
	    
      j = 0
      kk = 0
    1 j = j + 1

    7 if ( j .eq. 138 - kk) goto 9
      if ( a(j) .ne. '\' ) goto 1

    6 j = j +1
    4 continue
      if ( a(j) .eq. ' ' ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      a(137) = 'X'
      goto 4
      endif

      if ( a(j) .ne. ' ' ) then
      goto 1
      endif
      goto 6
    9 continue

      do 11 i = 1,136
      j = 137 - i
      if ( a(j) .eq. 'X' .or. a(j) .eq. ' ' ) then 
      a(j) = ' '
      else
      goto 15
      endif
   11 continue
   15 continue

      write (fname,5) (a(i),i=1,136)
      return
      end

      subroutine blank middle 2 ! eliminate blanks in the filenames (not used)
      include 'COMMON DATA.FOR'
      character*1 a(167)

      read(fname2,5) (a(i),i=1,166)
    5 format(166A1)

      a(167) = ' '
	    
      j = 0
      kk = 0
    1 j = j + 1

    7 if ( j .eq. 168 - kk ) goto 9
      if ( a(j) .ne. '\' ) goto 1
    6 j = j +1
    4 continue
      if ( a(j) .eq. ' ' ) then
      do k = j, 166
      a(k) = a(k+1)
      enddo
      a(167) = 'X'
      goto 4
      endif

      if ( a(j) .ne. ' ' ) then
      goto 1
      endif

      goto 6

    9 continue

      do 11 i = 1,166
      j = 167 - i
      if ( a(j) .eq. 'X' .or. a(j) .eq. ' ' ) then 
      a(j) = ' '
      else
      goto 15
      endif
   11 continue
   15 continue

      write (fname2,5) (a(i),i=1,166)
      return
      end



*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for non parametric data file (ichan)
      include 'COMMON DATA.FOR'
      character *2 colon

      Flnamesmall (ichan,nonpd) = Flname (ichan,nonpd)

*     check whether the file name includes the folder --------------------------
      colon = 'xx'
      read(Flname (ichan,nonpd),3)colon
    3 format(a2)
*     if it does then do not add the name of the DAT folder --------------------
      if ( colon .eq. 'c:' ) return
*     --------------------------------------------------------------------------

      if ( folder .eq. '    ' ) then
      write (FNAME,2) Flname(ichan,nonpd)
    2 format(A30)
      call blank ! eliminate blanks in filenames
      else
      write (FNAME,1) folder, Flname(ichan,nonpd)
    1 format(a100,A30)
      call blank ! eliminate blanks in filenames
      endif

      Flname (ichan,nonpd) = fname
      return
      end



*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for monthly data file (ichan)
      include 'COMMON DATA.FOR'
   
      Flmonthsmall (ichan,seasd) = Flmonth (ichan,seasd)

      if ( folder .eq. '    ' ) then

      if ( ichan .eq. 1 ) write (FNAME,2) flmonth(1,seasd)
      if ( ichan .eq. 2 ) write (FNAME,2) flmonth(2,seasd)
      if ( ichan .eq. 3 ) write (FNAME,2) flmonth(3,seasd)
      if ( ichan .eq. 4 ) write (FNAME,2) flmonth(4,seasd)
    2 format(A30)
      call blank ! eliminate blanks in filenames
      else
      if ( ichan .eq. 1 ) write (FNAME,1) folder, flmonth(1,seasd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, flmonth(2,seasd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, flmonth(3,seasd)
      if ( ichan .eq. 4 ) write (FNAME,1) folder, flmonth(4,seasd)
    1 format(a100,A30)
      call blank ! eliminate blanks in filenames
      endif

      if ( ichan .eq. 1 ) flmonth(1,seasd) = fname
      if ( ichan .eq. 2 ) flmonth(2,seasd) = fname
      if ( ichan .eq. 3 ) flmonth(3,seasd) = fname
      if ( ichan .eq. 4 ) flmonth(4,seasd) = fname

      return
      end




*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for monthly structure file (ichan)
      include 'COMMON DATA.FOR'

      FLSTRUCTsmall (ichan,struckd) = FLSTRUCT (ichan,struckd)
      if ( folder .eq. '    ' ) then
      if ( ichan .eq. 1 ) write (FNAME,2) FLSTRUCT(1,struckd)
      if ( ichan .eq. 2 ) write (FNAME,2) FLSTRUCT(2,struckd)
      if ( ichan .eq. 3 ) write (FNAME,2) FLSTRUCT(3,struckd)
      if ( ichan .eq. 4 ) write (FNAME,2) FLSTRUCT(4,struckd)
    2 format(A30)

      call blank ! eliminate blanks in filenames
      else

      if ( ichan .eq. 1 ) write (FNAME,1) folder, FLSTRUCT(1,struckd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, FLSTRUCT(2,struckd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, FLSTRUCT(3,struckd)
      if ( ichan .eq. 4 ) write (FNAME,1) folder, FLSTRUCT(4,struckd)
    1 format(a100,A30)

      call blank ! eliminate blanks in filenames
      endif

      if ( ichan .eq. 1 ) FLSTRUCT(1,struckd) = fname
      if ( ichan .eq. 2 ) FLSTRUCT(2,struckd) = fname
      if ( ichan .eq. 3 ) FLSTRUCT(3,struckd) = fname
      if ( ichan .eq. 4 ) FLSTRUCT(4,struckd) = fname
      return
      end




*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder to the name of the file (ichan)
      include 'COMMON DATA.FOR'

      FLtempsmall (ichan,tempd) = fltemp (ichan,tempd)
      if ( folder .eq. '    ' ) then
      if ( ichan .eq. 1 ) write (FNAME,2) fltemp(1,tempd)
      if ( ichan .eq. 2 ) write (FNAME,2) fltemp(2,tempd)
      if ( ichan .eq. 3 ) write (FNAME,2) fltemp(3,tempd)
      if ( ichan .eq. 4 ) write (FNAME,2) fltemp(4,tempd)
    2 format(A30)

      call blank ! eliminate blanks in filenames
      else

      if ( ichan .eq. 1 ) write (FNAME,1) folder, fltemp(1,tempd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, fltemp(2,tempd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, fltemp(3,tempd)
      if ( ichan .eq. 4 ) write (FNAME,1) folder, fltemp(4,tempd)
    1 format(a100,A30)

      call blank ! eliminate blanks in filenames
      endif

      if ( ichan .eq. 1 ) fltemp(1,tempd) = fname
      if ( ichan .eq. 2 ) fltemp(2,tempd) = fname
      if ( ichan .eq. 3 ) fltemp(3,tempd) = fname
      if ( ichan .eq. 4 ) fltemp(4,tempd) = fname

      return
      end







*     deal with non-standard correlation ---------------------------------------
      subroutine river flow correlation
      include 'COMMON DATA.FOR'

      jtrim = 0
      nscorr = 0

*     loop on all the flow data-sets -------------------------------------------
      do 2 lf = 1, nf

*     check whether flow data have been entered --------------------------------
      if ( PDRF(lf) .gt. -999 ) then

*     look at the correlation coefficient --------------------------------------
      xx = F(lf,MO)

*     check whether the default is to be retained ------------------------------
      if ( xx .eq. -9.9 ) goto 2

*     check for illegal values -------------------------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      F(lf,MO) = -9.9
      goto 2
      endif

*     check for zero -----------------------------------------------------------
      if ( xx .eq. 0.0 ) then
      F(lf,MO) = -9.9
      jtrim = jtrim + 1
      if ( jtrim .lt. 3 ) then
      suppress20 = suppress20 + 1 ! zero flow correlation
      if ( nobigout .le. 0 ) write(01,1821)lf
 1821 format(77('-')/
     &'*** A river flow dataset has a zero correlation coeffi',
     &'cient ...   Code:',i6/
     &'*** ',24x,'This has been over-written with the default value'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      write(09,1821)lf
      write(33,1821)lf
      endif
      goto 2
      else

      if ( nscorr .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(01,1822)xx,lf
      write(09,1822)xx,lf
 1822 format(77('-')/
     &'***    A river flow dataset has a special correlation coeffi',
     &'cient of ',f8.4/
     &'***      This sets correlation with the master set of ',
     &'flow data - code:',i6/77('-'))
      write(33,1822)xx,lf
      endif
      nscorr = nscorr + 1
      goto 2
      endif
      endif
    2 continue

      if ( jtrim .gt. 2 ) then
      if ( nobigout .le. 0 ) write(01,1001)jtrim
      write(09,1001)jtrim
      write(33,1001)jtrim
 1001 format(77('-')/
     &'***  Number of river flow data-sets with zero ',
     &'correlation coefficients:',i6/'*** ',
     &11x,'SIMCAT has over-written these with the default value of ',
     &'1.0000 '/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 ) write (01,1901) nscorr 
      write (09,1901) nscorr 
      write (33,1901) nscorr 
 1901 format('*** Number of river flow data-sets with special ',
     &'correlation coefficient:',i5/77('-'))

      endif
      return
      end





*     deal with non-standard correlation ---------------------------------------
      subroutine river quality correlation
      include 'COMMON DATA.FOR'

      jtrim = 0

*     count special correlation coefficients -----------------------------------
      nscorr = 0

*     loop on all the river quality data-sets ----------------------------------
      do 2 lv = 1, nv

*     check whether river quality data have been entered -----------------------
      do 22 idet = 1, ndet
      if ( qtype (idet) .eq. 4 ) goto 22
      if ( PDRC (lv,idet) .gt. -1 ) then

*     look at the correlation coefficient --------------------------------------
      xx = quolity data (lv, idet, MO)

*     check whether the default is to be retained ------------------------------
      if ( xx .eq. -9.9 ) goto 22

*     check for illegal values -------------------------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      quolity data (lv, idet, MO) = -9.9
      goto 22
      endif

*     check for zero -----------------------------------------------------------
      if ( xx .eq. 0.0 ) then
      quolity data (lv, idet, MO) = -9.9
      jtrim = jtrim + 1
      if ( jtrim .lt. 3 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) then
      write(01,1821)lv,dname(idet)
 1821 format(/77('-')/
     &'*** A river quality dataset has a zero correlation ',
     &'coefficient',4x,'Code:',i6/
     &'*** ',24x,'This has been over-written with the default value'/
     &'*** ',20x,'Use 0.00001 when you really want zero ... ',a11/
     &77('-'))
      endif
      write(33,1821)lv,dname(idet)
      endif
      goto 22
      else

      if ( nscorr .eq. 0 ) then
      write(33,1822)xx,lv
 1822 format(/77('-')/
     &'*** A river quality dataset has a special correlation coeffi',
     &'cient of ',f8.4/
     &'*** This sets the correlation with river flow ...',17x,
     &'Code:',i6/77('-'))
      !if ( nobigout .le. 0 ) write(01,1822)xx,lv
      endif
      nscorr = nscorr + 1
      goto 22
      endif
      endif
   22 continue

    2 continue

      if ( jtrim .gt. 2 ) then

      !if ( nobigout .le. 0 ) write(01,1001)jtrim
      write(09,1001)jtrim
      write(33,1001)jtrim
 1001 format(/77('-')/
     &'*** ',07x,'River quality data-sets with zero ',
     &'correlation coefficients =',i6/
     &'*** ',20x,'SIMCAT has over-written these with the default ',
     &'values'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0) write(01,1901) nscorr 
      write(33,1901) nscorr 
      write(08,1901) nscorr 
 1901 format(/77('-')/
     &'*** River quality data-sets with special ',
     &'correlation coefficients ...',i8/77('-'))
      endif
      
      return
      end




*     deal with non-standard correlation for effluent flow ---------------------
      subroutine deal with non standard correlation for effluent flow
      include 'COMMON DATA.FOR'

      jtrim = 0
      nscorr = 0

*     loop on all the effluent data-sets
      do 2 le = 1, ne

*     check whether flow data have been entered --------------------------------
      if ( pdef(le) .gt. -1 ) then

*     look at the correlation coefficient --------------------------------------
      xx = FE (le,MO)

*     check whether the default value is to be retained ------------------------
      if ( xx .eq. -9.9 ) goto 2

*     check for illegal values -------------------------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      FE (le,MO) = -9.9
      goto 2
      endif

*     check for zero -----------------------------------------------------------
      if ( xx .eq. 0.0 ) then
      FE (le,MO) = -9.9
      jtrim = jtrim + 1
      if ( jtrim .lt. 3 ) then
      if ( nobigout .le. 0 ) write(01,1821)le
 1821 format(77('-')/
     &'***       An effluent flow has a zero correlation ',
     &'coefficient ... Code:',i6/
     &'*** ',24x,'This has been over-written with the default value'/
     &'*** ',25x,'You should use 0.00001 when you really want zero',
     &/77('-'))
      write(09,1821)le
      write(33,1821)le
      endif
      goto 2
      else

      if ( nscorr .eq. 0 ) then 
      write(08,1822)xx,le
 1822 format(77('-')/
     &'*** ',7x,'An effluent flow has a special correlation coeffi',
     &'cient of ',f8.4/
     &'*** ',12x,'This sets the correlation with the river flow ... ',
     &'Code:',i6/77('-'))
      write(33,1822)xx,le
      endif
      nscorr = nscorr + 1
      goto 2

      endif
      endif

    2 continue

      if ( jtrim .gt. 2 ) then
      if ( nobigout .le. 0 ) write(01,1001) jtrim
      write(09,1001)jtrim
      write(33,1001)jtrim
 1001 format(77('-')/'*** ',7x,'Effluent flow data-sets with zero ',
     &'correlation coefficients =',i6/
     &'*** ',20x,'SIMCAT has over-written these with the default ',
     &'values'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 ) write (01,1901) nscorr 
      write (09,1901) nscorr  
      write (33,1901) nscorr 
 1901 format(77('-')/'*** ',4x,'Effluent flow data-sets with special ',
     &'correlation coefficients =',i6/77('-'))
      endif

      return
      end





*     deal with non-standard correlation for effluent quality ------------------
      subroutine deal with non standard correlation for effluent quality
      include 'COMMON DATA.FOR'

      jtrim = 0
      nscorr = 0

*     loop on all the effluent data-sets ---------------------------------------
      do 2 le = 1, ne

      do 22 idet = 1, ndet
      if ( qtype(idet) .eq. 4 ) goto 22

*     check whether data have been entered -------------------------------------
      if (PDEC (le,idet) .gt. -1) then

*     look at the correlation coefficient --------------------------------------
      xx = pollution data (le,idet,MO)

*     check whether the default is to be retained ------------------------------
      if ( xx .eq. -9.9 ) goto 22

*     check for illegal values of correlation ----------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      pollution data (le,idet,MO) = -9.9 ! initialise data - use defaults 
      goto 22
      endif

*     check for zero -----------------------------------------------------------
      if ( xx .eq. 0.0 ) then
      pollution data (le,idet,MO) = -9.9
      jtrim = jtrim + 1
      if ( jtrim .lt. 3 ) then
      if ( nobigout .le. 0 ) write(01,1821)le
 1821 format(77('-')/
     &'*** ',11x,'An effluent quality dataset has zero correlation ',
     &'- Code:',i6/
     &'*** ',24x,'This has been over-written with the default value'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      write(33,1821)le
      endif
      goto 22

      else

      if ( nscorr .eq. 0 ) then
      write(33,1822)xx,le
 1822 format(/77('-')/
     &'*** ',4x,'An effluent quality has a special correlation coeffi',
     &'cient of ',f8.4/'*** ',6x,
     &'This sets the correlation with the effluent flow data   ',
     &'Code:',i6/77('-'))
      !if ( nobigout .le. 0 ) write(01,1822)xx,le
      endif
      nscorr = nscorr + 1
      goto 22

      endif
      endif

   22 continue

    2 continue

      if ( jtrim .gt. 2 ) then
      if ( nobigout .le. 0 ) write(01,1001) jtrim
      write(09,1001)jtrim
 1001 format(77('-')/
     &'*** ',8x,'Effluent quality data with zero ',
     &'correlation coefficients ...',i5/
     &'*** ',20x,'SIMCAT has over-written these with the default ',
     &'values'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      write(33,1001)jtrim
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 .and. ical13 .eq. 0 ) write(01,1901)nscorr 
      write(09,1901)nscorr 
      write(33,1901)nscorr 
 1901 format(
     &'*** Effluent quality data-sets with special ',
     &'correlation coefficients ...',i5/77('-'))
      endif

      return
      end




*     add stem to an output file
      subroutine trunk (ch1,ch2,ch3) ! add stem to an output file
      include 'COMMON DATA.FOR'
      character *1 ch1,ch2,ch3
      character *1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137) = ' '
      do 1 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue
      write (fname,5) (a(i),i=1,j-1),'.',ch1,ch2,ch3

      return
      end
      






*     add stem for output file for determinands ... Di.MON
      subroutine trunk6 (kay) ! add a stem for an output file for determinands
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'M'
      a(j+5) = 'O'
      a(j+6) = 'N'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)

      return
      end

      
     
      
*     add stem for output file for apportionment ... .ADL
      subroutine trunk9 (kay) ! sort out a file for output on apportionment
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'A'
      a(j+5) = 'D'
      a(j+6) = 'L'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)
      
      return
      end
      
      
      
*     add stem for output file for apportionment ... .AGL
      subroutine trunk120 (kay) ! sort out a file for output on apportionment
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'  
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'A'
      a(j+5) = 'G'
      a(j+6) = 'L'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)

      return
      end

  
    
*     add stem for output file for apportionment ... .CSV
      subroutine trunk4 (kay) ! CSV output on apportionment of load from discharges
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '.' ) goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'P'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'C'
      a(j+5) = 'S'
      a(j+6) = 'V'

      write (fname,5) (a(i),i=1,j+6)

      return
      end


*     ====================================================================== 160
*     add stem for output files for apportionment ........... 160 ......... .ADC
      subroutine trunk10 (kay) ! files for apportionment of percentiles ... .ADC
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'A'
      a(j+5) = 'D'
      a(j+6) = 'C'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)

      return
      end ! ================================================================ 160

      
*     add stem for output files for discharges ............................  EFF
      subroutine trunk17 (kay) ! output for discharges ....................  EFF
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'E'
      a(j+5) = 'F'
      a(j+6) = 'F'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)

      return
      end
      
      
      

*     add stem for output file for apportionment ... .ACL
      subroutine trunk14 (kay) ! add stem for output file for apportionment .ACL
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)
      
      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\' ) goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      !a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      !write (fname,5) (a(i),i=1,j),'O','u','t','p','u','t','\',
      !&(a(i),i=j+1,129)

      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'A'
      a(j+5) = 'C'
      a(j+6) = 'L'
      
      write (fname,5) (a(i),i=1,j),
     &(a(i),i=j+1,129)

      return
      end

      


*     add stem for file - assistance with calibration ... .GAP
      subroutine trunk12 (kay) ! add stem for file - assistance with calibration
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137) = ' '
    
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '.' ) goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'C'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'G'
      a(j+5) = 'A'
      a(j+6) = 'P'

      write (fname,5) (a(i),i=1,j+6)

      return
      end


*     add stem for output file for GIS ... .CSV
      subroutine trunk7 (kay) ! add stem for output file for GIS ... .CSV

      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'C'
      a(j+5) = 'S'
      a(j+6) = 'V'

      write (fname,5) (a(i),i=1,j+6)

      return
      end


*     ====================================================================== 160
*     add stem for output file for GIS ... .CSV
      subroutine trunk37 (kay) ! add stem for output file for GIS ... .CSV

      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'Q'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'C'
      a(j+5) = 'S'
      a(j+6) = 'V'

      write (fname,5) (a(i),i=1,j+6)

      return
      end ! ================================================================ 160

      
*     ====================================================================== 160      
*     add stem for file used to set uo GIS.CSV data for discharges
      subroutine trunk38 (kay) ! add stem for files Qi.BAK

      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'
      
      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'Q'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'W'
      a(j+5) = 'A'
      a(j+6) = 'K'

      write (fname,5) (a(i),i=1,j+6)

      return
      end  
*     ====================================================================== 160

  
*     ====================================================================== 160      
*     add stem for file used to set uo GIS.CSV data for discharges
      subroutine trunk39 (kay) ! add stem for files Qi.BAK

      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'
      
      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'O'
      a(j+5) = 'U'
      a(j+6) = 'T'

      write (fname,5) (a(i),i=1,j+6)

      return
      end  
*     ====================================================================== 160

      
*     add stem for for output file for GIS ... DAi.CSV
      subroutine trunk27 (kay) ! add stem for output file for GIS ... DAi.CSV
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'D'
      a(j+2) = 'A'
      a(j+3) = b(kay)
      a(j+4) = '.'
      a(j+5) = 'C'
      a(j+6) = 'S'
      a(j+7) = 'V'

      write (fname,5) (a(i),i=1,j+7)

      return
      end

      
*     add stem for for output file for GIS ... WBi.CSV
      subroutine trunk28 (kay) ! add stem for output file for GIS ... WBi.CSV
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'W'
      a(j+2) = 'B'
      a(j+3) = b(kay)
      a(j+4) = '.'
      a(j+5) = 'C'
      a(j+6) = 'S'
      a(j+7) = 'V'

      write (fname,5) (a(i),i=1,j+7)

      return
      end

      

*     add stem for output file for apportionment ... .CSV
      subroutine trunk8 (kay)
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '-'
      a(j+1) = 'E'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'C'
      a(j+5) = 'S'
      a(j+6) = 'V'

      write (fname,5) (a(i),i=1,j+6)

      return
      end


*     add stem for output file for apportionment ... .CSV
      subroutine trunk15 (kay)
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '+'
      a(j+1) = 'G'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'c'
      a(j+5) = 's'
      a(j+6) = 'v'

      write (fname,5) (a(i),i=1,j+7)

      return
      end


*     add stem for output file for apportionment ... .CSV
      subroutine trunk16 (kay)
      include 'COMMON DATA.FOR'
      character*1 a(137)
      character*1 b(10)

      b(1) = '1'
      b(2) = '2'
      b(3) = '3'
      b(4) = '4'
      b(5) = '5'
      b(6) = '6'
      b(7) = '7'
      b(8) = '8'
      b(9) = '9'
      b(10) = '0'

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
    
      do 1 jj=1,136
      j=137-jj
    7 continue

      if ( a(j) .eq. '.' )goto 9
      if ( j .gt. 1 ) then
      do k = j,136
      a(k) = a(k+1)
      enddo
      endif

    1 continue
    9 continue

      a(j) = '+'
      a(j+1) = 'W'
      a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'c'
      a(j+5) = 's'
      a(j+6) = 'v'

      write (fname,5) (a(i),i=1,j+7)

      return
      end

      
*     add stem for output file .GIS1.CSV ---------------------------------------
      subroutine trunk2 ! add stem for output file .GIS1.CSV
      include 'COMMON DATA.FOR'
      character*1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
      do 1 jj=1,136
      j=137-jj
      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    9 continue

      write (fname,5) (a(i),i=1,j-1),'-','G','I','S','1','.','C','S','V'

      return
      end

 
*     add stem for output file for effluents ... .EFF
      subroutine trunk3 ! add stem for output file for effluents ... .EFF
      include 'COMMON DATA.FOR'
      character*1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137)=' '
      do 1 jj=1,136
      j=137-jj
      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    9 continue

      write (fname,5) (a(i),i=1,j-1),'-','E','F','F','.','C','S','V'

      return
      end

*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for globaldata
      include 'COMMON DATA.FOR'
      if ( folder .eq. '    ' ) then
      write (FNAME,2) Globaldataname
    2 format(A30)
      call blank ! eliminate blanks in filenames
      else
      write (FNAME,1) folder, Globaldataname
    1 format(a100,A30)
      call blank ! eliminate blanks in filenames
      endif
      Globaldataname = fname
      return
      end

      
      
*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for switches
      include 'COMMON DATA.FOR'
      if ( folder .eq. '    ' ) then
      write (FNAME,2) Switchesname
    2 format(A30)
      call blank ! eliminate blanks in filenames
      else
      write (FNAME,1) folder, Switchesname
    1 format(a100,A30)
      call blank ! eliminate blanks in filenames
      endif
      Switchesname = fname
      return
      end


*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for output 0
      include 'COMMON DATA.FOR'
      if ( folder .eq. '    ' ) then
      write (FNAME,2) 'TEMP'
    2 format(A30)
      call blank ! eliminate blanks in filenames
      else
      write (FNAME,1) folder,'TEMP'
    1 format(a100,A30)
      call blank ! eliminate blanks in filenames
      endif

      output_folder = fname
      return
      end
      
      
*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for output 1
      include 'COMMON DATA.FOR'
      character *07 SET    
       
      call GETTIM (IHR,IMIN,ISEC,IHUN) ! obtain the time -----------------------
      write(SET,5533)imin,isec,ihun
 5533 format(4i2)
      
      if ( ifbatch .eq. 1 ) icode = model number in batch
      if ( folder .eq. '    ' ) then
      write(FNAME,2) SET
    2 format(a6)
      call blank ! eliminate blanks in filenames
      else
      write(FNAME,1) folder,SET
    1 format(a100,a6)
      call blank ! eliminate blanks in filenames
      endif
      output_folder = fname
      
      return
      end

      
      
*     ====================================================================== 160
*     add stem for storage file for reach loads ... .LOD etc =============== 160
      subroutine trunkout (ch1,ch2,ch3)
      include 'COMMON DATA.FOR'
      character *1 ch1,ch2,ch3
      character *1 a(137),n(6)
      character *6 num
           
      read(fname,5)(a(i),i=1,136)
    5 format(136A1)
      write(num,6)ireach
    6 format(i6)
      read(num,7)(n(i),i=1,6)
    7 format(6A1)
      
      do i = 1, 6
      if ( n(i) .eq. ' ' ) n(i) = '0'     
      enddo
      
      a(137) = ' '
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .ne. ' ') goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      write (fname,5)(a(i),i=1,j)
      
      write (fname,5)(a(i),i=1,j),'\',
     &'R','e','a','c','h',(n(i),i=1,6),'.',
     &(a(i),i=j+1,86)
      
      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue
      write (fname,5) (a(i),i=1,j-1),'.',ch1,ch2,ch3
            
      return
      end
*     ====================================================================== 160
      

*     ====================================================================== 160
*     add stem for storage file for reach loads ... .LOD etc
      subroutine trunkoutC (idet,ch1,ch2,ch3)
      include 'COMMON DATA.FOR'
      character *1 ch1,ch2,ch3
      character *1 a(137),n(3)
      character *6 num
           
      read(fname,5)(a(i),i=1,136)
    5 format(136A1)
      write(num,6)idet
    6 format(i3)
      read(num,7)(n(i),i=1,3)
    7 format(6A1)
      
      do i = 1, 3
      if ( n(i) .eq. ' ' ) n(i) = '0'     
      enddo
      
      a(137) = ' '
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .ne. ' ') goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      write (fname,5)(a(i),i=1,j)
      
      write (fname,5)(a(i),i=1,j),'\',
     &'Q',(n(i),i=1,3),'.',
     &(a(i),i=j+1,86)
      
      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue
      write (fname,5) (a(i),i=1,j-1),'.',ch1,ch2,ch3
            
      return
      end
*     ====================================================================== 160

      
      
      
*     ====================================================================== 160      
*     add stem for output file for apportionment ... .ADL etc
      subroutine trunkoutB (irch,ch1,ch2,ch3)
      include 'COMMON DATA.FOR'
      character *1 ch1,ch2,ch3
      character *1 a(137),n(6)
      character *6 num
      
      read(fname,5)(a(i),i=1,136)
    5 format(136A1)
      write(num,6)irch
    6 format(i6)
      read(num,7)(n(i),i=1,6)
    7 format(6A1)
      
      do i = 1, 6
      if ( n(i) .eq. ' ' ) n(i) = '0'     
      enddo
      
      a(137) = ' '
      do 1 jj = 1, 136
      j = 137 - jj
      if ( a(j) .eq. '\') goto 8
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif
    1 continue
    8 continue
      
      write (fname,5)(a(i),i=1,j),'T','e','m','p','\',
     &'R','e','a','c','h',(n(i),i=1,6),'.',
     &(a(i),i=j+1,119)
      
      read(fname,5) (a(i),i=1,136)

      a(137) = ' '
      do 3 jj = 1, 136
      j = 137 - jj

      if ( a(j) .eq. '.') goto 9
      if ( j .gt. 1 ) then
      do k = j, 136
      a(k) = a(k+1)
      enddo
      endif

    3 continue
    9 continue
      write (fname,5) (a(i),i=1,j-1),'.',ch1,ch2,ch3

      return
      end
*     ====================================================================== 160