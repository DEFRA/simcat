*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File name: DIRECT.FOR (1907 lines)
*     --------------------------------------------------------------------------

*     attach the main SIMCAT data file -----------------------------------------
      subroutine attach SIMCAT data file for catchment
      include 'COM.FOR'
      logical exists

*     read the name of the data file -------------------------------------------
	read(23,3,err=9999,end=9999)DatName
    3 format(a136)

      write (fname,99) datname
   99 format(a136)
      call trunk ('E','N','D')
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
 9998 format('*** Error in reading RUNDAT.TMP '/
     &'*** Calculation stopped ...')
	call stop
      end




*     attach the file on global data changes -----------------------------------
      subroutine read the global data file
      include 'COM.FOR'
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
      
      Globaldataname='Globalpolicydata.GPD'
      call add folder for globaldata
      inquire ( FILE = Globaldataname, EXIST = exists )
      if ( .NOT. exists ) return

      open(188,FILE = Globaldataname, STATUS = 'OLD')

*     specify the use of the existing gap filling files if [calspecial] = 0 ----
*     otherwise renew the gap filling ------------------------------------------
      calspecial = 0
	read(188,*,ERR=999)calspecial
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
	read(188,*,ERR=999)iforce gap fill
      if ( ical .eq. 0 ) iforce gap fill = 0

*     produce large output files when nobigout = 0 -----------------------------
*     especially the files of GIS data -----------------------------------------
*     ==========================================================================
      nobigout = 0
	noGIS = nobigout

      close (188)    
      return
  999 continue
      write( *,900)
  900 format('Error in reading the file Global policy data.GPD ...')
      call stop
      end



*     ==========================================================================
*     Attach files =============================================================
*     ==========================================================================
*     Output channels.......
*     ..01   main and detailed output:                   filename.OUT
*     ..02   main data file:                                     .DAT
*     ..03   summary output and report on gap filling:           .SUM
*     ..08   input data:                                         .INP
*     ..09   copy of screen output:                              .SCN
*     ..22   results for graph plotting (VISUAL BASIC):          .SGR
*     ..27   results on loads:                                   .LOD
*     ..30   results on compliance with target (Batch runs):     .TGT 
*     ..33   record of possible errors found:                    .ERR 
*     ..34   information on natural purification:                .PUR 
*     ..35   output on water quality planning:                    WQP
*     ..37   output on water quality planning:                    WQC
*     ..40   results on apportionment                            .APT 
*     ..42   output for GIS                                      .GIS
*     ..44   output for GIS                                      .GI2
*     ..48   output output on classification:                    .WFD
*     ..64   output on costs                                     .COS
*     ..72   output on river flows                               .FLO
*     ..74   working file for flow gap filling:                  .FCL
*     ..75   working file for quality gap filling:               .QCL
*     ..64
*       90
*       91
*       92
*       93
*       94
*       95
*       96
*       97
*       98
*       99
*      101-110                  Files for output on apportionment.MON 
*      111-120                  Files for output on apportionment.CSV 
*      124
*      125
*     ==========================================================================

      subroutine attach files and sort out gap filling
      include 'COM.FOR'
	character *01 a(132)
      character *132 RECORD
      logical exists

*     open the main output file ------------------------------------------------
      write (fname,99) datname
   99 format(a136)
      call trunk ('O','U','T')
	open(01,FILE=FNAME)
	write(01,*)' '
      
*     open screen output file --------------------------------------------------
      write (fname,99) datname
      call trunk ('I','N','P')
	open(08,FILE=FNAME)

*     open screen output file --------------------------------------------------
      write (fname,99) datname
      call trunk ('S','C','N')
	open(09,FILE=FNAME)

*     open summary output file -------------------------------------------------
      write (fname,99) datname
      call trunk ('S','U','M')
	open(03,FILE=FNAME)
	write(03,*)' '

*     open output file for the results of calculations of load -----------------
      write (fname,99) datname
      call trunk ('L','O','D')
	open(27,FILE=FNAME)
	write(27,*)' '

*     file for tidy output .OWT ----------------------------------------------21
      write (fname,99) datname
      call trunk ('O','W','T')
	open(21,FILE=FNAME)
	write(21,*)' '

*     file for outputs to errors .ERR ... for this data file -----------------33
      write (fname,99) datname
      call trunk ('E','R','R')
	open(33,FILE=FNAME) ! outputs on errors
	write(33,*)' '

*     first file for outputs to .GIS ... for this data file ------------------42
      write (fname,99) datname
      call trunk2
	open(42,FILE=FNAME) ! GIS ... for particular model
	write(42,*)' '
	backspace(42)
      if ( n146 .eq. 2 ) then ! heading 2222222222222222222222222222222222222222
      write(42,8200)bday,bmon,IYEAR ! 222222222222222222222222222222222222222222
 8200 format('SIMCAT',',','Version 14.7 ',',', ! 2222222222222222222222222222222
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',') ! 2222222222
      endif ! if ( n146 .eq. 2 ) heading 222222222222222222222222222222222222222
      if ( n146 .eq. 3 ) then ! heading 3333333333333333333333333333333333333333
      write(42,8250)bday,bmon,IYEAR
 8250 format('SIMCAT',',','Version 14.8e',',', ! 3333333333333333333333333333333
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',')
      endif ! if ( n146 .eq. 3 ) 33333333333333333333333333333333333333333333333
      if ( n146 .eq. 4 ) then ! heading 4444444444444444444444444444444444444444
      write(42,8255)bday,bmon,IYEAR
 8255 format('SIMCAT',',','Version 14.9 ',',', ! 4444444444444444444444444444444
     &'Date of run: ',a2,'/',a2,'/',i4,',','Mode: ',i1,',',a70,',')
      endif ! if ( n146 .eq. 4 ) 44444444444444444444444444444444444444444444444

*     open output file for the results of calculations of compliance with ------
*     targets ------------------------------------------------------------------
      write (fname,99) datname
      call trunk ('T','G','T')
	open(30,FILE=FNAME)
	write(30,*)' '

*     open the file for Visual Basic graphics ----------------------------------
      write (fname,99) datname
      call trunk ('S','G','R')
	open(22,FILE=FNAME) ! open the file for Visual Basic graphics ------------

*     open the gap filling files -----------------------------------------------
*     prepare to open the working file for flow gap filling --------------------
      write (fname,99) datname
      call trunk ('F','C','L')
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
      
*     assume by default that gap filling is required in modes 7 and 8 ==========
      no gap filling 78 = 0 ! initialise gap filling as required ===============
*     check whether the working file for flow gap filling exists ===============
      inquire( FILE = Fname, EXIST = exists )
*     allow mode 7 and mode 8 to operate without gap filling ===================
      if ( .NOT. exists) then ! the file does not exist ========================
      if ( ical .eq. 04 .or. ical .eq. 07 .or. ical .eq. 08 
     &                  .or. ical .eq. 09 ) then
	no gap filling 78 = 1 ! no gap filling will be done ======================
      if ( ical .eq. 4 ) ical = 0
	return !  do not open the files .FCL and .QCL ============================
	endif ! if ( ical .eq. 04, 07, 08 or 09 ) ================================
	endif ! if ( .NOT. exists) ===============================================
      
*     open the working file for gap filling of river flows ---------------------
      if ( ical .ne. 0 ) open(74,FILE = Fname)

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
      call trunk ('Q','C','L')
      read(Fname,7)(a(i),i=1,132)

      if ( ical .eq. 04 .or. ical .eq. 07 .or. ical .eq. 08 
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
      endif ! if ( ical .eq. 04, 07, 08 or 09 ) 
 	write (Fname,7)(a(i),i=1,132) 
      
*     open the working file for gap filling of river quality -------------------
	if ( ical .ne. 0 ) open(75,FILE = fname)
      return

  999 continue
      return

 9998 write(33,3901)
      write(01,3901)
      write(09,3901)
      write( *,3901)
 3901 format('*** Error in setting up gap filling for river ',
     &'flows ...'/'*** CALCULATION STOPPED.....')
      rewind 74
      call stop
            
 9997 continue
      write(33,3900)
      write(01,3900)
      write(09,3900)
 	call change colour of text (22) ! light blue
      write( *,3910)
 3910 format('* There is no gap filling file for river ',
     &'flows     ...       a basic run will be performed ...')
      call set screen text colour
 3900 format('*** There is no gap filling file for river ',
     &'flows ... a basic run will be performed ...')
      rewind 74
      ical = 0
      return

      end






*     eliminate blanks in the filenames ----------------------------------------
      subroutine blank
      include 'COM.FOR'
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
      subroutine leadblanks
      include 'COM.FOR'
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

*     eliminate name of folder -------------------------------------------------
      subroutine lose folder
      include 'COM.FOR'
      character *1 a(130),b(130)

      read(datfilename,5) (a(i),i=1,130)
    5 format(130A1)

      do i=1,130
	j=130-i+1
	b(i)=a(j)
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

*     eliminate blanks in the filenames ----------------------------------------
      subroutine blank middle
      include 'COM.FOR'
      character*1 a(137)

      read(fname,5) (a(i),i=1,136)
    5 format(136A1)

      a(137) = ' '
	    
      j= 0
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

      subroutine blank middle 2
      include 'COM.FOR'
      character*1 a(167)

      read(fname2,5) (a(i),i=1,166)
    5 format(166A1)

      a(167) = ' '
	    
      j= 0
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

      subroutine blank3
      include 'COM.FOR'
      character *1 a(167)

      read(fname2,5) (a(i),i=1,166)
    5 format(166A1)

      a(167)=' '
      do 1 jj=1,166
      j=167-jj
    7 continue

      if ( a(j) .eq. ' ' ) then
      if ( j .gt. 1 ) then
      do k = j,166
      a(k) = a(k+1) 
	enddo
      endif
      endif

    1 continue
    9 continue

      write (fname2,5) (a(i),i=1,166)
      return
      end




*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for non parametric data file (ichan)
      include 'COM.FOR'
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
      call blank
      else
      write (FNAME,1) folder, Flname(ichan,nonpd)
    1 format(a100,A30)
      call blank
      endif

      Flname (ichan,nonpd) = fname
      return
      end



*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for monthly data file (ichan)
      include 'COM.FOR'
   
      Flmonthsmall (ichan,seasd) = Flmonth (ichan,seasd)

      if ( folder .eq. '    ' ) then

      if ( ichan .eq. 1 ) write (FNAME,2) flmonth(1,seasd)
      if ( ichan .eq. 2 ) write (FNAME,2) flmonth(2,seasd)
      if ( ichan .eq. 3 ) write (FNAME,2) flmonth(3,seasd)
      if ( ichan .eq. 4 ) write (FNAME,2) flmonth(4,seasd)
    2 format(A30)
      call blank
      else
      if ( ichan .eq. 1 ) write (FNAME,1) folder, flmonth(1,seasd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, flmonth(2,seasd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, flmonth(3,seasd)
      if ( ichan .eq. 4 ) write (FNAME,1) folder, flmonth(4,seasd)
    1 format(a100,A30)
      call blank
      endif

      if ( ichan .eq. 1 ) flmonth(1,seasd) = fname
      if ( ichan .eq. 2 ) flmonth(2,seasd) = fname
      if ( ichan .eq. 3 ) flmonth(3,seasd) = fname
      if ( ichan .eq. 4 ) flmonth(4,seasd) = fname

      return
      end




*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for monthly structure file (ichan)
      include 'COM.FOR'

      FLSTRUCTsmall (ichan,struckd) = FLSTRUCT (ichan,struckd)
      if ( folder .eq. '    ' ) then
      if ( ichan .eq. 1 ) write (FNAME,2) FLSTRUCT(1,struckd)
      if ( ichan .eq. 2 ) write (FNAME,2) FLSTRUCT(2,struckd)
      if ( ichan .eq. 3 ) write (FNAME,2) FLSTRUCT(3,struckd)
      if ( ichan .eq. 4 ) write (FNAME,2) FLSTRUCT(4,struckd)
    2 format(A30)

      call blank
      else

      if ( ichan .eq. 1 ) write (FNAME,1) folder, FLSTRUCT(1,struckd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, FLSTRUCT(2,struckd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, FLSTRUCT(3,struckd)
      if  (ichan .eq. 4 ) write (FNAME,1) folder, FLSTRUCT(4,struckd)
    1 format(a100,A30)

      call blank
      endif

      if ( ichan .eq. 1 ) FLSTRUCT(1,struckd) = fname
      if ( ichan .eq. 2 ) FLSTRUCT(2,struckd) = fname
      if ( ichan .eq. 3 ) FLSTRUCT(3,struckd) = fname
      if ( ichan .eq. 4 ) FLSTRUCT(4,struckd) = fname
      return
      end




*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for monthly background (ichan)
      include 'COM.FOR'

      FLtempsmall (ichan,tempd) = fltemp (ichan,tempd)
      if ( folder .eq. '    ' ) then
      if ( ichan .eq. 1 ) write (FNAME,2) fltemp(1,tempd)
      if ( ichan .eq. 2 ) write (FNAME,2) fltemp(2,tempd)
      if ( ichan .eq. 3 ) write (FNAME,2) fltemp(3,tempd)
      if ( ichan .eq. 4 ) write (FNAME,2) fltemp(4,tempd)
    2 format(A30)

      call blank
      else

      if ( ichan .eq. 1 ) write (FNAME,1) folder, fltemp(1,tempd)
      if ( ichan .eq. 2 ) write (FNAME,1) folder, fltemp(2,tempd)
      if ( ichan .eq. 3 ) write (FNAME,1) folder, fltemp(3,tempd)
      if ( ichan .eq. 4 ) write (FNAME,1) folder, fltemp(4,tempd)
    1 format(a100,A30)

      call blank
      endif

      if ( ichan .eq. 1 ) fltemp(1,tempd) = fname
      if ( ichan .eq. 2 ) fltemp(2,tempd) = fname
      if ( ichan .eq. 3 ) fltemp(3,tempd) = fname
      if ( ichan .eq. 4 ) fltemp(4,tempd) = fname

      return
      end







*     deal with non-standard correlation ---------------------------------------
      subroutine rfcorr
      include 'COM.FOR'

      jtrim = 0
      nscorr = 0

*     loop on all the flow data-sets -------------------------------------------
      do 2 lf = 1, nf

*     check whether flow data have been entered --------------------------------
      if ( PDRF(lf) .gt. -1 ) then

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
     &'correlation coefficients:',i6/
     &'*** SIMCAT has over-written these with the default value of ',
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
      include 'COM.FOR'

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
      xx = quolity data ( lv, idet, MO)

*     check whether the default is to be retained ------------------------------
      if ( xx .eq. -9.9 ) goto 22

*     check for illegal values -------------------------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      quolity data(lv,idet,MO) = -9.9
      goto 22
      endif

*     check for zero -----------------------------------------------------------
      if ( xx .eq. 0.0 ) then
      quolity data (lv, idet, MO ) = -9.9
      jtrim = jtrim + 1
      if ( jtrim .lt. 3 ) then
      if ( nobigout .le. 0 ) write(01,1821)lv
 1821 format(77('-')/
     &'*** A river quality dataset has a zero correlation ',
     &'coefficient',4x,'Code:',i6/
     &'*** ',24x,'This has been over-written with the default value'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      write(33,1821)lv
      endif
      goto 22
      else

      if ( nscorr .eq. 0 ) then
      write(33,1822)xx,lv
 1822 format(77('-')/
     &'*** A river quality dataset has a special correlation coeffi',
     &'cient of ',f8.4/
     &'*** This sets the correlation with river flow ...',17x,
     &'Code:',i6/77('-'))
      if ( nobigout .le. 0 ) write(01,1822)xx,lv
	endif
      nscorr = nscorr + 1
      goto 22
      endif
      endif
   22 continue

    2 continue

      if ( jtrim .gt. 2 ) then

      if ( nobigout .le. 0 ) write(01,1001)jtrim
      write(09,1001)jtrim
      write(33,1001)jtrim
 1001 format(77('-')/
     &'*** ',07x,'River quality data-sets with zero ',
     &'correlation coefficients =',i6/
     &'*** ',20x,'SIMCAT has over-written these with the default ',
     &'values'/
     &'*** ',25x,'You should use 0.00001 when you really want zero'/
     &77('-'))
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1901) nscorr 
      write(33,1901) nscorr 
      write(09,1901) nscorr 
 1901 format(
     &'*** River quality data-sets with special ',
     &'correlation coefficients ...',i8/77('-'))
      endif
      
      return
      end




*     deal with non-standard correlation for effluent flow ---------------------
      subroutine deal with non standard correlation for effluent flow
      include 'COM.FOR'

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
      write(09,1822)xx,le
 1822 format(77('-')/
     &'*** ',7x,'An effluent flow has a special correlation coeffi',
     &'cient of ',f8.4/
     &'*** ',12x,'This sets the correlation with the river flow ... ',
     &'Code:',i6/77('-'))
	write(33,1822)xx,le
      if ( nobigout .le. 0 ) write(01,1822)xx,le
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
      include 'COM.FOR'

      jtrim = 0
	nscorr = 0

*     loop on all the effluent data-sets ---------------------------------------
      do 2 le = 1, ne

      do 22 idet = 1, ndet
      if ( qtype(idet) .eq. 4 ) goto 22

*     check whether data have been entered -------------------------------------
      if ( PDEC (le,idet) .gt. -1 ) then

*     look at the correlation coefficient --------------------------------------
      xx = pollution data (le,idet,MO)

*     check whether the default is to be retained ------------------------------
      if ( xx .eq. -9.9 ) goto 22

*     check for illegal values -------------------------------------------------
      if ( xx .gt. 1.0 .or. xx .lt. -1.0 ) then
      pollution data (le,idet,MO) = -9.9
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
 1822 format(77('-')/
     &'*** ',4x,'An effluent quality has a special correlation coeffi',
     &'cient of ',f8.4/'*** ',6x,
     &'This sets the correlation with the effluent flow data   ',
     &'Code:',i6/77('-'))
      if ( nobigout .le. 0 ) write(01,1822)xx,le
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
      endif

      if ( nscorr .gt. 1 ) then
      if ( nobigout .le. 0 ) write(01,1901)nscorr 
      write(09,1901)nscorr 
      write(33,1901)nscorr 
 1901 format(
     &'*** Effluent quality data-sets with special ',
     &'correlation coefficients ...',i5/77('-'))
      endif

      return
      end




*     add stem for output file for apportionment ... .APT
      subroutine trunk (ch1,ch2,ch3)
      include 'COM.FOR'
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







*     add stem for output file for determinands ... .MON
      subroutine trunk6 (kay)
      include 'COM.FOR'
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
      a(j+1) = 'D'
	a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'M'
      a(j+5) = 'O'
      a(j+6) = 'N'

      write (fname,5) (a(i),i=1,j+6)

      return
      end

*     add stem for output file for apportionment ... .APT
      subroutine trunk9 (kay)
      include 'COM.FOR'
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
      a(j+1) = 'D'
	a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'A'
      a(j+5) = 'P'
      a(j+6) = 'T'

      write (fname,5) (a(i),i=1,j+6)

      return
      end

*     add stem for output file for apportionment ... .CAT
      subroutine trunk10 (kay)
      include 'COM.FOR'
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
      a(j+1) = 'D'
	a(j+2) = b(kay)
      a(j+3) = '.'
      a(j+4) = 'C'
      a(j+5) = 'T'
      a(j+6) = 'M'

      write (fname,5) (a(i),i=1,j+6)

      return
      end




*     add stem for output file for apportionment ... .CSV
      subroutine trunk7 (kay)
      include 'COM.FOR'
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


*     add stem for output file for apportionment ... .CSV
      subroutine trunk8 (kay)
      include 'COM.FOR'
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




*     add stem for output file for GIS ... .GIS --------------------------------
      subroutine trunk2
      include 'COM.FOR'
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


*     add stem for output file for EFF ... .GIS --------------------------------
      subroutine trunk3
      include 'COM.FOR'
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











*     add stem for output file for GIS -----------------------------------------
      subroutine cutneg (line)
      include 'COM.FOR'
      character *200 line
      character *1 a(200)
      read(fname,5) (a(i),i=1,200)
    5 format(136A1)
      do 1 j=1,9
      if (a(j) .eq. '-' ) then
	a(j) = ' '
	goto 9
      endif
    1 continue
    9 continue
      write (line,5) (a(i),i=1,200)
      return
      end

*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for globaldata
      include 'COM.FOR'
      if ( folder .eq. '    ' ) then
      write (FNAME,2) Globaldataname
    2 format(A30)
      call blank
      else
      write (FNAME,1) folder, Globaldataname
    1 format(a100,A30)
      call blank
      endif
      Globaldataname = fname
      return
      end

*     add the name of the folder to the name of the file -----------------------  
      subroutine add folder for switches
      include 'COM.FOR'
      if ( folder .eq. '    ' ) then
      write (FNAME,2) Switchesname
    2 format(A30)
      call blank
      else
      write (FNAME,1) folder, Switchesname
    1 format(a100,A30)
      call blank
      endif
      Switchesname = fname
      return
      end

      
      subroutine check for a special file of standards
      include 'COM.FOR'
      logical exists
 	if ( model number in batch .eq. 1 ) write( *,1)
    1 format(130('-'))    
      return
      end