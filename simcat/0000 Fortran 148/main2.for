*     ========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ========================================================================
*     File name: MAIN2.FOR   1999

      subroutine write river flows
      include 'COM.FOR'

      call rfcorr ! sort out correlation

      write(08,1)
    1 format(77('-')/'Data for River Flow')

      write(08,2)FUNIT
    2 format(77('-')/
     &'  Code           (1) Mean '/
     &' Number          (2) 95%-exceedence '/
     &' of Set          (3) Name of Feature'/
     &'                      (',A4,')'/
     &'                 (1)       (2)     (3)'/77('-'))

      do 3 IF=1,NF
      if (MK(IF))3,3,4
    4 MK(IF)=0
      call sort format 2 (F(IF,1),F(IF,2))
      write(08,5)IF,valchars10,valchars11,UNAME(IF)
    5 format(I6,4X,a10,a10,5X,A40)
    3 continue

      write(08,8)
    8 format(77('-'))
      return
      end



*     write out sets of data on river quality ----------------------------------
      subroutine write data on river quality (MV)
      include 'COM.FOR'
      character *25 STR/'(1)       (2)  (3)      '/
      character *10 vchars1(10),vchars2(10)

      if ( ICAL .eq. 1 ) return
      npx1 = 1
      npx4 = min0(ndet,4)
	npx = 4

      write(08,1009)
 1009 format(//105('-')/'Data for river quality ')
      write(08,1010)
 1010 format(105('-')/'    Code                 (1) Mean'/
     &'  Number                 (2) Standard deviation'/
     &'  of set                 (3) Number of Samples'/105('-'))
      write(08,1015)(DNAME(IP),IP=npx1,NPx4)
 1015 format(6x,4(7x,a11,4x))
      write(08,1915)(UNITS(IP),IP=npx1,NPx4)
 1915 format(8x,4(8x,A4,10X))
      write(08,1016)(STR,IP=npx1,NPx4)
 1016 format(/12x,4A25)
      write(08,1102)
 1102 format(105('-'))

      do IV = 1, MV
      if ( MK(IV) .ge. 1 ) then
      do JP = npx1, NPx4    
      call sort format 2 (quolity data(IV,JP,1),quolity data(IV,JP,2))
      vchars1(JP) = valchars10
      vchars2(JP) = valchars11
      enddo
      if ( npx4 .eq. npx ) then
      write(08,1011)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
 1011 format(I5,4(2a10,I5),3X,A27)
	endif
      if ( npx4 .eq. npx-1 ) then
      call sort format 2 (quolity data(IV,JP,1),quolity data(IV,JP,2))
      write(08,1012)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
 1012 format(I5,3(2a10,I5),22x,A27)
	endif
      if ( npx4 .eq. npx-2 ) then
      write(08,1013)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
 1013 format(I5,2(2a10,I5),41X,A27)
	endif
      if ( npx4 .eq. npx-3 ) then
      call sort format 2 (quolity data(IV,JP,1),quolity data(IV,JP,2))
      write(08,1014)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
 1014 format(I5,2a10,I5,78X,A27)
	endif
	endif
      enddo

 2000 continue

      if ( nPx4 .ge. NDET .or. Ndet .le. 4 ) goto 7878

      npx1 = npx1 + 4
	npx4 = npx4 + 4
      if ( npx4 .gt. ndet) npx4=ndet

      write(08,1002)
 1002 format(105('-'))

      write(08,1010)
      write(08,1015)(DNAME(IP),IP=npx1,NPx4)
      write(08,1915)(UNITS(IP),IP=npx1,NPx4)
      write(08,1016)(STR,IP=npx1,NPx4)
      write(08,1102)

      do IV=1,MV
      if ( MK(IV) .ge. 1 ) then

      do JP = npx1, NPx4    
      call sort format 2 (quolity data(IV,JP,1),quolity data(IV,JP,2))
      vchars1(JP) = valchars10
      vchars2(JP) = valchars11
      enddo
          
      if ( npx4-4 .eq. npx) then
      write(08,1011)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
	endif
      if ( npx4-4 .eq. npx-1) then
      write(08,1012)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
	endif
      if ( npx4-4 .eq. npx-2) then
      write(08,1013)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4)
	endif
      if ( npx4-4 .eq. npx-3) then
      write(08,1014)IV,(vchars1(JP),vchars2(JP),QNUM(IV,JP),
     &JP=npx1,NPx4),UNAME(IV)
	endif
	endif
	MK(IV)=0
      enddo
      
      goto 2000

 7878 write(08,8)
    8 format(105('-')//)

      return
      end


*     write out sets of data on effluent flow and quality ----------------------
      subroutine write effluent data
      include 'COM.FOR'
      character *25 STR/'  (1)       (2)  (3)     '/
      character *10 vchars1(10),vchars2(10)

      if ( ICAL .eq. 1 ) return

      npx1 = 1
      npx4 = 4
	npx = 4

      if ( MK(1) .eq. 0 ) return

      write(08,1900)
 1900 format(60('-')/'Flow Data for Discharges'/60('-')/
     &'   Code                    (1) Mean'/
     &' Number                    (2) Standard Deviation'/
     &' of Set                    (3) Number of Samples')

      write(08,1915)Funit
 1915 format(60('-')/10x,'Flow (',A4,')')
      write(08,1916)STR
 1916 format(12X,A25)
      write(08,1612)
 1612 format(60('-'))
      do IE = 1, NE
      if ( MK(IE) .ge. 1 ) then
      call sort format 2 (FE(IE,1),FE(IE,2))
      write(08,1919)IE,valchars10,valchars11,ENUM(IE),UNAME(IE)
 1919 format(i4,3X,2a10,i5,5x,A40)
      endif
      enddo
      write(08,1112)
 1112 format(60('-')/)

      npx1 = 1
      npx4 = min0(ndet,4)

      write(08,1000)
 1000 format(105('-')/'Water quality data for discharges'/
     &105('-')/'   Code                    (1) Mean '/
     &' Number                    (2) Standard Deviation '/
     &' of Set                    (3) Number of Samples')

      write(08,1015)(DNAME(IP),IP=npx1,NPx4)
 1015 format(105('-')/9X,4(3x,A11,5X))
      write(08,1995)(UNITS(IP),IP=npx1,NPx4)
 1995 format(4X,4(9x,a4,6x))
      write(08,1016)(STR,IP=npx1,NPx4)
 1016 format(10X,4A25)
      write(08,1002)
 1002 format(105('-'))

      do IE = 1, NE
      if ( MK(IE) .ge. 1 ) then

      do JP = npx1, NPx4    
      call sort format 2 (pollution data(IE,JP,1),
     &pollution data(IE,JP,2))
      vchars1(JP) = valchars10
      vchars2(JP) = valchars11
      enddo
    
      if ( npx4 .eq. npx ) then
      write(08,1019)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
 1019 format(i4,1X,4(2a10,I5),3x,A40)
      endif

      if ( npx4.eq. npx - 1 ) then
      write(08,1719)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
 1719 format(i4,1X,3(2a10,I5),28x,A40)
      endif

      if ( npx4 .eq. npx - 2 ) then
      write(08,1718)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
 1718 format(i4,1X,2(2a10,I5),53x,A40)
      endif

      if ( npx4 .eq. npx - 3 ) then
      write(08,1717)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
 1717 format(i4,1X,2a10,I5,78x,A40)
      endif
	endif
      enddo

      write(08,1102)
 1102 format(105('-'))
 2000 continue

      if ( nPx4.eq.NDET .or. NDET.le.4) return

      npx1 = npx1 + 4
	npx4 = npx4 + 4
	npx = npx + 4
      if ( npx4 .gt. ndet) npx4=ndet

      write(08,1015)(DNAME(IP),IP=npx1,NPx4)
      write(08,1995)(UNITS(IP),IP=npx1,NPx4)
      write(08,1016)(STR,IP=npx1,NPx4)
      write(08,1002)

      do 66 IE=1,NE
      if ( MK(IE) .lt. 1 ) goto 66

      do JP = npx1, NPx4    
      call sort format 2 (pollution data(IE,JP,1),
     &pollution data(IE,JP,2))
      vchars1(JP) = valchars10
      vchars2(JP) = valchars11
      enddo

      if ( npx4 .eq. npx ) then
      write(08,1019)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
      endif

      if ( npx4 .eq. npx - 1 ) then
      write(08,1719)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
      endif

      if ( npx4 .eq. npx - 2 ) then
      write(08,1718)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
      endif

      if ( npx4 .eq. npx - 3 ) then
      write(08,1717)IE,(vchars1(JP),vchars2(JP),
     &PNUM(IE,JP),JP=npx1,NPx4),UNAME(IE)
      endif

   66 continue

      write(08,1102)
      goto 2000

      return
      end


*     write out sets of data on river quality targets --------------------------
      subroutine write river targets
      include 'COM.FOR'

      if ( ICAL .eq. 1 ) return
      
*     initialise data on river quality targets for plotting -------------------- 
      do JP = 1, ndet
*     if ( qtype(JP) .ne. 4 ) RQO(JP) = RQS(1,JP)
      enddo

      if ( NQ .eq. 0 ) return
      if ( NOTARG .eq. 1) return

      npx1 = 1
      npx4 = 4
	npx = 4
      if ( npx4 .gt. ndet) npx4=ndet

      XXX = 0.0
      do IQ = 1, NQ
      do IP = npx1, NPx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      enddo

      if ( XXX .ge. 1.0E-10 ) then
      write(08,8261)
 8261 format(//77('-')/
     &'River Quality Targets'/77('-')/
     &'   Code                 Concentrations'/
     &' Number                 --------------'/' of Set')

      write(08,8000)(DNAME(IP),IP=npx1,NPx4)
 8000 format(77('-')/15X,4(3x,A11,2x))
      write(08,1915)(UNITS(IP),IP=npx1,NPx4)
 1915 format(14X,4('      ',A4,' ',5X))
      write(08,4102)
 4102 format(77('-'))

      do 8264 IQ = 1, NQ
      XXX = 0.0
      do IP = npx1,NPx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      if  (XXX .lt. 1.0E-10 ) goto 8264
      if ( npx4 .eq. npx ) then
	write(08,8265)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
 8265 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-1 ) then
	write(08,8365)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
 8365 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-2 ) then
	write(08,8465)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
 8465 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
      if ( npx4 .eq. npx-3 ) then
	write(08,8565)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
 8565 format(I4,6X,F15.1,F16.1,F16.2,F16.1)
      endif
 8264 continue

      write(08,4002)
 4002 format(77('-'))
      else
      write(08,8461)
 8461 format(//77('-')/
     &'No River Quality Targets were entered '/77('-'))
      endif

 2000 continue
      if ( NPx4 .eq. NDET .or. NDET .le. 4 ) return

      npx1= npx1 + 4
	npx4= npx4 + 4
      if ( npx4 .gt. ndet) npx4=ndet

      write(08,8261)
      write(08,8000)(DNAME(IP),IP=npx1,NPx4)
      write(08,1915)(UNITS(IP),IP=npx1,NPx4)
      write(08,4102)

      do IQ = 1, NQ
      XXX = 0.0
      do IP = npx1, NPx4
      XXX = XXX + RQS(IQ,IP)
      enddo
      if ( XXX .gt. 1.0E-10 ) write(08,8265)IQ,(RQS(IQ,IP),IP=npx1,NPx4)
      enddo
      write(08,4002)
      goto 2000

      return
      end


*     write out data on features -----------------------------------------------
      subroutine write the features
      include 'COM.FOR'

      mode05 = 0
      mode08 = 0

*     check the use of data on river flows -------------------------------------
      do i = 1, MU
	IF = JF(i)
	if ( IF .gt. 0 ) then
      if ( PDRF(IF) .eq. 5 ) mode05 = mode05 + 1
      if ( PDRF(IF) .eq. 8 ) mode08 = mode08 + 1
	endif
	enddo

*     check the use of data on disharge flow -----------------------------------
      do i = 1, MU
	IQ = JQ(i)
	IF = JF(i)
	if ( IQ .gt. 0 .and. IF .eq. 0 .and. JT(i) .ne. 1) then
      if ( PDEF(IQ) .eq. 5 ) mode05 = mode05 + 1
      if ( PDEF(IQ) .eq. 8 ) mode08 = mode08 + 1
	endif
	enddo

*     check the use of data on river quality -----------------------------------
      do i = 1, MU
	IQ = JQ(i)
	IF = JF(i)
	do j = 1, ndet
	if ( IQ .gt. 0 .and. IF .gt. 0 ) then
	if ( qtype (j) .ne. 4 .and. JT(i) .ne. 1 ) then
      if ( PDRC(IQ,j) .eq. 5 ) mode05 = mode05 + 1
      if ( PDRC(IQ,j) .eq. 8 ) mode08 = mode08 + 1
	endif
	endif
	enddo
	enddo

*     check the use of the data on discharge river quality ---------------------
      do i = 1, MU
	IQ = JQ(i)
	IF = JF(i)
	do j = 1, ndet
	if ( IQ .gt. 0 .and. IF .eq. 0 .and. JT(i) .ne. 1) then
	if ( qtype (j) .ne. 4 ) then
      if ( PDEC(IQ,j) .eq. 8 ) mode05 = mode05 + 1
      if ( PDEC(IQ,j) .eq. 8 ) mode08 = mode08 + 1
	endif
	endif
	enddo
	enddo

*     distribution types 5 or 8 - monthly structure ----------------------------
      if ( mode08 .gt. 0 .or. mode05 .gt. 0 ) then
      if ( munthly structure .eq. 0 ) then
      xcheck = float (NS) / 365.0
      icheck = xcheck
	xcheck = xcheck - icheck 
      if ( xcheck .gt. 1.0e-5 .or. xcheck .lt. -1.0e05 ) then
	NUS = 365 * (icheck + 1)
      if ( NUS .gt. MS) then
      xcheck = float (MS) / 365.0
      icheck = xcheck
	NUS = 365 * icheck
	endif
*     having noted the use of type 8 data, impose a monthly structure on annual 
*     data ---------------------------------------------------------------------
      munthly structure = 1 ! type 5 or 8 found for any distribution
	call change colour of text (34) ! dull yellow
      write( *,3866)NS,NUS
 3866 format('* Data entered with a monthly structure',
     &12x,'...       The number of shots is',i6,16x,
     &'This was set to',i6,' ...') 
      call set screen text colour
      write(01,3266)NS,NUS
      write(33,3266)NS,NUS
      write(08,3266)NS,NUS
      write(09,3266)NS,NUS
 3266 format(/123('-')/
     &'Data entered that has a monthly structure ... ',
     &'the number of shots is',i6,
     &' ... this should be a multiple of 365 ... '/
     &'The number of shots has been re-set to',i6/123('-')/) 
      NS = NUS
      call set up default random normal deviates
	endif
      endif
      endif

*     ensure names of features are all unique ----------------------------------
      do itemp = 1,MU
      write(TITLE,3743)UNAME(itemp),itemp
 3743 format(A40,I4)
      write(UNAME(itemp),3744)TITLE
 3744 format(A44)
      enddo

      write(08,1036)MU,funit
 1036 format(//100('-')/'List of Features ...'i7/100('-')/
     &'Type  Name of Feature                    ',
     &'Flow (',A4,')          Flow and Quality   Reach'/
     &'----  ---------------                    ',
     &'-----------          Data-sets          -----'/
     &'                                         ',
     &'Mean  95-percentile ----------------'/
     &'                                          ',
     &'     or standard                    '/
     &'                                          ',
     &'     deviation                      ')

      write(08,1002)
 1002 format(100('-'))

      do 81 itemp = 1, MU ! loop on features
      IT = JT(itemp)

*     Branch to feature type:

*          (1) Monitoring point
*          (2) Stream or tributary
*          (3) Sewage treatment works
*          (4) River flow gauge
*          (5) Industrial discharge
*          (6) Plotting point
*          (7) Continuous abstraction
*          (8) Weir
*          (9) Flow regulation point
*         (10) Boundary point
*         (12) Intermittent discharge (12)
*         (13) Switch on Diffuse Pollution
*         (14) Swtich off Diffuse Pollution
*         (15) Switch on Diffuse Pollution
*         (16) Switch off Diffuse Pollution
*         (17) Discharge with zero flow
*         (18) Abstraction (negative discharge - mean and 95-percentile)
*         (19) Abstraction (negative discharge - mean and standard deviation)
*         (20) Bifurcation: abstraction of river flow distribution - first branch
*         (21) Bifurcation: abstraction of river flow distribution - second branch
*         (22) Bifurcation: abstraction of effluent flow distribution - first branch
*         (23) Bifurcation: abstraction of effluent flow distribution - second branch
*         (24) Sub-catchment boundary
*         (25) Switch on Diffuse Pollution  - agriculture (livestock)
*         (26) Switch off Diffuse Pollution - agriculture (livestock)
*         (27) Switch on Diffuse Pollution  - agriculture (arable)
*         (28) Switch off Diffuse Pollution - agriculture (arable)
*         (29) Switch on Diffuse Pollution  - highways
*         (30) Switch off Diffuse Pollution - highways
*         (31) Switch on Diffuse Pollution  - urban 
*         (32) Switch off Diffuse Pollution - urban
*         (33) Switch on Diffuse Pollution  - atmospheric deposition
*         (34) Switch off Diffuse Pollution - atmospheric deposition
*         (35) Switch on Diffuse Pollution  - background
*         (36) Switch off Diffuse Pollution - background
*         (46) Switch on Diffuse Pollution  - mines
*         (47) Switch off Diffuse Pollution - mines
*         (48) Switch on Diffuse Pollution  - birds, boats and angling
*         (49) Switch off Diffuse Pollution - birds, boats and angling
*         (37) Switch on Diffuse Pollution  - septic tanks
*         (38) Switch off Diffuse Pollution - septic tanks
*         (39) Mine waters
*         (40) Switch on Aggregated CSOs
*         (41) Switch off Aggregated CSOs
*         (42) Switch on Aggregated sewage works
*         (43) Switch off Aggregated sewage works
*         (44) Flow into lakes
*         (45) Flow from lake

*     tributary, stream or upstream boundary --------------------------------------
      if ( it .eq. 2 .or. it .eq. 10 .or. it .eq. 45 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
 1238 format(I4,2X,A32,F7.2,F9.2,3x,i9,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      endif

*     diffuse inputs -----------------------------------------------------------
      if ( it .eq. 25 .or. it .eq. 27 .or. 
     &     it .eq. 29 .or. it .eq. 31 .or.
     &     it .eq. 33 .or. it .eq. 35 .or.
     &     it .eq. 46 .or. it .eq. 15 .or. 
     &     it .eq. 40 .or. it .eq. 42 .or. it .eq. 48 .or.
     &     it .eq. 37 .or. it .eq. 13 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      endif
*     end of diffuse inputs ----------------------------------------------------      
      if ( it .eq. 26 .or. it .eq. 28 .or. 
     &     it .eq. 30 .or. it .eq. 32 .or.
     &     it .eq. 34 .or. it .eq. 36 .or.
     &     it .eq. 41 .or. it .eq. 43 .or.
     &     it .eq. 47 .or. it .eq. 16 .or. it .eq. 49 .or.
     &     it .eq. 38 .or. it .eq. 14) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     effluent discharge -------------------------------------------------------
*     apply to [3] [5] [12] [39] [15] [42] ----------------------------- writing
      if ( it .eq. 3 .or. it .eq. 5 .or. it .eq. 12 .or.
     &     it .eq. 15 .or. it .eq. 39 .or. it .eq. 42 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NE ) then
      write(08,1038)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),'-',IQ,
     &rname(jreach(itemp))
 1038 format(I4,2X,A32,F7.2,F9.2,11x,a1,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      endif
      if ( it .eq. 16 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif

*     monitoring point ---------------------------------------------------------
      if ( it .eq. 1 ) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      write(08,1138)it,UNAME(itemp),'-',IQ,rname(jreach(itemp))
 1138 format(I4,2X,A32,6x,'-',8x,'-',11x,a1,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      endif

*     interpolation point or discharge with zero flow --------------------------
      if ( IT .eq. 6 ) then
      write(08,2738)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
 2738 format(I4,2x,A32,6x,a1,8x,a1,11x,a1,8x,a1,6x,a16)
      endif
      if ( IT .eq. 14 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      if ( IT .eq. 16 ) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      if ( IT .eq. 17) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     flow into lake -----------------------------------------------------------
      if ( IT .eq. 44) then
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif

*     flow gauge ---------------------------------------------------------------
      if ( IT .eq. 4 ) then
      IF = JF(itemp)
      if ( IF .gt. 0 .and. IF .le. NF ) then
      write(08,7338)it,UNAME(itemp),F(IF,1),F(IF,2),IF,
     &rname(jreach(itemp))
 7338 format(I4,2X,A32,F7.2,F9.2,3x,i9,8x,'-',6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
 2138 format(I4,2x,A32,6x,a1,8x,a1,11x,a1,8x,a1,6x,a16)
      endif
      endif

*     abstraction --------------------------------------------------------------
      if ( IT .eq. 7 ) then
      IF = JF(itemp)
      if ( IF .lt. 1 .or. IF .gt. NF ) then
      write(08,2238)it,UNAME(itemp),IF,rname(jreach(itemp))
 2238 format(I4,2X,A32,6X,'-',8X,'-',3x,i9,8x,'-',6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
	endif
      endif

*     abstraction (the negative discharge - river flow type (18) ---------------
      if ( IT .eq. 18 ) then
      IF = JF(itemp)
      if ( IF .gt. 0 .and. IF .le. NF ) then
      write(08,1238)it,UNAME(itemp),F(IF,1),F(IF,2),IF,IQ,
     &rname(jreach(itemp))
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
	endif

*     abstraction (the negative discharge - effluent type (19) -----------------
      if ( IT .eq. 19 ) then
      IQ = JQ(itemp)  
      if ( IQ .gt. 0 .and. IQ .le. NE ) then
      write(08,1838)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1838 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     weir ---------------------------------------------------------------------
      if ( IT .eq. 8 ) then
      IQ = JQCAL(itemp)
      if ( IQ .gt. 1 .and. IQ .le. NV ) then
      write(08,7138)it,UNAME(itemp),IQ,rname(jreach(itemp))
 7138 format(I4,2X,A32,6X,'-',8X,'-',12X,9x,I9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     flow regulation point ----------------------------------------------------
      if ( IT .eq. 9) then
      IQ = JQ(itemp)
      if ( IQ .ge. 1 .and. IQ .le. NV ) then
      IF = JF(itemp)
      if ( IF .ge. 1 .and. IF .le. NF ) then
      write(08,1058)it,UNAME(itemp),F(IF,2),IF,IQ,rname(jreach(itemp))
 1058 format(I4,2X,A32,6X,'-',F9.2,3x,i9,i9,6x,a16)
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      else
      write(08,2138)it,UNAME(itemp),'-','-','-','-',rname(jreach(itemp))
      endif
      endif

*     three types of bifurcation -----------------------------------------------
      if ( IT .eq. 18 ) then
      write(08,2630)it,UNAME(itemp),rname(jreach(itemp))
 2630 format(I4,2X,A32,6X,'-',8X,'-',20x,'-',6x,a16)
      endif
      if ( IT .eq. 20 .or. IT .eq. 22 ) then
      write(08,1638)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1638 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      endif
      if ( IT .eq. 21 .or. IT .eq. 23 ) then
      write(08,1637)it,UNAME(itemp),FE(IQ,1),FE(IQ,2),IQ,
     &rname(jreach(itemp))
 1637 format(I4,2X,A32,F7.2,F9.2,12X,I9,6x,a16)
      endif

   81 continue

      write(08,1022)
 1022 format(100('-')//)

      return
      end


*     set up arrays of default random normal deviates --------------------------
      subroutine set up default random normal deviates
      include 'COM.FOR'
      dimension IR2(MP10), IR4(MP10)

*     river flow ---------------------------------------------------------------
      IR1 = FSTART
*     effluent flow ------------------------------------------------------------
      IR3 = ESTART
*     river temperature --------------------------------------------------------
      IRT = CSTART
*     river suspended solids ---------------------------------------------------
      IRS = USTART

*     loop on determinands and set the random number starters ------------------
      do jdet = 1, ndet
*     river quality ------------------------------------------------------------
      IR2(jdet) = DSTART(jdet)
*     effluent quality ---------------------------------------------------------
      IR4(jdet) = PSTART(jdet)
      enddo

*     loop on shots ... set the uncorrelated values of the normal deviates -----
*     set the values of FRAN, ERAN, CRAN, PRAN ---------------------------------
*     set the uncorrelated values of the normal deviates      
      kstop = 0
      do is = 1, NS
      call set the uncorrelated values of the normal deviates 
     &(IR1, IR2, IR3, IR4, IS, IRT, IRS )
      enddo
*     call write shots for river flow FRAN

*     test and adjust the shots for random errors ... river quality ------------
      do jp = 1, ndet
      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = CRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
*     write(01,289) CM1, CM2
  289 Format('Calculated CRAN =',f12.6,8x,
     &       '           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25
      do is = 1,NS
      CRAN ( JP, IS ) = (CRAN ( JP, IS ) - CM1) 
	if ( CRAN ( JP, IS ) .gt. 0.0 ) then
	CRAN ( JP, IS )= CRAN ( JP, IS ) - add
	else
	CRAN ( JP, IS )= CRAN ( JP, IS ) + add
	endif
	enddo

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = CRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo
*	write(01,289) CM1, CM2
	enddo

*     test the and adjust the shots ... discharge quality ----------------------
      do jp = 1, ndet
      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = PRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
*	if ( nobigout .le. 0 ) write(01,239) CM1, CM2
  239 Format('Calculated PRAN =',f12.6,8x,
     &       '           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25

      do is = 1,NS
      PRAN ( JP, IS ) = (PRAN ( JP, IS ) - CM1) 
	if ( PRAN ( JP, IS ) .gt. 0.0 ) then
	PRAN ( JP, IS )= PRAN ( JP, IS ) - add
	else
	PRAN ( JP, IS )= PRAN ( JP, IS ) + add
	endif
	enddo

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = PRAN ( JP, IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo
*	if ( nobigout .le. 0 ) write(01,239) CM1, CM2
      enddo

*     test the and adjust the shots ... discharge flow -------------------------
      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = ERAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
*	if ( nobigout .le. 0 ) write(01,259) CM1, CM2
  259 Format('Calculated ERAN =',f12.6,8x,
     &       '           ...  Standard deviation =',f12.6)   

      do k = 1, 5
      add = (CM2 - 1.0) * 1.25

      do is = 1,NS
      ERAN (IS) = (ERAN (IS) - CM1) 

	if ( ERAN (IS) .gt. 0.0 ) then
	ERAN (IS)= ERAN (IS) - add
	else
	ERAN (IS)= ERAN (IS) + add
	endif

	enddo

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = ERAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
      enddo

*     test and adjust the shots for random errors ... river flow ---------------
      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = FRAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
*     if ( nobigout .le. 0 )write(01,299) CM1, CM2
  299 Format('Calculated FRAN =',f12.6,10x,
     &'...  Standard deviation =',f12.6)   
*     if ( MONF .gt. 1 ) call write shots for river flow FRAN
      do k = 1, 5
      add = (CM2 - 1.0) * 1.25
      do is = 1,NS
      FRAN (IS) = (FRAN (IS) - CM1) 
	if ( FRAN (IS) .gt. 0.0 ) then
	FRAN (IS)= FRAN (IS) - add
	else
	FRAN (IS)= FRAN (IS) + add
	endif
	enddo

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      xx = FRAN ( IS )
      CM1 = CM1 + xx
      CM2 = CM2 + xx * xx
	enddo

      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NS)
      enddo
     
      return
      end


*     set the uncorrelated values of the normal deviates -----------------------
      subroutine set the uncorrelated values of the normal deviates 
     &(IR1, IR2, IR3, IR4, IS, IRT, IRS )
      include 'COM.FOR'
      dimension RR2(MP10), RR4(MP10)
      dimension IR2(MP10), IR4(MP10)

      RR1 = GAS1 (IR1) ! uncorrelated normal deviates for river flow -----------
      RR3 = GAS3 (IR3) ! uncorrelated normal deviates for discharge flow -------
      do jdet= 1, ndet
      RR2(jdet) = GAS2 ( IR2(jdet) ) ! uncorrelated deviates for river quality -
      RR4(jdet) = GAS4 ( IR4(jdet) ) ! deviates for discharge quality ----------
      enddo
      RR5 = GAS5 (IRT) ! temperature -------------------------------------------
      RR7 = GAS7 (IRS) ! suspended solids --------------------------------------

*     store the values ---------------------------------------------------------
      FRAN (IS) = RR1 ! uncorrelated normal deviates for river flow ------------
      ERAN (IS) = RR3 ! uncorrelated normal deviates for discharge flow --------
      do idet = 1, ndet ! ------------------------------------------------------
      CRAN (idet,IS) = RR2 (idet) ! uncorrelated deviates for river quality ----
      PRAN (idet,IS) = RR4 (idet) ! uncorrelated deviates for discharge quality-
      enddo ! ------------------------------------------------------------------

*     impose correlation between temperature and river flow --------------------
      TRAN (IS) = RR5
      RR6 = TCORF * RR1 + RR5 * SQRMB(125, 1.0 - TCORF * TCORF )
      TRAN (IS) = RR6
      BMS global (1, IS) = TDEG + TSDEV * RR6
*     impose correlation between suspended solids and river flow ---------------
      SRAN (IS) = RR7

      return
      end


*     get random numbers and impose correlation --------------------------------
      subroutine get the correlated random numbers (IS, R1, R2, R3, R4)
      include 'COM.FOR'
      R1 = FRAN ( IS )
      js = jset ( IQ, NS, IS )
      R2 = CRAN ( JP, JS ) ! uncorrelated river flow
      R3 = ERAN ( IS )
      js = jset ( IQ, NS, IS )
      R4 = PRAN ( JP, JS )
      call impose correlation 4 (R1, R2, R3, R4) ! 44444444444444444444444444444
      return
      end


*     random number starters ---------------------------------------------------
      subroutine reset random number starters
      include 'COM.FOR'

      FSTART = -5119 ! river flows (default)
      ESTART = -1849 ! effluent flows (default) 
      CSTART = -1937 ! temperature
      USTART = -1937 ! suspended solids

      DSTART(1) = -1849 ! river quality (default values)
      if ( MP10 .gt. 1 ) DSTART(2) = -7849
      if ( MP10 .gt. 2 ) DSTART(3) = -2849
      if ( MP10 .gt. 3 ) DSTART(4) = -3849
      if ( MP10 .gt. 4 ) DSTART(5) = -5849
      if ( MP10 .gt. 5 ) DSTART(6) = -6849
      if ( MP10 .gt. 6 ) DSTART(7) = -9849
      if ( MP10 .gt. 7 ) DSTART(8) = -2249
      if ( MP10 .gt. 8 ) DSTART(9) = -4449
      if ( MP10 .gt. 9 ) DSTART(10) = -8649

      PSTART(1) = -7849 ! effluent quality (default values)
      if ( MP10 .gt. 1 ) PSTART(2) = -5149
      if ( MP10 .gt. 2 ) PSTART(3) = -6249
      if ( MP10 .gt. 3 ) PSTART(4) = -2480
      if ( MP10 .gt. 4 ) PSTART(5) = -1849
      if ( MP10 .gt. 5 ) PSTART(6) = -2849
      if ( MP10 .gt. 6 ) PSTART(7) = -4849
      if ( MP10 .gt. 7 ) PSTART(8) = -6249
      if ( MP10 .gt. 8 ) PSTART(9) = -9449
      if ( MP10 .gt. 9 ) PSTART(10) = -5649

      return
	end



*     set the starting values of variables -------------------------------------
      subroutine initialise variables
      include 'COM.FOR'

      check for infeasible quality = 0
      ihalt diffuse discharge = 0
      
      KSIM = 0 ! counter for iterations of gap filling -------------------------
      
      kount22 = 0
      kountd1 = 0
      kountd2 = 0
      iqmon = 0

      do isup = 1, MP10
      do itipe = 1, 9
      suppress (itipe,isup) = 0
      enddo
      enddo
      suppress1 = 0
      suppress3 = 0
      suppress4 = 0
      suppress5 = 0
      suppress6 = 0
      suppress7 = 0 ! unnecessary monthly structures
      suppress8 = 0 ! quality data not set for gap fill
      suppress9 = 0 ! failure to set up monthly structure
      suppress9a = 0 ! failure to set up monthly structure for temperature
      suppress9b = 0 ! failure to set up monthly structure for suspended solids
      suppress9c = 0 ! calculated discharge quality exceeds a huge number
      suppress9d = 0 ! calculated discharge quality exceeds a huge number
      suppress10 = 0
      suppress11 = 0
      suppress12 = 0
      suppress13 = 0
      suppress14 = 0
      suppress16 = 0
      suppress17 = 0 ! deleted feature
      suppress18 = 0 ! zero variation in river quality
      suppress19 = 0 ! unneeded data ignored
      suppress20 = 0 ! infeasible correlation coefficients
      suppress21 = 0 ! no data for gap filling
      suppress00 = 0
      
      do iu = 1, NU
      uname(iu) = '.............'
      JT(iu) = 0
      JREACH(iu) = 0
      DIST(iu) = 0.0
      JF(iu) = 0
      JQ(iu) = 0
      JFCAL(iu) = 0
      JQCAL(iu) = 0
      IFRQS(iu) = 0
      GISCODE(iu) = 'GIS code' 
      enddo
      
      do ir = 1, MR
      ADIST (ir) = 0.0
      enddo

*     initialise a test for finding the class limits ---------------------------
      jstructure message = 0

*     set the number of classes ------------------------------------------------
      nclass = 5

      IFDIST = 0
      IQDIST = 0
      if ( masterdata .eq. 1 .and. master set used .eq. 1) then
      masternocon = nocon
      master no tables = No tables
      masterIPRINT = IPRINT
      master output mode = output mode
      masterNS = NS
      masterIPUR = IPUR
	masterIDIFF = IDIFF
      masterKINT = KINT
      endif

*     if "ifbatch" is 1 this is a batch run ------------------------------------
	if ( ifbatch .eq. 1 .and. master set used .eq. 1 ) then
	if ( model number in batch .gt. 1 ) then
	nocon = masternocon
	No tables = master no tables
	IPRINT = masterIPRINT
	output mode = master output mode
	NS = masterNS
	IPUR = masterIPUR
	IDIFF = masterIDIFF
	KINT = masterKINT
      endif
      endif

      call set indices for percentiles

*     indicator that there are too many monitoring stations --------------------
      imm = 0

      call reset random number starters

*     set the correlation coefficients -----------------------------------------
*     these are default values which are overwritten by the values in ----------
*     the variables RFCL and EFCL in Suite FEATURE.FOR (prior to ---------------
*     Mass Balance.FOR) --------------------------------------------------------
*     or values read in for a particular site .... (this not yet done) ---------

      CO1 = 0.0 ! river flow on river quality
      CO2 = 0.0 ! river flow on discharge flow
      CO3 = 0.0 ! river flow on discharge quality ! 4444444444444444444444444444
      CO4 = 0.0 ! river quality on discharge flow ! 4444444444444444444444444444
      CO5 = 0.0 ! discharge flow on discharge quality
      CO6 = 0.0 ! river quality on discharge quality ! 4444444444444444444444444

*     counter for sampling points ----------------------------------------------
      LMONP = 0
*     counter for flow gauges --------------------------------------------------
      NGAUGE = 0
*     eventually NREACH will hold the number of Reaches ------------------------
      NREACH = 0
*     eventually NDET will hold the number of determinands ---------------------
      NDET = 0

      do if = 1, nf
*     distribution indicators for river flows ----------------------------------
      pdrf (if) = -1
      do imon = 1, MO
*     summary statistics for river flows ---------------------------------------
      F(if,1) = 0.0
      enddo
      enddo

      do ii = 1, 5
      FLOW(ii) = 0.0
      enddo

*     initialise shots ---------------------------------------------------------
      do is = 1, MS
      FMS(IS) = 0.0
      FMX(IS) = 0.0
	EFMS(IS) = 0.0
      do idet = 1, MP10
      CMS(idet,IS) = 0.0
      CMX(idet,IS) = 0.0
      do ip = 1, nprop
      LMS(ip,iDET,IS) = 0.0
      ULMS(ip,iDET,IS) = 0.0
      enddo
      enddo
      enddo
      
      do idet = 1, MP10
      do imom = 1,5
      C(idet,imom) = 0.0
      enddo
      enddo

*     distribution indicators for discharge flows ------------------------------
      do ie = 1, ne
      pdef (ie) = -1
      do imon = 1, MO
      FE(ie,imon) = 0.0      
      enddo
      enddo

*     distribution indicators for river quality --------------------------------
      do iv = 1, nv
      do ip = 1, mp10
      pdrc (iv,ip) = -1
      do imon = 1, MO
      quolity data(iv,ip,imon) = 0.0
      enddo
      enddo
      enddo

*     distribution indicators for discharge quality ----------------------------
      do ie = 1, ne
      do ip = 1, mp10
      pdec (ie,ip) = -1999
      do imon = 1, MO
      pollution data(ie,ip,imon) = 0.0
      enddo
      enddo
      enddo

*     storage for data on non-parametric distributions -------------------------
      do 6 k = 1, M7

*     river flows --------------------------------------------------------------
      flname(1,k) = '            '
      flnamesmall(1,k) = '            '
      flsequence(1,m7) = 0
*     river quality ------------------------------------------------------------
      flname(2,k) = '            '
      flnamesmall(2,k) = '            '
      flsequence(2,m7) = 0
*     discharge flow -----------------------------------------------------------
      flname(3,k) = '            '
      flnamesmall(3,k) = '            '
      flsequence(3,m7) = 0
*     discharge quality --------------------------------------------------------
      flname(4,k) = '            '
      flnamesmall(4,k) = '            '
      flsequence(4,m7) = 0

      do j = 1, MP10 + 1
      idenp(1,k,j) = 0
      idenp(2,k,j) = 0
      enddo

    6 continue

*     storage for monthly data -------------------------------------------------
      do 96 k = 1, M8
      flmonth(1,k) = 'File not read'
      flmonth(2,k) = 'File not read'
      flmonth(3,k) = 'File not read'
      flmonth(4,k) = 'File not read'
      flmonthsmall(1,k) = 'File not read'
      flmonthsmall(2,k) = 'File not read'
      flmonthsmall(3,k) = 'File not read'
      flmonthsmall(4,k) = 'File not read'
      do j = 1, MP10 + 1
      iseasp(1,k,j) = 0
      iseasp(2,k,j) = 0
      enddo
   96 continue

*     storage for monthly structure data ---------------------------------------
      do 99 k = 1, M9
      FLSTRUCT(1,k) = 'File not read'
      FLSTRUCT(2,k) = 'File not read'
      FLSTRUCT(3,k) = 'File not read'
      FLSTRUCT(4,k) = 'File not read'
      FLSTRUCTsmall(1,k) = 'File not read'
      FLSTRUCTsmall(2,k) = 'File not read'
      FLSTRUCTsmall(3,k) = 'File not read'
      FLSTRUCTsmall(4,k) = 'File not read'
      do j = 1, MP10 + 1
      istruct(1,k,j) = 0
      istruct(2,k,j) = 0
      enddo
   99 continue

      do 9 JP = 1,MP10
      qualn(JP) = 0.0 ! running sampling rates ---------------------------------
      QNAT(JP) = 0.0 ! natural purification ------------------------------------
      do IV = 1, NV
      QNUM(IV,JP) = -1 ! sampling rates ----------------------------------------
      enddo
    9 continue

      do 13 JM=1,MO
      
      do JP = 1,MP10
      do IV = 1,NV
*     arrays for storing quality data for rivers and streams -------------------
      quolity data(IV,JP,JM)=-1.0E10
      enddo
      do IE = 1,NE
*     arrays for storing quality data for effluents ----------------------------
      pollution data(IE,JP,JM)=-1.0E10
      enddo
      enddo
   13 continue
      do IE=1,NE
      ENUM(IE) = -1
      do JP = JP, MP10
      PNUM(IE,JP) = -1
      enddo
      enddo

*     background river quality data for reaches --------------------------------
      do iip = 1,3
      do IR = 1,MR
      BNUM(IR,iip) = -1 
 	PDBC(IR,iip) = 1
      do IM = 1,3
	BMAT(IR,iip,IM) = -1.0E10
      enddo
	enddo
      enddo

      do IR = 1,MR
      do idet = 1, MP10
      QDN(idet,IR) = 0.0 ! sampling rates at end of reaches --------------------
      enddo
      enddo

*     marker that a reach has specific standards -------------------------------
      number of stds = 0
      do IR = 1, 5
      do idet = 1, MP10
      EQS reach (IR,idet) = 0
      enddo
      standards for reach (IR,ic) = 1.0e10
      do ic = 1, NC
      standards for reach (IR,ic) = 0.0
      enddo
      enddo
     
*     river quality targets ----------------------------------------------------
      do ID = 1, MP10
      do IQ = 1, NQ
      RQS(IQ,ID) = 0.0
      enddo
*     Type of summary statistic for river quality targets. These will be -------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile-             
	MRQS(ID) = -99
      enddo

*     initialise river quality targets for graph plotting ---------------------- 
      do IQ = 1, MP10
      RQO(IQ) = 0.0
*     Type of summary statistic for river quality targets. These will be -------
*     1 mean; 2 95-percentile; 3 90-percentile; 4 5-percentile; 5 10-percentile-             
	MRQO(IQ) = 0
      enddo

*     pre-set the gap filling switches -----------------------------------------
      do itemp = 1,NU
      KFCAL(itemp) = 0
      KQCAL(itemp) = 0
      SKIPPEFF(itemp) = 0 ! device for ignoring discharges in Modes 7 and 8 ----
*     data-sets for gap filling ------------------------------------------------
      JFUSER(itemp) = 0
      JQUSER(itemp) = 0
      enddo

*     arrays for storing data for the end of a reach ---------------------------
      do 6629 IR = 1, KR
      JSTOR(IR) = 0 ! initialise store for reaches needed downstream -----------
      do JP = 1,MP10
      do J1 = 1, N13
      TGLODE3  (IR,JP,J1) = 0.0
      TRLOAD3  (IR,JP,J1) = 0.0
  	TELOAD3  (IR,JP,J1) = 0.0
	TNLOADUP3(IR,JP,J1) = 0.0
	TNLOADDN3(IR,JP,J1) = 0.0
      TDDLOAD3  (IR,JP,J1) = 0.0
	TALOADUP3(IR,JP,J1) = 0.0
	TALOADDN3(IR,JP,J1) = 0.0
      TILOADUP3(IR,JP,J1) = 0.0
	TILOADDN3(IR,JP,J1) = 0.0
      T13LOAD3 (IR,JP,J1) = 0.0
      T25LOAD3 (IR,JP,J1) = 0.0
      T27LOAD3 (IR,JP,J1) = 0.0
      T29LOAD3 (IR,JP,J1) = 0.0
      T31LOAD3 (IR,JP,J1) = 0.0
      T33LOAD3 (IR,JP,J1) = 0.0
      T35LOAD3 (IR,JP,J1) = 0.0
      T37LOAD3 (IR,JP,J1) = 0.0
      T40LOAD3 (IR,JP,J1) = 0.0
      T15LOAD3 (IR,JP,J1) = 0.0
      T42LOAD3 (IR,JP,J1) = 0.0
      T46LOAD3 (IR,JP,J1) = 0.0
      T48LOAD3 (IR,JP,J1) = 0.0
      TBLOAD3  (IR,JP,J1) = 0.0
      enddo
      do iworks = 1, NUED
  	TELOADAVrch(iworks,IR,JP) = 0.0
      
      if ( n148 .eq. 1 ) then ! l33333333333333333333333333333333333333333333333
      do is = 1, NS ! 3333333333333333333333333333333333333333333333333333333333
      if ( JP .eq. ndshot ) TELOADrchshots(iworks,IR,1,is) = 0.0 ! 3333333333333
      enddo ! do is = 1, NS 3333333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
	enddo
      do ibodies = 1, NUW
*     monthly loads from upstream sub-catchments at the ends of reaches --------
  	TWloadsrch(ibodies,IR,JP) = 0.0
      do ip = 1, nprop
*     breakdown of monthly loads from sub-catchments at ends of reaches --------
  	TWloadsrchapp(ibodies,IR,JP,ip) = 0.0
      enddo
      enddo
      enddo

      do IS = 1, MS
      FD(IR,IS) = 0.0
*     proportion of effluent ---------------------------------------------------
      EFD(IR,IS) = 0.0
      do JP = 1,MP10
      QD(IR,JP,IS) = 0.0
	do ip = 1, nprop
      EQD(ip,IR,JP,IS) = 0.0
	enddo
      enddo
      enddo

 6629 continue

      do IREACH = 1,MR
      IRHOLD (IREACH) = 0
      enddo

      IP = 0
   23 IP = IP + 1
      goto(31,32,33,34,35,36,37,38,39,40,41,41,41,41,41),IP
   31 DNAME(IP)='           '
      goto 41
   32 DNAME(IP)='.          '
      goto 41
   33 DNAME(IP)='..         '
      goto 41
   34 DNAME(IP)='...        '
      goto 41
   35 DNAME(IP)='....       '
      goto 41
   36 DNAME(IP)='.....      '
      goto 41
   37 DNAME(IP)='......     '
      goto 41
   38 DNAME(IP)='.......    '
      goto 41
   39 DNAME(IP)='........   '
      goto 41
   40 DNAME(IP)='.........  '
      goto 41
   41 if ( IP .EQ. MP10 ) goto 9899
      goto 23

 9899 continue

*     number of determinands ---------------------------------------------------
      NDET = 0
      NP = 0

*     initialise QTYPE and Detype ----------------------------------------------
      do jp = 1, MP10
	Detype (jp) = 0
	Qtype (jp) = 4
	units (jp) = "----"
      enddo

*     number of data-sets for non-parametric distributions ---------------------
      NONPD = 0

*     number of data-sets for monthly data -------------------------------------
	seasd = 0

*     initialise the face value to class one -----------------------------------
      Face value = 0
      Face confidence = 999.0
      Face determinand = 0
	do idet = 1, MP10
	Face Value dets (idet) = 0
      enddo

      return
      end






*     set up code numbers for determinands -------------------------------------
*     and assign correlation coefficients --------------------------------------
      subroutine set determinand codes (idet)
      include 'COM.FOR'
 
      ndetBOD = 0
      if ( qtype (idet) .ne. 4 ) then

*     default correlation coefficients -----------------------------------------
      if ( abs (dcorr (idet)) .lt. 1.000001 ) then
      RFCL(idet) = dcorr (idet)
      endif ! if ( abs (dcorr (idet)) .lt. 1.000001 ) --------------------------
	EFCL(idet) = 0.0
      
      QZEROGAP(idet) = amax1 (QZEROGAP(idet), 0.00000001)

*     chloride -----------------------------------------------------------------
      if (DNA(idet) .eq. 'CL  ' ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' CL ' ) Detype (idet) = 100
      if (DNA(idet) .eq. '  CL' ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'CL '  ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' CL'  ) Detype (idet) = 100
      if (DNA(idet) .eq. 'CL'   ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'Cl  ' ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' Cl ' ) Detype (idet) = 100
      if (DNA(idet) .eq. '  Cl' ) Detype (idet) = 100 
      if (DNA(idet) .eq. 'Cl '  ) Detype (idet) = 100 
      if (DNA(idet) .eq. ' Cl'  ) Detype (idet) = 100
      if (DNA(idet) .eq. 'Cl'   ) Detype (idet) = 100 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 100 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = -0.3 ! chloride
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = -0.1 ! chloride 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write(*,1)dname(idet)
      endif
	endif ! chloride

*     Biochemical Oxygen Demand ------------------------------------------------
      if (DNA(idet) .eq. 'BOD ' ) Detype (idet) = 101 
      if (DNA(idet) .eq. ' BOD' ) Detype (idet) = 101 
      if (DNA(idet) .eq. 'BOD'  ) Detype (idet) = 101 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 101 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.3 ! BOD
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.3 ! BOD
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write(*,1)dname(idet)
      endif
      endif ! BOD

*     TOC ----------------------------------------------------------------------
      if (DNA(idet) .eq. 'TOC ' ) Detype (idet) = 102 
      if (DNA(idet) .eq. ' TOC' ) Detype (idet) = 102 
      if (DNA(idet) .eq. 'TOC'  ) Detype (idet) = 102 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 102 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.3 ! TOC 
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.3 ! TOC 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write(*,1)dname(idet)
      endif
      endif ! TOC

*     ammonia ------------------------------------------------------------------
      if (DNA(idet) .eq. 'AMMN' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' AMM' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AmmN' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' Amm' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'Ammn' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4 ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' NH4' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AMM ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3 ' ) Detype (idet) = 103 
      if (DNA(idet) .eq. ' NH3' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3N' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4+' ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'AMM'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'Amm'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH4'  ) Detype (idet) = 103 
      if (DNA(idet) .eq. 'NH3'  ) Detype (idet) = 103 
*     assign default correlation coefficients ----------------------------------
      if ( detype (idet) .eq. 103 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.35 ! ammonia
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.4 ! ammonia 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
	call change colour of text (20) ! bright red
      write(*,1)dname(idet)
      call set screen text colour
    1 format('* Determinand type corrected to 2',18x,'...',7x,a11,
     &32x,25('.'))
      endif
	endif ! ammonia

*     dissolved oxygen ---------------------------------------------------------
      if (DNA(idet) .eq. 'DO  ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. '  DO' ) Detype (idet) = 104
      if (DNA(idet) .eq. ' DO ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DOX ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. ' DOX' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O.' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O ' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DO '  ) Detype (idet) = 104 
      if (DNA(idet) .eq. ' DO'  ) Detype (idet) = 104
      if (DNA(idet) .eq. 'DO'   ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'DOX'  ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O.' ) Detype (idet) = 104 
      if (DNA(idet) .eq. 'D.O'  ) Detype (idet) = 104 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 104 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.3 ! dissolved oxygen
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.0 ! dissolved oxygen 
	endif ! dissolved oxygen

*     phosphate ----------------------------------------------------------------
      if (DNA(idet) .eq. 'PO4P' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PO4 ' ) Detype (idet) = 105 
      if (DNA(idet) .eq. ' PO4' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'SRP ' ) Detype (idet) = 105 
      if (DNA(idet) .eq. ' SRP' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'TOTP' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PHOS' ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'PO4'  ) Detype (idet) = 105 
      if (DNA(idet) .eq. 'SRP'  ) Detype (idet) = 105 
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 105 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = -0.3 ! phosphate 
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = -0.1 ! phosphate 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write(*,1)dname(idet)
      endif
	endif ! phosphate

*     nitrate ------------------------------------------------------------------
      if (DNA(idet) .eq. 'NO3N' ) Detype (idet) = 106
      if (DNA(idet) .eq. ' NO3' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'TON ' ) Detype (idet) = 106
      if (DNA(idet) .eq. ' TON' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'NO3 ' ) Detype (idet) = 106
      if (DNA(idet) .eq. 'NO3'  ) Detype (idet) = 106
      if (DNA(idet) .eq. 'TON'  ) Detype (idet) = 106
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 106 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.6 ! nitrate
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = -0.2 ! nitrate 
      if ( qtype (idet) .eq. 3 ) then
      qtype (idet) = 2
      write(*,1)dname(idet)
      endif
	endif ! nitrate ----------------------------------------------------------

*     parent substances (where losses are added to another substance -----------
      if (DNA(idet) .eq. 'PAR ' ) Detype (idet) = 200 ! parent
      if (DNA(idet) .eq. ' PAR' ) Detype (idet) = 200 ! parent
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 200 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.0 ! parent substances
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.0 ! parent substances 
	endif ! parent substances

*     daughter substances (which receive additions from losses from a "parent")
      if (DNA(idet) .eq. 'DAU ' ) Detype (idet) = 201 ! daughter
      if (DNA(idet) .eq. ' DAU' ) Detype (idet) = 201 ! daughter
*     assign correlation coefficients ------------------------------------------
      if ( detype (idet) .eq. 201 ) then
      if ( abs (dcorr (idet)) .gt. 1.0 ) then
      RFCL(idet) = 0.0 ! daughter substances
      endif ! if ( abs (dcorr (idet)) .gt. 1.0 )
      EFCL(idet) = 0.0 ! daughter substances 
	endif ! daughter substances ----------------------------------------------

*     identify the positioning of BOD ------------------------------------------
	if ( detype (idet) .eq. 101 ) ndetBOD = idet
      
      endif ! if ( qtype (idet) .ne. 4 )
      
      return
	end









*     check first four characters of a line of input data ----------------------
*     skip comment lines -------------------------------------------------------

      subroutine check first four characters of a line of data (IFIN)
      character * 4 Line
      IFIN = 0
    1 read(02,2,end=9)Line
    2 format(A4)
      if ( Line .eq. '====' ) goto 1
      if ( Line .eq. ' ===' ) goto 1
      if ( Line .eq. '----' ) goto 1
	if ( Line .eq. "####" ) goto 1
      if ( Line .eq. '****' ) then
      IFIN = 1
      return
      endif
      backspace 02
      return
    9 write( *,8)
    8 format(77('-')/
     &'*** Unexpected end to data file ...'/
     &'*** No terminating line of asterisks ?   '/77('-'))
      call stop
      end









*     check for and read reach-specific data for temperature, standards etc ----
*     check first four characters of a line of input data ----------------------
*     skip comment lines -------------------------------------------------------
      subroutine check for reach data on background (KREACH,LTEST) 
      include 'COM.FOR'
      character *1 char(20)
	character *20 tname
	dimension value(4)
	
      ii1=0
	ii2=0
	ii3=0
	ii4=0
      do ii = 1, 4
      value(ii) = 0.0
      enddo
	
    1 read(02,2,end=9)(char(i),i=1,20)
    2 format(20a1) 

*     check for background data ------------------------------------------------
      do ichar = 1, 10
      if ( char(ichar) .ne. ' ' ) then
      if ( char(ichar) .eq. '0' .or. char(ichar) .eq. '1' .or.
     &     char(ichar) .eq. '2' .or. char(ichar) .eq. '3' .or.
     &     char(ichar) .eq. '4' .or. char(ichar) .eq. '5' .or.
     &     char(ichar) .eq. '6' .or. char(ichar) .eq. '7' .or.
     &     char(ichar) .eq. '8' .or. char(ichar) .eq. '9' ) then
      backspace 02
      LTEST = 1
      return
      endif
	jchar = ichar
      if ( char(ichar) .eq. "'" ) goto 3
      endif
      enddo
      
    3 continue
*     temperature --------------------------------------------------------------
      if ( char(jchar+1) .eq. 't' .or. char(jchar+1) .eq. 'T' ) ii1 = 1
      if ( char(jchar+2) .eq. 'e' .or. char(jchar+2) .eq. 'E' ) ii2 = 1
      if ( char(jchar+3) .eq. 'm' .or. char(jchar+3) .eq. 'M' ) ii3 = 1
      if ( char(jchar+4) .eq. 'p' .or. char(jchar+4) .eq. 'P' ) ii4 = 1
*     suspended solids ---------------------------------------------------------
      if ( char(jchar+1) .eq. 's' .or. char(jchar+1) .eq. 'S' ) ii1 = 2
      if ( char(jchar+2) .eq. 'u' .or. char(jchar+2) .eq. 'U' ) ii2 = 1
      if ( char(jchar+3) .eq. 's' .or. char(jchar+3) .eq. 'S' ) ii3 = 1
      if ( char(jchar+4) .eq. 'p' .or. char(jchar+4) .eq. 'P' ) ii4 = 1
*     reach-specific standards for river water quality --------------------------
      if ( char(jchar+1) .eq. 's' .or. char(jchar+1) .eq. 'S' ) ii1 = 2
      if ( char(jchar+2) .eq. 't' .or. char(jchar+2) .eq. 'T' ) ii2 = 2
      if ( char(jchar+3) .eq. 'a' .or. char(jchar+3) .eq. 'A' ) ii3 = 2
      if ( char(jchar+4) .eq. 'n' .or. char(jchar+4) .eq. 'N' ) ii4 = 1
      
      ii1 = ii1 * ii2 * ii3 * ii4 

      if ( ii1 .eq. 0 ) then
      LTEST = 1
      return
      endif
      
*     ===========================================temperature or suspended solids
      if ( ii1 .eq. 1 .or. ii1 .eq. 2 ) then  
      backspace 02
      read(02,*,err=96,end=9)tname,idist
      if ( idist .eq. 0 ) idist = 1
      if ( idist .eq. 3 ) idist = 2
*     normal or log normal distributions ---------------------------------------
	if ( idist .eq. 1 .or. idist .eq. 2 ) then
      backspace 02
      read(02,*,err=16,end=9)tname,idist,value(1),value(2),value(3),
     &value(4)
	goto 26
   16 continue
	value(4) = 36.0
      backspace 02
      read(02,*,err=36,end=9)tname,idist,value(1),value(2),value(3)
      goto 26
   36 continue
	value(3) = -0.6
      backspace 02
      read(02,*,err=36,end=9)tname,idist,value(1),value(2)
   26 continue
*     check and correct default correlation ------------------------------------
      if ( value(3) .gt. 1.0 .or. value(3) .lt. -1.0 ) then
      value(3) = -0.6
      endif
*     check for implausible data -----------------------------------------------
      if ( idist .gt. 0 ) then
      if ( value(2) .lt. 0.00000001 .and. value(1) .gt. 0.000001 ) then
      suppress18 = suppress18 + 1 ! zero variation in temperature etc
      if ( ii1 .eq. 1 ) write(33,6317)
 6317 format(77('-')/
     &'*** Implausible (zero) standard deviation for river ',
     &'temperature ... '/77('-'))
      if ( ii1 .eq. 2 ) write(33,6347)
 6347 format(77('-')/
     &'*** Implausible (zero) standard deviation for river ',
     &'suspended solids ... ')
      if ( tony warn overide .eq. 1 ) then
      value(2) = 0.25 * value(1)
      call sort format 1 (value(1))
	write(33,6288)valchars10
 6288 format('*** Over-written as 25% of the mean of ',a10)
      endif
      write(33,6399)
 6399 format(77('-'))
	endif
      endif ! if ( idist .gt. 0 )
*     load river quality data into SIMCAT's storage ----------------------------
      do JM = 1, 3
      BMAT ( KREACH, ii1, JM ) = value(JM)
      enddo
*     set the distribution to normal -------------------------------------------
      PDBC ( KREACH, ii1) = idist
      BNUM ( KREACH, ii2) = value(4)
      return
      endif
*     --------------------------------------- normal or log normal distributions

*     monthly structure distribution -------------------------------------------
	if ( idist .eq. 8 ) then
      backspace 02
      call get monthly structure file for background (KREACH, ii1)  
	return   
      endif
      endif ! temperature or suspended solids
   
*     read the reach-specific standards for river water quality ================
      if ( ii1 .eq. 8 ) then
	number of stds = number of stds + 1
      backspace 02
      read(02,*,err=86,end=9)tname,iidet,iclass,(value(i),i=1,iclass)
      EQS reach (KREACH,iidet) = number of stds
      icheck = 0
      do i = 1, iclass
      if ( value(i) .lt. -1.0e-10 .and. icheck .eq. 0 ) icheck = i 
	standards for reach (number of stds, i) = abs (value(i))
      enddo
      if ( icheck .eq. 0 ) then
      standards for reach (number of stds, 2) = 
     & - standards for reach (number of stds, 2)
      else
      standards for reach (number of stds, icheck) = 
     & - standards for reach (number of stds, icheck)
      endif
      return
      endif   
*     ================ read the reach-specific standards for river water quality
      
 	return

*     --------------------------------------------------------------------------
   96 continue
      write( *,18)
   18 format(77('=')/
     &'*** Error in reading data on background for reaches ...'/
     &'*** Check the name of the reach is not "Temperature" etc ...'/
     &77('='))
      backspace 02
      call close down
	return
   86 continue
      write( *,78)
   78 format(77('=')/
     &'*** Error in reading data on reach-specific standards ...'/
     &77('='))
      backspace 02
	return
*     --------------------------------------------------------------------------
    9 continue
      write( *,8)
    8 format(77('-')/
     &'*** Unexpected end to the SIMCAT main data file ...'/
     &77('-'))
      call stop
      end
*     --------------------------------------------------------------------------












*     check first four characters of a line of input data ----------------------
*     skip comment lines -------------------------------------------------------
      subroutine Getlin89 (IFIN)
      character * 4 Line
      IFIN = 0
    1 read(89,2,end=9)Line
    2 format(A4)
      if ( Line .eq. '====') goto 1
      if ( Line .eq. ' ===') goto 1
      if ( Line .eq. '----') goto 1
	if ( Line .eq. "####") goto 1
      if ( Line .eq. '****')then
      IFIN = 1
      return
      else
      backspace 89
      endif
      return
    9 write( *,8)
    8 format(
     &'------------------------------------'/
     &'Unexpected end to data file ...'/
     &'No terminating line of **** ?   '/
     &'------------------------------------')
      call stop
      end

      subroutine extract a line of data (line)
      character * 200 Line
      IFIN=0
    1 read(02,2)Line
    2 format(A200)
      return
      end
